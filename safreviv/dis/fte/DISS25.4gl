################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 03/03/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS24                                                   #
#Objetivo          => Programa que genera la intefaz de la salida de los       #
#                     ajustes de importes de amortízación excedentes.          #
#Fecha inicio      => 03/03/2014                                               #
################################################################################
DATABASE
  safre_viv

GLOBALS 
  DEFINE 
    v_archivo_copia          VARCHAR(25),
    v_comando_dos            STRING 
END GLOBALS 
     
MAIN
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_folio_liquida          LIKE dis_preliquida.folio_liquida,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08), -- fecha del archivo de salida
    v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03), -- Parametro consecutivo de registro por dia
    v_ins_reg                STRING,  -- almacena cadena a insertar en el archivo
    v_qry_txt                STRING, -- cadena para preparar consultas 
    v_busca_nom_archivo      STRING -- busca nombre de archivo
       
  DEFINE v_arr_tipo1         RECORD --Record para tipo de registro uno
    tipo_registro1           CHAR(02),
    espacios1                CHAR(8),
    f_archivo1               CHAR(10)
  END RECORD
       
  DEFINE v_arr_tipo2         RECORD  --Record para tipo de registro dos
    tipo_registro2           CHAR(02),
    id_derechohabiente       LIKE dis_det_avance_pago.id_derechohabiente,
    nss                      LIKE afi_derechohabiente.nss,
    num_credito              CHAR(10),--LIKE dis_det_avance_pago.num_credito,
    periodo_pago             CHAR(06),--LIKE dis_det_avance_pago.periodo_pago,
    f_pago                   CHAR (8),
    entidad_recau            CHAR(03),
    nrp                      LIKE dis_det_avance_pago.nrp,
    monto_aportacion         LIKE dis_det_avance_pago.monto_aportacion,
    monto_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion,
    folio_sua                CHAR(06),
    res_operacion            CHAR(02),
    mot_rechazo              CHAR(02)
  END RECORD,

    v_espacios2              CHAR(1),
    c_monto_apo              CHAR(09),
    c_monto_amo              CHAR(09),
    d_f_pago                 DATE

  DEFINE v_arr_tipo9         RECORD --Record para tipo de registro nueve
    v_tipo_registro9         CHAR(02),
    v_total_solicitud9       SMALLINT,
    v_total_aceptado9        SMALLINT,
    v_total_rechazados9      SMALLINT,
    v_total_monto9           DECIMAL(12,2),
    v_total_monto_acep9      DECIMAL(12,2),
    v_total_monto_rech9      DECIMAL(12,2)
  END RECORD, 
    
    c_total_solicitud9       CHAR(4),
    c_total_aceptado9        CHAR(4),
    c_total_rechazados9      CHAR(4),
    c_total_monto9           CHAR(11),
    c_total_monto_acep9      CHAR(11),
    c_total_monto_rech9      CHAR(11) 

  DEFINE 
    v_cuenta_hs              INTEGER,
    v_QryTxt                 STRING,
    v_dia_habil              SMALLINT

  DEFINE
    v_id_derechohabiente     DECIMAL(9,0),
    v_id_dis_interface_hs    DECIMAL(9,0),
    v_folio_ajuste           DECIMAL(9,0),
    v_folio_n_dis            DECIMAL(9,0),
    c_folio_n_dis            CHAR(9),
    v_cxc_calculada          DECIMAL(12,2),
    v_imp_diferencia         DECIMAL(12,2),
    c_imp_diferencia_p       CHAR(08),
    c_imp_diferencia_a       CHAR(16)
       
  --Asignación de parametros generales
  LET v_folio_liquida  = ARG_VAL(1) -- Valor de argumento uno de DISS22
  LET v_folio_ajuste   = ARG_VAL(2) -- Valor de argumento uno de DISS22
  LET v_folio_n_dis    = ARG_VAL(3) -- Valor de argumento uno de DISS22  
  LET v_modulo_cod     = "dis"
  LET v_cont_dia       = 1
  LET v_cuenta_hs      = 0

  --Sección consultas preparadas
  --Valida que existan datos para generar la interface, si no existen se sale del programa
  SELECT count(*)
  INTO   v_cuenta_hs
  FROM   dis_creditos_mtc
  WHERE  folio_liquida = v_folio_liquida
  IF v_cuenta_hs = 0 OR v_cuenta_hs IS NULL THEN
     DISPLAY "No se generó la interface de salida de ajustes de importes de amortización excedentes por falta de información"
     EXIT PROGRAM 
  END IF 
   
  PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio

  -- se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  -- se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/pex_dpie_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "pex_dpie_" || v_ddmmaaaa  --Concatena nombre a buscar
   
  --Obtine consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia
        
  LET v_reg_dia           = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo       = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  # LET v_nom_archivo = v_nom_archivo CLIPPED||"."|| v_modulo_cod
   
  LET v_ruta_nomarch      = v_ruta_envio_dis CLIPPED || v_nom_archivo
  --DISPLAY "v_ruta_nomarch ",v_ruta_nomarch
  -- se crea el manejador de archivo
  LET v_ch_arch_salida    = base.Channel.create()
   
  -- se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  #####################################
  #####Carga  tipo de registro uno#####
  #####################################
  LET v_arr_tipo1.tipo_registro1 = "01"
  LET v_arr_tipo1.espacios1      = "        "
  LET v_arr_tipo1.f_archivo1     = TODAY USING "dd-mm-yyyy"
  LET v_ins_reg                  = v_arr_tipo1.*
  CALL v_ch_arch_salida.write([v_ins_reg])

  #####################################
  #####Carga  tipo de registro dos#####
  #####################################
  --Asiganción de variables para el tipo de registro
  LET v_arr_tipo2.tipo_registro2 = "02"
  LET v_espacios2                = " "
  LET v_arr_tipo2.res_operacion  = "  "
  LET v_arr_tipo2.mot_rechazo    = "  "
  --LET v_dia_habil                = 6 --Para que traiga el séptimo día hábil

  LET v_arr_tipo9.v_total_solicitud9 = 0
  LET v_arr_tipo9.v_total_monto9     = 0

  DECLARE cur_consulta_folio_interface_hs CURSOR FOR
  SELECT a.nss,
         a.num_crd_ifv,
         a.periodo_pago,
         a.f_pago,
         a.nrp,
         a.imp_ap_pat,
         a.imp_am_cre,
         a.folio_sua,
         a.id_derechohabiente,
         a.id_dis_interface_hs,
         a.folio_ajuste,
         a.folio_n_dis,
         a.imp_diferencia
  FROM   dis_creditos_mtc a
  WHERE  folio_liquida = v_folio_liquida
  FOREACH cur_consulta_folio_interface_hs INTO v_arr_tipo2.nss,
                                               v_arr_tipo2.num_credito,
                                               v_arr_tipo2.periodo_pago,
                                               d_f_pago,
                                               v_arr_tipo2.nrp,
                                               v_arr_tipo2.monto_aportacion,
                                               v_arr_tipo2.monto_amortizacion,
                                               v_arr_tipo2.folio_sua,
                                               v_id_derechohabiente,
                                               v_id_dis_interface_hs,
                                               v_folio_ajuste,
                                               v_folio_n_dis,
                                               v_imp_diferencia

    --Se da formato al número de crédito
    LET v_arr_tipo2.num_credito        = v_arr_tipo2.num_credito USING "&&&&&&&&&&"
    LET c_imp_diferencia_p             = (v_imp_diferencia * 100) USING "&&&&&&&&"
    LET c_imp_diferencia_a             = (v_imp_diferencia * 1000000) USING "&&&&&&&&&&&&&&&&"
    LET c_folio_n_dis                  = v_folio_n_dis USING "&&&&&&&&&"

    --Se asigna el día 7 de la fecha de la aplicación
    --LET v_arr_tipo2.f_pago               = TODAY USING "yyyymm"||"07" 
    LET v_arr_tipo2.f_pago             = TODAY USING "ddmmyyyy" 
         
    --Parametros para tipo de registro nueve
    LET v_arr_tipo9.v_total_solicitud9 = v_arr_tipo9.v_total_solicitud9 + 1
    LET v_arr_tipo9.v_total_monto9     = v_arr_tipo9.v_total_monto9 + v_imp_diferencia
         
    --Concatenacion de registros para escribir archivo de salida
    LET v_ins_reg                      = v_arr_tipo2.tipo_registro2,
                                         v_arr_tipo2.nss,
                                         v_espacios2,
                                         v_arr_tipo2.num_credito,
                                         v_arr_tipo2.periodo_pago,
                                         c_imp_diferencia_p,
                                         v_espacios2,
                                         c_imp_diferencia_a,
                                         v_espacios2,
                                         c_folio_n_dis,
                                         v_espacios2,
                                         v_arr_tipo2.f_pago,
                                         v_espacios2,
                                         v_arr_tipo2.res_operacion,
                                         v_espacios2,
                                         v_arr_tipo2.mot_rechazo,
                                         v_espacios2
    CALL v_ch_arch_salida.write([v_ins_reg])
  END FOREACH 

  ######################################
  #####Carga tipo de registro nueve#####
  ######################################
  LET v_arr_tipo9.v_tipo_registro9 = "09"
  LET c_total_solicitud9           = v_arr_tipo9.v_total_solicitud9 USING "&&&&"
  LET c_total_aceptado9            = c_total_solicitud9
  LET c_total_rechazados9          = "0000"
  LET c_total_monto9               = v_arr_tipo9.v_total_monto9 USING "&&&&&&&&.&&"
  LET c_total_monto_acep9          = c_total_monto9
  LET c_total_monto_rech9          = "00000000.00"

  LET v_ins_reg                    = v_arr_tipo9.v_tipo_registro9,
                                     c_total_solicitud9,
                                     c_total_aceptado9,
                                     c_total_rechazados9,
                                     c_total_monto9,
                                     c_total_monto_acep9,
                                     c_total_monto_rech9
  CALL v_ch_arch_salida.write([v_ins_reg])

  CALL v_ch_arch_salida.close()
  DISPLAY "Se ha generado la interface para la salida de ajustes de importes de amortizaciones excedentes: ",v_nom_archivo

  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  --DISPLAY "v_comando_dos ",v_comando_dos 
  RUN v_comando_dos
   
  --Genera una copia de la interface con el nombre corto
  CALL fn_genera_copia_interface_real(v_ruta_nomarch,v_ruta_envio_dis)
   
END MAIN

#Objetivo: Obtener el folio_sua segun ultimo folio_liquida de dis_preliquida
#          relacionandolo con folio_sua de dis_interface_sh
{FUNCTION fn_obtiene_folio_sua(v_folio_liquida)
  DEFINE 
    v_folio_sua              CHAR(06),
    v_folio_liquida          LIKE dis_preliquida.folio_liquida

  --Obtiene folio_sua deacuerdo al ultimo folio_liquida de dis_preliquida
  SELECT DISTINCT folio_sua
  INTO   v_folio_sua
  FROM   dis_interface_hs
  WHERE  folio_liquida = v_folio_liquida

  RETURN v_folio_sua USING "&&&&&&"
END FUNCTION}

#Objetivo:Asignar el 7° día del mes posterior al año del bimestre que se
#         entrega(periodo de pago)
{FUNCTION fn_obtiene_f_pago(v_perido_pago)
  DEFINE 
    v_perido_pago            CHAR(06),
    v_fecha_pago             CHAR(08),
    v_bimestre_pago          CHAR(02),
    v_ano_pago               INTEGER

  LET v_ano_pago      = v_perido_pago[1,4]
  LET v_bimestre_pago = v_perido_pago[5,6]

  CASE v_bimestre_pago
    WHEN "01"
      LET v_fecha_pago = v_ano_pago||"03"||"07"
    WHEN "02"
      LET v_fecha_pago = v_ano_pago||"05"||"07"
    WHEN "03"
      LET v_fecha_pago = v_ano_pago||"07"||"07"
    WHEN "04"
      LET v_fecha_pago = v_ano_pago||"09"||"07"
    WHEN "05"
      LET v_fecha_pago = v_ano_pago||"11"||"07"
    WHEN "06"
      LET v_ano_pago   = v_ano_pago + 1
      LET v_fecha_pago = v_ano_pago||"01"||"07"
  END CASE

  RETURN v_fecha_pago

END FUNCTION}

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis           LIKE seg_modulo.ruta_envio,  --Ruta donde se genera archivo
    p_busca_nom_archivo        VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                      STRING,  --Cadena de comando a ejecutar
    v_consecutivo              INTEGER  --Consecutivo del archivo por día

  DEFINE fn                    CHAR(22)  --Almacena el nombre completo del nombre del archivo en el servidor con su extensión
  DEFINE ch                    base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"
  LET ch    = base.Channel.create()
    
  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")
  
  WHILE ch.read([fn])
    LET v_consecutivo = fn[16,18]  --Posición del consecutivo dentro de la cadena
  END WHILE
  
  CALL ch.close()
  
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION

--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface_real(p_archivo_envio,p_ruta_destino)
  DEFINE 
    v_cmd                    STRING,
    p_archivo_envio          VARCHAR(100),
    p_ruta_destino           VARCHAR(40)

  --DISPLAY "p_archivo_envio: ",p_archivo_envio
  --DISPLAY "p_ruta_destino: ",p_ruta_destino
  
  LET v_archivo_copia = "pex_dpie"
  LET v_archivo_copia = v_archivo_copia CLIPPED,".dis" 
  --DISPLAY "v_archivo_copia -- ",v_archivo_copia
    
  LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED 
  --DISPLAY "v_cmd -- ",v_cmd
  RUN v_cmd 

  LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
  --DISPLAY "v_comando_dos ",v_comando_dos 
  RUN v_comando_dos
  DISPLAY "     Se ha realizado la copia de la interface para la salida de los ajustes de importes de amortización excedentes: ",v_archivo_copia
    
END FUNCTION
