################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 14/08/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS31                                                   #
#Objetivo          => Programa que genera la inteface de los registros de      #
#                     de pago por Regla 27                                     #
#Fecha inicio      => 14/08/2014                                               #
################################################################################
DATABASE
  safre_viv

GLOBALS 
  DEFINE
    v_folio_registro         LIKE dis_det_avance_pago.folio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod

  DEFINE
    p_proceso_cod            LIKE cat_proceso.proceso_cod,
    p_opera_cod              LIKE cat_operacion.opera_cod,
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_archivo_copia          VARCHAR(25),
    v_comando_dos            STRING 
END GLOBALS 
     
MAIN
  DEFINE
    r_bandera                SMALLINT 

  --Asignación de parametros generales 
  LET p_usuario        = ARG_VAL(1)
  LET p_pid            = ARG_VAL(2) 
  LET p_proceso_cod    = ARG_VAL(3) 
  LET p_opera_cod      = ARG_VAL(4)
  LET v_folio_registro = ARG_VAL(5) --Valor de argumento uno de DISC01 

  LET v_modulo_cod = "dis"
     
  --Llama a la función que genera la interface de avance de pagos
  CALL fn_genera_interface_ava_r27()

  --Finaliza la operación
  CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
  RETURNING r_bandera
  
  IF r_bandera = 0 THEN 
     DISPLAY "Se ha realizado la Interface de Avances de Pago Regla 27."
     EXIT PROGRAM 
  ELSE 
     --Si ocurrió error 
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) 
     RETURNING r_bandera

     CALL fn_desplega_inc_operacion(r_bandera)
     EXIT PROGRAM 
  END IF

END MAIN

--Función que genera el archivo de salida de avance de pagos (Regla 27).
FUNCTION fn_genera_interface_ava_r27()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ins_reg                STRING,       --Almacena cadena a insertar en el archivo
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_periodo_archivo        CHAR(6),
    v_bnd_totales            INTEGER 

  DEFINE v_arr_tipo0         RECORD        --Record para tipo de registro cero
    tipo_registro0           CHAR(01),
    fehca0                   CHAR(08),
    espacios0                CHAR(76)
    END RECORD
    
  DEFINE v_arr_tipo1         RECORD        --Record para tipo de registro uno
    tipo_regitro1            CHAR(01),
    tipo_transaccion1        CHAR(04),
    fecha1                   CHAR(08),
    espacios1                CHAR(72)
  END RECORD
       
  DEFINE v_arr_tipo2         RECORD        --Record para tipo de registro dos
    tipo_registro2           CHAR(01),
    id_derechohabiente       DECIMAL(9,0),
    nss                      CHAR(11),
    num_credito              DECIMAL(10,0),
    periodo_pago             CHAR(06),
    f_pago                   DATE,
    entidad_recau            CHAR(03),
    nrp                      CHAR(11),
    monto_aportacion         DECIMAL(7,2),
    monto_amortizacion       DECIMAL(7,2),
    folio_sua                CHAR(06),
    indica_seg               CHAR(01),
    clave_rch                CHAR(10)
  END RECORD,
       
    c_monto_apo              CHAR(9),
    c_monto_amo              CHAR(9),
    v_fecha_pago             CHAR(8),
    v_dia_habil              SMALLINT
       
  DEFINE v_arr_tipo3         RECORD        --Record para tipo de registro tres
    v_tipo_registro3         CHAR(01),
    v_tipo_transaccion3      CHAR(04),
    v_i_registros_tipo2      INTEGER,
    v_c_registros_tipo2      CHAR(10),
    v_total_aportacion       DECIMAL(12,2),---Nota
    v_total_amortizacion     DECIMAL(12,2),---Nota
    v_c_total_seguridad      CHAR(12),
    v_d_total_seguridad      DECIMAL(10,2),
    v_total_general          DECIMAL(12,2),---Nota
    v_escacios3              CHAR(22)
  END RECORD,
       
    c_total_aportacion       CHAR(12),
    c_total_amortizacion     CHAR(12),
    c_total_general          CHAR(12)
       
  DEFINE v_arr_tipo4         RECORD        --Record para tipo de registro cuatro
    v_tipo_registro4         CHAR(01),
    v_total_registros        INTEGER,
    v_espacios4_1            CHAR(04),
    v_tot_apo_4              DECIMAL(12,2),--Nota
    v_tot_amo_4              DECIMAL(12,2),--Nota
    v_tot_seg_4              CHAR(12),
    v_tot_gen_4              DECIMAL(12,2),--Nota
    v_espacio4_2             CHAR(22)
  END RECORD,

    c_total_registros        CHAR(10),
    c_tot_apo_4              CHAR(12),
    c_tot_amo_4              CHAR(12),
    c_tot_gen_4              CHAR(12)

  LET v_cont_dia                       = 1
  LET v_arr_tipo3.v_total_aportacion   = 0.00
  LET v_arr_tipo3.v_total_amortizacion = 0.00
  LET v_arr_tipo3.v_total_general      = 0.00
  LET v_dia_habil                      = 6

  LET v_bnd_totales = 0

  --Valida que exista información para generar el archivo de avance de pagos
  SELECT COUNT(*)
  INTO   v_bnd_totales
  FROM   dis_ava_regla_27
  WHERE  folio = 0
  IF v_bnd_totales  = 0    OR 
     v_bnd_totales IS NULL THEN
     DISPLAY "Error: No existe información para generar el archivo."
     EXIT  PROGRAM   
  END IF    
   
  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se obtiene el periodo de pago para concatenarlo en el nombre del archivo
  {SELECT MAX(periodo_pago)
  INTO   v_periodo_archivo
  FROM   dis_ava_regla_27
  WHERE  folio = 0}

  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"
  
  LET v_nom_archivo       = "/avr27_"
  --LET v_busca_nom_archivo = "avr27_", v_periodo_archivo
  LET v_busca_nom_archivo = "avr27_", v_ddmmaaaa

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia  = v_cont_dia USING "&&&"
  LET v_ddmmaaaa = TODAY USING "ddmmyyyy"
   
  LET v_nom_archivo  = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Carga tipo de Registro cero
  LET v_arr_tipo4.v_total_registros = v_arr_tipo4.v_total_registros + 1
  LET v_arr_tipo0.tipo_registro0    = "0"
  LET v_arr_tipo0.fehca0            = TODAY USING "yyyymmdd"
  LET v_arr_tipo0.espacios0         = "                                        "||
                                      "                                    "
  LET v_ins_reg                     = v_arr_tipo0.*
  CALL v_ch_arch_salida.write([v_ins_reg])

  --Carga tipo de registro uno
  LET v_arr_tipo4.v_total_registros = v_arr_tipo4.v_total_registros + 1
  LET v_arr_tipo1.tipo_regitro1     = 1
  LET v_arr_tipo1.tipo_transaccion1 = "7080"
  LET v_arr_tipo1.fecha1            = TODAY USING "yyyymmdd"
  LET v_arr_tipo1.espacios1         = "                                        "||
                                      "                                "
  LET v_ins_reg                     = v_arr_tipo1.*
  CALL v_ch_arch_salida.write([v_ins_reg])

  --Carga tipo de registro dos
  --Asiganción de variables para el tipo de registro
  LET v_arr_tipo2.tipo_registro2 = "2"
  LET v_arr_tipo2.entidad_recau  = "181"
  LET v_arr_tipo2.folio_sua      = "000000"
  LET v_arr_tipo2.indica_seg     = " "
  LET v_arr_tipo2.clave_rch      = "          "

  DECLARE cur_consulta_folio_avance_pago CURSOR FOR
  SELECT a.nss,
         a.id_dis_det_ava_regla27,
         a.num_credito,
         a.periodo_pago,
         '01010001',
         a.nrp,
         0,
         a.monto_amortizacion
  FROM   dis_ava_regla_27 a
  WHERE  a.folio = 0
  FOREACH cur_consulta_folio_avance_pago INTO v_arr_tipo2.nss,
                                              v_arr_tipo2.id_derechohabiente,
                                              v_arr_tipo2.num_credito,
                                              v_arr_tipo2.periodo_pago,
                                              v_arr_tipo2.f_pago,
                                              v_arr_tipo2.nrp,
                                              v_arr_tipo2.monto_aportacion,
                                              v_arr_tipo2.monto_amortizacion

    LET v_arr_tipo2.num_credito = v_arr_tipo2.num_credito USING "&&&&&&&&&&"
    --LET v_fecha_pago            = TODAY USING "yyyymm"||"07" 
    LET v_fecha_pago            = TODAY USING "yyyymmdd" 
         
    --Parametros para tipo de registro cuatro
    LET v_arr_tipo4.v_total_registros    = v_arr_tipo4.v_total_registros + 1
           
    --Parametros para tipo de registro tres
    LET v_arr_tipo3.v_i_registros_tipo2  = v_arr_tipo3.v_i_registros_tipo2  + 1
    LET v_arr_tipo3.v_total_aportacion   = v_arr_tipo3.v_total_aportacion +
                                           v_arr_tipo2.monto_aportacion
    LET v_arr_tipo3.v_total_amortizacion = v_arr_tipo3.v_total_amortizacion + 
                                           v_arr_tipo2.monto_amortizacion

    --Concatenacion de registros para escribir archivo de salida
    LET c_monto_apo = (v_arr_tipo2.monto_aportacion   * 100) USING "&&&&&&&&&"
    LET c_monto_amo = (v_arr_tipo2.monto_amortizacion * 100) USING "&&&&&&&&&"
    LET v_ins_reg   = v_arr_tipo2.tipo_registro2 CLIPPED,
                      v_arr_tipo2.nss            USING "&&&&&&&&&&&" CLIPPED,
                      v_arr_tipo2.num_credito    USING "&&&&&&&&&&"  CLIPPED,   
                      v_arr_tipo2.periodo_pago   CLIPPED,
                      v_fecha_pago,
                      v_arr_tipo2.entidad_recau,
                      v_arr_tipo2.nrp,
                      c_monto_apo,
                      c_monto_amo,
                      v_arr_tipo2.folio_sua,
                      v_arr_tipo2.indica_seg,
                      v_arr_tipo2.clave_rch
    CALL v_ch_arch_salida.write([v_ins_reg])
  END FOREACH 

  --Carga tipo de registro tres
  LET v_arr_tipo4.v_total_registros   = v_arr_tipo4.v_total_registros + 1
  LET v_arr_tipo3.v_c_registros_tipo2 = v_arr_tipo3.v_i_registros_tipo2 USING "&&&&&&&&&&"
  LET v_arr_tipo3.v_tipo_registro3    = "3"
  LET v_arr_tipo3.v_tipo_transaccion3 = "7080"
  LET v_arr_tipo3.v_c_total_seguridad = "000000000000"
  LET v_arr_tipo3.v_d_total_seguridad = v_arr_tipo3.v_c_total_seguridad
  LET v_arr_tipo3.v_total_general     = v_arr_tipo3.v_total_aportacion +
                                        v_arr_tipo3.v_total_amortizacion +
                                        v_arr_tipo3.v_d_total_seguridad
  LET v_arr_tipo3.v_escacios3         = "                      "
  LET c_total_aportacion              = (v_arr_tipo3.v_total_aportacion * 100) USING "&&&&&&&&&&&&"
  LET c_total_amortizacion            = (v_arr_tipo3.v_total_amortizacion * 100) USING "&&&&&&&&&&&&"
  LET c_total_general                 = (v_arr_tipo3.v_total_general * 100) USING "&&&&&&&&&&&&"
  LET v_ins_reg                       = v_arr_tipo3.v_tipo_registro3,
                                        v_arr_tipo3.v_tipo_transaccion3,
                                        v_arr_tipo3.v_c_registros_tipo2,
                                        c_total_aportacion,
                                        c_total_amortizacion,
                                        v_arr_tipo3.v_c_total_seguridad,
                                        c_total_general,
                                        v_arr_tipo3.v_escacios3
  CALL v_ch_arch_salida.write([v_ins_reg])

  --Carga tipo de registro cuatro
  LET v_arr_tipo4.v_tipo_registro4  = "4"
  LET v_arr_tipo4.v_total_registros = v_arr_tipo4.v_total_registros + 1
  LET c_total_registros             = v_arr_tipo4.v_total_registros USING "&&&&&&&&&&"
  LET v_arr_tipo4.v_espacios4_1     = "0000"
  LET v_arr_tipo4.v_tot_apo_4       = v_arr_tipo3.v_total_aportacion
  LET c_tot_apo_4                   = (v_arr_tipo4.v_tot_apo_4 * 100) USING "&&&&&&&&&&&&"
  LET v_arr_tipo4.v_tot_amo_4       = v_arr_tipo3.v_total_amortizacion
  LET c_tot_amo_4                   = (v_arr_tipo4.v_tot_amo_4 * 100) USING "&&&&&&&&&&&&"
  LET v_arr_tipo4.v_tot_seg_4       = v_arr_tipo3.v_c_total_seguridad
  LET v_arr_tipo4.v_tot_gen_4       = v_arr_tipo3.v_total_general
  LET c_tot_gen_4                   = (v_arr_tipo4.v_tot_gen_4 * 100) USING "&&&&&&&&&&&&"
  LET v_arr_tipo4.v_espacio4_2      = "                      "
  LET v_ins_reg                     = v_arr_tipo4.v_tipo_registro4,
                                      c_total_registros,
                                      v_arr_tipo4.v_espacios4_1,
                                      c_tot_apo_4,
                                      c_tot_amo_4,
                                      v_arr_tipo4.v_tot_seg_4,
                                      c_tot_gen_4,
                                      v_arr_tipo4.v_espacio4_2
  CALL v_ch_arch_salida.write([v_ins_reg])
  CALL v_ch_arch_salida.close()

  UPDATE dis_ava_regla_27
  SET    folio = v_folio_registro
  WHERE  folio = 0

  --Despliega información en el log
  DISPLAY "\n ############### REGISTRO AVANCES DE PAGO ###############"
  DISPLAY " ###############          REGLA27         ###############"
  DISPLAY ""
  DISPLAY "Nombre del archivo             : ", v_nom_archivo
  DISPLAY "Folio Avance de Pagos Regla 27 : ", v_folio_registro
  DISPLAY "Suma Monto Amortización        : ", v_arr_tipo4.v_tot_gen_4
  DISPLAY "Total de registros Detalle     : ", v_bnd_totales
  DISPLAY "Total de registros Sumario     : ", v_arr_tipo4.v_total_registros
  DISPLAY "\n ########################################################"

  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY ""
  DISPLAY "El archivo ha sido generado exitosamente."
  DISPLAY "Ruta del archivo: ", v_ruta_nomarch
  
  --Genera una copia de la interface con el nombre corto
  CALL fn_genera_copia_interface(v_ruta_nomarch,v_ruta_envio_dis)

END FUNCTION 

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    p_busca_nom_archivo      VARCHAR(40),
    v_cmd                    STRING,
    v_consecutivo            INTEGER

  DEFINE fn                  CHAR(22)
  DEFINE ch                  base.Channel

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch    = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[15,17]
  END WHILE

  CALL ch.close()

  LET v_consecutivo = v_consecutivo + 1

  IF length(v_consecutivo) = 0 THEN
     LET v_consecutivo = 1
  END IF
   
  RETURN v_consecutivo

END FUNCTION

--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface(p_archivo_envio, p_ruta_destino)
  DEFINE 
    v_cmd                    STRING,
    p_archivo_envio          VARCHAR(100),
    p_ruta_destino           VARCHAR(40)

  LET v_archivo_copia = "avr27"
  LET v_archivo_copia = v_archivo_copia CLIPPED,".dis"     
  LET v_cmd           = "cat ", 
                        p_archivo_envio CLIPPED, " > ",
                        p_ruta_destino CLIPPED, "/", 
                        v_archivo_copia CLIPPED 
  RUN v_cmd

  --LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
  --RUN v_comando_dos

  DISPLAY "Se ha realizado la copia de la interface de Avance de Pagos Regla 27: ",v_archivo_copia
  DISPLAY ""

  DISPLAY "\n\nEjecutando scripts de transferencia de archivos (Avance de Pagos Regla 27)"
  LET v_cmd = " sh /opt/Interpel/Scripts/avr2.sh"
  --LET v_cmd = "sh ", p_ruta_destino CLIPPED, "/avr2.sh"
  RUN v_cmd
  
END FUNCTION 

#Objetivo:Asignar el 7° día del mes posterior al año del bimestre que se
#         entrega(periodo de pago)
FUNCTION fn_obtiene_f_pago(v_perido_pago)
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

END FUNCTION
