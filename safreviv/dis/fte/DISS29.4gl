################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 25/06/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS29                                                   #
#Objetivo          => Programa que genera la interface de Pago Real que se     #
#                     envía al area de Crédito o Hipotecaria Social para los   #
#                     casos de excepción a dispersar.                          #
#Fecha inicio      => 25/06/2014                                               #
################################################################################
DATABASE
  safre_viv

GLOBALS 
  DEFINE 
    v_archivo_copia          VARCHAR (25),
    v_comando_dos            STRING 
END GLOBALS 
     
MAIN
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_folio_liquida          DECIMAL(9,0),
    v_usuario                CHAR(20),
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_ruta_nomarch           VARCHAR(100), --ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --nombre del archivo de salida
    v_aaaammdd               VARCHAR(08),  --fecha del archivo de salida
    v_cont_dia               SMALLINT,     --consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_reg_folio              CHAR(09),     --Parametro folio de los casos de excepción
    v_ins_reg                STRING,       --almacena cadena a insertar en el archivo
    v_qry_txt                STRING,       --cadena para preparar consultas 
    v_busca_nom_archivo      STRING        --busca nombre de archivo
       
  DEFINE v_arr_tipo0         RECORD --Record para tipo de registro cero
    tipo_registro0           CHAR(01),
    fehca0                   CHAR(08),
    espacios0                CHAR(76)
  END RECORD
       
  DEFINE v_arr_tipo1         RECORD --Record para tipo de registro uno
    tipo_regitro1            CHAR(01),
    tipo_transaccion1        CHAR(04),
    fecha1                   CHAR(08),
    espacios1                CHAR(72)
  END RECORD
       
  DEFINE v_arr_tipo2         RECORD --Record para tipo de registro dos
    tipo_registro2           CHAR(01),
    id_dis_interface_ce      DECIMAL(9,0),
    id_derechohabiente       DECIMAL(9,0),
    nss                      CHAR(11),
    num_credito              CHAR(10),
    periodo_pago             CHAR(06),
    f_pago                   CHAR(08),
    entidad_recau            CHAR(03),
    nrp                      CHAR(11),
    monto_aportacion         DECIMAL(12,2),
    monto_amortizacion       DECIMAL(12,2),
    folio_sua                CHAR(06),
    indica_seg               CHAR(01),
    clave_rch                CHAR(10)
  END RECORD,

  c_monto_apo                CHAR(09),
  c_monto_amo                CHAR(09),
  d_f_pago                   DATE

  DEFINE v_arr_tipo3         RECORD --Record para tipo de registro tres
    v_tipo_registro3         CHAR(01),
    v_tipo_transaccion3      CHAR(04),
    v_i_registros_tipo2      INTEGER,
    v_c_registros_tipo2      CHAR(10),
    v_total_aportacion       DECIMAL(22,2),
    v_total_amortizacion     DECIMAL(22,2),
    v_c_total_seguridad      CHAR(12),
    v_d_total_seguridad      DECIMAL(10,2),
    v_total_general          DECIMAL(22,2),
    v_escacios3              CHAR(22)
  END RECORD,

  c_total_aportacion         CHAR(22),
  c_total_amortizacion       CHAR(22),
  c_total_general            VARCHAR(22)

  DEFINE v_arr_tipo4         RECORD --Record para tipo de registro cuatro
    v_tipo_registro4         CHAR(01),
    v_total_registros        INTEGER,
    v_espacios4_1            CHAR(04),
    v_tot_apo_4              DECIMAL(22,2),
    v_tot_amo_4              DECIMAL(22,2),
    v_tot_seg_4              CHAR(12),
    v_tot_gen_4              DECIMAL(22,2),
    v_espacio4_2             CHAR(22)
  END RECORD,

  c_total_registros          CHAR(10),
  c_tot_apo_4                CHAR(22),
  c_tot_amo_4                CHAR(22),
  c_tot_gen_4                VARCHAR(22)

  DEFINE 
    v_cuenta_hs              INTEGER,
    v_dia_habil              SMALLINT 
       
  --Asignación de parametros generales
  LET v_folio_liquida                  = ARG_VAL(1) --Valor de argumento uno de DISC16
  LET v_usuario                        = ARG_VAL(2) --Valor de argumento uno de DISC16
  LET v_modulo_cod                     = "dis"
  LET v_cont_dia                       = 1
  LET v_cuenta_hs                      = 0
  LET v_arr_tipo3.v_total_aportacion   = 0.00
  LET v_arr_tipo3.v_total_amortizacion = 0.00
  LET v_arr_tipo3.v_total_general      = 0.00

  PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio

  -- se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  -- se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/pagos_" --nombre de archivo
  LET v_aaaammdd          = TODAY USING "yyyymmdd" --Fecha del archivo sin separadores
  {LET v_busca_nom_archivo = "pagos_" || v_aaaammdd --Concatena nombre a buscar
   
  --Obtine consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia
        
  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_aaaammdd || v_reg_dia || "." || v_modulo_cod}

  LET v_reg_folio      = v_folio_liquida USING "&&&&&&&&&" --Folio de los casos de excepción
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_aaaammdd || v_reg_folio || "." || v_modulo_cod
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo

  -- se crea el manejador de archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  -- se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch, "w")
  CALL v_ch_arch_salida.setDelimiter("")

  LET v_arr_tipo4.v_total_registros = 0
   
  ######################################
  #####Carga  tipo de registro cero#####
  ######################################
  LET v_arr_tipo4.v_total_registros = v_arr_tipo4.v_total_registros + 1
  LET v_arr_tipo0.tipo_registro0    = "0"
  LET v_arr_tipo0.fehca0            = TODAY USING "yyyymmdd"
  LET v_arr_tipo0.espacios0         = "                                        " ||
                                      "                                    "
  LET v_ins_reg                     = v_arr_tipo0.*
  CALL v_ch_arch_salida.write([v_ins_reg])

  #####################################
  #####Carga  tipo de registro uno#####
  #####################################
  LET v_arr_tipo4.v_total_registros = v_arr_tipo4.v_total_registros + 1
  LET v_arr_tipo1.tipo_regitro1     = 1
  LET v_arr_tipo1.tipo_transaccion1 = "7080"
  LET v_arr_tipo1.fecha1            = TODAY USING "yyyymmdd"
  LET v_arr_tipo1.espacios1         = "                                        " ||
                                      "                                "
  LET v_ins_reg                     = v_arr_tipo1.*
  CALL v_ch_arch_salida.write([v_ins_reg])

  #####################################
  #####Carga  tipo de registro dos#####
  #####################################
  --Asiganción de variables para el tipo de registro
  LET v_arr_tipo2.tipo_registro2 = "2"
  LET v_arr_tipo2.entidad_recau  = "180"
  LET v_arr_tipo2.indica_seg     = " "
  LET v_arr_tipo2.clave_rch      = "          "
  LET v_dia_habil                = 6 --Para que traiga el séptimo día hábil
   
  --Consulta obtiene registros de detalle de casos de excepción a dispersar
  DECLARE cur_consulta_folio_casos_exc CURSOR FOR
  SELECT ce.id_dis_interface_ce,
         ce.nss,
         ce.num_credito,
         ce.periodo_pago,
         ce.f_pago,
         ce.nrp,
         ce.aportacion,
         ce.amortizacion,
         ce.folio_sua
  FROM   dis_caso_excepcion ce 
  WHERE  ce.folio  = v_folio_liquida
  AND    ce.estado = 0
  FOREACH cur_consulta_folio_casos_exc INTO v_arr_tipo2.id_dis_interface_ce,
                                            v_arr_tipo2.nss,
                                            v_arr_tipo2.num_credito,
                                            v_arr_tipo2.periodo_pago,
                                            d_f_pago,
                                            v_arr_tipo2.nrp,
                                            v_arr_tipo2.monto_aportacion,
                                            v_arr_tipo2.monto_amortizacion,
                                            v_arr_tipo2.folio_sua

    LET v_arr_tipo2.folio_sua            = v_arr_tipo2.folio_sua USING "&&&&&&"

    --Se da formato al número de crédito
    LET v_arr_tipo2.num_credito          = v_arr_tipo2.num_credito USING "&&&&&&&&&&"

    --Parametros para tipo de registro cuatro
    LET v_arr_tipo4.v_total_registros    = v_arr_tipo4.v_total_registros + 1
           
    --Parametros para tipo de registro tres
    LET v_arr_tipo3.v_i_registros_tipo2  = v_arr_tipo3.v_i_registros_tipo2 + 1
    LET v_arr_tipo3.v_total_aportacion   = v_arr_tipo3.v_total_aportacion +
                                           v_arr_tipo2.monto_aportacion
    LET v_arr_tipo3.v_total_amortizacion = v_arr_tipo3.v_total_amortizacion + 
                                           v_arr_tipo2.monto_amortizacion
                                                  
    --Concatenacion de registros para escribir archivo de salida
    --Obtiene fecha de pago
    --Se asigna el día 7 de la fecha de la aplicación
    LET v_arr_tipo2.f_pago               = TODAY USING "yyyymm"||"07" 
        
    LET c_monto_apo                      = (v_arr_tipo2.monto_aportacion * 100) USING "&&&&&&&&&"
    LET c_monto_amo                      = (v_arr_tipo2.monto_amortizacion * 100) USING "&&&&&&&&&"
    LET v_ins_reg                        = v_arr_tipo2.tipo_registro2,
                                           v_arr_tipo2.nss,
                                           v_arr_tipo2.num_credito,   
                                           v_arr_tipo2.periodo_pago,
                                           v_arr_tipo2.f_pago,
                                           v_arr_tipo2.entidad_recau,
                                           v_arr_tipo2.nrp,
                                           c_monto_apo,c_monto_amo,
                                           v_arr_tipo2.folio_sua,
                                           v_arr_tipo2.indica_seg,
                                           v_arr_tipo2.clave_rch

    CALL v_ch_arch_salida.write([v_ins_reg])

    UPDATE dis_caso_excepcion
    SET    estado              = 1 --Registro enviado en la interface
    WHERE  id_dis_interface_ce = v_arr_tipo2.id_dis_interface_ce;
  END FOREACH 

  INSERT INTO dis_ctr_caso_excepcion VALUES(v_folio_liquida,
                                            v_ruta_nomarch,
                                            TODAY,
                                            v_usuario,
                                            "",
                                            "",
                                            1); --Interface generada

  ######################################
  #####Carga  tipo de registro tres#####
  ######################################
  LET v_arr_tipo4.v_total_registros   = v_arr_tipo4.v_total_registros + 1
  LET v_arr_tipo3.v_c_registros_tipo2 = v_arr_tipo3.v_i_registros_tipo2 USING "&&&&&&&&&&"
  LET v_arr_tipo3.v_tipo_registro3    = "3"
  LET v_arr_tipo3.v_tipo_transaccion3 = "7080"
  LET v_arr_tipo3.v_c_total_seguridad = "000000000000"
  LET v_arr_tipo3.v_d_total_seguridad = v_arr_tipo3.v_c_total_seguridad
  LET v_arr_tipo3.v_total_general     = v_arr_tipo3.v_total_aportacion +
                                        v_arr_tipo3.v_total_amortizacion
  LET v_arr_tipo3.v_escacios3         = "                      "

  --Se actualiza programa para que muestre solo las 12 posiciones de derecha a izquierda
  #Se asigna la máscara a toda la variable
  LET c_total_aportacion              = (v_arr_tipo3.v_total_aportacion * 100) USING "&&&&&&&&&&&&&&&&&&&&&&"
  LET c_total_amortizacion            = (v_arr_tipo3.v_total_amortizacion * 100) USING "&&&&&&&&&&&&&&&&&&&&&&"
  LET c_total_general                 = (v_arr_tipo3.v_total_general * 100) USING "&&&&&&&&&&&&&&&&&&&&&&"
  LET v_ins_reg                       = v_arr_tipo3.v_tipo_registro3,
                                        v_arr_tipo3.v_tipo_transaccion3,
                                        v_arr_tipo3.v_c_registros_tipo2,
                                        c_total_aportacion[11,22], --Se imprimen los últimos 12 dígitos
                                        c_total_amortizacion[11,22],
                                        v_arr_tipo3.v_c_total_seguridad,
                                        c_total_general[11,22],
                                        v_arr_tipo3.v_escacios3
  CALL v_ch_arch_salida.write([v_ins_reg])

  ########################################
  #####Carga  tipo de registro cuatro#####
  ########################################
  LET v_arr_tipo4.v_tipo_registro4  = "4"
  LET v_arr_tipo4.v_total_registros = v_arr_tipo4.v_total_registros + 1
  LET c_total_registros             = v_arr_tipo4.v_total_registros USING "&&&&&&&&&&"
  LET v_arr_tipo4.v_espacios4_1     = "0000"
  LET v_arr_tipo4.v_tot_apo_4       = v_arr_tipo3.v_total_aportacion
  LET c_tot_apo_4                   = (v_arr_tipo4.v_tot_apo_4 * 100) USING "&&&&&&&&&&&&&&&&&&&&&&"
  LET v_arr_tipo4.v_tot_amo_4       = v_arr_tipo3.v_total_amortizacion
  LET c_tot_amo_4                   = (v_arr_tipo4.v_tot_amo_4 * 100) USING "&&&&&&&&&&&&&&&&&&&&&&"
  LET v_arr_tipo4.v_tot_gen_4       = v_arr_tipo3.v_total_general
  LET c_tot_gen_4                   = (v_arr_tipo4.v_tot_gen_4 * 100) USING "&&&&&&&&&&&&&&&&&&&&&&"
  LET v_arr_tipo4.v_tot_seg_4       = v_arr_tipo3.v_c_total_seguridad
  LET v_arr_tipo4.v_espacio4_2      = "                      "
  LET v_ins_reg                     = v_arr_tipo4.v_tipo_registro4,
                                      c_total_registros,
                                      v_arr_tipo4.v_espacios4_1,
                                      c_tot_apo_4[11,22],
                                      c_tot_amo_4[11,22],
                                      v_arr_tipo4.v_tot_seg_4,
                                      c_total_general[11,22],
                                      v_arr_tipo4.v_espacio4_2
  CALL v_ch_arch_salida.write([v_ins_reg])

  CALL v_ch_arch_salida.close()

  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos
   
  --Genera una copia de la interface con el nombre corto
  CALL fn_genera_copia_interface_real(v_ruta_nomarch, v_ruta_envio_dis)
   
END MAIN

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40), --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,      --Cadena de comando a ejecutar
    v_consecutivo            INTEGER      --Consecutivo del archivo por día
  DEFINE fn                  CHAR(22)     --Almacena el nombre completo del nombre del archivo en el servidor con su extensión
  DEFINE ch                  base.Channel --Canal de lectura

  LET v_cmd = "ls -lrt ", p_ruta_envio_dis CLIPPED, "/ | grep -i '", p_busca_nom_archivo CLIPPED, "' |awk '{print $9}'"
  LET ch    = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")
  WHILE ch.read([fn])
    LET v_consecutivo = fn[15,17]  --Posición del consecutivo dentro de la cadena
  END WHILE
  CALL ch.close()

  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION

--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface_real(p_archivo_envio, p_ruta_destino)
  DEFINE 
    v_cmd                    STRING,
    p_archivo_envio          VARCHAR(100),
    p_ruta_destino           VARCHAR(40)

  LET v_archivo_copia = "pagos"
  LET v_archivo_copia = v_archivo_copia CLIPPED,".dis" 
    
  LET v_cmd           = "cat ", p_archivo_envio CLIPPED, " > ", p_ruta_destino CLIPPED, "/", v_archivo_copia CLIPPED 
  RUN v_cmd

  LET v_comando_dos   = "unix2dos ", p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
  RUN v_comando_dos
    
END FUNCTION