################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 05/09/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS08                                                   #
#Objetivo          => Programa que genera la interface de las diferencias      #
#                     positivas por avance cuando la aportación o la           #
#                     amortización son mayores a $2 (Cargo para HS)            #
#Fecha inicio      => 14/12/2012                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_folio                  LIKE dis_preliquida.folio_liquida,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_periodo                STRING,
    v_query                  STRING,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_index                  INTEGER,
    v_bimestre               SMALLINT 

  --Variables para el tipo de registro 0
  DEFINE 
    v_tipo_registro0         SMALLINT,
    v_fecha0                 CHAR(8) 

  --Variables para el tipo de registro 1
  DEFINE 
    v_tipo_registro1         SMALLINT,
    v_tipo_transaccion1      CHAR(4),
    v_fecha1                 CHAR(8)

  --Variables para el tipo de registro 2
  DEFINE 
    v_tipo_registro2         SMALLINT,
    v_nss                    LIKE afi_derechohabiente.nss,
    v_numero_credito         LIKE dis_compensa_avance.num_credito,
    c_numero_credito         CHAR(10),
    v_periodo_pago           CHAR(6),
    v_fecha_p                DATE,
    v_fecha_pago             CHAR(8),
    v_entidad                CHAR(3),
    v_nrp                    LIKE dis_compensa_avance.nrp,
    v_importe_apo            CHAR(9),
    v_edo_compensa_apo       SMALLINT,
    v_edo_compensa_amo       SMALLINT,
    v_importe_apo2           DECIMAL(22,2),
    v_importe_amo2           DECIMAL(22,2),
    c_importe_apo2           CHAR(9),
    c_importe_amo2           CHAR(9),
    v_folio_sua1             CHAR(6),
    v_folio_sua2             CHAR(6),
    v_indicador_seg          CHAR(1),
    v_cve_rechazo            CHAR(10)

  --Variables para el tipo de registro 3
  DEFINE 
    v_tipo_registro3         SMALLINT,
    v_tipo_transaccion3      CHAR(4),
    v_tot_registros3         INTEGER,
    c_tot_registros3         CHAR(10),
    v_tot_aportacion3        CHAR(12),
    v_tot_amortizacion3      DECIMAL(22,2),
    c_tot_amortizacion3      CHAR(12),
    c_tot_aportacion3        CHAR(12),
    v_tot_seguridad3         CHAR(12),
    v_tot_general3           CHAR(12)

  --Variables para el tipo de registro 4
  DEFINE
    v_tipo_registro4         SMALLINT,
    v_tot_registros4         INTEGER,
    c_tot_registros4         CHAR(10),
    v_tot_aportacion4        CHAR(12),
    v_tot_amortizacion4      DECIMAL(22,2),
    c_tot_amortizacion4      CHAR(12),
    v_tot_seguridad4         CHAR(12),
    v_tot_general4           CHAR(12)
      
  DEFINE 
    v_cuenta_ava_apo         INTEGER,
    v_cuenta_ava_amo         INTEGER,
    v_QryTxt                 STRING,
    v_dia_habil              SMALLINT,
    v_importe_total_apo      DECIMAL(22,2),
    v_importe_total_amo      DECIMAL(22,2),
    p_proceso_cod            SMALLINT 

  DEFINE 
    v_archivo_copia          VARCHAR(25),
    v_comando_dos            STRING,
    v_usuario                CHAR(20)

END GLOBALS 

MAIN 
  --Asignación de parametros generales
  LET v_folio       = ARG_VAL(1) -- Valor de folio
  LET p_proceso_cod = ARG_VAL(2) -- Valor de proceso_cod
  LET v_modulo_cod  = "dis"

  LET v_cont_dia          = 1
  LET v_cuenta_ava_apo    = 0
  LET v_cuenta_ava_amo    = 0
  LET v_importe_total_apo = 0.00
  LET v_importe_total_amo = 0.00
   
  IF p_proceso_cod = 901 OR 
     p_proceso_cod = 914 THEN --Si viene de liquidación generamos la interfaz con dis_compensa_avance 
     CALL fn_crea_interface_liquidacion_esp()
     EXIT PROGRAM  
  ELSE 
     EXIT PROGRAM 
  END IF 

END MAIN 

--Objetivo: Genera la interfaz de las diferencias positivas cuando la llamada viene de 
--la liquidación de dispersion
FUNCTION fn_crea_interface_liquidacion_esp()
  DEFINE
    v_estado                 INTEGER, 
    r_bnd_proceso            INTEGER, 
    v_status_err             INTEGER, 
    v_desc_err               CHAR(50),
    p_opera_cod              INTEGER, 
    v_destino_dis            INTEGER  

  LET v_estado      = 101
  LET r_bnd_proceso = 0 
  LET p_opera_cod   = 1
  LET v_destino_dis = 10
  LET v_usuario     = "SAFREVIV"
  
  --Valida que exista información para generar la interface
  --Valida aportaciones
  SELECT COUNT(*)
  INTO   v_cuenta_ava_apo 
  FROM   dis_compensa_avance di, dis_det_avance_pago av
  WHERE  folio_dis                 = v_folio
  AND    av.id_dis_det_avance_pago = di.id_dis_det_avance_pago
  AND    di.edo_compensa_apo       = 1
  AND    av.monto_dif_apo          > 2
  --Si la variable sigue nula la pone a cero
  IF v_cuenta_ava_apo IS NULL THEN 
     LET v_cuenta_ava_apo = 0 
  END IF
   
  --Valida amortizaciones
  SELECT COUNT(*)
  INTO   v_cuenta_ava_amo 
  FROM   dis_compensa_avance di, dis_det_avance_pago av
  WHERE  folio_dis                 = v_folio
  AND    av.id_dis_det_avance_pago = di.id_dis_det_avance_pago
  AND    di.edo_compensa_amo       = 1
  AND    av.monto_dif_amo          > 2 
  --Si la variable sigue nula la pone a cero
  IF v_cuenta_ava_amo IS NULL THEN 
     LET v_cuenta_ava_amo = 0 
  END IF
   
  LET v_cuenta_ava_amo = v_cuenta_ava_amo + v_cuenta_ava_apo
   
  IF v_cuenta_ava_amo = 0 OR v_cuenta_ava_amo IS NULL THEN
     DISPLAY "No se generó la interface de Cargo para HS (>$2) por falta de información"
     EXIT PROGRAM 
  END IF 
   
  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se obtiene el usuario del folio liquidado
  SELECT a.usuario
  INTO   v_usuario
  FROM   glo_folio a
  WHERE  a.folio = v_folio

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/esp_cargo_avance_hs_"
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"
  LET v_busca_nom_archivo = "esp_cargo_avance_hs_" || v_ddmmaaaa

  --Obtine consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia      = v_cont_dia USING "&&&"
  LET v_nom_archivo  = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  WHENEVER ERROR CONTINUE

  PREPARE prp_fn_transaccion24
  FROM    "EXECUTE FUNCTION sp_dis_transaccion24(?,?,?,?,?,?,?)"
  EXECUTE prp_fn_transaccion24 INTO r_bnd_proceso, v_status_err, v_desc_err                                    
                              USING p_proceso_cod,
                                    p_opera_cod, 
                                    v_nom_archivo, 
                                    v_folio,
                                    v_estado,
                                    v_destino_dis,
                                    v_usuario

  WHENEVER ERROR STOP 

  DISPLAY "Función Transaccion 24 ",r_bnd_proceso
  DISPLAY "Código:",v_status_err         
  DISPLAY "mensaje", v_desc_err
         
  IF r_bnd_proceso <> 0 THEN
    DISPLAY "Error en la transacción 24 ",v_status_err," ",v_desc_err
    EXIT PROGRAM
  END IF

  --Se crea el manejador de archivo
  LET v_ch_arch_salida = base.Channel.create()

  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch, "w")
  CALL v_ch_arch_salida.setDelimiter("")

  ###############  Carga  tipos de registro 0  #####################
  --Inicialización de variables tipo 0
  LET v_tipo_registro0 = 0
  LET v_fecha0         = TODAY USING "yyyymmdd"

  --Escribe en archivo tipo de registro 0
  CALL v_ch_arch_salida.writeLine(v_tipo_registro0||
                                  v_fecha0||
                                  "                                                                            " --76 espacios
                                 )

  ###############  Carga  tipos de registro 1  #####################
  --Inicialización de variables tipo 1
  LET v_tipo_registro1    = 1  
  LET v_tipo_transaccion1 = "8819"  
  LET v_fecha1            = TODAY USING "yyyymmdd"

  --Escribe en archivo tipo de registro 1
  CALL v_ch_arch_salida.writeLine(v_tipo_registro1|| 
                                  v_tipo_transaccion1||  
                                  v_fecha1||
                                  "                                                                        " --72 espacios
                                 )

  ###############  Carga  tipos de registro 2  #####################
  LET v_index     = 0
  LET v_dia_habil = 6

  ### Se agrega tabla temporal para mejorar performance en lectura a la BD ###
  WHENEVER ERROR CONTINUE 
    DROP TABLE tmp_dis_interface_dp_esp;

    SELECT di.folio_dis, 
           af.nss, 
           di.num_credito, 
           di.f_pago,
           di.periodo_pago, 
           di.nrp,
           di.edo_compensa_apo, 
           di.edo_compensa_amo,
           av.monto_dif_apo, 
           av.monto_dif_amo  
    FROM   dis_compensa_avance di, 
           afi_derechohabiente af, 
           dis_det_avance_pago av 
    WHERE  folio_dis                     = v_folio
    AND    af.id_derechohabiente         = di.id_derechohabiente 
    AND    av.id_dis_det_avance_pago     = di.id_dis_det_avance_pago 
    AND   ((di.edo_compensa_apo IN (1) AND av.monto_dif_apo > 2)
    OR     (di.edo_compensa_amo IN (1) AND av.monto_dif_amo > 2))
    INTO TEMP tmp_dis_interface_dp_esp
  
    UPDATE STATISTICS FOR TABLE tmp_dis_interface_dp_esp

    #############################################################################
    DECLARE cur_consulta CURSOR FOR
    SELECT *
    FROM   tmp_dis_interface_dp_esp
    FOREACH cur_consulta INTO v_folio,
                              v_nss, 
                              v_numero_credito,
                              v_fecha_p,
                              v_periodo_pago, 
                              v_nrp,
                              v_edo_compensa_apo, 
                              v_edo_compensa_amo,
                              v_importe_apo2,
                              v_importe_amo2

      --Inicialización de variables tipo 2
      LET v_tipo_registro2 = 2
      LET v_nss            = v_nss USING "&&&&&&&&&&&"
      LET c_numero_credito = v_numero_credito USING "&&&&&&&&&&"
      LET v_fecha_pago     = TODAY USING "yyyymm"||"07" 
      LET v_periodo        = v_periodo_pago USING "&&&&&&"
      LET v_bimestre       = v_periodo.subString(5,6)
      LET v_entidad        = "197" --Diferencias Positivas – Cargo a Operación Cartera
      LET v_importe_apo    = "000000000"

      --Se obtiene valor absoluto de aportaciones 
      LET c_importe_apo2   = (v_importe_apo2 * 100) USING "&&&&&&&&&"
      LET c_importe_amo2   = (v_importe_amo2 * 100) USING "&&&&&&&&&"
      LET v_folio_sua1     = "999000"     --Aportaciones
      LET v_folio_sua2     = "888000"     --Amortizaciones
      LET v_indicador_seg  = " "
      LET v_cve_rechazo    = "          " --10 blancos 

      --Escribe en archivo tipo de registro 2
      IF v_edo_compensa_apo = 1 AND (v_importe_apo2 > 2 OR v_importe_apo2*(-1) < -2) THEN 
         CALL v_ch_arch_salida.writeLine(v_tipo_registro2||
                                         v_nss||
                                         c_numero_credito||
                                         v_periodo||
                                         v_fecha_pago||
                                         v_entidad||
                                         v_nrp||
                                         v_importe_apo||
                                         c_importe_apo2||
                                         v_folio_sua1|| 
                                         v_indicador_seg||
                                         v_cve_rechazo
                                        )

         LET v_importe_total_apo = v_importe_total_apo + v_importe_apo2
         LET v_index             = v_index + 1                           
      END IF 

      IF v_edo_compensa_amo = 1  AND (v_importe_amo2 > 2 OR v_importe_amo2*(-1) < -2) THEN
         CALL v_ch_arch_salida.writeLine(v_tipo_registro2||
                                         v_nss||
                                         c_numero_credito||
                                         v_periodo||
                                         v_fecha_pago||
                                         v_entidad||
                                         v_nrp||
                                         v_importe_apo||
                                         c_importe_amo2||
                                         v_folio_sua2||
                                         v_indicador_seg||
                                         v_cve_rechazo
                                        )

         LET v_importe_total_amo = v_importe_total_amo + v_importe_amo2
         LET v_index             = v_index + 1
      END IF 
    END FOREACH 

    ###############  Carga  tipos de registro 3  #####################
    --Asignación de variables tipo 3
    LET v_tipo_registro3    = 3
    LET v_tipo_transaccion3 = "8819"
    LET c_tot_registros3    = (v_index) USING "&&&&&&&&&&"
    LET c_tot_aportacion3   = "000000000000"
    LET c_tot_amortizacion3 = (v_importe_total_amo * 100 +
                               v_importe_total_apo * 100) USING "&&&&&&&&&&&&"
    LET v_tot_seguridad3    = "000000000000"
    LET v_tot_general3      = c_tot_amortizacion3

    --Escribe en archivo tipo de registro 3
    CALL v_ch_arch_salida.writeLine(v_tipo_registro3|| 
                                    v_tipo_transaccion3||  
                                    c_tot_registros3||
                                    c_tot_aportacion3||
                                    c_tot_amortizacion3||
                                    v_tot_seguridad3||
                                    v_tot_general3||
                                    "                      " --22 espacios
                                   )

    ###############  Carga  tipos de registro 4  #####################
    --Asignación de variables tipo 4
    LET v_tipo_registro4    = 4
    LET v_tot_registros4    = 4
    LET c_tot_registros4    = (v_tot_registros4 + v_index) USING "&&&&&&&&&&"
    LET c_tot_amortizacion4 = (v_importe_total_amo * 100 +
                               v_importe_total_apo * 100) USING "&&&&&&&&&&&&"
    LET v_tot_seguridad4    = "000000000000"
    LET v_tot_general4      = c_tot_amortizacion4

    --Escribe en archivo tipo de registro 4
    CALL v_ch_arch_salida.writeLine(v_tipo_registro4||
                                    c_tot_registros4||
                                    "0000"|| --4 ceros
                                    c_tot_aportacion3||
                                    c_tot_amortizacion4||
                                    v_tot_seguridad4||
                                    v_tot_general4||
                                    "                      "
                                   )   

    CALL v_ch_arch_salida.close()

    --Cambia el formato del archivo a DOS
    --LET v_comando_dos = "unix2dos ", v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
    --RUN v_comando_dos

    DISPLAY "Se ha generado el archivo de Cargo a HS (>$2) correctamente: ", v_nom_archivo

    --Genera una copia de la interface con el nombre corto
    CALL fn_genera_copia_interface_dif(v_ruta_nomarch, v_ruta_envio_dis)

END FUNCTION 
 
#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    p_busca_nom_archivo      VARCHAR(40),
    v_cmd                    STRING,
    v_consecutivo            INTEGER

  DEFINE 
    fn                       CHAR(31)
  
  DEFINE 
    ch                       base.Channel

  LET v_cmd = "ls -lrt ", p_ruta_envio_dis CLIPPED, "/ | grep -i '", p_busca_nom_archivo CLIPPED, "' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[25,27]
  END WHILE

  CALL ch.close()

  LET v_consecutivo = v_consecutivo + 1

  IF length(v_consecutivo) = 0 THEN
     LET v_consecutivo = 1
  END IF
    
  RETURN v_consecutivo

END FUNCTION

--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface_dif(p_archivo_envio, p_ruta_destino)
  DEFINE 
    v_cmd                    STRING,
    p_archivo_envio          VARCHAR(100),
    p_ruta_destino           VARCHAR(40)

  LET v_archivo_copia = "esp_cargo_avance_hs"
  LET v_archivo_copia = v_archivo_copia CLIPPED,".dis" 
    
  LET v_cmd = "cat ", p_archivo_envio CLIPPED, " > ", p_ruta_destino CLIPPED, "/", v_archivo_copia CLIPPED 
  RUN v_cmd

  --Cambia el formato del archivo a DOS
  --LET v_comando_dos = "unix2dos ", p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
  --RUN v_comando_dos
    
  DISPLAY "Se ha realizado la copia de la interface de Cargo a HS (>$2): ", v_archivo_copia

  DISPLAY "\n\nEjecutando scripts de transferencia de archivos (Cargo a HS >$2)"
  LET v_cmd = " sh /opt/Interpel/Scripts/cargo_avance_hs.sh"
  --LET v_cmd = "sh ", p_ruta_destino CLIPPED, "/cargo_avance_hs.sh"
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
