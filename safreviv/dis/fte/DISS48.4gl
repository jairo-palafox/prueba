################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 16/08/2018                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISS48                                                    #
#Objetivo         => Generar el extractor de pagos sin dispersión.             #
#Fecha de Inicio  => 16/08/2018                                                #
################################################################################
DATABASE safre_viv
GLOBALS

  --Sección de variables del programa
  DEFINE 
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    p_proceso_cod            LIKE cat_proceso.proceso_cod,
    p_opera_cod              LIKE cat_operacion.opera_cod,
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_tot_registros          DECIMAL(9,0), --Total de registros
    v_sum_apo_ava            DECIMAL(22,2), 
    v_sum_amo_ava            DECIMAL(22,2),
    v_sum_apo_pag            DECIMAL(22,2), 
    v_sum_amo_pag            DECIMAL(22,2)
    
  --Record para información del detalle de avances vs pagos compensación
  DEFINE v_reg_ava_vs_comp   RECORD
    v_reg_folio_ava          DECIMAL(9,0),
    v_reg_nss                CHAR(11),
    v_reg_num_credito        DECIMAL(10,0),
    v_reg_periodo_pago       CHAR(6),
    v_reg_nrp                CHAR(11),     
    v_reg_monto_apo_ava      DECIMAL(12,2),
    v_reg_monto_amo_ava      DECIMAL(12,2), 
    v_reg_monto_apo_pag      DECIMAL(12,2),
    v_reg_monto_amo_pag      DECIMAL(12,2), 
    v_reg_f_pago             DATE, 
    v_reg_edo_comp_apo       VARCHAR(30),    
    v_reg_edo_comp_amo       VARCHAR(30),    
    v_reg_edo_avance         VARCHAR(50) 
  END RECORD

  --Variables auxiliares
  DEFINE 
    g_sql_txt                STRING,
    v_qwery_ibx              STRING, 
    v_folio_dis              VARCHAR(9),
    v_nss                    CHAR(11),
    v_nrp                    CHAR(11),
    v_periodo_pago           CHAR(6),
    v_monto_apo_avance       VARCHAR(14),
    v_monto_amo_avance       VARCHAR(14),
    v_folio_pago             VARCHAR(9),
    v_monto_apo_pag          VARCHAR(14),
    v_monto_amo_pag          VARCHAR(14),
    v_folio_sua              VARCHAR(9),
    v_f_pago                 VARCHAR(8),
    v_edo_comp_apo           SMALLINT,
    v_edo_comp_amo           SMALLINT,
    v_desc_edo_comp_apo      VARCHAR(30),
    v_desc_edo_comp_amo      VARCHAR(30)
      
  DEFINE 
    f_folio                  DECIMAL(9,0),
    f_id_derechohabiente     DECIMAL(9,0),
    f_nss                    CHAR(11),
    f_nom_completo           CHAR(150),
    v_total_registros        BIGINT,
    f_tot_ava_apo            DECIMAL(22,2),
    f_tot_ava_amo            DECIMAL(22,2),
    f_tot_pag_apo            DECIMAL(22,2),
    f_tot_pag_amo            DECIMAL(22,2)

  DEFINE 
    v_tot_ava_apo            VARCHAR(22),
    v_tot_ava_amo            VARCHAR(22),
    v_tot_pag_apo            VARCHAR(22),
    v_tot_pag_amo            VARCHAR(22)

  DEFINE 
    v_folio_reg_pag          DECIMAL(9,0)

  DEFINE
    v_archivo_copia          VARCHAR (30),
    v_comando_dos            STRING

  DEFINE
    v_total_mov              DECIMAL(9,0),
    v_total_nss              DECIMAL(9,0),
    v_total_saldo_apo        DECIMAL(22,2),
    v_total_saldo_amo        DECIMAL(22,2),
    v_total_sal_apo_amo      DECIMAL(22,2),
    p_nom_archivo            VARCHAR(55)  --Nombre del archivo de salida

  DEFINE v_origen_datos      STRING    
  DEFINE v_ruta_reporte      STRING -- ruta del archivo del reporte
  DEFINE v_ruta_listados     STRING -- ruta de los listados
  DEFINE v_ruta_ejecutable   STRING -- ruta del ejecutable

END GLOBALS 

MAIN
  DEFINE 
    r_bandera                SMALLINT
    
  --Asignación de parametros generales
  LET p_usuario            = ARG_VAL(1)
  LET p_pid                = ARG_VAL(2)
  LET p_proceso_cod        = ARG_VAL(3)
  LET p_opera_cod          = ARG_VAL(4)
  LET f_folio              = ARG_VAL(5) --Valor de argumento uno de DISL66
  LET f_id_derechohabiente = ARG_VAL(6) --Valor de argumento dos de DISL66
  LET f_nss                = ARG_VAL(7) --Valor de argumento tres de DISL66
  LET f_nom_completo       = ARG_VAL(8) --Valor de argumento cuatro de DISL66

  --CALL fn_existe_nss(f_nss)

  LET v_modulo_cod = "dis"

  LET v_total_registros = 0

  DISPLAY "Inicia la generación del extractor de pagos sin dispersión. ", TIME
  DISPLAY ""

  DISPLAY "   Inicia etapa 1 identificación de datos. ", TIME
  CALL fn_crea_temporal_folio()
  DISPLAY "   Finaliza etapa 1 identificación de datos. ", TIME

  DISPLAY ""
  DISPLAY "   Inicia etapa 2 salida de datos. ", TIME
  CALL fn_genera_ext_pag_sin_dis()
  DISPLAY "   Finaliza etapa 2 salida de datos. ", TIME

  DISPLAY ""
  DISPLAY "   Inicia etapa 3 genera reporte. ", TIME
  CALL fn_genera_rep_epsd()
  DISPLAY "   Finaliza etapa 3 genera reporte. ", TIME

  --Finaliza la operación
  CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
  RETURNING r_bandera

  IF r_bandera = 0 THEN
     DISPLAY ""
     DISPLAY "Finalizó la generación del extractor de pagos sin dispersión. ", TIME
     EXIT PROGRAM
  ELSE --Si ocurrió error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
     CALL fn_desplega_inc_operacion(r_bandera)
     EXIT PROGRAM
  END IF

END MAIN

#Función para crear la temporal con el filtro de entrada
FUNCTION fn_crea_temporal_folio()
  DEFINE r_bnd               INTEGER,
         v_status_err        INTEGER ,
         v_desc_err          VARCHAR(200)

  WHENEVER ERROR CONTINUE
    PREPARE ps_sp_dis_hs FROM "EXECUTE PROCEDURE sp_ext_pag_sin_dis(?,?)"
    EXECUTE ps_sp_dis_hs USING f_folio, f_id_derechohabiente
                          INTO r_bnd, v_status_err, v_desc_err
  WHENEVER ERROR STOP

  DISPLAY "     Función Ext Pag sin Dis ",r_bnd
  DISPLAY "     Código                  ",v_status_err
  DISPLAY "     Mensaje                 ",v_desc_err

  IF r_bnd <> 0 THEN
    DISPLAY "Error en el Ext Pag sin Dis ",v_status_err," ",v_desc_err
    EXIT PROGRAM
  END IF

END FUNCTION 

# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_ext_pag_sin_dis()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo_orig       VARCHAR(55),  --Nombre del archivo de salida orig
    v_nom_archivo            VARCHAR(55),  --Nombre del archivo de salida
    v_nom_archivo_enc        VARCHAR(55),  --Nombre del archivo de salida enc
    v_nom_archivo_det        VARCHAR(55),  --Nombre del archivo de salida det
    v_nom_archivo_sum        VARCHAR(55),  --Nombre del archivo de salida sum
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_indice                 INTEGER

  DEFINE
    v_ch_arch_salida_enc     BASE.CHANNEL,
    v_ruta_nomarch_enc       VARCHAR(100), --Ruta y nombre del archivo de salida enc
    v_ruta_nomarch_det       VARCHAR(100), --Ruta y nombre del archivo de salida det
    v_ruta_nomarch_sum       VARCHAR(100)  --Ruta y nombre del archivo de salida sum 

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  --LET v_nom_archivo       = "/ava_compensa_"       --nombre de archivo
  LET v_nom_archivo       = "/ext_pagos_sin_dispersion_"    --nombre de archivo
  LET v_nom_archivo_orig  = v_nom_archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy" --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "ext_pagos_sin_dispersion_" || v_ddmmaaaa --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  --CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
  --RETURNING v_cont_dia

  --LET v_reg_dia         = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  --LET v_nom_archivo     = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_nom_archivo     = v_nom_archivo CLIPPED || v_ddmmaaaa || ".txt"
  LET p_nom_archivo     = v_nom_archivo
  LET v_ruta_nomarch    = v_ruta_envio_dis CLIPPED || v_nom_archivo

  --ENCABEZADO
  --LET v_ch_arch_salida     = base.Channel.create()
  {LET v_nom_archivo_enc    = v_nom_archivo_orig CLIPPED || v_ddmmaaaa || v_reg_dia||"_enc."|| v_modulo_cod
  LET v_ruta_nomarch_enc   = v_ruta_envio_dis CLIPPED || v_nom_archivo_enc
  --DISPLAY "Ruta encabezado: ", v_ruta_nomarch_enc
  LET v_ch_arch_salida_enc = base.Channel.create()

  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida_enc.openFile(v_ruta_nomarch_enc,"w" )
  CALL v_ch_arch_salida_enc.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE |", TODAY USING "dd-mm-yyyy"
  CALL v_ch_arch_salida_enc.write([v_encabezado])

  --Si se solicitó el folio de dispersión se incluye en el encabezado
  IF f_folio <> 0 THEN
     DISPLAY "Folio Avance de Pagos ", f_folio
     DISPLAY " "
     LET v_encabezado = " FOLIO AVANCE |", f_folio
     CALL v_ch_arch_salida_enc.write([v_encabezado])
  ELSE
     LET v_encabezado = " ARCHIVO MASIVO |"
     CALL v_ch_arch_salida_enc.write([v_encabezado])
  END IF
 
  LET v_encabezado = " FOLIO AVANCE |NSS |NÚMERO CRÉDITO AVANCE |PERIODO PAGO AVANCE |NRP AVANCE |MONTO APORTACIÓN AVANCE |MONTO AMORTIZACIÓN AVANCE |MONTO APORTACIÓN PAGO |MONTO AMORTIZACIÓN PAGO |FECHA PAGO |ESTADO COMPENSACIÓN APORTACIÓN |ESTADO COMPENSACIÓN AMORTIZACIÓN |ESTADO AVANCE |"
  CALL v_ch_arch_salida_enc.write([v_encabezado])

  --Cierra el archivo ENCABEZADO
  CALL v_ch_arch_salida_enc.close()

  LET v_comando_dos = "cat ", v_ruta_nomarch_enc CLIPPED, " >> ", v_ruta_nomarch CLIPPED
  RUN v_comando_dos

  --DETALLE
  LET v_nom_archivo_det  = v_nom_archivo_orig CLIPPED || v_ddmmaaaa || v_reg_dia||"_det."|| v_modulo_cod
  LET v_ruta_nomarch_det = v_ruta_envio_dis CLIPPED || v_nom_archivo_det
  --DISPLAY "Ruta detalle: ", v_ruta_nomarch_det}
  
  --UNLOAD TO v_ruta_nomarch_det

  SELECT a.nss,
         b.subcuenta,
         b.monto_acciones,
         b.monto_pesos, 
         TO_CHAR(b.f_liquida, "%d"  ||'-'||  "%m" ||"-" || "%Y") AS f_liquida
         --b.f_liquida
  FROM   tmp_pag_sin_dis b, OUTER afi_derechohabiente a
  WHERE  b.id_derechohabiente = a.id_derechohabiente
  --ORDER BY b.f_liquida DESC, b.subcuenta 
  INTO TEMP tmp_pag_sin_dis_sal

  UPDATE STATISTICS FOR TABLE tmp_pag_sin_dis_sal
         
  UNLOAD TO v_ruta_nomarch
  SELECT a.nss,
         a.subcuenta,
         a.monto_acciones,
         a.monto_pesos,
         a.f_liquida
         --TO_CHAR(a.f_liquida, "%d"  ||'-'||  "%m" ||"-" || "%Y") AS f_liquida
  FROM   tmp_pag_sin_dis_sal a
  --ORDER BY a.f_liquida desc, a.subcuenta 
  
  {LET v_comando_dos = "cat ", v_ruta_nomarch_det CLIPPED, " >> ", v_ruta_nomarch CLIPPED
  RUN v_comando_dos
  
  --SUMARIO
  LET v_nom_archivo_sum  = v_nom_archivo_orig CLIPPED || v_ddmmaaaa || v_reg_dia||"_sum."|| v_modulo_cod
  LET v_ruta_nomarch_sum = v_ruta_envio_dis CLIPPED || v_nom_archivo_sum
  --DISPLAY "Ruta sumario: ", v_ruta_nomarch_sum
  
  UNLOAD TO v_ruta_nomarch_sum
  SELECT "TOTALES",
         COUNT(*),
         "","","",
         SUM(monto_aportacion),
         SUM(monto_amortizacion),
         SUM(monto_apo_pag),
         SUM(monto_amo_pag)
  FROM   tmp_dis_ava_vs_comp_int
  --WHERE  estado           like "70%"
  --AND    f_pago           between '01012016' and '12312016'
  --AND    nss = '49806120462'

  LET v_comando_dos = "cat ", v_ruta_nomarch_sum CLIPPED, " >> ", v_ruta_nomarch CLIPPED
  RUN v_comando_dos

  LET v_comando_dos = "rm ", v_ruta_nomarch_enc CLIPPED
  RUN v_comando_dos

  LET v_comando_dos = "rm ", v_ruta_nomarch_det CLIPPED
  RUN v_comando_dos

  LET v_comando_dos = "rm ", v_ruta_nomarch_sum CLIPPED
  RUN v_comando_dos
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos}

  DISPLAY "     Se ha generado el archivo de Pagos sin Dispersión\n     en la ruta ",v_ruta_nomarch

  --Genera una copia de la interface con el nombre corto
  --CALL fn_genera_copia_interface_ava_c(v_ruta_nomarch,v_ruta_envio_dis)
   
END FUNCTION 

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,       --Cadena de comando a ejecutar
    v_consecutivo            INTEGER       --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(24)      --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    --LET v_consecutivo = fn[22,24] --Posición del consecutivo dentro de la cadena
    LET v_consecutivo = fn[22,24] --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION

--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface_ava_c(p_archivo_envio,p_ruta_destino)
  DEFINE
    v_cmd                    STRING,
    p_archivo_envio          VARCHAR(100),
    p_ruta_destino           VARCHAR(40)

  LET v_archivo_copia = "ava_compensa"
  LET v_archivo_copia = v_archivo_copia CLIPPED,".dis"

  LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED
  RUN v_cmd

  LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha realizado la copia de la interface de Avances de Pagos vs Pagos Compensación: ",v_archivo_copia

END FUNCTION

FUNCTION fn_genera_rep_epsd()
  DEFINE manejador_rpt om.SaxDocumentHandler --Contenedor documentos reporte
  DEFINE v_indice_reporte    SMALLINT
  DEFINE v_indice_fin        SMALLINT 

  LET v_indice_reporte = 1
  LET v_indice_fin     = 1
  LET v_origen_datos   = p_usuario

  -- Genera el reporte en PDF
  {IF fgl_report_loadCurrentSettings("DISS481.4rp") THEN
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF}
  
  --Se construye la ruta del archivo
  CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
  LET v_ruta_reporte = v_ruta_listados.trim(),
                       "/",
                       v_origen_datos.trim(),"-",
                       "DISS48","-",
                       p_pid USING "&&&&&","-",
                       p_proceso_cod USING "&&&&&","-",
                       p_opera_cod USING "&&&&&",".pdf"

  DISPLAY "     Ruta del reporte: ",v_ruta_reporte
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISS481.4rp") THEN

     CALL fgl_report_selectDevice ("PDF")
     CALL fgl_report_selectPreview(0)
     CALL fgl_report_setOutputFileName(v_ruta_reporte)

     LET manejador_rpt = fgl_report_commitCurrentSettings()

     CALL fn_obtiene_detalle_reporte()                                

     --Inicia el reporte
     START REPORT rep_con_disp TO XML HANDLER manejador_rpt
       FOR v_indice_reporte = 1 TO v_indice_fin
           OUTPUT TO REPORT rep_con_disp (p_nom_archivo,
                                          f_folio,
                                          p_usuario,
                                          v_total_mov,
                                          v_total_nss,
                                          v_total_saldo_apo,
                                          v_total_saldo_amo,
                                          v_total_sal_apo_amo)
       END FOR
     FINISH REPORT rep_con_disp
  END IF

END FUNCTION

--Objetivo: Obtiene detalle del reporte para complementarlo
FUNCTION fn_obtiene_detalle_reporte()

  LET v_total_mov         = 0
  LET v_total_nss         = 0
  LET v_total_saldo_apo   = 0
  LET v_total_saldo_amo   = 0
  LET v_total_sal_apo_amo = 0

  SELECT COUNT(UNIQUE id_derechohabiente), COUNT(*)
  INTO   v_total_nss, v_total_mov
  FROM   tmp_pag_sin_dis
  IF v_total_mov IS NULL THEN
     LET v_total_mov = 0
  END IF

  IF v_total_nss IS NULL THEN
     LET v_total_nss = 0
  END IF
   
  SELECT SUM(monto_pesos * -1)
  INTO   v_total_saldo_apo
  FROM   tmp_pag_sin_dis
  WHERE  subcuenta = 4
  IF v_total_saldo_apo IS NULL THEN
     LET v_total_saldo_apo = 0
  END IF
   
  SELECT SUM(monto_pesos * -1)
  INTO   v_total_saldo_amo
  FROM   tmp_pag_sin_dis
  WHERE  subcuenta = 41
  IF v_total_saldo_amo IS NULL THEN
     LET v_total_saldo_amo = 0
  END IF

  LET v_total_sal_apo_amo = v_total_saldo_apo + v_total_saldo_amo  

  --Despliega información en el log
  DISPLAY "\n     ############### EXTRACTOR PAGOS SIN DISPERSIÓN ###############"
  DISPLAY "          Total movimientos                                  : ", v_total_mov         USING "-,---,--&"
  DISPLAY "          Total NSS                                          : ", v_total_nss         USING "-,---,--&"
  DISPLAY "          Total saldo aportación sin dispersión              : ", v_total_saldo_apo   USING "###,###,###,###,##&.##"
  DISPLAY "          Total saldo amortización sin dispersión            : ", v_total_saldo_amo   USING "###,###,###,###,##&.##"
  DISPLAY "          Total saldo aportación y amortización sin dispersón: ", v_total_sal_apo_amo USING "###,###,###,###,##&.##"

END FUNCTION

#Objetivo: Estructura reporte de Factura Aportaciones Subsecuentes
REPORT rep_con_disp(r_nom_archivo,
                    r_folio,
                    r_usuario,
                    r_total_mov,
                    r_total_nss,
                    r_total_saldo_apo,
                    r_total_saldo_amo,
                    r_total_sal_apo_amo)

  DEFINE
   r_nom_archivo            VARCHAR(60),  --Nombre del archivo de salida   
   r_folio                  DECIMAL(9,0),
   r_usuario                CHAR(20),
   r_total_mov              DECIMAL(9,0),
   r_total_nss              DECIMAL(9,0),
   r_total_saldo_apo        DECIMAL(22,2),
   r_total_saldo_amo        DECIMAL(22,2),
   r_total_sal_apo_amo      DECIMAL(22,2)

  DEFINE v_fecha_reporte   DATE -- Fecha de proceso

  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta
      LET v_fecha_reporte = TODAY
      PRINTX r_usuario
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX r_folio USING "--------&"
      PRINTX r_nom_archivo
      --PRINTX f_nss
      --PRINTX f_nom_completo

     PAGE HEADER
      PRINTX r_usuario
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"

    ON EVERY ROW
      PRINTX r_total_mov         USING "-,---,--&"
      PRINTX r_total_nss         USING "-,---,--&"
      PRINTX r_total_saldo_apo   USING "###,###,###,###,##&.&&"
      PRINTX r_total_saldo_amo   USING "###,###,###,###,##&.&&"
      PRINTX r_total_sal_apo_amo USING "###,###,###,###,##&.&&"

    --ON LAST ROW
    --   PRINTX p_usuario
    --   PRINTX v_fecha_reporte USING "dd-mm-yyyy"

END REPORT

#Objetivo: Consulta para validar que exista el NSS
FUNCTION fn_existe_nss(v_nss)
  DEFINE
    v_nss                    CHAR(11),
    v_QryTxt                 STRING,
    v_sql_txt                STRING,
    v_nombre_af              CHAR(50),
    v_ap_paterno_af          CHAR(50),
    v_ap_materno_af          CHAR(50)

  -- Busca el nombre del derechohabiente
  LET v_sql_txt= "\n SELECT id_derechohabiente, ",
                  "\n        nombre_af, ", 
                  "\n        ap_paterno_af, ",
                  "\n        ap_materno_af ",  
                  "\n FROM   afi_derechohabiente ",
                  "\n WHERE  nss = ?"

  PREPARE prp_nom_der FROM v_sql_txt 
  EXECUTE prp_nom_der USING v_nss
                       INTO f_id_derechohabiente,
                            v_nombre_af,
                            v_ap_paterno_af,
                            v_ap_materno_af

  LET f_nom_completo = v_nombre_af CLIPPED," ",v_ap_paterno_af CLIPPED," ",v_ap_materno_af CLIPPED

END FUNCTION