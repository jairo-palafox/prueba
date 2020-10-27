###############################################################################
#VERSION                    => 1.0.0                                           #
#FECHA ULTIMA MODIFICACION  => 18/06/2015                                      #
################################################################################
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
#------------------------------------------------------------------------------#
#MODULO           => DIS                                                       #
#PROGRAMA         => DISE20                                                    #
#OBJETIVO         => EJECUTA LA FUNCIÓN DE LA CONFIRMACIÓN                     #
#FECHA DE INICIO  => 18/06/2015                                                #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_usuario                      VARCHAR(30),                  --Almacena al usuario
    g_proceso_cod                  LIKE cat_proceso.proceso_cod, --codigo de proceso
    g_opera_cod                    LIKE cat_operacion.opera_cod, --codigo de operacion
    g_arch_proceso                 VARCHAR(100),
    g_qrytxt                       STRING,                       --Prepara consultas
    g_sql_txt                      STRING                        --Consultas

  DEFINE 
    g_folio_regpag                 DECIMAL(9,0),
    g_folio_trans                  DECIMAL(9,0),
    g_f_actualiza                  DATE,         
    p_pid                          DECIMAL(9,0),
    g_proceso_cod_reg_pago         SMALLINT,
    g_folio_cons                   DECIMAL(9,0)

  DEFINE 
    p_b_despliegue_pantalla        SMALLINT,
    v_ruta_rep                     STRING,
    manejador_rpt                  om.SaxDocumentHandler,
    v_origen_datos                 STRING,
    v_ruta_listados                STRING,
    v_ruta_ejecutable              STRING

  --Totales
  DEFINE
    v_tot_registros                DECIMAL(9,0), --Total de registros
    v_tot_aivs                     DECIMAL(18,6),--Total de AIVS
    v_tot_aportacion               DECIMAL(12,2) --Total de aportaciones

   DEFINE g_f_ini_trans            DATE, 
          g_f_fin_trans            DATE

END GLOBALS

MAIN

  LET g_folio_trans     = 0
  LET g_f_ini_trans     = ""
  LET g_f_fin_trans     = ""
  LET g_f_actualiza     = ""
  
  LET g_folio_trans     = ARG_VAL(1) --Folio transaccion
  LET g_f_ini_trans     = ARG_VAL(2) --Fecha inicial transaccion
  LET g_f_fin_trans     = ARG_VAL(3) --Fecha final transaccion
  LET g_f_actualiza     = ARG_VAL(4) --Fecha de actualización
  LET g_usuario         = ARG_VAL(5) --Usuario que ejecuta el proceso

  CALL STARTLOG(g_usuario CLIPPED||".DISE20.log")

  DISPLAY "Inicia Validaciones para confirmar: ",TIME
  DISPLAY " "
  CALL fn_confirmar_apo_sub()

END MAIN

FUNCTION fn_confirmar_apo_sub()
    DEFINE 
    bnd_continuar                  SMALLINT,
    v_fecha_01mm                   CHAR(11),
    p_transaccion                  SMALLINT,
    r_bandera                      SMALLINT,
    r_bnd_opera_err                SMALLINT,
    v_bnd_confirmacion             SMALLINT,  
    r_ruta_reporte                 STRING, 
    v_QryTxt                       STRING,
    p_programa_cod                 VARCHAR(10),
    v_status_err                   SMALLINT,
    v_desc_err                     VARCHAR(200),
    r_id_derechohabinete           DECIMAL(9,0)

   DEFINE v_bnd_existe_info        SMALLINT
   
  LET g_proceso_cod = 3904 --Codigo del Proceso Confirma (4)(3904)       
  LET g_opera_cod   = 1    --Codigo de la operacion de confirma (1)

  CALL fn_max_pid(g_proceso_cod,g_opera_cod) RETURNING p_pid
   
  LET p_transaccion  = 0

  --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
  {SELECT  ind_tipo_ejecucion 
  INTO    p_transaccion
  FROM    bat_ctr_operacion 
  WHERE   proceso_cod = g_proceso_cod   
  AND     pid         = p_pid
  AND     opera_cod   = g_opera_cod
   
  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod,g_usuario)
     RETURNING g_folio_trans
  END IF} 

  LET bnd_continuar = 0
   
  --LET p_pid = r_bandera
  LET v_fecha_01mm =  MONTH(TODAY) USING "&&"||"/"||"01"||"/"||YEAR(TODAY)

  IF g_usuario IS NULL THEN 
     LET g_usuario = "infonavit"
  END IF 

  WHENEVER ERROR CONTINUE
    PREPARE prep_sp_dis_apo_subs5
    FROM "EXECUTE PROCEDURE sp_dis_apo_subs5(?)"

    EXECUTE prep_sp_dis_apo_subs5 INTO v_bnd_confirmacion, v_status_err, v_desc_err, r_id_derechohabinete
                                 USING g_folio_trans
  WHENEVER ERROR STOP 

  IF v_bnd_confirmacion = 0 THEN      
     WHENEVER ERROR CONTINUE
     
     --Actualiza el status del folio de confirmación
     LET v_QryTxt = " UPDATE glo_folio",
                    " SET    status      = 1",
                    " WHERE  folio       = ",g_folio_trans,
                    " AND    proceso_cod = ",g_proceso_cod,
                    " AND    opera_cod   = 1"
                     
     PREPARE prep_actualiza_folio_confirmado FROM v_QryTxt
     EXECUTE prep_actualiza_folio_confirmado
      
     LET bnd_continuar = 1
  ELSE
     DISPLAY "Error en la confirmación: ",v_bnd_confirmacion
     DISPLAY " "
     CALL fn_error_opera(p_pid, g_proceso_cod, g_opera_cod)
     RETURNING v_bnd_confirmacion
     EXIT PROGRAM
  END IF

  --DISPLAY "bnd_continuar: ",bnd_continuar
  IF bnd_continuar THEN
     CALL fn_actualiza_opera_fin(p_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bandera
      
     --DISPLAY "r_bandera: ", r_bandera
     IF r_bandera = 0 THEN 

         CALL fn_genera_reporte_conf(g_folio_trans, g_f_ini_trans, g_f_fin_trans)
         RETURNING r_ruta_reporte

         LET v_bnd_existe_info = 0
         CALL fn_verifica_info_arch(g_folio_trans, g_f_ini_trans, g_f_fin_trans) 
         RETURNING v_bnd_existe_info

         IF v_bnd_existe_info = 1 THEN
            CALL fn_genera_archivo(g_folio_trans, g_f_ini_trans, g_f_fin_trans)
         END IF
  
        -- Envío de correo de notificación de proceso finalizado
        CALL fn_correo_proceso(p_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               r_ruta_reporte CLIPPED,
                               "Confirmación",
                               "\n ID Proceso   : "||p_pid||
                               "\n Proceso      : Confirmación"||
                               "\n Operación    : Confirmación"||
                               "\n Fecha Inicio : "||TODAY||
                               "\n Fecha Fin    : "||TODAY)
     ELSE
        CALL fn_error_opera(p_pid, g_proceso_cod, g_opera_cod)
        RETURNING v_bnd_confirmacion
     END IF   
  END IF
END FUNCTION

FUNCTION fn_genera_reporte_conf(p_folio_trans, p_f_ini_trans, p_fin_trans)
  DEFINE p_folio_trans              DECIMAL(9,0)
  DEFINE p_f_ini_trans              DATE, 
         p_fin_trans                DATE
         
  DEFINE v_tot_nss                 DECIMAL(9,0)
  DEFINE v_tot_conf_no_acr         DECIMAL(9,0)
  DEFINE v_fec_proc                DATE
  DEFINE v_origen_datos            STRING
  DEFINE v_ruta_reporte            STRING
  DEFINE v_ruta_listados           STRING
  DEFINE v_ruta_ejecutable         STRING
  DEFINE manejador_rpt             om.SaxDocumentHandler

  DEFINE arr_confirma              DYNAMIC ARRAY OF RECORD
    v_estado                       SMALLINT,
    v_desc_estado                  CHAR(50),
    v_total_edo                    DECIMAl(9,0)
  END RECORD

  DEFINE v_indice_1                INTEGER
  DEFINE v_ind_rpt                 INTEGER
  DEFINE v_cantidad                STRING

  DEFINE v_ruta_envio_ocg          LIKE seg_modulo.ruta_envio
  DEFINE v_modulo_cod              LIKE seg_modulo.modulo_cod
  DEFINE v_nom_archivo             VARCHAR(40)
  DEFINE v_ddmmaaaa                VARCHAR(08)
  DEFINE v_busca_nom_archivo       STRING

  LET v_fec_proc = TODAY

  LET v_tot_nss         = 0
  LET v_tot_conf_no_acr = 0

  --Obtiene el total de NSS
  SELECT COUNT(*)
  INTO   v_tot_nss
  FROM   dis_ctr_aps_tns
  WHERE  folio_transaccion = g_folio_trans

  --Obtiene el total de NSS confirmados no acreditados
  SELECT COUNT(*)
  INTO   v_tot_conf_no_acr
  FROM   dis_ctr_aps_tns
  WHERE  folio_transaccion = g_folio_trans
  AND    estado            = 21
  
  --Obtiene nombre del archivo de salida
  LET v_modulo_cod = 'ocg'

  --Se obtiene la ruta envío del módulo
  SELECT a.ruta_envio
  INTO   v_ruta_envio_ocg
  FROM   seg_modulo a
  WHERE  a.modulo_cod = v_modulo_cod

  --Se crea el nombre dl archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "ocg_fa_conf_"         --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy" --fecha del archivo sin separadores
  LET v_busca_nom_archivo = "ocg_fa_conf_" || v_ddmmaaaa --Concatena nombre a buscar

  CALL fn_obtiene_nombre_archivo(v_ruta_envio_ocg, v_busca_nom_archivo)
  RETURNING v_nom_archivo

  --Despliega información en el log
  DISPLAY "\n ####### CONFIRMA ########"

  DISPLAY " Folio Transacción : ", g_folio_trans
  LET v_cantidad = v_tot_nss
  DISPLAY " Total de registros: ", v_cantidad CLIPPED
  DISPLAY " "
  
  LET v_cantidad = v_tot_conf_no_acr
  DISPLAY " Total de registros no acreditados: ", v_cantidad CLIPPED
  DISPLAY " "
  
  LET g_qrytxt = " SELECT a.estado, b.desc_edo_aps, COUNT(*) ",
                 " FROM   dis_ctr_aps_tns a, ",
                 "        OUTER cat_edo_aps b ",
                 " WHERE  a.folio_transaccion = ", g_folio_trans,
                 " AND    a.estado            = b.cod_edo_aps ",
                 " GROUP BY 1,2 ",
                 " ORDER BY 1,2 "

  PREPARE prp_consulta_cifras FROM g_qrytxt

  LET v_origen_datos = g_usuario

  --Se construte la ruta del archivo
  CALL fn_rutas("ocg")
  RETURNING v_ruta_ejecutable, v_ruta_listados

  LET v_ruta_reporte = v_ruta_listados.trim(), "/",
                       v_origen_datos.trim(), "-",
                       "DISE20", "-",
                       p_pid USING "&&&&&", "-",
                       g_proceso_cod USING "&&&&&", "-",
                       g_opera_cod USING "&&&&&", ".pdf"

  DISPLAY " Ruta del reporte: ", v_ruta_reporte
  DISPLAY " "

  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE201.4rp") THEN

     CALL fgl_report_selectDevice ("PDF")
     CALL fgl_report_selectPreview(0)
     CALL fgl_report_setOutputFileName(v_ruta_reporte)

     LET manejador_rpt = fgl_report_commitCurrentSettings()

     ### Inicia Reporte ###
     --Inicializamos variables para suma de totales
     LET v_indice_1    = 1

     --Inicia el reporte de registros con rechazo
     START REPORT rp_cur_conc_aps TO XML HANDLER manejador_rpt

       DECLARE cur_conc_aps CURSOR FOR prp_consulta_cifras
       FOREACH cur_conc_aps INTO arr_confirma[v_indice_1].*
         LET v_indice_1 = v_indice_1  + 1
       END FOREACH

       CALL arr_confirma.deleteElement(v_indice_1)

       LET v_indice_1 = v_indice_1 - 1

       IF v_indice_1 = 0 THEN
          DISPLAY "No se puede generar el reporte por falta de información."
          DISPLAY " "
       END IF

       IF g_usuario IS NULL THEN
          LET g_usuario = "infonavit"
       END IF
       
       FOR v_ind_rpt = 1 TO arr_confirma.getLength()
           OUTPUT TO REPORT rp_cur_conc_aps(g_folio_trans,
                                            p_f_ini_trans, 
                                            p_fin_trans,
                                            g_usuario,
                                            v_fec_proc,
                                            arr_confirma[v_ind_rpt].*,
                                            v_tot_nss,
                                            v_nom_archivo)
       END FOR
     FINISH REPORT rp_cur_conc_aps
  ELSE
     DISPLAY "no funciono"
     DISPLAY " "
     EXIT PROGRAM      
  END IF
    
  RETURN v_ruta_reporte 
  
END FUNCTION

#OBJETIVO: Generar el reporte de confirma
REPORT rp_cur_conc_aps(v_rfolio,
                       p_f_ini_trans, 
                       p_f_fin_trans,
                       v_rusurio,
                       v_rfec_proc,
                       reg_confirma,
                       v_tot_nss,
                       v_nom_archivo)

  DEFINE
    v_rfolio            DECIMAL(9,0),
    p_f_ini_trans       DATE, 
    p_f_fin_trans         DATE,
    v_rusurio           VARCHAR(30),   --Usuario de proceso
    v_rfec_proc         DATE,          --Fecha de procesos
    v_tot_nss           DECIMAL(9,0),
    v_nom_archivo       CHAR(40)

  DEFINE reg_confirma     RECORD
     v_estado          SMALLINT,
     v_desc_estado     CHAR(50),
     v_total_estado    DECIMAL(9,0)
  END RECORD

  FORMAT
    FIRST PAGE HEADER
      PRINTX v_rusurio
      PRINTX v_rfec_proc     USING "dd-mm-yyyy"

      --IF v_rfolio IS NOT NULL THEN
         PRINTX v_rfolio
      --END IF

      --IF p_f_ini_trans IS NOT NULL AND p_f_fin_trans IS NOT NULL THEN
         PRINTX p_f_ini_trans USING "dd-mm-yyyy"
         PRINTX p_f_fin_trans USING "dd-mm-yyyy"
      --END IF
      
      PRINTX v_nom_archivo

    ON EVERY ROW
       PRINTX reg_confirma.v_estado
       PRINTX reg_confirma.v_desc_estado
       PRINTX reg_confirma.v_total_estado

    ON LAST ROW
       PRINTX v_tot_nss

END REPORT

FUNCTION fn_obtiene_nombre_archivo(p_ruta_envio_ocg, p_busca_nom_archivo)
  DEFINE
    p_ruta_envio_ocg               LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo            VARCHAR(40), --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                          STRING,      --Cadena de comando a ejecutar
    v_consecutivo                  INTEGER,     --Consecutivo del archivo por día
    v_nom_archivo_sal              VARCHAR(40)  --Nombre del archivo de salida

  DEFINE fn                        CHAR(28)     --Almacena el nombre completo del nombre del archivo en el servidor con su extensión
  DEFINE ch                        base.Channel --Canal de lectura

  DEFINE
    v_reg_dia                      CHAR(03),   --Parametro consecutivo de registro por dia
    v_modulo_cod                   LIKE seg_modulo.modulo_cod,
    v_ddmmaaaa                     VARCHAR(08) --fecha del archivo de salida

  --Obtiene nombre del archivo de salida
  LET v_modulo_cod = "ocg"

  LET v_cmd = "ls -lrt ",p_ruta_envio_ocg CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"
  LET ch    = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo     = fn[21,23] --Posición del consecutivo dentro de la cadena
    LET v_nom_archivo_sal = fn
  END WHILE

  CALL ch.close()

  LET v_consecutivo = v_consecutivo + 1 --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  LET v_nom_archivo_sal = "ocg_fa_conf_"            --nombre de archivo
  LET v_ddmmaaaa        = TODAY USING "ddmmyyyy"    --Fecha del archivo sin separadores
  LET v_reg_dia         = v_consecutivo USING "&&&" --Consecutivo del día de numerico a char
  LET v_nom_archivo_sal = v_nom_archivo_sal CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod

  RETURN v_nom_archivo_sal --Regresa el nombre del archivo del día

END FUNCTION

FUNCTION fn_verifica_info_arch(p_folio_trans, p_f_ini_trans, p_f_fin_trans)
  DEFINE p_folio_trans                 DECIMAL(9,0), --Folio de transacción
         p_f_ini_trans                 DATE, 
         p_f_fin_trans                 DATE

  DEFINE v_bnd_existe_info       SMALLINT

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_afi_dise20;
  WHENEVER ERROR STOP

  PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT H.id_dis_interface_ef, ",
                  "\n        A.id_derechohabiente,  ",
                  "\n        A.nss,                 ",
                  "\n        A.nombre_af,           ",
                  "\n        A.ap_paterno_af,       ",
                  "\n        A.ap_materno_af        ",
                  "\n FROM   dis_ctr_aps_tns H,     ",
                  "\n        afi_derechohabiente A  ",
                  "\n WHERE  H.id_derechohabiente = A.id_derechohabiente ",
                  "\n AND    H.estado            IN (21,22,23) "
                  
  IF p_folio_trans IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt, "\n AND H.folio_transaccion = ", p_folio_trans   
  END IF 

  IF p_f_ini_trans IS NOT NULL AND p_f_fin_trans IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND H.f_liquida  >= '", p_f_ini_trans, "'",
                                "\n AND H.f_liquida  <= '", p_f_fin_trans, "'" 
  END IF

  LET g_sql_txt = g_sql_txt, "\n INTO TEMP tmp_afi_dise20 "

  PREPARE prep_tmp_afi FROM g_sql_txt
  EXECUTE prep_tmp_afi

  UPDATE STATISTICS FOR TABLE tmp_afi_dise20
  
  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT COUNT(*)                 ",
                  --"\n FROM   afi_derechohabiente ad,  ",
                  --"\n        dis_ctr_aps_tns dca,     ",
                  --"\n        glo_folio gf             ",
                  --"\n WHERE  dca.id_derechohabiente = ad.id_derechohabiente ",
                  "\n FROM   dis_ctr_aps_tns dca,  ",
                  "\n        tmp_afi_dise20 afi,   ",
                  "\n        glo_folio gf          ",
                  "\n WHERE  dca.id_dis_interface_ef = afi.id_dis_interface_ef ",                 
                  "\n AND    dca.id_derechohabiente  = afi.id_derechohabiente  ",
                  "\n AND    gf.folio                = dca.folio_transaccion  ",
                  "\n AND    dca.estado             IN (21,22,23) "

   IF p_folio_trans IS NOT NULL THEN          
      LET g_sql_txt = g_sql_txt,"\n AND dca.folio_transaccion = ",p_folio_trans   
   END IF 

   IF p_f_ini_trans IS NOT NULL AND p_f_fin_trans IS NOT NULL THEN 
      LET g_sql_txt = g_sql_txt,"\n AND dca.f_liquida  >= '",p_f_ini_trans,"'",
                               "\n AND dca.f_liquida  <= '",p_f_fin_trans,"'" 
   END IF

   --LET g_sql_txt = g_sql_txt,"\n ORDER BY ad.nss "  
  
   PREPARE ps_verifica_info_arch FROM g_sql_txt
   EXECUTE ps_verifica_info_arch INTO v_bnd_existe_info

   IF v_bnd_existe_info >= 1 THEN
      LET v_bnd_existe_info = 1
   ELSE
      LET v_bnd_existe_info = 0
   END IF

RETURN v_bnd_existe_info
END FUNCTION

FUNCTION fn_genera_archivo(p_folio_trans, p_f_ini_trans, p_f_fin_trans)
  DEFINE p_folio_trans                 DECIMAL(9,0), --Folio de transacción
         p_f_ini_trans                 DATE, 
         p_f_fin_trans                 DATE

  DEFINE
    v_nom_archivo                  VARCHAR(40), --nombre del archivo de salida
    v_ruta_envio_dis               CHAR(40),
    v_ruta_nomarch                 VARCHAR(100), --ruta y nombre del archivo de salida
    v_ch_arch_salida               BASE.CHANNEL,
    v_comando_dos                  STRING,
    v_encabezado                   STRING,
    v_titulos                      STRING,
    v_detalle                      STRING,
    v_sumario                      STRING
    
  DEFINE
    v_fecha_archivo                DATE,
    v_hora_archivo                 DATETIME HOUR TO HOUR ,
    v_min_archivo                  DATETIME MINUTE TO MINUTE,
    v_sec_archivo                  DATETIME SECOND TO SECOND,
    v_hora                         STRING,
    v_indice                       SMALLINT

  DEFINE arr_info_apo              DYNAMIC ARRAY OF RECORD
    v_folio_transaccion            DECIMAL(9,0),
    v_f_transaccion                DATE,
    v_nss                          CHAR(11),
    v_nombre_af                    CHAR(30),
    v_ap_paterno_af                CHAR(30),
    v_ap_materno_af                CHAR(30),
    v_periodo                      CHAR(6),
    v_f_pago                       DATE,
    v_folio_sua                    DECIMAL(6,0),
    v_nrp                          CHAR(11),
    v_aportacion                   DECIMAL(8,2),
    v_aivs                         DECIMAL(18,6),
    v_tpo_credito                  CHAR(20),
    v_estado                       SMALLINT,
    v_f_entrega                    DATE,
    v_interface                    CHAR(2)
  END RECORD

  DEFINE v_aportacion              CHAR(10)
  DEFINE v_desc_credito            CHAR(20)
  DEFINE v_desc_estado             CHAR(40)

  LET v_fecha_archivo = TODAY
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND

  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".ocg"
  --LET v_nom_archivo   = "/sp004_as_tpo_cred_", v_hora
  LET v_nom_archivo   = "/fact_conf_acr_rech_", v_hora
  
  --se obtienen la ruta envio del módulo
  SELECT ruta_envio
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED

  --se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT dca.folio_transaccion, ",
                  "\n        dca.f_transaccion, ",
                  "\n        ad.nss, ",
                  "\n        ad.nombre_af, ",
                  "\n        ad.ap_paterno_af, ",
                  "\n        ad.ap_materno_af, ",
                  "\n        dca.periodo_pago, ",
                  "\n        dca.f_pago, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.nrp, ",
                  "\n        dca.imp_ap_pat, ",
                  "\n        dca.aiv_ap_pat, ",
                  "\n        dca.estado, ",
                  "\n        dca.tpo_credito, ",
                  "\n        TODAY as f_entrega, ",
                  "\n        'AS' as interface ",
                  --"\n FROM   afi_derechohabiente ad, ",
                  --"\n        dis_ctr_aps_tns dca, ",
                  "\n FROM   dis_ctr_aps_tns dca, ",
                  "\n        tmp_afi_dise20 ad, ",
                  "\n        glo_folio gf ",
                  "\n WHERE  dca.id_dis_interface_ef = ad.id_dis_interface_ef ",
                  "\n AND    dca.id_derechohabiente  = ad.id_derechohabiente ",
                  "\n AND    gf.folio                = dca.folio_transaccion ",
                  "\n AND    dca.estado             IN (21,22,23) "

   IF p_folio_trans IS NOT NULL THEN          
      LET g_sql_txt = g_sql_txt,"\n AND dca.folio_transaccion = ",p_folio_trans

      --Imprime encabezado del archivo
      LET v_encabezado = "FOLIO TRANSACCION: ",p_folio_trans
      CALL v_ch_arch_salida.write([v_encabezado])     
   END IF 

   IF p_f_ini_trans IS NOT NULL AND p_f_fin_trans IS NOT NULL THEN 
      LET g_sql_txt = g_sql_txt,"\n AND dca.f_liquida  >= '",p_f_ini_trans,"'",
                               "\n AND dca.f_liquida  <= '",p_f_fin_trans,"'" 

      LET v_encabezado = "PERIODO FECHAS: ",p_f_ini_trans USING "dd-mm-yyyy", " - ", p_f_fin_trans USING "dd-mm-yyyy"
      CALL v_ch_arch_salida.write([v_encabezado])
   END IF

  LET g_sql_txt = g_sql_txt,"\n ORDER BY ad.nss "  
  
  PREPARE pr_sl_inf_arc FROM g_sql_txt
  DECLARE cur_sl_inf_arc CURSOR FOR pr_sl_inf_arc

  LET v_indice         = 1
  LET v_tot_aivs       = 0
  LET v_tot_aportacion = 0

  --Imprime encabezado del archivo
  LET v_titulos = "FOLIO TRANSACCIÓN |FECHA TRANSACCIÓN |NSS |NOMBRE |PERIODO PAGO |FECHA PAGO |FOLIO SUA |NRP |AIVS |APORTACIÓN |CONCEPTO PAGO |STATUS |FECHA ARCHIVO |INTERFACE "
  CALL v_ch_arch_salida.write([v_titulos])

  FOREACH cur_sl_inf_arc INTO arr_info_apo[v_indice].v_folio_transaccion,
                              arr_info_apo[v_indice].v_f_transaccion,
                              arr_info_apo[v_indice].v_nss,
                              arr_info_apo[v_indice].v_nombre_af,
                              arr_info_apo[v_indice].v_ap_paterno_af,
                              arr_info_apo[v_indice].v_ap_materno_af,
                              arr_info_apo[v_indice].v_periodo,
                              arr_info_apo[v_indice].v_f_pago,
                              arr_info_apo[v_indice].v_folio_sua,
                              arr_info_apo[v_indice].v_nrp,
                              arr_info_apo[v_indice].v_aportacion,
                              arr_info_apo[v_indice].v_aivs,
                              arr_info_apo[v_indice].v_estado,
                              arr_info_apo[v_indice].v_tpo_credito,
                              arr_info_apo[v_indice].v_f_entrega,
                              arr_info_apo[v_indice].v_interface

    SELECT a.desc_credito_ocg
    INTO   v_desc_credito
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = arr_info_apo[v_indice].v_tpo_credito
    AND    a.ind_activo      = 1

    IF arr_info_apo[v_indice].v_tpo_credito = 3 THEN
       LET arr_info_apo[v_indice].v_interface = 'UG'
    END IF

    SELECT ed.desc_edo_aps
    INTO   v_desc_estado
    FROM   cat_edo_aps ed
    WHERE  ed.cod_edo_aps = arr_info_apo[v_indice].v_estado

    LET v_detalle =  arr_info_apo[v_indice].v_folio_transaccion, "|",
                     arr_info_apo[v_indice].v_f_transaccion USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_nss, "|",
                     arr_info_apo[v_indice].v_nombre_af CLIPPED," ", arr_info_apo[v_indice].v_ap_paterno_af CLIPPED," ", arr_info_apo[v_indice].v_ap_materno_af CLIPPED, "|",
                     arr_info_apo[v_indice].v_periodo CLIPPED, "|",
                     arr_info_apo[v_indice].v_f_pago USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_folio_sua, "|",
                     arr_info_apo[v_indice].v_nrp , "|",
                     arr_info_apo[v_indice].v_aivs USING "#,###,##&.######", "|",
                     arr_info_apo[v_indice].v_aportacion  USING "#,###,##&.##", "|",
                     arr_info_apo[v_indice].v_tpo_credito CLIPPED, "-", v_desc_credito CLIPPED, "|",
                     arr_info_apo[v_indice].v_estado CLIPPED, "-", v_desc_estado CLIPPED, "|",
                     arr_info_apo[v_indice].v_f_entrega USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_interface, "|"

    CALL v_ch_arch_salida.write([v_detalle])

    LET v_tot_aivs       = v_tot_aivs       + arr_info_apo[v_indice].v_aivs
    LET v_tot_aportacion = v_tot_aportacion + arr_info_apo[v_indice].v_aportacion
    LET v_indice         = v_indice + 1
  END FOREACH

  FREE cur_sl_inf_arc

  CALL arr_info_apo.deleteElement(v_indice)
  LET v_indice    = v_indice - 1

  LET v_tot_registros =  v_indice

  LET v_sumario = "TOTALES: | ",v_tot_registros," | | | | | | |",
                                v_tot_aivs USING "###,###,##&.######"," | ",
                                v_tot_aportacion USING "###,###,##&.##", " | | | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  --CALL fn_mensaje("Información","Se ha generado el archivo de Acreditados No Existentes de Aportaciones Subsecuentes \n en la ruta "||v_ruta_nomarch,"information")

  DISPLAY "Se ha generado el archivo de Acreditados No Existentes \n en la ruta "||v_ruta_nomarch
  DISPLAY " "
  
END FUNCTION