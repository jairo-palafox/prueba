########################################################################### 
#Modulo            => OCG                                                 #
#Programa          => OCGS06                                              #
#Objetivo          => Programa que genera archivo recurrente (desmarcas)  # 
#                     43Bis                                               # 
#Autor             => Emilio Abarca Sánchez                               #
#Fecha inicio      => 01 NOVIEMBRE 2016                                   #
###########################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE g_usuario                CHAR(20)
   DEFINE g_proceso_cod            INTEGER 
   DEFINE g_opera_cod              INTEGER
   DEFINE v_pid                    DECIMAL(9,0) 
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio

   --Arreglo desmarca acreditados 43Bis
   DEFINE arr_desma_acreditados43 DYNAMIC ARRAY OF RECORD
      tpo_registro      CHAR(2),
      nss               CHAR(11),
      num_credito       CHAR(10),
      ssv_92_97_deudor  CHAR(8),
      f_otorgamiento    DATE,
      f_culminacion     DATE,
      tpo_credito       CHAR(3),
      status_credito    CHAR(3),
      tpo_dscto         CHAR(1),
      val_dscto         CHAR(8),
      nrp               CHAR(11),
      f_ini_dscto       CHAR(8),
      nss_liberado      CHAR(11),
      f_proceso         DATE,
      sdo_credito       CHAR(8),
      f_prox_liq        CHAR(8),
      f_desde_avis_des  CHAR(8),
      f_hasta_avis_des  CHAR(8),
      tpo_rechazo       CHAR(2)
   END RECORD 

    DEFINE arr_sum_desm DYNAMIC ARRAY OF RECORD
      tpo_registro      CHAR(2),
      tot_registros     CHAR(7),
      tot_reg01         CHAR(7),
      tot_registros_det CHAR(7),
      filer             CHAR(111)
   END RECORD

   DEFINE v_fecha_desde    DATE
   DEFINE v_fecha_hasta    DATE
   DEFINE v_fecha_actual   DATE 
   DEFINE r_b_valida       SMALLINT 
   DEFINE v_estado         SMALLINT

END GLOBALS 

MAIN
   LET g_usuario       = ARG_VAL (1)
   LET v_pid           = ARG_VAL (2)
   LET g_proceso_cod   = 3920
   LET g_opera_cod     = 1

   LET v_fecha_desde  = NULL    
   LET v_fecha_hasta  = TODAY -1 
   LET v_fecha_Actual = TODAY 
   
   --Se obtiene la ruta donde se dejará el archivo generado
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   --Recupera la ultima fecha de ejecución del archivo
   SELECT FIRST 1 f_proceso
      INTO v_fecha_desde
      FROM ocg_arh_rec_desmarcas
      ORDER BY f_proceso DESC 

   -- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".OCGS06.log")

   -- Genera el PID
   --CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid
            
   -- Inicializa proceso
   --CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,"","OCGS05","",g_usuario) RETURNING r_b_valida

    CALL fn_display_proceso(0,"INICIA GENERACIÓN ARCHIVO DE DESMARCAS")

    DISPLAY ""
    DISPLAY " RANGO DE FECHAS"
    DISPLAY " FECHA INICIAL  : ",v_fecha_desde    USING "dd/mm/yyyy"
    DISPLAY " FECHA FINAL    : ",v_fecha_hasta    USING "dd/mm/yyyy"
    DISPLAY " FECHA EJECUCIÓN: ",v_fecha_actual   USING "dd/mm/yyyy"
    DISPLAY ""

    -- Actualiza Operacion
    --CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,g_opera_cod,"","OCGS05","",g_usuario) RETURNING r_b_valida

    IF(v_fecha_desde IS NULL) OR (v_fecha_desde = TODAY) THEN
       -- Marca el proceso como erróneo
       DISPLAY " Ha ocurrido un error al generar el archivo: "
       DISPLAY " => La Fecha Inicial no puede ser nula o igual a la fecha actual"
       DISPLAY ""

      CALL fn_error_opera(v_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado
   ELSE 
      --Llama la función principal
      CALL fn_gen_rec_desmarca() RETURNING v_estado

      IF(v_estado = 0) THEN

         DISPLAY " PROCESO EJECUTADO CORRECTAMENTE"
         DISPLAY ""
         CALL fn_display_proceso(1,"FIN GENERACIÓN ARCHIVO DE DESMARCAS")

         CALL fn_actualiza_opera_fin(v_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado
      END IF
      
   END IF 
   
END MAIN 

FUNCTION fn_gen_rec_desmarca()

   DEFINE v_qry            STRING
   DEFINE cont             INTEGER
   DEFINE v_detalle        STRING   
   DEFINE ch               base.Channel
   DEFINE ch2              base.Channel
   DEFINE v_archivo_salida STRING
   DEFINE v_tot_reg        STRING
   DEFINE v_wc_comando     STRING   
   DEFINE t                CHAR(40)
   DEFINE v_rm_comando     STRING   
   DEFINE v_cp_comando     STRING
   DEFINE v_env_comando    STRING
   DEFINE v_ind_envio      BOOLEAN
   DEFINE v_conteo         INTEGER 
   DEFINE v_filer          CHAR(118)
   DEFINE i                INTEGER 
   DEFINE v_sumario        STRING
   DEFINE v_ind_cp         BOOLEAN 

   LET v_archivo_salida = v_ruta_envio CLIPPED,"/RECDESMA.TXT"
   LET ch = base.Channel.create() #Se crea el objeto base.channel

   CALL ch.openFile(v_archivo_salida,"w")

   LET v_conteo = 0

   IF (v_fecha_desde IS NULL) THEN
      LET v_qry = "SELECT afi.nss,
                          a.f_liquida_credito
                     FROM afi_derechohabiente afi,
                          ocg_acreditado a,
                          ocg_formalizacion frm
                    WHERE a.id_ocg_formalizacion = frm.id_ocg_formalizacion
                     AND  frm.id_derechohabiente = afi.id_derechohabiente
                     AND  a.situacion >= 140
                     AND  a.f_liquida_credito <= ",'"',v_fecha_hasta,'"'
                     
      SELECT count(*)
        INTO v_conteo
        FROM afi_derechohabiente afi,
             ocg_acreditado a,
             ocg_formalizacion frm
       WHERE a.id_ocg_formalizacion = frm.id_ocg_formalizacion
        AND  frm.id_derechohabiente = afi.id_derechohabiente
        AND  a.situacion >= 140
        AND  a.f_liquida_credito <= v_fecha_hasta
                     
   ELSE 
      --Si encuentra la ultima fecha de generación del archivo
 
      LET v_qry = "SELECT afi.nss,
                          a.f_liquida_credito
                     FROM afi_derechohabiente afi,
                          ocg_acreditado a,
                          ocg_formalizacion frm
                    WHERE a.id_ocg_formalizacion = frm.id_ocg_formalizacion
                     AND  frm.id_derechohabiente = afi.id_derechohabiente
                     AND  a.situacion >= 140
                     AND  a.f_liquida_credito >= ",'"',v_fecha_desde,'"',
                     "AND  a.f_liquida_credito <= ",'"',v_fecha_hasta,'"'

      SELECT count(*)
        INTO v_conteo
        FROM afi_derechohabiente afi,
             ocg_acreditado a,
             ocg_formalizacion frm
       WHERE a.id_ocg_formalizacion = frm.id_ocg_formalizacion
        AND  frm.id_derechohabiente = afi.id_derechohabiente
        AND  a.situacion >= 140
        AND  a.f_liquida_credito >= v_fecha_desde
        AND  a.f_liquida_credito <= v_fecha_hasta
                     
   END IF 
            
   PREPARE cons_desma43 FROM v_qry
   DECLARE cursr CURSOR FOR cons_desma43

   LET cont = 1
               
   CALL arr_desma_acreditados43.clear()   
   CALL arr_sum_desm.clear()
               
   FOREACH cursr INTO arr_desma_acreditados43[cont].nss,            #NSS
                      arr_desma_acreditados43[cont].f_otorgamiento  #f_liquida_credito

      LET arr_desma_acreditados43[cont].tpo_registro     = "11" --tipo de registro para desmarca
      LET arr_desma_acreditados43[cont].num_credito      = " " USING "&&&&&&&&&&"
      LET arr_desma_acreditados43[cont].ssv_92_97_deudor = " " USING "&&&&&&&&"
      LET arr_desma_acreditados43[cont].f_culminacion    = TODAY
      LET arr_desma_acreditados43[cont].tpo_credito      = "002" --apoyo infonavit
      LET arr_desma_acreditados43[cont].status_credito   = "002" --liquidado
      LET arr_desma_acreditados43[cont].tpo_dscto        = " " USING "&"
      LET arr_desma_acreditados43[cont].val_dscto        = " " USING "&&&&&&&&"
      LET arr_desma_acreditados43[cont].nrp              = " " USING "###########"
      LET arr_desma_acreditados43[cont].f_ini_dscto      = " " USING "&&&&&&&&"
      LET arr_desma_acreditados43[cont].nss_liberado     = " " USING "&&&&&&&&&&&"
      LET arr_desma_acreditados43[cont].f_proceso        = TODAY
      LET arr_desma_acreditados43[cont].sdo_credito      = " " USING "&&&&&&&&"
      LET arr_desma_acreditados43[cont].f_prox_liq       = " " USING "&&&&&&&&"
      LET arr_desma_acreditados43[cont].f_desde_avis_des = " " USING "&&&&&&&&"
      LET arr_desma_acreditados43[cont].f_hasta_avis_des = " " USING "&&&&&&&&"
      LET arr_desma_acreditados43[cont].tpo_rechazo      = " " USING "&&"

      #construye cada línea del archivo
      LET v_detalle = arr_desma_acreditados43[cont].tpo_registro,
                      arr_desma_acreditados43[cont].nss,
                      arr_desma_acreditados43[cont].num_credito,
                      arr_desma_acreditados43[cont].ssv_92_97_deudor,
                      arr_desma_acreditados43[cont].f_otorgamiento USING "yyyymmdd",
                      arr_desma_acreditados43[cont].f_culminacion  USING "yyyymmdd",
                      arr_desma_acreditados43[cont].tpo_credito,
                      arr_desma_acreditados43[cont].status_credito,
                      arr_desma_acreditados43[cont].tpo_dscto,
                      arr_desma_acreditados43[cont].val_dscto,
                      arr_desma_acreditados43[cont].nrp,             
                      arr_desma_acreditados43[cont].f_ini_dscto,     
                      arr_desma_acreditados43[cont].nss_liberado,    
                      arr_desma_acreditados43[cont].f_proceso USING "yyyymmdd",       
                      arr_desma_acreditados43[cont].sdo_credito,    
                      arr_desma_acreditados43[cont].f_prox_liq,      
                      arr_desma_acreditados43[cont].f_desde_avis_des,
                      arr_desma_acreditados43[cont].f_hasta_avis_des,
                      arr_desma_acreditados43[cont].tpo_rechazo

      LET cont = cont + 1

      #Se escribe la línea en el archivo de salida
      CALL ch.writeLine(v_detalle)
 
   END FOREACH  

   --se llena el filer con ceros
   LET v_filer = 0

   FOR i = 1 TO 111
      LET v_filer[i] = v_filer
   END FOR 

   LET arr_sum_desm[1].tpo_registro      = "99"
   LET arr_sum_desm[1].tot_registros     = v_conteo + 1
   LET arr_sum_desm[1].tot_reg01         = "1"
   LET arr_sum_desm[1].tot_registros_det = v_conteo
   LET arr_sum_desm[1].filer             = v_filer

   LET v_sumario = arr_sum_desm[1].tpo_registro,
                   arr_sum_desm[1].tot_registros USING "&&&&&&&",
                   arr_sum_desm[1].tot_reg01     USING "&&&&&&&",
                   arr_sum_desm[1].tot_registros_det USING "&&&&&&&",
                   arr_sum_desm[1].filer

   CALL ch.writeLine([v_sumario])
   CALL ch.close()
                               
   #Se obtiene el total de registros del arhivo generado
   LET v_tot_reg = v_ruta_envio CLIPPED,"/tot_desma43.txt"
   LET v_wc_comando = "cat ",v_archivo_salida," | wc -l "," > ", v_tot_reg
   RUN v_wc_comando 
              
   LET ch2 = base.Channel.create()
   CALL ch2.openFile(v_tot_reg,"r")
   LET t = ch2.readLine()
   CALL ch2.close()

   #Guarda registro del archivo generado en la tabla de control
   INSERT INTO ocg_arh_rec_desmarcas
      VALUES(v_fecha_Actual,t, g_usuario);

   #Elimina archivo del total de registros
   LET v_rm_comando = "rm ", v_tot_reg
   RUN v_rm_comando
                
   -- Realiza respaldo del archivo generado "
   LET v_cp_comando = "cp"," ",v_archivo_salida," ",v_ruta_envio CLIPPED,"/RECDESMA_" CLIPPED, TODAY USING "ddmmyyyy" CLIPPED,".TXT"
   RUN v_cp_comando RETURNING v_ind_cp

   IF(v_ind_cp = 0) THEN
      DISPLAY " => El archivo de desmarcas 43Bis se ha generado correctamente"
      DISPLAY " => El respaldo del archivo RECDESMA.TXT se realizó correctamente"
      DISPLAY " => El archivo de desmarcas se ha generado en la ruta /safreviv_int/ocg/envio"
   END IF 
   
   -- Ejecución del Script de envío a TRM
   LET v_env_comando = "sh ","/opt/Interpel/Scripts/SOA16024.sh" #Envío de Producción
   --LET v_env_comando = "sh ","/opt/Interpel/Scripts/SOA16024_QA.sh" #Envío QA 
   RUN v_env_comando RETURNING v_ind_envio

   IF(v_ind_envio = 0) THEN
      DISPLAY " => El archivo de desmarcas 43Bis se ha enviado a TRM exitosamente"
      DISPLAY ""
   ELSE 
      DISPLAY " => No se pudo enviar el archivo de desmarcas 43Bis a TRM"
      DISPLAY ""
   END IF

   RETURN v_ind_cp
   
END FUNCTION 
