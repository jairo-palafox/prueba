########################################################################### 
#Modulo            => OCG                                                 #
#Programa          => OCGS05                                              #
#Objetivo          => Programa que genera archivo de recurrentes (marcas) # 
#                     43Bis.                                              # 
#Autor             => Emilio Abarca Sánchez                               #
#Fecha inicio      => 19 OCTUBRE 2016                                     #
###########################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE g_usuario                CHAR(20)
   DEFINE g_proceso_cod            INTEGER 
   DEFINE g_opera_cod              INTEGER
   DEFINE v_pid                    DECIMAL(9,0) 
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio

   --Arreglo acreditados 43Bis (marcas)
   DEFINE arr_acreditados43 DYNAMIC ARRAY OF RECORD
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
      f_gen_arh         DATE,
      sdo_credito       CHAR(8),
      f_prox_liq        CHAR(8),
      f_desde_avis_des  CHAR(8),
      f_hasta_avis_des  CHAR(8),
      tpo_rch           CHAR(2)
   END RECORD

   DEFINE arr_sumario DYNAMIC ARRAY OF RECORD
      tpo_registro      CHAR(2),
      tot_registros     CHAR(7),
      tot_reg01         CHAR(7),
      tot_registros_det CHAR(7),
      filer             CHAR(111)
   END RECORD 

   DEFINE v_fecha_desde  DATE
   DEFINE v_fecha_hasta  DATE
   DEFINE v_fecha_actual DATE 
   DEFINE v_sumario      STRING
   DEFINE r_b_valida     SMALLINT 
   DEFINE v_estado       SMALLINT
   
END GLOBALS 

MAIN
   LET g_usuario       = ARG_VAL (1)
   LET v_pid           = ARG_VAL (2)
   LET g_proceso_cod   = 3919
   LET g_opera_cod     = 1
   
   --Vars para rango de fechas
   LET v_fecha_desde   = NULL    
   LET v_fecha_hasta   = TODAY -1 
   LET v_fecha_Actual  = TODAY 

   --Se obtiene la ruta donde se dejará el archivo generado
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   --Recupera la ultima fecha de ejecución del archivo
   SELECT FIRST 1 f_proceso
      INTO v_fecha_desde
      FROM ocg_arh_rec_marcas
      ORDER BY f_proceso DESC 

   -- Se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".OCGS05.log")

   -- Genera el PID
   --CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid
    
    
   -- Inicializa proceso
   --CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,"","OCGS05","",g_usuario) RETURNING r_b_valida

   
   CALL fn_display_proceso(0,"INICIA GENERACIÓN ARCHIVO DE MARCAS")

   DISPLAY ""
   DISPLAY " RANGO DE FECHAS"
   DISPLAY " FECHA INICIAL  : ",v_fecha_desde    USING "dd/mm/yyyy"
   DISPLAY " FECHA FINAL    : ",v_fecha_hasta      USING "dd/mm/yyyy"
   DISPLAY " FECHA EJECUCIÓN: ",v_fecha_actual USING "dd/mm/yyyy"
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
      CALL fn_gen_recurrente43() RETURNING v_estado

      IF(v_estado = 0) THEN

         DISPLAY " PROCESO EJECUTADO CORRECTAMENTE"
         DISPLAY ""
         CALL fn_display_proceso(1,"FIN GENERACIÓN ARCHIVO DE MARCAS")

         CALL fn_actualiza_opera_fin(v_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado

      END IF
      
   END IF 

END MAIN 

FUNCTION fn_gen_recurrente43()

   DEFINE v_qry            STRING
   DEFINE cont             INTEGER
   DEFINE v_detalle        STRING   
   DEFINE ch               base.Channel
   DEFINE v_archivo_salida STRING
   DEFINE v_cp_comando     STRING
   DEFINE v_env_comando    STRING
   DEFINE v_ind_cp         BOOLEAN 
   DEFINE v_ind_envio      BOOLEAN
   DEFINE v_contador       INTEGER
   DEFINE i                INTEGER 
   DEFINE v_filer          CHAR(118)

   LET v_archivo_salida = v_ruta_envio CLIPPED,"/RECMARCA.TXT"
   LET ch = base.Channel.create() #Se crea el objeto base.channel

   CALL ch.openFile(v_archivo_salida,"w")

   LET v_contador = 0
               
   IF (v_fecha_desde IS NULL) THEN
      LET v_qry = "SELECT afi.nss,
                          acr.f_formalizacion
                     FROM afi_derechohabiente afi,
                          ocg_formalizacion frm,
                          ocg_acreditado acr
                    WHERE frm.id_derechohabiente = afi.id_derechohabiente
                      AND frm.estado      = 20 
                      AND frm.diagnostico = 1
                      AND frm.situacion IN (55,60,70,80)
                      AND frm.id_ocg_formalizacion = acr.id_ocg_formalizacion
                      AND acr.f_formalizacion <= ",'"',v_fecha_hasta,'"'

      SELECT count(*)
        INTO v_contador
        FROM afi_derechohabiente afi,
             ocg_formalizacion frm,
             ocg_acreditado acr
       WHERE frm.id_derechohabiente = afi.id_derechohabiente
         AND frm.estado      = 20
         AND frm.diagnostico = 1
         AND frm.situacion IN (55,60,70,80)
         AND frm.id_ocg_formalizacion = acr.id_ocg_formalizacion
         AND acr.f_formalizacion <= v_fecha_hasta;
   ELSE 
      --Si encuentra la última fecha de generación del archivo
      LET v_qry = "SELECT afi.nss,
                          acr.f_formalizacion
                     FROM afi_derechohabiente afi,
                          ocg_formalizacion frm,
                          ocg_acreditado acr
                    WHERE frm.id_derechohabiente = afi.id_derechohabiente
                      AND frm.estado      = 20 
                      AND frm.diagnostico = 1
                      AND frm.situacion IN (55,60,70,80)
                      AND frm.id_ocg_formalizacion = acr.id_ocg_formalizacion
                      AND acr.f_formalizacion >= ",'"',v_fecha_desde,'"',
                     "AND acr.f_formalizacion <= ",'"',v_fecha_hasta,'"'
                                  
      SELECT COUNT(*)
         INTO v_contador
         FROM afi_derechohabiente afi,
              ocg_formalizacion frm,
              ocg_acreditado acr
        WHERE frm.id_derechohabiente = afi.id_derechohabiente
          AND frm.estado      = 20
          AND frm.diagnostico = 1
          AND frm.situacion IN (55,60,70,80)
          AND frm.id_ocg_formalizacion = acr.id_ocg_formalizacion
          AND acr.f_formalizacion BETWEEN v_fecha_desde AND v_fecha_hasta
    
   END IF 

   PREPARE cons_rec43 FROM v_qry
   DECLARE cursr CURSOR FOR cons_rec43

   LET cont = 1

   CALL arr_acreditados43.clear()
   CALL arr_sumario.clear()

   FOREACH cursr INTO  arr_acreditados43[cont].nss,
                       arr_acreditados43[cont].f_otorgamiento 

      LET arr_acreditados43[cont].tpo_registro     = "20"
      LET arr_acreditados43[cont].num_credito      = " " USING "&&&&&&&&&&"
      LET arr_acreditados43[cont].ssv_92_97_deudor = " " USING "&&&&&&&&"
      LET arr_acreditados43[cont].f_culminacion    = TODAY
      LET arr_acreditados43[cont].tpo_credito      = "002"
      LET arr_acreditados43[cont].status_credito   = "001"
      LET arr_acreditados43[cont].tpo_dscto        = " " USING "&"
      LET arr_acreditados43[cont].val_dscto        = " " USING "&&&&&&&&"
      LET arr_acreditados43[cont].nrp              = " " USING "###########"
      LET arr_acreditados43[cont].f_ini_dscto      = " " USING "&&&&&&&&"
      LET arr_acreditados43[cont].nss_liberado     = " " USING "&&&&&&&&&&&"
      LET arr_acreditados43[cont].f_gen_arh        = TODAY 
      LET arr_acreditados43[cont].sdo_credito      = " " USING "&&&&&&&&"
      LET arr_acreditados43[cont].f_prox_liq       = " " USING "&&&&&&&&"
      LET arr_acreditados43[cont].f_desde_avis_des = " " USING "&&&&&&&&"
      LET arr_acreditados43[cont].f_hasta_avis_des = " " USING "&&&&&&&&"
      LET arr_acreditados43[cont].tpo_rch          = " " USING "&&"

      LET v_detalle = arr_acreditados43[cont].tpo_registro, 
                      arr_acreditados43[cont].nss,
                      arr_acreditados43[cont].num_credito,
                      arr_acreditados43[cont].ssv_92_97_deudor,
                      arr_acreditados43[cont].f_otorgamiento USING "yyyymmdd",
                      arr_acreditados43[cont].f_culminacion  USING "yyyymmdd",
                      arr_acreditados43[cont].tpo_credito,
                      arr_acreditados43[cont].status_credito,
                      arr_acreditados43[cont].tpo_dscto,
                      arr_acreditados43[cont].val_dscto,
                      arr_acreditados43[cont].nrp,
                      arr_acreditados43[cont].f_ini_dscto,
                      arr_acreditados43[cont].nss_liberado,
                      arr_acreditados43[cont].f_gen_arh      USING "yyyymmdd",
                      arr_acreditados43[cont].sdo_credito,
                      arr_acreditados43[cont].f_prox_liq,
                      arr_acreditados43[cont].f_desde_avis_des,
                      arr_acreditados43[cont].f_hasta_avis_des,
                      arr_acreditados43[cont].tpo_rch

      LET cont = cont + 1

      --Se escribe en el archivo de salida
      CALL ch.writeLine(v_detalle)
      
   END FOREACH

   --Llenamos el filer con ceros
   LET v_filer = 0

   FOR i = 1 TO 111
      LET v_filer[i] = v_filer  
   END FOR 

   LET arr_sumario[1].tpo_registro      = "99"
   LET arr_sumario[1].tot_registros     = v_contador + 1
   LET arr_sumario[1].tot_reg01         = "1"
   LET arr_sumario[1].tot_registros_det = v_contador
   LET arr_sumario[1].filer             = v_filer

   LET v_sumario = arr_sumario[1].tpo_registro,
                   arr_sumario[1].tot_registros USING "&&&&&&&",
                   arr_sumario[1].tot_reg01 USING "&&&&&&&",
                   arr_sumario[1].tot_registros_det USING "&&&&&&&",
                   arr_sumario[1].filer  

   CALL ch.writeLine([v_sumario])

   CALL ch.close()

   --Guarda registro del archivo generado en la tabla de control
   INSERT INTO ocg_arh_rec_marcas
      VALUES(v_fecha_Actual,v_contador, g_usuario);

   -- Se realiza respaldo del archivo generado
   LET v_cp_comando = "cp"," ",v_archivo_salida," ",v_ruta_envio CLIPPED,"/RECMARCA_" CLIPPED, TODAY USING "ddmmyyyy" CLIPPED,".TXT"
   RUN v_cp_comando RETURNING v_ind_cp

   IF(v_ind_cp = 0) THEN
      DISPLAY " => El archivo recurrente de marcas 43Bis se ha generado correctamente"
      DISPLAY " => El respaldo del archivo RECMARCA.TXT se realizó correctamente"
      DISPLAY " => El archivo de marcas se ha generado en la ruta /safreviv_int/ocg/envio"
   END IF 

   -- Ejecución del Script de envío del archivo a TRM
   LET v_env_comando = "sh"," ","/opt/Interpel/Scripts/SOA16025.sh" #Envío de Producción
   --LET v_env_comando = "sh"," ","/opt/Interpel/Scripts/SOA16025_QA.sh" #Envío QA 
   RUN v_env_comando RETURNING v_ind_envio

   IF(v_ind_envio = 0) THEN
      DISPLAY " => El archivo de marcas 43Bis se ha enviado a TRM exitosamente"
      DISPLAY ""
   ELSE 
      DISPLAY " => No se pudo enviar el archivo de marcas 43Bis a TRM"
      DISPLAY ""
   END IF 

   RETURN v_ind_cp
   
END FUNCTION 
