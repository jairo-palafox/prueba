################################################################################
#Modulo            => AGR                                                      #
#Programa          => AGRS13                                                   #
#Objetivo          => Genera archivo de rechazos,operación "4" para el proceso #
#                     302-RECEPCIÓN USO ANUALIDADES GARANTIZADAS.              #
#Autor             => Emilio Abarca EFP                                        #
#Fecha inicio      => 16/Octubre/2017                                          #
################################################################################


DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario        CHAR(20)   
   DEFINE p_pid            DECIMAL(9,0)
   DEFINE p_proceso_cod    INTEGER
   DEFINE p_opera_cod      INTEGER
   DEFINE p_id_cre_ctr_arh DECIMAL(9,0)
   DEFINE v_ruta_envio     CHAR(40)
   DEFINE v_band_operacion SMALLINT 
   DEFINE V_ind_fin        SMALLINT
END GLOBALS 

MAIN

   -- Recibe parámetros que envía el programa AGRP13
   LET g_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_id_cre_ctr_arh = ARG_VAL(3)
   LET p_proceso_cod    = 302
   LET p_opera_cod      = 4

   CALL STARTLOG(g_usuario CLIPPED || ".AGRS13.log")

   --Se obtiene la ruta donde se alojará el archivo de rechazos
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   CALL fn_display_proceso(0,"GENERA ARCHIVO RECHAZOS") 
   DISPLAY ""

   -- Inicializa la operación
   CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,p_opera_cod,"","AGRS13","",g_usuario) RETURNING v_band_operacion
   
   -- Llama función principal
   CALL fn_genera_arh_salida() RETURNING v_ind_fin

   IF(v_ind_fin <> 0) THEN
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_band_operacion
   ELSE 
      --Finaliza operación
      CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod) RETURNING v_band_operacion
      
      CALL fn_display_proceso(1,"GENERA ARCHIVO RECHAZOS")
      DISPLAY ""
      
   END IF 
   
END MAIN 

FUNCTION fn_genera_arh_salida()

   DEFINE archivo       base.channel
   DEFINE v_arh_salida  STRING
   DEFINE v_qry_240     STRING
   DEFINE v_qry_150     STRING 
   DEFINE cont          INTEGER
   DEFINE v_detalle     STRING 
   DEFINE v_termina     SMALLINT 
   DEFINE v_aux_importe DECIMAL(12,2)
   
   DEFINE r_rechazos    RECORD
      nss         CHAR(11),
      num_credito CHAR(10),
      f_genera    DATE,
      importe     CHAR(15),
      tpo_sol     CHAR(1),
      causal      CHAR(3)
   END RECORD 

   LET v_qry_240 = "SELECT f.nss,
                           u.num_credito,
                           u.importe_v97,
                           u.tpo_uso,
                           u.diagnostico
                      FROM cre_uso_garantia u,
                           afi_derechohabiente f
                     WHERE u.id_Cre_ctr_archivo = ",p_id_cre_ctr_arh,
                     " AND u.id_derechohabiente = f.id_derechohabiente
                       AND u.estado = 240;"

   LET v_qry_150 = "SELECT f.nss,
                           u.num_credito,
                           u.importe_v97,
                           u.tpo_uso,
                           u.estado
                      FROM cre_uso_garantia u,
                           afi_derechohabiente f
                     WHERE u.id_Cre_ctr_archivo = ",p_id_cre_ctr_arh,
                     "  AND u.id_derechohabiente = f.id_derechohabiente
                        AND u.estado = 150;"

   LET v_aux_importe = 0
   LET v_detalle     = NULL 
   LET v_arh_salida  = v_ruta_envio CLIPPED,"/R_S",TODAY USING "yyyymmdd",".cag"

   LET archivo = base.Channel.create()
   
   -- Abre archivo de salida para escritura
   CALL archivo.openFile(v_arh_salida,"w")

   PREPARE prp_240 FROM v_qry_240
   DECLARE crs_240 CURSOR FOR prp_240

   LET cont = 1
   
   INITIALIZE r_rechazos.* TO NULL 
   
   FOREACH crs_240 INTO  r_rechazos.nss,
                          r_rechazos.num_credito,
                          v_aux_importe,
                          r_rechazos.tpo_sol,
                          r_rechazos.causal

      LET r_rechazos.f_genera = TODAY
      LET r_rechazos.importe  = v_aux_importe * 100
        
      LET v_detalle = r_rechazos.nss,
                      r_rechazos.num_credito USING "&&&&&&&&&&",
                      r_rechazos.f_genera USING "yyyymmdd",
                      r_rechazos.importe USING "&&&&&&&&&&&&&&&",
                      r_rechazos.tpo_sol,
                      r_rechazos.causal USING "&&&"
      
      CALL archivo.writeLine(v_detalle)
      
      LET cont = cont + 1
      
   END FOREACH 

   LET v_aux_importe = 0
   INITIALIZE r_rechazos.* TO NULL 

   PREPARE prp_150 FROM v_qry_150
   DECLARE crs_150 CURSOR FOR prp_150

   FOREACH crs_150 INTO r_rechazos.nss,
                         r_rechazos.num_credito,
                         v_aux_importe,
                         r_rechazos.tpo_sol,
                         r_rechazos.causal

      LET r_rechazos.f_genera = TODAY
      LET r_rechazos.importe  = v_aux_importe * 100

      LET v_detalle = r_rechazos.nss,
                      r_rechazos.num_credito USING "&&&&&&&&&&",
                      r_rechazos.f_genera USING "yyyymmdd",
                      r_rechazos.importe USING "&&&&&&&&&&&&&&&",
                      r_rechazos.tpo_sol,
                      r_rechazos.causal USING "&&&"

      CALL archivo.writeLine(v_detalle)
      
      LET cont = cont + 1
      
   END FOREACH 

   -- Cierra archivo
   CALL archivo.close()

   LET v_termina = 0
   
   -- En caso de que no encontrar rechazos
   IF(cont = 1) THEN
      DISPLAY " => No se encontraron registros rechazados"
   ELSE
      DISPLAY " => Total de registros rechazados: ",cont -1
   END IF 

   DISPLAY " => El archivo de rechazos se ha generado en la ruta: \n /safreviv_int/agr_envio con el nombre: R_S",TODAY USING "yyyymmdd",".cag"

   RETURN v_termina 
     
END FUNCTION 