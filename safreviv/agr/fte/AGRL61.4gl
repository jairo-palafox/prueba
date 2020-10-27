##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRL61                                             #
#Objetivo          => Lanzador para la transacción Microflujo informe.   #
#Autor             => Emilio Abarca, EFP                                 #
#Fecha inicio      => 10/Abril/2017                                      #
##########################################################################

DATABASE safre_viv

   DEFINE g_usuario         CHAR(20)
   DEFINE g_tipo_ejecucion  SMALLINT
   DEFINE g_nom_ventana     STRING
   DEFINE g_proceso_cod     INTEGER 
   DEFINE g_opera_cod       INTEGER 
   DEFINE v_ruta_ejecutable CHAR(40)
   DEFINE v_ruta_listados   CHAR(40)
   DEFINE v_respuesta       BOOLEAN
   DEFINE v_pid             DECIMAL(9,0)
   DEFINE v_return          SMALLINT 
   DEFINE v_s_comando       STRING 
   DEFINE v_notifica        STRING 
   DEFINE v_f_dia           DATE 

MAIN 

   LET g_usuario         = ARG_VAL(1)
   LET g_tipo_ejecucion  = ARG_VAL(2)
   LET g_nom_ventana     = ARG_VAL(3)
   LET g_proceso_cod     = 339
   LET g_opera_cod       = 1

   CALL STARTLOG(g_usuario CLIPPED|| ".OCGL61.log")

   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'agr'
    
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat' 

   CLOSE WINDOW SCREEN 

   OPEN WINDOW vtn WITH FORM "AGRL611"

      LET v_f_dia = TODAY 


      MENU
         BEFORE MENU 
         DISPLAY v_f_dia TO e_dia_ejecuta

         ON ACTION ACCEPT 
            CALL consulta_microflujo_informe()
        
         ON ACTION CANCEL 
            EXIT MENU 
         
      END MENU 
   
   CLOSE WINDOW vtn
   
END MAIN 

FUNCTION consulta_microflujo_informe()

   DEFINE v_query_tmt           STRING
   DEFINE v_query_orig          STRING 
   DEFINE v_desc_tmt1           CHAR(30)
   DEFINE v_desc_tmt2           CHAR(30)
   DEFINE v_desc_orig1          CHAR(30)
   DEFINE v_desc_orig2          CHAR(30) 
   DEFINE v_tmt_ex              INTEGER
   DEFINE v_tmt_no_ex           INTEGER
   DEFINE v_orig_ex             INTEGER
   DEFINE v_orig_no_ex          INTEGER
   DEFINE v_total               INTEGER
   DEFINE v_tmt_ex_prc          DECIMAL(3,0)
   DEFINE v_tmt_no_ex_prc       DECIMAL(3,0)
   DEFINE v_orig_ex_prc         DECIMAL(3,0)
   DEFINE v_orig_no_ex_prc      DECIMAL(3,0)
   DEFINE v_total_prc           DECIMAL(3,0)
   DEFINE v_prc_final           CHAR(6)
   DEFINE v_total_tmts          INTEGER
   DEFINE v_ef_tmt_exitoso      DECIMAL(3,0)
   DEFINE v_c_efectividad_tmt   CHAR(6)
   DEFINE v_ef_tmt_no_exitoso   DECIMAL(3,0)
   DEFINE v_c_ef_tmt_no_exitoso CHAR(6)
   DEFINE v_total_origs         INTEGER 
   DEFINE v_ef_orig_exitosas    DECIMAL(3,0)
   DEFINE v_c_efectividad_orig  CHAR(6)
   DEFINE v_ef_orig_no_exitosas DECIMAL(3,0)
   DEFINE v_c_ef_orig_no_ex     CHAR(6)
   DEFINE v_deudor_tmt1         INTEGER 
   DEFINE v_deudor_tmt2         INTEGER 
   DEFINE v_deudor_orig1        INTEGER 
   DEFINE v_deudor_orig2        INTEGER
   DEFINE v_total_deudores      INTEGER 
    
   -- Record que recupera información de los trámites
   DEFINE r_tramite RECORD
      id_cre_tramite     DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      num_credito        DECIMAL(10,0),
      estado    SMALLINT
   END RECORD 

   DEFINE v_id_cre_acreditado DECIMAL(9,0)
   DEFINE v_sdo_deudor        DECIMAL(12,2)

   LET v_desc_tmt1   = "Trámites Exitosos"
   LET v_desc_tmt2   = "Trámites No Exitosos"
   LET v_desc_orig1  = "Originaciones Exitosas"
   LET v_desc_orig2  = "Originaciones No Exitosas"
   LET v_tmt_ex_prc     = 0
   LET v_tmt_no_ex_prc  = 0
   LET v_orig_ex_prc    = 0
   LET v_orig_no_ex_prc = 0
   LET v_total_tmts     = 0
   LET v_total_origs    = 0
   LET v_ef_tmt_exitoso = 0
   LET v_ef_tmt_no_exitoso   = 0
   LET v_ef_orig_exitosas    = 0
   LET v_ef_orig_no_exitosas = 0

   -- Obtiene trámites exitosos y no exitosos 
   LET v_query_tmt = "SELECT t.id_cre_tramite,
                             t.id_derechohabiente,
                             t.num_credito,
                             t.estado
                        FROM cre_tramite t,
                             cre_his_tramite h
                       WHERE t.id_cre_tramite = h.id_cre_tramite 
                         AND t.num_credito    = h.num_credito 
                         AND t.estado IN (18,240) 
                         AND t.estado = h.estado
                         AND DATE(h.f_proceso) = TODAY;"

   -- Obbtiene Originaciones Exitosas y No Exitosas 
   LET v_query_orig = "SELECT t.id_cre_tramite,
                              t.id_derechohabiente,
                              t.num_credito,
                              t.estado
                         FROM cre_tramite t,
                              cre_his_tramite h
                        WHERE t.id_cre_tramite = h.id_cre_tramite 
                          AND t.num_credito    = h.num_credito 
                          AND t.estado IN (20,19) 
                          AND t.estado = h.estado
                          AND DATE(h.f_proceso) = TODAY;"
                      

   OPEN WINDOW vtn2 WITH FORM "AGRL612"
   
      MENU ""
         BEFORE MENU 
            DISPLAY TIME (CURRENT) TO e_hora
            DISPLAY v_desc_tmt1  TO e_desc_tmt1
            DISPLAY v_desc_tmt2  TO e_desc_tmt2
            DISPLAY v_desc_orig1 TO e_desc_orig1
            DISPLAY v_desc_orig2 TO e_Desc_orig2

         -- "TRÁMITES"
         INITIALIZE r_tramite.* TO NULL 

         -- Inicializando variables para trámites
         LET v_id_cre_acreditado = 0
         LET v_total_deudores    = 0
         LET v_sdo_deudor  = 0
         LET v_tmt_ex      = 0
         LET v_tmt_no_ex   = 0
         LET v_deudor_tmt1 = 0
         LET v_deudor_tmt2 = 0

         PREPARE prp_tmt FROM v_query_tmt
         DECLARE crs_tmt CURSOR FOR prp_tmt

         FOREACH crs_tmt INTO r_tramite.id_cre_tramite,
                               r_tramite.id_derechohabiente,
                               r_tramite.num_credito,
                               r_tramite.estado
                               
            -- Limpia variables por cada iteración
            LET v_id_cre_acreditado = NULL 
            LET v_sdo_deudor = NULL 
            
            -- Obtiene el máximo registro de cre_acreditado
            -- Sólo tipo de crédito 10 (COFINAVIT AG) ya que son trámites
            SELECT FIRST 1 a.id_cre_acreditado,
                          a.sdo_deudor
                    INTO v_id_cre_acreditado,
                          v_sdo_deudor
                    FROM cre_acreditado a,
                         cat_tipo_credito r
                   WHERE a.id_derechohabiente = r_tramite.id_derechohabiente
                     AND a.num_credito = r_tramite.num_credito
                     AND a.tpo_credito = 10
                     AND a.tpo_credito = r.tpo_credito
                     AND a.tpo_originacion = r.tpo_originacion
                     ORDER BY a.id_cre_acreditadO DESC;

            IF(v_id_cre_acreditado IS NOT NULL) THEN

               -- Conteo Trámite Exitoso
               IF(r_tramite.estado = 18) THEN

                  LET v_tmt_ex = v_tmt_ex + 1 

                  -- Conteo deudor Trámite Exitoso
                  IF(v_sdo_deudor > 0) THEN 
                     LET v_deudor_tmt1 = v_deudor_tmt1 + 1
                  END IF 
                  
               END IF 

               -- Conteo Trámite No Exitoso
               IF(r_tramite.estado = 240) THEN
               
                  LET v_tmt_no_ex = v_tmt_no_ex + 1 

                  -- Conteo deudor Trámite No Exitoso
                  IF(v_sdo_deudor > 0) THEN 
                     LET v_deudor_tmt2 = v_deudor_tmt2 + 1
                  END IF
                  
               END IF 
               
            END IF 
            
         END FOREACH 

         -- "ORIGINACIONES"
         INITIALIZE r_tramite.* TO NULL 

         -- Inicializando variables para las Originaciones
         LET v_id_cre_acreditado = 0
         LET v_sdo_deudor   = 0
         LET v_orig_ex      = 0
         LET v_orig_no_ex   = 0
         LET v_deudor_orig1 = 0
         LET v_deudor_orig2 = 0

         PREPARE prp_orig FROM v_query_orig
         DECLARE crs_orig CURSOR FOR prp_orig

         FOREACH crs_orig INTO r_tramite.id_cre_tramite,
                                r_tramite.id_derechohabiente,
                                r_tramite.num_credito,
                                r_tramite.estado

            -- Limpia variables por cada iteración
            LET v_id_cre_acreditado = NULL 
            LET v_sdo_deudor = NULL 

            -- Obtiene el máximo registro de cre_acreditado
            -- para cualquier tipo de crédito, ya que son Originaciones.
            SELECT FIRST 1 a.id_cre_acreditado,
                            a.sdo_deudor
                      INTO v_id_cre_acreditado,
                           v_sdo_deudor
                      FROM cre_acreditado a,
                           cat_tipo_credito r
                     WHERE a.id_derechohabiente = r_tramite.id_derechohabiente
                       AND a.num_credito = r_tramite.num_credito
                       AND a.tpo_credito = r.tpo_credito
                       AND a.tpo_originacion = r.tpo_originacion
                       ORDER BY a.id_cre_acreditado DESC;

            IF(v_id_cre_acreditado IS NOT NULL) THEN

               -- Conteo Originaciones Exitosas
               IF(r_tramite.estado = 20) THEN
               
                  LET v_orig_ex = v_orig_ex + 1

                  -- Conteo deudor Originacion Exitosa
                  IF(v_sdo_deudor > 0) THEN 
                     LET v_deudor_orig1 = v_deudor_orig1 + 1
                  END IF 
                  
               END IF 

               -- Conteo Originacones No Exitosas
               IF(r_tramite.estado = 19) THEN
               
                  LET v_orig_no_ex = v_orig_no_ex + 1

                 -- Conteo deudor Originaciones No Exitosas
                  IF(v_sdo_deudor > 0) THEN 
                     LET v_deudor_orig2 = v_deudor_orig2 + 1
                  END IF 
                  
               END IF 
               
            END IF 
            
         END FOREACH 

         -- Obtiene total de registros
         LET v_total = v_tmt_ex + v_tmt_no_ex + v_orig_ex + v_orig_no_ex
        
         -- Porcentaje para Trámites exitosos
         LET v_tmt_ex_prc   = (v_tmt_ex / v_total) * 100
        
         -- Porcentaje para Trámites no exitosos
         LET v_tmt_no_ex_prc = (v_tmt_no_ex / v_total) * 100
        
         -- Porcentaje para Originaciones exitosas
         LET v_orig_ex_prc   = (v_orig_ex / v_total) * 100
          
         -- Porcentaje para Originaciones no exitosas
         LET v_orig_no_ex_prc = (v_orig_no_ex / v_total) * 100
    
         -- Suma total del porcentaje
         LET v_total_prc = v_tmt_ex_prc CLIPPED + v_tmt_no_ex_prc CLIPPED  + v_orig_ex_prc CLIPPED + v_orig_no_ex_prc CLIPPED    
         LET v_prc_final = v_total_prc CLIPPED,"%"
    
         --Calcula efectividad porcentaje de tràmites exitosos
         LET v_total_tmts = v_tmt_ex + v_tmt_no_ex CLIPPED
         LET v_ef_tmt_exitoso = (v_tmt_ex / v_total_tmts) * 100 
         LET v_c_efectividad_tmt = v_ef_tmt_exitoso CLIPPED,"%"

         --Calcula efectividad porcentaje de trámites no exitosos
         LET v_ef_tmt_no_exitoso = (v_tmt_no_ex / v_total_tmts) * 100
         LET v_c_ef_tmt_no_exitoso = v_ef_tmt_no_exitoso CLIPPED,"%"
         
         --Calcula porcentaje de originaciones exitosas
         LET v_total_origs = v_orig_ex + v_orig_no_ex CLIPPED 
         LET v_ef_orig_exitosas = (v_orig_ex / v_total_origs) * 100
         LET v_c_efectividad_orig = v_ef_orig_exitosas CLIPPED,"%"

         --Calcula porcentaje de originaciones no exitosas
         LET v_ef_orig_no_exitosas = (v_orig_no_ex / v_total_origs) * 100
         LET v_c_ef_orig_no_ex     = v_ef_orig_no_exitosas CLIPPED,"%"
    
         -- Total Deudores
         LET v_total_deudores = v_deudor_tmt1 + v_deudor_tmt2 + v_deudor_orig1 + v_deudor_orig2
         
         -- Despliegua información.
         DISPLAY v_tmt_ex              TO e_tmt_ex
         DISPLAY v_tmt_no_ex           TO e_tmt_no_ex
         DISPLAY v_orig_ex             TO e_orig_ex
         DISPLAY v_orig_no_ex          TO e_orig_no_ex
         DISPLAY v_total               TO e_total
         DISPLAY v_tmt_ex_prc          TO e_tmt1
         DISPLAY v_tmt_no_ex_prc       TO e_tmt2
         DISPLAY v_orig_ex_prc         TO e_orig1
         DISPLAY v_orig_no_ex_prc      TO e_orig2
         DISPLAY v_prc_final           TO e_porcentaje
         DISPLAY v_c_efectividad_tmt   TO e_ef_tmt_exitoso
         DISPLAY v_c_ef_tmt_no_exitoso TO e_ef_tmt_no_exitoso
         DISPLAY v_c_efectividad_orig  TO e_ef_orig_exitosas
         DISPLAY v_c_ef_orig_no_ex     TO e_ef_orig_no_exitosas
         DISPLAY v_deudor_tmt1         TO e_deudor_tmt1
         DISPLAY v_deudor_tmt2         TO e_deudor_tmt2
         DISPLAY v_deudor_orig1        TO e_deudor_orig1
         DISPLAY v_deudor_orig2        TO e_deudor_orig2
         DISPLAY v_total_deudores      TO e_total_deudor   
         
         ON ACTION Informe
            LET v_respuesta = fn_ventana_confirma("","¿Está seguro de generar el informe .PDF y el extractor de microflujo?","")
            IF (v_respuesta = 0) THEN
               CALL fn_mensaje("","Se ha cancelado la generación del informe","")
            ELSE 
               -- Genera PID
               LET v_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

               -- Inicializa proceso
               CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,"","AGRS11","",g_usuario) RETURNING v_return

               -- Actualiza operación como iniciada
               CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,g_opera_cod,"","AGRS11","",g_usuario) RETURNING v_return

               -- Ejecuta lanzado
               LET v_s_comando = "nohup fglrun ",
                                  v_ruta_ejecutable CLIPPED,
                                  "/AGRS11 ",
                                  g_usuario," ",
                                  v_pid," ",
                                  g_proceso_cod," ",
                                  g_opera_cod," ",
                                  1," ",
                                  " ' ' 1>",v_ruta_listados CLIPPED,
                                  "/nohup:",v_pid USING "&&&&&",":",
                                  g_proceso_cod   USING "&&&&&",":",
                                  g_opera_cod     USING "&&&&&" ," 2>&1 &"

               --DISPLAY "v_s_comando: ",v_s_comando
               RUN v_s_comando 

               LET v_notifica = "Se ejecutó la generación del PDF y el extractor de microflujo"," \n",
                                 "Verificar en el monitor de procesos el PID: ",v_pid USING "<<<<<<<<<"

               CALL fn_mensaje("",v_notifica,"")
               EXIT MENU 
               
            END IF

            ON ACTION Salir
               EXIT MENU 
            
      END MENU 
      
   CLOSE WINDOW vtn2
   
END FUNCTION 