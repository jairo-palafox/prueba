##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRL62                                             #
#Objetivo          => Lanzador para la transacción Recurrente informe.   #
#                     (Archivo recurrente que llega de cartera).         #
#Autor             => Emilio Abarca, EFP                                 #
#Fecha inicio      => 08/Junio/2017                                      #
##########################################################################

DATABASE safre_viv

   DEFINE g_usuario         CHAR(20)
   DEFINE g_tipo_ejecucion  SMALLINT
   DEFINE g_nom_ventana     STRING
   DEFINE g_proceso_cod     INTEGER 
   DEFINE g_opera_cod       INTEGER 
   DEFINE v_dia             DATE
   DEFINE v_fecha           DATE  
   DEFINE v_ruta_ejecutable CHAR(40)
   DEFINE v_ruta_listados   CHAR(40)
   DEFINE v_respuesta       BOOLEAN
   DEFINE v_pid             DECIMAL(9,0)
   DEFINE v_return          SMALLINT
   DEFINE v_s_comando       STRING
   DEFINE v_notifica        STRING 
   DEFINE v_ind_dia         SMALLINT 
   DEFINE i                 INTEGER 
   DEFINE v_dia_inhabil     INTEGER 

MAIN 

   LET g_usuario         = ARG_VAL(1)
   LET g_tipo_ejecucion  = ARG_VAL(2)
   LET g_nom_ventana     = ARG_VAL(3)
   LET g_proceso_cod     = 340   -- PROCESO  : RECURRENTE INFORME
   LET g_opera_cod       = 1     -- OPERACION: GENERA RECURRENTE INFORME

   CALL STARTLOG(g_usuario CLIPPED|| ".OCGL62.log")

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

   LET v_dia = TODAY  
   
   --Verifica que el dia anterior sea un día hábil
   FOR i = 1 TO 10
   
      SELECT COUNT(*)
         INTO v_dia_inhabil
         FROM cat_feriado
        WHERE feriado_fecha = v_dia -i 

      -- Si el día es inhabil
      IF (v_dia_inhabil >= 1) THEN 
         CONTINUE FOR 
      ELSE 
         -- Verifica que no sea un domingo
         LET v_ind_dia = WEEKDAY(v_dia -i)

         -- 0 (Domingo)
         IF(V_ind_dia = 0) THEN
            CONTINUE FOR
         ELSE 
            --Asigna ese día hábil que no sea domingo y se sale del FOR 
            LET v_fecha = v_dia -i  
            EXIT FOR    
         END IF 
         
      END IF 
      
   END FOR  
   
   
   OPEN WINDOW vtn WITH FORM "AGRL621"

      MENU ""
         BEFORE MENU
            DISPLAY v_fecha TO e_ejecucion
        
         ON ACTION ACCEPT 
            CALL consulta_recurrente_informe()
            
         ON ACTION CANCEL 
            EXIT MENU 
         
      END MENU 
   
   CLOSE WINDOW vtn
   
END MAIN 

FUNCTION consulta_recurrente_informe()

   DEFINE e_desc_orig        CHAR(40)
   DEFINE e_desc_rtv         CHAR(25)
   DEFINE e_desc_cancel      CHAR(25)
   DEFINE e_desc_liq         CHAR(25)
   DEFINE e_desc_mov         CHAR(25)
   DEFINE v_query            STRING
   DEFINE v_t_originacion_a  INTEGER
   DEFINE v_t_reactivacion_a INTEGER 
   DEFINE v_t_cancelacion_a  INTEGER 
   DEFINE v_t_liquidacion_a  INTEGER 
   DEFINE v_t_mov_no_val_a   INTEGER 
   DEFINE v_t_originacion_r  INTEGER
   DEFINE v_t_reactivacion_r INTEGER 
   DEFINE v_t_cancelacion_r  INTEGER 
   DEFINE v_t_liquidacion_r  INTEGER 
   DEFINE v_t_mov_no_val_r   INTEGER 
   DEFINE contador           INTEGER 
   DEFINE cont_acep          INTEGER
   DEFINE cont_rch           INTEGER  
   DEFINE v_suma_acept       INTEGER
   DEFINE v_suma_rchz        INTEGER 
   DEFINE v_t_glo_orig       INTEGER 
   DEFINE v_t_glo_reactiva   INTEGER 
   DEFINE v_t_glo_cancela    INTEGER 
   DEFINE v_t_glo_liquida    INTEGER 
   DEFINE v_t_glo_mov        INTEGER 
   DEFINE v_suma_reg         INTEGER 
   DEFINE v_prc_orig         DECIMAL(3,0)
   DEFINE v_prc_reactiva     DECIMAL(3,0)
   DEFINE v_prc_cancela      DECIMAL(3,0)
   DEFINE v_prc_liquida      DECIMAL(3,0)
   DEFINE v_prc_mov          DECIMAL(3,0)
   DEFINE v_total_prc        DECIMAL(4,0)
   DEFINE v_prc_final        CHAR(7)
   DEFINE v_prc_orig_a       DECIMAL(3,0)
   DEFINE v_prc_reactiva_a   DECIMAL(3,0)
   DEFINE v_prc_cancela_a    DECIMAL(3,0)
   DEFINE v_prc_liquida_a    DECIMAL(3,0)
   DEFINE v_prc_mov_a        DECIMAL(3,0)
   DEFINE v_prc_aceptados    DECIMAL(4,0)
   DEFINE v_prc_fin_acept    CHAR(7)
   DEFINE v_prc_orig_r       DECIMAL(3,0)
   DEFINE v_prc_reactiva_r   DECIMAL(3,0)
   DEFINE v_prc_cancela_r    DECIMAL(3,0)
   DEFINE v_prc_liquida_r    DECIMAL(3,0)
   DEFINE v_prc_mov_r        DECIMAL(3,0)
   DEFINE v_prc_rchz         DECIMAL(4,0)
   DEFINE v_prc_fin_rchz     CHAR(7)

   DEFINE v_id_archivo       DECIMAL(9,0)
  
   --Record que recupera cifra total por cada entidad
   DEFINE r_conteo_entidad RECORD 
      entidad   SMALLINT,
      total     INTEGER 
   END RECORD 


   LET e_desc_orig   = "Originaciones (incluye reactivaciones)"
   LET e_desc_rtv    = "Reactivaciones"
   LET e_desc_cancel = "Cancelaciones"
   LET e_desc_liq    = "Liquidaciones"
   LET e_desc_mov    = "Movimientos no válidos"

   -- Inicia variables
   LET v_t_originacion_a  = 0
   LET v_t_reactivacion_a = 0 
   LET v_t_cancelacion_a  = 0 
   LET v_t_liquidacion_a  = 0
   LET v_t_mov_no_val_a   = 0 -- No hay aceptados para esta entidad
   LET v_t_mov_no_val_r   = 0
   LET v_suma_acept       = 0
   LET v_t_glo_orig       = 0 
   LET v_t_glo_reactiva   = 0
   LET v_t_glo_cancela    = 0
   LET v_t_glo_liquida    = 0
   LET v_t_glo_mov        = 0
   LET v_suma_reg         = 0
   LET v_prc_orig_a       = 0 
   LET v_prc_reactiva_a   = 0  
   LET v_prc_cancela_a    = 0  
   LET v_prc_liquida_a    = 0  
   LET v_prc_mov_a        = 0 
   LET v_prc_aceptados    = 0 
   LET v_prc_orig_r       = 0
   LET v_prc_reactiva_r   = 0
   LET v_prc_cancela_r    = 0
   LET v_prc_liquida_r    = 0
   LET v_prc_mov_r        = 0
   LET v_prc_rchz         = 0
   LET v_t_originacion_r  = 0 
   LET v_t_reactivacion_r = 0 
   LET v_t_cancelacion_r  = 0
   LET v_t_liquidacion_r  = 0
   LET v_suma_rchz        = 0

   -- Obtiene los ID´S de los archivos que se procesaron el día anterior
   LET v_query = "SELECT UNIQUE id_cre_ctr_archivo
                    FROM cre_ctr_archivo
                   WHERE id_proceso = 301
                     AND nom_archivo NOT MATCHES 'ORIGINA_MF*'
                     AND f_proceso = ",'"',v_fecha,'"'

   PREPARE prp_archivos FROM v_query
   DECLARE crs_archivos CURSOR FOR prp_archivos

   LET v_id_archivo = NULL
   LET contador  = 1
   LET cont_acep = 1
   LET cont_rch  = 1
   
   INITIALIZE r_conteo_entidad TO NULL 

   FOREACH crs_archivos INTO v_id_archivo

      IF(v_id_archivo IS NOT NULL) THEN

         ###################################################
         ## Registros Recepción recurrente Originación AG ##
         ###################################################
         
         # Búsca los registros aceptados que se guardan en cre_acreditado
            -- Entidad 0 --> Movimientos no válidos
            -- Entidad 1 --> Originaciones
            -- Entidad 2 --> Liquidaciones
            -- Entidad 5 --> Cancelaciones

         DECLARE crs_aceptados CURSOR FOR 
            SELECT m.entidad,COUNT(*)
              FROM cre_acreditado a,
                   cat_maq_credito m
             WHERE a.id_cre_ctr_archivo = v_id_archivo
               AND a.estado = m.estado
               AND m.entidad IN (0,1,2,5)
               GROUP BY 1;

         FOREACH crs_aceptados INTO r_conteo_entidad.entidad,
                                     r_conteo_entidad.total

            -- Entidad 0 --> Movimientos no validos (RECHAZOS por default)
            IF(r_conteo_entidad.entidad = 0) THEN
               LET v_t_mov_no_val_r   = v_t_mov_no_val_r + r_conteo_entidad.total
            END IF 
            
            -- Entidad 1 --> Originaciones (Incluye reactivaciones)
            IF(r_conteo_entidad.entidad = 1) THEN
               LET v_t_originacion_a = v_t_originacion_a + r_conteo_entidad.total
            END IF
            
            -- Entidad 2 --> Liquidaciones
            IF(r_conteo_entidad.entidad = 2) THEN
               LET v_t_liquidacion_a = v_t_liquidacion_a + r_conteo_entidad.total
            END IF
            
            -- Entidad 5 --> Cancelaciones
            IF(r_conteo_entidad.entidad = 5) THEN
               LET v_t_cancelacion_a = v_t_cancelacion_a + r_conteo_entidad.total
            END IF

            LET cont_acep = cont_acep + 1
            
         END FOREACH 

         ########################################
         ## Registros Solicitud de Desmarca AG ##
         ########################################
         DECLARE crs_acep_sol_dma CURSOR FOR 
            SELECT m.entidad,COUNT(*)
              FROM cre_acreditado a,
                   cre_his_acreditado h,
                   cat_maq_credito m,
                   cat_tipo_credito t
             WHERE a.id_cre_acreditado  = h.id_cre_acreditado
               AND h.id_cre_ctr_archivo = v_id_archivo
               AND a.estado = m.estado
               AND m.entidad IN (2,5)
               AND a.tpo_credito = t.tpo_credito
               AND a.tpo_originacion = t.tpo_originacion
               GROUP BY 1;
               
         -- Limpia el record, ya que se vuelve a utilizar en el siguiente foreach
         INITIALIZE r_conteo_entidad TO NULL 

         FOREACH crs_acep_sol_dma INTO r_conteo_entidad.entidad,
                                        r_conteo_entidad.total
            
            -- Entidad 2 --> Liquidaciones
            IF(r_conteo_entidad.entidad = 2) THEN
               LET v_t_liquidacion_a = v_t_liquidacion_a + r_conteo_entidad.total
            END IF
            
            -- Entidad 5 --> Cancelaciones
            IF(r_conteo_entidad.entidad = 5) THEN
               LET v_t_cancelacion_a = v_t_cancelacion_a + r_conteo_entidad.total
            END IF

            LET cont_acep = cont_acep + 1
            
         END FOREACH 
         
         # Busca los rechazos del archivo en cre_rch_acreditado
         DECLARE crs_rechazos CURSOR FOR 
            SELECT tpo_registro,COUNT(*)
              FROM cre_rch_acreditado
             WHERE id_cre_ctr_archivo = v_id_archivo
             GROUP BY 1;

         -- Limpia el record, ya que se vuelve a utilizar en el siguiente foreach
         INITIALIZE r_conteo_entidad TO NULL
         
         FOREACH crs_rechazos INTO r_conteo_entidad.entidad,
                                    r_conteo_entidad.total

            #Rechazos para las originaciones tpo_registro
           
            --(1(NUEVO ACREDITADO), 20(CAMBIO DE ESTATUS), 4(REACTIVACION DE CRÉDITO)
            IF(r_conteo_entidad.entidad = 01) OR 
              (r_conteo_entidad.entidad = 04) OR 
              (r_conteo_entidad.entidad = 20) THEN 
          
               LET v_t_originacion_r = v_t_originacion_r + r_conteo_entidad.total
               
            END IF 
          
            -- Rechazos para las reactivaciones
            --(4-->REACTIVACION DE CRÉDITOS)
            IF(r_conteo_entidad.entidad = 04) THEN 
               LET v_t_reactivacion_r = v_t_reactivacion_r + r_conteo_entidad.total
            END IF 
          
            --Rechazo para las liquidaciones 
            IF(r_conteo_entidad.entidad = 11) THEN
               LET v_t_liquidacion_r = v_t_liquidacion_r + r_conteo_entidad.total
            END IF 
          
            --Rechazo para las cancelaciones
            IF(r_conteo_entidad.entidad = 5) THEN
               LET v_t_cancelacion_r = v_t_cancelacion_r + r_conteo_entidad.total 
            END IF 

            LET cont_rch = cont_rch + 1
            
         END FOREACH 
      END IF 
      
      LET contador = contador + 1
      
   END FOREACH 

   -- Obtiene total para reactivaciones aceptadas   
   SELECT COUNT(*)
     INTO v_t_reactivacion_a
     FROM cre_ctr_archivo r,
          cre_his_acreditado h
    WHERE r.id_cre_ctr_archivo = h.id_cre_ctr_archivo
      AND r.id_proceso = 301
      AND h.id_cre_acreditado > 0
      AND h.estado IN (20,140)
      AND r.nom_archivo MATCHES '*dma*'
      AND r.f_proceso = v_fecha

   -- Suma todos los registros aceptados
   LET v_suma_acept = v_t_mov_no_val_a + v_t_originacion_a + v_t_liquidacion_a + v_t_cancelacion_a + v_t_reactivacion_a

   --Suma total de registros rechazados
   LET v_suma_rchz = v_t_mov_no_val_r + v_t_originacion_r + v_t_reactivacion_r + v_t_liquidacion_r + v_t_cancelacion_r

   --Aceptados + rechazados, se obtiene total de registros por entidad
   LET v_t_glo_orig     = v_t_originacion_a + v_t_originacion_r
   LET v_t_glo_reactiva = v_t_reactivacion_a + v_t_reactivacion_r
   LET v_t_glo_cancela  = v_t_cancelacion_a + v_t_cancelacion_r
   LET v_t_glo_liquida  = v_t_liquidacion_a + v_t_liquidacion_r
   LET v_t_glo_mov      = v_t_mov_no_val_a + v_t_mov_no_val_r

   LET v_suma_reg = v_t_glo_orig + v_t_glo_reactiva + v_t_glo_cancela + v_t_glo_liquida + v_t_glo_mov
   
   -- calcula porcentaje de cada entidad
   LET v_prc_orig       = (v_t_glo_orig / v_suma_reg) * 100
   LET v_prc_reactiva   = (v_t_glo_reactiva / v_suma_reg) * 100 
   LET v_prc_cancela    = (v_t_glo_cancela / v_suma_reg) * 100
   LET v_prc_liquida    = (v_t_glo_liquida / v_suma_reg) * 100
   LET v_prc_mov        = (v_t_glo_mov / v_suma_reg) * 100

   -- Obtiene porcentaje global
   LET v_total_prc = v_prc_orig + v_prc_reactiva + v_prc_cancela + v_prc_liquida + v_prc_mov
   LET v_prc_final = v_total_prc CLIPPED,"%"

   -- Obtiene % para los registros aceptados
   LET v_prc_orig_a     = (v_t_originacion_a / v_t_glo_orig) * 100
   LET v_prc_reactiva_a = (v_t_reactivacion_a / v_t_glo_reactiva) * 100
   LET v_prc_cancela_a  = (v_t_cancelacion_a / v_t_glo_cancela) * 100
   LET v_prc_liquida_a  = (v_t_liquidacion_a /v_t_glo_liquida ) * 100
   LET v_prc_mov_a      = (v_t_mov_no_val_a / v_t_glo_mov) * 100

   -- Porcentaje final aceptados
   let v_prc_aceptados = (v_suma_acept / v_suma_reg) * 100
   let v_prc_fin_acept = v_prc_aceptados CLIPPED,"%"

   -- obtiene % para los registros rechazados
   LET v_prc_orig_r       = (v_t_originacion_r / v_t_glo_orig) * 100
   LET v_prc_reactiva_r   = (v_t_reactivacion_r / v_t_glo_reactiva) * 100
   LET v_prc_cancela_r    = (v_t_cancelacion_r / v_t_glo_cancela) * 100
   LET v_prc_liquida_r    = (v_t_liquidacion_r / v_t_glo_liquida) * 100
   LET v_prc_mov_r        = (v_t_mov_no_val_r / v_t_glo_mov) * 100

   -- Porcentaje final rechazados
   LET v_prc_rchz     = (v_suma_rchz / v_suma_reg) * 100
   LET v_prc_fin_rchz = v_prc_rchz CLIPPED,"%"

OPEN WINDOW vtn2 WITH FORM "AGRL622"

   MENU ""
      BEFORE MENU 
         DISPLAY TIME (CURRENT) TO e_hora
         DISPLAY BY NAME e_desc_orig,
                           e_desc_rtv,
                           e_desc_cancel,
                           e_desc_liq,
                           e_desc_mov,
                           v_t_originacion_a,
                           v_t_reactivacion_a,
                           v_t_cancelacion_a,
                           v_t_liquidacion_a,
                           v_t_mov_no_val_a,
                           v_suma_acept,
                           v_t_originacion_r,
                           v_t_reactivacion_r,
                           v_t_liquidacion_r,
                           v_t_cancelacion_r,
                           v_t_mov_no_val_r,
                           v_suma_rchz,
                           v_t_glo_orig,
                           v_t_glo_reactiva,
                           v_t_glo_cancela,
                           v_t_glo_liquida,
                           v_t_glo_mov,
                           v_suma_reg,
                           v_prc_orig,
                           v_prc_reactiva,
                           v_prc_cancela,
                           v_prc_liquida,
                           v_prc_mov,
                           v_prc_final,
                           v_prc_orig_a,     
                           v_prc_reactiva_a, 
                           v_prc_cancela_a,
                           v_prc_liquida_a, 
                           v_prc_mov_a,
                           v_prc_fin_acept,
                           v_prc_orig_r,      
                           v_prc_reactiva_r,  
                           v_prc_cancela_r,   
                           v_prc_liquida_r,   
                           v_prc_mov_r,
                           v_prc_fin_rchz
  
         -- Misma cifras para deudores
         DISPLAY v_t_originacion_a  TO v_t_origen_deudor_a
         DISPLAY v_t_reactivacion_a TO v_t_reactiva_deudor_a
         DISPLAY v_t_cancelacion_a  TO v_t_cancela_deudor_a
         DISPLAY v_t_liquidacion_a  TO v_t_liquida_deudor_a
         DISPLAY v_t_mov_no_val_a   TO v_t_mov_deudor_a
         DISPLAY v_suma_acept       TO v_t_suma_deudor_a
         DISPLAY v_t_originacion_r  TO v_t_origen_deudor_r
         DISPLAY v_t_reactivacion_r TO v_t_reactiva_deudor_r
         DISPLAY v_t_liquidacion_r  TO v_t_liquida_deudor_r
         DISPLAY v_t_cancelacion_r  TO v_t_cancela_deudor_r
         DISPLAY v_t_mov_no_val_r   TO v_t_mov_deudor_r
         DISPLAY v_suma_rchz        TO v_t_suma_deudor_r

      ON ACTION Informe
         LET v_respuesta = fn_ventana_confirma("","¿Está seguro de generar el informe .PDF y el extractor recurrente informe?","")
            IF (v_respuesta = 0) THEN
               CALL fn_mensaje("","Se ha cancelado la operación","")
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
                                  "/AGRS12 ",
                                  g_usuario," ",
                                  v_pid," ",
                                  g_proceso_cod," ",
                                  g_opera_cod," ",
                                  v_fecha," ",
                                  " ' ' 1>",v_ruta_listados CLIPPED,
                                  "/nohup:",v_pid USING "&&&&&",":",
                                  g_proceso_cod   USING "&&&&&",":",
                                  g_opera_cod     USING "&&&&&" ," 2>&1 &"

               DISPLAY "v_s_comando: ",v_s_comando
               RUN v_s_comando 

               LET v_notifica = "Se ejecutó la generación del PDF y el extractor recurrente informe"," \n",
                                 "Verificar en el monitor de procesos el PID: ",v_pid USING "<<<<<<<<<"

               CALL fn_mensaje("",v_notifica,"")
               EXIT MENU 
            END IF
            
         
      ON ACTION CANCEL 
         EXIT MENU 
         
   END MENU  
   
CLOSE WINDOW vtn2
   
END FUNCTION 