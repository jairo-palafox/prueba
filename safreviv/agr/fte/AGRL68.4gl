######################################################################
#Modulo            => AGR                                            #
#Programa          => AGRL68                                         #
#Objetivo          => Transacción para solicitud de saldo remanente  #
#                     mediante por parámetros.                       #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 16/Enero/2018                                  #
######################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_ruta_binaria      CHAR(40)
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE g_proceso_cod       SMALLINT  
   DEFINE g_opera_cod         SMALLINT
   DEFINE g_pid               DECIMAL(9,0) 
END GLOBALS

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)
   LET g_proceso_cod      = 346
   LET g_opera_cod        = 1

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL68.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin
     INTO v_ruta_binaria
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   CALL seleccion_parametros()
   
END MAIN 

FUNCTION seleccion_parametros()

   DEFINE v_monto_rema_ini  DECIMAL(13,2)
   DEFINE v_monto_rema_fin  DECIMAL(13,2)
   DEFINE v_f_orig_ini      DATE
   DEFINE v_f_orig_fin      DATE
   DEFINE v_tpo_cre_1       SMALLINT
   DEFINE v_tpo_cre_3       SMALLINT
   DEFINE v_tpo_cre_4       SMALLINT 
   DEFINE v_tpo_cre_5       SMALLINT
   DEFINE v_tpo_cre_10      SMALLINT
   DEFINE v_tpo_cre_11      SMALLINT
   DEFINE v_tpo_cre_15      SMALLINT
   DEFINE v_bnd             SMALLINT 
   DEFINE v_comando         STRING
   DEFINE v_mensaje         STRING 

   --Inicializa parámetros de entrada
   LET v_monto_rema_ini = NULL
   LET v_monto_rema_fin = NULL
   LET v_f_orig_ini     = NULL
   LET v_f_orig_fin     = NULL
   LET v_tpo_cre_1      = 0
   LET v_tpo_cre_3      = 0
   LET v_tpo_cre_4      = 0
   LET v_tpo_cre_5      = 0
   LET v_tpo_cre_10     = 0
   LET v_tpo_cre_11     = 0
   LET v_tpo_cre_15     = 0
   
   OPEN WINDOW w1 WITH FORM "AGRL681"
      INPUT BY NAME v_monto_rema_ini,v_monto_rema_fin,v_f_orig_ini,v_f_orig_fin,
                     v_tpo_cre_1,v_tpo_cre_3,v_tpo_cre_4,v_tpo_cre_5,v_tpo_cre_10,
                     v_tpo_cre_11,v_tpo_cre_15 ATTRIBUTE(UNBUFFERED,WITHOUT DEFAULTS)

         ON ACTION ACCEPT 
            #Verifica que se haya ingresado parámetros de búsqueda
            IF(v_monto_rema_ini IS NULL) AND
              (v_monto_rema_fin IS NULL) AND 
              (v_f_orig_ini IS NULL) AND 
              (v_f_orig_fin IS NULL) AND 
              (v_tpo_cre_1  == 0) AND
              (v_tpo_cre_3  == 0) AND 
              (v_tpo_cre_4  == 0) AND 
              (v_tpo_cre_5  == 0) AND 
              (v_tpo_cre_10 == 0) AND 
              (v_tpo_cre_11 == 0) AND 
              (v_tpo_cre_15 == 0) THEN 
               CALL fn_mensaje("","Ingresa un parámetro de búsqueda","") 
               NEXT FIELD v_monto_rema_ini
            END IF 

             IF (v_monto_rema_ini < 0) THEN
               CALL fn_mensaje("","El monto inicial debe ser mayor a cero","")
               NEXT FIELD v_monto_rema_ini
            END IF
            IF (v_monto_rema_fin < 0) THEN
               CALL fn_mensaje("","El monto final debe ser mayor a cero","")
               NEXT FIELD v_monto_rema_fin
            END IF 

            #En caso de seleccionar un monto inicial y monto final, valida que el 
            #monto final no sea menor al monto inicial
            IF (v_monto_rema_ini IS NOT NULL) AND 
               (v_monto_rema_fin IS NOT NULL) THEN 
              IF (v_monto_rema_fin < v_monto_rema_ini) THEN
                 CALL fn_mensaje("","El monto final debe ser mayor al monto inicial","")
                 NEXT FIELD v_monto_rema_fin
              END IF 
            END IF

            IF (v_monto_rema_ini IS NOT NULL) OR 
               (v_monto_rema_fin IS NOT NULL) THEN  
               IF (v_f_orig_ini IS NULL) AND 
                  (v_f_orig_fin IS NULL) AND 
                  (v_tpo_cre_1  == 0) AND
                  (v_tpo_cre_3  == 0) AND 
                  (v_tpo_cre_4  == 0) AND 
                  (v_tpo_cre_5  == 0) AND 
                  (v_tpo_cre_10 == 0) AND 
                  (v_tpo_cre_11 == 0) AND 
                  (v_tpo_cre_15 == 0) THEN
                  CALL fn_mensaje("","Ingresa la fecha de originación o el tipo de crédito","")
                  NEXT FIELD v_f_orig_ini
               END IF 
            END IF 
            
            #En caso de seleccionar un rango de fechas, valida que la fecha final no sea 
            #menor a la fecha inicial
            IF (v_f_orig_ini IS NOT NULL) AND 
               (v_f_orig_fin IS NOT NULL) THEN
               IF (v_f_orig_fin < v_f_orig_ini) THEN
                  CALL fn_mensaje("","La fecha final debe ser mayor a la fecha inicial","")
                  NEXT FIELD v_f_orig_fin
               END IF 
            END IF 

            #crea temporal
            CALL crea_temporal()
            
            #Realiza inserciónes en temporales
            INSERT INTO safre_tmp:tmp_parametro_remanente
               VALUES (v_monto_rema_ini,
                        v_monto_rema_fin,
                        v_f_orig_ini,    
                        v_f_orig_fin); 

            --crédito tradicional
            IF( v_tpo_cre_1  == 1) THEN
               INSERT INTO safre_tmp:tmp_parametro_credito
               VALUES(1)
            END IF 
            --crédito cofinanciado sin cobranza
            IF( v_tpo_cre_3  == 1) THEN
               INSERT INTO safre_tmp:tmp_parametro_credito
               VALUES(3)
            END IF 
            --crédito infonavit total
            IF( v_tpo_cre_4  == 1) THEN
               INSERT INTO safre_tmp:tmp_parametro_credito
               VALUES(4)
            END IF
            --crédito cofinavit 08
            IF( v_tpo_cre_5  == 1) THEN
               INSERT INTO safre_tmp:tmp_parametro_credito
               VALUES(5)
            END IF 
            --crédito cofinavit AG
            IF( v_tpo_cre_10  == 1) THEN
               INSERT INTO safre_tmp:tmp_parametro_credito
               VALUES(10)
            END IF 
            --crédito infonavit total AG
            IF( v_tpo_cre_11  == 1) THEN
               INSERT INTO safre_tmp:tmp_parametro_credito
               VALUES(11)
            END IF
            --crédito segundo crédito 
            IF( v_tpo_cre_15  == 1) THEN
               INSERT INTO safre_tmp:tmp_parametro_credito
               VALUES(15)
            END IF 
            
            
            #Inicializa el proceso
            -- Genera PID
            LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

            -- Inicializa proceso
            CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","AGRL68","",g_usuario) RETURNING v_bnd

            -- Actualiza operación como iniciada
            CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"AGRL68","",g_usuario) RETURNING v_bnd

            -- Ejecuta lanzado
            LET v_comando = "nohup fglrun ",v_ruta_binaria CLIPPED,"/AGRP48 ",
                                      g_usuario," ",
                                      g_pid," ",
                                      g_proceso_cod," ",
                                      g_opera_cod," ",
                                      3," ",
                                      " ",
                                      " ",
                                      " ",
                                      " ' ' 1>",
                                      v_ruta_listados CLIPPED,
                                      "/nohup:",g_pid USING "&&&&&",":",
                                      g_proceso_cod   USING "&&&&&",":",
                                      g_opera_cod     USING "&&&&&" ," 2>&1 &"

            RUN v_comando

            LET v_mensaje = "Se ejecutó el proceso de aplicación saldos remanentes por parámetros"," \n",
                            "Verificar en el monitor de procesos el PID: ",g_pid USING "<<<<<<<<<"

            CALL fn_mensaje("",v_mensaje,"")
            EXIT INPUT
          
         ON ACTION CANCEL 
            EXIT INPUT
            
      END INPUT
      
   CLOSE WINDOW W1
   
END FUNCTION 

FUNCTION crea_temporal()

   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_parametro_remanente
      DROP TABLE tmp_parametro_credito
   WHENEVER ERROR STOP 
      CREATE TABLE tmp_parametro_remanente(
                       monto_rema_ini DECIMAL(13,2),
                       monto_rema_fin DECIMAL(13,2),
                       f_orig_ini     DATE,
                       f_orig_fin     DATE);

      CREATE TABLE tmp_parametro_credito(
                       tpo_credito    SMALLINT);

   DATABASE safre_viv
   
END FUNCTION  