######################################################################
#Modulo            => AGR                                            #
#Programa          => AGRL66                                         #
#Objetivo          => Transacción para solicitud de saldo remanente  #
#                     mediante la captura del NSS.                   #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 12/Enero/2018                                  #
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

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL66.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin
     INTO v_ruta_binaria
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   CALL seleccion_nss()
   
END MAIN 

FUNCTION seleccion_nss()

   DEFINE v_nss       CHAR(11)
   DEFINE v_ind_num   BOOLEAN 
   DEFINE v_bnd       SMALLINT
   DEFINE v_comando   STRING
   DEFINE v_mensaje   STRING 
   
   OPEN WINDOW w1 WITH FORM "AGRL661"
      INPUT BY NAME v_nss ATTRIBUTE(UNBUFFERED)

         ON ACTION ACCEPT 
            --Valida cadena NSS
            IF(v_nss IS NULL) THEN
               CALL fn_mensaje("","El nss no debe ser nulo","")
               NEXT FIELD v_nss
            END IF 
            IF(LENGTH(v_nss) < 11) THEN
               CALL fn_mensaje("","El nss debe ser de 11 dígitos","")
               NEXT FIELD v_nss
            END IF 
            LET v_ind_num = fn_es_numerico(v_nss)
            IF(v_ind_num = 1) THEN
               CALL fn_mensaje("","El nss debe ser numérico","")
               NEXT FIELD v_nss
            END IF

            #Inicializa el proceso
            -- Genera PID
            LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

            -- Inicializa proceso
            CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","AGRL66","",g_usuario) RETURNING v_bnd

            -- Actualiza operación como iniciada
            CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"AGRL66","",g_usuario) RETURNING v_bnd

            -- Ejecuta lanzado
            LET v_comando = "nohup fglrun ",v_ruta_binaria CLIPPED,"/AGRP48 ",
                                      g_usuario," ",
                                      g_pid," ",
                                      g_proceso_cod," ",
                                      g_opera_cod," ",
                                      1," ",
                                      v_nss,
                                      " ",
                                      " ",
                                      " ' ' 1>",
                                      v_ruta_listados CLIPPED,
                                      "/nohup:",g_pid USING "&&&&&",":",
                                      g_proceso_cod   USING "&&&&&",":",
                                      g_opera_cod     USING "&&&&&" ," 2>&1 &"

            RUN v_comando

            LET v_mensaje = "Se ejecutó el proceso de aplicación saldos remanentes por nss"," \n",
                            "Verificar en el monitor de procesos el PID: ",g_pid USING "<<<<<<<<<"

            CALL fn_mensaje("",v_mensaje,"")
            EXIT INPUT
            
         ON ACTION CANCEL 
            EXIT INPUT
                        
      END INPUT
      
   CLOSE WINDOW W1
   
END FUNCTION 

FUNCTION fn_es_numerico(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN

   LET p_cadena = p_cadena CLIPPED

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE
         LET indicador = 1
         EXIT FOR
      END IF
   END FOR

   RETURN indicador

END FUNCTION
