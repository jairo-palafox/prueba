######################################################################
#Modulo            => AGR                                            #
#Programa          => AGRL67                                         #
#Objetivo          => Transacción para solicitud de saldo remanente  #
#                     mediante por bloques de nss.                   #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 19/Enero/2018                                  #
######################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_ruta_binaria      CHAR(40)
   DEFINE v_ruta_rescate      CHAR(40)
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

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL67.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin,ruta_rescate
     INTO v_ruta_binaria,v_ruta_rescate
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   CALL seleccion_bloque()
   
END MAIN 

FUNCTION seleccion_bloque()

   DEFINE v_archivo      STRING
   DEFINE bnd_transfer   SMALLINT
   DEFINE buf            base.StringBuffer 
   DEFINE v_long_arh     INTEGER
   DEFINE v_pos_ext      SMALLINT
   DEFINE v_extension    STRING 
   DEFINE v_pos_arh      INTEGER
   DEFINE v_ruta_archivo STRING 
   DEFINE v_bnd          SMALLINT 
   DEFINE v_comando      STRING
   DEFINE v_mensaje      STRING 


   OPEN WINDOW w1 WITH FORM "AGRL671"
      INPUT BY NAME v_archivo ATTRIBUTE(UNBUFFERED)

         ON ACTION ACCEPT 
            LET bnd_transfer = 0  -- El archivo no ha transferido al servidor
            IF(v_archivo IS NULL) THEN 
                CALL fn_mensaje("","Selecciona el archivo a cargar","about")
               NEXT FIELD v_archivo
            END IF 

            -- Verifica si hay un espacio en el nombre del archivo
            IF (v_archivo.getIndexOf(" ",1)) THEN
               CALL fn_mensaje("","El archivo no debe contener espacios","")
               LET v_archivo = NULL 
               NEXT FIELD v_archivo 
            END IF

            IF (v_archivo IS NOT NULL) THEN
               LET buf = base.StringBuffer.create()   # Se crea objeto StringBuffer 
               CALL buf.append(v_archivo)

               -- Longitud del archivo
               LET v_long_arh = LENGTH(v_archivo)

               -- Posición donde se encuentra el '.' del archivo
               LET v_pos_ext = buf.getIndexOf(".",1)
               LET v_pos_ext   = v_pos_ext + 1                       # Incrementamos 1 para obtener solo el nombre de la extensión
               LET v_extension = buf.subString(v_pos_ext,v_long_arh) # Obtiene nombre de la extensión
               LET v_pos_arh   = buf.getIndexOf("C:",1)
               
               -- Obtiene sólo el nombre del archivo, sin la ruta
               IF(v_pos_arh >= 1) THEN
                  LET v_archivo = buf.subString(13,v_long_arh) 
               END IF 

               IF(v_extension <> "srbnss") THEN
                  CALL fn_mensaje ("","La extensión del archivo no es correcta.\nEl archivo debe tener extensión .srbnss","")
                  LET v_archivo = NULL 
                  NEXT FIELD v_archivo
               ELSE 
                  --*************************************************************************
                  --Se recupera el archivo cargado y se deja en la ruta rescate del servidor
                  --para trabajarlo desde ahí.
                  --*************************************************************************
                  LET v_ruta_archivo = v_ruta_rescate CLIPPED,"/",v_archivo

                  TRY 
                     CALL FGL_GETFILE(v_archivo,v_ruta_archivo)
                     LET bnd_transfer = 1 --Archivo transferido correctamente
                  CATCH
                     CALL fn_mensaje("","No se pudo realizar la carga del archivo","")
                     CONTINUE INPUT  
                  END TRY 

                  IF (bnd_transfer = 1) THEN
                     #Ejecuta lanzado
                     -- Genera PID
                     LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

                     -- Inicializa proceso
                     CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","AGRL67",v_archivo,g_usuario) RETURNING v_bnd

                     -- Actualiza operación como iniciada
                     CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"AGRL67",v_archivo,g_usuario) RETURNING v_bnd

                     -- Ejecuta lanzado
                     LET v_comando = "nohup fglrun ",v_ruta_binaria CLIPPED,"/AGRP48 ",
                                      g_usuario," ",
                                      g_pid," ",
                                      g_proceso_cod," ",
                                      g_opera_cod," ",
                                      2," ",
                                      "' '"," ",
                                      v_ruta_archivo CLIPPED," ",
                                      v_archivo CLIPPED," ",
                                      " ' ' 1>",
                                      v_ruta_listados CLIPPED,
                                      "/nohup:",g_pid USING "&&&&&",":",
                                      g_proceso_cod   USING "&&&&&",":",
                                      g_opera_cod     USING "&&&&&" ," 2>&1 &"

                     RUN v_comando

                     LET v_mensaje = "Se ejecutó el proceso de aplicación saldos remanentes por bloques de nss"," \n",
                                     "Verificar en el monitor de procesos el PID: ",g_pid USING "<<<<<<<<<"

                     CALL fn_mensaje("",v_mensaje,"")
                     EXIT INPUT
                  ELSE 
                     CALL fn_mensaje("","El archivo no pudo ser transferido","")
                  END IF
               END IF 
            END IF 
            
         ON ACTION CANCEL
            EXIT INPUT 

      END INPUT 
   CLOSE WINDOW w1
   
END FUNCTION 