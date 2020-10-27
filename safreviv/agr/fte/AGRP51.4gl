##############################################################################
#Modulo            => AGR                                                    #
#Programa          => AGRP51                                                 #
#Objetivo          => Transacción para la carga de archivo actualización de  #
#                  => marca para solicitud de saldo a Procesar desde la PC.  #
#Autor             => Emilio Abarca, EFP                                     #
#Fecha inicio      => 24/Agosto/2018                                         #
##############################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_ruta_rescate      CHAR(40)

END GLOBALS 

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP51.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_rescate
     INTO v_ruta_rescate
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   CALL carga_archivo_SSPr()
   
END MAIN 

FUNCTION carga_archivo_SSPr()

   DEFINE v_archivo      STRING
   DEFINE buf            base.StringBuffer
   DEFINE v_long_arh     INTEGER 
   DEFINE v_pos_ext      SMALLINT 
   DEFINE v_extension    STRING 
   DEFINE v_pos_arh      INTEGER
   DEFINE v_ruta_archivo STRING 
   DEFINE bnd_transfer   SMALLINT
   DEFINE v_mensaje      STRING 
   
   OPEN WINDOW vtn1 WITH FORM "AGRP511"
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

            IF(v_archivo IS NOT NULL) THEN
               -- valida extensión del archivo
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

               IF(v_extension <> "ctl") THEN
                  CALL fn_mensaje ("","La extensión del archivo no es correcta.\nEl archivo debe tener extensión .ctl ","")
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
                     CALL fn_mensaje("","No se pudo realizar la transferencia del archivo al servidor","")
                     CONTINUE INPUT  
                  END TRY 

                  IF(bnd_transfer = 1) THEN
                     LET v_mensaje = "El archivo: ",v_archivo, " se ha transferido al servidor correctamente"
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
   CLOSE WINDOW vtn1
END FUNCTION 