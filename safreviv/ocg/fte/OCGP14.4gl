###########################################################################
#Modulo            => OCG                                                 #
#Programa          => OCGP14                                              #
#Objetivo          => Programa para a actualizaci�n del n�mero de control #
#                     interno de un cr�dito vigente 43BIS mediante la     #
#                     la carga de un archivo.                             # 
#Autor             => Emilio Abarca S�nchez                               #
#Fecha inicio      => 06 OCTUBRE 2016                                     #
###########################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE p_usuario                CHAR(20)
   DEFINE p_titulo                 STRING
   DEFINE p_tipo_ejecucion         SMALLINT
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate
   DEFINE v_ruta_resct_arch        STRING                     --Ruta donde de alojar� el archivo rescatado de la PC
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio --Ruta donde se generar� el archivo de rechazos
   
END GLOBALS 

MAIN
   LET p_usuario          =   ARG_VAL  (1)
   LET p_tipo_ejecucion   =   ARG_VAL  (2)
   LET p_titulo           =   ARG_VAL  (3)

   -- Se asigna el t�tulo de la ventana
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGP14.log")

   SELECT ruta_rescate
     INTO v_ruta_rescate
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   SELECT ruta_envio
      INTO v_ruta_envio
      FROM seg_modulo
     WHERE modulo_cod = 'ocg' 

   CLOSE WINDOW SCREEN 
   CALL fn_carga_archivo()
END MAIN 

FUNCTION fn_carga_archivo()

   DEFINE v_archivo         STRING            --Nombre del archivo
   DEFINE buf               base.StringBuffer --Variable tipo base.StringBuffer
   DEFINE v_cantidad        INTEGER           --Cantidad de caracteres en el nombre del archivo
   DEFINE v_posicion        INTEGER           --Posici�n donde encontrar� "." de la extensi�n del archivo
   DEFINE v_extension       STRING            --Nombre de la extensi�n
   DEFINE v_ruta_exp        INTEGER 
   DEFINE v_bandera         SMALLINT 
   
   OPEN WINDOW OCGP14 WITH FORM "OCGP141"
      INPUT BY NAME v_archivo ATTRIBUTE(UNBUFFERED)
      
         ON ACTION ACCEPT 
            IF (v_archivo IS NULL) THEN 
               CALL fn_mensaje("Informaci�n","Debe seleccionar un archivo","about")
               NEXT FIELD v_archivo
            END IF 

            IF (v_archivo.getIndexOf(" ",1)) THEN
               CALL fn_mensaje("Informaci�n","El archivo no debe contener espacios","about")
               LET v_archivo = NULL 
               NEXT FIELD v_archivo 
            END IF

            IF (v_archivo IS NOT NULL) THEN
               -- CALL fn_mensaje("prueba",v_archivo,"about")
                              
               --Valida extensi�n del archivo cargado
                LET buf = base.StringBuffer.create()   # Se crea objeto StringBuffer 
                CALL buf.append(v_archivo)

                LET v_cantidad = LENGTH(v_archivo)     # Cantidad de caracteres en el nombre del archivo
                LET v_posicion = buf.getIndexOf(".",1) # Posici�n donde se encuenta el "."
                
                LET v_posicion = v_posicion + 1 # Incrementamos 1 para obtener solo el nombre de la extensi�n
                LET v_extension = buf.subString(v_posicion,v_cantidad) # Obtiene nombre de la extensi�n

                LET v_ruta_exp = buf.getIndexOf("C:",1)
                        
                IF (v_ruta_exp >= 1) THEN
                   LET v_archivo = buf.subString(13,v_cantidad) 
                END IF 

                LET v_bandera = 0

                IF (v_extension = "txt") THEN 
                   LET v_bandera = 1
                END IF 

                IF (v_bandera = 0) THEN
                   CALL fn_mensaje ("Informaci�n","La extensi�n del archivo no es correcta.\n El archivo debe tener extensi�n '.txt'","about")
                   LET v_archivo = NULL 
                   NEXT FIELD v_archivo 
                END IF 

                IF (v_bandera = 1) THEN
                --*************************************************************************
                --Se recupera el archivo cargado y se deja en la ruta rescate del servidor
                --para trabajarlo desde ah�.
                --*************************************************************************
                   LET v_ruta_resct_arch = v_ruta_rescate CLIPPED,"/",v_archivo
                   
                   TRY 
                   
                      CALL FGL_GETFILE(v_archivo,v_ruta_resct_arch)
                      CALL fn_mensaje ("Informaci�n","ARCHIVO TRANSFERIDO CORRECTAMENTE","about")
                      CALL fn_obtiene_datos()
                   CATCH
                      ERROR "NO SE PUDO TRANSFERIR EL ARCHIVO"
                      CONTINUE INPUT  
                   END TRY 
                 --  EXIT INPUT 
                END IF 
            END IF 
            
         ON ACTION CANCEL 
            EXIT INPUT 
      END INPUT 
   CLOSE WINDOW OCGP14
END FUNCTION 


FUNCTION fn_obtiene_datos()

   DEFINE v_line            CHAR(32)     --Variable para almacenar las l�neas del archivo
   DEFINE ch                base.channel
   DEFINE v_pos_nss         CHAR(11)
   DEFINE v_pos_ef          CHAR(3)
   DEFINE v_pos_num_ctr_int CHAR(18)

   CALL fn_crea_tablas_tmp()

   LET ch = base.Channel.create() # Creamos un objeto de la clase channel
   CALL ch.openFile(v_ruta_resct_arch,"r")

   WHILE TRUE
      LET v_line = ch.readLine()  # Lee la l�nea completa del archivo
      
      IF(ch.isEof()) THEN 
         EXIT WHILE
      ELSE
         # Recupera la informaci�n del archivo
         LET v_pos_nss = v_line[1,11]  
         LET v_pos_ef  = v_line[12,14] 
         LET v_pos_num_ctr_int = v_line[15,32]

         # inserta la cadena en la tabla temporal
         INSERT INTO safre_tmp:tmp_num_ctr_int_ef 
            VALUES(v_pos_nss,v_pos_ef,v_pos_num_ctr_int);
      END IF
   END WHILE

   CALL ch.close() # Cierra el archivo leido

   # Se valida la informaci�n de la tabla temporal tmp_num_ctr_int_ef
   CALL fn_procesa_informacion()
END FUNCTION 

FUNCTION fn_crea_tablas_tmp()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_num_ctr_int_ef
      DROP TABLE tmp_reg_rechazados

   WHENEVER ERROR STOP 
      CREATE TABLE tmp_num_ctr_int_ef(nss CHAR (11),
                                      cve_ent_financiera CHAR(3),
                                      num_ctr_int_ef CHAR(18))
      CREATE TABLE tmp_reg_rechazados(nss CHAR(11),
                                      cve_ent_financiera CHAR(3),
                                      num_ctr_int_ef CHAR(18),
                                      motivo CHAR(100))

DATABASE safre_viv

END FUNCTION

FUNCTION fn_procesa_informacion()
 
   DEFINE v_count_tmp        INTEGER
   DEFINE v_nss              CHAR(11)
   DEFINE v_cve_ef           CHAR(3)
   DEFINE v_nvo_num_ctr_int  CHAR(18)
   DEFINE v_ind_numerico     INTEGER
   DEFINE ind_actualiza      BOOLEAN
   DEFINE v_rch_desc         CHAR(100)
   DEFINE v_query            STRING 
   DEFINE v_return           SMALLINT
   DEFINE v_ind_cve_ent      BOOLEAN 
   
   SELECT COUNT(*) 
     INTO v_count_tmp
     FROM safre_tmp:tmp_num_ctr_int_ef;

   #Si la tmp tiene registros inicia la recuperaci�n 
   IF(v_count_tmp >= 1) THEN

      # Eliminamos los registros de la tmp en caso de que sean Nulos
      DELETE FROM safre_tmp:tmp_num_ctr_int_ef
         WHERE nss = ""
         AND cve_ent_financiera = ""
         AND num_ctr_int_ef = "";

      DECLARE cur_recupera CURSOR FOR 
         SELECT nss,
                cve_ent_financiera,
                num_ctr_int_ef
         FROM safre_tmp:tmp_num_ctr_int_ef

      FOREACH cur_recupera INTO v_nss,
                                v_cve_ef,
                                v_nvo_num_ctr_int
      
         LET ind_actualiza = 0
            
         #Inicia la validaci�n del nss  por cada registro
         IF(LENGTH (v_nss) < 11) THEN
         
            LET ind_actualiza = 1 # Indicador rechazo
            
            SELECT rechazo_desc
              INTO v_rch_desc
              FROM cat_ocg_rechazos
            WHERE id_cod_rechazo = 1; #Trabajador sin credito en ef
            
            INSERT INTO safre_tmp:tmp_reg_rechazados
               VALUES(v_nss,v_cve_ef,v_nvo_num_ctr_int,v_rch_desc);
         ELSE 
            LET v_ind_numerico = fn_es_numerico(v_nss)
            
            IF (v_ind_numerico = 1) THEN
            
               LET ind_actualiza = 1 ##Indicador rechazo

               SELECT rechazo_desc
                 INTO v_rch_desc
                 FROM cat_ocg_rechazos
                WHERE id_cod_rechazo = 1; #Trabajador sin cr�dito en ef
                
               INSERT INTO safre_tmp:tmp_reg_rechazados
                  VALUES(v_nss,v_cve_ef,v_nvo_num_ctr_int,v_rch_desc);
            END IF 
         END IF 

         # Valida la cve ef que sea numerica
         LET v_ind_cve_ent = fn_es_numerico(v_cve_ef)

         IF (v_ind_cve_ent = 1) THEN 
            LET ind_actualiza = 1 #Indicador rechazo

            SELECT rechazo_desc
               INTO v_rch_desc
               FROM cat_ocg_rechazos
              WHERE id_cod_rechazo = 6   -- Entidad financiera err�nea

            INSERT INTO safre_tmp:tmp_reg_rechazados
               VALUES(v_nss,v_cve_ef,v_nvo_num_ctr_int,v_rch_desc);
         END IF 

         # Valida que el nvo n�mero de control interno no sea Nulo
            IF(v_nvo_num_ctr_int = "                  " OR v_nvo_num_ctr_int IS NULL) THEN
            
               LET ind_actualiza = 1 # Indicador rechazo
               
                SELECT rechazo_desc
                 INTO v_rch_desc
                 FROM cat_ocg_rechazos
                WHERE id_cod_rechazo = 3; #El n�mero de control interno es NUlo
                
               INSERT INTO safre_tmp:tmp_reg_rechazados
                  VALUES(v_nss,v_cve_ef,v_nvo_num_ctr_int,v_rch_desc);
            END IF
            
         # Limpia tabla tmp los registros err�neos 
         IF(ind_actualiza = 1) THEN
            DELETE FROM safre_tmp:tmp_num_ctr_int_ef
               WHERE nss = v_nss
                 AND cve_ent_financiera = v_cve_ef
                 AND num_ctr_int_ef = v_nvo_num_ctr_int;
         END IF 
      END FOREACH

      # Se llama el SP para realizar la actualizaci�n del n�mero
      # de control interno del derechohabiente
      
      LET v_query ="EXECUTE FUNCTION fn_ocg_act_num_ctrl_interno(?)"
      PREPARE prp_exe_act FROM v_query 
      EXECUTE prp_exe_act USING p_usuario
                          INTO  v_return

      IF(v_return = 0) THEN
         CALL fn_mensaje("Informaci�n","La actualizaci�n se ha ejecutado correctamente","about")
      ELSE 
         CALL fn_mensaje("Informaci�n","No se pudo realizar la actualizaci�n","stop")
      END IF 

      --Se genera el archivo salida de los rechazos
      CALL genera_arch_rechazos()
      
   END IF 
END FUNCTION

FUNCTION fn_es_numerico(p_cadena)

   DEFINE p_cadena   STRING 
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN 

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE 
         LET indicador = 1
         IF(indicador == 1) THEN
            EXIT FOR 
         END IF 
      END IF 
   END FOR 

   RETURN indicador 

END FUNCTION 

FUNCTION genera_arch_rechazos()

   DEFINE v_arch_salida   base.Channel
   DEFINE v_arch_rch      STRING
   DEFINE v_query_rch     STRING
   DEFINE nss_rch         CHAR(11)
   DEFINE cve_ef_rch      CHAR(3)
   DEFINE num_ctr_int_rch CHAR(18)
   DEFINE motivo_rch      CHAR(100)
   DEFINE v_cadena_cmpt   STRING
   DEFINE v_mensaje       STRING
   DEFINE k               INTEGER 

   LET v_arch_rch = v_ruta_envio CLIPPED,"/rch_control_interno.ocg"
   
   LET v_arch_salida = base.Channel.create()
   CALL v_Arch_salida.openFile(v_arch_rch,"w")

   LET v_query_rch = "SELECT *
                         FROM safre_tmp:tmp_reg_rechazados"

   PREPARE prp_query FROM v_query_rch
   DECLARE cur_rch CURSOR FOR prp_query

   LET k = 0
   
   FOREACH cur_rch INTO nss_rch,
                        cve_ef_rch, 
                        num_ctr_int_rch, 
                        motivo_rch
   
      LET v_cadena_cmpt = nss_rch CLIPPED,"|", cve_ef_rch CLIPPED,"|",num_ctr_int_rch CLIPPED,"|", motivo_rch CLIPPED,"|"
      
      --Escribe en el archivo de salida
      CALL v_arch_salida.writeLine(v_cadena_cmpt)

      LET k = k + 1 
      
   END FOREACH 

   IF ( k >= 1) THEN 
      LET v_mensaje = "Los registros rechazados se grabaron en el archivo rch_control_interno.ocg \n de la ruta /safreviv_int/ocg/envio"
      CALL fn_mensaje("",v_mensaje,"")
   ELSE 
      CALL fn_mensaje("","No hubo registros rechazados","")
   END IF 
 
END FUNCTION 


