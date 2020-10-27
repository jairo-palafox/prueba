##################################################################################################
#Modulo       => AFI                                                                             #
#Programa     => AFIP18                                                                          #
#Objetivo     => Programa para carga masiva de NSS rojos por archivo                             #
#Autor        => Jose Eduardo Ventura Bonola                                                     #
#Fecha inicio => 18/JUNIO/2015                                                                   #
##################################################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario                CHAR(20)
   DEFINE p_titulo                 STRING
   DEFINE p_tipo_ejecucion         SMALLINT
   DEFINE v_archivo                STRING
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate
   DEFINE v_bandera                SMALLINT
   DEFINE i                        INTEGER    -- Variable para utilizar de contador
   DEFINE v_archivo_d              STRING     -- Variable para saber ruta y nombre donde se dejará el archivo
   DEFINE r_valida                 SMALLINT
   


END GLOBALS

MAIN

   LET p_usuario          =   ARG_VAL  (1)
   LET p_tipo_ejecucion   =   ARG_VAL  (2)
   LET p_titulo           =   ARG_VAL  (3)

-- Se asigna el título de la ventana
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

-- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".AFIP18.log")

   SELECT ruta_rescate
     INTO v_ruta_rescate
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   CALL fn_valida_archivo()

END MAIN

FUNCTION fn_valida_archivo()

   DEFINE v_archivo                STRING   -- Nombre de archivo seleccionado
   DEFINE v_pos                    INTEGER  -- Posición donde inicia la extensión ".txt"
   DEFINE cant                     INTEGER  -- Cantidad de caracteres que tiene el nombre del archivo
   DEFINE buf                      base.StringBuffer
   DEFINE v_extension              STRING
   DEFINE v_ruta_explorador        INTEGER
   
   CLOSE WINDOW SCREEN

   OPEN WINDOW AFIP18 WITH FORM "AFIP181"

   INPUT BY NAME  v_archivo ATTRIBUTES (UNBUFFERED)

   --AFTER INPUT
   ON ACTION ACCEPT

--********************************************************
--Se valida que el archivo tenga extensión correcta      *
--********************************************************
      IF v_archivo IS NULL THEN
         CALL fn_mensaje ("Archivo","Debe de seleccionar un archivo","information")
         NEXT FIELD v_archi
      END IF

      IF v_archivo.getIndexOf(" ", 1) THEN
         CALL fn_mensaje ("Archivo","Nombre de archivo no debe contener espacios","information")
         LET v_archivo = ""
         DISPLAY BY NAME v_archivo
         NEXT FIELD v_archivo
      END IF

      IF v_archivo IS NOT NULL THEN 
         --CALL fn_mensaje ("Validación de archivo" ,v_archivo,"information")
         LET buf = base.StringBuffer.create()
         CALL buf.append(v_archivo)

         LET cant         = LENGTH(v_archivo)    # cantidad de caracteres de nombre de archivo
         LET v_pos        = buf.getIndexof(".",1)# posición del punto para validar extensión
         LET v_pos = v_pos + 1
         LET v_extension  = buf.subString(v_pos,cant)
         LET v_ruta_explorador = buf.getIndexof("C:",1)

         --CALL fn_mensaje ("Transferencia Archivo", v_archivo,"information")

         IF v_ruta_explorador >= 1 THEN
            LET v_archivo = buf.subString(13,cant)
         END IF

        -- CALL fn_mensaje ("Transferencia Archivo", v_archivo,"information")

         LET v_bandera = 0

         IF v_extension = "ptco"  THEN
            LET v_bandera = 1
         END IF

         IF v_extension = "econ"  THEN
            LET v_bandera = 2
         END IF

         #IF v_extension = "ssva" THEN
            #LET v_bandera = 3
         #END IF

         IF v_bandera = 0 THEN
            CALL fn_mensaje ("Transferencia Archivo","Extensión del archivo no es correcta. \n El archivo debe tener extensión '.ptco' o '.econ' ","information")
            LET v_archivo = ""
            DISPLAY BY NAME v_archivo
            NEXT FIELD v_archivo
         END IF 

         IF (v_bandera = 1) OR
            (v_bandera = 2) THEN 
            #(v_bandera = 3) THEN
--***************************************************
--Se  recupera el archivo y se deja en ruta rescate *
--***************************************************
            LET v_archivo_d = v_ruta_rescate CLIPPED,"/",v_archivo
            TRY

            CALL FGL_GETFILE(v_archivo,v_archivo_d)
            CALL fn_mensaje ("Transferencia" ,"ARCHIVO TRANSFERIDO CORRRECTAMENTE","information")
            CALL fn_resp_info()
            EXIT INPUT

            --CONTINUE INPUT

            CATCH
               ERROR "NO SE PUDO TRANSFERIR"
               CONTINUE INPUT
            END TRY
         EXIT INPUT
      CLOSE WINDOW AFIP18

         END IF

      END IF

   EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT

   END INPUT
   CLOSE WINDOW AFIP18

END FUNCTION

FUNCTION fn_resp_info()

   DEFINE v_nom_resp               STRING

   LET v_nom_resp = v_ruta_rescate CLIPPED,"/","resp_nss_rojos",TODAY USING "DDMMYYYY",".unl"

   UNLOAD TO v_nom_resp SELECT * FROM afi_nss_rojo

   CALL fn_obtiene_datos()

END FUNCTION

FUNCTION fn_obtiene_datos()

   DEFINE s                        CHAR (21)  -- variable para leer lineas del archivo
   DEFINE ch1                      base.Channel 
   DEFINE tok                      base.StringTokenizer
   DEFINE buf1                     base.StringBuffer
   DEFINE cadena                   CHAR (11)  -- variable para rescatar lineas del archivo

   CALL fn_tablas_temporales()

   LET ch1 = base.Channel.create()
   CALL ch1.openFile(v_archivo_d,"r")
   LET buf1 = base.StringBuffer.create()

   LET i = 1
   WHILE TRUE
      LET s = ch1.readLine()
      LET tok = base.StringTokenizer.create(s," ")
      WHILE tok.hasMoreTokens()
         --DISPLAY "token:", tok.nextToken()
         LET cadena = tok.nextToken()
         CALL fn_valida_numero(cadena)

         {IF length (cadena) <> 11 THEN
            INSERT INTO safre_tmp:tmp_nss_rechazos VALUES (cadena,"NSS INCOMPLETO")
         END IF

         IF length(cadena) = 11 THEN
            CALL fn_valida_numero(cadena)
            IF r_valida = 1 THEN
               INSERT INTO safre_tmp:tmp_nss_rojo VALUES (cadena)
            END IF
         END IF}
      END WHILE

      IF ch1.isEof() THEN EXIT WHILE
      END IF
         LET i = i + 1
      END WHILE
   CALL ch1.close()
   CALL fn_procesa_info()

END FUNCTION

FUNCTION fn_procesa_info()

   DEFINE v_cat                    SMALLINT
   DEFINE v_edo                    SMALLINT
   DEFINE v_qry                    STRING
   DEFINE v_nss                    CHAR(11)
   DEFINE v_id                     DECIMAL(9,0)
   DEFINE v_cta_rechazo            INTEGER
   DEFINE v_cta_nvo                INTEGER
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   DEFINE v_ruta                   STRING
   DEFINE v_mensaje                STRING
   DEFINE v_nss_actualiza          CHAR(11)
   DEFINE v_cta_nss                INTEGER
   DEFINE v_cta                    INTEGER

   SELECT COUNT(*)
     INTO v_cta_nvo
     FROM safre_tmp:tmp_nss_rojo

   IF v_cta_nvo >= 1 THEN 

   IF v_bandera = 1 THEN
      LET v_cat = 1
      UPDATE afi_nss_rojo
         SET estado_rojo = 2
       WHERE categoria = v_cat 
   END IF

   IF v_bandera = 2 THEN
      LET v_cat = 2
      UPDATE afi_nss_rojo
         SET estado_rojo = 2
       WHERE categoria = v_cat
   END IF

   LET v_qry = "SELECT nss
                  FROM afi_nss_rojo
                 WHERE nss IN (SELECT nss
                    FROM safre_tmp:tmp_nss_rojo)"

   PREPARE prp_actualiza FROM v_qry
   DECLARE cur_actualiza CURSOR FOR prp_actualiza

   LET i = 1

   FOREACH cur_actualiza INTO v_nss_actualiza

      SELECT COUNT(*)
        INTO v_cta_nss
        FROM afi_nss_rojo
       WHERE nss = v_nss_actualiza

      IF v_cta_nss = 1 THEN
   
         UPDATE afi_nss_rojo
            SET estado_rojo = 1,
                categoria = v_cat,
                f_actualiza = TODAY
          WHERE nss = v_nss_actualiza
      END IF

      IF v_cta_nss > 1 THEN
         SELECT COUNT(*)
           INTO v_cta
           FROM afi_nss_rojo
          WHERE nss = v_nss_actualiza
            AND estado_rojo = 1

         IF v_cta >= 1 THEN
            UPDATE afi_nss_rojo
               SET estado_rojo = 1,
                   categoria = v_cat,
                   f_actualiza = TODAY
             WHERE nss = v_nss_actualiza
               AND estado_rojo = 1
            
         --END IF
         ELSE 
         --IF (v_cta < 1) OR (v_cta IS NULL) THEN
            UPDATE afi_nss_rojo
               SET estado_rojo = 1,
                   categoria = v_cat,
                   f_actualiza = TODAY
             WHERE id_rojo = (SELECT MAX (id_rojo)
                                    FROM afi_nss_rojo
                                   WHERE nss = v_nss_actualiza)
         END IF

      END IF
         LET i = i + 1
   END FOREACH
   LET v_edo = 1

   LET v_qry = "select nss, id_derechohabiente
                  from afi_derechohabiente
                 where nss in (
                select nss from safre_tmp:tmp_nss_rojo
                 where nss not in (select nss from afi_nss_rojo))"
        
   PREPARE prp_alta FROM v_qry
   DECLARE cur_alta CURSOR FOR prp_alta

   LET i = 1

   FOREACH cur_alta INTO v_nss,v_id

   IF (v_nss AND v_id ) IS NOT NULL THEN

      INSERT INTO afi_nss_rojo VALUES ( seq_afi_rojo.NEXTVAL,
                                        v_nss,
                                        v_id,
                                        v_cat,
                                        v_edo,
                                        TODAY,
                                        p_usuario)
    END IF
    
      LET i = i + 1
    

   END FOREACH
   SELECT COUNT(*)
     INTO v_cta_rechazo
     FROM safre_tmp:tmp_nss_rechazos

   IF v_cta_rechazo >= 1 THEN

      SELECT ruta_envio
        INTO v_ruta_envio
        FROM seg_modulo
       WHERE modulo_cod = "afi"

      LET v_ruta = v_ruta_envio CLIPPED,"/","rechazos_nss_rojos.rech"
      LET v_mensaje ="La carga de nss validos se relaizó de forma correcta y existen nss rechados, \n el archivo de rechazos fue generado en: \n",v_ruta
      CALL fn_mensaje("Atención",v_mensaje,"stop")
      --CALL fn_mensaje("Atención","Existen nss rechazados (verificar archivo de rechazos) y \n la carga de nss validos se realizó de forma correcta", "stop")

      UNLOAD TO v_ruta SELECT * FROM safre_tmp:tmp_nss_rechazos
   ELSE
      CALL fn_mensaje("Atención","Carga de nss realizada correctamente", "stop")
   END IF

   ELSE 
      CALL fn_mensaje("Atención","No se encontraron registros validos para alta en archivo", "stop")
   END IF

END FUNCTION

FUNCTION fn_tablas_temporales()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_nss_rojo
      DROP TABLE tmp_nss_rechazos

   WHENEVER ERROR STOP

   CREATE TABLE tmp_nss_rojo ( nss CHAR(11))
   CREATE TABLE tmp_nss_rechazos (nss CHAR(11),motivo CHAR(100))

DATABASE safre_viv

END FUNCTION

FUNCTION fn_valida_numero(cadena)

   DEFINE v_funcion                STRING
   DEFINE v_cadena                 CHAR (11)
   DEFINE a                        INTEGER
   DEFINE v_cuenta                 SMALLINT
   DEFINE cadena                   CHAR (11)

   LET v_funcion = "EXECUTE FUNCTION fn_es_numero(?)"

   PREPARE prp_es_numero FROM v_funcion

   FOR a = 1 TO length(cadena)
      LET v_cadena = cadena[a,a]
      EXECUTE prp_es_numero USING v_cadena INTO r_valida
      IF r_valida = 0 THEN
         EXIT FOR
      END IF
   END FOR


   IF r_valida = 0 THEN
         INSERT INTO safre_tmp:tmp_nss_rechazos VALUES (cadena,"NSS CONTIENE UN CARACTER NO VALIDO")
   END IF

   IF (r_valida = 1) AND (length(cadena) <> 11) THEN
      INSERT INTO safre_tmp:tmp_nss_rechazos VALUES (cadena,"NSS INCOMPLETO")
   END IF

   IF (r_valida = 1) AND (length(cadena) = 11) THEN
      SELECT COUNT(*)
        INTO v_cuenta
        FROM afi_derechohabiente
       WHERE nss = cadena

      IF v_cuenta >= 1 THEN
         INSERT INTO safre_tmp:tmp_nss_rojo VALUES (cadena)
      ELSE
         IF (v_cuenta IS NULL) OR (v_cuenta < 1) THEN
            INSERT INTO safre_tmp:tmp_nss_rechazos VALUES (cadena,"NSS NO EXISTE EN LA BASE DE DATOS" )
         END IF
      END IF
   END IF

END FUNCTION