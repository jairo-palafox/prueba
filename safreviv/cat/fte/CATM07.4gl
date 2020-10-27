##########################################################################
#Modulo            => CAT                                                #
#Programa          => CATM07                                             #
#Objetivo          => Mantenimiento a catálogo "afi_nss_rojo"            #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => Noviembre 2014                                     #
#Autor modifica    => Emilio Abarca                                      #
#Fecha modifica    => Julio 2017
##########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_titulo                 STRING    -- Variable para título de ventana
   DEFINE g_usuario                CHAR(20)  -- Variable para recuperar nombre de usuario
   DEFINE p_tipo_ejecucion         SMALLINT  -- Forma como ejecutará el programa
--********************************************************************
-- Variables para mostrar en display de informacion para alta o baja
   DEFINE v_nombre                 CHAR(60)
   DEFINE v_paterno                CHAR(60)
   DEFINE v_materno                CHAR(60)
   DEFINE v_nss                    CHAR(11)
   DEFINE v_estado                 CHAR(60)
   DEFINE v_curp                   CHAR(60)
   DEFINE v_rfc                    CHAR(60)
   DEFINE v_query                  STRING
   DEFINE a                        SMALLINT
   DEFINE v_categoria              CHAR(60)  -- Variable para display de información general
   DEFINE cb                       ui.ComboBox 
   DEFINE v_archivo_d              STRING    -- Variable para saber ruta y nombre donde se dejará el archivo
   
--****************************************************************************
   DEFINE arr_general              DYNAMIC ARRAY OF RECORD
          nss                      CHAR (11),
          curp                     CHAR (60),
          rfc                      CHAR (60),
          paterno                  CHAR (60),
          materno                  CHAR (60),
          nombre                   CHAR (60),
          categoria                CHAR (60),
          esatdo                   CHAR (60),
          fecha                    DATE
    END RECORD
    
END GLOBALS

MAIN

   LET g_usuario          =   ARG_VAL  (1)
   LET p_tipo_ejecucion   =   ARG_VAL  (2)
   LET g_titulo           =   ARG_VAL  (3)

-- Se asigna el título de la ventana
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

-- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".CATM07.log")

--CLOSE WINDOW SCREEN

--OPEN WINDOW opciones WITH FORM "CATM075"   
--**************************************************************************
--Menú de selección de acciones para ALTA,BAJA o CONSULTA en cat_nss_rojo  *
--**************************************************************************
   MENU --"SELECCIONE UNA ACCIÓN A REALIZAR"

      ON ACTION alta
         CALL fn_alta()
         --EXIT MENU

      ON ACTION baja
         CALL fn_baja()

      ON ACTION consulta
        CALL fn_consulta_todo()

      ON ACTION salir
         EXIT MENU

      ON ACTION CLOSE
         EXIT MENU

   END MENU

--CLOSE WINDOW opciones

END MAIN

FUNCTION fn_alta()

   DEFINE v_nss_alta               CHAR(11)  -- Variable de nss ingresado para alta
   DEFINE v_id_alta                DECIMAL(9,0) -- Variable de id_derechohabiente ingresado para alt a
   DEFINE v_estado_alta            SMALLINT     -- Variable de estado ingresado para alta
   DEFINE v_valida_nss             SMALLINT     -- Variable para validar nss
   DEFINE v_cuenta_id_rojo         INTEGER      -- Variable para seguir el consecutivo de id_rojo
   DEFINE v_id_rojo                INTEGER      -- Variable con secuencia de id
   DEFINE rec_categoria            RECORD LIKE cat_categoria_roja.*
   DEFINE v_cuenta_existencia      SMALLINT
   DEFINE v_categoria_alta         CHAR(3)   -- Variable de categoría ingresada para alta

   MENU
      ON ACTION ARCHIVO
         CALL fn_carga_masiva(1)
      EXIT MENU

      ON ACTION INDIVIDUAL

   OPEN WINDOW alta WITH FORM "CATM072"
   

      INPUT BY NAME v_nss_alta
                    ,v_categoria_alta ATTRIBUTES (UNBUFFERED)

     BEFORE INPUT 
     LET cb = ui.ComboBox.forName("v_categoria_alta")

    DECLARE cur_categoria CURSOR FOR SELECT * FROM cat_categoria_roja
   FOREACH cur_categoria INTO rec_categoria.*
      CALL cb.addItem(rec_categoria.categoria, rec_categoria.categoria_desc)
   END FOREACH
   

      ON ACTION ACCEPT

      IF v_nss_alta IS NULL THEN
         CALL fn_mensaje("Atención","Debe ingresar un NSS", "stop")
         NEXT FIELD v_nss_alta
      END IF

      IF v_categoria_alta IS NULL THEN
         CALL fn_mensaje("Atención","Debe elegir una categoría", "stop")
         NEXT FIELD v_nss_alta
      END IF

      IF (v_nss_alta IS NOT NULL ) AND (v_categoria_alta IS NOT NULL) THEN

      SELECT COUNT (*)
      INTO v_cuenta_existencia
      FROM afi_nss_rojo
      WHERE nss = v_nss_alta
      AND estado_rojo = 1

      IF v_cuenta_existencia <> 0 THEN

      CALL fn_mensaje("Atención","El NSS ingresado ya está ACTIVO", "stop")
      ELSE 

      OPEN WINDOW datos_generales WITH FORM "CATM071"

      SELECT af.ap_paterno_af,af.ap_materno_af,af.nombre_af,af.curp,af.rfc
        INTO v_paterno,v_materno,v_nombre,v_curp,v_rfc
        FROM afi_derechohabiente af
       WHERE af.nss = v_nss_alta

     SELECT categoria_desc
       INTO v_categoria
       FROM cat_categoria_roja cc
      WHERE cc.categoria = v_categoria_alta
        

        SELECT estado_rojo_desc
          INTO v_estado
          FROM cat_estado_rojo ce
         WHERE ce.estado_rojo = 1 

    LET v_nss = v_nss_alta

    DISPLAY BY NAME   v_nss,
                      v_curp,
                      v_rfc,
                      v_paterno,
                      v_materno,
                      v_nombre,
                      v_categoria,
                      v_estado

    MENU

   ON ACTION ACCEPT
      {SELECT MAX (id_rojo)
        INTO v_cuenta_id_rojo
        FROM afi_nss_rojo}

      SELECT COUNT (*)
        INTO v_valida_nss
        FROM afi_derechohabiente
       WHERE nss = v_nss_alta
       IF v_valida_nss <> 0 THEN

         SELECT id_derechohabiente
           INTO v_id_alta
           FROM afi_derechohabiente
          WHERE nss = v_nss_alta

         {SELECT estado_rojo
           INTO v_estado_alta
           FROM afi_estado_rojo
          WHERE estado_rojo_desc = "ACTIVO"}

          LET v_estado_alta = 1


         INSERT INTO afi_nss_rojo VALUES ( seq_afi_rojo.NEXTVAL,
                                           v_nss_alta,
                                           v_id_alta,
                                           v_categoria_alta,
                                           v_estado_alta,
                                           TODAY,
                                           g_usuario)

         CALL fn_mensaje("Atención","Los datos fueron almacenados correctamente", "stop")
         EXIT MENU
      END IF

ON ACTION CANCEL
EXIT MENU
--EXIT DIALOG
END MENU 
CLOSE WINDOW datos_generales

END IF
LET v_nss_alta = ""
END IF

      ON ACTION CANCEL
         EXIT INPUT
        -- EXIT DIALOG
         END INPUT
--END DIALOG
   CLOSE WINDOW alta
   EXIT MENU
   END MENU
END FUNCTION




FUNCTION fn_baja()

 MENU ""
    ON ACTION Individual
      CALL fn_baja_individual()

    ON ACTION Archivo
       CALL baja_archivo()
       
    ON ACTION CANCEL 
       EXIT MENU 
 END MENU 

END FUNCTION


FUNCTION fn_consulta_todo()

DEFINE v_cuenta             INTEGER  -- Variable que valida que existan registros en tabla cat_nss_rojo

OPEN WINDOW datos_generales WITH FORM "CATM074"

INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)

ON ACTION ACCEPT

    IF (v_nss IS NOT NULL) THEN


   SELECT COUNT (*)
     INTO v_cuenta
     FROM afi_nss_rojo
    WHERE nss = v_nss

    IF v_cuenta = 0 THEN
      CALL fn_mensaje("Atención","No se encontraron registros relacionados con los datos ingresados", "stop")

ELSE


  LET v_query = '
SELECT af.nss,
       af.curp,
       af.rfc,
       af.ap_paterno_af,
       af.ap_materno_af,
       af.nombre_af,
       cc.categoria_desc,
       ce.estado_rojo_desc,
       cn.f_actualiza
  FROM afi_derechohabiente af,
       afi_nss_rojo cn,
       cat_categoria_roja cc,
       cat_estado_rojo ce
 WHERE cc.categoria = cn.categoria
   AND ce.estado_rojo = cn.estado_rojo
   AND af.nss = cn.nss
   and cn.nss =',v_nss

PREPARE prp_consulta FROM v_query
DECLARE cur_consulta CURSOR FOR prp_consulta

LET a = 1

--Limpia arreglo
CALL arr_general.clear()

FOREACH cur_consulta INTO arr_general[a].*
LET a = a +1
END FOREACH

      IF arr_general[arr_general.getLength()].nss IS NULL AND
         a > 1 THEN
         CALL arr_general.deleteElement(arr_general.getLength())
      END IF

DISPLAY ARRAY arr_general TO tab1.*
END DISPLAY
EXIT INPUT 

  END IF  
END IF 

ON ACTION CANCEL
EXIT INPUT
END INPUT 
CLOSE WINDOW datos_generales

END FUNCTION

FUNCTION fn_carga_masiva(p_pgm)

   DEFINE p_pgm SMALLINT
   DEFINE v_pgm CHAR (6)
   DEFINE v_ruta_bin LIKE seg_modulo.ruta_bin
   DEFINE v_comando STRING

   INITIALIZE v_comando TO NULL

   SELECT ruta_bin
     INTO v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   IF p_pgm = 1 THEN
      LET v_pgm = 'AFIP18'
   END IF

   LET v_comando = "cd ",v_ruta_bin CLIPPED,"/; fglrun ", v_pgm," ",g_usuario CLIPPED,
                " ",p_tipo_ejecucion CLIPPED," ",g_titulo

   DISPLAY "comando :",v_comando
   -- CALL ui.interface.refresh()

    LET v_comando = v_comando CLIPPED
    RUN v_comando
   
END FUNCTION

FUNCTION fn_baja_individual()
   DEFINE v_cuenta_nss             INTEGER  -- Variable que valida que existan registros en tabla cat_nss_rojo
   DEFINE v_nss_baja               CHAR(11)  -- Variable de nss ingresado para baja de registros
   DEFINE v_mensaje                CHAR(15)

   OPEN WINDOW baja WITH FORM "CATM073"

      INPUT BY NAME v_nss_baja ATTRIBUTES (UNBUFFERED) 
      
         ON ACTION ACCEPT

            SELECT COUNT (*)
             INTO v_cuenta_nss
             FROM afi_nss_rojo cn, afi_derechohabiente af
            WHERE cn.nss = v_nss_baja
              AND cn.nss = af.nss
              AND estado_rojo = 1

            IF v_cuenta_nss <> 0 THEN

               OPEN WINDOW datos_generales WITH FORM "CATM071"
      
                  SELECT af.ap_paterno_af,af.ap_materno_af,af.nombre_af,af.curp,af.rfc
                   INTO v_paterno,v_materno,v_nombre,v_curp,v_rfc
                   FROM afi_derechohabiente af
                  WHERE af.nss = v_nss_baja

                  SELECT categoria_desc
                    INTO v_categoria
                    FROM afi_nss_rojo cn,cat_categoria_roja cc
                   WHERE cn.nss = v_nss_baja
                     AND cc.categoria = cn.categoria
                     AND cn.estado_rojo = 1

                  SELECT estado_rojo_desc
                    INTO v_estado
                    FROM cat_estado_rojo ce,afi_nss_rojo cn
                   WHERE cn.nss = v_nss_baja
                     AND cn.estado_rojo = ce.estado_rojo
                     AND cn.estado_rojo = 1


                  LET v_nss = v_nss_baja

                  DISPLAY BY NAME v_nss,
                                   v_curp,
                                   v_rfc,
                                   v_paterno,
                                   v_materno,
                                   v_nombre,
                                   v_categoria,
                                   v_estado

                  MENU

                     ON ACTION ACCEPT

                        UPDATE afi_nss_rojo
                           SET estado_rojo = "2",
                               f_actualiza = TODAY
                         WHERE nss = v_nss_baja

                         LET v_mensaje = "Baja Lógica"

                         CALL fn_mensaje("Atención","Los registros fueron actualizados correctamente como:"
                                                             ||v_mensaje,"stop")
                         LET v_nss = ""

                         EXIT MENU

                     ON ACTION CANCEL
                        EXIT MENU
                        
                  END MENU 

              CLOSE WINDOW datos_generales

            ELSE
               CALL fn_mensaje("Atención","No se encontraron registros para actualizar ligados a NSS ingresado", "stop")
            END IF

            LET v_nss_baja = ""

         ON ACTION CANCEL 
            EXIT INPUT 
      END INPUT

      CLOSE WINDOW baja

END FUNCTION 

FUNCTION baja_archivo()

   DEFINE v_archivo          STRING
   DEFINE v_cnt_nss          SMALLINT
   DEFINE v_pos              INTEGER  -- Posición donde inicia la extensión ".txt"
   DEFINE cant               INTEGER  -- Cantidad de caracteres que tiene el nombre del archivo
   DEFINE buf                base.StringBuffer
   DEFINE v_extension        STRING
   DEFINE v_ruta_explorador  INTEGER
   DEFINE v_bandera          SMALLINT
   DEFINE v_ruta_rescate     LIKE seg_modulo.ruta_rescate

OPEN WINDOW vtn_archivo WITH FORM "CATM076"

   INPUT BY NAME v_archivo ATTRIBUTES ( UNBUFFERED )

      ON ACTION ACCEPT

      --CALL fn_mensaje ("Validación de archivo" ,v_archivo,"information")
      
      --********************************************************
      --Se valida que el archivo tenga extensión correcta      *
      --********************************************************
      IF v_archivo IS NULL THEN
         CALL fn_mensaje ("Archivo","Debe de seleccionar un archivo","information")
         NEXT FIELD v_archivo
      END IF

      IF v_archivo.getIndexOf(" ", 1) THEN
         CALL fn_mensaje ("Archivo","Nombre de archivo no debe contener espacios","information")
         LET v_archivo = ""
         DISPLAY BY NAME v_archivo
         NEXT FIELD v_archivo
      END IF

      IF v_archivo IS NOT NULL THEN

         SELECT ruta_rescate
           INTO v_ruta_rescate
           FROM seg_modulo
          WHERE modulo_cod = 'afi'

         --CALL fn_mensaje ("Validación de archivo" ,v_archivo,"information")
         LET buf = base.StringBuffer.create()
         CALL buf.append(v_archivo)

         LET cant         = LENGTH(v_archivo)    # cantidad de caracteres de nombre de archivo
         LET v_pos        = buf.getIndexof(".",1) # posición del punto para validar extensión
         LET v_pos = v_pos + 1
         LET v_extension  = buf.subString(v_pos,cant)
         LET v_ruta_explorador = buf.getIndexof("C:",1)

         --CALL fn_mensaje ("Transferencia Archivo", v_archivo,"information")

         IF v_ruta_explorador >= 1 THEN
            LET v_archivo = buf.subString(13,cant)
         END IF

         --CALL fn_mensaje ("Transferencia Archivo", v_archivo,"information")

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
            CALL fn_mensaje ("","Extensión del archivo no es correcta. \n El archivo debe tener extensión '.ptco' o '.econ' ","")
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
            CALL fn_crea_tmp()
            CALL fn_obtiene_datos(v_bandera)
            CALL fn_mensaje ("" ,"Los registros fueron actualizados correctamente como baja lógica","")
            EXIT INPUT

            --CONTINUE INPUT

            CATCH
               ERROR "NO SE PUDO TRANSFERIR"
               CONTINUE INPUT
               END TRY
               EXIT INPUT
         END IF

      END IF

      ON ACTION CANCEL
         EXIT INPUT

      END INPUT
      
CLOSE WINDOW vtn_archivo
      
END FUNCTION 


FUNCTION fn_obtiene_datos(p_bandera)

   DEFINE s         CHAR (11)  -- variable para leer lineas del archivo
   DEFINE ch1       base.Channel 
   DEFINE tok       base.StringTokenizer
   DEFINE buf1      base.StringBuffer
   DEFINE cadena    CHAR (11)  -- variable para rescatar lineas del archivo
   DEFINE i         INTEGER 
   DEFINE p_bandera SMALLINT 

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
         
         INSERT INTO safre_tmp:nss_rojo
              VALUES (cadena)

         -- Actualiza, si bandera = 1 (Politicos)
         IF (p_bandera = 1) THEN 
         
            UPDATE afi_nss_rojo
               SET estado_rojo = 2,
                   f_actualiza = TODAY 
             WHERE nss IN ( SELECT nss
                               FROM safre_tmp:nss_rojo)
               AND estado_rojo = 1
               AND categoria   = 1;
         ELSE 
            -- Si la bandera = 2, actualiza sólo economicos
            IF(p_bandera = 2) THEN 
               UPDATE afi_nss_rojo
                  SET estado_rojo = 2,
                      f_actualiza = TODAY 
                WHERE nss IN ( SELECT nss
                                  FROM safre_tmp:nss_rojo)
                  AND estado_rojo = 1
                  AND categoria   = 2;
                  
            END IF 
         END IF 

      END WHILE

      IF ch1.isEof() THEN EXIT WHILE
      END IF
         LET i = i + 1
      END WHILE
   CALL ch1.close()
END FUNCTION

FUNCTION fn_crea_tmp()

   DATABASE safre_tmp
      DROP TABLE IF EXISTS nss_rojo
      
      CREATE TABLE nss_rojo (nss CHAR(11))
      
   DATABASE safre_viv
   
END FUNCTION
