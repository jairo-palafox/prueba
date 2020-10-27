##########################################################################
#Modulo            => CAT                                                #
#Programa          => CATM07                                             #
#Objetivo          => Mantenimiento a catálogo "afi_nss_rojo"            #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => Noviembre 2014                                     #
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
END FUNCTION




FUNCTION fn_baja()

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

 DISPLAY BY NAME      v_nss,
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
          SET estado_rojo = "2",f_actualiza = TODAY
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