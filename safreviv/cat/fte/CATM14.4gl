##########################################################################
#Modulo            => CAT                                                #
#Programa          => CATM14                                             #
#Objetivo          => Mantenimiento a catálogo "Entidades Financieras"   #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => 10/DICIEMBRE/2015                                  #
##########################################################################

DATABASE safre_viv

GLOBALS
   DEFINE g_titulo                 STRING    -- Variable para título de ventana
   DEFINE g_usuario                CHAR(20)  -- Variable para recuperar nombre de usuario
   DEFINE g_tipo_ejecucion         SMALLINT  -- Forma como ejecutará el programa
   DEFINE f                        ui.Form
   DEFINE w                        ui.Window
   DEFINE v_cve                    INTEGER
   DEFINE v_entidad                CHAR(50)
   DEFINE v_confirma               SMALLINT
   DEFINE cb                       ui.ComboBox
   DEFINE r_confirma               SMALLINT
   DEFINE v_combo                  SMALLINT

   DEFINE arr_fusion DYNAMIC ARRAY OF RECORD
      ent_receptora  CHAR(100),
      ent_cedente    CHAR(100),
      f_actualiza    DATE,
      usuario        CHAR(20)
   END RECORD

   DEFINE arr_consulta DYNAMIC ARRAY OF RECORD
          clave   SMALLINT,
          entidad CHAR(50),
          estado  CHAR(40)
   END RECORD
END GLOBALS

MAIN

   LET g_usuario          =   ARG_VAL  (1)
   LET g_tipo_ejecucion   =   ARG_VAL  (2)
   LET g_titulo           =   ARG_VAL  (3)

   LET v_confirma         = 0

   -- Se asigna el título de la ventana
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

-- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".CATM14.log")
   
--***********************************************************************
--Menú de selección de acciones para CONSULTA de entidades Financieras  *
--***********************************************************************
   MENU --"SELECCIONE UNA ACCIÓN A REALIZAR"

      ON ACTION agregar
         CALL fn_alta()

      ON ACTION consulta
         CALL fn_consulta_gral()
         IF v_confirma = 1 THEN
            CALL fn_mensaje("Atención"," Es necesario volver a ingresar al menú, \n todas las ventanas serán cerradas para refrescar la información.", "stop")
            EXIT MENU
         END IF

      ON ACTION salir
         EXIT MENU

      ON ACTION CLOSE
         EXIT MENU
   END MENU

END MAIN

FUNCTION fn_consulta_gral()

   DEFINE v_cuenta             INTEGER  -- Variable que valida que existan registros en tabla cat_municipio_inegi
   DEFINE a                    INTEGER
   DEFINE v_qry                STRING
   DEFINE v_pos                INTEGER

   OPEN WINDOW datos_generales WITH FORM "CATM141"

   SELECT COUNT (*)
     INTO v_cuenta
     FROM cat_entidad_financiera

   IF v_cuenta = 0 THEN
      CALL fn_mensaje("Atención","No se encontraron registros para esta consulta", "stop")

   ELSE

      LET v_qry = "select ef.cve_ent_financiera,
                          ef.ent_financiera_desc,
                          edo.estado_ef_desc
                     from cat_entidad_financiera ef, cat_estado_ef edo
                    WHERE ef.estado_ef = edo.estado_ef"

      PREPARE prp_consulta FROM v_qry
      DECLARE cur_consulta CURSOR FOR prp_consulta

      LET a = 1

      FOREACH cur_consulta INTO arr_consulta[a].clave,
                                arr_consulta[a].entidad,
                                arr_consulta[a].estado

         LET a = a +1
      END FOREACH

      IF arr_consulta[arr_consulta.getLength()].clave IS NULL AND
         a > 1 THEN
         CALL arr_consulta.deleteElement(arr_consulta.getLength())
      END IF

      IF arr_consulta[1].clave IS NOT NULL THEN
         DISPLAY ARRAY arr_consulta TO tab_entidad.* ATTRIBUTES (ACCEPT = FALSE, CANCEL=FALSE)

         ON ACTION editar
             LET v_pos = arr_curr()
             CALL fn_editar(v_pos)
             EXIT DISPLAY
{
         ON ACTION quitar
            LET v_pos = arr_curr()
            CALL fn_baja(v_pos)
            EXIT DISPLAY
}
         ON ACTION consultar

            LET v_pos = arr_curr()
         IF arr_consulta[v_pos].estado = "FUSIONADA" THEN

            LET v_qry = "SELECT a.cve_ent_financiera||'-'||a.ent_financiera_desc,
                                b.cve_ef_fusionada||'-'||c.ent_financiera_desc,
                                b.f_actualiza,
                                b.usuario
                           FROM cat_entidad_financiera a, cat_ef_fusionada b,cat_entidad_financiera c
                          WHERE a.cve_ent_financiera = b.cve_ent_financiera
                            AND b.cve_ef_fusionada = c.cve_ent_financiera
                            AND b.cve_ef_fusionada   = ",arr_consulta[v_pos].clave

            PREPARE prp_fusion FROM v_qry
            DECLARE cur_fusion CURSOR FOR prp_fusion

            LET a = 1

            FOREACH cur_fusion INTO arr_fusion[a].*
               LET a = a +1
            END FOREACH

            IF arr_fusion[arr_fusion.getLength()].ent_cedente IS NULL AND
               a > 1 THEN
               CALL arr_fusion.deleteElement(arr_fusion.getLength())
            END IF

            OPEN WINDOW datos_fusion WITH FORM "CATM142"

               LET w = ui.Window.getCurrent()
               LET f = w.getForm()

               CALL f.setElementHidden("tab_entidad",1)
               CALL f.setFieldHidden  ("v_combo",1)
               CALL f.setFieldHidden  ("v_cve",1)
               CALL f.setElementHidden("lb_cve",1)
               CALL f.setFieldHidden  ("v_entidad",1)
               CALL f.setElementHidden("lb_desc",1)

               IF arr_fusion[1].ent_cedente IS NOT NULL THEN

                  DISPLAY ARRAY arr_fusion TO tab_fusion.* ATTRIBUTES (ACCEPT = FALSE, CANCEL=FALSE)

                  ON ACTION salir
                     EXIT DISPLAY
                  END DISPLAY

                  CALL arr_fusion.clear()
               END IF

            CLOSE WINDOW datos_fusion
         ELSE
            CALL fn_mensaje ("Atención","Registro seleccionado no tiene entidad fusionada", "stop")
         END IF

         ON ACTION salir
            EXIT DISPLAY

         END DISPLAY
      ELSE
         CALL fn_mensaje ("Atención","No se encontraron registros para esta consulta", "stop")
      END IF
   END IF
   CLOSE WINDOW datos_generales

END FUNCTION

FUNCTION fn_alta()

   DEFINE v_bnd1        SMALLINT
   DEFINE v_cta_clave   SMALLINT
   DEFINE v_cta_entidad SMALLINT
   DEFINE v_bnd2        SMALLINT
   DEFINE v_estado      SMALLINT

   OPEN WINDOW alta WITH FORM "CATM142"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   CALL f.setElementHidden("tab_fusion",1)
   CALL f.setElementHidden("tab_entidad",1)
   CALL f.setFieldHidden("v_combo",1)

   INPUT BY NAME v_cve,v_entidad ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      IF v_cve IS NULL THEN
         CALL fn_mensaje ("Atención","Debe ingresar clave de entidad financiera válida", "stop")
      ELSE

         IF v_entidad IS NOT NULL THEN

            LET v_cta_clave = 0

            SELECT COUNT(*)
              INTO v_cta_clave
              FROM cat_entidad_financiera
             WHERE cve_ent_financiera = v_cve

            IF v_cta_clave > 0 THEN

               LET v_bnd1 = 0
               CALL fn_ventana_confirma ("Atención","Ya exíste una entidad financiera con la clave ingresada \n ¿ desea reactivarla ?", "stop") RETURNING r_confirma

               IF r_confirma = 1 THEN

                  UPDATE cat_entidad_financiera
                     SET estado_ef = 10
                   WHERE cve_ent_financiera = v_cve
                  CALL fn_mensaje ("Atención","Reactivación exitosa", "stop")
                  EXIT INPUT

               ELSE
                  CALL fn_mensaje ("Atención","Reactivación de municipio cancelada", "stop")
                  EXIT INPUT
               END IF

               EXIT INPUT

            ELSE
               LET v_bnd1 = 1
            END IF
         ELSE
            CALL fn_mensaje ("Atención","Debe ingresar nombre o descripción de entidad financiera válida", "stop")
         END IF
      END IF

      IF (v_bnd1 = 1 )THEN

         LET v_cta_entidad = 0

         SELECT COUNT(*)
           INTO v_cta_entidad
           FROM cat_entidad_financiera
          WHERE ent_financiera_desc = v_entidad

         LET v_cta_entidad = 0 -- se deja siempre en cero para evitar que haga la valudación para la descripción de entidad
         --ya que si puede existir dos claves diferentes para la misma descripción de entidad 13/02/2017

         IF v_cta_entidad > 0 THEN
            LET v_bnd2 = 0
            CALL fn_mensaje ("Atención","Ya exíste una entidad con la descripción ingresada", "stop")
            NEXT FIELD v_mun_desc
         ELSE
            LET v_bnd2 = 1
         END IF
      END IF

      IF (v_bnd1 = 1) AND 
         (v_bnd2 = 1) THEN

         LET v_estado = "10"

         INSERT INTO cat_entidad_financiera VALUES ( v_cve,
                                                 v_entidad,
                                                 v_estado,
                                                 g_usuario,
                                                 TODAY)

         CALL fn_mensaje ("Atención","Alta de entidad se realizó de forma correcta", "stop")
         EXIT INPUT
      END IF

      ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   CLOSE WINDOW alta

END FUNCTION

FUNCTION fn_editar(v_pos)

   DEFINE a                    INTEGER
   DEFINE v_qry                STRING
   DEFINE v_pos                INTEGER
   DEFINE rec_estado           RECORD LIKE cat_estado_ef.*
   DEFINE v_cod                SMALLINT
   DEFINE v_desc               CHAR(20)
   DEFINE v_cedente            SMALLINT

   DEFINE arr_entidad DYNAMIC ARRAY OF RECORD
      cve_ent_financiera   SMALLINT,
      ent_financiera_desc  CHAR(40)
   END RECORD


   OPEN WINDOW editar WITH FORM "CATM142"

   
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   CALL f.setElementHidden("tab_fusion",1)
   CALL f.setElementHidden("tab_entidad",1)

   LET cb = ui.ComboBox.forName("v_combo")

   INITIALIZE rec_estado.* TO NULL

   DECLARE cur_estado CURSOR FOR SELECT * FROM cat_estado_ef

   FOREACH cur_estado INTO rec_estado.*
      LET v_cod  = rec_estado.estado_ef CLIPPED
      LET v_desc = rec_estado.estado_ef_desc
      CALL cb.addItem(v_cod, v_desc)
   END FOREACH

   LET v_cve =""
   LET v_entidad =""

   INPUT BY NAME v_entidad,v_combo ATTRIBUTES(WITHOUT DEFAULTS ,UNBUFFERED)

   ON CHANGE v_combo
        
      IF v_combo = 20 THEN

         CALL f.setElementHidden("tab_entidad",0)

         LET v_qry = "SELECT cve_ent_financiera,
                             ent_financiera_desc
                        FROM cat_entidad_financiera
                       WHERE estado_ef = 10"

         PREPARE prp_entidad FROM v_qry
         DECLARE cur_entidad CURSOR FOR prp_entidad

         LET a = 1

         FOREACH cur_entidad INTO arr_entidad[a].*
            LET a = a +1
         END FOREACH

         IF arr_entidad[arr_entidad.getLength()].cve_ent_financiera IS NULL AND
            a > 1 THEN
               CALL arr_entidad.deleteElement(arr_entidad.getLength())
         END IF

         IF arr_entidad[1].cve_ent_financiera IS NOT NULL THEN

            DISPLAY ARRAY arr_entidad TO tab_entidad.* --ATTRIBUTES (ACCEPT = FALSE, CANCEL=FALSE)

            ON ACTION ACCEPT

               CALL fn_ventana_confirma ("Atención","¿Está seguro de fusionar entidad?", "stop") RETURNING r_confirma

               IF r_confirma = 1 THEN

                  DISPLAY "v_pos :",v_pos
                  LET v_cedente = arr_consulta[v_pos].clave

                  LET v_pos = ARR_CURR()

                  DISPLAY "cedente  :",v_cedente
                  DISPLAY "receptora:",arr_entidad[v_pos].cve_ent_financiera
                  
                  IF v_cedente = arr_entidad[v_pos].cve_ent_financiera THEN
                     CALL fn_mensaje ("Atención","No se puede realizar fusión sobre la misma entidad a fusionar", "stop")
                     EXIT DISPLAY
                  ELSE
                     INSERT INTO cat_ef_fusionada
                          VALUES (arr_entidad[v_pos].cve_ent_financiera,
                                  v_cve,
                                  g_usuario,
                                  TODAY)

                     UPDATE cat_entidad_financiera
                        SET estado_ef = 20
                      WHERE cve_ent_financiera = v_cve

                     CALL fn_mensaje ("Atención","Fusion de entidad se realizó de forma correcta", "stop")
                     EXIT DISPLAY
                  END IF
               ELSE
                  CALL fn_mensaje ("Atención","La fusión fue cancelada", "stop")
               END IF 
               EXIT DISPLAY

            ON ACTION CANCEL
               CALL f.setElementHidden("tab_entidad",1)
               LET v_combo = 10
               DISPLAY BY NAME v_combo
               EXIT DISPLAY

            END DISPLAY
            --EXIT INPUT
         END IF
      ELSE
         CALL f.setElementHidden("tab_entidad",1)
      END IF

   BEFORE INPUT

   LET v_combo = 10
   DISPLAY BY NAME v_combo

   LET v_cve = arr_consulta[v_pos].clave
   LET v_entidad = arr_consulta[v_pos].entidad

   DISPLAY BY NAME v_cve
   DISPLAY BY NAME v_entidad

   ON ACTION ACCEPT

      IF (v_cve IS NULL) OR
         (v_entidad IS NULL) THEN

         CALL fn_mensaje("Atención","No se puede actualizar información con datos en blanco", "stop")

      ELSE

         CALL fn_ventana_confirma("Alerta","¿Esta suguro de modificar la entidad seleccionada?","stop") RETURNING v_confirma
         IF v_confirma = 1 THEN

            UPDATE cat_entidad_financiera
               SET cve_ent_financiera  = v_cve,
                   ent_financiera_desc = v_entidad,
                   estado_ef           = v_combo,
                   f_actualiza         = TODAY
             WHERE cve_ent_financiera = arr_consulta[v_pos].clave
{
            IF v_combo = 10 THEN
               UPDATE cat_ef_fusionada
                  SET estado = 2
                WHERE cve_ef_fusionada = arr_consulta[v_pos].clave
            END IF 
}
            CALL fn_mensaje("Atención","Los datos se actualizaron de forma correcta", "stop")
            EXIT INPUT

         ELSE

            CALL fn_mensaje("Atención","La petición fué cancelada", "stop")
            EXIT INPUT

         END IF

      END IF
   ON ACTION CANCEL
      EXIT INPUT
   END INPUT

   CLOSE WINDOW editar
END FUNCTION

FUNCTION fn_baja(v_pos)

   DEFINE v_pos                INTEGER

   LET v_cve =""
   LET v_entidad =""

   CALL fn_ventana_confirma("Alerta","¿Esta suguro de eliminar la entidad seleccionada?","stop") RETURNING v_confirma

   IF v_confirma = 1 THEN
      LET v_cve = arr_consulta[v_pos].clave
      LET v_entidad = arr_consulta[v_pos].entidad

      DISPLAY 'v_clave :',arr_consulta[v_pos].clave

      UPDATE cat_entidad_financiera
         SET estado_ef = 30
       WHERE cve_ent_financiera  = arr_consulta[v_pos].clave

       CALL fn_mensaje("Atención","Los datos fueron eliminados de forma correcta", "stop")
   ELSE
      CALL fn_mensaje("Atención","La petición fue cancelada..", "stop")
   END IF

END FUNCTION