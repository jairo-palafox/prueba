--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación: 24/11/2015
--===============================================================
################################################################################
#Módulo       => CAT                                                           #
#Programa     => CATM11                                                        #
#Objetivo     => Mantenimiento al catálogo de inconsistencias                  #
#Autor        => Mauro Muñiz Caballero                                         #
#Fecha inicio => 24 de noviembre de 2015                                       #
################################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario_cod             CHAR(20)     # Clave del usuario
   DEFINE p_tipo_ejecucion          SMALLINT    # Forma como ejecutara el programa
   DEFINE p_titulo_vtna             STRING      # Texto que aparece en titulo de ventana
   DEFINE v_confirma                SMALLINT
   DEFINE f                         ui.Form
   DEFINE w                         ui.Window

   DEFINE v_reg_incons DYNAMIC ARRAY OF RECORD
      seleccion                     BOOLEAN,
      inconsistencia                CHAR(2),
      descripcion                   CHAR(80)
   END RECORD

   DEFINE v_ventana                 ui.Window
   DEFINE v_forma                   ui.Form
   DEFINE v_ruta_ejecutable         CHAR(40)

END GLOBALS

MAIN

   # Recupera parámetros de programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   CALL fn_elije_opcion()

END MAIN

FUNCTION fn_elije_opcion()

   # se recupera la ruta ejecutable del módulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "cat"

   #Se asigna el título de la ventana
   IF(p_titulo_vtna IS NOT NULL)THEN
      CALL ui.Interface.setText(p_titulo_vtna)
   END IF

   MENU
      BEFORE MENU
         ON ACTION agregar
            CALL fn_agrega_inconsistencia()

         ON ACTION consultar
            CALL fn_consulta_inconsistencia()
            IF v_confirma = 1 THEN
               CALL fn_mensaje("Atención"," Es necesario volver a ingresar al menú, \n todas las ventanas serán cerradas para refrescar la información.", "stop")
            EXIT MENU
         END IF

         ON ACTION cancelar
            EXIT MENU
   END MENU
END FUNCTION

FUNCTION fn_agrega_inconsistencia()

   DEFINE l_reg_incons RECORD
      v_inconsistencia              CHAR(2),
      v_descripcion                 CHAR(80)
   END RECORD

   DEFINE r_ind_incons              SMALLINT
   DEFINE r_confirma                BOOLEAN
   DEFINE v_continua                BOOLEAN

   OPEN WINDOW vtna_v_inconsistencia WITH FORM v_ruta_ejecutable CLIPPED||"/CATM111"

      #Se asigna el título de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma   = v_ventana.getForm()

      ## oculta tabla de catalogo de inconsistencias
      CALL v_forma.setElementHidden("gpo_catalogo_inconsistencias",TRUE)
      
      IF (p_titulo_vtna IS NOT NULL) THEN
         CALL ui.Interface.setText(p_titulo_vtna)
         CALL v_ventana.setText(p_titulo_vtna)
      END IF

      LET v_continua = TRUE

      WHILE v_continua
         DIALOG ATTRIBUTES (UNBUFFERED)
            INPUT l_reg_incons.v_inconsistencia,
                  l_reg_incons.v_descripcion --WITHOUT DEFAULTS
             FROM v_inconsistencia,
                  v_descripcion --ATTRIBUTES(UNBUFFERED)
{
               AFTER FIELD v_inconsistencia
                  CALL fn_verifica_inconsistencia(l_reg_incons.v_inconsistencia,l_reg_incons.v_descripcion) RETURNING r_ind_incons

                  CASE r_ind_incons
                     WHEN 0
                        NEXT FIELD v_descripcion
                     WHEN 1
                        CALL fn_mensaje("Aviso","La inconsistencia capturada ya existe en el catálogo","information")
                        NEXT FIELD v_inconsistencia
                     WHEN 2
                        CALL fn_mensaje("Aviso","La inconsistencia capturada es incorrecta","information")
                        NEXT FIELD v_inconsistencia
                     WHEN 3
                        
                  END CASE}
            END INPUT

            # Arreglo de inconsistencias ya capturadas
            DISPLAY ARRAY v_reg_incons TO sr_inconsistencias.*

            END DISPLAY

            BEFORE DIALOG
               CALL fn_recupera_inconsistencias()

            ON ACTION guardar
               CALL fn_verifica_inconsistencia(l_reg_incons.v_inconsistencia,l_reg_incons.v_descripcion) RETURNING r_ind_incons

               CASE r_ind_incons
                  WHEN 0
                     ---NEXT FIELD v_descripcion
                  WHEN 1
                     CALL fn_mensaje("Aviso","La inconsistencia capturada ya existe en el catálogo","information")
                     NEXT FIELD v_inconsistencia
                  WHEN 2
                     CALL fn_mensaje("Aviso","La inconsistencia capturada es incorrecta","information")
                     NEXT FIELD v_inconsistencia
               END CASE

               IF(l_reg_incons.v_inconsistencia IS NULL)THEN
                  CALL fn_mensaje("Aviso","Capture inconsistencia","information")
                  NEXT FIELD v_inconsistencia
               END IF

               IF(l_reg_incons.v_descripcion IS NULL)THEN
                  CALL fn_mensaje("Aviso","Capture descripción de inconsistencia","information")
                  NEXT FIELD v_descripcion
               END IF
               IF r_ind_incons <> 3 THEN
               CALL fn_ventana_confirma("Confimar","¿Guardar inconsistencia?","question") RETURNING r_confirma

               IF(r_confirma)THEN
                  CALL fn_inserta_inconsistencia(l_reg_incons.*)
                  CALL fn_mensaje("Aviso","Registro almacenado correctamente","information")
                  EXIT DIALOG
               ELSE
                  CONTINUE DIALOG
               END IF
               ELSE
                  EXIT DIALOG
               END IF
            ON ACTION cancelar
               LET v_continua = FALSE
               EXIT DIALOG

         END DIALOG
      END WHILE

   CLOSE WINDOW vtna_v_inconsistencia

END FUNCTION

FUNCTION fn_consulta_inconsistencia()

   DEFINE v_criterios_busqueda RECORD
      v_incons_ini                  CHAR(2),
      v_incons_fin                  CHAR(2),
      v_descripcion                 CHAR(80)
   END RECORD

   DEFINE v_continua                BOOLEAN
   DEFINE v_pos                     INTEGER

   OPEN WINDOW vtna_consulta_incons WITH FORM v_ruta_ejecutable CLIPPED||"/CATM112"

      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma   = v_ventana.getForm()

      ## oculta tabla grupo de inconsistencias
      CALL v_forma.setElementHidden("gpo_inconsistencias",TRUE) 

      IF(p_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_vtna)
         CALL v_ventana.setText(p_titulo_vtna)
      END IF

      INPUT v_criterios_busqueda.v_incons_ini,
            v_criterios_busqueda.v_incons_fin,
            v_criterios_busqueda.v_descripcion
       FROM v_incons_ini,
            v_incons_fin,
            v_descripcion ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE INPUT
            # recupera el combobox del ejercicio

         AFTER FIELD v_incons_fin
            IF(v_criterios_busqueda.v_incons_ini > v_criterios_busqueda.v_incons_fin)THEN
               CALL fn_mensaje("Aviso","Inconsistencia inicial no puede ser mayor a inconsistencia final","information")
               NEXT FIELD v_incons_fin
            END IF

         ON ACTION consultar
            IF(v_criterios_busqueda.v_incons_ini > v_criterios_busqueda.v_incons_fin)THEN
               CALL fn_mensaje("Aviso","Inconsistencia inicial no puede ser mayor a inconsistencia final","information")
               NEXT FIELD v_incons_fin
            END IF

            CALL fn_recupera_incons_criterio(v_criterios_busqueda.*) RETURNING v_continua

            IF NOT(v_continua)THEN
               CALL fn_mensaje("Aviso","No se encontraron registros con criterio dado","information")
               CONTINUE INPUT
            ELSE 
               ## pone visible tabla grupo de inconsistencias
               CALL v_forma.setElementHidden("gpo_inconsistencias",FALSE)

               ## oculta gpo_conceptos_busqueda
               CALL v_forma.setElementHidden("gpo_conceptos_busqueda",TRUE)
               
               DISPLAY ARRAY v_reg_incons TO sr_incons_c.* ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
               BEFORE DISPLAY

                  ON ACTION aceptar
                     ## pone visible gpo_conceptos_busqueda
                     CALL v_forma.setElementHidden("gpo_conceptos_busqueda",FALSE)
                     ## pone visible tabla grupo de inconsistencias
                     CALL v_forma.setElementHidden("gpo_inconsistencias",TRUE)
                     EXIT DISPLAY
               
                  ON ACTION editar
                     LET v_pos = arr_curr()
                     CALL fn_editar(v_pos)
                     EXIT DISPLAY

                  ON ACTION quitar
                     LET v_pos = arr_curr()
                     CALL fn_baja(v_pos)
                     EXIT DISPLAY 

               END DISPLAY
            END IF
          --  EXIT INPUT

         ON ACTION cancelar
            LET v_continua = FALSE
            EXIT INPUT
      END INPUT

   CLOSE WINDOW vtna_consulta_incons

END FUNCTION

FUNCTION fn_recupera_inconsistencias()

   DEFINE v_consulta                STRING
   DEFINE v_incons_ini              CHAR(2)
   DEFINE v_incons_fin              CHAR(2)
   DEFINE v_indice                  INTEGER

   DEFINE vl_inconsistencia RECORD
      inconsistencia              CHAR(2),
      descripcion                 CHAR(80)
   END RECORD

   # Limpia arreglo
   CALL v_reg_incons.clear()

   LET v_indice = 1

   DISPLAY v_incons_ini 
   DISPLAY v_incons_fin

   LET v_consulta = "\n SELECT inconsistencia,",
                    "\n        incons_desc",
                    "\n   FROM cat_inconsistencia",
                    "\n  WHERE inconsistencia BETWEEN ? AND ?
                           AND estado = 1",
                    "\n  ORDER by inconsistencia ASC"

   PREPARE prp_recupera_incons FROM v_consulta
   DECLARE cur_recupera_incons CURSOR FOR prp_recupera_incons
   FOREACH cur_recupera_incons USING v_incons_ini,
                                     v_incons_fin
                                INTO vl_inconsistencia.*

      LET v_reg_incons[v_indice].inconsistencia = vl_inconsistencia.inconsistencia
      LET v_reg_incons[v_indice].descripcion    = vl_inconsistencia.descripcion

      LET v_indice = v_indice + 1

   END FOREACH

   FREE cur_recupera_incons

END FUNCTION

FUNCTION fn_verifica_inconsistencia(p_inconsistencia,p_desc)

   DEFINE p_inconsistencia          CHAR(2)
   DEFINE l_inconsistencia          CHAR(1)
   DEFINE v_existe_incons           BOOLEAN
   DEFINE v_consulta                STRING
   DEFINE v_ind_incons              SMALLINT
   DEFINE v_valor                   SMALLINT
   DEFINE i                         SMALLINT
   DEFINE v_cnt_incons              INTEGER
   DEFINE p_desc                    CHAR (80)

   LET v_ind_incons    = 0
   LET v_existe_incons = 0
   LET v_valor         = 0

   SELECT COUNT(*)
     INTO v_cnt_incons
     FROM cat_inconsistencia
    WHERE inconsistencia = p_inconsistencia
      AND estado = 2

   IF v_cnt_incons >= 1 THEN
      UPDATE cat_inconsistencia
        SET estado  = 1,
        incons_desc = p_desc
    WHERE inconsistencia = p_inconsistencia
      CALL fn_mensaje("Atención","Inconsistencia reactivada de forma correcta", "stop")
      LET v_ind_incons = 3
   ELSE

   LET v_consulta = " SELECT FIRST 1 1",
                    "  FROM cat_inconsistencia",
                    " WHERE inconsistencia = ?
                        AND estado = 1"

   PREPARE prp_existe_incons FROM v_consulta
   EXECUTE prp_existe_incons USING p_inconsistencia
                             INTO v_existe_incons

   --DISPLAY v_existe_incons

   IF (v_existe_incons) THEN
      LET v_ind_incons = 1 # ya existe la inconsistencia
   ELSE
      FOR i = 1 TO 2
         LET l_inconsistencia = p_inconsistencia[i]

         DISPLAY l_inconsistencia,"  ",v_valor

         LET v_consulta = "SELECT valor\n",
                          "  FROM cat_ascii\n",
                          " WHERE chr = ?\n",
                          "   AND valor between 48 AND 57"
                          --"\n     OR  valor between 65 and 90)"

         PREPARE prp_existe_valor FROM v_consulta
         EXECUTE prp_existe_valor USING l_inconsistencia
                                  INTO v_valor

         IF (v_valor = 0 OR v_valor IS NULL) THEN
            LET v_ind_incons = 2 # inconsistencia inválida

            EXIT FOR
         END IF
      END FOR
   END IF
   END IF
   RETURN v_ind_incons

END FUNCTION


FUNCTION fn_inserta_inconsistencia(p_reg_incons)

   DEFINE p_reg_incons RECORD
      inconsistencia                CHAR(2),
      descripcion                   CHAR(80)
   END RECORD

   DEFINE v_fecha_actual            DATE

   LET v_fecha_actual = TODAY

   INSERT INTO cat_inconsistencia(inconsistencia,
                                  incons_desc,
                                  estado,
                                  usuario,
                                  f_actualiza)
                           VALUES(p_reg_incons.inconsistencia,
                                  p_reg_incons.descripcion,
                                  "1",
                                  p_usuario_cod,
                                  v_fecha_actual)

END FUNCTION

FUNCTION fn_recupera_incons_criterio(p_criterios_busqueda)

   DEFINE p_criterios_busqueda RECORD
      v_incons_ini                  CHAR(2),
      v_incons_fin                  CHAR(2),
      v_descripcion                 CHAR(80)
   END RECORD

   DEFINE v_filtro                  STRING
   DEFINE v_consulta                STRING

   DEFINE lc_inconsistencia RECORD
       v_inconsistencia             CHAR(2),
       v_descripcion                CHAR(80)
    END RECORD

    DEFINE v_indice                 INTEGER
    DEFINE v_recupero_incons        BOOLEAN

   IF(p_criterios_busqueda.v_incons_ini IS NULL AND
      p_criterios_busqueda.v_incons_fin IS NULL AND
      p_criterios_busqueda.v_descripcion IS NULL) THEN

      LET v_filtro = " 1 = 1 "
   ELSE
      LET v_filtro = " "
      # Rango
      IF LENGTH (p_criterios_busqueda.v_incons_ini) = 1 THEN
               LET p_criterios_busqueda.v_incons_ini = "0",p_criterios_busqueda.v_incons_ini
      END IF

      IF LENGTH (p_criterios_busqueda.v_incons_fin) = 1 THEN
               LET p_criterios_busqueda.v_incons_fin = "0",p_criterios_busqueda.v_incons_fin
      END IF
            
      IF(p_criterios_busqueda.v_incons_ini IS NOT NULL)THEN
         IF(p_criterios_busqueda.v_incons_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro," inconsistencia BETWEEN '",p_criterios_busqueda.v_incons_ini,"' AND '",p_criterios_busqueda.v_incons_fin,"' AND "  
         ELSE
            LET v_filtro = v_filtro," inconsistencia = '",p_criterios_busqueda.v_incons_ini,"' AND "
         END IF
      ELSE
         IF(p_criterios_busqueda.v_incons_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro," inconsistencia = '",p_criterios_busqueda.v_incons_fin,"' AND "
         END IF
      END IF

      # Descripción
      IF(p_criterios_busqueda.v_descripcion IS NOT NULL)THEN
         LET v_filtro = v_filtro," incons_desc MATCHES '",p_criterios_busqueda.v_descripcion CLIPPED,"*'"
      ELSE
      ---# elimina el ultimo AND
      LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-4)
      END IF
   END IF

   # Limpia arreglo a desplegar
   CALL v_reg_incons.clear()

   LET v_indice = 1

   --DISPLAY "filtro : ",v_filtro

   LET v_consulta = "SELECT inconsistencia,\n",
                    "       incons_desc\n",
                    "  FROM cat_inconsistencia\n",
                    " WHERE ",v_filtro," AND estado = 1 \n",
                    " ORDER BY inconsistencia"
   --DISPLAY "consulta : ",v_consulta 

   PREPARE prp_recupera_criterio FROM v_consulta
   DECLARE cur_recupera_criterio CURSOR FOR prp_recupera_criterio

   FOREACH cur_recupera_criterio INTO lc_inconsistencia.*
      LET v_reg_incons[v_indice].seleccion      = 0
      LET v_reg_incons[v_indice].inconsistencia = lc_inconsistencia.v_inconsistencia
      LET v_reg_incons[v_indice].descripcion   = lc_inconsistencia.v_descripcion

      LET v_indice = v_indice + 1
   END FOREACH

   FREE cur_recupera_criterio

   IF( v_reg_incons.getLength() > 0 )THEN
      LET v_recupero_incons = TRUE
   ELSE
      LET v_recupero_incons = FALSE
   END IF

   RETURN v_recupero_incons

END FUNCTION

FUNCTION fn_editar(v_pos)

   DEFINE v_pos            INTEGER
   DEFINE v_descripcion    CHAR(80)
   DEFINE v_inconsistencia CHAR(2)

   OPEN WINDOW vtna_v_inconsistencia WITH FORM v_ruta_ejecutable CLIPPED||"/CATM111"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   CALL f.setElementHidden("gpo_catalogo_inconsistencias",TRUE)

   LET v_descripcion =""
   LET v_inconsistencia =""

   INPUT BY NAME v_descripcion ATTRIBUTES(UNBUFFERED)

   BEFORE INPUT 

   LET v_inconsistencia = v_reg_incons[v_pos].inconsistencia
   LET v_descripcion    = v_reg_incons[v_pos].descripcion

   DISPLAY BY NAME v_inconsistencia
   DISPLAY BY NAME v_descripcion

   ON ACTION ACCEPT

   IF (v_inconsistencia IS NULL) OR
      (v_descripcion IS NULL) THEN
      CALL fn_mensaje("Atención","No se puede actualizar información con datos en blanco", "stop")
   ELSE
      UPDATE cat_inconsistencia
         SET inconsistencia = v_inconsistencia,
             incons_desc    = v_descripcion,
             f_actualiza    = TODAY
       WHERE inconsistencia = v_inconsistencia

       CALL fn_mensaje("Atención","Los datos se actualizaron de forma correcta", "stop")
       EXIT INPUT
   END IF

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT

   CLOSE WINDOW vtna_v_inconsistencia

END FUNCTION

FUNCTION fn_baja(v_pos)

   DEFINE v_pos                INTEGER
   DEFINE v_inconsistencia     CHAR(2)
   DEFINE v_descripcion        CHAR(80)

   LET v_inconsistencia =""
   LET v_descripcion =""

   CALL fn_ventana_confirma("Alerta","¿Esta suguro de eliminar la inconsistencia seleccionada?","stop") RETURNING v_confirma

   IF v_confirma = 1 THEN
      LET v_inconsistencia  = v_reg_incons[v_pos].inconsistencia
      LET v_descripcion     = v_reg_incons[v_pos].descripcion

      UPDATE cat_inconsistencia
         SET estado = 2
       WHERE inconsistencia  = v_inconsistencia

       CALL fn_mensaje("Atención","Los datos fueron eliminados de forma correcta", "stop")
   ELSE
      CALL fn_mensaje("Atención","La petición fue cancelada..", "stop")
   END IF

END FUNCTION