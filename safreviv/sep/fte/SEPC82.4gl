--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01/05/2012
--===============================================================
DATABASE safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ventana         ui.Window,
       v_detalle_docto   DYNAMIC ARRAY OF RECORD
         v_docto_cod     LIKE sep_cat_docto.docto_cod,
         v_numero        INTEGER,         
         v_documento     STRING,--LIKE sep_cat_docto.docto_desc,
         v_requerido     LIKE sep_cat_docto.ind_requerido
       END RECORD,
       v_elim_docto   DYNAMIC ARRAY OF RECORD
         v_docto_cod  LIKE sep_cat_docto.docto_cod,
         v_numero     INTEGER,         
         v_documento  STRING,--LIKE sep_cat_docto.docto_desc,
         v_requerido  LIKE sep_cat_docto.ind_requerido
       END RECORD,
       v_actualiza    BOOLEAN

MAIN

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   OPEN WINDOW vtna_docto WITH FORM v_ruta_ejecutable CLIPPED||"/SEPC821"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      DISPLAY ARRAY v_detalle_docto TO sr_docto.* 
                 ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         

         ON ACTION consultar
            CALL fn_recupera_doctos()
            IF NOT(v_detalle_docto.getLength() > 0)THEN
               CALL fn_mensaje(p_cad_ventana,"No hay información","info")
            END IF
            DISPLAY v_detalle_docto.getLength() TO flbl_total

         ON ACTION modificar
            # se inicializa para actualizar, comenzando como false para indicar que no hay actualizacion
            LET v_actualiza = FALSE
            # se revisa que exista informacion
            IF(v_detalle_docto.getLength() > 0)THEN            
               CALL fn_modifica_docto()
               CALL v_detalle_docto.clear()
               --ACCEPT DISPLAY
            ELSE
               CALL fn_mensaje(p_cad_ventana,"No hay información","info")
            END IF 

         ON ACTION salir
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_docto

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEP82                                                    #
#Descripcion       => Recupera el catálogo de documentos                       #
#Autor             => Hugo César Ramírez Gracía                                #
#Fecha inicio      => 01 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_doctos()
DEFINE v_docto       RECORD
         v_numero    LIKE sep_cat_docto.docto_cod,
         v_documento LIKE sep_cat_docto.docto_desc,
         v_requerido LIKE sep_cat_docto.ind_requerido
       END RECORD,
       v_indice      INTEGER

   WHENEVER ERROR CONTINUE
   CALL v_detalle_docto.clear()
   LET v_indice = 1
   
   DECLARE cur_rec_docto CURSOR FOR 
    SELECT docto_cod,docto_desc,ind_requerido
      FROM sep_cat_docto
      ORDER BY docto_cod

   FOREACH cur_rec_docto INTO v_docto.*
      LET v_detalle_docto[v_indice].v_numero    = v_indice
      LET v_detalle_docto[v_indice].v_docto_cod = v_docto.v_numero  
      LET v_detalle_docto[v_indice].v_documento = v_docto.v_documento  
      LET v_detalle_docto[v_indice].v_requerido = v_docto.v_requerido
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_docto

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEP82                                                    #
#Descripcion       => Mantenimiento a catálogo de documentos                   #
#Autor             => Hugo César Ramírez Gracía                                #
#Fecha inicio      => 01 Mayo 2012                                             #
################################################################################
FUNCTION fn_modifica_docto()
DEFINE v_indice   INTEGER,
       v_actual   INTEGER,
       r_confirma BOOLEAN,
       v_error_sql BOOLEAN

   INPUT ARRAY v_detalle_docto WITHOUT DEFAULTS FROM sr_docto.* 
                 ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
      BEFORE INPUT
         CALL DIALOG.setActionHidden( "aceptar", 1 )
         CALL DIALOG.setActionHidden( "cancelar", 1 )
         DISPLAY v_detalle_docto.getLength() TO flbl_total

      AFTER FIELD tedi_docto
         IF(LENGTH(v_detalle_docto[ARR_CURR()].v_documento CLIPPED) = 0)THEN
            CALL fn_mensaje("Aviso","Capture documento","info")
            NEXT FIELD tedi_docto
         END IF

      ON CHANGE tedi_docto
         CALL DIALOG.setActionHidden( "aceptar", 0 )
         CALL DIALOG.setActionHidden( "cancelar", 0 )
         LET v_actualiza = TRUE

      ON CHANGE tcb_requerido
         CALL DIALOG.setActionHidden( "aceptar", 0 )
         CALL DIALOG.setActionHidden( "cancelar", 0 )
         LET v_actualiza = TRUE

      ON ACTION aceptar
         IF(v_actualiza)THEN
            # se valida que no haya registros nulos
            FOR v_indice = 1 TO v_detalle_docto.getLength()
               LET v_detalle_docto[v_indice].v_numero = v_indice
               IF(v_detalle_docto[v_indice].v_documento CLIPPED IS NULL)THEN
                  CALL fn_mensaje("Aviso","Capture documento","information")
                  CALL FGL_SET_ARR_CURR(v_actual)
                  NEXT FIELD tedi_docto
               END IF
            END FOR
            CALL fn_ventana_confirma(p_cad_ventana,"¿Desea aceptar las modificaciones?","about")
                 RETURNING r_confirma
            IF(r_confirma)THEN  
               CALL fn_actualiza_docto() RETURNING v_error_sql
               IF(v_error_sql)THEN
                  CALL fn_mensaje("Aviso","Ocurrió un error al actualizar información","information")
               ELSE
                  CALL fn_mensaje("Aviso","Actualización realizada correctamente","information")
                  EXIT INPUT
               END IF
               
            END IF
         END IF

      ON ACTION agregar
         # se valida que no haya registros nulos
         FOR v_indice = 1 TO v_detalle_docto.getLength()
            LET v_detalle_docto[v_indice].v_numero = v_indice
            IF(v_detalle_docto[v_indice].v_documento CLIPPED IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture documento","info")
               CALL FGL_SET_ARR_CURR(v_actual)
               NEXT FIELD tedi_docto
            END IF
         END FOR
         LET v_actual = ARR_CURR() + 1
         CALL v_detalle_docto.insertElement(v_actual)
         LET v_detalle_docto[v_actual].v_requerido = 0
         # se indica la posision que le pertenece
         LET v_detalle_docto[v_actual].v_numero = v_actual
         FOR v_indice = v_actual TO v_detalle_docto.getLength()
            LET v_detalle_docto[v_indice].v_numero = v_indice
         END FOR
         --CALL fn_agrega_docto(v_actual)
         DISPLAY v_detalle_docto.getLength() TO flbl_total
         LET v_actualiza = TRUE
         CALL FGL_SET_ARR_CURR(v_actual)
         CALL DIALOG.setActionHidden( "aceptar", 0 )
         CALL DIALOG.setActionHidden( "cancelar", 0 )

      ON ACTION eliminar
         IF NOT(v_detalle_docto.getLength() >= 1)THEN
            CONTINUE INPUT
         END IF
         LET v_actual = ARR_CURR()
         # recuper los registros que estan en BD y que seran eliminados
         IF(v_detalle_docto[v_actual].v_docto_cod IS NOT NULL)THEN
            LET v_elim_docto[v_elim_docto.getLength() + 1].* = v_detalle_docto[v_actual].*
         END IF
         CALL v_detalle_docto.deleteElement(v_actual)
         # Actualiza el numero que le corresponde a cada registro y que es el que se despliega en pantalla
         FOR v_indice = v_actual TO v_detalle_docto.getLength()
            LET v_detalle_docto[v_indice].v_numero = v_indice
         END FOR
         # actualiza el numero de registros actuales
         DISPLAY v_detalle_docto.getLength() TO flbl_total
         LET v_actualiza = TRUE
         CALL DIALOG.setActionHidden( "aceptar", 0 )
         CALL DIALOG.setActionHidden( "cancelar", 0 )
         
      ON ACTION cancelar
         IF(v_actualiza)THEN
            CALL fn_ventana_confirma(p_cad_ventana,"¿Desea cancelar modificaciones?","about")
                 RETURNING r_confirma
            IF(r_confirma)THEN             
               EXIT INPUT
            END IF
         ELSE
            EXIT INPUT
         END IF

      ON ACTION salir
         IF(v_actualiza)THEN
            CALL fn_ventana_confirma(p_cad_ventana,"¿Aceptar o Rechazar Cambios al Catálogo?","about")
                 RETURNING r_confirma
            IF(r_confirma)THEN  
               CALL fn_actualiza_docto() RETURNING v_error_sql
               IF(v_error_sql)THEN
                  CALL fn_mensaje("Aviso","Ocurrió un error al actualizar información","information")
               ELSE
                  CALL fn_mensaje("Aviso","Actualización realizada correctamente","information")
                  EXIT INPUT
               END IF
               
            END IF
         ELSE
            EXIT INPUT
         END IF
         
   END INPUT

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEP82                                                    #
#Descripcion       => Realiza la alta, actualizacion y eliminacion en catálogo #
#                     de documentos                                            #
#Autor             => Hugo César Ramírez Gracía                                #
#Fecha inicio      => 01 Mayo 2012                                             #
################################################################################
FUNCTION fn_actualiza_docto()
DEFINE v_indice   INTEGER,
       v_consulta STRING,
       v_documento LIKE sep_cat_docto.docto_desc,
       v_error_sql BOOLEAN

   # si inicializa, suponiendo que no ocurre error
   LET v_error_sql = FALSE
   
   # Elimina todos los registros
   DELETE 
     FROM sep_cat_docto
    WHERE 1 = 1

   # inserta en la tabla según se manipuló el arreglo
   LET v_consulta = "\n INSERT INTO sep_cat_docto(docto_cod,docto_desc,ind_requerido)",
                    "\n  VALUES(?,?,?)"
                    
   PREPARE prp_inserta_docto FROM v_consulta

   FOR v_indice = 1 TO v_detalle_docto.getLength()
      LET v_documento = v_detalle_docto[v_indice].v_documento
      EXECUTE prp_inserta_docto USING v_detalle_docto[v_indice].v_numero, v_documento,
                                      v_detalle_docto[v_indice].v_requerido
      IF(SQLCA.SQLCODE <>0 )THEN
         LET v_error_sql = TRUE
         --CALL fn_mensaje("","Inserta: "||v_detalle_docto[v_indice].v_numero||" - "||v_detalle_docto[v_indice].v_documento|| " con codigo:"||SQLCA.SQLCODE,"")
         DISPLAY "Inserta: "||v_detalle_docto[v_indice].v_numero||" - "||v_detalle_docto[v_indice].v_documento|| " con codigo:"||SQLCA.SQLCODE
      END IF
   END FOR

   RETURN v_error_sql
END FUNCTION