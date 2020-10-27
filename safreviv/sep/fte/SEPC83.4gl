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
       v_tipo_caso   DYNAMIC ARRAY OF RECORD
         v_canal_cod     LIKE sep_cat_canal_recepcion_exp.canal_cod,
         v_numero        INTEGER,         
         v_caso          LIKE sep_cat_canal_recepcion_exp.canal_desc
       END RECORD,
       v_elim_caso   DYNAMIC ARRAY OF RECORD
         v_canal_cod  LIKE sep_cat_canal_recepcion_exp.canal_cod,
         v_numero     INTEGER,         
         v_caso       LIKE sep_cat_canal_recepcion_exp.canal_desc
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

   OPEN WINDOW vtna_tipo_caso WITH FORM v_ruta_ejecutable CLIPPED||"/SEPC831"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      DISPLAY ARRAY v_tipo_caso TO sr_caso.* 
                 ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         ON ACTION consultar
            CALL fn_recupera_casos()
            IF NOT(v_tipo_caso.getLength() > 0)THEN
               CALL fn_mensaje(p_cad_ventana,"No hay información","info")
            END IF
            DISPLAY v_tipo_caso.getLength() TO flbl_total

         ON ACTION modificar
            # se inicializa para actualizar, comenzando como false para indicar que no hay actualizacion
            LET v_actualiza = FALSE
            # se revisa que exista informacion
            IF(v_tipo_caso.getLength() > 0)THEN            
               CALL fn_modifica_caso()
               CALL v_tipo_caso.clear()
               --ACCEPT DISPLAY
            ELSE
               CALL fn_mensaje(p_cad_ventana,"No hay información","info")
            END IF 

         ON ACTION salir
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_tipo_caso

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC83                                                   #
#Descripcion       => Recupera el catálogo de tipos de casos                   #
#Autor             => Hugo César Ramírez Gracía                                #
#Fecha inicio      => 01 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_casos()
DEFINE v_caso       RECORD
         v_numero    LIKE sep_cat_canal_recepcion_exp.canal_cod,
         v_caso      LIKE sep_cat_canal_recepcion_exp.canal_desc
       END RECORD,
       v_indice      INTEGER

   WHENEVER ERROR CONTINUE
   CALL v_tipo_caso.clear()
   LET v_indice = 1
   
   DECLARE cur_rec_caso CURSOR FOR 
    SELECT canal_cod, canal_desc
      FROM sep_cat_canal_recepcion_exp
      ORDER BY canal_cod

   FOREACH cur_rec_caso INTO v_caso.*
      LET v_tipo_caso[v_indice].v_numero    = v_indice
      LET v_tipo_caso[v_indice].v_canal_cod = v_caso.v_numero  
      LET v_tipo_caso[v_indice].v_caso      = v_caso.v_caso
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_caso

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC83                                                   #
#Descripcion       => Mantenimiento a catálogo de tipo de casos                #
#Autor             => Hugo César Ramírez Gracía                                #
#Fecha inicio      => 01 Mayo 2012                                             #
################################################################################
FUNCTION fn_modifica_caso()
DEFINE v_indice   INTEGER,
       v_actual   INTEGER,
       r_confirma BOOLEAN,
       v_aux_caso    RECORD
         v_canal_cod LIKE sep_cat_canal_recepcion_exp.canal_cod,
         v_numero    INTEGER,         
         v_caso      LIKE sep_cat_canal_recepcion_exp.canal_desc
       END RECORD,
       v_error_sql BOOLEAN

   INPUT ARRAY v_tipo_caso WITHOUT DEFAULTS FROM sr_caso.* 
                 ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
      BEFORE INPUT
         CALL DIALOG.setActionHidden( "aceptar", 1 )
         CALL DIALOG.setActionHidden( "cancelar", 1 )
         DISPLAY v_tipo_caso.getLength() TO flbl_total

      AFTER FIELD tedi_tipo_caso
         IF(LENGTH(v_tipo_caso[ARR_CURR()].v_caso CLIPPED) = 0)THEN
            CALL fn_mensaje("Aviso","Capture tipo caso","info")
            NEXT FIELD tedi_tipo_caso
         END IF

      ON CHANGE tedi_tipo_caso
         CALL DIALOG.setActionHidden( "aceptar", 0 )
         CALL DIALOG.setActionHidden( "cancelar", 0 )
         LET v_actualiza = TRUE

      ON ACTION aceptar
         IF(v_actualiza)THEN
            # se valida que no haya registros nulos
            FOR v_indice = 1 TO v_tipo_caso.getLength()
               LET v_tipo_caso[v_indice].v_numero = v_indice
               IF(v_tipo_caso[v_indice].v_caso CLIPPED IS NULL)THEN
                  CALL fn_mensaje("Aviso","Capture tipo caso","info")
                  CALL FGL_SET_ARR_CURR(v_actual)
                  NEXT FIELD tedi_tipo_caso
               END IF
            END FOR
            CALL fn_ventana_confirma(p_cad_ventana,"¿Desea aceptar las modificaciones?","about")
                 RETURNING r_confirma
            IF(r_confirma)THEN  
               CALL fn_actualiza_caso() RETURNING v_error_sql
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
         FOR v_indice = 1 TO v_tipo_caso.getLength()
            LET v_tipo_caso[v_indice].v_numero = v_indice
            IF(v_tipo_caso[v_indice].v_caso CLIPPED IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture tipo caso","info")
               CALL FGL_SET_ARR_CURR(v_actual)
               NEXT FIELD tedi_tipo_caso
            END IF
         END FOR
         --LET v_actual = ARR_CURR() + 1
         LET v_actual = v_tipo_caso.getLength() + 1
         CALL v_tipo_caso.insertElement(v_actual)
         LET v_tipo_caso[v_actual].v_numero = v_actual
         {FOR v_indice = v_actual TO v_tipo_caso.getLength()
            LET v_tipo_caso[v_indice].v_numero = v_indice
         END FOR}
         --CALL fn_agrega_docto(v_actual)
         DISPLAY v_tipo_caso.getLength() TO flbl_total
         LET v_actualiza = TRUE
         CALL FGL_SET_ARR_CURR(v_actual)
         CALL DIALOG.setActionHidden( "aceptar", 0 )
         CALL DIALOG.setActionHidden( "cancelar", 0 )

      ON ACTION eliminar
         IF NOT(v_tipo_caso.getLength() >= 1)THEN
            CONTINUE INPUT
         END IF
         LET v_actual = ARR_CURR()
         # recuper los registros que estan en BD y que seran eliminados
         IF(v_tipo_caso[v_actual].v_canal_cod IS NOT NULL)THEN
            LET v_elim_caso[v_elim_caso.getLength() + 1].* = v_tipo_caso[v_actual].*
         END IF
         CALL v_tipo_caso.deleteElement(v_actual)
         # Actualiza el numero que le corresponde a cada registro y que es el que se despliega en pantalla
         FOR v_indice = v_actual TO v_tipo_caso.getLength()
            LET v_tipo_caso[v_indice].v_numero = v_indice
         END FOR
         # actualiza el numero de registros actuales
         DISPLAY v_tipo_caso.getLength() TO flbl_total
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
               CALL fn_actualiza_caso() RETURNING v_error_sql
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

      ON ACTION btn_subir
         IF(ARR_CURR() > 1)THEN
            --LET v_aux_caso.v_numero = v_tipo_caso[ARR_CURR()].v_numero
            LET v_aux_caso.v_caso   = v_tipo_caso[ARR_CURR()].v_caso
            
            --LET v_tipo_caso[ARR_CURR()].v_numero = v_tipo_caso[ARR_CURR() - 1].v_numero
            LET v_tipo_caso[ARR_CURR()].v_caso   = v_tipo_caso[ARR_CURR() - 1].v_caso
            
            --LET v_tipo_caso[ARR_CURR() - 1].v_numero = v_aux_caso.v_numero
            LET v_tipo_caso[ARR_CURR() - 1].v_caso   = v_aux_caso.v_caso
            CALL FGL_SET_ARR_CURR(ARR_CURR() - 1)
            LET v_actualiza = TRUE  
            CALL DIALOG.setActionHidden( "aceptar", 0 )
            CALL DIALOG.setActionHidden( "cancelar", 0 )
         END IF

      ON ACTION btn_bajar
         IF(ARR_CURR() < v_tipo_caso.getLength())THEN
            --LET v_aux_caso.v_numero = v_tipo_caso[ARR_CURR()].v_numero
            LET v_aux_caso.v_caso   = v_tipo_caso[ARR_CURR()].v_caso
            
            --LET v_tipo_caso[ARR_CURR()].v_numero = v_tipo_caso[ARR_CURR() + 1].v_numero
            LET v_tipo_caso[ARR_CURR()].v_caso   = v_tipo_caso[ARR_CURR() + 1].v_caso
            
            --LET v_tipo_caso[ARR_CURR() + 1].v_numero = v_aux_caso.v_numero
            LET v_tipo_caso[ARR_CURR() + 1].v_caso   = v_aux_caso.v_caso
            
            CALL FGL_SET_ARR_CURR(ARR_CURR() + 1)
            LET v_actualiza = TRUE
            CALL DIALOG.setActionHidden( "aceptar", 0 )
            CALL DIALOG.setActionHidden( "cancelar", 0 ) 
         END IF
         
   END INPUT

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEP82                                                    #
#Descripcion       => Realiza la alta, actualizacion y eliminacion en catálogo #
#                     de tipo de caso                                          #
#Autor             => Hugo César Ramírez Gracía                                #
#Fecha inicio      => 01 Mayo 2012                                             #
################################################################################
FUNCTION fn_actualiza_caso()
DEFINE v_indice   INTEGER,
       v_consulta STRING,
       v_error_sql BOOLEAN

   WHENEVER ERROR CONTINUE

   LET v_error_sql = FALSE
   
   DELETE 
     FROM sep_cat_canal_recepcion_exp
    WHERE 1 = 1
    

   LET v_consulta = "\n INSERT INTO sep_cat_canal_recepcion_exp(canal_cod,canal_desc)",
                    "\n  VALUES(?,?)"
                    
   PREPARE prp_inserta_caso FROM v_consulta

   # se recorre el arreglo para realizar tanto updates e inserts
   # se realiza update a todos los registros
   FOR v_indice = 1 TO v_tipo_caso.getLength()
      EXECUTE prp_inserta_caso USING v_tipo_caso[v_indice].v_numero, 
                                     v_tipo_caso[v_indice].v_caso
      IF(SQLCA.SQLCODE <> 0 )THEN
         LET v_error_sql = TRUE
         DISPLAY "Inserta: "||v_tipo_caso[v_indice].v_numero||" - "||v_tipo_caso[v_indice].v_caso||" con codigo"||SQLCA.SQLCODE
      END IF      
      --CALL fn_mensaje("","Inserta: "||v_tipo_caso[v_indice].v_numero||" - "||v_tipo_caso[v_indice].v_caso,"")
   END FOR
   
   RETURN v_error_sql
END FUNCTION