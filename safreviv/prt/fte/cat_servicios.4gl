--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 29/10/2015
--==============================================================================

################################################################################
#Modulo       =>                                                               #
#Programa     =>                                                               #
#Objetivo     => Administración del catálogo de servicios                      #
#Fecha inicio => 29 Octubre 2015                                               #
################################################################################

SCHEMA safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_titulo_ventana  STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ventana         ui.Window,
       v_forma           ui.Form
       
MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)   

   TRY
      CONNECT TO "safre_viv"
      CALL fn_inicializa_consultas()
      CALL fn_administra_cat_servicios()
      DISCONNECT "safre_viv"
   CATCH
      DISCONNECT "safre_viv"
   END TRY
   
END MAIN

# Descripción: inicializa consultas del programa
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   SELECT ruta_bin,
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   LET v_consulta = " SELECT modulo_cod,",
                    "        modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE 1 = 1",
                    "  ORDER BY modulo_cod"
   PREPARE prp_recupera_modulos FROM v_consulta

   LET v_consulta = " SELECT modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE modulo_cod = ?",
                    "  ORDER BY modulo_cod"
   PREPARE prp_recupera_desc_modulo FROM v_consulta

   LET v_consulta = " SELECT id_servicio,",
                    "        servicio_cod,",
                    "        servicio_desc",
                    "   FROM cat_servicio",
                    "  WHERE modulo_cod = ?",
                    "  ORDER BY servicio_cod"
   PREPARE prp_recupera_servicios FROM v_consulta

   LET v_consulta = " SELECT id_servicio,",
                    "        servicio_cod,",
                    "        modulo_cod,",
                    "        servicio_desc,",
                    "        url,",
                    "        archivo_conf,",
                    "        registra_peticion",
                    "   FROM cat_servicio",
                    "  WHERE id_servicio = ?"
   PREPARE prp_recupera_servicio FROM v_consulta

   LET v_consulta = " UPDATE cat_servicio",
                    "    SET servicio_desc = ?,",
                    "        url = ?,",
                    "        archivo_conf = ?,",
                    "        registra_peticion = ?,",
                    "        f_actualiza = ?,",
                    "        usuario_cod = ?",
                    "  WHERE id_servicio = ?"
   PREPARE prp_actualiza_servicio FROM v_consulta

   LET v_consulta = " DELETE",
                    "   FROM cat_servicio",
                    "  WHERE id_servicio = ?"
   PREPARE prp_elimina_servicio FROM v_consulta

   LET v_consulta = " INSERT INTO cat_servicio",
                    " (id_servicio,",
                    "  servicio_cod,",
                    "  modulo_cod,",
                    "  servicio_desc,",
                    "  url,",
                    "  archivo_conf,",
                    "  registra_peticion,",
                    "  f_actualiza,",
                    "  usuario_cod)",
                    " VALUES(seq_cat_servicio.nextval,?,?,?,?,?,?,?,?)"
   PREPARE prp_registra_servicio FROM v_consulta

   LET v_consulta = " SELECT MAX(servicio_cod)+1",
                    "   FROM cat_servicio",
                    "  WHERE modulo_cod = ?"
   PREPARE prp_recupera_cod_servicio FROM v_consulta

   LET v_consulta = " SELECT serie",
                    "   FROM cat_servicio_serie",
                    "  WHERE modulo_cod = ?"
   PREPARE prp_rec_serie_servicio FROM v_consulta
   
END FUNCTION

# Descripción: Muestra pantalla principal de administración de catálogo de servicios
FUNCTION fn_administra_cat_servicios()
DEFINE v_modulo LIKE seg_modulo.modulo_cod,
       v_servicios DYNAMIC ARRAY OF RECORD
          v_id_servicio   LIKE cat_servicio.id_servicio,
          v_servicio_cod  LIKE cat_servicio.servicio_cod,
          v_servicio_desc LIKE cat_servicio.servicio_desc
       END RECORD,
       r_servicio RECORD
          v_id_servicio     LIKE cat_servicio.id_servicio,
          v_servicio_cod    LIKE cat_servicio.servicio_cod,
          v_modulo_cod      LIKE cat_servicio.modulo_cod,
          v_servicio_desc   LIKE cat_servicio.servicio_desc,
          v_url             LIKE cat_servicio.url,
          v_archivo_conf    LIKE cat_servicio.archivo_conf,
          v_registra_log    LIKE cat_servicio.registra_peticion
       END RECORD,
       v_combo_modulo ui.ComboBox,
       r_actualiza BOOLEAN,
       r_error     BOOLEAN,
       r_confirma  BOOLEAN,
       v_indice    INTEGER

   OPEN WINDOW vtna_servicios WITH FORM v_ruta_ejecutable CLIPPED||"/cat_servicios01"

      DIALOG ATTRIBUTE (UNBUFFERED)

         INPUT v_modulo FROM filtro_modulo_cod

            ON CHANGE filtro_modulo_cod
               INITIALIZE r_servicio.* TO NULL
               # Recupera los servicios para el módulo
               CALL fn_recupera_servicios(v_modulo) RETURNING v_servicios
               CALL fn_muestra_servicio(r_servicio.*)
               IF(v_servicios.getLength() = 0)THEN
                  CALL DIALOG.setActionActive("modificar",FALSE)
                  CALL DIALOG.setActionActive("eliminar",FALSE)
               ELSE
                  CALL DIALOG.setActionActive("modificar",TRUE)
                  CALL DIALOG.setActionActive("eliminar",TRUE)
               END IF

         END INPUT

         DISPLAY ARRAY v_servicios TO sr_servicios.*

            BEFORE ROW
               CALL fn_recupera_servicio(v_servicios[ARR_CURR()].v_id_servicio) RETURNING r_servicio.*
               CALL fn_muestra_servicio(r_servicio.*)

         END DISPLAY

         BEFORE DIALOG         
            IF(p_titulo_ventana IS NOT NULL)THEN
               CALL ui.Interface.setText(p_titulo_ventana)
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo_ventana)
               LET v_forma = v_ventana.getForm()
            END IF                                             
            LET v_combo_modulo = ui.ComboBox.forName("formonly.filtro_modulo_cod")
            CALL fn_llena_combo_modulo(v_combo_modulo)
            CALL DIALOG.setActionActive("modificar",FALSE)
            CALL DIALOG.setActionActive("eliminar",FALSE)

         ON ACTION nuevo
            IF( v_modulo IS NOT NULL )THEN
               CALL fn_agrega_servicio(v_modulo) RETURNING r_actualiza
               IF( r_actualiza )THEN
                  CALL fn_recupera_servicios(v_modulo) RETURNING v_servicios
                  CALL fn_recupera_servicio(v_servicios[ARR_CURR()].v_id_servicio) RETURNING r_servicio.*
                  CALL fn_muestra_servicio(r_servicio.*)
               END IF
            ELSE
               CALL fn_mensaje(p_titulo_ventana,"Seleccione un módulo","information")
            END IF

         ON ACTION modificar
            LET v_indice = DIALOG.getCurrentRow("sr_servicios")
            CALL fn_recupera_servicio(v_servicios[v_indice].v_id_servicio) RETURNING r_servicio.*
            CALL fn_muestra_servicio(r_servicio.*)
            CALL fn_modifica_servicio(r_servicio.*)
            CALL fn_recupera_servicios(v_modulo) RETURNING v_servicios
            CALL fn_recupera_servicio(v_servicios[v_indice].v_id_servicio) RETURNING r_servicio.*
            CALL fn_muestra_servicio(r_servicio.*)

         ON ACTION eliminar
            CALL fn_ventana_confirma(p_titulo_ventana,"¿Eliminar servicio '"||v_servicios[ARR_CURR()].v_servicio_desc||"'?","question") RETURNING r_confirma
            IF( r_confirma )THEN
               CALL fn_elimina_servicio(v_servicios[ARR_CURR()].v_id_servicio) RETURNING r_error
               IF( r_error )THEN
                  CALL fn_mensaje(p_titulo_ventana,"Ocurrió un error al eliminar el registro","information")
               ELSE
                  CALL fn_mensaje(p_titulo_ventana,"¡Registro eliminado correctamente!","information")
               END IF
            END IF
            CALL fn_recupera_servicios(v_modulo) RETURNING v_servicios
            CALL fn_recupera_servicio(v_servicios[ARR_CURR()].v_id_servicio) RETURNING r_servicio.*
            CALL fn_muestra_servicio(r_servicio.*)
               
         ON ACTION cancelar
            CANCEL DIALOG

      END DIALOG

   CLOSE WINDOW vtna_servicios

END FUNCTION

# Descripción: Actualiza los campo que muestran el detalle del servicio
FUNCTION fn_muestra_servicio(p_servicio)
DEFINE p_servicio RECORD
          v_id_servicio     LIKE cat_servicio.id_servicio,
          v_servicio_cod    LIKE cat_servicio.servicio_cod,
          v_modulo_cod      LIKE cat_servicio.modulo_cod,
          v_servicio_desc   LIKE cat_servicio.servicio_desc,
          v_url             LIKE cat_servicio.url,
          v_archivo_conf    LIKE cat_servicio.archivo_conf,
          v_registra_log    LIKE cat_servicio.registra_peticion
       END RECORD

   DISPLAY p_servicio.v_servicio_cod  TO con_servicio_cod
   DISPLAY p_servicio.v_servicio_desc TO con_servicio_desc
   DISPLAY p_servicio.v_url           TO con_url
   DISPLAY p_servicio.v_archivo_conf  TO con_archivo_conf
   DISPLAY p_servicio.v_registra_log  TO con_registra_log
                  
END FUNCTION

# Descripción: Llena combo de módulos
FUNCTION fn_llena_combo_modulo(p_combo_modulo)
DEFINE p_combo_modulo ui.ComboBox,
       v_modulos RECORD
          v_modulo_cod  LIKE seg_modulo.modulo_cod,
          v_modulo_desc LIKE seg_modulo.modulo_desc
       END RECORD

   DECLARE cur_recupera_modulos CURSOR FOR prp_recupera_modulos
   FOREACH cur_recupera_modulos INTO v_modulos.*
      CALL p_combo_modulo.addItem(v_modulos.v_modulo_cod,
                                  v_modulos.v_modulo_desc)
   END FOREACH
   FREE cur_recupera_modulos
END FUNCTION 

# Descripción: Recupera los servicios registrados según el módulo
FUNCTION fn_recupera_servicios(p_modulo_cod)
DEFINE p_modulo_cod LIKE seg_modulo.modulo_cod,
       v_servicios DYNAMIC ARRAY OF RECORD
          v_id_servicio   LIKE cat_servicio.id_servicio,
          v_servicio_cod  LIKE cat_servicio.servicio_cod,
          v_servicio_desc LIKE cat_servicio.servicio_desc
       END RECORD,
       v_indice SMALLINT

   LET v_indice = 1
   DECLARE cur_recupera_servicios CURSOR FOR prp_recupera_servicios
   FOREACH cur_recupera_servicios USING p_modulo_cod
                                   INTO v_servicios[v_indice].*

      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_recupera_servicios

   IF(  v_servicios[v_servicios.getLength()].v_id_servicio IS NULL )THEN
      CALL v_servicios.deleteElement(v_servicios.getLength())
   END IF

   RETURN v_servicios
END FUNCTION 

# Descripción:
FUNCTION fn_recupera_servicio(p_id_servicio)
DEFINE p_id_servicio LIKE cat_servicio.id_servicio,
       v_servicio RECORD
          v_id_servicio     LIKE cat_servicio.id_servicio,
          v_servicio_cod    LIKE cat_servicio.servicio_cod,
          v_modulo_cod      LIKE cat_servicio.modulo_cod,
          v_servicio_desc   LIKE cat_servicio.servicio_desc,
          v_url             LIKE cat_servicio.url,
          v_archivo_conf    LIKE cat_servicio.archivo_conf,
          v_registra_log    LIKE cat_servicio.registra_peticion
       END RECORD
       
   EXECUTE prp_recupera_servicio USING p_id_servicio
                                  INTO v_servicio.*

   RETURN v_servicio.*
END FUNCTION 

# Descripción:
FUNCTION fn_modifica_servicio(p_servicio)
DEFINE p_servicio RECORD
          v_id_servicio     LIKE cat_servicio.id_servicio,
          v_servicio_cod    LIKE cat_servicio.servicio_cod,
          v_modulo_cod      LIKE cat_servicio.modulo_cod,
          v_servicio_desc   LIKE cat_servicio.servicio_desc,
          v_url             LIKE cat_servicio.url,
          v_archivo_conf    LIKE cat_servicio.archivo_conf,
          v_registra_log    LIKE cat_servicio.registra_peticion
       END RECORD,
       r_bnd_error BOOLEAN

   INPUT p_servicio.v_servicio_desc,
         p_servicio.v_url,
         p_servicio.v_archivo_conf,
         p_servicio.v_registra_log
    FROM con_servicio_desc,
         con_url,
         con_archivo_conf,
         con_registra_log ATTRIBUTES(ACCEPT=FALSE, CANCEL=FALSE, UNBUFFERED, WITHOUT DEFAULTS)

      ON ACTION aceptar
         CALL fn_actualiza_servicio(p_servicio.*) RETURNING r_bnd_error
         IF( r_bnd_error )THEN
            CALL fn_mensaje(p_titulo_ventana,"Ocurrió un error al actualizar la información","information")
         ELSE
            CALL fn_mensaje(p_titulo_ventana,"¡Registro actualizado correctamente!","information")
         END IF
         ACCEPT INPUT
      
      ON ACTION cancelar
         EXIT INPUT

   END INPUT

END FUNCTION

# Descripción:
FUNCTION fn_actualiza_servicio(p_servicio)
DEFINE p_servicio RECORD
          v_id_servicio     LIKE cat_servicio.id_servicio,
          v_servicio_cod    LIKE cat_servicio.servicio_cod,
          v_modulo_cod      LIKE cat_servicio.modulo_cod,
          v_servicio_desc   LIKE cat_servicio.servicio_desc,
          v_url             LIKE cat_servicio.url,
          v_archivo_conf    LIKE cat_servicio.archivo_conf,
          v_registra_log    LIKE cat_servicio.registra_peticion
       END RECORD,
       v_fecha_actual DATE,
       v_bnd_error BOOLEAN

   LET v_fecha_actual = TODAY
   TRY
      LET v_bnd_error = FALSE
      EXECUTE prp_actualiza_servicio USING p_servicio.v_servicio_desc,
                                           p_servicio.v_url,
                                           p_servicio.v_archivo_conf,
                                           p_servicio.v_registra_log,
                                           v_fecha_actual,
                                           p_usuario_cod,
                                           p_servicio.v_id_servicio
   CATCH
      DISPLAY "Ocurrió un error al actualizar el catálogo de servicios"
      DISPLAY "Id: ",p_servicio.v_id_servicio
      DISPLAY "Código: ",SQLCA.sqlcode
      DISPLAY "Mensaje: ",SQLCA.sqlerrm
      LET v_bnd_error = TRUE
   END TRY

   RETURN v_bnd_error
END FUNCTION

# Descripción:
FUNCTION fn_agrega_servicio(p_modulo)
DEFINE p_modulo   LIKE cat_servicio.modulo_cod,
       v_servicio RECORD
          v_id_servicio     LIKE cat_servicio.id_servicio,
          v_servicio_cod    LIKE cat_servicio.servicio_cod,
          v_modulo_cod      LIKE cat_servicio.modulo_cod,
          v_servicio_desc   LIKE cat_servicio.servicio_desc,
          v_url             LIKE cat_servicio.url,
          v_archivo_conf    LIKE cat_servicio.archivo_conf,
          v_registra_log    LIKE cat_servicio.registra_peticion
       END RECORD,
       r_bnd_error BOOLEAN,
       v_registra  BOOLEAN

   INPUT v_servicio.v_servicio_desc,
         v_servicio.v_url,
         v_servicio.v_archivo_conf,
         v_servicio.v_registra_log
    FROM con_servicio_desc,
         con_url,
         con_archivo_conf,
         con_registra_log ATTRIBUTES(ACCEPT=FALSE, CANCEL=FALSE, UNBUFFERED)

      BEFORE INPUT
         DISPLAY "" TO con_servicio_cod
         # Inicializa registro
         LET v_servicio.v_registra_log = 0 # No registra
         LET v_servicio.v_modulo_cod   = p_modulo
         CALL fn_recupera_cod_servicio(p_modulo) RETURNING v_servicio.v_id_servicio
         DISPLAY v_servicio.v_id_servicio TO con_servicio_cod
      
      ON ACTION guardar
         IF( v_servicio.v_servicio_desc IS NULL OR v_servicio.v_servicio_desc = " ")THEN
            CALL fn_mensaje(p_titulo_ventana,"Capture descripción del servicio","information")
            NEXT FIELD con_servicio_desc
         END IF
         IF( v_servicio.v_url IS NULL OR v_servicio.v_url = " ")THEN
            CALL fn_mensaje(p_titulo_ventana,"Capture URL del servicio","information")
            NEXT FIELD con_url
         END IF
         
         CALL fn_registra_servicio(v_servicio.*) RETURNING r_bnd_error
         IF( r_bnd_error )THEN
            CALL fn_mensaje(p_titulo_ventana,"Ocurrió un error al registrar la información","information")
         ELSE
            LET v_registra = TRUE
            CALL fn_mensaje(p_titulo_ventana,"¡Registro agregado correctamente!","information")
         END IF
         ACCEPT INPUT
      
      ON ACTION cancelar
         DISPLAY "" TO con_servicio_cod
         LET v_registra = FALSE
         EXIT INPUT

   END INPUT

   RETURN v_registra
END FUNCTION

# Descripción:
FUNCTION fn_registra_servicio(p_servicio)
DEFINE p_servicio RECORD
          v_id_servicio     LIKE cat_servicio.id_servicio,
          v_servicio_cod    LIKE cat_servicio.servicio_cod,
          v_modulo_cod      LIKE cat_servicio.modulo_cod,
          v_servicio_desc   LIKE cat_servicio.servicio_desc,
          v_url             LIKE cat_servicio.url,
          v_archivo_conf    LIKE cat_servicio.archivo_conf,
          v_registra_log    LIKE cat_servicio.registra_peticion
       END RECORD,
       v_fecha_actual DATE,
       v_bnd_error BOOLEAN

   LET v_fecha_actual = TODAY
   TRY
      LET v_bnd_error = FALSE
      EXECUTE prp_recupera_cod_servicio USING p_servicio.v_modulo_cod
                                         INTO p_servicio.v_servicio_cod
      EXECUTE prp_registra_servicio USING p_servicio.v_servicio_cod,
                                          p_servicio.v_modulo_cod,
                                          p_servicio.v_servicio_desc,
                                          p_servicio.v_url,
                                          p_servicio.v_archivo_conf,
                                          p_servicio.v_registra_log,
                                          v_fecha_actual,
                                          p_usuario_cod
   CATCH
      DISPLAY "Ocurrió un error al registrar el servicio"
      DISPLAY "Código: ",SQLCA.sqlcode
      DISPLAY "Mensaje: ",SQLCA.sqlerrm
      LET v_bnd_error = TRUE
   END TRY

   RETURN v_bnd_error
END FUNCTION

# Descripción:
FUNCTION fn_elimina_servicio(p_id_servicio)
DEFINE p_id_servicio LIKE cat_servicio.id_servicio,
       v_bnd_error BOOLEAN

   TRY
      LET v_bnd_error = FALSE
      
      EXECUTE prp_elimina_servicio USING p_id_servicio
   CATCH
      DISPLAY "Ocurrió un error al eliminar el servicio"
      DISPLAY "Código: ",SQLCA.sqlcode
      DISPLAY "Mensaje: ",SQLCA.sqlerrm
      LET v_bnd_error = TRUE
   END TRY

   RETURN v_bnd_error
END FUNCTION

# Descripción:
FUNCTION fn_recupera_cod_servicio(p_modulo)
DEFINE p_modulo LIKE cat_servicio.modulo_cod,
       v_servicio_cod    LIKE cat_servicio.servicio_cod       

   LET v_servicio_cod = 0
   EXECUTE prp_recupera_cod_servicio USING p_modulo
                                      INTO v_servicio_cod

   IF( v_servicio_cod = 0 OR v_servicio_cod IS NULL )THEN
      LET v_servicio_cod = 0
      EXECUTE prp_rec_serie_servicio USING p_modulo
                                      INTO v_servicio_cod
   END IF

   RETURN v_servicio_cod
END FUNCTION