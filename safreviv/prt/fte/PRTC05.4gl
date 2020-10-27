--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/07/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC05                                                     #
#Objetivo        => Consulta de cifras control portabilidad                    #
#Fecha Inicio    => 07 Mayo 2015                                               #
################################################################################
IMPORT FGL WSHelper
IMPORT com

SCHEMA safre_viv

GLOBALS "PRTW07.inc"
GLOBALS "PRTWS02.inc"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_titulo_ventana  STRING,
       g_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       g_ruta_listados   LIKE seg_modulo.ruta_listados,
       g_ventana         ui.Window,
       g_forma           ui.Form

MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)

   TRY
      CONNECT TO "safre_viv"
      CALL fn_inicializa_consultas()
      CALL fn_filtra_consulta()
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
     INTO g_ruta_ejecutable,
          g_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   LET v_consulta = " SELECT afore_desc",
                    "   FROM cat_afore",
                    "  WHERE afore_cod = ?"
   PREPARE prp_rec_desc_afore FROM v_consulta

   LET v_consulta = "SELECT descripcion_general",
                    "  FROM prt_diagnostico",
                    " WHERE destino_diagnostico = ?",
                    "   AND diagnostico_externo = ?"
   PREPARE prp_rec_desc_diag FROM v_consulta

   CALL com.WebServiceEngine.SetOption( "readwritetimeout", -1 )
END FUNCTION

# Descripción: Filtra consulta de cifras control de procesar
FUNCTION fn_filtra_consulta()
DEFINE v_filtros RECORD
          v_fecha_ini DATE,
          v_fecha_fin DATE,
          v_tipo_solicitud CHAR(3),
          v_diagnostico    CHAR(3),
          v_entidad        CHAR(3)
       END RECORD
       
   OPEN WINDOW vtna_cifras_control WITH FORM g_ruta_ejecutable CLIPPED||"/PRTC051"

      INPUT v_filtros.* FROM fecha_ini,
                             fecha_fin,
                             tipo_solicitud,
                             diagnostico,
                             entidad ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE, CANCEL=FALSE)

         BEFORE INPUT
            LET g_ventana = ui.Window.getCurrent()
            LET g_forma = g_ventana.getForm()               
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)         
               CALL g_ventana.setText(p_titulo_ventana)
            END IF

         ON ACTION consultar
            IF( v_filtros.v_fecha_ini IS NULL AND
                v_filtros.v_fecha_fin IS NULL )THEN
                CALL fn_mensaje(p_titulo_ventana,"Capture rango de fechas","information")
                CONTINUE INPUT
            END IF

            IF( v_filtros.v_fecha_ini > v_filtros.v_fecha_fin )THEN
                CALL fn_mensaje(p_titulo_ventana,"Fecha inicio no puede ser mayor a fecha fin","information")
                CONTINUE INPUT
            END IF

            CALL fn_consulta_cifras_control(v_filtros.*)

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_cifras_control

END FUNCTION

# Descripción: 
FUNCTION fn_consulta_cifras_control(p_filtros)
DEFINE p_filtros RECORD
          v_fecha_ini DATE,
          v_fecha_fin DATE,
          v_tipo_solicitud CHAR(3),
          v_diagnostico    CHAR(3),
          v_entidad        CHAR(3)
       END RECORD,
       v_entidades DYNAMIC ARRAY OF RECORD
          v_cve_entidad VARCHAR(18),
          v_desc_entidad LIKE cat_afore.afore_desc,
          v_detalle_ent CHAR
       END RECORD,
       v_indice SMALLINT,
       v_tam_arr SMALLINT,
       r_estado_ws         INTEGER,
       r_estado_mensaje_ws STRING

   DIALOG ATTRIBUTES(UNBUFFERED)
    
      INPUT ARRAY v_entidades 
        FROM sr_entidades.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)

         ON ACTION consulta_entidad
            CALL fn_consulta_fecha_solicitud(ARR_CURR(),v_entidades[ARR_CURR()].v_desc_entidad)

      END INPUT

      ON ACTION reporte
         CALL fn_reporte_consulta()

      ON ACTION cancelar
         DISPLAY "" TO estado_mensaje_ws
         DISPLAY "" TO motivo_rechazo
         CALL v_entidades.clear()
         EXIT DIALOG
      
      BEFORE DIALOG
         CALL DIALOG.setActionActive("reporte" , TRUE)
         CALL fn_ejecuta_consulta_ws(p_filtros.*) RETURNING r_estado_ws,
                                                            r_estado_mensaje_ws{,
                                                            r_mensaje_ws}
         IF(r_estado_ws = 0)THEN
            CALL v_entidades.clear()
            LET v_tam_arr = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad.getLength() 
            FOR v_indice = 1 TO v_tam_arr
               LET v_entidades[v_indice].v_cve_entidad = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_indice].claveAfore
               EXECUTE prp_rec_desc_afore USING v_entidades[v_indice].v_cve_entidad
                                           INTO v_entidades[v_indice].v_desc_entidad 
            END FOR
            # Oculta botón de reporte si no hay datos
            IF(v_tam_arr = 0)THEN
               CALL DIALOG.setActionActive("reporte" , FALSE)
            END IF
            DISPLAY ns1cifrasControlMarcaDesmarcaExtResponse.ssnrop.descRespuesta TO estado_mensaje_ws
            IF( ns1cifrasControlMarcaDesmarcaExtResponse.ssnrop.motivos.motivo[1].descripcion IS NOT NULL)THEN            
               DISPLAY ns1cifrasControlMarcaDesmarcaExtResponse.ssnrop.motivos.motivo[1].descripcion TO motivo_rechazo
            ELSE
               DISPLAY ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.descripcion TO motivo_rechazo
            END IF
         ELSE
            DISPLAY r_estado_mensaje_ws TO estado_mensaje_ws
            DISPLAY ns1cifrasControlMarcaDesmarcaExtResponse.ssnrop.descRespuesta TO motivo_rechazo
         END IF

   END DIALOG

END FUNCTION

# Descripción: Ejecuta WS de consulta de cifras control
FUNCTION fn_ejecuta_consulta_ws(p_filtros)
DEFINE p_filtros RECORD
          v_fecha_ini DATE,
          v_fecha_fin DATE,
          v_tipo_solicitud CHAR(3),
          v_diagnostico    CHAR(3),
          v_entidad        CHAR(3)
       END RECORD,
       v_idssn RECORD
          v_idSistema      INTEGER,
          v_idEbusiness    INTEGER,
          v_idPortafolio   INTEGER,
          v_idServicio     INTEGER,
          v_idCliente      INTEGER,
          v_idCanal        INTEGER,
          v_codoperCliente VARCHAR(50),
          v_fecha          DATETIME YEAR TO FRACTION(5)
       END RECORD,
       v_cuerpo RECORD
          v_entidad       VARCHAR(3),
          v_fechaInicio   VARCHAR(10),
          v_fechaFin      VARCHAR(10),
          v_tipoSolicitud VARCHAR(2),
          v_diagnostico   VARCHAR(3)
       END RECORD,
       r_estado_ws         INTEGER,
       r_estado_mensaje_ws STRING

   TRY
      WHENEVER ERROR CONTINUE
      # Parametros de WS para tener acceso al WS servidor de procesar
      LET v_idssn.v_idSistema    = 26
      LET v_idssn.v_idEbusiness  = 45
      LET v_idssn.v_idPortafolio = 8
      LET v_idssn.v_idServicio   = 587
      LET v_idssn.v_idCliente    = 44
      LET v_idssn.v_idCanal      = 13
      LET v_idssn.v_codoperCliente = "CODIGO_SAFRE"
      LET v_idssn.v_fecha          = CURRENT YEAR TO FRACTION(5)

      LET v_cuerpo.v_entidad       = p_filtros.v_entidad
      LET v_cuerpo.v_fechaInicio   = p_filtros.v_fecha_ini USING "dd/mm/yyyy"
      LET v_cuerpo.v_fechaFin      = p_filtros.v_fecha_fin USING "dd/mm/yyyy"
      LET v_cuerpo.v_tipoSolicitud = p_filtros.v_tipo_solicitud
      LET v_cuerpo.v_diagnostico   = p_filtros.v_diagnostico
      CALL consultaCifrasProcesarMarca(v_idssn.*,v_cuerpo.*) RETURNING r_estado_ws,
                                                                       r_estado_mensaje_ws{,
                                                                       r_mensaje_ws}
      IF( r_estado_ws <> 0 )THEN
         CALL fn_mensaje("Error de comunicación con WS",
                         r_estado_mensaje_ws,"exclamation")

      END IF

   CATCH
      CALL fn_mensaje(p_titulo_ventana,"Ocurrió un error con el WS","exclamation")
   END TRY
   WHENEVER ERROR STOP

   RETURN r_estado_ws,
          r_estado_mensaje_ws{,
          r_mensaje_ws}
END FUNCTION

# Descripción: Consulta la fecha de solicitud recibida en WS
FUNCTION fn_consulta_fecha_solicitud(p_indice_entidad,p_ruta_actual)
DEFINE p_indice_entidad SMALLINT,
       p_ruta_actual    STRING,
       v_fecha_solicitud DYNAMIC ARRAY OF RECORD
         v_fecha         CHAR(10),
         v_detalle_fecha CHAR
       END RECORD,
       v_indice SMALLINT

   OPEN WINDOW vtna_con_fecha_sol WITH FORM g_ruta_ejecutable CLIPPED||"/PRTC052" ATTRIBUTE(STYLE="dialog")
      INPUT ARRAY v_fecha_solicitud FROM sr_fecha_solicitud.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE,ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE INPUT
            LET g_ventana = ui.Window.getCurrent()
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)         
               CALL g_ventana.setText(p_titulo_ventana)
            END IF
            # Muestra ruta actual
            DISPLAY p_ruta_actual TO ruta_actual
            CALL v_fecha_solicitud.clear()
            # Recupera en otro arreglo, la información de las fechas
            FOR v_indice = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud.getLength()
               LET v_fecha_solicitud[v_indice].v_fecha = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[v_indice].fecha
            END FOR
            IF( v_fecha_solicitud.getLength() = 0 )THEN
               CALL fn_mensaje(p_titulo_ventana,"No hay información","infomation")
               EXIT INPUT
            END IF
         
         ON ACTION consulta_origen_sol
            CALL fn_consulta_origen_solicitud(p_indice_entidad,
                                              ARR_CURR(),
                                              p_ruta_actual CLIPPED||" > "||v_fecha_solicitud[ARR_CURR()].v_fecha)

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_con_fecha_sol

END FUNCTION

# Descripción: Consulta origen de la solicitud del WS
FUNCTION fn_consulta_origen_solicitud(p_indice_entidad,p_indice_fecha,p_ruta_actual)
DEFINE p_indice_entidad SMALLINT,
       p_indice_fecha   SMALLINT,
       p_ruta_actual    STRING,
       v_origen_solicitud DYNAMIC ARRAY OF RECORD
         v_cve_origen     VARCHAR(3),
         v_desc_origen    VARCHAR(18),
         v_detalle_origen CHAR
       END RECORD,
       v_indice SMALLINT

   OPEN WINDOW vtna_con_origen_sol WITH FORM g_ruta_ejecutable CLIPPED||"/PRTC053" ATTRIBUTE(STYLE="dialog")
      INPUT ARRAY v_origen_solicitud FROM sr_origen_solicitud.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE,ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE INPUT
            LET g_ventana = ui.Window.getCurrent()
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)         
               CALL g_ventana.setText(p_titulo_ventana)
            END IF
            # Muestra ruta actual de los arreglos anteriores
            DISPLAY p_ruta_actual TO ruta_actual
            CALL v_origen_solicitud.clear()
            # Recupera en otro arreglo los origenes de la solicitud
            FOR v_indice = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[p_indice_fecha].origenesDeSolicitud.origenDeSolicitud.getLength()
               LET v_origen_solicitud[v_indice].v_cve_origen = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[p_indice_fecha].origenesDeSolicitud.origenDeSolicitud[v_indice].claveOrigen
               # Determina el origen que sólo puede ser INFONAVIT o FOVISSSTE
               CALL fn_recupera_desc_origen_sol(v_origen_solicitud[v_indice].v_cve_origen) RETURNING v_origen_solicitud[v_indice].v_desc_origen 
               
            END FOR
            IF( v_origen_solicitud.getLength() = 0 )THEN
               CALL fn_mensaje(p_titulo_ventana,"No hay información","infomation")
               EXIT INPUT
            END IF
         
         ON ACTION consulta_tipo_sol
            CALL fn_consulta_tipo_solicitud(p_indice_entidad,
                                            p_indice_fecha,
                                            ARR_CURR(),
                                            p_ruta_actual CLIPPED||" > "||v_origen_solicitud[ARR_CURR()].v_desc_origen)

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_con_origen_sol

END FUNCTION

# Descripción: Consulta tipo solicitud de WS
FUNCTION fn_consulta_tipo_solicitud(p_indice_entidad,p_indice_fecha,p_indice_origen,p_ruta_actual)
DEFINE p_indice_entidad SMALLINT,
       p_indice_fecha   SMALLINT,
       p_indice_origen  SMALLINT,
       p_ruta_actual    STRING,
       v_tipo_solicitud DYNAMIC ARRAY OF RECORD
         v_cve_tipo     VARCHAR(3),
         v_desc_tipo    VARCHAR(40),
         v_total_tipo   INTEGER,
         v_detalle_tipo CHAR
       END RECORD,
       v_indice SMALLINT

   OPEN WINDOW vtna_con_tipo_sol WITH FORM g_ruta_ejecutable CLIPPED||"/PRTC054" ATTRIBUTE(STYLE="dialog")
      INPUT ARRAY v_tipo_solicitud FROM sr_tipo_solicitud.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE,ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE INPUT
            LET g_ventana = ui.Window.getCurrent()
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)         
               CALL g_ventana.setText(p_titulo_ventana)
            END IF
            # Muestra ruta actual de arreglos anteriores
            DISPLAY p_ruta_actual TO ruta_actual
            CALL v_tipo_solicitud.clear()
            # Recupera los datos en otro arreglo de la solicitud tipo solicitud
            FOR v_indice = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[p_indice_fecha].origenesDeSolicitud.origenDeSolicitud[p_indice_origen].tiposDeSolicitud.tipoSolicitud.getLength()
               LET v_tipo_solicitud[v_indice].v_cve_tipo   = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[p_indice_fecha].origenesDeSolicitud.origenDeSolicitud[p_indice_origen].tiposDeSolicitud.tipoSolicitud[v_indice].claveTipoSolicitud
               LET v_tipo_solicitud[v_indice].v_total_tipo = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[p_indice_fecha].origenesDeSolicitud.origenDeSolicitud[p_indice_origen].tiposDeSolicitud.tipoSolicitud[v_indice].diagnosticos.totalTipoSolicitud
               CALL fn_recupera_desc_tipo_sol(v_tipo_solicitud[v_indice].v_cve_tipo) RETURNING v_tipo_solicitud[v_indice].v_desc_tipo
               
            END FOR
            IF( v_tipo_solicitud.getLength() = 0 )THEN
               CALL fn_mensaje(p_titulo_ventana,"No hay información","infomation")
               EXIT INPUT
            END IF
         
         ON ACTION consulta_diag
            CALL fn_consulta_diagnostico(p_indice_entidad,
                                         p_indice_fecha,
                                         p_indice_origen,
                                         ARR_CURR(),
                                         p_ruta_actual CLIPPED||" > "||v_tipo_solicitud[ARR_CURR()].v_desc_tipo)

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_con_tipo_sol
   
END FUNCTION

# Descripción: Consulta diagnóstico WS
FUNCTION fn_consulta_diagnostico(p_indice_entidad,p_indice_fecha,p_indice_origen,p_indice_tipo,p_ruta_actual)
DEFINE p_indice_entidad SMALLINT,
       p_indice_fecha   SMALLINT,
       p_indice_origen  SMALLINT,
       p_indice_tipo    SMALLINT,
       p_ruta_actual    STRING,
       v_diagnostico DYNAMIC ARRAY OF RECORD
         v_cve_diag VARCHAR(3),
         v_desc_diag VARCHAR(100),
         v_total_diag  INTEGER
       END RECORD,
       v_indice SMALLINT

   OPEN WINDOW vtna_con_diagnostico WITH FORM g_ruta_ejecutable CLIPPED||"/PRTC055" ATTRIBUTE(STYLE="dialog")
      DISPLAY ARRAY v_diagnostico TO sr_diagnostico.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE DISPLAY
            LET g_ventana = ui.Window.getCurrent()
            IF( p_titulo_ventana IS NOT NULL )THEN
               CALL ui.Interface.setText(p_titulo_ventana)         
               CALL g_ventana.setText(p_titulo_ventana)
            END IF
            # Muestra la ruta actual de los arreglos
            DISPLAY p_ruta_actual TO ruta_actual
            CALL v_diagnostico.clear()
            # Recupera los datos en otro arreglo del diagnóstico
            FOR v_indice = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[p_indice_fecha].origenesDeSolicitud.origenDeSolicitud[p_indice_origen].tiposDeSolicitud.tipoSolicitud[p_indice_tipo].diagnosticos.diagnostico.getLength()
               LET v_diagnostico[v_indice].v_cve_diag = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[p_indice_fecha].origenesDeSolicitud.origenDeSolicitud[p_indice_origen].tiposDeSolicitud.tipoSolicitud[p_indice_tipo].diagnosticos.diagnostico[v_indice].diagnosticoSolicitud
               LET v_diagnostico[v_indice].v_total_diag  = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[p_indice_entidad].fechasDeSolicitud.fechaSolicitud[p_indice_fecha].origenesDeSolicitud.origenDeSolicitud[p_indice_origen].tiposDeSolicitud.tipoSolicitud[p_indice_tipo].diagnosticos.diagnostico[v_indice].totalDiagnosticoSolicitud
               EXECUTE prp_rec_desc_diag USING C_DESTINO_DIAG_P_I,
                                               v_diagnostico[v_indice].v_cve_diag 
                                          INTO v_diagnostico[v_indice].v_desc_diag
            END FOR
            IF( v_diagnostico.getLength() = 0 )THEN
               CALL fn_mensaje(p_titulo_ventana,"No hay información","infomation")
               EXIT DISPLAY
            END IF

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_con_diagnostico
   
END FUNCTION 

# Descripción: Recupera la descripción del tipo de solicitud
FUNCTION fn_recupera_desc_tipo_sol(p_cve_tipo_sol)
DEFINE p_cve_tipo_sol  VARCHAR(18),
       v_desc_tipo_sol VARCHAR(25)

   CASE p_cve_tipo_sol
      WHEN "01"
         LET v_desc_tipo_sol = "Crédito nuevo"

      WHEN "02"
         LET v_desc_tipo_sol = "Crédito existente"

      WHEN "03"
         LET v_desc_tipo_sol = "Desmarca de portabilidad"

   END CASE

   RETURN v_desc_tipo_sol
END FUNCTION 

# Descripción: Recupera la descripción del tipo de solicitud
FUNCTION fn_recupera_desc_origen_sol(p_cve_origen_sol)
DEFINE p_cve_origen_sol  VARCHAR(10),
       v_desc_origen_sol VARCHAR(15)
       
   CASE p_cve_origen_sol

      WHEN "001"
         LET v_desc_origen_sol = "INFONAVIT"

      WHEN "002"
         LET v_desc_origen_sol = "FOVISSSTE"

      OTHERWISE
         LET v_desc_origen_sol = "DESCONOCIDO"
         
   END CASE

   RETURN v_desc_origen_sol
END FUNCTION

FUNCTION fn_reporte_consulta()
DEFINE v_nom_reporte STRING,
       v_manejador_rpt OM.SaxDocumentHandler,
       v_idx_entidad   SMALLINT,
       v_idx_fecha     SMALLINT,
       v_idx_origen    SMALLINT,
       v_idx_tipo      SMALLINT,
       v_idx_diag      SMALLINT,
       v_desc_entidad  LIKE cat_afore.afore_desc,
       v_cve_entidad   LIKE cat_afore.afore_cod,
       v_desc_origen   VARCHAR(15),
       v_desc_tipo     VARCHAR(25),
       v_cve_diag      VARCHAR(15),
       v_desc_diag     VARCHAR(50)

   IF( fgl_report_loadCurrentSettings(g_ruta_ejecutable CLIPPED ||"/PRTC051.4rp") )THEN
      CALL fgl_report_selectDevice("PDF")

      LET v_nom_reporte = "cifras_control.pdf"

      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(g_ruta_listados CLIPPED||"/"||v_nom_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(1)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      START REPORT fn_rpt_cifras_control TO XML HANDLER v_manejador_rpt         
         FOR v_idx_entidad = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad.getLength()
            LET v_cve_entidad = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].claveAfore
            EXECUTE prp_rec_desc_afore USING v_cve_entidad
                                        INTO v_desc_entidad
            FOR v_idx_fecha = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud.getLength()
               FOR v_idx_origen = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud.getLength()
                  CALL fn_recupera_desc_origen_sol(ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].claveOrigen)
                  RETURNING v_desc_origen
                  FOR v_idx_tipo = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].tiposDeSolicitud.tipoSolicitud.getLength()                     
                     CALL fn_recupera_desc_tipo_sol(ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].tiposDeSolicitud.tipoSolicitud[v_idx_tipo].claveTipoSolicitud) 
                     RETURNING v_desc_tipo
                     FOR v_idx_diag = 1 TO ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].tiposDeSolicitud.tipoSolicitud[v_idx_tipo].diagnosticos.diagnostico.getLength()
                        LET v_cve_diag = ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].tiposDeSolicitud.tipoSolicitud[v_idx_tipo].diagnosticos.diagnostico[v_idx_diag].diagnosticoSolicitud  
                        EXECUTE prp_rec_desc_diag USING C_DESTINO_DIAG_P_I,
                                                        v_cve_diag
                                                   INTO v_desc_diag
                        OUTPUT TO REPORT fn_rpt_cifras_control(ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].claveAfore,
                                                               v_desc_entidad,
                                                               ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].fecha,
                                                               ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].claveOrigen,
                                                               v_desc_origen,
                                                               ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].tiposDeSolicitud.tipoSolicitud[v_idx_tipo].claveTipoSolicitud,
                                                               v_desc_tipo,
                                                               ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].tiposDeSolicitud.tipoSolicitud[v_idx_tipo].diagnosticos.diagnostico[v_idx_diag].diagnosticoSolicitud,
                                                               v_desc_diag,
                                                               ns1cifrasControlMarcaDesmarcaExtResponse.objetoRespuesta.entidades.entidad[v_idx_entidad].fechasDeSolicitud.fechaSolicitud[v_idx_fecha].origenesDeSolicitud.origenDeSolicitud[v_idx_origen].tiposDeSolicitud.tipoSolicitud[v_idx_tipo].diagnosticos.diagnostico[v_idx_diag].totalDiagnosticoSolicitud)
                     END FOR
                  END FOR
               END FOR
            END FOR
         END FOR
      FINISH REPORT fn_rpt_cifras_control
   ELSE
      CALL fn_mensaje(p_titulo_ventana,"No fue posible generar el reporte","information")     
   END IF

END FUNCTION

# Descripción: 
REPORT fn_rpt_cifras_control(p_cve_afore,
                             p_desc_afore,
                             p_fecha_sol,
                             p_cve_origen,
                             p_desc_origen,
                             p_cve_tipo_sol,
                             p_desc_tipo_sol,
                             p_cve_diagnostico,
                             p_desc_diagnostico,
                             p_total)
DEFINE p_cve_afore   LIKE cat_afore.afore_cod,
       p_desc_afore  LIKE cat_afore.afore_desc,
       p_fecha_sol        VARCHAR(10),
       p_cve_origen       VARCHAR(15),
       p_desc_origen      VARCHAR(15),
       p_cve_tipo_sol     VARCHAR(15),
       p_desc_tipo_sol    VARCHAR(25),
       p_cve_diagnostico  VARCHAR(15),
       p_desc_diagnostico VARCHAR(50),
       p_total            VARCHAR(4),
       v_pagina           SMALLINT,
       v_fecha_actual     CHAR(10)

   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_actual = TODAY USING "dd/mm/yyyy"
         PRINTX p_usuario_cod,
                v_fecha_actual 

      BEFORE GROUP OF p_cve_afore
         PRINTX p_desc_afore

      BEFORE GROUP OF p_fecha_sol
         PRINTX p_fecha_sol

      BEFORE GROUP OF p_cve_origen
         PRINTX p_desc_origen

      BEFORE GROUP OF p_cve_tipo_sol
         PRINTX p_desc_tipo_sol
   
      ON EVERY ROW
         PRINTX p_cve_diagnostico,
                p_desc_diagnostico,
                p_total

      PAGE TRAILER
         # imprime número de la página
         LET v_pagina = PAGENO 
         PRINTX v_pagina

END REPORT