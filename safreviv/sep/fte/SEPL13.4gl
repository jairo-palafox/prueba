--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 08/05/2012
--===============================================================
DATABASE safre_viv

DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion INTEGER,
       p_cad_ventana   STRING,
       g_proceso_cod   LIKE cat_proceso.proceso_cod,
       v_opera_cod_int LIKE cat_operacion.opera_cod,
       g_opera_cod_val LIKE cat_operacion.opera_cod,
       g_pid           LIKE glo_pid.pid,
       v_archivos_pendientes DYNAMIC ARRAY OF STRING,
       v_archivos_elegidos   DYNAMIC ARRAY OF STRING,
       v_dragdrop            UI.DRAGDROP, # manejador de arrastrar y soltar (drag and drop)
       v_posibles_cuentas DYNAMIC ARRAY OF RECORD
         v_resultado_operacion LIKE sep_cat_resultado_operacion.resultado_operacion_desc,
         v_diag_confronta      LIKE sep_cat_diag_confronta.diag_confronta_desc,
         v_clasificacion       LIKE sep_cat_clasificacion.clasificacion_desc,
         v_total               INTEGER
       END RECORD,
       v_nss_asociado DYNAMIC ARRAY OF RECORD
          v_resultado_operacion LIKE sep_cat_resultado_operacion.resultado_operacion_desc,
          v_tpo_entidad         LIKE sep_cat_tipo_entidad_asociado.tpo_entidad_desc,
          v_entidad_asociado    LIKE sep_det_03_op27.cve_entidad_asociado,
          v_total               INTEGER
       END RECORD,
       v_total_posibles_cuentas INTEGER,
       v_total_nss_asociado     INTEGER,
       v_ruta_ejecutable        LIKE seg_modulo.ruta_bin

MAIN

   {
    Se recuperan los parametros recibidos
    Clave de usuario
    Tipo de ejecucion (en línea o batch)
    Cadena que identifica al programa (lo que aparecería como título de la ventana)
   }
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   
   CALL fn_integra_diagnostico_op27()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL13                                                   #
#Descripcion       => Integra diagnóstico de operación 27                      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Mayo 2012                                             #
################################################################################
FUNCTION fn_integra_diagnostico_op27()
DEFINE 
       v_ventana         ui.Window,
       v_forma           ui.Form,
       r_valida          SMALLINT,
       v_arrastra_origen STRING,
       v_indice_arrastre INTEGER,
       v_indice_suelta   INTEGER,
       v_valor_arrastre  STRING,
       v_can_int         BOOLEAN,
       v_continua        BOOLEAN,
       v_renglon_actual  INTEGER,
       r_error           BOOLEAN

       CONSTANT v_sr_pendientes = "sr_pendientes" # Se asigna el nombre de la tabla de archivos pendientes
       CONSTANT v_sr_elegido    = "sr_elegido" # Se asigna el nombre de la tabla de archivos elegidos

   LET g_proceso_cod   = 2204 # Proceso diagnóstico Op. 27
   LET v_opera_cod_int = 2    # Integración de diagnóstico Operación 27
   LET g_opera_cod_val = 1    # Validación de diagnóstico Operación 27

   # se recupera la ruta ejecutable del módulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   --CALL fn_max_pid(g_proceso_cod,g_opera_cod_val) RETURNING g_pid
   
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
      AND estado_cod = 2 # procesando
      
   CALL fn_valida_operacion(g_pid,g_proceso_cod,v_opera_cod_int) RETURNING r_valida
   # Se verifica si la operacion es valida
   IF(r_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_valida)
      EXIT PROGRAM
   END IF
   LET v_continua = TRUE

   OPEN WINDOW vtna_integra_diagnostico_op27 WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL131"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      WHILE (v_continua)
         DIALOG ATTRIBUTE(UNBUFFERED)

            DISPLAY ARRAY v_archivos_pendientes TO sr_pendientes.*

               ON DRAG_START(v_dragdrop)
                  # se recupera tabla origen de arrastre
                  LET v_arrastra_origen = v_sr_pendientes
                  # se recupera el indice de la tabla origen de arrastre
                  LET v_indice_arrastre = ARR_CURR()
                  # se recupera el valor de la tabla origen de arrastre
                  LET v_valor_arrastre = v_archivos_pendientes[v_indice_arrastre]
   
               ON DRAG_FINISHED(v_dragdrop)
                  # se indica que no hay tabla origen
                  INITIALIZE v_arrastra_origen TO NULL
   
               ON DRAG_ENTER(v_dragdrop)
                  IF v_arrastra_origen IS NULL THEN
                     CALL v_dragdrop.setOperation(NULL)
                  END IF
   
               ON DROP(v_dragdrop)
                  IF(v_arrastra_origen == v_sr_pendientes)THEN
                     CALL v_dragdrop.dropInternal()
                  ELSE
                     # Se recupera el indice donde se soltó el archivo
                     LET v_indice_suelta = v_dragdrop.getLocationRow()
                     # Se inserta el archivo en el indice que se recuperó
                     CALL DIALOG.insertRow(v_sr_pendientes, v_indice_suelta)
                     # se establese el foco en la tabla destino
                     CALL DIALOG.setCurrentRow(v_sr_pendientes, v_indice_suelta)
                     # se agrega al arreglo el nomre del archivo
                     LET v_archivos_pendientes[v_indice_suelta] = v_valor_arrastre
                     # elimina el registro de la tabla origen
                     CALL DIALOG.deleteRow(v_sr_elegido, v_indice_arrastre)
                  END IF
            
            END DISPLAY
   
            DISPLAY ARRAY v_archivos_elegidos TO sr_elegido.*
               ON DRAG_START(v_dragdrop)
                  # se recupera tabla origen de arrastre
                  LET v_arrastra_origen = v_sr_elegido
                  # se recupera el indice de la tabla origen de arrastre
                  LET v_indice_arrastre = ARR_CURR()
                  # se recupera el valor de la tabla origen de arrastre
                  LET v_valor_arrastre = v_archivos_elegidos[v_indice_arrastre]
   
               ON DRAG_FINISHED(v_dragdrop)
                  # se indica que no hay tabla origen
                  INITIALIZE v_arrastra_origen TO NULL
   
               ON DRAG_ENTER(v_dragdrop)
                  IF v_arrastra_origen IS NULL THEN
                     CALL v_dragdrop.setOperation(NULL)
                  END IF
   
               ON DROP(v_dragdrop)
                   IF v_arrastra_origen == v_sr_elegido THEN                       
                       CALL v_dragdrop.dropInternal()
                   ELSE
                       # Se recupera el indice donde se soltó el archivo
                       LET v_indice_suelta = v_dragdrop.getLocationRow()
                       # Se inserta el archivo en el indice que se recuperó
                       CALL DIALOG.insertRow(v_sr_elegido, v_indice_suelta)
                       # se establese el foco en la tabla destino
                       CALL DIALOG.setCurrentRow(v_sr_elegido, v_indice_suelta)
                       # se agrega al arreglo el nomre del archivo
                       LET v_archivos_elegidos[v_indice_suelta] = v_valor_arrastre
                       # elimina el registro de la tabla origen
                       CALL DIALOG.deleteRow(v_sr_pendientes, v_indice_arrastre)
                   END IF

            END DISPLAY

            # se despliega las posibles cuentas a separar
            DISPLAY ARRAY v_posibles_cuentas TO sr_posibles_cuentas.*
   
            END DISPLAY
            
            # se despliegan los nss asociados
            DISPLAY ARRAY v_nss_asociado TO sr_nss_asociado.*

            END DISPLAY

            BEFORE DIALOG
               LET v_can_int = FALSE
               # Se recuperan los archivos pendientes
               CALL fn_recupera_archivos_pendientes()
               # se ocultan las tablas del detalle del archivo
               CALL v_forma.setElementHidden("gpo_posibles_cuentas",1)
               CALL v_forma.setElementHidden("gpo_nss_asociado",1)
               CALL DIALOG.setActionHidden("integrar",1)
                  
            # botones para elegir archivo
            ON ACTION aceptar
               # se valida que se haya seleccionado un archivo
               IF(v_archivos_elegidos.getLength() = 0)THEN
                  CALL fn_mensaje("Aviso","Debe arrastrar al menos un archivo a integrar","info")
                  CONTINUE DIALOG
               END IF
               # se valida que solo se integre un archivo
               IF(v_archivos_elegidos.getLength() > 1)THEN
                  CALL fn_mensaje("Aviso","Solo puede seleccionar un archivo","info")
                  CONTINUE DIALOG
               END IF
               # se recuperan las posibles cunetas a separar y los nss asociados
               CALL fn_recupera_detalle()
               LET v_can_int = TRUE
               # Se muestra las tablas del detalle del archivo
               CALL v_forma.setElementHidden("gpo_posibles_cuentas",0)
               CALL v_forma.setElementHidden("gpo_nss_asociado",0)
               CALL v_forma.setElementHidden("integrar",0)
               CALL DIALOG.setActionHidden("integrar",0)
               CALL DIALOG.setActionHidden("aceptar",1)
               # se imprimen los totales por tabla
               DISPLAY v_total_posibles_cuentas TO flbl_total_separacion
               DISPLAY v_total_nss_asociado TO flbl_total_nss
               --ACCEPT DIALOG

            # botones para integrar archivo
            ON ACTION integrar
               LET v_renglon_actual = DIALOG.getcurrentRow("sr_elegido")
               # Ejecuta SP de integración diagnóstico Op 27
               CALL fn_ejecuta_integracion_diagnostico(v_archivos_elegidos[v_renglon_actual])
                       RETURNING r_error
               IF(r_error)THEN
                  EXIT DIALOG
               ELSE
                  LET v_continua = FALSE
                  EXIT DIALOG
               END IF
            
            ON ACTION cancelar
               IF(v_can_int)THEN
                  # regresa a la seleccion del archivo
                  CALL v_forma.setElementHidden("gpo_posibles_cuentas",1)
                  CALL v_forma.setElementHidden("gpo_nss_asociado",1)
                  CALL DIALOG.setActionHidden("integrar",1)
                  CALL DIALOG.setActionHidden("aceptar",0)
                  LET v_archivos_pendientes[1] = v_archivos_elegidos[1]
                  CALL v_archivos_elegidos.clear()
                  LET v_can_int = FALSE
                  EXIT DIALOG
               ELSE
                  # sale de la seleccion del archivo
                  LET v_continua = FALSE
                  EXIT DIALOG
               END IF

         END DIALOG
      END WHILE

   CLOSE WINDOW vtna_integra_diagnostico_op27

END FUNCTION


################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL13                                                   #
#Descripcion       => Recupera los archivos de diagnostico pendientes de       #
#                     integrar                                                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_archivos_pendientes()
DEFINE v_consulta STRING,
       v_archivo  LIKE glo_ctr_archivo.nombre_archivo,
       v_indice   SMALLINT

   WHENEVER ERROR CONTINUE

   LET v_indice = 1
   # Recupera archivos pendientes de integrar
   LET v_consulta = "\n SELECT nombre_archivo",
                    "\n   FROM glo_ctr_archivo",
                    "\n  WHERE proceso_cod = ?",
                    "\n    AND opera_cod = ?",
                    "\n    AND estado = 1" # Cargado

   PREPARE prp_recupera_pendientes FROM v_consulta
   DECLARE cur_recupera_pendientes CURSOR FOR prp_recupera_pendientes
   FOREACH cur_recupera_pendientes USING g_proceso_cod,g_opera_cod_val
                                   INTO v_archivo
      LET v_archivos_pendientes[v_indice] = v_archivo
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_pendientes
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL13                                                   #
#Descripcion       => Recupera los posibles nss                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_detalle()
DEFINE v_consulta STRING,
       v_indice   INTEGER,
       v_posibles_cuentas_aux RECORD
         v_resultado_operacion LIKE sep_cat_resultado_operacion.resultado_operacion_desc,
         v_diag_confronta      LIKE sep_cat_diag_confronta.diag_confronta_desc,
         v_clasificacion       LIKE sep_cat_clasificacion.clasificacion_desc,
         v_total               INTEGER
       END RECORD,
       v_nss_asociado_aux RECORD
          v_resultado_operacion LIKE sep_cat_resultado_operacion.resultado_operacion_desc,
          v_tpo_entidad         LIKE sep_cat_tipo_entidad_asociado.tpo_entidad_desc,
          v_entidad_asociado    LIKE sep_det_03_op27.cve_entidad_asociado,
          v_total               INTEGER
       END RECORD

   WHENEVER ERROR CONTINUE

   LET v_indice = 1
   LET v_total_posibles_cuentas = 0
   CALL v_posibles_cuentas.clear()
   # Recupera detalle de posibles cuentas a separar
   LET v_consulta = "\n SELECT ro.resultado_operacion_desc, dc.diag_confronta_desc,",
                    "\n        cc.clasificacion_desc, COUNT(dg.invadido)",
                    "\n   FROM safre_tmp:tmp_sep_det02_op27_diag dg LEFT OUTER JOIN sep_cat_resultado_operacion ro",
                    "\n     ON dg.resultado_operacion = ro.resultado_operacion",
                    "\n        LEFT OUTER JOIN sep_cat_diag_confronta dc",
                    "\n     ON dg.diag_confronta = dc.diag_confronta",
                    "\n        LEFT OUTER JOIN sep_cat_clasificacion cc",
                    "\n     ON dg.clasifica_separacion = cc.clasifica_separacion",
                    "\n  GROUP BY ro.resultado_operacion_desc, dc.diag_confronta_desc,",
                    "\n           cc.clasificacion_desc"

   PREPARE prp_recupera_posibles_cuentas FROM v_consulta
   DECLARE cur_recupera_posibles_cuentas CURSOR FOR prp_recupera_posibles_cuentas
   FOREACH cur_recupera_posibles_cuentas INTO v_posibles_cuentas_aux.*
      LET v_posibles_cuentas[v_indice].v_clasificacion       = v_posibles_cuentas_aux.v_clasificacion
      LET v_posibles_cuentas[v_indice].v_diag_confronta      = v_posibles_cuentas_aux.v_diag_confronta
      LET v_posibles_cuentas[v_indice].v_resultado_operacion = v_posibles_cuentas_aux.v_resultado_operacion
      LET v_posibles_cuentas[v_indice].v_total               = v_posibles_cuentas_aux.v_total
      LET v_total_posibles_cuentas = v_total_posibles_cuentas + v_posibles_cuentas_aux.v_total      
      LET v_indice = v_indice + 1
   END FOREACH
   
   FREE cur_recupera_posibles_cuentas

   LET v_indice = 1
   LET v_total_nss_asociado = 0
   CALL v_nss_asociado.clear()
   # recupera detalle de NSS asociados
   LET v_consulta = "\n SELECT ro.resultado_operacion_desc, te.tpo_entidad_desc,",
                    "\n        d3.cve_entidad_asociado, COUNT(d3.asociado)",
                    "\n   FROM safre_tmp:tmp_sep_det_03_op27_diag d3 LEFT OUTER JOIN sep_cat_resultado_operacion ro",
                    "\n     ON d3.resultado_operacion = ro.resultado_operacion",
                    "\n        LEFT OUTER JOIN sep_cat_tipo_entidad_asociado te",
                    "\n     ON d3.tipo_entidad_asociado = te.tipo_entidad_asociado",
                    "\n  GROUP BY ro.resultado_operacion_desc, te.tpo_entidad_desc,",
                    "\n           d3.cve_entidad_asociado"

   PREPARE prp_recupera_nss_asociado FROM v_consulta
   DECLARE cur_recupera_nss_asociado CURSOR FOR prp_recupera_nss_asociado
   FOREACH cur_recupera_nss_asociado INTO v_nss_asociado_aux.*
      LET v_nss_asociado[v_indice].v_entidad_asociado    = v_nss_asociado_aux.v_entidad_asociado
      LET v_nss_asociado[v_indice].v_resultado_operacion = v_nss_asociado_aux.v_resultado_operacion
      LET v_nss_asociado[v_indice].v_tpo_entidad         = v_nss_asociado_aux.v_tpo_entidad
      LET v_nss_asociado[v_indice].v_total               = v_nss_asociado_aux.v_total
      LET v_total_nss_asociado = v_total_nss_asociado + v_nss_asociado_aux.v_total
      LET v_indice = v_indice + 1
   END FOREACH
   
   FREE cur_recupera_nss_asociado
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL13                                                   #
#Descripcion       => Ejecuta el SP que realiza la integración del diagnóstico #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
################################################################################
FUNCTION fn_ejecuta_integracion_diagnostico(v_archivo)
DEFINE v_consulta STRING,
       v_archivo  STRING,
       v_ruta_listados LIKE seg_modulo.ruta_listados,
       r_resultado_opera SMALLINT, # bandera de resultado de cambio de estatus
       v_error           BOOLEAN,
       r_folio           LIKE glo_folio.folio
       

   WHENEVER ERROR CONTINUE
   
   SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
   WHERE modulo_cod = "bat"

   # inicializa suponiendo que ejecuta correctamente
   LET v_error = FALSE
      
   CALL fn_valida_operacion(g_pid,g_proceso_cod,v_opera_cod_int) RETURNING r_resultado_opera
     
   IF r_resultado_opera <> 0 THEN
      LET v_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # Indica que ocurrió error
      RETURN v_error
   END IF

   # genera folio 
   {CALL fn_genera_folio(g_pid,g_proceso_cod,v_opera_cod_int)
                        RETURNING r_folio}
   # Folio se genera en lanzado
   LET r_folio = 0

   CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,v_opera_cod_int,r_folio,"SEPL13",
                               v_archivo,p_usuario_cod) RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      LET v_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      RETURN v_error
   END IF
   
   LET v_consulta = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPE13.42r '",
                                    p_usuario_cod CLIPPED, "' ",g_pid CLIPPED, " ",
                                    g_proceso_cod CLIPPED," ",v_opera_cod_int CLIPPED," ",
                                    r_folio CLIPPED, " '",v_archivo CLIPPED,
                     "' 1>", v_ruta_listados CLIPPED,
                           "/nohup:",g_pid USING "&&&&&",":",
                                     g_proceso_cod USING "&&&&&",":",
                                     v_opera_cod_int USING "&&&&&"," 2>&1 &"
   DISPLAY v_consulta 
   RUN v_consulta
   IF(STATUS)THEN
      LET v_error = TRUE
      CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la integración","about")
   ELSE
      CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos","about")
      LET v_error = FALSE
   END IF
   RETURN v_error
END FUNCTION