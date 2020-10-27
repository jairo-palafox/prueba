--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/04/2012
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGL62                                                        #
#Objetivo     => Programa lanzador del proceso de integración de registro de   #
#                pagos de aportacion voluntaria                                #
#Fecha inicio => 10 Abril de 2013                                              #
################################################################################

DATABASE safre_viv
GLOBALS "PAGG01.4gl"  # archivo de variables globales proceso_cod, opera_cod

GLOBALS
   DEFINE g_proceso_cod_pag_registro_pagos_av2 SMALLINT
END GLOBALS

MAIN
DEFINE p_usuario             LIKE seg_usuario.usuario,# usuario firmado al sistema
       p_tipo_carga          SMALLINT,                # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_nombre_programa     VARCHAR(30),             # nombre del programa
       v_archivo             CHAR(40),
       v_archivos_pendientes DYNAMIC ARRAY OF VARCHAR(100), # arreglo que contiene los archivos pendientes 
       v_archivos_integrar   DYNAMIC ARRAY OF VARCHAR(100), # arreglo que contiene los archivos a integrar
       v_detalle_archivo  DYNAMIC ARRAY OF RECORD           # arreglo que contiene los archivos integrados
         nom_archivo     VARCHAR(100),
         total_registros INTEGER,
         imp_ap_vol      DECIMAL(12,2)
       END RECORD,
       v_imp_ap_pat_aux    DECIMAL(12,2),
       v_archivo_proceso   CHAR(40),    # nombre del archivo en proceso
       v_manejador_ui      UI.DRAGDROP, # manejador del arrastrar y soltar (drag and drop)
       v_drag_index        INTEGER,     # indice del drag
       v_drop_index        INTEGER,     # indice del drop
       v_drag_source       STRING,      # fuente del drag
       v_drag_value        STRING,      # valor del drag
       v_contador_archivos SMALLINT,    # numero de archivos a integrar
       v_indice            SMALLINT,    # variable usada para iteracion
       v_indice_aux        SMALLINT,    # indice del arrego de archivos pendientes
       v_folio             LIKE glo_ctr_archivo.folio, # folio
       v_pid               LIKE bat_ctr_proceso.pid,   # identificador del proceso
       v_ruta_ejecutable   LIKE seg_modulo.ruta_bin,   # ruta del bin de sep
       v_ruta_listados     LIKE seg_modulo.ruta_listados, # ruta listados de bat       
       v_comando           STRING,  # contiene al comando a correr
       v_consulta          STRING,  # guarda una sentencia SQL a ejecutar
       v_resultado         SMALLINT # booleana que indica si el proceso se puede ejecutar o no
       CONSTANT l_nom_tbl_pend = "sr_pendientes" # se asigna el nombre de la tabla de archivos pendientes
       CONSTANT l_nom_tbl_int = "sr_integrar"    # se asigna el nombre de la tabla de archivos a integrar

   # se asignan los parametros que vienen del fglrun
   LET p_usuario         = ARG_VAL(1)
   LET p_tipo_carga      = ARG_VAL(2)
   LET p_nombre_programa = ARG_VAL(3)

   # se asigna el titulo del programa
   IF ( p_nombre_programa IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_nombre_programa)
   END IF

   # se inicializa el indice del arreglo
   LET v_indice_aux = 1

   # se asignan los valores necesarios para la intergración
   LET v_folio = 0

   # se crea la sentencia sql que obtiene el pid perteneciente al folio
   LET v_consulta = " SELECT MAX(pid)\n",
                    "   FROM bat_ctr_proceso\n",
                    "  WHERE proceso_cod = ?"

   LET g_proceso_cod_pag_registro_pagos_av2 = 1411;
 
   PREPARE prp_unq_pid_batCtrProc FROM v_consulta
   EXECUTE prp_unq_pid_batCtrProc USING g_proceso_cod_pag_registro_pagos_av2 
                                  INTO v_pid
   
   # se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_pid,
                            g_proceso_cod_pag_registro_pagos_av2,
                            g_opera_cod_pag_integracion) RETURNING v_resultado
   
   # se verifica si la operacion en proceso es valida
   IF( v_resultado <> 0 )THEN
      # en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_resultado)
      EXIT PROGRAM
   END IF

   # se obtienen las rutas de control del modulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
    FROM seg_modulo
    WHERE modulo_cod = 'pag'
  
   # se obtienen la ruta de listados para el modulo bat
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   # se crea la sentencia que busca los archivos disponibles por integrar
   LET v_consulta = " SELECT nombre_archivo\n",
                    "   FROM glo_ctr_archivo\n",
                    "  WHERE proceso_cod = ? \n",
                    "    AND opera_cod = ? \n",
                    "    AND estado = 1" # cargado

   PREPARE prp_archivos_val FROM v_consulta
   DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 

   FOREACH cur_archivos_val USING g_proceso_cod_pag_registro_pagos_av2,
                                  g_opera_cod_pag_carga
                            INTO v_archivo
      LET v_archivos_pendientes[v_indice_aux] = v_archivo CLIPPED
      # se incrementa el indice del arreglo
      LET v_indice_aux = v_indice_aux + 1
   END FOREACH
   

   # se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW vtna_integra_av WITH FORM v_ruta_ejecutable CLIPPED ||"/PAGL621-2"
      DIALOG ATTRIBUTE(UNBUFFERED)
         DISPLAY ARRAY v_archivos_pendientes TO sr_pendientes.*
            ON DRAG_START(v_manejador_ui)
               LET v_drag_source = l_nom_tbl_pend
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_archivos_pendientes[v_drag_index]

            ON DRAG_FINISHED(v_manejador_ui)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_manejador_ui)
               IF v_drag_source IS NULL THEN
                  CALL v_manejador_ui.setOperation(NULL)
               END IF

            ON DROP(v_manejador_ui)
               IF v_drag_source == l_nom_tbl_pend THEN
                  CALL v_manejador_ui.dropInternal()
               ELSE
                  LET v_drop_index = v_manejador_ui.getLocationRow()
                  CALL DIALOG.insertRow(l_nom_tbl_pend, v_drop_index)
                  CALL DIALOG.setCurrentRow(l_nom_tbl_pend, v_drop_index)
                  LET v_archivos_pendientes[v_drop_index] = v_drag_value
                  CALL DIALOG.deleteRow(l_nom_tbl_int, v_drag_index)
               END IF
         END DISPLAY

         DISPLAY ARRAY v_archivos_integrar TO sr_integrar.*
            ON DRAG_START(v_manejador_ui)
               LET v_drag_source = l_nom_tbl_int
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_archivos_integrar[v_drag_index]

            ON DRAG_FINISHED(v_manejador_ui)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_manejador_ui)
               IF v_drag_source IS NULL THEN
                  CALL v_manejador_ui.setOperation(NULL)
               END IF

            ON DROP(v_manejador_ui)
                IF v_drag_source == l_nom_tbl_int THEN
                    CALL v_manejador_ui.dropInternal()
                ELSE
                    LET v_drop_index = v_manejador_ui.getLocationRow()
                    CALL DIALOG.insertRow(l_nom_tbl_int, v_drop_index)
                    CALL DIALOG.setCurrentRow(l_nom_tbl_int, v_drop_index)
                    LET v_archivos_integrar[v_drop_index] = v_drag_value
                    CALL DIALOG.deleteRow(l_nom_tbl_pend, v_drag_index)
                END IF
                 CALL DIALOG.setActionHidden("integrar",1)
         END DISPLAY

         DISPLAY ARRAY v_detalle_archivo TO sr_integrados.*
         END DISPLAY

         BEFORE DIALOG
            -- se ocultan los botones (reporte, integrar)
            CALL DIALOG.setActionHidden("integrar",1)
            
         

         ON ACTION ACCEPT
            # se obtiene el numero de archivos a integrar
            LET v_contador_archivos = v_archivos_integrar.getLength()

            # en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF( v_contador_archivos = 0 )THEN
               CALL fn_mensaje("Aviso","Debe arrastrar al menos un archivo a integrar","stop")
               CONTINUE DIALOG
            END IF

            # se limpia el arreglo de los archivos ya integrados
            CALL v_detalle_archivo.clear()

            # se procesan los archivos seleccionados para integrar
            FOR v_indice = 1 TO v_contador_archivos
            
                LET v_archivo_proceso = v_archivos_integrar[v_indice]
                LET v_detalle_archivo[v_indice].nom_archivo = v_archivo_proceso

                SELECT COUNT(*),
                       SUM(imp_ap_vol)
                INTO   v_detalle_archivo[v_indice].total_registros,
                       v_imp_ap_pat_aux
                FROM  safre_tmp:tmp_pag_det_apvol

                LET v_detalle_archivo[v_indice].imp_ap_vol = v_imp_ap_pat_aux / 100
            END FOR
            
            
            # se limpia el arreglo
            CALL v_archivos_integrar.clear()
            CALL DIALOG.setActionHidden("accept",1)             
            CALL DIALOG.setActionHidden("integrar",0)

            CONTINUE DIALOG

         ON ACTION integrar
            # se obtiene el numero de archivos a integrar
            LET v_contador_archivos = v_detalle_archivo.getLength()

            # en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF( v_contador_archivos = 0 )THEN
               CALL fn_mensaje("Aviso","No se han seleccionado archivos a intergrar","stop")
               CONTINUE DIALOG
            END IF

            IF( v_contador_archivos <> 1 )THEN
               CALL fn_mensaje("Aviso","Solo puede integrar un archivo a la vez","stop")
               CONTINUE DIALOG
            END IF
            
            # se asigna el nombre del archivo en la variable 
            LET v_archivo_proceso = v_detalle_archivo[v_indice].nom_archivo
                        
            # Se inicia la operación en el monitor
            CALL fn_actualiza_opera_ini(v_pid,
                                        g_proceso_cod_pag_registro_pagos_av2,
                                        g_opera_cod_pag_integracion,
                                        v_folio,
                                        "PAGL62-2",
                                        v_archivo_proceso,
                                        p_usuario) RETURNING v_resultado
         
            

            # se verifica si fue posible inicializar la operacion
            IF( v_resultado = 0 )THEN
               # se crea el comando que ejecuta el modulo que reliza la integracion del archivo
               LET v_comando = " nohup time fglrun ",v_ruta_ejecutable CLIPPED,"/PAGP62 ",p_usuario, " ",
                                                                                          v_pid, " ",
                                                                                          g_proceso_cod_pag_registro_pagos_av2, " ",
                                                                                          g_opera_cod_pag_integracion, " ",
                                                                                          v_folio, " ",
                                                                                          v_archivo_proceso, 
                               " 1> ",v_ruta_listados CLIPPED,"/nohup:",v_pid USING "&&&&&",":",
                                                                        g_proceso_cod_pag_registro_pagos_av2 USING "&&&&&",":",
                                                                        g_opera_cod_pag_integracion USING "&&&&&",
                               " 2>&1 &"

               RUN v_comando
               IF(STATUS)THEN
                  CALL fn_mensaje(p_nombre_programa,"Ocurrio un error al ejecutar la integración","about")
               ELSE
                  CALL fn_mensaje(p_nombre_programa,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos","about")
               END IF
               EXIT DIALOG
            ELSE
               # en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(v_resultado)
               CONTINUE DIALOG
            END IF

            # se limpia el arreglo que contiene los archivos a integrar
            CALL v_archivos_integrar.clear()
            CONTINUE DIALOG

         ON ACTION cancelar
            EXIT DIALOG

      END DIALOG
   CLOSE WINDOW vtna_integra_av
END MAIN