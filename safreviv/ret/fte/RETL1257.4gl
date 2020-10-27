################################################################################
#Version                    => 1.0.0                                           #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => RET                                                      #
#Programa          => RETL1257                                                 #
#Objetivo          => Programa lanzador para integrar archivos de              #
#                     respuesta FICO                                           #
#Fecha inicio      => 09/03/2018                                               #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

GLOBALS
DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod, -- código del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod, -- código de operacion
       g_opera_cod_ant   LIKE cat_operacion.opera_cod, -- código de operacion
       v_usuario      VARCHAR(30), -- Almacena al usuario
       g_folio       LIKE dis_det_avance_pago.folio
END GLOBALS

MAIN
DEFINE v_tipo_proceso    SMALLINT,   -- Forma como ejecutara el programa 
       v_nom_prog        VARCHAR(30) -- Almacena opción del menú 

   -- se asignan los parametros que vienen del fglrun
   LET v_usuario = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog = ARG_VAL(3)
   LET g_proceso_cod = g_proceso_archivos_respuesta_fico_ssv
   LET g_opera_cod = g_opera_cod_integra_respuesta_fico
   LET g_opera_cod_ant = g_opera_cod_valida_respuesta_fico

   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   CALL fn_drgandrop_avancepago()
   
END MAIN
#Objetico: Función que permite seleccionar y arrastrar el archivo a integrar
FUNCTION fn_drgandrop_avancepago()
DEFINE l_arr_arch_pend    DYNAMIC ARRAY OF VARCHAR(100), -- Archivos pendientes 
       l_arr_arch_int     DYNAMIC ARRAY OF VARCHAR(100), -- Archivos a integrar
       l_arr_integrados   DYNAMIC ARRAY OF RECORD -- Detalle archivos a integrar
            folio         LIKE dis_det_avance_pago.folio,
            nom_archivo   VARCHAR(100),
            f_presenta    DATE,
            tot_reg       DECIMAL (10,0)--INTEGER
                          END RECORD,
       l_v_arch_proceso   VARCHAR(100),
       l_dnd              ui.DragDrop, -- manejador del (drag and drop)
       l_drag_index       INT, -- indice del drag
       l_drop_index       INT, -- indice del drop
       l_drag_source      STRING, -- fuente del drag
       l_drag_value       STRING, -- valor del drag
       l_i_num_arch       SMALLINT, -- numero de archivos a integrar
       l_i_iter           INTEGER,--SMALLINT, -- variable usada para iteracion
       l_i_indice         SMALLINT, -- indice del arrego de archivos pendientes
       l_i_tot_reg        DECIMAL (10,0),--INTEGER, -- total de registros en archivo
       l_i_tot_apo        DECIMAL(12,2), -- Total aportaciones
       l_i_tot_amo        DECIMAL(12,2), -- Total amortizaciones
       l_s_qryTxt         STRING, -- guarda una sentencia SQL a ejecutar
       l_comando          STRING,
       v_ruta_ejecutable  LIKE seg_modulo.ruta_bin, -- Ruta del ejecutable
       v_ruta_listados    LIKE seg_modulo.ruta_listados, -- Rute del log
       v_max_pid          LIKE bat_ctr_proceso.pid,
       v_s_mensaje        STRING,
       r_b_valida         INTEGER,
       r_bandera          INTEGER

   CONSTANT l_nom_tbl_pend = "tbl_pendientes" -- Tabla de archivos pendientes
   CONSTANT l_nom_tbl_int = "tbl_integrar" -- Tabla de archivos a integrar

   -- se inicializa el indice del arreglo
   LET l_i_indice = 1

   --Obtiene el ultimo pid correspondiente al proceso
   CALL fn_max_pid(g_proceso_cod, 1) RETURNING v_max_pid

   --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'ret'

   --Obtiene ruta listados
   SELECT ruta_listados
     INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET l_s_qryTxt = " SELECT nombre_archivo \n",
                    " FROM glo_ctr_archivo \n",
                    " WHERE opera_cod = ", g_opera_cod_ant,
                    "    AND proceso_cod = ", g_proceso_cod,
                    "    AND estado = 1"
   PREPARE prp_archivos_val FROM l_s_qryTxt

   DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 
      FOREACH cur_archivos_val INTO l_arr_arch_pend[l_i_indice]
         -- se incrementa el indice del arreglo
         LET l_i_indice = l_i_indice + 1
      END FOREACH

   -- se borra el ultimo indice del arreglo porque es nulo
   CALL l_arr_arch_pend.deleteElement(l_i_indice)
      
   -- se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW w_inte_fico WITH FORM "RETL12571"
   DIALOG ATTRIBUTE(UNBUFFERED)
      DISPLAY ARRAY l_arr_arch_pend TO tbl_pendientes.*
         ON DRAG_START(l_dnd)
            LET l_drag_source = l_nom_tbl_pend
            LET l_drag_index = arr_curr()
            LET l_drag_value = l_arr_arch_pend[l_drag_index]
             
         ON DRAG_FINISHED(l_dnd)
            INITIALIZE l_drag_source TO NULL

         ON DRAG_ENTER(l_dnd)
            IF l_drag_source IS NULL THEN
               CALL l_dnd.setOperation(NULL)
            END IF
             
         ON DROP(l_dnd)
            IF l_drag_source == l_nom_tbl_pend THEN
               CALL l_dnd.dropInternal()
            ELSE
               LET l_drop_index = l_dnd.getLocationRow()
               CALL DIALOG.insertRow(l_nom_tbl_pend, l_drop_index)
               CALL DIALOG.setCurrentRow(l_nom_tbl_pend, l_drop_index)
               LET l_arr_arch_pend[l_drop_index] = l_drag_value
               CALL DIALOG.deleteRow(l_nom_tbl_int, l_drag_index)
            END IF
      END DISPLAY
      
      DISPLAY ARRAY l_arr_arch_int TO tbl_integrar.*
         ON DRAG_START(l_dnd)
            LET l_drag_source = l_nom_tbl_int
            LET l_drag_index = arr_curr()
            LET l_drag_value = l_arr_arch_int[l_drag_index]
            
         ON DRAG_FINISHED(l_dnd)
            INITIALIZE l_drag_source TO NULL

         ON DRAG_ENTER(l_dnd)
            IF l_drag_source IS NULL THEN
               CALL l_dnd.setOperation(NULL)
            END IF

         ON DROP(l_dnd)
            IF l_drag_source == l_nom_tbl_int THEN
               CALL l_dnd.dropInternal()
            ELSE
               LET l_drop_index = l_dnd.getLocationRow()
               CALL DIALOG.insertRow(l_nom_tbl_int, l_drop_index)
               CALL DIALOG.setCurrentRow(l_nom_tbl_int, l_drop_index)
               LET l_arr_arch_int[l_drop_index] = l_drag_value
               CALL DIALOG.deleteRow(l_nom_tbl_pend, l_drag_index)
            END IF
            CALL DIALOG.setActionHidden("accept",0)
            --CALL DIALOG.setActionHidden("deshacer",0)
      END DISPLAY
      
      DISPLAY ARRAY l_arr_integrados TO tbl_integrados.*
      END DISPLAY

      BEFORE DIALOG
         CALL DIALOG.setActionHidden("accept",1)
         --CALL DIALOG.setActionHidden("deshacer",0)
         CALL DIALOG.setActionHidden("close",1)
         CALL DIALOG.setActionHidden("integrar",1)

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION accept
         -- se obtiene el numero de archivos a integrar
         LET l_i_num_arch = l_arr_arch_int.getLength()

         -- Debe de haber al menos archivo a procesar
         IF l_i_num_arch = 0 THEN
            CALL fn_mensaje("Aviso",
                            "Debe arrastrar al menos un archivo a integrar",
                            "stop")
            CONTINUE DIALOG
         END IF

         -- se limpia el arreglo de los archivos ya integrados
         CALL l_arr_integrados.clear()

         -- se procesan los archivos seleccionados para integrar
         FOR l_i_iter = 1 TO l_i_num_arch
            -- se asigna el nombre del archivo en la variable paramentro 
            LET l_v_arch_proceso = l_arr_arch_int[l_i_iter]

            LET l_s_qryTxt = "\n SELECT COUNT(*)",
                              "\n FROM safre_tmp:tmp_ret_res_fico"
            PREPARE pre_sql_total_registros_archivo FROM l_s_qryTxt
            EXECUTE pre_sql_total_registros_archivo INTO l_i_tot_reg



            -- se asigna la informacion del archivo integrado
            LET l_arr_integrados[l_i_iter].folio = g_folio
            LET l_arr_integrados[l_i_iter].nom_archivo = l_v_arch_proceso
            LET l_arr_integrados[l_i_iter].f_presenta = TODAY
            LET l_arr_integrados[l_i_iter].tot_reg = l_i_tot_reg
         END FOR

         -- se limpia el arreglo
         CALL l_arr_arch_int.clear()
         CALL DIALOG.setActionHidden("accept",1)
         --CALL DIALOG.setActionHidden("deshacer",0)
         CALL DIALOG.setActionHidden("integrar",0)
         CONTINUE DIALOG

  
      ON ACTION integrar
         --Valida Operación
         CALL fn_valida_operacion(v_max_pid,g_proceso_cod,g_opera_cod)
                        RETURNING r_bandera
         IF r_bandera = 0 THEN  
            -- se ejecuta la función que inserta la operacion a ejecutar            
            CALL fn_actualiza_opera_ini (v_max_pid,g_proceso_cod,g_opera_cod,g_folio,"RETP1257",l_v_arch_proceso,v_usuario)
                 RETURNING r_b_valida

            IF ( r_b_valida = 0 ) THEN --Actualiza Operación Inicial

               LET g_folio = 0
            
               LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                               "/RETP1257.42r ",
                               v_usuario CLIPPED," ",
                               v_max_pid," ",
                               g_proceso_cod," ",
                               g_opera_cod," ",
                               g_folio," ",
                               " '",l_v_arch_proceso CLIPPED, "' 1>", v_ruta_listados CLIPPED ,
                               "/nohup:",v_max_pid USING "&&&&&",":",
                                         g_proceso_cod USING "&&&&&",":",
                                         g_opera_cod USING "&&&&&" ," 2>&1 &"
               DISPLAY l_comando
                                         
               RUN l_comando
               CALL l_arr_integrados.clear()
               CALL DIALOG.setActionHidden("integrar",1)
               -- se asigna el mensaje a mostrar al usuario
               LET v_s_mensaje = "Se ha enviado la integración con PID: ",
                                  v_max_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
               CALL fn_mensaje("Integración",v_s_mensaje,"information")
            END IF--Actualiza Operación
               -- Envía mensaje con la descripcin del Error
               --CALL fn_muestra_inc_operacion(r_bandera)
         END IF--Valida operación
         EXIT DIALOG

   END DIALOG
   CLOSE WINDOW w_inte_fico
END FUNCTION