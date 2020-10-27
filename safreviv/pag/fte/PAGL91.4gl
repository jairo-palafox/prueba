-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Componente    => PAGL91
-- Objetivo      => Registra la información de detalle en el histórico de CambiaVit 
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 28 de Mayo de 2018
-- Requerimiento => plasgac-43
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl" --archivo de variables globales proceso_cod


MAIN
   DEFINE 
      p_v_usuario     LIKE seg_usuario.usuario,
      p_b_tipo_carga  SMALLINT,                      -- tipo de carga (1 - modo en linea y 2 - modo batch)
      p_v_nom_prog    VARCHAR(30),                   -- nombre del programa
      v_arr_arch_pend DYNAMIC ARRAY OF VARCHAR(100), -- arreglo que contiene los archivos pendientes 
      v_arr_arch_int  DYNAMIC ARRAY OF VARCHAR(100)  -- arreglo que contiene los archivos a integrar
          
   DEFINE v_arr_integrados DYNAMIC ARRAY OF RECORD
      nom_archivo          VARCHAR(100),
      num_reg_detalle      DECIMAL(9,0),
      total_monto_deposito DECIMAL(16,2)
   END RECORD

   DEFINE
      v_v_nomArch_proc VARCHAR(100), -- nombre del archivo en proceso
      v_ui_dnd         UI.DRAGDROP,  -- manejador del arrastrar y soltar (drag and drop)
      v_drag_index     INTEGER,      -- indice del drag
      v_drop_index     INTEGER,      -- indice del drop
      v_drag_source    STRING,       -- fuente del drag
      v_drag_value     STRING,       -- valor del drag
      v_i_num_arch     SMALLINT,     -- numero de archivos a integrar
      v_i_iter         SMALLINT,     -- variable usada para iteracion
      v_i_indice       SMALLINT,     -- indice del arrego de archivos pendientes
      v_s_cadena       STRING,       -- se asigna un mensaje que será presentado al usuario
      v_s_comando      STRING,       -- contiene al comando a correr
      v_s_qryTxt       STRING,       -- guarda una sentencia SQL a ejecutar
      r_b_valida       SMALLINT,     -- booleana que indica si el proceso se puede ejecutar o no
      v_ruta_vacia     STRING

   DEFINE      
      v_d_folio         LIKE glo_ctr_archivo.folio,          -- folio
      v_d_pid           LIKE bat_ctr_proceso.pid,            -- identificador del proceso
      v_c_ruta_bin_acr  LIKE seg_modulo.ruta_bin,            -- ruta del bin de acr
      v_c_ruta_list_bat LIKE seg_modulo.ruta_listados        -- ruta listados de bat

   CONSTANT l_nom_tbl_pend = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
   CONSTANT l_nom_tbl_int = "tbl_integrar"    -- se asigna el nombre de la tabla de archivos a integrar
         
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crear el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".PAGL91.log")

   -- se asigna el titulo del programa
   IF p_v_nom_prog IS NOT NULL THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializa el indice del arreglo
   LET v_i_indice = 1

   LET v_d_folio = 0      --Se indica folio 0 para que el proceso de historico genere un nuevo folio

   -- se crea la sentencia sql que obtiene el pid perteneciente al folio
   LET v_s_qryTxt = " SELECT MAX(pid)",
                    " FROM   bat_ctr_proceso",
                    " WHERE  proceso_cod = ",g_proceso_reg_pag_svt

   PREPARE prp_unq_pid_batCtrProc FROM v_s_qryTxt
   EXECUTE prp_unq_pid_batCtrProc INTO v_d_pid

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,g_proceso_reg_pag_svt,g_opera_cod_pag_integracion) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   --SE OBTIENEN las rutas de los ejecutables
   CALL fn_rutas("pag") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat
   
   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET v_s_qryTxt = " SELECT nombre_archivo",
                    " FROM   glo_ctr_archivo",
                    " WHERE  proceso_cod = ",g_proceso_reg_pag_svt,
                    " AND    opera_cod =   ",g_opera_cod_pag_carga,  --se asigna la variable global de integracion v_i_opera_cod_ant
                    " AND    estado = 1" -- cargado
   PREPARE prp_archivos_val FROM v_s_qryTxt
   DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 

   FOREACH cur_archivos_val INTO v_arr_arch_pend[v_i_indice]
      LET v_i_indice = v_i_indice + 1
   END FOREACH

   -- se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW w_inte_acred WITH FORM "PAGL911"
   
      DIALOG ATTRIBUTE(UNBUFFERED)
         DISPLAY ARRAY v_arr_arch_pend TO tbl_pendientes.*
            ON DRAG_START(v_ui_dnd)
               --DISPLAY "v_ui_dnd: ",v_ui_dnd
               LET v_drag_source = l_nom_tbl_pend
               DISPLAY "v_drag_source = ",v_drag_source
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_arr_arch_pend[v_drag_index]

            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            ON DROP(v_ui_dnd)
               IF v_drag_source == l_nom_tbl_pend THEN
                  CALL v_ui_dnd.dropInternal()
               ELSE
                  LET v_drop_index = v_ui_dnd.getLocationRow()
                  CALL DIALOG.insertRow(l_nom_tbl_pend, v_drop_index)
                  CALL DIALOG.setCurrentRow(l_nom_tbl_pend, v_drop_index)
                  LET v_arr_arch_pend[v_drop_index] = v_drag_value
                  CALL DIALOG.deleteRow(l_nom_tbl_int, v_drag_index)
               END IF
         END DISPLAY

         DISPLAY ARRAY v_arr_arch_int TO tbl_integrar.*
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = l_nom_tbl_int
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_arr_arch_int[v_drag_index]

            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            ON DROP(v_ui_dnd)
                IF v_drag_source == l_nom_tbl_int THEN
                    CALL v_ui_dnd.dropInternal()
                ELSE
                    LET v_drop_index = v_ui_dnd.getLocationRow()
                    CALL DIALOG.insertRow(l_nom_tbl_int, v_drop_index)
                    CALL DIALOG.setCurrentRow(l_nom_tbl_int, v_drop_index)
                    LET v_arr_arch_int[v_drop_index] = v_drag_value
                    CALL DIALOG.deleteRow(l_nom_tbl_pend, v_drag_index)
                END IF
              CALL DIALOG.setActionHidden("integrar",1)  
         END DISPLAY

         DISPLAY ARRAY v_arr_integrados TO tbl_integrados.*
         END DISPLAY

         BEFORE DIALOG
            -- se ocultan los botones (reporte, integrar)
            CALL DIALOG.setActionHidden("btn_reporte",1)
            CALL DIALOG.setActionHidden("integrar",1)

         ON ACTION cancelar
            EXIT DIALOG

         ON ACTION ACCEPT
            -- se obtiene el numero de archivos a integrar
            LET v_i_num_arch = v_arr_arch_int.getLength()

            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena = "Debe arrastrar al menos un archivo a integrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")

               CONTINUE DIALOG
            END IF

            -- se limpia el arreglo de los archivos ya integrados
            CALL v_arr_integrados.clear()

            -- se procesan los archivos seleccionados para integrar
            FOR v_i_iter = 1 TO v_i_num_arch
               -- se asigna el nombre del archivo en la variable paramentro 
               LET v_v_nomArch_proc = v_arr_arch_int[v_i_iter]
               SELECT COUNT(*), SUM(monto_deposito)/100
               INTO   v_arr_integrados[v_i_iter].num_reg_detalle,
                      v_arr_integrados[v_i_iter].total_monto_deposito
               FROM   safre_tmp:tmp_det_cvt
                
               LET v_arr_integrados[v_i_iter].nom_archivo = v_v_nomArch_proc
            END FOR
            -- se limpia el arreglo
            CALL v_arr_arch_int.clear()
            CALL DIALOG.setActionHidden("accept",1) 
            CALL DIALOG.setActionHidden("integrar",0)
            CONTINUE DIALOG

         ON ACTION integrar
            -- se obtiene el numero de archivos a integrar
            LET v_i_num_arch = v_arr_integrados.getLength()

            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena =  "No se han seleccionado archivos a integrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")

               CONTINUE DIALOG
            END IF
            IF v_i_num_arch > 1 THEN
               CALL fn_mensaje("Aviso","Solo es posible integrar un archivo a la vez","stop")
               CONTINUE DIALOG
            END IF
            -- se procesan los archivos seleccionados para integrar
            FOR v_i_iter = 1 TO v_i_num_arch
               -- se asigna el nombre del archivo en la variable 
               LET v_v_nomArch_proc = v_arr_integrados[v_i_iter].nom_archivo

               DISPLAY "PID: ", v_d_pid
               DISPLAY "PROCESO COD: ", g_proceso_reg_pag_svt
               DISPLAY "Opera COD: ", g_opera_cod_pag_integracion
               DISPLAY "FOLIO: ", v_d_folio
               DISPLAY "PROGRAMA: PAGP90" 
               DISPLAY "ARCHIVO: ", v_v_nomArch_proc
               DISPLAY "USUARIO: ", p_v_usuario
               
               
               -- Se registra el inicio de la operacion
               CALL fn_actualiza_opera_ini(v_d_pid,g_proceso_reg_pag_svt,g_opera_cod_pag_integracion,v_d_folio,
                              "PAGP90",v_v_nomArch_proc,p_v_usuario) RETURNING r_b_valida 

               DISPLAY "Valida: ", r_b_valida
                              
               -- se verifica si fue posible inicializar la operacion
               IF r_b_valida = 0 THEN
               	
                  LET v_s_comando = "nohup fglrun ",
                                  v_c_ruta_bin_acr CLIPPED,
                                  "/PAGP90.42r ",
                                  p_v_usuario, " ",
                                  v_d_pid, " ",
                                  g_proceso_reg_pag_svt," ",
                                  g_opera_cod_pag_integracion," ",
                                  v_d_folio, " ",
                                  v_v_nomArch_proc,
                                  " 1>", v_c_ruta_list_bat CLIPPED,
                                  "/nohup:",v_d_pid USING "&&&&&",":",
                                  g_proceso_reg_pag_svt USING "&&&&&",":",
                                  g_opera_cod_pag_integracion USING "&&&&&",
                                  " 2>&1 &"
                  
                  DISPLAY v_s_comando
                  RUN v_s_comando
                  IF(STATUS)THEN
                     CALL fn_mensaje(p_v_nom_prog,"Ocurrio un error al ejecutar la integración","about")
                  ELSE
                     CALL fn_mensaje(p_v_nom_prog,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                  END IF
                  EXIT DIALOG
 
               ELSE
                  -- en caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)

                  CONTINUE DIALOG
               END IF
            END FOR

            -- se limpia el arreglo que contiene los archivos a integrar
            CALL v_arr_arch_int.clear()

            -- Se muestra la opcion del reporte
            --CALL DIALOG.setActionHidden("btn_reporte",0)

            CONTINUE DIALOG

         ON ACTION btn_reporte
            LET v_s_cadena = "REPORTE"
            CALL fn_mensaje("Integración",v_s_cadena,"information")

            CONTINUE DIALOG
      END DIALOG
   CLOSE WINDOW w_inte_acred
END MAIN