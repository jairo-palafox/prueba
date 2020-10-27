################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 16/10/2014                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDL31                                                   #
#Objetivo          => Programa lanzador para integrar archivos                 #
#Fecha inicio      => 16/10/2014                                               #
################################################################################
DATABASE safre_viv

GLOBALS "CBDL31.inc"

PRIVATE DEFINE p_usuario            VARCHAR(30)
PRIVATE DEFINE v_folio              SMALLINT
PRIVATE DEFINE v_pid                LIKE bat_ctr_proceso.pid   #  ID del proceso

PRIVATE DEFINE g_reg_modulo   RECORD
   ruta_exp         VARCHAR(40),
   ruta_rescate     VARCHAR(40),
   ruta_listados    VARCHAR(40)
END RECORD

MAIN
   DEFINE p_tipo_ejecucion    SMALLINT    -- Forma como ejecutara el programa 
   DEFINE p_titulo            VARCHAR(30) -- Almacena opción del menú 

   -- se asignan los parametros que vienen del fglrun
   LET p_usuario = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO v_pid
   FROM bat_ctr_proceso
   WHERE proceso_cod = PROC_AJUSTE

    -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'cbd'

   CALL fn_drgandrop_bdnsviv()
   
END MAIN


#PENDIENTE
FUNCTION fn_drgandrop_bdnsviv()
   DEFINE   l_arr_arch_pend    DYNAMIC ARRAY OF VARCHAR(100), -- Archivos pendientes 
            l_arr_arch_int     DYNAMIC ARRAY OF VARCHAR(100), -- Archivos a integrar
            l_arr_integrados   DYNAMIC ARRAY OF RECORD -- Detalle archivos a integrar
               nom_archivo     VARCHAR(100),
               t_aivs_viv97    DECIMAL(26,6),
               t_registros97   DECIMAL(9,0),
               t_aivs_viv92    DECIMAL(26,6),
               t_registros92   DECIMAL(9,0)
            END RECORD,
            v_subcuenta        SMALLINT,
            l_v_arch_proceso   VARCHAR(100),
            l_dnd              ui.DragDrop, -- manejador del (drag and drop)
            l_drag_index       INT, -- indice del drag
            l_drop_index       INT, -- indice del drop
            l_drag_source      STRING, -- fuente del drag
            l_drag_value       STRING, -- valor del drag
            l_i_num_arch       SMALLINT, -- numero de archivos a integrar
            l_i_iter           SMALLINT, -- variable usada para iteracion
            l_i_indice         SMALLINT, -- indice del arrego de archivos pendientes
            l_i_tot_reg        SMALLINT, -- total de registros en archivo
            l_s_qryTxt         STRING, -- guarda una sentencia SQL a ejecutar
            l_comando          STRING,
            v_ruta_ejecutable  LIKE seg_modulo.ruta_bin, -- Ruta del ejecutable
            v_ruta_listados    LIKE seg_modulo.ruta_listados, -- Rute del log
            v_max_pid          LIKE bat_ctr_proceso.pid,
            v_respuesta        SMALLINT

   CONSTANT l_nom_tbl_pend = "tbl_pendientes" -- Tabla de archivos pendientes
   CONSTANT l_nom_tbl_int = "tbl_integrar" -- Tabla de archivos a integrar

   --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'cbd'

     -- se crea la sentencia que busca los archivos disponibles por integrar
   LET l_s_qryTxt = "SELECT nombre_archivo ",
                    "FROM glo_ctr_archivo ",
                    "WHERE opera_cod = ", OP_VALIDA_ARCHIVO, " ",        #Operacion de carga
                    "AND proceso_cod = ", PROC_AJUSTE, " ",
                    "AND estado = 1"
   PREPARE prp_archivos_val FROM l_s_qryTxt
   DECLARE cur_archivos_val CURSOR FOR prp_archivos_val
   
   -- se inicializa el indice del arreglo
   LET l_i_indice = 1

   FOREACH cur_archivos_val INTO l_arr_arch_pend[l_i_indice]
      -- se incrementa el indice del arreglo
      LET l_i_indice = l_i_indice + 1
   END FOREACH

   IF l_i_indice = 1 THEN
      CALL fn_mensaje("Atención",
           "No existen archivos cargados pendientes de integrar","info")
      RETURN
   END IF

   --Obtiene ruta listados
   SELECT ruta_listados
     INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

   -- se borra el ultimo indice del arreglo porque es nulo
   CALL l_arr_arch_pend.deleteElement(l_i_indice)
      
   -- se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW w_inte_cbd WITH FORM "CBDL311"
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
            CALL DIALOG.setActionHidden("integrar",0)
            CALL DIALOG.setActionHidden("deshacer",0)
      END DISPLAY
      
      DISPLAY ARRAY l_arr_integrados TO tbl_integrados.*
      END DISPLAY

      BEFORE DIALOG
         CALL DIALOG.setActionHidden("integrar",1)
         CALL DIALOG.setActionHidden("deshacer",1)
         CALL DIALOG.setActionHidden("close",1)
         CALL DIALOG.setActionHidden("accept",1)

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION integrar
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

            #Consulta para presentar la informacion del sumario
            LET l_s_qryTxt =  "SELECT ", 
                                 "SUM(monto_acciones / 1000000), ",
                                 "COUNT(*) ",
                              "FROM safre_tmp:tmp_cbd_ajuste ",
                              "WHERE subcuenta = ? "
            PREPARE exe_consulta_sumario FROM l_s_qryTxt

            #Se llena la informacion a presentar en la pantalla
            LET l_arr_integrados[l_i_iter].nom_archivo = l_v_arch_proceso
            
            #Sumairio de viv 97
            LET v_subcuenta = 4
            EXECUTE exe_consulta_sumario USING v_subcuenta INTO   l_arr_integrados[l_i_iter].t_aivs_viv97,
                                                                  l_arr_integrados[l_i_iter].t_registros97
            IF l_arr_integrados[l_i_iter].t_aivs_viv97 IS NULL THEN
               LET l_arr_integrados[l_i_iter].t_aivs_viv97 = 0
            END IF 

            #Sumairio de viv 92
            LET v_subcuenta = 8
            EXECUTE exe_consulta_sumario USING v_subcuenta INTO   l_arr_integrados[l_i_iter].t_aivs_viv92,
                                                                  l_arr_integrados[l_i_iter].t_registros92
            IF l_arr_integrados[l_i_iter].t_aivs_viv92 IS NULL THEN
               LET l_arr_integrados[l_i_iter].t_aivs_viv92 = 0
            END IF 
         END FOR

         -- se limpia el arreglo
         CALL l_arr_arch_int.clear()
         CALL DIALOG.setActionHidden("integrar",1)
         CALL DIALOG.setActionHidden("deshacer",1)
         CALL DIALOG.setActionHidden("accept",0)
         CONTINUE DIALOG

      ON ACTION deshacer
         CALL l_arr_arch_int.clear()
         CALL DIALOG.setActionHidden("integrar",1)
         CALL DIALOG.setActionHidden("deshacer",1)
         DECLARE cur_integra_arch CURSOR FOR prp_archivos_val
            LET l_i_indice = 1
            FOREACH cur_integra_arch INTO l_arr_arch_pend[l_i_indice]
            -- se incrementa el indice del arreglo
            LET l_i_indice = l_i_indice + 1
         END FOREACH
         CONTINUE DIALOG

      ON ACTION ACCEPT
         --Solicita confirmar(1) o cancelar(0) la operación de Registro
         CALL fn_ventana_confirma("Atención",
                    "¿Desea ejecutar el proceso de Integracion del archivo de ajuste operativo?",
                     "quest") RETURNING v_respuesta

        IF v_respuesta = 1 THEN
            CALL fn_ejecuta_integracion(l_v_arch_proceso, v_ruta_ejecutable, v_ruta_listados)
           CALL l_arr_integrados.clear()
           CALL DIALOG.setActionHidden("accept",1)
           EXIT DIALOG
        END IF

   END DIALOG
CLOSE WINDOW w_inte_cbd
END FUNCTION

FUNCTION fn_ejecuta_integracion(p_nombre_archivo, v_ruta_ejecutable, v_ruta_listados)
   DEFINE p_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo    -- nombre dle archivo
   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin               -- Ruta del ejecutable
   DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados          -- Rute del log
   DEFINE v_comando           STRING
   DEFINE v_resultado         INTEGER

   -- Se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(v_pid,PROC_AJUSTE,OP_INTEGRA_ARCHIVO) RETURNING v_resultado
   IF v_resultado = 0 THEN
      LET v_folio = 1
      
      # Inicia operación
      CALL fn_actualiza_opera_ini(v_pid,PROC_AJUSTE,OP_INTEGRA_ARCHIVO,v_folio,"CBDP31",
                            p_nombre_archivo,p_usuario) RETURNING v_resultado
      
      LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDP31 ",
                                       p_usuario," ",
                                       v_pid," ",
                                       PROC_AJUSTE," ",
                                       OP_INTEGRA_ARCHIVO," ",
                                       v_folio," '",
                                       p_nombre_archivo,
                                       "' 1>", v_ruta_listados CLIPPED ,
                                       "/nohup:",v_pid USING "&&&&&",":",
                                                PROC_AJUSTE USING "&&&&&",":",
                                                OP_INTEGRA_ARCHIVO USING "&&&&&" ," 2>&1 &"
      DISPLAY v_comando                        
      RUN v_comando
      CALL fn_mensaje("Atención",
               "Se ha enviado la integración.\n"||
               "Podrá revisar el resultado en el monitor de ejecución de procesos",
                "information")
   ELSE
      DISPLAY "v_i_resultado:",v_resultado
      CALL fn_mensaje("Atención",fn_mues_desc_valida(v_resultado),"stop")
   END IF
END FUNCTION

FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera
    
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida