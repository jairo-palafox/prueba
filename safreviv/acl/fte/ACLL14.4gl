--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

####################################################################
#Modulo            =>ACL                                           #
#Programa          =>ACLL14                                        #
#Objetivo          =>Programa que permite la generar archivo de    #
#                    salida manual de aclaratorio                  #
#Autor             =>Francsico Lopez                               #
#Fecha inicio      =>01 Febrero 2012                               #
####################################################################
-- genera unicamente el archivo
DATABASE safre_viv
DEFINE
    g_pid                 DECIMAL(9,0),
    g_proceso_cod         SMALLINT,
    g_opera_cod           SMALLINT
# Objetivo: Programa que permite la seleccion de archivos de lqinfo para su correspondiente integracion
MAIN
   DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
          p_b_tipo_carga    SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_nom_prog      VARCHAR(30), -- nombre del programa
          --v_arr_arch_pend   DYNAMIC ARRAY OF VARCHAR(100), -- arreglo que contiene los archivos pendientes
          v_arr_arch_pend   DYNAMIC ARRAY OF RECORD
            v_folio_pen         decimal(9,0),
            v_origen_pago_pen   smallint, 
            v_fecha_lote_pen    date,
            v_lote_cod_pen      smallint,
             v_id_referencia_int decimal(9,0)
          END RECORD,
          
          --v_arr_arch_int    DYNAMIC ARRAY OF VARCHAR(100), -- arreglo que contiene los archivos a integrar
          v_arr_arch_int DYNAMIC ARRAY OF RECORD
            v_folio_int         decimal(9,0),
            v_origen_pago_int   smallint, 
            v_fecha_lote_int    date,
            v_lote_cod_int      SMALLINT,
            v_id_referencia_int decimal(9,0)
          END RECORD,
          {v_arr_integrados  DYNAMIC ARRAY OF RECORD -- arreglo que contiene los archivos integrados
             nom_archivo    VARCHAR(100),
             fecha_trans    DATE,
             lote           INTEGER,
             fecha_proceso  DATE,
             tot_registros  INTEGER
          END RECORD,}
          v_arr_integrados  DYNAMIC ARRAY OF RECORD -- arreglo que contiene los archivos integrados
            v_folio_int         decimal(9,0),
            v_origen_pago_int   smallint, 
            v_fecha_lote_int    date,
            v_lote_cod_int      SMALLINT,
            v_id_referencia_int decimal(9,0)
          END RECORD,
          v_arr_drag_value RECORD
            v_folio_int         decimal(9,0),
            v_origen_pago_int   smallint, 
            v_fecha_lote_int    date,
            v_lote_cod_int      smallint,
            v_id_referencia_int decimal(9,0)
          END RECORD,
          --v_r_acr_ctr_arch  RECORD LIKE acr_ctr_archivo.*, -- registro del control de archivo  
          v_v_nomArch_proc  VARCHAR(100), -- nombre del archivo en proceso
          v_resultado       SMALLINT, -- resultado de ejecucion de la SP
          v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
          v_drag_index      INTEGER, -- indice del drag
          v_drop_index      INTEGER, -- indice del drop
          v_drag_source     STRING, -- fuente del drag
          v_drag_value      STRING, -- valor del drag
          v_i_num_arch      SMALLINT, -- numero de archivos a integrar
          v_i_iter          SMALLINT, -- variable usada para iteracion
          v_i_indice        SMALLINT, -- indice del arrego de archivos pendientes
          v_i_proceso_cod   LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod     LIKE cat_operacion.opera_cod, -- operación de la etapa que llama la funcion
          v_i_opera_cod_ant SMALLINT, --LIKE cat_operacion.opera_cod, -- operación de la etapa anterior
          v_i_operacion     SMALLINT,  --LIKE acr_ctr_archivo.operacion, -- operacion del proceso
          v_d_folio         LIKE glo_ctr_archivo.folio, -- folio
          v_d_pid           LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_c_ruta_bin_acr  LIKE seg_modulo.ruta_bin, -- ruta del bin de acr
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_cadena        STRING, -- se asigna un mensaje que será presentado al usuario
          v_s_comando       STRING, -- contiene al comando a correr
          v_s_qryTxt        STRING, -- guarda una sentencia SQL a ejecutar
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
          ,v_ruta_vacia      STRING
          ,p_nom_archivo         CHAR(40)
          ,v_lote_cod        SMALLINT,
          v_bandera           SMALLINT,
          v_existen_registros SMALLINT

          DEFINE p_titulo            STRING -- titulo del mensaje enviado en el correo
          DEFINE p_mensaje           STRING -- cuerpo del mensaje enviado
   
          CONSTANT l_nom_tbl_pend = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
          CONSTANT l_nom_tbl_int  = "tbl_integrar" -- se asigna el nombre de la tabla de archivos a integrar
         
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crear el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".ACLL14.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializa el indice del arreglo
   LET v_i_indice = 1

   --sE OBTIENEN las rutas de los ejecutables
   CALL fn_rutas("acl") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat

   SELECT COUNT(*)
   INTO  v_existen_registros
   FROM acl_ctr_lote
   WHERE lote_cod = 1 
   AND nombre_archivo IS NULL
   
   IF v_existen_registros IS NULL OR v_existen_registros = 0 THEN
     CALL fn_mensaje("Atención","No existen archivosa a Generar","information")
     EXIT PROGRAM
   END IF
        
   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET v_s_qryTxt = " SELECT folio, origen_pago, f_lote, lote_cod, id_referencia \n",
                    "   FROM acl_ctr_lote \n",
                    "  WHERE lote_cod = 1 \n",
                    "    AND nombre_archivo IS NULL"
 
   PREPARE prp_archivos_val FROM v_s_qryTxt
   DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 

   FOREACH cur_archivos_val INTO v_arr_arch_pend[v_i_indice].*
      -- se incrementa el indice del arreglo
      LET v_i_indice = v_i_indice + 1
   END FOREACH

   -- se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW w_inte_acred WITH FORM "ACLL141"
      DIALOG ATTRIBUTE(UNBUFFERED)
         DISPLAY ARRAY v_arr_arch_pend TO tbl_pendientes.*
            ON DRAG_START(v_ui_dnd)
               DISPLAY "v_ui_dnd: ",v_ui_dnd
               LET v_drag_source = l_nom_tbl_pend
               DISPLAY "v_drag_source = ",v_drag_source
               LET v_drag_index = arr_curr()
               LET v_arr_drag_value.* = v_arr_arch_pend[v_drag_index].*

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
                  LET v_arr_arch_pend[v_drop_index].* = v_arr_drag_value.* --v_drag_value
                  CALL DIALOG.deleteRow(l_nom_tbl_int, v_drag_index) 
               END IF
         END DISPLAY

         DISPLAY ARRAY v_arr_arch_int TO tbl_integrar.*
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = l_nom_tbl_int
               LET v_drag_index = arr_curr()
               LET v_arr_drag_value.* = v_arr_arch_int[v_drag_index].*
               
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
                    LET v_arr_arch_int[v_drop_index].* = v_arr_drag_value.*
                    CALL DIALOG.deleteRow(l_nom_tbl_pend, v_drag_index)
                END IF
         END DISPLAY

         DISPLAY ARRAY v_arr_integrados TO tbl_integrados.*
         END DISPLAY

         BEFORE DIALOG
            -- se ocultan los botones (reporte, integrar)
            CALL DIALOG.setActionHidden("generar",1)  
           

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
               LET v_arr_integrados[v_i_iter].* = v_arr_arch_int[v_i_iter].*

            END FOR

            -- si se muestra la informacion del archivo a integrar se habilita el boton de integrar
            IF v_arr_integrados.getLength() > 0 THEN
               -- Se muestra la opcion del reporte
               CALL DIALOG.setActionHidden("generar",0)
            END IF

            CONTINUE DIALOG

         ON ACTION Generar

            -- se obtiene el numero de archivos a integrar
            LET v_i_num_arch = v_arr_integrados.getLength()

            -- en caso de no existir registroa a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_cadena =  "No se han seleccionado archivos a intergrar"
               CALL fn_mensaje("Aviso",v_s_cadena,"stop")

               CONTINUE DIALOG
            END IF
            IF v_i_num_arch > 1 THEN
               CALL fn_mensaje("Aviso","Solo es posible integrar un archivo a la vez","stop")
               CONTINUE DIALOG
            END IF
            -- se procesan los archivos seleccionados para integrar
            FOR v_i_iter = 1 TO v_i_num_arch

               -- se verifica si fue posible inicializar la operacion
               IF r_b_valida = 0 THEN
       ##
-- se asignan los valores necesarios para la intergración
   LET v_i_proceso_cod = 105  -- LQINFO
   LET v_i_opera_cod = 4 -- Integracion de LQINFO
   LET v_i_opera_cod_ant = 3 -- Carga (Validar) de archivo
   LET v_i_operacion = 01 -- operacion del proceso
   LET v_c_programa_cod = "ACLL14"
   LET v_lote_cod = 1 --lote_cod: 1 = Seleccion y Preliquidación. 2 = Consulta

   
   -- se crea la sentencia sql que obtiene el pid perteneciente al folio
   LET v_s_qryTxt = " SELECT MAX(pid)\n",
                    "   FROM bat_ctr_proceso\n",
                    "\n WHERE  folio = ? \n",
                    "\n AND    proceso_cod = ? "

   PREPARE prp_unq_pid_batCtrProc FROM v_s_qryTxt
   EXECUTE prp_unq_pid_batCtrProc INTO v_d_pid
                                  USING v_arr_integrados[v_i_iter].v_folio_int,
                                        v_i_proceso_cod
   CALL fn_display_proceso(0,"Generar archivo Aclaratorio")



      --Se marca el proceso como iniciado
      CALL fn_actualiza_opera_ini (v_d_pid
                                  ,v_i_proceso_cod
                                  ,v_i_opera_cod
                                  ,v_arr_integrados[v_i_iter].v_folio_int
                                  ,"ACLS01"
                                  ,"SALIDA_MANUAL"
                                  ,p_v_usuario)
                                  RETURNING v_resultado

   
   LET v_s_comando = "fglrun ACLS01.42r ",p_v_usuario," ",
                                    v_d_pid," ", 
                                    v_i_proceso_cod," ", 
                                    v_i_opera_cod," ", 
                                    v_arr_integrados[v_i_iter].v_folio_int," ", 
                                    v_arr_integrados[v_i_iter].v_origen_pago_int," ", 
                                    v_arr_integrados[v_i_iter].v_id_referencia_int," ",
                                    v_lote_cod 
                                    ," 1>", v_c_ruta_list_bat CLIPPED
                                    ,"/nohup:",v_d_pid USING "&&&&&",":"
                                    ,v_i_proceso_cod USING "&&&&&",":"
                                    ,v_i_opera_cod USING "&&&&&"
                                    ," 2>&1 &"

                  -- se ejecuta el comando
                  DISPLAY v_s_comando
                  RUN v_s_comando
                  
                  IF ( STATUS ) THEN
                     CALL fn_mensaje(p_v_nom_prog,"Ocurrió un error al ejecutar la integración","about")
                     --CALL fn_error_opera(v_d_pid, v_i_proceso_cod, v_i_opera_cod)
                     --RETURNING v_bandera
                     --LET p_mensaje = "Generación del Archivo realizada con éxito."
                     --EXIT DIALOG
                  ELSE
                     --CALL fn_actualiza_opera_fin(v_d_pid, v_i_proceso_cod, v_i_opera_cod)
                     --RETURNING v_bandera
                     CALL fn_mensaje(p_v_nom_prog,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                     LET p_mensaje = "El proceso de Generación del Archivo ha finalizado pero con errores."  
                     --EXIT DIALOG
                  END IF
                  LET p_titulo = "Finalización de operación - Salida Aclaraciones - Generación del Archivo"
                  CALL fn_correo_proceso(v_d_pid, v_i_proceso_cod, 
                                         v_i_opera_cod, NULL, 
                                         p_titulo,p_mensaje)
                  CALL fn_display_proceso(1,"Generar archivo Aclaratorio")
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

            --CONTINUE DIALOG

{         ON ACTION btn_reporte
            LET v_s_cadena = "REPORTE"
            CALL fn_mensaje("Integración",v_s_cadena,"information")}

            CONTINUE DIALOG
      END DIALOG
   CLOSE WINDOW w_inte_acred
END MAIN