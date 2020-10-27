#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDL36                                                                 #
#Objetivo     => Programa lanzador que integra archivo de Ajuste de saldo               #
#Fecha inicio => 11 septiembre 2015                                                     #
#########################################################################################
DATABASE safre_viv

GLOBALS "CBDL36.inc"

PRIVATE DEFINE v_pid                   DECIMAL(9,0)
PRIVATE DEFINE v_usuario_cod           CHAR(20)
PRIVATE DEFINE v_tipo_ejecucion        SMALLINT
PRIVATE DEFINE v_titulo                STRING
PRIVATE DEFINE v_lista_rutas           rutas

MAIN
   LET v_usuario_cod    = ARG_VAL(1)
   LET v_tipo_ejecucion = ARG_VAL(2)
   LET v_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( v_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_titulo)
   END IF

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO v_pid
   FROM bat_ctr_proceso
   WHERE proceso_cod = PROC_AJUSTE_SALDO

   -- se obtienen las rutas de control del modulo
   SELECT 
      ruta_bin, 
      ruta_rescate, 
      ruta_listados
   INTO 
      v_lista_rutas.ruta_bin,
      v_lista_rutas.ruta_rescate,
      v_lista_rutas.ruta_listados_proceso
   FROM seg_modulo
   WHERE modulo_cod = MOD_CONCILIACION

   SELECT 
      ruta_listados
   INTO 
      v_lista_rutas.ruta_listados_batch
   FROM seg_modulo
   WHERE modulo_cod = MOD_BATCH

   CALL fn_drgandrop_integra()
END MAIN

FUNCTION fn_drgandrop_integra()
   DEFINE v_query             STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_indice            SMALLINT -- indice del arrego de archivos pendientes
   DEFINE v_lista_pendientes  DYNAMIC ARRAY OF VARCHAR(100) -- Archivos pendientes
   DEFINE v_lista_listos      DYNAMIC ARRAY OF VARCHAR(100) -- Archivos listos para integrar
   DEFINE v_lista_previo      DYNAMIC ARRAY OF datos_previos
   DEFINE v_archivo_pendiente VARCHAR(100) -- Archivos pendientes 
   
   DEFINE v_t_acciones_c      DECIMAL(26,0)
   DEFINE v_t_pesos_c         DECIMAL(22,0)
   DEFINE l_dnd               ui.DragDrop -- manejador del (drag and drop)
   DEFINE l_drag_index        INTEGER -- indice del drag
   DEFINE l_drop_index        INTEGER -- indice del drop
   DEFINE l_drag_source       STRING -- fuente del drag
   DEFINE l_drag_value        STRING -- valor del drag
   DEFINE l_i_num_arch        SMALLINT -- numero de archivos a integrar
   DEFINE l_i_iter            SMALLINT -- variable usada para iteracion
   DEFINE v_respuesta         SMALLINT--cachar el regreso de la ejecucion de los stores
   
   --se buscan los archivos disponibles por integrar
   LET v_query =  "SELECT ",
                     "nombre_archivo ",
                  "FROM glo_ctr_archivo ",
                  "WHERE proceso_cod = ? ",
                  "AND opera_cod = ? ",
                  "AND estado = ?"
   PREPARE exe_consulta_archivos_pendientes FROM v_query
   DECLARE cur_consulta_archivos_pendientes CURSOR FOR exe_consulta_archivos_pendientes 
   	
   LET v_indice = 0
   FOREACH cur_consulta_archivos_pendientes USING  PROC_AJUSTE_SALDO,
                                                   OP_VALIDA_ARCHIVO,
                                                   EDO_ARCHIVO_VALIDADO
                                             INTO  v_archivo_pendiente

      LET v_indice = v_indice + 1
      LET v_lista_pendientes[v_indice] = v_archivo_pendiente
      
   END FOREACH

   IF(v_indice < 1)THEN
      CALL fn_mensaje("Atención", "No existen archivos cargados pendientes de integrar", "info")
      RETURN
   END IF

   -- se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW w_inte_devol WITH FORM "CBDL361" 

   DIALOG ATTRIBUTE(UNBUFFERED)
      DISPLAY ARRAY v_lista_pendientes TO lista_pendientes.*
         ON DRAG_START(l_dnd)
            LET l_drag_source = v_nombre_pendientes
            LET l_drag_index = arr_curr()
            LET l_drag_value = v_lista_pendientes[l_drag_index]
            
         ON DRAG_FINISHED(l_dnd)
            INITIALIZE l_drag_source TO NULL

         ON DRAG_ENTER(l_dnd)--aun no sueltas el valor 
            IF l_drag_source IS NULL THEN
               CALL l_dnd.setOperation(NULL)
            END IF
             
         ON DROP(l_dnd)--si lo dejas en el mismo lugar o lo bajas
            IF l_drag_source == v_nombre_pendientes THEN
               CALL l_dnd.dropInternal()--el genero acomoda el arreglo no hay que hacer nada
            ELSE
               LET l_drop_index = l_dnd.getLocationRow()
               CALL DIALOG.insertRow(v_nombre_pendientes, l_drop_index)
               CALL DIALOG.setCurrentRow(v_nombre_pendientes, l_drop_index)
               LET v_lista_pendientes[l_drop_index] = l_drag_value
               CALL DIALOG.deleteRow(v_nombre_listos, l_drag_index)
            END IF
      END DISPLAY
      
      DISPLAY ARRAY v_lista_listos TO lista_listos.*
         ON DRAG_START(l_dnd)
            LET l_drag_source = v_nombre_listos
            LET l_drag_index = arr_curr()
            LET l_drag_value = v_lista_listos[l_drag_index]
            
         ON DRAG_FINISHED(l_dnd)
            INITIALIZE l_drag_source TO NULL

         ON DRAG_ENTER(l_dnd)
            IF l_drag_source IS NULL THEN
               CALL l_dnd.setOperation(NULL)
            END IF

         ON DROP(l_dnd)
            IF l_drag_source == v_nombre_listos THEN
               CALL l_dnd.dropInternal()
            ELSE
               LET l_drop_index = l_dnd.getLocationRow()
               CALL DIALOG.insertRow(v_nombre_listos, l_drop_index)
               CALL DIALOG.setCurrentRow(v_nombre_listos, l_drop_index)  --Visualizar activo
               LET v_lista_listos[l_drop_index] = l_drag_value  --agrega al arreglo
               CALL DIALOG.deleteRow(v_nombre_pendientes, l_drag_index)  --elimina el arreglo de donde estaba antes
            END IF
            CALL DIALOG.setActionHidden("integrar",1)
            --CALL DIALOG.setActionHidden("deshacer",0)
      END DISPLAY
      

      DISPLAY ARRAY v_lista_previo TO record3.*
      END DISPLAY

      BEFORE DIALOG
         CALL DIALOG.setActionHidden("integrar",1)

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION ACCEPT
         -- se obtiene el numero de archivos a integrar
         LET l_i_num_arch = v_lista_listos.getLength()

         -- Debe de haber al menos archivo a procesar
         IF l_i_num_arch = 0 THEN
            CALL fn_mensaje("Aviso",
                            "Debe arrastrar al menos un archivo a integrar",
                            "stop")
            CONTINUE DIALOG
         END IF

         -- se limpia el arreglo de los archivos ya integrados
         CALL v_lista_previo.clear()

         -- se procesan los archivos seleccionados para integrar
         FOR l_i_iter = 1 TO l_i_num_arch
            -- se asigna la informacion del archivo a integrar
            LET l_i_iter = 1
            LET v_lista_previo[l_i_iter].nom_archivo = v_lista_listos[l_i_iter]

            SELECT 
               SUM(monto_en_acciones), 
               SUM(monto_en_pesos), 
               COUNT(*)
            INTO 
               v_t_acciones_c, 
               v_t_pesos_c,
               v_lista_previo[l_i_iter].registros
            FROM safre_tmp:tmp_cbd_detalle_ajuste_saldo

            LET v_lista_previo[l_i_iter].acciones = v_t_acciones_c/1000000
            LET v_lista_previo[l_i_iter].pesos = v_t_pesos_c/100
                
         END FOR

         -- se limpia el arreglo
         CALL v_lista_listos.clear()
         CALL DIALOG.setActionHidden("integrar",0)
         CALL DIALOG.setActionHidden("accept",1)

         CONTINUE DIALOG

      ON ACTION integrar
         
         CALL fn_ventana_confirma("Atención","¿Desea ejecutar el proceso de Integración?","quest") RETURNING v_respuesta
         --1 aceptar, 0 cancelar ... glog01.42x fn_ventana_confirma
        IF ( v_respuesta = 1 ) THEN
        
           CALL fn_glo_integracion_demo(v_lista_previo[1].nom_archivo)
           EXIT DIALOG
        END IF
       
   END DIALOG
CLOSE WINDOW w_inte_devol
END FUNCTION -- fn_drgandrop_integra_devol

FUNCTION fn_glo_integracion_demo(p_archivo)--realiza el fgl_run
   DEFINE v_s_comando        STRING -- cadena con una instruccion de consola
   DEFINE v_mensaje          STRING
   DEFINE v_i_resultado      INTEGER -- resultado del proceso
   DEFINE v_folio            LIKE glo_folio.folio
   DEFINE v_archivo          STRING
   DEFINE r_resultado_opera  SMALLINT  --CODIGO  DE ERROR fn_actualiza_opera_ini
    DEFINE p_archivo string
   LET v_folio = 0
   LET v_archivo = "NA"
   
   CALL fn_valida_operacion(v_pid, PROC_AJUSTE_SALDO, OP_INTEGRA_ARCHIVO) RETURNING v_i_resultado
   
   IF ( v_i_resultado = 0 ) THEN
   
     -- Inicio operacion.
     CALL fn_genera_folio(PROC_AJUSTE_SALDO, OP_INTEGRA_ARCHIVO, v_usuario_cod) RETURNING v_folio

     CALL fn_actualiza_opera_ini(v_pid,
                                 PROC_AJUSTE_SALDO,
                                 OP_INTEGRA_ARCHIVO,
                                 v_folio,
                                 "CBDP36",
                                 p_archivo,
                                 v_usuario_cod)
      RETURNING r_resultado_opera
             
     IF ( r_resultado_opera = 0 ) THEN
       LET v_s_comando = " nohup time fglrun ",v_lista_rutas.ruta_bin CLIPPED,"/CBDP36 ",
                         v_usuario_cod CLIPPED, " ",
                         v_pid, " ",
                         PROC_AJUSTE_SALDO, " ",
                         OP_INTEGRA_ARCHIVO, " ",
                         v_folio, " '",
                         p_archivo CLIPPED, "' ",
                         " 1>",v_lista_rutas.ruta_listados_batch CLIPPED,
                         "/nohup:",v_pid USING "&&&&&",":",
                         PROC_AJUSTE_SALDO USING "&&&&&",":",
                         OP_INTEGRA_ARCHIVO USING "&&&&&" ,
                         " 2>&1 &"
       RUN v_s_comando
       CALL fn_mensaje("Atención",
                        "Se ha enviado la integración.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
                        "information")
      ELSE                   
      	CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
      END IF                   
   
   ELSE
      CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje--Mensaje de por que no se ejecuta la funcion
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   END IF 
END FUNCTION