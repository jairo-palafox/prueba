################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL379                                                      #
#Ojetivo       => Programa lanzador de integrador para la consulta de historico#
#                 anexo 1.                                                     #
#Fecha inicio  => 23 de Julio, 2015.                                           #
#Requerimiento => 841                                                          #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

DEFINE g_pid               LIKE bat_ctr_proceso.pid --ID del proceso
DEFINE g_proceso_cod       LIKE cat_proceso.proceso_cod --Codigo del proceso
DEFINE g_opera_cod         LIKE cat_operacion.opera_cod --Codigo de operacion
DEFINE g_opera_cod_ant     LIKE cat_operacion.opera_cod --Codigo de operacion anterior
DEFINE g_reg_modulo        RECORD
         ruta_bin               CHAR(40),
         ruta_rescate           CHAR(40),
         ruta_listados          CHAR(40)
END RECORD

DEFINE seg_modulo_bat      RECORD
         ruta_listados        CHAR(40)--Log de bitacora 
END RECORD

DEFINE g_usuario_cod       CHAR(20)
DEFINE g_tipo_ejecucion    SMALLINT --Forma como ejecutara el programa
DEFINE g_titulo            STRING --Titulo de la ventana

MAIN

    DEFINE v_mensaje    STRING
    DEFINE v_rest_valida    SMALLINT

    LET g_usuario_cod    = ARG_VAL(1)
    LET g_tipo_ejecucion = ARG_VAL(2)
    LET g_titulo         = ARG_VAL(3)

    --Si se obtuvo el titulo, se pone como titulo de programa
    IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
    END IF

    --Se inicia el log del programa
    CALL STARTLOG (g_usuario_cod CLIPPED||".RETL379.log")

    --Se asigna proceso y operacion
    LET g_proceso_cod = 1567
    LET g_opera_cod   = 2
    LET g_opera_cod_ant = 1

    --Se obtiene el PID del proceso
    SELECT MAX(pid)
        INTO g_pid
        FROM bat_ctr_proceso
        WHERE proceso_cod = g_proceso_cod

    -- Valida operacion para verificar si se puede continuar.
    CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
        
    IF ( v_rest_valida != 0 ) THEN

      CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_mensaje

      CALL fn_mensaje("Atención", v_mensaje, "stop")
      MENU
         COMMAND "Cerrar"
            EXIT MENU
      END MENU
      RETURN
    END IF

    --Se obtienen las rutas de control del modulo
    SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
        INTO g_reg_modulo.*
        FROM seg_modulo s
        WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
        INTO seg_modulo_bat.ruta_listados
        FROM seg_modulo b
        WHERE b.modulo_cod = 'bat'

    
    CALL fn_drag_drop_integra()

END MAIN

#Funcion en la cual se realiza el DRAG AND DROP
FUNCTION fn_drag_drop_integra()

    DEFINE v_arr_arch_pend      DYNAMIC ARRAY OF VARCHAR(100)--Archivos pendientes
    DEFINE v_arr_arch_int       DYNAMIC ARRAY OF VARCHAR(100)--Archivos a integrar
    DEFINE v_arr_integrados     DYNAMIC ARRAY OF RECORD -- Detalle archivos a integrar
            nom_archivo             VARCHAR(100),
            tot_reg                 INTEGER
    END RECORD
    DEFINE v_i_num_arch         SMALLINT--Numero de archivos a integrar
    DEFINE v_indice             SMALLINT--Variable usada para iteracion
    DEFINE v_i_iter            SMALLINT--Variable usada para iteracion
    
    DEFINE v_dnd                ui.DragDrop--Manejador del (drag and drop)
    DEFINE v_drag_index         INTEGER--Indice del drag
    DEFINE v_drop_index         INT --Indice del drop
    DEFINE v_drag_source        STRING--Fuente del drag
    DEFINE v_drag_value         STRING--Valor del drag

    DEFINE v_query              STRING--Guarda una sentencia SQL a ejecutar
    DEFINE v_respuesta          SMALLINT--Para verificar la respuesta de la ejecucion de store's

    CONSTANT v_nom_tbl_pend = "record1"--Tabla de archivos pendientes
    CONSTANT v_nom_tbl_int  = "record2"--Tabla de archivos a integrar

    --Se crea la sentencia que busca los archivos disponibles por integrar
    LET v_query    = "SELECT nombre_archivo"
                    ," FROM glo_ctr_archivo "
                    ,"WHERE proceso_cod = ", g_proceso_cod
                    ,"  AND opera_cod   = ", g_opera_cod_ant
                    ,"  AND estado = 1"

    PREPARE prp_archivos FROM v_query CLIPPED
    DECLARE cur_archivos CURSOR FOR prp_archivos 

    -- se inicializa el indice del arreglo
    LET v_indice = 0
    --Se agregan los archivos con extension valida para este proceso.
    FOREACH cur_archivos  INTO v_arr_arch_pend[v_indice:=v_indice+1]
    END FOREACH

    --Se elimina el ultimo elemento del arrglo si existe, pues es nulo
    IF v_indice > 0 THEN
        CALL v_arr_arch_pend.deleteElement(v_indice)
    END IF

    IF v_indice < 1 THEN
        CALL fn_mensaje("Atención",
                        "No existen archivos cargados pendientes de integrar",
                        "info")
        RETURN
    END IF

    --Se abre la ventana para elejir los archivos a integrar
    OPEN WINDOW w_actualiza_solicitud WITH FORM "RETL3791"
        DIALOG ATTRIBUTE(UNBUFFERED)
            --Archivos pendiente a integrar
            DISPLAY ARRAY v_arr_arch_pend TO record1.*
                ON DRAG_START(v_dnd)
                    LET v_drag_source = v_nom_tbl_pend
                    LET v_drag_index = arr_curr()
                    LET v_drag_value = v_arr_arch_pend[v_drag_index]

                ON DRAG_FINISHED(v_dnd)
                    INITIALIZE v_drag_source TO NULL

                ON DRAG_ENTER(v_dnd)
                    IF v_drag_source IS NULL THEN--Si la fuente no tiene valor entonces
                        CALL v_dnd.setOperation(NULL)--No realiza ninguna operacion
                    END IF

                ON DROP(v_dnd)
                    IF v_drag_source == v_nom_tbl_pend THEN
                        CALL v_dnd.dropInternal()--Se hace el reacomodo autimatico por parte de GENERO
                    ELSE
                        LET v_drop_index = v_dnd.getLocationRow()
                        CALL DIALOG.insertRow(v_nom_tbl_pend, v_drop_index)
                        CALL DIALOG.setCurrentRow(v_nom_tbl_pend, v_drop_index)
                        LET v_arr_arch_pend[v_drop_index] = v_drag_value
                        CALL DIALOG.deleteRow(v_nom_tbl_int, v_drag_index)
                    END IF
            END DISPLAY

            --Archivos seleccionados para integrar
            DISPLAY ARRAY v_arr_arch_int TO record2.*
                ON DRAG_START(v_dnd)
                    LET v_drag_source = v_nom_tbl_int
                    LET v_drag_index = arr_curr()
                    LET v_drag_value = v_arr_arch_int[v_drag_index]

                ON DRAG_FINISHED(v_dnd)
                    INITIALIZE v_drag_source TO NULL

                ON DRAG_ENTER(v_dnd)
                    IF v_drag_source IS NULL THEN
                        CALL v_dnd.setOperation(NULL)
                    END IF

                ON DROP(v_dnd)
                    IF v_drag_source == v_nom_tbl_int THEN
                        CALL v_dnd.dropInternal()
                    ELSE
                        LET v_drop_index = v_dnd.getLocationRow()
                        CALL DIALOG.insertRow(v_nom_tbl_int, v_drop_index)
                        CALL DIALOG.setCurrentRow(v_nom_tbl_int, v_drop_index)
                        LET v_arr_arch_int[v_drop_index] = v_drag_value
                        CALL DIALOG.deleteRow(v_nom_tbl_pend, v_drag_index)
                    END IF
                    CALL DIALOG.setActionHidden("integrar",1)
            END DISPLAY

            DISPLAY ARRAY v_arr_integrados TO record3.*
            END DISPLAY

            BEFORE DIALOG
                CALL DIALOG.setActionHidden("integrar",1)

            ON ACTION CANCEL
                EXIT DIALOG

            ON ACTION ACCEPT
                --Se obtiene el numero de archivos a integrar
                LET v_i_num_arch = v_arr_arch_int.getLength()

                --Debe de haber al menos archivo a procesar
                IF v_i_num_arch = 0 THEN
                    CALL fn_mensaje("Aviso",
                    "Debe arrastrar al menos un archivo a integrar",
                    "stop")
                    CONTINUE DIALOG
                END IF

                --Se limpia el arreglo de los archivos ya integrados
                CALL v_arr_integrados.clear()

                --Se procesan los archivos seleccionados para integrar
                FOR v_i_iter = 1 TO v_i_num_arch
                    --Se asigna el nombre del archivo en la variable paramentro 
                    LET v_arr_integrados[v_i_iter].nom_archivo = v_arr_arch_int[v_i_iter]
                    --Se obtiene el total de registros
                    SELECT COUNT(*)
                    INTO v_arr_integrados[v_i_iter].tot_reg
                    FROM safre_tmp:tmp_ret_anexo1_cons_det
                END FOR

                --Se limpia el arreglo
                CALL v_arr_arch_int.clear()
                CALL DIALOG.setActionHidden("integrar",0)
                CALL DIALOG.setActionHidden("accept",1)

                CONTINUE DIALOG

            ON ACTION integrar
                --Solicita confirmar(1) o cancelar(0) la operación de Registro
                CALL fn_ventana_confirma("Atención","¿Desea ejecutar el proceso de Integración?","quest") RETURNING v_respuesta

                IF v_respuesta = 1 THEN
                    CALL fn_integracion_lanzado(v_arr_integrados[1].nom_archivo)
                    EXIT DIALOG
                END IF

        END DIALOG
    CLOSE WINDOW w_actualiza_solicitud

END FUNCTION

#Funcion que lanza el proceso que realiza la integracion
FUNCTION fn_integracion_lanzado(p_archivo)

    DEFINE v_s_comando        STRING--Cadena con una instruccion de consola
    DEFINE v_mensaje          STRING
    DEFINE v_i_resultado      INTEGER--Resultado del proceso
    DEFINE v_folio            LIKE glo_folio.folio
    DEFINE p_archivo          STRING
    DEFINE r_resultado_opera  SMALLINT  --Código de error fn_actualiza_opera_ini

    LET v_folio = 0

    CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado

    IF v_i_resultado = 0 THEN
        CALL fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario_cod) RETURNING v_folio
        CALL fn_actualiza_opera_ini(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod,
                                    v_folio,
                                    "RETP379",
                                    p_archivo,
                                    g_usuario_cod)
        RETURNING r_resultado_opera

        IF r_resultado_opera = 0 THEN
            LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/RETP379 ",
                                                    g_usuario_cod CLIPPED, " ",
                                                    g_pid                , " ",
                                                    g_proceso_cod        , " ",
                                                    g_opera_cod          , " ",
                                                    v_folio              , " '",
                                                    p_archivo CLIPPED    , "' ",
                                                    " 1>",seg_modulo_bat.ruta_listados clipped,
                                                    "/nohup:",g_pid USING "&&&&&",":",
                                                    g_proceso_cod   USING "&&&&&",":",
                                                    g_opera_cod     USING "&&&&&" ,
                                                    " 2>&1 &"
            RUN v_s_comando
            CALL fn_mensaje("Atención",
                            "Se ha enviado la integración.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
                            "information")
        ELSE                   
            CALL fn_recupera_inconsis_opera(r_resultado_opera) RETURNING v_mensaje
        END IF                   

    ELSE
        CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
        CALL fn_mensaje("Atención", v_mensaje, "stop")
    END IF 
END FUNCTION