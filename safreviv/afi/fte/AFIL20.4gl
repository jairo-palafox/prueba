#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIL20                                                                 #
#Objetivo     => Programa que integra archivo masivo de notificaciones                  #
#Fecha inicio => FEBRERO 2015                                                           #
#########################################################################################
DATABASE safre_viv

DEFINE g_pid               LIKE bat_ctr_proceso.pid --  ID del proceso
DEFINE g_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
DEFINE g_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
DEFINE g_opera_cod_ant     LIKE cat_operacion.opera_cod -- codigo de operacion anterior
DEFINE g_reg_modulo        RECORD
        ruta_exp              CHAR(40),
        ruta_rescate          CHAR(40),
        ruta_listados         CHAR(40)
END RECORD

DEFINE seg_modulo_bat      RECORD
         ruta_listados        CHAR(40)
END RECORD

DEFINE g_usuario_cod       CHAR(20)
DEFINE g_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
DEFINE g_titulo            STRING -- titulo de la ventana

MAIN
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_tipo_ejecucion = ARG_VAL(2)
   LET g_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   -- se inicia el log del programa
   CALL STARTLOG (g_usuario_cod CLIPPED||".AFIL20.log")

   -- se asigna proceso y operacion
   LET g_proceso_cod = 1815
   LET g_opera_cod   = 2
   LET g_opera_cod_ant = 1

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'afi'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   CALL fn_drgandrop_integra_demo()
END MAIN

FUNCTION fn_drgandrop_integra_demo()
   DEFINE l_arr_arch_pend     DYNAMIC ARRAY OF VARCHAR(100) -- Archivos pendientes
   DEFINE l_arr_arch_int      DYNAMIC ARRAY OF VARCHAR(100) -- Archivos a integrar
   DEFINE l_arr_integrados    DYNAMIC ARRAY OF RECORD -- Detalle archivos a integrar
            nom_archivo          VARCHAR(100),
            marca_sms            INTEGER,
            desmarca_sms         INTEGER,
            marca_correo         INTEGER,
            desmarca_correo      INTEGER,
            bloqueo_sms          INTEGER,
            bloqueo_correo       INTEGER,
            tot_marca            INTEGER,
            tot_desmarca         INTEGER,
            tot_bloqueo          INTEGER,
            tot_det              INTEGER,
            tot_sum              INTEGER
   END RECORD
   
   DEFINE l_v_arch_proceso    VARCHAR(100)
   DEFINE l_dnd               ui.DragDrop -- manejador del (drag and drop)
   DEFINE l_drag_index        INT -- indice del drag
   DEFINE l_drop_index        INT -- indice del drop
   DEFINE l_drag_source       STRING -- fuente del drag
   DEFINE l_drag_value        STRING -- valor del drag
   DEFINE l_i_num_arch        SMALLINT -- numero de archivos a integrar
   DEFINE l_i_iter            SMALLINT -- variable usada para iteracion
   DEFINE l_i_indice          SMALLINT -- indice del arrego de archivos pendientes
   DEFINE v_query             STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_respuesta         SMALLINT
   DEFINE v_r_arch_pend       VARCHAR(100) -- Archivos pendientes

   DEFINE v_count             INTEGER
   DEFINE v_indicador         SMALLINT

   CONSTANT l_nom_tbl_pend = "record1" -- Tabla de archivos pendientes
   CONSTANT l_nom_tbl_int  = "record2" -- Tabla de archivos a integrar

   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET v_query    = "SELECT nombre_archivo"
                    ," FROM glo_ctr_archivo "
                    ,"WHERE proceso_cod = ", g_proceso_cod
                    ,"  AND opera_cod   = ", g_opera_cod_ant --,g_opera_cod que tiene la carga
                    ,"  AND estado = 1" -- archivos pendientes de integrar

   PREPARE prp_archivos FROM v_query CLIPPED
   DECLARE cur_archivos CURSOR FOR prp_archivos

   -- se inicializa el indice del arreglo
   LET l_i_indice = 0

   FOREACH cur_archivos  INTO v_r_arch_pend
      -- se incrementa el indice del arreglo para llenar tabla de archivos pendientes
      LET l_i_indice = l_i_indice + 1
      LET l_arr_arch_pend[l_i_indice] = v_r_arch_pend
   END FOREACH

   IF(l_i_indice<1)THEN
      CALL fn_mensaje("Atención",
           "No existen archivos cargados pendientes de integrar","info")
      RETURN
   END IF

   -- se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW afil20 WITH FORM "AFIL201"
   DIALOG ATTRIBUTE(UNBUFFERED)
      DISPLAY ARRAY l_arr_arch_pend TO record1.*
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

      DISPLAY ARRAY l_arr_arch_int TO record2.*
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
            CALL DIALOG.setActionHidden("integrar",1)
            --CALL DIALOG.setActionHidden("deshacer",0)
      END DISPLAY

      DISPLAY ARRAY l_arr_integrados TO record3.*
      END DISPLAY

      BEFORE DIALOG
         CALL DIALOG.setActionHidden("integrar",1)

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION ACCEPT
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
            -- se asigna la informacion del archivo integrado
            --LET l_arr_integrados[l_i_iter].folio = g_folio
            LET l_arr_integrados[l_i_iter].nom_archivo = l_v_arch_proceso
            LET l_arr_integrados[l_i_iter].marca_sms       = 0
            LET l_arr_integrados[l_i_iter].desmarca_sms    = 0
            LET l_arr_integrados[l_i_iter].desmarca_correo = 0
            LET l_arr_integrados[l_i_iter].marca_correo    = 0

            DECLARE cur_sms CURSOR FOR SELECT ind_sms, COUNT(*)
                                         FROM safre_tmp:tmp_det_ind_not
                                        GROUP BY 1
            FOREACH cur_sms INTO v_indicador, v_count
               CASE v_indicador
                 WHEN 0
                     LET l_arr_integrados[l_i_iter].desmarca_sms = v_count
                 WHEN 1
                     LET l_arr_integrados[l_i_iter].marca_sms = v_count 
               END CASE
            END FOREACH

            DECLARE cur_correo CURSOR FOR SELECT ind_correo, COUNT(*)
                                            FROM safre_tmp:tmp_det_ind_not
                                           GROUP BY 1
            FOREACH cur_correo INTO v_indicador, v_count
               CASE v_indicador
                 WHEN 0
                     LET l_arr_integrados[l_i_iter].desmarca_correo = v_count
                 WHEN 1
                     LET l_arr_integrados[l_i_iter].marca_correo = v_count 
               END CASE
            END FOREACH

            SELECT COUNT(*)
              INTO l_arr_integrados[l_i_iter].bloqueo_sms
              FROM safre_tmp:tmp_det_ind_not
             WHERE bloqueo_int_sms = 1

            SELECT COUNT(*)
              INTO l_arr_integrados[l_i_iter].bloqueo_correo
              FROM safre_tmp:tmp_det_ind_not
             WHERE bloqueo_int_correo = 1

            SELECT COUNT(*)
              INTO l_arr_integrados[l_i_iter].tot_det
              FROM safre_tmp:tmp_det_ind_not

            SELECT tot_registros
              INTO l_arr_integrados[l_i_iter].tot_sum
              FROM safre_tmp:tmp_sum_ind_not

            LET l_arr_integrados[l_i_iter].tot_marca = l_arr_integrados[l_i_iter].marca_correo +
                                                       l_arr_integrados[l_i_iter].marca_sms

            LET l_arr_integrados[l_i_iter].tot_desmarca = l_arr_integrados[l_i_iter].desmarca_correo +
                                                          l_arr_integrados[l_i_iter].desmarca_sms

            LET l_arr_integrados[l_i_iter].tot_bloqueo = l_arr_integrados[l_i_iter].bloqueo_correo + 
                                                         l_arr_integrados[l_i_iter].bloqueo_sms
            
         END FOR

         -- se limpia el arreglo
         CALL l_arr_arch_int.clear()
         CALL DIALOG.setActionHidden("integrar",0)
         CALL DIALOG.setActionHidden("accept",1)

         CONTINUE DIALOG

      ON ACTION integrar
         --Solicita confirmar(1) o cancelar(0) la operación de Registro
         CALL fn_ventana_confirma("Atención","¿Desea ejecutar el proceso de Integración?","quest") RETURNING v_respuesta

        IF ( v_respuesta = 1 ) THEN
           CALL fn_glo_integracion(l_v_arch_proceso)
           EXIT DIALOG
        END IF

   END DIALOG
CLOSE WINDOW afil20
END FUNCTION -- fn_drgandrop_integra_devol

FUNCTION fn_glo_integracion(v_archivo)
   DEFINE v_s_comando        STRING -- cadena con una instruccion de consola
   DEFINE v_mensaje          STRING
   DEFINE v_i_resultado      INTEGER -- resultado del proceso
   DEFINE v_folio            LIKE glo_folio.folio
   DEFINE v_archivo          STRING
   DEFINE r_resultado_opera  SMALLINT  --CODIGO  DE ERROR fn_actualiza_opera_ini

   LET v_folio = 0

   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado

   IF ( v_i_resultado = 0 ) THEN

     -- Inicio operacion.
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario_cod) RETURNING v_folio

     CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               v_folio,
                               "AFIP20",
                               v_archivo,
                               g_usuario_cod)
             RETURNING r_resultado_opera

     IF ( r_resultado_opera = 0 ) THEN
       LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/AFIP20 ",
                         g_usuario_cod CLIPPED, " ",
                         g_pid                , " ",
                         g_proceso_cod        , " ",
                         g_opera_cod          , " ",
                         v_folio              , " '",
                         v_archivo CLIPPED    , "' ",
                         " 1>",seg_modulo_bat.ruta_listados clipped,
                         "/nohup:",g_pid USING "&&&&&",":",
                         g_proceso_cod   USING "&&&&&",":",
                         g_opera_cod     USING "&&&&&" ,
                         " 2>&1 &"
       DISPLAY v_s_comando
       RUN v_s_comando
       CALL fn_mensaje("Atención",
                        "Se ha enviado la integración.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
                        "information")
      ELSE
        CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
      END IF

   ELSE
      CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   END IF
END FUNCTION
