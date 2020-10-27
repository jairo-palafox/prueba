--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12-04-2012
--===============================================================

####################################################################
#Modulo            =>HPS                                           #
#Programa          =>HPSL22                                        #
#Objetivo          =>Registrar instrucciones de mandatos           #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>07 Febrero 2012                               #
#Fecha modificacion=>26 Marzo 2012                                 #
####################################################################
DATABASE safre_viv
DEFINE x char(1)
DEFINE prueba INTEGER 
DEFINE v_s_qryTxt        STRING
DEFINE v_r_mandato       RECORD LIKE mdt_ctr_mandato.*
DEFINE v_r_mandato_det   RECORD LIKE mdt_det_ctr_mandato.*
DEFINE v_r_solicitud_mandato   RECORD LIKE mdt_solicitud_mandato.*
DEFINE v_r_credito       RECORD LIKE cre_acreditado.*   -- Cambio de acr_transferencia por cre_acreditado
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      VARCHAR(30),              -- nombre del programa
       v_ventana         ui.Window,
       p_proceso_cod   LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
       p_opera_cod     LIKE cat_operacion.opera_cod, -- operacion de la etapa que llama la funcion
       r_pid   LIKE bat_ctr_operacion.pid

MAIN
   DEFINE v_arr_arch_pend   DYNAMIC ARRAY OF STRING, -- arreglo que contiene los lotes pendientes
          v_arr_archPendAux DECIMAL(9,0), -- arreglo auxiliar que contiene los lotes pendientes
          v_arr_arch_eleg   DYNAMIC ARRAY OF STRING, -- arreglo que contiene los archivos a elegir
          v_arr_elegidos    DYNAMIC ARRAY OF RECORD -- arreglo que contiene los archivos elegidos
             f_proceso        DATE,
             folio            INTEGER,
             tot_reg          INTEGER
          END RECORD,
          v_v_f_Lote        DATE, -- nombre del archivo en proceso
          v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
          v_drag_index      INTEGER, -- indice del drag
          v_drop_index      INTEGER, -- indice del drop
          v_drag_source     STRING, -- fuente del drag
          v_drag_value      STRING, -- valor del drag
          v_i_num_arch      SMALLINT, -- numero de archivos a elegir
          v_i_iter          SMALLINT, -- variable usada para iteracion
          v_i_iter_dep      SMALLINT, -- variable usada para iteracion
          v_i_indice        SMALLINT, -- indice del arrego de archivos pendientes
          
          v_i_opera_cod_ant LIKE cat_operacion.opera_cod, -- operacion de la etapa anterior
          --v_i_operacion     LIKE acr_ctr_archivo.operacion, -- operacion del proceso
          v_d_folio         LIKE glo_ctr_archivo.folio, -- folio
          v_d_pid           LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_c_ruta_bin_acr  LIKE seg_modulo.ruta_bin, -- ruta del bin de acr
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_msj           STRING, -- se asigna un mensaje que será presentado al usuario
          r_b_valida        SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_ruta_vacia      STRING
   DEFINE v_i_tot_reg_sol   INTEGER
          
          CONSTANT l_nom_tbl_pend = "tbl_pendiente" -- se asigna el nombre de la tabla de archivos pendientes
          CONSTANT l_nom_tbl_int = "tbl_elegido" -- se asigna el nombre de la tabla de archivos elegidos

   DEFINE w               ui.window
   DEFINE f               ui.form

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   LET p_proceso_cod = 3104 -- 46 anterior -- lanzar recurrentes AHM 2012 04 25 Cambio de procesos
   LET p_opera_cod = 3 -- genera archivo mandatos

   -- se inicializa el indice del arreglo
   LET v_i_indice = 1
   # Recupera el pid
   CALL fn_max_pid(p_proceso_cod,2) RETURNING r_pid
   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(r_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida
   
   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se asignan los valores necesarios para la intergraci
   -- TMP verificar si va esto LET v_i_proceso_cod = 1  -- LQINFO
   -- TMP verificar si va esto LET v_i_opera_cod = 2 -- Integracion de LQINFO
   -- TMP verificar si va esto LET v_i_opera_cod_ant = 1 -- Carga (Validar) de archivo
   -- TMP verificar si va esto LET v_i_operacion = 01 -- operacion del proceso
   -- TMP verificar si va esto LET v_d_folio = 0      --Se indica folio 0 para que el proceso de historico genere un nuevo folio
   -- TMP verificar si va esto LET v_c_programa_cod = "HPSL22"
   -- TMP verificar si va esto -- se crea la sentencia sql que obtiene el pid perteneciente al folio
   -- TMP verificar si va esto LET v_s_qryTxt = " SELECT MAX(pid)\n",
   -- TMP verificar si va esto                  "   FROM bat_ctr_proceso\n",
   -- TMP verificar si va esto                  "  WHERE proceso_cod = ",v_i_proceso_cod
   -- TMP verificar si va esto 
   -- TMP verificar si va esto PREPARE prp_unq_pid_batCtrProc FROM v_s_qryTxt
   -- TMP verificar si va esto EXECUTE prp_unq_pid_batCtrProc INTO v_d_pid
   -- TMP verificar si va esto 
   -- TMP verificar si va esto 
   -- TMP verificar si va esto -- se invoca la funcion que valida la operacion
   -- TMP verificar si va esto CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida
   -- TMP verificar si va esto 
   -- TMP verificar si va esto -- se verifica si la operacion en proceso es valida
   -- TMP verificar si va esto IF r_b_valida <> 0 THEN
   -- TMP verificar si va esto    -- en caso de error se muestra un mensaje a usuario y no continua
   -- TMP verificar si va esto    CALL fn_muestra_inc_operacion(r_b_valida)
   -- TMP verificar si va esto 
   -- TMP verificar si va esto    EXIT PROGRAM
   -- TMP verificar si va esto END IF
   -- TMP NO se utilizan las salidas ? Que procede --sE OBTIENEN las rutas de los ejecutables
   -- TMP NO se utilizan las salidas ? Que procede CALL fn_rutas("mdt") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
   -- TMP NO se utilizan las salidas ? Que procede CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat
   -- se crea la sentencia que busca los lotes disponibles por elegir
   LET v_s_qryTxt = " SELECT UNIQUE folio\n",
                    "   FROM hps_lote_mandato\n",
                    "  WHERE 1=1 ",
                    --"    AND proceso_cod = ", v_i_proceso_cod,"\n",
                    --"    AND opera_cod = ",v_i_opera_cod_ant,"\n",
                    "    AND id_origen = 1",  -- AHM TMP Validarlo si es correcto
                    "    AND estado = 101" -- Lote completo -- AHM 2010423 Ajuste de 101 a 102 para acreditados

   PREPARE prp_archivos_val FROM v_s_qryTxt
   DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 
   FOREACH cur_archivos_val INTO v_arr_archPendAux --  INTO v_arr_arch_pend[v_i_indice]
     
      -- se incrementa el indice del arreglo
      CALL v_arr_arch_pend.appendElement()
      LET v_arr_arch_pend[v_arr_arch_pend.getLength()] = v_arr_archPendAux
      
      LET v_i_indice = v_i_indice + 1
   END FOREACH

   -- se abre la ventana para elejir los archivos a elegir
   OPEN WINDOW w_elige_acred WITH FORM "HPSL22"
   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF
   -- Se define la ventana para manipular los objetos a visualizar de acuerdo a la operacion
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
      DIALOG ATTRIBUTE(UNBUFFERED)
         -- Despligue de folios pendientes
         DISPLAY ARRAY v_arr_arch_pend TO tbl_pendiente.*
            BEFORE DISPLAY
                           
            ON DRAG_START(v_ui_dnd)
               DISPLAY "v_ui_dnd: ",v_ui_dnd
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
         -- Despligue de lotes elegidos
         DISPLAY ARRAY v_arr_arch_eleg TO tbl_elegido.*
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = l_nom_tbl_int
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_arr_arch_eleg[v_drag_index]
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
                   IF v_arr_arch_eleg.getLength() = 1 THEN
                      CALL v_ui_dnd.dropInternal()
                      LET v_s_msj = "Solo se permite elegir un lote a la vez"
                      CALL fn_mensaje("Aviso",v_s_msj,"stop")
                   ELSE
                      LET v_drop_index = v_ui_dnd.getLocationRow()
                      CALL DIALOG.insertRow(l_nom_tbl_int, v_drop_index)
                      CALL DIALOG.setCurrentRow(l_nom_tbl_int, v_drop_index)
                      LET v_arr_arch_eleg[v_drop_index] = v_drag_value
                      CALL DIALOG.deleteRow(l_nom_tbl_pend, v_drag_index)
                   END IF
                END IF
         END DISPLAY
         -- Despligue de lotes elegidos y listos para procesar despues de aceptar la primera parte
         DISPLAY ARRAY v_arr_elegidos TO tbl_lotes_elegidos.*
         END DISPLAY
         BEFORE DIALOG
            -- Restaura botones para elegir los lotes pendientes (1ra parte)
            DISPLAY "BEFORE DIALOG"
            CALL dialog.setactionhidden("aceptar_eleccion",0)
            CALL dialog.setactionhidden("close",0)
            CALL dialog.setactionhidden("aceptar",TRUE)
            CALL dialog.setactionhidden("regresar",TRUE)
         -- Accion que controla la aceptacion de elegidos, desplegando detalle de lotes (1ra parte)
         ON ACTION aceptar_eleccion
            -- se obtiene el numero de lotes elegidos
            LET v_i_num_arch = v_arr_arch_eleg.getLength()
            -- en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_msj = "Debe arrastrar al menos un archivo a lotes elegidos"
               CALL fn_mensaje("Aviso",v_s_msj,"stop")
               CONTINUE DIALOG
            END IF
            -- se limpia el arreglo de los lotes ya elegidos
            CALL v_arr_elegidos.clear()
            -- Controla los lotes realmente procesados
            LET v_i_iter_dep = 0
            LET v_i_tot_reg_sol = 0
            -- se procesan los lotes seleccionados
            FOR v_i_iter = 1 TO v_i_num_arch
               -- Validacion de lotes nulos o vacios, para que no se procesen
               IF LENGTH(v_arr_arch_eleg[v_i_iter] CLIPPED) = 0 THEN 
                  CONTINUE FOR 
               END IF
               LET v_i_iter_dep = v_i_iter_dep + 1
               -- se asigna el lote en la variable 
               {LET v_v_f_Lote = v_arr_arch_eleg[v_i_iter]
               -- se asigna la informacion del lote elegido
               LET v_arr_elegidos[v_i_iter_dep].f_proceso = v_v_f_Lote
               LET v_s_qryTxt = " SELECT folio",
                                "   FROM mdt_lote_mandato",
                                "  WHERE f_proceso = '", v_v_f_Lote,"'",
                                "    AND id_origen = 1",  -- AHM TMP Validarlo si es correcto
                                "  GROUP BY 1"}
               
               -- AHM TMP Validar que sea uno a uno !!!!!!!!!!!!!!
               -- Recuperacion de datos complementarios del lote elegido
               {PREPARE EnuDetLote FROM v_s_qryTxt
               EXECUTE EnuDetLote INTO v_arr_elegidos[v_i_iter_dep].folio}
--PROMPT "indice: ",v_i_iter FOR CHAR x
--PROMPT "folio: ",v_arr_arch_eleg[v_i_iter] FOR CHAR x 
               
--               LET prueba = v_arr_arch_eleg[v_i_iter]
--PROMPT "PRUEBA",prueba FOR CHAR x               
 --              LET v_arr_elegidos[v_i_iter_dep].folio = prueba
               LET v_arr_elegidos[v_i_iter_dep].folio = v_arr_arch_eleg[v_i_iter]
--PROMPT "indice",v_i_iter_dep FOR CHAR x               
--PROMPT "folio: ",v_arr_elegidos[v_i_iter_dep].folio FOR CHAR x

               SELECT f_proceso
                 INTO v_arr_elegidos[v_i_iter_dep].f_proceso
                 FROM hps_lote_mandato
                WHERE folio = v_arr_elegidos[v_i_iter_dep].folio 
                                       
               LET v_s_qryTxt = " SELECT NVL(count(*),0)",
                                "   FROM hps_solicitud_mandato",
                                "  WHERE folio = ?"
                                
                                
               DISPLAY "Detalle sol_mandatos v_s_qryTxt - ",v_s_qryTxt
               -- AHM TMP Validar que sea uno a uno !!!!!!!!!!!!!!
               -- Recuperacion de datos complementarios del lote elegido
               PREPARE EnuDetSol FROM v_s_qryTxt
               EXECUTE EnuDetSol USING v_arr_elegidos[v_i_iter_dep].folio 
                                  INTO v_arr_elegidos[v_i_iter_dep].tot_reg
                                       
               LET v_i_tot_reg_sol = v_i_tot_reg_sol + v_arr_elegidos[v_i_iter_dep].tot_reg
            END FOR
            -- si se muestra la informacion del lote a elegir se habilitan los botones de la 2da parte
            IF v_arr_elegidos.getLength() > 0 THEN
               -- Se muestra la opcion del proceso batch 
               CALL dialog.setactionhidden("aceptar_eleccion",TRUE)
               CALL dialog.setactionhidden("close",TRUE)
               CALL dialog.setactionhidden("aceptar",FALSE)
               CALL dialog.setactionhidden("regresar",FALSE)
            ELSE
               LET v_s_msj = "Debe arrastrar al menos un archivo a lotes elegidos"
               CALL fn_mensaje("Aviso",v_s_msj,"stop")
            END IF
            CONTINUE DIALOG
         -- Accion que controla la aceptacion de elegidos y autorizacion para su registro en 
         -- mandatos e inicio de proceso batch (2da parte)
         ON ACTION aceptar
            IF v_i_tot_reg_sol > 0 THEN
               --IF FGL_WINQUESTION("Confirmación", "Desea continuar con el proceso ? ",
               --                   "yes", "yes|no", "question", 0) = "yes" THEN
               -- CALL fn_mensaje("Aviso","Proceso 46 asignado.","info") -- AHM 2012 04 25 Cambio de procesos
               
               --IF FGL_WINQUESTION("Confirmación", "Proceso 46 asignado.",
               --                   "yes", "yes", "question", 0) = "yes" THEN
                  LET v_i_num_arch = v_arr_elegidos.getLength()
                  
                  FOR v_i_iter = 1 TO v_i_num_arch
                     -- Inserta movimientos en mdt_ctr_mandato y mdt_det_ctr_mandato
                     -- por cada lote elegido
                     -- TMP AHM Y este insert cuando ? CALL Fn_Ins_Mandato_Recurrente(v_arr_elegidos[v_i_iter].lote, v_arr_elegidos[v_i_iter].id_lote_mandato)
                     -- Se lanzara proceso batch por lote
                     CALL lanza_archivo_mdt(v_arr_elegidos[v_i_iter].folio, v_i_iter, v_i_num_arch)
                  END FOR
                  EXIT DIALOG
               --ELSE
               --   CALL v_arr_elegidos.clear()
               --   -- Restaura botones para elegir nuevamente los lotes (1ra parte)
               --   CONTINUE DIALOG
               --END IF
            ELSE
               CALL fn_mensaje("Advertencia","No hay registros a procesar","error")
            END IF
         -- Accion que controla la cancelacion despues de haber aceptado los elegidos (1ra parte)
         ON ACTION regresar
            CALL v_arr_elegidos.clear()
            -- Restaura botones para elegir nuevamente los lotes (1ra parte)
            CALL dialog.setactionhidden("aceptar_eleccion",0)
            CALL dialog.setactionhidden("close",0)
            CALL dialog.setactionhidden("aceptar",TRUE)
            CALL dialog.setactionhidden("regresar",TRUE)
            CONTINUE DIALOG
         -- Accion que controla la cancelacion de elegidos (2da parte)
         ON ACTION CLOSE   -- cancelar_eleccion
            EXIT DIALOG
      END DIALOG
   CLOSE WINDOW w_elige_acred
END MAIN
######################################################################
#  Modulo:      Libreria de funciones generales de mandatos          #
#  Nombre:      Fn_Ins_Mandato_Recurrente                            #
#  Descripcion: Realiza la insercion a las tablas de mandatos:       #
#               mdt_ctr_mandato y mdt_det_ctr_mandato a partir de    #
#               la tabla de paso (hps_solicitud_mandato)                   #
#  Parametros:  Entrada: v_lote_ins - lote que sera buscado en la        #
#                        tabla de paso y los movimientos ecuperados  #
#                        son los que se insertan en las tablas de    #
#                        mandatos                                    #
#               Salida:  Ninguno                                     #
#       Fecha de creacion:   08-Feb-2012                             #
#       Responsable:         Alexandro Hollmann Montiel, EFP         #
######################################################################
FUNCTION Fn_Ins_Mandato_Recurrente(v_lote_ins, p_id_lote_mandato)
DEFINE v_lote_ins             LIKE mdt_solicitud_mandato.folio
DEFINE p_id_lote_mandato  LIKE mdt_ctr_mandato.id_lote_mandato
   LET v_s_qryTxt = "SELECT *",
                    "  FROM hps_solicitud_mandato",
                    " WHERE id_origen = 1",
                    "   AND folio = ",v_lote_ins,
                    " ORDER BY id_credito"
   -- seleccion de movimientos en mdt_solicitud_mandato por lote elegido
   PREPARE EnuEncMandato FROM v_s_qryTxt
   DECLARE CurEncMandato CURSOR FOR EnuEncMandato
   FOREACH CurEncMandato INTO v_r_solicitud_mandato.*
      
      -- Verifica la existencia del credito para ser insertado o no
      IF NOT fn_sql_exi_mandato(v_r_solicitud_mandato.id_credito) THEN
         -- TMP AHM va a cambior por funcion BD seriacion
         SELECT NVL(MAX(id_ctr_mandato),0) INTO v_r_mandato.id_ctr_mandato
           FROM mdt_ctr_mandato
         IF v_r_mandato.id_ctr_mandato IS NULL or v_r_mandato.id_ctr_mandato = 0 THEN
           LET v_r_mandato.id_ctr_mandato = 1
         ELSE
           LET v_r_mandato.id_ctr_mandato = v_r_mandato.id_ctr_mandato + 1
         END IF      
         
         -- AHM TMP Validar que sea relacion uno a uno
         SELECT * INTO v_r_credito.*
           FROM cre_acreditado   -- Cambio de acr_transferencia por cre_acreditado 
          WHERE num_credito = v_r_solicitud_mandato.id_credito
          
         -- LET v_r_mandato.id_ctr_mandato          = 
         -- Inicializacion del arreglo de encabezado de mandatos antes de su insercion
         LET v_r_mandato.id_derechohabiente      = v_r_solicitud_mandato.id_derechohabiente
         LET v_r_mandato.nss                     = v_r_solicitud_mandato.nss
         LET v_r_mandato.id_credito              = v_r_solicitud_mandato.id_credito
         LET v_r_mandato.f_lote                  = NULL --v_r_solicitud_mandato.
         LET v_r_mandato.lote                    = NULL--v_r_solicitud_mandato.folio
         LET v_r_mandato.id_lote                 = 0 --v_r_solicitud_mandato.id_lote
         LET v_r_mandato.tpo_credito             = v_r_credito.tpo_credito
         LET v_r_mandato.edo_credito             = v_r_credito.edo_credito
         LET v_r_mandato.tpo_descuento_credito   = v_r_credito.tpo_dscto
         LET v_r_mandato.valor_descuento_credito = v_r_credito.valor_dscto
         LET v_r_mandato.estado                  = 100
         LET v_r_mandato.tpo_prelacion           = '' -- Por definir origen
         LET v_r_mandato.usuario                 = p_v_usuario
         LET v_r_mandato.id_lote_mandato         = NULL-- p_id_lote_mandato
         
         -- Inicializacion del arreglo de detalle de mandatos antes de su insercion (1ra parte)
         LET v_r_mandato_det.id_ctr_mandato      = v_r_mandato.id_ctr_mandato
         LET v_r_mandato_det.id_derechohabiente  = v_r_mandato.id_derechohabiente
         LET v_r_mandato_det.nss                 = v_r_mandato.nss
         
         INSERT INTO mdt_ctr_mandato VALUES(v_r_mandato.*)
      
      END IF
      
      -- TMP AHM va a cambior por funcion BD seriacion
      SELECT NVL(MAX(id_det_ctr_mandato),0) INTO v_r_mandato_det.id_det_ctr_mandato
        FROM mdt_det_ctr_mandato
      IF v_r_mandato_det.id_det_ctr_mandato IS NULL or v_r_mandato_det.id_det_ctr_mandato = 0 THEN
        LET v_r_mandato_det.id_det_ctr_mandato = 1
      ELSE
        LET v_r_mandato_det.id_det_ctr_mandato = v_r_mandato_det.id_det_ctr_mandato + 1
      END IF      
      
      --SELECT UNIQUE id_cat_mandato INTO v_r_mandato_det.id_cat_mandato
      --  FROM mdt_cat_mandato
      -- WHERE id_mandato = v_r_solicitud_mandato.id_mandato
      -- Inicializacion del arreglo de detalle de mandatos antes de su insercion (2da parte)
      -- AHM Eliminado 2012-02-10 LET v_r_mandato_det.id_mandato              = v_r_solicitud_mandato.id_mandato
      SELECT id_cat_mandato
        INTO v_r_mandato_det.id_cat_mandato
        FROM mdt_cat_mandato_paquete
       WHERE cve_mandato = v_r_solicitud_mandato.cve_mandato
      -- Se recupera mandato del catálogo a partir del id_mandato de tabla de paso
      {SELECT UNIQUE id_cat_mandato INTO v_r_mandato_det.id_cat_mandato
        FROM mdt_cat_mandato
       WHERE id_mandato = v_r_solicitud_mandato.id_mandato}

      LET v_r_mandato_det.tpo_descuento_mandato   = v_r_solicitud_mandato.tpo_descuento_mandato  
      LET v_r_mandato_det.valor_descuento_mandato = v_r_solicitud_mandato.valor_descuento_mandato
      LET v_r_mandato_det.f_inicio_mandato        = v_r_solicitud_mandato.f_inicio_mandato       
      LET v_r_mandato_det.f_culmina_mandato       = v_r_solicitud_mandato.f_culmina_mandato      
      LET v_r_mandato_det.referencia              = v_r_solicitud_mandato.referencia
      LET v_r_mandato_det.scta_origen_descuento   = v_r_solicitud_mandato.scta_origen_descuento
      LET v_r_mandato_det.movimiento              = NULL
      LET v_r_mandato_det.modalidad_aplicacion    = v_r_solicitud_mandato.modalidad_aplicacion
      LET v_r_mandato_det.f_presentacion          = NULL
      INSERT INTO mdt_det_ctr_mandato VALUES(v_r_mandato_det.*)
      
   END FOREACH
END FUNCTION
######################################################################
#  Modulo:      Libreria de funciones generales de mandatos          #
#  Nombre:      fn_sql_exi_mandato                                   #
#  Descripcion: Verifica la existencia del credito para insertarse   #
#               en la tabla maestro de mandatos                      #
#  Parametros:  Entrada: p_id_credito - Credito a validar si existe  #
#               Salida:  Verdadero si existe el credito, false en    #
#                        caso contrario                              #
#       Fecha de creacion:   08-Feb-2012                             #
#       Responsable:         Alexandro Hollmann Montiel, EFP         #
######################################################################
FUNCTION fn_sql_exi_mandato(p_id_credito)
DEFINE p_id_credito    LIKE mdt_ctr_mandato.id_credito
DEFINE v_i_count       INTEGER
   SELECT NVL(COUNT(*),0) INTO v_i_count
     FROM mdt_ctr_mandato
    WHERE id_credito = p_id_credito
    
   IF v_i_count > 0 THEN
      RETURN TRUE
   END IF
   RETURN FALSE
END FUNCTION

####################################################################
#Modulo            =>HPSL                                          #
#Programa          =>HPSL22                                        #
#Objetivo          =>                                              #
#Autor             =>                                              #
#Modificación      => Hugo César Ramírez García                    #
#                     cambio de nombres de columnas de tablas,     #
#                     ajuste para considerar la carga de           #
#                     recurrentes en mandatos  28 Junio 2012       #
#Fecha inicio      => 2012                                         #
####################################################################
FUNCTION lanza_archivo_mdt(p_folio, p_cont_lote, p_tot_lote)
   DEFINE p_folio             INTEGER
   DEFINE --v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          --v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_c_ruta_bin_mdt    LIKE seg_modulo.ruta_bin, -- ruta del bin de mdt
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_comando         STRING, -- contiene al comando a correr
          v_folio_mandato     INTEGER, -- folio
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_ruta_vacia        STRING 
   DEFINE v_cadena_pid        CHAR(5)
   DEFINE v_cadena_proc       CHAR(5)
   DEFINE v_cadena_opera      CHAR(5)
   DEFINE p_cont_lote         INTEGER
   DEFINE p_tot_lote          INTEGER
          
   -- se inicializan las variables
   
   --LET v_d_pid = 0
   LET v_v_nom_archivo = "NA"
   LET v_c_programa_cod = "HPSP22"

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(r_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida
   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
   ELSE
      -- ahora es la operacion 3 CALL fn_genera_pid(p_proceso_cod
      -- ahora es la operacion 3                   ,p_opera_cod
      -- ahora es la operacion 3                   ,p_v_usuario)
      -- ahora es la operacion 3                   RETURNING v_d_pid
      -- ahora es la operacion 3 
      -- ahora es la operacion 3 --CALL fn_obten_folio(v_i_proceso_cod,v_i_opera_cod) RETURNING v_folio_mandato
      -- ahora es la operacion 3 CALL fn_genera_folio(p_proceso_cod, p_opera_cod,p_v_usuario)  RETURNING v_folio_mandato
      -- ahora es la operacion 3 --sE OBTIENEN las rutas de los ejecutables
      -- ahora es la operacion 3 CALL fn_inicializa_proceso(v_d_pid,
      -- ahora es la operacion 3                            p_proceso_cod, 
      -- ahora es la operacion 3                            p_opera_cod,
      -- ahora es la operacion 3                            v_folio_mandato,
      -- ahora es la operacion 3                            v_c_programa_cod,
      -- ahora es la operacion 3                            v_v_nom_archivo,
      -- ahora es la operacion 3                            p_v_usuario) RETURNING r_b_valida
      -- ahora es la operacion 3 IF r_b_valida <> 0 THEN
      -- ahora es la operacion 3    -- en caso de error se muestra un mensaje a usuario y no continua
      -- ahora es la operacion 3    CALL fn_muestra_inc_operacion(r_b_valida)
      -- ahora es la operacion 3    RETURN
      -- ahora es la operacion 3 END IF
      -- se invoca la funcion que inicializa la operacion
      LET r_b_valida = fn_actualiza_opera_ini(r_pid, p_proceso_cod, p_opera_cod,
                                             v_folio_mandato, v_c_programa_cod,
                                             v_v_nom_archivo, p_v_usuario)
   
      -- se verifica si fue posible inicializar el proceso
      IF r_b_valida = 0 THEN
         CALL fn_rutas("hps") RETURNING v_c_ruta_bin_mdt, v_ruta_vacia
         CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat
         -- se crean las cadenas para el nombre del archivo log
         --LET v_cadena_pid   = v_d_pid USING "&&&&&"
         --LET v_cadena_proc  = v_i_proceso_cod USING "&&&&&"
         --LET v_cadena_opera = v_i_opera_cod USING "&&&&&" 
         -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidación
         LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin_mdt CLIPPED,"/HPSP22 ",
                                                 p_v_usuario, " ",
                                                 r_pid, " ",
                                                 p_proceso_cod, " ",
                                                 p_opera_cod, " ",
                                                 p_folio, " '",
                                                 v_v_nom_archivo, 
                           "' 1> ",v_c_ruta_list_bat CLIPPED,
                           "/nohup:",r_pid USING "&&&&&",":",
                                     p_proceso_cod USING "&&&&&",":",
                                     p_opera_cod USING "&&&&&",
                           " 2>&1 &"

         DISPLAY v_s_comando
         RUN v_s_comando      
         IF(STATUS)THEN
            CALL fn_mensaje(v_c_programa_cod,"Ocurrio un error al ejecutar: Origen recurrente","about")
         ELSE
            --CALL fn_mensaje("Aviso","Se ejecutó el proceso de generación de archivo del lote: "||p_f_lote,"info")
            --LET v_c_programa_cod = "HPSP22"
            --CALL fn_mensaje("Aviso","Proceso "||v_d_pid||" asignado.","info")
            CALL fn_mensaje("Aviso","Se ha enviado la operacion con pid "||r_pid||".\nPodrá revisar el detalle en el monitoreo de procesos","info")

           -- se invoca la función que deja la operación en estado Procesando
          --   LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
          --                                           v_folio_mandato, v_c_programa_cod,
          --                                           p_f_lote, p_v_usuario)

            -- se verifica si fue posible inicializar la operacion
          --IF r_b_valida <> 0 THEN
             -- en caso de error se muestra un mensaje a usuario y no continua
             --   CALL fn_muestra_inc_operacion(r_b_valida)
            --    EXIT PROGRAM
            --END IF
         END IF
      ELSE
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(r_b_valida)
         DISPLAY "ERROR en fn_inicializa_proceso"
      END IF
   END IF

END FUNCTION
