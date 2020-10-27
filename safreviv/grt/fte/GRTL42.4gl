--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#####################################################################
#Modulo            =>GRT                                            #
#Programa          =>GRTL42                                         #
#Objetivo          =>Programa que pide al usuario la seleccion del  #
#                    proceso a reversar: Archivos Entrada,          #
#                    Archivos Salida, Transacciones. Posteriormente #
#                    se ejecuta el programa correspondiente. Módulo #
#                    Créditos en Garantía 43 bis                    #
#Autor             =>Daniel Buendia, EFP                            #
#Fecha inicio      =>30 Mayo 2012                                   #
#####################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- usuario firmado al sistema
       p_b_tipo_carga      SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog        VARCHAR(30), -- nombre del programa
       m_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
       m_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
       m_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
       m_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
       m_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
       m_c_tpo_transf      LIKE dse_agrupa_devolucion.tpo_transferencia --  tipo de transferencia

MAIN
   DEFINE v_c_op_arch_ent   VARCHAR(5), -- selección del modulo a reversar (Archivos entrada)
          v_c_op_arch_sal   VARCHAR(5), -- selección del modulo a reversar (Archivos salida)
          v_ci_op_arch_tran VARCHAR(5), -- selección del modulo a reversar (Transacciones)
          v_d_pid           DECIMAL(9,0), -- identificador del proceso
          v_d_folio         LIKE bat_ctr_operacion.folio, -- folio
          v_s_qryTxt        STRING, -- se asigna una sentencia sql a ejecutar
          r_c_ruta_listados LIKE seg_modulo.ruta_listados, -- ruta listados del módulo 
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- recupera los parametros que se envian de la principal
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".GRTL42.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inializan variables
   LET m_i_proceso_cod = g_proc_cod_grt_reverso_dse -- reverso dev sdo exc
   LET m_i_opera_cod = 1 -- reverso dev sdo exc
   LET v_d_pid = 0
   LET v_d_folio = 0
   LET m_c_programa_cod = "GRTL42"
   LET v_c_op_arch_ent = NULL
   LET v_c_op_arch_sal = NULL
   LET v_ci_op_arch_tran = NULL
   LET m_c_tpo_transf = 19 -- Creditos en garantia

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,m_i_proceso_cod,m_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING m_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO m_c_ruta_list_bat

   OPEN WINDOW w_reverso WITH FORM "GRTL421"
   INPUT v_c_op_arch_ent, v_c_op_arch_sal, v_ci_op_arch_tran WITHOUT DEFAULTS
    FROM gentrada, gsalida, gtransacciones ATTRIBUTES(UNBUFFERED)
      ON CHANGE gentrada
         -- al realizar un cambio en la opcion de Archivos entrada, anula las demás
         LET v_c_op_arch_sal = NULL
         LET v_ci_op_arch_tran = NULL

      ON CHANGE gsalida
         -- al realizar un cambio en la opcion de Archivos entrada, anula las demás
         LET v_c_op_arch_ent = NULL
         LET v_ci_op_arch_tran = NULL

      ON CHANGE gtransacciones
         -- al realizar un cambio en la opcion de Archivos entrada, anula las demás
         LET v_c_op_arch_ent = NULL
         LET v_c_op_arch_sal = NULL

      AFTER FIELD gtransacciones
         NEXT FIELD gentrada

      ON ACTION ACCEPT
         -- se valida la operacion seleccionada
         IF v_c_op_arch_ent IS NULL AND v_c_op_arch_sal IS NULL AND
            v_ci_op_arch_tran IS NULL THEN
            CALL fn_mensaje("Reverso","Debe seleccionar un módulo a reversar para continuar","stop")
            CONTINUE INPUT
         END IF

         -- varifica si el usuario selecciono la opción Archivos Entrada a reversar
         IF v_c_op_arch_ent IS NOT NULL THEN
            CALL fn_reversa_arch_entrada(v_c_op_arch_ent)
         END IF

         -- varifica si el usuario selecciono la opción Archivos Salida a reversar
         IF v_c_op_arch_sal IS NOT NULL THEN
            CALL fn_reversa_arch_salida()
         END IF

         -- varifica si el usuario selecciono la opción Transacciones a reversar
         IF v_ci_op_arch_tran IS NOT NULL THEN
            CALL fn_reversa_transacciones(v_ci_op_arch_tran)
         END IF

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_reverso
END MAIN

#Objetivo: Reverso de Archivos Entrada, permite la seleccion del archivo a reversar
FUNCTION fn_reversa_arch_entrada(p_c_op_arch_ent)
   DEFINE p_c_op_arch_ent     VARCHAR(5), -- contiene la opcion a reversar de Archivos entrada
          v_nom_grupo         VARCHAR(30), -- texto en el grupo que mostrara en la forma
          v_v_nom_archivo     LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_i_lote            LIKE dse_ctr_archivo.lote, -- lote del archivo
          v_fecha_lote        LIKE dse_ctr_archivo.f_lote, -- feche de lote
          v_i_proceso_cod_rev LIKE cat_proceso.proceso_cod, -- proceso a reversar
          v_i_opera_cod_rev   LIKE cat_operacion.opera_cod, -- operación a reversar
          v_d_pid_rev         LIKE bat_ctr_operacion.pid, -- pid a reversar
          v_d_pid             LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_d_folio           LIKE bat_ctr_operacion.folio, -- folio
          v_d_folio_arch      LIKE bat_ctr_operacion.folio, -- folio del archivo elegido
          v_ar_tbl_archivos   DYNAMIC ARRAY OF RECORD
             nom_archivo      LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
             f_lote           LIKE dse_ctr_archivo.f_lote, -- fecha de lote
             lote             LIKE dse_ctr_archivo.lote, -- lote
             f_proceso        LIKE dse_ctr_archivo.f_proceso, -- fecha de proceso
             tot_registros    LIKE dse_ctr_archivo.tot_registros, -- numero total de registros
             folio            LIKE dse_ctr_archivo.folio -- folio
          END RECORD,
          v_ui_ventana        UI.WINDOW, -- manejador de la ventana
          v_ui_forma          UI.FORM, -- menejador de la forma
          v_b_continua        SMALLINT, -- booleana que indica si el proceso continua
          v_i_indice_arr      SMALLINT, -- indice del arreglo
          v_i_indice_slct     SMALLINT, -- indice seleccionado
          v_ban_salir         SMALLINT, -- booleana que indica si el usuario canceló la operación
          v_s_condicion       STRING, -- contiene la condición de la sentencia sql del construct
          v_s_mensaje         STRING, -- contiene un mensaje a mostrar a usuario
          v_s_qryTxt          STRING, -- se asigna una sentencia sql a ejecutar
          v_s_comando         STRING, -- contiene al comando a correr
          r_v_extension         LIKE cat_operacion.extension, -- extensión del archivo
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se inicializan variables
   LET v_d_folio = 0
   LET v_d_folio_arch = 0
   LET v_d_pid = 0

   -- se verifica la opción seleccionada para poner nombre en el grupo
   CASE p_c_op_arch_ent
      WHEN "opt1"
         LET v_nom_grupo = "Devolución de Saldos"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_dse -- devolución de saldos excedentes acr
         LET v_i_opera_cod_rev = 2
      WHEN "opt2"
         LET v_nom_grupo = "Recepción Rechazo Devolución"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_rech_dse -- recepción rechazo devolución saldos exc acr
         LET v_i_opera_cod_rev = 2
      WHEN "opt3"
         LET v_nom_grupo = "Recepción Confirmación Devolución"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_conf_dse -- recepción confirmación saldos exc acr
         LET v_i_opera_cod_rev = 2
   END CASE

   -- se obtiene el del proceso y operación a reversar
   LET v_d_pid_rev = fn_max_pid(v_i_proceso_cod_rev, v_i_opera_cod_rev)

   -- se valida el reverso
   LET r_b_valida = fn_valida_reverso(v_d_pid_rev, v_i_proceso_cod_rev, v_i_opera_cod_rev)

   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      RETURN
   END IF

   OPEN WINDOW w_arch_entrada WITH FORM "GRTL422"
      -- se deplega el nombre del proceso a reversar en el grupo
      LET v_ui_ventana = ui.Window.getCurrent()
      LET v_ui_forma = v_ui_ventana.getForm()
      CALL v_ui_forma.setElementText("grevent",v_nom_grupo)
   
      INPUT v_v_nom_archivo,v_i_lote,v_fecha_lote
       FROM nom_archivo, lote, f_lote
      ATTRIBUTES ( UNBUFFERED )
      
      ON ACTION ACCEPT         
         -- se validan las variables ingresadas
         IF v_v_nom_archivo IS NULL AND v_i_lote IS NULL AND v_fecha_lote IS NULL THEN
            CALL fn_mensaje("Reverso Archivo Entrada","Debe ingresar el nombre del archivo y/o lote y fecha para continuar","stop")
            CONTINUE INPUT
         END IF

         -- se valida el lote y la fecha
         IF (v_i_lote IS NULL AND v_fecha_lote IS NOT NULL) THEN
            CALL fn_mensaje("Reverso Archivo Entrada","Se debe ingresar el lote del archivo a reversar","stop")
            CONTINUE INPUT
         END IF
         IF (v_i_lote IS NOT NULL AND v_fecha_lote IS NULL) THEN
            CALL fn_mensaje("Reverso Archivo Entrada","Se debe ingresar la fecha de lote/presentación del archivo a reversar","stop")
            CONTINUE INPUT
         END IF

         -- se validan el nombre del archivo
         IF v_v_nom_archivo IS NOT NULL THEN
            LET v_b_continua = fn_valida_ext_archivo(p_c_op_arch_ent, v_v_nom_archivo)

            IF NOT v_b_continua THEN
               CONTINUE INPUT
            END IF
         END IF

         -- se invoca la función que obtiene la extensión el archivo
         CALL fn_obtiene_ext_archivo(p_c_op_arch_ent) RETURNING r_v_extension

         -- se valida la extensión obtenida
         IF r_v_extension IS NULL THEN
            CALL fn_mensaje("Reverso Archivo Entrada","No fue posible obtener la extensión del archivo","stop")
            CONTINUE INPUT 
         END IF

         -- se construye la condicion
         LET v_s_condicion = "1=1\n"
         
         IF ( v_v_nom_archivo IS NOT NULL ) THEN
            LET v_s_condicion = v_s_condicion, "AND nom_archivo ='",v_v_nom_archivo,"'\n"
         ELSE
            LET v_s_condicion = v_s_condicion, "AND nom_archivo LIKE '%",r_v_extension CLIPPED,"'\n"
         END IF

         IF ( v_i_lote IS NOT NULL ) THEN
            LET v_s_condicion = v_s_condicion, "AND lote =", v_i_lote, "\n"
         END IF

         IF ( v_fecha_lote IS NOT NULL ) THEN
            LET v_s_condicion = v_s_condicion, "AND f_lote ='", v_fecha_lote, "'\n"
         END IF
         EXIT INPUT 

      ON ACTION CANCEL
         LET v_ban_salir = TRUE  
         EXIT INPUT
   END INPUT     

   IF NOT v_ban_salir THEN 
      -- se inicializan las variables
      LET v_i_indice_arr = 1

      -- se realizará la busqueda por el nombre del archivo lote y fecha
      LET v_s_qryTxt = " SELECT nom_archivo, f_lote, lote, f_proceso, tot_registros, folio\n",
                       "   FROM dse_ctr_archivo\n",
                       "  WHERE ",v_s_condicion

      PREPARE prp_busca_infoArch FROM v_s_qryTxt
      DECLARE cur_busca_infoArch CURSOR FOR prp_busca_infoArch

      FOREACH cur_busca_infoArch INTO v_ar_tbl_archivos[v_i_indice_arr].*
         LET v_i_indice_arr = v_i_indice_arr + 1
      END FOREACH

      -- verifica si se encotraron archivos con el nombre dado
      IF v_i_indice_arr = 1 THEN
         CALL fn_mensaje("Reverso Archivo Entrada","No se encontraron archivos cargados con el nombre dado","stop")
      ELSE
         DISPLAY ARRAY v_ar_tbl_archivos TO tabla_reverso.*
            ON ACTION ACCEPT
               -- se obtiene el indice seleccionado por el usuario
               LET v_i_indice_slct = arr_curr()

               -- se asigna el nombre y el folio del archivo seleccionado
               LET v_v_nom_archivo = v_ar_tbl_archivos[v_i_indice_slct].nom_archivo
               LET v_d_folio_arch = v_ar_tbl_archivos[v_i_indice_slct].folio

               -- verifica si el usuario selecciono un registro
               IF v_i_indice_slct IS NULL THEN
                  CALL fn_mensaje("Reverso Archivo Entrada","Debe seleccionar un archivo a reversar para continuar","stop")
                  CONTINUE DISPLAY
               END IF

               -- se verifica la opción seleccionada para validar el reverso
               CASE p_c_op_arch_ent
                  WHEN "opt1" -- Devolución de Saldos
                     CALL fn_valida_dev_sdos(v_d_folio_arch) RETURNING v_b_continua
                  WHEN "opt2" -- Recepción Rechazo Devolución
                     CALL fn_valida_rech_dev(v_d_folio_arch) RETURNING v_b_continua
                  WHEN "opt3" -- Recepción Confirmación Devolución
                     CALL fn_valida_conf_dev(v_d_folio_arch) RETURNING v_b_continua
               END CASE

               DISPLAY ">>Continua: ",v_b_continua

               -- si la validacion no fue aceptada no continua con el reverso
               IF NOT v_b_continua THEN
                  CONTINUE DISPLAY
               END IF

               -- se invoca la funcion que genera el PID
               CALL fn_genera_pid(m_i_proceso_cod,m_i_opera_cod,p_v_usuario) RETURNING v_d_pid 

               -- se invoca la funcion que inicializa el proceso
               LET r_b_valida = fn_inicializa_proceso(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                      v_d_folio, m_c_programa_cod,
                                                      v_v_nom_archivo, p_v_usuario)

               IF r_b_valida = 0 THEN
                  -- se invoca la función que deja la operación en estado Procesando
                  LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                          v_d_folio, m_c_programa_cod,
                                                          v_v_nom_archivo, p_v_usuario)

                  -- se verifica si fue posible inicializar la operacion
                  IF r_b_valida = 0 THEN
                     -- se crea el comando que ejecuta el reverso del archivo de entrada seleccionado
                     LET v_s_comando = " nohup time fglrun ",m_c_ruta_bin CLIPPED,"/GRTR06 ",
                                                             p_v_usuario, " ",
                                                             v_d_pid, " ",
                                                             m_i_proceso_cod, " ",
                                                             m_i_opera_cod, " ",
                                                             v_d_folio_arch, " ",
                                                             v_v_nom_archivo, " ",
                                                             v_d_pid_rev, " ",
                                                             p_c_op_arch_ent, " 1> ",
                                                             m_c_ruta_list_bat CLIPPED,
                                                             "/nohup:",v_d_pid USING "&&&&&",":",
                                                             m_i_proceso_cod USING "&&&&&",":",
                                                             m_i_opera_cod USING "&&&&&",
                                                             " 2>&1 &"

                     --DISPLAY v_s_comando
                     RUN v_s_comando

                     -- se asigna el mensaje a mostrar al usuario
                     LET v_s_mensaje = "Se ha enviado el Reverso de ",v_nom_grupo CLIPPED, " con PID: ",v_d_pid CLIPPED,
                                       ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
                     CALL fn_mensaje("Aviso",v_s_mensaje,"information")
                  ELSE
                     -- en caso de error se muestra un mensaje a usuario y no continua
                     CALL fn_muestra_inc_operacion(r_b_valida)
                     --EXIT PROGRAM
                  END IF
               ELSE
                  -- en caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)
                  --EXIT PROGRAM
               END IF

               EXIT DISPLAY

            ON ACTION CANCEL
               EXIT DISPLAY
         END DISPLAY
      END IF
   END IF
   CLOSE WINDOW w_arch_entrada
END FUNCTION

#Objetivo: Valida la extención del archivo dado como parametro
FUNCTION fn_valida_ext_archivo(p_c_op_arch_ent, p_c_nom_archivo)
   DEFINE p_c_op_arch_ent   VARCHAR(5), -- contiene la opcion a reversar de Archivos entrada
          p_c_nom_archivo   LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_i_proceso_cod   LIKE cat_operacion.proceso_cod, -- codigo del proceso
          v_i_opera_cod     LIKE cat_operacion.opera_cod, -- codigo de la operación
          v_v_extension     LIKE cat_operacion.extension, -- extensión del archivo
          v_i_long_arch     SMALLINT, -- longitud del archivo
          v_s_nom_archivo   STRING, -- nombre del archivo
          v_i_arch_valido   SMALLINT, -- booleana que indica si el archivo es valido
          v_si_indice       SMALLINT, -- indice del nombre del archivo
          v_s_qryTxt        STRING -- se asigna una sentencia sql a ejecutar

   -- se asume que el archivo será valido
   LET v_i_arch_valido = TRUE

   -- se verifica la opción seleccionada para poner identificar la extension correspondiente
   CASE p_c_op_arch_ent
      WHEN "opt1" -- Devolución de Saldos Excedentes
         LET v_i_proceso_cod = g_proc_cod_grt_dse -- devolución de saldos excedentes grt
         LET v_i_opera_cod = 1 -- valida devolución saldos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt2" -- Recepción Rechazo Devolución Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_rech_dse -- recepción rechazo devolución saldos exc grt
         LET v_i_opera_cod = 1 -- Valida Rechazos Devolución

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt3" -- Recepción Confirmación Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_conf_dse -- recepción confirmación saldos exc grt
         LET v_i_opera_cod = 1 -- valida confirmación devolución

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt4" -- Salida Solicitud de Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_arch_solic_dse -- generación solicitud devolución sdo exc grt
         LET v_i_opera_cod = 1 -- solicitud devolución saldo excedente grt

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod
   END CASE

   PREPARE prp_consulta_ext FROM v_s_qryTxt
   EXECUTE prp_consulta_ext INTO v_v_extension

   -- se asigna el nombre del archivo en una variable STRING
   LET v_s_nom_archivo = p_c_nom_archivo CLIPPED

   -- se obtiene la longitud del archivo
   LET v_i_long_arch = v_s_nom_archivo.getlength()

      -- se procesa el nombre del archivo para obtener la extensión
   FOR v_si_indice = v_i_long_arch TO 1 STEP -1
     IF v_s_nom_archivo.subString(v_si_indice, v_si_indice) = "." THEN
       EXIT FOR
     END IF
   END FOR

   IF v_s_nom_archivo.subString(v_si_indice+1,v_i_long_arch) <> v_v_extension THEN
      DISPLAY "DIFERENTES:"
      DISPLAY v_s_nom_archivo.subString(v_si_indice+1,v_i_long_arch), "<>", v_v_extension
      CALL fn_mensaje("Reverso","El archivo ingresado no es del tipo de archivo elegido para reversar","stop")
      LET v_i_arch_valido = FALSE
   END IF

   RETURN v_i_arch_valido
END FUNCTION

#Objetivo: Reverso de Transacciones, permite la captura de la transaccion a reversar
FUNCTION fn_reversa_transacciones(p_c_op_arch_tran)
   DEFINE p_c_op_arch_tran    VARCHAR(5), -- contiene la opcion a reversar transacciones
          v_d_folio           LIKE glo_ctr_archivo.folio, -- numero de folio
          v_d_pid             LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_i_cta_reg         SMALLINT, --contador de registros
          v_s_comando         STRING, -- contiene al comando a correr
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_v_nom_archivo     LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo 
          v_i_num_edo_transac INTEGER,
          v_ui_win            ui.Window,
          v_ui_form           ui.Form,
          v_s_qryTxt          STRING, -- contiene sentencia sql a ejecutar
          v_i_cta_reg_dset    SMALLINT --contador de registros de la tabla dse transferencia
          
   -- se inicializan variables
   LET v_d_pid = 0
   LET v_d_folio = NULL

   OPEN WINDOW w_arch_transaccion WITH FORM "GRTL424"
   --se captura el folio a reversar
   INPUT v_d_folio WITHOUT DEFAULTS 
   FROM folio ATTRIBUTES(UNBUFFERED)
      BEFORE INPUT
         IF p_c_op_arch_tran = "opt5" THEN
            LET v_ui_win = ui.Window.getCurrent()
            LET v_ui_form = v_ui_win.getForm()
            CALL v_ui_form.setElementText("label1", "Folio Agrupación")
         END IF

      AFTER FIELD folio
         NEXT FIELD folio

      ON ACTION ACCEPT   
         IF v_d_folio IS NULL THEN
            CALL fn_mensaje("Reverso de Transacciones","Debe ingresar el folio para continuar","stop")
            CONTINUE INPUT 
         END IF
         
         --se inicializa el contador de registros
         LET v_i_cta_reg = 0

         -- se verifica si la opción a reversar es Agrupación
         IF p_c_op_arch_tran = "opt5" THEN 
            --se consulta si existe el folio
            SELECT COUNT(*)
              INTO v_i_cta_reg
              FROM dse_agrupa_devolucion
             WHERE id_dse_grp_devolucion IN (
                   SELECT id_dse_grp_devolucion
                     FROM dse_his_devolucion
                    WHERE tpo_transferencia = m_c_tpo_transf
                      AND folio = v_d_folio) -- folio del archivo
               AND tpo_transferencia = m_c_tpo_transf

            --si no existe el folio se muestra mensaje
            IF v_i_cta_reg = 0 THEN
               CALL fn_mensaje("Reverso de Transacciones","Folio a reversar no localizado","stop")   
               CONTINUE INPUT 
            ELSE
               -- validación de la agrupación
               SELECT COUNT(*)
                 INTO v_i_cta_reg
                 FROM dse_agrupa_devolucion
                WHERE id_dse_grp_devolucion IN (
                      SELECT id_dse_grp_devolucion
                        FROM dse_his_devolucion
                       WHERE folio = v_d_folio
                         AND tpo_transferencia = m_c_tpo_transf)
                  AND tpo_transferencia = m_c_tpo_transf
                  AND estado = 20
                  AND edo_procesar IN (5, 10, 20)

               --si no existen registros se muestra mensaje                                          
               IF v_i_cta_reg = 0 THEN
                  CALL fn_mensaje("Reverso de Transacciones","No hay registros localizados en agrupación ó los registros ya fueron preliquidados o liquidados","stop")   
                  CONTINUE INPUT 
               ELSE
                  -- se invoca la funcion que genera el PID
                  CALL fn_genera_pid(m_i_proceso_cod,m_i_opera_cod,p_v_usuario) RETURNING v_d_pid 

                  -- se invoca la funcion que inicializa el proceso
                  LET r_b_valida = fn_inicializa_proceso(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                         v_d_folio, m_c_programa_cod,
                                                         v_v_nom_archivo, p_v_usuario)

                  IF r_b_valida = 0 THEN
                     -- se invoca la función que deja la operación en estado Procesando
                     LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                             v_d_folio, m_c_programa_cod,
                                                             v_v_nom_archivo, p_v_usuario)

                     -- se verifica si fue posible inicializar la operacion
                     IF r_b_valida = 0 THEN
                        -- se crea el comando que ejecuta el reverso de la transacción seleccionada
                        LET v_s_comando = " nohup time fglrun ",m_c_ruta_bin CLIPPED,"/GRTR07 ",
                                                                p_v_usuario, " ",
                                                                v_d_pid, " ",
                                                                m_i_proceso_cod, " ",
                                                                m_i_opera_cod, " ",
                                                                v_d_folio, " ",
                                                                p_c_op_arch_tran, " 1> ",
                                                                m_c_ruta_list_bat CLIPPED,
                                                                "/nohup:",v_d_pid USING "&&&&&",":",
                                                                m_i_proceso_cod USING "&&&&&",":",
                                                                m_i_opera_cod USING "&&&&&",
                                                                " 2>&1 &"

                        --DISPLAY v_s_comando
                        RUN v_s_comando

                        CALL fn_mensaje("Aviso","Se ha ejecutado el proceso de reverso de transacciones","information")
                     ELSE
                        -- en caso de error se muestra un mensaje a usuario y no continua
                        CALL fn_muestra_inc_operacion(r_b_valida)
                        --EXIT PROGRAM
                     END IF
                  ELSE
                     -- en caso de error se muestra un mensaje a usuario y no continua
                     CALL fn_muestra_inc_operacion(r_b_valida)
                     --EXIT PROGRAM
                  END IF                                                      
                  EXIT INPUT
               END IF 
            END IF
         END IF                   

         -- se selecciono la opcion de preliquidacion
         IF p_c_op_arch_tran = "opt6" THEN 
            --consulta si existe el folio capturado
            SELECT COUNT(*)
              INTO v_i_cta_reg
              FROM dse_agrupa_devolucion
             WHERE folio_liquida = v_d_folio
            
            --si no se encontro el folio se muestra mensaje
            IF v_i_cta_reg = 0 THEN
               -- se valida que exista el folio
               LET v_s_qryTxt = " SELECT COUNT(*)\n",
                                "   FROM glo_folio\n",
                                "  WHERE folio = ",v_d_folio

               PREPARE prp_count_gloFolio FROM v_s_qryTxt
               EXECUTE prp_count_gloFolio INTO v_i_cta_reg

               IF v_i_cta_reg = 0 THEN
                  CALL fn_mensaje("Reverso de Transacciones","Folio a reversar no localizado","stop")   
                  CONTINUE INPUT
               END IF
            ELSE
               --se consulta si existen registros para el estado 130                                
               SELECT COUNT(*)
                 INTO v_i_cta_reg
                 FROM dse_agrupa_devolucion 
                WHERE folio_liquida = v_d_folio
                  AND estado = 130

               --si no encontro registros se muestra mensaje
               IF v_i_cta_reg = 0 THEN
                  CALL fn_mensaje("Reverso de Transacciones","No hay registros preliquidados ó registros ya liquidados","stop")   
                  CONTINUE INPUT 
               END IF
            END IF

            -- se invoca la funcion que genera el PID
            CALL fn_genera_pid(m_i_proceso_cod,m_i_opera_cod,p_v_usuario) RETURNING v_d_pid 

            -- se invoca la funcion que inicializa el proceso
            LET r_b_valida = fn_inicializa_proceso(v_d_pid,
                                                   m_i_proceso_cod,
                                                   m_i_opera_cod,
                                                   v_d_folio,
                                                   m_c_programa_cod,
                                                   v_v_nom_archivo,
                                                   p_v_usuario)

            IF r_b_valida = 0 THEN
               -- se invoca la función que deja la operación en estado Procesando
               LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,
                                                       m_i_proceso_cod,
                                                       m_i_opera_cod,
                                                       v_d_folio,
                                                       m_c_programa_cod,
                                                       v_v_nom_archivo,
                                                       p_v_usuario)

               -- se verifica si fue posible inicializar la operacion
               IF r_b_valida = 0 THEN
                  -- se crea el comando que ejecuta el reverso de la transacción seleccionada
                  LET v_s_comando = " nohup time fglrun ",m_c_ruta_bin CLIPPED,"/GRTR07 ",
                                                          p_v_usuario, " ",
                                                          v_d_pid, " ",
                                                          m_i_proceso_cod, " ",
                                                          m_i_opera_cod, " ",
                                                          v_d_folio, " ",
                                                          p_c_op_arch_tran, " 1> ",
                                                          m_c_ruta_list_bat CLIPPED,
                                                          "/nohup:",v_d_pid USING "&&&&&",":",
                                                          m_i_proceso_cod USING "&&&&&",":",
                                                          m_i_opera_cod USING "&&&&&",
                                                          " 2>&1 &"

                  --DISPLAY v_s_comando
                  RUN v_s_comando

                  CALL fn_mensaje("Aviso","Se ha ejecutado el proceso de reverso de transacciones","information")
               ELSE
                  -- en caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)
                  --EXIT PROGRAM
               END IF
            ELSE
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)
               --EXIT PROGRAM
            END IF

            EXIT INPUT                  
         END IF      
         
         --se inicializan variables de conteo
         LET v_i_cta_reg = 0
         LET v_i_cta_reg_dset = 0

         -- se selecciono la opcion de liquidacion
         IF p_c_op_arch_tran = "opt7" THEN 
            --se consulta si existe el folio a reversar
            SELECT COUNT(*)
              INTO v_i_cta_reg
              FROM cta_movimiento 
             WHERE folio_liquida = v_d_folio

            --si no existe el folio se muestra mensaje
            IF v_i_cta_reg = 0 THEN
               CALL fn_mensaje("Reverso de Transacciones","Folio a reversar no localizado","stop")   
               CONTINUE INPUT 
            ELSE                                
               --se consulta si existen registros con estado 140
               SELECT COUNT(*)
                 INTO v_i_cta_reg_dset
                 FROM dse_agrupa_devolucion
                WHERE folio_liquida = v_d_folio
                  AND estado = 140

               --si no existen registros se muestra mensaje
               IF v_i_cta_reg_dset = 0 THEN
                  CALL fn_mensaje("Reverso de Transacciones","No hay registros liquidados","stop")   
                  CONTINUE INPUT 
               ELSE
                  --se consulta el estado en cnt transaccion
                  SELECT COUNT(*)
                    INTO v_i_num_edo_transac
                    FROM cnt_transaccion
                   WHERE folio_liquida = v_d_folio
                     AND estado = 10
                  
                  --si el estado es diferente a 10 no se realiza el reverso
                  --IF v_i_num_edo_transac > 0 THEN
                  --   CALL fn_mensaje("Reverso de Transacciones","Folio ya contabilizado","stop")                        
                  --   CONTINUE INPUT
                  --ELSE                                        
                     -- se invoca la funcion que genera el PID
                     CALL fn_genera_pid(m_i_proceso_cod,m_i_opera_cod,p_v_usuario) RETURNING v_d_pid 
                     
                     -- se invoca la funcion que inicializa el proceso
                     LET r_b_valida = fn_inicializa_proceso(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                            v_d_folio, m_c_programa_cod,
                                                            v_v_nom_archivo, p_v_usuario)
                     
                     IF r_b_valida = 0 THEN
                        -- se invoca la función que deja la operación en estado Procesando
                        LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                                v_d_folio, m_c_programa_cod,
                                                                v_v_nom_archivo, p_v_usuario)
                     
                        -- se verifica si fue posible inicializar la operacion
                        IF r_b_valida = 0 THEN
                           -- se crea el comando que ejecuta el reverso de la transacción seleccionada
                           LET v_s_comando = " nohup time fglrun ",m_c_ruta_bin CLIPPED,"/GRTR07 ",
                                                                   p_v_usuario, " ",
                                                                   v_d_pid, " ",
                                                                   m_i_proceso_cod, " ",
                                                                   m_i_opera_cod, " ",
                                                                   v_d_folio, " ",
                                                                   p_c_op_arch_tran, " 1> ",
                                                                   m_c_ruta_list_bat CLIPPED,
                                                                   "/nohup:",v_d_pid USING "&&&&&",":",
                                                                   m_i_proceso_cod USING "&&&&&",":",
                                                                   m_i_opera_cod USING "&&&&&",
                                                                   " 2>&1 &"
                     
                           --DISPLAY v_s_comando
                           RUN v_s_comando
                           
                           CALL fn_mensaje("Aviso","Se ha ejecutado el proceso de reverso de transacciones","information")
                        ELSE
                           -- en caso de error se muestra un mensaje a usuario y no continua
                           CALL fn_muestra_inc_operacion(r_b_valida)
                           --EXIT PROGRAM
                        END IF
                     ELSE
                        -- en caso de error se muestra un mensaje a usuario y no continua
                        CALL fn_muestra_inc_operacion(r_b_valida)
                        --EXIT PROGRAM
                     END IF
                  --END IF
                  EXIT INPUT                                                      
               END IF 
            END IF
         END IF            
         
      ON ACTION CANCEL 
         EXIT INPUT   
   
   END INPUT 
   CLOSE WINDOW w_arch_transaccion
           
END FUNCTION 

#Objetivo: Reverso de Archivos Salida, permite la seleccion del archivo a reversar
FUNCTION fn_reversa_arch_salida()
   DEFINE v_nom_grupo       VARCHAR(30), -- texto en el grupo que mostrara en la forma
          v_v_nom_archivo   LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_i_lote          LIKE dse_ctr_archivo.lote, -- lote del archivo
          v_fecha_gen       LIKE dse_ctr_archivo.f_lote, -- feche de generacion
          v_d_pid           LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_d_folio         LIKE bat_ctr_operacion.folio, -- folio
          v_ar_tbl_archivos DYNAMIC ARRAY OF RECORD
             nom_archivo    LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo             
             lote           LIKE dse_ctr_archivo.lote, -- lote
             f_lote         LIKE dse_ctr_archivo.f_lote, -- fecha de lote             
             tot_registros  LIKE dse_ctr_archivo.tot_registros, -- numero total de registros
             folio            LIKE dse_ctr_archivo.folio -- folio
          END RECORD,
          v_ui_ventana      UI.WINDOW, -- manejador de la ventana
          v_ui_forma        UI.FORM, -- menejador de la forma
          v_b_continua      SMALLINT, -- booleana que indica si el proceso continua
          v_i_indice_arr    SMALLINT, -- indice del arreglo
          v_i_indice_slct   SMALLINT, -- indice seleccionado
          v_ban_salir       SMALLINT, -- booleana que indica si el usuario canceló la operación
          v_s_condicion     STRING, -- contiene la condición de la sentencia sql del construct
          v_s_mensaje       STRING, -- contiene un mensaje a mostrar a usuario
          v_s_qryTxt        STRING, -- se asigna una sentencia sql a ejecutar
          v_s_comando       STRING, -- contiene al comando a correr
          r_v_extension     LIKE cat_operacion.extension, -- extensión del archivo
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se inicializan variables
   LET v_nom_grupo = "Solicitud de saldos"
   LET v_d_pid = 0
   LET v_d_folio = 0

   OPEN WINDOW w_arch_salida WITH FORM "GRTL423"
      -- se deplega el nombre del proceso a reversar en el grupo
      LET v_ui_ventana = ui.Window.getCurrent()
      LET v_ui_forma = v_ui_ventana.getForm()
      CALL v_ui_forma.setElementText("grevent",v_nom_grupo)

      INPUT v_v_nom_archivo,v_i_lote,v_fecha_gen  
       FROM  nom_archivo, lote, fecha
      ATTRIBUTES(UNBUFFERED)

      ON ACTION ACCEPT
         -- se validan las variables ingresadas
         IF v_v_nom_archivo IS NULL AND v_i_lote IS NULL AND v_fecha_gen IS NULL THEN
            CALL fn_mensaje("Reverso Archivo Salida","Debe ingresar el nombre del archivo y/o lote y fecha para continuar","stop")
            CONTINUE INPUT
         END IF

         -- se valida el lote y la fecha
         IF (v_i_lote IS NULL AND v_fecha_gen IS NOT NULL) THEN
            CALL fn_mensaje("Reverso Archivo Salida","Se debe ingresar el lote del archivo a reversar","stop")
            CONTINUE INPUT
         END IF
         IF (v_i_lote IS NOT NULL AND v_fecha_gen IS NULL) THEN
            CALL fn_mensaje("Reverso Archivo Salida","Se debe ingresar la fecha de generación del archivo a reversar","stop")
            CONTINUE INPUT
         END IF

         -- se validan el nombre del archivo
         IF v_v_nom_archivo IS NOT NULL THEN
            LET v_b_continua = fn_valida_ext_archivo("opt4", v_v_nom_archivo)

            IF NOT v_b_continua THEN
               CONTINUE INPUT
            END IF
         END IF

         -- se invoca la función que obtiene la extensión el archivo
         CALL fn_obtiene_ext_archivo("opt4") RETURNING r_v_extension

         -- se valida la extensión obtenida
         IF r_v_extension IS NULL THEN
            CALL fn_mensaje("Reverso Archivo Salida","No fue posible obtener la extensión del archivo","stop")
            CONTINUE INPUT 
         END IF

         -- se construye la condicion
         LET v_s_condicion = "1=1\n"
         
         IF ( v_v_nom_archivo IS NOT NULL ) THEN
            LET v_s_condicion = v_s_condicion, "AND nom_archivo ='",v_v_nom_archivo,"'\n"
         ELSE
            LET v_s_condicion = v_s_condicion, "AND nom_archivo LIKE '%",r_v_extension CLIPPED,"'\n"
         END IF

         IF ( v_i_lote IS NOT NULL ) THEN
            LET v_s_condicion = v_s_condicion, "AND lote =", v_i_lote, "\n"
         END IF

         IF ( v_fecha_gen IS NOT NULL ) THEN
            LET v_s_condicion = v_s_condicion, "AND f_lote ='", v_fecha_gen, "'\n"
         END IF         
         EXIT INPUT

      ON ACTION CANCEL
         LET v_ban_salir = TRUE  
         EXIT INPUT
   END INPUT     

   IF NOT v_ban_salir THEN 
      -- se inicializan las variables
      LET v_i_indice_arr = 1

      -- se realizará la busqueda por el nombre del archivo lote y fecha
      LET v_s_qryTxt = " SELECT nom_archivo, lote, f_lote, tot_registros, folio\n",
                       "   FROM dse_ctr_archivo\n",
                       "  WHERE estado = 20\n",
                       "    AND ",v_s_condicion

      PREPARE prp_busca_infoArch2 FROM v_s_qryTxt
      DECLARE cur_busca_infoArch2 CURSOR FOR prp_busca_infoArch2

      FOREACH cur_busca_infoArch2 INTO v_ar_tbl_archivos[v_i_indice_arr].*
         LET v_i_indice_arr = v_i_indice_arr + 1
      END FOREACH

      -- verifica si se encotraron archivos con el nombre dado
      IF v_i_indice_arr = 1 THEN
         CALL fn_mensaje("Reverso Archivo Salida","No se encontraron archivos cargados con el nombre dado","stop")
      ELSE
         DISPLAY ARRAY v_ar_tbl_archivos TO tabla_reverso.*
            ON ACTION ACCEPT
               -- se obtiene el indice seleccionado por el usuario
               LET v_i_indice_slct = arr_curr()

               -- verifica si el usuario selecciono un registro
               IF v_i_indice_slct IS NULL THEN
                  CALL fn_mensaje("Reverso Archivo Salida","Debe seleccionar un archivo a reversar para continuar","stop")
                  CONTINUE DISPLAY
               END IF

               -- se invoca la función que valida el reverso del archivo de salida
               CALL fn_valida_sol_devol() RETURNING v_b_continua
               DISPLAY ">>Continua: ",v_b_continua

               -- si la validacion no fue aceptada no continua con el reverso
               IF NOT v_b_continua THEN
                  CONTINUE DISPLAY
               END IF

               -- se invoca la funcion que genera el PID
               CALL fn_genera_pid(m_i_proceso_cod,m_i_opera_cod,p_v_usuario) RETURNING v_d_pid 

               -- se invoca la funcion que inicializa el proceso
               LET r_b_valida = fn_inicializa_proceso(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                      v_d_folio, m_c_programa_cod,
                                                      v_v_nom_archivo, p_v_usuario)

               IF r_b_valida = 0 THEN
                  -- se invoca la función que deja la operación en estado Procesando
                  LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                          v_d_folio, m_c_programa_cod,
                                                          v_v_nom_archivo, p_v_usuario)

                  -- se verifica si fue posible inicializar la operacion
                  IF r_b_valida = 0 THEN
                     -- se crea el comando que ejecuta el reverso del archivo de salida seleccionado
                     LET v_s_comando = " nohup time fglrun ",m_c_ruta_bin CLIPPED,"/GRTR08 ",
                                                             p_v_usuario, " ",
                                                             v_d_pid, " ",
                                                             m_i_proceso_cod, " ",
                                                             m_i_opera_cod, " ",
                                                             --v_d_folio, " ",
                                                             v_ar_tbl_archivos[v_i_indice_slct].folio, " ",
                                                             v_ar_tbl_archivos[v_i_indice_slct].nom_archivo, " ", " 1> ",
                                                             m_c_ruta_list_bat CLIPPED,
                                                             "/nohup:",v_d_pid USING "&&&&&",":",
                                                             m_i_proceso_cod USING "&&&&&",":",
                                                             m_i_opera_cod USING "&&&&&",
                                                             " 2>&1 &"

                     --DISPLAY v_s_comando
                     RUN v_s_comando

                     -- se asigna el mensaje a mostrar al usuario
                     LET v_s_mensaje = "Se ha enviado el Reverso de Archivo de Salida ",v_nom_grupo CLIPPED, " con PID: ",v_d_pid CLIPPED,
                                       ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
                     CALL fn_mensaje("Aviso",v_s_mensaje,"information")
                  ELSE
                     -- en caso de error se muestra un mensaje a usuario y no continua
                     CALL fn_muestra_inc_operacion(r_b_valida)
                     --EXIT PROGRAM
                  END IF
               ELSE
                  -- en caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)
                  --EXIT PROGRAM
               END IF

               EXIT DISPLAY

            ON ACTION CANCEL
               EXIT DISPLAY
         END DISPLAY
      END IF
   END IF
   CLOSE WINDOW w_arch_salida
END FUNCTION   

# Objetivo: Se valida el proceso de reverso de Devolución de Saldos
FUNCTION fn_valida_dev_sdos(p_d_folio_arch)
   DEFINE p_d_folio_arch  LIKE glo_folio.folio, -- folio del archivo seleccionado
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se existen registros en dse devolucion
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_devolucion\n",
                    "  WHERE tpo_transferencia = '",m_c_tpo_transf,"'\n",
                    "    AND folio_referencia = ",p_d_folio_arch

   PREPARE prp_val_dev_sdos FROM v_s_qryTxt
   EXECUTE prp_val_dev_sdos INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      -- se asume que unicamente se ha validado el archivo
      LET v_b_val_proceso = 1

      RETURN v_b_val_proceso
   END IF

   -- se crea la sentencia sql que verifica si los registros ya fueron agrupados
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_devolucion\n",
                    "  WHERE folio_referencia = ",p_d_folio_arch,"\n",
                    "    AND tpo_transferencia = '",m_c_tpo_transf,"'",
                    "    AND estado = 15"

   PREPARE prp_busca_agrupados FROM v_s_qryTxt
   EXECUTE prp_busca_agrupados INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs > 0 THEN
      CALL fn_mensaje("Reverso","Los registros ya fueron agrupados. No es posible realizar la preliquidación","stop")

      -- se indica que no es posible realizar el reverso
      LET v_b_val_proceso = 0

      RETURN v_b_val_proceso
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Rechazo de Devolución de Saldos
FUNCTION fn_valida_rech_dev(p_d_folio_arch)
   DEFINE p_d_folio_arch  LIKE glo_folio.folio, -- folio del archivo seleccionado
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_his_devolucion\n",
                    "  WHERE folio = ",p_d_folio_arch

   PREPARE prp_val_rech_dev1 FROM v_s_qryTxt
   EXECUTE prp_val_rech_dev1 INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      -- se asuame que unicamente ha sido procesado la validacion
      LET v_b_val_proceso = 1

      RETURN v_b_val_proceso
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_agrupa_devolucion\n",
                    "  WHERE id_dse_grp_devolucion IN (\n",
                    "        SELECT id_dse_grp_devolucion\n",
                    "          FROM dse_his_devolucion\n",
                    "         WHERE folio = ",p_d_folio_arch,")\n",
                    "    AND tpo_transferencia = ",m_c_tpo_transf,"\n",
                    "    AND estado >= 140\n",
                    "    AND edo_procesar IN (90, 70)"

   PREPARE prp_val_rech_dev2 FROM v_s_qryTxt
   EXECUTE prp_val_rech_dev2 INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","No se puede reversar el proceso","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Confirmación de Devolución de Saldos
FUNCTION fn_valida_conf_dev(p_d_folio_arch)
   DEFINE p_d_folio_arch  LIKE glo_folio.folio, -- folio del archivo seleccionado
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_his_devolucion\n",
                    "  WHERE folio = ",p_d_folio_arch

   PREPARE prp_val_conf_dev1 FROM v_s_qryTxt
   EXECUTE prp_val_conf_dev1 INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      -- se asuame que unicamente ha sido procesado la validacion
      LET v_b_val_proceso = 1

      RETURN v_b_val_proceso
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_agrupa_devolucion\n",
                    "  WHERE id_dse_grp_devolucion IN (\n",
                    "        SELECT id_dse_grp_devolucion\n",
                    "          FROM dse_his_devolucion\n",
                    "         WHERE folio = ",p_d_folio_arch,")\n",
                    "    AND tpo_transferencia = ",m_c_tpo_transf,"\n",
                    "    AND estado >= 140\n",
                    "    AND edo_procesar IN (120)"

   PREPARE prp_val_conf_dev2 FROM v_s_qryTxt
   EXECUTE prp_val_conf_dev2 INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","No se puede reversar el proceso","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Solicitud de Devolución
FUNCTION fn_valida_sol_devol()
   DEFINE v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_agrupa_devolucion\n",
                    "  WHERE tpo_transferencia = ",m_c_tpo_transf,"\n",
                    "    AND estado = 140\n",
                    "    AND edo_procesar IN (80, 85)"

   PREPARE prp_val_solSdos FROM v_s_qryTxt
   EXECUTE prp_val_solSdos INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","No hay registros a reversar o registros ya enviados a PROCESAR","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

#Objetivo: Valida la extención del archivo dado como parametro
FUNCTION fn_obtiene_ext_archivo(p_c_op_arch_ent)
   DEFINE p_c_op_arch_ent   VARCHAR(5), -- contiene la opcion a reversar de Archivos entrada
          v_i_proceso_cod   LIKE cat_operacion.proceso_cod, -- codigo del proceso
          v_i_opera_cod     LIKE cat_operacion.opera_cod, -- codigo de la operación
          v_v_extension     LIKE cat_operacion.extension, -- extensión del archivo
          v_s_qryTxt        STRING -- se asigna una sentencia sql a ejecutar

   -- se verifica la opción seleccionada para poner identificar la extension correspondiente
   CASE p_c_op_arch_ent
      WHEN "opt1" -- Devolución de Saldos Excedentes
         LET v_i_proceso_cod = g_proc_cod_grt_dse -- devolución de saldos excedentes grt
         LET v_i_opera_cod = 1 -- valida devolución saldos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt2" -- Recepción Rechazo Devolución Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_rech_dse -- recepción rechazo devolución saldos exc grt
         LET v_i_opera_cod = 1 -- Valida Rechazos Devolución

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt3" -- Recepción Confirmación Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_conf_dse -- recepción confirmación saldos exc grt
         LET v_i_opera_cod = 1 -- valida confirmación devolución

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt4" -- Salida Solicitud de Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_arch_solic_dse -- generación solicitud devolución sdo exc grt
         LET v_i_opera_cod = 1 -- solicitud devolución saldo excedente grt

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod
   END CASE

   PREPARE prp_obtiene_ext FROM v_s_qryTxt
   EXECUTE prp_obtiene_ext INTO v_v_extension

   RETURN v_v_extension
END FUNCTION
