--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTL30                                        #
#Objetivo          =>Programa que pide al usuario la seleccion del #
#                    proceso a reversar: Archivos Entrada,         #
#                    Archivos Salida, Transacciones para el módulo #
#                    de Uso de Garantía 43 bis                     #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>22 Mayo 2012                                  #
####################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

DEFINE p_v_usuario           LIKE seg_usuario.usuario, -- usuario firmado al sistema
       m_c_programa_cod      LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
       m_i_proceso_cod       LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
       m_i_opera_cod         LIKE cat_operacion.opera_cod, -- operación que llama la funcion
       m_c_ruta_bin          LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
       m_c_ruta_listados     LIKE seg_modulo.ruta_listados, -- ruta listados del módulo
       m_c_ruta_list_bat     LIKE seg_modulo.ruta_listados -- ruta listados de bat

MAIN
   DEFINE p_b_tipo_carga      SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_nom_prog        VARCHAR(30), -- nombre del programa
          v_c_op_arch_ent     VARCHAR(5), -- selección del modulo a reversar (Archivos entrada)
          v_c_op_arch_sal     VARCHAR(5), -- selección del modulo a reversar (Archivos salida)
          v_ci_op_arch_tran   VARCHAR(5), -- selección del modulo a reversar (Transacciones)
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_d_folio           LIKE bat_ctr_operacion.folio, -- folio
          v_s_qryTxt          STRING, -- se asigna una sentencia sql a ejecutar
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- recupera los parametros que vienen del principal
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".GRTL30.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inializan variables
   LET m_i_proceso_cod = g_proc_cod_grt_uso_reverso -- reverso uso de garantía 43 bis
   LET m_i_opera_cod = 1 -- reverso uso de garantía 43 bis
   LET m_c_programa_cod = "GRTL30"
   LET v_d_pid = 0
   LET v_d_folio = 0
   LET v_c_op_arch_ent = NULL
   LET v_c_op_arch_sal = NULL
   LET v_ci_op_arch_tran = NULL

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,m_i_proceso_cod,m_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING m_c_ruta_bin, m_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO m_c_ruta_list_bat

   OPEN WINDOW w_reverso WITH FORM "GRTL301"
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
   DEFINE p_c_op_arch_ent       VARCHAR(5), -- contiene la opcion a reversar de Archivos entrada
          v_nom_grupo           VARCHAR(30), -- texto en el grupo que mostrara en la forma
          v_v_nom_archivo       LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_i_lote              INTEGER, -- lote del archivo
          v_fecha_lote          LIKE cre_ctr_archivo.f_lote, -- feche de lote
          v_i_proceso_cod_rev   LIKE cat_proceso.proceso_cod, -- proceso a reversar
          v_i_opera_cod_rev     LIKE cat_operacion.opera_cod, -- operación a reversar
          v_d_pid_rev           LIKE bat_ctr_operacion.pid, -- pid a reversar
          v_d_pid               LIKE bat_ctr_operacion.pid, -- identificador del proceso
          v_d_folio             LIKE bat_ctr_operacion.folio, -- folio
          v_d_folio_arch        LIKE bat_ctr_operacion.folio, -- folio del archivo elegido
          v_ar_tbl_archivos     DYNAMIC ARRAY OF RECORD
             nom_archivo        LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
             f_lote             LIKE cre_ctr_archivo.f_lote, -- fecha de lote
             lote               LIKE cre_ctr_archivo.lote, -- lote
             f_proceso          LIKE cre_ctr_archivo.f_proceso, -- fecha de proceso
             tot_registros      LIKE cre_ctr_archivo.tot_registros, -- numero total de registros
             id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- id control de archivo
             folio_archivo      LIKE cre_ctr_archivo.folio_archivo
          END RECORD,
          v_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador de la tabla de control
          v_ui_ventana          UI.WINDOW, -- manejador de la ventana
          v_ui_forma            UI.FORM, -- menejador de la forma
          v_b_continua          SMALLINT, -- booleana que indica si el proceso continua
          v_i_indice_arr        SMALLINT, -- indice del arreglo
          v_i_indice_slct       SMALLINT, -- indice seleccionado
          v_ban_salir           SMALLINT, -- booleana que indica si el usuario canceló la operación
          v_s_condicion         STRING, -- contiene la condición de la sentencia sql del construct
          v_s_mensaje           STRING, -- contiene un mensaje a mostrar a usuario
          v_s_qryTxt            STRING, -- se asigna una sentencia sql a ejecutar
          v_s_comando           STRING, -- contiene al comando a correr
          v_i_cuenta_reg        INTEGER, -- tiene el contador de registros
          r_v_extension         LIKE cat_operacion.extension, -- extensión del archivo
          r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se inicializan variables
   LET v_d_folio = 0
   LET v_d_folio_arch = 0
   LET v_d_pid = 0

   -- se verifica la opción seleccionada para poner nombre en el grupo
   CASE p_c_op_arch_ent
      WHEN "opt1"
         LET v_nom_grupo = "Uso Garantia"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_garantia -- recepción uso de garantía 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt2"
         LET v_nom_grupo = "Rechazo de Saldos"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_rech_saldos -- recepción rechazo de saldos uso 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt3"
         LET v_nom_grupo = "Saldos Transferidos"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_sdos_transf -- recepción saldos transferidos uso 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt4"
         LET v_nom_grupo = "Solicitudes Devueltas"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_devol_solic -- recepción devol solicitudes uso 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt5"
         LET v_nom_grupo = "Solicitudes no Atendidas"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_no_atendidas	-- recepción solic no atendidas uso 43bis
         LET v_i_opera_cod_rev = 1
   END CASE

   -- se obtiene el maximo PID para el proceso y operacion cod
   LET v_d_pid_rev = fn_max_pid(v_i_proceso_cod_rev, v_i_opera_cod_rev)

   OPEN WINDOW w_arch_entrada WITH FORM "GRTL302"
      -- se deplega el nombre del proceso a reversar en el grupo
      LET v_ui_ventana = ui.Window.getCurrent()
      LET v_ui_forma = v_ui_ventana.getForm()
      CALL v_ui_forma.setElementText("grevent",v_nom_grupo)
   
      INPUT v_v_nom_archivo,v_i_lote,v_fecha_lote,v_d_folio_arch
       FROM nom_archivo, lote, f_lote, folio_archivo
      ATTRIBUTES ( UNBUFFERED )
      
      ON ACTION ACCEPT
         -- se validan las variables ingresadas
         IF v_v_nom_archivo IS NULL AND v_i_lote IS NULL AND
            v_fecha_lote IS NULL AND v_d_folio_arch IS NULL THEN
            CALL fn_mensaje("Reverso Archivo Entrada","Debe ingresar algun campo para continuar","stop")
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

         IF ( v_fecha_lote IS NOT NULL ) THEN
            LET v_s_condicion = v_s_condicion, "AND f_lote ='", v_fecha_lote, "'\n"
         END IF

         IF ( v_d_folio_arch IS NOT NULL ) THEN
            LET v_s_condicion = v_s_condicion, "AND folio_archivo ='", v_d_folio_arch, "'\n"
         END IF       
         EXIT INPUT

      ON ACTION CANCEL
         LET v_ban_salir = TRUE
         EXIT INPUT
   END INPUT

   IF v_ban_salir THEN
      RETURN
   END IF

   -- se inicializan las variables
   LET v_i_indice_arr = 1

   -- se realizará la busqueda por el nombre del archivo lote y fecha
   LET v_s_qryTxt = " SELECT nom_archivo, f_lote, lote, f_proceso, tot_registros, id_cre_ctr_archivo, folio_archivo\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE ",v_s_condicion

   PREPARE prp_busca_infoArch FROM v_s_qryTxt
   DECLARE cur_busca_infoArch CURSOR FOR prp_busca_infoArch

   FOREACH cur_busca_infoArch INTO v_ar_tbl_archivos[v_i_indice_arr].*
      LET v_i_indice_arr = v_i_indice_arr + 1
   END FOREACH

   -- verifica si se encotraron archivos con el nombre dado
   IF v_i_indice_arr = 1 THEN
      IF v_v_nom_archivo IS NULL THEN
         -- se asigna el mensaje que se le mostrará a usuario
         LET v_s_mensaje = "No se encontraron archivos cargados con la información\n",
                           "proporcionada. Sí ocurrió un error durante el proceso de\n",
                           "carga debe reversar por nombre del archivo"
         CALL fn_mensaje("Reverso Archivo Entrada",v_s_mensaje,"stop")

         RETURN
      ELSE
         -- busca la información en glo, con estado no integrado
         LET v_s_qryTxt = " SELECT COUNT(*)\n",
                          "   FROM glo_ctr_archivo\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod_rev,"\n",
                          "    AND opera_cod = ",v_i_opera_cod_rev,"\n",
                          "    AND nombre_archivo = '",v_v_nom_archivo CLIPPED,"'\n",
                          "    AND estado = 1"

         PREPARE prp_glo_ctr_arch FROM v_s_qryTxt
         EXECUTE prp_glo_ctr_arch INTO v_i_cuenta_reg

         IF v_i_cuenta_reg = 0 THEN
            -- se asigna el mensaje que se le mostrará a usuario
            LET v_s_mensaje = "No se encontraron archivos cargados con la información proporcionada"
            CALL fn_mensaje("Reverso Archivo Entrada",v_s_mensaje,"stop")

            RETURN
         ELSE
            -- se asigna el nombre del archivo en el arreglo
            LET v_ar_tbl_archivos[v_i_indice_arr].nom_archivo = v_v_nom_archivo
            LET v_ar_tbl_archivos[v_i_indice_arr].id_cre_ctr_archivo = 0
         END IF
      END IF
   END IF

   DISPLAY ARRAY v_ar_tbl_archivos TO tabla_reverso.*
      ON ACTION ACCEPT
         -- se obtiene el indice seleccionado por el usuario
         LET v_i_indice_slct = arr_curr()

         -- verifica si el usuario selecciono un registro
         IF v_i_indice_slct IS NULL THEN
            CALL fn_mensaje("Reverso Archivo Entrada","Debe seleccionar un archivo a reversar para continuar","stop")
            CONTINUE DISPLAY
         END IF

         -- se asigna el nombre y el folio del archivo seleccionado
         LET v_v_nom_archivo      = v_ar_tbl_archivos[v_i_indice_slct].nom_archivo
         LET v_i_lote             = v_ar_tbl_archivos[v_i_indice_slct].lote
         LET v_fecha_lote         = v_ar_tbl_archivos[v_i_indice_slct].f_lote
         LET v_id_cre_ctr_archivo = v_ar_tbl_archivos[v_i_indice_slct].id_cre_ctr_archivo
         LET v_d_folio_arch       = v_ar_tbl_archivos[v_i_indice_slct].folio_archivo

         -- se valida la extensión del archivo
         LET v_b_continua = fn_valida_ext_archivo(p_c_op_arch_ent, v_v_nom_archivo)

         IF NOT v_b_continua THEN
            CONTINUE DISPLAY
         END IF

         -- se verifica la opción seleccionada para validar el reverso
         CASE p_c_op_arch_ent
            WHEN "opt1" -- Uso garantía
               CALL fn_valida_uso_garantia(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt2" -- Rechazo de Saldos
               CALL fn_valida_rechazos(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt3" -- Saldos Transferidos
               CALL fn_valida_saldos(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt4" -- Solicitudes Devueltas
               CALL fn_valida_devoluciones(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt5" -- Solicitudes no Atendidas
               CALL fn_valida_noatendidas(v_id_cre_ctr_archivo) RETURNING v_b_continua
         END CASE

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
               -- se crea el comando que ejecuta el reverso de archivo de entrada
               LET v_s_comando = " nohup time fglrun ",m_c_ruta_bin CLIPPED,"/GRTR03 ",
                                                       p_v_usuario, " ",
                                                       v_d_pid, " ",
                                                       m_i_proceso_cod, " ",
                                                       m_i_opera_cod, " ",
                                                       v_d_folio_arch, " ",
                                                       v_v_nom_archivo, " ",
                                                       v_d_pid_rev, " ",
                                                       p_c_op_arch_ent, " ",
                                                       v_id_cre_ctr_archivo, " 1> ",
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
      WHEN "opt1" -- uso de garantía
         LET v_i_proceso_cod = g_proc_cod_grt_uso_garantia -- recepción uso de garantía 43bis
         LET v_i_opera_cod = 1 -- valida archivo solicitud uso 43bis

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt2" -- Rechazo de Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_uso_rech_saldos -- recepciÓn rechazo de saldos uso 43bis
         LET v_i_opera_cod = 1 -- valida rechazo saldos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt3" -- Saldos Transferidos
         LET v_i_proceso_cod = g_proc_cod_grt_uso_sdos_transf -- recepción saldos transferidos uso 43bis
         LET v_i_opera_cod = 1 -- valida saldos transferidos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt4" -- Solicitudes Devueltas
         LET v_i_proceso_cod = g_proc_cod_grt_uso_devol_solic -- recepción devol solicitudes uso 43bis
         LET v_i_opera_cod = 1 -- valida devolución solicitudes

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt5" -- Solicitudes no Atendidas
         LET v_i_proceso_cod = g_proc_cod_grt_uso_no_atendidas -- recepción solic no atendidas uso 43bis
         LET v_i_opera_cod = 1 -- valida solicitudes no atendidas

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt7" -- Solicitud de Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_uso_arch_solic -- generación archivo solic sdos uso 43bis
         LET v_i_opera_cod = 1 -- solicitud de saldos

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
      CALL fn_mensaje("Reverso","El archivo ingresado no es del tipo de archivo elegido para reversar","stop")
      LET v_i_arch_valido = FALSE
   END IF

   RETURN v_i_arch_valido
END FUNCTION

#Objetivo: Reverso de Transacciones, permite la captura de la transaccion a reversar
FUNCTION fn_reversa_transacciones(p_c_op_arch_tran)
   DEFINE p_c_op_arch_tran      VARCHAR(5), -- contiene la opcion a reversar transacciones
          v_d_folio             INTEGER, -- numero de folio
          v_d_pid               LIKE bat_ctr_operacion.pid, -- identificador del proceso
          v_v_nom_archivo       LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_s_comando           STRING, -- contiene al comando a correr
          v_s_mensaje           STRING, -- mensaje a mostrar a usuario
          v_b_continua          SMALLINT, -- booleana que indica si el proceso continua
          v_i_cta_reg           SMALLINT, --contador de registros
          r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se inicializan variables
   LET v_d_pid = 0
   LET v_d_folio = NULL
   LET v_v_nom_archivo = NULL

   OPEN WINDOW w_arch_transaccion WITH FORM "GRTL304"
   INPUT v_d_folio WITHOUT DEFAULTS
   FROM folio ATTRIBUTES(UNBUFFERED)
      AFTER FIELD folio
         NEXT FIELD folio

      ON ACTION ACCEPT
         IF v_d_folio IS NULL THEN
            CALL fn_mensaje("Reverso de Transacciones","Debe ingresar el folio para continuar","stop")
            CONTINUE INPUT
         END IF

         -- se inicializa el contador de registros
         LET v_i_cta_reg = 0

         -- se verifica la opción seleccionada para validar el reverso
         CASE p_c_op_arch_tran
            WHEN "opt8" -- Preliquidación
               CALL fn_valida_preliquidacion(v_d_folio) RETURNING v_b_continua
            WHEN "opt9" -- Liquidación
               CALL fn_valida_liquidacion(v_d_folio) RETURNING v_b_continua
            OTHERWISE
               CALL fn_mensaje("Reverso","Opción no reconocida","stop")
               CONTINUE INPUT 
         END CASE

         -- si la validacion no fue aceptada no continua con el reverso
         IF NOT v_b_continua THEN
            CONTINUE INPUT
         END IF

         -- se invoca la funcion que genera el PID
         CALL fn_genera_pid(m_i_proceso_cod, m_i_opera_cod, p_v_usuario) RETURNING v_d_pid

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
               -- se asigna el nombre del archivo
               LET v_v_nom_archivo = "N/A"

               -- se crea el comando que ejecuta el proceso de reverso de transacciones
               LET v_s_comando = " nohup time fglrun ",m_c_ruta_bin CLIPPED,"/GRTR04 ",
                                                       p_v_usuario, " ",
                                                       v_d_pid, " ",
                                                       m_i_proceso_cod, " ",
                                                       m_i_opera_cod, " ",
                                                       v_d_folio, " ",
                                                       v_v_nom_archivo, " ",
                                                       p_c_op_arch_tran, " ", " 1> ",
                                                       m_c_ruta_list_bat CLIPPED,
                                                       "/nohup:",v_d_pid USING "&&&&&",":",
                                                       m_i_proceso_cod USING "&&&&&",":",
                                                       m_i_opera_cod USING "&&&&&",
                                                       " 2>&1 &"

               --DISPLAY v_s_comando
               RUN v_s_comando

               -- se asigna el mensaje a mostrar al usuario
               LET v_s_mensaje = "Se ha enviado el reverso con PID: ",v_d_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
               CALL fn_mensaje("Reverso",v_s_mensaje,"information")
            ELSE
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)
               CONTINUE INPUT
            END IF
         ELSE
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)
            CONTINUE INPUT
         END IF

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT

   END INPUT
   CLOSE WINDOW w_arch_transaccion

END FUNCTION

#Objetivo: Reverso de Archivos Salida, permite la seleccion del archivo a reversar
FUNCTION fn_reversa_arch_salida()
   DEFINE v_nom_grupo           VARCHAR(30), -- texto en el grupo que mostrara en la forma
          v_v_nom_archivo       LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_i_lote              LIKE cre_ctr_archivo.lote, -- lote del archivo
          v_fecha_gen           LIKE cre_ctr_archivo.f_lote, -- feche de generacion
          v_d_pid               LIKE bat_ctr_operacion.pid, -- identificador del proceso
          v_d_folio             INTEGER, -- folio
          v_ar_tbl_archivos     DYNAMIC ARRAY OF RECORD
             nom_archivo        LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
             lote               LIKE cre_ctr_archivo.lote, -- lote
             f_lote             LIKE cre_ctr_archivo.f_lote, -- fecha de lote
             tot_registros      LIKE cre_ctr_archivo.tot_registros, -- numero total de registros
             id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo
          END RECORD,
          v_ui_ventana          UI.WINDOW, -- manejador de la ventana
          v_ui_forma            UI.FORM, -- menejador de la forma
          v_b_continua          SMALLINT, -- booleana que indica si el proceso continua
          v_i_indice_arr        SMALLINT, -- indice del arreglo
          v_i_indice_slct       SMALLINT, -- indice seleccionado
          v_ban_salir           SMALLINT, -- booleana que indica si el usuario canceló la operación
          v_s_condicion         STRING, -- contiene la condición de la sentencia sql del construct
          v_s_mensaje           STRING, -- contiene un mensaje a mostrar a usuario
          v_s_qryTxt            STRING, -- se asigna una sentencia sql a ejecutar
          v_s_comando           STRING, -- contiene al comando a correr
          r_v_extension         LIKE cat_operacion.extension, -- extensión del archivo
          r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se inicializan variables
   LET v_nom_grupo = "Solicitud de saldos"
   LET v_d_pid = 0
   LET v_d_folio = 0

   OPEN WINDOW w_arch_salida WITH FORM "GRTL303"
      -- se deplega el nombre del proceso a reversar en el grupo
      LET v_ui_ventana = ui.Window.getCurrent()
      LET v_ui_forma = v_ui_ventana.getForm()
      CALL v_ui_forma.setElementText("grevent",v_nom_grupo)

      INPUT v_v_nom_archivo,v_i_lote,v_fecha_gen  
       FROM  nom_archivo, lote, f_lote
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
            LET v_b_continua = fn_valida_ext_archivo("opt7", v_v_nom_archivo)

            IF NOT v_b_continua THEN
               CONTINUE INPUT
            END IF
         END IF
         
         -- se invoca la función que obtiene la extensión el archivo
         CALL fn_obtiene_ext_archivo("opt7") RETURNING r_v_extension

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
      LET v_s_qryTxt = " SELECT nom_archivo, lote, f_lote, tot_registros,id_cre_ctr_archivo\n",
                       "   FROM cre_ctr_archivo\n",
                       "  WHERE ",v_s_condicion

      PREPARE prp_busca_infoArch2 FROM v_s_qryTxt
      DECLARE cur_busca_infoArch2 CURSOR FOR prp_busca_infoArch2

      FOREACH cur_busca_infoArch2 INTO v_ar_tbl_archivos[v_i_indice_arr].*
         LET v_i_indice_arr = v_i_indice_arr + 1
      END FOREACH

      -- verifica si se encotraron archivos con el nombre dado
      IF v_i_indice_arr = 1 THEN
         CALL fn_mensaje("Reverso Archivo Salida","No se encontraron archivos generados con la información proporcionada","stop")
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
               CALL fn_valida_sol_sdos() RETURNING v_b_continua
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
                     -- se crea el comando que ejecuta el reverso de archivo de salida
                     LET v_s_comando = " nohup time fglrun ",m_c_ruta_bin CLIPPED,"/GRTR05 ",
                                                             p_v_usuario, " ",
                                                             v_d_pid, " ",
                                                             m_i_proceso_cod, " ",
                                                             m_i_opera_cod, " ",
                                                             v_d_folio, " ",
                                                             v_ar_tbl_archivos[v_i_indice_slct].nom_archivo, " ",
                                                             --v_ar_tbl_archivos[v_i_indice_slct].lote, " 1> ",
                                                             v_ar_tbl_archivos[v_i_indice_slct].id_cre_ctr_archivo, " 1> ",
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

# Objetivo: Se valida el proceso de reverso de Uso garantía
FUNCTION fn_valida_uso_garantia(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo,
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_uso FROM v_s_qryTxt
   EXECUTE prp_val_uso INTO v_i_cont_regs

   -- se validca si existe información
   IF v_i_cont_regs = 0 THEN
      -- se asume que unicamente se proceso la Validación y permite el reverso
      LET v_b_val_proceso = 1

      RETURN v_b_val_proceso
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado IN (20, 150)\n",
                    "    AND edo_procesar IN (10,80)\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_uso_garant FROM v_s_qryTxt
   EXECUTE prp_val_uso_garant INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","No es posible reversar el proceso, ya que no se encontraron registros con esas condiciones","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Rechazo de Saldos
FUNCTION fn_valida_rechazos(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo,
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_rech1 FROM v_s_qryTxt
   EXECUTE prp_val_rech1 INTO v_i_cont_regs

   IF v_i_cont_regs = 0 THEN 
      -- se crea la sentencia sql que verifica si se puede reversar o no
      LET v_s_qryTxt = " SELECT COUNT(*)\n",
                       "   FROM cre_ctr_archivo\n",
                       "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

      PREPARE prp_val_rech2 FROM v_s_qryTxt
      EXECUTE prp_val_rech2 INTO v_i_cont_regs

      IF v_i_cont_regs = 0 THEN 
         CALL fn_mensaje("Reverso","No existe información a reversar","stop")
          -- se indica que el proceso de reverso no procede
          LET v_b_val_proceso = 0
          RETURN v_b_val_proceso
      ELSE
          -- se asume que solo se realizo la validacion del archivo. Si procede
          LET v_b_val_proceso = 1
          RETURN v_b_val_proceso
      END IF 
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado IN (140)\n",
                    "    AND edo_procesar IN (90, 70)\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_rech FROM v_s_qryTxt
   EXECUTE prp_val_rech INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","No se puede reversar el proceso","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Saldos Transferidos
FUNCTION fn_valida_saldos(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo,
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_saldos1 FROM v_s_qryTxt
   EXECUTE prp_val_saldos1 INTO v_i_cont_regs

   IF v_i_cont_regs = 0 THEN 
      -- se crea la sentencia sql que verifica si se puede reversar o no
      LET v_s_qryTxt = " SELECT COUNT(*)\n",
                       "   FROM cre_ctr_archivo\n",
                       "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

      PREPARE prp_val_saldos2 FROM v_s_qryTxt
      EXECUTE prp_val_saldos2 INTO v_i_cont_regs

      IF v_i_cont_regs = 0 THEN 
         CALL fn_mensaje("Reverso","No existe información a reversar","stop")
          -- se indica que el proceso de reverso no procede
          LET v_b_val_proceso = 0
          RETURN v_b_val_proceso
      ELSE
          -- se asume que solo se realizo la validacion del archivo. Si procede
          LET v_b_val_proceso = 1
          RETURN v_b_val_proceso
      END IF 
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado IN (140)\n",
                    "    AND edo_procesar IN (120)\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_saldos FROM v_s_qryTxt
   EXECUTE prp_val_saldos INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","No se puede reversar el proceso","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Solicitudes Devueltas
FUNCTION fn_valida_devoluciones(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo,
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_devol1 FROM v_s_qryTxt
   EXECUTE prp_val_devol1 INTO v_i_cont_regs

   IF v_i_cont_regs = 0 THEN
      -- se crea la sentencia sql que verifica si se puede reversar o no
      LET v_s_qryTxt = " SELECT COUNT(*)\n",
                       "   FROM cre_ctr_archivo\n",
                       "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

      PREPARE prp_val_devol2 FROM v_s_qryTxt
      EXECUTE prp_val_devol2 INTO v_i_cont_regs

      IF v_i_cont_regs = 0 THEN 
         CALL fn_mensaje("Reverso","No existe información a reversar","stop")
          -- se indica que el proceso de reverso no procede
          LET v_b_val_proceso = 0
          RETURN v_b_val_proceso
      ELSE 
          -- se asume que solo se realizo la validacion del archivo. Si procede
          LET v_b_val_proceso = 1
          RETURN v_b_val_proceso
      END IF
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado IN (140)\n",
                    "    AND edo_procesar IN (100, 70)\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_devol FROM v_s_qryTxt
   EXECUTE prp_val_devol INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","No se puede reversar el proceso","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Solicitudes no Atendidas
FUNCTION fn_valida_noatendidas(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo,
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_no_at1 FROM v_s_qryTxt
   EXECUTE prp_val_no_at1 INTO v_i_cont_regs

   IF v_i_cont_regs = 0 THEN 
      -- se crea la sentencia sql que verifica si se puede reversar o no
      LET v_s_qryTxt = " SELECT COUNT(*)\n",
                       "   FROM cre_ctr_archivo\n",
                       "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

      PREPARE prp_val_no_at2 FROM v_s_qryTxt
      EXECUTE prp_val_no_at2 INTO v_i_cont_regs

      IF v_i_cont_regs = 0 THEN 
         CALL fn_mensaje("Reverso","No existe información a reversar","stop")
          -- se indica que el proceso de reverso no procede
          LET v_b_val_proceso = 0
          RETURN v_b_val_proceso
      ELSE
          -- se asume que solo se realizo la validacion del archivo. Si procede
          LET v_b_val_proceso = 1
          RETURN v_b_val_proceso
      END IF 
   END IF 

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado IN (140)\n",
                    "    AND edo_procesar IN (110, 70)\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_no_at FROM v_s_qryTxt
   EXECUTE prp_val_no_at INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","No se puede reversar el proceso","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Solicitud de Saldos
FUNCTION fn_valida_sol_sdos()
   DEFINE v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado = 140\n",
                    "    AND edo_procesar IN (80, 85)\n",
                    "    AND tpo_transferencia IN ('18','48')"

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

# Objetivo: Se valida el proceso de reverso de Preliquidación
FUNCTION fn_valida_preliquidacion(p_d_folio)
   DEFINE p_d_folio    LIKE bat_ctr_operacion.folio, -- folio
          v_i_cta_reg  INTEGER, -- contador de registros
          v_b_valida   SMALLINT, -- variable que indica si la validación fue correcta o no
          v_s_qryTxt   STRING -- se asigna la sentencia sql a ejecutar

   -- se inicilizan variables
   LET v_b_valida = TRUE

   -- consulta si existe el identificador en la tabla de preliquidación
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_ug_preliquida\n",
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_grtPreliquida FROM v_s_qryTxt
   EXECUTE prp_count_grtPreliquida INTO v_i_cta_reg

   -- verifica si se entontraron registro en la consulta
   IF v_i_cta_reg = 0 THEN
      -- se valida que exista el folio
      LET v_s_qryTxt = " SELECT COUNT(*)\n",
                       "   FROM glo_folio\n",
                       "  WHERE folio = ",p_d_folio

      PREPARE prp_count_gloFolio FROM v_s_qryTxt
      EXECUTE prp_count_gloFolio INTO v_i_cta_reg

      IF v_i_cta_reg = 0 THEN
         -- no se encontro el folio se muestra mensaje de error
         CALL fn_mensaje("Reverso","Folio a reversar no localizado","stop")

         -- se indica que el reverso no procede ya que no existen registros en tabla maestro
         LET v_b_valida = FALSE

         RETURN v_b_valida
      ELSE
         -- se indica que el reverso procede ya que existe el folio
         LET v_b_valida = TRUE

         RETURN v_b_valida
      END IF
   END IF

   -- se consulta si existen registros con estado 130 (Preliquidados)
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado = 130\n",
                    "    AND tpo_transferencia IN ('18','48')"

   PREPARE prp_count_creUsoPrel FROM v_s_qryTxt
   EXECUTE prp_count_creUsoPrel INTO v_i_cta_reg

   -- si no encontro registros se muestra mensaje
   IF v_i_cta_reg = 0 THEN
      CALL fn_mensaje("Reverso","No hay existen preliquidados ó estos ya fueron liquidados","stop")

      -- se indica que el reverso no procede ya que no existen registros preliquidados
      LET v_b_valida = FALSE

      RETURN v_b_valida
   END IF

   RETURN v_b_valida
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Liquidación
FUNCTION fn_valida_liquidacion(p_d_folio)
   DEFINE p_d_folio           LIKE bat_ctr_operacion.folio, -- folio
          v_i_edo_transaccion LIKE cnt_transaccion.estado, -- estado transacción
          v_i_cta_reg         INTEGER, -- contador de registros
          v_b_valida          SMALLINT, -- variable que indica si la validación fue correcta o no
          v_s_qryTxt          STRING -- se asigna la sentencia sql a ejecutar

   -- se asume que la transacción se podrá realizar
   LET v_b_valida = TRUE

   -- se consulta si existe el folio a reversar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cta_movimiento\n",
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_ctaMovim FROM v_s_qryTxt
   EXECUTE prp_count_ctaMovim INTO v_i_cta_reg

   -- si no existe el folio se muestra mensaje
   IF v_i_cta_reg = 0 THEN
      -- no se encontro el folio se muestra mensaje de error
      CALL fn_mensaje("Reverso","Folio a reversar no localizado","stop")

      -- se indica que el reverso no procede ya que no existen registros en tabla maestro
      LET v_b_valida = FALSE

      RETURN v_b_valida
   END IF

   -- se consulta si existen registros con estado 140 (Liquidados) en tabla maestro
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado = 140\n",
                    "    AND tpo_transferencia IN ('18','48')"

   PREPARE prp_count_creUsoLiq FROM v_s_qryTxt
   EXECUTE prp_count_creUsoLiq INTO v_i_cta_reg

   -- si no existen registros se muestra mensaje
   IF v_i_cta_reg = 0 THEN
      CALL fn_mensaje("Reverso","No hay registros liquidados","stop")

      -- se indica que el reverso no procede ya que no existen registros en tabla maestro
      LET v_b_valida = FALSE

      RETURN v_b_valida
   END IF

   -- se consulta si existen registros en cnt transacción
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cnt_transaccion\n",                
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_cntTrans FROM v_s_qryTxt
   EXECUTE prp_count_cntTrans INTO v_i_cta_reg

   -- si no existen registros se muestra mensaje
   IF v_i_cta_reg > 0 THEN
      -- se consulta el estado en cnt transaccion
      LET v_s_qryTxt = " SELECT UNIQUE estado\n",
                       "   FROM cnt_transaccion\n",
                       "  WHERE folio_liquida = ",p_d_folio

      PREPARE prp_uniq_cntTrans FROM v_s_qryTxt
      EXECUTE prp_uniq_cntTrans INTO v_i_edo_transaccion

      -- si el estado es diferente a 10 no se realiza el reverso
      IF v_i_edo_transaccion <> 10 THEN
         CALL fn_mensaje("Reverso de Transacciones","Folio ya contabilizado","stop")

         -- se indica que el reverso no procede ya que no existen registros en tabla maestro
         LET v_b_valida = FALSE

         RETURN v_b_valida
      END IF
   END IF

   RETURN v_b_valida
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
      WHEN "opt1" -- uso de garantía
         LET v_i_proceso_cod = g_proc_cod_grt_uso_garantia -- recepción uso de garantía 43bis
         LET v_i_opera_cod = 1 -- valida archivo solicitud uso 43bis

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt2" -- Rechazo de Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_uso_rech_saldos -- recepciÓn rechazo de saldos uso 43bis
         LET v_i_opera_cod = 1 -- valida rechazo saldos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt3" -- Saldos Transferidos
         LET v_i_proceso_cod = g_proc_cod_grt_uso_sdos_transf -- recepción saldos transferidos uso 43bis
         LET v_i_opera_cod = 1 -- valida saldos transferidos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt4" -- Solicitudes Devueltas
         LET v_i_proceso_cod = g_proc_cod_grt_uso_devol_solic -- recepción devol solicitudes uso 43bis
         LET v_i_opera_cod = 1 -- valida devolución solicitudes

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt5" -- Solicitudes no Atendidas
         LET v_i_proceso_cod = g_proc_cod_grt_uso_no_atendidas -- recepción solic no atendidas uso 43bis
         LET v_i_opera_cod = 1 -- valida solicitudes no atendidas

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt7" -- Solicitud de Saldos
         LET v_i_proceso_cod = g_proc_cod_grt_uso_arch_solic -- generación archivo solic sdos uso 43bis
         LET v_i_opera_cod = 1 -- solicitud de saldos

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
