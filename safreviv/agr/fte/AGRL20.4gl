####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRL20                                        #
#Objetivo          =>Programa que pide al usuario la seleccion del #
#                    proceso a reversar: Archivos Entrada,         #
#                    Archivos Salida, Transacciones                #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>10 Febrero 2012                               #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- usuario firmado al sistema
       m_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
       m_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
       m_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
       m_i_tpo_originacion LIKE cre_acreditado.tpo_originacion -- tipo de originación

MAIN
   DEFINE p_b_tipo_carga      SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_nom_prog        VARCHAR(30), -- nombre del programa
          v_c_op_arch_ent     VARCHAR(5), -- selección del modulo a reversar (Archivos entrada)
          v_c_op_arch_sal     VARCHAR(5), -- selección del modulo a reversar (Archivos salida)
          v_ci_op_arch_tran   VARCHAR(5), -- selección del modulo a reversar (Transacciones)
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_d_folio           LIKE bat_ctr_operacion.folio, -- folio
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- recupera los parametros que vienen del principal
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".AGRL20.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inializan variables
   LET m_i_proceso_cod = g_proc_cod_agr_reverso -- reverso anualidades garantizadas
   LET m_i_opera_cod = 1 -- reverso anualidades garantizadas
   LET m_c_programa_cod = "AGRL20"
   LET m_i_tpo_originacion = 4 -- Anualidades Garantizadas
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

   OPEN WINDOW w_reverso WITH FORM "AGRL201"
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
          v_c_ruta_bin          LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          v_c_ruta_list_bat     LIKE seg_modulo.ruta_listados, -- ruta listados de bat
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
          r_b_valida            SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          r_v_extension         LIKE cat_operacion.extension, -- extensión del archivo
          v_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de la tabla de control

   -- se inicializan variables
   LET v_d_folio = 0
   LET v_d_folio_arch = 0
   LET v_d_pid = 0

   -- se verifica la opción seleccionada para poner nombre en el grupo
   CASE p_c_op_arch_ent
      WHEN "opt1"
         LET v_nom_grupo = "Recurrente"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_recurrente
         LET v_i_opera_cod_rev = 2
      WHEN "opt2"
         LET v_nom_grupo = "Rechazo de Saldos"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_rech_saldos
         LET v_i_opera_cod_rev = 2
      WHEN "opt3"
         LET v_nom_grupo = "Saldos Transferidos"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_sdos_transf -- recepción saldos transferidos agr
         LET v_i_opera_cod_rev = 2
      WHEN "opt4"
         LET v_nom_grupo = "Solicitudes Devueltas"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_devol_solic
         LET v_i_opera_cod_rev = 2
      WHEN "opt5"
         LET v_nom_grupo = "Solicitudes no Atendidas"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_no_atendidas
         LET v_i_opera_cod_rev = 2
      WHEN "opt6"
         LET v_nom_grupo = "Solicitudes de Desmarca"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_solic_desmarca
         LET v_i_opera_cod_rev = 2
      WHEN "opt11"
         LET v_nom_grupo = "Uso Anualidad o Garantía"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_uso_anualid 
         LET v_i_opera_cod_rev = 2
      WHEN "opt12"
         LET v_nom_grupo = "Solicitudes de Marca/Desmarca"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_recurr_marca 
         LET v_i_opera_cod_rev = 2
   END CASE

   -- se obtiene el del proceso y operación a reversar
   LET v_d_pid_rev = fn_max_pid(v_i_proceso_cod_rev, v_i_opera_cod_rev)

   -- se valida el pid del reverso
   IF v_d_pid_rev IS NULL THEN
      LET v_d_pid_rev = 0
   END IF

   -- se obtienen las rutas de control del modulo
   LET v_s_qryTxt = " SELECT ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_bin1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_bin1 INTO v_c_ruta_bin

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   OPEN WINDOW w_arch_entrada WITH FORM "AGRL202"
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
      CALL fn_mensaje("Reverso Archivo Entrada","No se encontraron archivos cargados con la información proporcionada","stop")
      RETURN
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
            WHEN "opt1" -- Recurrente
               CALL fn_valida_recurrente(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt2" -- Rechazo de Saldos
               CALL fn_valida_rechazos(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt3" -- Saldos Transferidos
               CALL fn_valida_saldos(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt4" -- Solicitudes Devueltas
               CALL fn_valida_devoluciones(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt5" -- Solicitudes no Atendidas
               CALL fn_valida_noatendidas(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt6" -- Solicitudes de Desmarca
               CALL fn_valida_desmarca(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt11" -- Uso Anualidad o Garantia
               CALL fn_valida_usoanualidad(v_id_cre_ctr_archivo) RETURNING v_b_continua
            WHEN "opt12" -- Solicitudes de Marca/Desmarca
               CALL fn_valida_marcadesmarca(v_id_cre_ctr_archivo) RETURNING v_b_continua
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
               LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin CLIPPED,"/AGRR01 ",
                                                       p_v_usuario, " ",
                                                       v_d_pid, " ",
                                                       m_i_proceso_cod, " ",
                                                       m_i_opera_cod, " ",
                                                       v_d_folio_arch, " ",
                                                       v_v_nom_archivo, " ",
                                                       v_d_pid_rev, " ",
                                                       p_c_op_arch_ent, " ",
                                                       v_id_cre_ctr_archivo, " 1> ",
                                                       v_c_ruta_list_bat CLIPPED,
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
      WHEN "opt1" -- Recurrente
         LET v_i_proceso_cod = g_proc_cod_agr_recurrente -- recepción recurrente anualidades
         LET v_i_opera_cod = 1 -- valida archivo recurrente

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt11" -- Uso de Anualidad o Garantia
         LET v_i_proceso_cod = g_proc_cod_agr_uso_anualid -- recepción uso anualidades garantizadas
         LET v_i_opera_cod = 1 -- valida archivo uso anualidad

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt2" -- Rechazo de Saldos
         LET v_i_proceso_cod = g_proc_cod_agr_rech_saldos -- recepciÓn rechazo de saldos agr
         LET v_i_opera_cod = 1 -- valida rechazo saldos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt3" -- Saldos Transferidos
         LET v_i_proceso_cod = g_proc_cod_agr_sdos_transf -- recepciÓn saldos transferidos agr
         LET v_i_opera_cod = 1 -- valida saldos transferidos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt4" -- Solicitudes Devueltas
         LET v_i_proceso_cod = g_proc_cod_agr_devol_solic -- recepciÓn devoluciÓn solicitudes agr
         LET v_i_opera_cod = 1 -- valida devoluciÓn solicitudes

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt5" -- Solicitudes no Atendidas
         LET v_i_proceso_cod = g_proc_cod_agr_no_atendidas -- recepción solicitudes no atendidas agr
         LET v_i_opera_cod = 1 -- valida solicitudes no atendidas

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt6" -- Solicitudes de Desmarca
         LET v_i_proceso_cod = g_proc_cod_agr_solic_desmarca -- solicitud desmarca anualidades
         LET v_i_opera_cod = 1 -- valida archivo solicitud desmarca

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt7" -- Solicitud de Saldos
         LET v_i_proceso_cod = g_proc_cod_agr_arch_solic -- generación archivo solicitud saldos agr
         LET v_i_opera_cod = 1 -- genera archivo solicitud de saldos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt12" -- Solicitudes de marca/desmarca
         LET v_i_proceso_cod = g_proc_cod_agr_recurr_marca -- recepción recurrente marca y desmarca
         LET v_i_opera_cod = 1 -- genera archivo solicitud de saldos

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
          v_c_ruta_list_bat     LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_c_ruta_bin          LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          v_d_pid               LIKE bat_ctr_operacion.pid, -- identificador del proceso
          v_v_nom_archivo       LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_s_qryTxt            STRING, -- se asigna una sentencia sql a ejecutar
          v_s_comando           STRING, -- contiene al comando a correr
          v_s_mensaje           STRING, -- mensaje a mostrar a usuario
          v_b_continua          SMALLINT, -- booleana que indica si el proceso continua
          v_i_cta_reg           SMALLINT, --contador de registros
          r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se inicializan variables
   LET v_d_pid = 0
   LET v_d_folio = NULL
   LET v_v_nom_archivo = NULL

   -- se obtienen las rutas de control del modulo
   LET v_s_qryTxt = " SELECT ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_bin2 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_bin2 INTO v_c_ruta_bin

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat4 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat4 INTO v_c_ruta_list_bat

   OPEN WINDOW w_arch_transaccion WITH FORM "AGRL204"
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
            WHEN "opt10" -- Saldos Remanentes
               CALL fn_valida_saldos_reman(v_d_folio) RETURNING v_b_continua
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
               LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin CLIPPED,"/AGRR02 ",
                                                       p_v_usuario, " ",
                                                       v_d_pid, " ",
                                                       m_i_proceso_cod, " ",
                                                       m_i_opera_cod, " ",
                                                       v_d_folio, " ",
                                                       v_v_nom_archivo, " ",
                                                       p_c_op_arch_tran, " ",
                                                       m_i_tpo_originacion, " 1> ",
                                                       v_c_ruta_list_bat CLIPPED,
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
          v_c_ruta_bin          LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          v_c_ruta_list_bat     LIKE seg_modulo.ruta_listados, -- ruta listados de bat
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

   -- se obtienen las rutas de control del modulo
   LET v_s_qryTxt = " SELECT ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_bin3 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_bin3 INTO v_c_ruta_bin

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat2 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat2 INTO v_c_ruta_list_bat

   OPEN WINDOW w_arch_salida WITH FORM "AGRL203"
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
            LET v_s_condicion = v_s_condicion, "AND nom_archivo LIKE '%.",r_v_extension CLIPPED,"'\n"
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
                     LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin CLIPPED,"/AGRR03 ",
                                                             p_v_usuario, " ",
                                                             v_d_pid, " ",
                                                             m_i_proceso_cod, " ",
                                                             m_i_opera_cod, " ",
                                                             v_d_folio, " ",
                                                             v_ar_tbl_archivos[v_i_indice_slct].nom_archivo, " ",
                                                             v_ar_tbl_archivos[v_i_indice_slct].id_cre_ctr_archivo, " 1> ",
                                                             v_c_ruta_list_bat CLIPPED,
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

# Objetivo: Se valida el proceso de reverso de Recurrente
FUNCTION fn_valida_recurrente(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo,
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_recurr1 FROM v_s_qryTxt
   EXECUTE prp_val_recurr1 INTO v_i_cont_regs

   -- se validca si existe información en cre acreditado
   IF v_i_cont_regs = 0 THEN
      -- se asume que unicamente se proceso la Validación y permite el reverso
      LET v_b_val_proceso = 1

      RETURN v_b_val_proceso
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado <> 150\n",
                    "    AND edo_procesar > 50\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_recurr FROM v_s_qryTxt
   EXECUTE prp_val_recurr INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs > 0 THEN
      CALL fn_mensaje("Reverso","No es posible reversar el proceso, ya que los estados ya fueron actualizados por PROCESAR","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0

      RETURN v_b_val_proceso
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado IN (130, 140, 145)\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_preliquidacion FROM v_s_qryTxt
   EXECUTE prp_val_preliquidacion INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs > 0 THEN
      CALL fn_mensaje("Reverso","No es posible reversar el proceso, ya que los créditos ya fueron preliquidados","stop")

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
                    "   FROM cre_his_acreditado\n",
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
                    "   FROM cre_his_acreditado\n",
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
                    "   FROM cre_his_acreditado\n",
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
                    "   FROM cre_his_acreditado\n",
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
                    "   FROM cre_his_acreditado\n",
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
                    "   FROM cre_his_acreditado\n",
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
                    "   FROM cre_his_acreditado\n",
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
                    "   FROM cre_his_acreditado\n",
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

# Objetivo: Se valida el proceso de reverso de Solicitudes de Desmarca
FUNCTION fn_valida_desmarca(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo,
          v_b_val_proceso SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs   INTEGER, -- contador de registros
          v_s_qryTxt      STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_desmarca1 FROM v_s_qryTxt
   EXECUTE prp_val_desmarca1 INTO v_i_cont_regs

   IF v_i_cont_regs = 0 THEN 
      -- se crea la sentencia sql que verifica si se puede reversar o no
      LET v_s_qryTxt = " SELECT COUNT(*)\n",
                       "   FROM cre_ctr_archivo\n",
                       "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

      PREPARE prp_val_desmarca2 FROM v_s_qryTxt
      EXECUTE prp_val_desmarca2 INTO v_i_cont_regs

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
                    "   FROM cre_acreditado\n",
                    "  WHERE estado IN (160, 170)\n",
                    "    AND edo_procesar < 170\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_desmarca FROM v_s_qryTxt
   EXECUTE prp_val_desmarca INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs = 0 THEN
      CALL fn_mensaje("Reverso","Solicitud de desmarca ya fue enviada a PROCESAR","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Solicitudes de Desmarca
FUNCTION fn_valida_usoanualidad(p_id_cre_ctr_archivo)
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

   PREPARE prp_val_uso_anualid FROM v_s_qryTxt
   EXECUTE prp_val_uso_anualid INTO v_i_cont_regs

   -- se validca si existe información en cre acreditado
   IF v_i_cont_regs = 0 THEN
      -- se asume que unicamente se proceso la Validación y permite el reverso
      LET v_b_val_proceso = 1

      RETURN v_b_val_proceso
   END IF
{
   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    " FROM cre_acreditado\n",
                    " WHERE edo_procesar > 60\n",
                    " AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_uso_anualid2 FROM v_s_qryTxt
   EXECUTE prp_val_uso_anualid2 INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs > 0 THEN
      CALL fn_mensaje("Reverso","No es posible reversar el proceso, ya que los estados ya fueron actualizados por PROCESAR","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0

      RETURN v_b_val_proceso
   END IF
}
   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado IN (130, 140)\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_uso_anualid3 FROM v_s_qryTxt
   EXECUTE prp_val_uso_anualid3 INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs > 0 THEN
      CALL fn_mensaje("Reverso","No es posible reversar el proceso, ya que los créditos ya fueron preliquidados","stop")

      -- se indica que el proceso de reverso no procede
      LET v_b_val_proceso = 0
   END IF

   RETURN v_b_val_proceso
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Solicitudes de Marca/Desmarca
FUNCTION fn_valida_marcadesmarca(p_id_cre_ctr_archivo)
   DEFINE p_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo,
          --p_folio_archivo      LIKE cre_ctr_archivo.folio_archivo, -- folio del archivo
          v_b_val_proceso      SMALLINT, -- indica si el proceso de reverso procede o no
          v_i_cont_regs        INTEGER, -- contador de registros
          v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   -- se asume que la validación será correcta
   LET v_b_val_proceso = 1

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_his_acreditado\n",
                    "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_marca_desm1 FROM v_s_qryTxt
   EXECUTE prp_val_marca_desm1 INTO v_i_cont_regs

   -- se validca si existe información en cre acreditado
   IF v_i_cont_regs = 0 THEN
      -- se asume que unicamente se proceso la Validación y permite el reverso
      LET v_b_val_proceso = 1

      RETURN v_b_val_proceso
   END IF

   -- se crea la sentencia sql que verifica si se puede reversar o no
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_his_acreditado\n",
                    "  WHERE edo_procesar NOT IN (30, 35, 50, 55, 60, 200, 210)\n",
                    "    AND id_cre_ctr_archivo = ",p_id_cre_ctr_archivo

   PREPARE prp_val_marca_desm2 FROM v_s_qryTxt
   EXECUTE prp_val_marca_desm2 INTO v_i_cont_regs

   -- se verifica el contador de regsitros
   IF v_i_cont_regs > 0 THEN
      CALL fn_mensaje("Reverso","No es posible reversar el proceso, ya que los créditos ya fueron procesados","stop")

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
                    "   FROM cre_acreditado\n",
                    "  WHERE estado = 140\n",
                    "    AND edo_procesar IN (80, 85)\n",
                    "    AND tpo_originacion = ",m_i_tpo_originacion

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
                    "   FROM cre_ag_preliquida\n",
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_agrPreliquida FROM v_s_qryTxt
   EXECUTE prp_count_agrPreliquida INTO v_i_cta_reg

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
         -- se indica que el reverso procede ya que existen folio
         LET v_b_valida = TRUE

         RETURN v_b_valida
      END IF
   END IF

   -- se consulta si existen registros con estado 130 (Preliquidados)
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado = 130\n",
                    "    AND tpo_originacion = ",m_i_tpo_originacion

   PREPARE prp_count_creAcredPrel FROM v_s_qryTxt
   EXECUTE prp_count_creAcredPrel INTO v_i_cta_reg

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
                    "   FROM cre_acreditado\n",
                    "  WHERE estado = 140\n",
                    "    AND tpo_originacion = ",m_i_tpo_originacion

   PREPARE prp_count_creAcredLiq FROM v_s_qryTxt
   EXECUTE prp_count_creAcredLiq INTO v_i_cta_reg

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

# Objetivo: Se valida el proceso de reverso de Saldos Remanentes
FUNCTION fn_valida_saldos_reman(p_d_folio)
   DEFINE p_d_folio    LIKE bat_ctr_operacion.folio, -- folio
          v_i_cta_reg  INTEGER, -- contador de registros
          v_b_valida   SMALLINT, -- variable que indica si la validación fue correcta o no
          v_s_qryTxt   STRING -- se asigna la sentencia sql a ejecutar

   -- se asume que la transacción se podrá realizar
   LET v_b_valida = TRUE

   -- se consulta que existan registros con estado = 20 y edo_procesar = 70
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE estado = 25\n",
                    "    AND edo_procesar IN (70, 5)"

   PREPARE prp_count_creAcredSdo FROM v_s_qryTxt
   EXECUTE prp_count_creAcredSdo INTO v_i_cta_reg

   -- si no existen registros se muestra mensaje
   IF v_i_cta_reg = 0 THEN
      CALL fn_mensaje("Reverso","No hay registros localizados como saldos remanentes","stop")

      -- se indica que el reverso no procede ya que no existen registros en tabla maestro
      LET v_b_valida = FALSE

      RETURN v_b_valida
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
      WHEN "opt1" -- Recurrente
         LET v_i_proceso_cod = g_proc_cod_agr_recurrente -- recepciÓn recurrente anualidades
         LET v_i_opera_cod = 1 -- valida archivo recurrente

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt11" -- Uso de Anualidad o Garantia
         LET v_i_proceso_cod = g_proc_cod_agr_uso_anualid  -- recepción uso anualidades garantizadas
         LET v_i_opera_cod = 1 -- valida archivo uso anualidad

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt2" -- Rechazo de Saldos
         LET v_i_proceso_cod = g_proc_cod_agr_rech_saldos -- recepciÓn rechazo de saldos agr
         LET v_i_opera_cod = 1 -- valida rechazo saldos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt3" -- Saldos Transferidos
         LET v_i_proceso_cod = g_proc_cod_agr_sdos_transf -- recepciÓn saldos transferidos agr
         LET v_i_opera_cod = 1 -- valida saldos transferidos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt4" -- Solicitudes Devueltas
         LET v_i_proceso_cod = g_proc_cod_agr_devol_solic -- recepción devolución solicitudes agr
         LET v_i_opera_cod = 1 -- valida devoluciÓn solicitudes

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt5" -- Solicitudes no Atendidas
         LET v_i_proceso_cod = g_proc_cod_agr_no_atendidas -- recepción solicitudes no atendidas agr
         LET v_i_opera_cod = 1 -- valida solicitudes no atendidas

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt6" -- Solicitudes de Desmarca
         LET v_i_proceso_cod = g_proc_cod_agr_solic_desmarca -- solicitud desmarca anualidades
         LET v_i_opera_cod = 1 -- valida archivo solicitud desmarca

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt7" -- Solicitud de Saldos
         LET v_i_proceso_cod = g_proc_cod_agr_arch_solic -- generación archivo solicitud saldos agr
         LET v_i_opera_cod = 1 -- genera archivo solicitud de saldos

         -- se consulta la extensión para el proceso y operacion
         LET v_s_qryTxt = " SELECT UNIQUE extension\n",
                          "   FROM cat_operacion\n",
                          "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                          "    AND opera_cod = ",v_i_opera_cod

      WHEN "opt12" -- Solicitud de marca/desmarca
         LET v_i_proceso_cod = g_proc_cod_agr_recurr_marca -- recepción recurrente marca y desmarca
         LET v_i_opera_cod = 1 -- genera archivo solicitud de saldos

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
