--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRL46                                        #
#Objetivo          =>Programa que genera el archivo de salida de   #
#                    amortización fondo 72 para el módulo de ACR   #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>03 Julio 2012                                 #
####################################################################

DATABASE safre_viv
GLOBALS "ACRG10.4gl"

MAIN
   DEFINE p_v_nom_prog        VARCHAR(30), -- nombre del programa
          p_b_tipo_carga      SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario         LIKE seg_usuario.usuario, -- usuario firmado al sistema
          v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          v_dt_f_liquidacion  DATE, -- fecha auxiliar de presentacion
          v_i_folio_liquida   INTEGER, -- numero de folio
          v_c_f_liquida_aux   CHAR(8), -- fecha ayxiliar de presentacion con formato YYYYMMDD
          v_conteo            DECIMAL(9,0), -- numerica para contar registros
          v_c_extension       LIKE cat_operacion.extension, -- extensión del archivo
          r_c_ruta_bin        LIKE seg_modulo.ruta_bin, -- ruta bin del módulo
          r_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados del módulo
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".ACRL46.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan variables
   LET v_i_proceso_cod = g_proc_cod_acr_arch_amort72 -- generación archivo amortizaciÓn fondo72
   LET v_i_opera_cod = 1 -- genera archivo amortización fondo72 acr
   LET v_d_pid = 0
   LET v_i_folio_liquida = NULL
   LET v_v_nom_archivo = "N/A"
   LET v_c_programa_cod = "ACRL46"

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   LET v_dt_f_liquidacion = ""
   OPEN WINDOW w_genera_archAmortizacion WITH FORM "ACRL461"

   INPUT
     v_i_folio_liquida, v_dt_f_liquidacion
   WITHOUT DEFAULTS
   FROM
     folio_liquidacion, f_liquidacion
   ATTRIBUTES ( UNBUFFERED )

   --se llama a la funcion que genera el archivo
   ON ACTION ACCEPT
      -- se valida el folio
      IF v_i_folio_liquida IS NULL OR v_i_folio_liquida = 0 THEN
         CALL fn_mensaje("Armotización","Debe ingresar un folio para continuar","stop")
         CONTINUE INPUT
      END IF

      -- se fecha de liquidación
      IF v_dt_f_liquidacion IS NULL THEN
         CALL fn_mensaje("Armotización","Debe ingresar una fecha para continuar","stop")
         CONTINUE INPUT
      END IF

      -- se obtiene la extensión del archivo
      LET v_c_extension = fn_recupera_extension(v_i_proceso_cod, v_i_opera_cod)

      -- se cambia el formato de la fecha de liquidación a YYYYMMDD
      LET v_c_f_liquida_aux = v_dt_f_liquidacion USING "YYYYMMDD"
      LET v_v_nom_archivo = "A" || v_c_f_liquida_aux || "." || v_c_extension CLIPPED

      -- se valida que exista informacion para generar el archivo
      -- (usando el mismo query que se usa en el archivo de salida)
      SELECT COUNT(*)
        INTO v_conteo
        FROM cta_fondo72
       WHERE folio_liquida = v_i_folio_liquida
         AND movimiento = 452  ---482 --52

      -- si hay por lo menos un registro
      IF ( v_conteo <= 0 ) THEN
         -- se le indica al usuario que no hay datos para generar el archivo
         CALL fn_mensaje("Aviso","No se tienen datos para generar el archivo con los criterios dados","stop")
         CONTINUE INPUT
      END IF

      -- se crea la sentencia sql que ejecuta la funcion que genera el pid
      LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_v_usuario)

      -- se invoca la funcion que inicializa el proceso
      LET r_b_valida = fn_inicializa_proceso(v_d_pid,
                                             v_i_proceso_cod,
                                             v_i_opera_cod,
                                             v_i_folio_liquida,
                                             v_c_programa_cod,
                                             v_v_nom_archivo,
                                             p_v_usuario)

      -- se verifica si fue posible inicializar el proceso
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(r_b_valida)

         EXIT INPUT
      END IF

      -- se invoca la función que deja la operación en estado Procesando
      LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,
                                              v_i_proceso_cod,
                                              v_i_opera_cod,
                                              v_i_folio_liquida,
                                              v_c_programa_cod,
                                              v_v_nom_archivo,
                                              p_v_usuario)

      -- se verifica si fue posible actualiza a PRECESANDO la operación
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(r_b_valida)

         EXIT INPUT
      END IF

      -- se crea el comando que ejecuta el modulo que genera el archivo de salida de amortización
      LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/ACRS15 ",
                                              p_v_usuario, " ",
                                              v_d_pid, " ",
                                              v_i_proceso_cod, " ",
                                              v_i_opera_cod, " ",
                                              v_i_folio_liquida, " ",
                                              v_v_nom_archivo, " 1> ",
                                              v_c_ruta_list_bat CLIPPED,
                                              "/nohup:",v_d_pid USING "&&&&&",":",
                                              v_i_proceso_cod USING "&&&&&",":",
                                              v_i_opera_cod USING "&&&&&",
                                              " 2>&1 &"

      -- se ejecuta el comando armado
      RUN v_s_comando

      CALL fn_mensaje("Aviso","Se ejecutó el proceso de generación de archivo Amortización","information")

      EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_genera_archAmortizacion
END MAIN