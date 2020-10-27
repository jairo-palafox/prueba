--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

################################################################################
#Modulo       => ACR                                                           #
#Programa     => ACRL43                                                        #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la       #
#                agrupacion para DSE devoluciones                              #
#Autor        => Daniel Buendia, EFP                                           #
#Fecha inicio => 4 de febrero de 2012                                          #
#                Mauro Muñiz Caballero                                         #
#                Se agrega agrupación por Mejoravit "mjv"                      #
#Fecha inicio => 4 de febrero de 2012                                          #
#                Mauro Muñiz Caballero                                         #
#                Se agrega agrupación por Manos a la obra "mao"                #
#Fecha inicio => 12 de febrero de 2016                                         #
#                Mauro Muñiz Caballero                                         #
#                Se agrega agrupación por Un cuarto mas "ucm"                  #
#Fecha inicio => 16 de enero de 2018                                           #
#                Mauro Muñiz Caballero                                         #
#                Se agrega agrupación por crédito liquidado "ral"              #
################################################################################

DATABASE safre_viv

GLOBALS "ACRG10.4gl"

    DEFINE g_pid                    LIKE bat_ctr_proceso.pid       -- ID del proceso
    DEFINE g_proceso_cod            LIKE cat_proceso.proceso_cod   -- cÓdigo del proceso
    DEFINE g_opera_cod              LIKE cat_operacion.opera_cod   -- cÓdigo de operaciÓn
    DEFINE g_ruta_bin               CHAR(40)
    DEFINE g_ruta_listados          CHAR(40)

MAIN

   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo                STRING -- titulo de la ventana
   --DEFINE v_folio                   LIKE deo_preliquida.folio_liquida,
   DEFINE v_i_num_regs              INTEGER -- contiene el numero de registro encontrados
   DEFINE v_proceso_desc            STRING  -- descripcion del proceso
   DEFINE v_opera_desc              STRING  -- descripcion de la operacion
   DEFINE v_s_qryTxt                STRING  -- contiene una sentencia sql a ejecutar
   DEFINE v_b_respuesta             BOOLEAN -- booleana que indica la confirmación de usuario
   DEFINE v_s_mensaje               STRING  -- mensaje a mostrar a usuario
   DEFINE v_si_precio_fondo         LIKE glo_valor_fondo.precio_fondo -- precio fondo de acción
   DEFINE v_c_tpo_transf            LIKE dse_devolucion.tpo_transferencia -- tipo de transferencia
   DEFINE r_b_resultado             SMALLINT -- resultado de validar la operacion

   DEFINE v_r_agrup RECORD 
      aivs97_dse                    DECIMAL(22,6), -- AIVS 97 DSE
      aivs97_sep                    DECIMAL(22,6), -- AIVS 97 SEP
      aivs97_dis                    DECIMAL(22,6), -- AIVS 97 DIS
      aivs97_mjv                    DECIMAL(22,6), -- AIVS 97 MEJORAVIT
      aivs97_mao                    DECIMAL(22,6), -- AIVS 97 Manos a la obra
      aivs97_ucm                    DECIMAL(22,6), -- AIVS 97 Un cuarto mas
      aivs97_ral                    DECIMAL(22,6), -- AIVS 97 Crédito liquidado
      pesos97_dse                   DECIMAL(22,2), -- Pesos 97 DSE
      pesos97_sep                   DECIMAL(22,2), -- Pesos 97 SEP
      pesos97_dis                   DECIMAL(22,2), -- Pesos 97 DIS
      pesos97_mjv                   DECIMAL(22,2), -- Pesos 97 MEJORAVIT
      pesos97_mao                   DECIMAL(22,2), -- Pesos 97 Manos a la obra
      pesos97_ucm                   DECIMAL(22,2), -- Pesos 97 Un cuarto mas
      pesos97_ral                   DECIMAL(22,2), -- Pesos 97 Crédito liquidado
      aivs92_dse                    DECIMAL(22,6), -- AIVS 92 DSE
      aivs92_sep                    DECIMAL(22,6), -- AIVS 92 SEP
      aivs92_dis                    DECIMAL(22,6), -- AIVS 92 DIS
      aivs92_mjv                    DECIMAL(22,6), -- AIVS 92 MEJORAVIT
      aivs92_mao                    DECIMAL(22,6), -- AIVS 92 Manos a la obra
      aivs92_ucm                    DECIMAL(22,6), -- AIVS 92 Un cuarto mas
      aivs92_ral                    DECIMAL(22,6), -- AIVS 92 Crédito liquidado
      pesos92_dse                   DECIMAL(22,2), -- Pesos 92 DSE
      pesos92_sep                   DECIMAL(22,2), -- Pesos 92 SEP
      pesos92_dis                   DECIMAL(22,2), -- Pesos 92 DIS
      pesos92_mjv                   DECIMAL(22,2), -- Pesos 92 MEJORAVIT
      pesos92_mao                   DECIMAL(22,2), -- Pesos 92 Manos a la obra
      pesos92_ucm                   DECIMAL(22,2), -- Pesos 92 Un cuarto mas
      pesos92_ral                   DECIMAL(22,2), -- Pesos 92 Crédito liquidado
      aivs97_tot                    DECIMAL(22,6), -- Total AIVS 97
      pesos97_tot                   DECIMAL(22,2), -- Total Pesos 97
      aivs92_tot                    DECIMAL(22,6), -- Total AIVS 92
      pesos92_tot                   DECIMAL(22,2) -- Total Pesos 92
   END RECORD

   -- se recuperan las claves desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el título, se pone como título de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se crea el archivo log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACRL43.log")
   
   -- se inicializan variables
   LET g_proceso_cod  = g_proc_cod_acr_agrupacion_dse -- Agrupación de DSE Devoluciones
   LET g_opera_cod    = 1  --  agrupación
   LET v_c_tpo_transf = "15"
   --LET v_folio       = 0

   -- se obtienen las rutas de control del modulo
   CALL fn_rutas("acr") RETURNING g_ruta_bin, g_ruta_listados

   -- se ejecuta la función que valida la operación
   LET r_b_resultado = fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   
   -- se verifica si se puede continuar con la operacion
   IF r_b_resultado <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_resultado)

      EXIT PROGRAM
   END IF

   -- se obtiene el valor del fondo de la fecha del dia
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11\n",
                    "    AND f_valuacion = TODAY"

   PREPARE prp_precio_fondo FROM v_s_qryTxt
   EXECUTE prp_precio_fondo INTO v_si_precio_fondo

   -- si no existe el valor del fondo para la fecha dada
   IF ( v_si_precio_fondo IS NULL ) THEN
      -- generar un error
      CALL fn_mensaje("Atención","No es posible continuar con la agrupación ya que no existe\nprecio de aiv para la fecha actual","stop")

      EXIT PROGRAM
   END IF

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_agrupacion WITH FORM "ACRL431"
   MENU
      BEFORE MENU
         -- se obtienen los totales para Cartera
         CALL fn_obt_agrup_modulo("acr", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_dse, v_r_agrup.pesos97_dse, v_r_agrup.aivs92_dse, v_r_agrup.pesos92_dse

         -- se obtienen los totales para Separación
         CALL fn_obt_agrup_modulo("sep", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_sep, v_r_agrup.pesos97_sep, v_r_agrup.aivs92_sep, v_r_agrup.pesos92_sep

         -- se obtienen los totales para Dispersión
         CALL fn_obt_agrup_modulo("dis", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_dis, v_r_agrup.pesos97_dis, v_r_agrup.aivs92_dis, v_r_agrup.pesos92_dis

         -- se obtienen los totales para Mejoravit
         CALL fn_obt_agrup_modulo("mjv", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_mjv, v_r_agrup.pesos97_mjv, v_r_agrup.aivs92_mjv, v_r_agrup.pesos92_mjv

         -- se obtienen los totales para Manos a la obra
         CALL fn_obt_agrup_modulo("mao", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_mao, v_r_agrup.pesos97_mao, v_r_agrup.aivs92_mao, v_r_agrup.pesos92_mao

         -- se obtienen los totales para Un cuarto mas
         CALL fn_obt_agrup_modulo("ucm", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_ucm, v_r_agrup.pesos97_ucm, v_r_agrup.aivs92_ucm, v_r_agrup.pesos92_ucm

         -- se obtienen los totales para Un cuarto mas
         CALL fn_obt_agrup_modulo("ral", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_ral, v_r_agrup.pesos97_ral, v_r_agrup.aivs92_ral, v_r_agrup.pesos92_ral

         -- se calculan los totales
         LET v_r_agrup.aivs97_tot  = v_r_agrup.aivs97_dse  + v_r_agrup.aivs97_sep  + v_r_agrup.aivs97_dis  + v_r_agrup.aivs97_mjv  + v_r_agrup.aivs97_mao  + v_r_agrup.aivs97_ucm  + v_r_agrup.aivs97_ral
         LET v_r_agrup.pesos97_tot = v_r_agrup.pesos97_dse + v_r_agrup.pesos97_sep + v_r_agrup.pesos97_dis + v_r_agrup.pesos97_mjv + v_r_agrup.pesos97_mao + v_r_agrup.pesos97_ucm + v_r_agrup.pesos97_ral
         LET v_r_agrup.aivs92_tot  = v_r_agrup.aivs92_dse  + v_r_agrup.aivs92_sep  + v_r_agrup.aivs92_dis  + v_r_agrup.aivs92_mjv  + v_r_agrup.aivs92_mao  + v_r_agrup.aivs92_ucm  + v_r_agrup.aivs92_ral
         LET v_r_agrup.pesos92_tot = v_r_agrup.pesos92_dse + v_r_agrup.pesos92_sep + v_r_agrup.pesos92_dis + v_r_agrup.pesos92_mjv + v_r_agrup.pesos92_mao + v_r_agrup.pesos92_ucm + v_r_agrup.pesos92_ral

         -- se despliegan las descripciones
         DISPLAY v_r_agrup.aivs97_dse,
                 v_r_agrup.aivs97_sep,
                 v_r_agrup.aivs97_dis,
                 v_r_agrup.aivs97_mjv,
                 v_r_agrup.aivs97_mao,
                 v_r_agrup.aivs97_ucm,
                 v_r_agrup.aivs97_ral,
                 v_r_agrup.pesos97_dse,
                 v_r_agrup.pesos97_sep,
                 v_r_agrup.pesos97_dis,
                 v_r_agrup.pesos97_mjv,
                 v_r_agrup.pesos97_mao,
                 v_r_agrup.pesos97_ucm,
                 v_r_agrup.pesos97_ral,
                 v_r_agrup.aivs92_dse,
                 v_r_agrup.aivs92_sep,
                 v_r_agrup.aivs92_dis,
                 v_r_agrup.aivs92_mjv,
                 v_r_agrup.aivs92_mao,
                 v_r_agrup.aivs92_ucm,
                 v_r_agrup.aivs92_ral,
                 v_r_agrup.pesos92_dse,
                 v_r_agrup.pesos92_sep,
                 v_r_agrup.pesos92_dis,
                 v_r_agrup.pesos92_mjv,
                 v_r_agrup.pesos92_mao,
                 v_r_agrup.pesos92_ucm,
                 v_r_agrup.pesos92_ral,
                 v_r_agrup.aivs97_tot,
                 v_r_agrup.pesos97_tot,
                 v_r_agrup.aivs92_tot,
                 v_r_agrup.pesos92_tot
              TO txt_aivs97_dse,
                 txt_aivs97_sep,
                 txt_aivs97_dis,
                 txt_aivs97_mjv,
                 txt_aivs97_mao,
                 txt_aivs97_ucm,
                 txt_aivs97_ral,
                 txt_pesos97_dse,
                 txt_pesos97_sep,
                 txt_pesos97_dis,
                 txt_pesos97_mjv,
                 txt_pesos97_mao,
                 txt_pesos97_ucm,
                 txt_pesos97_ral,
                 txt_aivs92_dse,
                 txt_aivs92_sep,
                 txt_aivs92_dis,
                 txt_aivs92_mjv,
                 txt_aivs92_mao,
                 txt_aivs92_ucm,
                 txt_aivs92_ral,
                 txt_pesos92_dse,
                 txt_pesos92_sep,
                 txt_pesos92_dis,
                 txt_pesos92_mjv,
                 txt_pesos92_mao,
                 txt_pesos92_ucm,
                 txt_pesos92_ral,
                 txt_aivs97_tot,
                 txt_pesos97_tot,
                 txt_aivs92_tot,
                 txt_pesos92_tot

         -- se valida que el folio ingresado exista
         LET v_s_qryTxt = " SELECT COUNT(*)\n",
                          "   FROM dse_devolucion\n",
                          "  WHERE tpo_transferencia = '",v_c_tpo_transf,"'\n",
                          "    AND estado = 10"

         PREPARE prp_cuenta_dse_devol FROM v_s_qryTxt
         EXECUTE prp_cuenta_dse_devol INTO v_i_num_regs

         -- se valida el número de registros
         IF v_i_num_regs = 0 THEN
            CALL fn_mensaje("Atención","No se encontró información para agrupar","stop")
            EXIT MENU
         END IF

      ON ACTION ACCEPT
         -- se invoca la ejecución del stored procedure
         CALL fn_dse_ejecuta_agrupacion(p_usuario_cod)

         EXIT MENU

      ON ACTION CANCEL
         EXIT MENU

   END MENU
   CLOSE WINDOW w_agrupacion

END MAIN

{
======================================================================
Clave: 
Nombre: fn_dse_ejecuta_agrupacion
Fecha creacion: Febrero 04, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta la agrupacion de una DSE devoluciones
para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_dse_ejecuta_agrupacion(p_usuario_cod)

   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod -- usuario que ejecuta el programa
   DEFINE v_folio                   LIKE glo_folio.folio -- folio para preliquidar
   DEFINE v_s_comando               STRING -- cadena con una instruccion de consola
   DEFINE v_s_mensaje               STRING -- mensaje a mostrar a usuario
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta de listados de bat
   DEFINE v_nombre_archivo          LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   DEFINE v_b_resultado             SMALLINT -- resultado de validar la operacion
   DEFINE v_s_qryTxt                STRING -- contiene una sentencia sql a ejecutar

   -- se inicializan variables
   LET v_nombre_archivo = "N/A" -- nombre del archivo
   LET v_folio = 0 -- folio del proceso

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat2 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat2 INTO v_c_ruta_list_bat

   -- se genera PID del proceso
   LET g_pid = fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)

   -- se inicia el proceso
   LET v_b_resultado = fn_inicializa_proceso(g_pid,
                                             g_proceso_cod,
                                             g_opera_cod,
                                             v_folio,
                                             "ACRL43",
                                             v_nombre_archivo,
                                             p_usuario_cod)

   IF v_b_resultado <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_b_resultado)

      RETURN
   END IF

   -- Inicio operacion.
   LET v_b_resultado = fn_actualiza_opera_ini(g_pid,
                                              g_proceso_cod,
                                              g_opera_cod,
                                              v_folio,
                                              "ACRL43",
                                              v_nombre_archivo,
                                              p_usuario_cod)

   IF v_b_resultado <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_b_resultado)

      RETURN
   END IF

   -- se invoca la ejecución del programa lanzado, los parámetros se envían 
   -- en orden según lo acordado el dia 12/Enero/2012 con equipo EFP
   -- usuario, pid, proceso_cod, opera_cod, folio y archivo

   LET v_s_comando = " nohup time fglrun ",g_ruta_bin CLIPPED,"/ACRP38 ",
                                           p_usuario_cod CLIPPED, " ",
                                           g_pid  , " " ,
                                           g_proceso_cod , " " ,
                                           g_opera_cod ," ",
                                           v_folio ," ",
                                           v_nombre_archivo CLIPPED," ",
                                           " 1>", v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",g_pid  USING "&&&&&",":",
                                           g_proceso_cod USING "&&&&&",":",
                                           g_opera_cod   USING "&&&&&" ,
                                           " 2>&1 &"

   DISPLAY v_s_comando
   -- se ejecuta el comando
   RUN v_s_comando

   -- se asigna el mensaje a mostrar
   LET v_s_mensaje = "Se ha enviado la agrupación con PID: ",g_pid,"\n",
                     "Puede revisar el avance del proceso en el monitor de ejecución de procesos"

   CALL fn_mensaje("Atención",v_s_mensaje,"information")

END FUNCTION

#Objetivo: Se obtienen el total de AIVS97, Pesos97, AIVS92 y Pesos92 a agrupar
#          para el módulo que entra como parámetro
FUNCTION fn_obt_agrup_modulo(p_c_modulo_cod, p_c_tpo_transf)

   DEFINE p_c_modulo_cod            LIKE seg_modulo.modulo_cod -- codigo del módulo
   DEFINE p_c_tpo_transf            LIKE dse_devolucion.tpo_transferencia -- tipo de transferencia
   DEFINE v_d_aivs97                DECIMAL(22,2) -- avis 97
   DEFINE v_d_pesos97               DECIMAL(22,2) -- pesos 97
   DEFINE v_d_aivs92                DECIMAL(22,2) -- avis 92
   DEFINE v_d_pesos92               DECIMAL(22,2) -- pesos 92
   DEFINE v_s_qryTxt                STRING

   -- se obtienen los totales a agrupar para el módulo en proceso
   LET v_s_qryTxt = " SELECT SUM(monto_aivs), SUM(monto_pesos)\n",
                    "   FROM dse_devolucion\n",
                    "  WHERE tpo_transferencia = '",p_c_tpo_transf,"'",
                    "    AND estado = 10\n",
                    "    AND modulo_cod = '",p_c_modulo_cod,"'\n",
                    "    AND subcuenta IN (4,44)"

   PREPARE prp_totales_97 FROM v_s_qryTxt
   EXECUTE prp_totales_97 INTO v_d_aivs97, v_d_pesos97

   -- se validan los aivs 97
   IF v_d_aivs97 IS NULL THEN
      LET v_d_aivs97 = 0
   END IF

   -- se validan los pesos 97
   IF v_d_pesos97 IS NULL THEN
      LET v_d_pesos97 = 0
   END IF

   -- se obtienen los totales a agrupar para el módulo en proceso
   LET v_s_qryTxt = " SELECT SUM(monto_aivs), SUM(monto_pesos)\n",
                    "   FROM dse_devolucion\n",
                    "  WHERE tpo_transferencia = '",p_c_tpo_transf,"'",
                    "    AND estado = 10\n",
                    "    AND modulo_cod = '",p_c_modulo_cod,"'\n",
                    "    AND subcuenta IN (8,42)"

   PREPARE prp_totales_92 FROM v_s_qryTxt
   EXECUTE prp_totales_92 INTO v_d_aivs92, v_d_pesos92

   -- se validan los aivs 92
   IF v_d_aivs92 IS NULL THEN
      LET v_d_aivs92 = 0
   END IF

   -- se validan los pesos 92
   IF v_d_pesos92 IS NULL THEN
      LET v_d_pesos92 = 0
   END IF

   RETURN v_d_aivs97, v_d_pesos97, v_d_aivs92, v_d_pesos92

END FUNCTION
