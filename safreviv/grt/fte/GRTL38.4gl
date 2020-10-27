--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#################################################################
#Modulo       => GRT                                            #
#Programa     => GRTL38                                         #
#Objetivo     => Programa que ejecuta el stored procedure que   #
#                realiza la agrupacion de DSE para el módulo de #
#                Créditos en Garantía 43 bis                    #
#Autor        => Daniel Buendia, EFP                            #
#Fecha inicio => 30 Mayo 2012                                   #
#################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

DEFINE g_pid           LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod   LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod     LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_ruta_bin      CHAR(40),
       g_ruta_listados CHAR(40)

MAIN
   DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_tipo_ejecucion  SMALLINT, -- forma como ejecutara el programa
          p_s_titulo        STRING, -- titulo de la ventana
          v_i_num_regs      INTEGER, -- contiene el numero de registro encontrados
          v_proceso_desc    STRING, -- descripcion del proceso
          v_opera_desc      STRING, -- descripcion de la operacion
          v_s_qryTxt        STRING, -- contiene una sentencia sql a ejecutar
          v_b_respuesta     BOOLEAN, -- booleana que indica la confirmación de usuario
          v_s_mensaje       STRING, -- mensaje a mostrar a usuario
          v_si_precio_fondo LIKE glo_valor_fondo.precio_fondo, -- precio fondo de acción
          v_c_tpo_transf    LIKE dse_devolucion.tpo_transferencia, -- tipo de transferencia
          v_r_agrup         RECORD 
             aivs97_dse     DECIMAL(22,6), -- AIVS 97 DSE
             aivs97_sep     DECIMAL(22,6), -- AIVS 97 SEP
             aivs97_dis     DECIMAL(22,6), -- AIVS 97 DIS
             pesos97_dse    DECIMAL(22,2), -- Pesos 97 DSE
             pesos97_sep    DECIMAL(22,2), -- Pesos 97 SEP
             pesos97_dis    DECIMAL(22,2), -- Pesos 97 DIS
             aivs92_dse     DECIMAL(22,6), -- AIVS 92 DSE
             aivs92_sep     DECIMAL(22,6), -- AIVS 92 SEP
             aivs92_dis     DECIMAL(22,6), -- AIVS 92 DIS
             pesos92_dse    DECIMAL(22,2), -- Pesos 92 DSE
             pesos92_sep    DECIMAL(22,2), -- Pesos 92 SEP
             pesos92_dis    DECIMAL(22,2), -- Pesos 92 DIS
             aivs97_tot     DECIMAL(22,6), -- Total AIVS 97
             pesos97_tot    DECIMAL(22,2), -- Total Pesos 97
             aivs92_tot     DECIMAL(22,6), -- Total AIVS 92
             pesos92_tot    DECIMAL(22,2) -- Total Pesos 92
          END RECORD,
          r_b_resultado     SMALLINT -- resultado de validar la operacion

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se crea el archivo log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".GRTL38.log")
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proc_cod_grt_agrupacion_dse -- agrupación registros con devolución grt
   LET g_opera_cod   = 1 -- agrupa registros con devolución grt
   LET v_c_tpo_transf = "19"

   -- se obtienen las rutas de control del modulo
   CALL fn_rutas("grt") RETURNING g_ruta_bin, g_ruta_listados

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
      CALL fn_mensaje("Atención","No es posible continuar con la agrupación ya que no existe\nprecio de fondo de inversión para la fecha actual","stop")

      EXIT PROGRAM
   END IF

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_agrupacion WITH FORM "GRTL381"
   MENU
      BEFORE MENU
         -- se obtienen los totales para DSE
         CALL fn_obt_agrup_modulo("grt", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_dse, v_r_agrup.pesos97_dse, v_r_agrup.aivs92_dse, v_r_agrup.pesos92_dse

         -- se obtienen los totales para DSE
         CALL fn_obt_agrup_modulo("sep", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_sep, v_r_agrup.pesos97_sep, v_r_agrup.aivs92_sep, v_r_agrup.pesos92_sep

         -- se obtienen los totales para DSE
         CALL fn_obt_agrup_modulo("dis", v_c_tpo_transf) RETURNING v_r_agrup.aivs97_dis, v_r_agrup.pesos97_dis, v_r_agrup.aivs92_dis, v_r_agrup.pesos92_dis

         -- se calculan los totales
         LET v_r_agrup.aivs97_tot = v_r_agrup.aivs97_dse + v_r_agrup.aivs97_sep + v_r_agrup.aivs97_dis
         LET v_r_agrup.pesos97_tot = v_r_agrup.pesos97_dse + v_r_agrup.pesos97_sep + v_r_agrup.pesos97_dis
         LET v_r_agrup.aivs92_tot = v_r_agrup.aivs92_dse + v_r_agrup.aivs92_sep + v_r_agrup.aivs92_dis
         LET v_r_agrup.pesos92_tot = v_r_agrup.pesos92_dse + v_r_agrup.pesos92_sep + v_r_agrup.pesos92_dis

         -- se despliegan las descripciones
         DISPLAY v_r_agrup.aivs97_dse,
                 v_r_agrup.aivs97_sep,
                 v_r_agrup.aivs97_dis,
                 v_r_agrup.pesos97_dse,
                 v_r_agrup.pesos97_sep,
                 v_r_agrup.pesos97_dis,
                 v_r_agrup.aivs92_dse,
                 v_r_agrup.aivs92_sep,
                 v_r_agrup.aivs92_dis,
                 v_r_agrup.pesos92_dse,
                 v_r_agrup.pesos92_sep,
                 v_r_agrup.pesos92_dis,
                 v_r_agrup.aivs97_tot,
                 v_r_agrup.pesos97_tot,
                 v_r_agrup.aivs92_tot,
                 v_r_agrup.pesos92_tot
              TO txt_aivs97_dse,
                 txt_aivs97_sep,
                 txt_aivs97_dis,
                 txt_pesos97_dse,
                 txt_pesos97_sep,
                 txt_pesos97_dis,
                 txt_aivs92_dse,
                 txt_aivs92_sep,
                 txt_aivs92_dis,
                 txt_pesos92_dse,
                 txt_pesos92_sep,
                 txt_pesos92_dis,
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
            CALL fn_mensaje("Atención","No se encontro información para agrupar","stop")
            EXIT MENU
         END IF

      ON ACTION ACCEPT
         -- se invoca la ejecucion del stored procedure
         CALL fn_dse_ejecuta_agrupacion(p_usuario_cod)

         EXIT MENU
        
      ON ACTION CANCEL
         EXIT MENU
   
   END MENU
   CLOSE WINDOW w_agrupacion
{
   -- solicita la confirmación de usuario para ejecutar el proceso de agrupación
   -- regresa: 1(confirmar) o 0(cancelar)
   LET v_s_mensaje = "Confirme la ejecución del proceso de Agrupación de\n",
                     "Devolución de Saldos Excedentes GRT"
   CALL fn_ventana_confirma("Agrupación",v_s_mensaje,"quest") RETURNING v_b_respuesta

   -- si se confirmó, se ejecuta el proceso que realiza Saldos remanentes
   IF v_b_respuesta THEN
      -- se invoca la ejecucion del stored procedure
      CALL fn_dse_ejecuta_agrupacion(p_usuario_cod)
   ELSE
      EXIT PROGRAM
   END IF
}
END MAIN

#Objetivo: Ejecuta la agrupacion de una DSE devoluciones para un folio dado
FUNCTION fn_dse_ejecuta_agrupacion(p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_s_mensaje       STRING, -- mensaje a mostrar a usuario
       v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta de listados de bat
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_b_resultado     SMALLINT, -- resultado de validar la operacion
       v_s_qryTxt        STRING -- contiene una sentencia sql a ejecutar

   -- se inicializan variables
   LET v_nombre_archivo = "N/A" -- nombre del archivo
   LET v_folio = 0 -- folio del proceso

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat2 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat2 INTO v_c_ruta_list_bat
{
   -- se ejecuta la función que valida la operación
   LET v_b_resultado = fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)

   -- se verifica si se puede continuar con la operacion
   IF v_b_resultado <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_b_resultado)

      RETURN
   END IF
}
   -- se genera PID
   LET g_pid = fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)

   -- se inicia el proceso
   LET v_b_resultado = fn_inicializa_proceso(g_pid,
                                             g_proceso_cod,
                                             g_opera_cod,
                                             v_folio,
                                             "GRTL38",
                                             v_nombre_archivo,
                                             p_usuario_cod)

   -- se verifica si se pudo inicializar el proceso
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
                                              "GRTL38",
                                              v_nombre_archivo,
                                              p_usuario_cod)

   -- se verifica si se pudo actualzar el estatus de la operacion como PROCESANDO
   IF v_b_resultado <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_b_resultado)

      RETURN
   END IF

   -- se invoca la ejecucion del programa lanzado. los parametros se envian 
   -- en orden segun lo acordado el dia 12/Enero/2012 con equipo EFP
   -- usuario, pid, proceso_cod, opera_cod, folio y archivo
   LET v_s_comando = " nohup time fglrun ",g_ruta_bin CLIPPED,"/GRTP24 ",
                                           p_usuario_cod CLIPPED, " ",
                                           g_pid, " " ,
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
   DEFINE p_c_modulo_cod  LIKE seg_modulo.modulo_cod, -- codigo del módulo
          p_c_tpo_transf    LIKE dse_devolucion.tpo_transferencia, -- tipo de transferencia
          v_d_aivs97      DECIMAL(22,2), -- avis 97
          v_d_pesos97     DECIMAL(22,2), -- pesos 97
          v_d_aivs92      DECIMAL(22,2), -- avis 92
          v_d_pesos92     DECIMAL(22,2), -- pesos 92
          v_s_qryTxt      STRING

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
