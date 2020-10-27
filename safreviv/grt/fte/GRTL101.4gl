##################################################################################
#Modulo             => GRT                                                       #
#Programa           => GRTL101                                                   #
#Objetivo           => Programa que realiza la carga de recepción de información #
#                      de entidades financieras del proyecto de otorgamiento     #
#                      de créditos en garantía 43 bis(Apoyo Infonavit).          #
#Autor              => Héctor F. Jiménez Lara                                    #
#Fecha inicio       => 08 Septiembre del 2015                                    #
##################################################################################
DATABASE safre_viv

   DEFINE v_s_qryTxt        STRING                                -- guarda una sentencia SQL a ejecutar

MAIN
   DEFINE p_v_usuario       LIKE seg_usuario.usuario              -- usuario firmado al sistema
   DEFINE p_b_tipo_carga    SMALLINT                              -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_nom_prog      VARCHAR(30)                           -- nombre del programa
   DEFINE v_d_pid           DECIMAL(9,0)                          -- identificador del proceso
   DEFINE v_i_proceso_cod   LIKE cat_proceso.proceso_cod          -- proceso que llama las funciones
   DEFINE v_i_opera_cod     LIKE cat_operacion.opera_cod          -- operación que llama la funcion
   DEFINE v_v_nom_archivo   LIKE glo_ctr_archivo.nombre_archivo   -- nombre del archivo en proceso
   DEFINE v_d_folio         LIKE bat_ctr_proceso.folio            -- folio del proceso
   DEFINE v_c_ruta_list_bat LIKE seg_modulo.ruta_listados         -- ruta listados de bat
   DEFINE v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod   -- nombre del programa
   DEFINE v_b_continua      SMALLINT                              -- booleana que indica si se debe continuar con el proceso o no
   DEFINE v_s_comando       STRING                                -- contiene al comando a correr
   DEFINE r_b_valida        SMALLINT                              -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_c_ruta_bin      LIKE seg_modulo.ruta_bin              -- ruta bin del módulo
   DEFINE r_c_ruta_listados LIKE seg_modulo.ruta_listados         -- ruta listados del módulo
   DEFINE v_estado          SMALLINT

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".GRTL101.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- Se invoca a la función que valida que no existan integraciones pendientes
   CALL fn_valida_fin_integracion()

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se inicializan variables del modulo
   LET v_d_pid          = 0
   LET v_i_proceso_cod  = 3901
   LET v_i_opera_cod    = 1                          -- valida archivo recurrente
   LET v_d_folio        = 0
   LET p_b_tipo_carga   = 2
   LET v_v_nom_archivo  = NULL
   LET v_c_programa_cod = "GRTL101"

   DISPLAY "p_v_usuario   : ",p_v_usuario
   DISPLAY "p_b_tipo_carga: ",p_b_tipo_carga
   DISPLAY "p_v_nom_prog  : ",p_v_nom_prog

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- si la operacion es valida se ejecuta
   IF r_b_valida = 0 THEN

      -- se desconoce el nombre del archivo por lo tanto se envia No Aplica
      LET v_v_nom_archivo = "N/A"

      -- se crea el comando que ejecuta el modulo que reliza la insercion del registro de control
      --LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/GRTP101 ",
      LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/GRTE101 ",
                                   p_v_usuario     , " ",
                                   v_d_pid         , " ",
                                   v_i_proceso_cod , " ",
                                   v_i_opera_cod   , " ",
                                   v_d_folio       , " ",
                                   v_v_nom_archivo

      CALL fn_carga_archivo(v_d_pid, v_i_proceso_cod, v_i_opera_cod, p_b_tipo_carga, v_c_programa_cod, v_s_comando, p_v_usuario, TRUE) RETURNING v_b_continua
   ELSE 
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_muestra_inc_operacion(r_b_valida)

      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_d_pid, v_i_proceso_cod, v_i_opera_cod)  RETURNING v_estado
   END IF

END MAIN

# Objetivo:
#          Validar que no se permita la valicación cuando no se han terminado de integrar los 4 subprocesos
FUNCTION fn_valida_fin_integracion()
   DEFINE v_cnt_reg           SMALLINT
   DEFINE v_id_grt_ctr        SMALLINT

   LET v_s_qryTxt = " SELECT COUNT(*)
                        FROM grt_ctr_proceso
                       WHERE fin_sp1 = 1
                         AND fin_sp2 = 1
                         AND fin_sp3 = 1
                         AND fin_sp4 = 1
                         AND fin_sp5 = 1 "

   PREPARE prp_cuenta_inte FROM v_s_qryTxt
   EXECUTE prp_cuenta_inte INTO v_cnt_reg

   DISPLAY "REGISTROS : " , v_cnt_reg
   -- Si se encuentran registros
   IF v_cnt_reg <> 0 THEN
      CALL fn_mensaje("Alerta","No puede ejecutar una validación antes de que terminen de ejecutarse los 5 subprocesos correspondientes","stop")
      EXIT PROGRAM
   END IF
END FUNCTION