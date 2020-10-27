--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRL42                                        #
#Objetivo          =>Programa que se ejecuta de manera automatica  #
#                    (batch) ejecuta el programa general de        #
#                    validación de carga y la integración del arch.#
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>05 Junio 2013                                 #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

#Objetivo: Programa que permite la validación y carga del archivo de Solicitud de
#          Saldo mediante la funcion general de carga
MAIN
   DEFINE p_b_tipo_carga    SMALLINT, -- tipo de carga (1 - modo en linea y 2 - modo batch)
          p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
          p_d_pid           DECIMAL(9,0), -- identificador del proceso
          p_i_proceso_cod   LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          p_i_opera_cod     LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          p_v_nom_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          p_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_d_folio         LIKE bat_ctr_proceso.folio, -- folio del proceso
          v_si_cnt_valor    SMALLINT, -- variable que indica si existe el precio de acción
          v_c_ruta_bin_agr  LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo agr
          v_c_ruta_bin_glo  LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo glo
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_mensaje       STRING, -- mensaje a mostrar al usuario
          v_s_comando_p     STRING, -- contiene el comando que se envia como parámetro
          v_s_comando_x     STRING, -- contiene el comando a ejecutar
          v_s_qryTxt        STRING, -- guarda una sentencia SQL a ejecutar
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_v_nom_archivo  = ARG_VAL(5)
   LET p_c_programa_cod = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".AGRL42.log")

   -- se inicializan variables del modulo
   LET v_d_folio = 0 -- para la carga no se envia folio
   LET p_b_tipo_carga = 0 -- Batch
   LET p_c_programa_cod = "AGRL42" -- nombre del programa

   -- se valida si llegó nombre del archivo
   IF p_v_nom_archivo IS NULL THEN
      -- se desconoce el nombre del archivo por lo tanto se envia No Aplica
      LET p_v_nom_archivo = "N/A"
   END IF

   DISPLAY " NOMBRE ARCHIVO: ",p_v_nom_archivo

   -- se obtiene la rutas dónde se guarda los programas del módulo AGR
   LET v_s_qryTxt = " SELECT ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_rutaBin_agr FROM v_s_qryTxt
   EXECUTE prp_slc_rutaBin_agr INTO v_c_ruta_bin_agr

   -- se obtiene la rutas dónde se guarda los programas del módulo GLO
   LET v_s_qryTxt = " SELECT ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'glo'"

   PREPARE prp_slc_rutaBin_glo FROM v_s_qryTxt
   EXECUTE prp_slc_rutaBin_glo INTO v_c_ruta_bin_glo

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se crea el comando que ejecuta el proceso que reliza la insercion del registro de control
   LET v_s_comando_p = " 'fglrun ",v_c_ruta_bin_agr CLIPPED,"/AGRP28 ",
                       p_v_usuario, " ",
                       p_d_pid, " ",
                       p_i_proceso_cod, " ",
                       p_i_opera_cod, " ",
                       v_d_folio, " ",
                       p_v_nom_archivo, "'"

   DISPLAY " EJECUTA FUNCIÓN GENERAL DE CARGA"
   -- se crea el comando a ejecutar (programa general de carga)
   LET v_s_comando_x = " fglrun ",v_c_ruta_bin_glo CLIPPED,"/GLOE02 ",
                       p_v_usuario, " ",
                       p_d_pid, " ",
                       p_i_proceso_cod, " ",
                       p_i_opera_cod, " ",
                       p_v_nom_archivo, " ",
                       v_s_comando_p, " 1> ",
                       v_c_ruta_list_bat CLIPPED,
                       "/nohup:",
                       p_d_pid USING "&&&&&",":",
                       p_i_proceso_cod USING "&&&&&",":",
                       p_i_opera_cod USING "&&&&&", " 2>&1"

   RUN v_s_comando_x

   -- se incrementa la operación
   LET p_i_opera_cod = p_i_opera_cod + 1

   -- se verifica que exista precio de la acción para el dia de hoy
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11\n",
                    "    AND f_valuacion = TODAY"

   PREPARE prp_slct_precioAcc FROM v_s_qryTxt
   EXECUTE prp_slct_precioAcc INTO v_si_cnt_valor

   IF v_si_cnt_valor = 0 THEN
      DISPLAY " No existe el precio de acción para el día de hoy por lo que"
      DISPLAY " no es posible continuar con la integración del archivo"

      EXIT PROGRAM
   END IF

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_d_pid, p_i_proceso_cod, p_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_d_pid, p_i_proceso_cod, p_i_opera_cod,
                                           v_d_folio, p_c_programa_cod,
                                           p_v_nom_archivo, p_v_usuario)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se crea el comando que ejecuta el modulo que reliza la integracion del archivo
   LET v_s_comando_x = " nohup time fglrun ",v_c_ruta_bin_agr CLIPPED,"/AGRE12 ",
                       p_v_usuario, " ",
                       p_d_pid, " ",
                       p_i_proceso_cod, " ",
                       p_i_opera_cod, " ",
                       v_d_folio, " ",
                       p_v_nom_archivo, " 1> ",
                       v_c_ruta_list_bat CLIPPED,
                       "/nohup:",p_d_pid USING "&&&&&",":",
                       p_i_proceso_cod USING "&&&&&",":",
                       p_i_opera_cod USING "&&&&&",
                       " 2>&1 &"

   --DISPLAY v_s_comando_x
   RUN v_s_comando_x

   -- se asigna el mensaje a mostrar al usuario
   DISPLAY " Se ha enviado la integración con PID: ",p_d_pid
   DISPLAY " Puede revisar el avance del proceso en el monitor de ejecución de procesos"
END MAIN
