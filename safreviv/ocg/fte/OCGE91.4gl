###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGE91                                                  #
#Objetivo          => Programa que inserta registro en la tabla de control    #
#                     de archivos de Recepcion de detalles migración          #
###############################################################################

DATABASE safre_viv

   DEFINE p_v_usuario               LIKE seg_usuario.usuario     -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid     -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operación de la etapa
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio   -- número de folio
   DEFINE p_v_arch_proceso          VARCHAR(100)                 -- nombre del archivo a integrar
   DEFINE v_s_qryTxt                STRING                       -- guarda una sentencia SQL a ejecutar
   DEFINE bnd_general               SMALLINT
   DEFINE v_operacion               SMALLINT

MAIN

   DEFINE v_i_estado_cod            LIKE bat_ctr_proceso.estado_cod    -- estado del proceso
   DEFINE v_i_estado                LIKE cre_ctr_archivo.estado        -- estado del registro de control
   DEFINE v_i_tot_reg_det           LIKE cre_ctr_archivo.tot_registros -- total de registros en detalle
   DEFINE r_si_cod_error            SMALLINT                           -- codigo de error en caso de excepción
   DEFINE r_b_estatus_proc          SMALLINT                           -- estatus de retorno del Store
   DEFINE r_b_valida                SMALLINT                           -- status de registro de las funciones de actualización

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".OCGE91.log")

   DISPLAY "=INICIA OCE91="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- si no viene el nombre del archivo, invocar la función que busca el nombre
   IF p_v_arch_proceso = "NA" OR p_v_arch_proceso = "N/A" THEN
     -- se invoca la función que obtiene el nombre del archivo
     LET p_v_arch_proceso = fn_recupera_arch_cargado(p_i_proceso_cod, p_i_opera_cod)
   END IF

   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   LET v_operacion = 91

   -- Se invoca a la función que no permite validar más de dos archivos mientras se esta integrando los subprocesos
   CALL fn_valida_fin_integracion(p_v_arch_proceso,p_i_proceso_cod)

   CALL fn_valida_estructura(p_v_arch_proceso)

   IF bnd_general = 0 THEN
      DISPLAY "ERROR: NO SE PROCESARON TODOS LOS REGISTROS CORRECTAMENTE"
      DISPLAY "ALGUNOS REGISTROS NO CUMPLIERON CON LAS VALIDACIONES NECESARIAS"
   ELSE
      -- se valida el PID
      IF p_d_pid = 0 THEN
         LET p_d_pid = fn_max_pid(p_i_proceso_cod, p_i_opera_cod)

         -- se valida el PID
         IF p_d_pid = 0 OR p_d_pid IS NULL THEN
            DISPLAY " ERROR: No fue posible obtener el PID"

            EXIT PROGRAM
         END IF
      END IF

      -- se crea la sentencia sql que verifica el estatus del proceso de carga
      LET v_s_qryTxt = " SELECT estado_cod\n",
                       "   FROM bat_ctr_operacion\n",
                       "  WHERE pid         = ",p_d_pid,"\n",
                       "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                       "    AND opera_cod   = ",p_i_opera_cod

      PREPARE prp_slc_estado_cod FROM v_s_qryTxt
      EXECUTE prp_slc_estado_cod INTO v_i_estado_cod

      -- verifica si el estado fue diferente de 'válido'
      IF v_i_estado_cod = 4 THEN
         LET v_i_estado = 10
      ELSE
         DISPLAY " ERROR: NO SE PROCESARON TODOS LOS REGISTROS CORRECTAMENTE"
         LET v_i_estado = 30
      END IF

      -- se comenta temporalmente para pruebas de validación EVB
      -- se invoca la funcion que inserta el registro en la tabla de control de archivos
      LET v_s_qryTxt = "EXECUTE FUNCTION fn_insrt_ocg_ctr_arch_mig43(?,?,?,?)"

      PREPARE prp_procd_ins_cre_ctr_arch FROM v_s_qryTxt
      EXECUTE prp_procd_ins_cre_ctr_arch USING p_v_arch_proceso, v_i_estado, p_v_usuario, v_operacion
                                          INTO r_b_estatus_proc, v_i_tot_reg_det

      -- se verifica si el archivo fue marcado como valido
      IF r_si_cod_error <> 0 THEN
         DISPLAY "ERROR: Ocurrió un error en el proceso: ",r_si_cod_error
         -- se invoca función que finaliza las operaciones del proceso
         CALL fn_finaliza_procesos()

         -- se actualiza al esta a glo ctr archivo como Reversado
         CALL fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, 3, p_v_usuario) RETURNING r_b_valida

         DISPLAY " RETORNO DE LA ACTUALIZACIÓN: " ,r_b_valida
      END IF

      -- se verifica si el archivo fue marcado como valido
      IF r_b_estatus_proc <> 0 OR v_i_estado = 30 THEN
         -- archivo rechazado. muestra mensaje del error
         CALL fn_display(r_b_estatus_proc)

         -- se invoca función que finaliza las operaciones del proceso
         CALL fn_finaliza_procesos()

         -- se actualiza al esta a glo ctr archivo como Reversado
         CALL fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, 3, p_v_usuario) RETURNING r_b_valida

         DISPLAY " RETORNO DE LA ACTUALIZACIÓN: " ,r_b_valida
      ELSE
         DISPLAY " TOTAL DE REGISTROS DETALLE: ",v_i_tot_reg_det
         ---DISPLAY " TOTAL DE REGISTROS SUMARIO:    1"
         DISPLAY " SE PROCESO LA VALIDACIÓN CORRECTAMENTE"
      END IF
     DISPLAY "=FIN="
  END IF
END MAIN

#Objetivo: Función que muestra el display correspondiente al código de error
FUNCTION fn_display(v_b_estatus_proc)
   DEFINE v_b_estatus_proc    SMALLINT

   CASE v_b_estatus_proc
      WHEN 1
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE DETALLE"
      WHEN 2
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE SUMARIO O EXISTE MÁS DE UNO"
      WHEN 3
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE ENCABEZADO O EXISTE MÁS DE UNO"
      WHEN 4
         DISPLAY " ERROR: EL TOTAL EN SUMARIO NO COINCIDE CON EL TOTAL DE REGISTROS EN ARCHIVO"
      WHEN 5
         DISPLAY " ERROR: EL ARCHIVO CONTIENE REGISTROS CON NSS NO CATALOGADOS"
      WHEN 6
         DISPLAY " ERROR: LA OPERACIÓN DEL ARCHIVO NO CORRESPONDE CON EL PROCESO"
      WHEN 7
         DISPLAY " ERROR: EL TIPO DE TRANSFERENCIA ES DIFERENTE AL REQUERIDO"
      WHEN 8
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE FECHA DE PROCESO"
      WHEN 9
         DISPLAY " ERROR: LA FECHA DE TRANSFERENCIA QUE CONTIENE EL ARCHIVO YA FUE PROCESADA"
      WHEN 10 
         DISPLAY " ERROR: EL ARCHIVO CONTIENE MÁS DE UN ENCABEZADO"
      WHEN 11
         DISPLAY " ERROR: EL ARCHIVO CONTIENE MÁS DE UN SUMARIO"
   END CASE

END FUNCTION

#Objetivo: Función que finaliza las operaciones del proceso
FUNCTION fn_finaliza_procesos()
   DEFINE v_i_proceso_cod     LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE v_i_opera_cod       LIKE cat_operacion.opera_cod -- código de la operacion
   DEFINE v_s_qryTxt          STRING -- se asigna una sentencia sql a ejecutar
   DEFINE v_dt_tiempo         DATETIME YEAR TO SECOND -- variable con fecha y hora
   DEFINE r_b_valida          SMALLINT -- status de registro de las funciones de actualización

   -- se asigna la fecha y hora actual
   LET v_dt_tiempo = CURRENT

   -- se actializa la operación como erronea
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET fecha_fin   = '",v_dt_tiempo,"',\n",
                    "        estado_cod  = 3\n",
                    "  WHERE pid         = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod   = ",p_i_opera_cod

   PREPARE prp_act_error_opera FROM v_s_qryTxt
   EXECUTE prp_act_error_opera

   -- se actualiza el proceso como erronea
   LET v_s_qryTxt = " UPDATE bat_ctr_proceso\n",
                    "    SET fecha_fin   = TODAY,\n",
                    "        estado_cod  = 3\n",
                    "  WHERE pid         = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod

   PREPARE prp_act_error_proceso FROM v_s_qryTxt
   EXECUTE prp_act_error_proceso

END FUNCTION

# Objetivo:
#          Validar que no se permita la validación cuando no se han terminado de integrar los 4 subprocesos
FUNCTION fn_valida_fin_integracion(p_nom_arch,p_proceso_cod)
   DEFINE p_nom_arch          CHAR(40)
   DEFINE p_proceso_cod       LIKE bat_ctr_proceso.proceso_cod
   DEFINE v_cnt_reg           SMALLINT
   DEFINE v_id_ocg_ctr        SMALLINT

   SELECT id_ocg_ctr_archivo
     INTO v_id_ocg_ctr
     FROM ocg_ctr_archivo
    WHERE nom_archivo = p_nom_arch

   LET v_s_qryTxt = " SELECT COUNT(*)
                        FROM ocg_ctr_proceso
                       WHERE id_ocg_ctr_archivo = ?
                         AND fin_sp1 = 0
                         AND fin_sp2 = 0
                         AND fin_sp3 = 0
                         AND fin_sp4 = 0
                         AND fin_sp5 = 0 "

   LET p_nom_arch = p_nom_arch CLIPPED

   PREPARE prp_cuenta_inte FROM v_s_qryTxt
   EXECUTE prp_cuenta_inte USING v_id_ocg_ctr
                            INTO v_cnt_reg

   -- Si se encuentran registros
   IF v_cnt_reg > 0 THEN
      CALL fn_mensaje("Alerta","No puede ejecutar una validación antes de que terminen de ejecutarse los 5 subprocesos correspondientes","stop")
      EXIT PROGRAM
   END IF

END FUNCTION


FUNCTION fn_valida_estructura(p_v_arch_proceso)

   DEFINE p_v_arch_proceso          VARCHAR(100)
   DEFINE v_archivo                 STRING
   DEFINE s                         VARCHAR(640)  -- variable para leer lineas del archivo
   DEFINE ch1                       base.Channel
   DEFINE i                         INTEGER
   DEFINE v_cnt_sp1                 INTEGER
   DEFINE v_cnt_sp2                 INTEGER
   DEFINE v_cnt_sp3                 INTEGER
   DEFINE v_cnt_sp5                 INTEGER
   DEFINE v_ruta_rescate            LIKE seg_modulo.ruta_rescate
   DEFINE v_comando                 STRING
   DEFINE v_cta                     INTEGER
   DEFINE v_cta_reg                 STRING
   DEFINE v_id_ocg_ctr              SMALLINT

   LET v_cnt_sp1 = 0
   LET v_cnt_sp2 = 0
   LET v_cnt_sp3 = 0
   LET v_cnt_sp5 = 0

   SELECT ruta_rescate
     INTO v_ruta_rescate
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   SELECT id_ocg_ctr_archivo
     INTO v_id_ocg_ctr
     FROM ocg_ctr_archivo
    WHERE nom_archivo = p_v_arch_proceso

   LET v_archivo = v_ruta_rescate CLIPPED,"/",p_v_arch_proceso
   LET v_cta_reg = v_ruta_rescate CLIPPED,"/","cuenta_registros.txt"

   LET ch1 = base.Channel.create()

   LET v_comando = "cat ",v_archivo,"|wc -l > ",v_cta_reg
   RUN v_comando

   CALL ch1.openFile(v_cta_reg,"r")

   LET v_cta = ch1.readLine()

   CALL ch1.close()

   LET v_comando = "rm ",v_cta_reg
   RUN v_comando

   CALL ch1.openFile(v_archivo,"r")

   LET bnd_general = 1
   LET i = 1
   WHILE TRUE
      LET s = ch1.readLine()

      IF ch1.isEof() THEN
         EXIT WHILE
      END IF

      LET v_cnt_sp1 = v_cnt_sp1 + 1

      --CALL fn_valida(s,i)

      LET i = i + 1
   END WHILE

   LET v_s_qryTxt = " UPDATE ocg_ctr_archivo
                         SET tot_sp1 = ? ,
                             tot_sp2 = ? ,
                             tot_sp3 = ? ,
                             tot_sp5 = ? 
                      WHERE id_ocg_ctr_Archivo = ? "

   IF SQLCA.SQLCODE < 0 THEN
      CALL fn_mensaje("Error","Ocurrió un error al insertar los totales\n
                               en la tabla de control de archivos", "stop")
   END IF 

   CALL ch1.close()
END FUNCTION

