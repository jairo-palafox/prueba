--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>ACR                                                #
#Programa          =>ACRP19                                             #
#Objetivo          =>Programa procedimiento que ejecuta el store        #
#                    procedure de saldos remanentes                     #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>28 ENERO 2012                                      #
#########################################################################

DATABASE safre_viv

MAIN
   DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid             LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio           LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_arch_proceso    VARCHAR(100), -- nombre del archivo
          v_d_monto_validar   DECIMAL(7,2), -- monto a validar
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          r_b_cod_error       SMALLINT, -- contiene el status de error del proceso
          r_isam_err          INTEGER, -- contiene el isam error
          r_c_msj             VARCHAR(250) -- contiene el mensaje de error

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRP19.log")

   DISPLAY "=INICIA ACRP19="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " MONTO VALIDAR : ",p_d_folio / 100

   -- se asigna el "folio" a la variable del monto y se regresa el valor del folio
   LET v_d_monto_validar = p_d_folio / 100
   LET p_d_folio = 0

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tbl_saldo_rem()

   DISPLAY " EJECUTA SALDOS REMANENTES"
   -- se invoca la funcion que ejecuta el procedimiento de saldos remanentes
   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:fn_acr_saldos_remanentes(?,?)"

   PREPARE prp_procd_saldos_reman FROM v_s_qryTxt
   EXECUTE prp_procd_saldos_reman USING p_v_usuario, v_d_monto_validar
                                   INTO r_b_cod_error,
                                        r_isam_err,
                                        r_c_msj

   -- se verifica si fue posible finalizar la operacion
   IF r_b_cod_error <> 0 THEN
      DISPLAY " Ocurrió un error durante el proceso de Saldos Remanentes: ",r_b_cod_error
      DISPLAY " ISAM ERR   : ",r_isam_err
      DISPLAY " MENSAJE ERR: ",r_c_msj

      -- En caso de error se marca el proceso como Erroneo
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible marcar como erroneo el proceso y operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF
   DISPLAY "=FIN="
END MAIN

# Objetivo: Función que crea la tabla temporal de saldos remanentes
FUNCTION fn_crea_tbl_saldo_rem()
   DEFINE v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   -- se crea la sentencia sql que elimina la tabla temporal
   LET v_s_qryTxt = " DROP TABLE IF EXISTS tmp_saldo_rem;"

   PREPARE prp_drop_tmp_sdo_rem FROM v_s_qryTxt
   EXECUTE prp_drop_tmp_sdo_rem

   -- se crea la sentencia sql que crea la tabla saldos rem
   LET v_s_qryTxt = " CREATE TABLE tmp_saldo_rem\n",
                    "  (id_cre_acreditado  DECIMAL(9,0),\n",
                    "   id_cre_ctr_archivo DECIMAL(9,0),\n",
                    "   id_derechohabiente DECIMAL(9,0),\n",
                    "   estado             SMALLINT,\n",
                    "   edo_procesar       SMALLINT\n",
                    "  );"

   PREPARE prp_create_tmp_sdo_rem FROM v_s_qryTxt
   EXECUTE prp_create_tmp_sdo_rem

   -- regresa a la base de datos safre viv
   DATABASE safre_viv
END FUNCTION
