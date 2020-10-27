--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRR05                                        #
#Objetivo          =>Programa que ejecuta los reversos de          #
#                    devoluciones (preliquidacion, liquidacion y   #
#                    agrupaciones) para el módulo de DSE           #
#                    Anualidades Garantizadas                      #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>07 Junio 2012                                 #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

MAIN
   DEFINE p_d_folio             LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_usuario           LIKE seg_usuario.usuario, -- usuario firmado al sistema
          p_d_pid               DECIMAL(9,0), -- identificador del proceso
          p_i_proceso_cod       LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          p_i_opera_cod         LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          p_c_op_arch_ent       VARCHAR(5), -- contiene la opcion a reversar de Archivos entrada    VARCHAR(100) -- nombre del archivo
          v_si_proceso_cod_rev  LIKE cat_proceso.proceso_cod, -- código de proceso del reverso
          v_si_opera_cod_rev    LIKE cat_proceso.proceso_cod, -- codigo de operación del reverso
          v_d_pid_rev           LIKE bat_ctr_proceso.pid, -- PID
          r_b_valida            SMALLINT -- status que regresa una función externa
          
   # se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_c_op_arch_ent  = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRR05.log")

   DISPLAY "=INICIA AGRR05="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " OPCIÓN        : ",p_c_op_arch_ent

   -- se iniacializan variables
   LET r_b_valida = 0 -- se asume que no ocurrirá error en el reverso

   -- se verifica la opción seleccionada
   CASE p_c_op_arch_ent
      WHEN "opt5" -- Agrupación
         LET v_si_proceso_cod_rev = g_proc_cod_agr_agrupacion_dse -- agrupación registros con devolución agr
         LET v_si_opera_cod_rev   = 1 -- agrupa registros con devolución
         CALL fn_reverso_agrupacion_dseagr(p_d_folio) RETURNING r_b_valida

      WHEN "opt6" -- Preliquidación
         LET v_si_proceso_cod_rev = g_proc_cod_agr_liquida_dse -- liquidación devolución saldos exc agr
         LET v_si_opera_cod_rev   = 1 -- preliquida devolución de saldos
         CALL fn_reverso_preliquida_dseagr(p_d_folio) RETURNING r_b_valida

      WHEN "opt7" -- Liquidación
         LET v_si_proceso_cod_rev = g_proc_cod_agr_liquida_dse -- liquidación devolución saldos exc agr
         LET v_si_opera_cod_rev   = 2 -- liquida devolución de saldos
         CALL fn_reverso_liquida_dseagr(p_d_folio) RETURNING r_b_valida
   END CASE
   
   -- se valida el codigo de error
   IF r_b_valida <> 0 THEN
      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtiene el maximo pid para el proceso y operacion de reverso
   LET v_d_pid_rev = fn_max_pid(v_si_proceso_cod_rev,  v_si_opera_cod_rev)

   -- se ejecuta la funcion que ejecuta el reverso de la operacion
   LET r_b_valida = fn_reversa_operacion(v_d_pid_rev, v_si_proceso_cod_rev, v_si_opera_cod_rev)

   -- se verifica si fue posible reversar la operación
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF

   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que ejecuta el reverso de la agrupación
FUNCTION fn_reverso_agrupacion_dseagr(p_d_folio)
   DEFINE p_d_folio             LIKE glo_ctr_archivo.folio, -- numero de folio
          r_codigo_sql          SMALLINT

   -- se asume que no ocurrirá error en el reverso
   LET r_codigo_sql = 0

   DISPLAY "REVERSO AGRUPACIÓN"
   -- Ejecuta el SP que realiza el reverso
   PREPARE prp_reverso_agrupacion_dse FROM "EXECUTE FUNCTION safre_viv:fn_reversa_agrupacion_dse(?)"
   EXECUTE prp_reverso_agrupacion_dse USING p_d_folio
                                       INTO r_codigo_sql 

   IF(r_codigo_sql <> 0)THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE REVERSO (CÓDIGO): ",r_codigo_sql
   END IF

   RETURN r_codigo_sql
END FUNCTION

#Objetivo: Función que ejecuta el reverso de la preliquidación
FUNCTION fn_reverso_preliquida_dseagr(p_d_folio)
   DEFINE p_d_folio             LIKE glo_ctr_archivo.folio, # número de folio
          r_codigo_sql          SMALLINT

   -- se asume que no ocurrirá error en el reverso
   LET r_codigo_sql = 0

   DISPLAY "REVERSO PRELIQUIDACIÓN"
   -- Ejecuta el SP que realiza el reverso
   PREPARE prp_reverso_preliquidacion_dse FROM "EXECUTE FUNCTION safre_viv:fn_reversa_preliquidacion_dse(?)"
   EXECUTE prp_reverso_preliquidacion_dse USING p_d_folio
                                           INTO r_codigo_sql 

   IF(r_codigo_sql <> 0)THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE REVERSO (CÓDIGO): ",r_codigo_sql
   END IF

   RETURN r_codigo_sql
END FUNCTION

#Objetivo: Función que ejecuta el reverso de la liquidación
FUNCTION fn_reverso_liquida_dseagr(p_d_folio)
   DEFINE p_d_folio        LIKE glo_ctr_archivo.folio, -- numero de folio
          r_sts_rev_cont   SMALLINT, -- estatus del registro contable
          r_codigo_sql     SMALLINT

   -- se asume que no ocurrirá error en el reverso
   LET r_codigo_sql = 0

   DISPLAY " REVERSA REGISTRO CONTABLE"
   -- se invoca la funcion que reversa el proceso de contabilidad
   CALL fn_reverso_reg_cnt(p_d_folio) RETURNING r_sts_rev_cont

   -- se verifica si ocurrió algun error con el reverso
   --IF r_sts_rev_cont = 1 THEN
   IF r_sts_rev_cont > 1 THEN
      DISPLAY " ERROR: No es posible continuar con el reverso debido a que ya fue generada la\n",
              " póliza contable, o bien, la fecha de reverso es posterior a la de liquidación"
      LET r_codigo_sql = r_sts_rev_cont

      RETURN r_codigo_sql
   END IF

   DISPLAY "REVERSO LIQUIDACIÓN"
   -- Ejecuta el SP que realiza el reverso
   PREPARE prp_reverso_liquidacion_dse FROM "EXECUTE FUNCTION safre_viv:fn_reverso_liquidacion_dse(?)"
   EXECUTE prp_reverso_liquidacion_dse USING p_d_folio
                                        INTO r_codigo_sql

   IF(r_codigo_sql <> 0)THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE REVERSO (CÓDIGO): ",r_codigo_sql

      RETURN r_codigo_sql
   END IF

   RETURN r_codigo_sql
END FUNCTION   
