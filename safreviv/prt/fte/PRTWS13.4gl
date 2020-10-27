--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20/11/2015
--==============================================================================
################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS13                                                       #
#Objetivo     => Solicitud de Cancelacion de Portabilidad                      #
#Fecha inicio => 20 Noviembre 2015                                             #
################################################################################

SCHEMA "safre_viv"

DEFINE r_respuesta RECORD
v_nss           CHAR(11),
v_noCaso        CHAR(10),
v_apPaterno     CHAR(40),
v_apMaterno     CHAR(40),
v_nombre        CHAR(40),
v_estatus_c     SMALLINT,
v_diagnostico   SMALLINT
END RECORD 
DEFINE v_id_solicitud DECIMAL(9,0)
DEFINE v_estado       SMALLINT

FUNCTION fn_genera_diagnostico_cancelacion(p_noCaso)

   DEFINE p_noCaso CHAR(10) 
   DEFINE v_query  STRING
   DEFINE maq_estado RECORD
      v_ind             SMALLINT,
      v_diag            CHAR(3),
      v_sql_error       INTEGER,
      v_isam_error      INTEGER,
      v_msg_error       CHAR(100),
      v_estado_destino  SMALLINT
   END RECORD

   WHENEVER SQLERROR CALL fn_error_consulta

   INITIALIZE r_respuesta.*  TO NULL
   INITIALIZE v_id_solicitud TO NULL
   INITIALIZE v_estado       TO NULL
   
   --Determinando diagnostico de la cancelacion
   --Si la solicitud no existe es diagnóstico 2
   SELECT nss,
          n_caso,
          paterno,
          materno,
          nombre,
          estado,
          id_prt_solicitud_cedente
      INTO r_respuesta.v_nss,
           r_respuesta.v_noCaso,
           r_respuesta.v_apPaterno,
           r_respuesta.v_apMaterno,
           r_respuesta.v_nombre,
           v_estado,
           v_id_solicitud
      FROM prt_solicitud_cedente
      WHERE n_caso = p_noCaso
   --- Si el registro no existe o es menor que 15 -> DIAGNOSTICO 2
   IF SQLCA.sqlcode == 100 THEN
      LET r_respuesta.v_diagnostico = 2
      LET r_respuesta.v_estatus_c = 2
      LET r_respuesta.v_noCaso = p_noCaso
      CALL fn_inserta_historico()
      RETURN r_respuesta.*
   END IF
   IF v_estado < 15 THEN
      INITIALIZE r_respuesta.* TO NULL
      LET r_respuesta.v_diagnostico = 2
      LET r_respuesta.v_estatus_c = 2
      LET r_respuesta.v_noCaso = p_noCaso
      CALL fn_inserta_historico()
      RETURN r_respuesta.*
   END IF

   -- ESTADO = 15 -> DIAGNOSTICO 1
   IF v_estado == 15 THEN
      LET r_respuesta.v_diagnostico = 1
      LET r_respuesta.v_estatus_c = 1
      CALL fn_inserta_historico()

      --Cancelacion Exitosa Cambia Estado A 16 a traves de la maquinaria, señal 16. Maquinaria 2
      LET v_query = "CALL fn_glo_maq_individual(2,?,16,\"safreviv\")"
      PREPARE prp_qry FROM v_query
      DECLARE cur CURSOR FOR prp_qry
      OPEN cur USING v_id_solicitud
      FETCH cur INTO maq_estado.*
      CLOSE cur
      FREE cur
      FREE prp_qry
      IF maq_estado.v_estado_destino == 16 THEN
         RETURN r_respuesta.*
      ELSE 
         CALL fn_error_consulta()
      END IF

    END IF

   -- estado = 30 -> DIAGNOSTICO 3
   IF v_estado == 30 THEN
      LET r_respuesta.v_diagnostico = 3
      LET r_respuesta.v_estatus_c = 2
      CALL fn_inserta_historico()
      RETURN r_respuesta.*
   END IF

   -- Estado = 35 -> DIAGNOSTICO 5
   IF v_estado == 35 THEN
      LET r_respuesta.v_diagnostico = 5
      LET r_respuesta.v_estatus_c = 2
      CALL fn_inserta_historico()
      RETURN r_respuesta.*
   END IF

   --Si el estado es nulo (Por alguna razon) retorna nulos
   IF v_estado == NULL THEN
      RETURN r_respuesta.*
   END IF

   --Ninguna de las anteriores -> DIAGNOSTICO 4
   LET r_respuesta.v_diagnostico = 4
   LET r_respuesta.v_estatus_c = 2
   CALL fn_inserta_historico()
   RETURN r_respuesta.*

END FUNCTION

FUNCTION fn_inserta_historico()

   INSERT INTO prt_cancelacion_crm 
      VALUES(seq_prt_cancelacion_crm.NEXTVAL,
             v_id_solicitud,
             r_respuesta.v_nss,
             r_respuesta.v_noCaso,
             TODAY,
             r_respuesta.v_estatus_c,
             r_respuesta.v_diagnostico)

END FUNCTION

FUNCTION fn_error_consulta()

   INITIALIZE r_respuesta.* TO NULL
   DISPLAY "SE HA PRESENTADO EL SIGUIENTE ERROR AL CONSULTAR LA BASE DE DATOS: "
   DISPLAY "-> ",SQLCA.sqlcode," : ",SQLCA.sqlerrm
   EXIT PROGRAM
   
END FUNCTION