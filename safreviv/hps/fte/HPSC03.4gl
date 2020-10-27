--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-AGO-2015
--===============================================================
####################################################################
#Modulo            =>HPS                                           #
#Programa          =>HPSC03                                        #
#Objetivo          =>Consulta de solicitudes de cancelación de Hipoteca con Servicios #
#Fecha inicio      =>10 de Agosto de 2015                          #
####################################################################
DATABASE safre_viv

DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      STRING,             -- nombre del programa
       p_v_tpo_mdt       SMALLINT,                 -- tipo de mandato, opcional
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin

DEFINE v_arr_solicitud DYNAMIC ARRAY OF RECORD
v_id_solicitud          DECIMAL(9,0),
v_nss                   CHAR(11),
v_nombre                CHAR(40),
v_resultado_operacion   CHAR(2),
v_diagnostico_interno   CHAR(18),
v_estado                SMALLINT
END RECORD

DEFINE v_arr_restitucion DYNAMIC ARRAY OF RECORD
v_atributo CHAR(40),
v_valor    VARCHAR(200)
END RECORD

DEFINE v_condition   STRING
DEFINE v_nss         CHAR(11)
DEFINE v_estado      SMALLINT
DEFINE f_ini_registro   DATE
DEFINE f_fin_registro   DATE
DEFINE f_ini_solicitado DATE
DEFINE f_fin_solicitado DATE
DEFINE f_ini_restituido DATE
DEFINE f_fin_restituido DATE

MAIN
   
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)
   LET p_v_tpo_mdt    = ARG_VAL(4)

   SELECT ruta_bin
    INTO v_ruta_ejecutable
    FROM seg_modulo
   WHERE modulo_cod = "hps" 

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   OPEN WINDOW vtna_consulta WITH FORM v_ruta_ejecutable CLIPPED||"/HPSC031"

      DIALOG ATTRIBUTES(UNBUFFERED)

         --Leyendo datos de entrada
         INPUT BY NAME v_nss,v_estado,f_ini_registro,f_fin_registro,f_ini_solicitado,f_fin_solicitado,f_ini_restituido,f_fin_restituido
         END INPUT
         
         --Despliega las cuentas con solicitud de cancelación
         DISPLAY ARRAY v_arr_solicitud TO Record2.*
            BEFORE ROW
               CALL fn_despliega_restitucion(v_arr_solicitud[ARR_CURR()].v_id_solicitud)
         END DISPLAY

         --Despliega Solicitud restitucion
         DISPLAY ARRAY v_arr_restitucion TO Record3.*
         END DISPLAY

         ON ACTION aceptar
            CALL fn_arma_cadena()
            CALL v_arr_restitucion.CLEAR()
            CALL fn_despliega_cancelacion(v_condition)

         ON ACTION cancelar
            EXIT DIALOG

      END DIALOG

   CLOSE WINDOW vtna_consulta
   
END MAIN

FUNCTION fn_despliega_cancelacion(p_condition)

   DEFINE p_condition STRING
   DEFINE p_query     STRING
   DEFINE i INTEGER

   CALL v_arr_solicitud.CLEAR()

   DISPLAY p_condition
   LET p_query = "SELECT a.id_hps_solicitud_cancelacion,
                         a.nss,
                         a.nombre,
                         a.resultado_operacion,
                         a.diagnostico_interno,
                         a.estado 
                         FROM hps_solicitud_cancelacion a, hps_restitucion b WHERE a.id_hps_solicitud_cancelacion = b.id_hps_solicitud_cancelacion AND ",p_condition
   PREPARE prp_cancela FROM p_query
   DECLARE cur_cancela CURSOR FOR prp_cancela

   LET i=1
   FOREACH cur_cancela INTO v_arr_solicitud[i].*
      LET i = i+1
   END FOREACH

   CALL v_arr_solicitud.deleteElement(i)

END FUNCTION


FUNCTION fn_despliega_restitucion(p_id_solicitud)

   DEFINE p_id_solicitud DECIMAL(9,0)
   DEFINE p_query_r      STRING 
   DEFINE j              INTEGER
   DEFINE p_entidad      CHAR(40)
   DEFINE p_id_registro_entidad DECIMAL(9,0)

   --RESPUESTAS de la funcion que no se guardan en el record
   DEFINE v_ind SMALLINT
   DEFINE v_diag                CHAR(3)    -- diagnostico de error
   DEFINE v_sql_error           INTEGER 
   DEFINE v_isam_error          INTEGER
   DEFINE v_msg_error           CHAR(100)

   CALL v_arr_restitucion.CLEAR()

   --Obteniendo los datos con los que se llamará a la función
   LET p_entidad = "hps_restitucion"
   SELECT id_hps_restitucion 
      INTO p_id_registro_entidad
      FROM hps_restitucion
      WHERE id_hps_restitucion = p_id_solicitud
      
   --Ejecutando Query
   LET p_query_r = "CALL fn_glo_recupera_etiquetas(?,?)"
   PREPARE prp_restitucion FROM p_query_r
   DECLARE cur_restitucion CURSOR FOR prp_restitucion
   
   LET j = 1 
   FOREACH cur_restitucion USING p_entidad,p_id_registro_entidad INTO v_ind,v_diag,v_sql_error,v_isam_error,v_msg_error,v_arr_restitucion[j].v_atributo,v_arr_restitucion[j].v_valor
      LET j = j+1
   END FOREACH

   CALL v_arr_restitucion.deleteElement(j)

END FUNCTION

FUNCTION fn_arma_cadena()

   --Arma la cadena segun las condiciones del INPUT
   LET v_condition = " 1=1 "
   --NSS
   IF v_nss IS NOT NULL THEN
      LET v_condition = v_condition," AND a.nss = ",v_nss
   END IF

   --ESTADO
   IF v_estado IS NOT NULL THEN
      LET v_condition = v_condition," AND a.estado = ",v_estado
   END IF

   --FECHA REGISTRO
   --Fecha Registro acotando ambos limites
   IF (f_ini_registro IS NOT NULL) AND (f_fin_registro IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_registro_solicitud BETWEEN '",f_ini_registro,"' AND '",f_fin_registro,"'"      
   END IF

   --Fecha Registro acotando anterior
   IF (f_ini_registro IS NOT NULL) AND (f_fin_registro IS NULL) THEN
      LET v_condition = v_condition," AND b.f_registro_solicitud >= '",f_ini_registro,"'"      
   END IF

   --Fecha Registro acotando después
   IF (f_ini_registro IS NULL) AND (f_fin_registro IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_registro_solicitud <= '",f_fin_registro,"'"      
   END IF

   
   --FECHA SOLICITADO
   IF (f_ini_solicitado IS NOT NULL) AND (f_fin_solicitado IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_agrupa_restitucion BETWEEN '",f_ini_solicitado,"' AND '",f_fin_solicitado,"'"      
   END IF

   IF (f_ini_solicitado IS NOT NULL) AND (f_fin_solicitado IS NULL) THEN
      LET v_condition = v_condition," AND b.f_agrupa_restitucion >= '",f_ini_solicitado,"'"      
   END IF

   IF (f_ini_solicitado IS NULL) AND (f_fin_solicitado IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_agrupa_restitucion <= '",f_fin_solicitado,"'"      
   END IF

   
   --FECHA RESTITUIDO
   IF (f_ini_restituido IS NOT NULL) AND (f_fin_restituido IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_liquida_restitucion BETWEEN '",f_ini_restituido,"' AND '",f_fin_restituido,"'"      
   END IF

   IF (f_ini_restituido IS NOT NULL) AND (f_fin_restituido IS NULL) THEN
      LET v_condition = v_condition," AND b.f_liquida_restitucion >= '",f_ini_restituido,"'"      
   END IF

   IF (f_ini_restituido IS NULL) AND (f_fin_restituido IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_liquida_restitucion <= '",f_fin_restituido,"'"      
   END IF

END FUNCTION