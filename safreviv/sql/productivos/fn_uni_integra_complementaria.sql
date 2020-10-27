






CREATE FUNCTION "safreviv".fn_uni_integra_complementaria(p_usuario_cod    CHAR(20),
                                              p_proceso_cod    SMALLINT,
                                              p_nombre_archivo CHAR(40),
                                              p_folio          DECIMAL(9,0), 
                                              p_pid            DECIMAL(9,0))

RETURNING SMALLINT,
          INTEGER,
          INTEGER,
          INTEGER,
          INTEGER,
          VARCHAR(255)

DEFINE v_tipo_registro     CHAR(2);
DEFINE v_consecutivo       CHAR(5);
DEFINE v_nss_unificador    CHAR(11);
DEFINE v_nss_unificado     CHAR(11);
DEFINE v_r_nss_unificador  CHAR(11);
DEFINE v_id_dh_unificador  DECIMAL(9,0);
DEFINE v_id_dh_unificado   DECIMAL(9,0);
DEFINE v_marca             SMALLINT;
DEFINE v_diagnostico       SMALLINT;
DEFINE v_n_referencia      DECIMAL(9,0);
DEFINE v_id_unificador     DECIMAL(9,0);
DEFINE v_id_unificado      DECIMAL(9,0);
DEFINE v_sub_cuenta        SMALLINT;
DEFINE v_fondo_inversion   SMALLINT;
DEFINE v_acciones          DECIMAL(16,2);
DEFINE v_pesos             DECIMAL(16,2);
DEFINE v_tot_acciones      DECIMAL(16,2);
DEFINE v_tot_pesos         DECIMAL(16,2);
DEFINE v_id_complementario DECIMAL(9,0);
DEFINE v_tipo_unificacion  SMALLINT;
DEFINE v_estado            SMALLINT;

-- Variables que se retornan
DEFINE v_i_resultado       SMALLINT;
DEFINE v_tot_solicitudes   INTEGER;
DEFINE v_tot_aceptadas     INTEGER;
DEFINE v_tot_rechazadas    INTEGER;
-- Control de Excepciones              
DEFINE sql_err             INTEGER;
DEFINE isam_err            INTEGER;
DEFINE err_txt             VARCHAR(255);
DEFINE v_si_resultado      SMALLINT;
DEFINE v_i_registros_insertados INTEGER;

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, 
             v_tot_solicitudes,
             v_tot_aceptadas, 
             v_tot_rechazadas, 
             isam_err, 
             err_txt;
   END EXCEPTION

LET v_tipo_registro     = "";
LET v_consecutivo       = "";
LET v_nss_unificador    = ""; 
LET v_nss_unificado     = "";
LET v_id_dh_unificador  = 0;
LET v_id_dh_unificado   = 0;
LET v_marca             = 0;
LET v_diagnostico       = 0;
LET v_n_referencia      = 0;
LET v_id_unificador     = 0;
LET v_id_unificado      = 0;
LET v_id_complementario = 0;
LET v_tot_acciones      = 0;
LET v_tot_pesos         = 0;
LET v_tipo_unificacion  = 0;
LET v_i_resultado       = 0;
LET v_tot_solicitudes   = 0;
LET v_tot_aceptadas     = 0;
LET v_tot_rechazadas    = 0;
LET sql_err             = NULL;
LET isam_err            = NULL;
LET err_txt             = NULL;

   --SET DEBUG FILE TO "/safreviv_int/BD/trace_uni_integra_compl.txt";
   --TRACE ON;
   
   FOREACH 
      SELECT *
      INTO   v_tipo_registro,
             v_consecutivo  ,
             v_nss_unificador,
             v_nss_unificado
      FROM   safre_tmp:tmp_uni_det_compl
      
      IF v_tipo_registro <> "02" THEN 
         LET v_diagnostico = 14;
         LET v_tot_rechazadas = v_tot_rechazadas + 1;
      END IF 
   
      SELECT id_derechohabiente 
      INTO   v_id_dh_unificador 
      FROM   afi_derechohabiente 
      WHERE  nss = v_nss_unificador;
      
      IF v_id_dh_unificador IS NULL THEN 
         LET v_diagnostico = 13; --DERECHOHABIENTE DOR NO EXISTE
      ELSE 
         SELECT id_derechohabiente 
         INTO   v_id_dh_unificado
         FROM   afi_derechohabiente 
         WHERE  nss = v_nss_unificado;

         IF v_id_dh_unificado IS NULL THEN
            LET v_diagnostico = 15; --DERECHOHABIENTE ADO NO EXISTE
         ELSE
            --UNIFICACIONES IMSS           
            SELECT nss_unificador
            INTO   v_r_nss_unificador
            FROM   uni_det_unificador
            WHERE  id_unificador in (SELECT id_unificador
                                     FROM   uni_det_unificado
                                     WHERE  id_derechohabiente = v_id_dh_unificado
                                     AND    estado_unificacion = 1
                                     GROUP BY 1)
            AND    estado_familia     = 1
            GROUP BY 1;

            --IF v_id_unificador IS NOT NULL AND v_id_unificado IS NOT NULL THEN
            IF v_nss_unificador = v_r_nss_unificador THEN
               SELECT marca, 
                      n_referencia 
               INTO   v_marca,
                      v_n_referencia
               FROM   sfr_marca_activa 
               WHERE  id_derechohabiente = v_id_dh_unificado 
               AND    marca = 150;
               
               IF  v_marca IS NULL THEN 
                  LET v_diagnostico = 29; --LA MARCA NO ACTIVA
               ELSE
                  LET v_tipo_unificacion = 1; --IMSS
                  
                  LET v_sub_cuenta = 0;
                  LET v_fondo_inversion = 0;
                  LET v_acciones = 0;
                  LET v_pesos = 0;
                  
                  FOREACH
                     EXECUTE FUNCTION fn_saldo_actual(v_nss_unificado,0,TODAY)
                     INTO v_sub_cuenta,
                          v_fondo_inversion,
                          v_acciones,
                          v_pesos
                          
                     IF v_acciones IS NULL THEN
                        LET v_acciones = 0;
                     END IF
                     
                     IF v_pesos IS NULL THEN
                        LET v_pesos = 0;
                     END IF

                     IF v_acciones > 0 AND v_pesos > 0 THEN 
                        LET v_tot_acciones = v_tot_acciones + v_acciones;
                        LET v_tot_pesos = v_tot_pesos + v_pesos;
                     END IF
                  END FOREACH

                  IF v_tot_acciones > 0 AND v_tot_pesos > 0 THEN 
                     LET v_diagnostico = 1; --ACEPTADA
                     
                     LET v_tot_acciones = 0;
                     LET v_tot_pesos    = 0;                     
                  ELSE
                     LET v_diagnostico = 44; --RECHAZADA
                  END IF
               END IF
            ELSE            
               SELECT nss
               INTO   v_r_nss_unificador
               FROM   uni_inf_unificador
               WHERE  id_inf_unificador IN (SELECT id_unificador
                                            FROM   uni_inf_unificado
                                            WHERE  id_derechohabiente = v_id_dh_unificado
                                            AND    estado_unificacion = 1
                                            GROUP BY 1)
               AND    estado_familia = 1
               GROUP BY 1;
               
               --IF v_id_unificador IS NOT NULL AND v_id_unificado IS NOT NULL THEN
               IF v_nss_unificador = v_r_nss_unificador THEN
                  SELECT marca, 
                         n_referencia 
                  INTO   v_marca,
                         v_n_referencia
                  FROM   sfr_marca_activa 
                  WHERE  id_derechohabiente = v_id_dh_unificado 
                  AND    marca = 151;

                  IF v_marca IS NULL THEN
                     LET v_diagnostico = 29; --LA MARCA NO ACTIVA
                  ELSE
                     LET v_tipo_unificacion = 2; --INFONAVIT
                     IF v_id_unificador IS NOT NULL AND v_id_unificado IS NOT NULL THEN
                        
                        LET v_sub_cuenta = 0;
                        LET v_fondo_inversion = 0;
                        LET v_acciones = 0;
                        LET v_pesos = 0;

                        FOREACH 
                           EXECUTE FUNCTION fn_saldo_actual(v_nss_unificado,0,TODAY)
                           INTO v_sub_cuenta,
                                v_fondo_inversion,
                                v_acciones,
                                v_pesos

                           IF v_acciones > 0 AND v_pesos > 0 THEN 
                              LET v_tot_acciones = v_tot_acciones + v_acciones;
                              LET v_tot_pesos = v_tot_pesos + v_pesos;
                           END IF
                        END FOREACH
                  
                        IF v_tot_acciones > 0 AND v_tot_pesos > 0 THEN 
                           LET v_diagnostico = 1; --ACEPTADA
                           
                           LET v_tot_acciones = 0;            
                           LET v_tot_pesos    = 0;
                        ELSE
                           LET v_diagnostico = 44; --RECHAZADA
                        END IF
                     ELSE
                        LET v_diagnostico = 43; --DOR Y ADO IFVT NO ASOCIADOS
                     END IF
                  END IF --MARCA 151
               ELSE
                  LET v_diagnostico = 42; --DOR Y ADO IMSS NO ASOCIADOS
               END IF --RELACION INFONAVIT
            END IF --RELACION IMSS
         END IF --UNIFICADO NULO
      END IF --UNIFICADOR NULO

      SELECT seq_uni_det_complementario.NEXTVAL
      INTO   v_id_complementario
      FROM   systables
      WHERE  tabid = 1;
      
      LET v_estado = 1; --En proceso de unificación

      INSERT INTO uni_det_complementario
                 (id_complementario     ,
                  folio_complentario    ,
                  consecutivo_archivo   ,
                  nss_unificador        ,
                  id_unificador         ,
                  id_derechohabiente_dor,
                  nss_unificado         ,
                  id_unificado          ,
                  id_derechohabiente_ado,
                  tipo_unificacion      ,
                  estado                ,
                  diagnostico)
      VALUES(v_id_complementario,
             p_folio,
             v_consecutivo,
             v_nss_unificador,
             v_id_unificador,
             v_id_dh_unificador,
             v_nss_unificado,
             v_id_unificado,
             v_id_dh_unificado,  
             v_tipo_unificacion,
             v_estado,
             v_diagnostico);

      LET v_tot_solicitudes  = v_tot_solicitudes + 1;
   END FOREACH;

   SELECT COUNT (*)
   INTO   v_tot_aceptadas
   FROM   uni_det_complementario
   WHERE  folio_complentario = p_folio
   AND    diagnostico = 1;
   
   SELECT COUNT (*)
   INTO   v_tot_rechazadas
   FROM   uni_det_complementario
   WHERE  folio_complentario = p_folio
   AND    diagnostico <> 1;
      
	 UPDATE glo_ctr_archivo
	 SET    folio     = p_folio,
	        estado    = 2
	 WHERE  nombre_archivo = p_nombre_archivo;
   
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;
  
 RETURN v_i_resultado,
        v_tot_solicitudes,
        v_tot_aceptadas,
        v_tot_rechazadas, 
        isam_err, 
        err_txt;
END FUNCTION;


