






CREATE PROCEDURE "safreviv".sp_sep_inserta_exp_f72(p_id_afi_invadido decimal(10,0) ,
                                        p_id_afi_asociado decimal(10,0) ,
                                        p_importe         DECIMAL(22,2)) 
RETURNING SMALLINT, SMALLINT, VARCHAR(255);

--#==========================================================================
--#Autor   : Jesus Yañez Moreno
--#Proyecto: INFONAVIT
--#Creacion: 12/05/2015
--#Descripcion: Registra historico de movimiento de separacion de cuentas
--              por CONTINGENTE.
--#=========================================================================


-- registro expediente

DEFINE v_e_id_expediente_fondo72       DECIMAL(10,0);
DEFINE v_e_id_afi_fondo72_invadido     DECIMAL(10,0);
DEFINE v_e_rfc_invadido                CHAR(013);
DEFINE v_e_id_afi_fondo72_asociado     DECIMAL(10,0);
DEFINE v_e_nss_asociado                CHAR(013);
DEFINE v_e_rfc_asociado                CHAR(013);
DEFINE v_e_nombre_asociado             CHAR(120);
DEFINE v_e_estado                      SMALLINT;

-- registro afi_fondo82

DEFINE v_a_id_afi_fondo72          DECIMAL(10,0);
DEFINE v_a_rfc                     CHAR(013);
DEFINE v_a_nss                     CHAR(011);
DEFINE v_a_id_derechohabiente      DECIMAL(10,0);
DEFINE v_a_nombre                  CHAR(120);
DEFINE v_a_f_apertura              DATE ;
DEFINE v_a_ind_estado_cuenta       SMALLINT;
DEFINE v_a_f_estado_cuenta         DATE;

-- registro sep_movimiento_invadido_fondo72

DEFINE v_m_id_sep_movimiento_invadido_fondo72   DECIMAL(9,0);
DEFINE v_m_id_expediente_fondo72                DECIMAL(9,0);
DEFINE v_m_id_cta_his_fondo72                   DECIMAL(9,0);
DEFINE v_m_nombre                               CHAR(40);
DEFINE v_m_folio                                DECIMAL(9,0);
DEFINE v_m_ejercicio                            CHAR(4);
DEFINE v_m_clave_mov                            CHAR(3);
DEFINE v_m_empresa                              CHAR(40);
DEFINE v_m_bimestres                            SMALLINT;
DEFINE v_m_importe                              DECIMAL(22,2);
DEFINE v_m_rfc_asociado                         CHAR(13);
DEFINE v_m_ind_mov_asociado                     SMALLINT;

DEFINE v_error_sql                              INTEGER;
DEFINE v_isam_err                               INTEGER;        
DEFINE v_msg_sql                                VARCHAR(255);

   ON EXCEPTION SET v_error_sql,v_isam_err,v_msg_sql

      RETURN  v_error_sql   ,
              v_isam_err    ,
              v_msg_sql     ;
         
   END EXCEPTION ;

   LET v_error_sql  = 0;
   LET v_isam_err   = 0;
   LET v_msg_sql    = "registro historico exitoso";

   LET v_e_id_expediente_fondo72   = seq_sep_expediente_fondo72.NEXTVAL;
   LET v_e_id_afi_fondo72_invadido = p_id_afi_invadido;

   SELECT a.rfc            ,
          a.nombre     
   INTO   v_e_rfc_invadido ,
          v_m_nombre
   FROM   afi_fondo72 a
   WHERE  a.id_afi_fondo72 = v_e_id_afi_fondo72_invadido;

   IF DBINFO('SQLCA.SQLERRD2') = 0 THEN

      LET v_error_sql = -746;
      LET v_isam_err  = 0   ; 
      LET v_msg_sql   = "invadido no identificado";

      RAISE EXCEPTION v_error_sql, v_isam_err, v_msg_sql;

   END IF;
   
     LET v_e_id_afi_fondo72_asociado = p_id_afi_asociado;       
   
     SELECT a.nss              ,
            a.rfc              ,
            a.nombre    
     INTO   v_e_nss_asociado   ,
            v_e_rfc_asociado   ,
            v_e_nombre_asociado 
     FROM   afi_fondo72 a 
     WHERE  a.id_afi_fondo72 = v_e_id_afi_fondo72_asociado;

     IF DBINFO('SQLCA.SQLERRD2') = 0 THEN

       LET v_error_sql = -746;
       LET v_isam_err  = 0   ; 
       LET v_msg_sql   = "asociado no identificado";

       RAISE EXCEPTION v_error_sql, v_isam_err, v_msg_sql;

    END IF;

LET v_e_estado = 85;

LET v_m_id_sep_movimiento_invadido_fondo72 = seq_sep_movimiento_invadido_fondo72.NEXTVAL;

LET v_m_id_expediente_fondo72 = v_e_id_expediente_fondo72;
LET v_m_id_cta_his_fondo72    = 0;
LET v_m_folio = 0;
LET v_m_ejercicio = "9999";
LET v_m_clave_mov = "999";
LET v_m_empresa = "CONTINGENTE";
LET v_m_bimestres = 0;
LET v_m_importe   = p_importe;
LET v_m_rfc_asociado = v_e_rfc_asociado;
LET v_m_ind_mov_asociado = 1;

INSERT INTO  sep_expediente_fondo72 VALUES ( v_e_id_expediente_fondo72       ,
                                             v_e_id_afi_fondo72_invadido     ,
                                             v_e_rfc_invadido                ,
                                             v_e_id_afi_fondo72_asociado     ,
                                             v_e_nss_asociado                ,
                                             v_e_rfc_asociado                ,
                                             v_e_nombre_asociado             ,
                                             v_e_estado                      );


INSERT INTO sep_movimiento_invadido_fondo72 VALUES (v_m_id_sep_movimiento_invadido_fondo72   ,
                                             v_m_id_expediente_fondo72                ,
                                             v_m_id_cta_his_fondo72                   ,
                                             v_m_nombre                               , 
                                             v_m_folio                                ,
                                             v_m_ejercicio                            ,
                                             v_m_clave_mov                            ,
                                             v_m_empresa                              ,
                                             v_m_bimestres                            ,
                                             v_m_importe                              ,
                                             v_m_rfc_asociado                         ,
                                             v_m_ind_mov_asociado                     );


RETURN v_error_sql, v_isam_err, v_msg_sql;

END PROCEDURE ;


