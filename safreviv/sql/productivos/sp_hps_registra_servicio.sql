






CREATE PROCEDURE "safreviv".sp_hps_registra_servicio(
p_nss                            CHAR(11),
p_tpo_credito                    CHAR(3),
p_id_credito                     DECIMAL(10,0),
p_f_otorgamiento                 DATE,
p_tpo_descuento                  SMALLINT,
p_valor_descuento                DECIMAL(8,0),
p_mto_liquidar                   DECIMAL(15,2),
p_f_proceso                      DATE,
p_nrp                            CHAR(11),
p_tpo_operacion                  CHAR(3),
p_ind_alta_servicios             SMALLINT,
p_cve_mandato_predial            CHAR(18),
p_f_ini_predial                  DATE,
p_f_fin_predial                  DATE,
p_mto_fondo_predial              DECIMAL(15,2),
p_f_primer_pago_predial          DATE,
p_mto_primer_pago_predial        DECIMAL(15,2),
p_referencia_predial             CHAR(40),
p_cve_mandato_conservacion       CHAR(18),
p_f_ini_conservacion             DATE,
p_f_fin_conservacion             DATE,
p_mto_fondo_conservacion         DECIMAL(15,2),
p_f_primer_pago_conservacion     DATE,
p_mto_primer_pago_conservacion   DECIMAL(15,2),
p_referencia_conservacion        CHAR(40)
)
RETURNING SMALLINT, CHAR(11), CHAR(4), VARCHAR(140), DECIMAL(10,0), SMALLINT

   DEFINE verror                    SMALLINT; -- código de error en caso de excepción
   DEFINE vdiag                    CHAR(3);
   DEFINE vcodResp                  CHAR(4);
   DEFINE vdescResp                 VARCHAR(140);
   DEFINE v_id_derechohabiente     CHAR(11);
   DEFINE v_existe                  DECIMAL(10);      

   ON EXCEPTION SET verror
      -- Devolverá el código de error cuando ocurra una excepción
      LET vcodResp  = "2"||verror;
      LET vdescResp = "Error: ocurrió una excepción";

      RETURN verror, p_nss, vcodResp, vdescResp, p_id_credito, p_tpo_credito;
   END EXCEPTION

  SET DEBUG FILE TO '/safreviv_int/BD/sp_hps_registra_servicio.trace';
  TRACE ON;

   LET verror = 0 ;


   SELECT a.id_derechohabiente
   INTO   v_id_derechohabiente 
   FROM afi_derechohabiente a
   WHERE  a.nss = p_nss;

   IF v_id_derechohabiente IS NULL THEN 
      LET vcodResp = "31";
      LET vdescResp = "nss no existe en BD";
      RETURN verror, p_nss, vcodResp, vdescResp, p_id_credito, p_tpo_credito;
   END IF


   SELECT NVL(MAX(a.id_derechohabiente),0)
   INTO   v_existe
   FROM   hps_solicitud_pago_servicio a 
   WHERE  a.id_derechohabiente = v_id_derechohabiente;  

   IF v_existe <> 0 THEN 
      LET vcodResp = "32";
      LET vdescResp = "nss ya cuenta con solicitud de pago de servicios";
      RETURN verror, p_nss, vcodResp, vdescResp, p_id_credito, p_tpo_credito;
   END IF


   --- se verifica el tipo de solicitud de servicio 
   -- 1 ambas
   -- 2 predial 
   -- 3 cuota de conservacion

   IF p_ind_alta_servicios = 1 OR p_ind_alta_servicios = 2 THEN
   
       SELECT NVL(MAX(a.id_cat_mandato),0)
       INTO   v_existe 
       FROM   mdt_cat_mandato_paquete a  ,
              mdt_cat_mandato  b
       WHERE  a.cve_mandato = p_cve_mandato_predial
       AND    a.id_cat_mandato = b.id_cat_mandato 
       AND    b.tpo_mandato = 1;  -- tipo predial 

       IF v_existe = 0 THEN 
          LET vcodResp = "33";
          LET vdescResp = "identificador de servicio predial no existe en catálogo";
          RETURN verror, p_nss, vcodResp, vdescResp, p_id_credito, p_tpo_credito;
       END IF
    END IF

TRACE "cve predial"||p_cve_mandato_predial;
TRACE "cve conservacion"||p_cve_mandato_conservacion;


   IF p_ind_alta_servicios = 1 OR p_ind_alta_servicios = 3 THEN

      SELECT NVL(MAX(a.id_cat_mandato),0)
      INTO   v_existe 
      FROM   mdt_cat_mandato_paquete a  ,
             mdt_cat_mandato  b
      WHERE  a.cve_mandato = p_cve_mandato_conservacion
      AND    a.id_cat_mandato = b.id_cat_mandato 
      AND    b.tpo_mandato = 2;  -- tipo mantenimiento (conservacion)

      IF v_existe = 0 THEN 
         LET vcodResp = "34";
         LET vdescResp = "identificador de servicio conservacion no existe en catálogo";
         RETURN verror, p_nss, vcodResp, vdescResp, p_id_credito, p_tpo_credito;
      END IF
    END IF

   LET vcodResp = "01";
   LET vdiag    = "000";
   LET vdescResp = "Insertado" ;

   IF p_ind_alta_servicios = 2 THEN 
     
      LET p_cve_mandato_conservacion             = "";
      LET p_f_ini_conservacion                   = "";
      LET p_f_fin_conservacion                   = "";
      LET p_mto_fondo_conservacion               = 0;  
      LET p_f_primer_pago_conservacion           = ""; 
      LET p_mto_primer_pago_conservacion         = "";  
      LET p_referencia_conservacion              = ""; 

    END IF

   IF p_ind_alta_servicios = 3 THEN 
      LET p_cve_mandato_predial                  = "";
      LET p_f_ini_predial                        = "";      
      LET p_f_fin_predial                        = "";     
      LET p_mto_fondo_predial                    = 0;    
      LET p_f_primer_pago_predial                = "";  
      LET p_mto_primer_pago_predial              = "";
      LET p_referencia_predial                   = "";
   END IF

   INSERT INTO hps_solicitud_pago_servicio VALUES (seq_hps_solicitud_pago_servicio.NEXTVAL  ,
                                                   v_id_derechohabiente                     ,
                                                   p_nss                                    ,
                                                   p_tpo_credito                            ,
                                                   p_id_credito                             ,
                                                   p_f_otorgamiento                         ,
                                                   p_tpo_descuento                          ,
                                                   p_valor_descuento                        ,
                                                   p_mto_liquidar                           ,               
                                                   p_f_proceso                              ,           
                                                   p_nrp                                    ,           
                                                   p_tpo_operacion                          ,           
                                                   p_ind_alta_servicios                     ,       
                                                   p_cve_mandato_predial                    ,       
                                                   p_f_ini_predial                          ,      
                                                   p_f_fin_predial                          ,     
                                                   p_mto_fondo_predial                      ,    
                                                   p_f_primer_pago_predial                  ,  
                                                   p_mto_primer_pago_predial                ,
                                                   p_referencia_predial                     ,
                                                   p_cve_mandato_conservacion               ,
                                                   p_f_ini_conservacion                     , 
                                                   p_f_fin_conservacion                     , 
                                                   p_mto_fondo_conservacion                 , 
                                                   p_f_primer_pago_conservacion             , 
                                                   p_mto_primer_pago_conservacion           ,
                                                   p_referencia_conservacion                ,
                                                   vcodResp                                 ,
                                                   vdiag                                    ,
                                                   "101"                                    ,
                                                   "servicio ws");
                     
      RETURN verror, p_nss, vcodResp, vdescResp, p_id_credito, p_tpo_credito;

END PROCEDURE;


