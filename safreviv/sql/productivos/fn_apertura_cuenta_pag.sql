






CREATE FUNCTION "safreviv".fn_apertura_cuenta_pag (p_nss                 CHAR(11)
                                       ,p_curp                CHAR(18)
                                       ,p_rfc                 CHAR(13)
                                       ,p_ind_nrp             CHAR(1)
                                       ,p_nombre_af           CHAR(50)
                                       ,p_tipo_trabajador     CHAR(1)
                                       ,p_id_credito          SMALLINT
                                       ,p_id_origen           SMALLINT
                                       ,p_folio               DECIMAL(9,0)  --folio en turno
                                       ,p_origen_afiliacion   CHAR(1)       --necesario ya que estaba en codigo duro
                                       )

   RETURNING DECIMAL(9)
   
   DEFINE v_id_derechohabiente   DECIMAL(9);
   DEFINE v_fecha_aux            CHAR(6);
   DEFINE v_f_nacimiento         DATE;
   DEFINE v_fecha_hoy            DATE;
   DEFINE v_ano_actual           CHAR(4);
   DEFINE v_error                SMALLINT;
   DEFINE v_nombre_af            CHAR(40);
   DEFINE v_ap_paterno_af        CHAR(40);
   DEFINE v_ap_materno_af        CHAR(40);
   DEFINE v_sexo                 CHAR(1);
   DEFINE v_ind_estado_cuenta    SMALLINT; -- estado de la cuenta 0- activa, 1-inactiva
   DEFINE v_f_estado_cuenta      DATE    ; -- igual a la fecha de apertura


   --ON EXCEPTION SET v_error
   --   TRACE 'Ocurrio el error:'||v_error;
   --END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/apertura_cuenta_pag.trace';

   --TRACE 'Inicia LA ALTA DE CUENTA automatica - FECHA:'||TODAY;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/apertura_cuenta.trace';
   --TRACE 'Inicia LA ALTA DE CUENTA automatica - FECHA:'||TODAY;

   --Inicializacion de variables
   LET v_fecha_hoy = TODAY;

   -- TRACE ("en la inserta pag_nss_sin_cuenta" );
   -- TRACE (p_nss|| v_fecha_hoy|| p_id_origen ) ;
   
   -- Insertar el registro en la tabla "pag_nss_sin_cuenta"
   INSERT INTO pag_nss_sin_cuenta
           (
            nss         ,
            f_alta      ,
            id_origen ,
            estado
           )

   VALUES (
            p_nss,
            v_fecha_hoy,
            p_id_origen ,
            0
          );
                          
   --TRACE 'v_f_nacimiento';
   LET v_f_nacimiento = fn_fnacimiento(p_nss,p_curp,p_rfc);

   --Se separa el nombre del derechohabiente
   CALL fn_separa_nombre(p_nombre_af)
   RETURNING v_ap_paterno_af, v_ap_materno_af, v_nombre_af;

   LET v_sexo = p_curp[11];

   IF v_sexo = 'H' THEN
      LET v_sexo = '1';
   ELIF v_sexo = 'M' THEN
      LET v_sexo = '2';
   ELSE 
      LET v_sexo = '';
   END IF
   
   --TRACE 'v_f_nacimiento'||v_ap_paterno_af||v_ap_materno_af|| v_nombre_af;
   
   -- se indica que la cuenta estara activa
   LET v_ind_estado_cuenta = 0; -- estado de la cuenta 0- activa, 1-inactiva
   LET v_f_estado_cuenta   = v_fecha_hoy; -- igual a la fecha de apertura

   
   -- se obtiene el id_derechohabiente para el derechohabiente nuevo
   SELECT seq_derechohabiente.NEXTVAL
   INTO   v_id_derechohabiente
   FROM   systables
   WHERE  tabid = 1;

   INSERT INTO afi_derechohabiente
          ( 
            id_derechohabiente   ,
            nss                  ,
            curp                 ,
            rfc                  ,
            ind_nrp              ,
            f_nacimiento         ,
            nombre_imss          ,
            nombre_af            ,
            ap_paterno_af        ,
            ap_materno_af        ,
            sexo                 ,
            tipo_trabajador      ,
            origen_afiliacion    ,
            id_credito           ,
            f_credito            ,
            folio_lote           ,
            f_apertura_inf       ,
            f_apertura           ,
            ind_estado_cuenta    ,
            f_estado_cuenta      
           )      
   VALUES (
            v_id_derechohabiente        , --id_derechohabiente
            p_nss                       ,--nss
            p_curp                      ,--curp
            p_rfc                       ,--rfc
            p_ind_nrp                   ,--ind_nrp
            v_f_nacimiento              ,--f_nacimiento
            p_nombre_af                 ,--nombre_imss
            v_nombre_af                 ,--nombre_af
            v_ap_paterno_af             ,--ap_paterno_af
            v_ap_materno_af             ,--ap_materno_af
            v_sexo                      ,--curp
            p_tipo_trabajador           ,--tipo_trabajador
            p_origen_afiliacion         ,--origen_afiliacion
            p_id_credito                ,--id_credito
            v_fecha_hoy                 ,--f_credito
            p_folio                     ,--folio_lote
            v_fecha_hoy                 ,--f_apertura_inf
            v_fecha_hoy                 ,--f_apertura
            v_ind_estado_cuenta         ,--estado de la cuenta
            v_f_estado_cuenta            --fecha de estado de la cuenta

          );

   -- se devuelve el id_derechohabiente   
   RETURN v_id_derechohabiente;
END FUNCTION;


