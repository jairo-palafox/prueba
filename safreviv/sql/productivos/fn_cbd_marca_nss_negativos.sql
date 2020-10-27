






CREATE FUNCTION "safreviv".fn_cbd_marca_nss_negativos()
	   RETURNING DECIMAL(9,0),varchar(11),varchar(13),varchar(40),
                varchar(40),varchar(40),DECIMAL(22,6),DECIMAL(22,6),smallint;
--Funcion que marca cuentas con nss negativos y retorna información de estas 
              
    DEFINE v_nss                   varchar(11);               
    DEFINE v_rfc                   varchar(13);   
    DEFINE v_a_paterno             varchar(40);
    DEFINE v_a_materno             varchar(40);
    DEFINE v_nombre                varchar(40);
    DEFINE v_s_92_AIVS             DECIMAL(22,6);
    DEFINE v_s_97_AIVS             DECIMAL(22,6);
    DEFINE v_id_derechohabiente    DECIMAL(9,0);
	DEFINE v_contador              SMALLINT;
	DEFINE v_respuesta             SMALLINT;
    DEFINE v_tabla                 VARCHAR(25);
    
	let v_contador=0;
	DROP TABLE IF EXISTS tmp_nss_negativos;
	CREATE TABLE tmp_nss_negativos
	(
		id_derechohabiente DECIMAL(9,0),
		nss CHAR (11) not null
	);
		
	INSERT INTO tmp_nss_negativos--tabal temporal para obtener id de las cuentas con 
                                --con nss insuficientes
	SELECT id_derechohabiente,nss from afi_derechohabiente
		where  nss  matches '*-*';
		
  SELECT tabla_saldo--busqueda de la tabla cta_saldo en servicio
  INTO v_tabla
  FROM safre_sdo@vivws_tcp:glo_saldo
  WHERE ind_saldo = 1;

   IF (v_tabla = 'cta_saldo_diario') THEN
        FOREACH
        
           
            select id_derechohabiente
            into v_id_derechohabiente
            from tmp_nss_negativos

            select afi.nss,
                    afi.rfc,
                    afi.ap_paterno_af,
                    afi.ap_materno_af,
                    afi.nombre_af
            into v_nss,
                 v_rfc,
                 v_a_paterno,
                 v_a_materno,
                 v_nombre
                 
            from afi_derechohabiente afi
            where afi.id_derechohabiente=v_id_derechohabiente;


          select monto_acciones 
          into v_s_92_AIVS
          from safre_sdo@vivws_tcp:cta_saldo_diario
          where  id_derechohabiente=v_id_derechohabiente and
                 subcuenta=4 and fondo_inversion=11;
                 
        
        if v_s_92_AIVS is null then 
            let v_s_92_AIVS=0;
        end if

        select monto_acciones 
          into v_s_97_AIVS
          from safre_sdo@vivws_tcp:cta_saldo_diario
          where  id_derechohabiente=v_id_derechohabiente and
                 subcuenta=8 and fondo_inversion=11;

        if v_s_97_AIVS is null then 
            let v_s_97_AIVS=0;
        end if
                CALL fn_marca_cuenta(v_id_derechohabiente,160,0,0,0,0,160,null,'OPSISSACI','703') RETURNING v_respuesta;
				IF v_respuesta!=0 then 
					LET v_contador=v_contador+1;
                    return v_id_derechohabiente,v_nss,v_rfc,v_a_paterno,
                    v_a_materno, v_nombre, v_s_92_AIVS, v_s_97_AIVS,v_contador with resume;
				END IF
            
            
           
        end FOREACH;  
   else
        
        FOREACH
        
           
            select id_derechohabiente
            into v_id_derechohabiente
            from tmp_nss_negativos

            select afi.nss,
                    afi.rfc,
                    afi.ap_paterno_af,
                    afi.ap_materno_af,
                    afi.nombre_af
            into v_nss,
                 v_rfc,
                 v_a_paterno,
                 v_a_materno,
                 v_nombre
                 
            from afi_derechohabiente afi
            where afi.id_derechohabiente=v_id_derechohabiente;


          select monto_acciones 
          into v_s_92_AIVS
          from safre_sdo@vivws_tcp:cta_saldo_diario_bis
          where  id_derechohabiente=v_id_derechohabiente and
                 subcuenta=4 and fondo_inversion=11;
                 
        
        if v_s_92_AIVS is null then 
            let v_s_92_AIVS=0;
        end if

        select monto_acciones 
          into v_s_97_AIVS
          from safre_sdo@vivws_tcp:cta_saldo_diario_bis
          where  id_derechohabiente=v_id_derechohabiente and
                 subcuenta=8 and fondo_inversion=11;

        if v_s_97_AIVS is null then 
            let v_s_97_AIVS=0;
        end if
                CALL fn_marca_cuenta(v_id_derechohabiente,160,0,0,0,0,160,null,'OPSISSACI','703') RETURNING v_respuesta;
				IF v_respuesta=0 then 
					LET v_contador=v_contador+1;
                    return v_id_derechohabiente,v_nss,v_rfc,v_a_paterno,
                   v_a_materno, v_nombre, v_s_92_AIVS, v_s_97_AIVS,v_contador with resume;

				END IF
            
            
                  
    end FOREACH;  
         
   end if
   DROP TABLE IF EXISTS tmp_nss_negativos;
	
	
END FUNCTION;


