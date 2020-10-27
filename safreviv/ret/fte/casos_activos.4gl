-- 
-- Programa para detectar marcas activas y que no debieran cuando 
-- caso 01: exista el registro en cta_movimiento y deba borrarse de la tabla sfr_marca_activa
-- caso 02: El registro de sfr_marca_activa no este en cta_movimiento, pero tenga ya varios dias activa 
--  

DATABASE safre_viv 

---------------------------------------

MAIN 

  DEFINE v_exi_cta_mov  SMALLINT
  DEFINE v_dia_tra      SMALLINT 
  DEFINE r_sfr_mar_act  RECORD LIKE sfr_marca_activa.*
  DEFINE r_cta_mov      RECORD LIKE cta_movimiento.*
  DEFINE v_nss          CHAR(11) 

  -- CREAR TABLA TEMPORAL 
  CALL crea_temporal()  
  
  -- RECORRER LOS REGISTROS BUSCADOS 
  DECLARE cur_activas CURSOR WITH HOLD FOR 

    SELECT a.*, b.nss
      FROM sfr_marca_activa a, OUTER afi_derechohabiente b 
     WHERE a.marca >= 801 
       AND a.marca <= 810
       AND a.id_derechohabiente = b.id_derechohabiente

  FOREACH cur_activas INTO r_sfr_mar_act.*, v_nss 

    -- CUANDO NO EXISTA EL NSS, ASIGNAR ESTA CADENA   
    IF v_nss IS NULL THEN 
       LET v_nss = "00000000000"
    END IF 

    LET v_exi_cta_mov = 0 

    -- SI LOS ENCUENTRA EN cta_movimento, REPORTARLO     
    DECLARE cur_cta_mov CURSOR WITH HOLD FOR 
     SELECT * 
       FROM cta_movimiento 
      WHERE id_derechohabiente = r_sfr_mar_act.id_derechohabiente 
        AND id_referencia      = r_sfr_mar_act.n_referencia 
		 
    FOREACH cur_cta_mov INTO r_cta_mov.*
	
      LET v_exi_cta_mov = 1 
	
      INSERT INTO revisar_casos 
      VALUES (v_nss                 , r_sfr_mar_act.id_derechohabiente, r_sfr_mar_act.n_referencia, 
              r_sfr_mar_act.f_inicio, r_cta_mov.f_registro            , r_sfr_mar_act.marca       , 
              NULL                  , 1)			   
	
    END FOREACH 
    FREE cur_cta_mov 
	
    -- SI NO ESTA EN cta_movimiento, REPORTAR DIAS TRANSCURRIDOS
    IF v_exi_cta_mov = 0 THEN 

       LET v_dia_tra = TODAY - r_sfr_mar_act.f_inicio

       INSERT INTO revisar_casos 
       VALUES (v_nss                 , r_sfr_mar_act.id_derechohabiente, r_sfr_mar_act.n_referencia, 
               r_sfr_mar_act.f_inicio, NULL                            , r_sfr_mar_act.marca       , 
               v_dia_tra             , 2)			   

    END IF 
	
    -- HACER LOS UNLOADS DE LA TEMPORAL AUNQUE ESTE VACIA PARA QUE QUEDE COMO REFERENCIA EN UN PROCESO BATCH 
    CALL descarga_temporal() 
	
  END FOREACH
  FREE cur_activas 
  
END MAIN 

---------------------------------------

FUNCTION crea_temporal()

   CREATE TEMP TABLE revisar_casos 
   ( 
    nss                  CHAR(11), 
    id_derechohabiente   INTEGER,
    id_solicitud         INTEGER,
    f_inicio             DATE,
    f_fin                DATE,
    marca                SMALLINT,
    dias_trans           SMALLINT,
    caso                 SMALLINT 
   )

END FUNCTION 

---------------------------------------

FUNCTION descarga_temporal()

   UNLOAD TO "/home/ichavez/revisar_casos_01.txt"
   SELECT * FROM revisar_casos WHERE caso = 1 
    ORDER BY  f_inicio, id_derechohabiente 
   
   UNLOAD TO "/home/ichavez/revisar_casos_02.txt"
   SELECT * FROM revisar_casos WHERE caso = 2 
    ORDER BY  f_inicio, id_derechohabiente 

END FUNCTION 

---------------------------------------
