






CREATE PROCEDURE "safreviv".sp_reversa_desmarca( p_id_derechohabiente     DECIMAL(9,0),
                                      p_marca        SMALLINT,
                                      p_n_referencia DECIMAL(9,0),
                                      p_folio        DECIMAL(9,0));
  DEFINE v_ind_habilita  SMALLINT;
  DEFINE v_marca_resulta SMALLINT;

  DEFINE v_fecha_ini DATE;
  DEFINE v_hora_ini  DATETIME HOUR TO SECOND;
  DEFINE v_fecha_fin DATE;

  DEFINE v_estado_marca SMALLINT;
  DEFINE v_marca_causa  SMALLINT;
  DEFINE v_usr_desmarca CHAR(8);
  DEFINE v_registros    SMALLINT;
  DEFINE v_fecha_causa  DATE;


--set debug file to "/safreviv_int/archivos/rev_desmarca_cuenta.log";
--trace on;

  LET v_registros = 0;

  FOREACH
    SELECT c.f_inicio      ,  -- Necesario para tomar el registro mas viejo
           c.h_inicio      ,
           c.f_fin         ,  -- Necesario para ind_habilita = 1 o 2
           c.estado_marca  ,  -- Necesario para ind_habilita = 2
           c.marca_causa   ,  -- Necesario para ind_habilita = 2
           c.f_marca_causa ,  -- Necesario para ind_habilita = 2
           t.marca_resulta ,  -- Marca con la que se habilita/inhabilita
           t.ind_habilita     -- 0=NO, 1=INHABILITA, 2=HABILITA
    INTO   v_fecha_ini    ,
           v_hora_ini     ,
           v_fecha_fin    ,
           v_estado_marca ,
           v_marca_causa  ,
           v_fecha_causa  ,
           v_marca_resulta,
           v_ind_habilita
      FROM sfr_marca_historica c, 
           sfr_marca t
     WHERE c.id_derechohabiente = p_id_derechohabiente
       AND c.marca        = p_marca
       AND c.n_referencia = p_n_referencia
       AND c.folio        = p_folio
       AND t.marca        = c.marca
    ORDER BY 1,2

     LET v_registros = 1;
     EXIT FOREACH;
  END FOREACH

  IF v_registros = 1 THEN

     IF ( v_ind_habilita > 0 ) THEN  -- Habilita o Inhabilita
   
        SET LOCK MODE TO WAIT ;
   
        DELETE                              -- Borra registro inhabilitado en
             FROM sfr_marca_activa                -- marcas activas
         WHERE id_derechohabiente  = p_id_derechohabiente
           AND marca        = v_marca_resulta
           AND n_referencia = p_n_referencia 
           AND folio      = p_folio;
   
        DELETE                              -- Borra registro in/hablitado de
          FROM sfr_marca_historica          -- historico
         WHERE id_derechohabiente  = p_id_derechohabiente
           AND marca        = v_marca_resulta
           AND n_referencia = p_n_referencia
           AND f_inicio     = v_fecha_fin  
           AND folio        = p_folio;

        UPDATE afi_derechohabiente
           SET ind_estado_cuenta = 0,
               f_estado_cuenta = f_apertura
         WHERE id_derechohabiente = p_id_derechohabiente;
   
     END IF

     LET v_fecha_fin    = NULL ;
     LET v_usr_desmarca = NULL ;


     IF v_ind_habilita < 2 THEN
        LET v_estado_marca = 0 ;
        LET v_marca_causa  = 0 ;
        LET v_fecha_causa   = NULL ;
     END IF
   
     UPDATE sfr_marca_historica                  -- Restaura valores en historico
        SET f_fin        = NULL    ,
            estado_marca = v_estado_marca ,
            marca_causa  = v_marca_causa  ,
            f_marca_causa = v_fecha_causa  ,
            usuario_desmarca = NULL       ,
            proceso_desmarca = NULL
      WHERE id_derechohabiente  = p_id_derechohabiente
        AND marca        = p_marca
        AND n_referencia = p_n_referencia
        AND folio        = p_folio;
   
     INSERT INTO sfr_marca_activa             -- Restaura registro en marcas activas
     SELECT id_derechohabiente,
            marca         ,
            n_referencia  ,
            f_inicio      ,
            h_inicio      ,
            folio         ,
            proceso_marca ,
            marca_causa   ,
            f_marca_causa ,
            f_vigencia    ,
            usuario_marca
       FROM sfr_marca_historica
      WHERE id_derechohabiente = p_id_derechohabiente
        AND marca        = p_marca
        AND n_referencia = p_n_referencia
        AND folio        = p_folio;
   
     SET LOCK MODE TO NOT WAIT;

  END IF  
 
  RETURN ;
  END PROCEDURE;


