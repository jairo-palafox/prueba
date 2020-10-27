






CREATE PROCEDURE "safreviv".sp_afi_imss_sinf_registra_rechazo(p_det_tpo_movimiento      CHAR(2)     ,
                                                   p_det_nrp                 CHAR(11)    ,
                                                   p_det_f_movimiento        CHAR(8)     ,
                                                   p_det_curp_rfc            CHAR(18)    ,
                                                   p_det_t_trabajador        DECIMAL(1,0),
                                                   p_det_nss                 CHAR(11)    ,
                                                   p_det_nombre              CHAR(50)    ,
                                                   p_det_presentacion_extemp decimal(1,0),
                                                   p_det_jornada_semana      decimal(1,0),
                                                   p_det_sdi                 decimal(6,0),
                                                   p_det_sexo                decimal(1,0),
                                                   p_det_nss_correcto        CHAR(11)    ,
                                                   p_det_nombre_correcto     CHAR(50)    ,
                                                   p_det_riss_imss           SMALLINT    ,
                                                   p_codigo_rechazo          INTEGER     ,
                                                   p_folio                   DECIMAL(9,0))
  
  -- se inserta el rechazo en la tabla de rechazo de afiliacion
  INSERT INTO afi_rch_afiliatorio ( tpo_movimiento      ,
                                    nrp                 ,
                                    f_movimiento        ,
                                    curp_rfc            ,
                                    t_trabajador        ,
                                    nss                 ,
                                    nombre              ,
                                    presentacion_extemp ,
                                    jornada_semana      ,
                                    sdi                 ,
                                    sexo                ,
                                    nss_correcto        ,
                                    nombre_correcto     ,
                                    riss_imss           ,
                                    cod_rechazo         ,
                                    folio_lote)
                           VALUES ( p_det_tpo_movimiento      ,
                                    p_det_nrp                 ,
                                    p_det_f_movimiento        ,
                                    p_det_curp_rfc            ,
                                    p_det_t_trabajador        ,
                                    p_det_nss                 ,
                                    p_det_nombre              ,
                                    p_det_presentacion_extemp ,
                                    p_det_jornada_semana      ,
                                    p_det_sdi                 ,
                                    p_det_sexo                ,
                                    p_det_nss_correcto        ,
                                    p_det_nombre_correcto     ,
                                    p_det_riss_imss           ,
                                    p_codigo_rechazo          ,
                                    p_folio );

END PROCEDURE;


