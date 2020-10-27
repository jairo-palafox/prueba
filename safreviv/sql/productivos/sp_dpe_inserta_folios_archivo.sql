






CREATE PROCEDURE "safreviv".sp_dpe_inserta_folios_archivo(p_proceso_cod SMALLINT,
                                               p_opera_cod   SMALLINT,
                                               p_nom_archivo CHAR(40),
                                               p_cad_folios  VARCHAR(150),
                                               p_usuario_cod CHAR(10))
   INSERT INTO dpe_ctr_archivo
   VALUES     (p_proceso_cod, 
               p_opera_cod,   
               p_nom_archivo, 
               p_cad_folios,  
               0, -- Archivo enviado
               TODAY,
               p_usuario_cod);
           
END PROCEDURE -- sp_dpe_pre_integra;


