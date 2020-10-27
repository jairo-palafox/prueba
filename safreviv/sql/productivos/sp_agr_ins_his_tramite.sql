






CREATE PROCEDURE "safreviv".sp_agr_ins_his_tramite(pid_cre_tramite DECIMAL(9,0),
                                        pnum_credito    DECIMAL(10,0),
                                        pf_vigencia     DATE,
                                        pestado         SMALLINT,
                                        pcodResp        CHAR(4),
                                        phProceso       DATETIME HOUR TO SECOND,
                                        pHist           SMALLINT)

   DEFINE vfProceso                 DATE;

   LET vfProceso = TODAY;

   INSERT INTO cre_his_tramite (
               id_cre_tramite ,
               num_credito    ,
               f_vigencia     ,
               estado         ,
               diagnostico    ,
               f_proceso)
       VALUES (pid_cre_tramite,
               pnum_credito   ,
               pf_vigencia    ,
               pestado        ,
               pcodResp       ,
               phProceso);

   IF pHist = 1 THEN
      INSERT INTO cre_num_credito (
                  id_cre_tramite,
                  num_credito   ,
                  f_proceso)
          VALUES (pid_cre_tramite,
                  pnum_credito  ,
                  vfProceso);
   END IF

   RETURN;

END PROCEDURE;


