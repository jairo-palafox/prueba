






CREATE FUNCTION "safreviv".fn_act_apellido()
RETURNING SMALLINT, integer, integer

   DEFINE v_nss                char(11);
   DEFINE v_id_derechohabiente decimal(9,0);
   DEFINE v_materno            char(40);
   DEFINE v_paterno            char(40);
   DEFINE v_error              smallint;
   DEFINE v_f_modifica         date;
   DEFINE v_folio              decimal(9,0);
   DEFINE v_proceso_cod        smallint;
   DEFINE v_opera_cod          smallint;
   DEFINE v_usuario            char(20);

   DEFINE vh_nombre_imss       char(40);
   DEFINE vh_curp              char(18);
   DEFINE vh_rfc               char(13);
   DEFINE vh_ind_nrp           char(1);
   DEFINE vh_f_nacimiento      date;
   DEFINE vh_nombre_af         char(40);
   DEFINE vh_ap_paterno_af     char(40);
   DEFINE vh_ap_materno_af     char(40);
   DEFINE vh_ind_modifica      smallint;

   DEFINE v_tot_mat            integer;
   DEFINE v_tot_pat            integer;

   ON EXCEPTION SET v_error
      RETURN v_error, v_tot_mat, v_tot_pat;
   END EXCEPTION;

   ---SET DEBUG FILE TO '/safreviv_int/archivos/fn_act_apellido.trace';
   ---TRACE ON;

   let v_error         = 0;
   let v_f_modifica    = today;
   let v_proceso_cod   = 1821;
   let v_opera_cod     = 1;
   let v_usuario       = "infonavit";
   let vh_ind_modifica = 9;
   let v_tot_mat       = 0;
   let v_tot_pat       = 0;

   let vh_nombre_imss   = "";
   let vh_curp          = "";
   let vh_rfc           = "";
   let vh_ind_nrp       = "";
   let vh_f_nacimiento  = "";
   let vh_nombre_af     = "";
   let vh_ap_paterno_af = "";
   let vh_ap_materno_af = "";

   let v_id_derechohabiente = "";

   EXECUTE FUNCTION fn_genera_folio(v_proceso_cod, v_opera_cod,v_usuario)
                               INTO v_folio;

   ---Actualiza apellido materno
   FOREACH
      SELECT nss,
             ap_materno
        INTO v_nss,
             v_materno
        FROM safre_tmp:ap_materno

      SELECT id_derechohabiente,
             nombre_imss  ,
             curp         ,
             rfc          ,
             ind_nrp      ,
             f_nacimiento ,
             nombre_af    ,
             ap_paterno_af,
             ap_materno_af
        INTO v_id_derechohabiente,
             vh_nombre_imss    ,
             vh_curp           ,
             vh_rfc            ,
             vh_ind_nrp        ,
             vh_f_nacimiento   ,
             vh_nombre_af      ,
             vh_ap_paterno_af  ,
             vh_ap_materno_af 
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      if v_id_derechohabiente is not null then
         INSERT INTO safre_tmp:tmp_materno_rq38
         VALUES (v_nss,
                 v_id_derechohabiente,
                 vh_ap_materno_af,
                 v_folio);

         INSERT INTO afi_his_derechohabiente (id_derechohabiente   ,
                                              f_modifica           ,
                                              folio_lote_modifica  ,
                                              ind_modifica         ,
                                              curp                 ,
                                              rfc                  ,
                                              ind_nrp              ,
                                              f_nacimiento         ,
                                              nombre_imss          ,
                                              nombre_af            ,
                                              ap_paterno_af        ,
                                              ap_materno_af)
                                      VALUES (v_id_derechohabiente ,
                                              v_f_modifica         ,
                                              v_folio              ,
                                              vh_ind_modifica      ,
                                              vh_curp              ,
                                              vh_rfc               ,
                                              vh_ind_nrp           ,
                                              vh_f_nacimiento      ,
                                              vh_nombre_imss       ,
                                              vh_nombre_af         ,
                                              vh_ap_paterno_af     ,
                                              vh_ap_materno_af);

         UPDATE afi_derechohabiente
            SET ap_materno_af      = v_materno
          WHERE id_derechohabiente = v_id_derechohabiente;
   
         LET v_tot_mat = v_tot_mat + 1;
      end if

      LET v_id_derechohabiente = "";
   END FOREACH

   ---Actualiza apellido paterno
   FOREACH
      SELECT nss,
             ap_paterno
        INTO v_nss,
             v_paterno
        FROM safre_tmp:ap_paterno

      SELECT id_derechohabiente,
             nombre_imss  ,
             curp         ,
             rfc          ,
             ind_nrp      ,
             f_nacimiento ,
             nombre_af    ,
             ap_paterno_af,
             ap_materno_af
        INTO v_id_derechohabiente,
             vh_nombre_imss    ,
             vh_curp           ,
             vh_rfc            ,
             vh_ind_nrp        ,
             vh_f_nacimiento   ,
             vh_nombre_af      ,
             vh_ap_paterno_af  ,
             vh_ap_materno_af 
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      if v_id_derechohabiente is not null then
         INSERT INTO safre_tmp:tmp_paterno_rq38
         VALUES (v_nss,
                 v_id_derechohabiente,
                 vh_ap_paterno_af,
                 v_folio);

         INSERT INTO afi_his_derechohabiente (id_derechohabiente   ,
                                              f_modifica           ,
                                              folio_lote_modifica  ,
                                              ind_modifica         ,
                                              curp                 ,
                                              rfc                  ,
                                              ind_nrp              ,
                                              f_nacimiento         ,
                                              nombre_imss          ,
                                              nombre_af            ,
                                              ap_paterno_af        ,
                                              ap_materno_af)
                                      VALUES (v_id_derechohabiente ,
                                              v_f_modifica         ,
                                              v_folio              ,
                                              vh_ind_modifica      ,
                                              vh_curp              ,
                                              vh_rfc               ,
                                              vh_ind_nrp           ,
                                              vh_f_nacimiento      ,
                                              vh_nombre_imss       ,
                                              vh_nombre_af         ,
                                              vh_ap_paterno_af     ,
                                              vh_ap_materno_af);

         UPDATE afi_derechohabiente
            SET ap_paterno_af      = v_paterno
          WHERE id_derechohabiente = v_id_derechohabiente;

         LET v_tot_pat = v_tot_pat + 1;
      end if

      LET v_id_derechohabiente = "";
   END FOREACH

   UPDATE STATISTICS FOR TABLE afi_derechohabiente;
   UPDATE STATISTICS FOR TABLE afi_his_derechohabiente;

   RETURN v_error, v_tot_mat, v_tot_pat;

END FUNCTION;


