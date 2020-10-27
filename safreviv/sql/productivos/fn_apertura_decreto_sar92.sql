






CREATE FUNCTION "safreviv".fn_apertura_decreto_sar92(lr_tmp_det_trabajador_nci            CHAR(30)
                                         ,lr_tmp_det_trabajador_id_unico       DECIMAL(11,0)
                                         ,lr_tmp_det_trabajador_nss            CHAR(11)
                                         ,lr_tmp_det_trabajador_rfc            CHAR(13)
                                         ,lr_tmp_det_trabajador_nom_trabajador CHAR(120)
                                         ,lr_tmp_det_trabajador_rfc_patron     CHAR(13)
                                         ,lr_tmp_det_trabajador_nom_patron     CHAR(40))
                                         
   RETURNING DECIMAL(11,0);
   
   DEFINE ld_ide_decreto    DECIMAL(11,0);
   DEFINE v_fecha_nacim_aux CHAR(10);
   DEFINE v_fecha_aux       CHAR(6);
   DEFINE v_fecha_nacim     CHAR(6);   DEFINE v_hoy             DATE;
   DEFINE v_ano_actual      CHAR(4);
   DEFINE v_error           SMALLINT;
   
   --ON EXCEPTION SET v_error
      --TRACE 'Ocurrio el error:'||v_error;
   --END EXCEPTION WITH RESUME
   
   
   --SET DEBUG FILE TO '/safreviv/apertura_decreto.trace';
   
--   --Inicializacion de variables
--   LET v_hoy = TODAY;
--
--   --Valida que el RFC contenga informacion
--   IF (lr_tmp_det_trabajador_rfc IS NULL) THEN
--      LET lr_tmp_det_trabajador_rfc = "AAAA010101";
--   ELSE
--      IF (lr_tmp_det_trabajador_rfc[5,10] = "111111" OR 
--          lr_tmp_det_trabajador_rfc[5,10] = "010101") THEN
--         LET lr_tmp_det_trabajador_rfc = "AAAA010101";
--      END IF
--   END IF
--   
--   --TRACE ("v_hoy = "||v_hoy) ;
--   
--   --Obtiene la fecha de nacimiento, la cual proviene 
--   --de los 6 caracteres de la CURP que indican la fecha.
--   --Si embargo es necesario transformalos a 
--   --un formato MM/DD/AAAA  MABJ640229M75
--   LET v_fecha_aux = lr_tmp_det_trabajador_rfc[5,10];
--   LET v_fecha_nacim_aux[1,6] = v_fecha_aux[3,4]||"/"||v_fecha_aux[5,6]||"/";
--   LET v_ano_actual = YEAR(TODAY);
--   IF ( v_fecha_aux[1,2] > v_ano_actual[1,2] ) THEN
--      LET v_fecha_nacim_aux[7,10] = "19"||v_fecha_aux[1,2];
--   ELSE
--      LET v_fecha_nacim_aux[7,10] = "20"||v_fecha_aux[1,2];
--   END IF
--   --Se asigna la fecha de nacimiento con formato MM/DD/AAAA
   
   LET v_fecha_nacim = NULL;
   
-------------   LET v_fecha_nacim = DATE(v_fecha_nacim_aux);

   IF (v_fecha_nacim IS NOT NULL) THEN
   
      INSERT INTO afi_decreto
      VALUES (
         seq_afi_decreto.NEXTVAL,               --id_decreto
         lr_tmp_det_trabajador_id_unico,        --consecutivo_cuenta
         lr_tmp_det_trabajador_nci,             --nci
         lr_tmp_det_trabajador_nss,             --nss
         lr_tmp_det_trabajador_rfc,             --rfc
--         lr_tmp_det_trabajador_curp,            --curp
         NULL,                                  --curp
         NULL,                                  --tpo_entidad
         NULL,                                  --cve_icefa
         lr_tmp_det_trabajador_nom_trabajador,  --nombre_trabajador
         v_fecha_nacim,                         --f_nacimiento       
         0,                                     --id_credito         
         NULL,                                  --cve_retiro
         lr_tmp_det_trabajador_rfc_patron,      --rfc_patron
         NULL,                                  --nss_patron
         lr_tmp_det_trabajador_nom_patron,      --nombre_patron
         NULL,                                   --exp_infonavit
         1                                       --se inserta el indicador de consistenco en 1 
         );
   ELSE
      INSERT INTO afi_decreto
      VALUES (
         seq_afi_decreto.NEXTVAL,     --id_decreto
         lr_tmp_det_trabajador_id_unico,        --consecutivo_cuenta
         lr_tmp_det_trabajador_nci,             --nci
         lr_tmp_det_trabajador_nss,             --nss
         lr_tmp_det_trabajador_rfc,             --rfc
         NULL,                                  --curp
         NULL,                                  --tpo_entidad
         NULL,                                  --cve_icefa
         lr_tmp_det_trabajador_nom_trabajador,  --nombre_trabajador
         NULL,                                  --f_nacimiento
         0,                                     --id_credito
         NULL,                                  --cve_retiro
         lr_tmp_det_trabajador_rfc_patron,      --rfc_patron
         NULL,                                  --nss_patron
         lr_tmp_det_trabajador_nom_patron,      --nombre_patron
         NULL,                                   --exp_infonavit
         1                                       --se inserta el indicador de consistenco en 1 
         );
   END IF

   --Obtenemos el ID insertado  
   SELECT seq_afi_decreto.CURRVAL
     INTO ld_ide_decreto
     FROM systables
    WHERE tabid = 1;
    
    --TRACE ("ID_DECRETO = "||ld_ide_decreto) ;

   RETURN ld_ide_decreto;   
END FUNCTION;


