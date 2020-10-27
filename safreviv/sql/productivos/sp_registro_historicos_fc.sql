






CREATE PROCEDURE "safreviv".sp_registro_historicos_fc(p_folio DECIMAL(9,0),
                                              p_pid DECIMAL(9,0),
                                              p_proceso_cod SMALLINT
                                              )     
   RETURNING SMALLINT, SMALLINT, VARCHAR(255), CHAR(11)

   --Se inicia con la declaracion de variables
   --Tabla: tmp_cza_forta                    
   
   DEFINE v_tmp_cza_forta_tpo_registro          CHAR(2)       ;
   DEFINE v_tmp_cza_forta_id_proceso            CHAR(4)       ;
   DEFINE v_tmp_cza_forta_id_operacion          CHAR(2)       ;
   DEFINE v_tmp_cza_forta_f_archivo             CHAR(8)       ;

   --table tmp_det_forta   
   DEFINE v_tmp_det_forta_tpo_registro          CHAR(2)       ;
   DEFINE v_tmp_det_forta_conse_registro        DECIMAL(10,0) ;
   DEFINE v_tmp_det_forta_f_pago                CHAR(8)  ;
   DEFINE v_tmp_det_forta_nss                   CHAR(11)      ;
   DEFINE v_tmp_det_forta_imp_ap_fort_cre       DECIMAL(12,2)  ;
   DEFINE v_det_forta_id_derechohabiente        DECIMAL(9,0)  ;

   --table tmp_sum_forta   
   DEFINE v_tmp_sum_forta_tpo_registro          CHAR(01)      ;
   DEFINE v_tmp_sum_forta_num_reg_detalle       DECIMAL(10,0) ;
   DEFINE v_tmp_sum_forta_tot_ap_forta          DECIMAL(18,2) ;
   DEFINE v_d_tmp_pag_det_forta_f_pago          DATE          ;
   DEFINE v_d_tmp_pag_det_forta_f_archivo       DATE          ;
   DEFINE v_c_result_operacion                  CHAR(02)      ;

   -- para validar sumario
   DEFINE v_tmp_num_registros                  DECIMAL (10,0);
   DEFINE v_tmp_sum_tot_aport                  DECIMAL (18,2);
   
   DEFINE v_tmp_num_registros_sum              DECIMAL (10,0);
   DEFINE v_tmp_sum_tot_aport_sum              DECIMAL (18,2);
   
   DEFINE v_nombre_archivo                     CHAR(40);
   DEFINE v_error_en_sumario                   SMALLINT; -- booleana para saber si hubo error en sumario
      
    -- Control de Excepciones
   DEFINE sql_err                               SMALLINT;
   DEFINE isam_err                              SMALLINT;
   DEFINE err_txt                               VARCHAR(255);
   DEFINE v_si_resultado                        SMALLINT;   

   --manejo de excepciones
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_forta_nss;
   END EXCEPTION

   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_registro_historicos_forta.trace';
   --TRACE 'Inicia el store procedure de registro historicos de SOLO INFONAVIT';
   --TRACE "Folio:"||p_folio;


--@@ Se valida que las tablas temporales y las de integración en sumario mantengan los mismos datos

   -- se asume que no hay error en sumario
   LET v_error_en_sumario = 0;
   
   LET v_tmp_det_forta_nss = NULL;

 -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
       SET folio = P_folio,
           estado = 2 -- integrado
     WHERE proceso_cod    = p_proceso_cod
       AND opera_cod      = 1 -- archivo cargado
       AND estado         = 1; -- etapa de carga

      -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;

   UPDATE bat_ctr_proceso
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND pid         = p_pid;
   
   -- se revisa el sumario contra el detalle
   -- DETALLE
   SELECT COUNT(*)
     INTO v_tmp_num_registros
     FROM safre_tmp:tmp_det_forta ;
   
   SELECT SUM(imp_ap_fort_cre)
     INTO v_tmp_sum_tot_aport
     FROM safre_tmp:tmp_det_forta ;
     
   -- SUMARIO
   SELECT num_registros
     INTO v_tmp_num_registros_sum
     FROM safre_tmp:tmp_sum_forta ;
   
   SELECT SUM(tot_imp_ap_fort_cre)
     INTO v_tmp_sum_tot_aport_sum
     FROM safre_tmp:tmp_sum_forta ;   
     

     -- con alguno que no coincida, es causa de rechazo de lote
     -- compara el numero de registros
     IF ( v_tmp_num_registros <> v_tmp_num_registros_sum ) THEN
        LET v_si_resultado = 10;
        LET isam_err       = 0;
        LET err_txt        = "@El numero de registros en detalle y en sumario no coinciden";
        
        LET v_error_en_sumario = 1;
     END IF

     -- compara el numero de registros
     IF ( v_tmp_sum_tot_aport <> (v_tmp_sum_tot_aport_sum) ) THEN
        LET v_si_resultado = 20;
        LET isam_err       = 0;
        LET err_txt        = "@La suma del total de aportaciones en detalle y en sumario no coincide";
        
        LET v_error_en_sumario = 1;
     END IF    
     
     -- si hubo error en sumario, entonces se cancela el folio y se devuelve el error
     IF ( v_error_en_sumario = 1 ) THEN
{
        -- se actualiza el folio en turno como erroneo
        UPDATE glo_folio
        SET    status = -1
        WHERE  folio = p_folio;

        -- se obtiene el nombre del archivo
        SELECT nombre_archivo
        INTO   v_nombre_archivo
        FROM   glo_ctr_archivo
        WHERE  proceso_cod = p_proceso_cod 
        AND    folio IS NULL;
        
        -- se deja el archivo en estatus de error
        UPDATE glo_ctr_archivo
           SET folio = P_folio,
               estado = 2 -- integrado
         WHERE proceso_cod    = p_proceso_cod
           AND opera_cod      = 1 -- archivo cargado
           AND estado         = 1; -- etapa de carga
}        
        -- se devuelve el error
        RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_forta_nss;
     END IF
   
             
   --TRACE 'Inicia el FOREACH tmp_cza_forta';  
   --se hace el foreach e insert de las tablas de encbezado solo infonavit 
   FOREACH
   	
      --se seleccionan los registros de la tabla temporal de encabezado
      SELECT tpo_registro ,
             id_proceso   , 
             id_operacion , 
             f_archivo
        INTO v_tmp_cza_forta_tpo_registro   ,
             v_tmp_cza_forta_id_proceso     ,
             v_tmp_cza_forta_id_operacion   ,
             v_tmp_cza_forta_f_archivo      
        FROM safre_tmp:tmp_cza_forta

      -- se formate la fecha de pago
      LET v_d_tmp_pag_det_forta_f_archivo =  v_tmp_cza_forta_f_archivo[5,6]||"/"||v_tmp_cza_forta_f_archivo[7,8]||"/"||v_tmp_cza_forta_f_archivo[1,4] ;

      --se insertan los registros en la tabla encabezado
      INSERT INTO  pag_cza_fc
                   (
                     folio           ,
                     tpo_registro    ,
                     proceso_cod     ,
                     opera_cod       , 
                     f_archivo       
                   )
               VALUES 
                   ( 
                     p_folio                             ,
                     v_tmp_cza_forta_tpo_registro        ,
                     v_tmp_cza_forta_id_proceso          ,
                     v_tmp_cza_forta_id_operacion        ,
                     v_d_tmp_pag_det_forta_f_archivo      
                   );
   END FOREACH ;
   
   FOREACH   	
   	  --se seleccionan los registros de la tabla temporal de detalle	
      SELECT tpo_registro            ,
             conse_registro          ,
             f_pago                  ,
             nss                     ,
             (imp_ap_fort_cre )/100
        INTO v_tmp_det_forta_tpo_registro          ,       
             v_tmp_det_forta_conse_registro        ,
             v_tmp_det_forta_f_pago                ,
             v_tmp_det_forta_nss                   ,
             v_tmp_det_forta_imp_ap_fort_cre          
        FROM safre_tmp:tmp_det_forta    

         --se selecciona el id_derechohabioente de afi_derechohabiente  

         SELECT id_derechohabiente 
           INTO v_det_forta_id_derechohabiente
           FROM afi_derechohabiente 
          WHERE nss  =  v_tmp_det_forta_nss ;

          --SI EL ID_DERECHOHABIENTE NO EXISTE  result_operacion = 02 
          IF (v_det_forta_id_derechohabiente IS NULL) THEN 
          	 LET v_c_result_operacion = "02"; --rechazado
          --En el caso contrario   result_operacion = 01 	
          --para el caso de la preliqiudación solo se tomaraqn en cuenta los  result_operacion = 01 
          ELSE
          	 LET v_c_result_operacion = "01";
          END IF 

       -- se formate la fecha de pago
       LET v_d_tmp_pag_det_forta_f_pago =  v_tmp_det_forta_f_pago[5,6]||"/"||v_tmp_det_forta_f_pago[7,8]||"/"||v_tmp_det_forta_f_pago[1,4] ;
       
       --se insertan los registros a la tabla del detalle 
       INSERT INTO  pag_det_fc
                   (
                     folio                ,
                     origen_archivo       ,
                     id_referencia        ,
                     f_pago               ,
                     id_derechohabiente   ,
                     nss                  ,
                     imp_ap_fc            ,
                     result_operacion     
                   )
             VALUES 
                   (
                     p_folio                            ,
                     9                                  ,--fortalecimiento de crédito
                     v_tmp_det_forta_conse_registro     ,
                     v_d_tmp_pag_det_forta_f_pago       ,
                     v_det_forta_id_derechohabiente     ,
                     v_tmp_det_forta_nss                ,
                     v_tmp_det_forta_imp_ap_fort_cre    ,
                     v_c_result_operacion
                   );
   END FOREACH ;


   FOREACH   	
   	  --se seleccionan los registros de la tabla temporal del sumatorio
      SELECT tpo_registro            ,
             num_registros           ,
             (tot_imp_ap_fort_cre ) /100   
             
        INTO v_tmp_sum_forta_tpo_registro     ,
             v_tmp_sum_forta_num_reg_detalle  ,
             v_tmp_sum_forta_tot_ap_forta     
        FROM safre_tmp:tmp_sum_forta
        
        --se insertan los registros a la tabla del suamrio 
       INSERT INTO  pag_sum_fc
                   (
                     folio            , 
                     tpo_registro     ,
                     num_reg_detalle  ,
                     tot_ap_fc    
                   )
            VALUES 
                   (   
                     p_folio     ,   
                     v_tmp_sum_forta_tpo_registro     ,
                     v_tmp_sum_forta_num_reg_detalle  ,
                     v_tmp_sum_forta_tot_ap_forta
                   );
   END FOREACH ;

   UPDATE STATISTICS FOR TABLE pag_cza_fc ;
   UPDATE STATISTICS FOR TABLE pag_det_fc ;
   UPDATE STATISTICS FOR TABLE pag_sum_fc ;
   
   --TRACE 'Finaliza el store procedure de registro historicos de FORTALECIMIENTO DE CRÉDITO';

   -- si no hubo error, se indica que el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración de FORTALECIMIENTO DE CRÉDITO terminó correctamente";
   
   RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_forta_nss;
END PROCEDURE;


