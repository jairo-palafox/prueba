DATABASE safre_viv

FUNCTION fn_genera_salida_rch_op29(p_folio)
DEFINE p_folio    LIKE sep_cza_op29.folio
DEFINE r_cza_op29 RECORD LIKE sep_cza_op29.*
DEFINE r_s_cza_op29 RECORD
          tipo_registro       LIKE sep_cza_op29.tipo_registro   ,
          id_servicio         LIKE sep_cza_op29.id_servicio     ,
          id_operacion        LIKE sep_cza_op29.id_operacion    ,
          tipo_ent_origen     LIKE sep_cza_op29.tipo_ent_origen ,
          cve_ent_origen      LIKE sep_cza_op29.cve_ent_origen  ,
          tipo_ent_destino    LIKE sep_cza_op29.tipo_ent_destino,
          cve_ent_destino     LIKE sep_cza_op29.cve_ent_destino,
          f_trans_lote        CHAR(8),
          consecutivo_dia     CHAR(3),
          resultado_operacion LIKE sep_cza_op29.resultado_operacion,
          motivo_rch          CHAR(9),
          filler              CHAR(262)
       END RECORD
--DEFINE r_det_02_op29 RECORD LIKE sep_det_02_op29.*
DEFINE r_det_02_op29 RECORD --LIKE sep_det_02_op29.*
          id_det_02_op29      LIKE sep_det_02_op29.id_det_02_op29,
          tipo_registro       LIKE sep_det_02_op29.tipo_registro,
          contador_servicio   LIKE sep_det_02_op29.contador_servicio,
          invadido            LIKE sep_det_02_op29.invadido,
          resultado_operacion LIKE sep_det_02_op29.resultado_operacion
       END RECORD
DEFINE r_s_det02_op29 RECORD
          tipo_registro                 LIKE sep_det_02_op29.tipo_registro              ,
          contador_servicio             CHAR(10),
          invadido                      LIKE sep_det_02_op29.invadido                   ,
          resultado_operacion           LIKE sep_det_02_op29.resultado_operacion        ,
          motivo_rch                    CHAR(9),
          filler                        CHAR(266)
	     END RECORD
DEFINE r_det_03_op29 RECORD LIKE sep_det_03_op29.*
DEFINE r_s_det03_op29 RECORD
          tipo_registro                 LIKE sep_det_03_op29.tipo_registro              ,
          contador_servicio             CHAR(10),
          asociado                      LIKE sep_det_03_op29.asociado                   ,
          resultado_operacion           LIKE sep_det_03_op29.resultado_operacion        ,
          motivo_rch                    CHAR(9),
          filler                        CHAR(266)
       END RECORD
DEFINE r_det_05_op29 RECORD LIKE sep_det_05_op29.*
DEFINE r_s_det05_op29 RECORD
          tipo_registro                 LIKE sep_det_02_op29.tipo_registro              ,
          contador_servicio             CHAR(10),
          nrp                           LIKE sep_det_02_op29.invadido                   ,
          resultado_operacion           LIKE sep_det_02_op29.resultado_operacion        ,
          motivo_rch                    CHAR(9),
          filler                        CHAR(266)
       END RECORD
DEFINE r_det_06_op29 RECORD LIKE sep_det_06_op29.*
DEFINE r_s_det06_op29 RECORD
          tipo_registro                 LIKE sep_det_02_op29.tipo_registro              ,
          contador_servicio             CHAR(10),
          nrp                           LIKE sep_det_02_op29.invadido                   ,
          resultado_operacion           LIKE sep_det_02_op29.resultado_operacion        ,
          motivo_rch                    CHAR(9),
          filler                        CHAR(266)
       END RECORD
DEFINE r_sum_op29 RECORD LIKE sep_sum_op29.*
DEFINE r_s_sum_op29 RECORD
          tipo_registro        CHAR(2),
          total_registro_det2  CHAR(10),
          total_registro_det5  CHAR(10),
          total_registro_det3  CHAR(10),
          total_registro_det6  CHAR(10),
          total_registros      CHAR(10),
          filler               CHAR(248)
       END RECORD
DEFINE v_s_registro  STRING
DEFINE v_c_ruta_env_sep LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
DEFINE v_v_nom_archivo  STRING
DEFINE v_ch_arch_solTransf  BASE.CHANNEL -- manejador de apuntador hacia archivo
DEFINE v_s_qryTxt       STRING
DEFINE v_ano          CHAR(4)
DEFINE v_mes          CHAR(2)
DEFINE v_dia          CHAR(2)
DEFINE v_fecha        CHAR(8)

   WHENEVER ERROR CONTINUE

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'sep'"

   PREPARE prp_slc_rutasSep1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutasSep1  INTO v_c_ruta_env_sep
   IF(SQLCA.SQLCODE <> 0)THEN
      RETURN FALSE
   END IF
   
   LET v_v_nom_archivo = v_c_ruta_env_sep CLIPPED || "/S" || p_folio || "rechazo_op29.sep"
   
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   DISPLAY "Archivo generado: ", v_v_nom_archivo

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_nom_archivo, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("") 

   LET v_s_qryTxt = " SELECT UNIQUE a.*",
                    "   FROM sep_cza_op29 a, sep_det_02_op29 b",
                    "  WHERE a.folio = b.folio",
                    "    AND estado = 10"
   
   PREPARE EnuCzaOp29 FROM v_s_qryTxt
   DECLARE CurCzaOp29 CURSOR FOR EnuCzaOp29
   	
   FOREACH CurCzaOp29 INTO r_cza_op29.*
   	
      LET r_s_cza_op29.tipo_registro       = r_cza_op29.tipo_registro      
      LET r_s_cza_op29.id_servicio         = r_cza_op29.id_servicio        
      LET r_s_cza_op29.id_operacion        = r_cza_op29.id_operacion       
      LET r_s_cza_op29.tipo_ent_origen     = r_cza_op29.tipo_ent_origen    
      LET r_s_cza_op29.cve_ent_origen      = r_cza_op29.cve_ent_origen     
      LET r_s_cza_op29.tipo_ent_destino    = r_cza_op29.tipo_ent_destino   
      LET r_s_cza_op29.cve_ent_destino     = r_cza_op29.cve_ent_destino   
      LET v_ano = YEAR(r_cza_op29.f_trans_lote)
      LET v_mes = MONTH(r_cza_op29.f_trans_lote)
      LET v_dia = DAY(r_cza_op29.f_trans_lote)
      LET v_fecha = v_ano USING "&&&&"
      LET v_fecha = v_fecha CLIPPED, v_mes USING "&&"
      LET v_fecha = v_fecha CLIPPED, v_dia USING "&&"
      LET r_s_cza_op29.f_trans_lote        = v_fecha
      LET r_s_cza_op29.consecutivo_dia     = r_cza_op29.consecutivo_dia USING "&&&"
      LET r_s_cza_op29.resultado_operacion = r_cza_op29.resultado_operacion
      LET r_s_cza_op29.motivo_rch          = '000000000' -- TMP definir origen
      
      LET v_s_registro = r_s_cza_op29.*
      CALL v_ch_arch_solTransf.write([v_s_registro])
   	
   	  LET v_s_qryTxt = "SELECT ",
                       " id_det_02_op29        ,",
                       " tipo_registro         ,",
                       " contador_servicio     ,",
                       " invadido              ,",
                       " resultado_operacion   ",
   	                   "  FROM sep_det_02_op29",
   	                   " WHERE folio = ? ",
   	                   "   AND estado = 10"


      PREPARE Enudet02Op29 FROM v_s_qryTxt
      DECLARE Curdet02Op29 CURSOR FOR Enudet02Op29
      	
      FOREACH Curdet02Op29 USING r_cza_op29.folio 
                            INTO r_det_02_op29.id_det_02_op29,
                                r_det_02_op29.tipo_registro,
                                r_det_02_op29.contador_servicio,
                                r_det_02_op29.invadido,
                                r_det_02_op29.resultado_operacion
                                

         LET r_s_det02_op29.tipo_registro       = r_det_02_op29.tipo_registro       
         LET r_s_det02_op29.contador_servicio   = r_det_02_op29.contador_servicio USING "&&&&&&&&&&"
         LET r_s_det02_op29.invadido            = r_det_02_op29.invadido            
         LET r_s_det02_op29.resultado_operacion = r_det_02_op29.resultado_operacion 
         LET r_s_det02_op29.motivo_rch          = '000000000' -- tmp por definir
         
         LET v_s_registro = r_s_det02_op29.*
         CALL v_ch_arch_solTransf.write([v_s_registro])
      	
      	 LET v_s_qryTxt = "SELECT *",
      	                  "  FROM sep_det_05_op29",
      	                  " WHERE id_det_02_op29 = ",r_det_02_op29.id_det_02_op29
      	  
         PREPARE Enudet05Op29 FROM v_s_qryTxt
         DECLARE Curdet05Op29 CURSOR FOR Enudet05Op29
         	
         FOREACH Curdet05Op29 INTO r_det_05_op29.*
      	 
            LET r_s_det05_op29.tipo_registro       = r_det_05_op29.tipo_registro      
            LET r_s_det05_op29.contador_servicio   = r_det_05_op29.contador_servicio USING "&&&&&&&&&&"
            LET r_s_det05_op29.nrp                 = r_det_05_op29.nrp                
            LET r_s_det05_op29.resultado_operacion = r_det_05_op29.resultado_operacion
            LET r_s_det05_op29.motivo_rch          = '000000000' --tmp por definir
      	 
            LET v_s_registro = r_s_det05_op29.*
            CALL v_ch_arch_solTransf.write([v_s_registro])
      	 
      	 END FOREACH
         IF(SQLCA.SQLCODE <> 0)THEN
            RETURN FALSE
         END IF
      	 LET v_s_qryTxt = "SELECT *",
      	                  "  FROM sep_det_03_op29",
      	                  " WHERE id_det_02_op29 = ",r_det_02_op29.id_det_02_op29
      	  
         PREPARE Enudet03Op29 FROM v_s_qryTxt
         EXECUTE Enudet03Op29 INTO r_det_03_op29.*
         
         LET r_s_det03_op29.tipo_registro       = r_det_03_op29.tipo_registro      
         LET r_s_det03_op29.contador_servicio   = r_det_03_op29.contador_servicio USING "&&&&&&&&&&"
         LET r_s_det03_op29.asociado            = r_det_03_op29.asociado           
         LET r_s_det03_op29.resultado_operacion = r_det_03_op29.resultado_operacion
         LET r_s_det03_op29.motivo_rch          = '000000000' -- tmp por definir

         LET v_s_registro = r_s_det03_op29.*
         CALL v_ch_arch_solTransf.write([v_s_registro])

      	 LET v_s_qryTxt = "SELECT *",
      	                  "  FROM sep_det_06_op29",
      	                  " WHERE id_det_03_op29 = ",r_det_03_op29.id_det_03_op29
      	  
         PREPARE Enudet06Op29 FROM v_s_qryTxt
         DECLARE Curdet06Op29 CURSOR FOR Enudet06Op29
         	
         FOREACH Curdet06Op29 INTO r_det_06_op29.*
      	 
            LET r_s_det06_op29.tipo_registro       = r_det_06_op29.tipo_registro      
            LET r_s_det06_op29.contador_servicio   = r_det_06_op29.contador_servicio USING "&&&&&&&&&&"
            LET r_s_det06_op29.nrp                 = r_det_06_op29.nrp                
            LET r_s_det06_op29.resultado_operacion = r_det_06_op29.resultado_operacion
            LET r_s_det06_op29.motivo_rch          = '000000000' --tmp por definir
      	 
            LET v_s_registro = r_s_det06_op29.*
            CALL v_ch_arch_solTransf.write([v_s_registro])
      	 
      	 END FOREACH
         IF(SQLCA.SQLCODE <> 0)THEN
            RETURN FALSE
         END IF
      END FOREACH
      IF(SQLCA.SQLCODE <> 0)THEN
         RETURN FALSE
      END IF
   END FOREACH
   IF(SQLCA.SQLCODE <> 0)THEN
      RETURN FALSE
   END IF
   
   LET v_s_qryTxt = " SELECT *",
                    "   FROM sep_sum_op29",
                    "  WHERE folio = ", p_folio
   
   PREPARE EnuSumOp29 FROM v_s_qryTxt
   EXECUTE EnuSumOp29 INTO r_sum_op29.*
   IF(SQLCA.SQLCODE <> 0)THEN
      RETURN FALSE
   END IF
   LET r_s_sum_op29.tipo_registro       = r_sum_op29.tipo_registro USING "&&"
   LET r_s_sum_op29.total_registro_det2 = r_sum_op29.total_registro_det2 USING "&&&&&&&&&&"
   LET r_s_sum_op29.total_registro_det5 = r_sum_op29.total_registro_det5 USING "&&&&&&&&&&"
   LET r_s_sum_op29.total_registro_det3 = r_sum_op29.total_registro_det3 USING "&&&&&&&&&&"
   LET r_s_sum_op29.total_registro_det6 = r_sum_op29.total_registro_6 USING "&&&&&&&&&&"
   LET r_s_sum_op29.total_registros     = r_sum_op29.total_registro USING "&&&&&&&&&&"

   LET v_s_registro = r_s_sum_op29.*
   CALL v_ch_arch_solTransf.write([v_s_registro])
       
   -- se cierra el manejador de escritura
   CALL v_ch_arch_solTransf.close()
   
   RETURN TRUE
   
END FUNCTION