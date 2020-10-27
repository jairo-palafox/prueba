##########################################################################
#Modulo            => OCG                                                #
#Programa          => OCGP24                                             #
#Objetivo          => Programa que republica un crédito del subproceso 5 #
#Autor             => Emilio Abarca Sánchez, EFP                         #
#Fecha inicio      => 05/ABRIL/2017                                      #
##########################################################################

DATABASE safre_viv

   DEFINE g_usuario      CHAR(20)
   DEFINE g_tipo_proceso SMALLINT
   DEFINE g_nom_ventana  STRING

MAIN
   -- Se reciben parámetros eviados por el menú
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_ventana  = ARG_VAL(3)

   CALL STARTLOG(g_usuario CLIPPED|| ".OCGP24.log")

   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF


   CLOSE WINDOW SCREEN  
   CALL republica_sp_005() 
   
END MAIN

FUNCTION republica_sp_005()

   DEFINE v_nss                 LIKE afi_derechohabiente.nss
   DEFINE ind_numerico          BOOLEAN
   DEFINE v_exist_dh            INTEGER
   DEFINE v_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_exist_lq            INTEGER
   DEFINE v_exist_rep           INTEGER 
   
   OPEN WINDOW vtn1 WITH FORM "OCGP241"
      INPUT BY NAME v_nss ATTRIBUTE (UNBUFFERED)

         ON ACTION ACCEPT 
            IF (LENGTH (v_nss) < 11) THEN
                CALL fn_mensaje("Información","El nss no debe ser menor a 11 dígitos ","about")
            ELSE
               LET ind_numerico = fn_es_numerico(v_nss)
               IF(ind_numerico = 1) THEN
                  CALL fn_mensaje("Información","El nss debe ser numérico","exclamation")
               ELSE 
                  --verifica que exista en la tabla afi_derechohabiente
                  SELECT COUNT(*) 
                     INTO v_exist_dh 
                     FROM afi_derechohabiente
                    WHERE nss = v_nss;

                  IF(v_exist_dh = 0) THEN
                     CALL fn_mensaje("Información","El trabajador no existe","about")
                  ELSE 
                     -- Recupera id_derechohabiente
                     SELECT id_derechohabiente
                        INTO v_id_derechohabiente 
                        FROM afi_derechohabiente
                       WHERE nss = v_nss;
                       
                     --Se verifica que sea un crédito liquidado.
                     SELECT COUNT(*)
                        INTO v_exist_lq
                        FROM ocg_liquidacion
                       WHERE id_derechohabiente = v_id_derechohabiente
                         AND diagnostico = 1   
                         AND situacion IN (160,155)
                         AND tpo_credito IN ('7','8') 
                         
                     IF (v_exist_lq = 0) THEN
                         CALL fn_mensaje("","Producto diferente a Cofinavit","")
                     ELSE 
                        -- Verifica que no exista un crédito ya publicado
                        SELECT COUNT(*)
                           INTO v_exist_rep
                           FROM ocg_liquidacion
                          WHERE id_derechohabiente = v_id_derechohabiente
                            AND diagnostico = 1
                            AND situacion IN (158,153)
                            AND tpo_credito IN ('7','8')

                        IF (v_exist_rep >= 1) THEN
                           CALL fn_mensaje("","Ya exite un crédito liquidado republicado para este trabajador","")
                        ELSE 
                           --Carga información del derechohabiente 
                           CALL carga_informacion(v_nss,v_id_derechohabiente)
                           LET v_nss = NULL 
                        END IF 
                     END IF 
                  END IF 
               END IF 
            END IF
            
         ON ACTION CANCEL 
            EXIT INPUT 
            
      END INPUT 
      
   CLOSE WINDOW vtn1
   
END FUNCTION 

FUNCTION fn_es_numerico(p_cadena)

   DEFINE p_cadena   STRING 
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN 

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE 
         LET indicador = 1
         IF(indicador == 1) THEN
            EXIT FOR 
         END IF 
      END IF 
   END FOR 

   RETURN indicador 

END FUNCTION 

FUNCTION carga_informacion(p_nss,p_id_derechohabiente)

   DEFINE p_nss                LIKE afi_derechohabiente.nss
   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE nva_ent_financiera   CHAR(67) --combobox
   DEFINE v_answer             BOOLEAN
   DEFINE v_id_ocg_lq          DECIMAL(9,0)
   DEFINE v_id_ocg_det         DECIMAL(9,0)
   DEFINE v_id_ocg_fz          DECIMAL(9,0)
   DEFINE v_id_ocg_tmt         DECIMAL(9,0)
   
   --var´s para recuperar información de ocg_tramite
   DEFINE v_id_ocg_tramite     LIKE ocg_tramite.id_ocg_tramite
   DEFINE v_id_ocg_detalle     LIKE ocg_tramite.id_ocg_detalle
   DEFINE v_rfc                LIKE ocg_tramite.rfc
   DEFINE v_curp               LIKE ocg_tramite.curp
   DEFINE v_ap_paterno         LIKE ocg_tramite.ap_paterno
   DEFINE v_ap_materno         LIKE ocg_tramite.ap_materno
   DEFINE v_nombre             LIKE ocg_tramite.nombre
   DEFINE v_cve_ef             LIKE ocg_tramite.cve_ent_financiera
   DEFINE v_ef_desc            LIKE cat_entidad_financiera.ent_financiera_desc
   DEFINE v_estado             LIKE ocg_tramite.estado
   DEFINE v_estado_desc        LIKE cat_ocg_estado.edo_credito_desc
   DEFINE v_situacion          LIKE ocg_tramite.situacion
   DEFINE v_situacion_desc     LIKE cat_ocg_situacion.situacion_desc

   --var´s para concatenar 
   DEFINE v_c_nombre_dh        CHAR(126)
   DEFINE v_c_ent_financiera   CHAR(67)
   DEFINE v_c_edo_credito      CHAR(50)
   DEFINE v_c_situacion        CHAR(47)

   OPEN WINDOW vtn2 WITH FORM "OCGP242"

      -- Rcupera el crédito iquidado más reciente
      SELECT FIRST 1
             lq.id_ocg_liquidacion    ,
             lq.id_ocg_detalle        ,
             lq.id_ocg_formalizacion  ,
             lq.id_ocg_tramite        ,
             fz.rfc                   ,
             fz.curp                  ,
             fz.ap_paterno            ,
             fz.ap_materno            ,
             fz.nombre                ,
             lq.cve_ent_financiera    ,
             ent.ent_financiera_desc  ,
             lq.estado                ,
             edo.edo_credito_desc     ,
             lq.situacion             ,
             sit.situacion_desc
        INTO v_id_ocg_lq      ,
             v_id_ocg_det     ,
             v_id_ocg_fz      ,
             v_id_ocg_tmt     ,
             v_rfc            ,           
             v_curp           ,
             v_ap_paterno     ,
             v_ap_materno     ,
             v_nombre         ,
             v_cve_ef         ,
             v_ef_desc        ,
             v_estado         ,
             v_estado_desc    ,
             v_situacion      ,
             v_situacion_desc
        FROM ocg_liquidacion lq         ,
       OUTER ocg_formalizacion fz       ,
             cat_entidad_financiera ent ,
             cat_ocg_estado edo         ,
             cat_ocg_situacion sit
       WHERE lq.id_derechohabiente = p_id_derechohabiente
         AND lq.id_ocg_formalizacion = fz.id_ocg_formalizacion
         AND lq.cve_ent_financiera = ent.cve_ent_financiera
         AND lq.estado = edo.edo_credito
         AND lq.situacion = sit.situacion
         AND lq.diagnostico = 1   
         AND lq.situacion IN (160,155) 
         AND lq.tpo_credito IN ('7','8')
         ORDER BY lq.f_liberacion_gtia DESC;
        
      --concatena
      LET v_c_nombre_dh      = v_ap_paterno CLIPPED," ",v_ap_materno CLIPPED," ",v_nombre CLIPPED 
      LET v_c_ent_financiera = v_cve_ef CLIPPED || " - ", v_ef_desc CLIPPED 
      LET v_c_edo_credito    = v_estado CLIPPED || " - ", v_estado_desc CLIPPED 
      LET v_c_situacion      = v_situacion CLIPPED || " - ",v_situacion_desc CLIPPED 
         
      INPUT BY NAME nva_ent_financiera ATTRIBUTE(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE )

         BEFORE INPUT
            DISPLAY BY NAME p_nss
            DISPLAY v_rfc TO e_rfc
            DISPLAY v_curp TO e_curp
            DISPLAY v_c_nombre_dh TO e_nombre_dh
            DISPLAY v_c_situacion TO e_situacion
            DISPLAY v_c_edo_credito TO e_edo_credito
            DISPLAY v_c_ent_financiera TO e_ent_financiera

            --Carga combobox de la EF
            CALL carga_cbx_EF()   
    
         ON ACTION Republicar
            IF(nva_ent_financiera IS NULL) THEN 
               CALL fn_mensaje("Información","La Entidad financiera no puede ser nula","about")
               CONTINUE INPUT 
            ELSE 
               LET v_answer = fn_ventana_confirma("Información","¿Desea republicar este crédito liquidado?","about")
               IF(v_answer = 0) THEN
                  CALL fn_mensaje("Información","Se ha cancelado la operacion","about")
                  EXIT INPUT
               ELSE 
                  #Inicia actualización
                  IF(v_situacion = 160) THEN
                  
                     UPDATE ocg_liquidacion
                        SET situacion = 158,
                            estado    = 70,
                            cve_ent_financiera = nva_ent_financiera
                      WHERE id_ocg_liquidacion = v_id_ocg_lq
                        AND id_derechohabiente = p_id_derechohabiente
                        AND cve_ent_financiera = v_cve_ef
                        AND diagnostico = 1
                        AND situacion   = 160
                        AND tpo_credito IN ('7','8');
                        
                  END IF

                  IF(v_situacion = 155) THEN
                  
                     UPDATE ocg_liquidacion
                        SET situacion = 153,
                            estado    = 70,
                            cve_ent_financiera = nva_ent_financiera
                      WHERE id_ocg_liquidacion = v_id_ocg_lq
                        AND id_derechohabiente = p_id_derechohabiente
                        AND cve_ent_financiera = v_cve_ef
                        AND diagnostico = 1
                        AND situacion   = 155
                        AND tpo_credito IN ('7','8');
                        
                  END IF 
                  
                  #Limpia fecha de respuesta en ocg_fecha_mig del sp_005
                  UPDATE ocg_fecha_mig
                     SET f_respuesta = NULL 
                   WHERE id_ocg_referencia = v_id_ocg_lq
                     AND subproceso = 5;
                    
                  #Actualiza la EF en ocg_detalle
                  UPDATE ocg_detalle
                     SET cve_ent_financiera = nva_ent_financiera 
                   WHERE id_ocg_detalle = v_id_ocg_det       
                    AND  id_derechohabiente = p_id_derechohabiente
                    AND  subproceso = 5
                    AND  nss =  p_nss;

                  IF (SQLCA.SQLCODE = 0) THEN
                     CALL fn_mensaje("Información","La republicación se ha realizado correctamente","about")
                     EXIT INPUT
                  ELSE 
                     CALL fn_mensaje("Información","No se pudo realizar la republicación","stop")
                     EXIT INPUT 
                  END IF
               END IF
           END IF     

         ON ACTION CANCEL
            EXIT INPUT 
    
      END INPUT 
   CLOSE WINDOW vtn2
END FUNCTION 

FUNCTION carga_cbx_EF()

   DEFINE cbx_ent_fin     ui.ComboBox
   DEFINE v_cve_ent_fin   LIKE cat_entidad_financiera.cve_ent_financiera
   DEFINE v_ent_fin_desc  LIKE cat_entidad_financiera.ent_financiera_desc
   DEFINE v_concat_ent    CHAR (67) 

   LET cbx_ent_fin = ui.ComboBox.forName("nva_ent_financiera")

   DECLARE cur_ent_fin CURSOR FOR
                       SELECT cve_ent_financiera, ent_financiera_desc
                       FROM cat_entidad_financiera
                       WHERE estado_ef = 10
                       ORDER BY cve_ent_financiera;

    CALL cbx_ent_fin.clear()

    FOREACH cur_ent_fin INTO v_cve_ent_fin, v_ent_fin_desc
        LET v_concat_ent = v_cve_ent_fin || " - ",v_ent_fin_desc
        CALL cbx_ent_fin.addItem(v_cve_ent_fin, v_concat_ent)
    END FOREACH 


END FUNCTION 
