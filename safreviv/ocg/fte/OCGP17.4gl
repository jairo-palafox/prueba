##########################################################################
#Modulo            => OCG                                                #
#Programa          => OCGP17                                             #
#Objetivo          => Programa que republica un crédito con solicitud de #
#                     trámite aceptado.                                  #
#Autor             => Emilio Abarca Sánchez, EFP                         #
#Fecha inicio      => 22 NOVIEMBRE 2016                                  #
##########################################################################

DATABASE safre_viv

   --Definiciòn de variables globales, parametros enviados del menu
   DEFINE g_usuario      CHAR(20)
   DEFINE g_tipo_proceso SMALLINT
   DEFINE g_nom_ventana  STRING

MAIN
   -- se incorpora como parametros enviados desde el menu el proceso y codigo de la operaciòn
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_ventana  = ARG_VAL(3)

   
   -- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".OCGP17.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF


   CLOSE WINDOW SCREEN  
   CALL ocg_principal() 
   
END MAIN

FUNCTION ocg_principal()

   DEFINE v_nss                 LIKE afi_derechohabiente.nss
   DEFINE ind_numerico          BOOLEAN
   DEFINE v_exist_dh            INTEGER
   DEFINE v_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_exist_tramite       INTEGER
   DEFINE v_get_id_ocg_tmt      LIKE ocg_tramite.id_ocg_tramite
   DEFINE v_exist_frm           INTEGER 
   
   OPEN WINDOW vtn1 WITH FORM "OCGP171"
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
                     --Se obtiene el id_derechohabiente
                     SELECT id_derechohabiente
                        INTO v_id_derechohabiente 
                        FROM afi_derechohabiente
                       WHERE nss = v_nss;
                       
                     --Se verifica que sea un crédito con solicitud de trámite aceptado.
                     SELECT COUNT(*)
                        INTO v_exist_tramite
                        FROM ocg_tramite
                       WHERE id_derechohabiente = v_id_derechohabiente
                         AND diagnostico = 1   --Valido
                         AND estado IN (10,70)
                         AND situacion   = 50  --Aceptado
                         AND tpo_credito IN (7,8) ; --Tipo de crédito cofinavit.
                         
                     IF (v_exist_tramite = 0) THEN
                         CALL fn_mensaje("","Producto diferente a Cofinavit","")
                     ELSE 
                        --Recupera el id_ocg_tramite 
                        SELECT id_ocg_tramite
                           INTO v_get_id_ocg_tmt
                           FROM ocg_tramite
                          WHERE id_derechohabiente = v_id_derechohabiente
                            AND diagnostico = 1   --Valido
                            AND estado IN (10,70)
                            AND situacion   = 50  --Aceptado
                            AND tpo_credito IN (7,8);
                        
                        -- Verifica que no exista en ocg_formalizacion, ya que debe ser un crédito con solicitud de trámite
                        -- y no un crédito formalizado.
                        SELECT COUNT(*)
                           INTO v_exist_frm
                           FROM ocg_formalizacion
                          WHERE id_ocg_tramite = v_get_id_ocg_tmt;

                        IF (v_exist_frm <> 0) THEN
                           CALL fn_mensaje("Información","Este trabajador cuenta con un crédito vigente","about")
                        ELSE 
                           --Carga información del derechohabiente 
                           CALL carga_inf_dh_ocg(v_nss,v_id_derechohabiente)
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

FUNCTION carga_inf_dh_ocg(p_nss,p_id_derechohabiente)

   DEFINE p_nss                LIKE afi_derechohabiente.nss
   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE nva_ent_financiera   CHAR(67) --combobox
   DEFINE v_answer             BOOLEAN 
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

   OPEN WINDOW vtn2 WITH FORM "OCGP172"

      LET v_id_ocg_tramite = NULL
      
      --Se obtiene información de ocg_tramite
      SELECT tmt.id_ocg_tramite,
             tmt.id_ocg_detalle,
             tmt.rfc,
             tmt.curp,
             tmt.ap_paterno,
             tmt.ap_materno,
             tmt.nombre,
             tmt.cve_ent_financiera,
             ent.ent_financiera_desc,
             tmt.estado,
             edo.edo_credito_desc,
             tmt.situacion,
             sit.situacion_desc
        INTO v_id_ocg_tramite ,
             v_id_ocg_detalle ,
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
        FROM ocg_tramite tmt, 
             cat_entidad_financiera ent,
             cat_ocg_estado edo,
             cat_ocg_situacion sit
       WHERE id_derechohabiente     = p_id_derechohabiente
         AND tmt.cve_ent_financiera = ent.cve_ent_financiera
         AND tmt.estado          = edo.edo_credito
         AND tmt.situacion       = sit.situacion
         AND tmt.diagnostico     = 1    # valido
         AND tmt.estado IN (10,70)
         AND tmt.situacion       = 50   # Aceptado
         AND tmt.tpo_credito IN (7,8);

      --concatena
      LET v_c_nombre_dh      = v_ap_paterno CLIPPED," ",v_ap_materno CLIPPED," ",v_nombre CLIPPED 
      LET v_c_ent_financiera = v_cve_ef CLIPPED || " - ", v_ef_desc CLIPPED 
      LET v_c_edo_credito    = v_estado CLIPPED || " - ", v_estado_desc CLIPPED 
      LET v_c_situacion      = v_situacion CLIPPED || " - ", v_situacion_desc
         
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
               LET v_answer = fn_ventana_confirma("Información","¿Esta seguro de republicar este crédito?","about")
               IF(v_answer = 0) THEN
                  CALL fn_mensaje("Información","Republicación cancelada","about")
                  EXIT INPUT
               ELSE 
                  #Actualiza en ocg_tramite
                  UPDATE ocg_tramite
                     SET situacion = 30, --diagnostico aceptado
                         estado    = 70, --aceptado
                         cve_ent_financiera = nva_ent_financiera  --nva clave EF
                   WHERE id_ocg_tramite     = v_id_ocg_tramite
                    AND  id_ocg_detalle     = v_id_ocg_detalle
                    AND  id_derechohabiente = p_id_derechohabiente
                    AND  cve_ent_financiera = v_cve_ef
                    AND  diagnostico        = 1  # Válido
                    AND  estado IN (10,70)
                    AND  situacion          = 50 # Aceptado

                  
                  #Limpia fecha de respuesta en ocg_fecha_mig
                  UPDATE ocg_fecha_mig
                     SET f_respuesta = NULL 
                   WHERE id_ocg_referencia = v_id_ocg_tramite
                     AND subproceso = 1;
                    
                  #Actualiza la EF en ocg_detalle
                  UPDATE ocg_detalle
                     SET cve_ent_financiera = nva_ent_financiera --Nueva EF
                   WHERE id_ocg_detalle = v_id_ocg_detalle       
                    AND  id_derechohabiente = p_id_derechohabiente
                    AND  subproceso = 1
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
