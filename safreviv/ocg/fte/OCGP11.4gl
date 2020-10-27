##########################################################################
#Modulo            => OCG                                                #
#Programa          => OCGP11                                             #
#Objetivo          => Programa que Actualiza la Entidad Financiera de un #
#                     crédito vigente en garantía 43 Bis.                #
#Autor             => Emilio Abarca Sánchez, EFP                         #
#Fecha inicio      => 2 SEPTIEMBRE 2016                                  #
##########################################################################

DATABASE safre_viv

   --Definiciòn de variables globales, parametros enviados del menu
   DEFINE g_usuario      CHAR(20)
   DEFINE
   g_tipo_proceso SMALLINT
   DEFINE g_nom_ventana  STRING

   --var´s para manipular componentes del formulario actual
   DEFINE v_ventana         ui.Window
   DEFINE v_forma           ui.Form

MAIN 
   -- se incorpora como parametros enviados desde el menu el proceso y codigo de la operaciòn
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_ventana  = ARG_VAL(3)

   -- se genera el archivo log en caso de Error
   CALL STARTLOG(g_usuario CLIPPED|| ".OCGP11.log")

    -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   CLOSE WINDOW SCREEN 
   CALL ingresa_nss_ocg()
   
END MAIN

FUNCTION  ingresa_nss_ocg()

   DEFINE e_nss                LIKE afi_derechohabiente.nss
   DEFINE bandera              BOOLEAN
   DEFINE v_ex_tbjdr           INTEGER
   DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente  
   DEFINE v_exist_43Bis        INTEGER

   OPEN WINDOW w1 WITH FORM "OCGP111"
      INPUT BY NAME e_nss ATTRIBUTE(UNBUFFERED, WITHOUT DEFAULTS)
      
         ON ACTION ACCEPT
             IF (LENGTH (e_nss) < 11) THEN
                CALL fn_mensaje("Información","El nss no debe ser menor a 11 dígitos ","about")
             ELSE 
                --se evalua que sea numerico
                LET bandera = fn_es_numerico(e_nss)

                IF(bandera == 1) THEN
                   CALL fn_mensaje("Información","El nss debe ser numérico","exclamation")
                ELSE 
                   --verifica que exista en la tabla afi_derechohabiente
                   SELECT COUNT(*) INTO v_ex_tbjdr  
                                   FROM afi_derechohabiente
                                   WHERE nss = e_nss
                   IF (v_ex_tbjdr == 0) THEN
                      CALL fn_mensaje("Información","El trabajador no existe","about")
                   ELSE
                      --obtenemos el id_derechohabiente
                      SELECT id_derechohabiente INTO v_id_derechohabiente 
                                                FROM afi_derechohabiente
                                                WHERE nss = e_nss;
                                                
                      -- verifica que tenga credito vigente 43 bis 
                      SELECT COUNT(*) INTO v_exist_43Bis 
                                      FROM ocg_formalizacion
                                      WHERE id_derechohabiente = v_id_derechohabiente AND 
                                            diagnostico = 1  AND --valido 
                                            estado      = 20 AND -- vigente
                                            situacion IN (55,60,70,80) --crédito aceptado vigente

                      IF(v_exist_43Bis == 0) THEN 
                         CALL fn_mensaje("Información","El trabajador no cuenta con crédito 43Bis vigente","about")
                      ELSE
                         CALL criterio_seleccion(e_nss,v_id_derechohabiente)  
                         LET e_nss = NULL 
                      END IF 
                   END IF  
                END IF 
            END IF 
            
         ON ACTION CANCEL
            EXIT INPUT
            
      END INPUT   
   CLOSE WINDOW w1

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

FUNCTION criterio_seleccion(p_nss,p_id_derechohabiente)

   DEFINE groupradio           STRING   --radiobutton
   DEFINE p_nss                LIKE afi_derechohabiente.nss
   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE ind_portabilidad     SMALLINT 
   
OPEN WINDOW win2 WITH FORM "OCGP113"
   
   INPUT BY NAME groupradio ATTRIBUTE(UNBUFFERED)

      ON ACTION ACCEPT
         IF(groupradio = "rad1") THEN
            LET ind_portabilidad = 2 #Actualiza la EF por crédito vigente
         ELSE 
            IF(groupradio = "rad2") THEN 
               LET ind_portabilidad = 1  #Actualiza la EF financiera por portabilidad
            END IF       
         END IF 
         
         #Carga la función para Actualizar la EF por crédito vigente o por portabilidad
         #dependiendo del indicador ind_portabilidad
         CALL carga_inf_dh_ocg(p_nss,p_id_derechohabiente,ind_portabilidad)
         EXIT INPUT 

      ON ACTION CANCEL 
         EXIT INPUT 
   END INPUT 
CLOSE WINDOW win2

END FUNCTION 

FUNCTION carga_inf_dh_ocg(p_nss,p_id_derechohabiente,p_portabilidad)

   DEFINE p_nss                LIKE afi_derechohabiente.nss
   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE p_portabilidad       SMALLINT 
   DEFINE nva_ent_financiera   CHAR(67) --combobox
   DEFINE v_answer             BOOLEAN
 
   --var´s para obtener información que se muestra en el formulario OCGP112
   DEFINE v_ocg_formalizacion  LIKE ocg_formalizacion.id_ocg_formalizacion
   DEFINE v_id_ocgf_det        LIKE ocg_formalizacion.id_ocg_detalle
   DEFINE v_id_ocgf_tram       LIKE ocg_formalizacion.id_ocg_tramite
   DEFINE e_rfc                LIKE ocg_formalizacion.rfc
   DEFINE e_curp               LIKE ocg_formalizacion.curp
   DEFINE v_ap_paterno         LIKE ocg_formalizacion.ap_paterno
   DEFINE v_ap_materno         LIKE ocg_formalizacion.ap_materno
   DEFINE v_nombre             LIKE ocg_formalizacion.nombre
   DEFINE v_cve_ent_fin        LIKE ocg_formalizacion.cve_ent_financiera
   DEFINE v_desc_ent_fin       LIKE cat_entidad_financiera.ent_financiera_desc
   DEFINE e_tpo_credito        LIKE ocg_formalizacion.tpo_credito
   DEFINE v_estado             LIKE ocg_formalizacion.estado
   DEFINE v_estado_desc        LIKE cat_ocg_estado.edo_credito_desc
   DEFINE v_situacion          LIKE ocg_formalizacion.situacion
   DEFINE v_situacion_desc     LIKE cat_ocg_situacion.situacion_desc
   DEFINE e_num_ctr_interno    LIKE ocg_formalizacion.num_ctr_int_ef

   --var´s para concatenar 
   DEFINE e_nombre_dh      CHAR(126)
   DEFINE e_ent_financiera CHAR (67)
   DEFINE e_edo_credito    CHAR(50)
   DEFINE e_situacion      CHAR (47)

   --var´s conteo de registros
   DEFINE v_exist_ent_fin  INTEGER
   DEFINE v_exist_ocg_tmt  INTEGER 
   DEFINE v_exist_ocg_ug   INTEGER 
   
   --var´s que recuperan id´s insertar en historicos
   DEFINE v_id_ocg_tramite   LIKE ocg_tramite.id_ocg_tramite
   DEFINE v_id_ocgt_detalle  LIKE ocg_tramite.id_ocg_detalle
   DEFINE v_id_ocg_sol_ug    LIKE ocg_solicitud_uso_garantia.id_ocg_solicitud_ug
   DEFINE v_id_ocgug_det     LIKE ocg_solicitud_uso_garantia.id_ocg_detalle
   DEFINE v_aux_ctrl_int     LIKE ocg_formalizacion.num_ctr_int_ef
   
OPEN WINDOW w3 WITH FORM "OCGP112"
   --la información se obtiene de ocg_formalizacion
   SELECT ocgf.id_ocg_formalizacion,
          ocgf.id_ocg_detalle,
          ocgf.id_ocg_tramite,
          ocgf.rfc,
          ocgf.curp,
          ocgf.ap_paterno,
          ocgf.ap_materno,
          ocgf.nombre,
          ocgf.cve_ent_financiera,
          ent.ent_financiera_desc,
          ocgf.tpo_credito,
          ocgf.estado,
          edo.edo_credito_desc,
          ocgf.situacion,
          sit.situacion_desc,
          ocgf.num_ctr_int_ef
     INTO v_ocg_formalizacion,
          v_id_ocgf_det,
          v_id_ocgf_tram,
          e_rfc,
          e_curp,
          v_ap_paterno,
          v_ap_materno,
          v_nombre,
          v_cve_ent_fin,
          v_desc_ent_fin,
          e_tpo_credito,
          v_estado,
          v_estado_desc,
          v_situacion,
          v_situacion_desc,
          e_num_ctr_interno
     FROM ocg_formalizacion ocgf, 
          cat_entidad_financiera ent,
          cat_ocg_estado edo,
          cat_ocg_situacion sit
    WHERE id_derechohabiente = p_id_derechohabiente AND 
          ocgf.cve_ent_financiera = ent.cve_ent_financiera AND 
          ocgf.estado = edo.edo_credito  AND 
          ocgf.situacion = sit.situacion AND
          ocgf.estado = 20     AND 
          ocgf.diagnostico = 1 AND 
          ocgf.situacion IN (55,60,70,80);
              
   --concatena
   LET e_nombre_dh = v_ap_paterno CLIPPED ||" ",v_ap_materno CLIPPED||" ",v_nombre CLIPPED
   LET e_ent_financiera = v_cve_ent_fin || " - ",v_desc_ent_fin
   LET e_edo_credito = v_estado ||" - ", v_estado_desc
   LET e_situacion = v_situacion || " - ",v_situacion_desc
        
   INPUT BY NAME e_num_ctr_interno, nva_ent_financiera ATTRIBUTE(UNBUFFERED, WITHOUT DEFAULTS)
      BEFORE INPUT 
         LET v_aux_ctrl_int = e_num_ctr_interno   #Guarda en aux el número de control  que tenía originalmente
         DISPLAY BY NAME p_nss,
                         e_rfc,
                         e_curp,
                         e_nombre_dh,
                         e_ent_financiera,
                         e_tpo_credito,
                         e_edo_credito,
                         e_situacion
                           
         CALL carga_cbx_ent_financiera()
            
      ON ACTION ACCEPT
        IF (e_num_ctr_interno IS NULL ) THEN 
               CALL fn_mensaje("Información","El número de control interno no puede ser nulo","about")
        ELSE 
           IF (nva_ent_financiera IS NULL ) THEN
               CALL fn_mensaje("Información","La Entidad Financiera no puede ser nula","exclamation")
           ELSE 
               --se verifica que exista en el catalogo de entidades financieras
               SELECT COUNT(*) 
                  INTO v_exist_ent_fin
                  FROM cat_entidad_financiera
                 WHERE cve_ent_financiera = nva_ent_financiera
                               
               IF (v_exist_ent_fin == 0) THEN
                  CALL fn_mensaje("Información","La Entidad Financiera no existe en el catálogo de entidades financieras","exclamation")
               ELSE 
                  IF (v_cve_ent_fin == nva_ent_financiera) THEN
                     CALL fn_mensaje("Información","No se puede realizar la actualización con la misma entidad financiera","exclamation") 
                  ELSE 
                     LET v_answer = fn_ventana_confirma("Información","¿Esta seguro que desea actualizar este registro?","question")

                     IF (v_answer == 0) THEN 
                        CALL fn_mensaje("Información","Se ha cancelado la actualización","about")
                        EXIT INPUT 
                     ELSE 
                        --Actualiza en *ocg_formalizacion*
                        UPDATE ocg_formalizacion
                           SET cve_ent_financiera = nva_ent_financiera,
                               num_ctr_int_ef     = e_num_ctr_interno
                         WHERE id_ocg_formalizacion = v_ocg_formalizacion
                           AND id_derechohabiente = p_id_derechohabiente 
                           AND cve_ent_financiera = v_cve_ent_fin
                           AND num_ctr_int_ef = v_aux_ctrl_int
                           AND diagnostico = 1  
                           AND estado      = 20 
                           AND situacion IN (55,60,70,80); 
                                  
                        ---Actualiza *ocg_detalle*
                        UPDATE ocg_detalle
                           SET cve_ent_financiera = nva_ent_financiera
                         WHERE id_ocg_detalle     = v_id_ocgf_det
                           AND subproceso         = 2;

                        --Inserta en *ocg_his_formalizacion* del registro que se actualizo
                        INSERT INTO ocg_his_formalizacion
                                             (id_ocg_formalizacion,
                                             id_derechohabiente,
                                             cve_ent_financiera,
                                             num_ctr_int_ef,
                                             f_proceso,
                                             usuario)
                                      VALUES(v_ocg_formalizacion,
                                             p_id_derechohabiente,
                                             v_cve_ent_fin, #(original)
                                             v_aux_ctrl_int,#(original)
                                             TODAY,
                                             g_usuario); 
                                 
                        --Busca que existe en *ocg_tramite*
                        SELECT COUNT(*) INTO v_exist_ocg_tmt
                                        FROM ocg_tramite 
                                        WHERE id_derechohabiente = p_id_derechohabiente
                                          AND id_ocg_tramite     = v_id_ocgf_tram 
                                          AND cve_ent_financiera = v_cve_ent_fin
                                          AND diagnostico = 1 
                                          AND estado      = 20
                                          AND situacion IN (55,60,70,80);
                                           
                        IF (v_exist_ocg_tmt == 1) THEN
                           --Actualiza en *ocg_tramite*
                           UPDATE ocg_tramite
                               SET cve_ent_financiera = nva_ent_financiera
                               WHERE id_derechohabiente = p_id_derechohabiente 
                                 AND id_ocg_tramite     = v_id_ocgf_tram 
                                 AND cve_ent_financiera = v_cve_ent_fin 
                                 AND diagnostico = 1 
                                 AND estado      = 20  
                                 AND situacion IN (55,60,70,80);

                           --Recupera id_ocg_tramite
                           SELECT id_ocg_tramite,
                                  id_ocg_detalle 
                             INTO v_id_ocg_tramite,
                                  v_id_ocgt_detalle
                             FROM ocg_tramite 
                            WHERE id_derechohabiente = p_id_derechohabiente
                              AND id_ocg_tramite     = v_id_ocgf_tram
                              AND diagnostico = 1 
                              AND estado      = 20
                              AND situacion IN (55,60,70,80);

                           --Actualiza en ocg_detalle
                           UPDATE ocg_detalle
                              SET cve_ent_financiera = nva_ent_financiera
                            WHERE id_ocg_detalle = v_id_ocgt_detalle
                             AND  subproceso = 1;

                           -- inserta en *ocg_his_tramite* el registro que se actualizó
                           INSERT INTO ocg_his_tramite
                                           (id_ocg_tramite,
                                            cve_ent_financiera,
                                            id_derechohabiente,
                                            f_proceso,
                                            usuario)
                                    VALUES (v_id_ocg_tramite,
                                            v_cve_ent_fin,       #(original)
                                            p_id_derechohabiente,
                                            TODAY,
                                            g_usuario);
                        END IF 

                        --Busca en *ocg_solicitud_uso_garantia*
                        SELECT COUNT(*)
                           INTO v_exist_ocg_ug 
                           FROM ocg_solicitud_uso_garantia
                          WHERE id_derechohabiente   = p_id_derechohabiente
                            AND id_ocg_formalizacion = v_ocg_formalizacion
                            AND cve_ent_financiera   = v_cve_ent_fin 
                            AND num_ctr_int_ef       = v_aux_ctrl_int
                            AND diagnostico = 1
                            AND estado      = 20
                            AND situacion IN (50,90);

                        IF (v_exist_ocg_ug >= 1) THEN 
                    
                          --Actualiza *ocg_solicitud_uso_garantia*
                          UPDATE ocg_solicitud_uso_garantia
                              SET cve_ent_financiera   = nva_ent_financiera,
                                  num_ctr_int_ef       = e_num_ctr_interno
                            WHERE id_derechohabiente   = p_id_derechohabiente
                              AND id_ocg_formalizacion = v_ocg_formalizacion
                              AND cve_ent_financiera   = v_cve_ent_fin 
                              AND num_ctr_int_ef = v_aux_ctrl_int
                              AND diagnostico    = 1
                              AND estado         = 20    
                              AND situacion IN (50,90);

                           --Recupera el id_ocg_solicitud_ug de los dh que se actualizaron
                           DECLARE cur_ocg_ug CURSOR FOR SELECT id_ocg_solicitud_ug,
                                                                id_ocg_detalle
                                                         FROM ocg_solicitud_uso_garantia
                                                         WHERE id_derechohabiente   = p_id_derechohabiente
                                                           AND id_ocg_formalizacion = v_ocg_formalizacion  
                                                           AND num_ctr_int_ef = e_num_ctr_interno
                                                           AND diagnostico  = 1
                                                           AND estado       = 20 
                                                           AND situacion IN (50,90);

                           FOREACH cur_ocg_ug INTO v_id_ocg_sol_ug, 
                                                   v_id_ocgug_det

                               IF (v_id_ocgug_det IS NOT NULL) THEN

                                 UPDATE ocg_detalle
                                    SET cve_ent_financiera = nva_ent_financiera
                                  WHERE id_ocg_detalle     = v_id_ocgug_det
                                   AND  subproceso         = 3;

                               END IF 
                        
                               --inserta en historico *ocg_his_solic_uso_garantia*
                               INSERT INTO ocg_his_solic_uso_garantia
                                                       (id_ocg_solicitud_ug,
                                                        id_derechohabiente,
                                                        cve_ent_financiera,
                                                        num_ctr_int_ef,
                                                        f_proceso,
                                                        usuario)
                                                VALUES (v_id_ocg_sol_ug,--id_ocg_solicitud_ug
                                                        p_id_derechohabiente,
                                                        v_cve_ent_fin,  #(original)
                                                        v_aux_ctrl_int, #(Original)
                                                        TODAY,
                                                        g_usuario);
                           END FOREACH 
                        END IF 

                        
                        #guarda en ocg_portabilidad de acuerdo al indicador del porqué se actualizó la EF
                        # 1-Portabilidad
                        # 2-Actualización el Línea
                        # 3-Actualización por archivo
                        
                           INSERT INTO ocg_portabilidad
                                                (id_derechohabiente,
                                                 id_ocg_tramite,
                                                 id_ocg_formalizacion,
                                                 nss,
                                                 cve_ent_fin_anterior,
                                                 cve_ent_fin_actual,
                                                 ind_actualiza,
                                                 f_actualiza,
                                                 usuario)
                                          VALUES(p_id_derechohabiente,
                                                 v_id_ocgf_tram,
                                                 v_ocg_formalizacion,
                                                 p_nss,
                                                 v_cve_ent_fin,
                                                 nva_ent_financiera,
                                                 p_portabilidad,   -- Indicador del cambio de EF
                                                 TODAY,
                                                 g_usuario); 
                                                 
                        IF (SQLCA.SQLCODE == 0) THEN
                           CALL fn_mensaje("Información","La actualización se ha realizado correctamente","about")
                           EXIT INPUT
                        ELSE 
                           CALL fn_mensaje("Información","No se pudo realizar la actualización","stop")
                           EXIT INPUT 
                        END IF
                        
                     END IF
                  END IF  
               END IF 
            END IF
        END IF       
        
      END INPUT
      
CLOSE WINDOW w3
   
END FUNCTION 

FUNCTION carga_cbx_ent_financiera()

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
