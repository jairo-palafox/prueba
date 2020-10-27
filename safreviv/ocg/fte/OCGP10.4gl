###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGP10                                                  #
#Objetivo          => Programa que Actualiza el Numero de Control Interno del #
#                     Derechohabiente.                                        #
#Autor             => Emilio Abarca Sánchez                                   #
#Fecha inicio      => 16 AGOSTO 2016                                          #
###############################################################################
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

   -- se genera el archivo log
    CALL STARTLOG(g_usuario CLIPPED|| ".OCGP10.log")

     -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   CLOSE WINDOW SCREEN 

   CALL consulta_nss_ocg()
    
END MAIN 

FUNCTION consulta_nss_ocg()

   DEFINE v_nss                LIKE afi_derechohabiente.nss
   DEFINE v_bandera            BOOLEAN
   DEFINE v_exist_NSS          INTEGER 
   DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_exist_43Bis        INTEGER 

OPEN WINDOW win1 WITH FORM "OCGP101"

    INPUT BY NAME v_nss ATTRIBUTE(UNBUFFERED,WITHOUT DEFAULTS)
        
        ON ACTION ACCEPT
            IF (LENGTH (v_nss) < 11) THEN
                CALL fn_mensaje("Información","El nss no debe ser menor a 11 dígitos ","about")
            ELSE 
               --evalua que sea Numero
                LET v_bandera = fn_evalua_numero(v_nss)
                
                IF (v_bandera == 1) THEN
                    CALL fn_mensaje("Información","El nss debe ser numérico","about")
                ELSE 
                  --verifica que exista en la tabla afi_derechohabiente
                  SELECT COUNT(*) INTO v_exist_NSS  
                                  FROM afi_derechohabiente
                                  WHERE nss = v_nss
                                  
                     IF (v_exist_NSS == 0) THEN
                        CALL fn_mensaje("Información","El trabajador no existe","about")
                     ELSE 
                        --obtenemos el id_derechohabiente
                        SELECT id_derechohabiente INTO v_id_derechohabiente 
                                                  FROM afi_derechohabiente
                                                  WHERE nss = v_nss

                        -- verifica que tenga credito vigente 43 bis 
                        SELECT COUNT(*) INTO v_exist_43Bis 
                                        FROM ocg_formalizacion
                                        WHERE id_derechohabiente = v_id_derechohabiente AND 
                                              diagnostico = 1  AND --valido 
                                              estado      = 20 AND -- vigente
                                              situacion IN (55,60,70,80) --aceptados
                                              
                         IF(v_exist_43Bis == 0) THEN 
                            CALL fn_mensaje("Información","El trabajador no cuenta con crédito 43Bis vigente","about")
                         ELSE 
                            CALL carga_inf_ocg(v_nss,v_id_derechohabiente)
                            LET v_nss = NULL 
                         END IF 
                     END IF 
                END IF
           END IF 
          
        ON ACTION CANCEL 
            EXIT INPUT 
        
    END INPUT 
CLOSE WINDOW win1
END FUNCTION 


FUNCTION carga_inf_ocg(p_nss,p_id_derechohabiente)

   DEFINE p_nss                LIKE afi_derechohabiente.nss
   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE e_id_ocg_form        LIKE ocg_formalizacion.id_ocg_formalizacion
   DEFINE e_nss                LIKE afi_derechohabiente.nss
   DEFINE e_rfc                LIKE ocg_formalizacion.rfc
   DEFINE e_curp               LIKE ocg_formalizacion.curp
   DEFINE e_paterno            LIKE ocg_formalizacion.ap_paterno
   DEFINE e_materno            LIKE ocg_formalizacion.ap_materno
   DEFINE e_nombre             LIKE ocg_formalizacion.nombre
   DEFINE e_cve_ent_fin        LIKE ocg_formalizacion.cve_ent_financiera
   DEFINE e_ent_fin_desc       LIKE cat_entidad_financiera.ent_financiera_desc
   DEFINE e_tpo_credito        LIKE ocg_formalizacion.tpo_credito
   DEFINE e_estado             LIKE ocg_formalizacion.estado
   DEFINE e_edo_credito_desc   LIKE cat_ocg_estado.edo_credito_desc--descripcion del estado del credito
   DEFINE e_situacion          LIKE ocg_formalizacion.situacion
   DEFINE e_situacion_desc     LIKE cat_ocg_situacion.situacion_desc --descripciòn de la situaciòn
   DEFINE e_num_ctr_interno    LIKE ocg_formalizacion.num_ctr_int_ef
   DEFINE e_nvo_num_ctr_int    LIKE ocg_formalizacion.num_ctr_int_ef
   DEFINE v_ent_financiera     CHAR (67)--concatena la cve de la entidad y su descripción
   DEFINE v_edo_credito        CHAR(50)-- concatena el estado y su descripciòn
   DEFINE v_situacion_desc     CHAR (47)-- concatena la situciòn y su descripciòn
   DEFINE v_nombre_dh          CHAR(126)--concatena nombre y apellidos del dh
   DEFINE v_bandera            BOOLEAN

   --variables de conteo
   DEFINE v_ex_sol_ug          INTEGER
   DEFINE v_ex_dev             INTEGER 
   DEFINE v_ex_ctr_trans       INTEGER

   --variable que recupera el id_ocg_solicitud_ug 
   DEFINE v_id_ocg_sol_ug      LIKE ocg_solicitud_uso_garantia.id_ocg_solicitud_ug

   --variables que recuperan campos de la tabla ocg_crt_transaccion 
   DEFINE v_id_ocg_trans       LIKE ocg_ctr_transaccion.id_ocg_ctr_transaccion
   DEFINE v_id_ocg_form_ctr    LIKE ocg_ctr_transaccion.id_ocg_formalizacion
   DEFINE v_id_ref_cta         LIKE ocg_ctr_transaccion.id_referencia_cta

   --variables que recuperan campos de la tabla ocg_devolucion (pendiente por confirmar)  
   {DEFINE v_id_ocg_dev        LIKE ocg_devolucion.id_ocg_devolucion
   DEFINE v_id_ocg_det         LIKE ocg_devolucion.id_ocg_detalle
   DEFINE v_id_ocg_form        LIKE ocg_devolucion.id_ocg_formalizacion
   DEFINE v_id_ocg_tmt         LIKE ocg_devolucion.id_ocg_tramite}

   DEFINE v_nss                LIKE afi_derechohabiente.nss
   DEFINE v_answer             BOOLEAN 

OPEN WINDOW win2 WITH FORM "OCGP102"

        --la información se extrae de ocg_formalización
        SELECT ocgf.id_ocg_formalizacion,
               ocgf.rfc, 
               ocgf.curp,
               ocgf.ap_paterno,
               ocgf.ap_materno,
               ocgf.nombre, 
               ocgf.cve_ent_financiera,
               ent.ent_financiera_desc,
               ocgf.tpo_credito, 
               ocgf.estado,
               est.edo_credito_desc,
               ocgf.situacion,
               sit.situacion_desc,
               ocgf.num_ctr_int_ef
          INTO e_id_ocg_form,
               e_rfc,
               e_curp,
               e_paterno, 
               e_materno, 
               e_nombre, 
               e_cve_ent_fin,
               e_ent_fin_desc,
               e_tpo_credito, 
               e_estado, 
               e_edo_credito_Desc,
               e_situacion,
               e_situacion_desc,
               e_num_ctr_interno
          FROM ocg_formalizacion ocgf, cat_entidad_financiera ent ,cat_ocg_estado est,cat_ocg_situacion sit
          WHERE id_derechohabiente = p_id_derechohabiente AND 
                ocgf.cve_ent_financiera = ent.cve_ent_financiera AND
                ocgf.estado = est.edo_credito AND
                ocgf.situacion = sit.situacion AND 
                ocgf.estado = 20 AND 
                ocgf.diagnostico = 1 AND 
                ocgf.situacion IN (55,60,70,80); 
                
        LET v_nombre_dh = e_paterno CLIPPED ||" ",e_materno CLIPPED ||" ",e_nombre CLIPPED 
        LET v_ent_financiera = e_cve_ent_fin ||" - ",e_ent_fin_desc
        LET v_edo_credito = e_estado ||" - ",e_edo_credito_Desc
        LET v_situacion_desc = e_situacion ||" - ", e_situacion_desc
    
    INPUT BY NAME e_nvo_num_ctr_int ATTRIBUTE(UNBUFFERED)
        BEFORE INPUT 
            DISPLAY p_nss TO e_nss
            DISPLAY BY NAME e_rfc,
                            e_curp,
                            v_nombre_dh,
                            v_ent_financiera,--e_cve_ent_fin,
                            e_tpo_credito, 
                            v_edo_credito,
                            v_situacion_desc,
                            e_num_ctr_interno

        ON ACTION ACCEPT 
           IF (e_nvo_num_ctr_int) IS NULL THEN
              CALL fn_mensaje("Información","El numero de control interno no puede ser nulo","about")
           ELSE 
              LET v_answer = fn_ventana_confirma("Información","¿Esta seguro que desea actualizar este registro?","question")
                  
               IF(v_answer == 0) THEN
                  CALL fn_mensaje("Información","Se ha cancelado la actualización","about")
                  EXIT INPUT
               ELSE 
                  --se actualiza el campo num_ctr_int_ef de la tabla **ocg_formalizacion**
                   UPDATE ocg_formalizacion
                      SET num_ctr_int_ef = e_nvo_num_ctr_int
                    WHERE id_derechohabiente = p_id_derechohabiente AND 
                          cve_ent_financiera = e_cve_ent_fin  AND
                          num_ctr_int_ef = e_num_ctr_interno  AND  
                          diagnostico = 1 AND
                          estado = 20     AND 
                          situacion IN (55,60,70,80);  

                   -- guarda un registro en el historico en la tabla **ocg_his_formalizacon**
                   INSERT INTO ocg_his_formalizacion 
                                            (id_ocg_formalizacion,
                                             id_derechohabiente,
                                             cve_ent_financiera,
                                             num_ctr_int_ef,
                                             f_proceso,
                                             usuario) 
                                     VALUES (e_id_ocg_form,-- id_ocg_formalizacion
                                             p_id_derechohabiente, --id_derechohabiente
                                             e_cve_ent_fin, -- clave entidad financiera
                                             e_num_ctr_interno, -- numero control interno(antiguo)
                                             TODAY,
                                             g_usuario);

                   -- Se verifica que exista en la tabla **ocg_solicitud_uso_garantia**
                   SELECT COUNT(*) INTO v_ex_sol_ug 
                                   FROM ocg_solicitud_uso_garantia 
                                  WHERE id_derechohabiente = p_id_derechohabiente AND 
                                        cve_ent_financiera = e_cve_ent_fin AND 
                                        num_ctr_int_ef = e_num_ctr_interno AND 
                                        diagnostico  = 1  AND 
                                        estado       = 20 AND 
                                        situacion IN (50,90)
                    
                       IF (v_ex_sol_ug >= 1 ) THEN
                                                                                            
                            --Actualiza en **ocg_solicitud_uso_garantia**
                            UPDATE ocg_solicitud_uso_garantia
                                    SET num_ctr_int_ef = e_nvo_num_ctr_int
                                    WHERE id_derechohabiente = p_id_derechohabiente AND 
                                          cve_ent_financiera = e_cve_ent_fin AND
                                          num_ctr_int_ef = e_num_ctr_interno AND 
                                          diagnostico  = 1  AND 
                                          estado       = 20 AND 
                                          situacion IN (50,90);

                            --recupera el id_ocg_solicitud de los derechohabientes que se actualizaron
                            DECLARE cur_ocg_ug CURSOR FOR SELECT id_ocg_solicitud_ug 
                                                          FROM ocg_solicitud_uso_garantia
                                                          WHERE id_derechohabiente = p_id_derechohabiente AND 
                                                                cve_ent_financiera = e_cve_ent_fin AND 
                                                                diagnostico  = 1 AND 
                                                                estado       = 20 AND 
                                                                situacion IN (50,90);
                                                                
                            FOREACH cur_ocg_ug INTO v_id_ocg_sol_ug
                                --inserta en historico los registros que se actualizaron
                                INSERT INTO ocg_his_solic_uso_garantia
                                                    (id_ocg_solicitud_ug,
                                                     id_derechohabiente,
                                                     cve_ent_financiera,
                                                     num_ctr_int_ef,
                                                     f_proceso,
                                                     usuario)
                                             VALUES (v_id_ocg_sol_ug,--id_ocg_solicitud_ug
                                                     p_id_derechohabiente,
                                                     e_cve_ent_fin,
                                                     e_num_ctr_interno,--antiguo
                                                     TODAY,
                                                     g_usuario);
                            END FOREACH 
                       END IF 

                       -- se busca si existe en **ocg_devolucion**  (PENDIENTE)
                       {SELECT COUNT(*) INTO v_ex_dev 
                                        FROM ocg_devolucion
                                        WHERE id_derechohabiente = p_id_derechohabiente AND 
                                              cve_ent_financiera = e_cve_ent_fin AND 
                                              num_ctr_int_ef = e_num_ctr_interno AND
                                              diagnostico  = 1  AND         
                                              estado       = 70 AND 
                                              situacion    = 30;

                        IF (v_ex_dev >= 1) THEN

                            -- si lo encuentra actualiza **ocg_devolucion**
                            UPDATE ocg_devolucion 
                                 SET num_ctr_int_ef = e_nvo_num_ctr_int
                                 WHERE id_derechohabiente = p_id_derechohabiente  AND 
                                       cve_ent_financiera = e_cve_ent_fin  AND
                                       num_ctr_int_ef = e_num_ctr_interno  AND 
                                       diagnostico  = 1  AND 
                                       estado       = 70 AND 
                                       situacion    = 30;

                            --recupera los ID´s de los registros que se actualizaron
                            DECLARE cur_ocg_dev CURSOR FOR SELECT id_ocg_devolucion,
                                                                  id_ocg_detalle, 
                                                                  id_ocg_formalizacion, 
                                                                  id_ocg_tramite
                                                             FROM ocg_devolucion
                                                             WHERE id_derechohabiente = p_id_derechohabiente AND 
                                                                   cve_ent_financiera = e_cve_ent_fin  AND
                                                                   diagnostico  = 1  AND 
                                                                   estado       = 70 AND 
                                                                   situacion    = 30;

                            FOREACH cur_ocg_dev INTO v_id_ocg_dev,
                                                     v_id_ocg_det, 
                                                     v_id_ocg_form,
                                                     v_id_ocg_tmt
                            
                                -- Guarda el registro en el historico en **ocg_his_devolucion**
                                INSERT INTO ocg_his_devolucion
                                                (id_ocg_devolucion,
                                                 id_ocg_detalle,
                                                 id_ocg_formalizacion,
                                                 id_ocg_tramite,
                                                 id_derechohabiente,
                                                 cve_ent_financiera,
                                                 num_ctr_int_ef,
                                                 f_proceso,
                                                 usuario)
                                        VALUES  (v_id_ocg_dev,
                                                 v_id_ocg_det,
                                                 v_id_ocg_form,
                                                 v_id_ocg_tmt,
                                                 p_id_derechohabiente,
                                                 e_cve_ent_fin,
                                                 e_num_ctr_interno, -- antiguo
                                                 TODAY,
                                                 g_usuario)
                            END FOREACH  
                        END IF }

                        -- busca en ocg_ctr_transaccion
                        {SELECT COUNT(*) INTO v_ex_ctr_trans 
                                        FROM ocg_ctr_transaccion 
                                        WHERE id_derechohabiente = p_id_derechohabiente AND 
                                              cve_ent_financiera = e_cve_ent_fin  AND
                                              num_ctr_int_ef = e_num_ctr_interno  AND
                                              estado < 70;


                        IF (v_ex_ctr_trans >= 1) THEN
                        
                            --actualiza **ocg_ctr_transaccion**
                            UPDATE ocg_ctr_transaccion 
                                  SET num_ctr_int_ef = e_nvo_num_ctr_int
                                  WHERE id_derechohabiente = p_id_derechohabiente  AND 
                                        cve_ent_financiera = e_cve_ent_fin  AND 
                                        num_ctr_int_ef = e_num_ctr_interno  AND
                                        estado < 70;
                                  
                            --recupera los ID´s de los registros que se actualizaron
                            DECLARE cur_ocg_trans CURSOR FOR SELECT id_ocg_ctr_transaccion,
                                                                    id_ocg_formalizacion,
                                                                    id_referencia_cta
                                                             FROM ocg_ctr_transaccion
                                                             WHERE id_derechohabiente = p_id_derechohabiente AND 
                                                                   cve_ent_financiera = e_cve_ent_fin  AND
                                                                   estado  < 70;
                      
                            FOREACH cur_ocg_trans INTO v_id_ocg_trans,
                                                       v_id_ocg_form_ctr,
                                                       v_id_ref_cta
                                --inserta los registros que se actualizaron
                                INSERT INTO ocg_his_transaccion
                                                    (id_ocg_ctr_transaccion,
                                                     id_ocg_formalizacion,
                                                     id_derechohabiente,
                                                     id_referencia_cta,
                                                     cve_ent_financiera,
                                                     num_ctr_int_ef,
                                                     f_proceso,
                                                     usuario)
                                             VALUES (v_id_ocg_trans,  --- id_ocg_his_transaccion
                                                     v_id_ocg_form_ctr,
                                                     p_id_derechohabiente,
                                                     v_id_ref_cta,
                                                     e_cve_ent_fin,
                                                     e_num_ctr_interno,--antiguo
                                                     TODAY,
                                                     g_usuario);
                            END FOREACH 
                        END IF }
                                                                       
                        IF (SQLCA.SQLCODE == 0) THEN
                            CALL fn_mensaje("Información","El número de control interno se ha actualizado correctamente","about")
                            EXIT INPUT
                        ELSE 
                            CALL fn_mensaje("Información","No se pudo realizar la actualización","stop")
                            EXIT INPUT 
                        END IF 
                    END IF
            END IF 
          
        ON ACTION CANCEL
             EXIT INPUT 
    END INPUT 
CLOSE WINDOW win2
END FUNCTION 

               
FUNCTION fn_evalua_numero(p_valor)

DEFINE p_valor       STRING    
DEFINE v_numero      SMALLINT
DEFINE i             INTEGER 
    
   FOR i=1 TO p_valor.getLength()
        
     IF (p_valor.subString(i,i) == 0 OR p_valor.subString(i,i) == 1 OR 
         p_valor.subString(i,i) == 2 OR p_valor.subString(i,i) == 3 OR 
         p_valor.subString(i,i) == 4 OR p_valor.subString(i,i) == 5 OR 
         p_valor.subString(i,i) == 6 OR p_valor.subString(i,i) == 7 OR 
         p_valor.subString(i,i) == 8 OR p_valor.subString(i,i) == 9  ) THEN  

         LET v_numero = 0
          
     ELSE 
        LET v_numero = 1
        
        IF (v_numero == 1) THEN 
            EXIT FOR 
         END IF 
        END IF
          
   END FOR 
   
   RETURN v_numero
END FUNCTION
