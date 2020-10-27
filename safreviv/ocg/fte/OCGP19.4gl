###########################################################################
#Modulo            => OCG                                                 #
#Programa          => OCGP19                                              #
#Objetivo          => Programa que republica  de forma masiva los créditos#
#                     con solicitud de trámite aceptada de una EF.        #
#Autor             => Emilio Abarca Sánchez, EFP                          #
#Fecha inicio      => 04 DICIEMBRE 2016                                   #
###########################################################################

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
   CALL STARTLOG(g_usuario CLIPPED|| ".OCGP19.log")


    -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   CLOSE WINDOW SCREEN  
   CALL seleccion_ef()
   
END MAIN

FUNCTION seleccion_ef()

   DEFINE v_cmbx_ef     CHAR(67)

OPEN WINDOW vtn1 WITH FORM "OCGP191"

   INPUT BY NAME v_cmbx_ef ATTRIBUTE(UNBUFFERED, WITHOUT DEFAULTS)
      BEFORE INPUT
         CALL carga_cbx_EF()
        
      ON ACTION ACCEPT
         IF (v_cmbx_ef IS NULL) THEN
            CALL fn_mensaje("","Debe seleccionar la entidad financiera","")
         ELSE 
            -- Carga de solicitudes aceptadas por EF
            CALL carga_solicitudes_ef(v_cmbx_ef)
         END IF 
      ON ACTION CANCEL 
         EXIT INPUT 
   END INPUT 
   
CLOSE WINDOW vtn1

END FUNCTION

FUNCTION carga_cbx_EF()

   DEFINE cbx_ef          ui.ComboBox
   DEFINE v_cve_ent_fin   LIKE cat_entidad_financiera.cve_ent_financiera
   DEFINE v_ent_fin_desc  LIKE cat_entidad_financiera.ent_financiera_desc
   DEFINE v_concat_ent    CHAR (67) 

   LET cbx_ef = ui.ComboBox.forName("v_cmbx_ef")

   DECLARE crsr_ef CURSOR FOR
                   SELECT cve_ent_financiera, 
                          ent_financiera_desc
                     FROM cat_entidad_financiera
                    WHERE estado_ef = 10
                    ORDER BY cve_ent_financiera;

    CALL cbx_ef.clear()

    FOREACH crsr_ef INTO v_cve_ent_fin, v_ent_fin_desc
        LET v_concat_ent = v_cve_ent_fin || " - ",v_ent_fin_desc
        CALL cbx_ef.addItem(v_cve_ent_fin, v_concat_ent)
    END FOREACH 
    
END FUNCTION 

FUNCTION carga_solicitudes_ef(p_cve_ef)

   DEFINE p_cve_ef       SMALLINT
   DEFINE v_ef_desc      LIKE cat_entidad_financiera.ent_financiera_desc
   DEFINE ef_completa    CHAR(67)
   DEFINE v_query        STRING
   DEFINE i              INTEGER
   DEFINE v_exist_frm    INTEGER
   DEFINE mnj_cadena     STRING 
   DEFINE v_answer       BOOLEAN
   DEFINE k              INTEGER 

   DEFINE rec_sol_acept RECORD
      nss            CHAR(11),
      id_ocg_tramite DECIMAL(9,0),
      id_ocg_detalle DECIMAL(9,0),
      id_dh          DECIMAL(9,0),
      rfc            CHAR(13),
      curp           CHAR(18),
      ap_paterno     CHAR(40),
      ap_materno     CHAR(40),
      nombre         CHAR(40),
      tpo_credito    CHAR(1),
      estado         SMALLINT,
      edo_desc       CHAR(40),
      situacion      SMALLINT,
      situacion_desc CHAR(40)
   END RECORD

   DEFINE arr_sol_acept DYNAMIC ARRAY  OF RECORD
      nss                CHAR(11),
      id_ocg_tramite     DECIMAL(9,0),
      id_ocg_detalle     DECIMAL(9,0),
      id_dh              DECIMAL(9,0),
      rfc                CHAR(13),
      curp               CHAR(18),
      nombre_completo    CHAR(126),
      tpo_credito        CHAR(1),
      estado_completo    CHAR(50),
      situacion_completo CHAR(50)
   END RECORD 

OPEN WINDOW vtn2 WITH FORM "OCGP192"

   SELECT ent_financiera_desc
      INTO v_ef_desc
      FROM cat_entidad_financiera
    WHERE cve_ent_financiera = p_cve_ef

   LET ef_completa = p_cve_ef CLIPPED ||" ", v_ef_desc

   LET v_query = "SELECT afi.nss                ,
                         tmt.id_ocg_tramite     , 
                         tmt.id_ocg_detalle     , 
                         tmt.id_derechohabiente , 
                         tmt.rfc                ,
                         tmt.curp               ,
                         tmt.ap_paterno         ,
                         tmt.ap_materno         ,
                         tmt.nombre             ,
                         tmt.tpo_credito        ,
                         tmt.estado             ,
                         edo.edo_credito_desc   ,
                         tmt.situacion          ,
                         st.situacion_desc
                    FROM afi_derechohabiente afi,
                         ocg_tramite tmt        ,
                         cat_ocg_estado edo     ,
                         cat_ocg_situacion st
                   WHERE tmt.id_derechohabiente = afi.id_derechohabiente
                     AND tmt.cve_ent_financiera = ",p_cve_ef,
                    "AND tmt.tpo_credito IN (7,8)
                     AND tmt.diagnostico = 1
                     AND tmt.estado      IN(10,70)
                     AND tmt.estado      = edo.edo_credito
                     AND tmt.situacion   = 50
                     AND tmt.situacion   = st.situacion;" 

   PREPARE consulta FROM v_query
   DECLARE crsr_solicitudes CURSOR FOR consulta

   LET i = 1
   
   CALL arr_sol_acept.clear()
   
   FOREACH crsr_solicitudes INTO rec_sol_acept.nss,
                                 rec_sol_acept.id_ocg_tramite,
                                 rec_sol_acept.id_ocg_detalle,
                                 rec_sol_acept.id_dh,
                                 rec_sol_acept.rfc,
                                 rec_sol_acept.curp,
                                 rec_sol_acept.ap_paterno,
                                 rec_sol_acept.ap_materno,
                                 rec_sol_acept.nombre,
                                 rec_sol_acept.tpo_credito,
                                 rec_sol_acept.estado,
                                 rec_sol_acept.edo_desc,
                                 rec_sol_acept.situacion,
                                 rec_sol_acept.situacion_desc
                                 
      # Verifica que no cuente con un crédito formalizado
      SELECT COUNT(*)
         INTO v_exist_frm
         FROM ocg_formalizacion
        WHERE id_ocg_tramite = rec_sol_acept.id_ocg_tramite

      IF (v_exist_frm = 0) THEN  
      
         -- Se llena el arreglo
         LET arr_sol_acept[i].nss                = rec_sol_acept.nss
         LET arr_sol_acept[i].id_ocg_tramite     = rec_sol_acept.id_ocg_tramite
         LET arr_sol_acept[i].id_ocg_detalle     = rec_sol_acept.id_ocg_detalle
         LET arr_sol_acept[i].id_dh              = rec_sol_acept.id_dh
         LET arr_sol_acept[i].rfc                = rec_sol_acept.rfc
         LET arr_sol_acept[i].curp               = rec_sol_acept.curp
         LET arr_sol_acept[i].nombre_completo    = rec_sol_acept.ap_paterno CLIPPED || " ",rec_sol_acept.ap_materno CLIPPED," ",rec_sol_acept.nombre CLIPPED
         LET arr_sol_acept[i].tpo_credito        = rec_sol_acept.tpo_credito
         LET arr_sol_acept[i].estado_completo    = rec_sol_acept.estado CLIPPED || " ",rec_sol_acept.edo_desc
         LET arr_sol_acept[i].situacion_completo = rec_sol_acept.situacion CLIPPED || " ",rec_sol_acept.situacion_desc

         LET i = i + 1   

      END IF 
         
   END FOREACH 

   
   DISPLAY ARRAY arr_sol_acept TO tabla_solicitudes.* ATTRIBUTE(ACCEPT = FALSE , CANCEL = FALSE )

      BEFORE DISPLAY 
         IF (i = 1) THEN
            CALL fn_mensaje("","No se encontraron registros para esta entidad financiera","")
            EXIT DISPLAY 
         END IF 
         
      ON ACTION Republicar
         LET mnj_cadena = "¿Esta seguro de republicar estos registros a la entidad financiera: ", ef_completa CLIPPED ,"?"
         
         LET v_answer = fn_ventana_confirma("Confirmación",mnj_cadena,"about")

         IF (v_answer = 0) THEN
            CALL fn_mensaje("","Republicación cancelada","")
            EXIT DISPLAY
         ELSE
            # Inicia la republicación masiva
          
            FOR k = 1 TO arr_sol_acept.getLength()
               -- Actualiza en ocg_tramite
               UPDATE ocg_tramite
                  SET situacion = 30, --diagnostico aceptado
                      estado    = 70  --aceptado
                WHERE id_ocg_tramite     = arr_sol_acept[k].id_ocg_tramite
                  AND id_ocg_detalle     = arr_sol_acept[k].id_ocg_detalle
                  AND id_derechohabiente = arr_sol_acept[k].id_dh
                  AND cve_ent_financiera = p_cve_ef
                  AND diagnostico        = 1  -- válido
                  AND estado IN (10,70) 
                  AND situacion          = 50 -- Aceptado

               --- Limpia fecha de respuesta en ocg_fecha_mig
               UPDATE ocg_fecha_mig
                  SET f_respuesta = NULL 
                WHERE id_ocg_referencia = arr_sol_acept[k].id_ocg_tramite
                  AND subproceso = 1;

            END FOR

            CALL fn_mensaje("","La republicación se ha realizado correctamente","")
            EXIT DISPLAY 
         END IF 
         
      ON ACTION CANCEL 
         EXIT DISPLAY 
         
   END DISPLAY 
   
CLOSE WINDOW vtn2

END FUNCTION 
