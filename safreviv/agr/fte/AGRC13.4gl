##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRC13                                             #
#Objetivo          => Consulta monitoreo "Microflujo SACI"               #
#Autor             => Emilio Abarca Sánchez, EFP                         #
#Fecha inicio      => 31/Marzo/2017                                      #
##########################################################################

DATABASE safre_viv

   DEFINE g_usuario      CHAR(20)
   DEFINE g_tipo_proceso SMALLINT
   DEFINE g_nom_ventana  STRING
   DEFINE v_f_ejecucion  CHAR(10) 
   DEFINE v_ventana      ui.Window
   DEFINE v_forma        ui.Form

MAIN 

   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_ventana  = ARG_VAL(3)

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRC13.log")

   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF


   CLOSE WINDOW SCREEN 

      
   LET v_f_ejecucion = TODAY USING "dd/mm/yyyy/"
   
   OPEN WINDOW vtn WITH FORM "AGRC131"

      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm()

      MENU ""
         BEFORE MENU
            DISPLAY v_f_ejecucion TO e_ejecucion
            CALL v_forma.setElementHidden("group2",1)
            
         ON ACTION ACCEPT 
            CALL consulta_microflujo()
            CALL v_forma.setElementHidden("group1",1)
            CALL v_forma.setElementHidden("group2",0)

         ON ACTION CANCEL 
            EXIT MENU 
         
      END MENU 
   
   CLOSE WINDOW vtn
   
END MAIN 

FUNCTION consulta_microflujo()

   DEFINE v_query               STRING
   DEFINE v_desc_tmt1           CHAR(30)
   DEFINE v_desc_tmt2           CHAR(30)
   DEFINE v_desc_orig1          CHAR(30)
   DEFINE v_desc_orig2          CHAR(30) 
   DEFINE v_tmt_ex              INTEGER
   DEFINE v_tmt_no_ex           INTEGER
   DEFINE v_orig_ex             INTEGER
   DEFINE v_orig_no_ex          INTEGER
   DEFINE v_total               INTEGER
   DEFINE v_tmt_ex_prc          DECIMAL(3,0)
   DEFINE v_tmt_no_ex_prc       DECIMAL(3,0)
   DEFINE v_orig_ex_prc         DECIMAL(3,0)
   DEFINE v_orig_no_ex_prc      DECIMAL(3,0)
   DEFINE v_total_prc           DECIMAL(3,0)
   DEFINE v_prc_final           CHAR(6)
   DEFINE v_total_tmts          INTEGER
   DEFINE v_ef_tmt_exitoso      DECIMAL(3,0)
   DEFINE v_c_efectividad_tmt   CHAR(6)
   DEFINE v_ef_tmt_no_exitoso   DECIMAL(3,0)
   DEFINE v_c_ef_tmt_no_exitoso CHAR(6)
   DEFINE v_total_origs         INTEGER 
   DEFINE v_ef_orig_exitosa     DECIMAL(3,0)
   DEFINE v_c_efectividad_orig  CHAR(6)
   DEFINE v_ef_orig_no_exitosa  DECIMAL(3,0)
   DEFINE v_c_ef_orig_no_ex     CHAR(6)

    DEFINE r_tramite RECORD
      id_cre_tramite     DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      num_credito        DECIMAL(10,0),
      estado    SMALLINT
   END RECORD 

   DEFINE v_id_cre_acreditado DECIMAL(9,0)


   LET v_desc_tmt1          = "Trámites Exitosos"
   LET v_desc_tmt2          = "Trámites No Exitosos"
   LET v_desc_orig1         = "Originaciones Exitosas"
   LET v_desc_orig2         = "Originaciones No Exitosas"
   LET v_tmt_ex             = 0
   LET v_tmt_no_ex          = 0
   LET v_tmt_ex_prc         = 0
   LET v_tmt_no_ex_prc      = 0
   LET v_orig_ex_prc        = 0
   LET v_orig_no_ex_prc     = 0
   LET v_total_tmts         = 0
   LET v_total_origs        = 0
   LET v_ef_tmt_exitoso     = 0
   LET v_ef_tmt_no_exitoso  = 0
   LET v_ef_orig_exitosa    = 0
   LET v_ef_orig_no_exitosa = 0
   LET v_id_cre_acreditado  = 0
   
   DISPLAY TIME (CURRENT) TO e_hora
   DISPLAY v_desc_tmt1  TO e_desc_tmt1
   DISPLAY v_desc_tmt2  TO e_desc_tmt2
   DISPLAY v_desc_orig1 TO e_desc_orig1
   DISPLAY v_desc_orig2 TO e_Desc_orig2

##=====> TRÁMITES EXITOSOS Y NO EXITOSOS

   LET v_query = "SELECT t.id_cre_tramite,
                         t.id_derechohabiente,
                         t.num_credito,
                         t.estado
                    FROM cre_tramite t,
                         cre_his_tramite h
                   WHERE t.id_cre_tramite = h.id_cre_tramite 
                     AND t.num_credito    = h.num_credito 
                     AND t.estado IN (18,240) 
                     AND t.estado = h.estado
                     AND DATE(h.f_proceso) = TODAY;"

   INITIALIZE r_tramite.* TO NULL 
    
   PREPARE prp_tmts FROM v_query
   DECLARE crs_tmts CURSOR FOR prp_tmts

   FOREACH crs_tmts INTO r_tramite.id_cre_tramite,
                          r_tramite.id_derechohabiente,
                          r_tramite.num_credito,
                          r_tramite.estado

      -- Limpia variables por cada iteración
      LET v_id_cre_acreditado = NULL 
   
      -- Obtiene el máximo registro de cre_acreditado
      -- Sólo tipo de crédito 10 (COFINAVIT AG) ya que son trámites
      SELECT FIRST 1
                 a.id_cre_acreditado
           INTO v_id_cre_acreditado
           FROM cre_acreditado a,
                cat_tipo_credito r
          WHERE a.id_derechohabiente = r_tramite.id_derechohabiente
            AND a.num_credito = r_tramite.num_credito
            AND a.tpo_credito = 10
            AND a.tpo_credito = r.tpo_credito
            AND a.tpo_originacion = r.tpo_originacion
            ORDER BY a.id_cre_acreditadO DESC;

      IF(v_id_cre_acreditado IS NOT NULL) THEN

         -- Conteo Trámite Exitoso
         IF(r_tramite.estado = 18) THEN
            LET v_tmt_ex = v_tmt_ex + 1 
         END IF 

         -- Conteo Trámite No Exitoso
         IF(r_tramite.estado = 240) THEN
            LET v_tmt_no_ex = v_tmt_no_ex + 1 
         END IF 
               
      END IF 
            
   END FOREACH 

##=====> ORIGINACIONES EXITOSAS Y NO EXITOSAS

   LET v_query = "SELECT t.id_cre_tramite,
                         t.id_derechohabiente,
                         t.num_credito,
                         t.estado
                    FROM cre_tramite t,
                         cre_his_tramite h
                   WHERE t.id_cre_tramite = h.id_cre_tramite 
                     AND t.num_credito    = h.num_credito 
                     AND t.estado IN (20,19) 
                     AND t.estado = h.estado
                     AND DATE(h.f_proceso) = TODAY;"
                      
   INITIALIZE r_tramite.* TO NULL
   
   LET v_orig_ex            = 0
   LET v_orig_no_ex         = 0 
   LET v_id_cre_acreditado  = 0
    
   PREPARE prp_orgs FROM v_query
   DECLARE crs_orgs CURSOR FOR prp_orgs

   FOREACH crs_orgs INTO r_tramite.id_cre_tramite,
                          r_tramite.id_derechohabiente,
                          r_tramite.num_credito,
                          r_tramite.estado

      -- Limpia variables por cada iteración
      LET v_id_cre_acreditado = NULL 
   
      -- Obtiene el máximo registro de cre_acreditado
      SELECT FIRST 1
                 a.id_cre_acreditado
           INTO v_id_cre_acreditado
           FROM cre_acreditado a,
                cat_tipo_credito r
          WHERE a.id_derechohabiente = r_tramite.id_derechohabiente
            AND a.num_credito = r_tramite.num_credito
            AND a.tpo_credito = r.tpo_credito
            AND a.tpo_originacion = r.tpo_originacion
            ORDER BY a.id_cre_acreditadO DESC;

      IF(v_id_cre_acreditado IS NOT NULL) THEN

         -- Conteo Trámite Exitoso
         IF(r_tramite.estado = 20) THEN
            LET v_orig_ex = v_orig_ex + 1 
         END IF 

         -- Conteo Trámite No Exitoso
         IF(r_tramite.estado = 19) THEN
            LET v_orig_no_ex = v_orig_no_ex + 1 
         END IF 
               
      END IF 
            
   END FOREACH 

  
   -- Obtiene total de registros
   LET v_total = v_tmt_ex + v_tmt_no_ex + v_orig_ex + v_orig_no_ex
  
   -- Porcentaje para Trámites exitosos
   LET v_tmt_ex_prc   = (v_tmt_ex / v_total) * 100
  
   -- Porcentaje para Trámites no exitosos
   LET v_tmt_no_ex_prc = (v_tmt_no_ex / v_total) * 100
  
   -- Porcentaje para Originaciones exitosas
   LET v_orig_ex_prc   = (v_orig_ex / v_total) * 100
    
   -- Porcentaje para Originaciones no exitosas
   LET v_orig_no_ex_prc = (v_orig_no_ex / v_total) * 100

   -- Suma total del porcentaje
   LET v_total_prc = v_tmt_ex_prc CLIPPED + v_tmt_no_ex_prc CLIPPED  + v_orig_ex_prc CLIPPED + v_orig_no_ex_prc CLIPPED    
   LET v_prc_final = v_total_prc CLIPPED,"%"

   
   --Calcula porcentaje de tràmites exitosos 
   LET v_total_tmts = v_tmt_ex + v_tmt_no_ex CLIPPED
   LET v_ef_tmt_exitoso = (v_tmt_ex / v_total_tmts) * 100 
   LET v_c_efectividad_tmt = v_ef_tmt_exitoso CLIPPED,"%"

   --Calcula porcentaje efectividad trámites no exitosos
   LET v_ef_tmt_no_exitoso = (v_tmt_no_ex / v_total_tmts ) *  100
   LET v_c_ef_tmt_no_exitoso = v_ef_tmt_no_exitoso CLIPPED,"%"
   
   --Calcula porcentaje de originaciones exitosas
   LET v_total_origs = v_orig_ex + v_orig_no_ex CLIPPED 
   LET v_ef_orig_exitosa = (v_orig_ex / v_total_origs) * 100
   LET v_c_efectividad_orig = v_ef_orig_exitosa CLIPPED,"%"

   -- Calcula porcentaje efectividad originaciones no exitosas
   LET v_ef_orig_no_exitosa = (v_orig_no_ex / v_total_origs) * 100
   LET v_c_ef_orig_no_ex = v_ef_orig_no_exitosa CLIPPED,"%"
   
   --Muestra información.
   DISPLAY v_tmt_ex     TO e_tmt_ex
   DISPLAY v_tmt_no_ex  TO e_tmt_no_ex
   DISPLAY v_orig_ex    TO e_orig_ex
   DISPLAY v_orig_no_ex TO e_orig_no_ex
   DISPLAY v_total      TO e_total
   DISPLAY v_tmt_ex_prc TO e_tmt1
   DISPLAY v_tmt_no_ex_prc  TO e_tmt2
   DISPLAY v_orig_ex_prc    TO e_orig1
   DISPLAY v_orig_no_ex_prc TO e_orig2
   DISPLAY v_prc_final      TO e_porcentaje
   DISPLAY v_c_efectividad_tmt   TO e_ef_tmt_exitoso
   DISPLAY v_c_ef_tmt_no_exitoso TO e_ef_tmt_no_exitoso
   DISPLAY v_c_efectividad_orig  TO e_ef_orig_exitosa
   DISPLAY v_c_ef_orig_no_ex TO e_ef_orig_no_ex
   
END FUNCTION 