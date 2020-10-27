--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 02-04-2012
--==============================================================================

################################################################################
#Modulo            =>MDT                                                       #
#Programa          =>MDTL02                                                    #
#Objetivo          =>Registrar instrucciones de mandatos Canales               #
#Autor             =>Alexandro Hollmann, EFP                                   #
#Fecha inicio      =>07 Febrero 2012                                           #
################################################################################

DATABASE safre_viv

DEFINE v_s_qryTxt        STRING
DEFINE v_r_mandato       RECORD LIKE mdt_ctr_mandato.*
DEFINE v_r_mandato_det   RECORD LIKE mdt_det_ctr_mandato.*
DEFINE v_r_pso_mandato   RECORD LIKE mdt_solicitud_mandato.*
DEFINE v_r_credito       RECORD LIKE cre_acreditado.*   -- Cambio de acr_transferencia por cre_acreditado
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      VARCHAR(30),              -- nombre del programa
       v_ventana         ui.Window

MAIN

   DEFINE v_i_count         INTEGER

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog = ARG_VAL(3)

   
	 
	 -- Contador del total de solicitudes a procesar 
	 SELECT NVL(COUNT(*),0) INTO v_i_count
	   FROM mdt_solicitud_mandato
	  WHERE id_origen = 2
   	                  
   OPEN WINDOW w_sol_canales WITH FORM "MDTL02"
   
   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF
   -- Despliegue del total de solicitudes a procesar 
   DISPLAY v_i_count TO tot_solicitudes
   
   MENU ""
      BEFORE MENU
         IF v_i_count = 0 OR v_i_count IS NULL THEN
            CALL fn_mensaje("Advertencia", "No hay mandatos de origen 2","info")
            CALL DIALOG.setActionHidden("accept",1)
         END IF
         
      ON ACTION ACCEPT
         IF fn_ventana_confirma("Confimar","Desea continuar con el proceso?","info") = 1 THEN
         --IF FGL_WINQUESTION("Confirmación", "Desea continuar con el proceso ? ",
         --                   "yes", "yes|no", "question", 0) = "yes" THEN
            CALL Fn_Ins_Mandato_Canales()
            -- AHM TMP Se lanzará proceso batch
            EXIT MENU
         ELSE
            EXIT MENU
         END IF
      ON ACTION CANCEL
         EXIT MENU
   END MENU

   CLOSE WINDOW w_sol_canales 

END MAIN

######################################################################
#  Modulo:      Librería de funciones generales de mandatos          #
#  Nombre:      Fn_Ins_Mandato_Recurrente                            #
#  Descripcion: Realiza la insercion a las tablas de mandatos:       #
#               mdt_ctr_mandato y mdt_det_ctr_mandato a partir de    #
#               la tabla de paso (mdt_solicitud_mandato)                   #
#  Parametros:  Entrada: Ninguno                                     #
#               Salida:  Ninguno                                     #
#       Fecha de creacion:   08-Feb-2012                             #
#       Responsable:         Alexandro Hollmann Montiel, EFP         #
######################################################################
FUNCTION Fn_Ins_Mandato_Canales()
   
   LET v_s_qryTxt = "SELECT *",
                    "  FROM mdt_solicitud_mandato",
                    " WHERE id_origen = 2",
                    " ORDER BY id_credito"
                    
   -- seleccion de movimientos en mdt_solicitud_mandato por lote elegido
   PREPARE EnuEncMandato FROM v_s_qryTxt
   DECLARE CurEncMandato CURSOR FOR EnuEncMandato
   
   FOREACH CurEncMandato INTO v_r_pso_mandato.*
      
      -- Verifica la existencia del credito para ser insertado o no
      IF NOT fn_sql_exi_mandato(v_r_pso_mandato.id_credito) THEN
         -- TMP AHM va a cambior por funcion BD seriacion
         SELECT NVL(MAX(id_ctr_mandato),0) INTO v_r_mandato.id_ctr_mandato
           FROM mdt_ctr_mandato
         IF v_r_mandato.id_ctr_mandato IS NULL or v_r_mandato.id_ctr_mandato = 0 THEN
           LET v_r_mandato.id_ctr_mandato = 1
         ELSE
           LET v_r_mandato.id_ctr_mandato = v_r_mandato.id_ctr_mandato + 1
         END IF      
         
         -- AHM TMP Validar que sea relacion uno a uno
         SELECT * INTO v_r_credito.*
           FROM cre_acreditado   -- Cambio de acr_transferencia por cre_acreditado 
          WHERE num_credito = v_r_pso_mandato.id_credito

         SELECT f_proceso
           INTO v_r_mandato.f_lote
           FROM mdt_lote_mandato
          WHERE folio = v_r_pso_mandato.folio
         -- LET v_r_mandato.id_ctr_mandato          = 
         -- Inicializacion del arreglo de encabezado de mandatos antes de su insercion
         LET v_r_mandato.id_derechohabiente      = v_r_pso_mandato.id_derechohabiente
         LET v_r_mandato.nss                     = v_r_pso_mandato.nss
         LET v_r_mandato.id_credito              = v_r_pso_mandato.id_credito
         --LET v_r_mandato.f_lote                  = v_r_pso_mandato.f_lote
         LET v_r_mandato.lote                    = NULL--v_r_pso_mandato.lote
         
         LET v_r_mandato.id_lote                 = v_r_pso_mandato.folio--NULL--v_r_pso_mandato.id_lote
         LET v_r_mandato.tpo_credito             = v_r_credito.tpo_credito
         LET v_r_mandato.edo_credito             = v_r_credito.edo_credito
         LET v_r_mandato.tpo_descuento_credito   = v_r_credito.tpo_dscto
         LET v_r_mandato.valor_descuento_credito = v_r_credito.valor_dscto
         LET v_r_mandato.estado                  = 100
         LET v_r_mandato.tpo_prelacion           = '' -- Por definir origen
         LET v_r_mandato.usuario                 = p_v_usuario
         
         -- AHM Se obtiene del lote_mandato LET v_r_mandato.id_lote_mandato         = 0  -- Por definir origen
         {SELECT UNIQUE id_lote_mandato INTO v_r_mandato.id_lote_mandato
           FROM mdt_lote_mandato
          WHERE lote = v_r_pso_mandato.lote}
          # en sustitucion de la consulta anterior
          LET v_r_mandato.id_lote_mandato = v_r_pso_mandato.folio

         -- Inicializacion del arreglo de detalle de mandatos antes de su insercion (1ra parte)
         LET v_r_mandato_det.id_ctr_mandato      = v_r_mandato.id_ctr_mandato
         LET v_r_mandato_det.id_derechohabiente  = v_r_mandato.id_derechohabiente
         LET v_r_mandato_det.nss                 = v_r_mandato.nss
         
         INSERT INTO mdt_ctr_mandato VALUES(v_r_mandato.*)
      
      END IF
      
      -- TMP AHM va a cambior por funcion BD seriacion
      SELECT NVL(MAX(id_det_ctr_mandato),0) INTO v_r_mandato_det.id_det_ctr_mandato
        FROM mdt_det_ctr_mandato
      IF v_r_mandato_det.id_det_ctr_mandato IS NULL or v_r_mandato_det.id_det_ctr_mandato = 0 THEN
        LET v_r_mandato_det.id_det_ctr_mandato = 1
      ELSE
        LET v_r_mandato_det.id_det_ctr_mandato = v_r_mandato_det.id_det_ctr_mandato + 1
      END IF      

      -- Inicializacion del arreglo de detalle de mandatos antes de su insercion (2da parte)
      -- AHM Eliminado de BD 20120210 LET v_r_mandato_det.id_mandato              = v_r_pso_mandato.id_mandato
      LET v_r_mandato_det.tpo_descuento_mandato   = v_r_pso_mandato.tpo_descuento_mandato  
      LET v_r_mandato_det.valor_descuento_mandato = v_r_pso_mandato.valor_descuento_mandato
      LET v_r_mandato_det.f_inicio_mandato        = v_r_pso_mandato.f_inicio_mandato       
      LET v_r_mandato_det.f_culmina_mandato       = v_r_pso_mandato.f_culmina_mandato      
      LET v_r_mandato_det.referencia              = v_r_pso_mandato.referencia
      LET v_r_mandato_det.scta_origen_descuento   = v_r_pso_mandato.scta_origen_descuento
      LET v_r_mandato_det.movimiento              = NULL
      LET v_r_mandato_det.modalidad_aplicacion    = v_r_pso_mandato.modalidad_aplicacion
      LET v_r_mandato_det.f_presentacion          = NULL

      INSERT INTO mdt_det_ctr_mandato VALUES(v_r_mandato_det.*)
      
   END FOREACH
   
END FUNCTION

######################################################################
#  Modulo:      Librería de funciones generales de mandatos          #
#  Nombre:      fn_sql_exi_mandato                                   #
#  Descripcion: Verifica la existencia del credito para insertarse   #
#               en la tabla maestro de mandatos                      #
#  Parametros:  Entrada: p_id_credito - Credito a validar si existe  #
#               Salida:  Verdadero si existe el credito, false en    #
#                        caso contrario                              #
#       Fecha de creacion:   08-Feb-2012                             #
#       Responsable:         Alexandro Hollmann Montiel, EFP         #
######################################################################
FUNCTION fn_sql_exi_mandato(p_id_credito)
DEFINE p_id_credito    LIKE mdt_ctr_mandato.id_credito
DEFINE v_i_count       INTEGER

   SELECT NVL(COUNT(*),0) INTO v_i_count
     FROM mdt_ctr_mandato
    WHERE id_credito = p_id_credito
    
   IF v_i_count > 0 THEN
      RETURN TRUE
   END IF
   
   RETURN FALSE
   
END FUNCTION
