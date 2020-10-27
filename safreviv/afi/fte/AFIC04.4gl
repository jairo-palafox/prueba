#########################################################################
#Proyecto          => INFONAVIT (MEXICO)                                #
#Propietario       => E.F.P.                                            #
#Programa AFIC04   => Programa de consulta de movimientos afiliatiorios #
#MODULO            => AFI                                               #
#Fecha             => 30 de junio de 2012                               #
#########################################################################
DATABASE safre_viv

GLOBALS
   DEFINE cb             ui.ComboBox

   DEFINE 
   v_d_pid                DECIMAL(9,0), -- identificador del proceso
   v_i_opera_cod          LIKE cat_operacion.opera_cod, -- codigo de operacion
   v_i_proceso_cod        LIKE cat_proceso.proceso_cod, -- codigo del proceso
   p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
   v_folio                DECIMAL (9,0),
   p_v_nom_prog           VARCHAR(30) -- nombre del programa


   DEFINE arr_afi_rch    DYNAMIC ARRAY OF RECORD 
     id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente,
     nss                   LIKE afi_derechohabiente.nss               ,
     curp                  LIKE afi_derechohabiente.curp              ,
     rfc                   LIKE afi_derechohabiente.rfc               ,
     nombre_imss           LIKE afi_derechohabiente.nombre_imss       ,
     nombre_af             LIKE afi_derechohabiente.nombre_af         ,
     ap_paterno_af         LIKE afi_derechohabiente.ap_paterno_af     ,
     ap_materno_af         LIKE afi_derechohabiente.ap_materno_af     ,
     tipo_trabajador       LIKE afi_derechohabiente.tipo_trabajador   ,
     folio_lote            LIKE afi_derechohabiente.folio_lote        
   END RECORD 

END GLOBALS

MAIN
--Sección de variables UI
DEFINE f_ventana   ui.Window, --provee la interfaz para la ventana
       f_forma     ui.Form, --provee la interfaz para la forma
       v_query     STRING,
       v_index     INTEGER, 
       nss         LIKE afi_rch_afiliatorio.nss,
       rfc         LIKE afi_rch_afiliatorio.curp_rfc,
       folio       DECIMAL(9,0) -- folio del proceso

   -- se recuperan los parametros 
   LET p_usuario_cod          = ARG_VAL(1) -- Recibe la variable de usuario
   LET v_i_proceso_cod        = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_v_nom_prog           = ARG_VAL(3) -- Recibe el nombre del programa

   LET v_index = 1
   LET v_query = NULL

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF


   OPEN WINDOW v_afic040 WITH FORM "AFIC040"
   DIALOG ATTRIBUTE(UNBUFFERED)
      
      -- se obtienen los parametros de consulta
      INPUT BY NAME 
         nss, rfc, folio

         BEFORE INPUT
            CALL fn_llena_combo_tipo_mov()
      
         ON ACTION ACCEPT 
            --Valida que se haya capturado al menos un parámetro
            IF (nss IS NULL AND rfc IS NULL AND folio IS NULL) THEN
                 CALL fn_mensaje("Atención","Debe ingresar al menos un criterio de búsqueda","stop")
               NEXT FIELD f_nss
            END IF 

            -- se inicia la clausula de filtro
            LET v_query = "\n 1=1"

            --Armamos el query dinámico
            IF ( nss IS NOT NULL ) THEN
               LET v_query = v_query || "\n AND afi.nss = '", nss, "'"
            END IF 

            IF ( rfc IS NOT NULL ) THEN
               LET v_query = v_query || "\n AND afi.curp_rfc = '", rfc, "'"
            END IF 

            IF ( folio IS NOT NULL ) THEN
               LET v_query = v_query || "\n AND afi.folio_lote = ", folio
            END IF 


            --DISPLAY "v_query -- ",v_query
            CALL arr_afi_rch.clear()
            CALL fn_muestra_consulta(v_query)
            LET v_query = NULL

      ON ACTION cancelar
         EXIT DIALOG  
   
      
      END INPUT 
   END DIALOG 
      
   CLOSE WINDOW v_afic040
   

END MAIN 

#Objetivo: Realiza la consulta y muestra en pantalla 
FUNCTION fn_muestra_consulta(v_comp)
DEFINE v_query   STRING,
       v_comp    STRING,
       v_index   INTEGER 

   LET v_index = 1
   
   -- se crea la consulta
   LET v_query =  "\n SELECT",
                  "\n afi.id_derechohabiente,",
                  "\n afi.nss, ",                
                  "\n afi.curp, ",       
                  "\n afi.rfc, ",           
                  "\n afi.nombre_imss,  ",      
                  "\n afi.nombre_af,   ",              
                  "\n afi.ap_paterno_af, ",               
                  "\n afi.ap_materno_af, ",     
                  "\n afi.tipo_trabajador, ",             
                  "\n afi.folio_lote  ",
                  "\n FROM afi_derechohabiente afi",
                  "\n WHERE" 

   -- se concatena la clausula de filtro
   LET v_query = v_query || v_comp

   DISPLAY "consulta -- ",v_query

   PREPARE prp_afi_rch FROM v_query
   DECLARE cur_afi_rch CURSOR FOR prp_afi_rch

   FOREACH cur_afi_rch INTO arr_afi_rch[v_index].*

         LET v_index = v_index + 1
   END FOREACH 

   -- se borra el ultimo elemento del arreglo que agrega el foreach
   CALL arr_afi_rch.deleteElement(v_index)
   
   LET v_index = v_index - 1

   --DISPLAY "v_index -- ",v_index
   IF v_index <=0 THEN 
   
      CALL fn_mensaje("Información","No existe información con los parámetros dados","stop")
      
   ELSE 
      -- se abre la ventana de resultados
      OPEN WINDOW v_afi WITH FORM "AFIC041"
      
         DISPLAY ARRAY arr_afi_rch TO tbl_derechohabiente.*
            ON ACTION reporte
               CALL fn_reporte_afiliacion()
               
            ON ACTION cancel
               EXIT DISPLAY
         END DISPLAY
      CLOSE WINDOW v_afi
   END IF 
            
   
END FUNCTION 

##Función que llena el combo para seleccionar un tipo de movimiento
FUNCTION fn_llena_combo_tipo_mov()
DEFINE v_ind_cmb  SMALLINT,
       v_folio    LIKE glo_folio.folio

   -- se obtiene el apuntador al combo
   LET cb = ui.ComboBox.forName("folio") --Asignación del combo a la forma

   -- Limpia el combo
   CALL cb.clear()

   -- se agrega el nulo al principio
   CALL cb.addItem(NULL, NULL)
   
   -- se obtienen los folios de afiliacion
   DECLARE cur_folios CURSOR FOR
   SELECT  folio
   FROM    glo_folio
   WHERE   proceso_cod IN (1801,1802)
   ORDER BY folio DESC
   
   FOREACH cur_folios INTO v_folio
      CALL cb.addItem(v_folio, v_folio)
   END FOREACH    
   
   -- se elimina el ultimo
   CALL cb.removeItem(cb.getItemCount())
END FUNCTION

-- funcion que invoca el reporte
FUNCTION fn_reporte_afiliacion()
DEFINE v_contador        SMALLINT,
       report_handler    om.SaxDocumentHandler -- handler para el reporte en PDF

   -- se asigna la plantilla del reporte
   IF ( fgl_report_loadCurrentSettings("AFII04.4rp") ) THEN
      
      -- se asigna el report_handler
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      CALL fn_mensaje("Atención","No se puede generar el reporte","stop")
      RETURN
   END IF

   -- se abre el driver del reporte
   START REPORT rpt_derechohabientes TO XML HANDLER report_handler

   FOR v_contador = 1 TO arr_afi_rch.getLength()
      OUTPUT TO REPORT rpt_derechohabientes(arr_afi_rch[v_contador].*)
   END FOR

   FINISH REPORT rpt_derechohabientes
END FUNCTION

-- reporte de derechohabientes encontrados en consulta de afiliacion
REPORT rpt_derechohabientes(r_derechohabiente)
DEFINE r_derechohabiente     RECORD -- registro de derechohabiente
          id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente,
          nss                   LIKE afi_derechohabiente.nss               ,
          curp                  LIKE afi_derechohabiente.curp              ,
          rfc                   LIKE afi_derechohabiente.rfc               ,
          nombre_imss           LIKE afi_derechohabiente.nombre_imss       ,
          nombre_af             LIKE afi_derechohabiente.nombre_af         ,
          ap_paterno_af         LIKE afi_derechohabiente.ap_paterno_af     ,
          ap_materno_af         LIKE afi_derechohabiente.ap_materno_af     ,
          tipo_trabajador       LIKE afi_derechohabiente.tipo_trabajador   ,
          folio_lote            LIKE afi_derechohabiente.folio_lote        
       END RECORD,
       p_folio               LIKE glo_folio.folio -- folio del lote

   FORMAT
      -- se envia la informacion a pantalla
      PAGE HEADER
         PRINTX p_folio

      BEFORE GROUP OF r_derechohabiente.folio_lote
         LET p_folio = r_derechohabiente.folio_lote
         
         PRINTX p_folio
      
      ON EVERY ROW
         -- se muestra el derechohabiente encontrado
         PRINTX r_derechohabiente.*
         

END REPORT