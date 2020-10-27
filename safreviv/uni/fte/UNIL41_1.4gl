--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 13/02/2013
--===============================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIC09                                                        #
#Objetivo     => Consultar los datos registrados por medio de la carga inicial,#
#                que cuentan con saldo y que aún NO han sido liquidados        # 
#Fecha inicio => Febrero 13, 2013                                              #
################################################################################
 
DATABASE safre_viv
GLOBALS "UNIG01.4gl"
GLOBALS 
--Arreglo que almacena los id_derechohabiente a liquidar
DEFINE arr_id_derechohabiente DYNAMIC ARRAY OF RECORD
          v_id_derechohabiente INTEGER
END RECORD

--Arreglo con los NSS pendientes de liquidar
DEFINE arr_detalles_liquidar DYNAMIC ARRAY OF RECORD
          v_nss_unificador CHAR(11),
          v_nss_unificado  CHAR(11), 
          v_total_aivs     DECIMAL(16,6), 
          v_check_box      SMALLINT
END RECORD

--Arreglo con los NSS seleccionados para ser liquidados
DEFINE arr_seleccionados_liquidar DYNAMIC ARRAY OF RECORD
          v_nss_unificador CHAR(11),
          v_nss_unificado  CHAR(11),
          v_total_aivs     DECIMAL(16,6)
END RECORD

END GLOBALS 


MAIN

DEFINE v_folio_unificacion DECIMAL (9,0),  --Folio unificacion = 3339
       v_nss_unificador    CHAR(11),        --NSS que capture el usuario 
       v_nss_liquidar      CHAR(11),
       v_seleccion         SMALLINT,
       v_ind_no_liq        INTEGER, 
       v_ind_detalles      INTEGER,
       v_ind_seleccion     INTEGER, 
       v_check_box_1       SMALLINT

DEFINE w ui.Window
DEFINE f ui.Form   

   OPEN WINDOW vtn_UNIC09 WITH FORM "UNIC090.4fd" 
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
   
   --Permite captura de NSS y Folio de unificación
   INPUT BY NAME v_nss_unificador    
   WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)

   BEFORE INPUT 
      --LET v_folio_unificacion = 3339  --Folio de produccion 
      LET v_folio_unificacion = 4608  --Folio de desarrollo
      DISPLAY BY NAME v_folio_unificacion

      --CALL f.setElementHidden("gr_detalles", 1)
   
      ON ACTION ACCEPT
         --Valida que el NSS sea un registro valido
         IF v_nss_unificador IS NOT NULL AND LENGTH(v_nss_unificador) < 11 THEN 
            CALL fn_mensaje("Atención","El NSS capturado debe contener 11 dígitos", "stop")

            NEXT FIELD v_nss_unificador
         END IF 
         
         --Consulta si hay registros pendientes de liquidar 
         CALL fn_consulta_pend_liquidar(v_folio_unificacion,
                                        v_nss_unificador)    
            RETURNING v_ind_no_liq

            --Si hay registros pendientes de liquidar los muestra para seleccionarlos  
            IF v_ind_no_liq > 2 THEN 
               CALL f.setElementHidden("gr_detalles",0)
               --Se consulta los NSS Unificador, NSS unificado y AIVS 
               CALL fn_consulta_detalles(arr_id_derechohabiente[v_ind_no_liq].*,
                                         v_folio_unificacion)
                  RETURNING v_ind_detalles
            ELSE
               CALL fn_mensaje ("Atención", "No existen registros con la información proporcionada", "stop");
            END IF 

            --Mostrar los detalles de los NSS a liquidar             
            --Se habilita la captura para seleccionar el NSS a liquidar 
            INPUT ARRAY arr_detalles_liquidar WITHOUT DEFAULTS 
            FROM scr_detalles.*
            ATTRIBUTES  (INSERT ROW = FALSE, 
                         APPEND ROW = FALSE, 
                         DELETE ROW = FALSE)
                         
               ON CHANGE v_check_box
                  LET v_ind_seleccion = v_ind_seleccion + 1

                  --Llena arreglo con NSS que se han seleccionados
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_nss_unificador = arr_detalles_liquidar[ARR_CURR()].v_nss_unificador
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_nss_unificado  = arr_detalles_liquidar[ARR_CURR()].v_nss_unificado
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_total_aivs     = arr_detalles_liquidar[ARR_CURR()].v_total_aivs
                  
              ON ACTION ACCEPT
                 --Si no se selecciona ningun registro no se debe liquidar nada
                 IF v_ind_seleccion = 0 THEN 
                    CALL fn_mensaje ("Atención", "Debe seleccionar al menos un registro", "stop");
                 ELSE 
                    CALL fn_mensaje ("Atención", "Se liquidarán los registros seleccionados", "info");

                    CALL fn_ejecuta_preliquida_pendientes()
                    
                 END IF 
                 
              ON ACTION CANCEL
                  CALL f.setElementHidden("gr_detalles",1)
                  EXIT INPUT
                  
            END INPUT 
      ON ACTION CANCEL
         EXIT INPUT            
   END INPUT
CLOSE WINDOW vtn_UNIC09
END MAIN 

#OBJETIVO: Consultar los NSS pendientes a liquidar cuyo origen provenga de la Carga Inicial
FUNCTION fn_consulta_pend_liquidar(p_folio_unificacion,
                                   p_nss_unificador)

DEFINE p_folio_unificacion DECIMAL(9,0), --Folio de unificación (3339)
       p_nss_unificador    CHAR(11), --NSS para la consulta
       v_QryTxt            STRING,   --Cadena para consulta  
       v_ind_no_liq        INTEGER   --Indice arreglo detalles
   
   LET v_QryTxt = "\n SELECT id_derechohabiente                                     ",
                  "\n FROM   cta_movimiento                                         ",
                  "\n WHERE  id_derechohabiente in (SELECT id_derechohabiente       ",
                  "\n                               FROM   uni_det_unificado        ",
                  "\n                               WHERE  folio_unificacion = ", p_folio_unificacion,
                  --"\n                               AND    diagnostico = 6          " --Diagnostico PRODUCCION
                  "\n                               AND    diagnostico = 1          " --Diagnostico DESARROLLO

   --Si la búsqueda se hace por NSS 
   IF p_nss_unificador IS NOT NULL THEN            
      LET v_QryTxt = v_QryTxt || "\n                               AND    nsscta2 = ", "'", p_nss_unificador,"')",
                                 "\n GROUP BY 1 ",
                                 "\n HAVING SUM(monto_acciones) > 0"
   ELSE 
      --Si la búsqueda se general
      LET v_QryTxt = v_QryTxt || ")",
                                 "\n GROUP BY 1 ",
                                 "\n HAVING SUM(monto_acciones) > 0"
   END IF 

   --DISPLAY v_QryTxt
   
   PREPARE prp_cons_det_liquidar FROM v_QryTxt
   DECLARE cur_cons_det_liquidar CURSOR FOR prp_cons_det_liquidar

   LET v_ind_no_liq = 1
   FOREACH cur_cons_det_liquidar INTO arr_id_derechohabiente[v_ind_no_liq].*
      LET v_ind_no_liq = v_ind_no_liq + 1
   END FOREACH

   RETURN v_ind_no_liq
   
END FUNCTION  

#OBJETIVO: Consultar los NSS Unificador, NSS unificado y AIVS 
FUNCTION fn_consulta_detalles(p_arr_id_derechohabiente, 
                              p_folio_unificacion)
DEFINE p_arr_id_derechohabiente RECORD
          v_id_derechohabiente INTEGER
END RECORD 

DEFINE p_folio_unificacion DECIMAL(9,0), --Folio de unificación (3339)
       p_nss_unificador    CHAR(11), --NSS para la consulta
       v_QryTxt            STRING,   --Cadena para consulta  
       v_ind_det_liq       INTEGER  --Indice arreglo detalles
       
   LET v_QryTxt = "\n SELECT a.nsscta2,          ",                                               
                  "\n        a.nsscta1,          ",
                  "\n        SUM(b.monto_acciones),",
                  "\n        0                   ", --1 Unchecked
                  "\n FROM   uni_det_unificado a,",
                  "\n        cta_movimiento    b ",
                  "\n WHERE  a.id_derechohabiente = b.id_derechohabiente",
                  "\n AND    a.folio_unificacion = ",p_folio_unificacion,
                  "\n GROUP BY 1,2"

   --DISPLAY v_QryTxt
   
   PREPARE prp_cons_nss_liquidar FROM v_QryTxt
   DECLARE cur_cons_nss_liquidar CURSOR FOR prp_cons_nss_liquidar 

   LET v_ind_det_liq = 1

   FOREACH cur_cons_nss_liquidar INTO arr_detalles_liquidar[v_ind_det_liq].*  
         LET v_ind_det_liq = v_ind_det_liq + 1
   END FOREACH 
   
   CALL arr_detalles_liquidar.deleteElement(arr_detalles_liquidar.getLength())

   RETURN v_ind_det_liq
   
END FUNCTION 

#OBJETIVO: Invoca el programa que se encarga de ejecutar la preliquidación
FUNCTION fn_ejecuta_preliquida_pendientes()


   
END FUNCTION  