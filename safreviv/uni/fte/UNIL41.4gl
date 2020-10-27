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

DEFINE g_pid          LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
--Arreglo que almacena los id_derechohabiente a liquidar
DEFINE arr_id_derechohabiente DYNAMIC ARRAY OF RECORD
          v_id_derechohabiente INTEGER
END RECORD
--Arreglo con los NSS pendientes de liquidar
DEFINE arr_detalles_liquidar DYNAMIC ARRAY OF RECORD
          v_id_unificador      DECIMAL(9,0) , 
          v_id_unificado       DECIMAL(9,0) , 
          v_id_derechohabiente DECIMAL(9,0) , 
          v_nss_unificador     CHAR(11)     ,
          v_nss_unificado      CHAR(11)     , 
          v_total_aivs         DECIMAL(16,6), 
          v_check_box          SMALLINT
END RECORD
--Arreglo con los NSS seleccionados para ser liquidados
DEFINE arr_seleccionados_liquidar DYNAMIC ARRAY OF RECORD
          v_id_unificador      DECIMAL(9,0) , 
          v_id_unificado       DECIMAL(9,0) , 
          v_id_derechohabiente DECIMAL(9,0) , 
          v_nss_unificador     CHAR(11)     ,
          v_nss_unificado      CHAR(11)     , 
          v_total_aivs         DECIMAL(16,6) 
END RECORD

DEFINE r_orig_credito RECORD
          v_tipo_originacion SMALLINT   ,
          v_tipo_origin_des  VARCHAR(25),
          v_tipo_credito     SMALLINT   ,
          v_tipo_credito_des VARCHAR(25),
          v_totales          INTEGER
END RECORD 

DEFINE arr_orig_credito DYNAMIC ARRAY OF RECORD
          v_tipo_originacion SMALLINT   ,
          v_tipo_origin_des  VARCHAR(25),
          v_tipo_credito     SMALLINT   ,
          v_tipo_credito_des VARCHAR(25),
          v_totales          INTEGER
END RECORD 

--Arreglo con rutas 
DEFINE g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
END RECORD

DEFINE seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
END RECORD

END GLOBALS 

MAIN
DEFINE v_folio_unificacion DECIMAL (9,0),  --Folio unificacion = 3339
       v_nss_unificador    CHAR(11),        --NSS que capture el usuario 
       v_ind_no_liq        INTEGER, 
       v_ind_detalles      INTEGER,
       v_ind_seleccion     INTEGER 
DEFINE w ui.Window
DEFINE f ui.Form   

DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion          SMALLINT, -- forma como ejecutara el programa
       p_titulo                  STRING, -- titulo de la ventana
       r_bnd_fin_oper            SMALLINT,
       r_tot_desmarca_unificador INTEGER, 
       r_tot_desmarca_unificado  INTEGER,
       r_folio_liquidacion       DECIMAL(9,0),
       r_bnd_preliquida          SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   LET r_bnd_fin_oper = 0
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2307 -- Unificación de cuentas que no se liquidaron en carga inicial
   LET g_opera_cod   = 1


   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'uni'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   OPEN WINDOW vtn_UNIC09 WITH FORM "UNIL410.4fd" 
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()

   --Permite captura de NSS y Folio de unificación
   INPUT BY NAME v_nss_unificador    
   WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)

   BEFORE INPUT         
      LET v_folio_unificacion = 3339  --Folio de produccion 
      --LET v_folio_unificacion = 4761  --Folio de desarrollo
      DISPLAY BY NAME v_folio_unificacion

      CALL f.setElementHidden("gr_detalles", 1)
   
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
            IF v_ind_no_liq >= 2 THEN 
               CALL f.setElementHidden("gr_detalles",0)

               --Se consulta los NSS Unificador, NSS unificado y AIVS 
               --DISPLAY "ARREGLOS", arr_id_derechohabiente[v_ind_no_liq].*, v_ind_no_liq
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
                         DELETE ROW = FALSE,
                         CANCEL = FALSE)

            AFTER INPUT 
               CALL arr_detalles_liquidar.clear()

            BEFORE INPUT 
      --Crea tabla temporal para preliquidar
      DATABASE safre_tmp

      CREATE TABLE tmp_pre_no_liquidado
      (id_unificador      DECIMAL(9,0),
       id_unificado       DECIMAL(9,0),
       id_derechohabiente DECIMAL(9,0),
       nss_unificador     CHAR(11),
       nss_unificado      CHAR(11),
       total_aivs         DECIMAL(16,6));
       ---
       ---
      DATABASE safre_viv
            
               ON CHANGE v_check_box
                  LET v_ind_seleccion = v_ind_seleccion + 1

                  --Llena arreglo con NSS que se han seleccionados
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_id_unificador      = arr_detalles_liquidar[ARR_CURR()].v_id_unificador
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_id_unificado       = arr_detalles_liquidar[ARR_CURR()].v_id_unificado
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_id_derechohabiente = arr_detalles_liquidar[ARR_CURR()].v_id_derechohabiente
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_nss_unificador     = arr_detalles_liquidar[ARR_CURR()].v_nss_unificador
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_nss_unificado      = arr_detalles_liquidar[ARR_CURR()].v_nss_unificado
                  LET arr_seleccionados_liquidar[v_ind_seleccion].v_total_aivs         = arr_detalles_liquidar[ARR_CURR()].v_total_aivs

                  IF arr_detalles_liquidar[ARR_CURR()].v_check_box = 1 THEN 
                     INSERT INTO safre_tmp:tmp_pre_no_liquidado
                          VALUES (arr_seleccionados_liquidar[v_ind_seleccion].v_id_unificador     ,
                                  arr_seleccionados_liquidar[v_ind_seleccion].v_id_unificado      ,
                                  arr_seleccionados_liquidar[v_ind_seleccion].v_id_derechohabiente,
                                  arr_seleccionados_liquidar[v_ind_seleccion].v_nss_unificador    ,
                                  arr_seleccionados_liquidar[v_ind_seleccion].v_nss_unificado     ,
                                  arr_seleccionados_liquidar[v_ind_seleccion].v_total_aivs        )
                  ELSE 
                     DELETE 
                     FROM   safre_tmp:tmp_pre_no_liquidado
                     WHERE  nss_unificador = arr_seleccionados_liquidar[v_ind_seleccion].v_nss_unificador
                     AND    nss_unificado  = arr_seleccionados_liquidar[v_ind_seleccion].v_nss_unificado                     
                  END IF

                  
                    DATABASE safre_viv

              ON ACTION ACCEPT
                 CALL arr_id_derechohabiente.clear()
                 --Si no se selecciona ningun registro no se debe liquidar nada
                 IF v_ind_seleccion = 0 THEN 
                    CALL fn_mensaje ("Atención", "Debe seleccionar al menos un registro", "stop");
                 ELSE 
                    CALL fn_mensaje ("Atención", 
                                     "Se liquidarán los registros seleccionados \n Puede revisar el avance en el monitor de procesos", 
                                     "info");                    
                    --Ejecuta la función de PRELIQUIDACIóN 
                    CALL fn_preliquida_no_liquidados(p_usuario_cod,
                                                     v_folio_unificacion)
                 END IF 

              ON ACTION regresar
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
--Arreglo que almacena los id_derechohabiente a liquidar
DEFINE rec_id_derechohabiente RECORD
          v_id_derechohabiente INTEGER
END RECORD

       
   LET v_QryTxt = "\n SELECT id_derechohabiente                                     ",
                  "\n FROM   cta_movimiento                                         ",
                  "\n WHERE  id_derechohabiente in (SELECT id_derechohabiente       ",
                  "\n                               FROM   uni_det_unificado        ",
                  "\n                               WHERE  folio_unificacion = ", p_folio_unificacion,
                  "\n                               AND    diagnostico = 6          "

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
   
   PREPARE prp_cons_det_liquidar FROM v_QryTxt
   DECLARE cur_cons_det_liquidar CURSOR FOR prp_cons_det_liquidar

   LET v_ind_no_liq = 1
   FOREACH cur_cons_det_liquidar INTO rec_id_derechohabiente.v_id_derechohabiente
      LET v_ind_no_liq = v_ind_no_liq + 1   
   END FOREACH

      LET arr_id_derechohabiente[v_ind_no_liq].v_id_derechohabiente = rec_id_derechohabiente.v_id_derechohabiente
   
-- CALL arr_id_derechohabiente.deleteElement(arr_id_derechohabiente.getLength())
 
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

   LET v_QryTxt = "\n SELECT a.id_unificador, ",
                  "\n        a.id_unificado, ",
                  "\n        a.id_derechohabiente, ",
                  "\n        a.nsscta2,            ",
                  "\n        a.nsscta1,            ",
                  "\n        SUM(b.monto_acciones),",
                  "\n        0                     ", --1 Unchecked
                  "\n FROM   uni_det_unificado a,  ",
                  "\n        cta_movimiento    b   ",
                  "\n WHERE  a.id_derechohabiente = b.id_derechohabiente",
                  "\n AND    a.folio_unificacion = ",p_folio_unificacion

   IF p_arr_id_derechohabiente.v_id_derechohabiente IS NOT NULL THEN 
      LET v_QryTxt = v_QryTxt || "\n AND    a.id_derechohabiente = ",p_arr_id_derechohabiente.v_id_derechohabiente    
   END IF  

   LET v_QryTxt = v_QryTxt ||   "\n GROUP BY 1,2,3,4,5"
  
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
FUNCTION fn_preliquida_no_liquidados(p_usuario_cod,
                                          p_folio)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio          LIKE glo_folio.folio, -- folio para preliquidar --FOLIO DE UNIFICACION 
       v_s_comando      STRING, -- cadena con una instruccion de consola
       v_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_fin_oper   SMALLINT,
       r_bdn_valida_op  SMALLINT,
       r_bnd_inicializa SMALLINT,
       r_bnd_oper_ini   SMALLINT,
       v_mensaje        STRING,
       v_estado_cod     SMALLINT

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "ARCHIVOAQUI"

   CALL fn_genera_pid(g_proceso_cod,g_opera_cod,p_usuario_cod)
   RETURNING g_pid

   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) 
   RETURNING r_bdn_valida_op 

   IF ( r_bdn_valida_op = 0 ) THEN
      CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,p_folio,"UNIL41",v_nombre_archivo,p_usuario_cod)
      RETURNING r_bnd_inicializa

      IF r_bnd_inicializa = 0 THEN 
   	     -- Inicio operacion.
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0 ,"UNIL41","",p_usuario_cod)
            RETURNING r_bnd_oper_ini
     
         IF (r_bnd_oper_ini = 0) THEN
      
            LET v_s_comando = " fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIP12 ",
                                p_usuario_cod, " ",
                                g_pid  , " " ,
                                g_proceso_cod , " " ,
                                g_opera_cod ," ",
                                p_folio ," ",
                                v_nombre_archivo ," ",
                                " 1>",seg_modulo_bat.ruta_listados clipped ,
                                "/nohup:",g_pid        USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod   USING "&&&&&" ,
                                " 2>&1 &"

            DISPLAY v_s_comando

            RUN v_s_comando

            DISPLAY "Atención, Se ha enviado la preliquidación."
            DISPLAY "Puede revisar el avance del proceso en el "
            DISPLAY "monitor de ejecución de procesos"

         ELSE
            CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
         END IF --Opera_ini
      ELSE 
         CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
      END IF --Inicializa proceso
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
   END IF --Valida operacion 
END FUNCTION