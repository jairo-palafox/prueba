--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/06/2012
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIL23                                                        #
#Objetivo     => Programa que ejecuta el stored procedure que realiza los      #
#                indicadores de crédito y desmarca de cuentas solo INFONAVIT   #
#Fecha inicio => 06/06/2012                                                    #
################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD,
       w               ui.Window,
       f               ui.Form

DEFINE r_orig_credito RECORD
          v_tipo_originacion SMALLINT,
          v_tipo_origin_des  VARCHAR(25),
          v_tipo_credito     SMALLINT,
          v_tipo_credito_des VARCHAR(25),
          v_totales          INTEGER
END RECORD 

DEFINE arr_orig_credito DYNAMIC ARRAY OF RECORD
          v_tipo_originacion SMALLINT,
          v_tipo_origin_des  VARCHAR(25),
          v_tipo_credito     SMALLINT,
          v_tipo_credito_des VARCHAR(25),
          v_totales          INTEGER
END RECORD 

END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT,-- forma como ejecutara el programa
       p_s_titulo       STRING ,-- titulo de la ventana
       v_folio          DECIMAL(9,0),--LIKE deo_preliquida.folio_liquida
       v_s_cadena       STRING, -- cadena de texto
       v_cbx_folios     ui.ComboBox, -- combo de afores
       v_i_conArch      INTEGER,
       v_r_glo_ctr_archivo      RECORD
          folio          DECIMAL(9,0),
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
       END RECORD,
       v_total_desmarca_unificador  SMALLINT,
       v_total_desmarca_unificado   SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_uni_infonavit -- Unificacion de cuentas solo INFONAVIT
   LET g_opera_cod   = 5 -- indicadores

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'uni'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
  
   -- se abre la ventana que envia el proceso de indicadores de credito
   OPEN WINDOW w_folio_indicadores WITH FORM "UNIL230"
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
      -- se le asigna el apuntado der combo a la variable
      LET v_cbx_folios = ui.ComboBox.forName("cmb_folio")

      --Oculta las secciones de detalle de unificadores y unificados 
      CALL f.setElementHidden("grup_previa",1)
      CALL f.setElementHidden("gr_desmarca_unificados",1)
      CALL f.setElementHidden("grid_indicadores_unificadores",1)
      
      LET INT_FLAG = FALSE
      -- se inicia el combobox en blanco
      --CALL v_cbx_folios.clear()
      -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
                 SELECT UNIQUE g.folio, 
                               a.nombre_archivo
                   FROM glo_ctr_archivo a, 
                        glo_folio g,
                        uni_inf_unificador b
                  WHERE a.proceso_cod = g_proceso_cod
                    AND a.proceso_cod = g.proceso_cod
                    AND a.folio = g.folio
                    AND b.folio_unificacion = g.folio
                    AND g.status = 2
                    AND b.diagnostico = 4
 
         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", v_r_glo_ctr_archivo.nombre_archivo 
                             
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH

   
         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente liquidados","info")
            CLOSE WINDOW w_folio_indicadores
            RETURN
         END IF
         FREE cur_folios
         CALL v_cbx_folios.addItem(-1," ")
         -- se asignan los valores por omision
         LET v_folio = -1
         -- se captura el folio
         INPUT v_folio WITHOUT DEFAULTS
            FROM cmb_folio
         ATTRIBUTES (UNBUFFERED)
         BEFORE INPUT
         ON ACTION ACCEPT
            IF (  v_folio IS NULL OR v_folio = -1) THEN
               CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
               CONTINUE INPUT
            ELSE
               --Muestra la sección de detalle de unificados
               CALL f.setElementHidden("grup_previa",0)
               CALL f.setElementHidden("gr_desmarca_unificados",0)
               --Recuperar recuperar detalles de unificados y unificadores, totales a desmarcar e inhabilitar 
               CALL fn_muestra_resumen_indicador(v_folio) 
               RETURNING v_total_desmarca_unificador, v_total_desmarca_unificado  
            END IF

            EXIT INPUT 

         ON ACTION CANCEL
            LET INT_FLAG = TRUE
            EXIT INPUT
         END INPUT                      

   --Muestra detalles de unificados 
   DISPLAY ARRAY arr_orig_credito TO scr_unificados.*
   ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)   
   BEFORE DISPLAY 
   DISPLAY v_folio TO cmb_folio 
   CALL ui.Interface.refresh()
  
      --Sale del programa al dar cancelar           
      ON ACTION cancelar
         EXIT PROGRAM
      
      --Muestra totales de unificados inhabilitar 
      ON ACTION Actualizacion
         --Muestra la sección de detalle de unificadores
         CALL f.setElementHidden("grid_indicadores_unificadores",0)
         DISPLAY v_total_desmarca_unificado TO txt_tot_des_do1
         --Muestra detalles de unificadores 
         DISPLAY ARRAY arr_orig_credito TO scr_unificadores.*
         ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)        
            ON ACTION cancelar
               EXIT DISPLAY       
            --Ejecuta la función de marca y desmarca de índicadores 
            ON ACTION Aplicar
               CALL fn_uni_ejecuta_indicadores_INFONAVIT(v_folio, p_usuario_cod)
               EXIT PROGRAM
         END DISPLAY  --Termina display UNIFICADORES 
      EXIT DISPLAY  --Se sale del display de UNIFOCADOS                              
   END DISPLAY --Termina display UNIFICADOS
         
   CLOSE WINDOW w_folio_indicadores
END MAIN

{======================================================================
Clave: 
Nombre: fn_uni_ejecuta_preliquidacion_INFONAVIT
Fecha creacion: 22/05/2012
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de unificacion de cuentas solo INFONAVIT
======================================================================}
FUNCTION fn_muestra_resumen_indicador(p_folio_unificacion)
DEFINE p_folio_unificacion            DECIMAL(9,0),
       v_folio_unificacion            DECIMAL(9,0),
       v_id_derechohabiente_unificado DECIMAL(9,0), -- id_derechohabiente del NSS unificado
       v_id_derechohabiente_unificador DECIMAL(9,0), -- id_derechohabiente del NSS UNIFICADOR
       v_nss_unificador            CHAR(11),
       v_s_sql                     STRING,
       v_cred_total                SMALLINT,
       v_cred_total_1              SMALLINT,
       v_cred_total_2              SMALLINT,
       v_cred_total_3              SMALLINT,
       v_desmarca_unificador       SMALLINT,
       v_total_desmarca_unificador SMALLINT,
       v_desmarca_unificado        SMALLINT,
       v_total_desmarca_unificado  SMALLINT,
       v_indice                    INTEGER,
       v_id_unificador             DECIMAL(9,0),
       v_indice_2                  INTEGER
       
   LET v_cred_total    = 0
   LET v_cred_total_1  = 0
   LET v_cred_total_2  = 0
   LET v_cred_total_3  = 0
   LET v_total_desmarca_unificador = 0
   LET v_total_desmarca_unificado = 0

   -- Se selecciona el registro de uni det_unificador
   -- Para saber si cuenta con credito
   
   DECLARE Curr_creditos CURSOR FOR
    SELECT b.id_inf_unificador,
           b.nss, 
           b.id_derechohabiente
      FROM uni_inf_unificador b
     WHERE b.estado_familia = 1
       AND b.folio_unificacion = p_folio_unificacion
       
   FOREACH Curr_creditos INTO v_id_unificador,
   	                          v_nss_unificador, 
                              v_id_derechohabiente_unificador
   	  
   	  DECLARE Curr_creditos_2 CURSOR FOR 
      
   	  SELECT id_derechohabiente
	      FROM uni_inf_unificado
	     WHERE id_unificador = v_id_unificador
	     
	    FOREACH Curr_creditos_2 INTO v_id_derechohabiente_unificado        
         IF v_id_derechohabiente_unificado IS NOT NULL THEN
            LET v_s_sql = "\n SELECT ca.tpo_originacion,TRIM(tr.originacion_desc),",
                          "\n        ca.tpo_credito,TRIM(tc.desc_credito),COUNT(*) ",
                          "\n   FROM cre_acreditado ca, ",
                          "\n        cat_cre_originacion tr, ",
                          "\n        cat_tipo_credito tc ",
                          "\n  WHERE ca.id_derechohabiente IN ", 
                          "\n                               (SELECT id_derechohabiente ",
                          "\n                                FROM   uni_inf_unificado ",
                          "\n                                WHERE  id_unificador IN ",
                          "\n                                                        (SELECT id_inf_unificador      ",
                          "\n                                                         FROM   uni_inf_unificador ",
                          "\n                                                         WHERE  folio_unificacion = ",p_folio_unificacion,
                          "\n                                                         AND    estado_familia = 1 ",
                          "\n                                                         AND    diagnostico = 4))  ",
                          "\n    AND ca.tpo_originacion = tr.tpo_originacion ",
                          "\n    AND ca.tpo_credito = tc.tpo_credito ",
                          "\n    AND ca.tpo_originacion = tc.tpo_originacion",                          
                          "\n    AND ca.estado <> 230 ",
                          "\n    GROUP BY 1,2,3,4 ", 
                          "\n    ORDER BY 1,3 " 
	         -- DISPLAY "v_s_sql: ", v_s_sql CLIPPED
	          
	          PREPARE Prp_derecho_cre FROM v_s_sql CLIPPED
	          DECLARE Curr_derecho_cre CURSOR FOR Prp_derecho_cre
         
            LET v_indice = 1
         
	          FOREACH Curr_derecho_cre INTO r_orig_credito.* 
                 LET arr_orig_credito[v_indice].v_tipo_originacion = r_orig_credito.v_tipo_originacion
                 LET arr_orig_credito[v_indice].v_tipo_origin_des = r_orig_credito.v_tipo_origin_des
                 LET arr_orig_credito[v_indice].v_tipo_credito = r_orig_credito.v_tipo_credito
                 LET arr_orig_credito[v_indice].v_tipo_credito_des = r_orig_credito.v_tipo_credito_des
                 LET arr_orig_credito[v_indice].v_totales = r_orig_credito.v_totales
         
                 LET v_indice = v_indice + 1
	          END FOREACH
         
            LET v_indice_2 = 1
         
            FOR v_indice = 2 TO   arr_orig_credito.getLength()
               IF arr_orig_credito[v_indice].v_tipo_originacion = arr_orig_credito[v_indice_2].v_tipo_originacion  THEN
                  LET arr_orig_credito[v_indice].v_tipo_originacion = NULL
                  LET arr_orig_credito[v_indice].v_tipo_origin_des  = ""
               ELSE 
                  LET v_indice_2 = v_indice
               END IF 
            END FOR
            
            SELECT COUNT(*)
	        INTO   v_desmarca_unificado
	        FROM   sfr_marca_activa
	        WHERE  marca = 504
	        AND    id_derechohabiente = v_id_derechohabiente_unificado
         
	           LET v_total_desmarca_unificado = v_total_desmarca_unificado +
	                                            v_desmarca_unificado
         END IF
      END FOREACH
      
      SELECT COUNT(*)
	  INTO   v_desmarca_unificador
	  FROM   sfr_marca_activa
	  WHERE  marca = 503
	  AND    id_derechohabiente = v_id_derechohabiente_unificador
	     
	     LET v_total_desmarca_unificador = v_total_desmarca_unificador +
	                                       v_desmarca_unificador

   END FOREACH

   DISPLAY v_total_desmarca_unificador, v_total_desmarca_unificado
        TO txt_tot_des_dor, txt_tot_des_do

   RETURN v_total_desmarca_unificador, v_total_desmarca_unificado

END FUNCTION

{======================================================================
Clave: 
Nombre: fn_uni_ejecuta_preliquidacion_INFONAVIT
Fecha creacion: 22/05/2012
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de unificacion de cuentas solo INFONAVIT
======================================================================}
FUNCTION fn_uni_ejecuta_indicadores_INFONAVIT(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_valida_operacion SMALLINT,
       r_bnd_opera_ini   SMALLINT,
       v_mensaje         STRING

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"
   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bnd_valida_operacion 
   
      IF (r_bnd_valida_operacion = 0) THEN
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"UNIL07","",p_usuario_cod)
         RETURNING r_bnd_opera_ini
         IF r_bnd_opera_ini = 0 THEN 
            LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIP05 ",
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
            CALL fn_mensaje("Atención","Se ha enviado la actualización de indicadores.\n"||
                            "Puede revisar el avance del proceso en el monitor de "||
                            "ejecución de procesos","information")
         ELSE
            CALL fn_recupera_inconsis_opera(r_bnd_opera_ini) RETURNING v_mensaje
            CALL fn_mensaje("Atención", v_mensaje, "stop")
         END IF
      ELSE
         CALL fn_recupera_inconsis_opera(r_bnd_valida_operacion) RETURNING v_mensaje
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF
 
END FUNCTION