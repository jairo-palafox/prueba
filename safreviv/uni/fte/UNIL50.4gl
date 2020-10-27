--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/06/2012
--===============================================================

#########################################################################################
#Modulo       => UNI                                                                    #
#Programa     => UNIL13                                                                 #
#Objetivo     => Programa que ejecuta el proceso de la generacion de archivos           # 
#                solo INFONAVIT                                                         #
#             => OCI ALS TRM                                                            #
#Fecha inicio => 06/06/2012                                                             #
#########################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
          ruta_bin      CHAR(40),
          ruta_rescate  CHAR(40),
          ruta_listados CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
          ruta_listados CHAR(40)
       END RECORD,
       w               ui.Window,
       f               ui.Form

END GLOBALS
MAIN
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod
       ,p_tipo_ejecucion SMALLINT,
       p_s_titulo        STRING,
       v_folio           DECIMAL (9,0),
       v_folio_liquida   DECIMAL (9,0),
       v_fecha_ini       DATE,
       v_fecha_fin       DATE

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se asigna proceso y operacion
   LET g_proceso_cod = 2316
   LET g_opera_cod   = 1

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
      
      -- se abre la ventana que envia el proceso de preliquidacion
      OPEN WINDOW w_folio_genera WITH FORM "UNIL500"
         -- Recupera punteros a ventana para control de grupos
         LET w = ui.Window.getCurrent()
         LET f = w.getForm()
         
         LET INT_FLAG = FALSE
         -- se asignan los valores por omision
         INPUT v_fecha_ini,
               v_fecha_fin,   
               v_folio_liquida 
         WITHOUT DEFAULTS
         FROM de_fecha_ini,
              de_fecha_fin,
              v_ed_folio
         ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE)

            BEFORE INPUT
               SELECT MIN(f_liquidacion)
               INTO v_fecha_ini
               FROM uni_inf_unificador

               LET v_fecha_fin = TODAY 
         
        
            ON ACTION Generar
               -- se invoca el prceso que crea el archivo de salida
               CALL fn_ejecuta_generacion_archivo_TRM_INF(v_folio, p_usuario_cod, v_fecha_ini, v_fecha_fin)
                  EXIT INPUT

            ON ACTION CANCEL
               LET INT_FLAG = TRUE
               EXIT INPUT
         END INPUT
      CLOSE WINDOW w_folio_genera   
END MAIN

{
======================================================================
Clave: 
Nombre: fn_muestra_resumen_INFONAVIT
Fecha creacion: 24/05/2012
Narrativa del proceso que realiza:
Ejecuta la consulta de resumen de archivo a generar solo INFONAVIT
======================================================================
}
FUNCTION fn_muestra_resumen_INFONAVIT(p_folio)
DEFINE p_folio, v_folio     LIKE glo_folio.folio,
       v_i_unificador   SMALLINT, -- Contador de numero de solicitudes por periodo
       v_i_unificado    SMALLINT, -- Contador de numero de registro de trabajadores
       v_i_tot_registros SMALLINT -- Contador de numero de registro de registros
       
   LET v_i_unificador = 0
   LET v_i_unificado = 0
   LET v_i_tot_registros = 0
    
   
   SELECT COUNT(*)
     INTO v_i_unificador
   FROM uni_inf_unificador
   WHERE folio_unificacion = p_folio
     AND estado_unificacion = 1
     AND estado_familia = 1
   
   SELECT COUNT(*)
     INTO v_i_unificado
   FROM uni_inf_unificado
   WHERE folio_unificacion = p_folio
     AND estado_unificacion = 1
     AND id_unificador IN (SELECT id_inf_unificador
                           FROM   uni_inf_unificador
                           WHERE  folio_unificacion = p_folio
                           AND    estado_familia = 1)
                                  
    
   DISPLAY p_folio, v_i_unificador, v_i_unificado, 
                    v_i_unificador
        TO cmb_folio, v_i_unificador, v_i_unificado, 
                    v_i_tot_registros
  END FUNCTION

#OBJETIVO: Ejecutar el lanzado de la generación del archivo de salida. 
FUNCTION fn_ejecuta_generacion_archivo_TRM_INF(p_folio, p_usuario, p_fecha_ini, p_fecha_fin)
DEFINE p_usuario         LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_mensaje         STRING,
       r_bnd_opera_ini   SMALLINT,
       p_fecha_ini       DATE,
       p_fecha_fin       DATE,
       r_bnd_valida_op   SMALLINT,
       v_inicia_proceso  SMALLINT

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"

   IF p_folio IS NULL THEN
      LET p_folio = 0  
   END IF 

   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) 
   RETURNING r_bnd_valida_op

   IF ( r_bnd_valida_op = 0 ) THEN  
       --Si la operación es válida se obtiene el ID del proceso  
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario )
      RETURNING g_pid
      
      --Inicia proceso
      CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"UNIL50","",p_usuario )
      RETURNING v_inicia_proceso

      IF (v_inicia_proceso = 0)THEN
         --Inicio operacion.
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"UNIL50","",p_usuario )
         RETURNING r_bnd_opera_ini

         IF (r_bnd_opera_ini = 0) THEN
         
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/UNIS11 ",
                             p_usuario , " ",
                             g_pid  , " " ,
                             g_proceso_cod , " " ,
                             g_opera_cod ," ",
                             p_folio ," ",
                             v_nombre_archivo CLIPPED," ",
                             p_fecha_ini ," ",
                             p_fecha_fin ," ",
                             " 1>",seg_modulo_bat.ruta_listados clipped ,
                             "/nohup:",g_pid        USING "&&&&&",":",
                             g_proceso_cod USING "&&&&&",":",
                             g_opera_cod   USING "&&&&&" ,
                             " 2>&1 &"
                            
          DISPLAY v_s_comando
          RUN v_s_comando
          
          CALL fn_mensaje("Atención","Se ha enviado la generación de archivo.\n"||
                          "Puede revisar el avance del proceso en el monitor de "||
                          "ejecución de procesos","information")
         ELSE
            CALL fn_recupera_inconsis_opera(r_bnd_opera_ini) RETURNING v_mensaje
            CALL fn_mensaje("Atención", v_mensaje, "stop")     
         END IF  --Actualiza opera_ini 
      ELSE
         CALL fn_recupera_inconsis_opera(v_inicia_proceso) RETURNING v_mensaje
         CALL fn_mensaje("Atención", v_mensaje, "stop")     
      END IF -- Inicia Proceso
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_valida_op) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")     
   END IF --Valida operación
END FUNCTION