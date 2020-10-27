--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/03/2014
--===============================================================

################################################################################
#Modulo       => DAC                                                           #
#Programa     => DACL05                                                        #
#Objetivo     => Lanzador Generar Archivo de Salida  Devolución de Amortización#
#                Mejora tu Casa                                                #
#Fecha inicio => 05/03/2014                                                    #
################################################################################
DATABASE safre_viv
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
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,v_folio          LIKE deo_preliquida.folio_liquida
       ,v_s_cadena       STRING -- cadena de texto
       ,v_cbx_folios     ui.ComboBox -- combo de afores
       ,v_i_conArch      INTEGER
       ,v_count_estado   SMALLINT
       ,v_r_glo_ctr_archivo      RECORD
          folio          DECIMAL(9,0),
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
       END RECORD

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
   LET g_proceso_cod = 2601
   LET g_opera_cod   = 5 -- generacion de archivo
   
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
   
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'dac'
   
   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW vtn_genera_archivo WITH FORM "DACL050"
      -- Recupera punteros a ventana para control de grupos
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
      -- se le asigna el apuntado der combo a la variable
      LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
      -- Ocultar tablas de resultados
      CALL f.setElementHidden("grupo_resumen",1)
      LET INT_FLAG = FALSE
      -- se inicia el combobox en blanco
      CALL v_cbx_folios.clear()
      -- se llena el arreglo de folios
      DECLARE cur_folios CURSOR FOR
      SELECT DISTINCT g.folio, a.nombre_archivo
        FROM glo_ctr_archivo a, glo_folio g
       WHERE a.proceso_cod = g_proceso_cod
         AND a.proceso_cod = g.proceso_cod
         AND a.folio = g.folio
         AND g.status = 2
      
      LET v_i_conArch = 0
      FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
         LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
             v_r_glo_ctr_archivo.nombre_archivo 
         CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_s_cadena)
         -- Contador de archivos eoncontrados
         LET v_i_conArch = v_i_conArch + 1
      END FOREACH
            
      IF(v_i_conArch<1)THEN
         CALL fn_mensaje("Atención","No existen archivos resientemente liquidados","info")
         CLOSE WINDOW vtn_genera_archivo
         RETURN
      END IF
      -- Se libera el cursor
      FREE cur_folios
      CALL v_cbx_folios.addItem(-1," ")
      -- se asignan los valores por omision
      LET v_folio = -1
      INPUT v_folio WITHOUT DEFAULTS
         FROM cmb_folio
      ATTRIBUTES (UNBUFFERED)
      BEFORE INPUT
      
      ON ACTION ACCEPT
         IF ( v_folio IS NULL OR v_folio = -1 ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF

         --Muestra el resúmen de información para el archivo 
         CALL fn_muestra_resumen(v_folio)
         
         -- Ocultar nuevamente las tablas de resultados
         CALL f.setElementHidden("grupo_resumen",0)
         
         ON ACTION Generar
            -- se invoca el prceso que crea el archivo de salida
            CALL fn_dac_ejecuta_generacion_archivo_MTC(v_folio, p_usuario_cod)
            EXIT INPUT
         ON ACTION CANCEL
            LET INT_FLAG = TRUE
            EXIT INPUT
      END INPUT
   CLOSE WINDOW vtn_genera_archivo   
END MAIN

#OBJETIVO: Ejecuta la consulta de resumen de archivo a generar
FUNCTION fn_muestra_resumen(p_folio)
DEFINE p_folio LIKE glo_folio.folio,
       v_s_sql STRING,
       v_tot_aceptados    INTEGER,
       v_tot_rechazados   INTEGER,
       v_tot_solicitudes  INTEGER,
       v_monto_aceptados  DECIMAL(16,6),
       v_monto_rechazados DECIMAL(16,6),
       v_monto_total      DECIMAL(16,6)       
       
   --Obtiene el total de ACEPTADOS 
   SELECT COUNT(id_dac_solicitud),
          SUM(imp_amortizacion)
   INTO   v_tot_aceptados, 
          v_monto_aceptados           
   FROM   dac_det_solicitud
   WHERE  folio_integracion = p_folio
   AND    resul_opera = 1;

   --Obtiene el total de RECHAZADOS
   SELECT COUNT(id_dac_solicitud),
          SUM(imp_amortizacion)
   INTO   v_tot_rechazados, 
          v_monto_rechazados           
   FROM   dac_det_solicitud
   WHERE  folio_integracion = p_folio
   AND    resul_opera = 2;

   --Obtiene el total de registros
   SELECT COUNT(id_dac_solicitud),
          SUM(imp_amortizacion)
   INTO   v_tot_solicitudes, 
          v_monto_total           
   FROM   dac_det_solicitud
   WHERE  folio_integracion = p_folio;

   --Inicia variables en ceros.
   IF v_tot_aceptados IS NULL THEN 
      LET v_tot_aceptados = 0
   END IF 
   IF v_monto_aceptados IS NULL THEN 
      LET v_monto_aceptados = 0
   END IF 
   IF v_tot_rechazados IS NULL THEN
      LET v_tot_rechazados = 0
   END IF  
   IF v_monto_rechazados IS NULL THEN
      LET v_monto_rechazados = 0
   END IF 
   IF v_tot_solicitudes IS NULL THEN
      LET v_tot_solicitudes = 0
   END IF  
   IF v_monto_total IS NULL THEN
      LET v_monto_total = 0
   END IF  
   
   DISPLAY v_tot_aceptados,
           v_monto_aceptados,
           v_tot_rechazados,
           v_monto_rechazados,
           v_tot_solicitudes,
           v_monto_total
   TO      ed_tot_aceptados,
           ed_monto_aceptados,
           ed_tot_rechazados,
           ed_monto_rechazados,
           ed_tot_solicitudes,
           ed_monto_total
  END FUNCTION

#OBJETIVO: Ejecutar el lanzado que genera el archivo de salida
FUNCTION fn_dac_ejecuta_generacion_archivo_MTC(p_folio, p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_s_comando      STRING,
       p_folio          DECIMAL(9,0),
       v_mensaje        STRING,
       r_bnd_valida     SMALLINT,
       r_bnd_opera_ini  SMALLINT

   LET p_nombre_archivo = "NA"       
   -- Se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bnd_valida

   IF ( r_bnd_valida = 0 ) THEN
      -- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"DACL05 ","",p_usuario_cod)
      RETURNING r_bnd_opera_ini

      IF (r_bnd_opera_ini = 0) THEN
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/DACS01 ",
                            p_usuario_cod CLIPPED, " ",
                            g_pid  , " " ,
                            g_proceso_cod , " " ,
                            g_opera_cod ," ",                            
                            p_folio, " ",
                            "'",p_nombre_archivo CLIPPED, "' ",
                            " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                            "/nohup:",g_pid USING "&&&&&",":",
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
      END IF       
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_valida) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   END IF
END FUNCTION