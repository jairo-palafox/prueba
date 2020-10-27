--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/Nov/2013
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => DAE                                                           #
#Programa     => DAEL20                                                        #
#Objetivo     => Programa lanzador para generar el archivode Ajuste            #
#                Amortizaciones Excedentes                                     #
#Fecha inicio => 28/Nov/2013                                                   #
################################################################################

--Lanzado: DAES03

DATABASE safre_viv
GLOBALS "DAEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
          ruta_exp      CHAR(40),
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
DEFINE p_usuario        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_titulo         STRING, -- titulo de la ventana
       v_folio          LIKE deo_preliquida.folio_liquida,
       v_s_cadena       STRING, -- cadena de texto
       v_cbx_folios     ui.ComboBox, -- combo de afores
       v_i_conArch      INTEGER,
       r_folio          DECIMAL(9,0)
DEFINE v_r_glo_ctr_archivo      RECORD 
          folio          DECIMAL(9,0),
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
END RECORD

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo   IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo  )
   END IF
   
      -- se asigna proceso y operacion
      LET g_proceso_cod = 2403 --Ajuste Amortización Excedente
      LET g_opera_cod   = 5    --Generar archivo salida
      
      -- se obtiene el PID del proceso
      SELECT MAX(pid)
        INTO g_pid
        FROM bat_ctr_proceso
       WHERE proceso_cod = g_proceso_cod
     
      -- se obtienen las rutas de control del modulo
      SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
        INTO g_reg_modulo.*
        FROM seg_modulo s
       WHERE s.modulo_cod = 'dae'
      
       SELECT b.ruta_listados
         INTO seg_modulo_bat.ruta_listados
         FROM seg_modulo b
        WHERE b.modulo_cod = 'bat'
      
      -- se abre la ventana que envia el proceso de preliquidacion
      OPEN WINDOW w_folio_genera WITH FORM "DAEL200"
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
                                       SELECT g.folio, a.nombre_archivo
                                       FROM glo_ctr_archivo a, glo_folio g
                                       WHERE a.proceso_cod = g_proceso_cod
                                       AND a.proceso_cod = g.proceso_cod
                                       AND a.folio = g.folio
                                       AND g.status = 0
                                       ORDER BY 1 DESC
         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.folio,
                                 v_r_glo_ctr_archivo.nombre_archivo
        
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH

         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención","No existen archivos recientemente liquidados","info")
            CLOSE WINDOW w_folio_genera
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
               
               CALL fn_muestra_resumen(v_folio)
               RETURNING r_folio
               -- Ocultar nuevamente las tablas de resultados
               CALL f.setElementHidden("grupo_resumen",0)

            ON ACTION Generar
               -- se invoca el prceso que crea el archivo de salida
               CALL fn_dae_generacion_archivo(r_folio, p_usuario       )
                  EXIT INPUT

            ON ACTION CANCEL
               LET INT_FLAG = TRUE
               EXIT INPUT
         END INPUT
      CLOSE WINDOW w_folio_genera   
END MAIN

#OBJETIVO: Ejecutar la consulta de resumen de archivo a generar.
FUNCTION fn_muestra_resumen(p_folio)
DEFINE p_folio, v_folio     LIKE glo_folio.folio,
       v_i_tot_registros    INTEGER, -- Contador de numero de registro de registros
       v_d_tot_vivienda     DECIMAL(16,2), -- Contador de numero de vivienda
       v_s_sql              STRING, -- cadena con una instruccion SQL
       v_folio_lote         DECIMAL(9,0),
       v_suma_aceptados_amort   DECIMAL(16,2),      
       v_total_aceptados        INTEGER,
       v_suma_rechazados_amort  DECIMAL(16,2),      
       v_total_rechazados       INTEGER,
       v_suma_pendientes_amort DECIMAL(16,2),
       v_total_pendientes INTEGER
       
   LET v_i_tot_registros = 0
   LET v_d_tot_vivienda = 0
   
   LET v_folio = p_folio

   DISPLAY "FOLIO LOTE ", v_folio

   LET v_s_sql = "\n SELECT COUNT(*)",
                 "\n FROM   dae_det_ajuste ",
                 "\n WHERE  folio_lote =",v_folio
--   DISPLAY v_s_sql
   
   PREPARE Prpr_Obt_sumas FROM v_s_sql CLIPPED
   EXECUTE Prpr_Obt_sumas INTO v_i_tot_registros
      --Total de Aceptados
      SELECT COUNT(nss)
      INTO   v_total_aceptados
      FROM   dae_det_ajuste
      WHERE  folio_lote = v_folio
      AND    resul_operacion = 1;

      IF v_total_aceptados = 0 THEN
         LET v_total_aceptados      = 0
      END IF 
       
      --Total de Rechazados       
      SELECT COUNT(nss)
      INTO   v_total_rechazados
      FROM   dae_det_ajuste
      WHERE  folio_lote = v_folio
      AND    resul_operacion = 2;

      IF v_total_rechazados = 0 THEN
         LET v_total_rechazados      = 0
      END IF
 
   DISPLAY p_folio, 
           v_i_tot_registros, 
           v_total_aceptados,
           v_total_rechazados

        TO cmb_folio, 
           v_i_tot_registros, 
           v_i_tot_aceptados,
           v_i_tot_rechazados

   RETURN p_folio

END FUNCTION

#OBJETIVO: Ejecutar la generación del archivo de salida de detalles.
FUNCTION fn_dae_generacion_archivo(p_folio, p_usuario       )
DEFINE p_usuario         LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_mensaje         STRING,
       bnd_valida_op     SMALLINT,
       bnd_opera_ini     SMALLINT

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"

   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) 
   RETURNING bnd_valida_op
   IF ( bnd_valida_op = 0 ) THEN
      -- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,p_folio,"DAEL20","",p_usuario)
      RETURNING bnd_opera_ini
      IF ( bnd_opera_ini = 0) THEN
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/DAES03 ",
                             p_usuario CLIPPED, " ",
                             g_pid  , " " ,
                             g_proceso_cod , " " ,
                             g_opera_cod ," ",
                             p_folio ," ",
                             v_nombre_archivo CLIPPED," ",
                             " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                             "/nohup:",g_pid USING "&&&&&",":",
                             g_proceso_cod   USING "&&&&&",":",
                             g_opera_cod     USING "&&&&&" ,
                             " 2>&1 &"
                         
         DISPLAY v_s_comando
         RUN v_s_comando
         
         CALL fn_mensaje("Atención","Se ha enviado la generación de archivo.\n"||
                         "Puede revisar el avance del proceso en el monitor de "||
                          "ejecución de procesos","information")
      ELSE
         CALL fn_recupera_inconsis_opera(bnd_opera_ini) RETURNING v_mensaje
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF         
   ELSE
      CALL fn_recupera_inconsis_opera(bnd_valida_op) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje,"information") 
   END IF
END FUNCTION