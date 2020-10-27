--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/11/2016
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEL23                                                        #
#Objetivo     => Programa que ejecuta la generacion de archivo de diagnósticos #
#Fecha inicio => Noviembre 28, 2016                                            #
################################################################################
GLOBALS "DPEG01.4gl"
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
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,v_folio          LIKE deo_preliquida.folio_liquida
       ,v_s_cadena       STRING -- cadena de texto
       ,v_cbx_folios     ui.ComboBox -- combo de afores
       ,v_i_conArch      INTEGER
       ,v_count_estado   SMALLINT
       ,v_r_glo_ctr_archivo      RECORD
          folio           DECIMAL(9,0)
          ,nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
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
   LET g_proceso_cod = 1001
   LET g_opera_cod   = 7  --Preliquidación 
   
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
  
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'dpe'
   
   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
   
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_genera WITH FORM "DPEL230.42f"
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
      DECLARE cur_folios CURSOR FOR SELECT g.folio, a.nombre_archivo
                                    FROM   glo_ctr_archivo a, glo_folio g
                                    WHERE  a.proceso_cod = g_proceso_cod
                                    AND    a.proceso_cod = g.proceso_cod
                                    AND    a.folio = g.folio
                                    AND    g.status = 0
                                    AND    a.opera_cod = 1
                                    ORDER BY 1
      
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
         CLOSE WINDOW w_folio_genera
         RETURN
      END IF
      -- Se libera el cursor
      FREE cur_folios
      CALL v_cbx_folios.addItem(-1," ")
      -- se asignan los valores por omision
      LET v_folio = -1
      INPUT v_folio  WITHOUT DEFAULTS
      FROM cmb_folio ATTRIBUTES (UNBUFFERED)
      BEFORE INPUT
         ON ACTION ACCEPT
            IF ( v_folio IS NULL OR v_folio = -1 ) THEN
               CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
               CONTINUE INPUT
            END IF
            
            CALL fn_muestra_resumen_IMSS(v_folio)
            -- Ocultar nuevamente las tablas de resultados
            CALL f.setElementHidden("grupo_resumen",0)
            -- Limpia datos para nueva consulta
            
         ON ACTION Generar
            LET v_folio = v_r_glo_ctr_archivo.folio
            
            DISPLAY "FOLIO ANTES DEL ENVIO ",v_folio 
            -- se invoca el prceso que crea el archivo de salida
            CALL fn_dpe_ejecuta_generacion_archivo_IMSS(v_folio, p_usuario_cod)
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
Nombre: fn_muestra_resumen_IMSS
Fecha creacion: 24/05/2012
Narrativa del proceso que realiza:
Ejecuta la consulta de resumen de archivo a generar solo IMSS
======================================================================
}
FUNCTION fn_muestra_resumen_IMSS(p_folio)
DEFINE p_folio, v_folio     LIKE glo_folio.folio,
       v_i_sol_patronales   SMALLINT, -- Contador de numero de solicitudes por periodo
       v_i_reg_trabajadores SMALLINT, -- Contador de numero de registro de trabajadores
       v_i_tot_registros    SMALLINT, -- Contador de numero de registro de registros
       v_d_tot_vivienda     DECIMAL(16,2), -- Contador de numero de vivienda
       v_d_tot_avis         DECIMAL(16,2), -- Contador de numero de avis
       v_s_sql              STRING -- cadena con una instruccion SQL
       
   LET v_i_tot_registros = 0
   LET v_i_sol_patronales = 0
   LET v_i_reg_trabajadores = 0
   LET v_d_tot_vivienda = 0
   LET v_d_tot_avis = 0

   LET v_folio = p_folio

   LET v_s_sql = "SELECT SUM(d.imp_viv_dev), SUM(d.aivs_viv_dev), COUNT(*)",
                          "\n FROM dpe_resp_procesar d, glo_folio g",
                          "\n WHERE d.folio = g.folio",
                          "\n AND d.folio =",v_folio
   DISPLAY v_s_sql
   
   PREPARE Prpr_Obt_sumas FROM v_s_sql CLIPPED
   EXECUTE Prpr_Obt_sumas INTO v_d_tot_vivienda, 
                               v_d_tot_avis,
                               v_i_reg_trabajadores
                               
   LET v_s_sql = "SELECT COUNT(*)",
                 "\n FROM dpe_patron a",
                 "\n WHERE a.folio =",v_folio
   
   PREPARE Prpr_Obt_patrones FROM v_s_sql CLIPPED
   EXECUTE Prpr_Obt_patrones INTO v_i_sol_patronales
   
   LET v_i_tot_registros =  v_i_sol_patronales + v_i_reg_trabajadores
                               
   DISPLAY BY NAME v_i_sol_patronales, v_i_reg_trabajadores, 
                            v_i_tot_registros, v_d_tot_vivienda, v_d_tot_avis
   
   DISPLAY p_folio, v_i_sol_patronales, v_i_reg_trabajadores, 
                    v_i_tot_registros, v_d_tot_vivienda, v_d_tot_avis
        TO cmb_folio, v_i_sol_patronales, v_i_reg_trabajadores, 
                    v_i_tot_registros, v_d_tot_vivienda, v_d_tot_avis
  END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_dpe_ejecuta_generacion_archivo_IMSS
Fecha creacion: 24/05/2012
Narrativa del proceso que realiza:
Ejecuta el proceso que genera el archivo de salida para dependecia de patrones
======================================================================
}

FUNCTION fn_dpe_ejecuta_generacion_archivo_IMSS(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_fin_oper    SMALLINT,
       v_mensaje         STRING

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"

   -- se verifica si se puede continuar con la operacion
   IF ( fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
       display "antes:",p_folio
   	   -- Inicio operacion.
       CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,p_folio,"DPEL23",
                                 "",p_usuario_cod)
          RETURNING r_bnd_fin_oper
       IF (r_bnd_fin_oper = 0) THEN
 display "despues:",p_folio   	                   
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/DPES03 ",
                             p_usuario_cod CLIPPED, " ",
                             g_pid  , " " ,
                             g_proceso_cod , " " ,
                             g_opera_cod ," ",
                             p_folio ," ",
                             v_nombre_archivo CLIPPED," ",
                             " 1>",seg_modulo_bat.ruta_listados clipped ,
                             "/nohup:",g_pid        USING "&&&&&",":",
                             g_proceso_cod USING "&&&&&",":",
                             g_opera_cod   USING "&&&&&" ,
                             " 2>&1 &"
                            
                            DISPLAY v_s_comando
                            
          RUN v_s_comando
          CALL fn_mensaje("Atención","Se ha enviado la generación de archivo.\n"||
               "Puede revisar el avance en el monitor de "||
               "ejecución de procesos","information")
       ELSE
          CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje

          CALL fn_mensaje("Atención", v_mensaje, "stop")
       END IF        
   END IF

END FUNCTION