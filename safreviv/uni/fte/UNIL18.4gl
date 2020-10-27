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
          folio          DECIMAL(9,0)
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
      LET g_proceso_cod = g_proceso_cod_uni_infonavit -- Unificación de cuentas
      LET g_opera_cod   = 6 -- generacion de archivo

      -- se obtiene el PID del proceso
      SELECT MAX(pid)
      INTO   g_pid
      FROM   bat_ctr_proceso
      WHERE  proceso_cod = g_proceso_cod
      
      -- se obtienen las rutas de control del modulo
      SELECT s.ruta_bin, 
             s.ruta_rescate, 
             s.ruta_listados
      INTO   g_reg_modulo.*
      FROM   seg_modulo s
      WHERE  s.modulo_cod = 'uni'
      
      SELECT b.ruta_listados
      INTO   seg_modulo_bat.ruta_listados
      FROM   seg_modulo b
      WHERE  b.modulo_cod = 'bat'

      -- se abre la ventana que envia el proceso de preliquidacion
      OPEN WINDOW w_folio_genera WITH FORM "UNIL18"
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
           FROM glo_ctr_archivo a, glo_folio g,
                uni_inf_unificador c
          WHERE a.proceso_cod = g_proceso_cod
            AND a.proceso_cod = g.proceso_cod
            AND g.folio = c.folio_unificacion
            AND g.status = 2
            AND a.folio = c.folio_unificacion
            AND c.diagnostico = 5

         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(
                 v_r_glo_ctr_archivo.folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH

         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente liquidados","info")
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
            DISPLAY v_folio
               IF ( v_folio IS NULL OR v_folio = -1 ) THEN
                  CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
                  CONTINUE INPUT
               END IF
               
               CALL fn_muestra_resumen_INFONAVIT(v_folio)
               -- Ocultar nuevamente las tablas de resultados
               CALL f.setElementHidden("grupo_resumen",0)
               -- Limpia datos para nueva consulta
               
            ON ACTION Generar
            DISPLAY v_folio
               -- se invoca el prceso que crea el archivo de salida
               CALL fn_dpe_ejecuta_generacion_archivo_INFONAVIT(v_folio, p_usuario_cod)
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

{
======================================================================
Clave: 
Nombre: fn_dpe_ejecuta_generacion_archivo_INFONAVIT
Fecha creacion: 24/05/2012
Narrativa del proceso que realiza:
Ejecuta el proceso que genera el archivo de salida para dependecia de patrones
======================================================================
}

FUNCTION fn_dpe_ejecuta_generacion_archivo_INFONAVIT(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_fin_oper    SMALLINT,
       v_mensaje         STRING

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "ARCHIVOAQUI"

   -- se verifica si se puede continuar con la operacion
   IF ( fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
       
          CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"UNIL18",
                                 "",p_usuario_cod)
            RETURNING r_bnd_fin_oper
      IF (r_bnd_fin_oper = 0) THEN
      
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIS02 ",
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
          CALL fn_mensaje("Atención","Se ha enviado la generación de archivo.\n"||
               "Puede revisar el avance del proceso en el monitor de "||
               "ejecución de procesos","information")
      ELSE
         CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje

         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF         
   END IF

END FUNCTION