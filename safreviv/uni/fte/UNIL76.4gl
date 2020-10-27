--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 13/01/2016
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIL76                                                        #
#Objetivo     => Lanzador del archivo de salida Unificación Complementaria     #
#Fecha inicio => Enero 13, 2015                                                #
################################################################################

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
       DEFINE v_folio_liquida DECIMAL (9,0)

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
      LET g_proceso_cod = 2307
      LET g_opera_cod   = 5

      -- se obtiene el PID del proceso
      SELECT MAX(pid)
      INTO   g_pid
      FROM   bat_ctr_proceso
      WHERE  proceso_cod = g_proceso_cod
      
      -- se obtienen las rutas de control del modulo
      SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
      INTO   g_reg_modulo.*
      FROM   seg_modulo s
      WHERE  s.modulo_cod = 'uni'
      
      SELECT b.ruta_listados
      INTO   seg_modulo_bat.ruta_listados
      FROM   seg_modulo b
      WHERE  b.modulo_cod = 'bat'
      
      -- se abre la ventana que envia el proceso de preliquidacion
      OPEN WINDOW w_folio_genera WITH FORM "UNIL760"
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
         DECLARE cur_folios CURSOR FOR  SELECT a.folio, b.nombre_archivo 
                                        FROM   glo_folio a, 
                                               glo_ctr_archivo b,
                                               uni_det_complementario c
                                        WHERE  a.proceso_cod = g_proceso_cod
                                        AND    a.proceso_cod = b.proceso_cod
                                        AND    a.folio_referencia = b.folio
                                        AND    a.folio_referencia = c.folio_complentario                                    
                                        AND    a.opera_cod   = 3
                                        AND    a.status      = 2 
                                        AND    c.diagnostico = 4
                                        GROUP BY 1,2

         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_s_cadena)
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
         LET v_folio_liquida = -1
         INPUT v_folio_liquida WITHOUT DEFAULTS
            FROM cmb_folio
         ATTRIBUTES (UNBUFFERED)
         BEFORE INPUT
            ON ACTION ACCEPT
               IF ( v_folio_liquida IS NULL OR v_folio_liquida = -1 ) THEN
                  CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
                  CONTINUE INPUT
               END IF

               CALL fn_ejecuta_generacion_archivo(v_folio_liquida , p_usuario_cod)
                  EXIT INPUT

{               CALL fn_muestra_resumen_IMSS(v_folio_liquida)

               -- Ocultar nuevamente las tablas de resultados
               CALL f.setElementHidden("grupo_resumen",0)

            ON ACTION Generar
               -- se invoca el prceso que crea el archivo de salida
               CALL fn_ejecuta_generacion_archivo(v_folio_liquida , p_usuario_cod)
                  EXIT INPUT}

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
DEFINE p_folio              DECIMAL(9,0), 
       v_folio_unificacion  DECIMAL(9,0), 
       v_i_unificador       INTEGER,
       v_i_unificado        INTEGER,
       v_i_unificador1      INTEGER,
       v_i_unificado1       INTEGER,
       v_i_tot_registros    INTEGER, -- Contador de numero de registro de registros
       v_s_sql              STRING -- cadena con una instruccion SQL
       
   LET v_i_unificador = 0
   LET v_i_unificado = 0
   LET v_i_tot_registros = 0

   LET v_s_sql = "SELECT folio_referencia",
                 "\n FROM glo_folio",
                 "\n WHERE folio = ",p_folio

   PREPARE Prpr_Obt_folio FROM v_s_sql CLIPPED
   EXECUTE Prpr_Obt_folio INTO v_folio_unificacion


   SELECT COUNT (id_unificado)
   INTO   v_i_unificado
   FROM   uni_det_unificado 
   WHERE  folio_unificacion IN (SELECT folio_unificacion
                                FROM   uni_det_unificador
                                WHERE  folio_liquidacion = p_folio
                                AND    estado_familia    = 1
                                AND    diagnostico       = 5
                                )
   AND    diagnostico       = 5
   ;  
   SELECT COUNT(id_unificador)
   INTO   v_i_unificador
   FROM   uni_det_unificador
   WHERE  folio_liquidacion = p_folio
   AND    estado_familia    = 1
   AND    diagnostico       = 5
   ;


   
   --LET v_i_unificador = v_i_unificador + v_i_unificador1
   --LET v_i_unificado  = v_i_unificado + v_i_unificado1
   
   DISPLAY p_folio, v_i_unificador, v_i_unificado,v_i_unificador
        TO cmb_folio, v_i_unificador, v_i_unificado,v_i_tot_registros

  END FUNCTION

#OBJETIVO: Ejecutar el archivo de salida
FUNCTION fn_ejecuta_generacion_archivo(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_fin_oper    SMALLINT,
       r_bdn_valida_op   SMALLINT,
       v_mensaje         STRING

   SELECT nom_archivo
   INTO   v_nombre_archivo  
   FROM   bat_ctr_operacion
   WHERE  pid = g_pid
   AND    proceso_cod = g_proceso_cod 
   AND    opera_cod = 1

   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bdn_valida_op
   
   IF r_bdn_valida_op = 0 THEN 
   	  CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"UNIL76 ","",p_usuario_cod)
           RETURNING r_bnd_fin_oper
           
      IF (r_bnd_fin_oper = 0) THEN
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/UNIS15 ",
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
   ELSE 
      CALL fn_recupera_inconsis_opera(r_bdn_valida_op) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")    
   END IF
END FUNCTION