################################################################################
#Modulo       => LAV                                                           #
#Programa     => LAVL03                                                        #
#Objetivo     => Lanzador de detección de montos para operaciones relevantes   #
#Fecha inicio => 29/12/2014                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario      CHAR(20)
DEFINE g_reg_modulo   RECORD
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
END RECORD
DEFINE seg_modulo_bat RECORD
        ruta_listados    CHAR(40)   
END RECORD

DEFINE f_ventana     ui.window
DEFINE f_forma       ui.form

END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT,    -- forma como ejecutara el programa
       p_titulo         STRING,      -- titulo de la ventana
       v_cbx_folios     ui.ComboBox, -- combo de afores
       v_tot_folios     INTEGER,
       v_folio          DECIMAL(9,0)
DEFINE arr_folios RECORD 
       v_folio DECIMAL(9,0),
       v_g_folio DECIMAL(9,0)
END RECORD
   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   LET g_usuario = p_usuario_cod
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2702
   LET g_opera_cod   = 2
   
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
   WHERE  s.modulo_cod = 'lav'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_genera WITH FORM "LAVL041"
      -- Recupera punteros a ventana para control de grupos
      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()
      -- se le asigna el apuntado der combo a la variable
      LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")

      -- se inicia el combobox en blanco
      CALL v_cbx_folios.clear()
      -- se llena el arreglo de folios
      DECLARE cur_folios CURSOR FOR SELECT a.folio, b.folio
                                    FROM   lav_det_lavado a, 
                                           glo_folio b
                                    WHERE  a.folio = b.folio 
                                    AND    b.status = 2
                                    AND    a.estado = 1
                                    GROUP BY 1,2
                                    ORDER BY 1,2
      LET v_tot_folios = 0

      FOREACH cur_folios INTO arr_folios.v_folio,
                              arr_folios.v_g_folio
         CALL v_cbx_folios.addItem(arr_folios.v_folio,
                                   arr_folios.v_g_folio)
         -- Contador de archivos eoncontrados
         LET v_tot_folios = v_tot_folios + 1
      END FOREACH
            
      IF ( v_tot_folios < 1 )THEN
         CALL fn_mensaje("Atención",
                         "No hay registros pendientes de notificar",
                         "info")
         CLOSE WINDOW w_folio_genera
         RETURN
      END IF
      -- Se libera el cursor
      FREE cur_folios
      CALL v_cbx_folios.addItem(-1," ")

      LET v_folio = -1
      
      INPUT v_folio WITHOUT DEFAULTS
      FROM  cmb_folio
      ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT
         ON ACTION ACCEPT
            IF ( v_folio IS NULL OR v_folio = -1 ) THEN
               CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
               CONTINUE INPUT
            END IF

            -- se invoca el prceso que crea el archivo de salida
            CALL fn_lav_ejecuta_archivo(v_folio, g_usuario)
               EXIT INPUT

         ON ACTION CANCEL
            LET INT_FLAG = TRUE
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_folio_genera   
END MAIN

#OBJETIVO: Ejecutar el lanzado para la ejecución del archivo de notificaciones
FUNCTION fn_lav_ejecuta_archivo(p_folio, p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       v_s_comando      STRING,
       v_mensaje        STRING,
       r_bnd_opera_ini  SMALLINT,
       p_folio          DECIMAL(9,0)

   CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"LAVL03","",p_usuario_cod)
        RETURNING r_bnd_opera_ini

   IF (r_bnd_opera_ini = 0) THEN
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/LAVS01 ",
                        p_usuario_cod CLIPPED, " ",
                        g_pid  , " " ,
                        g_proceso_cod , " " ,
                        g_opera_cod ," ",
                        p_folio, " ",
                        "NA ",
                        " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                        "/nohup:",g_pid        USING "&&&&&",":",
                        g_proceso_cod USING "&&&&&",":",
                        g_opera_cod   USING "&&&&&" ,
                        " 2>&1 &"

      DISPLAY v_s_comando
      RUN v_s_comando

      CALL fn_mensaje ("Atención",
                       "Se ha iniciado la ejecución del archivo de notificaciones \n Puede revisar el avance en el monitor de procesos",
                       "stop")
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_opera_ini) 
           RETURNING v_mensaje
   END IF
END FUNCTION
