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
        ruta_exp         CHAR(40),
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
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_titulo       STRING -- titulo de la ventana

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
   LET g_opera_cod   = 1
   
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

   CALL fn_selecciona_periodos (p_usuario_cod)

END MAIN

FUNCTION fn_selecciona_periodos (p_usuario_cod)
DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod,
       v_periodo     CHAR,
       v_fecha_ini   DATE,
       v_fecha_fin   DATE,
       v_year        CHAR(4),
       bnd           SMALLINT

   OPEN WINDOW w_sel_periodo WITH FORM "LAVL031"
   DIALOG ATTRIBUTE(UNBUFFERED)
  
   INPUT v_periodo
   FROM periodos

   BEFORE INPUT
      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()

      CALL f_forma.setElementHidden("gr_fechas", 1)

      DISPLAY TODAY TO ed_fecha
   END INPUT

   ON ACTION ACCEPT 
      LET v_year = YEAR (TODAY)

      CASE v_periodo
      WHEN 1
         LET v_fecha_ini = "01/01/" || v_year
         LET v_fecha_fin = "03/31/" || v_year
      WHEN 2
         LET v_fecha_ini = "04/01/" || v_year
         LET v_fecha_fin = "06/30/" || v_year
      WHEN 3
         LET v_fecha_ini = "07/01/" || v_year
         LET v_fecha_fin = "09/30/" || v_year
      WHEN 4
         LET v_fecha_ini = "10/01/" || v_year
         LET v_fecha_fin = "12/31/" || v_year
      WHEN 0
         CALL f_forma.setElementHidden("gr_fechas", 0)
         CALL fn_lav_captura_fechas()
              RETURNING v_fecha_ini, v_fecha_fin, bnd
         EXIT CASE
      END CASE

      IF v_fecha_ini AND v_fecha_fin > TODAY THEN
         CALL fn_mensaje ("Atención", 
                          "El periodo solicitado es posterior a la fecha actual", 
                          "stop")
      ELSE
         IF v_periodo = 0 AND bnd = 1 THEN 
            EXIT DIALOG
         ELSE 
            CALL fn_lav_ejecuta_deteccion ( v_fecha_ini, v_fecha_fin, p_usuario_cod)
            EXIT DIALOG         
         END IF
      END IF

   ON ACTION cancelar 
      EXIT DIALOG

   END DIALOG
   CLOSE WINDOW w_sel_periodo 

END FUNCTION 

#OBJETIVO: Capturar las fechas de inicio y fecha fin 
FUNCTION fn_lav_captura_fechas()
DEFINE v_fecha_ini DATE,
       v_fecha_fin DATE,
       bnd         SMALLINT

         INPUT v_fecha_ini, v_fecha_fin
         WITHOUT DEFAULTS 
         FROM fecha_ini, fecha_fin
         ATTRIBUTES (UNBUFFERED,
                     ACCEPT = FALSE,
                     CANCEL = FALSE)

         BEFORE INPUT 
            LET v_fecha_ini = TODAY 
            LET v_fecha_fin = TODAY
            
         ON ACTION ACCEPT
               IF v_fecha_fin < v_fecha_ini THEN
                  CALL fn_mensaje ("Atención", 
                                   "La fecha fin es anterior a la fecha inicio",
                                   "stop")
               ELSE
                  EXIT INPUT
               END IF
            ON ACTION cancelar
               LET bnd = 1
               CALL f_forma.setElementHidden("gr_fechas", 1)
               EXIT INPUT
         END INPUT        

RETURN v_fecha_ini, v_fecha_fin, bnd
END FUNCTION

#OBJETIVO: Ejecutar el lanzado para la detección de movimientos relevantes.
FUNCTION fn_lav_ejecuta_deteccion( p_fecha_ini, p_fecha_fin, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       v_s_comando       STRING,
       v_mensaje         STRING,
       r_bnd_opera_ini   SMALLINT,
       r_bnd_val_opera   SMALLINT,
       r_bnd_ini_proceso SMALLINT,
       p_fecha_ini       DATE, 
       p_fecha_fin       DATE

   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
        RETURNING r_bnd_val_opera
   IF r_bnd_val_opera = 0 THEN
      -- se obtiene el ID del proceso
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)
           RETURNING g_pid        
      --Inicia proceso
      CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"LAVL03","",p_usuario_cod)
           RETURNING r_bnd_ini_proceso
      
      IF (r_bnd_ini_proceso = 0) THEN 
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"LAVL03","",p_usuario_cod)
              RETURNING r_bnd_opera_ini
      
         IF (r_bnd_opera_ini = 0) THEN
            LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/LAVP02 ",
                              p_usuario_cod CLIPPED, " ",
                              g_pid  , " " ,
                              g_proceso_cod , " " ,
                              g_opera_cod ," ",
                              0, " ",
                              "NA ",
                              "'", p_fecha_ini, "' ", 
                              "'", p_fecha_fin, "' ",
                              " 1>",seg_modulo_bat.ruta_listados clipped ,
                              "/nohup:",g_pid        USING "&&&&&",":",
                              g_proceso_cod USING "&&&&&",":",
                              g_opera_cod   USING "&&&&&" ,
                              " 2>&1 &"
      
            DISPLAY v_s_comando
            RUN v_s_comando
      
            CALL fn_mensaje ("Atención", 
                             "Se ha iniciado la detección de movimientos relevantes \n del "||p_fecha_ini|| " al " || p_fecha_fin ||"."||
                             "\n Puede revisar el avance en el monitor de procesos",                          
                             "stop")
         ELSE
            CALL fn_mensaje ("Atención", fn_recupera_inconsis_opera(r_bnd_opera_ini), "stop");
         END IF
      ELSE
         CALL fn_mensaje ("Atención", fn_recupera_inconsis_opera(r_bnd_ini_proceso), "stop");
      END IF
   ELSE
      CALL fn_mensaje ("Atención", fn_recupera_inconsis_opera(r_bnd_val_opera), "stop");
   END IF
      
END FUNCTION