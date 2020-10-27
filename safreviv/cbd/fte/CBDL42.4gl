################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => CBD                                                           #
#Programa     => CBDL42                                                        #
#Objetivo     => LANZADOR PARA GENERAR EL ARCHIVO DE FALLECIDOS                #
# Autor        => Antonio Gómez                                                #
#Fecha inicio => 05/Sep/2017                                                   #
################################################################################

DATABASE safre_viv

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
DEFINE p_usuario        LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_titulo         STRING

   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo   IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo  )
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2121 --Ajuste Amortización Excedente
   LET g_opera_cod   = 1    --Generar archivo salida
      
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'cbd'
      
   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE   b.modulo_cod = 'bat'
      
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_genera WITH FORM "CBDL421"
      MENU 
         ON ACTION Aceptar
            CALL fn_generar_archivo(p_usuario)
            EXIT MENU

         ON ACTION CANCEL
            EXIT MENU
      END MENU
   CLOSE WINDOW w_folio_genera   
END MAIN


#OBJETIVO: Ejecutar la generación del archivo de salida de detalles.
FUNCTION fn_generar_archivo(p_usuario)
DEFINE p_usuario         LIKE seg_usuario.usuario_cod, 
       v_folio           LIKE glo_folio.folio,
       v_s_comando       STRING,
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       r_bnd_opera_ini    SMALLINT,
       v_mensaje          STRING,
       v_inicia_proceso   SMALLINT,
       v_valida_operacion SMALLINT
   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"
   LET v_folio = 0

   -- se obtiene el ID del proceso
   CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario)
   RETURNING g_pid
   
   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING v_valida_operacion
   
   IF (v_valida_operacion = 0) THEN
      --Inicia proceso 
      CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"CBDS16","",p_usuario)
      RETURNING v_inicia_proceso
   
      IF ( v_inicia_proceso = 0)THEN

         -- Inicio operacion.
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"CBDS16","",p_usuario_cod)
         RETURNING r_bnd_opera_ini

         IF (r_bnd_opera_ini = 0) THEN 
   
            LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/CBDS16 ",
                                      p_usuario CLIPPED, " ",
                                      g_pid  , " " ,
                                      g_proceso_cod , " " ,
                                      g_opera_cod ," ",
                                      v_folio ," ",
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
            CALL fn_recupera_inconsis_opera(r_bnd_opera_ini) RETURNING v_mensaje
            --CALL fn_mensaje ("Atención", v_mensaje, "stop")
            UPDATE bat_ctr_proceso   SET estado_cod = 4, fecha_fin = CURRENT YEAR TO SECOND WHERE pid = p_pid;
            UPDATE bat_ctr_operacion SET estado_cod = 4, fecha_fin = CURRENT YEAR TO SECOND WHERE pid = p_pid;
            DISPLAY v_mensaje
         END IF
      ELSE
         CALL fn_recupera_inconsis_opera(v_inicia_proceso) RETURNING v_mensaje
         --CALL fn_mensaje ("Atención", v_mensaje, "stop")--
         DISPLAY v_mensaje
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(v_valida_operacion) RETURNING v_mensaje
      --CALL fn_mensaje ("Atención", v_mensaje, "stop")
      DISPLAY v_mensaje
   END IF
END FUNCTION