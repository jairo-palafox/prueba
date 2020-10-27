--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL464                                                                #
#Objetivo     => Lanzador del programa que detecta las inactividades de las             #
#                solicitudes enviadas por tableta                                       #
#Fecha inicio => Marzo 05, 2018                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
            
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario      LIKE seg_usuario.usuario_cod, -- clave del usuario
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
       
END GLOBALS
#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana                       ui.Window
PRIVATE DEFINE forma                         ui.Form
DEFINE p_nombre_menu        CHAR(50)

MAIN
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion      SMALLINT,   -- forma como ejecutara el programa
       p_s_titulo            STRING,     -- titulo de la ventana
       v_folio               LIKE glo_folio.folio,
       arr_modalidad     DYNAMIC ARRAY OF RECORD -- arreglo de despliegue
       	v_id_modalidad      SMALLINT,
       	v_desc_modalidad    VARCHAR(100),
       	v_num_regs          INTEGER
      END RECORD

       ,v_reg_modalidad   RECORD -- registro de modalidad
       	v_id_modalidad      SMALLINT,
       	v_desc_modalidad    VARCHAR(100),
       	v_num_regs          INTEGER
      END RECORD,
       v_proceso_desc        STRING, -- descripcion del proceso
       v_opera_desc          STRING, -- descripcion de la operacion
       v_sql                 STRING,
       v_indice              SMALLINT,
       v_fecha_pago          DATE

   DEFINE v_indicador        CHAR(1),
          v_actividad        CHAR(20)

   DISPLAY "Inicia Pantalla de Inactividad"
   LET p_usuario_cod        = ARG_VAL(1)
   LET g_proceso_cod        = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   
   LET g_usuario = p_usuario_cod;
   LET v_indicador = ""
   LET v_actividad = ""
   LET v_fecha_pago = TODAY

   IF ( p_nombre_menu IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_nombre_menu)
   END IF

   DISPLAY "Asignó los parámetros a las variables de trabajo"
   
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'ret'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   SELECT UPPER(indicador)
   INTO   v_indicador
   FROM   ret_indicador_actividad
   DISPLAY "Se hicieron las consultas generales, El indicador es:",v_indicador
   --CLOSE WINDOW SCREEN
   -- se abre la ventana que envia el proceso de envío de archivos
   DISPLAY "Se abrirá la pantalla"
   OPEN WINDOW w_act_inact WITH FORM "RETL4641"

   --DIALOG ATTRIBUTE (UNBUFFERED)
   DISPLAY "Aqui debió abrir la pantalla"
   DIALOG ATTRIBUTE (UNBUFFERED) 
       INPUT BY NAME v_fecha_pago
       END INPUT 
       BEFORE DIALOG 
          LET v_fecha_pago = TODAY 
          LET ventana = ui.Window.getCurrent()
          LET forma   = ventana.getForm()
          DISPLAY "En el before Input"
          DISPLAY BY NAME v_fecha_pago 

          IF v_indicador = "S" THEN 
             CALL forma.setElementHidden("lbl_activado", 1) -- oculta etiqueta
             CALL forma.setElementHidden("lbl_desactivado", 0) -- se muestra la etiqueta
             DISPLAY "Desactivará el proceso"
          ELSE
             CALL forma.setElementHidden("lbl_activado", 0) -- se muestra la etiqueta
             CALL forma.setElementHidden("lbl_desactivado", 1) -- oculta etiqueta
             DISPLAY "Activará el proceso"
          END IF 

       ON ACTION ACCEPT
          -- se realiza la elección 
          CALL fn_activa_desactiva(v_indicador)
          CALL fn_mensaje(v_actividad, "Se ha realizado su elección.", "bn_about")
          EXIT DIALOG 
       ON ACTION CANCEL
          EXIT DIALOG  

   END DIALOG  
   
   CLOSE WINDOW w_act_inact
   
END MAIN

{
======================================================================
Clave: 
Nombre: fn_activa_desactiva
Fecha creacion: Marzo 05, 2018
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Actualiza la tabla ret_indicador_actividad para ejecutar o detener el proceso

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_activa_desactiva(p_indicador)
DEFINE p_indicador       CHAR(1), -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_resultado       INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio,
       v_fecha_pago      DATE


       IF p_indicador = "S" THEN --- se desactivará
          CALL fn_mensaje("Desactivación", "El proceso será detenido.", "bn_about")
          UPDATE ret_indicador_actividad
          SET    indicador = "N"
          WHERE  1 = 1
          DISPLAY "Proceso desactivado", CURRENT YEAR TO MINUTE 
       ELSE     --- Se activará
          CALL fn_mensaje("Activación", "El proceso será activado.", "bn_about")
          UPDATE ret_indicador_actividad
          SET    indicador = "S"
          WHERE  1 = 1
          DISPLAY "Proceso Activado", CURRENT YEAR TO MINUTE 
          LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETC464.42r "

          LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETC464.42r ",
                           " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                           "/nohup:inactividad:",TODAY USING "YYYYMMDD","-",CURRENT HOUR TO SECOND ,
                           " 2>&1 &"
          DISPLAY v_s_comando                        
          RUN v_s_comando
      END IF
END FUNCTION
