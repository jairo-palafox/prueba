--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL467                                                                #
#Objetivo     => Programa que ejecuta el programa que genera el archivo de salida con   #
#                las notificaciones de pago de los grupos 2, 3 y 4                      #
#Fecha inicio => Abril 8, 2018                                                          #
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

MAIN
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion      SMALLINT,   -- forma como ejecutara el programa
       p_s_titulo            STRING   -- titulo de la ventana
       ,v_s_cadena           STRING     -- cadena de texto
       ,v_i_conArch          INTEGER
       ,v_r_glo_ctr_archivo  RECORD
          proceso_cod          LIKE glo_ctr_archivo.proceso_cod
          ,opera_cod           LIKE glo_ctr_archivo.opera_cod
          ,nombre_archivo      LIKE glo_ctr_archivo.nombre_archivo
          ,folio               LIKE glo_folio.folio
          ,estado              LIKE glo_ctr_archivo.estado
          ,f_actualiza         LIKE glo_ctr_archivo.f_actualiza
          ,usuario             LIKE glo_ctr_archivo.usuario
       END RECORD,
       v_proceso_desc        STRING, -- descripcion del proceso
       v_opera_desc          STRING, -- descripcion de la operacion
       v_sql                 STRING,
       v_indice              SMALLINT,
       v_envios              INTEGER,
       v_contador_sol        INTEGER,
       v_reenvios            INTEGER  

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   LET g_usuario = p_usuario_cod
   
   -- se asigna proceso y operacion
   LET g_proceso_cod  = g_proceso_cod_ret_notifica_grupo    -- Notificación de pago de grupos 2, 3 y 4
   LET g_opera_cod    = g_opera_ret_notifica_grupo_genera -- generacion del archivo de Notificación de pago de Grupos 2, 3 y 4
   LET v_envios       = 0
   LET v_reenvios     = 0
   LET v_contador_sol = 0

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'
    
    -- se obtiene el numero de solicitudes que hay para generar el archivo DAP

    SELECT COUNT(*)
    INTO   v_contador_sol
    FROM   ret_ley73_generico rs
    WHERE  rs.estado_solicitud IN (71, 72) 
    AND    rs.gpo_ley73 IN (2,3,4)    
    AND    rs.id_solicitud NOT IN (
           SELECT DISTINCT id_solicitud_retiro
           FROM   ret_notifica_gpo)

   LET v_envios       = v_contador_sol
   LET v_contador_sol = 0

    SELECT COUNT(*)
    INTO   v_contador_sol
    FROM   ret_solo_infonavit rs
    WHERE rs.estado_solicitud IN (60)          
    AND   rs.id_solicitud NOT IN (
           SELECT DISTINCT id_solicitud_retiro
           FROM   ret_notifica_gpo)

   LET v_envios       = v_envios + v_contador_sol
   LET v_contador_sol = 0

    SELECT COUNT(*)
    INTO   v_contador_sol
    FROM   ret_excep_devol_ssv rs
    WHERE rs.estado_solicitud IN (71, 72)          
    AND   rs.id_solicitud NOT IN (
           SELECT DISTINCT id_solicitud_retiro
           FROM   ret_notifica_gpo)

   LET v_envios       = v_envios + v_contador_sol
   LET v_contador_sol = 0

    SELECT COUNT(*)
    INTO   v_reenvios
    FROM   ret_notifica_gpo_reenvio

   -- se abre la ventana que envia el proceso de envío de archivos
   OPEN WINDOW w_genera_archivo WITH FORM "RETL4671"
   
   -- se obtienen las descripciones del proceso y la operacion
   CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod) RETURNING v_opera_desc
   
   -- se despliegan las descripciones
   DISPLAY v_proceso_desc, v_opera_desc, v_envios, v_reenvios
   TO proceso_desc, opera_desc, ed_envios, ed_reenvios
      MENU  
  
         ON ACTION ACCEPT
            
            IF( v_envios + v_reenvios > 0 )THEN
              CALL fn_genera_salida_retiro_generico(p_usuario_cod)
            ELSE 
              CALL fn_mensaje("Atención","No existen registros para envío ","information")  
            END IF 
            EXIT MENU 
        
         ON ACTION CANCEL
            EXIT MENU 

      END MENU
   
   CLOSE WINDOW w_genera_archivo
   
END MAIN


{
======================================================================
Clave: 
Nombre: fn_genera_salida_retiro_generico
Fecha creacion: Noviembre 28, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el archivo de salida a Tesorería de retiros pagados por DAP

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_salida_retiro_generico(p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_resultado       INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio

   -- este proceso inicia en automático, no tiene archivo
   LET v_nombre_archivo = "PRTFT.DP.I04002.S", TODAY USING "YY", TODAY USING "MM", TODAY USING "DD", ".DEVIVG234.GDG"
   
   -- el folio se generara en el programa lanzado
   LET p_folio = 0

   -- se verifica si se puede continuar con la operacion
   LET v_resultado = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   -- se valida que se pueda iniciar la operacion
   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING v_resultado

   -- si se pudo validar
   IF ( v_resultado = 0 ) THEN
      	
      -- se genera el pid 
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, g_usuario) RETURNING g_pid
      
      -- Se inicializa el proceso
      CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 p_folio           ,
                                 "RETL467"          ,
                                 v_nombre_archivo  ,
                                 g_usuario)  RETURNING v_resultado
      
      -- si se pudo iniciar la operacion
      IF ( v_resultado = 0 ) THEN
         
         -- inicia la operacion
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETL467","NA",g_usuario)
         RETURNING v_resultado
         
         DISPLAY "Parámetros enviados :"
         DISPLAY "Usuario        : ", g_usuario
         DISPLAY "Pid            : ", g_pid
         DISPLAY "Proceso cod    : ", g_proceso_cod
         DISPLAY "Opera cod      : ", g_opera_cod
         DISPLAY "Folio          : ", p_folio
         DISPLAY "Nombre archivo : ", v_nombre_archivo                              
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETS467 ",
                           g_usuario CLIPPED, " ",
                           g_pid  , " " ,
                           g_proceso_cod , " " ,
                           g_opera_cod ," ",
                           p_folio ," ",
                           v_nombre_archivo CLIPPED," ",
                           " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                           "/nohup:",g_pid USING "&&&&&",":",
                           g_proceso_cod USING "&&&&&",":",
                           g_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"
         
         -- DISPLAY v_s_comando                        
         RUN v_s_comando
         
         --Se envía mensaje de advertencia indicando que se ha comenzado con la generación del archivo
         CALL fn_mensaje("Atención", "Se ha enviado la generación del archivo de salida.\nPodrá revisar el resultado en el monitor de ejecución de procesos", "information")
      ELSE
         CALL fn_mensaje("Atención","No se puede iniciar la operación.","stop")
      END IF
   ELSE
      -- se muestra en pantalla por que no se puede enviar el proceso
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
      
      MENU
         COMMAND "Cerrar"
           EXIT MENU
      END MENU
      
   END IF

END FUNCTION