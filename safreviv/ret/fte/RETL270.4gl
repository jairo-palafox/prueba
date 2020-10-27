--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL270                                                                 #
#Objetivo     => Programa que ejecuta el programa que genera el archivo de salida con   #
#                las solicitudes de retiro de fondo de ahorro para pago por DAP         #
#                para ser enviado a Tesoreria                                           #
#Fecha inicio => Noviembre 28, 2013                                                     #
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
       p_s_titulo            STRING,     -- titulo de la ventana
       v_folio               LIKE glo_folio.folio
       ,v_s_cadena           STRING      -- cadena de texto
       ,v_cbx_folios         ui.ComboBox -- combo de afores
       ,v_i_conArch          INTEGER
       ,arr_modalidad   DYNAMIC ARRAY OF RECORD       	
       	v_id_modalidad    SMALLINT,
       	v_desc_modalidad  VARCHAR(100),
       	v_num_regs        INTEGER
      END RECORD
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
       v_indice              SMALLINT

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
   LET g_proceso_cod = g_proceso_cod_ret_archivo_fico_DAP    -- creacion de archivo de pago por DAP
   LET g_opera_cod   = g_opera_cod_genera_archivo_dap -- generacion del archivo de salida para pago por DAP

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
    LET v_sql= "\n SELECT rs.modalidad_retiro,                    ",
               "\n        rm.des_larga       ,                    ",
               "\n        COUNT(*)                                ",
               "\n FROM ret_solicitud_generico rs,                ",
               "\n      ret_modalidad_retiro   rm,                ",
               "\n      ret_pago_dap           rp                 ",
               "\n WHERE rs.estado_solicitud = 60                 ",
               "\n AND   rm.modalidad_retiro = rs.modalidad_retiro",
               "\n AND   rm.modalidad_retiro = 2                  ",
               "\n AND   rs.id_solicitud     = rp.id_solicitud    ",
               "\n GROUP BY 1,2                                   "

   PREPARE stm_mod_retiro FROM v_sql
   DECLARE cur_mod_retiro CURSOR FOR stm_mod_retiro
  
   -- Inicializa índice
   LET v_indice = 1
  
   -- Se itera el resultado
   FOREACH cur_mod_retiro INTO arr_modalidad[v_indice].*
      --Incrementa el índice
      LET v_indice = v_indice + 1
   END FOREACH

   -- se borra el ultimo registro generado por el foreach
   CALL arr_modalidad.deleteElement(arr_modalidad.getLength())

   -- se abre la ventana que envia el proceso de envío de archivos
   OPEN WINDOW w_genera_archivo WITH FORM "RETL2701"
   
   -- se obtienen las descripciones del proceso y la operacion
   CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod) RETURNING v_opera_desc

   -- se despliegan las descripciones
   DISPLAY v_proceso_desc, v_opera_desc
   TO proceso_desc, opera_desc
   
   DISPLAY ARRAY arr_modalidad TO scr_modalidad.*
   
      ON ACTION ACCEPT
       
         -- Valida que existan registros
         IF ( arr_modalidad.getLength() > 0 ) THEN
         	
            -- se invoca la ejecucion de la función que genera el archivo
            CALL fn_genera_salida_retiro_generico(v_folio, p_usuario_cod)
            EXIT DISPLAY
            
         ELSE 
            -- Se envía mensaje de advertencia indicando que se ha comenzado con la generación del archivo
            CALL fn_mensaje("Atención", "No existen registros para la generación del archivo","stop")
         END IF
         
      ON ACTION CANCEL
         EXIT DISPLAY
   
   END DISPLAY
   
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
FUNCTION fn_genera_salida_retiro_generico(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_resultado       INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio

   -- este proceso inicia en automático, no tiene archivo
   LET v_nombre_archivo = "NA"
   
   -- el folio se generara en el programa lanzado
   LET p_folio = 0

   -- se verifica si se puede continuar con la operacion
   LET v_resultado = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   -- se valida que se pueda iniciar la operacion
   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING v_resultado

   -- si se pudo validar
   IF ( v_resultado = 0 ) THEN
      	
      -- se genera el pid 
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid
      
      -- Se inicializa el proceso
      CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 p_folio           ,
                                 "RETL270"          ,
                                 v_nombre_archivo  ,
                                 p_usuario_cod)  RETURNING v_resultado
      
      -- si se pudo iniciar la operacion
      IF ( v_resultado = 0 ) THEN
         
         -- inicia la operacion
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETL270","NA",p_usuario_cod)
         RETURNING v_resultado
         
                                       
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETS270 ",
                           p_usuario_cod CLIPPED, " ",
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