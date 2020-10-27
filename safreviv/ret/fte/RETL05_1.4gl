 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL01                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para retiro fondo ahorro                                             #
#Fecha inicio => Febrero 17, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
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
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING   -- titulo de la ventana
       ,v_folio          LIKE ret_preliquida.folio_liquida
{       ,v_r_glo_ctr_archivo      RECORD
           proceso_cod    LIKE glo_ctr_archivo.proceso_cod
          ,opera_cod      LIKE glo_ctr_archivo.opera_cod
          ,nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
          ,folio          CHAR(10)
          ,estado         LIKE glo_ctr_archivo.estado
          ,f_actualiza    LIKE glo_ctr_archivo.f_actualiza
          ,usuario        LIKE glo_ctr_archivo.usuario
       END RECORD}
   DEFINE v_count_ret       SMALLINT
   DEFINE v_estado_solicitud SMALLINT  

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)   
   LET v_estado_solicitud = 15

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_ret_fondo_ahorro          --100 -- RETIRO FONDO DE AHORRO
   LET g_opera_cod   = g_opera_cod_ret_fondoAho_preliquidacion --1  -- preliquidacion

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
  
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "RETL050"

   MENU    
   BEFORE MENU 
     SELECT NVL(COUNT(*),0) 
           INTO v_count_ret
           FROM ret_fondo_ahorro
          WHERE estado_solicitud = v_estado_solicitud
          IF v_count_ret = 1 THEN  
             DISPLAY "Se va a generar la preliquidacion de "|| v_count_ret ||" registro. Seleccione aceptar para continuar ." TO lb_mensage
          ELSE
             IF v_count_ret > 1 THEN  
                DISPLAY "Se va a generar la preliquidacion de "|| v_count_ret ||" registros. Seleccione aceptar para continuar ." TO lb_mensage
             ELSE 
                DISPLAY "No se va a generar la preliquidacion se encontraron "|| v_count_ret ||" registros. Seleccione cancelar para Salir ." TO lb_mensage
             END IF 
          END IF 
      ON ACTION ACCEPT
         SELECT NVL(COUNT(*),0) 
           INTO v_count_ret
           FROM ret_fondo_ahorro
          WHERE estado_solicitud = v_estado_solicitud
      
         IF v_count_ret>0 THEN
           CALL fn_ret_ejecuta_preliquidacion(v_folio,p_usuario_cod)
         ELSE 
           CALL fn_mensaje("Atención","No existen solicitudes para \nrealizar preliquidación","information")           
         END IF 
         EXIT MENU 
        
      ON ACTION CANCEL
         EXIT MENU 
   
   END MENU 
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

{
======================================================================
Clave: 
Nombre: fn_ret_ejecuta_preliquidacion
Fecha creacion: Febrero 17, 2012
Autor: Erick Rodriguez, EFP
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de Retiro Fondo ahorro
para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_ejecuta_preliquidacion(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_bandera         SMALLINT, -- para revisar que los procesos ejecutaron bien
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
DEFINE v_program         VARCHAR(50) 

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"  
   LET v_program        = "RETL05" 
   LET v_bandera = 0

   -- se verifica si se puede continuar con la operacion
   IF ( fn_valida_operacion(0,g_proceso_cod,g_opera_cod) = 0 ) THEN
   -- fn_valida_operacion(pid, proceso_cod, opera_cod)
   -- Si resultado = 0 
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid
      CALL fn_genera_folio(g_proceso_cod, g_opera_cod,p_usuario_cod) RETURNING p_folio

      CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 p_folio           ,
                                 v_program         ,
                                 v_nombre_archivo  ,
                                 p_usuario_cod)  RETURNING v_bandera
                                  
      -- el proceso se registro correctamente
    IF ( v_bandera = 0 ) THEN
      -- se invoca la ejecucion del programa lanzado. los parametros se envian 
      -- en orden segun lo acordado el dia 12/Enero/2012 con equipo EFP
      -- usuario, pid, proceso_cod, opera_cod, folio y archivo
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP05 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"
                          
       DISPLAY v_s_comando
       RUN v_s_comando
       CALL fn_mensaje("Atención","Se ha enviado la preliquidación.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
     ELSE
        CALL fn_mensaje("Atención","No se pudo iniciar el proceso","information")
     END IF
END IF
 
END FUNCTION