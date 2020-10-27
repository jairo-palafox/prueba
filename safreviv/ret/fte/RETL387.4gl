################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL387                                                      #
#Objetivo      => Programa lanzador para realizar la preliquidación de los     #
#                 montos obtenidos de la tabla ret_parcial_generico y          #
#                 ret_ctr_parcial                                              #
#Fecha inicio  => Agosto 2015.                                                 #
#Requerimiento => 867                                                          #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid             LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo      RECORD
          ruta_exp         CHAR(40),
          ruta_rescate     CHAR(40),
          ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat    RECORD
           ruta_listados    CHAR(40)
       END RECORD

END GLOBALS

MAIN
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion         SMALLINT, -- forma como ejecutara el programa
       p_s_titulo               STRING   -- titulo de la ventana
   DEFINE v_count_ret           SMALLINT
   DEFINE v_estado_solicitud    SMALLINT  

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
   LET g_proceso_cod = 1563
   LET g_opera_cod   = 1

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'ret'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
  
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "RETL3871"

   MENU    
      BEFORE MENU 
      
         SELECT COUNT(*)
         INTO  v_count_ret
		 FROM  ret_ctr_parcial c, ret_parcial_generico p
		 WHERE c.id_solicitud = p.id_solicitud     
		 AND   c.modalidad_retiro     = 11          
		 AND   c.estado_solicitud     = 15          
		 AND   c.cod_rechazo          = 0           
		 AND   MONTH(f_estimada_pago) = MONTH(TODAY)
         AND   YEAR(f_estimada_pago)  = YEAR(TODAY)
         
         -- se verifica si hay solicitudes para preliquidar
         IF ( v_count_ret = 1 ) THEN
            DISPLAY "Se va a generar la preliquidacion de "|| v_count_ret ||" registro. Seleccione aceptar para continuar ." TO lb_mensage
         ELSE
            IF ( v_count_ret > 1 ) THEN
               DISPLAY "Se va a generar la preliquidacion de "|| v_count_ret ||" registros. Seleccione aceptar para continuar ." TO lb_mensage
            ELSE 
               DISPLAY "No se va a generar la preliquidacion; se encontraron "|| v_count_ret ||" registros. Seleccione cancelar para salir ." TO lb_mensage
            END IF 
         END IF 

      ON ACTION ACCEPT     
         -- si se encontraron solicitudes que preliquidar
         IF ( v_count_ret > 0 ) THEN
           CALL fn_ret_ejecuta_preliquidacion(p_usuario_cod)
         ELSE 
           CALL fn_mensaje("Atención","No existen solicitudes para \nrealizar preliquidación","information")           
         END IF 
         EXIT MENU 
        
      ON ACTION CANCEL
         EXIT MENU 
   
   END MENU 
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

#Realiza el lanzado del programa que ejecutara la preliquidacion
FUNCTION fn_ret_ejecuta_preliquidacion(p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_resultado       SMALLINT,
       v_mensaje         STRING
       
   -- este proceso inicia por webservices, no tiene archivo
   LET v_nombre_archivo = "NA"
   
   -- el folio se generara en el programa lanzado
   LET v_folio = 0

   -- se verifica si se puede continuar con la operacion
   LET v_resultado = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   IF ( v_resultado = 0 ) THEN

      -- se genera el pid 
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid

      CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 v_folio           ,
                                 "RETL387"         ,
                                 v_nombre_archivo  ,
                                 p_usuario_cod)  RETURNING v_resultado
                                  
      -- el proceso se registro correctamente
      IF ( v_resultado = 0 ) THEN

         -- inicia la operacion
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETL387","NA",p_usuario_cod)
         RETURNING v_resultado
      
         -- se invoca la ejecucion del programa lanzado
         LET v_s_comando = "nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP387 ",
                            p_usuario_cod CLIPPED, " ",
                            g_pid                , " " ,
                            g_proceso_cod        , " " ,
                            g_opera_cod          , " ",
                            v_folio              , " '",
                            v_nombre_archivo CLIPPED ,"' ",
                            " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                            "/nohup:",g_pid  USING "&&&&&",":",
                            g_proceso_cod    USING "&&&&&",":",
                            g_opera_cod      USING "&&&&&" ,
                            " 2>&1 &"
                          
         DISPLAY v_s_comando
         RUN v_s_comando
         CALL fn_mensaje("Atención","Se ha enviado la preliquidación.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
      ELSE
         CALL fn_mensaje("Atención","No se pudo iniciar el proceso","information")
      END IF
   ELSE
      -- no se puede ejecutar la operacion
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   END IF
 
END FUNCTION