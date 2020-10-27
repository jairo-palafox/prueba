--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL220                                                                #
#Objetivo     => Programa lanzador de inicio del proceso de retiro por webservice       #
#                que ejecuta la preliquidacion de las solicitudes de este retiro        #
#Fecha inicio => Julio 02, 2013                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
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
       p_tipo_ejecucion      SMALLINT, -- forma como ejecutara el programa
       p_s_titulo            STRING, -- titulo de la ventana
       v_folio               LIKE glo_folio.folio,
       v_s_cadena            STRING, -- cadena de texto
       v_i_conArch           INTEGER,
       v_r_glo_ctr_archivo   RECORD
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
       v_num_solicitudes     INTEGER,
       v_resultado           SMALLINT -- booleana para verificar proceso
       

   -- se recuperan los parametros
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   LET g_usuario = p_usuario_cod
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_ret_webservice -- retiro por web serives
   LET g_opera_cod   = g_opera_cod_ret_ws_preliquidacion -- preliquidacion


   -- Valida operacion para verificar si se puede continuar
   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
                            RETURNING v_resultado
   IF ( v_resultado = 0 ) THEN
         
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
      OPEN WINDOW w_folio_preliquida WITH FORM "RETL2201"
     
      -- se obtienen las descripciones del proceso y la operacion
      CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc
      CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod) RETURNING v_opera_desc
      
      -- se despliegan las descripciones
      DISPLAY v_proceso_desc, v_opera_desc
      TO proceso_desc, opera_desc
      
      -- se captura el folio
      MENU "Retiro Web Service"
      
         BEFORE MENU
            -- se cuentan las solicitudes pendientes de integracion
            SELECT COUNT(*)
            INTO   v_num_solicitudes
            FROM   ret_his_saldo_sv97
            WHERE  estatus_ssv IN ("0014", "0017", "0019", "0021") -- confirmadas por banco para ser pagadas

            -- se muestran las solicitudes encontradas
            DISPLAY BY NAME v_num_solicitudes
            
         COMMAND "Aceptar"
            -- Si no se tiene monto, no se puede hacer la preliquidacion
            IF ( v_num_solicitudes IS NULL OR v_num_solicitudes < 1 ) THEN
               CALL fn_mensaje("Atención","No existen solicitudes pendientes de preliquidación","stop")
               CONTINUE MENU
            END IF  
                  
            -- se invoca la ejecucion del stored procedure
            CALL fn_preliquida_ret_ws(p_usuario_cod)
            EXIT MENU
           
         COMMAND "Cancelar"
            EXIT MENU
      
      END MENU
   ELSE
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_s_cadena

      CALL fn_mensaje("Atención", v_s_cadena, "stop")
      MENU
         COMMAND "Cerrar"
            EXIT MENU
      END MENU
   END IF
   
   CLOSE WINDOW w_folio_preliquida
   
END MAIN


{
======================================================================
Clave: 
Nombre: fn_preliquida_ret_ws
Fecha creacion: Julio 02, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de retiros por webservices de las solicitudes
encontradas al momento

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_preliquida_ret_ws(p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_i_resultado     INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT  --codigo de error fn_actualiza_opera_ini

   -- este proceso no tiene archivo
   LET v_nombre_archivo = "NA"
   
   -- se obtiene el ID del proceso
   CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING g_pid
   
   -- Inicializa proceso porque es la primera operacion
   CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"RETL220","",
                              p_usuario_cod) RETURNING v_i_resultado 
                              
   -- si se inicio el proceso correctamente
   IF ( v_i_resultado = 0) THEN
    	
    	-- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,
                                  g_proceso_cod,
                                  g_opera_cod,
                                  p_folio,
                                  "RETL220",
                                  v_nombre_archivo,
                                  p_usuario_cod
                                  )
         RETURNING r_resultado_opera
         
      -- si se pudo iniciar la operacion
    	IF ( r_resultado_opera  = 0 ) THEN
    	
    	   -- se invoca la preliquidacion
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP220 ",
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
         DISPLAY v_s_comando                        
         RUN v_s_comando
         CALL fn_mensaje("Atención",
                         "Se ha enviado la preliquidación.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
                         "information")
      ELSE 
    	   CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
   
    	   CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
   
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   
      MENU
         COMMAND "Cerrar"
            EXIT MENU
      END MENU
       
   END IF
       
 
END FUNCTION