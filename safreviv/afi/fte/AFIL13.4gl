#########################################################################
#Proyecto          => INFONAVIT (MEXICO)                                #
#Propietario       => E.F.P.                                            #
#Programa AFIL13   => Programa de consulta de afiliados con relacion    #
#                     laboral                                           #
#Sistema           => AFI                                               #
#Fecha             => 30 de junio de 2012                               #
#########################################################################
DATABASE safre_viv
MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_proceso_cod    LIKE cat_proceso.proceso_cod,
       p_opera_cod      LIKE cat_operacion.opera_cod,
       v_s_cadena       STRING, -- cadena de texto
       v_proceso_desc   STRING, -- descripcion del proceso
       v_opera_desc     STRING, -- descripcion de la operacion
       v_mensaje        STRING, -- mensaje en pantalla
       v_resultado      SMALLINT -- booleana para verificar proceso
       
   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod   = ARG_VAL(1) -- Recibe la variable de usuario
   LET p_proceso_cod   = ARG_VAL(2) -- Recibe el tipo de proceso
   LET v_s_cadena      = ARG_VAL(3) -- Recibe el nombre del programa

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( v_s_cadena IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_s_cadena)
   END IF

   LET v_mensaje  = "Este es un proceso de consulta de todos los derechohabientes ",
                    "que tienen una relación laboral activa al momento en que se ",
                    "ejecuta la consulta.\n\nDebido a que el proceso debe verificar cada ",
                    "cuenta de los derechohabientes registrados, este proceso puede tomar ",
                    "horas en concluir y no se puede ejecutar al mismo tiempo que el ",
                    "proceso de Registro de Pagos o de Precalificación."

   -- se asigna proceso y operacion
   LET p_proceso_cod = 1809 -- Consulta de relacion laboral
   LET p_opera_cod   = 1 -- consulta
                    
   OPEN WINDOW v_afic05 WITH FORM "AFIL131"

   -- se obtienen las descripciones del proceso y la operacion
   CALL fn_proceso_cod_desc(p_proceso_cod) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(p_proceso_cod, p_opera_cod) RETURNING v_opera_desc
      
   -- se despliegan las descripciones
   DISPLAY BY NAME v_proceso_desc, v_opera_desc, v_mensaje
  
   MENU "Consulta de relación laboral"

      COMMAND "Aceptar"

         -- Valida operacion para verificar si se puede continuar
         CALL fn_valida_operacion(0,p_proceso_cod,p_opera_cod)
                                  RETURNING v_resultado

         -- si se puede ejecutar la consulta
         IF ( v_resultado = 0 ) THEN
         
            -- se invoca la consulta
            CALL fn_consulta_afi_relacion_laboral(p_usuario_cod, p_proceso_cod, p_opera_cod)
            EXIT MENU

         ELSE
            CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_s_cadena
 
            CALL fn_mensaje("Atención", v_s_cadena, "stop")
            MENU
               COMMAND "Cerrar"
                  EXIT MENU
            END MENU
         END IF

      COMMAND "Cancelar"
         EXIT MENU

   END MENU
   
   CLOSE WINDOW v_afic05
   
END MAIN

{
======================================================================
Clave: 
Nombre: fn_consulta_afi_relacion_laboral
Fecha creacion: Julio 08, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta el programa en segundo plano que realiza la consulta de los
derechohabientes que tienen relacion laboral vigente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_afi_relacion_laboral(p_usuario_cod, p_proceso_cod, p_opera_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_proceso_cod    LIKE cat_proceso.proceso_cod,
       p_opera_cod      LIKE cat_operacion.opera_cod,
       v_seg_modulo RECORD LIKE seg_modulo.*,
       v_ruta_listados LIKE seg_modulo.ruta_listados,
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_i_resultado     INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio,
       p_pid             LIKE bat_ctr_proceso.pid,
       r_resultado_opera SMALLINT

   -- se obtienen las rutas de control del modulo
   SELECT s.*
   INTO   v_seg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'afi'
      
   SELECT b.ruta_listados
   INTO   v_ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   -- este proceso no tiene archivo
   LET v_nombre_archivo = "NA"
   LET p_folio = 0
   
   -- se obtiene el ID del proceso
   CALL fn_genera_pid(p_proceso_cod, p_opera_cod, p_usuario_cod)
        RETURNING p_pid
   
   -- Inicializa proceso porque es la primera operacion
   CALL fn_inicializa_proceso(p_pid,p_proceso_cod,p_opera_cod,0,"AFIL13","",
                              p_usuario_cod) RETURNING v_i_resultado 
                              
   -- si se inicio el proceso correctamente
   IF ( v_i_resultado = 0) THEN
    	
    	-- Inicio operacion.
      CALL fn_actualiza_opera_ini(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod,
                                  p_folio,
                                  "AFIL13",
                                  v_nombre_archivo,
                                  p_usuario_cod
                                  )
         RETURNING r_resultado_opera
         
      -- si se pudo iniciar la operacion
    	IF ( r_resultado_opera  = 0 ) THEN
    	
    	   -- se invoca la preliquidacion
         LET v_s_comando = " nohup time fglrun ",v_seg_modulo.ruta_bin CLIPPED,"/AFIP13 ",
                           p_usuario_cod CLIPPED, " ",
                           p_pid  , " " ,
                           p_proceso_cod , " " ,
                           p_opera_cod ," ",
                           p_folio ," ",
                           v_nombre_archivo CLIPPED," ",
                           " 1>", v_ruta_listados CLIPPED,
                           "/nohup:",p_pid USING "&&&&&",":",
                           p_proceso_cod USING "&&&&&",":",
                           p_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"
         DISPLAY v_s_comando                        
         RUN v_s_comando
         CALL fn_mensaje("Atención",
                         "Se ha enviado la consulta.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
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