--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-10-2013
--==============================================================================

################################################################################
#Modulo            =>                                                          #
#Programa          => RETL308                                                  #
#Objetivo          => Programa lanzador para la preliquidacion de restituci�n  #
#                     de retiros gen�ricos Ley 73 rechazados por FICO          #
#Autor             =>                                                          #
#Fecha inicio      => 08 Noviembre 2013                                        #
################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"
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

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion SMALLINT, # forma como ejecutara el programa
       p_titulo_vtna    STRING,   # titulo de la ventana
       v_folio          LIKE ret_preliquida.folio_liquida,
       v_consulta       STRING

   DEFINE v_countador_ret SMALLINT 
   DEFINE v_estado_solicitud SMALLINT 

   # recupera par�metros
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   LET v_estado_solicitud = gi_estado_restitucion_rechazo # 90
   LET v_folio = 0

   # se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_restitucion_ret_generico_ley73fico     # 1543 Restitucion retiro  ley 73
   LET g_opera_cod   = g_opera_cod_restitucion_ret_generico_preliquidacion # 1    Preliquidacion

   # se obtienen las rutas de control del modulo
   SELECT ruta_bin, 
          ruta_rescate, 
          ruta_listados
     INTO g_reg_modulo.ruta_exp,
          g_reg_modulo.ruta_rescate,
          g_reg_modulo.ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'ret'

    SELECT ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo
     WHERE modulo_cod = 'bat'

   LET v_consulta = " SELECT aa.cuenta_titular + bb.cuenta_beneficiario                    ",
                    " FROM   (SELECT COUNT(distinct a.id_solicitud) AS cuenta_titular      ",
                    "         FROM   ret_solicitud_generico a,                             ",
                    "                ret_beneficiario_generico b                           ",
                    "         WHERE  a.id_solicitud = b.id_solicitud                       ",
                    "         AND    b.tpo_beneficiario = 1                                ",
                    "         AND    a.estado_solicitud = ?                                ",
                    "         AND    a.cod_rechazo = 64                                    ",
                    "         AND    a.modalidad_retiro = 3) aa,                           ",
                    "        (SELECT COUNT(distinct a.id_solicitud) AS cuenta_beneficiario ",
                    "         FROM   ret_solicitud_generico a,                             ",
                    "                ret_beneficiario_generico b,                          ",
                    "                ret_beneficiario_juridico c                           ",
                    "         WHERE  a.id_solicitud = b.id_solicitud                       ",
                    "         AND    a.id_solicitud = c.id_solicitud                       ",
                    "         AND    b.tpo_beneficiario = 2                                ",
                    "         AND    b.consec_beneficiario = c.consec_beneficiario         ",
                    "         AND    c.estado_solicitud = ?                                ",
                    "         AND    c.cod_rechazo = 64                                    ",
                    "         AND    a.modalidad_retiro = 3) bb                            "



     
--   LET v_consulta = "  SELECT NVL(COUNT(*),0)", 
--                    "    FROM ret_solicitud_generico",
--                    "   WHERE estado_solicitud = ?",
--                    "   AND cod_rechazo = 64      ",
--                    "   AND modalidad_retiro = 3" # 3 =  Retiro gen�rico Ley 73

   PREPARE prp_conteo_solicitudes FROM v_consulta
  
   # se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM g_reg_modulo.ruta_exp CLIPPED||"/RETL3081"
      # si se obtuvo el titulo, se pone como titulo de programa
      IF ( p_titulo_vtna IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_titulo_vtna)
      END IF

      MENU  
         BEFORE MENU
            
            EXECUTE prp_conteo_solicitudes USING v_estado_solicitud, v_estado_solicitud 
                                            INTO v_countador_ret
            IF( v_countador_ret = 1 )THEN  
               DISPLAY "Se va a generar la preliquidacion de "|| v_countador_ret ||" registro. Seleccione aceptar para continuar ." TO lb_mensage
            ELSE
               IF( v_countador_ret > 1 )THEN  
                  DISPLAY "Se va a generar la preliquidacion de "|| v_countador_ret ||" registros. Seleccione aceptar para continuar ." TO lb_mensage
               ELSE 
                  DISPLAY "No se va a generar la preliquidacion se encontraron "|| v_countador_ret ||" registros. Seleccione cancelar para Salir ." TO lb_mensage
               END IF 
            END IF 
  
         ON ACTION ACCEPT
            
            EXECUTE prp_conteo_solicitudes USING v_estado_solicitud, v_estado_solicitud
                                            INTO v_countador_ret
            
            IF( v_countador_ret > 0 )THEN
              CALL fn_ret_ejecuta_preliquidacion(v_folio,p_usuario_cod)
            ELSE 
              CALL fn_mensaje("Atenci�n","No existen solicitudes para \nrealizar preliquidaci�n","information")  
            END IF 
            EXIT MENU 
        
         ON ACTION CANCEL
            EXIT MENU 

      END MENU

   CLOSE WINDOW w_folio_preliquida
END MAIN

################################################################################
#Modulo            => RET                                                      #
#Programa          => RETL308                                                  #
#Descripcion       =>                                                          #
#Autor             =>                                                          #
#Fecha inicio      => 15 de Octubre de 2013                                    #
################################################################################
FUNCTION fn_ret_ejecuta_preliquidacion(p_folio, p_usuario_cod)
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, # usuario que ejecuta el programa
       p_folio            LIKE glo_folio.folio, # folio para preliquidar
       v_comando          STRING,               # cadena con una instruccion de consola
       v_bandera          SMALLINT,             # para revisar que los procesos ejecutaron bien
       v_nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo
       v_programa         VARCHAR(50),
       v_estado_operacion SMALLINT
 
   # se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"  
   LET v_programa       = "RETL308" 
   LET v_bandera        = 0


   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING v_estado_operacion
   # se verifica si se puede continuar con la operacion
   IF ( v_estado_operacion = 0 ) THEN
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid
      LET p_folio = 0
      
      CALL fn_inicializa_proceso(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod,
                                 p_folio,
                                 v_programa,
                                 v_nombre_archivo,
                                 p_usuario_cod)  RETURNING v_bandera
                                  
      # el proceso se registro correctamente
      
      IF ( v_bandera = 0 ) THEN
         # inicializa la operacion
         CALL fn_actualiza_opera_ini(g_pid,         # pid
                                     g_proceso_cod, # proceso
                                     g_opera_cod,   # operacion
                                     p_folio,       # folio
                                     "RETL308",     # programa
                                     v_nombre_archivo,     # archivo  
                                     p_usuario_cod) RETURNING v_bandera
   
         IF( v_bandera = 0 ) THEN
            LET v_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP306 ",
                                                    p_usuario_cod CLIPPED, " ",
                                                    g_pid  , " " ,
                                                    g_proceso_cod , " " ,
                                                    g_opera_cod ," ",
                                                    p_folio ," ",
                                                    v_nombre_archivo CLIPPED," ",
                            " 1>",seg_modulo_bat.ruta_listados CLIPPED ,"/nohup:",g_pid USING "&&&&&",":",
                                                                                  g_proceso_cod USING "&&&&&",":",
                                                                                  g_opera_cod   USING "&&&&&" ,
                            " 2>&1 &"
                          
            DISPLAY v_comando                         
            RUN v_comando
       
            CALL fn_mensaje("Atenci�n","Se ha enviado la preliquidaci�n.\nPuede revisar el avance del proceso en el monitor de ejecuci�n de procesos","information")
         ELSE
            CALL fn_muestra_inc_operacion(v_bandera)
            CALL fn_error_opera(g_pid,
                                g_proceso_cod,
                                g_opera_cod)RETURNING v_bandera
            # si ocurri� un error con la actualizacion de la operacion operacion 
            # muestra el mensaje
            IF(v_bandera)THEN
               CALL fn_muestra_inc_operacion(v_bandera)
            END IF
         END IF
      ELSE
         CALL fn_mensaje("Atenci�n","No se pudo iniciar el proceso","information")
      END IF
   ELSE
      CALL fn_muestra_inc_operacion(v_estado_operacion)
      --CALL fn_mensaje("Atenci�n","No se pudo iniciar el proceso existe preliquidacion previa","information")
   END IF
 
END FUNCTION