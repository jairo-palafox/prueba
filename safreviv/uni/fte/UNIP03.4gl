--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 08/01/2013
--===============================================================

#########################################################################################
#Modulo       => UNI                                                                    #
#Programa     => UNIP03                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integración    #
#                para la Unificacion de cuentas solo INFONAVIT                          #
#Fecha inicio => 21/05/2012                                                             #
#########################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion

DEFINE g_reg_modulo   RECORD  --Almacena las rutas de archivos 
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
END GLOBALS

MAIN
   DEFINE p_pid                      LIKE bat_ctr_operacion.pid,           -- PID del proceso
          p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod,   -- codigo del proceso
          p_opera_cod                LIKE bat_ctr_operacion.opera_cod,     -- codigo de la operacion
          p_usuario_cod              LIKE seg_usuario.usuario_cod,         -- clave del usuario firmado
          v_s_sql                    STRING,                               -- cadena con una instruccion SQL
          v_i_resultado              INTEGER,                              -- resultado del proceso
          r_bnd_fin_oper             SMALLINT,
          p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo,  -- nombre dle archivo
          v_si_solicitudes_totales   SMALLINT,
          v_si_solicitudes_aceptadas SMALLINT,
          v_msj_sql                  CHAR(200),
          v_folio                    LIKE deo_preliquida.folio_liquida,

          v_si_status_detalle_trabaj  SMALLINT,
          v_c_status_proc             CHAR(200),
          v_si_total_trabaja          SMALLINT,
          p_titulo                    STRING,                              -- titulo del mensaje enviado en el correo
          p_mensaje                   STRING,                              -- cuerpo del mensaje enviado
          v_layout                    LIKE cat_operacion.layout_cod,
          v_ruta_rescate              STRING,
          v_usuario                   LIKE seg_modulo.usuario,
          v_proceso_desc              LIKE cat_proceso.proceso_desc,
          v_extension                 LIKE cat_operacion.extension,
          v_opera_desc                LIKE cat_operacion.opera_desc,
          v_ruta_listados             LIKE seg_modulo.ruta_listados,
          v_sum_total_registro        INTEGER,
          v_diagnostico               SMALLINT,
          v_si_solicitudes_aceptadas_unificador DECIMAL(9,0),
          v_si_solicitudes_aceptadas_unificadas DECIMAL(9,0),
          v_total_rechazados          INTEGER,
          v_s_comando                 STRING

   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro
   -- argumento con indice 1
   LET p_pid            = ARG_VAL(1)
   LET p_proceso_cod    = ARG_VAL(2)
   LET p_opera_cod      = ARG_VAL(3)
   LET p_usuario_cod    = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod)
        RETURNING v_proceso_desc,
                  v_extension,
                  v_opera_desc,
                  v_layout,
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   -- se asigna proceso y operacion
   -- se asigna proceso y operacion
   LET g_pid         = p_pid
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod   -- preliquidacion


   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'uni'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
    
   -- Ejecuta prevalidación de encabezados y sumarios
   LET v_i_resultado = 0
   LET v_s_sql = "EXECUTE FUNCTION fn_uni_integra_det_infonavit(?,?,?,?,?)"
   PREPARE Prpr_integraINFONAVIT FROM v_s_sql CLIPPED

    -- se ejecuta el stored procedure de integracion
    EXECUTE Prpr_integraINFONAVIT USING p_usuario_cod ,
                                        p_proceso_cod,
                                        p_nombre_archivo,
                                        v_folio,
                                        g_pid
            INTO v_i_resultado,
                 v_sum_total_registro,
                 v_msj_sql,
                 v_si_solicitudes_aceptadas_unificador,
                 v_si_solicitudes_aceptadas_unificadas

   -- si el proceso finalizo correctamente
   IF ( v_i_resultado = 0 ) THEN

      --Se finaliza aunque existan errores
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    "
      DISPLAY "#    La integración se terminó completamente."
      DISPLAY "#    "
      DISPLAY "#    Folio lote o de integración: "||v_folio

      IF ( v_sum_total_registro > 0 ) THEN
         DISPLAY "#  Integración realizada con exito"
      ELSE
         DISPLAY "#  Integración realizada pero con errores de validación"
      END IF

      DISPLAY "#  "
      DISPLAY "#  Total de familias     : ",v_sum_total_registro
      DISPLAY "#  Total de unificadores : ",v_si_solicitudes_aceptadas_unificador
      DISPLAY "#  Total de unificados   : ",v_si_solicitudes_aceptadas_unificadas
      DISPLAY "#    "
      DISPLAY "#  Codigo de error :",v_i_resultado

      --IF ( v_si_solicitudes_aceptadas_unificador >= 0 ) THEN
      IF ( v_si_solicitudes_aceptadas_unificador > 0 ) THEN
         CALL fn_actualiza_opera_fin(g_pid,
                                     g_proceso_cod,
                                     g_opera_cod)
              RETURNING r_bnd_fin_oper

         DISPLAY "#  Ya se puede Continuar con la Preliquidación"

         LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # #","\n",
                         "#  La integración se terminó completamente.","\n",
                         "#  ","\n",
                         "#  Integración realizada con exito","\n",
                         "#  ","\n",
                         "#  Folio lote o de integración : ",v_folio,"\n",
                         "#  ","\n",
                         "#  Total de familias     : ",v_sum_total_registro,"\n",
                         "#  Total de unificadores : ",v_si_solicitudes_aceptadas_unificador,"\n",
                         "#  Total de unificados   : ",v_si_solicitudes_aceptadas_unificadas,"\n",
                         "#  ","\n",
                         "#  Codigo de error :",v_i_resultado,"\n",
                         "#  Ya se puede Continuar con la Preliquidación","\n",
                         "# # # # # # # # # # # # # # # # # # # # # # # # # #"

      ELSE
         -- no se tienen solicitudes aceptadas de NSS unificador
         DISPLAY "ERROR: No se tienen solicitudes aceptadas de NSS unificador"
         DISPLAY "# ",fn_status_secciones_integradas(v_si_solicitudes_aceptadas_unificadas)

         CALL fn_error_opera(g_pid,
                             g_proceso_cod,
                             g_opera_cod)
              RETURNING r_bnd_fin_oper

         LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de Integración no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"
         DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)

         LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de Integración no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"
      END IF

      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "\n\n"
   ELSE
      -- la ejecucion del SP finalizo con errores
      DISPLAY "Ocurrio un error al ejecutar el SP: fn_uni_integra_det_infonavit"
      DISPLAY "Codigo de error :",   v_i_resultado
      DISPLAY "isam error:",         v_sum_total_registro
      DISPLAY "Ultimo mensaje:",     v_msj_sql
      DISPLAY "Derechohabiente Dor:",v_si_solicitudes_aceptadas_unificador
      DISPLAY "Derechohabiente Ado:",v_si_solicitudes_aceptadas_unificadas

      CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod)
           RETURNING r_bnd_fin_oper
   END IF

   --Se valida si se cuenta con rechazos
   SELECT COUNT(*)
   INTO   v_total_rechazados
   FROM   safre_tmp:tmp_mov_afi_trab_op21
   WHERE  diagnostico = 2

   LET p_nombre_archivo = p_nombre_archivo CLIPPED
   --Si existe algúnr registro con rechazos
   IF v_total_rechazados > = 1 THEN 
      --Se genera el archivo de rechazos
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIS03 ",
                                              g_pid  , " " ,
                                              g_proceso_cod , " " ,
                                              g_opera_cod ," ",
                                              p_usuario_cod, " ",
                                              v_folio, " ",
                                              "'",p_nombre_archivo, "' ",
                                              p_mensaje,
                                              " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                              "/nohup:",g_pid        USING "&&&&&",":",
                                              g_proceso_cod USING "&&&&&",":",
                                              g_opera_cod   USING "&&&&&" ,
                                              " 2>&1 &"
       DISPLAY v_s_comando
       RUN v_s_comando

       DISPLAY "Se ha generado un archivo de rechazos"       
   END IF 
   --#  

   
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
END MAIN

{

 Obtiene la descripcion del error de la validacion y
  la muestra en mensaje para suario.
}
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT,
         v_descripcion LIKE cat_bat_parametro_salida.descripcion

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado

   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

FUNCTION fn_status_secciones_integradas(v_si_detalle)
   DEFINE v_si_detalle        SMALLINT,
          v_c_mensaje CHAR(100)

   LET v_c_mensaje = ""

   IF(v_si_detalle = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Detalle ]"
   END IF

   RETURN v_c_mensaje

END FUNCTION 