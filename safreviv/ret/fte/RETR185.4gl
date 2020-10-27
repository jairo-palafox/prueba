--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR185                                                                 #
#Objetivo     => Programa que ejecuta la rutina de reverso de retiros por disposicion de#
#                de recursos por causa de inconsistencia entre montos notificados por   #
#                correo vs montos encontrados en archivo de carga                       #
#Fecha inicio => Octubre 19, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion  SMALLINT -- forma como ejecutara el programa
       ,p_d_folio         LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera         SMALLINT
       ,v_sql             STRING -- cadena con una instruccion SQL
       ,p_marca           SMALLINT 

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_marca          = 808

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR185.log")

   -- se inicia el reverso de los registros integrados
   --LET g_proceso_cod = g_proceso_cod_ret_disposicion -- retiros por disposicion de recursos
   --LET g_opera_cod   = g_opera_cod_ret_disp_integracion  -- integracion

   DISPLAY "Se inicia el reverso del proceso de Retiros por Disposición de Recursos por inconsistencia de\n",
           "montos encontrados en archivo vs montos notificados por correo electrónico.\n"
   
   DISPLAY "Aplicando reverso a registros integrados..."


   {
   
   -- Se invoca rutina para reversar la integración.
   CALL fn_reversa_integracion(p_d_folio, g_proceso_cod)
      RETURNING r_bandera
      
   IF ( r_bandera = 0 ) THEN
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   -- Reversa operación
   LET r_bandera = 0

   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
       RETURNING r_bandera

   IF ( r_bandera = 0 ) THEN

      UPDATE glo_ctr_archivo
         SET estado = 1, folio = NULL -- cargado
       WHERE folio = p_d_folio
         AND estado = 2; -- integrado
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   -- se realiza el reverso a la carga de archivo
   DISPLAY "Aplicando reverso a carga de archivo..."

   LET g_proceso_cod = g_proceso_cod_ret_disposicion -- retiros por disposicion de recursos
   LET g_opera_cod   = g_opera_cod_ret_disp_carga  -- carga

   -- Restaura datos afectados por carga de archivo
   CALL fn_corrige_reg_carga_archivo(p_nombre_archivo)

   -- Reversa operación
   LET r_bandera = 0
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
                             RETURNING r_bandera         
   IF ( r_bandera = 0 ) THEN
      
      DISPLAY "El reverso se realizó correctamente. Solicite el reenvío del archivo."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   }

   WHENEVER ERROR CONTINUE
   --WHENEVER ERROR STOP
   -- se ejecuta el SP que realiza el reverso de la integracion y carga por causa
   -- de inconsistencia entre montos notificados contra montos encontrados en archivo
   LET v_sql = "EXECUTE FUNCTION fn_ret_disposicion_reverso_inconsistencia(?, ?, ?,?)"
   
   PREPARE sid_spreversoinconsistencia FROM v_sql
   EXECUTE sid_spreversoinconsistencia  USING p_usuario_cod, p_d_folio, g_pid,p_marca
   INTO r_bandera
   
   WHENEVER ERROR STOP

   -- si ejecuto correctamente se marcan como erroneos los procesos
   IF ( r_bandera = 0 ) THEN
      DISPLAY "El proceso de reverso ha finalizado correctamente."


      DISPLAY "Marcando error en proceso"
      
      -- se marca como erronea la integracion y la carga
      UPDATE bat_ctr_operacion
      SET    estado_cod = 3 -- error
      WHERE pid         = g_pid 
        AND proceso_cod = g_proceso_cod

      -- se marca como erroneo el proceso
      UPDATE bat_ctr_proceso
      SET    estado_cod = 3 -- error
      WHERE pid         = g_pid 
        AND proceso_cod = g_proceso_cod

   ELSE
      DISPLAY "El proceso de reverso terminó pero con errores."

      -- se marca como erronea la integracion y la carga
      UPDATE bat_ctr_operacion
      SET    estado_cod = 3 -- error
      WHERE pid         = g_pid 
        AND proceso_cod = g_proceso_cod

      -- se marca como erroneo el proceso
      UPDATE bat_ctr_proceso
      SET    estado_cod = 3 -- error
      WHERE pid         = g_pid 
        AND proceso_cod = g_proceso_cod

   END IF
   
END MAIN

{
   Funcion : fn_corrige_reg_carga_archivo
   Fecha   : febrero 21, 2012
   Descrip : corrige datos adicionales de reverso de integracion
   Aturo   : Felipe Nava
}
FUNCTION fn_corrige_reg_carga_archivo(p_nombre_archivo)
  DEFINE 
   p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   ,v_s_qry          STRING

   LET v_s_qry =  "DELETE FROM glo_ctr_archivo"
                 ,"\n WHERE nombre_archivo = ?"
                 ,"\n   AND proceso_cod = ", g_proceso_cod
                 ,"\n   AND estado = 1"
   --DISPLAY v_s_qry 
   PREPARE Prpr_LimpiaCtrArchvo FROM v_s_qry 
   EXECUTE Prpr_LimpiaCtrArchvo USING  p_nombre_archivo

END FUNCTION -- fn_corrige_reg_carga_archivo
