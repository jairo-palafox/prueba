--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR432                                                                #
#Objetivo     => Programa que ejecuta el reverso de la carga de cargos SSV              #
#Fecha inicio => Junio 19, 2017                                                       #
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
       ,p_d_folio         LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera         SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR432.log")

   DISPLAY "Ejecutando rutina de reverso de carga..."
   DISPLAY "PID             : ", g_pid           
   DISPLAY "PROCESO COD     : ", g_proceso_cod   
   DISPLAY "OPERA COD       : ", g_opera_cod     
   DISPLAY "FOLIO           : ", p_d_folio       
   DISPLAY "NOMBRE ARCHIVO  : ", p_nombre_archivo


   -- Restaura datos afectados por carga de archivo
   CALL fn_corrige_reg_carga_archivo(p_nombre_archivo)
   DISPLAY "El reverso se realiz� con �xito"

   -- Reversa operaci�n
   LET r_bandera = 0
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
                             RETURNING r_bandera
         
   IF ( r_bandera = 0 ) THEN
      
      DISPLAY "Operaci�n lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

END MAIN

{
   Funcion : fn_corrige_reg_carga_archivo
   Fecha   : Octubre 08, 2012
   Descrip : corrige datos adicionales de reverso de integracion
}
FUNCTION fn_corrige_reg_carga_archivo(p_nombre_archivo)
DEFINE p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_s_qry          STRING

   LET v_s_qry = "DELETE FROM glo_ctr_archivo"
                ,"\n WHERE nombre_archivo = ?"
                ,"\n   AND proceso_cod = ", g_proceso_cod
                ,"\n   AND estado = 1"

   --DISPLAY v_s_qry 
   PREPARE Prpr_LimpiaCtrArchvo FROM v_s_qry 
   EXECUTE Prpr_LimpiaCtrArchvo USING  p_nombre_archivo

END FUNCTION -- fn_corrige_reg_carga_archivo