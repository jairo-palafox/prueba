--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#############################################################################
#Módulo          => PAG                                                     #
#Programa        => PAGP52.4gl                                              #
#Objetivo        => Programa de integracion de registro de pagos de         #
#                   fondo anterior                                          #
#Fecha Inicio    => 01 Febrero 2013                                         #
#############################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   g_opera_cod_integracion     LIKE cat_operacion.opera_cod -- codigo de operacion   
END GLOBALS

MAIN
DEFINE
   v_bnd_fin_proceso     SMALLINT,
   p_num_folio           DECIMAL(9),
   p_nom_archivo         LIKE glo_ctr_archivo.nombre_archivo,
   v_estatus             SMALLINT ,
   p_titulo              STRING, -- titulo del mensaje enviado en el correo
   p_mensaje             STRING -- cuerpo del mensaje enviado

   
   -- usuario
   LET p_usuario_cod = ARG_VAL(1)

   -- PID
   LET g_pid         = ARG_VAL(2)
   
   -- PROCESO COD
   LET g_proceso_cod = ARG_VAL(3)

   -- Opera Cod
   LET g_opera_cod_integracion = ARG_VAL(4)

   -- Folio
   LET p_num_folio = ARG_VAL(5)
   
   -- Nombre de acrhivo
   LET p_nom_archivo = ARG_VAL(6)
   
   --Inicializacion de variables
   LET v_bnd_fin_proceso = 0      

   -- se genera el numero de folio
   LET p_num_folio = fn_genera_folio(g_proceso_cod, g_opera_cod_integracion, p_usuario_cod)

   -- Se despliega el inicio de la operacion
   CALL fn_display_proceso(0,"INTEGRACIÓN")
                                 
      -- Llamada a ejecución de procedimiento almacenado
      CALL fn_guarda_historicos_fondo_anterior(p_num_folio) RETURNING v_estatus
      
      -- si el store regresa el v_estatus = 0 signifioca que no hubo errores 
      IF ( v_estatus = 0 ) THEN
         -- Se actualiza el archivo como integrado
         CALL fn_act_edo_archivo(p_nom_archivo, p_num_folio, 2, p_usuario_cod ) RETURNING v_estatus
         
         -- Se registra el FIN DE LA OPERACION COMO EXITOSA
         CALL fn_actualiza_opera_fin(g_pid
                                    ,g_proceso_cod
                                    ,g_opera_cod_integracion)
                                    RETURNING v_estatus
         LET p_mensaje = "Integración realizada con éxito.\nYa se puede continuar con la Preliquidación."
      ELSE
         LET p_mensaje = "El proceso de Integración ha finalizado pero con errores.\nNo se puede continuar con el proceso de Preliquidación."

         -- Si ocurrio un error se actualiza el estatus como erroneo
         CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_integracion)  RETURNING v_estatus
      END IF
      -- Se ejecutan los displays
      CALL fn_display_proceso(1,"INTEGRACIÓN")

      LET p_titulo = "Finalización de operación - FONDO ANTERIOR - Integración"
      CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                             g_opera_cod_integracion, 
                             NULL, p_titulo,p_mensaje)

END MAIN

{ ==========================================================================
Clave:  fn_guarda_historicos_fondo_anterior
Nombre: fn_guarda_historicos_fondo_anterior
Fecha creacion: 01 Febrero 2013     
Autor: Ivan Vega
Narrativa del proceso que realiza:
Ejecuta el Stored Procedure que realiza el proceso de integracion de los
datos cargados de fondo anterior

Parametros de Entrada:

Parámetros de salida;

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_guarda_historicos_fondo_anterior(p_folio)
DEFINE p_folio          DECIMAL(9),
       v_sql_procedure  STRING,
       v_estatus        SMALLINT,
       v_cod_error      SMALLINT,
       v_error_isam     INTEGER,
       v_mensaje_error  VARCHAR(255),
       v_nss_error      CHAR(11),
       v_rfc_error      CHAR(13)
       
       
   LET v_estatus = 0 -- El cero indica que se jecuto con exito
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_registro_historicos_fa(?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_historicoPag FROM v_sql_procedure
   EXECUTE prp_historicoPag USING p_folio ,g_pid,g_proceso_cod
   INTO v_cod_error, v_error_isam, v_mensaje_error, v_nss_error, v_rfc_error

   IF (v_cod_error = 0) THEN
      -- Ejecucion sin error
      DISPLAY v_mensaje_error
      DISPLAY v_cod_error
   ELSE 
      IF(v_cod_error = 1000)THEN
         UPDATE glo_folio
            SET STATUS = -1
          WHERE folio = p_folio
      END IF
      -- ocurrio un error al integrar
      LET v_estatus = 1  --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "Error (SQL)    : ", v_cod_error
      DISPLAY "Error (ISAM)   : ", v_mensaje_error
      DISPLAY "Error (Mensaje): ", v_error_isam
      DISPLAY "NSS en curso   : ", v_nss_error
      DISPLAY "RFC en curso   : ", v_rfc_error
   END IF

   -- se devuelve el resultado de la ejecucion
   RETURN v_estatus
END FUNCTION