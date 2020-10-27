--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETP481                                                 #
#OBJETIVO          => PROGRAMA LANZADO PARA EL ENVIO DE LAS CARTAS DE         #
#                     CONFORMIDAD DE LAS SOLICITUDES TRAITADAS POR TABLETA    #
#FECHA INICIO      => 28-MAY-2019                                             #
# Autor           Fecha      Modificación                                     #
###############################################################################

IMPORT com
IMPORT SECURITY
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
-- registro de entrada para la consulta
PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_folio                    LIKE glo_folio.folio

#
# MAIN
#
MAIN
DEFINE v_resultado        INTEGER -- recibe el resultado de la ejecucion del servicio 
DEFINE p_pid              LIKE bat_ctr_operacion.pid         # PID del proceso
DEFINE p_proceso_cod      LIKE bat_ctr_operacion.proceso_cod # codigo del proceso
DEFINE p_opera_cod        LIKE bat_ctr_operacion.opera_cod   # codigo de la operacion
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod       # clave del usuario firmado
DEFINE p_folio            LIKE ret_preliquida.folio_liquida
       
   # recupera parametros
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)

   --Descripción del proceso
   SELECT proceso_desc
   INTO   v_proceso_desc
   FROM   cat_proceso
   WHERE  proceso_cod = p_proceso_cod

   --Descripcion de la operacion
   SELECT opera_desc
   INTO   v_opera_desc
   FROM   cat_operacion
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = p_opera_cod

   --Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY ""
   DISPLAY ""

   -- Se solicita el numero de folio asociado a la operacion. Parametros: proceso, operacion, usuario
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   --Se actualiza el folio del proceso
   UPDATE bat_ctr_proceso   
   SET    folio = v_folio
   WHERE  pid = p_pid
   UPDATE bat_ctr_operacion
   SET    folio = v_folio
   WHERE  pid = p_pid
    
   CALL f_envia_cartas() RETURNING v_resultado
   IF v_resultado = 0 THEN
       --Finaliza la operacion
      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING v_resultado

      IF(v_resultado <> 0)THEN         
         # Actualiza a estado erróneo
         DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING v_resultado
      END IF

      --Encabezado para el archivo de monitoreo
      DISPLAY ""
      DISPLAY ""
      DISPLAY ""
      DISPLAY "Terminó el Batch de Envio de Cartas de Aceptación Tramitadas por Tableta"
      DISPLAY "*******************************************************************"
      DISPLAY " PROCESO            : ",v_proceso_desc
      DISPLAY " OPERACIÓN          : ",v_opera_desc
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY "*******************************************************************"
   ELSE 
      DISPLAY "Proceso concluido con error: ", v_resultado
      DISPLAY CURRENT YEAR TO SECOND
   END IF 
      
END MAIN

{
======================================================================
Clave: 
Nombre: f_envia_cartas
Fecha creacion: Mayo 28, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Envia las cartas de conformidad de las solicitudes tramitadas por la tableta

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_envia_cartas()
DEFINE v_ruta_listados       VARCHAR(40)
DEFINE v_comando             STRING 
DEFINE v_documentos RECORD LIKE ret_documento_devolucion.*
DEFINE v_ruta_zip            STRING 
DEFINE v_ruta_destino_zip    STRING
DEFINE v_archivo_zip         STRING
DEFINE v_carta_enviar        STRING
DEFINE v_c_id_solicitud      STRING
 
DEFINE v_resultado           SMALLINT 
DEFINE v_sql                 STRING        

   -- se obtiene la ruta de trabajo 
   SELECT ruta_envio
   INTO   v_ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"   
   LET v_resultado = 0
  

   LET v_sql = "SELECT *                             "||
               "FROM   ret_documento_devolucion      "||
               "WHERE  estado_solicitud IS NULL      "||
               "AND    correo_trabajador IS NOT NULL "

   PREPARE prp_carta_documento FROM v_sql
   DECLARE cur_carta_documento CURSOR FOR prp_carta_documento
   --- Se crea la carpeta temporal
  
   FOREACH cur_carta_documento INTO v_documentos.*
      -- Se crea el directorio de trabajo
      
      LET v_ruta_destino_zip = v_ruta_listados CLIPPED, "/doc_dev/", v_documentos.nss, "_tmp/"
      LET v_comando = "mkdir -p ", v_ruta_destino_zip
      DISPLAY "Se crea el directorio para el NSS :", v_documentos.nss
      DISPLAY "Directorio destino                :", v_ruta_destino_zip
      DISPLAY "Comando                           :", v_comando  
      RUN v_comando

      -- Copia el zip a la ruta de trabajo

      LET v_comando = "cp ", v_documentos.documentos CLIPPED, " ", v_ruta_destino_zip CLIPPED, "."
      DISPLAY "Copia el archivo a la ruta de trabajo :", v_ruta_destino_zip
      DISPLAY "Comando                           :", v_comando  
      RUN v_comando

      
      -- Desempaca el zip para la obtención de la carta de confirmación
      LET v_c_id_solicitud = v_documentos.id_solicitud
      LET v_archivo_zip = "DEV_SSV_", v_documentos.nss CLIPPED, "_ID_", v_c_id_solicitud CLIPPED, ".zip" 
      LET v_comando = "unzip ", v_ruta_destino_zip CLIPPED, v_archivo_zip CLIPPED, " -d ",v_ruta_destino_zip CLIPPED 
      DISPLAY "Desempaca el archivo para obtener la carta :", v_archivo_zip
      DISPLAY "Comando                                    :", v_comando  
      RUN v_comando
      
      -- Arma y envia el correo 
      LET v_carta_enviar = "CARTA_1_", v_c_id_solicitud CLIPPED, ".pdf"
      LET v_comando = "echo 'Carta de Confirmación Devolución del Saldo de la Subcuenta de Vivienda del NSS :", v_documentos.nss, "' > ", v_ruta_destino_zip CLIPPED, "body.txt ;" 
--      LET v_comando = "uuencode ",v_ruta_destino_zip CLIPPED, v_carta_enviar CLIPPED,"  ",v_ruta_destino_zip CLIPPED, v_carta_enviar CLIPPED, " > ",v_ruta_destino_zip CLIPPED, v_carta_enviar CLIPPED, ".txt ; ", 
--                   "cat ", v_ruta_destino_zip CLIPPED,"body.txt ",v_ruta_destino_zip CLIPPED, v_carta_enviar CLIPPED, ".txt > ",v_ruta_destino_zip CLIPPED, v_carta_enviar CLIPPED, ".txt.mail ; "

--      LET v_comando = v_comando CLIPPED, " mailx -s 'Devolución del Saldo de la Subcuenta de Vivienda' ", v_documentos.correo_trabajador, " < ", v_ruta_destino_zip CLIPPED, v_carta_enviar CLIPPED, ".txt.mail ;"
-- Comando con el parámetro -a
      LET v_comando = v_comando CLIPPED, " mailx -s 'Devolución del Saldo de la Subcuenta de Vivienda' -r safreviv@infonavit.org.mx -a ", v_ruta_destino_zip CLIPPED, v_carta_enviar CLIPPED, " ", v_documentos.correo_trabajador, " < ", v_ruta_destino_zip CLIPPED, "body.txt ;"
      DISPLAY v_comando
      RUN v_comando 
      
      -- Actualiza la tabla para indicar que se envió el correo
      UPDATE ret_documento_devolucion
      SET    estado_solicitud = 800 -- Procesada
      WHERE  id_solicitud = v_documentos.id_solicitud
      AND    estado_solicitud IS NULL 
      DISPLAY "Solicitud_actualizada :", v_documentos.id_solicitud
      DISPLAY "NSS                   :", v_documentos.nss
      -- Borra los archivos temporales
      DISPLAY "Borrando temporales :", v_ruta_destino_zip CLIPPED 
      LET v_comando = "rm -r ", v_ruta_destino_zip CLIPPED 
      DISPLAY "Comando             :", v_comando
      RUN v_comando
   END FOREACH
   RETURN v_resultado
END FUNCTION

