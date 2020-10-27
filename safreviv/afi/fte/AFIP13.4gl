--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIP13                                                                 #
#Objetivo     => Programa que escribe el archivo de salida de los registros de          #
#                derechohabientes con relacion laboral vigente                          #
#Fecha inicio => Julio 08, 2013                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio                LIKE deo_preliquida.folio_liquida,
       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       p_titulo               STRING, -- titulo del mensaje enviado en el correo
       p_mensaje              STRING, -- cuerpo del mensaje enviado
       v_resultado            SMALLINT,
       v_consulta             STRING, -- cadena con enunciado sql 
       v_mensaje              STRING,
       v_conteo_registros     DECIMAL(9,0),
       v_r_afi_derechohabiente RECORD
         nss               LIKE afi_derechohabiente.nss          ,
         rfc               LIKE afi_derechohabiente.rfc          ,
         curp              LIKE afi_derechohabiente.curp         ,
         ap_paterno_afore  LIKE afi_derechohabiente.ap_paterno_af,
         ap_materno_afore  LIKE afi_derechohabiente.ap_materno_af,
         ap_nombre_afore   LIKE afi_derechohabiente.nombre_af    ,
         nombre_imss       LIKE afi_derechohabiente.nombre_imss  ,
         nrp               LIKE afi_relacion_laboral.nrp         ,
         f_alta_nrp        LIKE afi_relacion_laboral.f_alta_nrp
       END RECORD,
       v_archivo           base.Channel, -- archivo de salida
       v_ruta_salida       VARCHAR(40), -- directorio de archivos de salida
       v_ruta_archivo      STRING -- ruta completa del archivo
       
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- retiros por disposicion de recursos
   LET g_opera_cod   = p_opera_cod -- integracion

   -- no se necesita folio
   LET p_folio = 0
     
   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "CONSULTA")

   -- se indica alta prioridad para la consulta
   EXECUTE IMMEDIATE "SET PDQPRIORITY HIGH"
   
   LET v_consulta = "\nSELECT a.nss    ,",
                    "\n a.rfc          ,",
                    "\n a.curp         ,",
                    "\n a.ap_paterno_af,",
                    "\n a.ap_materno_af,",
                    "\n a.nombre_af    ,",
                    "\n a.nombre_imss  ,",
                    "\n b.nrp          ,",
                    "\n b.f_alta_nrp    ",
                    "\nFROM afi_derechohabiente a,",
                    "\n     afi_relacion_laboral b",
                    "\nWHERE",
                    "\n a.id_derechohabiente = b.id_derechohabiente"
{
NSS (N11)
RFC (A18)
CURP(A18)
Apellido Paterno Afore (a40)
Apellido materno afore (a40)
Nombre afore (a40)
Nombre iMSS (a50)
Separados por pipe 
}

   -- se crea el objeto
   LET v_archivo = base.Channel.create()
   
   -- se usa el pipe como delimitador
   CALL v_archivo.setDelimiter("|")

   -- se obtiene la ruta de envio
   SELECT ruta_envio
   INTO   v_ruta_salida
   FROM   seg_modulo
   WHERE  modulo_cod = "afi"
   
   -- se conforma la ruta del archivo de salida
   LET v_mensaje = TODAY USING "yyyymmdd"
   LET v_ruta_archivo = v_ruta_salida CLIPPED, "/NSS_RL_", v_mensaje, ".txt"
   
   -- se abre el archivo para su escritura
   CALL v_archivo.openFile( v_ruta_archivo, "w" )
   
   PREPARE sid_relaboral FROM v_consulta
   DECLARE cur_relaboral CURSOR FOR sid_relaboral

   -- se inicia el contador
   LET v_conteo_registros = 0
   
   FOREACH cur_relaboral INTO v_r_afi_derechohabiente.*
      -- se escribe el registro en el archivo
      CALL v_archivo.write([v_r_afi_derechohabiente.*])

      -- se cuenta un registro escrito
      LET v_conteo_registros = v_conteo_registros + 1
   END FOREACH

   -- se devuelve la priodidad como estaba
   EXECUTE IMMEDIATE "SET PDQPRIORITY LOW"
   
   -- se cierra el archivo
   CALL v_archivo.close()
  
   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : AFILIACION - CONSULTA DE DERECHOHABIENTES CON RELACION LABORAL",
                   "Operación    : CONSULTA\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n"
                   
   -- se complementa el mensaje
   LET p_mensaje = p_mensaje || v_mensaje

   LET v_mensaje = "\n\nSe ha concluido la consulta de derechohabientes con relación laboral.\n",
                   "Archivo: ", v_ruta_archivo, "\n",
                   "Num. de registros: ", v_conteo_registros USING "########&",
                   "\n\n"

   DISPLAY v_mensaje

   -- se finaliza la operacion   
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                               RETURNING v_resultado

   LET p_titulo = "Finalización de operación - CONSULTA DE DERECHOHABIENTES CON RELACION LABORAL"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "CONSULTA")

END MAIN