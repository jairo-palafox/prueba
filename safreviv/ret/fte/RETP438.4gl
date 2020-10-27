--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP438                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                del archivo de Diferencias SSV Pensionados                             #
#Fecha inicio => Febrero 22, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS "../../cta/fte/CTAW15.inc"
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
       v_s_sql                STRING, -- cadena con una instruccion SQL
       v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper        SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,v_error_isam          INTEGER
       ,v_mensaje             VARCHAR(250)       
       ,v_nss                 char(11)
       ,v_s_comando           STRING
       ,v_ruta_bin            CHAR(40)
       ,v_ruta_listado        CHAR(40)

       ,v_url                CHAR(100) 
       ,v_usuario            CHAR(20) 
       ,v_pass               CHAR(20)
       ,v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
       ,v_subcta_viv97       SMALLINT
       ,v_subcta_viv92       SMALLINT
       ,v_hoy                DATE
       ,v_indice             INTEGER 
       ,v_diagnostico        INTEGER 
       ,v_ch_arc_aceptados   base.channel --Canal para escribir el archivo de salida
       ,v_s_detalle_acp      STRING
       
   DEFINE v_datos_consulta_procesar   RECORD LIKE ret_dif_ssv_pensionado.*

   DEFINE v_resp_serv_saldo RECORD
         apeMaternoBD      CHAR(40), 
         apePaternoBD      CHAR(40), 
         diagProceso       SMALLINT, 
         nombresBD         CHAR(40), 
         nss               CHAR(11), 
         numAIVS92         DECIMAL(24,6), 
         numAIVS97         DECIMAL(24,6), 
         origenTipoCredito SMALLINT, 
         resultOperacion   SMALLINT, 
         tramiteJudicial   CHAR(20)               
      END RECORD, 

      v_c_ruta_env_acr             LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
      v_v_nom_archivo              STRING, -- nombre del archivo de salida
      v_v_ruta_arc_aceptados       STRING --Ruta del archivo de aceptados


   
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
   LET g_proceso_cod = p_proceso_cod --retiros de fondo ahorro
   LET g_opera_cod   = p_opera_cod   --integracion
   INITIALIZE v_datos_consulta_procesar TO NULL    
   INITIALIZE v_resp_serv_saldo TO NULL
{   --Se obtiene el folio --ERV
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio
}
   --Se asume que el proceso termina correctamente--
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   --Se envia la cadena que indica el inicio de etapa--
   CALL fn_display_proceso(0, "Procesar")
   --- se buscan parámetros para la consulta del servicio

   SELECT url, usuario, pass
   INTO   v_url, v_usuario, v_pass
   FROM   ret_proc_parametros_ws
   WHERE  proceso_cod = g_proceso_cod

   -- Se busca la ruta donde se depositará el archivo de salida
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'agr'
   
   LET v_v_nom_archivo         = "/SSVPensionados_", TODAY USING "ddmmyyyy", ".ssvp"
   LET v_v_ruta_arc_aceptados = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
   --Creación del canal para la escritura del archivo
   LET v_ch_arc_aceptados = base.Channel.create()     
   
   --Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arc_aceptados.openFile(v_v_ruta_arc_aceptados, "w" )

   DISPLAY "Parametros de ejecucion: "
   DISPLAY "Usuario_cod: ", p_usuario_cod    
   DISPLAY "PID        : ", p_pid            
   DISPLAY "Proceso_cod: ", p_proceso_cod    
   DISPLAY "Opera_cod  : ", p_opera_cod      
   DISPLAY "Folio      : ", p_folio          
   DISPLAY "Archivo    : ", p_nombre_archivo 


   LET v_s_sql = "\n SELECT fch_inicio, fch_fin, nss,               ",
                 "\n        marca_credito, marca_operativa, aivs_viv92_procesar, ",
                 "\n        aivs_viv97_procesar,ape_paterno,ape_materno,nombre,  ",
                 "\n        aivs_viv92_saci,aivs_viv97_saci                      ",
                 "\n FROM   ret_dif_ssv_pensionado                               "
   -- consulta
   DISPLAY "Consulta:\n", v_s_sql
   --- se buscan parámetros para la consulta del servicio

   -- se llena el arreglo 
   PREPARE sid_query FROM v_s_sql
   DECLARE cur_query CURSOR FOR sid_query

   LET v_indice = 1

   -- se transfieren los datos al arreglo de despliegue agrupador
   FOREACH cur_query INTO v_datos_consulta_procesar.fch_inicio,
                          v_datos_consulta_procesar.fch_fin,
                          v_datos_consulta_procesar.nss,
                          v_datos_consulta_procesar.marca_credito,
                          v_datos_consulta_procesar.marca_operativa,
                          v_datos_consulta_procesar.aivs_viv92_procesar,
                          v_datos_consulta_procesar.aivs_viv97_procesar,
                          v_datos_consulta_procesar.ape_paterno,
                          v_datos_consulta_procesar.ape_materno,
                          v_datos_consulta_procesar.nombre,
                          v_datos_consulta_procesar.aivs_viv92_saci,
                          v_datos_consulta_procesar.aivs_viv97_saci
      -- llama al servicio de procesar para la consulta del saldo

      CALL consultaSaldo(v_url CLIPPED,v_usuario CLIPPED,v_pass CLIPPED,
                         v_datos_consulta_procesar.ape_materno CLIPPED,
                         v_datos_consulta_procesar.ape_paterno CLIPPED,
                         v_datos_consulta_procesar.nombre CLIPPED,v_datos_consulta_procesar.nss)
           RETURNING v_diagnostico,
                     v_resp_serv_saldo.apeMaternoBD, 
                     v_resp_serv_saldo.apePaternoBD,
                     v_resp_serv_saldo.diagProceso,
                     v_resp_serv_saldo.nombresBD,
                     v_resp_serv_saldo.nss,
                     v_resp_serv_saldo.numAIVS92,
                     v_resp_serv_saldo.numAIVS97,
                     v_resp_serv_saldo.origenTipoCredito,
                     v_resp_serv_saldo.resultOperacion,
                     v_resp_serv_saldo.tramiteJudicial
      -- Consulta Saldo en SACI
      DISPLAY "La respuesta de la función para el nss >",v_datos_consulta_procesar.nss,"<"
      DISPLAY "Diagnóstico >",v_diagnostico,"<"
      DISPLAY "Materno          >",v_resp_serv_saldo.apeMaternoBD,"<"
      DISPLAY "Paterno          >",v_resp_serv_saldo.apePaternoBD,"<"
      DISPLAY "Diag Procesar    >",v_resp_serv_saldo.diagProceso,"<"
      DISPLAY "Nombre           >",v_resp_serv_saldo.nombresBD,"<"
      DISPLAY "NSS              >",v_resp_serv_saldo.nss,"<"
      DISPLAY "AIVS VIV 92      >",v_resp_serv_saldo.numAIVS92,"<"
      DISPLAY "AIVS VIV 97      >",v_resp_serv_saldo.numAIVS97,"<"
      DISPLAY "Origen tipo Cred >",v_resp_serv_saldo.origenTipoCredito,"<"
      DISPLAY "Resultado Opera  >",v_resp_serv_saldo.resultOperacion,"<"
      DISPLAY "Trámite Judicial >",v_resp_serv_saldo.tramiteJudicial,"<"
      
      IF v_diagnostico IS NOT NULL AND v_diagnostico >= 0 THEN
         IF v_resp_serv_saldo.numAIVS92 IS NOT NULL THEN 
            LET  v_datos_consulta_procesar.aivs_viv92_procesar = v_resp_serv_saldo.numAIVS92 * 100
         ELSE 
            LET v_datos_consulta_procesar.aivs_viv92_procesar = 0 
         END IF 
         IF v_resp_serv_saldo.numAIVS97 IS NOT NULL THEN 
            LET  v_datos_consulta_procesar.aivs_viv97_procesar = v_resp_serv_saldo.numAIVS97 * 100
         ELSE 
            LET v_datos_consulta_procesar.aivs_viv97_procesar = 0 
         END IF 
         IF v_resp_serv_saldo.origenTipoCredito IS NOT NULL THEN 
            LET v_datos_consulta_procesar.marca_credito = v_resp_serv_saldo.origenTipoCredito
         ELSE 
            LET v_datos_consulta_procesar.marca_credito = 0   
         END IF 
      ELSE 
         LET v_datos_consulta_procesar.aivs_viv92_procesar = NULL 
         LET v_datos_consulta_procesar.aivs_viv97_procesar = NULL 
         LET v_datos_consulta_procesar.marca_credito = NULL 
      END IF 
      IF v_datos_consulta_procesar.aivs_viv92_saci IS NOT NULL THEN 
         LET v_datos_consulta_procesar.aivs_viv92_saci = v_datos_consulta_procesar.aivs_viv92_saci * 100
      ELSE 
         LET v_datos_consulta_procesar.aivs_viv92_saci = 0
      END IF 
      IF v_datos_consulta_procesar.aivs_viv97_saci IS NOT NULL THEN
         LET v_datos_consulta_procesar.aivs_viv97_saci = v_datos_consulta_procesar.aivs_viv97_saci * 100
      ELSE
         LET v_datos_consulta_procesar.aivs_viv97_saci = 0
      END IF 
      LET v_s_detalle_acp = v_datos_consulta_procesar.nss                  , "|",      
                            v_datos_consulta_procesar.marca_credito        , "|",     
                            v_datos_consulta_procesar.marca_operativa      , "|",    
                            v_datos_consulta_procesar.aivs_viv92_procesar  USING "###########", "|",        
                            v_datos_consulta_procesar.aivs_viv97_procesar  USING "###########", "|",        
                            v_datos_consulta_procesar.ape_paterno          , "|",    
                            v_datos_consulta_procesar.ape_materno          , "|", 
                            v_datos_consulta_procesar.nombre               , "|", 
                            v_datos_consulta_procesar.aivs_viv92_saci      USING "##########&", "|",
                            v_datos_consulta_procesar.aivs_viv97_saci      USING "##########&", "|"
                         
      --Se escribe el detalle en el archivo            
      CALL v_ch_arc_aceptados.writeLine(v_s_detalle_acp)                    

      INITIALIZE v_datos_consulta_procesar TO NULL 
      INITIALIZE v_resp_serv_saldo TO NULL 
      LET v_diagnostico = NULL 
      -- se incrementa el indice
      LET v_indice = v_indice + 1
   END FOREACH
   CALL v_ch_arc_aceptados.CLOSE()




   
   --Se finaliza aunque existan errores
   IF ( v_i_resultado = 0 ) THEN
      -- Cierra la operación
      DISPLAY "La integración se terminó completamente."
      DISPLAY "Estatus de integración:",v_i_resultado

      LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                      "Proceso      : Diferencias SSV Pensionados\n",
                      "Operación    : GENERACIÓN ARCHIVO\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n"
            
      -- si se termino correctamente 
      DISPLAY v_mensaje
      DISPLAY "Ya se puede conosultar el archivo:" , v_v_ruta_arc_aceptados

      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Generación concluida con éxito\n.Ya puede consultar el archivo"
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper

      LET p_titulo = "Finalización de operación - DIFERENCIAS SSV PENSIONADO - "
      
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
      
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "PROCESAR")

   ELSE 
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación.\nNo se puede continuar con el proceso de Preliquidación."                                    DISPLAY v_mensaje
      DISPLAY p_mensaje
      DISPLAY "Error (SQL) : ", v_i_resultado
      DISPLAY "Error (ISAM): ", v_error_isam
      DISPLAY "Mensaje     : ", v_mensaje
      DISPLAY "NSS         : ", v_nss
                                             
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado
   END IF

END MAIN