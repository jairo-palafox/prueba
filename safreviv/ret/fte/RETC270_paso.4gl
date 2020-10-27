--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETS270                                                                 #
#OBJETIVO          =>PROGRAMA QUE EJECUTA EL PROCESO DE GENERACION DE ARCHIVO DE SALIDA      #
#                    PARA CREACION DE CUENTAS POR PAGAR A FICO PARA PAGO POR DAP             #
##############################################################################################
IMPORT os
DATABASE safre_viv
GLOBALS "RETG01.4gl"    --Archivo que almacena las variables globales del modulo
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid,           --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod,       -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod,       -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod, -- nombre del usuario
          p_folio                LIKE glo_folio.folio         -- numero de folio         

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS270.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(p_folio, p_usuario_cod)
   
   
END MAIN

{
======================================================================
Clave: 
Nombre: fn_archivo_salida
Fecha creacion: Noviembre 28, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion que se encarga de crear el archivo de salida para FICO
para pago por DAP

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_archivo_salida(p_folio, p_usuario_cod)
DEFINE p_folio        LIKE glo_folio.folio,
       p_usuario_cod  LIKE seg_usuario.usuario_cod,
       v_r_despliegue       RECORD
         v_id_solicitud        LIKE ret_solicitud_generico.id_solicitud    ,
         v_modalidad_retiro    LIKE ret_solicitud_generico.modalidad_retiro,
         v_nss                 LIKE afi_derechohabiente.nss                ,
         v_rfc                 LIKE afi_derechohabiente.rfc                ,
         v_caso_adai           LIKE ret_solicitud_generico.caso_adai       ,
         v_f_solicitud         LIKE ret_disposicion.f_solicitud            ,
         v_aivs                LIKE ret_disposicion.aivs_viv92             ,
         v_pesos               DECIMAL(22,2)                               ,
         v_estado_solicitud    VARCHAR(100)                                ,
         v_cod_rechazo         VARCHAR(100)                                
       END RECORD,          
       
       v_nom_archivo                    STRING, -- nombre del archivo de salida
       v_extension_txt                  STRING, -- extension del archivo de salida
       v_archivo_txt                    STRING, -- nombre y extension del archivo con el detalle
       v_archivo_key                    STRING, -- nombre y extension del archivo con el HASH
       v_c_ruta_env_acr                 LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       cont_cza_solicitud               SMALLINT,      -- Contador de encabezado de solicitudes
       v_comando                        STRING,
       v_bandera                        SMALLINT,
       p_titulo                         STRING,        -- titulo del mensaje enviado en el correo
       p_mensaje                        STRING,        -- cuerpo del mensaje enviado     
       v_s_detalle                      STRING,
       -- VARIABLES PARA CALCULAR EL HASH DEL ARCHIVO GENERADO
       v_chpipe                         base.Channel, -- channel para leer la salida standard
       v_r_ret_ctr_archivo_fico         RECORD LIKE ret_ctr_archivo_fico.*, -- registro de control de archivo
       v_hash                           STRING, -- hash calculado
       v_tokenizer                      base.StringTokenizer, -- para obtener el hash
       v_ruta_bin                       LIKE seg_modulo.ruta_bin, -- ruta donde estan los ejecutables
       v_cambio_directorio              SMALLINT,
       v_ejecuta_sh                     STRING

       v_v_ruta_nomarch                 STRING, -- ruta y nombre del archivo de salida
       v_ch_arch_ret_generico           BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       v_conteo                         INTEGER, -- contador de registros
     
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio, ruta_bin
   INTO   v_c_ruta_env_acr, v_ruta_bin
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son TXT para el detalle y KEY para el hash
   LET v_extension_txt = ".TXT"

   -- los nombres son todo en mayusculas con la siguiente mascara
   -- SG_USUARIO_AAAAMMDD.TXT
   LET v_nom_archivo = "SG_",p_usuario_cod CLIPPED, "_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo, v_extension_txt
   
   -- el archivo con ruta destino que contiene el detalle
   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED , "/", v_archivo_txt
   
   -- nombre de archivo generado
   DISPLAY "~~~~~~~~~~~"
   DISPLAY "Archivo generado: ", v_v_ruta_nomarch

   -- se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter("|")
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )

   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_cza_solicitud = 1

   -- se inicia el contador de registros
   LET v_conteo = 0
   
   FOR v_conteo = 1 TO v_r_despliegue.getLength()
       LET v_s_detalle = v_r_despliegue.v_id_solicitud                 ,
                         v_r_despliegue.v_modalidad_retiro             ,
                         v_r_despliegue.v_nss                          , 
                         v_r_despliegue.v_rfc                          ,
                         v_r_despliegue.v_caso_adai                    ,
                         v_r_despliegue.v_f_solicitud  USING "yyyymmdd",
                         v_r_despliegue.v_aivs  USING "&&&&&&&&&.&&"   ,
                         v_r_despliegue.v_pesos USING "&&&&&&&&&.&&"   ,
                         v_r_despliegue.v_estado_solicitud             ,
                         v_r_despliegue.v_cod_rechazo

       CALL v_ch_arch_ret_generico.write(v_s_detalle)

   END FOR

   -- se cierra el archivo
   CALL v_ch_arch_ret_generico.close()

   -- se cambia el formato de archivo a DOS
   LET v_comando = v_c_ruta_env_acr CLIPPED
   CALL os.Path.chdir(v_comando) RETURNING v_cambio_directorio
   RUN "pwd"

   LET v_comando = "unix2dos . " , v_archivo_txt
   DISPLAY "Codificando el archivo a formato DOS..."
   DISPLAY v_comando
   RUN v_comando
   DISPLAY "Finalizado..."

   -- se crea el SHA1 del archivo
   LET v_comando = "shasum ", v_v_ruta_nomarch, " > ", v_c_ruta_env_acr CLIPPED, "/", v_archivo_key
   DISPLAY "\n\n ==== Generando hash del archivo ==="
   DISPLAY v_comando
   RUN v_comando 
   DISPLAY "Finalizado..."
   
   --Genera copia del archivo
   LET v_comando = "cp ",v_v_ruta_nomarch," ",v_c_ruta_env_acr CLIPPED,"/SACI_DAPS.TXT"
   DISPLAY "\n\n === Generando archivo de transferencia para Axway === "
   DISPLAY v_comando
   RUN v_comando
   DISPLAY "Archivo TXT... Correcto"
   
   -- Genera SHA1 del archivo copia
   LET v_comando = "shasum ", v_c_ruta_env_acr CLIPPED, "/SACI_DAPS.TXT > ", v_c_ruta_env_acr CLIPPED, "/SACI_DAPS.KEY"
   DISPLAY "\n\n ==== Generando hash del archivo SACI_DAPS.TXT ==="
   DISPLAY v_comando
   RUN v_comando 
   DISPLAY "Finalizado..."
   
   LET v_comando = v_ruta_bin CLIPPED
   CALL os.Path.chdir(v_comando) RETURNING v_cambio_directorio
   RUN "pwd"
  
   -- se obtiene el hash y se registra el archivo en la tabla de control
   LET v_chpipe = base.Channel.create()

   -- se conforma el comando que calculara el hash
   LET v_comando = "shasum ", v_v_ruta_nomarch
   
   -- se abre comunicacion para leer la salida standard
   CALL v_chpipe.openPipe( v_comando, "u")

   CALL v_chpipe.setDelimiter(" ")
   
   DISPLAY "Obteniendo HASH para base de datos: ", v_comando
   
   -- se lee el resultado del hash
   WHILE v_chpipe.read([v_hash])
      -- se crea un tokenizer para obtener el hash y el nombre del archivo
      LET v_tokenizer = base.StringTokenizer.create(v_hash," ")
      
      -- si hay tokens
      IF ( v_tokenizer.hasMoreTokens() ) THEN
        
         -- =====================================================================
         -- se actualiza el hash y el numero de registros en el registro de control
         LET v_r_ret_ctr_archivo_fico.hash           = v_tokenizer.nextToken()         
         DISPLAY "Hash calculado: ", v_r_ret_ctr_archivo_fico.hash
         LET v_r_ret_ctr_archivo_fico.num_registros  = v_conteo
         LET v_r_ret_ctr_archivo_fico.f_actualiza    = TODAY
         LET v_r_ret_ctr_archivo_fico.h_actualiza    = CURRENT HOUR TO SECOND
         
         UPDATE ret_ctr_archivo_fico
         SET    hash          = v_r_ret_ctr_archivo_fico.hash         ,
                num_registros = v_r_ret_ctr_archivo_fico.num_registros,
                f_actualiza   = v_r_ret_ctr_archivo_fico.f_actualiza  ,
                h_actualiza   = v_r_ret_ctr_archivo_fico.h_actualiza  
         WHERE  id_archivo    = v_r_ret_ctr_archivo_fico.id_archivo
      END IF
   END WHILE

   DISPLAY "\n\nEjecutando scripts de transferencia de archivos"
   --LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/SACI_TRANSF.sh"
   LET v_ejecuta_sh = "sh /safreviv_int/ret/envio/SACI_DAPS.sh"
   RUN v_ejecuta_sh

   --LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/SACI_TRANSF_KEY.sh"
   LET v_ejecuta_sh = "sh /safreviv_int/ret/envio/SACI_DAPS_KEY.sh"
   RUN v_ejecuta_sh

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "ARCHIVO SAP-FICO PAGO POR DAP")

   -- se finaliza la operacion
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                     RETURNING v_bandera

   -- se complementa el mensaje
   LET p_mensaje = "Generación de archivo de retiro genérico para FICO pago por DAP realizado."
                        
   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - ARCHIVO SAP-FICO PAGO POR DAP"
   
    CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                                  RETURNING v_bandera
               
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   

END FUNCTION 
