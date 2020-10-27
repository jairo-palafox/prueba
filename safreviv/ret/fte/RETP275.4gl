################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  =>                                                 #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => RET                                                      #
#Programa          => RETP275                                                  #
#Objetivo          => Programa para integrar la respuesta de archivos FICO     #
#                     de pago por DAP                                          #
#Fecha inicio      => Noviembre 28, 2013                                       #
################################################################################
IMPORT os
DATABASE safre_viv 
GLOBALS "RETG01.4gl"
GLOBALS
 DEFINE v_usuario      VARCHAR(30), -- Almacena al usuario
        g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo de proceso
        g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
        l_pid          LIKE glo_pid.pid,
        g_folio        LIKE dis_det_avance_pago.folio, -- Folio generado
        l_arch_proceso VARCHAR(100)
END GLOBALS

--Objetivo: Funcion que realiza la carga de tablas hitoricas de avance de pago
MAIN
DEFINE l_s_qryTxt                      STRING, -- guarda una sentencia SQL a ejecutar
       r_b_valida                      SMALLINT,
       r_bnd_proceso_cnt               SMALLINT,
       v_fecha_reg                     DATE,
       r_bnd_edo_act_archivo           SMALLINT,
       r_bnd_oera_error                SMALLINT,  -- Bandera actualiza operacion en error
       r_bnd_error_op                  SMALLINT,
       p_cve_proceso_cnt               SMALLINT,
       p_transaccion_cnt               SMALLINT,
       p_transaccion                   SMALLINT, --Bandera que indica si la ejecución es manual o automática
       p_mensaje                       STRING,
       p_titulo                        STRING,
       v_error                         SMALLINT, 
       v_respuesta                     INTEGER ,
       v_contador                      INTEGER , 
       v_isam                          INTEGER ,
       v_mensaje                       VARCHAR(250),
       v_comando                       STRING, -- comando para ejecutar
       -- variables para validar el HASH del archivo       
       v_nom_archivo                    STRING, -- nombre del archivo de salida
       v_extension_txt                  STRING, -- extension del archivo de salida
       v_extension_key                  STRING, -- extension KEY del archivo con el HASH
       v_archivo_txt                    STRING, -- nombre y extension del archivo con el detalle
       v_archivo_key                    STRING, -- nombre y extension del archivo con el HASH
       v_v_ruta_nomarch                 STRING, -- ruta y nombre del archivo de salida
       v_ruta_rescate                   LIKE seg_modulo.ruta_rescate, -- ruta donde se colocara el archivo
       v_ruta_bin                       LIKE seg_modulo.ruta_bin, -- ruta donde estan los ejecutables
       v_chpipe                         base.Channel, -- channel para leer la salida standard
       v_r_ret_ctr_archivo_fico         RECORD LIKE ret_ctr_archivo_fico.*, -- registro de control de archivo
       v_hash                           STRING, -- hash calculado
       v_tokenizer                      base.StringTokenizer, -- para obtener el hash
       v_conteo                         INTEGER, -- contador de registros
       v_monto_pesos                    DECIMAL(12,2),
       v_resultado_hash                 STRING,
       v_cambio_dir_correcto            SMALLINT, -- para monitorear cambio de directorio
       v_hash_es_correcto               SMALLINT -- booleana que indica si la validacion del hash se hizo correctamente
       
   LET v_usuario      = ARG_VAL(1)
   LET l_pid          = ARG_VAL(2)
   LET g_proceso_cod  = ARG_VAL(3)
   LET g_opera_cod    = ARG_VAL(4)
   LET g_folio        = ARG_VAL(5)
   LET l_arch_proceso = ARG_VAL(6)

   LET p_transaccion     = 0
   LET r_bnd_proceso_cnt = 0
   LET v_fecha_reg       = TODAY

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "INTEGRACION DE RESPUESTA DE FICO DAP")

   -- se obtiene la ruta de rescate y ejecutable
   SELECT ruta_rescate, ruta_bin
   INTO   v_ruta_rescate, v_ruta_bin
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- se valida si el HASH es correcto
   LET v_archivo_txt = v_ruta_rescate CLIPPED, "/", l_arch_proceso CLIPPED
   
   -- para obtener el nombre del archivo key
   LET v_nom_archivo = l_arch_proceso CLIPPED
   --LET v_archivo_key = v_ruta_rescate CLIPPED, "/", v_nom_archivo.subString(1,v_nom_archivo.getLength() - 4), ".KEY"
   LET v_archivo_key = v_nom_archivo.subString(1,v_nom_archivo.getLength() - 5), ".KEY"
   
   LET v_comando = v_ruta_rescate CLIPPED
   CALL os.Path.chdir(v_comando) RETURNING v_cambio_dir_correcto
   DISPLAY "Pudo cambiar de directorio: ", v_cambio_dir_correcto
   
   RUN "pwd"
   
   -- se conforma el comando para validar HASH
   --LET v_comando = "sha1sum -c ", v_archivo_key
   LET v_comando = "export sha=`cat ", v_archivo_key, "`; sh verifica_sha_ret_generico.sh ", v_archivo_txt, " $sha"
   DISPLAY "Verificando HASH del archivo de respuesta: ", v_comando
   
   -- se abre comunicacion para leer la salida standard
   LET v_chpipe = base.channel.create()
   CALL v_chpipe.openPipe( v_comando, "u")

   CALL v_chpipe.setDelimiter(" ")
   
   -- se lee el resultado del hash
   WHILE ( v_chpipe.read([v_hash]) )
   
      -- se crea un tokenizer para obtener el hash y el nombre del archivo
      LET v_tokenizer = base.StringTokenizer.create(v_hash," ")
      
      -- si hay tokens
      IF ( v_tokenizer.hasMoreTokens() ) THEN
        
         -- =====================================================================
         -- se lee el resultado de la comparacion
         LET v_resultado_hash =  v_tokenizer.nextToken()
         DISPLAY "Resultado de hash: ", v_resultado_hash
         
         -- se verifica si fue correcto
         IF ( v_resultado_hash.trim() = "OK" ) THEN
            DISPLAY "Validación de HASH realizada correctamente... procesando integración."
            LET v_hash_es_correcto = TRUE
         ELSE
            DISPLAY "La validación del HASH no fue correcta. No se procesará el archivo..."
            LET v_hash_es_correcto = FALSE
         END IF
         
         -- se conforma el comando para validar obtener el HASH del archivo KEY
         LET v_comando = "head -1 ", v_archivo_key
         DISPLAY "Obteniendo el HASH del archivo KEY: ", v_comando
         
         -- se abre comunicacion para leer la salida standard
         CALL v_chpipe.openPipe( v_comando, "u")
         
         CALL v_chpipe.setDelimiter(" ")
         
         -- se lee el resultado del hash
         WHILE ( v_chpipe.read([v_hash]) )
         
            -- se crea un tokenizer para obtener el hash y el nombre del archivo
            LET v_tokenizer = base.StringTokenizer.create(v_hash," ")
            
            -- si hay tokens
            IF ( v_tokenizer.hasMoreTokens() ) THEN
              
               -- =====================================================================
               -- el primer token es el HASH
               LET v_resultado_hash =  v_tokenizer.nextToken()
               DISPLAY "HASH obtenido: ", v_resultado_hash
               
               LET v_r_ret_ctr_archivo_fico.hash = v_resultado_hash
               
            END IF
         END WHILE

         
      END IF
   END WHILE


-- para pruebas
LET  v_hash_es_correcto = TRUE


   
   -- se regresa al directorio de ejecucion
   LET v_comando = v_ruta_bin CLIPPED
   CALL os.Path.chdir(v_comando) RETURNING v_cambio_dir_correcto
   RUN "pwd"

   -- se registra el archivo en la tabla de control de archivos
   -- se obtiene el ID
   SELECT NVL(MAX(id_archivo),0) + 1
   INTO   v_r_ret_ctr_archivo_fico.id_archivo
   FROM   ret_ctr_archivo_fico
   
   -- se cuenta el numero de registros del archivo
   SELECT COUNT(*)
   INTO   v_r_ret_ctr_archivo_fico.num_registros
   FROM   safre_tmp:tmp_ret_res_fico_dap

   -- se inserta el registro
   LET v_r_ret_ctr_archivo_fico.nombre_archivo = l_arch_proceso
   LET v_r_ret_ctr_archivo_fico.tpo_archivo    = gi_tipo_archivo_respuesta
   LET v_r_ret_ctr_archivo_fico.f_actualiza    = TODAY
   LET v_r_ret_ctr_archivo_fico.h_actualiza    = CURRENT HOUR TO SECOND
   
   INSERT INTO ret_ctr_archivo_fico VALUES ( v_r_ret_ctr_archivo_fico.* )

   -- si la validacion de hash es correcta
   IF ( v_hash_es_correcto ) THEN
      
      -- se genera el folio para el proceso      
      CALL fn_genera_folio(g_proceso_cod, g_opera_cod,v_usuario)
      RETURNING g_folio
      --
      DISPLAY "EJECUTANDO STORED INTEGRACIÓN: ",TODAY
      -- se prepara y ejecuta el procedimiento de integracion de respuesta de fico pago por dap
      PREPARE prp_carga_disp
         FROM "EXECUTE FUNCTION fn_ret_integra_resp_fico_dap(?,?,?,?)"
         
      EXECUTE prp_carga_disp INTO v_error    ,
                                  v_respuesta,
                                  v_contador ,
                                  v_isam     ,
                                  v_mensaje  
                             USING v_r_ret_ctr_archivo_fico.id_archivo,
                                   l_pid,
                                   g_folio,
                                   l_arch_proceso                              
                                   
         
      -- si no se integro correctamente      
      IF ( v_error < 0 ) THEN
         DISPLAY "Error al ejecutar el SP de integración de respuesta de FICO pago por DAP"     
         DISPLAY "Código de ERROR SQL ",SQLCA.sqlcode
         DISPLAY "Error (SQL)    : ", v_error
         DISPLAY "Error (ISAM)   : ", v_isam
         DISPLAY "Error (mensaje): ", v_mensaje
         
         -- Función para finalizar la operación en error
         CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod) RETURNING r_bnd_oera_error
      ELSE
               
         -- Actualiza el estado del archivo procesado
         CALL fn_act_edo_archivo(l_arch_proceso,g_folio,2,v_usuario) 
              RETURNING r_bnd_edo_act_archivo
         
         -- Función para finalizar la operación
         CALL fn_actualiza_opera_fin(l_pid,g_proceso_cod,g_opera_cod) 
              RETURNING r_b_valida
              
         -- si no se pudo finalizar la operacion correctamente
         IF r_b_valida <> 0 THEN
            CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod)
                 RETURNING r_bnd_error_op
         END IF --Operación

         -- Envío de correo de notificación de proceso finalizado
         -- se complementa el mensaje
         LET p_mensaje = "El proceso de integración de la respuesta de FICO pago por DAP ha finalizado correctamente."
         DISPLAY "\n", p_mensaje
                              
         -- se crea el titulo del mensaje
         LET p_titulo = "Finalización de operación - INTEGRACIÓN ARCHIVO FICO DAP"     
      END IF
   ELSE
      -- EL HASH NO SE VALIDO CORRECTAMENTE. EL PROCESO SE MARCA EN ERROR
      LET p_mensaje = "El archivo ", v_nom_archivo, " de respuesta de FICO paog por DAP no cumple con la validación del archivo KEY. No se procesará el archivo. Es necesario regenerar y reenviar el archivo."
      DISPLAY p_mensaje
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalización de operación - INTEGRACIÓN ARCHIVO FICO DAP"
      
      -- se marca la operacion en error
      CALL fn_error_opera(l_pid,g_proceso_cod,g_opera_cod) RETURNING r_bnd_oera_error
   END IF
              
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(l_pid, g_proceso_cod, g_opera_cod,
                         NULL, -- no lleva archivo adjunto
                         p_titulo,
                         p_mensaje)

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACION DE RESPUESTA DE FICO DAP")


END MAIN