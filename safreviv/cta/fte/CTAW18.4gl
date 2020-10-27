#####################################################################
#Modulo            => CTA                                           #
#Programa          => CTAW18                                        #
#Objetivo          => Programa obtiene la lista de cuentas para     #
#                     solicitud de marca en procesar                #
#Fecha inicio      => 06 Junio 2016                                 #
#Modifica:         => Eduardo Ventura Bonola                        #
#Fecha modif:      => 08 DICIEMBRE de 2015                          #
#Adecuación        => Verificación marca Procesar                   #
#####################################################################

DATABASE safre_viv

--19/11/2015
GLOBALS "CTAW15.inc"    #archivo de variables globales del WS de consulta de saldo

#Parámetros generales del proceso

   PRIVATE DEFINE p_usuario_cod              CHAR(20)            -- clave del usuario firmado

#Parámetros de conexión
   PRIVATE DEFINE v_url_servidor             LIKE wsv_cliente.ruta_servidor 
   PRIVATE DEFINE v_usuario                  LIKE wsv_cliente.usuario
   PRIVATE DEFINE v_password                 LIKE wsv_cliente.password
   PRIVATE DEFINE v_intentos                 LIKE wsv_cliente.num_reintento

MAIN

   DEFINE p_archivo     STRING
   DEFINE v_estado      SMALLINT
   DEFINE p_pid         INTEGER
   DEFINE p_proceso_cod SMALLINT
   DEFINE p_opera_cod   SMALLINT
   DEFINE p_usuario     CHAR(40)

   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_archivo        = ARG_VAL(5)

   #Se ejecuta la funcion que obtiene los parámetros de conexión del cliente
   CALL fn_tablas_temporales()
   CALL fn_busca_datos(p_archivo)

   LET v_estado = 1

   IF v_estado = 0 THEN
         DISPLAY ""
         DISPLAY "PROCESO EJECUTADO CORRECTAMENTE..."
         DISPLAY ""
         --DISPLAY v_pid," ",g_proceso_cod," ",g_opera_cod

     CALL fn_actualiza_opera_fin( p_pid,
                                  p_proceso_cod,
                                  p_opera_cod)
                        RETURNING v_estado
      --DISPLAY v_estado
   ELSE
 --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF
   --CALL fn_carga_archivo()

END MAIN


   PRIVATE FUNCTION fn_configura_ws()

   DEFINE v_consulta    STRING
   DEFINE cve_cliente   CHAR(10)

   #La clave 'cre_1' del catálogo de clientes de webServices corresponde a la solicitud de marca
   LET v_consulta = "SELECT   ruta_servidor, 
                              usuario, 
                              password, 
                              num_reintento 
                     FROM     wsv_cliente 
                     WHERE    cve_cliente = ?"

   PREPARE exe_consulta FROM v_consulta

   LET cve_cliente = "cre_3"

   EXECUTE exe_consulta USING cve_cliente INTO v_url_servidor,
                                            v_usuario,
                                            v_password,
                                            v_intentos

END FUNCTION

FUNCTION fn_recupera_marca()

   DEFINE soapStatus            INTEGER
   DEFINE v_qry                 STRING
   DEFINE a                     INTEGER

   DEFINE arr_verifica DYNAMIC ARRAY OF RECORD
          nss CHAR (11),
          id_derechohabiente decimal(9,0),
          paterno CHAR(40),
          materno CHAR(40),
          nombre CHAR(40)
   END RECORD

   DEFINE v_aivs97             DECIMAL(18,6)
   DEFINE v_aivs92             DECIMAL(18,6)
   DEFINE v_marca              SMALLINT
   DEFINE v_cadena             STRING
   DEFINE v_arch_salida        STRING
   DEFINE v_ruta_envio         LIKE seg_modulo.ruta_envio

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "cta"

   LET v_qry = "SELECT afi.nss,afi.id_derechohabiente,afi.ap_paterno_af,afi.ap_materno_af,afi.nombre_af
                  FROM afi_derechohabiente afi, safre_tmp:tmp_nss a
                 WHERE a.nss = afi.nss"

   PREPARE prp_verifica FROM v_qry
   DECLARE cur_verifica CURSOR FOR prp_verifica

   LET a = 1

   FOREACH cur_verifica INTO arr_verifica[a].*
      LET a=a+1
   END FOREACH

   --CALL fn_mensaje ("Transferencia Archivo","punto de control 1","information")

   IF arr_verifica[arr_verifica.getLength()].id_derechohabiente IS NULL THEN
      CALL arr_verifica.deleteElement(arr_verifica.getLength())
   END IF

   FOR a = 1 TO arr_verifica.getLength()
   --CALL fn_mensaje ("Transferencia Archivo","punto de control 2","information")

      -- Se invoca a la función que ejecuta el web service
      CALL consultaSaldo(v_url_servidor CLIPPED,
                         v_usuario,
                         v_password,
                         arr_verifica[a].materno CLIPPED,
                         arr_verifica[a].paterno CLIPPED,
                         arr_verifica[a].nombre CLIPPED,
                         arr_verifica[a].nss)
               RETURNING soapStatus,
                         ConsultaSaldoRespVO.apeMaternoBD,
                         ConsultaSaldoRespVO.apePaternoBD,
                         ConsultaSaldoRespVO.diagProceso,
                         ConsultaSaldoRespVO.nombresBD,
                         ConsultaSaldoRespVO.nss,
                         ConsultaSaldoRespVO.numAIVS92,
                         ConsultaSaldoRespVO.numAIVS97,
                         ConsultaSaldoRespVO.origenTipoCredito,
                         ConsultaSaldoRespVO.resultOperacion,
                         ConsultaSaldoRespVO.tramiteJudicial

            -- Si no hay ningún error, se despliegan los datos obtenidos del WS en la forma
      IF soapStatus = 0 THEN
         LET v_marca  = ConsultaSaldoRespVO.origenTipoCredito
         LET v_aivs92 = ConsultaSaldoRespVO.numAIVS92
         LET v_aivs97 = ConsultaSaldoRespVO.numAIVS97
         --CALL fn_mensaje ("Transferencia Archivo","punto de control 3","information")
         --CALL fn_mensaje ("Transferencia Archivo",arr_verifica[a].id_derechohabiente,"information")
         --CALL fn_mensaje ("Transferencia Archivo",arr_verifica[a].nss,"information")
         --CALL fn_mensaje ("Transferencia Archivo",v_marca,"information")
         INSERT INTO  safre_tmp:tmp_resultados VALUES (arr_verifica[a].nss,
                                                       arr_verifica[a].id_derechohabiente,
                                                       v_marca,
                                                       v_aivs92,
                                                       v_aivs97)
        --CALL fn_mensaje ("Transferencia Archivo",arr_verifica[a].nss,"information")
      END IF
      --CALL fn_mensaje ("Transferencia Archivo","punto de control 4","information")
   END FOR

   LET v_arch_salida = v_ruta_envio CLIPPED,"/marca_procesar.unl"
   LET v_cadena = "Puede veriricar el archivo en \n",v_arch_salida
   UNLOAD TO v_arch_salida SELECT * FROM safre_tmp:tmp_resultados
   CALL fn_mensaje ("Transferencia Archivo",v_cadena,"information")

END FUNCTION
{
FUNCTION fn_carga_archivo()

   DEFINE v_archivo_d              STRING   -- Variable para saber ruta y nombre donde se dejará el archivo
   DEFINE v_archivo                STRING   -- Nombre de archivo seleccionado
   DEFINE v_pos                    INTEGER  -- Posición donde inicia la extensión ".txt"
   DEFINE cant                     INTEGER  -- Cantidad de caracteres que tiene el nombre del archivo
   DEFINE buf                      base.StringBuffer
   DEFINE v_extension              STRING
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   DEFINE v_nss                    CHAR(11)
   DEFINE v_entrada                STRING

   OPEN WINDOW AGRL521 WITH FORM "CTAW181"

   INPUT BY NAME v_archivo ATTRIBUTES (UNBUFFERED)
   AFTER INPUT
   ON ACTION ACCEPT

--Se valida que el archivo tenga nombre y extensión correctos

      IF v_archivo IS NULL THEN
         CALL fn_mensaje ("Archivo","Debe de seleccionar un archivo","information")
         NEXT FIELD v_archivo
      END IF

      IF v_archivo.getIndexOf(" ", 1) THEN
         CALL fn_mensaje ("Transferencia Archivo","El nombre del archivo no debe contener espacios en blanco","information")
         LET v_archivo = ""
         DISPLAY BY NAME v_archivo
         NEXT FIELD v_archivo
      END IF

      IF v_archivo IS NOT NULL THEN
         LET v_entrada = v_archivo
         --CALL fn_mensaje ("Transferencia" ,v_archivo,"information")
         LET buf = base.StringBuffer.create()
         CALL buf.append(v_archivo)

         LET cant         = LENGTH(v_archivo)
         LET v_pos        = buf.getIndexof(".",1)
         LET v_extension  = buf.subString(v_pos,cant)

            {IF (v_extension <> ".cmci") THEN 
            CALL fn_mensaje ("Transferencia Archivo","La extensión del archivo no es correcta,  \n
                               el archivo debe tener extensión .cmci","information")
                           LET v_archivo = ""
               DISPLAY BY NAME v_archivo
               NEXT FIELD v_archivo

            END IF }
            --CALL fn_mensaje ("Transferencia" ,v_nom_archivo,"information")

            --IF  v_extension   = ".cmci" THEN

         LET v_archivo_d = v_archivo
            --DISPLAY "archivo :",v_archivo_d
--**********************************************************************************************************

--***************************************************
--Se  recupera el archivo y se deja en ruta rescate *
--***************************************************

-- Se recuperan las rutas de rescate y envio para el archivo
         SELECT ruta_rescate,
                ruta_envio
           INTO v_ruta_rescate,
                v_ruta_envio
           FROM seg_modulo
          WHERE modulo_cod ="cta"

         LET v_archivo_d = v_ruta_rescate CLIPPED,"/",v_archivo--||v_extension
         TRY
         CALL FGL_GETFILE(v_archivo,v_archivo_d)
         LET v_entrada = v_archivo_d
         CALL fn_busca_datos(v_entrada)
         EXIT INPUT
         CONTINUE INPUT

         CATCH
            ERROR "NO SE PUDO TRANSFERIR"
            CONTINUE INPUT
            END TRY
         EXIT INPUT
      CLOSE WINDOW AGRL521

      END IF
    --END IF
      EXIT INPUT
      CLOSE WINDOW AGRL521
      END INPUT

END FUNCTION}

FUNCTION fn_busca_datos(v_archivo)

   DEFINE s                        CHAR (21)  -- variable para leer líneas del archivo
   DEFINE ch                       base.Channel 
   DEFINE tok                      base.StringTokenizer
   DEFINE buf                      base.StringBuffer
   DEFINE cadena                   CHAR (21)  -- variable para rescatar líneas del archivo
   DEFINE v_ruta_carga             STRING
   DEFINE v_archivo                STRING
   DEFINE i                        INTEGER

   LET v_ruta_carga = v_archivo
   LET ch = base.Channel.create()
   CALL ch.openFile(v_ruta_carga,"r")
   LET buf = base.StringBuffer.create()

   LET i = 1
   WHILE TRUE
      LET s = ch.readLine()
      LET tok = base.StringTokenizer.create(s," ")
      WHILE tok.hasMoreTokens()
         LET cadena = tok.nextToken()

         INSERT INTO safre_tmp:tmp_nss VALUES (cadena)
      END WHILE

      IF ch.isEof() THEN EXIT WHILE
      END IF
         LET i = i + 1
   END WHILE
   CALL ch.close()
            --CALL fn_configura_ws()
            --CALL fn_recupera_marca()
      DISPLAY "EL ARCHIVO FUE CARGADO"

END FUNCTION

FUNCTION fn_tablas_temporales()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_nss
      DROP TABLE tmp_resultados

   WHENEVER ERROR STOP

  --  se crea tabla temporal para guardar registros de cifras control
   CREATE TABLE tmp_nss ( nss CHAR(11))
   CREATE TABLE tmp_resultados ( nss CHAR(11),
                                 id_derechohabiente DECIMAL(9,0),
                                 marca SMALLINT,
                                 aivs92 DECIMAL(12,2),
                                 aivs97 DECIMAL(12,2))
   

DATABASE safre_viv

END FUNCTION