###############################################################################
#Modulo            => AGR                                                     #
#Programa          => AGR16                                                   #
#Objetivo          => Programa que integra consulta de saldos y marcas        #
#                     procesar                                                #
#Autor             => Jose Eduardo Ventura                                    #
#Fecha inicio      => 08 Enero 2016                                           #
###############################################################################
DATABASE safre_viv 

GLOBALS "../../cta/fte/CTAW15.inc"    #archivo de variables globales del WS de consulta de saldo

   DEFINE p_usuario           CHAR(20)
   DEFINE p_tipo_ejecucion    SMALLINT                      -- forma como ejecutara el programa
   DEFINE p_s_titulo          STRING                        -- título de la ventana
   DEFINE p_pid               DECIMAL (9,0)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE p_folio             LIKE glo_folio.folio                                         
   DEFINE p_nom_archivo       LIKE glo_ctr_archivo.nombre_archivo
   DEFINE r_b_valida          SMALLINT

   #Parámetros generales del proceso

   PRIVATE DEFINE p_usuario_cod              CHAR(20)            -- clave del usuario firmado

#Parámetros de conexión
   PRIVATE DEFINE v_url_servidor             LIKE wsv_cliente.ruta_servidor 
   PRIVATE DEFINE v_usuario                  LIKE wsv_cliente.usuario
   PRIVATE DEFINE v_password                 LIKE wsv_cliente.password
   PRIVATE DEFINE v_intentos                 LIKE wsv_cliente.num_reintento

MAIN

   DEFINE v_qry               STRING

   -- se recupera la clave de usuario desde parametro
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nom_archivo    = ARG_VAL(6)

   DISPLAY "usuario   : ", p_usuario
   DISPLAY "tipo ejec : ", p_tipo_ejecucion
   DISPLAY "titulo    : ",p_s_titulo

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGE11.log")

   --CALL FGL_SETENV("FGLLDPATH","$FGLLDPATH:$SAFREDIR/cta/bin")

   CALL fn_tablas_temporales()
   CALL fn_configura_ws()
   CALL fn_recupera_marca()

    -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
   ELSE
      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_b_valida
   END IF
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
   DEFINE v_paterno             CHAR(40)
   DEFINE v_materno             CHAR(40)
   DEFINE v_nombre              CHAR(40)
   DEFINE v_marca_inf           SMALLINT
   DEFINE v_marca_prcr          SMALLINT
   DEFINE v_cta                 INTEGER
   DEFINE v_cta_ent             INTEGER

   DEFINE arr_verifica DYNAMIC ARRAY OF RECORD
          nss CHAR (11),
          id_derechohabiente decimal(9,0),
          paterno CHAR(40),
          materno CHAR(40),
          nombre CHAR(40)
   END RECORD

   DEFINE v_aivs97             DECIMAL(18,2)
   DEFINE v_aivs92             DECIMAL(18,2)
   DEFINE v_marca              SMALLINT
   DEFINE v_cadena             STRING
   DEFINE v_arch_salida        STRING
   DEFINE v_ruta_envio         LIKE seg_modulo.ruta_envio

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "agr"

   LET v_qry = "SELECT afi.nss,
                       afi.id_derechohabiente,
                       afi.ap_paterno_af,
                       afi.ap_materno_af,
                       afi.nombre_af
                  FROM afi_derechohabiente afi, safre_tmp:tmp_cons_mca_prcr a
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

   LET v_cta_ent =  arr_verifica.getLength()
   LET v_cta = 0

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
      --DISPLAY soapStatus
      IF soapStatus = 0 THEN
         LET v_marca  = ConsultaSaldoRespVO.origenTipoCredito
         LET v_aivs92 = ConsultaSaldoRespVO.numAIVS92
         LET v_aivs97 = ConsultaSaldoRespVO.numAIVS97
         LET v_paterno= ConsultaSaldoRespVO.apeMaternoBD CLIPPED
         LET v_materno= ConsultaSaldoRespVO.apePaternoBD CLIPPED
         LET v_nombre = ConsultaSaldoRespVO.nombresBD    CLIPPED

         --FOR a = 1 TO arr_verifica.getLength()

            LET v_qry = "SELECT FIRST 1 marca
                           FROM sfr_marca_activa
                          WHERE id_derechohabiente = ?
                            AND marca > 230"

             PREPARE prp_mca_prcr FROM v_qry
             EXECUTE prp_mca_prcr USING arr_verifica[a].id_derechohabiente
                                   INTO v_marca_prcr


            LET v_qry = "SELECT FIRST 1 marca
                           FROM sfr_marca_activa
                          WHERE id_derechohabiente = ?
                            AND marca < 230"

            PREPARE prp_mca_inf FROM v_qry
            EXECUTE prp_mca_inf USING arr_verifica[a].id_derechohabiente
                                   INTO v_marca_inf

         INSERT INTO safre_tmp:tmp_cmp VALUES (arr_verifica[a].nss,
                                                v_marca  ,
                                                v_marca_prcr,
                                                v_marca_inf ,
                                                v_aivs92    ,
                                                v_aivs97    ,
                                                v_paterno   ,
                                                v_materno   ,
                                                v_nombre   )
         LET v_cta = v_cta + 1
         --END FOR
      END IF
   END FOR

   DISPLAY "Total de registros a procesar           : ",v_cta_ent
   DISPLAY "Total de registros en archivo de salida : ",v_cta
   DISPLAY ""

   LET v_arch_salida = v_ruta_envio CLIPPED,"/",TODAY USING "yyyymmdd","consulta.cmp"
   LET v_cadena = "Puede verificar el archivo en \n",v_arch_salida
   UNLOAD TO v_arch_salida SELECT * FROM safre_tmp:tmp_cmp

   DISPLAY v_cadena
   DISPLAY ""
   DISPLAY "FIN"
   --CALL fn_mensaje ("Transferencia Archivo",v_cadena,"information")

END FUNCTION

FUNCTION fn_tablas_temporales()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_cmp

   WHENEVER ERROR STOP

  --  se crea tabla temporal para guardar registros de cifras control
   CREATE TABLE tmp_cmp ( nss           CHAR(11),
                          marca_ws      SMALLINT,
                          marca_prcr    SMALLINT,
                          marca_inf     SMALLINT,
                          aivs92        DECIMAL(9,2),
                          aivs97        DECIMAL(9,2),
                          paterno       CHAR(40),
                          materno       CHAR(40),
                          nombre        CHAR(40))
DATABASE safre_viv

END FUNCTION