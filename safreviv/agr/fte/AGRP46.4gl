##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRP46                                             #
#Objetivo          => Solicitud de Marca y Desmarca a PROCESAR en línea  #
#                     por registro o carga masiva (archivo).             #
#Autor             => Emilio Abarca, EFP                                 #
#Fecha inicio      => 05/Diciembre/2017                                  #
##########################################################################

DATABASE safre_viv

GLOBALS "../../cta/fte/CTAW12.inc" --Variables globales del WS de Marcas
GLOBALS 
   DEFINE g_usuario         CHAR(20) 
   DEFINE g_titulo          STRING    
   DEFINE g_tipo_ejecucion  SMALLINT
   DEFINE v_query           STRING
   DEFINE v_qury_acre       STRING 
   --Parámetros de conexión WS Solicitud de Marca PROCESAR
   DEFINE g_url_servidor    LIKE wsv_cliente.ruta_servidor 
   DEFINE g_usuario_ws      LIKE wsv_cliente.usuario
   DEFINE g_password        LIKE wsv_cliente.password
   DEFINE g_intentos        LIKE wsv_cliente.num_reintento
   --Parámetros de conexión WS Solicitud Desmarca PROCESAR
   DEFINE g_url_servidor_d  LIKE wsv_cliente.ruta_servidor 
   DEFINE g_usuario_ws_d    LIKE wsv_cliente.usuario
   DEFINE g_password_d      LIKE wsv_cliente.password
   DEFINE g_intentos_d      LIKE wsv_cliente.num_reintento
   --variables para consumir WS
   DEFINE v_marca_ws        CHAR(2)
   DEFINE v_marca_modulo    CHAR(3)
   DEFINE v_proceso_marca   CHAR(4)
   DEFINE v_id_cre_acre     DECIMAL(9,0)
   DEFINE g_solicita_m      tSolicMarcaVO
   DEFINE v_respuesta_m     tSolicMarcaRespVO
   DEFINE g_solicita_d      tSolicDesmarcaVO
   DEFINE v_respuesta_d     tSolicDesmarcaRespVO
   DEFINE v_cod_result_op   SMALLINT 
   DEFINE v_diagnostico     CHAR(3)
   DEFINE v_marca_activa    SMALLINT
   DEFINE v_fn_cuenta       STRING
   DEFINE v_estado_marca    SMALLINT
   DEFINE v_cod_rechazo     SMALLINT
   DEFINE v_result_marca    SMALLINT
   DEFINE v_marca_causa     SMALLINT 

   DEFINE r_inf_dh RECORD
      id_derechohabiente  DECIMAL(9,0),
      nss        CHAR(11),
      ap_paterno CHAR(40),
      ap_materno CHAR(40),
      nombre     CHAR(50),
      rfc        CHAR(13)
   END RECORD 

   DEFINE r_inf_acre RECORD
      f_otorga      DATE,
      folio_archivo DECIMAL(9,0),
      marca_prc     SMALLINT
   END RECORD
   DEFINE v_comando       STRING  

END GLOBALS 

MAIN 

   LET g_usuario        = ARG_VAL  (1)
   LET g_tipo_ejecucion = ARG_VAL  (2)
   LET g_titulo         = ARG_VAL  (3)

   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP46.log")
   
   --CLOSE WINDOW SCREEN 
   
   -- Recupera información del WS Marca PROCESAR
   SELECT ruta_servidor,usuario, password,num_reintento 
      INTO g_url_servidor,g_usuario_ws,g_password,g_intentos
      FROM wsv_cliente 
     WHERE cve_cliente = "cre_1";

   -- Recupera información del WS Desmarca PROCESAR
   SELECT ruta_servidor,usuario, password,num_reintento 
      INTO g_url_servidor_d,g_usuario_ws_d,g_password_d,g_intentos_d
      FROM wsv_cliente 
     WHERE cve_cliente = "cre_2";

   CALL inicializa_consultas()
   
   MENU ""
      ON ACTION Individual
         CALL sol_individual()
         
      ON ACTION Archivo
         CALL sol_archivo()

      ON ACTION CANCEL 
         EXIT MENU
         
   END MENU
   
END MAIN

FUNCTION inicializa_consultas()

   --Incializa consultas
   LET v_query = "SELECT id_derechohabiente,
                          nss,
                          ap_paterno_af,
                          ap_materno_af,
                          nombre_af,
                          rfc
                     FROM afi_derechohabiente 
                    WHERE nss = ?" 

   LET v_qury_acre = "SELECT c.f_otorga,
                             a.folio_archivo,
                             t.marca_prc
                        FROM cre_acreditado c,
                             cre_ctr_archivo a,
                             cat_tipo_credito t
                       WHERE c.id_cre_acreditado = ?
                         AND c.id_cre_ctr_archivo = a.id_cre_ctr_archivo
                         AND c.tpo_originacion    = t.tpo_originacion
                         AND c.tpo_credito        = t.tpo_credito"
END FUNCTION 

FUNCTION sol_individual()

   DEFINE v_nss             CHAR(11)
   DEFINE v_num_credito     CHAR(10)
   DEFINE v_tpo_credito     SMALLINT 
   DEFINE v_solicitud       CHAR(12)
   DEFINE v_marca           SMALLINT 
   DEFINE bnd_nss           SMALLINT
   DEFINE bnd_credito       SMALLINT 
   DEFINE v_tot_afi         SMALLINT
   DEFINE v_tot_cre         SMALLINT
   
   OPEN WINDOW vtn_individual WITH FORM "AGRP461"

      -- Inicializa valores
      INITIALIZE g_solicita_m.* TO NULL 
      INITIALIZE v_respuesta_m.* TO NULL 
      INITIALIZE g_solicita_d.* TO NULL
      INITIALIZE v_respuesta_d.* TO NULL  
      
      LET v_nss           = NULL 
      LET v_num_credito   = NULL 
      LET v_tpo_credito   = NULL
      LET v_id_cre_acre   = NULL
      LET v_marca         = NULL 
      LET v_marca_ws      = NULL
      LET v_marca_modulo  = NULL
      LET v_marca_activa  = 0 
      LET v_proceso_marca = NULL
      LET v_result_marca  = 0
      LET v_estado_marca  = 0 
      LET v_cod_rechazo   = 0
      LET v_marca_causa   = 0
      
      INPUT BY NAME v_nss,
                    v_num_credito,
                    v_tpo_credito,
                    v_solicitud,
                    v_marca ATTRIBUTE(UNBUFFERED,WITHOUT DEFAULTS)

         #NOTA : v_marca sólo se debe ingresar(1 o 2 o 4) que es el tipo de originación
         -------1 es "03" TA
         -------2 es "16" 43BIS
         -------4 es "43" AG
         BEFORE INPUT 
         CALL combobox_solicitud() 
      
         ON ACTION ACCEPT
            LET v_diagnostico   = NULL 
            LET v_cod_result_op = NULL 

            #validaciones
            IF(v_nss IS NULL) OR 
              (v_num_credito IS NULL) OR    
              (v_tpo_credito IS NULL) OR  
              (v_solicitud IS NULL) OR 
              (v_marca IS NULL) THEN
               CALL fn_mensaje("","Ingresa todos los parámetros de captura","")
               CONTINUE INPUT 
            END IF
            
            #validaciones NSS
            IF(LENGTH(v_nss) < 11) THEN
               CALL fn_mensaje("","El NSS debe ser de 11 dígitos","")
               NEXT FIELD v_nss
            END IF 
            
            LET bnd_nss = fn_numerico(v_nss)
            IF(bnd_nss = 1) THEN
               CALL fn_mensaje("","El NSS debe ser numérico","") 
               NEXT FIELD v_nss
            END IF
          
            SELECT COUNT(*)
              INTO v_tot_afi
              FROM afi_derechohabiente
             WHERE nss = v_nss;
             
            IF(v_tot_afi = 0) THEN
               CALL fn_mensaje("","El NSS no existe en la base de derechohabientes","")
               NEXT FIELD v_nss
            END IF  

            --validaciones Número de crédito
            IF(LENGTH(v_num_credito) < 9) THEN
               CALL fn_mensaje("","El número de crédito debe ser de 9 o 10 dígitos","")
               NEXT FIELD v_num_credito
            ELSE
               LET bnd_credito = fn_numerico(v_num_credito)
               IF(bnd_credito = 1) THEN
                  CALL fn_mensaje("","El número de crédito debe ser numérico","")
                  NEXT FIELD v_num_credito
               END IF 
            END IF 

            --validaciones tipo de crédito
            IF(v_tpo_credito = 0) THEN
               CALL fn_mensaje("","El tipo de crédito debe ser mayor a cero","")
               NEXT FIELD v_tpo_credito
            ELSE 
               --consulta existencia en el catálogo de créditos
               SELECT COUNT(*)
                 INTO v_tot_cre
                 FROM cat_tipo_credito
                WHERE tpo_credito = v_tpo_credito

               IF(v_tot_cre = 0) THEN
                  CALL fn_mensaje("","El tipo de crédito ingresado no existe","")
                  NEXT FIELD v_tpo_credito
               END IF  
            END IF 

            --valida marca
            IF(v_marca <> 1) AND 
              (v_marca <> 2) AND 
              (v_marca <> 4) THEN
               CALL fn_mensaje("","La marca debe ser 1 o 2 o 4","")
               NEXT FIELD v_marca
            END IF 

            CASE
               WHEN v_marca = 1
                  LET v_marca_ws      = "01"
                  LET v_marca_modulo  = "03"
                  LET v_proceso_marca = "201"
               WHEN v_marca = 2
                  LET v_marca_ws      = "02"
                  LET v_marca_modulo  = "16"
                  LET v_proceso_marca = "1201"
               WHEN v_marca = 4
                  LET v_marca_ws      = "04"
                  LET v_marca_modulo  = "43" 
                  LET v_proceso_marca = "301"
            END CASE 

            --> PROCESA INFORMACIÓN AL PASAR LAS VALIDACIONES <--
            INITIALIZE r_inf_dh.* TO NULL 
            PREPARE prp_afi FROM v_query 
            EXECUTE prp_afi USING v_nss INTO r_inf_dh.id_derechohabiente,
                                             r_inf_dh.nss,
                                             r_inf_dh.ap_paterno,
                                             r_inf_dh.ap_materno,
                                             r_inf_dh.nombre,
                                             r_inf_dh.rfc

            --Recupera el acreditado más reciente
            SELECT MAX(id_cre_acreditado)
               INTO v_id_cre_acre
               FROM cre_acreditado
              WHERE id_derechohabiente = r_inf_dh.id_derechohabiente
                AND num_credito     = v_num_credito
                AND tpo_originacion = v_marca
                AND tpo_credito     = v_tpo_credito;
            
            IF(v_id_cre_acre IS NULL) THEN
               CALL fn_mensaje("","El registro ingresado no es un acreditado","") 
               CONTINUE INPUT 
            ELSE 
               --Recupera información del acreditado
               INITIALIZE r_inf_acre.* TO NULL
               PREPARE prp_acre FROM v_qury_acre
               EXECUTE prp_acre USING v_id_cre_acre INTO r_inf_acre.f_otorga,
                                                         r_inf_acre.folio_archivo,
                                                         r_inf_acre.marca_prc

               --WS Solicitud de MARCA
               IF(v_solicitud = 2) THEN
               
                  --Asigna valores que se envían al WS de Marca
                  LET g_solicita_m.apeMaterno          = r_inf_dh.ap_materno CLIPPED
                  LET g_solicita_m.apePaterno          = r_inf_dh.ap_paterno CLIPPED
                  LET g_solicita_m.fechaPresentacion   = TODAY USING "yyyymmdd"
                  LET g_solicita_m.nombres             = r_inf_dh.nombre CLIPPED
                  LET g_solicita_m.nss                 = r_inf_dh.nss  CLIPPED
                  LET g_solicita_m.numCreditoInfonavit = v_num_credito CLIPPED
                  LET g_solicita_m.rfc                 = r_inf_dh.rfc  CLIPPED
                  LET g_solicita_m.sitCredito          = v_solicitud CLIPPED
                  LET g_solicita_m.tipoCredito         = v_marca_ws  CLIPPED
                  
                  --Llama la función que invoca el ws
                  CALL fn_solicita_marca( g_url_servidor CLIPPED,
                                          g_usuario_ws   CLIPPED,
                                          g_password     CLIPPED,
                                          g_intentos,
                                          g_solicita_m.*) 
                               RETURNING v_respuesta_m.*

                  LET v_diagnostico   = v_respuesta_m.diagProceso
                  LET v_cod_result_op = v_respuesta_m.resultOperacion

                  -----------> Prueba ejcuta WS Solicitud Marca Procesar <-------------
                  {
                  LET v_comando = "apeMaterno          = ",g_solicita_m.apeMaterno         ,"\n",
                                  "apePaterno          = ",g_solicita_m.apePaterno         ,"\n",
                                  "fechaPresentacion   = ",g_solicita_m.fechaPresentacion  ,"\n",
                                  "nombres             = ",g_solicita_m.nombres            ,"\n", 
                                  "nss                 = ",g_solicita_m.nss                ,"\n",
                                  "numCreditoInfonavit = ",g_solicita_m.numCreditoInfonavit,"\n",
                                  "rfc                 = ",g_solicita_m.rfc                ,"\n",
                                  "sitCredito          = ",g_solicita_m.sitCredito         ,"\n",
                                  "tipoCredito         = ",g_solicita_m.tipoCredito        ,"\n",
                                  "Diagnostico WS      = ",v_diagnostico                   ,"\n",
                                  "Cod.ResultOp        = ",v_cod_result_op

                  CALL fn_mensaje("",v_comando,"")
                  }
                  --Guarda en histórico
                  INSERT INTO cta_his_marca_ws(
                                  id_derechohabiente,
                                  id_origen ,
                                  modulo_cod,
                                  tpo_credito,
                                  marca,
                                  num_credito,
                                  f_infonavit,
                                  marca_procesar,
                                  situacion,
                                  f_solicita,
                                  intento,
                                  cod_result_op,
                                  diagnostico,
                                  f_actualiza, 
                                  usuario,
                                  folio_archivo,
                                  usr_marca)
                         VALUES (r_inf_dh.id_derechohabiente,
                                  v_id_cre_acre,
                                  v_marca_modulo,
                                  v_tpo_credito,
                                  r_inf_acre.marca_prc,
                                  v_num_credito,
                                  r_inf_acre.f_otorga,
                                  v_marca_ws,
                                  v_solicitud,
                                  TODAY,
                                  1,
                                  v_cod_result_op,
                                  v_diagnostico,
                                  TODAY,
                                  g_usuario,
                                  r_inf_acre.folio_archivo,
                                  g_usuario);

                  -- Diagnóstico aceptado
                  IF(v_diagnostico = "   ") OR (v_diagnostico = "") THEN
                     
                     --verifica si existe la marca
                     SELECT COUNT(*)
                       INTO v_marca_activa
                       FROM sfr_marca_activa
                      WHERE id_derechohabiente = r_inf_dh.id_derechohabiente
                        AND marca =  r_inf_acre.marca_prc;
                     
                     IF(v_marca_activa = 0) THEN
                        LET v_fn_cuenta = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
                        PREPARE prp_marca_cuenta FROM v_fn_cuenta
                        EXECUTE prp_marca_cuenta USING r_inf_dh.id_derechohabiente,
                                                       r_inf_acre.marca_prc,
                                                       v_id_cre_acre,
                                                       r_inf_acre.folio_archivo,
                                                       v_estado_marca, 
                                                       v_cod_rechazo,
                                                       "",
                                                       "",
                                                       g_usuario,
                                                       v_proceso_marca
                                                  INTO v_result_marca
                     END IF 
                  END IF 
                  CALL fn_mensaje("","La solicitud de marca a Procesar se ha realizado correctamente","")
                  EXIT INPUT 
               ELSE 
                  --Asigna valores para consulta WS Desmarca PROCESAR
                  LET g_solicita_d.apeMaterno = r_inf_dh.ap_materno CLIPPED
                  LET g_solicita_d.apePaterno = r_inf_dh.ap_paterno CLIPPED
                  LET g_solicita_d.fechaPresentacion = TODAY USING "yyyymmdd"
                  LET g_solicita_d.nombres    = r_inf_dh.nombre CLIPPED
                  LET g_solicita_d.nss        = r_inf_dh.nss CLIPPED
                  LET g_solicita_d.numCreditoInfonavit = v_num_credito CLIPPED
                  LET g_solicita_d.rfc        = r_inf_dh.rfc CLIPPED
                  
                  --Llama la función que invoca el WS de Desmarca
                  CALL fn_solicita_desmarca( g_url_servidor_d CLIPPED,
                                          g_usuario_ws_d   CLIPPED,
                                          g_password_d     CLIPPED,
                                          g_intentos_d,
                                          g_solicita_d.*) 
                               RETURNING v_respuesta_d.*

                  LET v_diagnostico   = v_respuesta_d.diagProceso
                  LET v_cod_result_op = v_respuesta_d.resultOperacion

                  ----> Prueba ejecución WS Desmarca Procesar
                  {LET v_comando = "apeMaterno          = ",g_solicita_d.apeMaterno,"\n",              
                                  "apePaterno          = ",g_solicita_d.apePaterno,"\n",              
                                  "fechaPresentacion   = ",g_solicita_d.fechaPresentacion,"\n",       
                                  "nombres             = ",g_solicita_d.nombres,"\n",                 
                                  "nss                 = ",g_solicita_d.nss,"\n",                     
                                  "numCreditoInfonavit = ",g_solicita_d.numCreditoInfonavit,"\n",     
                                  "rfc                 = ",g_solicita_d.rfc,"\n",                     
                                  "Diagnostico WS      = ",v_diagnostico,"\n",                        
                                  "Cod.ResultOp        = ",v_cod_result_op                            

                  CALL fn_mensaje("",v_comando,"")
                  }
                  --Guarda en histórico
                  INSERT INTO cta_his_marca_ws(
                                  id_derechohabiente,
                                  id_origen ,
                                  modulo_cod,
                                  tpo_credito,
                                  marca,
                                  num_credito,
                                  f_infonavit,
                                  marca_procesar,
                                  situacion,
                                  f_solicita,
                                  intento,
                                  cod_result_op,
                                  diagnostico,
                                  f_actualiza, 
                                  usuario,
                                  folio_archivo,
                                  usr_marca)
                         VALUES (r_inf_dh.id_derechohabiente,
                                  v_id_cre_acre,
                                  v_marca_modulo,
                                  v_tpo_credito,
                                  r_inf_acre.marca_prc,
                                  v_num_credito,
                                  r_inf_acre.f_otorga,
                                  v_marca_ws,
                                  v_solicitud,
                                  TODAY,
                                  1,
                                  v_cod_result_op,
                                  v_diagnostico,
                                  TODAY,
                                  g_usuario,
                                  r_inf_acre.folio_archivo,
                                  g_usuario);

                  -- Diagnóstico aceptado
                  IF(v_diagnostico = "   ") OR (v_diagnostico = "") THEN
                     --verifica si existe la marca
                     SELECT COUNT(*)
                       INTO v_marca_activa
                       FROM sfr_marca_activa
                      WHERE id_derechohabiente = r_inf_dh.id_derechohabiente
                        AND marca =  r_inf_acre.marca_prc;
                     
                     IF(v_marca_activa >= 1) THEN
                        LET v_fn_cuenta = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
                        PREPARE prp_desm_cuenta FROM v_fn_cuenta
                        EXECUTE prp_desm_cuenta USING r_inf_dh.id_derechohabiente,
                                                      r_inf_acre.marca_prc,
                                                      v_id_cre_acre,
                                                      v_estado_marca, 
                                                      v_marca_causa,
                                                      g_usuario,
                                                      v_proceso_marca
                                                 INTO v_result_marca
                     END IF 
                  END IF
                  CALL fn_mensaje("","La solicitud de desmarca a Procesar se ha realizado correctamente","")
                  EXIT INPUT 
               END IF 
            END IF 
         ON ACTION CANCEL 
            EXIT INPUT
            
      END INPUT 
   CLOSE WINDOW vtn_individual
END FUNCTION  

FUNCTION sol_archivo()

   DEFINE v_archivo      STRING
   DEFINE buf            base.StringBuffer
   DEFINE v_long_arh     INTEGER
   DEFINE v_pos_ext      SMALLINT
   DEFINE v_extension    STRING 
   DEFINE v_pos_arh      SMALLINT
   DEFINE v_ruta_archivo STRING 
   DEFINE v_ruta_rescate CHAR(40)
   DEFINE bnd_transfer   SMALLINT
   DEFINE v_ind_arh      SMALLINT  

   SELECT ruta_rescate
     INTO v_ruta_rescate
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   LET bnd_transfer = 0
   LET v_ind_arh    = 0

   OPEN WINDOW vtn_archivo WITH FORM "AGRP462"
      INPUT BY NAME v_archivo ATTRIBUTE(UNBUFFERED,WITHOUT DEFAULTS)
      
         ON ACTION ACCEPT 
            IF(v_archivo IS NULL) THEN 
                CALL fn_mensaje("","Selecciona el archivo a cargar","about")
               NEXT FIELD v_archivo
            END IF
            
             -- Verifica si hay un espacio en el nombre del archivo
            IF (v_archivo.getIndexOf(" ",1)) THEN
               CALL fn_mensaje("","El archivo no debe contener espacios","")
               LET v_archivo = NULL 
               NEXT FIELD v_archivo 
            END IF

            IF(v_archivo IS NOT NULL) THEN
               -- valida extensión del archivo
               LET buf = base.StringBuffer.create()   # Se crea objeto StringBuffer 
               CALL buf.append(v_archivo)

               -- Longitud del archivo
               LET v_long_arh = LENGTH(v_archivo)

                -- Posición donde se encuentra el '.' del archivo
               LET v_pos_ext = buf.getIndexOf(".",1)
               LET v_pos_ext   = v_pos_ext + 1         # Incrementamos 1 para obtener solo el nombre de la extensión
               LET v_extension = buf.subString(v_pos_ext,v_long_arh) # Obtiene nombre de la extensión
               LET v_pos_arh   = buf.getIndexOf("C:",1)

               -- Obtiene sólo el nombre del archivo, sin la ruta
               IF(v_pos_arh >= 1) THEN
                  LET v_archivo = buf.subString(13,v_long_arh) 
               END IF 

               IF(v_extension <> "txt") THEN
                  CALL fn_mensaje ("","La extensión del archivo no es correcta.\nEl archivo debe tener extensión txt","")
                  LET v_archivo = NULL 
                  NEXT FIELD v_archivo
               ELSE
                  --*************************************************************************
                  --Se recupera el archivo cargado y se deja en la ruta rescate del servidor
                  --para trabajarlo desde ahí.
                  --*************************************************************************
                  LET v_ruta_archivo = v_ruta_rescate CLIPPED,"/",v_archivo
                  
                  TRY 
                     CALL FGL_GETFILE(v_archivo,v_ruta_archivo)
                     LET bnd_transfer = 1 --Archivo transferido al server correctamente
                  CATCH
                     LET bnd_transfer = 0 
                     CALL fn_mensaje("","No se pudo realizar la carga del archivo","")
                     CONTINUE INPUT  
                  END TRY 
               END IF

               IF(bnd_transfer = 1) THEN
                  CALL procesa_informacion(v_ruta_archivo) RETURNING v_ind_arh
               END IF

               IF(v_ind_arh = 1) THEN
                  CALL fn_mensaje("","Se han procesado las solicitudes a Procesar correctamente","")
                  EXIT INPUT 
               ELSE 
                  CALL fn_mensaje("","No existe información para la solicitud de marca/desmarca","")
                  EXIT INPUT 
               END IF   
            END IF 
               
         ON ACTION CANCEL 
            EXIT INPUT 
      END INPUT 
   CLOSE WINDOW vtn_archivo 
END FUNCTION 

FUNCTION procesa_informacion(p_ruta_archivo)

   DEFINE p_ruta_archivo  STRING 
   DEFINE ch              base.channel
   DEFINE a_nss           CHAR(11)
   DEFINE a_num_credito   DECIMAL(10,0)
   DEFINE a_tpo_credito   SMALLINT
   DEFINE a_solicitud     SMALLINT 
   DEFINE a_marca_orig    SMALLINT
   DEFINE v_linea         CHAR(26)
   DEFINE v_tot_tmp       INTEGER 
   DEFINE contador        INTEGER
   DEFINE bnd_archivo     SMALLINT 

   DEFINE r_sol_archivo RECORD
      nss         CHAR(11),
      num_credito DECIMAL(10,0),
      tpo_credito SMALLINT,
      solicitud   SMALLINT,
      marca_orig  SMALLINT 
   END RECORD  
   
   CALL crea_temporal()
   
   LET a_nss         = NULL 
   LET a_num_credito = NULL 
   LET a_tpo_credito = NULL
   LET a_solicitud   = NULL 
   LET a_marca_orig  = NULL 
   LET v_linea       = NULL
   LET v_tot_tmp     = 0
   LET bnd_archivo   = 0 
   
   LET ch = base.Channel.create() # Creamos un objeto de la clase channel
   CALL ch.openFile(p_ruta_archivo,"r")

   WHILE TRUE

      LET v_linea = ch.readLine()

      IF(ch.isEof()) THEN
         EXIT WHILE
         DISPLAY "no encuentra registros"
      ELSE 
         -- Recupera información por linea
         LET a_nss         = v_linea[1,11]
         LET a_num_credito = v_linea[12,21]
         LET a_tpo_credito = v_linea[22,24]
         LET a_solicitud   = v_linea[25]
         LET a_marca_orig  = v_linea[26]

         INSERT INTO safre_tmp:tmp_solic_procesar
            VALUES (a_nss,a_num_credito,a_tpo_credito,a_solicitud,a_marca_orig);
      END IF 
      
   END WHILE 

   CALL ch.close()

   --> PROCESA INFORMACIÓN DE LA TEMPORAL <--
   SELECT COUNT(*)
     INTO v_tot_tmp
     FROM safre_tmp:tmp_solic_procesar;

   IF(v_tot_tmp = 0) THEN
      LET bnd_archivo = 0 --Mantiene la bandera apagada
   ELSE
      LET bnd_archivo = 1 --Enciende bandera

      INITIALIZE g_solicita_m.* TO NULL 
      INITIALIZE v_respuesta_m.* TO NULL 
      INITIALIZE g_solicita_d.* TO NULL
      INITIALIZE v_respuesta_d.* TO NULL
      INITIALIZE r_sol_archivo.* TO NULL
      INITIALIZE r_inf_dh.* TO NULL
      INITIALIZE r_inf_acre.* TO NULL 

      DECLARE crs_inf_archivo CURSOR FOR 
      SELECT nss, 
             num_credito,
             tpo_credito,
             solicitud,
             marca_orig
        FROM safre_tmp:tmp_solic_procesar

      LET v_marca_ws      = NULL
      LET v_marca_modulo  = NULL
      LET v_proceso_marca = NULL
      LET v_marca_activa  = 0 
      LET v_estado_marca  = 0
      LET v_cod_rechazo   = 0
      LET v_result_marca  = 0
      LET v_marca_causa   = 0
      LET contador        = 1

      -- Prepara query que extraen información del acreditado
      PREPARE prp_afi_arh FROM v_query
      PREPARE prp_acre_arh FROM v_qury_acre

      FOREACH crs_inf_archivo INTO r_sol_archivo.nss,
                                   r_sol_archivo.num_credito,
                                   r_sol_archivo.tpo_credito,
                                   r_sol_archivo.solicitud,
                                   r_sol_archivo.marca_orig
         
         CASE
           WHEN r_sol_archivo.marca_orig = 1
              LET v_marca_ws      = "01"
              LET v_marca_modulo  = "03"
              LET v_proceso_marca = "201"
           WHEN r_sol_archivo.marca_orig = 2
              LET v_marca_ws      = "02"
              LET v_marca_modulo  = "16"
              LET v_proceso_marca = "1201"
           WHEN r_sol_archivo.marca_orig = 4
              LET v_marca_ws      = "04"
              LET v_marca_modulo  = "43"
              LET v_proceso_marca = "301"
         END CASE 

         -- inicializa variables
         LET v_id_cre_acre   = NULL
         LET v_diagnostico   = NULL
         LET v_cod_result_op = NULL 

         --Obtiene inf personal
         EXECUTE prp_afi_arh USING r_sol_archivo.nss INTO r_inf_dh.id_derechohabiente,
                                                          r_inf_dh.nss,
                                                          r_inf_dh.ap_paterno,
                                                          r_inf_dh.ap_materno,
                                                          r_inf_dh.nombre,
                                                          r_inf_dh.rfc

         --Recupera el acreditado más reciente
         SELECT MAX(id_cre_acreditado)
            INTO v_id_cre_acre
            FROM cre_acreditado
           WHERE id_derechohabiente = r_inf_dh.id_derechohabiente
             AND num_credito     = r_sol_archivo.num_credito
             AND tpo_originacion = r_sol_archivo.marca_orig
             AND tpo_credito     = r_sol_archivo.tpo_credito;

         IF(v_id_cre_acre IS NOT NULL) THEN
            --recupera info del acreditado
            EXECUTE prp_acre_arh USING v_id_cre_acre INTO r_inf_acre.f_otorga,
                                                          r_inf_acre.folio_archivo,
                                                          r_inf_acre.marca_prc
         ELSE 
            CONTINUE FOREACH 
         END IF 
             
         --solicitud Marca PROCESAR
         IF(r_sol_archivo.solicitud = 2) THEN

            --Asigna valores que se envían al WS de Marca
            LET g_solicita_m.apeMaterno          = r_inf_dh.ap_materno CLIPPED
            LET g_solicita_m.apePaterno          = r_inf_dh.ap_paterno CLIPPED
            LET g_solicita_m.fechaPresentacion   = TODAY USING "yyyymmdd"
            LET g_solicita_m.nombres             = r_inf_dh.nombre CLIPPED
            LET g_solicita_m.nss                 = r_inf_dh.nss CLIPPED
            LET g_solicita_m.numCreditoInfonavit = r_sol_archivo.num_credito CLIPPED
            LET g_solicita_m.rfc                 = r_inf_dh.rfc CLIPPED
            LET g_solicita_m.sitCredito          = r_sol_archivo.solicitud CLIPPED
            LET g_solicita_m.tipoCredito         = v_marca_ws CLIPPED
            
            CALL fn_solicita_marca( g_url_servidor CLIPPED,
                                    g_usuario_ws   CLIPPED,
                                    g_password     CLIPPED,
                                    g_intentos,
                                    g_solicita_m.*) 
                          RETURNING v_respuesta_m.*

            LET v_diagnostico   = v_respuesta_m.diagProceso
            LET v_cod_result_op = v_respuesta_m.resultOperacion

            { ----> Prueba ejecución WS Marca a Procesar <-----
            LET v_comando = "apeMaterno          = ",g_solicita_m.apeMaterno         ,"\n",
                            "apePaterno          = ",g_solicita_m.apePaterno         ,"\n",
                            "fechaPresentacion   = ",g_solicita_m.fechaPresentacion  ,"\n",
                            "nombres             = ",g_solicita_m.nombres            ,"\n", 
                            "nss                 = ",g_solicita_m.nss                ,"\n",
                            "numCreditoInfonavit = ",g_solicita_m.numCreditoInfonavit,"\n",
                            "rfc                 = ",g_solicita_m.rfc                ,"\n",
                            "sitCredito          = ",g_solicita_m.sitCredito         ,"\n",
                            "tipoCredito         = ",g_solicita_m.tipoCredito        ,"\n",
                            "Diagnostico WS      = ",v_diagnostico                   ,"\n",
                            "Cod.ResultOp        = ",v_cod_result_op

            CALL fn_mensaje("",v_comando,"")
            }

            --Guarda en histórico
            INSERT INTO cta_his_marca_ws(
                           id_derechohabiente,
                           id_origen ,
                           modulo_cod,
                           tpo_credito,
                           marca,
                           num_credito,
                           f_infonavit,
                           marca_procesar,
                           situacion,
                           f_solicita,
                           intento,
                           cod_result_op,
                           diagnostico,
                           f_actualiza, 
                           usuario,
                           folio_archivo,
                           usr_marca)
                  VALUES (r_inf_dh.id_derechohabiente,
                           v_id_cre_acre,
                           v_marca_modulo,
                           r_sol_archivo.tpo_credito,
                           r_inf_acre.marca_prc,
                           r_sol_archivo.num_credito,
                           r_inf_acre.f_otorga,
                           v_marca_ws,
                           r_sol_archivo.solicitud,
                           TODAY,
                           1,
                           v_cod_result_op,
                           v_diagnostico,
                           TODAY,
                           g_usuario,
                           r_inf_acre.folio_archivo,
                           g_usuario);

            -- Diagnóstico aceptado
            IF(v_diagnostico = "   ") OR (v_diagnostico = "") THEN
               SELECT COUNT(*)
                 INTO v_marca_activa
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = r_inf_dh.id_derechohabiente
                  AND marca =  r_inf_acre.marca_prc;
                        
               IF(v_marca_activa = 0) THEN
                  LET v_fn_cuenta = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
                  PREPARE prp_marca_arh FROM v_fn_cuenta
                  EXECUTE prp_marca_arh USING r_inf_dh.id_derechohabiente,
                                              r_inf_acre.marca_prc,
                                              v_id_cre_acre,
                                              r_inf_acre.folio_archivo,
                                              v_estado_marca, 
                                              v_cod_rechazo,
                                              "",
                                              "",
                                              g_usuario,
                                              v_proceso_marca
                                         INTO v_result_marca
               END IF 
            END IF 
         ELSE 
            --solicitud Desmarca PROCESAR
            IF(r_sol_archivo.solicitud = 0) THEN

               --Asigna valores para consulta WS Desmarca PROCESAR
               LET g_solicita_d.apeMaterno = r_inf_dh.ap_materno CLIPPED
               LET g_solicita_d.apePaterno = r_inf_dh.ap_paterno CLIPPED
               LET g_solicita_d.fechaPresentacion = TODAY USING "yyyymmdd"
               LET g_solicita_d.nombres    = r_inf_dh.nombre CLIPPED
               LET g_solicita_d.nss        = r_inf_dh.nss CLIPPED
               LET g_solicita_d.numCreditoInfonavit = r_sol_archivo.num_credito CLIPPED
               LET g_solicita_d.rfc        = r_inf_dh.rfc CLIPPED
               
               --Llama la función que invoca el WS de Desmarca
               CALL fn_solicita_desmarca(g_url_servidor_d CLIPPED,
                                         g_usuario_ws_d   CLIPPED,
                                         g_password_d     CLIPPED,
                                         g_intentos_d,
                                         g_solicita_d.*) 
                               RETURNING v_respuesta_d.*

               LET v_diagnostico   = v_respuesta_d.diagProceso
               LET v_cod_result_op = v_respuesta_d.resultOperacion

               --Guarda en histórico
               INSERT INTO cta_his_marca_ws(
                              id_derechohabiente,
                              id_origen ,
                              modulo_cod,
                              tpo_credito,
                              marca,
                              num_credito,
                              f_infonavit,
                              marca_procesar,
                              situacion,
                              f_solicita,
                              intento,
                              cod_result_op,
                              diagnostico,
                              f_actualiza, 
                              usuario,
                              folio_archivo,
                              usr_marca)
                     VALUES (r_inf_dh.id_derechohabiente,
                              v_id_cre_acre,
                              v_marca_modulo,
                              r_sol_archivo.tpo_credito,
                              r_inf_acre.marca_prc,
                              r_sol_archivo.num_credito,
                              r_inf_acre.f_otorga,
                              v_marca_ws,
                              r_sol_archivo.solicitud,
                              TODAY,
                              1,
                              v_cod_result_op,
                              v_diagnostico,
                              TODAY,
                              g_usuario,
                              r_inf_acre.folio_archivo,
                              g_usuario);

               -- Diagnóstico aceptado
               IF(v_diagnostico = "   ") OR (v_diagnostico = "") THEN
                  --verifica si existe la marca
                  SELECT COUNT(*)
                    INTO v_marca_activa
                    FROM sfr_marca_activa
                   WHERE id_derechohabiente = r_inf_dh.id_derechohabiente
                     AND marca =  r_inf_acre.marca_prc;
                        
                  IF(v_marca_activa >= 1) THEN
                     LET v_fn_cuenta = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
                     PREPARE prp_desm_arh FROM v_fn_cuenta
                     EXECUTE prp_desm_arh USING r_inf_dh.id_derechohabiente,
                                                r_inf_acre.marca_prc,
                                                v_id_cre_acre,
                                                v_estado_marca, 
                                                v_marca_causa,
                                                g_usuario,
                                                v_proceso_marca
                                           INTO v_result_marca
                     END IF 
                  END IF 
            END IF 
         END IF 
         
         LET contador = contador + 1
      
      END FOREACH
   END IF 
   
   RETURN bnd_archivo

END FUNCTION 

FUNCTION combobox_solicitud()

   DEFINE cbx           ui.ComboBox

   LET cbx = ui.ComboBox.forName("v_solicitud")

   --llena combo
   CALL cbx.addItem(2,"2-Marca")
   CALL cbx.addItem(0,"0-Desmarca")
   
END FUNCTION 

FUNCTION crea_temporal()

   DATABASE safre_tmp
   
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_solic_procesar

   WHENEVER ERROR STOP 
      CREATE TABLE tmp_solic_procesar (
                       nss CHAR(11),
                       num_credito DECIMAL(10,0),
                       tpo_credito SMALLINT,
                       solicitud   SMALLINT,
                       marca_orig  SMALLINT);

   DATABASE safre_viv
   
END FUNCTION 

FUNCTION fn_numerico(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE indice     INTEGER
   DEFINE bnd        SMALLINT 

   LET bnd = 0
   LET p_cadena = p_cadena CLIPPED  --Quita espacios en blanco
   
   FOR indice = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(indice,indice) MATCHES '[0-9]') THEN
         LET bnd = 0
      ELSE
         LET bnd = 1
         EXIT FOR 
      END IF
   END FOR

   RETURN bnd

END FUNCTION
