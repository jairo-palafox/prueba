






CREATE FUNCTION "safreviv".fn_afi_integra_nuevos_nss_trm(p_usuario_cod CHAR(20),
                                              p_folio DECIMAL(10), 
                                              p_nombre_archivo CHAR(18),
                                              p_pid DECIMAL(9,0),
                                              p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255), INTEGER, INTEGER, VARCHAR(11)

   -- campos de la tabla temporal
   DEFINE tmp_afi_nss_trm_nss         CHAR(11);
   DEFINE tmp_afi_nss_trm_curp        CHAR(18);
   DEFINE tmp_afi_nss_trm_rfc         CHAR(13);
   DEFINE tmp_afi_nss_trm_nombre_imss CHAR(50);
   DEFINE tmp_afi_nss_trm_sexo        CHAR(1) ;

   -- campos de la tabla de rechazos de nuevos nss de trm
   DEFINE afi_nss_trm_rch_nss         CHAR(11);
   DEFINE afi_nss_trm_rch_curp        CHAR(18);
   DEFINE afi_nss_trm_rch_rfc         CHAR(13);
   DEFINE afi_nss_trm_rch_nombre_imss CHAR(50);
   DEFINE afi_nss_trm_rch_sexo        CHAR(1) ;
   DEFINE afi_nss_trm_rch_cod_rechazo SMALLINT;
   DEFINE afi_nss_trm_rch_folio       DECIMAL(9,0);

   DEFINE afi_rel_lab_ind_rel       SMALLINT;

   -- para cambiar el formato de la fecha
   DEFINE v_fecha_texto             VARCHAR(10);
   DEFINE v_rfc                     VARCHAR(13);
   DEFINE v_curp                    VARCHAR(18);
   DEFINE v_rfc_curp                VARCHAR(18);

   -- Control de Excepciones
   DEFINE v_i_resultado             SMALLINT;
   DEFINE sql_err                   INTEGER;
   DEFINE isam_err                  INTEGER;
   DEFINE err_txt                   VARCHAR(255);
   DEFINE v_fecha_valida            SMALLINT;
   DEFINE v_fecha_movimiento        DATE;

   -- Variables de validaciones
   DEFINE v_d_id_referencia         DECIMAL(9,0);
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_diagnostico_unificador  SMALLINT;
   DEFINE v_diagnostico_unificadas  SMALLINT;
   DEFINE v_diagnostico_rechazo     SMALLINT;

   -- número de altas aceptadas y rechzadas
   DEFINE v_num_altas_aceptadas     INTEGER;
   DEFINE v_num_altas_rechazadas    INTEGER;
   DEFINE v_registro_rechazado      SMALLINT; -- booleana para verificar un registro rechazado

   -- Variable para marca de cuenta
   DEFINE v_i_estado_marca          INTEGER;

   -- constantes para codigos de error
   DEFINE v_error_nss_vacio         SMALLINT;
   DEFINE v_error_nss_ya_existe     SMALLINT;
   DEFINE v_error_f_mov_vacia       SMALLINT;
   DEFINE v_error_nrp_vacio         SMALLINT;
   DEFINE v_error_nombre_vacio      SMALLINT;
   DEFINE v_error_f_mov_post_act    SMALLINT; -- la fecha de movimiento es posterior a la actual
   DEFINE v_error_nss_mal_formado   SMALLINT; -- el NSS esta mal formado
   DEFINE v_rfc_nom                 CHAR (4);
   DEFINE v_cont                    SMALLINT;
   DEFINE bnb_hclv                  SMALLINT;
   DEFINE bnd_rfc                   SMALLINT;
   DEFINE bnd_curp                  SMALLINT;
   DEFINE v_evalua_hclv             CHAR(1);
   DEFINE v_evalua_rfc              CHAR(1);
   DEFINE v_evalua_curp             CHAR(1);
   DEFINE v_ind_rfc                 SMALLINT;
   DEFINE v_raiz_rfc                CHAR(4);
   DEFINE v_raiz_curp               CHAR(4);

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, isam_err, err_txt, v_num_altas_aceptadas, v_num_altas_rechazadas, tmp_afi_nss_trm_nss;
   END EXCEPTION

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de nuevos NSS de TRM finalizó correctamente.";
   LET tmp_afi_nss_trm_nss   = NULL; -- no hay nss con error
   LET bnb_hclv           = 0;
   LET bnd_rfc            = 0;
   LET bnd_curp           = 0;

   SET DEBUG FILE TO "/safreviv_int/BD/fn_afi_integra_nuevos_nss_trm.trace";
   trace ON;

   -- se definen las constantes de codigo de error
   LET v_error_nss_vacio                = 8;
   LET v_error_nss_ya_existe            = 2;
   LET v_error_f_mov_vacia   = 5;
   LET v_error_nrp_vacio                = 6;
   LET v_error_nombre_vacio             = 7;
   LET v_error_f_mov_post_act = 9;
   LET v_error_nss_mal_formado          = 14;

   -- se inician los contadores de altas aceptadas y rechazadas
   LET v_num_altas_aceptadas  = 0;
   LET v_num_altas_rechazadas = 0;

   -- actualizacion de las tablas de control
   UPDATE bat_ctr_proceso
      SET folio = p_folio
    WHERE pid = p_pid;
   
   UPDATE bat_ctr_operacion
      SET folio = p_folio
    WHERE pid = p_pid
      AND opera_cod = 2;

   UPDATE glo_ctr_archivo
      SET estado = 2,
          folio  = p_folio
    WHERE proceso_cod = p_proceso_cod
      AND nombre_archivo = p_nombre_archivo;

   -- lectura de la tabla temporal
   --trace "Se leen las altas";
   FOREACH
      SELECT nss        ,
             curp       ,
             rfc        ,
             nombre_imss,
             sexo       
        INTO tmp_afi_nss_trm_nss        ,
             tmp_afi_nss_trm_curp       ,
             tmp_afi_nss_trm_rfc        ,
             tmp_afi_nss_trm_nombre_imss,
             tmp_afi_nss_trm_sexo       
        FROM safre_tmp:tmp_afi_nuevos_nss_trm

    --trace "NSS :" || tmp_afi_nss_trm_nss;

      -- se asume que el registro es correcto
      LET v_registro_rechazado        = 0;
      LET afi_nss_trm_rch_cod_rechazo = 0;

      LET v_fecha_valida     = 0;
      LET v_fecha_movimiento = NULL;

      -- ====================================================================
      --   VERIFICACIÓN DE ERRORES EN LA INFORMACION CARGADA
      -- ====================================================================

      -- se verifica que el NSS venga
      IF ( tmp_afi_nss_trm_nss IS NULL AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo
         LET v_registro_rechazado = 1;

         LET afi_nss_trm_rch_cod_rechazo = v_error_nss_vacio; -- NSS vacio
      END IF
      
      -- se verifica si el NSS ya se ha dado de alta
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_afi_nss_trm_nss;

      -- si el nss se encontro entonces ya no se agrega
      IF ( v_id_derechohabiente IS NOT NULL AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo porque ya existe el NSS y el NRP
         LET v_registro_rechazado = 1;
         LET afi_nss_trm_rch_cod_rechazo = v_error_nss_ya_existe; -- NSS YA EXISTE         
      END IF

      -- el nombre vacio no es valido
      IF ( tmp_afi_nss_trm_nombre_imss IS NULL AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo porque el nombre esta vacio
         LET v_registro_rechazado = 1;
         LET afi_nss_trm_rch_cod_rechazo = v_error_nombre_vacio; -- nombre vacio
      END IF
    
    -- se verifica que el NSS este formado correctamente
      IF ( LENGTH(tmp_afi_nss_trm_nss) < 11 AND v_registro_rechazado = 0 ) THEN
         -- se marca el rechazo porque el NSS no tiene la estructura correcta
         LET v_registro_rechazado = 1;
         LET afi_nss_trm_rch_cod_rechazo = v_error_nss_mal_formado; -- NSS mal formado
      END IF

      IF ( v_registro_rechazado = 1 ) THEN
         -- se inserta en la tabla de rechazos
         -- no se pudo abrir la cuenta, se registra en rechazos
         LET afi_nss_trm_rch_nss         = tmp_afi_nss_trm_nss;
         LET afi_nss_trm_rch_curp        = tmp_afi_nss_trm_curp;
         LET afi_nss_trm_rch_rfc         = tmp_afi_nss_trm_rfc;
         LET afi_nss_trm_rch_nombre_imss = tmp_afi_nss_trm_nombre_imss;
         LET afi_nss_trm_rch_sexo        = tmp_afi_nss_trm_sexo;
         LET afi_nss_trm_rch_folio       = p_folio;

         INSERT INTO afi_nss_trm_rch (
                nss        ,
                curp       ,
                rfc        ,
                nombre_imss,
                sexo       ,
                cod_rechazo,
                folio)
         VALUES (
                afi_nss_trm_rch_nss        ,
                afi_nss_trm_rch_curp       ,
                afi_nss_trm_rch_rfc        ,
                afi_nss_trm_rch_nombre_imss,
                afi_nss_trm_rch_sexo       ,
                afi_nss_trm_rch_cod_rechazo,
                afi_nss_trm_rch_folio);

         -- se cuenta una alta rechazada
         LET v_num_altas_rechazadas = v_num_altas_rechazadas + 1;

         --trace "rechazado";
         -- se continua con el siguiente registro
         CONTINUE FOREACH;
      END IF

      -- se asigna el curp/rfc
      LET v_rfc  = tmp_afi_nss_trm_rfc;
      LET v_curp = tmp_afi_nss_trm_curp;

      --Paso 1
      IF (LENGTH (v_rfc) <> 13 ) THEN
         LET v_rfc   = NULL;
         LET bnd_rfc = 1;
      END IF

      --Paso 2
      IF (v_rfc IS NOT NULL) AND 
         (LENGTH (v_rfc) = 13)  THEN
         IF (v_rfc[13] <> "A") AND
            (v_rfc[13] NOT MATCHES '[0-9]') THEN 
            -- Elimina la homoclave
            LET v_rfc[11,13] = NULL;
         END IF

         --Paso 3
         IF (v_rfc[11,13] = "000") THEN
            --Elimina la homoclave
            LET v_rfc [11,13] = NULL; 
         END IF

         -- Barre cadena homoclave por si encuentra un caracter especial
         LET bnb_hclv = 0; --Bandera apagada de no existe caracteres especiales

         IF (v_rfc[1]  NOT MATCHES  '[0-9]' AND v_rfc[1]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[2]  NOT MATCHES  '[0-9]' AND v_rfc[2]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[3]  NOT MATCHES  '[0-9]' AND v_rfc[3]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[4]  NOT MATCHES  '[0-9]' AND v_rfc[4]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[5]  NOT MATCHES  '[0-9]' AND v_rfc[5]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[6]  NOT MATCHES  '[0-9]' AND v_rfc[6]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[7]  NOT MATCHES  '[0-9]' AND v_rfc[7]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[8]  NOT MATCHES  '[0-9]' AND v_rfc[8]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[9]  NOT MATCHES  '[0-9]' AND v_rfc[9]  NOT MATCHES  '[A-Z]') OR
            (v_rfc[10] NOT MATCHES  '[0-9]' AND v_rfc[10] NOT MATCHES  '[A-Z]') OR
            (v_rfc[11] NOT MATCHES  '[0-9]' AND v_rfc[11] NOT MATCHES  '[A-Z]') OR
            (v_rfc[12] NOT MATCHES  '[0-9]' AND v_rfc[12] NOT MATCHES  '[A-Z]') OR
            (v_rfc[13] NOT MATCHES  '[0-9]' AND v_rfc[13] NOT MATCHES  '[A-Z]') THEN 
            LET bnb_hclv = 1;
         END IF

         IF(bnb_hclv = 1) THEN
             -- Si contiene un caracter especial elimina la homoclave
             LET v_rfc [11,13] = NULL;  
         END IF

         --Paso 4 
         IF (v_rfc[1,10]) <> (v_curp[1,10]) THEN
            -- Verifica que la raiz de la rfc no sea una palabra inconveniente
            SELECT raiz_rfc
              INTO v_raiz_rfc
              FROM cat_inconveniente
             WHERE palabra_inconv = v_rfc[1,4];

            IF(v_raiz_rfc IS NOT NULL) THEN 
               LET v_rfc = v_raiz_rfc||v_rfc[5,10];
            END IF
         END IF
      END IF

      IF (v_rfc[1] MATCHES '0-9') OR
         (v_rfc[2] MATCHES '0-9') OR
         (v_rfc[3] MATCHES '0-9') OR
         (v_rfc[4] MATCHES '0-9') THEN

         LET bnd_rfc = 1; -- Enciende bandera de que encontró un número
      END IF 

      IF(bnd_rfc = 1) THEN
         -- Deja en Nulo al RFC
         LET v_rfc = NULL;
      END IF 

     --Paso 7
      IF (LENGTH (v_curp ) <> 18) THEN
         LET v_curp = NULL;
      ELSE
         -- Paso 8
         LET bnd_curp = 0; --Apaga bandera

         IF (v_curp[1] MATCHES '0-9') OR
            (v_curp[2] MATCHES '0-9') OR
            (v_curp[3] MATCHES '0-9') OR
            (v_curp[4] MATCHES '0-9') THEN

            LET bnd_curp = 1; --Enciende bandera de que encontró un número
         END IF 

         IF (bnd_curp = 1) THEN 
            --Deja en nulo la CURP
            LET v_curp = NULL;
         END IF

         -- Verifica que la raiz de la curp no sea una palabra inconveniente
         SELECT raiz_curp
           INTO v_raiz_curp
           FROM cat_inconveniente
          WHERE palabra_inconv = v_curp[1,4];

         IF(v_raiz_curp IS NOT NULL) THEN 
            LET v_curp = v_raiz_curp||v_curp[5,10];
         END IF
      END IF

      ---RFC es nulo, se traen las 10 posiciones de la CURP 
       IF (v_rfc IS NULL) OR
          (v_rfc = '')  THEN 
           LET v_rfc = v_curp[1,10];
      END IF

      -- Paso 9
      LET bnd_rfc= 0; --Bandera apagada de no existe caracteres especiales
      -- LET v_evalua_rfc = NULL;

      IF (v_rfc[1]  NOT MATCHES '[0-9]' AND v_rfc[1]  NOT MATCHES '[A-Z]') OR
         (v_rfc[2]  NOT MATCHES '[0-9]' AND v_rfc[2]  NOT MATCHES '[A-Z]') OR
         (v_rfc[3]  NOT MATCHES '[0-9]' AND v_rfc[3]  NOT MATCHES '[A-Z]') OR
         (v_rfc[4]  NOT MATCHES '[0-9]' AND v_rfc[4]  NOT MATCHES '[A-Z]') OR
         (v_rfc[5]  NOT MATCHES '[0-9]' AND v_rfc[5]  NOT MATCHES '[A-Z]') OR
         (v_rfc[6]  NOT MATCHES '[0-9]' AND v_rfc[6]  NOT MATCHES '[A-Z]') OR
         (v_rfc[7]  NOT MATCHES '[0-9]' AND v_rfc[7]  NOT MATCHES '[A-Z]') OR
         (v_rfc[8]  NOT MATCHES '[0-9]' AND v_rfc[8]  NOT MATCHES '[A-Z]') OR
         (v_rfc[9]  NOT MATCHES '[0-9]' AND v_rfc[9]  NOT MATCHES '[A-Z]') OR
         (v_rfc[10] NOT MATCHES '[0-9]' AND v_rfc[10] NOT MATCHES '[A-Z]') THEN
          LET bnd_rfc = 1; --Enciende bandera de que encontro un caracter especial
      END IF

      IF (bnd_rfc = 1 ) THEN 
         LET v_rfc[1,10] = NULL;
      END IF

      LET bnd_curp = 0; --Bnadera apagada de no existe caracteres especiales
      -- LET v_evalua_curp = NULL;

      IF (v_curp[1]  NOT MATCHES '[0-9]' AND v_curp[1]  NOT MATCHES '[A-Z]') OR
         (v_curp[2]  NOT MATCHES '[0-9]' AND v_curp[2]  NOT MATCHES '[A-Z]') OR
         (v_curp[3]  NOT MATCHES '[0-9]' AND v_curp[3]  NOT MATCHES '[A-Z]') OR
         (v_curp[4]  NOT MATCHES '[0-9]' AND v_curp[4]  NOT MATCHES '[A-Z]') OR
         (v_curp[5]  NOT MATCHES '[0-9]' AND v_curp[5]  NOT MATCHES '[A-Z]') OR
         (v_curp[6]  NOT MATCHES '[0-9]' AND v_curp[6]  NOT MATCHES '[A-Z]') OR
         (v_curp[7]  NOT MATCHES '[0-9]' AND v_curp[7]  NOT MATCHES '[A-Z]') OR
         (v_curp[8]  NOT MATCHES '[0-9]' AND v_curp[8]  NOT MATCHES '[A-Z]') OR
         (v_curp[9]  NOT MATCHES '[0-9]' AND v_curp[9]  NOT MATCHES '[A-Z]') OR
         (v_curp[10] NOT MATCHES '[0-9]' AND v_curp[10] NOT MATCHES '[A-Z]') THEN
         LET bnd_curp = 1;  --Enciende bandera de que encontro un caracter especial
      END IF

      IF (bnd_curp = 1) THEN
         LET v_curp[1,10] = NULL;
      END IF

      -- sin relacion laboral
      LET afi_rel_lab_ind_rel = 0;

      -- se cuenta un derechohabiente dado de alta
      LET v_num_altas_aceptadas = v_num_altas_aceptadas + 1;
         
      EXECUTE FUNCTION fn_apertura_cuenta_afi(tmp_afi_nss_trm_nss         -- NSS
                                             ,v_curp                      -- CURP
                                             ,v_rfc                       -- RFC
                                             ,afi_rel_lab_ind_rel         -- Ind relacion laboral 
                                             ,tmp_afi_nss_trm_nombre_imss -- Nombre
                                             ,"I"                         -- tipo trabajador (IMSS)
                                             ,0                           -- id_credito (sin credito)
                                             ,p_folio                     -- folio         
                                             ,"A" )                       -- origen de afiliacion
      INTO v_id_derechohabiente;

   END FOREACH;

   -- se actualizan las tablas
   UPDATE STATISTICS FOR TABLE afi_derechohabiente;

    -- se devuelve el resultado de la ejecucion del SP
    RETURN v_i_resultado, isam_err, err_txt, v_num_altas_aceptadas, v_num_altas_rechazadas, tmp_afi_nss_trm_nss;

END FUNCTION
;


