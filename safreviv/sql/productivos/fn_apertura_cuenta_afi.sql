






CREATE FUNCTION "safreviv".fn_apertura_cuenta_afi (p_nss                 CHAR(11),
                                        p_curp                CHAR(18),
                                        p_rfc                 CHAR(13),
                                        p_ind_nrp             CHAR(1),
                                        p_nombre_af           CHAR(50),
                                        p_tipo_trabajador     CHAR(1),
                                        p_id_credito          SMALLINT,
                                        p_folio               DECIMAL(9,0),  --folio en turno
                                        p_origen_afiliacion   CHAR(1))       --necesario ya que estaba en codigo duro

   RETURNING DECIMAL(9);

   DEFINE v_id_derechohabiente   DECIMAL(9);
   DEFINE v_fecha_nacim_aux      CHAR(10);
   DEFINE v_fecha_aux            CHAR(6);
   DEFINE v_mes_aux              SMALLINT;
   DEFINE v_dia_aux              SMALLINT;
   DEFINE v_numero_aux           SMALLINT;
   DEFINE v_ano_aux              VARCHAR(4);
   DEFINE v_fecha_nacim          DATE;
   DEFINE v_fecha_hoy            DATE;
   DEFINE v_ano_actual           CHAR(4);
   DEFINE v_error                SMALLINT;
   DEFINE v_nombre_af            CHAR(40);
   DEFINE v_ap_paterno_af        CHAR(40);
   DEFINE v_ap_materno_af        CHAR(40);
   DEFINE v_ind_estado_cuenta    SMALLINT; -- estado de la cuenta 0- activa, 1-inactiva
   DEFINE v_f_estado_cuenta      DATE    ; -- igual a la fecha de apertura

   -- variables para control de errores
   DEFINE v_sql_error            INTEGER;
   DEFINE v_isam_error           INTEGER;
   DEFINE v_mensaje              VARCHAR(250);

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/apertura_cuenta.trace';
   --TRACE 'Inicia LA ALTA DE CUENTA automatica - FECHA:'||TODAY;

   -- en caso de excepcion se devuelve error
   ON EXCEPTION SET v_sql_error, v_isam_error, v_mensaje

      RETURN v_sql_error;
   END EXCEPTION

   --Inicializacion de variables
   LET v_fecha_hoy = TODAY;

   LET v_fecha_nacim = fn_fnacimiento(p_nss, p_curp, p_rfc);

   --Se separa el nombre del derechohabiente
   CALL fn_separa_nombre(p_nombre_af)
   RETURNING v_ap_paterno_af, v_ap_materno_af, v_nombre_af;

   --Obtenemos el ID insertado
   SELECT seq_derechohabiente.NEXTVAL
   INTO   v_id_derechohabiente
   FROM   systables
   WHERE  tabid = 1;

   -- se indica que la cuenta estará activa
   LET v_ind_estado_cuenta = 0; -- estado de la cuenta 0- activa, 1-inactiva
   LET v_f_estado_cuenta   = v_fecha_hoy; -- igual a la fecha de apertura

   IF NOT (p_curp IS NOT NULL AND v_fecha_nacim IS NOT NULL) THEN
      LET p_curp = NULL;
   END IF

   IF (LENGTH (p_rfc) <> 13) AND
      (LENGTH (p_rfc) <> 10) THEN
       LET p_rfc = NULL;
   ELSE
     --Punto 2
      IF (LENGTH (p_rfc) = 10) THEN
         IF (p_rfc[1]  = ' ') OR
            (p_rfc[2]  = ' ') OR
            (p_rfc[3]  = ' ') OR
            (p_rfc[4]  = ' ') OR
            (p_rfc[5]  = ' ') OR
            (p_rfc[6]  = ' ') OR
            (p_rfc[7]  = ' ') OR
            (p_rfc[8]  = ' ') OR
            (p_rfc[9]  = ' ') OR
            (p_rfc[10] = ' ') THEN
            LET p_rfc = ""; -- Prende Bandera de la CURP errónea
         END IF
      END IF

      IF (LENGTH (p_rfc) = 13) THEN
         IF (p_rfc[1]  = ' ') OR
            (p_rfc[2]  = ' ') OR
            (p_rfc[3]  = ' ') OR
            (p_rfc[4]  = ' ') OR
            (p_rfc[5]  = ' ') OR
            (p_rfc[6]  = ' ') OR
            (p_rfc[7]  = ' ') OR
            (p_rfc[8]  = ' ') OR
            (p_rfc[9]  = ' ') OR
            (p_rfc[10] = ' ') OR
            (p_rfc[11] = ' ') OR
            (p_rfc[12] = ' ') OR
            (p_rfc[13] = ' ') THEN
            LET p_rfc = ""; -- Prende Bandera de la CURP errónea
         END IF
      ELSE
         IF (p_rfc[13] <> "A") AND
            (p_rfc[13] NOT MATCHES '[0-9]') THEN

            -- Elimina la homoclave
            LET p_rfc[11,13] = NULL;
         END IF

         IF (p_rfc[11,13] = "000") THEN
            --Elimina la homoclave
            LET p_rfc [11,13] = NULL;
         END IF
      END IF

      IF (LENGTH (p_rfc) = 10) THEN
         IF (p_rfc[1]  NOT MATCHES '[0-9]' AND p_rfc[1]  NOT MATCHES '[A-Z]') OR
            (p_rfc[2]  NOT MATCHES '[0-9]' AND p_rfc[2]  NOT MATCHES '[A-Z]') OR
            (p_rfc[3]  NOT MATCHES '[0-9]' AND p_rfc[3]  NOT MATCHES '[A-Z]') OR
            (p_rfc[4]  NOT MATCHES '[0-9]' AND p_rfc[4]  NOT MATCHES '[A-Z]') OR
            (p_rfc[5]  NOT MATCHES '[0-9]' AND p_rfc[5]  NOT MATCHES '[A-Z]') OR
            (p_rfc[6]  NOT MATCHES '[0-9]' AND p_rfc[6]  NOT MATCHES '[A-Z]') OR
            (p_rfc[7]  NOT MATCHES '[0-9]' AND p_rfc[7]  NOT MATCHES '[A-Z]') OR
            (p_rfc[8]  NOT MATCHES '[0-9]' AND p_rfc[8]  NOT MATCHES '[A-Z]') OR
            (p_rfc[9]  NOT MATCHES '[0-9]' AND p_rfc[9]  NOT MATCHES '[A-Z]') OR
            (p_rfc[10] NOT MATCHES '[0-9]' AND p_rfc[10] NOT MATCHES '[A-Z]') THEN
         
            LET p_rfc = "";
         END IF
      END IF

      IF (LENGTH (p_rfc) = 13) THEN
         IF (p_rfc[1]  NOT MATCHES '[0-9]' AND p_rfc[1]  NOT MATCHES '[A-Z]') OR
            (p_rfc[2]  NOT MATCHES '[0-9]' AND p_rfc[2]  NOT MATCHES '[A-Z]') OR
            (p_rfc[3]  NOT MATCHES '[0-9]' AND p_rfc[3]  NOT MATCHES '[A-Z]') OR
            (p_rfc[4]  NOT MATCHES '[0-9]' AND p_rfc[4]  NOT MATCHES '[A-Z]') OR
            (p_rfc[5]  NOT MATCHES '[0-9]' AND p_rfc[5]  NOT MATCHES '[A-Z]') OR
            (p_rfc[6]  NOT MATCHES '[0-9]' AND p_rfc[6]  NOT MATCHES '[A-Z]') OR
            (p_rfc[7]  NOT MATCHES '[0-9]' AND p_rfc[7]  NOT MATCHES '[A-Z]') OR
            (p_rfc[8]  NOT MATCHES '[0-9]' AND p_rfc[8]  NOT MATCHES '[A-Z]') OR
            (p_rfc[9]  NOT MATCHES '[0-9]' AND p_rfc[9]  NOT MATCHES '[A-Z]') OR
            (p_rfc[10] NOT MATCHES '[0-9]' AND p_rfc[10] NOT MATCHES '[A-Z]') OR
            (p_rfc[11] NOT MATCHES '[0-9]' AND p_rfc[11] NOT MATCHES '[A-Z]') OR
            (p_rfc[12] NOT MATCHES '[0-9]' AND p_rfc[12] NOT MATCHES '[A-Z]') OR
            (p_rfc[13] NOT MATCHES '[0-9]' AND p_rfc[13] NOT MATCHES '[A-Z]') THEN
         
            LET p_rfc = "";
         END IF
      END IF

      IF (p_rfc[1] NOT MATCHES '[A-Z]') OR
         (p_rfc[2] NOT MATCHES '[A-Z]') OR
         (p_rfc[3] NOT MATCHES '[A-Z]') OR
         (p_rfc[4] NOT MATCHES '[A-Z]') THEN

         LET p_rfc = "";
      END IF

      IF (p_rfc[5]  NOT MATCHES '[0-9]') OR
         (p_rfc[6]  NOT MATCHES '[0-9]') OR
         (p_rfc[7]  NOT MATCHES '[0-9]') OR
         (p_rfc[8]  NOT MATCHES '[0-9]') OR
         (p_rfc[9]  NOT MATCHES '[0-9]') OR
         (p_rfc[10] NOT MATCHES '[0-9]') THEN

         LET p_rfc = "";
      END IF
   END IF

   IF (p_curp = "000000000000000000") THEN
       LET p_curp = NULL;
   END IF 

   IF (p_curp <> "000000000000000000") THEN
      IF (p_curp[1]  = ' ') OR 
         (p_curp[2]  = ' ') OR
         (p_curp[3]  = ' ') OR
         (p_curp[4]  = ' ') OR
         (p_curp[5]  = ' ') OR
         (p_curp[6]  = ' ') OR
         (p_curp[7]  = ' ') OR
         (p_curp[8]  = ' ') OR
         (p_curp[9]  = ' ') OR
         (p_curp[10] = ' ') OR
         (p_curp[11] = ' ') OR
         (p_curp[12] = ' ') OR            
         (p_curp[13] = ' ') OR
         (p_curp[14] = ' ') OR
         (p_curp[15] = ' ') OR
         (p_curp[16] = ' ') OR
         (p_curp[17] = ' ') OR
         (p_curp[18] = ' ') THEN
         LET p_curp = ""; -- Prende Bandera de la CURP errónea
      END IF

      IF (p_curp[1] NOT MATCHES '[A-Z]') OR
         (p_curp[2] NOT MATCHES '[A-Z]') OR
         (p_curp[3] NOT MATCHES '[A-Z]') OR
         (p_curp[4] NOT MATCHES '[A-Z]') THEN

          LET p_curp = ""; -- Prende Bandera de la CURP errónea
      END IF

      IF (p_curp[1] = '0-9') OR
         (p_curp[2] = '0-9') OR
         (p_curp[3] = '0-9') OR
         (p_curp[4] = '0-9') OR
         (p_curp[5] = '0-9') OR
         (p_curp[6] = '0-9') OR
         (p_curp[7] = '0-9') OR
         (p_curp[8] = '0-9') OR
         (p_curp[9] = '0-9') OR
         (p_curp[10]= '0-9') THEN

         LET p_curp = ""; --Enciende bandera de que encontro un numero
      END IF

      IF (p_curp[1]  NOT MATCHES '[0-9]' AND p_curp[1]  NOT MATCHES '[A-Z]') OR
         (p_curp[2]  NOT MATCHES '[0-9]' AND p_curp[2]  NOT MATCHES '[A-Z]') OR
         (p_curp[3]  NOT MATCHES '[0-9]' AND p_curp[3]  NOT MATCHES '[A-Z]') OR
         (p_curp[4]  NOT MATCHES '[0-9]' AND p_curp[4]  NOT MATCHES '[A-Z]') OR
         (p_curp[5]  NOT MATCHES '[0-9]' AND p_curp[5]  NOT MATCHES '[A-Z]') OR
         (p_curp[6]  NOT MATCHES '[0-9]' AND p_curp[6]  NOT MATCHES '[A-Z]') OR
         (p_curp[7]  NOT MATCHES '[0-9]' AND p_curp[7]  NOT MATCHES '[A-Z]') OR
         (p_curp[8]  NOT MATCHES '[0-9]' AND p_curp[8]  NOT MATCHES '[A-Z]') OR
         (p_curp[9]  NOT MATCHES '[0-9]' AND p_curp[9]  NOT MATCHES '[A-Z]') OR
         (p_curp[10] NOT MATCHES '[0-9]' AND p_curp[10] NOT MATCHES '[A-Z]') OR
         (p_curp[11] NOT MATCHES '[0-9]' AND p_curp[11] NOT MATCHES '[A-Z]') OR
         (p_curp[12] NOT MATCHES '[0-9]' AND p_curp[12] NOT MATCHES '[A-Z]') OR
         (p_curp[13] NOT MATCHES '[0-9]' AND p_curp[13] NOT MATCHES '[A-Z]') OR
         (p_curp[14] NOT MATCHES '[0-9]' AND p_curp[14] NOT MATCHES '[A-Z]') OR
         (p_curp[15] NOT MATCHES '[0-9]' AND p_curp[15] NOT MATCHES '[A-Z]') OR
         (p_curp[16] NOT MATCHES '[0-9]' AND p_curp[16] NOT MATCHES '[A-Z]') OR
         (p_curp[17] NOT MATCHES '[0-9]' AND p_curp[17] NOT MATCHES '[A-Z]') OR
         (p_curp[18] NOT MATCHES '[0-9]' AND p_curp[18] NOT MATCHES '[A-Z]') THEN
      
         LET p_curp = "";  --Enciende bandera de que encontro un caracter especial
      END IF
   END IF

   -- se inserta el derechohabiente
   INSERT INTO afi_derechohabiente
               (id_derechohabiente ,
                nss                ,
                curp               ,
                rfc                ,
                ind_nrp            ,
                f_nacimiento       ,
                nombre_imss        ,
                nombre_af          ,
                ap_paterno_af      ,
                ap_materno_af      ,
                tipo_trabajador    ,
                origen_afiliacion  ,
                id_credito         ,
                f_credito          ,
                folio_lote         ,
                f_apertura_inf     ,
                f_apertura         ,
                ind_estado_cuenta  ,
                f_estado_cuenta)
        VALUES (v_id_derechohabiente , --id_derechohabiente
                p_nss                , --nss
                p_curp               , --curp
                p_rfc                , --rfc
                p_ind_nrp            , --ind_nrp
                v_fecha_nacim        , --f_nacimiento
                p_nombre_af          , --nombre_imss
                v_nombre_af          , --nombre_af
                v_ap_paterno_af      , --ap_paterno_af
                v_ap_materno_af      , --ap_materno_af
                p_tipo_trabajador    , --tipo_trabajador
                p_origen_afiliacion  , --origen_afiliacion
                p_id_credito         , --id_credito
                v_fecha_hoy          , --f_credito
                p_folio              , --folio_lote
                v_fecha_hoy          , --f_apertura_inf
                v_fecha_hoy          , --f_apertura
                v_ind_estado_cuenta  , --estado de la cuenta
                v_f_estado_cuenta);  --fecha de estado de la cuenta

   RETURN v_id_derechohabiente;

END FUNCTION;


