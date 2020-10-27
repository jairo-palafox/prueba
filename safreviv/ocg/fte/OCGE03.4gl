###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGE03                                                  #
#Objetivo          => Programa para la captura de la carta de instrucción     #
#                     SP002                                                   #
#Autor             => Héctor F. Jiménez Lara                                  #
#Fecha inicio      => 08 Octube 2015                                          #
###############################################################################
DATABASE safre_viv

   DEFINE v_s_qry             STRING
   DEFINE cb                  ui.ComboBox 
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod  -- clave del usuario firmado
   DEFINE v_i_cnt             INTEGER
   DEFINE v_cmd               STRING
   DEFINE r_cod_rechazo       INTEGER
   DEFINE v_bnd               SMALLINT
   DEFINE v_cnt_rl            INTEGER

   DEFINE v_nss               CHAR(11)
   DEFINE v_rec_dat RECORD
    id_ocg_formalizacion      DECIMAL(9,0),
    nss                       CHAR(11),
    nombre_af                 CHAR(80),
    rfc                       CHAR(13),
    curp                      CHAR(18),
    id_ocg_entidad_financiera DECIMAL(9,0),
    tpo_moneda                SMALLINT,
    tasa_base                 CHAR(20),
    margen                    CHAR(20),
    monto_credito             DECIMAL(15,2),
    valor_avaluo              DECIMAL(15,2),
    tpo_credito               CHAR(1),
    f_otorga_ent_fed          DATE,
    ent_fed_inmueble          SMALLINT,
    mcpio_inmueble            SMALLINT,
    id_derechohabiente        DECIMAL(9,0),
    id_ocg_tramite            DECIMAL(9,0)
   END RECORD
   
MAIN
   DEFINE p_tipo_ejecucion    SMALLINT                      -- forma como ejecutara el programa
   DEFINE p_s_titulo          STRING                        -- título de la ventana

   -- se recupera la clave de usuario desde parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".OCGE03.log")

   CLOSE WINDOW SCREEN 

   CALL fn_cons_nss()
END MAIN 

FUNCTION fn_cons_nss()

   DEFINE v_r_nss_correcto    SMALLINT

   OPEN WINDOW w_nss WITH FORM "OCGE031"
      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED, CANCEL=FALSE)
         ON ACTION ACCEPT
            IF v_nss IS NULL THEN
               CALL fn_captura_carta_instruccion(v_nss)
               INITIALIZE v_nss TO NULL 
            ELSE
               DISPLAY "INPUT NSS : ",v_nss 
               CALL fn_valida_nss(v_nss) RETURNING v_r_nss_correcto
               IF v_r_nss_correcto = TRUE THEN
                  CALL fn_captura_carta_instruccion(v_nss)
                  INITIALIZE v_nss TO NULL 
               END IF 
            END IF
         ON ACTION salir 
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_nss
END FUNCTION 


FUNCTION fn_captura_carta_instruccion(p_nss)
   DEFINE p_nss               CHAR(11)

   DEFINE v_genero            CHAR(1)
   DEFINE v_desc_genero       CHAR(10)
   DEFINE v_f_firma           DATE 
   DEFINE v_desc_ef           CHAR(60)
   DEFINE v_desc_moneda       CHAR(40)
   DEFINE v_desc_ent_inmueble CHAR(40)
   DEFINE v_respuesta         SMALLINT

   LET v_s_qry = " SELECT gf.id_ocg_formalizacion,
                          gd.nss,
                          TRIM(NVL(gf.ap_paterno,'')) ||' '|| TRIM(NVL(gf.ap_materno,'')) || ' ' || TRIM(NVL(gf.nombre,'')),
                          gf.rfc,
                          gf.curp,
                          gf.cve_ent_financiera,
                          gf.tpo_moneda,
                          gf.tasa_base,
                          gf.margen,
                          gf.monto_credito,
                          gf.valor_avaluo,
                          gf.tpo_credito,
                          gf.f_otorga_ent_fin,
                          gf.ent_fed_inmueble,
                          gf.mcpio_inmueble,
                          gf.id_derechohabiente,
                          gf.id_ocg_tramite
                     FROM ocg_formalizacion gf,
                          ocg_detalle gd
                    WHERE gd.id_derechohabiente = gf.id_derechohabiente
                      AND gd.id_ocg_detalle     = gf.id_ocg_detalle
                      AND gd.subproceso = '002'
                      AND gf.situacion = 50
                      AND gf.diagnostico = 1  " 

   PREPARE prp_cons_dat FROM v_s_qry

   LET v_s_qry = v_s_qry || " AND gd.nss = ? "
   PREPARE prp_cons_solo FROM v_s_qry

   OPEN WINDOW w_cons WITH FORM "OCGE032"
      IF p_nss IS NOT NULL THEN
      
         EXECUTE prp_cons_solo USING p_nss INTO v_rec_dat.*

         IF SQLCA.SQLCODE = 100 THEN
            CALL fn_mensaje("Alerta", "No se encontraron registros o el registro se encuentra rechazado","stop")
            EXIT PROGRAM
         END IF 
         
         IF v_rec_dat.ent_fed_inmueble IS NULL OR 
            v_rec_dat.ent_fed_inmueble = " " THEN

            LET v_rec_dat.ent_fed_inmueble = 0 
         END IF 

         -- Se extrae la posición 11 de la CURP, que es el genero
         LET v_genero = v_rec_dat.curp[11]

         IF v_genero = "" THEN
            LET v_genero = ""
            LET v_desc_genero = ""
         ELSE
            -- Se cambia la letra del genero para desplegarla en la forma 
            IF v_genero = "M" THEN
               LET v_genero = "F"
               LET v_desc_genero = "Femenino"
            ELSE 
               LET v_genero = "M"
               LET v_desc_genero = "Masculino"
            END IF
         END IF
         -- Se consulta la descripción de la entidad financiera 
         LET v_s_qry = " SELECT ent_financiera_desc
                           FROM cat_entidad_financiera
                          WHERE cve_ent_financiera = ? "
         PREPARE prp_cons_ef1 FROM v_s_qry
         EXECUTE prp_cons_ef1 INTO v_desc_ef 
                             USING v_rec_dat.id_ocg_entidad_financiera

         -- Se consulta la descripción del tipo de moneda
         LET v_s_qry = " SELECT desc_moneda
                           FROM cat_tipo_moneda
                          WHERE tpo_moneda = ? "
         PREPARE prp_cons_moneda1 FROM v_s_qry
         EXECUTE prp_cons_moneda1 INTO v_desc_moneda
                                 USING v_rec_dat.tpo_moneda

         -- Se consulta la descripcion de la entidad del inmueble
         LET v_s_qry = " SELECT entidad_desc_larga
                           FROM cat_entidad_federativa
                          WHERE entidad_federativa = ? "
         PREPARE prp_cons_ef_inm1 FROM v_s_qry
         EXECUTE prp_cons_ef_inm1 INTO v_desc_ent_inmueble
                                 USING v_rec_dat.ent_fed_inmueble

         DISPLAY BY NAME v_rec_dat.nss
         DISPLAY BY NAME v_rec_dat.nombre_af
         DISPLAY BY NAME v_rec_dat.rfc
         DISPLAY BY NAME v_rec_dat.curp
         DISPLAY BY NAME v_genero
         DISPLAY BY NAME v_rec_dat.tasa_base
         DISPLAY BY NAME v_rec_dat.margen
         DISPLAY BY NAME v_rec_dat.monto_credito
         DISPLAY BY NAME v_rec_dat.valor_avaluo
         DISPLAY BY NAME v_rec_dat.tpo_credito
         DISPLAY BY NAME v_rec_dat.f_otorga_ent_fed
         DISPLAY BY NAME v_rec_dat.mcpio_inmueble
         DISPLAY BY NAME v_desc_genero
         DISPLAY v_desc_ef               TO id_ocg_entidad_financiera
         DISPLAY v_desc_moneda           TO tpo_moneda
         DISPLAY v_rec_dat.monto_credito TO monto_pesos
         DISPLAY v_desc_ent_inmueble     TO ent_fed_inmueble

         CALL fn_llena_combo(v_rec_dat.ent_fed_inmueble)

         INPUT BY NAME v_genero,v_rec_dat.mcpio_inmueble,v_f_firma ATTRIBUTES(UNBUFFERED, CANCEL=FALSE )
            BEFORE INPUT 
               LET v_genero = v_desc_genero[1,1]
               DISPLAY BY NAME v_genero

            ON ACTION ACCEPT
               IF v_genero IS NULL OR v_rec_dat.mcpio_inmueble IS NULL OR v_f_firma IS NULL THEN
                  CALL fn_mensaje("","Favor de capturar los campos obligatorios","stop")
               ELSE
                  CALL fn_ventana_confirma("","¿Está seguro que desea formalizar la carta de instrucción?","") 
                  RETURNING v_respuesta
                  IF v_respuesta = 1 THEN
                     CALL fn_inserta_carta(v_f_firma,v_rec_dat.mcpio_inmueble,v_genero,v_rec_dat.nss,v_rec_dat.id_ocg_formalizacion)
                  ELSE
                     CALL fn_mensaje("","Se cancela formalización","")
                  END IF 
                  EXIT INPUT
               END IF 
            ON ACTION rechazar
               CALL fn_ventana_confirma("","¿Está seguro que desea rechazar la carta de instrucción?","") 
               RETURNING v_respuesta
               IF v_respuesta = 1 THEN  
                  CALL fn_rch_carta(v_rec_dat.id_ocg_formalizacion)
                  EXIT INPUT 
               ELSE
                  CALL fn_mensaje("","Se cancela rechazo de carta de instrucción","stop")
                  EXIT INPUT 
               END IF 

            ON ACTION salir
               EXIT INPUT
         END INPUT
      ELSE
         LET v_i_cnt = 1 

         DECLARE cur_cons CURSOR FOR prp_cons_dat

         FOREACH cur_cons INTO v_rec_dat.* 
            IF sqlca.sqlcode = 100 THEN
               CALL fn_mensaje("Alerta", "No se encontraron registros","stop")
            END IF
            
            DISPLAY "Entidad federativa :",v_rec_dat.ent_fed_inmueble

            -- Si la entidad federativa es nula se le asigna un cero 
            IF v_rec_dat.ent_fed_inmueble IS NULL OR 
               v_rec_dat.ent_fed_inmueble = " " THEN

               LET v_rec_dat.ent_fed_inmueble = 0

            END IF 

            LET v_genero = v_rec_dat.curp[11]

            IF v_genero = "" THEN
               LET v_genero = ""
               LET v_desc_genero = ""
            ELSE
               IF v_genero = "M" THEN
                  LET v_genero = "F"
                  LET v_desc_genero = "Femenino"
               ELSE 
                  LET v_genero = "M"
                  LET v_desc_genero = "Masculino"
               END IF 
            END IF
           -- Se consulta la descripción de la entidad financiera 
            LET v_s_qry = " SELECT ent_financiera_desc
                              FROM cat_entidad_financiera
                             WHERE cve_ent_financiera = ? "

            PREPARE prp_cons_ef FROM v_s_qry
            EXECUTE prp_cons_ef INTO v_desc_ef 
                               USING v_rec_dat.id_ocg_entidad_financiera

            -- Se consulta la descripción del tipo de moneda
            LET v_s_qry = " SELECT desc_moneda
                              FROM cat_tipo_moneda
                             WHERE tpo_moneda = ? "

            PREPARE prp_cons_moneda FROM v_s_qry
            EXECUTE prp_cons_moneda INTO v_desc_moneda
                                    USING v_rec_dat.tpo_moneda

            -- Se consulta la descripcion de la entidad del inmueble
            LET v_s_qry = " SELECT entidad_desc_larga
                              FROM cat_entidad_federativa
                             WHERE entidad_federativa = ? "

            PREPARE prp_cons_ef_inm FROM v_s_qry
            EXECUTE prp_cons_ef_inm INTO v_desc_ent_inmueble
                                   USING v_rec_dat.ent_fed_inmueble

            DISPLAY BY NAME v_rec_dat.nss
            DISPLAY BY NAME v_rec_dat.nombre_af
            DISPLAY BY NAME v_rec_dat.rfc
            DISPLAY BY NAME v_rec_dat.curp
            DISPLAY BY NAME v_genero
            DISPLAY BY NAME v_rec_dat.tasa_base
            DISPLAY BY NAME v_rec_dat.margen
            DISPLAY BY NAME v_rec_dat.monto_credito
            DISPLAY BY NAME v_rec_dat.valor_avaluo
            DISPLAY BY NAME v_rec_dat.tpo_credito
            DISPLAY BY NAME v_rec_dat.f_otorga_ent_fed            
            DISPLAY BY NAME v_rec_dat.mcpio_inmueble
            DISPLAY BY NAME v_desc_genero
            DISPLAY v_desc_ef               TO id_ocg_entidad_financiera
            DISPLAY v_desc_moneda           TO tpo_moneda
            DISPLAY v_rec_dat.monto_credito TO monto_pesos
            DISPLAY v_desc_ent_inmueble     TO ent_fed_inmueble

            CALL fn_llena_combo(v_rec_dat.ent_fed_inmueble)

            INPUT BY NAME v_genero,v_rec_dat.mcpio_inmueble,v_f_firma ATTRIBUTES(UNBUFFERED, CANCEL=FALSE )
               BEFORE INPUT 
                  LET v_genero = v_desc_genero[1,1]
                  DISPLAY BY NAME v_genero

               ON ACTION ACCEPT 
                  IF v_genero IS NULL OR v_rec_dat.mcpio_inmueble IS NULL OR v_f_firma IS NULL THEN
                     CALL fn_mensaje("","Favor de capturar los campos obligatorios","stop")
                  ELSE
                     CALL fn_ventana_confirma("","¿Está seguro que desea formalizar la carta de instrucción?","") 
                     RETURNING v_respuesta
                     IF v_respuesta = 1 THEN 
                        CALL fn_inserta_carta(v_f_firma,v_rec_dat.mcpio_inmueble,v_genero,v_rec_dat.nss,v_rec_dat.id_ocg_formalizacion)
                     ELSE
                        CALL fn_mensaje("","Se cancela formalización","")
                     END IF
                     CONTINUE FOREACH
                  END IF 

               ON ACTION rechazar
                  CALL fn_ventana_confirma("","¿Está seguro que desea rechazar la carta de instrucción?","") 
                  RETURNING v_respuesta
                  IF v_respuesta = 1 THEN  
                     CALL fn_rch_carta(v_rec_dat.id_ocg_formalizacion)
                     EXIT INPUT 
                  ELSE
                     CALL fn_mensaje("","Se cancela rechazo de carta de instrucción","stop")
                     EXIT INPUT 
                  END IF 
                  --EXIT INPUT
                  CONTINUE FOREACH

               ON ACTION siguiente
                  CONTINUE FOREACH

               ON ACTION salir
                  EXIT FOREACH
                  EXIT INPUT
            END INPUT  

         END FOREACH
      END IF
      
   CLOSE WINDOW w_cons
END FUNCTION 



FUNCTION fn_inserta_carta(p_f_firma,p_mcpio_inmueble,p_genero,p_nss,p_id_forma)
   DEFINE p_id_forma        DECIMAL(9,0)
   DEFINE p_nss             CHAR(11)
   DEFINE p_f_firma         DATE
   DEFINE p_mcpio_inmueble  DECIMAL(5,0)
   DEFINE p_genero          CHAR(1)
   DEFINE v_respuesta       SMALLINT 
   DEFINE v_msj_err         STRING
   DEFINE v_bnd_rch         BOOLEAN
   DEFINE v_ax_genero       CHAR(1)
   DEFINE v_id_ocg_detalle  DECIMAL(9,0)
   DEFINE v_f_proceso       DATE
   DEFINE p_r_nss           CHAR(11)
   DEFINE v_id_cre_acreditado DECIMAL(9,0)
   DEFINE v_error           SMALLINT
   DEFINE v_f_vigencia      DATE
   DEFINE v_id_tramite      DECIMAL(9,0)

   LET v_f_proceso = TODAY
   LET v_bnd_rch = 0

   --CALL fn_ventana_confirma("Alerta","¿Está seguro que desea formalizar?","information")
   --RETURNING v_respuesta

   LET v_respuesta = 1

   IF v_respuesta = 1 THEN 
      -- Se valida la información ingresada
      IF p_f_firma IS NULL OR p_mcpio_inmueble IS NULL OR p_genero IS NULL THEN
         LET v_bnd_rch = 1
      END IF 

      SELECT DECODE(curp[11],"M","F","H","M")
        INTO v_ax_genero
        FROM afi_derechohabiente
       WHERE nss = p_nss

      IF p_genero == 'M' OR p_genero == 'F' THEN
         LET v_bnd_rch = 0
         IF v_ax_genero = "" THEN
            LET v_bnd_rch = 0
         ELSE
            IF p_genero <> v_ax_genero THEN
               LET v_bnd_rch = 1
            END IF
         END IF
         --LET v_msj_err = "El genero ingresado no es válido"
      ELSE 
        LET v_bnd_rch = 1 
      END IF 

      IF v_bnd_rch = 0 THEN

         
      -- Validaci?n de relaci?n laboral
      SELECT count(*)
        INTO v_cnt_rl
        FROM afi_derechohabiente ad ,
             afi_relacion_laboral arl
       WHERE ad.nss = v_nss
         AND ad.id_derechohabiente = arl.id_derechohabiente

         IF v_cnt_rl >= 1THEN
            LET v_bnd = 1
         ELSE
            SELECT id_ocg_tramite
              INTO v_id_tramite
              FROM ocg_formalizacion
            WHERE id_ocg_formalizacion = p_id_forma

            SELECT f_vigencia
              INTO v_f_vigencia
              FROM ocg_tramite
             WHERE id_ocg_tramite = v_id_tramite

             IF v_f_vigencia >= TODAY THEN
               LET v_bnd = 1
             ELSE
                LET v_bnd = 0
             END IF
         END IF

         IF v_bnd = 0 THEN

          --TRACE 'No tiene relacion laboral nss : ' || v_nss;
            INSERT INTO ocg_inconsistencia
                 VALUES( p_id_forma,
                         2,
                         13,
                         TODAY );

            LET v_s_qry = "SELECT id_ocg_detalle
                             FROM ocg_formalizacion
                            WHERE id_ocg_formalizacion = ?"

            PREPARE prp_cons_detalle1 FROM v_s_qry
            EXECUTE prp_cons_detalle1 INTO v_id_ocg_detalle
                                    USING p_id_forma 

            LET v_s_qry = "UPDATE ocg_detalle
                              SET f_proceso = ?
                            WHERE id_ocg_detalle = ?"

            PREPARE prp_act_ocg_detalle1 FROM v_s_qry
            EXECUTE prp_act_ocg_detalle1 USING v_f_proceso,
                                              v_id_ocg_detalle

            --se actualiza el estado del registro 
            LET v_s_qry = " UPDATE ocg_formalizacion
                               SET situacion = 40,
                                   estado    = 60
                             WHERE id_ocg_formalizacion = ? "

            PREPARE prp_upd_edo1 FROM v_s_qry
            EXECUTE prp_upd_edo1 USING p_id_forma

            -- se actualiza situacion en ocg_tramite a 60 (acreditado)

            LET v_s_qry = " UPDATE ocg_tramite
                               SET situacion = 40,
                                   estado    = 60
                             WHERE id_ocg_tramite = (SELECT id_ocg_tramite
                                                       FROM ocg_formalizacion
                                                      WHERE id_ocg_formalizacion = ?)"
            PREPARE prp_upd_tmte1 FROM v_s_qry
            EXECUTE prp_upd_tmte1 USING p_id_forma

            CALL fn_mensaje("Aviso","Formalización rechazada por fecha de vigencia vencida","")
         END IF

      END IF

      IF v_bnd = 1 THEN
         LET v_s_qry = " UPDATE ocg_formalizacion
                            SET f_registro_carta  = ?,
                                mcpio_inmueble    = ?,
                                usuario_reg_carta = ?,
                                genero            = ?
                          WHERE id_ocg_formalizacion = ?
                            AND id_derechohabiente IN (
                                SELECT id_derechohabiente 
                                  FROM afi_derechohabiente
                                 WHERE nss = ? )"

         PREPARE prp_carta FROM v_s_qry
         EXECUTE prp_carta USING p_f_firma,
                                 p_mcpio_inmueble,
                                 p_usuario_cod,
                                 p_genero,
                                 p_id_forma,
                                 p_nss

         IF SQLCA.SQLCODE = 0 THEN
            LET v_s_qry = "SELECT id_ocg_detalle
                             FROM ocg_formalizacion
                            WHERE id_ocg_formalizacion = ?"

            PREPARE prp_cons_detalle FROM v_s_qry
            EXECUTE prp_cons_detalle INTO v_id_ocg_detalle
                                    USING p_id_forma 

            LET v_s_qry = "UPDATE ocg_detalle
                              SET f_proceso = ?
                            WHERE id_ocg_detalle = ?"

            PREPARE prp_act_ocg_detalle FROM v_s_qry
            EXECUTE prp_act_ocg_detalle USING v_f_proceso,
                                              v_id_ocg_detalle

            --se actualiza el estado del registro 
            LET v_s_qry = " UPDATE ocg_formalizacion
                               SET situacion = 55
                             WHERE id_ocg_formalizacion = ? "

            PREPARE prp_upd_edo FROM v_s_qry
            EXECUTE prp_upd_edo USING p_id_forma

            -- se actualiza situacion en ocg_tramite a 60 (acreditado)

            LET v_s_qry = " UPDATE ocg_tramite
                               SET situacion = 60
                             WHERE id_ocg_tramite = (SELECT id_ocg_tramite
                                                       FROM ocg_formalizacion
                                                      WHERE id_ocg_formalizacion = ?)"
            PREPARE prp_upd_tmte FROM v_s_qry
            EXECUTE prp_upd_tmte USING p_id_forma

--******************************************************************************
                     LET v_cmd = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
                     PREPARE prp_cmd FROM v_cmd 
                     EXECUTE prp_cmd USING v_rec_dat.id_derechohabiente,
                                             "206",
                                             v_rec_dat.id_ocg_tramite,
                                             "0",
                                             "0",
                                             p_usuario_cod,
                                             "3903" INTO r_cod_rechazo 
--******************************************************************************

--******************************************************************************
            -- se actualiza estado en ocg_formalización a 20 (acreditado)

            LET v_s_qry = " UPDATE ocg_formalizacion
                               SET estado = 20
                             WHERE id_ocg_tramite = (SELECT id_ocg_tramite
                                                       FROM ocg_formalizacion
                                                      WHERE id_ocg_formalizacion = ?)"
            PREPARE prp_upd_f FROM v_s_qry
            EXECUTE prp_upd_f USING p_id_forma

             LET v_s_qry = " UPDATE ocg_tramite
                               SET estado = 20
                             WHERE id_ocg_tramite = (SELECT id_ocg_tramite
                                                       FROM ocg_formalizacion
                                                      WHERE id_ocg_formalizacion = ?)"
            PREPARE prp_upd_t FROM v_s_qry
            EXECUTE prp_upd_t USING p_id_forma

            INSERT INTO ocg_acreditado
                 VALUES ( p_id_forma,
                          TODAY,      
                          "",
                          "",
                          "",
                          "",
                          "",
                          "",
                          20,
                          55)


            LET v_s_qry = " EXECUTE FUNCTION fn_ocg_originacion(?,?,?)"

            PREPARE prp_fn_origina FROM v_s_qry
            EXECUTE prp_fn_origina USING v_nss,
                                         v_rec_dat.id_derechohabiente,
                                         v_rec_dat.id_ocg_formalizacion
                                    INTO v_error,
                                         p_r_nss,
                                         v_id_cre_acreditado

--*******************************************************************************
            CALL fn_mensaje("Aviso","Formalización exitosa","")
         ELSE
            CALL fn_mensaje("Error","Error al aceptar la carta de instrucción","stop")
         END IF
      ELSE 
         CALL fn_mensaje("Aviso","Los datos ingresados son incorrectos","stop")
      END IF 
   ELSE 
      EXIT PROGRAM
   END IF 
END FUNCTION


FUNCTION  fn_rch_carta(p_id_ocg_formalizacion)
   DEFINE p_id_ocg_formalizacion DECIMAL(9,0)
   DEFINE v_sp_respuesta         SMALLINT

   LET v_sp_respuesta = 1 

   -- Se actualiza el estado a rechazado 
   LET v_s_qry = "EXECUTE FUNCTION fn_rch_carta_formalizacion(?)"

   PREPARE prp_rechazo FROM v_s_qry
   EXECUTE prp_rechazo USING p_id_ocg_formalizacion
                        INTO v_sp_respuesta

   IF v_sp_respuesta = 0 THEN
      CALL fn_mensaje("","Carta de Instrucción Rechazada","")
   ELSE
      CALL fn_mensaje("","Ocurrió un error al rechazar la Carta de Instrucción","")
   END IF 
END FUNCTION 


-- Función para validar nss
FUNCTION fn_valida_nss(p_nss_modificado)
   DEFINE p_nss_modificado    LIKE afi_derechohabiente.nss
   DEFINE v_nss               STRING    -- cadena con el NSS,
   DEFINE v_mensaje           STRING    -- mensaje para el usuario
   DEFINE v_indice            SMALLINT  -- indice pivote
   DEFINE v_nss_es_correcto   SMALLINT  -- booleana que indica si un NSS esta correctamente construido

   LET v_nss = p_nss_modificado CLIPPED

   -- se asume que el NSS esta correcto
   LET v_nss_es_correcto = TRUE
   
   -- NSS debe ser de 11 digitos
   IF ( v_nss.getLength() <> 11 ) THEN
      LET v_mensaje = "La longitud del NSS debe ser de 11 dígitos"
      LET v_nss_es_correcto = FALSE
   ELSE
      -- se verifica que todos los caracteres sean numericos
      FOR v_indice = 1 TO v_nss.getLength()
         IF ( v_nss.getCharAt(v_indice) < "0" OR v_nss.getCharAt(v_indice) > "9" ) THEN
            LET v_mensaje = "El NSS contiene caracteres no numéricos."
            LET v_nss_es_correcto = FALSE
            EXIT FOR
         END IF
      END FOR
   END IF

   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF ( NOT v_nss_es_correcto ) THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_nss_es_correcto
END FUNCTION

FUNCTION fn_llena_combo(p_ent_federativa)
   DEFINE p_ent_federativa    SMALLINT
   DEFINE v_rec_municipio     RECORD
      municipio               SMALLINT,
      municipio_desc          CHAR(40)
   END RECORD
   DEFINE v_ax_mcpio          CHAR(5)
   DEFINE v_ax_sm_mcpio       SMALLINT

   LET cb = ui.ComboBox.forName("mcpio_inmueble")

   LET v_s_qry = "SELECT municipio,
                         municipio_desc
                    FROM cat_municipio_inegi
                   WHERE entidad_federativa = ?
                   ORDER BY municipio"

   PREPARE prp_mcpio FROM v_s_qry
   DECLARE cur_mcpio CURSOR FOR prp_mcpio

   -- limpia el combo , no mover de aqui
   CALL cb.clear()

   FOREACH cur_mcpio USING p_ent_federativa INTO v_rec_municipio.*
      LET v_ax_mcpio = v_rec_municipio.municipio

      IF LENGTH(v_ax_mcpio) = 5 THEN
         LET v_ax_mcpio = v_ax_mcpio[3,5]
      ELSE
         LET v_ax_mcpio = v_ax_mcpio[2,4]
      END IF

      LET v_rec_municipio.municipio = v_ax_mcpio
      LET v_ax_sm_mcpio = v_rec_municipio.municipio USING "&&&"

      --LET v_rec_municipio.municipio_desc = v_rec_municipio.municipio ||" - "|| v_rec_municipio.municipio_desc
      LET v_rec_municipio.municipio_desc = v_ax_mcpio ||" - "|| v_rec_municipio.municipio_desc

      CALL cb.addItem(v_rec_municipio.municipio,v_rec_municipio.municipio_desc)
   END FOREACH
END FUNCTION



