#################################################################################
#Modulo              => OCG                                                     #
#Programa            => OCGC13                                                  #
#Objetivo            => PANTALLA DE CONSULTA PARA MIGRACIÓN DE ACREDITADOS      #
#                       HISTÓRICOS 43BIS                                        #
#Autor               => JOSÉ EDUARDO VENTURA                                    #
#Fecha inicio        => 16 de ENERO del 2019                                    #
#################################################################################

DATABASE safre_viv

   DEFINE p_usuario                 CHAR(20)
   DEFINE p_s_titulo                CHAR(20)
   DEFINE p_tpo_ejecucion           SMALLINT
   DEFINE v_e_nss                   CHAR(11)
   DEFINE v_cve_ef                  SMALLINT
   DEFINE v_fecha_proceso           DATE
   DEFINE v_e_situacion             CHAR(1)
   DEFINE v_tpo_credito             CHAR(2)
   DEFINE v_e_subproceso            SMALLINT
   DEFINE v_cadena                  STRING
   DEFINE v_pos                     INTEGER

   DEFINE v_arr_cons_acreditado     DYNAMIC ARRAY OF RECORD
      id_his_acreditado                DECIMAL(9,0),
      situacion                     CHAR(1), -- Anteriormente estado
      subproceso                    CHAR(3),
      cve_ent_financiera            CHAR(3),
      f_proceso                     CHAR(8),
      nss                           CHAR(11),
      tipo_credito                  CHAR(3),
      nombre                        CHAR(80)
   END RECORD

   DEFINE v_ejercicio             CHAR(4)
   DEFINE v_f_subcta              CHAR(8)
   DEFINE v_97                    CHAR(12)
   DEFINE v_bimestre              CHAR(6)
   DEFINE v_genero                CHAR(2)
   DEFINE v_nombre                CHAR(40)
   DEFINE v_materno               CHAR(40)
   DEFINE v_paterno               CHAR(40)
   DEFINE v_marca_asoc            CHAR(3)
   DEFINE v_nss_asoc              CHAR(11)
   DEFINE v_ctr_ef                CHAR(18)
   DEFINE v_curp                  CHAR(18)
   DEFINE v_rfc                   CHAR(13)
   DEFINE v_nss                   CHAR(11)
   DEFINE v_f_liquida_cofi        CHAR(8)
   DEFINE v_causa_liquida         CHAR(2)
   DEFINE v_f_credito             CHAR(8)
   DEFINE v_f_proceso             CHAR(8)
   DEFINE v_formalizacion         CHAR(8)
   DEFINE v_marca                 CHAR(3)
   DEFINE v_producto              CHAR(3)
   DEFINE v_ent_financiera        CHAR(3)
   DEFINE v_subproceso            CHAR(3)
   DEFINE v_situacion             CHAR(3)
   DEFINE v_id_his_acreditado     DECIMAL(9,0)
   DEFINE v_tpo_registro          char(3)
   DEFINE v_seccion               CHAR( 6)
   DEFINE v_tomo                  CHAR(6)
   DEFINE v_libro                 CHAR(6)
   DEFINE v_volumen               CHAR(6)
   DEFINE v_foja                  CHAR(8)
   DEFINE v_partida               CHAR(6)
   DEFINE v_folio                 CHAR(8)
   DEFINE v_rpp                   CHAR(15)
   DEFINE v_escritura             CHAR(8)
   DEFINE v_ent_fed_notario       CHAR(2)
   DEFINE v_edo_notario           CHAR(3)
   DEFINE v_notario               CHAR(4)
   DEFINE v_ent_inmueble          CHAR(2)
   DEFINE v_domicilio             CHAR(30)
   DEFINE v_avaluo                CHAR(12)
   DEFINE v_monto_credito         CHAR(12)
   DEFINE v_plazo_credito         CHAR(5)
   DEFINE v_moneda                CHAR(2)
   DEFINE v_tasa                  CHAR(20)
   DEFINE v_margen                CHAR(20)
   DEFINE v_oto_ef                CHAR(8)
   DEFINE v_reg_carta             CHAR(8)
   DEFINE v_usuario_reg           CHAR(20)
   DEFINE v_tpo_saldo1            char(2)
   DEFINE v_tpo_saldo2            char(2)
   DEFINE v_tpo_saldo3            char(2)
   DEFINE v_tpo_saldo4            char(2)
   DEFINE v_traspaso92            CHAR(12)
   DEFINE v_traspaso97            CHAR(12)
   DEFINE v_f_traspaso            CHAR(8)
   DEFINE v_subcta92              CHAR(12)
   DEFINE v_subcta97              CHAR(12)
   DEFINE v_ssv                   CHAR(12)
   DEFINE v_saldo1                CHAR(12)
   DEFINE v_saldo2                CHAR(12)
   DEFINE v_saldo3                CHAR(12)
   DEFINE v_saldo4                CHAR(12)
   DEFINE v_f_sol_mca_prcr        CHAR(8)
   DEFINE v_f_conf_mca_prcr       CHAR(8)
   DEFINE v_sol_traspaso          CHAR(8)
   DEFINE v_sol_ap                CHAR(8)
   DEFINE v_traspaso_ap           CHAR(8)
   DEFINE v_f_sol_desm_prcr       CHAR(8)
   DEFINE v_f_conf_desm_prcr      CHAR(8)
   DEFINE v_causa                 CHAR(3)
   DEFINE v_f_autorizacion        CHAR(8)
   DEFINE v_f_saldo1              CHAR(8)
   DEFINE v_f_saldo2              CHAR(8)
   DEFINE v_f_saldo3              CHAR(8)
   DEFINE v_f_saldo4              CHAR(8)
   DEFINE v_f_subcuenta           char(8)
   DEFINE v_tpo_credito1          char(3)
   DEFINE v_tpo_credito2          char(3)

MAIN
      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC16.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW busqueda WITH FORM "OCGC160"

   INPUT BY NAME v_e_nss,
                 v_e_subproceso,
                 v_cve_ef,
                 v_fecha_proceso,
                 v_e_situacion,
                 v_tpo_credito  ATTRIBUTES(UNBUFFERED)


   ON ACTION ACCEPT

   LET v_cadena = " "

   IF v_e_nss IS NOT NULL THEN
      LET v_cadena = v_cadena," AND h.nss = '",v_e_nss,"'"
   END IF

   IF v_e_subproceso IS NOT NULL THEN
      LET v_cadena = v_cadena," AND h.subproceso  = '",v_e_subproceso,"'"
   END IF

   IF v_cve_ef IS NOT NULL THEN
      LET v_cadena = v_cadena," AND h.ent_financiera = '",v_cve_ef,"'"
   END IF

   IF v_fecha_proceso IS NOT NULL THEN
      LET v_cadena = v_cadena," AND h.f_proceso   = '",v_fecha_proceso,"'"
   END IF

   IF v_e_situacion IS NOT NULL THEN
      LET v_cadena = v_cadena," AND h.situacion   = '",v_e_situacion,"'"
   END IF

   IF v_tpo_credito IS NOT NULL THEN
      LET v_cadena = v_cadena," AND h.tipo_credito1 = '",v_tpo_credito,"'"
   END IF

   DISPLAY v_cadena

   CALL fn_consulta_detalle()

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT

   CLOSE WINDOW busqueda

END MAIN

FUNCTION fn_consulta_detalle()

   DEFINE v_query                   STRING
   DEFINE a                         SMALLINT

   LET v_query = "SELECT first 32000 
                         h.id_his_acreditado,
                         h.situacion,
                         h.subproceso,
                         h.ent_financiera,
                         h.f_proceso,
                         h.nss,
                         h.nci_ef,
                         TRIM(h.ap_paterno)||' '||TRIM(h.ap_materno)||' '||TRIM(h.nombre)     
                    FROM safre_tmp:tmp_his_acreditado_ocg h
                   WHERE 1 = 1 ",v_cadena

   PREPARE prp_cons_acreditado FROM v_query
   DECLARE cur_cons_acreditado CURSOR FOR prp_cons_acreditado

   LET a = 1

   FOREACH cur_cons_acreditado INTO v_arr_cons_acreditado[a].id_his_acreditado,
                                    v_arr_cons_acreditado[a].situacion,
                                    v_arr_cons_acreditado[a].subproceso,
                                    v_arr_cons_acreditado[a].cve_ent_financiera,
                                    v_arr_cons_acreditado[a].f_proceso,
                                    v_arr_cons_acreditado[a].nss,
                                    v_arr_cons_acreditado[a].tipo_credito,
                                    v_arr_cons_acreditado[a].nombre
      LET a = a + 1
   END FOREACH

   CALL v_arr_cons_acreditado.deleteElement(v_arr_cons_acreditado.getLength())

   IF v_arr_cons_acreditado.getLength() <= 0 THEN
      CALL fn_mensaje("Alerta","No se encontraron registros","stop")
   ELSE
      OPEN WINDOW w_consulta WITH FORM "OCGC161"
      DISPLAY ARRAY v_arr_cons_acreditado TO arr_tabla1.* ATTRIBUTES(ACCEPT=FALSE,CANCEL=FALSE)

         ON ACTION ACCEPT
            CALL fn_limpia_variables()
            LET v_pos = ARR_CURR()
            CALL fn_carga_datos(v_pos)
            -- inicia pantalla 3
            OPEN WINDOW w_1 WITH FORM "OCGC162"
            CALL fn_despliega_1()
            MENU
               ON ACTION siguiente
                  --inicia pantalla 4
                  OPEN WINDOW w_2 WITH FORM "OCGC163"
                  CALL fn_despliega_2()
                  MENU
                     ON ACTION siguiente
                        --inicia pantalla 5
                        OPEN WINDOW w_3 WITH FORM "OCGC164"
                        CALL fn_despliega_3()
                        MENU
                           ON ACTION anterior
                           EXIT MENU
                        END MENU
                        CLOSE WINDOW w_3
                     ON ACTION anterior
                        EXIT MENU
                  END MENU
                  CLOSE WINDOW w_2
                  --finaliza pantalla 4
               ON ACTION anterior
               EXIT MENU
            END MENU
            CLOSE WINDOW w_1

         ON ACTION CANCEL
            EXIT DISPLAY
      END DISPLAY

      CLOSE WINDOW w_consulta

   END IF

END FUNCTION

FUNCTION fn_despliega_1()

   DISPLAY BY NAME v_ejercicio
   DISPLAY BY NAME v_f_subcta
   DISPLAY BY NAME v_97
   DISPLAY BY NAME v_bimestre
   DISPLAY BY NAME v_genero
   DISPLAY BY NAME v_nombre
   DISPLAY BY NAME v_materno
   DISPLAY BY NAME v_paterno
   DISPLAY BY NAME v_marca_asoc
   DISPLAY BY NAME v_nss_asoc
   DISPLAY BY NAME v_ctr_ef
   DISPLAY BY NAME v_curp
   DISPLAY BY NAME v_rfc
   DISPLAY BY NAME v_nss
   DISPLAY BY NAME v_f_liquida_cofi
   DISPLAY BY NAME v_causa_liquida
   DISPLAY BY NAME v_f_credito
   DISPLAY BY NAME v_f_proceso
   DISPLAY BY NAME v_formalizacion
   DISPLAY BY NAME v_marca
   DISPLAY BY NAME v_producto
   DISPLAY BY NAME v_ent_financiera
   DISPLAY BY NAME v_subproceso
   DISPLAY BY NAME v_situacion

END FUNCTION

FUNCTION fn_despliega_2()

   DISPLAY BY NAME v_seccion
   DISPLAY BY NAME v_tomo
   DISPLAY BY NAME v_libro
   DISPLAY BY NAME v_volumen
   DISPLAY BY NAME v_foja
   DISPLAY BY NAME v_partida
   DISPLAY BY NAME v_folio
   DISPLAY BY NAME v_rpp
   DISPLAY BY NAME v_escritura
   DISPLAY BY NAME v_ent_fed_notario
   DISPLAY BY NAME v_edo_notario
   DISPLAY BY NAME v_notario
   DISPLAY BY NAME v_ent_inmueble
   DISPLAY BY NAME v_domicilio
   DISPLAY BY NAME v_avaluo
   DISPLAY BY NAME v_monto_credito
   DISPLAY BY NAME v_plazo_credito
   DISPLAY BY NAME v_moneda
   DISPLAY BY NAME v_tasa
   DISPLAY BY NAME v_margen
   DISPLAY BY NAME v_oto_ef
   DISPLAY BY NAME v_reg_carta
   DISPLAY BY NAME v_usuario_reg

END FUNCTION

FUNCTION fn_despliega_3()

   DISPLAY BY NAME v_traspaso92
   DISPLAY BY NAME v_traspaso97
   DISPLAY BY NAME v_f_traspaso
   DISPLAY BY NAME v_subcta92
   DISPLAY BY NAME v_subcta97
   DISPLAY BY NAME v_ssv
   DISPLAY BY NAME v_saldo1
   DISPLAY BY NAME v_saldo2
   DISPLAY BY NAME v_saldo3
   DISPLAY BY NAME v_saldo4
   DISPLAY BY NAME v_f_sol_mca_prcr
   DISPLAY BY NAME v_f_conf_mca_prcr
   DISPLAY BY NAME v_sol_traspaso
   DISPLAY BY NAME v_sol_ap
   DISPLAY BY NAME v_traspaso_ap
   DISPLAY BY NAME v_f_sol_desm_prcr
   DISPLAY BY NAME v_f_conf_desm_prcr
   DISPLAY BY NAME v_causa
   DISPLAY BY NAME v_f_autorizacion
   DISPLAY BY NAME v_f_saldo1
   DISPLAY BY NAME v_f_saldo2
   DISPLAY BY NAME v_f_saldo3
   DISPLAY BY NAME v_f_saldo4

END FUNCTION

FUNCTION fn_carga_datos(v_pos)

   DEFINE v_pos  SMALLINT
   DEFINE v_id DECIMAL(9,0)

   LET v_id = v_arr_cons_acreditado[v_pos].id_his_acreditado

   SELECT *
     INTO v_id_his_acreditado,
          v_tpo_registro,
          v_marca,
          v_subproceso,
          v_f_proceso,
          v_situacion,
          v_ent_financiera,
          v_nss,
          v_ctr_ef,
          v_rfc,
          v_paterno,
          v_materno,
          v_nombre,
          v_97,
          v_f_subcuenta,
          v_traspaso97,
          v_traspaso92,
          v_subcta97,
          v_escritura,
          v_notario,
          v_ent_fed_notario,
          v_edo_notario,
          v_rpp,
          v_folio,
          v_partida,
          v_foja,
          v_volumen,
          v_libro,
          v_tomo,
          v_seccion,
          v_ent_inmueble,
          v_domicilio,
          v_avaluo,
          v_monto_credito,
          v_plazo_credito,
          v_moneda,
          v_tasa,
          v_margen,
          v_f_credito,
          v_reg_carta,
          v_usuario_reg,
          v_formalizacion,
          v_subproceso,
          v_f_sol_mca_prcr,
          v_f_conf_mca_prcr,
          v_sol_traspaso,
          v_sol_ap,
          v_traspaso_ap,
          v_causa,
          v_ejercicio,
          v_f_autorizacion,
          v_tpo_saldo1,
          v_saldo1,
          v_f_saldo1,
          v_tpo_saldo2,
          v_saldo2,
          v_f_saldo2,
          v_tpo_saldo3,
          v_saldo3,
          v_f_saldo3,
          v_tpo_saldo4,
          v_saldo4,
          v_f_saldo4,
          v_f_liquida_cofi,
          v_f_sol_desm_prcr,
          v_f_conf_desm_prcr,
          v_tpo_credito1,
          v_tpo_credito2,
          v_nss_asoc
     FROM safre_tmp:tmp_his_acreditado_ocg
    WHERE id_his_acreditado = v_id

END FUNCTION

FUNCTION fn_limpia_variables()

   LET v_ejercicio        = NULL
   LET v_f_subcta         = NULL
   LET v_97               = NULL
   LET v_bimestre         = NULL
   LET v_genero           = NULL
   LET v_nombre           = NULL
   LET v_materno          = NULL
   LET v_paterno          = NULL
   LET v_marca_asoc       = NULL
   LET v_nss_asoc         = NULL
   LET v_ctr_ef           = NULL
   LET v_curp             = NULL
   LET v_rfc              = NULL
   LET v_nss              = NULL
   LET v_f_liquida_cofi   = NULL
   LET v_causa_liquida    = NULL
   LET v_f_credito        = NULL
   LET v_f_proceso        = NULL
   LET v_formalizacion    = NULL
   LET v_marca            = NULL
   LET v_producto         = NULL
   LET v_ent_financiera   = NULL
   LET v_subproceso       = NULL
   LET v_situacion        = NULL
   LET v_seccion          = NULL
   LET v_tomo             = NULL
   LET v_libro            = NULL
   LET v_volumen          = NULL
   LET v_foja             = NULL
   LET v_partida          = NULL
   LET v_folio            = NULL
   LET v_rpp              = NULL
   LET v_escritura        = NULL
   LET v_ent_fed_notario  = NULL
   LET v_edo_notario      = NULL
   LET v_notario          = NULL
   LET v_ent_inmueble     = NULL
   LET v_domicilio        = NULL
   LET v_avaluo           = NULL
   LET v_monto_credito    = NULL
   LET v_plazo_credito    = NULL
   LET v_moneda           = NULL
   LET v_tasa             = NULL
   LET v_margen           = NULL
   LET v_oto_ef           = NULL
   LET v_reg_carta        = NULL
   LET v_usuario_reg      = NULL
   LET v_traspaso92       = NULL
   LET v_traspaso97       = NULL
   LET v_f_traspaso       = NULL
   LET v_subcta92         = NULL
   LET v_subcta97         = NULL
   LET v_ssv              = NULL
   LET v_saldo1           = NULL
   LET v_saldo2           = NULL
   LET v_saldo3           = NULL
   LET v_saldo4           = NULL
   LET v_f_sol_mca_prcr   = NULL
   LET v_f_conf_mca_prcr  = NULL
   LET v_sol_traspaso     = NULL
   LET v_sol_ap           = NULL
   LET v_traspaso_ap      = NULL
   LET v_f_sol_desm_prcr  = NULL
   LET v_f_conf_desm_prcr = NULL
   LET v_causa            = NULL
   LET v_f_autorizacion   = NULL
   LET v_f_saldo1         = NULL
   LET v_f_saldo2         = NULL
   LET v_f_saldo3         = NULL
   LET v_f_saldo4         = NULL

END FUNCTION