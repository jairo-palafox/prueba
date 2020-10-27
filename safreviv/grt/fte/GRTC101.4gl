################################################################################
#Modulo              => GRT                                                    #
#Programa            => GRTC101                                                #
#Objetivo            => Programa que realiza la consulta general               #
#                       de ubicación de registros Garantía 43 bis              #
#Autor               => Héctor F. Jiménez Lara                                 #
#Fecha inicio        => 23 Octubre 2015                                        #
################################################################################
DATABASE safre_viv

   DEFINE p_bnd_sp                  CHAR(3)
   DEFINE v_edo_desc                CHAR(40)
   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod
   DEFINE v_cnt                     SMALLINT   
   DEFINE v_s_qry                   STRING
   DEFINE v_arr_inconsistencia DYNAMIC ARRAY OF RECORD
      inconsistencia                CHAR(2),
      descripcion                   CHAR(80)
   END RECORD
   
   CONSTANT v_id_maq_tramite   = 4
   CONSTANT v_id_maq_formaliza = 5

MAIN
   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo                STRING   -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET p_bnd_sp         = ARG_VAL(4)

   DISPLAY "BANDERA SP : ",p_bnd_sp

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTC101.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se invoca la función de consulta
   CALL fn_cons_ubicacion_registros_nss(p_bnd_sp)
END MAIN



FUNCTION fn_cons_ubicacion_registros_nss(v_bnd_sp)
   DEFINE v_bnd_correcto            BOOLEAN
   DEFINE v_tpo_credito             CHAR(1)
   DEFINE v_bnd_sp                  CHAR(3)
   DEFINE v_subproceso              CHAR(3)
   DEFINE v_nss                     CHAR(11)
   DEFINE v_f_proceso               DATE
   DEFINE v_f_carga                 DATE
   DEFINE v_cve_ent_financiera      SMALLINT  
   DEFINE v_situacion               SMALLINT 
   DEFINE v_forma                   ui.Form
   DEFINE v_window                  ui.Window

   OPEN WINDOW w1 WITH FORM "GRTC1011"

      -- Sí la ejecución proviene de un SP se oculta el campo subproceso
      IF v_bnd_sp IS NOT NULL THEN 
         LET v_window = ui.Window.getCurrent()
         LET v_forma = v_window.getForm()

         CALL v_forma.setFieldHidden("v_subproceso",1)
         CALL v_forma.setElementHidden("lb_subproceso",1)
      END IF

      INPUT BY NAME v_nss,
                    v_subproceso,
                    v_cve_ent_financiera,
                    v_f_proceso,
                    v_f_carga,
                    v_situacion,
                    v_tpo_credito ATTRIBUTES (UNBUFFERED)

         ON ACTION ACCEPT
            DISPLAY v_nss,v_subproceso,v_cve_ent_financiera,v_f_proceso,v_f_carga,v_situacion,v_tpo_credito

            IF v_nss IS NOT NULL THEN
               CALL fn_valida_nss(v_nss) RETURNING v_bnd_correcto
               IF v_bnd_correcto = TRUE THEN 
                  CALL fn_cons_ubicacion_registros(v_nss,
                                                   v_subproceso,
                                                   v_cve_ent_financiera,
                                                   v_f_proceso,
                                                   v_f_carga,
                                                   v_situacion,
                                                   v_tpo_credito,
                                                   v_bnd_sp)
               END IF
            ELSE
               CALL fn_cons_ubicacion_registros(v_nss,
                                                v_subproceso,
                                                v_cve_ent_financiera,
                                                v_f_proceso,
                                                v_f_carga,
                                                v_situacion,
                                                v_tpo_credito,
                                                v_bnd_sp)
            END IF
      END INPUT

   CLOSE WINDOW w1
END FUNCTION



FUNCTION fn_cons_ubicacion_registros( p_nss,
                                      p_subproceso,
                                      p_cve_ent_financiera,
                                      p_f_proceso,
                                      p_f_carga,
                                      p_situacion,
                                      p_tpo_credito,
                                      p_bnd_sp )

   DEFINE p_tpo_credito             CHAR(1)
   DEFINE p_subproceso              CHAR(3)
   DEFINE p_bnd_sp                  CHAR(3)
   DEFINE p_nss                     CHAR(11)
   DEFINE v_ax_fecha                CHAR(15)
   DEFINE p_f_proceso               DATE
   DEFINE p_f_carga                 DATE
   DEFINE p_cve_ent_financiera      SMALLINT  
   DEFINE p_situacion               SMALLINT 
   DEFINE v_ax_cnt                  SMALLINT 
   DEFINE v_bnd_nulos               SMALLINT
   DEFINE v_pos                     SMALLINT
   DEFINE v_desc_edo                SMALLINT
   DEFINE v_s_condicion             STRING
   DEFINE v_cadena_aux              STRING
   DEFINE v_arr_consulta DYNAMIC ARRAY OF RECORD
      id_derechohabiente            DECIMAL(9,0),
      id_detalle                    DECIMAL(9,0),
      id                            DECIMAL(9,0),
      situacion                     SMALLINT, -- Anteriormente estado
      subproceso                    SMALLINT, --CHAR(3),
      cve_ent_financiera            SMALLINT,
      diagnostico                   SMALLINT, --CHAR(2),
      f_proceso                     DATE,
      nss                           CHAR(11),
      rfc                           CHAR(13),
      nombre                        VARCHAR(80)
   END RECORD
   DEFINE v_arr_cons_tabla DYNAMIC ARRAY OF RECORD
      nom_tabla                     LIKE cat_tabla_subproceso.nom_tabla,
      subproceso                    LIKE cat_tabla_subproceso.subproceso,
      alias                         LIKE cat_tabla_subproceso.alias,
      identificador                 LIKE cat_tabla_subproceso.identificador
   END RECORD

   LET v_cnt        = 1
   LET v_ax_cnt     = 1
   LET v_cadena_aux = ""
   LET v_bnd_nulos  = 0

   CALL v_arr_consulta.clear()

   LET v_s_qry = " SELECT nom_tabla,             \n
                          subproceso,            \n
                          alias,                 \n
                          identificador          \n
                     FROM cat_tabla_subproceso   \n
                    WHERE 1=1                    \n "

   CASE p_bnd_sp 
      WHEN 1 
         LET v_s_qry = v_s_qry || ' AND subproceso = "001" \n'
         LET p_subproceso = "001"
      WHEN 2 
         LET v_s_qry = v_s_qry || ' AND subproceso = "002" \n'
         LET p_subproceso = "002"
      WHEN 3 
         LET v_s_qry = v_s_qry || ' AND subproceso = "003" \n'
         LET p_subproceso = "003"
      WHEN 5
         LET v_s_qry = v_s_qry || ' AND subproceso = "005" \n'
         LET p_subproceso = "005"
   END CASE  

   DISPLAY "SUBPROCESO CASE : ",p_subproceso

   DISPLAY "QUERY DE TABLA : \n",v_s_qry
   PREPARE prp_cons_tabla FROM v_s_qry
   DECLARE cur_tabla CURSOR FOR prp_cons_tabla

   IF p_nss IS NOT NULL THEN
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND gd.nss = " || p_nss
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF

   IF p_subproceso IS NOT NULL THEN
      LET v_bnd_nulos = 1 
      LET v_cadena_aux =  "\n AND subproceso = " || p_subproceso 
      LET v_s_condicion =  v_s_condicion, v_cadena_aux
   END IF 

   IF p_cve_ent_financiera IS NOT NULL THEN
      LET v_bnd_nulos = 1 
      DISPLAY "alias: ", v_arr_cons_tabla[v_ax_cnt].alias
      LET v_cadena_aux = "\n AND gd.cve_ent_financiera = "   , p_cve_ent_financiera  
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF 

   IF p_f_proceso IS NOT NULL THEN
      LET v_ax_fecha = p_f_proceso
      LET v_ax_fecha = "'", v_ax_fecha CLIPPED , "'"
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND gd.f_proceso = " || v_ax_fecha
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF

   IF p_f_carga IS NOT NULL THEN
      LET v_ax_fecha = p_f_carga
      LET v_ax_fecha = "'", v_ax_fecha CLIPPED , "'"
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND gar.f_proceso = " || v_ax_fecha
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF  

   IF p_tpo_credito IS NOT NULL THEN
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND tpo_credito = '" || p_tpo_credito || "'"
      LET v_s_condicion = v_s_condicion , "\n AND tpo_credito = '" || p_tpo_credito || "'" --v_cadena_aux
   END IF

   IF p_situacion IS NOT NULL THEN
      LET v_bnd_nulos = 1 
      --LET v_cadena_aux = "\n AND situacion = " || p_situacion
      LET v_cadena_aux = "\n AND estado = " || p_situacion
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF

   DISPLAY "CONDICIÓN : \n", v_s_condicion

   -- todos son nulos 
   IF v_bnd_nulos = 0 THEN
      -- Si no ingresan paámetros de búsuqeda se notifica con un mensaje
      CALL fn_mensaje("Alterta", "Debe ingresar al menos un criterio de búsqueda","stop")
   ELSE 
      FOREACH cur_tabla INTO v_arr_cons_tabla[v_ax_cnt].*
{         LET v_s_qry = "SELECT " || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || "." || v_arr_cons_tabla[v_ax_cnt].identificador || ",\n"
                                 || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".estado, \n"
                                 || "gd.subproceso, \n" 
                                 || "gd.cve_ent_financiera, \n"
                                 || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".diagnostico, \n"
                                 || "gd.f_proceso, \n"
                                 || "gd.nss, \n"
                                 || "'', \n"
                                 || "'' \n" ||
                         "FROM " || "grt_detalle gd, \n"
                                 || v_arr_cons_tabla[v_ax_cnt].nom_tabla CLIPPED || " " || v_arr_cons_tabla[v_ax_cnt].alias || ", \n"
                                 || "grt_ctr_archivo gar \n "   ||
                        "WHERE " || "gd.id_derechohabiente = " || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".id_derechohabiente \n" ||
                          "AND " || "gd.id_grt_detalle = " || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".id_grt_detalle \n " ||
                          "AND      gar.id_grt_ctr_Archivo = gd.id_grt_ctr_archivo "
                          || v_s_condicion ||
                        "\nORDER BY gd.cve_ent_financiera \n"
}
         LET v_s_qry = "SELECT " || "gd.id_derechohabiente,\n"
                                 || "gd.id_grt_detalle,\n"
                                 || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || "." || v_arr_cons_tabla[v_ax_cnt].identificador || ",\n"
                                 || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".estado, \n"
                                 || "gd.subproceso, \n" 
                                 || "gd.cve_ent_financiera, \n"
                                 || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".diagnostico, \n"
                                 || "gd.f_proceso, \n"
                                 || "gd.nss, \n"
                                 || "'', \n"
                                 || "'' \n" ||
                         "FROM " || "grt_detalle gd, \n"
                                 || v_arr_cons_tabla[v_ax_cnt].nom_tabla CLIPPED || " " || v_arr_cons_tabla[v_ax_cnt].alias || ", \n"
                                 || "grt_ctr_archivo gar \n "   ||
                        "WHERE " || "gd.id_derechohabiente = " || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".id_derechohabiente \n" ||
                          "AND " || "gd.id_grt_detalle = " || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".id_grt_detalle \n " ||
                          "AND      gar.id_grt_ctr_Archivo = gd.id_grt_ctr_archivo "
                          || v_s_condicion ||
                        "\nORDER BY gd.cve_ent_financiera \n"

         DISPLAY "Query a ejecutar "||v_ax_cnt||" : "
         DISPLAY v_s_qry
         DISPLAY "\n\n"

         PREPARE prp_consulta FROM v_s_qry
         DECLARE cur_consulta CURSOR FOR prp_consulta

         FOREACH cur_consulta INTO v_arr_consulta[v_cnt].*
            -- Se consulta el nombre 
            LET v_s_qry = " SELECT rfc,
                                   TRIM(NVL(ap_paterno,'')) ||' '|| TRIM(NVL(ap_materno,'')) || ' ' || TRIM(NVL(nombre,'')) \n
                              FROM grt_tramite           \n
                             WHERE id_grt_tramite = (    \n
                                   SELECT id_grt_tramite \n 
                                     FROM " || v_arr_cons_tabla[v_ax_cnt].nom_tabla || "\n" ||
                                  " WHERE " || v_arr_cons_tabla[v_ax_cnt].identificador || " = " || v_arr_consulta[v_cnt].id || ")\n"

            DISPLAY "CONSULTA DE RFC y NOMBRE : \n",v_s_qry

            PREPARE prp_cons_ax_edo FROM v_s_qry
            EXECUTE prp_cons_ax_edo INTO v_arr_consulta[v_cnt].rfc,
                                         v_arr_consulta[v_cnt].nombre

            LET v_cnt = v_cnt + 1
         END FOREACH
         CALL v_arr_consulta.deleteElement(v_arr_consulta.getLength())

         LET v_ax_cnt = v_ax_cnt + 1
      END FOREACH

      IF v_arr_consulta.getLength() <= 0 THEN
         CALL fn_mensaje("Alerta","No se encontraron registros","stop")
      ELSE  
         OPEN WINDOW w_consulta WITH FORM "GRTC1012"
            DISPLAY ARRAY v_arr_consulta TO r_consulta.*
               ON ACTION ACCEPT
                  LET v_pos = ARR_CURR()
                  DISPLAY "CASE : ",v_arr_consulta[v_pos].subproceso
                  CALL fn_consulta_detalle(v_arr_consulta[v_pos].*)
            END DISPLAY
         CLOSE WINDOW w_consulta
      END IF
   END IF

END FUNCTION

FUNCTION fn_consulta_detalle(v_rec_detalle)
   DEFINE v_rec_detalle         RECORD
      id_derechohabiente            DECIMAL(9,0),   
      id_detalle                    DECIMAL(9,0),
      id                            DECIMAL(9,0),
      situacion                     SMALLINT,
      subproceso                    SMALLINT, --CHAR(3),
      cve_ent_financiera            SMALLINT,      
      diagnostico                   SMALLINT, --CHAR(2),
      f_proceso                     DATE,
      nss                           CHAR(11),
      rfc                           CHAR(13),
      nombre                        VARCHAR(80)
   END RECORD
   DEFINE v_ax_id_tram              DECIMAL(9,0)
   DEFINE v_desc_ent_financiera     CHAR(60)
   DEFINE v_ax_nss                  CHAR(11)
   DEFINE v_curp                    CHAR(18)
   DEFINE v_control_ef              CHAR(18)
   DEFINE v_ap_paterno              CHAR(40)
   DEFINE v_ap_materno              CHAR(40)
   DEFINE v_nombre                  CHAR(40)
   DEFINE v_f_envio                 DATE 
   DEFINE v_producto                CHAR(1)
   DEFINE v_bim_aport               SMALLINT
   DEFINE v_sdo_97                  DECIMAL(12,2)
   DEFINE v_id_grt_tramite          DECIMAL(9,0)
   DEFINE v_f_subcta                DATE
   DEFINE v_carga                   DATE   -- consultar 
   DEFINE v_respuesta               DATE   -- consultar 
   DEFINE v_f_vigencia              DATE 
   DEFINE v_estado                  SMALLINT
   DEFINE v_ax_id_dh                DECIMAL(9,0)
   DEFINE v_ax_situacion            SMALLINT
   DEFINE v_desc_situacion          CHAR(40)
   DEFINE v_ax_id_grt_formalizacion DECIMAL(9,0)
   -- variables SP002
   DEFINE v_num_escritura          DECIMAL(8,0)
   DEFINE v_notario                DECIMAL(4,0)
   DEFINE v_ent_fed_notario        SMALLINT
   DEFINE v_mcpio_notario          SMALLINT
   DEFINE v_num_rpp                CHAR(15)
   DEFINE v_folio_real             DECIMAL(8,0)
   DEFINE v_partida                DECIMAL(6,0)
   DEFINE v_foja                   DECIMAL(8,0)
   DEFINE v_volumen                DECIMAL(6,0)
   DEFINE v_libro                  DECIMAL(6,0)
   DEFINE v_tomo                   DECIMAL(6,0)
   DEFINE v_seccion                DECIMAL(6,0)
   DEFINE v_ent_fed_inmueble       SMALLINT
   DEFINE v_domicilio_inmueble     CHAR(30)
   DEFINE v_valor_avaluo           DECIMAL(15,0)
   DEFINE v_monto_credito          DECIMAL(15,0)
   DEFINE v_plazo_credito          DECIMAL(5,0)
   DEFINE v_tpo_moneda             SMALLINT
   DEFINE v_tasa_base              CHAR(20)
   DEFINE v_margen                 CHAR(20)
   DEFINE v_f_otorga_ent_fin       DATE
   DEFINE v_f_reg_carta            DATE
   DEFINE v_usuario_carta          CHAR(20)
   DEFINE v_diagnostico            CHAR(2)
   DEFINE v_ax_id_grt_solic_ug     DECIMAL(9,0)
   DEFINE v_estado_f               SMALLINT
   -- Variables SP3 
   DEFINE v_importe_solicitado     DECIMAL(12,2)
   DEFINE v_f_vencimiento          DATE
   DEFINE v_importe_utilizado      DECIMAL(12,2)
   DEFINE v_ax_tabla               CHAR(40)
   DEFINE v_ax_columna             CHAR(40)
   DEFINE v_estado_ug              SMALLINT
   DEFINE v_ax_id                  SMALLINT
   DEFINE v_ax_f_otorga_ent_fin    SMALLINT 
   DEFINE v_ax_f_reg_carta         SMALLINT 
   DEFINE v_ax_f_vencimiento       SMALLINT
   DEFINE p_id_dh                  DECIMAL(9,0)
   DEFINE p_id_proceso             DECIMAL(9,0)
   DEFINE p_subproceso             SMALLINT
   DEFINE v_nom_tabla              CHAR(40)
   DEFINE v_alias                  CHAR(2)
   DEFINE v_identificador          CHAR(25)

   CALL fn_llena_inconsistencias(v_rec_detalle.id_derechohabiente,v_rec_detalle.id,v_rec_detalle.subproceso)

   OPEN WINDOW w_det_1 WITH FORM "GRTC1013"

      CASE v_rec_detalle.subproceso
         WHEN 1         
            LET v_s_qry = "SELECT ap_paterno,         \n
                                  ap_materno,         \n
                                  nombre,             \n
                                  curp,               \n
                                  tpo_credito,        \n
                                  viv97,              \n
                                  f_saldo,            \n
                                  f_vigencia,         \n
                                  id_grt_tramite,     \n
                                  estado,             \n
                                  id_derechohabiente, \n
                                  f_respuesta,        \n
                                  situacion           \n
                             FROM grt_tramite         \n
                            WHERE id_grt_tramite = ? "
   
            DISPLAY "Qry 1 a ejecutar : \n" ,v_s_qry
   
            PREPARE prp_cons_dat_tram FROM v_s_qry
            EXECUTE prp_cons_dat_tram USING v_rec_detalle.id
                                       INTO v_ap_paterno,
                                            v_ap_materno,
                                            v_nombre,
                                            v_curp,
                                            v_producto,
                                            v_sdo_97,
                                            v_f_subcta,
                                            v_f_vigencia,
                                            v_id_grt_tramite,
                                            v_estado,
                                            v_ax_id_dh,
                                            v_respuesta,
                                            v_ax_situacion
   
            LET v_s_qry = "SELECT ga.f_lote,                                      \n
                                  gd.f_proceso                                    \n
                             FROM grt_tramite g,                                  \n
                                  grt_detalle gd,                                 \n
                                  grt_ctr_archivo ga                              \n
                            WHERE gd.id_derechohabiente = g.id_derechohabiente    \n
                              AND gd.cve_ent_financiera = g.cve_ent_financiera    \n
                              AND gd.id_grt_detalle     = g.id_grt_detalle        \n
                              AND ga.id_grt_ctr_archivo = gd.id_grt_ctr_archivo   \n
                              AND gd.nss           = ?                            \n
                              AND g.id_grt_tramite = ? "
   
            DISPLAY "Qry 2 a ejecutar \n: " ,v_s_qry
   
            PREPARE prp_cons_dat_arch FROM v_s_qry
            EXECUTE prp_cons_dat_arch USING v_rec_detalle.nss,
                                            v_rec_detalle.id
                                       INTO v_f_envio,
                                            v_carga
   
            LET v_s_qry = "SELECT LPAD(c.cve_ent_financiera,3,'0') || ' - ' || c.ent_financiera_desc \n
                             FROM cat_ent_financiera c,                         \n
                                  grt_tramite g                                 \n
                            WHERE g.cve_ent_financiera  = c.cve_ent_financiera  \n
                              AND g.id_grt_tramite = ? "
   
            DISPLAY "Qry 3 a ejecutar : \n" ,v_s_qry
   
            PREPARE prp_cons_dat_ef FROM v_s_qry
            EXECUTE prp_cons_dat_ef USING v_rec_detalle.id
                                     INTO v_desc_ent_financiera
   
         WHEN 2
            -- Se obtiene el id de tramite para obtener datos generales
            LET v_s_qry = " SELECT id_grt_tramite
                              FROM grt_formalizacion
                             WHERE id_grt_formalizacion = ? "

            PREPARE prp_ax_tramite FROM v_s_qry
            EXECUTE prp_ax_tramite INTO v_ax_id_tram
                                  USING v_rec_detalle.id
            
            LET v_s_qry = "SELECT viv97,            \n
                                  f_saldo           \n
                             FROM grt_tramite       \n
                            WHERE id_grt_tramite = ?"
   
            DISPLAY "Qry 1 a ejecutar : \n" ,v_s_qry
   
            PREPARE prp_cons_dat_form FROM v_s_qry
            EXECUTE prp_cons_dat_form USING v_ax_id_tram
                                       INTO v_sdo_97,
                                            v_f_subcta

            LET v_s_qry = "SELECT ga.f_lote,                                    \n
                                  gd.f_proceso                                  \n
                             FROM grt_formalizacion g,                          \n
                                  grt_detalle gd,                               \n
                                  grt_ctr_archivo ga                            \n
                            WHERE gd.id_derechohabiente = g.id_derechohabiente  \n
                              AND gd.cve_ent_financiera = g.cve_ent_financiera  \n
                              AND gd.id_grt_detalle     = g.id_grt_detalle      \n
                              AND ga.id_grt_ctr_archivo = gd.id_grt_ctr_archivo \n
                              AND gd.nss = ?                                    \n
                              AND g.id_grt_formalizacion = ? "
   
            DISPLAY "Qry 2 a ejecutar \n: " ,v_s_qry
   
            PREPARE prp_cons_dat_arch_f FROM v_s_qry
            EXECUTE prp_cons_dat_arch_f USING v_rec_detalle.nss,
                                              v_rec_detalle.id
                                         INTO v_f_envio,
                                              v_carga
   
            LET v_s_qry = "SELECT LPAD(c.cve_ent_financiera,3,'0') || ' - ' || c.ent_financiera_desc \n
                             FROM cat_ent_financiera c,                         \n
                                  grt_formalizacion g                           \n
                            WHERE g.cve_ent_financiera  = c.cve_ent_financiera  \n
                              AND g.id_grt_formalizacion = ? "
   
            DISPLAY "Qry 3 a ejecutar : \n" ,v_s_qry
   
            PREPARE prp_cons_dat_ef_f FROM v_s_qry
            EXECUTE prp_cons_dat_ef_f USING v_rec_detalle.id
                                       INTO v_desc_ent_financiera

            LET v_s_qry = "SELECT ap_paterno,
                                  ap_materno,
                                  nombre,
                                  curp,
                                  tpo_credito,
                                  num_escritura ,
                                  notario,
                                  ent_fed_notario,
                                  mcpio_notario ,
                                  num_rpp ,
                                  folio_real,
                                  partida ,
                                  foja ,
                                  volumen ,
                                  libro ,
                                  tomo ,
                                  seccion ,
                                  ent_fed_inmueble ,
                                  domicilio_inmueble ,
                                  valor_avaluo ,
                                  monto_credito ,
                                  plazo_credito ,
                                  tpo_moneda ,
                                  tasa_base ,
                                  margen ,
                                  f_otorga_ent_fin,
                                  f_registro_carta,
                                  usuario_reg_carta,
                                  estado           ,
                                  f_vigencia, 
                                  situacion,
                                  f_respuesta
                             FROM grt_formalizacion
                            WHERE id_grt_formalizacion = ?"

            PREPARE prp_cons_formalizacion FROM v_s_qry
            EXECUTE prp_cons_formalizacion USING v_rec_detalle.id
                                            INTO v_ap_paterno        ,
                                                 v_ap_materno        ,
                                                 v_nombre            ,
                                                 v_curp              ,
                                                 v_producto          ,
                                                 v_num_escritura     ,
                                                 v_notario           ,
                                                 v_ent_fed_notario   ,
                                                 v_mcpio_notario     ,
                                                 v_num_rpp           ,
                                                 v_folio_real        ,
                                                 v_partida           ,
                                                 v_foja              ,
                                                 v_volumen           ,
                                                 v_libro             ,
                                                 v_tomo              ,
                                                 v_seccion           ,
                                                 v_ent_fed_inmueble  ,
                                                 v_domicilio_inmueble,
                                                 v_valor_avaluo      ,
                                                 v_monto_credito     ,
                                                 v_plazo_credito     ,
                                                 v_tpo_moneda        ,
                                                 v_tasa_base         ,
                                                 v_margen            ,
                                                 v_f_otorga_ent_fin  ,
                                                 v_f_reg_carta       ,
                                                 v_usuario_carta     ,
                                                 v_estado            ,
                                                 v_f_vigencia        ,
                                                 v_ax_situacion      ,
                                                 v_respuesta


         WHEN 3
            LET v_s_qry = " SELECT importe_solicitado,
                                   f_vencimiento,
                                   importe_utilizado,
                                   NVL(estado,'')
                              FROM grt_solicitud_uso_garantia
                             WHERE id_grt_solicitud_ug = ? "

            PREPARE prp_cons_ug FROM v_s_qry
            EXECUTE prp_cons_ug USING v_ax_id_grt_solic_ug
                                 INTO v_importe_solicitado,
                                      v_f_vencimiento,
                                      v_importe_utilizado,
                                      v_estado


      END CASE

      DISPLAY v_ax_situacion            TO v_situacion  
      DISPLAY v_desc_ent_financiera     TO v_cve_ent_financiera
      DISPLAY v_rec_detalle.diagnostico TO v_diagnostico                
      DISPLAY v_rec_detalle.nss         TO v_nss
      DISPLAY v_rec_detalle.rfc         TO v_rfc
      DISPLAY BY NAME v_sdo_97      
      DISPLAY BY NAME v_ap_paterno
      DISPLAY BY NAME v_ap_materno
      DISPLAY BY NAME v_nombre
      DISPLAY BY NAME v_producto
      DISPLAY BY NAME v_curp
      DISPLAY v_rec_detalle.subproceso  TO v_subproceso
      DISPLAY v_rec_detalle.f_proceso   TO v_f_proceso                  
      DISPLAY BY NAME v_estado
      DISPLAY BY NAME v_f_subcta
      DISPLAY BY NAME v_f_envio
      DISPLAY BY NAME v_respuesta
      DISPLAY BY NAME v_f_vigencia
      DISPLAY BY NAME v_carga


      --MENU
      DISPLAY ARRAY v_arr_inconsistencia TO r_inconsistencia.* ATTRIBUTES(ACCEPT=FALSE,CANCEL=FALSE)
         ON ACTION siguiente
            OPEN WINDOW w_det_2 WITH FORM "GRTC1014"
               --Display de segunda pantalla
               DISPLAY v_rec_detalle.nss TO v_nss
               --DISPLAY v_ax_situacion    TO situacion
               DISPLAY BY NAME v_estado
               DISPLAY BY NAME v_num_escritura
               DISPLAY BY NAME v_notario
               DISPLAY BY NAME v_num_rpp
               DISPLAY BY NAME v_folio_real
               DISPLAY BY NAME v_partida
               DISPLAY BY NAME v_foja
               DISPLAY BY NAME v_volumen
               DISPLAY BY NAME v_libro
               DISPLAY BY NAME v_tomo
               DISPLAY BY NAME v_seccion
               DISPLAY BY NAME v_domicilio_inmueble
               DISPLAY BY NAME v_valor_avaluo
               DISPLAY BY NAME v_monto_credito
               DISPLAY BY NAME v_plazo_credito
               DISPLAY BY NAME v_tasa_base
               DISPLAY BY NAME v_margen
               DISPLAY BY NAME v_usuario_carta
               IF v_ent_fed_notario <> 0 THEN
                  DISPLAY v_ent_fed_notario USING '&&' TO v_ent_fed_notario
               END IF
               IF v_mcpio_notario <> 0THEN 
                  DISPLAY v_mcpio_notario USING '&&&' TO v_mcpio_notario
               END IF
               IF v_f_reg_carta IS NULL OR v_f_reg_carta = "12/31/1899" THEN
                  LET v_ax_f_reg_carta = NULL
                  DISPLAY v_ax_f_reg_carta TO v_f_reg_carta
               ELSE 
                  DISPLAY v_f_reg_carta USING 'dd-mm-yyyy' TO v_f_reg_carta
               END IF
               IF v_f_otorga_ent_fin IS NULL OR v_f_otorga_ent_fin = "12/31/1899" THEN
                  LET v_ax_f_otorga_ent_fin = NULL 
                  DISPLAY v_ax_f_otorga_ent_fin TO v_f_otorga_ent_fin
               ELSE
                  DISPLAY v_f_otorga_ent_fin USING 'dd-mm-yyyy' TO v_f_otorga_ent_fin
               END IF
               IF v_tpo_moneda <> 0 THEN 
                  DISPLAY v_tpo_moneda USING '&&' TO v_tpo_moneda
               END IF
               IF v_ent_fed_inmueble <> 0 THEN
                  DISPLAY v_ent_fed_inmueble USING '&&' TO v_ent_fed_inmueble
               END IF

               MENU 
                  ON ACTION siguiente
                     OPEN WINDOW w_det_3 WITH FORM "GRTC1015"
                     -- Display de variables terecera pantalla
                     DISPLAY v_rec_detalle.nss TO v_nss
                     --DISPLAY v_ax_situacion    TO situacion
                     DISPLAY BY NAME v_estado
                     DISPLAY BY NAME v_importe_solicitado
                     DISPLAY BY NAME v_importe_utilizado
                     IF v_f_vencimiento IS NULL OR v_f_vencimiento = "12/31/1899" THEN
                        LET v_ax_f_vencimiento = NULL
                        DISPLAY v_ax_f_vencimiento USING '##########' TO v_f_vencimiento
                     ELSE
                        DISPLAY v_f_vencimiento USING 'dd-mm-yyyy' TO v_f_vencimiento
                     END IF

                     MENU 
                        ON ACTION anterior
                           EXIT MENU
                     END MENU

                     CLOSE WINDOW w_det_3

                  ON ACTION anterior
                     EXIT MENU 
               END MENU 
            CLOSE WINDOW w_det_2

         ON ACTION salir 
            EXIT DISPLAY
      --END MENU
      END DISPLAY 

   CLOSE WINDOW w_det_1
END FUNCTION

FUNCTION fn_llena_inconsistencias(p_id_dh,p_id_proceso,p_subproceso)
   DEFINE p_id_dh                   DECIMAL(9,0)
   DEFINE p_id_proceso              DECIMAL(9,0)
   DEFINE p_subproceso              SMALLINT
   DEFINE v_nom_tabla               CHAR(40)
   DEFINE v_alias                   CHAR(2)
   DEFINE v_identificador           CHAR(25)

   LET v_cnt = 1      -- Se inicializa el contador
   
   -- se limpia el arreglo
   CALL v_arr_inconsistencia.clear()

   LET v_s_qry = "SELECT nom_tabla,
                         alias,
                         identificador
                    FROM cat_tabla_subproceso
                   WHERE subproceso = ?"

   PREPARE prp_c_ax_tabla FROM v_s_qry
   EXECUTE prp_c_ax_tabla INTO v_nom_tabla,
                               v_alias,
                               v_identificador
                         USING p_subproceso

   LET v_s_qry = " SELECT gci.inconsistencia,   \n"
               ||"        gci.incons_desc       \n"
               ||"   FROM cat_inconsistencia gci, \n" 
               ||         v_nom_tabla || " " || v_alias ||",\n" 
               ||"        grt_inconsistencia gi \n "
               ||"  WHERE gci.inconsistencia = gi.inconsistencia \n "
               ||"    AND " || v_alias || "."|| v_identificador || " = gi.id_grt_referencia \n"
               ||"    AND " || v_alias || ".id_derechohabiente = " || p_id_dh || "\n"
               ||"    AND " || v_alias || "." || v_identificador ||" = " || p_id_proceso|| " \n "
               ||"    AND gi.subproceso = " || p_subproceso 

   DISPLAY "qry incons :\n",v_s_qry
   
   PREPARE prp_inconsis FROM v_s_qry
   DECLARE cur_inc CURSOR FOR prp_inconsis

   --FOREACH cur_inc USING p_id_dh, p_id_proceso, p_subproceso INTO v_arr_inconsistencia[v_cnt].*
   FOREACH cur_inc INTO v_arr_inconsistencia[v_cnt].*
      DISPLAY "Arreglo :",v_arr_inconsistencia[v_cnt].*
      LET v_cnt = v_cnt + 1 
   END FOREACH 
   CALL v_arr_inconsistencia.deleteElement(v_arr_inconsistencia.getLength())

END FUNCTION

{
FUNCTION fn_consulta_detalle(v_rec_detalle)
   DEFINE v_rec_detalle         RECORD
      id                            DECIMAL(9,0),
      situacion                     SMALLINT,
      subproceso                    SMALLINT, --CHAR(3),
      cve_ent_financiera            SMALLINT,      
      diagnostico                   SMALLINT, --CHAR(2),
      f_proceso                     DATE,
      nss                           CHAR(11),
      rfc                           CHAR(13),
      nombre                        VARCHAR(80)
   END RECORD
   --DEFINE v_arr_inconsistencia DYNAMIC ARRAY OF RECORD
   DEFINE v_arr_incons_tramite DYNAMIC ARRAY OF RECORD
      inconsistencia                CHAR(2),
      descripcion                   CHAR(80)
   END RECORD
   DEFINE v_desc_ent_financiera     CHAR(60)
   DEFINE v_ax_nss                  CHAR(11)
   DEFINE v_curp                    CHAR(18)
   DEFINE v_control_ef              CHAR(18)
   DEFINE v_ap_paterno              CHAR(40)
   DEFINE v_ap_materno              CHAR(40)
   DEFINE v_nombre                  CHAR(40)
   DEFINE v_f_envio                 DATE 
   DEFINE v_producto                CHAR(1)
   DEFINE v_bim_aport               SMALLINT
   DEFINE v_sdo_97                  DECIMAL(12,2)
   DEFINE v_id_grt_tramite          DECIMAL(9,0)
   DEFINE v_f_subcta                DATE
   DEFINE v_carga                   DATE   -- consultar 
   DEFINE v_respuesta               DATE   -- consultar 
   DEFINE v_f_vigencia              DATE 
   DEFINE v_estado                  SMALLINT
   DEFINE v_ax_id_dh                DECIMAL(9,0)
   DEFINE v_ax_situacion            SMALLINT
   DEFINE v_desc_situacion          CHAR(40)
   DEFINE v_ax_id_grt_formalizacion DECIMAL(9,0)
   -- variables SP002
   DEFINE v_rec_det_formal    RECORD
      id                            DECIMAL(9,0),
      situacion                     SMALLINT, -- Anteriormente estado
      edo_desc                      CHAR(40),
      subproceso                    SMALLINT, --CHAR(3),
      cve_ent_financiera            SMALLINT,
      diagnostico                   SMALLINT, --CHAR(2),
      f_proceso                     DATE,
      nss                           CHAR(11),
      rfc                           CHAR(13),
      nombre                        VARCHAR(80)
   END RECORD
   DEFINE v_arr_incons_formaliza DYNAMIC ARRAY OF RECORD
      inconsistencia                CHAR(2),
      descripcion                   CHAR(80)
   END RECORD
   DEFINE v_num_escritura          DECIMAL(8,0)
   DEFINE v_notario                DECIMAL(4,0)
   DEFINE v_ent_fed_notario        SMALLINT
   DEFINE v_mcpio_notario          SMALLINT
   DEFINE v_num_rpp                CHAR(15)
   DEFINE v_folio_real             DECIMAL(8,0)
   DEFINE v_partida                DECIMAL(6,0)
   DEFINE v_foja                   DECIMAL(8,0)
   DEFINE v_volumen                DECIMAL(6,0)
   DEFINE v_libro                  DECIMAL(6,0)
   DEFINE v_tomo                   DECIMAL(6,0)
   DEFINE v_seccion                DECIMAL(6,0)
   DEFINE v_ent_fed_inmueble       SMALLINT
   DEFINE v_domicilio_inmueble     CHAR(30)
   DEFINE v_valor_avaluo           DECIMAL(15,0)
   DEFINE v_monto_credito          DECIMAL(15,0)
   DEFINE v_plazo_credito          DECIMAL(5,0)
   DEFINE v_tpo_moneda             SMALLINT
   DEFINE v_tasa_base              CHAR(20)
   DEFINE v_margen                 CHAR(20)
   DEFINE v_f_otorga_ent_fin       DATE
   DEFINE v_f_reg_carta            DATE
   DEFINE v_usuario_carta          CHAR(20)
   DEFINE v_diagnostico            CHAR(2)
   DEFINE v_ax_id_grt_solic_ug     DECIMAL(9,0)
   DEFINE v_estado_f               SMALLINT
   -- Variables SP3 
   DEFINE v_importe_solicitado     DECIMAL(12,2)
   DEFINE v_f_vencimiento          DATE
   DEFINE v_importe_utilizado      DECIMAL(12,2)
   DEFINE v_ax_tabla               CHAR(40)
   DEFINE v_ax_columna             CHAR(40)
   DEFINE v_estado_ug              SMALLINT
   DEFINE v_ax_id                  SMALLINT
   DEFINE v_arr_incons_ug DYNAMIC ARRAY OF RECORD
      inconsistencia               CHAR(2),
      descripcion                  CHAR(80)
   END RECORD
   DEFINE v_ax_f_otorga_ent_fin    SMALLINT 
   DEFINE v_ax_f_reg_carta         SMALLINT 
   DEFINE v_ax_f_vencimiento       SMALLINT 

   LET v_bim_aport = 0
   LET v_ax_nss    = v_rec_detalle.nss

   DISPLAY "SITUACIÓN  : ",v_rec_detalle.situacion         
   DISPLAY "SUBPROCESO : ",v_rec_detalle.subproceso        
   DISPLAY "ENTIDAD FIN: ",v_rec_detalle.cve_ent_financiera
   DISPLAY "DIAGNOSTICO: ",v_rec_detalle.diagnostico       
   DISPLAY "F PROCESO  : ",v_rec_detalle.f_proceso         
   DISPLAY "NSS        : ",v_rec_detalle.nss               
   DISPLAY "RFC        : ",v_rec_detalle.rfc               
   DISPLAY "NOMBRE     : ",v_rec_detalle.nombre 
   DISPLAY "ID TRÁMITE : ",v_rec_detalle.id   

   IF v_rec_detalle.subproceso <> 1 THEN
      LET v_s_qry = " SELECT nom_tabla,
                             identificador
                        FROM cat_tabla_subproceso
                       WHERE subproceso = ? "

      PREPARE prp_ax_tabla FROM v_s_qry
      EXECUTE prp_ax_tabla INTO v_ax_tabla,
                                v_ax_columna
                          USING v_rec_detalle.subproceso  

      LET v_s_qry = " SELECT id_grt_tramite \n
                        FROM " || v_ax_tabla CLIPPED   || " \n" ||
                     " WHERE " || v_ax_columna CLIPPED || " = " || v_rec_detalle.id

      DISPLAY "QRY tabla por SP diferente de uno\n",v_s_qry

      PREPARE prp_ax_id FROM v_s_qry
      EXECUTE prp_ax_id INTO v_ax_id --v_rec_detalle.id

      IF v_ax_id <> 0 THEN
         LET v_rec_detalle.id = v_ax_id
      END IF
   END IF

   OPEN WINDOW w_cons2 WITH FORM "GRTC1013"
      LET v_s_qry = "SELECT ap_paterno,         \n
                            ap_materno,         \n
                            nombre,             \n
                            curp,               \n
                            tpo_credito,        \n
                            viv97,              \n
                            f_saldo,            \n
                            f_vigencia,         \n
                            id_grt_tramite,     \n
                            estado,             \n
                            id_derechohabiente, \n
                            f_respuesta,        \n
                            situacion           \n
                       FROM grt_tramite         \n
                      WHERE id_grt_tramite = ? "

      DISPLAY "NSS : ",v_rec_detalle.nss
      DISPLAY "Qry 1 a ejecutar : " ,v_s_qry

      PREPARE prp_cons_dat_tram FROM v_s_qry
      EXECUTE prp_cons_dat_tram USING v_rec_detalle.id
                                 INTO v_ap_paterno,
                                      v_ap_materno,
                                      v_nombre,
                                      v_curp,
                                      v_producto,
                                      v_sdo_97,
                                      v_f_subcta,
                                      v_f_vigencia,
                                      v_id_grt_tramite,
                                      v_estado,
                                      v_ax_id_dh,
                                      v_respuesta,
                                      v_ax_situacion

      LET v_s_qry = "SELECT ga.f_lote,                                      \n
                            gd.f_proceso                                    \n
                       FROM grt_tramite g,                                  \n
                            grt_detalle gd,                                 \n
                            grt_ctr_archivo ga                              \n
                      WHERE gd.id_derechohabiente = g.id_derechohabiente    \n
                        AND gd.cve_ent_financiera = g.cve_ent_financiera    \n
                        AND gd.id_grt_detalle     = g.id_grt_detalle        \n
                        AND ga.id_grt_ctr_archivo = gd.id_grt_ctr_archivo   \n
                        AND gd.nss           = ?                            \n
                        AND g.id_grt_tramite = ? "

      DISPLAY "Qry 2 a ejecutar : " ,v_s_qry

      PREPARE prp_cons_dat_arch FROM v_s_qry
      EXECUTE prp_cons_dat_arch USING v_rec_detalle.nss,
                                      v_rec_detalle.id
                                 INTO v_f_envio,
                                      v_carga

      LET v_s_qry = "SELECT LPAD(c.cve_ent_financiera,3,'0') || ' - ' || c.ent_financiera_desc \n
                       FROM cat_ent_financiera c,                         \n
                            grt_tramite g                                 \n
                      WHERE g.cve_ent_financiera  = c.cve_ent_financiera  \n
                        AND g.id_grt_tramite = ? "

      DISPLAY "Qry 3 a ejecutar : " ,v_s_qry

      PREPARE prp_cons_dat_ef FROM v_s_qry
      EXECUTE prp_cons_dat_ef USING v_rec_detalle.id
                               INTO v_desc_ent_financiera

      DISPLAY "ID SITUACIÓN : ",v_ax_situacion
      DISPLAY "PRODUCTO  : ", v_producto
      DISPLAY "SITUACIÓN : ",v_desc_situacion
      DISPLAY "SALDO     :",v_sdo_97

      DISPLAY v_ax_situacion            TO v_situacion  
      DISPLAY v_rec_detalle.subproceso  TO v_subproceso
      DISPLAY v_desc_ent_financiera     TO v_cve_ent_financiera
      DISPLAY v_rec_detalle.diagnostico TO v_diagnostico                
      DISPLAY v_rec_detalle.f_proceso   TO v_f_proceso                  
      DISPLAY v_rec_detalle.nss         TO v_nss
      DISPLAY v_rec_detalle.rfc         TO v_rfc
      DISPLAY BY NAME v_estado
      DISPLAY BY NAME v_sdo_97      
      DISPLAY BY NAME v_ap_paterno
      DISPLAY BY NAME v_ap_materno
      DISPLAY BY NAME v_nombre
      DISPLAY BY NAME v_producto
      DISPLAY BY NAME v_f_subcta
      DISPLAY BY NAME v_f_envio
      DISPLAY BY NAME v_curp
      DISPLAY BY NAME v_carga
      DISPLAY BY NAME v_respuesta
      DISPLAY BY NAME v_f_vigencia

      DISPLAY "ID DERECHOHABIENTE : ",v_ax_id_dh

      -- Se llenan las inconsistencias
      LET v_s_qry = " SELECT gci.inconsistencia,
                             gci.incons_desc
                        FROM cat_inconsistencia gci,
                             grt_tramite gt,
                             grt_inconsistencia gi
                       WHERE gci.inconsistencia = gi.inconsistencia
                         AND gt.id_grt_tramite = gi.id_grt_referencia
                         AND gt.id_derechohabiente = ?
                         AND gt.id_grt_tramite = ? 
                         AND subproceso = 1 "

      PREPARE prp_inconsis FROM v_s_qry
      DECLARE cur_inc CURSOR FOR prp_inconsis

      -- se limpia el arreglo
      CALL v_arr_incons_tramite.clear()

      -- Se inicializa el contador 
      LET v_cnt = 1 

      FOREACH cur_inc USING v_ax_id_dh, v_id_grt_tramite INTO v_arr_incons_tramite[v_cnt].*
         LET v_cnt = v_cnt + 1 
      END FOREACH 

      CALL v_arr_incons_tramite.deleteElement(v_arr_incons_tramite.getLength())


IF v_ax_id <> 0 THEN      
      -- Se consulta el id de formalizacion 
      LET v_s_qry = " SELECT a.id_grt_formalizacion 
                        FROM grt_formalizacion a , grt_tramite b
                       WHERE a.id_grt_tramite = ? 
                         AND a.id_gtr_tramite = b.id_grt_tramite
                         AND b.estado in (30,50)"

      PREPARE prp_c_id_f FROM v_s_qry
      EXECUTE prp_c_id_f USING v_id_grt_tramite
                          INTO v_ax_id_grt_formalizacion 
ELSE 
LET v_ax_id_grt_formalizacion = v_rec_detalle.id
END IF

      DISPLAY ARRAY v_arr_incons_tramite TO r_inconsistencia.* ATTRIBUTES( ACCEPT=FALSE) 
         ON ACTION siguiente 
            DISPLAY "ID :",v_rec_detalle.id
            OPEN WINDOW w_forma WITH FORM "GRTC1014" 
               LET v_s_qry = "SELECT num_escritura ,
                                     notario,
                                     ent_fed_notario,
                                     mcpio_notario ,
                                     num_rpp ,
                                     folio_real,
                                     partida ,
                                     foja ,
                                     volumen ,
                                     libro ,
                                     tomo ,
                                     seccion ,
                                     ent_fed_inmueble ,
                                     domicilio_inmueble ,
                                     valor_avaluo ,
                                     monto_credito ,
                                     plazo_credito ,
                                     tpo_moneda ,
                                     tasa_base ,
                                     margen ,
                                     f_otorga_ent_fin,
                                     f_registro_carta,
                                     usuario_reg_carta,
                                     estado
                                FROM grt_formalizacion
                               WHERE id_grt_formalizacion = ?"

               PREPARE prp_cons_formalizacion FROM v_s_qry
               EXECUTE prp_cons_formalizacion USING v_ax_id_grt_formalizacion
                                               INTO v_num_escritura     ,
                                                    v_notario           ,
                                                    v_ent_fed_notario   ,
                                                    v_mcpio_notario     ,
                                                    v_num_rpp           ,
                                                    v_folio_real        ,
                                                    v_partida           ,
                                                    v_foja              ,
                                                    v_volumen           ,
                                                    v_libro             ,
                                                    v_tomo              ,
                                                    v_seccion           ,
                                                    v_ent_fed_inmueble  ,
                                                    v_domicilio_inmueble,
                                                    v_valor_avaluo      ,
                                                    v_monto_credito     ,
                                                    v_plazo_credito     ,
                                                    v_tpo_moneda        ,
                                                    v_tasa_base         ,
                                                    v_margen            ,
                                                    v_f_otorga_ent_fin  ,
                                                    v_f_reg_carta       ,
                                                    v_usuario_carta     ,
                                                    v_estado_f

               DISPLAY "Estado : ",v_estado
               
               DISPLAY v_rec_detalle.nss TO v_nss
               DISPLAY v_estado_f        TO situacion
               DISPLAY BY NAME v_num_escritura
               DISPLAY BY NAME v_notario
               DISPLAY "entidad notario", v_ent_fed_notario
               IF v_ent_fed_notario <> 0 THEN
                  DISPLAY v_ent_fed_notario USING '&&' TO v_ent_fed_notario
               END IF
               IF v_mcpio_notario <> 0THEN 
                  DISPLAY v_mcpio_notario USING '&&&' TO v_mcpio_notario
               END IF
               DISPLAY BY NAME v_num_rpp
               DISPLAY BY NAME v_folio_real
               DISPLAY BY NAME v_partida
               DISPLAY BY NAME v_foja
               DISPLAY BY NAME v_volumen
               DISPLAY BY NAME v_libro
               DISPLAY BY NAME v_tomo
               DISPLAY BY NAME v_seccion

               IF v_ent_fed_inmueble <> 0 THEN
                  DISPLAY v_ent_fed_inmueble USING '&&' TO v_ent_fed_inmueble
               END IF

               DISPLAY BY NAME v_domicilio_inmueble
               DISPLAY BY NAME v_valor_avaluo
               DISPLAY BY NAME v_monto_credito
               DISPLAY BY NAME v_plazo_credito

               IF v_tpo_moneda <> 0 THEN 
                  DISPLAY v_tpo_moneda USING '&&' TO v_tpo_moneda
               END IF 
               DISPLAY BY NAME v_tasa_base
               DISPLAY BY NAME v_margen

               IF v_f_otorga_ent_fin IS NULL OR v_f_otorga_ent_fin = "12/31/1899" THEN
                  LET v_ax_f_otorga_ent_fin = NULL 
                  DISPLAY v_ax_f_otorga_ent_fin TO v_f_otorga_ent_fin
               ELSE
                  DISPLAY v_f_otorga_ent_fin USING 'dd-mm-yyyy' TO v_f_otorga_ent_fin
               END IF

               IF v_f_reg_carta IS NULL OR v_f_reg_carta = "12/31/1899" THEN
                  LET v_ax_f_reg_carta = NULL
                  DISPLAY v_ax_f_reg_carta TO v_f_reg_carta
               ELSE 
                  DISPLAY v_f_reg_carta USING 'dd-mm-yyyy' TO v_f_reg_carta
               END IF 

               DISPLAY BY NAME v_usuario_carta

               -- Se llenan las inconsistencias
               LET v_s_qry = " SELECT gci.inconsistencia,
                                      gci.incons_desc
                                 FROM cat_inconsistencia gci,
                                      grt_formalizacion gf,
                                      grt_inconsistencia gi
                                WHERE gci.inconsistencia = gi.inconsistencia
                                  AND gf.id_grt_formalizacion = gi.id_grt_referencia
                                  AND gf.id_grt_formalizacion = ? 
                                  AND gi.subproceso = 2"

               PREPARE prp_incons_forma FROM v_s_qry
               DECLARE cur_incons_forma CURSOR FOR prp_incons_forma

               -- se limpia el arreglo
               CALL v_arr_incons_formaliza.clear()

               -- Se inicializa el contador 
               LET v_cnt = 1 

               FOREACH cur_incons_forma USING v_ax_id_grt_formalizacion INTO v_arr_incons_formaliza[v_cnt].* --v_rec_det_formal.id INTO v_arr_inconsistencia[v_cnt].*
                  LET v_cnt = v_cnt + 1 
               END FOREACH 

               CALL v_arr_incons_formaliza.deleteElement(v_arr_incons_formaliza.getLength())


IF v_ax_id = 0 THEN 
               -- Se consulta el id de solicitud
               LET v_s_qry = " SELECT FIRST 1 id_grt_solicitud_ug
                                 FROM grt_solicitud_uso_garantia
                                WHERE id_grt_tramite = ? "

               PREPARE prp_c_id_su FROM v_s_qry
               EXECUTE prp_c_id_su USING v_id_grt_tramite
                                    INTO v_ax_id_grt_solic_ug
ELSE 
LET v_ax_id_grt_solic_ug = v_rec_detalle.id
END IF 
               DISPLAY ARRAY v_arr_incons_formaliza TO r_inconsistencia.*  ATTRIBUTES( ACCEPT=FALSE) 
                  ON ACTION Anterior 
                     EXIT DISPLAY

                  ON ACTION siguiente
                     OPEN WINDOW w_s_ug WITH FORM "GRTC1015"
                        LET v_s_qry = " SELECT importe_solicitado,
                                               f_vencimiento,
                                               importe_utilizado,
                                               NVL(estado,'')
                                          FROM grt_solicitud_uso_garantia
                                         WHERE id_grt_solicitud_ug = ? "

                        PREPARE prp_cons_ug FROM v_s_qry
                        EXECUTE prp_cons_ug USING v_ax_id_grt_solic_ug
                                             INTO v_importe_solicitado,
                                                  v_f_vencimiento,
                                                  v_importe_utilizado,
                                                  v_estado_ug

                        DISPLAY v_rec_detalle.nss TO v_nss
                        DISPLAY v_estado_ug       TO situacion
                        DISPLAY BY NAME v_importe_solicitado

                        IF v_f_vencimiento IS NULL OR v_f_vencimiento = "12/31/1899" THEN
                           LET v_ax_f_vencimiento = NULL
                           DISPLAY v_ax_f_vencimiento USING '##########' TO v_f_vencimiento
                        ELSE
                           DISPLAY v_f_vencimiento USING 'dd-mm-yyyy' TO v_f_vencimiento
                        END IF
                        DISPLAY BY NAME v_importe_utilizado

                        -- Se llenan las inconsistencias
                        LET v_s_qry = " SELECT gci.inconsistencia,
                                               gci.incons_desc
                                          FROM cat_inconsistencia gci,
                                               grt_solicitud_uso_garantia gsu,
                                               grt_inconsistencia gi
                                         WHERE gci.inconsistencia = gi.inconsistencia
                                           AND gsu.id_grt_solicitud_ug = gi.id_grt_referencia
                                           AND gsu.id_grt_solicitud_ug = ? 
                                           AND gi.subproceso = 3"

                        PREPARE prp_incons_ug FROM v_s_qry
                        DECLARE cur_incons_ug CURSOR FOR prp_incons_ug

                        -- se limpia el arreglo
                        CALL v_arr_incons_ug.clear()

                        -- Se inicializa el contador 
                        LET v_cnt = 1

                        FOREACH cur_incons_ug USING v_ax_id_grt_solic_ug INTO v_arr_incons_ug[v_cnt].*
                           LET v_cnt = v_cnt + 1 
                        END FOREACH 

                        CALL v_arr_incons_ug.deleteElement(v_arr_incons_ug.getLength())

                        DISPLAY ARRAY v_arr_incons_ug TO r_inconsistencia.*  ATTRIBUTES( ACCEPT=FALSE) 
                           ON ACTION Anterior
                             EXIT DISPLAY

                           ON ACTION CANCEL
                              EXIT DISPLAY

                        END DISPLAY
                     CLOSE WINDOW w_s_ug

                  ON ACTION CANCEL
                     EXIT DISPLAY

               END DISPLAY
            CLOSE WINDOW w_forma
         ON ACTION CANCEL 
            EXIT DISPLAY 

      END DISPLAY
   CLOSE WINDOW w_cons2
END FUNCTION
}

-- Función para validar nss
FUNCTION fn_valida_nss(p_nss_modificado)
   DEFINE p_nss_modificado          LIKE afi_derechohabiente.nss
   DEFINE v_nss                     STRING       -- cadena con el NSS,
   DEFINE v_mensaje                 STRING       -- mensaje para el usuario
   DEFINE v_indice                  SMALLINT     -- indice pivote
   DEFINE v_nss_es_correcto         SMALLINT     -- booleana que indica si un NSS esta correctamente construido
   DEFINE v_ax_id_dh                DECIMAL(9,0) -- variable auxiliar para la busqueda del nss en BD

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

-- Se comenta debído a que para la consulta general, si ingresan un nss no existente nunca
-- dejará pasar a consultar el detalle debído a esta validación 
{   -- Si el nss es correcto lo busca en la BD
   IF v_nss_es_correcto THEN
      LET v_s_qry = " SELECT id_derechohabiente
                        FROM afi_derechohabiente
                       WHERE nss = ? "

      PREPARE prp_existe_nss FROM v_s_qry
      EXECUTE prp_existe_nss INTO v_ax_id_dh USING p_nss_modificado

      DISPLAY v_ax_id_dh," AUX"
      IF v_ax_id_dh IS NULL OR v_ax_id_dh < 0 THEN
         LET v_mensaje = "El NSS no existe en la Base de Datos"
         LET v_nss_es_correcto = FALSE
      END IF 
   END IF
}
 
   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF ( NOT v_nss_es_correcto ) THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_nss_es_correcto
END FUNCTION