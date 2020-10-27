################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIC17                                                        #
#Objetivo     => Consulta prospecto de unificación                             #
#Fecha inicio => 14/12/2015                                                    #
################################################################################

DATABASE safre_viv
DEFINE p_usuario                CHAR(20)      -- Obtiene dato de usuario
DEFINE p_tipo_ejecucion         SMALLINT      -- Forma como ejecutará el programa
DEFINE p_s_titulo               STRING        -- Título de la ventana
DEFINE w ui.Window,
       f ui.Form
MAIN
   -- se recupera la clave de usuario desde parámetro
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".UNIC17.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   OPEN WINDOW v_menu WITH FORM "UNIC176"

      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
      
      MENU "Seleccionar modo de entrada"

      BEFORE MENU 
         CALL f.setElementHidden("gr_nss",1)
         CALL f.setElementHidden("gr_folio",1)
         CALL f.setElementHidden("gr_folio_uni",1)
         CALL f.setElementHidden("gr_detalles",1)
         
      ON ACTION nss
         CALL f.setElementHidden("gr_nss",0)
         CALL fn_nss(1)

      ON ACTION Folio
         CALL f.setElementHidden("gr_folio",0)
         CALL f.setElementHidden("gr_folio_uni",0)
         CALL fn_folio(2)

      ON ACTION CANCEL
         EXIT MENU 

      END MENU
   CLOSE WINDOW v_menu
END MAIN

#OBJETIVO:  Consultar la información del NSS capturado.
FUNCTION fn_nss(p_origen)
DEFINE v_nss                VARCHAR(11),
       v_id_derechohabiente DECIMAL(9,0),
       v_tipo_nss           SMALLINT,
       p_origen             SMALLINT

   INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)
      ON ACTION ACCEPT
         IF length(v_nss) <> 11 THEN
            CALL fn_mensaje ("Archivo","NSS no contiene 11 caracteres","information")
            LET v_nss = NULL
         ELSE
            SELECT id_derechohabiente
            INTO   v_id_derechohabiente
            FROM   afi_derechohabiente
            WHERE  nss = v_nss

            IF v_id_derechohabiente IS NULL THEN
               CALL fn_mensaje ("Archivo","El NSS no existe","information")
            ELSE
               CALL fn_tipo_nss(v_id_derechohabiente)
                    RETURNING v_tipo_nss

               IF v_tipo_nss = 1 THEN 
                  --DISPLAY "Consulta por UNIFICADOR "
                  CALL fn_consulta_unificador(v_id_derechohabiente, p_origen, 0,0)
               ELSE
                  IF v_tipo_nss = 2 THEN 
                     --DISPLAY "Consulta por UNIFICADO "
                     CALL fn_consulta_unificado(v_id_derechohabiente, p_origen,0,0)
                  END IF

                  IF v_tipo_nss = 0 THEN 
                     CALL fn_mensaje ("Archivo","El NSS no existe como prospecto","information")
                  END IF                  
               END IF

               IF v_tipo_nss = 3 THEN 
                  CALL fn_mensaje ("Atención", "El NSS capturado existe como Unificador y Unificado", "stop")
                  INITIALIZE v_nss TO NULL 
                  NEXT FIELD v_nss
               END IF
               
               DISPLAY "NSS capturado : ", v_nss
               INITIALIZE v_nss TO NULL 
               CONTINUE INPUT
            END IF 
         END IF

      ON ACTION CANCEL
         CALL f.setElementHidden("gr_nss",1)
         EXIT INPUT
      END INPUT
END FUNCTION 

#OBJETIVO:  Consultar la información del folio de afiliación selecciondo.
FUNCTION fn_folio(p_origen)
DEFINE p_origen        SMALLINT,
       v_folio         DECIMAL(9,0),
       v_folio_uni     DECIMAL(9,0),
       v_cadena        STRING,
       v_cbx_folios    ui.ComboBox,
       v_cbx_folio_uni ui.ComboBox,
       i               INTEGER,
       x               INTEGER

DEFINE v_r_glo_ctr_archivo RECORD
          folio          DECIMAL(9,0),
          nombre_archivo CHAR(40)
END RECORD


   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
   LET v_cbx_folio_uni = ui.ComboBox.forName("formonly.cmb_folio_uni")
  
   CALL v_cbx_folios.clear()
   CALL v_cbx_folios.addItem(-1," ")

   CALL v_cbx_folio_uni.clear()
   CALL v_cbx_folio_uni.addItem(-1," ")
   
   INPUT v_folio, v_folio_uni WITHOUT DEFAULTS
   FROM  cmb_folio, cmb_folio_uni
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR SELECT g.folio, 
                                              a.nombre_archivo
                                       FROM   glo_ctr_archivo a, 
                                              glo_folio g
                                       WHERE  a.proceso_cod = 1801
                                       AND    a.proceso_cod = g.proceso_cod
                                       AND    a.folio = g.folio
                                       AND    g.status = 0
                                       AND    g.opera_cod = 2
                                       ORDER BY 1 

         LET i = 0

         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_cadena)

            LET i = i + 1
            --DISPLAY "Total registros : ", i
         END FOREACH
         IF(i<1)THEN
            CALL fn_mensaje("Atención",
                            "No existen información para consultar",
                            "info")
            EXIT INPUT
         END IF

         FREE cur_folios

         -- se llena el arreglo de folios
         DECLARE cur_folio_uni CURSOR FOR SELECT DISTINCT b.folio, 
                                                 a.nombre_archivo
                                          FROM   glo_ctr_archivo a,
                                                 glo_folio b
                                          WHERE  a.proceso_cod IN ( 2301, 2309, 2318)
                                          AND    a.opera_cod IN (1,2)
                                          AND    a.folio = b.folio

         LET x = 0

         FOREACH cur_folio_uni INTO v_r_glo_ctr_archivo.*
            LET v_cadena = v_r_glo_ctr_archivo.folio USING "##########", " - ", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folio_uni.addItem(v_r_glo_ctr_archivo.folio,v_cadena)
            -- Contador de archivos eoncontrados
            LET x = x + 1
         END FOREACH

      ON ACTION ACCEPT
         IF ( v_folio IS NULL OR v_folio = -1 ) AND ( v_folio_uni IS NULL OR v_folio_uni = -1 )THEN
            CALL fn_mensaje("Atención","Es necesario seleccionar un folio","stop")
            CONTINUE INPUT
         ELSE
            IF  v_folio IS NOT NULL AND v_folio_uni IS NOT NULL THEN
               CALL fn_mensaje("Atención","Solo puede seleccionar un folio","stop")
               CONTINUE INPUT
            ELSE
               CALL fn_consulta_unificador(0, p_origen,v_folio, v_folio_uni)
               DISPLAY "Manda consulta de unificador "
            END IF
         END IF

      ON ACTION CANCEL
         CALL f.setElementHidden("gr_folio",1)
         CALL f.setElementHidden("gr_folio_uni",1)
         EXIT INPUT
   
   END INPUT
END FUNCTION 

#OBJETIVO: Identifica el tipo de NSS a consultar
FUNCTION fn_tipo_nss(p_id_derechohabiente)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       v_id_pre_unificador  DECIMAL (9,0),
       v_id_pre_unificado   DECIMAL (9,0),
       v_tipo_nss           SMALLINT

   --Identificar si es unificador o unificado
   --Busca registros en AFILIACIÓN
   SELECT id_derechohabiente 
   INTO   v_id_pre_unificador
   FROM   uni_pre_unificador
   WHERE  id_derechohabiente = p_id_derechohabiente
   GROUP BY id_derechohabiente

   IF v_id_pre_unificador IS NOT NULL THEN
      SELECT id_derechohabiente 
      INTO   v_id_pre_unificado 
      FROM   uni_pre_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      GROUP BY id_derechohabiente

      IF v_id_pre_unificado IS NOT NULL THEN
         LET v_tipo_nss = 3
      ELSE 
         LET v_tipo_nss = 1
      END IF
   ELSE
      SELECT id_derechohabiente 
      INTO   v_id_pre_unificado 
      FROM   uni_pre_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      GROUP BY id_derechohabiente

      IF v_id_pre_unificado IS NOT NULL THEN
         LET v_tipo_nss = 2
      END IF
   END IF

   --Busca registros en UNIFICACIÓN
   --Identificar si es unificador o unificado
   SELECT id_derechohabiente 
   INTO   v_id_pre_unificador
   FROM   uni_det_unificador
   WHERE  id_derechohabiente = p_id_derechohabiente
   GROUP BY id_derechohabiente

   IF v_id_pre_unificador IS NOT NULL THEN
      SELECT id_derechohabiente 
      INTO   v_id_pre_unificado 
      FROM   uni_det_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      GROUP BY id_derechohabiente

      IF v_id_pre_unificado IS NOT NULL THEN
         LET v_tipo_nss = 3
      ELSE 
         LET v_tipo_nss = 1
      END IF
   ELSE
      SELECT id_derechohabiente 
      INTO   v_id_pre_unificado 
      FROM   uni_det_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      GROUP BY id_derechohabiente

      IF v_id_pre_unificado IS NOT NULL THEN
         LET v_tipo_nss = 2
      END IF
   END IF 

   RETURN v_tipo_nss
END FUNCTION 

#OBJETIVO: Identifica la función que se va a ejecutar
FUNCTION fn_detalles_nss(p_id_derechohabiente, p_origen, p_folio_lote,p_tipo_nss)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       p_tipo_nss           SMALLINT,
       p_origen             SMALLINT,
       p_folio_lote         DECIMAL (9,0),
       p_folio_uni          DECIMAL (9,0)

   IF p_tipo_nss = 1 THEN 
      --DISPLAY "Consulta por UNIFICADOR "
      CALL fn_consulta_unificador(p_id_derechohabiente, p_origen, p_folio_lote, p_folio_uni)
   ELSE
      IF p_tipo_nss = 2 THEN 
         --DISPLAY "Consulta por UNIFICADO "
      END IF
   END IF
END FUNCTION 

#OBJETIVO: Recueperar el detalle del posible UNIFICADOR
FUNCTION fn_consulta_unificador(p_id_derechohabiente, p_origen, p_folio_lote, p_folio_uni)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       p_origen             SMALLINT,
       p_folio_lote         DECIMAL (9,0),
       p_folio_uni          DECIMAL (9,0),
       v_QryTxt             STRING,
       v_QryTxt1            STRING,
       i                    INTEGER,
       v_ini                INTEGER,
       v_ruta_listados      CHAR(40),
       v_nom_reporte        CHAR(40),
       manejador_rpt        om.SaxDocumentHandler  

DEFINE rec_detalles RECORD 
       v_id_dh_dor            DECIMAL(9,0),
       v_id_dh_ado            DECIMAL(9,0),
       v_id_unificador        DECIMAL(9,0),
       v_folio_unificacion    DECIMAL(9,0),
       v_folio_resp_confronta DECIMAL(9,0),
       v_f_unificacion        DATE, 
       v_f_liquidacion        DATE, 
       ind_procedencia        SMALLINT,
       v_nss                  CHAR(11),
       v_tipo                 CHAR(10),
       v_marca                SMALLINT,
       v_mov_imss             CHAR(10),
       v_f_apertura           DATE,
       v_op_22_notifica       DATE,
       v_resp_procedencia     CHAR(15),
       v_fecha_unificacion    DATE
END RECORD

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD 
          v_nss             CHAR(11),
          v_tipo            CHAR(10),
          v_mov_imss        CHAR(10),
          v_f_apertura      DATE,
          v_op_21_solicitud DATE,
          v_op_21_respuesta CHAR(30),
          v_op_22_notifica  DATE
END RECORD 

   IF p_folio_lote IS NOT NULL THEN
      LET v_QryTxt = "\n SELECT id_derechohabiente, ",
                     "\n        nss_correcto,       ",
                     "\n        id_pre_unificador   ",
                     "\n FROM   uni_pre_unificador  "
   
   
      IF p_origen = 1 THEN 
         LET v_QryTxt = v_QryTxt || "\n WHERE  id_derechohabiente = ",  p_id_derechohabiente
      END IF
   
      IF p_origen = 2 THEN 
         LET v_QryTxt = v_QryTxt || "\n WHERE  folio_lote = ",  p_folio_lote
      END IF

      LET v_QryTxt1 = "\n SELECT id_derechohabiente, ",
                     "\n         nss       ",
                     "\n FROM   uni_pre_unificado  ",
                     "\n WHERE  id_pre_unificador = ? "
   END IF
   
   IF p_folio_uni IS NOT NULL THEN 
      LET v_QryTxt = "\n SELECT id_derechohabiente, ",
                     "\n        nss_unificador,       ",
                     "\n        id_unificador, ",
                     "\n        folio_unificacion ",
                     "\n FROM   uni_det_unificador  "

      IF p_origen = 1 THEN 
         LET v_QryTxt = v_QryTxt || "\n WHERE  id_derechohabiente = ",  p_id_derechohabiente,
                                    "\n GROUP BY 1,2,3,4 "
      END IF

      IF p_origen = 2 THEN 
         LET v_QryTxt = v_QryTxt || "\n WHERE  folio_unificacion = ",  p_folio_uni,
                                    "\n GROUP BY 1,2,3,4 "
      END IF   

      LET v_QryTxt1 = "\n SELECT id_derechohabiente, ",
                      "\n         nsscta1, ",
                      "\n         folio_unificacion ",
                      "\n FROM    uni_det_unificado  ",
                      "\n WHERE  id_unificador = ? ",
                      "\n GROUP BY 1,2,3 "
   END IF

   PREPARE prp_preunificador FROM v_QryTxt
   DECLARE cur_preunificador CURSOR FOR prp_preunificador   

   PREPARE prp_preunificado FROM v_QryTxt1
   DECLARE cur_preunificado CURSOR FOR prp_preunificado

   SELECT  ruta_listados
   INTO    v_ruta_listados 
   FROM    seg_modulo
   WHERE   modulo_cod = "uni"

   LET i = 1

   FOREACH cur_preunificador INTO rec_detalles.v_id_dh_dor,
                                  rec_detalles.v_nss,
                                  rec_detalles.v_id_unificador,
                                  rec_detalles.v_folio_unificacion

      IF rec_detalles.v_id_dh_dor IS NOT NULL THEN
         SELECT marca
         INTO   rec_detalles.v_marca
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor
         AND    marca = 511
         GROUP BY marca
         
         IF rec_detalles.v_marca = 511 THEN  
            LET rec_detalles.v_mov_imss = "PROSPECTO"
         END IF

         SELECT f_apertura
         INTO   rec_detalles.v_f_apertura
         FROM   afi_derechohabiente
         WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor 

         SELECT f_liquidacion, 
                f_notificacion,
                ind_procedencia
         INTO   rec_detalles.v_f_liquidacion,
                rec_detalles.v_op_22_notifica,
                rec_detalles.ind_procedencia
         FROM   uni_det_unificador
         WHERE  id_unificador = rec_detalles.v_id_unificador
         AND    estado_familia IN (1,2)
         GROUP BY f_liquidacion, 
                  f_notificacion,
                  ind_procedencia

         CASE rec_detalles.ind_procedencia
         WHEN 0
            LET rec_detalles.v_resp_procedencia = "POR CONFRONTAR"
         WHEN 1
            LET rec_detalles.v_resp_procedencia = "PROCEDENTE"
         WHEN 2
            LET rec_detalles.v_resp_procedencia = "NO PROCEDENTE"
         END CASE 

         IF rec_detalles.v_id_dh_dor IS NOT NULL THEN
            LET rec_detalles.v_tipo = "UNIFICADOR"
         END IF 

         SELECT f_actualiza
         INTO   rec_detalles.v_fecha_unificacion
         FROM   glo_folio 
         WHERE  folio = rec_detalles. v_folio_unificacion
         GROUP BY f_actualiza 

         IF rec_detalles.v_f_apertura IS NULL OR rec_detalles.v_f_apertura = "12/31/1899" THEN
            LET rec_detalles.v_f_apertura  = NULL 
         END IF 

         IF rec_detalles.v_fecha_unificacion IS NULL OR rec_detalles.v_fecha_unificacion = "12/31/1899" THEN
            LET rec_detalles.v_fecha_unificacion = NULL 
         END IF

         IF rec_detalles.v_op_22_notifica IS NULL OR rec_detalles.v_op_22_notifica = "12/31/1899" THEN
            LET rec_detalles.v_op_22_notifica = NULL 
         END IF

         LET arr_detalles[i].v_nss             = rec_detalles.v_nss
         LET arr_detalles[i].v_tipo            = rec_detalles.v_tipo
         LET arr_detalles[i].v_mov_imss        = rec_detalles.v_mov_imss
         LET arr_detalles[i].v_f_apertura      = rec_detalles.v_f_apertura
         LET arr_detalles[i].v_op_21_solicitud = rec_detalles.v_fecha_unificacion
         LET arr_detalles[i].v_op_21_respuesta = rec_detalles.v_resp_procedencia
         LET arr_detalles[i].v_op_22_notifica  = rec_detalles.v_op_22_notifica

         LET i = i + 1;

         FOREACH cur_preunificado USING rec_detalles.v_id_unificador
                                  INTO  rec_detalles.v_id_dh_ado,
                                        rec_detalles.v_nss,
                                        rec_detalles.v_folio_unificacion

            SELECT marca
            INTO   rec_detalles.v_marca
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_ado
            AND    marca = 512
            GROUP BY marca

            IF rec_detalles.v_marca = 512 THEN  
               LET rec_detalles.v_mov_imss = "PROSPECTO"
            END IF

            SELECT f_apertura
            INTO   rec_detalles.v_f_apertura
            FROM   afi_derechohabiente
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_ado

            SELECT f_liquidacion, 
                   f_notificacion,
                   ind_procedencia
            INTO   rec_detalles.v_f_liquidacion,
                   rec_detalles.v_op_22_notifica,
                   rec_detalles.ind_procedencia
            FROM   uni_det_unificador
            WHERE  id_unificador = rec_detalles.v_id_unificador
            AND    estado_familia IN (1,2)

            CASE rec_detalles.ind_procedencia
            WHEN 0
               LET rec_detalles.v_resp_procedencia = "POR CONFRONTAR"
            WHEN 1
               LET rec_detalles.v_resp_procedencia = "PROCEDENTE"
            WHEN 2
               LET rec_detalles.v_resp_procedencia = "NO PROCEDENTE"
            END CASE 

            IF rec_detalles.v_id_dh_dor IS NOT NULL THEN
               LET rec_detalles.v_tipo = "UNIFICADO"
            END IF 

            SELECT f_actualiza
            INTO   rec_detalles.v_fecha_unificacion
            FROM   glo_folio 
            WHERE  folio = rec_detalles. v_folio_unificacion
            GROUP BY f_actualiza

            IF rec_detalles.v_f_apertura IS NULL OR rec_detalles.v_f_apertura = "12/31/1899" THEN
               LET rec_detalles.v_f_apertura  = NULL 
            END IF 

            IF rec_detalles.v_fecha_unificacion IS NULL OR rec_detalles.v_fecha_unificacion = "12/31/1899" THEN
               LET rec_detalles.v_fecha_unificacion = NULL 
            END IF

            IF rec_detalles.v_op_22_notifica IS NULL OR rec_detalles.v_op_22_notifica = "12/31/1899" THEN
               LET rec_detalles.v_op_22_notifica = NULL 
            END IF

            LET arr_detalles[i].v_nss             = rec_detalles.v_nss
            LET arr_detalles[i].v_tipo            = rec_detalles.v_tipo
            LET arr_detalles[i].v_mov_imss        = rec_detalles.v_mov_imss
            LET arr_detalles[i].v_f_apertura      = rec_detalles.v_f_apertura
            LET arr_detalles[i].v_op_21_solicitud = rec_detalles.v_fecha_unificacion
            LET arr_detalles[i].v_op_21_respuesta = rec_detalles.v_resp_procedencia --rec_detalles.v_folio_resp_confronta
            LET arr_detalles[i].v_op_22_notifica  = rec_detalles.v_op_22_notifica

            LET i = i + 1;
         END FOREACH
      END IF
   END FOREACH
      DISPLAY i
      DISPLAY ARRAY arr_detalles TO scr_detalles.*
         BEFORE DISPLAY
            IF (i <= 1) THEN
               CALL fn_mensaje ("Atención", "No existe información con los datos proporcionados", "stop")
               EXIT DISPLAY
            ELSE
               CALL f.setElementHidden("gr_detalles",0)
            END IF

         ON ACTION reporte
            IF fgl_report_loadCurrentSettings("UNIC175.4rp") THEN
               CALL fgl_report_selectDevice ("PDF")
               {CALL fgl_report_selectPreview(0)
               LET v_nom_reporte = p_usuario CLIPPED || "_UNIC17-","00000","-","00000","-","00000"||".pdf"
               CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)
               }
               LET manejador_rpt = fgl_report_commitCurrentSettings()
            END IF

            --Inicia el reporte de detalles
            START REPORT rpt_detalles TO XML HANDLER manejador_rpt
               FOR v_ini = 1 TO i
                  OUTPUT TO REPORT rpt_detalles (p_usuario,
                                                 arr_detalles[v_ini].*
                                                 )
               END FOR
            FINISH REPORT rpt_detalles

         ON ACTION CANCEL
            CALL f.setElementHidden("gr_detalles",1)
            IF p_origen = 1 THEN 
               CALL f.setElementHidden("gr_nss",1)
            ELSE
               CALL f.setElementHidden("gr_folio",1)
               CALL f.setElementHidden("gr_folio_uni",1)
            END IF
            EXIT DISPLAY
      END DISPLAY
END FUNCTION

#OBJETIVO: Recueperar el detalle del posible UNIFICADO
FUNCTION fn_consulta_unificado(p_id_derechohabiente, p_origen, p_folio_lote, p_folio_uni)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       p_origen             SMALLINT,
       p_folio_lote         DECIMAL (9,0),
       p_folio_uni          DECIMAL (9,0),
       v_QryTxt             STRING,
       v_QryTxt1             STRING,
       i                    INTEGER,
       v_ini                INTEGER,
       manejador_rpt        om.SaxDocumentHandler,
       v_ruta_listados      CHAR(40),
       v_nom_reporte        CHAR(40)

DEFINE rec_detalles RECORD 
       v_id_dh_dor            DECIMAL(9,0),
       v_id_dh_ado            DECIMAL(9,0),
       v_id_unificador        DECIMAL(9,0),
       v_folio_unificacion    DECIMAL(9,0),
       v_folio_resp_confronta DECIMAL(9,0),
       v_f_unificacion        DATE, 
       v_f_liquidacion        DATE, 
       ind_procedencia        SMALLINT,
       v_nss                  CHAR(11),
       v_tipo                 CHAR(10),
       v_marca                SMALLINT,
       v_mov_imss             CHAR(10),
       v_f_apertura           DATE,
       v_op_22_notifica       DATE,
       v_resp_procedencia     CHAR(15),
       v_fecha_unificacion    DATE
END RECORD

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD 
          v_nss             CHAR(11),
          v_tipo            CHAR(10),
          v_mov_imss        CHAR(10),
          v_f_apertura      DATE,
          v_op_21_solicitud DATE,
          v_op_21_respuesta CHAR(30),
          v_op_22_notifica  DATE
END RECORD 

DISPLAY "Datos para consulta por unificado : ", p_id_derechohabiente," - ", p_origen, " - ",p_folio_lote," - ", p_folio_uni

   IF p_folio_lote IS NOT NULL THEN
      LET v_QryTxt = "\n SELECT id_derechohabiente, ",
                     "\n        nss_correcto,       ",
                     "\n        id_pre_unificador   ",
                     "\n FROM   uni_pre_unificador  "

      IF p_origen = 1 THEN 
         LET v_QryTxt = v_QryTxt || "\n WHERE  id_unificador = ? "
      END IF

      IF p_origen = 2 THEN 
         LET v_QryTxt = v_QryTxt || "\n WHERE  folio_lote = ",  p_folio_lote
      END IF

      LET v_QryTxt1 = "\n SELECT id_derechohabiente, ",
                     "\n         nss       ",
                     "\n FROM   uni_pre_unificado  ",
                     "\n WHERE  id_derechohabiente  = ? "
   END IF

   IF p_folio_uni IS NOT NULL THEN 
      LET v_QryTxt = "\n SELECT id_derechohabiente, ",
                     "\n        nss_unificador,       ",
                     "\n        id_unificador, ",
                     "\n        folio_unificacion ",
                     "\n FROM   uni_det_unificador  "

      IF p_origen = 1 THEN 
         LET v_QryTxt = v_QryTxt || "\n WHERE  id_unificador = ? ",
                                    "\n GROUP BY 1,2,3,4 "
      END IF
   
      IF p_origen = 2 THEN 
         LET v_QryTxt = v_QryTxt || "\n WHERE  folio_unificacion = ",  p_folio_uni,
                                    "\n GROUP BY 1,2,3,4 "
      END IF

      LET v_QryTxt1 = "\n SELECT id_derechohabiente, ",
                      "\n         nsscta1, ",
                      "\n         folio_unificacion, ",
                      "\n         id_unificador      ",
                      "\n FROM    uni_det_unificado  ",
                      "\n WHERE  id_derechohabiente = ? ",
                      "\n GROUP BY 1,2,3,4 "
   END IF

   PREPARE prp_preunificador_ado FROM v_QryTxt
   DECLARE cur_preunificador_ado CURSOR FOR prp_preunificador_ado

   PREPARE prp_preunificado_ado FROM v_QryTxt1
   DECLARE cur_preunificado_ado CURSOR FOR prp_preunificado_ado

   DISPLAY v_QryTxt
   DISPLAY v_QryTxt1
   LET i = 1

   FOREACH cur_preunificado_ado USING p_id_derechohabiente 
   --rec_detalles.v_id_unificador
                                INTO  rec_detalles.v_id_dh_ado,
                                      rec_detalles.v_nss,
                                      rec_detalles.v_folio_unificacion,
                                      rec_detalles.v_id_unificador

DISPLAY  rec_detalles.v_id_dh_ado,
                                      rec_detalles.v_nss,
                                      rec_detalles.v_fecha_unificacion,
                                      rec_detalles.v_id_unificador

      SELECT marca
      INTO   rec_detalles.v_marca
      FROM   sfr_marca_activa
      WHERE  id_derechohabiente = rec_detalles.v_id_dh_ado
      AND    marca = 512
      GROUP BY marca

      IF rec_detalles.v_marca = 512 THEN  
         LET rec_detalles.v_mov_imss = "PROSPECTO"
      END IF
  
      SELECT f_apertura
      INTO   rec_detalles.v_f_apertura
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = rec_detalles.v_id_dh_ado

      SELECT f_liquidacion, 
             f_notificacion,
             ind_procedencia
      INTO   rec_detalles.v_f_liquidacion,
             rec_detalles.v_op_22_notifica,
             rec_detalles.ind_procedencia
      FROM   uni_det_unificador
      WHERE  id_unificador = rec_detalles.v_id_unificador
      AND    estado_familia IN (1,2)

      CASE rec_detalles.ind_procedencia
      WHEN 0
         LET rec_detalles.v_resp_procedencia = "POR CONFRONTAR"
      WHEN 1
         LET rec_detalles.v_resp_procedencia = "PROCEDENTE"
      WHEN 2
         LET rec_detalles.v_resp_procedencia = "NO PROCEDENTE"
      END CASE 

      --IF rec_detalles.v_id_dh_dor IS NOT NULL THEN
      IF rec_detalles.v_id_dh_ado IS NOT NULL THEN
         LET rec_detalles.v_tipo = "UNIFICADO"
      END IF

      SELECT f_actualiza
      INTO   rec_detalles.v_fecha_unificacion
      FROM   glo_folio 
      WHERE  folio = rec_detalles. v_folio_unificacion
      GROUP BY f_actualiza

      IF rec_detalles.v_f_apertura IS NULL OR rec_detalles.v_f_apertura = "12/31/1899" THEN
         LET rec_detalles.v_f_apertura  = NULL
      END IF 

      IF rec_detalles.v_fecha_unificacion IS NULL OR rec_detalles.v_fecha_unificacion = "12/31/1899" THEN
         LET rec_detalles.v_fecha_unificacion = NULL
      END IF

      IF rec_detalles.v_op_22_notifica IS NULL OR rec_detalles.v_op_22_notifica = "12/31/1899" THEN 
         LET rec_detalles.v_op_22_notifica = NULL
      END IF

      LET arr_detalles[i].v_nss             = rec_detalles.v_nss
      LET arr_detalles[i].v_tipo            = rec_detalles.v_tipo
      LET arr_detalles[i].v_mov_imss        = rec_detalles.v_mov_imss
      LET arr_detalles[i].v_f_apertura      = rec_detalles.v_f_apertura
      LET arr_detalles[i].v_op_21_solicitud = rec_detalles.v_fecha_unificacion
      LET arr_detalles[i].v_op_21_respuesta = rec_detalles.v_resp_procedencia --rec_detalles.v_folio_resp_confronta
      LET arr_detalles[i].v_op_22_notifica  = rec_detalles.v_op_22_notifica

      LET i = i + 1

      FOREACH cur_preunificador_ado USING rec_detalles.v_id_unificador 
                                    INTO  rec_detalles.v_id_dh_dor,
                                          rec_detalles.v_nss,
                                          rec_detalles.v_id_unificador,
                                          rec_detalles.v_folio_unificacion

      DISPLAY rec_detalles.v_id_dh_dor,
              rec_detalles.v_nss,
              rec_detalles.v_id_unificador,
              rec_detalles.v_folio_unificacion

         IF rec_detalles.v_id_dh_dor IS NOT NULL THEN
            SELECT marca
            INTO   rec_detalles.v_marca
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor
            AND    marca = 511
            GROUP BY marca
            
            IF rec_detalles.v_marca = 511 THEN  
               LET rec_detalles.v_mov_imss = "PROSPECTO"
            END IF
      
            SELECT f_apertura
            INTO   rec_detalles.v_f_apertura
            FROM   afi_derechohabiente
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor 
      
            SELECT f_liquidacion, 
                   f_notificacion,
                   ind_procedencia
            INTO   rec_detalles.v_f_liquidacion,
                   rec_detalles.v_op_22_notifica,
                   rec_detalles.ind_procedencia
            FROM   uni_det_unificador
            WHERE  id_unificador = rec_detalles.v_id_unificador
            AND    estado_familia IN (1,2)
            GROUP BY f_liquidacion, 
                     f_notificacion,
                     ind_procedencia

            CASE rec_detalles.ind_procedencia
            WHEN 0
               LET rec_detalles.v_resp_procedencia = "POR CONFRONTAR"
            WHEN 1
               LET rec_detalles.v_resp_procedencia = "PROCEDENTE"
            WHEN 2
               LET rec_detalles.v_resp_procedencia = "NO PROCEDENTE"
            END CASE 

            IF rec_detalles.v_id_dh_dor IS NOT NULL THEN
               LET rec_detalles.v_tipo = "UNIFICADOR"
            END IF 

            SELECT f_actualiza
            INTO   rec_detalles.v_fecha_unificacion
            FROM   glo_folio 
            WHERE  folio = rec_detalles. v_folio_unificacion
            GROUP BY f_actualiza 

            IF rec_detalles.v_f_apertura IS NULL OR rec_detalles.v_f_apertura = "12/31/1899" THEN
               LET rec_detalles.v_f_apertura  = NULL 
            END IF 

            IF rec_detalles.v_fecha_unificacion IS NULL OR rec_detalles.v_fecha_unificacion = "12/31/1899" THEN
               LET rec_detalles.v_fecha_unificacion = NULL 
            END IF 

            IF rec_detalles.v_op_22_notifica IS NULL OR rec_detalles.v_op_22_notifica = "12/31/1899" THEN 
               LET rec_detalles.v_op_22_notifica = NULL 
            END IF

            LET arr_detalles[i].v_nss             = rec_detalles.v_nss
            LET arr_detalles[i].v_tipo            = rec_detalles.v_tipo
            LET arr_detalles[i].v_mov_imss        = rec_detalles.v_mov_imss
            LET arr_detalles[i].v_f_apertura      = rec_detalles.v_f_apertura
            LET arr_detalles[i].v_op_21_solicitud = rec_detalles.v_fecha_unificacion
            LET arr_detalles[i].v_op_21_respuesta = rec_detalles.v_resp_procedencia
            LET arr_detalles[i].v_op_22_notifica  = rec_detalles.v_op_22_notifica

            LET i = i + 1;
         END IF
      END FOREACH
   END FOREACH

      DISPLAY "total registros : ", i
      DISPLAY ARRAY arr_detalles TO scr_detalles.*
         BEFORE DISPLAY
            IF (i <= 1) THEN
               CALL fn_mensaje ("Atención", "No existe información con los datos proporcionados", "stop")
               EXIT DISPLAY
            ELSE
               CALL f.setElementHidden("gr_detalles",0)
            END IF

         ON ACTION reporte
            IF fgl_report_loadCurrentSettings("UNIC175.4rp") THEN
               CALL fgl_report_selectDevice ("PDF")
               {CALL fgl_report_selectPreview(0)
               CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)}
               LET manejador_rpt = fgl_report_commitCurrentSettings()
            END IF

            --Inicia el reporte de detalles
            START REPORT rpt_detalles TO XML HANDLER manejador_rpt
               FOR v_ini = 1 TO i
                  OUTPUT TO REPORT rpt_detalles (p_usuario,
                                                 arr_detalles[v_ini].*
                                                 )
               END FOR
            FINISH REPORT rpt_detalles

         ON ACTION CANCEL
            CALL f.setElementHidden("gr_detalles",1)
            IF p_origen = 1 THEN 
               CALL f.setElementHidden("gr_nss",1)
            ELSE
               CALL f.setElementHidden("gr_folio",1)
               CALL f.setElementHidden("gr_folio_uni",1)
            END IF
            EXIT DISPLAY
      END DISPLAY

END FUNCTION

REPORT rpt_detalles(p_usuario, arr_detalles)
DEFINE p_usuario CHAR(20),
       v_fecha_reporte DATE
DEFINE arr_detalles RECORD 
          v_nss             CHAR(11),
          v_tipo            CHAR(10),
          v_mov_imss        CHAR(10),
          v_f_apertura      DATE,
          v_op_21_solicitud DATE,
          v_op_21_respuesta CHAR(30),
          v_op_22_notifica  DATE
END RECORD 
FORMAT

   FIRST PAGE HEADER                            
      LET v_fecha_reporte    = TODAY CLIPPED

      PRINTX v_fecha_reporte USING "DD-MM-YYYY" 
      PRINTX p_usuario

   ON EVERY ROW
      PRINTX arr_detalles.v_nss
      PRINTX arr_detalles.v_tipo
      PRINTX arr_detalles.v_mov_imss
      PRINTX arr_detalles.v_f_apertura USING "DD-MM-YYYY"
      PRINTX arr_detalles.v_op_21_solicitud USING "DD-MM-YYYY"
      PRINTX arr_detalles.v_op_21_respuesta 
      PRINTX arr_detalles.v_op_22_notifica  

END REPORT