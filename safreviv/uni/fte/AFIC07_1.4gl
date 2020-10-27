################################################################################
#Modulo       => AFI                                                           #
#Programa     => AFIC07                                                        #
#Objetivo     => Consulta prospecto de unificación                             #
#Fecha inicio => 14/12/2015                                                    #
################################################################################

--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
-- 20/11/2014 Se adecúa para ejecución de unificación manual AG
--==============================================================================

DATABASE safre_viv

   DEFINE p_usuario                CHAR(20)      -- Obtiene dato de usuario
   DEFINE p_tipo_ejecucion         SMALLINT      -- Forma como ejecutará el programa
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE v_extension              STRING        -- Variable para validar que la extensión sea ".txt"
   DEFINE v_nom_archivo            STRING        -- Variable para validar que el nombre de archivo comienze con ""
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate -- Ruta donde se deja archivo rescatado del usuario
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio-- Ruta donde se deja archivo rescatado del usuario
   DEFINE v_nom_arh_rescate        STRING        -- Nombre de archivo en ruta rescate
   DEFINE y                        INTEGER       -- Contador para arreglo de unificados
   DEFINE v_nss                    CHAR(11)      -- id derechohabiente para nss consultado
   DEFINE v_cta_unificado          INTEGER       -- variable valida existencia de NSS en tabla unificados
   DEFINE v_cta_unificador         INTEGER       -- variable valida existencia en tabla unificador
   DEFINE r_bandera                INTEGER       -- Bandera para llamar llenado de tabla 
   DEFINE v_id_pre_unificador      INTEGER       -- variable donde se allmacena id_pre_identificador
   DEFINE v_query_unificado        STRING        -- Consulta de unificados
   DEFINE c                        INTEGER       -- Contador General para llenar tabla de porspectos de unificación
   DEFINE ch                       base.Channel 
   DEFINE tok                      base.StringTokenizer
   DEFINE buf                      base.StringBuffer

   DEFINE arr_tabla_prospecto DYNAMIC ARRAY OF RECORD -- arreglo para desplegar datos de unificador y unificado
        nss                       CHAR(11),
        tipo                      CHAR(20),
        --marca                     SMALLINT,
        mov_imss                  CHAR(20),
        f_movimiento              DATE,
        op_21_disp                CHAR(30),
        op_21_confronta           CHAR(30),
        op_22_conclucion          CHAR(30)
   END RECORD

   DEFINE arr_tabla DYNAMIC ARRAY OF RECORD -- aareglo para almacenar datos de nss unificado
          nss                      CHAR(11),
          estado                   SMALLINT,
          f_ap_unificado           DATE
    END RECORD

   DEFINE arr_tabla_tmp_unificador DYNAMIC ARRAY OF RECORD --a arreglo para almacenar datos de nss unificador
        nss                        CHAR(11),
        id                         INTEGER ,
        f_apertura                 DATE,
        id_pre_unificador          INTEGER,
        estado                     SMALLINT
   END RECORD


MAIN
   -- se recupera la clave de usuario desde parámetro
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".AFIC07.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   MENU "Seleccionar modo de entrada"

   ON ACTION nss
      CALL fn_nss(1)
      EXIT MENU

   ON ACTION folio
      CALL fn_folio(2)
      EXIT MENU
      
   --ON ACTION archivo
      --CALL fn_archivo()
     -- EXIT MENU

   ON ACTION CANCEL
      EXIT MENU

   END MENU

END MAIN

#OBJETIVO:  Consultar la información del NSS capturado.
FUNCTION fn_nss(p_origen)
DEFINE v_nss                VARCHAR(11),
       v_id_derechohabiente DECIMAL(9,0),
       v_tipo_nss           SMALLINT,
       p_origen             SMALLINT
       
   OPEN WINDOW w_nss WITH FORM "AFIC072"

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
            DISPLAY v_id_derechohabiente
               CALL fn_tipo_nss(v_id_derechohabiente)
                    RETURNING v_tipo_nss

               IF v_tipo_nss = 1 THEN 
                  DISPLAY "Consulta por UNIFICADOR "
                  CALL fn_consulta_unificador(v_id_derechohabiente, p_origen, 0)
               ELSE
                  IF v_tipo_nss = 2 THEN 
                     DISPLAY "Consulta por UNIFICADO "
                     CALL fn_consulta_unificado(v_id_derechohabiente, p_origen, 0)
                  END IF

                  IF v_tipo_nss = 0 THEN 
                     CALL fn_mensaje ("Archivo","El NSS no existe como prospecto","information")
                  END IF
                  
               END IF

               DISPLAY "NSS capturado : ", v_nss
               INITIALIZE v_nss TO NULL 
               CONTINUE INPUT
            END IF 
         END IF

      ON ACTION CLOSE
         EXIT INPUT
      END INPUT
   CLOSE WINDOW w_nss

END FUNCTION 

FUNCTION fn_folio(p_origen)
DEFINE p_origen     SMALLINT,
       v_folio      DECIMAL(9,0),
       v_cadena     STRING,
       v_cbx_folios ui.ComboBox,
       i            INTEGER
       
DEFINE v_r_glo_ctr_archivo RECORD
          folio          DECIMAL(9,0),
          nombre_archivo CHAR(40)
END RECORD


   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio WITH FORM "AFIC074"

   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   CALL v_cbx_folios.clear()
   CALL v_cbx_folios.addItem(-1," ")
   
   INPUT v_folio WITHOUT DEFAULTS
   FROM  cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         LET v_folio = -1       

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
            CALL v_cbx_folios.addItem(
                 v_r_glo_ctr_archivo.folio USING "##########", v_cadena)

            LET i = i + 1
         END FOREACH
         IF(i<1)THEN
            CALL fn_mensaje("Atención",
                            "No existen información para consultar",
                            "info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL OR v_folio = -1 ) THEN
            CALL fn_mensaje("Atención","Es necesario seleccionar un folio","stop")
            CONTINUE INPUT
         ELSE
            --CALL fn_detalles_nss(0, p_origen,v_folio, 0)
            CALL fn_consulta_unificador(0, p_origen,v_folio)
         END IF

      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT   
   CLOSE WINDOW w_folio
END FUNCTION 

FUNCTION fn_tipo_nss(p_id_derechohabiente)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       v_id_pre_unificador  DECIMAL (9,0),
       v_id_pre_unificado   DECIMAL (9,0),
       v_tipo_nss           SMALLINT

 DISPLAY p_id_derechohabiente
 
   --Identificar si es unificador o unificado
   SELECT id_derechohabiente 
   INTO   v_id_pre_unificador
   FROM   uni_pre_unificador
   WHERE  id_derechohabiente = p_id_derechohabiente
   GROUP BY 1

   IF v_id_pre_unificador IS NOT NULL THEN
      SELECT id_derechohabiente 
      INTO   v_id_pre_unificado 
      FROM   uni_pre_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      GROUP BY 1

      IF v_id_pre_unificado IS NOT NULL THEN
         LET v_tipo_nss = 3
         --CALL fn_mensaje ("Archivo","El NSS es unificador y unificado","information")
      ELSE 
         LET v_tipo_nss = 1
         --CALL fn_mensaje ("Archivo","El NSS es unificador","information")
      END IF
   ELSE
      SELECT id_derechohabiente 
      INTO   v_id_pre_unificado 
      FROM   uni_pre_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      GROUP BY 1

      IF v_id_pre_unificado IS NOT NULL THEN
         LET v_tipo_nss = 2
         --CALL fn_mensaje ("Archivo","El NSS es unificado","information")
      END IF
   END IF 

   RETURN v_tipo_nss
END FUNCTION 


FUNCTION fn_detalles_nss(p_id_derechohabiente, p_origen, p_folio_lote,p_tipo_nss)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       p_tipo_nss           SMALLINT,
       p_origen             SMALLINT,
       p_folio_lote         DECIMAL (9,0)

   IF p_tipo_nss = 1 THEN 
      DISPLAY "Consulta por UNIFICADOR "
      --CALL fn_consulta_unificador(p_id_derechohabiente, p_origen, p_folio_lote,p_tipo_nss)
   ELSE
      IF p_tipo_nss = 2 THEN 
         DISPLAY "Consulta por UNIFICADO "
      END IF
   END IF
END FUNCTION 

#OBJETIVO: Recueperar el detalle del posible UNIFICADOR
FUNCTION fn_consulta_unificador(p_id_derechohabiente, p_origen, p_folio_lote)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       p_tipo_nss           SMALLINT,
       p_origen             SMALLINT,
       p_folio_lote         DECIMAL (9,0),
       v_QryTxt             STRING,
       i                    INTEGER,
       v_n_referencia       DECIMAL(9,0),
       v_ini                INTEGER,
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
       v_resp_procedencia     CHAR(15)
END RECORD

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD 
          v_nss             CHAR(11),
          v_tipo            CHAR(10),
          --v_marca           SMALLINT,
          v_mov_imss        CHAR(10),
          v_f_apertura      DATE,
          --v_op_21_solicitud DECIMAL(9,0),
          --v_op_21_respuesta DECIMAL(9,0),
          v_op_21_solicitud DATE,
          v_op_21_respuesta CHAR(15),
          v_op_22_notifica  DATE
END RECORD 
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

   PREPARE prp_preunificador FROM v_QryTxt
   DECLARE cur_preunificador CURSOR FOR prp_preunificador

   LET v_QryTxt = "\n SELECT id_derechohabiente, ",
                  "\n        nss       ",
                  "\n FROM   uni_pre_unificado  ",
                  "\n WHERE  id_pre_unificador = ? "

   PREPARE prp_preunificado FROM v_QryTxt
   DECLARE cur_preunificado CURSOR FOR prp_preunificado

   LET i = 1 
   
   FOREACH cur_preunificador INTO rec_detalles.v_id_dh_dor,
                                  rec_detalles.v_nss,
                                  rec_detalles.v_id_unificador

      IF rec_detalles.v_id_dh_dor IS NOT NULL THEN
         SELECT marca
         INTO   rec_detalles.v_marca
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor
         AND    marca = 511
         
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
         WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor

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

         IF rec_detalles.v_f_apertura IS NULL OR rec_detalles.v_f_apertura = "12/31/1899" THEN
            LET rec_detalles.v_f_apertura  = NULL 
         END IF 
         
         IF rec_detalles.v_op_22_notifica IS NULL OR rec_detalles.v_op_22_notifica = "12/31/1899" THEN 
            LET rec_detalles.v_op_22_notifica = NULL 
         END IF     

         LET arr_detalles[i].v_nss             = rec_detalles.v_nss
         LET arr_detalles[i].v_tipo            = rec_detalles.v_tipo
         --LET arr_detalles[i].v_marca           = rec_detalles.v_marca
         LET arr_detalles[i].v_mov_imss        = rec_detalles.v_mov_imss
         LET arr_detalles[i].v_f_apertura      = rec_detalles.v_f_apertura
         LET arr_detalles[i].v_op_21_solicitud = rec_detalles.v_f_liquidacion--rec_detalles.v_folio_unificacion
         LET arr_detalles[i].v_op_21_respuesta = rec_detalles.v_folio_resp_confronta
         LET arr_detalles[i].v_op_22_notifica  = rec_detalles.v_op_22_notifica

         FOREACH cur_preunificado USING rec_detalles.v_id_unificador
                                  INTO  rec_detalles.v_id_dh_ado,
                                        rec_detalles.v_nss

            SELECT marca
            INTO   rec_detalles.v_marca
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_ado
            AND    marca = 512
            GROUP BY 1

            IF rec_detalles.v_marca = 512 THEN  
               LET rec_detalles.v_mov_imss = "PROSPECTO"
            END IF
  
            SELECT f_apertura
            INTO   rec_detalles.v_f_apertura
            FROM   afi_derechohabiente
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_ado
         
            {SELECT folio_unificacion
                   folio_liquidacion,
                   f_notificacion
            INTO   rec_detalles.v_folio_unificacion,
                   rec_detalles.v_folio_resp_confronta,
                   rec_detalles.v_op_22_notifica
            FROM   uni_det_unificador
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor}

            SELECT f_liquidacion, 
                   f_notificacion,
                   ind_procedencia
            INTO   rec_detalles.v_f_liquidacion,
                   rec_detalles.v_op_22_notifica,
                   rec_detalles.ind_procedencia
            FROM   uni_det_unificador
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor

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

            IF rec_detalles.v_f_apertura IS NULL OR rec_detalles.v_f_apertura = "12/31/1899" THEN
               LET rec_detalles.v_f_apertura  = NULL 
            END IF 

            IF rec_detalles.v_op_22_notifica IS NULL OR rec_detalles.v_op_22_notifica = "12/31/1899" THEN 
               LET rec_detalles.v_op_22_notifica = NULL 
            END IF     

            LET i = i + 1;
            
            LET arr_detalles[i].v_nss             = rec_detalles.v_nss
            LET arr_detalles[i].v_tipo            = rec_detalles.v_tipo
            --LET arr_detalles[i].v_marca           = rec_detalles.v_marca
            LET arr_detalles[i].v_mov_imss        = rec_detalles.v_mov_imss
            LET arr_detalles[i].v_f_apertura      = rec_detalles.v_f_apertura
            --LET arr_detalles[i].v_op_21_solicitud = rec_detalles.v_folio_unificacion
            LET arr_detalles[i].v_op_21_respuesta = rec_detalles.v_folio_resp_confronta
            LET arr_detalles[i].v_op_22_notifica  = rec_detalles.v_op_22_notifica
         END FOREACH
      END IF
   END FOREACH

   --LET i = i - 1; 
DISPLAY i
   OPEN WINDOW v_detalles WITH FORM "AFIC073"   
      DISPLAY ARRAY arr_detalles TO scr_detalles.*
         BEFORE DISPLAY
         IF (i < 1) THEN
            CALL fn_mensaje ("Atención", "No existe información para el NSS capturado", "stop")
            EXIT DISPLAY 
         END IF

         ON ACTION reporte
            IF fgl_report_loadCurrentSettings("AFIC075.4rp") THEN
               CALL fgl_report_selectDevice ("PDF")
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
            EXIT PROGRAM
      END DISPLAY
   CLOSE WINDOW v_detalles
END FUNCTION

#OBJETIVO: Recueperar el detalle del posible UNIFICADO
FUNCTION fn_consulta_unificado(p_id_derechohabiente, p_origen, p_folio_lote)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       p_tipo_nss           SMALLINT,
       p_origen             SMALLINT,
       p_folio_lote         DECIMAL (9,0),
       v_QryTxt             STRING,
       i                    INTEGER,
       v_ini                INTEGER,
       v_n_referencia       DECIMAL(9,0),
       manejador_rpt        om.SaxDocumentHandler  

DEFINE rec_detalles RECORD 
       v_id_dh_dor            DECIMAL(9,0),
       v_id_dh_ado            DECIMAL(9,0),
       v_id_unificador        DECIMAL(9,0),
       v_folio_unificacion    DECIMAL(9,0),
       v_folio_resp_confronta DECIMAL(9,0),
       v_f_liquidacion        DATE,
       v_resp_procedencia     CHAR(15),
       ind_procedencia        SMALLINT,
       v_nss                  CHAR(11),
       v_tipo                 CHAR(10),
       v_marca                SMALLINT,
       v_mov_imss             SMALLINT,
       v_f_apertura           DATE,
       v_op_22_notifica       DATE
END RECORD

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD 
          v_nss             CHAR(11),
          v_tipo            CHAR(10),
          --v_marca           SMALLINT,
          v_mov_imss        SMALLINT,
          v_f_apertura      DATE,
          --v_op_21_solicitud DECIMAL(9,0),
          --v_op_21_respuesta DECIMAL(9,0),
          v_op_21_solicitud DATE,
          v_op_21_respuesta CHAR(15),
          v_op_22_notifica  DATE
END RECORD 

   LET v_QryTxt = "\n SELECT id_derechohabiente, ",
                  "\n        nss, ",
                  "\n        id_pre_unificador ",
                  "\n FROM   uni_pre_unificado "

   IF p_origen = 1 THEN 
      LET v_QryTxt = v_QryTxt || "\n WHERE  id_derechohabiente = ",  p_id_derechohabiente
   END IF

   IF p_origen = 2 THEN 
      LET v_QryTxt = v_QryTxt || "\n WHERE  folio_lote = ",  p_folio_lote
   END IF

   PREPARE prp_preunificado_1 FROM v_QryTxt
   DECLARE cur_preunificado_1 CURSOR FOR prp_preunificado_1

   LET i = 1 

   FOREACH cur_preunificado_1 INTO rec_detalles.v_id_dh_ado,
                                   rec_detalles.v_nss,
                                   rec_detalles.v_id_unificador

      IF rec_detalles.v_id_unificador IS NOT NULL THEN
         SELECT marca
         INTO   rec_detalles.v_marca
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = rec_detalles.v_id_dh_ado
         AND    marca = 512

         IF rec_detalles.v_marca = 512 THEN  
            LET rec_detalles.v_mov_imss = "PROSPECTO"
         END IF         

         SELECT f_apertura
         INTO   rec_detalles.v_f_apertura
         FROM   afi_derechohabiente
         WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor
         
         SELECT id_unificador 
         INTO   rec_detalles.v_id_dh_dor
         FROM   uni_det_unificado
         WHERE  id_derechohabiente = rec_detalles.v_id_dh_ado
         AND    estado_unificacion = 1

         SELECT f_liquidacion, 
                f_notificacion,
                ind_procedencia
         INTO   rec_detalles.v_f_liquidacion,
                rec_detalles.v_op_22_notifica,
                rec_detalles.ind_procedencia
         FROM   uni_det_unificador
         WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor

         CASE rec_detalles.ind_procedencia
            WHEN 0
               LET rec_detalles.v_resp_procedencia = "POR CONFRONTAR"
            WHEN 1
               LET rec_detalles.v_resp_procedencia = "PROCEDENTE"
            WHEN 2
               LET rec_detalles.v_resp_procedencia = "NO PROCEDENTE"
         END CASE 
            
         IF rec_detalles.v_id_dh_ado IS NOT NULL THEN
            LET rec_detalles.v_tipo = "UNIFICADO"
         END IF 
         
         IF rec_detalles.v_f_apertura IS NULL OR rec_detalles.v_f_apertura = "12/31/1899" THEN
            LET rec_detalles.v_f_apertura  = NULL 
         END IF 

         IF rec_detalles.v_op_22_notifica IS NULL OR rec_detalles.v_op_22_notifica = "12/31/1899" THEN 
            LET rec_detalles.v_op_22_notifica = NULL 
         END IF     

         LET arr_detalles[i].v_nss             = rec_detalles.v_nss
         LET arr_detalles[i].v_tipo            = rec_detalles.v_tipo
         --LET arr_detalles[i].v_marca           = rec_detalles.v_marca
         LET arr_detalles[i].v_mov_imss        = rec_detalles.v_mov_imss
         LET arr_detalles[i].v_f_apertura      = rec_detalles.v_f_apertura
         LET arr_detalles[i].v_op_21_solicitud = rec_detalles.v_folio_unificacion
         LET arr_detalles[i].v_op_21_respuesta = rec_detalles.v_folio_resp_confronta
         LET arr_detalles[i].v_op_22_notifica  = rec_detalles.v_op_22_notifica

         LET i = i + 1;
      END IF
   END FOREACH

   LET v_QryTxt = "\n SELECT id_derechohabiente, ",
                  "\n        nss_correcto        ",
                  "\n FROM   uni_pre_unificador  ",
                  "\n WHERE  id_pre_unificador = ", rec_detalles.v_id_unificador

      PREPARE prp_preunificador_1 FROM v_QryTxt
      DECLARE cur_preunificador_1 CURSOR FOR prp_preunificador_1

         FOREACH cur_preunificador_1 INTO  rec_detalles.v_id_dh_dor,
                                           rec_detalles.v_nss
            SELECT marca
            INTO   rec_detalles.v_marca
            FROM   sfr_marca_activa
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor
            AND    marca = 511

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
            WHERE  id_derechohabiente = rec_detalles.v_id_dh_dor

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

            IF rec_detalles.v_f_apertura IS NULL OR rec_detalles.v_f_apertura = "12/31/1899" THEN
               LET rec_detalles.v_f_apertura  = NULL 
            END IF 

            IF rec_detalles.v_op_22_notifica IS NULL OR rec_detalles.v_op_22_notifica = "12/31/1899" THEN 
               LET rec_detalles.v_op_22_notifica = NULL 
            END IF     
            
            LET arr_detalles[i].v_nss             = rec_detalles.v_nss
            LET arr_detalles[i].v_tipo            = rec_detalles.v_tipo
            --LET arr_detalles[i].v_marca           = rec_detalles.v_marca
            LET arr_detalles[i].v_mov_imss        = rec_detalles.v_mov_imss
            LET arr_detalles[i].v_f_apertura      = rec_detalles.v_f_apertura
            LET arr_detalles[i].v_op_21_solicitud = rec_detalles.v_folio_unificacion
            LET arr_detalles[i].v_op_21_respuesta = rec_detalles.v_folio_resp_confronta
            LET arr_detalles[i].v_op_22_notifica  = rec_detalles.v_op_22_notifica

            LET i = i + 1;
         END FOREACH

   OPEN WINDOW v_detalles WITH FORM "AFIC073"   
      DISPLAY ARRAY arr_detalles TO scr_detalles.*
         BEFORE DISPLAY
         IF (i < 1) THEN
            CALL fn_mensaje ("Atención", "No existe información para el NSS capturado", "stop")
            EXIT DISPLAY 
         END IF

         ON ACTION reporte
            IF fgl_report_loadCurrentSettings("AFIC075.4rp") THEN
               CALL fgl_report_selectDevice ("PDF")
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
            EXIT PROGRAM
      END DISPLAY
   CLOSE WINDOW v_detalles
END FUNCTION

REPORT rpt_detalles(p_usuario, arr_detalles)
DEFINE p_usuario CHAR(20),
       v_fecha_reporte DATE
DEFINE arr_detalles RECORD 
          v_nss             CHAR(11),
          v_tipo            CHAR(10),
          --v_marca           SMALLINT,
          v_mov_imss        CHAR(10),
          v_f_apertura      DATE,
          v_op_21_solicitud DECIMAL(9,0),
          v_op_21_respuesta DECIMAL(9,0),
          v_op_22_notifica  DATE
END RECORD 
FORMAT

   FIRST PAGE HEADER                            
      LET v_fecha_reporte    = TODAY CLIPPED

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario

   ON EVERY ROW
      PRINTX arr_detalles.v_nss
      PRINTX arr_detalles.v_tipo
      --PRINTX arr_detalles.v_marca
      PRINTX arr_detalles.v_mov_imss
      PRINTX arr_detalles.v_f_apertura
      PRINTX arr_detalles.v_op_21_solicitud
      PRINTX arr_detalles.v_op_21_respuesta
      PRINTX arr_detalles.v_op_22_notifica 

END REPORT