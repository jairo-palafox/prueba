################################################################################
#Modulo       => LAV                                                           #
#Programa     => LAVC01                                                        #
#Objetivo     => Consulta de detalles de Prevención de Lavado de Dinero        #
#Fecha inicio => 30/12/2014                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

GLOBALS
DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod,        -- Usuario firmado 
       g_pid            LIKE bat_ctr_proceso.pid,            -- ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod,        -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod,        -- codigo de operacion,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo  -- Nombre Archivo Procesado
DEFINE g_reg_modulo   RECORD
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
END RECORD
DEFINE seg_modulo_bat RECORD
        ruta_listados    CHAR(40)   
END RECORD

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD 
          v_folio          DECIMAL (9,0),
          v_reporte        CHAR (30),
          v_nss            CHAR (11),
          v_nombre_compl   CHAR (130),
          v_operacion      CHAR (30),
          v_fecha_mvto     DATE,
          v_no_depositos   INTEGER,
          v_subcuenta      SMALLINT,
          v_pesos          DECIMAL(16,4),
          v_dolares        DECIMAL(16,4),
          v_precio_dolar   DECIMAL(16,4),
          v_validacion     CHAR(30),
          v_estado         CHAR (30)
END RECORD 

DEFINE arr_totales DYNAMIC ARRAY OF RECORD
          v_tot_validacion CHAR(30),
          v_tot_registros  INTEGER,
          v_tot_pesos      DECIMAL(16,4),
          v_tot_dolares    DECIMAL(16,4) 
END RECORD
END GLOBALS

MAIN
DEFINE f_ventana     ui.window
DEFINE f_forma       ui.form
DEFINE manejador_rpt om.SaxDocumentHandler
DEFINE v_folio           DECIMAL(9,0), 
       v_tipo_reporte    SMALLINT, 
       v_tipo_validacion SMALLINT, 
       v_estado          SMALLINT,
       v_nss             CHAR(11),
       v_fecha_ini       DATE,
       v_fecha_fin       DATE,
       v_tot_regs        INTEGER,
       p_tipo_ejecucion  SMALLINT,
       p_titulo          STRING,
       v_inicia          INTEGER 
DEFINE r_tot_validacion  CHAR(30),
       r_tot_pesos       DECIMAL(16,4),
       r_tot_dolar       DECIMAL(16,4) 
       
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
    
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, 
          s.ruta_rescate, 
          s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'lav'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_genera WITH FORM "LAVC011"
   DIALOG ATTRIBUTES(UNBUFFERED) 

   --Se obtienen los parámetros de folio, estado, NSS para consulta
   INPUT v_folio, v_nss,  v_tipo_reporte, v_tipo_validacion, v_estado,v_fecha_ini, v_fecha_fin
   FROM  cb_folio, ed_nss,cb_tipo_reporte, cb_tipo_validacion, cb_estado, de_f_ini, de_f_fin
               
   BEFORE INPUT

      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()

      CALL f_forma.setelementhidden("gr_detalles", 1)
      CALL f_forma.setelementhidden("gr_totales", 1)
      CALL fn_llena_combos()
   END INPUT

   DISPLAY ARRAY arr_detalles TO scr_detalles.*
      ON ACTION exportar
          CALL fn_genera_salida(v_folio, v_nss,  v_tipo_reporte, v_tipo_validacion, v_estado,v_fecha_ini, v_fecha_fin)

      ON ACTION reporte
         IF fgl_report_loadCurrentSettings("LAVC013.4rp") THEN
            CALL fgl_report_selectDevice ("PDF")
            LET manejador_rpt = fgl_report_commitCurrentSettings()
         END IF
                  
         --Inicia el reporte de detalles
         START REPORT rpt_detalle TO XML HANDLER manejador_rpt
         
            FOR v_inicia = 1 TO v_tot_regs
               OUTPUT TO REPORT rpt_detalle(g_usuario_cod,
                                            arr_detalles[v_inicia].*,
                                            r_tot_validacion,
                                            v_tot_regs,
                                            r_tot_pesos,
                                            r_tot_dolar
                                            )
            END FOR
         FINISH REPORT rpt_detalle
   END DISPLAY
         
   DISPLAY ARRAY arr_totales TO scr_totales.*
   END DISPLAY   
   
   ON ACTION ACCEPT
      CALL fn_consulta_detalles(v_folio, v_nss,v_tipo_reporte, v_tipo_validacion, v_estado,v_fecha_ini, v_fecha_fin)
           RETURNING v_tot_regs, r_tot_validacion,r_tot_pesos,r_tot_dolar

      IF v_tot_regs >= 1 THEN
         CALL f_forma.setelementhidden("gr_detalles", 0)
         CALL f_forma.setelementhidden("gr_totales", 0)
      ELSE 
         CALL fn_mensaje ("Atención", 
                          "No existen registros con los parámetros seleccionados",
                          "stop")
      END IF

   ON ACTION cancelar 
      EXIT DIALOG   
   END DIALOG    

   
   CLOSE WINDOW w_folio_genera

END MAIN

#OBJETIVO: Llenar los combos que se utilizan como parámetro en la consulta
FUNCTION fn_llena_combos()
--Combo folio
DEFINE v_cbx_folios ui.ComboBox,
       v_indice     SMALLINT
DEFINE arr_folios RECORD 
          v_folio DECIMAL(9,0),
          v_g_folio DECIMAL(9,0)
END RECORD

DEFINE arr_tipo_reporte RECORD
          id_tipo_reporte  DECIMAL(9,0),
          tipo_reporte     SMALLINT,
          des_tipo_reporte CHAR(30)
END RECORD 

DEFINE arr_tipo_validacion RECORD
          id_lav_tipo_validacion DECIMAL(9,0),
          des_validacion         CHAR(40)
END RECORD

DEFINE arr_tipo_estado RECORD
          id_lav_estado DECIMAL(9,0),
          descripcion         CHAR(40)
END RECORD

   LET v_cbx_folios = ui.ComboBox.forName("cb_folio") --Asignación del combo a la forma 
   LET v_indice = 1
   
   DECLARE cur_folios CURSOR FOR SELECT a.folio, b.folio
                                 FROM   lav_det_lavado a, 
                                        glo_folio b
                                 WHERE  a.folio = b.folio 
                                 AND    b.status = 2
                                 GROUP BY 1,2
                                 ORDER BY 1,2
   LET v_indice = 0

   FOREACH cur_folios INTO arr_folios.v_folio,
                           arr_folios.v_g_folio

      --LET v_folio_desc = arr_folios.v_g_folio
      
      CALL v_cbx_folios.addItem(arr_folios.v_folio,arr_folios.v_g_folio)
      -- Contador de archivos eoncontrados
      LET v_indice = v_indice + 1
   END FOREACH
--
--Combo Tipo reporte
   LET v_cbx_folios = ui.ComboBox.forName("cb_tipo_reporte") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF v_cbx_folios IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1

   DECLARE cur_tipo_reporte CURSOR FOR SELECT id_tipo_reporte,
                                              tipo_reporte,
                                              des_tipo_reporte
                                       FROM   lav_tipo_reporte
                                       GROUP BY 1,2,3
                                       ORDER BY 1,2,3
   LET v_indice = 0

   FOREACH cur_tipo_reporte INTO arr_tipo_reporte.id_tipo_reporte,
                              arr_tipo_reporte.tipo_reporte,
                              arr_tipo_reporte.des_tipo_reporte
                           
      CALL v_cbx_folios.addItem(arr_tipo_reporte.id_tipo_reporte, 
                                --arr_tipo_reporte.tipo_reporte || " - " ||
                                arr_tipo_reporte.des_tipo_reporte)

      -- Contador de archivos eoncontrados
      LET v_indice = v_indice + 1
   END FOREACH
--   
--Combo Tipo validación
   LET v_cbx_folios = ui.ComboBox.forName("cb_tipo_validacion") --Asignación del combo a la forma
 
   -- Validación si el combo es nulo 
   IF v_cbx_folios IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1
   
   DECLARE cur_tipo_validacion CURSOR FOR SELECT id_lav_tipo_validacion,
                                           des_validacion
                                    FROM   lav_tipo_validaciones
                                    GROUP BY 1,2
                                    ORDER BY 1,2
   LET v_indice = 0

   FOREACH cur_tipo_validacion INTO arr_tipo_validacion.id_lav_tipo_validacion,
                                    arr_tipo_validacion.des_validacion
                           
      CALL v_cbx_folios.addItem(arr_tipo_validacion.id_lav_tipo_validacion,
                                arr_tipo_validacion.des_validacion)

      -- Contador de archivos eoncontrados
      LET v_indice = v_indice + 1
   END FOREACH
--
--Combo estado
   LET v_cbx_folios = ui.ComboBox.forName("cb_estado") --Asignación del combo a la forma
 
   -- Validación si el combo es nulo 
   IF v_cbx_folios IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1
   
   DECLARE cur_tipo_edo CURSOR FOR SELECT id_lav_estado,
                                          descripcion
                                    FROM  lav_tipo_estado
                                    GROUP BY 1,2
                                    ORDER BY 1,2
   LET v_indice = 0

   FOREACH cur_tipo_edo INTO arr_tipo_estado.id_lav_estado,
                             arr_tipo_estado.descripcion

      CALL v_cbx_folios.addItem(arr_tipo_estado.id_lav_estado,
                                arr_tipo_estado.descripcion)
      -- Contador de archivos eoncontrados
      LET v_indice = v_indice + 1
   END FOREACH
--
END FUNCTION

#OBJETIVO: Consultar los detalles de los registros detectados
FUNCTION fn_consulta_detalles(p_folio,p_nss,p_tipo_reporte,p_tipo_validacion,p_estado,p_fecha_ini,p_fecha_fin)

DEFINE p_folio           DECIMAL(9,0),
       p_tipo_reporte    SMALLINT, 
       p_tipo_validacion SMALLINT, 
       p_estado          SMALLINT,
       p_nss             CHAR(11),
       p_fecha_ini       DATE,
       p_fecha_fin       DATE,
       v_qry_txt  STRING,
       v_tot_regs INTEGER

DEFINE rec_detalles RECORD 
          v_folio          DECIMAL (9,0),
          v_reporte        SMALLINT,
          v_desc_reporte   CHAR(30),
          v_nss            CHAR (11),
          v_nombre         CHAR(50),
          v_ap_paterno_af  CHAR(40),
          v_ap_materno_af  CHAR(40),
          v_nombre_compl   CHAR (130),
          v_operacion      SMALLINT,
          v_fecha_mvto     DATE,
          v_no_depositos   INTEGER,
          v_subcuenta      SMALLINT,
          v_pesos          DECIMAL(16,4),
          v_dolares        DECIMAL(16,4),
          v_id_tipo_cambio SMALLINT,
          v_precio_dolar   DECIMAL(16,4),
          v_validacion     SMALLINT,
          v_desc_validacion CHAR(30),
          v_estado         SMALLINT,
          v_desc_estado    CHAR(30)
END RECORD

DEFINE v_tot_validacion CHAR(30),
       v_tot_pesos      DECIMAL(16,4),
       v_tot_dolar      DECIMAL(16,4) 

   LET v_qry_txt = " \n SELECT a.folio,             ",
                   " \n        a.id_tipo_reporte,   ",
                   " \n        a.nss,               ",
                   " \n        b.nombre_af,         ",
                   " \n        b.ap_paterno_af,     ",
                   " \n        b.ap_materno_af,     ",
                   " \n        a.tipo_operacion,    ",
                   " \n        a.f_deteccion,       ",
                   " \n        a.pesos,             ",
                   " \n        a.id_tipo_cambio,    ",
                   " \n        a.id_tipo_validacion,",
                   " \n        a.estado             ",
                   " \n FROM   lav_det_lavado a, ",
                   " \n        afi_derechohabiente b ",
                   " \n WHERE  a.id_derechohabiente = b.id_derechohabiente "

   IF p_folio IS NOT NULL THEN 
      LET v_qry_txt = v_qry_txt,
                      " \n AND   a.folio = ", p_folio
   END IF

   IF p_nss IS NOT NULL THEN 
      LET v_qry_txt = v_qry_txt,
                      " \n AND   a.nss = '", p_nss, "'"
   END IF 

   IF p_tipo_reporte IS NOT NULL THEN 
      LET v_qry_txt = v_qry_txt,
                      " \n AND   a.id_tipo_reporte = ", p_tipo_reporte
   END IF 

   IF p_tipo_validacion IS NOT NULL THEN 
      LET v_qry_txt = v_qry_txt,
                      " \n AND   a.id_tipo_validacion = ", p_tipo_validacion
   END IF

   IF p_estado IS NOT NULL THEN 
   END IF

   IF p_fecha_ini IS NOT NULL AND p_fecha_fin IS NOT NULL THEN    
      LET v_qry_txt = v_qry_txt,
                      " \n AND   a.f_deteccion BETWEEN '", p_fecha_ini, "'",
                      " \n AND   '", p_fecha_fin, "'"
   END IF

   LET v_qry_txt = v_qry_txt," \n GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12 ",
                             " \n ORDER BY 1"

   LET v_tot_regs = 1
   LET v_tot_pesos = 0
   LET v_tot_dolar = 0

   PREPARE prp_detalles FROM v_qry_txt
   DECLARE cur_detalles CURSOR FOR prp_detalles

   FOREACH cur_detalles INTO rec_detalles.v_folio
                             ,rec_detalles.v_reporte
                             ,rec_detalles.v_nss
                             ,rec_detalles.v_nombre
                             ,rec_detalles.v_ap_paterno_af
                             ,rec_detalles.v_ap_materno_af 
                             ,rec_detalles.v_operacion
                             ,rec_detalles.v_fecha_mvto
                             ,rec_detalles.v_pesos
                             ,rec_detalles.v_id_tipo_cambio
                             ,rec_detalles.v_validacion
                             ,rec_detalles.v_estado

      SELECT valor
      INTO   rec_detalles.v_precio_dolar
      FROM   lav_tipo_cambio
      WHERE  id_tipo_cambio = rec_detalles.v_id_tipo_cambio

      SELECT des_tipo_reporte
      INTO   rec_detalles.v_desc_reporte
      FROM   lav_tipo_reporte
      WHERE  id_tipo_reporte = rec_detalles.v_reporte

      SELECT des_validacion
      INTO   rec_detalles.v_desc_validacion
      FROM   lav_tipo_validaciones
      WHERE  id_lav_tipo_validacion = rec_detalles.v_validacion

      SELECT descripcion
      INTO   rec_detalles.v_desc_estado
      FROM   lav_tipo_estado 
      WHERE  id_lav_estado = rec_detalles.v_estado  

      LET rec_detalles.v_dolares = (rec_detalles.v_pesos / rec_detalles.v_precio_dolar)  

      LET arr_detalles[v_tot_regs].v_folio        = rec_detalles.v_folio      
      LET arr_detalles[v_tot_regs].v_reporte      = rec_detalles.v_reporte || " - " || rec_detalles.v_desc_reporte
      LET arr_detalles[v_tot_regs].v_nss          = rec_detalles.v_nss           
      LET arr_detalles[v_tot_regs].v_nombre_compl = rec_detalles.v_nombre CLIPPED ||" "||rec_detalles.v_ap_paterno_af CLIPPED ||" "||rec_detalles.v_ap_materno_af CLIPPED
      LET arr_detalles[v_tot_regs].v_operacion    = rec_detalles.v_operacion || " - " || "ABONO"
      LET arr_detalles[v_tot_regs].v_fecha_mvto   = rec_detalles.v_fecha_mvto     
      LET arr_detalles[v_tot_regs].v_no_depositos = 1
      LET arr_detalles[v_tot_regs].v_subcuenta    = 45
      LET arr_detalles[v_tot_regs].v_pesos        = rec_detalles.v_pesos
      LET arr_detalles[v_tot_regs].v_dolares      = rec_detalles.v_dolares 
      LET arr_detalles[v_tot_regs].v_precio_dolar = rec_detalles.v_precio_dolar
      LET arr_detalles[v_tot_regs].v_validacion   = rec_detalles.v_validacion|| " - " || rec_detalles.v_desc_validacion
      LET arr_detalles[v_tot_regs].v_estado       = rec_detalles.v_estado|| " - " ||rec_detalles.v_desc_estado

      LET v_tot_regs = v_tot_regs + 1      
      LET v_tot_pesos = v_tot_pesos + rec_detalles.v_pesos
      LET v_tot_dolar = v_tot_dolar + rec_detalles.v_dolares

   END FOREACH

   SELECT des_validacion
   INTO   v_tot_validacion
   FROM   lav_tipo_validaciones
   WHERE  id_lav_tipo_validacion = rec_detalles.v_validacion
   GROUP BY 1

   CALL arr_detalles.deleteElement(v_tot_regs)
   LET v_tot_regs = v_tot_regs - 1 

   LET arr_totales[1].v_tot_validacion = v_tot_validacion
   LET arr_totales[1].v_tot_registros  = v_tot_regs
   LET arr_totales[1].v_tot_pesos      = v_tot_pesos
   LET arr_totales[1].v_tot_dolares    = v_tot_dolar

   RETURN v_tot_regs, v_tot_validacion,v_tot_pesos,v_tot_dolar
END FUNCTION 

#OBJETIVO: Crear el nombre del archivo que se va a generar
FUNCTION fn_crea_nombre_archivo()
DEFINE v_nombre_archivo CHAR(40),
       r_ruta_nombre    VARCHAR(100),
       v_ruta_envio     LIKE seg_modulo.ruta_envio,
       v_year           CHAR(4),
       v_year_c         CHAR(2),
       v_mes            CHAR(2)

   SELECT ruta_envio 
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = 'lav'

   LET v_year = YEAR (TODAY)
   LET v_mes  = MONTH(TODAY)

   LET v_year_c = v_year[3,4]
   LET v_mes = v_mes USING "&&"
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_nombre_archivo = "/OP_REL_" || v_year_c ||v_mes ||".lav"

   LET r_ruta_nombre = v_ruta_envio CLIPPED || v_nombre_archivo

   RETURN r_ruta_nombre

END FUNCTION

#OBJETIVO: Generar archivo de salida para exportar datos de la consulta
FUNCTION fn_genera_salida(p_folio,p_nss,p_tipo_reporte,p_tipo_validacion,p_estado,p_fecha_ini,p_fecha_fin)
DEFINE p_folio             DECIMAL(9,0),
       p_tipo_reporte      SMALLINT, 
       p_tipo_validacion   SMALLINT, 
       p_estado            SMALLINT,
       p_nss               CHAR(11),
       p_fecha_ini         DATE,
       p_fecha_fin         DATE,
       p_usuario_cod       LIKE seg_usuario.usuario_cod,
       r_ruta_nombre       VARCHAR(100),
       v_ch_arch_solTransf BASE.CHANNEL,
       v_tot_registros     INTEGER,
       v_registro          STRING,
       v_tot_regs          INTEGER,
       v_ini               INTEGER,
       v_mensaje           STRING
DEFINE r_tot_validacion    CHAR(30),
       r_tot_pesos         DECIMAL(16,4),
       r_tot_dolar         DECIMAL(16,4) 

   CALL fn_crea_nombre_archivo()
        RETURNING r_ruta_nombre

   CALL fn_consulta_detalles(p_folio,p_nss,p_tipo_reporte,p_tipo_validacion,p_estado,p_fecha_ini,p_fecha_fin)
        RETURNING v_tot_regs, r_tot_validacion,r_tot_pesos,r_tot_dolar

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(r_ruta_nombre, "w" )

   FOR v_ini = 1 TO v_tot_regs
      LET v_registro =  arr_detalles[v_ini].v_folio CLIPPED,
                        "|",arr_detalles[v_ini].v_reporte CLIPPED,
                        "|",arr_detalles[v_ini].v_nss CLIPPED,
                        "|",arr_detalles[v_ini].v_nombre_compl CLIPPED,
                        "|",arr_detalles[v_ini].v_operacion CLIPPED,
                        "|",arr_detalles[v_ini].v_fecha_mvto CLIPPED,
                        "|",arr_detalles[v_ini].v_no_depositos CLIPPED,
                        "|",arr_detalles[v_ini].v_subcuenta CLIPPED,
                        "|",arr_detalles[v_ini].v_pesos CLIPPED,
                        "|",arr_detalles[v_ini].v_dolares CLIPPED,
                        "|",arr_detalles[v_ini].v_precio_dolar CLIPPED,
                        "|",arr_detalles[v_ini].v_validacion CLIPPED,
                        "|",arr_detalles[v_ini].v_estado CLIPPED
      CALL v_ch_arch_solTransf.writeline(v_registro)
   END FOR   

   CALL v_ch_arch_solTransf.close()

   LET v_mensaje =  "La información se ha exportado en el siguiente archivo : \n " || r_ruta_nombre 
   CALL fn_mensaje ("Atención", 
                    v_mensaje CLIPPED ,
                    "stop")

END FUNCTION

#OBJETIVO: Generar el reporte con los datos de la consulta
REPORT rpt_detalle(p_usuario, p_rec_detalles, p_tot_validacion,p_tot_registros,p_tot_pesos,p_tot_dolar)
DEFINE v_fecha_reporte        DATE,
       p_usuario              LIKE seg_modulo.usuario

DEFINE p_rec_detalles RECORD 
          v_folio          DECIMAL (9,0),
          v_reporte        CHAR (30),
          v_nss            CHAR (11),
          v_nombre_compl   CHAR (130),
          v_operacion      CHAR (30),
          v_fecha_mvto     DATE,
          v_no_depositos   INTEGER,
          v_subcuenta      SMALLINT,
          v_pesos          DECIMAL(16,4),
          v_dolares        DECIMAL(16,4),
          v_precio_dolar   DECIMAL(16,4),
          v_validacion     CHAR(30),
          v_estado         CHAR (30)
END RECORD 

DEFINE p_tot_validacion CHAR(30),
       p_tot_registros  INTEGER,
       p_tot_pesos      DECIMAL(16,4),
       p_tot_dolar      DECIMAL(16,4)

FORMAT

   FIRST PAGE HEADER                            
      LET v_fecha_reporte    = TODAY CLIPPED
      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario

   ON EVERY ROW
      PRINTX p_rec_detalles.v_folio
      PRINTX p_rec_detalles.v_reporte
      PRINTX p_rec_detalles.v_nss
      PRINTX p_rec_detalles.v_nombre_compl
      PRINTX p_rec_detalles.v_operacion
      PRINTX p_rec_detalles.v_fecha_mvto USING "dd-mm-yyyy" 
      PRINTX p_rec_detalles.v_no_depositos
      PRINTX p_rec_detalles.v_subcuenta
      PRINTX p_rec_detalles.v_pesos
      PRINTX p_rec_detalles.v_dolares
      PRINTX p_rec_detalles.v_precio_dolar
      PRINTX p_rec_detalles.v_validacion
      PRINTX p_rec_detalles.v_estado

   ON LAST ROW 
      PRINTX p_tot_validacion
      PRINTX p_tot_registros
      PRINTX p_tot_pesos
      PRINTX p_tot_dolar

END REPORT 