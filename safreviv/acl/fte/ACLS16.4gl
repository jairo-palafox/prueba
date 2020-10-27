-----------------------------------------------------------------------------------------
-- Modulo        => ACL
-- Programa      => ACLS16
-- Objetivo      => Reporte de causales sin liquidar SOLO cauales en blanco
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 04 de Septiembre de 2018.
-- Requerimiento => saci2018-67-02
-----------------------------------------------------------------------------------------
-- Modificación =>
-- Fehca        =>
-- Autor        =>
-- Clave cambio =>
-- Descripción  =>
-----------------------------------------------------------------------------------------
-- Modificación =>
-- Fehca        =>
-- Autor        =>
-- Clave cambio =>
-- Descripción  =>
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS

   DEFINE
      p_fecha_corte DATE,
      p_usuario     CHAR(20),
      p_pid         SMALLINT,
      p_proceso_cod SMALLINT
      
   DEFINE  
      v_ventana ui.Window,
      v_forma   ui.form,
      p_ventana STRING, 
      g_usuario_cod LIKE seg_usuario.usuario_cod,
      v_i_cont_registros INTEGER
        
   DEFINE v_arr_causales DYNAMIC ARRAY OF RECORD
--   DEFINE v_arr_causales RECORD
      v_causal           LIKE cta_his_pagos.tpo_aclaracion,
      v_causal_des       LIKE pag_tpo_aclaracion.aclaracion_descripcion,
      v_tot_causal       INTEGER,
      v_imp_ap           DECIMAL (22,2), 
      v_aiv_ap_pat       DECIMAL (18,6),
      v_amp_am           DECIMAL (22,2),
      v_int_gen_pgo_ext  DECIMAL (22,2),
      v_aiv_gen_pgo_ext  DECIMAL (18,6),
      v_imp_ap_am        DECIMAL (22,2),
      v_porcentaje       DECIMAL (16,4)
   END RECORD
   
   DEFINE v_arr_totales RECORD
      v_tot_causales_gral   DECIMAL(9,0),
      v_tot_imp_ap          DECIMAL(22,2),
      v_tot_aiv_ap_pat      DECIMAL(18,6),
      v_tot_imp_am          DECIMAL(22,2),
      v_tot_int_gen_pgo_ext DECIMAL(22,2),
      v_tot_aiv_gen_pgo_ext DECIMAL(18,6),
      v_tot_imp_apam        DECIMAL(22,2),
      v_tot_porcentajes     DECIMAL(16,4),
      v_aux                 DECIMAL
   END RECORD

END GLOBALS

MAIN
   DEFINE v_radio       CHAR(15)
   DEFINE v_folio       DECIMAL(9,0)
   DEFINE  
      v_ventana ui.Window,
      v_forma   ui.form,
      p_ventana STRING, 
      g_usuario_cod LIKE seg_usuario.usuario_cod,
      g_proceso_cod SMALLINT,
      v_i_cont_registros INTEGER   
   

--   LET p_fecha_corte = ARG_VAL(1)
--   LET p_usuario     = ARG_VAL(2)

   LET p_fecha_corte = TODAY

   SELECT USER              
   INTO   p_usuario        
   FROM   seg_modulo           
   WHERE  modulo_cod = "acl"

   LET p_pid = 111
   LET p_proceso_cod = 100
   LET g_usuario_cod = p_usuario

   DISPLAY "Usuario ",g_usuario_cod

   CLOSE WINDOW SCREEN

   OPEN WINDOW ventana01 WITH FORM "ACLS161"

   -- Se asigna el titulo de la ventana
   LET v_ventana = ui.Window.getCurrent()
   LET v_forma = v_ventana.getForm() 
   
   IF p_ventana IS NOT NULL THEN
      CALL ui.Interface.setText(p_ventana)         
      CALL v_ventana.setText(p_ventana)
   END IF

   INPUT v_radio WITHOUT DEFAULTS   
      FROM radiogroup1 ATTRIBUTES (UNBUFFERED ,ACCEPT=FALSE)
      
      -- Se evita seleccion en nulo de los criterio con la forma
      BEFORE INPUT
        LET v_radio = NULL
        LET v_folio = -1

      ON CHANGE radiogroup1
         IF v_radio = 1 THEN
         	LET g_proceso_cod = 102
         END IF

      ON ACTION ACCEPT
         IF v_radio IS NULL THEN
            CALL fn_mensaje("Consulta","Debes elegir causales en blanco","about")
            LET v_radio = NULL
            NEXT FIELD radiogroup1
        END IF

        LET v_folio = 0

        CALL genera_reporte()
        
        CALL fn_mensaje("Consulta","Reporte generado en ruta /safreviv_lst/acl ","about")
    
      ON ACTION CANCEL
        EXIT INPUT

   END INPUT

   CLOSE WINDOW ventana01

END MAIN

FUNCTION genera_reporte()

   DEFINE
      aux  INTEGER, -- Auxiliar para los ciclos 
      mes  CHAR(2), -- Mes de busqueda
      anio CHAR(4)  -- Año de busqueda

   DEFINE 
      v_consulta_folio STRING,   -- Consulta de folios
      v_consulta_causa STRING    -- Consulta de causales
      
   DEFINE v_arr_folios DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para los folios encontrados
      v_folio LIKE glo_folio.folio
   END RECORD
   
--============================      
   LET v_i_cont_registros  = 1  --SE INICIALIZA EL CONMTADOR DE REGISTROS PARA CTA_HIS_PAGOS

   -- Busqueda de existencia de registros con los criterios recibidos
   -- Se utilizan los codigos de proceso definidos en el requerimiento   
   LET v_consulta_folio = "\n SELECT folio ",
                          "\n FROM glo_folio",
                          "\n WHERE f_actualiza < ? " ,
                          "\n AND proceso_cod IN (1401, 101,102,103,105,107,110)"

    PREPARE prp_busca_folios FROM v_consulta_folio
    DECLARE cur_busca_folios CURSOR FOR prp_busca_folios
    DISPLAY v_consulta_folio

    -- Inicializa en 0 el valor de de la bandera   
    LET aux = 1
    FOREACH cur_busca_folios USING p_fecha_corte  INTO v_arr_folios[aux].*
      LET aux = aux + 1
    END FOREACH
    FREE cur_busca_folios
    -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
    CALL v_arr_folios.deleteElement(aux)
    DISPLAY v_arr_folios.getLength()

    -- Se evalua el resultado de la validacion
   IF v_arr_folios.getLength() > 0  THEN

      CALL genera_datos()

      PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
      EXECUTE eje_prio

      LET v_consulta_causa = " SELECT tpo_aclaracion,                  ",
                             "        '',                              ", 
                             "        count(tpo_aclaracion) as no,     ",
                             "        SUM(imp_ap_pat) as pat,          ",
                             "        SUM(aiv_ap_pat) as aiv,          ",
                             "        SUM(imp_am_cre) as cre ,         ",
                             "        SUM(int_gen_pgo_ext) as ext,     ",
                             "        SUM(aiv_gen_pgo_ext) as aiv_ext, ",
                             "        SUM(aiv_ap_pat + aiv_gen_pgo_ext ) as suma, ", 
                             "        0 ",
                             " FROM   tmp_blancos_uni ",
                             " GROUP  by tpo_aclaracion ",
                             " HAVING count(*)>0 "

{
      LET v_consulta_causa = " SELECT cta.tpo_aclaracion,               ",
                             "        '',                               ", 
                             "        count(cta.tpo_aclaracion) as no,  ",
                             "        SUM(cta.imp_ap_pat) as pat,       ",
                             "        SUM(cta.aiv_ap_pat) as aiv,       ",
                             "        SUM(cta.imp_am_cre) as cre ,      ",
                             "        SUM(cta.int_gen_pgo_ext) as ext,  ",
                             "        SUM(cta.aiv_gen_pgo_ext) as aiv_ext, ",
                             "        SUM(cta.aiv_ap_pat + cta.aiv_gen_pgo_ext ) as suma, ", 
                             "        0 ",
                             " FROM   cta_rechazos_acl cta ",
                             "        LEFT OUTER JOIN acl_pag_rechazo acl ",
                             "             ON (cta.folio = acl.folio AND cta.id_referencia = acl.id_referencia) ",
                             "        LEFT OUTER JOIN acl_cat_rechazo cat ",
                             "             ON (acl.codigo_rechazo = cat.codigo_rechazo), ",
                             "        cta_pag_complemento b, ",
                             "        afi_derechohabiente c  ",
                             " WHERE  cta.folio = b.folio    ",
                             " AND    cta.id_derechohabiente = c.id_derechohabiente ",
                             " AND    cta.id_referencia = b.id_referencia ",
                             " AND    cta.result_operacion in (2,3) ",
                             " AND   (cta.tpo_aclaracion = '' OR cta.tpo_aclaracion IS NULL) ",
                             " GROUP  by cta.tpo_aclaracion ",
                             " HAVING count(*)>0 "
}

      DISPLAY v_consulta_causa   
      PREPARE prp_busca_causales FROM v_consulta_causa
      
      DECLARE cur_busca_causales CURSOR FOR prp_busca_causales

      -- Inicializa en 1 el valor de de la bandera   
      LET v_i_cont_registros = 1
      LET v_arr_totales.v_tot_imp_apam = 0
      LET v_arr_totales.v_tot_causales_gral = 0
      LET v_arr_totales.v_tot_imp_ap = 0
      LET v_arr_totales.v_tot_imp_am = 0
      LET v_arr_totales.v_tot_porcentajes = 45.6245
      LET v_arr_totales.v_tot_aiv_ap_pat = 0
      LET v_arr_totales.v_tot_int_gen_pgo_ext = 0
      LET v_arr_totales.v_tot_aiv_gen_pgo_ext = 0
      
      --FOREACH cur_busca_causales USING p_fecha_corte INTO v_arr_causales[v_i_cont_registros].*
      
      FOREACH cur_busca_causales INTO v_arr_causales[v_i_cont_registros].*
      
         LET v_arr_totales.v_tot_causales_gral   = v_arr_totales.v_tot_causales_gral   + v_arr_causales[v_i_cont_registros].v_tot_causal
         LET v_arr_totales.v_tot_imp_ap          = v_arr_totales.v_tot_imp_ap          + v_arr_causales[v_i_cont_registros].v_imp_ap
         LET v_arr_totales.v_tot_aiv_ap_pat      = v_arr_totales.v_tot_aiv_ap_pat      + v_arr_causales[v_i_cont_registros].v_aiv_ap_pat
         LET v_arr_totales.v_tot_imp_am          = v_arr_totales.v_tot_imp_am          + v_arr_causales[v_i_cont_registros].v_amp_am
         LET v_arr_totales.v_tot_int_gen_pgo_ext = v_arr_totales.v_tot_int_gen_pgo_ext + v_arr_causales[v_i_cont_registros].v_int_gen_pgo_ext
         LET v_arr_totales.v_tot_aiv_gen_pgo_ext = v_arr_totales.v_tot_aiv_gen_pgo_ext + v_arr_causales[v_i_cont_registros].v_aiv_gen_pgo_ext
         LET v_arr_totales.v_tot_imp_apam        = v_arr_totales.v_tot_imp_apam        + v_arr_causales[v_i_cont_registros].v_imp_ap_am
         
         LET v_i_cont_registros = v_i_cont_registros + 1 

      END FOREACH
      FREE cur_busca_causales
      
      -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
      LET v_i_cont_registros = v_i_cont_registros  - 1
      
      CALL v_arr_causales.deleteElement(v_arr_causales.getLength())
      
      IF v_arr_causales.getLength()>0 THEN  
         -- Calculo de porcentajes
         LET v_arr_totales.v_tot_porcentajes = 0
         
         FOR aux = 1 TO v_arr_causales.getLength() STEP +1
            --DISPLAY v_arr_causales[aux].v_porcentaje," ", v_arr_causales[aux].v_imp_ap_am ," ",  v_arr_totales.v_tot_imp_apam     
            
            LET v_arr_causales[aux].v_porcentaje = (v_arr_causales[aux].v_imp_ap_am / v_arr_totales.v_tot_imp_apam) * 100    
            LET v_arr_totales.v_tot_porcentajes  = v_arr_totales.v_tot_porcentajes + v_arr_causales[aux].v_porcentaje
            
            --DISPLAY v_arr_causales[aux].v_porcentaje ," GRAL ",v_arr_totales.v_tot_porcentajes
         END FOR  
         
         CALL fn_reporte_carga_archivo( TRUE, mes, anio)

      ELSE
          -- Se notifica al usuario que no se encontro informacion con los criterios
          CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
      END IF 
   ELSE
      -- Se notifica al usuario que no se encontro informacion con los criterios
      CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
   END IF
       
END FUNCTION

FUNCTION genera_datos()

   DEFINE reg_blancos RECORD
      id_derechohabiente decimal(9,0), 
      folio_sua          decimal(6,0),
      periodo_pago       char(6),
      f_pago             date,
      nrp                char(11),
      ind_liquidacion    smallint,
      result_operacion   smallint,
      tpo_aclaracion     char(2),
      imp_ap_pat         decimal(12,2),
      aiv_ap_pat         decimal(18,6),
      imp_am_cre         decimal(12,2),
      int_gen_pgo_ext    decimal(18,6),
      aiv_gen_pgo_ext    decimal(18,6)   	
   END RECORD

   DEFINE v_existe_liq SMALLINT

   WHENEVER ERROR CONTINUE
   DROP TABLE tmp_blancos_acl
   DROP TABLE tmp_blancos_uni
   WHENEVER ERROR STOP
   
   CREATE TEMP table tmp_blancos_acl
      (
      id_derechohabiente decimal(9,0), 
      folio_sua          decimal(6,0),
      periodo_pago       char(6),
      f_pago             date,
      nrp                char(11),
      ind_liquidacion    smallint,
      result_operacion   smallint,
      tpo_aclaracion     char(2),
      imp_ap_pat         decimal(12,2),
      aiv_ap_pat         decimal(18,6),
      imp_am_cre         decimal(12,2),
      int_gen_pgo_ext    decimal(18,6),
      aiv_gen_pgo_ext    decimal(18,6)
      );

   CREATE TEMP table tmp_blancos_uni
      (
      id_derechohabiente decimal(9,0), 
      folio_sua          decimal(6,0),
      periodo_pago       char(6),
      f_pago             date,
      nrp                char(11),
      ind_liquidacion    smallint,
      result_operacion   smallint,
      tpo_aclaracion     char(2),
      imp_ap_pat         decimal(12,2),
      aiv_ap_pat         decimal(18,6),
      imp_am_cre         decimal(12,2),
      int_gen_pgo_ext    decimal(18,6),
      aiv_gen_pgo_ext    decimal(18,6)
      );

   INSERT INTO tmp_blancos_acl
   SELECT a.id_derechohabiente,
          a.folio_sua,
          a.periodo_pago,
          a.f_pago,
          a.nrp,
          a.ind_liquidacion,
          a.result_operacion,
          a.tpo_aclaracion,
          a.imp_ap_pat,
          a.aiv_ap_pat,
          a.imp_am_cre,
          a.int_gen_pgo_ext,
          a.aiv_gen_pgo_ext
   FROM   cta_rechazos_acl a
          LEFT OUTER JOIN acl_pag_rechazo acl
               ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia)
          LEFT OUTER JOIN acl_cat_rechazo cat
               ON (acl.codigo_rechazo = cat.codigo_rechazo),
          cta_pag_complemento b,
          afi_derechohabiente c
   WHERE  a.folio              = b.folio
   AND    a.id_derechohabiente = c.id_derechohabiente
   AND    a.id_referencia      = b.id_referencia
   AND    a.result_operacion in (2,3)
   AND   (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL)

   DECLARE cur_blancos CURSOR FOR
   SELECT *
   FROM   tmp_blancos_acl

   --CREATE INDEX idx_tmp_blancos_acl ON tmp_blancos_acl (id_derechohabiente,folio_sua,periodo_pago,f_pago,nrp)

   FOREACH cur_blancos INTO reg_blancos.*

      SELECT COUNT(*)
      INTO   v_existe_liq
      FROM   cta_his_pagos
      WHERE  id_derechohabiente = reg_blancos.id_derechohabiente
      AND    folio_sua          = reg_blancos.folio_sua
      AND    periodo_pago       = reg_blancos.periodo_pago
      AND    f_pago             = reg_blancos.f_pago
      AND    nrp                = reg_blancos.nrp
      AND    ind_liquidacion not in (1,-1)
      AND    result_operacion <> 2

      IF v_existe_liq = 0 THEN
         INSERT INTO tmp_blancos_uni VALUES (reg_blancos.*)
      END IF
   	
   END FOREACH
   
END FUNCTION

FUNCTION fn_reporte_carga_archivo(p_b_despliegue_pantalla, v_mes, v_anio)
   
   DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
   DEFINE manejador_rpt om.SaxDocumentHandler,
          v_i_inicio_for INTEGER
   
   DEFINE v_mes      CHAR(2)-- Mes de busqueda
   DEFINE v_anio     CHAR(4)-- Año de busqueda
   DEFINE v_mes_desc CHAR(12)
   DEFINE v_fecha    DATETIME YEAR TO SECOND
   DEFINE v_fecha_1  CHAR(19)
   DEFINE v_fecha_2  CHAR(19)
   DEFINE v_fecha_c  CHAR(10)
   DEFINE v_hora_c   CHAR(08)
   
   --DEFINE manejador_rpt      om.SaxDocumentHandler
   DEFINE v_genero_reporte_pdf BOOLEAN
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte   VARCHAR(80)
   DEFINE v_existe        SMALLINT

   LET v_mes  = MONTH(p_fecha_corte)
   LET v_anio = YEAR(p_fecha_corte)

   LET v_fecha   = CURRENT
   LET v_fecha_1 = v_fecha
   LET v_fecha_c = v_fecha_1[1,10]
   LET v_hora_c  = v_fecha_1[12,19]
   LET v_fecha_2 = v_fecha_c,"-",v_hora_c   

   LET v_i_inicio_for = 1

   LET v_nom_reporte = p_usuario     CLIPPED,"-ACLS16-",
                       p_pid         USING "&&&&&", "-",
                       p_proceso_cod USING "&&&&&","-",
                       v_fecha_2

   CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados
--   LET r_ruta_listados = "/safreviv/acl/envio/"

   LET v_genero_reporte_pdf= FALSE

   CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/ACLS16.4rp") RETURNING v_existe

   IF v_existe THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte)
      CALL fgl_report_selectPreview(0)       -- 1=Genera reporte de inmediato 0=Genera reporte en ruta
      LET manejador_rpt = fgl_report_commitCurrentSettings()
      LET v_genero_reporte_pdf = TRUE
   ELSE
      RETURN v_genero_reporte_pdf
   END IF

   -- se asigna descripción al mes
   CASE v_mes <> 0
       WHEN v_mes = 1
           LET v_mes_desc = 'Enero'      EXIT CASE 
       WHEN v_mes = 2  
           LET v_mes_desc = 'Febrero'    EXIT CASE  
       WHEN v_mes = 3  
           LET v_mes_desc = 'Marzo'      EXIT CASE 
       WHEN v_mes = 4  
           LET v_mes_desc = 'Abril'      EXIT CASE  
       WHEN v_mes = 5  
           LET v_mes_desc = 'Mayo'       EXIT CASE  
       WHEN v_mes = 6  
           LET v_mes_desc = 'Junio'      EXIT CASE 
       WHEN v_mes = 7 
           LET v_mes_desc = 'Julio'      EXIT CASE  
       WHEN v_mes = 8  
           LET v_mes_desc = 'Agosto'     EXIT CASE 
       WHEN v_mes = 9  
           LET v_mes_desc = 'Septiembre' EXIT CASE  
       WHEN v_mes = 10 
           LET v_mes_desc = 'Octubre'    EXIT CASE  
       WHEN v_mes = 11 
           LET v_mes_desc = 'Noviembre'  EXIT CASE  
       WHEN v_mes = 12 
           LET v_mes_desc = 'Diciembre'  EXIT CASE  
   END CASE  
    
   --Inicia el reporte de registros con rechazo
   START REPORT rpt_causales_aclaracion_lqinfo TO XML HANDLER manejador_rpt
   FOR v_i_inicio_for = 1 TO v_i_cont_registros
      OUTPUT TO REPORT rpt_causales_aclaracion_lqinfo(v_arr_causales[v_i_inicio_for].*, v_mes_desc, v_anio)
   END FOR  
   FINISH REPORT rpt_causales_aclaracion_lqinfo

END FUNCTION

REPORT rpt_causales_aclaracion_lqinfo(v_arr_causales, p_mes_desc, p_anio)
   
   DEFINE v_arr_causales    RECORD -- Arreglo dinamico para las cuenstas causales encontradas
      v_causal          LIKE cta_his_pagos.tpo_aclaracion,
      v_causal_des      LIKE pag_tpo_aclaracion.aclaracion_descripcion,
      v_tot_causal      INTEGER,
      v_imp_ap          DECIMAL (22,2),
      v_aiv_ap_pat      DECIMAL (18,6),
      v_amp_am          DECIMAL (22,2),
      v_int_gen_pgo_ext DECIMAL (22,2),
      v_aiv_gen_pgo_ext DECIMAL (18,6),
      v_imp_ap_am       DECIMAL (22,2),
      v_porcentaje      DECIMAL (16,4)
   END RECORD
   
   DEFINE
      v_fecha_reporte    DATE,
      v_nombre_usuario   VARCHAR(100),
      p_mes_desc         CHAR(12),
      p_anio             CHAR(4)
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER    
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED
                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_arr_totales.*,
             v_nombre_usuario,
             g_usuario_cod ,
             p_mes_desc ,
             p_anio
                                                                                              
   ON EVERY ROW
      PRINTX v_arr_causales.*
                                                                                           
END REPORT  

                  