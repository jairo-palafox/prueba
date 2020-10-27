################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 03/07/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTC08                                                   #
#Objetivo          => Realizar la consulta y generación del reporte de los     #
#                     montos por Tipo de Crédito.                              #
#Fecha inicio      => 02/07/2012                                               #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
DATABASE "safre_viv"

GLOBALS "CNTG01.4gl"
GLOBALS 
DEFINE g_arr_tipo_credito DYNAMIC ARRAY OF RECORD
v_tipo_cred CHAR(30),
v_subcta CHAR(40),
v_monto DECIMAL(22,2)
END RECORD 
DEFINE v_indice INTEGER,
       v_QryTxt STRING,
       v_tipo_credito SMALLINT,
       v_fecha_ini DATE,
       V_fecha_fin DATE 
DEFINE i int
DEFINE arr_tbl_mov DYNAMIC ARRAY OF VARCHAR(50)
END GLOBALS

MAIN 
DEFINE f_ventana        ui.Window,   -- Define las propìedades de la Ventana
       f_forma          ui.Form     -- Define las propiedades de la forma

   -- se asignan los parametros que vienen del fglrun
   LET g_usuario = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog = ARG_VAL(3)
   
   --Se invoca la función que asigna el titulo a la ventana
   CALL ui.Interface.setText(g_nom_prog)

   -- se genera el listado de tablas cta_movimiento
   LET i=1
   DECLARE cur_tbl_mov CURSOR FOR SELECT tabla from cat_tab_movimiento
   FOREACH cur_tbl_mov INTO arr_tbl_mov[i]
      LET i=i+1
   END FOREACH
   LET arr_tbl_mov[i] = "cta_movimiento"

CLOSE WINDOW SCREEN   
OPEN WINDOW vtn_CNTC06 WITH FORM "CNTC081"
   DIALOG ATTRIBUTES(UNBUFFERED)

   INPUT v_cmb_tpo_crd,v_fecha_ini,v_fecha_fin 
    FROM v_cmb_tpo_crd,v_fecha_ini,v_fecha_fin 

      BEFORE INPUT
         --Llena el combo de tipo de crédito
         CALL fn_cmb_tpo_credito()
         CALL g_arr_tipo_credito.clear()
         LET f_ventana = ui.Window.getCurrent()
         LET f_forma = f_ventana.getForm()
         CALL f_forma.setElementHidden("gr_montos", 1)   
   END INPUT 

   --Acciones al presionar el botón CANCELAR
   ON ACTION cancelar
      EXIT DIALOG

   --Acciones al presionar el botón CONSULTAR
   ON ACTION consultar
      --Si el usuario no captura tipo de crédito ni periodo
      IF v_cmb_tpo_crd IS NULL AND v_fecha_ini IS NULL AND v_fecha_fin IS NULL THEN
         CALL fn_cons_sin_tipo_credito() 
         IF v_indice > 1 THEN  
            CALL f_forma.setElementHidden("gr_montos", 0)
         END IF
      END IF 

      --VALIDACIONES DE FECHAS
      --Si se tiene fecha inicial pero no se tiene fecha final 
      IF v_fecha_ini IS NOT NULL AND v_fecha_fin IS NULL THEN
         CALL fn_mensaje("ATENCION", "La fecha final es requerida", "stop")
         NEXT FIELD v_fecha_fin
      END IF 

      --Si se tiene fecha final pero no fecha inicial
      IF v_fecha_ini IS NULL AND v_fecha_fin IS NOT NULL THEN
         CALL fn_mensaje("ATENCION", "La fecha inicial es requerida", "stop")
         NEXT FIELD v_fecha_ini      
      END IF 

      IF  v_fecha_ini >  v_fecha_fin THEN 
         CALL fn_mensaje("ATENCION", "La fecha inicial es mayor que la fecha final", "stop")
         NEXT FIELD v_fecha_ini
      END IF

      IF v_fecha_fin > TODAY THEN 
         CALL fn_mensaje("ATENCION", "La fecha final es mayor que la fecha actual", "stop")
         NEXT FIELD v_fecha_fin
      END IF

      --Consulta por periodo de fechas
      IF  v_fecha_ini IS NOT NULL AND v_fecha_fin IS NOT NULL AND v_cmb_tpo_crd IS NULL THEN
         CALL fn_cons_sin_tipo_credito()     
         IF v_indice > 1 THEN  
            CALL f_forma.setElementHidden("gr_montos", 0)
         ELSE 
            CALL fn_mensaje("ATENCION","No existe información de montos","stop")              
         END IF
      END IF  
      
      --- Tipo de Originación de Créditos Tradicionales ---
      IF v_cmb_tpo_crd = 1 OR v_cmb_tpo_crd = 3 OR v_cmb_tpo_crd = 4 OR v_cmb_tpo_crd = 5 THEN 
         CALL fn_cons_cred_tradicionales() 
         IF v_indice > 1 THEN  
            CALL f_forma.setElementHidden("gr_montos", 0)
         ELSE 
            CALL fn_mensaje("ATENCION","No existe información de montos","stop")   
         END IF
      END IF 

      --- Tipo de Originación de Apoyo Infonavit ---
      IF v_cmb_tpo_crd = 2 THEN 
         CALL fn_cons_cred_apoyo_infonavit() 
         IF v_indice > 1 THEN  
            CALL f_forma.setElementHidden("gr_montos", 0)
         ELSE 
            CALL fn_mensaje("ATENCION","No existe información de montos","stop")   
         END IF
      END IF

      --- Tipo de Originación de Créditos Cofinanciados ---
      IF v_cmb_tpo_crd = 10 OR v_cmb_tpo_crd = 11 OR v_cmb_tpo_crd = 12 OR v_cmb_tpo_crd = 14 THEN 
         CALL fn_cons_cred_cofinanciados()
         IF v_indice > 1 THEN  
            CALL f_forma.setElementHidden("gr_montos", 0)
         ELSE
            CALL fn_mensaje("ATENCION","No existe información de montos","stop")               
         END IF
      END IF

      DISPLAY ARRAY  g_arr_tipo_credito TO scr_tipocred.*
      ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
         --Acciones al presionar el botón CANCELAR
         ON ACTION cancelar
            CALL f_forma.setElementHidden("gr_montos", 1) 
            EXIT DISPLAY
            EXIT DIALOG
         --Acciones al presionar el botón REPORTE
         ON ACTION reporte
            CALL fn_rpt_montos_tipo_cred()
         ON ACTION archivo
            CALL fn_genera_archivo_saldos_vivienda()
      END DISPLAY 
   

   END DIALOG 
CLOSE WINDOW vtn_CNTC06 
END MAIN

#OBJETIVO: Consultar todos los datos cuando no se ha capturado ningún parametro
FUNCTION fn_cons_sin_tipo_credito()
   LET v_indice = 1
   --20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
   FOR i = 1 TO arr_tbl_mov.getLength()

      LET v_QryTxt = "\n SELECT cre.tpo_credito||'-'|| tcr.desc_credito,",
                     "\n       mov.subcuenta||'-'|| sub.subcuenta_desc,",
                     "\n       SUM(mov.monto_pesos)",
                     "\n  FROM cre_acreditado cre,",
                     "\n       ",arr_tbl_mov[i]," mov,",
                     "\n       cat_tipo_credito tcr,",
                     "\n       cat_subcuenta sub",
                     "\n WHERE cre.folio_liquida > 0",
                     "\n   AND cre.folio_liquida = mov.folio_liquida",
                     "\n   AND cre.tpo_credito = tcr.tpo_credito",
                     "\n   AND mov.subcuenta = sub.subcuenta"

      IF v_fecha_ini IS NULL AND v_fecha_fin IS NULL THEN  
         LET v_QryTxt = v_QryTxt || "\n AND mov.f_liquida BETWEEN 01/01/2012 AND TODAY "
      ELSE 
         LET v_QryTxt =  v_QryTxt || "\n AND mov.f_liquida BETWEEN ", "'",v_fecha_ini,"'", 
                                     "\n AND ", "'",v_fecha_fin,"'" 
      END IF

      LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,2",
                                 "\n ORDER BY 1,2"   
      -- DISPLAY v_QryTxt

      PREPARE prp_cons_sin_param FROM v_QryTxt
      DECLARE cur_cons_sin_param CURSOR FOR prp_cons_sin_param

      FOREACH cur_cons_sin_param INTO g_arr_tipo_credito[v_indice].*
         LET v_indice = v_indice + 1
      END FOREACH
   END FOR
   CALL g_arr_tipo_credito.deleteElement(v_indice) 
END FUNCTION

#OBJETIVO: Consultar todos los datos de Créditos Tradicionales 
FUNCTION fn_cons_cred_tradicionales()
   LET v_indice = 1
   --20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
   FOR i = 1 TO arr_tbl_mov.getLength()

      LET v_QryTxt = "\n SELECT cre.tpo_credito ||'-'|| tcr.desc_credito,",
                    "\n        mov.subcuenta ||'-'|| sub.subcuenta_desc,",
                    "\n        SUM(mov.monto_pesos)",
                    "\n   FROM cre_acreditado cre,",
                    "\n        ",arr_tbl_mov[i]," mov,",
                    "\n        cat_tipo_credito tcr,",
                    "\n        cat_subcuenta sub",
                    "\n  WHERE cre.folio_liquida > 0",
                    "\n    AND cre.folio_liquida = mov.folio_liquida",
                    "\n    AND mov.movimiento IN (42,122,52,132)",
                    "\n    AND cre.tpo_credito =",  v_cmb_tpo_crd,
                    "\n    AND cre.tpo_credito = tcr.tpo_credito",
                    "\n    AND mov.subcuenta = sub.subcuenta"

      IF v_fecha_ini IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
         LET v_QryTxt =  v_QryTxt || "\n    AND mov.f_liquida BETWEEN ", "'",v_fecha_ini,"'", 
                                     "\n    AND ", "'",v_fecha_fin,"'" 
      END IF

      LET v_QryTxt =  v_QryTxt || "\n GROUP BY 1,2",
                                  "\n ORDER BY 1,2"
   -- DISPLAY "TRADICIONALES \n",v_QryTxt
      PREPARE prp_cons_cred_trad FROM v_QryTxt
      DECLARE cur_cons_cred_trad  CURSOR FOR prp_cons_cred_trad

      FOREACH cur_cons_cred_trad INTO g_arr_tipo_credito[v_indice].*
         LET v_indice = v_indice + 1
      END FOREACH

   END FOR
   CALL g_arr_tipo_credito.deleteElement(v_indice)
END FUNCTION

#OBJETIVO: Consultar todos los datos de Apoyo Infonavit 
FUNCTION fn_cons_cred_apoyo_infonavit()
   LET v_indice = 1
   --20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
   FOR i = 1 TO arr_tbl_mov.getLength()

      LET v_QryTxt = "\n SELECT cre.tpo_credito ||'-'|| tcr.desc_credito,",
                    "\n        mov.subcuenta ||'-'|| sub.subcuenta_desc,",
                    "\n        SUM(mov.monto_pesos)",
                    "\n   FROM cre_acreditado cre,",
                    "\n        ",arr_tbl_mov[i]," mov,",
                    "\n        cat_tipo_credito tcr,",
                    "\n        cat_subcuenta sub",
                    "\n  WHERE cre.folio_liquida > 0",
                    "\n    AND cre.folio_liquida = mov.folio_liquida    ", 
                    "\n    AND mov.movimiento IN (62,142)",
                    "\n    AND cre.tpo_credito =",  v_cmb_tpo_crd,
                    "\n    AND cre.tpo_credito = tcr.tpo_credito",
                    "\n    AND mov.subcuenta = sub.subcuenta"

      IF v_fecha_ini IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
         LET v_QryTxt =  v_QryTxt || "\n    AND mov.f_liquida BETWEEN ", "'",v_fecha_ini,"'", 
                                     "\n    AND ", "'",v_fecha_fin,"'" 
      END IF                 

      LET v_QryTxt =  v_QryTxt || "\n GROUP BY 1,2",
                                  "\n ORDER BY 1,2"
                               
   -- DISPLAY "APOYOS INFONAVIT \n",v_QryTxt
      PREPARE prp_cons_apoyo_inf FROM v_QryTxt
      DECLARE cur_cons_apoyo_inf CURSOR FOR prp_cons_apoyo_inf

      LET v_indice = 1
      FOREACH cur_cons_apoyo_inf INTO g_arr_tipo_credito[v_indice].*
         LET v_indice = v_indice + 1
      END FOREACH
   END FOR
   CALL g_arr_tipo_credito.deleteElement(v_indice)
END FUNCTION 

#OBJETIVO: Consultar todos los datos de Créditos Cofinanciados
FUNCTION fn_cons_cred_cofinanciados()
   LET v_indice = 1
   --20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
   FOR i = 1 TO arr_tbl_mov.getLength()

      LET v_QryTxt = "\n SELECT cre.tpo_credito ||'-'|| tcr.desc_credito,",
                    "\n        mov.subcuenta ||'-'|| sub.subcuenta_desc,",
                    "\n        SUM(mov.monto_pesos)",
                    "\n   FROM cre_acreditado cre,",
                    "\n        ",arr_tbl_mov[i]," mov,",
                    "\n        cat_tipo_credito tcr,",
                    "\n        cat_subcuenta sub",
                    "\n  WHERE cre.folio_liquida > 0",
                    "\n    AND cre.folio_liquida = mov.folio_liquida    ",
                    "\n    AND mov.movimiento IN (72,152,82,162)",
                    "\n    AND cre.tpo_credito =", v_cmb_tpo_crd,
                    "\n    AND cre.tpo_credito = tcr.tpo_credito",
                    "\n    AND mov.subcuenta = sub.subcuenta"

      IF v_fecha_ini IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
         LET v_QryTxt =  v_QryTxt || "\n    AND mov.f_liquida BETWEEN ", "'",v_fecha_ini,"'", 
                                     "\n    AND ", "'",v_fecha_fin,"'" 
      END IF

      LET v_QryTxt =  v_QryTxt || "\n GROUP BY 1,2",
                                  "\n ORDER BY 1,2"
                               
   -- DISPLAY "COFINANCIADOS \n",v_QryTxt
    
      PREPARE prp_cons_cofinanciados FROM v_QryTxt
      DECLARE cur_cons_cofinanciados CURSOR FOR prp_cons_cofinanciados

      LET v_indice = 1
      FOREACH cur_cons_cofinanciados INTO g_arr_tipo_credito[v_indice].*
         LET v_indice = v_indice + 1
      END FOREACH
   END FOR
   CALL g_arr_tipo_credito.deleteElement(v_indice)
   
END FUNCTION 

--g_arr_tipo_credito
--Genera un archivo de salida en texto plano con la información de los saldos vivienda
FUNCTION fn_genera_archivo_saldos_vivienda()

   DEFINE 
      v_nom_archivo        VARCHAR(40), -- nombre del archivo de salida
      v_ruta_envio_cnt     LIKE seg_modulo.ruta_envio,
      v_ruta_nomarch       VARCHAR(100), -- ruta y nombre del archivo de salida
      v_ch_arch_salida     BASE.CHANNEL,
      v_recorre_arreglo    INTEGER,
      v_archivo_copia      VARCHAR (50),
      v_comando_dos        STRING,
      v_encabezado         STRING,
      v_detalle            STRING,
      v_sumario            STRING,
      v_desc_credito       VARCHAR (30)

   DEFINE 
      v_fecha_archivo      DATE,  
      v_hora_archivo       DATETIME HOUR TO HOUR ,
      v_min_archivo        DATETIME MINUTE TO MINUTE,
      v_sec_archivo        DATETIME SECOND TO SECOND,
      v_hora               STRING

   LET v_fecha_archivo = TODAY 
   LET v_hora_archivo = CURRENT HOUR TO HOUR
   LET v_min_archivo = CURRENT MINUTE TO MINUTE
   LET v_sec_archivo = CURRENT SECOND TO SECOND
   
   LET v_hora = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".cnt"
      
   LET v_nom_archivo = "/saldos_vivienda_",v_hora

   -- se obtienen la ruta envio del módulo
   SELECT ruta_envio 
   INTO v_ruta_envio_cnt
   FROM seg_modulo
   WHERE modulo_cod = "cnt"

   LET v_ruta_nomarch = v_ruta_envio_cnt CLIPPED || v_nom_archivo CLIPPED 
   DISPLAY "Ruta: ",v_ruta_nomarch

   -- se crea el manejador de archivo y se indica que se escribirá en el mismo
   LET v_ch_arch_salida = base.Channel.create()
   CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
   CALL v_ch_arch_salida.setDelimiter("")

   --Imprime encabezado del archivo
   --v_cmb_tpo_crd,v_fecha_ini,v_fecha_fin 
   IF v_cmb_tpo_crd IS NOT NULL THEN 
 
      SELECT desc_credito
      INTO v_desc_credito
      FROM cat_tipo_credito
      WHERE tpo_credito = v_cmb_tpo_crd

      LET v_encabezado = "Tipo Crédito: ",v_cmb_tpo_crd,"-",v_desc_credito
      CALL v_ch_arch_salida.write([v_encabezado])
      
   END IF 

   IF v_fecha_ini IS NOT NULL THEN
      LET v_encabezado = "Fecha inicial: ",v_fecha_ini USING "dd-mm-yyyy"
      CALL v_ch_arch_salida.write([v_encabezado])
   END IF 

   IF v_fecha_fin IS NOT NULL THEN
      LET v_encabezado = "Fecha final: ",v_fecha_fin USING "dd-mm-yyyy"
      CALL v_ch_arch_salida.write([v_encabezado])
   END IF 
   

   LET v_encabezado = "Tipo Crédito|Subcuenta|Monto|"
   CALL v_ch_arch_salida.write([v_encabezado])
   
   FOR v_recorre_arreglo = 1 TO g_arr_tipo_credito.getLength()
      LET v_detalle = g_arr_tipo_credito[v_recorre_arreglo].v_tipo_cred, "|",
                      g_arr_tipo_credito[v_recorre_arreglo].v_subcta, "|",
                      g_arr_tipo_credito[v_recorre_arreglo].v_monto, "|"
                      
                      
    
      CALL v_ch_arch_salida.write([v_detalle])

   END FOR

   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   
   --Cambia el formato del archivo a DOS
   LET v_comando_dos = "unix2dos ",v_ruta_envio_cnt CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   CALL fn_mensaje("Información","Se ha generado el archivo de Saldos Vivienda por Tipo de Acreditado\n en la ruta"||v_ruta_nomarch,"information")
   

END FUNCTION 


#OBJETIVO: Generación del reporte de los montos por Tipo de Crédito.
FUNCTION fn_rpt_montos_tipo_cred()
DEFINE manejador_rpt    om.SaxDocumentHandler,  -- Contenedor documentos reporte
       v_ind_ini INTEGER 
       
   IF fgl_report_loadCurrentSettings("CNTC082.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   END IF

      --Inicia el REPORTE DE MONTOS POR TIPO DE CRÉDITO.
   START REPORT rpt_montos_tipo_cred TO XML HANDLER manejador_rpt
      FOR v_ind_ini = 1 TO v_indice
         OUTPUT TO REPORT rpt_montos_tipo_cred(g_arr_tipo_credito[v_ind_ini].*,
                                               g_usuario)
      END FOR 
   FINISH REPORT rpt_montos_tipo_cred

END FUNCTION 

REPORT rpt_montos_tipo_cred(p_arr_tipo_credito,p_usuario)
DEFINE p_usuario LIKE seg_usuario.usuario,
       v_fecha_reporte  DATE 
DEFINE p_arr_tipo_credito RECORD
        v_tipo_cred CHAR(30),
        v_subcta CHAR(40),
        v_monto DECIMAL(22,2)
END RECORD
FORMAT 
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario
   ON EVERY ROW
      PRINTX p_arr_tipo_credito.* 
END REPORT