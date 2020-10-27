--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/06/2012
--===============================================================

#########################################################################################
#Modulo       => pag                                                                    #
#Programa     => PAGC22                                                                 #
#Objetivo     => Consulta Análisis del Registro de Pagos                                #
#Fecha inicio => Junio 28, 2012                                                         # 
#Autor        => Oscar Ramírez Ramírez                                                  #
#########################################################################################

DATABASE safre_viv

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- Función que hace realiza la consulta del análisis del registro de pagos
   CALL fn_consulta_registros(p_usuario_cod)

END MAIN 

{ ======================================================================
Clave: PAGC22
Nombre: fn_consulta_registros
Fecha creacion: JUNIO 28, 2012
Autor: Oscar Ramírez Ramírez
Narrativa del proceso que realiza:

lleva a cabo la consulta del Análisis del Registro de Pagos (Repote),
apartir de una fecha determinada (año y mes)

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_registros(p_usuario_cod)
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod -- clave del usuario
DEFINE v_anio,v_mes       INTEGER -- variables para la captura de la fecha
DEFINE v_tot_registros    INTEGER --variable que indica la cantidad de folios encontrados


   OPEN WINDOW w_consulta_registros WITH FORM "PAGC221"

   INPUT v_anio,v_mes FROM cb_anio,cb_mes
   ATTRIBUTES(UNBUFFERED,WITHOUT DEFAULTS,ACCEPT=FALSE)

      BEFORE INPUT 
         CALL fn_llena_combos("cb_anio","cb_mes") -- Se cargan los datos para elegir el año y el mes
         LET v_mes = 1
         LET v_anio = 2012

      ON ACTION reporte
         IF (v_anio == 0) OR (v_mes == 0) THEN
            CALL fn_mensaje("Seleccione Fecha",
                        "Debe seleccionar la fecha correctamente.",   
                        "information")
            CONTINUE INPUT
         END IF
         SELECT COUNT(*)
         INTO v_tot_registros 
         FROM glo_folio
         WHERE MONTH(f_actualiza) = v_mes
               AND YEAR(f_actualiza) = v_anio
         IF v_tot_registros > 0 THEN 
            CALL fn_genera_reporte(v_anio,v_mes)
         ELSE
            CALL fn_mensaje("Sin Información",
                        "No hay información para la fecha seleccionada.",   
                        "information")
            CONTINUE INPUT
         END IF 
      
      ON ACTION CANCEL
         EXIT INPUT

   END INPUT 

   CLOSE WINDOW w_consulta_registros

END FUNCTION

{ ======================================================================
Clave: PAGC22
Nombre: fn_llena_combos
Fecha creacion: JUNIO 28, 2012
Autor: Oscar Ramírez Ramírez
Narrativa del proceso que realiza:

Llenar de info los combos correspondientes al mes y al año

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_llena_combos(p_anio,p_mes)
DEFINE p_anio,p_mes       STRING --variables con los nombres de los combos a llenar
DEFINE v_combo            ui.ComboBox --variable para hacer referencia al combo
DEFINE v_indice           INTEGER

   -- Primero llenamos los datos del combo correspondiente al año
   LET v_combo = ui.ComboBox.forName(p_anio)

   CALL v_combo.clear()

   FOR v_indice=YEAR(TODAY) TO (YEAR(TODAY)-20) STEP -1
      CALL v_combo.addItem(v_indice,v_indice)
   END FOR

   -- Ahora llenamos el combo correspondiente al mes
   LET v_combo = ui.ComboBox.forName(p_mes)

   CALL v_combo.clear()

   CALL v_combo.addItem(1,"ENERO")
   CALL v_combo.addItem(2,"FEBRERO")
   CALL v_combo.addItem(3,"MARZO")
   CALL v_combo.addItem(4,"ABRIL")
   CALL v_combo.addItem(5,"MAYO")
   CALL v_combo.addItem(6,"JUNIO")
   CALL v_combo.addItem(7,"JULIO")
   CALL v_combo.addItem(8,"AGOSTO")
   CALL v_combo.addItem(9,"SEPTIEMBRE")
   CALL v_combo.addItem(10,"OCTUBRE")
   CALL v_combo.addItem(11,"NOVIEMBRE")
   CALL v_combo.addItem(12,"DICIEMBRE")
   

END FUNCTION 

{ ======================================================================
Clave: PAGC22
Nombre: fn_genera_reporte
Fecha creacion: JUNIO 28, 2012
Autor: Oscar Ramírez Ramírez
Narrativa del proceso que realiza:

Función encargada de preparar la info para mandarla directamente al reporte

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_reporte(p_anio,p_mes)
DEFINE p_anio,p_mes                    INTEGER -- corresponden al año y mes a obtener los datos en la BD
DEFINE v_num_pagos_lqinfo              INTEGER -- guardará el total de pagos recibidos en el LQINFO masivo
DEFINE v_importe_aporta_lqinfo         DECIMAL(18,2) -- guardará el importe de aportación del LQINFO masivo
DEFINE v_importe_amorta_lqinfo         DECIMAL(18,2) -- guardará el importe de amortización del LQINFO masivo
DEFINE v_suma_aporta_amorta_lqinfo     DECIMAL(15,0) -- guardará la suma entre la amortización y la aportacion del LQINFO masivo
DEFINE v_num_pagos_aclara              INTEGER -- guardará el total de pagos recibidos marcados en aclaratorio
DEFINE v_importe_aporta_aclara         DECIMAL(18,2) -- guardará el importe de aportación marcados en aclaratorio
DEFINE v_importe_amorta_aclara         DECIMAL(18,2) -- guardará el importe de amortización marcados en aclaratorio
DEFINE v_suma_aporta_amorta_aclara     DECIMAL(15,0) -- guardará la suma entre la amortización y la aportacion en aclaratorio
DEFINE v_porciento_no_reg_aclara       DECIMAL(4,2)  -- guardará el porcentaje correspondiente a los registros marcados en aclaratorio
DEFINE v_porciento_importe_aclara      DECIMAL(4,2)  -- guardará el porcentaje correspondiente al importe marcado en aclaratorio
DEFINE v_num_pagos_causa13_17          INTEGER -- guardará el total de pagos que salieron de aclaratorio por tener causal 13 y/o 17
DEFINE v_importe_aporta_causa13_17     DECIMAL(18,2) -- guardará el importe de aportación que salieron de aclaratorio por tener causal 13 y/o 17
DEFINE v_importe_amorta_causa13_17     DECIMAL(18,2) -- guardará el importe de amortización que salieron de aclaratorio por tener causal 13 y/o 17
DEFINE v_suma_aporta_amorta_causa13_17 DECIMAL(15,0) -- guardará la suma entre la amortización y la aportacion que salieron de aclaratorio por tener causal 13 y/o 17
DEFINE v_porciento_no_reg_causa13_17   DECIMAL(4,2)  -- guardará el porcentaje correspondiente a los registros en aclaratorio por tener causal 13 y/o 17
DEFINE v_porciento_importe_causa13_17  DECIMAL(4,2)  -- guardará el porcentaje correspondiente al importe en aclaratorio por tener causal 13 y/o 17
DEFINE v_num_pagos_reg_aclara          INTEGER -- guardará el total de pagos que se registraron efectivamente en aclaratorio
DEFINE v_importe_aporta_reg_aclara     DECIMAL(18,2) -- guardará el importe de aportación que se registraron efectivamente en aclaratorio
DEFINE v_importe_amorta_reg_aclara     DECIMAL(18,2) -- guardará el importe de amortización que se registraron efectivamente en aclaratorio
DEFINE v_suma_aporta_amorta_reg_aclara DECIMAL(15,0) -- guardará la suma entre la amortización y la aportacion que se registraron efectivamente en aclaratorio
DEFINE v_porciento_no_reg_reg_aclara   DECIMAL(4,2)  -- guardará el porcentaje correspondiente a los registros que efectivamente se registraron en aclaratorio
DEFINE v_porciento_importe_reg_aclara  DECIMAL(4,2)  -- guardará el porcentaje correspondiente al importe que efectivamente se registraron en aclaratorio
DEFINE manejador_rpt                   OM.SaxDocumentHandler -- Manejador del reporte
DEFINE v_ruta_ejecutable, 
       v_ruta_listados, 
       v_ruta_reporte                  STRING

   -- Se obtiene el No. de pagos recibidos en el LQINFO masivo
   SELECT SUM(total_registros)
   INTO v_num_pagos_lqinfo
   FROM pag_sum_recauda
   WHERE folio IN (
      SELECT folio
      FROM glo_folio       
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio
   )

   -- Se obtiene el Importe de Aportación de LQINFO masivo
   SELECT SUM(imp_ap_pat)
   INTO v_importe_aporta_lqinfo
   FROM pag_sum_recauda
   WHERE folio IN (
      SELECT folio
      FROM glo_folio          
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio
   )

   -- Se obtiene el Importe de Amortización de LQINFO masivo
   SELECT SUM(imp_am_crd)
   INTO v_importe_amorta_lqinfo
   FROM pag_sum_recauda
   WHERE folio in (
      SELECT folio
      FROM glo_folio          
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio
   )

   -- Se obtiene la Suma de los importes de Aportación y Amortización de LQINFO masivo

   LET v_suma_aporta_amorta_lqinfo = v_importe_aporta_lqinfo + v_importe_amorta_lqinfo

   -- Se obtiene el No. de pagos Marcados en Aclaratorio
   SELECT COUNT(*)
   INTO v_num_pagos_aclara
   FROM cta_his_pagos
   WHERE folio IN (  
      SELECT folio
      FROM glo_folio          
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND localiza_trabajador = 3

   --Se obtiene el porcentaje de registros de pagos Marcados en Aclaratorio
   LET v_porciento_no_reg_aclara = (v_num_pagos_aclara / v_num_pagos_lqinfo) * 100

   
   -- Se obtiene el importe de Aportación Marcados en Aclaratorio
   SELECT SUM(imp_ap_pat)
   INTO v_importe_aporta_aclara
   FROM cta_his_pagos
   WHERE folio IN (
      SELECT folio
      FROM glo_folio          
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND localiza_trabajador = 3

   -- Se obtiene el importe de Amotización Marcados en Aclaratorio
   SELECT SUM(imp_am_cre)
   INTO v_importe_amorta_aclara
   FROM cta_his_pagos
   WHERE folio IN (
      SELECT folio
      FROM glo_folio          
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND localiza_trabajador = 3

   -- Se obtiene la suma del importe de las Aportaciones y Amortizaciones Marcados en Aclaratorio
   LET v_suma_aporta_amorta_aclara = v_importe_aporta_aclara + v_importe_amorta_aclara

   -- Se obtiene el porcentaje correspondiente al Importe de pagos Marcados en Aclaratorio
   LET v_porciento_importe_aclara = (v_suma_aporta_amorta_aclara / v_suma_aporta_amorta_lqinfo) * 100


   -- Se obtiene el numero de pagos que salieron de Aclaratorio por tener Causal 13 y/o 17
   SELECT COUNT(*)
   INTO v_num_pagos_causa13_17
   FROM cta_his_pagos
   WHERE folio IN (
      SELECT folio
      FROM glo_folio  
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND tpo_aclaracion IN (13,17)

 
   -- Se obtiene el porcentaje de registros de pagos que salieron de Aclaratorio por tener Causal 13 y/o 17
   LET v_porciento_no_reg_causa13_17 = (v_num_pagos_causa13_17 / v_num_pagos_lqinfo) * 100

   -- Se obtiene el importe de Aportación de pagos que salieron de Aclaratorio por tener Causal 13 y/o 17
   SELECT SUM(imp_ap_pat)
   INTO v_importe_aporta_causa13_17
   FROM cta_his_pagos
   WHERE folio IN (
      SELECT folio
      FROM glo_folio  
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND tpo_aclaracion IN (13,17)


   -- Se obtiene el importe de Amortización de pagos que salieron de Aclaratorio por tener Causal 13 y/o 17
   SELECT SUM(imp_am_cre)
   INTO v_importe_amorta_causa13_17
   FROM cta_his_pagos
   WHERE folio IN (
      SELECT folio
      FROM glo_folio  
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND tpo_aclaracion IN (13,17)


   -- Se obtiene la suma entre las aportaciones y amortizaciones de pagos que salieron de Aclaratorio por tener Causal 13 y/o 17
   LET v_suma_aporta_amorta_causa13_17 = v_importe_aporta_causa13_17 + v_importe_amorta_causa13_17

   -- Se obtiene el porcentaje correspondiente al Importe de pagos que salieron de Aclaratorio por tener Causal 13 y/o 17
   LET v_porciento_importe_causa13_17 = (v_suma_aporta_amorta_causa13_17 / v_suma_aporta_amorta_lqinfo) * 100

   -- Se obtiene el No. de pagos del total de registros que efectivamente se registraron en Aclaratorio
   SELECT COUNT(*)
   INTO v_num_pagos_reg_aclara
   FROM cta_his_pagos
   WHERE folio IN (
      SELECT folio
      FROM glo_folio  
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND tpo_aclaracion IN (13,17)
   AND ind_liquidacion IN (2,3,5)

   -- Se obtiene el porcentaje del núm. de registros que efectivamente se registraron en Aclaratorio
   LET v_porciento_no_reg_reg_aclara = (v_num_pagos_reg_aclara / v_num_pagos_lqinfo) * 100

   -- Se obtiene el importe de Aportación de registros que efectivamente se registraron en Aclaratorio
   SELECT SUM(imp_ap_pat)
   INTO v_importe_aporta_reg_aclara
   FROM cta_his_pagos
   WHERE folio IN (
      SELECT folio
      FROM glo_folio  
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND tpo_aclaracion IN (13,17)
   AND ind_liquidacion IN (2,3,5)


   -- Se obtiene el importe de Amortización de registros que efectivamente se registraron en Aclaratorio
   SELECT SUM(imp_am_cre)
   INTO v_importe_amorta_reg_aclara
   FROM cta_his_pagos
   WHERE folio IN (
      SELECT folio
      FROM glo_folio  
      WHERE MONTH(f_actualiza) = p_mes AND YEAR(f_actualiza) = p_anio )
   AND tpo_aclaracion IN (13,17)
   AND ind_liquidacion IN (2,3,5)

   -- Se obtiene la suma de Aportaciones y Amortizaciones que efectivamente se registraron en Aclaratorio
   LET v_suma_aporta_amorta_reg_aclara = v_importe_aporta_reg_aclara + v_importe_amorta_reg_aclara

   -- Se obtiene el porcentaje del importe que efectivamente se registraron en Aclaratorio
   LET v_porciento_importe_reg_aclara = (v_suma_aporta_amorta_reg_aclara / v_suma_aporta_amorta_lqinfo) * 100

   
   # Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_listados
            
   --Se asigna la plantilla para generar el reporte
   IF fgl_report_loadCurrentSettings("rpt_analisis_registro_pagos.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","analisis_registro_pagos"
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET manejador_rpt = fgl_report_commitCurrentSettings() 
   ELSE         
      DISPLAY "no fue posible generar el reporte"
      EXIT PROGRAM 
   END IF
            
   -- Se pasa toda la info para el formato en el reporte
   START REPORT rpt_analisis_registro_pagos TO XML HANDLER manejador_rpt
      OUTPUT TO REPORT rpt_analisis_registro_pagos(p_anio,
                                                   p_mes,
                                                   v_num_pagos_lqinfo,
                                                   v_importe_aporta_lqinfo,
                                                   v_importe_amorta_lqinfo,
                                                   v_suma_aporta_amorta_lqinfo,
                                                   v_num_pagos_aclara,
                                                   v_importe_aporta_aclara,
                                                   v_importe_amorta_aclara,
                                                   v_suma_aporta_amorta_aclara,
                                                   v_porciento_no_reg_aclara,
                                                   v_porciento_importe_aclara,
                                                   v_num_pagos_causa13_17,
                                                   v_importe_aporta_causa13_17,
                                                   v_importe_amorta_causa13_17,
                                                   v_suma_aporta_amorta_causa13_17,
                                                   v_porciento_no_reg_causa13_17,
                                                   v_porciento_importe_causa13_17,
                                                   v_num_pagos_reg_aclara,
                                                   v_importe_aporta_reg_aclara,
                                                   v_importe_amorta_reg_aclara,
                                                   v_suma_aporta_amorta_reg_aclara,
                                                   v_porciento_no_reg_reg_aclara,
                                                   v_porciento_importe_reg_aclara)
   FINISH REPORT rpt_analisis_registro_pagos

END FUNCTION


REPORT rpt_analisis_registro_pagos(p_anio,
                                    p_mes,
                                    p_num_pagos_lqinfo,
                                    p_importe_aporta_lqinfo,
                                    p_importe_amorta_lqinfo,
                                    p_suma_aporta_amorta_lqinfo,
                                    p_num_pagos_aclara,
                                    p_importe_aporta_aclara,
                                    p_importe_amorta_aclara,
                                    p_suma_aporta_amorta_aclara,
                                    p_porciento_no_reg_aclara,
                                    p_porciento_importe_aclara,
                                    p_num_pagos_causa13_17,
                                    p_importe_aporta_causa13_17,
                                    p_importe_amorta_causa13_17,
                                    p_suma_aporta_amorta_causa13_17,
                                    p_porciento_no_reg_causa13_17,
                                    p_porciento_importe_causa13_17,
                                    p_num_pagos_reg_aclara,
                                    p_importe_aporta_reg_aclara,
                                    p_importe_amorta_reg_aclara,
                                    p_suma_aporta_amorta_reg_aclara,
                                    p_porciento_no_reg_reg_aclara,
                                    p_porciento_importe_reg_aclara)
DEFINE p_anio,p_mes                    INTEGER
DEFINE p_num_pagos_lqinfo              INTEGER
DEFINE p_importe_aporta_lqinfo         DECIMAL(18,2)
DEFINE p_importe_amorta_lqinfo         DECIMAL(18,2)
DEFINE p_suma_aporta_amorta_lqinfo     DECIMAL(15,0)
DEFINE p_num_pagos_aclara              INTEGER
DEFINE p_importe_aporta_aclara         DECIMAL(18,2)
DEFINE p_importe_amorta_aclara         DECIMAL(18,2)
DEFINE p_suma_aporta_amorta_aclara     DECIMAL(15,0)
DEFINE p_porciento_no_reg_aclara       DECIMAL(4,2) 
DEFINE p_porciento_importe_aclara      DECIMAL(4,2) 
DEFINE p_num_pagos_causa13_17          INTEGER
DEFINE p_importe_aporta_causa13_17     DECIMAL(18,2)
DEFINE p_importe_amorta_causa13_17     DECIMAL(18,2)
DEFINE p_suma_aporta_amorta_causa13_17 DECIMAL(15,0)
DEFINE p_porciento_no_reg_causa13_17   DECIMAL(4,2)
DEFINE p_porciento_importe_causa13_17  DECIMAL(4,2)
DEFINE p_num_pagos_reg_aclara          INTEGER 
DEFINE p_importe_aporta_reg_aclara     DECIMAL(18,2)
DEFINE p_importe_amorta_reg_aclara     DECIMAL(18,2)
DEFINE p_suma_aporta_amorta_reg_aclara DECIMAL(15,0)
DEFINE p_porciento_no_reg_reg_aclara   DECIMAL(4,2) 
DEFINE p_porciento_importe_reg_aclara  DECIMAL(4,2)
DEFINE v_fecha_reporte                 DATE

   FORMAT 


      ON EVERY ROW
         LET v_fecha_reporte = TODAY 
         PRINTX p_anio
         PRINTX p_mes
         PRINTX p_num_pagos_lqinfo
         PRINTX p_importe_aporta_lqinfo
         PRINTX p_importe_amorta_lqinfo
         PRINTX p_suma_aporta_amorta_lqinfo
         PRINTX p_num_pagos_aclara 
         PRINTX p_importe_aporta_aclara
         PRINTX p_importe_amorta_aclara
         PRINTX p_suma_aporta_amorta_aclara
         PRINTX p_porciento_no_reg_aclara 
         PRINTX p_porciento_importe_aclara
         PRINTX p_num_pagos_causa13_17
         PRINTX p_importe_aporta_causa13_17
         PRINTX p_importe_amorta_causa13_17
         PRINTX p_suma_aporta_amorta_causa13_17
         PRINTX p_porciento_no_reg_causa13_17
         PRINTX p_porciento_importe_causa13_17
         PRINTX p_num_pagos_reg_aclara 
         PRINTX p_importe_aporta_reg_aclara
         PRINTX p_importe_amorta_reg_aclara
         PRINTX p_suma_aporta_amorta_reg_aclara
         PRINTX p_porciento_no_reg_reg_aclara
         PRINTX p_porciento_importe_reg_aclara
         PRINTX v_fecha_reporte USING "DD-MM-YYYY"
         
END REPORT
