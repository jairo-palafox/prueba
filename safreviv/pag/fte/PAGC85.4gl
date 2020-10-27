----------------------------------------------------------------------------------------
-- Modulo       => PAG
-- Programa     => PAGC85
-- Objetivo     => Reporte previo Integración archivo LQINFO.
-- Autor        => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio => 8 de Febrero de 2015.
-----------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.
-- Fec Mod.     => 27 de Marzo de 2018.
-- Fec ini Mod. => 5 de Abril 2018.
-- Modificación => Que el reporte se puda imprimir cuando el usuario lo requiera.
-- Clave cambio => saci2018-13
-- Actualizado  => Gerardo Alfonso Vega Paredes.
-----------------------------------------------------------------------------------------
-- Fec ini Mod. => 6 de Junio 2018.
-- Modificación => Convertir programa en batch. Mandar correo al usuario cuando finalice.
-- Clave cambio => saci2018-13-02
-----------------------------------------------------------------------------------------
-- Fec ini Mod. => 15 de febrero 2019.
-- Modificación => Se colocó función "CALL STARTLOG"
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

   DEFINE
      p_usuario     LIKE seg_usuario.usuario_cod,
      p_pid         DECIMAL (9,0),
      p_proceso_cod LIKE cat_proceso.proceso_cod,
      p_opera_cod   LIKE cat_operacion.opera_cod,
      r_bandera      SMALLINT

   DEFINE v_causal_sin_distincion DYNAMIC ARRAY OF RECORD
      v_causal CHAR(9),
      v_cuenta DECIMAL(10,0)
   END RECORD

   DEFINE v_causal_afore DYNAMIC ARRAY OF RECORD
      v_causal CHAR(9),
      v_cuenta DECIMAL(10,0)
   END RECORD

   DEFINE v_causal_credito DYNAMIC ARRAY OF RECORD
      v_causal CHAR(9),
      v_cuenta DECIMAL(10,0)
   END RECORD

   DEFINE v_nombre_archivo CHAR(40)        --saci2018-13
   DEFINE v_cuenta_pagos   DECIMAL(10,0)
   DEFINE v_cuenta_nss     DECIMAL(10,0)
   DEFINE v_cuenta_nrp     DECIMAL(10,0)
   DEFINE v_total_ssv_sin  DECIMAL(10,0)
   DEFINE v_total_ssv_con  DECIMAL(10,0)
   DEFINE v_total_cre_sin  DECIMAL(10,0)
   DEFINE v_total_cre_con  DECIMAL(10,0)

MAIN

   --Asignación de parametros generales
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET v_nombre_archivo = ARG_VAL(5)

   CALL STARTLOG(p_usuario CLIPPED|| ".PAGC85.log")

   CALL fn_crea_reporte()

   --Finaliza la operación
   CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
   RETURNING r_bandera
  
   IF r_bandera = 0 THEN 
      DISPLAY "Se ha realizado la generación del archivo."
      EXIT PROGRAM 
   ELSE --Si ocurrió error 
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
      CALL fn_desplega_inc_operacion(r_bandera)
      EXIT PROGRAM 
   END IF   
   
END MAIN

FUNCTION fn_crea_reporte()
   DEFINE  manejador_rpt      om.SaxDocumentHandler
   DEFINE v_genero_reporte_pdf BOOLEAN
   DEFINE i                        INTEGER
   DEFINE i_min                    SMALLINT
   DEFINE i_max                    INTEGER
   DEFINE v_total_causales         INTEGER
   DEFINE v_total_causales_afore   INTEGER
   DEFINE v_total_causales_credito INTEGER
   DEFINE v_porcentaje             DECIMAL(10,2)
   DEFINE v_estado                 SMALLINT
   DEFINE v_folio                  DECIMAL(9,0)
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte            VARCHAR(80)
   DEFINE v_existe                 SMALLINT

   LET v_nom_reporte = p_usuario     CLIPPED,"-PAGC85-",     --CLIPPED,"-GLOE02-",
                       p_pid         USING "&&&&&", "-",
                       p_proceso_cod USING "&&&&&","-",
                       1             USING "&&&&&"

   CALL fn_rutas("pag") RETURNING r_ruta_bin, r_ruta_listados
   
   LET v_genero_reporte_pdf= FALSE

   CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/PAGC851.4rp") RETURNING v_existe

   IF v_existe THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte)
      CALL fgl_report_selectPreview(0)  -- 1=Genera reporte de inmediato 0=Genera reporte en ruta
      LET manejador_rpt = fgl_report_commitCurrentSettings()
      LET v_genero_reporte_pdf = TRUE
   ELSE
      RETURN v_genero_reporte_pdf
   END IF

   SELECT estado,
          folio 
   INTO   v_estado,
          v_folio
   FROM   glo_ctr_archivo
   WHERE  nombre_archivo = v_nombre_archivo

   CALL fn_iniciar_consultas(v_estado,v_folio)  
   CALL fn_llenar_arreglos(v_estado,v_folio)

   LET i_min=1
   LET v_total_causales=0

   START REPORT reporte_causales_validacion TO XML HANDLER manejador_rpt  

   LET i_max=v_causal_sin_distincion.getLength()

   FOR i=i_min TO  i_max
      LET v_total_causales=v_total_causales+v_causal_sin_distincion[i].v_cuenta
      LET v_porcentaje=(v_causal_sin_distincion[i].v_cuenta/(v_total_ssv_con+v_total_cre_con))*100
      --DISPLAY v_porcentaje
      --CONTINUE FOR
      OUTPUT TO REPORT reporte_causales_validacion(v_causal_sin_distincion[i].*,v_total_causales,2,v_porcentaje||"%")
   END FOR

   --  EXIT PROGRAM
   LET v_porcentaje = 0
   LET v_total_causales_afore = 0
   LET i_max=v_causal_afore.getLength()

   FOR i=i_min TO  i_max 
      LET v_total_causales_afore = v_total_causales_afore + v_causal_afore[i].v_cuenta
      LET v_porcentaje = (v_causal_afore[i].v_cuenta / v_total_ssv_con) * 100
      OUTPUT TO REPORT reporte_causales_validacion(v_causal_afore[i].*,v_total_causales_afore,0,v_porcentaje||"%")
   END FOR
   
   LET v_porcentaje = 0
   LET v_total_causales_credito = 0
   LET i_max=v_causal_credito.getLength()
   FOR i = i_min TO i_max 
       LET v_total_causales_credito = v_total_causales_credito + v_causal_credito[i].v_cuenta
       LET v_porcentaje = (v_causal_credito[i].v_cuenta / v_total_cre_con) * 100
       OUTPUT TO REPORT reporte_causales_validacion(v_causal_credito[i].*,v_total_causales_credito,1,v_porcentaje||"%")
   END FOR
   
   FINISH REPORT reporte_causales_validacion

END FUNCTION

FUNCTION fn_iniciar_consultas(v_estado,v_folio)
   DEFINE v_estado SMALLINT,
          v_folio  DECIMAL(9,0)

   IF v_estado = 1 THEN
      --CUENTA PAGOS
      SELECT COUNT(*)
      INTO   v_cuenta_pagos
      FROM   tmp_det_trabajador
      WHERE  tpo_registro=4

      --OBTIENE NSS
      SELECT COUNT(DISTINCT nss)
      INTO   v_cuenta_nss
      FROM   tmp_det_trabajador
      WHERE  tpo_registro = 4

      --Obtiene NRP
      SELECT COUNT(DISTINCT nrp)
      INTO   v_cuenta_nrp
      FROM   tmp_det_trabajador
      WHERE  tpo_registro = 4

      --Total SSV sin aclaratorio
      SELECT COUNT(*)
      INTO   v_total_ssv_sin
      FROM   tmp_det_trabajador
      WHERE  localiza_trabajajdor <> 3 
      AND    tpo_registro = 4 
      AND    destino_ap_viv = 2
      order by 1 ASC 

      --Total SSV con aclaratorio
      {
      SELECT COUNT(*)
      INTO v_total_ssv_con
      FROM tmp_det_trabajador
      WHERE localiza_trabajador=3 AND 
      destino_ap_viv =2
      order by 1 ASC }
    
      --Total credito sin aclaratorio
      SELECT COUNT(*)
      INTO   v_total_cre_sin
      FROM   tmp_det_trabajador
      WHERE  localiza_trabajajdor <> 3
      AND    tpo_registro = 4 
      AND    destino_ap_viv = 1
      order by 1 ASC 

      --Total credito con aclaratorio
      {SELECT COUNT(*)
      INTO v_total_cre_con
      FROM tmp_det_trabajador
      WHERE localiza_trabajador=3 AND 
      destino_ap_viv =1
      order by 1 asc }
   ELSE
      --CUENTA PAGOS
      SELECT COUNT(*)
      INTO   v_cuenta_pagos
      FROM   cta_his_pagos
      WHERE  folio = v_folio

      --OBTIENE NSS
      SELECT COUNT(DISTINCT id_derechohabiente)
      INTO   v_cuenta_nss
      FROM   cta_his_pagos
      WHERE  folio = v_folio

      --Obtiene NRP
      SELECT COUNT(DISTINCT nrp)
      INTO   v_cuenta_nrp
      FROM   cta_his_pagos
      WHERE  folio = v_folio

      --Total SSV sin aclaratorio
      SELECT COUNT(*)
      INTO   v_total_ssv_sin
      FROM   cta_his_pagos
      WHERE  folio = v_folio
      AND    localiza_trabajador <> 3 
      AND    destino_ap_viv = 2
      order by 1 ASC 

      --Total credito sin aclaratorio
      SELECT COUNT(*)
      INTO   v_total_cre_sin
      FROM   cta_his_pagos
      WHERE  folio = v_folio
      AND    localiza_trabajador <> 3
      AND    destino_ap_viv = 1
      order by 1 ASC 

   END IF	 

END FUNCTION

FUNCTION fn_llenar_arreglos(v_estado,v_folio)
   DEFINE v_estado SMALLINT,
          v_folio  DECIMAL(9,0)

   CALL fn_obtiene_aclaratorio_sin_distincion(v_estado,v_folio)
   CALL fn_obtiene_causales_afore(v_estado,v_folio)
   CALL fn_obtiene_causales_credito(v_estado,v_folio)
    
END FUNCTION

FUNCTION fn_obtiene_aclaratorio_sin_distincion(v_estado,v_folio)
   DEFINE v_estado SMALLINT,
          v_folio  DECIMAL(9,0)
   DEFINE v_cuenta_registros INTEGER
   DEFINE v_query STRING
   
   IF v_estado = 1 THEN
      LET v_query="SELECT 'Causal '||tpo_aclaracion, ",
                          "count(*) ",
                  "FROM   tmp_det_trabajador ",
                  "WHERE  localiza_trabajajdor = 3 ",
                  "AND    tpo_registro = 4 ",
                  "AND    destino_ap_viv IN (1,2) ",--AFORE Y CREDITO
                  "group by 1 ",
                  "order by 1 asc "
   ELSE
      LET v_query="SELECT 'Causal '||tpo_aclaracion, ",
                          "count(*) ",
                  "FROM   cta_his_pagos ",
                  "WHERE  folio = ",v_folio,
                  "AND    localiza_trabajador = 3 ",
                  "AND    destino_ap_viv IN (1,2) ",--AFORE Y CREDITO
                  "group by 1 ",
                  "order by 1 asc "   
   END IF

   PREPARE stm_causales_afore_credito FROM v_query
   DECLARE cur_causales_afore_credito CURSOR FOR stm_causales_afore_credito
   
   LET v_cuenta_registros=1

   FOREACH cur_causales_afore_credito INTO v_causal_sin_distincion[v_cuenta_registros].*
       LET v_cuenta_registros=v_cuenta_registros+1
   END FOREACH

   CALL v_causal_sin_distincion.deleteElement(v_causal_sin_distincion.getLength())
           
END FUNCTION

FUNCTION fn_obtiene_causales_afore(v_estado,v_folio)
   DEFINE v_estado SMALLINT,
          v_folio  DECIMAL(9,0)

   DEFINE v_query STRING
   DEFINE v_cuenta_registros INTEGER
   
   IF v_estado = 1 THEN 
      LET v_query="SELECT 'Causal '||tpo_aclaracion, ",
                          "count(*) ",
                  "FROM tmp_det_trabajador ",
                  "WHERE localiza_trabajajdor = 3 ",
                  "AND   tpo_registro = 4 ",
                  "AND   destino_ap_viv = 2 ", --AFORE
                  "group by 1 ",
                  "order by 1 asc "
   ELSE
      LET v_query="SELECT 'Causal '||tpo_aclaracion, ",
                          "count(*) ",
                  "FROM cta_his_pagos ",
                  "WHERE folio = ",v_folio,
                  "AND   localiza_trabajador = 3 ",
                  "AND   destino_ap_viv = 2 ", --AFORE
                  "group by 1 ",
                  "order by 1 asc "
   END IF

   PREPARE stm_causales_afore FROM v_query
   DECLARE cur_causales_afore CURSOR FOR stm_causales_afore

   LET v_cuenta_registros=1
   LET v_total_ssv_con=0
   FOREACH cur_causales_afore INTO v_causal_afore[v_cuenta_registros].*
       --DISPLAY v_total_ssv_con
       LET v_total_ssv_con=v_total_ssv_con+v_causal_afore[v_cuenta_registros].v_cuenta
       LET v_cuenta_registros=v_cuenta_registros+1
   END FOREACH
   CALL v_causal_afore.deleteElement(v_causal_afore.getLength())

END FUNCTION

FUNCTION fn_obtiene_causales_credito(v_estado,v_folio)
   DEFINE v_estado SMALLINT,
          v_folio  DECIMAL(9,0)
          
   DEFINE v_query STRING
   DEFINE v_cuenta_registros INTEGER

   IF v_estado = 1 THEN
      LET v_query="SELECT 'Causal '||tpo_aclaracion, ",
                          "count(*) ",
                   "FROM tmp_det_trabajador ",
                   "WHERE localiza_trabajajdor = 3 ",
                   "AND   tpo_registro   = 4 ",
                   "AND   destino_ap_viv = 1 ",  --CREDITO
                   "group by 1 ",
                   "order by 1 asc "
   ELSE
      LET v_query="SELECT 'Causal '||tpo_aclaracion, ",
                          "count(*) ",
                   "FROM cta_his_pagos ",
                   "WHERE folio = ",v_folio,
                   "AND   localiza_trabajador = 3 ",
                   "AND   destino_ap_viv = 1 ",  --CREDITO
                   "group by 1 ",
                   "order by 1 asc "
   END IF

   PREPARE stm_causales_credito FROM v_query
   DECLARE cur_causales_credito CURSOR FOR stm_causales_credito

   LET v_cuenta_registros=1
   LET v_total_cre_con=0
   FOREACH cur_causales_credito INTO v_causal_credito[v_cuenta_registros].*
      LET v_total_cre_con=v_total_cre_con+v_causal_credito[v_cuenta_registros].v_cuenta
      LET v_cuenta_registros=v_cuenta_registros+1
   END FOREACH
   
   CALL v_causal_credito.deleteElement(v_causal_credito.getLength())


END FUNCTION

REPORT reporte_causales_validacion(v_causal_sin_distincion_reporte,v_total_causales,v_tipo_arreglo,v_porcentaje)
   
   DEFINE v_causal_sin_distincion_reporte RECORD
      v_causal CHAR(9),
      v_cuenta DECIMAL(10,0)
   END RECORD
   
   DEFINE v_total_causales     INTEGER
   DEFINE v_tipo_arreglo       SMALLINT
   DEFINE v_tipo_tabla         STRING
   DEFINE v_porcentaje         STRING
   DEFINE v_tipo_registros_sin STRING
   DEFINE v_tipo_registros_con STRING
   DEFINE v_total_sin_string   STRING
   DEFINE v_total_con_string   STRING
   DEFINE v_total_registros    STRING
   DEFINE v_total_registros_ssv_cre_string STRING
   DEFINE v_total_porcentaje_sin_string    STRING
   DEFINE v_total_porcentaje_con_string    STRING
   DEFINE v_porcentaje_sin    DECIMAL(10,2)
   DEFINE v_porcentaje_con    DECIMAL(10,2)
   DEFINE v_signo_porcentaje  STRING
   DEFINE v_bandera_string    BOOLEAN
   DEFINE v_f_actualiza       STRING

   FORMAT

   FIRST PAGE HEADER

      LET v_f_actualiza = TODAY USING "dd-mm-yyyy"

      PRINTX v_cuenta_pagos
      PRINTX v_cuenta_nss 
      PRINTX v_cuenta_nrp 
      PRINTX v_f_actualiza 
      PRINTX v_nombre_archivo
   
   
   BEFORE GROUP OF v_tipo_arreglo
      CASE v_tipo_arreglo
         WHEN 0  
            LET v_tipo_registros_sin='Pagos Destino SSV Sin Aclaratorio:'
            LET v_tipo_registros_con='Pagos Destino SSV Con Aclaratorio:'
            LET v_total_registros_ssv_cre_string='Total pagos Destino SSV:'
            LET v_total_sin_string=v_total_ssv_sin USING "-,---,---,---"
            LET v_total_con_string=v_total_ssv_con USING "-,---,---,---"
            --    LET v_total_sin_string=v_total_sin_string CLIPPED
            --    LET v_total_con_string=v_total_con_string CLIPPED
            LET v_porcentaje_sin=(v_total_ssv_sin/(v_total_ssv_sin+v_total_ssv_con))*100
            LET v_porcentaje_con=(v_total_ssv_con/(v_total_ssv_sin+v_total_ssv_con))*100
            LET v_total_porcentaje_sin_string=v_porcentaje_sin||"%"
            LET v_total_porcentaje_con_string=v_porcentaje_con||"%"
            LET v_total_registros=v_total_ssv_sin+v_total_ssv_con
            LET v_total_registros=v_total_registros USING "-,---,---,---"
            LET v_tipo_tabla='Tipos de Causal Pagos Destino SSV con Aclaratorio'
            LET v_signo_porcentaje='%'
         WHEN 1 
            LET v_tipo_registros_sin='Pagos Destino Crédito Sin Aclaratorio:'
            LET v_tipo_registros_con='Pagos Destino Crédito Con Aclaratorio:'
            LET v_total_registros_ssv_cre_string='Total Pagos Destino Crédito:'            
            LET v_total_sin_string=v_total_cre_sin USING "-,---,---,---"
            LET v_total_con_string=v_total_cre_con USING "-,---,---,---"
            --  LET v_total_sin_string=v_total_sin_string CLIPPED
            --  LET v_total_con_string=v_total_con_string CLIPPED
            LET v_porcentaje_sin=(v_total_cre_sin/(v_total_cre_sin+v_total_cre_con))*100
            LET v_porcentaje_con=(v_total_cre_con/(v_total_cre_sin+v_total_cre_con))*100
            LET v_total_porcentaje_sin_string=v_porcentaje_sin||"%"
            LET v_total_porcentaje_con_string=v_porcentaje_con||"%"
            LET v_total_registros=v_total_cre_sin+v_total_cre_con
            LET v_total_registros=v_total_registros USING "-,---,---,---"
            LET v_tipo_tabla='Tipos de Causal Pagos Destino Crédito con Aclaratorio'
            LET v_signo_porcentaje='%'
         WHEN 2
            LET v_tipo_tabla='Aclaratorios sin distinción de destino'
            LET v_total_sin_string=' '
            LET v_total_con_string=' '
            --  LET v_porcentaje=' '
            LET v_tipo_registros_sin=' '
            LET v_tipo_registros_con=' '
            LET v_total_porcentaje_sin_string =' '
            LET v_total_porcentaje_con_string=' '
            LET v_total_registros_ssv_cre_string=' '
            LET v_total_registros=' '
            LET v_signo_porcentaje='%'
      END CASE
      
      PRINTX v_tipo_tabla,v_tipo_registros_sin,v_tipo_registros_con,v_total_sin_string,v_total_con_string,v_total_porcentaje_sin_string,v_total_porcentaje_con_string,v_total_registros_ssv_cre_string,v_total_registros,v_signo_porcentaje
   
   AFTER GROUP OF v_tipo_arreglo
      PRINTX v_causal_sin_distincion_reporte.v_causal
      PRINTX v_causal_sin_distincion_reporte.v_cuenta
      PRINTX v_porcentaje
      PRINTX v_total_causales
      --DISPLAY v_porcentaje
       
       
       
   ON EVERY ROW
      PRINTX v_causal_sin_distincion_reporte.v_causal
      PRINTX v_causal_sin_distincion_reporte.v_cuenta
      PRINTX v_porcentaje
      PRINTX v_total_causales
      --DISPLAY v_porcentaje
   
   ON LAST ROW
      PRINTX v_total_causales
       --SKIP TO TOP OF PAGE
   
END REPORT
