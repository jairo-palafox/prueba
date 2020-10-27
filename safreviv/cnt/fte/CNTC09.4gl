################################################################################
# VERSION: 1.0.0                                                               #
# FECHA ULTIMA MODIFICACION:04/07/2012                                        #
################################################################################
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#MODULO            => CNT                                                      #
#PROGRAMA          => CNTC09                                                   #
#OBJETIVO          => PROGRAMA DE CONSULTA DE GENERACIÓN DE OPERACIONES        #
#FECHA INICIO      => 29/06/2012                                               #
################################################################################
DATABASE safre_viv
GLOBALS "CNTG01.4gl"
GLOBALS
   DEFINE g_arr_cta_importe DYNAMIC ARRAY OF RECORD
         v_proceso VARCHAR(60),
         v_apo_con DECIMAL(22,2),
         v_amo_con DECIMAL(22,2),
         v_apo_ope DECIMAL(22,2),
         v_amo_ope DECIMAL(22,2)
   END RECORD

   DEFINE g_indice INTEGER
   DEFINE g_indice2 INTEGER

   DEFINE 
      f_fecha_corte DATE
END GLOBALS

MAIN

   DEFINE 
       f_ventana     ui.Window,   -- Define las propìedades de la Ventana
       f_forma       ui.Form

   -- se asignan los parametros que vienen del fglrun
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog     = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW vtn_cntc09 WITH FORM "CNTC091"
      DIALOG ATTRIBUTES(UNBUFFERED)
         INPUT v_cmb_sub_cta,f_fecha_corte
          FROM v_cmb_sub_cta,f_fecha_corte

         BEFORE INPUT
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()
            CALL f_forma.setElementHidden("grp_res_ope",1)
            
            CALL fn_cmb_subcta(" NOT IN (41,43) ")--Llena combo de subcuentas

         ON ACTION cancelar
            EXIT DIALOG
         

         ON ACTION consultar
            
            --Valida parametros de entrada sean obligatorios         
            IF LENGTH(v_cmb_sub_cta) = 0 OR LENGTH(f_fecha_corte) = 0 THEN
               CALL fn_mensaje("ATENCIÓN",
                               "Los dos campos son obligatorios",
                               "about")
               NEXT FIELD v_cmb_sub_cta
            END IF
            --Valida que la fecha de corte no sea mayor a la fecha del sistema
            IF f_fecha_corte > TODAY THEN
               CALL fn_mensaje("ATENCIÓN","Fecha improcedente","about")
               NEXT FIELD f_fecha_corte
            END IF
            --Valida que la fecha de corte exista en la tabla modulo de periodo
            IF (fn_valida_fecha_corte(f_fecha_corte) = 0) THEN
               CALL fn_mensaje("ATENCIÓN","Fecha de corte no existe","about")
               NEXT FIELD f_fecha_corte
            END IF

            CALL f_forma.setElementHidden("grp_res_ope",FALSE )
            --Función para obtener montos
            CALL fn_obtine_montos(v_cmb_sub_cta,f_fecha_corte)
            

            --Despliega en pantalla la información consultada
            DISPLAY ARRAY g_arr_cta_importe TO scr_operacion.* ATTRIBUTES (ACCEPT = FALSE ) 
               ON ACTION CANCEL  
                  EXIT DISPLAY 
               ON ACTION reporte 
               --Función para generar reporte
               CALL fn_genera_reporte()

               ON ACTION archivo
                  CALL fn_genera_archivo_resumen()
            
            END DISPLAY

         END INPUT

      END DIALOG 
   CLOSE WINDOW vtn_cntc09
END MAIN
#Objetivo:Función que valida que la fecha de corte capturada exista
FUNCTION fn_valida_fecha_corte(p_fecha_corte)
DEFINE p_fecha_corte DATE
DEFINE r_valida_fecha_corte SMALLINT

   LET r_valida_fecha_corte = 0

   SELECT COUNT(*) INTO r_valida_fecha_corte
   FROM cbd_modulo_periodo
   WHERE f_saldo = p_fecha_corte

   RETURN r_valida_fecha_corte
END FUNCTION
#Objetivo:Función que obtiene los montos de "Partidas Confirmadas" y "Partidas 
#         en Conciliación Operativa" o "Movimientos Adelantados del Periódo"
FUNCTION fn_obtine_montos(p_subcuenta_apo,p_fecha_corte)
DEFINE p_subcuenta_apo SMALLINT
DEFINE r_subcuenta_amo SMALLINT
DEFINE p_fecha_corte DATE
DEFINE v_qry_txt STRING
--Movimientos del periodo
DEFINE v_per_modulo CHAR(03)
DEFINE v_per_desc_modulo VARCHAR(60)
DEFINE v_per_monto_apo DECIMAL(22,2)
DEFINE v_per_monto_amo DECIMAL(22,2)
--Movimientos adelantados del periodo
DEFINE v_ade_desc_modulo VARCHAR(60)
DEFINE v_ade_monto_apo DECIMAL(22,2)
DEFINE v_ade_monto_amo DECIMAL(22,2)

   LET v_per_monto_apo = 0.00
   LET v_per_monto_amo = 0.00
   LET v_ade_monto_apo = 0.00
   LET v_ade_monto_amo = 0.00
   LET g_indice = 1
   LET g_indice2 = 1

   --Obtiene subcuenta de amortizaciones segun subcuenta aportacion
   CALL fn_obtiene_scta_amo(p_subcuenta_apo) RETURNING r_subcuenta_amo
   -----------------------------------------------------------------------------
   LET v_qry_txt = "\n SELECT apo.modulo,apo.modulo||'-'||mdl.modulo_desc,",
                   "\n        SUM(apo.monto_pesos) apo,",
                   "\n        SUM(amo.monto_pesos) amo",
                   "\n FROM cbd_modulo_periodo apo,",
                   "\n      outer cbd_modulo_periodo amo,",
                   "\n      outer seg_modulo mdl",
                   "\n WHERE apo.f_saldo   = '",p_fecha_corte,"'",
                   "\n   AND apo.f_saldo   = amo.f_saldo",
                   "\n   AND apo.subcuenta = ",p_subcuenta_apo,
                   "\n   AND amo.subcuenta = ",r_subcuenta_amo,
                   "\n   AND apo.modulo    = amo.modulo",
                   "\n   AND apo.modulo    = mdl.modulo_cod",
                   "\n GROUP BY 1,2"
   DISPLAY "Movimientnos del periodo \n",v_qry_txt
   PREPARE prp_mov_periodo FROM v_qry_txt
   -----------------------------------------------------------------------------
   LET v_qry_txt = "\n SELECT apo.modulo||'-'||mdl.modulo_desc,",
                   "\n        SUM(apo.monto_pesos) apo,",
                   "\n        SUM(amo.monto_pesos) amo",
                   "\n FROM cbd_modulo_adelanto apo,",
                   "\n      outer cbd_modulo_adelanto amo,",
                   "\n      outer seg_modulo mdl",
                   "\n WHERE apo.f_saldo   = '",p_fecha_corte,"'",
                   "\n   AND apo.f_saldo   = amo.f_saldo",
                   "\n   AND apo.subcuenta = ",p_subcuenta_apo,
                   "\n   AND amo.subcuenta = ",r_subcuenta_amo,
                   "\n   AND apo.modulo    = ?",
                   "\n   AND apo.modulo    = amo.modulo",
                   "\n   AND apo.modulo    = mdl.modulo_cod",
                   "\n GROUP BY 1"

                  
   DISPLAY "Movimientnos Adelantados del periodo \n",v_qry_txt
   
   PREPARE prp_mov_ade_pariodo FROM v_qry_txt
   -----------------------------------------------------------------------------

   DECLARE cur_mov_periodo CURSOR FOR prp_mov_periodo
      FOREACH cur_mov_periodo INTO v_per_modulo,
                                   v_per_desc_modulo,
                                   v_per_monto_apo,
                                   v_per_monto_amo
                                   
         DISPLAY "v_per_modulo ",v_per_modulo

               IF v_per_monto_apo IS NULL THEN
                  LET v_per_monto_apo = 0.00
               END IF 

               IF v_per_monto_amo IS NULL THEN
                  LET v_per_monto_amo = 0.00
               END IF

                                   
         DECLARE cur_mov_adelantado CURSOR FOR prp_mov_ade_pariodo
            FOREACH cur_mov_adelantado  USING v_per_modulo
                                        INTO v_ade_desc_modulo,
                                             v_ade_monto_apo,
                                             v_ade_monto_amo

               IF v_ade_monto_apo IS NULL THEN
                  LET v_ade_monto_apo = 0.00
               END IF 

               IF v_ade_monto_amo IS NULL THEN
                  LET v_ade_monto_amo = 0.00
               END IF 

               LET g_arr_cta_importe[g_indice].v_proceso = v_per_desc_modulo
               LET g_arr_cta_importe[g_indice].v_apo_con = v_per_monto_apo - v_ade_monto_apo
               LET g_arr_cta_importe[g_indice].v_amo_con = v_per_monto_amo - v_ade_monto_amo
               LET g_arr_cta_importe[g_indice].v_apo_ope = v_per_monto_apo
               LET g_arr_cta_importe[g_indice].v_amo_ope = v_per_monto_amo

               LET g_indice = g_indice + 1

            END FOREACH

            LET g_indice2 =  g_indice2 + 1
      END FOREACH

      CALL g_arr_cta_importe.deleteElement(g_indice) --Elimina registro en blanco

END FUNCTION
#objetico: Función para obtener la subcuenta de amortización
FUNCTION fn_obtiene_scta_amo(p_scta_apo)
DEFINE p_scta_apo SMALLINT
DEFINE r_scta_amo SMALLINT

   CASE
      -- Amortización
      WHEN p_scta_apo = 4  OR p_scta_apo = 8  OR
           p_scta_apo = 40 OR p_scta_apo = 48 OR
           p_scta_apo = 49
         LET r_scta_amo = 41
      -- Amortización solo INFONAVIT
      WHEN p_scta_apo = 42 OR p_scta_apo = 44
         LET r_scta_amo = 43
   END CASE

   RETURN r_scta_amo
END FUNCTION

--g_arr_cta_importe
--Genera un archivo de salida en texto plano con la información del resumen de operaciones
FUNCTION fn_genera_archivo_resumen()

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
      v_desc_scta          VARCHAR (40)

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

   LET v_nom_archivo = "/resumen_operaciones_", v_hora

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
   --v_cmb_sub_cta,f_fecha_corte
   IF v_cmb_sub_cta IS NOT NULL THEN 

      SELECT subcuenta_desc
        INTO v_desc_scta
      FROM cat_subcuenta
      WHERE subcuenta = v_cmb_sub_cta
      
      LET v_encabezado = "Subcuenta: ",v_cmb_sub_cta,"-",v_desc_scta
      CALL v_ch_arch_salida.write([v_encabezado])
   END IF 

   IF f_fecha_corte IS NOT NULL THEN 
      LET v_encabezado = "Fecha de corte: ",f_fecha_corte USING "dd-mm-yyyy"
      CALL v_ch_arch_salida.write([v_encabezado])
   END IF 

   
   LET v_encabezado = " |PARTIDAS CONFIRMADAS| |PARTIDAS EN CONCILIACIÓN OPERATIVA| |"
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = "Procesos |Aportaciones|Amortizaciones |Aportaciones|Amortizaciones |"
   CALL v_ch_arch_salida.write([v_encabezado])
   
   FOR v_recorre_arreglo = 1 TO g_arr_cta_importe.getLength()
      LET v_detalle = g_arr_cta_importe[v_recorre_arreglo].v_proceso, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_apo_con, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_apo_ope, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_amo_con, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_amo_ope, "|"
    
      CALL v_ch_arch_salida.write([v_detalle])

   END FOR

   CALL v_ch_arch_salida.close()
   
   --Cambia el formato del archivo a DOS
   LET v_comando_dos = "unix2dos ",v_ruta_envio_cnt CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   CALL fn_mensaje("Información","Se ha generado el archivo de Resumen de Operaciones\n en la ruta"||v_ruta_nomarch,"information")
   

END FUNCTION 


#Objetivo: Función para generar el reporte del resumen de operaciones
FUNCTION fn_genera_reporte()
DEFINE manejador_rpt  om.SaxDocumentHandler  -- Contenedor documentos reporte
DEFINE v_ini_rep INTEGER

   -- Botón para generar el reporte en PDF de la consulta
   IF fgl_report_loadCurrentSettings("CNTC092.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   END IF

   --Inicia el reporte de registros con rechazo
   START REPORT rpt_cntc092 TO XML HANDLER manejador_rpt
   FOR v_ini_rep= 1 TO g_arr_cta_importe.getLength()
      OUTPUT TO REPORT rpt_cntc092(g_arr_cta_importe[v_ini_rep].*,
                                   g_usuario)
      END FOR

   FINISH REPORT rpt_cntc092

END FUNCTION
#Objetico: Genera la estructura para el reporte de resumend e operaciones
REPORT rpt_cntc092(p_rcd_cta_importe,p_usuario)
DEFINE p_rcd_cta_importe RECORD
         v_proceso VARCHAR(60),
         v_apo_con DECIMAL(22,2),
         v_amo_con DECIMAL(22,2),
         v_apo_ope DECIMAL(22,2),
         v_amo_ope DECIMAL(22,2)
                   END RECORD
DEFINE p_usuario LIKE seg_usuario.usuario_cod
DEFINE p_fecha_reporte DATE

FORMAT
   FIRST PAGE HEADER
      LET p_fecha_reporte = TODAY 
      PRINTX p_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario

   ON EVERY ROW
      PRINTX p_rcd_cta_importe.v_proceso
      PRINTX p_rcd_cta_importe.v_apo_con
      PRINTX p_rcd_cta_importe.v_amo_con
      PRINTX p_rcd_cta_importe.v_apo_ope
      PRINTX p_rcd_cta_importe.v_amo_ope
END REPORT
