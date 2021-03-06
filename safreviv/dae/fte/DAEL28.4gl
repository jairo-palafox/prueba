--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22/12/2015
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
#Modulo            => DAE                                                      #
#Programa          => DAEL28                                                   #
#Objetivo          => Lanzador para el extractor de detalle de pendientes      #
#Fecha inicio      => 22/12/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       ventana ui.Window,
       forma   ui.Form,
       g_seg_modulo   RECORD
          ruta_exp      CHAR(40),
          ruta_rescate  CHAR(40),
          ruta_listados CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
          ruta_listados CHAR(40)
       END RECORD

DEFINE v_num_credito      CHAR(10),
       v_fecha_pago       DATE,
       v_periodo_pago     CHAR(4),
       v_fec_reg_pag      DATE,
       v_delegacion       CHAR(2),
       v_imp_amortizacion DECIMAL(16,6),
       v_tipo_pago        CHAR(3),
       v_nss              CHAR(11),
       v_ent_receptora    CHAR(3),
       v_folio_liquida    DECIMAL(9,0),
       v_fec_ing_saci     DATE,
       p_usuario_cod      LIKE seg_usuario.usuario_cod
END GLOBALS

MAIN
DEFINE v_condicion      STRING,
       p_tipo_ejecucion SMALLINT,
       p_s_titulo       STRING

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   LET g_proceso_cod = 2404
   LET g_opera_cod   = 1  
   
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
     
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_seg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'dae'
      
   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   CLOSE WINDOW SCREEN
   OPEN WINDOW w_captura WITH FORM "DAEL280.4fd" 

      LET ventana = ui.Window.getCurrent()
      LET forma   = ventana.getForm()

      CONSTRUCT v_condicion ON num_credito,
                               fecha_pago,
                               periodo_pago,
                               fecha_pago,
                               delegacion,
                               importe_amort,
                               tipo_pago,
                               nss,
                               entidad_receptora,
                               folio_liquida,
                               fecha_liquida
      FROM v_num_credito,
           v_fecha_pago,
           v_periodo_pago,
           v_fec_reg_pag,
           v_delegacion,
           v_imp_amortizacion,
           v_tipo_pago,
           v_nss,
           v_ent_receptora,
           v_folio_liquida,
           v_fec_ing_saci
           
      BEFORE CONSTRUCT
         CALL forma.setElementHidden("grupo_detalles",1)
         CALL forma.setElementHidden("grupo_totales",1)
         CALL fn_inicia_valores()

      ON ACTION ACCEPT
         ACCEPT CONSTRUCT


      ON ACTION CANCEL
         LET INT_FLAG = 1
         EXIT CONSTRUCT
   END CONSTRUCT

   IF v_condicion = "1=1" THEN 
      CALL fn_mensaje("Atenci�n",
                      "Debe de ingresar alg�n campo de b�squeda.",
                      "about")
      RETURN 1
   ELSE 
      CALL fn_consulta_detalles(v_condicion, p_usuario_cod)
   END IF

   CLOSE WINDOW w_captura 

END MAIN 

#OBJETIVO: Inicializa valores 
FUNCTION fn_inicia_valores()
DEFINE v_QryTxt     STRING,
       v_cmbx       ui.ComboBox,
       i            INTEGER
DEFINE arr_folios DYNAMIC ARRAY OF RECORD
          v_folio_valor DECIMAL(9,0),
          v_folio       DECIMAL(9,0)
END RECORD

   LET v_fec_reg_pag      = ""
   LET v_fecha_pago       = ""
   LET v_fec_ing_saci     = ""

   LET v_cmbx = ui.ComboBox.forName("v_folio_liquida")
   IF v_cmbx IS NULL THEN
      ERROR "Campo no encontrado"
      EXIT PROGRAM
   END IF

   LET i = 1
   
   LET v_QryTxt = "\n SELECT folio_liquida",
                  "\n FROM   dae_det_solicitud",
                  "\n GROUP BY 1 ",
                  "\n ORDER BY 1 "

   PREPARE prp_folios_dpe FROM v_QryTxt
   
   CALL v_cmbx.clear()

   DECLARE cur_llena_combo_folio CURSOR FOR prp_folios_dpe
   FOREACH cur_llena_combo_folio INTO arr_folios[i].v_folio

      CALL v_cmbx.addItem(arr_folios[i].v_folio,
                          arr_folios[i].v_folio )

      LET i = i + 1
    END FOREACH

    CALL arr_folios.deleteElement(i)

END FUNCTION

#OBJETIVO: Consultar los datos para generar el reporte
FUNCTION fn_consulta_detalles(p_condicion, p_usuario_cod)
DEFINE p_condicion       STRING,
       v_QryTxt          STRING, 
       p_usuario_cod     CHAR(20),
       i                 INTEGER,
       j                 INTEGER,
       v_tot_monto_pesos DECIMAL(16,6),
       v_tot_monto_aivs  DECIMAL(16,6)

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD
          v_id_derechohabiente DECIMAL(9,0), 
          v_num_credito        CHAR(10),
          v_fecha_pago         DATE,
          v_periodo_pago       CHAR(4),
          v_fec_reg_pag        CHAR(8),
          v_delegacion         CHAR(2),
          v_imp_amortizacion   DECIMAL(16,6),
          v_monto_aivs         DECIMAL(16,6),
          v_tipo_pago          CHAR(3),
          v_nss                CHAR(11),
          v_ent_receptora      CHAR(3),
          v_folio_liquida      DECIMAL(9,0),
          v_fec_ing_saci       DATE
END RECORD

   LET v_QryTxt = "\n SELECT id_derechohabiente,", 
                  "\n        num_credito, ",
                  "\n        fecha_pago, ",
                  "\n        periodo_pago, ",
                  "\n        registro_pago, ",
                  "\n        delegacion, ",
                  "\n        importe_amort, ",
                  "\n        monto_aivs, ",
                  "\n        tipo_pago, ",
                  "\n        nss, ",
                  "\n        entidad_receptora,",
                  "\n        folio_liquida, ",
                  "\n        fecha_liquida ",
                  "\n FROM   dae_det_solicitud ",
                  "\n WHERE  resul_opera = '01' ",
                  "\n AND    estado IN (4,5) ",
                  "\n AND    status_retiro = 1 ",
                  "\n AND    ",p_condicion,
                  "\n ORDER BY 10,2 "

      PREPARE prp_detalles FROM v_QryTxt
      DECLARE cur_detalles CURSOR FOR prp_detalles 

      LET i = 1

      LET v_tot_monto_pesos = 0;
      LET v_tot_monto_aivs  = 0;
    
      FOREACH cur_detalles INTO arr_detalles[i].v_id_derechohabiente,
                                arr_detalles[i].v_num_credito,
                                arr_detalles[i].v_fecha_pago,
                                arr_detalles[i].v_periodo_pago,
                                arr_detalles[i].v_fec_reg_pag,
                                arr_detalles[i].v_delegacion,
                                arr_detalles[i].v_imp_amortizacion ,
                                arr_detalles[i].v_monto_aivs,
                                arr_detalles[i].v_tipo_pago,
                                arr_detalles[i].v_nss,
                                arr_detalles[i].v_ent_receptora,
                                arr_detalles[i].v_folio_liquida,
                                arr_detalles[i].v_fec_ing_saci

         IF arr_detalles[i].v_monto_aivs IS NOT NULL THEN 
            LET v_tot_monto_aivs  = v_tot_monto_aivs + arr_detalles[i].v_monto_aivs
         END IF 

         LET v_tot_monto_pesos = v_tot_monto_pesos + arr_detalles[i].v_imp_amortizacion

         LET i = i+1;
      END FOREACH

      LET i = i - 1;

      IF i >= 1 THEN
         DISPLAY i TO v_tot_regs
         DISPLAY v_tot_monto_pesos TO v_tot_pesos
         DISPLAY v_tot_monto_aivs TO v_tot_aivs
      
         CALL forma.setElementHidden("grupo_detalles",0)
         CALL forma.setElementHidden("grupo_totales",0)  
      ELSE
         CALL fn_mensaje ("Atenci�n",
                          "No existe informaci�n con los registros proporcionados",
                          "stop")
      END IF

      DISPLAY ARRAY arr_detalles TO scr_detalles.*

         ON ACTION reporte
            CALL fn_dae_generacion_archivo(p_usuario_cod, p_condicion)

         ON ACTION CANCEL
            EXIT DISPLAY
            --DELETE FROM safre_tmp:dae_tmp_detalles_extractor; 
      END DISPLAY

END FUNCTION

#OBJETIVO: Ejecutar la generaci�n del archivo de salida de detalles.
FUNCTION fn_dae_generacion_archivo(p_usuario_cod, p_condicion)
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio            LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando        STRING,
       r_bnd_fin_oper     SMALLINT,
       v_mensaje          STRING,
       v_inicia_proceso   SMALLINT,
       v_valida_operacion SMALLINT, 
       v_folio_archivo    DECIMAL(9,0),
       p_condicion        STRING

   --Valida que se pueda ejecutar la operaci�n.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING v_valida_operacion
--DISPLAY "PRIMERA v_valida_operacion  :: ", v_valida_operacion
   LET v_valida_operacion = 0
   -- se obtiene el ID del proceso
   CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)
   RETURNING g_pid
--DISPLAY "Genera PID :: ", g_pid
   --Inicia proceso
   CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"DAEL28","",p_usuario_cod) 
   RETURNING v_inicia_proceso
--DISPLAY "Inicia Proceso :: ", v_inicia_proceso
   IF ( v_inicia_proceso = 0)THEN
      -- se verifica si se puede continuar con la operacion
      IF ( v_valida_operacion = 0 ) THEN
         CALL fn_genera_folio_dis(g_proceso_cod, g_opera_cod, p_folio, p_usuario_cod)
         RETURNING v_folio_archivo 
--DISPLAY "Genera folio :: ", v_folio_archivo
      	  -- Inicio operacion.
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,v_folio_archivo ,"DAEL28","",p_usuario_cod)
         RETURNING r_bnd_fin_oper
DISPLAY "Actualiza opera_ini :: ", r_bnd_fin_oper
            IF (r_bnd_fin_oper = 0) THEN
      
               LET v_s_comando = " nohup time fglrun ",g_seg_modulo.ruta_exp CLIPPED,"/DAES04 ",
                                   p_usuario_cod CLIPPED, " ",
                                   g_pid  , " " ,
                                   g_proceso_cod , " " ,
                                   g_opera_cod ," ",
                                   0," ",
                                   "NA"," ",
                                   p_condicion," ",
                                   " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                   "/nohup:",g_pid USING "&&&&&",":",
                                   g_proceso_cod USING "&&&&&",":",
                                   g_opera_cod   USING "&&&&&" ,
                                   " 2>&1 &"

               DISPLAY v_s_comando
               RUN v_s_comando

               CALL fn_mensaje("Atenci�n","Se ha enviado la generaci�n de archivo.\n"||
                               "Puede revisar el avance del proceso en el monitor de "||
                                "ejecuci�n de procesos","information")
            ELSE
               CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje      
               CALL fn_mensaje("Atenci�n", v_mensaje, "stop")
            END IF
      ELSE
         CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje      
         CALL fn_mensaje("Atenci�n", v_mensaje, "stop")
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(v_inicia_proceso) RETURNING v_mensaje
   END IF 
END FUNCTION
