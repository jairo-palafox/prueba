--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/Mar/2016
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => DAE                                                           #
#Programa     => DAEL40                                                        #
#Objetivo     => Lanzador de validación del archivo de Ajuste Individual       #
#                Amortizaciones Excedentes por Captura Manual                  #
#Fecha inicio => 11/Mar/2016                                                   #
################################################################################

DATABASE safre_viv
GLOBALS "DAEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion

DEFINE g_reg_modulo   RECORD
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
DEFINE arr_detalles DYNAMIC ARRAY OF RECORD 
          v_id_dae_referencia DECIMAL(9,0),
          v_num_credito       CHAR(10),
          v_fecha_pago        DATE,
          v_periodo_pago      CHAR(4),
          v_registro_pago     CHAR(8),
          v_imp_amortizacion  DECIMAL(16,6),
          v_nss               CHAR(11),
          v_ajustar           SMALLINT
END RECORD

DEFINE arr_seleccion DYNAMIC ARRAY OF RECORD
          v_id_dae_referencia DECIMAL(9,0),
          v_num_credito       CHAR(10),
          v_fecha_pago        DATE,
          v_periodo_pago      CHAR(4),
          v_registro_pago     CHAR(8),
          v_imp_amortizacion  DECIMAL(16,6),
          v_nss               CHAR(11)
END RECORD

DEFINE arr_datos_generales RECORD 
          v_id_derechohabiente DECIMAL(9,0),
          v_nss                CHAR(11),
          v_curp               CHAR(18),
          v_rfc                CHAR(13),
          v_nombre_af          CHAR(40),
          v_ap_paterno_af      CHAR(40),
          v_ap_materno_af      CHAR(40),
          v_num_credito        CHAR(10)
END RECORD
END GLOBALS

MAIN
DEFINE p_usuario            LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion     SMALLINT,
       p_titulo             STRING,
       r_bnd_fin_oper       SMALLINT,
       r_bnd_valida_op      SMALLINT,
       v_mensaje            STRING,
       v_num_credito        CHAR(10),
       v_fecha_pago         DATE,
       v_periodo_pago       CHAR(4),
       v_registro_pago      CHAR(8),
       v_imp_amortizacion   DECIMAL(16,6),
       v_nss                CHAR(11),
       v_is                 INTEGER,
       v_id_derechohabiente DECIMAL(9,0),
       v_tot_regs           INTEGER

DEFINE w ui.Window
DEFINE f ui.Form   
       
   -- se recupera la clave de usuario desde parametro 
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   LET r_bnd_fin_oper = 0
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo   IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo  )
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2407 --Ajuste Amortización Excedente Individual
   LET g_opera_cod   = 1    --Validación de archivo

   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'dae'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
   
   OPEN WINDOW v_seleccion WITH FORM "DAEL400"
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
DIALOG ATTRIBUTES(UNBUFFERED) 
      --Permite captura de los críterios de búsqueda
      INPUT BY NAME v_nss
      {v_num_credito,
                    v_fecha_pago,
                    v_periodo_pago,
                    v_registro_pago,
                    v_imp_amortizacion}

      END INPUT 

   BEFORE DIALOG         
      CALL f.setElementHidden("gr_detalles", 1)
      CALL f.setElementHidden("gr_datos_generales", 1)

      --LET v_fecha_pago = TODAY
      --LET v_registro_pago = TODAY 
      
      ON ACTION ACCEPT
         --Valida que el NSS sea un registro valido
         IF v_nss
            {v_num_credito    IS NULL OR 
            v_fecha_pago     IS NULL OR
            v_periodo_pago   IS NULL OR
            v_registro_pago  IS NULL OR
            v_imp_amortizacion} IS NULL THEN

            CALL fn_mensaje("Atencion", "El NSS es obligatorio", "stop")
            CLEAR FORM 
            NEXT FIELD v_nss
         ELSE
            SELECT id_derechohabiente 
            INTO   v_id_derechohabiente  
            FROM   afi_derechohabiente 
            WHERE  nss = v_nss

            CALL fn_datos_generales (v_id_derechohabiente)
                 RETURNING v_tot_regs

            IF v_tot_regs > 1 THEN 
               CALL f.setElementHidden("gr_datos_generales", 0)
               CALL f.setElementHidden("gr_detalles", 0)
               CALL fn_consulta_detalles (v_num_credito,
                                          v_fecha_pago,
                                          v_periodo_pago,
                                          v_registro_pago,
                                          v_imp_amortizacion,
                                          v_id_derechohabiente)
            END IF

      --Crea tabla temporal para preliquidar
      DATABASE safre_tmp

      DROP TABLE IF EXISTS tmp_dae_preliquida;
      
      CREATE TABLE tmp_dae_preliquida
      (id_dae_referencia DECIMAL(9,0),
       num_credito       CHAR(10),
       fecha_pago        DATE,
       periodo_pago      CHAR(4),
       registro_pago     CHAR(8),
       imp_amortizacion  DECIMAL(16,6),
       nss               CHAR(11)
       )
       ---

       DATABASE safre_viv
         INPUT ARRAY arr_detalles WITHOUT DEFAULTS 
         FROM scr_detalles.*
         ATTRIBUTES  (INSERT ROW = FALSE, 
                      APPEND ROW = FALSE, 
                      DELETE ROW = FALSE,
                      CANCEL = FALSE)

            ON CHANGE v_ajustar
               LET v_is = v_is + 1;

               LET arr_seleccion[v_is].v_id_dae_referencia = arr_detalles[ARR_CURR()].v_id_dae_referencia
               LET arr_seleccion[v_is].v_num_credito       = arr_detalles[ARR_CURR()].v_num_credito     
               LET arr_seleccion[v_is].v_fecha_pago        = arr_detalles[ARR_CURR()].v_fecha_pago      
               LET arr_seleccion[v_is].v_periodo_pago      = arr_detalles[ARR_CURR()].v_periodo_pago    
               LET arr_seleccion[v_is].v_registro_pago     = arr_detalles[ARR_CURR()].v_registro_pago   
               LET arr_seleccion[v_is].v_imp_amortizacion  = arr_detalles[ARR_CURR()].v_imp_amortizacion
               LET arr_seleccion[v_is].v_nss               = arr_detalles[ARR_CURR()].v_nss             
               
               IF arr_detalles[ARR_CURR()].v_ajustar = 1 THEN
                  INSERT INTO safre_tmp:tmp_dae_preliquida
                         VALUES (arr_seleccion[v_is].v_id_dae_referencia,
                                 arr_seleccion[v_is].v_num_credito     ,
                                 arr_seleccion[v_is].v_fecha_pago      ,  
                                 arr_seleccion[v_is].v_periodo_pago    ,  
                                 arr_seleccion[v_is].v_registro_pago   ,  
                                 arr_seleccion[v_is].v_imp_amortizacion,
                                 arr_seleccion[v_is].v_nss )  

               ELSE
                  DELETE FROM safre_tmp:tmp_dae_preliquida
                  WHERE num_credito      = arr_seleccion[v_is].v_num_credito
                  AND   fecha_pago       = arr_seleccion[v_is].v_fecha_pago
                  AND   periodo_pago     = arr_seleccion[v_is].v_periodo_pago
                  AND   registro_pago    = arr_seleccion[v_is].v_registro_pago
                  AND   imp_amortizacion = arr_seleccion[v_is].v_imp_amortizacion
                  AND   nss              = arr_seleccion[v_is].v_nss
               END IF

           ON ACTION ACCEPT
                 CALL arr_seleccion.clear()
                 --Si no se selecciona ningun registro no se debe liquidar nada
                 IF v_is = 0 THEN 
                    CALL fn_mensaje ("Atención", "Debe seleccionar al menos un registro", "stop");
                 ELSE                   
                    --Ejecuta la función de INTEGRACIÓN
                    CALL fn_integra_ajuste(p_usuario)
                    LET v_nss = NULL
                    DISPLAY BY NAME v_nss  
                    CALL f.setElementHidden("gr_detalles",1)
                    CALL f.setElementHidden("gr_datos_generales",1)
                    EXIT INPUT
                 END IF 

              ON ACTION regresar
                  CALL f.setElementHidden("gr_detalles",1)
                  CALL f.setElementHidden("gr_datos_generales",1)
                  EXIT INPUT
            END INPUT 
         END IF 

      ON ACTION cancelar 
         EXIT DIALOG 

END DIALOG
   CLOSE WINDOW v_seleccion 
END MAIN

#OBJETIVO: Recuperar datos generales del derechohabiente
FUNCTION fn_datos_generales (p_id_derechohabiente)
DEFINE p_nss                CHAR(11),
       p_id_derechohabiente DECIMAL(9,0),
       QryTxt               STRING,
       i                    INTEGER

   LET QryTxt = "\n SELECT a.id_derechohabiente, ",
                "\n        a.nss               , ",
                "\n        a.curp              , ",
                "\n        a.rfc               , ",
                "\n        a.nombre_af         , ",
                "\n        a.ap_paterno_af     , ",
                "\n        a.ap_materno_af     , ",
                "\n        b.num_credito         ",
                "\n FROM   afi_derechohabiente a,", 
                "\n        dae_det_solicitud b   ",
                "\n WHERE  a.id_derechohabiente = b.id_derechohabiente ", 
                "\n AND    a.id_derechohabiente = ", p_id_derechohabiente,
                "\n GROUP BY 1,2,3,4,5,6,7,8 "
--DISPLAY QryTxt
   PREPARE prp_datos_generales FROM QryTxt
   DECLARE cur_datos_generales CURSOR FOR prp_datos_generales

   LET i = 1; 
   
   FOREACH cur_datos_generales INTO arr_datos_generales.* 
      LET i = i+1;
   END FOREACH

   IF i > 1 THEN
      DISPLAY arr_datos_generales.v_ap_paterno_af TO v_ap_paterno
      DISPLAY arr_datos_generales.v_ap_materno_af TO v_ap_materno
      DISPLAY arr_datos_generales.v_nombre_af     TO v_nombre
      DISPLAY arr_datos_generales.v_rfc           TO v_rfc
      DISPLAY arr_datos_generales.v_curp          TO v_curp
      DISPLAY arr_datos_generales.v_num_credito   TO v_no_credito
   ELSE 
      CALL fn_mensaje ("Atención", "No se encontraron registros con los críterios de búsqueda", "stop")
   END IF

   RETURN i

END FUNCTION



#OBJETIVO: Recuperar las solicitudes de devolución de amortizaciones excedentes 
FUNCTION fn_consulta_detalles (p_num_credito,
                               p_fecha_pago,
                               p_periodo_pago,
                               p_registro_pago,
                               p_imp_amortizacion,
                               p_id_derechohabiente)

DEFINE p_num_credito        CHAR(10),
       p_fecha_pago         DATE,
       p_periodo_pago       CHAR(4),
       p_registro_pago      CHAR(8),
       p_imp_amortizacion   DECIMAL(16,6),
       p_nss                CHAR(11),
       p_id_derechohabiente DECIMAL (9,0),
       QryTxt               STRING,
       i                    INTEGER

   LET QryTxt = "\n SELECT id_dae_referencia, ",
                "\n        num_credito, ",
                "\n        fecha_pago, ",
                "\n        periodo_pago, ",
                "\n        registro_pago, ",
                "\n        importe_amort, ",
                "\n        nss ",
                "\n FROM   dae_det_solicitud ",
                --"\n WHERE  nss = '",p_nss,"'",
                "\n WHERE  id_derechohabiente = ",p_id_derechohabiente,
                {--num_credito = '",  p_num_credito, "'",
                "\n        AND    fecha_pago = '",   p_fecha_pago, "'",
                "\n        AND    periodo_pago = '", p_periodo_pago, "'",
                "\n        AND    registro_pago = '",p_registro_pago, "'",
                "\n        AND    importe_amort = ",p_imp_amortizacion,}
                "\n AND    resul_opera = '01' ",
                "\n AND    folio_liquida IS NOT NULL ",
                "\n AND    folio_ajuste IS NULL ",
                "\n AND    (status_retiro = 1 ",
                "\n OR      status_retiro = 3) "
--DISPLAY QryTxt
   PREPARE prp_detalles FROM QryTxt
   DECLARE cur_detalles CURSOR FOR prp_detalles

   LET i = 1; 
   
   FOREACH cur_detalles INTO arr_detalles[i].* 
      LET i = i+1;
   END FOREACH

   IF i > 1 THEN
      CALL arr_detalles.deleteElement(i)
   ELSE 
      CALL fn_mensaje ("Atención", "No se encontraron registros con los críterios de búsqueda", "stop")
   END IF

END FUNCTION 

#OBJETIVO: Invoca el programa que se encarga de ejecutar la preliquidación
FUNCTION fn_integra_ajuste(p_usuario_cod)
DEFINE p_usuario_cod    CHAR(20),
       p_folio          DECIMAL(9,0),
       v_s_comando      STRING,
       v_nombre_archivo CHAR(40),
       r_bnd_fin_oper   SMALLINT,
       r_bdn_valida_op  SMALLINT,
       r_bnd_inicializa SMALLINT,
       r_bnd_oper_ini   SMALLINT,
       v_mensaje        STRING,
       v_estado_cod     SMALLINT

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"

   CALL fn_genera_pid(g_proceso_cod,g_opera_cod,p_usuario_cod)
   RETURNING g_pid
   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) 
   RETURNING r_bdn_valida_op 

   IF ( r_bdn_valida_op = 0 ) THEN
      CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,p_folio,"DAEL40",v_nombre_archivo,p_usuario_cod)
      RETURNING r_bnd_inicializa

      IF r_bnd_inicializa = 0 THEN 
   	     -- Inicio operacion.
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0 ,"DAEL40","",p_usuario_cod)
              RETURNING r_bnd_oper_ini

         IF (r_bnd_oper_ini = 0) THEN
            LET v_s_comando = " fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/DAEP11 ",
                                p_usuario_cod CLIPPED, " ",
                                g_pid  , " " ,
                                g_proceso_cod , " " ,
                                g_opera_cod ," ",
                                0 ," ",
                                v_nombre_archivo CLIPPED," ",
                                " 1>",seg_modulo_bat.ruta_listados clipped ,
                                "/nohup:",g_pid        USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod   USING "&&&&&" ,
                                " 2>&1 &"

            DISPLAY v_s_comando
            RUN v_s_comando

            DISPLAY "Atención, Se ha enviado la integración del proceso "
            DISPLAY "Ajuste Manual de Devolución de Amortizaciones Excedentes"
            DISPLAY "Puede revisar el avance en el monitor de procesos."

            CALL fn_mensaje ("Atención", 
                             "Se integrarán los registros seleccionados \n Puede revisar el avance en el monitor de procesos", 
                             "info");  
         ELSE
            CALL fn_mensaje ("Atención", fn_recupera_inconsis_opera(r_bnd_oper_ini),"stop");
         END IF --Opera_ini
      ELSE 
         CALL fn_mensaje ("Atención", fn_recupera_inconsis_opera(r_bnd_inicializa),"stop");
      END IF --Inicializa proceso
   ELSE
      CALL fn_mensaje ("Atención", fn_recupera_inconsis_opera(r_bdn_valida_op),"stop");
   END IF --Valida operacion 
END FUNCTION