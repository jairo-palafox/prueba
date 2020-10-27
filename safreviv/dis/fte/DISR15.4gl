################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 30/09/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR15                                                        #
#Objetivo     => Programa que ejecuta el reverso de la DISPERSIÓN DE LAS       #
#                APORTACIONES SUBSECUENTES PORTABILIDAD receptora.             # 
#                                                                              #
#Fecha inicio => 30/09/2015                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Codigo del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Codigo de operacion
    r_bnd_cnt                SMALLINT,
    r_tipo_rev_cnt           SMALLINT
END GLOBALS

MAIN
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, --Clave del usuario
    p_tipo_ejecucion         SMALLINT,     --Forma como ejecutara el programa
    p_s_titulo               STRING,       --Titulo de la ventana
    v_folio                  DECIMAL(9,0), --Folio liquidado
    v_fecha                  DATE,
    r_respuesta              SMALLINT
    
  DEFINE v_arr_prt    DYNAMIC ARRAY OF RECORD
    v_periodo_pago           CHAR(6),
    v_aportacion             DECIMAL(12,2),
    v_amortizacion           DECIMAL(12,2),
    v_aivs                   DECIMAL(22,2),
    v_total_cuentas          DECIMAL(9,0) 
  END RECORD,

    r_tot_aportacion         DECIMAL(12,2),
    r_tot_amortizacion       DECIMAL(12,2),
    r_tot_aivs               DECIMAL(22,2),
    r_tot_ctas               DECIMAL(12,0)

  DEFINE 
    f_ventana                ui.Window,   --Define las propìedades de la Ventana
    f_forma                  ui.Form,     --Define las propiedades de la forma
    v_bnd_val_rev            SMALLINT,    --Bandera valida reverso
    v_QryTxt                 STRING

  DEFINE 
    v_seg_modulo             RECORD LIKE seg_modulo.*

  DEFINE 
    seg_modulo_bat           RECORD LIKE seg_modulo.*

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  CALL STARTLOG(p_usuario_cod CLIPPED||".DISR15.log")

  LET v_fecha          = ""
  LET g_proceso_cod    = 931
  LET g_opera_cod      = 1
  LET r_tipo_rev_cnt   = 0

  SELECT *
  INTO   v_seg_modulo.*
  FROM   seg_modulo
  WHERE  modulo_cod = "dis" 

  SELECT *
  INTO   seg_modulo_bat.*
  FROM   seg_modulo
  WHERE  modulo_cod = "bat" 

  --Si se obtuvo el titulo, se pone como titulo de programa
  IF (p_s_titulo IS NOT NULL) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  CLOSE WINDOW SCREEN

  OPEN WINDOW w_rev_creditos_cero WITH FORM "DISR151"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio FROM f_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL f_forma.setElementHidden("grp_det_sub_cta", 1) --Oculta el detalle de los periodos
          CALL DIALOG.setActionHidden("reverso", 1)
      END INPUT

      DISPLAY ARRAY v_arr_prt TO svr_detalle.* 
      END DISPLAY

      ON ACTION cancelar
         EXIT DIALOG 
         
      ON ACTION ACCEPT
         IF v_fecha > TODAY THEN
            CALL fn_mensaje("Atención", "Fecha improcedente", "about")
            --NEXT FIELD f_fecha
         END IF
         LET v_fecha = TODAY

         IF length(v_folio) = 0 AND 
            length(v_fecha) = 0 THEN
            CALL fn_mensaje("Atención", "Debe capturar al menos un parámetro",
                            "about")
            NEXT FIELD f_folio
         END IF

         CALL fn_movimiento_proceso(v_folio, v_fecha)
         RETURNING v_folio

         DISPLAY "v_folio -- ", v_folio

         {IF r_tot_ctas = 0 THEN
            CALL DIALOG.setActionHidden("reverso", 1)
            CALL f_forma.setElementHidden("grp_det_sub_cta", 1) --Muestra el detalle de los periodos
            CALL DIALOG.setActionHidden("accept", 1)
         
            CALL fn_mensaje("Reverso Casos Excepción",
                            "Información no existe ", "about")
            LET v_folio = NULL 
            LET v_fecha = NULL 
            EXIT DIALOG
         END IF}

         IF v_folio > 0 THEN
            CALL DIALOG.setActionHidden("reverso", 0)
            CALL f_forma.setElementHidden("grp_det_sub_cta", 0) --Muestra el detalle de los periodos
            CALL DIALOG.setActionHidden("accept", 1)

            --Obtiene informacion de los casos de excepción a dispersar
            CALL fn_obtiene_informacion(v_folio, v_fecha)
            RETURNING v_arr_prt,
                      r_tot_aportacion,
                      r_tot_amortizacion,
                      r_tot_aivs,
                      r_tot_ctas,
                      v_fecha,
                      r_tipo_rev_cnt

            DISPLAY r_tot_aportacion   TO f_det_monto_apo  
            DISPLAY r_tot_amortizacion TO f_det_monto_amo 
            DISPLAY r_tot_aivs         TO f_det_monto_aivs
            DISPLAY r_tot_ctas         TO f_det_tot_ctas
            --DISPLAY v_fecha            TO f_fecha

            IF r_tipo_rev_cnt = 1 THEN
               CALL fn_mensaje("Reverso Portabilidad",
                               "Información ya generada en la interface ", "about")
               LET v_folio        = NULL 
               LET v_fecha        = NULL 
               LET r_tipo_rev_cnt = 0
               EXIT DIALOG
            END IF
            
            IF r_tot_ctas = 0 THEN
               CALL DIALOG.setActionHidden("reverso", 1)
               CALL f_forma.setElementHidden("grp_det_sub_cta", 1) --Muestra el detalle de los periodos
               CALL DIALOG.setActionHidden("accept", 1)
            
               CALL fn_mensaje("Reverso Casos Excepción",
                               "Información no existe ", "about")
               LET v_folio = NULL 
               LET v_fecha = NULL 
               EXIT DIALOG
            END IF
         ELSE
            CALL DIALOG.setActionHidden("reverso", 1)
            CALL f_forma.setElementHidden("grp_det_sub_cta", 1) --Muestra el detalle de los periodos
            --CALL DIALOG.setActionHidden("accept", 1)
         
            CALL fn_mensaje("Reverso Portabilidad",
                            "Información no existe ", "about")
            LET v_folio = NULL 
            LET v_fecha = NULL 
            NEXT FIELD f_folio
            --EXIT DIALOG
         END IF 
        
      ON ACTION reverso
         CALL fn_ventana_confirma("Portabilidad", 
                                  "¿Desea ejecutar el reverso?", 
                                  "quest") 
         RETURNING r_respuesta

         IF r_respuesta = 1 THEN
            --Se obtiene pid de la operación de acuerdo al folio
            SELECT DISTINCT pid 
            INTO   g_pid
            FROM   bat_ctr_operacion
            WHERE  folio = v_folio
            --Si no existe el pid de acuerdo al folio, traerá el último
            IF g_pid IS NULL OR g_pid = 0 THEN
               CALL fn_max_pid(g_proceso_cod, 1) RETURNING g_pid 
            END IF  
            
            CALL fn_valida_reverso(g_pid, g_proceso_cod, g_opera_cod)
            RETURNING v_bnd_val_rev

            DISPLAY "validación del reverso -- ", v_bnd_val_rev
                            
            IF v_bnd_val_rev = 0 THEN
               LET v_QryTxt = " nohup time fglrun ",v_seg_modulo.ruta_bin CLIPPED,"/DISR151.42r ",
                                p_usuario_cod CLIPPED, " ",
                                g_pid, " ",
                                g_proceso_cod, " ",
                                g_opera_cod, " ",
                                v_folio, " '",
                                "0" CLIPPED,"' ",
                                " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                "/nohup:", g_pid USING "&&&&&",":",
                                g_proceso_cod    USING "&&&&&",":",
                                g_opera_cod      USING "&&&&&",
                                " 2>&1 &"
               RUN v_QryTxt
                  
               CALL fn_mensaje("Atención",
                               "Se ha enviado el reverso de la dispersión de Portabilidad.\n"||
                               "Puede revisar el avance del proceso en el monitor de ejecución de procesos"
                              ,"information")
            END IF --Valida reverso
         ELSE
            CALL fn_mensaje("Atención","Se canceló el reverso","about")
         END IF

         EXIT DIALOG
    END DIALOG
  CLOSE WINDOW w_rev_creditos_cero
END MAIN

#Objetivo: Consulta para validar que exista el folio y/o fecha capturadas en la
#          la tabla de casos de excepción a dispersar
FUNCTION fn_movimiento_proceso(v_folio, v_fecha)
  DEFINE 
    v_folio                  DECIMAL(9,0), --Folio
    v_fecha                  DATE,         --Fecha registro
    v_existe_reg             INTEGER,      --Contador si existen registros
    v_sql_txt                STRING
       
  --Valida que existan registros para los casos de excepción
  LET v_sql_txt = "\n SELECT folio ",
                  "\n FROM glo_folio ",
                  "\n WHERE 1 = 1 "
      
  IF length(v_folio) > 0 THEN
     --Construye la primera consulta
     LET v_sql_txt = v_sql_txt || "\n AND folio = '", v_folio, "'"
  END IF
     
  {IF length(v_fecha) > 0 THEN
     --Construye la primer consulta
     LET v_sql_txt = v_sql_txt || "\n AND f_actualiza = '", v_fecha, "'"
  END IF} 

  LET v_sql_txt = v_sql_txt || " GROUP BY 1"
        
  PREPARE prp_busca_folio_casos FROM v_sql_txt
  EXECUTE prp_busca_folio_casos INTO v_existe_reg

  --Si hay registros para ejecutar, se filtra después por glo_folio
  IF v_existe_reg > 0 THEN
     DISPLAY "Folio encontrado -- ", v_existe_reg
  ELSE 
     DISPLAY "No existe folio en glo folio  "
  END IF 
     
  RETURN v_existe_reg

END FUNCTION

#Objetivo: Función para obtener la información de la tabla dis_caso_excepcion
FUNCTION fn_obtiene_informacion(v_folio, v_fecha)
   DEFINE v_folio                  DECIMAL(9,0), --Folio
         v_fecha                  DATE,         --Fecha registro
         v_sql_txt                STRING,
         v_tot_ctas_gen           SMALLINT,
  
         v_arr_prt         DYNAMIC ARRAY OF RECORD
            v_periodo_pago           CHAR(6),
            v_aportacion             DECIMAL(12,2),
            v_amortizacion           DECIMAL(12,2),
            v_aivs                   DECIMAL(22,2),
            v_total_cuentas          DECIMAL(9,0) 
         END RECORD,
  
         v_tot_aportacion         DECIMAL(12,2),
         v_tot_amortizacion       DECIMAL(12,2),
         v_tot_aivs               DECIMAL(22,2),
         v_tot_total_cuentas      DECIMAL(12,0),
         v_indice                 INTEGER

   LET v_tot_ctas_gen = 0
    
   SELECT COUNT(*)
   INTO   v_tot_ctas_gen
   FROM   dis_interface_prt
   WHERE  folio_liquida  = v_folio
   AND    tipo_prt = 2
   
   --IF v_tot_ctas_gen >= 1 THEN
      --LET r_tipo_rev_cnt = 1
   --ELSE
      LET v_sql_txt= "\n SELECT prt.periodo_pago, ",
                     "\n        sum(prt.imp_ap_pat),",
                     "\n        sum(prt.imp_am_cre),", 
                     "\n        sum(prt.aiv_ap_pat),",
                     "\n        count(*)",
                     "\n FROM   dis_interface_prt prt",
                     "\n WHERE  prt.folio_liquida = ", v_folio

      LET v_sql_txt = v_sql_txt||"\n GROUP BY 1"
      LET v_sql_txt = v_sql_txt||"\n ORDER BY 1"

      DISPLAY v_sql_txt
      PREPARE prp_sql_his_casos FROM v_sql_txt

      LET v_indice             = 1
      LET v_tot_aportacion     = 0.00
      LET v_tot_amortizacion   = 0.00
      LET v_tot_aivs           = 0.00
      LET v_tot_total_cuentas  = 0

      DECLARE cur_casos_excep CURSOR FOR prp_sql_his_casos  
      FOREACH cur_casos_excep INTO v_arr_prt[v_indice].v_periodo_pago,
                                    v_arr_prt[v_indice].v_aportacion,
                                    v_arr_prt[v_indice].v_amortizacion,
                                    v_arr_prt[v_indice].v_aivs,
                                    v_arr_prt[v_indice].v_total_cuentas
                                 
         LET v_tot_aportacion    = v_tot_aportacion    + v_arr_prt[v_indice].v_aportacion 
         LET v_tot_amortizacion  = v_tot_amortizacion  + v_arr_prt[v_indice].v_amortizacion
         LET v_tot_aivs          = v_tot_aivs          + v_arr_prt[v_indice].v_aivs
         LET v_tot_total_cuentas = v_tot_total_cuentas + v_arr_prt[v_indice].v_total_cuentas
         LET v_indice            = v_indice            + 1
      END FOREACH

      CALL v_arr_prt.deleteElement(v_indice)

      {IF v_tot_total_cuentas = 0 THEN
         SELECT UNIQUE dli.f_registro
         INTO   v_fecha
         FROM   dis_liq_inconsistente dli, dis_arh_num_cred_0 dcc
         WHERE  dcc.id_dis_arh_num_cred = dli.id_dis_arh_num_cred
         AND    dcc.folio = dli.folio_arh_num_cred 
         AND    dli.folio_liquida = v_folio
      END IF}
      
  --END IF
  
   RETURN v_arr_prt,
          v_tot_aportacion,
          v_tot_amortizacion,
          v_tot_aivs,
          v_tot_total_cuentas,
          v_fecha,
          r_tipo_rev_cnt

END FUNCTION