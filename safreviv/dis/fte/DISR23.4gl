################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 11/04/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR23                                                        #
#Objetivo     => Programa que ejecuta el reverso de                            #
#                Factura - Modulo Otorgamiento Créditos 43 Bis.                #
#Fecha inicio => 28/06/2016                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --id del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Código del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod  --Código de operación

  CONSTANT Por_Folio = 0
  CONSTANT Por_Fecha = 1
  CONSTANT Sin       = 0

  DEFINE a_apo_sub_fac       DYNAMIC ARRAY OF RECORD
    tpo_credito	             SMALLINT,
    desc_credito	         VARCHAR(40),
    cve_ent_financiera	     SMALLINT,
    ent_financiera_desc	     VARCHAR(40),
    clabe                    CHAR(18),
    estado	                 SMALLINT,
    desc_edo_aps	         VARCHAR(40),
    aportacion	             DECIMAL(22,2)
  END RECORD

  DEFINE a_scr_apo_sub       DYNAMIC ARRAY OF RECORD
    concepto	             VARCHAR(45),
    ent_financiera	         VARCHAR(45),
    clabe                    CHAR(18),
    estado	                 VARCHAR(45),
    aportacion	             DECIMAL(22,2)
  END RECORD,

  v_tot_monto_pesos          DECIMAL(22,2),
  v_tot_monto_acciones       DECIMAL(26,6),
  v_tot_total_cuentas        DECIMAL(12,0)

  DEFINE
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, --Clave del usuario
    p_tipo_ejecucion         SMALLINT,   --Forma como ejecutara el programa
    p_s_titulo               STRING,     --Título de la ventana
    v_folio_factura          DECIMAL(9,0),
    r_respuesta              INTEGER,
    r_tot_pesos              DECIMAL(22,2),
    r_tot_acciones           DECIMAL(26,6),
    r_tot_ctas               DECIMAL(12,0)
    
  DEFINE 
    f_ventana                ui.Window,  --Define las propìedades de la Ventana
    f_forma                  ui.Form,    --Define las propiedades de la forma
    r_bandera                SMALLINT,
    v_QryTxt                 STRING
    
  DEFINE v_seg_modulo        RECORD LIKE seg_modulo.*
  DEFINE seg_modulo_bat      RECORD LIKE seg_modulo.*

  DEFINE v_bnd_existe_info   DECIMAL(9,0)

  SELECT *
  INTO   v_seg_modulo.*
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg" 

  SELECT *
  INTO   seg_modulo_bat.*
  FROM   seg_modulo
  WHERE  modulo_cod = "bat" 

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)
  LET g_proceso_cod    = 3905
  LET g_opera_cod      = 1

  --Si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", g_proceso_cod,
                  "AND    a.cod_proceso_valida = b.proceso_cod ",
                  "AND    a.cod_convivencia    = 0             ",
                  "ORDER BY cod_proceso_valida   "
  PREPARE ps_val_proc FROM g_sql_txt
  DECLARE cur_val_proc CURSOR FOR ps_val_proc
  FOREACH cur_val_proc INTO v_proc_entra,
                            v_proc_val,
                            v_desc_proc_val
    IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH
  
  CLOSE WINDOW SCREEN 

  OPEN WINDOW vtn_rev_fact WITH FORM "DISR231"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio_factura
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL f_forma.setElementHidden("gr_detalle", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_tot_registros", 1)    --Oculta el total de registros
          CALL f_forma.setElementHidden("gr_tot_aportaciones", 1) --Oculta el total de aportaciones
          CALL f_forma.setElementHidden("gr_tot_aivs", 1)         --Oculta el total de aivs
          CALL DIALOG.setActionHidden("reverso", 1)            
      END INPUT

      DISPLAY ARRAY a_scr_apo_sub TO rec_apo_sub.*
      END DISPLAY 

      ON ACTION cancelar
         EXIT DIALOG    
         
      ON ACTION ACCEPT
         IF v_folio_factura IS NULL OR
            v_folio_factura = "" THEN
            CALL fn_mensaje("ATENCIÓN","Debe capturar el Folio de Factura.","about")
            NEXT FIELD v_folio_factura   
         ELSE
            LET v_bnd_existe_info = 0
            CALL fn_validar_folio(v_folio_factura) RETURNING v_bnd_existe_info    

            IF v_bnd_existe_info > 0 THEN
               CALL f_forma.setElementHidden("gr_detalle", 0)  --Muestra detalle de la consulta
               CALL DIALOG.setActionHidden("reverso", 0)        
               CALL DIALOG.setActionHidden("accept", 1)        --Muestra el botón reporte 

               --Obtiene informacion Factura OCG
               CALL fn_obtiene_informacion(v_folio_factura)
                   
               --Muestra las cifras totales de la Factura OCG
               DISPLAY v_tot_monto_pesos    TO txt_tot_aportaciones
               DISPLAY v_tot_monto_acciones TO txt_tot_aivs
               DISPLAY v_tot_total_cuentas  TO txt_tot_registros
            END IF
         END IF

      ON ACTION reverso
         --Solicita confirmar(1) o cancelar(0) la operación de Registro
         CALL fn_ventana_confirma("Confirmación", 
                                  "¿Desea ejecutar el reverso?", 
                                  "quest") 
         RETURNING r_respuesta

         IF r_respuesta = 1 THEN
            --Se obtiene pid de la operación de acuerdo al folio
            SELECT DISTINCT pid 
            INTO   g_pid
            FROM   bat_ctr_operacion
            WHERE  folio = v_folio_factura

            --Si no existe el pid de acuerdo al folio, traerá el último
            IF g_pid IS NULL OR g_pid = 0 THEN
               CALL fn_max_pid(g_proceso_cod, 1) RETURNING g_pid 
            END IF
                  
            --Llama la función para validar la ejecucución del reverso
            CALL fn_valida_reverso(g_pid, g_proceso_cod, g_opera_cod)
            RETURNING r_bandera

            IF r_bandera  <> 0 THEN
               CALL fn_muestra_inc_operacion(r_bandera)
               EXIT PROGRAM 
            END IF 

            LET v_QryTxt = " nohup time fglrun ",v_seg_modulo.ruta_bin CLIPPED,"/DISR231.42r ",
                           p_usuario_cod CLIPPED, " ",
                           g_pid  , " " ,
                           g_proceso_cod , " " ,
                           g_opera_cod ," ",
                           v_folio_factura ," '",
                           "0" CLIPPED,"' ",
                           " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                           "/nohup:",g_pid        USING "&&&&&",":",
                           g_proceso_cod USING "&&&&&",":",
                           g_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"
            RUN v_QryTxt
                           
            CALL fn_mensaje("Atención",
                            "Se ha enviado el reverso de la Facturación.\n"||
                            "Puede revisar el avance del proceso en el monitor de ejecución de procesos.",
                            "information")
         ELSE
            CALL fn_mensaje("Atención","Se canceló el reverso.","about")
         END IF

         EXIT DIALOG 
    END DIALOG   
  CLOSE WINDOW vtn_rev_fact
END MAIN

#Objetivo: Consulta para validar que exista el folio
FUNCTION fn_validar_folio(p_folio_factura)
DEFINE p_folio_factura       DECIMAL(9,0), --Folio liquidado
       v_consulta            STRING,
       v_existe_folio        DECIMAL(9,0)

  --Valida que no existan registros en dis_ctr_aps_tns ya facturados de acuerdo a los criterios de búsqueda
  LET v_consulta = "\n SELECT COUNT(*) ",
                   "\n FROM   dis_ctr_aps_tns ", 
                   "\n WHERE  estado IN (60, 70) "
  IF (p_folio_factura IS NOT NULL) THEN 
     LET v_consulta = v_consulta || "AND  folio_factura = ", p_folio_factura
  END IF 

  LET v_existe_folio = 0
  --Ejecuta la consulta para validar que existan registros
  PREPARE ps_folio_trans FROM v_consulta
  EXECUTE ps_folio_trans INTO v_existe_folio
   
  IF v_existe_folio > 0 THEN
     CALL fn_mensaje("Atención","Existen registro en trámite de pago o pagados.","about")
     LET v_existe_folio = 0
  ELSE
     --Valida que existan registros en dis_ctr_aps_tns de acuerdo a los criterios de búsqueda
     LET v_consulta = "\n SELECT COUNT(*) ",
                      "\n FROM   dis_ctr_aps_tns ", 
                      "\n WHERE  estado IN (30) "
     IF (p_folio_factura IS NOT NULL) THEN 
        LET v_consulta = v_consulta || "AND folio_factura = ", p_folio_factura
     END IF 

     LET v_existe_folio = 0
     --Ejecuta la consulta para validar que existan registros
     PREPARE ps_folio_trans1 FROM v_consulta
     EXECUTE ps_folio_trans1 INTO v_existe_folio
   
     IF v_existe_folio > 0 THEN
     ELSE
        CALL fn_mensaje("Atención","No existe registro con los parámetros capturados.","about")
     END IF 
  END IF 
   
  RETURN v_existe_folio
END FUNCTION

#Objetivo: Función para obtener la información de la tabla dis_ctr_aps_tns
FUNCTION fn_obtiene_informacion(p_folio_factura)
  DEFINE 
    p_folio_factura          DECIMAL(9,0), --Folio factura
    v_sql_txt                STRING,
    v_indice                 INTEGER 

  LET v_sql_txt = "\n SELECT dc.tpo_credito,               ",
                  "\n        tc.desc_credito_ocg,          ",
                  "\n        dc.cve_ent_financiera,        ",
                  "\n        ef.ent_financiera_desc,       ",
                  "\n        ef.clabe,                     ",
                  "\n        dc.estado,                    ",
                  "\n        ce.desc_edo_aps,              ",
                  "\n        SUM (dc.imp_ap_pat)           ",
                  "\n FROM   dis_ctr_aps_tns dc,           ",
                  "\n        glo_folio gf,                 ",
                  "\n        OUTER cat_tpo_credito_ocg tc, ",
                  "\n        OUTER cat_cta_cnt_ocg ef,     ",
                  --"\n        OUTER dis_ctr_factura_aps fa, ",
                  "\n        OUTER cat_edo_aps ce          ",
                  "\n WHERE  gf.folio              = dc.folio_factura      ",
                  "\n AND    dc.tpo_credito	       = ef.tpo_credito        ",
                  "\n AND    dc.tpo_credito        = tc.tpo_credito_ocg    ",
                  "\n AND    dc.cve_ent_financiera = ef.cve_ent_financiera ",
                  "\n AND    dc.tpo_credito	       = ef.tpo_credito        ",                    
                  --"\n AND    dc.cve_ent_financiera = fa.cve_ent_financiera ",
                  --"\n AND    dc.tpo_credito	     = fa.tpo_credito        ",
                  --"\n AND    dc.folio_factura	     = fa.folio_factura      ",
                  "\n AND    tc.ind_activo         = 1                     ",
                  "\n AND    dc.estado             = ce.cod_edo_aps        ",
                  "\n AND    gf.status             = 1                     ",
                  "\n AND    gf.proceso_cod        = 3905                  ",
                  "\n AND    dc.folio_factura      =  ", p_folio_factura,
                  "\n GROUP BY dc.tpo_credito, tc.desc_credito_ocg,        ",
                  "\n          dc.cve_ent_financiera, ef.ent_financiera_desc, ",
                  "\n          ef.clabe, dc.estado, ce.desc_edo_aps        ",
                  "\n ORDER BY dc.tpo_credito, dc.cve_ent_financiera       "
          
  PREPARE ps_rev_factura_ocg FROM v_sql_txt
  DECLARE cur_rev_factura_ocg CURSOR FOR ps_rev_factura_ocg

  LET v_indice          = 1
  LET v_tot_monto_pesos = 0.00

  CALL a_apo_sub_fac.clear()
  CALL a_scr_apo_sub.clear()

  FOREACH cur_rev_factura_ocg INTO a_apo_sub_fac[v_indice].*
    LET a_scr_apo_sub[v_indice].concepto       = a_apo_sub_fac[v_indice].tpo_credito, " - ",a_apo_sub_fac[v_indice].desc_credito
    LET a_scr_apo_sub[v_indice].ent_financiera = a_apo_sub_fac[v_indice].cve_ent_financiera USING "&&&", " - ",a_apo_sub_fac[v_indice].ent_financiera_desc
    LET a_scr_apo_sub[v_indice].clabe          = a_apo_sub_fac[v_indice].clabe
    LET a_scr_apo_sub[v_indice].estado         = a_apo_sub_fac[v_indice].estado, " - ",a_apo_sub_fac[v_indice].desc_edo_aps
    LET a_scr_apo_sub[v_indice].aportacion     = a_apo_sub_fac[v_indice].aportacion
    
    LET v_tot_monto_pesos = v_tot_monto_pesos + a_apo_sub_fac[v_indice].aportacion
    LET v_indice          = v_indice          + 1
  END FOREACH

  CALL a_apo_sub_fac.deleteElement(v_indice)
  CALL a_scr_apo_sub.deleteElement(v_indice)
END FUNCTION