################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 09/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISP04                                                    #
#Objetivo         => Programa lanzador de la confirmación                      #
#Fecha de Inicio  => 16/06/2015                                                #
################################################################################
-- Base que se utilizará
DATABASE safre_viv

-- Definición de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_proceso_cod            SMALLINT,    --Codigo del proceso
    v_proceso_cod            SMALLINT,    --Codigo del proceso
    p_opera_cod              SMALLINT,    --Codigo de operacion
    p_pid                    DECIMAL(9,0)
 
  --Datos de salida
  DEFINE 
    a_datos_apo_sub          DYNAMIC ARRAY OF RECORD  
    tpo_credito              STRING,
    tot_registros            DECIMAL(9,0),    
    aiv_ap_pat               DECIMAL(22,6),    
    imp_ap_pat               DECIMAL(12,2),
    confirmar                SMALLINT
    END RECORD 

  --Totales 
  DEFINE 
    v_tot_registros          DECIMAL(9,0),  --Total de registros
    v_tot_aivs               DECIMAL(22,6), --Total de AIVS
    v_tot_aportacion         DECIMAL(12,2), --Total de aportaciones
    v_tot_reg_cred           DECIMAL(9,0)   --Total de registros
    
  DEFINE v_indice            INTEGER
  DEFINE v_respuesta         SMALLINT
  DEFINE v_folio_disp        DECIMAL(9,0)

  DEFINE 
    p_programa               CHAR(10),  
    r_bandera                SMALLINT,
    r_nom_archivo            CHAR(40)

  DEFINE l_comando           STRING
  DEFINE v_ruta_ejecutable   CHAR(40)
  DEFINE v_ruta_listados     CHAR(40)
  DEFINE v_qwery_ibx         STRING 
  DEFINE v_mensaje           STRING

  DEFINE arr_det_apo_sub     DYNAMIC ARRAY OF RECORD 
    id_dis_interface_ef      DECIMAL(9,0),
    id_derechohabiente       DECIMAL(9,0),
    folio_sua                DECIMAL(6,0),
    periodo_pago             CHAR(6),
    f_pago                   DATE,
    nrp                      CHAR(11),
    ind_liquidacion          SMALLINT,
    folio_liquida            DECIMAL(9,0),
    f_liquida                DATE,
    num_crd_ifv              DECIMAL(10,0),
    imp_ap_pat               DECIMAL(12,2),
    aiv_ap_pat               DECIMAL(22,6),
    tpo_credito              SMALLINT,
    cve_ent_financiera       SMALLINT,
    num_ctr_int_ef           CHAR(18),
    concepto                 SMALLINT,
    id_ctr_transaccion       DECIMAL(9,0),
    folio_transaccion        DECIMAL(9,0),
    f_transaccion            DATE,
    folio_factura            DECIMAL(9,0),
    f_factura                DATE,
    estado                   SMALLINT
  END RECORD

  DEFINE v_estado            SMALLINT

  DEFINE
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING
    
END GLOBALS

MAIN
  --Datos de entrada
  DEFINE 
     v_folio_liquida         DECIMAL(9,0),  --Folio de liquidación de la dispersión
     v_f_liquida_ini         DATE,          --Fecha de liquidación inicial
     v_f_liquida_fin         DATE           --Fecha de liquidacion final   
 
  DEFINE 
     bnd_consulta            SMALLINT, 
     f_ventana               ui.Window, --Define las propìedades de la Ventana
     f_forma                 ui.Form    --Define las propiedades de la forma

  DEFINE i                   INTEGER
  DEFINE v_tipo_credito      STRING
  DEFINE v_bnd_confirma      SMALLINT

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
  LET p_proceso_cod  = 3904
  LET p_opera_cod    = 1 
   
  CALL STARTLOG (g_usuario CLIPPED||".DISP04.log")

  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance
   
  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  --Obtiene las rutas ejecutable
  SELECT ruta_bin 
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'ocg'

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
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

  LET bnd_consulta = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISP04" 
    DIALOG ATTRIBUTES(UNBUFFERED) 
   
      INPUT BY NAME v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_detalle", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_tot_registros", 1)    --Oculta el total de registros
          CALL f_forma.setElementHidden("gr_tot_aportaciones", 1) --Oculta el total de aportaciones
          CALL f_forma.setElementHidden("gr_tot_aivs", 1)         --Oculta el total de aivs
          CALL f_forma.setElementHidden("lbl_totales", 1)         --Oculta etiqueta Totales

          NEXT FIELD v_folio_liquida
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro
           IF (v_folio_liquida IS NULL AND v_f_liquida_ini IS NULL AND v_f_liquida_fin IS NULL) THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar un Folio de Operación o un Periodo de Fechas de Operación.",
                              "about")
              NEXT FIELD v_folio_liquida   
           END IF   
   
           IF v_f_liquida_ini > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Operación Inicial no puede ser mayor al Día de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF    

           IF v_f_liquida_fin > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Operación Final no puede ser mayor al Día de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 
          
           IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Operación Final.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 

           IF v_f_liquida_fin IS NOT NULL AND v_f_liquida_ini IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Operación Inicial.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF 

           IF (v_f_liquida_ini > v_f_liquida_fin) THEN
              CALL fn_mensaje("ATENCIÓN", 
                              "La Fecha Inicial de Operación no puede ser mayor a la Fecha Final de Operación.",
                              "about")
              NEXT FIELD v_f_liquida_ini 
           END IF

           IF (v_f_liquida_fin < v_f_liquida_ini) THEN
              CALL fn_mensaje("ATENCIÓN", 
                              "La Fecha Final de Operación no puede ser menor a la Fecha Inicial de Operación.",
                              "about")
              NEXT FIELD v_f_liquida_ini 
           END IF          

           --LET v_f_liquida_ini = v_f_liquida_ini USING "dd-mm-yyyy"
           --LET v_f_liquida_fin = v_f_liquida_fin USING "dd-mm-yyyy"

           --DISPLAY "v_f_liquida_ini: --",v_f_liquida_ini,"-"
           --DISPLAY "v_f_liquida_fin: --",v_f_liquida_fin,"-"          

           CALL fn_consultar(v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin)  

           IF v_indice > 0 THEN 
              CALL f_forma.setElementHidden("gr_detalle", 0)          --Muestra detalle de la consulta
              --CALL f_forma.setElementHidden("gr_tot_registros", 0)    --Muestra el total de registros
              --CALL f_forma.setElementHidden("gr_tot_aportaciones", 0) --Muestra el total de aportaciones
              --CALL f_forma.setElementHidden("gr_tot_aivs", 0)         --Muestra el total de aivs
              CALL f_forma.setElementHidden("lbl_totales", 0)         --Muestra etiqueta Totales

              --DISPLAY ARRAY a_datos_apo_sub TO rec_detalle.*
              --ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)

              INPUT ARRAY a_datos_apo_sub WITHOUT DEFAULTS FROM rec_detalle.*
              ATTRIBUTES (APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, UNBUFFERED, CANCEL = FALSE, ACCEPT = FALSE) 
                --BEFORE DISPLAY
                BEFORE INPUT
                  --DISPLAY v_tot_registros  TO txt_tot_registros
                  --DISPLAY v_tot_aivs       TO txt_tot_aivs
                  --DISPLAY v_tot_aportacion TO txt_tot_aportaciones
           
                --AFTER DISPLAY 
                AFTER INPUT
                  CALL ui.interface.refresh()
                  --CALL DIALOG.setActionHidden("reporte",0)                  
                  --CALL DIALOG.setActionHidden("btn_liquidar", 0)                  
                  CALL ui.interface.refresh()

                ON ACTION cancelar 
                   EXIT PROGRAM 
                    
                --ON ACTION reporte
                   --CALL fn_reporte()
                
                --ON ACTION archivo
                   --CALL fn_genera_archivo(v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin) 

                ON ACTION aceptar
                   LET v_bnd_confirma = 0
                   CALL fn_verificar_apo_sele() RETURNING v_bnd_confirma

                   IF v_bnd_confirma = 1 THEN
                      LET v_respuesta = 0
                      CALL fn_ventana_confirma("CONFIRMAR", 
                                               "¿Está seguro que desea confirmar los registros seleccionados?", 
                                               "quest")
                      RETURNING v_respuesta

                      --Si el usuario confirma 
                      IF v_respuesta = 1 THEN
                         CALL fn_borra_crea_tmp_apo_sub()
                           
                         FOR i = 1 TO a_datos_apo_sub.getLength()
                             IF a_datos_apo_sub[i].confirmar = 1 THEN
                                LET v_tipo_credito = a_datos_apo_sub[i].tpo_credito.subString(6,7) CLIPPED

                                --DISPLAY i, " - ", a_datos_apo_sub[i].*
                                --DISPLAY "v_tipo_credito: ", v_tipo_credito
                                
                                CALL fn_llenar_tmp_apo_sub(v_tipo_credito, v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin)
                             END IF
                         END FOR
                                                   
                         LET v_proceso_cod = 3904
         
                         --Si se acepta la ejecución se genera PID del proceso
                         CALL fn_genera_pid (v_proceso_cod, 1, g_usuario) RETURNING p_pid

                         IF (fn_valida_operacion(p_pid, v_proceso_cod, 1) = 0 ) THEN
                            --Enlaza el folio referencia 
                            LET v_folio_disp = 0;
                            CALL fn_genera_folio_dis(v_proceso_cod, 1, 0, g_usuario)
                            RETURNING v_folio_disp

                            LET p_programa    = "DISP04"
                            LET r_nom_archivo = ""

                            DISPLAY "Folio Disp -- ",v_folio_disp
                            --Ejecuta la funcion de preliquida y asigna estatus de LISTO
                            CALL fn_inicializa_proceso (p_pid, v_proceso_cod, 1, v_folio_disp, 
                                                        p_programa, r_nom_archivo, g_usuario)
                            RETURNING r_bandera

                            --Inicia la operación asignando el estatus de PROCESANDO
                            CALL fn_actualiza_opera_ini(p_pid, v_proceso_cod, 1, v_folio_disp, 
                                                        p_programa, r_nom_archivo, g_usuario)
                            RETURNING r_bandera

                            --DISPLAY "r_bandera: ", r_bandera
                            --DISPLAY "ANTES DE MANDAR nohup DISE20"

                            --Validaciones 
                            LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISE20.42r ",
                                            v_folio_disp," ",
                                            0," ",
                                            0," ",
                                            TODAY," ",
                                            g_usuario, " ",
                                            --f_folio,
                                            " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                            p_pid         USING "&&&&&",":",
                                            v_proceso_cod USING "&&&&&",":",
                                            1             USING "&&&&&" ," 2>&1 &"
                            RUN l_comando
                     
                            LET v_mensaje = "Se ha enviado la confirmación de los registros: ",
                                            p_pid CLIPPED,
                                            ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos.",
                                            ".\nFolio Transacción: ",
                                            v_folio_disp CLIPPED
                            CALL fn_mensaje("Confirmar", v_mensaje, "information")
                                          
                            --EXIT DISPLAY
                            EXIT INPUT
                         ELSE
                            CALL fn_muestra_inc_operacion(fn_valida_operacion(p_pid, v_proceso_cod, 1))
                         END IF  --De valida operacion
                      ELSE
                         CALL fn_mensaje("Aviso","Operación cancelada.","stop") 
                         EXIT PROGRAM
                      END IF
                    ELSE -- v_bnd_confirma = 0
                       LET v_mensaje = "Aún no ha seleccionado ninguna registro para confirmar."
                       CALL fn_mensaje("Confirmar", v_mensaje, "information")
                    END IF
                    --END DISPLAY       
              END INPUT
              EXIT DIALOG
           ELSE
              IF (v_folio_liquida IS NOT NULL) THEN
                 LET v_estado = 0;

                 SELECT UNIQUE dc.estado
                 INTO   v_estado
                 FROM   dis_ctr_aps_tns dc
                 WHERE  dc.folio_liquida = v_folio_liquida
                 AND    dc.estado       IN (70)
                 IF v_estado = 70 THEN
                    CALL fn_mensaje("ATENCIÓN",
                                    "El Folio de Operación ya fue Pagado.",
                                    "about")
                    CALL ui.interface.refresh()
                 ELSE
                    SELECT UNIQUE dc.estado
                    INTO   v_estado
                    FROM   dis_ctr_aps_tns dc
                    WHERE  dc.folio_liquida = v_folio_liquida
                    AND    dc.estado       IN (60)
                    IF v_estado = 60 THEN
                       CALL fn_mensaje("ATENCIÓN",
                                       "El Folio de Operación ya fue Publicado.",
                                       "about")
                       CALL ui.interface.refresh()
                    ELSE
                       SELECT UNIQUE dc.estado
                       INTO   v_estado
                       FROM   dis_ctr_aps_tns dc
                       WHERE  dc.folio_liquida = v_folio_liquida
                       AND    dc.estado       IN (30)
                       IF v_estado = 30 THEN
                          CALL fn_mensaje("ATENCIÓN",
                                          "El Folio de Operación ya fue Facturado.",
                                          "about")
                          CALL ui.interface.refresh()
                       ELSE
                          SELECT UNIQUE dc.estado
                          INTO   v_estado
                          FROM   dis_ctr_aps_tns dc
                          WHERE  dc.folio_liquida = v_folio_liquida
                          AND    dc.estado       IN (20)
                          IF v_estado = 20 THEN
                             CALL fn_mensaje("ATENCIÓN",
                                             "El Folio de Operación ya fue Confirmado.",
                                             "about")
                             CALL ui.interface.refresh()
                          ELSE
                             LET v_tot_reg_cred = 0
                       
                             SELECT COUNT(*)
                             INTO   v_tot_reg_cred
                             FROM   dis_ctr_aps_tns dc
                             WHERE  dc.folio_liquida = v_folio_liquida
                             AND    dc.tpo_credito  IN (2,5)
                             IF v_tot_reg_cred = 0 THEN
                                CALL fn_mensaje("ATENCIÓN",
                                                "El Folio de Operación no es de un concepto de pago Apoyo INFONAVIT o COFINAVIT.",
                                                "about")
                                CALL ui.interface.refresh()
                             END IF
                          END IF
                       END IF
                    END IF
                 END IF
              ELSE
                 CALL fn_mensaje("ATENCIÓN",
                                 "No se encontraron registros.",
                                 "about")
                 CALL ui.interface.refresh()
              END IF   
           END IF           
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG      
          
    END DIALOG 
  CLOSE WINDOW w1

END MAIN 

FUNCTION fn_consultar(p_folio_liquida, p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_folio_liquida     DECIMAL(9,0)--Folio de liquidación de la dispersión
  DEFINE p_f_liquida_ini     DATE        --Fecha de liquidación inicial
  DEFINE p_f_liquida_fin     DATE        --Fecha de liquidacion final   
  DEFINE v_desc_credito      CHAR(50)
  DEFINE v_tpo_credito       SMALLINT 

  LET g_sql_txt = "\n SELECT dca.tpo_credito, ",
                  "\n        COUNT(dca.id_dis_interface_ef), ",
                  "\n        SUM(dca.aiv_ap_pat), ",
                  "\n        SUM(dca.imp_ap_pat) ",
                  "\n FROM   dis_ctr_aps_tns dca, ", 
                  "\n        glo_folio gf ",
                  "\n WHERE  gf.folio       = dca.folio_liquida ",
                  "\n AND    dca.estado     = 10 ", 
                  "\n AND    gf.status      = 2 ",
                  "\n AND    gf.proceso_cod = 932 "  --Falta modificar el proceso_cod     

                  --"\n AND   (dca.estado     = 10 ", 
                  --"\n AND    gf.status      = 2 ",
                  --"\n AND    gf.proceso_cod = 932 ",  --Falta modificar el proceso_cod     
                  --"\n OR     dca.estado     = 10 ", 
                  --"\n AND    gf.proceso_cod = 1217) " --Falta modificar el proceso_cod     

  IF p_folio_liquida IS NOT NULL THEN          
    LET g_sql_txt = g_sql_txt,"\n      AND dca.folio_liquida = ",p_folio_liquida
  END IF

  DISPLAY "p_f_liquida_ini: ", p_f_liquida_ini
  DISPLAY "p_f_liquida_fin: ",p_f_liquida_fin
  
  IF p_f_liquida_ini IS NOT NULL AND p_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dca.f_liquida  >= '",p_f_liquida_ini,"'",
                               "\n AND dca.f_liquida  <= '",p_f_liquida_fin,"'" 
                               --"\n AND gf.f_actualiza >= '",p_f_liquida_ini,"'",
                               --"\n AND gf.f_actualiza <= '",p_f_liquida_fin,"'"
  END IF  
                  
  LET g_sql_txt = g_sql_txt,"\n GROUP BY dca.tpo_credito ",
                            "\n ORDER BY dca.tpo_credito "
                
  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE ps_apo_sub_conf FROM g_sql_txt
  DECLARE cur_apo_sub_conf CURSOR FOR ps_apo_sub_conf
  
  LET v_indice         = 1
  LET v_tot_registros  = 0
  LET v_tot_aivs       = 0.00
  LET v_tot_aportacion = 0.00
  
  FOREACH cur_apo_sub_conf INTO v_tpo_credito,    
                                a_datos_apo_sub[v_indice].tot_registros,
                                a_datos_apo_sub[v_indice].aiv_ap_pat,
                                a_datos_apo_sub[v_indice].imp_ap_pat
                          
    SELECT a.desc_credito_ocg
    INTO   v_desc_credito 
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = v_tpo_credito
    AND    a.ind_activo      = 1

    INITIALIZE a_datos_apo_sub[v_indice].tpo_credito TO NULL
    --LET a_datos_apo_sub[v_indice].confirmar   = 0
    LET a_datos_apo_sub[v_indice].confirmar   = 1
    LET a_datos_apo_sub[v_indice].tpo_credito = v_tpo_credito CLIPPED, " - ", v_desc_credito CLIPPED    
                 
    LET v_tot_registros   = v_tot_registros    + a_datos_apo_sub[v_indice].tot_registros         
    LET v_tot_aivs        = v_tot_aivs         + a_datos_apo_sub[v_indice].aiv_ap_pat
    LET v_tot_aportacion  = v_tot_aportacion   + a_datos_apo_sub[v_indice].imp_ap_pat     
    LET v_indice          = v_indice + 1 
  END FOREACH

  FREE cur_apo_sub_conf
  
  CALL a_datos_apo_sub.deleteElement(v_indice)
  LET v_indice    = v_indice - 1  
  
END FUNCTION  

FUNCTION fn_verificar_apo_sele()
  DEFINE v_bnd_ok            SMALLINT
  DEFINE i                   INTEGER

  LET v_bnd_ok = 0

  FOR i = 1 TO a_datos_apo_sub.getLength()
      IF a_datos_apo_sub[i].confirmar = 1 THEN
         LET v_bnd_ok = 1
         EXIT FOR
      END IF
  END FOR

  RETURN v_bnd_ok
END FUNCTION

FUNCTION fn_borra_crea_tmp_apo_sub()
  WHENEVER ERROR CONTINUE;
   
  DROP TABLE tmp_dis_ctr_aps_tns;
  CREATE TABLE tmp_dis_ctr_aps_tns (id_dis_interface_ef    DECIMAL(9,0),
                                    id_derechohabiente     DECIMAL(9,0),
                                    folio_sua              DECIMAL(6,0),
                                    periodo_pago           CHAR(6),
                                    f_pago                 DATE,
                                    nrp                    CHAR(11),
                                    ind_liquidacion        SMALLINT,
                                    folio_liquida          DECIMAL(9,0),
                                    f_liquida              DATE,
                                    num_crd_ifv            DECIMAL(10,0),
                                    imp_ap_pat             DECIMAL(12,2),
                                    aiv_ap_pat             DECIMAL(18,6),
                                    tpo_credito            SMALLINT,
                                    cve_ent_financiera     SMALLINT,
                                    num_ctr_int_ef         CHAR(18),
                                    concepto               SMALLINT,
                                    id_ctr_transaccion     DECIMAL(9,0),
                                    folio_transaccion      DECIMAL(9,0),
                                    f_transaccion          DATE,
                                    folio_factura          DECIMAL(9,0),
                                    f_factura              DATE,
                                    estado                 SMALLINT);

END FUNCTION

FUNCTION fn_llenar_tmp_apo_sub(p_tipo_credito, p_folio_liquida, p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_tipo_credito            CHAR(03)
  DEFINE p_folio_liquida           DECIMAL(9,0) --Folio de liquidación de la dispersión
  DEFINE p_f_liquida_ini           DATE         --Fecha de liquidación inicial
  DEFINE p_f_liquida_fin           DATE         --Fecha de liquidacion final 
  DEFINE i                         INTEGER

  LET g_sql_txt = "\n SELECT dca.* ",
                  "\n FROM   dis_ctr_aps_tns dca, ", 
                  "\n        glo_folio gf ",
                  "\n WHERE  gf.folio        = dca.folio_liquida ",
                  "\n AND    dca.estado      = 10 ", 
                  "\n AND    gf.status       = 2 ",
                  "\n AND    gf.proceso_cod  = 932 ", --Falta modificar el proceso_cod
                    
                  --"\n AND   (dca.estado      = 10 ", 
                  --"\n AND    gf.status       = 2 ",
                  --"\n AND    gf.proceso_cod  = 932 ", --Falta modificar el proceso_cod
                  --"\n OR     dca.estado      = 10 ", 
                  --"\n AND    gf.proceso_cod  = 1217) ", --Falta modificar el proceso_cod                 

                  "\n AND    dca.tpo_credito = ", p_tipo_credito     

  IF p_folio_liquida IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dca.folio_liquida = ",p_folio_liquida
  END IF
  
  IF p_f_liquida_ini IS NOT NULL AND p_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dca.f_liquida  >= '",p_f_liquida_ini,"'",
                               "\n AND dca.f_liquida  <= '",p_f_liquida_fin,"'" 
                               --"\n AND gf.f_actualiza >= '",p_f_liquida_ini,"'",
                               --"\n AND gf.f_actualiza <= '",p_f_liquida_fin,"'"
  END IF  
                  
  LET g_sql_txt = g_sql_txt,"\n ORDER BY dca.tpo_credito "
                
  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE ps_det_apo_sub FROM g_sql_txt
  DECLARE cur_det_apo_sub CURSOR FOR ps_det_apo_sub

  LET g_sql_txt = " INSERT INTO tmp_dis_ctr_aps_tns VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
  PREPARE ps_insert_tmp_apo_sub FROM g_sql_txt
      
  LET i = 1

  FOREACH cur_det_apo_sub INTO arr_det_apo_sub[i].* 
    --DISPLAY i, " - ", arr_det_apo_sub[i].* 
    EXECUTE ps_insert_tmp_apo_sub USING arr_det_apo_sub[i].* 
    LET i = i + 1
  END FOREACH
   
  CALL arr_det_apo_sub.deleteElement(i)

END FUNCTION

{FUNCTION fn_reporte()
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos reporte
  DEFINE v_rep_indice        INTEGER
  
  LET v_rep_indice = 1

  -- Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISC231.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte
  START REPORT rep_con_disp TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO  a_datos_apo_sub.getLength()
        OUTPUT TO REPORT rep_con_disp(a_datos_apo_sub[v_rep_indice].*,
                                      v_tot_registros,
                                      v_tot_aivs,
                                      v_tot_aportacion, 
                                      g_usuario)
    END FOR
  FINISH REPORT rep_con_disp
  
END FUNCTION}

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
{REPORT rep_con_disp(v_datos_apo_sub, 
                    v_rep_tot_registros, 
                    v_rep_sum_aivs, 
                    v_rep_sum_aportacion, 
                    v_usuario)

  DEFINE v_datos_apo_sub     RECORD  
    tpo_credito              CHAR(30),
    tot_registros            DECIMAL(9,0),    
    aiv_ap_pat               DECIMAL(18,6),    
    imp_ap_pat               DECIMAL(12,2)
  END RECORD                        
  
  DEFINE 
    v_fecha_consulta         DATE,         --Fecha de proceso
    v_usuario                VARCHAR(30),  --Almacena al usuario
    v_rep_tot_registros      DECIMAL(9,0), --Total de registros
    v_rep_sum_aivs           DECIMAL(18,6),
    v_rep_sum_aportacion     DECIMAL(12,2)      
    
  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

    PAGE HEADER
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

    ON EVERY ROW
       PRINTX v_datos_apo_sub.tpo_credito
       PRINTX v_datos_apo_sub.tot_registros USING "###,###,###,###,##&.##"
       PRINTX v_datos_apo_sub.aiv_ap_pat USING "###,###,###,###,##&.##"  
       PRINTX v_datos_apo_sub.imp_ap_pat USING "###,###,###,###,##&.##"       

    ON LAST ROW
       PRINTX v_rep_tot_registros
       PRINTX v_rep_sum_aivs
       PRINTX v_rep_sum_aportacion

END REPORT}

{FUNCTION fn_genera_archivo(v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin)
  DEFINE 
    v_folio_liquida          DECIMAL(9,0),--Folio de liquidación de la dispersión
    v_f_liquida_ini          DATE,        --Fecha de liquidación inicial
    v_f_liquida_fin          DATE         --Fecha de liquidacion final

  DEFINE  
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ruta_envio_dis         CHAR(40),
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_recorre_arreglo        INTEGER,
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_folio                  DECIMAL(9,0)  --Folio liquidación dispersión

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_indice                 SMALLINT

  DEFINE arr_info_apo        DYNAMIC ARRAY OF RECORD
    v_nss                    CHAR(11), 
    v_nombre_af              CHAR(30),
    v_ap_paterno_af          CHAR(30), 
    v_ap_materno_af          CHAR(30), 
    v_periodo                CHAR(6), 
    v_aportacion             DECIMAL(8,2), 
    v_folio_sua              DECIMAL(6,0), 
    v_f_liquida              DATE, 
    v_f_entrega              DATE, 
    v_interface              CHAR(2), 
    v_tpo_credito            CHAR(20)
  END RECORD

  DEFINE v_aportacion        CHAR(10)
  DEFINE v_desc_credito      CHAR(20) 

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/consulta_info_disp_", v_hora

  --se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  --Imprime encabezado del archivo
  LET v_encabezado = "NSS |NOMBRE |PERIODO |APORTACIÓN |FOLIO SUA |FECHA APLIACIÓN EN SAFRE |FECHA ENTREGA |INTERFACE |TIPO CRÉDITO"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT ad.nss, ",
                  "\n        ad.nombre_af, ",
                  "\n        ad.ap_paterno_af, ",
                  "\n        ad.ap_materno_af, ",
                  "\n        dca.periodo_pago, ",
                  "\n        dca.imp_ap_pat, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.f_liquida, ",
                  "\n        TODAY as f_entrega, ",
                  "\n        'AS' as interface, ",
                  "\n        dca.tpo_credito ",
                  "\n FROM   afi_derechohabiente ad, ",
                  "\n        dis_ctr_aps_tns dca ",
                  "\n WHERE  dca.id_derechohabiente = ad.id_derechohabiente " 
  
  IF v_folio_liquida IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND folio_liquida = ",v_folio_liquida
  END IF 

  IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND f_liquida >= '",v_f_liquida_ini,"'",
                               "\n AND f_liquida <= '",v_f_liquida_fin,"'"
  END IF

  LET g_sql_txt = g_sql_txt,"\n ORDER BY ad.nss "  
      
  PREPARE pr_sl_inf_arc FROM g_sql_txt
  DECLARE cur_apo_sub_conf_arc CURSOR FOR pr_sl_inf_arc
  
  LET v_indice = 1
  
  FOREACH cur_apo_sub_conf_arc INTO arr_info_apo[v_indice].v_nss,     
                                    arr_info_apo[v_indice].v_nombre_af,
                                    arr_info_apo[v_indice].v_ap_paterno_af,
                                    arr_info_apo[v_indice].v_ap_materno_af, 
                                    arr_info_apo[v_indice].v_periodo, 
                                    arr_info_apo[v_indice].v_aportacion,
                                    arr_info_apo[v_indice].v_folio_sua,
                                    arr_info_apo[v_indice].v_f_liquida,
                                    arr_info_apo[v_indice].v_f_entrega,
                                    arr_info_apo[v_indice].v_interface,
                                    arr_info_apo[v_indice].v_tpo_credito
                         
    SELECT UNIQUE(desc_credito)
    INTO   v_desc_credito 
    FROM   cat_tipo_credito
    WHERE  tpo_credito = arr_info_apo[v_indice].v_tpo_credito

    LET v_aportacion = arr_info_apo[v_indice].v_aportacion * 100 USING "&&&&&&&&&&"

    LET v_detalle =  arr_info_apo[v_indice].v_nss, "|",
                     arr_info_apo[v_indice].v_nombre_af CLIPPED," ", arr_info_apo[v_indice].v_ap_paterno_af CLIPPED," ", arr_info_apo[v_indice].v_ap_materno_af CLIPPED, "|",
                     arr_info_apo[v_indice].v_periodo CLIPPED, "|",
                     --arr_info_apo[v_indice].v_aportacion  USING "###,###,###,###,##&.##", "|",
                     v_aportacion, "|",
                     arr_info_apo[v_indice].v_folio_sua, "|",
                     arr_info_apo[v_indice].v_f_liquida USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_f_entrega USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_interface, "|",
                     arr_info_apo[v_indice].v_tpo_credito CLIPPED, " - ", v_desc_credito CLIPPED

    CALL v_ch_arch_salida.write([v_detalle])
    
    LET v_indice = v_indice + 1 
  END FOREACH

  FREE cur_apo_sub_conf_arc
  
  CALL arr_info_apo.deleteElement(v_indice)
  LET v_indice = v_indice - 1 

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta de Dispersion por NSS \n en la ruta "||v_ruta_nomarch,"information") 
  
END FUNCTION}