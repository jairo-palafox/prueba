################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 09/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISP09                                                    #
#Objetivo         => Programa que lanza el proceso de Facturaci�n del m�dulo   #
#                    OCG                                                       #
#Fecha de Inicio  => 26/06/2016                                                #
################################################################################
-- Base que se utilizar�
DATABASE safre_viv

-- Definici�n de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opci�n del men�
    p_pid                    DECIMAL(9,0)
 
  --Datos de salida
  DEFINE a_fac_apo_sub       DYNAMIC ARRAY OF RECORD
    cve_ent_financiera       SMALLINT,   
    entidad_financiera       CHAR(65),
    cuenta_bancaria          CHAR(18),    
    documento_fico           CHAR(10),    
    estado                   CHAR(50),
    monto                    DECIMAL (18,6),
    facturar                 SMALLINT
  END RECORD 

  DEFINE a_ent_fin_bloq      DYNAMIC ARRAY OF RECORD
    cve_ent_financiera       SMALLINT,   
    entidad_financiera       CHAR(65),
    cuenta_bancaria          CHAR(18),    
    documento_fico           CHAR(10),    
    --concepto			       CHAR(50),
    estado                   CHAR(50),
    monto                    DECIMAL (18,6),
    facturar                 SMALLINT
  END RECORD 

  DEFINE a_fac_apo_con       DYNAMIC ARRAY OF RECORD
    concepto                 CHAR(50), 
    imp_ap_pat               DECIMAL(18,2)
  END RECORD 

  --Totales 
  DEFINE 
    v_tot_det                CHAR(50),      --Total del detalle
    v_tot_con                DECIMAL(18,6), --Total por concepto
    v_tot_bloq               CHAR(50)       --Total del detalle   

  DEFINE v_indice1           INTEGER
  DEFINE v_indice2           INTEGER
  DEFINE v_indice3           INTEGER  
  DEFINE v_respuesta         SMALLINT
  DEFINE v_proceso_cod       SMALLINT      --C�digo del proceso

  DEFINE arr_fac_apo_sub     DYNAMIC ARRAY OF RECORD 
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
    aiv_ap_pat               DECIMAL(18,6),
    tpo_credito              SMALLINT,
    cve_ent_financiera       SMALLINT,
    num_ctr_int_ef           CHAR(18),
    concepto                 SMALLINT,
    id_ctr_transaccion       DECIMAL(9,0),
    folio_transaccion        DECIMAL(9,0),
    f_transaccion            DATE,
    folio_factura			 DECIMAL(9,0),
    f_factura				 DATE,
    estado                   SMALLINT
  END RECORD

  DEFINE 
    v_folio_disp             DECIMAL(9,0),
    p_programa               CHAR(10),  
    r_bandera                SMALLINT,
    r_nom_archivo            CHAR(40)  

  DEFINE 
    l_comando                STRING,
    v_ruta_ejecutable        CHAR(40),
    v_ruta_listados          CHAR(40),
    v_qwery_ibx              STRING, 
    v_mensaje                STRING,
    v_estado                 SMALLINT

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
    v_folio_transaccion      DECIMAL(9,0),  --Folio de transacci�n
    --v_f_transaccion_ini      DATE,          --Fecha de transacci�n inicial
    --v_f_transaccion_fin      DATE,          --Fecha de transacci�n final 
    v_tipo_credito           SMALLINT,      --Tipo de cr�dito
    v_concepto               SMALLINT,
    v_estado                 SMALLINT
 
  DEFINE 
    bnd_consulta             SMALLINT, 
    f_ventana                ui.Window, --Define las propiedades de la Ventana
    f_forma                  ui.Form    --Define las propiedades de la forma    

  DEFINE 
    i                        INTEGER, 
    p_proceso_cod            SMALLINT,
    p_opera_cod              SMALLINT,          
    v_bnd_confirma           SMALLINT,
    v_ent_financiera         SMALLINT,
    v_num_ctr_int_ef         CHAR(18)

  DEFINE v_edo_aux           CHAR(02)
 
  DEFINE v_status            SMALLINT
  DEFINE v_error             CHAR(70)
  DEFINE v_bnd_confirmacion  SMALLINT 
   
  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  CALL STARTLOG (g_usuario CLIPPED||".DISP09.log")

  LET bnd_consulta  = 0
  LET p_proceso_cod = 3905        
  LET p_opera_cod   = 1

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  --Validaci�n que NO se tenga alguna operaci�n de Dispersi�n de Pagos ejecut�ndose
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
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecut�ndose,\ningrese a esta opci�n cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH

  --Obtiene las rutas ejecutable
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo
  WHERE  modulo_cod = 'ocg'

  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  --CALL fn_rutas("ocg") RETURNING v_ruta_ejecutable, v_ruta_listados

  CLOSE WINDOW SCREEN

  LET v_folio_transaccion          = 0
  --INITIALIZE v_f_transaccion_ini   TO NULL 
  --INITIALIZE v_f_transaccion_fin   TO NULL 
  LET v_tipo_credito               = 0

  OPEN WINDOW w1 WITH FORM "DISP091"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio_transaccion, 
                    --v_f_transaccion_ini, v_f_transaccion_fin, 
                    v_tipo_credito
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_detalle", 1) --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_tot_con", 1) --Oculta total por concepto de la consulta                    
          CALL f_forma.setElementHidden("gr_ent_fin_bloq", 1) --Oculta entidades financieras bloqueadas                  
          
          CALL f_llena_tipo_credito()
          
          NEXT FIELD v_folio_transaccion
          CALL ui.interface.refresh()

        ON ACTION ACCEPT
           --V�lidaciones de criterios de b�squeda
           IF v_tipo_credito IS NULL THEN 
              CALL fn_mensaje("ATENCI�N",
                              "Debe seleccionar un Concepto de Pago.",
                              "about")
              NEXT FIELD v_tipo_credito  
           END IF 
          
           {IF v_f_transaccion_ini > TODAY THEN 
              CALL fn_mensaje("ATENCI�N",
                              "La Fecha de Transacci�n Inicial no puede ser mayor al D�a de Hoy.",
                              "about")
              NEXT FIELD v_f_transaccion_ini
           END IF    

           IF v_f_transaccion_fin > TODAY THEN 
              CALL fn_mensaje("ATENCI�N",
                              "La Fecha de Transacci�n Final no puede ser mayor al D�a de Hoy.",
                              "about")
              NEXT FIELD v_f_transaccion_fin
           END IF 
          
           IF v_f_transaccion_ini IS NOT NULL AND v_f_transaccion_fin IS NULL THEN 
              CALL fn_mensaje("ATENCI�N", 
                             "Debe capturar una Fecha de Transacci�n Final.",
                             "about")
             NEXT FIELD v_f_transaccion_fin
           END IF 

           IF v_f_transaccion_fin IS NOT NULL AND v_f_transaccion_ini IS NULL THEN 
              CALL fn_mensaje("ATENCI�N", 
                              "Debe capturar una Fecha de Transacci�n Inicial.",
                              "about")
              NEXT FIELD v_f_transaccion_fin
           END IF 

           IF (v_f_transaccion_ini > v_f_transaccion_fin) THEN
               CALL fn_mensaje("ATENCI�N", 
                               "La Fecha Inicial de Transacci�n no puede ser mayor a la Fecha Final de Transacci�n.",
                               "about")
               NEXT FIELD v_f_transaccion_ini 
           END IF

           IF (v_f_transaccion_fin < v_f_transaccion_ini) THEN
               CALL fn_mensaje("ATENCI�N", 
                               "La Fecha Final de Transacci�n no puede ser menor a la Fecha Inicial de Transacci�n.",
                               "about")
               NEXT FIELD v_f_transaccion_fin 
           END IF}                    

           CALL fn_borra_crea_tmp_apo_sub()
           --CALL fn_llenar_tabla_tmp_fac(v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, v_tipo_credito)
           CALL fn_llenar_tabla_tmp_fac(v_folio_transaccion, v_tipo_credito)
           
           --PARA EL PROGRAMA DE FACTURAR SI SE NECESITA ESTA EJECUCI�N
           WHENEVER ERROR CONTINUE  

           DISPLAY "EXECUTE PROCEDURE sp_dis_fac_apo_subs1(?)"
           PREPARE prp_exe_alta FROM "EXECUTE PROCEDURE sp_dis_fac_apo_subs1(?)"
           EXECUTE prp_exe_alta USING v_folio_transaccion
           INTO v_status, v_error

           --DISPLAY "------------->>> v_status: ",v_status
           --DISPLAY "------------->>> v_error: ",v_error

           ### FUNCION PARA ACTUALIZAR BANCO, CUENTA

           --CALL fn_consultar(v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, v_tipo_credito)
           --CALL fn_llenar_ent_fin_bloq(v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, v_tipo_credito)

           CALL fn_consultar(v_folio_transaccion, v_tipo_credito)
           CALL fn_llenar_ent_fin_bloq(v_folio_transaccion, v_tipo_credito)

           IF v_indice1 > 0 AND v_indice2 > 0 THEN 
              CALL f_forma.setElementHidden("gr_detalle", 0) --Muestra detalle de la consulta 
              CALL f_forma.setElementHidden("gr_tot_con", 0) --Muestra totales por concepto
              CALL f_forma.setElementHidden("gr_ent_fin_bloq", 0) --Muestra entidades financieras bloqueadas                  
             
              INPUT ARRAY a_fac_apo_sub WITHOUT DEFAULTS FROM rec_detalle.*
              ATTRIBUTES (APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, UNBUFFERED, CANCEL = FALSE, ACCEPT = FALSE) 

                BEFORE INPUT                 
                  --DISPLAY v_tot_det     TO txt_tot_det
                  --DISPLAY v_tot_con     TO txt_tot_con
                  --DISPLAY v_tot_bloq    TO txt_tot_bloq
                  
                  DISPLAY ARRAY a_fac_apo_con TO rec_tot_con.*
                  ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                    BEFORE DISPLAY
                      EXIT DISPLAY
                  END DISPLAY   

                  DISPLAY ARRAY a_ent_fin_bloq TO rec_ent_fin_bloq.*
                  ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                    BEFORE DISPLAY
                        --DISPLAY "DISPLAY ARRAY entidades financieras bloqueadas"
                      EXIT DISPLAY
                  END DISPLAY      
 
                AFTER INPUT                                             
                  CALL ui.interface.refresh()                                                  

                ON ACTION cancelar 
                   EXIT PROGRAM 

                ON ACTION facturar
                   LET v_bnd_confirma = 0
                   CALL fn_verificar_apo_sele() RETURNING v_bnd_confirma

                   IF v_bnd_confirma = 1 THEN
                      LET v_respuesta = 0
                      CALL fn_ventana_confirma("FACTURAR", 
                                               "�Est� seguro que desea facturar los registros seleccionados?", 
                                               "quest")
                      RETURNING v_respuesta

                      IF v_respuesta = 1 THEN
                         CALL fn_borra_crea_tmp_apo_sub()

                         INITIALIZE v_edo_aux TO NULL

                         --DISPLAY "LLENA fn_llen_tmp_fac_apo_sub"
                         FOR i = 1 TO a_fac_apo_sub.getLength()
                             IF a_fac_apo_sub[i].facturar = 1 THEN
                                LET v_ent_financiera = a_fac_apo_sub[i].cve_ent_financiera CLIPPED 
                                --LET v_concepto       = a_fac_apo_sub[i].concepto CLIPPED
                                LET v_edo_aux        = a_fac_apo_sub[i].estado[1,2] CLIPPED
                                LET v_estado         = v_edo_aux
                                LET v_num_ctr_int_ef = a_fac_apo_sub[i].cuenta_bancaria CLIPPED

                                {DISPLAY "v_ent_financiera: ", v_ent_financiera
                                DISPLAY "v_edo_aux: ",v_edo_aux
                                DISPLAY "v_estado: ", v_estado
                                DISPLAY "a_fac_apo_sub:",i, " - ", a_fac_apo_sub[i].*}
                                                  
                                {CALL fn_llen_tmp_fac_apo_sub(v_folio_transaccion, v_f_transaccion_ini, 
                                                             v_f_transaccion_fin, v_tipo_credito, v_ent_financiera,
                                                             v_concepto, v_estado, v_num_ctr_int_ef)}
                                CALL fn_llen_tmp_fac_apo_sub(v_folio_transaccion, v_tipo_credito, v_ent_financiera,
                                                             v_concepto, v_estado, v_num_ctr_int_ef)
                             END IF
                         END FOR                                
                        
                         LET v_proceso_cod = 3905
                
                         --Si se acepta la ejecuci�n se genera PID del proceso
                         CALL fn_genera_pid (v_proceso_cod, 1, g_usuario) RETURNING p_pid

                         IF (fn_valida_operacion(p_pid, v_proceso_cod, 1) = 0 ) THEN
                            --Enlaza el folio referencia 
                            LET v_folio_disp = 0;
                            CALL fn_genera_folio_dis(v_proceso_cod, 1, 0, g_usuario)
                            RETURNING v_folio_disp

                            LET p_programa    = "DISP09"
                            LET r_nom_archivo = ""

                            DISPLAY "Folio Disp -- ",v_folio_disp
                            --Ejecuta la funcion de preliquida y asigna estatus de LISTO
                            CALL fn_inicializa_proceso (p_pid, v_proceso_cod, 1, v_folio_disp, 
                                                        p_programa, r_nom_archivo, g_usuario)
                            RETURNING r_bandera

                            --Inicia la operaci�n asignando el estatus de PROCESANDO
                            CALL fn_actualiza_opera_ini(p_pid, v_proceso_cod, 1, v_folio_disp, 
                                                        p_programa, r_nom_archivo, g_usuario)
                            RETURNING r_bandera

                            --DISPLAY "r_bandera: ", r_bandera 

                            --LA EJECUCI�N DE LA ESTA FUNCI�N SE COLOCA EN EL PROGRAMA DISE25                            
                            {WHENEVER ERROR CONTINUE  
                            
                            DISPLAY "EXECUTE PROCEDURE sp_dis_fac_apo_subs(?)"
                            PREPARE ps_valida_prev_facturacion_apo_sub FROM "EXECUTE PROCEDURE sp_dis_fac_apo_subs(?)"
                            EXECUTE ps_valida_prev_facturacion_apo_sub USING v_folio_disp
                            INTO v_status, v_error

                            LET v_bnd_confirmacion = 0  --Quitar para el programa de facturar
                            IF v_status = 0 THEN                                            
                                LET v_bnd_confirmacion = 0
                                DISPLAY "La facturaci�n se ha realizado exitosamente."
                            ELSE                      
                                LET v_bnd_confirmacion = 1
                                DISPLAY "Ocurri� un error inesperado."
                            END IF

                            IF v_bnd_confirmacion = 0 THEN      
                                WHENEVER ERROR CONTINUE         
                                --Actualiza el status del folio de dispersi�n a Preliquidado 
                                LET g_sql_txt = " UPDATE glo_folio",
                                                " SET    status      = 1",
                                                " WHERE  folio       = ",v_folio_disp,
                                                " AND    proceso_cod = ",p_proceso_cod,
                                                " AND    opera_cod   = 1"}
                                             
                                PREPARE prep_actualiza_folio_confirmado FROM g_sql_txt
                                EXECUTE prep_actualiza_folio_confirmado
                                                              
                                --Validaciones 
                                LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISE25.42r ",
                                                v_folio_disp," ",
                                                v_folio_transaccion," ",
                                                --v_f_transaccion_ini, " ",
                                                --v_f_transaccion_fin," ", 
                                                TODAY, " ",
                                                TODAY, " ", 
                                                v_tipo_credito," ",
                                                TODAY," ",
                                                g_usuario, " ",
                                                --f_folio,
                                                " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                                p_pid         USING "&&&&&",":",
                                                v_proceso_cod USING "&&&&&",":",
                                                1             USING "&&&&&" ," 2>&1 &"
                                RUN l_comando

                                DISPLAY l_comando
                         
                                LET v_mensaje = "Se ha enviado la facturaci�n: ",
                                                p_pid CLIPPED,
                                                ".\nPuede revisar el avance del proceso en el monitor de ejecuci�n de procesos.",
                                                ".\nFolio Factura: ",
                                                v_folio_disp CLIPPED

                                CALL fn_mensaje("Facturaci�n", v_mensaje, "information")
                            {ELSE
                                DISPLAY "Error en la facturaci�n: ",v_bnd_confirmacion
                                CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
                                RETURNING v_bnd_confirmacion
                                EXIT PROGRAM
                            END IF}
                                       
                            EXIT INPUT
                         ELSE
                            CALL fn_muestra_inc_operacion(fn_valida_operacion(p_pid, v_proceso_cod, 1))
                         END IF  --De valida operacion
                      ELSE
                         CALL fn_mensaje("Aviso","Operaci�n cancelada.","stop") 
                         EXIT PROGRAM
                      END IF --v_respuesta
                   ELSE --v_bnd_confirma = 0
                      LET v_mensaje = "A�n no ha seleccionado ning�n registro para facturar."
                      CALL fn_mensaje("Facturaci�n", v_mensaje, "information")   
                   END IF  --v_bnd_confirma = 1 
            
                {ON ACTION reporte
                   CALL fn_reporte()
                
                ON ACTION archivo
                   CALL fn_genera_archivo(v_folio_transaccion, v_f_transaccion_ini, v_f_transaccion_fin, v_tipo_credito)}                              

              END INPUT
           ELSE
              {IF (v_folio_transaccion IS NOT NULL) THEN
                 LET v_estado = 0;
                
                 SELECT COUNT(*)
                 INTO   v_estado
                 FROM   dis_ctr_aps_tns dc
                 WHERE  dc.folio_transaccion = v_folio_transaccion
                 AND    dc.estado            = 20
                 IF v_estado = 0 THEN
                    CALL fn_mensaje("ATENCI�N",
                                    "El Folio de Transacci�n no ha sido Confirmado.",
                                    "about")
                    CALL ui.interface.refresh()
                 END IF

              ELSE}
                 CALL fn_mensaje("ATENCI�N",
                                 "No se encontraron registros para Facturar.",
                                 "about")
                 CALL ui.interface.refresh()
              --END IF
           END IF   
           EXIT DIALOG
      END INPUT 
      
      ON ACTION cancelar
        EXIT DIALOG      

    END DIALOG 
  CLOSE WINDOW w1 
END MAIN 

FUNCTION f_llena_tipo_credito()
  DEFINE v_cbx_tipo_credito  ui.ComboBox -- Combo de tipo de cr�dito

  DEFINE 
    v_tipo_credito           SMALLINT,
    v_desc_credito           CHAR(30)

  LET v_cbx_tipo_credito = ui.ComboBox.forName("formonly.v_tipo_credito")

  CALL v_cbx_tipo_credito.clear()

  LET g_sql_txt = "\n SELECT a.tpo_credito_ocg, a.desc_credito_ocg ",
                  "\n FROM   cat_tpo_credito_ocg a ",
                  "\n WHERE  a.tpo_credito_ocg IN (2,3,5) ",
                  "\n AND    a.ind_activo       = 1 ",
                  "\n ORDER BY a.tpo_credito_ocg "

  PREPARE pr_sl_tc FROM g_sql_txt
  DECLARE cur_sl_tc CURSOR FOR pr_sl_tc
  FOREACH cur_sl_tc INTO v_tipo_credito, v_desc_credito 
    CALL v_cbx_tipo_credito.addItem(v_tipo_credito, v_tipo_credito||' - '||v_desc_credito)                         
  END FOREACH  
  
  FREE cur_sl_tc
END FUNCTION 

--FUNCTION fn_consultar(p_folio_transaccion, p_f_transaccion_ini, p_f_transaccion_fin, p_tipo_credito)
FUNCTION fn_consultar(p_folio_transaccion, p_tipo_credito)
  DEFINE
    p_folio_transaccion      DECIMAL(9,0), --Folio de transacci�n
    p_f_transaccion_ini      DATE,         --Fecha de transacci�n inicial
    p_f_transaccion_fin      DATE,         --Fecha de transacci�n final 
    p_tipo_credito           SMALLINT      --Tipo de cr�dito 

  DEFINE v_desc_edo_aps      CHAR(40)
  DEFINE v_cve_concepto      SMALLINT 

  DEFINE 
    v_entidad_financiera     CHAR(5), 
    v_estado                 CHAR(5)

  --Consulta del detalle principal
  LET g_sql_txt = "\n SELECT dc.cve_ent_financiera, ",
                  "\n        ef.ent_financiera_desc, ",
                  "\n        ef.clabe, ", 
                  "\n        '0000000000' AS documento_fico, ", --FALTA ESTE CAMPO 
                  "\n        dc.estado, ", 
                  "\n        ce.desc_edo_aps, ", 
                  "\n        SUM(imp_ap_pat) AS monto ",       
                  "\n FROM   dis_ctr_aps_tns dc, ",
                  "\n        glo_folio gf, ",
                  "\n        OUTER cat_edo_aps ce, ",
                  "\n        OUTER cat_cta_cnt_ocg ef ",
                  "\n WHERE  gf.folio              = dc.folio_transaccion  ",
                  "\n AND    dc.estado             = ce.cod_edo_aps        ",
                  "\n AND    dc.cve_ent_financiera = ef.cve_ent_financiera ",
                  "\n AND    dc.tpo_credito	       = ef.tpo_credito        ",
                  "\n AND    dc.estado             = 20                    ",    
                  "\n AND    gf.status             = 1                     ",     
                  "\n AND    gf.proceso_cod        = 3904                  "

  {IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dc.f_transaccion >= '",p_f_transaccion_ini,"'",
                                "\n AND dc.f_transaccion <= '",p_f_transaccion_fin,"'",
                                "\n AND gf.f_actualiza   >= '",p_f_transaccion_ini,"'",
                                "\n AND gf.f_actualiza   <= '",p_f_transaccion_fin,"'"
  END IF}  

  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt, "\n AND dc.folio_transaccion = ", p_folio_transaccion
  END IF                  
                  
  IF p_tipo_credito = 2 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 2 ",
                                "\n AND dc.concepto   IN (107,109) " 
  END IF
  
  IF p_tipo_credito = 3 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 3 ",
                                "\n AND dc.concepto   IN (307,407) " 
  END IF

  IF p_tipo_credito = 5 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 5 ",
                                "\n AND dc.concepto   IN (807,809) "   	 	 
  END IF
                  
  LET g_sql_txt = g_sql_txt,"\n GROUP BY 1,2,3,4,5,6 " -- ,7 "
                
  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE pr_sl_inf FROM g_sql_txt
  DECLARE cur_sl_inf CURSOR FOR pr_sl_inf
  
  LET v_indice1 = 1
  LET v_tot_det = 0.0 

  INITIALIZE v_entidad_financiera, v_estado TO NULL
  CALL a_fac_apo_sub.clear()
  FOREACH cur_sl_inf INTO --a_fac_apo_sub[v_indice1].cve_ent_financiera,
                          v_entidad_financiera,
                          a_fac_apo_sub[v_indice1].entidad_financiera,
                          a_fac_apo_sub[v_indice1].cuenta_bancaria,
                          a_fac_apo_sub[v_indice1].documento_fico,
                          v_estado,
                          a_fac_apo_sub[v_indice1].estado,
                          a_fac_apo_sub[v_indice1].monto

    LET a_fac_apo_sub[v_indice1].cve_ent_financiera = v_entidad_financiera CLIPPED
    LET a_fac_apo_sub[v_indice1].entidad_financiera = v_entidad_financiera USING "&&&",' - ',a_fac_apo_sub[v_indice1].entidad_financiera CLIPPED
    LET a_fac_apo_sub[v_indice1].estado             = v_estado CLIPPED,' - ',a_fac_apo_sub[v_indice1].estado CLIPPED
    --LET a_fac_apo_sub[v_indice1].facturar           = 0
    LET a_fac_apo_sub[v_indice1].facturar           = 1    
    LET v_tot_det                                   = v_tot_det + a_fac_apo_sub[v_indice1].monto

   --CALL fn_mensaje("",a_fac_apo_sub[v_indice1].entidad_financiera,"")
    
    LET v_indice1                                   = v_indice1 + 1
  END FOREACH                           
  
  CALL a_fac_apo_sub.deleteElement(v_indice1)
  LET v_indice1    = v_indice1 - 1  

  FREE cur_sl_inf

  -- Consulta del total por concepto
  LET g_sql_txt = "\n SELECT dc.concepto, ",
                  "\n        co.desc_concepto_ocg, ",
                  "\n        SUM (dc.imp_ap_pat) ", 
                  "\n FROM   dis_ctr_aps_tns dc, ",
                  "\n        glo_folio gf, ",
                  "\n        OUTER cat_concepto_ocg co ",
                  "\n WHERE  gf.folio       = dc.folio_transaccion ",
                  "\n AND   dc.concepto     = co.cod_concepto_ocg  ",
                  "\n AND    dc.estado      = 20 ",
                  "\n AND    gf.status      = 1  ",    
                  "\n AND    gf.proceso_cod = 3904 " --Falta modificar al que defina el usuario                  

  {IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dc.f_transaccion >= '",p_f_transaccion_ini,"'",
                                "\n AND dc.f_transaccion <= '",p_f_transaccion_fin,"'",
                                "\n AND gf.f_actualiza   >= '",p_f_transaccion_ini,"'",
                                "\n AND gf.f_actualiza   <= '",p_f_transaccion_fin,"'"
  END IF}  

  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dc.folio_transaccion = ",p_folio_transaccion
  END IF                  
                  
  IF p_tipo_credito = 2 THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 2 ",
                                "\n AND dc.concepto   IN (107,109) "
  END IF

  IF p_tipo_credito = 3 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 3 ",
                                "\n AND dc.concepto   IN (307,407) "
  END IF
  
  IF p_tipo_credito = 5 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 5 ",
                                "\n AND dc.concepto   IN (807,809) "   	 	 
  END IF

  LET g_sql_txt = g_sql_txt, "\n GROUP BY dc.concepto,  desc_concepto_ocg ", 
                             "\n ORDER BY dc.concepto "

  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE pr_sl_con FROM g_sql_txt
  DECLARE cur_sl_con CURSOR FOR pr_sl_con
  
  LET v_indice2 = 1
  LET v_tot_con = 0.0

  CALL a_fac_apo_con.clear()
  FOREACH cur_sl_con INTO v_cve_concepto,    
                          a_fac_apo_con[v_indice2].concepto,    
                          a_fac_apo_con[v_indice2].imp_ap_pat  

    LET a_fac_apo_con[v_indice2].concepto = v_cve_concepto CLIPPED, ' - ',a_fac_apo_con[v_indice2].concepto CLIPPED
    LET v_tot_con = v_tot_con + a_fac_apo_con[v_indice2].imp_ap_pat

    --DISPLAY "v_tot_con : -",v_tot_con,'-'
    
    LET v_indice2 = v_indice2 + 1

  END FOREACH                           
  
  CALL a_fac_apo_con.deleteElement(v_indice2)
  LET v_indice2    = v_indice2 - 1  

  FREE cur_sl_con
END FUNCTION  

FUNCTION fn_verificar_apo_sele()
  DEFINE v_bnd_ok            SMALLINT
  DEFINE i                   INTEGER

  LET v_bnd_ok = 0

  FOR i = 1 TO a_fac_apo_sub.getLength()
      IF a_fac_apo_sub[i].facturar = 1 THEN
         LET v_bnd_ok = 1
         EXIT FOR
      END IF
  END FOR

  RETURN v_bnd_ok
END FUNCTION

{FUNCTION fn_llen_tmp_fac_apo_sub(p_folio_transaccion, p_f_transaccion_ini, 
                                 p_f_transaccion_fin, p_tipo_credito, p_ent_financiera, 
                                 p_concepto, p_estado, p_num_ctr_int_ef)}
FUNCTION fn_llen_tmp_fac_apo_sub(p_folio_transaccion, p_tipo_credito, p_ent_financiera, 
                                 p_concepto, p_estado, p_num_ctr_int_ef)
  DEFINE 
    p_folio_transaccion      DECIMAL(9,0), --Folio de transacci�n
    --p_f_transaccion_ini      DATE,         --Fecha de transacci�n inicial
    --p_f_transaccion_fin      DATE,         --Fecha de transacci�n final 
    p_tipo_credito           SMALLINT,     --Tipo de Cr�dito         
    p_ent_financiera         SMALLINT,     --Clave de Entidad Financiera seleccionada
    p_concepto               SMALLINT,     --Concepto seleccionado
    p_estado                 SMALLINT,     --Estado seleeccionado
    p_num_ctr_int_ef         CHAR(18)      --Numero de Control Interno de la Entidad Financiera seleccionada 

  DEFINE i                   INTEGER

  LET g_sql_txt = "\n SELECT dc.* ",       
                  "\n FROM   dis_ctr_aps_tns dc, ",
                  "\n        glo_folio gf, ", 
                  "\n        OUTER cat_cta_cnt_ocg ef ",                 
                  "\n WHERE  gf.folio              = dc.folio_transaccion  ",
                  "\n AND    dc.cve_ent_financiera = ef.cve_ent_financiera ",
                  "\n AND    dc.tpo_credito	       = ef.tpo_credito        ",
                  "\n AND    gf.status             = 1    ",     
                  "\n AND    gf.proceso_cod        = 3904 " --Falta modificar al que defina el usuario                  

  IF p_tipo_credito = 2 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 2 ",
                                "\n AND dc.concepto   IN (107,109) "
  END IF
                  
  IF p_tipo_credito = 3 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 3 ",
                                "\n AND dc.concepto   IN (307,407) "
  END IF

  IF p_tipo_credito = 5 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 5 ",
                                "\n AND dc.concepto   IN (807,809) "   	 	 
  END IF
                
  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt, "\n AND dc.folio_transaccion = ",p_folio_transaccion
  END IF

  {IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dc.f_transaccion >= '",p_f_transaccion_ini,"'",
                                "\n AND dc.f_transaccion <= '",p_f_transaccion_fin,"'",
                                "\n AND gf.f_actualiza   >= '",p_f_transaccion_ini,"'",
                                "\n AND gf.f_actualiza   <= '",p_f_transaccion_fin,"'"
  END IF}

  IF p_ent_financiera IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.cve_ent_financiera =",p_ent_financiera
  END IF

  IF p_num_ctr_int_ef IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND ef.clabe =", p_num_ctr_int_ef
  END IF

  {IF p_concepto IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.concepto =",p_concepto
  END IF}

  IF p_estado IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.estado =",p_estado
  END IF  
                
  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"
                   
  PREPARE ps_fac_apo_sub FROM g_sql_txt
  DECLARE cur_fac_apo_sub CURSOR FOR ps_fac_apo_sub
   
  LET g_sql_txt = " INSERT INTO tmp_dis_fac_aps_tns VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
  PREPARE ps_ins_tmp_apo_sub FROM g_sql_txt
      
  LET i = 1
  
  FOREACH cur_fac_apo_sub INTO arr_fac_apo_sub[i].* 
    DISPLAY i, " - ", arr_fac_apo_sub[i].* 
    EXECUTE ps_ins_tmp_apo_sub USING arr_fac_apo_sub[i].* 
    LET i = i + 1
  END FOREACH
   
  CALL arr_fac_apo_sub.deleteElement(i)
END FUNCTION

{FUNCTION fn_llenar_tabla_tmp_fac(p_folio_transaccion, p_f_transaccion_ini, 
                                 p_f_transaccion_fin, p_tipo_credito)}
FUNCTION fn_llenar_tabla_tmp_fac(p_folio_transaccion, p_tipo_credito)
  DEFINE 
    p_folio_transaccion      DECIMAL(9,0), --Folio de transacci�n
    --p_f_transaccion_ini      DATE,         --Fecha de transacci�n inicial
    --p_f_transaccion_fin      DATE,         --Fecha de transacci�n final 
    p_tipo_credito           SMALLINT      --Tipo de Cr�dito         
    
  DEFINE i                   INTEGER

  LET g_sql_txt = "\n SELECT dc.* ",       
                  "\n FROM   dis_ctr_aps_tns dc, ",
                  "\n        glo_folio gf, ",
                  "\n        OUTER cat_edo_aps ce, ",
                  "\n        OUTER cat_cta_cnt_ocg ef ",
                  "\n WHERE  gf.folio              = dc.folio_transaccion ",
                  "\n AND    dc.estado             = ce.cod_edo_aps ",
                  "\n AND    dc.cve_ent_financiera = ef.cve_ent_financiera ",
                  "\n AND    dc.tpo_credito        = ef.tpo_credito        ",
                  "\n AND    dc.estado             = 20 ",    
                  "\n AND    gf.status             = 1 ",     
                  "\n AND    gf.proceso_cod        = 3904 "
                  
  IF p_tipo_credito = 2 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 2 ",
                                "\n AND dc.concepto   IN (107,109) "
  END IF
                                
  IF p_tipo_credito = 3 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 3 ",
                                "\n AND dc.concepto   IN (307,407) "
  END IF

  IF p_tipo_credito = 5 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 5 ",
                                "\n AND dc.concepto   IN (807,809) "   	 	 
  END IF
                
  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt, "\n AND dc.folio_transaccion = ",p_folio_transaccion
  END IF

  {IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dc.f_transaccion >= '",p_f_transaccion_ini,"'",
                                "\n AND dc.f_transaccion <= '",p_f_transaccion_fin,"'",
                                "\n AND gf.f_actualiza   >= '",p_f_transaccion_ini,"'",
                                "\n AND gf.f_actualiza   <= '",p_f_transaccion_fin,"'"
  END IF}
                
  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"
                   
  PREPARE ps_tmp_dis_fac_aps_tns FROM g_sql_txt
  DECLARE cur_tmp_dis_fac_aps_tns CURSOR FOR ps_tmp_dis_fac_aps_tns
   
  LET g_sql_txt = " INSERT INTO tmp_dis_fac_aps_tns VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
  PREPARE ps_ins_tmp_dis_fac FROM g_sql_txt
      
  LET i = 1
  
  FOREACH cur_tmp_dis_fac_aps_tns INTO arr_fac_apo_sub[i].* 
    --DISPLAY i, " - ", arr_fac_apo_sub[i].* 
    EXECUTE ps_ins_tmp_dis_fac USING arr_fac_apo_sub[i].* 
    LET i = i + 1
  END FOREACH
   
  CALL arr_fac_apo_sub.deleteElement(i)
END FUNCTION

--FUNCTION fn_llenar_ent_fin_bloq(p_folio_transaccion, p_f_transaccion_ini, p_f_transaccion_fin, p_tipo_credito)
FUNCTION fn_llenar_ent_fin_bloq(p_folio_transaccion, p_tipo_credito)
  DEFINE 
    p_folio_transaccion      DECIMAL(9,0), --Folio de transacci�n
    --p_f_transaccion_ini      DATE,         --Fecha de transacci�n inicial
    --p_f_transaccion_fin      DATE,         --Fecha de transacci�n final 
    p_tipo_credito           SMALLINT      --Tipo de cr�dito 

  DEFINE v_desc_edo_aps      CHAR(40)
  DEFINE v_cve_concepto      SMALLINT 

  --Consulta del detalle principal
  LET g_sql_txt = "\n SELECT dc.cve_ent_financiera, ",
                  "\n        ef.ent_financiera_desc, ",
                  "\n        ef.clabe, ", 
                  "\n        SUM(imp_ap_pat) AS monto ",       
                  "\n FROM   dis_ctr_aps_tns dc, ",
                  "\n        glo_folio gf, ",
                  "\n        OUTER cat_cta_cnt_ocg ef ",
                  "\n WHERE  gf.folio              = dc.folio_transaccion  ",
                  "\n AND    dc.cve_ent_financiera = ef.cve_ent_financiera ",
                  "\n AND    dc.tpo_credito        = ef.tpo_credito        ",
                  "\n AND    dc.estado             = 40   ",    
                  "\n AND    gf.status             = 1    ",     
                  "\n AND    gf.proceso_cod        = 3904 "

  {IF p_f_transaccion_ini IS NOT NULL AND p_f_transaccion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt, "\n AND dc.f_transaccion >= '",p_f_transaccion_ini,"'",
                                "\n AND dc.f_transaccion <= '",p_f_transaccion_fin,"'",
                                "\n AND gf.f_actualiza   >= '",p_f_transaccion_ini,"'",
                                "\n AND gf.f_actualiza   <= '",p_f_transaccion_fin,"'"
  END IF}  

  IF p_folio_transaccion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt, "\n AND dc.folio_transaccion = ",p_folio_transaccion
  END IF                  
                  
  IF p_tipo_credito = 2 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 2 ",
                                "\n AND dc.concepto   IN (107,109) "
  END IF
                                
  IF p_tipo_credito = 3 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 3 ",
                                "\n AND dc.concepto   IN (307,407) " 
  END IF

  IF p_tipo_credito = 5 THEN
     LET g_sql_txt = g_sql_txt, "\n AND dc.tpo_credito = 5 ",
                                "\n AND dc.concepto   IN (807,809) "   	 	 
  END IF
                  
  LET g_sql_txt = g_sql_txt,"\n GROUP BY 1,2,3 " -- ,7 "
                
  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE ps_ent_fin_bloq FROM g_sql_txt
  DECLARE cur_ent_fin_bloq CURSOR FOR ps_ent_fin_bloq
  
  LET v_indice3  = 1
  LET v_tot_bloq = 0.0 

  CALL a_ent_fin_bloq.clear()
  
  FOREACH cur_ent_fin_bloq INTO a_ent_fin_bloq[v_indice3].cve_ent_financiera,
                                a_ent_fin_bloq[v_indice3].entidad_financiera,
                                a_ent_fin_bloq[v_indice3].cuenta_bancaria,
                                --a_ent_fin_bloq[v_indice3].documento_fico,
                                --a_ent_fin_bloq[v_indice3].concepto,
                                --a_ent_fin_bloq[v_indice3].estado,
                                --v_desc_edo_aps,
                                a_ent_fin_bloq[v_indice3].monto

    --LET a_ent_fin_bloq[v_indice3].facturar           = 0  
    --LET a_ent_fin_bloq[v_indice3].estado             = a_fac_apo_sub[v_indice1].estado CLIPPED,' - ',v_desc_edo_aps CLIPPED
    LET a_ent_fin_bloq[v_indice3].entidad_financiera = a_ent_fin_bloq[v_indice3].cve_ent_financiera USING "&&&",' - ',a_ent_fin_bloq[v_indice3].entidad_financiera CLIPPED
    LET v_tot_bloq                                   = v_tot_bloq + a_ent_fin_bloq[v_indice3].monto
    LET v_indice3                                    = v_indice3 + 1
  END FOREACH                           
  
  CALL a_ent_fin_bloq.deleteElement(v_indice3)
  LET v_indice3    = v_indice3 - 1  

  FREE cur_ent_fin_bloq
END FUNCTION

FUNCTION fn_borra_crea_tmp_apo_sub()
  WHENEVER ERROR CONTINUE;
   
  DROP TABLE tmp_dis_fac_aps_tns;
  CREATE TABLE tmp_dis_fac_aps_tns (id_dis_interface_ef DECIMAL(9,0),
                                      id_derechohabiente  DECIMAL(9,0),
                                      folio_sua           DECIMAL(6,0),
                                      periodo_pago        CHAR(6),
                                      f_pago              DATE,
                                      nrp                 CHAR(11),
                                      ind_liquidacion     SMALLINT,
                                      folio_liquida       DECIMAL(9,0),
                                      f_liquida           DATE,
                                      num_crd_ifv         DECIMAL(10,0),
                                      imp_ap_pat          DECIMAL(12,2),
                                      aiv_ap_pat          DECIMAL(18,6),
                                      tpo_credito         SMALLINT,
                                      cve_ent_financiera  SMALLINT,
                                      num_ctr_int_ef      CHAR(18),
                                      concepto            SMALLINT,
                                      id_ctr_transaccion  DECIMAL(9,0),
                                      folio_transaccion   DECIMAL(9,0),
                                      f_transaccion       DATE,
                                      folio_factura		  DECIMAL(9,0),
                                      f_factura			  DATE,
                                      estado              SMALLINT);
                                              
  --DISPLAY "Se borr� y se cre� la tabla tmp_dis_fac_aps_tns"  
END FUNCTION