################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 10/03/2016                                      #
################################################################################
################################################################################
#Proyecto          => INFONAVIT                                                #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL56                                                   #
#Objetivo          => Consulta de Archivos Integrados                          #
#Fecha inicio      => 10/03/2016                                               #
################################################################################
DATABASE safre_viv

GLOBALS

   DEFINE g_sql_txt       STRING,      --Consultas
          g_usuario       VARCHAR(30), --Almacena al usuario
          g_proceso_cod   LIKE cat_proceso.proceso_cod, --codigo del proceso
          g_opera_cod     LIKE cat_operacion.opera_cod, --codigo de operacion
          g_pid           DECIMAL(9,0),
          g_folio         DECIMAL(9,0),
          opera_cod       INTEGER 
   
   DEFINE v_mensaje       STRING

   DEFINE f_ventana       ui.Window,   --Define las propìedades de la Ventana
          f_forma         ui.Form      --Define las propiedades de la forma
         
   DEFINE v_indice        SMALLINT
   DEFINE g_folio         DECIMAL(9,0)
   DEFINE v_ind_llena_cb  INTEGER

   DEFINE cb_diagnostico  SMALLINT 
   
   DEFINE v_arr_int DYNAMIC ARRAY OF RECORD
      fecha_carga    DATE, 
      folio          DECIMAL(10,0),
      tot_reg        INTEGER,
      aivs           DECIMAL(12,2),
      pesos          DECIMAL(15,6)
   END RECORD

   DEFINE v_arr_dup DYNAMIC ARRAY OF RECORD
      fecha_carga    DATE, 
      folio          DECIMAL(10,0),
      tot_reg        INTEGER,
      aivs           DECIMAL(12,2),
      pesos          DECIMAL(15,6)
   END RECORD

END GLOBALS

MAIN

   DEFINE v_tipo_proceso      SMALLINT,    --Forma como ejecutara el programa 
          v_nom_prog          VARCHAR(30), --Almacena opción del menú 
          v_nom_archivo       STRING       --Nombre del archivo          
    
   --Datos de entrada
   DEFINE p_folio             DECIMAL(10,0),
          p_f_ini_carga       DATE,
          p_f_fin_carga       DATE
 
   DEFINE bnd_consulta        SMALLINT
   DEFINE v_bnd_existe_int    INTEGER
   DEFINE v_bnd_existe_dup    INTEGER
   DEFINE tpo_ajuste SMALLINT
   DEFINE p_programa CHAR(10)     

   CALL STARTLOG (g_usuario CLIPPED|| ".DISL56.log")
          
   --Recibe valores de argumentos
   LET p_programa     = "DISL56"
   LET g_usuario      = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)

   
   LET g_proceso_cod  = 933
   LET g_opera_cod = 6
   LET tpo_ajuste = 0 

   LET v_bnd_existe_int = 0
   LET v_bnd_existe_dup = 0 

   --se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   LET v_nom_archivo = ""   
   LET bnd_consulta  = 0   

   CLOSE WINDOW SCREEN   

   OPEN WINDOW w1 WITH FORM "DISL561"
      DIALOG ATTRIBUTES(UNBUFFERED)      

      INPUT BY NAME p_folio, p_f_ini_carga, p_f_fin_carga
         ON ACTION ACCEPT
            IF p_f_ini_carga IS NOT NULL AND p_f_ini_carga IS NULL THEN 
               CALL fn_mensaje("Aviso","Debe seleccionar la fecha final","stop")
               NEXT FIELD p_f_final 
            END IF
         
            IF p_f_ini_carga IS NULL AND p_f_fin_carga IS NOT NULL THEN 
               CALL fn_mensaje("Aviso","Debe seleccionar la fecha inicial","stop")
               NEXT FIELD p_f_inicial 
            END IF

            IF p_folio IS NULL AND p_f_ini_carga IS NULL AND p_f_fin_carga IS NULL THEN
               CALL fn_mensaje("Aviso","Debe seleccionar algún criterio de búsqueda","stop") 
            ELSE

            IF p_f_ini_carga > p_f_fin_carga THEN
               CALL fn_mensaje("Aviso","La fecha inicial de carga no puede ser mayor a la fecha final de carga","stop")
            END IF
            
            LET v_bnd_existe_int = 0
            LET v_bnd_existe_dup = 0
            CALL fn_consultar_info(p_folio, p_f_ini_carga, p_f_fin_carga) RETURNING v_bnd_existe_int, v_bnd_existe_dup

            --DISPLAY "v_bnd_existe_int: ",v_bnd_existe_int
            --DISPLAY "v_bnd_existe_dup: ",v_bnd_existe_dup
            
            IF v_bnd_existe_int >= 1 THEN
               CALL f_forma.setElementHidden("gr_res_int",0)  --Muestra el resultado de la consulta
               DISPLAY ARRAY v_arr_int TO r_res_int.*
                  ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)

                  BEFORE DISPLAY 
                    IF v_bnd_existe_dup >= 1 THEN
                       CALL f_forma.setElementHidden("gr_res_dup",0)  --Muestra el resultado de la consulta
                       DISPLAY ARRAY v_arr_dup TO r_res_dup.* 
                          ATTRIBUTES(ACCEPT=FALSE, CANCEL=FALSE)
                          BEFORE DISPLAY
                             EXIT DISPLAY
                       END DISPLAY
                    END IF
                  
                  ON ACTION cancelar 
                     EXIT PROGRAM

                  ON ACTION reporte
                     CALL fn_generar_reporte(p_folio, p_f_ini_carga, p_f_fin_carga)                  
                     
               END DISPLAY                

            ELSE
               CALL fn_mensaje("Aviso","No hay registros disponibles.","stop") 
               EXIT PROGRAM
            END IF
            END IF 

            BEFORE INPUT 
               LET f_ventana = ui.Window.getCurrent()
               LET f_forma   = f_ventana.getForm()

               CALL f_forma.setElementHidden("gr_res_int",1)  --Oculta el resultado de la consulta
               CALL f_forma.setElementHidden("gr_res_dup",1)  --Oculta el resultado de la consulta
        
      END INPUT
          
      
      ON ACTION cancelar
         EXIT DIALOG
          
    END DIALOG 
  CLOSE WINDOW w1 

END MAIN 


FUNCTION fn_consultar_info(v_folio, v_f_ini_carga, v_f_fin_carga) 
   DEFINE v_folio             DECIMAL(10,0),
          v_f_ini_carga       DATE,
          v_f_fin_carga       DATE

   DEFINE i_int INTEGER,
          i_dup INTEGER

  -- Obtiene las cifras globales de los registros integrados
  LET g_sql_txt = "\n    SELECT gr.f_actualiza, ",
                  "\n           ds.folio_ap_subs, ", 
                  "\n           COUNT(ds.id) AS tot_reg, ", 
                  "\n           NVL(SUM(ds.imp_apo_aivs),0) AS imp_apo_aivs, ", 
                  "\n           NVL(SUM(ds.imp_apo_pat),0) AS imp_apo_pat ", 
                  "\n      FROM dis_as_sin_conciliar ds, ", 
                  "\n           glo_ctr_archivo gr ", 
                  "\n     WHERE ds.ind_concilia IN (2,3,4) ",
                  "\n       AND ds.folio_ap_subs = gr.folio "

   IF v_folio IS NOT NULL THEN
      LET g_sql_txt = g_sql_txt, "\n       AND ds.folio_ap_subs = ",v_folio
   END IF 

   IF v_f_ini_carga IS NOT NULL AND v_f_fin_carga IS NOT NULL THEN 
      LET g_sql_txt = g_sql_txt, "\n       AND gr.f_actualiza >= '",v_f_ini_carga,"'"
      LET g_sql_txt = g_sql_txt, "\n       AND gr.f_actualiza <= '",v_f_fin_carga,"'"
   END IF

   LET g_sql_txt = g_sql_txt, "\n  GROUP BY ds.folio_ap_subs, ",
                              "\n           gr.f_actualiza "

   --DISPLAY "g_sql_txt: -",g_sql_txt
   PREPARE ps_int FROM g_sql_txt

  -- Obtiene las cifras globales de los registros duplicados
  LET g_sql_txt = "\n   SELECT gr.f_actualiza, ",
                  "\n          dd.folio_ap_subs, ", 
                  "\n          NVL(SUM(tot_apo_subs),0) AS tot_reg, ", 
                  "\n          NVL(SUM(imp_aivs),0) AS imp_apo_aivs, ",
                  "\n          NVL(SUM(imp_pesos),0) AS imp_apo_pat ", 
                  "\n     FROM dis_dup_ap_sc dd, ",
                  "\n          glo_ctr_archivo gr ",
                  "\n    WHERE dd.folio_ap_subs = gr.folio "

   IF v_folio IS NOT NULL THEN
      LET g_sql_txt = g_sql_txt, "\n       AND dd.folio_ap_subs = ",v_folio
   END IF 

   IF v_f_ini_carga IS NOT NULL AND v_f_fin_carga IS NOT NULL THEN 
      LET g_sql_txt = g_sql_txt, "\n       AND gr.f_actualiza >= '",v_f_ini_carga,"'"
      LET g_sql_txt = g_sql_txt, "\n       AND gr.f_actualiza <= '",v_f_fin_carga,"'"
   END IF

   LET g_sql_txt = g_sql_txt, "\n GROUP BY dd.folio_ap_subs, ",
                              "\n          gr.f_actualiza "

   --DISPLAY "g_sql_txt: -",g_sql_txt
   PREPARE ps_dup FROM g_sql_txt   

   DECLARE cur_int CURSOR FOR ps_int

   LET i_int = 1

   FOREACH cur_int INTO v_arr_int[i_int].fecha_carga, 
                        v_arr_int[i_int].folio,
                        v_arr_int[i_int].tot_reg,
                        v_arr_int[i_int].aivs,
                        v_arr_int[i_int].pesos
                               
      LET i_int = i_int + 1
   END FOREACH

   LET i_int = i_int - 1   
   CALL v_arr_int.deleteElement(v_arr_int.getLength()) 

   CLOSE cur_int
   FREE cur_int

   DECLARE cur_dup CURSOR FOR ps_dup

   LET i_dup = 1

   FOREACH cur_dup INTO v_arr_dup[i_dup].fecha_carga, 
                        v_arr_dup[i_dup].folio,
                        v_arr_dup[i_dup].tot_reg,
                        v_arr_dup[i_dup].aivs,
                        v_arr_dup[i_dup].pesos
                               
      LET i_dup = i_dup + 1
   END FOREACH

   LET i_dup = i_dup - 1   
   CALL v_arr_dup.deleteElement(v_arr_dup.getLength()) 

   CLOSE cur_dup
   FREE cur_dup

   RETURN i_int, i_dup
END FUNCTION


#Objetivo: Genera reporte en pdf de la Bitácora de Validación Telefónica
FUNCTION fn_generar_reporte(v_folio, v_f_ini_carga, v_f_fin_carga) 
   DEFINE v_folio         DECIMAL(10,0),
          v_f_ini_carga   DATE,
          v_f_fin_carga   DATE, 
          v_tp_rep        SMALLINT

  DEFINE  manejador_rpt      om.SaxDocumentHandler, --Contenedor documentos reporte
          v_rep_indice       INTEGER,
          v_fecha_actual     DATE
  
  LET v_rep_indice = 1  
  
  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISL561.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  LET v_fecha_actual = TODAY  

  --Inicia el reporte de registros con rechazo
  START REPORT rep_bitacora TO XML HANDLER manejador_rpt  

  LET v_tp_rep = 1 
  
  FOR v_rep_indice = 1 TO  v_arr_int.getLength()
     
    OUTPUT TO REPORT rep_bitacora(g_usuario,
                                  v_fecha_actual,
                                  v_folio, 
                                  v_f_ini_carga,
                                  v_f_fin_carga,                                     
                                  v_arr_int[v_rep_indice].*,
                                  v_rep_indice,
                                  v_tp_rep)
     END FOR 
     
  LET v_tp_rep = 2 
  
  FOR v_rep_indice = 1 TO  v_arr_dup.getLength()
     
    OUTPUT TO REPORT rep_bitacora(g_usuario,
                                  v_fecha_actual,
                                  v_folio, 
                                  v_f_ini_carga,
                                  v_f_fin_carga,                                     
                                  v_arr_dup[v_rep_indice].*,
                                  v_rep_indice, 
                                  v_tp_rep)
  END FOR 
  
  FINISH REPORT rep_bitacora
END FUNCTION



#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_bitacora(g_usuario,
                    v_fecha_actual,
                    v_folio, 
                    v_f_ini_carga,
                    v_f_fin_carga,                                     
                    v_arr_int_dup,
                    v_rep_indice, 
                    v_tp_rep)

   DEFINE v_folio        DECIMAL(10,0),
          v_f_ini_carga  DATE,
          v_f_fin_carga  DATE

   DEFINE v_arr_int_dup RECORD
      fecha_carga    DATE, 
      folio          DECIMAL(10,0),
      tot_reg        INTEGER,
      aivs           DECIMAL(12,2),
      pesos          DECIMAL(15,6)
   END RECORD

   DEFINE v_rep_indice  INTEGER,
          v_tp_rep      SMALLINT

  DEFINE g_usuario         VARCHAR(30), --Almacena al usuario
         v_fecha_actual    DATE

  FORMAT

   FIRST PAGE HEADER
      PRINTX g_usuario
      PRINTX v_fecha_actual USING "dd-mm-yyyy"
      PRINTX v_folio
      PRINTX v_f_ini_carga USING "dd-mm-yyyy"
      PRINTX v_f_ini_carga USING "dd-mm-yyyy"      

   BEFORE GROUP OF v_tp_rep
      PRINTX v_rep_indice

   ON EVERY ROW
      PRINTX v_arr_int_dup.fecha_carga USING "dd-mm-yyyy"
      PRINTX v_arr_int_dup.folio
      PRINTX v_arr_int_dup.tot_reg
      PRINTX v_arr_int_dup.aivs
      PRINTX v_arr_int_dup.pesos
      PRINTX v_tp_rep

   ON LAST ROW 
      PRINTX v_rep_indice

END REPORT