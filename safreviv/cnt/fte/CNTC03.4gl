################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 28/05/2012                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTC03                                                   #
#Objetivo          => Programa de consulta y generación del reporte del cuadro #
#                     de validación por procesos                               #
#Fecha inicio      => 15/05/2012                                               #
################################################################################
IMPORT os
DATABASE
   safre_viv
GLOBALS "CNTG01.4gl"
GLOBALS
CONSTANT l_proc_tbl = "scr_procesos" -- Tabla cuentas del periodo
CONSTANT l_proc_tbl_con = "scr_procesos_cons" -- Tabla de cuantas a consultar
DEFINE 
   g_arr_proceso  DYNAMIC ARRAY OF VARCHAR(10),  
   v_arr_proceso_cons  DYNAMIC ARRAY OF VARCHAR(10),
   
   v_arr_valida_proceso DYNAMIC ARRAY OF RECORD
         f_liquida         DATE,
         folio_liquida     DECIMAL (9,0)  ,
         folio             DECIMAL (9,0)  ,
         doc_contable      DECIMAL(10,0) ,--LIKE cnt_ctr_proceso.num_poliza,
         importe1          DECIMAL(22,2),
         importe2          DECIMAL(22,2),
         importe3          DECIMAL(22,2),
         importe4          DECIMAL(22,2),
         importe5          DECIMAL(22,2),
         v_valor_importe   DECIMAL(22,2),
         v_cuenta_contable LIKE  cnt_transaccion.cta_contable
   END RECORD,

   v_arr_valida_proceso_agrupado DYNAMIC ARRAY OF RECORD
         f_liquida         DATE,
         folio_liquida     DECIMAL (9,0)  ,
         folio             DECIMAL (9,0)  ,
         doc_contable      DECIMAL(10,0) ,--LIKE cnt_ctr_proceso.num_poliza,
         importe1          DECIMAL(22,2),
         importe2          DECIMAL(22,2),
         importe3          DECIMAL(22,2),
         importe4          DECIMAL(22,2),
         importe5          DECIMAL(22,2),
         v_valor_importe   DECIMAL(22,2),
         v_cuenta_contable LIKE  cnt_transaccion.cta_contable
   END RECORD,

   
   v_aux_valida_proceso  DYNAMIC ARRAY OF DECIMAL(22,2)

   DEFINE v_desc_proceso      VARCHAR (70) 

   DEFINE 
      f_folio_cnt       LIKE cnt_transaccion.folio_cnt,
      f_folio_liquida   LIKE cnt_transaccion.folio_liquida,
      v_query_cnt       STRING --Consulta

   --Totales de las cuentas contables
   DEFINE 
      v_total1          DECIMAL(22,2),
      v_total2          DECIMAL(22,2),
      v_total3          DECIMAL(22,2),
      v_total4          DECIMAL(22,2),
      v_total5          DECIMAL(22,2)

   --Banderas para indicar si se muestran o no los totales
   DEFINE 
      v_bnd1          SMALLINT,
      v_bnd2          SMALLINT,
      v_bnd3          SMALLINT,
      v_bnd4          SMALLINT,
      v_bnd5          SMALLINT

   DEFINE 
      f_fecha       DATE,
      f_fecha_fin   DATE
    
END GLOBALS

MAIN  
  CALL fn_main_cntc03()-- Función que inicia la  forma
END MAIN

FUNCTION fn_main_cntc03()
DEFINE 
    i  INTEGER ,
   l_dnd         ui.DragDrop, -- manejador del (drag and drop)
   l_drag_source STRING,      -- fuente del drag
   l_drag_index  INTEGER,     -- indice del drag
   l_drop_index  INTEGER,     -- indice del drop
   l_drag_value  STRING,       -- valor del drag
   f_ventana     ui.Window,   -- Define las propìedades de la Ventana
   f_forma       ui.Form,     -- Define las propiedades de la forma

   v_registros   SMALLINT 
   
-- se asignan los parametros que vienen del fglrun
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog     = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF
   
   CLOSE WINDOW SCREEN
   OPEN WINDOW wMain WITH FORM "CNTC031"
   DIALOG ATTRIBUTES (UNBUFFERED)
       --INPUT v_cmb_proceso, f_fecha FROM v_cmb_proceso, f_fecha
       INPUT BY NAME v_cmb_proceso, f_fecha, f_fecha_fin,f_folio_cnt, f_folio_liquida
          BEFORE INPUT          
             CALL fn_oculta_camposInicio() 
             LET f_ventana = ui.Window.getCurrent()
             LET f_forma = f_ventana.getForm()
             CALL fn_llena_combo_proceso()
             CALL DIALOG.setActionHidden("consultar",1)
             CALL DIALOG.setActionHidden("reporte",1)
             CALL DIALOG.setActionHidden("archivo",1)
             --Oculta cuadros de totales
             CALL f_forma.setFieldHidden("v_total1",TRUE)
             CALL f_forma.setFieldHidden("v_total2",TRUE) 
             CALL f_forma.setFieldHidden("v_total3",TRUE)
             CALL f_forma.setFieldHidden("v_total4",TRUE)
             CALL f_forma.setFieldHidden("v_total5",TRUE)   
           

             LET v_query_cnt = "" --Inicializa 
             
             ON ACTION ACCEPT

                  IF (v_cmb_proceso IS NULL) AND (f_fecha IS NULL) AND (f_fecha_fin IS NULL) AND (f_folio_cnt IS NULL) AND (f_folio_liquida IS NULL) THEN
                     CALL fn_mensaje("Error","Ingrese al menos un criterio de búsqueda","information")
                     NEXT FIELD v_cmb_proceso
                  END IF 

                  IF v_cmb_proceso IS NOT NULL THEN 
                     LET v_query_cnt = v_query_cnt,"\n AND T.cod_proceso_cnt = ",v_cmb_proceso
                  END IF

                  IF f_folio_cnt IS NOT NULL THEN 
                     LET v_query_cnt = v_query_cnt,"\n AND T.folio_cnt = ",f_folio_cnt
                  END IF 

                  --Valida que se capture el folio de liquidación 
                  IF f_folio_liquida IS NOT NULL THEN 
                     LET v_query_cnt = v_query_cnt,"\n AND T.folio_liquida = ",f_folio_liquida
                  END IF 

                  --Valida que se capturen ambas fechas
                  IF f_fecha IS NOT NULL AND f_fecha_fin IS NULL THEN
                     CALL fn_mensaje("Periodo inválido","Debe ingresar ambas fechas para la búsqueda","information")
                     NEXT FIELD f_fecha_fin
                  END IF  

                  --Valida que se capturen ambas fechas
                  IF f_fecha IS NULL AND f_fecha_fin IS NOT NULL THEN
                     CALL fn_mensaje("Periodo inválido","Debe ingresar ambas fechas para la búsqueda","information")
                     NEXT FIELD f_fecha
                  END IF  
                  
                  IF f_fecha IS NOT NULL AND f_fecha_fin IS NOT NULL THEN 
                     LET v_query_cnt = v_query_cnt,"\n AND T.f_liquida BETWEEN '",f_fecha,"' AND '",f_fecha_fin,"'"
                  END IF
                  
                  DISPLAY v_query_cnt
                  
                  CALL g_arr_proceso.clear()
                  CALL v_arr_proceso_cons.clear()
                  CALL fn_limpia_encabezado ()
                  CALL v_arr_valida_proceso.clear()
  
                   --Realiza la consulta por proceso y/o fecha
                  CALL fn_cuentas_proceso (v_cmb_proceso,f_fecha)
               

                IF g_arr_proceso.getLength()=0 THEN
                  CALL fn_oculta_campos ("grp_procesos",1,1)
                  CALL fn_oculta_campos ("grp_procesos_cons",1,1)
                  CALL fn_mensaje("ATENCIÓN","No se encontraron cuentas para este proceso","about")
                  LET v_query_cnt = ""
                ELSE    
                   CALL fn_oculta_campos ("grp_procesos",0,1)
                   CALL fn_oculta_campos ("grp_procesos_cons",0,1)
                END IF 

                --LET v_query_cnt = "" --Inicializa 
       END INPUT 

       --Tabla de cuentas contables
       DISPLAY ARRAY g_arr_proceso TO scr_procesos.* 
       
          ON DRAG_START(l_dnd)
             LET l_drag_source = l_proc_tbl
             LET l_drag_index = arr_curr()
             LET l_drag_value = g_arr_proceso[l_drag_index]
          ON DRAG_FINISHED(l_dnd)
             INITIALIZE l_drag_source TO NULL

          ON DRAG_ENTER(l_dnd)
             IF l_drag_source IS NULL THEN
                CALL l_dnd.setOperation(NULL)
             END IF
     
          ON DROP(l_dnd)   
             IF v_arr_proceso_cons.getLength()=0 THEN
                CALL DIALOG.setActionHidden("consultar",1)
             ELSE
                CALL DIALOG.setActionHidden("consultar",0)
             END IF        
            
             IF l_drag_source == l_proc_tbl THEN
                CALL l_dnd.dropInternal()
             ELSE
                LET l_drop_index = l_dnd.getLocationRow()
                CALL DIALOG.insertRow(l_proc_tbl, l_drop_index)
                CALL DIALOG.setCurrentRow(l_proc_tbl, l_drop_index)
                LET g_arr_proceso[l_drop_index] = l_drag_value
                CALL DIALOG.deleteRow(l_proc_tbl_con, l_drag_index)
                
                IF v_arr_proceso_cons.getLength()=0 THEN --si no  hay  procesos  seleccionados oculta el boton Consultar
                   CALL DIALOG.setActionHidden("consultar",1)             
                END IF 
             END IF
       END DISPLAY 

       --Tabla de cuentas contbales a consultar
       DISPLAY ARRAY v_arr_proceso_cons TO  scr_procesos_cons.*
          ON DRAG_START(l_dnd)
             LET l_drag_source = l_proc_tbl_con
             LET l_drag_index = arr_curr()
             LET l_drag_value = v_arr_proceso_cons[l_drag_index]

          ON DRAG_FINISHED(l_dnd)
             INITIALIZE l_drag_source TO NULL

          ON DRAG_ENTER(l_dnd)
             IF l_drag_source IS NULL THEN
                CALL l_dnd.setOperation(NULL)
             END IF

          ON DROP(l_dnd)
          
             
             IF v_arr_proceso_cons.getLength()==5 THEN 
                    CALL fn_mensaje("ATENCIÓN","Solo se permiten  5 procesos","about")
                 ELSE    
                 IF l_drag_source == l_proc_tbl_con THEN
                    CALL l_dnd.dropInternal()
                 ELSE
                    LET l_drop_index = l_dnd.getLocationRow()
                    CALL DIALOG.insertRow(l_proc_tbl_con, l_drop_index)
                    CALL DIALOG.setCurrentRow(l_proc_tbl_con, l_drop_index)
                    LET v_arr_proceso_cons[l_drop_index] = l_drag_value
                    CALL DIALOG.deleteRow(l_proc_tbl, l_drag_index)
                 END IF
                 --AFTER DISPLAY 
                 
                 
                 IF v_arr_proceso_cons.getLength()>0 THEN --si hay  procesos  seleccionados muestra el botón Consultar
                    CALL DIALOG.setActionHidden("consultar",0)             
                 END IF 
             END IF --
                   
       END DISPLAY 

       DISPLAY ARRAY v_arr_valida_proceso_agrupado TO scr_valida_procesos.* 
           
       END DISPLAY

       ON  ACTION Cancelar 
             EXIT DIALOG 
       
       ON ACTION Reporte
          --CALL fn_mensaje("ATENCIÓN","Reporte en  construcción","about")
          --ON ACTION reporte
          CALL fn_reporte_valida_proceso(v_arr_valida_proceso_agrupado,v_arr_proceso_cons, f_fecha)
          
      ON ACTION archivo
         CALL fn_genera_archivo_detalle_folio()
          
       ON ACTION Consultar   
          CALL DIALOG.setActionHidden("consultar",1)
          CALL DIALOG.setActionHidden("reporte",0)
          CALL DIALOG.setActionHidden("archivo",0)          
          CALL fn_oculta_campos ("grp_valida_proceso",0,1) 
          CALL fn_nombre_encabezado()--- Asigna  titulos  al cuadro validción  por proceso
          CALL fn_valida_proceso() RETURNING v_registros--Obtiene  la información  que se muestra  en el cuadro validción  por proceso  


          CALL fn_agrupa_cuentas()

          DISPLAY "v_registros -- $$ ",v_registros
          IF v_registros <= 1 THEN
            NEXT FIELD v_cmb_proceso
          ELSE 
               --Muestra cuadros de totales
               IF v_bnd1 = 1 THEN 
                  CALL f_forma.setFieldHidden("v_total1",FALSE) 
               END IF  
               IF v_bnd2 = 1 THEN 
                  CALL f_forma.setFieldHidden("v_total2",FALSE) 
               END IF  
               IF v_bnd3 = 1 THEN 
                  CALL f_forma.setFieldHidden("v_total3",FALSE) 
               END IF  
               IF v_bnd4 = 1 THEN 
                  CALL f_forma.setFieldHidden("v_total4",FALSE) 
               END IF  
               IF v_bnd5 = 1 THEN 
                  CALL f_forma.setFieldHidden("v_total5",FALSE) 
               END IF  
              
          END IF 

          
   END DIALOG 
   
  { MENU 
   ON  ACTION CLOSE 
   EXIT MENU 
   END MENU }
    
   CLOSE WINDOW wMain
END FUNCTION


--Obtiene el listado de procesos por fecha y periodo
FUNCTION fn_cuentas_proceso(v_cmb_proceso, f_fecha)
   DEFINE 
      v_cmb_proceso     LIKE cat_proceso_cnt.cod_proceso_cnt,
      f_fecha           DATE,
      consulta          STRING,
      indx              INT 

      --DISPLAY "Proceso " , v_cmb_proceso
      --DISPLAY "Fecha ", f_fecha

      LET  consulta =   "\n SELECT DISTINCT T.cta_contable ",
                        "\n   FROM  cnt_transaccion T",
                        "\n      WHERE   1=1 "

                        
      {IF (v_cmb_proceso IS NOT NULL) AND (f_fecha IS NOT NULL) AND (f_folio_cnt IS NOT NULL) AND (f_folio_liquida IS NOT NULL) THEN 
         LET consulta = consulta || " T.cod_proceso_cnt = ", v_cmb_proceso,
                        " AND T.f_emision = '",f_fecha,"'",
                        " AND T.folio_cnt = ",f_folio_cnt,
                        " AND T.folio_liquida = ",f_folio_liquida
      END IF 
      
      IF (v_cmb_proceso IS NOT NULL) AND (f_fecha IS NULL) THEN 
         LET consulta = consulta || " T.cod_proceso_cnt = ", v_cmb_proceso
      END IF 

      IF (v_cmb_proceso IS NULL) AND (f_fecha IS NOT NULL) THEN 
         LET consulta = consulta || " T.f_emision = '",f_fecha,"'"
      END IF }

      LET consulta = consulta||v_query_cnt

      DISPLAY "La consulta: ", consulta

      LET indx = 1
    DECLARE cuenta_contable CURSOR FROM consulta
    FOREACH cuenta_contable INTO g_arr_proceso[indx]
        LET indx = indx + 1
    END FOREACH
    CALL g_arr_proceso.deleteElement(g_arr_proceso.getLength())
      
   
END FUNCTION 



FUNCTION fn_oculta_campos (campo,valor,tipo)
   DEFINE valor SMALLINT
   DEFINE  tipo INTEGER 
   DEFINE campo STRING
   DEFINE win ui.Window, fm ui.Form
   LET win = ui.Window.getCurrent()
   LET fm = win.getForm()
  
      IF tipo =1 THEN 
         CALL fm.setElementHidden(campo, valor)
      END IF 
      IF  tipo =2 THEN
      --CALL fm.setFieldHidden("customer.custid",1)
      CALL fm.setElementHidden("label_custid",1)
      END  IF 
  
END FUNCTION


FUNCTION fn_oculta_camposInicio()   
   CALL fn_oculta_campos ("grp_procesos",1,1)
   CALL fn_oculta_campos ("grp_procesos_cons",1,1)
   CALL fn_oculta_campos ("grp_valida_proceso",1,1)

   
END FUNCTION 

FUNCTION fn_nombre_encabezado ()--- Asigna  titulos  al cuadro validción  por proceso
   DEFINE win ui.Window, fm ui.Form,
          v_incremento INTEGER,
          v_titulo    STRING 
   LET win = ui.Window.getCurrent()
   LET fm = win.getForm()    
    FOR v_incremento=1 TO v_arr_proceso_cons.getLength()
       LET  v_titulo = "formonly.tb_importe"||v_incremento
       CALL fm.setElementText(v_titulo,v_arr_proceso_cons[v_incremento])
    END FOR 
    
    
END FUNCTION
FUNCTION fn_limpia_encabezado ()--- Limpia los titulos  del cuadro validción  por proceso
   DEFINE win ui.Window, fm ui.Form,
          v_incremento INTEGER,
          v_titulo    STRING 
   LET win = ui.Window.getCurrent()
   LET fm = win.getForm()    
    FOR v_incremento=1 TO 5
    --DISPLAY "limpiando "
       LET  v_titulo = "formonly.tb_importe"||v_incremento
       CALL fm.setElementText(v_titulo," " )
    END FOR 
END FUNCTION 
FUNCTION fn_valida_proceso() --Obtiene  listado para llenar el cuadro de validación por procesos
   DEFINE 
      --v_cod_proceso LIKE cat_proceso_cnt.cod_proceso_cnt,--Número de proceso
      consulta STRING,
      indx INT  ,
      v_incremento INT,
      v_inicio       SMALLINT,
      v_final        SMALLINT,
      v_tpo_cuenta   SMALLINT  

      LET v_inicio = 1
      LET v_final = v_arr_proceso_cons.getLength()
      DISPLAY "Final --X ",v_final

      --Inicializa variables de totales
      LET v_total1 = 0.00
      LET v_total2 = 0.00
      LET v_total3 = 0.00
      LET v_total4 = 0.00
      LET v_total5 = 0.00

      --Inicializa banderas, 0 no se muestra; 1 se activa y se muestra el campo
      LET v_bnd1 = 0
      LET v_bnd2 = 0
      LET v_bnd3 = 0
      LET v_bnd4 = 0
      LET v_bnd5 = 0
      

    FOR   v_incremento=1 TO 5
       IF v_arr_proceso_cons[v_incremento] IS  NULL THEN 
          LET v_arr_proceso_cons[v_incremento]=0
       END IF 
    END FOR 
    --Consulta para la el cuadro de Validación por proceso
    {LET  consulta =  "\n SELECT P.folio_cnt, P.num_poliza, SUM(T.importe), T.cta_contable ",
                     "\n FROM   cnt_transaccion T, cnt_ctr_proceso P ",
                     "\n WHERE  T.folio_cnt = P.folio_cnt ",
                     "\n AND    T.cta_contable in ('",v_arr_proceso_cons[1],"',","'",
                                                      v_arr_proceso_cons[2],"',","'",
                                                      v_arr_proceso_cons[3],"',","'",
                                                      v_arr_proceso_cons[4],"',","'",
                                                      v_arr_proceso_cons[5],"')"}

   LET  consulta =   "\n SELECT T.f_liquida, T.folio_liquida, T.folio_cnt, P.num_poliza, SUM(T.importe), T.cta_contable, T.cod_naturaleza_cta ",
                     "\n FROM cnt_transaccion T LEFT OUTER JOIN cnt_ctr_proceso P ",
                     "\n ON T.folio_liquida = P.folio_liquida ",
                     "\n AND T.folio_cnt = P.folio_cnt ",
                     "\n WHERE    T.cta_contable in ('",v_arr_proceso_cons[1],"',","'",
                                                      v_arr_proceso_cons[2],"',","'",
                                                      v_arr_proceso_cons[3],"',","'",
                                                      v_arr_proceso_cons[4],"',","'",
                                                      v_arr_proceso_cons[5],"')"

   DISPLAY "v_query_cnt -- ",v_query_cnt                                                       
   LET  consulta = consulta||v_query_cnt --Se agregan criterios de búsqueda también a la consulta

   --Pasarle filtros de entrada
   --

   LET consulta = consulta || " \n GROUP BY 1,2,3,4,6,7 ORDER BY 1,2 ASC"

   DISPLAY  "Cunsulta detalle: \n",consulta
    --CALL usuarios.clear()
    LET indx = 1
    DECLARE valida_proceso CURSOR FROM consulta 
    FOREACH valida_proceso  INTO v_arr_valida_proceso[indx].f_liquida,
                                 v_arr_valida_proceso[indx].folio_liquida,
                                 v_arr_valida_proceso[indx].folio,   
                                 v_arr_valida_proceso[indx].doc_contable,
                                 v_arr_valida_proceso[indx].v_valor_importe,                
                                 v_arr_valida_proceso[indx].v_cuenta_contable,
                                 v_tpo_cuenta -- 2 es cargo, 1 es abono

    --Si la naturaleza es Abono (1), entonces hace el importe negativo 
    IF v_tpo_cuenta = 1 THEN 
         LET  v_arr_valida_proceso[indx].v_valor_importe = (v_arr_valida_proceso[indx].v_valor_importe * -1)
    END IF 
                                 
    IF v_arr_valida_proceso[indx].doc_contable = 0 OR v_arr_valida_proceso[indx].doc_contable IS NULL THEN 
         LET v_arr_valida_proceso[indx].doc_contable = 0
    END IF 

    IF v_arr_valida_proceso[indx].folio = 0 OR v_arr_valida_proceso[indx].folio IS NULL THEN 
         LET v_arr_valida_proceso[indx].folio = 0
    END IF 
                                 
   --Compara cuentas para asignar los importes a la tabla; compara los encabezados
   --contra las cuentas de la consulta
   DISPLAY "importe -- ", v_arr_valida_proceso[indx].v_valor_importe

   
         FOR v_inicio = 1 TO v_final
            --DISPLAY "v_arr_proceso_cons[v_inicio] ------",v_arr_proceso_cons[v_inicio]
            --DISPLAY "v_arr_valida_proceso[indx].v_cuenta_contable -------",v_arr_valida_proceso[indx].v_cuenta_contable

            IF v_inicio = 1 THEN 
               IF v_arr_proceso_cons[v_inicio] = v_arr_valida_proceso[indx].v_cuenta_contable THEN 
                  LET v_arr_valida_proceso[indx].importe1 = v_arr_valida_proceso[indx].v_valor_importe
                  LET v_total1 = v_total1 + v_arr_valida_proceso[indx].v_valor_importe
                  DISPLAY "v_total1 -- ",v_total1
                  
                  LET v_bnd1 = 1 --Se activa la bandera cuando se registre un importe
               ELSE 
                  LET v_arr_valida_proceso[indx].importe1 = 0.00
                  
               END IF
            END IF 

            IF v_inicio = 2 THEN 
               IF v_arr_proceso_cons[v_inicio] = v_arr_valida_proceso[indx].v_cuenta_contable THEN 
                  LET v_arr_valida_proceso[indx].importe2 = v_arr_valida_proceso[indx].v_valor_importe
                  LET v_total2 = v_total2 + v_arr_valida_proceso[indx].v_valor_importe
                  LET v_bnd2 = 1 --Se activa la bandera cuando se registre un importe
               ELSE 
                  LET v_arr_valida_proceso[indx].importe2 = 0.00
               END IF
            END IF 

            IF v_inicio = 3 THEN 
               IF v_arr_proceso_cons[v_inicio] = v_arr_valida_proceso[indx].v_cuenta_contable THEN 
                  LET v_arr_valida_proceso[indx].importe3 = v_arr_valida_proceso[indx].v_valor_importe
                  LET v_total3 = v_total3 + v_arr_valida_proceso[indx].v_valor_importe
                  LET v_bnd3 = 1 --Se activa la bandera cuando se registre un importe
               ELSE 
                  LET v_arr_valida_proceso[indx].importe3 = 0.00
               END IF
            END IF 

            IF v_inicio = 4 THEN 
               IF v_arr_proceso_cons[v_inicio] = v_arr_valida_proceso[indx].v_cuenta_contable THEN 
                  LET v_arr_valida_proceso[indx].importe4 = v_arr_valida_proceso[indx].v_valor_importe
                  LET v_total4 = v_total4 + v_arr_valida_proceso[indx].v_valor_importe
                  LET v_bnd4 = 1 --Se activa la bandera cuando se registre un importe
               ELSE 
                  LET v_arr_valida_proceso[indx].importe4 = 0.00
               END IF
            END IF 

            IF v_inicio = 5 THEN 
               IF v_arr_proceso_cons[v_inicio] = v_arr_valida_proceso[indx].v_cuenta_contable THEN 
                  LET v_arr_valida_proceso[indx].importe5 = v_arr_valida_proceso[indx].v_valor_importe
                  LET v_total5 = v_total5 + v_arr_valida_proceso[indx].v_valor_importe
                  LET v_bnd5 = 1 --Se activa la bandera cuando se registre un importe
               ELSE 
                  LET v_arr_valida_proceso[indx].importe5 = 0.00
               END IF
            END IF 

            {DISPLAY "total 1",
            DISPLAY "total 2",
            DISPLAY "total 3",
            DISPLAY "total 4",
            DISPLAY "total 5",}

            
         END FOR 
                           
        LET indx = indx + 1
        
    END FOREACH
    
    CALL v_arr_valida_proceso.deleteElement(v_arr_valida_proceso.getLength())
    FOR   v_incremento=1 TO 5
       IF v_arr_proceso_cons[v_incremento] =0 THEN 
          LET v_arr_proceso_cons[v_incremento] =  NULL 
       END IF 
    END FOR 

    DISPLAY "El index de la consulta -- ", indx
    IF indx < 1 THEN
      CALL fn_mensaje("INFORMACIÓN","No existe información de importes para la(s) cuenta(s) contable(s)","info")
    ELSE 
   
      --DISPLAY BY NAME v_total1,v_total2,v_total3,v_total4,v_total5
    END IF 
    
      RETURN indx
    
END FUNCTION

--Función que limpia el arreglo agrupando los datos por folio de liquidación
FUNCTION fn_agrupa_cuentas()

   DEFINE 
      v_total_arreglo   INTEGER, 
      v_indice_row      INTEGER,
      v_indice_agrupa   INTEGER,
      v_folio_agrupa    DECIMAL (9,0)
      


      LET v_total_arreglo = v_arr_valida_proceso.getLength()
      LET v_indice_row = 1 
      LET v_indice_agrupa = 1

      FOR v_indice_row = 1 TO v_total_arreglo

        DISPLAY " v_indice_row folio: ", v_arr_valida_proceso[v_indice_row].folio_liquida
        DISPLAY " v_indice_row fecha: ", v_arr_valida_proceso[v_indice_row].f_liquida
         
      END FOR 


      LET v_arr_valida_proceso_agrupado[1].importe1 = 0.00
      LET v_arr_valida_proceso_agrupado[1].importe2 = 0.00
      LET v_arr_valida_proceso_agrupado[1].importe3 = 0.00
      LET v_arr_valida_proceso_agrupado[1].importe4 = 0.00
      LET v_arr_valida_proceso_agrupado[1].importe5 = 0.00

      LET v_total1 = 0.00
      LET v_total2 = 0.00
      LET v_total3 = 0.00
      LET v_total4 = 0.00
      LET v_total5 = 0.00
      
      DISPLAY "Rows -- ",v_total_arreglo

      --Ciclo para recorrer el arreglo 
      FOR v_indice_row = 1 TO v_total_arreglo

            --Obtiene el folio
            LET v_folio_agrupa = v_arr_valida_proceso[v_indice_row].folio_liquida
            DISPLAY "El folio agrupa - ",v_folio_agrupa

            --Asigna variables al nuevo arreglo
            LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].doc_contable = v_arr_valida_proceso[v_indice_row].doc_contable
            LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].f_liquida = v_arr_valida_proceso[v_indice_row].f_liquida
            LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio = v_arr_valida_proceso[v_indice_row].folio
            LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio_liquida = v_arr_valida_proceso[v_indice_row].folio_liquida
            LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].v_valor_importe = v_arr_valida_proceso[v_indice_row].v_valor_importe
            
            DISPLAY "ValoR importe ", v_arr_valida_proceso_agrupado[v_indice_agrupa].v_valor_importe
            DISPLAY "1:",  v_arr_valida_proceso[v_indice_row].importe1
            DISPLAY "2:",  v_arr_valida_proceso[v_indice_row].importe2
            DISPLAY "3:",  v_arr_valida_proceso[v_indice_row].importe3
            DISPLAY "4:",  v_arr_valida_proceso[v_indice_row].importe4
            DISPLAY "5:",  v_arr_valida_proceso[v_indice_row].importe5

            --valida que si hay montos diferentes a 0.00 para el mismo folio, entonces los almacena en otro arreglo
            IF v_arr_valida_proceso[v_indice_row].importe1 <> 0.00 THEN 
               --Suma si encuentra cantidades previas
               IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe1 <> 0.00 THEN 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe1 = v_arr_valida_proceso_agrupado[v_indice_agrupa].importe1 + v_arr_valida_proceso[v_indice_row].importe1

                  --Asigna otras variables
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].doc_contable = v_arr_valida_proceso[v_indice_row].doc_contable
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].f_liquida = v_arr_valida_proceso[v_indice_row].f_liquida
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio = v_arr_valida_proceso[v_indice_row].folio
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio_liquida = v_arr_valida_proceso[v_indice_row].folio_liquida
               ELSE 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe1 = v_arr_valida_proceso[v_indice_row].importe1 
               END IF  
            END IF 
            
            IF v_arr_valida_proceso[v_indice_row].importe2 <> 0.00 THEN
               IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe2 <> 0.00 THEN 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe2 = v_arr_valida_proceso_agrupado[v_indice_agrupa].importe2 + v_arr_valida_proceso[v_indice_row].importe2

                  --Asigna otras variables
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].doc_contable = v_arr_valida_proceso[v_indice_row].doc_contable
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].f_liquida = v_arr_valida_proceso[v_indice_row].f_liquida
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio = v_arr_valida_proceso[v_indice_row].folio
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio_liquida = v_arr_valida_proceso[v_indice_row].folio_liquida
               ELSE 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe2 = v_arr_valida_proceso[v_indice_row].importe2 
               END IF  
               --LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe2 = v_arr_valida_proceso[v_indice_row].importe2
            END IF
            
            IF v_arr_valida_proceso[v_indice_row].importe3 <> 0.00 THEN
               IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe3 <> 0.00 THEN 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe3 = v_arr_valida_proceso_agrupado[v_indice_agrupa].importe3 + v_arr_valida_proceso[v_indice_row].importe3

                  --Asigna otras variables
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].doc_contable = v_arr_valida_proceso[v_indice_row].doc_contable
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].f_liquida = v_arr_valida_proceso[v_indice_row].f_liquida
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio = v_arr_valida_proceso[v_indice_row].folio
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio_liquida = v_arr_valida_proceso[v_indice_row].folio_liquida
               ELSE 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe3 = v_arr_valida_proceso[v_indice_row].importe3 
               END IF  
               --LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe3 = v_arr_valida_proceso[v_indice_row].importe3
            END IF 
            
            IF v_arr_valida_proceso[v_indice_row].importe4 <> 0.00 THEN
               IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe4 <> 0.00 THEN 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe4 = v_arr_valida_proceso_agrupado[v_indice_agrupa].importe4 + v_arr_valida_proceso[v_indice_row].importe4

                  --Asigna otras variables
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].doc_contable = v_arr_valida_proceso[v_indice_row].doc_contable
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].f_liquida = v_arr_valida_proceso[v_indice_row].f_liquida
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio = v_arr_valida_proceso[v_indice_row].folio
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio_liquida = v_arr_valida_proceso[v_indice_row].folio_liquida
               ELSE 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe4 = v_arr_valida_proceso[v_indice_row].importe4 
               END IF  
               --LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe4 = v_arr_valida_proceso[v_indice_row].importe4
            END IF 
            
            IF v_arr_valida_proceso[v_indice_row].importe5 <> 0.00 THEN
               IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe5 <> 0.00 THEN 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe5 = v_arr_valida_proceso_agrupado[v_indice_agrupa].importe5 + v_arr_valida_proceso[v_indice_row].importe5

                  --Asigna otras variables
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].doc_contable = v_arr_valida_proceso[v_indice_row].doc_contable
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].f_liquida = v_arr_valida_proceso[v_indice_row].f_liquida
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio = v_arr_valida_proceso[v_indice_row].folio
                  LET  v_arr_valida_proceso_agrupado[v_indice_agrupa].folio_liquida = v_arr_valida_proceso[v_indice_row].folio_liquida
               ELSE 
                  LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe5 = v_arr_valida_proceso[v_indice_row].importe5 
               END IF  
               --LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe5 = v_arr_valida_proceso[v_indice_row].importe5
            END IF 






            IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe1 IS NULL 
            AND v_bnd1 = TRUE THEN 
               LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe1 = 0.00
            END IF
           IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe2 IS NULL 
           AND v_bnd2 = TRUE THEN 
               LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe2 = 0.00
            END IF
           IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe3 IS NULL 
           AND v_bnd3 = TRUE THEN 
               LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe3 = 0.00
            END IF
           IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe4 IS NULL 
           AND v_bnd4 = TRUE THEN 
               LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe4 = 0.00
            END IF
           IF v_arr_valida_proceso_agrupado[v_indice_agrupa].importe5 IS NULL 
           AND v_bnd5 = TRUE THEN 
               LET v_arr_valida_proceso_agrupado[v_indice_agrupa].importe5 = 0.00
            END IF 


            
            --Valida que hay un cambio de folio
            IF v_folio_agrupa <> v_arr_valida_proceso[v_indice_row + 1].folio_liquida THEN 
               LET v_indice_agrupa = v_indice_agrupa +1
            END IF 
            
      END FOR 

      

      FOR v_indice_row = 1 TO (v_arr_valida_proceso_agrupado.getLength())

         --Obtiene totales
         
         LET v_total1 = v_total1 + v_arr_valida_proceso_agrupado[v_indice_row].importe1
         LET v_total2 = v_total2 + v_arr_valida_proceso_agrupado[v_indice_row].importe2
         LET v_total3 = v_total3 + v_arr_valida_proceso_agrupado[v_indice_row].importe3
         LET v_total4 = v_total4 + v_arr_valida_proceso_agrupado[v_indice_row].importe4
         LET v_total5 = v_total5 + v_arr_valida_proceso_agrupado[v_indice_row].importe5
      
         DISPLAY "#####################################"
         DISPLAY "Folio: ",v_arr_valida_proceso_agrupado[v_indice_row].folio_liquida
         DISPLAY "Importe 1: ",v_arr_valida_proceso_agrupado[v_indice_row].importe1
         DISPLAY "Importe 2: ",v_arr_valida_proceso_agrupado[v_indice_row].importe2
         DISPLAY "Importe 3: ",v_arr_valida_proceso_agrupado[v_indice_row].importe3
         DISPLAY "Importe 4: ",v_arr_valida_proceso_agrupado[v_indice_row].importe4
         DISPLAY "Importe 5: ",v_arr_valida_proceso_agrupado[v_indice_row].importe5
      END FOR 


      DISPLAY "total 1",v_total1
            DISPLAY "total 2",v_total2
            DISPLAY "total 3",v_total3
            DISPLAY "total 4",v_total4
            DISPLAY "total 5",v_total5

   DISPLAY BY NAME v_total1,v_total2,v_total3,v_total4,v_total5
      
END FUNCTION 


--v_arr_valida_proceso_agrupado
--Genera un archivo de salida en texto plano con la información del detalle de folio por proceso
FUNCTION fn_genera_archivo_detalle_folio()

   DEFINE 
      v_nom_archivo        VARCHAR(50), -- nombre del archivo de salida
      v_ruta_envio_cnt     LIKE seg_modulo.ruta_envio,
      v_ruta_nomarch       VARCHAR(100), -- ruta y nombre del archivo de salida
      v_ch_arch_salida     BASE.CHANNEL,
      v_recorre_arreglo    INTEGER,
      v_archivo_copia      VARCHAR (50),
      v_comando_dos        STRING,
      v_encabezado         STRING,
      v_detalle            STRING,
      v_sumario            STRING,
      v_desc_proceso_cnt   VARCHAR(65) 

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

   LET v_nom_archivo = "/detalle_folio_", v_hora

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
   --INPUT BY NAME v_cmb_proceso, f_fecha, f_fecha_fin,f_folio_cnt, f_folio_liquida
   IF v_cmb_proceso IS NOT NULL THEN

      SELECT desc_proceso_cnt
      INTO v_desc_proceso_cnt
      FROM cat_proceso_cnt
      WHERE cod_proceso_cnt = v_cmb_proceso
   
      LET v_encabezado = v_encabezado,"|Proceso: ",v_cmb_proceso,"-",v_desc_proceso_cnt
   END IF 
   IF f_fecha IS NOT NULL THEN
      LET v_encabezado = v_encabezado,"|Fecha Inicial:",f_fecha USING "dd-mm-yyyy"
   END IF 
   IF f_fecha_fin IS NOT NULL THEN
      LET v_encabezado = v_encabezado,"|Fecha Final:",f_fecha_fin USING "dd-mm-yyyy"
   END IF 
   IF f_folio_cnt IS NOT NULL THEN
      LET v_encabezado = v_encabezado,"|Folio Contable:",f_folio_cnt
   END IF 
   IF f_folio_liquida IS NOT NULL THEN
      LET v_encabezado = v_encabezado,"|Folio Liquidación:",f_folio_liquida
   END IF 
   
   CALL v_ch_arch_salida.write([v_encabezado])
   
   LET v_encabezado = "Fecha Liquidación|Folio Liquidación|Folio Contable |Documento Contable SAP-FICO|",v_arr_proceso_cons[1],"|",v_arr_proceso_cons[2],"|",v_arr_proceso_cons[3],"|",v_arr_proceso_cons[4],"|",v_arr_proceso_cons[5],"|"
   CALL v_ch_arch_salida.write([v_encabezado])

   
   
   FOR v_recorre_arreglo = 1 TO v_arr_valida_proceso_agrupado.getLength()
      LET v_detalle = v_arr_valida_proceso_agrupado[v_recorre_arreglo].f_liquida USING "dd-mm-yyyy", "|",
                      v_arr_valida_proceso_agrupado[v_recorre_arreglo].folio_liquida, "|",
                      v_arr_valida_proceso_agrupado[v_recorre_arreglo].folio, "|",
                      v_arr_valida_proceso_agrupado[v_recorre_arreglo].doc_contable, "|",
                      v_arr_valida_proceso_agrupado[v_recorre_arreglo].importe1, "|",
                      v_arr_valida_proceso_agrupado[v_recorre_arreglo].importe2, "|",
                      v_arr_valida_proceso_agrupado[v_recorre_arreglo].importe3, "|",
                      v_arr_valida_proceso_agrupado[v_recorre_arreglo].importe4, "|",
                      v_arr_valida_proceso_agrupado[v_recorre_arreglo].importe5
    
      CALL v_ch_arch_salida.write([v_detalle])

   END FOR

   --Escribe el sumario
    LET v_sumario = "TOTALES| | | |",v_total1,"|",
                     v_total2, "|",
                     v_total3,  "|",
                     v_total4, "|",
                     v_total5, "|"
                     

   CALL v_ch_arch_salida.write([v_sumario])
   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   
   --Cambia el formato del archivo a DOS
   LET v_comando_dos = "unix2dos ",v_ruta_envio_cnt CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   CALL fn_mensaje("Información","Se ha generado el archivo de Detalle de Folio por Proceso\n en la ruta"||v_ruta_nomarch,"information")
   

END FUNCTION

FUNCTION fn_reporte_valida_proceso(v_arr_valida_proceso,v_arr_proceso_cons, f_fecha)
DEFINE 
          v_arr_valida_proceso DYNAMIC ARRAY OF RECORD 
                   f_liquida         DATE,
                   folio_liquida     DECIMAL (9,0)  ,
                   folio             DECIMAL (9,0)  ,
                   doc_contable      DECIMAL (10,0) ,--LIKE cnt_ctr_proceso.num_poliza,
                   importe1          LIKE  cnt_transaccion.importe,
                   importe2          LIKE  cnt_transaccion.importe,
                   importe3          LIKE  cnt_transaccion.importe,
                   importe4          LIKE  cnt_transaccion.importe,
                   importe5          LIKE  cnt_transaccion.importe,
                   v_valor_importe   LIKE  cnt_transaccion.importe,
                   v_cuenta_contable LIKE  cnt_transaccion.cta_contable
          END RECORD,
          v_arr_proceso_cons  DYNAMIC ARRAY OF VARCHAR(10),
          i                   INTEGER,
          f_fecha             DATE
          

   DEFINE manejador_rpt  om.SaxDocumentHandler  -- Contenedor documentos reporte


   -- Botón para generar el reporte en PDF de la consulta
   IF fgl_report_loadCurrentSettings("CNTC032.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   END IF
   DISPLAY "f_fecha ----V ",f_fecha

   --Obtiene la descripci{on del proceso
   
   SELECT cod_proceso_cnt||"-"||desc_proceso_cnt 
   INTO v_desc_proceso
   FROM cat_proceso_cnt
   WHERE cod_proceso_cnt = v_cmb_proceso

   DISPLAY "v_desc_proceso -- ",v_desc_proceso


   
   --Inicia el reporte de registros con rechazo
   START REPORT rpt_proc_cnt TO XML HANDLER manejador_rpt 
   FOR i = 1 TO   v_arr_valida_proceso.getLength() 
         DISPLAY "Folio --",v_arr_valida_proceso[i].folio
         DISPLAY "Doc --",v_arr_valida_proceso[i].doc_contable
         OUTPUT TO REPORT rpt_proc_cnt(v_arr_valida_proceso[i].*,v_arr_proceso_cons[1],
                                        v_arr_proceso_cons[2],v_arr_proceso_cons[3],
                                        v_arr_proceso_cons[4],v_arr_proceso_cons[5],f_fecha)
   END FOR      

   FINISH REPORT rpt_proc_cnt

END FUNCTION

REPORT rpt_proc_cnt(v_f_liquida,v_folio_liquida,v_folio ,v_doc_contable ,v_importe1,v_importe2,v_importe3,v_importe4,v_importe5,v_valor_importe,v_cuenta_contable,v_titulo_imp1,v_titulo_imp2,v_titulo_imp3,v_titulo_imp4,v_titulo_imp5, f_fecha)
DEFINE 
          v_f_liquida         DATE,
          v_folio_liquida   DECIMAL,
          v_folio           DECIMAL,
          v_doc_contable CHAR(10),-- DECIMAL(10,0), --LIKE cnt_ctr_proceso.num_poliza,
          v_importe1  LIKE  cnt_transaccion.importe,
          v_importe2  LIKE  cnt_transaccion.importe,
          v_importe3  LIKE  cnt_transaccion.importe,
          v_importe4  LIKE  cnt_transaccion.importe,
          v_importe5  LIKE  cnt_transaccion.importe,
          v_valor_importe   LIKE  cnt_transaccion.importe,
          v_cuenta_contable LIKE  cnt_transaccion.cta_contable,
          v_titulo_imp1  LIKE cnt_regla_contable.cta_contable,
          v_titulo_imp2  LIKE cnt_regla_contable.cta_contable,
          v_titulo_imp3  LIKE cnt_regla_contable.cta_contable,
          v_titulo_imp4  LIKE cnt_regla_contable.cta_contable,
          v_titulo_imp5  LIKE cnt_regla_contable.cta_contable,
          v_fecha_reporte DATE,
          f_fecha         DATE 
          



FORMAT
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED
      PRINTX g_usuario
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_fecha USING "dd-mm-yyyy"
      PRINTX v_desc_proceso
      PRINTX v_titulo_imp1
      PRINTX v_titulo_imp2
      PRINTX v_titulo_imp3
      PRINTX v_titulo_imp4
      PRINTX v_titulo_imp5
      PRINTX f_folio_cnt
      PRINTX f_folio_liquida
      PRINTX v_total1
      PRINTX v_total2
      PRINTX v_total3
      PRINTX v_total4
      PRINTX v_total5
      PRINTX v_bnd1
      PRINTX v_bnd2
      PRINTX v_bnd3
      PRINTX v_bnd4
      PRINTX v_bnd5
      
      

   ON EVERY ROW
      PRINTX v_f_liquida
      PRINTX v_folio_liquida
      PRINTX v_folio
      PRINTX v_doc_contable 
      PRINTX v_importe1  
      PRINTX v_importe2  
      PRINTX v_importe3  
      PRINTX v_importe4            
      PRINTX v_importe5  
      
END REPORT