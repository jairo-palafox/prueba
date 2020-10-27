################################################################################
# VERSION: 1.0.0                                                               #
# FECHA ULTIMA MODIFICACION: 15/05/2012                                        #
################################################################################
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#MODULO            => CNT                                                      #
#PROGRAMA          => CNTC02                                                   #
#OBJETIVO          => PROGRAMA DE CONSULTA Y GENERACIÓN DEL REPORTE DEL CUADRO #
#                     DE VALIDACIÓN DE PROCESOS                                #
#FECHA INICIO      => 15/05/2012                                               #
################################################################################
DATABASE
   safre_viv
GLOBALS "CNTG01.4gl"
GLOBALS
DEFINE v_QryTxt      STRING
DEFINE l_i_indice    INTEGER
DEFINE v_indice      INTEGER
DEFINE i             INTEGER
DEFINE v_registros   INTEGER
DEFINE v_arr_ctas_per       DYNAMIC ARRAY OF VARCHAR(10) -- Archivos pendientes
DEFINE v_arr_ctas_per_cons  DYNAMIC ARRAY OF VARCHAR(10) -- Archivos pendientes
DEFINE g_arr_cta_importe    DYNAMIC ARRAY OF RECORD --Arreglo almacena
         v_proceso   VARCHAR(50),                   --cuenta importe por periodo
         v_imp_cta1  DECIMAL(22,2),
         v_imp_cta2  DECIMAL(22,2),
         v_imp_cta3  DECIMAL(22,2),
         v_imp_cta4  DECIMAL(22,2),
         v_imp_cta5  DECIMAL(22,2),
         v_sum_proc  DECIMAL(22,2)
                     END RECORD


DEFINE v_saldo_cta1 DECIMAL(22,2),
       v_saldo_cta2 DECIMAL(22,2),
       v_saldo_cta3 DECIMAL(22,2),
       v_saldo_cta4 DECIMAL(22,2),
       v_saldo_cta5 DECIMAL(22,2),
       v_saldo_suma DECIMAL(22,2)

   DEFINE
      f_fecha_ini    DATE , --fecha de liquidación
      f_fecha_fin    DATE   --fecha de liquidación
       
END GLOBALS                            
MAIN
DEFINE f_ventana     ui.Window,   -- Define las propìedades de la Ventana
       f_forma       ui.Form,     -- Define las propiedades de la forma
       l_dnd         ui.DragDrop, -- manejador del (drag and drop)
       l_drag_source STRING,      -- fuente del drag
       l_drag_index  INTEGER,     -- indice del drag
       l_drop_index  INTEGER,     -- indice del drop
       l_drag_value  STRING       -- valor del drag
DEFINE v_column1     VARCHAR(10),      -- columna de cuenta uno
       v_column2     VARCHAR(10),      -- columna de cuenta dos
       v_column3     VARCHAR(10),      -- columna de cuenta tres
       v_column4     VARCHAR(10),      -- columna de cuenta cuatro
       v_column5     VARCHAR(10)       -- columna de cuenta cinco



       
CONSTANT l_nom_tbl_per = "scr_ctas_per" -- Tabla cuentas del periodo
CONSTANT l_nom_tbl_con = "scr_ctas_per_cons" -- Tabla de cuantas a consultar

   -- se asignan los parametros que vienen del fglrun
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog     = ARG_VAL(3)

   LET v_saldo_cta1 = 0.00
   LET v_saldo_cta2 = 0.00
   LET v_saldo_cta3 = 0.00
   LET v_saldo_cta4 = 0.00
   LET v_saldo_cta5 = 0.00
   LET v_saldo_suma = 0.00

   -- se asigna el titulo del programa
   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW vtn_cntc02 WITH FORM "CNTC021"
      DIALOG ATTRIBUTES(UNBUFFERED)
         --INPUT v_cmb_sub_cta,v_fecha FROM v_cmb_sub_cta,f_fecha
         INPUT BY NAME f_fecha_ini, f_fecha_fin 
         BEFORE INPUT
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()
            CALL f_forma.setElementHidden("grp_cons_ctas",1)
            CALL f_forma.setElementHidden("grp_cons_per",1)
            CALL f_forma.setElementHidden("grp_valida_proc",1)
            CALL DIALOG.setActionHidden("consultar",1)
            CALL DIALOG.setActionHidden("reporte",1)
            CALL DIALOG.setActionHidden("archivo",1)

            --CALL fn_llena_combo_periodo()
            
            --CALL fn_cmb_subcta(" IN (49,48,44,43,41,40,8,4)") -- Llena combo subcuenta

            --ON CHANGE v_cmb_sub_cta
            ON ACTION ACCEPT

               --DISPLAY "v_cmb_sub_cta -- ",v_cmb_sub_cta
               DISPLAY "f_fecha_ini -- ",f_fecha_ini
               DISPLAY "f_fecha_fin -- ",f_fecha_fin
            
               IF (f_fecha_ini IS NULL) AND (f_fecha_fin IS NULL) THEN
                  CALL fn_mensaje("Error",
                                  "Ingrese al menos un criterio de búsqueda",
                                  "information")
                     NEXT FIELD f_fecha_ini
               END IF 
               --Limpia tablas de cuentas(D&D)
               CALL v_arr_ctas_per.clear()
               CALL v_arr_ctas_per_cons.clear()
               --Obtiene cuentas del periodo
               CALL fn_cuntas_periodo()
               --Si no hay cuentas a consultar con importes
               IF l_i_indice > 1 THEN
                  CALL f_forma.setElementHidden("grp_cons_ctas",0)
                  CALL f_forma.setElementHidden("grp_cons_per",0)
               ELSE
                  CALL fn_mensaje("ATENCIÓN","No hay cuentas a consultar","about")
               END IF
         END INPUT

         --Tabla de cuentas correspondientes al periodo
         DISPLAY ARRAY v_arr_ctas_per TO scr_ctas_per.*
            ON DRAG_START(l_dnd)
               LET l_drag_source = l_nom_tbl_per
               LET l_drag_index = arr_curr()
               LET l_drag_value = v_arr_ctas_per[l_drag_index]
             
            ON DRAG_FINISHED(l_dnd)
               INITIALIZE l_drag_source TO NULL

            ON DRAG_ENTER(l_dnd)
               IF l_drag_source IS NULL THEN
                  CALL l_dnd.setOperation(NULL)
               END IF
             
            ON DROP(l_dnd)
               IF v_arr_ctas_per_cons.getLength()=0 THEN
                  CALL DIALOG.setActionHidden("consultar",1)
               ELSE
                 CALL DIALOG.setActionHidden("consultar",0)
               END IF        

               IF l_drag_source == l_nom_tbl_per THEN
                  CALL l_dnd.dropInternal()
               ELSE
                  LET l_drop_index = l_dnd.getLocationRow()
                  CALL DIALOG.insertRow(l_nom_tbl_per, l_drop_index)
                  CALL DIALOG.setCurrentRow(l_nom_tbl_per, l_drop_index)
                  LET v_arr_ctas_per[l_drop_index] = l_drag_value
                  CALL DIALOG.deleteRow(l_nom_tbl_con, l_drag_index)

                  --si no  hay  procesos  seleccionados oculta el boton Consultar
                  IF v_arr_ctas_per_cons.getLength()=0 THEN
                     CALL DIALOG.setActionHidden("consultar",1)             
                  END IF 
               END IF
         END DISPLAY

         --Tabla de cuentas a consultar del periodo
         DISPLAY ARRAY v_arr_ctas_per_cons TO scr_ctas_per_cons.*
            ON DRAG_START(l_dnd)
               LET l_drag_source = l_nom_tbl_con
               LET l_drag_index = arr_curr()
               LET l_drag_value = v_arr_ctas_per_cons[l_drag_index]

            ON DRAG_FINISHED(l_dnd)
               INITIALIZE l_drag_source TO NULL

            ON DRAG_ENTER(l_dnd)
               IF l_drag_source IS NULL THEN
                  CALL l_dnd.setOperation(NULL)
               END IF

            ON DROP(l_dnd)
               IF v_arr_ctas_per_cons.getLength() == 5 THEN
                  CALL fn_mensaje("ATENCIÓN",
                               "Sólo se puede consultar un máximo de 5 cuentas",
                               "about")
                ELSE
                  IF l_drag_source == l_nom_tbl_con THEN
                     CALL l_dnd.dropInternal()
                  ELSE
                     LET l_drop_index = l_dnd.getLocationRow()
                     CALL DIALOG.insertRow(l_nom_tbl_con, l_drop_index)
                     CALL DIALOG.setCurrentRow(l_nom_tbl_con, l_drop_index)
                     LET v_arr_ctas_per_cons[l_drop_index] = l_drag_value
                     CALL DIALOG.deleteRow(l_nom_tbl_per, l_drag_index)
                  END IF
                  CALL DIALOG.setActionHidden("consultar",0)
               END IF
         END DISPLAY

        --Muestra importes de las cuentas consultadas por proceso del periodo
        DISPLAY ARRAY g_arr_cta_importe TO scr_valida_proceso.*
        END DISPLAY

         ON ACTION cancelar
            EXIT DIALOG
        
         ON ACTION consultar
               --Asigana el la cuenta contable a la columna de la tabla
               LET v_column1 = v_arr_ctas_per_cons[1]
               LET v_column2 = v_arr_ctas_per_cons[2]
               LET v_column3 = v_arr_ctas_per_cons[3]
               LET v_column4 = v_arr_ctas_per_cons[4]
               LET v_column5 = v_arr_ctas_per_cons[5]
               CALL DIALOG.setActionHidden("reporte",0)
               CALL DIALOG.setActionHidden("archivo",0)
               CALL DIALOG.setActionHidden("consultar",1)
               CALL f_forma.setElementHidden("grp_valida_proc",0)
               --Muestra el nombre de las columnas segun tablas consultadas
               CALL f_forma.setElementText("formonly.tb_cta1",v_column1)
               CALL f_forma.setElementText("formonly.tb_cta2",v_column2)
               CALL f_forma.setElementText("formonly.tb_cta3",v_column3)
               CALL f_forma.setElementText("formonly.tb_cta4",v_column4)
               CALL f_forma.setElementText("formonly.tb_cta5",v_column5)

               --Oculta columnas y campos de totales
               {IF v_column1 IS NULL THEN
                  CALL f_forma.setElementHidden("formonly.tb_cta1",TRUE)
                  CALL f_forma.setElementHidden("formonly.sum_cta1",TRUE)
               END IF
               IF v_column2 IS NULL THEN
                  CALL f_forma.setElementHidden("formonly.tb_cta2",TRUE)
                  CALL f_forma.setElementHidden("formonly.sum_cta2",TRUE)
               END IF
               IF v_column3 IS NULL THEN
                  CALL f_forma.setElementHidden("formonly.tb_cta3",TRUE)
                  CALL f_forma.setElementHidden("formonly.sum_cta3",TRUE)
               END IF
               IF v_column4 IS NULL THEN
                  CALL f_forma.setElementHidden("formonly.tb_cta4",TRUE)
                  CALL f_forma.setElementHidden("formonly.sum_cta4",TRUE)
               END IF
               IF v_column5 IS NULL THEN
                  CALL f_forma.setElementHidden("formonly.tb_cta5",TRUE)
                  CALL f_forma.setElementHidden("formonly.sum_cta5",TRUE)
               END IF}
               --CALL fn_nombre_encabezado()
               CALL fn_valida_proceso()
               IF v_registros > 0 THEN
                  CALL fn_importe_cuenta_periodo() 
               ELSE
                  CALL fn_mensaje("ATENCIÓN","Cuentas sin importe","about")
                  CALL v_arr_ctas_per.clear()
                  CALL v_arr_ctas_per_cons.clear()
                  DROP TABLE safre_tmp:tmp_valida_proceso_cnt
                  NEXT FIELD f_fecha_ini
               END IF
               
                 { --Despliega totales por cuenta
                  IF v_saldo_cta1 = 0.00 THEN
                     LET v_saldo_cta1 = v_saldo_suma
                     LET v_saldo_cta2 = 0.00
                     LET v_saldo_cta3 = 0.00
                     LET v_saldo_cta4 = 0.00
                     LET v_saldo_cta5 = 0.00
                     LET v_saldo_suma = 0.00
                  END IF
                  IF v_saldo_cta2 = 0.00 THEN
                     LET v_saldo_cta2 = v_saldo_suma
                     LET v_saldo_cta3 = 0.00
                     LET v_saldo_cta4 = 0.00
                     LET v_saldo_cta5 = 0.00
                     LET v_saldo_suma = 0.00
                  END IF
                  IF v_saldo_cta3 = 0.00 THEN
                     LET v_saldo_cta3 = v_saldo_suma
                     LET v_saldo_cta4 = 0.00
                     LET v_saldo_cta5 = 0.00
                     LET v_saldo_suma = 0.00
                  END IF
                  IF v_saldo_cta4 = 0.00 THEN
                     LET v_saldo_cta4 = v_saldo_suma
                     LET v_saldo_cta5 = 0.00
                     LET v_saldo_suma = 0.00
                  END IF
                  IF v_saldo_cta5 = 0.00 THEN
                     LET v_saldo_cta5 = v_saldo_suma
                     LET v_saldo_suma = 0.00
                  END IF}
                  DISPLAY v_saldo_cta1 TO sum_cta1
                  DISPLAY v_saldo_cta2 TO sum_cta2
                  DISPLAY v_saldo_cta3 TO sum_cta3
                  DISPLAY v_saldo_cta4 TO sum_cta4
                  DISPLAY v_saldo_cta5 TO sum_cta5
                  DISPLAY v_saldo_suma TO sum_tot
                  {IF v_saldo_cta1 = 0.00 THEN
                     CALL f_forma.setElementHidden("formonly.sum_cta1",1)
                  END IF
                  IF v_saldo_cta2 = 0.00 THEN
                     CALL f_forma.setElementHidden("formonly.sum_cta2",1)
                  END IF
                  IF v_saldo_cta3 = 0.00 THEN
                     CALL f_forma.setElementHidden("formonly.sum_cta3",1)
                  END IF
                  IF v_saldo_cta4 = 0.00 THEN
                     CALL f_forma.setElementHidden("formonly.sum_cta4",1)
                  END IF
                  IF v_saldo_cta5 = 0.00 THEN
                     CALL f_forma.setElementHidden("formonly.sum_cta5",1)
                  END IF
                  IF v_saldo_suma = 0.00 THEN
                     CALL f_forma.setElementHidden("formonly.sum_tot",1)
                  END IF}

        
         ON ACTION reporte
            CALL fn_reporte_valida_proceso(v_saldo_cta1,v_saldo_cta2,
                                           v_saldo_cta3,v_saldo_cta4,
                                           v_saldo_cta5,v_saldo_suma,
                                           v_column1,v_column2,v_column3,
                                           v_column4,v_column5)
         ON ACTION archivo
            CALL fn_genera_archivo_totales_cuenta()
         
      END DIALOG 
   CLOSE WINDOW vtn_cntc02
END MAIN
#Objetivo: Identificar las cuentas contables del periodo consultado
FUNCTION fn_cuntas_periodo()

   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET v_QryTxt = "\n SELECT cta_contable",
          "\n FROM cnt_regla_contable",
          "\n WHERE cod_proceso_cnt IN (SELECT cp.cod_proceso_cnt",
          "\n                           FROM cat_proceso_cnt cp,cnt_transaccion ct",
          "\n                           WHERE cp.cod_proceso_cnt = ct.cod_proceso_cnt "

   
   IF (f_fecha_ini IS NOT NULL) AND (f_fecha_fin IS NOT NULL) THEN
      LET v_QryTxt = v_QryTxt,"\n AND ct.f_liquida BETWEEN '",f_fecha_ini,"' AND '",f_fecha_fin,"'"
   END IF 
          
   LET v_QryTxt = v_QryTxt,"\n ) \n GROUP BY 1 ORDER BY 1"
   DISPLAY v_QryTxt
   
   PREPARE prp_ctas_per FROM v_QryTxt

   LET l_i_indice = 1
   DECLARE cur_ctas_per CURSOR FOR prp_ctas_per 
      FOREACH cur_ctas_per INTO v_arr_ctas_per[l_i_indice]
         -- se incrementa el indice del arreglo
         LET l_i_indice = l_i_indice + 1
      END FOREACH

   -- se borra el ultimo indice del arreglo porque es nulo
   CALL v_arr_ctas_per.deleteElement(l_i_indice)
END FUNCTION
#Objetivo: Obtener el saldo de las cuentas contables deacuerdo al proceso del
#          periodo consultado
FUNCTION fn_valida_proceso()
DEFINE v_arr_valida_proceso RECORD
          v_proceso     VARCHAR(50),
          v_imp_cta     LIKE cnt_transaccion.importe,
          v_imp_cta_sum LIKE cnt_transaccion.importe
                        END RECORD

DEFINE 
   v_naturaleza   SMALLINT 
                        
   --Invoca función para dropear tablas
   CALL fn_crea_tabla_tem()

--Hace recorrido del arrglo segun cuentas a consultar por periodo para consulta
FOR i =1 TO v_arr_ctas_per_cons.getLength()
--Sele hace la cosulta sobre el numero de cuentas a consulta, maximo 5
IF length(v_arr_ctas_per_cons[i]) > 0 OR v_arr_ctas_per_cons[i] IS NOT NULL THEN
   LET v_QryTxt = "\n SELECT cp.cod_proceso_cnt||'-'||cp.desc_proceso_cnt,",
                  "\n        SUM(ct.importe), ct.cod_naturaleza_cta",
                  "\n FROM cat_proceso_cnt cp,cnt_transaccion ct",
                  "\n WHERE ct.cta_contable = ","'",v_arr_ctas_per_cons[i],"'"
                  --"\n AND ct.tpo_transaccion = 0",

      
      IF (f_fecha_ini IS NOT NULL) AND (f_fecha_fin IS NOT NULL) THEN
         LET v_QryTxt = v_QryTxt,"\n AND ct.f_liquida BETWEEN '",f_fecha_ini,"' AND '",f_fecha_fin,"'"
      END IF

      LET v_QryTxt = v_QryTxt,"\n AND cp.cod_proceso_cnt = ct.cod_proceso_cnt"
      
      LET v_QryTxt = v_QryTxt ,"\n GROUP BY 1,3 ORDER BY 1"

      DISPLAY "Consulta de importes -- ",v_QryTxt
      
      PREPARE prp_valida_proceso FROM v_QryTxt

      DECLARE cur_valida_proceso CURSOR FOR prp_valida_proceso
      FOREACH cur_valida_proceso INTO v_arr_valida_proceso.v_proceso,
                                      v_arr_valida_proceso.v_imp_cta,
                                      v_naturaleza

         LET v_registros = v_registros + 1

         IF v_naturaleza = 1 THEN --Si es un abono, lo multiplica por -1 para manejar cargos y abonos
            DISPLAY "La cuenta ",v_arr_ctas_per_cons[i], "es un abono"
            LET v_arr_valida_proceso.v_imp_cta = (v_arr_valida_proceso.v_imp_cta * -1)
         END IF 

         
         
         --Inserta el importe segun posición de columna(tabla)
         IF i = 1 THEN
            INSERT INTO safre_tmp:tmp_valida_proceso_cnt
                   VALUES(v_arr_valida_proceso.v_proceso,
                          v_arr_valida_proceso.v_imp_cta,
                          0.00,0.00,0.00,0.00)
            LET v_arr_valida_proceso.v_imp_cta = NULL
         END IF
         IF i = 2 THEN
            INSERT INTO safre_tmp:tmp_valida_proceso_cnt
                   VALUES(v_arr_valida_proceso.v_proceso,
                          0.00,v_arr_valida_proceso.v_imp_cta,
                          0.00,0.00,0.00)
            LET v_arr_valida_proceso.v_imp_cta = NULL
         END IF
         IF i = 3 THEN
            INSERT INTO safre_tmp:tmp_valida_proceso_cnt
                   VALUES(v_arr_valida_proceso.v_proceso,
                          0.00,0.00,v_arr_valida_proceso.v_imp_cta,
                          0.00,0.00)
            LET v_arr_valida_proceso.v_imp_cta = NULL
         END IF
         IF i = 4 THEN
            INSERT INTO safre_tmp:tmp_valida_proceso_cnt
                   VALUES(v_arr_valida_proceso.v_proceso,
                          0.00,0.00,0.00,v_arr_valida_proceso.v_imp_cta,
                          0.00)
            LET v_arr_valida_proceso.v_imp_cta = NULL
         END IF
         IF i = 5 THEN
            INSERT INTO safre_tmp:tmp_valida_proceso_cnt
                   VALUES(v_arr_valida_proceso.v_proceso,
                          0.00,0.00,0.00,0.00,
                          v_arr_valida_proceso.v_imp_cta)
            LET v_arr_valida_proceso.v_imp_cta = NULL
         END IF
      END FOREACH
END IF
END FOR

END FUNCTION
#Objetivo: Se crea tabla temporar para la consulta y reporte de valida proceso
FUNCTION fn_crea_tabla_tem()

   --DELETE FROM safre_tmp:tmp_valida_proceso_cnt
   --WHENEVER ERROR CONTINUE 
   CREATE TEMP TABLE tmp_valida_proceso_cnt
                     (proceso    CHAR(50),
                      imp_cta1   DECIMAL(22,2),
                      imp_cta2   DECIMAL(22,2),
                      imp_cta3   DECIMAL(22,2),
                      imp_cta4   DECIMAL(22,2),
                      imp_cta5   DECIMAL(22,2));
       
END FUNCTION
#Objetivo: Consulta pra presentar la información de la seccion valida proceso
FUNCTION fn_importe_cuenta_periodo()


   
       
   LET v_QryTxt = "\n SELECT proceso,SUM(imp_cta1),SUM(imp_cta2),SUM(imp_cta3),",
                  "\n                SUM(imp_cta4),SUM(imp_cta5)",
                  "\n FROM tmp_valida_proceso_cnt",
                  "\n GROUP BY 1",
                  "\n ORDER BY 1"
   PREPARE prp_suma_cuentas_importe FROM v_QryTxt

   LET v_indice = 1

   DECLARE cur_suma_cuentas_importe CURSOR FOR prp_suma_cuentas_importe
      FOREACH cur_suma_cuentas_importe INTO g_arr_cta_importe[v_indice].v_proceso,
                                            g_arr_cta_importe[v_indice].v_imp_cta1,
                                            g_arr_cta_importe[v_indice].v_imp_cta2,
                                            g_arr_cta_importe[v_indice].v_imp_cta3,
                                            g_arr_cta_importe[v_indice].v_imp_cta4,
                                            g_arr_cta_importe[v_indice].v_imp_cta5


         --Valida que si viene nulo algún valor, lo indique en 0.00
         IF g_arr_cta_importe[v_indice].v_imp_cta1 IS NULL THEN 
            LET g_arr_cta_importe[v_indice].v_imp_cta1 = 0.00
         END IF
         IF g_arr_cta_importe[v_indice].v_imp_cta2 IS NULL THEN 
            LET g_arr_cta_importe[v_indice].v_imp_cta2 = 0.00
         END IF 
         IF g_arr_cta_importe[v_indice].v_imp_cta3 IS NULL THEN 
            LET g_arr_cta_importe[v_indice].v_imp_cta3 = 0.00
         END IF 
         IF g_arr_cta_importe[v_indice].v_imp_cta4 IS NULL THEN 
            LET g_arr_cta_importe[v_indice].v_imp_cta4 = 0.00
         END IF 
         IF g_arr_cta_importe[v_indice].v_imp_cta5 IS NULL THEN 
            LET g_arr_cta_importe[v_indice].v_imp_cta5 = 0.00
         END IF  

         --Suma de cuentas por proceso
         LET g_arr_cta_importe[v_indice].v_sum_proc =
             g_arr_cta_importe[v_indice].v_imp_cta1 +
             g_arr_cta_importe[v_indice].v_imp_cta2 +
             g_arr_cta_importe[v_indice].v_imp_cta3 +
             g_arr_cta_importe[v_indice].v_imp_cta4 +
             g_arr_cta_importe[v_indice].v_imp_cta5

         --Saldos por cuenta
         LET v_saldo_cta1 = v_saldo_cta1 + g_arr_cta_importe[v_indice].v_imp_cta1
         LET v_saldo_cta2 = v_saldo_cta2 + g_arr_cta_importe[v_indice].v_imp_cta2
         LET v_saldo_cta3 = v_saldo_cta3 + g_arr_cta_importe[v_indice].v_imp_cta3
         LET v_saldo_cta4 = v_saldo_cta4 + g_arr_cta_importe[v_indice].v_imp_cta4
         LET v_saldo_cta5 = v_saldo_cta5 + g_arr_cta_importe[v_indice].v_imp_cta5
         LET v_saldo_suma = v_saldo_suma + g_arr_cta_importe[v_indice].v_sum_proc

         DISPLAY "Saldo 1 --",v_saldo_cta1
         DISPLAY "Saldo 2 --",v_saldo_cta2
         DISPLAY "Saldo 3 --",v_saldo_cta3
         DISPLAY "Saldo 4 --",v_saldo_cta4
         DISPLAY "Saldo 5 --",v_saldo_cta5
         DISPLAY "Saldo T --",v_saldo_suma
         
         LET v_indice = v_indice + 1

      END FOREACH
         CALL g_arr_cta_importe.deleteElement(v_indice)
      --CALL v_arr_ctas_per_cons.deleteElement(v_indice)

   
END FUNCTION

--Genera un archivo de salida en texto plano con la información de los totales por cuenta contable
FUNCTION fn_genera_archivo_totales_cuenta()

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
      v_sumario            STRING

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

      
   LET v_nom_archivo = "/totales_cuenta_contable_", v_hora


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
   LET v_encabezado = "Fecha Inicial: ",f_fecha_ini USING "dd-mm-yyyy","|Fecha Final: ", f_fecha_fin USING "dd-mm-yyyy"
   CALL v_ch_arch_salida.write([v_encabezado])
   LET v_encabezado = "Procesos| ",v_arr_ctas_per_cons[1],"|",v_arr_ctas_per_cons[2],"|",v_arr_ctas_per_cons[3],"|",v_arr_ctas_per_cons[4],"|",v_arr_ctas_per_cons[5],"|"
   CALL v_ch_arch_salida.write([v_encabezado])

   FOR v_recorre_arreglo = 1 TO g_arr_cta_importe.getLength()
      LET v_detalle = g_arr_cta_importe[v_recorre_arreglo].v_proceso, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_imp_cta1, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_imp_cta2, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_imp_cta3, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_imp_cta4, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_imp_cta5, "|",
                      g_arr_cta_importe[v_recorre_arreglo].v_sum_proc, "|"
    
      CALL v_ch_arch_salida.write([v_detalle])

   END FOR

   {DISPLAY "Saldo 1 --",v_saldo_cta1
         DISPLAY "Saldo 2 --",v_saldo_cta2
         DISPLAY "Saldo 3 --",v_saldo_cta3
         DISPLAY "Saldo 4 --",v_saldo_cta4
         DISPLAY "Saldo 5 --",v_saldo_cta5
         DISPLAY "Saldo T --",v_saldo_suma}
   --Escribe el sumario
    LET v_sumario = "Saldos|",v_saldo_cta1,"|",
                     v_saldo_cta2, "|",
                     v_saldo_cta3,  "|",
                     v_saldo_cta4, "|",
                     v_saldo_cta5, "|",
                     v_saldo_suma, "|"

   CALL v_ch_arch_salida.write([v_sumario])
   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   
   --Cambia el formato del archivo a DOS
   LET v_comando_dos = "unix2dos ",v_ruta_envio_cnt CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   CALL fn_mensaje("Información","Se ha generado el archivo de Totales por cuenta contable\n en la ruta"||v_ruta_nomarch,"information")
   

END FUNCTION 


#Objetivo: Función para generar reporte de la validación de procesos
FUNCTION fn_reporte_valida_proceso(v_rsaldo_cta1,v_rsaldo_cta2,v_rsaldo_cta3,
                                   v_rsaldo_cta4,v_rsaldo_cta5,v_rsaldo_suma,
                                   v_rep_col1,v_rep_col2,v_rep_col3,v_rep_col4,
                                   v_rep_col5)
DEFINE v_rep_col1,v_rep_col2,
       v_rep_col3,v_rep_col4,
       v_rep_col5     VARCHAR(10)
DEFINE v_rsaldo_cta1  DECIMAL(22,2),
       v_rsaldo_cta2  DECIMAL(22,2),
       v_rsaldo_cta3  DECIMAL(22,2),
       v_rsaldo_cta4  DECIMAL(22,2),
       v_rsaldo_cta5  DECIMAL(22,2),
       v_rsaldo_suma  DECIMAL(22,2)
DEFINE manejador_rpt  om.SaxDocumentHandler  -- Contenedor documentos reporte

   -- Botón para generar el reporte en PDF de la consulta
   IF fgl_report_loadCurrentSettings("CNTC022.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   END IF

   --Inicia el reporte de registros con rechazo
   START REPORT rpt_val_proc_cnt TO XML HANDLER manejador_rpt
      FOR i = 1 TO g_arr_cta_importe.getLength()
      OUTPUT TO REPORT rpt_val_proc_cnt(g_arr_cta_importe[i].*,
                                        g_usuario,v_rsaldo_cta1,v_rsaldo_cta2,
                                                  v_rsaldo_cta3,v_rsaldo_cta4,
                                                  v_rsaldo_cta5,v_rsaldo_suma,
                                        v_rep_col1,v_rep_col2,v_rep_col3,
                                        v_rep_col4,v_rep_col5)
      END FOR

   FINISH REPORT rpt_val_proc_cnt

END FUNCTION
#Objetivo: Estructura del reporte para la validación de procesos
REPORT rpt_val_proc_cnt(v_rep_cta_importe,v_usuario,v_rsaldo_cta1,v_rsaldo_cta2,
                        v_rsaldo_cta3,v_rsaldo_cta4,v_rsaldo_cta5,v_rsaldo_suma,
                        v_rep_col1,v_rep_col2,v_rep_col3,v_rep_col4,v_rep_col5)
DEFINE v_rep_col1,v_rep_col2,v_rep_col3,
       v_rep_col4,v_rep_col5 VARCHAR(10)
DEFINE v_rep_cta_importe RECORD
          v_rep_proceso  VARCHAR(50),
          v_rep_imp_cta1 DECIMAL(22,2),
          v_rep_imp_cta2 DECIMAL(22,2),
          v_rep_imp_cta3 DECIMAL(22,2),
          v_rep_imp_cta4 DECIMAL(22,2),
          v_rep_imp_cta5 DECIMAL(22,2),
          v_rep_sum_proc DECIMAL(22,2)
                         END RECORD
DEFINE v_rsaldo_cta1  DECIMAL(22,2),
       v_rsaldo_cta2  DECIMAL(22,2),
       v_rsaldo_cta3  DECIMAL(22,2),
       v_rsaldo_cta4  DECIMAL(22,2),
       v_rsaldo_cta5  DECIMAL(22,2),
       v_rsaldo_suma  DECIMAL(22,2)
DEFINE v_usuario LIKE seg_usuario.usuario_cod,
       v_fecha_reporte DATE

FORMAT
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY 
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_usuario
      PRINTX v_rep_col1
      PRINTX v_rep_col2
      PRINTX v_rep_col3
      PRINTX v_rep_col4
      PRINTX v_rep_col5

   ON EVERY ROW
      PRINTX v_rep_cta_importe.v_rep_proceso
      PRINTX v_rep_cta_importe.v_rep_imp_cta1
      PRINTX v_rep_cta_importe.v_rep_imp_cta2
      PRINTX v_rep_cta_importe.v_rep_imp_cta3
      PRINTX v_rep_cta_importe.v_rep_imp_cta4
      PRINTX v_rep_cta_importe.v_rep_imp_cta5
      PRINTX v_rep_cta_importe.v_rep_sum_proc

   ON LAST ROW
      PRINTX v_rsaldo_cta1
      PRINTX v_rsaldo_cta2
      PRINTX v_rsaldo_cta3
      PRINTX v_rsaldo_cta4
      PRINTX v_rsaldo_cta5
      PRINTX v_rsaldo_suma

END REPORT
#Objetivo: Asigna  titulos  al cuadro validción  por proceso
FUNCTION fn_nombre_encabezado()
   DEFINE win ui.Window, fm ui.Form,
          v_incremento INTEGER,
          v_titulo    STRING 
   LET win = ui.Window.getCurrent()
   LET fm = win.getForm()    
    FOR v_incremento=1 TO v_arr_ctas_per_cons.getLength()
       LET  v_titulo = "formonly.tb_cta"||v_incremento
       CALL fm.setElementText(v_titulo,v_arr_ctas_per_cons[v_incremento])
    END FOR 
    
END FUNCTION