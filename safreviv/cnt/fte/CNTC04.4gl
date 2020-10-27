################################################################################
# VERSION: 1.0.0                                                               #
# FECHA ULTIMA MODIFICACION: 15/05/2012                                        #
################################################################################
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#MODULO            => CNT                                                      #
#PROGRAMA          => CNTC04                                                   #
#OBJETIVO          => PROGRAMA DE CONSULTA Y GENERACIÓN DEL REPORTE DEL CUADRO #
#                     DE VALIDACIÓN DE PROCESOS                                #
#FECHA INICIO      => 15/05/2012                                               #
################################################################################
DATABASE
   safre_viv
GLOBALS "CNTG01.4gl"
GLOBALS
DEFINE v_cmb_proceso SMALLINT,  --Variable para clave de proceso_cnt
       g_f_inicial DATE,   --Fecha inicial para consulta
       g_f_final   DATE,   --Fecha final para consulta
       v_QryTxt    STRING,
       v_indice    INTEGER,
       v_indice1   SMALLINT,
       v_indice2   SMALLINT,
       v_indice3   SMALLINT,
       v_indice4   SMALLINT,
       v_bnd_suma SMALLINT,
       v_tit_proc  CHAR(08),
       v_tot_proc LIKE cnt_transaccion.importe

DEFINE g_arr_ctas_importe DYNAMIC ARRAY OF RECORD
          cta_contable LIKE cnt_transaccion.cta_contable,
          importe LIKE cnt_transaccion.importe,
          proceso_cnt LIKE cnt_transaccion.cod_transaccion_cnt,
          count_procesos INTEGER 
       END RECORD

DEFINE arr_enc_procesos DYNAMIC ARRAY OF RECORD 
          v_proc_referencia VARCHAR(10)
       END RECORD     
--Arreglo de importes       
DEFINE g_arr_ctas_procesos DYNAMIC ARRAY OF RECORD
          cta_contable CHAR(10),
          imp_proc1 ,imp_proc2 ,imp_proc3 ,imp_proc4 ,imp_proc5 ,imp_proc6 ,
          imp_proc7 ,imp_proc8 ,imp_proc10,imp_proc11,imp_proc12,imp_proc14,
          imp_proc15,imp_proc16,imp_proc17,imp_proc18,imp_proc19,imp_proc20,
          imp_proc22,imp_proc23,imp_proc24,imp_proc25,imp_proc26,imp_proc27,
          imp_proc28,imp_proc29,imp_proc30,imp_proc31,imp_proc32,imp_proc33,
          imp_proc34,imp_proc36,imp_proc37,imp_proc38,imp_proc39,imp_proc42,
          imp_proc43, imp_proc46, imp_proc47, imp_proc48, imp_proc49 
          ,imp_proc50,imp_proc51,imp_proc52,imp_proc53,imp_proc54,imp_proc55,
          imp_proc56,imp_proc57,imp_proc58,imp_proc59,imp_proc60,imp_proc61, imp_proc62
          DECIMAL(22,2)
       END RECORD

DEFINE g_arr_ctas_procesos_agrupado DYNAMIC ARRAY OF RECORD
          cta_contable CHAR(10),
          imp_proc1 ,imp_proc2 ,imp_proc3 ,imp_proc4 ,imp_proc5 ,imp_proc6 ,
          imp_proc7 ,imp_proc8 ,imp_proc10,imp_proc11,imp_proc12,imp_proc14,
          imp_proc15,imp_proc16,imp_proc17,imp_proc18,imp_proc19,imp_proc20,
          imp_proc22,imp_proc23,imp_proc24,imp_proc25,imp_proc26,imp_proc27,
          imp_proc28,imp_proc29,imp_proc30,imp_proc31,imp_proc32,imp_proc33,
          imp_proc34,imp_proc36,imp_proc37,imp_proc38,imp_proc39,imp_proc42,
          imp_proc43, imp_proc46, imp_proc47, imp_proc48, imp_proc49 
          ,imp_proc50,imp_proc51,imp_proc52,imp_proc53,imp_proc54,imp_proc55,
          imp_proc56,imp_proc57,imp_proc58,imp_proc59,imp_proc60,imp_proc61, imp_proc62
          DECIMAL(22,2)
       END RECORD
 
--Variable de totales por proceso
DEFINE columna1  ,tot_proc1 ,tot_proc2 ,tot_proc3 ,tot_proc4 ,tot_proc5 ,
       tot_proc6 ,tot_proc7 ,tot_proc8 ,tot_proc10,tot_proc11,tot_proc12,
       tot_proc14,tot_proc15,tot_proc16,tot_proc17,tot_proc18,tot_proc19,
       tot_proc20,tot_proc22,tot_proc23,tot_proc24,tot_proc25,tot_proc26,
       tot_proc27,tot_proc28,tot_proc29,tot_proc30,tot_proc31,tot_proc32,
       tot_proc33,tot_proc34,tot_proc36,tot_proc37,tot_proc38,tot_proc39,
       tot_proc42, tot_proc43, tot_proc46, tot_proc47, tot_proc48, tot_proc49
       ,tot_proc50,tot_proc51,tot_proc52,tot_proc53,tot_proc54,tot_proc55,
          tot_proc56,tot_proc57,tot_proc58,tot_proc59,tot_proc60,tot_proc61, tot_proc62
       DECIMAL(22,2)   

--Variables para definir cuáles procesos mostrar
DEFINE bnd_proc1 ,bnd_proc2 ,bnd_proc3 ,bnd_proc4 ,bnd_proc5 ,
       bnd_proc6 ,bnd_proc7 ,bnd_proc8 ,bnd_proc10,bnd_proc11,bnd_proc12,
       bnd_proc14,bnd_proc15,bnd_proc16,bnd_proc17,bnd_proc18,bnd_proc19,
       bnd_proc20,bnd_proc22,bnd_proc23,bnd_proc24,bnd_proc25,bnd_proc26,
       bnd_proc27,bnd_proc28,bnd_proc29,bnd_proc30,bnd_proc31,bnd_proc32,
       bnd_proc33,bnd_proc34,bnd_proc36,bnd_proc37,bnd_proc38,bnd_proc39,
       bnd_proc42,bnd_proc43, bnd_proc46, bnd_proc47, bnd_proc48, bnd_proc49
      ,bnd_proc50,bnd_proc51,bnd_proc52,bnd_proc53,bnd_proc54,bnd_proc55,
      bnd_proc56,bnd_proc57,bnd_proc58,bnd_proc59,bnd_proc60,bnd_proc61, bnd_proc62
       
       SMALLINT 
       
DEFINE g_arr_totales_proc DYNAMIC ARRAY OF RECORD 
          columna1, tot_proc1 ,tot_proc2 ,tot_proc3 ,tot_proc4 ,tot_proc5 ,
          tot_proc6 ,tot_proc7 ,tot_proc8 ,tot_proc10,tot_proc11,tot_proc12,
          tot_proc14,tot_proc15,tot_proc16,tot_proc17,tot_proc18,tot_proc19,
          tot_proc20,tot_proc22,tot_proc23,tot_proc24,tot_proc25,tot_proc26,
          tot_proc27,tot_proc28,tot_proc29,tot_proc30,tot_proc31,tot_proc32,
          tot_proc33,tot_proc34,tot_proc36,tot_proc37,tot_proc38,tot_proc39,
          tot_proc42,tot_proc43, tot_proc46, tot_proc47, tot_proc48, tot_proc49 
          ,tot_proc50,tot_proc51,tot_proc52,tot_proc53,tot_proc54,tot_proc55,
          tot_proc56,tot_proc57,tot_proc58,tot_proc59,tot_proc60,tot_proc61, tot_proc62
          DECIMAL(22,2)   
       END RECORD

DEFINE v_tit_proc1 ,v_tit_proc2 ,v_tit_proc3 ,v_tit_proc4 ,v_tit_proc5 ,
       v_tit_proc6 ,v_tit_proc7 ,v_tit_proc8 ,v_tit_proc10,v_tit_proc11,
       v_tit_proc12,v_tit_proc14,v_tit_proc15,v_tit_proc16,v_tit_proc17,
       v_tit_proc18,v_tit_proc19,v_tit_proc20,v_tit_proc22,v_tit_proc23,
       v_tit_proc24,v_tit_proc25,v_tit_proc26,v_tit_proc27,v_tit_proc28,
       v_tit_proc29,v_tit_proc30,v_tit_proc31,v_tit_proc32,v_tit_proc33,
       v_tit_proc34,v_tit_proc36,v_tit_proc37,v_tit_proc38,v_tit_proc39,
       v_tit_proc42,v_tit_proc43, v_tit_proc46, v_tit_proc47, v_tit_proc48,
       v_tit_proc49
       ,v_tit_proc50,v_tit_proc51,v_tit_proc52,v_tit_proc53,v_tit_proc54,v_tit_proc55,
       v_tit_proc56,v_tit_proc57,v_tit_proc58,v_tit_proc59,v_tit_proc60,v_tit_proc61, v_tit_proc62
       CHAR(08)

--Arreglo para almacenar las claves del proceso contable
DEFINE 
   g_arr_claves DYNAMIC ARRAY OF RECORD 
         arr_referencia    VARCHAR (8),--LIKE cat_proceso_cnt.referencia_cnt,
         arr_descripcion   LIKE cat_proceso_cnt.desc_proc_corta_cnt 
   
   END RECORD 
   

   DEFINE 
      v_encabezado_cuentas    STRING 
END GLOBALS 

MAIN
DEFINE f_ventana     ui.Window, -- Define las propìedades de la Ventana
       f_forma       ui.Form    -- Define las propiedades de la forma

   -- se asignan los parametros que vienen del fglrun
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog     = ARG_VAL(3)

   LET v_encabezado_cuentas = ""
   
   -- se asigna el titulo del programa
   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF
   
CLOSE WINDOW SCREEN   
OPEN WINDOW vtn_consulta WITH FORM "CNTC041"
   DIALOG ATTRIBUTES(UNBUFFERED)
   
      INPUT v_cmb_proceso, g_f_inicial, g_f_final
      FROM v_cmb_proceso, f_inicial, f_final
         BEFORE INPUT
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()
            CALL f_forma.setElementHidden("grp_valida_proc",1)
            CALL f_forma.setElementHidden("grp_valida_totales",1)            
            CALL DIALOG.setActionHidden("reporte",1)
            CALL DIALOG.setActionHidden("archivo",1)
            --Se llena el combo para seleccionar el proceso  
            CALL fn_llena_combo_proceso()
            CALL fn_obtiene_claves_cnt()


             ON ACTION cancelar 
               EXIT DIALOG
            
            ON ACTION ACCEPT 
               --Valida los parametros para consulta
               IF g_f_inicial IS NULL AND g_f_final IS NOT NULL THEN
                  CALL fn_mensaje("Atención", "Ambas fechas son requeridas", "stop")
                  NEXT FIELD f_inicial
               ELSE              
                  IF g_f_inicial IS NOT NULL AND g_f_final IS NULL THEN
                     CALL fn_mensaje("Atención", "Ambas fechas son requeridas", "stop")
                     NEXT FIELD f_inicial
                  END IF 
               END IF

               --Función para obtner cuentas, importe y proceso al que pertenece
               CALL fn_cons_ctas()
               --Función para obtener importe por proceso
               CALL fn_consulta_ctas_procesos()
               --Función para obtener la sumatoria por proceso
               --CALL fn_obtiene_totales()

               CALL fn_agrupa_cuentas()
                  IF v_indice < = 1 THEN 
                     CALL fn_mensaje ("Atención", 
                                      "No se encontró informacion con los parámetros proporcionados",
                                      "about")
                  ELSE
                     CALL fn_obtiene_titulos_tabla()
                     CALL f_forma.setElementHidden("grp_valida_proc",0)
                     CALL f_forma.setElementHidden("grp_valida_totales",0)
                     CALL DIALOG.setActionHidden("archivo",0)
                     
                         IF bnd_proc1 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_1",1)
                           CALL f_forma.setElementHidden("formonly.tot1",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = 1
                           LET v_tit_proc = "1-PAG1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc2 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_2",1)
                           CALL f_forma.setElementHidden("formonly.tot2",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "2-ACR1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc3 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_3",1)
                           CALL f_forma.setElementHidden("formonly.tot3",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "3-RET1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc4 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_4",1)
                           CALL f_forma.setElementHidden("formonly.tot4",1)
                           LET v_tit_proc = ""                        
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "4-RST1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc5 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_5",1)
                           CALL f_forma.setElementHidden("formonly.tot5",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "5-PAG2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc6 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_6",1)
                           CALL f_forma.setElementHidden("formonly.tot6",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1    
                           LET v_tit_proc = "6-TIA1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc7 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_7",1)
                           CALL f_forma.setElementHidden("formonly.tot7",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "7-REN1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc 
                         END IF
                         
                         IF bnd_proc8 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_8",1)
                           CALL f_forma.setElementHidden("formonly.tot8",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "8-RET2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc10 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_10",1)
                           CALL f_forma.setElementHidden("formonly.tot10",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "10-REN2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc11 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_11",1)
                           CALL f_forma.setElementHidden("formonly.tot11",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "11-DEO1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc12 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_12",1)
                           CALL f_forma.setElementHidden("formonly.tot12",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "12-RST2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc14 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_14",1)
                           CALL f_forma.setElementHidden("formonly.tot14",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "14-RET3"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc15 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_15",1)
                           CALL f_forma.setElementHidden("formonly.tot15",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "15-PAG2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc16 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_16",1)
                           CALL f_forma.setElementHidden("formonly.tot16",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "16-PAG3"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc17 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_17",1)
                           CALL f_forma.setElementHidden("formonly.tot17",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "17-ACL1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc18 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_18",1)
                           CALL f_forma.setElementHidden("formonly.tot18",1)
                           LET v_tit_proc = ""                        
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "18-DIS1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc19 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_19",1)
                           CALL f_forma.setElementHidden("formonly.tot19",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "19-DIS2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc20 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_20",1)
                           CALL f_forma.setElementHidden("formonly.tot20",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "20-TIA3"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc22 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_22",1)
                           CALL f_forma.setElementHidden("formonly.tot22",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "22-RST3"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc23 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_23",1)
                           CALL f_forma.setElementHidden("formonly.tot23",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "23-RST4"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc24 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_24",1)
                           CALL f_forma.setElementHidden("formonly.tot24",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "24-REN3"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc25 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_25",1)
                           CALL f_forma.setElementHidden("formonly.tot25",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "25-REN4"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc26 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_26",1)
                           CALL f_forma.setElementHidden("formonly.tot26",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "26-DEO2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc27 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_27",1)
                           CALL f_forma.setElementHidden("formonly.tot27",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "27-DPE1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc28 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_28",1)
                           CALL f_forma.setElementHidden("formonly.tot28",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "28-DPE2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc29 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_29",1)
                           CALL f_forma.setElementHidden("formonly.tot29",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "29-RET4"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc30 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_30",1)
                           CALL f_forma.setElementHidden("formonly.tot30",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "30-RET5"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc31 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_31",1)
                           CALL f_forma.setElementHidden("formonly.tot31",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "31-RET6"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc32 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_32",1)
                           CALL f_forma.setElementHidden("formonly.tot32",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "32-AG1" 
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc33 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_33",1)
                           CALL f_forma.setElementHidden("formonly.tot33",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "33-AG2" 
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc34 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_34",1)
                           CALL f_forma.setElementHidden("formonly.tot34",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "34-AG3" 
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc36 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_36",1)
                           CALL f_forma.setElementHidden("formonly.tot36",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "36-SEP1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc37 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_37",1)
                           CALL f_forma.setElementHidden("formonly.tot37",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "37-UNI1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc38 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_38",1)
                           CALL f_forma.setElementHidden("formonly.tot38",1)
                           LET v_tit_proc = "" 
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "38-R731"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc39 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_39",1)
                           CALL f_forma.setElementHidden("formonly.tot39",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "39-R732"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc42 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_42",1)
                           CALL f_forma.setElementHidden("formonly.tot42",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "42-FOR1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc43 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_43",1)
                           CALL f_forma.setElementHidden("formonly.tot43",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "43-FOR2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc46 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_46",1)
                           CALL f_forma.setElementHidden("formonly.tot46",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "46-CON1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc47 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_47",1)
                           CALL f_forma.setElementHidden("formonly.tot47",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "47-CON2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc48 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_48",1)
                           CALL f_forma.setElementHidden("formonly.tot48",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "48-CON3"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc49 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_49",1)
                           CALL f_forma.setElementHidden("formonly.tot49",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "49-RET7"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc50 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_50",1)
                           CALL f_forma.setElementHidden("formonly.tot50",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "50-SEP2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc51 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_51",1)
                           CALL f_forma.setElementHidden("formonly.tot51",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "51-DIS5"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc52 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_52",1)
                           CALL f_forma.setElementHidden("formonly.tot52",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "52-VOL1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         IF bnd_proc53 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_53",1)
                           CALL f_forma.setElementHidden("formonly.tot53",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "53-DAE1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc54 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_54",1)
                           CALL f_forma.setElementHidden("formonly.tot54",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "54-CPR1"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc55 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_55",1)
                           CALL f_forma.setElementHidden("formonly.tot55",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "55-CPR2"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc56 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_56",1)
                           CALL f_forma.setElementHidden("formonly.tot56",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "56-CPR3"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc57 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_57",1)
                           CALL f_forma.setElementHidden("formonly.tot57",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "57-RET7"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc58 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_58",1)
                           CALL f_forma.setElementHidden("formonly.tot58",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "58-CPR4"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc59 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_59",1)
                           CALL f_forma.setElementHidden("formonly.tot59",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "59-CPR5"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc60 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_60",1)
                           CALL f_forma.setElementHidden("formonly.tot60",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "60-CPR6"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc61 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_61",1)
                           CALL f_forma.setElementHidden("formonly.tot61",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "61-R734"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF

                         IF bnd_proc62 = 0 THEN
                           CALL f_forma.setElementHidden("formonly.proceso_62",1)
                           CALL f_forma.setElementHidden("formonly.tot62",1)
                           LET v_tit_proc = ""
                         ELSE 
                           LET v_bnd_suma = v_bnd_suma + 1
                           LET v_tit_proc = "62-CPR7"
                           LET v_encabezado_cuentas = v_encabezado_cuentas, "|", v_tit_proc
                         END IF
                         
                         
                         CALL DIALOG.setActionHidden("accept", 1)
                        
                  END IF   

                  IF v_bnd_suma = 1 THEN
                     CALL DIALOG.setActionHidden("reporte",0)
                  END IF 
            ON ACTION reporte 
               CALL fn_consulta_reporte()

            ON ACTION archivo
               CALL fn_genera_archivo_validacion()
            ON ACTION claves
               OPEN WINDOW vtn_claves WITH FORM "CNTC042"
                  DIALOG   ATTRIBUTES(UNBUFFERED) 

                        DISPLAY ARRAY g_arr_claves TO src_claves.* END DISPLAY 

                        ON ACTION regresar
                           EXIT DIALOG 
                        
                  END DIALOG 

               CLOSE WINDOW vtn_claves
            
      END INPUT


  --DISPLAY ARRAY g_arr_ctas_procesos TO scr_detalles_ctas.*
  DISPLAY ARRAY g_arr_ctas_procesos_agrupado TO scr_detalles_ctas.*  
         --BEFORE DISPLAY 
           -- CALL fn_agrupa_cuentas()
         ON ACTION claves
               OPEN WINDOW vtn_claves WITH FORM "CNTC042"
                  DIALOG   ATTRIBUTES(UNBUFFERED) 

                        DISPLAY ARRAY g_arr_claves TO src_claves.* END DISPLAY 

                        ON ACTION regresar
                           EXIT DIALOG 
                  END DIALOG 

               CLOSE WINDOW vtn_claves
  
         ON ACTION cancelar
            EXIT DIALOG    
      END DISPLAY 

      DISPLAY ARRAY g_arr_totales_proc TO scr_totales_proceso.*
      END DISPLAY 
      
   END DIALOG  
CLOSE WINDOW vtn_consulta

END MAIN

#OBJETIVO: Consultar las cuentas por proceso o periodo  
FUNCTION fn_cons_ctas()

   DEFINE 
      v_criterios    STRING,
      v_naturaleza   SMALLINT, --1 es Abono; 2 es cargo
      v_total_pos    SMALLINT 
      
   LET v_criterios = ""
   LET v_total_pos = 1 --La posición del arreglo para los totales

   --Inicializa totales
   LET tot_proc1  = 0.00
   LET tot_proc2  = 0.00
   LET tot_proc3  = 0.00
   LET tot_proc4  = 0.00
   LET tot_proc5  = 0.00
   LET tot_proc6  = 0.00
   LET tot_proc7  = 0.00
   LET tot_proc8  = 0.00
   LET tot_proc10 = 0.00
   LET tot_proc11 = 0.00
   LET tot_proc12 = 0.00
   LET tot_proc14 = 0.00
   LET tot_proc15 = 0.00
   LET tot_proc16 = 0.00
   LET tot_proc17 = 0.00
   LET tot_proc18 = 0.00
   LET tot_proc19 = 0.00
   LET tot_proc20 = 0.00
   LET tot_proc22 = 0.00
   LET tot_proc23 = 0.00
   LET tot_proc24 = 0.00
   LET tot_proc25 = 0.00
   LET tot_proc26 = 0.00
   LET tot_proc27 = 0.00
   LET tot_proc28 = 0.00
   LET tot_proc29 = 0.00
   LET tot_proc30 = 0.00
   LET tot_proc31 = 0.00
   LET tot_proc32 = 0.00
   LET tot_proc33 = 0.00
   LET tot_proc34 = 0.00
   LET tot_proc36 = 0.00
   LET tot_proc37 = 0.00
   LET tot_proc38 = 0.00
   LET tot_proc39 = 0.00
   LET tot_proc42 = 0.00
   LET tot_proc43 = 0.00
   LET tot_proc46 = 0.00
   LET tot_proc47 = 0.00
   LET tot_proc48 = 0.00
   LET tot_proc49 = 0.00
   
   LET tot_proc50 = 0.00
   LET tot_proc51 = 0.00
   LET tot_proc52 = 0.00
   LET tot_proc53 = 0.00
   LET tot_proc54 = 0.00
   LET tot_proc55 = 0.00
   LET tot_proc56 = 0.00
   LET tot_proc57 = 0.00
   LET tot_proc58 = 0.00
   LET tot_proc59 = 0.00
   LET tot_proc60 = 0.00
   LET tot_proc61 = 0.00
   LET tot_proc62 = 0.00
   
   
   --Inicializa banderas para saber qué columnas mostrar en la forma
   LET bnd_proc1  = 0 --0 no muestra columna; 1 se muestra la columna del proceso
   LET bnd_proc2  = 0
   LET bnd_proc3  = 0
   LET bnd_proc4  = 0
   LET bnd_proc5  = 0
   LET bnd_proc6  = 0
   LET bnd_proc7  = 0
   LET bnd_proc8  = 0
   LET bnd_proc10 = 0
   LET bnd_proc11 = 0
   LET bnd_proc12 = 0
   LET bnd_proc14 = 0
   LET bnd_proc15 = 0
   LET bnd_proc16 = 0
   LET bnd_proc17 = 0
   LET bnd_proc18 = 0
   LET bnd_proc19 = 0
   LET bnd_proc20 = 0
   LET bnd_proc22 = 0
   LET bnd_proc23 = 0
   LET bnd_proc24 = 0
   LET bnd_proc25 = 0
   LET bnd_proc26 = 0
   LET bnd_proc27 = 0
   LET bnd_proc28 = 0
   LET bnd_proc29 = 0
   LET bnd_proc30 = 0
   LET bnd_proc31 = 0
   LET bnd_proc32 = 0
   LET bnd_proc33 = 0
   LET bnd_proc34 = 0
   LET bnd_proc36 = 0
   LET bnd_proc37 = 0
   LET bnd_proc38 = 0
   LET bnd_proc39 = 0
   LET bnd_proc42 = 0
   LET bnd_proc43 = 0
   LET bnd_proc46 = 0
   LET bnd_proc47 = 0
   LET bnd_proc48 = 0
   LET bnd_proc49 = 0
   
   LET bnd_proc50 = 0
   LET bnd_proc51 = 0
   LET bnd_proc52 = 0
   LET bnd_proc53 = 0
   LET bnd_proc54 = 0
   LET bnd_proc55 = 0
   LET bnd_proc56 = 0
   LET bnd_proc57 = 0
   LET bnd_proc58 = 0
   LET bnd_proc59 = 0
   LET bnd_proc60 = 0
   LET bnd_proc61 = 0
   LET bnd_proc62 = 0
   

   --Crea tabla temporal para guardar importes
   CALL fn_crea_tabla_tem_cuentas()
   CALL fn_crea_tabla_tem_cargo()
   CALL fn_crea_tabla_tem_abono()

   --Se arma query dinámico
   
      --Consulta por procesos
   IF v_cmb_proceso IS NOT NULL AND g_f_inicial IS NULL AND g_f_final IS NULL THEN
      LET v_criterios = v_criterios, "\n AND cod_proceso_cnt =", v_cmb_proceso 
   END IF

   --Consulta por fechas
   IF v_cmb_proceso IS NULL AND g_f_inicial IS NOT NULL 
   AND g_f_final IS NOT NULL THEN
      LET v_criterios = v_criterios, "\n AND f_liquida BETWEEN '",g_f_inicial,
                                       "' AND '",g_f_final,"'" 
   END IF
   
   --Consulta por fechas y procesos
   IF v_cmb_proceso IS NOT NULL AND g_f_inicial IS NOT NULL
   AND g_f_final IS NOT NULL THEN
      LET v_criterios = v_criterios, "\n AND cod_proceso_cnt =", v_cmb_proceso 
      LET v_criterios = v_criterios, "\n AND f_liquida BETWEEN '",g_f_inicial,
                                    "' AND '",g_f_final,"'" 
   END IF


   LET v_QryTxt = "\n SELECT cta_contable, SUM(importe), cod_proceso_cnt,cod_naturaleza_cta",
                  "\n FROM cnt_transaccion", 
                  "\n WHERE 1=1 " 


   LET v_QryTxt = v_QryTxt, v_criterios
   LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,3,4 \n ORDER BY 1" 

   DISPLAY "La consulta!!! -- ",v_QryTxt

   PREPARE prp_consulta_cuentas FROM v_QryTxt
   DECLARE cur_cons_ctas CURSOR FOR prp_consulta_cuentas

   LET v_indice = 1
   FOREACH cur_cons_ctas INTO g_arr_ctas_importe[v_indice].cta_contable,
                              g_arr_ctas_importe[v_indice].importe,
                              g_arr_ctas_importe[v_indice].proceso_cnt,
                              
                              v_naturaleza

   --DISPLAY "Datos de la consulta: \n"
   --DISPLAY g_arr_ctas_importe[v_indice].*
                            

   DISPLAY "Naturaleza de la cuenta -- ",v_naturaleza
   IF v_naturaleza = 1 THEN  --Si es abono. hace el importe negativo
      LET g_arr_ctas_importe[v_indice].importe = (g_arr_ctas_importe[v_indice].importe * -1)
   END IF 
                              
   ########################################################

   IF g_arr_ctas_importe[v_indice].proceso_cnt = 1 THEN
      --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
      LET bnd_proc1  = 1 --Se mostrará la columna del proceso
      INSERT INTO safre_tmp:tmp_id_proceso_cnt
      VALUES(g_arr_ctas_importe[v_indice].cta_contable,
             g_arr_ctas_importe[v_indice].importe,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc1  = tot_proc1 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc1 -- ",tot_proc1
             
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 2 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc2  = 1
      INSERT INTO safre_tmp:tmp_id_proceso_cnt
      VALUES(g_arr_ctas_importe[v_indice].cta_contable,
             0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )   

      --Para la sumatoria
      LET tot_proc2  = tot_proc2 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc2 -- ",tot_proc2
             
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 3 THEN
      --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
      LET bnd_proc3  = 1
      INSERT INTO safre_tmp:tmp_id_proceso_cnt
      VALUES(g_arr_ctas_importe[v_indice].cta_contable,
             0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
             0.00 --49
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )     

      --Para la sumatoria
      LET tot_proc3  = tot_proc3 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc3 -- ",tot_proc3

   END IF

   

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 4 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt 
       LET bnd_proc4  = 1  
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc4  = tot_proc4 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc4 -- ",tot_proc4
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 5 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc5  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc5  = tot_proc5 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc5 -- ",tot_proc5
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 6 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc6  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc6  = tot_proc6 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc6 -- ",tot_proc6
              
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 7 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt 
       LET bnd_proc7  = 1   
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc7  = tot_proc7 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc7 -- ",tot_proc7
              
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 8 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc8  = 1    
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc8  = tot_proc8 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc8 -- ",tot_proc8
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 10 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc10  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc10  = tot_proc10 + g_arr_ctas_importe[v_indice].importe    
      --DISPLAY "tot_proc10 -- ",tot_proc10
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 11 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc11  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc11  = tot_proc11 + g_arr_ctas_importe[v_indice].importe   
     --DISPLAY "tot_proc11 -- ",tot_proc11 
              
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 12 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc12  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc12  = tot_proc12 + g_arr_ctas_importe[v_indice].importe  
    --DISPLAY "tot_proc12 -- ",tot_proc12  
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 14 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc14  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )  

      --Para la sumatoria
      LET tot_proc14  = tot_proc14 + g_arr_ctas_importe[v_indice].importe 
      --DISPLAY "tot_proc14 -- ",tot_proc14   
              
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 15 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc15  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc15  = tot_proc15 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc15 -- ",tot_proc15    
              
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 16 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc16  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc16  = tot_proc16 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc16 -- ",tot_proc16    
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 17 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc17  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc17  = tot_proc17 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc17 -- ",tot_proc17    
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 18 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc18  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc18  = tot_proc18 + g_arr_ctas_importe[v_indice].importe 
      --DISPLAY "tot_proc18 -- ",tot_proc18   
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 19 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc19  = 1  
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 19 POSICION 17
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc19  = tot_proc19 + g_arr_ctas_importe[v_indice].importe   
     --DISPLAY "tot_proc19 -- ",tot_proc19 
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 20 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc20  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 20 POSICION 18
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc20  = tot_proc20 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc20 -- ",tot_proc20
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 22 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc22  = 1    
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 22 POSICION 19
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc22  = tot_proc22 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc22 -- ",tot_proc22    
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 23 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc23  = 1    
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 23 POSICION 20
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc23  = tot_proc23 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc23 -- ",tot_proc23      
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 24 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc24  = 1    
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 24 POSICION 21
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc24 = tot_proc24 + g_arr_ctas_importe[v_indice].importe    
      --DISPLAY "tot_proc24 -- ",tot_proc24
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 25 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc25  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 25 POSICION 22
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc25  = tot_proc25 + g_arr_ctas_importe[v_indice].importe 
      --DISPLAY "tot_proc25 -- ",tot_proc25   
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 26 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc26  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 26 POSICION 23
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc26  = tot_proc26 + g_arr_ctas_importe[v_indice].importe    
      --DISPLAY "tot_proc26 -- ",tot_proc26
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 27 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc27  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 27 POSICION 24
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--23
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc27  = tot_proc27 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc27 -- ",tot_proc27    
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 28 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc28  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 28 POSICION 25
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc28  = tot_proc28 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc28 -- ",tot_proc28    
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 29 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc29  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 29 POSICION 26
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc29  = tot_proc29 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc29 -- ",tot_proc29    
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 30 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc30  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 30 POSICION 27
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc30  = tot_proc30 + g_arr_ctas_importe[v_indice].importe  
    --DISPLAY "tot_proc30 -- ",tot_proc30  
    END IF 
    
    IF g_arr_ctas_importe[v_indice].proceso_cnt = 31 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc31  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 31 POSICION 28
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc31  = tot_proc31 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc31 -- ",tot_proc31 
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 32 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc32  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 32 POSICION 29
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,--29
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )
            

      --Para la sumatoria
      LET tot_proc32  = tot_proc32 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc32 -- ",tot_proc32 
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 33 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc33  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 33 POSICION 30
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc33  = tot_proc33 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc33 -- ",tot_proc33 
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 34 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc34  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 34 POSICION 31
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc34  = tot_proc34 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc34 -- ",tot_proc34 
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 36 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc36  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 36 POSICION 32
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc36  = tot_proc36 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc36 -- ",tot_proc36 
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 37 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc37  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 37 POSICION 33
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc37  = tot_proc37 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc37 -- ",tot_proc37 
    END IF 
    
    IF g_arr_ctas_importe[v_indice].proceso_cnt = 38 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc38  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 38 POSICION 34
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc38  = tot_proc38 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc38 -- ",tot_proc38 
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 39 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc39  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 39 POSICION 35
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc39  = tot_proc39 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc39 -- ",tot_proc39 
    END IF 


    IF g_arr_ctas_importe[v_indice].proceso_cnt = 42 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc42  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
--PROCESO 39 POSICION 35
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,g_arr_ctas_importe[v_indice].importe,0.00,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc42  = tot_proc42 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc42 -- ",tot_proc42
    END IF 


    IF g_arr_ctas_importe[v_indice].proceso_cnt = 43 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc43  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt

       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,g_arr_ctas_importe[v_indice].importe,
              0.00,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc43  = tot_proc43 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc43 -- ",tot_proc43
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 46 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc46  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt

       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc46  = tot_proc46 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc43 -- ",tot_proc43
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 47 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc47  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt

       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc47  = tot_proc47 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc43 -- ",tot_proc43
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 48 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc48  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt

       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc48  = tot_proc48 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc43 -- ",tot_proc43
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 49 THEN
       --DISPLAY "ENTRO EN EL NUMERO #  ", g_arr_ctas_importe[v_indice].proceso_cnt
       LET bnd_proc49  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt

       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe
             --Del 50 al 62
             ,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )

      --Para la sumatoria
      LET tot_proc49  = tot_proc49 + g_arr_ctas_importe[v_indice].importe
      --DISPLAY "tot_proc43 -- ",tot_proc43
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 50 THEN
       LET bnd_proc50  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              g_arr_ctas_importe[v_indice].importe, 0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc50  = tot_proc50 + g_arr_ctas_importe[v_indice].importe
    END IF 

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 51 THEN
       LET bnd_proc51  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc51  = tot_proc51 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 52 THEN
       LET bnd_proc52  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc52  = tot_proc52 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 53 THEN
       LET bnd_proc53  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc53  = tot_proc53 + g_arr_ctas_importe[v_indice].importe
    END IF  

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 54 THEN
       LET bnd_proc54  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc54  = tot_proc54 + g_arr_ctas_importe[v_indice].importe
    END IF  

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 55 THEN
       LET bnd_proc55  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc55  = tot_proc55 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 56 THEN
       LET bnd_proc56  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,0.00,0.00,
              g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc56  = tot_proc56 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 57 THEN
       LET bnd_proc57  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc57  = tot_proc57 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 58 THEN
       LET bnd_proc58  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc58  = tot_proc58 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 59 THEN
       LET bnd_proc59  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc59  = tot_proc59 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 60 THEN
       LET bnd_proc60  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00,0.00
             )
      --Para la sumatoria
      LET tot_proc60  = tot_proc60 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 61 THEN
       LET bnd_proc61  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe,0.00
             )
      --Para la sumatoria
      LET tot_proc61  = tot_proc61 + g_arr_ctas_importe[v_indice].importe
    END IF

    IF g_arr_ctas_importe[v_indice].proceso_cnt = 62 THEN
       LET bnd_proc62  = 1
       INSERT INTO safre_tmp:tmp_id_proceso_cnt
       VALUES(g_arr_ctas_importe[v_indice].cta_contable,
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--12
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,--24
              0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,
              --Del 50 al 62
              0.00,0.00,0.00,0.00,0.00,0.00,
              0.00,0.00,0.00,0.00,0.00,0.00,g_arr_ctas_importe[v_indice].importe
             )
      --Para la sumatoria
      LET tot_proc62  = tot_proc62 + g_arr_ctas_importe[v_indice].importe
    END IF
    
    
   ########################################################
     
   
         LET v_indice = v_indice + 1

   END FOREACH

   CALL g_arr_ctas_importe.deleteElement(v_indice)


   --Asigna totales

   LET g_arr_totales_proc[v_total_pos].tot_proc1 = tot_proc1
   LET g_arr_totales_proc[v_total_pos].tot_proc2 = tot_proc2
   LET g_arr_totales_proc[v_total_pos].tot_proc3 = tot_proc3
   LET g_arr_totales_proc[v_total_pos].tot_proc4 = tot_proc4
   LET g_arr_totales_proc[v_total_pos].tot_proc5 = tot_proc5
   LET g_arr_totales_proc[v_total_pos].tot_proc6 = tot_proc6
   LET g_arr_totales_proc[v_total_pos].tot_proc7 = tot_proc7
   LET g_arr_totales_proc[v_total_pos].tot_proc8 = tot_proc8
   LET g_arr_totales_proc[v_total_pos].tot_proc10 = tot_proc10
   LET g_arr_totales_proc[v_total_pos].tot_proc11 = tot_proc11
   LET g_arr_totales_proc[v_total_pos].tot_proc12 = tot_proc12
   LET g_arr_totales_proc[v_total_pos].tot_proc14 = tot_proc14
   LET g_arr_totales_proc[v_total_pos].tot_proc15 = tot_proc15
   LET g_arr_totales_proc[v_total_pos].tot_proc16 = tot_proc16
   LET g_arr_totales_proc[v_total_pos].tot_proc17 = tot_proc17
   LET g_arr_totales_proc[v_total_pos].tot_proc18 = tot_proc18
   LET g_arr_totales_proc[v_total_pos].tot_proc19 = tot_proc19
   LET g_arr_totales_proc[v_total_pos].tot_proc20 = tot_proc20
   LET g_arr_totales_proc[v_total_pos].tot_proc22 = tot_proc22
   LET g_arr_totales_proc[v_total_pos].tot_proc23 = tot_proc23
   LET g_arr_totales_proc[v_total_pos].tot_proc24 = tot_proc24
   LET g_arr_totales_proc[v_total_pos].tot_proc25 = tot_proc25
   LET g_arr_totales_proc[v_total_pos].tot_proc26 = tot_proc26
   LET g_arr_totales_proc[v_total_pos].tot_proc27 = tot_proc27
   LET g_arr_totales_proc[v_total_pos].tot_proc28 = tot_proc28
   LET g_arr_totales_proc[v_total_pos].tot_proc29 = tot_proc29
   LET g_arr_totales_proc[v_total_pos].tot_proc30 = tot_proc30
   LET g_arr_totales_proc[v_total_pos].tot_proc31 = tot_proc31
   LET g_arr_totales_proc[v_total_pos].tot_proc32 = tot_proc32
   LET g_arr_totales_proc[v_total_pos].tot_proc33 = tot_proc33
   LET g_arr_totales_proc[v_total_pos].tot_proc34 = tot_proc34
   LET g_arr_totales_proc[v_total_pos].tot_proc36 = tot_proc36
   LET g_arr_totales_proc[v_total_pos].tot_proc37 = tot_proc37
   LET g_arr_totales_proc[v_total_pos].tot_proc38 = tot_proc38
   LET g_arr_totales_proc[v_total_pos].tot_proc39 = tot_proc39
   LET g_arr_totales_proc[v_total_pos].tot_proc42 = tot_proc42
   LET g_arr_totales_proc[v_total_pos].tot_proc43 = tot_proc43
   LET g_arr_totales_proc[v_total_pos].tot_proc46 = tot_proc46
   LET g_arr_totales_proc[v_total_pos].tot_proc47 = tot_proc47
   LET g_arr_totales_proc[v_total_pos].tot_proc48 = tot_proc48
   LET g_arr_totales_proc[v_total_pos].tot_proc49 = tot_proc49

   
   LET g_arr_totales_proc[v_total_pos].tot_proc50 =  tot_proc50
   LET g_arr_totales_proc[v_total_pos].tot_proc51 =  tot_proc51
   LET g_arr_totales_proc[v_total_pos].tot_proc52 =  tot_proc52
   LET g_arr_totales_proc[v_total_pos].tot_proc53 =  tot_proc53
   LET g_arr_totales_proc[v_total_pos].tot_proc54 =  tot_proc54
   LET g_arr_totales_proc[v_total_pos].tot_proc55 =  tot_proc55
   LET g_arr_totales_proc[v_total_pos].tot_proc56 =  tot_proc56
   LET g_arr_totales_proc[v_total_pos].tot_proc57 =  tot_proc57
   LET g_arr_totales_proc[v_total_pos].tot_proc58 =  tot_proc58
   LET g_arr_totales_proc[v_total_pos].tot_proc59 =  tot_proc59
   LET g_arr_totales_proc[v_total_pos].tot_proc60 =  tot_proc60
   LET g_arr_totales_proc[v_total_pos].tot_proc61 =  tot_proc61
   LET g_arr_totales_proc[v_total_pos].tot_proc62 =  tot_proc62
   
   
   --DISPLAY "Totales : \n",g_arr_totales_proc[v_total_pos].*
   
END FUNCTION


#OBJETIVO: Obtener las claves de proceso y descripcion 
FUNCTION fn_obtiene_titulos_tabla()
       
 LET v_QryTxt = "\n SELECT cod_proceso_cnt ||'-'|| referencia_cnt",
                "\n   FROM cat_proceso_cnt"

      LET v_indice1 = 1

   PREPARE prp_proc_ref FROM v_QryTxt
   DECLARE cur_proc_ref CURSOR FOR prp_proc_ref

   FOREACH cur_proc_ref INTO arr_enc_procesos[v_indice1].v_proc_referencia
      LET v_indice1 = v_indice1 + 1
   END FOREACH

  CALL arr_enc_procesos.deleteElement(v_indice1)
  
END FUNCTION 

#OBJETIVO: Se crea tabla temporal para almacenar cargos de la consulta
FUNCTION fn_crea_tabla_tem_cuentas()

    WHENEVER ERROR CONTINUE

    CREATE TEMP TABLE tmp_id_proceso_cnt
       (cta_contable CHAR(10),
       imp_proc1    DECIMAL(22,2),
       imp_proc2    DECIMAL(22,2),  
       imp_proc3    DECIMAL(22,2),
       imp_proc4    DECIMAL(22,2),
       imp_proc5    DECIMAL(22,2),
       imp_proc6    DECIMAL(22,2),
       imp_proc7    DECIMAL(22,2),
       imp_proc8    DECIMAL(22,2),
       imp_proc10   DECIMAL(22,2),
       imp_proc11   DECIMAL(22,2),
       imp_proc12   DECIMAL(22,2),
       imp_proc14   DECIMAL(22,2),
       imp_proc15   DECIMAL(22,2),
       imp_proc16   DECIMAL(22,2),
       imp_proc17   DECIMAL(22,2),
       imp_proc18   DECIMAL(22,2),
       imp_proc19   DECIMAL(22,2),
       imp_proc20   DECIMAL(22,2),
       imp_proc22   DECIMAL(22,2),
       imp_proc23   DECIMAL(22,2),
       imp_proc24   DECIMAL(22,2),
       imp_proc25   DECIMAL(22,2),
       imp_proc26   DECIMAL(22,2),
       imp_proc27   DECIMAL(22,2),
       imp_proc28   DECIMAL(22,2),
       imp_proc29   DECIMAL(22,2),
       imp_proc30   DECIMAL(22,2),
       imp_proc31   DECIMAL(22,2),
       imp_proc32   DECIMAL(22,2),
       imp_proc33   DECIMAL(22,2),
       imp_proc34   DECIMAL(22,2),
       imp_proc36   DECIMAL(22,2),
       imp_proc37   DECIMAL(22,2),
       imp_proc38   DECIMAL(22,2),
       imp_proc39   DECIMAL(22,2),
       imp_proc42   DECIMAL(22,2),
       imp_proc43   DECIMAL(22,2),
       imp_proc46   DECIMAL(22,2),
       imp_proc47   DECIMAL(22,2),
       imp_proc48   DECIMAL(22,2),
       imp_proc49   DECIMAL(22,2)

       
      ,imp_proc50   DECIMAL(22,2),
       imp_proc51   DECIMAL(22,2),
       imp_proc52   DECIMAL(22,2),
       imp_proc53   DECIMAL(22,2),
       imp_proc54   DECIMAL(22,2),
       imp_proc55   DECIMAL(22,2),
       imp_proc56   DECIMAL(22,2),
       imp_proc57   DECIMAL(22,2),
       imp_proc58   DECIMAL(22,2),
       imp_proc59   DECIMAL(22,2),
       imp_proc60   DECIMAL(22,2),
       imp_proc61   DECIMAL(22,2),
       imp_proc62   DECIMAL(22,2)
      
      
       );

END FUNCTION

#OBJETIVO: Se crea tabla temporal para almacenar datos de consulta - Cargos
FUNCTION fn_crea_tabla_tem_cargo()

    WHENEVER ERROR CONTINUE

    CREATE TEMP TABLE tmp_importe_cargo
       (cta_contable CHAR(10),
       imp_proceso    DECIMAL(22,2));

END FUNCTION

#OBJETIVO: Se crea tabla temporal para almacenar datos de consulta - Abonos
FUNCTION fn_crea_tabla_tem_abono()

    WHENEVER ERROR CONTINUE

    CREATE TEMP TABLE tmp_importe_abono
       (cta_contable CHAR(10),
       imp_proceso    DECIMAL(22,2));

END FUNCTION

#OBJETIVO: Ejecuta consulta con información para el usuario
FUNCTION fn_consulta_ctas_procesos()

   LET v_QryTxt = "\n SELECT *",
                  "\n FROM  safre_tmp:tmp_id_proceso_cnt"

    LET v_indice2 = 1
    
    PREPARE prp_ctas_procesos FROM  v_QryTxt
    DECLARE cur_ctas_procesos CURSOR FOR prp_ctas_procesos

    FOREACH cur_ctas_procesos INTO g_arr_ctas_procesos[v_indice2].*
       LET v_indice2 = v_indice2 + 1
    END FOREACH 

    CALL g_arr_ctas_procesos.deleteElement(v_indice2)
    LET v_indice2 = v_indice3 - 1
END FUNCTION 


--Función que llena el arreglo de las claves del proceso contable
FUNCTION fn_obtiene_claves_cnt()

   DEFINE 
      v_ind_clave    INTEGER,
      v_consulta_cve STRING


   LET v_ind_clave = 1

   LET v_consulta_cve = "\n SELECT cod_proceso_cnt ||'-'|| referencia_cnt referencia, desc_proc_corta_cnt ",
                        "\n FROM cat_proceso_cnt "

   PREPARE prp_clave FROM v_consulta_cve
   DECLARE cur_clave CURSOR FOR prp_clave

   FOREACH cur_clave INTO  g_arr_claves[v_ind_clave].arr_referencia,
                           g_arr_claves[v_ind_clave].arr_descripcion

      LET v_ind_clave = v_ind_clave + 1
      
   END FOREACH 

   --Borra el registro vacío
   CALL g_arr_claves.deleteElement(v_ind_clave)

END FUNCTION 


--Función que limpia el arreglo agrupando los datos por cuenta
FUNCTION fn_agrupa_cuentas()

   DEFINE 
      v_total_arreglo   INTEGER, 
      v_indice_row      INTEGER,
      v_indice_agrupa   INTEGER,
      v_cuenta_agrupa   CHAR(10)
      


      LET v_total_arreglo = g_arr_ctas_procesos.getLength()
      LET v_indice_row = 1 
      LET v_indice_agrupa = 1
      
      DISPLAY "Rows -- ",v_total_arreglo
 
 
			--Inicializa el arreglo nuevo con el primer registro del arreglo a limpiar
			LET v_cuenta_agrupa = g_arr_ctas_procesos[v_indice_agrupa].cta_contable
			
			
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].cta_contable = g_arr_ctas_procesos[v_indice_agrupa].cta_contable
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc1 	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc1
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc2		= g_arr_ctas_procesos[v_indice_agrupa].imp_proc2
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc3		= g_arr_ctas_procesos[v_indice_agrupa].imp_proc3
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc4		= g_arr_ctas_procesos[v_indice_agrupa].imp_proc4
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc5		= g_arr_ctas_procesos[v_indice_agrupa].imp_proc5
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc6		= g_arr_ctas_procesos[v_indice_agrupa].imp_proc6
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc7		= g_arr_ctas_procesos[v_indice_agrupa].imp_proc7
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc8		= g_arr_ctas_procesos[v_indice_agrupa].imp_proc8
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc10	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc10
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc11	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc11
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc12	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc12
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc14	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc14
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc15	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc15
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc16	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc16
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc17	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc17
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc18	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc18
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc19	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc19
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc20	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc20
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc22	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc22
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc23	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc23
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc24	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc24
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc25	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc25
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc26	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc26
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc27	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc27
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc28	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc28
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc29	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc29
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc30	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc30
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc31	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc31
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc32	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc32
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc33	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc33
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc34	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc34
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc36	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc36
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc37	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc37
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc38	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc38
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc39	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc39
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc42	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc42
			LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc43	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc43
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc46	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc46
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc47	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc47
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc48	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc48
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc49	= g_arr_ctas_procesos[v_indice_agrupa].imp_proc49

         
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc50 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc50
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc51 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc51
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc52 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc52
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc53 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc53
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc54 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc54
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc55 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc55
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc56 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc56
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc57 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc57
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc58 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc58
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc59 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc59
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc60 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc60
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc61 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc61
         LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc62 = g_arr_ctas_procesos[v_indice_agrupa].imp_proc62
         
         
####################################################################################################################################

		--Ciclo para recorrer el arreglo 
		FOR v_indice_row = 2 TO v_total_arreglo

			--Si la cuenta es la misma, suma los importes
			IF v_cuenta_agrupa = g_arr_ctas_procesos[v_indice_row].cta_contable THEN 

			
						--valida que si hay montos diferentes a 0.00 los suma
						##1
						IF g_arr_ctas_procesos[v_indice_row].imp_proc1 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc1 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc1 + g_arr_ctas_procesos[v_indice_row].imp_proc1
						END IF 
						##2
						IF g_arr_ctas_procesos[v_indice_row].imp_proc2 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc2 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc2 + g_arr_ctas_procesos[v_indice_row].imp_proc2
						END IF
						##3
						IF g_arr_ctas_procesos[v_indice_row].imp_proc3 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc3 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc3 + g_arr_ctas_procesos[v_indice_row].imp_proc3
						END IF
						##4
						IF g_arr_ctas_procesos[v_indice_row].imp_proc4 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc4 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc4 + g_arr_ctas_procesos[v_indice_row].imp_proc4
						END IF
						##5
						IF g_arr_ctas_procesos[v_indice_row].imp_proc5 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc5 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc5 + g_arr_ctas_procesos[v_indice_row].imp_proc5
						END IF
						##6
						IF g_arr_ctas_procesos[v_indice_row].imp_proc6 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc6 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc6 + g_arr_ctas_procesos[v_indice_row].imp_proc6
						END IF
						##7
						IF g_arr_ctas_procesos[v_indice_row].imp_proc7 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc7 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc7 + g_arr_ctas_procesos[v_indice_row].imp_proc7
						END IF
						##8
						IF g_arr_ctas_procesos[v_indice_row].imp_proc8 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc8 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc8 + g_arr_ctas_procesos[v_indice_row].imp_proc8
						END IF
						##10
						IF g_arr_ctas_procesos[v_indice_row].imp_proc10 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc10 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc10 + g_arr_ctas_procesos[v_indice_row].imp_proc10
						END IF
						##11
						IF g_arr_ctas_procesos[v_indice_row].imp_proc11 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc11 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc11 + g_arr_ctas_procesos[v_indice_row].imp_proc11
						END IF
						##12
						IF g_arr_ctas_procesos[v_indice_row].imp_proc12 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc12 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc12 + g_arr_ctas_procesos[v_indice_row].imp_proc12
						END IF
						##14
						IF g_arr_ctas_procesos[v_indice_row].imp_proc14 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc14 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc14 + g_arr_ctas_procesos[v_indice_row].imp_proc14
						END IF
						##15
						IF g_arr_ctas_procesos[v_indice_row].imp_proc15 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc15 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc15 + g_arr_ctas_procesos[v_indice_row].imp_proc15
						END IF
						##16
						IF g_arr_ctas_procesos[v_indice_row].imp_proc16 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc16 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc16 + g_arr_ctas_procesos[v_indice_row].imp_proc16
						END IF
						##17
						IF g_arr_ctas_procesos[v_indice_row].imp_proc17 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc17 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc17 + g_arr_ctas_procesos[v_indice_row].imp_proc17
						END IF
						##18
						IF g_arr_ctas_procesos[v_indice_row].imp_proc18 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc18 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc18 + g_arr_ctas_procesos[v_indice_row].imp_proc18
						END IF
						##19
						IF g_arr_ctas_procesos[v_indice_row].imp_proc19 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc19 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc19 + g_arr_ctas_procesos[v_indice_row].imp_proc19
						END IF
						##20
						IF g_arr_ctas_procesos[v_indice_row].imp_proc20 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc20 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc20 + g_arr_ctas_procesos[v_indice_row].imp_proc20
						END IF
						##22
						IF g_arr_ctas_procesos[v_indice_row].imp_proc22 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc22 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc22 + g_arr_ctas_procesos[v_indice_row].imp_proc22
						END IF
						##23
						IF g_arr_ctas_procesos[v_indice_row].imp_proc23 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc23 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc23 + g_arr_ctas_procesos[v_indice_row].imp_proc23
						END IF
						##24
						IF g_arr_ctas_procesos[v_indice_row].imp_proc24 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc24 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc24 + g_arr_ctas_procesos[v_indice_row].imp_proc24
						END IF
						##25
						IF g_arr_ctas_procesos[v_indice_row].imp_proc25 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc25 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc25 + g_arr_ctas_procesos[v_indice_row].imp_proc25
						END IF
						##26
						IF g_arr_ctas_procesos[v_indice_row].imp_proc26 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc26 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc26 + g_arr_ctas_procesos[v_indice_row].imp_proc26
						END IF
						##27
						IF g_arr_ctas_procesos[v_indice_row].imp_proc27 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc27 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc27 + g_arr_ctas_procesos[v_indice_row].imp_proc27
						END IF
						##28
						IF g_arr_ctas_procesos[v_indice_row].imp_proc28 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc28 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc28 + g_arr_ctas_procesos[v_indice_row].imp_proc28
						END IF
						##29
						IF g_arr_ctas_procesos[v_indice_row].imp_proc29 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc29 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc29 + g_arr_ctas_procesos[v_indice_row].imp_proc29
						END IF
						##30
						IF g_arr_ctas_procesos[v_indice_row].imp_proc30 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc30 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc30 + g_arr_ctas_procesos[v_indice_row].imp_proc30
						END IF
						##31
						IF g_arr_ctas_procesos[v_indice_row].imp_proc31 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc31 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc31 + g_arr_ctas_procesos[v_indice_row].imp_proc31
						END IF
						##32
						IF g_arr_ctas_procesos[v_indice_row].imp_proc32 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc32 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc32 + g_arr_ctas_procesos[v_indice_row].imp_proc32
						END IF
						##33
						IF g_arr_ctas_procesos[v_indice_row].imp_proc33 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc33 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc33 + g_arr_ctas_procesos[v_indice_row].imp_proc33
						END IF
						##34
						IF g_arr_ctas_procesos[v_indice_row].imp_proc34 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc34 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc34 + g_arr_ctas_procesos[v_indice_row].imp_proc34
						END IF
						##36
						IF g_arr_ctas_procesos[v_indice_row].imp_proc36 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc36 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc36 + g_arr_ctas_procesos[v_indice_row].imp_proc36
						END IF
						##37
						IF g_arr_ctas_procesos[v_indice_row].imp_proc37 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc37 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc37 + g_arr_ctas_procesos[v_indice_row].imp_proc37
						END IF
						##38
						IF g_arr_ctas_procesos[v_indice_row].imp_proc38 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc38 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc38 + g_arr_ctas_procesos[v_indice_row].imp_proc38
						END IF
						##39
						IF g_arr_ctas_procesos[v_indice_row].imp_proc39 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc39 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc39 + g_arr_ctas_procesos[v_indice_row].imp_proc39
						END IF
						##42
						IF g_arr_ctas_procesos[v_indice_row].imp_proc42 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc42 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc42 + g_arr_ctas_procesos[v_indice_row].imp_proc42
						END IF
						##43
						IF g_arr_ctas_procesos[v_indice_row].imp_proc43 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc43 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc43 + g_arr_ctas_procesos[v_indice_row].imp_proc43
						END IF
                  ##46
						IF g_arr_ctas_procesos[v_indice_row].imp_proc46 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc46 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc46 + g_arr_ctas_procesos[v_indice_row].imp_proc46
						END IF
                  ##47
						IF g_arr_ctas_procesos[v_indice_row].imp_proc47 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc47 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc47 + g_arr_ctas_procesos[v_indice_row].imp_proc47
						END IF
                  ##48
						IF g_arr_ctas_procesos[v_indice_row].imp_proc48 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc48 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc48 + g_arr_ctas_procesos[v_indice_row].imp_proc48
						END IF
                  ##49
						IF g_arr_ctas_procesos[v_indice_row].imp_proc49 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc49 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc49 + g_arr_ctas_procesos[v_indice_row].imp_proc49
						END IF
                  
                  ##50
						IF g_arr_ctas_procesos[v_indice_row].imp_proc50 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc50 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc50 + g_arr_ctas_procesos[v_indice_row].imp_proc50
						END IF
                  ##51
						IF g_arr_ctas_procesos[v_indice_row].imp_proc51 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc51 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc51 + g_arr_ctas_procesos[v_indice_row].imp_proc51
						END IF
                  ##52
						IF g_arr_ctas_procesos[v_indice_row].imp_proc52 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc52 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc52 + g_arr_ctas_procesos[v_indice_row].imp_proc52
						END IF
                  ##53
						IF g_arr_ctas_procesos[v_indice_row].imp_proc53 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc53 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc53 + g_arr_ctas_procesos[v_indice_row].imp_proc53
						END IF
                  ##54
						IF g_arr_ctas_procesos[v_indice_row].imp_proc54 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc54 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc54 + g_arr_ctas_procesos[v_indice_row].imp_proc54
						END IF
                  ##55
						IF g_arr_ctas_procesos[v_indice_row].imp_proc55 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc55 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc55 + g_arr_ctas_procesos[v_indice_row].imp_proc55
						END IF
                  ##56
						IF g_arr_ctas_procesos[v_indice_row].imp_proc56 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc56 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc56 + g_arr_ctas_procesos[v_indice_row].imp_proc56
						END IF
                  ##57
						IF g_arr_ctas_procesos[v_indice_row].imp_proc57 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc57 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc57 + g_arr_ctas_procesos[v_indice_row].imp_proc57
						END IF
                  ##58
						IF g_arr_ctas_procesos[v_indice_row].imp_proc58 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc58 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc58 + g_arr_ctas_procesos[v_indice_row].imp_proc58
						END IF
                  ##59
						IF g_arr_ctas_procesos[v_indice_row].imp_proc59 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc59 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc59 + g_arr_ctas_procesos[v_indice_row].imp_proc59
						END IF
                  ##60
						IF g_arr_ctas_procesos[v_indice_row].imp_proc60 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc60 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc60 + g_arr_ctas_procesos[v_indice_row].imp_proc60
						END IF
                  ##61
						IF g_arr_ctas_procesos[v_indice_row].imp_proc61 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc61 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc61 + g_arr_ctas_procesos[v_indice_row].imp_proc61
						END IF
                  ##62
						IF g_arr_ctas_procesos[v_indice_row].imp_proc62 IS NOT NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc62 = g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc62 + g_arr_ctas_procesos[v_indice_row].imp_proc62
						END IF
                  
                  


						--###################################################################################
						--Valida que no haya datos nulos y les asigna 0.00
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc1 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc1 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc2 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc2 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc3 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc3 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc4 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc4 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc5 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc5 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc6 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc6 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc7 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc7 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc8 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc8 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc10 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc10 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc11 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc11 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc12 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc12 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc14 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc14 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc15 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc15 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc16 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc16 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc17 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc17 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc18 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc18 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc19 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc19 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc20 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc20 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc22 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc22 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc23 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc23 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc24 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc24 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc25 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc25 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc26 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc26 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc27 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc27 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc28 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc28 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc29 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc29 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc30 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc30 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc31 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc31 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc32 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc32 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc33 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc33 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc34 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc34 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc36 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc36 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc37 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc37 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc38 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc38 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc39 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc39 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc42 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc42 = 0.00
						END IF
						IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc43 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc43 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc46 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc46 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc47 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc47 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc48 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc48 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc49 IS NULL THEN 
						 LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc49 = 0.00
						END IF
                  
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc50 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc50 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc51 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc51 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc52 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc52 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc53 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc53 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc54 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc54 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc55 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc55 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc56 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc56 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc57 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc57 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc58 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc58 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc59 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc59 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc60 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc60 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc61 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc61 = 0.00
						END IF
                  IF g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc62 IS NULL THEN 
						  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc62 = 0.00
						END IF
                  
						--###################################################################################


			ELSE --Si la cuenta no es la misma que la siguiente, asigna el nuevo registro al arreglo e incrementa el índice agrupado
					
						LET v_indice_agrupa = v_indice_agrupa + 1
						
						LET v_cuenta_agrupa = g_arr_ctas_procesos[v_indice_row].cta_contable
			
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].cta_contable = g_arr_ctas_procesos[v_indice_row].cta_contable
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc1 	= g_arr_ctas_procesos[v_indice_row].imp_proc1
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc2		= g_arr_ctas_procesos[v_indice_row].imp_proc2
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc3		= g_arr_ctas_procesos[v_indice_row].imp_proc3
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc4		= g_arr_ctas_procesos[v_indice_row].imp_proc4
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc5		= g_arr_ctas_procesos[v_indice_row].imp_proc5
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc6		= g_arr_ctas_procesos[v_indice_row].imp_proc6
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc7		= g_arr_ctas_procesos[v_indice_row].imp_proc7
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc8		= g_arr_ctas_procesos[v_indice_row].imp_proc8
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc10	= g_arr_ctas_procesos[v_indice_row].imp_proc10
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc11	= g_arr_ctas_procesos[v_indice_row].imp_proc11
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc12	= g_arr_ctas_procesos[v_indice_row].imp_proc12
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc14	= g_arr_ctas_procesos[v_indice_row].imp_proc14
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc15	= g_arr_ctas_procesos[v_indice_row].imp_proc15
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc16	= g_arr_ctas_procesos[v_indice_row].imp_proc16
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc17	= g_arr_ctas_procesos[v_indice_row].imp_proc17
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc18	= g_arr_ctas_procesos[v_indice_row].imp_proc18
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc19	= g_arr_ctas_procesos[v_indice_row].imp_proc19
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc20	= g_arr_ctas_procesos[v_indice_row].imp_proc20
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc22	= g_arr_ctas_procesos[v_indice_row].imp_proc22
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc23	= g_arr_ctas_procesos[v_indice_row].imp_proc23
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc24	= g_arr_ctas_procesos[v_indice_row].imp_proc24
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc25	= g_arr_ctas_procesos[v_indice_row].imp_proc25
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc26	= g_arr_ctas_procesos[v_indice_row].imp_proc26
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc27	= g_arr_ctas_procesos[v_indice_row].imp_proc27
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc28	= g_arr_ctas_procesos[v_indice_row].imp_proc28
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc29	= g_arr_ctas_procesos[v_indice_row].imp_proc29
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc30	= g_arr_ctas_procesos[v_indice_row].imp_proc30
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc31	= g_arr_ctas_procesos[v_indice_row].imp_proc31
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc32	= g_arr_ctas_procesos[v_indice_row].imp_proc32
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc33	= g_arr_ctas_procesos[v_indice_row].imp_proc33
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc34	= g_arr_ctas_procesos[v_indice_row].imp_proc34
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc36	= g_arr_ctas_procesos[v_indice_row].imp_proc36
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc37	= g_arr_ctas_procesos[v_indice_row].imp_proc37
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc38	= g_arr_ctas_procesos[v_indice_row].imp_proc38
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc39	= g_arr_ctas_procesos[v_indice_row].imp_proc39
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc42	= g_arr_ctas_procesos[v_indice_row].imp_proc42
						LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc43	= g_arr_ctas_procesos[v_indice_row].imp_proc43
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc46	= g_arr_ctas_procesos[v_indice_row].imp_proc46
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc47	= g_arr_ctas_procesos[v_indice_row].imp_proc47
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc48	= g_arr_ctas_procesos[v_indice_row].imp_proc48
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc49	= g_arr_ctas_procesos[v_indice_row].imp_proc49
                  
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc50 = g_arr_ctas_procesos[v_indice_row].imp_proc50
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc51 = g_arr_ctas_procesos[v_indice_row].imp_proc51
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc52 = g_arr_ctas_procesos[v_indice_row].imp_proc52
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc53 = g_arr_ctas_procesos[v_indice_row].imp_proc53
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc54 = g_arr_ctas_procesos[v_indice_row].imp_proc54
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc55 = g_arr_ctas_procesos[v_indice_row].imp_proc55
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc56 = g_arr_ctas_procesos[v_indice_row].imp_proc56
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc57 = g_arr_ctas_procesos[v_indice_row].imp_proc57
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc58 = g_arr_ctas_procesos[v_indice_row].imp_proc58
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc59 = g_arr_ctas_procesos[v_indice_row].imp_proc59
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc60 = g_arr_ctas_procesos[v_indice_row].imp_proc60
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc61 = g_arr_ctas_procesos[v_indice_row].imp_proc61
                  LET g_arr_ctas_procesos_agrupado[v_indice_agrupa].imp_proc62 = g_arr_ctas_procesos[v_indice_row].imp_proc62
                  

			END IF


		END FOR 


      DISPLAY "Longitud  del arreglo nuevo: ", g_arr_ctas_procesos_agrupado.getLength()

END FUNCTION 


--g_arr_ctas_procesos_agrupado
--Genera un archivo de salida en texto plano con la información de validación de procesos
FUNCTION fn_genera_archivo_validacion()

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
      v_desc_proceso_cnt   VARCHAR (65),
      v_sumario_cuentas    STRING 

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

   LET v_nom_archivo = "/validacion_de_procesos_",v_hora

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
   --v_cmb_proceso, g_f_inicial, g_f_final
   LET v_encabezado = ""
   
   IF v_cmb_proceso IS NOT NULL THEN

      SELECT desc_proceso_cnt
      INTO v_desc_proceso_cnt
      FROM cat_proceso_cnt
      WHERE cod_proceso_cnt = v_cmb_proceso
   
      LET v_encabezado = v_encabezado,"|Proceso: ",v_cmb_proceso,"-",v_desc_proceso_cnt
   END IF 
   IF g_f_inicial IS NOT NULL THEN
      LET v_encabezado = v_encabezado,"|Fecha Inicial:",g_f_inicial USING "dd-mm-yyyy"
   END IF 
   IF g_f_final IS NOT NULL THEN
      LET v_encabezado = v_encabezado,"|Fecha Final:",g_f_final USING "dd-mm-yyyy"
   END IF 
   CALL v_ch_arch_salida.write([v_encabezado])

   --Arma el encabezado con el nombre de las cuentas activas
   LET v_encabezado = "Cuenta Contable",v_encabezado_cuentas, "|"

   CALL v_ch_arch_salida.write([v_encabezado])
   
   FOR v_recorre_arreglo = 1 TO g_arr_ctas_procesos_agrupado.getLength()
      LET v_detalle = g_arr_ctas_procesos_agrupado[v_recorre_arreglo].cta_contable, "|"

      IF bnd_proc1 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc1, "|" END IF
      IF bnd_proc2 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc2, "|" END IF
      IF bnd_proc3 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc3, "|" END IF
      IF bnd_proc4 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc4, "|" END IF
      IF bnd_proc5 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc5, "|" END IF      
      IF bnd_proc6 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc6, "|" END IF      
      IF bnd_proc7 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc7, "|" END IF      
      IF bnd_proc8 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc8, "|" END IF
      IF bnd_proc10 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc10, "|" END IF
      
      IF bnd_proc11 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc11, "|" END IF      
      IF bnd_proc12 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc12, "|" END IF      
      IF bnd_proc14 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc14, "|" END IF      
      IF bnd_proc15 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc15, "|" END IF      
      IF bnd_proc16 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc16, "|" END IF      
      IF bnd_proc17 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc17, "|" END IF      
      IF bnd_proc18 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc18, "|" END IF      
      IF bnd_proc19 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc19, "|" END IF      
      IF bnd_proc20 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc20, "|" END IF
      
      IF bnd_proc22 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc22, "|" END IF      
      IF bnd_proc23 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc23, "|" END IF      
      IF bnd_proc24 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc24, "|" END IF      
      IF bnd_proc25 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc25, "|" END IF      
      IF bnd_proc26 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc26, "|" END IF      
      IF bnd_proc27 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc27, "|" END IF      
      IF bnd_proc28 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc28, "|" END IF      
      IF bnd_proc29 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc29, "|" END IF
      IF bnd_proc30 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc30, "|" END IF                  
                      
      IF bnd_proc31 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc31, "|" END IF
      IF bnd_proc32 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc32, "|" END IF            
      IF bnd_proc33 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc33, "|" END IF      
      IF bnd_proc34 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc34, "|" END IF            
      IF bnd_proc36 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc36, "|" END IF      
      IF bnd_proc37 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc37, "|" END IF      
      IF bnd_proc38 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc38, "|" END IF      
      IF bnd_proc39 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc39, "|" END IF
      
      IF bnd_proc42 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc42, "|" END IF            
      IF bnd_proc43 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc43, "|" END IF      
      IF bnd_proc46 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc46, "|" END IF      
      IF bnd_proc47 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc47, "|" END IF      
      IF bnd_proc48 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc48, "|" END IF      
      IF bnd_proc49 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc49, "|" END IF

      
      IF bnd_proc50 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc50, "|" END IF            
      IF bnd_proc51 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc51, "|" END IF      
      IF bnd_proc52 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc52, "|" END IF      
      IF bnd_proc53 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc53, "|" END IF      
      IF bnd_proc54 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc54, "|" END IF      
      IF bnd_proc55 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc55, "|" END IF
      IF bnd_proc56 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc56, "|" END IF
      IF bnd_proc57 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc57, "|" END IF
      IF bnd_proc58 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc58, "|" END IF
      IF bnd_proc59 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc59, "|" END IF

      IF bnd_proc60 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc60, "|" END IF            
      IF bnd_proc61 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc61, "|" END IF      
      IF bnd_proc62 = 1 THEN LET v_detalle = v_detalle, g_arr_ctas_procesos_agrupado[v_recorre_arreglo].imp_proc62, "|" END IF      
      
      
    
      CALL v_ch_arch_salida.write([v_detalle])

   END FOR

   
   --Escribe el sumario
    LET v_sumario_cuentas = "" 
      IF bnd_proc1 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc1,"|" END IF
      IF bnd_proc2 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc2,"|" END IF
      IF bnd_proc3 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc3,"|" END IF
      IF bnd_proc4 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc4,"|" END IF
      IF bnd_proc5 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc5,"|" END IF
      IF bnd_proc6 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc6,"|" END IF
      IF bnd_proc7 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc7,"|" END IF
      IF bnd_proc8 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc8,"|" END IF
      IF bnd_proc10 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc10,"|" END IF

      IF bnd_proc11 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc11,"|" END IF
      IF bnd_proc12 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc12,"|" END IF
      IF bnd_proc14 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc14,"|" END IF
      IF bnd_proc15 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc15,"|" END IF
      IF bnd_proc16 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc16,"|" END IF
      IF bnd_proc17 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc17,"|" END IF
      IF bnd_proc18 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc18,"|" END IF
      IF bnd_proc19 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc19,"|" END IF
      IF bnd_proc20 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc20,"|" END IF

      IF bnd_proc22 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc22,"|" END IF
      IF bnd_proc23 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc23,"|" END IF
      IF bnd_proc24 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc24,"|" END IF
      IF bnd_proc25 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc25,"|" END IF
      IF bnd_proc26 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc26,"|" END IF
      IF bnd_proc27 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc27,"|" END IF
      IF bnd_proc28 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc28,"|" END IF
      IF bnd_proc29 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc29,"|" END IF
      IF bnd_proc30 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc30,"|" END IF
                      
      IF bnd_proc31 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc31,"|" END IF
      IF bnd_proc32 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc32,"|" END IF
      IF bnd_proc33 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc33,"|" END IF
      IF bnd_proc34 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc34,"|" END IF
      IF bnd_proc36 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc36,"|" END IF
      IF bnd_proc37 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc37,"|" END IF
      IF bnd_proc38 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc38,"|" END IF
      IF bnd_proc39 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc39,"|" END IF

      IF bnd_proc42 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc42,"|" END IF
      IF bnd_proc43 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc43,"|" END IF
      IF bnd_proc46 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc46,"|" END IF
      IF bnd_proc47 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc47,"|" END IF
      IF bnd_proc48 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc48,"|" END IF
      IF bnd_proc49 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc49,"|" END IF

      
      IF bnd_proc50 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc50,"|" END IF
      IF bnd_proc51 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc51,"|" END IF
      IF bnd_proc52 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc52,"|" END IF
      IF bnd_proc53 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc53,"|" END IF
      IF bnd_proc54 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc54,"|" END IF
      IF bnd_proc55 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc55,"|" END IF
      IF bnd_proc56 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc56,"|" END IF
      IF bnd_proc57 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc57,"|" END IF
      IF bnd_proc58 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc58,"|" END IF
      IF bnd_proc59 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc59,"|" END IF

      IF bnd_proc60 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc60,"|" END IF
      IF bnd_proc61 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc61,"|" END IF
      IF bnd_proc62 = 1 THEN LET v_sumario_cuentas = v_sumario_cuentas,    g_arr_totales_proc[1].tot_proc62,"|" END IF
      
      
   LET v_sumario = "Saldos|",v_sumario_cuentas

   CALL v_ch_arch_salida.write([v_sumario])
   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   
   --Cambia el formato del archivo a DOS
   LET v_comando_dos = "unix2dos ",v_ruta_envio_cnt CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   CALL fn_mensaje("Información","Se ha generado el archivo de Validación de Procesos\n en la ruta"||v_ruta_nomarch,"information")
   

END FUNCTION 




FUNCTION fn_consulta_reporte()

   DEFINE 
      manejador_rpt  om.SaxDocumentHandler  -- Contenedor documentos reporte

   DEFINE 
      g_arr_rpt DYNAMIC ARRAY OF RECORD
          cta_contable  LIKE cnt_transaccion.cta_contable,
          importe       LIKE cnt_transaccion.importe
       END RECORD

   DEFINE 
      v_total_cargo  DECIMAL (22,2),
      v_total_abono  DECIMAL (22,2),
      v_cod_cta      SMALLINT -- 1 es abono y 2 es cargo   
       

   LET v_QryTxt = "\n SELECT cta_contable, SUM(importe), cod_naturaleza_cta",
                  "\n FROM cnt_transaccion", 
                  "\n WHERE"

   --Consulta por procesos
   IF v_cmb_proceso IS NOT NULL AND g_f_inicial IS NULL AND g_f_final IS NULL THEN
      LET v_QryTxt = v_QryTxt || "\n cod_proceso_cnt =", v_cmb_proceso 
   END IF
   LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,3 \n ORDER BY 1" 
   --DISPLAY v_QryTxt

   SELECT cod_proceso_cnt ||'-'|| referencia_cnt
     INTO v_tit_proc
     FROM cat_proceso_cnt
    WHERE cod_proceso_cnt = v_cmb_proceso

   
    
   SELECT SUM(importe)
   INTO v_total_cargo
   FROM cnt_transaccion
   WHERE cod_proceso_cnt = v_cmb_proceso
   AND cod_naturaleza_cta = 2 --Cargo

   SELECT SUM(importe)
   INTO v_total_abono
   FROM cnt_transaccion
   WHERE cod_proceso_cnt = v_cmb_proceso
   AND cod_naturaleza_cta = 1 --Cargo

   LET v_tot_proc = v_total_cargo - v_total_abono

   DISPLAY "El total -- ", v_tot_proc

   -- Botón para generar el reporte en PDF de la consulta
   IF fgl_report_loadCurrentSettings("CNTC043.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   END IF
   
   PREPARE prp_consulta_rpt FROM v_QryTxt
   DECLARE cur_cons_rpt CURSOR FOR prp_consulta_rpt

   LET v_indice4 = 1
   START REPORT rpt_ctas_procesos TO XML HANDLER manejador_rpt
   
      FOREACH cur_cons_rpt INTO g_arr_rpt[v_indice4].cta_contable,
                                g_arr_rpt[v_indice4].importe,
                                v_cod_cta

         OUTPUT TO REPORT rpt_ctas_procesos(g_usuario,
                                            v_tit_proc,
                                            v_tot_proc,
                                            g_arr_rpt[v_indice4].*,v_cod_cta)
         LET v_indice4 = v_indice4 + 1
 
      END FOREACH
   FINISH REPORT rpt_ctas_procesos
END FUNCTION

REPORT rpt_ctas_procesos(v_usuario, p_tit_proc,p_tot_proc,arr_rpt,v_cod_cta)

DEFINE 
   v_usuario LIKE seg_usuario.usuario_cod,
   v_fecha_reporte  DATE,
   p_tit_proc       CHAR(08),
   p_tot_proc       LIKE cnt_transaccion.importe
       
DEFINE 
   arr_rpt RECORD
          cta_contable  LIKE cnt_transaccion.cta_contable,
          importe       LIKE cnt_transaccion.importe
   END RECORD
       
DEFINE v_cod_cta      SMALLINT -- 1 es abono y 2 es cargo   
      
FORMAT
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_usuario
      PRINTX v_tit_proc

   ON EVERY ROW
      PRINTX arr_rpt.cta_contable
      PRINTX arr_rpt.importe
      PRINTX v_cod_cta

   ON LAST ROW
      PRINTX p_tot_proc

END REPORT
