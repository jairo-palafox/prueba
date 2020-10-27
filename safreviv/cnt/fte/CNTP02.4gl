################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificación  => 12/11/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Módulo            => CNT                                                      #
#Programa          => CNTP02                                                   #
#Objetivo          => Programa para realizar registro contable manualmente     #
#Fecha inicio      => 12/11/2012                                               #
################################################################################

DATABASE safre_viv

GLOBALS "CNTG01.4gl"


GLOBALS 

   --Sección de variables de entrada
   DEFINE 
      --cmb_proceso viene definido en las variables globales
      f_folio_liquida         DECIMAL (9,0),
      f_importe               DECIMAL (22,2),
      f_fecha_liquidacion     DATE,
      v_cmb_sub_cta           SMALLINT,
      f_cta_contable          CHAR (10),
      f_naturaleza_cta        SMALLINT

   --Otras variables de entrada por default
   DEFINE 
      v_f_emision             DATE,
      --v_cod_proceso_op       SMALLINT,
      v_cmb_proceso_op        SMALLINT,
      v_tpo_transaccion       SMALLINT,
      v_estado                SMALLINT,
      f_folio_cnt             DECIMAL (9,0),
      v_cod_transaccion_cnt   SMALLINT,
      v_id_cuenta_contable    SMALLINT 

END GLOBALS 


MAIN

   --Sección de variables UI
  DEFINE 
    f_ventana                ui.Window, --provee la interfaz para la ventana
    f_forma                  ui.Form --provee la interfaz para la forma

   DEFINE 
      v_existe_cta      SMALLINT 

   LET g_usuario      = ARG_VAL(1) -- Recibe la variable de usuario
   LET g_tipo_proceso = ARG_VAL(2) -- Recibe el tipo de proceso
   LET g_nom_prog     = ARG_VAL(3) -- Recibe el nombre del programa

   LET f_folio_cnt = 0 
   LET v_f_emision = TODAY
   LET v_tpo_transaccion = 0
   LET v_estado = 10
   LET v_cod_transaccion_cnt = 0 --verificar
   LET v_id_cuenta_contable = 0

   CLOSE WINDOW SCREEN

  OPEN WINDOW v_cntp02 WITH FORM "CNTP021" CLEAR FORM
    DIALOG   ATTRIBUTES(UNBUFFERED) 
      
      INPUT BY NAME v_cmb_proceso, v_cmb_proceso_op, f_folio_liquida, f_importe, f_fecha_liquidacion, v_cmb_sub_cta, f_cta_contable, f_naturaleza_cta 
         
         
         BEFORE INPUT 

            CLEAR FORM 

            LET f_ventana = ui.Window.getCurrent()
            LET f_forma   = f_ventana.getForm()

            --Invocamos la función para asignar el título a la ventana
            CALL ui.Interface.setText(g_nom_prog)

            --Llena el combo de proceso 
            CALL fn_llena_combo_proceso()
            --Llena combo subcuenta
            CALL fn_cmb_subcta_contable()
            --LLena combo naturaleza
            CALL fn_llena_combo_naturaleza()

         

         ON CHANGE v_cmb_proceso
            CALL fn_llena_combo_proceso_operativo() 

         ON ACTION cancelar 
            EXIT PROGRAM 
            

         ON ACTION insertar
            --verifica que se ingresen todos los parámetros del registro contable
            IF v_cmb_proceso IS NULL 
               AND f_folio_liquida IS NULL
               AND v_cmb_proceso_op IS NULL  
               AND f_importe IS NULL 
               AND f_fecha_liquidacion IS NULL 
               AND v_cmb_sub_cta IS NULL 
               AND f_cta_contable IS NULL 
               AND f_naturaleza_cta IS NULL THEN 

               CALL fn_mensaje ("Error", "Debe ingresar todos los criterios para el registro contable", "information")
               NEXT FIELD v_cmb_proceso
            END IF  

            --Verifica el proceso contable seleccionado
            IF v_cmb_proceso IS NULL THEN 
               CALL fn_mensaje ("Error", "Debe seleccionar un proceso contable", "information")
               NEXT FIELD v_cmb_proceso
            END IF 

            --Verifica el proceso operativo seleccionado
            IF v_cmb_proceso_op IS NULL THEN 
               CALL fn_mensaje ("Error", "Debe seleccionar un proceso operativo", "information")
               NEXT FIELD v_cmb_proceso_op
            END IF 

            --Verifica el folio de liquidación
            IF f_folio_liquida IS NULL THEN 
               CALL fn_mensaje ("Error", "Debe insertar un folio de liquidación", "information")
               NEXT FIELD f_folio_liquida
            END IF 

            --Verifica el importe
            IF f_importe IS NULL THEN 
               CALL fn_mensaje ("Error", "Debe ingresar un monto para el importe", "information")
               NEXT FIELD f_importe
            END IF 

            --Verifica la fecha de liquidación
            IF f_fecha_liquidacion IS NULL THEN 
               CALL fn_mensaje ("Error", "Debe ingresar la fecha de liquidación", "information")
               NEXT FIELD f_fecha_liquidacion
            END IF 

            --Verifica la subcuenta
            IF v_cmb_sub_cta IS NULL THEN 
               CALL fn_mensaje ("Error", "Debe seleccionar una subcuenta", "information")
               NEXT FIELD v_cmb_sub_cta
               --LET v_cmb_sub_cta = 0
               --DISPLAY BY NAME v_cmb_sub_cta
            END IF

            --Verifica la cuenta contable
            IF f_cta_contable IS NULL THEN 
               CALL fn_mensaje ("Error", "Debe ingresar una cuenta contable", "information")
               NEXT FIELD f_cta_contable
            END IF

            --Verifica la naturaleza de la cuenta
            IF f_naturaleza_cta IS NULL THEN 
               CALL fn_mensaje ("Error", "Debe seleccionar la naturaleza de la cuenta", "information")
               NEXT FIELD f_naturaleza_cta
            END IF
            

            IF f_fecha_liquidacion >  TODAY THEN 
               CALL fn_mensaje ("Error", "La fecha de liquidación no puede ser mayor al día de hoy.", "information")
               NEXT FIELD v_cmb_proceso
            END IF 

            IF f_cta_contable IS NOT NULL THEN 
               CALL fn_verifica_cta_contable() RETURNING v_existe_cta
            END IF 
            --Si la cuenta contable no existe manda mensaje en pantalla
            IF v_existe_cta = 0 THEN 
               CALL fn_mensaje ("Error", "La cuenta contable no existe, verifique.", "information")
               NEXT FIELD f_cta_contable
            END IF 

            --Llama a la función para identificar el proceso para el registro contable
            --CALL fn_identifica_proceso_contable()
            --DISPLAY "Proceso : ",v_cmb_proceso
            --DISPLAY "Proceso contable: ",v_cod_proceso_op
            --DISPLAY "Folio liquida: ",f_folio_liquida
            --DISPLAY "Importe: ",f_importe
            --DISPLAY "Subcuenta: ",v_cmb_sub_cta
            --DISPLAY "Naturaleza: ",f_naturaleza_cta

            --Da de alta el registro contable
            CALL fn_inserta_registro_contable()
            CALL fn_mensaje ("Information", "Se ha realizado el registro contable exitosamente.", "information")

            CLEAR FORM 
            LET v_cmb_proceso = NULL --, , , , , , ,  
            LET v_cmb_proceso_op = NULL
            LET f_folio_liquida = NULL
            LET f_importe = NULL
            LET f_fecha_liquidacion = NULL
            LET v_cmb_sub_cta = NULL
            LET f_cta_contable = NULL
            LET f_naturaleza_cta = NULL

            DISPLAY BY NAME v_cmb_proceso, v_cmb_proceso_op, f_folio_liquida, f_importe, f_fecha_liquidacion, v_cmb_sub_cta, f_cta_contable, f_naturaleza_cta
            CALL ui.Interface.refresh()   

         END INPUT 
         
      END DIALOG
  CLOSE WINDOW v_cntp02
END MAIN


--Verifica que exista la cuenta contable en el catálogo
FUNCTION fn_verifica_cta_contable()

   DEFINE 
      v_max_cta   SMALLINT 

   --Cuenta registros de la cuenta contable, en teoría hay sólo 1
   SELECT COUNT (cta_contable)
   INTO v_max_cta
   FROM cat_cuenta_contable
   WHERE cta_contable = f_cta_contable 

   --Si el valor de regreso es nulo, deja la variable en 0
   IF v_max_cta IS NULL THEN 
      LET v_max_cta = 0 
   END IF 

   RETURN v_max_cta
      
END FUNCTION 

#OBJETIVO: Mostrar la información de la naturaleza de las cuentas
FUNCTION fn_llena_combo_naturaleza()

   DEFINE arr_naturaleza_cta DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
       v_cve_cod_naturaleza_cta  LIKE cnt_regla_contable.cod_naturaleza_cta,
       v_naturaleza              CHAR(45)
    END RECORD

   DEFINE f_naturaleza_cta CHAR(45),
       v_indice     SMALLINT,                   -- Variable del indice
       v_QryTxt     STRING,                     -- Cadena para almacenar Query
       cb           ui.ComboBox                 -- Variable de Combobox

   LET cb = ui.ComboBox.forName("f_naturaleza_cta") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1

   LET v_QryTxt = "\n SELECT rc.cod_naturaleza_cta,", 
                  "\n        rc.cod_naturaleza_cta || '-' ||",
                  "\n        nm.desc_naturaleza_mov AS NATURALEZA_CTA",
                  "\n   FROM cnt_regla_contable rc",
                  "\n   JOIN cat_naturaleza_mov nm",
                  "\n     ON rc.cod_naturaleza_cta = nm.cod_naturaleza_mov ",
                  "\n  GROUP BY 1,2",
                  "\n  ORDER BY 1,2"

   -- Prepara la consulta para obtener los codigos de proceso
   PREPARE prp_naturaleza_cta FROM v_QryTxt

   -- Limpia el combo
   CALL cb.clear()

      -- Declara el cursor para la consulta 
      DECLARE cur_naturaleza_cta CURSOR FOR prp_naturaleza_cta
      FOREACH cur_naturaleza_cta INTO arr_naturaleza_cta[v_indice].v_cve_cod_naturaleza_cta,
                                      arr_naturaleza_cta[v_indice].v_naturaleza
         -- Agrega elementos al combobox
         CALL cb.addItem(arr_naturaleza_cta[v_indice].v_cve_cod_naturaleza_cta,
                         arr_naturaleza_cta[v_indice].v_naturaleza)
         LET v_indice = v_indice + 1
      END FOREACH
      
   CALL arr_naturaleza_cta.deleteElement(v_indice)

   
   --RETURN f_naturaleza_cta, v_indice, arr_naturaleza_cta[1].v_cve_cod_naturaleza_cta
   
END FUNCTION

--Da de alta el registro contable de forma manual
FUNCTION fn_inserta_registro_contable()

   DISPLAY "f_folio_cnt: ",f_folio_cnt
   DISPLAY "v_cmb_proceso_op: ",v_cmb_proceso_op
   DISPLAY "v_cmb_proceso contable: ",v_cmb_proceso
   DISPLAY "v_cod_transaccion_cnt: ",v_cod_transaccion_cnt
   DISPLAY "v_cmb_sub_cta: ",v_cmb_sub_cta
   DISPLAY "f_cta_contable: ",f_cta_contable
   DISPLAY "f_naturaleza_cta: ",f_naturaleza_cta
   DISPLAY "f_folio_liquida: ",f_folio_liquida
   DISPLAY "f_importe: ",f_importe
   DISPLAY "f_fecha_liquidacion: ",f_fecha_liquidacion
   DISPLAY "v_f_emision: ",v_f_emision
   DISPLAY "v_tpo_transaccion: ",v_tpo_transaccion
   DISPLAY "v_estado: ",v_estado
      


   INSERT INTO cnt_transaccion
   (id_cuenta_contable, folio_cnt, cod_proceso_cnt, cod_proceso, cod_transaccion_cnt, cod_subcta_cnt, 
   cta_contable, cod_naturaleza_cta, folio_liquida, importe, f_liquida, 
   f_emision, tpo_transaccion, estado)
   VALUES 
   (v_id_cuenta_contable, f_folio_cnt,v_cmb_proceso, v_cmb_proceso_op, v_cod_transaccion_cnt, v_cmb_sub_cta,
   f_cta_contable, f_naturaleza_cta, f_folio_liquida, f_importe, f_fecha_liquidacion,
   v_f_emision, v_tpo_transaccion, v_estado)

   
   
END FUNCTION 


#OBJETIVO: Llenar el ComboBox para seleccionar el proceso operativo
FUNCTION fn_llena_combo_proceso_operativo()

   DEFINE 
      arr_cat_proceso_cnt DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cve_cod_proceso_cnt  LIKE cat_proceso_cnt.cod_proceso_cnt,
          v_desc_proceso_cnt     CHAR(45)--LIKE cat_proceso_cnt.desc_proceso_cnt
      END RECORD
      
   DEFINE 
      v_indice     SMALLINT,                   -- Variable del indice
      v_QryTxt     STRING                      -- Cadena para almacenar Query
       
   LET cb = ui.ComboBox.forName("v_cmb_proceso_op") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_QryTxt = "\n SELECT P.cod_proceso,P.cod_proceso || '-' || TRIM(CP.proceso_desc)",
                  "\n FROM cnt_proceso P, cat_proceso CP",
                  "\n WHERE P.cod_proceso = CP.proceso_cod",
                  "\n AND P.cod_proceso_cnt = ",v_cmb_proceso  
                  

   -- Prepara la consulta para obtener los codigos de proceso
   PREPARE prp_cat_proceso_cnt FROM v_QryTxt

   -- Limpia el combo
   CALL cb.clear()
   LET v_indice = 1

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_cb_proc_cnt CURSOR FOR prp_cat_proceso_cnt
      FOREACH cur_llena_cb_proc_cnt INTO arr_cat_proceso_cnt[v_indice].v_cve_cod_proceso_cnt, 
                                         arr_cat_proceso_cnt[v_indice].v_desc_proceso_cnt
         -- Agrega elementos al combobox
         CALL cb.addItem(arr_cat_proceso_cnt[v_indice].v_cve_cod_proceso_cnt,
                         arr_cat_proceso_cnt[v_indice].v_desc_proceso_cnt)
         LET v_indice = v_indice + 1
      END FOREACH

   CALL arr_cat_proceso_cnt.deleteElement(v_indice)

   LET v_cmb_proceso_op = arr_cat_proceso_cnt[1].v_cve_cod_proceso_cnt
   DISPLAY "Tamaño: ", arr_cat_proceso_cnt.getLength()

END FUNCTION

#OBJETIVO: Mostrar la información de las subcuentas
FUNCTION fn_cmb_subcta_contable()
   DEFINE 
      arr_cat_sctas DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cve_cod_subcta_cnt   LIKE cnt_regla_contable.cod_subcta_cnt,
          v_desc_sctas           VARCHAR(60)--LIKE cat_proceso_cnt.desc_proceso_cnt
      END RECORD
      
   DEFINE 
         v_indice      SMALLINT,                   -- Variable del indice
         v_QryTxt      STRING                     -- Cadena para almacenar Query

   LET cb = ui.ComboBox.forName("v_cmb_sub_cta") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   -- Limpia el combo
   CALL cb.clear()

   --Agrega un registro con cero al combo para permitir insertar una subcuenta no asociada
   --CALL cb.addItem(0,"SIN SUBCUENTA ASOCIADA")

   LET arr_cat_sctas[1].v_cve_cod_subcta_cnt = 0
   LET arr_cat_sctas[1].v_desc_sctas = "SIN SUBCUENTA ASOCIADA"
   CALL cb.addItem(arr_cat_sctas[1].v_cve_cod_subcta_cnt,
                  arr_cat_sctas[1].v_desc_sctas)
   
   LET v_indice = 1

   LET v_indice = v_indice + 1
 
   LET v_QryTxt = "\n SELECT subcuenta,subcuenta||'-'||subcuenta_desc",
                  "\n FROM cat_subcuenta",
                  "\n ORDER BY 1 ASC"
   PREPARE prp_cmb_subctas FROM v_QryTxt

   -- Declara el cursor para la consulta 
   DECLARE cur_cmb_sctas CURSOR FOR prp_cmb_subctas
      FOREACH cur_cmb_sctas INTO arr_cat_sctas[v_indice].v_cve_cod_subcta_cnt,
                                 arr_cat_sctas[v_indice].v_desc_sctas
         -- Agrega elementos al combobox
         CALL cb.addItem(arr_cat_sctas[v_indice].v_cve_cod_subcta_cnt,
                         arr_cat_sctas[v_indice].v_desc_sctas)
         LET v_indice = v_indice + 1
      END FOREACH
   
   LET v_cmb_sub_cta = arr_cat_sctas[1].v_cve_cod_subcta_cnt
   
END FUNCTION
