################################################################################
#Version                    => 1.0.1                                           #
#Fecha ultima modificacion  => 14/05/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTG02                                                   #
#Objetivo          => Libreria de funciones para el modulo de contabilidad     #
#Fecha inicio      => 04/05/2012                                               #
################################################################################
DATABASE
     safre_viv
GLOBALS "CNTG01.4gl"
#OBJETIVO: Llenar el ComboBox para seleccionar el proceso contable
FUNCTION fn_llena_combo_proceso()
DEFINE arr_cat_proceso_cnt DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cve_cod_proceso_cnt LIKE cat_proceso_cnt.cod_proceso_cnt,
          v_desc_proceso_cnt CHAR(45)--LIKE cat_proceso_cnt.desc_proceso_cnt
       END RECORD
DEFINE v_indice     SMALLINT,                   -- Variable del indice
       v_QryTxt     STRING                      -- Cadena para almacenar Query
       
   LET cb = ui.ComboBox.forName("v_cmb_proceso") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_QryTxt = "\n SELECT cod_proceso_cnt,",
                  "\n        cod_proceso_cnt||'-'||desc_proceso_cnt",
                  "\n   FROM cat_proceso_cnt" 

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

END FUNCTION

#Objetivo: Obtener los proceso_cod de acuerdo al proceso_cod_cnt seleccionado
FUNCTION fn_llena_combo_proceso_cod(v_cmb_proceso)
   DEFINE      
      v_cmb_proceso  LIKE cnt_proceso.cod_proceso_cnt
      

   --Define el record para llenar el combo
   DEFINE v_arr_cod_proceso DYNAMIC ARRAY OF RECORD 
      v_cod_proceso        LIKE cat_proceso.proceso_cod,
      v_desc_cod_proceso   CHAR (45)
   END RECORD 

   DEFINE 
               v_indice     SMALLINT,      
               v_QryTxt     STRING         

   LET cb = ui.ComboBox.forName("v_cmb_proceso_cod") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_QryTxt = "\n   SELECT p.proceso_cod, p.proceso_cod || '-' || p.proceso_desc ",
                  "\n   FROM cat_proceso p,cnt_proceso c ",
                  "\n   WHERE p.proceso_cod = c.cod_proceso ", 
                  "\n   AND c.cod_proceso_cnt = ",v_cmb_proceso,
                  "\n   ORDER BY 1 "
   
   PREPARE prp_cod_proceso FROM v_QryTxt
   -- Limpia el combo
   CALL cb.clear()
   LET v_indice = 1

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_cod_proceso CURSOR FOR prp_cod_proceso

   FOREACH  cur_llena_cod_proceso INTO v_arr_cod_proceso[v_indice].v_cod_proceso,
                                       v_arr_cod_proceso[v_indice].v_desc_cod_proceso  

       -- Agrega elementos al combobox
         CALL cb.addItem(v_arr_cod_proceso[v_indice].v_cod_proceso,
                                       v_arr_cod_proceso[v_indice].v_desc_cod_proceso)
                                       
      LET v_indice = v_indice + 1
   END FOREACH 

   CALL v_arr_cod_proceso.deleteElement(v_indice)

   RETURN v_arr_cod_proceso[1].v_cod_proceso
   
END FUNCTION 

#OBJETIVO: Mostrar la información de las subcuentas
FUNCTION fn_combo_subcuenta(p_transaccion,p_cod_subcta_cnt)
DEFINE arr_cat_sctas DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cve_cod_subcta_cnt LIKE cnt_regla_contable.cod_subcta_cnt,
          v_desc_sctas VARCHAR(60)--LIKE cat_proceso_cnt.desc_proceso_cnt
                       END RECORD
DEFINE p_transaccion LIKE cnt_regla_contable.cod_transaccion_cnt,
       v_indice      SMALLINT,                   -- Variable del indice
       v_QryTxt      STRING,                     -- Cadena para almacenar Query
       p_cod_subcta_cnt LIKE cnt_regla_contable.cod_subcta_cnt

   LET cb = ui.ComboBox.forName("v_cmb_sub_cta") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   -- Limpia el combo
   CALL cb.clear()
   LET v_indice = 1

   IF length(p_cod_subcta_cnt) > 0 OR p_cod_subcta_cnt IS NOT NULL THEN
      SELECT subcuenta,subcuenta||'-'||subcuenta_desc
        INTO arr_cat_sctas[v_indice].v_cve_cod_subcta_cnt,
             arr_cat_sctas[v_indice].v_desc_sctas
      FROM cat_subcuenta
      WHERE subcuenta = p_cod_subcta_cnt

      CALL cb.addItem(arr_cat_sctas[v_indice].v_cve_cod_subcta_cnt,
                      arr_cat_sctas[v_indice].v_desc_sctas)
   END IF

   LET v_indice = v_indice + 1
 
   LET v_QryTxt = "\n SELECT subcuenta,subcuenta||'-'||subcuenta_desc",
                  "\n FROM cat_subcuenta",
                  "\n WHERE subcuenta NOT IN (SELECT cod_subcta_cnt",
                  "\n                         FROM cnt_regla_contable",
                  "\n               WHERE cod_transaccion_cnt = ",p_transaccion,
                  "\n                 AND cod_proceso_cnt = ",v_cmb_proceso,")",
                  "\n ORDER BY 1"
   PREPARE prp_cons_subctas FROM v_QryTxt

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_cb_sctas CURSOR FOR prp_cons_subctas
      FOREACH cur_llena_cb_sctas INTO arr_cat_sctas[v_indice].v_cve_cod_subcta_cnt,
                                      arr_cat_sctas[v_indice].v_desc_sctas
         -- Agrega elementos al combobox
         CALL cb.addItem(arr_cat_sctas[v_indice].v_cve_cod_subcta_cnt,
                         arr_cat_sctas[v_indice].v_desc_sctas)
         LET v_indice = v_indice + 1
      END FOREACH

      RETURN arr_cat_sctas[1].v_cve_cod_subcta_cnt
      
   --LET v_indice = v_indice - 1
--
   --CALL arr_cat_sctas.deleteElement(v_indice)

END FUNCTION
#OBJETIVO: Llenar el ComboBox para seleccionar el perido contable
FUNCTION fn_llena_combo_periodo()
DEFINE arr_cat_periodo_cnt DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cod_periodo_cnt LIKE cat_proceso_cnt.cod_periodo,
          v_desc_periiodo_cnt VARCHAR(60)
       END RECORD
DEFINE v_indice     SMALLINT,                   -- Variable del indice
       v_QryTxt     STRING                      -- Cadena para almacenar Query
       
   LET cb = ui.ComboBox.forName("f_cmb_periodo") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_QryTxt = "\n SELECT cp.cod_periodo,",
                  "\n        cp.cod_periodo||'-'||cs.subcuenta_desc",
                  "\n FROM cat_proceso_cnt cp,cat_subcuenta cs",
                  "\n WHERE cp.cod_periodo = cs.subcuenta",
                  "\n GROUP BY 1,2",
                  "\n ORDER BY 1 DESC"

   -- Prepara la consulta para obtener los codigos de proceso
   PREPARE prp_cat_periodo_cnt FROM v_QryTxt
   -- Limpia el combo
   CALL cb.clear()
   LET v_indice = 1
   -- Declara el cursor para la consulta 
   DECLARE cur_cat_periodo_cnt CURSOR FOR prp_cat_periodo_cnt
      FOREACH cur_cat_periodo_cnt INTO arr_cat_periodo_cnt[v_indice].v_cod_periodo_cnt,
                                       arr_cat_periodo_cnt[v_indice].v_desc_periiodo_cnt
         -- Agrega elementos al combobox
         CALL cb.addItem(arr_cat_periodo_cnt[v_indice].v_cod_periodo_cnt,
                         arr_cat_periodo_cnt[v_indice].v_desc_periiodo_cnt)
         LET v_indice = v_indice + 1
      END FOREACH

   --CALL arr_cat_periodo_cnt.deleteElement(v_indice)

END FUNCTION
#OBJETIVO: Mostrar la información de las subcuentas
FUNCTION fn_cmb_subcta(p_cuentas)
DEFINE arr_cat_sctas DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cve_cod_subcta_cnt LIKE cnt_regla_contable.cod_subcta_cnt,
          v_desc_sctas VARCHAR(60)--LIKE cat_proceso_cnt.desc_proceso_cnt
                       END RECORD
DEFINE 
         p_cuentas     STRING,
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
   LET v_indice = 1

   LET v_indice = v_indice + 1
 
   LET v_QryTxt = "\n SELECT subcuenta,subcuenta||'-'||subcuenta_desc",
                  "\n FROM cat_subcuenta",
                  "\n WHERE subcuenta ",p_cuentas,
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

END FUNCTION
#OBJETIVO: Mostrar la información de tipos de credito
FUNCTION fn_cmb_tpo_credito()
DEFINE arr_tpo_crd DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_tpo_credito LIKE cat_tipo_credito.tpo_credito,
          v_desc_credito VARCHAR(60)--LIKE cat_proceso_cnt.desc_proceso_cnt
                       END RECORD
DEFINE v_indice      SMALLINT,                   -- Variable del indice
       v_QryTxt      STRING                     -- Cadena para almacenar Query

   LET cb = ui.ComboBox.forName("v_cmb_tpo_crd") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   -- Limpia el combo
   CALL cb.clear()
   LET v_indice = 1

   LET v_indice = v_indice + 1
 
   LET v_QryTxt = "\n SELECT tpo_credito,tpo_credito||'-'||desc_credito",
                  "\n FROM cat_tipo_credito",
                  "\n WHERE tpo_credito <> 0",
                  "\n ORDER BY 1"
   PREPARE prp_cmb_tpo_crd FROM v_QryTxt

   -- Declara el cursor para la consulta 
   DECLARE cur_cmb_tpo_crd CURSOR FOR prp_cmb_tpo_crd
      FOREACH cur_cmb_tpo_crd INTO arr_tpo_crd[v_indice].v_tpo_credito,
                                   arr_tpo_crd[v_indice].v_desc_credito
         -- Agrega elementos al combobox
         CALL cb.addItem(arr_tpo_crd[v_indice].v_tpo_credito,
                         arr_tpo_crd[v_indice].v_desc_credito)
         LET v_indice = v_indice + 1
      END FOREACH

END FUNCTION