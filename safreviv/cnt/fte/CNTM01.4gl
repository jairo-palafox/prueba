################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 17/05/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTM01                                                   #
#Objetivo          => Realizar el mantenimiento al catálogo de las cuentas     #
#                     contables.                                               #
#Fecha inicio      => 02/05/2012                                               #
################################################################################
DATABASE safre_viv
GLOBALS "CNTG01.4gl"
   
GLOBALS 
DEFINE g_id_arr INTEGER
DEFINE g_transaccion LIKE cat_transaccion_cnt.desc_transaccion_cnt --Cod Transaccion

DEFINE g_arr_inf_cta_contable  DYNAMIC ARRAY OF RECORD    
          v_cta_contable CHAR(66), 
          v_naturaleza_cta CHAR(45),
          v_proceso_cnt CHAR(45),
          v_subcta_cnt CHAR(45),
          v_cve_transaccion SMALLINT
       END RECORD
DEFINE g_arr_inf_cta_cont_trans  DYNAMIC ARRAY OF RECORD    
          v_transaccion_cnt CHAR(45)
       END RECORD

DEFINE g_arr_trans_det DYNAMIC ARRAY OF RECORD --Arreglo descripciones transaccion
          v_desc_trans_cnt   VARCHAR(60)
                             END RECORD     
DEFINE bnd_insertar SMALLINT
DEFINE bnd_actualizar SMALLINT
DEFINE bnd_eliminar SMALLINT
END GLOBALS 

MAIN
--Sección de Variables del Programa
DEFINE v_transaccion LIKE cnt_regla_contable.cod_transaccion_cnt, --Valor transaccion
       f_subcuenta LIKE cnt_regla_contable.cod_subcta_cnt, --Valor de subcuenta
       v_valida_datos SMALLINT, --DECIMAL (10,2),
       v_indice SMALLINT,
       v_QryTxt STRING, 
       f_cta_contable LIKE cnt_regla_contable.cta_contable, 
       f_desc_cta_contable CHAR(66), 
       f_naturaleza_cta SMALLINT,
       f_naturaleza_cta_1 SMALLINT,
       p_programa CHAR(10),
       v_existe_cuenta SMALLINT, 
       v_describe_cuenta CHAR(40),
       bnd_accion SMALLINT,
       v_count_cod_proceso SMALLINT

--Sección de Variable UI
DEFINE f_ventana        ui.Window, -- Define las propìedades de la Ventana
       f_forma          ui.Form -- Define las propiedades de la forma

--Sección Variables de Retorno 
DEFINE v_ind_trans SMALLINT,
       v_ind_scta SMALLINT,
       r_naturaleza  CHAR (45), 
       v_ind_naturaleza SMALLINT
DEFINE arr_transacciones DYNAMIC ARRAY OF RECORD
       v_cve_cod_transaccion_cnt SMALLINT,
       v_desc_transaccion CHAR(45) 
       END RECORD

       
   LET p_programa    = "CNTM01"       
   LET g_usuario      = ARG_VAL(1) -- Recibe la variable de usuario
   LET g_tipo_proceso = ARG_VAL(2) -- Recibe el tipo de proceso
   LET g_nom_prog     = ARG_VAL(3) -- Recibe el nombre del programa

CLOSE WINDOW SCREEN 
 OPEN WINDOW v_cntm01 WITH FORM "CNTM011"
  DIALOG   ATTRIBUTES(UNBUFFERED) 
     INPUT BY NAME v_cmb_proceso, f_subcuenta

     --AFTER INPUT 

     BEFORE INPUT 
        LET f_ventana = ui.Window.getCurrent()
        LET f_forma = f_ventana.getForm()

        --Se invoca la función que asigna el titulo a la ventana
        CALL ui.Interface.setText(g_nom_prog)
        --Oculta las secciones de detalle
        CALL f_forma.setElementHidden("gr_proc_cod", 1) --Oculta el combo de Proceso Cod
        CALL f_forma.setElementHidden("gr_inf_cta_cont", 1) --Oculta la Sección Información Cta Contable 
        CALL f_forma.setElementHidden("gr_inf_cta_cont1", 1) --Oculta la sección Información Cta Contable 
        CALL f_forma.setElementHidden("gr_asig_cta_cont", 1) --Oculta la sección Asignación Cta Contable 
        CALL DIALOG.setActionHidden("insertar", 1) --Oculta el botón "Insertar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("actualizar", 1) --Oculta el botón "Actualizar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("eliminar", 1) --Oculta el botón "Eliminar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("accept", 1) --Oculta el botón "Aceptar"(1 Oculta, 0 Muestra)
        
        --Llena el combo de proceso 
        CALL fn_llena_combo_proceso()
        
        ON CHANGE v_cmb_proceso
          CALL fn_llena_transacciones(v_cmb_proceso) RETURNING arr_transacciones, v_ind_trans
          IF v_ind_trans <= 1 THEN 
             CALL fn_mensaje ("INFORMACION", "El proceso no cuenta con transacciones", "info")
             LET v_cmb_proceso = NULL 
             NEXT FIELD v_cmb_proceso
          ELSE    
             --Muestra la información de la transacción en la seccion de Consulta
             DISPLAY ARRAY arr_transacciones TO scr_desc_transacciones.*
             ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)

             ON ACTION ver 
                LET v_transaccion = arr_transacciones[arr_curr()].v_cve_cod_transaccion_cnt
                DISPLAY arr_transacciones[arr_curr()].v_desc_transaccion TO f_transaccion
             EXIT DISPLAY 
             
             ON ACTION cancel 
                CLEAR FORM
                LET v_cmb_proceso = NULL  
                EXIT DISPLAY
                EXIT DIALOG 
             END DISPLAY 
          END IF 

          CALL fn_llena_combo_subcuenta(v_transaccion) RETURNING f_subcuenta, v_ind_scta, f_subcuenta
             IF v_ind_scta = 0 THEN 
                CALL fn_mensaje("INFORMACION", "La transacción no tiene subcuentas", "warning")
             END IF

        IF LENGTH(v_cmb_proceso) <= 0 OR LENGTH(v_transaccion) <= 0 AND LENGTH(f_subcuenta) <= 0 THEN
           CALL fn_mensaje("INFORMACION", "Los tres parámetros son requeridos", "about")
        ELSE 
           CALL DIALOG.setActionHidden("accept", 0) --Muestra el botón "Aceptar"(1 Oculta, 0 Muestra)
        END IF  
     END INPUT

       --Muestra la sección información cuenta contable 
       DISPLAY ARRAY g_arr_inf_cta_contable  TO scr_inf_cta_cont.*
        BEFORE ROW
           LET g_id_arr = arr_curr() 
           --Llama la función para buscar las transacciones 
           CALL fn_desc_transaccion(g_arr_inf_cta_contable[arr_curr()].v_cve_transaccion)
           --Muestra la sección Información Cta Contable 
           CALL f_forma.setElementHidden("gr_inf_cta_cont1", 0)
       END DISPLAY

       --Muestra la información del detalle de las transacciones
       DISPLAY ARRAY g_arr_trans_det  TO scr_inf_cta_cont_trans.*
       END DISPLAY 
    
     ON ACTION ACCEPT
        CALL fn_llena_combo_naturaleza() 
        RETURNING r_naturaleza, v_ind_naturaleza, f_naturaleza_cta
           --Ejecuta consulta de información a detalle
           CALL fn_consulta_cta_contable(v_cmb_proceso, v_transaccion, f_subcuenta) 
           RETURNING g_arr_inf_cta_contable , g_arr_inf_cta_cont_trans , v_indice
           --Muestra secciones de detalle
           CALL f_forma.setElementHidden("gr_inf_cta_cont", 0) --Muestra la Sección Información Cta Contable 
           CALL f_forma.setElementHidden("gr_inf_cta_cont1", 0) --Muestra la sección Información Cta Contable 
           CALL DIALOG.setActionHidden("insertar", 0) --Muestra el botón "Insertar"(1 Oculta, 0 Muestra)
           CALL DIALOG.setActionHidden("actualizar", 0) --Muestra el botón "Actualizar"(1 Oculta, 0 Muestra)
           CALL DIALOG.setActionHidden("eliminar", 0) --Muestra el botón "Eliminar"(1 Oculta, 0 Muestra)
           CALL DIALOG.setActionHidden("accept", 1) --Oculta el botón "Insertar"(1 Oculta, 0 Muestra)
           LET bnd_accion = 1

     ON ACTION cancelar
       IF bnd_accion = 1 THEN 
          CALL DIALOG.setActionHidden("insertar", 1) --Oculta el botón "Insertar"(1 Oculta, 0 Muestra)
          CALL DIALOG.setActionHidden("actualizar", 1) --Oculta el botón "Actualizar"(1 Oculta, 0 Muestra)
          CALL DIALOG.setActionHidden("eliminar", 1) --Oculta el botón "Eliminar"(1 Oculta, 0 Muestra)
          CALL DIALOG.setActionHidden("accept", 0) --Muestra el botón "Aceptar"(1 Oculta, 0 Muestra)
          LET bnd_accion = 0
       ELSE  
          EXIT DIALOG
        END IF

     ON ACTION insertar
        LET bnd_accion = 2
        CALL f_forma.setElementHidden("gr_asig_cta_cont", 0) --Muestra la sección Información Cta Contable 
        CALL DIALOG.setActionHidden("insertar", 1) --Oculta el botón "Insertar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("actualizar", 1) --Oculta el botón "Actualizar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("eliminar", 1) --Oculta el botón "Eliminar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("accept", 0) --Muestra el botón "Aceptar"(1 Oculta, 0 Muestra)

        INPUT BY NAME f_cta_contable, f_desc_cta_contable, f_naturaleza_cta
        ATTRIBUTES (UNBUFFERED=TRUE) 

BEFORE INPUT 

   --Obtiene el proceso cod
   SELECT COUNT (cod_proceso)
     INTO v_count_cod_proceso
     FROM cnt_proceso
    WHERE cod_proceso_cnt = v_cmb_proceso

    IF v_count_cod_proceso > 1 THEN      
      CALL f_forma.setElementHidden("gr_proc_cod", 0) --Muestra el combo de Proceso Cod
      INPUT BY NAME v_cmb_proceso_cod
          BEFORE INPUT 
          --Llena el combo proceso_cod
          CALL fn_llena_combo_proceso_cod(v_cmb_proceso) RETURNING v_cmb_proceso_cod
          DISPLAY BY NAME v_cmb_proceso_cod

          CALL ui.Interface.refresh()
       END INPUT   
  NEXT FIELD f_cta_contable     
     END IF        
        
             --Valida que la cuenta contable exista en el catalogo  
             AFTER FIELD f_cta_contable
                   SELECT desc_cta_contable 
                     INTO f_desc_cta_contable
                     FROM cat_cuenta_contable
                    WHERE cta_contable = f_cta_contable

               IF LENGTH(f_desc_cta_contable) > 0 THEN
                  DISPLAY f_desc_cta_contable TO f_desc_cta_contable
                     NEXT FIELD f_naturaleza_cta
               ELSE
                  NEXT FIELD f_desc_cta_contable  
               END IF          


               
             ON ACTION ACCEPT
                --Valida que se hayan capturado todos los campos 
                IF LENGTH(f_cta_contable) <= 0 OR LENGTH(f_desc_cta_contable) <= 0 OR LENGTH(f_naturaleza_cta) <= 0 THEN 
                   CALL fn_mensaje ("INFORMACION", "Se deberá capturar todos los campos", "warning")
                ELSE
                   LET bnd_insertar = 0
                   --Si todos los campos están llenos se invoca la función.
                   CALL fn_inserta_cta_contable(v_cmb_proceso, v_transaccion, f_subcuenta,
                                                f_cta_contable, f_desc_cta_contable CLIPPED, 
                                                f_naturaleza_cta, g_usuario, v_cmb_proceso_cod)
                   RETURNING bnd_insertar
                END IF
                
                CALL fn_consulta_cta_contable(v_cmb_proceso, v_transaccion, f_subcuenta) 
                RETURNING g_arr_inf_cta_contable , g_arr_inf_cta_cont_trans , v_indice

                CALL ui.Interface.refresh()
                CALL f_forma.setElementHidden("gr_asig_cta_cont", 1) --Oculta la sección Asignación Cta Contable 
                
             ON ACTION CANCEL
             IF bnd_accion = 2 THEN 
                EXIT INPUT 
                CALL f_forma.setElementHidden("gr_asig_cta_cont", 1) --Oculta la sección Asignación Cta Contable 
                CALL DIALOG.setActionHidden("insertar", 0) --Muestra el botón "Insertar"(1 Oculta, 0 Muestra)
                CALL DIALOG.setActionHidden("actualizar", 0) --Muestra el botón "Actualizar"(1 Oculta, 0 Muestra)
                CALL DIALOG.setActionHidden("eliminar", 0) --Muestra el botón "Eliminar"(1 Oculta, 0 Muestra)
             ELSE 
                EXIT DIALOG
             END IF 
             END INPUT

                IF bnd_insertar = 1 THEN
                   CALL DIALOG.setActionHidden("accept", 1) --Ocultar el botón "Eliminar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("insertar", 0) --Muestra el botón "Insertar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("actualizar", 0) --Muestra el botón "Actualizar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("eliminar", 0) --Muestra el botón "Eliminar"(1 Oculta, 0 Muestra)
                END IF               
              
     ON ACTION actualizar
        CALL f_forma.setElementHidden("gr_asig_cta_cont", 0) --Muestra la sección Información Cta Contable 
        CALL DIALOG.setActionHidden("insertar", 1) --Oculta el botón "Insertar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("actualizar", 1) --Oculta el botón "Actualizar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("eliminar", 1) --Oculta el botón "Eliminar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("accept", 0) --Muestra el botón "Aceptar"(1 Oculta, 0 Muestra)

        --INPUT BY NAME f_cta_contable, f_desc_cta_contable, f_naturaleza_cta
        INPUT BY NAME  f_desc_cta_contable, f_naturaleza_cta
        ATTRIBUTES (UNBUFFERED=FALSE) 

        BEFORE INPUT 
           LET f_cta_contable = g_arr_inf_cta_contable [arr_curr()].v_cta_contable[1,10]
           LET f_desc_cta_contable =  g_arr_inf_cta_contable [arr_curr()].v_cta_contable[12,45]
           LET f_naturaleza_cta = g_arr_inf_cta_contable [arr_curr()].v_naturaleza_cta

        --Llama la función que llena el combo de la naturaleza
        CALL fn_llena_combo_naturaleza() 
        RETURNING r_naturaleza, v_ind_naturaleza, f_naturaleza_cta
           
           DISPLAY f_cta_contable TO f_cta_contable
           DISPLAY f_desc_cta_contable TO f_desc_cta_contable
           DISPLAY f_naturaleza_cta TO f_naturaleza_cta

             ON ACTION ACCEPT

             --CALL GET_FLDBUF(f_cta_contable, f_desc_cta_contable, f_naturaleza_cta)
            CALL GET_FLDBUF(f_desc_cta_contable, f_naturaleza_cta)  
             --RETURNING f_cta_contable, f_desc_cta_contable, f_naturaleza_cta
             RETURNING f_desc_cta_contable, f_naturaleza_cta_1

                --Valida que se hayan capturado todos los campos 
                IF LENGTH(f_desc_cta_contable) <= 0 OR LENGTH(f_naturaleza_cta) <= 0 THEN 
                   CALL fn_mensaje ("INFORMACION", "Se deberá capturar todos los campos", "warning")
                ELSE
                   --Si todos los campos están llenos se invoca la función.
                   CALL fn_actualiza_cuentas(v_cmb_proceso, v_transaccion, f_subcuenta,
                                             f_cta_contable, f_desc_cta_contable CLIPPED, 
                                             f_naturaleza_cta_1, g_usuario, f_naturaleza_cta)
                   RETURNING bnd_actualizar
                END IF

                CALL fn_consulta_cta_contable(v_cmb_proceso, v_transaccion, f_subcuenta) 
                RETURNING g_arr_inf_cta_contable , g_arr_inf_cta_cont_trans , v_indice

                CALL ui.Interface.refresh()  
                CALL f_forma.setElementHidden("gr_asig_cta_cont", 1) --Oculta la sección Asignación Cta Contable               

             ON ACTION CANCEL
                EXIT INPUT 
                CALL f_forma.setElementHidden("gr_asig_cta_cont", 1) --Oculta la sección Asignación Cta Contable 
                CALL DIALOG.setActionHidden("insertar", 0) --Muestra el botón "Insertar"(1 Oculta, 0 Muestra)
                CALL DIALOG.setActionHidden("actualizar", 0) --Muestra el botón "Actualizar"(1 Oculta, 0 Muestra)
                CALL DIALOG.setActionHidden("eliminar", 0) --Muestra el botón "Eliminar"(1 Oculta, 0 Muestra)
             END INPUT

                IF bnd_actualizar = 1 THEN
                   CALL DIALOG.setActionHidden("accept", 1) --Ocultar el botón "Eliminar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("insertar", 0) --Muestra el botón "Insertar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("actualizar", 0) --Muestra el botón "Actualizar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("eliminar", 0) --Muestra el botón "Eliminar"(1 Oculta, 0 Muestra)
                END IF        

     ON ACTION eliminar
        CALL f_forma.setElementHidden("gr_asig_cta_cont", 0) --Muestra la sección Información Cta Contable 
        CALL DIALOG.setActionHidden("insertar", 1) --Oculta el botón "Insertar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("actualizar", 1) --Oculta el botón "Actualizar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("eliminar", 1) --Oculta el botón "Eliminar"(1 Oculta, 0 Muestra)
        CALL DIALOG.setActionHidden("accept", 0) --Muestra el botón "Aceptar"(1 Oculta, 0 Muestra)

        --Llama la función que llena el combo de la naturaleza
        CALL fn_llena_combo_naturaleza() 
        RETURNING r_naturaleza, v_ind_naturaleza, f_naturaleza_cta
               
        LET f_cta_contable = g_arr_inf_cta_contable [arr_curr()].v_cta_contable[1,10]
        LET f_desc_cta_contable =  g_arr_inf_cta_contable [arr_curr()].v_cta_contable[12,45]
        LET f_naturaleza_cta = g_arr_inf_cta_contable[arr_curr()].v_naturaleza_cta[1]
        
        DISPLAY f_cta_contable TO f_cta_contable
        DISPLAY f_desc_cta_contable TO f_desc_cta_contable       
        DISPLAY f_naturaleza_cta TO f_naturaleza_cta

           --Si todos los campos están llenos se invoca la función.
           CALL fn_elimina_cta_contable(v_cmb_proceso, v_transaccion, f_subcuenta,
                                        f_cta_contable, f_desc_cta_contable CLIPPED, 
                                        f_naturaleza_cta, g_usuario)
           RETURNING bnd_eliminar                                         
                
                CALL fn_consulta_cta_contable(v_cmb_proceso, v_transaccion, f_subcuenta) 
                RETURNING g_arr_inf_cta_contable , g_arr_inf_cta_cont_trans , v_indice

                CALL ui.Interface.refresh()
                CALL f_forma.setElementHidden("gr_asig_cta_cont", 1) --Oculta la sección Asignación Cta Contable 
       
                IF bnd_eliminar = 1 THEN
                   CALL DIALOG.setActionHidden("accept", 1) --Ocultar el botón "Eliminar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("insertar", 0) --Muestra el botón "Insertar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("actualizar", 0) --Muestra el botón "Actualizar"(1 Oculta, 0 Muestra)
                   CALL DIALOG.setActionHidden("eliminar", 0) --Muestra el botón "Eliminar"(1 Oculta, 0 Muestra)
                END IF        

  END DIALOG
 CLOSE WINDOW v_cntm01
END MAIN

#OBJETIVO: Llenar el ComboBox para seleccionar el proceso
FUNCTION fn_llena_combo_proceso_original()
DEFINE arr_cat_proceso_cnt DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cve_cod_proceso_cnt LIKE cat_proceso_cnt.cod_proceso_cnt,
          v_desc_proceso_cnt CHAR(45)--LIKE cat_proceso_cnt.desc_proceso_cnt
       END RECORD
DEFINE f_proceso LIKE cat_proceso_cnt.desc_proceso_cnt,
       v_indice     SMALLINT,                   -- Variable del indice
       v_QryTxt     STRING,                     -- Cadena para almacenar Query
       cb           ui.ComboBox                 -- Variable de Combobox

   LET cb = ui.ComboBox.forName("f_proceso") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1

         LET v_QryTxt = "\n SELECT cod_proceso_cnt, cod_proceso_cnt || '-' || ",
                        "\n        desc_proceso_cnt",
                        "\n   FROM cat_proceso_cnt" 

   -- Prepara la consulta para obtener los codigos de proceso
   PREPARE prp_cat_proceso_cnt FROM v_QryTxt

   -- Limpia el combo
   CALL cb.clear()

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_cb_proc_cnt CURSOR FOR prp_cat_proceso_cnt
      FOREACH cur_llena_cb_proc_cnt INTO arr_cat_proceso_cnt[v_indice].v_cve_cod_proceso_cnt, 
                                         arr_cat_proceso_cnt[v_indice].v_desc_proceso_cnt
         -- Agrega elementos al combobox
         CALL cb.addItem(arr_cat_proceso_cnt[v_indice].v_cve_cod_proceso_cnt,
                         arr_cat_proceso_cnt[v_indice].v_desc_proceso_cnt)
         LET v_indice = v_indice + 1

      END FOREACH

   LET v_indice = v_indice - 1

   CALL arr_cat_proceso_cnt.deleteElement(v_indice)

   RETURN f_proceso, v_indice
   
END FUNCTION

#OBJETIVO: Mostrar la información de las transacciones del proceso
FUNCTION fn_llena_transacciones(p_proceso_cnt)
DEFINE p_proceso_cnt SMALLINT,
       v_indice SMALLINT,
       v_QryTxt STRING 
DEFINE arr_transacciones DYNAMIC ARRAY OF RECORD
       v_cve_cod_transaccion_cnt SMALLINT,
       v_desc_transaccion CHAR(45) 
       END RECORD 
       
   LET v_indice = 1

         LET v_QryTxt = "\n SELECT rc.cod_transaccion_cnt, ", 
                        "\n        rc.cod_transaccion_cnt || '-' || ",
                        "\n        ct.desc_transaccion_cnt",
                        "\n   FROM cnt_regla_contable rc, cat_transaccion_cnt ct",
                        --"\n   JOIN cat_transaccion_cnt ct",
                        --"\n     ON rc.cod_transaccion_cnt = ct.cod_transaccion_cnt",
                        "\n  WHERE rc.cod_transaccion_cnt = ct.cod_transaccion_cnt", 
                        "\n    AND cod_proceso_cnt =", p_proceso_cnt,                        
                        "\n  GROUP BY 1,2"

   -- Prepara la consulta para obtener las transacciones del proceso
   PREPARE prp_trans_proc FROM v_QryTxt

   -- Declara el cursor para la consulta 
   DECLARE cur_trans_proc CURSOR FOR prp_trans_proc
      FOREACH cur_trans_proc INTO arr_transacciones[v_indice].v_cve_cod_transaccion_cnt,
                                  arr_transacciones[v_indice].v_desc_transaccion
         LET v_indice = v_indice + 1
      END FOREACH

   CALL arr_transacciones.deleteElement(v_indice)
   
   RETURN arr_transacciones, v_indice
END FUNCTION

#OBJETIVO: Mostrar la información de las subcuentas
FUNCTION fn_llena_combo_subcuenta(p_transaccion)
DEFINE arr_cat_sctas DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cve_cod_subcta_cnt LIKE cnt_regla_contable.cod_subcta_cnt,
          v_desc_sctas CHAR(45)--LIKE cat_proceso_cnt.desc_proceso_cnt
       END RECORD
DEFINE p_transaccion LIKE cnt_regla_contable.cod_transaccion_cnt,
       f_subcuenta SMALLINT,
       v_indice     SMALLINT,                   -- Variable del indice
       v_QryTxt     STRING,                     -- Cadena para almacenar Query
       cb           ui.ComboBox                 -- Variable de Combobox

   LET cb = ui.ComboBox.forName("f_subcuenta") --Asignación del combo a la forma

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   LET v_indice = 1

         LET v_QryTxt = "\n SELECT rc.cod_subcta_cnt, ", 
                        "\n        rc.cod_subcta_cnt || '-' || ",
                        "\n        cs.subcuenta_desc",
                        "\n   FROM cnt_regla_contable rc",
                        "\n   JOIN cat_subcuenta cs",
                        "\n     ON rc.cod_subcta_cnt = cs.subcuenta",
                        "\n  WHERE cod_transaccion_cnt = ", p_transaccion,
                        "\n  GROUP BY 1,2"

   -- Prepara la consulta para obtener los codigos de proceso
   PREPARE prp_cons_subctas FROM v_QryTxt

   -- Limpia el combo
   CALL cb.clear()

   -- Declara el cursor para la consulta 
   DECLARE cur_llena_cb_sctas CURSOR FOR prp_cons_subctas
      FOREACH cur_llena_cb_sctas INTO arr_cat_sctas[v_indice].v_cve_cod_subcta_cnt,
                                      arr_cat_sctas[v_indice].v_desc_sctas
         -- Agrega elementos al combobox
         CALL cb.addItem(arr_cat_sctas[v_indice].v_cve_cod_subcta_cnt,
                         arr_cat_sctas[v_indice].v_desc_sctas)
         LET v_indice = v_indice + 1
      END FOREACH

   CALL arr_cat_sctas.deleteElement(v_indice)

   RETURN f_subcuenta, v_indice, arr_cat_sctas[1].v_cve_cod_subcta_cnt
   
END FUNCTION

#OBJETIVO: Consultar información de la cuenta contable
FUNCTION fn_consulta_cta_contable(p_proceso, p_transaccion, p_subcuenta)
DEFINE p_proceso, 
       p_transaccion,
       p_subcuenta,
       v_indice SMALLINT,
       v_indice1 SMALLINT,
       v_QryTxt STRING 

   LET v_indice = 1
   LET v_indice1 = 1

           LET v_QryTxt = "\n SELECT rc.cta_contable || '-' ||",   
                          "\n        cc.desc_cta_contable AS CTA_CONTABLE,",
                          "\n        rc.cod_naturaleza_cta || '-' ||",
                          "\n        nm.desc_naturaleza_mov AS NATURALEZA_CTA,",
                          "\n        rc.cod_proceso_cnt || '-' ||",
                          "\n        dp.desc_proceso_cnt AS PROCESO,",
                          "\n        rc.cod_subcta_cnt || '-' ||",
                          "\n        ds.subcuenta_desc AS SUBCUENTA,",
                          "\n        rc.cod_transaccion_cnt,",                          
                          "\n        rc.cod_transaccion_cnt || '-' || ",
                          "\n        ct.desc_transaccion_cnt AS TRANSACCION",
                          "\n   FROM cnt_regla_contable rc, cat_cuenta_contable cc,",
                          "\n        cat_naturaleza_mov nm, cat_proceso_cnt dp,",
                          "\n        cat_subcuenta ds, cat_transaccion_cnt ct",
                          "\n  WHERE rc.cta_contable = cc.cta_contable",                          
                          "\n    AND rc.cod_naturaleza_cta = nm.cod_naturaleza_mov ",                          
                          "\n    AND rc.cod_proceso_cnt = dp.cod_proceso_cnt",                          
                          "\n    AND rc.cod_subcta_cnt = ds.subcuenta",                          
                          "\n    AND rc.cod_transaccion_cnt  = ct.cod_transaccion_cnt", ##se modifico tambien
                          "\n    AND rc.cod_proceso_cnt =",p_proceso,
                          "\n    AND rc.cod_transaccion_cnt =", p_transaccion,
                          "\n    AND rc.cod_subcta_cnt =", p_subcuenta
   -- Prepara la consulta para obtener las transacciones del proceso
   PREPARE prp_inf_cta_contable FROM v_QryTxt

   -- Declara el cursor para la consulta 
   DECLARE cur_inf_cta_contable CURSOR FOR prp_inf_cta_contable
      FOREACH cur_inf_cta_contable INTO g_arr_inf_cta_contable[v_indice].v_cta_contable,
                                        g_arr_inf_cta_contable[v_indice].v_naturaleza_cta,
                                        g_arr_inf_cta_contable[v_indice].v_proceso_cnt,
                                        g_arr_inf_cta_contable[v_indice].v_subcta_cnt,
                                        g_arr_inf_cta_contable[v_indice].v_cve_transaccion,
                                        g_arr_inf_cta_cont_trans[v_indice].v_transaccion_cnt
         LET v_indice = v_indice + 1
      END FOREACH

   CALL g_arr_inf_cta_contable .deleteElement(v_indice)
   CALL g_arr_inf_cta_cont_trans .deleteElement(v_indice)

   RETURN g_arr_inf_cta_contable , g_arr_inf_cta_cont_trans , v_indice
END FUNCTION 

#OBJETIVO: Mostrar la información de la naturaleza de las cuentas
FUNCTION fn_llena_combo_naturaleza()
DEFINE arr_naturaleza_cta DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
          v_cve_cod_naturaleza_cta LIKE cnt_regla_contable.cod_naturaleza_cta,
          v_naturaleza CHAR(45)--LIKE cat_proceso_cnt.desc_proceso_cnt
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

   RETURN f_naturaleza_cta, v_indice, arr_naturaleza_cta[1].v_cve_cod_naturaleza_cta
   
END FUNCTION

FUNCTION fn_inserta_cta_contable(p_proceso_cnt, p_transaccion, p_subcuenta,
                                 p_cta_contable, p_desc_cta_contable, 
                                 p_naturaleza_cta, g_usuario, p_cmb_proceso_cod)
DEFINE  p_proceso_cnt LIKE cnt_regla_contable.cod_proceso_cnt, 
        p_transaccion LIKE cnt_regla_contable.cod_transaccion_cnt, 
        p_subcuenta LIKE cnt_regla_contable.cod_subcta_cnt,
        p_cta_contable LIKE cnt_regla_contable.cta_contable, 
        p_desc_cta_contable CHAR(55),
        p_naturaleza_cta LIKE cnt_regla_contable.cod_naturaleza_cta,
        v_existe_cta_contable SMALLINT,
        v_existe_regla_contable SMALLINT,
        v_f_actualiza DATE,
        v_id_cta_contable DECIMAL(20,0), 
        r_bandera SMALLINT,
        v_cod_proceso SMALLINT,
        p_cmb_proceso_cod SMALLINT,
        v_count_cod_proceso SMALLINT,
        v_ins_cta_contable LIKE cnt_regla_contable.cta_contable,
        v_ins_desc_cta_contable LIKE cat_cuenta_contable.desc_cta_contable,
        g_usuario LIKE seg_usuario.usuario

   LET v_f_actualiza = TODAY

   --Asigna valor de ID cta contable 
   SELECT MAX (id_cuenta_contable)
     INTO v_id_cta_contable  
     FROM cnt_regla_contable
    WHERE cod_proceso_cnt = p_proceso_cnt

    LET v_id_cta_contable = v_id_cta_contable + 1

    DISPLAY "valor proceso cod", p_cmb_proceso_cod
   --Obtiene el proceso cod
--IF p_cmb_proceso_cod = 0 THEN 
   --SELECT cod_proceso
     --INTO p_cmb_proceso_cod
     --FROM cnt_proceso
    --WHERE cod_proceso_cnt = p_proceso_cnt
--END IF     

   --Valida que la cuenta contable exista en el catalogo  
   SELECT COUNT (cta_contable)
     INTO v_existe_cta_contable
     FROM cat_cuenta_contable
    WHERE cta_contable = p_cta_contable

    --Si la cuenta existe 
    IF v_existe_cta_contable <> 0 THEN 
       --Valida que la cuenta exista en regla contable 
       SELECT COUNT (id_cuenta_contable)
         INTO v_existe_regla_contable
         FROM cnt_regla_contable
        WHERE cod_proceso_cnt = p_proceso_cnt
          AND cod_transaccion_cnt = p_transaccion
          AND cod_subcta_cnt = p_subcuenta
          AND cta_contable = p_cta_contable
          AND cod_naturaleza_cta = p_naturaleza_cta
        
       --Si la cuenta existe en regla contable
       IF v_existe_regla_contable <> 0 THEN
          CALL fn_mensaje("INFORMACION", "Cuenta Contable ya existe dada de alta", "information")
       END IF
    END IF 

    --Si la cuenta no existe en Catalogo ni en Regla Contable  
    IF v_existe_cta_contable = 0 AND v_existe_regla_contable = 0 THEN
       --Solicitar confirmación del alta 
       CALL fn_ventana_confirma ("CONFIRMAR", 
                                 "¿Está seguro que desea dar de alta la cuenta contable?", 
                                 "quest")
       RETURNING r_bandera

          --Si el usuario ACEPTA 
          IF r_bandera = 1 THEN
             --Inserta en el Catálogo de Cuenta Contable 
               LET p_desc_cta_contable = p_desc_cta_contable CLIPPED      
               INSERT INTO cat_cuenta_contable
               VALUES (p_cta_contable, p_desc_cta_contable, 
                       v_f_actualiza, g_usuario);
             --Inserta en Cuenta Regla Contable
               INSERT INTO cnt_regla_contable
               VALUES (v_id_cta_contable, p_proceso_cnt, p_cmb_proceso_cod,--v_cod_proceso,
                       p_transaccion, p_subcuenta, p_cta_contable,
                       p_naturaleza_cta, v_f_actualiza, g_usuario);
             LET bnd_insertar = 1                   
           END IF
    END IF   
    DISPLAY "bandera insertar 1 ", bnd_insertar    
    --Si la cuenta existe en catalogo insertar solo en regla
    IF v_existe_cta_contable <> 0 AND v_existe_regla_contable = 0 THEN
       --Solicitar confirmación del alta 
        CALL fn_ventana_confirma ("CONFIRMAR", 
                                  "¿Está seguro que desea dar de alta la cuenta contable?", 
                                  "quest")
          RETURNING r_bandera

          --Si el usuario ACEPTA 
          IF r_bandera = 1 THEN
             --Inserta en Cuenta Regla Contable  
             INSERT INTO cnt_regla_contable
             VALUES (v_id_cta_contable, p_proceso_cnt, p_cmb_proceso_cod,--v_cod_proceso,
                     p_transaccion, p_subcuenta, p_cta_contable,
                     p_naturaleza_cta, v_f_actualiza, g_usuario);
             LET bnd_insertar = 1                   
          END IF
    END IF

    DISPLAY "bandera insertar 2 ", bnd_insertar

    IF bnd_insertar = 1 THEN 
    CALL fn_mensaje("INFORMACION", 
                    "La Cuenta Contable ha sido dada de alta", 
                    "information")
    END IF 
    RETURN bnd_insertar
END FUNCTION 

FUNCTION fn_actualiza_cuentas(p_proceso_cnt, p_transaccion, p_subcuenta,
                              p_cta_contable, p_desc_cta_contable, 
                              p_naturaleza_cta, g_usuario, p_naturaleza_cta_1)
DEFINE  p_proceso_cnt LIKE cnt_regla_contable.cod_proceso_cnt, 
        p_transaccion LIKE cnt_regla_contable.cod_transaccion_cnt, 
        p_subcuenta LIKE cnt_regla_contable.cod_subcta_cnt,
        p_cta_contable LIKE cnt_regla_contable.cta_contable, 
        p_desc_cta_contable CHAR(45),
        p_naturaleza_cta LIKE cnt_regla_contable.cod_naturaleza_cta,
        p_naturaleza_cta_1 LIKE cnt_regla_contable.cod_naturaleza_cta,
        v_existe_cta_contable SMALLINT,
        v_existe_regla_contable SMALLINT,
        v_valida_desc_cta SMALLINT,
        r_bandera SMALLINT,
        g_usuario LIKE seg_usuario.usuario

   IF LENGTH(p_cta_contable) = 0 OR LENGTH(p_naturaleza_cta) = 0 OR LENGTH(p_desc_cta_contable) = 0 THEN
      CALL fn_mensaje("INFORMACION",
                      "Se deberá capturar todos los campos",
                      "about")   
   ELSE 
      --Valida que la cuenta contable exista en el catalogo  
      SELECT COUNT (cta_contable)
        INTO v_existe_cta_contable
        FROM cat_cuenta_contable
       WHERE cta_contable = p_cta_contable
   
       --Si la cuenta existe 
       IF v_existe_cta_contable <=0 THEN 
          CALL fn_mensaje("INFORMACION",
                          "Cuenta Contable no existe en catálogo, favor de realizar el alta",
                          "about")  
       ELSE
          --Valida que la cuenta exista en regla contable 
          SELECT COUNT (id_cuenta_contable)
            INTO v_existe_regla_contable
            FROM cnt_regla_contable
           WHERE cod_proceso_cnt = p_proceso_cnt
             AND cod_transaccion_cnt = p_transaccion
             AND cod_subcta_cnt = p_subcuenta
             AND cta_contable = p_cta_contable
             AND cod_naturaleza_cta = p_naturaleza_cta


             --Valida que la descripcion se modifique
             SELECT COUNT (desc_cta_contable)
               INTO v_valida_desc_cta
               FROM cat_cuenta_contable
              WHERE desc_cta_contable = p_desc_cta_contable
           
          --Si la cuenta existe en regla contable
          IF v_existe_regla_contable <> 0 AND v_valida_desc_cta <> 0 THEN
             CALL fn_mensaje("INFORMACION", 
                             "Cuenta Contable ya existe dada de alta", 
                             "information")           
          ELSE
             --Solicitar confirmación de la modificación
             CALL fn_ventana_confirma ("CONFIRMAR", 
                                       "¿Está seguro de la actualización de la cuenta contable?", 
                                       "quest")
             RETURNING r_bandera
                --Si el usuario ACEPTA 
                IF r_bandera = 1 THEN
                   --Inserta en el Catálogo de Cuenta Contable 
                     LET p_desc_cta_contable = p_desc_cta_contable CLIPPED      
                   --Actualiza descripción en el catálogo
                  UPDATE cat_cuenta_contable
                     SET desc_cta_contable = p_desc_cta_contable,
                         f_actualiza    = TODAY,
                         usuario        = g_usuario
                   WHERE cta_contable = p_cta_contable

                  --Actualiza en Cuenta Regla Contable
                  UPDATE cnt_regla_contable
                     SET cta_contable = p_cta_contable,
                         cod_naturaleza_cta = p_naturaleza_cta,
                         f_actualiza    = TODAY,
                         usuario        = g_usuario
                   WHERE cod_proceso_cnt = p_proceso_cnt
                     AND cod_transaccion_cnt = p_transaccion
                     AND cod_subcta_cnt = p_subcuenta
                     AND cta_contable = p_cta_contable
                     AND cod_naturaleza_cta = p_naturaleza_cta_1
                     
                     LET bnd_actualizar = 1
                   
                   CALL fn_mensaje("INFORMACION", 
                                   "La información de la Cuenta Contable ha sido actualizada", 
                                   "information")
                END IF
         END IF
      END IF 
   END IF

   RETURN bnd_actualizar
END FUNCTION  

FUNCTION fn_elimina_cta_contable(p_proceso_cnt, p_transaccion, p_subcuenta,
                                 p_cta_contable, p_desc_cta_contable, 
                                 p_naturaleza_cta, g_usuario)
DEFINE  p_proceso_cnt LIKE cnt_regla_contable.cod_proceso_cnt, 
        p_transaccion LIKE cnt_regla_contable.cod_transaccion_cnt, 
        p_subcuenta LIKE cnt_regla_contable.cod_subcta_cnt,
        p_cta_contable LIKE cnt_regla_contable.cta_contable, 
        p_desc_cta_contable CHAR(45),
        p_naturaleza_cta LIKE cnt_regla_contable.cod_naturaleza_cta,
        r_bandera SMALLINT,
        g_usuario LIKE seg_usuario.usuario

        --Solicitar confirmación del alta 
        CALL fn_ventana_confirma ("CONFIRMAR", 
                                  "¿Está seguro que desea eliminar la cuenta contable?", 
                                  "quest")
        RETURNING r_bandera
          
          --Si el usuario ACEPTA 
          IF r_bandera = 1 THEN
             --Inserta en Cuenta Regla Contable  
             DELETE FROM cnt_regla_contable
              WHERE cod_proceso_cnt = p_proceso_cnt
                AND cod_transaccion_cnt = p_transaccion
                AND cod_subcta_cnt = p_subcuenta
                AND cta_contable = p_cta_contable
                AND cod_naturaleza_cta = p_naturaleza_cta

                CALL fn_mensaje("INFORMACION", 
                                "La Cuenta Contable ha sido eliminada", 
                                "information")
                LET bnd_eliminar = 1
          END IF
   RETURN bnd_eliminar
END FUNCTION

#Objetivo: Obtiene la descripcion de la transaccion
FUNCTION fn_desc_transaccion(p_cod_transaccion_cnt)
DEFINE p_cod_transaccion_cnt LIKE cat_transaccion_cnt.cod_transaccion_cnt
DEFINE v_id_trnas2 INTEGER,
       v_QryTxt     STRING -- Cadena para almacenar Query

   LET v_QryTxt = "\n SELECT cod_transaccion_cnt||'-'||desc_transaccion_cnt,",
                  "\n        desc_transaccion_cnt",
                  "\n FROM cat_transaccion_cnt",
                  "\n WHERE cod_transaccion_cnt = ",p_cod_transaccion_cnt
                  
   PREPARE prp_desc_tran FROM v_QryTxt

   LET v_id_trnas2 = 1
   DECLARE cur_desc_trans_cnt CURSOR FOR prp_desc_tran
      FOREACH cur_desc_trans_cnt INTO g_arr_trans_det[v_id_trnas2].v_desc_trans_cnt,
                                      g_transaccion
         LET v_id_trnas2 = v_id_trnas2 + 1
      END FOREACH
   CALL g_arr_trans_det.deleteElement(v_id_trnas2)    
END FUNCTION