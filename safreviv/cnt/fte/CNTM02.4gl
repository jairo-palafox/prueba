################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 08/05/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTM02                                                   #
#Objetivo          => Programa para el mantenimiento del catálogo de           #
#                     Transacciones                                            #
#Fecha inicio      => 04/05/2012                                               #
################################################################################
DATABASE   
     safre_viv
GLOBALS "CNTG01.4gl"
GLOBALS
DEFINE v_QryTxt          STRING
DEFINE g_transaccion     LIKE cat_transaccion_cnt.desc_transaccion_cnt --Cod Transaccion
DEFINE g_cod_transaccion LIKE cat_transaccion_cnt.cod_transaccion_cnt --Desc Transaccion
DEFINE g_id_arr          INTEGER --Indice global del cursor
DEFINE g_arr_trans_all   DYNAMIC ARRAY OF RECORD --Arreglo regla contable
          arr_cod_pro_cnt LIKE cnt_regla_contable.cod_proceso_cnt,
          arr_cod_trn_cnt LIKE cnt_regla_contable.cod_transaccion_cnt,
          arr_cod_sub_cat LIKE cnt_regla_contable.cod_subcta_cnt
                             END RECORD
DEFINE g_arr_transaccion DYNAMIC ARRAY OF RECORD --Arreglo codigos regal contable
          v_cod_trans_cnt    LIKE cnt_regla_contable.cod_transaccion_cnt,
          v_cod_proceso_cnt  VARCHAR(60),
          v_cod_subcta_cnt   VARCHAR(60)
                             END RECORD
DEFINE g_arr_trans_det DYNAMIC ARRAY OF RECORD --Arreglo descripciones transaccion
          v_desc_trans_cnt   VARCHAR(60)
                             END RECORD
END GLOBALS
MAIN
DEFINE f_ventana     ui.Window, -- Define las propìedades de la Ventana
       f_forma       ui.Form,   -- Define las propiedades de la forma
       v_bnd_action  SMALLINT,  -- Bandera para ejecutar los botones de accion
       r_vtn_action  SMALLINT,  -- Bandera retorna valor de funcion de botones
       v_str_colm    STRING 

   -- se asignan los parametros que vienen del fglrun
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog     = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW vtn_cntm02 WITH FORM "CNTM021"
      DIALOG   ATTRIBUTES(UNBUFFERED) 
         INPUT v_cmb_proceso
          FROM v_cmb_proceso
            BEFORE INPUT
               LET f_ventana = ui.Window.getCurrent()
               LET f_forma = f_ventana.getForm()
               
               IF v_cmb_proceso IS NULL THEN 
                  
                     
                     CALL f_forma.setElementHidden("grp_inf_trasn",1)
                     CALL f_forma.setElementHidden("grp_det_trasn",1)
                     CALL f_forma.setElementHidden("grp_asg_trasn",1)
                     CALL f_forma.setElementHidden("grp_con_sub_cta",1)
                     CALL f_forma.setElementHidden("grp_proceso_cod",1)
                     CALL DIALOG.setActionHidden("insertar",1)
                     CALL DIALOG.setActionHidden("actualizar",1)
                     CALL DIALOG.setActionHidden("eliminar",1)
                     CALL DIALOG.setActionHidden("accept",1)
                     CALL fn_llena_combo_proceso()

               ELSE 
                  --Si v_cmb_proceso ya trea un valor, se muestran las otras secciones y se vuelven a activar la funcionalidad
                     CALL f_forma.setElementHidden("grp_inf_trasn",0)
                     CALL f_forma.setElementHidden("grp_det_trasn",0)
                     CALL f_forma.setElementHidden("grp_asg_trasn",1)
                     CALL f_forma.setElementHidden("grp_con_sub_cta",1)
                     CALL f_forma.setElementHidden("grp_proceso_cod",1)
                     CALL DIALOG.setActionHidden("insertar",0)
                     CALL DIALOG.setActionHidden("actualizar",0)
                     CALL DIALOG.setActionHidden("eliminar",0)
                     CALL DIALOG.setActionHidden("accept",1)

                     LET v_bnd_action = 1
                     LET g_transaccion = ""
                     DISPLAY g_transaccion TO f_transaccion
               END IF 
               

               

           ON CHANGE v_cmb_proceso --Cambia datos segun proceso contable
              --Carga datos deacuerdo al codigo selleccionado en el combo
              CALL g_arr_transaccion.clear()
              CALL g_arr_trans_det.clear()
              CALL fn_llena_tran_detalle(v_cmb_proceso)
                                    RETURNING g_arr_transaccion,g_arr_trans_det,
                                              g_arr_trans_all
               
             --Si encuentra regostro muestra los datos de regla contable
             IF g_arr_transaccion.getLength() > 0 THEN
                CALL f_forma.setElementHidden("grp_inf_trasn",0)
                CALL f_forma.setElementHidden("grp_det_trasn",0)
                CALL DIALOG.setActionHidden("insertar",0)
                CALL DIALOG.setActionHidden("actualizar",0)
                CALL DIALOG.setActionHidden("eliminar",0)
             ELSE
                CALL fn_mensaje("ATENCIÓN",
                                "No hay datos con el proceso seleccionado",
                                "about")
                CALL f_forma.setElementHidden("grp_inf_trasn",1)
                CALL f_forma.setElementHidden("grp_det_trasn",1)
                CALL f_forma.setElementHidden("grp_asg_trasn",1)
                CALL f_forma.setElementHidden("grp_con_sub_cta",1)
                CALL DIALOG.setActionHidden("accept",1)
                CALL DIALOG.setActionHidden("insertar",1)
                CALL DIALOG.setActionHidden("actualizar",1)
                CALL DIALOG.setActionHidden("eliminar",1)
             END IF

         END INPUT

         --Despliega arreglo de regla contable
         DISPLAY ARRAY g_arr_transaccion TO scr_trans_det.*
            BEFORE ROW
              LET g_id_arr = DIALOG.getCurrentRow("scr_trans_det")  --El indice debe tomarse de la descripción
              CALL fn_desc_transaccion(g_arr_transaccion[g_id_arr].v_cod_trans_cnt)-- RETURNING g_transaccion
              
         END DISPLAY

         --Despliega arreglo del detalle de la transaccion
         DISPLAY ARRAY g_arr_trans_det TO scr_trans_desc.* 

         END DISPLAY
      
         

            --Input de la forma para insertar o actualizar
            INPUT g_transaccion,v_cmb_sub_cta,v_cmb_proceso_cod
             FROM f_transaccion,v_cmb_sub_cta,v_cmb_proceso_cod
                BEFORE INPUT  --Llena combo de subcuenta segun codigo transacción
                     
                  IF v_bnd_action = 1 THEN
                     DISPLAY g_transaccion TO f_transaccion
                     LET g_transaccion = ""
                     
                  END IF 

                
                   CALL fn_combo_subcuenta(g_arr_transaccion[arr_curr()].v_cod_trans_cnt,
                                           g_arr_trans_all[arr_curr()].arr_cod_sub_cat)
                                           RETURNING v_cmb_sub_cta
                                           
                  CALL fn_llena_combo_proceso_cod(v_cmb_proceso) RETURNING v_cmb_proceso_cod
                  --CALL g_arr_transaccion.clear()
                  
                     DISPLAY BY NAME v_cmb_proceso_cod

                     CALL ui.Interface.refresh()
             END INPUT
               
         ON ACTION ACCEPT
            --Valida si se presiono boton eliminar no requiere de validaciones
            IF v_bnd_action = 3 THEN
               LET g_id_arr = DIALOG.getCurrentRow("scr_trans_det")  --El indice debe tomarse de la descripción
               --LET g_id_arr = 0
            ELSE
               --Valida que la trasaccion no este vacia o nula
               IF LENGTH(g_transaccion) = 0 THEN
                  CALL fn_mensaje("ATENCIÓN",
                                  "Se deberán capturar los dos campos","about")
                  LET g_transaccion = ""
                  DISPLAY g_transaccion TO f_transaccion
                  NEXT FIELD f_transaccion
               END IF
               --Valida que la subcuenta no este vacia o nula
               IF LENGTH(v_cmb_sub_cta) = 0 THEN
                  CALL fn_mensaje("ATENCIÓN",
                                  "Se deberán capturar los dos campos","about")
                  NEXT FIELD v_cmb_sub_cta
               END IF

               IF v_bnd_action = 1 THEN 
                  --Valida que el proceso_cod no esté vacío
                  IF LENGTH(v_cmb_proceso_cod) = 0 THEN
                     CALL fn_mensaje("ATENCIÓN",
                                     "Se deberá capturar el código de proceso","about")
                     NEXT FIELD v_cmb_proceso_cod
                  END IF

               END IF 

            END IF
            --Función para ejecutar segun boton de accion
            CALL fn_valida_accion(v_bnd_action) RETURNING r_vtn_action
               INITIALIZE g_arr_transaccion TO NULL
               INITIALIZE g_arr_trans_det TO NULL
               DISPLAY g_transaccion TO f_transaccion
               LET g_transaccion = ""
               
               --Carga datos deacuerdo al codigo selleccionado en el combo
               --Actualiza la consulta
               CALL fn_llena_tran_detalle(v_cmb_proceso)
                                    RETURNING g_arr_transaccion,g_arr_trans_det,
                                              g_arr_trans_all
               CALL ui.Interface.refresh()

               CALL DIALOG.setActionHidden("accept",1)

               IF r_vtn_action = 1 THEN 
                  CALL f_forma.setElementHidden("grp_cons_proc",0)
                  CALL f_forma.setElementHidden("grp_inf_trasn",0)
                  CALL f_forma.setElementHidden("grp_det_trasn",0)
                  
                  NEXT FIELD v_cmb_proceso
                  
               END IF 

               IF r_vtn_action = 0 THEN 
                   CALL DIALOG.setActionHidden("accept",1)
                   CALL DIALOG.setActionHidden("insertar",0)
                   CALL DIALOG.setActionHidden("actualizar",0)
                   CALL DIALOG.setActionHidden("eliminar",0)

                     CALL f_forma.setElementHidden("grp_asg_trasn",1)
                     CALL f_forma.setElementHidden("grp_con_sub_cta",1)
                     CALL f_forma.setElementHidden("grp_proceso_cod",1)
                     NEXT FIELD v_cmb_proceso
               END IF 
               
              
               
         ON ACTION insertar
            --Valida que el indice o registro a modificar sea el de descripción
            --de transaccion y no de informacion de transaccion


            
               LET g_arr_trans_all[arr_curr()].arr_cod_sub_cat = 0
               LET v_bnd_action = 1
               LET g_transaccion = ""
               DISPLAY g_transaccion TO f_transaccion
               CALL f_forma.setElementHidden("grp_asg_trasn",0)
               CALL f_forma.setElementHidden("grp_con_sub_cta",0)
               CALL f_forma.setElementHidden("grp_proceso_cod",0)
               CALL DIALOG.setActionHidden("accept",0)
               CALL DIALOG.setActionHidden("insertar",1)
               CALL DIALOG.setActionHidden("actualizar",1)
               CALL DIALOG.setActionHidden("eliminar",1)

               CALL f_forma.setElementText("f_transaccion","")
               
               --LET g_transaccion = ""
               LET v_cmb_sub_cta = ""
               CALL ui.Interface.refresh()

               CALL fn_llena_combo_proceso_cod(v_cmb_proceso) RETURNING v_cmb_proceso_cod
                     DISPLAY BY NAME v_cmb_proceso_cod

                     CALL ui.Interface.refresh()
               
               
               
               
              
               
                  NEXT FIELD f_transaccion   
            --END IF

         ON ACTION actualizar
            --Valida que el indice o registro a modificar sea el de descripción
            --de transaccion y no de informacion de transaccion

               
               --Valida que se seleccione una transaccion
               IF g_arr_trans_det.getLength() = 0 THEN

                  CALL fn_mensaje("informaction","Debe seleccionar una transacción","Info")
                  
            
               ELSE 
                  LET v_bnd_action = 2
                  CALL f_forma.setElementHidden("grp_asg_trasn",0)
                  CALL f_forma.setElementHidden("grp_con_sub_cta",0)
                  CALL DIALOG.setActionHidden("accept",0)
                  CALL DIALOG.setActionHidden("insertar",1)
                  CALL DIALOG.setActionHidden("actualizar",1)
                  CALL DIALOG.setActionHidden("eliminar",1)
                  
                  CALL fn_combo_subcuenta(g_arr_transaccion[arr_curr()].v_cod_trans_cnt,
                                              g_arr_trans_all[arr_curr()].arr_cod_sub_cat)
                                              RETURNING v_cmb_sub_cta
                                              
                  DISPLAY v_cmb_sub_cta TO v_cmb_sub_cta
                  CALL ui.Interface.refresh()
               END IF 

         ON ACTION eliminar
            --Valida que el indice o registro a modificar sea el de descripción
            --de transaccion y no de informacion de transaccion
            IF g_id_arr = 0 THEN
              CALL fn_mensaje("informaction","Debe seleccionar una transacción","Info")
            ELSE
               LET v_bnd_action = 3
               LET g_transaccion = ""
               DISPLAY g_transaccion TO f_transaccion
               CALL DIALOG.setActionHidden("accept",0)
               CALL DIALOG.setActionHidden("insertar",1)
               CALL DIALOG.setActionHidden("actualizar",1)
               CALL DIALOG.setActionHidden("eliminar",1)
            END IF
            CALL ui.Interface.refresh()
         ON ACTION cancelar
            EXIT DIALOG

      END DIALOG 
   CLOSE WINDOW vtn_cntm02
   
END MAIN
#OBJETIVO: Mostrar la información de las transacciones del proceso
FUNCTION fn_llena_tran_detalle(p_cod_proceso_cnt)
DEFINE p_cod_proceso_cnt LIKE cnt_regla_contable.cod_proceso_cnt,
       v_id_trnas1       INTEGER,
       v_id_trnas3       INTEGER

   LET v_QryTxt = "\n SELECT rc.cod_transaccion_cnt,",
                  "\n        rc.cod_proceso_cnt||'-'||cp.desc_proceso_cnt,",
                  "\n        rc.cod_subcta_cnt||'-'||cs.subcuenta_desc",
                  "\n FROM cnt_regla_contable rc,",
                  "\n      cat_proceso_cnt cp,",
                  "\n      cat_subcuenta cs",
                  "\n WHERE rc.cod_proceso_cnt = ",p_cod_proceso_cnt,
                  "\n   AND rc.cod_proceso_cnt = cp.cod_proceso_cnt",
                  "\n   AND rc.cod_subcta_cnt  = cs.subcuenta",
                  "\n GROUP BY 1,2,3",
                  "\n ORDER BY 1 ASC"
   PREPARE prp_trans_proc FROM v_QryTxt

   LET v_id_trnas1 = 1
   -- Declara el cursor para la consulta de transaccion
   DECLARE cur_trans_proc CURSOR FOR prp_trans_proc
      FOREACH cur_trans_proc INTO g_arr_transaccion[v_id_trnas1].v_cod_trans_cnt,
                                  g_arr_transaccion[v_id_trnas1].v_cod_proceso_cnt,
                                  g_arr_transaccion[v_id_trnas1].v_cod_subcta_cnt

         LET v_id_trnas1 = v_id_trnas1 + 1

      END FOREACH

   LET v_id_trnas3 = 1
   DECLARE cur_regla_contable CURSOR FOR
      SELECT cod_proceso_cnt,cod_transaccion_cnt,cod_subcta_cnt
      FROM cnt_regla_contable
      WHERE cod_proceso_cnt = p_cod_proceso_cnt
      GROUP BY 1,2,3
   FOREACH cur_regla_contable INTO g_arr_trans_all[v_id_trnas3].*
      LET v_id_trnas3 = v_id_trnas3 + 1
   END FOREACH 

   CALL g_arr_transaccion.deleteElement(v_id_trnas1)
   CALL g_arr_trans_all.deleteElement(v_id_trnas3)

   RETURN g_arr_transaccion,g_arr_trans_det,g_arr_trans_all

END FUNCTION
#Objetivo:Ejecuta la acción a realizar segun la opción del usuario
FUNCTION fn_valida_accion(p_bnd_action)
DEFINE p_bnd_action SMALLINT
DEFINE r_vtn_confirma SMALLINT

   DISPLAY "Proceso del combo -",v_cmb_proceso
   --LET r_vtn_confirma = 0
   DISPLAY "p_bnd_action -- ",p_bnd_action
   DISPLAY "r_vtn_confirma -",r_vtn_confirma

   CASE
      WHEN p_bnd_action = 1 --insertar
         --Solicita confirmar(1) o cancel(0) la operación de Registro
         
         CALL fn_ventana_confirma("ATENCIóN", 
                                  "¿Está seguro que desea dar de alta la transacción?",
                                  "quest") RETURNING r_vtn_confirma
         IF r_vtn_confirma = 1 THEN
            --LET g_transaccion = ""
               --LET v_cmb_sub_cta = ""
               CALL ui.Interface.refresh()
            CALL fn_inserta_nueva_transaccion()
            
         END IF
      WHEN p_bnd_action = 2 --actualizar
         CALL fn_actualiza_transaccion()
         
      WHEN p_bnd_action = 3 --eliminar
         LET g_id_arr = g_id_arr + 1
         CALL fn_ventana_confirma("ATENCIóN", 
                                  "¿Está seguro de eliminar la transaccion " ||g_arr_trans_all[g_id_arr].arr_cod_trn_cnt || " ",
                                  "quest") RETURNING r_vtn_confirma
         IF r_vtn_confirma = 1 THEN
            CALL fn_elimina_transaccion()
            LET g_id_arr = 0
         END IF
   END CASE

   DISPLAY "r_vtn_confirma regresado -",r_vtn_confirma
   RETURN r_vtn_confirma

END FUNCTION
#Objetico:Inserta nueva transacción en regla contable y catalogo transacciones
FUNCTION fn_inserta_nueva_transaccion()

 --Sección de variables UI
   DEFINE 
      f_ventana ui.Window, --provee la interfaz para la ventana
      f_forma ui.Form --provee la interfaz para la forma

DEFINE v_cod_transaccion_cnt LIKE cat_transaccion_cnt.cod_transaccion_cnt
DEFINE v_id_cuenta_contable  INTEGER
DEFINE v_cod_proceso         LIKE cnt_proceso.cod_proceso
DEFINE v_ind_proceso_cod      SMALLINT 

   LET f_ventana = ui.Window.getCurrent()
   LET f_forma = f_ventana.getForm()
   DISPLAY v_cmb_proceso

   --Obtione condigo transacción del catalogo de transacciones maximo mas uno
   SELECT MAX(cod_transaccion_cnt) INTO v_cod_transaccion_cnt
   FROM cat_transaccion_cnt
      LET v_cod_transaccion_cnt = v_cod_transaccion_cnt + 1

   INSERT INTO cat_transaccion_cnt VALUES(v_cod_transaccion_cnt,
                                          g_transaccion,
                                          TODAY,
                                          g_usuario)

   --Obtienen id de cuanta contable maximo de regla contable mas uno
   SELECT MAX(id_cuenta_contable) INTO v_id_cuenta_contable
   FROM cnt_regla_contable
   WHERE cod_proceso_cnt = v_cmb_proceso
      LET v_id_cuenta_contable = v_id_cuenta_contable + 1


         LET v_QryTxt = "\n INSERT INTO cnt_regla_contable",
                        "\n     VALUES(",v_id_cuenta_contable,
                        "\n                   ,",v_cmb_proceso,
                        "\n                   ,",v_cod_proceso,
                        "\n                   ,",v_cod_transaccion_cnt,
                        "\n                   ,",v_cmb_sub_cta,
                        "\n                   ,","'","0000000000","'",
                        "\n                   ,",0,
                        "\n                   ,",TODAY,
                        "\n                   ,","'",g_usuario,"'",")"

         DISPLAY "INSERTA >>>>>>>>>>>>> ", v_QryTxt              
         PREPARE prp_inserta_transaccion FROM v_QryTxt
         EXECUTE prp_inserta_transaccion

         CALL f_forma.setElementHidden("grp_cons_proc",0)
         CALL f_forma.setElementHidden("grp_inf_trasn",0)
         CALL f_forma.setElementHidden("grp_det_trasn",0)
         CALL f_forma.setElementHidden("grp_asg_trasn",1)
         CALL f_forma.setElementHidden("grp_con_sub_cta",1)
         CALL f_forma.setElementHidden("grp_proceso_cod",1)
         --NEXT FIELD v_cmb_proceso

END FUNCTION
#Objetivo:Actualiza catalogo de transacciones y regla contable
FUNCTION fn_actualiza_transaccion()


         --Actualiza descripción de la transaccion
         UPDATE cat_transaccion_cnt
            SET desc_transaccion_cnt = g_transaccion,
                f_actualiza          = TODAY,
                usuario              = g_usuario
         WHERE cod_transaccion_cnt  = g_arr_trans_all[g_id_arr].arr_cod_trn_cnt


   
      UPDATE cnt_regla_contable
         SET cod_subcta_cnt = v_cmb_sub_cta,
             f_actualiza    = TODAY,
             usuario        = g_usuario
             
      WHERE cod_transaccion_cnt = g_arr_trans_all[g_id_arr].arr_cod_trn_cnt
        AND cod_proceso_cnt     = g_arr_trans_all[g_id_arr].arr_cod_pro_cnt
        AND cod_subcta_cnt      = g_arr_trans_all[g_id_arr].arr_cod_sub_cat
      CALL fn_mensaje("ATENCIÓN","Registro actualizado","about")
--   END IF

END FUNCTION
#Objetivo:Elimina registro de regla contable
FUNCTION fn_elimina_transaccion()

   DELETE FROM cnt_regla_contable
          WHERE cod_proceso_cnt = g_arr_trans_all[g_id_arr].arr_cod_pro_cnt
            AND cod_transaccion_cnt = g_arr_trans_all[g_id_arr].arr_cod_trn_cnt
            AND cod_subcta_cnt = g_arr_trans_all[g_id_arr].arr_cod_sub_cat

   CALL fn_mensaje("ATENCIÓN","Registro eliminado","about")

END FUNCTION
#Objetivo: Obtiene la descripcion de la transaccion
FUNCTION fn_desc_transaccion(p_cod_transaccion_cnt)
DEFINE p_cod_transaccion_cnt LIKE cat_transaccion_cnt.cod_transaccion_cnt
DEFINE v_id_trnas2           INTEGER

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
