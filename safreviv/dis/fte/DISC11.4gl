################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 21/03/2018                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC09                                                    #
#Objetivo         => Realizar la consulta de saldos mayores o menores a $2     #
#                    que se envían a las interfaces positivas y negativas.     #
#Fecha de Inicio  => 17/09/2012                                                #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose
#Eneas Armas     20140123      Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa

DATABASE safre_viv
GLOBALS
  --Arreglo para la sección de pagos
  DEFINE arr_superiores      DYNAMIC ARRAY OF RECORD 
    f_descripcion_sup        VARCHAR(50),
    f_avance_apo             DECIMAL(22,2),
    --f_avance_apo_aivs        DECIMAL (22,2), --Verificar
    f_avance_amo             DECIMAL(22,2),
    f_apo_amo                DECIMAL(22,2)
  END RECORD   
         
  --Arreglo para la sección de información de inconsistencias
  DEFINE arr_inconsistentes  DYNAMIC ARRAY OF RECORD
    --v_folio_dis              DECIMAL (9,0),
    v_sum_apo_aivs           DECIMAL(22,2), --Verificar            
    v_sum_apo                DECIMAL(22,2),
    v_sum_amo                DECIMAL(22,2),
    v_tot_cuentas            DECIMAL(10,0),
    v_desc_tpo_inconsistente STRING
  END RECORD 

  --Variables para reporte
  DEFINE 
    v_rpt_sum_aiv1           DECIMAL(22,2),
    v_rpt_sum_apo1           DECIMAL(22,2),
    v_rpt_sum_amo1           DECIMAL(22,2),
    v_rpt_tot_cuentas1       DECIMAL(10,0),
    v_rpt_descripcion1       STRING,
    v_rpt_sum_aiv2           DECIMAL(22,2),
    v_rpt_sum_apo2           DECIMAL(22,2),
    v_rpt_sum_amo2           DECIMAL(22,2),
    v_rpt_tot_cuentas2       DECIMAL(10,0),
    v_rpt_descripcion2       STRING

  --Variables para inconsistencia
  DEFINE 
    v_folio_inco             DECIMAL(9,0),
    v_sum_apo                DECIMAL(22,2),
    v_sum_amo                DECIMAL(22,2),
    v_tpo_inconsistente      SMALLINT,
    v_tot_cuentas            DECIMAL(10,0)
      
  --Variables para montos de avances
  DEFINE 
    v_avance_pago_apo        DECIMAL(22,2),
    v_avance_pago_amo        DECIMAL(22,2),
    v_avance_pago_neg_apo    DECIMAL(22,2),
    v_avance_pago_neg_amo    DECIMAL(22,2)

  --Variables para montos virtuales
  DEFINE 
    v_virtual_mayor_apo      DECIMAL(22,2),
    v_virtual_mayor_amo      DECIMAL(22,2),
    v_virtual_menor_apo      DECIMAL(22,2),
    v_virtual_menor_amo      DECIMAL(22,2)

  --Variables para suma de avances
  DEFINE 
    v_sum_avance_pago_mayor_apo DECIMAL(22,2),
    v_sum_avance_pago_mayor_amo DECIMAL(22,2),
    v_sum_avance_pago_menor_apo DECIMAL(22,2),
    v_sum_avance_pago_menor_amo DECIMAL(22,2),
    v_sum_apo                   DECIMAL(22,2),
    v_sum_amo                   DECIMAL(22,2),
    v_total                     DECIMAL(22,2),
    v_total_apo_amo_cargo       DECIMAL(22,2),
    v_total_apo_amo_abono       DECIMAL(22,2),
    v_total_apo_amo             DECIMAL(22,2)

  --Variables para suma de avances
  DEFINE 
    v_sum_virtual_mayor_apo  DECIMAL(22,2),
    v_sum_virtual_mayor_amo  DECIMAL(22,2),
    v_sum_virtual_menor_apo  DECIMAL(22,2),
    v_sum_virtual_menor_amo  DECIMAL(22,2)
     
  --Sección de detalle
  DEFINE 
    f_folio                  DECIMAL(9,0),
    v_cmb_tipo_monto         SMALLINT

  --Sección de variables del programa
  DEFINE 
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT
      
  --Sección de variables UI
  DEFINE 
    f_ventana ui.Window, --provee la interfaz para la ventana
    f_forma ui.Form --provee la interfaz para la forma

  DEFINE 
    v_sumado                 DECIMAL(10,0),
    v_folio_reg_pag          DECIMAL(9,0),
    v_etiqueta               STRING,
    v_edo_compensa_apo       SMALLINT,
    v_edo_compensa_amo       SMALLINT,
    v_etiqueta_precio        STRING

  --Define arreglo para detalle de inconsistentes
  DEFINE arr_detalle_inconsistentes DYNAMIC ARRAY OF RECORD 
    v_nss                    CHAR(11),
    v_nrp                    CHAR(11),
    v_folio_sua              DECIMAL(9,0),
    v_folio_reg_pag          DECIMAL(9,0),
    v_folio_dis              DECIMAL(9,0),
    v_monto_aivs             DECIMAL(22,2),
    v_monto_apo              DECIMAL(22,2),
    v_monto_amo              DECIMAL(22,2)
  END RECORD 

  CONSTANT Por_Folio = 0
  CONSTANT Por_Fecha = 1
  CONSTANT Sin       = 0
  
  DEFINE v_tbl_mov           VARCHAR(50),
         g_sql_txt           STRING,
         v_proc_entra        SMALLINT,
         v_proc_val          SMALLINT,
         v_cod_conv          SMALLINT,
         v_desc_proc_val     CHAR(40),
         v_mensaje_val       STRING,
         p_proceso_cod       SMALLINT
 
END GLOBALS

MAIN 
  LET p_usuario        = ARG_VAL(1) -- Recibe la variable de usuario
  LET p_tipo_proceso   = ARG_VAL(2) -- Recibe el tipo de proceso
  LET p_nom_prog       = ARG_VAL(3) -- Recibe el nombre del programa

  LET p_proceso_cod    = 901

  PREPARE fn_tbl_mov FROM "execute function fn_tab_movimiento(?,?,?)"

  CLOSE WINDOW SCREEN
  
  OPEN WINDOW v_consulta WITH FORM "DISC111"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_folio,v_cmb_tipo_monto
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          --Invocamos la función para asignar el título a la ventana
          CALL ui.Interface.setText(p_nom_prog)

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

          --Oculta las secciones de detalle de pagos
          CALL f_forma.setElementHidden("gr_pago_superior",TRUE) --Oculta la sección de pagos superiores
          CALL f_forma.setElementHidden("gr_inconsistente",TRUE) --Oculta la sección de pagos anteriores
          CALL f_forma.setElementHidden("gr_info_reg_pag",TRUE) --Oculta la sección de información de registro de pagos
          CALL f_forma.setElementHidden("gr_ceros",TRUE) --Oculta la sección de información de inconsistentes
          CALL f_forma.setElementHidden("gr_info_precio",TRUE) --Oculta la sección de información del precio del fondo al día

        ON ACTION cancelar 
           EXIT DIALOG

        ON ACTION aceptar
           IF f_folio          IS NULL AND 
              v_cmb_tipo_monto IS NULL THEN 
              CALL fn_mensaje("Error", "Debe capturar ambos criterios para realizar la búsqueda.", "information")
              NEXT FIELD f_folio
           END IF 

           IF f_folio IS NULL THEN 
              CALL fn_mensaje("Error", "Debe capturar ambos criterios para realizar la búsqueda.", "information")
              NEXT FIELD f_folio
           END IF

           IF v_cmb_tipo_monto IS NULL THEN 
              CALL fn_mensaje("Error", "Debe capturar ambos criterios para realizar la búsqueda.", "information")
              NEXT FIELD f_folio
           END IF

           IF f_folio          IS NOT NULL AND 
              v_cmb_tipo_monto IS NOT NULL THEN
              --Obtiene folio de registro de pagos
              SELECT folio_referencia
              INTO   v_folio_reg_pag 
              FROM   glo_folio
              WHERE  folio = f_folio

              IF v_folio_reg_pag IS NULL THEN 
                 CALL fn_mensaje("Información", "No existe folio de registro de pagos para este folio introducido. Verifique", "information")
                 NEXT FIELD f_folio
              ELSE 
                 --Muestra grupos en la forma
                 CALL f_forma.setElementHidden("gr_pago_superior",FALSE) --Muestra la sección de pagos superiores
                 CALL f_forma.setElementHidden("gr_inconsistente",FALSE) --Muestra la sección de pagos anteriores
                 CALL f_forma.setElementHidden("gr_info_reg_pag",FALSE) --Muestra la sección de información de re
                 CALL f_forma.setElementHidden("gr_info_precio",FALSE) --Oculta la sección de información del precio del fondo al día
              END IF 

              --Llama a función para realizar la consulta
              CALL fn_consulta_saldos_interfaces()
               
           ELSE
              CALL fn_mensaje("Error", "Debe capturar ambos criterios para realizar la búsqueda.", "information")
              NEXT FIELD f_folio
           END IF

           DISPLAY ARRAY arr_superiores TO src_superiores.* 
           ATTRIBUTES (ACCEPT = FALSE , CANCEL = FALSE )
           
             BEFORE DISPLAY 
               --Muestra folio de registro de pagos
               IF v_folio_reg_pag IS NULL THEN 
                  LET v_etiqueta = "No existe Folio de Registro de Pagos asociado."
               ELSE 
                  LET v_etiqueta = "Folio de Registro de Pagos:",v_folio_reg_pag
                  CALL fn_obtiene_precio_valor_fondo()
               END IF 
                  
               CALL f_forma.setElementText("lbl_reg_pag",v_etiqueta)
               CALL f_forma.setElementText("lbl_precio",v_etiqueta_precio)
                  
               CALL fn_consulta_informacion_inconsistente()

               CALL fn_detalle_inconsistentes()

               DISPLAY ARRAY arr_inconsistentes TO src_inconsistente.* 
               ATTRIBUTES (ACCEPT = FALSE , CANCEL = FALSE )

                 BEFORE DISPLAY 
                   CALL f_forma.setElementHidden("gr_ceros",FALSE) --Oculta la sección de información de inconsistentes
   
                   DISPLAY ARRAY arr_detalle_inconsistentes TO src_inconsistentes_ceros.* 
                   ATTRIBUTES (CANCEL = FALSE, ACCEPT = FALSE )

                     --BEFORE DISPLAY 
                         --IF NOT INT_FLAG THEN 
                              -- ACCEPT DISPLAY 
                         --END IF
                         
                     ON ACTION cancelar 
                        EXIT PROGRAM 

                     ON ACTION reporte
                        CALL fn_genera_reporte_saldos()
                                 
                   END DISPLAY
               END DISPLAY 
           END DISPLAY 
      END INPUT 
    END DIALOG 
  CLOSE WINDOW v_consulta

END MAIN 

--Objetivo: Consulta saldos de avance de pagos
FUNCTION fn_consulta_saldos_interfaces()
  DEFINE 
    v_query_positivas        STRING,
    v_query_negativas        STRING,
    v_indice                 SMALLINT,
    v_final                  SMALLINT 

  LET v_indice = 1
  LET v_final  = 2 --Total de filas a recorrer

  --Avance
  LET v_avance_pago_apo     = 0.00
  LET v_avance_pago_amo     = 0.00
  LET v_avance_pago_neg_apo = 0.00
  LET v_avance_pago_neg_amo = 0.00

  LET v_sum_avance_pago_mayor_apo = 0.00
  LET v_sum_avance_pago_mayor_amo = 0.00
  LET v_sum_avance_pago_menor_apo = 0.00
  LET v_sum_avance_pago_menor_amo = 0.00

  {--Virtuales
  LET v_virtual_mayor_apo = 0.00
  LET v_virtual_mayor_amo = 0.00
  LET v_virtual_menor_apo = 0.00
  LET v_virtual_menor_amo = 0.00

  LET v_sum_virtual_mayor_apo = 0.00
  LET v_sum_virtual_mayor_amo = 0.00
  LET v_sum_virtual_menor_apo = 0.00
  LET v_sum_virtual_menor_amo = 0.00}

  --Asigna descripciones
  LET arr_superiores[1].f_descripcion_sup = "Cargo (Positivas)"
  LET arr_superiores[2].f_descripcion_sup = "Abono (Negativas)"
  LET arr_superiores[3].f_descripcion_sup = "Sumatorias"
  LET arr_superiores[4].f_descripcion_sup = "Total"

  {LET arr_anteriores[1].f_descripcion_ant = "Positivas"
  LET arr_anteriores[2].f_descripcion_ant = "Negativas"
  LET arr_anteriores[3].f_descripcion_ant = "Sumatorias"
  LET arr_anteriores[4].f_descripcion_ant = "Total"}


  # Arma el query de las diferencias positivas
  LET v_query_positivas = "\n SELECT av.monto_dif_apo, ",
                          "\n        av.monto_dif_amo, ",
                          "\n        edo_compensa_apo, ",
                          "\n        edo_compensa_amo  ",
                          --"\n SELECT av.aiv_ap_pat, av.monto_dif_amo, edo_compensa_apo, edo_compensa_amo",
                          "\n FROM   dis_compensa_avance di, ",
                          "\n        afi_derechohabiente af, ",
                          "\n        dis_det_avance_pago av",
                          "\n WHERE  folio_dis                 = ",f_folio,
                          "\n AND    af.id_derechohabiente     = di.id_derechohabiente ",
                          "\n AND    av.id_dis_det_avance_pago = di.id_dis_det_avance_pago ",
                          "\n AND    ((di.edo_compensa_apo IN (1)) OR ",
                          "\n         (di.edo_compensa_amo IN (1)))"
                                    
  --## Cambiar esto por las AIVS
  IF v_cmb_tipo_monto = 1 THEN --Si se elige visualizar los montos mayores a $2
     LET v_query_positivas = v_query_positivas,
                             "\n AND (av.monto_dif_apo >2 OR av.monto_dif_amo >2)"
  ELSE  --Si se elige visualizar los montos menores a $2
     LET v_query_positivas = v_query_positivas,
                             "\n AND (av.monto_dif_apo <=2 OR av.monto_dif_amo <=2)"
  END IF 
   
  DISPLAY v_query_positivas

  PREPARE prp_diferencias_positivas FROM v_query_positivas
  DECLARE cur_dif_positivas CURSOR FOR prp_diferencias_positivas
  FOREACH cur_dif_positivas INTO v_avance_pago_apo, v_avance_pago_amo,
                                 v_edo_compensa_apo, v_edo_compensa_amo
      
    IF v_cmb_tipo_monto = 1 THEN --si se filtran las mayores  
       --Si el monto es mayor a $2 lo suma al total
       IF v_avance_pago_apo > 2 AND v_edo_compensa_apo = 1 THEN 
          LET v_sum_avance_pago_mayor_apo = v_sum_avance_pago_mayor_apo + v_avance_pago_apo
       END IF

       IF v_avance_pago_amo > 2 AND v_edo_compensa_amo = 1 THEN 
          LET v_sum_avance_pago_mayor_amo = v_sum_avance_pago_mayor_amo + v_avance_pago_amo
       END IF 
    ELSE --Si se filtran las menores
         --Si el monto es menor o igual a $2 lo suma al total
       IF v_avance_pago_apo <= 2 AND v_edo_compensa_apo = 1 THEN 
          LET v_sum_avance_pago_mayor_apo = v_sum_avance_pago_mayor_apo + v_avance_pago_apo
       END IF

       IF v_avance_pago_amo <= 2 AND v_edo_compensa_amo = 1 THEN 
          LET v_sum_avance_pago_mayor_amo = v_sum_avance_pago_mayor_amo + v_avance_pago_amo
       END IF 
    END IF
  END FOREACH 

  # Arma el query de las diferencias negativas
  LET v_query_negativas = "\n SELECT av.monto_dif_apo, ",
                          "\n        av.monto_dif_amo, ",
                          "\n        edo_compensa_apo, ",
                          "\n        edo_compensa_amo  ",
                          --"\n SELECT av.aiv_ap_pat, av.monto_dif_amo, edo_compensa_apo, edo_compensa_amo",
                          "\n FROM   dis_compensa_avance di, ",
                          "\n        afi_derechohabiente af, ",
                          "\n        dis_det_avance_pago av",
                          "\n WHERE  folio_dis                 = ",f_folio,
                          "\n AND    af.id_derechohabiente     = di.id_derechohabiente ",
                          "\n AND    av.id_dis_det_avance_pago = di.id_dis_det_avance_pago ",
                          "\n AND    ((di.edo_compensa_apo IN (2)) ",
                          "\n OR      (di.edo_compensa_amo IN (2)))"
                                                                        
  IF v_cmb_tipo_monto = 1 THEN --Si se elige visualizar los montos mayores a $2
     LET v_query_negativas = v_query_negativas,
                             "\n AND (av.monto_dif_apo >2 OR av.monto_dif_amo >2 OR av.monto_dif_apo <-2 OR av.monto_dif_amo <-2 )"
  ELSE  --Si se elige visualizar los montos menores a $2
    LET v_query_negativas = v_query_negativas,
                            "\n AND ((av.monto_dif_apo <=2 OR av.monto_dif_amo <=2) AND (av.monto_dif_apo >=-2 OR av.monto_dif_amo >=-2))"
  END IF 
   
  DISPLAY v_query_negativas
  
  PREPARE prp_diferencias_negativas FROM v_query_negativas
  DECLARE cur_dif_negativas CURSOR FOR prp_diferencias_negativas
  FOREACH cur_dif_negativas INTO v_avance_pago_neg_apo, v_avance_pago_neg_amo,
                                 v_edo_compensa_apo, v_edo_compensa_amo

    --LET v_avance_pago_neg_apo = v_avance_pago_neg_apo * -1
    --LET v_avance_pago_neg_amo = v_avance_pago_neg_amo * -1
      
    IF v_cmb_tipo_monto = 1 THEN --si se filtran las mayores  
       --Si el monto es mayor a $2 lo suma al total
       IF (v_avance_pago_neg_apo > 2 OR v_avance_pago_neg_apo < -2) AND v_edo_compensa_apo = 2 THEN 
          LET v_sum_avance_pago_menor_apo = v_sum_avance_pago_menor_apo + v_avance_pago_neg_apo
       END IF

       IF (v_avance_pago_neg_amo > 2 OR v_avance_pago_neg_amo < -2) AND v_edo_compensa_amo = 2 THEN 
          LET v_sum_avance_pago_menor_amo = v_sum_avance_pago_menor_amo + v_avance_pago_neg_amo
       END IF 
    ELSE --Si se filtran las menores
         --Si el monto es menor o igual a $2 lo suma al total
       IF (v_avance_pago_neg_apo <= 2 AND v_avance_pago_neg_apo >= -2) AND v_edo_compensa_apo = 2 THEN 
          LET v_sum_avance_pago_menor_apo = v_sum_avance_pago_menor_apo + v_avance_pago_neg_apo
       END IF

       IF (v_avance_pago_neg_amo <= 2 AND v_avance_pago_neg_amo >= -2) AND v_edo_compensa_amo = 2 THEN 
          LET v_sum_avance_pago_menor_amo = v_sum_avance_pago_menor_amo + v_avance_pago_neg_amo
       END IF 
    END IF
   END FOREACH 

   ############################################# 

   --Asigna Totales acumulados posteriores
   LET arr_superiores[1].f_avance_apo = v_sum_avance_pago_mayor_apo
   LET arr_superiores[1].f_avance_amo = v_sum_avance_pago_mayor_amo
   LET arr_superiores[2].f_avance_apo = v_sum_avance_pago_menor_apo
   LET arr_superiores[2].f_avance_amo = v_sum_avance_pago_menor_amo

   {--Asigna Totales acumulados anteriores a 2005
   LET arr_anteriores[1].f_virtual_apo = v_sum_virtual_mayor_apo
   LET arr_anteriores[1].f_virtual_amo = v_sum_virtual_mayor_amo
   LET arr_anteriores[2].f_virtual_apo = v_sum_virtual_menor_apo
   LET arr_anteriores[2].f_virtual_amo = v_sum_virtual_menor_amo}

   --Valida que no existan nulos
   IF arr_superiores[1].f_avance_apo IS NULL THEN LET arr_superiores[1].f_avance_apo = 0.00 END IF
   IF arr_superiores[1].f_avance_amo IS NULL THEN LET arr_superiores[1].f_avance_amo = 0.00 END IF  
   IF arr_superiores[2].f_avance_apo IS NULL THEN LET arr_superiores[2].f_avance_apo = 0.00 END IF
   IF arr_superiores[2].f_avance_amo IS NULL THEN LET arr_superiores[2].f_avance_amo = 0.00 END IF

   --Obtiene valor absoluto
   IF arr_superiores[1].f_avance_apo <0 THEN LET arr_superiores[1].f_avance_apo = arr_superiores[1].f_avance_apo * -1 END IF
   IF arr_superiores[1].f_avance_amo <0 THEN LET arr_superiores[1].f_avance_amo = arr_superiores[1].f_avance_amo * -1 END IF  
   IF arr_superiores[2].f_avance_apo <0 THEN LET arr_superiores[2].f_avance_apo = arr_superiores[2].f_avance_apo * -1 END IF
   IF arr_superiores[2].f_avance_amo <0 THEN LET arr_superiores[2].f_avance_amo = arr_superiores[2].f_avance_amo * -1 END IF
   
   --Realiza sumatorias
   LET arr_superiores[3].f_avance_apo = arr_superiores[1].f_avance_apo + arr_superiores[2].f_avance_apo
   LET arr_superiores[3].f_avance_amo = arr_superiores[1].f_avance_amo + arr_superiores[2].f_avance_amo

   --LET arr_anteriores[3].f_virtual_apo = arr_anteriores[1].f_virtual_apo + arr_anteriores[2].f_virtual_apo
   --LET arr_anteriores[3].f_virtual_amo = arr_anteriores[1].f_virtual_amo + arr_anteriores[2].f_virtual_amo

   --Obtiene totales
   LET arr_superiores[4].f_avance_amo = arr_superiores[3].f_avance_apo + arr_superiores[3].f_avance_amo
   --LET arr_anteriores[4].f_virtual_amo = arr_anteriores[3].f_virtual_apo + arr_anteriores[3].f_virtual_amo

   
   --Asigna valores a variables
   --Asigna Totales acumulados posteriores a 2005
   LET v_sum_avance_pago_mayor_apo = arr_superiores[1].f_avance_apo
   LET v_sum_avance_pago_mayor_amo = arr_superiores[1].f_avance_amo 
   LET v_sum_avance_pago_menor_apo = arr_superiores[2].f_avance_apo 
   LET v_sum_avance_pago_menor_amo = arr_superiores[2].f_avance_amo 

   LET v_sum_apo = arr_superiores[3].f_avance_apo
   LET v_sum_amo = arr_superiores[3].f_avance_amo
   LET v_total   = arr_superiores[4].f_avance_amo

   --Asigna totales apo + amo
   LET arr_superiores[1].f_apo_amo = arr_superiores[1].f_avance_apo + arr_superiores[1].f_avance_amo
   LET arr_superiores[2].f_apo_amo = arr_superiores[2].f_avance_apo + arr_superiores[2].f_avance_amo
   LET arr_superiores[3].f_apo_amo = arr_superiores[3].f_avance_apo + arr_superiores[3].f_avance_amo
   
   LET v_total_apo_amo_cargo = arr_superiores[1].f_apo_amo
   LET v_total_apo_amo_abono = arr_superiores[2].f_apo_amo
   LET v_total_apo_amo       = arr_superiores[3].f_apo_amo
   
END FUNCTION 

# Función para obtener la información de las cuentas inconsistentes
FUNCTION fn_consulta_informacion_inconsistente()
  DEFINE
    v_query_inc              STRING,
    v_ind_inc                INTEGER 

  LET v_ind_inc = 1

  --Consulta información de las cuentas inconsistentes del folio
  LET v_query_inc = "\nSELECT SUM(aiv_ap_pat), ",
                    "\n       SUM(imp_ap_pat), ",
                    "\n       SUM(imp_am_cre), ",
                    "\n       COUNT(*), ",
                    "\n       tpo_inconsistente ",
                    "\nFROM   dis_info_inconsistente",
                    "\nWHERE  folio_liquida = ",f_folio,
                    "\nGROUP BY 5"

  DISPLAY v_query_inc
  
  PREPARE prp_info_inconsistente FROM v_query_inc
  DECLARE cur_info_inconsistente CURSOR FOR prp_info_inconsistente  
  FOREACH cur_info_inconsistente INTO arr_inconsistentes[v_ind_inc].v_sum_apo_aivs,
                                      arr_inconsistentes[v_ind_inc].v_sum_apo,
                                      arr_inconsistentes[v_ind_inc].v_sum_amo,
                                      arr_inconsistentes[v_ind_inc].v_tot_cuentas,
                                      v_tpo_inconsistente

    --Verifica el tipo de inconsistencia
    IF v_tpo_inconsistente = 0 THEN 
       LET arr_inconsistentes[v_ind_inc].v_desc_tpo_inconsistente = "0-Sin Número de Crédito"
    END IF 
    IF v_tpo_inconsistente = 1 THEN 
       LET arr_inconsistentes[v_ind_inc].v_desc_tpo_inconsistente = "1-Destino aportación Infonavit a Crédito 43 – Bis"
    END IF
    IF v_tpo_inconsistente = 2 THEN 
       LET arr_inconsistentes[v_ind_inc].v_desc_tpo_inconsistente = "2-Aclaratorio sin Destino"
    END IF
    IF v_tpo_inconsistente = 3 THEN 
       LET arr_inconsistentes[v_ind_inc].v_desc_tpo_inconsistente = "3-Aclaratorio Afore sin Marca Confirmada"
    END IF

    LET v_ind_inc = v_ind_inc + 1
  END FOREACH 

  --Borra el elemento vacío del arreglo
  CALL arr_inconsistentes.deleteElement(v_ind_inc)
   
  --Asigna variables para reporte
  LET v_rpt_tot_cuentas1 = arr_inconsistentes[1].v_tot_cuentas
  LET v_rpt_sum_aiv1     = arr_inconsistentes[1].v_sum_apo_aivs
  LET v_rpt_sum_apo1     = arr_inconsistentes[1].v_sum_apo
  LET v_rpt_sum_amo1     = arr_inconsistentes[1].v_sum_amo
  LET v_rpt_descripcion1 = arr_inconsistentes[1].v_desc_tpo_inconsistente
  LET v_rpt_tot_cuentas2 = arr_inconsistentes[2].v_tot_cuentas
  LET v_rpt_sum_aiv2     = arr_inconsistentes[2].v_sum_apo_aivs
  LET v_rpt_sum_apo2     = arr_inconsistentes[2].v_sum_apo
  LET v_rpt_sum_amo2     = arr_inconsistentes[2].v_sum_amo
  LET v_rpt_descripcion2 = arr_inconsistentes[2].v_desc_tpo_inconsistente

END FUNCTION 

--Obtiene el detalle de los registros inconsistentes con número de crédito en ceros
FUNCTION fn_detalle_inconsistentes()
  DEFINE 
    v_query_detalle          STRING,
    v_ind_detalle            INTEGER --[indice de recorrido]

  LET v_query_detalle = "\n SELECT af.nss, ",
                        "\n        pag.nrp, ",
                        "\n        pag.folio_sua, ",
                        "\n        fo.folio_referencia, ",
                        "\n        dis.folio_liquida, ",
                        "\n        dis.aiv_ap_pat, ",
                        "\n        dis.imp_ap_pat, ",
                        "\n        dis.imp_am_cre",
                        "\n FROM   cta_his_pagos pag, ",
                        "\n        afi_derechohabiente af, ",
                        "\n        dis_info_inconsistente dis, ",
                        "\n        glo_folio fo",
                        "\n WHERE  dis.folio_liquida      = ", f_folio,
                        "\n AND    dis.id_derechohabiente = af.id_derechohabiente",
                        "\n AND    dis.id_referencia      = pag.id_referencia",
                        "\n AND    dis.folio_liquida      = fo.folio",
                        "\n AND    fo.folio_referencia    = pag.folio",
                        "\n AND    dis.tpo_inconsistente  = 0"

  --arr_detalle_inconsistentes
  LET v_ind_detalle = 1

  DISPLAY v_query_detalle
   
  PREPARE prp_detalle_ceros FROM v_query_detalle
  DECLARE cur_detalle_ceros CURSOR FOR prp_detalle_ceros
  FOREACH cur_detalle_ceros INTO arr_detalle_inconsistentes[v_ind_detalle].*
    LET v_ind_detalle = v_ind_detalle + 1
  END FOREACH 

  --Borra el elemento vacío del arreglo
  CALL arr_detalle_inconsistentes.deleteElement(v_ind_detalle)

END FUNCTION 

--Obtiene el precio del valor del fondo el día que se liquidó
FUNCTION fn_obtiene_precio_valor_fondo()
  DEFINE 
    v_precio_fondo           LIKE glo_valor_fondo.precio_fondo,
    v_fecha_precio           DATE
    ,v_query_detalle         STRING

   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio,Sin INTO v_tbl_mov --por folio

  --Obtiene fecha de liquidación
  LET v_query_detalle =  "\n   SELECT DISTINCT f_liquida "
                        ,"\n   FROM   ",v_tbl_mov
                        ,"\n   WHERE  folio_liquida = ?"
  PREPARE prp_mov_a FROM v_query_detalle
  EXECUTE prp_mov_a USING f_folio INTO v_fecha_precio

   IF v_fecha_precio IS NULL OR v_fecha_precio = '12-31-1899' THEN 

      SELECT DISTINCT f_liquida 
      INTO v_fecha_precio
      FROM  dis_preliquida 
      WHERE  folio_liquida = f_folio
   
   END IF 

  SELECT precio_fondo
  INTO   v_precio_fondo
  FROM   glo_valor_fondo
  WHERE  f_valuacion = v_fecha_precio
  AND    fondo       = 11

  IF v_fecha_precio IS NOT NULL THEN 
     LET v_etiqueta_precio = "El precio de fondo es ",v_precio_fondo USING  "######.&&&&&&", " al día ", v_fecha_precio USING "dd-mm-yyyy" 
  END IF 

END FUNCTION 

--Función para realizar el reporte de los saldos de la consulta
FUNCTION fn_genera_reporte_saldos()
  DEFINE
    v_manejador_rpt          om.SaxDocumentHandler,
    v_indice_saldo           INTEGER 

  IF fgl_report_loadCurrentSettings("DISC111.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET v_manejador_rpt = fgl_report_commitCurrentSettings()

     

     START REPORT rpt_consulta_saldos_interfaces TO XML HANDLER v_manejador_rpt


       IF arr_detalle_inconsistentes.getLength() = 0 THEN 
         FOR v_indice_saldo = 1 TO 1
           OUTPUT TO REPORT rpt_consulta_saldos_interfaces(
                                                           arr_detalle_inconsistentes[v_indice_saldo].*
                                                          )
         END FOR 
       ELSE 
         FOR v_indice_saldo = 1 TO arr_detalle_inconsistentes.getLength()
           OUTPUT TO REPORT rpt_consulta_saldos_interfaces(
                                                           arr_detalle_inconsistentes[v_indice_saldo].*
                                                          )
       END FOR 
       END IF 
     
       
     FINISH REPORT rpt_consulta_saldos_interfaces
  ELSE
     CALL fn_mensaje("Error", "No se puede generar el reporte. Verifique la ruta", "information")
  END IF 
      
END FUNCTION 

--Definición del reporte de saldos del avance
REPORT rpt_consulta_saldos_interfaces(p_arr_detalle_inconsistentes)
  --Arreglo para la sección de información de inconsistencias
  DEFINE p_arr_inconsistentes RECORD
    v_sum_apo_aivs            DECIMAL(22,2),
    v_sum_apo                 DECIMAL(22,2),
    v_sum_amo                 DECIMAL(22,2),
    v_tot_cuentas             DECIMAL(10,0),
    v_desc_tpo_inconsistente  STRING
  END RECORD

  DEFINE p_arr_detalle_inconsistentes  RECORD 
    v_nss                    CHAR(11),
    v_nrp                    CHAR(11),
    v_folio_sua              DECIMAL(9,0),
    v_folio_reg_pag          DECIMAL(9,0),
    v_folio_dis              DECIMAL(9,0),
    v_monto_aivs             DECIMAL(22,2),
    v_monto_apo              DECIMAL(22,2),
    v_monto_amo              DECIMAL(22,2)
  END RECORD 
         
  DEFINE
    v_fecha_reporte          DATE 
         
  FORMAT 
    FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      
      PRINTX v_fecha_reporte USING "yyyy-mm-dd"
      PRINTX p_usuario
      PRINTX f_folio
      PRINTX v_folio_reg_pag
      PRINTX v_etiqueta_precio
      PRINTX v_cmb_tipo_monto
      PRINTX v_sum_avance_pago_mayor_apo     
      PRINTX v_sum_avance_pago_mayor_amo     
      PRINTX v_sum_avance_pago_menor_apo     
      PRINTX v_sum_avance_pago_menor_amo     
      PRINTX v_sum_apo                       
      PRINTX v_sum_amo                       
      PRINTX v_total    
      PRINTX v_rpt_tot_cuentas1
      PRINTX v_rpt_sum_aiv1
      PRINTX v_rpt_sum_apo1
      PRINTX v_rpt_sum_amo1 
      PRINTX v_rpt_descripcion1
      PRINTX v_rpt_tot_cuentas2
      PRINTX v_rpt_sum_aiv2
      PRINTX v_rpt_sum_apo2
      PRINTX v_rpt_sum_amo2 
      PRINTX v_rpt_descripcion2
      PRINTX v_total_apo_amo_cargo
      PRINTX v_total_apo_amo_abono
      PRINTX v_total_apo_amo
    
    ON EVERY ROW
      PRINTX p_arr_detalle_inconsistentes.*
         
END REPORT
