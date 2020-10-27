################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 17/08/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTC06                                                   #
#Objetivo          => Consulta y generación del reporte del deudor de          #
#                     Transferencia de acreditados
#Fecha inicio      => 29/06/2012                                               #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
DATABASE "safre_viv"

GLOBALS "CNTG01.4gl"
GLOBALS 
  {DEFINE g_arr_cat_sctas     DYNAMIC ARRAY OF RECORD   --Arreglo para almacenar el folio
    v_cve_cod_subcta_cnt     LIKE cnt_regla_contable.cod_subcta_cnt,
    v_desc_sctas             CHAR(45)--LIKE cat_proceso_cnt.desc_proceso_cnt
  END RECORD

  DEFINE 
    v_ind_sctas              INTEGER
    
  DEFINE 
    v_monto_scta_sindif      DECIMAL(22,2),
    v_monto_scta_mayor       DECIMAL(22,2),
    v_monto_scta_menor       DECIMAL(22,2),
    v_monto_deudor_sindif    DECIMAL(22,2),
    v_monto_deudor_mayor     DECIMAL(22,2),
    v_monto_deudor_menor     DECIMAL(22,2),
    v_monto_dif_sindif       DECIMAL(22,2),      
    v_monto_dif_mayor        DECIMAL(22,2),
    v_monto_dif_menor        DECIMAL(22,2),
    v_sumatoria_scta         DECIMAL(22,2), 
    v_sumatoria_deudor       DECIMAL(22,2),
    v_sumatoria_dif          DECIMAL(22,2)}

  --Arreglo para almacenar los valores de Deudor transferencia de acreditados
  DEFINE arr_acreditados     DYNAMIC ARRAY OF RECORD 
    v_nada                   VARCHAR(20),
    v_deudor                 DECIMAL(22,2),
    v_monto92                DECIMAL(22,2),
    v_monto97                DECIMAL(22,2),
    v_montoSI                DECIMAL(22,2),
    v_diferencia             DECIMAL(22,2)
  END RECORD 

  --Arreglo para almacenar los valores totales de Deudor transferencia de acreditados
  DEFINE arr_tot_acreditados DYNAMIC ARRAY OF RECORD 
    v_nada                   VARCHAR(10),
    v_tot_deudor             DECIMAL(22,2),
    v_tot_monto92            DECIMAL(22,2),
    v_tot_monto97            DECIMAL(22,2),
    v_tot_montoSI            DECIMAL(22,2),
    v_tot_diferencia         DECIMAL(22,2)
  END RECORD

  DEFINE
    v_monto                  DECIMAL(22,2)

   DEFINE 
      v_folio_liquidacion      DECIMAL(9,0),
      f_fecha_ini              DATE,
      f_fecha_fin              DATE
   
   DEFINE i      INT
      ,v_monto_g DECIMAL(22,2)
   DEFINE arr_tbl_mov DYNAMIC ARRAY OF VARCHAR(50)
END GLOBALS 

MAIN 
  DEFINE
    
    f_tipo_trabajador        CHAR(1)
               
  --Sección de variables UI
  DEFINE 
    f_ventana ui.Window, --provee la interfaz para la ventana
    f_forma ui.Form      --provee la interfaz para la forma

  LET f_tipo_trabajador = ""
      
  LET g_usuario         = ARG_VAL(1) -- Recibe la variable de usuario
  LET g_tipo_proceso    = ARG_VAL(2) -- Recibe el tipo de proceso
  LET g_nom_prog        = ARG_VAL(3) -- Recibe el nombre del programa

      
  --Se invoca la función que asigna el titulo a la ventana
  CALL ui.Interface.setText(g_nom_prog)


   LET i=1
   DECLARE cur_tbl_mov CURSOR FOR SELECT tabla from cat_tab_movimiento
   FOREACH cur_tbl_mov INTO arr_tbl_mov[i]
      LET i=i+1
   END FOREACH
   LET arr_tbl_mov[i] = "cta_movimiento"

   CLOSE WINDOW SCREEN
  
  OPEN WINDOW vtn_CNTC06 WITH FORM "CNTC061"
    DIALOG ATTRIBUTES(UNBUFFERED)
   
    INPUT BY NAME v_folio_liquidacion, f_fecha_ini, f_fecha_fin 
    
      BEFORE INPUT

        LET f_ventana = ui.Window.getCurrent()
        LET f_forma   = f_ventana.getForm()
        CALL ui.Interface.refresh()
        --CALL fn_cmb_subcta(" IN (4,8,42,44) ")
        CALL arr_tot_acreditados.clear()
        CALL arr_acreditados.clear()

        --CALL DIALOG.setActionHidden("reporte",TRUE) --Oculta el botón de Reporte
        --CALL f_forma.setElementText("Table3.t_nada[1]","Deudor")
        
    END INPUT 

    --Acciones al presionar el botón CANCELAR
    ON ACTION cancelar
       EXIT DIALOG
         
    --Acciones al presionar el botón CONSULTAR
    ON ACTION consultar      
       --Valida que capture un folio de liquidación
       IF (v_folio_liquidacion IS NULL) AND (f_fecha_ini IS NULL) AND (f_fecha_fin IS NULL)THEN 
          CALL fn_mensaje("Error", "Debe capturar al menos un criterio de búsqueda.", "information")
          NEXT FIELD v_folio_liquidacion
       END IF 

       --Valida que se ingresen ambas fechas del periodo
       IF (f_fecha_ini IS NOT NULL) AND (f_fecha_fin IS NULL) THEN 
          CALL fn_mensaje("Error", "Debe seleccionar una fecha final.", "information")
          NEXT FIELD f_fecha_fin
       END IF 

       --Valida que se ingresen ambas fechas del periodo
       IF (f_fecha_ini IS NULL) AND (f_fecha_fin IS NOT NULL) THEN 
          CALL fn_mensaje("Error", "Debe seleccionar una fecha inicial.", "information")
          NEXT FIELD f_fecha_ini
       END IF 

       --Valida que la fecha inicial no sea mayor a la final
       IF f_fecha_ini > f_fecha_fin THEN 
          CALL fn_mensaje("Error", "La fecha inicial no debe ser mayor a la fecha final.", "information")
          NEXT FIELD f_fecha_ini
       END IF 

       --Valida que la fecha final no sea mayor a la actual
       IF f_fecha_fin > TODAY THEN 
          CALL fn_mensaje("Error", "La fecha final no debe ser mayor a la fecha de hoy.", "information")
          NEXT FIELD f_fecha_fin
       END IF 

       --Si la subcuenta es 4 (VIVIENDA 97) u 8 (VIVIENDA 92), asignar como Tipo de Trabajador la clave I
       {IF v_cmb_sub_cta = 4 OR v_cmb_sub_cta = 8 THEN
          LET f_tipo_trabajador = "I"
       END IF 

       --Si la subcuenta es 42 (VIVIENDA 92 SOLO INFONAVIT) ó 44 (VIVIENDA 97 SOLO INFONAVIT), asignar como Tipo de Trabajador la clave S
       IF v_cmb_sub_cta = 42 OR v_cmb_sub_cta = 44 THEN
          LET f_tipo_trabajador = "S"
       END IF}

       DISPLAY "v_folio_liquidacion ",v_folio_liquidacion
       DISPLAY "f_fecha_ini ",f_fecha_ini
       DISPLAY "f_fecha_fin ",f_fecha_fin
       DISPLAY "f_tipo_trabajador ",f_tipo_trabajador

       CALL arr_acreditados.clear()
       CALL arr_tot_acreditados.clear()
       CALL fn_obtiene_montos(v_folio_liquidacion, f_fecha_ini, f_fecha_fin, f_tipo_trabajador)

       DISPLAY ARRAY arr_acreditados TO src_acre_2.* 
       ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE  )
         BEFORE DISPLAY 
           --CALL DIALOG.setActionHidden("reporte",TRUE) --Oculta el botón de Reporte
           DISPLAY ARRAY arr_tot_acreditados TO src_tot_acre.* 
           ATTRIBUTES (ACCEPT = FALSE )

             ON ACTION CANCEL
                CALL arr_tot_acreditados.clear()
                CALL arr_acreditados.clear()
                CALL ui.Interface.refresh()
                EXIT DISPLAY 
                EXIT DIALOG 
                     
             --Acciones al presionar el botón Reporte
             ON ACTION reporte
                --CALL fn_mensaje("ATENCION", "Se ejecutará el reporte", "about")
                CALL fn_genera_reporte_transferencia(f_fecha_ini, f_fecha_fin, f_tipo_trabajador)

             ON ACTION archivo
                CALL fn_genera_archivo_transferencia() 
           END DISPLAY

           EXIT DISPLAY 
       END DISPLAY 
   
    END DIALOG 
  CLOSE WINDOW vtn_CNTC06 
END MAIN 

#Función para llenar tablas de montos
FUNCTION fn_obtiene_montos(p_folio_liquidacion, p_fecha_ini, p_fecha_fin, p_tipo_trabajador)
  DEFINE 
    v_index                  SMALLINT, 
    p_folio_liquidacion      DECIMAL(9,0),
    p_fecha_ini              DATE,
    p_fecha_fin              DATE,
    p_tipo_trabajador        CHAR(1),
    v_query                  STRING,
    v_criterios              STRING, --Arma el query dinámico   
    v_criterios2             STRING  --Arma el query dinámico 
    
  LET v_index = 1

  --Escribimos título de las filas
  LET arr_acreditados[v_index].v_nada       = "Deudor Igual"

  LET arr_acreditados[v_index].v_deudor     = 0.00
  LET arr_acreditados[v_index].v_diferencia = 0.00
  LET arr_acreditados[v_index].v_monto92    = 0.00
  LET arr_acreditados[v_index].v_monto97    = 0.00
  LET arr_acreditados[v_index].v_montoSI    = 0.00
  LET v_monto                               = 0.00

   LET v_criterios  = " " --Inicializa variable
   LET v_criterios2 = " " --Inicializa variable
  
   IF p_folio_liquidacion IS NOT NULL THEN 
      LET v_criterios  = v_criterios,  " \n AND cnt.folio_liquida = ",p_folio_liquidacion
      LET v_criterios2 = v_criterios2, " \n AND cnt.folio_liquida = ",p_folio_liquidacion
   END IF 

   IF (p_fecha_ini IS NOT NULL) AND (p_fecha_fin IS NOT NULL ) THEN 
      LET v_criterios  = v_criterios,  " \n AND mov.f_liquida    BETWEEN '",p_fecha_ini,"'  AND '",p_fecha_fin,"' "
      LET v_criterios2 = v_criterios2, " \n AND sdo.f_movimiento between '",p_fecha_ini, "' and '",p_fecha_fin,"' " 
   END IF 

   --20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
   FOR i = 1 TO arr_tbl_mov.getLength()
      --Obtiene el "Monto de la Subcuenta” donde la diferencia es igual entre el “Deudor” y el “SSV”
      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE  cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",
                     "\n AND    mov.subcuenta         = 8",
                     --"\n AND    cnt.tpo_trabajador     = '", p_tipo_trabajador,"'",
                     "\n AND    cnt.tpo_deudor        = 3 ",
                     "\n AND    mov.movimiento    IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda
 
      --DISPLAY "Consulta 1 ",  v_query
      LET v_monto_g = 0.00 
 
      PREPARE prp_consulta1 FROM v_query
      EXECUTE prp_consulta1 INTO v_monto_g
 
      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN 
         LET v_monto_g = 0.00 
      END IF
      LET arr_acreditados[v_index].v_monto92 = arr_acreditados[v_index].v_monto92 + v_monto_g
 
      --Obtiene el "Monto de la Subcuenta” donde la diferencia es igual entre el “Deudor” y el “SSV”
      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE  cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",                 
                     "\n AND    mov.subcuenta         = 4",
                     "\n AND    cnt.tpo_deudor        = 3 ",
                     "\n AND    mov.movimiento       IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda
      --DISPLAY "Consulta 1 ",  v_query
      LET v_monto_g = 0.00 
 
      PREPARE prp_consulta11 FROM v_query
      EXECUTE prp_consulta11 INTO v_monto_g
 
      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN 
         LET v_monto_g = 0.00 
      END IF
 
      LET arr_acreditados[v_index].v_monto97 = arr_acreditados[v_index].v_monto97 + v_monto_g
 
      --Obtiene el "Monto de la Subcuenta” donde la diferencia es igual entre el “Deudor” y el “SSV”
      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE  cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",                 
                     "\n AND    mov.subcuenta         = 44",
                     "\n AND    cnt.tpo_deudor        = 3 ",
                     "\n AND    mov.movimiento       IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda
      --DISPLAY "Consulta 1 ",  v_query
      LET v_monto_g = 0.00 
 
      PREPARE prp_consulta111 FROM v_query
      EXECUTE prp_consulta111 INTO v_monto_g
 
      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN 
         LET v_monto_g = 0.00 
      END IF
 
      LET arr_acreditados[v_index].v_montoSI = arr_acreditados[v_index].v_montoSI + v_monto_g

   END FOR
  --Obtiene el “Monto del Deudor” donde la diferencia es igual entre el  “Deudor” y el “SSV”
  LET v_query =  "\n SELECT SUM(sdo.monto_pesos)  ",
                 "\n FROM   safre_viv:cre_saldo_deudor sdo, cre_ctr_contable cnt ",
                 --"\n WHERE  cnt.folio_liquida     = ", p_folio_liquidacion,
                 --"\n AND    cnt.folio_referencia  = sdo.folio_referencia ",
                 "\n WHERE  cnt.folio_referencia  = sdo.folio_referencia ",
                 "\n AND    cnt.id_cre_acreditado = sdo.id_cre_acreditado ",
                 "\n AND    cnt.tpo_trabajador   IN ('I','S')",
                 "\n AND    cnt.tpo_deudor        = 3",
                 "\n AND    sdo.movimiento        = 181"

  LET v_query = v_query,v_criterios2  --Concatena los filtros de búsqueda                 

  {IF p_folio_liquidacion IS NOT NULL THEN 
     LET v_query = v_query || "\n AND  cnt.folio_liquida = ", p_folio_liquidacion
  END IF 
   
  IF p_fecha_ini IS NOT NULL AND 
     p_fecha_fin IS NOT NULL THEN 
     LET v_query = v_query || "\n AND  sdo.f_movimiento between '",p_fecha_ini, "' and '", p_fecha_fin,"'"
  END IF}

  --DISPLAY "Consulta 2 ",  v_query
   
  PREPARE prp_consulta2 FROM v_query
  EXECUTE prp_consulta2 INTO arr_acreditados[v_index].v_deudor

  --Si no devuelve nada asignamos 0.00 como valor
  IF arr_acreditados[v_index].v_deudor IS NULL THEN 
     LET arr_acreditados[v_index].v_deudor = 0.00 
  END IF

  --Obtenemos las diferencias
  LET v_monto = arr_acreditados[v_index].v_monto92 +
                arr_acreditados[v_index].v_monto97 +
                arr_acreditados[v_index].v_montoSI
                
  IF v_monto < 0.00 THEN 
     LET arr_acreditados[v_index].v_diferencia = arr_acreditados[v_index].v_deudor + v_monto
  ELSE
     LET arr_acreditados[v_index].v_diferencia = arr_acreditados[v_index].v_deudor - v_monto
  END IF 
   
  --Llenamos el siguiente registro donde Deudor es mayor a SSV
  LET v_index = 2

  --Escribimos título de las filas
  LET arr_acreditados[v_index].v_nada       = "Deudor > SSV"
   
  LET arr_acreditados[v_index].v_deudor     = 0.00
  LET arr_acreditados[v_index].v_diferencia = 0.00
  LET arr_acreditados[v_index].v_monto92    = 0.00
  LET arr_acreditados[v_index].v_monto97    = 0.00
  LET arr_acreditados[v_index].v_montoSI    = 0.00
  LET v_monto                               = 0.00

   --20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
   FOR i = 1 TO arr_tbl_mov.getLength()

      --Obtiene el “Monto de la Subcuenta” donde el “Deudor” es mayor al “SSV” 
      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",                 
                     "\n AND    mov.subcuenta         = 8",
                     "\n AND    cnt.tpo_deudor       IN (2,5) ",
                     "\n AND    mov.movimiento       IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda
      --DISPLAY "Consulta 3 ",  v_query

      LET v_monto_g = 0.00
      
      PREPARE prp_consulta3 FROM v_query
      EXECUTE prp_consulta3 INTO v_monto_g

      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN
         LET v_monto_g = 0.00
      END IF

      LET arr_acreditados[v_index].v_monto92 = arr_acreditados[v_index].v_monto92 + v_monto_g

      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",                 
                     "\n AND    mov.subcuenta         = 4",
                     "\n AND    cnt.tpo_deudor       IN (2,5) ",
                     "\n AND    mov.movimiento       IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda
      --DISPLAY "Consulta 3 ",  v_query

      LET v_monto_g = 0.00

      PREPARE prp_consulta33 FROM v_query
      EXECUTE prp_consulta33 INTO v_monto_g

      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN 
         LET v_monto_g = 0.00 
      END IF

      LET arr_acreditados[v_index].v_monto97 = arr_acreditados[v_index].v_monto97 + v_monto_g
      
      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",                 
                     "\n AND    mov.subcuenta         = 44",
                     "\n AND    cnt.tpo_deudor       IN (2,5) ",
                     "\n AND    mov.movimiento       IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda
      --DISPLAY "Consulta 3 ",  v_query

      LET v_monto_g = 0.00

      PREPARE prp_consulta333 FROM v_query
      EXECUTE prp_consulta333 INTO v_monto_g

      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN 
         LET v_monto_g = 0.00 
      END IF

      LET arr_acreditados[v_index].v_montoSI = arr_acreditados[v_index].v_montoSI + v_monto_g

   END FOR

  --Obtiene el “Monto del Deudor” donde el “Deudor” es mayor al “SSV”
  LET v_query =  "\n SELECT SUM(sdo.monto_pesos)  ",
                 "\n FROM   safre_viv:cre_saldo_deudor sdo, cre_ctr_contable cnt ",
                 --"\n WHERE  cnt.folio_liquida     = ", p_folio_liquidacion,
                 --"\n AND    cnt.folio_referencia  = sdo.folio_referencia ",
                 "\n WHERE    cnt.id_cre_acreditado = sdo.id_cre_acreditado ",
                 "\n AND    cnt.tpo_trabajador   IN ('I','S')",
                 "\n AND    cnt.tpo_deudor       IN (2,5) ",                                                   
                 "\n AND    sdo.movimiento        = 181"

  LET v_query = v_query,v_criterios2  --Concatena los filtros de búsqueda

  --DISPLAY "Consulta 4 ",  v_query
   
  PREPARE prp_consulta4 FROM v_query
  EXECUTE prp_consulta4 INTO arr_acreditados[v_index].v_deudor

  --Si no devuelve nada asignamos 0.00 como valor
  IF arr_acreditados[v_index].v_deudor IS NULL THEN 
     LET arr_acreditados[v_index].v_deudor = 0.00 
  END IF

  --Obtenemos las diferencias
  LET v_monto = arr_acreditados[v_index].v_monto92 +
                arr_acreditados[v_index].v_monto97 +
                arr_acreditados[v_index].v_montoSI
                
  IF v_monto < 0.00 THEN 
     LET arr_acreditados[v_index].v_diferencia = arr_acreditados[v_index].v_deudor + v_monto
  ELSE
     LET arr_acreditados[v_index].v_diferencia = arr_acreditados[v_index].v_deudor - v_monto
  END IF 

  --Llenamos el registro donde Deudor es menor
  LET v_index = 3

  --Escribimos título de las filas
  LET arr_acreditados[v_index].v_nada       = "Deudor < SSV"

  LET arr_acreditados[v_index].v_deudor     = 0.00
  LET arr_acreditados[v_index].v_diferencia = 0.00
  LET arr_acreditados[v_index].v_monto92    = 0.00
  LET arr_acreditados[v_index].v_monto97    = 0.00
  LET arr_acreditados[v_index].v_montoSI    = 0.00
  LET v_monto                               = 0.00

   --20140123      Se agrega uso busqueda en tablas separadas de cta_movimiento
   FOR i = 1 TO arr_tbl_mov.getLength()
      --Obtiene el el “Monto de la Subcuenta” donde el “Deudor” es menor al “SSV”
      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",                 
                     "\n AND    mov.subcuenta         = 8",
                     "\n AND    cnt.tpo_deudor       IN (1,4) ",                 
                     "\n AND    mov.movimiento       IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda
      --DISPLAY "Consulta 5 ",  v_query

      LET v_monto_g = 0.00
                      
      PREPARE prp_consulta5 FROM v_query
      EXECUTE prp_consulta5 INTO v_monto_g

      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN 
         LET v_monto_g = 0.00 
      END IF

      LET arr_acreditados[v_index].v_monto92 = arr_acreditados[v_index].v_monto92 + v_monto_g

      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",                 
                     "\n AND    mov.subcuenta         = 4",
                     "\n AND    cnt.tpo_deudor       IN (1,4) ",                 
                     "\n AND    mov.movimiento       IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda

      --DISPLAY "Consulta 5 ",  v_query

      LET v_monto_g = 0.00
                      
      PREPARE prp_consulta55 FROM v_query
      EXECUTE prp_consulta55 INTO v_monto_g

      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN 
         LET v_monto_g = 0.00 
      END IF

      LET arr_acreditados[v_index].v_monto97 = arr_acreditados[v_index].v_monto97 + v_monto_g

      LET v_query =  "\n SELECT SUM(mov.monto_pesos) ",
                     "\n FROM   ",arr_tbl_mov[i]," mov, cre_ctr_contable cnt ",
                     --"\n WHERE  mov.folio_liquida     = ", p_folio_liquidacion,
                     --"\n AND    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n WHERE    cnt.folio_liquida     = mov.folio_liquida ",
                     "\n AND    cnt.id_cre_acreditado = mov.id_referencia ",                 
                     "\n AND    mov.subcuenta         = 44",
                     "\n AND    cnt.tpo_deudor       IN (1,4) ",                 
                     "\n AND    mov.movimiento       IN (472,482,122,132,242,432,492,152,82) "
                     ,v_criterios  --Concatena los filtros de búsqueda
      --DISPLAY "Consulta 5 ",  v_query

      LET v_monto_g = 0.00
                      
      PREPARE prp_consulta555 FROM v_query
      EXECUTE prp_consulta555 INTO v_monto_g

      --Si no devuelve nada asignamos 0.00 como valor
      IF v_monto_g IS NULL THEN 
         LET v_monto_g = 0.00 
      END IF

      LET arr_acreditados[v_index].v_montoSI = arr_acreditados[v_index].v_montoSI + v_monto_g
      
END FOR

  --Obtiene el “Monto del Deudor” donde el “Deudor” es menor al “SSV” 
  LET v_query =  "\n SELECT SUM(sdo.monto_pesos)  ",
                 "\n FROM   safre_viv:cre_saldo_deudor sdo, cre_ctr_contable cnt ",
                 --"\n WHERE  cnt.folio_liquida     = ", p_folio_liquidacion,
                 --"\n AND    cnt.folio_referencia  = sdo.folio_referencia ",
                 "\n WHERE    cnt.folio_referencia  = sdo.folio_referencia ",
                 "\n AND    cnt.id_cre_acreditado = sdo.id_cre_acreditado ",
                 "\n AND    cnt.tpo_trabajador   IN ('I','S')",
                 "\n AND    cnt.tpo_deudor       IN (1,4) ",
                 "\n AND    sdo.movimiento        = 181"

  LET v_query = v_query,v_criterios2  --Concatena los filtros de búsqueda
  --DISPLAY "Consulta 6 ",  v_query

   PREPARE prp_consulta6 FROM v_query
   EXECUTE prp_consulta6 INTO arr_acreditados[v_index].v_deudor

   --Si no devuelve nada asignamos 0.00 como valor
   IF arr_acreditados[v_index].v_deudor IS NULL THEN 
      LET arr_acreditados[v_index].v_deudor = 0.00 
   END IF
   
   --Obtenemos las diferencias 
   LET v_monto = arr_acreditados[v_index].v_monto92 +
                 arr_acreditados[v_index].v_monto97 +
                 arr_acreditados[v_index].v_montoSI
                
   IF v_monto < 0.00 THEN 
      LET arr_acreditados[v_index].v_diferencia = arr_acreditados[v_index].v_deudor + v_monto
   ELSE
      LET arr_acreditados[v_index].v_diferencia = arr_acreditados[v_index].v_deudor - v_monto
   END IF 

   --Llenamos el arreglo de la sumatoria
   LET arr_tot_acreditados[1].v_nada           = "Sumatoria"
   
   LET arr_tot_acreditados[1].v_tot_monto92    = arr_acreditados[1].v_monto92 + arr_acreditados[2].v_monto92 + arr_acreditados[3].v_monto92
   LET arr_tot_acreditados[1].v_tot_monto97    = arr_acreditados[1].v_monto97 + arr_acreditados[2].v_monto97 + arr_acreditados[3].v_monto97
   LET arr_tot_acreditados[1].v_tot_montoSI    = arr_acreditados[1].v_montoSI + arr_acreditados[2].v_montoSI + arr_acreditados[3].v_montoSI
   LET arr_tot_acreditados[1].v_tot_deudor     = arr_acreditados[1].v_deudor + arr_acreditados[2].v_deudor + arr_acreditados[3].v_deudor
   LET arr_tot_acreditados[1].v_tot_diferencia = arr_acreditados[1].v_diferencia + arr_acreditados[2].v_diferencia + arr_acreditados[3].v_diferencia

END FUNCTION  

--arr_acreditados
--arr_tot_acreditados[1]
--Genera un archivo de salida en texto plano con la información de transferencia de acreditados
FUNCTION fn_genera_archivo_transferencia()

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
      
   LET v_nom_archivo = "/deudor_trans_acreditados_", v_hora

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
   --v_folio_liquidacion, f_fecha_ini, f_fecha_fin 
   
   IF v_folio_liquidacion IS NOT NULL THEN 
      LET v_encabezado = "Folio de liquidación: ",v_folio_liquidacion 
      CALL v_ch_arch_salida.write([v_encabezado])
   END IF 
   
   IF f_fecha_ini IS NOT NULL THEN 
      LET v_encabezado = "Fecha inicial: ",f_fecha_ini USING "dd-mm-yyyy"
      CALL v_ch_arch_salida.write([v_encabezado])
   END IF 
   
   IF f_fecha_fin IS NOT NULL THEN 
      LET v_encabezado = "Fecha final: ",f_fecha_fin USING "dd-mm-yyyy"
      CALL v_ch_arch_salida.write([v_encabezado])
   END IF 
   
   
   LET v_encabezado = "|Deudor|Monto Aportación 92 |Monto Aportación 97|Monto Aportación Solo Infonavit |Diferencia| "
   CALL v_ch_arch_salida.write([v_encabezado])
   
   
   FOR v_recorre_arreglo = 1 TO arr_acreditados.getLength()
      LET v_detalle = arr_acreditados[v_recorre_arreglo].v_nada, "|",
                      arr_acreditados[v_recorre_arreglo].v_deudor, "|",
                      arr_acreditados[v_recorre_arreglo].v_monto92, "|",
                      arr_acreditados[v_recorre_arreglo].v_monto97, "|",
                      arr_acreditados[v_recorre_arreglo].v_montoSI, "|",
                      arr_acreditados[v_recorre_arreglo].v_diferencia, "|"
    
      CALL v_ch_arch_salida.write([v_detalle])

   END FOR

   --Escribe el sumario
    LET v_sumario = "Sumatoria|",arr_tot_acreditados[1].v_tot_deudor,"|",
                     arr_tot_acreditados[1].v_tot_monto92,"|",
                     arr_tot_acreditados[1].v_tot_monto97,"|",
                     arr_tot_acreditados[1].v_tot_montoSI,"|",
                     arr_tot_acreditados[1].v_tot_diferencia,"|"

   CALL v_ch_arch_salida.write([v_sumario])
   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   
   --Cambia el formato del archivo a DOS
   LET v_comando_dos = "unix2dos ",v_ruta_envio_cnt CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   CALL fn_mensaje("Información","Se ha generado el archivo de Deudor Transferencia de Acreditados\n en la ruta"||v_ruta_nomarch,"information")
   

END FUNCTION 



#Función que genera el reporte de transferencia de acreditados
FUNCTION fn_genera_reporte_transferencia(p_fecha_ini, p_fecha_fin, p_tipo_trabajador)
  DEFINE 
    p_fecha_ini              DATE,
    p_fecha_fin              DATE,
    p_tipo_trabajador        CHAR (1)
          
  DEFINE 
    v_manejador_rpt          om.SaxDocumentHandler,  -- Contenedor documentos reporte
    v_indice                 SMALLINT

  LET v_indice = 1
      
  IF fgl_report_loadCurrentSettings("CNTC062.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET v_manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  START REPORT rp_transferencia_acreditados TO XML HANDLER v_manejador_rpt
    FOR v_indice = 1 TO 3
        OUTPUT TO REPORT rp_transferencia_acreditados(p_fecha_ini,
                                                      p_fecha_fin,
                                                      p_tipo_trabajador,
                                                      arr_acreditados[v_indice].*,
                                                      arr_tot_acreditados[1].*
                                                      )
    END FOR 

  FINISH REPORT rp_transferencia_acreditados
  DISPLAY "Se generó el reporte "

END FUNCTION 

#Reporte de transferencia de acreditados
REPORT rp_transferencia_acreditados (p_fecha_ini,p_fecha_fin,p_tipo_trabajador,rp_arr_acreditados,rp_arr_tot_acreditados)
  --Arreglo para almacenar los valores de Deudor transferencia de acreditados
  DEFINE 
    p_fecha_ini              DATE,
    p_fecha_fin              DATE,
    p_tipo_trabajador        CHAR (1)
   
  DEFINE 
    rp_arr_acreditados       RECORD 
    v_nada                   VARCHAR(10),
    v_deudor                 DECIMAL(22,2),    
    v_monto92                DECIMAL(22,2),
    v_monto97                DECIMAL(22,2),
    v_montoSI                DECIMAL(22,2),
    v_diferencia             DECIMAL(22,2)
  END RECORD 

  --Arreglo para almacenar los valores totales de Deudor transferencia de acreditados
  DEFINE 
    rp_arr_tot_acreditados   RECORD 
    v_nada                   VARCHAR(10),
    v_tot_deudor             DECIMAL(22,2),    
    v_tot_monto92            DECIMAL(22,2),
    v_tot_monto97            DECIMAL(22,2),
    v_tot_montoSI            DECIMAL(22,2),    
    v_tot_diferencia         DECIMAL(22,2)
  END RECORD

  DEFINE 
    f_fecha_reporte          DATE
   
  FORMAT 
    FIRST PAGE HEADER
      LET f_fecha_reporte = TODAY
      
      PRINTX f_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_fecha_ini     USING "dd-mm-yyyy"
      PRINTX p_fecha_fin     USING "dd-mm-yyyy"
      PRINTX p_tipo_trabajador
      PRINTX g_usuario

    ON EVERY ROW 
       PRINTX rp_arr_acreditados.*
       PRINTX rp_arr_tot_acreditados.*
   
END REPORT 
