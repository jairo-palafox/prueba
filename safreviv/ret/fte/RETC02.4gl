--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC02                                                                 #
#OBJETIVO     => CONSULTA  Y VALIDACION DE CIFRAS DE CONTROL                            #
#FECHA INICIO => OCTUBRE 29, 2012                                                       #
#MODIFICACION => Se agrega generacion de reportes de disposicion, transferencia y tipo N#
#########################################################################################
DATABASE safre_viv
GLOBALS "RETX02.4gl" 

GLOBALS 
  DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod -- clave del usuario firmado
  DEFINE v_reg_sobregirados  
        ,v_indice_reg_1
        ,v_indice_reg              INTEGER 
  DEFINE v_f_liquidacion           DATE
        ,v_valor_fondo             LIKE glo_valor_fondo.precio_fondo -- valor fondo para tipo_n
        ,v_fecha_valuacion_nueva   VARCHAR(10) -- fecha de valuacion nueva
        ,v_mes_en_texto            VARCHAR(2)
        ,v_fecha_valuacion         DATE -- fecha de valuacion 
END GLOBALS

MAIN
 DEFINE p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING   -- titulo de la ventana        
       ,v_tipo_retiro    SMALLINT  

DEFINE  r_folio RECORD  
              folio    DECIMAL(9,0)
   END RECORD 
       
DEFINE cb_tipo_retiro  ui.ComboBox
DEFINE cb_folio        ui.ComboBox
DEFINE w  ui.window
DEFINE f  ui.form
DEFINE d  ui.Dialog

DEFINE ar_ret_tipo_retiro      RECORD LIKE ret_tipo_retiro.*
DEFINE v_cb_folio              DECIMAL(9,0)
  
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
    -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   CLOSE WINDOW SCREEN 
   OPEN WINDOW c_criterios  WITH FORM "RETC020"

   LET cb_tipo_retiro     = ui.ComboBox.forName("formonly.cb_tipo_retiro")
   LET cb_folio           = ui.ComboBox.forName("formonly.cb_folio")
   CALL cb_tipo_retiro.clear()
   CALL cb_folio.clear()
   LET w         = ui.Window.getCurrent()
   LET f         = w.getForm()
   
   #se capturan los campos para realizar los fltros 
   INPUT v_tipo_retiro  
        ,v_cb_folio
        ,v_folio
   WITHOUT DEFAULTS 
   FROM cb_tipo_retiro
       ,cb_folio
       ,e_folio 
   ATTRIBUTES (ACCEPT  = FALSE ,CANCEL  = FALSE ,UNBUFFERED = TRUE)      
   
      BEFORE INPUT 
         INITIALIZE  v_folio                TO NULL
         INITIALIZE r_ret_cza_disposicion   TO NULL           
         INITIALIZE arr_cifras_control      TO NULL
         CALL f.setElementHidden("formonly.e_folio",1)
   
         DECLARE  c_cb_tipo_retiro CURSOR FOR
         SELECT * 
         FROM   ret_modalidad_retiro
         WHERE  modalidad_retiro IN (4,5,6,8)
         ORDER BY modalidad_retiro
        
         FOREACH c_cb_tipo_retiro INTO ar_ret_modalidad_retiro.*
            CALL cb_tipo_retiro.addItem(ar_ret_modalidad_retiro.modalidad_retiro ,ar_ret_modalidad_retiro.modalidad_retiro||" - "||ar_ret_modalidad_retiro.des_corta)
         END FOREACH 
         INITIALIZE ar_ret_tipo_retiro.des_corta TO NULL
   
      ON CHANGE cb_tipo_retiro        
         IF ( v_tipo_retiro IS NULL OR v_tipo_retiro = 0 ) THEN 
            CALL fn_mensaje("Atención", "Se debe seleccionar un tipo de Retiro", "stop")
            CONTINUE INPUT
         END IF 
         
         -- se borra el arreglo de folios
         CALL cb_folio.clear()
         CALL cb_folio.addItem("0", "Capturar Folio")
         
         -- tipo de retiro
         CASE  v_tipo_retiro           
        	  WHEN 4  
               DECLARE  c_cb_folio_tipo_n CURSOR FOR
                                   SELECT folio
                                     FROM ret_cza_tipo_n
                                    ORDER BY folio  DESC
            
               FOREACH c_cb_folio_tipo_n INTO r_folio.*
                  CALL cb_folio.addItem(r_folio.folio , r_folio.folio)
               END FOREACH
               
            WHEN 5  
               DECLARE  c_cb_folio_disp CURSOR FOR
                                   SELECT a.folio
                                   FROM glo_folio a, ret_cza_disposicion b
                                   WHERE a.proceso_cod = 1502
                                   AND a.folio = b.folio
                                   ORDER BY 1 DESC
            
               FOREACH c_cb_folio_disp INTO r_folio.*
                  CALL cb_folio.addItem(r_folio.folio , r_folio.folio)
               END FOREACH 
            
            WHEN 6  
               DECLARE  c_cb_folio_trans CURSOR FOR
                                   SELECT folio
                                     FROM ret_cza_transferencia
                                    ORDER BY folio  DESC
            
               FOREACH c_cb_folio_trans INTO r_folio.*
                   CALL cb_folio.addItem(r_folio.folio , r_folio.folio)
               END FOREACH
            
            WHEN 8  
               DECLARE  c_cb_folio_pmg CURSOR FOR
                                   SELECT a.folio
                                   FROM glo_folio a, ret_cza_disposicion b
                                   WHERE a.proceso_cod = 1519
                                   AND a.folio = b.folio
                                   ORDER BY 1 DESC
              
               FOREACH c_cb_folio_pmg INTO r_folio.*
                   CALL cb_folio.addItem(r_folio.folio , r_folio.folio)
               END FOREACH
               
            OTHERWISE 
               CONTINUE INPUT  
         END CASE
         
         -- se actualiza la interfaz
         CALL ui.Interface.refresh()
{      
      ON CHANGE cb_folio
         LET v_cb_folio = GET_FLDBUF(cb_folio) 
         IF v_cb_folio = 0 THEN
            CALL f.setElementHidden("formonly.e_folio",0)
            LET v_folio = ""
            NEXT FIELD e_folio
         ELSE 
            CALL f.setElementHidden("formonly.e_folio",1)               
         END IF
        NEXT FIELD cb_tipo_retiro 
}         
          
      ON ACTION aceptar
         LET v_cb_folio = GET_FLDBUF(cb_folio)
         LET v_folio    = GET_FLDBUF(e_folio) 
         IF  v_cb_folio <> 0  THEN 
            LET v_folio = v_cb_folio
         END IF 
                    
         IF  v_tipo_retiro IS NULL OR v_tipo_retiro = 0 THEN 
            CALL fn_mensaje("Atención", "Se debe seleccionar un tipo de Retiro", "stop")
            NEXT FIELD cb_tipo_retiro
         END IF
         
         IF  v_folio IS NULL THEN 
            CALL fn_mensaje("Atención", "Es necesario elegir un folio", "stop")
            NEXT FIELD cb_tipo_retiro
         END IF 
   
         -- se verifica el tipo de retiro elegido para mostrar sus datos
         CASE v_tipo_retiro
            WHEN 4
               CALL fn_consulta_tipo_n()                 
            WHEN 5
               CALL fn_consulta_disposicion()
            WHEN 6
               CALL fn_consulta_transferencia()
            WHEN 8
               CALL fn_consulta_pmg()                 
         END CASE 
       
      ON ACTION cancelar 
         EXIT INPUT    
    
   END INPUT
   CLOSE WINDOW c_criterios  
END MAIN

FUNCTION fn_consulta_disposicion()
   
   SELECT *
     INTO  r_ret_cza_disposicion.*
     FROM ret_cza_disposicion
    WHERE folio = v_folio

     -- se obtienen los registros sobregirados
    SELECT COUNT(*)
      INTO v_reg_sobregirados
      FROM ret_preliquida
     WHERE folio_liquida = v_folio
       AND movimiento = 832  -- para disposicion

    -- se obtiene la fecha de liquidación
    SELECT MAX(f_liquida)
      INTO v_f_liquidacion 
      FROM ret_preliquida
     WHERE folio_liquida = v_folio
        
   IF  r_ret_cza_disposicion.folio IS NULL THEN 
      CALL fn_mensaje("Atención", "No se encontro el folio capturado", "stop")
      RETURN 
   END IF

      -- se obtiene la fecha valuación
      -- se calcula el primer dia del mes de la fecha valor transferencia
      -- MM/DD/YYYY
      -- DISPLAY "@ r_ret_cza_disposicion.f_valor_transferencia: ", r_ret_cza_disposicion.f_valor_transferencia
      IF ( MONTH(r_ret_cza_disposicion.f_valor_transferencia) < 10 ) THEN
        LET v_mes_en_texto = "0" || MONTH(r_ret_cza_disposicion.f_valor_transferencia)
      ELSE
        LET v_mes_en_texto = MONTH(r_ret_cza_disposicion.f_valor_transferencia)
      END IF
      
      LET v_fecha_valuacion_nueva = v_mes_en_texto || "/01/" || YEAR(r_ret_cza_disposicion.f_valor_transferencia)
      LET v_fecha_valuacion = DATE(v_fecha_valuacion_nueva)

     -- DISPLAY "@ v_fecha_valuacion: ", v_fecha_valuacion
      
   OPEN WINDOW c_cifras_control  WITH FORM "RETC021"  -- consulta por disposicion
      DISPLAY r_ret_cza_disposicion.folio               TO e_folio_muestra
      DISPLAY r_ret_cza_disposicion.nombre_archivo      TO e_archivo
      DISPLAY r_ret_cza_disposicion.f_operacion_proceso TO e_fech_oper_proc
      DISPLAY r_ret_cza_disposicion.f_carga             TO e_f_carga
      DISPLAY r_ret_cza_disposicion.total_registros     TO e_tot_registros
      DISPLAY v_reg_sobregirados                        TO e_reg_sobreg
      DISPLAY v_f_liquidacion                           TO e_f_liquidacion
      DISPLAY v_fecha_valuacion                         TO e_f_valuacion
      DISPLAY r_ret_cza_disposicion.precio_fondo        TO e_precio_fondo
      
      -------En achivo rechazados                                          
       CALL fn_carga_archivo_dispo(1,100,101,'EN ARCHIVO','RECHAZADO')      
       --DISPLAY arr_cifras_control[1].*
       
       -------En achivo aceptados
       CALL fn_carga_archivo_dispo(2,0,99,' ','ACEPTADO')
       --DISPLAY arr_cifras_control[2].*
       
        -------Preliquidado
       CALL fn_carga_preliquidacion(3,212,'PRELIQUIDADO',' ')
       --DISPLAY arr_cifras_control[3].*
       -------diferencia preliquidado
       CALL fn_calculo_diferencia_pre(4,2,3,"DIFERENCIA","ACEPTADO vs PRELIQUIDADO")
       
       -------Liquidado
       CALL fn_carga_liquidacion(5,212,'LIQUIDADO',' ')
       --DISPLAY arr_cifras_control[4].*
       -------DIFERENCIA Liquidado
       CALL fn_calculo_diferencia_liq(6,2,5,"DIFERENCIA","ACEPTADO vs LIQUIDADO")

       -- Se obtienen los pesos de aivs
       -- DISPLAY "@ .. arr_cifras_control.getLength(): ", arr_cifras_control.getLength()
       LET v_indice_reg = 1
              
       FOR v_indice_reg = 1 TO arr_cifras_control.getLength() 
         LET arr_cifras_control[v_indice_reg].v_viv92_pesos = arr_cifras_control[v_indice_reg].v_viv92 * r_ret_cza_disposicion.precio_fondo
         LET arr_cifras_control[v_indice_reg].v_viv97_pesos = arr_cifras_control[v_indice_reg].v_viv97 * r_ret_cza_disposicion.precio_fondo
       END FOR 
       
      DISPLAY ARRAY arr_cifras_control TO t_cifras_control.*
      ATTRIBUTE (ACCEPT  = FALSE ,CANCEL  = FALSE ,UNBUFFERED = TRUE)

         ON ACTION reporte 
           -- se genera reporte del tipo disposicion
            CALL fn_consulta_reporte_disposicion()

         ON ACTION cancelar
            CALL arr_cifras_control.clear()
            INITIALIZE r_ret_cza_disposicion.* TO NULL     
            EXIT DISPLAY   
      END DISPLAY   
   CLOSE WINDOW c_cifras_control          
END FUNCTION  

FUNCTION fn_consulta_transferencia()
   SELECT *
     INTO  r_ret_cza_transferencia.*
     FROM ret_cza_transferencia
    WHERE folio =   v_folio

     -- se obtienen los registros sobregirados
    SELECT COUNT(*)
      INTO v_reg_sobregirados
      FROM ret_preliquida
     WHERE folio_liquida = v_folio
       AND movimiento = 842  -- para transferencia
    
     -- se obtiene la fecha de liquidación
    SELECT MAX(f_liquida)
      INTO v_f_liquidacion 
      FROM ret_preliquida
     WHERE folio_liquida = v_folio
     
   IF  r_ret_cza_transferencia.folio IS NULL THEN 
      CALL fn_mensaje("Atención", "No se encontro el folio capturado", "stop")
      RETURN 
   END IF

      -- se obtiene la fecha valuación
      -- se calcula el primer dia del mes de la fecha valor transferencia
      -- MM/DD/YYYY
     -- DISPLAY "@ r_ret_cza_transferencia.f_valor_transferencia: ", r_ret_cza_transferencia.f_valor_transferencia
      IF ( MONTH(r_ret_cza_transferencia.f_valor_transferencia) < 10 ) THEN
        LET v_mes_en_texto = "0" || MONTH(r_ret_cza_transferencia.f_valor_transferencia)
      ELSE
        LET v_mes_en_texto = MONTH(r_ret_cza_transferencia.f_valor_transferencia)
      END IF
      
      LET v_fecha_valuacion_nueva = v_mes_en_texto || "/01/" || YEAR(r_ret_cza_transferencia.f_valor_transferencia)
      LET v_fecha_valuacion = DATE(v_fecha_valuacion_nueva)

     -- DISPLAY "@ v_fecha_valuacion: ", v_fecha_valuacion
   
   OPEN WINDOW c_cifras_control  WITH FORM "RETC022"  -- consulta transferencia
      DISPLAY r_ret_cza_transferencia.folio               TO e_folio_muestra
      DISPLAY r_ret_cza_transferencia.nombre_archivo      TO e_archivo
      DISPLAY r_ret_cza_transferencia.f_operacion_proceso TO e_fech_oper_proc
      DISPLAY r_ret_cza_transferencia.f_carga             TO e_f_carga
      DISPLAY r_ret_cza_transferencia.total_registros     TO e_tot_registros
      DISPLAY v_reg_sobregirados                          TO e_reg_sobreg
      DISPLAY v_f_liquidacion                             TO e_f_liquidacion
      DISPLAY v_fecha_valuacion                           TO e_f_valuacion
      DISPLAY r_ret_cza_transferencia.precio_fondo        TO e_precio_fondo
      
      -------En achivo rechazados 
      --indice,estado_solictud_rechazado_ini,estado_solictud_rechazado_fin,concepto,concepto_aux
      CALL fn_carga_archivo_transfe(1,100,101,'EN ARCHIVO','RECHAZADO')
      --DISPLAY arr_cifras_control[1].*
      
      -------En achivo aceptados
      --indice,estado_solictud_rechazado_ini,estado_solictud_rechazado_fin,concepto,concepto_aux
      CALL fn_carga_archivo_transfe(2,0,99,' ','ACEPTADO')
      --DISPLAY arr_cifras_control[2].*
      
       -------Preliquidado
      --indice,movimiento,concepto,concepto_aux
      CALL fn_carga_preliquidacion(3,222,'PRELIQUIDADO',' ')
      DISPLAY arr_cifras_control[3].*
      -------diferencia preliquidado
      CALL fn_calculo_diferencia_pre(4,2,3,"DIFERENCIA","ACEPTADO vs PRELIQUIDADO")
      
      -------Liquidado
      --indice,movimiento,concepto,concepto_aux
      CALL fn_carga_liquidacion(5,222,'LIQUIDADO',' ')
      DISPLAY arr_cifras_control[5].*
      -------DIFERENCIA Liquidado
      CALL fn_calculo_diferencia_liq(6,2,5,"DIFERENCIA","ACEPTADO vs LIQUIDADO")

       -- Se obtienen los pesos de aivs
      -- DISPLAY "@ .. arr_cifras_control.getLength(): ", arr_cifras_control.getLength()
      -- DISPLAY "@ .. r_ret_cza_transferencia.precio_fondo: ", r_ret_cza_transferencia.precio_fondo
       LET v_indice_reg = 1
              
       FOR v_indice_reg = 1 TO arr_cifras_control.getLength() 
         LET arr_cifras_control[v_indice_reg].v_viv97_pesos = arr_cifras_control[v_indice_reg].v_viv97 * r_ret_cza_transferencia.precio_fondo
       END FOR       
      
      DISPLAY ARRAY arr_cifras_control TO t_cifras_control.*
      ATTRIBUTE (ACCEPT  = FALSE ,CANCEL  = FALSE ,UNBUFFERED = TRUE)

         ON ACTION reporte
           -- se genera reporte del tipo disposicion
            CALL fn_consulta_reporte_transferencia()
            
         ON ACTION cancelar
            CALL arr_cifras_control.clear() 
            INITIALIZE r_ret_cza_transferencia.* TO NULL
                        
            EXIT DISPLAY   
      END DISPLAY   
   CLOSE WINDOW c_cifras_control          
END FUNCTION 

FUNCTION fn_consulta_tipo_n()
DEFINE v_f_operacion_procesar    DATE
    
    SELECT *
      INTO  r_ret_cza_tipo_n.*
      FROM ret_cza_tipo_n
     WHERE folio = v_folio

     -- se obtienen los registros sobregirados
    SELECT COUNT(*)
      INTO v_reg_sobregirados
      FROM ret_preliquida92
     WHERE folio_liquida = v_folio
       AND movimiento IN (862, 1052) -- para tipo n

    -- se obtiene la fecha de liquidación
    SELECT MAX(f_liquida)
      INTO v_f_liquidacion 
      FROM ret_preliquida92
     WHERE folio_liquida = v_folio

         
    IF  r_ret_cza_tipo_n.folio IS NULL THEN 
       CALL fn_mensaje("Atención", "No se encontro el folio capturado", "stop")
       RETURN 
    END IF

      -- se obtiene el valor fondo según la fecha procesar
      SELECT f_operacion_procesar
      INTO v_f_operacion_procesar
      FROM ret_cza_tipo_n
      WHERE folio = r_ret_cza_tipo_n.folio 

     -- DISPLAY "@ v_f_operacion_procesar: ", v_f_operacion_procesar
      
      -- se calcula el primer dia del mes de la fecha de operacion
      -- MM/DD/YYYY
      IF ( MONTH(v_f_operacion_procesar) < 10 ) THEN
        LET v_mes_en_texto = "0" || MONTH(v_f_operacion_procesar)
      ELSE
        LET v_mes_en_texto = MONTH(v_f_operacion_procesar)
      END IF
      
      LET v_fecha_valuacion_nueva = v_mes_en_texto || "/01/" || YEAR(v_f_operacion_procesar)
      LET v_fecha_valuacion = DATE(v_fecha_valuacion_nueva)
      
      -- se obtiene el valor del fondo del dia
      SELECT precio_fondo
      INTO v_valor_fondo
      FROM glo_valor_fondo
      WHERE fondo = 11
      AND f_valuacion = v_fecha_valuacion
    
    OPEN WINDOW c_cifras_control  WITH FORM "RETC023"   -- consulta tipo n
       DISPLAY r_ret_cza_tipo_n.folio               TO e_folio_muestra 
       DISPLAY r_ret_cza_tipo_n.nombre_archivo      TO e_archivo       
       DISPLAY r_ret_cza_tipo_n.f_operacion_proceso TO e_fech_oper_proc
       DISPLAY r_ret_cza_tipo_n.f_carga             TO e_f_carga       
       DISPLAY r_ret_cza_tipo_n.total_registros     TO e_tot_registros
       DISPLAY v_reg_sobregirados                   TO e_reg_sobreg  
       DISPLAY v_f_liquidacion                      TO e_f_liquidacion
       DISPLAY v_valor_fondo                        TO e_precio_fondo
       DISPLAY v_fecha_valuacion                    TO e_f_valuacion

        
      -------En achivo rechazados 
      CALL fn_carga_archivo_tipo_n(1,100,101,'EN ARCHIVO','RECHAZADO')
      --DISPLAY arr_cifras_control[1].*
      
      -------En achivo aceptados
      CALL fn_carga_archivo_tipo_n(2,0,99,' ','ACEPTADO')
      --DISPLAY arr_cifras_control[2].*
      
       -------Preliquidado
      CALL fn_carga_preliquidacion_decreto(3,202,'PRELIQUIDADO',' ')
      --DISPLAY arr_cifras_control[3].*

      -------diferencia preliquidado
      CALL fn_calculo_diferencia_pre(4,2,3,"DIFERENCIA","ACEPTADO vs PRELIQUIDADO")
      
      -------Liquidado
      CALL fn_carga_liquidacion_decreto(5,202,'LIQUIDADO',' ')
      --DISPLAY arr_cifras_control[4].*
      -------DIFERENCIA Liquidado
      CALL fn_calculo_diferencia_liq(6,2,5,"DIFERENCIA","ACEPTADO vs LIQUIDADO")

      -- Se obtienen los pesos de aivs
      -- DISPLAY "@ .. arr_cifras_control.getLength(): ", arr_cifras_control.getLength()
      -- DISPLAY "@ .. v_valor_fondo: ",v_valor_fondo
       
       LET v_indice_reg = 1
              
       FOR v_indice_reg = 1 TO arr_cifras_control.getLength() 
         LET arr_cifras_control[v_indice_reg].v_viv92_pesos = arr_cifras_control[v_indice_reg].v_viv92 * v_valor_fondo
       END FOR       

       DISPLAY ARRAY arr_cifras_control TO t_cifras_control.*
       ATTRIBUTE (ACCEPT  = FALSE ,CANCEL  = FALSE ,UNBUFFERED = TRUE)

          ON ACTION reporte
            -- se genera reporte del tipo N
             CALL fn_consulta_reporte_tipo_n()
      
          ON ACTION cancelar
             CALL arr_cifras_control.clear()
             INITIALIZE r_ret_cza_tipo_n.* TO NULL     
             EXIT DISPLAY   
       END DISPLAY   
    CLOSE WINDOW c_cifras_control          
END FUNCTION  

FUNCTION fn_consulta_pmg()
   
   SELECT *
     INTO  r_ret_cza_disposicion.*
     FROM ret_cza_disposicion
    WHERE folio = v_folio

     -- se obtienen los registros sobregirados
    SELECT COUNT(*)
      INTO v_reg_sobregirados
      FROM ret_preliquida
     WHERE folio_liquida = v_folio
       AND movimiento = 852  -- para consulta PMG

    -- se obtiene la fecha de liquidación
    SELECT MAX(f_liquida)
      INTO v_f_liquidacion 
      FROM ret_preliquida
     WHERE folio_liquida = v_folio       
        
   IF  r_ret_cza_disposicion.folio IS NULL THEN 
      CALL fn_mensaje("Atención", "No se encontro el folio capturado", "stop")
      RETURN 
   END IF

      -- se obtiene la fecha valuación
      -- se calcula el primer dia del mes de la fecha valor transferencia
      -- MM/DD/YYYY
      -- DISPLAY "@ r_ret_cza_disposicion.f_valor_transferencia: ", r_ret_cza_disposicion.f_valor_transferencia
      IF ( MONTH(r_ret_cza_disposicion.f_valor_transferencia) < 10 ) THEN
        LET v_mes_en_texto = "0" || MONTH(r_ret_cza_disposicion.f_valor_transferencia)
      ELSE
        LET v_mes_en_texto = MONTH(r_ret_cza_disposicion.f_valor_transferencia)
      END IF
      
      LET v_fecha_valuacion_nueva = v_mes_en_texto || "/01/" || YEAR(r_ret_cza_disposicion.f_valor_transferencia)
      LET v_fecha_valuacion = DATE(v_fecha_valuacion_nueva)

     -- DISPLAY "@ v_fecha_valuacion: ", v_fecha_valuacion
   
   OPEN WINDOW c_cifras_control  WITH FORM "RETC024"    -- consulta PMG
      DISPLAY r_ret_cza_disposicion.folio               TO e_folio_muestra
      DISPLAY r_ret_cza_disposicion.nombre_archivo      TO e_archivo
      DISPLAY r_ret_cza_disposicion.f_operacion_proceso TO e_fech_oper_proc
      DISPLAY r_ret_cza_disposicion.f_carga             TO e_f_carga
      DISPLAY r_ret_cza_disposicion.total_registros     TO e_tot_registros
      DISPLAY v_reg_sobregirados                        TO e_reg_sobreg
      DISPLAY v_f_liquidacion                           TO e_f_liquidacion
      DISPLAY v_fecha_valuacion                         TO e_f_valuacion
      DISPLAY r_ret_cza_disposicion.precio_fondo        TO e_precio_fondo
      
      -------En achivo rechazados                                          
       CALL fn_carga_archivo_dispo(1,100,101,'EN ARCHIVO','RECHAZADO')      
       --DISPLAY arr_cifras_control[1].*
       
       -------En achivo aceptados
       CALL fn_carga_archivo_dispo(2,0,99,' ','ACEPTADO')
       --DISPLAY arr_cifras_control[2].*
       
        -------Preliquidado
       CALL fn_carga_preliquidacion(3,212,'PRELIQUIDADO',' ')
       --DISPLAY arr_cifras_control[3].*
       -------diferencia preliquidado
       CALL fn_calculo_diferencia_pre(4,2,3,"DIFERENCIA","ACEPTADO vs PRELIQUIDADO")
       
       -------Liquidado
       CALL fn_carga_liquidacion(5,212,'LIQUIDADO',' ')
       --DISPLAY arr_cifras_control[4].*
       -------DIFERENCIA Liquidado
       CALL fn_calculo_diferencia_liq(6,2,5,"DIFERENCIA","ACEPTADO vs LIQUIDADO")

       -- Se obtienen los pesos de aivs
       DISPLAY "@ .. arr_cifras_control.getLength(): ", arr_cifras_control.getLength()
       LET v_indice_reg = 1
              
       FOR v_indice_reg = 1 TO arr_cifras_control.getLength() 
         LET arr_cifras_control[v_indice_reg].v_viv92_pesos = arr_cifras_control[v_indice_reg].v_viv92 * r_ret_cza_disposicion.precio_fondo
         LET arr_cifras_control[v_indice_reg].v_viv97_pesos = arr_cifras_control[v_indice_reg].v_viv97 * r_ret_cza_disposicion.precio_fondo
       END FOR        
            
      DISPLAY ARRAY arr_cifras_control TO t_cifras_control.*
      ATTRIBUTE (ACCEPT  = FALSE ,CANCEL  = FALSE ,UNBUFFERED = TRUE)

         ON ACTION reporte 
           -- se genera reporte del tipo disposicion
            CALL fn_consulta_reporte_pmg()

         ON ACTION cancelar
            CALL arr_cifras_control.clear()
            INITIALIZE r_ret_cza_disposicion.* TO NULL     
            EXIT DISPLAY   
      END DISPLAY   
   CLOSE WINDOW c_cifras_control          
END FUNCTION 

--Manda llamar las funciones que generan los reportes
-- si el tipo de reporte es por tipo N
FUNCTION fn_consulta_reporte_tipo_n()
    DEFINE manejador_rpt         om.SaxDocumentHandler,
           v_i_inicio_for    INTEGER
    
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
    DEFINE v_i_cont_registros   INTEGER

    LET v_i_inicio_for = 1

    LET v_i_cont_registros = arr_cifras_control.getLength()


    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados

        --Se asigna la plantilla para generar el reporte
        IF fgl_report_loadCurrentSettings("RETC0231.4rp") THEN 
            CALL fgl_report_selectDevice ("PDF")
                    
            LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","RETC0231"
            CALL fgl_report_setOutputFileName(v_ruta_reporte)
            CALL fgl_report_selectPreview(1)
            LET manejador_rpt = fgl_report_commitCurrentSettings()
        ELSE         
            DISPLAY "No fue posible generar el reporte"
            EXIT PROGRAM 
        END IF 
        
        --Inicia el reporte de registros tipo N
         START REPORT rpt_tipo_n TO XML HANDLER manejador_rpt
           FOR v_i_inicio_for = 1 TO v_i_cont_registros
                OUTPUT TO REPORT rpt_tipo_n(arr_cifras_control[v_i_inicio_for].*)
           END FOR  
         FINISH REPORT rpt_tipo_n
       
END FUNCTION

--Manda llamar las funciones que generan los reportes
-- si el tipo de reporte es por disposicion
FUNCTION fn_consulta_reporte_disposicion()
    DEFINE manejador_rpt         om.SaxDocumentHandler,
           v_i_inicio_for    INTEGER
    
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
    DEFINE v_i_cont_registros   INTEGER

    LET v_i_inicio_for = 1

    LET v_i_cont_registros = arr_cifras_control.getLength()


    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados

        --Se asigna la plantilla para generar el reporte
        IF fgl_report_loadCurrentSettings("RETC0211.4rp") THEN 
            CALL fgl_report_selectDevice ("PDF")
                    
            LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","RETC0211"
            CALL fgl_report_setOutputFileName(v_ruta_reporte)
            CALL fgl_report_selectPreview(1)
            LET manejador_rpt = fgl_report_commitCurrentSettings()
        ELSE         
            DISPLAY "No fue posible generar el reporte"
            EXIT PROGRAM 
        END IF

        --Inicia el reporte por disposicion
         START REPORT rpt_disposicion TO XML HANDLER manejador_rpt       
           FOR v_i_inicio_for = 1 TO v_i_cont_registros
                OUTPUT TO REPORT rpt_disposicion(arr_cifras_control[v_i_inicio_for].*)
           END FOR  
         FINISH REPORT rpt_disposicion  
       
END FUNCTION

--Manda llamar las funciones que generan los reportes
-- si el tipo de reporte es por transferencia
FUNCTION fn_consulta_reporte_transferencia()
    DEFINE manejador_rpt         om.SaxDocumentHandler,
           v_i_inicio_for    INTEGER
    
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
    DEFINE v_i_cont_registros   INTEGER

    LET v_i_inicio_for = 1

    LET v_i_cont_registros = arr_cifras_control.getLength()


    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados

        --Se asigna la plantilla para generar el reporte
        IF fgl_report_loadCurrentSettings("RETC0221.4rp") THEN 
            CALL fgl_report_selectDevice ("PDF")
                    
            LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","RETC0221"
            CALL fgl_report_setOutputFileName(v_ruta_reporte)
            CALL fgl_report_selectPreview(1)
            LET manejador_rpt = fgl_report_commitCurrentSettings()
        ELSE         
            DISPLAY "No fue posible generar el reporte"
            EXIT PROGRAM 
        END IF

        --Inicia el reporte por transferencia
         START REPORT rpt_transferencia TO XML HANDLER manejador_rpt       
           FOR v_i_inicio_for = 1 TO v_i_cont_registros
                OUTPUT TO REPORT rpt_transferencia(arr_cifras_control[v_i_inicio_for].*)
           END FOR  
         FINISH REPORT rpt_transferencia  
       
END FUNCTION

--Manda llamar las funciones que generan los reportes
-- si el tipo de reporte es por PMG
FUNCTION fn_consulta_reporte_pmg()
    DEFINE manejador_rpt         om.SaxDocumentHandler,
           v_i_inicio_for    INTEGER
    
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
    DEFINE v_i_cont_registros   INTEGER

    LET v_i_inicio_for = 1

    LET v_i_cont_registros = arr_cifras_control.getLength()


    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados

        --Se asigna la plantilla para generar el reporte
        IF fgl_report_loadCurrentSettings("RETC0241.4rp") THEN 
            CALL fgl_report_selectDevice ("PDF")
                    
            LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","RETC0241"
            CALL fgl_report_setOutputFileName(v_ruta_reporte)
            CALL fgl_report_selectPreview(1)
            LET manejador_rpt = fgl_report_commitCurrentSettings()
        ELSE         
            DISPLAY "No fue posible generar el reporte"
            EXIT PROGRAM 
        END IF

        --Inicia el reporte por disposicion
         START REPORT rpt_pmg TO XML HANDLER manejador_rpt       
           FOR v_i_inicio_for = 1 TO v_i_cont_registros
                OUTPUT TO REPORT rpt_pmg(arr_cifras_control[v_i_inicio_for].*)
           END FOR  
         FINISH REPORT rpt_pmg  
       
END FUNCTION


-- Funcion que genera el reporte tipo N
REPORT rpt_tipo_n(p_arr_cifras_control)
DEFINE  p_arr_cifras_control RECORD  
           v_concepto         STRING 
           ,v_concepto_aux    STRING
           ,v_registros       INTEGER 
           ,v_viv92           DECIMAL (14,6)
           ,v_viv92_pesos     DECIMAL (24,2)
           ,v_viv97           DECIMAL (14,6)
           ,v_viv97_pesos     DECIMAL (24,2)
           ,v_viv72           DECIMAL (14,2)
        END RECORD ,
        v_fecha_reporte       DATE,
        v_nombre_usuario      VARCHAR(100),
        v_nombre_archivo      VARCHAR(40)
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER    
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED

      -- si no se encuentra el nombre del archivo indicamos error
      IF ( v_nombre_archivo IS NULL ) THEN
         LET v_nombre_archivo = "No se encuentra nombre archivo"
      END IF

                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_nombre_usuario,
             p_usuario_cod ,
             v_nombre_archivo,
             r_ret_cza_tipo_n.folio               ,
             r_ret_cza_tipo_n.nombre_archivo      ,
             r_ret_cza_tipo_n.f_operacion_proceso USING "dd-mm-yyyy",
             r_ret_cza_tipo_n.f_carga             USING "dd-mm-yyyy",
             r_ret_cza_tipo_n.total_registros, 
             v_reg_sobregirados, 
             v_f_liquidacion                      USING "dd-mm-yyyy",
             r_ret_cza_tipo_n.total_registros,
             v_valor_fondo,
             v_fecha_valuacion                    USING "dd-mm-yyyy"
                                                                                              
   ON EVERY ROW                                                                               
    PRINTX p_arr_cifras_control.*
                                                                                           
END REPORT  

--Funcion que genera el reporte por disposicion
REPORT rpt_disposicion(p_arr_cifras_control)
DEFINE  p_arr_cifras_control RECORD  
           v_concepto         STRING 
           ,v_concepto_aux    STRING
           ,v_registros       INTEGER 
           ,v_viv92           DECIMAL (14,6)
           ,v_viv92_pesos     DECIMAL (24,2)
           ,v_viv97           DECIMAL (14,6)
           ,v_viv97_pesos     DECIMAL (24,2)
           ,v_viv72           DECIMAL (14,2)
        END RECORD ,
        v_fecha_reporte       DATE,
        v_nombre_usuario      VARCHAR(100),
        v_nombre_archivo      VARCHAR(40)
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER    
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED

      -- si no se encuentra el nombre del archivo indicamos error
      IF ( v_nombre_archivo IS NULL ) THEN
         LET v_nombre_archivo = "No se encuentra nombre archivo"
      END IF

                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_nombre_usuario,
             p_usuario_cod ,
             v_nombre_archivo,
             r_ret_cza_disposicion.folio               ,
             r_ret_cza_disposicion.nombre_archivo      ,
             r_ret_cza_disposicion.f_operacion_proceso USING "dd-mm-yyyy",
             r_ret_cza_disposicion.f_carga             USING "dd-mm-yyyy",
             r_ret_cza_disposicion.total_registros, 
             v_reg_sobregirados, 
             v_f_liquidacion                      USING "dd-mm-yyyy",
             r_ret_cza_disposicion.total_registros ,
             r_ret_cza_disposicion.precio_fondo ,
             v_fecha_valuacion                    USING "dd-mm-yyyy"
                                                                                              
   ON EVERY ROW                                                                               
    PRINTX p_arr_cifras_control.*
                                                                                           
END REPORT  

--Funcion que genera el reporte por transferencia
REPORT rpt_transferencia(p_arr_cifras_control)
DEFINE  p_arr_cifras_control RECORD  
           v_concepto         STRING 
           ,v_concepto_aux    STRING
           ,v_registros       INTEGER 
           ,v_viv92           DECIMAL (14,6)
           ,v_viv92_pesos     DECIMAL (24,2)
           ,v_viv97           DECIMAL (14,6)
           ,v_viv97_pesos     DECIMAL (24,2)
           ,v_viv72           DECIMAL (14,2)
        END RECORD ,
        v_fecha_reporte       DATE,
        v_nombre_usuario      VARCHAR(100),
        v_nombre_archivo      VARCHAR(40)
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER    
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED

      -- si no se encuentra el nombre del archivo indicamos error
      IF ( v_nombre_archivo IS NULL ) THEN
         LET v_nombre_archivo = "No se encuentra nombre archivo"
      END IF

                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_nombre_usuario,
             p_usuario_cod ,
             v_nombre_archivo,
             r_ret_cza_transferencia.folio               ,
             r_ret_cza_transferencia.nombre_archivo      ,
             r_ret_cza_transferencia.f_operacion_proceso USING "dd-mm-yyyy",
             r_ret_cza_transferencia.f_carga             USING "dd-mm-yyyy",
             r_ret_cza_transferencia.total_registros , 
             v_reg_sobregirados, 
             v_f_liquidacion                             USING "dd-mm-yyyy",
             r_ret_cza_transferencia.total_registros,
             r_ret_cza_transferencia.precio_fondo,
             v_fecha_valuacion                           USING "dd-mm-yyyy"
                                                                                              
   ON EVERY ROW                                                                               
    PRINTX p_arr_cifras_control.*
                                                                                           
END REPORT  

--Funcion que genera el reporte por PMG
REPORT rpt_pmg(p_arr_cifras_control)
DEFINE  p_arr_cifras_control RECORD  
           v_concepto         STRING 
           ,v_concepto_aux    STRING
           ,v_registros       INTEGER 
           ,v_viv92           DECIMAL (14,6)
           ,v_viv92_pesos     DECIMAL (24,2)
           ,v_viv97           DECIMAL (14,6)
           ,v_viv97_pesos     DECIMAL (24,2)
           ,v_viv72           DECIMAL (14,2)
        END RECORD ,
        v_fecha_reporte       DATE,
        v_nombre_usuario      VARCHAR(100),
        v_nombre_archivo      VARCHAR(40)
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER    
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED

      -- si no se encuentra el nombre del archivo indicamos error
      IF ( v_nombre_archivo IS NULL ) THEN
         LET v_nombre_archivo = "No se encuentra nombre archivo"
      END IF

                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX v_nombre_usuario,
             p_usuario_cod ,
             v_nombre_archivo,
             r_ret_cza_disposicion.folio               ,
             r_ret_cza_disposicion.nombre_archivo      ,
             r_ret_cza_disposicion.f_operacion_proceso USING "dd-mm-yyyy",
             r_ret_cza_disposicion.f_carga             USING "dd-mm-yyyy",
             r_ret_cza_disposicion.total_registros , 
             v_reg_sobregirados, 
             v_f_liquidacion                           USING "dd-mm-yyyy",
             r_ret_cza_disposicion.total_registros,
             r_ret_cza_disposicion.precio_fondo,
             v_fecha_valuacion                         USING "dd-mm-yyyy"
                                                                                              
   ON EVERY ROW                                                                               
    PRINTX p_arr_cifras_control.*
                                                                                           
END REPORT
