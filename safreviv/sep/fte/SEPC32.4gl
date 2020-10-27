--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#Modulo            => SEP                                                     #
#Programa          => SEPC32                                                  #
#Objetivo          => CONSULTA OP 28                                          #
#Autor             => SINUHE DIAZ LUNA                                        #
#Fecha Inicio      => Junio 27 2012                                           #
###############################################################################
DATABASE safre_viv
DEFINE  p_usuario_cod        LIKE seg_usuario.usuario_cod, # Clave del usuario firmado
        v_ventana            ui.Window,
        v_forma              ui.form,
        p_tipo_ejecucion     SMALLINT,                     # Forma como ejecutara el programa
        p_ventana            STRING,                       # Titulo de la ventana
        p_nss_invadido       LIKE sep_det_02_op28.invadido, # NSS recibido por SEPC22(consulta de expedientes)
        p_nss_asociado       LIKE sep_det_03_op28.asociado, # NSS recibido por SEPC22(consulta de expedientes)
        v_arr_op28           DYNAMIC ARRAY OF RECORD        # Arreglo par la consulta de resultados de operacion 28
           v_id_det02         LIKE sep_det_02_op28.id_det_02_op28,
           v_folio            LIKE sep_det_02_op28.folio,
           v_invadido         LIKE sep_det_02_op28.invadido,
           v_asociado         LIKE sep_det_03_op28.asociado,
           v_clasificacion    LIKE sep_det_02_op28.clasifica_separacion,
           v_sdo_inv92        LIKE sep_det_02_op28.saldo_viv_92,
           v_sdo_aso92        LIKE sep_det_03_op28.saldo_viv_92,
           v_sdo_inv97        LIKE sep_det_02_op28.saldo_viv_97,
           v_sdo_aso97        LIKE sep_det_03_op28.saldo_viv_97,
           v_sdo_tot92        LIKE sep_det_02_op28.saldo_viv_92, -- Sumatoria del saldo v92 invadido y asociado
           v_sdo_tot97        LIKE sep_det_02_op28.saldo_viv_97, -- Sumatoria del saldo v97 invadido y asociado
           v_estado           LIKE sep_estado_det_op28.descripcion,
           v_descripcion      LIKE sep_estado_det_op28.descripcion
        END RECORD,
        arr_invadido_sar92 DYNAMIC ARRAY OF RECORD        # Arreglo para la vista de movimientos invadido v92
           v_atributo         STRING,
           v_valor            STRING
        END RECORD,
        arr_asociado_sar92 DYNAMIC ARRAY OF RECORD        # Arreglo para la vista de movimientos asociado v92
           v_atributo         STRING,
           v_valor            STRING
        END RECORD,
        arr_invadido_viv97 DYNAMIC ARRAY OF RECORD        # Arreglo para la vista de movimientos invadido v97
           v_atributo         STRING,
           v_valor            STRING
        END RECORD,
        arr_asociado_viv97 DYNAMIC ARRAY OF RECORD        # Arreglo para la vista de movimientos asociado v97
           v_atributo         STRING,
           v_valor            STRING
        END RECORD
MAIN

 # Se recupera la clave de usuario desde parámetro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_ventana        = ARG_VAL(3)
   LET p_nss_invadido   = ARG_VAL(4)
   LET p_nss_asociado   = ARG_VAL(5)
   -- se invoca la consulta de operacion 28 individual
   CALL f_consulta_operacion28()

END MAIN

FUNCTION f_consulta_operacion28()
# Funcion para buscar registros individuales de la operacion 28
DEFINE   v_folio     LIKE glo_folio.folio -- folio
        ,v_invadido  LIKE sep_det_02_op28.invadido -- nss invadido
        ,v_asociado  LIKE sep_det_03_op28.asociado -- nss asociado
        ,v_busca     STRING, -- cadena para generar la condicion de busqueda 
         v_condicion STRING ,
         v_resultados INTEGER  ,
         v_band_cancel INTEGER 
-- se abre la ventana de consulta
OPEN WINDOW sep_c32 WITH FORM "SEPC321"
   
   -- Se asigna el titulo de la ventana
   LET v_ventana = ui.Window.getCurrent()
   LET v_forma = v_ventana.getForm() 
   IF ( p_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_ventana)         
      CALL v_ventana.setText(p_ventana)
   END IF
   -- se abre el dialogo para realizar la captura de los datos de consulta y realizar
   -- el despligeue de los resultados
   -- los arreglos de datos se muestran en un dialogo
   -- oculta los grupos de detalle 
      CALL v_forma.setElementHidden("gpo_resultado_op28",1)
      CALL v_forma.setElementHidden("hb_movimientos",1)
      LET v_band_cancel =0
      -- LET p_nss_invadido   = "01018103919"
     --  LET p_nss_asociado   = "01018103919"
   DIALOG ATTRIBUTE(UNBUFFERED) 
      -- captura de parametros de consulta
      -- Si hay resultados de la busqueda se muestra el arreglo con los resultados  

      INPUT  v_folio, v_invadido, v_asociado FROM folio, invadido, asociado 
         BEFORE INPUT
            -- se limpian las varibles
            LET v_folio  = NULL
            --LET v_invadido  = NULL
           -- LET v_asociado = NULL
           IF p_nss_asociado  IS NOT  NULL AND  p_nss_invadido IS NOT  NULL THEN 
              CALL  DIALOG.setFieldActive ("asociado",FALSE )
              CALL  DIALOG.setFieldActive ("invadido",FALSE )
               CALL  DIALOG.setFieldActive ("folio",FALSE )
              LET v_invadido  = p_nss_invadido
              LET v_asociado = p_nss_asociado
              DISPLAY    v_invadido, v_asociado TO   invadido, asociado 
              LET v_band_cancel=1
               CALL f_genera_condicion(v_folio,v_invadido,v_asociado) RETURNING v_condicion 
               CALL f_busca_op28( v_condicion ) RETURNING v_resultados
           END IF 

        AFTER FIELD folio
          LET v_folio = GET_FLDBUF(folio)
        AFTER FIELD invadido
           LET v_invadido = GET_FLDBUF(invadido)
        AFTER FIELD asociado
           LET v_asociado = GET_FLDBUF(asociado)
           NEXT FIELD folio     

        ON ACTION Aceptar
          CALL  DIALOG.setFieldActive ("folio",FALSE )
          CALL  DIALOG.setFieldActive ("asociado",FALSE )
           CALL  DIALOG.setFieldActive ("invadido",FALSE )
          CALL f_genera_condicion(v_folio,v_invadido,v_asociado) RETURNING v_condicion   
          { IF v_folio IS NULL AND
              v_invadido IS NULL AND
              v_asociado IS NULL THEN
              CALL fn_mensaje("Aviso","Debe indicar un folio de búsqueda obligatoriamente","information")
              NEXT FIELD folio
           ELSE
           -- Se llama a la funcion de busqueda de registros si se capturaron los criterios
              CALL f_busca_op28(v_folio,v_invadido,v_asociado)
           END IF }
          CALL f_busca_op28( v_condicion ) RETURNING v_resultados
          
          --AFTER INPUT 
          IF v_resultados ==0 THEN 
             NEXT FIELD folio  
          END IF 

          
         ON ACTION cancelar
            EXIT DIALOG
      END INPUT
    

      DISPLAY ARRAY v_arr_op28 TO sr_op28.* 
      -- Cuando el usario seleccione un registro del arreglo y use la accion se llaman a los movimientos        
         ON ACTION Diferencias      
            CALL arr_asociado_sar92.clear()
            CALL arr_asociado_viv97.clear()
            CALL arr_invadido_viv97.clear()
            CALL arr_invadido_sar92.clear()
            CALL v_forma.setElementHidden("hb_movimientos",1) 
            IF ( ARR_CURR() IS NOT NULL ) THEN
            -- Se valida que el estado del registro sea 25 (liquedado) para buscar
            -- informacion de los movimientos en las vistas
               IF ( v_arr_op28[ARR_CURR()].v_estado = '25' ) THEN
               -- Se llama a la funcion para mostrar los arreglos de movimientos
                  CALL f_cargaVistas()
               ELSE
                  CALL fn_mensaje("","Registro de Op. 28 sin Liquidar","about")
               END IF
            END IF             
      -- Se se cancela sobre el grid de la busqueda se ocultan los grupos de tablas
      -- para una nueva busqueda
         ON ACTION cancelar
            IF v_band_cancel=0 THEN 
                CALL v_forma.setElementHidden("gpo_resultado_op28",1)
                CALL v_forma.setElementHidden("hb_movimientos",1)
                LET v_folio    = NULL 
                LET v_invadido = NULL 
                LET v_asociado = NULL 
                CALL  DIALOG.setFieldActive ("folio",TRUE  )
                CALL  DIALOG.setFieldActive ("asociado",TRUE )
                CALL  DIALOG.setFieldActive ("invadido",TRUE )
                NEXT FIELD folio
            END IF 
            IF v_band_cancel=1 THEN 
               EXIT  DIALOG
            END IF 
            
           
      END DISPLAY 

       -- Si hay resultafos de movimientos de muestras en su tabla correspondiente 
       DISPLAY ARRAY arr_invadido_sar92 TO sr_m92_in.*
       END DISPLAY
       -- Si hay resultafos de movimientos de muestras en su tabla correspondiente
       DISPLAY ARRAY arr_asociado_sar92 TO sr_m92_as.*
       END DISPLAY
       -- Si hay resultafos de movimientos de muestras en su tabla correspondiente
       DISPLAY ARRAY arr_invadido_viv97 TO sr_m97_in.*
       END DISPLAY
       -- Si hay resultafos de movimientos de muestras en su tabla correspondiente
       DISPLAY ARRAY arr_asociado_viv97 TO sr_m97_as.*
       END DISPLAY
    END DIALOG
   -- se cierra la ventana de consulta
CLOSE WINDOW sep_c32
END FUNCTION


FUNCTION f_busca_op28(v_condicion)
# Funcion que llena el arreglo de despliegue de la op28 si encuentra datos con los criterios 
DEFINE v_resultados                   INTEGER, -- condiciones
       v_folio,v_invadido,v_asociado  VARCHAR(200), 
       v_aux                          INTEGER, -- contador
       v_consulta,v_consulta_valida   STRING ,  
       v_condicion                    STRING 

       
     -- se prepara la consulta para validar que existan datos con los criterios recibidos
     LET v_consulta_valida = "\n SELECT COUNT(*)            " ,
                             "\n   FROM sep_det_02_op28 det2", 
                             "\n   JOIN  sep_det_03_op28 det3 ",
                             "\n     ON det2.id_det_02_op28 = det3.id_det_02_op28", 
                             "\n WHERE ",v_condicion
                          --   "\n  WHERE det2.folio = ", v_folio ,
                            -- "\n  AND det2.invadido = ", v_invadido,
                            -- "\n  AND det3.asociado = ", v_asociado

     DISPLAY v_consulta_valida
     -- se prepara la consulta
     PREPARE prp_valida_resultados FROM v_consulta_valida
     -- se ejecuta la consulta que regresa un unico valor
     EXECUTE prp_valida_resultados INTO v_resultados

     -- se evalua si se encontro almenos 1 resultado, si es asi se realiza la consulta para llenar el arreglo
     IF (v_resultados > 0) THEN   

    -- se prepara la consulta de los datos 
        LET v_consulta = "\n SELECT det2.id_det_02_op28,det2.folio, det2.invadido, det3.asociado,det2.clasifica_separacion,",
                         "\n        det2.saldo_viv_92,det3.saldo_viv_92,det2.saldo_viv_97,det3.saldo_viv_97,",
                         "\n        det2.saldo_viv_92+det3.saldo_viv_92 AS V92,det2.saldo_viv_97+det3.saldo_viv_97 AS V97,",
                         "\n        det2.estado , ",
                         "\n        edo.descripcion ",
                         "\n FROM sep_det_02_op28 det2 JOIN  sep_det_03_op28 det3 ",
                         "\n    ON det2.id_det_02_op28 = det3.id_det_02_op28 ,",
                         "\n    sep_estado_det_op28 edo ",                         
                         "\n  WHERE ",v_condicion,
                         "\n  AND   det2.estado = edo.estado "
                         --det2.folio = ", v_folio ,
                         --"\n  AND det2.invadido = ", v_invadido,
                         --"\n  AND det3.asociado = ", v_asociado
        
    -- se borra el arreglo de despliegue
       CALL v_arr_op28.clear()
    
    -- se prepara la consulta
       PREPARE p_consulta_op28 FROM v_consulta
       DISPLAY v_consulta
       DECLARE c_consulta_op28 CURSOR FOR p_consulta_op28
    -- se inicia el contador
       LET v_aux = 1
    -- se transfieren los datos al arreglo de despliegue
       FOREACH c_consulta_op28 INTO v_arr_op28[v_aux].*
       -- se incrementa el indice
          LET v_aux = v_aux + 1
       END FOREACH
    
    -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
       CALL v_arr_op28.deleteElement(v_aux)
       CALL v_forma.setElementHidden("gpo_resultado_op28",0)
    -- se libera el cursor 
       FREE c_consulta_op28
    ELSE
    -- si no se encentran resultados de la con los criterios se notifica al usuario
       CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
    END IF
RETURN v_resultados
END FUNCTION


FUNCTION f_cargaVistas()
# funcion que carga los arreglos de despliege de SAR92 y VIV97 invadidos utilizando la funcion
# fn_ser_recupera_etiquetas
DEFINE v_consulta                STRING, -- cadena de consulta
       aux                       INTEGER, -- contador
       v_atributo                VARCHAR(50), -- atributo
       v_valor                   VARCHAR(50), -- valor
       v_id_his_preliquida_op28  VARCHAR(50) -- id de la tabla his_preliquida_op28

    -- Priemro se busca que existan valores en la tabla de historicos
    -- con el valor id de la tabla detalle 
   SELECT id_his_preliquida_op28
      INTO   v_id_his_preliquida_op28
      FROM   sep_his_preliquida_op28
      WHERE  id_det_02_op28 = v_arr_op28[ARR_CURR()].v_id_det02
  display "select: ",v_arr_op28[ARR_CURR()].v_id_det02
  DISPLAY "v_id_his_preliquida_op28: ",v_id_his_preliquida_op28
  
  -- Si existen valores se intentara cargar cada vista con el id obtenido
  -- No se indico esta validacion pero se agrega, no existe mensaje de error en caso
  -- de no encontrar relacion alguna
  IF (v_id_his_preliquida_op28 IS NOT NULL) THEN
  CALL v_forma.setElementHidden("hb_movimientos",0) 
  
  -- se define la consulta que se utiliza en la carga de los moviemientos   
     LET v_consulta = "EXECUTE FUNCTION fn_sep_recupera_etiquetas(?,?)"

     PREPARE prp_datosVista1 FROM v_consulta
     DECLARE cur_datosVista1 CURSOR FOR prp_datosVista1
     LET aux = 1
  -- se llena el arreglo con la llamada a la funcion 
--     FOREACH cur_datosVista1 USING 'sep_v_preliquida_sar92_invadido',v_id_his_preliquida_op28
     FOREACH cur_datosVista1 USING 'sep_v_preliquida_sar92_invadido',v_arr_op28[ARR_CURR()].v_id_det02
                          INTO v_atributo,v_valor   
        display "v_atributo:",v_atributo," v_valor :" ,v_valor                      
        LET arr_invadido_sar92[aux].v_atributo = v_atributo
        LET arr_invadido_sar92[aux].v_valor    = v_valor
        
        LET aux = aux + 1
     END FOREACH
     FREE cur_datosVista1

     PREPARE prp_datosVista2 FROM v_consulta
     DECLARE cur_datosVista2 CURSOR FOR prp_datosVista1
     LET aux = 1
  -- se llena el arreglo con la llamada a la funcion 
     FOREACH cur_datosVista2 USING 'sep_v_preliquida_sar92_asociado',v_arr_op28[ARR_CURR()].v_id_det02
                         INTO v_atributo,v_valor      
        LET arr_asociado_sar92[aux].v_atributo = v_atributo
        LET arr_asociado_sar92[aux].v_valor    = v_valor
        LET aux = aux + 1
     END FOREACH
     FREE cur_datosVista2

     PREPARE prp_datosVista3 FROM v_consulta
     DECLARE cur_datosVista3 CURSOR FOR prp_datosVista1
     LET aux = 1
  -- se llena el arreglo con la llamada a la funcion 
     FOREACH cur_datosVista3 USING 'sep_v_preliquida_viv97_invadido',v_arr_op28[ARR_CURR()].v_id_det02
                          INTO v_atributo,v_valor      
       LET arr_invadido_viv97[aux].v_atributo = v_atributo
       LET arr_invadido_viv97[aux].v_valor    = v_valor
       LET aux = aux + 1
     END FOREACH
     FREE cur_datosVista3

     PREPARE prp_datosVista4 FROM v_consulta
     DECLARE cur_datosVista4 CURSOR FOR prp_datosVista1
     LET aux = 1
  -- se llena el arreglo con la llamada a la funcion 
     FOREACH cur_datosVista4 USING 'sep_v_preliquida_viv97_asociado',v_arr_op28[ARR_CURR()].v_id_det02
                          INTO v_atributo,v_valor      
        LET arr_asociado_viv97[aux].v_atributo = v_atributo
        LET arr_asociado_viv97[aux].v_valor    = v_valor
        LET aux = aux + 1
     END FOREACH
     FREE cur_datosVista4
  END IF
END FUNCTION

FUNCTION f_genera_condicion(v_folio,v_invadido,v_asociado)

DEFINE v_condicion STRING ,
       v_folio            LIKE sep_det_02_op28.folio,
       v_invadido         LIKE sep_det_02_op28.invadido,
       v_asociado         LIKE sep_det_03_op28.asociado


LET v_condicion = " 1=1 "

IF v_folio IS NOT  NULL THEN 
LET v_condicion = v_condicion ,"\n  AND  det2.folio = ", v_folio 
END IF 
IF v_invadido IS NOT NULL THEN 
LET v_condicion = v_condicion , "\n  AND det2.invadido ='", v_invadido ,"'"
END IF 
IF v_asociado IS  NOT NULL THEN
LET v_condicion = v_condicion,"\n  AND det3.asociado ='", v_asociado ,"'"
END IF 

RETURN v_condicion
END FUNCTION 