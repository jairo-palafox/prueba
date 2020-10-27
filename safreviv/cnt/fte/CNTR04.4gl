################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 29/10/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => CNT                                                           #
#Programa     => CNTR02                                                        #
#Objetivo     => Realizar el reverso de la confirmación de la póliza contable  #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     20140123      Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
DATABASE
   safre_viv
GLOBALS "CNTG01.4gl"

GLOBALS

   DEFINE
      f_folio_liquida   LIKE cnt_transaccion.folio_liquida

   DEFINE  

   v_arr_valida_proceso DYNAMIC ARRAY OF RECORD
      tb_f_liquida      LIKE cnt_transaccion.f_liquida,
      tb_proceso        CHAR (42),
      tb_folio_liquida  DECIMAL(9,0),
      tb_folio          DECIMAL(9,0),
      tb_doc_contable   DECIMAL(10,0),--LIKE cnt_ctr_proceso.num_poliza,
      tb_estado         CHAR (50)
   END RECORD,
   
   v_aux_valida_proceso  DYNAMIC ARRAY OF DECIMAL(22,2)

   DEFINE 
      aux      DATETIME HOUR TO SECOND
   

   DEFINE 
       p_pid            LIKE bat_ctr_proceso.pid, -- ID del proceso
       p_proceso_cod    LIKE cat_proceso.proceso_cod, -- Código del proceso
       p_opera_cod      LIKE cat_operacion.opera_cod -- Código de operacion


   DEFINE 
      f_cta_contable       CHAR (10),
      f_cargo              DECIMAL (22,2),
      cod_naturaleza_mov   SMALLINT 

   --define variables para registro manual
   DEFINE
      v_estado                SMALLINT,
      p_f_liquida             DATE,
      p_folio_liquida         DECIMAL (9,0),
      v_id_cuenta_contable    SMALLINT,
      v_cod_transaccion_cnt   SMALLINT,
      v_cod_proceso_cnt       SMALLINT,
      v_cod_subcta_cnt        SMALLINT,
      p_cod_proceso           SMALLINT,
      v_suma_pesos             DECIMAL(22,2),
      v_suma_aivs              DECIMAL(22,2)

   --Arreglo para el Detalle Información Montos de las Operaciones
  DEFINE g_arr_inf_montos_op DYNAMIC ARRAY OF RECORD 
    --arr_op_fecha             CHAR(10),
    arr_op_fecha             DATE,
    arr_op_folio             LIKE cta_movimiento.folio_liquida,
    arr_op_subcuenta         CHAR(20),
    arr_op_tipo              CHAR(42),
    arr_op_fondo             CHAR(10),
    arr_op_pesos             DECIMAL(22,2),
    arr_op_aivs              DECIMAL(22,2)
  END RECORD

  --Arreglo para el Detalle Información Póliza Contable
  DEFINE g_arr_inf_poliza_contable DYNAMIC ARRAY OF RECORD
    arr_po_fecha             LIKE cnt_transaccion.f_liquida,
    arr_po_folio_liquida     LIKE cnt_transaccion.folio_liquida,
    arr_po_folio             LIKE cnt_transaccion.folio_cnt,
    arr_po_proceso           CHAR(42),
    arr_po_cuenta            LIKE cnt_transaccion.cta_contable,
    arr_po_descripcion       LIKE cat_cuenta_contable.desc_cta_contable,
    arr_po_cargo             LIKE cnt_transaccion.importe,
    arr_po_abono             LIKE cnt_transaccion.importe,
    arr_po_estado            VARCHAR (40)
   END RECORD 

   DEFINE 
      v_suma_cargos          LIKE cnt_transaccion.importe,
      v_suma_abonos          LIKE cnt_transaccion.importe

   DEFINE 
      v_tpo_reg_cnt           SMALLINT,
      v_bandera_rev           SMALLINT 

   CONSTANT Por_Folio = 0
   CONSTANT Por_Fecha = 1
   CONSTANT Sin = 0
   DEFINE v_tbl_mov    VARCHAR(50) 
      
END GLOBALS


MAIN
   DEFINE
      f_ventana     ui.Window,   -- Define las propìedades de la Ventana
      f_forma       ui.Form,     -- Define las propiedades de la forma
      f_fecha       DATE,
      v_registros   INTEGER,
      v_estado_cnt  SMALLINT, 
      v_respuesta   SMALLINT,
      r_bandera_rev SMALLINT  

   DEFINE 
      v_ruta_ejecutable  LIKE seg_modulo.ruta_bin, -- Ruta del ejecutable
      v_ruta_listados    LIKE seg_modulo.ruta_listados, -- Rut del log
      v_s_mensaje        STRING,
      l_comando          STRING 

   DEFINE 
      v_bnd_confirma          INTEGER,
      v_existe_cuenta         INTEGER,
      v_cmb_reverso           SMALLINT,
      v_ind_current           INTEGER,
      v_estado_reg            INTEGER,
      r_bandera_reg_cnt       SMALLINT,
      v_consulta_est          STRING,
      v_status_liq            SMALLINT 

   DEFINE 
      wdw ui.Window
   
-- se asignan los parametros que vienen del fglrun
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog     = ARG_VAL(3)

   LET p_proceso_cod = 602
   LET p_opera_cod = 2

   -- se asigna el titulo del programa
   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF

   PREPARE fn_tbl_mov FROM "execute function fn_tab_movimiento(?,?,?)"

   CLOSE WINDOW SCREEN
   OPEN WINDOW wMain WITH FORM "CNTR041"
   DIALOG ATTRIBUTES (UNBUFFERED)
       INPUT BY NAME f_folio_liquida, v_cmb_reverso
          BEFORE INPUT          

            
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()
            CALL f_forma.setElementHidden("grp_valida_proceso",TRUE)
            CALL f_forma.setElementHidden("gr_info_reverso",TRUE)
            CALL f_forma.setElementHidden("gr_inf_montos_op",TRUE)
            CALL f_forma.setElementHidden("gr_nuevos_registros",TRUE)
            LET v_tpo_reg_cnt = 0 --Inicializa

             ON ACTION cancelar
               EXIT PROGRAM 
            
             ON ACTION ACCEPT
                  DISPLAY "v_cmb_reverso: ", v_cmb_reverso
                  IF v_cmb_reverso IS NULL OR f_folio_liquida IS NULL THEN 
                     CALL fn_mensaje("Error","Debe ingresar ambos criterios de búsqueda","information")
                     NEXT FIELD f_folio_liquida
                  ELSE 

                  
                     IF v_cmb_reverso = 1 THEN --Si se elige la opción de ajuste de estado
                        CALL f_forma.setElementHidden("grp_valida_proceso",FALSE)
                        CALL v_arr_valida_proceso.clear()

                        --Realiza la consulta por proceso y/o rango de periodo
                        CALL fn_valida_confirmacion_poliza() RETURNING v_registros, v_estado_cnt

                         IF v_registros <= 1 THEN
                           CALL fn_mensaje("INFORMACIÓN","No existe información con ese criterio de búsqueda.","info")
                           CALL v_arr_valida_proceso.clear()
                           CALL f_forma.setElementHidden("grp_valida_proceso",TRUE)
                           --CALL DIALOG.setActionHidden("reporte",1)
                           NEXT FIELD f_folio_liquida
                        END IF 

                        --Despliega la consulta en la tabla
                        DISPLAY ARRAY v_arr_valida_proceso TO scr_valida_procesos.* ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
                              ON ACTION cancelar 
                                    EXIT PROGRAM 

                               --Este botón se activa solo si el estado es 60 para regresarlo a 10
                               ON ACTION actualizar
                                    CALL fn_rechaza_registro_contable()
                                    CALL fn_inserta_reverso_contable(6) --Este estado no existe en el catálogo
                                    CALL fn_mensaje("Información","Se ha realizado el reverso del registro contable","information")
                                    CALL DIALOG.setActionHidden("actualizar",TRUE)
                                    CALL fn_envia_correo_cnt("Reverso Operativo de Contabilidad", "Actualización de Estado.")
                        
                        END DISPLAY 
                        
                     END IF

                     IF  v_cmb_reverso = 2 THEN --Si se elige la opción de ajustar los registros contables

                           CALL fn_reversa_poliza_manual()
                           CALL f_forma.setElementHidden("gr_inf_montos_op",FALSE) --Muestra la sección del detalle de las operaciones

                           -- si existen datos en el arreglo, desplegamos la información
                           IF g_arr_inf_montos_op.getLength() > 0 THEN 
                              
                                    LET wdw = ui.Window.getCurrent()

                                    --Consulta si ya existen registros contables creados de forma operativa
                                    CALL fn_muestra_registros_manuales()
                                    IF g_arr_inf_poliza_contable.getLength() > 0 THEN 
                                       CALL fn_mensaje("Información","Existen registros de ajuste operativo actualmente para este folio","information")
                                       CALL f_forma.setElementHidden("gr_nuevos_registros",FALSE)
                                       DISPLAY ARRAY g_arr_inf_poliza_contable TO src_inf_pol_cont.* ATTRIBUTES (CANCEL = FALSE )
                                       END DISPLAY 
                                    END IF 

                                    
                                    DISPLAY ARRAY g_arr_inf_montos_op TO scr_info_montos_op.* ATTRIBUTES (ACCEPT = FALSE )
                                       
                                       ON ACTION ajustar
                                           DIALOG ATTRIBUTES (UNBUFFERED)
                                             INPUT BY NAME f_cta_contable, cod_naturaleza_mov, f_cargo
                                                BEFORE INPUT
                                                   --Limpia campos de la forma
                                                   LET f_cta_contable = NULL
                                                   LET cod_naturaleza_mov = NULL
                                                   LET f_cargo = NULL 
                                                   
                                                   CALL f_forma.setElementHidden("gr_info_reverso",FALSE)
                                                   --Llena el combo de naturaleza de la cuenta
                                                   CALL fn_llena_combo_naturaleza()
                                                --Aplica el reverso de la póliza con los criterios capturados
                                                ON ACTION aplicar
                                                   
                                                
                                                   IF f_cta_contable IS NULL AND cod_naturaleza_mov IS NULL AND f_cargo IS NULL THEN 
                                                      CALL fn_mensaje("Error","Debe capturar todos los parámetros","information")
                                                      NEXT FIELD f_cta_contable
                                                   END IF 

                                                   --valida cuenta contable
                                                   SELECT COUNT(*)
                                                   INTO v_existe_cuenta
                                                   FROM cnt_regla_contable
                                                   WHERE cta_contable = f_cta_contable


                                                   IF v_existe_cuenta = 0 OR v_existe_cuenta IS NULL THEN 
                                                      CALL fn_mensaje("Error","La cuenta contable no existe, verifique","information")
                                                      NEXT FIELD f_cta_contable
                                                   ELSE 
                                                      --genera el registro manual
                                                      CALL fn_inserta_registro_contable()
                                                      CALL fn_inserta_reverso_contable(5) -- Ajuste operativo
                                                      CALL fn_mensaje("Información","Se ha agregado el nuevo registro contable.","information")

                                                      --Envía correo cada vez que se inserta un registro de ajuste operativo
                                                      CALL fn_envia_correo_cnt("Reverso Operativo de Contabilidad ", "Ajuste Operativo")

                                                      
                                                      CALL f_forma.setElementHidden("gr_nuevos_registros",FALSE)
                                                      CALL g_arr_inf_poliza_contable.clear()
                                                      CALL fn_muestra_registros_manuales()

                                                      --Limpia campos de la forma
                                                      LET f_cta_contable = NULL
                                                      LET cod_naturaleza_mov = NULL
                                                      LET f_cargo = NULL 
                                                      --CALL f_forma.setElementHidden("gr_nuevos_registros",FALSE)
                                                      DISPLAY ARRAY g_arr_inf_poliza_contable TO src_inf_pol_cont.* ATTRIBUTES (CANCEL = FALSE )
                                                         ON ACTION eliminar 

                                                            CALL arr_curr() RETURNING v_ind_current

                                                            CALL fn_ventana_confirma("Informacion","¿Está seguro que desea dar de baja este registro nuevo?","Información") 
                                                               RETURNING v_bnd_confirma

                                                            DISPLAY "Confirmacion ", v_bnd_confirma
                                                            IF v_bnd_confirma = 1 THEN 
                                                               IF v_ind_current >0 THEN 
                                                                  CALL fn_elimina_registro_contable(v_ind_current)
                                                                  CALL g_arr_inf_poliza_contable.clear()
                                                                  CALL fn_muestra_registros_manuales()
                                                               ELSE 
                                                                  CALL fn_mensaje("Error","Seleccione un registro de ajuste a eliminar","information")
                                                               END IF
                                                            --ELSE 
                                                            
                                                            END IF 
                                                         ON ACTION salir 
                                                            IF fn_valida_montos_contables() = FALSE THEN --Si los montos no coinciden 
                                                               CALL fn_mensaje("Información","Los importes de la póliza contable aun no coinciden. Verifique.","information")
                                                            ELSE 
                                                               EXIT PROGRAM 
                                                            END IF 
                                                            
                                                      
                                                      END DISPLAY 
                                                      
                                                   END IF 

                                                AFTER INPUT 
                                                   --Limpia campos de la forma
                                                   LET f_cta_contable = NULL
                                                   LET cod_naturaleza_mov = NULL
                                                   LET f_cargo = NULL 
                                                   
                                                ON ACTION cancelar 
                                                   EXIT PROGRAM 
                                                
                                             END INPUT 
                                          END DIALOG 
                                          
                                    END DISPLAY
                                    
                           ELSE 
                              --muestra mensaje de error
                              CALL fn_mensaje("INFORMACIÓN","No existe información para ese folio.","info")
                           END IF 
                     END IF 

                     IF v_cmb_reverso = 3 THEN --Si se elige la opción de reversar la póliza para volver a generarla

                        --Llena el arreglo con la información correspondiente
                        CALL fn_muestra_info_poliza_contable()

                        IF g_arr_inf_poliza_contable.getLength() > 0 THEN
                           CALL f_forma.setElementHidden("gr_nuevos_registros",FALSE)
                        ELSE 
                           CALL fn_mensaje("Información","No existe información de este folio para regenerar la póliza contable.","info")
                           --NEXT FIELD f_folio_liquida

                           LET r_bandera_reg_cnt = 0
                           CALL fn_ventana_confirma ("Pregunta", 
                                 "¿Desea realizar el registro contable para este folio?", 
                                 "quest")
                           RETURNING r_bandera_reg_cnt

                           IF r_bandera_reg_cnt = 1 THEN  -- Si acepta
                              --Verificar si el folio está liquidado o no en glo_folio
                              LET v_consulta_est =    "\n SELECT status ",
                                                      "\n FROM glo_folio ",
                                                      "\n WHERE folio = ", f_folio_liquida 

                              PREPARE prp_status FROM v_consulta_est
                              EXECUTE prp_status INTO v_status_liq 

                              DISPLAY "Estado liquidación: ", v_status_liq

                              #Estados de acuerdo a cat_edo_archivo
                              --1 CARGADO
                              --2 INTEGRADO
                              --3 REVERSADO
                              
                              IF  v_status_liq = 2 THEN 
                                 CALL fn_regenera_registros_contables()
                                 CALL fn_inserta_reverso_contable(7) --Este estado no existe en el catálogo
                                 CALL fn_envia_correo_cnt("Reverso Operativo de Contabilidad", "Regeneración de póliza")
                                 
                                 
                                 --Verificar cuadre del registro contable con fn_revisa_reg_cnt
                                 --si NO cuadra mostrar mensaje y estado = 100
                                 PREPARE prp_reverso_revisa_reg_cnt FROM "EXECUTE PROCEDURE fn_revisa_reg_cnt(?,?,?)"
                                 EXECUTE prp_reverso_revisa_reg_cnt USING  f_folio_liquida,
                                                                           p_f_liquida, 
                                                                           v_tpo_reg_cnt
                                 INTO v_bandera_rev

                                 DISPLAY "v_bandera_rev: ", v_bandera_rev
                                 --0 = correcto
                                 --1 = No existe monto abono registro contable
                                 --2 = No existe monto cargo registro contable
                                 --3 = Diferencia importes abono - cargo
                                 IF v_bandera_rev = 1 THEN 
                                    CALL fn_mensaje("Error","No existe monto abono para el registro contable","info")
                                 END IF 
                                 IF v_bandera_rev = 2 THEN 
                                    CALL fn_mensaje("Error","No existe monto cargo para el registro contable","info")
                                 END IF
                                 IF v_bandera_rev = 3 THEN 
                                    CALL fn_mensaje("Error","Existe diferencia entre los importes de Cargo y Abono","info")
                                 END IF  

                                 DISPLAY "Muestra!!"
                                 --Muestra información del registro contable para el folio dado
                                 CALL g_arr_inf_poliza_contable.clear() 
                                 CALL fn_muestra_info_poliza_contable()
                                 CALL f_forma.setElementHidden("gr_nuevos_registros",FALSE)
                                 
                                 DISPLAY ARRAY g_arr_inf_poliza_contable TO src_inf_pol_cont.* ATTRIBUTES (ACCEPT = FALSE)

                                       ON ACTION CANCEL 
                                          EXIT PROGRAM 
                                 END DISPLAY 
                                 
                              ELSE
                                 --muestra mensaje de error
                                 CALL fn_mensaje("INFORMACIÓN","No es posible realizar el registro contable, el folio no se encuentre liquidado.","info")
                                 EXIT PROGRAM 
                              END IF 
                              
                           ELSE 
                              CALL fn_mensaje("Información","Se ha cancelado el registro contable ","info")
                              NEXT FIELD f_folio_liquida
                           END IF 
                        END IF 
                        
                        
                        DISPLAY ARRAY g_arr_inf_poliza_contable TO src_inf_pol_cont.* ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
                           BEFORE DISPLAY 
                              SELECT DISTINCT estado 
                              INTO v_estado_reg
                              FROM cnt_transaccion
                              WHERE folio_liquida = f_folio_liquida
                              AND estado IN (10,90,100)  --Solo se podrá regenerar la póliza cuando este estado sea 10 ó 100
                              AND tpo_transaccion = 0

                           ON ACTION cancelar 
                              EXIT PROGRAM 
                              
                           ON ACTION regenerar
                              IF v_estado_reg = 10 OR v_estado_reg = 90 OR v_estado_reg = 100 THEN 

                                 CALL fn_regenera_registros_contables()
                                 
                                 CALL fn_inserta_reverso_contable(7) --Este estado no existe en el catálogo

                                 CALL fn_envia_correo_cnt("Reverso Operativo de Contabilidad", "Regeneración de póliza")


                                 --Verificar cuadre del registro contable con fn_revisa_reg_cnt
                                 --si NO cuadra mostrar mensaje y estado = 100
                                 PREPARE prp_reverso_revisa_reg_cnt2 FROM "EXECUTE PROCEDURE fn_revisa_reg_cnt(?,?,?)"
                                 EXECUTE prp_reverso_revisa_reg_cnt2 USING  f_folio_liquida,
                                                                           p_f_liquida, 
                                                                           v_tpo_reg_cnt
                                 INTO v_bandera_rev

                                 DISPLAY "v_bandera_rev: ", v_bandera_rev
                                 --0 = correcto
                                 --1 = No existe monto abono registro contable
                                 --2 = No existe monto cargo registro contable
                                 --3 = Diferencia importes abono - cargo
                                 IF v_bandera_rev = 1 THEN 
                                    CALL fn_mensaje("Error","No existe monto abono para el registro contable","info")
                                 
                                 END IF 
                                 IF v_bandera_rev = 2 THEN 
                                    CALL fn_mensaje("Error","No existe monto cargo para el registro contable","info")
                                 END IF
                                 IF v_bandera_rev = 3 THEN 
                                    CALL fn_mensaje("Error","Existe diferencia entre los importes de Cargo y Abono","info")
                                 END IF  

                                 
                                 CALL g_arr_inf_poliza_contable.clear() 
                                 CALL fn_muestra_info_poliza_contable()

                                 DISPLAY "4"
                                 DISPLAY ARRAY g_arr_inf_poliza_contable TO src_inf_pol_cont.* ATTRIBUTES (ACCEPT = FALSE)
                                    --BEFORE DISPLAY 
                                       --DISPLAY "INT FLAG: ", INT_FLAG
                                       --IF NOT INT_FLAG THEN
                                          --ACCEPT DISPLAY 
                                       --END IF 
                                       ON ACTION CANCEL 
                                          EXIT PROGRAM 
                                 END DISPLAY 

                                 
                              
                              ELSE
                                 --muestra mensaje de error
                                 CALL fn_mensaje("Información","No se puede regenerar la póliza para este folio. Verifique el estado.","info")
                              END IF 
                        
                        END DISPLAY 
                     END IF 
                     
                     IF v_cmb_reverso = 4 THEN --Si se elige la opción de no enviar la póliza
                        CALL g_arr_inf_poliza_contable.clear() 
                        CALL fn_muestra_info_poliza_contable()
                        IF g_arr_inf_poliza_contable.getLength() > 0 THEN
                           CALL f_forma.setElementHidden("gr_nuevos_registros",FALSE)
                        ELSE 
                           CALL fn_mensaje("Información","No existe información de para este folio de liquidación.","info")
                           NEXT FIELD f_folio_liquida
                        END IF 
                        
                        
                        DISPLAY ARRAY g_arr_inf_poliza_contable TO src_inf_pol_cont.* ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE )
                           BEFORE DISPLAY 
                              SELECT DISTINCT estado 
                              INTO v_estado_reg
                              FROM cnt_transaccion
                              WHERE folio_liquida = f_folio_liquida
                              AND estado IN (10) --Solo se podrá cambiar el estado cuando sea 10

                           
                           ON ACTION cancelar 
                              EXIT PROGRAM 

                           ON ACTION cambiar
                              IF v_estado_reg = 10 THEN 
                                 DISPLAY "---- En 10"
                                 CALL fn_cambia_estado_contable()
                                 CALL fn_inserta_reverso_contable(3) -- Estado registrado a rechazado
                                 CALL fn_envia_correo_cnt("Reverso Operativo de Contabilidad", "No enviar la póliza")
                                 CALL g_arr_inf_poliza_contable.clear() 
                                 CALL fn_muestra_info_poliza_contable()

                                 DISPLAY ARRAY g_arr_inf_poliza_contable TO src_inf_pol_cont.* ATTRIBUTES (CANCEL = FALSE)
                                    BEFORE DISPLAY 
                                       DISPLAY "INT FLAG: ", INT_FLAG
                                       IF NOT INT_FLAG THEN
                                          ACCEPT DISPLAY 
                                       END IF 
                                 END DISPLAY
                              ELSE
                                 --muestra mensaje de error
                                 CALL fn_mensaje("Información","No se puede cambiar el estado de este folio. Verifique el estado.","info")
                              END IF 
                              
                           CONTINUE DISPLAY 
                        END DISPLAY 
                     END IF 

                     --Agregar función de correo 
                     
                  END IF 
                          
       END INPUT 
      
   END DIALOG 
   
   CLOSE WINDOW wMain
END MAIN 


--Función para obtener las cuentas contables, folios y documentos contables 
FUNCTION fn_valida_confirmacion_poliza() 
   DEFINE
      consulta          STRING,
      indx              INTEGER

      LET consulta =   "\n SELECT T.f_liquida, T.cod_proceso_cnt || '-' || PC.desc_proceso_cnt Proceso, T.folio_liquida, T.folio_cnt,",
                         "\n        P.num_poliza, T.estado || '-' || E.desc_estado_cnt Estado ",
                         "\n FROM cnt_transaccion T INNER JOIN cat_estado_cnt E ",
                         "\n ON E.cod_estado_cnt = T.estado ",
                         "\n INNER JOIN cat_proceso_cnt PC ",
                         "\n ON T.cod_proceso_cnt = PC.cod_proceso_cnt",
                         "\n LEFT OUTER JOIN cnt_ctr_proceso P ",
                         "\n ON T.folio_liquida = P.folio_liquida  ",
                         "\n WHERE T.folio_liquida is not null ",
                         "\n AND T.estado in (60)"
       
      LET consulta = consulta || "\n AND T.folio_liquida = ", f_folio_liquida
           
      LET consulta = consulta, "\n GROUP BY 1,3,2,4,5,6 ORDER BY 1,3,2"
      
      DISPLAY "La consulta:  \n ", consulta
                          
      LET indx = 1
      DECLARE valida_proceso CURSOR FROM consulta 
      FOREACH valida_proceso  INTO v_arr_valida_proceso[indx].tb_f_liquida,
                                 v_arr_valida_proceso[indx].tb_proceso,  
                                 v_arr_valida_proceso[indx].tb_folio_liquida, 
                                 v_arr_valida_proceso[indx].tb_folio,
                                 v_arr_valida_proceso[indx].tb_doc_contable,
                                 v_arr_valida_proceso[indx].tb_estado 
    
                                 
         IF v_arr_valida_proceso[indx].tb_folio IS  NULL THEN 
            LET v_arr_valida_proceso[indx].tb_folio = 0
         END IF 

         IF v_arr_valida_proceso[indx].tb_doc_contable IS  NULL THEN 
            LET v_arr_valida_proceso[indx].tb_doc_contable = 0
         END IF
                                 
        LET indx = indx + 1
    END FOREACH

    
    CALL v_arr_valida_proceso.deleteElement(v_arr_valida_proceso.getLength())


    DISPLAY "El index de la consulta -- ", indx
    
    RETURN indx, v_arr_valida_proceso[1].tb_estado
    
    
END FUNCTION

--Muestra los registros contables que se dan de alta de forma manual para el folio de liquidación 
FUNCTION fn_muestra_registros_manuales()

   DEFINE 
      v_query_info      STRING,
      v_id_det1         INTEGER,
      v_ret             SMALLINT 

   LET v_suma_cargos = 0
   LET v_suma_abonos = 0

   --Limpia el arreglo previo a utilizar
   CALL g_arr_inf_poliza_contable.clear()

   LET v_query_info =  "\n SELECT T.f_liquida, ",
                 "\n	    T.folio_liquida, ",
                 "\n	    T.folio_cnt, ",
                 "\n	    T.cod_proceso_cnt || '-' || P.desc_proceso_cnt Proceso, ",
                 "\n	    T.cta_contable, ",
                 "\n  	 C.desc_cta_contable, ",
                 "\n	    T.importe , ", --Cargo
                 "\n	    0, ", --Abono
                 "\n	    T.estado || '-' || E.desc_estado_cnt Estado ",
                 "\n   FROM cnt_transaccion T, ",
                 "\n	    cat_proceso_cnt P, ",
                 "\n	    cat_cuenta_contable C, ",
                 "\n	    cat_estado_cnt E ",
                 "\n  WHERE P.cod_proceso_cnt    = T.cod_proceso_cnt ",
                 "\n	AND C.cta_contable       = T.cta_contable ",
                 "\n	AND E.cod_estado_cnt     = T.estado ",
                 "\n	AND T.cod_naturaleza_cta = 2 ",
                 "\n	AND T.folio_liquida = ",f_folio_liquida,
                 "\n	AND T.tpo_transaccion = 2", --Los que se acaban de insertar
                 "\n UNION ",
                 "\n SELECT T.f_liquida, ",
                 "\n	    T.folio_liquida, ",
                 "\n	    T.folio_cnt, ",
                 "\n	    T.cod_proceso_cnt || '-' || P.desc_proceso_cnt Proceso, ",
                 "\n	    T.cta_contable, ",
                 "\n	    C.desc_cta_contable, ",
                 "\n	    0, ", --Cargo
                 "\n	    T.importe, ", --Abono
                 "\n	    T.estado || '-' || E.desc_estado_cnt Estado ",
                 "\n  FROM  cnt_transaccion T, ",
                 "\n	    cat_proceso_cnt P, ",
                 "\n	    cat_cuenta_contable C, ",
                 "\n	    cat_estado_cnt E ",
                 "\n  WHERE P.cod_proceso_cnt    = T.cod_proceso_cnt ",
                 "\n	AND C.cta_contable       = T.cta_contable ",
                 "\n	AND E.cod_estado_cnt     = T.estado ",
                 "\n	AND T.cod_naturaleza_cta = 1 ",
                 "\n	AND T.folio_liquida = ",f_folio_liquida,
                 "\n	AND T.tpo_transaccion = 2", --Los que se acaban de insertar
                 "\n ORDER BY 1,2,3" 

  DISPLAY "Consulta de detalle 1 -- ", v_query_info
  PREPARE prp_det_inf_pol_cont1 FROM v_query_info

  LET v_id_det1 = 1

  --Declara el cursor para la consulta
  DECLARE cur_pol_cont1 CURSOR FOR prp_det_inf_pol_cont1
  FOREACH cur_pol_cont1 INTO g_arr_inf_poliza_contable[v_id_det1].arr_po_fecha,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_folio_liquida,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_folio,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_proceso,        
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_cuenta,     
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_descripcion,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_cargo,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_abono,
                             g_arr_inf_poliza_contable[v_id_det1].arr_po_estado
                           
    LET v_id_det1 = v_id_det1 + 1
  END FOREACH  

  --Borra el último elemento vacío
  CALL g_arr_inf_poliza_contable.deleteElement(v_id_det1)
  LET v_id_det1 = v_id_det1 - 1

  --FOR v_counter = 1 TO v_id_det1 
      --LET v_suma_cargos = v_suma_cargos + g_arr_inf_poliza_contable[v_counter].arr_po_cargo
      --LET v_suma_abonos = v_suma_abonos + g_arr_inf_poliza_contable[v_counter].arr_po_abono
  --END FOR 

  --DISPLAY "v_suma_cargos: ",v_suma_cargos
  --DISPLAY "v_suma_abonos: ",v_suma_abonos
  --DISPLAY v_suma_cargos TO f_suma_cargos
  --DISPLAY v_suma_abonos TO f_suma_abonos

   CALL fn_valida_montos_contables()
      RETURNING v_ret
   
END FUNCTION 

--Realiza el rechazo del registro contable por una confirmación no exitosa
FUNCTION fn_rechaza_registro_contable()

   UPDATE cnt_transaccion
   SET estado = 10
   WHERE folio_liquida = f_folio_liquida;


END FUNCTION 

--Reversa la póliza manual y crea otra con los criterios indicados
FUNCTION fn_reversa_poliza_manual()

      
   --LET p_f_liquida = v_arr_valida_proceso[1].tb_f_liquida
   --LET p_folio_liquida = v_arr_valida_proceso[1].tb_folio_liquida
   LET v_estado = 10
   LET v_cod_transaccion_cnt = 0

    --obtiene cod_proceso y cod_proceso_cnt
    SELECT proceso_cod
    INTO p_cod_proceso
    FROM glo_folio
    WHERE folio = f_folio_liquida

    --Asigna valor al proceso contable y a la transaccion
    CALL fn_obtiene_proceso_cnt()
    --


    CALL fn_llena_det_inf_montos_op(v_cod_proceso_cnt)
    --Aquí se tiene que desplegar el arreglo para mostrar la info y después pedir los datos
    --para la póliza de manera manual
    
   
END FUNCTION

--Inserta el registro contable para hacer el ajuste manual
FUNCTION fn_inserta_registro_contable()
 DEFINE v_query_info      STRING
    {SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt
    FROM cnt_regla_contable 
    WHERE cta_contable = f_cta_contable
    AND cod_proceso = p_cod_proceso
    AND cod_proceso_cnt = v_cod_proceso_cnt}

    LET v_id_cuenta_contable = 0
    LET v_cod_transaccion_cnt = 0
    LET v_cod_subcta_cnt = 0
    
   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio_liquida,Sin INTO v_tbl_mov --por folio

    --traer de cta_movimiento la fecha
   LET v_query_info =   "SELECT DISTINCT f_liquida"
                    ,"\n FROM ",v_tbl_mov
                    ,"\n WHERE folio_liquida = ?  "
   PREPARE c_mov FROM v_query_info
   EXECUTE c_mov USING f_folio_liquida INTO p_f_liquida

   INSERT INTO cnt_transaccion 
   VALUES
      (v_id_cuenta_contable,
       0, --folio_cnt
       v_cod_proceso_cnt,
       p_cod_proceso,
       v_cod_transaccion_cnt,
       v_cod_subcta_cnt,
       f_cta_contable,
       cod_naturaleza_mov,
       f_folio_liquida,
       f_cargo,
       p_f_liquida,
       TODAY,
       2,   -- 0=Registro Contable, 1=Reverso, 2=Ajuste
       v_estado)

   DISPLAY "id: ",v_id_cuenta_contable
   DISPLAY "v_cod_proceso_cnt: ",v_cod_proceso_cnt
   DISPLAY "p_cod_proceso: ", p_cod_proceso
   DISPLAY "v_cod_transaccion_cnt: ", v_cod_transaccion_cnt
   DISPLAY "v_cod_subcta_cnt: ", v_cod_subcta_cnt
   DISPLAY "f_cta_contable: ", f_cta_contable
   DISPLAY "v_cod_naturaleza_cta: ", cod_naturaleza_mov
   DISPLAY "p_folio_liquida: ",f_folio_liquida
   DISPLAY "monto: ",f_cargo
   DISPLAY "fecha liquida: ",p_f_liquida
   
   

END FUNCTION   

--Función que llena el segundo detalle a partir de las fechas y folios de liquidación obtenidas en la consulta de la Póliza Contable
FUNCTION fn_llena_det_inf_montos_op(v_cod_pro_cnt) 
  DEFINE 
    v_query                  STRING,
    v_id_det1                INTEGER,
    v_flag                   BOOLEAN,
    v_counter                INTEGER,
    v_comp_query             STRING,
    v_liq                    INTEGER,
    v_cod_pro_cnt            SMALLINT 

   LET v_liq     = 1
   LET v_id_det1 = 1

  --Esta 
    --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio_liquida,Sin INTO v_tbl_mov --por folio
 
   LET v_query = "\nSELECT M.f_liquida, M.folio_liquida, ",
                   "\n       M.subcuenta || '-' || S.subcuenta_desc Subcuenta, ",
                   "\n	     M.movimiento || '-' || N.movimiento_desc Movimiento, ",
                   "\n       M.fondo_inversion || '-' || F.razon_social Fondo, ",
                   "\n       SUM (M.monto_pesos) pesos, SUM(M.monto_acciones) acciones",
                   "\n FROM  ",v_tbl_mov," M  ,cat_subcuenta  S,cat_movimiento N, cat_fondo_local F",
                   "\n       WHERE M.folio_liquida = ",f_folio_liquida,
                   "\n       AND M.subcuenta       = S.subcuenta ",
                   "\n       AND M.movimiento      = N.movimiento ",
                   "\n       AND M.fondo_inversion = F.fondo ",
                   "\n GROUP BY 1,2,3,4,5 "
                   

  DISPLAY "Consulta 2 -- ",v_query   
  PREPARE prp_det_inf_montos_op FROM v_query

  LET v_id_det1 = 1

  --Declara el cursor para la consulta
  DECLARE cur_montos_op CURSOR FOR prp_det_inf_montos_op
  FOREACH cur_montos_op INTO g_arr_inf_montos_op[v_id_det1].arr_op_fecha,
                             g_arr_inf_montos_op[v_id_det1].arr_op_folio,
                             g_arr_inf_montos_op[v_id_det1].arr_op_subcuenta,
                             g_arr_inf_montos_op[v_id_det1].arr_op_tipo,
                             g_arr_inf_montos_op[v_id_det1].arr_op_fondo,
                             g_arr_inf_montos_op[v_id_det1].arr_op_pesos,
                             g_arr_inf_montos_op[v_id_det1].arr_op_aivs

    IF g_arr_inf_montos_op[v_id_det1].arr_op_aivs IS NULL THEN
       LET g_arr_inf_montos_op[v_id_det1].arr_op_aivs = 0.00
    END IF 

    IF g_arr_inf_montos_op[v_id_det1].arr_op_pesos IS NULL THEN
       LET g_arr_inf_montos_op[v_id_det1].arr_op_pesos = 0.00
    END IF
                               
    LET v_id_det1 = v_id_det1 + 1
  END FOREACH 
      
  LET v_suma_pesos = 0
  LET v_suma_aivs  = 0

  --Borra el último elemento vacío
  CALL g_arr_inf_montos_op.deleteElement(v_id_det1)

  LET v_id_det1 = v_id_det1 - 1
     
  FOR v_counter = 1 TO v_id_det1
      LET v_suma_pesos = v_suma_pesos + g_arr_inf_montos_op[v_counter].arr_op_pesos
      LET v_suma_aivs  = v_suma_aivs  + g_arr_inf_montos_op[v_counter].arr_op_aivs
  END FOR 

  DISPLAY "SUMA AIVS ",v_suma_aivs
  DISPLAY "SUMA PESOS ",v_suma_pesos
      
  --RETURN v_id_det1, v_suma_pesos, v_suma_aivs

END FUNCTION 

--Llena el combo de la naturaleza de la cuenta
FUNCTION fn_llena_combo_naturaleza()

   DEFINE 
      v_naturaleza           STRING,
      cb_naturaleza          ui.combobox,
      v_ind_nat              INTEGER,
      v_cat_naturaleza     DYNAMIC ARRAY OF RECORD
            v_cod_naturaleza  SMALLINT, 
            v_descripcion     CHAR (5)
      END RECORD 

   #Se llena el combo de naturaleza
   LET cb_naturaleza = ui.combobox.forname("cod_naturaleza_mov")
   CALL cb_naturaleza.clear()
   LET v_ind_nat = 1
   
   LET v_naturaleza =   "SELECT cod_naturaleza_mov, desc_naturaleza_mov ",
                        "FROM cat_naturaleza_mov ",
                        "ORDER BY cod_naturaleza_mov"

   PREPARE exe_naturaleza FROM v_naturaleza
   DECLARE cur_naturaleza CURSOR FOR exe_naturaleza

   FOREACH cur_naturaleza INTO v_cat_naturaleza[v_ind_nat].v_cod_naturaleza,v_cat_naturaleza[v_ind_nat].v_descripcion 
      IF v_cat_naturaleza[v_ind_nat].v_cod_naturaleza IS NOT NULL THEN
         CALL cb_naturaleza.additem(v_cat_naturaleza[v_ind_nat].v_cod_naturaleza, v_cat_naturaleza[v_ind_nat].v_descripcion)
      END IF
      LET v_ind_nat = v_ind_nat + 1
   END FOREACH
   
   CLOSE cur_naturaleza
   FREE cur_naturaleza
END FUNCTION 

--Elimina el registro contable seleccionado
FUNCTION fn_elimina_registro_contable(v_indice)

   DEFINE 
      v_indice   INTEGER 

   DELETE FROM cnt_transaccion
   WHERE folio_liquida = f_folio_liquida
   AND f_liquida =  g_arr_inf_poliza_contable[v_indice].arr_po_fecha
   AND folio_cnt =  g_arr_inf_poliza_contable[v_indice].arr_po_folio
   AND cta_contable = g_arr_inf_poliza_contable[v_indice].arr_po_cuenta
   AND tpo_transaccion = 2
   AND estado = 10

   CALL fn_mensaje("Información", "Se ha eliminado el registro de ajuste","Information")


END FUNCTION 

--Muestra la información de la póliza contable
FUNCTION fn_muestra_info_poliza_contable()
   DEFINE 
      v_query_info      STRING,
      v_id_det1         INTEGER,
      v_cargo_cnt       DECIMAL (22,2),
      v_abono_cnt       DECIMAL (22,2)

   LET v_cargo_cnt = 0.00
   LET v_abono_cnt = 0.00
   
      
   LET v_query_info =  "\n SELECT T.f_liquida, ",
                 "\n	    T.folio_liquida, ",
                 "\n	    T.folio_cnt, ",
                 "\n	    T.cod_proceso_cnt || '-' || P.desc_proceso_cnt Proceso, ",
                 "\n	    T.cta_contable, ",
                 "\n  	 C.desc_cta_contable, ",
                 "\n	    T.importe , ", --Cargo
                 "\n	    0, ", --Abono
                 "\n	    T.estado || '-' || E.desc_estado_cnt Estado ",
                 "\n   FROM cnt_transaccion T, ",
                 "\n	    cat_proceso_cnt P, ",
                 "\n	    cat_cuenta_contable C, ",
                 "\n	    cat_estado_cnt E ",
                 "\n  WHERE P.cod_proceso_cnt    = T.cod_proceso_cnt ",
                 "\n	AND C.cta_contable       = T.cta_contable ",
                 "\n	AND E.cod_estado_cnt     = T.estado ",
                 "\n	AND T.cod_naturaleza_cta = 2 ",
                 "\n	AND T.folio_liquida = ",f_folio_liquida,
                 "\n UNION ",
                 "\n SELECT T.f_liquida, ",
                 "\n	    T.folio_liquida, ",
                 "\n	    T.folio_cnt, ",
                 "\n	    T.cod_proceso_cnt || '-' || P.desc_proceso_cnt Proceso, ",
                 "\n	    T.cta_contable, ",
                 "\n	    C.desc_cta_contable, ",
                 "\n	    0, ", --Cargo
                 "\n	    T.importe, ", --Abono
                 "\n	    T.estado || '-' || E.desc_estado_cnt Estado ",
                 "\n  FROM  cnt_transaccion T, ",
                 "\n	    cat_proceso_cnt P, ",
                 "\n	    cat_cuenta_contable C, ",
                 "\n	    cat_estado_cnt E ",
                 "\n  WHERE P.cod_proceso_cnt    = T.cod_proceso_cnt ",
                 "\n	AND C.cta_contable       = T.cta_contable ",
                 "\n	AND E.cod_estado_cnt     = T.estado ",
                 "\n	AND T.cod_naturaleza_cta = 1 ",
                 "\n	AND T.folio_liquida = ",f_folio_liquida,
                 "\n ORDER BY 1,2,3" 

  --DISPLAY "Consulta de detalle 1 -- ", v_query_info
  PREPARE prp_det_poliza_contable FROM v_query_info

  LET v_id_det1 = 1

  --Declara el cursor para la consulta
  DECLARE cur_poliza_contable CURSOR FOR prp_det_poliza_contable
  FOREACH cur_poliza_contable INTO g_arr_inf_poliza_contable[v_id_det1].arr_po_fecha,
                                   g_arr_inf_poliza_contable[v_id_det1].arr_po_folio_liquida,
                                   g_arr_inf_poliza_contable[v_id_det1].arr_po_folio,
                                   g_arr_inf_poliza_contable[v_id_det1].arr_po_proceso,        
                                   g_arr_inf_poliza_contable[v_id_det1].arr_po_cuenta,     
                                   g_arr_inf_poliza_contable[v_id_det1].arr_po_descripcion,
                                   g_arr_inf_poliza_contable[v_id_det1].arr_po_cargo,
                                   g_arr_inf_poliza_contable[v_id_det1].arr_po_abono,
                                   g_arr_inf_poliza_contable[v_id_det1].arr_po_estado



    IF g_arr_inf_poliza_contable[v_id_det1].arr_po_cargo IS NOT NULL THEN 
      LET v_cargo_cnt = v_cargo_cnt + g_arr_inf_poliza_contable[v_id_det1].arr_po_cargo
    END IF 
    IF g_arr_inf_poliza_contable[v_id_det1].arr_po_abono IS NOT NULL THEN 
      LET v_abono_cnt = v_abono_cnt + g_arr_inf_poliza_contable[v_id_det1].arr_po_abono
    END IF 
    
    LET v_id_det1 = v_id_det1 + 1
  END FOREACH  

  --Borra el último elemento vacío
  CALL g_arr_inf_poliza_contable.deleteElement(v_id_det1)
  LET v_id_det1 = v_id_det1 - 1

  DISPLAY v_cargo_cnt TO v_suma_cargos
  DISPLAY v_abono_cnt TO v_suma_abonos
END FUNCTION 

--Regresa el proceso contable y la transaccion asignada
FUNCTION fn_obtiene_proceso_cnt()

   

   --Obtiene el proceso_cnt
    CASE 
         -- retiro fondo de ahorro
         WHEN ( p_cod_proceso = 1503 )
            LET v_cod_transaccion_cnt = 3
            LET v_cod_proceso_cnt     = 3

         -- retiro solo infonavit por webservice
         WHEN ( p_cod_proceso = 1501 )
            LET v_cod_transaccion_cnt = 36
            LET v_cod_proceso_cnt     = 30

         -- retiro solo infonavit por flujo contingente
         WHEN ( p_cod_proceso = 1517 )
            LET v_cod_transaccion_cnt = 36
            LET v_cod_proceso_cnt     = 30
            
         -- ACLARACIONES 101 y 102
         WHEN ( p_cod_proceso = 101 OR p_cod_proceso = 102 )
            LET v_cod_transaccion_cnt = 17
            LET v_cod_proceso_cnt     = 17
            
         -- ACLARACIONES 103 y 104
         WHEN ( p_cod_proceso = 103 OR p_cod_proceso = 104 )
            LET v_cod_transaccion_cnt = 18
            LET v_cod_proceso_cnt     = 17

         -- ACLARACIONES 105 y 107
         WHEN ( p_cod_proceso = 105 OR p_cod_proceso = 107 )
            LET v_cod_transaccion_cnt = 19
            LET v_cod_proceso_cnt     = 17
         
         -- 902 AVANCE DE PAGOS
         WHEN ( p_cod_proceso = 902 )
            LET v_cod_transaccion_cnt = 20
            LET v_cod_proceso_cnt     = 18
            
         -- 903 CANCELACION AVANCE DE PAGOS
         WHEN ( p_cod_proceso = 903 )
            LET v_cod_transaccion_cnt = 62
            LET v_cod_proceso_cnt     = 44

         -- 905 CANCELACION AVANCE DE PAGOS
         WHEN ( p_cod_proceso = 905 )
            LET v_cod_transaccion_cnt = 22
            LET v_cod_proceso_cnt     = 45
            
         -- 222 AFECTACIÓN CTA IND DEV SDOS EXC ACR
         -- 318 LIQUIDACIÓN DEVOLUCIÓN SALDOS EXC AGR
         -- 1224 LIQUIDACIÓN DEVOLUCIÓN SALDOS EXC GRT
         WHEN ( p_cod_proceso = 222 OR p_cod_proceso = 318 OR p_cod_proceso = 1224 )
            --Desaparece proceso contable 12 y 23            
            LET v_cod_proceso_cnt = 22
                            
         -- 220 AFECTACIÓN CTA IND TRANSF ACREDITADOS
         WHEN ( p_cod_proceso = 220 )
            LET v_cod_proceso_cnt     = 2
			
            LET v_cod_proceso_cnt     = 20
         
         -- 1402 REGISTRO DE PAGOS SAR92
         WHEN ( p_cod_proceso = 1402 )
            LET v_cod_proceso_cnt     = 5
            
         -- 1701 TRASPASO INFONAVIT AFORE
         WHEN ( p_cod_proceso = 1701 )
            LET v_cod_proceso_cnt     = 6
            
         -- 2101 CONCILIACION BDNSVIV
         WHEN ( p_cod_proceso = 2101 )
            LET v_cod_proceso_cnt     = 7
            
         -- 1505 RETIRO TIPO N
         WHEN ( p_cod_proceso = 1505 )
            LET v_cod_proceso_cnt     = 8
            
         -- 801 DEVOLUCIÓN AL INFONAVIT POR APLICACIONES
         WHEN ( p_cod_proceso = 801 ) --Se quita el proceso_cnt 11 y se deja sólo el 26
            LET v_cod_proceso_cnt     = 26
            
         -- 1502 RETIROS POR DISPOSICION DE RECURSOS
         WHEN ( p_cod_proceso = 1502 )
            LET v_cod_proceso_cnt     = 14
            
         -- 1401 REGISTRO DE PAGOS LQINFO
         WHEN ( p_cod_proceso = 1401 )
            LET v_cod_proceso_cnt     = 15
            
         -- 1403 REGISTRO DE PAGOS Sólo Infonavit
         WHEN ( p_cod_proceso = 1403 )
            LET v_cod_proceso_cnt     = 16
            
         -- 901 DISPERSIÓN DE PAGOS
         WHEN ( p_cod_proceso = 901 )
            LET v_cod_proceso_cnt     = 19
            
         -- 1001 DEVOLUCIÓN DE PAGOS INDEBIDOS O EN EXCES
         WHEN ( p_cod_proceso = 1001 )
            LET v_cod_proceso_cnt     = 27
            
         -- 1002 DEVOLUCIÓN DE PAGOS INDEBIDOS COMPLEMENTARIO
         WHEN ( p_cod_proceso = 1002 )
            LET v_cod_proceso_cnt     = 27
            
         -- 1003 DEVOLUCIÓN DE PAGOS INDEBIDOS O EN EXCES
         WHEN ( p_cod_proceso = 1003 )
            LET v_cod_proceso_cnt     = 27
            
         -- 1003 DEVOLUCION PAGOS INDEBIDOS INFONAVIT
         WHEN ( p_cod_proceso = 1005 )
            LET v_cod_proceso_cnt     = 28
            
         -- 1504 RETIROS POR TRANSFERENCIA
         WHEN ( p_cod_proceso = 1504 )
            LET v_cod_proceso_cnt     = 29
            
         -- 1217 LIQUIDACIÓN USO GARANTÍA 43BIS
         WHEN ( p_cod_proceso = 1217 )
            LET v_cod_proceso_cnt     = 32
            
         -- 312 LIQUIDACIÓN ANUALIDAD GARANTIZADA
         WHEN ( p_cod_proceso = 312 )
            LET v_cod_proceso_cnt     = 34
            
         -- 2202 SEPARACION DE CUENTAS
         WHEN ( p_cod_proceso = 2202 )
            LET v_cod_proceso_cnt     = 36

         -- 2301 UNIFICACIÓN DE CUENTAS IMSS
         WHEN ( p_cod_proceso = 2301 )
            LET v_cod_proceso_cnt     = 37
            
         -- 2302 UNIFICACIÓN DE CUENTAS INFONAVIT 
         WHEN ( p_cod_proceso = 2302 )
            LET v_cod_proceso_cnt     = 37
            
         -- 1405 FORTALECIMIENTO DE CRÉDITO
         WHEN ( p_cod_proceso = 1405 )
            LET v_cod_proceso_cnt     = 42
            
         -- 1507 RETIROS FORTALECIMIENTO DE CRÉDITO
         WHEN ( p_cod_proceso = 1507 )
            LET v_cod_proceso_cnt     = 43
            
         -- 1515 RETIROS FONDO AHORRO CONTINGENTE
         WHEN ( p_cod_proceso = 1515 )
            LET v_cod_transaccion_cnt = 3
            LET v_cod_proceso_cnt     = 46
            
         -- 1516 RETIROS LEY 73 CONTINGENTE
         WHEN ( p_cod_proceso = 1516 )
            LET v_cod_transaccion_cnt = 58
            LET v_cod_proceso_cnt     = 47
            
         -- 1517 RETIROS SOLO INFONAVIT CONTINGENTE
         WHEN ( p_cod_proceso = 1517 )
            LET v_cod_transaccion_cnt = 36
            LET v_cod_proceso_cnt     = 48
                            
         -- 1519 RETIROS POR DISPOSICION PMG
         WHEN ( p_cod_proceso = 1519 )
            LET v_cod_proceso_cnt     = 49
            
         -- 2230 SEPARACION DE CUENTAS SOLO INFONAVIT
         WHEN ( p_cod_proceso = 2230 )
            LET v_cod_proceso_cnt     = 50

         -- 1407 REG PAGOS APORTACIONES VOLUNTARIAS
         WHEN ( p_proceso_cod = 1407 )
            LET v_cod_transaccion_cnt = 64
            LET v_cod_proceso_cnt     = 52
            
      END CASE

END FUNCTION

--Elimina los registros contables y los vuelve a generar
FUNCTION  fn_regenera_registros_contables()
   DEFINE 
       v_si_resultado    INTEGER
      ,v_query_evalua    STRING
      ,v_query_info      STRING
      

   --Por omisión, la transacción contable es cero
   LET v_cod_transaccion_cnt = 0
   
   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio_liquida,Sin INTO v_tbl_mov --por folio

    --traer de cta_movimiento la fecha
   LET v_query_info =   "SELECT DISTINCT f_liquida"
                    ,"\n FROM ",v_tbl_mov
                    ,"\n WHERE folio_liquida = ?  "
   PREPARE c_mov2 FROM v_query_info
   EXECUTE c_mov2 USING f_folio_liquida INTO p_f_liquida

   --Elimina los registros contables que actualmente existen para ese folio
   CALL fn_elimina_registros_actuales()
   
   --obtiene cod_proceso y cod_proceso_cnt
   SELECT proceso_cod
   INTO p_cod_proceso
   FROM glo_folio
   WHERE folio = f_folio_liquida

   CALL fn_obtiene_proceso_cnt()
   
   --DISPLAY "v_cod_transaccion_cnt: ", v_cod_transaccion_cnt
   --DISPLAY "f_folio_liquida: ",f_folio_liquida
   --DISPLAY "p_f_liquida: ",p_f_liquida
   DISPLAY "v_cod_proceso_cnt: ",v_cod_proceso_cnt
   DISPLAY "p_cod_proceso: ",p_cod_proceso
   --DISPLAY "v_cod_transaccion_cnt: ",v_cod_transaccion_cnt
   
   CALL fn_registro_contable(f_folio_liquida,p_f_liquida,v_cod_proceso_cnt,
              p_cod_proceso,v_cod_transaccion_cnt)
   RETURNING v_si_resultado

   LET v_query_evalua = "\nSELECT FIRST 1 estado",
                        "\nFROM cnt_transaccion ",
                        "\nWHERE folio_liquida = ",f_folio_liquida
                        
   PREPARE prp_evalua_cnt FROM v_query_evalua
   EXECUTE prp_evalua_cnt INTO v_si_resultado

   
   -- se verifica si LA EJECUCION FUE CORRECTA ( 1 = CORRECTO )
   IF ( v_si_resultado IS NOT NULL) THEN
      CALL fn_mensaje("Información", "El registro contable se realizó exitosamente.","Info")
   ELSE
       CALL fn_mensaje("Error","ERROR: El registro contable no se pudo realizar.","Info")
   END IF

   
      
END FUNCTION 

--Elimina los registros contables de la póliza para un folio dado
FUNCTION fn_elimina_registros_actuales()

   DELETE FROM cnt_transaccion
   WHERE folio_liquida = f_folio_liquida

END FUNCTION 


-- FUNCION QUE INVOCA LA EJECUCION DEL REGISTRO CONTABLE
FUNCTION fn_registro_contable(v_folio,v_fecha_liquida,v_cod_proceso_cnt,
                              v_proceso_cod,v_cod_transaccion_cnt)
DEFINE v_folio                DECIMAL(9,0)-- Folio de liquidación del proceso
DEFINE v_fecha_liquida        DATE-- Fecha de liquidación del proceso
DEFINE v_cod_proceso_cnt      SMALLINT-- Código de proceso contable
DEFINE v_proceso_cod          SMALLINT-- Código Proceso
DEFINE v_cod_transaccion_cnt  SMALLINT-- Código de transaccion
DEFINE v_si_resultado         SMALLINT--Valor De Retorno
DEFINE v_cadena               STRING

      -- se invoca el stored procedure
      LET v_cadena = "EXECUTE PROCEDURE fn_identifica_proceso_cnt(?,?,?,?,?)"
      PREPARE sid_identifica_proceso_cnt FROM v_cadena
      EXECUTE sid_identifica_proceso_cnt
            USING  v_folio              , -- Folio de liquidación del proceso
                   v_fecha_liquida      , -- Fecha de liquidación del proceso
                   v_cod_proceso_cnt    , -- Código de proceso contable
                   v_proceso_cod        , -- Código Proceso
                   v_cod_transaccion_cnt  -- código de transacción
            INTO   v_si_resultado

      RETURN v_si_resultado

END FUNCTION

--Actualiza el estado del registro contable para que la póliza no sea generada
FUNCTION fn_cambia_estado_contable()

   UPDATE cnt_transaccion
   SET estado = 90
   WHERE folio_liquida = f_folio_liquida

   CALL fn_mensaje("Información","Se ha actualizado el estado a la póliza contable","Info")

END FUNCTION 


--Valida que los cargos y abonos sean los mismos
FUNCTION fn_valida_montos_contables()

   SELECT SUM (importe)
   INTO v_suma_cargos
   FROM cnt_transaccion
   WHERE folio_liquida = f_folio_liquida
   AND cod_naturaleza_cta = 2 --Cargo

   SELECT SUM (importe)
   INTO v_suma_abonos
   FROM cnt_transaccion
   WHERE folio_liquida = f_folio_liquida
   AND cod_naturaleza_cta = 1 --Abono

   DISPLAY "Cargos: ", v_suma_cargos
   DISPLAY "Abonos: ", v_suma_abonos
   DISPLAY BY NAME v_suma_cargos,v_suma_abonos

   IF v_suma_cargos = v_suma_abonos THEN 
      RETURN TRUE --Si concuerdan, es un resultado exitoso
   ELSE 
      RETURN FALSE 
   END IF 
   

END FUNCTION 

FUNCTION fn_envia_correo_cnt(v_titulo, v_mensaje)

   DEFINE 
      v_pid             LIKE bat_ctr_proceso.pid,
      v_proceso_cod     LIKE bat_ctr_proceso.proceso_cod,
      v_opera_cod       LIKE cat_operacion.opera_cod,
      v_titulo          STRING,
      v_mensaje         STRING,
      v_consulta        STRING  

   LET v_consulta =  "\n SELECT first 1 ",
                     "\n P.pid, P.proceso_cod, opera_cod ",  
                     "\n FROM bat_ctr_proceso P, bat_ctr_operacion O ",
                     "\n WHERE P.folio = ", f_folio_liquida,
                     "\n AND P.pid = O.pid ",
                     "\n ORDER by opera_cod desc"

   PREPARE prp_consulta_info_proceso FROM v_consulta
   EXECUTE prp_consulta_info_proceso INTO v_pid, v_proceso_cod, v_opera_cod

   DISPLAY "v_pid ", v_pid
   DISPLAY "v_proceso_cod ", v_proceso_cod
   DISPLAY "v_opera_cod ", v_opera_cod


   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(v_pid, v_proceso_cod, v_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          v_titulo,
                          v_mensaje)

END FUNCTION 

--Realiza un insert a la tabla de la bitácora de reversos contables
FUNCTION fn_inserta_reverso_contable(p_tpo_reverso)


   DEFINE 
      p_tpo_reverso          SMALLINT,
      p_folio_cnt            DECIMAL (9,0),
      --p_proceso_cod          SMALLINT,
      p_f_liquida            DATE,
      p_f_emision            DATE,
      p_estado               SMALLINT,
      p_usuario              CHAR(20),
      f_ejecuta              DATE,
      h_ejecuta              DATETIME HOUR TO SECOND
     ,v_query_info      STRING


   
   LET p_usuario = g_usuario
   LET p_estado = 0
    
   --obtiene cod_proceso y cod_proceso
   SELECT proceso_cod
   INTO p_proceso_cod
   FROM glo_folio
   WHERE folio = f_folio_liquida


   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio_liquida,Sin INTO v_tbl_mov --por folio

   --Obtiene fecha de liquidación
   --traer de cta_movimiento la fecha
   LET v_query_info =   "SELECT DISTINCT f_liquida"
                    ,"\n FROM ",v_tbl_mov
                    ,"\n WHERE folio_liquida = ?  "
   PREPARE c_mov3 FROM v_query_info
   EXECUTE c_mov3 USING f_folio_liquida INTO p_f_liquida

   --obtiene fecha de emisión de la tabla de control
   SELECT DISTINCT f_liquida, estado, folio_cnt
   INTO p_f_emision, p_estado, p_folio_cnt
   FROM cnt_transaccion
   WHERE folio_liquida = f_folio_liquida 
   --AND tpo_transaccion = 0


   DISPLAY " fn_inserta_reverso_contable: "
   DISPLAY "p_proceso_cod: ", p_proceso_cod
   DISPLAY "f_folio_liquida: ", f_folio_liquida
   DISPLAY "p_f_liquida: ", p_f_liquida
   LET p_cod_proceso = p_proceso_cod
   
   --Obtiene el proceso cnt en la variable v_cod_proceso_cnt
   CALL fn_obtiene_proceso_cnt()
   
   LET f_ejecuta = TODAY
   LET h_ejecuta = CURRENT HOUR TO SECOND
   
   INSERT INTO cnt_ctr_reverso
   VALUES (p_tpo_reverso,
            p_folio_cnt,
            f_folio_liquida,
            v_cod_proceso_cnt,
            p_proceso_cod,
            p_f_liquida,
            p_f_emision,
            p_estado,
            p_usuario,
            f_ejecuta,
            h_ejecuta)

END FUNCTION 