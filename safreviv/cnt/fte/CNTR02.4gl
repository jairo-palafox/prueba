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


DATABASE
   safre_viv
GLOBALS "CNTG01.4gl"

GLOBALS

   DEFINE
      f_folio_cnt      LIKE cnt_transaccion.folio_cnt

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
       p_pid            LIKE bat_ctr_proceso.pid, -- ID del proceso
       p_proceso_cod    LIKE cat_proceso.proceso_cod, -- Código del proceso
       p_opera_cod      LIKE cat_operacion.opera_cod -- Código de operacion

    
END GLOBALS


MAIN
   DEFINE
      f_ventana     ui.Window,   -- Define las propìedades de la Ventana
      f_forma       ui.Form,     -- Define las propiedades de la forma
      f_fecha       DATE,
      v_registros   INTEGER,
      v_respuesta   SMALLINT,
      r_bandera_rev SMALLINT  

   DEFINE 
      v_ruta_ejecutable  LIKE seg_modulo.ruta_bin, -- Ruta del ejecutable
      v_ruta_listados    LIKE seg_modulo.ruta_listados, -- Rut del log
      v_s_mensaje        STRING,
      l_comando          STRING 

   DEFINE 
      f_folio_confirmacion   DECIMAL (9,0)
   
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

   CLOSE WINDOW SCREEN
   OPEN WINDOW wMain WITH FORM "CNTR021"
   DIALOG ATTRIBUTES (UNBUFFERED)
       INPUT BY NAME f_folio_cnt
          BEFORE INPUT          

            
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()
            CALL f_forma.setElementHidden("grp_valida_proceso",TRUE)
            
             
             ON ACTION ACCEPT

                  IF (f_folio_cnt IS NULL) THEN
                     CALL fn_mensaje("Error","Ingrese al menos un criterio de búsqueda","information")
                     NEXT FIELD f_folio_cnt
                  END IF 

                  CALL f_forma.setElementHidden("grp_valida_proceso",FALSE)
                  CALL v_arr_valida_proceso.clear()

                  --Realiza la consulta por proceso y/o rango de periodo
                  CALL fn_valida_confirmacion_poliza() RETURNING v_registros  

                   IF v_registros <= 1 THEN
                     CALL fn_mensaje("INFORMACIÓN","No existe información con ese criterio de búsqueda.","info")
                     CALL v_arr_valida_proceso.clear()
                     CALL f_forma.setElementHidden("grp_valida_proceso",TRUE)
                     --CALL DIALOG.setActionHidden("reporte",1)
                     NEXT FIELD f_folio_cnt
                  END IF 


                  --Despliega la consulta en la tabla
                  DISPLAY ARRAY v_arr_valida_proceso TO scr_valida_procesos.* ATTRIBUTES (ACCEPT = FALSE)

                      ON ACTION CANCEL 
                           EXIT DISPLAY 
       
                      ON ACTION reverso
                           --Ejecuta el reverso por batch
                           CALL fn_ventana_confirma("REVERSO", 
                            "¿Está seguro que desea realizar el reverso de la generación de la póliza contable?", 
                            "quest")
                            RETURNING v_respuesta

                           --Se obtiene pid de la operación de confirmación, 
                           --No de la operación de generación
                           SELECT folio_referencia
                           INTO f_folio_confirmacion
                           FROM glo_folio
                           WHERE folio = f_folio_cnt;
                            
                           --Se obtiene pid de la operación de acuerdo al folio de la generación de la póliza
                           SELECT DISTINCT pid 
                           INTO p_pid
                           FROM bat_ctr_operacion
                           WHERE folio = f_folio_confirmacion

                           --Si no existe el pid de acuerdo al folio, traerá el último
                           IF p_pid IS NULL OR p_pid = 0 THEN
                              CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid 
                           END IF


                           -- Llama la función para validar la ejecución del reverso
                           CALL fn_valida_reverso(p_pid, p_proceso_cod, p_opera_cod)
                           RETURNING r_bandera_rev

                           --Si no se valida correctamente el reverso, muestra el mensaje del error en pantalla
                           IF r_bandera_rev  <> 0 THEN
                              CALL fn_muestra_inc_operacion(r_bandera_rev)
                              EXIT PROGRAM 
                           END IF 
      
                           DISPLAY "g_usuario ",g_usuario
                           DISPLAY "p_pid ",p_pid
                           DISPLAY "p_proceso_cod ",p_proceso_cod
                           DISPLAY "p_opera_cod ",p_opera_cod
                           DISPLAY "f_folio_cnt ",f_folio_cnt
                           DISPLAY "f_folio_ref ",f_folio_confirmacion
                           
                           --Si el usuario confirma la ejecución del reverso
                           IF v_respuesta = 1 THEN

                              CALL fn_rutas("cnt") RETURNING v_ruta_ejecutable, v_ruta_listados
                  
                              LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                                              "/CNTR021.42r ",g_usuario," ",p_pid," ",
                                              p_proceso_cod," ",p_opera_cod," ",f_folio_cnt," ",
                                              " '","NA","' 1>", v_ruta_listados CLIPPED ,
                                              "/nohup:",p_pid USING "&&&&&",":",
                                                        p_proceso_cod USING "&&&&&",":",
                                                        p_opera_cod USING "&&&&&" ," 2>&1 &"

                              RUN l_comando

                              LET v_s_mensaje = "Se ha enviado el Reverso de la Confirmación de la Póliza Contable con el pid: ",
                                  p_pid CLIPPED,
                                 ".\nPuede revisar el estado del proceso en el monitor de ejecución de procesos."

                              CALL fn_mensaje("Reverso",v_s_mensaje,"information")

                              EXIT PROGRAM 

                           
                           ELSE

                           END IF  

                  END DISPLAY
       END INPUT 
       
       ON  ACTION Cancelar 
          EXIT DIALOG  
          
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
                         "\n AND T.estado in (30,60)"
                        
       
      LET consulta = consulta || "\n AND T.folio_cnt = ", f_folio_cnt
           
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
    
    ################Checar estas validaciones con Fernando Herrera#############
                                 
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
    
    RETURN indx
    
END FUNCTION