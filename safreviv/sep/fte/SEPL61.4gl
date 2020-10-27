--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15/10/2012
--==============================================================================
DATABASE safre_viv
GLOBALS "SEPG02.4gl"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       v_opera_cod       LIKE cat_operacion.opera_cod,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_expedientes     DYNAMIC ARRAY OF RECORD
          v_num          SMALLINT,
          v_id_expediente LIKE sep_movimiento_invadido.id_expediente,
          v_invadido      LIKE sep_movimiento_invadido.invadido,
          v_asociado      LIKE sep_movimiento_invadido.asociado,
          v_subcuenta     LIKE sep_movimiento_invadido.subcuenta,
          v_total_aivs    LIKE sep_movimiento_invadido.monto_acciones
       END RECORD,
       v_sum_total_aivs   LIKE sep_movimiento_invadido.monto_acciones
          

MAIN
   # recupera parámetros
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_consulta_exp_preliq_solo_infonavit()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL61                                                   #
#Descripcion       => Preliquidación de expedientes solo infonavit             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Octubre 2012                                          #
################################################################################
FUNCTION fn_consulta_exp_preliq_solo_infonavit()
DEFINE r_valida    SMALLINT,
       v_ventana   ui.Window,
       v_forma     ui.Form,
       r_confirma  BOOLEAN,
       r_continuar BOOLEAN

   LET v_proceso_cod = v_proc_expedientes_solo_infonavit
   LET v_opera_cod   = v_opera_preliq_expedientes_solo_infonavit
   
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_valida

   # Se verifica si la operacion es valida
   IF(r_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_valida)
      EXIT PROGRAM
   END IF

   # se recupera la ruta ejecutable del módulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   OPEN WINDOW vtna_con_exp_preliq_solo_infonavit WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL611"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      DISPLAY ARRAY v_expedientes TO sr_expedientes.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
      
         BEFORE DISPLAY
            # recupera los expedientes pendientes de preliquidar
            CALL fn_recupera_expedientes_pendientes()
            IF(v_expedientes.getLength() = 0)THEN
               CALL fn_mensaje(p_cad_ventana,"Sin expedientes que preliquidar","information")
               EXIT DISPLAY
            END IF
            DISPLAY v_sum_total_aivs TO total_aivs 

         ON ACTION preliquidar
             CALL fn_ventana_confirma("Confirmar","Preliquidar expedientes?","question") RETURNING r_confirma
             IF(r_confirma)THEN
                # funcion que ejecuta preliquidación
                CALL fn_ejecuta_preliquidacion_exp() RETURNING r_continuar
                IF(r_continuar)THEN
                   CONTINUE DISPLAY
                ELSE
                   EXIT DISPLAY
                END IF
             END IF

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY
   CLOSE WINDOW vtna_con_exp_preliq_solo_infonavit 


END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL61                                                   #
#Descripcion       => funcion para recuperar los expedientes pendientes        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Octubre 2012                                          #
################################################################################
FUNCTION fn_recupera_expedientes_pendientes()
DEFINE v_consulta         STRING,
       v_indice           INTEGER,
       v_expedientes_aux  RECORD
          v_id_expediente LIKE sep_movimiento_invadido.id_expediente,
          v_invadido      LIKE sep_movimiento_invadido.invadido,
          v_asociado      LIKE sep_movimiento_invadido.asociado,
          v_subcuenta     LIKE sep_movimiento_invadido.subcuenta,
          v_total_aivs    LIKE sep_movimiento_invadido.monto_acciones
       END RECORD

   CALL v_expedientes.clear()
   LET v_consulta = "\n SELECT id_expediente, ",
                    "\n        invadido, ",
                    "\n        asociado, ",
                    "\n        subcuenta,",
                    "\n        SUM(aivs)",
                    "\n FROM ",
                    "\n TABLE( MULTISET(",
                    # recupera los registros que no son saldo inicial
                    "\n SELECT mov.id_expediente, ",
                    "\n        mov.invadido, ",
                    "\n        mov.asociado, ",
                    "\n        mov.subcuenta,",
                    "\n        SUM(mov.monto_acciones) aivs",
                    "\n   FROM sep_expediente exp JOIN sep_movimiento_invadido mov",
                    "\n     ON mov.id_expediente = exp.id_expediente",
                    "\n  WHERE exp.estado = 75", # previo confirmado
                    "\n    AND mov.ind_mov_asociado = 1",# saldo a separar
                    "\n    AND mov.movimiento <> 999", # saldo inicial
                    "\n  GROUP BY mov.id_expediente,mov.invadido,mov.asociado,mov.subcuenta",
                    "\n UNION ",
                    # recupera los registrosde saldo inicial y realiza la sumatoria del saldo especificado
                    "\n SELECT mov.id_expediente,",
                    "\n        mov.invadido,",
                    "\n        mov.asociado,",
                    "\n        mov.subcuenta,",
                    "\n        SUM(sdo.mto_aivs_separado) aivs",
                    "\n   FROM sep_expediente exp JOIN sep_movimiento_invadido mov",
                    "\n     ON mov.id_expediente = exp.id_expediente",
                    "\n        JOIN sep_saldo_inicial sdo",
                    "\n     ON sdo.id_sep_movimiento_invadido = mov.id_sep_movimiento_invadido",
                    "\n  WHERE exp.estado = 75",# previo confirmado
                    "\n    AND mov.ind_mov_asociado = 1", # saldo a separar
                    "\n    AND mov.movimiento = 999", # saldo inicial
                    "\n  GROUP BY mov.id_expediente,mov.invadido,mov.asociado,mov.subcuenta",
                    "\n ))",
                    "\n WHERE 1 = 1",
                    "\n GROUP BY id_expediente,invadido,asociado,subcuenta"
   LET v_indice = 1
   LET v_sum_total_aivs = 0
   PREPARE prp_rec_exp_pendientes FROM v_consulta
   DECLARE cur_rec_exp_pendientes CURSOR FOR prp_rec_exp_pendientes
   FOREACH cur_rec_exp_pendientes INTO v_expedientes_aux.*
      LET v_expedientes[v_indice].v_num           = v_indice
      LET v_expedientes[v_indice].v_id_expediente = v_expedientes_aux.v_id_expediente
      LET v_expedientes[v_indice].v_invadido      = v_expedientes_aux.v_invadido
      LET v_expedientes[v_indice].v_asociado      = v_expedientes_aux.v_asociado
      LET v_expedientes[v_indice].v_subcuenta     = v_expedientes_aux.v_subcuenta
      LET v_expedientes[v_indice].v_total_aivs    = v_expedientes_aux.v_total_aivs

      LET v_sum_total_aivs = v_sum_total_aivs + v_expedientes_aux.v_total_aivs
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_exp_pendientes

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL61                                                   #
#Descripcion       => funcion que ejecuta lanzado de preliquidación            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Octubre 2012                                          #
################################################################################
FUNCTION fn_ejecuta_preliquidacion_exp()
DEFINE r_resultado_opera SMALLINT,
       v_continuar       BOOLEAN,
       v_folio           LIKE glo_folio.folio,
       v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       r_pid             LIKE glo_pid.pid,
       v_ruta_bin_aux    LIKE seg_modulo.ruta_bin,
       v_comando         STRING

   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   # Se verifica si la operacion es valida
   IF(r_resultado_opera <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF

   LET v_folio = 0
   LET v_nom_archivo = "NA"
   CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod) 
          RETURNING r_pid 
   CALL fn_inicializa_proceso(r_pid,         # pid
                              v_proceso_cod, # proceso
                              v_opera_cod,   # operacion
                              0,             # folio
                              "SEPL61",      # programa
                              v_nom_archivo, # archivo
                              p_usuario_cod) # usuario
              RETURNING r_resultado_opera
   IF( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   CALL fn_actualiza_opera_ini(r_pid,
                               v_proceso_cod,
                               v_opera_cod,
                               v_folio,
                               "SEPL61",
                               v_nom_archivo,
                               p_usuario_cod) RETURNING r_resultado_opera
   IF(r_resultado_opera <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
   LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPP61.42r ",
                                   p_usuario_cod CLIPPED, " ",
                                   r_pid, " ",
                                   v_proceso_cod," ",
                                   v_opera_cod," ",
                                   v_folio, " ",
                                   v_nom_archivo,
                            " 1> ",v_ruta_listados CLIPPED,
                         "/nohup:",r_pid USING "&&&&&",":",
                                   v_proceso_cod USING "&&&&&",":",
                                   v_opera_cod USING "&&&&&",
                          " 2>&1 &"
   DISPLAY v_comando
   RUN v_comando
   IF(STATUS)THEN
      LET v_continuar = TRUE
      CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la operación","about")
   ELSE
      CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos para el pid "||r_pid,"about")
      LET v_continuar = FALSE
   END IF

   RETURN v_continuar
END FUNCTION