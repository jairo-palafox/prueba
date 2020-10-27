--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/05/2012
--==============================================================================

################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPL52                                                        #
#Objetivo     => Intergración de archivo de compensacion deudor                #
#Fecha inicio => 19 Septiembre 2012                                            #
################################################################################
DATABASE safre_viv
GLOBALS "SEPG02.4gl"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion  SMALLINT,
       p_cad_ventana     STRING,
       r_pid             LIKE glo_pid.pid,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       v_opera_cod_val   LIKE cat_operacion.opera_cod,
       v_opera_cod_int       LIKE cat_operacion.opera_cod,
       v_archivos_pendientes DYNAMIC ARRAY OF STRING,
       v_archivos_elegidos   DYNAMIC ARRAY OF STRING,
       r_valida              INTEGER,
       v_det_compensacion_deudor DYNAMIC ARRAY OF RECORD
         v_num     INTEGER,
         v_nss     LIKE sep_deudor.nss,
         v_credito LIKE sep_deudor.num_credito,
         v_periodo LIKE sep_deudor.periodo_pago,
         v_total_aportacion DECIMAL(8,2)
       END RECORD

MAIN
DEFINE v_continua        BOOLEAN,
       v_dragdrop        UI.DRAGDROP,
       v_arrastra_origen STRING,
       v_indice_arrastre INTEGER,
       v_indice_suelta   INTEGER,
       v_valor_arrastre  STRING,
       v_can_int         BOOLEAN,
       v_renglon_actual  INTEGER,
       r_error           BOOLEAN
CONSTANT v_sr_pendientes = "sr_pendientes" # Se asigna el nombre de la tabla de archivos pendientes
CONSTANT v_sr_elegido    = "sr_elegidos" # Se asigna el nombre de la tabla de archivos elegidos

   # parametros estandar de invocacion de programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana    = ARG_VAL(3)

   LET v_proceso_cod   = v_proc_compensacion_deudor          # compensacion deudor
   LET v_opera_cod_val = v_opera_carga_compensacion_deudor   # carga de archivo
   LET v_opera_cod_int = v_opera_integra_compensacion_deudor # integracion de archivo

   
   
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   CALL fn_max_pid(v_proceso_cod,v_opera_cod_val) RETURNING r_pid
   CALL fn_valida_operacion(r_pid,v_proceso_cod,v_opera_cod_int) RETURNING r_valida

   # Se verifica si la operacion es valida
   IF(r_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_valida)
      EXIT PROGRAM
   END IF



   OPEN WINDOW vtna_carga_compensacion_deud WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL521"
      
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      LET v_continua = TRUE
      WHILE (v_continua)
         
         DIALOG ATTRIBUTE(UNBUFFERED)

            DISPLAY ARRAY v_archivos_pendientes TO sr_pendientes.*

               ON DRAG_START(v_dragdrop)
                  # se recupera tabla origen de arrastre
                  LET v_arrastra_origen = v_sr_pendientes
                  # se recupera el indice de la tabla origen de arrastre
                  LET v_indice_arrastre = ARR_CURR()
                  # se recupera el valor de la tabla origen de arrastre
                  LET v_valor_arrastre = v_archivos_pendientes[v_indice_arrastre]
   
               ON DRAG_FINISHED(v_dragdrop)
                  # se indica que no hay tabla origen
                  INITIALIZE v_arrastra_origen TO NULL
   
               ON DRAG_ENTER(v_dragdrop)
                  IF v_arrastra_origen IS NULL THEN
                     CALL v_dragdrop.setOperation(NULL)
                  END IF
   
               ON DROP(v_dragdrop)
                  IF(v_arrastra_origen == v_sr_pendientes)THEN
                     CALL v_dragdrop.dropInternal()
                  ELSE
                     # Se recupera el indice donde se soltó el archivo
                     LET v_indice_suelta = v_dragdrop.getLocationRow()
                     # Se inserta el archivo en el indice que se recuperó
                     CALL DIALOG.insertRow(v_sr_pendientes, v_indice_suelta)
                     # se establese el foco en la tabla destino
                     CALL DIALOG.setCurrentRow(v_sr_pendientes, v_indice_suelta)
                     # se agrega al arreglo el nomre del archivo
                     LET v_archivos_pendientes[v_indice_suelta] = v_valor_arrastre
                     # elimina el registro de la tabla origen
                     CALL DIALOG.deleteRow(v_sr_elegido, v_indice_arrastre)
                  END IF
            
            END DISPLAY
   
            DISPLAY ARRAY v_archivos_elegidos TO sr_elegidos.*
               ON DRAG_START(v_dragdrop)
                  # se recupera tabla origen de arrastre
                  LET v_arrastra_origen = v_sr_elegido
                  # se recupera el indice de la tabla origen de arrastre
                  LET v_indice_arrastre = ARR_CURR()
                  # se recupera el valor de la tabla origen de arrastre
                  LET v_valor_arrastre = v_archivos_elegidos[v_indice_arrastre]
   
               ON DRAG_FINISHED(v_dragdrop)
                  # se indica que no hay tabla origen
                  INITIALIZE v_arrastra_origen TO NULL
   
               ON DRAG_ENTER(v_dragdrop)
                  IF v_arrastra_origen IS NULL THEN
                     CALL v_dragdrop.setOperation(NULL)
                  END IF
   
               ON DROP(v_dragdrop)
                   IF v_arrastra_origen == v_sr_elegido THEN                       
                       CALL v_dragdrop.dropInternal()
                   ELSE
                       # Se recupera el indice donde se soltó el archivo
                       LET v_indice_suelta = v_dragdrop.getLocationRow()
                       # Se inserta el archivo en el indice que se recuperó
                       CALL DIALOG.insertRow(v_sr_elegido, v_indice_suelta)
                       # se establese el foco en la tabla destino
                       CALL DIALOG.setCurrentRow(v_sr_elegido, v_indice_suelta)
                       # se agrega al arreglo el nomre del archivo
                       LET v_archivos_elegidos[v_indice_suelta] = v_valor_arrastre
                       # elimina el registro de la tabla origen
                       CALL DIALOG.deleteRow(v_sr_pendientes, v_indice_arrastre)
                   END IF

            END DISPLAY

            # se despliega el detalle de compensaciones
            DISPLAY ARRAY v_det_compensacion_deudor TO sr_compensacion_deudor.*
   
            END DISPLAY
            

            BEFORE DIALOG
               LET v_can_int = FALSE
               # Se recuperan los archivos pendientes
               CALL fn_recupera_arch_pendientes()
               # se ocultan las tablas del detalle del archivo
               --CALL v_forma.setElementHidden("gpo_posibles_cuentas",1)
               --CALL v_forma.setElementHidden("gpo_nss_asociado",1)
               CALL DIALOG.setActionHidden("integrar",1)
                  
            # botones para elegir archivo
            ON ACTION aceptar
               # se valida que se haya seleccionado un archivo
               IF(v_archivos_elegidos.getLength() = 0)THEN
                  CALL fn_mensaje("Aviso","Debe arrastrar al menos un archivo a integrar","info")
                  CONTINUE DIALOG
               END IF
               # se valida que solo se integre un archivo
               IF(v_archivos_elegidos.getLength() > 1)THEN
                  CALL fn_mensaje("Aviso","Solo puede seleccionar un archivo","info")
                  CONTINUE DIALOG
               END IF
               # se recuperan las posibles cunetas a separar y los nss asociados
               CALL fn_recupera_detalle()
               LET v_can_int = TRUE
               # Se muestra las tablas del detalle del archivo
               --CALL v_forma.setElementHidden("gpo_posibles_cuentas",0)
               --CALL v_forma.setElementHidden("gpo_nss_asociado",0)
               CALL v_forma.setElementHidden("integrar",0)
               CALL DIALOG.setActionHidden("integrar",0)
               CALL DIALOG.setActionHidden("aceptar",1)
               # se imprimen los totales por tabla
               --DISPLAY v_total_posibles_cuentas TO flbl_total_separacion
               --DISPLAY v_total_nss_asociado TO flbl_total_nss
               --ACCEPT DIALOG

            # botones para integrar archivo
            ON ACTION integrar
               LET v_renglon_actual = DIALOG.getcurrentRow("sr_elegidos")
               # Ejecuta SP de integración diagnóstico Op 27
               CALL fn_ejecuta_integracion_compensacion(v_archivos_elegidos[v_renglon_actual])
                       RETURNING r_error
               IF(r_error)THEN
                  EXIT DIALOG
               ELSE
                  LET v_continua = FALSE
                  EXIT DIALOG
               END IF
            
            ON ACTION cancelar
               IF(v_can_int)THEN
                  CALL v_det_compensacion_deudor.clear()
                  # regresa a la seleccion del archivo
                  --CALL v_forma.setElementHidden("gpo_posibles_cuentas",1)
                  --CALL v_forma.setElementHidden("gpo_nss_asociado",1)
                  CALL DIALOG.setActionHidden("integrar",1)
                  CALL DIALOG.setActionHidden("aceptar",0)
                  LET v_archivos_pendientes[1] = v_archivos_elegidos[1]
                  CALL v_archivos_elegidos.clear()
                  LET v_can_int = FALSE
                  EXIT DIALOG
               ELSE
                  # sale de la seleccion del archivo
                  LET v_continua = FALSE
                  EXIT DIALOG
               END IF

         END DIALOG
      END WHILE

   CLOSE WINDOW vtna_carga_compensacion_deud

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL52                                                   #
#Descripcion       => Recupera archivos pendientes                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 19 Septiembre 2012                                       #
################################################################################
FUNCTION fn_recupera_arch_pendientes()
DEFINE v_consulta STRING,
       v_archivo  LIKE glo_ctr_archivo.nombre_archivo,
       v_indice   INTEGER

   LET v_indice = 1
   CALL v_archivos_pendientes.clear()

   LET v_consulta = "\n SELECT nombre_archivo",
                    "\n   FROM glo_ctr_archivo",
                    "\n  WHERE proceso_cod = ?",
                    "\n    AND opera_cod = ?",
                    "\n    AND estado = 1"
   PREPARE prp_rec_archivos FROM v_consulta
   DECLARE cur_rec_archivos CURSOR FOR prp_rec_archivos
   FOREACH cur_rec_archivos USING v_proceso_cod,
                                  v_opera_cod_val
                             INTO v_archivo
      LET v_archivos_pendientes[v_indice] = v_archivo
   END FOREACH 


END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL52                                                   #
#Descripcion       => Recupera detalle de archivo a integrar                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 19 Septiembre 2012                                       #
################################################################################
FUNCTION fn_recupera_detalle()
DEFINE v_consulta  STRING,
       v_detalle   RECORD
         v_nss     VARCHAR(11),
         v_credito VARCHAR(10),
         v_periodo VARCHAR(6),
         v_total_aportacion VARCHAR(10)
       END RECORD,
       v_indice    INTEGER

   LET v_indice = 1
   LET v_consulta = "\n SELECT nss,credito,periodo_pago,tot_aportacion",
                    "\n   FROM safre_tmp:tmp_sep_det_compensacion_deud",
                    "\n  WHERE 1 = 1"
   PREPARE prp_recupera_detalle FROM v_consulta
   DECLARE cur_recupera_detalle CURSOR FOR prp_recupera_detalle
   FOREACH cur_recupera_detalle INTO v_detalle.*
   
      LET v_det_compensacion_deudor[v_indice].v_num     = v_indice
      LET v_det_compensacion_deudor[v_indice].v_nss     = v_detalle.v_nss
      LET v_det_compensacion_deudor[v_indice].v_credito = v_detalle.v_credito
      LET v_det_compensacion_deudor[v_indice].v_periodo = v_detalle.v_periodo
      LET v_det_compensacion_deudor[v_indice].v_total_aportacion = v_detalle.v_total_aportacion / 100

      LET v_indice = v_indice + 1
      
   END FOREACH 
   FREE cur_recupera_detalle


END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL52                                                   #
#Descripcion       => Ejecuta el SP que realiza la integración compensacion    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 19 Septiembre 2012                                       #
################################################################################
FUNCTION fn_ejecuta_integracion_compensacion(v_archivo)
DEFINE v_comando         STRING,
       v_archivo         STRING,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       r_resultado_opera SMALLINT, # bandera de resultado de cambio de estatus
       v_error           BOOLEAN,
       r_folio           LIKE glo_folio.folio
       

   WHENEVER ERROR CONTINUE
   
   SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
   WHERE modulo_cod = "bat"

   # inicializa suponiendo que ejecuta correctamente
   LET v_error = FALSE
      
   CALL fn_valida_operacion(r_pid,v_proceso_cod,v_opera_cod_int) RETURNING r_resultado_opera
     
   IF r_resultado_opera <> 0 THEN
      LET v_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # Indica que ocurrió error
      RETURN v_error
   END IF

   # genera folio 
   {CALL fn_genera_folio(g_pid,g_proceso_cod,v_opera_cod_int)
                        RETURNING r_folio}
   # Folio se genera en lanzado
   LET r_folio = 0

   CALL fn_actualiza_opera_ini(r_pid,v_proceso_cod,v_opera_cod_int,r_folio,"SEPL52",
                               v_archivo,p_usuario_cod) RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      LET v_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      RETURN v_error
   END IF
   
   LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPE52.42r '",
                                    p_usuario_cod CLIPPED, "' ",
                                    r_pid CLIPPED, " ",
                                    v_proceso_cod CLIPPED," ",
                                    v_opera_cod_int CLIPPED," ",
                                    r_folio CLIPPED, " '",
                                    v_archivo CLIPPED,
                     "' 1>", v_ruta_listados CLIPPED,
                           "/nohup:",r_pid USING "&&&&&",":",
                                     v_proceso_cod USING "&&&&&",":",
                                     v_opera_cod_int USING "&&&&&"," 2>&1 &"
   
   RUN v_comando
   IF(STATUS)THEN
      LET v_error = TRUE
      CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la integración","about")
   ELSE
      CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos","about")
      LET v_error = FALSE
   END IF
   RETURN v_error
END FUNCTION