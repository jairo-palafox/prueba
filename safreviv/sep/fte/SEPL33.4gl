--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15/05/2012
--===============================================================
DATABASE safre_viv

DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion INTEGER,
       p_cad_ventana   STRING,
       g_proceso_cod   LIKE cat_proceso.proceso_cod,
       g_opera_cod     LIKE cat_operacion.opera_cod,
       g_opera_cod_val LIKE cat_operacion.opera_cod,
       g_pid           LIKE glo_pid.pid,
       v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,
       v_folios_pendientes DYNAMIC ARRAY OF STRING,
       v_folios_elegidos   DYNAMIC ARRAY OF STRING,
       v_dragdrop            UI.DRAGDROP, # manejador de arrastrar y soltar (drag and drop)
       v_saldos_preliquidar DYNAMIC ARRAY OF RECORD
         v_clasificacion  STRING,
         v_saldo92_invadido STRING,
         v_saldo92_asociado STRING,
         v_saldo97_invadido STRING,
         v_saldo97_asociado STRING,
         v_saldo92          STRING,
         v_saldo97          STRING,
         v_total_parejas    STRING
       END RECORD,
       r_resultado_opera SMALLINT,
       v_folio_preliquidar STRING

       DEFINE v_tot92_invadido STRING
       DEFINE v_tot92_asociado STRING
       DEFINE v_tot97_invadido STRING
       DEFINE v_tot97_asociado STRING
       DEFINE v_tot_saldo92    STRING
       DEFINE v_tot_saldo97    STRING
       DEFINE v_tot_parejas    STRING

MAIN

   {
    Se recuperan los parametros recibidos
    Clave de usuario
    Tipo de ejecucion (en línea o batch)
    Cadena que identifica al programa (lo que aparecería como título de la ventana)
   }
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   
   CALL fn_preliquidacion_notficacion_separacion()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL33                                                   #
#Descripcion       => Preliquidación de op28                                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Mayo 2012                                             #
################################################################################
FUNCTION fn_preliquidacion_notficacion_separacion()
DEFINE v_ventana         ui.Window,
       v_forma           ui.Form,
       r_valida          SMALLINT,
       v_arrastra_origen STRING,
       v_indice_arrastre INTEGER,
       v_indice_suelta   INTEGER,
       v_valor_arrastre  STRING,
       v_continua        BOOLEAN

       CONSTANT v_sr_pendientes = "sr_pendientes" # Se asigna el nombre de la tabla de archivos pendientes
       CONSTANT v_sr_elegido    = "sr_elegido" # Se asigna el nombre de la tabla de archivos elegidos

   LET g_proceso_cod   = 2202 # Proceso Op. 28
   LET g_opera_cod     = 3    # preliquidacion de Operación 28
   LET g_opera_cod_val = 1    # Validación de Operación 28
   
   CALL fn_max_pid(g_proceso_cod,g_opera_cod) RETURNING g_pid
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_valida
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
    
   LET v_continua = TRUE

   OPEN WINDOW vtna_integra_notficacion_separacion WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL331"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      WHILE (v_continua)
         DIALOG ATTRIBUTE(UNBUFFERED)

            DISPLAY ARRAY v_folios_pendientes TO sr_pendientes.*

               ON DRAG_START(v_dragdrop)
                  # se recupera tabla origen de arrastre
                  LET v_arrastra_origen = v_sr_pendientes
                  # se recupera el indice de la tabla origen de arrastre
                  LET v_indice_arrastre = ARR_CURR()
                  # se recupera el valor de la tabla origen de arrastre
                  LET v_valor_arrastre = v_folios_pendientes[v_indice_arrastre]
   
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
                     LET v_folios_pendientes[v_indice_suelta] = v_valor_arrastre
                     # elimina el registro de la tabla origen
                     CALL DIALOG.deleteRow(v_sr_elegido, v_indice_arrastre)
                  END IF
            
            END DISPLAY
   
            DISPLAY ARRAY v_folios_elegidos TO sr_elegido.*
               ON DRAG_START(v_dragdrop)
                  # se recupera tabla origen de arrastre
                  LET v_arrastra_origen = v_sr_elegido
                  # se recupera el indice de la tabla origen de arrastre
                  LET v_indice_arrastre = ARR_CURR()
                  # se recupera el valor de la tabla origen de arrastre
                  LET v_valor_arrastre = v_folios_elegidos[v_indice_arrastre]
   
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
                       LET v_folios_elegidos[v_indice_suelta] = v_valor_arrastre
                       # elimina el registro de la tabla origen
                       CALL DIALOG.deleteRow(v_sr_pendientes, v_indice_arrastre)
                   END IF

            END DISPLAY

            BEFORE DIALOG
               # limpia los arreglos de los archiv pendientes y a elegir
               CALL v_folios_pendientes.clear()
               CALL v_folios_elegidos.clear()
               # se ocultan las tablas del detalle del archivo
               CALL v_forma.setElementHidden("gpo_saldos_preliquidar",1)
               
               # Se recuperan los archivos pendientes
               CALL fn_recupera_folios_pendientes()
               IF(v_folios_pendientes.getLength() = 0)THEN
                  CALL fn_mensaje(p_cad_ventana,"No hay información para preliquidar","about")
                  LET v_continua = FALSE
                  EXIT DIALOG
               END IF              
                  
            # botones para elegir archivo
            ON ACTION aceptar
               # se valida que se haya seleccionado un archivo
               IF(v_folios_elegidos.getLength() = 0)THEN
                  CALL fn_mensaje("Aviso","Debe arrastrar al menos un archivo a integrar","info")
                  CONTINUE DIALOG
               END IF
               # se valida que solo se integre un archivo
               IF(v_folios_elegidos.getLength() > 1)THEN
                  CALL fn_mensaje("Aviso","Solo puede seleccionar un archivo","info")
                  CONTINUE DIALOG
               END IF
               LET v_folio_preliquidar = v_folios_elegidos[1]
               # se recuperan las posibles cunetas a separar y los nss asociados
               CALL fn_recupera_detalle(v_folio_preliquidar)               
               # Se muestra las tablas del detalle del archivo
               CALL v_forma.setElementHidden("gpo_saldos_preliquidar",0)

               
               
               DISPLAY v_tot92_invadido TO lbl_v92_invadido
               DISPLAY v_tot92_asociado TO lbl_v92_asociado
               DISPLAY v_tot97_invadido TO lbl_v97_invadido
               DISPLAY v_tot97_asociado TO lbl_v97_asociado
               DISPLAY v_tot_saldo92    TO lbl_v92
               DISPLAY v_tot_saldo97    TO lbl_v97
               DISPLAY v_tot_parejas    TO lbl_total_parejas

               # Llama funcion que muestra detalles del archivo a preliquidar
               CALL fn_preliquida_notificacion(v_folio_preliquidar) RETURNING v_continua
               # si v_continua = false termina ejecucion
               # si v_continua = true vuelve a elegir archivo
               EXIT DIALOG
   
            ON ACTION cancelar
               # termina ejecucion y regresa a menu principal
               LET v_continua = FALSE
               EXIT DIALOG

         END DIALOG
      END WHILE

   CLOSE WINDOW vtna_integra_notficacion_separacion

END FUNCTION


################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL33                                                   #
#Descripcion       => recupera los folios a preliquidar                        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_folios_pendientes()
DEFINE v_folio    LIKE glo_folio.folio,
       v_indice   SMALLINT,
       v_consulta STRING

   WHENEVER ERROR CONTINUE

   LET v_indice = 1
   CALL v_folios_pendientes.clear()

   LET v_consulta = "\n SELECT det.folio",
                    "\n   FROM sep_det_02_op28 det JOIN sep_cza_op28 cza",
                    "\n     ON det.folio = cza.folio",
                    "\n  WHERE cza.estado = 15",
                    "\n    AND det.estado = 10",
                    "\n  GROUP BY det.folio"
   PREPARE prp_rec_folios_pendientes FROM v_consulta 
   DECLARE cur_recupera_pendientes CURSOR FOR prp_rec_folios_pendientes 
   --PREPARE prp_recupera_pendientes FROM v_consulta
   {DECLARE cur_recupera_pendientes CURSOR FOR --prp_recupera_pendientes
   SELECT nombre_archivo
     FROM glo_ctr_archivo
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod = g_opera_cod_val
      AND estado = 1 # cargado}
   FOREACH cur_recupera_pendientes INTO v_folio
      LET v_folios_pendientes[v_indice] = v_folio
      LET v_indice = v_indice + 1
   END FOREACH
   
   FREE cur_recupera_pendientes
   --LET v_folios_pendientes[v_indice] = "12345"
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL33                                                   #
#Descripcion       => Recupera detalle de folio a preliquidar                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_detalle(v_folio_preliquidar)
DEFINE v_folio_preliquidar LIKE glo_folio.folio,
       v_consulta STRING,
       v_indice   INTEGER,
       v_saldos_preliquidar_aux RECORD
         v_clasificacion    LIKE sep_det_02_op28.clasifica_separacion,
         v_saldo92_invadido LIKE sep_det_02_op28.saldo_viv_92,
         v_saldo92_asociado LIKE sep_det_02_op28.saldo_viv_92,
         v_saldo97_invadido LIKE sep_det_02_op28.saldo_viv_97,
         v_saldo97_asociado LIKE sep_det_02_op28.saldo_viv_97,
         v_saldo92          LIKE sep_det_02_op28.saldo_viv_92,
         v_saldo97          LIKE sep_det_02_op28.saldo_viv_97,
         v_total_parejas    INTEGER
       END RECORD

   WHENEVER ERROR CONTINUE

   LET v_indice = 1
   LET v_tot92_invadido = 0
   LET v_tot92_asociado = 0
   LET v_tot97_invadido = 0
   LET v_tot97_asociado = 0
   LET v_tot_saldo92 = 0
   LET v_tot_saldo97 = 0
   LET v_tot_parejas = 0
   
   CALL v_saldos_preliquidar.clear()
   {LET v_consulta = "\n SELECT ro.resultado_operacion_desc, dc.diag_confronta_desc,",
                    "\n        cc.clasificacion_desc, COUNT(dg.rfc)",
                    "\n   FROM safre_tmp:tmp_sep_det_02_op27_diag dg LEFT OUTER JOIN sep_cat_resultado_operacion ro",
                    "\n     ON dg.resultado_operacion = ro.resultado_operacion",
                    "\n        LEFT OUTER JOIN sep_cat_diag_confronta dc",
                    "\n     ON dg.diag_confronta = dc.diag_confronta",
                    "\n        LEFT OUTER JOIN sep_cat_clasificacion cc",
                    "\n     ON dg.clasifica_separacion = cc.clasifica_separacion",
                    "\n  GROUP BY ro.resultado_operacion_desc, dc.diag_confronta_desc,",
                    "\n           cc.clasificacion_desc"}

   LET v_consulta = "\n SELECT det2.clasifica_separacion, SUM(det2.saldo_viv_92),",
                    "\n        SUM(det3.saldo_viv_92),SUM(det2.saldo_viv_97),",
                    "\n        SUM(det3.saldo_viv_97),SUM(det2.saldo_viv_92+det3.saldo_viv_92),",
                    "\n        SUM(det3.saldo_viv_97+det2.saldo_viv_97),COUNT(det2.id_det_02_op28)",
                    "\n   FROM sep_det_02_op28 det2 JOIN sep_det_03_op28 det3",
                    "\n     ON det3.id_det_02_op28 = det2.id_det_02_op28",
                    "\n  WHERE det2.folio = ?",
                    "\n  GROUP BY 1"

   PREPARE prp_cifras_preliquidar FROM v_consulta
   DECLARE cur_cifras_preliquidar CURSOR FOR prp_cifras_preliquidar
   FOREACH cur_cifras_preliquidar USING v_folio_preliquidar 
                                   INTO v_saldos_preliquidar_aux.*
            
      LET v_saldos_preliquidar[v_indice].v_clasificacion    = v_saldos_preliquidar_aux.v_clasificacion
      LET v_saldos_preliquidar[v_indice].v_saldo92_invadido = v_saldos_preliquidar_aux.v_saldo92_invadido USING "###,###,###.##"
      LET v_saldos_preliquidar[v_indice].v_saldo92_asociado = v_saldos_preliquidar_aux.v_saldo92_asociado USING "###,###,###.##"
      LET v_saldos_preliquidar[v_indice].v_saldo97_invadido = v_saldos_preliquidar_aux.v_saldo97_invadido USING "###,###,###.##"
      LET v_saldos_preliquidar[v_indice].v_saldo97_asociado = v_saldos_preliquidar_aux.v_saldo97_asociado USING "###,###,###.##"
      LET v_saldos_preliquidar[v_indice].v_saldo92          = v_saldos_preliquidar_aux.v_saldo92 USING "###,###,###.##"
      LET v_saldos_preliquidar[v_indice].v_saldo97          = v_saldos_preliquidar_aux.v_saldo97 USING "###,###,###.##"
      LET v_saldos_preliquidar[v_indice].v_total_parejas    = v_saldos_preliquidar_aux.v_total_parejas

      LET v_tot92_invadido = v_tot92_invadido + v_saldos_preliquidar[v_indice].v_saldo92_invadido USING "###,###,###.##"

      LET v_tot92_asociado = v_tot92_asociado + v_saldos_preliquidar[v_indice].v_saldo92_asociado USING "###,###,###.##"

      LET v_tot97_invadido = v_tot97_invadido + v_saldos_preliquidar[v_indice].v_saldo97_invadido USING "###,###,###.##"

      LET v_tot97_asociado = v_tot97_asociado + v_saldos_preliquidar[v_indice].v_saldo97_asociado USING "###,###,###.##"

      LET v_tot_saldo92 = v_tot_saldo92 + v_saldos_preliquidar[v_indice].v_saldo92 USING "###,###,###.##"

      LET v_tot_saldo97 = v_tot_saldo97 + v_saldos_preliquidar[v_indice].v_saldo97 USING "###,###,###.##"

      LET v_tot_parejas = v_tot_parejas + v_saldos_preliquidar[v_indice].v_total_parejas 
      
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_cifras_preliquidar

   {LET v_saldos_preliquidar[1].v_clasificacion =  "C"
   LET v_saldos_preliquidar[1].v_saldo92_invadido = 234456.87 USING "###,###,###.##"
   LET v_saldos_preliquidar[1].v_saldo92_asociado = 345678.00 USING "###,###,###.##"
   LET v_saldos_preliquidar[1].v_saldo97_invadido = 1345890.98 USING "###,###,###.##"
   LET v_saldos_preliquidar[1].v_saldo97_asociado = 980999.89 USING "###,###,###.##"
   LET v_saldos_preliquidar[1].v_saldo92 = 580134.87 USING "###,###,###.##"
   LET v_saldos_preliquidar[1].v_saldo97 = 2326890.87  USING "###,###,###.##"
   LET v_saldos_preliquidar[1].v_total_parejas = 15

   LET v_saldos_preliquidar[2].v_clasificacion =  "B"
   LET v_saldos_preliquidar[2].v_saldo92_invadido = 456987.87 USING "###,###,###.##"
   LET v_saldos_preliquidar[2].v_saldo92_asociado = 1034578.34 USING "###,###,###.##"
   LET v_saldos_preliquidar[2].v_saldo97_invadido = 1487980.98 USING "###,###,###.##"
   LET v_saldos_preliquidar[2].v_saldo97_asociado = 3123458.89 USING "###,###,###.##"
   LET v_saldos_preliquidar[2].v_saldo92 = 1491566.21 USING "###,###,###.##"
   LET v_saldos_preliquidar[2].v_saldo97 = 4611439.87 USING "###,###,###.##"
   LET v_saldos_preliquidar[2].v_total_parejas = 20
   }
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL33                                                   #
#Descripcion       => Muestra los detalles del archivo elegido a preliquidar   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 18 Mayo 2012                                             #
################################################################################
FUNCTION fn_preliquida_notificacion(v_folio_preliquidar)
DEFINE v_continua BOOLEAN,
       r_confirma BOOLEAN,
       v_folio_preliquidar STRING

   # suponiendo que se realiza la integracion
   LET v_continua = FALSE
   
   # se despliega los posibles saldos a separar
   DISPLAY ARRAY v_saldos_preliquidar TO sr_saldos_preliquidar.*
       ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
      # botones para integrar archivo
      ON ACTION preliquidar
         CALL fn_ventana_confirma(p_cad_ventana,"¿Preliquidar Folio?","question") 
                 RETURNING r_confirma
         IF(r_confirma)THEN
            CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
            IF(r_resultado_opera <> 0 )THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               CONTINUE DISPLAY
            ELSE
               CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,v_folio_preliquidar,'SEPL33','',p_usuario_cod)
                    RETURNING r_resultado_opera
               IF(r_resultado_opera = 0)THEN
                  CALL fn_ejecuta_preliquidacion_notificacion(v_folio_preliquidar)
                  # indica que tiene que salir de la pantalla y regresa al menu principal
                  LET v_continua = FALSE
                  ACCEPT DISPLAY
               ELSE
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE DISPLAY
               END IF   
            END IF
         ELSE
            CONTINUE DISPLAY
         END IF
            
      ON ACTION cancelar
         # indica que regresa a la pantalla para elegir archivo
         LET v_continua = TRUE
         EXIT DISPLAY
   
   END DISPLAY

   RETURN v_continua
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL33                                                   #
#Descripcion       => Ejecuta el programa que realiza la preliquidacion de op28#
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
################################################################################
FUNCTION fn_ejecuta_preliquidacion_notificacion(v_folio_preliquidar)
DEFINE v_folio_preliquidar STRING, 
       v_comando          STRING,
       v_ruta_listados    LIKE seg_modulo.ruta_listados,
       v_ruta_bin_aux     LIKE seg_modulo.ruta_bin

   
   CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
   
   LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPE33.42r "
                                 ,p_usuario_cod, " ",g_pid, " ",g_proceso_cod," "
                                 ,g_opera_cod," ",v_folio_preliquidar," NA",
                   " 1>", v_ruta_listados CLIPPED,
                   "/nohup:",g_pid USING "&&&&&",":",
                             g_proceso_cod USING "&&&&&",":",
                             g_opera_cod USING "&&&&&",
                   " 2>&1 &"
   RUN v_comando
   IF(STATUS)THEN
      CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la preliquidacion","about")
   ELSE
      CALL fn_mensaje(p_cad_ventana,
                      "Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos",
                      "about")
   END IF

END FUNCTION