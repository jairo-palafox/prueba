--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15/05/2012
--===============================================================
DATABASE safre_viv

DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion INTEGER,
       p_cad_ventana   STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       g_proceso_cod   LIKE cat_proceso.proceso_cod,
       g_opera_cod     LIKE cat_operacion.opera_cod,
       g_opera_cod_val LIKE cat_operacion.opera_cod,
       g_pid           LIKE glo_pid.pid,
       v_archivos_pendientes DYNAMIC ARRAY OF STRING,
       v_archivos_elegidos   DYNAMIC ARRAY OF STRING,
       v_dragdrop            UI.DRAGDROP, # manejador de arrastrar y soltar (drag and drop)
       v_saldos_separar DYNAMIC ARRAY OF RECORD
         v_detalle          char(5),
         v_tipo_movimiento  char(5),         
         v_saldo_viv_92     dec(16,2),         
         v_saldo_viv_97     dec(16,2)
       END RECORD,
       r_resultado_opera SMALLINT,
       v_archivo_integrar STRING

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
   
   CALL fn_integra_notificacion_correccion()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL04                                                   #
#Descripcion       => Integra Notificación Correción Saldos Separacion         #
#Autor             => Jesus David Yanez Moreno EFP                             #
################################################################################

FUNCTION fn_integra_notificacion_correccion()
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

   LET g_proceso_cod   = 2237 # Proceso Op.40 
   LET g_opera_cod     = 2    # Integración de Operación 40
   LET g_opera_cod_val = 1    # Validación de Operación 40

   CALL fn_max_pid(g_proceso_cod,g_opera_cod_val) RETURNING g_pid

   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
   
   IF(r_resultado_opera <> 0)THEN
      # en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   END IF

   # se recupera la ruta ejecutable del módulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   LET v_continua = TRUE

   OPEN WINDOW vtna_integra_notficacion_separacion WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL041"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
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
   
            DISPLAY ARRAY v_archivos_elegidos TO sr_elegido.*
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

            BEFORE DIALOG
               # limpia los arreglos de los archiv pendientes y a elegir
               CALL v_archivos_pendientes.clear()
               CALL v_archivos_elegidos.clear()
               # se ocultan las tablas del detalle del archivo
               CALL v_forma.setElementHidden("gpo_saldos_separar",1)
               
               # Se recuperan los archivos pendientes
               CALL fn_recupera_archivos_pendientes()
               IF(v_archivos_pendientes.getLength() = 0)THEN
                  CALL fn_mensaje(p_cad_ventana,"No hay información para integrar","about")
                  LET v_continua = FALSE
                  EXIT DIALOG
               END IF
               
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
               LET v_archivo_integrar = v_archivos_elegidos[1]
               # se recuperan las posibles cunetas a separar y los nss asociados
               CALL fn_recupera_detalle(v_archivo_integrar)
               # Se muestra las tablas del detalle del archivo
               CALL v_forma.setElementHidden("gpo_saldos_separar",0)

               # imprime totales del detalle
               --DISPLAY v_tot_parejas    TO lbl_total_parejas
   
               # Llama funcion que muestra detalles del archivo a integrar
               CALL fn_integra_notificacion(v_archivo_integrar) RETURNING v_continua
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
#Programa          => SEPL31                                                   #
#Descripcion       => Recupera los archivos de notificacion pendientes de      #
#                     integrar                                                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_archivos_pendientes()
DEFINE v_archivo  LIKE glo_ctr_archivo.nombre_archivo,
       v_indice   SMALLINT

   WHENEVER ERROR CONTINUE

   LET v_indice = 1
   CALL v_archivos_pendientes.clear()
   
   DECLARE cur_recupera_pendientes CURSOR FOR --prp_recupera_pendientes
   # consulta para recuperar los archivos a integrar de operacion 28
   SELECT nombre_archivo
     FROM glo_ctr_archivo
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod = g_opera_cod_val
      AND estado = 1 # cargado
      
   FOREACH cur_recupera_pendientes USING g_proceso_cod,g_opera_cod_val
                                   INTO v_archivo
      LET v_archivos_pendientes[v_indice] = v_archivo
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_pendientes
   
   --LET v_archivos_pendientes[v_indice] = "notificacion.op28"
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL31                                                   #
#Descripcion       => Recupera los posibles nss                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
#Modifico          => Alexandro Hollmann, EFP                                  #
#Fecha inicio      => 05 Junio 2012                                            #
################################################################################
FUNCTION fn_recupera_detalle(v_archivo_integrar)
DEFINE v_archivo_integrar STRING,
       v_consulta STRING,
       v_indice   INTEGER,
       v_saldos_separar_aux RECORD
         detalle              char(5),
         tipo_movimiento      char(5),         
         saldo_viv_92         DECIMAL(16,6),
         saldo_viv_97         DECIMAL(16,6)
        END RECORD 

   LET v_consulta = "EXECUTE PROCEDURE sep_op40_cifras_integra();"
   PREPARE prp_cifras_integra FROM v_consulta
   DECLARE cur_cifras_integra CURSOR FOR prp_cifras_integra

   WHENEVER ERROR CONTINUE

   LET v_indice = 1
   LET v_tot_parejas    = 0
   
   CALL v_saldos_separar.clear()

   FOREACH cur_cifras_integra INTO v_saldos_separar_aux.*

      LET v_saldos_separar[v_indice].v_detalle    = v_saldos_separar_aux.detalle
      LET v_saldos_separar[v_indice].v_tipo_movimiento    = v_saldos_separar_aux.tipo_movimiento
      LET v_saldos_separar[v_indice].v_saldo_viv_92 = v_saldos_separar_aux.saldo_viv_92
      LET v_saldos_separar[v_indice].v_saldo_viv_97 = v_saldos_separar_aux.saldo_viv_97

      LET v_indice = v_indice + 1
   END FOREACH

   IF v_saldos_separar.getLength() > 0 THEN
      IF LENGTH(v_saldos_separar[v_saldos_separar.getLength()].v_detalle CLIPPED) = 0 THEN
      	  CALL v_saldos_separar.DeleteElement(v_saldos_separar.getLength())
      END IF
   END IF
   FREE cur_cifras_integra

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL04                                                  #
#Descripcion       => Muestra los detalles del archivo elegido a integrar      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 18 Mayo 2012                                             #
################################################################################
FUNCTION fn_integra_notificacion(v_archivo_integrar)
DEFINE v_continua BOOLEAN,
       r_confirma BOOLEAN,
       v_archivo_integrar STRING

   # suponiendo que se realiza la integracion
   LET v_continua = FALSE
   
   # se despliega los posibles saldos a separar
   DISPLAY ARRAY v_saldos_separar TO sr_saldos_separar.*
       ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
      # botones para integrar archivo
      ON ACTION integrar
         CALL fn_ventana_confirma(p_cad_ventana,"¿Integrar Archivo?","question") 
                 RETURNING r_confirma
         IF(r_confirma)THEN
            CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
            IF(r_resultado_opera <> 0 )THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               CONTINUE DISPLAY
            ELSE
               CALL fn_ejecuta_integracion_notificacion(v_archivo_integrar)
               # indica que tiene que salir de la pantalla y regresa al menu principal
               LET v_continua = FALSE
               ACCEPT DISPLAY            
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
#Programa          => SEPL31                                                   #
#Descripcion       => Ejecuta el SP que realiza la integración del diagnóstico #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Mayo 2012                                             #
#Modifico          => Alexandro Hollmann, EFP                                  #
#Fecha modificacion=> 05 Junio 2012                                            #
################################################################################
FUNCTION fn_ejecuta_integracion_notificacion(v_archivo_integrar)
DEFINE v_archivo_integrar STRING, 
       v_comando          STRING,
       v_ruta_listados    LIKE seg_modulo.ruta_listados,
       v_ruta_bin_aux     LIKE seg_modulo.ruta_bin,
       v_res_opera        SMALLINT

   -- se invoca la función que deja la operación en estado Procesando
   LET v_res_opera = fn_actualiza_opera_ini(g_pid,              # PID
                                            g_proceso_cod,      # proceso
                                            g_opera_cod,        # operacion
                                            0,                  # folio
                                            "SEPL04",   # programa
                                            v_archivo_integrar, # archivo
                                            p_usuario_cod)

   # se verifica si fue posible inicializar la operacion
   IF v_res_opera <> 0 THEN
      # mensaje en pantalla
      CALL fn_muestra_inc_operacion(v_res_opera)
   ELSE
      CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
   
      LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPE04.42r ",
                                      p_usuario_cod, " ",
                                      g_pid, " ",
                                      g_proceso_cod," ",
                                      g_opera_cod," 0 ",
                                      v_archivo_integrar,
                               " 1>", v_ruta_listados CLIPPED,
                            "/nohup:",g_pid USING "&&&&&",":",
                                      g_proceso_cod USING "&&&&&",":",
                                      g_opera_cod USING "&&&&&",
                            " 2>&1 &"
      RUN v_comando
   
      IF(STATUS)THEN
         CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la integración","about")
      ELSE
         CALL fn_mensaje(p_cad_ventana,
                         "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||g_pid,
                         "about")
      END IF
   END IF

END FUNCTION
