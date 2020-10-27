--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-04-2012
--===============================================================

####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTL10                                        #
#Objetivo          =>Generar cambios de mandatos para Hipotecaria S#
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>14 Febrero 2012                               #
####################################################################

DATABASE safre_viv

DEFINE v_s_qryTxt        STRING
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      VARCHAR(30),             -- nombre del programa
       tot_altas          INTEGER,
       tot_bajas          INTEGER,
       tot_modificaciones INTEGER,
       v_ventana ui.Window


MAIN

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog = ARG_VAL(3)
   
   -- se abre la ventana de consulta
   OPEN WINDOW w_hipo_soc WITH FORM "MDTL101"
   #Se asigna el titulo de la ventana
   IF(p_v_nom_prog IS NOT NULL)THEN
       CALL ui.Interface.setText(p_v_nom_prog)
       LET v_ventana = ui.Window.getCurrent()
       CALL v_ventana.setText(p_v_nom_prog)
   END IF
   MENU ""
      BEFORE MENU
         LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                          "   FROM mdt_notifica_mandato ",
                          "  WHERE estado = 100 ",
                          "  AND tipo_operacion = ? "
                          
         PREPARE EnuTotReg FROM v_s_qryTxt
         EXECUTE EnuTotReg USING "A" INTO tot_altas
         EXECUTE EnuTotReg USING "B" INTO tot_bajas
         EXECUTE EnuTotReg USING "M" INTO tot_modificaciones
         DISPLAY BY NAME tot_altas, tot_bajas, tot_modificaciones 
      
      ON ACTION accept
         IF tot_altas+tot_bajas+tot_modificaciones > 0 THEN   
            -- LANZAR BATCH
            IF fn_ventana_confirma("Confimar","Generar archivo de Cambios al Catalogo de Mandatos?","info") = 1 THEN
            --IF fgl_winquestion("Confirmación","Generar archivo de Cambios al Catalogo de Mandatos?","no","yes|no", "question", 0) = 'yes' THEN
               CALL lanza_archivo_mdt()
            END IF
         ELSE
            CALL fn_mensaje("Información","No hay registros a procesar","info")
         END IF
         EXIT MENU
         
      ON ACTION close
         EXIT MENU
   END MENU
   
   CLOSE WINDOW w_hipo_soc
   
END MAIN

FUNCTION lanza_archivo_mdt()
   DEFINE v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_c_ruta_bin_mdt    LIKE seg_modulo.ruta_bin, -- ruta del bin de mdt
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_comando         STRING, -- contiene al comando a correr
          v_folio_mandato     INTEGER, -- folio
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_ruta_vacia        STRING 
   DEFINE v_cadena_pid        CHAR(5)
   DEFINE v_cadena_proc       CHAR(5)
   DEFINE v_cadena_opera      CHAR(5)
          
   -- se inicializan las variables
   LET v_i_proceso_cod = 1302 -- 45 anterior-- generación archivo mandatos -- AHM 2012 04 25 Cambio de procesos
   LET v_i_opera_cod = 1 -- genera archivo mandatos
   LET v_d_pid = 0
   LET v_v_nom_archivo = "NA"
   LET v_c_programa_cod = "MDTL10"

   
   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operacion"
      EXIT PROGRAM
   END IF

   CALL fn_genera_pid(v_i_proceso_cod
                     ,v_i_opera_cod
                     ,p_v_usuario)
                     RETURNING v_d_pid

   LET v_folio_mandato = v_d_pid -- AHM TMP Validarlo si es correcto

   --sE OBTIENEN las rutas de los ejecutables
   CALL fn_rutas("mdt") RETURNING v_c_ruta_bin_mdt, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat

   -- se crean las cadenas para el nombre del archivo log
   LET v_cadena_pid   = v_d_pid USING "&&&&&"
   LET v_cadena_proc  = v_i_proceso_cod USING "&&&&&"
   LET v_cadena_opera = v_i_opera_cod USING "&&&&&" 

   

   

   -- se invoca la funcion que inicializa el proceso
   LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                          v_folio_mandato, v_c_programa_cod,
                                          v_v_nom_archivo, p_v_usuario)

   -- se verifica si fue posible inicializar el proceso
   IF r_b_valida = 0 THEN
      LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                            v_folio_mandato, v_c_programa_cod,
                                            v_v_nom_archivo, p_v_usuario)

      -- se verifica si fue posible inicializar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(r_b_valida)
         EXIT PROGRAM
      END IF
      -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidación
      LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin_mdt CLIPPED,"/MDTS10 ",
                                              p_v_usuario, " ",
                                              v_d_pid, " ",
                                              v_i_proceso_cod, " ",
                                              v_i_opera_cod, " ",
                                              v_folio_mandato, " ",
                                              v_v_nom_archivo, " 1> ",
                                              v_c_ruta_list_bat CLIPPED,
                                              "/nohup:",v_d_pid USING "&&&&&",":",
                                              v_i_proceso_cod USING "&&&&&",":",
                                              v_i_opera_cod USING "&&&&&",
                                              " 2>&1 &"

      DISPLAY v_s_comando
      RUN v_s_comando

      IF(STATUS)THEN
         CALL fn_mensaje(v_c_programa_cod,"Ocurrio un error al ejecutar la integración","about")
      ELSE
         CALL fn_mensaje("Aviso","Se ejecutó el proceso de generación de archivo","info")
      END IF
      
   ELSE
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_inicializa_proceso"
   END IF

END FUNCTION