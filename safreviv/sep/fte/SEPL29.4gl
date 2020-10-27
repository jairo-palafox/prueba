####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPL29                                        #
#Objetivo          =>Lanzador archivo salida de datos contacto sep #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>26 Junio   2012                               #
####################################################################
DATABASE safre_viv

DEFINE v_s_qryTxt        STRING
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      VARCHAR(30)              -- nombre del programa
DEFINE v_i_rechazo       INTEGER,
       v_ventana         ui.Window
DEFINE v_i_tot_reg_sol   INTEGER
DEFINE v_arr_contacto DYNAMIC ARRAY OF RECORD
	         consecutivo      INTEGER                                ,
	         caso_adai        LIKE sep_expediente.caso_adai          ,
	         estado           CHAR(40)                               ,
	         nss              LIKE sep_nss_expediente.nss            ,
	         tipo_trabajador  CHAR(40)                               ,
	         nombre           LIKE sep_nss_expediente.nombre         ,
	         tel_contacto1    LIKE sep_nss_expediente.tel_contacto1  ,
	         tel_contacto2    LIKE sep_nss_expediente.tel_contacto2  ,
	         tel_celular      LIKE sep_nss_expediente.tel_celular    ,
	         correo_e         STRING
	     END RECORD
DEFINE v_nomarc          CHAR(40) -- archivo 

MAIN
   DEFINE v_i_iter          SMALLINT, -- variable usada para iteracion
          v_i_iter_dep      SMALLINT, -- variable usada para iteracion
          v_i_indice        SMALLINT, -- indice del arrego de archivos pendientes
          v_i_proceso_cod   LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod     LIKE cat_operacion.opera_cod, -- operacion de la etapa que llama la funcion
          v_i_opera_cod_ant LIKE cat_operacion.opera_cod, -- operacion de la etapa anterior
          --v_i_operacion     LIKE acr_ctr_archivo.operacion, -- operacion del proceso
          v_d_folio         LIKE glo_ctr_archivo.folio, -- folio
          v_d_pid           LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_c_ruta_bin_acr  LIKE seg_modulo.ruta_bin, -- ruta del bin de acr
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_msj           STRING, -- se asigna un mensaje que será presentado al usuario
          r_b_valida        SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_ruta_vacia      STRING,
          v_continuar  BOOLEAN
          
   DEFINE w               ui.window
   DEFINE f               ui.form

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog = ARG_VAL(3)

   -- se abre la ventana para elejir los archivos a elegir
   OPEN WINDOW w_genera_archivo WITH FORM "SEPL291"
   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF
   -- Se define la ventana para manipular los objetos a visualizar de acuerdo a la operacion
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   
   CALL fn_carga_arreglo_contacto() RETURNING v_continuar

   IF(v_continuar)THEN
      DISPLAY ARRAY v_arr_contacto TO arr_contacto.*
         ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE DISPLAY 
         	 DISPLAY v_i_tot_reg_sol TO v_total
      	 
         ON ACTION aceptar
            IF v_i_tot_reg_sol > 0 THEN
               IF fn_ventana_confirma("Confimar","Desea generar archivo de contactos?","info") = 1 THEN
                  CALL lanza_archivo_sep_contacto()
                  EXIT DISPLAY
               END IF
            ELSE
               CALL fn_mensaje("Advertencia","No hay registros a procesar","error")
            END IF
         -- Accion que controla la cancelacion de elegidos (2da parte)
         ON ACTION CLOSE -- cancelar_eleccion
            EXIT DISPLAY
      END DISPLAY
   ELSE
      CALL fn_mensaje(p_v_nom_prog,"No se encontraron registros para contacto","information")
   END IF
   CLOSE WINDOW w_genera_archivo
END MAIN

FUNCTION fn_carga_arreglo_contacto()
DEFINE v_consulta    STRING
DEFINE v_datos_contacto RECORD
          v_ind             SMALLINT     ,
          v_diag            CHAR(3)      ,
          v_sql_error       INTEGER      ,
          v_id_expediente   DECIMAL(9,0) ,
          v_caso_adai       DECIMAL(9,0) ,
          v_estado          CHAR(40)     ,
          v_nss             CHAR(11)     ,
          v_tipo_trabajador CHAR(40)     ,
          v_nombre          CHAR(120)    ,
          v_tel1            DECIMAL(10,0),
          v_tel2            DECIMAL(10,0),
          v_cel             DECIMAL(10,0),
          v_correo_e        CHAR(40)     ,
          v_contactado      CHAR(40)     
	     END RECORD           
                            
   LET v_consulta = "EXECUTE FUNCTION sp_sep_consulta_datos_contacto(0,0,0)"
   PREPARE prp_rec_contactos FROM v_consulta
   DECLARE cur_rec_contactos CURSOR FOR prp_rec_contactos
   LET v_i_tot_reg_sol = 0
   FOREACH cur_rec_contactos INTO v_datos_contacto.*
      LET v_i_tot_reg_sol = v_i_tot_reg_sol + 1
      LET v_arr_contacto[v_i_tot_reg_sol].consecutivo      = v_i_tot_reg_sol
   	  LET v_arr_contacto[v_i_tot_reg_sol].caso_adai        = v_datos_contacto.v_caso_adai      
   	  LET v_arr_contacto[v_i_tot_reg_sol].estado           = v_datos_contacto.v_estado         
   	  LET v_arr_contacto[v_i_tot_reg_sol].nss              = v_datos_contacto.v_nss            
   	  LET v_arr_contacto[v_i_tot_reg_sol].tipo_trabajador  = v_datos_contacto.v_tipo_trabajador
   	  LET v_arr_contacto[v_i_tot_reg_sol].nombre           = v_datos_contacto.v_nombre         
   	  LET v_arr_contacto[v_i_tot_reg_sol].tel_contacto1    = v_datos_contacto.v_tel1           
   	  LET v_arr_contacto[v_i_tot_reg_sol].tel_contacto2    = v_datos_contacto.v_tel2           
   	  LET v_arr_contacto[v_i_tot_reg_sol].tel_celular      = v_datos_contacto.v_cel            
   	  LET v_arr_contacto[v_i_tot_reg_sol].correo_e         = "<a href='mailto:",v_datos_contacto.v_correo_e CLIPPED,"'>",v_datos_contacto.v_correo_e,"</a>"
   	         
   END FOREACH 

   IF(v_arr_contacto.getLength() > 0)THEN
      RETURN TRUE
   ELSE
      RETURN TRUE
   END IF

END FUNCTION


FUNCTION lanza_archivo_sep_contacto()
   DEFINE v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_c_ruta_bin_mdt    LIKE seg_modulo.ruta_bin, -- ruta del bin de mdt
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_comando         STRING, -- contiene al comando a correr
          v_folio_sep         INTEGER, -- folio
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_ruta_vacia        STRING 
   DEFINE v_cadena_pid        CHAR(5)
   DEFINE v_cadena_proc       CHAR(5)
   DEFINE v_cadena_opera      CHAR(5)
          
   -- se inicializan las variables
   LET v_i_proceso_cod = 2218 
   LET v_i_opera_cod = 1 
   LET v_d_pid = 0
   LET v_v_nom_archivo = "N/A"
   LET v_c_programa_cod = "MDTS29"
   
   CALL fn_genera_pid(v_i_proceso_cod
                     ,v_i_opera_cod
                     ,p_v_usuario)
                     RETURNING v_d_pid
   
   DISPLAY "v_d_pid generado  : ",v_d_pid
   
   LET v_folio_sep     = v_d_pid -- AHM TMP Validarlo si es correcto

   --sE OBTIENEN las rutas de los ejecutables
   CALL fn_rutas("sep") RETURNING v_c_ruta_bin_mdt, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat

   -- se crean las cadenas para el nombre del archivo log
   LET v_cadena_pid   = v_d_pid USING "&&&&&"
   LET v_cadena_proc  = v_i_proceso_cod USING "&&&&&"
   LET v_cadena_opera = v_i_opera_cod USING "&&&&&" 

   ---- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operacion"
      EXIT PROGRAM
   END IF

   -- se invoca la funcion que inicializa el proceso
   LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                          v_folio_sep, v_c_programa_cod,
                                          v_v_nom_archivo, p_v_usuario)
   --LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
   --                                        v_folio_sep, v_c_programa_cod,v_v_nom_archivo, p_v_usuario)
   --
   -- se verifica si fue posible inicializar el proceso
   IF r_b_valida = 0 THEN
      -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidación
      

      IF(STATUS)THEN
         CALL fn_mensaje(v_c_programa_cod,"Ocurrio un error en la generación de archivo","about")
      ELSE
         CALL fn_mensaje("Aviso","Se ejecutó el proceso de generación de archivo","info")

         -- se invoca la función que deja la operación en estado Procesando
         LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
                                            v_d_pid, v_c_programa_cod,
                                            v_folio_sep, p_v_usuario)

                                            

         -- se verifica si fue posible inicializar la operacion
         IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)
            EXIT PROGRAM
         ELSE
            LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin_mdt CLIPPED,"/SEPS29 ",
                                                    p_v_usuario, " ",
                                                    v_d_pid, " ",
                                                    v_i_proceso_cod, " ",
                                                    v_i_opera_cod, " ",
                                                    v_folio_sep, " ",
                                                    v_nomarc," 1> ",
                                                    v_c_ruta_list_bat CLIPPED,
                             "/nohup:",v_d_pid USING "&&&&&",":",
                                       v_i_proceso_cod USING "&&&&&",":",
                                       v_i_opera_cod USING "&&&&&",
                             " 2>&1 &"

            DISPLAY v_s_comando
            RUN v_s_comando
         END IF
      END IF
   ELSE
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_inicializa_proceso"
   END IF

END FUNCTION

