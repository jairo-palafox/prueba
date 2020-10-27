################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 04/04/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR09                                                        #
#Objetivo     => Realizar el reverso de la carga del archivo de ajuste de      #
#                avances abiertos                                              #
#Fecha inicio => 16/01/2013                                                    #
################################################################################
DATABASE safre_viv
GLOBALS 
DEFINE 
  v_edit_Folio               VARCHAR(20),--Recibe el valor del folio a consultar
  v_de_Fecha                 DATE,       --Recibe el valor de la fecha a consultar
  r_bnd_status               SMALLINT,   --Bandera de reverso
  r_mensaje                  VARCHAR   

DEFINE arr_Avances           DYNAMIC ARRAY OF RECORD --Arreglo que recibe los valores de los avances abiertos
       folio                 LIKE dis_sum_avance_pago.folio,
       tot_registros         INTEGER,
       tot_aceptados         INTEGER,
       tot_rechazados        INTEGER
       END RECORD

DEFINE 
  p_nombre_menu              LIKE seg_menu.opcion, --Asigna el titulo de la ventana
  p_pid                      LIKE bat_ctr_proceso.pid,     --ID del proceso
  p_proceso_cod              LIKE cat_proceso.proceso_cod, --Código del proceso
  p_opera_cod                LIKE cat_operacion.opera_cod, --Código de operacion
  p_usuario                  LIKE seg_usuario.usuario_cod, --Clave de usuario
  p_tipo_proc                CHAR(1)               --Recibe el tipo de proceso

DEFINE 
  v_registros_det            DECIMAL(10,0),
  v_registros_rch            DECIMAL(10,0),
  v_registros_tot            DECIMAL(10,0),
  g_sql_txt                  STRING,
  v_proc_entra               SMALLINT,
  v_proc_val                 SMALLINT,
  v_cod_conv                 SMALLINT,
  v_desc_proc_val            CHAR(40),
  v_mensaje_val              STRING
      
END GLOBALS 

MAIN 
DEFINE 
  v_valida_fecha             LIKE dis_det_avance_pago.f_presentacion, --Recibe el valor si la fecha capturada es valida
  v_valida_folio             LIKE dis_det_avance_pago.folio,          --Recibe el valor si el folio capturado es valido
  r_bandera_rev              SMALLINT
       
DEFINE f_ventana             ui.Window,  --Define las propìedades de la Ventana
       f_forma               ui.Form     --Define las propiedades de la forma
       
DEFINE 
  v_ruta_ejecutable          LIKE seg_modulo.ruta_bin, -- Ruta del ejecutable
  v_ruta_listados            LIKE seg_modulo.ruta_listados, -- Rut del log
  v_s_mensaje                STRING,
  l_comando                  STRING 
       
DEFINE 
  r_bnd_cnt                  SMALLINT,
  r_tipo_rev_cnt             SMALLINT,
  v_respuesta                SMALLINT,
  v_valida_estado            INTEGER,
  v_QryTxt                   STRING 

  LET p_usuario     = ARG_VAL(1) -- Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) -- Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) -- Recibe el nombre del programa 
  LET p_proceso_cod = 907 -- Código asignado al proceso 
  LET p_opera_cod   = 2 -- Código asignado a la operación 

  INITIALIZE v_de_Fecha   TO NULL  -- Inicializa la variable fecha a nulo
  INITIALIZE v_edit_Folio TO NULL  -- Inicializa la variable folio a nulo      

  --Abre la ventana para ingresar parámetros de consulta    
  CLOSE WINDOW SCREEN 

  OPEN WINDOW vtn_AvancePagos WITH FORM "DISR091"
    LET r_bnd_cnt      = 0
    LET r_tipo_rev_cnt = 0
    LET r_bandera_rev  = 0
    LET v_respuesta    = 0

    LET f_ventana = ui.Window.getCurrent()
    LET f_forma   = f_ventana.getForm()

    --Se invoca la función que asigna el titulo a la ventana
    CALL ui.Interface.setText(p_nombre_menu)

    --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
    LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                    "       a.cod_proceso_valida,  ",
                    "       b.proceso_desc         ",
                    "FROM   cat_convivencia_dis a, ",
                    "       cat_proceso b          ",
                    "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
                    "AND    a.cod_proceso_valida = b.proceso_cod ",
                    "AND    a.cod_convivencia    = 0             ",
                    "ORDER BY cod_proceso_valida   "
    PREPARE ps_val_proc FROM g_sql_txt
    DECLARE cur_val_proc CURSOR FOR ps_val_proc
    FOREACH cur_val_proc INTO v_proc_entra,
                              v_proc_val,
                              v_desc_proc_val
      IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
         LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
         MENU "No se puede ejecutar" 
           ATTRIBUTES ( STYLE="dialog",
           COMMENT= v_mensaje_val,
           IMAGE="information" )

           ON ACTION salir
               RETURN
         END MENU
      END IF
    END FOREACH

    CALL f_forma.setElementHidden("gr_resregavpag", 1) --Oculta la Sección Resúmen Registro Av. Pagos
    CALL f_forma.setElementHidden("reverso", 1)   --Oculta el botón de Reverso

    --Se obtienen los parámetros de fecha y folio para consulta
    INPUT BY NAME v_edit_Folio, v_de_Fecha WITHOUT DEFAULTS
    ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)
      ON ACTION cancelar
         EXIT PROGRAM 
   
      -- Botón aceptar que realiza la consulta en base a folio y fecha
      ON ACTION aceptar
         IF v_de_Fecha IS NULL AND  v_edit_Folio IS NULL THEN 
            CALL fn_mensaje("ERROR", "Ingrese al menos un criterio de búsqueda", "information")
            NEXT FIELD v_edit_Folio
         END IF 

         --Valida que la fecha capturada no sea mayor a la fecha actual
         IF ( v_de_Fecha > TODAY ) THEN
            CALL fn_mensaje("ERROR", "Fecha posterior a fecha actual", "information")
            NEXT FIELD v_de_Fecha
         ELSE
            --Valida que el folio capturado exista en la tabla de histórico
            CALL fn_valida_parametros_ajuste(v_edit_Folio, v_de_Fecha) 
            RETURNING v_valida_folio, v_valida_fecha

            --Si el folio no existe en el histórico envia mensaje 
            IF (v_valida_folio IS NULL OR v_valida_folio = 0) THEN
               CALL fn_mensaje("ERROR", "No existe información con los datos capturados", "information")
               NEXT FIELD v_edit_Folio
            ELSE
               CALL fn_obtiene_registros_afectados()
            END IF
         END IF

         ####################################################################
         IF ( v_valida_folio > 0 OR v_valida_fecha > 0 ) THEN 
            CALL f_forma.setElementHidden("gr_resregavpag", 0) --Muestra la Sección Resúmen Registro Av. Pagos
            CALL f_forma.setElementHidden("reverso", 0)        --Muestra el botón de Reverso
            CALL f_forma.setElementHidden("accept", 1)         --Oculta el botón de Aceptar  

            DISPLAY ARRAY arr_Avances TO scr_ResAvPag.*
            ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
              --Si se solicita la ejecución del reverso 
              ON ACTION reverso
                 LET v_edit_Folio = arr_Avances[arr_curr()].folio

                 --Consulta si hay registros con avance cerrado
                 SELECT COUNT (*) 
                 INTO   v_valida_estado
                 FROM   dis_det_avance_pago
                 WHERE  folio  = v_edit_Folio
                 AND    estado = 50

                 --Si hay registros con avance cerrado, no ejecuta reverso      
                 IF v_valida_estado > = 1 THEN 
                    CALL fn_mensaje ("ATENCIÓN", 
                                     "No se puede ejecutar el reverso, folio ya tiene avances cerrados",
                                     "information")
                    EXIT PROGRAM 
                 ELSE    
                    CALL fn_ventana_confirma("REVERSO", 
                                             "¿Está seguro que desea realizar el reverso?", 
                                             "quest")
                    RETURNING v_respuesta

                    --Se obtiene pid de la operación de acuerdo al folio
                    SELECT DISTINCT pid 
                    INTO   p_pid
                    FROM   bat_ctr_operacion
                    WHERE  folio = v_edit_Folio

                    DISPLAY "Pid: ", p_pid
                        
                    --Si no existe el pid de acuerdo al folio, traerá el último
                    IF p_pid IS NULL OR p_pid = 0 THEN
                       CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid 
                    END IF
                     
                    --Llama la función para validar la ejecución del reverso
                    CALL fn_valida_reverso(p_pid, p_proceso_cod, p_opera_cod)
                    RETURNING r_bandera_rev

                    DISPLAY "band: ",r_bandera_rev

                    IF r_bandera_rev  <> 0 THEN
                       CALL fn_muestra_inc_operacion(r_bandera_rev)
                       EXIT PROGRAM 
                    END IF 

                    DISPLAY "v_respuesta: ",v_respuesta
                                    
                    --Si el usuario confirma la ejecución del reverso
                    IF v_respuesta = 1 THEN 
                       --valida si la póliza contable ya fue generada, de lo contrario, rechaza el reverso
                       LET v_QryTxt = "EXECUTE PROCEDURE fn_reverso_reg_cnt(?,?)"
                           
                       PREPARE prp_reverso_reg_cnt FROM v_QryTxt
                       EXECUTE prp_reverso_reg_cnt USING v_edit_Folio,p_proceso_cod INTO r_bnd_cnt, r_tipo_rev_cnt

                       DISPLAY "r_bnd_cnt: ",r_bnd_cnt
                       DISPLAY "r_tipo_rev_cnt: ",r_tipo_rev_cnt
                           
                       IF r_bnd_cnt = 0 THEN --Si el registro contable no manda error, entonces:
                          CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
                       
                          LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                                          "/DISR091.42r ",p_usuario," ",p_pid," ",
                                          p_proceso_cod," ",p_opera_cod," ",v_edit_Folio," ",
                                          " '","NA","' 1>", v_ruta_listados CLIPPED ,
                                          "/nohup:",p_pid USING "&&&&&",":",
                                          p_proceso_cod USING "&&&&&",":",
                                          p_opera_cod USING "&&&&&" ," 2>&1 &"
                          RUN l_comando

                          LET v_s_mensaje = "Se ha enviado el Reverso del Ajuste de Avances Abiertos con el pid: ",
                                            p_pid CLIPPED,
                                            ".\nPuede revisar el estado del proceso en el monitor de ejecución de procesos."
                          CALL fn_mensaje("Reverso",v_s_mensaje,"information")

                          EXIT PROGRAM 
                       ELSE 
                          IF r_bnd_cnt = 1 THEN 
                             CALL fn_mensaje("No se puede realizar el reverso","No existe el registro contable del proceso","error")
                          END IF

                          IF r_bnd_cnt = 2 THEN 
                             CALL fn_mensaje("No se puede realizar el reverso","La fecha de emisión del registro contable es diferente al día de hoy","error")
                          END IF

                          IF r_bnd_cnt = 3 THEN 
                             CALL fn_mensaje("No se puede realizar el reverso","La póliza contable ya fue generada","error")
                          END IF

                          IF r_bnd_cnt = 4 THEN 
                             CALL fn_mensaje("No se puede realizar el reverso","Periodo Contable Cerrado","error")
                          END IF

                          IF r_bnd_cnt = 5 THEN 
                             CALL fn_mensaje("No se puede realizar el reverso","La póliza no ha sido confirmada","error")
                          END IF    
                       END IF 
                    ELSE 
                       CALL fn_mensaje("REVERSO","Se ha cancelado el reverso","information")
                       EXIT PROGRAM 
                    END IF
                 END IF 

                 ON ACTION cancelar
                    CALL fn_mensaje("REVERSO","Se ha cancelado el reverso","information")
                    EXIT PROGRAM 
                    
            END DISPLAY  
         ELSE 
            --Si no existen registros 
            CALL fn_mensaje("Error", "No existe información para estos criterios de búsqueda", "information") 
         END IF 
    END INPUT 
END MAIN 
         
#OBJETIVO: Validar que el folio y la fecha capturados sean validos
FUNCTION fn_valida_parametros_ajuste(p_folio,p_fecha)
DEFINE 
  p_folio                    DECIMAL(9,0),
  p_fecha                    DATE,
  v_folio_valido             LIKE dis_det_avance_pago.folio,
  v_fecha_valida             LIKE dis_det_avance_pago.f_presentacion,--DATE,
  v_qry_txt                  STRING

  LET v_qry_txt = "\n SELECT folio, max(f_presentacion)",
                  "\n FROM   dis_det_avance_pago",
                  "\n WHERE  estado = 80"  --Buscamos el estado ajuste de avances
                   
  IF length(p_folio) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n AND folio = ",p_folio
  END IF
      
  IF length(p_fecha) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n AND f_presentacion = '",p_fecha,"'"
  END IF

  --Concatena criterios de búsqueda
  LET v_qry_txt = v_qry_txt||"\n GROUP BY folio"
      
  PREPARE prp_valida_folio_dis_det FROM v_qry_txt
  EXECUTE prp_valida_folio_dis_det INTO v_folio_valido,v_fecha_valida

  --Si no existen registros en dis_det_avance_pago, busca en los rechazados
  IF v_folio_valido = 0 OR v_fecha_valida IS NULL OR v_folio_valido IS NULL THEN 
     LET v_qry_txt = "\n SELECT folio, f_presentacion",
                     "\n FROM   dis_rch_avance_pago",
                     "\n WHERE  estado in (82,83,84)"
                         
     IF length(p_folio) > 0 THEN
        LET v_qry_txt = v_qry_txt||"\n AND folio = ",p_folio
     END IF
            
     IF length(p_fecha) > 0 THEN
        LET v_qry_txt = v_qry_txt||"\n AND f_presentacion = '",p_fecha,"'"
     END IF
            
     LET v_qry_txt = v_qry_txt||"\n GROUP BY folio, f_presentacion"
     PREPARE prp_valida_folio_dis_rch FROM v_qry_txt
     EXECUTE prp_valida_folio_dis_rch INTO v_folio_valido,v_fecha_valida
  END IF

  RETURN v_folio_valido,v_fecha_valida
   
END FUNCTION

--Obtiene los registros que fueron afectados por la integración del ajuste de avances abiertos,
--así como los rechazados.
FUNCTION fn_obtiene_registros_afectados()

  LET v_registros_det = 0
  LET v_registros_rch = 0

  IF v_edit_Folio = 0 OR v_edit_Folio IS NULL THEN 
     --Obtiene registros por la fecha
     SELECT COUNT(id_dis_det_avance_pago)
     INTO   v_registros_det
     FROM   dis_det_avance_pago
     WHERE  f_pago = v_de_Fecha

     --Obtiene registros por la fecha
     SELECT COUNT(id_dis_rch_avance)
     INTO   v_registros_rch
     FROM   dis_rch_avance_pago
     WHERE  f_pago = v_de_Fecha
  ELSE 
     --Obtiene registros por folio
     SELECT COUNT(id_dis_det_avance_pago)
     INTO   v_registros_det
     FROM   dis_det_avance_pago
     WHERE  folio = v_edit_Folio

     --Obtiene registros por folio
     SELECT COUNT(id_dis_rch_avance)
     INTO   v_registros_rch
     FROM   dis_rch_avance_pago
     WHERE  folio = v_edit_Folio
  END IF

  --Valida que no haya nulos
  IF v_registros_rch IS NULL THEN 
     LET v_registros_rch = 0 
  END IF

  IF v_registros_det IS NULL THEN 
     LET v_registros_det = 0 
  END IF  
   
  LET v_registros_tot               = v_registros_rch + v_registros_det

  LET arr_Avances[1].folio          = v_edit_Folio
  LET arr_Avances[1].tot_registros  = v_registros_tot
  LET arr_Avances[1].tot_rechazados = v_registros_rch
  LET arr_Avances[1].tot_aceptados  = v_registros_det
   
END FUNCTION