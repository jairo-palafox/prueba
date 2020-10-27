################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 09/10/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR26                                                        #
#Objetivo     => Programa que ejecuta el reverso de la carga del archivo de    #
#                cancelación parcial de avances de pago.                       #
#Fecha inicio => 04/10/2018                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE arr_det_crt         DYNAMIC ARRAY OF RECORD
    r_folio_det_ctr          DECIMAL(9,0),
    r_tot_imp_apo_pat        DECIMAL(12,2),
    r_tot_imp_amo_cred       DECIMAL(12,2),
    r_estado                 CHAR(25)
  END RECORD

END GLOBALS
MAIN
  --Sección de Variables Locales
  DEFINE 
     v_folio_carga           DECIMAL(9,0),--Folio de Carga de Archivo Canc PAV
     v_des_error             CHAR(150)    --Recibe la descripción del error 
     
  --Sección de Parámetros
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, --Clave del usuario
    p_tipo_ejecucion         SMALLINT, --Forma como ejecutara el programa
    p_s_titulo               STRING,   --Titulo de la ventana
    p_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    p_proceso_cod            LIKE cat_proceso.proceso_cod, --Código del proceso
    p_opera_cod              LIKE cat_operacion.opera_cod  --Código de operación
    
  --Seccion de Variables de Retorno
  DEFINE  
    r_folio_valido           DECIMAL(9,0),
    r_bandera                SMALLINT,
    des_bandera              CHAR(100),
    r_nom_archivo            CHAR(40)
                                                             
  DEFINE 
    f_ventana                ui.Window,  --Define las propìedades de la Ventana
    f_forma                  ui.Form     --Define las propiedades de la forma

  DEFINE 
    v_ruta_ejecutable        LIKE seg_modulo.ruta_bin,     --Ruta del ejecutable
    v_ruta_listados          LIKE seg_modulo.ruta_listados,--Ruta del log
    v_s_mensaje              STRING,
    l_comando                STRING,
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING,
    r_indice_ctr             INTEGER
       
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  LET p_proceso_cod    = 939
  LET p_opera_cod      = 1

  --Obtiene el nombre del archivo a reversar
  CALL fn_recupera_arch_cargado(p_proceso_cod, p_opera_cod)
  RETURNING r_nom_archivo

  --Llama la función para obtener el PID
  --CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid   
   
  --Si se obtuvo el titulo, se pone como titulo de programa
  IF (p_s_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(p_s_titulo)
  END IF

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

  CLOSE WINDOW SCREEN
  
  OPEN WINDOW vtn_reverso_cpav WITH FORM "DISR261"   
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_carga
      FROM  f_folio_carga_cpav
    
        --Oculta secciones de la forma
        BEFORE INPUT 
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma  = f_ventana.getForm()
          CALL DIALOG.setActionHidden("reverso",1)
          CALL f_forma.setElementHidden("gr_detalle",1)
      END INPUT

      DISPLAY ARRAY arr_det_crt TO scr_detalle.* 
      END DISPLAY
      
      ON ACTION ACCEPT            
         --Valida que el folio capturado exista en la tabla de histórico
         CALL fn_valida_folio_cpav(v_folio_carga)
         RETURNING r_folio_valido
                              
         --Si el folio no existe en el histórico envia mensaje 
         IF r_folio_valido = 0 OR  r_folio_valido IS NULL  THEN
            CALL fn_mensaje("ATENCIÓN", 
                            "No hay datos con los parametros capturados",
                            "about")
            NEXT FIELD f_folio_carga_cpav
         ELSE               
            --Ejecuta funcion de consulta de totales 
            CALL fn_consulta_cifras(r_folio_valido)
         END IF

         CALL DIALOG.setActionHidden("accept",1)
         CALL DIALOG.setActionHidden("reverso",0)
         CALL f_forma.setElementHidden("gr_detalle",0)

      ON ACTION reverso
         --LET v_folio_carga = arr_det_crt[arr_curr()].r_folio_det_ctr

         --Se obtiene pid de la operación de acuerdo al folio
         SELECT DISTINCT pid 
         INTO   p_pid
         FROM   bat_ctr_operacion
         WHERE  folio = v_folio_carga

         --Si no existe el pid de acuerdo al folio, traerá el último
         IF p_pid IS NULL OR p_pid = 0 THEN
            CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid 
         END IF 
      
         --Llama la función para validar la ejecucución del reverso
         CALL fn_valida_reverso(p_pid, p_proceso_cod, 2) RETURNING r_bandera
         
         SELECT descripcion 
         INTO   des_bandera
         FROM   cat_bat_parametro_salida
         WHERE  cod_salida = r_bandera             
         DISPLAY "BANDERA REVERSO ",des_bandera

         --Si el reverso no tiene ningún bloqueo
         IF r_bandera = 0 THEN   
            --Mensaje para confirmar(0) o cancelar(1) el reverso 
            CALL fn_ventana_confirma ("REVERSO","¿Desea ejecutar el reverso?",
                                      "question") RETURNING r_bandera

            IF r_bandera = 1 THEN
               CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados

               LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                               "/DISR261.42r ",p_usuario_cod," ",p_pid," ",
                               p_proceso_cod," ",p_opera_cod," ",r_folio_valido," ",
                               " '","NA","' 1>", v_ruta_listados CLIPPED ,
                               "/nohup:",p_pid USING "&&&&&",":",
                                         p_proceso_cod USING "&&&&&",":",
                                         p_opera_cod USING "&&&&&" ," 2>&1 &"
               RUN l_comando
DISPLAY "l_comando: ", l_comando
               LET v_s_mensaje = "Se ha enviado el Reverso de la Carga del Archivo de Cancelación Parcial Avances de Pago con el pid: ",
                   p_pid CLIPPED,
                  ".\nPuede revisar el estado del proceso en el monitor de ejecución de procesos."

               CALL fn_mensaje("Reverso",v_s_mensaje,"information")

               EXIT PROGRAM 
            ELSE
              CALL fn_mensaje("REVERSO","Se ha cancelado el reverso","about")
            END IF
         ELSE --Si se detecta algún bloqueo
            --Se realiza consulta de descripción en base al código de bloqueo
            SELECT descripcion 
            INTO   v_des_error 
            FROM   cat_bat_parametro_salida
            WHERE  cod_salida = r_bandera

            CALL fn_mensaje("REVERSO", v_des_error, "information")
         END IF

         EXIT DIALOG

      ON ACTION cancelar
         EXIT DIALOG

    END DIALOG 
  CLOSE WINDOW vtn_reverso_cpav 
END MAIN

#OBJETIVO: Validar que el folio exista en tabla de proceso de cancelación
#          parcial de avances de pago
FUNCTION fn_valida_folio_cpav(p_folio)
  DEFINE 
    p_folio                  DECIMAL(9,0),
    r_folio_valido           DECIMAL(9,0),
    v_qry_txt                STRING

  LET v_qry_txt = "\n SELECT MAX(folio)",
                  "\n FROM   dis_canc_par_ava_pag"
                  
  IF length(p_folio) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n WHERE folio = ",p_folio
  END IF
                              
  --DISPLAY v_qry_txt               
  PREPARE prp_valida_folio_liquidado FROM v_qry_txt
  EXECUTE prp_valida_folio_liquidado INTO r_folio_valido

  RETURN r_folio_valido

END FUNCTION 

#OBJETIVO: Consultar que la información a reversar exista en la tabla de
#          cancelación parcial de avances de pago
FUNCTION fn_consulta_cifras(p_folio)
  DEFINE 
    p_folio                  DECIMAL(9,0),
    v_estado                 SMALLINT
                             
  DEFINE 
    v_indice_ctr             INTEGER,
    v_indice_det             INTEGER,
    v_QryTxt                 STRING 

  LET v_indice_ctr = 1
  LET v_indice_det = 1
  LET v_estado     = 0

  LET v_QryTxt = "\n SELECT folio,              ",
                 "\n        sum(monto_aportacion),   ",
                 "\n        sum(monto_amortizacion), ",
                 "\n        estado              ",
                 "\n FROM   dis_canc_par_ava_pag",
                 "\n WHERE  1=1"
  IF length(p_folio) > 0 THEN 
     LET v_QryTxt = v_QryTxt || "\n AND folio = ", p_folio
  END IF
     
  LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,4" || "\n ORDER BY 4"

  DISPLAY "v_QryTxt>>", v_QryTxt
  PREPARE prp_cifras_tot FROM v_QryTxt
  --Obtiene sumario para cifras globales de la carga del archivo
  DECLARE cur_det_crt_cpav CURSOR FOR prp_cifras_tot
  FOREACH cur_det_crt_cpav INTO arr_det_crt[v_indice_ctr].r_folio_det_ctr,
                                arr_det_crt[v_indice_ctr].r_tot_imp_apo_pat,
                                arr_det_crt[v_indice_ctr].r_tot_imp_amo_cred,
                                v_estado
    IF v_estado = 0 THEN
       LET arr_det_crt[v_indice_ctr].r_estado = v_estado, " - RECHAZADOS" 
    ELSE
       LET arr_det_crt[v_indice_ctr].r_estado = v_estado, " - ACEPTADOS"
    END IF

    LET v_indice_ctr = v_indice_ctr + 1
  END FOREACH 

  CALL arr_det_crt.deleteElement(v_indice_ctr)

END FUNCTION