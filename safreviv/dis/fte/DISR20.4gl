################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 28/03/2016                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR20                                                        #
#Objetivo     => Programa que ejecuta el reverso de aportaciones subsecuentes  #
#                sin adelanto.                                                 #
#Fecha inicio => 18/11/2015                                                    #
################################################################################
DATABASE safre_viv

MAIN
  --Sección de Variables Locales
  DEFINE 
     v_folio_carga_ap_sub    DECIMAL(9,0), --Folio de Carga de Archivo de Aportaciones Subsecuentes
     v_f_carga               DATE, -- Fecha de carga
     v_des_error             CHAR(150) -- Recibe la descripción del error 
     
  --Sección de Parámetros
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, -- Clave del usuario
    p_tipo_ejecucion         SMALLINT, -- Forma como ejecutara el programa
    p_s_titulo               STRING, -- Titulo de la ventana
    p_pid                    LIKE bat_ctr_proceso.pid, -- ID del proceso
    p_proceso_cod            LIKE cat_proceso.proceso_cod, -- Código del proceso
    p_opera_cod              LIKE cat_operacion.opera_cod -- Código de operacion
    
  --Seccion de Variables de Retorno
  DEFINE  
    r_folio_valido           DECIMAL(9,0),
    r_bandera                SMALLINT,
    des_bandera              CHAR(100),
    r_nom_archivo            CHAR(40)
                                
  DEFINE r_tot_registros     INTEGER 
  
  DEFINE arr_apo_sub         DYNAMIC ARRAY OF RECORD
    r_sum_apo_aiv_sub        DECIMAL(12,2),
    r_sum_apo_pat_sub        DECIMAL(12,2),
    r_folio_apo_sub          DECIMAL(9,0),
    r_f_actualiza            DATE, 
    r_periodo_pago           CHAR(04)
  END RECORD
                            
  DEFINE v_ret_rev           SMALLINT
  
  DEFINE 
    f_ventana                ui.Window,  -- Define las propìedades de la Ventana
    f_forma                  ui.Form     -- Define las propiedades de la forma

  DEFINE 
    v_ruta_ejecutable        LIKE seg_modulo.ruta_bin, -- Ruta del ejecutable
    v_ruta_listados          LIKE seg_modulo.ruta_listados, -- Rut del log
    v_s_mensaje              STRING,
    l_comando                STRING 
       
  LET p_proceso_cod = 933
  LET p_opera_cod   = 2

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  LET v_folio_carga_ap_sub = "" 
  LET v_f_carga = ""
  LET v_ret_rev     = 0

  --Obtiene el nombre del archivo a reversar
  CALL fn_recupera_arch_cargado(p_proceso_cod, p_opera_cod)
  RETURNING r_nom_archivo

  -- Llama la función para obtener el PID
  --CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid   
   
  -- si se obtuvo el titulo, se pone como titulo de programa
  IF (p_s_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(p_s_titulo)
  END IF

  CLOSE WINDOW SCREEN
  
  OPEN WINDOW vtn_reverso_apo_sub WITH FORM "DISR201"   
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_carga_ap_sub, v_f_carga
      FROM  f_folio_carga_ap_sub, f_fecha_carga
    
        --Oculta secciones de la forma
        BEFORE INPUT 
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma  = f_ventana.getForm()
          CALL DIALOG.setActionHidden("reverso",1)
          CALL f_forma.setElementHidden("gr_detalle",1)
          CALL f_forma.setElementHidden("gr_sumario",1)
          CALL f_forma.setElementHidden("gr_total",1)    
      END INPUT
      
      DISPLAY ARRAY arr_apo_sub TO scr_sumario.* END DISPLAY
      
      ON ACTION ACCEPT
         IF v_folio_carga_ap_sub IS NULL AND v_f_carga IS NULL THEN
            CALL fn_mensaje("ATENCIÓN",
                            "No ha capturado ningún criterio de búsqueda",
                            "about")
            NEXT FIELD f_folio_carga_ap_sub
         ELSE
            -- Valida que la fecha capturada no sea mayor a la fecha actual
            IF v_f_carga > TODAY THEN
               CALL fn_mensaje("Error","Fecha posterior a fecha actual","about")
               NEXT FIELD f_carga
            END IF
            
            --Valida que el folio capturado exista en la tabla de historico
            CALL fn_valida_folio_ap_sub(v_folio_carga_ap_sub,v_f_carga)
                              RETURNING r_folio_valido
                              
            -- Si el folio no existe en el histórico envia mensaje 
            IF r_folio_valido = 0 OR  r_folio_valido IS NULL  THEN
               CALL fn_mensaje("ATENCIÓN", 
                               "No hay datos con los parametros capturados",
                               "about")
               NEXT FIELD f_folio_carga_ap_sub
            ELSE               
               --Ejecuta funcion de consulta de totales 
               CALL fn_consulta_cifras(r_folio_valido, v_f_carga)
               RETURNING arr_apo_sub,r_tot_registros
            END IF
         END IF

      CALL DIALOG.setActionHidden("accept",1)
      CALL DIALOG.setActionHidden("reverso",0)
      CALL f_forma.setElementHidden("gr_detalle",0)
      CALL f_forma.setElementHidden("gr_sumario",0)
      CALL f_forma.setElementHidden("gr_total",0) 

      DISPLAY r_tot_registros TO tot_registros

      ON ACTION reverso        
         LET v_folio_carga_ap_sub = arr_apo_sub[arr_curr()].r_folio_apo_sub

         --Se obtiene pid de la operación de acuerdo al folio
         SELECT DISTINCT pid 
         INTO p_pid
         FROM bat_ctr_operacion
         WHERE folio = v_folio_carga_ap_sub

         --Si no existe el pid de acuerdo al folio, traerá el último
         IF p_pid IS NULL OR p_pid = 0 THEN
            CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid 
         END IF 
      
         -- Llama la función para validar la ejecución del reverso
         CALL fn_valida_reverso(p_pid, p_proceso_cod, 3) RETURNING r_bandera
         
         SELECT descripcion 
         INTO   des_bandera
         FROM   cat_bat_parametro_salida
         WHERE  cod_salida = r_bandera             
         --DISPLAY "BANDERA REVERSO ",des_bandera

         -- Si el reverso no tiene ningún bloqueo
         IF r_bandera = 0 THEN   
            --Mensaje para confirmar(1) o cancelar(0) el reverso 
            CALL fn_ventana_confirma ("REVERSO","¿Desea ejecutar el reverso?",
                                      "question") RETURNING r_bandera

            IF r_bandera = 1 THEN
               CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
                  
               LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                               "/DISR201.42r ",p_usuario_cod," ",p_pid," ",
                               p_proceso_cod," ",p_opera_cod," ",r_folio_valido," ",
                               " '","NA","' 1>", v_ruta_listados CLIPPED ,
                               "/nohup:",p_pid USING "&&&&&",":",
                                         p_proceso_cod USING "&&&&&",":",
                                         p_opera_cod USING "&&&&&" ," 2>&1 &"
               RUN l_comando

               LET v_s_mensaje = "Se ha enviado el Reverso de Aportaciones Subsecuentes Sin Conciliar con el pid: ",
                   p_pid CLIPPED,
                  ".\nPuede revisar el estado del proceso en el monitor de ejecución de procesos."

               CALL fn_mensaje("Reverso",v_s_mensaje,"information")

               EXIT PROGRAM 
            ELSE
              CALL fn_mensaje("REVERSO","Se ha cancelado el reverso","about")
            END IF
         ELSE-- Si se detecta algún bloqueo
            --Se realiza consulta de descripción en base al codigo de bloqueo
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

  CLOSE WINDOW vtn_reverso_apo_sub 

END MAIN

#OBJETIVO: Validar que el folio exista en tabla de proceso de aportaciones
#          subsecuentes sin conciliar
FUNCTION fn_valida_folio_ap_sub(p_folio,p_fecha)
  DEFINE 
    p_folio                  DECIMAL(9,0),
    p_fecha                  DATE,
    r_folio_valido           DECIMAL(9,0),--LIKE dis_ctr_ap_subsecuente.folio,
    v_qry_txt                STRING

  LET v_qry_txt = "\n SELECT MAX(ac.folio_ap_subs)", --INTO r_folio_valido
                  "\n FROM dis_as_sin_conciliar ac,",
                  "\n      glo_ctr_archivo gc ",
                  "\n WHERE ac.ind_concilia = 2 ",
                  "\n AND ac.folio_ap_subs = gc.folio "
                  
  IF length(p_folio) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n AND ac.folio_ap_subs = ",p_folio
  END IF
               
  IF length(p_fecha) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n AND gc.f_actualiza = '",p_fecha,"'"
  END IF
               
  --DISPLAY v_qry_txt               
  PREPARE prp_valida_folio_liquidado FROM v_qry_txt
  EXECUTE prp_valida_folio_liquidado INTO r_folio_valido

  RETURN r_folio_valido

END FUNCTION 

#OBJETIVO: Consultar que la información a reversar exista en la tabla de
#          aportaciones subsecuentes sin conciliar
FUNCTION fn_consulta_cifras(p_folio, p_fecha)
  DEFINE 
    p_folio                  DECIMAL(9,0),
    p_fecha                  DATE             
                             
  DEFINE arr_apo_sub         DYNAMIC ARRAY OF RECORD
    r_sum_apo_aiv_sub        DECIMAL(12,2),
    r_sum_apo_pat_sub        DECIMAL(12,2),    
    r_folio_apo_sub          DECIMAL(9,0),
    r_f_actualiza            DATE, 
    r_periodo_pago           CHAR(04)
  END RECORD
  
  DEFINE
    v_indice_det             INTEGER,
    v_QryTxt                 STRING 
  
  LET v_indice_det = 1              

  LET v_QryTxt = "\n SELECT SUM(ac.imp_apo_aivs),",
                 "\n        SUM(ac.imp_apo_pat),",
                 "\n        ac.folio_ap_subs,",
                 "\n        gc.f_actualiza,",
                 "\n        ac.periodo_pago ",
                 "\n   FROM dis_as_sin_conciliar ac ,",  
                 "\n        glo_ctr_archivo gc ", 
                 "\n  WHERE ind_concilia = 2 ",
                 "\n    AND gc.folio = ac.folio_ap_subs "

   IF p_folio IS NOT NULL THEN    
      LET v_QryTxt = v_QryTxt,"\n   AND gc.folio =", p_folio
   END IF

   IF p_fecha IS NOT NULL THEN 
      LET v_QryTxt = v_QryTxt,"\n   AND gc.f_actualiza ='",p_fecha,"'"
   END IF
      
   LET v_QryTxt = v_QryTxt, "\n GROUP BY 3,4,5",
                  "\n ORDER BY gc.f_actualiza DESC"

   --DISPLAY "v_QryTxt: ",v_QryTxt
   
  --Obtiene detalles para cifras globales de la carga del archivo
  PREPARE prp_cifras_sum FROM v_QryTxt
  DECLARE cur_det_apo_sub CURSOR FOR prp_cifras_sum
  FOREACH cur_det_apo_sub INTO arr_apo_sub[v_indice_det].r_sum_apo_aiv_sub,
                               arr_apo_sub[v_indice_det].r_sum_apo_pat_sub,
                               arr_apo_sub[v_indice_det].r_folio_apo_sub,
                               arr_apo_sub[v_indice_det].r_f_actualiza,
                               arr_apo_sub[v_indice_det].r_periodo_pago
    LET v_indice_det = v_indice_det + 1
  END FOREACH

  CALL arr_apo_sub.deleteElement(v_indice_det) 
  LET v_indice_det = v_indice_det -1 

  RETURN arr_apo_sub,v_indice_det

END FUNCTION
