--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07-jul-2015
--===============================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Modulo            => HPS                                                      #
#Programa          => HPSL21                                                   #
#Objetivo          => Integracion instrucciones de pagos de servicio HPS       #
#Autor             => Jesus David Yañez Moreno                                 #
#Fecha Creacion    => Marzo 16 2015                                            #
################################################################################

DATABASE safre_viv
DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod,
       g_ruta_bin     LIKE seg_modulo.ruta_bin,       
       p_titulo       STRING,
       v_ventana      ui.Window,
       v_pid          LIKE glo_pid.pid,
       v_proceso             LIKE cat_proceso.proceso_cod,
       v_operacion           LIKE cat_operacion.opera_cod,
       r_bandera             SMALLINT

# Objetivo:
MAIN
DEFINE p_tipo_ejecucion    SMALLINT,
       v_total_solicitudes DYNAMIC ARRAY OF RECORD
         v_tot_sol         INTEGER
       END RECORD,
       v_aux_solicitudes   INTEGER 
   
   # Se recupera la clave de usuario desde parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)


   LET v_proceso = 3104 # validacion mandatos origen recurrente
   LET v_operacion = 2
   # Recupera la ruta de los archivos ejecutables para mandatos
   SELECT ruta_bin
     INTO g_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = "hps"
    
   LET v_total_solicitudes[1].v_tot_sol = 0

   SELECT COUNT(*)
     INTO v_aux_solicitudes 
     FROM safre_tmp:tmp_hps_det_instrucciones
    --WHERE estado = 101
    --  AND id_origen = 2

   # Recupera el pid
   CALL fn_max_pid(v_proceso,v_operacion) RETURNING v_pid

   # Valida que se pueda ejecutar la operación 
   CALL fn_valida_operacion(v_pid,v_proceso,v_operacion) RETURNING r_bandera
           
   IF(r_bandera <> 0)THEN
      # Imprime el mensaje de inconsistencia en consola y archivo
      CALL fn_muestra_inc_operacion(r_bandera)
      EXIT PROGRAM
   END IF

   LET v_total_solicitudes[1].v_tot_sol = v_aux_solicitudes
         
   OPEN WINDOW w_solicitud_mandatos WITH FORM g_ruta_bin CLIPPED||"/HPSL211"
      #Se asigna el titulo de la ventana
      IF(p_titulo IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo)
      END IF
      DISPLAY ARRAY v_total_solicitudes TO tbl_solicitudes.* 
                     ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         ON ACTION Aceptar
            IF(v_total_solicitudes[1].v_tot_sol <= 0 )THEN
               CALL fn_mensaje(p_titulo, 
                               "No Hay Registros para integrar",
                               "about")   
            ELSE
               CALL fn_muestra_detalle_solicitudes()
            END IF            
            EXIT DISPLAY

         ON ACTION CLOSE -- Cancelar
           EXIT DISPLAY

      END DISPLAY
   CLOSE WINDOW w_solicitud_mandatos
END MAIN

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSL06                                                        #
#Objetivo     => Muestra detealle de mandatos                                  #
#Autor        => Hugo César Ramírez Gracía                                     #
#Fecha Inicio => 11 Junio 2012                                                 #
################################################################################
FUNCTION fn_muestra_detalle_solicitudes()
DEFINE v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,
       v_detalle_sol_aux     RECORD
          v_mandato          LIKE mdt_cat_mandato.desc_mandato,
          v_fecha_ingreso    LIKE safre_tmp:tmp_acr_transf_30.fec_ini_mandato,
          v_total            INTEGER
       END RECORD,
       v_detalle_solicitudes DYNAMIC ARRAY OF RECORD
          v_mandato          LIKE safre_tmp:tmp_acr_transf_30.cve_mandato,
          v_fecha_ingreso    STRING,-- DATE,
          v_total            INTEGER
       END RECORD,
       v_consulta            STRING,
       v_indice              INTEGER,
       v_comando             STRING,
       
       
       v_ruta_listados       LIKE seg_modulo.ruta_listados,
       
       v_cadena              STRING
DEFINE v_folio               LIKE glo_ctr_archivo.folio
DEFINE v_archivo             LIKE glo_ctr_archivo.nombre_archivo

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "bat"
   
   
     
   OPEN WINDOW w_detalle_solicituides WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL212"
      #Se asigna el titulo de la ventana
      IF(p_titulo IS NOT NULL)THEN
          CALL ui.Interface.setText(p_titulo)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo)
      END IF
      DISPLAY ARRAY v_detalle_solicitudes TO tbl_solicitudes.* 
                                          ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
        BEFORE DISPLAY
           LET v_consulta = "\n SELECT rec.cve_mandato, rec.fec_ini_mandato, COUNT(rec.nss)",
                            "\n FROM safre_tmp:tmp_hps_det_instrucciones rec ",
                            "\n GROUP BY 1,2 "
           #SDL -- Muevo el segmento no utiliado de la consulta
                            --"\n        mdt_cat_mandato mcat ",
                            --"\n     ON mcat.cve_mandato = rec.cve_mandato[12,18]", -- TMP Por definir longitud final cve_mandato
                            --"\n  WHERE sol.estado = 101",
                            --"\n    AND sol.id_origen = 2",           
           PREPARE prp_detalle_solicitudes FROM v_consulta
           DECLARE cur_detalle_solicitudes CURSOR FOR prp_detalle_solicitudes
           LET v_indice = 1
           FOREACH  cur_detalle_solicitudes INTO v_detalle_sol_aux.* 
              LET v_detalle_solicitudes[v_indice].v_mandato       = v_detalle_sol_aux.v_mandato 
              LET v_detalle_solicitudes[v_indice].v_fecha_ingreso = v_detalle_sol_aux.v_fecha_ingreso-- USING "MM-DD-YYYY" 
              LET v_detalle_solicitudes[v_indice].v_total         = v_detalle_sol_aux.v_total
              LET v_indice = v_indice + 1
           END FOREACH
           IF(v_detalle_solicitudes[v_detalle_solicitudes.getLength()].v_total IS NULL)THEN
              CALL v_detalle_solicitudes.deleteElement(v_detalle_solicitudes.getLength()) 
           END IF

        ON ACTION Aceptar
          -- LET v_pid = 0
           
           # Valida que se pueda ejecutar la operación 
           CALL fn_valida_operacion(v_pid,v_proceso,v_operacion) RETURNING r_bandera
           
           IF(r_bandera <> 0)THEN
              # Imprime el mensaje de inconsistencia en consola y archivo
              CALL fn_muestra_inc_operacion(r_bandera)
           ELSE
              
              LET v_folio = 0
              CALL fn_actualiza_opera_ini(v_pid,v_proceso,v_operacion,v_folio,'HPSL21','',p_usuario_cod)
                    RETURNING r_bandera
              IF(r_bandera = 0)THEN
                 CALL fn_recupera_arch_cargado(v_proceso,v_operacion) RETURNING v_archivo
                 #Construye comando
                 LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/HPSP21.42r ",
                                                 p_usuario_cod," ",
                                                 v_pid," ",
                                                 v_proceso," ",
                                                 v_operacion," ",
                                                 v_folio," ",
                                                 v_archivo,
                                 " 1>", v_ruta_listados CLIPPED ,
                                 "/nohup:",v_pid USING "&&&&&",":",
                                           v_proceso USING "&&&&&",":",
                                           v_operacion   USING "&&&&&" ,
                                 " 2>&1 &"
                 RUN v_comando
                 DISPLAY v_comando 
                 IF(STATUS)THEN
                    CALL fn_mensaje(p_titulo, 
                                    "Ocurrió un error al iniciar el proceso batch",
                                    "about")
                    CALL fn_error_opera(v_pid,v_proceso,v_operacion)
                                       RETURNING r_bandera
                    IF(r_bandera)THEN
                       # Imprime el mensaje de inconsistencia en consola y archivo
                       CALL fn_muestra_inc_operacion(r_bandera)
                        EXIT DISPLAY           
                    END IF
                 ELSE
                    LET v_cadena = v_pid 
                    LET v_cadena = v_cadena.trim()
                    LET v_cadena = "Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos"
                    CALL fn_mensaje(p_titulo, 
                                    v_cadena ,
                                    "about")
                 END IF
              ELSE
                 # Imprime el mensaje de inconsistencia en consola y archivo
                 CALL fn_muestra_inc_operacion(r_bandera)
                 EXIT DISPLAY
              END IF
           END IF
           EXIT DISPLAY

        ON ACTION Cancelar
           EXIT DISPLAY

      END DISPLAY
      
   CLOSE WINDOW w_detalle_solicituides 
   
END FUNCTION
