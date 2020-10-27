--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-04-2012
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTL06                                                   #
#Objetivo          => SOLICITUD VALIDACION A SUSTENTABILIDAD                   #
#Autor             => Hugo César Ramírez Gracía                                #
#Fecha Inicio      => 15/02/2012                                               #
################################################################################
DATABASE safre_viv
DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_ruta_bin     LIKE seg_modulo.ruta_bin,       
       p_titulo       STRING,   -- titulo de la ventana
       v_ventana      ui.Window

# Objetivo:
MAIN
DEFINE p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa       
       v_total_solicitudes DYNAMIC ARRAY OF RECORD
         v_tot_sol         INTEGER
       END RECORD,
       v_aux_solicitudes   INTEGER 
   
   -- se recupera la clave de usuario desde parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   # Recupera la ruta de los archivos ejecutables para mandatos
   SELECT ruta_bin
     INTO g_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = "mdt"
    
   LET v_total_solicitudes[1].v_tot_sol = 0

   SELECT COUNT(*)
     INTO v_aux_solicitudes 
     FROM mdt_solicitud_mandato
    WHERE estado = 101
      AND id_origen = 2


   LET v_total_solicitudes[1].v_tot_sol = v_aux_solicitudes

         
   OPEN WINDOW w_solicitud_mandatos WITH FORM g_ruta_bin CLIPPED||"/MDTL061"
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
                               "No Hay Registros para generar archivo",
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

###############################################################################
#Modulo       => MDT                                                          #
#Programa     => MDTL06                                                       #
#Objetivo     =>                                                              #
#                                                                             #
#Autor        => Hugo César Ramírez Gracía                                    #
#Fecha Inicio => 15 Febrero 2012                                              #
###############################################################################
FUNCTION fn_muestra_detalle_solicitudes()
DEFINE v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,
       v_detalle_solicitudes DYNAMIC ARRAY OF RECORD
          v_mandato          LIKE mdt_cat_mandato.desc_mandato,
          v_fecha_ingreso    LIKE mdt_solicitud_mandato.f_canales,
          v_total            INTEGER
       END RECORD,
       v_consulta            STRING,
       v_indice              INTEGER,
       v_comando             STRING,
       v_pid                 LIKE glo_pid.pid,
       v_proceso             LIKE cat_proceso.proceso_cod,
       v_operacion           LIKE cat_operacion.opera_cod,
       v_ruta_listados       LIKE seg_modulo.ruta_listados,
       r_bandera             SMALLINT,
       v_cadena              STRING
DEFINE v_folio               LIKE glo_ctr_archivo.folio
DEFINE v_archivo             VARCHAR(80)


   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "mdt"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "bat"

   LET v_proceso = 1305 
   LET v_operacion = 1
     
   OPEN WINDOW w_detalle_solicituides WITH FORM v_ruta_ejecutable CLIPPED||"/MDTL062"
      #Se asigna el titulo de la ventana
      IF(p_titulo IS NOT NULL)THEN
          CALL ui.Interface.setText(p_titulo)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo)
      END IF
      DISPLAY ARRAY v_detalle_solicitudes TO tbl_solicitudes.* 
                                          ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
        BEFORE DISPLAY
           LET v_consulta = "\n SELECT mcat.desc_mandato, sol.f_canales, COUNT(sol.id_solicitud_mandato)",
                            "\n   FROM mdt_solicitud_mandato sol LEFT OUTER JOIN ",
                            "\n        mdt_cat_mandato_paquete paq ",
                            "\n     ON paq.cve_mandato = sol.cve_mandato",
                            "\n        JOIN mdt_cat_mandato mcat ",
                            "\n     ON mcat.id_cat_mandato = paq.id_cat_mandato",
                            "\n  WHERE sol.estado = 101",
                            "\n    AND sol.id_origen = 2",
                            "\n  GROUP BY 1,2 "
           PREPARE prp_detalle_solicitudes FROM v_consulta
           DECLARE cur_detalle_solicitudes CURSOR FOR prp_detalle_solicitudes
           LET v_indice = 1
           FOREACH  cur_detalle_solicitudes INTO v_detalle_solicitudes[v_indice].*
              LET v_indice = v_indice + 1
           END FOREACH
           IF(v_detalle_solicitudes[v_detalle_solicitudes.getLength()].v_mandato IS NULL)THEN
              CALL v_detalle_solicitudes.deleteElement(v_detalle_solicitudes.getLength()) 
           END IF

        ON ACTION Aceptar
           LET v_pid = 0
           CALL fn_valida_operacion(v_pid,v_proceso,v_operacion) RETURNING r_bandera
           IF(r_bandera <> 0)THEN
              # Imprime el mensaje de inconsistencia en consola y archivo
              CALL fn_muestra_inc_operacion(r_bandera)
           ELSE
              #Genera identificador del proceso
              CALL  fn_genera_pid(v_proceso, v_operacion,p_usuario_cod)
                                 RETURNING v_pid
              IF(SQLCA.SQLCODE <> 0)THEN
                 CALL fn_mensaje(p_titulo, 
                                 "Ocurrió un error al generar PID",
                                 "about")
                 DISPLAY "Error al generar pid (Codigo):",SQLCA.SQLCODE
                 
              END IF
              #inicializa el proceso con todas sus operaciones en estado LISTO
              CALL fn_inicializa_proceso(v_pid, 
                                         v_proceso, 
                                         v_operacion, 
                                         0,
                                         "MDTL06",
                                         "", 
                                         p_usuario_cod) 
                                  RETURNING r_bandera
              IF(r_bandera)THEN
                 # Imprime el mensaje de inconsistencia en consola y archivo
                 CALL fn_muestra_inc_operacion(r_bandera)
                 CALL fn_error_opera(v_pid,v_proceso,v_operacion)
                                    RETURNING r_bandera
                 IF(r_bandera)THEN
                    # Imprime el mensaje de inconsistencia en consola y archivo
                    CALL fn_muestra_inc_operacion(r_bandera)
                     EXIT DISPLAY           
                 END IF
                 EXIT DISPLAY           
              END IF
              --Se marca el proceso como inciado
              CALL fn_actualiza_opera_ini (v_pid,
                                           v_proceso,
                                           v_operacion,
                                           0,
                                           "MDTL06","",
                                           p_usuario_cod)
                                  RETURNING r_bandera
              IF(r_bandera)THEN
                 # Imprime el mensaje de inconsistencia en consola y archivo
                 CALL fn_muestra_inc_operacion(r_bandera)
                 CALL fn_error_opera(v_pid,v_proceso,v_operacion)
                                    RETURNING r_bandera
                 IF(r_bandera)THEN
                    # Imprime el mensaje de inconsistencia en consola y archivo
                    CALL fn_muestra_inc_operacion(r_bandera)
                     EXIT DISPLAY           
                 END IF
                 EXIT DISPLAY           
              END IF                    

              #Construye comando
              LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/MDTS06.42r ",
                                              p_usuario_cod," ",v_pid," ",v_proceso," ",
                                              v_operacion," ",v_folio," ",v_archivo,
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
                 LET v_cadena = "Se generó el proceso no. ",v_cadena USING "&&&&&"
                 CALL fn_mensaje(p_titulo, 
                                 v_cadena ,
                                 "about")
              END IF
           END IF
           EXIT DISPLAY

        ON ACTION Cancelar
           EXIT DISPLAY

      END DISPLAY
      
   CLOSE WINDOW w_detalle_solicituides 
   
END FUNCTION
