--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 25/06/2012
--===============================================================

################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPL21                                                        #
#Objetivo     => Lanzador de generacion de avisos de suspensión                #
#Autor        => Hugo César Ramírez Gracía                                     #
#Fecha inicio => Junio 25, 2012                                                #
################################################################################
DATABASE safre_viv
DEFINE r_pid            LIKE bat_ctr_proceso.pid,     # ID del proceso
       v_proceso_cod    LIKE cat_proceso.proceso_cod, # Código del proceso
       v_opera_cod      LIKE cat_operacion.opera_cod, # Código de operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, # Clave del usuario firmado
       v_folio          LIKE glo_folio.folio,
       v_nom_archivo    STRING,
       p_tipo_ejecucion SMALLINT,                     # Forma como ejecutara el programa
       p_titulo_vtna    STRING,    
       v_ventana        ui.Window,
       v_forma          ui.Form,
       v_avisos_suspension DYNAMIC ARRAY OF RECORD
         v_conteo        INTEGER,
         v_id_expediente LIKE sep_aviso_suspension.id_expediente,
         v_delegacion    LIKE sep_aviso_suspension.delegacion,
         v_nombre        LIKE sep_aviso_suspension.nombre,
         v_rfc           LIKE sep_aviso_suspension.rfc,
         v_num_credito   LIKE sep_aviso_suspension.num_credito,
         v_nss           LIKE sep_aviso_suspension.nss,
         v_nrp           LIKE sep_aviso_suspension.nrp,
         v_empresa       LIKE sep_aviso_suspension.empresa,
         v_motivo        LIKE sep_aviso_suspension.motivo,
         v_descripcion   LIKE sep_aviso_suspension.descripcion
       END RECORD

MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   CALL fn_lanza_batch_aviso_suspensio()
 
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL21                                                   #
#Descripcion       =>                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 25, 2012                                             #
################################################################################
FUNCTION fn_lanza_batch_aviso_suspensio()
DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_lst        LIKE seg_modulo.ruta_listados,
       v_ruta            LIKE seg_modulo.ruta_listados,
       r_resultado_opera SMALLINT,
       r_continua        BOOLEAN,
       r_confirma        BOOLEAN,
       v_comando         STRING

   # Se asigna proceso y operación
   LET v_proceso_cod = 2217 # avisos de suspension 
   LET v_opera_cod   = 1
   LET v_nom_archivo = "NA"

   CALL fn_rutas("sep") RETURNING v_ruta_ejecutable,v_ruta
   CALL fn_rutas("bat") RETURNING v_ruta,v_ruta_lst

   OPEN WINDOW vtna_aviso_suspensio WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL211"

      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_vtna)         
         CALL v_ventana.setText(p_titulo_vtna)
      END IF

      CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
      # Se verifica si la operacion es valida
      IF(r_resultado_opera <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         EXIT PROGRAM
      ELSE
         CALL fn_recupera_avisos_suspencion() RETURNING r_continua
         IF(r_continua)THEN
            DISPLAY ARRAY v_avisos_suspension TO sr_avisos_suspension.*
               ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

               BEFORE DISPLAY
                 DISPLAY v_avisos_suspension.getLength() TO total_registros


               ON ACTION aceptar
                  # Se verifica si se puede ejecutar el proceso
                  CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
                  IF(r_resultado_opera <> 0)THEN
                     # En caso de error se muestra un mensaje a usuario y no continua
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                     CONTINUE DISPLAY
                  END IF
                  CALL fn_ventana_confirma(p_titulo_vtna,"Confirmar Generar Archivo","question")
                      RETURNING r_confirma
                  IF(r_confirma)THEN                     
                     CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod) 
                         RETURNING r_pid

                     CALL fn_inicializa_proceso(r_pid,
                                                v_proceso_cod,
                                                v_opera_cod,
                                                v_folio,
                                                "SEPL21",
                                                v_nom_archivo,
                                                p_usuario_cod)
                           RETURNING r_resultado_opera
                     IF(r_resultado_opera <> 0)THEN
                        CALL fn_muestra_inc_operacion(r_resultado_opera)
                        CONTINUE DISPLAY
                     END IF

                     CALL fn_actualiza_opera_ini(r_pid,
                                                 v_proceso_cod,
                                                 v_opera_cod,
                                                 v_folio,
                                                 "SEPL21",
                                                 v_nom_archivo,
                                                 p_usuario_cod)
                           RETURNING r_resultado_opera
                     IF(r_resultado_opera <> 0)THEN
                        CALL fn_muestra_inc_operacion(r_resultado_opera)
                        CONTINUE DISPLAY
                     END IF                            


                     LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPP21.42r ",
                                                     p_usuario_cod, " ",
                                                     r_pid, " ",
                                                     v_proceso_cod," ",
                                                     v_opera_cod," ",
                                                     v_folio, " ",
                                                     v_nom_archivo,
                                     " 1>", v_ruta_lst CLIPPED,
                                     "/nohup:",r_pid USING "&&&&&",":",
                                               v_proceso_cod USING "&&&&&",":",
                                               v_opera_cod USING "&&&&&",
                                     " 2>&1 &"
                  
                     DISPLAY v_comando
                     RUN v_comando
                     IF(STATUS)THEN
                        CALL fn_mensaje(p_titulo_vtna,"Ocurrio un error al ejecutar generación de avisos de suspensión","about")
                     ELSE
                        CALL fn_mensaje(p_titulo_vtna,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                     END IF
                     EXIT DISPLAY
                  ELSE
                     CONTINUE DISPLAY
                  END IF


               ON ACTION cancelar
                  EXIT DISPLAY


            END DISPLAY

         ELSE
            CALL fn_mensaje(p_titulo_vtna,"No se encontraron registros para avisos de suspensión","information")
         END IF
      END IF
   CLOSE WINDOW vtna_aviso_suspensio

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL21                                                   #
#Descripcion       =>                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_avisos_suspencion()
DEFINE v_consulta STRING,
       v_avisos_suspension_aux RECORD
         v_id_expediente LIKE sep_aviso_suspension.id_expediente,
         v_delegacion    LIKE sep_aviso_suspension.delegacion,
         v_nombre        LIKE sep_aviso_suspension.nombre,
         v_rfc           LIKE sep_aviso_suspension.rfc,
         v_num_credito   LIKE sep_aviso_suspension.num_credito,
         v_nss           LIKE sep_aviso_suspension.nss,
         v_nrp           LIKE sep_aviso_suspension.nrp,
         v_empresa       LIKE sep_aviso_suspension.empresa,
         v_motivo        LIKE sep_aviso_suspension.motivo,
         v_descripcion   CHAR(40)--LIKE sep_aviso_suspension.descripcion
       END RECORD,
       v_indice        INTEGER


   WHENEVER ERROR CONTINUE
   LET v_indice = 1
   
   {LET v_consulta = "\n SELECT con.id_expediente,",
                    "\n        con.nss,",
                    "\n        est.descripcion,",
                    "\n        SUM(con.monto)",
                    "\n   FROM sep_expediente exp JOIN sep_batch_contabilidad con",
                    "\n     ON con.id_expediente = exp.id_expediente",
                    "\n        LEFT OUTER JOIN sep_estado_expediente est",
                    "\n     ON est.estado = exp.estado",
                    "\n  WHERE exp.estado = 50",
                    "\n    AND con.ind_envio = 0",
                    "\n  GROUP BY con.id_expediente,con.nss,est.descripcion"}
   LET v_consulta = "\n SELECT sus.id_expediente,",
                    "\n        sus.delegacion,",
                    "\n        sus.nombre,",
                    "\n        sus.rfc,",
                    "\n        sus.num_credito,",
                    "\n        sus.nss,",
                    "\n        sus.nrp,",
                    "\n        sus.empresa,",
                    "\n        sus.motivo,",
                    "\n        sus.descripcion",
                    "\n   FROM sep_aviso_suspension sus JOIN sep_expediente exp",
                    "\n     ON sus.id_expediente = exp.id_expediente",
                    "\n  WHERE exp.estado IN (40,45,46,50)",
                    "\n    AND exp.ind_aviso_suspension = 2"
   PREPARE prp_rec_avisos_suspension FROM v_consulta
   DECLARE cur_rec_avisos_suspension CURSOR FOR prp_rec_avisos_suspension
   FOREACH cur_rec_avisos_suspension INTO v_avisos_suspension_aux.*
      
      LET v_avisos_suspension[v_indice].v_conteo        = v_indice
      LET v_avisos_suspension[v_indice].v_id_expediente = v_avisos_suspension_aux.v_id_expediente
      LET v_avisos_suspension[v_indice].v_delegacion    = v_avisos_suspension_aux.v_delegacion
      LET v_avisos_suspension[v_indice].v_nombre        = v_avisos_suspension_aux.v_nombre
      LET v_avisos_suspension[v_indice].v_rfc           = v_avisos_suspension_aux.v_rfc
      LET v_avisos_suspension[v_indice].v_num_credito   = v_avisos_suspension_aux.v_num_credito
      LET v_avisos_suspension[v_indice].v_nss           = v_avisos_suspension_aux.v_nss
      LET v_avisos_suspension[v_indice].v_nrp           = v_avisos_suspension_aux.v_nrp
      LET v_avisos_suspension[v_indice].v_empresa       = v_avisos_suspension_aux.v_empresa
      LET v_avisos_suspension[v_indice].v_motivo        = v_avisos_suspension_aux.v_motivo
      LET v_avisos_suspension[v_indice].v_descripcion   = v_avisos_suspension_aux.v_descripcion
      DISPLAY "v_indice:",v_indice
      DISPLAY "v_avisos_suspension_aux.v_descripcion:",v_avisos_suspension_aux.v_descripcion
      DISPLAY "v_avisos_suspension[v_indice].v_descripcion:",v_avisos_suspension[v_indice].v_descripcion

      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_avisos_suspension

   IF(v_avisos_suspension.getLength() > 0)THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF
END FUNCTION