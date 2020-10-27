--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 24-07-2013
--==============================================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL30                                                        #
#Objetivo     => Reporte abonos mandatos                                       #
#Autor        => Hugo Ramírez                                                  #
#Fecha inicio => 24 Junio 2013                                                 #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"
DEFINE p_usuario         LIKE seg_usuario.usuario, # usuario firmado al sistema
       p_tipo_carga      SMALLINT,                 # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo_vtna     STRING,                   # Titulo ventana
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       --p_proceso_cod     LIKE cat_proceso.proceso_cod,
       v_opera_cod_rpt_abonos LIKE cat_operacion.opera_cod,
       r_pid             LIKE bat_ctr_proceso.pid


MAIN

   LET p_usuario     = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo_vtna = ARG_VAL(3)
   

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "hps" 

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "bat"

   CALL fn_genera_rpt_abonos_mandatos()

END MAIN

{===============================================================================
Nombre: fn_genera_rpt_abonos_mandatos
Fecha creacion: 24 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para generar reporte de abonos mandatos
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_genera_rpt_abonos_mandatos()
DEFINE r_proceso_desc    LIKE cat_proceso.proceso_desc,
       r_opera_desc      LIKE cat_operacion.opera_desc,
       r_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT,
       r_confirma        BOOLEAN,
       v_comando         STRING
       
   LET v_opera_cod_rpt_abonos = g_opera_cod_carga
   OPEN WINDOW vtna_rpt_abonos_mandatos WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL011"

      MENU ""
         BEFORE MENU
            CALL fn_recupera_datos_folio(g_proceso_cod_originacion_deudor,
                                         v_opera_cod_rpt_abonos) RETURNING r_proceso_desc,r_opera_desc,r_folio
            IF(r_folio IS NULL OR r_folio = 0)THEN
               CALL fn_mensaje("Aviso","No hay folios para procesar","information")
               EXIT MENU
            ELSE
               DISPLAY r_proceso_desc TO proceso_desc
               DISPLAY r_opera_desc   TO opera_desc
               DISPLAY r_folio        TO folio
               CONTINUE MENU
            END IF

         ON ACTION aceptar
            #Se verifica si se puede iniciar la operacion      
            CALL fn_valida_operacion(0,
                                     g_proceso_cod_abonos_mandatos,
                                     v_opera_cod_rpt_abonos) RETURNING r_resultado_opera
            IF(r_resultado_opera = 0)THEN
               CALL fn_ventana_confirma(p_titulo_vtna,"¿Generar provision Traspaso Fondo Servicios?","question") RETURNING r_confirma
               IF NOT( r_confirma )THEN
                  CONTINUE MENU
               END IF            
               # se genera el pid para el proceso
               CALL fn_genera_pid(g_proceso_cod_abonos_mandatos,v_opera_cod_rpt_abonos,p_usuario)RETURNING r_pid
               CALL fn_inicializa_proceso(r_pid,
                                          g_proceso_cod_abonos_mandatos,
                                          v_opera_cod_rpt_abonos,
                                          r_folio,
                                          "HPSL01",
                                          "NA",
                                          p_usuario) RETURNING r_resultado_opera
               IF ( r_resultado_opera <> 0 ) THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE MENU
               END IF
               CALL fn_actualiza_opera_ini(r_pid,
                                           g_proceso_cod_abonos_mandatos,
                                           v_opera_cod_rpt_abonos,
                                           r_folio,
                                           "HPSL01",
                                           "NA",
                                           p_usuario) RETURNING r_resultado_opera 
            
               # En el caso de que exista una inconsistencia al iniciar el proceso, se
               # Muestra un mensaje con la descripcion
               IF(r_resultado_opera <> 0)THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE MENU
               END IF
               
               LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/HPSP01.42r '",p_usuario CLIPPED, "' ",
                                                                                         r_pid CLIPPED, " ",
                                                                                         g_proceso_cod_abonos_mandatos CLIPPED," ",
                                                                                         v_opera_cod_rpt_abonos CLIPPED," ",
                                                                                         "0 ",
                                                                                         "NA",
                               " 1>", v_ruta_listados CLIPPED,"/nohup:",r_pid USING "&&&&&",":",
                                                                        g_proceso_cod_abonos_mandatos USING "&&&&&",":",
                                                                        v_opera_cod_rpt_abonos USING "&&&&&"," 2>&1 &"
               
               RUN v_comando
               IF(STATUS)THEN
                  CALL fn_mensaje(p_titulo_vtna,"Ocurrio un error al ejecutar la operación","about")
               ELSE
                  CALL fn_mensaje(p_titulo_vtna,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                  EXIT MENU
               END IF
            ELSE
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               CONTINUE MENU
            END IF
         
         ON ACTION cancelar
            EXIT MENU

      END MENU


   CLOSE WINDOW vtna_rpt_abonos_mandatos

END FUNCTION

{===============================================================================
Nombre: fn_genera_rpt_abonos_mandatos
Fecha creacion: 24 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para generar reporte de abonos mandatos
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_datos_folio(p_proceso_cod, p_opera_cod)
DEFINE p_proceso_cod  LIKE cat_proceso.proceso_cod,
       p_opera_cod    LIKE cat_operacion.opera_cod,
       v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_opera_desc   LIKE cat_operacion.opera_desc,
       v_folio        LIKE glo_folio.folio,
       v_consulta    STRING


   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = p_opera_cod

   INITIALIZE v_folio TO NULL
   LET v_consulta = "\n SELECT MAX(folio)",
                    "\n   FROM glo_folio",
                    "\n  WHERE proceso_cod = ?",
--                    "\n    AND opera_cod = ?",
                    "\n    AND opera_cod = 2",
                    "\n    AND status =  0" # registrado
   PREPARE prp_recupera_folio FROM v_consulta
   EXECUTE prp_recupera_folio USING p_proceso_cod
                                    --p_opera_cod
                               INTO v_folio
   
   RETURN v_proceso_desc,
          v_opera_desc,
          v_folio
END FUNCTION