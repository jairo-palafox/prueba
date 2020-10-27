--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05-07-2013
--==============================================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL31                                                        #
#Objetivo     => Preliquidacion de mandatos                                    #
#Autor        => Hugo Ramírez                                                  #
#Fecha inicio => 05 Junio 2013                                                 #
################################################################################
DATABASE safre_viv
GLOBALS "MDTG02.4gl"
DEFINE p_usuario         LIKE seg_usuario.usuario, # usuario firmado al sistema
       p_tipo_carga      SMALLINT,                 # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo_vtna     STRING,                   # Titulo ventana
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ventana         ui.Window,
       --p_proceso_cod     LIKE cat_proceso.proceso_cod,
       p_opera_cod_preliquidacion LIKE cat_operacion.opera_cod,
       r_pid             LIKE bat_ctr_proceso.pid
       
MAIN

   LET p_usuario     = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo_vtna = ARG_VAL(3)
   

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "mdt" 

   SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
   WHERE modulo_cod = "bat"

   CALL fn_preliquida_mandatos()

END MAIN

{===============================================================================
Nombre: fn_preliquida_mandatos
Fecha creacion: 05 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para preliquidar madnatos según catálogo de ejecución
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_preliquida_mandatos()
DEFINE v_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--STRING,
          v_padre           STRING,
          v_identificador   SMALLINT,--STRING,
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--STRING,
          v_recurrencia     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.recurrencia,
          v_complemento_recurrencia SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.complemento_recurrencia,
          v_ejecuta_cod     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.ejecuta_cod,
          v_descripcion     VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
          v_ent_federeativa VARCHAR(40)--STRING
       END RECORD,
       r_continuar BOOLEAN,
       v_indice    SMALLINT,
       r_resultado_opera SMALLINT,
       v_comando         STRING,
       r_confirma         BOOLEAN

   OPEN WINDOW vtna_preliquida_mandatos WITH FORM v_ruta_ejecutable CLIPPED||"/MDTL311"
      IF(p_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_vtna)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo_vtna)
      END IF
      DISPLAY ARRAY v_mandatos TO sr_mandatos.* ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY
            LET p_opera_cod_preliquidacion = g_opera_cod_carga
            CALL fn_busca_mandatos_preliq() RETURNING r_continuar, v_mandatos
            IF NOT( r_continuar )THEN
               EXIT DISPLAY
            END IF
            
         ON ACTION preliquidar
            
            #Se verifica si se puede iniciar la operacion      
            CALL fn_valida_operacion(0,g_proceso_cod_pago_mandatos,p_opera_cod_preliquidacion) RETURNING r_resultado_opera
            IF(r_resultado_opera = 0)THEN
               CALL fn_ventana_confirma(p_titulo_vtna,"¿Preliquidar mandatos?","question") RETURNING r_confirma
               IF NOT( r_confirma )THEN
                  CONTINUE DISPLAY
               END IF            
               # se genera el pid para el proceso
               CALL fn_genera_pid(g_proceso_cod_pago_mandatos,p_opera_cod_preliquidacion,p_usuario)RETURNING r_pid
               CALL fn_inicializa_proceso(r_pid,
                                          g_proceso_cod_pago_mandatos,
                                          p_opera_cod_preliquidacion,
                                          0,
                                          "MDTL31",
                                          "NA",
                                          p_usuario) RETURNING r_resultado_opera
               IF ( r_resultado_opera <> 0 ) THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE DISPLAY
               END IF
               CALL fn_actualiza_opera_ini(r_pid,
                                           g_proceso_cod_pago_mandatos,
                                           p_opera_cod_preliquidacion,
                                           0,
                                           "MDTL31",
                                           "NA",
                                           p_usuario) RETURNING r_resultado_opera 
            
               # En el caso de que exista una inconsistencia al iniciar el proceso, se
               # Muestra un mensaje con la descripcion
               IF(r_resultado_opera <> 0)THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE DISPLAY
               END IF
               {LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/MDTP31.42r ",p_usuario," ",
                                                                                  r_pid," ",
                                                                                  g_proceso_cod_pago_mandatos," ",
                                                                                  p_opera_cod_preliquidacion," ",
                                                                                  0," ", # folio
                                                                                  "NA" # archivo}
               LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/MDTP31.42r '",
                                    p_usuario CLIPPED, "' ",
                                    r_pid CLIPPED, " ",
                                    g_proceso_cod_pago_mandatos CLIPPED," ",
                                    p_opera_cod_preliquidacion CLIPPED," ",
                                    "0 ",
                                    "NA",
                     " 1>", v_ruta_listados CLIPPED,"/nohup:",r_pid USING "&&&&&",":",
                                                              g_proceso_cod_pago_mandatos USING "&&&&&",":",
                                                              p_opera_cod_preliquidacion USING "&&&&&"," 2>&1 &"
               DISPLAY v_comando 
               RUN v_comando
               IF(STATUS)THEN
                  CALL fn_mensaje(p_titulo_vtna,"Ocurrio un error al ejecutar la preliquidación","about")
               ELSE
                  CALL fn_mensaje(p_titulo_vtna,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                  EXIT DISPLAY
               END IF
            ELSE
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               CONTINUE DISPLAY
            END IF

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY
   CLOSE WINDOW vtna_preliquida_mandatos

END FUNCTION

{===============================================================================
Nombre: fn_busca_mandatos_preliq
Fecha creacion: 05 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para 
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_busca_mandatos_preliq()
DEFINE v_continuar BOOLEAN,
       r_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--STRING,
          v_padre           STRING,
          v_identificador   SMALLINT,--STRING,
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--STRING,
          v_recurrencia     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.recurrencia,
          v_complemento_recurrencia SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.complemento_recurrencia,
          v_ejecuta_cod      SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.ejecuta_cod,
          v_descripcion      VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
          v_ent_federeativa  VARCHAR(40)--STRING
       END RECORD


   MENU ""
      BEFORE MENU
         CALL r_mandatos.clear()
         LET v_continuar = FALSE 

      ON ACTION buscar
         --CALL fn_recupera_mandatos() RETURNING v_continuar, r_mandatos
         CALL fn_valida_ejecucion_mandato(g_estado_abonado_pago_mdt) RETURNING v_continuar, r_mandatos
         IF (v_continuar)THEN
            LET v_continuar = TRUE
            EXIT MENU
         ELSE
            CALL fn_mensaje("Aviso","No se encontraror registros con criterio dado","information")
            CONTINUE MENU
         END IF

      ON ACTION cancelar
         LET v_continuar = FALSE
         EXIT MENU

   END MENU

   RETURN v_continuar,r_mandatos
END FUNCTION
