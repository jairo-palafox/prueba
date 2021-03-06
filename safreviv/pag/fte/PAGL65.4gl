--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16/04/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGL67                                                        #
#Objetivo     => Lanzador del reverso de carga e integracion del archivo de    #
#                registro de pagos de aportacion voluntaria                    #
#Fecha inicio => 16 Abril de 2013                                              #
################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"

DEFINE g_pid         LIKE bat_ctr_proceso.pid,     #  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, # codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod, # codigo de operacion
       v_rutas_pag  RECORD
        v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
        v_ruta_rescate    LIKE seg_modulo.ruta_rescate,
        v_ruta_listados   LIKE seg_modulo.ruta_listados
       END RECORD,
       v_ruta_listados_bat LIKE seg_modulo.ruta_listados


MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     # forma como ejecutara el programa
       p_titulo_vtna    STRING,                       # titulo de la ventana
       v_folio          LIKE glo_folio.folio,
       v_cadena_aux     STRING,                       # cadena de texto
       v_cbx_folios     ui.ComboBox,                  # combo de afores
       v_indice         INTEGER,
       v_r_glo_ctr_archivo RECORD
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
          folio          CHAR(10)
       END RECORD,
       v_proceso_desc   LIKE cat_proceso.proceso_desc,
       v_opera_desc     LIKE cat_operacion.opera_desc

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   # si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo_vtna IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo_vtna)
   END IF
   
   # se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_pag_registro_pagos_av # Aportaciones voluntarias
   LET g_opera_cod   = g_opera_cod_pag_integracion  # integración

   # se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO v_rutas_pag.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'pag'

   SELECT b.ruta_listados
     INTO v_ruta_listados_bat
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'
  
   CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc

   CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod) RETURNING v_opera_desc
     
   # se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM v_rutas_pag.v_ruta_ejecutable CLIPPED||"/PAGL651"
      # Muestra detalle de operación
      DISPLAY BY NAME v_proceso_desc, v_opera_desc
      # se le asigna el apuntado del combo a la variable
      LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
      # se inicia el combobox en blanco
      CALL v_cbx_folios.clear()
   
      # se captura el folio
      INPUT v_folio WITHOUT DEFAULTS FROM cmb_folio ATTRIBUTES (UNBUFFERED)

         BEFORE INPUT
            # se asignan los valores por omision
            LET v_folio = NULL
         
            # se llena el arreglo de folios
            DECLARE cur_folios CURSOR FOR SELECT a.nombre_archivo, 
                                                 g.folio
                                            FROM glo_folio g LEFT OUTER JOIN glo_ctr_archivo a
                                              ON a.folio = g.folio
                                           WHERE g.proceso_cod = g_proceso_cod
                                             AND g.status = 0 -- integrado
                                             AND g.folio IS NOT NULL
                                           ORDER BY g.folio DESC 

            LET v_indice = 0
            FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
               LET v_cadena_aux = v_r_glo_ctr_archivo.folio USING "##########", " -",v_r_glo_ctr_archivo.nombre_archivo 
               CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio USING "##########", v_cadena_aux)
               # Contador de archivos eoncontrados
               LET v_indice = v_indice + 1
            END FOREACH
         
            IF( v_indice < 1 )THEN
               CALL fn_mensaje("Atención","No existen archivos recientemente Integrados","info")
               EXIT INPUT
            END IF
         
         ON ACTION ACCEPT
            IF( v_folio IS NULL OR v_folio = -1)THEN
               CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
               CONTINUE INPUT
            END IF
      
            # se invoca la ejecucion del stored procedure
            CALL fn_reverso_integracion_aportaciones(v_folio, p_usuario_cod)
            EXIT INPUT
        
         ON ACTION CANCEL
            EXIT INPUT
   
      END INPUT   
   CLOSE WINDOW w_folio_preliquida
   
END MAIN

{
================================================================================
Clave: 
Nombre: fn_reverso_integracion_aportaciones
Fecha creacion: 16 Abril 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
Ejecuta el reverso de la integracion de aportaciones voluntarias
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================
}
FUNCTION fn_reverso_integracion_aportaciones(p_folio, p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # usuario que ejecuta el programa
       p_folio          LIKE glo_folio.folio,         # folio para preliquidar
       v_comando        STRING,                       # cadena con una instruccion de consola
       v_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo
       v_resultado      SMALLINT,
       v_mensaje        STRING 

   # se obtiene el nombre del archivo
   SELECT nombre_archivo
     INTO v_nombre_archivo
     FROM glo_ctr_archivo
    WHERE folio = p_folio

   # se obtiene el pid en base al folio seleccionado
   SELECT MAX(pid) 
     INTO g_pid 
     FROM bat_ctr_proceso
    WHERE proceso_cod =  g_proceso_cod
      AND folio       =  p_folio
   
   # se verifica si se puede continuar con la operacion
   LET v_resultado = fn_valida_reverso(g_pid, 
                                       g_proceso_cod,
                                       g_opera_cod)
   
   IF( v_resultado = 0 )THEN
      LET v_comando = " nohup time fglrun ",v_rutas_pag.v_ruta_ejecutable CLIPPED,
                                          "/PAGR65 ",p_usuario_cod CLIPPED, " ",
                                                     g_pid  , " " ,
                                                     g_proceso_cod , " " ,
                                                     g_opera_cod ," ",
                                                     p_folio ," ",
                                                     v_nombre_archivo CLIPPED," ",
                      " 1>",v_ruta_listados_bat CLIPPED,"/nohup:",g_pid USING "&&&&&",":",
                                                                  g_proceso_cod USING "&&&&&",":",
                                                                  g_opera_cod   USING "&&&&&" ,
                      " 2>&1 &"
   
      RUN v_comando
      CALL fn_mensaje("Atención",
                      "Se ha enviado el reverso de la Integración.\n"||"Puede revisar el avance del proceso en el monitor de ejecución de procesos",
                      "information")
   ELSE
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   END IF
 
END FUNCTION