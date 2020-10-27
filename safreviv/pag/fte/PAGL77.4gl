--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/06/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGL77                                                        #
#Objetivo     => Lanzador del reverso de la liquidacion de registro de pagos   #
#                de garantía de estados y municipios                           #
#Fecha inicio => 06 Junio de 2013                                              #
################################################################################
DATABASE safre_viv

GLOBALS "PAGG01.4gl"

DEFINE g_pid          LIKE bat_ctr_operacion.pid,   #  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, # codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, # codigo de operacion
       v_rutas_pag   RECORD
        v_ruta_ejecutable  LIKE seg_modulo.ruta_bin,
        v_ruta_rescate     LIKE seg_modulo.ruta_rescate,
        v_ruta_listados    LIKE seg_modulo.ruta_listados
       END RECORD,
       v_ruta_listados_bat LIKE seg_modulo.ruta_listados
       


MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     # forma como ejecutara el programa
       p_titulo_vtna    STRING,                       # titulo de la ventana
       v_folio          LIKE deo_preliquida.folio_liquida,
       v_cadena_aux     STRING,                       # cadena de texto
       v_cbx_folios     ui.ComboBox,                  # combo de afores
       v_indice         INTEGER,
       v_registro_folios RECORD
         nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo,
        folio            CHAR(10)
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
   LET g_proceso_cod = g_proceso_cod_pag_registro_pagos_gem # Aportaciones voluntarias
   LET g_opera_cod   = g_opera_cod_pag_liquidacion # liquidacion

   # se obtienen las rutas de control del modulo
   SELECT ruta_bin, 
          ruta_rescate, 
          ruta_listados
     INTO v_rutas_pag.*
     FROM seg_modulo
    WHERE modulo_cod = 'pag'

    SELECT ruta_listados
      INTO v_ruta_listados_bat
      FROM seg_modulo
     WHERE modulo_cod = 'bat'

    CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc

    CALL fn_opera_cod_desc(g_proceso_cod, 
                           g_opera_cod) RETURNING v_opera_desc
     
   # se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM v_rutas_pag.v_ruta_ejecutable CLIPPED||"/PAGL771"
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
                                             AND g.status = 2
                                           ORDER BY g.folio DESC

            LET v_indice = 0
            FOREACH cur_folios INTO v_registro_folios.*
               LET v_cadena_aux = v_registro_folios.folio USING "##########", " -",v_registro_folios.nombre_archivo 
               CALL v_cbx_folios.addItem(v_registro_folios.folio USING "##########", v_cadena_aux)
               # Contador de archivos eoncontrados
               LET v_indice = v_indice + 1
            END FOREACH
         
            IF( v_indice < 1 )THEN
               CALL fn_mensaje("Atención","No existen archivos recientemente liquidados","info")
               EXIT INPUT
            END IF
         
         ON ACTION ACCEPT
            IF( v_folio IS NULL OR v_folio = -1)THEN
               CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
               CONTINUE INPUT
            END IF
      
            # se invoca la ejecucion del stored procedure
            CALL fn_reverso_liquidacion_voluntarias(v_folio, p_usuario_cod)
            EXIT INPUT
        
         ON ACTION CANCEL
            EXIT INPUT
   
      END INPUT
   
   CLOSE WINDOW w_folio_preliquida
   
END MAIN

{===============================================================================
Clave: 
Nombre: fn_reverso_liquidacion_voluntarias
Fecha creacion: 06 Junio del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
Ejecuta el reverso de la liquidacion de garantía de estados y municipios

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================
}
FUNCTION fn_reverso_liquidacion_voluntarias(p_folio, p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,        # usuario que ejecuta el programa
       p_folio          LIKE glo_folio.folio,                # folio para preliquidar
       v_comando        STRING,                              # cadena con una instruccion de consola
       v_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo
       v_resultado      SMALLINT,
       v_mensaje        STRING 

   # se verifica si se puede lanzar el reverso de la liquidacion de acuerdo con la poliza
   # contable
   CALL fn_valida_poliza_cnt(p_folio, g_proceso_cod) RETURNING v_resultado
   
   # la poliza no se ha generado y se puede enviar el reverso
   IF( v_resultado = 0 )THEN

      # se obtiene el nombre del archivo  
      SELECT nombre_archivo 
        INTO v_nombre_archivo
        FROM glo_ctr_archivo
       WHERE folio = p_folio
   
      # se obtiene el pid en base al folio seleccionado
      SELECT pid 
        INTO g_pid 
        FROM bat_ctr_proceso
       WHERE proceso_cod =  g_proceso_cod
         AND folio       =  p_folio
     
      CALL fn_valida_reverso(g_pid,
                             g_proceso_cod,
                             g_opera_cod) RETURNING v_resultado

      # se verifica si se puede continuar con la operacion
      IF( v_resultado = 0 )THEN
         # se construye la cadena de ejecucion del programa lanzado de reverso
         LET v_comando = " nohup time fglrun ",v_rutas_pag.v_ruta_ejecutable CLIPPED,
                                             "/PAGR77 ",p_usuario_cod CLIPPED, " ",
                                                        g_pid  , " " ,
                                                        g_proceso_cod , " " ,
                                                        g_opera_cod ," ",
                                                        p_folio ," '",
                                                        v_nombre_archivo CLIPPED,"' ",
                         " 1>",v_ruta_listados_bat CLIPPED,"/nohup:",g_pid USING "&&&&&",":",
                                                                     g_proceso_cod USING "&&&&&",":",
                                                                     g_opera_cod   USING "&&&&&" ,
                         " 2>&1 &"
                  
         RUN v_comando
         CALL fn_mensaje("Atención",
                         "Se ha enviado el reverso de la Liquidación.\n"||"Puede revisar el avance del proceso en el monitor de ejecución de procesos",
                         "information")
      ELSE
         CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF

   ELSE
      CALL fn_mensaje("Atención","No se puede realizar el reverso. La póliza contable ya ha sido generada.","stop")
   END IF     
 
END FUNCTION