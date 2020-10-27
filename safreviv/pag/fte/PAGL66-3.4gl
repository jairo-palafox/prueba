--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11/04/2012
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGL66                                                        #
#Objetivo     => Programa lanzador del reverso de liqudiación de registro de   #
#                pagos de aportacion voluntaria                                #
#Fecha inicio => 11 Abril de 2013                                              #
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
       p_s_titulo       STRING,                       # titulo de la ventana
       v_folio          LIKE deo_preliquida.folio_liquida,
       v_cadena_aux     STRING,                       # cadena de texto
       v_cbx_folios     ui.ComboBox,                  # combo de folios
       v_indice         INTEGER,
       v_r_glo_ctr_archivo RECORD
          nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo,
          folio            CHAR(10)
       END RECORD,
       v_proceso_desc   LIKE cat_proceso.proceso_desc,
       v_opera_desc     LIKE cat_operacion.opera_desc

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   # si se obtuvo el titulo, se pone como titulo de programa
   IF( p_s_titulo IS NOT NULL )THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   # se asigna proceso y operación
   LET g_proceso_cod = 1412 # Aportaciones voluntarias
   LET g_opera_cod   = g_opera_cod_pag_preliquidacion      # preliquidacion

   # se obtienen las rutas de control del módulo
   SELECT ruta_bin, 
          ruta_rescate, 
          ruta_listados
     INTO v_rutas_pag.v_ruta_ejecutable,
          v_rutas_pag.v_ruta_rescate,
          v_rutas_pag.v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'pag'

   SELECT b.ruta_listados
     INTO v_ruta_listados_bat
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'
  
   CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc

   CALL fn_opera_cod_desc(g_proceso_cod, 
                          g_opera_cod) RETURNING v_opera_desc
     
   # se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW vtna_revesa_preliquidacion WITH FORM v_rutas_pag.v_ruta_ejecutable CLIPPED||"/PAGL661"
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
                                             AND g.status = 1
                                             AND a.estado = 2                                             
                                           ORDER BY g.folio DESC

            LET v_indice = 0
            FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
               LET v_cadena_aux = v_r_glo_ctr_archivo.folio USING "##########", " -",v_r_glo_ctr_archivo.nombre_archivo 
               CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio USING "##########", v_cadena_aux)
               # Contador de archivos eoncontrados
               LET v_indice = v_indice + 1
            END FOREACH
         
            IF( v_indice < 1 ) THEN
               CALL fn_mensaje("Atención","No existen archivos recientemente preliquidados","info")
               EXIT INPUT
            END IF
         
         ON ACTION ACCEPT
            IF( v_folio IS NULL OR v_folio = -1 )THEN
               CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
               CONTINUE INPUT
            END IF
      
            # se invoca la ejecucion del stored procedure
            CALL fn_reverso_preliquidacion_volutarias(v_folio, p_usuario_cod)
            EXIT INPUT
        
         ON ACTION CANCEL
            EXIT INPUT
   
      END INPUT
   
   CLOSE WINDOW vtna_revesa_preliquidacion
   
END MAIN

{
================================================================================
Clave: 
Nombre: fn_reverso_preliquidacion_volutarias
Fecha creacion: 16 Abril del 2013
Autor: Hugo Ramírez 
Narrativa del proceso que realiza:
 Ejecuta el reverso de la preliquidacion de aportaciones voluntarias tarias

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================
}
FUNCTION fn_reverso_preliquidacion_volutarias(p_folio, p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # usuario que ejecuta el programa
       p_folio          LIKE glo_folio.folio,         # folio para preliquidar
       v_comando        STRING,                       # cadena con una instruccion de consola
       v_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo
       v_cod_salida     SMALLINT,                     # codigo de salida de la validacion de reverso
       v_mensaje        STRING

   # se obtiene el nombre del archivo
   SELECT a.nombre_archivo
     INTO v_nombre_archivo
     FROM glo_ctr_archivo a
    WHERE a.folio = p_folio

   # se obtiene el PID
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
      AND folio = p_folio
   
   # se verifica que se pueda ejecutar el reverso
   LET v_cod_salida = fn_valida_reverso(g_pid,
                                        g_proceso_cod,
                                        g_opera_cod)
   
   # se verifica si se puede continuar con la operacion
   IF( v_cod_salida = 0 ) THEN
      LET v_comando = " nohup time fglrun ",v_rutas_pag.v_ruta_ejecutable CLIPPED,
                                          "/PAGR66 ",p_usuario_cod CLIPPED, " ",
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

      IF(STATUS)THEN
         CALL fn_mensaje("Atención",
                         "No se ha enviado la operación",
                         "information")
      ELSE
         CALL fn_mensaje("Atención",
                         "Se ha enviado el reverso de la Preliquidación.\n"||"Puede revisar el avance del proceso en el monitor de ejecución de procesos",
                         "information")
      END IF

                      
   ELSE
      # se obtiene la descripcion del motivo
      LET v_mensaje = fn_recupera_inconsis_opera(v_cod_salida)
      
      # se indica en pantalla el motivo por el cual no se puede ejecutar el reverso
      CALL fn_mensaje("Atención",v_mensaje,"stop")
   END IF
 
END FUNCTION