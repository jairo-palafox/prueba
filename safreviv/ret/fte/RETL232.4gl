--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL232                                                                #
#Objetivo     => Programa que ejecuta el programa que realiza la finalizacion del retiro#
#                de aportaciones voluntarias realizando comunicacion con los WS de SAP  #
#                FICO para indicar Acreedor, cuenta por pagar y pagos                   #
#Fecha inicio => Agosto 20, 2013                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario      LIKE seg_usuario.usuario_cod, -- clave del usuario
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
       
END GLOBALS

MAIN
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion     SMALLINT, -- forma como ejecutara el programa
       p_s_titulo           STRING, -- titulo de la ventana
       v_folio              LIKE glo_folio.folio,
       v_s_cadena           STRING, -- cadena de texto
       v_cbx_folios         ui.ComboBox, -- combo de afores
       v_i_conArch          INTEGER,
       v_proceso_desc       STRING, -- descripcion del proceso
       v_opera_desc         STRING -- descripcion de la operacion
       

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   LET g_usuario = p_usuario_cod
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_ret_aport_voluntarias -- retiro aportaciones voluntarias
   LET g_opera_cod   = g_opera_cod_ret_av_notifica_sap_fico -- notificacion SAP-FICO de los retiros realizados

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "RETL2321"

   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   -- se obtienen las descripciones del proceso y la operacion
   CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod) RETURNING v_opera_desc

   -- se despliegan las descripciones
   DISPLAY v_proceso_desc, v_opera_desc
   TO proceso_desc, opera_desc
   
   -- se captura el folio
   INPUT   v_folio
   WITHOUT DEFAULTS
   FROM    cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT g.folio
         FROM   glo_folio g
         WHERE g.proceso_cod = g_proceso_cod
         AND   g.status      = 2 -- liquidados
         ORDER BY g.folio DESC

         LET v_i_conArch = 0
         
         FOREACH cur_folios INTO v_folio
            LET v_s_cadena = v_folio USING "##########"
            CALL v_cbx_folios.addItem(v_folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         
         -- si no se encontraron folios
         IF ( v_i_conArch < 1 ) THEN
            CALL fn_mensaje("Atención","No existen folios recientemente liquidados","stop")
            EXIT INPUT
         END IF
         -- se inicia en nulo
         LET v_folio = NULL
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
        
         -- se invoca la ejecucion de comunicacion con SAP-FICO
         CALL fn_ret_av_notifica_sap_fico(v_folio, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
   
END MAIN


{
======================================================================
Clave: 
Nombre: fn_ret_av_notifica_sap_fico
Fecha creacion: Agosto 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta el programa que realizara las notificaciones correspondientes
a SAP-FICO de los retiros realizados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_av_notifica_sap_fico(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_i_resultado     INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT --codigo de error  fn_actualiza_opera_ini

   -- este proceso no usa archivo
   LET v_nombre_archivo = "NA" 
   
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado
   
   IF ( v_i_resultado = 0 ) THEN
   	  -- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,
                                  g_proceso_cod,
                                  g_opera_cod,
                                  p_folio,
                                  "RETL232",
                                  v_nombre_archivo,
                                  p_usuario_cod
                                  )
      RETURNING r_resultado_opera
   	  IF (r_resultado_opera  = 0) THEN
   	  	
        LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP232 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                          "/nohup:",g_pid USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"
        DISPLAY v_s_comando                        
        RUN v_s_comando
        CALL fn_mensaje("Atención",
                        "Se ha enviado el programa de notificación a SAP-FICO.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
                        "information")
      ELSE 
         CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF  
      
   ELSE
      CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
   
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   
     MENU
        COMMAND "Cerrar"
           EXIT MENU
     END MENU
      
   END IF
       
 
END FUNCTION