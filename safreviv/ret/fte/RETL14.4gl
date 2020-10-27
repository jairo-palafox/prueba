--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL14                                                                 #
#Objetivo     => Programa que ejecuta el programa que genera el archivo de salida con   #
#                las solicitudes de fondo ahorro liquidadas                             #
#                para ser enviado a Tesoreria                                           #
#Fecha inicio => Abril 23, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
            
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     --  ID del proceso
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
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion      SMALLINT,   -- forma como ejecutara el programa
       p_s_titulo            STRING,     -- titulo de la ventana
       v_folio               LIKE glo_folio.folio
       ,v_s_cadena           STRING      -- cadena de texto
       ,v_cbx_folios         ui.ComboBox -- combo de afores
       ,v_i_conArch          INTEGER
       ,v_r_glo_ctr_archivo  RECORD
          proceso_cod          LIKE glo_ctr_archivo.proceso_cod
          ,opera_cod           LIKE glo_ctr_archivo.opera_cod
          ,nombre_archivo      LIKE glo_ctr_archivo.nombre_archivo
          ,folio               LIKE glo_folio.folio
          ,estado              LIKE glo_ctr_archivo.estado
          ,f_actualiza         LIKE glo_ctr_archivo.f_actualiza
          ,usuario             LIKE glo_ctr_archivo.usuario
       END RECORD,
       v_proceso_desc        STRING, -- descripcion del proceso
       v_opera_desc          STRING -- descripcion de la operacion

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
   LET g_proceso_cod = g_proceso_cod_ret_fondo_ahorro            -- retiro por fondo ahorro
   LET g_opera_cod   = g_opera_cod_ret_fondoAho_salida_tesoreria -- generacion del archivo de salida para Tesoreria

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
   OPEN WINDOW w_folio_preliquida WITH FORM "RETL141"

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
   INPUT
    v_folio
   WITHOUT DEFAULTS
   FROM
    cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio                  = NULL   
         
         -- se llena el arreglo de folios
         LET v_s_cadena = "\n SELECT gl.folio               ",
                          "\n FROM   glo_folio gl           ",
                          "\n WHERE gl.proceso_cod = ", g_proceso_cod,
                          "\n   AND gl.status  =  2"

         PREPARE ffd FROM v_s_cadena
         DECLARE cur_folios CURSOR FOR ffd
         --DISPLAY v_s_cadena
         LET v_i_conArch = 0
         
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.folio
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########"
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_s_cadena)
             --Contador de archivos encontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         --DISPLAY v_i_conArch
         IF ( v_i_conArch < 1 OR v_r_glo_ctr_archivo.folio IS NULL) THEN
            CALL fn_mensaje("Atenci�n",
                 "No existen archivos recientemente de tesoreria","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atenci�n","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
        
         -- se invoca la ejecucion del stored procedure
         CALL fn_genera_salida_ret_fondo_ahorro(v_folio, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
   
END MAIN


{
======================================================================
Clave: 
Nombre: fn_genera_salida_ret_fondo_ahorro
Fecha creacion: Mayo 01, 2012
Autor: Erick Rodriguez, EFP
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de retiros fondo ahorro 
para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_salida_ret_fondo_ahorro(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_resultado       INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio

   -- se recupera el folio
   SELECT nombre_archivo
   INTO v_nombre_archivo
   FROM
      glo_ctr_archivo
   WHERE
      folio = p_folio
   
   -- si no se encontro el nombre del archivo
   IF ( v_nombre_archivo IS NULL ) THEN
      LET v_nombre_archivo = "NA"
   END IF

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
   AND    folio = p_folio

   -- se valida que se pueda iniciar la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_resultado

   -- si se pudo validar
   IF ( v_resultado = 0 ) THEN
   
      -- se inicia la operacion
      LET v_resultado = fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,p_folio,"RETL14",v_nombre_archivo,p_usuario_cod)
      
      -- si se pudo iniciar la operacion
      IF ( v_resultado = 0 ) THEN
                              
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETS05 ",
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
         
         -- DISPLAY v_s_comando                        
         RUN v_s_comando
         CALL fn_mensaje("Atenci�n", "Se ha enviado la generaci�n del archivo de salida.\nPodr� revisar el resultado en el monitor de ejecuci�n de procesos", "information")
      ELSE
         CALL fn_mensaje("Atenci�n","No se puede iniciar la operaci�n.","stop")
      END IF
   ELSE
      -- se muestra en pantalla por que no se puede enviar el proceso
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atenci�n", v_mensaje, "stop")
      
      MENU
         COMMAND "Cerrar"
           EXIT MENU
      END MENU
      
   END IF

END FUNCTION