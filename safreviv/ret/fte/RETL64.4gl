--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL64                                                                 #
#Objetivo     => Lanzador del reverso de la generacion de archivo de salida de Retiros  #
#                tipo N para Tesoreria                                                  #
#Fecha inicio => Marzo 01, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo     RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
        ruta_listados    CHAR(40)
       END RECORD

END GLOBALS

MAIN
DEFINE  p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion     SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo           STRING -- titulo de la ventana
       ,v_folio              LIKE deo_preliquida.folio_liquida
       ,v_s_cadena           STRING -- cadena de texto
       ,v_cbx_folios         ui.ComboBox -- combo de afores
       ,v_i_conArch          INTEGER
       ,v_r_glo_ctr_archivo  RECORD
         nombre_archivo        LIKE glo_ctr_archivo.nombre_archivo
        ,folio                 CHAR(10)
       END RECORD
       ,v_proceso_desc       LIKE cat_proceso.proceso_desc
       ,v_opera_desc         LIKE cat_operacion.opera_desc

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_ret_tipo_N -- retiros tipo N
   LET g_opera_cod   = g_opera_cod_ret_tipoN_salida_tesoreria -- archivo de salida para tesoreria

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'

    CALL fn_proceso_cod_desc(g_proceso_cod)
    RETURNING v_proceso_desc

    CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod)
    RETURNING v_opera_desc
     
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "RETL641"
   -- Muestra detalle de operación
   DISPLAY BY NAME v_proceso_desc, v_opera_desc

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
     
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
         DECLARE cur_folios CURSOR FOR
         SELECT a.nombre_archivo, g.folio
           FROM glo_folio g LEFT OUTER JOIN glo_ctr_archivo a
             ON a.folio = g.folio
          WHERE g.proceso_cod = g_proceso_cod
            AND g.status = 2

         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(
                 v_r_glo_ctr_archivo.folio USING "##########", v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         
         IF ( v_i_conArch < 1 ) THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente liquidados","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL OR v_folio = -1) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
      
         -- se invoca la ejecucion del stored procedure
         CALL fn_ret_tipoN_reverso_archivo_salida(v_folio, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

{
======================================================================
Clave: 
Nombre: fn_ret_tipoN_reverso_archivo_salida
Fecha creacion: Marzo 01, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta el reverso de la creacion de archivo de retiros tipo N 
para Tesoreria para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_tipoN_reverso_archivo_salida(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_resultado       SMALLINT,
       v_mensaje         STRING

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"

   -- se obtiene el PID del proceso
   CALL fn_recupera_pid(g_proceso_cod,p_folio,1) RETURNING g_pid
    
   -- se verifica si se puede lanzar el reverso de la liquidacion de acuerdo con la poliza
   -- contable
   CALL fn_valida_poliza_cnt(p_folio, g_proceso_cod)
   RETURNING v_resultado

   -- la poliza no se ha generado y se puede enviar el reverso
   IF ( v_resultado = 0 ) THEN

      CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING v_resultado
      -- se verifica si se puede continuar con la operacion
      IF ( v_resultado = 0 ) THEN
         -- se construye la cadena de ejecucion del programa lanzado de reverso
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETR64 ",
                           p_usuario_cod CLIPPED, " ",
                           g_pid  , " " ,
                           g_proceso_cod , " " ,
                           g_opera_cod ," ",
                           p_folio ," ",
                           v_nombre_archivo CLIPPED," ",
                           " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                           "/nohup:",g_pid        USING "&&&&&",":",
                           g_proceso_cod USING "&&&&&",":",
                           g_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"
 
         DISPLAY v_s_comando
                         
         RUN v_s_comando
         CALL fn_mensaje("Atención","Se ha enviado el reverso de la generación de archivo para Tesorería.\n"||
              "Puede revisar el avance del proceso en el monitor de ejecución de procesos"
              ,"information")
      ELSE
         -- no se puede enviar el reverso por validacion del mismo
         CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF
   ELSE
      CALL fn_mensaje("Atención","No se puede realizar el reverso. La póliza contable ya ha sido generada.","stop")
   END IF

 
END FUNCTION