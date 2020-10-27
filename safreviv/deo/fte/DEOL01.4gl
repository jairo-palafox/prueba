--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOL01                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para la devolucion por errores de operacion                            #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

END GLOBALS
GLOBALS "DEOG01.4gl"

MAIN
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo          STRING -- titulo de la ventana
       ,v_folio             LIKE deo_preliquida.folio_liquida
       ,v_s_cadena          STRING -- cadena de texto
       ,v_cbx_folios        ui.ComboBox -- combo de afores
       ,v_i_conArch         INTEGER
       ,v_r_glo_ctr_archivo RECORD
          folio              CHAR(10),
          nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo
       END RECORD
       
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
   LET g_proceso_cod = g_proceso_cod_deo -- devolucion por errores de operacion
   LET g_opera_cod   = g_opera_cod_deo_preliquidacion -- preliquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO g_pid
   FROM
    bat_ctr_proceso
   WHERE
    proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'deo'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
  
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "DEOL010"

   -- se muestra la descripcion de la operacion y el proceso en pantalla
   DISPLAY fn_proceso_cod_desc(g_proceso_cod) TO proceso_desc
   DISPLAY fn_opera_cod_desc(g_proceso_cod, g_opera_cod) TO opera_desc

   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   CALL v_cbx_folios.addItem(-1," ")
   
   -- se captura el folio
   INPUT
    v_folio
   WITHOUT DEFAULTS
   FROM
    cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio                  = -1       
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT g.folio, a.nombre_archivo
           FROM glo_ctr_archivo a, glo_folio g
          WHERE a.proceso_cod = g_proceso_cod_deo
            AND a.proceso_cod = g.proceso_cod
            AND a.folio = g.folio
            AND g.status = 0 -- solo integrados

         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(
                 v_r_glo_ctr_archivo.folio USING "##########", v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente integrados","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
      
         -- se invoca la ejecucion del stored procedure
         CALL fn_deo_ejecuta_preliquidacion(v_folio, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

{
======================================================================
Clave: 
Nombre: fn_deo_ejecuta_preliquidacion
Fecha creacion: Enero 02, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de una devolucion por errores de operacion
para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_deo_ejecuta_preliquidacion(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,v_i_resultado    INTEGER -- resultado del proceso


   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"

   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING v_i_resultado
   IF ( v_i_resultado = 0 ) THEN

      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"DEOP02",
                                 "",p_usuario_cod) 
                              RETURNING v_i_resultado

      IF ( v_i_resultado = 0 ) THEN
         -- se invoca la ejecucion del programa lanzado. los parametros se envian 
         -- en orden segun lo acordado el dia 12/Enero/2012 con equipo EFP
         -- usuario, pid, proceso_cod, opera_cod, folio y archivo
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/DEOP02 ",
                           p_usuario_cod CLIPPED, " ",
                           g_pid  , " " ,
                           g_proceso_cod , " " ,
                           g_opera_cod ," ",
                           p_folio ," ",
                           v_nombre_archivo CLIPPED," ",
                           " 1>",seg_modulo_bat.ruta_listados clipped ,
                           "/nohup:",  g_pid USING "&&&&&",":",
                           g_proceso_cod     USING "&&&&&",":",
                           g_opera_cod       USING "&&&&&" ,
                           " 2>&1 &"
                         
         DISPLAY v_s_comando
                         
         RUN v_s_comando
         CALL fn_mensaje("Atención","Se ha enviado la preliquidación.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
      ELSE
         -- no se pudo iniciar la ejecucion del proceso
         CALL fn_muestra_inc_operacion(v_i_resultado)
      END IF
   ELSE
      DISPLAY "v_i_resultado:",v_i_resultado
      -- no se pudo ejecutar el proceso
      CALL fn_mensaje("Atención",fn_mues_desc_valida(v_i_resultado),"stop")
   END IF
 
END FUNCTION


FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   RETURN v_descripcion CLIPPED

END FUNCTION -- fn_mues_desc_valida

