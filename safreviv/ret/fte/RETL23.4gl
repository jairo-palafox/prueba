--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL23                                                                 #
#Objetivo     => Lanzador del reverso de la carga de archivo de retiros por disposicion #
#                de recursos                                                            #
#Fecha inicio => Marzo 01, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid           LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod   LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod     LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo    RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,     
       seg_modulo_bat  RECORD
        ruta_listados    CHAR(40)
       END RECORD

END GLOBALS

MAIN
DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion      SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo            STRING -- titulo de la ventana
       ,v_s_cadena            STRING -- cadena de texto
       ,v_cbx_archivos        ui.ComboBox -- combo de afores
       ,v_i_conArch           INTEGER
       ,v_r_glo_ctr_archivo   RECORD
          nombre_archivo        LIKE glo_ctr_archivo.nombre_archivo
       END RECORD
       ,v_proceso_desc        LIKE cat_proceso.proceso_desc
       ,v_opera_desc          LIKE cat_operacion.opera_desc
       ,v_nombre_archivo      LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       

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
   LET g_proceso_cod = g_proceso_cod_ret_disposicion -- retiros por disposicion de recursos
   LET g_opera_cod   = g_opera_cod_ret_disp_carga -- carga de archivo

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
   OPEN WINDOW w_folio_preliquida WITH FORM "RETL231"
   -- Muestra detalle de operación
   DISPLAY BY NAME v_proceso_desc, v_opera_desc
   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_archivos = ui.ComboBox.forName("formonly.cmb_archivo")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_archivos.clear()
   
   -- se captura el folio
   INPUT
    v_nombre_archivo
   WITHOUT DEFAULTS
   FROM
    cmb_archivo
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_nombre_archivo   = NULL
         
         -- se llena el arreglo de folios
         DECLARE cur_archivos CURSOR FOR
         SELECT a.nombre_archivo
           FROM glo_ctr_archivo a
          WHERE a.proceso_cod = g_proceso_cod
            AND a.estado = 1 -- cargado
            AND (a.folio IS NULL OR a.folio = 0)

         --SELECT b.pid||'-'||a.nombre_archivo AS nombre_archivo
           --FROM glo_ctr_archivo a,bat_ctr_operacion b
          --WHERE a.proceso_cod = g_proceso_cod
            --AND a.estado = 1 -- cargado
            --AND (a.folio IS NULL OR a.folio = 0)
            --AND b.proceso_cod = g_proceso_cod
            --AND b.nom_archivo = a.nombre_archivo
            --AND b.opera_cod = g_opera_cod_ret_disp_integracion
            --AND b.estado_cod = 1 -- que este listo para ejecutarse

  
            LET v_i_conArch = 0
         
         FOREACH cur_archivos INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_archivos.addItem(v_r_glo_ctr_archivo.nombre_archivo,
                                        v_s_cadena CLIPPED)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         
         IF ( v_i_conArch < 1 ) THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente Cargados","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_nombre_archivo IS NULL OR v_nombre_archivo IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
      
         -- se invoca la ejecucion del stored procedure
         DISPLAY "v_nombre_archivo",v_nombre_archivo,"-"
         CALL fn_ret_disposicion_reverso_carga_archivo(v_nombre_archivo, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

{
======================================================================
Clave: 
Nombre: fn_ret_disposicion_reverso_carga_archivo
Fecha creacion: Marzo 01, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta el reverso de la carga de archivo de retiros por disposicion
de recursos


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_disposicion_reverso_carga_archivo(v_nombre_archivo, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_resultado       SMALLINT,
       v_mensaje         STRING
       
   LET p_folio = 0

   -- se obtiene el folio de la operacion posterior
   SELECT folio
   INTO   p_folio
   FROM   bat_ctr_operacion
   WHERE  proceso_cod = g_proceso_cod
      AND nom_archivo = v_nombre_archivo
      AND opera_cod = g_opera_cod_ret_disp_integracion
      AND estado_cod = 1 -- que este listo para ejecutarse
   
   -- se obtiene el PID del proceso
   CALL fn_recupera_pid(g_proceso_cod,p_folio,1) RETURNING g_pid

   LET v_resultado = fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod) 
        
   -- se verifica si se puede continuar con la operacion
   IF ( v_resultado = 0 ) THEN
      -- se invoca la ejecucion del programa lanzado. los parametros se envian 
      -- en orden segun lo acordado el dia 12/Enero/2012 con equipo EFP
      -- usuario, pid, proceso_cod, opera_cod, folio y archivo
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETR23 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ,' "',
                          v_nombre_archivo CLIPPED,'" ',
                          " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"

       DISPLAY v_s_comando
                          
       RUN v_s_comando
       CALL fn_mensaje("Atención",
            "Se ha enviado el reverso de la Carga de Archivo.\n"||
            "Puede revisar el avance del proceso en el monitor de ejecución "||
            "de procesos","information")
   ELSE
      -- no se puede enviar el reverso por validacion del mismo
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   END IF

END FUNCTION