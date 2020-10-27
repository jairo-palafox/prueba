--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL51                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                del detalle de retiro tipo N                                           #
#Fecha inicio => Marzo 08, 2012                                                         #
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
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion      SMALLINT, -- forma como ejecutara el programa
       p_s_titulo            STRING, -- titulo de la ventana
       v_folio               LIKE glo_folio.folio
       ,v_s_cadena           STRING -- cadena de texto
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
       v_opera_desc          STRING, -- descripcion de la operacion
       v_total_importe       LIKE ret_mto_notificado.total_importe
       

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
   LET g_proceso_cod = g_proceso_cod_ret_tipo_N -- retiro tipo N
   LET g_opera_cod   = g_opera_cod_ret_tipoN_preliquidacion -- preliquidacion

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
    WHERE s.modulo_cod = 'ret'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "RETL511"

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
         DECLARE cur_folios CURSOR FOR
         SELECT *
         FROM glo_ctr_archivo
         WHERE proceso_cod = g_proceso_cod -- retiros tipo N
           AND estado = 2 -- integrado

         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         IF ( v_i_conArch < 1 ) THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente integrados","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF

         -- se verifica si ya se tiene el monto notificado capturado
         SELECT total_importe
         INTO v_total_importe
         FROM
            ret_mto_notificado
         WHERE
            folio = v_folio

         -- Si no se tiene monto, no se puede hacer la preliquidacion
         IF ( v_total_importe IS NULL ) THEN
            CALL fn_mensaje("Atención","No se ha capturado los montos notificados para el folio dado.\nNo puede lanzar la preliquidación","stop")
            CONTINUE INPUT
         END IF  
         
         -- se invoca la ejecucion del stored procedure
         CALL fn_preliquida_ret_tipoN(v_folio, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
   
END MAIN


{
======================================================================
Clave: 
Nombre: fn_preliquida_ret_tipoN
Fecha creacion: Marzo 08, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de retiros tipo N para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_preliquida_ret_tipoN(p_folio, p_usuario_cod)
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando        STRING, -- cadena con una instruccion de consola
       v_mensaje          STRING,
       v_i_resultado      INTEGER, -- resultado del proceso
       v_nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio            LIKE glo_folio.folio,
       r_resultado_opera  SMALLINT  --codigo de error  fn_actualiza_opera_ini


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
       

       CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado

       IF ( v_i_resultado = 0 ) THEN
       	
       	  -- Inicio operacion.
          CALL fn_actualiza_opera_ini(g_pid,
                                      g_proceso_cod,
                                      g_opera_cod,
                                      p_folio,
                                      "RETL51",
                                      v_nombre_archivo,
                                      p_usuario_cod
                                      )
          RETURNING r_resultado_opera
       	  IF (r_resultado_opera  = 0) THEN
       	
             LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP51 ",
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
                             "Se ha enviado la preliquidación.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
                             "information")
          ELSE 
            CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
            CALL fn_mensaje("Atención", v_mensaje, "stop")
          	
          END IF 

       ELSE
          CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje

          CALL fn_mensaje("Atención", v_mensaje, "stop")

         MENU
            COMMAND "Salir"
               EXIT MENU
         END MENU
           
       END IF
       
 
END FUNCTION