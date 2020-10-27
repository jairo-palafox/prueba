--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLL41                                                                 #
#Objetivo     => Programa que inicia el proceso de actualizacion de aclaraciones sin    #
#                causal via un archivo construido por el instituto                      #
#                                                                                       #
#Fecha inicio => Febrero 07, 2014                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "ACLG02.4gl"
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
       v_opera_desc         STRING, -- descripcion de la operacion
       v_conteo             DECIMAL(9,0) -- contador de solicitudes
       

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_acl_act_sin_causal -- actualizacion de operaciones sin causal
   LET g_opera_cod   = g_opera_cod_busqueda -- busqueda de aclaraciones

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'acl'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_consulta WITH FORM "ACLL411"

   -- se obtienen las descripciones del proceso y la operacion
   CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod) RETURNING v_opera_desc

   -- se despliegan las descripciones
   DISPLAY v_proceso_desc, v_opera_desc
   TO      proceso_desc, opera_desc
   
   -- se cuentan cuantas solicitudes se van a buscar
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   safre_tmp:tmp_aclaraciones_sin_causal
   
   -- se despliega el conteo
   DISPLAY BY NAME v_conteo
   CALL ui.interface.refresh()

   -- se prepara el proceso para se iniciado o cancelado
   MENU
      COMMAND "Aceptar"
         -- si no aclaraciones, no se envia el proceso
         IF ( v_conteo < 1 ) THEN
		    CALL fn_mensaje("Atenci�n","No se tienen aclaraciones cargadas para actualizar","stop")
            CONTINUE MENU
         END IF      

         -- se invoca la inicializacion del proceso
         CALL fn_acl_inicia_proceso(g_usuario)
         EXIT MENU

      
      COMMAND "Cancelar"
         EXIT MENU
   END MENU

   CLOSE WINDOW w_consulta
   
END MAIN


{
======================================================================
Clave: 
Nombre: fn_acl_inicia_proceso
Fecha creacion: Febreo 07, 2014
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Inicia el proceso de analisis y actualizacion de aclaraciones sin causal

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_acl_inicia_proceso(p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_resultado       SMALLINT,
       v_mensaje         STRING
       
   -- este proceso inicia por webservices, no tiene archivo
   LET v_nombre_archivo = "NA"
   
   -- el proceso no tiene folio
   LET v_folio = 0

   -- se verifica si se puede continuar con la operacion
   LET v_resultado = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   IF ( v_resultado = 0 ) THEN

      -- se genera el pid 
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid

      CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 v_folio           ,
                                 "ACLP41"          ,
                                 v_nombre_archivo  ,
                                 p_usuario_cod)  RETURNING v_resultado
                                  
      -- el proceso se registro correctamente
      IF ( v_resultado = 0 ) THEN

         -- inicia la operacion
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"ACLP41","NA",p_usuario_cod)
         RETURNING v_resultado
      
         -- se invoca la ejecucion del programa lanzado
         LET v_s_comando = "nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/ACLP41.42r ",
                            p_usuario_cod CLIPPED, " ",
                            g_pid                , " " ,
                            g_proceso_cod        , " " ,
                            g_opera_cod          , " ",
                            v_folio              , " '",
                            v_nombre_archivo CLIPPED ,"' ",
                            " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                            "/nohup:",g_pid  USING "&&&&&",":",
                            g_proceso_cod    USING "&&&&&",":",
                            g_opera_cod      USING "&&&&&" ,
                            " 2>&1 &"
                          
         DISPLAY v_s_comando
         RUN v_s_comando
         CALL fn_mensaje("Atenci�n",
                "Se ha enviado el programa de verificaci�n de aclaracione sin causal.\nPodr� revisar el resultado en el monitor de ejecuci�n de procesos",
                "information")

      ELSE
         CALL fn_mensaje("Atenci�n","No se pudo iniciar el proceso","information")
      END IF
   ELSE
      -- no se puede ejecutar la operacion
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atenci�n", v_mensaje, "stop")
      
     MENU
        COMMAND "Cerrar"
           EXIT MENU
     END MENU
   END IF

END FUNCTION