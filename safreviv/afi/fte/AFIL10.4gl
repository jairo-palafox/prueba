--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIL10                                                                 #
#Objetivo     => Lanzador del programa de actualizacion de datos de BDNSVIV en los      #
#                registros de afiliacion                                                #
#Fecha inicio => Marzo 04, 2013                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "AFIG01.4gl"
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
       v_acepto              SMALLINT -- booleana para monitorear si inicio el proceso
       

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
   LET g_proceso_cod = g_proceso_cod_afi_actualiza_bdnsviv -- Actualizar los datos de AFI con la BDNSVIV
   LET g_opera_cod   = g_opera_cod_afi_actualiza_bdnsviv -- lectura de la bdnsviv y actualizacion de AFI

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'afi'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_concilia WITH FORM "AFIL101"
   
   -- se obtienen las descripciones del proceso y la operacion
   CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod) RETURNING v_opera_desc

   -- se despliegan las descripciones
   DISPLAY v_proceso_desc, v_opera_desc
   TO proceso_desc, opera_desc
   
   -- se captura el folio
   MENU

      BEFORE MENU
         -- se asignan los valores por omision
         LET v_folio = NULL   
         
         -- se busca el folio mas reciente de conciliacion BDNSVIV asumiendo que seran
         -- los ultimos datos cargados con 
         SELECT MAX(b.folio)
         INTO  v_folio
         FROM  glo_folio b
         WHERE b.proceso_cod = 2101 -- conciliacion BDNSVIV
         AND b.status = 0

         -- se despliega el folio
         DISPLAY BY NAME v_folio
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE MENU
         END IF

         -- se asume que el usuario no aceptara lanzar el proceso
         LET v_acepto = FALSE
         
         -- se confirma con el usuario que se ejecutara el proceso
         MENU "Confirmación"
         ATTRIBUTES (STYLE="dialog",COMMENT = "¿Desea iniciar el proceso de actualizacion\nde datos de Afiliación?",IMAGE="question")
           ON ACTION ACCEPT
              -- se invoca la ejecucion del stored procedure
              CALL fn_preliquida_ret_disposicion(v_folio, p_usuario_cod)
              LET v_acepto = TRUE
              EXIT MENU
              
           ON ACTION CANCEL
              EXIT MENU
         
         END MENU

         -- si se acepto, se termina el menu
         IF ( v_acepto ) THEN
            EXIT MENU
         END IF
        
      ON ACTION CANCEL
         EXIT MENU
   
   END MENU
   
   CLOSE WINDOW w_folio_concilia
   
END MAIN


{
======================================================================
Clave: 
Nombre: fn_preliquida_ret_disposicion
Fecha creacion: Febrero 23, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de retiros por disposicion de recursos
para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_preliquida_ret_disposicion(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_i_resultado     INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT  --codigo de error fn_actualiza_opera_ini


   -- el proceso no carga archivo
   LET v_nombre_archivo = "NA"

   CALL fn_mensaje("VPD","LANZADO AQUI","info")
   RETURN
   
   -- se verifica si se puede iniciar la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado

   IF ( v_i_resultado = 0 ) THEN
       	
      -- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,
                                     g_proceso_cod,
                                     g_opera_cod,
                                     p_folio,
                                     "RETL11",
                                     v_nombre_archivo,
                                     p_usuario_cod
                                     )
         RETURNING r_resultado_opera

      IF ( r_resultado_opera  = 0 ) THEN   
       	 -- se ejecuta el programa lanzado que realiza la actualizacion de los datos
         -- de afiliados
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/AFIP10 ",
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
         CALL fn_mensaje("Atención", "Se ha enviado el proceso.\nPodrá revisar el resultado en el monitor de ejecución de procesos", "information")
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