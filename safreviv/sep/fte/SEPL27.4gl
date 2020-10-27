--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => SEP                                                                    #
#Programa     => SEPL27                                                                 #
#Objetivo     => Programa que ejecuta el programa Batch que genera el archivo de baja   #
#                de notificacion                                                        #
#Fecha inicio => Junio 23, 2012                                                         #
#########################################################################################
DATABASE safre_viv
--GLOBALS "SEPG01.4gl"
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
       rec_sep_expediente     RECORD -- registro de expediente para archivo de salida
          trabajador            CHAR(11)     ,
          nombre_trabajador     CHAR(120)    ,
          num_caso              CHAR(40)     ,
          acreditado            CHAR(11)     ,
          nombre_acreditado     CHAR(40)     ,
--          num_credito           DECIMAL(10,0),
          f_envio_dm            CHAR(10)     ,
          f_confirmacion        CHAR(10)
       END RECORD,
       arr_sep_expediente     DYNAMIC ARRAY OF RECORD -- arreglo de expediente para archivo de salida
          num                   SMALLINT     ,
          trabajador            CHAR(11)     ,
          nombre_trabajador     CHAR(120)    ,
          num_caso              CHAR(40)     ,
          acreditado            CHAR(11)     ,
          nombre_acreditado     CHAR(40)     ,
--          num_credito           DECIMAL(10,0),
          f_envio_dm            DATE         ,
          f_confirmacion        DATE
       END RECORD,
       v_conteo              SMALLINT, -- contador
       v_sql                 STRING, -- cadena con un enunciado SQL
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
   LET g_proceso_cod = 2216 -- Baja de notificacion
   LET g_opera_cod   = 1 -- Generar archivo de salida

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'sep'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_notificacion_baja WITH FORM "SEPL271"
   
   -- se leen los expedientes  
   LET v_sql = "SELECT\n",
               "b.trabajador,\n",
               "b.nombre_trabajador,\n",
               "b.num_caso,\n",
               "b.acreditado,\n",
               "b.nombre_acreditado,\n",
--               "b.num_credito,\n",
               "b.f_envio_dm,\n",
               "b.f_confirmacion\n",
               "FROM\n",
               "sep_expediente a,\n",
               "sep_aviso_desvinculacion b\n",
               "WHERE\n",
               "a.estado IN (40,45,50)\n",
               "AND\n",
               "a.ind_baja_notificacion = 2",
               "AND\n",
               "a.id_expediente = b.id_expediente"

   -- se prepara la consulta
   PREPARE sid_expediente_baja FROM v_sql
   DECLARE cur_expediente_baja CURSOR FOR sid_expediente_baja
   
   -- se inicia el contador
   LET v_conteo = 1
   
   -- se llena el arreglo de despliegue
   FOREACH cur_expediente_baja INTO rec_sep_expediente.*
      
      -- si se tiene expediente
      IF ( rec_sep_expediente.trabajador IS NOT NULL ) THEN
         LET arr_sep_expediente[v_conteo].num               = v_conteo
         LET arr_sep_expediente[v_conteo].trabajador        = rec_sep_expediente.trabajador       
         LET arr_sep_expediente[v_conteo].nombre_trabajador = rec_sep_expediente.nombre_trabajador
         LET arr_sep_expediente[v_conteo].num_caso          = rec_sep_expediente.num_caso         
         LET arr_sep_expediente[v_conteo].acreditado        = rec_sep_expediente.acreditado       
         LET arr_sep_expediente[v_conteo].nombre_acreditado = rec_sep_expediente.nombre_acreditado
         LET arr_sep_expediente[v_conteo].f_envio_dm        = rec_sep_expediente.f_envio_dm       
         LET arr_sep_expediente[v_conteo].f_confirmacion    = rec_sep_expediente.f_confirmacion   
         
         -- se incrementa el contador
         LET v_conteo = v_conteo + 1
      END IF
   END FOREACH
   IF(arr_sep_expediente.getLength() > 0)THEN
      -- se captura el folio
      DISPLAY ARRAY arr_sep_expediente TO tbl_expediente_baja.*
      ATTRIBUTES (UNBUFFERED)

         -- se muestra el total de registros
         BEFORE DISPLAY
            DISPLAY arr_sep_expediente.getLength() TO total_registros
         
            -- si no hay registros
            IF ( arr_sep_expediente.getLength() < 1 ) THEN
               CALL fn_mensaje("Atención","Sin avisos a generar","stop")
               EXIT DISPLAY
            END IF
         
         ON ACTION ACCEPT
            -- se invoca la generacion del archivo
            CALL fn_archivo_salida_baja_notificacion(p_usuario_cod)
            EXIT DISPLAY
        
         ON ACTION CANCEL
            EXIT DISPLAY
   
      END DISPLAY
   ELSE
      CALL fn_mensaje(p_s_titulo,"No se encontraron registros para baja de notificación","information")
   END IF
   
   CLOSE WINDOW w_notificacion_baja
   
END MAIN


{
======================================================================
Clave: 
Nombre: fn_archivo_salida_baja_notificacion
Fecha creacion: Junio 23, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta en segundo plano el programa que genera el archivo de salida
de baja de notificacion de separacion de cuentas

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_archivo_salida_baja_notificacion(p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_i_resultado     INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT  --codigo de error fn_actualiza_opera_ini

       -- no se tiene archivo
       LET v_nombre_archivo = "NA"

       -- se verifica si se puede iniciar la operacion
       CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado

       IF ( v_i_resultado = 0 ) THEN
       	  -- el folio se genera al emitir el archivo
       	  LET p_folio = 0

          -- se solicita confirmacion por parte del usuario para generar el archivo
          MENU "Atención"
          ATTRIBUTES ( STYLE = "dialog" , COMMENT = "Confirmar generar archivo")
          
             -- si se acepta
             COMMAND "Aceptar"
                -- se genera el PID
 	              CALL fn_genera_pid(g_proceso_cod,g_opera_cod, p_usuario_cod)
       	                           RETURNING g_pid
                
                -- se inicia el proceso
                CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,p_folio,"SEPL27",v_nombre_archivo,p_usuario_cod)
       	                                  RETURNING v_i_resultado
       	                                  
                -- si se inicio el proceso
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
       	           IF (r_resultado_opera  = 0) THEN   
       	           
       	           
                       LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/SEPP27 ",
                                         p_usuario_cod CLIPPED, " ",
                                         g_pid         , " " ,
                                         g_proceso_cod , " " ,
                                         g_opera_cod   ," ",
                                         p_folio       ," ",
                                         v_nombre_archivo CLIPPED," ",
                                         " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                         "/nohup:",g_pid USING "&&&&&",":",
                                         g_proceso_cod USING "&&&&&",":",
                                         g_opera_cod   USING "&&&&&" ,
                                         " 2>&1 &"
                       DISPLAY v_s_comando                        
                       RUN v_s_comando
                       CALL fn_mensaje("Atención",
                                       "Se ha enviado la generación del archivo de salida.\nPodrá revisar el resultado en el monitor de ejecución de procesos.",
                                       "information")
                   ELSE -- no se puede iniciar la operacion
                   	CALL fn_recupera_inconsis_opera(r_resultado_opera) RETURNING v_mensaje
                   
                   	CALL fn_mensaje("Atención", v_mensaje, "stop")
                   END IF
                ELSE -- no se pudo iniciar el proceso
                   CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
                   CALL fn_mensaje("Atención", v_mensaje, "stop")
                
                END IF
                EXIT MENU
             
             -- si se cancela
             COMMAND "Cancelar"
                EXIT MENU
          END MENU
       ELSE -- no se puede enviar la operacion
          CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje

          CALL fn_mensaje("Atención", v_mensaje, "stop")

         MENU
            COMMAND "Cerrar"
               EXIT MENU
         END MENU
           
       END IF
       
 
END FUNCTION