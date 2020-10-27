--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/06/2012
--===============================================================
#########################################################################################
#Modulo       => MDT                                                                    #
#Programa     => MDTL14                                                                 #
#Objetivo     => Programa que interfaz de usuario para generar reverso de carga de      #
#             => archivo de validacion de recurrente de mandatos                        #
#Fecha inicio => Junio 05, 2012                                                         #
#########################################################################################
DATABASE safre_viv
--GLOBALS
DEFINE g_enter char(01)
DEFINE v_ruta_bin string
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  INTEGER   , -- codigo del proceso
       g_opera_cod    SMALLINT  , -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    char(40)
       END RECORD
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod
      ,p_s_titulo       STRING -- titulo de la ventana
      ,v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo

--END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,v_folio          DECIMAL (9,0)
       ,v_s_cadena       STRING -- cadena de texto
       ,v_cbx_archivos     ui.ComboBox -- combo de afores
       ,v_i_conArch      INTEGER
       ,v_r_glo_ctr_archivo      RECORD
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
       END RECORD
       ,v_proceso_desc    CHAR(40)
       ,v_opera_desc      CHAR(40)
       

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
   LET g_proceso_cod = 1303
   LET g_opera_cod   = 1

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO g_pid
   FROM bat_ctr_proceso
   WHERE proceso_cod = g_proceso_cod
   AND   estado_cod  in (2,3) -- procesando o con error

   -- se obtienen las rutas de control del modulo

   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'mdt'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
  
--   CALL fn_recuper_datos_proceso(g_proceso_cod, g_opera_cod)

   CALL fn_recuper_datos_proceso(1308, 1)
   
    RETURNING v_proceso_desc, v_opera_desc
     
  -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_archivo WITH FORM "MDTL140"
   -- Muestra detalle de operación
   DISPLAY BY NAME v_proceso_desc, v_opera_desc
   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_archivos = ui.ComboBox.forName("formonly.cmb_archivo")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_archivos.clear()
   
   CALL v_cbx_archivos.addItem("-1"," ")
   
   -- se captura el folio
   INPUT
    v_nombre_archivo
   WITHOUT DEFAULTS
   FROM
    cmb_archivo
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_nombre_archivo   = "-1"       
         
         -- se llena el arreglo de folios
         DECLARE cur_archivos CURSOR FOR
         SELECT a.nombre_archivo
           FROM glo_ctr_archivo a
          WHERE a.proceso_cod = g_proceso_cod
            AND a.estado = 1 -- cargado
            --AND (a.folio IS NULL OR a.folio = 0)
            AND a.opera_cod = 1

         LET v_i_conArch = 0
         FOREACH cur_archivos INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_archivos.addItem(
                 v_r_glo_ctr_archivo.nombre_archivo , v_s_cadena CLIPPED)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente Cargados","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_nombre_archivo IS NULL OR v_nombre_archivo = "-1") THEN
            CALL fn_mensaje("Atención","Es necesario capturar un archivo","stop")
            CONTINUE INPUT
         END IF
      
         -- se invoca la ejecucion del stored procedure
         DISPLAY "v_nombre_archivo",v_nombre_archivo,"-"
         CALL fn_incializa_proceso()
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_archivo
END MAIN

FUNCTION fn_incializa_proceso()
DEFINE v_folio     LIKE mdt_lote_mandato.folio,
       v_comando   STRING 
DEFINE v_continua  SMALLINT
DEFINE v_pid       INTEGER
DEFINE v_proceso_cod INTEGER ,
       v_opera_cod   INTEGER

LET v_proceso_cod = 1308
LET v_opera_cod = 1
LET v_folio = 0
       
   WHENEVER ERROR CONTINUE
   CALL fn_valida_operacion(g_pid,1301,1) RETURNING v_continua
   IF(v_continua <> 0)THEN
      # Imprime en pantalla
      CALL fn_muestra_inc_operacion(v_continua)
   ELSE
      #Genera identificador del proceso
      CALL fn_genera_pid(v_proceso_cod, v_opera_cod,p_usuario_cod)
                        RETURNING v_pid
      IF(SQLCA.SQLCODE <> 0)THEN
         CALL fn_mensaje(p_s_titulo,"Ocurrió un error al generar PID","about")
         DISPLAY "Error al generar pid (Codigo):",SQLCA.SQLCODE
         RETURN 0         
      END IF
      #inicializa el proceso con todas sus operaciones en estado LISTO
      CALL fn_inicializa_proceso(v_pid, v_proceso_cod, v_opera_cod, v_folio,
                                 "MDTL14","", p_usuario_cod) 
                        RETURNING v_continua
      IF(v_continua  <> 0)THEN
         # Imprime el mensaje de inconsistencia en pantalla
         CALL fn_muestra_inc_operacion(v_continua)
         RETURN 0
      END IF
              
   END IF 
   #Construye comando
   LET v_comando = "nohup fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/MDTR14.42r ",
                                   p_usuario_cod," ",
                                   v_pid," ",
                                   v_proceso_cod," ",
                                   v_opera_cod," ",
                                   v_folio," '",
                                   v_nombre_archivo clipped,
                             "' 1>", seg_modulo_bat.ruta_listados CLIPPED ,
                          "/nohup:",v_pid USING "&&&&&",":",
                                    v_proceso_cod USING "&&&&&",":",
                                    v_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"
                           
   RUN v_comando

   IF(STATUS)THEN
      CALL fn_mensaje(p_s_titulo,"Ocurrió un error al iniciar el proceso batch","about")
      CALL fn_error_opera(v_pid,v_proceso_cod,v_opera_cod)
                         RETURNING v_continua
      IF(v_continua)THEN
         # Imprime el mensaje de inconsistencia en consola y archivo
         CALL fn_muestra_inc_operacion(v_continua)
      END IF
   END IF
   CALL fn_mensaje(p_s_titulo, 
                   "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                   "bn_about")
END FUNCTION


FUNCTION fn_recuper_datos_proceso(p_proceso, p_operacion)

  DEFINE p_proceso    INTEGER      
        ,p_operacion  SMALLINT
   ,v_proceso_desc    CHAR(40)
   ,v_opera_desc      CHAR(40)
   
   -- <Recupera descripción del proceso> --
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso

    -- <Recupera descripción de operación> --
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod   = p_operacion
      
 --DISPLAY BY NAME v_proceso_desc, v_opera_desc
 RETURN v_proceso_desc, v_opera_desc
END FUNCTION -- fn_recuper_datos_proceso
