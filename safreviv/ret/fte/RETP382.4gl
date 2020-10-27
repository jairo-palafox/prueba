################################################################################
#Módulo        => RET                                                          #        
#Programa      => RETP382                                                      #
#Objetivo      => Programa carga de archivo SIAFF.                             #
#Fecha Inicio  => Agosto, 2015.                                                #
#Requerimiento => 878                                                          #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv
GLOBALS 
DEFINE v_reg_cargados_x_tabla DYNAMIC ARRAY OF RECORD
        v_registro CHAR(2),  --LIKE cat_layout.registro
        v_tabla    CHAR(30), --LIKE cat_layout.tabla
        v_conteo   INTEGER
       END RECORD
       
END GLOBALS 
#Objetivo: Carga Archivo en proceso nohup 
MAIN
DEFINE p_nom_archivo       STRING,
       p_proceso           LIKE cat_proceso.proceso_cod,
       p_operacion         LIKE cat_operacion.opera_cod,
       p_pid               DECIMAL(9,0),
       p_usuario           CHAR(20),
       p_prog_a_lanzar     STRING
DEFINE v_layout            LIKE cat_operacion.layout_cod,
       v_ruta_rescate      LIKE seg_modulo.ruta_rescate, -- ruta de rescate del modulo
       v_ruta_archivo      STRING, -- ruta completa del archivo leido
       v_usuario           LIKE seg_modulo.usuario,
       v_proceso_desc      LIKE cat_proceso.proceso_desc,
       v_extension         LIKE cat_operacion.extension,
       v_opera_desc        LIKE cat_operacion.opera_desc,
       v_indice            INTEGER,
       v_detalle_monitoreo STRING,
       v_archivo_monitoreo STRING,
       v_ruta_listados     LIKE seg_modulo.ruta_listados,
       v_reg_archivo       INTEGER,
       v_reg_aceptados     INTEGER,
       v_reg_rechazados    INTEGER,
       r_bnd_carga         BOOLEAN,
       r_resultado_opera   SMALLINT,
       g_reg_tab_cargados  INTEGER,
       v_comando           STRING,
       v_cadena_registros  STRING,
       v_canal             base.Channel,
       v_ltr_archivo       STRING,
       v_ltr_archivo_aux   STRING,
       v_reg_no_procesados INTEGER,
       v_fecha_inicio      DATETIME YEAR TO SECOND,
       v_mensaje           STRING,
       v_continua          BOOLEAN,
       v_sql               STRING,
       v_ch_archivo        base.channel, -- archivo que se carga
       --Campos de la tabla ret_his_respuesta_siaff
       v_registro_archivo  RECORD    -- registro del archivo
                des_ramo                 CHAR(100)    ,
                des_unidad               CHAR(100)    ,
                uni_folio                CHAR(12)     ,
                archivo_envio            CHAR(250)    ,
                archivo_salida           CHAR(250)    ,
                archivo_acuse            CHAR(250)    ,
                archivo_devol            CHAR(250)    ,
                archivo_regreso          CHAR(250)    ,
                total_rechazo            DECIMAL(16,2),
                estatus_nom              CHAR(4)      ,
                cod_banco                CHAR(4)      ,
                clave_rastreo            CHAR(30)     ,
                estatus_det              CHAR(2)      ,
                des_estatus_det          CHAR(150)    ,
                ramo                     CHAR(2)      ,
                unidad                   CHAR(3)      ,
                folio_clc                CHAR(10)     ,
                f_presenta               DATE         ,
                f_pago                   DATE         ,
                archivo_entrada          CHAR(250)    ,
                nss                      CHAR(20)     ,
                nombre                   CHAR(150)    ,
                cta_bancaria             CHAR(20)     ,
                importe                  DECIMAL(16,2),
                numero_oprbanc           CHAR(10)     ,
                cod_rechazo              CHAR(3)      ,
                des_rechazo              CHAR(150)
        END RECORD
    DEFINE v_nss STRING
       
   #Parametros
   CALL ARG_VAL(1) RETURNING p_usuario
   CALL ARG_VAL(2) RETURNING p_pid
   CALL ARG_VAL(3) RETURNING p_proceso
   CALL ARG_VAL(4) RETURNING p_operacion
   CALL ARG_VAL(5) RETURNING p_nom_archivo
   CALL ARG_VAL(6) RETURNING p_prog_a_lanzar

   -- se inicia el log del programa
   LET v_cadena_registros = p_usuario CLIPPED, ".", "RETP382.log"
   CALL STARTLOG(v_cadena_registros)
   
   LET v_fecha_inicio = CURRENT YEAR TO SECOND
   LET r_resultado_opera = 0
   LET v_indice = 0   
   LET v_mensaje = " "
   #Recuper la información necesaria para cargar el archivo seleccionado 
   #dependidendo del proceso y operación
   CALL fn_recupera_inf_proceso(p_proceso, p_operacion) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario 
   
   #Encabezado para el archivo de monitoreo
   LET v_detalle_monitoreo = " PROCESO            : ",v_proceso_desc,"\n",
                             " OPERACIÓN          : ",v_opera_desc,"\n",
                             " NOMBRE ARCHIVO     : ",p_nom_archivo,"\n",
                             " FECHA              : ",TODAY,"\n",
                             " HORA               : ",TIME(CURRENT),"\n \n \n"
                             
   DISPLAY "Inicio ","\n",v_detalle_monitoreo 
   #Nombre del archivo de monitoreo
   LET v_archivo_monitoreo = "nohup:",p_pid USING "&&&&&",":",p_proceso USING "&&&&&",":",p_operacion USING "&&&&&"
   DISPLAY "========================",v_archivo_monitoreo

   #Genera archivo de monitoreo
   CALL fn_monitorea_proceso(v_archivo_monitoreo,v_ruta_listados,v_detalle_monitoreo)    
   
   #Se elimina los espacios al final de cada variable
   LET v_ruta_rescate = v_ruta_rescate CLIPPED
   LET v_usuario      = v_usuario CLIPPED

   DISPLAY "Preparando lectura de archivo : ", p_nom_archivo
   
   -- se crea el apuntador para apertura y lectura de archivo
   LET v_ch_archivo = base.Channel.create() 
   
   -- se obtiene la ruta de rescate del proceso
   SELECT ruta_rescate
   INTO   v_ruta_rescate
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"
   
   -- la ruta completa del archivo es la ruta rescate mas el nombre del archivo
   LET v_ruta_archivo = v_ruta_rescate
   
   LET v_ruta_archivo = v_ruta_archivo.trim(),"/", p_nom_archivo
   
   DISPLAY "Ruta archivo: ", v_ruta_archivo
   
   -- se abre el archivo
   CALL v_ch_archivo.openFile(v_ruta_archivo,"r")

   -- los registros del archivo estan separados por pipes
   CALL v_ch_archivo.setDelimiter("|")

   -- se genera la tabla temporal en base de datos TMP
   DISPLAY "Generando tabla temporal tmp_ret_his_respuesta_siaff"

   -- cambio a base de datos temporal
   DATABASE safre_tmp

   -- se regenera la tabla temporal
   LET v_sql = "\nDROP TABLE IF EXISTS tmp_ret_his_respuesta_siaff"
   
   -- se inician los contadores
   LET v_reg_archivo   = 0
   LET v_reg_aceptados = 0
   
   PREPARE sid_tabla_temporal FROM v_sql
   EXECUTE sid_tabla_temporal
   
   -- se crea la tabla temporal
   LET v_sql = "\nCREATE TABLE tmp_ret_his_respuesta_siaff (            ",
               "\n        des_ramo                 CHAR(100)    ,   ",
               "\n        des_unidad               CHAR(100)    ,   ",
               "\n        uni_folio                CHAR(12)     ,   ",
               "\n        archivo_envio            CHAR(250)    ,   ",
               "\n        archivo_salida           CHAR(250)    ,   ",
               "\n        archivo_acuse            CHAR(250)    ,   ",
               "\n        archivo_devol            CHAR(250)    ,   ",
               "\n        archivo_regreso          CHAR(250)    ,   ",
               "\n        total_rechazo            DECIMAL(16,2),   ",
               "\n        estatus_nom              CHAR(4)      ,   ",
               "\n        cod_banco                CHAR(4)      ,   ",
               "\n        clave_rastreo            CHAR(30)     ,   ",
               "\n        estatus_det              CHAR(2)      ,   ",
               "\n        des_estatus_det          CHAR(150)    ,   ",
               "\n        ramo                     CHAR(2)      ,   ",
               "\n        unidad                   CHAR(3)      ,   ",
               "\n        folio_clc                CHAR(10)     ,   ",
               "\n        f_presenta               DATE         ,   ",
               "\n        f_pago                   DATE         ,   ",
               "\n        archivo_entrada          CHAR(250)    ,   ",
               "\n        nss                      CHAR(11)     ,   ",
               "\n        nombre                   CHAR(150)    ,   ",
               "\n        cta_bancaria             CHAR(20)     ,   ",
               "\n        importe                  DECIMAL(16,2),   ",
               "\n        numero_oprbanc           CHAR(10)     ,   ",
               "\n        cod_rechazo              CHAR(3)      ,   ",
               "\n        des_rechazo              CHAR(150)    )   "
               
   PREPARE sid_crea_tabla FROM v_sql
   EXECUTE sid_crea_tabla

   -- se lee el archivo para guardar los datos
   WHILE ( v_ch_archivo.read([v_registro_archivo.*]) )
       -- Se omite el encabezado
       IF v_reg_archivo > 0 THEN
           LET v_nss = v_registro_archivo.nss
           LET v_nss = v_nss.trim()
           IF v_nss.getLength() > 11 THEN
              -- se elimina el "relleno"
              LET v_nss = v_nss.subString((v_nss.getLength() - 10),v_nss.getLength())
              LET v_registro_archivo.nss = v_nss
           END IF
           DISPLAY "Registro leido (REFERENCIA): ", v_registro_archivo.nss
         
           -- se inserta un registro
           INSERT INTO tmp_ret_his_respuesta_siaff values (v_registro_archivo.*)
        
           -- si no hubo error en la insercion
           IF ( SQLCA.SQLCODE = 0 ) THEN
               -- se cuenta un registro aceptado
               LET v_reg_aceptados = v_reg_aceptados + 1
           END IF
       END IF
       -- se cuenta un registro leido
       LET v_reg_archivo = v_reg_archivo + 1
   END WHILE

   -- Se omite el encabezado
   LET v_reg_archivo = v_reg_archivo - 1

   -- se regresa a safreviv
   DATABASE safre_viv
   DISPLAY "Contadores: regs archivo >", v_reg_archivo, "< regs aceptados >", v_reg_aceptados, "< regs rechazados >", v_reg_archivo - v_reg_aceptados, "<"
   -- si el numero de registros leidos es igual al de aceptados
   IF ( v_reg_archivo = v_reg_aceptados ) THEN
      -- la carga es correcta
      DISPLAY "La carga se ha realizdo correctamente..."
      
      -- Almacena registro para archivo procesado correctamente
      CALL fn_ingresa_etapa(p_proceso, p_operacion, p_nom_archivo) RETURNING r_bnd_carga
      
      IF ( r_bnd_carga ) THEN
         -- Finaliza la operacion de carga de archivo
         CALL  fn_actualiza_opera_fin(p_pid,p_proceso,p_operacion)
                          RETURNING r_resultado_opera
      END IF

      LET v_mensaje = "El proceso de carga ha finalizado correctamente"
      
      -- Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso, 
                             p_operacion, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                              v_mensaje
                             )
      
                          
   ELSE
      -- hubo un error en la carga
      DISPLAY "Hubo un error en la carga"
      LET v_mensaje = "Hubo un error en la carga"

      
      CALL fn_error_opera(p_pid,p_proceso,p_operacion) 
           RETURNING r_resultado_opera

            
      # Envia correo de estado de operación
      CALL fn_correo_proceso(p_pid, 
                             p_proceso, 
                             p_operacion, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - CARGA DE ARCHIVO',
                              v_mensaje
                             )
   END IF
END MAIN