-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:18/04/2012
--===============================================================

#############################################################################
#Módulo          => RET                                                     #
#Programa        => RETP100.4gl                                              #
#Objetivo        => Programa de registro de información histórica para la   #
#                   carga inicial de retiros SPESS                          #
#Fecha Inicio    => 07 Febrero 2012                                         #
#############################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, # Código del proceso
       g_opera_cod_integracion     LIKE cat_operacion.opera_cod, # Código de operación
       g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  # Código de operación
       
END GLOBALS

MAIN
DEFINE p_opera_cod_carga LIKE cat_operacion.opera_cod, # Código de operacion
       p_usuario_cod     LIKE seg_usuario.usuario_cod, # Clave de usuario
       p_nom_archivo     STRING, 
       v_cadena_pid      VARCHAR(5),
       v_cadena_proc     VARCHAR(5),
       v_cadena_opera    VARCHAR(5),
       v_comando         STRING,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_listados   LIKE seg_modulo.ruta_listados,
       r_folio           DECIMAL(9,0),
       r_resultado_opera SMALLINT,
       r_ruta_vacia      LIKE seg_modulo.ruta_bin,
       r_bnd_estado      SMALLINT,
       p_titulo          STRING, -- titulo del mensaje enviado en el correo
       p_mensaje         STRING, -- cuerpo del mensaje enviado
       v_s_comando       STRING,
       v_c_ruta_bin_acr  LIKE seg_modulo.ruta_bin, -- ruta del bin de acr
       v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
       v_ruta_vacia      STRING

   WHENEVER ERROR CONTINUE
   LET r_bnd_estado = 0 # Inicializa bandera
   
   #Si se ha recibido parámetros se continua    
   #Primer parámetro
   LET p_usuario_cod = ARG_VAL(1)
   #Segundo parámetro
   LET g_pid         = ARG_VAL(2)
   #Tercer parámetro
   LET g_proceso_cod = ARG_VAL(3)
   #Cuarto parámetro
   LET g_opera_cod_integracion = ARG_VAL(4) # Paso de información a las tablas históricas
   #Quinto parámetro
   LET r_folio       = ARG_VAL(5)
   #Segundo parámetro
   LET p_nom_archivo = ARG_VAL(6)
   # Indicamos operacion de carga
   LET p_opera_cod_carga = 1
   # Archivo de bitacora
   CALL STARTLOG(p_usuario_cod CLIPPED||".RETP100.log")
   
   CALL fn_display_proceso(0," Integración Carga Retiros SPESS")
   
   # Genera folio
   LET r_folio = 0.0
   
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod_integracion, p_usuario_cod) 
               RETURNING r_folio

   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               g_opera_cod_integracion,
                               r_folio,
                               "RETP100",
                               p_nom_archivo,
                               p_usuario_cod)
               RETURNING r_resultado_opera
   --DISPLAY "Resultado de inicia operacion:"
   --DISPLAY "Folio obtenido: ", r_folio

   # Recupera el nombre del archivo cargado
   CALL fn_recupera_arch_cargado(g_proceso_cod,p_opera_cod_carga)
                        RETURNING p_nom_archivo
   
   # Llamada a ejecución de procedimiento almacenado
   CALL fn_integra_SPESS(r_folio, p_usuario_cod, p_nom_archivo) RETURNING r_resultado_opera
  
   # cambia a estado errone si no se ejecutó correctamente el SP
   IF ( r_resultado_opera ) THEN
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_integracion) 
                 RETURNING r_resultado_opera
      CALL fn_act_edo_archivo(p_nom_archivo,r_folio,3,p_usuario_cod)
                     RETURNING r_resultado_opera

      LET p_mensaje = "El proceso de Integración ha finalizado pero con errores."
   ELSE
      # Actualiza el estado del archivo en glo_ctr_archivo a integrado
      # 2 = integrado
      CALL fn_act_edo_archivo(p_nom_archivo,r_folio,2,p_usuario_cod)
                     RETURNING r_resultado_opera

      LET p_mensaje = "Integración realizada con éxito."
      # Se finaliza la carga de registros historicos
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod_integracion) 
                         RETURNING r_resultado_opera
      #PRODINFXV-91
      #Se corre el programa en nohup que llena la tabla empleada para el proceso de
      #notificacion por SMS o correo
      -- Se obtienen las rutas de los ejecutables
      CALL fn_rutas("ret") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
      CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat

      LET v_s_comando = "nohup fglrun "
                        ,v_c_ruta_bin_acr CLIPPED
                        ,"/RETL396.42r "
                        ,p_usuario_cod, " "
                        ,g_proceso_notificacion_datamart_diario," "
                        ,g_opera_cod_validacion_datamart_diario," "
                        ,r_folio
                        ," 1>", v_c_ruta_list_bat CLIPPED
                        ,"/nohup:",g_proceso_notificacion_datamart_diario USING "&&&&&",":"
                        ,g_opera_cod_validacion_datamart_diario USING "&&&&&"
                        ," 2>&1 &"

      DISPLAY v_s_comando
      RUN v_s_comando
   END IF 

   -- se muestra la finalizacion del proceso
   CALL fn_display_proceso(1," Integración carga SPESS ")
   LET p_titulo = "Finalización de operación - Carga SPESS - Integración"
   CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                          g_opera_cod_integracion, 
                          NULL, p_titulo,p_mensaje)

END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la integracion
{ ==========================================================================
Clave:  fn_integra_SPESS
Nombre: fn_integra_SPESS
Fecha creacion: 10 de Enero de 2012
Autor: Hugo César Ramírez García
Narrativa del proceso que realiza:
Ejecuta el stored procedure de integracion de la carga inicial de retiros SPESS
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_integra_SPESS(p_folio, p_usuario_cod, p_nombre_archivo)
DEFINE p_folio          LIKE glo_folio.folio,
       p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_archivo_envio  LIKE glo_ctr_archivo.nombre_archivo,
       v_sql_procedure  STRING,
       v_ruta_envio     LIKE seg_modulo.ruta_envio,
       v_cod_error      SMALLINT,
       v_error_isam     INTEGER,
       v_cant_rechazos  INTEGER,
       v_mensaje_error  VARCHAR(255)
       
       #se aregaron los campos por construccion de analisis para los registros procesados
       ,v_diag_registro  INTEGER  
       ,v_count_diag_registro INTEGER
       ,v_descrip        VARCHAR(50)   

      LET v_diag_registro       = 0 
      LET v_count_diag_registro = 0
      LET v_descrip             = ""  
      LET v_cant_rechazos       = 0

   -- cadena de ejecucion del SP de integracion de carga inicial del SPESS
   LET v_sql_procedure = "EXECUTE FUNCTION fn_ret_integra_spes(?,?,?,?,?)" 
   PREPARE prp_copia_spes FROM v_sql_procedure

   -- se ejecuta el stored procedure
   --DISPLAY p_usuario_cod, p_folio, p_nombre_archivo, g_pid, g_proceso_cod
   EXECUTE prp_copia_spes USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, g_proceso_cod
   INTO v_cod_error, v_error_isam, v_mensaje_error
     
   IF ( v_cod_error = 0 ) THEN
      DISPLAY "\nResultado de ejecución del proceso: ", v_cod_error
      DISPLAY v_mensaje_error
  

     DECLARE cur_log CURSOR FOR SELECT dat.diag_registro,COUNT(dat.diag_registro),rdd.des_corta
                             FROM ret_datamart dat,
                                  ret_diag_datamart rdd                             
                            WHERE dat.folio = p_folio
                              AND dat.diag_registro =  rdd.diag_registro
                              AND rdd.estado_resolucion = 1
                            GROUP BY diag_registro,rdd.des_corta
                            ORDER BY diag_registro,rdd.des_corta


      DISPLAY "#################   ACEPTADOS   #####################"
      DISPLAY "Diagnostico                      ---   Registros procesados"
      FOREACH cur_log INTO v_diag_registro ,v_count_diag_registro,v_descrip

          IF v_descrip = "" OR v_descrip IS NULL THEN
            LET v_descrip = "               " 
          END IF 

         DISPLAY  v_diag_registro , "-" , v_descrip , "   ", v_count_diag_registro             
      END FOREACH
      DISPLAY "#############################################################"
      
     DECLARE cur_log_rec CURSOR FOR SELECT dat.diag_registro,COUNT(dat.diag_registro),rdd.des_corta
                             FROM ret_datamart dat,
                                  ret_diag_datamart rdd                             
                            WHERE dat.folio = p_folio
                              AND dat.diag_registro =  rdd.diag_registro
                              AND rdd.estado_resolucion <> 1
                            GROUP BY diag_registro,rdd.des_corta
                            ORDER BY diag_registro,rdd.des_corta


      DISPLAY "#################   RECHAZADOS   #####################"
      DISPLAY "Diagnostico                      ---   Registros procesados"
      FOREACH cur_log_rec INTO v_diag_registro ,v_count_diag_registro,v_descrip

          IF v_descrip = "" OR v_descrip IS NULL THEN
            LET v_descrip = "               " 
          END IF 

         DISPLAY  v_diag_registro , "-" , v_descrip , "   ", v_count_diag_registro             
      END FOREACH

      SELECT COUNT(*)
        INTO v_cant_rechazos
        FROM ret_datamart_rch_carga
       WHERE folio = p_folio
      IF v_cant_rechazos > 0 THEN
      ----- Se reportan los registros rechazados en un archivo de salida
      -----
          SELECT ruta_envio
            INTO v_ruta_envio
            FROM seg_modulo
           WHERE modulo_cod = 'ret'
          LET v_ruta_envio = v_ruta_envio CLIPPED, "/"
          LET v_archivo_envio = p_nombre_archivo CLIPPED, "rechazos"
          DISPLAY "Genera archivo de rechazos \n"
          DISPLAY "Archivo salida     :", v_archivo_envio
          DISPLAY "Ruta envio         :", v_ruta_envio
          DISPLAY "registros a reportr:", v_cant_rechazos
          CALL fn_genera_archivo_rechazos(p_folio,v_archivo_envio,v_ruta_envio)

          LET v_cant_rechazos = 0;

          SELECT COUNT(*)
            INTO v_cant_rechazos
            FROM ret_datamart_rch_carga
           WHERE folio = p_folio
             AND cod_rechazo = 9

          DISPLAY "Registros con fecha emisión de resolución invalida:", v_cant_rechazos
          LET v_cant_rechazos = 0;

          SELECT COUNT(*)
            INTO v_cant_rechazos
            FROM ret_datamart_rch_carga
           WHERE folio = p_folio
             AND cod_rechazo = 8

          DISPLAY "Registros con fecha inicio de pensión invalida    :", v_cant_rechazos
          LET v_cant_rechazos = 0;

          SELECT COUNT(*)
            INTO v_cant_rechazos
            FROM ret_datamart_rch_carga
           WHERE folio = p_folio
             AND cod_rechazo = 10

          DISPLAY "Registros cargados con anterioridad (repetidos)   :", v_cant_rechazos
          LET v_cant_rechazos = 0;

          SELECT COUNT(*)
            INTO v_cant_rechazos
            FROM ret_datamart_rch_carga
           WHERE folio = p_folio
             AND cod_rechazo = 4

          DISPLAY "Registros no encontrados en la matriz de derechos :", v_cant_rechazos
          
      END IF
      DISPLAY "#############################################################"

          DECLARE cur_log_sinc CURSOR FOR 
          SELECT dat.diag_registro,COUNT(dat.diag_registro)
          FROM ret_datamart dat
          WHERE dat.folio = p_folio
          AND diag_registro NOT IN (SELECT diag_registro FROM ret_diag_datamart)
          GROUP BY diag_registro
          ORDER BY diag_registro



      DISPLAY "#################   SIN CATEGORIA   #####################"
      DISPLAY "No dados de alta en la tabla de diagnostico_datamart"
      DISPLAY "Diagnostico                      ---   Registros procesados"
      FOREACH cur_log_sinc INTO v_diag_registro ,v_count_diag_registro,v_descrip

          IF v_descrip = "" OR v_descrip IS NULL THEN
            LET v_descrip = "               " 
          END IF 

         DISPLAY  v_diag_registro , "-" , v_descrip , "   ", v_count_diag_registro             
      END FOREACH
      DISPLAY "#############################################################"       
      RETURN FALSE
   ELSE
      DISPLAY "\nError ejecucion fn_ret_integra_spes (Codigo): ",v_cod_error
      DISPLAY v_mensaje_error
      RETURN TRUE
   END IF
   
END FUNCTION

FUNCTION fn_genera_archivo_rechazos(p_folio, v_v_nom_arc_rechazados,v_c_ruta_env_arch)
DEFINE  p_folio                  LIKE glo_folio.folio,
        v_v_nom_arc_rechazados   STRING,   --Nombre del archivo
        v_c_ruta_env_arch        LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
        v_s_encabezado_rec       STRING,
        v_s_detalle_rec          STRING,
        v_s_sumario_rec          STRING,
        v_v_ruta_arc_rechazados  STRING, --Ruta del archivo de aceptados
        v_ch_arc_rechazados       base.channel, --Canal para escribir el archivo de salida
        ls_sql                   STRING, --Variable usada para generar las instrucciones sql
        v_id_derechohabiente     LIKE ret_fondo_ahorro.id_derechohabiente,
        v_sum_total_pesos        DECIMAL(12,6),
        v_c_fec_solicitud        CHAR(8),
        v_c_suma_total           CHAR(14),
        v_c_imp_viv72            CHAR(14),
        v_c_tanto_imp_viv72      CHAR(8),
        v_c_entidad              CHAR(6),
        v_c_causal_retiro        CHAR(6),
        v_c_caso_adai            CHAR(10),
        v_f_carga_datamart       DATE,
        v_f_proceso              DATE, 
        v_c_f_inicio_pension     CHAR(8),
        v_c_f_resolucion         CHAR(8),
        v_c_imp_viv97            CHAR(14),
        v_c_imp_viv92            CHAR(14),
        v_c_imp_fondo72          CHAR(14),
        v_c_porcentaje_valuacion CHAR(5),
        v_c_semanas_cotizadas    CHAR(4),
        v_c_diagnostico          CHAR(3),
        v_registros              INTEGER,
        v_cod_rechazo            SMALLINT
 DEFINE r_tmp_ret_encabezado  RECORD
 	        v_tpo_registro     CHAR(2),
 	        v_id_servicio      CHAR(2),
 	        v_id_operacion     CHAR(2),
            v_entidades        CHAR(10),
 	        f_carga_datamart   CHAR(8),
 	        f_proceso          CHAR(8),
            filler             CHAR(270) 
         END RECORD
 DEFINE r_tmp_ret_sumario    RECORD
 	        v_tpo_registro     CHAR(2),
 	        v_id_servicio      CHAR(2),
 	        v_id_operacion     CHAR(2),
            v_entidades        CHAR(10),
 	        f_carga_datamart   CHAR(8),
 	        f_proceso          CHAR(8),
            v_cant_registros   CHAR(6), 
            filler             CHAR(264)
         END RECORD
         
 DEFINE r_tmp_ret_detalle RECORD
        tmp_ret_spes_tpo_registro                    CHAR(2)             ,
        tmp_ret_spes_id_servicio                     CHAR(2)             ,
        tmp_ret_spes_id_operacion                    CHAR(2)             ,
        tmp_ret_spes_nss                             CHAR(11)            ,
        tmp_ret_spes_curp                            CHAR(18)            ,
        tmp_ret_spes_nombre_datamart                 CHAR(50)            ,
        tmp_ret_spes_nombre_afore                    CHAR(40)            ,
        tmp_ret_spes_ap_paterno_afore                CHAR(40)            ,
        tmp_ret_spes_ap_materno_afore                CHAR(40)            ,
        tmp_ret_spes_sec_pension                     CHAR(2)             ,
        tmp_ret_spes_tpo_movimiento                  CHAR(3)             ,
        tmp_ret_spes_regimen                         CHAR(2)             ,
        tmp_ret_spes_tpo_seguro                      CHAR(2)             ,
        tmp_ret_spes_tpo_pension                     CHAR(2)             ,
        tmp_ret_spes_tpo_prestacion                  CHAR(2)             ,
        tmp_ret_spes_art_negativa                    CHAR(3)             ,
        tmp_ret_spes_fracc_negativa                  CHAR(2)             ,
        tmp_ret_spes_num_considerando                CHAR(2)             ,
        tmp_ret_spes_fec_inicio_pension              CHAR(8)            ,
        tmp_ret_spes_fec_resolucion                  CHAR(8)            ,
        tmp_ret_spes_porc_valuacion                  DECIMAL(5,0)        ,
        tmp_ret_spes_semanas_cotizadas               DECIMAL(4,0)        ,
        tmp_ret_spes_diag_retiro                     DECIMAL(3,0)        ,
        tmp_ret_spes_estatus_subcuenta               CHAR(1)             ,
        tmp_ret_spes_importe_viv97                   DECIMAL(14,0)       ,
        tmp_ret_spes_importe_viv92                   DECIMAL(14,0)       ,
        tmp_ret_spes_importe_fondo72                 DECIMAL(14,0)       ,
        tmp_ret_spes_cve_afore                       CHAR(3)             ,
        tmp_ret_spes_cod_rechazo                     CHAR(3)              
    END RECORD
         
 	--se inicializan las variables 
   LET v_s_encabezado_rec = NULL
   LET v_s_detalle_rec    = NULL
   LET v_s_sumario_rec    = NULL
   LET v_sum_total_pesos  = 0.00
   LET v_v_ruta_arc_rechazados = v_c_ruta_env_arch CLIPPED || v_v_nom_arc_rechazados
  
   --Muestra la ruta donde quedará el archivo  
   DISPLAY "\n   Archivo[RECHAZADOS]: ", v_v_ruta_arc_rechazados 
   
   --Creación del canal para la escritura del archivo
   LET v_ch_arc_rechazados = base.Channel .create() 
   
   --Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arc_rechazados.openFile(v_v_ruta_arc_rechazados, "w" )
   
   --Asignación de las variables del encabezado
   LET r_tmp_ret_encabezado.v_tpo_registro    ="01"
   LET r_tmp_ret_encabezado.v_id_servicio     ="04"
   LET r_tmp_ret_encabezado.v_id_operacion    ="19"
   LET r_tmp_ret_encabezado.v_entidades       ="0300104002"
   LET r_tmp_ret_encabezado.filler            =" "
   
   --Asignación del encabezado
   ----- Obtiene las fechas del archivo que vienen en el encabezado
   SELECT f_carga_datamart, f_proceso
     INTO v_f_carga_datamart, v_f_proceso
     FROM ret_cza_datamart
    WHERE folio = p_folio

   LET r_tmp_ret_encabezado.f_carga_datamart = v_f_carga_datamart  USING "yyyymmdd"
   LET r_tmp_ret_encabezado.f_proceso        = v_f_proceso         USING "yyyymmdd"
   
   LET v_s_encabezado_rec = r_tmp_ret_encabezado.v_tpo_registro  ,
                            r_tmp_ret_encabezado.v_id_servicio   ,
                            r_tmp_ret_encabezado.v_id_operacion  ,
                            r_tmp_ret_encabezado.v_entidades     ,
                            r_tmp_ret_encabezado.f_carga_datamart,
                            r_tmp_ret_encabezado.f_proceso,
                            r_tmp_ret_encabezado.filler
   
    --Se escribe el encabezado en el archivo            
    CALL v_ch_arc_rechazados.writeLine(v_s_encabezado_rec)
    
    --Query para la selección de los campos   

    LET ls_sql = "\n SELECT nss                     ,",
                 "\n        curp                    ,",
				 "\n        nombre_datamart         ,",
                 "\n        nombre_afore            ,",
                 "\n        apaterno_afore          ,",
                 "\n        amaterno_afore          ,",
                 "\n        sec_pension             ,",
                 "\n        tpo_movimiento          ,",
                 "\n        regimen                 ,",
                 "\n        tpo_seguro              ,",
                 "\n        tpo_pension             ,",
                 "\n        tpo_prestacion          ,",
                 "\n        art_negativa            ,",
                 "\n        fraccion_negativa       ,",
                 "\n        num_considerando        ,",
                 "\n        f_inicio_pension        ,",
                 "\n        f_resolucion            ,",
                 "\n        porcentaje_valuacion    ,",
                 "\n        semanas_cotizadas       ,",
                 "\n        diagnostico             ,",
                 "\n        estatus_subcuenta       ,",
                 "\n        importe_viv97           ,",
                 "\n        importe_viv92           ,",
                 "\n        importe_fondo72         ,",
                 "\n        cve_afore               ,",
                 "\n        cod_rechazo              ",
                 "\n FROM   ret_datamart_rch_carga   ",
                 "\n WHERE  folio = ",p_folio

    DISPLAY "Query: ",ls_sql
    
    PREPARE stm_det_datamart_rch_carga FROM ls_sql        
    DECLARE cur_det_datamart_rch_carga CURSOR FOR stm_det_datamart_rch_carga
    
    --Se asignan valores por defecto en el detalle
    LET r_tmp_ret_detalle.tmp_ret_spes_tpo_registro = "03"
    LET r_tmp_ret_detalle.tmp_ret_spes_id_servicio  = "04"
    LET r_tmp_ret_detalle.tmp_ret_spes_id_operacion = "19"
    LET v_registros = 0
    DISPLAY "Antes del fetch "	
    FOREACH cur_det_datamart_rch_carga
    INTO 
        r_tmp_ret_detalle.tmp_ret_spes_nss                             ,
        r_tmp_ret_detalle.tmp_ret_spes_curp                            ,
        r_tmp_ret_detalle.tmp_ret_spes_nombre_datamart                 ,
        r_tmp_ret_detalle.tmp_ret_spes_nombre_afore                    ,
        r_tmp_ret_detalle.tmp_ret_spes_ap_paterno_afore                ,
        r_tmp_ret_detalle.tmp_ret_spes_ap_materno_afore                ,
        r_tmp_ret_detalle.tmp_ret_spes_sec_pension                     ,
        r_tmp_ret_detalle.tmp_ret_spes_tpo_movimiento                  ,
        r_tmp_ret_detalle.tmp_ret_spes_regimen                         ,
        r_tmp_ret_detalle.tmp_ret_spes_tpo_seguro                      ,
        r_tmp_ret_detalle.tmp_ret_spes_tpo_pension                     ,
        r_tmp_ret_detalle.tmp_ret_spes_tpo_prestacion                  ,
        r_tmp_ret_detalle.tmp_ret_spes_art_negativa                    ,
        r_tmp_ret_detalle.tmp_ret_spes_fracc_negativa                  ,
        r_tmp_ret_detalle.tmp_ret_spes_num_considerando                ,
        r_tmp_ret_detalle.tmp_ret_spes_fec_inicio_pension              ,
        r_tmp_ret_detalle.tmp_ret_spes_fec_resolucion                  ,
        r_tmp_ret_detalle.tmp_ret_spes_porc_valuacion                  ,
        r_tmp_ret_detalle.tmp_ret_spes_semanas_cotizadas               ,
        r_tmp_ret_detalle.tmp_ret_spes_diag_retiro                     ,
        r_tmp_ret_detalle.tmp_ret_spes_estatus_subcuenta               ,
        r_tmp_ret_detalle.tmp_ret_spes_importe_viv97                   ,
        r_tmp_ret_detalle.tmp_ret_spes_importe_viv92                   ,
        r_tmp_ret_detalle.tmp_ret_spes_importe_fondo72                 ,
        r_tmp_ret_detalle.tmp_ret_spes_cve_afore                       ,
        v_cod_rechazo                       

         
         
         --Asigna máscara al motivo de rechazo
         LET r_tmp_ret_detalle.tmp_ret_spes_cod_rechazo = v_cod_rechazo USING "&&&";
         
         --Formatea los importes
         --LET v_c_imp_viv97       = fn_elimina_punto(r_tmp_ret_detalle.tmp_ret_spes_importe_viv97)
         LET v_c_imp_viv97       = r_tmp_ret_detalle.tmp_ret_spes_importe_viv97       USING "&&&&&&&&&&&&&&"        
         --LET v_c_imp_viv92       = fn_elimina_punto(r_tmp_ret_detalle.tmp_ret_spes_importe_viv92)
         LET v_c_imp_viv92       = r_tmp_ret_detalle.tmp_ret_spes_importe_viv92       USING "&&&&&&&&&&&&&&"
         --LET v_c_imp_fondo72     = fn_elimina_punto(r_tmp_ret_detalle.tmp_ret_spes_importe_fondo72)
         LET v_c_imp_fondo72     = r_tmp_ret_detalle.tmp_ret_spes_importe_fondo72     USING "&&&&&&&&&&&&&&"
         LET v_c_porcentaje_valuacion = r_tmp_ret_detalle.tmp_ret_spes_porc_valuacion USING "&&&&&" 
         LET v_c_semanas_cotizadas    = r_tmp_ret_detalle.tmp_ret_spes_semanas_cotizadas USING "&&&&"
         LET v_c_diagnostico          = r_tmp_ret_detalle.tmp_ret_spes_diag_retiro USING "&&&"


        
        --Escribe en el archivo
        LET v_s_detalle_rec = r_tmp_ret_detalle.tmp_ret_spes_tpo_registro        ,      
                              r_tmp_ret_detalle.tmp_ret_spes_id_servicio         ,      
                              r_tmp_ret_detalle.tmp_ret_spes_id_operacion        ,      
                              r_tmp_ret_detalle.tmp_ret_spes_nss                 ,     
                              r_tmp_ret_detalle.tmp_ret_spes_curp                ,         
                              r_tmp_ret_detalle.tmp_ret_spes_nombre_datamart                 ,
                              r_tmp_ret_detalle.tmp_ret_spes_nombre_afore                    ,
                              r_tmp_ret_detalle.tmp_ret_spes_ap_paterno_afore                ,
                              r_tmp_ret_detalle.tmp_ret_spes_ap_materno_afore                ,
                              r_tmp_ret_detalle.tmp_ret_spes_sec_pension                     ,
                              r_tmp_ret_detalle.tmp_ret_spes_tpo_movimiento                  ,
                              r_tmp_ret_detalle.tmp_ret_spes_regimen                         ,
                              r_tmp_ret_detalle.tmp_ret_spes_tpo_seguro                      ,
                              r_tmp_ret_detalle.tmp_ret_spes_tpo_pension                     ,
                              r_tmp_ret_detalle.tmp_ret_spes_tpo_prestacion                  ,
                              r_tmp_ret_detalle.tmp_ret_spes_art_negativa                    ,
                              r_tmp_ret_detalle.tmp_ret_spes_fracc_negativa                  ,
                              r_tmp_ret_detalle.tmp_ret_spes_num_considerando                ,
                              r_tmp_ret_detalle.tmp_ret_spes_fec_inicio_pension              ,
                              r_tmp_ret_detalle.tmp_ret_spes_fec_resolucion                  ,
                              v_c_porcentaje_valuacion                                       ,
                              v_c_semanas_cotizadas                                          ,
                              v_c_diagnostico                                                ,
                              r_tmp_ret_detalle.tmp_ret_spes_estatus_subcuenta               ,
                              v_c_imp_viv97                                                  ,
                              v_c_imp_viv92                                                  ,
                              v_c_imp_fondo72                                                ,
                              r_tmp_ret_detalle.tmp_ret_spes_cve_afore                       ,
                              r_tmp_ret_detalle.tmp_ret_spes_cod_rechazo                       
                            
         --Se escribe el detalle en el archivo            
         LET v_registros = v_registros + 1
         IF v_registros MOD 100 = 0 THEN 
             DISPLAY "Registros procesados en rechazos <", v_registros
         END IF 
         CALL v_ch_arc_rechazados.writeLine(v_s_detalle_rec)                                               
   
   END FOREACH

   --Se asigna el valor del  tipo de registro para sumario
   LET r_tmp_ret_sumario.v_tpo_registro    = "09"
   LET r_tmp_ret_sumario.v_id_servicio     = "04"
   LET r_tmp_ret_sumario.v_id_operacion    = "01"
   LET r_tmp_ret_sumario.v_entidades       = "0300104002"
   LET r_tmp_ret_sumario.f_carga_datamart  = v_f_carga_datamart  USING "yyyymmdd"
   LET r_tmp_ret_sumario.f_proceso         = v_f_proceso         USING "yyyymmdd"
   LET r_tmp_ret_sumario.v_cant_registros  = v_registros         USING "&&&&&&"
   LET r_tmp_ret_sumario.filler            = " "
   
   --Asigna sumario     
   LET v_s_sumario_rec = r_tmp_ret_sumario.v_tpo_registro,
                         r_tmp_ret_sumario.v_id_servicio ,
                         r_tmp_ret_sumario.v_id_operacion,
                         r_tmp_ret_sumario.v_entidades,
                         r_tmp_ret_sumario.f_carga_datamart,
                         r_tmp_ret_sumario.f_proceso,
                         r_tmp_ret_sumario.v_cant_registros,
                         r_tmp_ret_sumario.filler
                         
   --Se escribe el sumario en el archivo            
   CALL v_ch_arc_rechazados.writeLine(v_s_sumario_rec)
   
   --se cierra el archivo   
   CALL v_ch_arc_rechazados.CLOSE()                    
                                      

END FUNCTION

#Objetivo. Elimina el punto decimal de las cifras
FUNCTION fn_elimina_punto(v_c_cifra)
DEFINE   v_c_cifra              CHAR(14),
         v_t_token              base.StringTokenizer,
         v_c_cifra_caracter     CHAR(14),
         v_c_cifra_enteros      CHAR(12),
         v_c_cifra_decimal      CHAR(2),
         v_s_indice             SMALLINT
         
   --Crea el tokenizer para obtener las subcadenas
   LET v_t_token = base.StringTokenizer.create(v_c_cifra,".")
   
   --Inicializa el índice
   LET v_s_indice = 1
   
   -- Iteración de las cadenas
   WHILE v_t_token.hasMoreTokens()
   
      IF ( v_s_indice = 1 ) THEN
   
          --Se asignan los enteros de la cifra
         LET v_c_cifra_enteros = v_t_token.nextToken()
         LET v_s_indice = v_s_indice + 1 
      ELSE
        --Se asignan los decimales
        LET v_c_cifra_decimal = v_t_token.nextToken()
      END IF  
   
   END WHILE
   
   --Se concatena la cadena
   LET v_c_cifra_caracter = v_c_cifra_enteros CLIPPED || v_c_cifra_decimal
   
   --Regresa la cadena formada  
   RETURN v_c_cifra_caracter
END FUNCTION