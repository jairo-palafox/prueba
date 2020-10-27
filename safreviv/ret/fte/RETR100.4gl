--===============================================================
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
       p_mensaje         STRING -- cuerpo del mensaje enviado

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
       v_sql_procedure  STRING,
       v_cod_error      SMALLINT,
       v_error_isam     INTEGER,
       v_mensaje_error  VARCHAR(255)
       
       #se aregaron los campos por construccion de analisis para los registros procesados
       ,v_diag_registro  INTEGER  
       ,v_count_diag_registro INTEGER
       ,v_descrip        VARCHAR(50)   

      LET v_diag_registro       = 0 
      LET v_count_diag_registro = 0
      LET v_descrip             = ""  

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