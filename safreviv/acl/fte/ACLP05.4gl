--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#############################################################################
#M�dulo          => ACL                                                     #
#Programa        => ACLP05.4gl                                              #
#Objetivo        => Programa de registro de informaci�n hist�rica para el   #
#                   m�dulo de "Aclaraciones Enacalara"                      #
#Fecha Inicio    => 07 Febrero 2012                                         #
#Modificacion    => se agrega archivo globales de aclaratorio y se sustitu- #
#                   yen las variables correspondientes; hilda rivas         #
#############################################################################
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, # C�digo del proceso
       g_opera_cod_integra     LIKE cat_operacion.opera_cod, # C�digo de operaci�n
       g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  # C�digo de operaci�n
       
END GLOBALS

#Objetivo:
MAIN
DEFINE p_opera_cod_carga LIKE cat_operacion.opera_cod, # C�digo de operacion
       p_usuario_cod     LIKE seg_usuario.usuario_cod, # Clave de usuario
       p_nom_archivo     STRING, 
       --v_cadena_pid      VARCHAR(5),
       --v_cadena_proc     VARCHAR(5),
       --v_cadena_opera    VARCHAR(5),
       --v_comando         STRING,
       --r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       --r_ruta_listados   LIKE seg_modulo.ruta_listados,
       r_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT,
       --r_ruta_vacia      LIKE seg_modulo.ruta_bin,
       r_bnd_estado      SMALLINT,
       p_titulo              STRING, -- titulo del mensaje enviado en el correo
       p_mensaje             STRING -- cuerpo del mensaje enviado

   --WHENEVER ERROR CONTINUE
   LET r_bnd_estado = 0 # Inicializa bandera
   
   #Si se ha recibido par�metros se continua    
   #Primer par�metro
   LET p_usuario_cod = ARG_VAL(1)
   #Segundo par�metro
   LET g_pid         = ARG_VAL(2)
   #Tercer par�metro
   LET g_proceso_cod = ARG_VAL(3)
   #Cuarto par�metro
   LET g_opera_cod_integra = ARG_VAL(4) # Paso de informaci�n a las tablas hist�ricas
   #Quinto par�metro
   LET r_folio       = ARG_VAL(5)
   #Segundo par�metro
   LET p_nom_archivo = ARG_VAL(6)
   # Indicamos operacion de carga
   LET p_opera_cod_carga = g_opera_cod_carga

   # Archivo de bitacora
   CALL STARTLOG(p_usuario_cod CLIPPED||".ACLP05.log")
   
   CALL fn_display_proceso(0," Integraci�n enaclara")
   
   # Genera folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod_integra, p_usuario_cod)
                        RETURNING r_folio

   # Recupera el nombre del archivo cargado
   CALL fn_recupera_arch_cargado(g_proceso_cod,p_opera_cod_carga)
                     RETURNING p_nom_archivo

{-- se cambia esta instruccion al programa lanzador 25 Mayo 2012
   # Se registra el inicio de la operacion
   CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod_integracion,r_folio,
                               "ACLP05",p_nom_archivo,p_usuario_cod) 
                     RETURNING r_resultado_opera
}
   
   # Llamada a ejecuci�n de procedimiento almacenado
   CALL fn_guarda_historicos_enaclara(r_folio) RETURNING r_resultado_opera
      
   # cambia a estado errone si no se ejecut� correctamente el SP
   IF ( r_resultado_opera ) THEN         
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_integra) 
               RETURNING r_resultado_opera
      CALL fn_act_edo_archivo(p_nom_archivo,r_folio,3,p_usuario_cod)
                            RETURNING r_resultado_opera

      LET p_mensaje = "El proceso de Integraci�n ha finalizado pero con errores.\nNo se puede continuar con el proceso de Preliquidaci�n."              
   ELSE
      # Actualiza el estado del archivo en glo_ctr_archivo a integrado
      # 2 = integrado
      CALL fn_act_edo_archivo(p_nom_archivo,r_folio,2,p_usuario_cod)
                            RETURNING r_resultado_opera

      LET p_mensaje = "Integraci�n realizada con �xito.\nYa se puede continuar con la Preliquidaci�n."
      # Se finaliza la carga de registros historicos
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod_integra) 
                          RETURNING r_resultado_opera
 
   END IF 

   -- se muestra la finalizacion del proceso
   CALL fn_display_proceso(1," Integraci�n enaclara ")
         
   LET p_titulo = "Finalizaci�n de operaci�n - Aclaraciones Enaclara - Integraci�n"

   CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                          g_opera_cod_integra, 
                          NULL, p_titulo,p_mensaje)
END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidaci�n
{ ==========================================================================
Clave:  fn_guarda_historicos_enaclara
Nombre: fn_guarda_historicos_enaclara
Fecha creacion: 10 de Enero de 2012
Autor: Hugo C�sar Ram�rez Garc�a
Narrativa del proceso que realiza:
 Esta funci�n ejecuta el store procedure que almacena la informaci�n 
 de los registros hist�ricos para el m�dulo de "Aclaracion Sin cambio"
 Parametros de Entrada:
 -
 Par�metros de salida:
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_guarda_historicos_enaclara(p_folio)
DEFINE p_folio          LIKE glo_folio.folio,
       v_sql_procedure  STRING,
       v_cod_error      SMALLINT,
       v_error_isam     INTEGER,
       v_mensaje_error  VARCHAR(255),
       v_det_acl_nss    CHAR(11)

   --WHENEVER ERROR CONTINUE
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_historicos_enaclara(?,?,?)"
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_historicoAclaracionSinCambio FROM v_sql_procedure
   EXECUTE prp_historicoAclaracionSinCambio USING p_folio,g_pid,g_proceso_cod 
   INTO    v_cod_error, v_error_isam, v_mensaje_error, v_det_acl_nss

      
   IF ( v_cod_error = 0 ) THEN
      DISPLAY v_mensaje_error
      DISPLAY " v_det_acl_nss: ",v_det_acl_nss
      RETURN FALSE
   ELSE
      DISPLAY "\nError ejecucion sp_historicos_enaclara (Codigo): ",v_cod_error
      DISPLAY "\nError ejecucion sp_historicos_enaclara (Mensaje): ",v_mensaje_error
      DISPLAY "Error en sp_historicos_con_cambio (Mensaje): ", v_mensaje_error,"\n"
      DISPLAY "NSS en curso: ",v_det_acl_nss
      RETURN TRUE
   END IF
   
END FUNCTION