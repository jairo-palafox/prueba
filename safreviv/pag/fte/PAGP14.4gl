--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#############################################################################
#Módulo          => PAG                                                     #
#Programa        => PAGP14.4GL                                              #
#Objetivo        => Programa de registro de información histórica para el   #
#                   módulo de  "Solo infonavit"                             #
#Fecha Inicio    => 11 ENERO 2012                                           #
#############################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   --g_opera_cod_carga           LIKE cat_operacion.opera_cod, -- codigo de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   g_opera_cod_integracion     LIKE cat_operacion.opera_cod, -- codigo de operacion   
   g_num_folio                 DECIMAL(9)
END GLOBALS

#Objetivo:
MAIN
DEFINE
   v_bnd_fin_proceso   SMALLINT,
   v_nom_archivo       STRING,
   v_estatus           SMALLINT
  ,p_titulo            STRING -- titulo del mensaje enviado en el correo
  ,p_mensaje           STRING -- cuerpo del mensaje enviado
  
   #Si se ha recibido parámetros se continua    
      #Primer parámetro
      LET p_usuario_cod = ARG_VAL(1)
      #Segundo parámetro
      LET g_pid         = ARG_VAL(2)
      #Tercer parámetro
      LET g_proceso_cod = ARG_VAL(3)
      #Cuarto parámetro
      LET g_opera_cod_integracion = ARG_VAL(4)
      #Quinto parámetro
      LET g_num_folio = ARG_VAL(5)
      #Segundo parámetro
      LET v_nom_archivo = ARG_VAL(6)
      --Indica que tabla se va a modificar 
      --LET v_tabla = ARG_VAL(7)

      --Inicializacion de variables
      LET v_bnd_fin_proceso = 0

         --Obtenemos el numero de Folio
         LET g_num_folio = fn_genera_folio(g_proceso_cod
                                          ,g_opera_cod_integracion
                                          ,p_usuario_cod)
         --DISPLAY "Número de Folio:",g_num_folio
         
         
         --Recuperamos el nombre del archivo cargado
         --LET v_nom_archivo = fn_recupera_arch_cargado(g_proceso_cod,g_opera_cod_carga)
          -- se lleva esta funcion al lanzador 25 Mayo 2012         
         --Se registra el inicio de la operacion
         --Se inicia la operación en el monitor
         {CALL fn_actualiza_opera_ini(g_pid
                                    ,g_proceso_cod
                                    ,g_opera_cod_integracion
                                    ,g_num_folio
                                    ,"PAGP14"
                                    ,v_nom_archivo
                                    ,p_usuario_cod
                                    ) RETURNING v_estatus
         }
                                   
         #Llamada a ejecución de procedimiento almacenado
         CALL fn_guarda_historicos_soloInfonavit(g_num_folio) RETURNING v_estatus
         
         IF v_estatus = 0 THEN
         	
            --Se actualiza el archivo como integrado
            CALL fn_act_edo_archivo(v_nom_archivo, g_num_folio, 2, p_usuario_cod ) RETURNING v_estatus
            -- se invoca la finalizacion de la operacion
            --CALL fn_finaliza_operacion(g_pid,    --- Identificador del proceso
            --                           g_proceso_cod, --- Clave del proceso
            --                           g_opera_cod_integracion) --- Clave de la operación

            CALL fn_actualiza_opera_fin(g_pid
                                       ,g_proceso_cod
                                       ,g_opera_cod_integracion)
                                       RETURNING v_estatus
            LET p_mensaje = "Integración realizada con éxito.\nYa se puede continuar con la Preliquidación."
         ELSE
            LET p_mensaje = "El proceso de Integración ha finalizado pero con errores.\nNo se puede continuar con el proceso de Preliquidación."
            --Si ocurrio un error se actualiza el estatus como erroneo
            CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_integracion)  RETURNING v_estatus
         END IF

         CALL fn_display_proceso(0,"INTEGRACIÓN")

         LET p_titulo = "Finalización de operación - SOLO-INFONAVIT- Integración"
         CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                             g_opera_cod_integracion, 
                             NULL, p_titulo,p_mensaje)

END MAIN

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidación
{ ==========================================================================
Clave:  fn_guarda_historicos_soloInfonavit
Nombre: fn_guarda_historicos_soloInfonavit
Fecha creacion: 10 de Enero de 2012
Autor: David Miguel Garibay Rivera
Narrativa del proceso que realiza:
Esta función ejecuta el store procedure que almacena la información 
de los registros históricos para el módulo de "Registro de pagos"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_guarda_historicos_soloInfonavit(p_folio)
DEFINE p_folio          DECIMAL(9),
       v_sql_procedure  STRING,
       v_estatus        SMALLINT,
       v_cod_error      SMALLINT,
       v_error_isam     SMALLINT,
       v_mensaje_error  VARCHAR(255),
       v_pag_det_nss    CHAR(11)
       
   --WHENEVER SQLERROR CONTINUE    

   LET v_estatus = 0 --El cero indica que se jecuto con exito

   LET v_sql_procedure = "EXECUTE PROCEDURE sp_registro_historicos_sinf(?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_sp_registro_historicos_soloinfonavit FROM v_sql_procedure
   EXECUTE prp_sp_registro_historicos_soloinfonavit USING p_folio ,g_pid,g_proceso_cod
   INTO v_cod_error, v_error_isam, v_mensaje_error, v_pag_det_nss

   IF (v_cod_error = 0) THEN
      # Ejecucion sin error
      DISPLAY v_mensaje_error
      DISPLAY v_cod_error    
      --DISPLAY "@v_pag_det_nss ", v_pag_det_nss
      RETURN FALSE

   ELSE 
      LET v_estatus = 1  --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "\nError de ejecución en 'sp_registro_historicos_sinf' (Código): ",v_cod_error
      DISPLAY "Error en 'sp_registro_historicos_sinf' (Mensaje):",v_mensaje_error,"\n"
      DISPLAY "Error en 'sp_registro_historicos_sinf' (ISAM):",v_error_isam,"\n"
      DISPLAY "NSS en curso: ", v_pag_det_nss
       RETURN TRUE
   END IF

   --WHENEVER SQLERROR STOP
   --RETURN v_estatus

END FUNCTION