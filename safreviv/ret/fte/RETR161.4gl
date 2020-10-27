--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:Octubre 10, 2012
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR161                                                                #
#Objetivo     => Programa que realiza reverso preliquida contingente solo infonavit     #
#Fecha inicio => Octubre 05, 2012                                                       #
#########################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera        SMALLINT
       ,v_sql            STRING
       ,v_error_isam         INTEGER
       ,v_mensaje            VARCHAR(255)
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR161.log")

   DISPLAY "Ejecutando rutina de reverso de preliquidación de Retiro Solo Infonavit Contingente..."

   -- Reversa operación
   LET r_bandera = 0

   LET v_sql = " EXECUTE PROCEDURE sp_ret_contingente_sinf_rev_preliquidacion(?,?)"
   PREPARE prp_exec_rev_prel_tran FROM v_sql
   EXECUTE prp_exec_rev_prel_tran INTO r_bandera, v_error_isam, v_mensaje
           USING p_d_folio, g_proceso_cod

   DISPLAY "Resultado: ", r_bandera
   DISPLAY "Mensaje: ", v_mensaje  

   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bandera

   DISPLAY "El reverso se realizó con éxito."
      
END MAIN