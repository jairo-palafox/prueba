--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:Octubre 05, 2012
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR162                                                                #
#Objetivo     => Programa que realiza el reverso de liquidación de contingente solo     #
#                infonavit                                                              #
#Fecha inicio => Octubre 05, 2012                                                         #
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
       ,p_i_folio            LIKE glo_folio.folio
       ,p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera            SMALLINT
       ,v_s_sql              STRING -- cadena con un enunciado SQL
       ,v_error_isam         INTEGER
       ,v_mensaje            VARCHAR(255) 
       
       -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)    
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR162.log")

   -- Reversa operación
   LET r_bandera = 0

   -- reverso del registro contable
   CALL fn_reverso_reg_cnt(p_i_folio) RETURNING   r_bandera          

   DISPLAY "Ejecutando rutina de reverso de liquidación de Retiro Solo Infonavit Contingente..."
      
   -- se ejecuta el SP de reverso
   LET v_s_sql = " EXECUTE PROCEDURE sp_ret_contingente_sinf_rev_liquidacion(?,?,?)"

   PREPARE sid_reverso FROM v_s_sql
   EXECUTE sid_reverso INTO r_bandera, v_error_isam, v_mensaje
      USING p_i_folio, g_proceso_cod, p_usuario_cod

   DISPLAY "Resultado: ", r_bandera
   DISPLAY "Mensaje: ", v_mensaje
      
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bandera

   IF ( r_bandera = 0 ) THEN
      -- se termino el reverso
      DISPLAY "El reverso se realizó con éxito"

   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
 
END MAIN