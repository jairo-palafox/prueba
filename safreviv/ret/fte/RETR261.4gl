--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR22                                                                 #
#Objetivo     => Programa que ejecuta el rutna de reverso de liquidaci�n                #
#                de retiros de amortizaciones excedente
#Fecha inicio => Febrero 28, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_i_folio            LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera            SMALLINT
       ,v_s_sql              STRING, -- cadena con un enunciado SQL
       v_marca_amort_excedente   SMALLINT,
       v_edo_marca          SMALLINT,
       v_caus_marac          SMALLINT,
       v_cod_rechazo         SMALLINT 

       -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)    
   
   --se le asigna la marca de retiro 
   LET v_marca_amort_excedente = g_marca_ret_amort_excedente      
   LET v_edo_marca  = 0
   LET v_cod_rechazo = 0
   LET v_caus_marac  = 0


   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR261.log")

   -- Reversa operaci�n
   LET r_bandera = 0

   LET v_s_sql = " EXECUTE PROCEDURE sp_ret_rev_liquida_amort_excedente(?,?,?)"
   PREPARE prp_exec_sp_estatus FROM v_s_sql
   EXECUTE prp_exec_sp_estatus USING p_i_folio,g_proceso_cod,v_marca_amort_excedente


   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bandera

   -- se termino el reverso
   DISPLAY "El reverso se realiz� con �xito"

 
END MAIN