--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR182                                                                 #
#Objetivo     => Programa que ejecuta el rutna de reverso de liquidación                #
#                de retiros por disposicion de recursos PMG
#Fecha inicio => Erick Rodriguez, 2012                                                       #
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
       v_marca_pmg           SMALLINT,
       v_edo_maraca          SMALLINT,
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
   LET v_marca_pmg = 808      
   LET v_edo_maraca  = 0
   LET v_cod_rechazo = 0
   LET v_caus_marac  = 0

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR182.log")

   -- Reversa operación
   LET r_bandera = 0
   
   -- Ejecuta el SP que realiza el reverso contable
   CALL fn_reverso_reg_cnt(p_i_folio) RETURNING r_bandera

   -- si el reverso contable se ejecuto correctamente
   IF ( r_bandera = 0 ) THEN

      LET v_s_sql = " EXECUTE PROCEDURE sp_ret_rev_liquida_disposicion(?,?,?)"
      PREPARE prp_exec_sp_estatus FROM v_s_sql
      EXECUTE prp_exec_sp_estatus USING p_i_folio,g_proceso_cod,v_marca_pmg
      
      
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bandera
      
      -- se termino el reverso
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      DISPLAY "============================================================"
      DISPLAY " REVERSO DEL REGISTRO DE LA PÓLIZA CONTABLE                 "
      DISPLAY "____________________________________________________________"
      DISPLAY "\nNo se pudo realiza el reverso del registro de la póliza contable debido a que"
 
      -- se indica en consola que fue lo que paso
      CASE r_bandera
         WHEN 1 -- No existe poliza contable
            DISPLAY "la póliza contable no existe"
         
         WHEN 2 -- fecha de emision distinta a la actual
            DISPLAY "la fecha de emisión de la misma no corresponde a la fecha actual"
            DISPLAY ""
            DISPLAY "Debe ejecutar el proceso de Reversos Operativos."
        
         WHEN 3 -- la poliza ya se genero
            DISPLAY "la póliza ya fue emitida y contabilizada"       
          WHEN 4 -- el periodo contable ya fue cerrado
            DISPLAY "periodo contable cerrado"       
          WHEN 5 -- la póliza contable no ha sido confirmada
            DISPLAY "la póliza no ha sido contabilizada"
      END CASE
      
      DISPLAY "Para el proceso identificado por el folio: ", p_i_folio  
   END IF
 
END MAIN