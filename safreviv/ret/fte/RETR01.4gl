--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR01                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la reverso        #
#                preliquidacion para retiro solo infonavit                              #               #
#Fecha inicio => Febrero 21, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     -- ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
END GLOBALS

MAIN
DEFINE  p_s_titulo       STRING   -- titulo de la ventana
  --     ,v_folio          LIKE ret_preliquida.folio_liquida
       ,v_b_rev_pre     SMALLINT
       ,p_pid            LIKE bat_ctr_operacion.pid         -- PID del proceso
       ,p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod      LIKE bat_ctr_operacion.opera_cod   -- codigo de la operacion
       ,p_usuario_cod    LIKE seg_usuario.usuario_cod       -- clave del usuario firmado
       ,p_folio          LIKE ret_preliquida.folio_liquida
       ,p_doc_cod       varchar(20) 


   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_doc_cod       = ARG_VAL(6) 

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
  

         CALL fn_reverso_ret_viv97_preliquidacion(p_folio, p_proceso_cod, p_opera_cod,p_usuario_cod) RETURNING v_b_rev_pre
         IF v_b_rev_pre = 0 THEN
            CALL fn_reversa_preliquidacion(p_folio,p_proceso_cod,p_opera_cod) RETURNING v_b_rev_pre
        END IF 
         
END MAIN

{
======================================================================
 Clave:  
Nombre: fn_reverso_ret_viv97_preliquidacion
Fecha creacion: Febrero 21, 2012
Autor: Erick Rodriguez, EFP
Narrativa del proceso que realiza:
Ejecuta Reverso de preliquidacion de Retiro solo infonavit
para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_reverso_ret_viv97_preliquidacion(p_folio, p_proceso_cod, p_opera_cod,p_usuario_cod)
DEFINE p_folio       DECIMAL(9,0),
       p_proceso_cod SMALLINT,
       p_opera_cod   SMALLINT,
       r_sql_reverso SMALLINT,
       p_usuario_cod  char(20)

       
   LET r_sql_reverso = 0
   # Ejecuta el fn que realiza el reverso
   PREPARE prp_reversa_preli FROM "EXECUTE FUNCTION fn_reverso_ret_viv97_preli( ?, ?, ? ,?)"
   EXECUTE prp_reversa_preli USING p_folio, p_proceso_cod, p_opera_cod ,p_usuario_cod
                                       INTO r_sql_reverso 
   RETURN r_sql_reverso
END FUNCTION 