--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
##########################################################################################
#Modulo       => RET                                                                     #
#Programa     => RETR306                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza el reverso         #
#                preliquidacion de restitucion de rechazo de retiros SIAFF, BANCO y FICO #
#                ley73                                                                   #
#Fecha inicio => Noviembre 11, 2020                                                      #
##########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

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
DEFINE  p_s_titulo      STRING   -- titulo de la ventana
       ,v_b_rev_pre     SMALLINT
       ,p_pid           LIKE bat_ctr_operacion.pid         -- PID del proceso
       ,p_proceso_cod   LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod     LIKE bat_ctr_operacion.opera_cod   -- codigo de la operacion
       ,p_usuario_cod   LIKE seg_usuario.usuario_cod       -- clave del usuario firmado
       ,p_folio         LIKE ret_preliquida.folio_liquida
       ,p_doc_cod       VARCHAR(20)
       ,p_cod_rechazo   SMALLINT
       ,lr_cat_proceso  RECORD LIKE cat_proceso.*


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

   -- selecciona el tipo de codigo de rechazo
   CASE p_proceso_cod

     -- banco
   	 WHEN g_proceso_cod_restitucion_ret_generico_ley73 
   	 	LET p_cod_rechazo = 65
   		
   	 WHEN g_proceso_cod_restitucion_ret_generico_ley73fico
   		LET p_cod_rechazo = 64

     WHEN g_proceso_restitucion_rechazo_siaff
        LET p_cod_rechazo = 66
   		
   END CASE

   -- datos del proceso de reverso para log
   DISPLAY "Inicia proceso de reverso de preliquidacion"
   SELECT *
   INTO   lr_cat_proceso.*
   FROM   cat_proceso
   WHERE  proceso_cod = p_proceso_cod

   DISPLAY lr_cat_proceso.proceso_desc

   DISPLAY "\n\nEjecutando rutina de reverso..."
   
   CALL fn_reverso_preliquidacion_restitucion_rch_siaff_banco_fico_ley73(p_folio, p_proceso_cod, p_opera_cod,p_usuario_cod, p_cod_rechazo, p_pid) RETURNING v_b_rev_pre

   DISPLAY "\n\nProceso de reverso de preliquidación finalizado"
   
   -- si el reverso ejecuto correctamente
   IF ( v_b_rev_pre = 0 ) THEN
      -- se reversa la operacion
      CALL fn_reversa_preliquidacion(p_folio,p_proceso_cod,p_opera_cod) RETURNING v_b_rev_pre
   END IF 
         
END MAIN

{
======================================================================
 Clave:  
Nombre: fn_reverso_preliquidacion_restitucion_rch_siaff_banco_fico_ley73
Fecha creacion: Noviembre 11, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Ejecuta el SP que tiene las reglas de negocio para realizar el reverso de la preliquidacion
de restitucion de rechazo de siaff, banco y fico de retiro ley73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_reverso_preliquidacion_restitucion_rch_siaff_banco_fico_ley73(p_folio, p_proceso_cod, p_opera_cod,p_usuario_cod, p_cod_rechazo, p_pid)
DEFINE p_folio        DECIMAL(9,0),
       p_proceso_cod  SMALLINT,
       p_opera_cod    SMALLINT,
       r_sql_reverso  SMALLINT,
       p_usuario_cod  CHAR(20),
       p_cod_rechazo  SMALLINT, -- codigo de rechazo que tienen los registros segun el proceso que los rechazo
       p_pid          LIKE bat_ctr_proceso.pid
DEFINE v_estado_solicitud SMALLINT
-- variables para el control de excepciones
DEFINE v_estatus    SMALLINT
DEFINE v_sql_error  INTEGER
DEFINE v_isam_error INTEGER
DEFINE v_msg_error  CHAR(254)

   -- se asigna el estado en el que las solicitudes se encuentran antes de la restitucion
   LET v_estado_solicitud = gi_estado_restitucion_rechazo

   # Ejecuta el fn que realiza el reverso
   PREPARE prp_reversa_preli FROM "EXECUTE FUNCTION fn_rev_preliquida_rest_siaffbancofico_l73( ?, ?, ? ,?, ?, ?)"
   EXECUTE prp_reversa_preli USING p_folio, p_proceso_cod, p_opera_cod ,p_usuario_cod, p_pid, v_estado_solicitud
                                       INTO v_estatus, v_sql_error, v_isam_error, v_msg_error

                                       
   RETURN v_estatus
END FUNCTION 