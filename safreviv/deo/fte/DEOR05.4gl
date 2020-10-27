--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOR05                                                                 #
#Objetivo     => Programa que ejecuta la rutina de reverso de Integración de archivo    #
#                para la devolucion por errores de operacion                            #
#Fecha inicio => Febrero 21, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_operacion      SMALLINT
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera        SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DEOL05.log")

   -- Restaura datos afectados por integración
   CALL fn_corrige_reg_integracion(p_d_folio)
   -- Se invoca rutina para reversar la integración.
   CALL fn_reversa_integracion(p_d_folio, g_proceso_cod)
      RETURNING r_bandera
   IF(r_bandera = 0)THEN
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
   IF(r_bandera = 0)THEN

      UPDATE glo_ctr_archivo
         SET estado = 1, folio = NULL -- cargado
       WHERE folio = p_d_folio
         AND estado = 2; -- integrado
      
      DISPLAY "Operación lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo

   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

END MAIN

{
   Funcion : fn_corrige_reg_integracion
   Fecha   : febrero 21, 2012
   Descrip : corrige datos adicionales de reverso de integracion
   Aturo   : Felipe Nava
}
FUNCTION fn_corrige_reg_integracion(p_d_folio)
  DEFINE 
   p_d_folio                 LIKE dis_preliquida.folio_liquida
   --
   ,v_cve_afore_archivo      SMALLINT
   ,v_dte_f_valor_devolucion DATE
   ,v_s_qry                  STRING 
    
  LET v_s_qry = 
      "SELECT DISTINCT cve_afore, f_valor_devol_inf"
      ,"\n  FROM safre_viv:deo_det_op98 "
      ,"\n WHERE folio = ?"
  PREPARE Prpr_ObtAforeFechaDevol FROM v_s_qry CLIPPED
  DECLARE Curr_ObtAforeFechaDevol CURSOR FOR Prpr_ObtAforeFechaDevol
  
  FOREACH Curr_ObtAforeFechaDevol USING p_d_folio
     INTO v_cve_afore_archivo, v_dte_f_valor_devolucion
     -- Libera registros de saldos para poder ser utilizados
     
     UPDATE deo_mto_deposito
        SET estado_devolucion = 1 -- pendiente procesar
      WHERE cve_afore = v_cve_afore_archivo
        AND f_valor_devol_inf = v_dte_f_valor_devolucion
        AND estado_devolucion = 2; -- procesar     
     
  END FOREACH 

   UPDATE glo_ctr_archivo
      SET estado = 1, folio = NULL -- cargado
    WHERE proceso_cod = g_proceso_cod
      AND folio = p_d_folio
      AND estado = 2 -- integrado

   -- ]

END FUNCTION -- fn_corrige_reg_integracion
