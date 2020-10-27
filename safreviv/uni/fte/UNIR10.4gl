################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR10                                                        #
#Objetivo     => Programa que ejecuta el reverso de la Integración de archivo  #
#                para la unificación de cuentas                                #
#Fecha inicio => Junio 05, 2012                                                #
################################################################################

--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/06/2012
--===============================================================

GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_operacion      SMALLINT
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera        SMALLINT
       ,v_cadena         STRING
       ,v_r_reverso_id_derechohabiente DECIMAL(9,0)
       ,v_r_reverso_id_referencia      DECIMAL(9,0)
       ,v_r_reverso_marca              SMALLINT
       ,v_r_reverso_folio              DECIMAL(9,0)
       ,bn_reverso_desmarca            SMALLINT
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado
    DEFINE v_si_resultado  SMALLINT 
    DEFINE isam_err INTEGER
    DEFINE err_txt  CHAR(200),
           v_s_sql  STRING,
           v_folio  LIKE dis_preliquida.folio_liquida,
           v_id_derechohabiente DECIMAL(9,0)

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR10.log")

   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0
   
   -- Restaura datos afectados por integración
   CALL fn_dpe_corrige_reg_integracion(p_d_folio)

   -- Se invoca rutina para reversar la integración.
   CALL fn_reversa_integracion(p_d_folio, g_proceso_cod)
      RETURNING r_bandera
   IF(r_bandera = 0)THEN
      DISPLAY "El reverso se realizó con éxito"
      
      
      DELETE FROM uni_det_unificador
      WHERE folio_unificacion = p_d_folio
      
      DELETE FROM uni_det_unificado
      WHERE folio_unificacion = p_d_folio
      
      DELETE FROM glo_folio
      WHERE proceso_cod = g_proceso_cod
      AND folio = p_d_folio
      
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
   IF(r_bandera = 0)THEN   
      DISPLAY "Operación lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo

   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - REVERSO INTEGRACIÓN"
   
   LET p_mensaje = "Finalización de proceso - REVERSO INTEGRACIÓN","\n",
                  "#\n",
                  "# Folio: "||p_d_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"
END MAIN

{
   Funcion : fn_dpe_corrige_reg_integracion
   Fecha   : Marzo 21, 2012
   Descrip : corrige datos adicionales de reverso de integracion
   Aturo   : Felipe Nava
}
FUNCTION fn_dpe_corrige_reg_integracion(p_d_folio)
  DEFINE 
   p_d_folio LIKE dis_preliquida.folio_liquida
  
   UPDATE glo_ctr_archivo
      SET estado = 1, 
          folio = NULL, -- cargado
          opera_cod = 1
    WHERE proceso_cod = g_proceso_cod
      AND folio = p_d_folio
      AND estado = 2 -- integrado

END FUNCTION -- fn_dpe_corrige_reg_integracion
