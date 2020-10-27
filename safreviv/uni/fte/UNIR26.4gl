--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/06/2012
--===============================================================

#########################################################################################
#Modulo       => UNI                                                                    #
#Programa     => UNIR10-1                                                               #
#Objetivo     => Programa que ejecuta la rutina de reverso de Integración de archivo    #
#                confrontado para la unificación de cuentas                             #
#Fecha inicio => Junio 05, 2012                                                         #
#########################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera            SMALLINT
       ,bn_reverso_confronta SMALLINT
       ,p_titulo             STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje            STRING -- cuerpo del mensaje enviado

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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR26.log")

   LET g_proceso_cod = g_proceso_cod_uni_IMSS -- devolucion de operaciones
   LET g_opera_cod   = g_opera_cod_uni_integracion_confronta  -- integración confrotado
   
   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_confronta = 0

   -- Restaura datos afectados por integración
   CALL fn_dpe_corrige_reg_integracion(p_d_folio, p_nombre_archivo)
   -- Prepara el la cadena para ejecutar el procedimiento de reverso desmarca
   DISPLAY "Folio confrontación: ", p_d_folio

   -- Reversa operación  
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
        RETURNING r_bandera

   IF(r_bandera = 0)THEN
      UPDATE glo_folio
         SET opera_cod = 2
       WHERE folio = p_d_folio
         AND opera_cod = 4
         AND proceso_cod = g_proceso_cod
    
     UPDATE glo_ctr_archivo
         SET opera_cod = 3,
             estado = 1,
             folio = NULL
       WHERE folio = p_d_folio
         AND opera_cod = 4
         AND proceso_cod = g_proceso_cod
      
      DISPLAY "Operación lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF   
   
    -- Recupera los registro que estan confrontados
    -- Solo unificador IMSS
   DECLARE Curr_confronta CURSOR FOR
    SELECT *
      FROM uni_det_unificado
     WHERE folio_unificacion = p_d_folio
   
   -- Por cada registro confrontado actualiza su estado
   FOREACH Curr_confronta
      WHENEVER ERROR CONTINUE
      --Se actualizan indicadores en UNIFICADO 
      UPDATE uni_det_unificado
         SET estado_unificacion = 2,
             diagnostico_uni = "00" 
       WHERE folio_unificacion = p_d_folio           
         AND diagnostico = 17 
         AND diagnostico_uni = "02"
         AND estado_unificacion = 2
         
      UPDATE uni_det_unificado
         SET estado_unificacion = 1,
             diagnostico_uni = "00" 
       WHERE folio_unificacion = p_d_folio           
         AND diagnostico = 1 
         AND diagnostico_uni = "02"
         AND estado_unificacion = 2

      UPDATE uni_det_unificado
         SET estado_unificacion = 2,
             diagnostico_uni = "00" 
       WHERE folio_unificacion = p_d_folio           
         AND diagnostico = 17 
         AND diagnostico_uni IN ("04","05")
         AND estado_unificacion = 3
         
      UPDATE uni_det_unificado
         SET estado_unificacion = 1,
             diagnostico_uni = "00" 
       WHERE folio_unificacion = p_d_folio           
         AND diagnostico = 1 
         AND diagnostico_uni IN ("04","05")
         AND estado_unificacion = 3
                  
      UPDATE uni_det_unificado
         SET diagnostico_uni = "00"
       WHERE folio_unificacion = p_d_folio
         AND diagnostico = 17 
         AND diagnostico_uni = "01"
         AND estado_unificacion = 1
         
      UPDATE uni_det_unificado
         SET diagnostico_uni = "00",
             diagnostico = 1
       WHERE folio_unificacion = p_d_folio
         AND diagnostico = 30 
         AND diagnostico_uni = "01"
         AND estado_unificacion = 1

      UPDATE uni_det_unificado
         SET diagnostico_uni = "00"
       WHERE folio_unificacion = p_d_folio
         AND diagnostico_uni = "01"
         AND estado_unificacion = 1
         
      --Se actualizan indicadores en UNIFICADOR
      UPDATE uni_det_unificador
         SET estado_unificacion = 2,
             estado_familia = 2 
       WHERE folio_unificacion = p_d_folio           
         AND diagnostico <> 1
         AND estado_unificacion IN (2,3)
        
      UPDATE uni_det_unificador
         SET estado_unificacion = 1,
             estado_familia = 1 
       WHERE folio_unificacion = p_d_folio           
         AND diagnostico = 1
         AND estado_unificacion IN (2,3)
        
      UPDATE uni_det_unificador
         SET diagnostico = 1 
       WHERE folio_unificacion = p_d_folio           
         AND diagnostico = 30
         AND estado_unificacion = 1

   --   WHENEVER ERROR STOP
   
   END FOREACH
    
   IF SQLCA.SQLCODE  < 0 THEN
       DISPLAY "La confrontación no puedo concretarse: ", SQLCA.SQLCODE
   END IF 
   
   
   LET p_titulo = "Finalización de proceso - REVERSO INTEGRACIÓN CONFRONTADO"
   
   LET p_mensaje = "Finalización de proceso - REVERSO INTEGRACIÓN CONFRONTADO","\n",
                  "#\n",
                  "# Folio confrontación: "||p_d_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"

   DISPLAY p_mensaje
   --CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
   --                       "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
   --                       p_titulo,
   --                       p_mensaje)

END MAIN

{
   Funcion : fn_dpe_corrige_reg_integracion
   Fecha   : Marzo 21, 2012
   Descrip : corrige datos adicionales de reverso de integracion
   Aturo   : Felipe Nava
}
FUNCTION fn_dpe_corrige_reg_integracion(p_d_folio, p_nombre_archivo)
  DEFINE 
   p_d_folio        LIKE dis_preliquida.folio_liquida,
   p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
   v_cadena         STRING
   --
   LET p_nombre_archivo = p_nombre_archivo CLIPPED
   
   LET v_cadena = "UPDATE glo_ctr_archivo",
                  "\n SET estado = 1,", 
                  "\n folio = NULL,", -- cargado
                  "\n opera_cod = 3",
                  "\n WHERE proceso_cod = ",g_proceso_cod_uni_IMSS,
                  "\n AND folio = ",p_d_folio,
                  "\n AND estado = ",2, -- integrado
                  "\n AND nombre_archivo = '",p_nombre_archivo,"'"

      PREPARE Prpr_Actu_archivo FROM v_cadena CLIPPED
      EXECUTE Prpr_Actu_archivo

END FUNCTION -- fn_dpe_corrige_reg_integracion
