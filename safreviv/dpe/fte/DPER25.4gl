--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/01/2012
--===============================================================

################################################################################
#Modulo      => DPE                                                            #
#Programa    => DPEL41                                                         #
#Objetivo    => Lanzado que ejecuta el reverso de la integración del archivo   #
#            => que PROCESAR envia como acuse después de que recibio el archivo#
#            => que se genero en SACI                                          #      
#Fecha inicio=> Noviembre 5, 2012                                              #
################################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera        SMALLINT
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado

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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER25.log")

   LET g_proceso_cod = g_proceso_cod_dpe_procesar_gen -- PROCESAR
   LET g_opera_cod   = 4  -- INTEGRACION

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

       -- Restaura datos afectados por integración
   CALL fn_dpe_corrige_reg_integracion(p_d_folio)
   -- Prepara el la cadena para ejecutar el procedimiento de reverso desmarca
    
      DISPLAY "Operación lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - REVERSO INTEGRACIÓN ACUSE PROCESAR"
   
   LET p_mensaje = "Finalización de proceso - REVERSO INTEGRACIÓN ACUSE PROCESAR","\n",
                  "#\n",
                  "# Folio: "||p_d_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
END MAIN

{
   Funcion : fn_dpe_corrige_reg_integracion
   Fecha   : Marzo 21, 2012
   Descrip : corrige datos adicionales de reverso de integracion
   Aturo   : Felipe Nava
}
FUNCTION fn_dpe_corrige_reg_integracion(p_d_folio)
  DEFINE 
   p_d_folio   LIKE dis_preliquida.folio_liquida,
   bnd_reverso SMALLINT
   --Actualiza archivo a cargadodpe_resp_procesar
   UPDATE glo_ctr_archivo
      SET estado = 1,
          folio = NULL, -- cargado
          opera_cod = 1
    WHERE proceso_cod = g_proceso_cod
      AND folio = p_d_folio
      AND estado = 2 -- integrado
   --Elimina folio    
    DELETE FROM glo_folio
     WHERE folio = p_d_folio
       AND proceso_cod = g_proceso_cod

   --Actualiza Integrar PROCESAR a GENERAR ARCHIVO PROCESAR
   UPDATE dpe_sol_trab_parcial
      SET diagnostico = 100
    WHERE folio_liquida = p_d_folio
      AND diagnostico = 12

   UPDATE dpe_sol_trab_parcial
      SET diagnostico = 13
    WHERE folio_liquida = p_d_folio
      AND diagnostico = 11
    
   --Elimina de la tabla de respuesta    
   DELETE FROM dpe_resp_procesar
     WHERE folio = p_d_folio

    UPDATE bat_ctr_operacion 
    SET    folio = NULL 
    WHERE  folio = p_d_folio
    AND    opera_cod = 4
      
   -- ]

   LET bnd_reverso = 1

   DISPLAY bnd_reverso
   
END FUNCTION -- fn_dpe_corrige_reg_integracion
