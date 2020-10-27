################################################################################
#Proyecto         => SAFRE VIVIENDA                                            #
#Propietario      => E.F.P.                                                    #
#Modulo           => DAE                                                       #
#Programa         => DAER12                                                    #
#Objetivo         => Lanzado del reverso de Integración Ajuste Ind DAE         #
#Fecha inicio     => 18/Abr/2016                                               #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

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
       ,bn_reverso_desmarca            SMALLINT
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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DAER12.log")

   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0
    
    DISPLAY "Folio del lote: ", p_d_folio
    DISPLAY "REVERSO DE LA INTEGRACIÓN"
      
   -- Se invoca rutina para reversar la integración.
   CALL fn_reversa_integracion(p_d_folio, g_proceso_cod)
      RETURNING r_bandera
   IF(r_bandera = 0)THEN
      DISPLAY "   El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
   IF(r_bandera = 0)THEN
      CALL fn_dpe_corrige_reg_integracion(p_d_folio)
      DISPLAY "   Operación lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de la operación de Integración"
   
   LET p_mensaje = "   Finalización de la operación de Integración","\n",
                  "   Folio: "||p_d_folio,"\n",
                  "   Fecha de inicio: "||TODAY,"\n",
                  "   Hora           : ",CURRENT HOUR TO SECOND,"\n"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
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
   p_d_folio                 LIKE dis_preliquida.folio_liquida

   DELETE FROM dae_aceptados_ajuste
   WHERE id_dae_ref_ajuste IN (SELECT id_dae_ref_ajuste
                               FROM  dae_det_ajuste
                               WHERE folio_lote = p_d_folio);
   --   
   DELETE FROM dae_det_ajuste
   WHERE folio_lote = p_d_folio;
   
   DELETE FROM dae_rch_archivo
   WHERE folio = p_d_folio;

   --Actualiza el folio en la tabla de detalles
   UPDATE dae_det_solicitud 
   SET    folio_ajuste = NULL
   WHERE  folio_ajuste = p_d_folio;
  
   UPDATE glo_ctr_archivo
   SET    folio  = NULL, 
          estado = 1
   WHERE  proceso_cod = g_proceso_cod
   AND    folio = p_d_folio
   AND    estado = 2;
   
   UPDATE glo_folio
   SET STATUS = -1
   WHERE  folio = p_d_folio;

   DELETE FROM sfr_marca_activa 
   WHERE marca = 403
   AND   folio = p_d_folio;

   DELETE FROM sfr_marca_historica 
   WHERE marca = 403
   AND   folio = p_d_folio;
   
END FUNCTION -- fn_dpe_corrige_reg_integracion