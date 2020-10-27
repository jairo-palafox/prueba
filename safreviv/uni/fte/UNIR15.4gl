--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/06/2012
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR15                                                        #
#Objetivo     => Programa lanzador de la integración de unificación INFONAVIT  #
#Fecha inicio => Junio 06, 2012                                                #
################################################################################

--Lanzador: UNIL15

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
           v_s_sql  STRING 
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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR15.log")

   LET g_proceso_cod = g_proceso_cod_uni_infonavit -- devolucion de operaciones
   LET g_opera_cod   = 2  -- integración
   
   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0

   -- Restaura datos afectados por integración
   CALL fn_dpe_corrige_reg_integracion(p_d_folio)
   -- Prepara el la cadena para ejecutar el procedimiento de reverso desmarca
    
    DISPLAY "Folio del lote: ", p_d_folio
    --Reversa marcas 503 y 504
    LET v_s_sql = "EXECUTE FUNCTION fn_uni_reverso_integracion_infonavit(?)"
    PREPARE Prpr_integraIMSS FROM v_s_sql CLIPPED
    -- se ejecuta el stored procedure de integracion
    EXECUTE Prpr_integraIMSS USING p_d_folio
    INTO v_si_resultado, isam_err, err_txt
    IF v_si_resultado < 0 THEN
       DISPLAY "La Reversa marca no puedo concretarse: ", bn_reverso_desmarca
    ELSE
       --Borra registros de tablas históricas.
       --DELETE FROM  uni_inf_unificador
             --WHERE  folio_unificacion =  p_d_folio
       --DELETE FROM  uni_inf_unificado
             --WHERE  folio_unificacion =  p_d_folio
       DISPLAY "Reversa marca se aplico correctamente: ", bn_reverso_desmarca
    END IF

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
   
   LET p_titulo = "Finalización de proceso - REVERSO INTEGRACIÓN"
   
   LET p_mensaje = "Finalización de proceso - REVERSO INTEGRACIÓN","\n",
                  "#\n",
                  "# Folio: "||p_d_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"
   
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
FUNCTION fn_dpe_corrige_reg_integracion(p_d_folio)
  DEFINE 
   p_d_folio                 LIKE dis_preliquida.folio_liquida
   --

   
   UPDATE glo_ctr_archivo
      SET estado = 1, folio = NULL -- cargado
    WHERE proceso_cod = g_proceso_cod_uni_infonavit
      AND folio = p_d_folio
      AND estado = 2 -- integrado

   -- ]

END FUNCTION -- fn_dpe_corrige_reg_integracion
