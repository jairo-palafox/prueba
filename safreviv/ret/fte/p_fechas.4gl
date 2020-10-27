MAIN
CALL fechas()


END MAIN

FUNCTION fechas()
DEFINE v_f_inicio_pension,date_compara   DATE, -- fecha de inicio de pension en el SPESS
p_grupo_ley73        SMALLINT


LET v_f_inicio_pension = 01/12/2012
-- 01/12/2012
LET date_compara = "01/13/2012"

LET p_grupo_ley73 =1


CASE p_grupo_ley73
       -- GRUPO 1
       WHEN 1
          -- si la fecha de inicio de pension es igual o posterior al 13 de enero de 2012
          IF ( v_f_inicio_pension >= "01/13/2012" ) THEN
             -- se valida la solicitud para un grupo 1
             --CALL fn_retl73_valida_grupo1(p_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
             DISPLAY "entra a validar trae saldos"
          ELSE
                DISPLAY "donde deberia de entrar para marcar error"
          -- la fecha es invalida para grupo 1
            { CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 8, 0, TODAY)
             CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 4, 0, TODAY)
             CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 12,0, TODAY)}
          END IF
          
       -- GRUPO 2
       WHEN 2
          -- si la fecha de inicio de pension es anterior al 13 de enero de 2012
          IF ( v_f_inicio_pension < "01/13/2012" ) THEN
             -- se valida la solicitud para un grupo 2
            -- CALL fn_retl73_valida_grupo_2_y_3(p_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
             DISPLAY "entra en validar grupo 2"
          ELSE
             DISPLAY "ERROR EN  grupo 2"
             -- la fecha es invalida para grupo 2
             {CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 8, 0, TODAY)
             CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 4, 0, TODAY)            
             CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 12,0, TODAY)}
          END IF
          
       -- GRUPO 3
       WHEN 3
          -- si la fecha de inicio de pension es anterior al 13 de enero de 2012
          IF ( v_f_inicio_pension < "01/13/2012" ) THEN
             -- se valida la solicitud para un grupo 3
            --CALL fn_retl73_valida_grupo_2_y_3(p_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
            DISPLAY "entra en validar grupo 3"
          ELSE
              DISPLAY "ERROR grupo 3"
             -- la fecha es invalida para grupo 3
            { CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 8, 0, TODAY)
             CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 4, 0, TODAY)
             CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 12,0, TODAY) }           
          END IF
 
       -- GRUPO 4
       WHEN 4
          -- si la fecha de inicio de pension es anterior al 13 de enero de 2012
          IF ( v_f_inicio_pension < "01/13/2012" ) THEN
             -- se valida la solicitud para un grupo 4
             --CALL fn_retl73_valida_grupo4(p_nss, v_aivs_viv92, v_aivs_viv97, v_f_inicio_pension, p_es_consulta)
             DISPLAY "entra en validar grupo 4"
          ELSE
             -- la fecha es invalida para grupo 4
             {CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 8, 0, TODAY)
             CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 4, 0, TODAY)
             CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_fecha_resolucion_invalida_l73, 12,0, TODAY)}
             DISPLAY "ERROR grupo 2"
          END IF
 
       -- otro grupo es invalido
       OTHERWISE
          -- se rechaza viv92 y viv97

          DISPLAY "OTHERWISE"
          {CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_causal_retiro_invalido, 8, 0, TODAY)
          CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_causal_retiro_invalido, 4, 0, TODAY)
          CALL fn_respuesta_ws_ley73(gi_solicitud_rechazada, gi_causal_retiro_invalido, 12,0, TODAY)}
    END CASE


END FUNCTION 