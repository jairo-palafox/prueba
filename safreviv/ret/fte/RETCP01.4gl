--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC270                                                                #
#OBJETIVO     => Consulta de digito verificador de un nss                               #
#Fecha inicio => 18 de Agosto 2015                                                      #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

{
======================================================================
Clave: 
Nombre: main
Fecha creacion: agosto 18, 2015
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Consulta el calculo del digito verificador para un nss dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
MAIN 
DEFINE cbx_modalidad_retiro ui.ComboBox, -- combo de modalidad de retiro
       cbx_causal_retiro    ui.ComboBox,    -- combo de causal de retiro
       cbx_estado_solicitud ui.ComboBox, -- combo de estado de la solicitud
       cbx_cod_rechazo      ui.ComboBox, -- combo con los codigos de rechazo       
       ar_ret_tipo_retiro   RECORD LIKE ret_tipo_retiro.*, -- registro con los tipos de retiro
       ar_ret_rechazo       RECORD LIKE ret_rechazo.*, -- registro con los codigos de rechazo
       -- parametros de consulta
       v_tipo_retiro        LIKE ret_tipo_retiro.tpo_retiro,
       v_etapa              SMALLINT, -- 1 Solicitud, 2 Preliquidacion, 3 Liquidacion
       v_nss                LIKE afi_derechohabiente.nss, 
       v_sql                STRING,
       v_rfc                LIKE afi_derechohabiente.rfc,
       v_id_solicitud       LIKE ret_solicitud_generico.id_solicitud,
       v_folio              LIKE glo_folio.folio,
       v_folio_restitucion  LIKE ret_solicitud_generico.folio_restitucion,
       v_estado             LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo        SMALLINT, -- codigo de rechazo
       v_fecha_inicio       DATE, -- fecha de inicio de consulta
       v_fecha_fin          DATE, -- fecha fin de consulta
       v_cadena             STRING, -- cadena para concatenar
       v_modalidad_retiro   LIKE ret_modalidad_retiro.modalidad_retiro, -- modalidad de retiro
       v_causal_retiro      LIKE ret_causal_retiro.causal_retiro,   -- causal de retiro
       v_causal_retiro_trab LIKE ret_causal_retiro.causal_retiro,   -- causal de retiro
       v_r_ret_modalidad_retiro RECORD LIKE ret_modalidad_retiro.*, -- registro de modalidad de retiro
       v_r_ret_causal_retiro    RECORD LIKE ret_causal_retiro.*,    -- registro de causal de retiro
       v_caso_adai         LIKE ret_solicitud_generico.caso_adai,   -- caso ADAI de la solicitud
       v_formulario        ui.Form, -- para modificar el formulario
       ar_ret_estado_solicitud RECORD LIKE ret_estado_solicitud.*,
       v_resultado_funcion  INTEGER,
       v_error              INTEGER,
       v_mensaje_error      CHAR(250),
       v_digito             SMALLINT

   
   
    CLOSE WINDOW SCREEN

    -- se abre la ventana de consulta
    OPEN WINDOW w_consulta WITH FORM "RETCP011"

    -- se capturan los datos de la consulta
    INPUT BY NAME
        v_nss              
    WITHOUT DEFAULTS
    ATTRIBUTES ( UNBUFFERED )

        BEFORE INPUT
            -- se obtiene control del formulario
            LET v_formulario = DIALOG.getForm()
              
        ON ACTION cancel
            EXIT INPUT

        ON ACTION accept
            -- se validan los datos capturados
            IF ( v_nss IS NOT NULL AND LENGTH(v_nss CLIPPED) <> 10 ) THEN
                CALL fn_mensaje("Atención","La longitud del NSS debe ser de 10 caracteres","stop")
                CONTINUE INPUT
            END IF
            LET v_sql = "EXECUTE FUNCTION fn_calcula_digito_verificador_nss(?)"
            DISPLAY "Consulta Completa >" || v_sql || "<"
            --obteción de datos extras, acumulado de aivs y pesos
            PREPARE sid_sumas FROM v_sql
            EXECUTE sid_sumas USING v_nss INTO v_resultado_funcion,v_error,
                                               v_mensaje_error,v_digito

            DISPLAY "El resultado ", v_resultado_funcion
            DISPLAY "El error     ", v_error
            DISPLAY "El mensaje   ", v_mensaje_error
            
            DISPLAY v_digito TO v_digito
            
   END INPUT

  CLOSE WINDOW w_consulta
END MAIN


