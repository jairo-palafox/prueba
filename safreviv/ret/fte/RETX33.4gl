IMPORT util
IMPORT SECURITY

SCHEMA safre_viv
{
RETX33

funciones de interacción con CRM

}
GLOBALS "ret_busca_caso_crm.inc"        -- Busca caso en CRM para conocer su estado
GLOBALS "ret_cancela_caso_crm.inc"      -- Cancela caso en CRM 
GLOBALS "ret_confirma_pago_crm.inc"     -- Confirma pago realizado a CRM
GLOBALS "ret_confirma_pago_ae.inc"     -- Confirma pago realizado a CRM de Amortizaciones Excedentes
GLOBALS "ret_crea_caso_crm.inc"         -- Crea el caso en CRM
GLOBALS "ret_actualiza_estado_crm.inc"  -- Actualiza el estado del caso en CRM
GLOBALS "ret_adjunta_docto_crm.inc"     -- Adjunta documentos en CRM (tableta)
GLOBALS "RETG01.4gl"                -- Constantes para los codigos de error
{
======================================================================
Clave: 
Nombre: fn_busca_caso_crm
Fecha creacion: Junio 11, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Busca en CRM si hay un caso abierto

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_busca_caso_crm(p_nss)
DEFINE p_nss              LIKE afi_derechohabiente.nss, 
       p_rfc              LIKE afi_derechohabiente.rfc,
       p_modalidad        SMALLINT, -- modalidad de retiro
       v_conteo           SMALLINT, -- num de solicitudes encontradas
       p_rango_estados    STRING, -- rango de estados de solicitud
       v_sql              STRING, -- cadena con consulta SQL
       v_existe_solicitud SMALLINT, -- booleana que indica si existe una solicitud
       v_id_solicitud     LIKE ret_fondo_ahorro_generico.id_solicitud,
       v_resultado        INTEGER 

   DEFINE v_arr_respuesta DYNAMIC ARRAY OF RECORD
         casos           STRING, 
         fecha_creacion  CHAR(10),
         status          CHAR(5),
         fecha_modifica  CHAR(10),
         clase_operacion STRING,
         tipificacion    CHAR(4),
         texto_status    STRING,
         permite_adoc    CHAR(05),
         marca_origen    STRING
   END RECORD 
   DEFINE v_indice       INTEGER
   DEFINE v_mensaje      STRING   
   
   LET MT_BusquedaCasosCRM_SSV_req.nss =  p_nss  
   LET v_mensaje = CURRENT YEAR TO SECOND, " Se llama al servicio de busqueda de Caso" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " NSS >", p_nss, "<"
   DISPLAY v_mensaje
 
   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL SI_BusquedaCasosCRMSSV_SO_g() RETURNING v_resultado       

   LET v_mensaje = CURRENT YEAR TO SECOND, " El resultado del llamado al servicio de busqueda de caso es >", v_resultado, "<"
   DISPLAY v_mensaje
   
   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN      
      CALL v_arr_respuesta.clear()

      FOR v_indice = 1 TO MT_BusquedaCasosCRM_SSV_res.Casos.getLength()
         LET v_arr_respuesta[v_indice].casos           = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].casos
         LET v_arr_respuesta[v_indice].fecha_creacion  = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].fecha_creacion
         LET v_arr_respuesta[v_indice].status          = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].status
         LET v_arr_respuesta[v_indice].fecha_modifica  = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].fecha_modifica
         LET v_arr_respuesta[v_indice].clase_operacion = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].clase_operacion
         LET v_arr_respuesta[v_indice].tipificacion    = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].tipificacion
         LET v_arr_respuesta[v_indice].texto_status    = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].texto_status
         LET v_arr_respuesta[v_indice].permite_adoc    = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].permite_adoc
         LET v_arr_respuesta[v_indice].marca_origen    = MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].marca_origen
         LET v_mensaje = CURRENT YEAR TO SECOND, "El registro:",v_indice, " de la busqueda de casos"
         DISPLAY v_mensaje
         DISPLAY MT_BusquedaCasosCRM_SSV_res.Casos[v_indice].*
      END FOR 
    ELSE 
        LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de búsqueda de caso CRM"
        DISPLAY v_mensaje
        DISPLAY "CODE       : ", wsError.code
        DISPLAY "CODENS     : ", wsError.codeNS
        DISPLAY "DESCRIPTION: ", wsError.description
        DISPLAY "ACTION     : ", wsError.action	  
        CALL v_arr_respuesta.clear()
    END IF

    
    RETURN v_resultado, v_arr_respuesta
    
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_cancela_caso_crm
Fecha creacion: Junio 11, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Cancela un caso en CRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_cancela_caso_crm(p_caso)
DEFINE p_caso             CHAR(10),
       v_ecod             CHAR(5),
       v_resultado        INTEGER 


   DEFINE v_mensaje      STRING   
   
   LET MT_cancelaCasoSSV_req.caso =  p_caso
   LET v_mensaje = CURRENT YEAR TO SECOND, " Se llama al servicio de Cancelación de Caso" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Caso >", p_caso, "<"
   DISPLAY v_mensaje
       
   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL SI_cancelaCasoSSV_SO_g() RETURNING v_resultado       
   LET v_mensaje = CURRENT YEAR TO SECOND, " El resultado del llamado al servicio de cancelación de caso es >", v_resultado, "<"
   DISPLAY v_mensaje

   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN      
      LET v_ecod = MT_cancelaCasoSSV_res.eCod
      LET v_mensaje = CURRENT YEAR TO SECOND, " El código resultado de la cancelación >", MT_cancelaCasoSSV_res.eCod, "<"
      DISPLAY v_mensaje
    ELSE 
        LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de cancelación de caso CRM"
        DISPLAY v_mensaje
        DISPLAY "CODE       : ", wsError.code
        DISPLAY "CODENS     : ", wsError.codeNS
        DISPLAY "DESCRIPTION: ", wsError.description
        DISPLAY "ACTION     : ", wsError.action	  
    END IF
    
    RETURN v_resultado, v_ecod
    
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_crea_caso_crm
Fecha creacion: Junio 12, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Se crea un caso en CRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_caso_crm(p_nss, p_nombre, p_paterno, p_materno,p_rfc, p_curp, p_clase_operacion, p_tipificacion)
DEFINE p_nombre           CHAR(40),
       p_paterno          CHAR(40),
       p_materno          CHAR(40),
       p_nss              CHAR(11),
       p_rfc              CHAR(13),
       p_curp             CHAR(18),
       p_tipificacion     CHAR(14),
       p_clase_operacion  CHAR(4),
       v_caso_crm         CHAR(10),
       v_resultado        INTEGER 


   DEFINE v_mensaje      STRING  
   LET v_resultado = 0
   LET v_caso_crm  = ""
   
   LET MT_CrearOrdenGenerico_req.nrp                         = ""
   LET MT_CrearOrdenGenerico_req.correo                      = ""
   LET MT_CrearOrdenGenerico_req.claseOperacion              = p_clase_operacion
   LET MT_CrearOrdenGenerico_req.parametros[1].identificador = "NSS"
   LET MT_CrearOrdenGenerico_req.parametros[1].linea         = "1"
   LET MT_CrearOrdenGenerico_req.parametros[1].valor         = p_nss
   LET MT_CrearOrdenGenerico_req.parametros[2].identificador = "TIPIFICACION"
   LET MT_CrearOrdenGenerico_req.parametros[2].linea         = "2"
   LET MT_CrearOrdenGenerico_req.parametros[2].valor         = p_tipificacion
   LET MT_CrearOrdenGenerico_req.parametros[3].identificador = "NOMBRE"
   LET MT_CrearOrdenGenerico_req.parametros[3].linea         = "3"
   LET MT_CrearOrdenGenerico_req.parametros[3].valor         = p_nombre
   LET MT_CrearOrdenGenerico_req.parametros[4].identificador = "APPATERNO"
   LET MT_CrearOrdenGenerico_req.parametros[4].linea         = "4"
   LET MT_CrearOrdenGenerico_req.parametros[4].valor         = p_paterno
   LET MT_CrearOrdenGenerico_req.parametros[5].identificador = "APMATERNO"
   LET MT_CrearOrdenGenerico_req.parametros[5].linea         = "5"
   LET MT_CrearOrdenGenerico_req.parametros[5].valor         = p_materno
   LET MT_CrearOrdenGenerico_req.parametros[6].identificador = "RFC"
   LET MT_CrearOrdenGenerico_req.parametros[6].linea         = "6"
   LET MT_CrearOrdenGenerico_req.parametros[6].valor         = p_rfc
   LET MT_CrearOrdenGenerico_req.parametros[7].identificador = "CURP"
   LET MT_CrearOrdenGenerico_req.parametros[7].linea         = "7"
   LET MT_CrearOrdenGenerico_req.parametros[7].valor         = p_curp

   LET v_mensaje = CURRENT YEAR TO SECOND, " Se llama al servicio de Creación de Caso" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Nss >", p_nss, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " NRP >", MT_CrearOrdenGenerico_req.nrp, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Correo >", MT_CrearOrdenGenerico_req.correo, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Clase Operacion >", MT_CrearOrdenGenerico_req.claseOperacion, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Linea 1 >", MT_CrearOrdenGenerico_req.parametros[1].linea,"|", MT_CrearOrdenGenerico_req.parametros[1].identificador,"|",MT_CrearOrdenGenerico_req.parametros[1].valor, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Linea 2 >", MT_CrearOrdenGenerico_req.parametros[2].linea,"|", MT_CrearOrdenGenerico_req.parametros[2].identificador,"|",MT_CrearOrdenGenerico_req.parametros[2].valor, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Linea 3 >", MT_CrearOrdenGenerico_req.parametros[3].linea,"|", MT_CrearOrdenGenerico_req.parametros[3].identificador,"|",MT_CrearOrdenGenerico_req.parametros[3].valor, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Linea 4 >", MT_CrearOrdenGenerico_req.parametros[4].linea,"|", MT_CrearOrdenGenerico_req.parametros[4].identificador,"|",MT_CrearOrdenGenerico_req.parametros[4].valor, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Linea 5 >", MT_CrearOrdenGenerico_req.parametros[5].linea,"|", MT_CrearOrdenGenerico_req.parametros[5].identificador,"|",MT_CrearOrdenGenerico_req.parametros[5].valor, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Linea 6 >", MT_CrearOrdenGenerico_req.parametros[6].linea,"|", MT_CrearOrdenGenerico_req.parametros[6].identificador,"|",MT_CrearOrdenGenerico_req.parametros[6].valor, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Linea 7 >", MT_CrearOrdenGenerico_req.parametros[7].linea,"|", MT_CrearOrdenGenerico_req.parametros[7].identificador,"|",MT_CrearOrdenGenerico_req.parametros[7].valor, "<"
   DISPLAY v_mensaje

   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL SI_CrearOrdenGenerico_SO_g() RETURNING v_resultado       
   LET v_mensaje = CURRENT YEAR TO SECOND, " El resultado del llamado al servicio de Creación de caso es >", v_resultado, "<"
   DISPLAY v_mensaje

   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN
      LET v_mensaje = CURRENT YEAR TO SECOND, " Los valores regresados >"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " Código                 >", MT_CrearOrdenGenerico_res.codigo, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " Descripción            >", MT_CrearOrdenGenerico_res.descripcion, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " DescripcionRefCatalogo >", MT_CrearOrdenGenerico_res.descripcionRefCatalogo, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " Estatus                >", MT_CrearOrdenGenerico_res.estatus, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " Fecha Nivel Servicio   >", MT_CrearOrdenGenerico_res.fechaNivelServicio, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " Órden Servicio         >", MT_CrearOrdenGenerico_res.ordenServicio, "<"
      DISPLAY v_mensaje
      IF MT_CrearOrdenGenerico_res.codigo = "0000" THEN 
         LET v_caso_crm = MT_CrearOrdenGenerico_res.ordenServicio
         LET v_mensaje = CURRENT YEAR TO SECOND, " La orden de servicio (Caso)  >", MT_CrearOrdenGenerico_res.ordenServicio, "<"
         DISPLAY v_mensaje
         LET v_resultado = 0
      ELSE 
         LET v_resultado = 3
         LET v_caso_crm  = ""
      END IF  
   ELSE 
      LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de Creación de caso CRM"
      DISPLAY v_mensaje
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESCRIPTION: ", wsError.description
      DISPLAY "ACTION     : ", wsError.action
      LET v_resultado = -3
      LET v_caso_crm  = ""
   END IF
    
   RETURN v_resultado, v_caso_crm
    
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_confirma_pago_crm
Fecha creacion: Junio 11, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Se Confirma el pago de un retiro a CRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_confirma_pago_crm(p_nss, p_caso, p_tipo_beneficiario, p_ref_pago, p_estado_solicitud, p_c_rechazo, p_des_rechazo)
   DEFINE p_nss               CHAR(11),
          p_caso              CHAR(10),
          p_tipo_beneficiario SMALLINT,
          p_ref_pago          CHAR(10), 
          p_estado_solicitud  SMALLINT,
          p_c_rechazo         CHAR(3),
          p_des_rechazo       CHAR(30),
          v_codigo            CHAR(4),
          v_resultado        INTEGER,
          v_nombre           CHAR(40),
          v_paterno          CHAR(40),
          v_materno          CHAR(40)


   DEFINE v_mensaje      STRING   
   
   LET MT_confirmacionPagoSSV_req.requestConfirmacionPago.nss              = p_nss
   LET MT_confirmacionPagoSSV_req.requestConfirmacionPago.noCaso           = p_caso
   LET MT_confirmacionPagoSSV_req.requestConfirmacionPago.tipoBeneficiario = p_tipo_beneficiario
   LET MT_confirmacionPagoSSV_req.requestConfirmacionPago.referenciaPago   = p_ref_pago
   IF p_estado_solicitud = 71 THEN 
      LET MT_confirmacionPagoSSV_req.confirmacionPago.estatusSACI = '01'
      LET MT_confirmacionPagoSSV_req.confirmacionPago.cRechazo = ''
      LET MT_confirmacionPagoSSV_req.confirmacionPago.dRechazo = ''
      LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].nota = 'PAGADO'
   ELSE 
      LET MT_confirmacionPagoSSV_req.confirmacionPago.estatusSACI = '95'
      LET MT_confirmacionPagoSSV_req.confirmacionPago.cRechazo = p_c_rechazo
      LET MT_confirmacionPagoSSV_req.confirmacionPago.dRechazo = p_des_rechazo
      LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].nota = 'RECHAZADO'
   END IF 
   -- Se reporta solo un beneficiario
   --- Busca la información del derechohabiente
   SELECT nombre_af, ap_paterno_af, ap_materno_af
   INTO   v_nombre, v_paterno, v_materno
   FROM   afi_derechohabiente
   WHERE  nss = p_nss

   LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].idBeneficiario = 1
   LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].marcaPago = ''
   LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].porcentaje = 100
   LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].referenciaPago = p_ref_pago
   LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].nombreBeneficiario = v_nombre
   LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].paternoBeneficiario = v_paterno
   LET MT_confirmacionPagoSSV_req.confirmacionBeneficiarios[1].maternoBeneficiario = v_materno
   
   LET v_mensaje = CURRENT YEAR TO SECOND, " Se llama al servicio de Confirmación de pago en CRM" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Caso >", p_caso, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " NSS >", p_nss, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Tipo beneficiario >", p_tipo_beneficiario, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Referencia Pago >", p_ref_pago, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Estatus SACI >", MT_confirmacionPagoSSV_req.confirmacionPago.estatusSACI, "<"
   DISPLAY v_mensaje
       
   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL SI_confirmacionPagoSSV_SO_g() RETURNING v_resultado       
   LET v_mensaje = CURRENT YEAR TO SECOND, " El resultado del llamado al servicio de confirmación de pago es >", v_resultado, "<"
   DISPLAY v_mensaje

   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN      
      LET v_codigo = MT_confirmacionPagoSSV_res.confirmacionPago[1].codigo
      LET v_mensaje = CURRENT YEAR TO SECOND, " El código devuelto >", MT_confirmacionPagoSSV_res.confirmacionPago[1].codigo, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " La descripcón devuelta >", MT_confirmacionPagoSSV_res.confirmacionPago[1].descripcion, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " El mensaje devuelto >", MT_confirmacionPagoSSV_res.confirmacionPago[1].mensaje, "<"
      DISPLAY v_mensaje
    ELSE 
      LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de confirmación de pago CRM"
      DISPLAY v_mensaje
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESCRIPTION: ", wsError.description
      DISPLAY "ACTION     : ", wsError.action	  
    END IF
    
    RETURN v_resultado, v_codigo
    
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_confirma_pago_ae
Fecha creacion: Octubre 2, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Se Confirma el pago de un retiro a de Amortizaciones Excedentes a CRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_confirma_pago_ae(p_nss, p_caso, p_tipo_beneficiario, p_ref_pago, p_estado_solicitud, p_c_rechazo, p_des_rechazo)
   DEFINE p_nss               CHAR(11),
          p_caso              CHAR(10),
          p_tipo_beneficiario SMALLINT,
          p_ref_pago          CHAR(10), 
          p_estado_solicitud  SMALLINT,
          p_c_rechazo         CHAR(3),
          p_des_rechazo       CHAR(30),
          v_codigo            CHAR(4),
          v_resultado        INTEGER 


   DEFINE v_mensaje      STRING   
   LET MT_confirmacionPago_req.nss = p_nss
   LET MT_confirmacionPago_req.noCaso = p_caso
   LET MT_confirmacionPago_req.referenciaPago = p_ref_pago
   IF p_estado_solicitud = 71 THEN 
      LET MT_confirmacionPago_req.estatusSACI = '01'
      LET MT_confirmacionPago_req.codRechazo = ''
      LET MT_confirmacionPago_req.descRechazo = ''
   ELSE 
      LET MT_confirmacionPago_req.estatusSACI = '95'
      LET MT_confirmacionPago_req.codRechazo = p_c_rechazo
      LET MT_confirmacionPago_req.descRechazo = p_des_rechazo
   END IF 
   LET v_mensaje = CURRENT YEAR TO SECOND, " Se llama al servicio de Confirmación de pago en CRM de Amortizaciones Excedentes" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Caso >", p_caso, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " NSS >", p_nss, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Referencia Pago >", p_ref_pago, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Estatus SACI >", MT_confirmacionPago_req.estatusSACI, "<"
   DISPLAY v_mensaje

   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL SI_confirmacionPago_SO_g() RETURNING v_resultado       
   LET v_mensaje = CURRENT YEAR TO SECOND, " El resultado del llamado al servicio de confirmación de pago es >", v_resultado, "<"
   DISPLAY v_mensaje

   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN     
      LET v_codigo = MT_confirmacionPago_res.codigo

      LET v_mensaje = CURRENT YEAR TO SECOND, " El código devuelto >", MT_confirmacionPago_res.codigo, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " La descripcón devuelta >", MT_confirmacionPago_res.descripcion, "<"
      DISPLAY v_mensaje
      LET v_mensaje = CURRENT YEAR TO SECOND, " El mensaje devuelto >", MT_confirmacionPago_res.mensaje, "<"
      DISPLAY v_mensaje
    ELSE 
      LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de confirmación de pago CRM"
      DISPLAY v_mensaje
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESCRIPTION: ", wsError.description
      DISPLAY "ACTION     : ", wsError.action	  
    END IF
    
    RETURN v_resultado, v_codigo
    
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_actualiza_estado_crm
Fecha creacion: Julio 4, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Actualiza el estado del caso en CRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_actualiza_estado_crm(p_caso, p_nss)
DEFINE p_caso             CHAR(10),
       p_nss              CHAR(11),
       v_codigo           CHAR(5),
       v_resultado        INTEGER 


   DEFINE v_mensaje      STRING   

   LET MT_ActualizarEstatusCasoCRM_req.RequestActualizaEstatus.noCaso = p_caso
   LET MT_ActualizarEstatusCasoCRM_req.RequestActualizaEstatus.nss    = p_nss
   LET v_mensaje = CURRENT YEAR TO SECOND, " Se llama al servicio de Actualización del Estado en CRM" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Caso >", p_caso, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " NSS  >", p_nss, "<"
   DISPLAY v_mensaje
   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL SI_ActualizarEstatusCasoCRM_SO_g() RETURNING v_resultado       
   LET v_mensaje = CURRENT YEAR TO SECOND, " El resultado del llamado al servicio de actualización del estado en CRM es >", v_resultado, "<"
   DISPLAY v_mensaje

   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN      
      LET v_codigo = MT_ActualizarEstatusCasoCRM_res.ResponseActualizaEstatus.codigo
      LET v_mensaje = CURRENT YEAR TO SECOND, " El código devuelto >", MT_ActualizarEstatusCasoCRM_res.ResponseActualizaEstatus.codigo, "<"
      DISPLAY v_mensaje
    ELSE 
      LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de actualización del estado en CRM"
      DISPLAY v_mensaje
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESCRIPTION: ", wsError.description
      DISPLAY "ACTION     : ", wsError.action	  
    END IF
    
    RETURN v_resultado, v_codigo
    
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_adjunta_docto_crm
Fecha creacion: Julio 4, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Adjunta documentos en CRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_adjunta_docto_crm(p_caso, p_arr_documentos)
DEFINE p_caso             CHAR(10),
       p_arr_documentos   RECORD 
           nombre_docto    STRING,
           docto           STRING  
       END RECORD,
       v_codigo           CHAR(5),
       v_resultado        INTEGER,
       v_indice           SMALLINT  


   DEFINE v_mensaje      STRING   
   LET v_indice = 1

   LET MT_AdjuntarDocumento_req.caso = p_caso
   LET MT_AdjuntarDocumento_req.marca_origen = "CRM"
   
   --FOR v_indice = 1 TO p_arr_documentos.getLength()
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].archivo        = p_arr_documentos.docto
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].nombre_archivo = p_arr_documentos.nombre_docto
   --END FOR 
   LET v_mensaje = CURRENT YEAR TO SECOND, " Se llama al servicio de Adjunta documentos en CRM" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Caso           >", p_caso, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Marca Origen   >", MT_AdjuntarDocumento_req.marca_origen, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Nombre Archivo >",p_arr_documentos.nombre_docto, "<" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, "                >",MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].nombre_archivo, "<" 
   DISPLAY v_mensaje
--   LET v_mensaje = CURRENT YEAR TO SECOND, " Archivo        >",p_arr_documentos.docto, "<" 
--   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, "                >",MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].archivo, "<" 
   DISPLAY v_mensaje
--   LET v_mensaje = CURRENT YEAR TO SECOND, " Se adjuntaron :", p_arr_documentos.getLength(), " documentos"
--   DISPLAY v_mensaje
   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL SI_AdjuntarDocumento_SO_g() RETURNING v_resultado       
   LET v_mensaje = CURRENT YEAR TO SECOND, " El resultado del llamado al servicio de adjunta documentos en CRM es >", v_resultado, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Respuesta >", MT_AdjuntarDocumento_res.status, "<"
   DISPLAY v_mensaje

   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN      
      LET v_mensaje = CURRENT YEAR TO SECOND, " El estatus devuelto >", MT_AdjuntarDocumento_res.status, "<"
      DISPLAY v_mensaje
   ELSE 
      LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de adjunta documentos en CRM"
      DISPLAY v_mensaje
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESCRIPTION: ", wsError.description
      DISPLAY "ACTION     : ", wsError.action	  
   END IF
    
   RETURN v_resultado, MT_AdjuntarDocumento_res.status
    
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_adjunta_docto_crm
Fecha creacion: Julio 4, 2018
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Adjunta documentos en CRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_adjunta_multidocto_crm(p_caso, p_arr_documentos)
DEFINE p_caso             CHAR(10),
       p_arr_documentos   RECORD   
           nombre_docto    STRING,
           docto_1         STRING,
           docto_2         STRING,
           docto_3         STRING,
           docto_4         STRING,
           docto_5         STRING
       END RECORD,
       v_codigo           CHAR(5),
       v_resultado        INTEGER,
       v_indice           SMALLINT  


   DEFINE v_mensaje      STRING   
   LET v_indice = 0

   LET MT_AdjuntarDocumento_req.caso = p_caso
   LET MT_AdjuntarDocumento_req.marca_origen = "CRM"
   IF p_arr_documentos.docto_1 IS NOT NULL THEN
      LET v_indice = v_indice + 1
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].archivo        = p_arr_documentos.docto_1
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].nombre_archivo = p_arr_documentos.nombre_docto
   END IF
   IF p_arr_documentos.docto_2 IS NOT NULL THEN
      LET v_indice = v_indice + 1
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].archivo        = p_arr_documentos.docto_2
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].nombre_archivo = p_arr_documentos.nombre_docto
   END IF
   IF p_arr_documentos.docto_3 IS NOT NULL THEN
      LET v_indice = v_indice + 1
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].archivo        = p_arr_documentos.docto_3
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].nombre_archivo = p_arr_documentos.nombre_docto
   END IF
   IF p_arr_documentos.docto_4 IS NOT NULL THEN
      LET v_indice = v_indice + 1
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].archivo        = p_arr_documentos.docto_4
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].nombre_archivo = p_arr_documentos.nombre_docto
   END IF
   IF p_arr_documentos.docto_5 IS NOT NULL THEN
      LET v_indice = v_indice + 1
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].archivo        = p_arr_documentos.docto_5
      LET MT_AdjuntarDocumento_req.Adjunta_doc[v_indice].nombre_archivo = p_arr_documentos.nombre_docto
   END IF
   
   LET v_mensaje = CURRENT YEAR TO SECOND, " Se llama al servicio de Adjunta documentos en CRM" 
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Caso           >", p_caso, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Se adjuntaron :", v_indice, " documentos"
   DISPLAY v_mensaje
   -- se invoca el servicio
   -- DISPLAY "Ejecutando WSSaldoPreliminar..."
   CALL SI_AdjuntarDocumento_SO_g() RETURNING v_resultado       
   LET v_mensaje = CURRENT YEAR TO SECOND, " El resultado del llamado al servicio de adjunta documentos en CRM es >", v_resultado, "<"
   DISPLAY v_mensaje
   LET v_mensaje = CURRENT YEAR TO SECOND, " Respuesta >", MT_AdjuntarDocumento_res.status, "<"
   DISPLAY v_mensaje

   -- si el webservice NO se ejecuto correctamente
   IF ( v_resultado = 0 ) THEN      
      LET v_mensaje = CURRENT YEAR TO SECOND, " El estatus devuelto >", MT_AdjuntarDocumento_res.status, "<"
      DISPLAY v_mensaje
   ELSE 
      LET v_mensaje = CURRENT YEAR TO SECOND, "ERROR al invocar webservice de adjunta documentos en CRM"
      DISPLAY v_mensaje
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESCRIPTION: ", wsError.description
      DISPLAY "ACTION     : ", wsError.action	  
   END IF
    
   RETURN v_resultado, MT_AdjuntarDocumento_res.status
    
END FUNCTION
