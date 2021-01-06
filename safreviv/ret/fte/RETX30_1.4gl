IMPORT util
IMPORT SECURITY
IMPORT com
IMPORT xml


SCHEMA safre_viv
{
RETX30

funciones de validacion para el webservice de consulta de saldos disponibles de
retiro generico

}
GLOBALS "ret_ws_cons_rel_laboral.inc" -- consulta de pago a FICO
GLOBALS "ret_saldo_preliminar.inc" -- saldo prelimminar PROCESAR
GLOBALS "RETG01.4gl"               -- Constantes para los codigos de error
{
======================================================================
Clave: 
Nombre: fn_existe_solicitud_retiro_generico
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera una solicitud de retiro de fondo de ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_existe_solicitud_retiro_generico(p_nss, p_rfc, p_modalidad, p_rango_estados)
DEFINE p_nss              LIKE afi_derechohabiente.nss, 
       p_rfc              LIKE afi_derechohabiente.rfc,
       p_modalidad        SMALLINT, -- modalidad de retiro
       v_conteo           SMALLINT, -- num de solicitudes encontradas
       p_rango_estados    STRING, -- rango de estados de solicitud
       v_sql              STRING, -- cadena con consulta SQL
       v_existe_solicitud SMALLINT, -- booleana que indica si existe una solicitud
       v_id_solicitud     LIKE ret_fondo_ahorro_generico.id_solicitud

   -- se asume que no hay una solicitud previa
   LET v_existe_solicitud = FALSE

   -- se verifica si el NSS/RFC ya tiene una solicitud en tramite
   LET v_sql = "\nSELECT COUNT(*)               ",
               "\nFROM   ret_solicitud_generico ",
               "\nWHERE  nss = ?                ",
               "\nAND    rfc = ?                ",
               "\nAND    modalidad_retiro = ?   ",
               "\nAND    estado_solicitud ", p_rango_estados
   
   -- se prepara y ejecuta la consulta
   PREPARE sid_buscasol FROM v_sql
   EXECUTE sid_buscasol USING p_nss, p_rfc
      INTO v_conteo
   
   -- si existe al menos una
   IF ( v_conteo > 0 ) THEN
      LET v_existe_solicitud = TRUE
   END IF
   
   -- se devuelve el resultado de la consulta
   RETURN v_existe_solicitud
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_obtener_id_solicitud_generico
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene el id_solicitud de la tabla de solicitudes de retiro
generico para un NSS/RFC y modalidad de retiro dados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_obtener_id_solicitud_generico(p_nss, p_rfc, p_modalidad, p_estado_solicitud)
DEFINE p_nss                      LIKE afi_derechohabiente.nss, 
       p_rfc                      LIKE afi_derechohabiente.rfc,
       p_modalidad                SMALLINT, -- modalidad de retiro
       v_conteo                   SMALLINT, -- num de solicitudes encontradas
       p_estado_solicitud         SMALLINT, -- rango de estados de solicitud
       v_sql                      STRING, -- cadena con consulta SQL
       v_id_solicitud             LIKE ret_solicitud_generico.id_solicitud

   -- se busca la solicitud en el estado indicado en el parametro
   SELECT id_solicitud
   INTO   v_id_solicitud
   FROM   ret_solicitud_generico
   WHERE  nss              = p_nss
   --AND    rfc              = p_rfc
   AND    modalidad_retiro = p_modalidad
   AND    estado_solicitud = p_estado_solicitud
      
   -- se devuelve el id encontrado
   RETURN v_id_solicitud
END FUNCTION


{
======================================================================
Clave: 
Nombre: fn_obtener_id_solicitud_generico
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Busca un ID de solicitud y su estado de un NSS/RFC para una
modalidad de retiro dada y caso adai

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_obtener_id_solicitud_generico_con_estado(p_nss, p_rfc, p_modalidad, p_caso_adai)
DEFINE p_nss                      LIKE afi_derechohabiente.nss, 
       p_rfc                      LIKE afi_derechohabiente.rfc,
       p_modalidad                SMALLINT, -- modalidad de retiro
       p_caso_adai                LIKE ret_solicitud_generico.caso_adai, -- caso adai de la solicitud
       v_estado_solicitud         SMALLINT, -- rango de estados de solicitud
       v_id_solicitud             LIKE ret_solicitud_generico.id_solicitud

   -- se busca la solicitud en el estado indicado en el parametro
   SELECT id_solicitud    ,
          estado_solicitud
   INTO   v_id_solicitud,
          v_estado_solicitud
   FROM   ret_solicitud_generico
   WHERE  nss              = p_nss
   AND    rfc              = p_rfc
   AND    modalidad_retiro = p_modalidad
   AND    caso_adai        = p_caso_adai
   
   -- se devuelve el id encontrado
   RETURN v_id_solicitud, v_estado_solicitud
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_registra_beneficiario_retiro_generico
Fecha creacion: Octubre 07, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera un registro de beneficiario para una solicitud de retiro generico

Registro de modificaciones:
Autor           Fecha        Descrip. cambio
Eneas Armas     20140122     Se agrega pago por SIAF
======================================================================
}
FUNCTION fn_registra_beneficiario_retiro_generico(p_id_solicitud, p_tpo_beneficiario, p_tpo_pago, p_cod_parentesco, p_ap_paterno, p_ap_materno, p_nombre,
                                                  p_telefono, p_correo, p_porcentaje, p_aivs, p_importe, p_clabe, p_referencia_bancaria, p_entidad_federativa)
DEFINE p_id_solicitud                LIKE ret_beneficiario_generico.id_solicitud, -- solicitud de retiro
       p_tpo_beneficiario            LIKE ret_beneficiario_generico.tpo_beneficiario,
       p_tpo_pago                    LIKE ret_beneficiario_generico.tpo_pago,
       p_cod_parentesco              LIKE ret_beneficiario_generico.cod_parentesco,
       p_ap_paterno                  LIKE ret_beneficiario_generico.ap_paterno,
       p_ap_materno                  LIKE ret_beneficiario_generico.ap_materno,
       p_nombre                      LIKE ret_beneficiario_generico.nombre,
       p_telefono                    LIKE ret_beneficiario_generico.telefono,
       p_correo                      LIKE ret_beneficiario_generico.correo,
       p_porcentaje                  LIKE ret_beneficiario_generico.porcentaje,
       p_aivs                        LIKE ret_beneficiario_generico.aivs,
       p_importe                     LIKE ret_beneficiario_generico.importe,
       p_clabe                       CHAR(18), -- clabe interbancaria 
       p_referencia_bancaria         LIKE ret_pago_dap.cve_referencia, -- referencia bancaria
       p_entidad_federativa          CHAR(2), -- clave de entidad federativa COMO LA ENVIA ADAI
       v_sql                         STRING, -- cadena con consulta SQL
       v_r_ret_beneficiario_generico RECORD LIKE ret_beneficiario_generico.*, -- registro de beneficiario
       v_r_ret_pago_spei             RECORD LIKE ret_pago_spei.*, -- registro de pago por spei
       v_r_ret_pago_dap              RECORD LIKE ret_pago_dap.*, -- registro de pago por referencia bancaria
       v_r_ret_pago_siaf              RECORD LIKE ret_pago_siaf.*, -- registro de pago por referencia bancaria
       v_consec_beneficiario         SMALLINT -- consecutivo de beneficiario para la solicitud de retiro

DEFINE v_tipo_benef_str           CHAR(3)

  
   -- se obtiene el consecutivo del beneficiario para la solicitud
   IF p_tpo_beneficiario > 20 THEN 
      LET v_tipo_benef_str = p_tpo_beneficiario
      LET v_consec_beneficiario = v_tipo_benef_str[2,2]
      LET p_tpo_beneficiario = 2
      DISPLAY "El p_tpo_beneficiario generico ",p_tpo_beneficiario
      DISPLAY "El consecutivo generico ",v_consec_beneficiario
      DISPLAY "El parametro generico ",v_tipo_benef_str
      DISPLAY "El parametro recortado generico ",v_tipo_benef_str[2,2]
      
   ELSE 
      SELECT NVL(MAX(consec_beneficiario),0) + 1
      INTO   v_consec_beneficiario
      FROM   ret_beneficiario_generico
      WHERE  id_solicitud = p_id_solicitud
   END IF 
   
   -- se asignan los datos al registro de beneficiario
   LET v_r_ret_beneficiario_generico.id_solicitud          = p_id_solicitud -- decimal(9,0)   NOT NULL,
   LET v_r_ret_beneficiario_generico.consec_beneficiario   = v_consec_beneficiario-- smallint   NOT NULL,
   LET v_r_ret_beneficiario_generico.tpo_beneficiario      = p_tpo_beneficiario-- smallint   NOT NULL,
   LET v_r_ret_beneficiario_generico.tpo_pago              = p_tpo_pago-- smallint   NOT NULL,
   LET v_r_ret_beneficiario_generico.cod_parentesco        = p_cod_parentesco-- smallint   NOT NULL,
   LET v_r_ret_beneficiario_generico.ap_paterno            = p_ap_paterno-- char(40)  ,
   LET v_r_ret_beneficiario_generico.ap_materno            = p_ap_materno-- char(40)  ,
   LET v_r_ret_beneficiario_generico.nombre                = p_nombre-- char(40)  ,
   LET v_r_ret_beneficiario_generico.telefono              = p_telefono-- char(20)  ,
   LET v_r_ret_beneficiario_generico.correo                = p_correo-- char(50)  ,
   LET v_r_ret_beneficiario_generico.porcentaje            = p_porcentaje-- decimal(5,2)  ,
   LET v_r_ret_beneficiario_generico.aivs                  = p_aivs-- decimal(14,6)  ,
   LET v_r_ret_beneficiario_generico.importe               = p_importe-- decimal(14,2)  
   LET v_r_ret_beneficiario_generico.id_entidad_federativa = p_entidad_federativa-- decimal(14,2)  

   -- se inserta el beneficiario
   INSERT INTO ret_beneficiario_generico VALUES ( v_r_ret_beneficiario_generico.* )
   
   
   -- se verifica el tipo de pago
   CASE p_tpo_pago
   WHEN 1
      -- PAGO SPEI
      LET v_r_ret_pago_spei.id_solicitud        = p_id_solicitud
      LET v_r_ret_pago_spei.consec_beneficiario = v_consec_beneficiario
      LET v_r_ret_pago_spei.tpo_pago            = p_tpo_pago
      LET v_r_ret_pago_spei.cod_banco           = 0
      LET v_r_ret_pago_spei.sucursal            = 0
      LET v_r_ret_pago_spei.cuenta_clabe        = p_clabe

      -- se inserta el registro
      INSERT INTO ret_pago_spei VALUES ( v_r_ret_pago_spei.* )
   WHEN 2
      -- PAGO POR DAP
      LET v_r_ret_pago_dap.id_solicitud        = p_id_solicitud -- id de la solicitud
      LET v_r_ret_pago_dap.consec_beneficiario = v_consec_beneficiario -- consecutivo del beneficiario
      LET v_r_ret_pago_dap.tpo_pago            = p_tpo_pago -- tipo de pago
      LET v_r_ret_pago_dap.cve_referencia      = p_referencia_bancaria
  
      -- se inserta el registro
      INSERT INTO ret_pago_dap VALUES ( v_r_ret_pago_dap.* )
   WHEN 3
      -- 20140122     Se agrega pago por SIAF
      LET v_r_ret_pago_siaf.id_solicitud        = p_id_solicitud
      LET v_r_ret_pago_siaf.consec_beneficiario = v_consec_beneficiario
      LET v_r_ret_pago_siaf.cod_banco           = 0
      LET v_r_ret_pago_siaf.sucursal            = 0
      LET v_r_ret_pago_siaf.cuenta_clabe        = p_clabe
  
      -- se inserta el registro
      INSERT INTO ret_pago_siaf VALUES ( v_r_ret_pago_siaf.* )
   END CASE

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_registra_beneficiario_retiro_generico
Fecha creacion: Octubre 07, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera un registro de beneficiario para una solicitud de retiro generico

Registro de modificaciones:
Autor           Fecha        Descrip. cambio
Eneas Armas     20140122     Se agrega pago por SIAF
======================================================================
}
FUNCTION fn_registra_beneficiario_retiro_generico_juridico(p_id_solicitud, p_juicio, p_num_acuerdo, p_desc_juez, 
                                                           p_nombre_facultado, p_puesto_facultado, p_fecha_ejecucion,
                                                           p_procede_juicio, p_tpo_beneficiario)
DEFINE p_id_solicitud                LIKE ret_beneficiario_generico.id_solicitud, -- solicitud de retiro
       p_juicio                      CHAR(15),
       p_num_acuerdo                 CHAR(15),
       p_desc_juez                   CHAR(100),
       p_nombre_facultado            CHAR(40),
       p_puesto_facultado            CHAR(50),
       p_fecha_ejecucion             CHAR(10),
       p_procede_juicio              CHAR(40),
       p_tpo_beneficiario            LIKE ret_beneficiario_generico.tpo_beneficiario,

       v_sql                         STRING, -- cadena con consulta SQL
       v_r_ret_beneficiario_juridico RECORD
          v_id_solicitud                DECIMAL(11,0),
          v_consec_beneficiario         SMALLINT, 
          v_juicio                      CHAR(15),
          v_num_acuerdo                 CHAR(15),
          v_desc_juez                   CHAR(100),
          v_nombre_facultado            CHAR(40),
          v_puesto_facultado            CHAR(50),
          v_fecha_ejecucion             CHAR(10),
          v_procede_juicio              CHAR(40),
          v_estado_solicitud            SMALLINT,
          v_cod_rechazo                 SMALLINT 
         END RECORD, 
          
       v_consec_beneficiario         SMALLINT -- consecutivo de beneficiario para la solicitud de retiro

DEFINE v_tipo_benef_str           CHAR(3)
DEFINE v_consecutivo_beneficiario SMALLINT 
  
   -- se obtiene el consecutivo del beneficiario para la solicitud
   IF p_tpo_beneficiario > 20 THEN 
      LET v_tipo_benef_str = p_tpo_beneficiario
      LET v_consec_beneficiario = v_tipo_benef_str[2,2]
      LET p_tpo_beneficiario = 2
      DISPLAY "El p_tpo_beneficiario juridico ",p_tpo_beneficiario
      DISPLAY "El consecutivo juridico ",v_consec_beneficiario
      DISPLAY "El parametro juridico ",v_tipo_benef_str
      DISPLAY "El parametro recortado juridico ",v_tipo_benef_str[2,2]
   ELSE 
      SELECT NVL(MAX(consec_beneficiario),0)
      INTO   v_consec_beneficiario
      FROM   ret_beneficiario_generico
      WHERE  id_solicitud = p_id_solicitud
   END IF 
   
   -- se asignan los datos al registro de beneficiario
   LET v_r_ret_beneficiario_juridico.v_id_solicitud         = p_id_solicitud 
   LET v_r_ret_beneficiario_juridico.v_consec_beneficiario  = v_consec_beneficiario
   LET v_r_ret_beneficiario_juridico.v_juicio               = p_juicio
   LET v_r_ret_beneficiario_juridico.v_num_acuerdo          = p_num_acuerdo
   LET v_r_ret_beneficiario_juridico.v_desc_juez            = p_desc_juez
   LET v_r_ret_beneficiario_juridico.v_nombre_facultado     = p_nombre_facultado
   LET v_r_ret_beneficiario_juridico.v_puesto_facultado     = p_puesto_facultado
   LET v_r_ret_beneficiario_juridico.v_fecha_ejecucion      = p_fecha_ejecucion
   LET v_r_ret_beneficiario_juridico.v_procede_juicio       = p_procede_juicio
   LET v_r_ret_beneficiario_juridico.v_estado_solicitud     = 10
   LET v_r_ret_beneficiario_juridico.v_cod_rechazo          = 0

   -- se inserta el la información de Juridico por beneficiario
   INSERT INTO ret_beneficiario_juridico VALUES ( v_r_ret_beneficiario_juridico.* )

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_trabajador_pensionado
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un trabajador ya esta pensionado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_trabajador_pensionado(p_nss) 
DEFINE p_nss         LIKE afi_derechohabiente.nss,
       v_id_datamart DECIMAL(9,0),
       v_pension     SMALLINT  

   SELECT COUNT(a.nss)
   INTO   v_id_datamart
   FROM   ret_datamart a
         ,ret_matriz_derecho b
   WHERE a.nss            = p_nss
   AND diag_registro    = 101
   AND b.tpo_retiro     = 'E'
   AND (a.tpo_pension   <> 'IP'
   OR  (a.tpo_pension   = 'IP' AND a.porcentaje_valuacion >= 50))
   AND a.regimen        = b.regimen
   AND a.tpo_seguro     = b.tpo_seguro
   AND a.tpo_pension    = b.tpo_pension
   AND a.tpo_prestacion = b.tpo_prestacion  

   -- si se encontro, esta pensionado
   IF ( v_id_datamart > 0 ) THEN
      LET  v_pension = TRUE
   ELSE 
      LET  v_pension = FALSE
   END IF   
    
   -- se devuelve el resultado de la consulta
   RETURN v_pension
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_trabajador_resolucion_spess
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si el trabajador tiene una resolucion en el SPESS

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     27 Nov 2013             - La busqueda en el SPESS para Ley73 se hace con el regimen
                                        73. Es forzosa esa condicion de acuerdo a reunion en 
                                        infonavit del 26 de nov 2013.
Ivan Vega     13dic2013               - En Causal Pension del IMSS, no se valida diagnostico 101
                                        En Ley 73, no se valida diagnostico 101
Ivan Vega     16Dic2013               - Se solicita agregar Retiro Programado (RP) a la lista
                                        de tipo de pension validos en Fondo de Ahorro causal
                                        Pension del IMSS
Ivan Vega     24Feb2014               - se cambian las reglas de validacion del SPESS para causales de fondo de ahorro
                                        segun indicaciones de Benjamin Rodriguez de Infonavit. Correo del
										21feb2014.
Termino de relación laboral
  Se debe de considerar, que las resoluciones positivas son todas aquellas 
  que en el campo tipo de prestación, son diferentes de 03 negativa de pensión y en los casos en los que el 
  campo tipo de pensión corresponda  a IP - Incapacidad Permanente, el porcentaje deberá de ser igual o mayor al 50%.
  
  ADICIONALMENTE, POR CORREO DE PERLA PEREZ 13 DE MARZO 2014
  Regla: Para TODOS los casos donde el solicitante se perfile en la solicitud como recuperación 
  de recursos del Fondo de Ahorro por TRL, el sistema deberá hacer la consulta al SPES por NSS, 
  en caso de ubicar una pensión procedente, de forma automática deberá generar el pago con tanto adicional,
  en caso contrario continuar con el proceso de pago por el total de lo acumulado en SACI 
  bajo el concepto del Fondo de Ahorro.

Pensión del IMSS
  Se debe de considerar, que las resoluciones positivas son todas aquellas que en el campo 
  tipo de prestación son diferentes de 03 negativa de pensión y en los casos en los que el campo 
  tipo de pensión corresponda  a IP - Incapacidad Permanente, el porcentaje deberá de ser igual 
  o mayor al 50%.

Plan privado de pension:
  Solo se valida el catálogo de planes privados de pensión.

Defunción:
  Tiene que cumplir que el campo Tipo de pensión, corresponda a OR, VI, VO, AS
======================================================================
}
FUNCTION fn_trabajador_resolucion_spess(p_nss, p_causal)
DEFINE  v_tiene_resolucion     SMALLINT, -- booleana que indica si el trabajador tiene resolucion en spess
        p_nss                  LIKE afi_derechohabiente.nss, -- nss del trabajador
        p_causal               SMALLINT, -- causal de retiro
        v_sec_pension          LIKE ret_datamart.sec_pension, -- max secuencia de pension
        v_diag_registro        LIKE ret_datamart.diag_registro, -- diagnostico del registro
        v_id_datamart          LIKE ret_datamart.id_datamart, -- clave de la tabla del datamart
		v_tipo_pension         LIKE ret_datamart.tpo_pension, -- tipo de pension
		v_porcentaje_valuacion LIKE ret_datamart.porcentaje_valuacion, -- porcentaje de valuacion
        v_sql                  STRING
   
   -- se asume que el trabajador no tiene resolucion
   LET v_tiene_resolucion = FALSE
   
   DISPLAY "Validando causal: ", p_causal
   
   CASE p_causal

      -- termino de relacion laboral
      -- se elimina el cruce contra la matriz de derechos, ya que esta no aplica para fondo de ahorro
      WHEN 1
         --LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension), a.id_datamart, a.tpo_pension, a.porcentaje_valuacion",
                     --"\nFROM  ret_datamart a,                     ",
                     --"\n      ret_matriz_derecho b                ",
                     --"\nWHERE a.nss            = ?                ", -- el trabajador
                     --"\nAND   a.regimen        = b.regimen        ", -- condiciones del retiro validas
                     --"\nAND   a.tpo_seguro     = b.tpo_seguro     ", 
                     --"\nAND   a.tpo_pension    = b.tpo_pension    ", 
                     --"\nAND   a.tpo_prestacion = b.tpo_prestacion ",
                     --"\nAND   a.tpo_prestacion <> '03'            ", -- dif de negativa de pension
                     --"\nGROUP BY a.id_datamart, a.tpo_pension, a.porcentaje_valuacion   ", 
                     --"\nORDER BY a.id_datamart DESC               " 
         LET v_sql = "\nSELECT MAX(a.sec_pension)                 ",
                     "\nFROM  ret_datamart a                      ",
                     "\nWHERE a.nss            = ?                ", -- el trabajador
                     "\nAND   a.tpo_prestacion <> '03'            " 
         
         PREPARE sid_rellab FROM v_sql
         EXECUTE sid_rellab USING p_nss INTO v_sec_pension
         IF v_sec_pension IS NOT NULL THEN 
             LET v_sql = "\nSELECT FIRST 1 a.id_datamart, a.tpo_pension, a.porcentaje_valuacion",
                         "\nFROM  ret_datamart a                      ",
                         "\nWHERE a.nss            =                  ", p_nss,  -- el trabajador
                         "\nAND   a.tpo_prestacion <> '03'            ", 
                         "\nAND   a.sec_pension    =                  ", v_sec_pension
             
             PREPARE sid_datamartl FROM v_sql
             EXECUTE sid_datamartl INTO v_id_datamart, v_tipo_pension, v_porcentaje_valuacion
         END IF 
         -- si se encontro un registro
         IF ( v_id_datamart IS NOT NULL ) THEN
		    -- si es tipo de pension IP, debe tener porcentaje valuacion
		    IF ( v_tipo_pension = "IP" ) THEN
			   IF ( v_porcentaje_valuacion >= 50 ) THEN
			      -- resolucion valida
			      LET v_tiene_resolucion = TRUE
			   ELSE
			      -- no tiene resolucion valida
			      LET v_tiene_resolucion = FALSE
			   END IF
			ELSE
			   -- es resolucion valida
			   LET v_tiene_resolucion = TRUE
			END IF
         END IF
     
      -- Resolucion de pension otorgada por el IMSS
      WHEN 2
         --LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension), a.id_datamart, a.tpo_pension, a.porcentaje_valuacion",
                     --"\nFROM  ret_datamart a,                     ",
                     --"\n      ret_matriz_derecho b                ",
                     --"\nWHERE a.nss            = ?                ", -- el trabajador
                     --"\nAND   a.regimen        = b.regimen        ", -- condiciones del retiro validas
                     --"\nAND   a.tpo_seguro     = b.tpo_seguro     ", 
                     --"\nAND   a.tpo_pension    = b.tpo_pension    ", 
                     --"\nAND   a.tpo_prestacion = b.tpo_prestacion ",
					 --"\nAND   a.tpo_prestacion <> '03'            ", -- dif de negativa de pension
                     --"\nGROUP BY a.id_datamart, a.tpo_pension, a.porcentaje_valuacion   ", 
                     --"\nORDER BY a.id_datamart DESC                       " 
         LET v_sql = "\nSELECT MAX(a.sec_pension)                 ",
                     "\nFROM  ret_datamart a                      ",
                     "\nWHERE a.nss            = ?                ", -- el trabajador
					 "\nAND   a.tpo_prestacion <> '03'            "
         
         PREPARE sid_pensionimss FROM v_sql
         EXECUTE sid_pensionimss USING p_nss INTO v_sec_pension

         IF v_sec_pension IS NOT NULL THEN
             LET v_sql = "\nSELECT FIRST 1 a.id_datamart, a.tpo_pension, a.porcentaje_valuacion",
                         "\nFROM  ret_datamart a                      ",
                         "\nWHERE a.nss            =                  ", p_nss,  -- el trabajador
                         "\nAND   a.tpo_prestacion <> '03'            ",
                         "\nAND   a.sec_pension = ", v_sec_pension
             
             PREPARE sid_datamart FROM v_sql
             EXECUTE sid_datamart INTO v_id_datamart, v_tipo_pension, v_porcentaje_valuacion
         END IF 

         -- si se encontro un registro
         IF ( v_id_datamart IS NOT NULL ) THEN
		    -- si es tipo de pension IP, debe tener porcentaje valuacion
		    IF ( v_tipo_pension = "IP" ) THEN
			   IF ( v_porcentaje_valuacion >= 50 ) THEN
			      -- resolucion valida
			      LET v_tiene_resolucion = TRUE
			   ELSE
			      -- no tiene resolucion valida
			      LET v_tiene_resolucion = FALSE
			   END IF
			ELSE
			   -- es resolucion valida
			   LET v_tiene_resolucion = TRUE
			END IF
         END IF
              
      -- plan privado de pension
      WHEN 3
	     -- 24feb2014. No requiere ser validado, se asume que se tiene
		 LET v_tiene_resolucion = TRUE
         {		 
         SELECT MAX(a.sec_pension), a.id_datamart
         INTO   v_sec_pension, v_id_datamart
         FROM   ret_datamart a
               ,ret_matriz_derecho b
         WHERE a.nss = p_nss
         AND   diag_registro    = 101
         AND   b.tpo_retiro     = 'F'
         AND   TODAY BETWEEN f_inicio_pension AND f_inicio_pension  + 10 UNITS YEAR  
         AND   a.regimen        = b.regimen
         AND   a.tpo_seguro     = 'PP'
         AND   a.tpo_pension    = b.tpo_pension
         AND   a.tpo_prestacion = b.tpo_prestacion
         GROUP BY a.id_datamart
     
         -- si tiene una secuencia de pension, tiene resolucion de pension del imss
         IF ( v_sec_pension IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
         END IF
		 }
     
      WHEN 4
         --SELECT MAX(a.sec_pension)
         --INTO    v_sec_pension
         --FROM    ret_datamart a
                --,ret_matriz_derecho b
         --WHERE  a.nss = p_nss
         --AND    a.regimen        = b.regimen
         --AND    a.tpo_seguro     = b.tpo_seguro
         --AND    a.tpo_pension    IN ('OR','VI','VO','AS')
         --AND    a.tpo_prestacion = b.tpo_prestacion
         SELECT MAX(a.sec_pension)
         INTO    v_sec_pension
         FROM    ret_datamart a
         WHERE  a.nss = p_nss
         AND    a.tpo_pension    IN ('OR','VI','VO','AS')
         -- si tiene una secuencia de pension, tiene resolucion de pension que corresponde con una defuncion
         IF ( v_sec_pension IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
            SELECT  MAX(a.id_datamart)
            INTO    v_id_datamart
            FROM    ret_datamart a
            WHERE  a.nss = p_nss
            AND    a.tpo_pension    IN ('OR','VI','VO','AS')
            AND    a.sec_pension    = v_sec_pension
         END IF
     
      -- ====================================================================================
      -- BUSQUEDA DE RESOLUCION EN EL SPESS PARA LEY 73
      WHEN 5
         -- Resolucion del spess para ley 73
         --LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension)",
                     --"\nFROM  ret_datamart a                            ",
                     --"\n      ,ret_matriz_derecho b                     ",
                     --"\nWHERE a.nss            = ?                      ", -- el trabajador
                     --"\nAND   a.regimen        = 73                     ", -- el regimen es forzoso que sea Ley73
                     --"\nAND   b.tpo_retiro     = 'E'                    ", -- retiro avalado por el imss
                     --"\nAND   a.regimen        = b.regimen              ",                      
                     --"\nAND   a.tpo_seguro     = b.tpo_seguro           ", 
                     --"\nAND   a.tpo_pension    = b.tpo_pension          ", 
                     --"\nAND   a.tpo_prestacion = b.tpo_prestacion       "
         --
         --PREPARE sid_ley73 FROM v_sql
         --EXECUTE sid_ley73 USING p_nss INTO v_sec_pension
         --IF v_sec_pension IS NOT NULL THEN 
             --LET v_tiene_resolucion = TRUE
             --SELECT MAX(a.id_datamart)
             --INTO   v_id_datamart
             --FROM   ret_datamart a                            
             --WHERE  a.nss            = p_nss                     
             --AND    a.regimen        = 73                     
             --AND    a.sec_pension    = v_sec_pension
         --END IF 
             -- Resolucion del spess para ley 73
         LET v_sql = "\nSELECT MAX(a.sec_pension)",
                     "\nFROM  ret_datamart a                            ",
                     "\nWHERE a.nss            = ?                      " -- el trabajador
         
         PREPARE sid_ley73 FROM v_sql
         EXECUTE sid_ley73 USING p_nss INTO v_sec_pension
         IF v_sec_pension IS NOT NULL THEN 
             LET v_tiene_resolucion = TRUE
             SELECT MAX(a.id_datamart)
             INTO   v_id_datamart
             FROM   ret_datamart a                            
             WHERE  a.nss            = p_nss                     
             AND    a.sec_pension    = v_sec_pension
         END IF 
     
   END CASE
   
   -- se devuelve el resultado de la consulta
   RETURN v_tiene_resolucion, v_id_datamart
END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_verifica_ano_sin_relacion_laboral
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un trabajador tiene un ano sin relacion laboral

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_verifica_ano_sin_relacion_laboral(p_fecha_relacion_laboral, p_fecha_comparacion)
DEFINE p_fecha_relacion_laboral DATE,
       p_fecha_comparacion      DATE,
       v_diferencia             SMALLINT,
       v_ano_sin_relacion       SMALLINT -- TRUE lo tiene, FALSE no lo tiene

   DISPLAY "valida el tiempo sin actividd laboral del derechohabiente"

   -- se asume que tiene un ano sin relacion laboral
   LET v_ano_sin_relacion = TRUE

   --SELECT v_fecha_comparacion - p_fecha_relacion_laboral
   --  INTO v_diferencia
   --  FROM systables
   -- WHERE tabid = 1
    
   LET v_diferencia = p_fecha_comparacion - p_fecha_relacion_laboral

   -- si no hay diferencia de un ano entre las fechas
   IF ( v_diferencia > 365 ) THEN
      LET v_ano_sin_relacion = TRUE
   ELSE
      LET v_ano_sin_relacion = FALSE
   END IF
  
   -- se devuelve el resultado de la consulta
   RETURN v_ano_sin_relacion
END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_trabajador_credito_vigente
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente tiene un credito vigente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_trabajador_credito_vigente(p_nss)
DEFINE p_nss                LIKE afi_derechohabiente.nss,
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_id_credito         LIKE afi_derechohabiente.id_credito, -- indicador de credito vigente. 1 con credito. 0 sin credito
       v_conteo             SMALLINT, -- contador
       v_tiene_credito      SMALLINT, -- booleana que indica si existe credito
       v_tipo_credito       SMALLINT -- tipo de credito

   -- se asume que no tiene credito
   LET v_tiene_credito = FALSE
  
   CALL fn_ret_ley73_credito_vigente(p_nss, NULL) RETURNING v_tiene_credito, v_tipo_credito
   
   -- se devuelve el resultado de la consulta
   RETURN v_tiene_credito
END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_trabajador_tipo_credito
Fecha creacion: Agosto 22, 2013
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Verifica que tipo de credito tiene el trabajador

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_trabajador_tipo_credito(p_nss)
DEFINE p_nss                    LIKE afi_derechohabiente.nss,
       v_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente, -- id_der del NSS
       v_valida_credito_vigente SMALLINT, -- indica que se valide si existe credito vigente
       v_vigente                SMALLINT,
       r_resultado              SMALLINT, 
       r_tpo_originacion        SMALLINT, 
       r_tpo_credito            SMALLINT, 
       r_num_credito            DECIMAL(10,0), 
       r_f_otorga               DATE,
       r_f_liquida              DATE

   -- se busca credito vigente
   LET v_valida_credito_vigente = 0
   LET r_tpo_originacion        = 0

   -- se obtiene el id_derechohabiente
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss = p_nss

   -- se asume que el derechohabiente tiene un credito vigente
   LET v_vigente = TRUE
   
   -- se consulta si el derechohauiente tiene un credito
   CALL fn_credito_vivienda(v_id_derechohabiente, v_valida_credito_vigente) RETURNING r_resultado      ,
                                                                                      r_tpo_originacion,
                                                                                      r_tpo_credito    ,
                                                                                      r_num_credito    ,
                                                                                      r_f_otorga       ,
                                                                                      r_f_liquida

   -- se verifica si tiene credito
   IF ( r_resultado = 0 ) THEN
      LET v_vigente = TRUE
   ELSE
      -- no tiene credito o ya se liquido
      LET v_vigente = FALSE
   END IF
       
   -- se devuelve el resultado de la consulta
   RETURN r_tpo_credito, r_tpo_originacion
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_recupera_saldo_fa
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene el saldo de fondo de ahorro de un trabajador identificado
por su NSS para los n-afi_fondo72 que obtenga

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recupera_saldo_fa(p_nss, p_rfc)
DEFINE p_nss            LIKE afi_derechohabiente.nss, -- nss del trabajador
       p_rfc            LIKE afi_derechohabiente.rfc, -- nss del trabajador
       v_saldo_pesos    DECIMAL(22,2) -- suma del saldo del trabajador

   -- se inicia el acumulador
   LET v_saldo_pesos  = 0 

   SELECT SUM(a.importe)
   INTO   v_saldo_pesos
   FROM   cta_fondo72 a,
          afi_fondo72 b
   WHERE  b.nss               = p_nss -- el trabajador
   AND    b.ind_estado_cuenta = 0     -- Cuenta Activa
--   AND    b.rfc = p_rfc
   AND    a.id_afi_fondo72 = b.id_afi_fondo72 -- coinddencias en la tabla de movimientos

   IF ( v_saldo_pesos <= 0 OR v_saldo_pesos IS NULL ) THEN
      -- el saldo se considera cero
      LET v_saldo_pesos = 0
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_saldo_pesos
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_edad_derechohabiente
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene la edad cumplida de un derechohabiente registrado en afi_fondo72
identificado por su id_afi_fondo72

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_edad_derechohabiente(p_nss)
DEFINE p_nss            LIKE afi_derechohabiente.nss, -- nss del trabajador
       p_id_afi_fondo72 DECIMAL(9,0), -- id afi del trabajador
       v_f_nac_rfc      CHAR(13),
       v_f_year         CHAR(2),
       v_f_month        CHAR(2),
       v_f_day          CHAR(2),
       v_f_year_today   CHAR(4),
       v_f_nac_bueno    DECIMAL(4,0),
       v_f_nac          DATE,
       v_edad           SMALLINT,
       v_rfc            LIKE afi_fondo72.rfc, -- rfc del trabajador
       v_sql            STRING -- cadena con consulta sql

   -- se obtiene el id_afi_fondo72 del trabajador
   LET v_sql = "\nSELECT FIRST 1 id_afi_fondo72",
               "\nFROM   afi_fondo72",
               "\nWHERE  nss = ? ",
               "\nAND    ind_estado_cuenta = 0 "
               
   -- se ejecuta la consulta
   PREPARE sid_obtener_id_afi FROM v_sql
   EXECUTE sid_obtener_id_afi INTO p_id_afi_fondo72 USING p_nss

   -- se recupera el rfc del derechohabiente
   SELECT rfc
   INTO   v_rfc
   FROM   afi_fondo72
   WHERE  id_afi_fondo72 = p_id_afi_fondo72

   -- ====================================================
   -- se calcula la edad a partir del RFC
   
   -- si no se encontro el rfc, no se puede calcular la edad
   IF ( v_rfc = 0 OR v_rfc IS NULL ) THEN
      LET v_f_nac  = -1
   ELSE
      -- se obtienen las particulas de la fecha de nacimiento
      LET v_f_year  = v_rfc [5,6]
      LET v_f_month = v_rfc [7,8]
      LET v_f_day   = v_rfc [9,10]
 
      -- se obtiene el ano de la fecha del dia
      LET v_f_year_today = YEAR(TODAY)
     
      -- se valida si la fecha es de los anos 1900
      IF ( v_f_year_today[3,4] < v_f_year ) THEN
         LET v_f_nac_bueno = v_f_year_today[1,2] - 1 UNITS YEAR || v_f_year
         --DISPLAY v_f_nac_bueno
      ELSE
         -- es de los anos 2000 en adelante
         LET v_f_nac_bueno =  v_f_year_today[1,2] || v_f_year
         --DISPLAY v_f_nac_bueno
      END IF 
    
      -- la fecha de nacimiento se crea a partir del ano calculado
      LET v_f_nac = MDY(v_f_month,v_f_day,v_f_nac_bueno)    

      -- se valida qe la fecha sea correcta si no lo es se rechaza solicitud por edad
      IF ( v_f_nac = 0 ) THEN
        LET v_f_nac = -1
      ELSE
        -- se calcula la edad del trabajador  
        -- si coincide el dia y el mes de nacimiento con el dia y mes de la fecha del dia
--        IF ( MONTH(TODAY) >= MONTH(v_f_nac) AND DAY(TODAY) >= DAY(v_f_nac) ) THEN 
--           LET v_edad = YEAR(TODAY) - YEAR(v_f_nac)
--        ELSE
           -- como es una fecha anterior al cumpleanos, no se cuenta el ano corriente por no ser ano cumplido
--           LET v_edad = (YEAR(TODAY) - YEAR(v_f_nac)) - 1
--        END IF
        -- Se modifica el cálculo de los años cumplidos pendiente el Jira para respaldo de esta petición
        LET v_edad = YEAR(TODAY - v_f_nac) - 1900 
      END IF 
   END IF 
   
   -- se devuelve la edad calculada
   RETURN v_edad
END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_edad_derechohabiente_rfc
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene la edad cumplida de un derechohabiente mediante su RFC

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_edad_derechohabiente_rfc(p_rfc)
DEFINE p_rfc            LIKE afi_derechohabiente.rfc, -- rfc del trabajador
       p_id_afi_fondo72 DECIMAL(9,0), -- id afi del trabajador
       v_f_nac_rfc      CHAR(13),
       v_f_year         CHAR(2),
       v_f_month        CHAR(2),
       v_f_day          CHAR(2),
       v_f_year_today   CHAR(4),
       v_f_nac_bueno    DECIMAL(4,0),
       v_f_nac          DATE,
       v_edad           SMALLINT,
       v_rfc            LIKE afi_fondo72.rfc, -- rfc del trabajador
       v_sql            STRING -- cadena con consulta sql

   -- ====================================================
   -- se calcula la edad a partir del RFC
   
   -- si no se encontro el rfc, no se puede calcular la edad
   IF ( p_rfc = 0 OR p_rfc IS NULL ) THEN
      LET v_f_nac  = -1
   ELSE
      -- se obtienen las particulas de la fecha de nacimiento
      LET v_f_year  = p_rfc [5,6]
      LET v_f_month = p_rfc [7,8]
      LET v_f_day   = p_rfc [9,10]
 
      -- se obtiene el ano de la fecha del dia
      LET v_f_year_today = YEAR(TODAY)

      { -- ISVH 19 jul 2013
      -- se valida si la fecha es de los anos 1900
      IF ( v_f_year_today[3,4] < v_f_year ) THEN
         LET v_f_nac_bueno = v_f_year_today[1,2] - 1 UNITS YEAR || v_f_year
         --DISPLAY v_f_nac_bueno
      ELSE
         -- es de los anos 2000 en adelante
         LET v_f_nac_bueno =  v_f_year_today[1,2] || v_f_year
         --DISPLAY v_f_nac_bueno
      END IF 
      }

      -- ISVH 19 Julio 2013
      -- como es fondo de ahorro, ningun trabajador nacido en la decada de los 2000 puede estar registrado
      -- por lo tanto, todas las fechas se asumen como de los anos 1900
      LET v_f_nac_bueno = v_f_year_today[1,2] - 1 UNITS YEAR || v_f_year
    
      -- la fecha de nacimiento se crea a partir del ano calculado
      LET v_f_nac = MDY(v_f_month,v_f_day,v_f_nac_bueno)    

      -- se valida qe la fecha sea correcta si no lo es se rechaza solicitud por edad
      IF ( v_f_nac = 0 ) THEN
        LET v_f_nac = -1
      ELSE
        -- se calcula la edad del trabajador  
        -- si coincide el dia y el mes de nacimiento con el dia y mes de la fecha del dia
        IF ( MONTH(TODAY) >= MONTH(v_f_nac) AND DAY(TODAY) >= DAY(v_f_nac) ) THEN 
           LET v_edad = YEAR(TODAY) - YEAR(v_f_nac)
        ELSE
           -- como es una fecha anterior al cumpleanos, no se cuenta el ano corriente por no ser ano cumplido
           LET v_edad = (YEAR(TODAY) - YEAR(v_f_nac)) - 1
        END IF 
      END IF 
   END IF 
   
   -- se devuelve la edad calculada
   RETURN v_edad
END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_genera_referencia_bancaria
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Crea la referencia bancaria para un NSS-RFC y una causal de retiro
dados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     21mar2014               - Se asigna la causal de pago segun definicion de Noe Martinez Infonavit
                                        en correo del 26 de feb de 2014
                                        Causal de Pago:
                                        1 - Cesantía
                                        2-  Vejez
                                        3 - Plan Privado de Pensión
                                        4-  Incapacidad Permanente Total
                                        5-  Incapacidad Permanente Parcial > 50%
                                        6-  Invalidez
                                        7 - Defunción
                                        8 - Terminación Laboral
======================================================================
}
FUNCTION fn_genera_referencia_bancaria(p_causal_retiro, p_tpo_pension)
DEFINE p_causal_retiro    SMALLINT,
       p_tpo_pension      LIKE ret_matriz_derecho.tpo_pension,
       v_referencia       CHAR(12)    ,
       v_concepto_pago    CHAR(2), -- 1-2	Siempre 50	Concepto de pago para FICO (Tesorería)
       v_ano_solicitud    CHAR(2), -- 3-4	Año 	Año en que se solicita la Devolución de Fondo de Ahorro 
       v_ref_fondo_ahorro CHAR(2), -- 5-6	Siempre 72	Referencia a que se paga Fondo de Ahorro 72-92 
       v_causal_pago      CHAR(1), -- 7	Causal del pago (va del 1 al 9)
       v_num_consecutivo  CHAR(5), -- 8-12	Numero consecutivo	Se toma de una tabla
       v_consecutivo      DECIMAL(5,0), -- numero consecutivo 
       v_ano_fecha_dia    CHAR(4)  -- para extrar las unidades y decenas del ano

   -- se asingnan los datos de la referencia
   LET v_concepto_pago    = "50"
   LET v_ano_fecha_dia    = YEAR(TODAY)
   LET v_ano_solicitud    = v_ano_fecha_dia[3,4]
   LET v_ref_fondo_ahorro = "72"
   
   -- para el campo de causal de pago, se revisa cual es la causal de retiro
   { valores segun correo de Noe Martinez
      1 - Cesantía
      2-  Vejez
      3 - Plan Privado de Pensión
      4-  Incapacidad Permanente Total
      5-  Incapacidad Permanente Parcial > 50%
      6-  Invalidez
      7 - Defunción
      8 - Terminación Laboral
   }
   CASE p_causal_retiro
      WHEN 1 -- Termino de relacion laboral
         LET v_causal_pago = "8"
      
      WHEN 2 -- pension del imss
         -- se verifica el tipo de pension
         CASE 
            WHEN p_tpo_pension = "CE" -- Cesantia
               LET v_causal_pago = "1"

            WHEN p_tpo_pension = "VE" -- Vejez
               LET v_causal_pago = "2"

            WHEN p_tpo_pension = "IP" -- Incapacidad permanente total
               LET v_causal_pago = "4"
               
            WHEN p_tpo_pension = "ip" -- Incapacidad permanente parcial > 50% [ SE ENVIA EN MINUSCULAS PARA DIFERENCIARLO]
               LET v_causal_pago = "5"
               
            WHEN p_tpo_pension = "IN" -- invalidez
               LET v_causal_pago = "6"
            WHEN (p_tpo_pension = "VI" OR p_tpo_pension = "AS" OR p_tpo_pension = "OR" OR p_tpo_pension = "VO")
               LET v_causal_pago = "7"
            WHEN p_tpo_pension = "TR" 
               LET v_causal_pago = "8"
         END CASE
      
      WHEN 3 -- Plan privado pension
         LET v_causal_pago = "3"
      
      WHEN 4 -- defuncion
         LET v_causal_pago = "7"
     
   END CASE
   
   -- se obtiene el numero consecutivo
   SELECT seq_ret_ref_bancaria72.nextVal
   INTO   v_consecutivo
   FROM   systables
   WHERE  tabid = 1

   -- se formatea el numero a 5 cifras
   LET v_num_consecutivo = v_consecutivo USING "&&&&&"

   -- se concatenan los componentes de la referencia
   LET v_referencia = v_concepto_pago, v_ano_solicitud, v_ref_fondo_ahorro,
                      v_causal_pago, v_num_consecutivo
   
   -- se devuelve la referencia
   RETURN v_referencia
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_rechazo_por_tramite_fondo_ahorro
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
valida que no exista otra solicitd en proceso de tramite 
pendiente de algun proceso con el mismo nss

Registro de modificaciones:
Autor         Fecha        Descrip. cambio
Eneas Armas   20140122     Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
              20140122     Se cambia la tabla ret_ley73 por ret_ley73_generico
======================================================================
}
FUNCTION fn_rechazo_por_tramite_fondo_ahorro(p_nss, p_rfc)
DEFINE p_nss            LIKE afi_derechohabiente.nss, -- NSS del derechohabiente
       p_rfc            LIKE afi_derechohabiente.rfc, -- RFC del derechohabiente
       v_id_afi_fondo72 LIKE afi_fondo72.id_afi_fondo72,
       v_conteo         SMALLINT,
       v_en_tramite     SMALLINT, -- booleana que indica si se tiene una solicitud en tramite
       v_cadena         STRING

   -- se asume que no hay solicitud en tramite 
   LET v_en_tramite = FALSE
   LET v_conteo = 0

   -- se busca el id_afi_fondo72 del nss
   SELECT MAX(id_afi_fondo72)
   INTO   v_id_afi_fondo72
   FROM   afi_fondo72
   WHERE  nss = p_nss
   -- busca al derechohabiente en la tabla ret_fondo_ahorro con estatus en tramite
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_fondo_ahorro_generico
   WHERE  id_derechohabiente = v_id_afi_fondo72
   AND    estado_solicitud IN (8, 10, 15, 50, 60, 70, 700, 71,
                               90, 209, 210, 211, 212)
   -- si lo encuentra
   IF ( v_conteo > 0 ) THEN
      LET v_en_tramite = TRUE
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_en_tramite
END FUNCTION  

{
======================================================================
Nombre: fn_prescripcion_fondo_ahorro
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica el numero de anos de la prescripcion de un trabajador

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_prescripcion_fondo_ahorro()

   -- se devuelven 5 en pruebas
   RETURN 5
END FUNCTION


{
======================================================================
Nombre: fn_nrp_existe_en_catalogo
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si una clave de NRP existe en el catalogo

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_nrp_existe_en_catalogo(p_nrp)
DEFINE p_nrp         LIKE afi_relacion_laboral.nrp, -- NRP del empleador
       v_nrp_buscado LIKE afi_relacion_laboral.nrp, -- NRP del empleador buscado
       v_existe SMALLINT -- booleana que indica si el NRP existe

   -- se asume que el NRP existe
   LET v_existe = TRUE

   SELECT nrp
   INTO   v_nrp_buscado
   FROM   ret_cat_nrp
   WHERE  nrp = p_nrp
   
   -- si no se encuentra
   IF ( v_nrp_buscado IS NULL ) THEN
      -- no esta en el catalogo
      LET v_existe = FALSE
   END IF
   
   -- se devuelve el resultado de la consulta
   RETURN v_existe
END FUNCTION

{
======================================================================
Nombre: fn_anos_FIP
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene el numnero de anos del FIP

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_anos_FIP()

   -- se devuelve 3 para que funcione
   RETURN 3
END FUNCTION





-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

--          F U N C I O N E S   P A R A   R E T I R O   L E Y   7 3

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{
======================================================================
Nombre: fn_ret_ley73_credito_vigente
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
 Verifica si el trabajador tiene un credito vigente o en tramite e indica
 de que tipo es si lo tiene
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_ley73_credito_vigente(p_nss, p_grupo)
DEFINE p_nss                    LIKE afi_derechohabiente.nss,
       p_grupo                  SMALLINT,
       v_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente, -- id_der del NSS
       v_valida_credito_vigente SMALLINT, -- indica que se valide si existe credito vigente
       v_vigente                SMALLINT,
       r_resultado              SMALLINT, 
       r_tpo_originacion        SMALLINT, 
       r_tpo_credito            SMALLINT, 
       r_num_credito            DECIMAL(10,0), 
       r_f_otorga               DATE,
       r_f_liquida              DATE

   -- se busca credito vigente
   LET v_valida_credito_vigente = 0

   -- se obtiene el id_derechohabiente
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss = p_nss

   -- se asume que el derechohabiente tiene un credito vigente
   LET v_vigente = TRUE
   
   -- se consulta si el derechohauiente tiene un credito
   CALL fn_credito_vivienda(v_id_derechohabiente, v_valida_credito_vigente) RETURNING r_resultado      ,
                                                                                      r_tpo_originacion,
                                                                                      r_tpo_credito    ,
                                                                                      r_num_credito    ,
                                                                                      r_f_otorga       ,
                                                                                      r_f_liquida

   -- se verifica si tiene credito
   IF ( r_resultado = 0 ) THEN
      LET v_vigente = TRUE
   ELSE
      -- no tiene credito o ya se liquido
      LET v_vigente = FALSE
   END IF
       
   -- se devuelve el resultado de la consulta
   RETURN v_vigente, r_tpo_credito
END FUNCTION

{
======================================================================
Nombre: fn_verifica_tipo_credito_43bis
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
  Verifica si un tipo de credito es 43BIS

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_tipo_credito_43bis(p_tipo_credito)
DEFINE p_tipo_credito SMALLINT, -- clave del tipo de credito
       v_es_43bis     SMALLINT  -- booleana que indica si el credito es 43bis

   -- se asume que es no 43bis
   LET v_es_43bis = FALSE

   -- 43bis es tipo 2
   IF ( p_tipo_credito = 2 ) THEN
      LET v_es_43bis = TRUE
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_es_43bis 
END FUNCTION

{
======================================================================
Nombre: fn_nss_tuvo_retiro
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si el NSS en cuestion ha tenido un retiro o esta en un proceso
de devolucion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_nss_tuvo_retiro(p_nss)
DEFINE p_nss CHAR(11),
       p_grupo SMALLINT,
       v_tuvo_retiro SMALLINT
       
   -- se asume que tuvo retiro
   LET v_tuvo_retiro = FALSE
   
   
   -- se devuelve el resultado de la consulta
   RETURN v_tuvo_retiro
END FUNCTION

{
======================================================================
Nombre: fn_verifica_transferencia_tipo_b
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
  Valida si la solicitudo ha tenido transferencia de tipo B

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     13mar2014              - El monto disponible en anexo 1 se registro como
                                       una subcuenta con un abono y cargos iniciales de
									   los montos historicos transferidos y devueltos
									   La funcion ahora busca el saldo con la funcion de saldos
======================================================================
}
FUNCTION fn_verifica_transferencia_tipo_b(p_id_derechohabiente)
DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente, -- id derechohabiente del NSS consultado
       v_pesos_viv97_transf  LIKE ret_his_anexo1.pesos_viv97_transf, -- monto transferico
       v_tiene_transferencia SMALLINT -- booleana que indica si tuvo transferncia
   
   -- se asume que tuvo la transferencia
   LET v_tiene_transferencia = TRUE

   -- se obtiene el registro del id_derechohabiente
   SELECT SUM(monto_pesos)
   INTO   v_pesos_viv97_transf
   FROM   cta_movimiento
   WHERE  id_derechohabiente = p_id_derechohabiente
   AND    subcuenta          = 47 -- TESOFE
   AND    fondo_inversion    = 10 -- solo pesos

   -- si no se encuentra monto transferido, no tuvo transferencia
   IF ( v_pesos_viv97_transf IS NULL ) THEN
      LET v_tiene_transferencia = FALSE
	  LET v_pesos_viv97_transf  = 0
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_tiene_transferencia, v_pesos_viv97_transf
END FUNCTION

{
======================================================================
Nombre: fn_calcula_saldo_ley73
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_calcula_saldo_ley73(p_nss, p_subcuenta, p_f_valuacion)
DEFINE p_nss                LIKE afi_derechohabiente.nss,
       p_subcuenta          SMALLINT,
       p_f_valuacion        DATE, -- fecha de valuacion
       v_saldo_aivs         DECIMAL(24,6),
       v_saldo_pesos        DECIMAL(22,2), 
       v_resultado_consulta SMALLINT,
       v_sql                STRING

   -- se ejecuta el SP de consulta de saldo
   LET v_sql = "EXECUTE FUNCTION fn_saldo_dia(?,NULL,?,?)"
   
   -- se ejecuta la consulta de saldo
   PREPARE sid_saldo FROM v_sql
   EXECUTE sid_saldo USING p_nss, p_subcuenta, p_f_valuacion
                     INTO v_resultado_consulta, v_saldo_aivs, v_saldo_pesos
                     
      
   -- se devuelve el resultado de la consulta
   RETURN v_resultado_consulta, v_saldo_aivs, v_saldo_pesos
END FUNCTION

{
======================================================================
Nombre: 
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
  Calcula los saldos 97 y 92 si tuvo transferencias tipo B para ley 73
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_calcula_saldos_transferencia_tipo_b(p_nss)
DEFINE p_nss     LIKE afi_derechohabiente.nss, -- NSS del trabajador con transferencia B
       v_saldo   DECIMAL(22,2)
   
   -- se calcula el saldo
   LET v_saldo = 0
   
   
   -- se devuelve el resultado de la consulta
   RETURN v_saldo
END FUNCTION 


--==============================================================================
-- Amortizaciones excedentes
{
======================================================================
Nombre: fn_verifica_saldo_amortizaciones_excedentes
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
  Verifica si existe saldo para amortizaciones excedentes
   
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_saldo_amortizaciones_excedentes(p_nss)
DEFINE p_nss          CHAR(11),
       v_existe_saldo SMALLINT

   RETURN v_existe_saldo
END FUNCTION

{
======================================================================
Nombre: fn_verifica_convivencia_marca
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
  Verifica la convivencia de marca para amortizaciones excedentes

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_convivencia_marca(p_nss)
DEFINE p_nss           CHAR(11),
       v_convive_marca SMALLINT


   RETURN v_convive_marca
END FUNCTION




--==============================================================================


{
======================================================================
Nombre: fn_recibe_estatus_disponibilidad
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
  Recibe el estatus para la disponibilida de los retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recibe_estatus_disponibilidad(p_estatus)
DEFINE p_estatus SMALLINT


END FUNCTION 

{
======================================================================
Nombre: fn_verifica_disponibilidad_retiros
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
  Verifica la disponibilidad completa para todos los retiros

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_disponibilidad_retiros()
DEFINE v_disponible SMALLINT


   RETURN v_disponible
END FUNCTION 

{
======================================================================
Nombre: fn_informa_disponibilidad_retiros_adai
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
  Informa disponiblidad de los retiros a ADAI

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_informa_disponibilidad_retiros_adai()

END FUNCTION


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

--      A M O R T I Z A C I O N E S   E X C E D E N T E S

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




{
======================================================================
Nombre: fn_ret_generico_desmarca_cuenta
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta el SP de desmarca de cuenta para un id_derechohabiente y marca
dados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, p_marca_entra, p_num_referencia, p_marca_causa,
                                         p_usuario, p_proceso_cod)
DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, 
       p_marca_entra        LIKE sfr_marca.marca,
       p_num_referencia     LIKE ret_solicitud_generico.id_solicitud,
       p_estado_marca       SMALLINT,
       p_marca_causa        LIKE sfr_marca.marca,
       p_usuario            LIKE seg_usuario.usuario_cod,
       p_proceso_cod        LIKE cat_proceso.proceso_cod,
       v_sql                STRING, -- cadena con instruccion sql
       v_respuesta_marcaje  SMALLINT -- resultado de la desmarca

   -- se prepara la ejecucion de la desmarca
   LET v_sql="EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   
   -- se ejecuta
   PREPARE stm_desmarcaje_fahorro FROM  v_sql
   EXECUTE stm_desmarcaje_fahorro USING p_id_derechohabiente, 
                                        p_marca_entra       ,
                                        p_num_referencia    ,
                                        p_estado_marca      ,
                                        p_marca_causa       ,
                                        p_usuario           ,
                                        p_proceso_cod 
      INTO v_respuesta_marcaje

END FUNCTION


{
======================================================================
Nombre: fn_ret_generico_marca_cuenta
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecuta el SP de marca de cuenta para un id_derechohabiente y marca
dados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_generico_marca_cuenta(p_id_derechohabiente, p_marca_entra, p_num_referencia, p_folio, 
                                      p_estado_marca, p_codigo_rechazo, p_marca_causa, p_fecha_causa,
                                      p_usuario, p_proceso_cod)
DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, 
       p_marca_entra        LIKE sfr_marca.marca,
       p_num_referencia     LIKE ret_solicitud_generico.id_solicitud,
       p_folio              LIKE glo_folio.folio, 
       p_estado_marca       SMALLINT,
       p_codigo_rechazo     SMALLINT,
       p_marca_causa        LIKE sfr_marca.marca,
       p_fecha_causa        DATE,
       p_usuario            LIKE seg_usuario.usuario_cod,
       p_proceso_cod        LIKE cat_proceso.proceso_cod,
       v_sql                STRING, -- cadena con instruccion sql
       v_respuesta_marcaje  SMALLINT -- resultado de la desmarca

   -- se prepara la ejecucion de la desmarca
   LET v_sql = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
         		                 
   -- se prepara y ejecuta la funcion de marcaje    		                 
   PREPARE stm_marcaje FROM v_sql
   EXECUTE stm_marcaje USING p_id_derechohabiente,
                             p_marca_entra       ,
                             p_num_referencia      ,
                             p_folio             ,
                             p_estado_marca      ,
                             p_codigo_rechazo    ,
                             p_marca_causa       ,
                             p_fecha_causa       ,
                             p_usuario           ,
                             p_proceso_cod 
                       INTO v_respuesta_marcaje

   -- en caso de error se muestra que paso
   IF ( v_respuesta_marcaje <> 0 ) THEN
      DISPLAY "ERROR AL MARCAR: ", v_respuesta_marcaje
   END IF

   -- se devuelve el resultado de la ejecucion
   RETURN v_respuesta_marcaje
END FUNCTION

{
======================================================================
Nombre: fn_notificacion_correo_retiro
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Envia un correo con titulo y mensaje a la direccion recibida como parametro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_notificacion_correo_retiro(p_email, p_adjunto, p_titulo,p_mensaje)
    DEFINE p_pid            DECIMAL(9,0)
    DEFINE p_proceso        SMALLINT
    DEFINE p_operacion      SMALLINT
    DEFINE p_adjunto        STRING
    DEFINE p_mensaje        STRING
    DEFINE p_titulo         STRING
    DEFINE p_email          STRING

    DEFINE v_rec_correo     RECORD
        usuario_cod             CHAR(12),
        correo_e                CHAR(100)
    END RECORD
    DEFINE v_fecha          DATE
    DEFINE v_hora           DATETIME HOUR TO SECOND
    DEFINE v_adjunto        CHAR(200)
    DEFINE v_ruta_listado   CHAR(40)
    DEFINE v_ruta_listado_nohup CHAR(40)
    DEFINE v_proceso_desc   CHAR(80)
    DEFINE v_operacion_desc CHAR(100)
    DEFINE v_fecha_ini      DATE
    DEFINE v_fecha_fin      char(18)
    DEFINE v_comando        STRING
    DEFINE v_cat_adjunto    STRING
    DEFINE v_cat_mail       STRING
    DEFINE v_archivo        STRING
    DEFINE ch               base.Channel
    DEFINE ch_mensaje       base.Channel
    DEFINE v_linea          STRING    
    DEFINE v_modulo_cod     CHAR(003)
    DEFINE v_estado         CHAR(020)         
    DEFINE v_archivo_nohup  STRING    
    DEFINE v_ind_correo     SMALLINT
    DEFINE v_estado_cod_opera SMALLINT

    LET v_comando = "echo '", p_mensaje, "' | mailx -s '", p_titulo, "' ", p_email
    DISPLAY "comando para correo: ", v_comando
    RUN v_comando
    
END FUNCTION

{
======================================================================
Nombre: fn_verifica_prescripcion
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si existe prescripcion de acuerdo con 2 fechas dadas y una cantidad de tiempo
especificada

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_prescripcion(p_fecha_nacimiento, p_fecha_inicio_pension, p_fecha_comparacion, p_num_anos)
DEFINE p_fecha_nacimiento      DATE, -- fecha de nacimiento de un trabajador
       p_fecha_inicio_pension  DATE, -- fecha de inicio de pension o de resolucion
       v_fecha_50_anos         DATE, -- fecha en que el trabajador cumple 50 anos
       v_fecha_cadena          CHAR(10), -- para calcular fechasr
       p_fecha_comparacion     DATE, -- fecha contra la que se compara
       v_ano                   CHAR(4), -- calculo de ano para fecha
       p_num_anos              SMALLINT, -- numero de anos para que aplique la prescripcion
       v_fecha_auxiliar        DATE, -- fecha auxiliar calculada
       v_existe_prescripcion   SMALLINT, -- booleana que indica si hay prescripcion
       v_dia_auxiliar          SMALLINT, -- para calcular anos de prescripcion
       v_mes_auxiliar          SMALLINT, -- para calcular anos de prescripcion
       v_ano_auxiliar          SMALLINT -- para calcular anos
       
   -- se asume que no hay prescripcion
   LET v_existe_prescripcion = FALSE
   
   -- si se recibieron ambas fechas es para verificar contra termino de relacion laboral
   IF ( p_fecha_nacimiento IS NOT NULL AND p_fecha_inicio_pension IS NOT NULL ) THEN
      -- se obtiene la fecha en que el derechohabiente cumple 50 anos
      CALL ERRORLOG("Calculando ano + 50 anos")
      LET v_ano = YEAR(p_fecha_nacimiento) + 50
      LET v_ano_auxiliar = v_ano + 50
      LET v_ano = v_ano_auxiliar
      LET v_dia_auxiliar = DAY(p_fecha_nacimiento)
      LET v_mes_auxiliar = MONTH(p_fecha_nacimiento)
      LET v_fecha_cadena = v_mes_auxiliar USING "&&", "/", v_dia_auxiliar USING "&&", "/", v_ano_auxiliar USING "&&&&"
      LET v_fecha_50_anos = DATE(v_fecha_cadena)
      
      CALL ERRORLOG("Fecha de prescripcion (+50 anos): " || v_fecha_cadena)
      
      -- se verifica cual de las dos fechas es mayor
      IF ( v_fecha_50_anos > p_fecha_inicio_pension ) THEN
      
         -- se calcula la fecha que daria la fecha de cumplimiento de 50 anos mas los anos para prescribir
         LET v_fecha_cadena = MONTH(v_fecha_50_anos) CLIPPED,"/",DAY(v_fecha_50_anos) CLIPPED,"/",(YEAR(v_fecha_50_anos) + p_num_anos) CLIPPED
         LET v_fecha_auxiliar = DATE(v_fecha_cadena)
         -- se verifica si prescribe con la fecha de cuando cumplio 50 anos
         IF ( p_fecha_comparacion > v_fecha_auxiliar  ) THEN
            LET v_existe_prescripcion = TRUE
         END IF
      ELSE
         -- se calcula la fecha de inicio de pension mas el numero de anos para prescribir
         LET v_fecha_cadena = MONTH(p_fecha_inicio_pension) CLIPPED ,"/",DAY(p_fecha_inicio_pension) CLIPPED ,"/",(YEAR(p_fecha_inicio_pension) + p_num_anos) CLIPPED
         LET v_fecha_auxiliar = DATE(v_fecha_cadena)
         -- se verifica si prescribe con la fecha de inicio de pension/emision de resolucion
         IF ( p_fecha_comparacion > v_fecha_auxiliar  ) THEN
            LET v_existe_prescripcion = TRUE
         END IF         
      END IF
   ELSE
      CALL ERRORLOG("calculando fecha + 10 anos")
      LET v_ano = YEAR(p_fecha_inicio_pension)
      LET v_ano_auxiliar = v_ano + p_num_anos
      LET v_dia_auxiliar = DAY(p_fecha_inicio_pension)
      LET v_mes_auxiliar = MONTH(p_fecha_inicio_pension)

      CALL ERRORLOG("Ano auxiliar: " || v_ano_auxiliar)
      -- se calcula la fecha de inicio de pension mas el numero de anos para prescribir
      LET v_fecha_cadena = v_mes_auxiliar USING "&&", "/", v_dia_auxiliar USING "&&", "/", v_ano_auxiliar USING "&&&&"
      CALL ERRORLOG("Fecha de prescripcion: " || v_fecha_cadena)
      LET v_fecha_auxiliar = DATE(v_fecha_cadena)
      CALL ERRORLOG("Fecha de prescripcion (DATE): " || v_fecha_auxiliar)
      -- se verifica si prescribe con la fecha de inicio de pension/emision de resolucion
      CALL ERRORLOG("Verifica (fecha negocio:)" || v_fecha_auxiliar || " vs. (fecha compara): " || p_fecha_comparacion)
      IF ( p_fecha_comparacion > v_fecha_auxiliar ) THEN
         CALL ERRORLOG("Prescribio")
         LET v_existe_prescripcion = TRUE         
      END IF         
   END IF
   
   -- se devuelve el resultado de la consulta
   RETURN v_existe_prescripcion
END FUNCTION

{
======================================================================
Nombre: fn_fecha_nacimiento_rfc
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene la fecha de nacimiento del RFC de un derechohabiente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_fecha_nacimiento_rfc(p_rfc)
DEFINE p_rfc              LIKE afi_derechohabiente.rfc, -- RFC del trabajador
       v_fecha_nacimiento DATE,
       v_fecha_cadena     CHAR(10),
       v_ano              CHAR(4)
       
   -- se obtiene la fecha
   -- VEHI820205
   -- 1234567890
   -- como es para fondo de ahorro se asume que todos son nacidos en 1900
   LET v_ano = "19", p_rfc[5,6]
   
   LET v_fecha_cadena = p_rfc[7,8], "/", p_rfc[9,10], "/", v_ano
   
   -- se transforma la fecha a DATE
   LET v_fecha_nacimiento = DATE(v_fecha_cadena)
   
   -- se devuelve la fecha calculada
   RETURN v_fecha_nacimiento
END FUNCTION
   
{
======================================================================
Nombre: 
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_obtener_precio_fondo(p_fecha_valuacion, p_fondo)
DEFINE p_fecha_valuacion DATE,
       p_fondo           LIKE glo_valor_fondo.fondo,
       v_valor_fondo     LIKE glo_valor_fondo.precio_fondo
       
   -- se obtiene el valor de la accion
   SELECT precio_fondo
   INTO   v_valor_fondo
   FROM   glo_valor_fondo
   WHERE  f_valuacion = p_fecha_valuacion
   AND    fondo = p_fondo
   
   -- se devuelve el valor encontrado
   RETURN v_valor_fondo
END FUNCTION

{
======================================================================
Nombre: 
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_buscar_disponibilidad_retiro(p_modalidad_retiro, p_subcuenta)
DEFINE p_modalidad_retiro LIKE ret_subcuenta_disponibilidad.modalidad_retiro,
		   p_subcuenta        LIKE ret_subcuenta_disponibilidad.subcuenta,
		   v_busca_disp       LIKE ret_subcuenta_disponibilidad.busca_disponibilidad
		
   -- se verifica si se buscara disponibilidad de la subcuenta y la modalidad de retiro   
   SELECT busca_disponibilidad
   INTO   v_busca_disp
   FROM   ret_subcuenta_disponibilidad
   WHERE  modalidad_retiro = p_modalidad_retiro
   AND    subcuenta        = p_subcuenta
		
   -- si esta activa, devuelve verdadero
   IF ( v_busca_disp = 1 ) THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF
END FUNCTION

{
======================================================================
Nombre: fn_verifica_estructura_clabe
Fecha creacion: Septiembre 20, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Valida si una cuenta clabe cumple con la estructura definida

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_estructura_clabe(p_clabe)
DEFINE p_clabe CHAR(18),
       v_clabe_validada STRING,
       v_indice SMALLINT,
       v_clabe_correcta SMALLINT -- booleana que indica si la cuenta clabe es correcta
       
   -- se asume que es correcta
   LET v_clabe_correcta = TRUE
   
   -- se transforma en string
   LET v_clabe_validada = p_clabe
   
   -- se borran los espacios en blanco
   LET v_clabe_validada = v_clabe_validada.trim()
   
   -- la longitud debe ser de 18 caracteres
   IF ( v_clabe_validada.getLength() < 18 ) THEN
      LET v_clabe_correcta = FALSE
   ELSE
      -- todos los caracteres deben ser numericos
      FOR v_indice = 1 TO v_clabe_validada.getLength()
         IF ( v_clabe_validada.getCharAt(v_indice) < "0" OR v_clabe_validada.getCharAt(v_indice) > "9" ) THEN
            LET v_clabe_correcta = FALSE
            EXIT FOR
         END IF
      END FOR
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_clabe_correcta
END FUNCTION

{
======================================================================
Nombre: fn_consulta_saldo_vivienda_afore
Fecha creacion: Noviembre 25, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Obtiene el saldo de la subcuenta de vivienda en la AFORE

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
{
FUNCTION fn_consulta_saldo_vivienda_afore(p_nss,p_id_cliente)
DEFINE 
      p_nss       LIKE afi_derechohabiente.nss, -- nss consultado
      p_id_cliente SMALLINT,
      v_aivs_viv92  DECIMAL(14,6), -- monto en aivs de vivienda 92
      v_pesos_viv92 DECIMAL(12,2), -- monto en pesos de vivienda 92
      v_aivs_viv97 DECIMAL(14,6), -- monto en aivs de vivienda 97
      v_pesos_viv97 DECIMAL(12,2), -- monto en pesos de vivienda 92
      v_diagnostico SMALLINT,      -- Diagnostico de la consulta en la Afore
      v_estatus     SMALLINT,      -- Estatus de la Cuenta Individual
      v_resultado   SMALLINT,
      v_probabilidad SMALLINT,
       v_cod_rechazo SMALLINT
   
   --Inicializaciones para la simulacion
   LET v_diagnostico = 101
   LET v_aivs_viv92  = 0
   LET v_aivs_viv97  = 0
   LET v_pesos_viv92 = 0
   LET v_pesos_viv97 = 0
   LET v_estatus     = 0
   
   --Simulo una probabilidad al azar de entre 1 y 39
   CALL util.math.rand(40) RETURNING v_probabilidad
}
   {
   --Posibilidad total de que no pudo conectarse con PROCESAR
   DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
   DISPLAY "CODE       : ", wsError.code
   DISPLAY "CODENS     : ", wsError.codeNS
   DISPLAY "DESCRIPTION: ", wsError.description
   DISPLAY "ACTION     : ", wsError.action	  
   LET v_diagnostico = 127
   --LET v_diagnostico = 0
   LET v_estatus     = 0
   LET v_aivs_viv92  = 0
   LET v_aivs_viv97  = 0
   LET v_pesos_viv92 = 0
   LET v_pesos_viv97 = 0
   }
            
{   
   --Posibilidad total para buscar en SACI
   CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
   CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92
   IF ( v_resultado = 0 ) THEN
      LET v_diagnostico = 101
      LET v_estatus     = 101
   ELSE
      DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESCRIPTION: ", wsError.description
      DISPLAY "ACTION     : ", wsError.action	  
      LET v_diagnostico = 127
      --LET v_diagnostico = 0
      LET v_estatus     = 0
      LET v_aivs_viv92  = 0
      LET v_aivs_viv97  = 0
      LET v_pesos_viv92 = 0
      LET v_pesos_viv97 = 0
   END IF
   

}
   {
   --Posibilidad mayor para que no se revise desde PROCESAR
   CASE
      WHEN v_probabilidad = 1 OR v_probabilidad = 3 OR v_probabilidad = 5 OR v_probabilidad = 7
      OR v_probabilidad = 9 OR v_probabilidad = 11
         --Para las probabilidades nones simulare que SI hubo conexión a Procesar
         --pero intentaré obtener saldo, sino encuentro saldo doy como error al invocar WS
         CALL fn_calcula_saldo_ley73(p_nss, 4, TODAY) RETURNING v_resultado, v_aivs_viv97, v_pesos_viv97
         CALL fn_calcula_saldo_ley73(p_nss, 8, TODAY) RETURNING v_resultado, v_aivs_viv92, v_pesos_viv92
         IF ( v_resultado = 0 ) THEN
            LET v_diagnostico = 101
            LET v_estatus     = 101
         ELSE
            DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
            DISPLAY "CODE       : ", wsError.code
            DISPLAY "CODENS     : ", wsError.codeNS
            DISPLAY "DESCRIPTION: ", wsError.description
            DISPLAY "ACTION     : ", wsError.action	  
            LET v_diagnostico = 127
            --LET v_diagnostico = 0
            LET v_estatus     = 0
            LET v_aivs_viv92  = 0
            LET v_aivs_viv97  = 0
            LET v_pesos_viv92 = 0
            LET v_pesos_viv97 = 0
         END IF
         EXIT CASE
      WHEN v_probabilidad = 13 OR v_probabilidad = 15
      OR v_probabilidad = 17 OR v_probabilidad = 19 OR v_probabilidad = 21 OR v_probabilidad = 23
      OR v_probabilidad = 25 OR v_probabilidad = 27 OR v_probabilidad = 29 OR v_probabilidad = 31
      OR v_probabilidad = 33 OR v_probabilidad = 35 OR v_probabilidad = 37
      OR (v_probabilidad >= 39 AND v_probabilidad <= 100)
         DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
         DISPLAY "CODE?????? : ", wsError.code
         DISPLAY "CODENS???? : ", wsError.codeNS
         DISPLAY "DESCRIPTION: ", wsError.description
         DISPLAY "ACTION???? : ", wsError.action
         LET v_diagnostico = 127
         LET v_estatus     = 0
         LET v_aivs_viv92  = 0
         LET v_aivs_viv97  = 0
         LET v_pesos_viv92 = 0
         LET v_pesos_viv97 = 0
         -- se devuelve el codigo de error del WS y fecha nula
         --RETURN wsError.code, NULL
      --Para las probabilidades pares asignaré un estado de error.
      WHEN v_probabilidad = 2
         LET v_estatus = 201     EXIT CASE
      WHEN v_probabilidad = 4
         LET v_estatus = 202     EXIT CASE
      WHEN v_probabilidad = 6
         LET v_estatus = 203     EXIT CASE
      WHEN v_probabilidad = 8
         LET v_estatus = 204     EXIT CASE
      WHEN v_probabilidad = 10
         LET v_estatus = 205     EXIT CASE
      WHEN v_probabilidad = 12
         LET v_estatus = 207     EXIT CASE
      WHEN v_probabilidad = 14
         LET v_estatus = 208     EXIT CASE
      WHEN v_probabilidad = 16
         LET v_estatus = 211     EXIT CASE
      WHEN v_probabilidad = 18
         LET v_estatus = 212     EXIT CASE
      WHEN v_probabilidad = 20
         LET v_estatus = 213     EXIT CASE
      WHEN v_probabilidad = 22
         LET v_estatus = 216     EXIT CASE
      WHEN v_probabilidad = 24
         LET v_estatus = 217     EXIT CASE
      WHEN v_probabilidad = 26
         LET v_estatus = 219     EXIT CASE
      WHEN v_probabilidad = 28
         LET v_estatus = 220     EXIT CASE
      WHEN v_probabilidad = 30
         LET v_estatus = 221     EXIT CASE
      WHEN v_probabilidad = 32
         LET v_estatus = 223     EXIT CASE
      WHEN v_probabilidad = 34
         LET v_estatus = 224     EXIT CASE
      WHEN v_probabilidad = 36
         LET v_estatus = 225     EXIT CASE
      WHEN v_probabilidad = 38
         LET v_estatus = 226     EXIT CASE
   END CASE

   CASE v_estatus
       WHEN 102
           LET v_cod_rechazo = gi_101_102 -- 999, -- Codigo 101 102 NSS no se encuentra registrado en la BDNSAR
       WHEN 201
           LET v_cod_rechazo = gi_101_201 -- 22,  -- La cuenta se encuentra en proceso de 43 BIS
       WHEN 202
           LET v_cod_rechazo = gi_101_202 -- 820, -- En proceso de Traspaso A-A
       WHEN 203
           LET v_cod_rechazo = gi_101_203 -- 597, -- En proceso de Unificación de cuentas
       WHEN 204
           LET v_cod_rechazo = gi_101_204 -- 821, -- En proceso de Fusión de Afore
       WHEN 205
           LET v_cod_rechazo = gi_101_205 -- 201, -- En proceso de separación de cuentas
       WHEN 207
           LET v_cod_rechazo = gi_101_207 -- 822, -- En proceso de transferencias de recursos
       WHEN 208
           LET v_cod_rechazo = gi_101_208 -- 823, -- En proceso de disposición de recursos
       WHEN 211
           LET v_cod_rechazo = gi_101_211 -- 824, -- En proceso de Devolución de pagos efectuados sin Justificación Legal
       WHEN 212
           LET v_cod_rechazo = gi_101_212 -- 825, -- En proceso de retiros parciales
       WHEN 213
           LET v_cod_rechazo = gi_101_213 -- 826, -- En proceso de tramite judicial iniciado por Afore
       WHEN 216
           LET v_cod_rechazo = gi_101_216 -- 827, -- Cuenta en proceso de aclaración por conciliación
       WHEN 217
           LET v_cod_rechazo = gi_101_217 -- 828, -- Cuenta en proceso de selección SIEFORE
       WHEN 219
           LET v_cod_rechazo = gi_101_219 -- 829, -- Cuenta en proceso de modificación
       WHEN 220
           LET v_cod_rechazo = gi_101_220 -- 836, -- Cuenta en proceso de crédito de vivienda
       WHEN 221
           LET v_cod_rechazo = gi_101_221 -- 837, -- Cuenta en proceso de crédito de vivienda 43BIS
       WHEN 223
           LET v_cod_rechazo = gi_101_223 -- 830, -- Cuenta en proceso de saldo previo
       WHEN 224
           LET v_cod_rechazo = gi_101_224 -- 832, -- No se encuentra marcado
       WHEN 225
           LET v_cod_rechazo = gi_101_225 -- 833, -- Existe alguna notificación de pago por Ventanilla INFONAVIT
       WHEN 226
           LET v_cod_rechazo = gi_101_226 -- 834, -- Existe alguna notificación de pago por Ventanilla INFONAVIT (ESTE LO ENVIA LA AF0RE)
       OTHERWISE 
           LET v_cod_rechazo = gi_103_0   -- 835, -- No se encuentra marcado (ESTE LO ENVIA LA AF0RE)
    END CASE
   }
{    
   -- se devuelve el saldo total en aivs y pesos
   RETURN v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
   
END FUNCTION
}

FUNCTION fn_consulta_saldo_vivienda_afore(p_nss,p_id_cliente)
DEFINE p_nss   LIKE afi_derechohabiente.nss, -- nss consultado
       p_id_cliente SMALLINT,     
       p_curp  LIKE afi_derechohabiente.curp,  -- curp consultado
       v_aivs_viv92  DECIMAL(14,6), -- monto en aivs de vivienda 92
       v_pesos_viv92 DECIMAL(12,2), -- monto en pesos de vivienda 92
       v_aivs_viv97  DECIMAL(14,6), -- monto en aivs de vivienda 97
       v_pesos_viv97 DECIMAL(12,2), -- monto en pesos de vivienda 92
       v_total_aivs  DECIMAL(14,6), -- monto total en aivs
       v_total_pesos DECIMAL(12,2),  -- monto total en pesos,
       v_diagnostico SMALLINT,       -- Diagnostico de la consulta en la Afore
       v_estatus     SMALLINT,       -- Estatus de la Cuenta Individual
       v_resultado   SMALLINT,
       v_nss_tmp     STRING,
       v_cod_rechazo SMALLINT


    -- se inicializan las variables de retorno del saldo
    LET v_aivs_viv92 = 0
    LET v_aivs_viv97 = 0

    LET v_pesos_viv92 = 0
    LET v_pesos_viv97 = 0
    LET v_diagnostico = 0
    LET v_estatus     = 0
    LET v_cod_rechazo = 0

    INITIALIZE ns1parametersConsultarSaldoPreliminarRequest TO NULL
    INITIALIZE ns1parametersConsultarSaldoPreliminarResponse TO NULL 

    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = 49
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = p_id_cliente   -- 30 - Consulta, 44 - marca y consulta, 60 - desmarca 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = "INFONAVIT"    
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION
    
    -- se inicializan las variables del WS con los valores a consultar   
    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.curp = p_curp
    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = p_nss
  
    -- se invoca el servicio
    -- DISPLAY "Ejecutando WSSaldoPreliminar..."
    CALL ConsultarSaldoPreliminar_g() RETURNING v_resultado       

    -- si el webservice NO se ejecuto correctamente
    IF ( v_resultado = 0 ) THEN      

        LET v_diagnostico = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.diagnostico
        LET v_estatus     = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusCuentaIndividual

        LET v_aivs_viv92  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92AIVS
        LET v_pesos_viv92 = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92
        LET v_aivs_viv97  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97AIVS
        LET v_pesos_viv97 = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97

    ELSE 
        DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
        DISPLAY "CODE       : ", wsError.code
        DISPLAY "CODENS     : ", wsError.codeNS
        DISPLAY "DESCRIPTION: ", wsError.description
        DISPLAY "ACTION     : ", wsError.action	  
        LET v_diagnostico = 127
        -- se devuelve el codigo de error del WS y fecha nula
        --RETURN wsError.code, NULL
    END IF

    -- se valida el estatus por si es rechazado se traduce a los códigos de SACI
   CASE v_estatus
       WHEN 101
           LET v_cod_rechazo = 0  --- Procede retiro
       WHEN 102
           LET v_cod_rechazo = gi_101_102 -- 999, -- Codigo 101 102 NSS no se encuentra registrado en la BDNSAR
       WHEN 201
           LET v_cod_rechazo = gi_101_201 -- 22,  -- La cuenta se encuentra en proceso de 43 BIS
       WHEN 202
           LET v_cod_rechazo = gi_101_202 -- 820, -- En proceso de Traspaso A-A
       WHEN 203
           LET v_cod_rechazo = gi_101_203 -- 597, -- En proceso de Unificación de cuentas
       WHEN 204
           LET v_cod_rechazo = gi_101_204 -- 821, -- En proceso de Fusión de Afore
       WHEN 205
           LET v_cod_rechazo = gi_101_205 -- 201, -- En proceso de separación de cuentas
       WHEN 207
           LET v_cod_rechazo = gi_101_207 -- 822, -- En proceso de transferencias de recursos
       WHEN 208
           LET v_cod_rechazo = gi_101_208 -- 823, -- En proceso de disposición de recursos
       WHEN 211
           LET v_cod_rechazo = gi_101_211 -- 824, -- En proceso de Devolución de pagos efectuados sin Justificación Legal
       WHEN 212
           LET v_cod_rechazo = gi_101_212 -- 825, -- En proceso de retiros parciales
       WHEN 213
           LET v_cod_rechazo = gi_101_213 -- 826, -- En proceso de tramite judicial iniciado por Afore
       WHEN 216
           LET v_cod_rechazo = gi_101_216 -- 827, -- Cuenta en proceso de aclaración por conciliación
       WHEN 217
           LET v_cod_rechazo = gi_101_217 -- 828, -- Cuenta en proceso de selección SIEFORE
       WHEN 219
           LET v_cod_rechazo = gi_101_219 -- 829, -- Cuenta en proceso de modificación
       WHEN 220
           LET v_cod_rechazo = gi_101_220 -- 836, -- Cuenta en proceso de crédito de vivienda
       WHEN 221
           LET v_cod_rechazo = gi_101_221 -- 837, -- Cuenta en proceso de crédito de vivienda 43BIS
       WHEN 223
           LET v_cod_rechazo = gi_101_223 -- 830, -- Cuenta en proceso de saldo previo
       WHEN 224
           LET v_cod_rechazo = gi_101_224 -- 832, -- No se encuentra marcado
       WHEN 225
           LET v_cod_rechazo = gi_101_225 -- 833, -- Existe alguna notificación de pago por Ventanilla INFONAVIT
       WHEN 226
           LET v_cod_rechazo = gi_101_226 -- 834, -- Existe alguna notificación de pago por Ventanilla INFONAVIT (ESTE LO ENVIA LA AF0RE)
       OTHERWISE 
           LET v_cod_rechazo = gi_103_0   -- 835, -- No se encuentra marcado (ESTE LO ENVIA LA AF0RE)
    END CASE
    
    RETURN v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
END FUNCTION

FUNCTION fn_consulta_saldo_vivienda_afore_completa(p_nss,p_id_cliente)
DEFINE p_nss   LIKE afi_derechohabiente.nss, -- nss consultado
       p_id_cliente SMALLINT,     
       p_curp  LIKE afi_derechohabiente.curp,  -- curp consultado
       v_aivs_viv92  DECIMAL(14,6), -- monto en aivs de vivienda 92
       v_pesos_viv92 DECIMAL(12,2), -- monto en pesos de vivienda 92
       v_aivs_viv97  DECIMAL(14,6), -- monto en aivs de vivienda 97
       v_pesos_viv97 DECIMAL(12,2), -- monto en pesos de vivienda 92
       v_total_aivs  DECIMAL(14,6), -- monto total en aivs
       v_total_pesos DECIMAL(12,2),  -- monto total en pesos,
       v_diagnostico SMALLINT,       -- Diagnostico de la consulta en la Afore
       v_estatus     SMALLINT,       -- Estatus de la Cuenta Individual
       v_resultado   SMALLINT,
       v_nss_tmp     STRING,
       v_cod_rechazo SMALLINT,
       v_cve_afore   CHAR(3)


    -- se inicializan las variables de retorno del saldo
    LET v_aivs_viv92 = 0
    LET v_aivs_viv97 = 0

    LET v_pesos_viv92 = 0
    LET v_pesos_viv97 = 0
    LET v_diagnostico = 0
    LET v_estatus     = 0
    LET v_cod_rechazo = 0

    INITIALIZE ns1parametersConsultarSaldoPreliminarRequest TO NULL
    INITIALIZE ns1parametersConsultarSaldoPreliminarResponse TO NULL 

    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = 49
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = p_id_cliente   -- 30 - Consulta, 44 - marca y consulta, 60 - desmarca 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = 7 
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = "INFONAVIT"    
    LET ns1parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION
    
    -- se inicializan las variables del WS con los valores a consultar   
    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.curp = p_curp
    LET ns1parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = p_nss
  
    -- se invoca el servicio
    -- DISPLAY "Ejecutando WSSaldoPreliminar..."
    CALL ConsultarSaldoPreliminar_g() RETURNING v_resultado       

    -- si el webservice NO se ejecuto correctamente
    IF ( v_resultado = 0 ) THEN      

        LET v_diagnostico = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.diagnostico
        LET v_estatus     = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusCuentaIndividual

        LET v_aivs_viv92  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92AIVS
        LET v_pesos_viv92 = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda92
        LET v_aivs_viv97  = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97AIVS
        LET v_pesos_viv97 = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.saldoVivienda97
        LET v_cve_afore   = ns1parametersConsultarSaldoPreliminarResponse.objetoRespuesta.cveAfore
    ELSE 
        DISPLAY "ERROR al invocar webservice de consulta de saldo Afore"
        DISPLAY "CODE       : ", wsError.code
        DISPLAY "CODENS     : ", wsError.codeNS
        DISPLAY "DESCRIPTION: ", wsError.description
        DISPLAY "ACTION     : ", wsError.action	  
        LET v_diagnostico = 127
        -- se devuelve el codigo de error del WS y fecha nula
        --RETURN wsError.code, NULL
    END IF

    -- se valida el estatus por si es rechazado se traduce a los códigos de SACI
   CASE v_estatus
       WHEN 101
           LET v_cod_rechazo = 0  --- Procede retiro
       WHEN 102
           LET v_cod_rechazo = gi_101_102 -- 999, -- Codigo 101 102 NSS no se encuentra registrado en la BDNSAR
       WHEN 201
           LET v_cod_rechazo = gi_101_201 -- 22,  -- La cuenta se encuentra en proceso de 43 BIS
       WHEN 202
           LET v_cod_rechazo = gi_101_202 -- 820, -- En proceso de Traspaso A-A
       WHEN 203
           LET v_cod_rechazo = gi_101_203 -- 597, -- En proceso de Unificación de cuentas
       WHEN 204
           LET v_cod_rechazo = gi_101_204 -- 821, -- En proceso de Fusión de Afore
       WHEN 205
           LET v_cod_rechazo = gi_101_205 -- 201, -- En proceso de separación de cuentas
       WHEN 207
           LET v_cod_rechazo = gi_101_207 -- 822, -- En proceso de transferencias de recursos
       WHEN 208
           LET v_cod_rechazo = gi_101_208 -- 823, -- En proceso de disposición de recursos
       WHEN 211
           LET v_cod_rechazo = gi_101_211 -- 824, -- En proceso de Devolución de pagos efectuados sin Justificación Legal
       WHEN 212
           LET v_cod_rechazo = gi_101_212 -- 825, -- En proceso de retiros parciales
       WHEN 213
           LET v_cod_rechazo = gi_101_213 -- 826, -- En proceso de tramite judicial iniciado por Afore
       WHEN 216
           LET v_cod_rechazo = gi_101_216 -- 827, -- Cuenta en proceso de aclaración por conciliación
       WHEN 217
           LET v_cod_rechazo = gi_101_217 -- 828, -- Cuenta en proceso de selección SIEFORE
       WHEN 219
           LET v_cod_rechazo = gi_101_219 -- 829, -- Cuenta en proceso de modificación
       WHEN 220
           LET v_cod_rechazo = gi_101_220 -- 836, -- Cuenta en proceso de crédito de vivienda
       WHEN 221
           LET v_cod_rechazo = gi_101_221 -- 837, -- Cuenta en proceso de crédito de vivienda 43BIS
       WHEN 223
           LET v_cod_rechazo = gi_101_223 -- 830, -- Cuenta en proceso de saldo previo
       WHEN 224
           LET v_cod_rechazo = gi_101_224 -- 832, -- No se encuentra marcado
       WHEN 225
           LET v_cod_rechazo = gi_101_225 -- 833, -- Existe alguna notificación de pago por Ventanilla INFONAVIT
       WHEN 226
           LET v_cod_rechazo = gi_101_226 -- 834, -- Existe alguna notificación de pago por Ventanilla INFONAVIT (ESTE LO ENVIA LA AF0RE)
       OTHERWISE 
           LET v_cod_rechazo = gi_103_0   -- 835, -- No se encuentra marcado (ESTE LO ENVIA LA AF0RE)
    END CASE
    
    RETURN v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo,v_cve_afore
END FUNCTION

{
======================================================================
Nombre: fn_fecha_ultima_relacion_laboral
Fecha creacion: diciembre 11, 2013
Autor: Eneas Armas, EFP
Narrativa del proceso que realiza:
Funcion que invoca la consulta de la ultima relacion laboral mediante
Web service

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_fecha_ultima_relacion_laboral(p_nss)
DEFINE p_nss           LIKE afi_derechohabiente.nss -- nss consultado
       ,v_ws_status    SMALLINT -- estatus de ejecucion de un webservice
       ,v_indice       INTEGER
       ,v_fecha_fin    DATE
       ,v_fecha_f      DATE
       ,v_fecha_c      STRING
       ,v_con_sin_rl   SMALLINT 

   LET ns1consultarTrabajador.nss = p_nss
   DISPLAY "se invoca al servicio de relación laboral para el nss ", p_nss

   -- se invoca la consulta de pago a FICO
   CALL consultarTrabajador_g() RETURNING v_ws_status

   -- si el webservice NO se ejecuto correctamente
   IF ( v_ws_status <> 0 ) THEN      

      DISPLAY "ERROR al invocar webservice de consulta de pago"
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESRIPTION : ", wsError.description
      DISPLAY "ACTION     : ", wsError.action
	  
	  -- se devuelve el codigo de error del WS y fecha nula
      RETURN wsError.code, NULL, 0
   END IF

   {
Cuando la fecha de baja de relación laboral sea: 9999-12-31, quiere decir que la 
relación laboral aún se encuentra vigente.

En los casos que el servicio conteste el campo NRP, como blanco o ceros, y los campos 
de fecha de alta y fecha de baja tengan el formato 1900-01-01, significa que para ese 
NSS no existe relación laboral.
   }
   
   --fecha de referencia inicial para la comparación
   LET v_fecha_fin = DATE ( "01/01/1899" )
   LET v_con_sin_rl = 0   -- Inicializa el indicador 

   FOR v_indice = 1 TO ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab.getLength()
      --se formatea la fecha
--	  DISPLAY "Fecha encontrada en WS de relacion laboral"
      LET v_fecha_c = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja
	  DISPLAY "Fecha encontrada en WS de relacion laboral", v_fecha_c
--	  CALL ERRORLOG(ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja)
      LET v_fecha_c = v_fecha_c.subString(6,7),"/",v_fecha_c.subString(9,10),"/",v_fecha_c.subString(1,4)
      LET v_fecha_f = DATE(v_fecha_c)
      
	  -- se selecciona la fecha mas alta
      IF ( v_fecha_f > v_fecha_fin ) THEN
         LET v_fecha_fin = v_fecha_f
      END IF
   END FOR

   -- si la fecha mayor es 9999-12-31 entonces aun hay relacion laboral
   IF ( v_fecha_fin = "12/31/9999" ) THEN
      -- no se devuelve fecha
      LET v_fecha_fin = NULL
      LET v_con_sin_rl = 1  -- Con Relacion Laboral
   END IF
   
   -- si la fecha es 1900-01-01, entonces no hay relacion laboral
   IF ( v_fecha_fin = "01/01/1900" ) THEN
      LET v_fecha_fin = NULL
      LET v_con_sin_rl = 2   -- Sin Relacion Laboral
   END IF
   
   -- se devuelve el resultado de la ejecucion
   RETURN v_ws_status, v_fecha_fin, v_con_sin_rl
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_detalle_resolucion_spess
Fecha creacion: Agosto 04, 2014
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Detalle el error por el que no se encontro la resolucion en el spess

======================================================================
}
FUNCTION fn_detalle_resolucion_spess(p_nss, p_causal)
DEFINE  v_tiene_resolucion     SMALLINT, -- booleana que indica si el trabajador tiene resolucion en spess
        p_nss                  LIKE afi_derechohabiente.nss, -- nss del trabajador
        p_causal               SMALLINT, -- causal de retiro
        v_sec_pension          LIKE ret_datamart.sec_pension, -- max secuencia de pension
        v_diag_registro        LIKE ret_datamart.diag_registro, -- diagnostico del registro
        v_id_datamart          LIKE ret_datamart.id_datamart, -- clave de la tabla del datamart
		v_tipo_pension         LIKE ret_datamart.tpo_pension, -- tipo de pension
		v_porcentaje_valuacion LIKE ret_datamart.porcentaje_valuacion, -- porcentaje de valuacion
        v_sql                  STRING,
        v_error_det            SMALLINT,
        v_tipo_prestacion      LIKE ret_datamart.tpo_prestacion 
   
   -- se asume que el trabajador no tiene resolucion
   LET v_tiene_resolucion = FALSE
   LET v_error_det        = 91 -- Sin resolucion en el SPESS
   
   DISPLAY "Validando causal: ", p_causal
   
   CASE p_causal

      -- termino de relacion laboral
      WHEN 1
         --LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension), a.id_datamart, a.tpo_pension, a.porcentaje_valuacion, a.tpo_prestacion ",
                     --"\nFROM  ret_datamart a,                     ",
                     --"\n      ret_matriz_derecho b                ",
                     --"\nWHERE a.nss            = ?                ", -- el trabajador
                     --"\nAND   a.regimen        = b.regimen        ", -- condiciones del retiro validas
                     --"\nAND   a.tpo_seguro     = b.tpo_seguro     ", 
                     --"\nAND   a.tpo_pension    = b.tpo_pension    ", 
                     --"\nAND   a.tpo_prestacion = b.tpo_prestacion ",
                     --"\nGROUP BY a.id_datamart, a.tpo_pension, a.porcentaje_valuacion, a.tpo_prestacion    ", 
                     --"\nORDER BY a.id_datamart DESC               " 
         
         LET v_sql = "\nSELECT MAX(a.sec_pension)                 ",
                     "\nFROM  ret_datamart a,                     ",
                     "\n      ret_matriz_derecho b                ",
                     "\nWHERE a.nss            = ?                "
         
         PREPARE detsid_rellab FROM v_sql
         EXECUTE detsid_rellab USING p_nss INTO v_sec_pension
         IF v_sec_pension IS NOT NULL THEN  
             LET v_sql = "\n SELECT FIRST 1 a.id_datamart, a.tpo_pension , a.porcentaje_valuacion, a.tpo_prestacion ",
                         "\n FROM   ret_datamart a ",
                         "\n WHERE  a.nss = ", p_nss,
                         "\n AND    a.sec_pension = ", v_sec_pension
             PREPARE detsid_datamartcausal FROM v_sql
             EXECUTE detsid_datamartcausal INTO v_id_datamart, v_tipo_pension, v_porcentaje_valuacion, v_tipo_prestacion 
         END IF 

         LET v_error_det = 91 -- Sin resolucion en el SPESS
         
         -- si se encontro un registro
         IF ( v_id_datamart IS NOT NULL ) THEN
             IF (v_tipo_prestacion = "03") THEN 
                LET v_tiene_resolucion = FALSE 
                LET v_error_det = 92 -- Resolucion por Negativa de Pension
             ELSE 
                 -- si es tipo de pension IP, debe tener porcentaje valuacion
                IF ( v_tipo_pension = "IP" ) THEN
                   IF ( v_porcentaje_valuacion >= 50 ) THEN
                      -- resolucion valida
                      LET v_tiene_resolucion = TRUE
                      LET v_error_det = 0
                   ELSE
                      -- no tiene resolucion valida
                      LET v_tiene_resolucion = FALSE
                      LET v_error_det = 140 -- El porcentaje de valuacion es menor al 50 %
                   END IF
                ELSE
                   -- es resolucion valida
                   LET v_tiene_resolucion = TRUE
                   LET v_error_det = 0
                END IF
            END IF 
         END IF
     
      -- Resolucion de pension otorgada por el IMSS
      WHEN 2 -- se elimina el cruce con la matriz de derechos 20150702
         --LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension), a.id_datamart, a.tpo_pension, a.porcentaje_valuacion, a.tpo_prestacion ",
                     --"\nFROM  ret_datamart a,                     ",
                     --"\n      ret_matriz_derecho b                ",
                     --"\nWHERE a.nss            = ?                ", -- el trabajador
                     --"\nAND   a.regimen        = b.regimen        ", -- condiciones del retiro validas
                     --"\nAND   a.tpo_seguro     = b.tpo_seguro     ", 
                     --"\nAND   a.tpo_pension    = b.tpo_pension    ", 
                     --"\nAND   a.tpo_prestacion = b.tpo_prestacion ",
                     --"\nGROUP BY a.id_datamart, a.tpo_pension, a.porcentaje_valuacion, a.tpo_prestacion    ", 
                     --"\nORDER BY a.id_datamart DESC                       " 
         LET v_sql = "\nSELECT MAX(a.sec_pension)                 ",
                     "\nFROM  ret_datamart a,                     ",
                     "\n      ret_matriz_derecho b                ",
                     "\nWHERE a.nss            = ?                " -- el trabajador
         
         PREPARE detsid_pensionimss FROM v_sql
         EXECUTE detsid_pensionimss USING p_nss INTO v_sec_pension
         IF v_sec_pension IS NOT NULL THEN  
             LET v_sql = "\n SELECT FIRST 1 a.id_datamart, a.tpo_pension , a.porcentaje_valuacion, a.tpo_prestacion ",
                         "\n FROM   ret_datamart a ",
                         "\n WHERE  a.nss = ", p_nss,
                         "\n AND    a.sec_pension = ", v_sec_pension
             PREPARE detsid_datamart FROM v_sql
             EXECUTE detsid_datamart INTO v_id_datamart, v_tipo_pension, v_porcentaje_valuacion, v_tipo_prestacion 
         END IF 
         LET v_error_det = 91   -- no existe resolucion en el spess

         -- si se encontro un registro
         IF ( v_id_datamart IS NOT NULL ) THEN
		    -- si es tipo de pension IP, debe tener porcentaje valuacion
            IF (v_tipo_prestacion = "03") THEN
                LET v_error_det = 92 -- resolucion por negativa de pension
                LET v_tiene_resolucion = FALSE 
            ELSE 
                IF ( v_tipo_pension = "IP" ) THEN
                   IF ( v_porcentaje_valuacion >= 50 ) THEN
                      -- resolucion valida
                      LET v_tiene_resolucion = TRUE
                      LET v_error_det = 0
                   ELSE
                      LET v_error_det = 140 -- porcentaje de valuacion menor al 50 %
                      -- no tiene resolucion valida
                      LET v_tiene_resolucion = FALSE
                   END IF
                ELSE
                   -- es resolucion valida
                   LET v_tiene_resolucion = TRUE
                   LET v_error_det = 0
                END IF
            END IF 
         END IF
              
      -- plan privado de pension
      WHEN 3
	     -- 24feb2014. No requiere ser validado, se asume que se tiene
		 LET v_tiene_resolucion = TRUE
         LET v_error_det = 0
     
      WHEN 4
         SELECT MAX(a.sec_pension), a.id_datamart
         INTO    v_sec_pension, v_id_datamart
         FROM    ret_datamart a
                ,ret_matriz_derecho b
         WHERE  a.nss = p_nss
         AND    a.regimen        = b.regimen
         AND    a.tpo_seguro     = b.tpo_seguro
         AND    a.tpo_pension    IN ('OR','VI','VO','AS')
         AND    a.tpo_prestacion = b.tpo_prestacion
         GROUP BY a.id_datamart
     
         -- si tiene una secuencia de pension, tiene resolucion de pension que corresponde con una defuncion
         IF ( v_sec_pension IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
            LET v_error_det = 0
         END IF
     
      -- ====================================================================================
      -- BUSQUEDA DE RESOLUCION EN EL SPESS PARA LEY 73
      WHEN 5
         -- Resolucion del spess para ley 73
         LET v_sql = "\nSELECT FIRST 1 MAX(a.sec_pension), a.diag_registro, a.id_datamart",
                     "\nFROM  ret_datamart a                            ",
                     "\n      ,ret_matriz_derecho b                     ",
                     "\nWHERE a.nss            = ?                      ", -- el trabajador
                     "\nAND   a.regimen        = 73                     ", -- el regimen es forzoso que sea Ley73
                     "\nAND   b.tpo_retiro     = 'E'                    ", -- retiro avalado por el imss
                     "\nAND   a.regimen        = b.regimen              ",                      
                     "\nAND   a.tpo_seguro     = b.tpo_seguro           ", 
                     "\nAND   a.tpo_pension    = b.tpo_pension          ", 
                     "\nAND   a.tpo_prestacion = b.tpo_prestacion       ",
                     "\nGROUP BY a.diag_registro, a.id_datamart         ", 
                     "\nORDER BY a.id_datamart DESC                     " 
         
         PREPARE detsid_ley73 FROM v_sql
         EXECUTE detsid_ley73 USING p_nss INTO v_sec_pension, v_diag_registro, v_id_datamart
         
         -- si se encontro un datamart
         IF ( v_id_datamart IS NOT NULL ) THEN
            LET v_tiene_resolucion = TRUE
            LET v_error_det = 0
         END IF

      
   END CASE
   
   -- se devuelve el resultado de la consulta
   RETURN v_tiene_resolucion, v_id_datamart, v_error_det
END FUNCTION 
{
======================================================================
Clave: 
Nombre: fn_guarda_consulta_ws_vent_afore
Fecha creacion: Diciembre 28, 2015
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Guarda la consulta realizada a Procesar

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_guarda_consulta_ws_vent_afore(p_nss, p_indicador_marca, p_estado_indicador, p_f_intento,
                                          p_h_intento, p_diagnostico, p_estado_cuenta, p_saldo_viv92, 
                                          p_saldo_viv97, p_usuario, p_id_solicitud, p_caso_crm, p_origen)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         p_indicador_marca  SMALLINT,                -- Indica el tipo de accion 1 - marca, 2 - desmarca y 3 - consulta
         p_estado_indicador SMALLINT,                -- indica el evento en el que se encuentra el indicador de la marca
         p_f_intento        DATE,                    -- Fecha en la que se realiza el intento
         p_h_intento        DATETIME HOUR TO SECOND, -- Hora en la que se realiza el intento
         p_diagnostico      SMALLINT,                -- Diagnostico de la cunsulta, marca y/o desmarca
         p_estado_cuenta    SMALLINT,                -- Estado resultado de la consulta, marca y/o desmarca
         p_saldo_viv92      DECIMAL(14,2),           -- Saldo de la subcuenta de vivienda 92
         p_saldo_viv97      DECIMAL(14,2),           -- Saldo de la subcuenta de vivienda 97
         p_usuario          CHAR(20),                -- Usuario que realiza la consulta
         p_id_solicitud     DECIMAL(9,0),            -- Numero de solicitud asociado
         p_caso_crm         CHAR(10),                -- Numero de caso CRM asignado
         p_origen           SMALLINT,                -- Origen de la ejecución
         v_cuenta           INTEGER

    --Reviso dependiendo el indicador marca
    CASE p_indicador_marca
       WHEN 1
          --Reviso en caso de ser Marcar
          LET v_cuenta = 0
          --
          SELECT COUNT(*)
          INTO v_cuenta
          FROM ret_ctr_marca_procesar_maestra
          WHERE nss = p_nss
          AND id_solicitud = p_id_solicitud
          AND estado_indicador IN (1,2)
          --
          IF v_cuenta = 1 THEN
             --Actualizo registro maestro
             UPDATE ret_ctr_marca_procesar_maestra
             SET estado_indicador = p_estado_indicador, origen = p_origen
             WHERE nss = p_nss
             AND id_solicitud = p_id_solicitud
             AND estado_indicador IN (1,2)
          ELSE
             --Inserto registro maestro
             INSERT INTO ret_ctr_marca_procesar_maestra VALUES (p_id_solicitud, p_nss, p_estado_indicador, p_origen)
          END IF
       WHEN 2
          IF p_estado_indicador = 6 THEN
             --Actualizo registro maestro de No marcado a Intentado desmarcar sin ser marcada.
             UPDATE ret_ctr_marca_procesar_maestra
             SET estado_indicador = p_estado_indicador, origen = p_origen
             WHERE nss = p_nss
             AND id_solicitud = p_id_solicitud
             AND estado_indicador = 1
          ELSE
             --Reviso en caso de ser Desmarcar
             LET v_cuenta = 0
             --
             SELECT COUNT(*)
             INTO v_cuenta
             FROM ret_ctr_marca_procesar_maestra
             WHERE nss = p_nss
             AND id_solicitud = p_id_solicitud
             AND estado_indicador IN (4,5)
             --
             IF v_cuenta = 1 THEN
                --Actualizo registro maestro
                UPDATE ret_ctr_marca_procesar_maestra
                SET estado_indicador = p_estado_indicador, origen = p_origen
                WHERE nss = p_nss
                AND id_solicitud = p_id_solicitud
                AND estado_indicador IN (4,5)
             ELSE
                --Inserto registro maestro
                INSERT INTO ret_ctr_marca_procesar_maestra VALUES (p_id_solicitud, p_nss, p_estado_indicador, p_origen)
             END IF
          END IF
       WHEN 3
          --Reviso en caso de ser Consultar
          LET v_cuenta = 0
          --
          IF p_id_solicitud IS NULL THEN
             SELECT COUNT(*)
             INTO v_cuenta
             FROM ret_ctr_marca_procesar_maestra
             WHERE nss = p_nss
             AND id_solicitud IS NULL
             AND estado_indicador IN (3,7)
          ELSE
             SELECT COUNT(*)
             INTO v_cuenta
             FROM ret_ctr_marca_procesar_maestra
             WHERE nss = p_nss
             AND id_solicitud = p_id_solicitud
             AND estado_indicador IN (3,7)
          END IF
          --
          IF v_cuenta = 1 THEN
             IF p_id_solicitud IS NULL THEN
                --Actualizo registro maestro
                UPDATE ret_ctr_marca_procesar_maestra
                SET estado_indicador = p_estado_indicador, origen = p_origen
                WHERE nss = p_nss
                AND id_solicitud IS NULL
                AND estado_indicador IN (3,7)
             ELSE
                --Actualizo registro maestro
                UPDATE ret_ctr_marca_procesar_maestra
                SET estado_indicador = p_estado_indicador, origen = p_origen
                WHERE nss = p_nss
                AND id_solicitud = p_id_solicitud
                AND estado_indicador IN (3,7)
             END IF
          ELSE
             --Inserto registro maestro
             INSERT INTO ret_ctr_marca_procesar_maestra VALUES (p_id_solicitud, p_nss, p_estado_indicador, p_origen)
          END IF
    END CASE

    --Inserto en la tabla histórica ret_ctr_marca_procesar
    INSERT 
    INTO   ret_ctr_marca_procesar (nss, indicador_marca, estado_indicador, f_intento, 
                                    h_intento, diagnostico, estado_cuenta, saldo_viv92,
                                    saldo_viv97, usuario, id_solicitud, caso_crm, origen)
                           VALUES (p_nss, p_indicador_marca, p_estado_indicador, p_f_intento,
                                   p_h_intento, p_diagnostico, p_estado_cuenta, p_saldo_viv92, 
                                   p_saldo_viv97, p_usuario, p_id_solicitud, p_caso_crm, p_origen);

                                   
         

END FUNCTION
{
======================================================================
Clave: 
Nombre: fn_recupera_saldo_consulta
Fecha creacion: Diciembre 29, 2015
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Consulta el último saldo devuelto por Procesar

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recupera_saldo_consulta(p_nss)
DEFINE   p_nss              LIKE afi_derechohabiente.nss, -- NSS del trabajador
         v_aivs_viv92       DECIMAL(24,6),
         v_aivs_viv97       DECIMAL(24,6),
         v_pesos_viv92      DECIMAL(22,2),
         v_pesos_viv97      DECIMAL(22,2),
         p_indicador_marca  SMALLINT,                -- Indica el tipo de accion 1 - marca, 2 - desmarca y 3 - consulta
         p_estado_indicador SMALLINT,                -- indica el evento en el que se encuentra el indicador de la marca
         v_f_intento        DATE,                    -- Fecha en la que se realiza el intento
         v_h_intento        DATETIME HOUR TO SECOND, -- Hora en la que se realiza el intento
         p_diagnostico      SMALLINT,                -- Diagnostico de la cunsulta, marca y/o desmarca
         p_estado_cuenta    SMALLINT,                -- Estado resultado de la consulta, marca y/o desmarca
         p_saldo_viv92      DECIMAL(14,2),           -- Saldo de la subcuenta de vivienda 92
         p_saldo_viv97      DECIMAL(14,2),           -- Saldo de la subcuenta de vivienda 97
         p_usuario          CHAR(20),                -- Usuario que realiza la consulta
         p_id_solicitud     DECIMAL(9,0),            -- Numero de solicitud asociado
         p_caso_crm         DECIMAL(9,0)             -- Numero de caso CRM asignado

    DISPLAY "Procesa consulta ";


    LET v_aivs_viv92  = 0
    LET v_pesos_viv92 = 0
    LET v_aivs_viv97  = 0
    LET v_pesos_viv97 = 0
    
    SELECT MAX(f_intento)
    INTO   v_f_intento
    FROM   ret_ctr_marca_procesar
    WHERE  nss           = p_nss
    AND    diagnostico   = 101
    AND    estado_cuenta = 101

    IF v_f_intento IS NOT NULL THEN 
        SELECT MAX(h_intento)
        INTO   v_h_intento
        FROM   ret_ctr_marca_procesar
        WHERE  nss           = p_nss
        AND    diagnostico   = 101
        AND    estado_cuenta = 101
        AND    f_intento     = v_f_intento
        IF v_h_intento IS NOT NULL THEN 

            SELECT saldo_viv92, saldo_viv92 * b.precio_fondo, saldo_viv97, saldo_viv97 * b.precio_fondo
            INTO   v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97
            FROM   ret_ctr_marca_procesar a,
                  (SELECT NVL(precio_fondo,0) AS precio_fondo
                   FROM   glo_valor_fondo
                   WHERE  f_valuacion = (SELECT last_day(add_months(TODAY, -1))+1 
                                         FROM   (SELECT LIMIT 1 1 
                                                 FROM   systables))
                   AND    fondo = 11) b
            WHERE  indicador_marca = 3
            AND    nss             = p_nss
            AND    f_intento       = v_f_intento
            AND    h_intento       = v_h_intento
            AND    diagnostico     = 101
            AND    estado_cuenta   = 101
        END IF 
    END IF 
    RETURN v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97
         
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_hash
Fecha creacion: Marzo 03, 2016
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
Genera un código HASH (pasado por parámetro) de una cadena de texto STRING
(pasada por parámetro),

códigos HASH permitidos:
  - SHA1 (Recomendado)
  - SHA512
  - SHA384
  - SHA256
  - SHA224
  - MD5

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_hash(toDigest, algo)

   DEFINE toDigest, algo, result STRING
   DEFINE dgst security.Digest

   IF algo IS NULL OR algo = "" THEN
      LET algo = "SHA1" --(Default)
   END IF

   TRY
      LET dgst = security.Digest.CreateDigest(algo)
      CALL dgst.AddStringData(toDigest)
      --LET result = dgst.DoBase64Digest()
      LET result = dgst.DoHexBinaryDigest()
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      EXIT PROGRAM(-1)
   END TRY

   RETURN result
   
END FUNCTION

FUNCTION fn_busca_mov_cargo_fa(p_nss) 
DEFINE p_nss        CHAR(11)
DEFINE v_fecha      DATE
DEFINE v_movimiento CHAR(18)
DEFINE v_importe    DECIMAL(12,2)
DEFINE v_tipo_cargo CHAR(1)
DEFINE v_contador   INTEGER
DEFINE v_sql        STRING 

  -- Busca en la tabla histórica los movimientos de cargo
   LET v_tipo_cargo = ""
   LET v_movimiento = ""
   LET v_importe    = 0
   LET v_fecha      = NULL
  
   SELECT COUNT(*)
   INTO   v_contador
   FROM   cta_his_fondo72 a,
          cta_his_fondo72_complemento b
   WHERE  a.nss = p_nss
   AND    a.clave_mov IN ('RE','TR')
   AND    a.id_cta_his_fondo72 = b.id_cta_his_fondo72;
   IF v_contador > 0 THEN
      DISPLAY "Entro por el histórico"
      LET v_sql = "\nSELECT FIRST 1 b.cve_movimiento, ABS(a.importe), b.f_movimiento, ",
                  "\n       CASE a.clave_mov WHEN 'RE' THEN 'D' ELSE 'T' END     ",
                  "\nFROM  cta_his_fondo72 a,                          ",
                  "\n      cta_his_fondo72_complemento b               ",
                  "\nWHERE a.nss            = ?                        ", -- el trabajador
                  "\nAND   a.clave_mov IN ('RE','TR')                  ", -- el tipo de movimiento para determinar si es traspaso o devolución
                  "\nAND   a.id_cta_his_fondo72 = b.id_cta_his_fondo72 ",
                  "\nORDER BY b.f_movimiento DESC                      " 
      PREPARE pr_det_his FROM v_sql
      EXECUTE pr_det_his USING p_nss INTO v_movimiento, v_importe, v_fecha, v_tipo_cargo
   ELSE --- Se busca en los movimientos aplicados por el contingente
      SELECT COUNT(*)
      INTO v_contador
      FROM   cta_fondo72 a,
             afi_fondo72 b,
             ret_fondo_ahorro d
      WHERE  a.id_afi_fondo72 = b.id_afi_fondo72
      AND    a.id_referencia = d.id_solicitud
      AND    b.nss = p_nss
      AND    a.movimiento = 182
      IF v_contador > 0 THEN 
         DISPLAY "Entro por el contingente"
         SELECT FIRST 1 d.cve_referencia, ABS(a.importe), a.f_liquida, 'D'
         INTO   v_movimiento, v_importe, v_fecha, v_tipo_cargo
         FROM   cta_fondo72 a,
                afi_fondo72 b,
                ret_fondo_ahorro d
         WHERE  a.id_afi_fondo72 = b.id_afi_fondo72
         AND    a.id_referencia = d.id_solicitud
         AND    b.nss = p_nss
         AND    a.movimiento = 182
         ORDER BY a.f_liquida DESC
      END IF
   END IF 
   DISPLAY "Los valores regresados ",v_tipo_cargo, "-", v_fecha, "-", v_movimiento, "-", v_importe 

RETURN v_tipo_cargo, v_fecha, v_movimiento, v_importe
END FUNCTION

FUNCTION fn_valida_solicitud_clabe(p_nss, p_caso_adai, p_cuenta_clabe) 
DEFINE p_nss           CHAR(11)
DEFINE p_caso_adai     CHAR(10)
DEFINE p_cuenta_clabe  CHAR(18)
DEFINE v_sql           STRING
DEFINE v_cuenta_clabes SMALLINT  

  -- Busca en la tabla histórica los movimientos de cargo
   LET v_cuenta_clabes = 0
   SELECT COUNT(*)
   INTO   v_cuenta_clabes
   FROM   ret_solicitud_generico a,
          ret_beneficiario_generico b,
          ret_pago_spei c
   WHERE  a.id_solicitud = b.id_solicitud
   AND    b.id_solicitud = c.id_solicitud
   AND    b.consec_beneficiario = c.consec_beneficiario
   AND    c.cuenta_clabe = p_cuenta_clabe
   AND    a.nss <> p_nss

   IF v_cuenta_clabes = 0 THEN
      --- Se verifica si entro por siaff
      SELECT COUNT(*)
      INTO   v_cuenta_clabes
      FROM   ret_solicitud_generico a,
             ret_beneficiario_generico b,
             ret_pago_siaf c
      WHERE  a.id_solicitud = b.id_solicitud
      AND    b.id_solicitud = c.id_solicitud
      AND    b.consec_beneficiario = c.consec_beneficiario
      AND    c.cuenta_clabe = p_cuenta_clabe
      AND    a.nss <> p_nss
      IF v_cuenta_clabes = 0 THEN
      
         SELECT COUNT(*)
         INTO   v_cuenta_clabes
         FROM   ret_solicitud_generico a,
                ret_beneficiario_generico b,
                ret_pago_spei c
         WHERE  a.id_solicitud = b.id_solicitud
         AND    b.id_solicitud = c.id_solicitud
         AND    b.consec_beneficiario = c.consec_beneficiario
         AND    c.cuenta_clabe = p_cuenta_clabe
         AND    a.nss = p_nss
         AND    b.tpo_beneficiario = 2
         IF v_cuenta_clabes <= 1 THEN 
            LET v_cuenta_clabes = 0
         END IF
      END IF 
   END IF 

   RETURN v_cuenta_clabes

END FUNCTION

