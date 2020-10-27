--==============================================================================
################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPWS04                                                       #
#Objetivo     => Solicitud de Validación de Derecho de Portabilidad            #
#Fecha inicio => 28 de Junio de 2016                                           #
################################################################################

DATABASE "safre_viv"

DEFINE sp_respuesta_solicitud RECORD
   nss                CHAR(11),
   diagMarcaActiva    CHAR(1),
   codAforeActiva     CHAR(3),
   descAforeActiva    CHAR(40),
   diagMarcaHistorica CHAR(1),
   codAforeHistorica  CHAR(3),
   descAforeHistorica CHAR(40),
   resultadoOperacion SMALLINT
END RECORD

--CONSULTA MARCA ACTUAL DE SEPARACION DE CUENTAS 280 y LA HISTORICA MAS ACTUAL DIFERENTE A LA MARCA ACTIVA
FUNCTION fn_consulta_marca_sep(p_nss)
 
   DEFINE p_nss           CHAR(11)
   DEFINE p_id_derechohab DECIMAL(9,0)
   DEFINE p_referencia_a  DECIMAL(9,0)
   DEFINE p_referencia_h  DECIMAL(9,0)
   DEFINE p_cod_afore_a   CHAR(3)
   DEFINE p_cod_afore_h   CHAR(3)
   DEFINE p_desc_afore_a  CHAR(40)
   DEFINE p_desc_afore_h  CHAR(40)
   DEFINE p_marca_a       SMALLINT
   DEFINE p_marca_h       SMALLINT

   WHENEVER SQLERROR CALL fn_error_consulta

   --Valores iniciales
   INITIALIZE sp_respuesta_solicitud.* TO NULL
   INITIALIZE p_id_derechohab TO NULL
   INITIALIZE p_referencia_a  TO NULL
   INITIALIZE p_referencia_h  TO NULL 
   INITIALIZE p_marca_a       TO NULL 
   INITIALIZE p_marca_h       TO NULL 
   INITIALIZE p_cod_afore_a   TO NULL
   INITIALIZE p_cod_afore_h   TO NULL
   INITIALIZE p_desc_afore_a  TO NULL
   INITIALIZE p_desc_afore_h  TO NULL
   LET sp_respuesta_solicitud.nss = p_nss   
   
   -- BUSCANDO EL NSS
   DISPLAY "BUSCO NSS: ",p_nss
   SELECT id_derechohabiente 
      INTO p_id_derechohab
      FROM afi_derechohabiente
      WHERE nss = p_nss
   IF p_id_derechohab IS NULL THEN  --Si no existe NSS responde diagnostico operacion 1
      DISPLAY "NO EXISTE NSS"
      LET sp_respuesta_solicitud.resultadoOperacion = 1
      RETURN sp_respuesta_solicitud.*   
   ELSE
      LET sp_respuesta_solicitud.resultadoOperacion = 0
   END IF

   --BUSCANDO MARCA ACTIVA
   DISPLAY "BUSCO MARCA ACTIVA DE ID: ",p_id_derechohab
   SELECT FIRST 1 marca,n_referencia
      INTO p_marca_a,p_referencia_a
      FROM sfr_marca_activa
      WHERE marca in (280,702)
      AND id_derechohabiente = p_id_derechohab
DISPLAY "marca: ",p_marca_a," referencia: ",p_referencia_a 
   IF p_referencia_a IS NOT NULL THEN
      LET sp_respuesta_solicitud.diagMarcaActiva = 1
      -- Ahora buscamos en la tabla de operación 27 la afore correspondiente usando la referencia
      IF p_marca_a = 280 THEN -- para invadido marca 280
         SELECT NVL(cve_entidad_admon,0)
         INTO p_cod_afore_a
         FROM sep_det_02_op27
         WHERE id_det_02_op27 = p_referencia_a
      ELSE   -- para asociado marca 702
         SELECT NVL(a.cve_entidad_admon,0)
         INTO p_cod_afore_a
         FROM sep_det_02_op27 a ,
              sep_det_03_op27 b
         WHERE b.id_det_03_op27 = p_referencia_a
         AND   b.id_det_02_op27 = a.id_det_02_op27
DISPLAY "codigo afore: ",p_cod_afore_a         
      END IF
      --Teniendo el código de la afore obtenemos su descripcion
      SELECT afore_desc
         INTO p_desc_afore_a
         FROM cat_afore
         WHERE afore_cod = p_cod_afore_a
      LET sp_respuesta_solicitud.codAforeActiva = p_cod_afore_a
      LET sp_respuesta_solicitud.descAforeActiva = p_desc_afore_a
      DISPLAY p_cod_afore_a,"-",p_desc_afore_a
   ELSE  -- SI NO HAY REFERENCIA, NO HAY MARCA
      LET sp_respuesta_solicitud.diagMarcaActiva = 2
   END IF  

   -- BUSCANDO MARCA HISTORICA MAS RECIENTE Y DIFERENTE A LA ACTIVA
   DISPLAY "BUSCO MARCA HISTORICA DE ID: ",p_id_derechohab
   IF p_referencia_a IS NULL THEN
      SELECT FIRST 1 marca,n_referencia
         INTO p_marca_h,p_referencia_h
         FROM sfr_marca_historica
         WHERE marca in (280,702)
         AND id_derechohabiente = p_id_derechohab
         AND f_fin IS NOT NULL
         ORDER BY f_inicio,h_inicio DESC      
   ELSE
      SELECT FIRST 1 marca,n_referencia
         INTO p_marca_h,p_referencia_h
         FROM sfr_marca_historica
         WHERE marca = p_marca_a
         AND id_derechohabiente = p_id_derechohab
         AND n_referencia <> p_referencia_a
         AND f_fin IS NOT NULL
         ORDER BY f_inicio,h_inicio DESC   
   END IF

   IF p_referencia_h IS NOT NULL THEN
      LET sp_respuesta_solicitud.diagMarcaHistorica = 3
      -- Ahora buscamos en la tabla de operación 27 la afore correspondiente usando la referencia
      IF p_marca_h = 280 THEN
         SELECT NVL(cve_entidad_admon,0)
         INTO p_cod_afore_h
         FROM sep_det_02_op27
         WHERE id_det_02_op27 = p_referencia_h
      ELSE 
         SELECT NVL(a.cve_entidad_admon,0)
         INTO p_cod_afore_h
         FROM sep_det_02_op27 a ,
              sep_det_03_op27 b
         WHERE b.id_det_03_op27 = p_referencia_h
         AND   b.id_det_02_op27 = a.id_det_02_op27 
      END IF

      --Teniendo el código de la afore obtenemos su descripcion
      SELECT afore_desc
         INTO p_desc_afore_h
         FROM cat_afore
         WHERE afore_cod = p_cod_afore_h
      LET sp_respuesta_solicitud.codAforeHistorica = p_cod_afore_h
      LET sp_respuesta_solicitud.descAforeHistorica = p_desc_afore_h
      DISPLAY p_cod_afore_h,"-",p_desc_afore_h
   ELSE  -- SI NO HAY REFERENCIA, NO HAY MARCA
      LET sp_respuesta_solicitud.diagMarcaHistorica = 4
   END IF

   DISPLAY "CONSULTA EXITOSA, DEVUELVO: "
   DISPLAY sp_respuesta_solicitud.nss,"\n",
   sp_respuesta_solicitud.diagMarcaActiva,"\n",
   sp_respuesta_solicitud.codAforeActiva,"\n",
   sp_respuesta_solicitud.descAforeActiva,"\n",
   sp_respuesta_solicitud.diagMarcaHistorica,"\n",
   sp_respuesta_solicitud.codAforeHistorica,"\n",
   sp_respuesta_solicitud.descAforeHistorica,"\n",
   sp_respuesta_solicitud.resultadoOperacion
   
   RETURN sp_respuesta_solicitud.*

END FUNCTION

--FUNCION SI OCURRE ALGUN ERROR EN LA CONSULTA, IMPRIME MENSAJE DE ERROR
FUNCTION fn_error_consulta()

   INITIALIZE sp_respuesta_solicitud.* TO NULL
   DISPLAY "SE HA PRESENTADO EL SIGUIENTE ERROR AL CONSULTAR LA BASE DE DATOS: "
   DISPLAY "-> ",SQLCA.sqlcode," : ",SQLCA.sqlerrm
   EXIT PROGRAM
   
END FUNCTION
