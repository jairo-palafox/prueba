-- ==================================================================
-- variables globales y funciones para consulta de cifras de control
-- de retiros
DATABASE safre_viv

GLOBALS
DEFINE ar_ret_modalidad_retiro RECORD LIKE ret_modalidad_retiro.*
DEFINE arr_cifras_control DYNAMIC ARRAY  OF RECORD  
               v_concepto        STRING 
              ,v_concepto_aux    STRING
              ,v_registros       INTEGER 
              ,v_viv92           DECIMAL (24,6)
              ,v_viv92_pesos     DECIMAL (24,2)
              ,v_viv97           DECIMAL (24,6)
              ,v_viv97_pesos     DECIMAL (24,2)
              ,v_viv72           DECIMAL (24,2)
       END RECORD  
       ,v_folio          DECIMAL(9,0)

DEFINE r_ret_cza_disposicion  RECORD
                folio                    DECIMAL(9,0)
                ,nombre_archivo          CHAR(20)
                ,f_operacion_proceso     DATE
                ,f_carga                 DATE
                ,h_carga                 DATETIME HOUR TO SECOND
                ,f_valor_transferencia   DATE
                ,precio_fondo            DECIMAL(14,6)
                ,total_registros         INTEGER
                ,total_importe           DECIMAL(22,2)
                ,usuario                 CHAR(20)
      END RECORD

DEFINE r_ret_cza_transferencia  RECORD
              folio                  DECIMAL(9,0)
             ,nombre_archivo         CHAR(20)
             ,f_operacion_proceso    DATE
             ,f_carga                DATE
             ,h_carga                DATETIME HOUR TO MINUTE
             ,f_valor_transferencia  DATE
             ,precio_fondo           DECIMAL(14,6)
             ,total_registros        INTEGER
             ,total_importe          DECIMAL(22,2)
             ,usuario                CHAR(20)
       END RECORD
  
DEFINE r_ret_cza_tipo_n  RECORD
                folio                 DECIMAL(9,0)            
                ,nombre_archivo       CHAR(20)               
                ,f_operacion_proceso  DATE                   
                ,f_carga              DATE                   
                ,hora_carga           DATETIME HOUR TO SECOND
                ,total_registros      INTEGER                
                ,total_ret92          DECIMAL(17,2)          
                ,total_viv92          DECIMAL(24,6) 
  END RECORD
DEFINE v_status_folio   SMALLINT  

END GLOBALS 


{
======================================================================
Clave: 
Nombre: fn_carga_archivo_dispo
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Consulta los datos de registros cargados de Disposicio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_carga_archivo_dispo(p_indice,p_estado_ini,p_estado_fin,p_desc_concp,p_desc_concp_aux)
DEFINE p_indice           SMALLINT,
       p_estado_ini       SMALLINT, 
       p_estado_fin       SMALLINT,
       p_desc_concp       STRING  ,
       p_desc_concp_aux   STRING  ,
       v_conteo_en_rch    INTEGER ,
       v_suma_aivs97_rch  DECIMAL(24,6),
       v_suma_aivs92_rch  DECIMAL(24,6),
       v_suma_viv72_rch   DECIMAL(22,2),
       v_conteo_en_acep   INTEGER,
       v_suma_aivs97_acep DECIMAL(24,6),
       v_suma_aivs92_acep DECIMAL(24,6),
       v_suma_viv72_acep  DECIMAL(22,2)
                                               
   LET arr_cifras_control[p_indice].v_registros = 0
   LET arr_cifras_control[p_indice].v_viv92     = 0
   LET arr_cifras_control[p_indice].v_viv97     = 0
   LET arr_cifras_control[p_indice].v_viv72     = 0

   -- cifras de aceptados
   LET v_conteo_en_acep   = 0
   LET v_suma_aivs97_acep = 0
   LET v_suma_aivs92_acep = 0
   LET v_suma_viv72_acep  = 0

   -- cifras de rechazos en rch
   LET v_conteo_en_rch   = 0
   LET v_suma_aivs97_rch = 0
   LET v_suma_aivs92_rch = 0
   LET v_suma_viv72_rch  = 0

   
   SELECT COUNT(*)          ,
          SUM(aivs_viv97)   ,
          SUM(aivs_viv92)   ,
          SUM(importe_viv72)
   INTO  v_conteo_en_acep   ,
         v_suma_aivs97_acep ,
         v_suma_aivs92_acep ,
         v_suma_viv72_acep
   FROM  ret_disposicion
   WHERE folio = v_folio
   AND   estado_solicitud BETWEEN p_estado_ini AND p_estado_fin

   -- si no hay, es cero
   IF ( v_conteo_en_acep IS NULL ) THEN 
      LET v_conteo_en_acep = 0
   END IF

   IF ( v_suma_aivs97_acep IS NULL ) THEN 
      LET v_suma_aivs97_acep = 0
   END IF

   IF ( v_suma_aivs92_acep IS NULL ) THEN 
      LET v_suma_aivs92_acep = 0
   END IF

   IF ( v_suma_viv72_acep IS NULL ) THEN 
      LET v_suma_viv72_acep = 0
   END IF
   
   -- se cuentan los registros rechazados en tabla de rechazo
   IF ( p_estado_ini = 100 ) THEN
      SELECT COUNT(*)
             ,SUM(aivs_viv97)
             ,SUM(aivs_viv92) 
             ,SUM(importe_viv72)
      INTO   v_conteo_en_rch
             ,v_suma_aivs97_rch
             ,v_suma_aivs92_rch
             ,v_suma_viv72_rch
      FROM  ret_disposicion_rch
      WHERE folio = v_folio

      -- si no hay, es cero
      IF ( v_conteo_en_rch IS NULL ) THEN 
         LET v_conteo_en_rch = 0
      END IF

      IF ( v_suma_aivs97_rch IS NULL ) THEN 
         LET v_suma_aivs97_rch = 0
      END IF

      IF ( v_suma_aivs92_rch IS NULL ) THEN 
         LET v_suma_aivs92_rch = 0
      END IF

      IF ( v_suma_viv72_rch IS NULL ) THEN 
         LET v_suma_viv72_rch = 0
      END IF
   END IF

   DISPLAY "Conteo: ", v_conteo_en_acep
   DISPLAY "viv97 : ", v_suma_aivs97_acep 
   DISPLAY "viv92 : ", v_suma_aivs92_acep 
   DISPLAY "viv72 : ", v_suma_viv72_acep 

   DISPLAY "Conteo_rch: ", v_conteo_en_rch
   DISPLAY "viv97_rch : ", v_suma_aivs97_rch
   DISPLAY "viv92_rch : ", v_suma_aivs92_rch
   DISPLAY "viv72_rch : ", v_suma_viv72_rch
   
   -- se suman los rechazos en RCH a los rechazos en disposicion
   LET arr_cifras_control[p_indice].v_registros = v_conteo_en_acep   + v_conteo_en_rch
   LET arr_cifras_control[p_indice].v_viv92     = v_suma_aivs92_acep + v_suma_aivs92_rch
   LET arr_cifras_control[p_indice].v_viv97     = v_suma_aivs97_acep + v_suma_aivs97_rch
   LET arr_cifras_control[p_indice].v_viv72     = v_suma_viv72_acep  + v_suma_viv72_rch

   
   LET arr_cifras_control[p_indice].v_concepto     = p_desc_concp
   LET arr_cifras_control[p_indice].v_concepto_aux = p_desc_concp_aux
     
   IF arr_cifras_control[p_indice].v_registros <= 0 THEN
      LET arr_cifras_control[p_indice].v_viv92 = 0
      LET arr_cifras_control[p_indice].v_viv97 = 0
      LET arr_cifras_control[p_indice].v_viv72 = 0
   END IF
END FUNCTION   

{
======================================================================
Clave: 
Nombre: fn_carga_archivo_transfe
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Consulta los datos cargados de Retiros por Transferencia

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_carga_archivo_transfe(p_indice,p_estado_ini,p_estado_fin,p_desc_concp,p_desc_concp_aux)
DEFINE p_indice           SMALLINT,
       p_estado_ini       SMALLINT, 
       p_estado_fin       SMALLINT,
       p_desc_concp       STRING  ,
       p_desc_concp_aux   STRING  ,
       v_conteo_en_rch    INTEGER ,
       v_suma_aivs97_rch  DECIMAL(24,6),
       v_suma_aivs92_rch  DECIMAL(24,6),
       v_suma_viv72_rch   DECIMAL(22,2),
       v_conteo_en_acep   INTEGER,
       v_suma_aivs97_acep DECIMAL(24,6),
       v_suma_aivs92_acep DECIMAL(24,6),
       v_suma_viv72_acep  DECIMAL(22,2)

   -- se inician las variables
   LET v_conteo_en_rch    = 0
   LET v_suma_aivs97_rch  = 0
   LET v_suma_aivs92_rch  = 0
   LET v_suma_viv72_rch   = 0
   LET v_conteo_en_acep   = 0
   LET v_suma_aivs97_acep = 0
   LET v_suma_aivs92_acep = 0
   LET v_suma_viv72_acep  = 0
   LET arr_cifras_control[p_indice].v_viv92 = 0 -- transferencia solo tiene viv97
   LET arr_cifras_control[p_indice].v_viv97 = 0 
   LET arr_cifras_control[p_indice].v_viv72 = 0 -- transferencia solo tiene viv97

   -- se obtienen los datos de la tabla de registros aceptados
   SELECT COUNT(*)       ,
          SUM(aivs_viv97)
   INTO   v_conteo_en_acep  ,
          v_suma_aivs97_acep
   FROM   ret_transferencia
   WHERE  folio = v_folio
   AND    estado_solicitud BETWEEN p_estado_ini AND p_estado_fin

   -- si no hay, es cero
   IF ( v_conteo_en_acep IS NULL ) THEN 
      LET v_conteo_en_acep = 0
   END IF

   IF ( v_suma_aivs97_acep IS NULL ) THEN 
      LET v_suma_aivs97_acep = 0
   END IF

   -- se verifica si se solicitaron los datos de la tabla de rechazos
   IF ( p_estado_ini = 100 ) THEN
      -- se consultan los datos rechazados
      SELECT COUNT(*)       ,
             SUM(aivs_viv97)
      INTO   v_conteo_en_rch  ,
             v_suma_aivs97_rch
      FROM   ret_transferencia_rch
      WHERE  folio = v_folio

      -- si no hay, es cero
      IF ( v_conteo_en_rch IS NULL ) THEN 
         LET v_conteo_en_rch = 0
      END IF

      IF ( v_suma_aivs97_rch IS NULL ) THEN 
         LET v_suma_aivs97_rch = 0
      END IF
   END IF

   -- se asignan los datos al arreglo
   LET arr_cifras_control[p_indice].v_concepto     = p_desc_concp
   LET arr_cifras_control[p_indice].v_concepto_aux = p_desc_concp_aux
   LET arr_cifras_control[p_indice].v_registros    = v_conteo_en_acep + v_conteo_en_rch
   LET arr_cifras_control[p_indice].v_viv97        = v_suma_aivs97_acep + v_suma_aivs97_rch

   -- si no se encontro monto, se deja en cero
   IF ( arr_cifras_control[p_indice].v_registros <= 0 ) THEN
      LET arr_cifras_control[p_indice].v_viv92 = 0
      LET arr_cifras_control[p_indice].v_viv97 = 0
      LET arr_cifras_control[p_indice].v_viv72 = 0
   END IF
END FUNCTION     

{
======================================================================
Clave: 
Nombre: fn_carga_archivo_tipo_n
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Consulta los datos cargados de Retiro Tipo N

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_carga_archivo_tipo_n(p_indice,p_estado_ini,p_estado_fin,p_desc_concp,p_desc_concp_aux)
DEFINE p_indice           SMALLINT,
       p_estado_ini       SMALLINT, 
       p_estado_fin       SMALLINT,
       p_desc_concp       STRING  ,
       p_desc_concp_aux   STRING  ,
       v_conteo_en_rch    INTEGER ,
       v_suma_aivs97_rch  DECIMAL(24,6),
       v_suma_aivs92_rch  DECIMAL(24,6),
       v_suma_viv72_rch   DECIMAL(22,2),
       v_conteo_en_acep   INTEGER,
       v_suma_aivs97_acep DECIMAL(24,6),
       v_suma_aivs92_acep DECIMAL(24,6),
       v_suma_viv72_acep  DECIMAL(22,2)

   -- se inician las variables
   LET v_conteo_en_rch    = 0
   LET v_suma_aivs97_rch  = 0
   LET v_suma_aivs92_rch  = 0
   LET v_suma_viv72_rch   = 0
   LET v_conteo_en_acep   = 0
   LET v_suma_aivs97_acep = 0
   LET v_suma_aivs92_acep = 0
   LET v_suma_viv72_acep  = 0
   LET arr_cifras_control[p_indice].v_viv92 = 0 
   LET arr_cifras_control[p_indice].v_viv97 = 0 -- tipo N solo tiene viv92
   LET arr_cifras_control[p_indice].v_viv72 = 0 -- tipo N solo tiene viv92

   -- se obtienen los datos de la tabla de historicos
   SELECT COUNT(*)       ,
          SUM(aivs_viv92)
   INTO   v_conteo_en_acep,
          v_suma_aivs92_acep
   FROM   ret_tipo_n
   WHERE  folio = v_folio
   AND    estado_solicitud BETWEEN p_estado_ini AND p_estado_fin

   -- si no hay, es cero
   IF ( v_conteo_en_acep IS NULL ) THEN 
      LET v_conteo_en_acep = 0
   END IF

   IF ( v_suma_aivs92_acep IS NULL ) THEN 
      LET v_suma_aivs92_acep = 0
   END IF

   -- se verifica si se solicitaron los datos de la tabla de rechazos
   IF ( p_estado_ini = 100 ) THEN
      -- se consultan los datos rechazados
      SELECT COUNT(*)       ,
             SUM(aivs_viv92)
      INTO   v_conteo_en_rch  ,
             v_suma_aivs92_rch
      FROM   ret_tipo_n_rch
      WHERE  folio = v_folio

      -- si no hay, es cero
      IF ( v_conteo_en_rch IS NULL ) THEN 
         LET v_conteo_en_rch = 0
      END IF

      IF ( v_suma_aivs92_rch IS NULL ) THEN 
         LET v_suma_aivs92_rch = 0
      END IF
   END IF

   DISPLAY "Aivs acept: ", v_suma_aivs92_acep
   DISPLAY "Aivs rch  : ", v_suma_aivs92_rch
   
   -- se asignan los datos al arreglo
   LET arr_cifras_control[p_indice].v_concepto     = p_desc_concp
   LET arr_cifras_control[p_indice].v_concepto_aux = p_desc_concp_aux
   LET arr_cifras_control[p_indice].v_registros    = v_conteo_en_acep + v_conteo_en_rch
   LET arr_cifras_control[p_indice].v_viv92        = v_suma_aivs92_acep + v_suma_aivs92_rch

   -- si no hay datos, se queda en ceros 
   IF arr_cifras_control[p_indice].v_registros <= 0 THEN
      LET arr_cifras_control[p_indice].v_viv92 = 0
      LET arr_cifras_control[p_indice].v_viv97 = 0
      LET arr_cifras_control[p_indice].v_viv72 = 0
   END IF
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_recuperar_monto_preliq
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Consulta los datos preliquidados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recuperar_monto_preliq(p_movimiento,p_subcuenta)
DEFINE p_subcuenta  SMALLINT  
DEFINE p_registros  INTEGER  
DEFINE p_movimiento SMALLINT
DEFINE v_monto      DECIMAL(14,6)
DEFINE v_fondo      SMALLINT 
   LET v_fondo       = 11

  SELECT NVL(SUM(monto_acciones),0), NVL(COUNT (*),0)
    INTO v_monto ,p_registros
    FROM ret_preliquida
   WHERE folio_liquida     = v_folio
    -- AND movimiento        = p_movimiento 
     AND subcuenta         = p_subcuenta
     AND fondo_inversion   = v_fondo

     --DISPLAY  v_folio," ",p_movimiento," ",p_subcuenta," ",v_fondo
     --DISPLAY  v_monto ," ",p_registros

RETURN  v_monto ,p_registros
END FUNCTION  

{
======================================================================
Clave: 
Nombre: fn_recuperar_monto_preliq_decreto
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Consulta los montos preliquidados en Decreto

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recuperar_monto_preliq_decreto(p_movimiento,p_subcuenta)
DEFINE p_subcuenta  SMALLINT  
DEFINE p_registros  INTEGER  
DEFINE p_movimiento SMALLINT
DEFINE v_monto      DECIMAL(14,6)
DEFINE v_fondo      SMALLINT 
   LET v_fondo       = 11

  SELECT NVL(SUM(monto_acciones),0), NVL(COUNT (*),0)
    INTO v_monto ,p_registros
    FROM ret_preliquida92
   WHERE folio_liquida     = v_folio
    -- AND movimiento        = p_movimiento 
     AND subcuenta         = p_subcuenta
     AND fondo_inversion   = v_fondo

     --DISPLAY  v_folio," ",p_movimiento," ",p_subcuenta," ",v_fondo
     --DISPLAY  v_monto ," ",p_registros

RETURN  v_monto ,p_registros
END FUNCTION  

{
======================================================================
Clave: 
Nombre: fn_recuperar_monto_liq
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Consulta los datos liquidados en cta_movimiento

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recuperar_monto_liq(p_movimiento,p_subcuenta)
DEFINE p_subcuenta  SMALLINT  
DEFINE p_registros  INTEGER  
DEFINE p_movimiento SMALLINT
DEFINE v_monto      DECIMAL(14,6)
DEFINE v_fondo      SMALLINT
 
   LET v_fondo       = 11

   SELECT NVL(SUM(monto_acciones),0), NVL(COUNT (*),0)
     INTO v_monto ,p_registros
     FROM cta_movimiento
    WHERE folio_liquida     = v_folio
     -- AND movimiento        = p_movimiento 
      AND subcuenta         = p_subcuenta
      AND fondo_inversion   = v_fondo
RETURN  v_monto ,p_registros
END FUNCTION  

{
======================================================================
Clave: 
Nombre: fn_recuperar_monto_liq_decreto
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Consulta los datos liquidados en cta_decreto

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_recuperar_monto_liq_decreto(p_movimiento,p_subcuenta)
DEFINE p_subcuenta  SMALLINT  
DEFINE p_registros  INTEGER  
DEFINE p_movimiento SMALLINT
DEFINE v_monto      DECIMAL(14,6)
DEFINE v_fondo      SMALLINT
 
   LET v_fondo       = 11

   SELECT NVL(SUM(monto_acciones),0), NVL(COUNT (*),0)
     INTO v_monto ,p_registros
     FROM cta_decreto
    WHERE folio_liquida     = v_folio
     -- AND movimiento        = p_movimiento 
      AND subcuenta         = p_subcuenta
      AND fondo_inversion   = v_fondo
RETURN  v_monto ,p_registros
END FUNCTION  

{
======================================================================
Clave: 
Nombre: fn_carga_preliquidacion
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Carga los datos de preliquidacion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_carga_preliquidacion(p_indice,p_movimiento,p_desc_concp,p_desc_concp_aux)
DEFINE p_indice      SMALLINT 
DEFINE p_movimiento SMALLINT
DEFINE p_desc_concp STRING 
DEFINE p_desc_concp_aux STRING   
DEFINE v_count_registros INTEGER
LET v_count_registros = 0 
LET arr_cifras_control[p_indice].v_registros = 0
  
   CALL fn_recuperar_monto_preliq(p_movimiento,8) 
       RETURNING arr_cifras_control[p_indice].v_viv92, v_count_registros
      LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros   
    --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
    
   CALL fn_recuperar_monto_preliq(p_movimiento,4) 
         RETURNING arr_cifras_control[p_indice].v_viv97, v_count_registros
   LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
   --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
   
   CALL fn_recuperar_monto_preliq(p_movimiento,44) 
         RETURNING arr_cifras_control[p_indice].v_viv72 , v_count_registros
   LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
   --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros

   LET arr_cifras_control[p_indice].v_concepto     = p_desc_concp
   LET arr_cifras_control[p_indice].v_concepto_aux = p_desc_concp_aux
   
   IF arr_cifras_control[p_indice].v_registros <= 0 THEN
     LET arr_cifras_control[p_indice].v_viv92 = 0
     LET arr_cifras_control[p_indice].v_viv97 = 0
     LET arr_cifras_control[p_indice].v_viv72 = 0
   END IF
END FUNCTION  

{
======================================================================
Clave: 
Nombre: fn_carga_preliquidacion_decreto
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Carga los datos de preliquidación en decreto

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_carga_preliquidacion_decreto(p_indice,p_movimiento,p_desc_concp,p_desc_concp_aux)
   DEFINE p_indice          SMALLINT 
   DEFINE p_movimiento      SMALLINT
   DEFINE p_desc_concp      STRING 
   DEFINE p_desc_concp_aux  STRING   
   DEFINE v_count_registros INTEGER
   
      LET v_count_registros = 0 
      LET arr_cifras_control[p_indice].v_registros = 0
     
      CALL fn_recuperar_monto_preliq_decreto(p_movimiento,8) 
          RETURNING arr_cifras_control[p_indice].v_viv92, v_count_registros
         LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros   
       --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
       
      CALL fn_recuperar_monto_preliq_decreto(p_movimiento,4) 
            RETURNING arr_cifras_control[p_indice].v_viv97, v_count_registros
      LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
      --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
      
      CALL fn_recuperar_monto_preliq_decreto(p_movimiento,44) 
            RETURNING arr_cifras_control[p_indice].v_viv72 , v_count_registros
      LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
      --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
   
      LET arr_cifras_control[p_indice].v_concepto     = p_desc_concp
      LET arr_cifras_control[p_indice].v_concepto_aux = p_desc_concp_aux
      
      IF arr_cifras_control[p_indice].v_registros <= 0 THEN
        LET arr_cifras_control[p_indice].v_viv92 = 0
        LET arr_cifras_control[p_indice].v_viv97 = 0
        LET arr_cifras_control[p_indice].v_viv72 = 0
      END IF
END FUNCTION  

{
======================================================================
Clave: 
Nombre: fn_carga_liquidacion
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Carga los datos de liquidación

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_carga_liquidacion(p_indice,p_movimiento,p_desc_concp,p_desc_concp_aux)
DEFINE p_indice          SMALLINT 
DEFINE p_movimiento      SMALLINT
DEFINE p_desc_concp      STRING 
DEFINE p_desc_concp_aux  STRING   
DEFINE v_count_registros INTEGER

   LET v_count_registros = 0 
   LET arr_cifras_control[p_indice].v_registros = 0
  
   CALL fn_recuperar_monto_liq(p_movimiento,4) 
       RETURNING arr_cifras_control[p_indice].v_viv97, v_count_registros     
   LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
    --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
    
   CALL fn_recuperar_monto_liq(p_movimiento,8) 
         RETURNING arr_cifras_control[p_indice].v_viv92, v_count_registros
   LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
   --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
   
   CALL fn_recuperar_monto_liq(p_movimiento,44) 
         RETURNING arr_cifras_control[p_indice].v_viv72 , v_count_registros
   LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
   --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros

   LET arr_cifras_control[p_indice].v_concepto     = p_desc_concp
   LET arr_cifras_control[p_indice].v_concepto_aux = p_desc_concp_aux
   
   IF arr_cifras_control[p_indice].v_registros <= 0 THEN
     LET arr_cifras_control[p_indice].v_viv92 = 0
     LET arr_cifras_control[p_indice].v_viv97 = 0
     LET arr_cifras_control[p_indice].v_viv72 = 0
   END IF
END FUNCTION  

{
======================================================================
Clave: 
Nombre: fn_carga_liquidacion_decreto
Fecha creacion: Marzo 22, 2012
Autor: ERick Rodriguez
Narrativa del proceso que realiza:
Carga los datos de liquidación en decreto

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_carga_liquidacion_decreto(p_indice,p_movimiento,p_desc_concp,p_desc_concp_aux)
   DEFINE p_indice          SMALLINT 
   DEFINE p_movimiento      SMALLINT
   DEFINE p_desc_concp      STRING 
   DEFINE p_desc_concp_aux  STRING   
   DEFINE v_count_registros INTEGER
   
      LET v_count_registros = 0 
      LET arr_cifras_control[p_indice].v_registros = 0
     
      CALL fn_recuperar_monto_liq_decreto(p_movimiento,4) 
          RETURNING arr_cifras_control[p_indice].v_viv97, v_count_registros     
      LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
       --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
       
      CALL fn_recuperar_monto_liq_decreto(p_movimiento,8) 
            RETURNING arr_cifras_control[p_indice].v_viv92, v_count_registros
      LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
      --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
      
      CALL fn_recuperar_monto_liq_decreto(p_movimiento,44) 
            RETURNING arr_cifras_control[p_indice].v_viv72 , v_count_registros
      LET arr_cifras_control[p_indice].v_registros = "" --arr_cifras_control[p_indice].v_registros + v_count_registros
      --DISPLAY arr_cifras_control[p_indice].v_registros ," ", v_count_registros
   
      LET arr_cifras_control[p_indice].v_concepto     = p_desc_concp
      LET arr_cifras_control[p_indice].v_concepto_aux = p_desc_concp_aux
      
      IF arr_cifras_control[p_indice].v_registros <= 0 THEN
        LET arr_cifras_control[p_indice].v_viv92 = 0
        LET arr_cifras_control[p_indice].v_viv97 = 0
        LET arr_cifras_control[p_indice].v_viv72 = 0
      END IF
END FUNCTION  

{
======================================================================
Clave: 
Nombre: fn_calculo_diferencia_pre
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Calcula la diferencia de preliquidacion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_calculo_diferencia_pre(v_indice,v_indice_acep,v_indice_preliq,p_desc_concp,p_desc_concp_aux)
DEFINE v_indice         SMALLINT 
DEFINE v_indice_acep    SMALLINT
DEFINE v_indice_preliq  SMALLINT 
DEFINE p_desc_concp     STRING 
DEFINE p_desc_concp_aux STRING  

   SELECT a.status
   INTO   v_status_folio
   FROM   glo_folio a
   WHERE  a.folio = v_folio

  -- DISPLAY "@ PELIQ v_status_folio: ", v_status_folio
  -- DISPLAY "@ arreglo: ",arr_cifras_control[v_indice_acep].v_viv92 ," + ", arr_cifras_control[v_indice_preliq].v_viv92
  -- display "@ ..", arr_cifras_control[v_indice_acep].v_viv97  ," + ", arr_cifras_control[v_indice_preliq].v_viv97
  -- display "@ ..", arr_cifras_control[v_indice_acep].v_viv72  ," + ", arr_cifras_control[v_indice_preliq].v_viv72
   
   
   -- si aun no se ha preliquidado el folio, no hay diferencia
   IF ( v_status_folio > 0 ) THEN
     LET arr_cifras_control[v_indice].v_concepto     = p_desc_concp 
     LET arr_cifras_control[v_indice].v_concepto_aux = p_desc_concp_aux 
     LET arr_cifras_control[v_indice].v_registros    = "" --(arr_cifras_control[v_indice_acep].v_registros) - arr_cifras_control[v_indice_preliq].v_registros
     LET arr_cifras_control[v_indice].v_viv92        = (arr_cifras_control[v_indice_acep].v_viv92    ) + arr_cifras_control[v_indice_preliq].v_viv92
     LET arr_cifras_control[v_indice].v_viv97        = (arr_cifras_control[v_indice_acep].v_viv97    ) + arr_cifras_control[v_indice_preliq].v_viv97
     LET arr_cifras_control[v_indice].v_viv72        = (arr_cifras_control[v_indice_acep].v_viv72    ) + arr_cifras_control[v_indice_preliq].v_viv72
   ELSE
     LET arr_cifras_control[v_indice].v_concepto     = p_desc_concp 
     LET arr_cifras_control[v_indice].v_concepto_aux = p_desc_concp_aux 
     LET arr_cifras_control[v_indice].v_registros    = "" 
     LET arr_cifras_control[v_indice].v_viv92        = 0
     LET arr_cifras_control[v_indice].v_viv97        = 0
     LET arr_cifras_control[v_indice].v_viv72        = 0
   END IF 
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_calculo_diferencia_liq
Fecha creacion: Marzo 22, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Calcula la diferencia de liquidacion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_calculo_diferencia_liq(v_indice,v_indice_acep,v_indice_liq,p_desc_concp,p_desc_concp_aux)

DEFINE v_indice         SMALLINT
DEFINE v_indice_acep    SMALLINT
DEFINE v_indice_liq     SMALLINT 
DEFINE p_desc_concp     STRING 
DEFINE p_desc_concp_aux STRING 

   SELECT a.status
   INTO   v_status_folio
   FROM   glo_folio a
   WHERE  a.folio = v_folio

  -- DISPLAY "@ LIQ v_status_folio: ", v_status_folio
  -- DISPLAY "@ .. ", arr_cifras_control[v_indice_acep].v_viv92, " + ", arr_cifras_control[v_indice_liq].v_viv92 
  -- DISPLAY "@ .. ", arr_cifras_control[v_indice_acep].v_viv97, " + ", arr_cifras_control[v_indice_liq].v_viv97 
  -- DISPLAY "@ .. ", arr_cifras_control[v_indice_acep].v_viv72, " + ", arr_cifras_control[v_indice_liq].v_viv72 
   
   
   -- si aun no se ha liquidado el folio, no hay diferencia
   IF ( v_status_folio > 1 ) THEN
      LET arr_cifras_control[v_indice].v_concepto     = p_desc_concp
      LET arr_cifras_control[v_indice].v_concepto_aux = p_desc_concp_aux
      LET arr_cifras_control[v_indice].v_registros    = ""
      LET arr_cifras_control[v_indice].v_viv92        = (arr_cifras_control[v_indice_acep].v_viv92    ) + arr_cifras_control[v_indice_liq].v_viv92
      LET arr_cifras_control[v_indice].v_viv97        = (arr_cifras_control[v_indice_acep].v_viv97    ) + arr_cifras_control[v_indice_liq].v_viv97
      LET arr_cifras_control[v_indice].v_viv72        = (arr_cifras_control[v_indice_acep].v_viv72    ) + arr_cifras_control[v_indice_liq].v_viv72
   ELSE
      LET arr_cifras_control[v_indice].v_concepto     = p_desc_concp
      LET arr_cifras_control[v_indice].v_concepto_aux = p_desc_concp_aux
      LET arr_cifras_control[v_indice].v_registros    = ""
      LET arr_cifras_control[v_indice].v_viv92        = 0
      LET arr_cifras_control[v_indice].v_viv97        = 0
      LET arr_cifras_control[v_indice].v_viv72        = 0
   END IF
END FUNCTION 