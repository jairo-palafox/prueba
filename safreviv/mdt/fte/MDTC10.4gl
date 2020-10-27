--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01-07-2013
--==============================================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTC10                                                        #
#Objetivo     =>                                                               #
#Fecha inicio => 01 Junio 2013                                                 #
################################################################################
DATABASE safre_viv

PRIVATE DEFINE v_ventana         ui.Window,
               v_forma           ui.Form,
               v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
               p_usuario_cod     LIKE seg_usuario.usuario_cod,
               p_tipo_ejecucion  SMALLINT,
               p_titulo_ventana  STRING

MAIN
DEFINE v_comando STRING

   # Se recupera la clave de usuario desde parámetro 
   LET p_usuario_cod    = ARG_VAL(1) # parámetro recibido por menú
   LET p_tipo_ejecucion = ARG_VAL(2) # parámetro recibido por menú
   LET p_titulo_ventana = ARG_VAL(3) # parámetro recibido por menú

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "mdt"

   OPEN WINDOW vtna_principal WITH FORM v_ruta_ejecutable CLIPPED||"/MDTC100"
      LET v_ventana = ui.Window.getCurrent()
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)         
         CALL v_ventana.setText(p_titulo_ventana)
      END IF
      
      MENU ""

         ON ACTION Individual
            CALL fn_consulta_individual_mandato()

         ON ACTION Mandato
            --CALL fn_consulta_movimiento_mandato()
            LET v_comando = "fglrun MDTC02.42r '", p_usuario_cod CLIPPED,"' ",
                                                  p_tipo_ejecucion," '",
                                                  p_titulo_ventana,"'"
            RUN v_comando

         ON ACTION cancelar
            EXIT MENU

      END MENU
   CLOSE WINDOW vtna_principal
END MAIN

{===============================================================================
Nombre: fn_consulta_individual_mandato
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para la consulta individual de movimientos (abonos y pagos) de 
 mandatos aplicados
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_consulta_individual_mandato()
DEFINE v_continua BOOLEAN,
       v_filtro   STRING,
       r_derechohabientes DYNAMIC ARRAY OF RECORD
         v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
         v_nss                LIKE afi_derechohabiente.nss,
         v_nombre             VARCHAR(80)
       END RECORD,
       r_mandatos DYNAMIC ARRAY OF RECORD
         v_desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
         v_id_cat_mandato   LIKE mdt_cat_mandato.id_cat_mandato,
         v_desc_mandato     LIKE mdt_cat_mandato.desc_mandato,
         v_abonos           LIKE cta_movimiento.monto_pesos,
         v_pagos            LIKE cta_movimiento.monto_pesos,
         v_saldo            LIKE cta_movimiento.monto_pesos
       END RECORD,
       r_movimientos DYNAMIC ARRAY OF RECORD
         v_num                 SMALLINT,
         v_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
         v_f_liquida           LIKE cta_movimiento.f_liquida,
         v_nss                 LIKE afi_derechohabiente.nss,
         v_subcuenta           LIKE cat_subcuenta.subcuenta_desc,
         v_movimiento          LIKE cat_movimiento.movimiento_desc,
         v_folio_liquida       LIKE cta_movimiento.folio_liquida,
         v_monto_pesos         LIKE cta_movimiento.monto_pesos,
         v_origen              LIKE cta_movimiento.origen,
         v_periodo_pago        LIKE mdt_det_aplica_mandato.periodo_pago,
         v_estado              LIKE mdt_det_aplica_monto.estado,
         v_boton               CHAR(1)
       END RECORD,
       v_id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente,
       v_abonos  STRING,
       v_pagos   STRING
       
       
   LET v_abonos = "313,323,333" # Movimientos para abonos
   LET v_pagos  = "314,324,334"  # Movimientos para pagos
   
                       
   OPEN WINDOW vtna_consulta_movimientos WITH FORM v_ruta_ejecutable CLIPPED||"/MDTC101"

      LET v_ventana = ui.Window.getCurrent()
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)         
         CALL v_ventana.setText(p_titulo_ventana)
      END IF
      LET v_continua = TRUE
      WHILE v_continua
         # Funcion para recuperar el filtro de búsqueda
         CALL fn_recupera_filtros() RETURNING v_continua,v_filtro
         IF( v_continua )THEN
            DIALOG ATTRIBUTES(UNBUFFERED)
               DISPLAY ARRAY r_derechohabientes TO sr_derechohabiente.*

                  BEFORE ROW
                     CALL r_mandatos.clear()
                     LET v_id_derechohabiente = r_derechohabientes[ARR_CURR()].v_id_derechohabiente
                     CALL fn_recupera_montos_mandatos(v_id_derechohabiente,
                                                      v_filtro,
                                                      v_abonos,
                                                      v_pagos) RETURNING r_mandatos

                  ON ACTION reporte
                     CALL fn_consulta_individual(v_filtro,v_id_derechohabiente,0,v_abonos,v_pagos,"Individual")

               END DISPLAY

               DISPLAY ARRAY r_mandatos TO sr_mandatos.*

                  BEFORE ROW
                     CALL r_movimientos.clear()
                     CALL fn_recupera_mandatos(v_filtro,
                                               v_id_derechohabiente,
                                               r_mandatos[ARR_CURR()].v_id_cat_mandato,
                                               v_abonos,
                                               v_pagos) RETURNING r_movimientos

               END DISPLAY 

               {DISPLAY ARRAY r_movimientos TO sr_movimientos.*

               END DISPLAY}
               INPUT ARRAY r_movimientos FROM sr_movimientos.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE)

                  ON ACTION buscar
                     IF( r_movimientos[ARR_CURR()].v_id_det_aplica_monto = 0 )THEN
                        CALL fn_mensaje("Aviso","No existen datos para el registro","information")
                     ELSE
                        CALL fn_muestra_datos_complementarios(r_movimientos[ARR_CURR()].v_id_det_aplica_monto)
                     END IF

               END INPUT

               BEFORE DIALOG
                  CALL fn_recupera_derechohabientes(v_filtro,v_abonos,v_pagos) RETURNING v_continua,r_derechohabientes
                  IF NOT( v_continua )THEN
                     CALL fn_mensaje("Aviso","No se encontraron registros con criterio dado","information")
                     LET v_continua = TRUE
                     EXIT DIALOG
                  END IF

               ON ACTION cancelar
                  LET v_continua = FALSE
                  EXIT DIALOG

            END DIALOG
         END IF
      END WHILE

   CLOSE WINDOW vtna_consulta_movimientos
END FUNCTION

{===============================================================================
Nombre: fn_recupera_filtros
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar el filtro de la consulta
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_filtros()
DEFINE v_filtros RECORD
         v_nss          LIKE afi_derechohabiente.nss,
         v_fecha_inicio DATE,
         v_fecha_fin    DATE
       END RECORD,
       v_continuar      BOOLEAN,
       v_filtro         STRING

   LET v_continuar = TRUE
   LET v_filtro = " "
   
   INPUT v_filtros.* FROM nss,periodo_inicio,periodo_fin ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

      AFTER FIELD periodo_fin
         IF(v_filtros.v_fecha_inicio IS NOT NULL AND v_filtros.v_fecha_fin IS NOT NULL AND v_filtros.v_fecha_inicio > v_filtros.v_fecha_fin)THEN
            CALL fn_mensaje("Aviso","Fecha incio es mayor a fecha fin","information")
            NEXT FIELD periodo_fin
         END IF

      ON ACTION consultar
         IF(v_filtros.v_fecha_inicio IS NOT NULL AND v_filtros.v_fecha_fin IS NOT NULL AND v_filtros.v_fecha_inicio > v_filtros.v_fecha_fin)THEN
            CALL fn_mensaje("Aviso","Fecha incio es mayor a fecha fin","information")
            NEXT FIELD periodo_fin
         END IF
         LET v_continuar = TRUE
         ACCEPT INPUT

      ON ACTION cancelar
         LET v_continuar = FALSE
         EXIT INPUT

   END INPUT

   IF(v_continuar)THEN
      LET v_filtro = " 1 = 1 AND "
      IF(v_filtros.v_nss CLIPPED IS NOT NULL)THEN
         LET v_filtro = "afi.nss = '"||v_filtros.v_nss||"' AND "
      END IF
      IF(v_filtros.v_fecha_inicio IS NOT NULL)THEN
         IF(v_filtros.v_fecha_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro||"cta.f_liquida BETWEEN '"||v_filtros.v_fecha_inicio||"' AND '"||v_filtros.v_fecha_fin||"' AND "
         ELSE
            LET v_filtro = v_filtro||"cta.f_liquida = '"||v_filtros.v_fecha_inicio||"' AND "
         END IF
      ELSE
         IF(v_filtros.v_fecha_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro||"cta.f_liquida = '"||v_filtros.v_fecha_fin||"' AND "
         END IF
      END IF
      LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-4)
   END IF
   
   RETURN v_continuar,v_filtro

END FUNCTION 

{===============================================================================
Nombre: fn_recupera_derechohabientes
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar los derechohabientes según el filtro
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_derechohabientes(p_filtro,p_abonos,p_pagos)
DEFINE p_filtro      STRING,
       p_abonos      STRING,
       p_pagos       STRING,
       v_consulta    STRING,
       v_derechohabiente RECORD
         v_id_derechohabiente LIKE mdt_det_aplica_monto.id_derechohabiente,
         v_nss                LIKE afi_derechohabiente.nss,
         v_nombre             LIKE afi_derechohabiente.nombre_af,
         v_paterno            LIKE afi_derechohabiente.ap_paterno_af,
         v_materno            LIKE afi_derechohabiente.ap_materno_af
       END RECORD,
       v_derechohabientes DYNAMIC ARRAY OF RECORD
         v_id_derechohabiente LIKE mdt_det_aplica_monto.id_derechohabiente,
         v_nss                LIKE afi_derechohabiente.nss,
         v_nombre             VARCHAR(80)
       END RECORD,
       v_indice               SMALLINT,
       v_continua             BOOLEAN
   
   LET v_indice   = 1   
   CALL v_derechohabientes.clear()
   LET v_consulta = "\n SELECT DISTINCT mto.id_derechohabiente,",
                    "\n        afi.nss,",
                    "\n        afi.nombre_af,",
                    "\n        afi.ap_paterno_af,",
                    "\n        afi.ap_materno_af",
                    "\n   FROM mdt_det_aplica_monto mto JOIN afi_derechohabiente afi",
                    "\n     ON afi.id_derechohabiente = mto.id_derechohabiente",
                    "\n        JOIN TABLE(MULTISET( ",
                    "\n                SELECT *",
                    "\n                  FROM cta_movimiento",
                    "\n                 WHERE movimiento IN (",p_abonos,",",p_pagos,"))) cta",
                    "\n     ON cta.id_referencia = mto.id_det_aplica_monto",
                    "\n  WHERE "||p_filtro

   PREPARE prp_recupera_reg_derechohabiente FROM v_consulta
   DECLARE cur_recupera_reg_derechohabiente CURSOR FOR prp_recupera_reg_derechohabiente
   FOREACH cur_recupera_reg_derechohabiente INTO v_derechohabiente.*
      LET v_derechohabientes[v_indice].v_id_derechohabiente = v_derechohabiente.v_id_derechohabiente
      LET v_derechohabientes[v_indice].v_nss                = v_derechohabiente.v_nss
      LET v_derechohabientes[v_indice].v_nombre             = v_derechohabiente.v_paterno CLIPPED||" "|| v_derechohabiente.v_materno CLIPPED||" "||v_derechohabiente.v_nombre
      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_recupera_reg_derechohabiente

   IF(v_derechohabientes.getLength() > 0)THEN
      LET v_continua = TRUE
   ELSE
      LET v_continua = FALSE
   END IF
   
   RETURN v_continua,v_derechohabientes

END FUNCTION

{===============================================================================
Nombre: fn_recupera_montos_mandatos
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar los mandatos y montos totales por mandato según el filtro
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_montos_mandatos(p_id_derechohabiente,p_filtro, p_abonos, p_pagos)
DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_filtro             STRING,
       p_abonos             STRING,
       p_pagos              STRING,
       v_consulta           STRING,
       v_mandato RECORD
         v_desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
         v_id_cat_mandato   LIKE mdt_cat_mandato.id_cat_mandato,
         v_desc_mandato     LIKE mdt_cat_mandato.desc_mandato,
         v_abonos           LIKE cta_movimiento.monto_pesos,
         v_pagos            LIKE cta_movimiento.monto_pesos
       END RECORD,
       v_mandatos DYNAMIC ARRAY OF RECORD
         v_desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
         v_id_cat_mandato   LIKE mdt_cat_mandato.id_cat_mandato,
         v_desc_mandato     LIKE mdt_cat_mandato.desc_mandato,
         v_abonos           LIKE cta_movimiento.monto_pesos,
         v_pagos            LIKE cta_movimiento.monto_pesos,
         v_saldo            LIKE cta_movimiento.monto_pesos
       END RECORD,
       v_indice             SMALLINT

   LET v_indice = 1
   CALL v_mandatos.clear()
   {LET v_consulta = "\n SELECT tpo.desc_tpo_mandato,",
                    "\n        mdt.cve_mandato,",
                    "\n        cat.desc_mandato,",
                    "\n        NVL(SUM(tab1.monto_pesos),0),",
                    "\n        NVL(SUM(tab2.monto_pesos),0)",
                    "\n   FROM TABLE(MULTISET(",
                    "\n        SELECT mto.id_det_ctr_mandato,",
                    "\n               cta.monto_pesos",
                    "\n          FROM mdt_det_aplica_monto mto JOIN afi_derechohabiente afi",
                    "\n            ON afi.id_derechohabiente = mto.id_derechohabiente",
                    "\n               JOIN TABLE(MULTISET(",
                    "\n                       SELECT f_liquida,",
                    "\n                              id_referencia,",
                    "\n                              monto_pesos",
                    "\n                         FROM cta_movimiento",
                    "\n                        WHERE movimiento IN (",p_abonos,"))) cta",
                    "\n            ON cta.id_referencia = mto.id_det_aplica_monto",
                    "\n         WHERE ",p_filtro,
                    "\n           AND mto.id_derechohabiente = ? )) tab1, OUTER",
                    "\n        TABLE(MULTISET(",
                    "\n        SELECT mto.id_det_ctr_mandato,",
                    "\n               cta.monto_pesos",
                    "\n          FROM mdt_det_aplica_monto mto JOIN afi_derechohabiente afi",
                    "\n            ON afi.id_derechohabiente = mto.id_derechohabiente",
                    "\n               JOIN TABLE(MULTISET(",
                    "\n                       SELECT f_liquida,",
                    "\n                              id_referencia,",
                    "\n                              monto_pesos",
                    "\n                         FROM cta_movimiento",
                    "\n                        WHERE movimiento IN (",p_pagos,"))) cta",
                    "\n            ON cta.id_referencia = mto.id_det_aplica_monto",
                    "\n         WHERE ",p_filtro,
                    "\n           AND mto.id_derechohabiente = ? )) tab2,",
                    "\n        mdt_det_ctr_mandato mdt,",
                    "\n        mdt_cat_mandato_paquete paq,",
                    "\n        mdt_cat_mandato cat,",
                    "\n        mdt_tpo_mandato tpo",
                    "\n  WHERE tab1.id_det_ctr_mandato = tab2.id_det_ctr_mandato",
                    "\n    AND mdt.id_det_ctr_mandato = tab1.id_det_ctr_mandato",
                    "\n    AND mdt.cve_mandato = paq.cve_mandato",
                    "\n    AND paq.id_cat_mandato = cat.id_cat_mandato",
                    "\n    AND tpo.tpo_mandato = cat.tpo_mandato",
                    "\n  GROUP BY 1,2,3"}
   LET v_consulta = "\n SELECT tpo.desc_tpo_mandato,",
                    "\n        tab1.id_cat_mandato,",
                    "\n        cat.desc_mandato,",
                    "\n        NVL(SUM(tab1.monto_pesos),0),",
                    "\n        NVL(SUM(tab2.monto_pesos),0)",
                    "\n   FROM TABLE(MULTISET(",
                    "\n        SELECT mto.id_cat_mandato,",
                    "\n               SUM(cta.monto_pesos) as monto_pesos",
                    "\n          FROM mdt_det_aplica_monto mto JOIN afi_derechohabiente afi",
                    "\n            ON afi.id_derechohabiente = mto.id_derechohabiente",
                    "\n               JOIN TABLE(MULTISET(",
                    "\n                       SELECT f_liquida,",
                    "\n                              id_referencia,",
                    "\n                              monto_pesos",
                    "\n                         FROM cta_movimiento",
                    "\n                        WHERE movimiento IN (",p_abonos,")",
                    "\n                          AND folio_liquida IN (SELECT folio_aplica_mdt FROM mdt_ctr_aplica_mandato WHERE estado = 100))) cta",
                    "\n            ON cta.id_referencia = mto.id_det_aplica_monto",
                    "\n         WHERE ",p_filtro,
                    "\n           AND mto.id_derechohabiente = ?",
                    "\n         GROUP BY 1)) tab1 LEFT OUTER JOIN",
                    "\n        TABLE(MULTISET(",
                    "\n        SELECT mto.id_cat_mandato,",
                    "\n               SUM(cta.monto_pesos) as monto_pesos",
                    "\n          FROM TABLE(MULTISET(SELECT id_derechohabiente,",
                    "\n                                     id_det_aplica_pago_mandato,",
                    "\n                                     id_cat_mandato",
                    "\n                                FROM mdt_det_aplica_monto",
                    "\n                               WHERE 1 = 1",
                    "\n                               GROUP BY 1,2,3)) mto JOIN afi_derechohabiente afi",
                    "\n            ON afi.id_derechohabiente = mto.id_derechohabiente",
                    "\n               JOIN mdt_det_aplica_pago_mandato pag",
                    "\n            ON pag.id_det_aplica_pago_mandato = mto.id_det_aplica_pago_mandato",
                    "\n               JOIN TABLE(MULTISET(",
                    "\n                       SELECT f_liquida,",
                    "\n                              id_referencia,",
                    "\n                              monto_pesos",
                    "\n                         FROM cta_movimiento",
                    "\n                        WHERE movimiento IN (",p_pagos,")",
                    "\n                          AND folio_liquida IN (SELECT folio_pago_mandato FROM mdt_ctr_aplica_pago_mandato WHERE ind_proceso = 1))) cta",
                    "\n            ON cta.id_referencia = pag.id_det_aplica_pago_mandato",
                    "\n         WHERE ",p_filtro,
                    "\n           AND mto.id_derechohabiente = ?",
                    "\n         GROUP BY 1)) tab2",
                    "\n     ON tab2.id_cat_mandato = tab1.id_cat_mandato",
                    "\n        JOIN mdt_cat_mandato cat",
                    "\n     ON cat.id_cat_mandato = tab1.id_cat_mandato",
                    "\n        JOIN mdt_tpo_mandato tpo",
                    "\n     ON tpo.tpo_mandato = cat.tpo_mandato",
                    "\n  GROUP BY 1,2,3"
   --DISPLAY v_consulta
   PREPARE prp_rec_montos_mandatos FROM v_consulta
   DECLARE cur_rec_montos_mandatos CURSOR FOR prp_rec_montos_mandatos
   FOREACH cur_rec_montos_mandatos USING p_id_derechohabiente,
                                         p_id_derechohabiente
                                    INTO v_mandato.*
      LET v_mandatos[v_indice].v_desc_tpo_mandato = v_mandato.v_desc_tpo_mandato
      LET v_mandatos[v_indice].v_id_cat_mandato   = v_mandato.v_id_cat_mandato
      LET v_mandatos[v_indice].v_desc_mandato     = v_mandato.v_desc_mandato
      LET v_mandatos[v_indice].v_abonos           = v_mandato.v_abonos
      LET v_mandatos[v_indice].v_pagos            = v_mandato.v_pagos
      LET v_mandatos[v_indice].v_saldo            = v_mandato.v_abonos + v_mandato.v_pagos # los pagos se recuperan con valor negativo, por eso se suma (abonos + (-pagos)) para obtener el saldo
      
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_montos_mandatos
      
   RETURN v_mandatos
END FUNCTION 

{===============================================================================
Nombre: fn_recupera_mandatos
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar los movimientos de mandatos según el filtro
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_mandatos(p_filtro,p_id_derechohabiente,p_id_cat_mandato,p_abonos,p_pagos)
DEFINE p_filtro             STRING,
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_id_cat_mandato        LIKE mdt_cat_mandato.id_cat_mandato,       
       p_abonos             STRING,
       p_pagos              STRING,
       v_consulta           STRING,       
       v_movimiento RECORD
         v_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
         v_f_liquida           LIKE cta_movimiento.f_liquida,
         v_nss                 LIKE afi_derechohabiente.nss,
         v_subcuenta           LIKE cat_subcuenta.subcuenta_desc,
         v_movimiento          LIKE cat_movimiento.movimiento_desc,
         v_folio_liquida       LIKE cta_movimiento.folio_liquida,
         v_monto_pesos         LIKE cta_movimiento.monto_pesos,
         v_origen              LIKE cta_movimiento.origen,
         v_periodo_pago        LIKE mdt_det_aplica_mandato.periodo_pago,
         v_estado              LIKE mdt_det_aplica_monto.estado
       END RECORD,
       v_movimientos DYNAMIC ARRAY OF RECORD
         v_num                 SMALLINT,
         v_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
         v_f_liquida           LIKE cta_movimiento.f_liquida,
         v_nss                 LIKE afi_derechohabiente.nss,
         v_subcuenta           LIKE cat_subcuenta.subcuenta_desc,
         v_movimiento          LIKE cat_movimiento.movimiento_desc,
         v_folio_liquida       LIKE cta_movimiento.folio_liquida,
         v_monto_pesos         LIKE cta_movimiento.monto_pesos,
         v_origen              LIKE cta_movimiento.origen,
         v_periodo_pago        LIKE mdt_det_aplica_mandato.periodo_pago,
         v_estado              LIKE mdt_det_aplica_monto.estado,
         v_boton               CHAR(1)
       END RECORD,
       v_indice         SMALLINT

   CALL v_movimientos.clear()
   LET v_indice = 1
   LET v_consulta = "\n SELECT mto.id_det_aplica_monto,",
                    "\n        cta.f_liquida,",
                    "\n        afi.nss,",
                    "\n        sub.subcuenta_desc,",
                    "\n        mov.movimiento_desc,",
                    "\n        cta.folio_liquida,",
                    "\n        cta.monto_pesos,",
                    "\n        cta.origen,",
                    "\n        apl.periodo_pago,",
                    "\n        mto.estado",
                    "\n   FROM afi_derechohabiente afi",
                    "\n        JOIN TABLE(MULTISET(",
                    "\n                       SELECT f_liquida,",
                    "\n                              id_derechohabiente,",
                    "\n                              subcuenta,",
                    "\n                              movimiento,",
                    "\n                              id_referencia,",
                    "\n                              folio_liquida,",
                    "\n                              monto_pesos,",
                    "\n                              origen",
                    "\n                         FROM cta_movimiento",
                    "\n                        WHERE movimiento IN (",p_abonos,"))) cta",
                    "\n     ON afi.id_derechohabiente = cta.id_derechohabiente",
                    "\n        JOIN cat_subcuenta sub",
                    "\n     ON sub.subcuenta = cta.subcuenta",
                    "\n        JOIN cat_movimiento mov",
                    "\n     ON mov.movimiento = cta.movimiento",
                    "\n        JOIN mdt_det_aplica_monto mto",
                    "\n     ON cta.id_referencia = mto.id_det_aplica_monto",
                    "\n        JOIN mdt_det_aplica_mandato apl",
                    "\n     ON apl.id_det_aplica_mandato = mto.id_det_aplica_mandato",
                    "\n  WHERE ",p_filtro,
                    "\n    AND cta.id_derechohabiente = ?",
                    "\n    AND mto.id_cat_mandato = ?",
                    "\n UNION ALL",
                    
                    "\n SELECT 0,",
                    "\n        cta.f_liquida,",
                    "\n        afi.nss,",
                    "\n        sub.subcuenta_desc,",
                    "\n        mov.movimiento_desc,",
                    "\n        cta.folio_liquida,",
                    "\n        cta.monto_pesos,",
                    "\n        cta.origen,",
                    "\n        '',",
                    "\n        0",
                    "\n   FROM afi_derechohabiente afi",
                    "\n        JOIN TABLE(MULTISET(",
                    "\n                       SELECT f_liquida,",
                    "\n                              id_derechohabiente,",
                    "\n                              subcuenta,",
                    "\n                              movimiento,",
                    "\n                              id_referencia,",
                    "\n                              folio_liquida,",
                    "\n                              monto_pesos,",
                    "\n                              origen",
                    "\n                         FROM cta_movimiento",
                    "\n                        WHERE movimiento IN (",p_pagos,"))) cta",
                    "\n     ON afi.id_derechohabiente = cta.id_derechohabiente",
                    "\n        JOIN cat_subcuenta sub",
                    "\n     ON sub.subcuenta = cta.subcuenta",
                    "\n        JOIN cat_movimiento mov",
                    "\n     ON mov.movimiento = cta.movimiento",
                    "\n        JOIN mdt_det_aplica_pago_mandato pag",
                    "\n     ON cta.id_referencia = pag.id_det_aplica_pago_mandato",
                    "\n        JOIN TABLE(MULTISET(SELECT id_derechohabiente,",
                    "\n                                   id_det_aplica_pago_mandato,",
                    "\n                                   id_cat_mandato",
                    "\n                              FROM mdt_det_aplica_monto",
                    "\n                             WHERE 1 = 1",
                    "\n                             GROUP BY 1,2,3)) mto",
                    "\n     ON mto.id_det_aplica_pago_mandato = pag.id_det_aplica_pago_mandato",
                    "\n  WHERE ",p_filtro,
                    "\n    AND cta.id_derechohabiente = ?",
                    "\n    AND mto.id_cat_mandato = ?"
   PREPARE prp_recupera_movimientos FROM v_consulta
   DECLARE cur_recupera_movimientos CURSOR FOR prp_recupera_movimientos
   FOREACH cur_recupera_movimientos USING p_id_derechohabiente,
                                          p_id_cat_mandato,
                                          p_id_derechohabiente,
                                          p_id_cat_mandato
                                     INTO v_movimiento.*
      LET v_movimientos[v_indice].v_num                 = v_indice
      LET v_movimientos[v_indice].v_id_det_aplica_monto = v_movimiento.v_id_det_aplica_monto
      LET v_movimientos[v_indice].v_f_liquida           = v_movimiento.v_f_liquida
      LET v_movimientos[v_indice].v_nss                 = v_movimiento.v_nss
      LET v_movimientos[v_indice].v_subcuenta           = v_movimiento.v_subcuenta
      LET v_movimientos[v_indice].v_movimiento          = v_movimiento.v_movimiento
      LET v_movimientos[v_indice].v_folio_liquida       = v_movimiento.v_folio_liquida
      LET v_movimientos[v_indice].v_monto_pesos         = v_movimiento.v_monto_pesos
      LET v_movimientos[v_indice].v_origen              = v_movimiento.v_origen
      LET v_movimientos[v_indice].v_periodo_pago        = v_movimiento.v_periodo_pago
      LET v_movimientos[v_indice].v_estado              = v_movimiento.v_estado
      
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_movimientos
   
   RETURN v_movimientos
END FUNCTION 

{===============================================================================
Nombre: fn_muestra_datos_complementarios
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para mostrar los datos complementarios
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_muestra_datos_complementarios(p_id_det_aplica_monto)
DEFINE p_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
       v_ventana   ui.Window,
       v_datos_com DYNAMIC ARRAY OF RECORD
         v_etiqueta VARCHAR(40),
         v_valor    VARCHAR(200)
       END RECORD,
       v_entidad   CHAR(20),
       v_continuar BOOLEAN

   LET v_entidad = "mdt_referencia_abono"
   OPEN WINDOW vtna_datos_complementarios WITH FORM v_ruta_ejecutable CLIPPED||"/MDTC102" ATTRIBUTES(STYLE = "dialog")
      LET v_ventana = ui.Window.getCurrent()
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)         
         CALL v_ventana.setText(p_titulo_ventana)
      END IF
      DISPLAY ARRAY v_datos_com TO sr_complementario.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY
            CALL fn_recupera_datos_complementarios(p_id_det_aplica_monto,v_entidad) RETURNING v_continuar, v_datos_com
            IF NOT( v_continuar )THEN
               CALL fn_mensaje("Aviso","No se encontraror registros con citerio dado","information")
               EXIT DISPLAY
            END IF

         ON ACTION aceptar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_datos_complementarios

END FUNCTION

{===============================================================================
Nombre: fn_recupera_datos_complementarios
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar los datos complementarios
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_datos_complementarios(p_id_det_aplica_monto, p_entidad)
DEFINE p_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
       p_entidad             CHAR(20),
       v_consulta            STRING,
       v_dato_complementario RECORD
         v_ind        SMALLINT,
         v_diag       CHAR(3),
         v_sql_error  INTEGER,
         v_isam_error INTEGER,
         v_msg_error  VARCHAR(100),
         v_etiqueta   VARCHAR(40),
         v_valor      VARCHAR(200)
       END RECORD,
       v_datos_complementarios DYNAMIC ARRAY OF RECORD
         v_etiqueta   VARCHAR(40),
         v_valor      VARCHAR(200)
       END RECORD,
       v_indice    SMALLINT,
       v_continuar BOOLEAN

   LET v_indice = 1
   CALL v_datos_complementarios.clear()
   LET v_consulta = "EXECUTE FUNCTION fn_glo_recupera_etiquetas(?,?)"
   PREPARE prp_recupera_datos FROM v_consulta
   DECLARE cur_recupera_datos CURSOR FOR prp_recupera_datos
   FOREACH cur_recupera_datos USING p_entidad,
                                    p_id_det_aplica_monto
                               INTO v_dato_complementario.*
      LET v_datos_complementarios[v_indice].v_etiqueta = v_dato_complementario.v_etiqueta
      LET v_datos_complementarios[v_indice].v_valor = v_dato_complementario.v_valor
      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_recupera_datos

   IF(v_datos_complementarios.getLength() > 0)THEN
      LET v_continuar = TRUE 
   ELSE
      LET v_continuar = FALSE
   END IF
   
   RETURN v_continuar, v_datos_complementarios
END FUNCTION