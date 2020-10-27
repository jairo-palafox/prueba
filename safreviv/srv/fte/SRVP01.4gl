######################################################################
#Modulo            =>SRV                                             #
#Programa          =>SRVP01                                          #
#Objetivo          =>Programa que genera los datos para el estado de #
#                    cuenta del afiliado                             #
#Fecha inicio      =>21 Marzo 2012                                   #
######################################################################

GLOBALS "SRVP02.inc"

DATABASE safre_viv

PRIVATE DEFINE v_parametros        datos_edo_cuenta 

MAIN
   DEFINE nss                 LIKE afi_derechohabiente.nss
   LET nss = ARG_VAL(1)
   CALL fn_crea_edo_cuenta(nss, FALSE)
END MAIN

FUNCTION fn_crea_edo_cuenta(p_nss, p_vista_pantalla)
   DEFINE p_nss               LIKE afi_derechohabiente.nss
   DEFINE p_vista_pantalla    BOOLEAN
   DEFINE i                   SMALLINT
   DEFINE consulta_datos      STRING
   DEFINE consulta_movtos     STRING
   DEFINE consulta_anterior   STRING
   DEFINE consulta_periodo    STRING
   DEFINE consulta_subcuentas STRING
   DEFINE id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_corte_anterior    DATE
   DEFINE v_subcuenta         LIKE cat_subcuenta.subcuenta
   DEFINE v_desc_subcuenta    LIKE cat_subcuenta.subcuenta_desc

   DEFINE v_totales RECORD 
      tipo_mov                LIKE cat_movimiento.tipo,
      subcuenta               LIKE cta_movimiento.subcuenta,
      suma                    LIKE cta_movimiento.monto_pesos
   END RECORD 

   CALL fn_inicio()

   #LET nss = ARG_VAL(1)

   LET consulta_datos = "  SELECT ",
                              "afiliado.id_derechohabiente, ",
                              "afiliado.nss, ",
                              "afiliado.curp, ",
                              "TRIM(afiliado.nombre_af) || ' ' || ",
                              "TRIM(afiliado.ap_paterno_af) || ' ' || ",
                              "TRIM(afiliado.ap_materno_af), ",
                              "CASE afiliado.tipo_trabajador ",
                                 "WHEN 'I' THEN 'IMSS' ",
                                 "WHEN 'S' THEN 'SOLO INFONAVIT' ",
                              "END CASE, ",
                              "CASE afiliado.origen_afiliacion ",
                                 "WHEN 'A' THEN 'AFILIACIÓN' ",
                                 "WHEN 'R' THEN 'RECAUDACIÓN' ",
                                 "WHEN 'S' THEN 'SEPARACIÓN' ",
                                 "WHEN 'U' THEN 'UNIFICACIÓN' ",
                              "END CASE, ",
                              "CASE afiliado.id_credito ",
                                 "WHEN 0 THEN 'SIN CREDITO' ",
                                 "WHEN 1 THEN 'ACREDITADO' ",
                                 "WHEN 2 THEN 'CREDITO 43 BIS' ",
                                 "WHEN 3 THEN 'ANUALIDAD GARANTIZADA' ",
                              "END CASE, ",
                              "domicilio.calle, ",
                              "NVL('EXT ' || TRIM(domicilio.num_exterior) || ",
                              "' INT ' || TRIM(domicilio.num_interior),' '), ",
                              "NVL(colonia.colonia_desc,' '), ",
                              "NVL(munic.municipio_desc,' '), ",
                              "NVL(estado.entidad_desc_larga,' '), ",
                              "NVL(domicilio.cp,' ') ",
                           "FROM afi_derechohabiente afiliado ",
                           "LEFT JOIN afi_domicilio domicilio ON domicilio.id_derechohabiente = afiliado.id_derechohabiente ",
                           "LEFT JOIN cat_colonia colonia ON colonia.colonia = domicilio.colonia ",
                           "LEFT JOIN cat_cp codigo ON codigo.cp = domicilio.cp ",
                           "LEFT JOIN cat_municipio munic ON munic.municipio = codigo.municipio ",
                           "LEFT JOIN cat_entidad_federativa estado ON estado.entidad_federativa = codigo.entidad_federativa ",
                           "WHERE afiliado.NSS = ?"
   PREPARE exe_consulta_datos FROM consulta_datos

   LET consulta_anterior = "SELECT ",
                              "0, ",
                              "mov.subcuenta, ",
                              "SUM(mov.monto_acciones * precio.precio_fondo) ",
                           "FROM cta_movimiento mov ",
                           "INNER JOIN cat_movimiento cat_mov ON cat_mov.movimiento = mov.movimiento ",
                           "INNER JOIN glo_valor_fondo precio ON (precio.fondo = mov.fondo_inversion AND precio.f_valuacion = TODAY) ",
                           "WHERE mov.subcuenta IN (4,8,40,41,42,43,44)  ",
                           "AND mov.f_liquida < ? ",
                           "AND mov.id_derechohabiente = ? ",
                           "GROUP BY mov.subcuenta"
   PREPARE exe_consulta_anterior FROM consulta_anterior

   LET consulta_periodo =  "SELECT ",
                              "cat_mov.tipo, ",
                              "mov.subcuenta, ",
                              "SUM(mov.monto_acciones * precio.precio_fondo) ",
                           "FROM cta_movimiento mov ",
                           "INNER JOIN cat_movimiento cat_mov ON cat_mov.movimiento = mov.movimiento ",
                           "INNER JOIN glo_valor_fondo precio ON (precio.fondo = mov.fondo_inversion AND precio.f_valuacion = TODAY) ",
                           "WHERE mov.subcuenta IN (4,8,40,41,42,43,44)  ",
                           "AND mov.f_liquida BETWEEN ? AND ? ",
                           "AND mov.id_derechohabiente = ? ",
                           "GROUP BY mov.subcuenta, cat_mov.tipo"
   PREPARE exe_consulta_periodo FROM consulta_periodo

   LET consulta_movtos = " SELECT ",
                              "movimientos.subcuenta, ",
                              "movimientos.f_liquida, ",
                              "movimientos.origen, ",
                              "tipo_movimiento.movimiento_desc, ",
                              "movimientos.monto_pesos ",
                           "FROM cta_movimiento movimientos ",
                           "INNER JOIN cat_movimiento tipo_movimiento ON movimientos.movimiento = tipo_movimiento.movimiento ",
                           "WHERE movimientos.subcuenta IN (4,8,40,41,42,43,44) ",
                           "AND movimientos.id_derechohabiente = ? ",
                           "AND movimientos.f_liquida > ? ",
                           "AND movimientos.f_liquida <= ? ",
                           "ORDER BY movimientos.f_liquida DESC, movimientos.movimiento ASC"
   PREPARE exe_consulta_movtos FROM consulta_movtos

   LET consulta_subcuentas = "SELECT subcuenta, INITCAP(subcuenta_desc) FROM cat_subcuenta WHERE subcuenta IN (4,8,40,41,42,43,44)"

   PREPARE exe_consulta_subcuentas FROM consulta_subcuentas

   #Se establece como fecha de corte anterior el primer dia del mes actual
   LET  v_corte_anterior = MDY(MONTH(TODAY), 1, YEAR(TODAY))

   EXECUTE exe_consulta_datos USING p_nss
                              INTO  id_derechohabiente,
                                    v_parametros.nss,
                                    v_parametros.curp,
                                    v_parametros.nombre_completo,
                                    v_parametros.tipo_derechohabiente,
                                    v_parametros.origen_afiliacion,
                                    v_parametros.indicador_credito,
                                    v_parametros.calle,
                                    v_parametros.numero,
                                    v_parametros.colonia,
                                    v_parametros.municipio,
                                    v_parametros.estado,
                                    v_parametros.codigo_postal

   LET v_parametros.fecha_corte = TODAY
   LET v_parametros.periodo_inicio = v_corte_anterior
   LET v_parametros.periodo_fin = TODAY

   #Se llenan los totales anteriores al  periodo
   DECLARE anterior CURSOR FOR exe_consulta_anterior

   FOREACH anterior USING v_parametros.periodo_inicio, id_derechohabiente INTO v_totales.*
      CASE v_totales.subcuenta
         WHEN 4
            LET v_parametros.anterior_97 = v_totales.suma
         WHEN 8
            LET v_parametros.anterior_92 = v_totales.suma
         WHEN 40
            LET v_parametros.anterior_72 = v_totales.suma
         WHEN 41
            LET v_parametros.anterior_amortizacion = v_totales.suma
         WHEN 42
            LET v_parametros.anterior_92_infonavit = v_totales.suma
         WHEN 43
            LET v_parametros.anterior_amortizacion_infonavit = v_totales.suma
         WHEN 44
            LET v_parametros.anterior_97_infonavit = v_totales.suma
      END CASE
   END FOREACH

   #Se llenan los totales del periodo
   DECLARE periodo CURSOR FOR exe_consulta_periodo

   FOREACH periodo USING v_parametros.periodo_inicio, v_parametros.periodo_fin, id_derechohabiente INTO v_totales.*
      CASE v_totales.subcuenta
         WHEN 4
            IF v_totales.tipo_mov = 1 THEN
               LET v_parametros.abono_97 = v_totales.suma
            ELSE
               LET v_parametros.cargo_97 = v_totales.suma
            END IF 
         WHEN 8
            IF v_totales.tipo_mov = 1 THEN
               LET v_parametros.abono_92 = v_totales.suma
            ELSE
               LET v_parametros.cargo_92 = v_totales.suma
            END IF 
         WHEN 40
            IF v_totales.tipo_mov = 1 THEN
               LET v_parametros.abono_72 = v_totales.suma
            ELSE
               LET v_parametros.cargo_72 = v_totales.suma
            END IF 
         WHEN 41
            IF v_totales.tipo_mov = 1 THEN
               LET v_parametros.abono_amortizacion = v_totales.suma
            ELSE
               LET v_parametros.cargo_amortizacion = v_totales.suma
            END IF 
         WHEN 42
            IF v_totales.tipo_mov = 1 THEN
               LET v_parametros.abono_92_infonavit = v_totales.suma
            ELSE
               LET v_parametros.cargo_92_infonavit = v_totales.suma
            END IF 
         WHEN 43
            IF v_totales.tipo_mov = 1 THEN
               LET v_parametros.abono_amortizacion_infonavit = v_totales.suma
            ELSE
               LET v_parametros.cargo_amortizacion_infonavit = v_totales.suma
            END IF 
         WHEN 44
            IF v_totales.tipo_mov = 1 THEN
               LET v_parametros.abono_97_infonavit = v_totales.suma
            ELSE
               LET v_parametros.cargo_97_infonavit = v_totales.suma
            END IF 
      END CASE
   END FOREACH

   #Se llenan las descripciones de las subcuentas
   DECLARE subcuentas CURSOR FOR exe_consulta_subcuentas

   FOREACH subcuentas INTO v_subcuenta, v_desc_subcuenta
      CASE v_subcuenta
         WHEN 4
            LET v_parametros.desc_97 = v_desc_subcuenta
         WHEN 8
            LET v_parametros.desc_92 = v_desc_subcuenta
         WHEN 40
            LET v_parametros.desc_72 = v_desc_subcuenta
         WHEN 41
            LET v_parametros.desc_amortizacion = v_desc_subcuenta
         WHEN 42
            LET v_parametros.desc_92_infonavit = v_desc_subcuenta
         WHEN 43
            LET v_parametros.desc_amortizacion_infonavit = v_desc_subcuenta
         WHEN 44
            LET v_parametros.desc_97_infonavit = v_desc_subcuenta
      END CASE
   END FOREACH

   #Se llenan los montos finales
   LET v_parametros.final_72 =   v_parametros.anterior_72
                              +  v_parametros.cargo_72
                              +  v_parametros.abono_72

   LET v_parametros.final_92 =   v_parametros.anterior_92
                              +  v_parametros.cargo_92
                              +  v_parametros.abono_92

   LET v_parametros.final_97 =   v_parametros.anterior_97
                              +  v_parametros.cargo_97
                              +  v_parametros.abono_97

   LET v_parametros.final_amortizacion =  v_parametros.anterior_amortizacion
                                       +  v_parametros.cargo_amortizacion
                                       +  v_parametros.abono_amortizacion

   LET v_parametros.final_92_infonavit =  v_parametros.anterior_92_infonavit
                                       +  v_parametros.cargo_92_infonavit
                                       +  v_parametros.abono_92_infonavit

   LET v_parametros.final_97_infonavit =  v_parametros.anterior_97_infonavit
                                       +  v_parametros.cargo_97_infonavit
                                       +  v_parametros.abono_97_infonavit

   LET v_parametros.final_amortizacion_infonavit =    v_parametros.anterior_amortizacion_infonavit
                                                   +  v_parametros.cargo_amortizacion_infonavit
                                                   +  v_parametros.abono_amortizacion_infonavit
                                                   
   LET v_parametros.saldo_total =   v_parametros.final_72 
                                 +  v_parametros.final_92 
                                 +  v_parametros.final_97
                                 +  v_parametros.final_amortizacion
                                 +  v_parametros.final_92_infonavit 
                                 +  v_parametros.final_97_infonavit
                                 +  v_parametros.final_amortizacion_infonavit

    #Se buscan los movimientos del periodo
   DECLARE consulta CURSOR FOR exe_consulta_movtos

   #Se guarda el resultado en el arreglo que se enviara al estado de cuenta
   LET i = 1
   FOREACH consulta USING id_derechohabiente, v_parametros.periodo_inicio, v_parametros.periodo_fin INTO v_parametros.movimientos[i].*
      LET i = i + 1
   END FOREACH

   CALL fn_genera_estado_cuenta(p_vista_pantalla, v_parametros.*)

END FUNCTION

PRIVATE FUNCTION fn_inicio()
   LET v_parametros.anterior_72 = 0
   LET v_parametros.cargo_72 = 0
   LET v_parametros.abono_72 = 0
   LET v_parametros.anterior_92 = 0
   LET v_parametros.cargo_92 = 0
   LET v_parametros.abono_92 = 0
   LET v_parametros.anterior_97 = 0
   LET v_parametros.cargo_97 = 0
   LET v_parametros.abono_97 = 0
   LET v_parametros.anterior_amortizacion = 0
   LET v_parametros.cargo_amortizacion = 0
   LET v_parametros.abono_amortizacion = 0
   LET v_parametros.anterior_92_infonavit = 0
   LET v_parametros.cargo_92_infonavit = 0
   LET v_parametros.abono_92_infonavit = 0
   LET v_parametros.anterior_97_infonavit = 0
   LET v_parametros.cargo_97_infonavit = 0
   LET v_parametros.abono_97_infonavit = 0
   LET v_parametros.anterior_amortizacion_infonavit = 0
   LET v_parametros.cargo_amortizacion_infonavit = 0
   LET v_parametros.abono_amortizacion_infonavit = 0
   LET v_parametros.hoja_detalle = FALSE
END FUNCTION 