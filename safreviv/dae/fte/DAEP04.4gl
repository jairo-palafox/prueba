--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 21/01/2014
-- 21/01/2014 --Se excluye el movimiento 999 del saldado de cta_movimiento AG
--==============================================================================


################################################################################
#Modulo            => DAE                                                      #
#Programa          => DAEP04                                                   #
#Objetivo          => Programa que obtiene la información para generar el      #
#                     estado de cuenta exclusivo de Amortizaciones Excedentes  #
#Fecha inicio      => Julio 2013                                               #
################################################################################

GLOBALS "DAEP03.inc"

DATABASE safre_viv

PRIVATE DEFINE v_parametros        datos_edo_cuenta 

FUNCTION fn_archivo_edo_cuenta(p_nss, p_ruta)
   DEFINE p_nss               LIKE afi_derechohabiente.nss
   DEFINE p_ruta              STRING
   DEFINE v_archivo           STRING
   
   CALL fn_datos_edo_cuenta(p_nss)
   
   IF p_ruta IS NULL OR p_ruta.getLength() = 0 THEN
      LET p_ruta = "/safreviv/dae/edo_cuenta/"
   END IF
   CALL fn_genera_estado_cuenta(p_ruta, v_parametros.*) RETURNING v_archivo
   
   RETURN v_archivo
END FUNCTION

FUNCTION fn_pantalla_edo_cuenta(p_nss)
   DEFINE p_nss               LIKE afi_derechohabiente.nss
   DEFINE v_archivo           STRING
   
   CALL fn_datos_edo_cuenta(p_nss)
   CALL fn_genera_estado_cuenta(NULL, v_parametros.*) RETURNING v_archivo
   
   RETURN v_archivo
END FUNCTION

PRIVATE FUNCTION fn_datos_edo_cuenta(p_nss)
   DEFINE p_nss               LIKE afi_derechohabiente.nss
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

   LET consulta_datos = "  SELECT FIRST 1 ",
                              "afiliado.id_derechohabiente, ",
                              "afiliado.nss, ",
                              "afiliado.curp, ",
                              "TRIM(afiliado.nombre_af) || ' ' || ",
                              "TRIM(afiliado.ap_paterno_af) || ' ' || ",
                              "TRIM(afiliado.ap_materno_af), ",
                              "CASE afiliado.tipo_trabajador ",
                                 "WHEN 'I' THEN 'IMSS' ",
                                 "WHEN 'S' THEN 'SOLO INFONAVIT' ",
                                 "WHEN 'E' THEN 'ESTADO-MUNICIPIO' ",
                              "END CASE, ",
                              "CASE afiliado.origen_afiliacion ",
                                 "WHEN 'A' THEN 'REGISTRO' ",
                                 "WHEN 'I' THEN 'REGISTRO INICIAL' ",
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
                              "NVL(domicilio.colonia,' '), ",
                              "NVL(munic.municipio_desc,' '), ",
                              "NVL(estado.entidad_desc_larga,' '), ",
                              "NVL(domicilio.cp,' ') ",
                           "FROM afi_derechohabiente afiliado ",
                           "LEFT JOIN afi_domicilio domicilio ON (domicilio.id_derechohabiente = afiliado.id_derechohabiente AND domicilio.ind_envio = '1' ) ",
                           "LEFT JOIN cat_cp codigo ON codigo.cp = domicilio.cp ",
                           "LEFT JOIN cat_municipio munic ON munic.municipio = codigo.municipio ",
                           "LEFT JOIN cat_entidad_federativa estado ON estado.entidad_federativa = codigo.entidad_federativa ",
                           "WHERE afiliado.NSS = ?"
   PREPARE exe_consulta_datos FROM consulta_datos

   LET consulta_anterior = "\n SELECT ",               
                           "\n   0, ",                 
                           "\n   mov.subcuenta, ",     
                           "\n   SUM(mov.monto_acciones * precio.precio_fondo) ",
                           "\n FROM cta_movimiento mov ",
                           "\n INNER JOIN cat_movimiento cat_mov ON cat_mov.movimiento = mov.movimiento ",
                           "\n INNER JOIN glo_valor_fondo precio ON (precio.fondo = mov.fondo_inversion AND precio.f_valuacion = TODAY) ",
                           "\n WHERE mov.subcuenta = 46  ",
                           "\n AND   mov.movimiento <> 999  ",
                           "\n AND   mov.f_liquida < ? ",
                           "\n AND   mov.id_derechohabiente = ? ",
                           "\n GROUP BY mov.subcuenta" 
   PREPARE exe_consulta_anterior FROM consulta_anterior
   
   LET consulta_periodo =  "SELECT ",
                              "cat_mov.tipo, ",
                              "mov.subcuenta, ",
                              "SUM(mov.monto_acciones * precio.precio_fondo) ",
                           "FROM cta_movimiento mov ",
                           "INNER JOIN cat_movimiento cat_mov ON cat_mov.movimiento = mov.movimiento ",
                           "INNER JOIN glo_valor_fondo precio ON (precio.fondo = mov.fondo_inversion AND precio.f_valuacion = TODAY) ",
                           "WHERE mov.subcuenta = 46  ",
                           "AND   mov.movimiento <> 999  ",
                           "AND   mov.f_liquida BETWEEN ? AND ? ",
                           "AND   mov.id_derechohabiente = ? ",
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
                           "WHERE movimientos.id_derechohabiente = ? ",
                           "AND   movimientos.movimiento <> 999  ",
                           "AND   movimientos.f_liquida >= ? ",
                           "AND   movimientos.f_liquida <= ? ",
                           "ORDER BY movimientos.f_liquida DESC, movimientos.movimiento ASC"
   PREPARE exe_consulta_movtos FROM consulta_movtos

   LET consulta_subcuentas = "SELECT subcuenta, subcuenta_desc FROM cat_subcuenta WHERE subcuenta = 46 "
#INITCAP(subcuenta_desc)
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
         WHEN 46
            LET v_parametros.anterior_voluntaria = v_totales.suma
      END CASE
   END FOREACH

   #Se llenan los totales del periodo
   DECLARE periodo CURSOR FOR exe_consulta_periodo

   FOREACH periodo USING v_parametros.periodo_inicio, v_parametros.periodo_fin, id_derechohabiente INTO v_totales.*
      CASE v_totales.subcuenta
         WHEN 46
            IF v_totales.tipo_mov = 1 THEN
               LET v_parametros.abono_voluntaria = v_totales.suma
            ELSE
               LET v_parametros.cargo_voluntaria = v_totales.suma
            END IF
      END CASE
   END FOREACH

   #Se llenan las descripciones de las subcuentas
   DECLARE subcuentas CURSOR FOR exe_consulta_subcuentas

   FOREACH subcuentas INTO v_subcuenta, v_desc_subcuenta
      CASE v_subcuenta
         WHEN 46
            LET v_parametros.desc_voluntaria = v_desc_subcuenta
      END CASE
   END FOREACH

   #Se llenan los montos finales

   LET v_parametros.final_voluntaria =    v_parametros.anterior_voluntaria
                                       +  v_parametros.cargo_voluntaria
                                       +  v_parametros.abono_voluntaria

   LET v_parametros.saldo_total =   v_parametros.final_voluntaria

    #Se buscan los movimientos del periodo
   DECLARE consulta CURSOR FOR exe_consulta_movtos

   #Se guarda el resultado en el arreglo que se enviara al estado de cuenta
   LET i = 1
   FOREACH consulta USING id_derechohabiente, 
                          v_parametros.periodo_inicio, 
                          v_parametros.periodo_fin 
                     INTO v_parametros.movimientos[i].*
      LET i = i + 1
   END FOREACH

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
   LET v_parametros.anterior_voluntaria = 0
   LET v_parametros.cargo_voluntaria = 0
   LET v_parametros.abono_voluntaria = 0
   LET v_parametros.anterior_fortalecimiento = 0
   LET v_parametros.cargo_fortalecimiento = 0
   LET v_parametros.abono_fortalecimiento = 0
   LET v_parametros.hoja_detalle = FALSE
END FUNCTION 

