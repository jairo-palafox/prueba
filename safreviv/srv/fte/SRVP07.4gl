######################################################################
#Modulo            =>SRV                                             #
#Programa          =>SRVP07                                          #
#Objetivo          =>Programa que genera los estados de cuenta       #
#                    de forma masiva                                 #
#Fecha inicio      =>21 Marzo 2012                                   #
######################################################################
GLOBALS "SRVP05.inc"

DATABASE safre_tmp

PRIVATE DEFINE id_periodo           LIKE srv_periodo_edo_cuenta.id_periodo_edo_cuenta
PRIVATE DEFINE f_inicio             DATE
PRIVATE DEFINE f_fin                DATE
PRIVATE DEFINE v_ruta               STRING
PRIVATE DEFINE v_nom_archivo        VARCHAR(50)
PRIVATE DEFINE v_usuario            LIKE srv_hist_edo_cuenta.usuario
PRIVATE DEFINE v_ind_vigencia       LIKE srv_hist_edo_cuenta.ind_vigencia

FUNCTION fn_genera_edo_cuenta_masivo(p_ruta)
   DEFINE p_ruta                    STRING
   DEFINE v_consulta_periodo        STRING 

   IF p_ruta IS NULL OR p_ruta.getLength() = 0 THEN
      LET p_ruta = "/safreviv/srv/edo_cuenta/"
   END IF

   LET v_ruta = p_ruta
   LET v_ind_vigencia = 1
   LET v_usuario = "safreviv"

   LET v_consulta_periodo =  "SELECT ",
                           "id_periodo_edo_cuenta, ",
                           "f_inicio, ",
                           "f_fin ",
                           "FROM srv_periodo_edo_cuenta ",
                           "WHERE ind_pdf = 0 "
                           
   PREPARE exe_consulta_periodo FROM v_consulta_periodo
   EXECUTE exe_consulta_periodo INTO id_periodo,
                                     f_inicio,
                                     f_fin

   #Se preparan las consultas
   CALL fn_prepara_consultas()
   #se ejecuta el proceso masivo
   CALL fn_genera_edo_cuenta()
   
END FUNCTION

PRIVATE FUNCTION fn_prepara_consultas()
   DEFINE v_consulta_datos       STRING
   DEFINE v_consulta_saldos      STRING
   DEFINE v_consulta_movtos      STRING
   DEFINE v_insert_historico     STRING
   DEFINE v_insert_hist_saldo    STRING

   LET v_consulta_datos =  "SELECT ",
                              "id_derechohabiente, ",
                              "nss, ",
                              "curp, ",
                              "nombre_completo, ",
                              "tipo_trabajador, ",
                              "origen_afiliacion, ",
                              "id_credito, ",
                              "calle, ",
                              "numero, ",
                              "colonia, ",
                              "municipio, ",
                              "estado, ",
                              "cp ",
                           "FROM srv_edo_cuenta ",
                           "WHERE id_periodo_edo_cuenta = ? "
   PREPARE exe_consulta_datos FROM v_consulta_datos

   LET v_consulta_saldos = "SELECT ",
                              "subcuenta, ",
                              "subcuenta_desc, ",
                              "anterior, ",
                              "cargo, ",
                              "abono, ",
                              "final, ",
                              "rendimiento ",
                           "FROM srv_sdo_edo_cuenta ",
                           "WHERE id_derechohabiente = ?"
   PREPARE exe_consulta_saldos FROM v_consulta_saldos

   LET v_consulta_movtos = " SELECT ",
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
   PREPARE exe_consulta_movtos FROM v_consulta_movtos

   LET v_insert_historico = "INSERT INTO srv_hist_edo_cuenta(id_derechohabiente, ",
                                                            "id_periodo_edo_cuenta, ",
                                                            "nom_archivo, ",
                                                            "usuario, ",
                                                            "nss, ",
                                                            "curp, ",
                                                            "f_proceso, ",
                                                            "ind_vigencia) ",
                                                            "VALUES(?,?,?,?,?,?,?,?)"
   PREPARE exe_insert_historico FROM v_insert_historico

   LET v_insert_hist_saldo = "INSERT INTO srv_hist_sdo_edo_cuenta( id_derechohabiente ",
                                                                  "id_periodo_edo_cuenta ",
                                                                  "subcuenta ",
                                                                  "monto ",
                                                                  "precio) ",
                                                                  "VALUES(?,?,?,?,?)"
   PREPARE exe_insert_hist_saldo FROM v_insert_hist_saldo
END FUNCTION

PRIVATE FUNCTION fn_genera_edo_cuenta()
   DEFINE i                      SMALLINT
   
   DEFINE v_id_derechohabiente   LIKE srv_edo_cuenta.id_derechohabiente
   DEFINE v_parametros           datos_edo_cuenta

   --Sumario de saldos
	DEFINE v_subcuenta_desc             LIKE srv_sdo_edo_cuenta.subcuenta_desc
	DEFINE v_subcuenta						LIKE srv_sdo_edo_cuenta.subcuenta
	DEFINE v_anterior                	LIKE srv_sdo_edo_cuenta.anterior
	DEFINE v_cargo                   	LIKE srv_sdo_edo_cuenta.cargo
	DEFINE v_abono                   	LIKE srv_sdo_edo_cuenta.abono
	DEFINE v_final                   	LIKE srv_sdo_edo_cuenta.final
	DEFINE v_rendimiento                LIKE srv_sdo_edo_cuenta.final
   DEFINE v_precio_fondo					DECIMAL(22,6)

   LET v_precio_fondo = 0
   
   #Se obtienen los datos de los trabajadores a los que se les generara el estado de cuenta
   DECLARE datos CURSOR FOR exe_consulta_datos
   FOREACH datos USING id_periodo INTO v_id_derechohabiente,
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

      #Se asignan las fechas
      LET v_parametros.fecha_corte = f_fin
      LET v_parametros.periodo_inicio = f_inicio
      LET v_parametros.periodo_fin = f_fin

      #Se obtiene el sumario de saldos
      DECLARE saldos CURSOR  FOR exe_consulta_saldos
      FOREACH saldos USING v_id_derechohabiente INTO  v_subcuenta,
                                                      v_subcuenta_desc,
                                                      v_anterior,
                                                      v_cargo,
                                                      v_abono,
                                                      v_final,
                                                      v_rendimiento,
                                                      v_precio_fondo
         CASE v_subcuenta
            WHEN 4
               LET v_parametros.desc_97 = v_subcuenta_desc
               LET v_parametros.anterior_97 = v_anterior
               LET v_parametros.abono_97 = v_abono
               LET v_parametros.cargo_97 = v_cargo
               LET v_parametros.final_97 =v_final
            WHEN 8
               LET v_parametros.desc_92 = v_subcuenta_desc
               LET v_parametros.anterior_92 = v_anterior
               LET v_parametros.abono_92 = v_abono
               LET v_parametros.cargo_92 = v_cargo
               LET v_parametros.final_92 = v_final
            WHEN 40
               LET v_parametros.desc_72 = v_subcuenta_desc
               LET v_parametros.anterior_72 = v_anterior
               LET v_parametros.abono_72 = v_abono
               LET v_parametros.cargo_72 = v_cargo
               LET v_parametros.final_72 = v_final
            WHEN 41
               LET v_parametros.desc_amortizacion = v_subcuenta_desc
               LET v_parametros.anterior_amortizacion = v_anterior
               LET v_parametros.abono_amortizacion = v_abono
               LET v_parametros.cargo_amortizacion = v_cargo
               LET v_parametros.final_amortizacion = v_final
            WHEN 42
               LET v_parametros.desc_92_infonavit = v_subcuenta_desc
               LET v_parametros.anterior_92_infonavit = v_anterior
               LET v_parametros.abono_92_infonavit = v_abono
               LET v_parametros.cargo_92_infonavit = v_cargo
               LET v_parametros.final_92_infonavit = v_final
            WHEN 43
               LET v_parametros.desc_amortizacion_infonavit = v_subcuenta_desc
               LET v_parametros.anterior_amortizacion_infonavit = v_anterior
               LET v_parametros.abono_amortizacion_infonavit = v_abono
               LET v_parametros.cargo_amortizacion_infonavit = v_cargo
               LET v_parametros.final_amortizacion_infonavit = v_final
            WHEN 44
               LET v_parametros.desc_97_infonavit = v_subcuenta_desc
               LET v_parametros.anterior_97_infonavit = v_anterior
               LET v_parametros.abono_97_infonavit = v_abono
               LET v_parametros.cargo_97_infonavit = v_cargo
               LET v_parametros.final_97_infonavit = v_final
               
         END CASE
      END FOREACH
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
      FOREACH consulta USING v_id_derechohabiente, v_parametros.periodo_inicio, v_parametros.periodo_fin INTO v_parametros.movimientos[i].*
         LET i = i + 1
      END FOREACH

      #Se genera la impresion
      CALL fn_genera_estado_cuenta(v_ruta, v_parametros.*) RETURNING v_nom_archivo

      #Guardamos en el historico el registro del estado de cuenta
      EXECUTE exe_insert_historico USING  v_id_derechohabiente,
                                          id_periodo,
                                          v_nom_archivo,
                                          v_usuario,
                                          v_parametros.nss,
                                          v_parametros.curp,
                                          f_fin,
                                          v_ind_vigencia

      #Se guarda el historico de saldos finales por subcuenta
      LET v_subcuenta = 4
      EXECUTE exe_insert_hist_saldo USING v_id_derechohabiente,
                                          id_periodo,
                                          v_subcuenta,
                                          v_parametros.final_72,
                                          v_precio_fondo
                                          
      
   END FOREACH
   
END FUNCTION

PRIVATE FUNCTION fn_inicio(v_parametros)
   DEFINE v_parametros              datos_edo_cuenta
   
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