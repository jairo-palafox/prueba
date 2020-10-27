--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

##########################################################################
#Modulo       => GRT                                                     #
#Programa     => GRTP25                                                  #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la #
#                preliquidacion de DSE para el módulo de Créditos en     #
#                Garantía 43 bis                                         #
#Autor        => Daniel Buendia, EFP                                     #
#Fecha inicio => 30 Mayo 2012                                            #
#Modificado por    => Mauro Muñiz Cabllero, EFP                          #
#Modificación      => Eliminación de adelantos                           #
#Fecha modifica    => 15 de noviembre de 2016                            #
##########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario_cod             LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE g_pid                     LIKE bat_ctr_proceso.pid --  ID del proceso
   DEFINE g_proceso_cod             LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod               LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE g_folio                   LIKE deo_preliquida.folio_liquida -- folio de la operacion
   DEFINE g_f_valor                 DATE

END GLOBALS

MAIN

   DEFINE p_nombre_archivo          LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
   DEFINE v_f_valuacion             LIKE glo_valor_fondo.f_valuacion -- fecha de valuación
   DEFINE v_precio_fondo            LIKE glo_valor_fondo.precio_fondo -- precio de accion
   DEFINE v_ind_tipo_ejec           LIKE bat_ctr_operacion.ind_tipo_ejecucion -- tipo de ejecucion (0-manual, 1-batch)

   DEFINE v_r_cifras_pre RECORD
      subcuenta                     LIKE cat_subcuenta.subcuenta, -- subcuenta
      subcuenta_desc                LIKE cat_subcuenta.subcuenta_desc, -- descripción de subcuenta
      movimiento                    LIKE cat_movimiento.movimiento, -- movimiento
      movto_desc                    LIKE cat_movimiento.movimiento_desc, -- descripción de movimiento
      monto_aivs                    LIKE cta_movimiento.monto_acciones, -- monto de aivs
      monto_pesos                   DECIMAL(12,2) --LIKE cta_movimiento.monto_pesos -- monto en pesos
   END RECORD

   DEFINE v_s_sql                    STRING -- cadena con una instruccion SQL
   DEFINE v_i_resultado              INTEGER -- resultado del proceso
   DEFINE v_b_error                  SMALLINT -- bandera que indica si ocurrio error en la función
   DEFINE v_s_qryTxt                 STRING -- guarda una sentencia sql a ejecutar

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET g_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET g_f_valor        = ARG_VAL(7)

   -- se crea la fecha de valuación
   LET v_f_valuacion = TODAY - DAY(TODAY) + 1
   --LET v_f_valuacion = TODAY

   DISPLAY "=INICIA GRTP24="
   DISPLAY " USUARIO       : ",g_usuario_cod
   DISPLAY " PID           : ",g_pid
   DISPLAY " ARCHIVO       : ",p_nombre_archivo

   -- se obtiene el tipo de proceso de la tabla de control de procesos
   LET v_s_qryTxt = " SELECT ind_tipo_ejecucion\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",g_pid,"\n",
                    "    AND proceso_cod = ",g_proceso_cod,"\n",
                    "    AND opera_cod = ",g_opera_cod

   PREPARE prp_tipo_ejecucion FROM v_s_qryTxt
   EXECUTE prp_tipo_ejecucion INTO v_ind_tipo_ejec

   -- se valida si la operacion se ejecutó manualmente o a través del batch
   IF v_ind_tipo_ejec = 1 THEN
      -- se genera el folio
      LET g_folio = fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario_cod)
   END IF
   DISPLAY " FOLIO         : ",g_folio USING "#########&"

   -- validar si existe registros a preliquidar
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11\n",
                    "    AND f_valuacion = '",g_f_valor,"'"

   PREPARE prp_precio_fondo FROM v_s_qryTxt
   EXECUTE prp_precio_fondo INTO v_precio_fondo

   -- se invoca la función que crea la tabla de preliquidación
   CALL cre_tabla_dse_preliquda() RETURNING v_b_error

   -- se asume que el proceso termina correctamente
   LET v_i_resultado = 0

   DISPLAY " Se ejecuta el proceso de liquidación"
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION safre_viv:fn_dse_preliquida_grt(?,?,?,?)"

   -- se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE sid_preliquidadse FROM v_s_sql
   EXECUTE sid_preliquidadse USING g_folio, g_usuario_cod, v_precio_fondo, g_f_valor
                              INTO v_i_resultado

   IF ( v_i_resultado = 0 ) THEN
      -- se invoca la finalización de la operacion
      LET v_i_resultado = fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)

      -- se verifica si fue posible finalizar la operación
      IF v_i_resultado <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(v_i_resultado)
      ELSE
         DISPLAY " GENERA REPORTE DE LA OPERACION"
         -- se invoca la función que ejecuta el reporte de preliquidación
         CALL fn_ejec_reporte_preliquida()
      END IF
   ELSE
      DISPLAY " OCURRIÓ UN ERROR AL FINALIZAR LA OPERACIÓN"
      DISPLAY " CÓDIGO DEVUELTO: ", v_i_resultado

      -- ocurrio un error al terminar el stored procedure
      LET v_i_resultado = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF v_i_resultado <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(v_i_resultado)
      END IF
   END IF

   DISPLAY " = PRELIQUIDACIÓN DSE (GRT)="
   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT pre.subcuenta, sub.subcuenta_desc, pre.movimiento, mto.movimiento_desc,\n",
                    "        SUM(pre.monto_acciones), SUM(pre.monto_pesos)\n",
                    "   FROM dse_cg_preliquida pre, cat_subcuenta sub, cat_movimiento mto\n",
                    "  WHERE pre.subcuenta = sub.subcuenta\n",
                    "    AND pre.movimiento = mto.movimiento\n",
                    "  GROUP BY 1,2,3,4\n",
                    "  ORDER BY 1,3"

   PREPARE prp_preliquida_acr FROM v_s_qryTxt
   DECLARE cur_preliquida_acr CURSOR FOR prp_preliquida_acr

   FOREACH cur_preliquida_acr INTO v_r_cifras_pre.*
      DISPLAY "SUBCUENTA    ", v_r_cifras_pre.subcuenta, "-", v_r_cifras_pre.subcuenta_desc
      DISPLAY "MOVIMIENTO   ", v_r_cifras_pre.movimiento, "-", v_r_cifras_pre.movto_desc
      DISPLAY "MONTO AIVS   ", v_r_cifras_pre.monto_aivs
      DISPLAY "MONTO PESOS  ", v_r_cifras_pre.monto_pesos
   END FOREACH

END MAIN

#Objetivo: Función que elimina y crea nuevamente la tabla de preliquoidación
FUNCTION cre_tabla_dse_preliquda()

   DEFINE v_s_qryTxt                STRING -- contiene una sentencia sql a ejecutar
   DEFINE v_b_error                 SMALLINT -- bandera que indica si ocurrió un error

   -- se asume que no ocurrirá error en la función
   LET v_b_error = 0

   -- se crea la sentencia que elimina la tabla de preliquidación dse
   LET v_s_qryTxt = " DROP TABLE dse_cg_preliquida;"

   PREPARE prp_drop_table_preliq FROM v_s_qryTxt
   EXECUTE prp_drop_table_preliq

   -- verifica si ocurrió un error en la sentencia
   IF SQLCA.sqlcode <> 0 THEN
      DISPLAY " OCURRIÓ UN ERROR AL BORRAR LA TABLA DE PRELIQUIDACIÓN: ",SQLCA.sqlcode

      -- se indica que ocurrió un error y no continua con el proceso
      LET v_b_error = 1

      RETURN v_b_error
   END IF

   -- se crea la sentencia que crea la tabla de preliquidación dse
   LET v_s_qryTxt = " CREATE TABLE dse_cg_preliquida\n",
                    " (f_liquida date not null,\n",
                    " id_derechohabiente decimal(9,0) not null,\n",
                    " subcuenta smallint not null,\n",
                    " fondo_inversion smallint not null,\n",
                    " movimiento smallint not null,\n",
                    " folio_liquida decimal(9,0) not null,\n",
                    " id_referencia decimal(9,0) not null,\n",
                    " monto_acciones decimal(22,2),\n",
                    " monto_pesos decimal(22,2),\n",
                    " f_valor date,\n",
                    " f_registro date,\n",
                    " h_registro datetime hour to second,\n",
                    " origen char(20)) IN dse_dbs;"

   PREPARE prp_crea_table_preliq FROM v_s_qryTxt
   EXECUTE prp_crea_table_preliq

   -- verifica si ocurrió un error en la sentencia
   IF SQLCA.sqlcode <> 0 THEN
      DISPLAY " OCURRIÓ UN ERROR AL CREAR LA TABLA DE PRELIQUIDACIÓN: ",SQLCA.sqlcode

      -- se indica que ocurrió un error y no continua con el proceso
      LET v_b_error = 1

      RETURN v_b_error
   END IF

   RETURN v_b_error

END FUNCTION

# Objetivo: Función que ejcuta el programa que genera el reporte de liquidación
FUNCTION fn_ejec_reporte_preliquida()

   DEFINE v_s_comando               STRING -- comando a ejecutar
   DEFINE v_v_tabla                 STRING -- nombre de la tabla para el reporte
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin
   DEFINE r_ruta_listados           LIKE seg_modulo.ruta_listados -- ruta listados

   -- se inicializan variables
   LET v_v_tabla        = "dse_cg_preliquida" -- tabla de preliquidación

   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod = "GRTL39" -- programa lanzador
   LET v_c_programa_cod = fn_obten_nom_programa(g_proceso_cod , g_opera_cod)

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_ruta_listados

   -- se crea el comando que ejecuta el modulo que reliza la integracion del archivo
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/GRTP19 ",
                                                 g_usuario_cod, " ",
                                                 g_pid, " ",
                                                 g_proceso_cod, " ",
                                                 g_opera_cod, " ",
                                                 g_folio, " ",
                                                 v_v_tabla, " ", 
                                                 v_c_programa_cod, " "

   RUN v_s_comando

END FUNCTION
