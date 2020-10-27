################################################################################
#Programa      => RET895                                                       #
#Ojetivo       => Programa para Corregir historia de variación menor           #
#                 en ley 73                                                    #
#Fecha inicio  => 01 de Septiembre, 2015                                       #
#Por           => Luis Felipe Prieto Cano                                      #
#Requerimiento => PRODINF-895                                                  #
################################################################################
DATABASE safre_viv

MAIN
   DISPLAY "Inicio del proceso:",TIME
   --CALL fn_genera_historia()     --Genera historia
   DISPLAY "Termina de generar historia:",TIME
   CALL fn_corrige_historia()    --Genera casuistica
   DISPLAY "Termina de corregir historia:",TIME
   --CALL fn_genera_reporte()      --Genera el reporte de PDF
   DISPLAY "Termina de generar reporte:",TIME
END MAIN

FUNCTION fn_genera_historia()

   DEFINE v_id_derechohabiente   DECIMAL(9,0)
   DEFINE v_subcuenta            SMALLINT

   DISPLAY "Inicia proceso de generacion de historia"

   CALL crea_tablas1()

   DISPLAY "Genera historia 2015"
   --Obtengo los id_derechohabientes por subcuenta para evaluar posteriormente
   DECLARE cur_movto_2015 CURSOR FOR
   SELECT id_derechohabiente, subcuenta
   FROM cta_movimiento
   WHERE movimiento = 812
   AND subcuenta IN (8,4)
   --AND id_derechohabiente = 23882617      --Prueba
   GROUP BY 1,2
   FOREACH cur_movto_2015 INTO v_id_derechohabiente, v_subcuenta
      CALL integra_en_cta_movimiento_total2(v_id_derechohabiente, v_subcuenta)
   END FOREACH
   FREE cur_movto_2015

   DISPLAY "Genera historia 2014"
   --Obtengo los id_derechohabientes por subcuenta para evaluar posteriormente
   DECLARE cur_movto_2014 CURSOR FOR
   SELECT id_derechohabiente, subcuenta
   FROM cta_movimiento14
   WHERE movimiento = 812
   AND subcuenta IN (8,4)
   GROUP BY 1,2
   FOREACH cur_movto_2014 INTO v_id_derechohabiente, v_subcuenta
      CALL integra_en_cta_movimiento_total2(v_id_derechohabiente, v_subcuenta)
   END FOREACH
   FREE cur_movto_2014

   DISPLAY "Genera historia 2013"
   --Obtengo los id_derechohabientes por subcuenta para evaluar posteriormente
   DECLARE cur_movto_2013 CURSOR FOR
   SELECT id_derechohabiente, subcuenta
   FROM cta_movimiento13
   WHERE movimiento = 812
   AND subcuenta IN (8,4)
   GROUP BY 1,2
   FOREACH cur_movto_2013 INTO v_id_derechohabiente, v_subcuenta
      CALL integra_en_cta_movimiento_total2(v_id_derechohabiente, v_subcuenta)
   END FOREACH
   FREE cur_movto_2013

   DISPLAY "Genera historia 2012"
   --Obtengo los id_derechohabientes por subcuenta para evaluar posteriormente
   DECLARE cur_movto_2012 CURSOR FOR
   SELECT id_derechohabiente, subcuenta
   FROM cta_movimiento12
   WHERE movimiento = 812
   AND subcuenta IN (8,4)
   GROUP BY 1,2
   FOREACH cur_movto_2012 INTO v_id_derechohabiente, v_subcuenta
      CALL integra_en_cta_movimiento_total2(v_id_derechohabiente, v_subcuenta)
   END FOREACH
   FREE cur_movto_2012

   UPDATE STATISTICS FOR TABLE cta_movimiento_total2
   
END FUNCTION

FUNCTION fn_corrige_historia()

   DEFINE v_r_cta_movimiento     RECORD LIKE cta_movimiento.*
   DEFINE v_r_cta_movto          RECORD LIKE cta_movimiento.*
   DEFINE v_consecutivo          DECIMAL(9,0)
   DEFINE v_num_regs,v_total     INTEGER
   DEFINE v_hoy                  DATE
   DEFINE v_precio_fondo         LIKE glo_valor_fondo.precio_fondo
   DEFINE v_hora                 DATETIME HOUR TO SECOND
   DEFINE v_id_derechohabiente   DECIMAL(9,0)
   DEFINE v_subcuenta            SMALLINT
   DEFINE v_f_registro           DATE
   DEFINE v_h_registro           DATETIME HOUR TO SECOND
   DEFINE v_monto_acciones       DECIMAL(16,6)
   DEFINE v_monto_acciones_rev   DECIMAL(16,6)
   DEFINE a_reg                  DYNAMIC ARRAY OF RECORD
      f_registro                 DATE,
      h_registro                 DATETIME HOUR TO SECOND,
      monto_acciones             DECIMAL(16,6),
      f_registro_rev             DATE,
      h_registro_rev             DATETIME HOUR TO SECOND,
      saldo_acciones             DECIMAL(16,6),
      saldo_pesos                DECIMAL(16,6)
   END RECORD
   DEFINE ind                    INTEGER
   DEFINE v_nss                  CHAR(11)
   DEFINE v_resultado            SMALLINT
   DEFINE v_anio,v_anio2         SMALLINT
   DEFINE max_f_registro         DATE
   DEFINE max_f_liquida          DECIMAL(9,0)
   DEFINE max_h_registro         DATETIME HOUR TO SECOND

   DISPLAY "Inicia proceso de Correccion de Historia"

   CALL crea_tablas2()

   LET v_num_regs = 0
   LET v_total = 0
   LET v_hoy = DATE
   LET v_hora = CURRENT HOUR TO SECOND

   --Obtengo el valor del fondo
   SELECT precio_fondo
   INTO   v_precio_fondo
   FROM   glo_valor_fondo
   WHERE  fondo       = 11
   AND    f_valuacion = v_hoy

   IF sqlca.sqlcode = NOTFOUND THEN
      DISPLAY "Imposible cargar valor del fondo, revisar!"
      RETURN
   END IF

   --Se obtiene el numero de consecutivo
   SELECT seq_glo_folio.nextval
   INTO   v_consecutivo
   FROM   systables
   WHERE  tabid = 1
   
   ---
   --Obtengo los id_derechohabientes por subcuenta para evaluar posteriormente
   DECLARE cur_movimiento CURSOR FOR
   SELECT id_derechohabiente, subcuenta
   FROM cta_movimiento_total2
   WHERE movimiento = 812
   AND subcuenta IN (8,4)
   GROUP BY 1,2
   FOREACH cur_movimiento INTO v_id_derechohabiente, v_subcuenta

      CALL a_reg.clear()

      SELECT nss
      INTO v_nss
      FROM afi_derechohabiente
      WHERE id_derechohabiente = v_id_derechohabiente

      --Obtengo todos los registros con el id_derechohabiente, subcuenta encontrados
      DECLARE cur_movto2 CURSOR FOR
      SELECT f_registro, h_registro, monto_acciones
      FROM cta_movimiento_total2
      WHERE movimiento = 812
      AND subcuenta = v_subcuenta
      AND id_derechohabiente = v_id_derechohabiente
      ORDER BY 1, 2
      FOREACH cur_movto2 INTO v_f_registro, v_h_registro, v_monto_acciones
         CALL a_reg.appendElement()
         LET a_reg[a_reg.getLength()].f_registro = v_f_registro
         LET a_reg[a_reg.getLength()].h_registro = v_h_registro
         LET a_reg[a_reg.getLength()].monto_acciones = v_monto_acciones
      END FOREACH
      FREE cur_movto2

      --Para cada movimiento reviso si tiene reversa operativo,
      --En caso de ser asi anexo al arreglo
      FOR ind = 1 TO a_reg.getLength()

         LET v_monto_acciones_rev = a_reg[ind].monto_acciones * (-1)
         
         IF ind = a_reg.getLength() THEN
            --Llegue al ultimo registro
            --Aqui reviso solo por el ultimo registro (Fechas)
            DECLARE cur_reverso_1 CURSOR FOR
            SELECT f_registro, h_registro
            FROM cta_movimiento_total2
            WHERE movimiento = 521
            AND subcuenta = v_subcuenta
            AND id_derechohabiente = v_id_derechohabiente
            AND monto_acciones = v_monto_acciones_rev             --Valor en acciones reversado
            AND f_registro >= a_reg[ind].f_registro               --Mayor/Igual a la fecha de registro
            ORDER BY 1, 2
            FOREACH cur_reverso_1 INTO v_f_registro, v_h_registro
               IF a_reg[ind].f_registro = v_f_registro THEN
                  --La hora de reverso debe ser mayor a la hora de registro
                  IF v_h_registro > a_reg[ind].h_registro THEN
                     LET a_reg[ind].f_registro_rev = v_f_registro
                     LET a_reg[ind].h_registro_rev = v_h_registro
                     EXIT FOREACH      --Forzo salida del foreach
                  END IF
               ELSE
                  LET a_reg[ind].f_registro_rev = v_f_registro
                  LET a_reg[ind].h_registro_rev = v_h_registro
                  EXIT FOREACH      --Forzo salida del foreach
               END IF
            END FOREACH
            FREE cur_reverso_1
         ELSE
            --Todavia existen registros
            --Aqui reviso por el ultimo y el penultimo registro (Fechas)
            DECLARE cur_reverso_2 CURSOR FOR
            SELECT f_registro, h_registro
            FROM cta_movimiento_total2
            WHERE movimiento = 521
            AND subcuenta = v_subcuenta
            AND id_derechohabiente = v_id_derechohabiente
            AND monto_acciones = v_monto_acciones_rev             --Valor en acciones reversado
            AND f_registro >= a_reg[ind].f_registro               --Mayor/Igual a la fecha de registro
            AND f_registro <= a_reg[ind+1].f_registro             --Menor/Igual a la fecha de registro del siguiente registro
            ORDER BY 1, 2
            FOREACH cur_reverso_2 INTO v_f_registro, v_h_registro
               IF a_reg[ind].f_registro = v_f_registro THEN
                  --La hora de reverso debe ser mayor a la hora de registro
                  IF v_h_registro > a_reg[ind].h_registro THEN
                     LET a_reg[ind].f_registro_rev = v_f_registro
                     LET a_reg[ind].h_registro_rev = v_h_registro
                     EXIT FOREACH      --Forzo salida del foreach
                  END IF
               ELSE
                  LET a_reg[ind].f_registro_rev = v_f_registro
                  LET a_reg[ind].h_registro_rev = v_h_registro
                  EXIT FOREACH      --Forzo salida del foreach
               END IF
            END FOREACH
            FREE cur_reverso_2
         END IF
         
      END FOR

      --Finalmente reviso si cada registro tiene reverso, en caso de ser asi no lo tomo
      --En caso contrario es el movimiento que tomare en cuenta para generar registro
      FOR ind = 1 TO a_reg.getLength()

         --Obtengo Saldo
         --Genero datos para obtener saldos
         PREPARE prp_saldo_dia FROM "EXECUTE FUNCTION fn_saldo_dia(?,?,?,TODAY)"
         EXECUTE prp_saldo_dia USING v_nss,v_id_derechohabiente,v_subcuenta
         INTO v_resultado,a_reg[ind].saldo_acciones,a_reg[ind].saldo_pesos

         {
         DISPLAY ""
         DISPLAY "Registro: ",v_total USING "<<<,<<<,<<<"
         DISPLAY "   Id Derechohabiente: ",v_id_derechohabiente
         DISPLAY "   Subcuenta         : ",v_subcuenta
         DISPLAY "   NSS               : ",v_nss
         DISPLAY "   Monto Acciones    : ",a_reg[ind].monto_acciones
         DISPLAY "   Fecha de registro : ",a_reg[ind].f_registro
         DISPLAY "   Hora de registro  : ",a_reg[ind].h_registro
         DISPLAY "   Fecha de reverso  : ",a_reg[ind].f_registro_rev
         DISPLAY "   Hora de reverso   : ",a_reg[ind].h_registro_rev
         }
         

         LET v_total = v_total + 1

         IF a_reg[ind].f_registro_rev IS NULL THEN

            --Evaluo que el saldo final  - monto encontrado sea menor a cero para proceder
            IF a_reg[ind].saldo_acciones - a_reg[ind].monto_acciones <= 0 THEN

               SELECT MAX(f_registro)
               INTO max_f_registro
               FROM cta_movimiento_total2
               WHERE id_derechohabiente = v_id_derechohabiente
               AND subcuenta = v_subcuenta
               AND movimiento = 812

               SELECT MAX(h_registro)
               INTO max_h_registro
               FROM cta_movimiento_total2
               WHERE id_derechohabiente = v_id_derechohabiente
               AND subcuenta = v_subcuenta
               AND movimiento = 812
               AND f_registro = max_f_registro

               SELECT *
               INTO v_anio2,v_r_cta_movto.*
               FROM cta_movimiento_total2
               WHERE id_derechohabiente = v_id_derechohabiente
               AND subcuenta = v_subcuenta
               AND movimiento = 812
               AND f_registro = max_f_registro
               AND h_registro = max_h_registro

               LET v_r_cta_movimiento.* = v_r_cta_movto.*
               
               --Solo se integran con los cambios solicitados
               LET v_r_cta_movimiento.f_liquida       = v_hoy
               LET v_r_cta_movimiento.folio_liquida   = v_consecutivo
               LET v_r_cta_movimiento.monto_acciones  = a_reg[ind].monto_acciones * (-1)
               LET v_r_cta_movimiento.monto_pesos     = v_r_cta_movimiento.monto_acciones * v_precio_fondo
               LET v_r_cta_movimiento.movimiento      = 1671
               LET v_r_cta_movimiento.f_valor         = v_hoy
               LET v_r_cta_movimiento.f_registro      = v_hoy
               LET v_r_cta_movimiento.h_registro      = v_hora
               LET v_r_cta_movimiento.origen          = "PRODINF-895"

               LET v_anio = YEAR(a_reg[ind].f_registro)
               INSERT INTO revisa_log2 VALUES (v_anio,v_id_derechohabiente,v_nss,v_subcuenta,a_reg[ind].monto_acciones,
               a_reg[ind].saldo_acciones, a_reg[ind].saldo_pesos, a_reg[ind].f_registro,
               a_reg[ind].h_registro,a_reg[ind].f_registro_rev,a_reg[ind].h_registro_rev,3)

               --Revisa que no exista duplicidad en la insercion
               ###INSERT INTO cta_movimiento VALUES (v_r_cta_movimiento.*)
               LET v_num_regs = v_num_regs + 1

            ELSE

               LET v_anio = YEAR(a_reg[ind].f_registro)
               INSERT INTO revisa_log2 VALUES (v_anio,v_id_derechohabiente,v_nss,v_subcuenta,a_reg[ind].monto_acciones,
               a_reg[ind].saldo_acciones, a_reg[ind].saldo_pesos, a_reg[ind].f_registro,
               a_reg[ind].h_registro,a_reg[ind].f_registro_rev,a_reg[ind].h_registro_rev,2)

               #DISPLAY "No se integra movimiento dado que dejaria el saldo negativo"
               
            END IF

         ELSE

            LET v_anio = YEAR(a_reg[ind].f_registro)
            INSERT INTO revisa_log2 VALUES (v_anio,v_id_derechohabiente,v_nss,v_subcuenta,a_reg[ind].monto_acciones,
            a_reg[ind].saldo_acciones, a_reg[ind].saldo_pesos, a_reg[ind].f_registro,
            a_reg[ind].h_registro,a_reg[ind].f_registro_rev,a_reg[ind].h_registro_rev,1)

            #DISPLAY "No se integra movimiento dado que existe reverso"

         END IF
         
      END FOR
   
   END FOREACH
   FREE cur_movimiento
   
   DISPLAY "Se corrigieron: ", v_num_regs USING "<<<,<<<,<<<"," de ",
   v_total USING "<<<,<<<,<<<"," registros"

   UPDATE STATISTICS FOR TABLE revisa_log2
   
END FUNCTION

FUNCTION fn_genera_reporte()

   CALL crea_tablas3()

   --Genero informes
   ---Obtengo Vivienda 92
   INSERT INTO t92_revisa_log2
   SELECT nss,saldo_acciones,monto_acciones
   FROM revisa_log2
   WHERE caso = 2
   AND subcuenta = 8

   ---Obtengo Vivienda 97
   INSERT INTO t97_revisa_log2
   SELECT nss,saldo_acciones,monto_acciones
   FROM revisa_log2
   WHERE caso = 2
   AND subcuenta = 4
   
   --Obtengo join Donde tengan tanto saldo 92 como saldo 97
   INSERT INTO t1
   SELECT t92.nss,t92.saldo_acciones AS saldo_acciones_92,
   t92.monto_acciones AS monto_acciones_92,
   t97.saldo_acciones AS saldo_acciones_97,
   t97.monto_acciones AS monto_acciones_97
   FROM t92_revisa_log2 t92, t97_revisa_log2 t97
   WHERE t92.nss = t97.nss
   
   --Outer Join Donde solo tienen Vivienda 92
   INSERT INTO t2
   SELECT t92.nss AS nss_92,t92.saldo_acciones AS saldo_acciones_92,
   t92.monto_acciones AS monto_acciones_92,t97.nss AS nss_97
   FROM t92_revisa_log2 t92, OUTER t97_revisa_log2 t97
   WHERE t92.nss = t97.nss

   INSERT INTO t3
   SELECT nss_92,saldo_acciones_92,monto_acciones_92,
   0 AS saldo_acciones_97,
   0 AS monto_acciones_97
   FROM t2
   WHERE nss_97 IS NULL

   --Outer Join Donde solo tienen Vivienda 97
   INSERT INTO t4
   SELECT t92.nss AS nss_92,t97.saldo_acciones AS saldo_acciones_97,
   t97.monto_acciones AS monto_acciones_97,t97.nss AS nss_97
   FROM t97_revisa_log2 t97, OUTER t92_revisa_log2 t92
   WHERE t92.nss = t97.nss

   INSERT INTO t5
   SELECT nss_97,
   0 AS saldo_acciones_92,
   0 AS monto_acciones_92,
   saldo_acciones_97,monto_acciones_97
   FROM t4
   WHERE nss_92 IS NULL

   --Junto las tres tablas
   INSERT INTO t6
   SELECT * FROM t1

   INSERT INTO t6
   SELECT * FROM t3

   INSERT INTO t6
   SELECT * FROM t5

   CALL fn_genera_archivo()
   
END FUNCTION


FUNCTION fn_genera_archivo()

   DEFINE
      v_detalle                  RECORD -- registro de detalle del archivo de salida
         v_nss                   CHAR(11),
         v_saldo_acciones_92,
         v_monto_acciones_92,
         v_saldo_acciones_97,
         v_monto_acciones_97     DECIMAL(25,6)
	   END RECORD,
      manejador_rpt              om.SaxDocumentHandler,
      v_ruta_reporte,
      v_ruta_ejecutable,
      v_ruta_listados            STRING,
      v_b_despliegue_pantalla    SMALLINT,
      v_fecha                    DATE
   
   DISPLAY "Se inicia la generacion del archivo"

   LET v_fecha = TODAY

   -- Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados

   -- Obtengo el nombre del reporte a generar
   LET v_ruta_reporte = v_ruta_listados.trim() , "/" ,
                        "resultado_proc_",
                        TODAY USING "yyyymmdd",
                        ".pdf"

   LET v_b_despliegue_pantalla = FALSE

   IF ( fgl_report_loadCurrentSettings("RET895.4rp") ) THEN
      CALL fgl_report_selectDevice ("PDF")
      -- si no se pidio el reporte en pantalla
      IF (v_b_despliegue_pantalla) THEN
         CALL fgl_report_selectPreview(TRUE)
      ELSE
         -- sin preview
         CALL fgl_report_selectPreview(FALSE)
         CALL fgl_report_setOutputFileName(v_ruta_reporte)
      END IF
      -- se indica que se escriba en archivo
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE         
      DISPLAY "No fue posible generar el reporte. No se encuentra la plantilla RET895.4rp"
      RETURN
   END IF
   
   --Inicia el reporte de Acta de Finiquito por SPEI
   START REPORT rpt_detalle_solicitado TO XML HANDLER manejador_rpt
   
   DECLARE cur_rep_movimiento CURSOR FOR
   SELECT nss,saldo_acciones_92,monto_acciones_92,
   saldo_acciones_97,monto_acciones_97
   FROM t6
   ORDER BY nss

   FOREACH cur_rep_movimiento INTO v_detalle.v_nss,v_detalle.v_saldo_acciones_92,
   v_detalle.v_monto_acciones_92,v_detalle.v_saldo_acciones_97,v_detalle.v_monto_acciones_97
      OUTPUT TO REPORT rpt_detalle_solicitado(v_detalle.v_nss,v_detalle.v_saldo_acciones_92,
      v_detalle.v_monto_acciones_92,v_detalle.v_saldo_acciones_97,v_detalle.v_monto_acciones_97,v_fecha)
   END FOREACH
   FREE cur_rep_movimiento
   
   FINISH REPORT rpt_detalle_solicitado

   DISPLAY "Nombre del reporte:",v_ruta_reporte
   DISPLAY "Termina de generar el reporte"
   
END FUNCTION

REPORT rpt_detalle_solicitado(v_nss,v_saldo_acciones_92,v_monto_acciones_92,v_saldo_acciones_97,v_monto_acciones_97,v_fecha)
   DEFINE
      v_nss                   CHAR(11),
      v_saldo_acciones_92,
      v_monto_acciones_92,
      v_saldo_acciones_97,
      v_monto_acciones_97     DECIMAL(25,6),
      v_fecha                 DATE
   FORMAT
      FIRST PAGE HEADER
         PRINTX v_fecha
      ON EVERY ROW
         PRINTX v_nss,v_saldo_acciones_92,v_monto_acciones_92,v_saldo_acciones_97,
         v_monto_acciones_97
END REPORT

--Funcion que integra la historia de un derechohabiente
FUNCTION integra_en_cta_movimiento_total2(v_id_derechohabiente, v_subcuenta)
   DEFINE v_id_derechohabiente   DECIMAL(9,0)
   DEFINE v_subcuenta            SMALLINT
   DEFINE v_cuenta               SMALLINT

   INITIALIZE v_cuenta TO NULL

   SELECT COUNT(*)
   INTO v_cuenta
   FROM cta_movimiento_total2
   WHERE id_derechohabiente = v_id_derechohabiente
   AND subcuenta = v_subcuenta
   AND movimiento IN (812,521)

   IF v_cuenta IS NULL OR v_cuenta = 0 THEN
      --Movimientos 2015
      INSERT INTO cta_movimiento_total2
      SELECT 2015, *
      FROM cta_movimiento
      WHERE movimiento IN (812,521)
      AND subcuenta = v_subcuenta
      AND id_derechohabiente = v_id_derechohabiente
      --Movimientos 2014
      INSERT INTO cta_movimiento_total2
      SELECT 2014, *
      FROM cta_movimiento14
      WHERE movimiento IN (812,521)
      AND subcuenta = v_subcuenta
      AND id_derechohabiente = v_id_derechohabiente
      --Movimientos 2013
      INSERT INTO cta_movimiento_total2
      SELECT 2013, *
      FROM cta_movimiento13
      WHERE movimiento IN (812,521)
      AND subcuenta = v_subcuenta
      AND id_derechohabiente = v_id_derechohabiente
      --Movimientos 2012
      INSERT INTO cta_movimiento_total2
      SELECT 2012, *
      FROM cta_movimiento12
      WHERE movimiento IN (812,521)
      AND subcuenta = v_subcuenta
      AND id_derechohabiente = v_id_derechohabiente
   END IF
   
END FUNCTION

FUNCTION crea_tablas1()

   TRY
      DROP TABLE cta_movimiento_total2
   CATCH
      DISPLAY "No existenn las tablas"
   END TRY

   CREATE TABLE cta_movimiento_total2 (
      anio                 SMALLINT,
      f_liquida            DATE,
      id_derechohabiente   DECIMAL(9,0),
      subcuenta            SMALLINT,
      fondo_inversion      SMALLINT,
      movimiento           SMALLINT,
      folio_liquida        DECIMAL(9,0),
      id_referencia        DECIMAL(9,0),
      monto_acciones       DECIMAL(16,6),
      monto_pesos          DECIMAL(12,2),
      f_valor              DATE,
      f_registro           DATE,
      h_registro           DATETIME HOUR TO SECOND,
      origen               CHAR(20))
   CREATE INDEX i_cta_movimiento_total2_1 ON cta_movimiento_total2(id_derechohabiente)
   CREATE INDEX i_cta_movimiento_total2_2 ON cta_movimiento_total2(id_derechohabiente,subcuenta)
   CREATE INDEX i_cta_movimiento_total2_3 ON cta_movimiento_total2(id_derechohabiente,subcuenta,movimiento)
   CREATE INDEX i_cta_movimiento_total2_4 ON cta_movimiento_total2(id_derechohabiente,subcuenta,movimiento,monto_acciones,f_registro)
   
END FUNCTION

FUNCTION crea_tablas2()

   TRY
      DROP TABLE revisa_log2
   CATCH
      DISPLAY "No existen las tablas"
   END TRY

   CREATE TABLE revisa_log2 (
      anio                  smallint,
      id_derechohabiente    decimal(9,0),
      nss                   CHAR(11),
      subcuenta             smallint,
      monto_acciones        decimal(16,6),
      saldo_acciones        decimal(16,6),
      saldo_pesos           decimal(16,6),
      f_registro            date,
      h_registro            datetime hour to second,
      f_registro_rev        date,
      h_registro_rev        datetime hour to SECOND,
      caso                  SMALLINT);
   CREATE INDEX i_revisa_log2_1 ON revisa_log2(nss)
   CREATE INDEX i_revisa_log2_2 ON revisa_log2(id_derechohabiente)
   CREATE INDEX i_revisa_log2_3 ON revisa_log2(caso,subcuenta)
END FUNCTION

FUNCTION crea_tablas3()

   WHENEVER ERROR CONTINUE
      DROP TABLE t92_revisa_log2
      DROP TABLE t97_revisa_log2
      DROP TABLE t1
      DROP TABLE t2
      DROP TABLE t3
      DROP TABLE t4
      DROP TABLE t5
      DROP TABLE t6
   WHENEVER ERROR STOP

   CREATE TABLE t92_revisa_log2 (
      nss                  CHAR(11),
      saldo_acciones       DECIMAL(16,6),
      monto_acciones       DECIMAL(16,6))
   CREATE INDEX i_t92_revisa_log2_1 ON t92_revisa_log2(nss)

   CREATE TABLE t97_revisa_log2 (
      nss                  CHAR(11),
      saldo_acciones       DECIMAL(16,6),
      monto_acciones       DECIMAL(16,6))
   CREATE INDEX i_t97_revisa_log2_1 ON t97_revisa_log2(nss)

   CREATE TABLE t1 (
      nss                  CHAR(11),
      saldo_acciones_92    DECIMAL(16,6),
      monto_acciones_92    DECIMAL(16,6),
      saldo_acciones_97    DECIMAL(16,6),
      monto_acciones_97    DECIMAL(16,6))
   CREATE INDEX i_t1_1 ON t1(nss)

   CREATE TABLE t2 (
      nss_92               CHAR(11),
      saldo_acciones_92    DECIMAL(16,6),
      monto_acciones_92    DECIMAL(16,6),
      nss_97               CHAR(11))
   CREATE INDEX i_t2_1 ON t2(nss_92)
   CREATE INDEX i_t2_2 ON t2(nss_97)

   CREATE TABLE t3 (
      nss                  CHAR(11),
      saldo_acciones_92    DECIMAL(16,6),
      monto_acciones_92    DECIMAL(16,6),
      saldo_acciones_97    DECIMAL(16,6),
      monto_acciones_97    DECIMAL(16,6))
   CREATE INDEX i_t3_1 ON t3(nss)

   CREATE TABLE t4 (
      nss_92               CHAR(11),
      saldo_acciones_97    DECIMAL(16,6),
      monto_acciones_97    DECIMAL(16,6),
      nss_97               CHAR(11))
   CREATE INDEX i_t4_1 ON t4(nss_92)
   CREATE INDEX i_t4_2 ON t4(nss_97)

   CREATE TABLE t5 (
      nss                  CHAR(11),
      saldo_acciones_92    DECIMAL(16,6),
      monto_acciones_92    DECIMAL(16,6),
      saldo_acciones_97    DECIMAL(16,6),
      monto_acciones_97    DECIMAL(16,6))
   CREATE INDEX i_t5_1 ON t5(nss)

   CREATE TABLE t6 (
      nss                  CHAR(11),
      saldo_acciones_92    DECIMAL(16,6),
      monto_acciones_92    DECIMAL(16,6),
      saldo_acciones_97    DECIMAL(16,6),
      monto_acciones_97    DECIMAL(16,6))
   CREATE INDEX i_t6_1 ON t6(nss)
   
END FUNCTION

