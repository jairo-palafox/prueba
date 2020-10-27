#############################################################################
#Módulo          => RET                                                     #        
#Programa        => RETSI18-2                                               #
#Objetivo        => Programa de detección de cuentas con saldo negativo     #
#                   en la subcuenta de Solo Infonavit 44, búsqueda del      #
#                   movimiento 172 que provocó el sobregiro para registrar  #
#                   el movimiento contrario                                 #
#                   Ejecución por única ocasión                             #
#Fecha Inicio    => Abril 11, 2018                                          #
#############################################################################
DATABASE safre_viv
GLOBALS 
       
END GLOBALS 
#Objetivo: Detectar cuentas con saldo negativo en la subcuenta Solo Infonavit
MAIN
--- Las actividades que realiza el programa son:
--- 1. Creación del tipo de movimiento para abonos
--- 2. Detección del universo de cuentas con saldos negativos
--- 3. Dentro de las cuentas anteriores buscar las que tengan doble movimiento de cargo 172 del mismo folio, fecha y aivs
--- 4. Registro del movimiento contrario
--- 5. Descarga del archivo final

DEFINE v_mensaje        CHAR(100)
   CALL fn_crea_temporales()
   LET v_mensaje = "Inicia Proceso: ", TODAY USING "dd/mm/yyyy", " - ", CURRENT HOUR TO SECOND
   CALL fn_inserta_mensaje(v_mensaje); 
--   CALL fn_crea_movimiento()              
   CALL fn_busca_cuentas_saldo_negativo()
   CALL fn_detecta_saldos_negativos()
--   CALL fn_procesa_desmarca()
--   CALL fn_descarga_rechazos()
--   CALL fn_descarga_cifras_control() 
--   CALL fn_descarga_log_proceso()

END MAIN

###################################################################################################################
## Creación del nuevo tipo de movimiento "ABONO POR CONCILIACION DE RETIRO SOLO INFONAVIT"                       ##
###################################################################################################################
FUNCTION fn_crea_movimiento()
DEFINE v_contador     SMALLINT 
DEFINE v_mensaje      CHAR(100)

   LET v_mensaje = "Inserta marca"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   INSERT INTO cat_movimiento VALUES (1821,'ABONO POR CONCILIACION DE RETIRO SOLO INFONAVIT',1,1,'ret',null,today,'OPSISSACI');

   SELECT COUNT(*) 
   INTO   v_contador
   FROM   cat_movimiento
   WHERE  movimiento = 1821;

   LET v_mensaje = "   Tipo de movimiento insertado: ",v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
                          
   LET v_mensaje = "Termina procesamiento de movimiento 1821 ABONO POR CONCILIACION DE RETIRO SOLO INFONAVIT"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

END FUNCTION 

###################################################################################################################
## funcion para insertar mensajes en tabla temporal para reporte de cifras control
###################################################################################################################
FUNCTION fn_inserta_mensaje(p_mensaje)
DEFINE p_mensaje      CHAR(100) 
DEFINE v_consecutivo  INTEGER 

   SELECT MAX(consecutivo)
   INTO   v_consecutivo
   FROM   tmp_mensajes_saci2018_2
   IF v_consecutivo IS NULL OR v_consecutivo = 0 THEN 
      LET v_consecutivo = 0
   END IF 
   LET v_consecutivo = v_consecutivo + 1
   INSERT INTO tmp_mensajes_saci2018_2 VALUES (v_consecutivo, p_mensaje);

END FUNCTION 

###################################################################################################################
## funcion para crear las tablas temporales
###################################################################################################################
FUNCTION fn_crea_temporales()
DEFINE v_query        STRING 

--   LET v_mensaje = "Crea tablas temporales "
--   CALL fn_inserta_mensaje(v_mensaje);

--   DROP TABLE IF EXISTS tmp_marcas_saci2018_2;
--   DROP TABLE IF EXISTS tmp_desmarcas_saci2018_2;
--   DROP TABLE IF EXISTS tmp_marcas_rch_saci2018_2;
--   DROP TABLE IF EXISTS tmp_desmarcas_rch_saci2018_2;
--   DROP TABLE IF EXISTS tmp_marcas_acep_saci2018_2;
--   DROP TABLE IF EXISTS tmp_desmarcas_acep_saci2018_2;
   DROP TABLE IF EXISTS tmp_mensajes_saci2018_2;
--   DROP TABLE IF EXISTS tmp_cifras_control_saci2018_2;
   
--   CREATE TEMP TABLE tmp_marcas_saci2018_2 (
--          nss   CHAR(11) );

--   CREATE TEMP TABLE tmp_desmarcas_saci2018_2 (
--          nss   CHAR(11),
--          marca SMALLINT );

--   CREATE TEMP TABLE tmp_marcas_rch_saci2018_2 (
--          nss        CHAR(11),
--          comentario CHAR(50) );

--   CREATE TEMP TABLE tmp_desmarcas_rch_saci2018_2 (
--          nss        CHAR(11),
--          marca      SMALLINT,
--          comentario CHAR(100) );

--   CREATE TEMP TABLE tmp_marcas_acep_saci2018_2 (
--          nss        CHAR(11));

--   CREATE TEMP TABLE tmp_desmarcas_acep_saci2018_2 (
--          nss        CHAR(11),
--          marca      SMALLINT,
--          referencia DECIMAL(9,0) );
          
   CREATE TEMP TABLE tmp_mensajes_saci2018_2 (
          consecutivo  INTEGER,
          mensaje      CHAR(100));
   CREATE UNIQUE INDEX ipktmp_mensajes_saci2018_2 on tmp_mensajes_saci2018_2(consecutivo);

--   CREATE TEMP TABLE tmp_cifras_control_saci2018_2 (
--          consecutivo  INTEGER, 
--          mensaje      CHAR(100));

--   LET v_mensaje = "Creación de tablas temporales finalizada "
--   CALL fn_inserta_mensaje(v_mensaje);
          
   LET v_query = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_marca_cuenta FROM v_query
   
   LET v_query = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   PREPARE prp_desmarca FROM v_query


   LET v_query = "INSERT INTO tmp_cta_mov_172_sub_44 \n",
                 "SELECT *                           \n", 
                 "FROM   cta_movimiento12            \n",
                 "WHERE  id_derechohabiente = ?      \n",
                 "AND    subcuenta = 44              \n",
                 "AND    movimiento = 172            \n",
                 "UNION ALL                          \n",
                 "SELECT *                           \n", 
                 "FROM   cta_movimiento13            \n",
                 "WHERE  id_derechohabiente = ?      \n",
                 "AND    subcuenta = 44              \n",
                 "AND    movimiento = 172            \n",
                 "UNION ALL                          \n",
                 "SELECT *                           \n", 
                 "FROM   cta_movimiento14            \n",
                 "WHERE  id_derechohabiente = ?      \n",
                 "AND    subcuenta = 44              \n",
                 "AND    movimiento = 172            \n",
                 "UNION ALL                          \n",
                 "SELECT *                           \n", 
                 "FROM   cta_movimiento15            \n",
                 "WHERE  id_derechohabiente = ?      \n",
                 "AND    subcuenta = 44              \n",
                 "AND    movimiento = 172            \n",
                 "UNION ALL                          \n",
                 "SELECT *                           \n", 
                 "FROM   cta_movimiento16            \n",
                 "WHERE  id_derechohabiente = ?      \n",
                 "AND    subcuenta = 44              \n",
                 "AND    movimiento = 172            \n",
                 "UNION ALL                          \n",
                 "SELECT *                           \n", 
                 "FROM   cta_movimiento              \n",
                 "WHERE  id_derechohabiente = ?      \n",
                 "AND    subcuenta = 44              \n",
                 "AND    movimiento = 172            \n"
   PREPARE prp_busca_172 FROM v_query

END FUNCTION 

###################################################################################################################
## funcion para detectar las cuentas con saldo negativo
###################################################################################################################
FUNCTION fn_busca_cuentas_saldo_negativo()
DEFINE v_archivo      CHAR(60)
DEFINE v_archivo_des  CHAR(60)
DEFINE v_contador     INTEGER 
DEFINE v_mensaje      CHAR(100)

   LET v_contador = 0
   LET v_mensaje = "Inicia búsqueda de movimientos"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
  
   LET v_mensaje = "    buscando movimientos:",v_archivo
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   SELECT * 
   FROM   cta_movimiento
   WHERE  subcuenta = 44
   INTO TEMP tmp_movimientos_solo_infonavit; 

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_movimientos_solo_infonavit

   LET v_mensaje = "    Se cargaron: ", v_contador, " movimientos"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_mensaje = "Búsqueda de movimientos finalizada"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 

###################################################################################################################
## funcion para procesar los registros de marca
###################################################################################################################
FUNCTION fn_detecta_saldos_negativos()
DEFINE v_resultado          SMALLINT 
DEFINE v_marca              SMALLINT 
DEFINE v_contador           INTEGER 
DEFINE v_nss                CHAR(11)
DEFINE v_inexistentes       INTEGER 
DEFINE v_marca_previa       INTEGER 
DEFINE v_marcados           INTEGER 
DEFINE v_problema_marca     INTEGER 
DEFINE v_mensaje            CHAR(100)
DEFINE v_id_derechohabiente DECIMAL(10,0)
DEFINE v_suma_pesos         DECIMAL(22,2)
DEFINE v_suma_acciones      DECIMAL(22,2)
DEFINE v_f_liquida          DATE 
DEFINE v_folio_liquida      DECIMAL(10,0)
DEFINE v_monto_acciones     DECIMAL(22,2)
DEFINE v_num_movs           SMALLINT 
DEFINE v_origen             CHAR(20)

   LET v_contador           = 0
   LET v_inexistentes       = 0
   LET v_marca_previa       = 0
   LET v_id_derechohabiente = 0
   LET v_marcados           = 0
   LET v_marca              = 822
   LET v_problema_marca     = 0
   LET v_f_liquida          = NULL
   LET v_folio_liquida      = 0
   LET v_monto_acciones     = 0

   LET v_mensaje = "Inicia el de detección de saldos negativos"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   -- SE CREA LA TABLA TEMPORAL DONDE SE GUARDARÀN LOS MOVIMIENTO 172

   SELECT * 
   FROM   cta_movimiento 
   WHERE  1 = 2
   INTO TEMP tmp_cta_mov_172_sub_44;

   -- SE CREA LA TABLA PARA INSERTAR LOS MOVIMIENTOS A LIQUIDAR

   SELECT * 
   FROM   cta_movimiento 
   WHERE  1 = 2
   INTO TEMP tmp_movtos_abono;

   DECLARE cur_negativos CURSOR FOR 
                         SELECT SUM(monto_pesos) AS suma_pesos, SUM(monto_acciones) AS suma_acciones, 
                                id_derechohabiente
                         FROM   cta_movimiento
                         WHERE  subcuenta = 44
                         GROUP BY id_derechohabiente
                         HAVING SUM(monto_acciones) < 0

   FOREACH cur_negativos INTO v_suma_pesos, v_suma_acciones, v_id_derechohabiente
      IF v_id_derechohabiente IS NOT NULL AND v_id_derechohabiente <> 0 THEN 
         EXECUTE prp_busca_172 USING v_id_derechohabiente,
                                     v_id_derechohabiente,
                                     v_id_derechohabiente,
                                     v_id_derechohabiente,
                                     v_id_derechohabiente,
                                     v_id_derechohabiente
         SELECT COUNT(*), f_liquida, folio_liquida, monto_acciones
         INTO   v_num_movs, v_f_liquida, v_folio_liquida, v_monto_acciones
         FROM   tmp_cta_mov_172_sub_44
         GROUP BY f_liquida, folio_liquida, monto_acciones
         HAVING COUNT(*) > 2
         IF v_num_movs IS NOT NULL AND v_num_movs <> 0 THEN 
            LET v_origen = "AJUSTE ", v_folio_liquida
            INSERT INTO tmp_movtos_abono 
                 VALUES (TODAY, v_id_derechohabiente, 44, 11, v_movto_nuevo, 0, v_monto_acciones, v_f_liquida,
                         TODAY, CURRENT HOUR TO SECOND, v_origen); 
         END IF
      END IF 
   END FOREACH 


   LET v_mensaje = "Termina de procesar el archivo de marcas"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 
###################################################################################################################
## funcion para insertar rechazos por marca 
###################################################################################################################
FUNCTION fn_inserta_rechazos_marca(p_nss,p_mensaje)
DEFINE p_nss          CHAR(11)
DEFINE p_mensaje      CHAR(100) 

   INSERT INTO tmp_marcas_rch_saci2018_2 VALUES (p_nss, p_mensaje);

END FUNCTION 
###################################################################################################################
## funcion para insertar rechazos por desmarca 
###################################################################################################################
FUNCTION fn_inserta_rechazos_desmarca(p_nss,p_marca, p_mensaje)
DEFINE p_nss          CHAR(11)
DEFINE p_marca        SMALLINT
DEFINE p_mensaje      CHAR(100) 

   INSERT INTO tmp_desmarcas_rch_saci2018_2 VALUES (p_nss, p_marca, p_mensaje);

END FUNCTION 
###################################################################################################################
## funcion para insertar aceptados por marca 
###################################################################################################################
FUNCTION fn_inserta_aceptados_marca(p_nss)
DEFINE p_nss          CHAR(11)
DEFINE p_mensaje      CHAR(100) 

   INSERT INTO tmp_marcas_acep_saci2018_2 VALUES (p_nss);

END FUNCTION 
###################################################################################################################
## funcion para insertar aceptados por desmarca 
###################################################################################################################
FUNCTION fn_inserta_aceptados_desmarca(p_nss,p_marca, p_referencia)
DEFINE p_nss          CHAR(11)
DEFINE p_marca        SMALLINT
DEFINE p_referencia   DECIMAL(9,0)

   INSERT INTO tmp_desmarcas_acep_saci2018_2 VALUES (p_nss, p_marca, p_referencia);

END FUNCTION 
###################################################################################################################
## funcion para descargar los rechazos
###################################################################################################################
FUNCTION fn_descarga_rechazos()
DEFINE v_resultado    SMALLINT 
DEFINE v_marca        SMALLINT 
DEFINE v_archivo      CHAR(60)
DEFINE v_contador     INTEGER 
DEFINE v_mensaje      CHAR(100)

   LET v_contador = 0
   LET v_mensaje = "Inicia descarga de rechazos";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_rch_saci2018_2
   IF v_contador > 0 THEN 
      LET v_mensaje = "    Descargando rechazos de marca"
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/marcas_rch_TJ.unl"
      UNLOAD TO v_archivo SELECT * FROM tmp_marcas_rch_saci2018_2
      LET v_mensaje = "    Rechazos por marca descargados en: ", v_archivo;
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/marcas_rch_TJ.unl"
      UNLOAD TO v_archivo SELECT * FROM tmp_marcas_rch_saci2018_2
   ELSE 
      LET v_mensaje = "    No existen rechazos por marca";
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
   END IF 

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_rch_saci2018_2
   IF v_contador > 0 THEN 
      LET v_mensaje = "    Descargando rechazos de desmarca"
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/desmarcas_rch_TJ.unl"
      UNLOAD TO v_archivo SELECT * FROM tmp_desmarcas_rch_saci2018_2
      LET v_mensaje = "    Rechazos por desmarca descargados en: ", v_archivo;
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
   ELSE 
      LET v_mensaje = "    No existen rechazos por desmarca";
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/desmarcas_rch_TJ.unl"
      UNLOAD TO v_archivo SELECT * FROM tmp_desmarcas_rch_saci2018_2
   END IF 

   LET v_mensaje = "Descarga de rechazos finalizada";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

-----
   LET v_contador = 0
   LET v_mensaje = "Inicia descarga de aceptados";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_acep_saci2018_2
   IF v_contador > 0 THEN 
      LET v_mensaje = "    Descargando aceptados de marca"
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/marcas_acep_TJ.unl"
      UNLOAD TO v_archivo SELECT * FROM tmp_marcas_acep_saci2018_2
      LET v_mensaje = "    Aceptados por marca descargados en: ", v_archivo;
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
   ELSE 
      LET v_mensaje = "    No existen aceptados por marca";
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/marcas_acep_TJ.unl"
      UNLOAD TO v_archivo SELECT * FROM tmp_marcas_acep_saci2018_2
   END IF 

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_acep_saci2018_2
   IF v_contador > 0 THEN 
      LET v_mensaje = "    Descargando aceptados de desmarca"
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/desmarcas_acep_TJ.unl" --- se crea el archivo vacio por si se debe reversar no falle el reverso
      UNLOAD TO v_archivo SELECT * FROM tmp_desmarcas_acep_saci2018_2
      LET v_mensaje = "    Aceptados por desmarca descargados en: ", v_archivo;
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
   ELSE 
      LET v_mensaje = "    No existen aceptados por desmarca";
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/desmarcas_acep_TJ.unl"   --- se crea el archivo vacio por si se debe reversar no falle el reverso
      UNLOAD TO v_archivo SELECT * FROM tmp_desmarcas_acep_saci2018_2
   END IF 

   LET v_mensaje = "Descarga de aceptados finalizada";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

-----   
END FUNCTION 

###################################################################################################################
## funcion para descargar las cifras de control
###################################################################################################################
FUNCTION fn_descarga_cifras_control()
DEFINE v_resultado    SMALLINT 
DEFINE v_marca        SMALLINT 
DEFINE v_archivo      CHAR(60)
DEFINE v_contador     INTEGER 
DEFINE v_correctos    INTEGER 
DEFINE v_mensaje      CHAR(100)

   LET v_contador = 0
   LET v_mensaje = "Inicia descarga de Cifras de Control";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   CALL fn_inserta_cifras_control( 1, "Cifras Totales de Control");

   --- Registros procesados de marca
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_saci2018_2
   LET v_mensaje = "Registros procesados de marca_________________:", v_contador;
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 2, v_mensaje)

   LET v_correctos = v_contador -- Se asignan los totales a la variable para posteriormente restale los rechazados
   
   --- Registros rechazados del archivo de marca
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_rch_saci2018_2
   LET v_mensaje = "Registros rechazados del archivo de marca_____:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 4, v_mensaje)

   LET v_correctos = v_correctos - v_contador
   LET v_mensaje = "Registros efectivamente marcados______________:", v_correctos
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 3, v_mensaje)

   --- Registros inexistentes del archivo de marca
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_rch_saci2018_2
   WHERE  comentario = 'NSS INEXISTENTE'
   LET v_mensaje = "Registros inexistentes del archivo de marca___:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 5, v_mensaje)

   --- Registros previamente marcados
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_rch_saci2018_2
   WHERE  comentario = 'NSS PREVIAMENTE MARCADO'
   LET v_mensaje = "Registros previamente marcados________________:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 6, v_mensaje)

   --- Registros con problemas en la marca   
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_rch_saci2018_2
   WHERE  comentario = 'NSS CON PROBLEMAS EN LA MARCA'
   LET v_mensaje = "Registros con problemas en la marca___________:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 7, v_mensaje)

   --- Registros procesados de desmarca
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_saci2018_2
   LET v_mensaje = "Registros procesados de desmarca______________:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 8, v_mensaje)

   LET v_correctos = v_contador
   
   --- Registros rechazados del archivo de marca
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_rch_saci2018_2
   LET v_mensaje = "Registros rechazados del archivo de marca_____:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control(10, v_mensaje)

   LET v_correctos = v_correctos - v_contador
   LET v_mensaje = "Registros efectivamente desmarcados___________:", v_correctos
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 9, v_mensaje)

   --- Registros inexistentes del archivo de desmarca
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_rch_saci2018_2
   WHERE  comentario = 'NSS INEXISTENTE'
   LET v_mensaje = "Registros inexistentes del archivo de desmarca:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control(11, v_mensaje)

   --- Registros no marcados para desmarcar
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_rch_saci2018_2
   WHERE  comentario = 'NSS NO SE ENCUENTRA MARCADO'
   LET v_mensaje = "Registros no marcados para desmarcar__________:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control(12, v_mensaje)

   --- Registros con problemas en la marca   
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_rch_saci2018_2
   WHERE  comentario = 'NSS CON PROBLEMAS EN LA DESMARCA'
   LET v_mensaje = "Registros con problemas en la desmarca________:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control(13, v_mensaje)

   LET v_mensaje = "    Bajando información"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   LET v_archivo = "/safreviv_req/SACI2018-2/cifras_control_TJ.unl"
   UNLOAD TO v_archivo DELIMITER '' SELECT * FROM tmp_cifras_control_saci2018_2 ORDER BY consecutivo

   LET v_mensaje = "    Archivo generado de Cifras Control: ", v_archivo
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_mensaje = "Descarga de cifras control finalizada"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 

###################################################################################################################
## funcion para insertar cifras de control 
###################################################################################################################
FUNCTION fn_inserta_cifras_control(p_consecutivo, p_mensaje)
DEFINE p_consecutivo  INTEGER
DEFINE p_mensaje      CHAR(100) 
DEFINE v_resultado    SMALLINT 
DEFINE v_marca        SMALLINT 
DEFINE v_consecutivo  INTEGER 

   INSERT INTO tmp_cifras_control_saci2018_2 VALUES (p_consecutivo, p_mensaje);

END FUNCTION 

###################################################################################################################
## funcion para descargar el log del proceso
###################################################################################################################
FUNCTION fn_descarga_log_proceso()
DEFINE v_resultado    SMALLINT 
DEFINE v_marca        SMALLINT 
DEFINE v_archivo      CHAR(60)
DEFINE v_contador     INTEGER 
DEFINE v_correctos    INTEGER 
DEFINE v_mensaje      CHAR(100)
   LET v_contador = 0
   LET v_mensaje = "Finaliza Proceso: ", TODAY USING "dd/mm/yyyy", " - ", CURRENT HOUR TO SECOND
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje); 
   
   LET v_archivo = "/safreviv_req/SACI2018-2/log_proceso_TJ.unl"
   UNLOAD TO v_archivo DELIMITER '' SELECT * FROM tmp_mensajes_saci2018_2 ORDER BY consecutivo
 
END FUNCTION 
