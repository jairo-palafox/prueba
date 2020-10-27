#############################################################################
#Módulo          => RET                                                     #        
#Programa        => RETSI18-2R                                              #
#Objetivo        => Programa de Reverso del proceso de marca y desmarca     #
#                   trámite judicial                                        #
#                   Ejecución por única ocasión                             #
#Fecha Inicio    => Marzo 6, 2018                                           #
#############################################################################
DATABASE safre_viv
GLOBALS 
       
END GLOBALS 
#Objetivo: Reversar el proceso de Actualización de las marcas de Jurídico
MAIN
--- Las actividades que realiza el programa son:
--- 1. Eliminación de la nueva marca "Actualización de marcas por trámite judicial del proceso de Devolición del SSV"
--- 2. Carga de los archivos de marca y desmarca aceptados en el proceso de Actualización a tablas temporales
--- 3. Proceso de reverso de marca
--- 4. Proceso de reverso de desmarca
--- 5. Descarga las cifras de control del reverso a archivo plano

DEFINE v_mensaje        CHAR(100)
   CALL fn_crea_temporales()
   LET v_mensaje = "Inicia Proceso: ", TODAY USING "dd/mm/yyyy", " - ", CURRENT HOUR TO SECOND
   CALL fn_inserta_mensaje(v_mensaje); 
   CALL fn_reversa_crea_marca()              
   CALL fn_carga_archivos()          
   CALL fn_procesa_reverso_marca()           
   CALL fn_procesa_reverso_desmarca()
   CALL fn_descarga_cifras_control() 
   CALL fn_descarga_log_proceso()

END MAIN

###################################################################################################################
## Creación de la nueva marca "Actualización de marcas por trámite judicial del proceso de Devolición del SSV"   ##
###################################################################################################################
FUNCTION fn_reversa_crea_marca()
DEFINE v_contador     SMALLINT 
DEFINE v_mensaje      CHAR(100)

   LET v_mensaje = "Reversa marca"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   DELETE 
   FROM   sfr_marca 
   WHERE  marca = 822;
   
   DELETE 
   FROM   sfr_convivencia 
   WHERE  marca_entra = 822 OR 
          marca_activa = 822;

   DELETE 
   FROM   cat_rch_marca 
   WHERE  rch_cod = 822;

   LET v_mensaje = "Termina reverso de marca 822 ACT MARCAS POR TJ DEVOLUCIÓN DEL SSV"
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

   DROP TABLE IF EXISTS tmp_marcas_acep_saci2018_2;
   DROP TABLE IF EXISTS tmp_desmarcas_acep_saci2018_2;
   DROP TABLE IF EXISTS tmp_mensajes_saci2018_2;
   DROP TABLE IF EXISTS tmp_cifras_control_saci2018_2;
   
   CREATE TEMP TABLE tmp_marcas_acep_saci2018_2 (
          nss        CHAR(11));

   CREATE TEMP TABLE tmp_desmarcas_acep_saci2018_2 (
          nss        CHAR(11),
          marca      SMALLINT,
          referencia DECIMAL(9,0) );
          
   CREATE TEMP TABLE tmp_mensajes_saci2018_2 (
          consecutivo  INTEGER,
          mensaje      CHAR(100));
   CREATE UNIQUE INDEX ipktmp_mensajes_saci2018_2 on tmp_mensajes_saci2018_2(consecutivo);

   CREATE TEMP TABLE tmp_cifras_control_saci2018_2 (
          consecutivo  INTEGER, 
          mensaje      CHAR(100));
   
END FUNCTION 

###################################################################################################################
## funcion para cargar los archivos a tablas temporales
###################################################################################################################
FUNCTION fn_carga_archivos()
DEFINE v_archivo      CHAR(60)
DEFINE v_archivo_des  CHAR(60)
DEFINE v_contador     INTEGER 
DEFINE v_mensaje      CHAR(100)

   LET v_contador = 0
   LET v_mensaje = "Inicia carga de archivos para reverso"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_archivo = "/safreviv_req/SACI2018-2/marcas_acep_TJ.unl"
   
   LET v_mensaje = "    cargando archivo de marcas aceptadas para reverso:",v_archivo
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   LOAD FROM v_archivo INSERT INTO tmp_marcas_acep_saci2018_2;

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_acep_saci2018_2

   LET v_mensaje = "    Se cargaron: ", v_contador, " del archivo de marcas aceptadas para reverso"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_archivo_des = "/safreviv_req/SACI2018-2/desmarcas_acep_TJ.unl"

   LET v_mensaje = "    cargando archivo de desmarcas aceptadas para reverso:",v_archivo_des
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   LOAD FROM v_archivo_des INSERT INTO tmp_desmarcas_acep_saci2018_2;

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_acep_saci2018_2

   LET v_mensaje = "    Se cargaron: ", v_contador, " del archivo de desmarcas aceptadas para reverso"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_mensaje = "Carga de archivos para reverso finalizada"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 

###################################################################################################################
## funcion para procesar los registros de marca
###################################################################################################################
FUNCTION fn_procesa_reverso_marca()
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

   LET v_contador           = 0
   LET v_inexistentes       = 0
   LET v_marca_previa       = 0
   LET v_id_derechohabiente = 0
   LET v_marcados           = 0
   LET v_marca              = 822
   LET v_problema_marca     = 0

   LET v_mensaje = "Inicia el proceso de reverso de marca de cuentas"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   DELETE 
   FROM   sfr_marca_activa
   WHERE  marca = v_marca;

   DELETE 
   FROM   sfr_marca_historica
   WHERE  marca = v_marca;

   LET v_mensaje = "Termina de procesar el reverso de marca de cuentas"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 
###################################################################################################################
## funcion para procesar los registros de desmarca
###################################################################################################################
FUNCTION fn_procesa_reverso_desmarca()
DEFINE v_resultado          SMALLINT 
DEFINE v_marca              SMALLINT 
DEFINE v_contador           INTEGER 
DEFINE v_nss                CHAR(11)
DEFINE v_inexistentes       INTEGER 
DEFINE v_sin_marca_previa   INTEGER 
DEFINE v_desmarcados        INTEGER 
DEFINE v_problema_desmarca  INTEGER 
DEFINE v_referencia         DECIMAL(9,0)
DEFINE v_mensaje            CHAR(100)
DEFINE v_id_derechohabiente DECIMAL(10,0)
DEFINE v_actualizadas       INTEGER
DEFINE v_remarcadas         INTEGER 


   LET v_contador           = 0
   LET v_inexistentes       = 0
   LET v_sin_marca_previa   = 0
   LET v_id_derechohabiente = 0
   LET v_desmarcados        = 0
   LET v_marca              = 0
   LET v_problema_desmarca  = 0
   LET v_referencia         = 0
   LET v_actualizadas       = 0
   LET v_remarcadas         = 0

   LET v_mensaje = "Inicia reverso de registros para desmarca";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   DECLARE cur_desmarca CURSOR FOR SELECT * FROM tmp_desmarcas_acep_saci2018_2
   FOREACH cur_desmarca INTO v_nss, v_marca, v_referencia 
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss  = v_nss
      IF v_id_derechohabiente IS NOT NULL AND v_id_derechohabiente <> 0 THEN 
         UPDATE sfr_marca_historica
         SET    f_fin            = NULL,
                proceso_desmarca = NULL,
                usuario_desmarca = NULL  
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    marca              = v_marca
         AND    n_referencia       = v_referencia
         LET v_actualizadas = v_actualizadas + 1

         INSERT INTO sfr_marca_activa 
         SELECT id_derechohabiente, marca, n_referencia, f_inicio, h_inicio, folio,
                proceso_marca, marca_causa, f_marca_causa, f_vigencia, usuario_marca
         FROM   sfr_marca_historica
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    marca              = v_marca
         AND    n_referencia       = v_referencia

         LET v_remarcadas = v_remarcadas + 1
      END IF 

   END FOREACH
   
   LET v_mensaje = "Cuentas actualizadas en el historico_______________________:", v_actualizadas
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 6, v_mensaje)
   LET v_mensaje = "Cuentas marcadas nuevamente por reverso____________________:", v_remarcadas
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 7, v_mensaje)
   LET v_mensaje = "Termina de procesar el reverso de desmarcas";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
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
   END IF 

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_acep_saci2018_2
   IF v_contador > 0 THEN 
      LET v_mensaje = "    Descargando aceptados de desmarca"
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
      LET v_archivo = "/safreviv_req/SACI2018-2/desmarcas_acep_TJ.unl"
      UNLOAD TO v_archivo SELECT * FROM tmp_desmarcas_acep_saci2018_2
      LET v_mensaje = "    Aceptados por desmarca descargados en: ", v_archivo;
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
   ELSE 
      LET v_mensaje = "    No existen aceptados por desmarca";
      DISPLAY v_mensaje
      CALL fn_inserta_mensaje(v_mensaje);
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
   LET v_mensaje = "Inicia descarga de Cifras de Control del Reverso";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   CALL fn_inserta_cifras_control( 1, "Cifras Totales de Control del Reverso");

   --- Registros procesados para reverso de marca
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_acep_saci2018_2
   LET v_mensaje = "Registros procesados de marca para reverso_________________:", v_contador;
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 2, v_mensaje)

   --- Cuentas marcadas con la marca 822
   SELECT COUNT(*)
   INTO   v_contador
   FROM   sfr_marca_activa
   WHERE  marca = 822
   LET v_mensaje = "Cuentas marcadas con la marca 822__________________________:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 3, v_mensaje)

   --- Cuentas marcadas en el historico con la marca 822
   SELECT COUNT(*)
   INTO   v_contador
   FROM   sfr_marca_historica
   WHERE  marca = 822
   LET v_mensaje = "Cuentas marcadas en el historico con la marca 822__________:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 4, v_mensaje)

   --- Cuentas marcadas nuevamente
   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_acep_saci2018_2
   LET v_mensaje = "Cuentas para volver a marcar por reverso___________________:", v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_cifras_control( 5, v_mensaje)

   LET v_mensaje = "    Bajando información cifras control del reverso"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   LET v_archivo = "/safreviv_req/SACI2018-2/cifras_control_reverso_TJ.unl"
   UNLOAD TO v_archivo DELIMITER '' SELECT * FROM tmp_cifras_control_saci2018_2 ORDER BY consecutivo

   LET v_mensaje = "    Archivo generado de Cifras Control del reverso: ", v_archivo
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_mensaje = "Descarga de cifras control del reverso finalizada"
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
   LET v_mensaje = "Finaliza Proceso de Reverso: ", TODAY USING "dd/mm/yyyy", " - ", CURRENT HOUR TO SECOND
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje); 
   
   LET v_archivo = "/safreviv_req/SACI2018-2/log_proceso_reverso_TJ.unl"
   UNLOAD TO v_archivo DELIMITER '' SELECT * FROM tmp_mensajes_saci2018_2 ORDER BY consecutivo
 
END FUNCTION 
