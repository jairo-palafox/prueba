#############################################################################
#Módulo          => RET                                                     #        
#Programa        => RETSI18-2                                               #
#Objetivo        => Programa de marca y desmarca trámite judicial           #
#                   Ejecución por única ocasión                             #
#Fecha Inicio    => Febrero 22, 2018                                        #
#############################################################################
DATABASE safre_viv
GLOBALS 
       
END GLOBALS 
#Objetivo: Actualizar las marcas de Jurídico
MAIN
--- Las actividades que realiza el programa son:
--- 1. Creación de la nueva marca "Actualización de marcas por trámite judicial del proceso de Devolición del SSV"
--- 2. Carga de los archivos de marca y desmarca a tablas temporales
--- 3. Proceso de marca, considera que si al intentar marcar el nss ya se encuentra marcado con la misma marca 
---    inserta el registro en la tabla temporal rechazados por marca
--- 4. Proceso de desmarca, considera que si al intentar desmarcar no se encuentra marcado inserta en la tabla
---    temporal rechazados por desmarca
--- 5. Descarga los rechazados a archivo plano
--- 6. Descarga las cifras de control a archivo plano

DEFINE v_mensaje        CHAR(100)
   CALL fn_crea_temporales()
   LET v_mensaje = "Inicia Proceso: ", TODAY USING "dd/mm/yyyy", " - ", CURRENT HOUR TO SECOND
   CALL fn_inserta_mensaje(v_mensaje); 
   --CALL fn_crea_marca()              
   CALL fn_carga_archivos()          
   --CALL fn_procesa_marca()           
   CALL fn_procesa_desmarca()
   CALL fn_descarga_rechazos()
   CALL fn_descarga_cifras_control() 
   CALL fn_descarga_log_proceso()

END MAIN

###################################################################################################################
## Creación de la nueva marca "Actualización de marcas por trámite judicial del proceso de Devolición del SSV"   ##
###################################################################################################################
FUNCTION fn_crea_marca()
DEFINE v_contador     SMALLINT 
DEFINE v_mensaje      CHAR(100)

   LET v_mensaje = "Inserta marca"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   INSERT INTO sfr_marca VALUES (822,'ACT MARCAS POR TJ DEVOLUCIÓN DEL SSV',0,0,0,0,TODAY,'OPSISSACI');
   INSERT INTO cat_rch_marca VALUES (822,'EN ACT MARCAS POR TJ DEVOLUCIÓN DEL SSV', TODAY, 'OPSISSACI');

   SELECT COUNT(*) 
   INTO   v_contador
   FROM   sfr_marca
   WHERE  marca = 822;

   LET v_mensaje = "   Marca insertada 822: ",v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_mensaje = "   Inserta la convivencia "
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
  

   INSERT INTO sfr_convivencia 
   SELECT 822, marca_activa, 0, 0, TODAY, 'OPSISSACI'
   FROM   sfr_convivencia
   WHERE  marca_entra = 803;

   SELECT COUNT(*)
   INTO   v_contador
   FROM   sfr_convivencia
   WHERE  marca_entra = 822;

   LET v_mensaje = "   Convivencia entrante 822: ",v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);


   INSERT INTO sfr_convivencia 
   SELECT marca_entra, 822, 0, 0, TODAY, 'OPSISSACI'
   FROM   sfr_convivencia
   WHERE  marca_activa = 803;

   SELECT COUNT(*)
   INTO   v_contador
   FROM   sfr_convivencia
   WHERE  marca_activa = 822;

   LET v_mensaje = "   Convivencia activa 822: ",v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   UPDATE sfr_convivencia
   SET    ind_convivencia = 20,
          rch_cod         = 822
   WHERE  marca_activa    = 822
   AND    marca_entra IN (201,202,203,204,205,206,210,
                          211,212,213,214,215,216,217,
                          218,221,223,232,233,280,501,
                          502,590,701,702,803,805,806,
                          808,815,819);

   SELECT COUNT(*)
   INTO   v_contador
   FROM   sfr_convivencia
   WHERE  marca_activa = 822
   AND    ind_convivencia = 20;

   LET v_mensaje = "   Combinaciones de no convivencia 822: ",v_contador
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
                          
   LET v_mensaje = "Termina procesamiento de marca 822 ACT MARCAS POR TJ DEVOLUCIÓN DEL SSV"
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

   DROP TABLE IF EXISTS tmp_marcas_saci2018_2;
   DROP TABLE IF EXISTS tmp_desmarcas_saci2018_2;
   DROP TABLE IF EXISTS tmp_marcas_rch_saci2018_2;
   DROP TABLE IF EXISTS tmp_desmarcas_rch_saci2018_2;
   DROP TABLE IF EXISTS tmp_marcas_acep_saci2018_2;
   DROP TABLE IF EXISTS tmp_desmarcas_acep_saci2018_2;
   DROP TABLE IF EXISTS tmp_mensajes_saci2018_2;
   DROP TABLE IF EXISTS tmp_cifras_control_saci2018_2;
   
   CREATE TEMP TABLE tmp_marcas_saci2018_2 (
          nss   CHAR(11) );

   CREATE TEMP TABLE tmp_desmarcas_saci2018_2 (
          nss   CHAR(11),
          marca SMALLINT );

   CREATE TEMP TABLE tmp_marcas_rch_saci2018_2 (
          nss        CHAR(11),
          comentario CHAR(50) );

   CREATE TEMP TABLE tmp_desmarcas_rch_saci2018_2 (
          nss        CHAR(11),
          marca      SMALLINT,
          comentario CHAR(100) );

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

--   LET v_mensaje = "Creación de tablas temporales finalizada "
--   CALL fn_inserta_mensaje(v_mensaje);
          
   LET v_query = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_marca_cuenta FROM v_query
   
   LET v_query = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   PREPARE prp_desmarca FROM v_query
   
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
   LET v_mensaje = "Inicia carga de archivos"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_archivo = "/safreviv_req/SACI2018-2/marcas_TJ.unl"
   
   LET v_mensaje = "    cargando archivo de marcas:",v_archivo
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   LOAD FROM v_archivo INSERT INTO tmp_marcas_saci2018_2;

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_marcas_saci2018_2

   LET v_mensaje = "    Se cargaron: ", v_contador, " del archivo de marcas"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_archivo_des = "/safreviv_req/SACI2018-2/desmarcas_TJ.unl"

   LET v_mensaje = "    cargando archivo de desmarcas:",v_archivo_des
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
   LOAD FROM v_archivo_des INSERT INTO tmp_desmarcas_saci2018_2;

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_desmarcas_saci2018_2

   LET v_mensaje = "    Se cargaron: ", v_contador, " del archivo de desmarcas"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_mensaje = "Carga de archivos finalizada"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 

###################################################################################################################
## funcion para procesar los registros de marca
###################################################################################################################
FUNCTION fn_procesa_marca()
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

   LET v_mensaje = "Inicia el proceso de marca de cuentas"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   DECLARE cur_marca CURSOR FOR SELECT * FROM tmp_marcas_saci2018_2
   FOREACH cur_marca INTO v_nss 
      LET v_mensaje = ""
      LET v_id_derechohabiente = 0
      LET v_contador = 0
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss  = v_nss
      IF v_id_derechohabiente IS NOT NULL AND v_id_derechohabiente <> 0 THEN 
         SELECT COUNT(*)
         INTO   v_contador
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    marca = v_marca
         IF v_contador = 0 THEN 
            LET v_marcados = v_marcados + 1
            EXECUTE prp_marca_cuenta USING v_id_derechohabiente,
                                           v_marca,
                                           v_marcados,
                                           '0',
                                           '0',
                                           '0',
                                           '0',
                                           '',
                                           'OPSISSACI',
                                           ''
                  INTO v_resultado
            IF v_resultado <> 0 THEN 
               CALL fn_inserta_rechazos_marca(v_nss,"NSS CON PROBLEMAS EN LA MARCA");
               LET v_mensaje = "    NSS No fue posible marcarlo: ", v_nss
               DISPLAY v_mensaje
               CALL fn_inserta_mensaje(v_mensaje);
               LET v_marcados = v_marcados -1
               LET v_problema_marca = v_problema_marca + 1
            ELSE 
               CALL fn_inserta_aceptados_marca(v_nss);
            END IF 
         ELSE 
            CALL fn_inserta_rechazos_marca(v_nss,"NSS PREVIAMENTE MARCADO");
            LET v_mensaje = "    NSS Previamente marcado: ", v_nss
            DISPLAY v_mensaje
            CALL fn_inserta_mensaje(v_mensaje);
            LET v_marca_previa = v_marca_previa + 1
         END IF 
      ELSE 
         CALL fn_inserta_rechazos_marca(v_nss,"NSS INEXISTENTE");
         LET v_mensaje = "    NSS inexistente: ", v_nss
         DISPLAY v_mensaje
         CALL fn_inserta_mensaje(v_mensaje);
         LET v_inexistentes = v_inexistentes + 1 
      END IF 

   END FOREACH 


   LET v_mensaje = "Termina de procesar el archivo de marcas"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 
###################################################################################################################
## funcion para procesar los registros de desmarca
###################################################################################################################
FUNCTION fn_procesa_desmarca()
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
DEFINE v_sql                STRING 


   LET v_contador           = 0
   LET v_inexistentes       = 0
   LET v_sin_marca_previa   = 0
   LET v_id_derechohabiente = 0
   LET v_desmarcados        = 0
   LET v_marca              = 0
   LET v_problema_desmarca  = 0
   LET v_referencia         = 0

   LET v_mensaje = "Inicia procesamiento de registros para desmarca";
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_sql=  " SELECT n_referencia           ",
               " FROM   sfr_marca_activa       ",
               " WHERE  id_derechohabiente = ? ",
               " AND    marca              = ? "
               
   PREPARE stm_marca FROM v_sql
   DECLARE cur_marca_des CURSOR FOR stm_marca
  
   -- Se itera el resultado

   DECLARE cur_desmarca CURSOR FOR SELECT * FROM tmp_desmarcas_saci2018_2
   FOREACH cur_desmarca INTO v_nss, v_marca 
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss  = v_nss
      IF v_id_derechohabiente IS NOT NULL AND v_id_derechohabiente <> 0 THEN 
         SELECT COUNT(*)
         INTO   v_contador
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    marca = v_marca
         IF v_contador = 0 THEN 
            CALL fn_inserta_rechazos_desmarca(v_nss,v_marca, "NSS NO SE ENCUENTRA MARCADO");
            LET v_mensaje = "    NSS no se encuentra marcado: ", v_nss, " con la marca: ", v_marca;
            DISPLAY v_mensaje
            CALL fn_inserta_mensaje(v_mensaje);
            LET v_sin_marca_previa = v_sin_marca_previa + 1
         ELSE 
            LET v_desmarcados = v_desmarcados + 1
            FOREACH cur_marca_des USING v_id_derechohabiente, v_marca INTO v_referencia
            -- se transfieren los datos al arreglo de despliegue
               EXECUTE prp_desmarca USING
                             v_id_derechohabiente
                            ,v_marca
                            ,v_referencia
                            ,'0'
                            ,'0'
                            ,'OPSISSACI'
                            ,''
                        INTO v_resultado;
               IF v_resultado <> 0 THEN 
                  CALL fn_inserta_rechazos_desmarca(v_nss, v_marca, "NSS CON PROBLEMAS EN LA DESMARCA");
                  LET v_mensaje = "    NSS con problemas en la desmarca:", v_nss, " marca: ",v_marca;
                  DISPLAY v_mensaje
                  CALL fn_inserta_mensaje(v_mensaje);
                  LET v_desmarcados = v_desmarcados -1
                  LET v_problema_desmarca = v_problema_desmarca + 1
               ELSE 
                  CALL fn_inserta_aceptados_desmarca(v_nss, v_marca, v_referencia);
               END IF 
            END FOREACH
           
         END IF 
      ELSE 
         CALL fn_inserta_rechazos_desmarca(v_nss, v_marca, "NSS INEXISTENTE");
         LET v_mensaje = "    NSS no existe: ", v_nss, " marca: ", v_marca;
         DISPLAY v_mensaje
         CALL fn_inserta_mensaje(v_mensaje);
         LET v_inexistentes = v_inexistentes + 1 
      END IF 

   END FOREACH 
   LET v_mensaje = "Termina de procesar el archivo de desmarcas";
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
