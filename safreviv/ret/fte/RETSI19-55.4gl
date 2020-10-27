#############################################################################
#Módulo          => RET                                                     #        
#Programa        => RETSI19-55                                              #
#Objetivo        => Programa para carde de historicos del Fono de Ahorro    #
#                   Ejecución por única ocasión                             #
#Fecha Inicio    => Junio 24, 2019                                          #
#############################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"


GLOBALS 
DEFINE g_proceso        SMALLINT
DEFINE g_operacion      SMALLINT
DEFINE g_pid            INTEGER
DEFINE g_folio          DECIMAL(9,0)

       
END GLOBALS 
#Objetivo: Cargar la información historica de movimientos del Fondo de Ahorro para la generación de las cartas de Negativa
MAIN
--- Las actividades que realiza el programa son:
--- 1. Carga de lo archivo histórico a tablas temporales
--- 2. Integración de la información a la tabla definitiva
--- 3. Descarga las cifras de control a archivo plano
DEFINE v_archivo        STRING
DEFINE v_resultado      SMALLINT


DEFINE v_mensaje        CHAR(100)


   CALL fn_crea_temporales()
   LET v_archivo = ARG_VAL(1)
   LET v_mensaje = "Inicia Proceso: ", TODAY USING "dd/mm/yyyy", " - ", CURRENT HOUR TO SECOND
   CALL fn_inserta_mensaje(v_mensaje); 
   LET v_mensaje = "Archivo a Procesar: ", v_archivo
   CALL fn_inserta_mensaje(v_mensaje); 

   LET v_resultado = fn_valida_operacion(0,g_proceso,g_operacion)
   IF v_resultado = 0 THEN 
      CALL fn_genera_pid(g_proceso, g_operacion, "OPSISSACI") RETURNING g_pid
      CALL fn_genera_folio(g_proceso, g_operacion,"OPSISSACI") RETURNING g_folio
      CALL fn_inicializa_proceso(g_pid            ,
                                 g_proceso        ,
                                 g_operacion      ,
                                 g_folio          ,
                                 "RETSI19-55"     ,
                                 v_archivo        ,
                                 'OPSISSACI')  RETURNING v_resultado
                                  
      -- el proceso se registro correctamente
      IF ( v_resultado = 0 ) THEN
         CALL fn_actualiza_opera_ini(g_pid,g_proceso,g_operacion,g_folio,"RETSI19-55",v_archivo,"OPSISSACI") RETURNING v_resultado
         CALL fn_carga_archivos(v_archivo)          
         CALL fn_integra_historico()
         CALL fn_actualiza_opera_fin(g_pid,g_proceso,g_operacion) RETURNING v_resultado
         --Vacia la tabla de mensajes 
         LET v_archivo = "/safreviv_lst/bat/finnohup:", g_pid USING "&&&&&",":",g_proceso USING "&&&&&",":", g_operacion USING "&&&&&" 
         UNLOAD TO v_archivo SELECT * FROM tmp_mensajes

      ELSE
         -- no se puede ejecutar la operacion
         CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
         CALL fn_inserta_mensaje(v_mensaje); 
         DISPLAY "No se puedo inicializar la operación", v_mensaje
      END IF 
   ELSE
      -- no se puede ejecutar la operacion
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_inserta_mensaje(v_mensaje); 
      DISPLAY "No se puede ejecutar la operación", v_mensaje
   END IF
   

END MAIN

###################################################################################################################
## funcion para insertar mensajes en tabla temporal para reporte de cifras control
###################################################################################################################
FUNCTION fn_inserta_mensaje(p_mensaje)
DEFINE p_mensaje      CHAR(100) 

   INSERT INTO tmp_mensajes VALUES (CURRENT YEAR TO FRACTION(3), p_mensaje);

END FUNCTION 

###################################################################################################################
## funcion para crear las tablas temporales
###################################################################################################################
FUNCTION fn_crea_temporales()

   DROP TABLE IF EXISTS tmp_historico_fa;
   CREATE TEMP TABLE tmp_historico_fa(
                     cadena          CHAR(161)
--                     nss             CHAR(11),
--                     rfc             CHAR(13),
--                     nombre          CHAR(40),
--                     ejercicio       CHAR( 4),
--                     tipo_mov        CHAR( 2),
--                     empresa         CHAR(40),
--                     bimestres       CHAR( 5),
--                     importe         CHAR(10),
--                     f_movimiento    CHAR( 8),
--                     desc_movimiento CHAR(17),
--                     cve_movimiento  CHAR(20)
);
   DROP TABLE IF EXISTS tmp_mensajes;
   CREATE TEMP TABLE tmp_mensajes (
          fecha_hora   DATETIME YEAR TO FRACTION(3),
          mensaje      CHAR(100));
   LET g_proceso   = g_proceso_cod_historico_fondo_ahorro
   LET g_operacion = 1
   LET g_pid       = 0
   LET g_folio     = 0
   
END FUNCTION 

###################################################################################################################
## funcion para cargar los archivos a tablas temporales
###################################################################################################################
FUNCTION fn_carga_archivos(p_archivo)
DEFINE v_archivo      CHAR(60)
DEFINE p_archivo      STRING
DEFINE v_archivo_des  CHAR(60)
DEFINE v_contador     INTEGER 
DEFINE v_mensaje      CHAR(100)

   LET v_contador = 0
   LET v_mensaje = "Cargando archivo de movimientos historicos"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   LET v_archivo = "/safreviv_req/SACI2019-55/", p_archivo
   
   LOAD FROM v_archivo INSERT INTO tmp_historico_fa;

   SELECT COUNT(*)
   INTO   v_contador
   FROM   tmp_historico_fa

   LET v_mensaje = "    Se cargaron: ", v_contador, " del archivo "
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 

###################################################################################################################
## funcion para procesar los registros de marca
###################################################################################################################
FUNCTION fn_integra_historico()
DEFINE v_contador           INTEGER 
DEFINE v_mensaje            CHAR(100)
DEFINE v_tmp_contenido      CHAR(161)
DEFINE v_rec_historico      RECORD LIKE cta_his_fondo72.*
DEFINE v_rec_hist_comp      RECORD LIKE cta_his_fondo72_complemento.*
DEFINE v_f_paso             CHAR(8) 
DEFINE v_folio              DECIMAL(9,0)


   LET v_mensaje = "Integrando información "
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);

   -- Pendiente la creación del Folio
   LET v_folio = 0
   
   
   DECLARE cur_marca CURSOR FOR SELECT * FROM tmp_historico_fa
   FOREACH cur_marca INTO v_tmp_contenido 
      LET v_mensaje = ""
      INITIALIZE v_rec_historico TO NULL  
      IF LENGTH(v_tmp_contenido) > 0 THEN 
         
         LET v_rec_historico.nss             = v_tmp_contenido[1,11]
         LET v_rec_historico.rfc             = v_tmp_contenido[12,24]
         LET v_rec_historico.nombre          = v_tmp_contenido[25,64]
         LET v_rec_historico.ejercicio       = v_tmp_contenido[65,68]
         LET v_rec_historico.clave_mov       = v_tmp_contenido[69,70]
         LET v_rec_historico.empresa         = v_tmp_contenido[71,110]
         LET v_rec_historico.bimestres       = v_tmp_contenido[111,115]
         LET v_rec_historico.importe         = v_tmp_contenido[116,125]
         IF LENGTH(v_tmp_contenido) > 125 THEN 
            LET v_f_paso                        = v_tmp_contenido[126,133]
            LET v_rec_hist_comp.desc_movimiento = v_tmp_contenido[134,150]
            LET v_rec_hist_comp.cve_movimiento  = v_tmp_contenido[151,LENGTH(v_tmp_contenido)]
         ELSE 
            LET v_f_paso                        = NULL
            LET v_rec_hist_comp.desc_movimiento = NULL
            LET v_rec_hist_comp.cve_movimiento  = NULL
         END IF 

         SELECT seq_cta_his_fondo72.nextval
         INTO   v_rec_historico.id_cta_his_fondo72
         FROM   systables
         WHERE  tabid = 1
         IF v_f_paso IS NOT NULL THEN 
            LET v_rec_hist_comp.f_movimiento = MDY(v_f_paso[3,4],v_f_paso[1,2],v_f_paso[5,8])
            INSERT INTO cta_his_fondo72_complemento 
                 VALUES (v_rec_historico.id_cta_his_fondo72,v_rec_historico.empresa,v_rec_hist_comp.cve_movimiento, v_rec_hist_comp.f_movimiento);
         END IF 
         INSERT INTO cta_his_fondo72
              VALUES (v_rec_historico.id_cta_his_fondo72,
                      v_rec_historico.nss,
                      v_rec_historico.rfc,
                      v_rec_historico.nombre,
                      v_folio,
                      v_rec_historico.ejercicio,
                      v_rec_historico.clave_mov,
                      v_rec_historico.empresa,
                      v_rec_historico.bimestres,
                      v_rec_historico.importe,
                      0);

         LET v_contador = v_contador + 1
         IF v_contador MOD 1000 = 0 THEN 
            DISPLAY "Registros procesados : ", v_contador
         END IF 

      END IF 
   END FOREACH

   LET v_mensaje = "Termina la integración de los movimientos históricos, se cargaron : ", v_contador, " registros"
   DISPLAY v_mensaje
   CALL fn_inserta_mensaje(v_mensaje);
   
END FUNCTION 
 
