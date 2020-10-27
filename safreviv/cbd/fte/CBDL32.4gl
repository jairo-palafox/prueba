################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDL32                                                   #
#Objetivo          => Lanzador se la preliquidación para el ajuste operativo   #
#                     por Conciliación                                         #
#Fecha inicio      => 21/10/2014                                               #
################################################################################
DATABASE safre_viv

PRIVATE DEFINE v_tipo_proceso       SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE v_nom_prog           VARCHAR(30) -- Almacena opción del menú 
PRIVATE DEFINE v_usuario            VARCHAR(30) -- Almacena al usuario

PRIVATE DEFINE v_folio              DECIMAL(9,0)
PRIVATE DEFINE v_nom_archivo        VARCHAR(50)

#Arreglo dinamico para el manejo de los saldos
PRIVATE DEFINE v_cifras DYNAMIC ARRAY OF RECORD
   subcuenta                        STRING,
   acciones                         DECIMAL(26,6),
   registros                        INTEGER
END RECORD

MAIN
   -- se asignan los parametros que vienen del fglrun
   LET v_usuario       = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   
   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   CALL fn_buscar_saldos()

   OPEN WINDOW vtn_cbdl321 WITH FORM "CBDL321"
      DIALOG   ATTRIBUTES(UNBUFFERED)
         DISPLAY ARRAY v_cifras TO cifras.* 
         END DISPLAY

         BEFORE DIALOG
            DISPLAY  v_folio TO folio
            DISPLAY  v_nom_archivo TO nom_archivo

         ON ACTION preliquidar
            CALL fn_preliquidar()
            EXIT DIALOG
            
         ON ACTION cancelar
            #CALL fn_mensaje ("Ajuste operativo", "Se canceló el proceso de ajuste operativo" , "info")
            EXIT DIALOG
      END DIALOG
   CLOSE WINDOW vtn_cbdl321
END MAIN

PRIVATE FUNCTION fn_buscar_saldos()
   DEFINE v_consulta_saldo       STRING

   DEFINE v_subcuenta                     SMALLINT
   DEFINE v_subcuenta_desc                VARCHAR(50)
   DEFINE v_acciones                      DECIMAL(26,6)
   DEFINE v_registros                     INTEGER
   DEFINE i                               SMALLINT

   #Primero se busca el folio del archivo integrado
   SELECT
      cza.folio,
      arc.nombre_archivo
   INTO
      v_folio,
      v_nom_archivo
   FROM cbd_ctr_ajuste_operativo cza
   INNER JOIN glo_ctr_archivo arc ON arc.folio = cza.folio
   WHERE cza.estado = 2

   LET v_consulta_saldo =  "SELECT ",
                              "det.subcuenta, ",
                              "(det.subcuenta || ' - ' || cat.subcuenta_desc) subcuenta, ",
                              "det.total_acciones, ",
                              "det.total_registros ",
                           "FROM cbd_cifras_ajuste_operaivo det ",
                           "INNER JOIN cat_subcuenta cat on cat.subcuenta = det.subcuenta ",
                           "WHERE det.folio = ? ",
                           "ORDER BY det.subcuenta"
   PREPARE exe_consulta_saldo FROM v_consulta_saldo
   DECLARE cur_consulta_saldo CURSOR FOR exe_consulta_saldo
   
   #Se genera la informacion para la seccion de saldos
   LET i = 1
   FOREACH cur_consulta_saldo USING v_folio INTO v_subcuenta, v_subcuenta_desc, v_acciones, v_registros
      IF v_subcuenta IS NOT NULL THEN
         LET v_cifras[i].subcuenta = v_subcuenta_desc
         LET v_cifras[i].acciones = v_acciones
         LET v_cifras[i].registros = v_registros
         LET i = i + 1
      END IF
   END FOREACH
END FUNCTION

PRIVATE FUNCTION fn_preliquidar()
   DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
   DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados -- Rute del log

   DEFINE r_resultado_opera   INTEGER
   DEFINE v_comando           STRING
   
   LET v_proceso_cod = 2111
   LET v_opera_cod = 3

   # se busca el pid para el proceso
   SELECT pid INTO v_pid FROM bat_ctr_proceso where folio = v_folio

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(v_pid,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      --Obtiene las rutas ejecutable
      SELECT ruta_bin
        INTO v_ruta_ejecutable
      FROM seg_modulo 
      WHERE modulo_cod = 'cbd'

      --Obtiene ruta listados
      SELECT ruta_listados
        INTO v_ruta_listados
      FROM seg_modulo 
      WHERE modulo_cod = 'bat'

      # Inicia operación
      CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,v_folio,"CBDP32",
                         v_nom_archivo,v_usuario) RETURNING r_resultado_opera

      LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDP32 ",
                                       v_usuario," ",
                                       v_pid," ",
                                       v_proceso_cod," ",
                                       v_opera_cod," ",
                                       v_folio," '",
                                       v_nom_archivo,
                                       "' 1>", v_ruta_listados CLIPPED ,
                                       "/nohup:",v_pid USING "&&&&&",":",
                                                v_proceso_cod USING "&&&&&",":",
                                                v_opera_cod USING "&&&&&" ," 2>&1 &"

      #DISPLAY v_comando                        
      RUN v_comando
      IF(STATUS)THEN
         CALL fn_mensaje("Preliquidacion", 
                         "Ocurrió un error al iniciar el proceso batch",
                         "bn_about")
      ELSE
         # Se indica que se realizo el proceso de carga
         CALL fn_mensaje("Preliquidacion", 
                         "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid,
                         "bn_about")
      END IF
      
   END IF
END FUNCTION