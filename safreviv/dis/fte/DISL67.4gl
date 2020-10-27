################################################################################
#Versión                    => 1.0.0                                           #
#Fecha ultima modificacion  => 26/09/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL67                                                   #
#Objetivo          => Programa lanzador para la carga del archivo de           #
#                     Cancelación Parcial de Avance de Pagos.                  #
#Fecha inicio      => 26/09/2018                                               #
################################################################################
DATABASE safre_viv

GLOBALS
DEFINE g_pid                 LIKE bat_ctr_proceso.pid,     --ID del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod, --Codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod, --Codigo de operacion
       g_sql_txt             STRING,
       v_proc_entra          SMALLINT,
       v_proc_val            SMALLINT,
       v_cod_conv            SMALLINT,
       v_desc_proc_val       CHAR(40),
       v_mensaje_val         STRING

DEFINE 
  arr_cancelacion            DYNAMIC ARRAY OF RECORD
    arr_nss                  LIKE afi_derechohabiente.nss,
    arr_nrp                  LIKE dis_det_avance_pago.nrp,
    arr_periodo              CHAR(4),
    arr_causal               SMALLINT 
    END RECORD  

END GLOBALS

MAIN
DEFINE v_usuario             VARCHAR(30),     --Almacena al usuario
       v_tipo_proceso        SMALLINT,        --Forma como ejecutara el programa 
       v_nom_prog            VARCHAR(30),     --Almacena opción del menú 
       v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,     --Ruta del ejecutable
       v_ruta_listados       LIKE seg_modulo.ruta_listados,--Rute del log
       r_bnd_carga           SMALLINT         --Bandera de carga de archivo

  -- se asignan los parametros que vienen del fglrun
  LET v_usuario      = ARG_VAL(1)
  LET v_tipo_proceso = ARG_VAL(2)
  LET v_nom_prog     = ARG_VAL(3)
  LET g_proceso_cod  = 939
  LET g_opera_cod    = 1

  -- se asigna el titulo del programa
  IF ( v_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(v_nom_prog)
  END IF

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", g_proceso_cod,
                  "AND    a.cod_proceso_valida = b.proceso_cod ",
                  "AND    a.cod_convivencia    = 0             ",
                  "ORDER BY cod_proceso_valida   "
  PREPARE ps_val_proc FROM g_sql_txt
  DECLARE cur_val_proc CURSOR FOR ps_val_proc
  FOREACH cur_val_proc INTO v_proc_entra,
                            v_proc_val,
                            v_desc_proc_val
    IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH
   
  --Se obtienen las variables para invocar el siguente proceso
  SELECT ruta_bin, ruta_listados
  INTO   v_ruta_ejecutable, v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  IF (fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
     CALL fn_carga_archivo(g_pid,
                           g_proceso_cod,
                           g_opera_cod,
                           2, --Tipo de carga: 1 es en línea y 2 es nohup
                           "DISL67", --Programa
                           "", --Programa lanzado
                           v_usuario, 
                           TRUE) --Con TRUE la carga inicializa el proceso
     RETURNING r_bnd_carga
         
     IF r_bnd_carga = FALSE THEN
        CALL fn_mensaje("Atención","Carga Cancelada","about")
     END IF
  ELSE
     CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid,g_proceso_cod,
                                                       g_opera_cod))
  END IF
END MAIN