################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 26/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL02                                                   #
#Objetivo          => Programa lanzador para integrar archivos                 #
#Fecha inicio      => 18/01/2012                                               #
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

END GLOBALS

MAIN
DEFINE v_usurio              VARCHAR(30), --Almacena al usuario
       v_tipo_proceso        SMALLINT,    --Forma como ejecutara el programa 
       v_nom_prog            VARCHAR(30), --Almacena opción del menú 
       v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,     --Ruta del ejecutable
       v_ruta_listados       LIKE seg_modulo.ruta_listados,--Rute del log
       v_comando             STRING,
       v_sql                 STRING,  --Cadena con una instruccion SQL
       r_bandera             SMALLINT,
       r_bnd_carga           SMALLINT --Bandera de carga de archivo

  -- se asignan los parametros que vienen del fglrun
  LET v_usurio       = ARG_VAL(1)
  LET v_tipo_proceso = ARG_VAL(2)
  LET v_nom_prog     = ARG_VAL(3)
  LET g_proceso_cod  = 902
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

  --OPEN WINDOW w_disl02 WITH FORM "DISL031"
   
  --Se obtienen las variables para invocar el siguente proceso
  SELECT ruta_bin, ruta_listados
  INTO   v_ruta_ejecutable, v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  IF (fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
     {-- se obtiene el PID del proceso
     LET v_sql = "EXECUTE FUNCTION fn_genera_pid(?,?,?)"
     PREPARE sid_obtpid FROM v_sql
     EXECUTE sid_obtpid USING g_proceso_cod, g_opera_cod, v_usurio
                         INTO g_pid}

     -- se invoca la funcion que inicializa el proceso
     {LET v_sql = " EXECUTE FUNCTION fn_inicializa_proceso(?,?,?,?,?,?,?)"
     PREPARE prp_inicia_proceso FROM v_sql
     EXECUTE prp_inicia_proceso USING g_pid,g_proceso_cod,g_opera_cod,'0',
                                      "DISL02","",v_usurio INTO r_bandera}
     --IF r_bandera = 0 THEN
        CALL fn_carga_archivo(g_pid,
                              g_proceso_cod,
                              g_opera_cod,
                              2,
                              "DISL02",
                              "",v_usurio,
                              TRUE) --Con TRUE la carga inicializa ep proceso
                              RETURNING r_bnd_carga
        IF r_bnd_carga = FALSE THEN
           CALL fn_mensaje("Atención","Carga Cancelada","about")
        END IF
     --END IF
  ELSE
    CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid,g_proceso_cod,
                                                      g_opera_cod))
  END IF
END MAIN