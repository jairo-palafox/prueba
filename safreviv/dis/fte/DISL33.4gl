################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 19/03/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL33                                                   #
#Objetivo          => Programa lanzador para integrar archivos de la operaci�n #
#                     923 - Dispersi�n por NSS                                 #
#Fecha inicio      => 19/03/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod  --codigo de operacion
END GLOBALS

MAIN
  DEFINE 
    v_usuario                VARCHAR(30), --Almacena al usuario
    v_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa 
    v_nom_prog               VARCHAR(30), --Almacena opci�n del men� 
    v_ruta_ejecutable        LIKE seg_modulo.ruta_bin,     --Ruta del ejecutable
    v_ruta_listados          LIKE seg_modulo.ruta_listados,--Rute del log
    r_bnd_carga              SMALLINT     --Bandera de carga de archivo    

   CALL STARTLOG("DISL33.log")
   
  -- se asignan los parametros que vienen del fglrun
  LET v_usuario      = ARG_VAL(1)
  LET v_tipo_proceso = ARG_VAL(2)
  LET v_nom_prog     = ARG_VAL(3)
  LET g_proceso_cod  = 923
  LET g_opera_cod    = 1

  -- se asigna el titulo del programa
  IF ( v_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(v_nom_prog)
  END IF  

  --Se obtienen las variables para invocar el siguente proceso
  SELECT ruta_bin 
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  SELECT ruta_listados 
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'
   
  IF (fn_valida_operacion(g_pid, g_proceso_cod, g_opera_cod) = 0 ) THEN
     CALL fn_carga_archivo(g_pid,
                           g_proceso_cod,
                           g_opera_cod,
                           2,
                           "DISL33",
                           "",
                           v_usuario,
                           TRUE)
     RETURNING r_bnd_carga

     IF r_bnd_carga = FALSE THEN
        CALL fn_mensaje("Atenci�n", "Carga Cancelada", "about")
     END IF
  ELSE
    CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid,
                                                      g_proceso_cod,
                                                      g_opera_cod))
  END IF
END MAIN
