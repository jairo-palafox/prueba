################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  =>  04/07/2017                                     #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISP111                                                       #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la       #
#                liquidacion para dispersion                                   #
#Fecha inicio => 04/07/2017                                                    #
################################################################################
--------------------------------------------------------------------------------
-- Modificado   => GERARDO ALFONSO VEGA PAREDES
-- Fecha        => 1-AGO-2017
-- Descripción  => Agregar liquidación, registro contable e interfaces
-- Clave cambio => --PROINFXVII-17-GVP 
--------------------------------------------------------------------------------
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    p_folio                  DECIMAL(9,0),
    p_usuario_cod            LIKE seg_usuario.usuario_cod  --clave usuario firmado
END GLOBALS

MAIN
DEFINE 
  p_s_titulo                 STRING, -- titulo de la ventana
  p_operacion                SMALLINT,
  v_ruta_ejecutable          LIKE seg_modulo.ruta_bin,
  v_ruta_listados            LIKE seg_modulo.ruta_listados,
  l_comando                  STRING,
  v_folio_liquida            LIKE dis_preliquida.folio_liquida

  --se recupera la clave de usuario desde parametro 
  --argumento con indice 1
  LET p_usuario_cod = ARG_VAL(1)
  LET p_s_titulo    = ARG_VAL(2)
  LET p_folio       = ARG_VAL(3)  --PROINFXVII-17-GVP

  --Obtiene las rutas de modulo a ejecutar
  SELECT ruta_bin, ruta_listados
  INTO   v_ruta_ejecutable, v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  --si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  CALL STARTLOG (p_usuario_cod CLIPPED|| ".DISP111.log")

  LET g_proceso_cod = 901 -- dispersion
  LET g_opera_cod   = 2   -- liquidacion
  LET p_operacion   = 2   -- ejecutar liquidacion

  -- se invoca la funcion para enviar la liquidacion
  --PROINFXVII-17-GVP 
  INSERT INTO cta_movimiento
  SELECT * 
  FROM   dis_preliquida
  WHERE  folio_liquida = p_folio
   
  CALL Registro_contable()  --PROINFXVII-17-GVP
  CALL Genera_interfaces()  --PROINFXVII-17-GVP

  DISPLAY "Se ha liquidado correctamente."
 
END MAIN

FUNCTION Registro_contable()  --PROINFXVII-17-GVP
  DEFINE g_proceso_cod       LIKE cat_proceso.proceso_cod --codigo de proceso
  DEFINE v_folio_liquida_dis DECIMAL(9,0)

  DEFINE r_bnd_proceso_cnt   SMALLINT,
         p_cve_proceso_cnt   SMALLINT,
         p_transaccion       SMALLINT,
         p_transaccion_83    SMALLINT,
         v_fecha_reg         DATE

  LET v_folio_liquida_dis = p_folio
  LET g_proceso_cod       = 901     --Codigo del Proceso de Dispersion (4)(901)
  LET p_cve_proceso_cnt   = 19      --Cierre avance de pagos
  LET p_transaccion       = 0       --Se deberá registrar el monto del cierre de ava
  LET p_transaccion_83    = 83      
  LET r_bnd_proceso_cnt   = 0       --Bandera del stored del registro de avance de pagos
  LET v_fecha_reg         = TODAY

  WHENEVER ERROR CONTINUE
    PREPARE prep_fn_dis_cnt19
    FROM "EXECUTE PROCEDURE fn_dis_cnt19(?,?,?,?,?)"
    EXECUTE prep_fn_dis_cnt19 USING v_folio_liquida_dis,
                                    v_fecha_reg,
                                    p_cve_proceso_cnt,
                                    g_proceso_cod,
                                    p_transaccion
                               INTO r_bnd_proceso_cnt

     PREPARE prep_fn_dis_cnt19_83
     FROM "EXECUTE PROCEDURE fn_dis_cnt19(?,?,?,?,?)"
     EXECUTE prep_fn_dis_cnt19_83 USING v_folio_liquida_dis,
                                        v_fecha_reg,
                                        p_cve_proceso_cnt,
                                        g_proceso_cod,
                                        p_transaccion_83
                                  INTO  r_bnd_proceso_cnt

  WHENEVER ERROR STOP

  PREPARE prep_fn_revisa_reg_cnt
  FROM "EXECUTE PROCEDURE fn_revisa_reg_cnt(?,?,?)"
  EXECUTE prep_fn_revisa_reg_cnt USING v_folio_liquida_dis,
                                       v_fecha_reg,
                                       p_transaccion
                                 INTO  r_bnd_proceso_cnt

  IF r_bnd_proceso_cnt = 0 THEN
     DISPLAY "El registro contable se realizó exitosamente."
  ELSE
     IF r_bnd_proceso_cnt = 1 THEN
        DISPLAY "Error: No existe monto de abono en el registro contable."
     END IF
     IF r_bnd_proceso_cnt = 2 THEN
        DISPLAY "Error: No existe monto de cargo en el registro contable."
     END IF
     IF r_bnd_proceso_cnt = 3 THEN
        DISPLAY "Error: Diferencia de montos abono - cargo."
     END IF
  END IF

END FUNCTION

FUNCTION Genera_interfaces()  --PROINFXVII-17-GVP
  DEFINE v_folio_liquida_dis DECIMAL(9,0)
  DEFINE p_pid               DECIMAL(9,0)
  DEFINE p_proceso_cod       SMALLINT
  DEFINE p_opera_cod         SMALLINT
  DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin --Ruta del bin del módulo
  DEFINE r_ruta_listados     LIKE seg_modulo.ruta_listados --Ruta de listados del módulo
  DEFINE v_proceso_desc      LIKE cat_proceso.proceso_desc
  DEFINE v_extension         LIKE cat_operacion.extension
  DEFINE v_opera_desc        LIKE cat_operacion.opera_desc
  DEFINE v_layout            LIKE cat_operacion.layout_cod
  DEFINE v_ruta_rescate      STRING
  DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados
  DEFINE v_usuario           LIKE seg_modulo.usuario
  DEFINE v_cadena            STRING
  DEFINE v_s_qry             STRING
  DEFINE v_query             STRING

  LET v_folio_liquida_dis = p_folio
  LET p_proceso_cod       = 901 --Código del Proceso de Dispersión de Pagos
  LET p_opera_cod         = 2   --Código de la Operación de Liquidación
   
  CALL fn_max_pid(p_proceso_cod, p_opera_cod) 
  RETURNING p_pid
   
  CALL fn_rutas("dis")
  RETURNING r_c_ruta_bin, r_ruta_listados
   
  DISPLAY ""
  DISPLAY "Folio Dispersión: ", v_folio_liquida_dis
  DISPLAY ""
   
  CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod)
  RETURNING v_proceso_desc,
            v_extension,
            v_opera_desc,
            v_layout,
            v_ruta_rescate,
            v_ruta_listados,
            v_usuario
   
  --Se despliega el inicio de la etapa
  LET v_cadena = " PROCESO            : ", v_proceso_desc, "\n",
                 " OPERACIÓN          : ", v_opera_desc, "\n",
                 " FOLIO              : ", v_folio_liquida_dis, "\n",
                 " FECHA              : ", TODAY, "\n",
                 " HORA               : ", TIME(CURRENT), "\n \n \n",
                 " INICIO ETAPA       : GENERACIÓN INTERFACES", "\n",
                 " FECHA              : ", TODAY, "\n",
                 " HORA               : ", TIME(CURRENT),"\n \n \n"
  DISPLAY v_cadena
   
  --Generar el archivo o interface de Pago REAL HS
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS01.42r ", v_folio_liquida_dis
  RUN v_s_qry
   
  --Generar el archivo o interface a las Entidades Financieras y/o Servicios
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS03.42r ", v_folio_liquida_dis
  RUN v_s_qry
   
  --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS04.42r ", v_folio_liquida_dis, " ", 901
  RUN v_s_qry
  
  --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS)
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS05.42r ", v_folio_liquida_dis
  RUN v_s_qry
   
  --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS) <=$2
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS08.42r ", v_folio_liquida_dis, " ", 901
  RUN v_s_qry
   
  --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS) <=$2
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS09.42r ", v_folio_liquida_dis
  RUN v_s_qry
  
  DISPLAY "\n### Generación de Reporte de Inconsistencias ###"
   
  --Generar el reporte de inconsistencias
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS07.42r ", p_usuario_cod," ",
                                                                p_pid," ",
                                                                p_proceso_cod, " ",
                                                                p_opera_cod, " ",
                                                                v_folio_liquida_dis
  RUN v_s_qry

  LET v_query = " UPDATE glo_folio",
                " SET    status      = 2",
                " WHERE  folio       = ", p_folio,
                " AND    proceso_cod = ", g_proceso_cod,
                " AND   opera_cod    = 1"
  PREPARE ps_actualiza_folio_liquidado FROM v_query
  EXECUTE ps_actualiza_folio_liquidado

END FUNCTION