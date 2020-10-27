################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 03/06/2019                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISE17                                                        #
#Objetivo     => Elemento lanzado por DISL32, liquidación de créditos vigentes.#
#                (Sin Adelanto).                                               #
#Fecha inicio => 20/02/2015                                                    #
################################################################################
DATABASE safre_viv
  DEFINE p_usuario           CHAR(20)
  DEFINE p_pid               DECIMAL(9,0)
  DEFINE p_proceso_cod       SMALLINT
  DEFINE p_opera_cod         SMALLINT
  DEFINE p_folio_pag         DECIMAL(9,0)
  DEFINE p_proceso_cod_pag   SMALLINT

MAIN
  DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin --Ruta del bin del módulo
  DEFINE r_ruta_listados     LIKE seg_modulo.ruta_listados --Ruta de listados del módulo
  DEFINE v_s_qry             STRING
  DEFINE v_folio_liquida_dis DECIMAL(9,0)

  DEFINE v_bandera           SMALLINT
  DEFINE v_cadena            STRING
  DEFINE p_titulo            STRING --Titulo del mensaje enviado en el correo
  DEFINE p_mensaje           STRING --Cuerpo del mensaje enviado

  DEFINE v_proceso_desc      LIKE cat_proceso.proceso_desc
  DEFINE v_extension         LIKE cat_operacion.extension
  DEFINE v_opera_desc        LIKE cat_operacion.opera_desc
  DEFINE v_layout            LIKE cat_operacion.layout_cod
  DEFINE v_ruta_rescate      STRING
  DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados
  DEFINE v_usuario           LIKE seg_modulo.usuario

  DEFINE v_fecha_01mm        CHAR(11)
  DEFINE p_programa_cod      VARCHAR(10)

  DEFINE 
    r_bnd_opera_err          SMALLINT,
    r_bnd_preliquidacion     SMALLINT,
    v_status_err             SMALLINT,
    v_desc_err               VARCHAR(200),
    r_id_derechohabinete     DECIMAL(9,0),
    v_QryTxt                 STRING,
    bnd_continuar            SMALLINT,
    r_bandera                SMALLINT,
    r_ruta_reporte           STRING,
    s_ruta_listados          STRING,
    s_origen_datos           STRING

  --Se recuperan los parametros
  LET v_folio_liquida_dis = ARG_VAL(1) --Folio de liquidación
  LET p_folio_pag         = ARG_VAL(2) --Folio de pagos
  LET p_proceso_cod_pag   = ARG_VAL(3) --Código de operación
  LET p_usuario           = ARG_VAL(4) --Usuario que ejecuta el proceso

  LET p_proceso_cod = 922 --Código del Proceso de Dispersión Créditos Vigentes
  LET p_opera_cod   = 1   --Código de la Operación de Liquidación

  CALL fn_max_pid(p_proceso_cod, p_opera_cod) RETURNING p_pid
  
  --Se inicia el log del proceso
  CALL STARTLOG(p_usuario CLIPPED|| ".DISE17.log")

  LET v_fecha_01mm =  MONTH(TODAY) USING "&&"||"/"||"01"||"/"||YEAR(TODAY)

  IF p_usuario IS NULL THEN
     LET p_usuario = "infonavit"
  END IF

  --Borra y crea tablas dis_preliquida, dis_interface_hs y dis_interface_ef_ad
  CALL fn_borra_crea_tablas()

  --Ejecuta funcción para la preliquidación de dispersión
  WHENEVER ERROR CONTINUE

  PREPARE prp_fn_transaccion
  FROM    "EXECUTE FUNCTION fn_dis_transaccion12(?,?,?,?,?,?,?,?)"
  EXECUTE prp_fn_transaccion INTO r_bnd_preliquidacion, v_status_err, v_desc_err, r_id_derechohabinete
                             USING v_fecha_01mm,
                                   p_usuario,
                                   v_folio_liquida_dis,
                                   p_pid,
                                   p_proceso_cod,
                                   p_opera_cod,
                                   p_folio_pag,
                                   p_proceso_cod_pag
  WHENEVER ERROR STOP

  DISPLAY "Función Transaccion ", r_bnd_preliquidacion
  DISPLAY "Código:", v_status_err
  DISPLAY "Id_derechohabiente", r_id_derechohabinete
  DISPLAY "mensaje", v_desc_err

  IF r_bnd_preliquidacion <> 0 THEN
     -- Función para finalizar la operación en error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
     RETURNING r_bnd_opera_err

     DISPLAY "Error en la transacción ", v_status_err," ", v_desc_err
     EXIT PROGRAM
  END IF

  --PRELIQUIDACION 
  IF r_bnd_preliquidacion = 0 THEN
      --Termina proceso de preliquidación pero no ejecuto datos
      CALL fn_valida_preliquidacion(p_pid, p_proceso_cod, p_opera_cod)

      --Actualiza el status del folio de dispersión a Liquidado
      LET v_QryTxt = " UPDATE glo_folio",
                     " SET status      = 2",
                     " WHERE folio     = ", v_folio_liquida_dis,
                     " AND proceso_cod = ", p_proceso_cod,
                     " AND opera_cod   = 1"

      PREPARE prp_actualiza_folio_prliquidado FROM v_QryTxt
      EXECUTE prp_actualiza_folio_prliquidado

      --Se obtiene el código de programa
      SELECT programa_cod
      INTO   p_programa_cod
      FROM   cat_operacion
      WHERE  proceso_cod = p_proceso_cod
      AND    opera_cod   = p_opera_cod

      CALL fn_reporte_liquidacion(v_folio_liquida_dis, 
                                  "cta_movimiento", 
                                  p_usuario, 
                                  p_pid,
                                  p_proceso_cod, 
                                  p_opera_cod, 
                                  p_programa_cod,
                                  FALSE)

      LET bnd_continuar = 1
  ELSE
      DISPLAY "Error en la liquidación ",r_bnd_preliquidacion
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
      RETURNING r_bnd_opera_err
      EXIT PROGRAM
  END IF

  CALL fn_rutas("dis") 
  RETURNING r_c_ruta_bin, r_ruta_listados
   
  IF bnd_continuar THEN
     --CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
     --RETURNING r_bandera

     --IF r_bandera = 0 THEN

        DISPLAY ""
        DISPLAY "El registro contable se realizó exitosamente. "
        DISPLAY ""

        LET s_origen_datos = v_usuario

        LET r_ruta_reporte = s_ruta_listados.trim(),
                             "/",
                             s_origen_datos.trim(), "-",
                             "DISE17", "-",
                             p_pid USING "&&&&&","-",
                             p_proceso_cod USING "&&&&&","-",
                             p_opera_cod USING "&&&&&",".pdf"

        --Envío de correo de notificación de proceso finalizado
        CALL fn_correo_proceso(p_pid,
                               p_proceso_cod,
                               p_opera_cod,
                               r_ruta_reporte CLIPPED,
                               "Liquidación de Dispersión Créditos Vigentes",
                               "\n ID Proceso   : "||p_pid||
                               "\n Proceso      : Dispersión de Pagos"||
                               "\n Operacion    : Liquidación de dispersión créditos vigentes"||
                               "\n Fecha Inicio : "||TODAY||
                               "\n Fecha Fin    : "||TODAY)
     --END IF
  END IF

  DISPLAY "\n### Generación de Interfaces ###\n"

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
                 " INICIO ETAPA       : GENERACIÓN INTERFACES",
                 " FECHA              : ", TODAY, "\n",
                 " HORA               : ", TIME(CURRENT),"\n \n \n"
  DISPLAY v_cadena

  --Generar el archivo o interface de Pago REAL HS
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS01.42r ", v_folio_liquida_dis, " ",p_usuario
  RUN v_s_qry

  --Generar el archivo o interface a las Entidades Financieras y/o Servicios
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS03.42r ", v_folio_liquida_dis, " ",p_usuario
  RUN v_s_qry

  --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS04.42r ", v_folio_liquida_dis, " ", 901, " ",p_usuario
  RUN v_s_qry

  --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS)
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS05.42r ", v_folio_liquida_dis, " ",p_usuario
  RUN v_s_qry

  --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS) <=$2
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS08.42r ", v_folio_liquida_dis, " ", 901
  RUN v_s_qry

  --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS) <=$2
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS09.42r ", v_folio_liquida_dis
  RUN v_s_qry

  DISPLAY "\n### Generación de Reporte de Inconsistencias ###"

  --Generar el reporte de inconsistencias
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS07.42r ", p_usuario," ",
                                                                p_pid," ",
                                                                p_proceso_cod, " ",
                                                                p_opera_cod, " ",
                                                                v_folio_liquida_dis
  RUN v_s_qry
  
  DISPLAY "\n################################################"
  
  CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod) 
  RETURNING v_bandera

  DISPLAY "Regresa de opera fin: ", v_bandera

  IF ( v_bandera = 0 ) THEN
     LET v_cadena = " FIN ETAPA          : GENERACIÓN INTERFACES",
                    " FECHA              : ",TODAY,"\n",
                    " HORA               : ",TIME(CURRENT),"\n"
  ELSE
     #Imprime en pantalla el error, por el cual no se finalizó la operación
     CALL fn_desplega_inc_operacion(v_bandera)
             
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
     RETURNING v_bandera

     #Cadena para imprimir en LOG
     LET v_cadena = " --- ERROR ---\n",
                    " El proceso de generación de interfaces no pudo finalizar correctamente.\n",
                    " Código de error: ", v_bandera,"\n ",
                    " FECHA          : ",TODAY,"\n",
                    " HORA           : ",CURRENT HOUR TO SECOND,"\n"

     #Cadena para enviar por correo
     LET p_mensaje = " --- ERROR ---\n",
                     " El proceso de generación de interfaces no pudo finalizar correctamente.\n",
                     " Código de error: ", v_bandera,"\n ",
                     " FECHA          : ",TODAY,"\n",
                     " HORA           : ",CURRENT HOUR TO SECOND,"\n"

     --Se crea el título del mensaje
     LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - LIQUIDACION"

     --Se invoca el envío de correo electrónico de notificación
     CALL fn_correo_proceso(p_pid, 
                            p_proceso_cod, 
                            p_opera_cod,
                            NULL, --No lleva archivo adjunto
                            p_titulo,
                            p_mensaje)
  END IF

  --Se despliega la cadena con el mensaje de finalizacion
  DISPLAY v_cadena

END MAIN

#Objetivo: Borrar y crear sobre la base de datos las estructuras de las tablas
#          dis_preliquida, dis_interface_hs y dis_interface_ef_ad
FUNCTION fn_borra_crea_tablas()
  WHENEVER ERROR CONTINUE;
    DROP TABLE IF EXISTS dis_preliquida;
    DROP TABLE IF EXISTS dis_pre_his_trans;
    DROP TABLE IF EXISTS dis_pre_interface_hs;
    
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_info_inconsistente DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_interface_ef_ad DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_compensa_avance DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_crd_tramite DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_crd_ceros DISABLED;"
  WHENEVER ERROR STOP;
END FUNCTION

#Objetivo:Al termino del proceso de preliquidación de dispersión, valida si
#         en contro información relacionada en las tablas de pag_det_trabajador
#         con el folio de registro de pagos, derechohabiente y numero de credito
#         de cta_credito
FUNCTION fn_valida_preliquidacion(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
DEFINE
  v_bnd_preliquidacion       DECIMAL(10,0),
  v_bnd_eror_opera           SMALLINT

DEFINE
  v_p_pid                    LIKE bat_ctr_proceso.pid,
  v_p_proceso_cod            LIKE cat_proceso.proceso_cod,
  v_p_opera_cod              LIKE cat_operacion.opera_cod

  LET v_bnd_preliquidacion = 0

  SELECT COUNT(*)
  INTO   v_bnd_preliquidacion
  FROM   dis_preliquida
  IF v_bnd_preliquidacion <= 0 THEN
     DISPLAY "No se encontraron registros a liquidar con el folio de registro de pagos seleccionado"

     CALL fn_error_opera(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
     RETURNING v_bnd_eror_opera

     EXIT PROGRAM
  END IF

  SELECT COUNT(*)
  INTO   v_bnd_preliquidacion
  FROM   dis_preliquida
  IF v_bnd_preliquidacion <= 0 THEN
     DISPLAY "No se encontraron registros a liquidar con el folio de registro de pagos seleccionado"

     CALL fn_error_opera(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
     RETURNING v_bnd_eror_opera

     EXIT PROGRAM
  ELSE
     DISPLAY "Se Liquidaron ", v_bnd_preliquidacion," registros"
  END IF

END FUNCTION