################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 27/05/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISS27                                                        #
#Objetivo     => Elemento lanzado por DISL22                                   #
#                                                                              #
#Fecha inicio => 27/05/2014                                                    #
################################################################################
DATABASE safre_viv
  DEFINE p_usuario           CHAR(20)
  DEFINE p_pid               DECIMAL(9,0)
  DEFINE p_proceso_cod       SMALLINT
  DEFINE p_opera_cod         SMALLINT
  DEFINE p_folio             DECIMAL(9,0)

MAIN
  DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
  DEFINE r_ruta_listados     LIKE seg_modulo.ruta_listados -- ruta de listados del módulo
  DEFINE v_s_qry             STRING
  DEFINE v_folio_liquida_dis DECIMAL(9,0)

  DEFINE v_bandera           SMALLINT
  DEFINE v_cadena            STRING
  DEFINE p_titulo            STRING -- titulo del mensaje enviado en el correo
  DEFINE p_mensaje           STRING -- cuerpo del mensaje enviado

  DEFINE v_proceso_desc      LIKE cat_proceso.proceso_desc
  DEFINE v_extension         LIKE cat_operacion.extension
  DEFINE v_opera_desc        LIKE cat_operacion.opera_desc
  DEFINE v_layout            LIKE cat_operacion.layout_cod
  DEFINE v_ruta_rescate      STRING
  DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados
  DEFINE v_usuario           LIKE seg_modulo.usuario

  -- se recuperan los parametros
  LET p_usuario           = ARG_VAL(1)
  LET p_pid               = ARG_VAL(2)
  LET p_proceso_cod       = ARG_VAL(3)
  LET p_opera_cod         = ARG_VAL(4)
  LET p_folio             = ARG_VAL(5)
  LET v_folio_liquida_dis = ARG_VAL(6)

  -- se inicia el log del proceso
  CALL STARTLOG(p_usuario CLIPPED|| ".DISS27.log")

  CALL fn_rutas("dis") 
  RETURNING r_c_ruta_bin, r_ruta_listados

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
            
  --se despliega el inicio de la etapa 
  LET v_cadena = " PROCESO            : ",v_proceso_desc,"\n",
                 " OPERACIÓN          : ",v_opera_desc,"\n",
                 " FOLIO              : ",p_folio,"\n",
                 " FECHA              : ",TODAY,"\n",
                 " HORA               : ",TIME(CURRENT),"\n \n \n",
                 " INICIO ETAPA       : GENERACIÓN INTERFACES",
                 " FECHA              : ",TODAY,"\n",
                 " HORA               : ",TIME(CURRENT),"\n \n \n"
  DISPLAY v_cadena

  --Generar el archivo o interface de Pago REAL HS
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS01.42r ", v_folio_liquida_dis
  RUN v_s_qry

  ----Generar el archivo o interface a las Entidades Financieras y/o Servicios
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
  LET v_s_qry = "fglrun ", r_c_ruta_bin CLIPPED,"/DISS07.42r ", p_usuario," ",
                                                                p_pid," ",
                                                                p_proceso_cod, " ",
                                                                p_opera_cod, " ",
                                                                v_folio_liquida_dis
  RUN v_s_qry
  
  DISPLAY "\n################################################"

  {--Generar el archivo o interface de Pago REAL HS
  LET v_s_qry = "fglrun ",r_c_ruta_bin CLIPPED,"/DISS11.42r ",v_folio_liquida_dis
  RUN v_s_qry

  --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
  LET v_s_qry = "fglrun ",r_c_ruta_bin CLIPPED,"/DISS14.42r ",v_folio_liquida_dis, " ", p_proceso_cod
  RUN v_s_qry

  --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS)
  LET v_s_qry = "fglrun ",r_c_ruta_bin CLIPPED,"/DISS15.42r ",v_folio_liquida_dis
  RUN v_s_qry

  --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS) <=$2
  LET v_s_qry = "fglrun ",r_c_ruta_bin CLIPPED,"/DISS18.42r ",v_folio_liquida_dis, " ", p_proceso_cod
  RUN v_s_qry

  --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS) <=$2
  LET v_s_qry = "fglrun ",r_c_ruta_bin CLIPPED,"/DISS19.42r ",v_folio_liquida_dis
  RUN v_s_qry
   
  ----Generar el archivo o interface a las Entidades Financieras y/o Servicios
  LET v_s_qry = "fglrun ",r_c_ruta_bin CLIPPED,"/DISS13.42r ",v_folio_liquida_dis
  RUN v_s_qry}
   
  CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod) 
  RETURNING v_bandera

  DISPLAY "Regresa de opera fin: ", v_bandera

  IF ( v_bandera = 0 ) THEN
     LET v_cadena = " FIN ETAPA          : GENERACIÓN INTERFACES",
                    " FECHA              : ",TODAY,"\n",
                    " HORA               : ",TIME(CURRENT),"\n"
  ELSE
     # Imprime en pantalla el error, por el cual no se finalizó la operación
     CALL fn_desplega_inc_operacion(v_bandera)
             
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
     RETURNING v_bandera

     # Cadena para imprimir en LOG
     LET v_cadena = " --- ERROR ---\n",
                    " El proceso de generación de interfaces no pudo finalizar correctamente.\n",
                    " Código de error: ", v_bandera,"\n ",
                    " FECHA          : ",TODAY,"\n",
                    " HORA           : ",CURRENT HOUR TO SECOND,"\n"

     # Cadena para enviar por correo
     LET p_mensaje = " --- ERROR ---\n",
                     " El proceso de generación de interfaces no pudo finalizar correctamente.\n",
                     " Código de error: ", v_bandera,"\n ",
                     " FECHA          : ",TODAY,"\n",
                     " HORA           : ",CURRENT HOUR TO SECOND,"\n"

     -- se crea el titulo del mensaje
     LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - LIQUIDACION"

     -- se invoca el envio de correo electronico de notificacion
     CALL fn_correo_proceso(p_pid, 
                            p_proceso_cod, 
                            p_opera_cod,
                            NULL, -- no lleva archivo adjunto
                            p_titulo,
                            p_mensaje)
  END IF

  -- se despliega la cadena con el mensaje de finalizacion
  DISPLAY v_cadena

END MAIN