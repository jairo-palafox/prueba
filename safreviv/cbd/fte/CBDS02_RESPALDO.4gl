#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDS02                                                                 #
#Objetivo     => Programa que genera el archivo de la BDNSVIV-plus                      #
#Fecha inicio => 19/06/2012                                                             #
#########################################################################################
DATABASE safre_viv

GLOBALS "CBDS02.inc"

##Parametros generales del proceso
PRIVATE DEFINE p_pid                      LIKE bat_ctr_operacion.pid                  -- PID del proceso
PRIVATE DEFINE p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod          -- codigo del proceso
PRIVATE DEFINE p_opera_cod                LIKE bat_ctr_operacion.opera_cod            -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod                -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo         -- nombre dle archivo
PRIVATE DEFINE v_folio                    LIKE glo_ctr_archivo.folio

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_detalle_monitoreo        STRING
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados

#Variables para la generacion del archivo
PRIVATE DEFINE v_fcorte                   DATE
PRIVATE DEFINE v_detalle                  detalle
PRIVATE DEFINE v_detalle_solo_inf         detalle

PRIVATE DEFINE v_ind_saldo_imss           SMALLINT
PRIVATE DEFINE v_ind_saldo_inf            SMALLINT

PRIVATE DEFINE v_sum_pesos_viv97          DECIMAL(22,2)
PRIVATE DEFINE v_sum_acciones_viv97       DECIMAL(26,6)
PRIVATE DEFINE v_num_aport_viv97          DECIMAL(12,0)

PRIVATE DEFINE v_sum_pesos_viv92          DECIMAL(22,2)
PRIVATE DEFINE v_sum_acciones_viv92       DECIMAL(26,6)
PRIVATE DEFINE v_num_aport_viv92          DECIMAL(12,0)

PRIVATE DEFINE v_sum_pesos_fondo_72       DECIMAL(22,2)
PRIVATE DEFINE v_sum_acciones_fondo72     DECIMAL(26,6)
PRIVATE DEFINE v_num_aport_fondo72        DECIMAL(12,0)

PRIVATE DEFINE v_sum_pesos_viv97_inf      DECIMAL(22,2)
PRIVATE DEFINE v_sum_acciones_viv97_inf   DECIMAL(26,6)
PRIVATE DEFINE v_num_aport_viv97_inf      DECIMAL(12,0)

PRIVATE DEFINE v_sum_pesos_viv92_inf      DECIMAL(22,2)
PRIVATE DEFINE v_sum_acciones_viv92_inf   DECIMAL(26,6)
PRIVATE DEFINE v_num_aport_viv92_inf      DECIMAL(12,0)

#Variables para el manejo de los Nombres de archivos
PRIVATE DEFINE v_nom_archivo_bdnsviv_plus         STRING
PRIVATE DEFINE v_nom_archivo_bdnsviv_inf          STRING

#Variables para las rutas fisicas
PRIVATE DEFINE v_ruta_bdnsviv_plus                STRING
PRIVATE DEFINE v_ruta_bdnsviv_inf                 STRING

#Variables donde se guardaran los apuntadores a los archivos
PRIVATE DEFINE v_archivo_bdnsviv_plus     BASE.CHANNEL
PRIVATE DEFINE v_archivo_bdnsviv_inf      BASE.CHANNEL

MAIN
   DEFINE r_resultado_opera            INTEGER
   DEFINE v_resultado_gen              INTEGER

    -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   WHENEVER ERROR CONTINUE
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 

   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   LET v_detalle_monitoreo = " PROCESO            : ",v_proceso_desc,"\n",
                             " OPERACIÓN          : ",v_opera_desc,"\n",
                             " FECHA              : ",TODAY USING 'dd-mm-yyyy',"\n",
                             " HORA               : ",TIME(CURRENT)," "
   DISPLAY v_detalle_monitoreo;
   DISPLAY "*******************************************************************"

   CALL fn_inicializa_proceso(p_pid,p_proceso_cod,p_opera_cod,0,
                                          "CBDS02",p_nombre_archivo,p_usuario_cod)
                                 RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      -- se solicita el numero de folio asociado a la operacion
      -- parametros: proceso, operacion, usuario
      CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
      RETURNING v_folio
   
      # Inicia operación
      CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,p_opera_cod,v_folio,"CBDS02",
                            p_nombre_archivo,p_usuario_cod) RETURNING r_resultado_opera
      # En el caso de que exista una inconsistencia al iniciar el proceso, se
      # Muestra un mensaje con la descripcion
      IF(r_resultado_opera)THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         #Se manda a generar el archivo de la bdnsviv-plus
         CALL fn_genera_archivo() RETURNING v_resultado_gen
         
         IF v_resultado_gen = 0 THEN
            # Finaliza la operacion de generacion del archivo
            CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
            RETURNING r_resultado_opera
            IF(r_resultado_opera <> 0)THEN         
               # Actualiza a estado erróneo
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
            END IF 
         ELSE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
         END IF
      END IF
   END IF
   
   WHENEVER ERROR STOP
END MAIN

PRIVATE FUNCTION fn_genera_archivo()
   DEFINE v_id_derechohabiente                  LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_subcuenta                           SMALLINT
   DEFINE v_fondo                               SMALLINT
   DEFINE v_pesos                               DECIMAL(12,2)
   DEFINE v_acciones                            DECIMAL(12,2)
   DEFINE v_precio                              DECIMAL(12,6)
   DEFINE v_precio_format                       DECIMAL(22,0)

   DEFINE v_pesos_mnt                           DECIMAL(12,2)
   DEFINE v_acciones_mnt                        DECIMAL(12,2)

   DEFINE v_temp_f_credito                      DATE
   DEFINE v_temp_id_credito			SMALLINT
   
   #Variables para el manejo de las consultas
   DEFINE v_consulta_trabajador                 STRING
   DEFINE v_consulta_saldo                      STRING
   
   DISPLAY "Se inicia la generacion del archivo"

   #Se preparan las consultas

   #Consulta de trabajadores
   LET v_consulta_trabajador =   "SELECT ",
                                    "afi.id_derechohabiente, ",
                                    "afi.nss, ",
                                    "afi.rfc, ",
                                    "afi.curp, ",
                                    "afi.ap_paterno_af, ",
                                    "afi.ap_materno_af, ",
                                    "afi.nombre_af, ",
                                    "afi.nombre_imss, ",
                                    "afi.f_credito, ",
                                    "afi.id_credito, ",
                                    "bdnsviv.tipo_trabajador, ",
                                    "bdnsviv.cve_afore, ",
                                    "bdnsviv.dif_afore_procanase, ",
                                    "bdnsviv.ind_credito_vivienda, ",
                                    "bdnsviv.ind_credito_43bis, ",
                                    "bdnsviv.ind_unificacion, ",
                                    "bdnsviv.ind_retiro, ",
                                    "bdnsviv.ind_traspaso_9297, ",
                                    "bdnsviv.ind_afore_afore, ",
                                    "bdnsviv.ind_dev_pagos, ",
                                    "bdnsviv.ind_trans_acreditados, ",
                                    "bdnsviv.ind_acr_43bis_prev, ",
                                    "bdnsviv.ind_acr_43bis_gar, ",
                                    "bdnsviv.ind_acr_43bis_sub ",
                                 "FROM afi_derechohabiente afi ",
                                 "LEFT JOIN cbd_detalle_bdnsviv bdnsviv ON bdnsviv.nss = afi.nss "
   PREPARE exe_consulta_trabajador FROM v_consulta_trabajador
   DECLARE cur_consulta_trabajador CURSOR FOR exe_consulta_trabajador

   #Consulta de saldos
   LET v_consulta_saldo =  "SELECT ",
                              "sdo.subcuenta, ",
                              "sdo.fondo_inversion, ",
                              "sdo.monto_acciones, ",
                              "sdo.monto_pesos ",
                           "FROM safre_sdo@vivws_tcp:cta_saldo_mensual sdo ",
                           "WHERE sdo.id_derechohabiente = ?"
                           #"AND sdo.subcuenta NOT IN (41,43)"
   PREPARE exe_consulta_saldo FROM v_consulta_saldo
   DECLARE cur_consulta_saldo CURSOR FOR exe_consulta_saldo


   #Se inicializan las variables del detalle
   CALL fn_inicializa_detalle()

   #Se inicializan los sumarios
   LET v_sum_pesos_viv97        = 0
   LET v_sum_acciones_viv97     = 0
   LET v_num_aport_viv97        = 0
   
   LET v_sum_pesos_viv92        = 0
   LET v_sum_acciones_viv92     = 0
   LET v_num_aport_viv92        = 0
   
   LET v_sum_pesos_fondo_72     = 0
   LET v_sum_acciones_fondo72   = 0
   LET v_num_aport_fondo72      = 0
   
   LET v_sum_pesos_viv97_inf    = 0
   LET v_sum_acciones_viv97_inf = 0
   LET v_num_aport_viv97_inf    = 0
   
   LET v_sum_pesos_viv92_inf    = 0
   LET v_sum_acciones_viv92_inf = 0
   LET v_num_aport_viv92_inf    = 0

   #Se establece la fecha de corte en el ultimo dia natural del mes anterior
   LET v_fcorte = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1

   #Se crean los archivos
   CALL fn_crea_archivos()
   
   #Se busca el precio de accion para la fecha de corte
   SELECT
      precio_fondo
   INTO
      v_precio
   FROM glo_valor_fondo
   WHERE fondo = 11
   AND f_valuacion = v_fcorte
   LET v_precio_format = v_precio * 1000000

   DISPLAY ""
   DISPLAY "Inicia la generacion de los detalles del archivo"
   DISPLAY ""
   #Se ejecuta la consulta de trabajadores
   PREPARE exe_pdq FROM "SET PDQPRIORITY HIGH"
   EXECUTE exe_pdq
   FOREACH cur_consulta_trabajador INTO v_id_derechohabiente,
                                        v_detalle.nss,                        #1
                                        v_detalle.rfc,                        #2
                                        v_detalle.curp,                       #3
                                        v_detalle.ape_paterno,                #4
                                        v_detalle.ape_materno,                #5
                                        v_detalle.nombre,                     #6
                                        v_detalle.nombre_imss,                #7
                                        v_temp_f_credito,   --v_detalle.f_credito,                   #33?
                                        v_temp_id_credito,
                                        v_detalle.tipo_trabajador,                      #tipo_trabajador
                                        v_detalle.cve_afore,                            #cve_afore
                                        v_detalle.diferencia_nombre,                    #dif_afore_procanase
                                        v_detalle.estatus_trabajador_credito,           #ind_credito_vivienda
                                        v_detalle.marca_trans_43bis,                    #ind_credito_43bis
                                        v_detalle.estatus_unificacion,                  #ind_unificacion
                                        v_detalle.estatus_retiro,                       #ind_retiro
                                        v_detalle.estatus_traspaso_sar92,               #ind_traspaso_9297
                                        v_detalle.estatus_traspaso_afore,               #ind_afore_afore
                                        v_detalle.estatus_devolucion_pagos,             #ind_dev_pagos
                                        v_detalle.estatus_trans_acreditados,            #ind_trans_acreditados
                                        v_detalle.estatus_acreditados_43pr,             #ind_acr_43bis_prev
                                        v_detalle.estatus_acreditados_43gt,             #ind_acr_43bis_gar
                                        v_detalle.estatus_acreditados_43su
      IF v_id_derechohabiente IS NOT NULL THEN
         #Primero se valida que los valores no vengan nulos y si es asi asignarles un espacio para respetar el layout
         IF v_detalle.nombre CLIPPED = 'S/N' THEN
            LET v_detalle.nombre = ' '
         END IF

         IF v_temp_id_credito <> 0 AND v_temp_f_credito IS NOT NULL THEN
            LET v_detalle.f_credito = v_temp_f_credito USING 'yymmdd'
         ELSE
            LET v_detalle.f_credito = ' '
         END IF

         IF v_detalle.diferencia_nombre IS NULL THEN
            LET v_detalle.diferencia_nombre          = '0'
         END IF
         IF v_detalle.estatus_trabajador_credito IS NULL THEN
            LET v_detalle.estatus_trabajador_credito = ' '
         END IF
         IF v_detalle.marca_trans_43bis IS NULL THEN
            LET v_detalle.marca_trans_43bis          = '0'
         END IF
         IF v_detalle.estatus_unificacion IS NULL THEN
            LET v_detalle.estatus_unificacion        = '0'
         END IF
         IF v_detalle.estatus_retiro IS NULL THEN
            LET v_detalle.estatus_retiro             = '0'
         END IF
         IF v_detalle.estatus_traspaso_sar92 IS NULL THEN
            LET v_detalle.estatus_traspaso_sar92     = '0'
         END IF
         IF v_detalle.estatus_traspaso_afore IS NULL THEN
            LET v_detalle.estatus_traspaso_afore     = '0'
         END IF
         IF v_detalle.estatus_devolucion_pagos IS NULL THEN
            LET v_detalle.estatus_devolucion_pagos   = '0'
         END IF
         IF v_detalle.estatus_trans_acreditados IS NULL THEN
            LET v_detalle.estatus_trans_acreditados  = '0'
         END IF
         IF v_detalle.estatus_acreditados_43pr IS NULL THEN
            LET v_detalle.estatus_acreditados_43pr   = '0'
         END IF
         IF v_detalle.estatus_acreditados_43gt IS NULL THEN
            LET v_detalle.estatus_acreditados_43gt   = '0'
         END IF
         IF v_detalle.estatus_acreditados_43su IS NULL THEN
            LET v_detalle.estatus_acreditados_43su   = '0'
         END IF

         #Se asigna la fecha del ultimo movimiento como la fecha de generacion del archivo
         LET v_detalle.f_ultimo_movimiento = v_fcorte USING 'yymmdd'
         
         #Se inicializan los indicadores de saldo
         LET v_ind_saldo_imss = 0
         LET v_ind_saldo_inf = 0

         #Se asigna el precio de accion y la fecha
         LET v_detalle.valor_paivs = v_precio_format   USING "&&&&&&&&&&&&"
         LET v_detalle.f_paivs = v_fcorte              USING "yymmdd"

         #Se establece la fecha de saldo para vivienda 97,92 y fondo 72
         LET v_detalle.f_ssv97 = v_fcorte              USING "yymmdd"
         LET v_detalle.f_ssv92 = v_fcorte              USING "yymmdd"
         --LET v_detalle.f_fondo_anterior = v_fcorte     USING "yymmdd"
         
         #Para cada nss se buscan los saldos con corte al fin de mes
         FOREACH cur_consulta_saldo USING v_id_derechohabiente INTO v_subcuenta,
                                                                    v_fondo,
                                                                    v_acciones_mnt,
                                                                    v_pesos_mnt
            IF v_acciones_mnt IS NOT NULL THEN
               LET v_acciones  = (v_acciones_mnt * 100)
               LET v_pesos  = (v_pesos_mnt * 100)
               IF v_acciones_mnt < 0 THEN
                  LET v_acciones  = (v_acciones * -1)
                  LET v_pesos  = (v_pesos * -1)
               END IF

               #Vivienda 97
               IF v_subcuenta = VIVIENDA_97 AND v_fondo = 11 THEN
                  LET v_detalle.aport_viv97 = v_pesos    USING "&&&&&&&&&&"
                  LET v_detalle.aivs_viv97 = v_acciones  USING "&&&&&&&&&&&&"
                  LET v_ind_saldo_imss = 1

                  #Se incrementan los sumarios
                  LET v_sum_pesos_viv97 = v_sum_pesos_viv97 + v_pesos_mnt
                  LET v_sum_acciones_viv97 = v_sum_acciones_viv97 + v_acciones_mnt
                  LET v_num_aport_viv97 = v_num_aport_viv97 + 1
               END IF   #FIN Vivienda 97

               #Vivienda 92
               IF v_subcuenta = VIVIENDA_92 AND v_fondo = 11 THEN
                  LET v_detalle.aport_viv92 = v_pesos    USING "&&&&&&&&"
                  LET v_detalle.aivs_viv92 = v_acciones  USING "&&&&&&&&&&&&"
                  LET v_ind_saldo_imss = 1

                  #Se incrementan los sumarios
                  LET v_sum_pesos_viv92 = v_sum_pesos_viv92 + v_pesos_mnt
                  LET v_sum_acciones_viv92 = v_sum_acciones_viv92 + v_acciones_mnt
                  LET v_num_aport_viv92 = v_num_aport_viv92 + 1
               END IF   #FIN Vivienda 92

               #Fonto 72
               IF v_subcuenta = FONDO_72 AND v_fondo = 0 THEN
                  LET v_detalle.fondo_viv72 = v_pesos USING "&&&&&&&"
                  LET v_ind_saldo_imss = 1

                  #Se incrementan los sumarios
                  LET v_sum_pesos_fondo_72 = v_sum_pesos_fondo_72 + v_pesos_mnt
                  LET v_num_aport_fondo72 = v_num_aport_fondo72 + 1
               END IF   #FIN Fonto 72

               #Vivienda 97 solo infonavit
               IF v_subcuenta = SOLO_INFONAVIT_97 AND v_fondo = 11 THEN

                  #Si es el primer saldo solo infonavit se genera todo el registro de detalle
                  IF v_ind_saldo_inf = 0 THEN
                     LET v_detalle_solo_inf.* = v_detalle.*
                     LET v_detalle_solo_inf.aport_viv92 = 0 USING "&&&&&&&&"
                     LET v_detalle_solo_inf.aivs_viv92 = 0  USING "&&&&&&&&&&&&"
                     LET v_detalle_solo_inf.fondo_viv72 = 0 USING "&&&&&&&"
                  END IF
                  
                  LET v_detalle_solo_inf.aport_viv97 = v_pesos    USING "&&&&&&&&&&"
                  LET v_detalle_solo_inf.aivs_viv97 = v_acciones  USING "&&&&&&&&&&&&"
                  LET v_ind_saldo_inf = 1

                  #Se incrementan los sumarios
                  LET v_sum_pesos_viv97_inf = v_sum_pesos_viv97_inf + v_pesos_mnt
                  LET v_sum_acciones_viv97_inf = v_sum_acciones_viv97_inf + v_acciones_mnt
                  LET v_num_aport_viv97_inf = v_num_aport_viv97_inf + 1
               END IF   #FIN Vivienda 97 solo infonavit

               #Vivienda 92 solo infonavit
               IF v_subcuenta = SOLO_INFONAVIT_92 AND v_fondo = 11 THEN

                  #Si es el primer saldo solo infonavit se genera todo el registro de detalle
                  IF v_ind_saldo_inf = 0 THEN
                     LET v_detalle_solo_inf.* = v_detalle.*
                     LET v_detalle_solo_inf.aport_viv97 = 0 USING "&&&&&&&&&&"
                     LET v_detalle_solo_inf.aivs_viv97 = 0  USING "&&&&&&&&&&&&"
                     LET v_detalle_solo_inf.fondo_viv72 = 0 USING "&&&&&&&"
                  END IF
                  
                  LET v_detalle_solo_inf.aport_viv92 = v_pesos    USING "&&&&&&&&"
                  LET v_detalle_solo_inf.aivs_viv92 = v_acciones  USING "&&&&&&&&&&&&"
                  LET v_ind_saldo_inf = 1

                  #Se incrementan los sumarios
                  LET v_sum_pesos_viv92_inf = v_sum_pesos_viv92_inf + v_pesos_mnt
                  LET v_sum_acciones_viv92_inf = v_sum_acciones_viv92_inf + v_acciones_mnt
                  LET v_num_aport_viv92_inf = v_num_aport_viv92_inf + 1
               END IF   #FIN Vivienda 92 solo infonavit
            END IF   #FIN v_acciones IS NOT NULL
         END FOREACH #FIN cur_consulta_saldo

         #Se inserta el registro en el archivo correspondiente
         CALL fn_inserta_registro()
         
         #Se inicializan los detalles
         CALL fn_inicializa_detalle()
      END IF   #FIN v_id_derechohabiente IS NOT NULL
   END FOREACH #FIN cur_consulta_trabajador
   CALL fn_finaliza_archivos()
   RETURN 0
END FUNCTION

PRIVATE FUNCTION fn_llena_indicadores_solo_infonavit()
   #Indicadores para los trabajadores solo infonavit
   IF v_ind_saldo_inf = 1 THEN
   END IF   #FIN Indicadores solo infonavit
   
END FUNCTION

PRIVATE FUNCTION fn_inserta_registro()
   DEFINE v_registro                   STRING

   #Este caso es para los nss's que no tienen saldo
   IF v_ind_saldo_imss = 0 AND v_ind_saldo_inf = 0 THEN
      #Se valida si el NSS que no tiene saldo corresponde a un derechohabiente imss o solo infonavit
      IF v_detalle.nss[1,2] = '77' THEN
         #Para los NSS's solo infonavit se crea el registro y se activa el indicador
         LET v_detalle_solo_inf.* = v_detalle.*
         LET v_ind_saldo_inf = 1
      ELSE
         #Para los NSS's imss solo se activa el indicador
         LET v_ind_saldo_imss = 1
      END IF
   END IF

   #Se valida si el registro es de Saldo IMSS
   IF v_ind_saldo_imss = 1 THEN
      LET v_registro =  v_detalle.nss,
                        v_detalle.rfc,
                        v_detalle.curp,
                        v_detalle.ape_paterno,
                        v_detalle.ape_materno,
                        v_detalle.nombre,
                        v_detalle.nombre_imss,
                        v_detalle.tipo_trabajador,
                        v_detalle.cve_afore,
                        v_detalle.aport_viv97,
                        v_detalle.num_aportaciones97,
                        v_detalle.f_ssv97,
                        v_detalle.aport_viv92,
                        v_detalle.num_aportaciones92,
                        v_detalle.f_ssv92,
                        v_detalle.fondo_viv72,
                        v_detalle.f_fondo_anterior,
                        v_detalle.aivs_viv97,
                        v_detalle.aivs_viv92,
                        v_detalle.valor_paivs,
                        v_detalle.f_paivs,
                        v_detalle.marca_modif_nombre,
                        v_detalle.estatus_unificacion,
                        v_detalle.marca_unificacion,
                        v_detalle.estatus_retiro,
                        v_detalle.marca_retiro,
                        v_detalle.estatus_traspaso_afore,
                        v_detalle.estatus_devolucion_pagos,
                        v_detalle.estatus_trans_acreditados,
                        v_detalle.marca_trans_43bis,
                        v_detalle.estatus_trabajador_credito,
                        v_detalle.solicitud_credito,
                        v_detalle.f_credito,
                        v_detalle.f_aivs_retiro,
                        v_detalle.f_ultimo_movimiento,
                        v_detalle.diferencia_nombre,
                        v_detalle.estatus_traspaso_sar92,
                        v_detalle.estatus_acreditados_43pr,
                        v_detalle.estatus_acreditados_43gt,
                        v_detalle.estatus_acreditados_43su

      #se escribe el registro en el archivo                   
      CALL v_archivo_bdnsviv_plus.write([v_registro])
   END IF #FIN Saldo IMSS
   
   #Se valida si el registro es de Saldo Solo Infonavit
   IF v_ind_saldo_inf = 1 THEN
      LET v_registro =  v_detalle_solo_inf.nss,
                        v_detalle_solo_inf.rfc,
                        v_detalle_solo_inf.curp,
                        v_detalle_solo_inf.ape_paterno,
                        v_detalle_solo_inf.ape_materno,
                        v_detalle_solo_inf.nombre,
                        v_detalle_solo_inf.nombre_imss,
                        v_detalle_solo_inf.tipo_trabajador,
                        v_detalle_solo_inf.cve_afore,
                        v_detalle_solo_inf.aport_viv97,
                        v_detalle_solo_inf.num_aportaciones97,
                        v_detalle_solo_inf.f_ssv97,
                        v_detalle_solo_inf.aport_viv92,
                        v_detalle_solo_inf.num_aportaciones92,
                        v_detalle_solo_inf.f_ssv92,
                        v_detalle_solo_inf.fondo_viv72,
                        v_detalle_solo_inf.f_fondo_anterior,
                        v_detalle_solo_inf.aivs_viv97,
                        v_detalle_solo_inf.aivs_viv92,
                        v_detalle_solo_inf.valor_paivs,
                        v_detalle_solo_inf.f_paivs,
                        v_detalle_solo_inf.marca_modif_nombre,
                        v_detalle_solo_inf.estatus_unificacion,
                        v_detalle_solo_inf.marca_unificacion,
                        v_detalle_solo_inf.estatus_retiro,
                        v_detalle_solo_inf.marca_retiro,
                        v_detalle_solo_inf.estatus_traspaso_afore,
                        v_detalle_solo_inf.estatus_devolucion_pagos,
                        v_detalle_solo_inf.estatus_trans_acreditados,
                        v_detalle_solo_inf.marca_trans_43bis,
                        v_detalle_solo_inf.estatus_trabajador_credito,
                        v_detalle_solo_inf.solicitud_credito,
                        v_detalle_solo_inf.f_credito,
                        v_detalle_solo_inf.f_aivs_retiro,
                        v_detalle_solo_inf.f_ultimo_movimiento,
                        v_detalle_solo_inf.diferencia_nombre,
                        v_detalle_solo_inf.estatus_traspaso_sar92,
                        v_detalle_solo_inf.estatus_acreditados_43pr,
                        v_detalle_solo_inf.estatus_acreditados_43gt,
                        v_detalle_solo_inf.estatus_acreditados_43su
      #se escribe el registro en el archivo                   
      CALL v_archivo_bdnsviv_inf.write([v_registro])
   END IF   #FIN Saldo Solo Infonavit
END FUNCTION

PRIVATE FUNCTION fn_crea_archivos()
   DEFINE v_ruta_envio                       LIKE seg_modulo.ruta_envio

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   #Se asigna el nombre del archivo
   LET v_nom_archivo_bdnsviv_plus = "SALDOS_" , v_fcorte USING "ddmmyyyy",".saldos"
   LET v_nom_archivo_bdnsviv_inf = "SALDOS_INFONAVIT_" , v_fcorte USING "ddmmyyyy", ".soloinfonavit"

   #Se crea el nombre del archivo con la ruta fisica en el servidor
   LET v_ruta_bdnsviv_plus = v_ruta_envio CLIPPED || "/" ||v_nom_archivo_bdnsviv_plus
   LET v_ruta_bdnsviv_inf = v_ruta_envio CLIPPED || "/" ||v_nom_archivo_bdnsviv_inf

   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Se crean los archivos: "
   DISPLAY v_ruta_bdnsviv_plus
   DISPLAY v_ruta_bdnsviv_inf
   DISPLAY ""
   DISPLAY "*******************************************************************"

   -- se crea el manejador de archivo
   LET v_archivo_bdnsviv_plus = base.Channel.create()
   LET v_archivo_bdnsviv_inf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo_bdnsviv_plus.openFile(v_ruta_bdnsviv_plus, "w" )
   CALL v_archivo_bdnsviv_plus.setDelimiter("")

   CALL v_archivo_bdnsviv_inf.openFile(v_ruta_bdnsviv_inf, "w" )
   CALL v_archivo_bdnsviv_inf.setDelimiter("")
      
END FUNCTION

PRIVATE FUNCTION fn_finaliza_archivos()
   DEFINE v_archivo                             VARCHAR(40)                 
   DEFINE v_inserta_ctr_interface               STRING
   DEFINE v_inserta_cifras_interface            STRING
   DEFINE v_comando                             STRING
   

   #se cierran los archivos
   CALL v_archivo_bdnsviv_plus.close()
   CALL v_archivo_bdnsviv_inf.close()
   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generacion de los archivos: "
   DISPLAY v_ruta_bdnsviv_plus
   DISPLAY v_ruta_bdnsviv_inf

   DISPLAY ""
   DISPLAY "Uniendo los archivos..."
   DISPLAY ""
   LET v_comando = "cat ", v_ruta_bdnsviv_inf, " >> ", v_ruta_bdnsviv_plus
   DISPLAY "comando: ", v_comando
   RUN v_comando

   DISPLAY ""
   DISPLAY "Eliminando el archivo: ", v_ruta_bdnsviv_inf
   DISPLAY ""
   LET v_comando = "rm ", v_ruta_bdnsviv_inf
   DISPLAY "comando: ", v_comando
   RUN v_comando

   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Registrando las cifras de control del archivo generado"
   DISPLAY "Folio del archivo: ", v_folio USING "&&&&&&&&&"
   DISPLAY "Fecha de corte: ", v_fcorte USING 'dd-mm-yyyy'
   DISPLAY "Nombre del archivo: ", v_nom_archivo_bdnsviv_plus
   DISPLAY "*******************************************************************"
   
   #Se inserta el registro de control de la generacion del archivo
   LET v_archivo = v_nom_archivo_bdnsviv_plus CLIPPED
   LET v_inserta_ctr_interface = "INSERT INTO cbd_ctr_interface VALUES(?,?,?)"
   PREPARE exe_inserta_ctr_interface FROM v_inserta_ctr_interface
   EXECUTE exe_inserta_ctr_interface USING v_folio, v_fcorte, v_archivo

   #Se insertan las cifras de control
   LET v_inserta_cifras_interface = "INSERT INTO cbd_cifras_interface VALUES(?,?,?,?,?)"
   PREPARE exe_inserta_cifras_interface FROM v_inserta_cifras_interface

   #Regitro de vivienda 97
   EXECUTE exe_inserta_cifras_interface USING v_folio,
                                              VIVIENDA_97,
                                              v_sum_acciones_viv97,
                                              v_sum_pesos_viv97,
                                              v_num_aport_viv97

   #Regitro de vivienda 92
   EXECUTE exe_inserta_cifras_interface USING v_folio,
                                              VIVIENDA_92,
                                              v_sum_acciones_viv92,
                                              v_sum_pesos_viv92,
                                              v_num_aport_viv92

   #Regitro de fondo 72
   EXECUTE exe_inserta_cifras_interface USING v_folio,
                                              FONDO_72,
                                              v_sum_acciones_fondo72,
                                              v_sum_pesos_fondo_72,
                                              v_num_aport_fondo72

   #Regitro de vivienda 97 Solo Infonavit
   EXECUTE exe_inserta_cifras_interface USING v_folio,
                                              SOLO_INFONAVIT_97,
                                              v_sum_acciones_viv97_inf,
                                              v_sum_pesos_viv97_inf,
                                              v_num_aport_viv97_inf

   #Regitro de vivienda 92 Solo Infonavit
   EXECUTE exe_inserta_cifras_interface USING v_folio,
                                              SOLO_INFONAVIT_92,
                                              v_sum_acciones_viv92_inf,
                                              v_sum_pesos_viv92_inf,
                                              v_num_aport_viv92_inf

   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generacion del archivo de saldos: "
   DISPLAY ""
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY "Registros IMSS"
   DISPLAY "Total de registros con aportaciones en la subcuenta de vivienda 97:" , v_num_aport_viv97
   DISPLAY "Total de registros con aportaciones en la subcuenta de vivienda 92:" , v_num_aport_viv92
   DISPLAY "Total de registros con aportaciones en el fondo de vivienda 72:" , v_num_aport_fondo72
   DISPLAY ""
   DISPLAY "Registros Solo Infonavit"
   DISPLAY "Total de registros con aportaciones en la subcuenta de vivienda 97:" , v_num_aport_viv97_inf
   DISPLAY "Total de registros con aportaciones en la subcuenta de vivienda 92:" , v_num_aport_viv92_inf
   DISPLAY ""
   DISPLAY "*******************************************************************"

END FUNCTION

PRIVATE FUNCTION fn_inicializa_detalle()
   INITIALIZE v_detalle_solo_inf.*     TO NULL
   INITIALIZE v_detalle.*              TO NULL

   LET v_detalle.cve_afore                  = '000'
   LET v_detalle.aport_viv97                = 0 USING '&&&&&&&&&&'
   LET v_detalle.num_aportaciones97         = 0 USING '&&&&&'
   LET v_detalle.f_ssv97                    = 0 USING '&&&&&&&&'
   LET v_detalle.aport_viv92                = 0 USING '&&&&&&&&'
   LET v_detalle.num_aportaciones92         = 0 USING '&&&&&'
   LET v_detalle.f_ssv92                    = 0 USING '&&&&&&&&'
   LET v_detalle.fondo_viv72                = 0 USING '&&&&&&&'
   LET v_detalle.f_fondo_anterior           = ' '
   LET v_detalle.aivs_viv97                 = 0 USING '&&&&&&&&&&&&'
   LET v_detalle.aivs_viv92                 = 0 USING '&&&&&&&&&&&&'
   LET v_detalle.valor_paivs                = 0 USING '&&&&&&&&&&&&'
   LET v_detalle.f_paivs                    = 0 USING '&&&&&&&&'
   LET v_detalle.marca_modif_nombre         = '0'
   LET v_detalle.estatus_unificacion        = '0'
   LET v_detalle.marca_unificacion          = '0'
   LET v_detalle.estatus_retiro             = '0'
   LET v_detalle.marca_retiro               = '0'
   LET v_detalle.estatus_traspaso_afore     = '0'
   LET v_detalle.estatus_devolucion_pagos   = '0'
   LET v_detalle.estatus_trans_acreditados  = '0'
   LET v_detalle.marca_trans_43bis          = '0'
   LET v_detalle.estatus_trabajador_credito = ' '
   LET v_detalle.solicitud_credito          = ' '
   LET v_detalle.f_credito                  = ' '
   LET v_detalle.f_aivs_retiro              = ' '
   LET v_detalle.f_ultimo_movimiento        = ' '
   LET v_detalle.diferencia_nombre          = '0'
   LET v_detalle.estatus_traspaso_sar92     = '0'
   LET v_detalle.estatus_acreditados_43pr   = '0'
   LET v_detalle.estatus_acreditados_43gt   = '0'
   LET v_detalle.estatus_acreditados_43su   = '0'
   
END FUNCTION
