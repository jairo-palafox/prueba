--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#############################################################################
#Módulo          => GRT                                                     #
#Programa        => GRTS04                                                  #
#Objetivo        => Programa para generar el archivo de solicitudes de      #
#                   devolucion para el módulo de Créd. Garant. 43 bis       #
#Autor:          => Franciso López, EFP                                     #
#Fecha Inicio    => 30 Mayo 2012                                            #
#############################################################################

DATABASE safre_viv

   DEFINE g_pid                       LIKE bat_ctr_proceso.pid     --  ID del proceso
   DEFINE g_proceso_cod               LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod                 LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE p_usuario_cod               LIKE seg_usuario.usuario_cod -- Clave de usuario
   DEFINE g_num_folio                 DECIMAL(9)
   DEFINE g_tpo_transferencia         LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia
   DEFINE v_ejecuta_sh                STRING

#Objetivo:
MAIN

   DEFINE p_nom_archivo               LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_estatus                   SMALLINT
   DEFINE v_id_lote                   DECIMAL(10,0)
   DEFINE v_lote                      SMALLINT
   DEFINE v_f_lote                    DATE
   DEFINE v_f_presentacion            DATE   --Fecha de presentacion, today
   DEFINE v_f_movimiento              DATE   --Fecha de movimiento, 14 dia habil del mes posterior
   DEFINE v_id_lote_dev               CHAR(16)
   DEFINE v_s_titulo_correo           STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo          STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_mens_correo             STRING -- contiene el cuerpo del correo
   DEFINE v_v_nom_reporte             VARCHAR(80) -- nombre del reporte
   DEFINE v_c_programa_cod            LIKE cat_operacion.programa_cod -- programa de la operación
   DEFINE v_c_ruta_bin                LIKE seg_modulo.ruta_bin -- ruta bin
   DEFINE v_c_ruta_listados           LIKE seg_modulo.ruta_listados -- ruta listados

   #Si se ha recibido parámetros se continua
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)  --número de proceso
   LET g_num_folio      = ARG_VAL(5)
   LET p_nom_archivo    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTS04.log")

   DISPLAY "=INICIA GRTS04="
   DISPLAY " USUARIO       : ",p_usuario_cod
   DISPLAY " PID           : ",g_pid
   DISPLAY " ARCHIVO       : ",p_nom_archivo

   -- se inicializan variables
   LET g_tpo_transferencia = "91" -- 91- Solicitud DSE GRT
   LET v_id_lote           = 02 -- ya que es solo un archivo el lote siempre es 1
   LET v_lote              = 1  -- ya que es solo un archivo el lote siempre es 1
   LET v_f_lote            = TODAY

   -- se genera el folio
   LET g_num_folio = fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
   DISPLAY " FOLIO         : ",g_num_folio USING "#########&"

   -- se obtiene el catorceavo dia habil del mes actual
   LET v_f_presentacion = TODAY - DAY(TODAY) + 1

   PREPARE prp_obtiene_habil FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_f_presentacion||"',11)"
   EXECUTE prp_obtiene_habil INTO v_f_presentacion

  {
   -- se valida que la fecha de presentación sea mayor o igual que HOY
   IF v_f_presentacion < TODAY THEN
      -- se obtiene el catorceavo dia habil del mes siguiente
      LET v_f_presentacion = TODAY - DAY(TODAY) + 1
      LET v_f_presentacion = v_f_presentacion + 1 UNITS MONTH

      PREPARE prp_obtiene_habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_f_presentacion||"',11)"
      EXECUTE prp_obtiene_habil_sig INTO v_f_presentacion
   END IF
  }

   -- Se contruye la fecha movimiento: Primer día natural del mes siguiente a la fecha de presentación
   -- Finalmente se quedará como el primer día natural de la fecha de presentación 22/11/2012 01:33:09 p.m.
   -- Debe ser primer día natural del mes siguiente de acuerdo con el req. 212
   LET v_f_movimiento = v_f_presentacion - DAY(v_f_presentacion) + 1
   LET v_f_movimiento = v_f_movimiento + 1 UNITS MONTH

   DISPLAY " Fecha presentación : ",v_f_presentacion USING "dd/mm/yyyy"
   DISPLAY " Fecha movimiento   : ",v_f_movimiento USING "dd/mm/yyyy"

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING v_c_ruta_bin, v_c_ruta_listados

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_proceso_cod , g_opera_cod)

   -- se indica la salida del reporte
   LET v_v_nom_reporte = p_usuario_cod CLIPPED, "-", v_c_programa_cod CLIPPED,"-", g_pid USING "&&&&&", "-", g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"

   --Se asigna la fecha con formato AAAAMMDD
   LET v_id_lote_dev = '04002',YEAR(TODAY)  USING "&&&&",MONTH(TODAY) USING "&&",
                       DAY(TODAY) USING "&&",'001'

   DISPLAY " Se crea tabla temporal"
   CALL fn_cre_tbl_temp_SolicDev()

   DISPLAY " Ejecuta solicitud devolución sdos exc "
   #Llamada a ejecución de procedimiento almacenado
   CALL fn_sp_grt_solicitud_devolucion(v_id_lote, v_lote, v_f_lote, v_f_presentacion, v_f_movimiento, v_id_lote_dev)

   --Se actualiza el proceso como finalizado
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estatus

   -- se verifica si fue posible finalizar la operacion
   IF v_estatus <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(v_estatus)

      EXIT PROGRAM
   END IF

   DISPLAY " Se genera reporte"
   --Se genera el reporte de las solicitudes de las devoluciones
   CALL fn_reporte_archivo_solicitud_devolucion(v_id_lote, v_lote, v_f_lote, v_f_presentacion, v_f_movimiento, v_id_lote_dev, p_nom_archivo, v_v_nom_reporte)

   DISPLAY " Envia correo del reporte"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: SOLICITUD DE DEVOLUCIÓN DE SALDOS EXCEDENTES CRÉDITOS EN GARANTÍA 43BIS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",g_pid,"\n",
                          "Proceso      : DEVOLUCIÓN DE SALDOS EXCEDENTES CRÉDITOS EN GARANTÍA 43BIS\n",
                          "Operacion    : SOLICITUD DE DEVOLUCIÓN\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod, v_s_archivo_correo, v_s_titulo_correo, v_s_mens_correo)

   DISPLAY "=FIN="

END MAIN

#Objetivo: Ejecuta el Store procedure para la incializacion de la solicitud de las devoluciones
FUNCTION fn_sp_grt_solicitud_devolucion(p_id_lote, p_lote, p_f_lote, p_f_presentacion, p_f_movimiento, p_id_lote_dev)

   DEFINE p_id_lote                 DECIMAL(10,0)
   DEFINE p_lote                    SMALLINT
   DEFINE p_f_lote                  DATE
   DEFINE p_f_presentacion          DATE
   DEFINE p_f_movimiento            DATE
   DEFINE p_id_lote_dev             CHAR(16)
   DEFINE v_sql_procedure           STRING

   LET v_sql_procedure = "EXECUTE PROCEDURE sp_dse_solicitud_devol_grt(?,?,?,?,?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_sp_solicitud_devolucion_pag FROM v_sql_procedure
   EXECUTE prp_sp_solicitud_devolucion_pag USING p_id_lote,
                                                 p_lote,
                                                 p_f_lote,
                                                 p_f_presentacion,
                                                 p_f_movimiento,
                                                 p_id_lote_dev,
                                                 p_usuario_cod

END FUNCTION

#Objetivo: Ejecuta el Store procedure para la incializacion de la solicitud de las devoluciones
FUNCTION fn_reporte_archivo_solicitud_devolucion(p_id_lote, p_lote, p_f_lote, p_f_presentacion, p_f_movimiento, p_id_lote_dev, p_c_nom_archivo, p_v_nom_reporte)

   DEFINE p_id_lote                 DECIMAL(10,0)
   DEFINE p_lote                    SMALLINT
   DEFINE p_f_lote                  DATE
   DEFINE p_f_presentacion          DATE
   DEFINE p_f_movimiento            DATE
   DEFINE v_c_f_presentacion        CHAR(8) -- fecha de presentación con formato "yyyymmdd"
   DEFINE v_c_f_movimiento          CHAR(8) -- fecha de movimiento con formato "yyyymmdd"
   DEFINE p_id_lote_dev             CHAR(16)
   DEFINE p_c_nom_archivo           LIKE cre_ctr_archivo.nom_archivo -- nombre del archivo
   DEFINE p_v_nom_reporte           VARCHAR(80) -- nombre del reporte
   DEFINE v_string_encabezado       STRING
   DEFINE v_string_detalle          STRING
   DEFINE v_string_sumario          STRING
   DEFINE v_suma_id_22              DECIMAL(22,2)
   DEFINE v_suma_id_23              DECIMAL(22,2)
   --DEFINE v_suma_id_29              DECIMAL(22,2)
   --DEFINE v_suma_id_30              DECIMAL(22,2)
   DEFINE v_indice                  INTEGER
   DEFINE v_archivo_sol_devolucion  BASE.CHANNEL
   DEFINE v_ruta_envio              LIKE seg_modulo.ruta_envio
   DEFINE v_s_ruta_archivo          STRING
   DEFINE v_s_ruta_archivo_cp       STRING
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_query                   STRING
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_manejador_rpt           om.SaxDocumentHandler -- Contenedor de Documentos para el reporte

   --Datos de la tabla temporal rep_devolucion_grt
   DEFINE v_r_sol_sdos RECORD
      curp                          CHAR(18),
      nss                           CHAR(11),
      rfc                           CHAR(13),
      ap_paterno_af                 CHAR(40),
      ap_materno_af                 CHAR(40),
      nombre_af                     CHAR(40),
      aivs97                        DECIMAL(22,2),
      pesos97                       DECIMAL(22,2),
      --nss_separacion                VARCHAR(20,0),
      --origen_devolucion             CHAR(2),
      monto_aportacion              DECIMAL(22,2),
      aivs_aportacion               DECIMAL(22,2),
      --ivs92                        DECIMAL(22,2),
      --esos92                       DECIMAL(22,2),
      nom_imss                      CHAR(50),
      num_credito                   DECIMAL(10,0)
   END RECORD

   DEFINE v_arch_encabezado RECORD
      id_01                         CHAR(02),   --Tipo de Registro
      id_02                         CHAR(02),   --Identificador de Servicio
      id_03                         CHAR(02),   --Identificador de Operación
      id_04                         CHAR(02),   --Tipo de entidad origen
      id_05                         CHAR(03),   --Clave de entidad origen
      id_06                         CHAR(02),   --Tipo de entidad destino
      id_07                         CHAR(03),   --Clave de entidad destino
      id_08                         CHAR(03),   --Entidad federativa de envío de lote
      id_09                         CHAR(08),   --Fecha de presentación
      id_10                         CHAR(03),   --Consecutivo del lote en el día
      id_11                         CHAR(02),   --Clave de modalidad de recepción
      id_12                         CHAR(02),   --Código de resultado de la Operación
      id_13                         CHAR(09),   --Motivo de rechazo del lote
      id_14                         CHAR(687)   --Filler
   END RECORD

--Arreglos con los datos del archivo
   DEFINE v_arch_detalle RECORD
      id_01                         CHAR(02),  --Tipo de Registro
      id_02                         CHAR(10),  --Contador de Servicio
      id_03                         CHAR(02),  --Tipo de entidad receptora de la cuenta
      id_04                         CHAR(03),  --Clave de entidad receptora de la cuenta
      id_05                         CHAR(02),  --Tipo de entidad cedente de la cuenta
      id_06                         CHAR(03),  --Clave de entidad ced. de la cuenta
      id_07                         CHAR(02),  --Origen/Tipo de la Transferencia
      id_08                         CHAR(08),  --Fecha de presentación
      id_09                         CHAR(08),  --Fecha de movimiento
      id_10                         CHAR(18),  --CURP del trabajador
      id_11                         CHAR(11),  --NSS del trabajador
      --id_12                         CHAR(13) --Filler
      id_12                         CHAR(28),  --Filler
      --id_13                         CHAR(01),  --Identificador de Asignación
      --id_14                         CHAR(01),  --Tipo de devolución
      --id_15                         CHAR(13),  --RFC del Trabajador según Infonavit
      id_16                         CHAR(40),  --Apellido paterno del trabajador en AFORE receptora
      id_17                         CHAR(40),  --Apellido materno del trabajador en AFORE receptora
      id_18                         CHAR(40),  --Nombres del trabajador en AFORE receptora
      id_19                         CHAR(22),  --Filler
      id_20                         CHAR(16),  --Identificador de lote de la devolución
      id_21                         CHAR(219),  --Filler
      id_22                         CHAR(15),  --(09,06) Número de "Aplicaciones de Intereses de Vivienda" 97
      id_23                         CHAR(15),  --(13,02) Saldo de vivienda 97
      --id_24                         CHAR(11),  --filler
      id_24                         CHAR(78),  --filler
      --id_25                         CHAR(02),  --origen de la devolución
      --id_26                         CHAR(15),  --(13,02) Monto de Aportaciones 5%
      --id_27                         CHAR(15),  --(09,06) Aplicación de Intereses de Aportaciones 5%
      --id_28                         CHAR(02),  --  
      --id_29                         CHAR(15),  --(09,06) Número de "Aplicaciones de Intereses de Vivienda" 92
      --id_30                         CHAR(15),  --(13,02) Saldo de Vivienda 92
      --id_31                         CHAR(03),  --Filler
      id_32                         CHAR(02),  --Código Resultado de la Operación
      --id_33                         CHAR(03),  --Diagnóstico del proceso
      id_33                         CHAR(15),  --Diagnóstico del proceso
      id_34                         CHAR(50),  --Nombre del Trabajador según IMSS
      id_35                         CHAR(10),  --Número de Crédito INFONAVIT
      id_36                         CHAR(15),  --(13,02) Intereses Saldo  de Vivienda 97
      --id_37                        CHAR(15),  --(13,02) Intereses Saldo de Vivienda 92
      id_38                         CHAR(56)  --Filler
   END RECORD

   DEFINE  v_arch_sumario RECORD
      id_01                         CHAR(2),  --(2  ,0 ) Tipo de Registro
      id_02                         CHAR(9),  --(9  ,0 ) Cantidad de Registros de Detalle
      id_03                         CHAR(30),  --(30 ,0 ) Filler
      id_04                         CHAR(18),  --(12 ,06) Suma de número de "Aplicaciones de Intereses de Vivienda" 97
      id_05                         CHAR(15),  --(13 ,02) Suma del Saldo de Vivienda 97
      --id_06                         CHAR(45),  --(45 ,0 ) Filler
      id_06                         CHAR(75),  --(45 ,0 ) Filler
      --id_07                         CHAR(18),  --(12 ,06) Suma de número de "Aplicaciones de Intereses de Vivienda" 92
      --id_08                         CHAR(15),  --(13 ,02) Suma del Saldo de Vivienda 92
      id_09                         CHAR(15),  --(13 ,02) Intereses Vivienda 97
      --id_10                         CHAR(15),  --(13 ,02) Intereses Vivienda 92
      id_11                         CHAR(566)  --(548,0 ) Filler
   END RECORD

   DEFINE v_r_dse_ctr_archivo       RECORD LIKE dse_ctr_archivo.* -- registro de la tabla de control
   DEFINE v_i_lote                  LIKE dse_ctr_archivo.lote -- lote del archivo
   DEFINE v_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin
   DEFINE v_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados
   DEFINE v_suma_aivs97             DECIMAL(22,6)
   DEFINE v_suma_saldo97            DECIMAL(22,2)
   DEFINE r_b_valida                SMALLINT -- estatus del proceso
   DEFINE v_cnt_envio               INTEGER
   DEFINE v_cnt_reenvio             INTEGER
   DEFINE v_envio_aivs97            DECIMAL(22,6)
   DEFINE v_envio_saldo97           DECIMAL(22,2)
   DEFINE v_envio_aivs92            DECIMAL(22,6)
   DEFINE v_envio_saldo92           DECIMAL(22,2)
   DEFINE v_reenvio_aivs97          DECIMAL(22,6)
   DEFINE v_reenvio_saldo97         DECIMAL(22,2)
   DEFINE v_reenvio_aivs92          DECIMAL(22,6)
   DEFINE v_reenvio_saldo92         DECIMAL(22,2)

   LET v_cnt_envio       = 0
   LET v_cnt_reenvio     = 0
   LET v_envio_aivs97    = 0
   LET v_envio_saldo97   = 0
   LET v_envio_aivs92    = 0
   LET v_envio_saldo92   = 0
   LET v_reenvio_aivs97  = 0
   LET v_reenvio_saldo97 = 0
   LET v_reenvio_aivs92  = 0
   LET v_reenvio_saldo92 = 0

   --Se obtiene las rutas para la generacion del archivo
      -- se obtienen la ruta envio del modulo
   LET v_query = " SELECT ruta_envio\n",
                 "   FROM seg_modulo\n",
                 "  WHERE modulo_cod = 'grt' "

   PREPARE prp_ruta_archivo FROM v_query
   EXECUTE prp_ruta_archivo INTO v_ruta_envio

   DISPLAY " Ruta envio: ",v_ruta_envio
   -- se crea el nombre del archivo
   --LET p_c_nom_archivo = "sol_dev",v_f_fecha_archivo USING "yyyymmdd",".cdse"

   -- se concatena el nombre del archivo con la ruta de envio
   LET v_s_ruta_archivo = v_ruta_envio CLIPPED,"/",p_c_nom_archivo CLIPPED

   -- se crea el manejador de archivo
   LET v_archivo_sol_devolucion = BASE.CHANNEL.CREATE()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo_sol_devolucion.openFile(v_s_ruta_archivo, "w" )
   CALL v_archivo_sol_devolucion.setDelimiter("")

   -- se asigna la fecha de presentación con formato AAAAMMDD
   LET v_c_f_presentacion = p_f_presentacion USING "yyyymmdd"
   LET v_c_f_movimiento = p_f_movimiento USING "yyyymmdd"

   --Se asignan los valores del encabezado
   LET v_arch_encabezado.id_01 = "01"    --CHAR(02 )   --Tipo de Registro
   LET v_arch_encabezado.id_02 = "02"    --CHAR(02 )   --Identificador de Servicio
   LET v_arch_encabezado.id_03 = "15"    --CHAR(02 )   --Identificador de Operación
   LET v_arch_encabezado.id_04 = "04"    --CHAR(02 )   --Tipo de entidad origen
   LET v_arch_encabezado.id_05 = "002"   --CHAR(03 )   --Clave de entidad origen
   LET v_arch_encabezado.id_06 = "01"    --CHAR(02 )   --Tipo de entidad destino
   LET v_arch_encabezado.id_07 = ""      --CHAR(03 )   --Clave de entidad destino
   LET v_arch_encabezado.id_08 = "009"   --CHAR(03 )   --Entidad federativa de envío de lote
   LET v_arch_encabezado.id_09 = v_c_f_presentacion
   LET v_arch_encabezado.id_10 = "001"   --CHAR(03 )   --Consecutivo del lote en el día
   LET v_arch_encabezado.id_11 = "" --"02"    --CHAR(02 )   --Clave de modalidad de recepción
   LET v_arch_encabezado.id_12 = ""      --CHAR(02 )   --Código de resultado de la Operación
   LET v_arch_encabezado.id_13 = ""      --CHAR(03x3)  --Motivo de rechazo del lote
   LET v_arch_encabezado.id_14 = ""      --CHAR(687)   --Filler

   --Se concatena el encabezado
   LET v_string_encabezado = v_arch_encabezado.id_01,
                             v_arch_encabezado.id_02,
                             v_arch_encabezado.id_03,
                             v_arch_encabezado.id_04,
                             v_arch_encabezado.id_05,
                             v_arch_encabezado.id_06,
                             v_arch_encabezado.id_07,
                             v_arch_encabezado.id_08,
                             v_arch_encabezado.id_09,
                             v_arch_encabezado.id_10,
                             v_arch_encabezado.id_11,
                             v_arch_encabezado.id_12,
                             v_arch_encabezado.id_13,
                             v_arch_encabezado.id_14

   --Se ingresa el encabezado en el archivo
   CALL v_archivo_sol_devolucion.WRITE([v_string_encabezado])

   LET v_query = " SELECT * FROM safre_tmp:tmp_rep_devolucion_grt"

   PREPARE prp_rep_selicitud_pago FROM  v_query
   DECLARE cur_rep_selicitud_pago CURSOR FOR prp_rep_selicitud_pago

   LET v_indice     = 1
   LET v_suma_id_22 = 0
   LET v_suma_id_23 = 0
   --LET v_suma_id_29 = 0
   --LET v_suma_id_30 = 0

   --Inicia el detalle del archivo
   --Se inicia el foreach para la lectura de los registros de la tabla temporal
   FOREACH cur_rep_selicitud_pago INTO v_r_sol_sdos.*
      -- se validan los importes consultados
      IF v_r_sol_sdos.aivs97 IS NULL THEN
         LET v_r_sol_sdos.aivs97 = 0
      END IF

      IF v_r_sol_sdos.pesos97 IS NULL THEN
         LET v_r_sol_sdos.pesos97 = 0
      END IF

      LET v_arch_detalle.id_01 = "02"
      LET v_arch_detalle.id_02 = v_indice USING "&&&&&&&&&&"
      LET v_arch_detalle.id_03 = "01"
      LET v_arch_detalle.id_04 = ""
      LET v_arch_detalle.id_05 = "04"
      LET v_arch_detalle.id_06 = "002"
      LET v_arch_detalle.id_07 = "19"
      LET v_arch_detalle.id_08 = v_c_f_presentacion
      LET v_arch_detalle.id_09 = v_c_f_movimiento
      LET v_arch_detalle.id_10 = v_r_sol_sdos.curp
      LET v_arch_detalle.id_11 = v_r_sol_sdos.nss
      LET v_arch_detalle.id_12 = ""
      --LET v_arch_detalle.id_13 = ""
      --LET v_arch_detalle.id_14 = "0"
      --LET v_arch_detalle.id_15 = v_r_sol_sdos.rfc
      LET v_arch_detalle.id_16 = v_r_sol_sdos.ap_paterno_af
      LET v_arch_detalle.id_17 = v_r_sol_sdos.ap_materno_af
      LET v_arch_detalle.id_18 = v_r_sol_sdos.nombre_af
      LET v_arch_detalle.id_19 = ""
      LET v_arch_detalle.id_20 = p_id_lote_dev
      LET v_arch_detalle.id_21 = ""
      LET v_arch_detalle.id_22 = (v_r_sol_sdos.aivs97 * 1000000) USING "&&&&&&&&&&&&&&&" --SUM(AIVS97) his_devolucion           --(09,06) Número de "Aplicaciones de Intereses de Vivienda" 97
      LET v_arch_detalle.id_23 = (v_r_sol_sdos.pesos97 * 100)    USING "&&&&&&&&&&&&&&&" --SUM(PESOS97) his_devolucion          --(13,02) Saldo de vivienda 97
      LET v_arch_detalle.id_24 = ""
      --LET v_arch_detalle.id_25 = v_r_sol_sdos.origen_devolucion
      --LET v_arch_detalle.id_26 = (v_r_sol_sdos.monto_aportacion * 100)    USING "&&&&&&&&&&&&&&&"      --(13,02) MONTO_APORTACION EN his_devolucion    --CHAR(13 )  --Monto de Aportaciones 5%
      --LET v_arch_detalle.id_27 = (v_r_sol_sdos.aivs_aportacion * 1000000) USING "&&&&&&&&&&&&&&&"    --(09,06) AIVS_APORTACION EN his_devolucion    --CHAR(09 )  --Aplicación de Intereses de Aportaciones 5%
      --LET v_arch_detalle.id_28 = ""
      --LET v_arch_detalle.id_29 = (v_r_sol_sdos.aivs92 * 1000000) USING "&&&&&&&&&&&&&&&"   --(09,06) SUM(AIVS92) his_devolucion           --CHAR(09 )  --Número de "Aplicaciones de Intereses de Vivienda" 92
      --LET v_arch_detalle.id_30 = (v_r_sol_sdos.pesos92 * 100)    USING "&&&&&&&&&&&&&&&"  --(13,02) SUM(PESOS92) his_devolucion          --CHAR(13 )  --Saldo de Vivienda 92
      --LET v_arch_detalle.id_31 = ""
      LET v_arch_detalle.id_32 = ""
      LET v_arch_detalle.id_33 = ""
      LET v_arch_detalle.id_34 = v_r_sol_sdos.nom_imss
      LET v_arch_detalle.id_35 = v_r_sol_sdos.num_credito USING "&&&&&&&&&&"
      LET v_arch_detalle.id_36 = "000000000000000"
      --LET v_arch_detalle.id_37 = "000000000000000"
      LET v_arch_detalle.id_38 = ""

      --Se concatena el detalle
      LET v_string_detalle = v_arch_detalle.id_01
                            ,v_arch_detalle.id_02
                            ,v_arch_detalle.id_03
                            ,v_arch_detalle.id_04
                            ,v_arch_detalle.id_05
                            ,v_arch_detalle.id_06
                            ,v_arch_detalle.id_07
                            ,v_arch_detalle.id_08
                            ,v_arch_detalle.id_09
                            ,v_arch_detalle.id_10
                            ,v_arch_detalle.id_11
                            ,v_arch_detalle.id_12
                            --,v_arch_detalle.id_13
                            --,v_arch_detalle.id_14
                            --,v_arch_detalle.id_15
                            ,v_arch_detalle.id_16
                            ,v_arch_detalle.id_17
                            ,v_arch_detalle.id_18
                            ,v_arch_detalle.id_19
                            ,v_arch_detalle.id_20
                            ,v_arch_detalle.id_21
                            ,v_arch_detalle.id_22
                            ,v_arch_detalle.id_23
                            ,v_arch_detalle.id_24
                            --,v_arch_detalle.id_25
                            --,v_arch_detalle.id_26
                            --,v_arch_detalle.id_27
                            --,v_arch_detalle.id_28
                            --,v_arch_detalle.id_29
                            --,v_arch_detalle.id_30
                            --,v_arch_detalle.id_31
                            ,v_arch_detalle.id_32
                            ,v_arch_detalle.id_33
                            ,v_arch_detalle.id_34
                            ,v_arch_detalle.id_35
                            ,v_arch_detalle.id_36
                            --,v_arch_detalle.id_37
                            ,v_arch_detalle.id_38

      LET v_indice       = v_indice + 1
      LET v_suma_id_22   = v_suma_id_22 + v_arch_detalle.id_22
      LET v_suma_id_23   = v_suma_id_23 + v_arch_detalle.id_23
      --LET v_suma_id_29 = v_suma_id_29 + v_arch_detalle.id_29
      --LET v_suma_id_30 = v_suma_id_30 + v_arch_detalle.id_30

      --Se escribe el detalle en el archivo de salida
      CALL v_archivo_sol_devolucion.WRITE([v_string_detalle])
   END FOREACH

   -- se actualiza el contador de registros
   LET v_indice = v_indice - 1

   --Inicia el sumario del archivo
   LET v_arch_sumario.id_01 = "09"
   LET v_arch_sumario.id_02 = v_indice USING "&&&&&&&&&"
   LET v_arch_sumario.id_03 = ""
   --LET v_arch_sumario.id_04 = (v_suma_id_22 * 1000000) USING "&&&&&&&&&&&&&&&&&&"
   --LET v_arch_sumario.id_05 = (v_suma_id_23 * 100)     USING "&&&&&&&&&&&&&&&"
   LET v_arch_sumario.id_04 = (v_suma_id_22) USING "&&&&&&&&&&&&&&&&&&"
   LET v_arch_sumario.id_05 = (v_suma_id_23) USING "&&&&&&&&&&&&&&&"
   LET v_arch_sumario.id_06 = ""
   --LET v_arch_sumario.id_07 = (v_suma_id_29 * 1000000) USING "&&&&&&&&&&&&&&&&&&"
   --LET v_arch_sumario.id_08 = (v_suma_id_30 * 100)     USING "&&&&&&&&&&&&&&&"
   LET v_arch_sumario.id_09 = "000000000000000"
   --LET v_arch_sumario.id_10 = "000000000000000"
   LET v_arch_sumario.id_11 = ""

   --Se concatena el sumario
   LET v_string_sumario = v_arch_sumario.id_01
                         ,v_arch_sumario.id_02
                         ,v_arch_sumario.id_03
                         ,v_arch_sumario.id_04
                         ,v_arch_sumario.id_05
                         ,v_arch_sumario.id_06
                         --,v_arch_sumario.id_07
                         --,v_arch_sumario.id_08
                         ,v_arch_sumario.id_09
                         --,v_arch_sumario.id_10
                         ,v_arch_sumario.id_11

   --Se escribe el detalle en el archivo de salida
   CALL v_archivo_sol_devolucion.WRITE([v_string_sumario])
   
   -- se cierra el archivo
   CALL v_archivo_sol_devolucion.CLOSE()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_s_ruta_archivo_cp = v_ruta_envio CLIPPED || "/" || "sol_dev_ug." || v_c_extension

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_s_ruta_archivo, " ", v_s_ruta_archivo_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

  {
   DISPLAY ""
   DISPLAY " Ejecutando envío interfaz para Procesar"

   LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/sol_dev_ug.sh"
   RUN v_ejecuta_sh
  }

   DISPLAY ""

   -- se obtiene el maximo lote para la fecha de presentación
   LET v_query = " SELECT MAX(lote)\n",
                 "   FROM dse_ctr_archivo\n",
                 "  WHERE tpo_transferencia = '",g_tpo_transferencia,"'\n",
                 "    AND f_lote = '",p_f_presentacion,"'"

   PREPARE prp_max_lote FROM v_query
   EXECUTE prp_max_lote INTO v_i_lote

   -- se valida el lote
   IF v_i_lote IS NULL THEN
      LET v_i_lote = 1
   ELSE
      LET v_i_lote = v_i_lote + 1
   END IF

   --agregar nuevos campos de la tabla cre ctr archivo
   LET v_r_dse_ctr_archivo.tpo_transferencia  = g_tpo_transferencia
   LET v_r_dse_ctr_archivo.lote               = v_i_lote
   LET v_r_dse_ctr_archivo.f_lote             = p_f_presentacion
   LET v_r_dse_ctr_archivo.tot_registros      = v_indice
   LET v_r_dse_ctr_archivo.tot_aceptados      = 0
   LET v_r_dse_ctr_archivo.tot_rechazados     = 0
   LET v_r_dse_ctr_archivo.estado             = 20
   LET v_r_dse_ctr_archivo.f_proceso          = TODAY
   LET v_r_dse_ctr_archivo.usuario            = p_usuario_cod
   LET v_r_dse_ctr_archivo.folio              = g_num_folio
   LET v_r_dse_ctr_archivo.nom_archivo        = p_c_nom_archivo

   -- se inserta el registro en la tabla de control
   INSERT INTO dse_ctr_archivo VALUES (v_r_dse_ctr_archivo.tpo_transferencia,
                                       v_r_dse_ctr_archivo.lote,
                                       v_r_dse_ctr_archivo.f_lote,
                                       v_r_dse_ctr_archivo.tot_registros,
                                       v_r_dse_ctr_archivo.tot_aceptados,
                                       v_r_dse_ctr_archivo.tot_rechazados,
                                       v_r_dse_ctr_archivo.estado,
                                       v_r_dse_ctr_archivo.f_proceso,
                                       v_r_dse_ctr_archivo.usuario,
                                       v_r_dse_ctr_archivo.folio,
                                       v_r_dse_ctr_archivo.nom_archivo)

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING v_c_ruta_bin, v_c_ruta_listados

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("GRTS041.4rp") THEN
      CALL fgl_report_setOutputFileName(v_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
      LET r_b_valida = fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      RETURN
   END IF

   LET v_suma_aivs97  = v_arch_sumario.id_04 / 1000000
   LET v_suma_saldo97 = v_arch_sumario.id_05 / 100

   --Se obtienen los rregistros 20-envío , 70 reenvío
   --envío
   LET v_s_comando = "SELECT COUNT(*)
                        FROM safre_tmp:tmp_rep_devolucion_grt
                       WHERE edo_procesar = 20"
                                                   
   PREPARE prp_devo FROM v_s_comando
   EXECUTE prp_devo INTO v_cnt_envio

   -- reenvio
   LET v_s_comando = "SELECT COUNT(*)
                        FROM safre_tmp:tmp_rep_devolucion_grt
                       WHERE edo_procesar = 70"

   PREPARE prp_reenvio FROM v_s_comando
   EXECUTE prp_reenvio INTO v_cnt_reenvio


   LET v_s_comando = "SELECT SUM(aivs97)  ,
                             SUM(pesos97) ,
                             SUM(aivs92)  ,
                             SUM(pesos92)
                        FROM safre_tmp:tmp_rep_devolucion_grt
                       WHERE edo_procesar = 20"

   PREPARE prp_tot_envio FROM v_s_comando
   EXECUTE prp_tot_envio INTO v_envio_aivs97,
                              v_envio_saldo97,
                              v_envio_aivs92,
                              v_envio_saldo92

   LET v_s_comando = "SELECT SUM(aivs97)  ,
                             SUM(pesos97) ,
                             SUM(aivs92)  ,
                             SUM(pesos92)
                        FROM safre_tmp:tmp_rep_devolucion_grt
                       WHERE edo_procesar = 70"

   PREPARE prp_tot_reenvio FROM v_s_comando
   EXECUTE prp_tot_reenvio INTO v_reenvio_aivs97,
                                v_reenvio_saldo97,
                                v_reenvio_aivs92,
                                v_reenvio_saldo92


   --Se genera el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- salida de reporte
   OUTPUT TO REPORT reporte_archivo_salida(p_usuario_cod,
                                           p_c_nom_archivo,
                                           v_suma_aivs97,
                                           v_suma_saldo97,
                                           v_indice,
                                           v_cnt_envio,
                                           v_cnt_reenvio,
                                           v_envio_aivs97,
                                           v_envio_saldo97,
                                           v_envio_aivs92,
                                           v_envio_saldo92,
                                           v_reenvio_aivs97,
                                           v_reenvio_saldo97,
                                           v_reenvio_aivs92,
                                           v_reenvio_saldo92)

   -- finaliza el reporte
   FINISH REPORT reporte_archivo_salida

END FUNCTION

#OBJETIVO: Genera el reporte de Rechazos
REPORT reporte_archivo_salida(p_v_usuario,
                              p_v_arch_proceso,
                              p_suma_aivs97,
                              p_suma_saldo97,
                              p_count_reg,
                              p_cnt_envio,
                              p_cnt_reenvio,
                              p_envio_aivs97,
                              p_envio_saldo97,
                              p_envio_aivs92,
                              p_envio_saldo92,
                              p_reenvio_aivs97,
                              p_reenvio_saldo97,
                              p_reenvio_aivs92,
                              p_reenvio_saldo92)

   DEFINE p_v_usuario               LIKE seg_usuario.usuario_cod
   DEFINE v_fecha_reporte           DATE
   DEFINE v_fecha_present           LIKE dis_sum_avance_pago.f_presentacion
   DEFINE p_count_reg               INTEGER
   DEFINE p_suma_aivs97             DECIMAL(22,6)
   DEFINE p_suma_saldo97            DECIMAL(22,2)
   DEFINE p_v_arch_proceso          CHAR(100)
   DEFINE p_cnt_envio               SMALLINT
   DEFINE p_cnt_reenvio             SMALLINT
   DEFINE p_envio_aivs97            DECIMAL(22,6)
   DEFINE p_envio_saldo97           DECIMAL(22,2)
   DEFINE p_envio_aivs92            DECIMAL(22,6)
   DEFINE p_envio_saldo92           DECIMAL(22,2)
   DEFINE p_reenvio_aivs97          DECIMAL(22,6)
   DEFINE p_reenvio_saldo97         DECIMAL(22,2)
   DEFINE p_reenvio_aivs92          DECIMAL(22,6)
   DEFINE p_reenvio_saldo92         DECIMAL(22,2)

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      LET v_fecha_present = v_fecha_reporte

      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX g_num_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX p_v_usuario
      PRINTX p_suma_aivs97
      PRINTX p_suma_saldo97
      PRINTX p_count_reg
      PRINTX p_v_arch_proceso

      --Se imprimen los totales de envio y reenvio
      PRINTX p_cnt_envio
      PRINTX p_cnt_reenvio

      PRINTX p_envio_aivs97
      PRINTX p_envio_saldo97
      PRINTX p_reenvio_aivs97
      PRINTX p_reenvio_saldo97

END REPORT

FUNCTION fn_cre_tbl_temp_SolicDev()
   -- se declara la base de datos temporal
   DEFINE v_sql_crea_dse_grt STRING
   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   LET v_sql_crea_dse_grt = "EXECUTE PROCEDURE sp_crea_tmp_dse_grt()"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_sp_crea_dse_grt FROM v_sql_crea_dse_grt
   EXECUTE prp_sp_crea_dse_grt
   
   WHENEVER ERROR STOP

   -- regresa a la base de datoa safre viv
   DATABASE safre_viv

END FUNCTION
