--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

################################################################################
#Módulo             => ACR                                                     #
#Programa           => ACRS14                                                  #
#Objetivo           => Programa para generar el archivo de solicitudes de      #
#                      Devolución de Saldos Excedentes ACR                     #
#Fecha Inicio       => 24 ENERO 2012                                           #
#Autor:             => Franciso López, EFP                                     #
#Actualización      => Fecha de envío cambia a 12 día hábil                    #
#Fecha actaliz      => 29 de octubre de 2014                                   #
#Modifica:          => Eduardo Ventura Bonola                                  #
#Fecha modif:       => 12 de noviembre de 2015                                 #
#Adecuación         => Generación archivo "agrupación de la agrupación"        #
#                      y registros sin afectación a la cuenta individual       #
################################################################################

DATABASE safre_viv

   DEFINE g_pid                    LIKE bat_ctr_proceso.pid     --  ID del proceso
   DEFINE g_proceso_cod            LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod              LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE g_usuario_cod            LIKE seg_usuario.usuario_cod -- Clave de usuario
   DEFINE g_tpo_transferencia      LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia
   DEFINE g_num_folio              DECIMAL(9,0)
   DEFINE g_nom_archivo            LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_ejecuta_sh             STRING


#Objetivo:
MAIN

   DEFINE v_estatus                SMALLINT
   DEFINE v_id_lote                DECIMAL(10,0)
   DEFINE v_lote                   SMALLINT
   DEFINE v_f_lote                 DATE
   DEFINE v_f_presentacion         DATE -- fecha de presentacion, today
   DEFINE v_f_movimiento           DATE -- fecha de movimiento, 12 día hábil del mes de envío
   DEFINE v_id_lote_dev            CHAR(16)
   DEFINE v_s_titulo_correo        STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo       STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_mens_correo          STRING -- contiene el cuerpo del correo
   DEFINE v_v_nom_reporte          VARCHAR(80) -- nombre del reporte
   DEFINE v_c_programa_cod         LIKE cat_operacion.programa_cod -- nombre del programa origen
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados -- ruta listados
   DEFINE v_ruta_bin               LIKE seg_modulo.ruta_bin 

   #Si se ha recibido parámetros se continua
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)  --numero de proceso
   LET g_num_folio      = ARG_VAL(5)
   LET g_nom_archivo    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".ACRS14.log")

   DISPLAY "=INICIA ACRS14="
   DISPLAY " USUARIO       : ",g_usuario_cod
   DISPLAY " PID           : ",g_pid
   DISPLAY " ARCHIVO       : ",g_nom_archivo

   -- se inicializan variables
   LET g_tpo_transferencia = "51" -- tipo de transferencia de Solicitud DSE
   LET v_id_lote           = 02 -- ya que es solo un archivo el lote siempre es 1
   LET v_lote              = 1  -- ya que es solo un archivo el lote siempre es 1
   LET v_f_lote            = TODAY

   -- se genera el folio
   LET g_num_folio = fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario_cod)
   DISPLAY " FOLIO         : ",g_num_folio USING "#########&"

   -- se obtiene el catorceavo dia habil del mes actual
   LET v_f_presentacion = v_f_lote - DAY(v_f_lote) + 1
   PREPARE prp_obtiene_habil FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_f_presentacion||"',11)"
   EXECUTE prp_obtiene_habil INTO v_f_presentacion

  {
   -- se valida que la fecha de presentación sea mayor o igual que HOY
   IF v_f_presentacion < v_f_lote THEN
      -- se obtiene el catorceavo dia habil del mes siguiente
      LET v_f_presentacion = v_f_lote - DAY(v_f_lote) + 1
      LET v_f_presentacion = v_f_presentacion + 1 UNITS MONTH

      PREPARE prp_obtiene_habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_f_presentacion||"',11)"
      EXECUTE prp_obtiene_habil_sig INTO v_f_presentacion
   END IF
  }

   -- Se contruye la fecha movimiento: Primer dia natural del mes siguiente a la fecha de presentación
   LET v_f_movimiento = v_f_presentacion - DAY(v_f_presentacion) + 1
   LET v_f_movimiento = v_f_movimiento + 1 UNITS MONTH

   DISPLAY " Fecha presentación: ",v_f_presentacion USING "dd/mm/yyyy"
   DISPLAY " Fecha movimiento  : ",v_f_movimiento USING "dd/mm/yyyy"

   -- Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("acr") RETURNING v_ruta_bin, v_ruta_listados

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_proceso_cod , g_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = g_usuario_cod CLIPPED, "-",v_c_programa_cod CLIPPED,"-", g_pid USING "&&&&&", "-", g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"

   --Se asigna la fecha con formato AAAAMMDD
   LET v_id_lote_dev = '04002',YEAR(v_f_lote)  USING "&&&&",MONTH(v_f_lote) USING "&&",
                       DAY(v_f_lote) USING "&&",'001'

   DISPLAY " Se crea tabla temporal"
   CALL fn_cre_tbl_temp_SolicDev()

   DISPLAY " Ejecuta identificación devolución sdos exc "
   #Llamada a ejecución de procedimiento almacenado
   CALL fn_sp_acr_solicitud_devolucion(v_id_lote, v_lote, v_f_lote, v_f_presentacion, v_f_movimiento, v_id_lote_dev)

   DISPLAY " Se crea índice temporal"
   CALL fn_cre_idx_temp_SolicDev()

   DISPLAY " Ejecuta solicitud devolución sdos exc "
   #Llamada a ejecución de procedimiento almacenado
   CALL fn_sp_acr_solicitud_grp_devol()

   DISPLAY " Se genera reporte"
   --Se genera el reporte de las solicitudes de las devoluciones
   CALL fn_reporte_archivo_solicitud_devolucion(v_id_lote, v_lote, v_f_lote, v_f_presentacion, v_f_movimiento, v_id_lote_dev, v_v_nom_reporte)

   --Se actualiza el proceso como finalizado
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estatus

   -- se verifica si fue posible finalizar la operacion
   IF v_estatus <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(v_estatus)

      --EXIT PROGRAM
   END IF

   DISPLAY " Envía correo del reporte"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: SOLICITUD DE DEVOLUCIÓN DE SALDOS EXCEDENTES ACREDITADOS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",g_pid,"\n",
                          "Proceso      : DEVOLUCIÓN DE SALDOS EXCEDENTES TA\n",
                          "Operación    : SOLICITUD DE DEVOLUCIÓN\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo) 

   DISPLAY "=FIN="

END MAIN

#Objetivo: Ejecuta el Store procedure para la incializacion de la solicitud de las devoluciones
FUNCTION fn_sp_acr_solicitud_devolucion(p_id_lote, p_lote, p_f_lote, p_f_presentacion, p_f_movimiento, p_id_lote_dev)

   DEFINE p_id_lote               DECIMAL(10,0)
   DEFINE p_lote                  SMALLINT
   DEFINE p_f_lote                DATE
   DEFINE p_f_presentacion        DATE
   DEFINE p_f_movimiento          DATE
   DEFINE p_id_lote_dev           CHAR(16)
   DEFINE v_sql_procedure         STRING

   LET v_sql_procedure = "EXECUTE PROCEDURE sp_acr_solicitud_devolucion(?,?,?,?,?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_sp_solicitud_devolucion FROM v_sql_procedure
   EXECUTE prp_sp_solicitud_devolucion USING p_id_lote,
                                                 p_lote,
                                                 p_f_lote,
                                                 p_f_presentacion,
                                                 p_f_movimiento,
                                                 p_id_lote_dev,
                                                 g_usuario_cod

END FUNCTION

#Objetivo: Ejecuta el Store procedure para la incializacion de la solicitud de las devoluciones
FUNCTION fn_sp_acr_solicitud_grp_devol()

   DEFINE v_sql_procedure         STRING

   LET v_sql_procedure = "EXECUTE PROCEDURE sp_acr_solicitud_grp_devol()"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_sp_solicitud_grp_dev FROM v_sql_procedure
   EXECUTE prp_sp_solicitud_grp_dev

END FUNCTION

#Objetivo: Ejecuta el Store procedure para la incializacion de la solicitud de las devoluciones
FUNCTION fn_reporte_archivo_solicitud_devolucion(p_id_lote,
                                                 p_lote,
                                                 p_f_lote,
                                                 p_f_presentacion,
                                                 p_f_movimiento,
                                                 p_id_lote_dev,
                                                 p_v_nom_reporte)

   DEFINE p_id_lote                DECIMAL(10,0)
   DEFINE p_lote                   SMALLINT
   DEFINE p_f_lote                 DATE
   DEFINE p_f_presentacion         DATE
   DEFINE p_f_movimiento           DATE
   DEFINE p_id_lote_dev            CHAR(16)
   DEFINE p_v_nom_reporte          VARCHAR(80) -- nombre del reporte
   DEFINE v_c_f_presentacion       CHAR(8) -- fecha de presentación con formato "yyyymmdd"
   DEFINE v_c_f_movimiento         CHAR(8) -- fecha de movimiento con formato "yyyymmdd"

    --Variables para la genracion de los archivos
   DEFINE v_string_encabezado      STRING
   DEFINE v_string_detalle         STRING
   DEFINE v_string_sumario         STRING
   DEFINE v_indice                 INTEGER

    --Se definen las variables para el archivo físico
   DEFINE v_archivo_sol_devolucion BASE.CHANNEL
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   DEFINE v_s_ruta_archivo         STRING
   DEFINE v_s_ruta_archivo_cp      STRING
   DEFINE v_c_extension            LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_query                  STRING
   DEFINE v_s_comando              STRING -- contiene al comando a correr

          --VARIABLES para el reporte
   DEFINE v_manejador_rpt          om.SaxDocumentHandler -- Contenedor de Documentos para el reporte

          --Datos de la tabla temporal tmp_rep_devolucion
   DEFINE v_r_sol_sdos RECORD
      curp                         CHAR(18),
      nss                          CHAR(11),
      rfc                          CHAR(13),
      ap_paterno_af                CHAR(40),
      ap_materno_af                CHAR(40),
      nombre_af                    CHAR(40),
      aivs97                       DECIMAL(22,2),
      pesos97                      DECIMAL(22,2),
      nss_separacion               CHAR(11),
      origen_devolucion            CHAR(2),
      monto_aportacion             DECIMAL(22,2),
      aivs_aportacion              DECIMAL(22,2),
      aivs92                       DECIMAL(22,2),
      pesos92                      DECIMAL(22,2),
      nom_imss                     CHAR(50),
      num_credito                  DECIMAL(10,0)
   END RECORD

   DEFINE v_arch_encabezado RECORD
      tpo_reg                      CHAR(02),  -- Tipo de Registro (01-02)
      id_serv                      CHAR(02),  -- Identificador de Servicio (03-04)
      id_operacion                 CHAR(02),  -- Identificador de Operación (05-06)
      tpo_ent_origen               CHAR(02),  -- Tipo de entidad origen (07-08)
      cve_ent_origen               CHAR(03),  -- Clave de entidad origen (09-11)
      tpo_ent_destino              CHAR(02),  -- Tipo de entidad destino (12-13)
      cve_ent_destino              CHAR(03),  -- Clave de entidad destino (14-16)
      ent_fed_envio                CHAR(03),  -- Entidad federativa de envío de lote (17-19)
      fec_presentacion             CHAR(08),  -- Fecha de presentación (20-27)
      cons_lote_dia                CHAR(03),  -- Consecutivo del lote en el día (28-30)
      cve_modal_recep              CHAR(02),  -- Clave de modalidad de recepción (31-32)
      cod_result_op                CHAR(02),  -- Código de resultado de la Operación (33-34)
      mot_rech_lote                CHAR(09),  -- Motivo de rechazo del lote (35-43)
      filler                       CHAR(687)  -- Filler (44-730)
   END RECORD

  --Arreglos con los datos del archivo
   DEFINE v_arch_detalle RECORD
      tpo_reg                      CHAR(02), --Tipo de Registro (1-2)
      cont_serv                    CHAR(10), --Contador de Servicio (3-12)
      tpo_ent_recep                CHAR(02), --Tipo de entidad receptora de la cuenta (13-14)
      cve_ent_recep                CHAR(03), --Clave de entidad receptora de la cuenta (15-17)
      tpo_ent_ceden                CHAR(02), --Tipo de entidad cedente de la cuenta (18-19)
      cve_ent_ceden                CHAR(03), --Clave de entidad ced. de la cuenta (20-22)
      filler1                      CHAR(02), --Filler (23-24)
      fec_present                  CHAR(08), --Fecha de presentación (25-32)
      fec_movimiento               CHAR(08), --Fecha de movimiento (33-40)
      curp_trab                    CHAR(18), --CURP del trabajador (41-58)
      nss_trab                     CHAR(11), --NSS del trabajador (59-69)
      filler2                      CHAR(13), --Filler (70-82)
      id_asignac                   CHAR(01), --Identificador de Asignación (83-83)
      tpo_devoluc                  CHAR(01), --Tipo de devolución (84-84)
      rfc_trab_info                CHAR(13), --RFC del Trabajador según Infonavit (85-97)
      ap_pat_trab_afore            CHAR(40), --Apellido paterno del trabajador en AFORE receptora (98-137)
      ap_mat_trab_afore            CHAR(40), --Apellido materno del trabajador en AFORE receptora (138-177)
      nom_trab_afore               CHAR(40), --Nombres del trabajador en AFORE receptora (178-217)
      filler3                      CHAR(22), --Filler (218-239)
      id_lote_devol                CHAR(16), --Identificador de lote de la devolución (240-255)
      filler4                      CHAR(219), --Filler (256-474)
      aivs97                       CHAR(15), --Número de "Aplicaciones de Intereses de Vivienda" 97 (475-489)
      pesos97                      CHAR(15), --Saldo de vivienda 97 (490-504)
      nss_separacion               CHAR(11), --NSS por separacón de cuentas (505-515)
      origen_devol                 CHAR(02), --origen de la devolución (516-517)
      mto_aportacion               CHAR(15), --Monto de Aportaciones 5% (518-532)
      apl_int_aport                CHAR(15), --Aplicación de Intereses de Aportaciones 5% (533-547)
      filler5                      CHAR(02), --Filler (548-549)
      aivs92                       CHAR(15), --Número de "Aplicaciones de Intereses de Vivienda" 92 (550-564)
      pesos92                      CHAR(15), --Saldo de Vivienda 92 (565-579)
      filler6                      CHAR(03), --Filler (580-582)
      cod_resul_op                 CHAR(02), --Código Resultado de la Operación (583-584)
      diagn_proc                   CHAR(15), --Diagnóstico del proceso (585-599)
      nom_imss                     CHAR(50), --Nombre del Trabajador según IMSS (600-649)
      num_cred_infonavit           CHAR(10), --Número de Crédito INFONAVIT (650-659)
      int_sdo_viv97                CHAR(15), --Intereses Saldo  de Vivienda 97 (660-674)
      int_sdo_viv92                CHAR(15), --Intereses Saldo de Vivienda 92 (675-689)
      filler7                      CHAR(41)  --Filler (690-730)
   END RECORD

   DEFINE v_arch_sumario RECORD
      tpo_reg                      CHAR(2),  -- Tipo de Registro
      tot_regs_det                 CHAR(9),  -- Cantidad de Registros de Detalle
      filler1                      CHAR(30), -- Filler
      sum_aivs97                   CHAR(18), -- Suma de número de "Aplicaciones de Intereses de Vivienda" 97
      sum_pesos97                  CHAR(15), -- Suma del Saldo de Vivienda 97
      filler2                      CHAR(45), -- Filler
      sum_aivs92                   CHAR(18), -- Suma de número de "Aplicaciones de Intereses de Vivienda" 92
      sum_pesos92                  CHAR(15), -- Suma del Saldo de Vivienda 92
      int_viv97                    CHAR(15), -- Intereses Vivienda 97
      int_viv92                    CHAR(15), -- Intereses Vivienda 92
      filler3                      CHAR(548) -- Filler
   END RECORD

   DEFINE v_precio_fondo           LIKE glo_valor_fondo.precio_fondo -- precio de accion
   DEFINE v_suma_aivs97            DECIMAL(22,6)
   DEFINE v_suma_pesos97           DECIMAL(22,2)
   DEFINE v_suma_aivs92            DECIMAL(22,6)
   DEFINE v_suma_pesos92           DECIMAL(22,2)
   DEFINE v_i_lote                 LIKE dse_ctr_archivo.lote -- lote del archivo
   DEFINE v_r_dse_ctr_archivo      RECORD LIKE dse_ctr_archivo.* -- registro de la tabla de control
   DEFINE v_c_ruta_bin             LIKE seg_modulo.ruta_bin -- ruta bin
   DEFINE v_c_ruta_listados        LIKE seg_modulo.ruta_listados -- ruta listados
   DEFINE r_b_valida               SMALLINT -- estatus del proceso
   DEFINE v_cnt_envio              INTEGER
   DEFINE v_cnt_reenvio            INTEGER
   DEFINE v_envio_aivs92           DECIMAL(22,6)
   DEFINE v_envio_saldo92          DECIMAL(22,2)
   DEFINE v_envio_aivs97           DECIMAL(22,6)
   DEFINE v_envio_saldo97          DECIMAL(22,2)
   DEFINE v_reenvio_aivs92         DECIMAL(22,6)
   DEFINE v_reenvio_saldo92        DECIMAL(22,2)
   DEFINE v_reenvio_aivs97         DECIMAL(22,6)
   DEFINE v_reenvio_saldo97        DECIMAL(22,2)


   DISPLAY " INICIA REPORTE"
   --Se obtiene las rutas para la generacion del archivo
      -- se obtienen la ruta envio del modulo
   LET v_query = " SELECT ruta_envio\n",
                 "   FROM seg_modulo\n",
                 "  WHERE modulo_cod = 'acr' "

   PREPARE prp_ruta_archivo FROM v_query
   EXECUTE prp_ruta_archivo INTO v_ruta_envio

   --Se obtiene las rutas para la generacion del archivo
      -- se obtienen la ruta envio del modulo
   LET v_query = " SELECT precio_fondo\n",
                 "   FROM glo_valor_fondo\n",
                 "  WHERE fondo = 11\n",
                 "    AND f_valuacion = '",p_f_movimiento,"'"

   PREPARE prp_precio_accion FROM v_query
   EXECUTE prp_precio_accion INTO v_precio_fondo

   DISPLAY " RUTA ENVIO: ",v_ruta_envio
   -- se concatena el nombre del archivo con la ruta de envio
   LET v_s_ruta_archivo = v_ruta_envio CLIPPED,"/",g_nom_archivo CLIPPED

   -- se crea el manejador de archivo
   LET v_archivo_sol_devolucion = BASE.CHANNEL.CREATE()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo_sol_devolucion.openFile(v_s_ruta_archivo, "w")
   CALL v_archivo_sol_devolucion.setDelimiter("")

   -- se asigna la fecha de presentación con formato AAAAMMDD
   LET v_c_f_presentacion = p_f_presentacion USING "yyyymmdd"
   LET v_c_f_movimiento = p_f_movimiento USING "yyyymmdd"

   --Se asignan los valores del encabezado
   LET v_arch_encabezado.tpo_reg          = "01"  --CHAR(02 )   --Tipo de Registro
   LET v_arch_encabezado.id_serv          = "02"  --CHAR(02 )   --Identificador de Servicio
   LET v_arch_encabezado.id_operacion     = "15"  --CHAR(02 )   --Identificador de Operación
   LET v_arch_encabezado.tpo_ent_origen   = "04"  --CHAR(02 )   --Tipo de entidad origen
   LET v_arch_encabezado.cve_ent_origen   = "002" --CHAR(03 )   --Clave de entidad origen
   LET v_arch_encabezado.tpo_ent_destino  = "01"  --CHAR(02 )   --Tipo de entidad destino
   LET v_arch_encabezado.cve_ent_destino  = ""    --CHAR(03 )   --Clave de entidad destino
   LET v_arch_encabezado.ent_fed_envio    = "009" --CHAR(03 )   --Entidad federativa de envío de lote
   LET v_arch_encabezado.fec_presentacion = v_c_f_presentacion
   LET v_arch_encabezado.cons_lote_dia    = "001"  --CHAR(03 )   --Consecutivo del lote en el día
   LET v_arch_encabezado.cve_modal_recep  = "02"   --"" --"02"    --CHAR(02 )   --Clave de modalidad de recepción
   LET v_arch_encabezado.cod_result_op    = ""     --CHAR(02 )   --Código de resultado de la Operación
   LET v_arch_encabezado.mot_rech_lote    = ""     --CHAR(03x3)  --Motivo de rechazo del lote
   LET v_arch_encabezado.filler           = ""     --CHAR(687)   --Filler

   --Se concatena el encabezado
   LET v_string_encabezado = v_arch_encabezado.tpo_reg,
                             v_arch_encabezado.id_serv,
                             v_arch_encabezado.id_operacion,
                             v_arch_encabezado.tpo_ent_origen,
                             v_arch_encabezado.cve_ent_origen,
                             v_arch_encabezado.tpo_ent_destino,
                             v_arch_encabezado.cve_ent_destino,
                             v_arch_encabezado.ent_fed_envio,
                             v_arch_encabezado.fec_presentacion,
                             v_arch_encabezado.cons_lote_dia,
                             v_arch_encabezado.cve_modal_recep,
                             v_arch_encabezado.cod_result_op,
                             v_arch_encabezado.mot_rech_lote,
                             v_arch_encabezado.filler

   --Se ingresa el encabezado en el archivo
   CALL v_archivo_sol_devolucion.WRITE([v_string_encabezado])

   LET v_indice       = 1
   LET v_suma_aivs97  = 0
   LET v_suma_pesos97 = 0
   LET v_suma_aivs92  = 0
   LET v_suma_pesos92 = 0

   LET v_query = " SELECT * FROM safre_tmp:tmp_rep_devolucion "

   PREPARE prp_rep_selicitud_pago FROM v_query
   DECLARE cur_rep_selicitud_pago CURSOR FOR prp_rep_selicitud_pago

   --Se inicia el foreach para la lectura de los registros de la tabla temporal
   FOREACH cur_rep_selicitud_pago INTO v_r_sol_sdos.*
      -- se validan los importes obtenidos
      IF v_r_sol_sdos.pesos92 IS NULL THEN
         LET v_r_sol_sdos.pesos92 = 0
      END IF

      IF v_r_sol_sdos.pesos97 IS NULL THEN
         LET v_r_sol_sdos.pesos97 = 0
      END IF

      IF(v_r_sol_sdos.aivs_aportacion IS NULL) THEN
         LET v_r_sol_sdos.aivs_aportacion = 0 
      END IF

      LET v_r_sol_sdos.aivs92 = 0
      LET v_r_sol_sdos.aivs97 = 0

      LET v_arch_detalle.tpo_reg            = "02"
      LET v_arch_detalle.cont_serv          = v_indice USING "&&&&&&&&&&"
      LET v_arch_detalle.tpo_ent_recep      = "01"
      LET v_arch_detalle.cve_ent_recep      = ""
      LET v_arch_detalle.tpo_ent_ceden      = "04"
      LET v_arch_detalle.cve_ent_ceden      = "002"
      LET v_arch_detalle.filler1            = ""
      LET v_arch_detalle.fec_present        = v_c_f_presentacion
      LET v_arch_detalle.fec_movimiento     = v_c_f_movimiento
      LET v_arch_detalle.curp_trab          = v_r_sol_sdos.curp
      LET v_arch_detalle.nss_trab           = v_r_sol_sdos.nss
      LET v_arch_detalle.filler2            = ""
      LET v_arch_detalle.id_asignac         = ""
      LET v_arch_detalle.tpo_devoluc        = " "
      LET v_arch_detalle.rfc_trab_info      = v_r_sol_sdos.rfc
      LET v_arch_detalle.ap_pat_trab_afore  = v_r_sol_sdos.ap_paterno_af
      LET v_arch_detalle.ap_mat_trab_afore  = v_r_sol_sdos.ap_materno_af
      LET v_arch_detalle.nom_trab_afore     = v_r_sol_sdos.nombre_af
      LET v_arch_detalle.filler3            = ""           
      LET v_arch_detalle.id_lote_devol      = p_id_lote_dev
      LET v_arch_detalle.filler4            = ""           
      LET v_arch_detalle.aivs97             = (v_r_sol_sdos.aivs97 * 1000000) USING "&&&&&&&&&&&&&&&" --SUM(AIVS97) his_devolucion           --(09,06) Número de "Aplicaciones de Intereses de Vivienda" 97
      LET v_arch_detalle.pesos97            = (v_r_sol_sdos.pesos97 * 100)    USING "&&&&&&&&&&&&&&&" --SUM(PESOS97) his_devolucion          --(13,02) Saldo de vivienda 97                                
      LET v_arch_detalle.nss_separacion     = v_r_sol_sdos.nss_separacion   
      LET v_arch_detalle.origen_devol       = v_r_sol_sdos.origen_devolucion
      LET v_arch_detalle.mto_aportacion     = (v_r_sol_sdos.monto_aportacion * 100)    USING "&&&&&&&&&&&&&&&"      --(13,02) MONTO_APORTACION EN his_devolucion    --CHAR(13 )  --Monto de Aportaciones 5%                            
      LET v_arch_detalle.apl_int_aport      = (v_r_sol_sdos.aivs_aportacion * 1000000) USING "&&&&&&&&&&&&&&&"    --(09,06) AIVS_APORTACION EN his_devolucion    --CHAR(09 )  --Aplicación de Intereses de Aportaciones 5%          
      LET v_arch_detalle.filler5            = ""
      LET v_arch_detalle.aivs92             = (v_r_sol_sdos.aivs92 * 1000000) USING "&&&&&&&&&&&&&&&"   --(09,06) SUM(AIVS92) his_devolucion           --CHAR(09 )  --Número de "Aplicaciones de Intereses de Vivienda" 92
      LET v_arch_detalle.pesos92            = (v_r_sol_sdos.pesos92 * 100)    USING "&&&&&&&&&&&&&&&"  --(13,02) SUM(PESOS92) his_devolucion          --CHAR(13 )  --Saldo de Vivienda 92                                
      LET v_arch_detalle.filler6            = ""
      LET v_arch_detalle.cod_resul_op       = ""
      LET v_arch_detalle.diagn_proc         = ""
      LET v_arch_detalle.nom_imss           = v_r_sol_sdos.nom_imss    
      LET v_arch_detalle.num_cred_infonavit = v_r_sol_sdos.num_credito USING "&&&&&&&&&&"
      LET v_arch_detalle.int_sdo_viv97      = "000000000000000"
      LET v_arch_detalle.int_sdo_viv92      = "000000000000000"
      LET v_arch_detalle.filler7            = ""

      IF v_arch_detalle.mto_aportacion IS NULL THEN
         LET v_arch_detalle.mto_aportacion = "000000000000000"
      END IF

      IF v_arch_detalle.nss_separacion IS NOT NULL AND
         v_arch_detalle.nss_separacion <> "           " THEN
          LET v_arch_detalle.tpo_devoluc = "1"

         IF LENGTH(v_arch_detalle.nss_separacion) = 10 THEN
            LET v_arch_detalle.nss_separacion = "0",v_arch_detalle.nss_separacion
         END IF
      END IF

      --Se concatena el detalle
      LET v_string_detalle = v_arch_detalle.tpo_reg,
                             v_arch_detalle.cont_serv,
                             v_arch_detalle.tpo_ent_recep,
                             v_arch_detalle.cve_ent_recep,
                             v_arch_detalle.tpo_ent_ceden,
                             v_arch_detalle.cve_ent_ceden,
                             v_arch_detalle.filler1,
                             v_arch_detalle.fec_present,
                             v_arch_detalle.fec_movimiento,
                             v_arch_detalle.curp_trab,
                             v_arch_detalle.nss_trab,
                             v_arch_detalle.filler2,
                             v_arch_detalle.id_asignac,
                             v_arch_detalle.tpo_devoluc,
                             v_arch_detalle.rfc_trab_info,
                             v_arch_detalle.ap_pat_trab_afore,
                             v_arch_detalle.ap_mat_trab_afore,
                             v_arch_detalle.nom_trab_afore,
                             v_arch_detalle.filler3,
                             v_arch_detalle.id_lote_devol,
                             v_arch_detalle.filler4,
                             v_arch_detalle.aivs97,
                             v_arch_detalle.pesos97,
                             v_arch_detalle.nss_separacion,
                             v_arch_detalle.origen_devol,
                             v_arch_detalle.mto_aportacion,
                             v_arch_detalle.apl_int_aport,
                             v_arch_detalle.filler5,
                             v_arch_detalle.aivs92,
                             v_arch_detalle.pesos92,
                             v_arch_detalle.filler6,
                             v_arch_detalle.cod_resul_op,
                             v_arch_detalle.diagn_proc,
                             v_arch_detalle.nom_imss,
                             v_arch_detalle.num_cred_infonavit,
                             v_arch_detalle.int_sdo_viv97,
                             v_arch_detalle.int_sdo_viv92,
                             v_arch_detalle.filler7

      LET v_indice       = v_indice + 1
      LET v_suma_aivs97  = v_suma_aivs97 + v_arch_detalle.aivs97
      LET v_suma_pesos97 = v_suma_pesos97 + v_arch_detalle.pesos97
      LET v_suma_aivs92  = v_suma_aivs92 + v_arch_detalle.aivs92
      LET v_suma_pesos92 = v_suma_pesos92 + v_arch_detalle.pesos92

      --Se escribe el detalle en el archivo de salida
      CALL v_archivo_sol_devolucion.WRITE([v_string_detalle])
   END FOREACH

   -- se actualiza el contador de registros
   LET v_indice = v_indice - 1

   --Inicia el sumario del archivo
   LET v_arch_sumario.tpo_reg      = "09"
   LET v_arch_sumario.tot_regs_det = v_indice USING "&&&&&&&&&"
   LET v_arch_sumario.filler1      = ""
   --LET v_arch_sumario.sum_aivs97 = (v_suma_aivs97 * 1000000) USING "&&&&&&&&&&&&&&&&&&"
   --LET v_arch_sumario.sum_pesos97 = (v_suma_pesos97 * 100)     USING "&&&&&&&&&&&&&&&"
   LET v_arch_sumario.sum_aivs97   = (v_suma_aivs97) USING "&&&&&&&&&&&&&&&&&&"
   LET v_arch_sumario.sum_pesos97  = (v_suma_pesos97) USING "&&&&&&&&&&&&&&&"
   LET v_arch_sumario.filler2      = ""
   --LET v_arch_sumario.sum_aivs92 = (v_suma_aivs92 * 1000000) USING "&&&&&&&&&&&&&&&&&&"
   --LET v_arch_sumario.sum_pesos92 = (v_suma_pesos92 * 100)     USING "&&&&&&&&&&&&&&&"
   LET v_arch_sumario.sum_aivs92    = (v_suma_aivs92) USING "&&&&&&&&&&&&&&&&&&"
   LET v_arch_sumario.sum_pesos92   = (v_suma_pesos92) USING "&&&&&&&&&&&&&&&"   
   LET v_arch_sumario.int_viv97     = "000000000000000"
   LET v_arch_sumario.int_viv92     = "000000000000000"
   LET v_arch_sumario.filler3       = ""

   --Se concatena el sumario
   LET v_string_sumario = v_arch_sumario.tpo_reg,
                          v_arch_sumario.tot_regs_det,
                          v_arch_sumario.filler1,
                          v_arch_sumario.sum_aivs97,
                          v_arch_sumario.sum_pesos97,
                          v_arch_sumario.filler2,
                          v_arch_sumario.sum_aivs92,
                          v_arch_sumario.sum_pesos92,
                          v_arch_sumario.int_viv97,
                          v_arch_sumario.int_viv92,
                          v_arch_sumario.filler3

   --Se escribe el detalle en el archivo de salida
   CALL v_archivo_sol_devolucion.WRITE([v_string_sumario])

   -- se cierra el archivo
   CALL v_archivo_sol_devolucion.CLOSE()

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_s_ruta_archivo_cp = v_ruta_envio CLIPPED || "/" || "sol_dev_acr." || v_c_extension

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_s_ruta_archivo, " ", v_s_ruta_archivo_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   DISPLAY ""
   --DISPLAY " Ejecutando envío interfaz para Procesar"

   --LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/sol_dev_acr.sh"
   --RUN v_ejecuta_sh

   --DISPLAY ""

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
   LET v_r_dse_ctr_archivo.usuario            = g_usuario_cod
   LET v_r_dse_ctr_archivo.folio              = g_num_folio
   LET v_r_dse_ctr_archivo.nom_archivo        = g_nom_archivo

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
   CALL fn_rutas("acr") RETURNING v_c_ruta_bin, v_c_ruta_listados

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("ACRS141.4rp") THEN
      -- se indica la salida del reporte
      --LET p_v_nom_reporte = g_usuario_cod CLIPPED, "-ACRL30-", g_pid USING "&&&&&", "-", g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"
      CALL fgl_report_setOutputFileName(v_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "no fue posible generar el reporte"
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 
                         RETURNING r_b_valida

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF

   LET v_suma_aivs97  = v_arch_sumario.sum_aivs97 / 1000000
   LET v_suma_pesos97 = v_arch_sumario.sum_pesos97 / 100
   LET v_suma_aivs92  = v_arch_sumario.sum_aivs92 / 1000000
   LET v_suma_pesos92 = v_arch_sumario.sum_pesos92 / 100

   --Se obtienen los rregistros 20-envío , 70 reenvío
     --envío
   LET v_s_comando = "SELECT COUNT(*)
                        FROM safre_tmp:tmp_rep_devolucion
                       WHERE edo_procesar = 20"

   PREPARE prp_devo FROM v_s_comando
   EXECUTE prp_devo INTO v_cnt_envio

     -- reenvio
   LET v_s_comando = "SELECT COUNT(*) 
                        FROM safre_tmp:tmp_rep_devolucion
                       WHERE edo_procesar = 70"

   PREPARE prp_reenvio FROM v_s_comando
   EXECUTE prp_reenvio INTO v_cnt_reenvio

   LET v_s_comando = "SELECT SUM(aivs97)  ,
                             SUM(pesos97) ,
                             SUM(aivs92)  ,
                             SUM(pesos92)
                        FROM safre_tmp:tmp_rep_devolucion
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
                        FROM safre_tmp:tmp_rep_devolucion
                       WHERE edo_procesar = 70"

   PREPARE prp_tot_reenvio FROM v_s_comando
   EXECUTE prp_tot_reenvio INTO v_reenvio_aivs97,
                                v_reenvio_saldo97,
                                v_reenvio_aivs92,
                                v_reenvio_saldo92

   --Se genera el reporte
   START REPORT rpt_acr_solicicitud_devolucion TO XML HANDLER v_manejador_rpt

   -- salida del reporte
   OUTPUT TO REPORT rpt_acr_solicicitud_devolucion(v_indice,
                                                   p_f_presentacion,
                                                   v_suma_aivs97,
                                                   v_suma_pesos97, 
                                                   v_suma_aivs92,
                                                   v_suma_pesos92,
                                                   v_cnt_envio,
                                                   v_cnt_reenvio,
                                                   v_envio_aivs92,
                                                   v_envio_saldo92,
                                                   v_envio_aivs97,
                                                   v_envio_saldo97,
                                                   v_reenvio_aivs92,
                                                   v_reenvio_saldo92,
                                                   v_reenvio_aivs97,
                                                   v_reenvio_saldo97)

   -- finaliza el reporte
   FINISH REPORT rpt_acr_solicicitud_devolucion

   
END FUNCTION

{ ==========================================================================
Nombre: rpt_acr_solicicitud_devolucion
Fecha creacion: 02 de Febrero de 2012
Autor: Francisco López, EFP
Narrativa del proceso que realiza:
Genera el reporte del resumen de los datos generados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
REPORT rpt_acr_solicicitud_devolucion( v_total_registros,
                                       p_f_presentacion,
                                       p_suma_aivs97,
                                       p_suma_saldo97, 
                                       p_suma_aivs92,
                                       p_suma_saldo92,
                                       p_cnt_envio,
                                       p_cnt_reenvio,
                                       p_envio_aivs92,
                                       p_envio_saldo92,
                                       p_envio_aivs97,
                                       p_envio_saldo97,
                                       p_reenvio_aivs92,
                                       p_reenvio_saldo92,
                                       p_reenvio_aivs97,
                                       p_reenvio_saldo97)

   DEFINE v_total_registros    INTEGER
   DEFINE p_f_presentacion     DATE
   DEFINE v_fecha              DATE
   DEFINE p_suma_aivs97        DECIMAL(22,6)
   DEFINE p_suma_saldo97       DECIMAL(22,2)
   DEFINE p_suma_aivs92        DECIMAL(22,6)
   DEFINE p_suma_saldo92       DECIMAL(22,2)
   DEFINE v_folio              INTEGER
   DEFINE p_cnt_envio          INTEGER
   DEFINE p_cnt_reenvio        INTEGER
   DEFINE p_envio_aivs92       DECIMAL(22,6)
   DEFINE p_envio_saldo92      DECIMAL(22,2)
   DEFINE p_envio_aivs97       DECIMAL(22,6)
   DEFINE p_envio_saldo97      DECIMAL(22,2)
   DEFINE p_reenvio_aivs92     DECIMAL(22,6)
   DEFINE p_reenvio_saldo92    DECIMAL(22,2)
   DEFINE p_reenvio_aivs97     DECIMAL(22,6)
   DEFINE p_reenvio_saldo97    DECIMAL(22,2)


   FORMAT

   FIRST PAGE HEADER
      IF p_suma_aivs97 IS NULL THEN
         LET p_suma_aivs97 = 0
      END IF

      IF p_suma_saldo97 IS NULL THEN
         LET p_suma_saldo97 = 0
      END IF

      IF p_suma_aivs92 IS NULL THEN
         LET p_suma_aivs92 = 0
      END IF

      IF p_suma_saldo92 IS NULL THEN
         LET p_suma_saldo92 = 0
      END IF

      LET v_folio =  g_num_folio
      LET v_fecha = TODAY USING "dd/mm/yyyy" 

      PRINTX g_usuario_cod
      PRINTX v_fecha
      PRINTX p_f_presentacion
      PRINTX v_folio
      
   ON EVERY ROW
      PRINTX g_nom_archivo
      PRINTX v_total_registros
      PRINTX p_suma_aivs97
      PRINTX p_suma_saldo97
      PRINTX p_suma_aivs92
      PRINTX p_suma_saldo92
      PRINTX p_cnt_envio
      PRINTX p_cnt_reenvio
      PRINTX p_envio_aivs92
      PRINTX p_envio_saldo92
      PRINTX p_envio_aivs97
      PRINTX p_envio_saldo97
      PRINTX p_reenvio_aivs92
      PRINTX p_reenvio_saldo92
      PRINTX p_reenvio_aivs97
      PRINTX p_reenvio_saldo97

END REPORT

FUNCTION fn_cre_tbl_temp_SolicDev()

   DEFINE v_sql_crea_dse STRING

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   LET v_sql_crea_dse = "EXECUTE PROCEDURE sp_crea_tmp_dse()"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_sp_crea_dse FROM v_sql_crea_dse
   EXECUTE prp_sp_crea_dse
   
   WHENEVER ERROR STOP

   -- regresa a la base de datoa safre viv
   DATABASE safre_viv

END FUNCTION

FUNCTION fn_cre_idx_temp_SolicDev()

   DEFINE v_sql_crea_ix STRING

   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   LET v_sql_crea_ix = "EXECUTE PROCEDURE sp_cre_ix_dse_ta()"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_sp_crea_ix FROM v_sql_crea_ix
   EXECUTE prp_sp_crea_ix
   
   WHENEVER ERROR STOP

   -- regresa a la base de datoa safre viv
   DATABASE safre_viv

END FUNCTION
