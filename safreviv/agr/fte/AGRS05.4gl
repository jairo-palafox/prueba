--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

################################################################################
#M�dulo          => AGR                                                        #
#Programa        => AGRS05                                                     #
#Objetivo        => Programa para generar el archivo de solicitudes de         #
#                   devolucion para el m�dulo de Anualidades Garantizadas      #
#Autor           => Daniel Buendia, EFP                                        #
#Fecha Inicio    => 30 Mayo 2012                                               #
#Modifiaciones                                                                 #
#Autor           => H�ctor F. Jim�nez                                          #
#Descripci�n     => Se modifica la salida del reporte para que muestre los     #
#                   registros que son envios y reenvios                        #
################################################################################

DATABASE safre_viv

GLOBALS
   DEFINE g_pid                       LIKE bat_ctr_proceso.pid      --  ID del proceso
   DEFINE g_proceso_cod               LIKE cat_proceso.proceso_cod  -- c�digo del proceso
   DEFINE g_opera_cod                 LIKE cat_operacion.opera_cod  -- c�digo de operacion
   DEFINE p_usuario_cod               LIKE seg_usuario.usuario_cod  -- Clave de usuario
   DEFINE g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  -- c�digo de operaci�n
   DEFINE g_tpo_transferencia         LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia
   DEFINE g_num_folio                 DECIMAL(9)
END GLOBALS

#Objetivo:
MAIN
   DEFINE p_nom_archivo         LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_c_programa_cod      LIKE cat_operacion.programa_cod -- programa de la operaci�n
   DEFINE v_estatus             SMALLINT
   DEFINE v_id_lote             DECIMAL(10,0)
   DEFINE v_lote                SMALLINT
   DEFINE v_f_lote              DATE
   DEFINE v_f_presentacion      DATE    --Fecha de presentacion, today
   DEFINE v_f_movimiento        DATE    --Fecha de movimiento, 14 dia habil del mes posterior
   DEFINE v_id_lote_dev         CHAR(16)
   DEFINE v_s_titulo_correo     STRING  -- contiene el titulo del correo
   DEFINE v_s_archivo_correo    STRING  -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_mens_correo       STRING  -- contiene el cuerpo del correo
   DEFINE v_v_nom_reporte       VARCHAR(80) -- nombre del reporte 
   DEFINE r_ruta_bin            LIKE seg_modulo.ruta_bin
   DEFINE r_ruta_listados       LIKE seg_modulo.ruta_listados

   #Si se ha recibido par�metros se continua
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)  --numero de proceso
   LET g_num_folio      = ARG_VAL(5)
   LET p_nom_archivo    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".AGRS05.log")

   DISPLAY "=INICIA AGRS05="
   DISPLAY " USUARIO       : ",p_usuario_cod
   DISPLAY " PID           : ",g_pid
   DISPLAY " ARCHIVO       : ",p_nom_archivo

   -- inicializacion de variables
   LET g_tpo_transferencia = "31" -- 31- Solicitud de DSE AGR
   LET v_id_lote   = 02 -- ya que es solo un archivo el lote siempre es 1
   LET v_lote      = 1  -- ya que es solo un archivo el lote siempre es 1
   LET v_f_lote    = TODAY

   -- se genera el folio
   LET g_num_folio = fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
   DISPLAY " FOLIO         : ",g_num_folio USING "#########&"

   -- se obtiene el catorceavo dia habil del mes actual
   LET v_f_presentacion = TODAY - DAY(TODAY) + 1
   PREPARE prp_obtiene_14habil FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_f_presentacion||"',11)"
   EXECUTE prp_obtiene_14habil INTO v_f_presentacion

   -- se valida que la fecha de presentaci�n sea mayor o igual que HOY
   IF v_f_presentacion < TODAY THEN
      -- se obtiene el catorceavo dia habil del mes siguiente
      LET v_f_presentacion = TODAY - DAY(TODAY) + 1
      LET v_f_presentacion = v_f_presentacion + 1 UNITS MONTH

      PREPARE prp_obtiene_14habil_sig FROM "EXECUTE FUNCTION fn_habil_siguiente('"||v_f_presentacion||"',11)"
      EXECUTE prp_obtiene_14habil_sig INTO v_f_presentacion
   END IF

   -- Se contruye la fecha movimiento: Primer dia natural del mes siguiente a la fecha de presentaci�n
   LET v_f_movimiento = v_f_presentacion - DAY(v_f_presentacion) + 1
   LET v_f_movimiento = v_f_movimiento + 1 UNITS MONTH

   DISPLAY " Fecha presentaci�n: ",v_f_presentacion USING "DD/MM/YYYY"
   DISPLAY " Fecha movimiento  : ",v_f_movimiento USING "DD/MM/YYYY"

   -- Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_proceso_cod , g_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_usuario_cod CLIPPED, "-", v_c_programa_cod CLIPPED,"-", g_pid USING "&&&&&", "-", g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"

   --Se asigna el numero de lote
   LET v_id_lote = 02  --ya que es solo un archivo el lote siempre es 1
   LET v_lote = 1      --ya que es solo un archivo el lote siempre es 1
   LET v_f_lote = TODAY

   --Se asigna la fecha con formato AAAAMMDD
   LET v_id_lote_dev = '04002',YEAR(TODAY)  USING "&&&&",MONTH(TODAY) USING "&&",
                       DAY(TODAY) USING "&&",'001'

   DISPLAY " SE CREA TABLA TEMPORAL"
   CALL fn_cre_tbl_temp_SolicDev()

   DISPLAY " Ejecuta solicitud de devoluci�n sdos exc "
   #Llamada a ejecuci�n de procedimiento almacenado
   CALL fn_sp_agr_solicitud_devolucion(v_id_lote, v_lote, v_f_lote, v_f_presentacion, v_f_movimiento, v_id_lote_dev)

   DISPLAY " Se genera reporte"
   --Se genera el reporte de las solicitudes de las devoluciones
   CALL fn_reporte_archivo_solicitud_devolucion(v_id_lote,
                                                v_lote,
                                                v_f_lote,
                                                v_f_presentacion,
                                                v_f_movimiento,
                                                v_id_lote_dev,
                                                p_nom_archivo,
                                                v_v_nom_reporte)

   --Se actualiza el proceso como finalizado
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estatus

   -- se verifica si fue posible finalizar la operacion
   IF v_estatus <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(v_estatus)

      --EXIT PROGRAM
   END IF

      DISPLAY " Envia correo del reporte"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: SOLICITUD DE DEVOLUCI�N DE SALDOS EXCEDENTES ANUALIDADES GARANTIZADAS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",g_pid,"\n",
                          "Proceso      : DEVOLUCI�N DE SALDOS EXCEDENTES ANUALIDADES GARANTIZADAS\n",
                          "Operacion    : SOLICITUD DE DEVOLUCI�N\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la funci�n que env�a por correo el elemento generado
   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)
   
   DISPLAY "=FIN="
END MAIN

#Objetivo: Ejecuta el Stored procedure para la incializacion de la solicitud de las devoluciones
FUNCTION fn_sp_agr_solicitud_devolucion(p_id_lote, p_lote, p_f_lote, p_f_presentacion, p_f_movimiento, p_id_lote_dev)
   DEFINE p_id_lote        DECIMAL(10,0)
   DEFINE p_lote           SMALLINT
   DEFINE p_f_lote         DATE
   DEFINE p_f_presentacion DATE
   DEFINE p_f_movimiento   DATE
   DEFINE p_id_lote_dev    CHAR(16)
   DEFINE v_sql_procedure  STRING
   
   -- se asigna la sentencia que ejecuta el store de la solic de DSE
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_dse_solicitud_devol_agr(?,?,?,?,?,?,?)"

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
FUNCTION fn_reporte_archivo_solicitud_devolucion(p_id_lote,
                                                 p_lote,
                                                 p_f_lote,
                                                 p_f_presentacion,
                                                 p_f_movimiento,
                                                 p_id_lote_dev,
                                                 p_c_nom_archivo,
                                                 p_v_nom_reporte)

   DEFINE p_id_lote                  DECIMAL(10,0)
   DEFINE p_lote                     SMALLINT
   DEFINE p_f_lote                   DATE
   DEFINE p_f_presentacion           DATE
   DEFINE p_f_movimiento             DATE
   DEFINE p_id_lote_dev              CHAR(16)
   DEFINE p_c_nom_archivo            LIKE cre_ctr_archivo.nom_archivo -- nombre del archivo
   DEFINE p_v_nom_reporte            VARCHAR(80) -- nombre del reporte
   DEFINE v_c_f_presentacion         CHAR(8) -- fecha de presentaci�n con formato "yyyymmdd"
   DEFINE v_c_f_movimiento           CHAR(8) -- fecha de movimiento con formato "yyyymmdd"

   --Variables para la genracion de los archivos
   DEFINE v_string_encabezado        STRING
   DEFINE v_string_detalle           STRING
   DEFINE v_string_sumario           STRING
   DEFINE v_suma_tot_aivs97          DECIMAL(22,2)
    --,v_suma_tot_pesos97      DECIMAL(22,2)
   DEFINE v_suma_tot_aivs92          DECIMAL(22,2)
    --,v_suma_tot_pesos92      DECIMAL(22,2)
   DEFINE v_indice                   INTEGER
   DEFINE v_archivo_sol_devolucion   BASE.CHANNEL
   DEFINE v_ruta_envio               LIKE seg_modulo.ruta_envio
   DEFINE v_s_ruta_archivo           STRING
   DEFINE v_s_ruta_archivo_cp        STRING
   DEFINE v_c_extension              LIKE cat_operacion.extension -- extensi�n del archivo
   DEFINE v_query                    STRING
   DEFINE v_s_comando                STRING -- contiene al comando a correr
   DEFINE v_manejador_rpt            om.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_r_sol_sdos           RECORD
             curp                    CHAR(18),
             nss                     CHAR(11),
             rfc                     CHAR(13),
             ap_paterno_af           CHAR(40),
             ap_materno_af           CHAR(40),
             nombre_af               CHAR(40),
             aivs97                  DECIMAL(22,2),
             pesos97                 DECIMAL(22,2),
             nss_separacion          CHAR(11),
             origen_devolucion       CHAR(2),
             monto_aportacion        DECIMAL(22,2),
             aivs_aportacion         DECIMAL(22,2),
             aivs92                  DECIMAL(22,2),
             pesos92                 DECIMAL(22,2),
             nom_imss                CHAR(50),
             num_credito             DECIMAL(10,0)
      END RECORD
   DEFINE v_arch_encabezado      RECORD
             tpo_reg                 CHAR(02),   --Tipo de Registro
             id_serv                 CHAR(02),   --Identificador de Servicio
             id_oper                 CHAR(02),   --Identificador de Operaci�n
             tpo_ent_orig            CHAR(02),   --Tipo de entidad origen
             cve_ent_orig            CHAR(03),   --Clave de entidad origen
             tpo_ent_dest            CHAR(02),   --Tipo de entidad destino
             cve_ent_dest            CHAR(03),   --Clave de entidad destino
             ent_fed_env             CHAR(03),   --Entidad federativa de env�o de lote
             fec_present             CHAR(08),   --Fecha de presentaci�n
             cons_lote               CHAR(03),   --Consecutivo del lote en el d�a
             cve_mod_recept          CHAR(02),   --Clave de modalidad de recepci�n
             cod_res_oper            CHAR(02),   --C�digo de resultado de la Operaci�n
             mot_rech_lote           CHAR(09),   --Motivo de rechazo del lote
             filler                  CHAR(687)   --Filler
      END RECORD
   DEFINE v_arch_detalle         RECORD
             tpo_reg                 CHAR(02), -- Tipo de Registro [1-2]
             cont_serv               CHAR(10), -- Contador de Servicio [3-12]
             tpo_ent_recep           CHAR(02), -- Tipo de entidad receptora de la cuenta [13-14]
             cve_ent_recep           CHAR(03), -- Clave de entidad receptora de la cuenta [15-17]
             tpo_ent_ced             CHAR(02), -- Tipo de entidad cedente de la cuenta [18-19]
             cve_ent_ced             CHAR(03), -- Clave de entidad ced. de la cuenta [20-22]
             tpo_transfer            CHAR(02), -- Tipo de transferencia [23-24]
             fec_present             CHAR(08), -- Fecha de presentaci�n [25-32]
             fec_mov                 CHAR(08), -- Fecha de movimiento [33-40]
             curp_trab               CHAR(18), -- CURP del trabajador [41-58]
             nss_trab                CHAR(11), -- NSS del trabajador [59-69]
             filler1                 CHAR(13), -- Filler [70-82]
             id_asign                CHAR(01), -- Identificador de Asignaci�n [83-83]
             tpo_devol               CHAR(01), -- Tipo de devoluci�n [84-84]
             rfc_trab_inf            CHAR(13), -- RFC del Trabajador seg�n Infonavit [85-97]
             ap_pat_afore            CHAR(40), -- Apellido paterno del trabajador en AFORE receptora [98-137]
             ap_mat_afore            CHAR(40), -- Apellido materno del trabajador en AFORE receptora [138-177]
             nom_trab_afore          CHAR(40), -- Nombres del trabajador en AFORE receptora [178-217]
             filler2                 CHAR(22), -- Filler [218-239]
             id_lote_devol           CHAR(16), -- Identificador de lote de la devoluci�n [240-255]
             filler3                 CHAR(219), -- Filler [256-474]
             sum_aivs97              CHAR(15), -- N�mero de AIVS 97 [475-489]
             filler4                 CHAR(15), -- filler4 [490-504]
             nss_separ_cta           CHAR(11), -- NSS por separaci�n de cuentas [505-515]
             orig_devol              CHAR(02), -- origen de la devoluci�n [516-517]
             mto_aportac             CHAR(15), -- Monto de Aportaciones 5% [518-532]
             apl_int_aportac         CHAR(15), -- Aplicaci�n de Intereses de Aportaciones 5% [533-547]
             filler5                 CHAR(02), -- Filler [548-549]
             sum_aivs92              CHAR(15), -- N�mero de AIVS 92 [550-564]
             filler6                 CHAR(18), -- Filler [565-582]
             cod_res_opera           CHAR(02), -- C�digo Resultado de la Operaci�n [583-584]
             diag_proc               CHAR(15), -- Diagn�stico del proceso [585-599]
             nom_trab_imss           CHAR(50), -- Nombre del Trabajador seg�n IMSS [600-649]
             num_cred_info           CHAR(10), -- N�mero de Cr�dito INFONAVIT [650-659]
             filler7                 CHAR(71)  -- Filler [660-730]
      END RECORD
   DEFINE v_arch_sumario         RECORD
             tpo_reg                 CHAR(2),  -- Tipo de Registro [1-2]
             tot_regs_det            CHAR(9),  -- Cantidad de Registros de Detalle [3-11]
             filler1                 CHAR(30), -- Filler [12-41]
             sum_aivs97              CHAR(18), -- Suma de n�mero de AIVS 97 [42-59]
             filler2                 CHAR(60), -- Filler [60-119]
             sum_aivs92              CHAR(18), -- Suma de n�mero de AIVS 92 [120-137]
             filler3                 CHAR(593) -- Filler [138-730]
      END RECORD
   DEFINE v_r_dse_ctr_archivo        RECORD LIKE dse_ctr_archivo.*  -- registro de la tabla de control
   DEFINE v_c_ruta_bin               LIKE seg_modulo.ruta_bin       -- ruta bin
   DEFINE v_c_ruta_listados          LIKE seg_modulo.ruta_listados  -- ruta listados
   DEFINE v_i_lote                   LIKE dse_ctr_archivo.lote      -- lote del archivo
   DEFINE v_f_fecha_archivo          DATE                           -- fecha para nomenclatura de archivo
   DEFINE v_suma_aivs97_aux          DECIMAL(22,6)
   DEFINE v_suma_aivs92_aux          DECIMAL(22,6)
   DEFINE r_b_valida                 SMALLINT                       -- estatus del proceso
   DEFINE v_cnt_envio                SMALLINT
   DEFINE v_cnt_reenvio              SMALLINT
   DEFINE v_envio_aivs97             DECIMAL(22,6)
   DEFINE v_envio_saldo97            DECIMAL(22,2)
   DEFINE v_envio_aivs92             DECIMAL(22,6)
   DEFINE v_envio_saldo92            DECIMAL(22,2)
   DEFINE v_reenvio_aivs97           DECIMAL(22,6)
   DEFINE v_reenvio_saldo97          DECIMAL(22,2)
   DEFINE v_reenvio_aivs92           DECIMAL(22,6)
   DEFINE v_reenvio_saldo92          DECIMAL(22,2)

   --Se obtiene las rutas para la generacion del archivo
   -- se obtienen la ruta envio del modulo
   LET v_query = " SELECT ruta_envio\n",
                 "   FROM seg_modulo\n",
                 "  WHERE modulo_cod = 'agr' "

   PREPARE prp_ruta_archivo FROM v_query
   EXECUTE prp_ruta_archivo INTO v_ruta_envio

   DISPLAY " Ruta envio: ",v_ruta_envio
   -- se crea el nombre del archivo
   LET v_f_fecha_archivo = TODAY
   --LET p_c_nom_archivo = "sol_dev",v_f_fecha_archivo USING "yyyymmdd",".adse"

   -- se concatena el nombre del archivo con la ruta de envio
   LET v_s_ruta_archivo = v_ruta_envio CLIPPED,"/",p_c_nom_archivo CLIPPED

   -- se crea el manejador de archivo
   LET v_archivo_sol_devolucion = BASE.CHANNEL.CREATE()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo_sol_devolucion.openFile(v_s_ruta_archivo, "w" )
   CALL v_archivo_sol_devolucion.setDelimiter("")

   -- se asigna la fecha de presentaci�n con formato AAAAMMDD
   LET v_c_f_presentacion = p_f_presentacion USING "yyyymmdd"
   LET v_c_f_movimiento   = p_f_movimiento USING "yyyymmdd"

   --Se asignan los valores del encabezado
   LET v_arch_encabezado.tpo_reg        = "01"    --CHAR(02 )   --Tipo de Registro
   LET v_arch_encabezado.id_serv        = "02"    --CHAR(02 )   --Identificador de Servicio
   LET v_arch_encabezado.id_oper        = "15"    --CHAR(02 )   --Identificador de Operaci�n
   LET v_arch_encabezado.tpo_ent_orig   = "04"    --CHAR(02 )   --Tipo de entidad origen
   LET v_arch_encabezado.cve_ent_orig   = "002"   --CHAR(03 )   --Clave de entidad origen
   LET v_arch_encabezado.tpo_ent_dest   = "01"    --CHAR(02 )   --Tipo de entidad destino
   LET v_arch_encabezado.cve_ent_dest   = ""      --CHAR(03 )   --Clave de entidad destino
   LET v_arch_encabezado.ent_fed_env    = "009"   --CHAR(03 )   --Entidad federativa de env�o de lote
   LET v_arch_encabezado.fec_present    = v_c_f_presentacion
   LET v_arch_encabezado.cons_lote      = "001"      --CHAR(03 )   --Consecutivo del lote en el d�a
   LET v_arch_encabezado.cve_mod_recept = "" --"02"  --CHAR(02 )   --Clave de modalidad de recepci�n
   LET v_arch_encabezado.cod_res_oper   = ""         --CHAR(02 )   --C�digo de resultado de la Operaci�n
   LET v_arch_encabezado.mot_rech_lote  = ""         --CHAR(03x3)  --Motivo de rechazo del lote
   LET v_arch_encabezado.filler         = ""         --CHAR(687)   --Filler
   
   --Se concatena el encabezado
   LET v_string_encabezado = v_arch_encabezado.tpo_reg,
                             v_arch_encabezado.id_serv,
                             v_arch_encabezado.id_oper,
                             v_arch_encabezado.tpo_ent_orig,
                             v_arch_encabezado.cve_ent_orig,
                             v_arch_encabezado.tpo_ent_dest,
                             v_arch_encabezado.cve_ent_dest,
                             v_arch_encabezado.ent_fed_env,
                             v_arch_encabezado.fec_present,
                             v_arch_encabezado.cons_lote,
                             v_arch_encabezado.cve_mod_recept,
                             v_arch_encabezado.cod_res_oper,
                             v_arch_encabezado.mot_rech_lote,
                             v_arch_encabezado.filler
   
   --Se ingresa el encabezado en el archivo
   CALL v_archivo_sol_devolucion.WRITE([v_string_encabezado])

   LET v_query = " SELECT * FROM safre_tmp:tmp_rep_devolucion_agr"

   PREPARE prp_rep_selicitud_pago FROM  v_query
   DECLARE cur_rep_selicitud_pago CURSOR FOR prp_rep_selicitud_pago

   LET v_indice          = 1
   LET v_suma_tot_aivs97 = 0
   --LET v_suma_tot_pesos97 = 0
   LET v_suma_tot_aivs92 = 0
   --LET v_suma_tot_pesos92 = 0
   
   --Inicia el detalle del archivo
   --Se inicia el foreach para la lectura de los registros de la tabla temporal
   FOREACH cur_rep_selicitud_pago INTO v_r_sol_sdos.*
      -- se valida el importe viv97 obtenido
      IF v_r_sol_sdos.aivs97 IS NULL THEN
         LET v_r_sol_sdos.aivs97 = 0
      END IF

      -- se valida el importe viv92 obtenido
      IF v_r_sol_sdos.aivs92 IS NULL THEN
         LET v_r_sol_sdos.aivs92 = 0
      END IF

      LET v_arch_detalle.tpo_reg         = "02"
      LET v_arch_detalle.cont_serv       = v_indice USING "&&&&&&&&&&"
      LET v_arch_detalle.tpo_ent_recep   = "01" 
      LET v_arch_detalle.cve_ent_recep   = ""
      LET v_arch_detalle.tpo_ent_ced     = "04"
      LET v_arch_detalle.cve_ent_ced     = "002"
      LET v_arch_detalle.tpo_transfer    = "43"
      LET v_arch_detalle.fec_present     = v_c_f_presentacion
      LET v_arch_detalle.fec_mov         = v_c_f_movimiento
      LET v_arch_detalle.curp_trab       = v_r_sol_sdos.curp
      LET v_arch_detalle.nss_trab        = v_r_sol_sdos.nss
      LET v_arch_detalle.filler1         = ""
      LET v_arch_detalle.id_asign        = ""
      LET v_arch_detalle.tpo_devol       = "0"
      LET v_arch_detalle.rfc_trab_inf    = v_r_sol_sdos.rfc
      LET v_arch_detalle.ap_pat_afore    = v_r_sol_sdos.ap_paterno_af
      LET v_arch_detalle.ap_mat_afore    = v_r_sol_sdos.ap_materno_af
      LET v_arch_detalle.nom_trab_afore  = v_r_sol_sdos.nombre_af
      LET v_arch_detalle.filler2         = ""
      LET v_arch_detalle.id_lote_devol   = p_id_lote_dev
      LET v_arch_detalle.filler3         = ""
      LET v_arch_detalle.sum_aivs97      = (v_r_sol_sdos.aivs97 * 1000000) USING "&&&&&&&&&&&&&&&" --SUM(AIVS97) dse_his_devolucion           --(09,06) N�mero de AIVS 97
      LET v_arch_detalle.filler4         = "" --(v_r_sol_sdos.pesos97 * 100)    USING "&&&&&&&&&&&&&&&" --SUM(PESOS97) dse_his_devolucion          --(13,02) Saldo de vivienda 97
      LET v_arch_detalle.nss_separ_cta   = v_r_sol_sdos.nss_separacion
      LET v_arch_detalle.orig_devol      = v_r_sol_sdos.origen_devolucion
      LET v_arch_detalle.mto_aportac     = (v_r_sol_sdos.monto_aportacion * 100)    USING "&&&&&&&&&&&&&&&"      --(13,02) MONTO_APORTACION EN dse_his_devolucion    --CHAR(13 )  --Monto de Aportaciones 5%
      LET v_arch_detalle.apl_int_aportac = (v_r_sol_sdos.aivs_aportacion * 1000000) USING "&&&&&&&&&&&&&&&"    --(09,06) AIVS_APORTACION EN dse_his_devolucion    --CHAR(09 )  --Aplicaci�n de Intereses de Aportaciones 5%
      LET v_arch_detalle.filler5         = ""
      LET v_arch_detalle.sum_aivs92      = (v_r_sol_sdos.aivs92 * 1000000) USING "&&&&&&&&&&&&&&&"   --(09,06) SUM(AIVS92) dse_his_devolucion           --CHAR(09 )  --N�mero de AIVS 92
      LET v_arch_detalle.filler6         = ""
      LET v_arch_detalle.cod_res_opera   = ""
      LET v_arch_detalle.diag_proc       = ""
      LET v_arch_detalle.nom_trab_imss   = v_r_sol_sdos.nom_imss    
      LET v_arch_detalle.num_cred_info   = v_r_sol_sdos.num_credito USING "&&&&&&&&&&"
      LET v_arch_detalle.filler7         = ""

      IF v_arch_detalle.nss_separ_cta IS NOT NULL AND
         v_arch_detalle.nss_separ_cta <> "           " THEN
          LET v_arch_detalle.tpo_devol = "1"
      END IF

      --Se concatena el detalle
      LET v_string_detalle = v_arch_detalle.tpo_reg,
                             v_arch_detalle.cont_serv,
                             v_arch_detalle.tpo_ent_recep,
                             v_arch_detalle.cve_ent_recep,
                             v_arch_detalle.tpo_ent_ced,
                             v_arch_detalle.cve_ent_ced,
                             v_arch_detalle.tpo_transfer,
                             v_arch_detalle.fec_present,
                             v_arch_detalle.fec_mov,
                             v_arch_detalle.curp_trab,
                             v_arch_detalle.nss_trab,
                             v_arch_detalle.filler1,
                             v_arch_detalle.id_asign,
                             v_arch_detalle.tpo_devol,
                             v_arch_detalle.rfc_trab_inf,
                             v_arch_detalle.ap_pat_afore,
                             v_arch_detalle.ap_mat_afore,
                             v_arch_detalle.nom_trab_afore,
                             v_arch_detalle.filler2,
                             v_arch_detalle.id_lote_devol,
                             v_arch_detalle.filler3,
                             v_arch_detalle.sum_aivs97,
                             v_arch_detalle.filler4,
                             v_arch_detalle.nss_separ_cta,
                             v_arch_detalle.orig_devol,
                             v_arch_detalle.mto_aportac,
                             v_arch_detalle.apl_int_aportac,
                             v_arch_detalle.filler5,
                             v_arch_detalle.sum_aivs92,
                             v_arch_detalle.filler6,
                             v_arch_detalle.cod_res_opera,
                             v_arch_detalle.diag_proc,
                             v_arch_detalle.nom_trab_imss,
                             v_arch_detalle.num_cred_info,
                             v_arch_detalle.filler7

      LET v_indice             = v_indice + 1
      LET v_suma_tot_aivs97    = v_suma_tot_aivs97 + v_arch_detalle.sum_aivs97
      --LET v_suma_tot_pesos97 = v_suma_tot_pesos97 + v_arch_detalle.filler4
      LET v_suma_tot_aivs92    = v_suma_tot_aivs92 + v_arch_detalle.sum_aivs92
      
      --Se escribe el detalle en el archivo de salida
      CALL v_archivo_sol_devolucion.WRITE([v_string_detalle])
   END FOREACH
   
   -- se actualiza el contador de registros
   LET v_indice = v_indice - 1

   --Inicia el sumario del archivo
   LET v_arch_sumario.tpo_reg      = "09"
   LET v_arch_sumario.tot_regs_det = v_indice USING "&&&&&&&&&"
   LET v_arch_sumario.filler1      = ""
   --LET v_arch_sumario.sum_aivs97 = (v_suma_tot_aivs97 * 1000000) USING "&&&&&&&&&&&&&&&&&&"
   LET v_arch_sumario.sum_aivs97   = (v_suma_tot_aivs97) USING "&&&&&&&&&&&&&&&&&&"
   LET v_arch_sumario.filler2      = ""
   --LET v_arch_sumario.sum_aivs92 = (v_suma_tot_aivs92 * 1000000) USING "&&&&&&&&&&&&&&&&&&"
   LET v_arch_sumario.sum_aivs92   = (v_suma_tot_aivs92) USING "&&&&&&&&&&&&&&&&&&"
   LET v_arch_sumario.filler3      = ""
   
   --Se concatena el sumario
   LET v_string_sumario = v_arch_sumario.tpo_reg,
                          v_arch_sumario.tot_regs_det,
                          v_arch_sumario.filler1,
                          v_arch_sumario.sum_aivs97,
                          v_arch_sumario.filler2,
                          v_arch_sumario.sum_aivs92,
                          v_arch_sumario.filler3
                         
   --Se escribe el detalle en el archivo de salida
   CALL v_archivo_sol_devolucion.WRITE([v_string_sumario])
   
   -- se cierra el archivo
   CALL v_archivo_sol_devolucion.CLOSE()

   -- se obtiene la extensi�n del archivo
   LET v_c_extension = fn_recupera_extension(g_proceso_cod, g_opera_cod)

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_s_ruta_archivo_cp = v_ruta_envio CLIPPED || "/" || "sol_dev_ag." || v_c_extension

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_s_ruta_archivo, " ", v_s_ruta_archivo_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   -- se obtiene el maximo lote para la fecha de presentaci�n
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
   CALL fn_rutas("agr") RETURNING v_c_ruta_bin, v_c_ruta_listados

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("AGRS051.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(v_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuraci�n en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 
                         RETURNING r_b_valida

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF

   LET v_suma_aivs97_aux  = v_arch_sumario.sum_aivs97 / 1000000
   LET v_suma_aivs92_aux  = v_arch_sumario.sum_aivs92 / 1000000

    --Se obtienen los rregistros 20-env�o , 70 reenv�o
   --env�o
   LET v_s_comando = "SELECT COUNT(*)
                        FROM safre_tmp:tmp_rep_devolucion_agr
                       WHERE edo_procesar = 20"
                                                   
   PREPARE prp_devo FROM v_s_comando
   EXECUTE prp_devo INTO v_cnt_envio

   -- reenvio
   LET v_s_comando = "SELECT COUNT(*)
                        FROM safre_tmp:tmp_rep_devolucion_agr
                       WHERE edo_procesar = 70"

   PREPARE prp_reenvio FROM v_s_comando
   EXECUTE prp_reenvio INTO v_cnt_reenvio

   LET v_s_comando = "SELECT SUM(aivs97)  ,
                             SUM(pesos97) ,
                             SUM(aivs92)  ,
                             SUM(pesos92)
                        FROM safre_tmp:tmp_rep_devolucion_agr
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
                        FROM safre_tmp:tmp_rep_devolucion_agr
                       WHERE edo_procesar = 70"

   PREPARE prp_tot_reenvio FROM v_s_comando
   EXECUTE prp_tot_reenvio INTO v_reenvio_aivs97,
                                v_reenvio_saldo97,
                                v_reenvio_aivs92,
                                v_reenvio_saldo92


   --Se genera el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- salida del reporte
   OUTPUT TO REPORT reporte_archivo_salida(p_usuario_cod,
                                           p_c_nom_archivo,
                                           v_suma_aivs97_aux,
                                           v_suma_aivs92_aux,
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
   FINISH REPORT reporte_archivo_salida
   
END FUNCTION

#OBJETIVO: Genera el reporte de Rechazos
REPORT reporte_archivo_salida(p_v_usuario, 
                              p_v_arch_proceso,
                              p_total_aivs97,
                              p_total_aivs92,
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
                          
   DEFINE p_v_usuario            LIKE seg_usuario.usuario_cod
   DEFINE v_fecha_reporte        DATE
   DEFINE v_fecha_present        LIKE dis_sum_avance_pago.f_presentacion
   DEFINE p_count_reg            INTEGER
   DEFINE p_total_aivs92         DECIMAL(22,6)
   DEFINE p_total_aivs97         DECIMAL(22,6)
   DEFINE p_v_arch_proceso       CHAR(100)
   DEFINE p_cnt_envio            SMALLINT
   DEFINE p_cnt_reenvio          SMALLINT
   DEFINE p_envio_aivs97         DECIMAL(22,6)
   DEFINE p_envio_saldo97        DECIMAL(22,2)
   DEFINE p_envio_aivs92         DECIMAL(22,6)
   DEFINE p_envio_saldo92        DECIMAL(22,2)
   DEFINE p_reenvio_aivs97       DECIMAL(22,6)
   DEFINE p_reenvio_saldo97      DECIMAL(22,2)
   DEFINE p_reenvio_aivs92       DECIMAL(22,6)
   DEFINE p_reenvio_saldo92      DECIMAL(22,2)

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      LET v_fecha_present = v_fecha_reporte      

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX g_num_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX p_v_usuario
      PRINTX p_total_aivs92
      PRINTX p_total_aivs97
      PRINTX p_count_reg
      PRINTX p_v_arch_proceso

      --Se imprimen los contadores en envios y reenvios
      PRINTX p_cnt_envio
      PRINTX p_cnt_reenvio

      PRINTX p_envio_aivs97
      PRINTX p_envio_aivs92
      PRINTX p_reenvio_aivs97
      PRINTX p_reenvio_aivs92

END REPORT

FUNCTION fn_cre_tbl_temp_SolicDev()
   DEFINE v_sql_crea_dse STRING
   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   LET v_sql_crea_dse = "EXECUTE PROCEDURE sp_crea_tmp_dse_agr()"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_sp_crea_dse FROM v_sql_crea_dse
   EXECUTE prp_sp_crea_dse
   
   WHENEVER ERROR STOP

   -- regresa a la base de datoa safre viv
   DATABASE safre_viv

END FUNCTION
