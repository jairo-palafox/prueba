--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#####################################################################
#Modulo            => GRT                                           #
#Programa          => GRTS03                                        #
#Objetivo          => Programa que ejecuta el proceso de generaci�n #
#                     de archivo solicitud de saldos, para el       #
#                     m�dulo de Uso de Garant�a 43 bis              #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 26 Abril 2012                                 #
#Modifica:         => Mauro Mu�iz Caballero                         #
#Fecha modif:      => 23 de marzo de 2016                           #
#Adecuaci�n        => Eliminaci�n de adelantos                      #
#####################################################################

DATABASE safre_viv

GLOBALS "GRTG01.4gl"

   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE p_v_arch_proceso          LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo

   --Arreglo para inf. del reporte PDF
   DEFINE arr_sol_saldo DYNAMIC ARRAY OF RECORD
      estado_desc    CHAR(50),
      t_registros    INTEGER,
      pesos          DECIMAL(12,2),
      porcentaje     CHAR(12) 
   END RECORD 
   --Record para el total global de registros procesados
   DEFINE r_total_global RECORD
      t_registros    INTEGER,
      pesos          DECIMAL(12,2),
      porcentaje     CHAR(12) 
   END RECORD

   DEFINE v_aux_porcentaje DECIMAL(6,2)
   DEFINE v_f_opera_ini    LIKE bat_ctr_operacion.fecha_ini
   DEFINE v_f_opera_fin    LIKE bat_ctr_operacion.fecha_fin
   DEFINE v_arh_salida     STRING
   DEFINE v_c_ruta_envio   LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE r_b_valida       SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_c_programa_cod LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_c_ruta_listado LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo
   DEFINE v_ruta_bin       LIKE seg_modulo.ruta_bin
   DEFINE v_manejador_rpt  OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte

MAIN 

   DEFINE v_r_cre_uso_garantia RECORD
      id_derhabiente                LIKE cre_uso_garantia.id_derechohabiente, -- id derechohabiente
      num_credito                   LIKE cre_uso_garantia.num_credito, -- n�mero de cr�dito
      periodo_pago                  LIKE cre_uso_garantia.periodo_pago, -- periodo de pago
      tpo_transferencia             LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia
   END RECORD

   DEFINE v_d_id_cre_uso_garant     LIKE cre_uso_garantia.id_cre_uso_garantia -- identificar de uso de garantia
   DEFINE v_si_edo_procesar         LIKE cre_uso_garantia.edo_procesar -- estado procesar
   DEFINE v_d_aivs_v97              DECIMAL(18,6) -- importe de vivienda 97
   DEFINE v_d_importe_v97           DECIMAL(12,2) -- importe de vivienda 97
   DEFINE v_r_afi_derechohab        RECORD LIKE afi_derechohabiente.* -- registro de afi derechohabiente

   DEFINE v_r_encabezado RECORD
      tpo_registro                  CHAR(2), -- Tipo de Registro (001-002)
      id_servicio                   CHAR(2), -- Identificador de Servicio (003-004)
      id_operacion                  CHAR(2), -- Identificador de Operaci�n (005-006)
      tpo_ent_origen                CHAR(2), -- Tipo de entidad origen (007-008)
      cve_ent_origen                CHAR(3), -- Clave de entidad origen (009-011)
      tpo_ent_destino               CHAR(2), -- Tipo de entidad destino (012-013)
      cve_ent_destino               CHAR(3), -- Clave de entidad destino (014-016)
      ent_fed_env_lote              CHAR(3), -- Entidad federativa de env�o de lote (017-019)
      fec_presentacion              CHAR(8), -- Fecha de presentaci�n (020-027)
      consec_lote                   CHAR(3), -- Consecutivo del lote en el d�a (028-030)
      cve_modal_recep               CHAR(2), -- Clave de modalidad de recepci�n (031-032)
      cod_resul_opera               CHAR(2), -- C�digo de resultado de la Operaci�n (033-034)
      mot_rechazo                   CHAR(9), -- Motivo de rechazo del lote (035-043)
      filler                        CHAR(687)-- Filler (044-730)
   END RECORD

   DEFINE v_r_detalle RECORD
      tpo_registro                  CHAR(2), -- Tipo de Registro (001-002)
      cont_servicio                 CHAR(10), -- Contador de Servicio (003-012)
      tpo_ent_recept                CHAR(2), -- Tipo de entidad receptora de la cuenta (013-014)
      cve_ent_recept                CHAR(3), -- Clave de entidad receptora de la cuenta (015-017)
      tpo_ent_cedente               CHAR(2), -- Tipo de entidad cedente de la cuenta (018-019)
      cve_ent_cedente               CHAR(3), -- Clave de entidad ced. de la cuenta (020-022)
      tpo_transferencia             CHAR(2), -- Origen/Tipo de transferencia (023-024)
      f_presentacion                CHAR(8), -- Fecha de presentaci�n (025-032)
      f_movimiento                  CHAR(8), -- fecha de movimiento (033-040)
      curp_trabajador               CHAR(18), -- CURP del trabajador (041-058)
      nss_trab_infonavit            CHAR(11), -- NSS del trabajador seg�n INFONAVIT (059-069)
      filler2                       CHAR(15), -- Filler2 (070-084)
      rfc_trab_infonavit            CHAR(13), -- RFC del trabajador seg�n INFONAVIT (085-097)
      ape_pat_infonavit             CHAR(40), -- Apellido paterno del trabajador en el INFONAVIT (098-137)
      ape_mat_infonavit             CHAR(40), -- Apellido materno del trabajador en el INFONAVIT (138-177)
      nom_trab_infonavit            CHAR(40), -- Nombres del trabajador en el INFONAVIT (178-217)
      filler3                       CHAR(22), -- Filler3 (218-239)
      id_lote_solic                 CHAR(16), -- Identificador de lote de la solicitud (240-255)
      filler4                       CHAR(15), -- Filler4 (256-270)
      nss_trab_afore                CHAR(11), -- NSS del trabajador seg�n AFORE/PS. (271-281)
      rfc_trab_afore                CHAR(13), -- RFC del trabajador seg�n AFORE (282-294)
      filler5                       CHAR(30), -- Filler5 (295-324)
      ape_pat_afore                 CHAR(40), -- Apellido paterno del trabajador en la AFORE cedente (325-364)
      ape_mat_afore                 CHAR(40), -- Apellido materno del trabajador en la AFORE cedente (365-404)
      nom_trab_afore                CHAR(40), -- Nombres del trabajador en la AFORE cedente (405-444)
      filler6                       CHAR(30), -- Filler6 (445-474)
      num_apl_int_viv97             CHAR(15), -- N�mero de "Aplicaciones de Intereses de Vivienda" 97 de la �ltima aportaci�n (475-489)
      ult_aport_vivi97              CHAR(15), -- Ultima aportaci�n Vivienda 97 (490-504)
      filler7                       CHAR(78), -- Filler7 (505-582)
      cod_result_opera              CHAR(2), -- C�digo Resultado de la Operaci�n (583-584)
      diagnostico_proc              CHAR(15), -- Diagn�stico del Proceso (585-599)
      nom_trab_imss                 CHAR(50), -- Nombre del Trabajador seg�n IMSS (600-649)
      num_cred_infonavit            CHAR(10), -- N�mero de Cr�dito INFONAVIT (650-659)
      filler8                       CHAR(53), -- Filler8 (660-712)
      periodo_pago                  CHAR(6), -- Per�odo de pago (713-718)
      filler9                       CHAR(12) -- Filler9 (719-730)
   END RECORD

   DEFINE v_r_sumario RECORD
      tpo_registro                  CHAR(2), -- tipo de registro (001-002)
      cant_regs_detalle             CHAR(9), -- cantidad de registros de detalle (003-011)
      filler1                       CHAR(30), -- filler1 (012-041)
      sum_numAplInt_viv97           CHAR(15), -- suma del n�mero de "aplicaciones intereses de vivienda" 97 de la �ltima aportaci�n (042-056)
      sum_ult_aport_viv97           CHAR(15), -- suma de la ultima aportaci�n de vivienda 97 (057-071)
      filler2                       CHAR(659) -- filler2 (072-730)
   END RECORD

   DEFINE v_r_tmp_uso_solic_sdo RECORD
      nss                           CHAR(11),
      id_derechohabiente            DECIMAL(9,0),
      id_cre_uso_garantia           DECIMAL(9,0), 
      periodo_pago                  CHAR(6),
      modulo_cod                    CHAR(2),
      f_proceso                     DATE
   END RECORD

   DEFINE v_edo_procesar_aux        LIKE cre_uso_garantia.edo_procesar -- estado procesar a actualizar
   DEFINE v_dt_fec_present          DATE -- fecha de presentacion
   DEFINE v_dt_fec_movimiento       DATE -- fecha de movimiento
   DEFINE v_c_fec_present_ax        CHAR(8) -- fecha ayxiliar de presentacion con formato YYYYMMDD
   DEFINE v_dt_f_solic_saldos       DATE -- fecha para la generaci�n de solicitud de saldos
   DEFINE v_v_ruta_nomarch          VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp       VARCHAR(100) -- ruta y nombre del archivo de salida
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensi�n del archivo
   DEFINE v_ch_arch_solTransf       BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_i_contrador_reg         INTEGER -- contrador de registros
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_r_cre_ctr_archivo       RECORD LIKE cre_ctr_archivo.* -- regsitro de cre ctr archivo
   DEFINE v_archivo_nom             STRING
   DEFINE v_d_sum_importe_v97       DECIMAL(25,2) -- suma del importe de vivienda 97
   DEFINE v_d_precio_fondo          LIKE glo_valor_fondo.precio_fondo -- precio de accion
   DEFINE v_i_lote                  LIKE dse_ctr_archivo.lote -- lote del archivo

   DEFINE v_folio                   DECIMAL(9,0)
   DEFINE v_criterio                SMALLINT
   DEFINE v_tabla                   CHAR(20)
   DEFINE v_f_liquida               DATE
   DEFINE v_ejecuta_sh              STRING
   DEFINE v_ax_id_cre_ug            DECIMAL(9,0)
   DEFINE v_id_dh_resp              DECIMAL(9,0)
   DEFINE v_id_cre_ug_resp          DECIMAL(9,0)
   DEFINE v_tot_cambio              INTEGER 
   DEFINE v_cadena                  STRING

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTS03.log")

   DISPLAY "=INICIA GRTS03="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY ""

   -- se inicializan variables
   LET v_d_sum_importe_v97 = 0 -- suma del importe de vivienda 97
   LET v_i_contrador_reg   = 0 -- contador de registros
   LET v_criterio          = 0
   LET v_f_liquida         = "12/31/1899"

   -- se crea tabla temporal la cual identifica que registros se procesaron en la generaci�n del archivo
   CALL fn_crea_tmp_solic_sdo_uso()

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados,ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'grt'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio, v_c_ruta_listado,v_ruta_bin

   -- se crea la fecha. Primer dia del mes
   LET v_dt_fec_present = TODAY - DAY(TODAY) + 1

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_dt_fec_present CLIPPED, "',2)" 

   PREPARE prp_obtiene_3habil FROM v_s_qryTxt
   EXECUTE prp_obtiene_3habil INTO v_dt_fec_present

   LET v_dt_fec_movimiento = v_dt_fec_present - DAY(v_dt_fec_present) + 1
   LET v_dt_fec_movimiento = v_dt_fec_movimiento + 1 UNITS MONTH

   DISPLAY " Fecha presentaci�n: ",v_dt_fec_present USING "DD/MM/YYYY"
   DISPLAY " Fecha movimiento  : ",v_dt_fec_movimiento USING "DD/MM/YYYY"

   -- se obtiene el precio de accion para el d�a de liquidaci�n
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11",
                    "    AND f_valuacion = '",v_dt_fec_movimiento,"'"

   PREPARE prp_precio_accion FROM v_s_qryTxt
   EXECUTE prp_precio_accion INTO v_d_precio_fondo

   -- se crea la fecha con formato YYYYMMDD
   LET v_c_fec_present_ax = v_dt_fec_present USING "YYYYMMDD"

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   --LET v_v_nom_archivo = "A" || v_c_fec_present_ax || ".usog"
   LET v_v_ruta_nomarch = v_c_ruta_envio CLIPPED || "/" || p_v_arch_proceso

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   -- se asignan los valores del registro encabezado a insertar
   LET v_r_encabezado.tpo_registro     = "01"
   LET v_r_encabezado.id_servicio      = "02"
   LET v_r_encabezado.id_operacion     = "01"
   LET v_r_encabezado.tpo_ent_origen   = "04"
   LET v_r_encabezado.cve_ent_origen   = "002"
   LET v_r_encabezado.tpo_ent_destino  = "01"
   LET v_r_encabezado.cve_ent_destino  = "" -- 3 espacios en blanco
   LET v_r_encabezado.ent_fed_env_lote = "009"
   LET v_r_encabezado.fec_presentacion = v_c_fec_present_ax
   LET v_r_encabezado.consec_lote      = "001"
   LET v_r_encabezado.cve_modal_recep  = "" --"02"
   LET v_r_encabezado.cod_resul_opera  = (2 SPACES) -- 2 espacios en blanco
   LET v_r_encabezado.mot_rechazo      = "" -- 9 espacios en blanco
   LET v_r_encabezado.filler           = "" -- 687 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_encabezado.tpo_registro,
                      v_r_encabezado.id_servicio,
                      v_r_encabezado.id_operacion,
                      v_r_encabezado.tpo_ent_origen,
                      v_r_encabezado.cve_ent_origen,
                      v_r_encabezado.tpo_ent_destino,
                      v_r_encabezado.cve_ent_destino,
                      v_r_encabezado.ent_fed_env_lote,
                      v_r_encabezado.fec_presentacion,
                      v_r_encabezado.consec_lote,
                      v_r_encabezado.cve_modal_recep,
                      v_r_encabezado.cod_resul_opera,
                      v_r_encabezado.mot_rechazo,
                      v_r_encabezado.filler

   -- se escribe el registro (encabezado) en el archivo
   CALL v_ch_arch_solTransf.write([v_s_registro])

   -- se obtiene la fecha del ultimo d�a del mes anterior de la fecha de presentaci�n
   LET v_dt_f_solic_saldos = v_dt_fec_present - DAY(v_dt_fec_present)
   LET v_dt_f_solic_saldos = TODAY

   LET v_r_detalle.tpo_registro      = "02"
   LET v_r_detalle.tpo_ent_recept    = "04"
   LET v_r_detalle.cve_ent_recept    = "002"
   LET v_r_detalle.tpo_ent_cedente   = "01"
   LET v_r_detalle.cve_ent_cedente   = "" -- 3 espacios en blanco
   LET v_r_detalle.tpo_transferencia = "18" -- siempre se va "18" v_r_cre_uso_garantia.tpo_transferencia -- 2 espacios en blanco
   LET v_r_detalle.f_presentacion    = v_c_fec_present_ax
   LET v_r_detalle.f_movimiento      = v_dt_fec_movimiento USING "yyyymmdd"
   LET v_r_detalle.filler2           = "" -- 15 espacios en blanco
   LET v_r_detalle.filler3           = "" -- 22 espacios en blanco
   LET v_r_detalle.id_lote_solic     = "04002" || v_c_fec_present_ax || "001"
   LET v_r_detalle.filler4           = "" -- 15 espacios en blanco 
   LET v_r_detalle.nss_trab_afore    = "" -- 1a espacios en blanco. No se sabe el nss afore
   LET v_r_detalle.rfc_trab_afore    = "" -- 13 espacios en blanco. No se sabe el rfc afore
   LET v_r_detalle.filler5           = "" -- 30 espacios en blanco
   LET v_r_detalle.ape_pat_afore     = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
   LET v_r_detalle.ape_mat_afore     = "" -- 40 espacios en blanco. No se sabe el ap paterno afore
   LET v_r_detalle.nom_trab_afore    = "" -- 40 espacios en blanco. No se sabe el nombre afore
   LET v_r_detalle.filler6           = "" -- 30 espacios en blanco
   LET v_r_detalle.num_apl_int_viv97 = "000000000000000"
   LET v_r_detalle.filler7           = "" -- 78 espacios en blanco
   LET v_r_detalle.cod_result_opera  = "" -- 2 espacios en blanco
   LET v_r_detalle.diagnostico_proc  = "" -- 15 espacios en blanco
   LET v_r_detalle.filler8           = "" -- 53 espacios en blanco
   LET v_r_detalle.filler9           = "" -- 12 espacios en blanco

   -- se consultan todos los registro de cre_uso_garantia con edo_procesar = 70 y con estado = 140, Registros con adelantos
   --REGISTROS CON USO DE GARANT�A CON ADELANTO

   DISPLAY ""
   DISPLAY " REGISTROS CON USO DE ANUALIDAD CON ADELANTO"
   LET v_s_qryTxt = " SELECT UNIQUE id_derechohabiente, num_credito,\n",
                    "        periodo_pago, tpo_transferencia\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado = 140\n",
                    "    AND edo_procesar IN (10, 70)\n",
                    "    AND tpo_transferencia IN ('18','48')\n"
                    ---"    AND f_proceso <= '",v_dt_f_solic_saldos,"'"

   PREPARE prp_uniq_cre_uso FROM v_s_qryTxt
   DECLARE cur_uniq_cre_uso CURSOR FOR prp_uniq_cre_uso

   LET v_tot_cambio = 0
   LET v_cadena     = NULL 

   FOREACH cur_uniq_cre_uso INTO v_r_cre_uso_garantia.*
   ---DISPLAY v_r_cre_uso_garantia.id_derhabiente," ",v_r_cre_uso_garantia.num_credito," ",v_r_cre_uso_garantia.periodo_pago," ",v_r_cre_uso_garantia.tpo_transferencia

      -- se obtiene el primer registros con las condiciones �nicas
      LET v_s_qryTxt = " SELECT FIRST 1 id_cre_uso_garantia\n",
                       "   FROM cre_uso_garantia\n",
                       "  WHERE id_derechohabiente = ",v_r_cre_uso_garantia.id_derhabiente,"\n",
                       " AND estado             = 140\n",
                       " AND edo_procesar      IN (10, 70)\n",
                       " AND tpo_transferencia  = ",v_r_cre_uso_garantia.tpo_transferencia,"\n",
                       " AND num_credito        = ",v_r_cre_uso_garantia.num_credito,"\n",
                       " AND periodo_pago       = ",v_r_cre_uso_garantia.periodo_pago
--DISPLAY ""
--DISPLAY v_s_qryTxt
--DISPLAY ""

      PREPARE prp_sqlFrst_140 FROM v_s_qryTxt
      EXECUTE prp_sqlFrst_140 INTO v_d_id_cre_uso_garant

      SELECT folio_liquida,
             id_cre_uso_garantia,
             edo_procesar
        INTO v_folio,
             v_d_id_cre_uso_garant,
             v_si_edo_procesar
        FROM cre_uso_garantia
       WHERE id_cre_uso_garantia = v_d_id_cre_uso_garant

      LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_uso_unificacion(?,?)"

      PREPARE prp_exe_unif FROM v_s_qryTxt
      EXECUTE prp_exe_unif USING v_r_cre_uso_garantia.id_derhabiente,
                                 v_d_id_cre_uso_garant INTO v_id_dh_resp,
                                                            v_id_cre_ug_resp

      LET v_r_cre_uso_garantia.id_derhabiente = v_id_dh_resp
      LET v_d_id_cre_uso_garant               = v_id_cre_ug_resp

      LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

      PREPARE prp_obt_mov FROM v_s_qryTxt
      EXECUTE prp_obt_mov USING v_criterio,
                                v_folio,
                                v_f_liquida
                           INTO v_tabla

      --Verifica si el registro tuvo un cambio de marca 225 a 223
      SELECT COUNT(*)
        INTO v_tot_cambio
        FROM cre_cambio_marca
       WHERE id_referencia = v_d_id_cre_uso_garant
         AND id_derechohabiente = v_r_cre_uso_garantia.id_derhabiente
         AND marca_origen = 225
         AND marca_final  = 223;

      IF(v_tot_cambio >= 1) THEN
         LET v_cadena = "    AND id_referencia = ",v_d_id_cre_uso_garant
      ELSE 
        LET v_cadena = "    AND id_referencia = ",v_r_cre_uso_garantia.periodo_pago
      END IF 
         
      LET v_s_qryTxt = " SELECT SUM(monto_acciones)\n",
                       "   FROM ",v_tabla,"\n",
                       "  WHERE folio_liquida = ",v_folio,"\n",
                        v_cadena CLIPPED ,"\n",
                       "    AND id_derechohabiente = ",v_id_dh_resp

      PREPARE prp_slc_sdoliq FROM v_s_qryTxt
      EXECUTE prp_slc_sdoliq INTO v_d_aivs_v97

      -- se valida el importe obtenido
      IF v_d_aivs_v97 IS NULL OR v_d_aivs_v97 = 0 THEN
         LET v_d_aivs_v97 = 0

         CONTINUE FOREACH
      ELSE
         -- se obtiene el importe en pesos
         LET v_d_importe_v97 = v_d_aivs_v97 * v_d_precio_fondo * -1

         -- se invoca la funcion que obtiene los datos del trabajador
         CALL f_obt_datos_trab(v_r_cre_uso_garantia.id_derhabiente) RETURNING v_r_afi_derechohab.*

         -- se incrementa el contador de registro y el acumulador del importe
         LET v_i_contrador_reg   = v_i_contrador_reg + 1
         LET v_d_sum_importe_v97 = v_d_sum_importe_v97 + v_d_importe_v97

         -- se asignan los valores en el registro detalle
         LET v_r_detalle.cont_servicio      = v_i_contrador_reg USING "&&&&&&&&&&"
         LET v_r_detalle.curp_trabajador    = v_r_afi_derechohab.curp
         LET v_r_detalle.nss_trab_infonavit = v_r_afi_derechohab.nss
         LET v_r_detalle.rfc_trab_infonavit = v_r_afi_derechohab.rfc
         LET v_r_detalle.ape_pat_infonavit  = v_r_afi_derechohab.ap_paterno_af
         LET v_r_detalle.ape_mat_infonavit  = v_r_afi_derechohab.ap_materno_af
         LET v_r_detalle.nom_trab_infonavit = v_r_afi_derechohab.nombre_af
         LET v_r_detalle.ult_aport_vivi97   = v_d_importe_v97 * 100 USING "&&&&&&&&&&&&&&&"  --MMC
         LET v_r_detalle.nom_trab_imss      = v_r_afi_derechohab.nombre_imss
         LET v_r_detalle.num_cred_infonavit = v_r_cre_uso_garantia.num_credito USING "&&&&&&&&&&"
         LET v_r_detalle.periodo_pago       = v_r_cre_uso_garantia.periodo_pago -- 6 espacios en blanco

         -- se concatenan los campos a insertar
         LET v_s_registro = v_r_detalle.tpo_registro,
                            v_r_detalle.cont_servicio,
                            v_r_detalle.tpo_ent_recept,
                            v_r_detalle.cve_ent_recept,
                            v_r_detalle.tpo_ent_cedente,
                            v_r_detalle.cve_ent_cedente,
                            v_r_detalle.tpo_transferencia,
                            v_r_detalle.f_presentacion,
                            v_r_detalle.f_movimiento,
                            v_r_detalle.curp_trabajador,
                            v_r_detalle.nss_trab_infonavit,
                            v_r_detalle.filler2,
                            v_r_detalle.rfc_trab_infonavit,
                            v_r_detalle.ape_pat_infonavit,
                            v_r_detalle.ape_mat_infonavit,
                            v_r_detalle.nom_trab_infonavit,
                            v_r_detalle.filler3,
                            v_r_detalle.id_lote_solic,
                            v_r_detalle.filler4,
                            v_r_detalle.nss_trab_afore,
                            v_r_detalle.rfc_trab_afore,
                            v_r_detalle.filler5,
                            v_r_detalle.ape_pat_afore,
                            v_r_detalle.ape_mat_afore,
                            v_r_detalle.nom_trab_afore,
                            v_r_detalle.filler6,
                            v_r_detalle.num_apl_int_viv97,
                            v_r_detalle.ult_aport_vivi97,
                            v_r_detalle.filler7,
                            v_r_detalle.cod_result_opera,
                            v_r_detalle.diagnostico_proc,
                            v_r_detalle.nom_trab_imss,
                            v_r_detalle.num_cred_infonavit,
                            v_r_detalle.filler8,
                            v_r_detalle.periodo_pago,
                            v_r_detalle.filler9

         -- se escribe el registro (detalle) en el archivo
         CALL v_ch_arch_solTransf.write([v_s_registro])

         -- se inserta el registro en la tabla temporal
         LET v_r_tmp_uso_solic_sdo.nss                 = v_r_afi_derechohab.nss
         LET v_r_tmp_uso_solic_sdo.id_derechohabiente  = v_r_cre_uso_garantia.id_derhabiente
         LET v_r_tmp_uso_solic_sdo.id_cre_uso_garantia = v_d_id_cre_uso_garant
         LET v_r_tmp_uso_solic_sdo.periodo_pago        = v_r_detalle.periodo_pago
         LET v_r_tmp_uso_solic_sdo.modulo_cod          = "UG"
         LET v_r_tmp_uso_solic_sdo.f_proceso           = TODAY

         -- se inserta el registro en la tabla temporal
         INSERT INTO safre_tmp:tmp_uso_solic_sdo
         VALUES (v_r_tmp_uso_solic_sdo.*)

         #Guarda la petici�n en hist�rico (Modificaci�n Emilio Abarca,EFP)
         INSERT INTO cre_his_solic_sdo(
                        id_referencia      ,
                        id_derechohabiente , 
                        nss                , 
                        modulo_cod         ,
                        aivs92             ,
                        aivs97             ,
                        pesos92            ,
                        pesos97            ,
                        periodo_pago       ,
                        f_proceso) 
               VALUES (v_r_tmp_uso_solic_sdo.id_cre_uso_garantia,  
                        v_r_tmp_uso_solic_sdo.id_derechohabiente ,
                        v_r_tmp_uso_solic_sdo.nss                ,
                        'UG'                                     ,
                        0                                        , -- No aplica
                        v_d_aivs_v97                             ,
                        0                                        , -- No aplica
                        v_d_importe_v97                          ,
                        v_r_tmp_uso_solic_sdo.periodo_pago       ,
                        TODAY);

         -- se valida el estado procesar
         IF v_si_edo_procesar = 70 THEN
            LET v_edo_procesar_aux = 85
         ELSE
            LET v_edo_procesar_aux = 80
         END IF

         -- se actualiza el registro le�do de cre acreditado
         UPDATE cre_uso_garantia
            SET edo_procesar        = v_edo_procesar_aux
          WHERE id_cre_uso_garantia = v_d_id_cre_uso_garant
      END IF

      LET v_folio                                = 0
      LET v_d_id_cre_uso_garant                  = 0
      LET v_si_edo_procesar                      = 0
      LET v_d_aivs_v97                           = 0
      LET v_r_cre_uso_garantia.id_derhabiente    = 0
      LET v_r_cre_uso_garantia.tpo_transferencia = 0
      LET v_r_cre_uso_garantia.num_credito       = 0
      LET v_r_cre_uso_garantia.periodo_pago      = 0
   END FOREACH

   -- se consultan  todos los registro de cre_uso_garantia con estado = 20 Registros sin adelantos
   --REGISTROS CON USO DE ANUALIDAD SIN ADELANTO

   DISPLAY " REGISTROS CON USO DE ANUALIDAD SIN ADELANTO"
   DISPLAY ""

   LET v_s_qryTxt = " SELECT UNIQUE id_derechohabiente, num_credito,\n",
                    "        periodo_pago, tpo_transferencia\n",
                    "   FROM cre_uso_garantia\n",
                    "  WHERE estado IN(20, 142)\n",
                    "    AND edo_procesar IN (10, 70)\n",
                    "    AND tpo_transferencia IN ('18','48')"

   PREPARE prp_uniq_cre_uso_no FROM v_s_qryTxt
   DECLARE cur_uniq_cre_uso_no CURSOR FOR prp_uniq_cre_uso_no

   FOREACH cur_uniq_cre_uso_no INTO v_r_cre_uso_garantia.*
   --DISPLAY v_r_cre_uso_garantia.id_derhabiente," ",v_r_cre_uso_garantia.num_credito," ",v_r_cre_uso_garantia.periodo_pago," ",v_r_cre_uso_garantia.tpo_transferencia

      -- se obtiene el primer registros con las condiciones �nicas
      LET v_s_qryTxt = " SELECT FIRST 1 id_cre_uso_garantia\n",
                       "   FROM cre_uso_garantia\n",
                       "  WHERE id_derechohabiente = ",v_r_cre_uso_garantia.id_derhabiente,"\n",
                       " AND estado            IN (20, 142)\n",
                       " AND edo_procesar      IN (10, 70)\n",
                       " AND tpo_transferencia  = ",v_r_cre_uso_garantia.tpo_transferencia,"\n",
                       " AND num_credito        = ",v_r_cre_uso_garantia.num_credito,"\n",
                       " AND periodo_pago       = ",v_r_cre_uso_garantia.periodo_pago

      PREPARE prp_sqlFrst_142 FROM v_s_qryTxt
      EXECUTE prp_sqlFrst_142 INTO v_d_id_cre_uso_garant

---display "id_cre_uso: ", v_d_id_cre_uso_garant

      SELECT importe_v97,
             id_cre_uso_garantia,
             edo_procesar
        INTO v_d_importe_v97,
             v_d_id_cre_uso_garant,
             v_si_edo_procesar
        FROM cre_uso_garantia
       WHERE id_cre_uso_garantia = v_d_id_cre_uso_garant

      LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_uso_unificacion(?,?)"

      PREPARE prp_exe_unif_no FROM v_s_qryTxt
      EXECUTE prp_exe_unif_no USING v_r_cre_uso_garantia.id_derhabiente,
                                 v_d_id_cre_uso_garant INTO v_id_dh_resp,
                                                            v_id_cre_ug_resp

      LET v_r_cre_uso_garantia.id_derhabiente = v_id_dh_resp
      LET v_d_id_cre_uso_garant               = v_id_cre_ug_resp

      -- se invoca la funcion que obtiene los datos del trabajador
      CALL f_obt_datos_trab(v_r_cre_uso_garantia.id_derhabiente) RETURNING v_r_afi_derechohab.*

      -- Calcula AIVS (Modificaci�n Emilio Abarca,EFP)
      LET v_d_aivs_v97 = (v_d_importe_v97 / v_d_precio_fondo)
      
      -- se incrementa el contador de registro y el acumulador del importe
      LET v_i_contrador_reg   = v_i_contrador_reg + 1
      LET v_d_sum_importe_v97 = v_d_sum_importe_v97 + v_d_importe_v97

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.cont_servicio      = v_i_contrador_reg USING "&&&&&&&&&&"
      LET v_r_detalle.curp_trabajador    = v_r_afi_derechohab.curp
      LET v_r_detalle.nss_trab_infonavit = v_r_afi_derechohab.nss
      LET v_r_detalle.rfc_trab_infonavit = v_r_afi_derechohab.rfc
      LET v_r_detalle.ape_pat_infonavit  = v_r_afi_derechohab.ap_paterno_af
      LET v_r_detalle.ape_mat_infonavit  = v_r_afi_derechohab.ap_materno_af
      LET v_r_detalle.nom_trab_infonavit = v_r_afi_derechohab.nombre_af
      LET v_r_detalle.ult_aport_vivi97   = v_d_importe_v97 * 100 USING "&&&&&&&&&&&&&&&"  --MMC
      LET v_r_detalle.nom_trab_imss      = v_r_afi_derechohab.nombre_imss
      LET v_r_detalle.num_cred_infonavit = v_r_cre_uso_garantia.num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.periodo_pago       = v_r_cre_uso_garantia.periodo_pago -- 6 espacios en blanco

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tpo_registro,
                         v_r_detalle.cont_servicio,
                         v_r_detalle.tpo_ent_recept,
                         v_r_detalle.cve_ent_recept,
                         v_r_detalle.tpo_ent_cedente,
                         v_r_detalle.cve_ent_cedente,
                         v_r_detalle.tpo_transferencia,
                         v_r_detalle.f_presentacion,
                         v_r_detalle.f_movimiento,
                         v_r_detalle.curp_trabajador,
                         v_r_detalle.nss_trab_infonavit,
                         v_r_detalle.filler2,
                         v_r_detalle.rfc_trab_infonavit,
                         v_r_detalle.ape_pat_infonavit,
                         v_r_detalle.ape_mat_infonavit,
                         v_r_detalle.nom_trab_infonavit,
                         v_r_detalle.filler3,
                         v_r_detalle.id_lote_solic,
                         v_r_detalle.filler4,
                         v_r_detalle.nss_trab_afore,
                         v_r_detalle.rfc_trab_afore,
                         v_r_detalle.filler5,
                         v_r_detalle.ape_pat_afore,
                         v_r_detalle.ape_mat_afore,
                         v_r_detalle.nom_trab_afore,
                         v_r_detalle.filler6,
                         v_r_detalle.num_apl_int_viv97,
                         v_r_detalle.ult_aport_vivi97,
                         v_r_detalle.filler7,
                         v_r_detalle.cod_result_opera,
                         v_r_detalle.diagnostico_proc,
                         v_r_detalle.nom_trab_imss,
                         v_r_detalle.num_cred_infonavit,
                         v_r_detalle.filler8,
                         v_r_detalle.periodo_pago,
                         v_r_detalle.filler9

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- se inserta el registro en la tabla temporal
      LET v_r_tmp_uso_solic_sdo.nss                 = v_r_afi_derechohab.nss
      LET v_r_tmp_uso_solic_sdo.id_derechohabiente  = v_r_cre_uso_garantia.id_derhabiente
      LET v_r_tmp_uso_solic_sdo.id_cre_uso_garantia = v_d_id_cre_uso_garant
      LET v_r_tmp_uso_solic_sdo.periodo_pago        = v_r_detalle.periodo_pago
      LET v_r_tmp_uso_solic_sdo.modulo_cod          = "UG"
      LET v_r_tmp_uso_solic_sdo.f_proceso           = TODAY

      -- se inserta el registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_uso_solic_sdo
      VALUES (v_r_tmp_uso_solic_sdo.*)

      #Guarda la petici�n en hist�rico (Modificaci�n Emilio Abarca,EFP)
      INSERT INTO cre_his_solic_sdo(
                     id_referencia      ,
                     id_derechohabiente , 
                     nss                , 
                     modulo_cod         ,
                     aivs92             ,
                     aivs97             ,
                     pesos92            ,
                     pesos97            ,
                     periodo_pago       ,
                     f_proceso) 
            VALUES (v_r_tmp_uso_solic_sdo.id_cre_uso_garantia,  
                     v_r_tmp_uso_solic_sdo.id_derechohabiente ,
                     v_r_tmp_uso_solic_sdo.nss                ,
                     'UG'                                     ,
                     0                                        , -- No aplica
                     v_d_aivs_v97                             ,
                     0                                        , -- No aplica
                     v_d_importe_v97                          ,
                     v_r_tmp_uso_solic_sdo.periodo_pago       ,
                     TODAY);

      -- se valida el estado procesar
      IF v_si_edo_procesar = 70 THEN
         LET v_edo_procesar_aux = 85
      ELSE
         LET v_edo_procesar_aux = 80
      END IF

      -- se actualiza el registro le�do de cre acreditado
      UPDATE cre_uso_garantia
         SET edo_procesar        = v_edo_procesar_aux
       WHERE id_cre_uso_garantia = v_d_id_cre_uso_garant

      LET v_folio                                = 0
      LET v_d_id_cre_uso_garant                  = 0
      LET v_si_edo_procesar                      = 0
      LET v_d_aivs_v97                           = 0
      LET v_r_cre_uso_garantia.id_derhabiente    = 0
      LET v_r_cre_uso_garantia.tpo_transferencia = 0
      LET v_r_cre_uso_garantia.num_credito       = 0
      LET v_r_cre_uso_garantia.periodo_pago      = 0
   END FOREACH

   -- se asignan los valores del registro sumario
   LET v_r_sumario.tpo_registro        = "09"
   LET v_r_sumario.cant_regs_detalle   = v_i_contrador_reg USING "&&&&&&&&&"
   LET v_r_sumario.filler1             = "" -- 30 espacios en blanco
   LET v_r_sumario.sum_numAplInt_viv97 = "000000000000000"
   LET v_r_sumario.sum_ult_aport_viv97 = (v_d_sum_importe_v97 * 100) USING "&&&&&&&&&&&&&&&"
   LET v_r_sumario.filler2             = "" -- 659 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_sumario.tpo_registro,
                      v_r_sumario.cant_regs_detalle,
                      v_r_sumario.filler1,
                      v_r_sumario.sum_numAplInt_viv97,
                      v_r_sumario.sum_ult_aport_viv97,
                      v_r_sumario.filler2

   -- se escribe el registro (sumario) en el archivo
   CALL v_ch_arch_solTransf.write([v_s_registro])

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   -- se obtiene la extensi�n del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo (COPIA) y posteriormente se concatena con la ruta
   LET v_v_ruta_nomarch_cp = v_c_ruta_envio CLIPPED || "/" || "sol_ug." || v_c_extension

   -- se copia el archivo en la misma ruta pero con nombre requerido por Infonavit
   LET v_s_comando = "cp ", v_v_ruta_nomarch, " ", v_v_ruta_nomarch_cp

   -- se ejecuta el comando armado
   RUN v_s_comando

   -- se obtiene el maximo lote para la fecha de presentaci�n
   LET v_s_qryTxt = " SELECT MAX(lote)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE f_lote = '",v_dt_fec_present,"'\n",
                    "    AND id_proceso = ",g_proc_cod_grt_uso_arch_solic

   PREPARE prp_max_lote FROM v_s_qryTxt
   EXECUTE prp_max_lote INTO v_i_lote

   -- se valida el lote
   IF v_i_lote IS NULL THEN
      LET v_i_lote = 1
   ELSE
      LET v_i_lote = v_i_lote + 1
   END IF

   DISPLAY " Total de registros por enviar: ",v_i_contrador_reg
   DISPLAY " Suma total de saldo viv 97: ",v_d_sum_importe_v97

  {
   DISPLAY " Ejecutando env�o interfaz PROCESAR"

   LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/sol_ug.sh"
   RUN v_ejecuta_sh

   DISPLAY ""
  }

   --agregar nuevos campos de la tabla cre ctr archivo
   LET v_r_cre_ctr_archivo.folio_archivo  = p_d_folio
   LET v_r_cre_ctr_archivo.lote           = v_i_lote
   LET v_r_cre_ctr_archivo.f_lote         = v_dt_fec_present
   LET v_r_cre_ctr_archivo.id_proceso     = g_proc_cod_grt_uso_arch_solic
   LET v_r_cre_ctr_archivo.operacion      = 0
   LET v_r_cre_ctr_archivo.nom_archivo    = p_v_arch_proceso
   LET v_r_cre_ctr_archivo.tot_registros  = v_i_contrador_reg
   LET v_r_cre_ctr_archivo.tot_aceptados  = 0
   LET v_r_cre_ctr_archivo.tot_rechazados = 0
   LET v_r_cre_ctr_archivo.tot_sin_origen = 0
   LET v_r_cre_ctr_archivo.estado         = 10
   LET v_r_cre_ctr_archivo.f_proceso      = TODAY
   LET v_r_cre_ctr_archivo.usuario        = p_v_usuario

   -- se inserta el registro en la tabla de control
   INSERT INTO cre_ctr_archivo VALUES (seq_cre_archivo.NEXTVAL,
                                       v_r_cre_ctr_archivo.folio_archivo,
                                       v_r_cre_ctr_archivo.lote,
                                       v_r_cre_ctr_archivo.f_lote,
                                       v_r_cre_ctr_archivo.id_proceso,
                                       v_r_cre_ctr_archivo.operacion,
                                       v_r_cre_ctr_archivo.nom_archivo,
                                       v_r_cre_ctr_archivo.tot_registros,
                                       v_r_cre_ctr_archivo.tot_aceptados,
                                       v_r_cre_ctr_archivo.tot_rechazados,
                                       v_r_cre_ctr_archivo.tot_sin_origen,
                                       v_r_cre_ctr_archivo.estado,
                                       v_r_cre_ctr_archivo.f_proceso,
                                       v_r_cre_ctr_archivo.usuario)

   --Ejecuta funci�n que genera PDF y archivo de salida
   CALL inf_sol_reporte()
   DISPLAY ""
   DISPLAY " Genera reporte PDF ...completado"
   DISPLAY ""
   DISPLAY " El archivo detalle de salida podr� ser recuperado en la ruta"
   DISPLAY " /safreviv_int/grt/envio con el siguiente nombre: ",v_arh_salida
   DISPLAY ""
   DISPLAY "=FIN="
   DISPLAY ""

END MAIN

#Objetivo: Funci�n que recupera informaci�n de las peticiones para el PDF
FUNCTION inf_sol_reporte()

   DEFINE v_reporte_bin  STRING
   DEFINE v_ruta_rpt     STRING
   --DEFINE v_hoy        DATE
   --DEFINE v_f_pivote     DATE
   --DEFINE v_s_qryTxt     STRING
   DEFINE r_solicitud    RECORD
      id_referencia      DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      periodo_pago       CHAR(6),
      estado             SMALLINT,
      edo_procesar       SMALLINT
   END RECORD

   DEFINE r_inf_archivo RECORD
      f_genera      DATE,
      nss           CHAR(11),
      pesos         CHAR(15),
      periodo       CHAR(6),
      tipo          CHAR(2),
      num_intento   CHAR(4)
   END RECORD

   DEFINE v_aux_pesos    DECIMAL(12,2)
   DEFINE v_ruta_archivo STRING
   DEFINE archivo        base.Channel
   DEFINE v_detalle      STRING

   --Inicializa valores del arreglo
   --Estado = 20
   LET arr_sol_saldo[1].estado_desc = "Solicitud Nueva UG/Anualidad"
   LET arr_sol_saldo[1].t_registros = 0
   LET arr_sol_saldo[1].pesos       = 0
   --Estado = 140
   LET arr_sol_saldo[2].estado_desc = "Conciliaci�n por Adelanto"
   LET arr_sol_saldo[2].t_registros = 0
   LET arr_sol_saldo[2].pesos       = 0
   --Estado = 142
   LET arr_sol_saldo[3].estado_desc = "Casos Especiales"
   LET arr_sol_saldo[3].t_registros = 0
   LET arr_sol_saldo[3].pesos       = 0

   --Inicializa record del total global
   LET r_total_global.t_registros = 0
   LET r_total_global.pesos       = 0

  { #Calcula fecha para solicitudes de meses anteriores
   LET v_hoy = TODAY

   --Obtiene fecha del primer d�a h�bil del mes
   LET v_f_pivote = v_hoy - DAY (v_hoy) + 1

   --Obtiene fecha del mes anterior
   LET v_f_pivote = v_f_pivote -1 UNITS MONTH

   -- se crea la sentencia que invoca "funcion habil siguiente" que le suma 3 dias habiles
   LET v_s_qryTxt = " EXECUTE FUNCTION fn_habil_siguiente('",v_f_pivote CLIPPED, "',2)"

   PREPARE prp_dia_habil FROM v_s_qryTxt
   EXECUTE prp_dia_habil INTO v_f_pivote

   DISPLAY ""
   DISPLAY " Fecha pivote mes anterior: ",v_f_pivote}

   LET v_arh_salida   = "Adetalle_",TODAY USING "yyyymmdd",".cgt"
   LET v_ruta_archivo = v_c_ruta_envio CLIPPED,"/",v_arh_salida

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   DECLARE crs_tipo_UG CURSOR FOR
   SELECT t.id_cre_uso_garantia,
          t.id_derechohabiente,
          t.nss,
          t.periodo_pago,
          u.estado,
          u.edo_procesar
     FROM safre_tmp:tmp_uso_solic_sdo t,
          cre_uso_garantia u
    WHERE t.id_cre_uso_garantia = u.id_cre_uso_garantia;

   INITIALIZE r_solicitud.* TO NULL
   INITIALIZE r_inf_archivo.* TO NULL

   LET r_inf_archivo.f_genera    = TODAY
   LET r_inf_archivo.num_intento = "0001"
   LET v_aux_pesos               = 0
   LET v_detalle                 = NULL

   FOREACH crs_tipo_UG INTO r_solicitud.id_referencia,
                            r_solicitud.id_derechohabiente,
                            r_solicitud.nss,
                            r_solicitud.periodo_pago,
                            r_solicitud.estado,
                            r_solicitud.edo_procesar

      LET r_inf_archivo.tipo = "  "
      LET v_aux_pesos = 0

      --Recupera el monto pesos de hist�rico
      SELECT MAX(pesos97)
        INTO v_aux_pesos
        FROM cre_his_solic_sdo
       WHERE id_referencia      = r_solicitud.id_referencia
         AND id_derechohabiente = r_solicitud.id_derechohabiente
         AND modulo_cod = 'UG'
         AND f_proceso  = TODAY;

      IF(v_aux_pesos IS NULL) THEN
         LET v_aux_pesos = 0
      END IF

      --Asigna valores para el archivo
      LET r_inf_archivo.nss     = r_solicitud.nss
      LET r_inf_archivo.pesos   = v_aux_pesos
      LET r_inf_archivo.periodo = r_solicitud.periodo_pago

      #Incrementa totales globales
      LET r_total_global.t_registros = r_total_global.t_registros + 1
      LET r_total_global.pesos       = r_total_global.pesos + v_aux_pesos

      #Eval�a el tipo de solicitud del mes actual
      CASE
         --Solicitud nueva
         WHEN r_solicitud.estado = 20
            LET arr_sol_saldo[1].t_registros = arr_sol_saldo[1].t_registros + 1
            LET arr_sol_saldo[1].pesos       = arr_sol_saldo[1].pesos + v_aux_pesos
            LET r_inf_archivo.tipo = "UA"

         --Conciliaci�n por adelanto
         WHEN r_solicitud.estado = 140
            LET arr_sol_saldo[2].t_registros = arr_sol_saldo[2].t_registros + 1
            LET arr_sol_saldo[2].pesos       = arr_sol_saldo[2].pesos + v_aux_pesos
            LET r_inf_archivo.tipo = "CA"

         --Caso Especial
         WHEN r_solicitud.estado = 142
            LET arr_sol_saldo[3].t_registros = arr_sol_saldo[3].t_registros + 1
            LET arr_sol_saldo[3].pesos      = arr_sol_saldo[3].pesos + v_aux_pesos
            LET r_inf_archivo.tipo = "CE"

      END CASE

      LET v_detalle = r_inf_archivo.f_genera USING "yyyymmdd",
                      r_inf_archivo.nss,
                      r_inf_archivo.pesos USING "&&&&&&&&&.&&&&&",
                      r_inf_archivo.periodo,
                      r_inf_archivo.tipo,
                      r_inf_archivo.num_intento

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)

   END FOREACH

   CALL archivo.close()

   #Calcula porcentajes %
   LET v_aux_porcentaje = 0
   --Porcentaje total global
   LET v_aux_porcentaje = (r_total_global.t_registros / r_total_global.t_registros) * 100
   LET r_total_global.porcentaje = v_aux_porcentaje CLIPPED,"%"

   --finaliza operaci�n
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
   END IF

   SELECT fecha_ini,fecha_fin
     INTO v_f_opera_ini,v_f_opera_fin
     FROM bat_ctr_operacion
    WHERE pid         = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod   = p_i_opera_cod

   #################################################
   #   CONFIGURACION PARA SALIDA DEL REPORTE PDF   #
   #################################################
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/GRTS031.4rp"
   LET v_ruta_rpt    = v_c_ruta_listado CLIPPED,"/",
                       p_v_usuario CLIPPED,"-",v_c_programa_cod CLIPPED,"-",
                       p_d_pid USING "&&&&&","-",
                       p_i_proceso_cod USING "&&&&&","-",
                       p_i_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN

         START REPORT genera_PDF TO XML HANDLER v_manejador_rpt

            OUTPUT TO REPORT genera_PDF()

         FINISH REPORT genera_PDF

      END IF
   ELSE
       DISPLAY "ERROR: No fue posible abrir plantilla del reporte"

       LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      
   END IF

END FUNCTION

#OBJETIVO: Genera Reporte PDF
REPORT genera_PDF()

   DEFINE v_f_presentacion DATE
   DEFINE v_desc_operacion CHAR(60)
   DEFINE f                INTEGER

   FORMAT
   FIRST PAGE HEADER
      LET v_f_presentacion = TODAY
      LET v_desc_operacion = "Solicitud de Saldos GRT"

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX v_f_presentacion USING "dd/mm/yyyy"

      #RESUMEN
      PRINTX p_v_arch_proceso           --Nombre del archivo
      PRINTX v_f_opera_ini
      PRINTX v_f_opera_fin
      PRINTX v_desc_operacion
      PRINTX r_total_global.t_registros

      IF(r_total_global.pesos < 0) THEN
         LET r_total_global.pesos = r_total_global.pesos * -1 
      END IF 
      
      PRINTX r_total_global.pesos
      PRINTX r_total_global.porcentaje

   ON EVERY ROW
      --rechazos solicitudes nuevas
      FOR f = 1 TO arr_sol_saldo.getLength()

         PRINTX arr_sol_saldo[f].estado_desc
         PRINTX arr_sol_saldo[f].t_registros

         IF(arr_sol_saldo[f].pesos < 0) THEN
            LET arr_sol_saldo[f].pesos = arr_sol_saldo[f].pesos * -1 
         END IF 
         
         PRINTX arr_sol_saldo[f].pesos

         --Calcula porcentaje por cada fila
         LET v_aux_porcentaje = 0
         LET v_aux_porcentaje = (arr_sol_saldo[f].t_registros / r_total_global.t_registros) * 100
         LET arr_sol_saldo[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_sol_saldo[f].porcentaje
         
      END FOR

END REPORT

#Objetivo: Funcion que consulta los datos de la tabla afi derechohabiente
FUNCTION f_obt_datos_trab(p_id_derechohabiente)

   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente -- identificador del derechohabiente 
   DEFINE v_r_afi_derechohab   RECORD LIKE afi_derechohabiente.*           -- registro de afi derechohabiente
   --DEFINE v_s_qryTxt           STRING -- guarda una sentencia sql a ejecutar

   -- se asigna la sentencia que obtiene la informaci�n del trabajador
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM afi_derechohabiente\n",
                    "  WHERE id_derechohabiente = ",p_id_derechohabiente

   PREPARE prp_afi_derechohabiente FROM v_s_qryTxt
   EXECUTE prp_afi_derechohabiente INTO v_r_afi_derechohab.*

   RETURN v_r_afi_derechohab.*

END FUNCTION

#Objetivo: Funci�n que crea tabla temporal la cual tiene por objetivo diferenciar el m�dulo
#          correspondiente a cada NSS que llega de Infonavit
FUNCTION fn_crea_tmp_solic_sdo_uso()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   LET v_s_qryTxt = "EXECUTE PROCEDURE fn_crea_tmp_solic_sdo_uso2()"

   PREPARE prp_cre_tabla2 FROM v_s_qryTxt
   EXECUTE prp_cre_tabla2

   INSERT INTO tmp_uso_solic_sdo2
   SELECT *
     FROM tmp_uso_solic_sdo

   UNLOAD TO "tmp_uso_solic_sdo.unl"
   SELECT *
     FROM safre_tmp:tmp_uso_solic_sdo

   LET v_s_qryTxt = "EXECUTE PROCEDURE fn_crea_tmp_solic_sdo_uso()"

   PREPARE prp_cre_tabla FROM v_s_qryTxt
   EXECUTE prp_cre_tabla

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION
