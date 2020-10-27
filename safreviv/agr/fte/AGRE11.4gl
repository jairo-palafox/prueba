--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRE11                                        #
#Objetivo          =>Programa que permite la integración del       #
#                    archivo Marca y Desmarca para el módulo de    #
#                    Anualidades Garantizadas                      #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>24 Septiembre 2012                            #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

DEFINE g_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
       g_d_pid              LIKE bat_ctr_proceso.pid, -- pid
       g_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion
       g_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
       g_v_arch_proceso     VARCHAR(100), -- nombre del archivo a integrar
       g_id_cre_ctr_archivo LIKE cre_acreditado.id_cre_ctr_archivo, -- id del archivo
       g_c_ruta_bin         LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
       g_c_ruta_listados    LIKE seg_modulo.ruta_listados -- ruta de listados del módulo

#Objetivo: Función que realiza la integracion del archivo recurrente
MAIN
   DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_i_operacion       LIKE cre_ctr_archivo.operacion, -- operacion del proceso
          v_si_id_proceso     LIKE cre_ctr_archivo.id_proceso, -- identificador del proceso
          v_si_tpo_originac   LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_c_programa_cod    LIKE cat_operacion.programa_cod, -- programa de la operación
          v_r_cre_ctr_archivo RECORD
             tot_registros    LIKE cre_ctr_archivo.tot_registros, -- total de registros
             tot_aceptados    LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados
             tot_rechazados   LIKE cre_ctr_archivo.tot_rechazados -- total rechazados
          END RECORD,
          v_r_marcas          RECORD
             tot_marca_acept  INTEGER, -- Total de marcas aceptadas
             tot_marca_rech   INTEGER, -- Total de marcas rechazadas
             tot_desm_acept   INTEGER, -- Total de desmarcas aceptadas
             tot_desm_rech    INTEGER -- Total de desmarcas rechazadas
          END RECORD,
          v_si_lote           INTEGER, -- lote
          v_si_cuenta_valor   SMALLINT, -- variable que consulta si hay precio de acción
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          v_s_mens_correo     STRING, -- contiene el cuerpo del correo
          v_s_titulo_correo   STRING, -- contiene el titulo del correo
          v_s_archivo_correo  STRING, -- ruta y nombre del archivo adjunto en el correo 
          v_v_nom_reporte     VARCHAR(80), -- nombre del reporte
          r_existe_error      SMALLINT, -- indica si ocurrió un error durante algun proceso
          r_cod_error         SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          r_isam_err          INTEGER, 
          r_c_msj             VARCHAR(250)

   -- se recuperan los parametros que envia el programa lanzador
   LET g_v_usuario          = ARG_VAL(1)
   LET g_d_pid              = ARG_VAL(2)
   LET g_i_proceso_cod      = ARG_VAL(3)
   LET g_i_opera_cod        = ARG_VAL(4)
   LET g_d_folio            = ARG_VAL(5)
   LET g_v_arch_proceso     = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_v_usuario CLIPPED|| ".AGRE11.log")

   DISPLAY "=INICIA AGRE11="
   DISPLAY " USUARIO       : ",g_v_usuario
   DISPLAY " PID           : ",g_d_pid
   DISPLAY " ARCHIVO       : ",g_v_arch_proceso

   -- se inicializan variables
   LET v_i_operacion = 23 -- Operación del proceso
   LET v_si_id_proceso = g_id_proceso_agr -- Anualidades Garantizadas
   LET v_si_tpo_originac = 4 -- Anualidades Garantizadas

   -- se genera el folio
   LET g_d_folio = fn_genera_folio(g_i_proceso_cod, g_i_opera_cod, g_v_usuario)
   DISPLAY " FOLIO         : ",g_d_folio USING "#########&"

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING g_c_ruta_bin, g_c_ruta_listados
{
   -- se verifica que exista precio de la acción para el dia de hoy
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE f_valuacion = TODAY\n",
                    "    AND fondo = 11"

   PREPARE prp_slct_precioAcc FROM v_s_qryTxt
   EXECUTE prp_slct_precioAcc INTO v_si_cuenta_valor

   IF v_si_cuenta_valor = 0 THEN
      DISPLAY " ERROR: NO EXISTE EL PRECIO DE ACCIÓN PARA EL DÍA DE HOY,",
              "        POR LO TANTO NO ES POSIBLE GENERAR DEUDOR"

      -- ocurrió un error y se marca como rechazado la operación
      LET r_cod_error = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_cod_error <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_cod_error)
      END IF

      EXIT PROGRAM
   END IF
}
   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
   LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo, lote\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",v_si_id_proceso,"\n",
                    "    AND operacion = ",v_i_operacion,"\n",
                    "    AND estado = 10\n",
                    "  ORDER BY id_cre_ctr_archivo DESC"

   PREPARE prp_id_creCtrArchivo FROM v_s_qryTxt
   EXECUTE prp_id_creCtrArchivo INTO g_id_cre_ctr_archivo, v_si_lote

   -- se verifica si fue posible obtener el identificador del archivo
   IF g_id_cre_ctr_archivo IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      EXIT PROGRAM
   END IF

   DISPLAY " INTEGRA MARCA Y DESMARCA"
   -- se crea la sentencia que ejecuta el procedure que integra el archivo recurrente
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_agr_integra_marca_desm(?,?,?,?,?)"

   PREPARE prp_integra_recurrente FROM v_s_qryTxt
   EXECUTE prp_integra_recurrente USING g_id_cre_ctr_archivo,
                                        g_v_arch_proceso,
                                        g_v_usuario,
                                        g_d_folio,
                                        g_i_proceso_cod
                                   INTO r_cod_error,
                                        r_isam_err,
                                        r_c_msj,
                                        v_r_marcas.tot_marca_acept,
                                        v_r_marcas.tot_marca_rech,
                                        v_r_marcas.tot_desm_acept,
                                        v_r_marcas.tot_desm_rech

   IF r_cod_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE INTEGRACIÓN: ",r_cod_error
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_cod_error = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_cod_error <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_cod_error)
      END IF

      EXIT PROGRAM
   END IF

{--Ya no se insertará en cta crédito en este proceso unica en saldos transferidos (GRT) 22/02/2013
   DISPLAY " INSERTA CTA CRÉDITO"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_inserta_cta_credito(?,?)"

   PREPARE prp_insrt_cta_credito FROM v_s_qryTxt
   EXECUTE prp_insrt_cta_credito USING g_i_proceso_cod, g_id_cre_ctr_archivo
}
   -- se invoca la función que deja la operación en estado Finalizado
   LET r_cod_error = fn_actualiza_opera_fin(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_cod_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_cod_error)

      EXIT PROGRAM
   END IF

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros, tot_aceptados, tot_rechazados\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.*

   DISPLAY " TOTAL REGISTROS :",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " TOTAL ACEPTADOS :",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " TOTAL RECHAZADOS:",v_r_cre_ctr_archivo.tot_rechazados

   -- se invoca la función que genera el reporte (texto plano)
   CALL fn_genera_rep_proc(g_id_cre_ctr_archivo, g_i_proceso_cod, g_i_opera_cod)

  DISPLAY " GENERA REPORTE"
   -- se actualiza el folio en la tabla de control de operaciones
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET folio = ",g_d_folio,"\n",
                    "  WHERE pid = ",g_d_pid,"\n",
                    "    AND proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod

   PREPARE prp_actualiza_folio FROM v_s_qryTxt
   EXECUTE prp_actualiza_folio

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_i_proceso_cod , g_i_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = g_v_usuario CLIPPED, "-", v_c_programa_cod CLIPPED,"-", g_d_pid USING "&&&&&", "-", g_i_proceso_cod USING "&&&&&", "-", g_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegRecurr(v_v_nom_reporte, v_r_marcas.*) RETURNING r_existe_error

   IF NOT r_existe_error THEN
      DISPLAY " ENVIA CORREO DEL REPORTE"
      -- se asigna el titulo del correo
      LET v_s_titulo_correo = "Proceso: RECEPCIÓN MARCA Y DESMARCA"

      -- se asigna el archivo a adjuntar
      LET v_s_archivo_correo = g_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

      -- se asigna el cuerpo del correo
      LET v_s_mens_correo =  "ID Proceso   : ",g_d_pid,"\n",
                             "Proceso      : RECEPCIÓN MARCA Y DESMARCA AGR\n",
                             "Operacion    : INTEGRA ARCHIVO MARCA Y DESMARCA\n",
                             "Fecha Inicio : ",TODAY,"\n",
                             "Fecha Fin    : ",TODAY

      -- se invoca la función que envía por correo el elemento generado
      CALL fn_correo_proceso(g_d_pid,
                             g_i_proceso_cod,
                             g_i_opera_cod,
                             v_s_archivo_correo,
                             v_s_titulo_correo,
                             v_s_mens_correo)
   END IF

   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que genera el reporte de Integración de recurrente
FUNCTION f_genera_rpt_IntegRecurr(p_v_nom_reporte, p_r_marcas)
   DEFINE p_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          p_r_marcas          RECORD
             tot_marca_acept  INTEGER, -- Total de marcas aceptadas
             tot_marca_rech   INTEGER, -- Total de marcas rechazadas
             tot_desm_acept   INTEGER, -- Total de desmarcas aceptadas
             tot_desm_rech    INTEGER -- Total de desmarcas rechazadas
          END RECORD,
          v_r_rpt_res        RECORD -- registro de resumen
             nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini    LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin    LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             id_operacion    LIKE cre_ctr_archivo.operacion, -- operacion
             desc_operacion  LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
             usuario         LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_marca_env   INTEGER, -- Total de marcas aceptadas
             tot_marca_acept INTEGER, -- Total de marcas aceptadas
             tot_marca_rech  INTEGER, -- Total de marcas rechazadas
             tot_desm_env    INTEGER, -- Total de desmarcas aceptadas
             tot_desm_acept  INTEGER, -- Total de desmarcas aceptadas
             tot_desm_rech   INTEGER -- Total de desmarcas rechazadas
          END RECORD,
          v_r_reporte_det   RECORD -- registro de detalle
             tpo_detalle    SMALLINT, -- tipo detalle: 1-Detalle Rechazados, 2-Detalle Sin Originación
             tpo_registro   LIKE cre_rch_acreditado.tpo_registro, -- tipo de registro
             nss            LIKE cre_rch_acreditado.nss, -- nss del derechohabiente
             num_credito    LIKE cre_rch_acreditado.num_credito, -- numero de crédito
             sdo_deudor     LIKE cre_rch_acreditado.sdo_deudor, -- saldo deudor
             f_otorga       LIKE cre_rch_acreditado.f_otorga, -- fecha de otorgamiento
             valor_dscto    LIKE cre_rch_acreditado.valor_dscto, -- valor del descuento
             f_ini_dscto    LIKE cre_rch_acreditado.f_ini_dscto, -- fecha inicial del descuento
             tpo_rch        LIKE cre_rch_acreditado.tpo_rch, -- tipo de rechazo
             estado         LIKE cre_rch_acreditado.estado, -- estado
             des_rch        LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
             num_registros  INTEGER -- numero de registros (solo se usa en las marcas)
          END RECORD,
          v_r_cre_ctr_arch      RECORD LIKE cre_ctr_archivo.*, -- registro de cre ctr archivo
          v_i_cuenta_rch_marca  LIKE cre_ctr_archivo.tot_rechazados, -- número de registros rechazados en el marcaje
          v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.*, -- registro de bat ctr operación
          v_i_folio_format      INTEGER, -- numero de folio con formato
          v_manejador_rpt       OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          v_s_qryTxt            STRING, -- contiene una sentencia sql a ejecutar
          v_si_marca            LIKE sfr_marca.marca, -- marca
          v_v_marca_aux         VARCHAR(5), -- numero de marca
          v_i_tot_marcados      INTEGER, -- total de registro marcados
          v_si_existe_err       SMALLINT -- booleana que indica si existe un error durante la creación del reporte

   -- se inicializan variables
   LET v_i_tot_marcados = 0
   LET v_si_existe_err = FALSE -- se asume que no ocurrirá ningun error

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("AGRE111.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(g_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
      -- se indica que ha ocurrido un error
      LET v_si_existe_err = true

      RETURN v_si_existe_err
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    " FROM cre_ctr_archivo\n",
                    " WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*
{
   -- se crea la sentencia sql que cuenta el numero de registros rechazados en el marcaje
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    " FROM cre_acreditado\n",
                    " WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo,"\n",
                    " AND estado = 150"

   PREPARE prp_cuenta_rch_marca FROM v_s_qryTxt
   EXECUTE prp_cuenta_rch_marca INTO v_i_cuenta_rch_marca

   IF v_i_cuenta_rch_marca > 0 THEN
      -- se incrementa el numero de registros rechazados y se decrementan los aceptados
      LET v_r_cre_ctr_arch.tot_aceptados = v_r_cre_ctr_arch.tot_aceptados - v_i_cuenta_rch_marca
      LET v_r_cre_ctr_arch.tot_rechazados = v_r_cre_ctr_arch.tot_rechazados + v_i_cuenta_rch_marca
   END IF
}
   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod,"\n",
                    "    AND folio = ",g_d_folio

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   -- se asignan los valores del registro del reporte
   LET v_r_rpt_res.nom_archivo     = v_r_bat_ctr_opera.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini    = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin    = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.id_operacion    = v_r_cre_ctr_arch.operacion
   LET v_r_rpt_res.desc_operacion  = fn_obt_desc_operacion(v_r_rpt_res.id_operacion)
   LET v_r_rpt_res.usuario         = v_r_bat_ctr_opera.usuario
   LET v_r_rpt_res.tot_marca_env   = p_r_marcas.tot_marca_acept + p_r_marcas.tot_marca_rech
   LET v_r_rpt_res.tot_marca_acept = p_r_marcas.tot_marca_acept
   LET v_r_rpt_res.tot_marca_rech  = p_r_marcas.tot_marca_rech
   LET v_r_rpt_res.tot_desm_env    = p_r_marcas.tot_desm_acept + p_r_marcas.tot_desm_rech
   LET v_r_rpt_res.tot_desm_acept  = p_r_marcas.tot_desm_acept
   LET v_r_rpt_res.tot_desm_rech   = p_r_marcas.tot_desm_rech

   -- se le da formato al folio
   LET v_i_folio_format = g_d_folio --USING "&&&&&&&&&&"

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integ_recurr TO XML HANDLER v_manejador_rpt
{
   -- Se busca el detalle de las marcas
   LET v_s_qryTxt = " SELECT a.marca, b.descripcion_marca, count(*)\n",
                    "   FROM sfr_marca_activa a, sfr_marca b\n",
                    "  WHERE a.id_derechohabiente IN (\n",
                    "        SELECT id_derechohabiente\n",
                    "        FROM cre_acreditado\n",
                    "        WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo,")\n",
                    "    AND a.marca = b.marca\n",
                    "  GROUP BY a.marca, b.descripcion_marca",
                    "  ORDER BY a.marca"

   PREPARE prp_regs_marcados FROM v_s_qryTxt
   DECLARE cur_regs_marcados CURSOR FOR prp_regs_marcados

   FOREACH cur_regs_marcados INTO v_si_marca, v_r_reporte_det.des_rch, v_r_reporte_det.num_registros
      -- se asigna la marca a una variable de tipo varchar
      LET v_v_marca_aux = v_si_marca

      -- se concatena la marca con la descripción
      LET v_r_reporte_det.des_rch = v_v_marca_aux CLIPPED, " - ", v_r_reporte_det.des_rch

      -- se indica que el detalle es de la marca (4)
      LET v_r_reporte_det.tpo_detalle = 4

      -- salida del reporte
      OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_r_reporte_det.*, v_i_folio_format)

      -- se incrementa el contador de registros marcados
      LET v_i_tot_marcados = v_i_tot_marcados + 1
   END FOREACH

   -- verifica si existen registros rechazados
   IF v_r_rpt_res.tot_rechazados > 0 THEN
      -- existen registros rechazados. Se busca el detalle
      LET v_s_qryTxt = " SELECT 1, tpo_registro, nss, num_credito, sdo_deudor, f_otorga,\n",
                       "        valor_dscto, f_ini_dscto, tpo_rch, estado\n",
                       "   FROM cre_rch_acreditado\n",
                       "  WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

      PREPARE prp_cre_rch_acred FROM v_s_qryTxt
      DECLARE cur_cre_rch_acred CURSOR FOR prp_cre_rch_acred

      FOREACH cur_cre_rch_acred INTO v_r_reporte_det.*
         -- se invoca la función que obtiene la descripción del estado
         LET v_r_reporte_det.des_rch = f_busca_desc_rch(v_r_reporte_det.estado)

         -- salida del reporte
         OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_r_reporte_det.*, v_i_folio_format)
      END FOREACH

      -- si existen registros rechazados en la marca. Se busca el detalle
      LET v_s_qryTxt = " SELECT 1, '00', afi.nss, num_credito, sdo_deudor, f_otorga,\n",
                       "        valor_dscto, f_ini_dscto, estado, estado\n",
                       "   FROM cre_acreditado cre, afi_derechohabiente afi\n",
                       "  WHERE cre.id_cre_ctr_archivo = ",g_id_cre_ctr_archivo,"\n",
                       "    AND cre.estado = 150\n",
                       "    AND cre.id_derechohabiente = afi.id_derechohabiente"

      PREPARE prp_cre_rch_marca FROM v_s_qryTxt
      DECLARE cur_cre_rch_marca CURSOR FOR prp_cre_rch_marca

      FOREACH cur_cre_rch_marca INTO v_r_reporte_det.*
         -- se invoca la función que obtiene la descripción del estado
         LET v_r_reporte_det.des_rch = f_busca_desc_rch_marca(v_r_reporte_det.estado)
         --LET v_r_reporte_det.tpo_detalle = 1
         -- salida del reporte
         OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_r_reporte_det.*, v_i_folio_format)
      END FOREACH
   END IF

   IF v_r_rpt_res.tot_rechazados = 0 AND
      v_r_rpt_res.tot_sin_origen = 0  AND 
      v_i_tot_marcados = 0 THEN
      -- se asigna estatus 3, indica que no hay registros rechazados ni sin originación
      LET v_r_reporte_det.tpo_detalle = 3
      -- salida del reporte
      OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_r_reporte_det.*, v_i_folio_format)
   END IF
}
   -- salida del reporte
   OUTPUT TO REPORT reporte_integ_recurr(v_r_rpt_res.*, v_i_folio_format)

   -- finaliza el reporte
   FINISH REPORT reporte_integ_recurr

   RETURN v_si_existe_err
END FUNCTION

#OBJETIVO: Genera el reporte de Integración de Recurrente
REPORT reporte_integ_recurr(p_r_res, p_i_folio)
   DEFINE p_i_folio         VARCHAR(10), -- numero de folio con formato
          p_r_res           RECORD
             nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini    LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin    LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             id_operacion    LIKE cre_ctr_archivo.operacion, -- operacion
             desc_operacion  LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
             usuario         LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_marca_env   INTEGER, -- Total de marcas aceptadas
             tot_marca_acept INTEGER, -- Total de marcas aceptadas
             tot_marca_rech  INTEGER, -- Total de marcas rechazadas
             tot_desm_env    INTEGER, -- Total de desmarcas aceptadas
             tot_desm_acept  INTEGER, -- Total de desmarcas aceptadas
             tot_desm_rech   INTEGER -- Total de desmarcas rechazadas
          END RECORD,
          v_v_desc_detalle     VARCHAR(50),
          v_v_desc_campo       VARCHAR(50),
          v_sum_sdo_deudor     DECIMAL(22,6),
          v_sum_val_dsto       DECIMAL(22,6),
          v_fecha_reporte      DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_sum_sdo_deudor = 0
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX g_v_usuario
      PRINTX p_i_folio
      PRINTX p_r_res.nom_archivo
      PRINTX p_r_res.fecha_hr_ini
      PRINTX p_r_res.fecha_hr_fin
      PRINTX p_r_res.id_operacion
      PRINTX p_r_res.desc_operacion
      PRINTX p_r_res.usuario
      PRINTX p_r_res.tot_marca_env USING "#########&"
      PRINTX p_r_res.tot_marca_acept USING "#########&"
      PRINTX p_r_res.tot_marca_rech USING "#########&"
      PRINTX p_r_res.tot_desm_env USING "#########&"
      PRINTX p_r_res.tot_desm_acept USING "#########&"
      PRINTX p_r_res.tot_desm_rech USING "#########&"
END REPORT
{
#Objetivo: Busca la descripción del estatus de rechazo en catalogo
FUNCTION f_busca_desc_rch(p_si_estado)
   DEFINE p_si_estado     LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
          v_c_desc_estado LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
          v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_estado\n",
                    "   FROM cat_rch_acreditado\n",
                    "  WHERE estado = ",p_si_estado

   PREPARE prp_desc_estado FROM v_s_qryTxt
   EXECUTE prp_desc_estado INTO v_c_desc_estado

   -- se verifica si se encontró descripción
   IF v_c_desc_estado IS NULL THEN
      LET v_c_desc_estado = "Descripción no encontrada"
   END IF

   RETURN v_c_desc_estado
END FUNCTION

#Objetivo: Busca la descripción del estatus de rechazo en catalogo
FUNCTION f_busca_desc_rch_marca(p_si_estado)
   DEFINE p_si_estado     LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
          v_c_desc_estado LIKE cat_rch_acreditado.desc_estado, -- descripción del estado
          v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_rechazo\n",
                    "   FROM cat_rechazo\n",
                    "  WHERE cod_rechazo = '",p_si_estado,"'\n",
                    "    AND tpo_rechazo = 'SIS' "

   PREPARE prp_desc_rechazo FROM v_s_qryTxt
   EXECUTE prp_desc_rechazo INTO v_c_desc_estado

   -- se verifica si se encontró descripción
   IF v_c_desc_estado IS NULL THEN
      LET v_c_desc_estado = "Descripción no encontrada"
   END IF

   RETURN v_c_desc_estado
END FUNCTION
}

#Objetivo: Busca la descripción de la operación
FUNCTION fn_obt_desc_operacion(p_c_operacion)
   DEFINE p_c_operacion  LIKE cat_operacion_prc.operacion, -- operación
          v_c_desc_opera LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
          v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_operacion\n",
                    "   FROM cat_operacion_prc\n",
                    "  WHERE operacion = '",p_c_operacion,"'"

   PREPARE prp_desc_operacion FROM v_s_qryTxt
   EXECUTE prp_desc_operacion INTO v_c_desc_opera

   -- se verifica si se encontró descripción
   IF v_c_desc_opera IS NULL THEN
      LET v_c_desc_opera = "DESCRIPCIÓN NO ENCONTRADA"
   END IF

   RETURN v_c_desc_opera
END FUNCTION
{
#Objetivo: Busca la descripción del tipo de originación
FUNCTION fn_obt_desc_originacion(p_si_tpo_originac)
   DEFINE p_si_tpo_originac LIKE cat_cre_originacion.tpo_originacion, -- tipo de originación
          p_c_originac_desc LIKE cat_cre_originacion.originacion_desc, -- descripción de originación
          v_s_qryTxt        STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT originacion_desc\n",
                    "   FROM cat_cre_originacion\n",
                    "  WHERE tpo_originacion = ",p_si_tpo_originac

   PREPARE prp_desc_originacion FROM v_s_qryTxt
   EXECUTE prp_desc_originacion INTO p_c_originac_desc

   -- se verifica si se encontró descripción
   IF p_c_originac_desc IS NULL THEN
      LET p_c_originac_desc = "DESCRIPCIÓN NO ENCONTRADA"
   END IF

   RETURN p_c_originac_desc
END FUNCTION
}

#Objetivo: Función que genera el reporte de los registros integrados
FUNCTION fn_genera_rep_proc(p_id_cre_ctr_archivo, p_i_proceso_cod, p_i_opera_cod)
   DEFINE p_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador de archivo
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion
          v_v_arch_salida      VARCHAR(100), -- nombre del archivo de salida
          v_v_ruta_archivo     VARCHAR(150), -- ruta y nombre del archivo de salida
          v_c_fec_hoy          CHAR(8), -- fecha con formato "yyyymmdd"
          v_c_extension        LIKE cat_operacion.extension, -- extensión del archivo
          v_ch_arch_reporte    BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_r_archivo_rep      RECORD
             nss               CHAR(11),
             tpo_credito       SMALLINT, -- tipo de crédito
             monto_acciones    DECIMAL(18,6),
             monto_pesos       DECIMAL(14,2)
          END RECORD,
          v_r_detalle          RECORD
             nss               CHAR(11),
             monto_acciones    CHAR(15),
             monto_pesos       CHAR(15)
          END RECORD,
          v_si_tpo_originacion SMALLINT, -- tipo de originación
          v_d_id_derechohab    DECIMAL(9,0), -- identificador del derechohabiente
          v_d_folio_liquida    DECIMAL(9,0), -- folio de liquidación
          v_s_registro         STRING, -- registro a insertar
          v_c_ruta_envio       LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_i_contrador_reg    INTEGER, -- contrador de registros
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          v_c_ruta_listado     LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo

   -- se inicializan variables
   LET v_c_fec_hoy = TODAY USING "yyyymmdd"
   LET v_i_contrador_reg = 0 -- contador de registros

   -- se obtiene la extensión del archivo
   LET v_c_extension = fn_recupera_extension(p_i_proceso_cod, p_i_opera_cod)

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_arch_salida = "repSdosLiq_" || v_c_fec_hoy || "_AGR." || v_c_extension CLIPPED
   DISPLAY " REPORTE SALIDA : ", v_v_arch_salida

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_envio1 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio1 INTO v_c_ruta_envio, v_c_ruta_listado

   -- se crea el nombre del archivo y se concatena con la ruta donde se alojará
   LET v_v_ruta_archivo = v_c_ruta_envio CLIPPED || "/" || v_v_arch_salida CLIPPED

   -- se crea el manejador de archivo
   LET v_ch_arch_reporte = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_reporte.openFile(v_v_ruta_archivo, "w" )
   CALL v_ch_arch_reporte.setDelimiter("")

   -- se consultan los datos que componen el cuerpo del archivo de salida
   LET v_s_qryTxt = " SELECT nss, tpo_credito\n",
                    "   FROM safre_tmp:tmp_marca_desmarca_det\n",
                    "  WHERE edo_solicitud = '01'\n",
                    "    AND sit_credito = 'M'"

   PREPARE prp_tmp_solic_sdo FROM v_s_qryTxt
   DECLARE cur_tmp_solic_sdo CURSOR FOR prp_tmp_solic_sdo

   FOREACH cur_tmp_solic_sdo INTO v_r_archivo_rep.nss, v_r_archivo_rep.tpo_credito
      -- se inicializan variables
      LET v_d_id_derechohab = NULL
      LET v_d_folio_liquida = NULL

      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se obtiene el folio de liquidación y el identificador del derechohabiente
      LET v_s_qryTxt = " SELECT FIRST 1 cre.id_derechohabiente, cre.folio_liquida\n",
                       "   FROM cre_acreditado cre, afi_derechohabiente afi, cat_tipo_credito tpo\n",
                       "  WHERE afi.nss = '",v_r_archivo_rep.nss,"'\n",
                       "    AND afi.id_derechohabiente = cre.id_derechohabiente\n",
                       "    AND tpo.tpo_credito = ",v_r_archivo_rep.tpo_credito,"\n",
                       "    AND cre.edo_procesar IN (55, 60)\n",
                       "    AND tpo.tpo_originacion = cre.tpo_originacion\n",
                       "    AND cre.tpo_credito = tpo.tpo_credito\n",
                       "    AND tpo.f_actualiza <= cre.f_otorga\n",
                       "    AND cre.edo_credito = 1\n",
                       "  ORDER BY cre.f_otorga DESC, cre.estado"

      PREPARE prp_slct_idDer_folLiq FROM v_s_qryTxt
      EXECUTE prp_slct_idDer_folLiq INTO v_d_id_derechohab, v_d_folio_liquida

      -- en caso de no haber encontrado su registro correspondiente en acreditados o este
      -- no ha sido liquidado continua con el siguiente registro
      IF v_d_id_derechohab IS NULL OR v_d_id_derechohab = 0 OR
         v_d_folio_liquida IS NULL OR v_d_folio_liquida = 0 THEN
         CONTINUE FOREACH
      END IF

      -- se obtienen los importes liquidados
      CALL fn_obtn_imp_lqdd(v_d_id_derechohab, v_d_folio_liquida) RETURNING v_r_archivo_rep.monto_acciones, v_r_archivo_rep.monto_pesos

      -- en caso de no haber encontrado importe, no se inserta el registro
      IF v_r_archivo_rep.monto_acciones IS NULL OR v_r_archivo_rep.monto_acciones = 0 OR
         v_r_archivo_rep.monto_pesos IS NULL OR v_r_archivo_rep.monto_pesos = 0 THEN  
         CONTINUE FOREACH
      END IF

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.nss            = v_r_archivo_rep.nss
      LET v_r_detalle.monto_acciones = (v_r_archivo_rep.monto_acciones * 1000000) USING "&&&&&&&&&&&&&&&"
      LET v_r_detalle.monto_pesos    = (v_r_archivo_rep.monto_pesos * 100) USING "&&&&&&&&&&&&&&&"

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.nss, v_r_detalle.monto_acciones, v_r_detalle.monto_pesos

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_reporte.write([v_s_registro])
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_ch_arch_reporte.close()
END FUNCTION

#Objetivo: Función que genera selecciona y regresa el importe liquidado (pesos y acciones) para un
#          nss que entra como parámentro
FUNCTION fn_obtn_imp_lqdd(p_d_id_derechohab, p_d_folio_liquida)

   DEFINE p_d_id_derechohab DECIMAL(9,0) -- identificador del derechohabiente
   DEFINE p_d_folio_liquida DECIMAL(9,0) -- folio de liquidación
   DEFINE v_monto_acciones  DECIMAL(18,6) -- monto total en acciones
   DEFINE v_monto_pesos     DECIMAL(14,2) -- monto total en pesos
   DEFINE v_s_qryTxt        STRING -- guarda una sentencia SQL a ejecutar

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_obtiene_mov_liq(?,?)"

   PREPARE prp_obt_mov FROM v_s_qryTxt
   EXECUTE prp_obt_mov USING p_d_id_derechohab,
                             p_d_folio_liquida
                        INTO v_monto_acciones, v_monto_pesos

   RETURN v_monto_acciones, v_monto_pesos

END FUNCTION
