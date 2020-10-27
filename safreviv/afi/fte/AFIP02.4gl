--=============================================================================
-- Version: 1.0.0
-- Fecha última modificación:
--=============================================================================
#########################################################################################
#Módulo       => AFI                                                                    #
#Programa     => AFIP02                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integración    #
#                de movimientos afiliatorios SOLO INFONAVIT                             #
#Fecha inicio => 22 de junio de 2012                                                    #
# 21 Sep 2012. Cambia el orden de ejecución de los movimientos por incidencia INFQA-62  #
# 01 Alta
# 06 Cambio de Nombre
# 08 Reingreso
# 02 Baja 
#
# 06junio2013. El reporte ya no emite detalle de rechazos, solo cifras globales
#########################################################################################

DATABASE safre_viv

GLOBALS "AFIG01.4gl"

GLOBALS

   DEFINE g_pid         LIKE bat_ctr_proceso.pid      --  ID del proceso
   DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod  -- codigo del proceso
   DEFINE g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion

END GLOBALS

MAIN

   DEFINE p_pid                         LIKE bat_ctr_operacion.pid -- PID del proceso
   DEFINE p_proceso_cod                 LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
   DEFINE p_opera_cod                   LIKE bat_ctr_operacion.opera_cod -- codigo de la operacion
   DEFINE p_usuario_cod                 LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE p_folio                       LIKE deo_preliquida.folio_liquida
   DEFINE p_nombre_archivo              LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_s_sql                       STRING -- cadena con una instruccion SQL
   DEFINE v_i_resultado                 INTEGER -- resultado del proceso
   DEFINE v_i_resultado_alta            SMALLINT -- resultado de la integracion de movimientos de alta 
   DEFINE v_i_resultado_baja            SMALLINT -- resultado de la integracion de movimientos de baja
   DEFINE v_i_resultado_cambio_nombre   SMALLINT -- resultado de la integracion de movimientos de cambio de nombre
   DEFINE v_i_resultado_reingreso       SMALLINT -- resultado de la integracion de movimientos de reingreso
   DEFINE r_bnd_fin_oper                SMALLINT
   DEFINE v_si_correcto_integra         SMALLINT
   DEFINE p_titulo                      STRING -- titulo del mensaje enviado en el correo
   DEFINE p_mensaje                     STRING -- cuerpo del mensaje enviado
   DEFINE v_error_isam                  INTEGER
   DEFINE v_mensaje                     VARCHAR(250)
   DEFINE v_mensaje_alta                VARCHAR(250) -- mensaje devuelto por la integracion de alta
   DEFINE v_mensaje_baja                VARCHAR(250) -- mensaje devuelto por la integracion de baja
   DEFINE v_mensaje_cambio_nombre       VARCHAR(250) -- mensaje devuelto por la integracion de cambio de nombre
   DEFINE v_mensaje_reingreso           VARCHAR(250) -- mensaje devuelto por la integracion de reingreso
   DEFINE v_regs_aceptados_alta         INTEGER -- número de movimientos aceptados
   DEFINE v_regs_rechazados_alta        INTEGER -- número de movimientos rechazados
   DEFINE v_bajas_realizadas            INTEGER -- número de bajas realizadas
   DEFINE v_cambios_nombre_realizados   INTEGER -- número de cambios de nombre realizados
   DEFINE v_reingresos                  INTEGER -- número de reingresos realizados
   DEFINE v_altas_riss                  INTEGER -- número de movimientos altas riss
   DEFINE v_bajas_riss                  INTEGER -- número de movimientos bajas riss
   DEFINE v_rfc_riss                    INTEGER -- número de movimientos rfc riss
   DEFINE v_nss_error                   LIKE afi_derechohabiente.nss

   -- variables para el reporte de integraciÓn
   DEFINE p_programa_cod                VARCHAR(10)
   DEFINE p_modulo_cod                  VARCHAR(3)
   DEFINE v_ruta_reporte                STRING -- ruta del archivo del reporte
   DEFINE v_ruta_listados               VARCHAR(40) -- ruta de los listados
   DEFINE v_ruta_ejecutable             STRING -- ruta del ejecutable
   DEFINE report_handler                om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE v_nombre_usuario              CHAR (40)
   DEFINE v_usuario_cod_temp            CHAR (20)

   -- cifras de control
   DEFINE v_regs_procesados             INTEGER
   DEFINE v_regs_aceptados              INTEGER
   DEFINE v_regs_rechazados             INTEGER

   -- registros rechazados
   DEFINE v_afi_rch_afiliatorio RECORD LIKE afi_rch_afiliatorio.*

   DEFINE v_regs_procesados_alta        INTEGER  --total procesados ALTA
   DEFINE v_bajas_aceptadas             INTEGER  --total aceptadas  BAJA
   DEFINE v_bajas_rechazadas            INTEGER  --total rechazadas BAJA 
   DEFINE v_cambio_nom_aceptados        INTEGER  --total aceptados  CAMBIO NOMBRE
   DEFINE v_cambio_nom_rechazados       INTEGER  --total rechazados CAMBIO NOMBRE
   DEFINE v_reingresos_aceptados        INTEGER  --total aceptados  REINGRESOS
   DEFINE v_reingresos_rechazados       INTEGER  --total rechazados REINGRESOS
   DEFINE v_num_altas_en_reingreso      INTEGER  --número de altas realizadas en tratamiento de reingresos
   DEFINE v_alta_riss_acep              INTEGER  --total aceptadas  ALTA RISS
   DEFINE v_alta_riss_rech              INTEGER  --total rechazadas ALTA RISS
   DEFINE v_baja_riss_acep              INTEGER  --total aceptadas  BAJA RISS
   DEFINE v_baja_riss_rech              INTEGER  --total rechazadas BAJA RISS
   DEFINE v_rfc_riss_acep               INTEGER  --total aceptadas  ALTA RISS RFC
   DEFINE v_rfc_riss_rech               INTEGER  --total rechazadas ALTA RISS RFC
   DEFINE v_rech_alta_riss              INTEGER  --total rechazos alta riss
   DEFINE v_rech_baja_riss              INTEGER  --total rechazos baja riss
   DEFINE v_rech_rfc_riss               INTEGER  --total rechazos rfc riss
   DEFINE v_comando                     STRING
   DEFINE v_ind_rech                    INTEGER

   DEFINE v_rch_tipo_mov RECORD
      v_tpo_movimiento CHAR(2),
      v_folio_lote     DECIMAL(9,0),
      v_total_rechazos INTEGER
   END RECORD

   ##Ejecuta prevalidación de saldos
   -- se recuperan los parÁmetros la clave de usuario desde parÁmetro
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se asigna proceso y operación
   LET g_pid         = p_pid
   LET g_proceso_cod = p_proceso_cod -- movimientos afiliatorios
   LET g_opera_cod   = p_opera_cod   -- integracion

   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio

   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")

   -- =============================================================================
   -- PREVALIDACIÓN DE CONTENIDO DE ARCHIVO
   -- se verifican registros contra sumario
   -- =============================================================================
   DISPLAY "Ejecutando prevalidación de la integración de movimientos afiliatorios"
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_afi_prevalida_integracion_sinf(?,?,?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_afi_prevalida FROM v_s_sql

   -- se ejecuta el stored procedure
   EXECUTE sid_afi_prevalida USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje

   -- se muestra el mensaje de la prevalidacion
   DISPLAY v_mensaje

   -- si el resultado fue correcto
   IF ( v_i_resultado <> 0 ) THEN
      DISPLAY "Error - ", v_i_resultado
      DISPLAY "Mensaje: ", v_mensaje
      DISPLAY "No se puede continuar con la integración"

      -- se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING v_i_resultado

      -- se prepara el mensaje de correo
      LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                      "Proceso      : MOVIMIENTOS AFILIATORIOS SOLO INFONAVIT\n",
                      "Operación    : INTEGRACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n",
                      "El folio asociado a su operación es: ", p_folio, "\n\n",
                      v_mensaje

      LET p_titulo = "Finalización de operación - MOVIMIENTOS AFILIATORIOS - INTEGRACIÓN"

      -- se invoca el envio de correo electrónico de notificación
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE
      -- =============================================================================
      -- INTEGRACIÓN DE MOVIMIENTOS DE ALTA/REINGRESO/BAJA/RISS
      -- =============================================================================
      DISPLAY ""
      DISPLAY "Ejecutando integración de movimientos de Alta/Reingreso/Baja/RISS Sólo Infonavit"
      --creación tabal temporal de trabajo
      CALL fn_crea_tabla_integracion()

      -- se contruye el enuncionado SQL
      LET v_s_sql = "EXECUTE FUNCTION fn_afi_integra_alta_reingreso_baja_sinf(?,?,?,?,?)"

      -- se prepara la ejecucion del stored procedure para la integracion
      PREPARE sid_afi_alta FROM v_s_sql

      -- se ejecuta el stored procedure
      EXECUTE sid_afi_alta USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
         INTO v_i_resultado_alta, v_error_isam, v_mensaje_alta,
              v_regs_aceptados_alta, v_regs_rechazados_alta,
              v_reingresos_aceptados, v_reingresos_rechazados,
              v_num_altas_en_reingreso,
              v_bajas_aceptadas, v_bajas_rechazadas,
              v_alta_riss_acep, v_alta_riss_rech,
              v_baja_riss_acep, v_baja_riss_rech,
              v_rfc_riss_acep, v_rfc_riss_rech, 
              v_nss_error

      -- =============================================================================
      -- UNIFICACIÓN / CAMBIO DE NOMBRE
      -- =============================================================================
      DISPLAY "Ejecutando integración de movimientos de cambio de nombre Sólo Infonavit"
      -- se contruye el enuncionado SQL
      LET v_s_sql = "EXECUTE FUNCTION fn_afi_integra_cambio_nombre_sinf(?,?,?,?,?)"

      -- se prepara la ejecución del stored procedure para la integración
      PREPARE sid_afi_cambio_nombre FROM v_s_sql

      -- se ejecuta el stored procedure
      EXECUTE sid_afi_cambio_nombre USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
         INTO v_i_resultado, v_error_isam, v_mensaje_cambio_nombre, v_cambio_nom_aceptados --v_cambios_nombre_realizados

      -- =============================================================================
      -- UNIFICACIÓN / CAMBIO DE NSS
      -- Solo infonavit no hace cambio de nss. Esos movimientos deben llegar por
      -- el modulo de unificacion
      -- =============================================================================
      --DISPLAY " Ejecutando integración de movimientos de cambio de NSS Sólo Infonavit"
      -- se contruye el enuncionado SQL
      --LET v_s_sql = "EXECUTE FUNCTION fn_afi_integra_cambio_nss_sinf(?,?,?,?,?)"

      -- se prepara la ejecución del stored procedure para la integración
      --PREPARE sid_afi_cambio_nss FROM v_s_sql

      -- se ejecuta el stored procedure
      --EXECUTE sid_afi_cambio_nss USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      --   INTO v_i_resultado_cambio_nss, v_error_isam, v_mensaje_cambio_nss, v_nss_unificadores, v_nss_unificados

      -- =============================================================================
      -- CAMBIO DE SALARIO
      --
      -- 27 JUNIO 2012
      -- EL CAMBIO DE SALARIO NO SE LLEVARÁ A CABO EN EL PROCESO DE INTEGRACIÓN PUES
      -- SE REALIZA EN OTRO PROCESO
      --
      -- SE DEJA COMENTADO EL CÓDIGO POR SI SE CAMBIA ESTA DISPOSICIÓN
      -- =============================================================================
      {
      -- se contruye el enuncionado SQL
      LET v_s_sql = "EXECUTE FUNCTION fn_afi_integra_modif_salario_sinf(?,?,?,?,?)"

      -- se prepara la ejecucion del stored procedure para la integracion
      PREPARE sid_afi_modif_salario FROM v_s_sql

      -- se ejecuta el stored procedure
      EXECUTE sid_afi_modif_salario USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
         INTO v_i_resultado, v_error_isam, v_mensaje

      DISPLAY v_mensaje
      }

      -- se escribe el mensaje para el correo electronico
      LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                      "Proceso      : MOVIMIENTOS AFILIATORIOS SOLO INFONAVIT\n",
                      "Operación    : INTEGRACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n",
                      "El folio asociado a su operación es: ", p_folio, "\n"

      DISPLAY "=========================================================================="
      DISPLAY "                    RESULTADOS DE LA INTEGRACIÓN"
      DISPLAY "=========================================================================="

      -- si todos los subprocesos finalizaron correctamente
      IF ( (v_i_resultado_alta          = 0) AND
           (v_i_resultado_cambio_nombre = 0) ) THEN
               -- se complementa el mensaje
         LET p_mensaje = p_mensaje || "Integración de movimientos afiliatorios realizada con éxito.\n"
         DISPLAY "Integración de movimientos afiliatorios realizada con éxito.\n"

         -- se muestra el resumen de los resultados

         DISPLAY "ALTA          : ",v_mensaje_alta
         DISPLAY "  Altas aceptadas       : ", v_regs_aceptados_alta
         DISPLAY "  Altas rechazadas      : ", v_regs_rechazados_alta
         DISPLAY "  Reingresos aceptados  : ", v_reingresos_aceptados
         DISPLAY "  Reingresos rechazados : ", v_reingresos_rechazados
         DISPLAY "  Altas RISS aceptadas  : ", v_alta_riss_acep
         DISPLAY "  Altas RISS rechazadas : ", v_alta_riss_rech
         DISPLAY "  Bajas RISS aceptadas  : ", v_baja_riss_acep
         DISPLAY "  Bajas RISS rechazadas : ", v_baja_riss_rech
         DISPLAY "  Altas RFC RISS acep   : ", v_rfc_riss_acep
         DISPLAY "  Altas RFC RISS rech   : ", v_rfc_riss_rech
         DISPLAY "CAMBIO          "
         DISPLAY "      ",v_mensaje_cambio_nombre
         --DISPLAY "CAMBIO NSS    : ",v_mensaje_cambio_nss

         -- Generación del archivo de rechazos
         SELECT ruta_bin
           INTO v_ruta_listados
           FROM seg_modulo
          WHERE modulo_cod = "afi"

         LET v_comando = "fglrun ", v_ruta_listados CLIPPED, "/AFIS01.42r ", p_usuario_cod, " ", p_folio, " ", g_proceso_cod

         -- se ejecuta el programa que genera el archivo con los rechazos
         RUN v_comando

         -- =====================================================================================
         -- =====================================================================================
         --            REPORTE DE CIFRAS DE CONTROL DE LA INTEGRACIÓN
         -- =====================================================================================
         -- se obtiene el modulo del proceso
         SELECT ruta_listados
           INTO v_ruta_listados
           FROM seg_modulo
          WHERE modulo_cod = "afi"

         LET v_ruta_reporte = v_ruta_listados CLIPPED, "/",
                        p_usuario_cod   CLIPPED, "-", -- usuario
                        "AFIP02-"                   , -- programa
                        g_pid           USING "&&&&&","-", -- PID
                        g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                        g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

         DISPLAY "Ruta del reporte: ", v_ruta_reporte

         -- se indica que el reporte usara la plantilla creada
         IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIP02.4rp") ) THEN  -- if  the file loaded OK
            -- sin preview
            CALL fgl_report_selectPreview(0)

            -- se indica que se escriba en archivo
            CALL fgl_report_setOutputFileName(v_ruta_reporte)

            LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings

            -- se inicia el reporte
            START REPORT rpt_cifras_control TO XML HANDLER report_handler

            INITIALIZE v_afi_rch_afiliatorio.* TO NULL

            LET v_afi_rch_afiliatorio.cod_rechazo = -99

            -- se obtienen los totales de registros procesados
            SELECT num_altas         ,
                   num_bajas         ,
                   num_cambio_nombre ,
                   num_reingreso     ,
                   num_alta_riss     ,
                   num_baja_riss     ,
                   num_rfc_riss
              INTO v_regs_procesados_alta     ,
                   v_bajas_realizadas         ,
                   v_cambios_nombre_realizados,
                   v_reingresos               ,
                   v_altas_riss               ,
                   v_bajas_riss               ,
                   v_rfc_riss
              FROM safre_tmp:tmp_afi_sinf_sumario;

            -- se obtienen los totales de rechazados por tipo de movimiento
            DECLARE cur_total_rch CURSOR FOR
            SELECT tpo_movimiento, folio_lote, count(*)
              FROM afi_rch_afiliatorio
             WHERE folio_lote = p_folio
            GROUP BY 1,2

            LET v_ind_rech = 0

            FOREACH cur_total_rch  INTO v_rch_tipo_mov.*
               IF v_rch_tipo_mov.v_total_rechazos IS NULL THEN
                  LET v_rch_tipo_mov.v_total_rechazos = 0 
               END IF  

               CASE 
                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "01"
                     LET v_regs_rechazados_alta = v_rch_tipo_mov.v_total_rechazos
                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "06"
                     LET v_cambio_nom_rechazados = v_rch_tipo_mov.v_total_rechazos
                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "08"
                     LET v_reingresos_rechazados = v_rch_tipo_mov.v_total_rechazos
                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "22"
                     LET v_rech_alta_riss        = v_rch_tipo_mov.v_total_rechazos
                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "23"
                     LET v_rech_baja_riss        = v_rch_tipo_mov.v_total_rechazos
                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "24"
                     LET v_rech_rfc_riss         = v_rch_tipo_mov.v_total_rechazos
               END CASE

               LET v_ind_rech = v_ind_rech + 1
            END FOREACH

            -- se envia el encabezado
            OUTPUT TO REPORT rpt_cifras_control(p_folio                    ,
                                                p_usuario_cod              ,
                                                v_regs_procesados_alta     ,
                                                v_regs_aceptados_alta      ,
                                                v_regs_rechazados_alta     ,
                                                v_bajas_realizadas         ,
                                                v_cambios_nombre_realizados,
                                                0                          , --cambio nss
                                                0                          , --cambio nss
                                                v_reingresos               ,
                                                v_num_altas_en_reingreso   ,
                                                v_bajas_aceptadas          ,
                                                v_bajas_rechazadas         ,
                                                v_cambio_nom_aceptados     ,
                                                v_cambio_nom_rechazados    ,
                                                v_reingresos_aceptados     ,
                                                v_reingresos_rechazados    ,
                                                v_alta_riss_acep           ,
                                                v_alta_riss_rech           ,
                                                v_baja_riss_acep           ,
                                                v_baja_riss_rech           ,
                                                v_rfc_riss_acep            ,
                                                v_rfc_riss_rech            ,
                                                v_altas_riss               ,
                                                v_bajas_riss               ,
                                                v_rfc_riss                 ,
                                                v_afi_rch_afiliatorio.*    ,
                                                p_nombre_archivo)

            -- ya no se requiere detalle en los reportes.
            -- 06junio2013.

            -- se finaliza
            FINISH REPORT rpt_cifras_control
            -- =====================================================================================
         ELSE
            DISPLAY "No se puede leer la plantilla del reporte AFIP02.4rp"
         END IF

         -- se finaliza la operacion
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                     RETURNING r_bnd_fin_oper

         -- si no se pudo finalizar la operacion
         IF ( r_bnd_fin_oper <> 0 ) THEN
            DISPLAY "Ocurrió un error al finalizar la operación:"
            -- En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_bnd_fin_oper)
         END IF
      ELSE
         -- se complementa el mensaje
         LET p_mensaje = p_mensaje || "Ocurrió un error al realizar la integración de movimientos afiliatorios\n"
         DISPLAY "Ocurrió un error al realizar la integración de movimientos afiliatorios\n"

         -- se verifica el resultado de la integracion de los movimientos de alta
         IF ( v_i_resultado_alta = 0 ) THEN
            DISPLAY "Alta: Correcto"
         ELSE
            DISPLAY "Alta: ERROR - ", v_i_resultado_alta
         END IF

         DISPLAY v_mensaje_alta

         {
         -- se verifica el resultado de la integracion de los movimientos de baja
         IF ( v_i_resultado_baja = 0 ) THEN
            DISPLAY "Baja: Correcto."
         ELSE
            DISPLAY "Baja: ERROR - ", v_i_resultado_baja
         END IF

         DISPLAY v_mensaje_baja
         }

         -- se verifica el resultado de la integracion de los movimientos de cambio de nombre
         IF ( v_i_resultado_cambio_nombre = 0 ) THEN
            DISPLAY "Cambio de nombre: Correcto."
         ELSE
            DISPLAY "Cambio de nombre: ERROR - ", v_i_resultado_cambio_nombre
         END IF

         DISPLAY v_mensaje_cambio_nombre
 
         -- se verifica el resultado de la integracion de los movimientos de cambio de NSS
         --IF ( v_i_resultado_cambio_nss    = 0 ) THEN
         --   DISPLAY "Cambio de NSS: Correcto."
         --ELSE
         --   DISPLAY "Cambio de NSS: ERROR"
         --END IF
         --DISPLAY "Mensaje: ", v_mensaje_cambio_nss

         {
         IF ( v_i_resultado_reingreso     = 0 ) THEN
            DISPLAY "Reingreso: Correcto."
         ELSE
            DISPLAY "Reingreso: ERROR - ", v_i_resultado_reingreso
         END IF
         DISPLAY "Mensaje: ", v_mensaje_reingreso
         }

         -- se complementa el mensaje
         LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación\n"
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING v_i_resultado
      END IF

      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "INTEGRACION")

      LET p_titulo = "Finalización de operación - MOVIMIENTOS AFILIATORIOS SOLO INFONAVIT - INTEGRACION"

      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF

END MAIN

FUNCTION fn_crea_tabla_integracion()
#fcti

   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   -- en caso de error continua
   WHENEVER ERROR CONTINUE

   -- se elimina la tabla temporal
   DROP TABLE tmp_afi_alta_reingreso_baja_sinf;

   -- al encontrar un error detiene el programa
   WHENEVER ERROR STOP

   -- se ejecuta el script para crear la tabla temporal
   CREATE TABLE tmp_afi_alta_reingreso_baja_sinf (
      tpo_movimiento      char(2)     ,
      espacios            char(2)     ,
      nrp                 char(11)    ,
      f_movimiento        char(8)     ,
      curp_rfc            char(18)    ,
      t_trabajador        decimal(1,0),
      nss                 char(11)    ,
      nombre              char(50)    ,
      presentacion_extemp decimal(1,0),
      jornada_semana      decimal(1,0),
      sdi                 decimal(6,0),
      sexo                decimal(1,0),
      nss_correcto        char(11)    ,
      nombre_correcto     char(50)
     ) IN tmp_2_dbs;

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION

{ ==========================================================================
Clave:  rpt_cifras_control
Nombre: rpt_cifras_control
Fecha creacion: 14 Agosto 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de cifras de control de carga de archivo

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}

REPORT rpt_cifras_control(p_folio                    ,
                          p_usuario_cod              ,
                          p_regs_procesados_alta     ,
                          p_regs_aceptados_alta      ,
                          p_regs_rechazados_alta     ,
                          p_bajas_realizadas         ,
                          p_cambios_nombre_realizados,
                          p_nss_unificadores         ,
                          p_nss_unificados           ,
                          p_reingresos               ,
                          p_num_altas_en_reingreso   ,
                          p_bajas_aceptadas          ,
                          p_bajas_rechazadas         ,
                          p_cambio_nom_aceptados     ,
                          p_cambio_nom_rechazados    ,
                          p_reingresos_aceptados     ,
                          p_reingresos_rechazados    ,
                          p_altas_riss_aceptadas     ,
                          p_altas_riss_rechazadas    ,
                          p_bajas_riss_aceptadas     ,
                          p_bajas_riss_rechazadas    ,
                          p_rfc_aceptadas            ,
                          p_rfc_rechazadas           ,
                          p_altas_riss               ,
                          p_bajas_riss               ,
                          p_rfc_riss                 ,
                          p_afi_rch_afiliatorio      ,
                          p_nombre_archivo)

   DEFINE p_usuario_cod                LIKE seg_usuario.usuario_cod -- Clave de usuario
   DEFINE p_folio                      DECIMAL(9,0)
   -- cifras de control
   DEFINE v_fecha_texto                VARCHAR(10)
   DEFINE p_regs_aceptados_alta        INTEGER -- numero de movimientos aceptados
   DEFINE p_regs_rechazados_alta       INTEGER -- numero de movimientos rechazados
   DEFINE p_bajas_realizadas           INTEGER -- numero de bajas realizadas
   DEFINE p_cambios_nombre_realizados  INTEGER -- numero de cambios de nombre realizados
   DEFINE p_nss_unificadores           INTEGER -- numero de NSS unificadores
   DEFINE p_nss_unificados             INTEGER -- numero de NSS unificados
   DEFINE p_reingresos                 INTEGER -- numero de reingresos realizados
   DEFINE p_afi_rch_afiliatorio        RECORD LIKE afi_rch_afiliatorio.* -- registro de afiliacion rechazado
   DEFINE v_desc_rechazo               VARCHAR(250)
   DEFINE p_regs_procesados_alta       INTEGER  --total procesados ALTA
   DEFINE p_bajas_aceptadas            INTEGER  --total aceptadas  BAJA
   DEFINE p_bajas_rechazadas           INTEGER  --total rechazadas BAJA 
   DEFINE p_cambio_nom_aceptados       INTEGER  --total aceptados  CAMBIO NOMBRE
   DEFINE p_cambio_nom_rechazados      INTEGER  --total rechazados CAMBIO NOMBRE
   DEFINE p_reingresos_aceptados       INTEGER  --total aceptados  REINGRESOS
   DEFINE p_reingresos_rechazados      INTEGER  --total rechazados REINGRESOS
   DEFINE p_altas_riss_aceptadas       INTEGER  --total aceptadas  ALTA RISS
   DEFINE p_altas_riss_rechazadas      INTEGER  --total rechazadas ALTA RISS
   DEFINE p_bajas_riss_aceptadas       INTEGER  --total aceptadas  BAJA RISS
   DEFINE p_bajas_riss_rechazadas      INTEGER  --total rechazadas BAJA RISS
   DEFINE p_rfc_aceptadas              INTEGER  --total aceptadas  RISS RFC
   DEFINE p_rfc_rechazadas             INTEGER  --total rechazadas RISS RFC
   DEFINE p_altas_riss                 INTEGER  --total procesados ALTA RISS
   DEFINE p_bajas_riss                 INTEGER  --total procesados BAJA RISS
   DEFINE p_rfc_riss                   INTEGER  --total procesados RISS RFC
   DEFINE p_num_altas_en_reingreso     INTEGER  -- número de altas realizadas en el reingreso
   DEFINE p_nombre_archivo             STRING

   FORMAT
      FIRST PAGE HEADER
         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"
         -- se despliegan los datos del encabezado
         PRINTX p_folio
         PRINTX p_usuario_cod
         PRINTX v_fecha_texto
         PRINTX p_regs_procesados_alta
         PRINTX p_regs_aceptados_alta
         PRINTX p_regs_rechazados_alta
         PRINTX p_bajas_realizadas
         PRINTX p_cambios_nombre_realizados
         PRINTX p_nss_unificadores
         PRINTX p_nss_unificados
         PRINTX p_reingresos
         PRINTX p_bajas_aceptadas
         PRINTX p_bajas_rechazadas
         PRINTX p_cambio_nom_aceptados
         PRINTX p_cambio_nom_rechazados
         PRINTX p_reingresos_aceptados
         PRINTX p_reingresos_rechazados
         PRINTX p_num_altas_en_reingreso
         PRINTX p_altas_riss
         PRINTX p_bajas_riss
         PRINTX p_rfc_riss  
         PRINTX p_altas_riss_aceptadas
         PRINTX p_altas_riss_rechazadas
         PRINTX p_bajas_riss_aceptadas
         PRINTX p_bajas_riss_rechazadas
         PRINTX p_rfc_aceptadas
         PRINTX p_rfc_rechazadas
         PRINTX p_nombre_archivo

      BEFORE GROUP OF p_afi_rch_afiliatorio.cod_rechazo
         -- se obtiene la descripcion del rechazo
         SELECT des_rechazo
           INTO v_desc_rechazo
           FROM afi_cat_rch
          WHERE cod_rechazo = p_afi_rch_afiliatorio.cod_rechazo

         -- se envia la descripcion del tipo de rechazo
         PRINTX p_afi_rch_afiliatorio.cod_rechazo, v_desc_rechazo

      ON EVERY ROW
         -- se envian los registros rechazados
         PRINTX p_afi_rch_afiliatorio.*

END REPORT
