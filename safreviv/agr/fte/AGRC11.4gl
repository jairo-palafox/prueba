--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Módulo            => AGR                                           #
#Programa          => AGRC11                                        #
#Objetivo          => Programa que realiza la consulta de marcas    #
#                     de WS con Procesar                            #
#Autor             => Mauro Muñiz Caballero                         #
#Fecha creación    => 25 septiembre 2013                            #
#Modificó          => Mauro Muñiz Caballero                         #
#Fecha modifica    => 24 de junio de 2016                           #
#                     Nuevo diagnóstico de registros aceptados      #
#####################################################################

IMPORT os

DATABASE safre_viv

GLOBALS

   DEFINE g_ban_salir               SMALLINT
   DEFINE g_ciclo                   SMALLINT
   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutará el programa
   DEFINE p_s_titulo                STRING   -- título de la ventana
   DEFINE g_usuario                 CHAR(12)
   DEFINE g_fecha                   DATE
   DEFINE g_fecha_f                 DATE
   DEFINE f_fecha_fin               DATE

   DEFINE arr_marca DYNAMIC ARRAY OF RECORD
      situacion                     SMALLINT,
      desc_situacion                CHAR(10),
      fecha                         DATE,
      tot_marcas                    SMALLINT
   END RECORD

   DEFINE r_detsol DYNAMIC ARRAY OF RECORD
      solicitud                     CHAR(25),
      total_sol                     INTEGER
   END RECORD

   DEFINE f_w                       ui.form
   DEFINE w                         ui.window

END GLOBALS

MAIN

   -- se recuperan las claves desde parámetro
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRC11.log")

   CLOSE WINDOW SCREEN

   -- si se obtuvo el título, se pone como título de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL fn_proceso_principal()

END MAIN

FUNCTION fn_proceso_principal()
#pp----------------------------

   --se inicializan variables
   LET g_ban_salir = FALSE

   OPEN WINDOW w_consulta_marca WITH FORM "AGRC111"
      --LET w = ui.Window.forName("AGRC112")
      LET w = ui.Window.getCurrent()
      LET f_w = w.getForm()

      LET g_ciclo = TRUE
      LET g_fecha = NULL

      WHILE g_ciclo
         CALL fn_cons_fecha() RETURNING g_fecha, g_fecha_f

         IF g_fecha > '12/31/1899' AND g_fecha IS NOT NULL AND
            g_fecha_f > '12/31/1899' AND g_fecha_f IS NOT NULL THEN
            CALL fn_llena_arbol_marcas(g_fecha, g_fecha_f)
         END IF

         LET g_fecha   = '12/31/1899'
         LET g_fecha_f = '12/31/1899'
      END WHILE

   CLOSE WINDOW w_consulta_marca

END FUNCTION

FUNCTION fn_cons_fecha()
#fcf--------------------

   DEFINE v_fecha        DATE
   DEFINE v_fecha_f      DATE

   CALL f_w.setElementHidden("group2",1)
   --CALL f_w.setElementHidden("group3",1)

   INPUT v_fecha, v_fecha_f FROM fecha_consulta, fecha_hasta ATTRIBUTE (UNBUFFERED)
      BEFORE INPUT
         LET v_fecha_f = TODAY - 1 UNITS DAY
         LET v_fecha   = v_fecha_f - 6 UNITS DAY

         DISPLAY v_fecha, v_fecha_f TO fecha_consulta, fecha_hasta

         NEXT FIELD fecha_consulta

         ON ACTION ACCEPT
            IF v_fecha IS NULL THEN
               CALL fn_mensaje("Aviso","Se requiere ingresar la 'fecha desde' de la consulta","stop")
               LET v_fecha   = v_fecha_f - 6 UNITS DAY
               DISPLAY v_fecha TO fecha_consulta
               CONTINUE INPUT
            ELSE
               IF v_fecha_f IS NULL THEN
                  CALL fn_mensaje("Aviso","Se requiere ingresar la 'fecha hasta' de la consulta","stop")
                  LET v_fecha_f = TODAY - 1 UNITS DAY
                  DISPLAY v_fecha_f TO fecha_hasta
                  CONTINUE INPUT
               ELSE
                  IF v_fecha > v_fecha_f THEN
                     CALL fn_mensaje("Aviso","La 'fecha hasta' debe ser mayor o igual a la 'fecha desde' de la consulta","stop")
                     LET v_fecha_f = TODAY - 1 UNITS DAY
                     LET v_fecha   = v_fecha_f - 6 UNITS DAY
                     DISPLAY v_fecha, v_fecha_f TO fecha_consulta, fecha_hasta
                     NEXT FIELD fecha_consulta
                     CONTINUE INPUT
                  ELSE
                     --Valida la existencia de información
                     IF fn_verifica_registros(v_fecha, v_fecha_f) THEN
                        EXIT INPUT
                     ELSE
                        CALL fn_mensaje("Aviso","No existe información con la fecha indicada","stop")
                        CONTINUE INPUT
                     END IF	

                     NEXT FIELD fecha_consulta
                  END IF
               END IF
            END IF

         ON ACTION CANCEL
            --se canceló y se cierra la ventana de despliegue de registros
            LET g_ciclo     = FALSE
            LET v_fecha     = '12/31/1899'
            LET v_fecha_f   = '12/31/1899'

            EXIT INPUT

      END INPUT

   RETURN v_fecha, v_fecha_f

END FUNCTION

## Función que verifica si existen registros con base en la fecha indicada ##
FUNCTION fn_verifica_registros(p_fec_marca, p_fec_hasta)
#fvr----------------------------------------------------

   DEFINE v_tot_marca         INTEGER
   DEFINE v_s_existe_info     SMALLINT
   DEFINE v_situacion         SMALLINT
   DEFINE p_fec_marca         DATE
   DEFINE p_fec_hasta         DATE
   DEFINE v_f_marca           DATE
   DEFINE qry_string          STRING

   --Inicializa la variable de existencia de información
   LET v_s_existe_info = FALSE

   --Armado del query que obtiene el detalle de la consulta para verificar si existen registros
   LET qry_string = " SELECT s.situacion, s.f_actualiza, COUNT(*)\n ",
                    "   FROM cta_his_marca_ws s \n",
                    "  WHERE s.f_actualiza BETWEEN '",p_fec_marca,"' AND '",p_fec_hasta,"'\n",
                    "    AND s.tpo_credito = 2 \n",
                    "    AND s.situacion = 0 \n",
                    "    AND s.cod_result_op = 1 \n",
                    "    AND s.diagnostico = '' \n",
                    " GROUP BY 1,2 \n",
                    " ORDER BY 1 desc, 2 DESC "

      --Preparación del statement
      PREPARE prp_cnt_marcas FROM qry_string
      DECLARE cur_cnt_marcas CURSOR FOR prp_cnt_marcas

      FOREACH cur_cnt_marcas INTO v_situacion,v_f_marca,v_tot_marca
         --Si existe al menos un registro se envía señal que si existen registros
         LET v_s_existe_info = TRUE
      END FOREACH

   --Regresa valor recién obtenido
   RETURN v_s_existe_info

END FUNCTION

## Función que llena el árbol de marcas a desplegar ##
FUNCTION fn_llena_arbol_marcas(p_fec_marca, p_fec_hasta)
#flam--------------------------------------------------

   DEFINE v_tot_marca      INTEGER
   DEFINE v_sum_marca      INTEGER

   DEFINE i                SMALLINT
   DEFINE v_situacion      SMALLINT
   DEFINE v_pos            SMALLINT
   DEFINE resp_visualiza   SMALLINT

   DEFINE p_fec_marca      DATE
   DEFINE p_fec_hasta      DATE
   DEFINE v_f_marca        DATE
   DEFINE v_f_situacion    DATE

   DEFINE qry_string       STRING

   --DISPLAY "consulta crédito", qry_string

   LET i = 1

    --se limpia el arreglo que se va a desplegar
      CALL arr_marca.clear()

      LET qry_string = " SELECT UNIQUE s.situacion, s.f_actualiza, COUNT(*)\n ",
                       "   FROM cta_his_marca_ws s \n",
                       "  WHERE s.f_actualiza BETWEEN '",p_fec_marca,"' AND '",p_fec_hasta,"'\n",
                       "    AND s.tpo_credito = 2 \n",
                       "    AND s.situacion = 0 \n",
                       "    AND s.cod_result_op = 1 \n",
                       "    AND s.diagnostico = '' \n",
                       " GROUP BY 1,2 \n",
                       " ORDER BY 1 desc, 2 DESC "

      DISPLAY "MARCAS ", qry_string

      PREPARE prp_marcas FROM qry_string
      DECLARE cur_marcas CURSOR FOR prp_marcas

      FOREACH cur_marcas INTO v_situacion, v_f_marca, v_tot_marca
         LET arr_marca[i].situacion      = v_situacion
         LET arr_marca[i].desc_situacion = fn_desc_situacion(v_situacion)
         LET arr_marca[i].fecha          = v_f_marca
         LET arr_marca[i].tot_marcas     = v_tot_marca

         LET v_sum_marca = v_sum_marca + v_tot_marca
         LET i= i + 1
      END FOREACH

   CLOSE cur_marcas
   FREE cur_marcas

   LET i = i - 1

   CALL f_w.setElementHidden("group2",0)
   --CALL f_w.setElementHidden("group3",0)

   DIALOG ATTRIBUTES(UNBUFFERED)

   DISPLAY ARRAY arr_marca TO scr1.*

      BEFORE DISPLAY
         CALL DIALOG.setactionhidden("close",1)

      ON ACTION ACCEPT
         EXIT DIALOG

      ON ACTION CANCEL
         EXIT DIALOG

      --Invoca la generación del archivo   
      ON ACTION ARCHIVO
         CALL fn_genera_archivo_marcas(p_fec_marca, p_fec_hasta)
         CONTINUE DIALOG

      --Invoca la generación de reporte
      ON ACTION REPORTE
         CALL fn_genera_reporte_marcas()
         CONTINUE DIALOG

   END DISPLAY

   --DISPLAY ARRAY r_detsol TO scr2.*

   --END DISPLAY

   END DIALOG

END FUNCTION

## Funcion que obtiene la descripcion de la situación##
FUNCTION fn_desc_situacion(p_situacion)
#fds-----------------------------------

   DEFINE p_situacion      SMALLINT
   DEFINE v_desc_situacion CHAR(8)

   IF p_situacion = 2 THEN
      LET v_desc_situacion = 'MARCA'
   ELSE
      LET v_desc_situacion = 'DESMARCA'
   END IF

   RETURN  v_desc_situacion

END FUNCTION

FUNCTION fn_despliega_detalle_sol(p_situacion, p_fecha)
#fdds--------------------------------------------------

   DEFINE p_situacion   SMALLINT
   DEFINE i             SMALLINT
   DEFINE v_tot         DECIMAL(6,0)
   DEFINE v_sol         DECIMAL(6,0)

   DEFINE p_fecha       DATE

   CALL r_detsol.clear()

   LET i = 1
   LET v_sol = 0
   LET v_tot = 0

      ---Solicitudes aceptadas
   SELECT count(*)
     INTO v_sol
     FROM cta_his_marca_ws ha
    WHERE ha.situacion     = p_situacion
      AND ha.tpo_credito   = 2
      AND ha.f_actualiza   = p_fecha
      AND ha.cod_result_op = 1
      AND ha.diagnostico   = ''

   LET r_detsol[i].solicitud = "ACEPTADAS"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   LET i = i + 1

   IF v_tot = 0 THEN
      RETURN 0
   ELSE
      RETURN 1
   END IF

END FUNCTION

## Funcion que genera el archivo de marcas##
FUNCTION fn_genera_archivo_marcas(p_fecha, p_fecha_f)

   DEFINE v_cont_reg                INTEGER   --contador registro de detalle
   DEFINE v_s_existe_info           SMALLINT  --Bandera para indicar si existe información
   DEFINE v_c_fec_archivo           CHAR(8)  --Fecha del nombre de archivo
   DEFINE v_c_nom_archivo           CHAR(30)
   DEFINE p_fecha                   DATE  ---Fecha desde de solicitud
   DEFINE p_fecha_f                 DATE  ---Fecha hasta de solicitud
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio --Ruta envío del archivo
   DEFINE v_c_ruta_escritura        STRING --Ruta donde escribirá el archivo
   DEFINE v_s_qry                   STRING --String para armar el query
   DEFINE v_s_registro              STRING --Registro del archivo
   DEFINE v_ch_archivo              base.channel --Canal para escritura del archivo
   DEFINE v_paterno                 CHAR(40) --Apellido paterno
   DEFINE v_materno                 CHAR(40) --Apellido materno
   DEFINE v_nombre                  CHAR(40) --Nombre
   DEFINE v_curp                    CHAR(18) --Curp
   DEFINE v_rfc                     CHAR(13) --RFC
   DEFINE v_c_nss                   CHAR (11) --Número de seguridad social
   DEFINE v_c_num_credito           DECIMAL(10,0) --Número de crédito para la consulta de cta_marca_ws
   DEFINE v_c_tpo_credito           CHAR (2) --Tipo de crédito de la tabla cta_marca_ws
   DEFINE v_c_rechazo               CHAR (3) --Motivo de rechazo de acuerdo a código de PROCESAR
   DEFINE v_c_situacion             CHAR (1) --Situación del registro 
   DEFINE v_c_accion                CHAR (1) --Acción de acuerdo con situación

   DEFINE v_r_encabezado RECORD
      tpo_registro                  CHAR(2), -- Tipo de Registro (001-002)
      id_servicio                   CHAR(2), -- Identificador de Servicio (003-004)
      id_operacion                  CHAR(2), -- Identificador de Operación (005-006)
      tpo_ent_origen                CHAR(2), -- Tipo de entidad origen (007-008)
      cve_ent_origen                CHAR(3), -- Clave de entidad origen (009-011)
      tpo_ent_destino               CHAR(2), -- Tipo de entidad destino (012-013)
      cve_ent_destino               CHAR(3), -- Clave de entidad destino (014-016)
      ent_fed_env_lote              CHAR(3), -- Entidad federativa de envío de lote (017-019)
      fec_presentacion              CHAR(8), -- Fecha de presentación (020-027)
      consec_lote                   CHAR(3), -- Consecutivo del lote en el día (028-030)
      filler1                       CHAR(2), -- Filler (031-032)
      cod_resul_opera               CHAR(2), -- Código de resultado de la Operación (033-034)
      mot_rechazo                   CHAR(9), -- Motivo de rechazo del lote (035-043)
      filler2                       CHAR(687)-- Filler (044-730)
   END RECORD

   DEFINE v_r_detalle RECORD
      tpo_registro                  CHAR(2), -- Tipo de Registro (001-002)
      cont_servicio                 CHAR(10), -- Contador de Servicio (003-012)
      tpo_ent_recept                CHAR(2), -- Tipo de entidad receptora de la cuenta (013-014)
      cve_ent_recept                CHAR(3), -- Clave de entidad receptora de la cuenta (015-017)
      tpo_ent_cedente               CHAR(2), -- Tipo de entidad cedente de la cuenta (018-019)
      cve_ent_cedente               CHAR(3), -- Clave de entidad ced. de la cuenta (020-022)
      tpo_transferencia             CHAR(2), -- Origen/Tipo de transferencia (023-024)
      f_presentacion                CHAR(8), -- Fecha de presentación (025-032)
      filler1                       CHAR(8), -- Filler1 (033-040)
      curp_trabajador               CHAR(18), -- CURP del trabajador (041-058)
      nss_trab_infonavit            CHAR(11), -- NSS del trabajador según INFONAVIT (059-069)
      filler2                       CHAR(15), -- Filler2 (070-084)
      rfc_trab_infonavit            CHAR(13), -- RFC del trabajador según INFONAVIT (085-097)
      ape_pat_infonavit             CHAR(40), -- Apellido paterno del trabajador en el INFONAVIT (098-137)
      ape_mat_infonavit             CHAR(40), -- Apellido materno del trabajador en el INFONAVIT (138-177)
      nom_trab_infonavit            CHAR(40), -- Nombres del trabajador en el INFONAVIT (178-217)
      filler3                       CHAR(22), -- Filler3 (218-239)
      id_lote_solic                 CHAR(16), -- Identificador de lote de la solicitud (240-255)
      filler4                       CHAR(15), -- Filler4 (256-270)
      nss_trab_afore                CHAR(11), -- NSS del trabajador según AFORE/PS. (271-281)
      rfc_trab_afore                CHAR(13), -- RFC del trabajador según AFORE (282-294)
      filler5                       CHAR(30), -- Filler5 (295-324)
      ape_pat_afore                 CHAR(40), -- Apellido paterno del trabajador en la AFORE cedente (325-364)
      ape_mat_afore                 CHAR(40), -- Apellido materno del trabajador en la AFORE cedente (365-404)
      nom_trab_afore                CHAR(40), -- Nombres del trabajador en la AFORE cedente (405-444)
      filler6                       CHAR(45), -- Filler6 (445-489)
      num_apl_int_viv92             CHAR(15), -- Número de "Aplicaciones de Intereses de Vivienda" 92 de la última aportación (490-504)
      filler7                       CHAR(60), -- Filler7 (505-564)
      num_apl_int_viv97             CHAR(15), -- Número de "Aplicaciones de Intereses de Vivienda" 97 de la última aportación (465-479)
      filler8                       CHAR(3), -- Filler8 (580-582)
      cod_result_opera              CHAR(2), -- Código Resultado de la Operación (583-584)
      diagnostico_proc              CHAR(15), -- Diagnóstico del Proceso (585-599)
      nom_trab_imss                 CHAR(50), -- Nombre del Trabajador según IMSS (600-649)
      num_cred_infonavit            CHAR(10), -- Número de Crédito INFONAVIT (650-659)
      ints_v97                      CHAR(15), -- Intereses saldo viv 97 (660-674)
      ints_v92                      CHAR(15), -- Intereses saldo viv 92 (675-689)
      filler9                       CHAR(41) -- Filler9 (690-730)
   END RECORD

   DEFINE v_r_sumario RECORD
      tpo_registro                 CHAR(2), -- tipo de registro (001-002)
      cant_regs_detalle            CHAR(9), -- cantidad de registros de detalle (003-011)
      filler                       CHAR(719) -- filler2 (147-730)
   END RECORD

   --Inicializa bandera y contador registgro detalle
   LET v_s_existe_info = FALSE

   LET v_cont_reg = 0

   --Se obtiene la ruta de envío del archivo
   SELECT ruta_envio 
     INTO v_c_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   --Se asgna la fecha de solicitud
   LET v_c_fec_archivo = TODAY USING "ddmmyyyy"

   --Se asigna el nombre del archivo
   LET v_c_nom_archivo = "/Acepdm43bisprc",v_c_fec_archivo,".bws"

      --Concatenación de la ruta donde escribirá el archivo
   LET v_c_ruta_escritura = v_c_ruta_envio CLIPPED || v_c_nom_archivo

   --Se abre canal de escritura
   LET v_ch_archivo= base.Channel.create()
   CALL v_ch_archivo.openFile(v_c_ruta_escritura,"w")

   --Se arma el query de aceptados
   LET v_s_qry= " SELECT a.nss,                 \n",
                "        a.ap_paterno_af,       \n",
                "        a.ap_materno_af,       \n",
                "        a.nombre_af,           \n",
                "        a.curp,                \n",
                "        a.rfc,                 \n",
                "        c.num_credito,         \n",
                "        c.tpo_credito,         \n",
                "        c.diagnostico,         \n",
                "        c.situacion            \n",
                "   FROM cta_his_marca_ws c,    \n",
                "        afi_derechohabiente a  \n",
                "  WHERE c.f_actualiza BETWEEN '",p_fecha,"' AND '",p_fecha_f,"' \n",
                "    AND c.tpo_credito = 2 \n",
                "    AND c.situacion = 0   \n",
                "    AND c.cod_result_op = 1 \n",
                "    AND c.diagnostico = '' \n",
                "    AND c.id_derechohabiente = a.id_derechohabiente \n",
                "ORDER BY c.situacion DESC, a.nss "

   #debug#
   DISPLAY "Query aceptados ",v_s_qry
  #END DEBUG#

   -- se asignan los valores del registro encabezado a insertar
   LET v_r_encabezado.tpo_registro     = "01"
   LET v_r_encabezado.id_servicio      = "02"
   LET v_r_encabezado.id_operacion     = "30"
   LET v_r_encabezado.tpo_ent_origen   = "04"
   LET v_r_encabezado.cve_ent_origen   = "002"
   LET v_r_encabezado.tpo_ent_destino  = "01"
   LET v_r_encabezado.cve_ent_destino  = "" -- 3 espacios en blanco
   LET v_r_encabezado.ent_fed_env_lote = "009"
   LET v_r_encabezado.fec_presentacion = TODAY USING "YYYYMMDD"
   LET v_r_encabezado.consec_lote      = "001"
   LET v_r_encabezado.filler1          = "" -- 2 espacios en blanco
   LET v_r_encabezado.cod_resul_opera  = "01" -- 2 espacios en blanco
   LET v_r_encabezado.mot_rechazo      = "" -- 9 espacios en blanco
   LET v_r_encabezado.filler2          = "" -- 687 espacios en blanco

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
                      v_r_encabezado.filler1,
                      v_r_encabezado.cod_resul_opera,
                      v_r_encabezado.mot_rechazo,
                      v_r_encabezado.filler2

   CALL v_ch_archivo.writeLine(v_s_registro)

   --Se prepara el statement
   PREPARE stm_aceptados FROM v_s_qry
   DECLARE cur_aceptados CURSOR FOR stm_aceptados

     --Recorrido del cursor
   FOREACH cur_aceptados INTO  v_c_nss,
                               v_paterno,
                               v_materno,
                               v_nombre,
                               v_curp,
                               v_rfc,
                               v_c_num_credito,
                               v_c_tpo_credito,
                               v_c_rechazo,
                               v_c_situacion

      LET v_cont_reg = v_cont_reg + 1

      --Se asigna la acción correspondiente
      IF v_c_situacion = 2 THEN
         --Marca
         LET v_c_accion='M'
      END IF

      IF v_c_situacion = 0 THEN
           --Desmarca   
           LET v_c_accion = 'D'
      END IF

      --Se formatea el tipo y el número
      LET v_c_tpo_credito = v_c_tpo_credito USING "&&"
      --LET v_c_num_credito = v_c_num_credito USING "&&&&&&&&&&"

      -- se asignan los valores en el registro detalle
      LET v_r_detalle.tpo_registro       = "02"
      LET v_r_detalle.cont_servicio      = v_cont_reg USING "&&&&&&&&&&"
      LET v_r_detalle.tpo_ent_recept     = "01"
      LET v_r_detalle.cve_ent_recept     = "566"
      LET v_r_detalle.tpo_ent_cedente    = "04"
      LET v_r_detalle.cve_ent_cedente    = "002" -- 3 espacios en blanco
      LET v_r_detalle.tpo_transferencia  = "" -- 2 espacios en blanco
      LET v_r_detalle.f_presentacion     = TODAY USING "yyyymmdd"
      LET v_r_detalle.filler1            = "" -- 8 espacios en blanco
      LET v_r_detalle.curp_trabajador    = v_curp
      LET v_r_detalle.nss_trab_infonavit = v_c_nss
      LET v_r_detalle.filler2            = "" -- 15 espacios en blanco
      LET v_r_detalle.rfc_trab_infonavit = v_rfc
      LET v_r_detalle.ape_pat_infonavit  = v_paterno
      LET v_r_detalle.ape_mat_infonavit  = v_materno
      LET v_r_detalle.nom_trab_infonavit = v_nombre
      LET v_r_detalle.filler3            = "" -- 22 espacios en blanco
      LET v_r_detalle.id_lote_solic      = "04002",TODAY USING "YYYYMMDD","001"
      LET v_r_detalle.filler4            = "" -- 15 espacios en blanco
      LET v_r_detalle.nss_trab_afore     = v_c_nss
      LET v_r_detalle.rfc_trab_afore     = v_rfc
      LET v_r_detalle.filler5            = "" -- 30 espacios en blanco
      LET v_r_detalle.ape_pat_afore      = v_paterno
      LET v_r_detalle.ape_mat_afore      = v_materno
      LET v_r_detalle.nom_trab_afore     = v_nombre
      LET v_r_detalle.filler6            = "" -- 30 espacios en blanco
      LET v_r_detalle.num_apl_int_viv92  = "000000000000000"
      LET v_r_detalle.filler7            = "" -- 78 espacios en blanco
      LET v_r_detalle.num_apl_int_viv97  = "000000000000000"
      LET v_r_detalle.filler8            = "" -- 3 espacios en blanco
      LET v_r_detalle.cod_result_opera   = "01"
      LET v_r_detalle.diagnostico_proc   = "" -- 15 espacios en blanco
      LET v_r_detalle.nom_trab_imss      = "" -- 50 espacios en blanco
      LET v_r_detalle.num_cred_infonavit = v_c_num_credito USING "&&&&&&&&&&"
      LET v_r_detalle.ints_v97           = "000000000000000"
      LET v_r_detalle.ints_v92           = "000000000000000"
      LET v_r_detalle.filler9            = "" -- 41 espacios en blanco

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tpo_registro,
                         v_r_detalle.cont_servicio,
                         v_r_detalle.tpo_ent_recept,
                         v_r_detalle.cve_ent_recept,
                         v_r_detalle.tpo_ent_cedente,
                         v_r_detalle.cve_ent_cedente,
                         v_r_detalle.tpo_transferencia,
                         v_r_detalle.f_presentacion,
                         v_r_detalle.filler1,
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
                         v_r_detalle.num_apl_int_viv92,
                         v_r_detalle.filler7,
                         v_r_detalle.num_apl_int_viv97,
                         v_r_detalle.filler8,
                         v_r_detalle.cod_result_opera,
                         v_r_detalle.diagnostico_proc,
                         v_r_detalle.nom_trab_imss,
                         v_r_detalle.num_cred_infonavit,
                         v_r_detalle.ints_v97,
                         v_r_detalle.ints_v92,
                         v_r_detalle.filler9

         --Escribe el detalle en el archivo
         CALL v_ch_archivo.writeLine(v_s_registro)
      END FOREACH

   -- se asignan los valores del registro sumario
   LET v_r_sumario.tpo_registro      = "09"
   LET v_r_sumario.cant_regs_detalle = v_cont_reg USING "&&&&&&&&&"
   LET v_r_sumario.filler            = "" -- 719 espacios en blanco

   -- se concatenan los campos a insertar
   LET v_s_registro = v_r_sumario.tpo_registro,
                      v_r_sumario.cant_regs_detalle,
                      v_r_sumario.filler

   -- se escribe el registro (sumario) en el archivo
   CALL v_ch_archivo.writeLine(v_s_registro)

   --Actualiza bandera de existencia
   LET v_s_existe_info = TRUE

   --Concluye la escritura del archivo
   CALL v_ch_archivo.close()

   IF v_s_existe_info THEN
      --Envía mensaje indicando que se acaba de crear un archivo 
      CALL fn_mensaje("Información","Se ha generado el archivo de Desmarcas Aceptadas de 43 Bis de Procesar via WS\n en la ruta"||v_c_ruta_escritura,"information")                     
   ELSE
      CALL fn_mensaje("Aviso","No existe información para generar el archivo","stop")
   END IF

END FUNCTION

## Funcion que genera el reporte de marcas##
FUNCTION fn_genera_reporte_marcas()

   DEFINE manejador_rpt             om.SaxDocumentHandler --Contenedor documentos reporte 
   DEFINE v_i_indice_situacion      SMALLINT
   DEFINE v_i_indice_solicitud      SMALLINT
   DEFINE v_i_resp_detalle          SMALLINT

   DEFINE r_det_envio RECORD
      v_s_situacion                 SMALLINT,
      v_c_desc_situacion            CHAR(10),
      v_d_fecha                     DATE,
      v_c_tot_marcas                SMALLINT,
      v_c_solicitud                 CHAR(25),
      v_i_total_sol                 INTEGER
   END RECORD 

   --Se asigna el manejador del reporte
   IF fgl_report_loadCurrentSettings("AGRC111.4rp") THEN 
       CALL fgl_report_selectDevice ("PDF")
       LET manejador_rpt = fgl_report_commitCurrentSettings()
    END IF

   --Inicializa el reporte
   START REPORT rpt_situacion TO XML HANDLER manejador_rpt

   --Recorrido del arreglo de marcas
   FOR v_i_indice_situacion = 1 TO arr_marca.getLength()
      --Asigna valores
      --Valida que no sea nula la situación
      #DEBUG#
      DISPLAY "Situación", arr_marca[v_i_indice_situacion].situacion 
      #END DEBUG#

      IF arr_marca[v_i_indice_situacion].situacion  IS NULL THEN 
         LET r_det_envio.v_s_situacion = 0
      ELSE
         LET r_det_envio.v_s_situacion = arr_marca[v_i_indice_situacion].situacion 
      END IF

      LET r_det_envio.v_c_desc_situacion = arr_marca[v_i_indice_situacion].desc_situacion 
      LET r_det_envio.v_d_fecha          = arr_marca[v_i_indice_situacion].fecha          
      LET r_det_envio.v_c_tot_marcas     = arr_marca[v_i_indice_situacion].tot_marcas    

      --Se invoca la función que obtiene la situacion
      CALL  fn_despliega_detalle_sol(r_det_envio.v_s_situacion,r_det_envio.v_d_fecha)
      RETURNING v_i_resp_detalle

      --Se recorre el resultado obtenido
      FOR v_i_indice_solicitud = 1 TO r_detsol.getLength()
         --Se asigna el valor de la solicitud
         LET r_det_envio.v_c_solicitud = r_detsol[v_i_indice_solicitud].solicitud

         --Valida 0
         IF r_detsol[v_i_indice_solicitud].total_sol IS NULL THEN
            LET r_det_envio.v_i_total_sol = 0
         ELSE
            LET r_det_envio.v_i_total_sol = r_detsol[v_i_indice_solicitud].total_sol
         END IF 

         --Se envía el registro al reporte
         OUTPUT TO REPORT rpt_situacion (r_det_envio.*)

      END FOR
   END FOR

   --Finaliza la elaboración del reporte
   FINISH REPORT rpt_situacion

END FUNCTION

## Reporte de marcas##
REPORT rpt_situacion (r_det_reporte)

   DEFINE r_det_reporte RECORD 
      v_s_situacion          CHAR(1),
      v_c_desc_situacion     CHAR(10),
      v_d_fecha              DATE,
      v_c_tot_marcas         SMALLINT,
      v_c_solicitud          CHAR(25),
      v_i_total_sol          CHAR(20)
   END RECORD

   DEFINE v_d_fecha          CHAR(10) 

  ORDER BY r_det_reporte.v_s_situacion

  FORMAT

   FIRST PAGE HEADER
   --Asigna fecha
   LET v_d_fecha = TODAY USING "DD-MM-YYYY"

    PRINTX g_usuario
    PRINTX v_d_fecha

   BEFORE GROUP OF r_det_reporte.v_s_situacion

      PRINTX r_det_reporte.v_s_situacion
      PRINTX r_det_reporte.v_c_desc_situacion
      PRINTX r_det_reporte.v_d_fecha
      PRINTX r_det_reporte.v_c_tot_marcas

  ON EVERY ROW

    PRINTX r_det_reporte.v_c_solicitud
    PRINTX r_det_reporte.v_i_total_sol

END REPORT