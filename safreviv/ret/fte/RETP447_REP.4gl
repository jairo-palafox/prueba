################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  =>                                                 #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => RET                                                      #
#Programa          => RETP447_REP                                              #
#Objetivo          => Programa para probar el reporte de respuesta para aviso  #
#                     a CERAC                                                  #
#Fecha inicio      => Mayo 25, 2018                                            #
################################################################################
IMPORT os
DATABASE safre_viv 
GLOBALS "RETG01.4gl"
PRIVATE DEFINE v_ruta_pdf    STRING
GLOBALS
 DEFINE v_usuario      VARCHAR(30), -- Almacena al usuario
        g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo de proceso
        g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
        l_pid          LIKE glo_pid.pid,
        g_folio        LIKE dis_det_avance_pago.folio, -- Folio generado
        l_arch_proceso VARCHAR(100)
END GLOBALS

--Objetivo: Funcion que realiza la carga de tablas hitoricas de avance de pago
MAIN

         CALL fn_procesa_reportes() 
END MAIN

FUNCTION fn_procesa_reportes()
   DEFINE p_id_derechohabiente DECIMAL(10,0)
   DEFINE v_consulta           STRING 
   DEFINE i                    INTEGER 
   DEFINE v_id_derechohabiente DECIMAL(10,0)

   DEFINE arr_datos_reporte RECORD
            id_solicitud         LIKE ret_excep_devol_ssv.id_solicitud,
            nss                  LIKE ret_excep_devol_ssv.nss,
            folio                LIKE ret_excep_devol_ssv.folio,
            estado_solicitud     LIKE ret_excep_devol_ssv.estado_solicitud,
            cod_rechazo          LIKE ret_excep_devol_ssv.cod_rechazo,
            num_delega           LIKE ret_excep_devol_ssv.num_delega,
            beneficiario         LIKE ret_excep_devol_ssv.beneficiario,
            importe              LIKE ret_excep_devol_ssv.importe,
            entidad              LIKE ret_excep_devol_ssv.entidad,
            juicio               LIKE ret_excep_devol_ssv.juicio,
            num_acuerdo          LIKE ret_excep_devol_ssv.num_acuerdo,
            desc_juez            LIKE ret_excep_devol_ssv.desc_juez,
            facultado            LIKE ret_excep_devol_ssv.facultado,
            puesto               LIKE ret_excep_devol_ssv.puesto,
            fch_ejecuta          LIKE ret_excep_devol_ssv.fch_ejecuta,
            procede_juicio       LIKE ret_excep_devol_ssv.procede_juicio,
            tipo_sol             LIKE ret_excep_devol_ssv.tipo_sol,
            tipo_prod            LIKE ret_excep_devol_ssv.tipo_prod,
            correo_elec          LIKE ret_excep_devol_ssv.correo_elec,
            desc_rechazo         LIKE ret_excep_devol_ssv.desc_rechazo,
            fch_contabiliza      LIKE ret_excep_devol_ssv.fch_contabiliza,
            fch_vencimiento      LIKE ret_excep_devol_ssv.fch_vencimiento,
            id_archivo_envio     LIKE ret_excep_devol_ssv.id_archivo_envio,
            id_archivo_respuesta LIKE ret_excep_devol_ssv.id_archivo_respuesta,
            importe_cuenta       LIKE ret_excep_devol_ssv.importe_cuenta,
            f_solicitud          DATE,
            f_autoriza           DATE,
            f_pago               DATE 
 
   END RECORD 

   -- Limpia el arreglo
   LET i = 0


   LET v_consulta = "SELECT *                          " ,
                    " FROM                             " ,
                    "       ret_excep_devol_ssv        " ,
                    " WHERE id_solicitud IN (SELECT referencia          ",
                    "                        FROM   ret_respuesta_fico  ",
                    "                        WHERE  folio = 8950        ",
                    "                        AND    bandera = 0)        "
   DISPLAY "La consulta para los reportes ,", v_consulta
   PREPARE prp_casos_reporte FROM v_consulta
   DECLARE cur_casos_reporte CURSOR FOR prp_casos_reporte

   FOREACH cur_casos_reporte INTO arr_datos_reporte.*
      CALL fn_genera_reporte(arr_datos_reporte.*)
      LET i = i + 1
   END FOREACH

END FUNCTION


PRIVATE  FUNCTION fn_genera_reporte(p_arr_datos_reporte)
   DEFINE p_arr_datos_reporte RECORD
            id_solicitud         LIKE ret_excep_devol_ssv.id_solicitud,
            nss                  LIKE ret_excep_devol_ssv.nss,
            folio                LIKE ret_excep_devol_ssv.folio,
            estado_solicitud     LIKE ret_excep_devol_ssv.estado_solicitud,
            cod_rechazo          LIKE ret_excep_devol_ssv.cod_rechazo,
            num_delega           LIKE ret_excep_devol_ssv.num_delega,
            beneficiario         LIKE ret_excep_devol_ssv.beneficiario,
            importe              LIKE ret_excep_devol_ssv.importe,
            entidad              LIKE ret_excep_devol_ssv.entidad,
            juicio               LIKE ret_excep_devol_ssv.juicio,
            num_acuerdo          LIKE ret_excep_devol_ssv.num_acuerdo,
            desc_juez            LIKE ret_excep_devol_ssv.desc_juez,
            facultado            LIKE ret_excep_devol_ssv.facultado,
            puesto               LIKE ret_excep_devol_ssv.puesto,
            fch_ejecuta          LIKE ret_excep_devol_ssv.fch_ejecuta,
            procede_juicio       LIKE ret_excep_devol_ssv.procede_juicio,
            tipo_sol             LIKE ret_excep_devol_ssv.tipo_sol,
            tipo_prod            LIKE ret_excep_devol_ssv.tipo_prod,
            correo_elec          LIKE ret_excep_devol_ssv.correo_elec,
            desc_rechazo         LIKE ret_excep_devol_ssv.desc_rechazo,
            fch_contabiliza      LIKE ret_excep_devol_ssv.fch_contabiliza,
            fch_vencimiento      LIKE ret_excep_devol_ssv.fch_vencimiento,
            id_archivo_envio     LIKE ret_excep_devol_ssv.id_archivo_envio,
            id_archivo_respuesta LIKE ret_excep_devol_ssv.id_archivo_respuesta,
            importe_cuenta       LIKE ret_excep_devol_ssv.importe_cuenta,
            f_solicitud          DATE,
            f_autoriza           DATE,
            f_pago               DATE 
 
   END RECORD 

   DEFINE arr_detalle_reporte    RECORD 
            entidad           CHAR(40),
            juicio            LIKE ret_excep_devol_ssv.juicio,
            autoridad         CHAR(40),
            paterno           CHAR(40),     -- Apellido paterno
            materno           CHAR(40),     -- Apellido materno
            nombre            CHAR(40),     -- Nombre
            nss               CHAR(11),     -- NSS
            imp_pago          DECIMAL(22,2),
            c_imp_pago        STRING,
            ref_dap           CHAR(20),
            cve_servicio      CHAR(2),
            no_transaccion    CHAR(2),
            expediente        CHAR(15),
            procede_juicio    CHAR(40),
            desc_juez         CHAR(40),
            puesto_facultado  CHAR(40),
            nombre_facultado  CHAR(40)
   END RECORD 

   DEFINE reporte           om.SaxDocumentHandler

   DEFINE i                 SMALLINT
   DEFINE v_reporte         STRING
   DEFINE v_ruta_listados   CHAR(40)
   DEFINE v_ruta_reporte    STRING
   DEFINE f_inicio          DATE
   DEFINE f_fin             DATE
   DEFINE v_query           STRING 
   DEFINE v_nombre          CHAR(120)
   DEFINE v_rfc             CHAR(13)
   DEFINE v_curp            CHAR(18)
   DEFINE v_nombre_stg      STRING 
   DEFINE v_fecha_paso      CHAR(16)
   DEFINE v_fecha           CHAR(16)

   LET v_reporte= "RETP447.4rp"

   -- Pasa los valores al arreglo de impresión
   LET arr_detalle_reporte.juicio            = p_arr_datos_reporte.juicio
   LET arr_detalle_reporte.autoridad         = ""
   LET arr_detalle_reporte.nss               = p_arr_datos_reporte.nss
   LET arr_detalle_reporte.imp_pago          = p_arr_datos_reporte.importe_cuenta
   LET arr_detalle_reporte.cve_servicio      = "20"
   LET arr_detalle_reporte.no_transaccion    = "24"
   LET arr_detalle_reporte.expediente        = p_arr_datos_reporte.num_acuerdo
   LET arr_detalle_reporte.procede_juicio    = p_arr_datos_reporte.procede_juicio
   LET arr_detalle_reporte.desc_juez         = p_arr_datos_reporte.desc_juez
   LET arr_detalle_reporte.puesto_facultado  = p_arr_datos_reporte.puesto
   LET arr_detalle_reporte.nombre_facultado  = p_arr_datos_reporte.facultado


   LET arr_detalle_reporte.ref_dap           = "0"

   
   --- Se obtiene la descripción de la entidad 
   LET arr_detalle_reporte.entidad = ""
   
   SELECT DISTINCT entidad_desc_larga
   INTO   arr_detalle_reporte.entidad
   FROM   cat_entidad_federativa
   WHERE  entidad_federativa = p_arr_datos_reporte.entidad;

   --   Se obtiene el nombre del derechohabiente por separado
   SELECT nombre_af, ap_paterno_af, ap_materno_af
   INTO   arr_detalle_reporte.nombre, arr_detalle_reporte.paterno,
          arr_detalle_reporte.materno
   FROM   afi_derechohabiente
   WHERE  nss = p_arr_datos_reporte.nss

   --   se obtiene la cantidad en letra del importe del retiro
   CALL fn_importe_monto(p_arr_datos_reporte.importe_cuenta) RETURNING arr_detalle_reporte.c_imp_pago;
   

    SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = "ret"

    LET v_nombre_stg = v_nombre
    LET v_nombre_stg = v_nombre_stg CLIPPED 
    LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                       p_arr_datos_reporte.nss CLIPPED,"_", 
                       p_arr_datos_reporte.id_solicitud USING "&&&&&&&&&&","_"
                       ||YEAR(TODAY) CLIPPED ||MONTH(TODAY) CLIPPED
                       ||DAY(TODAY) CLIPPED,".pdf" 

    LET v_ruta_pdf = v_ruta_reporte
   DISPLAY "El archivo :", v_reporte
   DISPLAY "Ruta reporte :", v_ruta_reporte
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(FALSE)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      DISPLAY "NSS reporte ", p_arr_datos_reporte.nss
      IF reporte IS NOT NULL THEN
         START REPORT pdf_notificacion TO XML HANDLER reporte
            OUTPUT TO REPORT pdf_notificacion(arr_detalle_reporte.*)
         FINISH REPORT pdf_notificacion
      END IF
      IF p_arr_datos_reporte.correo_elec IS NOT NULL THEN 
         CALL fn_envia_pdf(v_ruta_reporte, p_arr_datos_reporte.correo_elec, p_arr_datos_reporte.nss)
      END IF 
   END IF
  
END FUNCTION 

REPORT pdf_notificacion(p_arr_detalle_reporte) 
   DEFINE p_arr_detalle_reporte    RECORD 
            entidad           CHAR(40),
            juicio            LIKE ret_excep_devol_ssv.juicio,
            autoridad         CHAR(40),
            paterno           CHAR(40),     -- Apellido paterno
            materno           CHAR(40),     -- Apellido materno
            nombre            CHAR(40),     -- Nombre
            nss               CHAR(11),     -- NSS
            imp_pago          DECIMAL(22,2),
            c_imp_pago        STRING,
            ref_dap           CHAR(20),
            cve_servicio      CHAR(2),
            no_transaccion    CHAR(2),
            expediente        CHAR(15),
            procede_juicio    CHAR(40),
            desc_juez         CHAR(40),
            puesto_facultado  CHAR(40),
            nombre_facultado  CHAR(40)
   END RECORD -- Código de rechazo


   DEFINE v_fecha           STRING 
   DEFINE v_acuerdo         STRING
   DEFINE v_juicio          STRING
   DEFINE v_autoridad       STRING
   DEFINE v_sr_a            CHAR(6)
   DEFINE v_nombre          STRING
   DEFINE v_presente        CHAR(9)
   DEFINE v_parrafo_p_1     STRING 
   DEFINE v_ref_dap         CHAR(40)
   DEFINE v_cve_servicio    CHAR(30)
   DEFINE v_n_transaccion   char(30)
   DEFINE v_atentamente     CHAR(12)
   DEFINE v_infonavit       CHAR(65)
   DEFINE v_anio            CHAR(8)
   DEFINE v_asunto          STRING 
   DEFINE v_parrafo_p_2_1   STRING
   DEFINE v_parrafo_p_2_2   STRING 
   DEFINE v_parrafo_p_2_3   STRING
   DEFINE v_parrafo_p_2_4   STRING
   DEFINE v_parrafo_p_3_1   STRING
   DEFINE v_parrafo_p_3_2   STRING
   DEFINE v_parrafo_p_3_3   STRING
   DEFINE v_parrafo_p_3_4   STRING
   DEFINE v_parrafo_p_3_5   STRING
   DEFINE v_pie_pagina      STRING 


      DEFINE p_nss             LIKE afi_derechohabiente.nss
    DEFINE p_curp            LIKE afi_derechohabiente.curp
    DEFINE p_paterno         CHAR(40)     -- Apellido paterno
    DEFINE p_materno         CHAR(40)     -- Apellido materno
    DEFINE p_nombre          CHAR(40)     -- Nombre
    DEFINE p_fecha_hora      CHAR(16)     -- Fecha de la Solicitud
    DEFINE p_id_solicitud    LIKE ret_solicitud_generico.id_solicitud
    DEFINE p_pesos_viv92     DECIMAL(22,2) -- Vivienda 92
    DEFINE p_pesos_viv97     DECIMAL(22,2) -- Vivienda 97
    DEFINE p_sello           CHAR(64) -- Acuse Generado

    DEFINE v_tramite            CHAR(50)     -- Descripción del Trámite
    DEFINE v_grupo              CHAR(55)
    DEFINE v_medioSolicitud     CHAR(10)     -- Medio por el cual se hizo la solicitud 
    DEFINE v_pesosViv92         CHAR(18) -- Vivienda 92
    DEFINE v_pesosViv97         CHAR(18) -- Vivienda 97
    DEFINE v_pesosTotal         CHAR(18) -- Suma en pesos total devuelto

   FORMAT


   FIRST PAGE HEADER

      LET v_fecha = p_arr_detalle_reporte.entidad CLIPPED , ", a ", TODAY USING "dd", " de ",
                    TODAY USING "mmm", " de ", TODAY USING "yyyy"
      LET v_acuerdo = "Acuerdo: SGRF/GAPSS/1801/2017"
      LET v_juicio  = "Juicio: ", p_arr_detalle_reporte.juicio CLIPPED 
      LET v_autoridad = "Autoridad"
      LET v_sr_a = "Sr(a):"
      LET v_nombre = p_arr_detalle_reporte.nombre CLIPPED, " ", 
                     p_arr_detalle_reporte.paterno CLIPPED, " ",
                     p_arr_detalle_reporte.materno CLIPPED 
      LET v_presente = "Presente:"
      DISPLAY "el Importe que se reporta >", p_arr_detalle_reporte.imp_pago, "<"
      LET v_parrafo_p_1 = "Por este conducto se le informa que este Instituto transfirió ",
                          "los recursos de su Subcuenta de Vivienda 97, con el Número de ",
                          "Seguridad Social ",p_arr_detalle_reporte.nss, ", en cantidad total",
                          " de $ ",p_arr_detalle_reporte.imp_pago USING "###,###,##&.&&", " (",
                          p_arr_detalle_reporte.c_imp_pago CLIPPED, "),  a la institución bancaria ",
                          "BANCO HSBC INSTITUCIÓN DE BANCA MÚLTIPLE, GRUPO FINANCIERO HSBC, ",
                          "movimiento que se acredita con una Orden de Pago DAP (Dispersión ",
                          "Automatizada de Pagos), con la referencia indicada por la institución ",
                          "bancaria que es la siguiente:"
      LET v_ref_dap = "CLAVE DE REFERENCIA DAP:"
      LET v_cve_servicio = "CLAVE DE SERVICIO: 20"
      LET v_n_transaccion = "NO. DE TRANSACCIÓN: 24"
      LET v_atentamente = "Atentamente."
      LET v_infonavit = "Instituto del Fondo Nacional de la Vivienda para los Trabajadores"
      LET v_anio = "año ", TODAY USING "yyyy"
      LET v_asunto = "ASUNTO: DEVOLUCIÓN DEL SALDO DE LA SUBCUENTA DE VIVIENDA DEL TRABAJADOR ",
                     "CUYO NÚMERO DE SEGURIDAD SOCIAL SE INDICA."
      LET v_parrafo_p_2_1 = p_arr_detalle_reporte.desc_juez CLIPPED, ", promoviendo mi carácter de ",
                            p_arr_detalle_reporte.puesto_facultado CLIPPED, " del Instituto del ",
                            "Fondo Nacional de la Vivienda para los Trabajadores en el estado de ",
                            p_arr_detalle_reporte.entidad CLIPPED, ", en los autos del expediente ",
                            "al rubro citado, ante usted respetuosamente expongo:"
      LET v_parrafo_p_2_2 = "Por medio del presente oficio, vengo a manifestar que este Instituto ",
                            "por este conducto, está realizando la DEVOLUCIÓN DEL SALDO DE LA ",
                            "SUBCUENTA DE VIVIENDA DEL TRABAJADOR MATERIA DEL PRESENTE JUICIO ",
                            "LABORAL CUYO NÚMERO DE SEGURIDAD SOCIAL Y MONTO A CONTINUACIÓN SE SEÑALA:"
      LET v_parrafo_p_2_3 = "Lo anterior, toda vez que este Instituto transfirió los recursos de la ",
                            "subcuenta de vivienda del trabajador a la institución de crédito denominada ",
                            "BANCO HSBC INSTITUCIÓN DE BANCA MÚLTIPLE, GRUPO FINANCIERO HSBC, por el ",
                            "monto total que en el cuadro que antecede se señala."
      LET v_parrafo_p_2_4 = "Para acreditar lo anterior, me permito adjuntar en sobre cerrado la orden ",
                            "de pago DAP (Dispersión Automatizada de Pagos, cuyos últimos cuatro dígitos ",
                            "de referencia o folio son ", v_anio, ", con base en la cual se acredita la ",
                            "TRANSFERENCIA BANCARIA realizada por este Organismo Fiscal Autónomo a favor ",
                            "de ", v_nombre CLIPPED, " con cargo a la institución de crédito referida, lo ",
                            "anterior a efecto de que sirva notificar al citado trabajador para que ",
                            "comparezca a esta H. Junta a recoger la citada orden de pago DAP y a su vez ",
                            "acuda a la Institución Bancaria de referencia, tres días hábiles contados a ",
                            "partir de la fecha de recepción de este documento para recibir la totalidad ",
                            "de los recursos, lo anterior para la protección de la referida orden de pago."
      LET v_parrafo_p_3_1 = "De igual manera, me permito manifestar que en virtud de la presente promoción ",
                            "queda sin efecto cualquier oficio que hubiera sido girado por cualquier ",
                            "funcionario del Instituto que represento, mediante el cual se hubiere llegado ",
                            "a negar al trabajador la devolución del saldo de su subcuenta de vivienda."
      LET v_parrafo_p_3_2 = "Así mismo solicito se de vista al trabajador y se le aperciba que una vez ",
                            "que recoja el DAP, ante la presencia Judicial, deberá de manifestar lo que a ",
                            "su interés convenga en el plazo de tres días, apercibiéndole que en caso de ",
                            "no llevar a cabo alguna manifestación, se dará por concluido el presente ",
                            "juicio."
      LET v_parrafo_p_3_3 = "Lo anterior, de conformidad con el acuerdo SGRG/GAPSS/1801/2017 de fecha 12 ",
                            "de julio de 2017, mediante el cual el C. Coordinador General de Recaudación ",
                            "Fiscal de este Instituto, tuvo a bien determinar los montos a devolver de ",
                            "los fondos acumulados en la subcuenta de vivienda posteriores al 30 de junio ",
                            "de 1997, que les corresponden a los trabajadores pensionados de conformidad ",
                            "con la normatividad aplicable a esta Institución, acorde con la información ",
                            "que obra en las bases de datos de esta Autoridad y al procedimiento que señala  ",
                            "en la tesis de Jurisprudencia número 31/2012 (10ª) de la Segunda Sala de la H. ",
                            "Suprema Corte de Justicia de la Nación, así como en términos de lo dispuesto ",
                            "por el Pleno del mismo H. Máximo Tribunal del país en el Acuerdo General 7/2012 ",
                            "emitido por él, publicado en el Diario Oficial de la Federación el veinte de ",
                            "julio de dos mil doce."
      LET v_parrafo_p_3_4 = "Por lo antes expuesto, a esta H. Junta, atentamente pido se sirva:"
      LET v_parrafo_p_3_5 = "ÚNICO. Tenerme por presentado en los términos del presente escrito, ",
                            "realizando la DEVOLUCIÓN DEL SALDO DE LA SUBCUENTA DE VIVIENDA DEL TRABAJADOR ",
                            "y en su oportunidad dar por concluido el presente juicio."
      LET v_pie_pagina = p_arr_detalle_reporte.puesto_facultado CLIPPED, " del Instituto del Fondo ",
                         "Nacional de la Vivienda para los Trabajadores en el estado de ", p_arr_detalle_reporte.entidad CLIPPED 
      PRINTX p_arr_detalle_reporte.entidad
      PRINTX p_arr_detalle_reporte.juicio
      PRINTX p_arr_detalle_reporte.autoridad
      PRINTX p_arr_detalle_reporte.paterno
      PRINTX p_arr_detalle_reporte.materno
      PRINTX p_arr_detalle_reporte.nombre
      PRINTX p_arr_detalle_reporte.nss
      PRINTX p_arr_detalle_reporte.imp_pago
      PRINTX p_arr_detalle_reporte.c_imp_pago
      PRINTX p_arr_detalle_reporte.ref_dap
      PRINTX p_arr_detalle_reporte.cve_servicio
      PRINTX p_arr_detalle_reporte.no_transaccion
      PRINTX p_arr_detalle_reporte.expediente
      PRINTX p_arr_detalle_reporte.procede_juicio
      PRINTX p_arr_detalle_reporte.desc_juez
      PRINTX p_arr_detalle_reporte.puesto_facultado
      PRINTX p_arr_detalle_reporte.nombre_facultado
      PRINTX v_asunto
      PRINTX v_parrafo_p_2_1
      PRINTX v_parrafo_p_2_2
      PRINTX v_parrafo_p_2_3
      PRINTX v_parrafo_p_2_4
      PRINTX v_parrafo_p_3_1
      PRINTX v_parrafo_p_3_2
      PRINTX v_parrafo_p_3_3
      PRINTX v_parrafo_p_3_4
      PRINTX v_parrafo_p_3_5
      PRINTX v_pie_pagina


      PRINTX v_fecha
      PRINTX v_acuerdo
      PRINTX v_juicio
      PRINTX v_autoridad
      PRINTX v_sr_a
      PRINTX v_nombre
      PRINTX v_presente
      PRINTX v_parrafo_p_1
      PRINTX v_ref_dap
      PRINTX v_cve_servicio
      PRINTX v_n_transaccion
      PRINTX v_atentamente
      PRINTX v_infonavit
      PRINTX v_anio

      
END REPORT

FUNCTION fn_importe_monto(monto)
   DEFINE monto      DECIMAL(10,2)
   DEFINE monto1     DECIMAL(10,2)
   DEFINE monto2     DECIMAL(10,2)
   DEFINE c_monto    STRING
   DEFINE monto_c    STRING
   DEFINE largo      SMALLINT
   DEFINE pos_pto    SMALLINT
   DEFINE centavos   SMALLINT
   DEFINE millones   SMALLINT
   DEFINE miles      SMALLINT
   DEFINE unos       SMALLINT

   --Obtengo los valores para cada unidad
   LET c_monto    = monto
   LET largo      = c_monto.getLength()
   LET pos_pto    = c_monto.getIndexOf(".",1)
   LET monto_c    = c_monto.subString(pos_pto+1,largo)
   LET centavos   = monto_c
   LET millones   = monto / 1000000
   LET monto1     = monto - (millones * 1000000)
   LET miles      = monto1 / 1000
   LET monto2     = monto1 - (miles * 1000)
   LET unos       = monto2 / 1
   LET c_monto    = ""
   
   --Defino el nivel y realizo la transformación
   IF millones > 0 THEN
      CALL fn_cadena_nivel(millones,3) RETURNING monto_c
      LET c_monto = c_monto.append(monto_c)
   END IF
   
   IF miles > 0 THEN
      CALL fn_cadena_nivel(miles,2) RETURNING monto_c
      LET c_monto = c_monto.append(monto_c)
   END IF
   
   IF unos > 0 THEN
      CALL fn_cadena_nivel(unos,1) RETURNING monto_c
      LET c_monto = c_monto.append(monto_c)
   END IF
   
   IF millones > 0 AND miles = 0 AND unos = 0 THEN
      LET c_monto = c_monto.append("de ")
   END IF
   
   IF (millones+miles+unos) = 0 THEN
      LET c_monto  = "cero pesos " , centavos USING "&&" , "/100 M.N."
   ELSE
      IF monto >= 2 THEN
         LET monto_c = "pesos " , centavos USING "&&" , "/100 M.N."
      ELSE
         LET monto_c = "peso " , centavos USING "&&" , "/100 M.N."
      END IF
      LET c_monto = c_monto.append(monto_c)
   END IF
   
   RETURN c_monto
   
END FUNCTION

{
======================================================================
Nombre: fn_cadena_nivel
Fecha creacion: agosto 03, 2015
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
  Funcion que obtiene la cadena en base al nivel
     - Millones
     - Miles
     - Cientos

Registro de modificaciones:
======================================================================
}
FUNCTION fn_cadena_nivel(numero,grupo)
   DEFINE numero,grupo  SMALLINT
   DEFINE c_numero      CHAR(3)
   DEFINE unidades      SMALLINT
   DEFINE decenas       SMALLINT
   DEFINE centenas      SMALLINT
   DEFINE c_l           STRING
   
   LET c_numero = numero USING "&&&"
   LET centenas = c_numero[1]
   LET decenas  = c_numero[2]
   LET unidades = c_numero[3]
   IF centenas > 0 THEN
      CASE centenas
         WHEN 9 LET c_l = "novecientos "
         WHEN 8 LET c_l = "ochocientos "
         WHEN 7 LET c_l = "setecientos "
         WHEN 6 LET c_l = "seiscientos "
         WHEN 5 LET c_l = "quinientos "
         WHEN 4 LET c_l = "cuatrocientos "
         WHEN 3 LET c_l = "trescientos "
         WHEN 2 LET c_l = "doscientos "
         WHEN 1
            IF unidades = 0 AND decenas = 0 THEN
               LET c_l = "cien "
            ELSE
               LET c_l = "ciento "
            END IF
      END CASE
   END IF
   IF decenas > 0 THEN
      IF decenas > 2 THEN
         CASE decenas
            WHEN 9 LET c_l = c_l.append("noventa ")
            WHEN 8 LET c_l = c_l.append("ochenta ")
            WHEN 7 LET c_l = c_l.append("setenta ")
            WHEN 6 LET c_l = c_l.append("sesenta ")
            WHEN 5 LET c_l = c_l.append("cincuenta ")
            WHEN 4 LET c_l = c_l.append("cuarenta ")
            WHEN 3 LET c_l = c_l.append("treinta ")
         END CASE
         IF unidades > 0 THEN
            LET c_l = c_l.append("y ")
         END IF
      ELSE
         CASE decenas
            WHEN 2 
               IF unidades = 0 THEN  
                  LET c_l = c_l.append("veinte ")
               ELSE
                  LET c_l = c_l.append("veinti")
               END IF
            WHEN 1
               IF unidades = 0 THEN
                  LET c_l = c_l.append("diez ")
               ELSE
                  IF unidades > 5 THEN
                     LET c_l = c_l.append("dieci")
                  END IF
               END IF
         END CASE
      END IF
   END IF
   IF unidades > 0 THEN
      CASE unidades
         WHEN 9 LET c_l = c_l.append("nueve ")
         WHEN 8 LET c_l = c_l.append("ocho ")
         WHEN 7 LET c_l = c_l.append("siete ")
         WHEN 6 LET c_l = c_l.append("seis ")
         WHEN 5
            IF decenas = 1 THEN
               LET c_l = c_l.append("quince ")
            ELSE
               LET c_l = c_l.append("cinco ")
            END IF
         WHEN 4
            IF decenas = 1 THEN
               LET c_l = c_l.append("catorce ")
            ELSE
               LET c_l = c_l.append("cuatro ")
            END IF
         WHEN 3
            IF decenas = 1 THEN
               LET c_l = c_l.append("trece ")
            ELSE
               LET c_l = c_l.append("tres ")
            END IF
         WHEN 2
            IF decenas = 1 THEN
               LET c_l = c_l.append("doce ")
            ELSE
               LET c_l = c_l.append("dos ")
            END IF
         WHEN 1
            IF decenas = 1 THEN
               LET c_l = c_l.append("once ")
            ELSE
               IF grupo = 2 THEN
                  IF numero > 1 THEN
                     LET c_l = c_l.append("un ")
                  END IF
               ELSE
                  LET c_l = c_l.append("un ")
               END IF
            END IF
      END CASE
   END IF
   CASE grupo
      WHEN 3 
         IF numero > 1 THEN
            LET c_l = c_l.append("millones ")
         ELSE
            LET c_l = c_l.append("millón ")
         END IF
      WHEN 2
         LET c_l = c_l.append("mil ")
   END CASE
   --LET c_l = c_l.toUpperCase()
   RETURN c_l
END FUNCTION

PUBLIC FUNCTION fn_envia_pdf(v_ruta_reporte, p_correo, p_nss)
   DEFINE archivo           BYTE
   DEFINE p_nss             CHAR(11)
   DEFINE p_correo          STRING 
   DEFINE v_ruta_reporte    STRING 
   DEFINE v_comando         STRING 

   #DISPLAY v_ruta_reporte
   LET v_comando = "echo 'Se proceso el pago de la solicitud con el NSS:", p_nss, "' > body.txt |" 
   LET v_comando = v_comando CLIPPED, " mailx -s 'Excepciones Devolución SSV' -a ", v_ruta_reporte CLIPPED, " ", p_correo, " < body.txt"
   DISPLAY v_comando
   --RUN v_comando 
   
END FUNCTION


