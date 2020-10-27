###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGE10                                                  #
#Objetivo          => Programa que integra créditos liquidados de cartera     #
#                     e inicia credito43 bis                                  #
#Autor             => Jose Eduardo Ventura                                    #
#Fecha inicio      => 08 Enero 2016                                           #
###############################################################################
DATABASE safre_viv 

GLOBALS "../../cta/fte/CTAW15.inc"    #archivo de variables globales del WS de consulta de saldo

   DEFINE p_usuario           CHAR(20)
   DEFINE p_tipo_ejecucion    SMALLINT                      -- forma como ejecutara el programa
   DEFINE p_s_titulo          STRING                        -- título de la ventana
   DEFINE p_pid               DECIMAL (9,0)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE p_folio             LIKE glo_folio.folio                                         
   DEFINE p_nom_archivo       LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_arch_salida       base.Channel
   DEFINE v_nom_arch_rch      STRING
   DEFINE v_nom_arch_rch2     STRING
   DEFINE v_nom_arch_ace      STRING
   DEFINE v_nom_arch_ace2     STRING
   DEFINE v_cnt_rch_valida    INTEGER
   DEFINE v_cnt_rch_licuadora INTEGER
   DEFINE v_cnt_rch_nvo       INTEGER
   DEFINE v_incons_bd         INTEGER
   DEFINE v_tot_rch           INTEGER

   #Parámetros generales del proceso

   PRIVATE DEFINE p_usuario_cod              CHAR(20)            -- clave del usuario firmado

#Parámetros de conexión
   PRIVATE DEFINE v_url_servidor             LIKE wsv_cliente.ruta_servidor 
   PRIVATE DEFINE v_usuario                  LIKE wsv_cliente.usuario
   PRIVATE DEFINE v_password                 LIKE wsv_cliente.password
   PRIVATE DEFINE v_intentos                 LIKE wsv_cliente.num_reintento

MAIN

   DEFINE v_qry               STRING
   DEFINE v_cnt_tot           INTEGER
   DEFINE v_cnt_acep          INTEGER
   DEFINE r                   INTEGER
   DEFINE v_cnt_reciclaje     INTEGER
   DEFINE v_tot_procesar      INTEGER
   DEFINE v_f_liquidacion     CHAR(8)

   DEFINE arr_reciclaje DYNAMIC ARRAY OF RECORD
      nss                 CHAR(11),               
      num_credito         DECIMAL(10,0),
      estado_cta          CHAR(4),
      f_liquidacion       DATE,
      producto            CHAR(4),
      cve_ent_financiera  DECIMAL(4,0),
      marca_conyuge       CHAR (1),
      nss_conyuge         CHAR(15),
      num_credito_conyuge DECIMAL(10,0),
      producto_conyuge    CHAR(4),
      rfc_conyuge         CHAR(15)
   END RECORD

   -- se recupera la clave de usuario desde parametro
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nom_archivo    = ARG_VAL(6)

   DISPLAY "usuario   : ", p_usuario
   DISPLAY "tipo ejec : ", p_tipo_ejecucion
   DISPLAY "titulo    : ",p_s_titulo

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGE11.log")


   -- Se obtienen el total de registros nuevos provenientes de archivo
   LET v_qry = " SELECT COUNT(*)
                     FROM safre_tmp:tmp_cred_liq_cartera "

   PREPARE prp_cnt_tot FROM v_qry
   EXECUTE prp_cnt_tot INTO v_cnt_tot

  -- Se obtienen el total de registros en reciclaje a procesar nuevamente
   LET v_qry = " SELECT nss,
                        num_credito,
                        estado_cta,
                        f_liquidacion,
                        producto,
                        cve_ent_financiera,
                        marca_conyuge,
                        nss_conyuge,
                        num_credito_conyuge,
                        producto_conyuge,
                        rfc_conyuge
                   FROM ocg_liquidacion_cofi
                  WHERE id_ocg_liq_cofi in (SELECT id_ocg_liq_cofi
                                              from ocg_rechazo_cartera 
                                             WHERE rechazo_cod = 1)
                    AND estado    = 60
                    AND situacion = 190"

   PREPARE prp_reciclaje FROM v_qry
   DECLARE cur_reciclaje CURSOR FOR prp_reciclaje

   LET r = 1

   FOREACH cur_reciclaje INTO arr_reciclaje[r].*
      IF arr_reciclaje[arr_reciclaje.getLength()].nss IS NOT NULL THEN
         LET v_f_liquidacion = NULL
         LET v_f_liquidacion = arr_reciclaje[r].f_liquidacion USING "yyyymmdd"

         IF arr_reciclaje[r].producto = "8" THEN
            LET arr_reciclaje[r].producto = "IL4C"
         ELSE
            LET arr_reciclaje[r].producto = "IL3C"
         END IF

         --DATABASE safre_tmp
         INSERT INTO safre_tmp:tmp_cred_liq_cartera
              VALUES(0,
                     arr_reciclaje[r].nss,
                     arr_reciclaje[r].num_credito,
                     arr_reciclaje[r].estado_cta,
                     v_f_liquidacion,
                     arr_reciclaje[r].producto,
                     arr_reciclaje[r].cve_ent_financiera,
                     arr_reciclaje[r].marca_conyuge,
                     arr_reciclaje[r].nss_conyuge,
                     arr_reciclaje[r].num_credito_conyuge,
                     arr_reciclaje[r].producto_conyuge,
                     arr_reciclaje[r].rfc_conyuge)
         --DATABASE safre_viv
      END IF
      LET r = r+1
   END FOREACH

   IF arr_reciclaje[arr_reciclaje.getLength()].nss IS NULL THEN
      CALL arr_reciclaje.deleteElement(arr_reciclaje.getLength())
   END IF

   LET v_cnt_reciclaje = arr_reciclaje.getLength()

   CALL fn_ocg_cred_liq_cartera()

   UPDATE ocg_ctr_archivo
      SET tot_registros = v_cnt_tot
    WHERE nom_archivo = p_nom_archivo
      AND f_proceso = TODAY

-- se obtienen total de rechazos nuevos en archivo cargado
   LET v_qry = " SELECT COUNT(*)
                   FROM safre_tmp:tmp_cred_liq_rch
                  WHERE id_derechohabiente in (
                 SELECT id_ocg_liq_cofi
                   FROM ocg_liquidacion_cofi
                  WHERE diagnostico in (1,2)
                    AND estado      = 60
                    AND situacion   = 190
                    AND f_proceso   = TODAY)"

   PREPARE prp_cnt_rch FROM v_qry
   EXECUTE prp_cnt_rch INTO v_cnt_rch_nvo

-- se obtienen rechazos de validación por estructura
   LET v_qry = " SELECT COUNT(*)
                   FROM ocg_liq_cofi_rechazo
                  WHERE f_proceso = TODAY"

   PREPARE prp_cnt_val FROM v_qry
   EXECUTE prp_cnt_val INTO v_cnt_rch_valida

--se obtiene cantidad de aceptados

   LET v_qry = " SELECT COUNT(*)
                   FROM ocg_liquidacion_cofi
                  WHERE diagnostico = 1
                    AND situacion in (30,50)
                    AND f_proceso = TODAY"

   PREPARE prp_cnt_ace FROM v_qry
   EXECUTE prp_cnt_ace INTO v_cnt_acep


   --LET v_incons_bd = (v_cnt_tot - v_cnt_rch_nvo - v_cnt_rch_val - v_cnt_acep)
   LET v_tot_procesar = (v_cnt_tot + v_cnt_reciclaje)
   LET v_tot_rch      = ( v_cnt_rch_valida + v_cnt_rch_nvo)

   -- displays para log
   DISPLAY "TOTAL DE REGISTROS            :",v_cnt_tot
   DISPLAY ""
   DISPLAY "TOTAL DE RECHAZOS EN RECICLAJE:",v_cnt_reciclaje
   DISPLAY ""
   DISPLAY "TOTAL DE REGISTROS EN ARCHIVO :",v_cnt_tot
   DISPLAY ""
   DISPLAY "TOTAL DE REGISTROS A PROCESAR :",v_tot_procesar
   DISPLAY ""
   DISPLAY "TOTAL DE RECHAZOS OPERATIVOS  :",v_cnt_rch_nvo
   DISPLAY ""
   DISPLAY "TOTAL RECHAZOS DE VALIDACIÓN  :",v_cnt_rch_valida
   DISPLAY ""
   DISPLAY "TOTAL ACEPTADOS               :",v_cnt_acep
   DISPLAY ""
   DISPLAY "TOTAL RECHAZOS                :",v_tot_rch
   DISPLAY  ""
   DISPLAY "RUTA Y NOMBRE ARCHIVO DE RECHAZOS:"
   DISPLAY "",v_nom_arch_rch2
   DISPLAY ""
   DISPLAY "RUTA Y NOMBRE ARCHIVO DE ACEPTADOS:"
   DISPLAY "",v_nom_arch_ace2
   DISPLAY ""
   DISPLAY ""

END MAIN

FUNCTION fn_ocg_cred_liq_cartera()

   DEFINE v_qry_marca         STRING
   DEFINE bnd_cred_liq        SMALLINT
   DEFINE v_error             SMALLINT
   DEFINE v_cta_ws            INTEGER
   DEFINE a                   INTEGER
   DEFINE bnd_desmarca        SMALLINT
   DEFINE v_qry               STRING
   DEFINE r_b_valida          SMALLINT
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_detalle           STRING
   DEFINE v_s_comando         STRING

   DEFINE arr_marca DYNAMIC ARRAY OF RECORD
      nss                 CHAR(11),
      id_derechohabiente  DECIMAL(9,0),
      marca               SMALLINT,
      viv92               DECIMAL(12,2),
      viv87               DECIMAL(12,2)
   END RECORD

   DEFINE arr_rch DYNAMIC ARRAY OF RECORD
      id_derechohabiente   decimal(9,0), 
      nss                  char(11), 
      num_credito          CHAR(10),
      estado_cta           char(4),
      producto             char(1),     
      cve_ent_financiera   CHAR(3),     
      nss_conyuge          char(11),    
      num_credito_conyuge  CHAR(10),
      producto_conyuge     char(1),
      diagnostico          CHAR(2),
      paterno              CHAR(40),
      materno              CHAR(40),
      nombre               CHAR(40),
      rechazo_cod          CHAR(3),
      rechazo_desc         CHAR(50),
      f_proceso            DATE
   END RECORD

    DEFINE arr_aceptados DYNAMIC ARRAY OF RECORD
      id_derechohabiente   decimal(9,0), 
      nss                  char(11), 
      cve_ent_financiera   CHAR(3),
      paterno              CHAR(40),
      materno              CHAR(40),
      nombre               CHAR(40),
      f_liquidacion        DATE,    
      diagnostico          CHAR(2),
      rechazo_cod          CHAR(3),
      rechazo_desc         CHAR(50),
      f_proceso            DATE
   END RECORD

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   CALL fn_tablas_temporales()
   LET bnd_desmarca = ""
   
   LET v_qry = "EXECUTE FUNCTION fn_ocg_cred_liq_cartera(?,?)"

   PREPARE prp_ocg_cred_liq FROM v_qry
   EXECUTE prp_ocg_cred_liq USING p_usuario,
                                  p_nom_archivo 
                             INTO v_error

   DISPLAY v_error

   SELECT COUNT(*)
     INTO v_cta_ws
     FROM safre_tmp:tmp_nss_cred_liq

   LET v_qry = " SELECT COUNT(*)
                     FROM safre_tmp:tmp_nss_cred_liq "

   PREPARE prp_cnt_ws FROM v_qry
   EXECUTE prp_cnt_ws INTO v_cta_ws

  -- DISPLAY "se ejecuta WS "

  IF v_cta_ws > 0 THEN
      --CALL fn_configura_ws()
     -- CALL fn_recupera_marca()

   {   LET v_qry = "SELECT * FROM afre_tmp:tmp_ws_cred_liq"
      PREPARE prp_marca FROM v_qry
      DECLARE cur_marca CURSOR FOR prp_marca

      LET a = 1

      FOREACH cur_marca INTO arr_marca[a].*

         LET v_qry_marca= "EXECUTE FUNCTION fn_ocg_procesa_desmarca(?,?,?)"

         PREPARE prp_desmarca_cuenta FROM v_qry_marca

         IF (arr_marca[a].marca = 01) OR
            (arr_marca[a].marca = 04) THEN
            LET bnd_desmarca = 1
            --se ejecuta función con bandera de solicitud de desmarca a procesar prendida
            EXECUTE prp_desmarca_cuenta USING arr_marca[a].id_derechohabiente,
                                              bnd_desmarca,
                                              p_usuario
                                         INTO v_error
         END IF

         IF (arr_marca[a].marca = "") OR
            (arr_marca[a].marca = 02) THEN
            LET bnd_desmarca = 0
             --se ejecuta función con bandera de solicitud de desmarca a procesar apagada
            EXECUTE prp_desmarca_cuenta USING arr_marca[a].id_derechohabiente,
                                              bnd_desmarca,
                                              p_usuario
                                         INTO v_error
            --se actualiza edo procesar a 210
         END IF
         
            LET a=a+1

      END FOREACH}
   END IF

   LET v_qry = " SELECT *
                   FROM safre_tmp:tmp_cred_liq_rch "

   PREPARE prp_rch_tmp FROM v_qry
   DECLARE cur_rch_tmp CURSOR FOR prp_rch_tmp

   LET a = 1

   FOREACH cur_rch_tmp INTO arr_rch[a].id_derechohabiente,
                            arr_rch[a].nss,
                            arr_rch[a].num_credito,
                            arr_rch[a].estado_cta,
                            arr_rch[a].producto,
                            arr_rch[a].cve_ent_financiera,
                            arr_rch[a].nss_conyuge,
                            arr_rch[a].num_credito_conyuge,
                            arr_rch[a].producto_conyuge,
                            arr_rch[a].diagnostico
      SELECT ap_paterno_af,
             ap_materno_af,
             nombre_af
        INTO arr_rch[a].paterno,
             arr_rch[a].materno,
             arr_rch[a].nombre
        FROM afi_derechohabiente
       WHERE nss = arr_rch[a].nss

      SELECT ocg.rechazo_cod,
             cat.rechazo_desc
        INTO arr_rch[a].rechazo_cod,
             arr_rch[a].rechazo_desc
        FROM ocg_rechazo_cartera ocg, cat_rechazo_cartera cat
       WHERE ocg.id_ocg_liq_cofi = arr_rch[a].id_derechohabiente
         AND ocg.rechazo_cod = cat.rechazo_cod
         AND ocg.f_proceso = TODAY

      LET arr_rch[a].f_proceso = TODAY
      --DISPLAY "arr 1",arr_rch[a].*
      LET a = a+1
   END FOREACH

   LET v_qry = " SELECT a.id_derechohabiente,
                        a.nss,
                        a.num_credito,
                        a.estado_cta,
                        a.producto,
                        a.cve_ent_financiera,
                        a.nss_conyuge,
                        a.num_credito_conyuge,
                        a.producto_conyuge,
                        a.diagnostico,
                        afi.ap_paterno_af,
                        afi.ap_materno_af,
                        afi.nombre_af,
                        c.rechazo_cod,
                        d.rechazo_desc,
                        a.f_proceso
                   FROM ocg_liquidacion_cofi a,
                        afi_derechohabiente afi,
                        ocg_rechazo_cartera c,
                        cat_rechazo_cartera d
                  WHERE a.diagnostico in (2,1)
                    AND a.estado      = 60
                    AND a.situacion   = 190
                    AND a.id_ocg_liq_cofi = c.id_ocg_liq_cofi
                    AND c.rechazo_cod = d.rechazo_cod
                    AND afi.id_derechohabiente = a.id_derechohabiente
                    AND a.id_ocg_liq_cofi not in (SELECT id_derechohabiente
                                                       FROM safre_tmp:tmp_cred_liq_rch)
                    AND a.f_proceso = TODAY"

   PREPARE prp_rch_ocg FROM v_qry
   DECLARE cur_rch_ocg CURSOR FOR prp_rch_ocg

   FOREACH cur_rch_ocg INTO arr_rch[a].*
      --DISPLAY "arr 2",arr_rch[a].*
      LET a = a+1
   END FOREACH

   -- se obtienen rechazos de validación por estructura
   LET v_qry = " SELECT '',
                        a.nss,
                        a.num_credito,
                        a.estado_cta,
                        a.producto,
                        a.cve_ent_financiera,
                        a.nss_conyuge,
                        a.num_credito_conyuge,
                        a.producto_conyuge,
                        '2',
                        '',
                        '',
                        '',
                        '',
                        'ERROR DE VALIDACIÓN',
                        a.f_proceso
                   FROM ocg_liq_cofi_rechazo a
                  WHERE f_proceso = TODAY"

   PREPARE prp_rch_val FROM v_qry
   DECLARE cur_rch_val CURSOR FOR prp_rch_val

   FOREACH cur_rch_val INTO arr_rch[a].*
      LET a = a + 1
   END FOREACH

   IF arr_rch[arr_rch.getLength()].nss IS NULL THEN
      CALL arr_rch.deleteElement(arr_rch.getLength())
   END IF

   LET v_nom_arch_rch  = v_ruta_envio CLIPPED,"/rch_cartera"   CLIPPED,TODAY USING "DDMMYYYY" CLIPPED,".clc"
   LET v_nom_arch_rch2 = v_ruta_envio CLIPPED,"/rch_cartera"   CLIPPED,".clc"
   LET v_nom_arch_ace  = v_ruta_envio CLIPPED,"/acept_cartera" CLIPPED,TODAY USING "DDMMYYYY" CLIPPED,".clc"
   LET v_nom_arch_ace2 = v_ruta_envio CLIPPED,"/acept_cartera" CLIPPED,".clc"

   LET v_arch_salida = base.Channel.create()
   CALL v_arch_salida.openFile(v_nom_arch_rch,"w" )
  -- CALL v_arch_salida.setDelimiter("|")

   --DISPLAY "punto de control : ",arr_rch.getLength()

   IF arr_rch.getLength() >= 1 THEN
      FOR a = 1 TO arr_rch.getLength()
   
         LET v_detalle = arr_rch[a].nss                        CLIPPED,"¿",
                         arr_rch[a].num_credito                CLIPPED,"¿",
                         arr_rch[a].estado_cta                 CLIPPED,"¿",
                         arr_rch[a].producto                   CLIPPED,"¿",
                         arr_rch[a].cve_ent_financiera         CLIPPED,"¿",  
                         arr_rch[a].nss_conyuge                CLIPPED,"¿",
                         arr_rch[a].producto_conyuge           CLIPPED,"¿",
                         arr_rch[a].diagnostico                CLIPPED,"¿",
                         arr_rch[a].rechazo_cod                CLIPPED,"¿",
                         arr_rch[a].rechazo_desc               CLIPPED,"¿",
                         arr_rch[a].paterno                    CLIPPED," ",
                         arr_rch[a].materno                    CLIPPED," ",
                         arr_rch[a].nombre                     CLIPPED,"¿",
                         arr_rch[a].f_proceso USING "YYYYMMDD" CLIPPED

            --DISPLAY v_detalle
            CALL v_arch_salida.write([v_detalle])
           -- DISPLAY "detalle : ",v_detalle
      END FOR
   END IF
   CALL v_arch_salida.close()

   --   se crea comando que elimina diagonales
      LET v_s_comando = "sed 's/¿/|/g' ",v_nom_arch_rch," > ",v_nom_arch_rch2
      RUN v_s_comando

   LET v_arch_salida = base.Channel.create()
   CALL v_arch_salida.openFile(v_nom_arch_ace,"w" )
  -- CALL v_arch_salida.setDelimiter("|")

   LET v_qry = " SELECT a.id_derechohabiente,
                        a.nss,
                        a.cve_ent_financiera,
                        afi.ap_paterno_af,
                        afi.ap_materno_af,
                        afi.nombre_af,
                        a.f_liquidacion,
                        a.diagnostico,
                        c.rechazo_cod,
                        d.rechazo_desc,
                        a.f_proceso
                   FROM ocg_liquidacion_cofi a,
                        afi_derechohabiente afi,
                        ocg_rechazo_cartera c,
                        cat_rechazo_cartera d
                  WHERE a.diagnostico = 1
                    AND a.estado      = 70
                    AND a.situacion   = 30
                    AND a.id_ocg_liq_cofi = c.id_ocg_liq_cofi
                    AND c.rechazo_cod = d.rechazo_cod
                    AND a.f_proceso   = TODAY
                    AND afi.id_derechohabiente = a.id_derechohabiente"

   PREPARE prp_aceptados_rch FROM v_qry
   DECLARE cur_aceptados_rch CURSOR FOR prp_aceptados_rch

   LET a = 1

   FOREACH cur_aceptados_rch INTO arr_aceptados[a].*
      UPDATE ocg_liquidacion_cofi
         SET situacion = 50
       WHERE id_derechohabiente = arr_aceptados[a].id_derechohabiente
         AND diagnostico = 1
         AND situacion = 30
      --DISPLAY "arr 2",arr_rch[a].*
      LET a = a+1
   END FOREACH

   IF arr_aceptados[arr_aceptados.getLength()].nss IS NULL THEN
      CALL arr_aceptados.deleteElement(arr_aceptados.getLength())
   END IF

   LET v_qry = " SELECT a.id_derechohabiente,
                        a.nss,
                        a.cve_ent_financiera,
                        afi.ap_paterno_af,
                        afi.ap_materno_af,
                        afi.nombre_af,
                        a.f_liquidacion,
                        a.diagnostico,
                        '',
                        '',
                        a.f_proceso
                   FROM ocg_liquidacion_cofi a,
                        afi_derechohabiente afi
                  WHERE a.diagnostico = 1
                    AND a.estado      = 70
                    AND a.situacion   = 30
                    AND f_proceso     = TODAY
                    AND afi.id_derechohabiente = a.id_derechohabiente"

   PREPARE prp_aceptados_ocg FROM v_qry
   DECLARE cur_aceptados_ocg CURSOR FOR prp_aceptados_ocg

   --LET a = 1

   FOREACH cur_aceptados_ocg INTO arr_aceptados[a].*
      UPDATE ocg_liquidacion_cofi
         SET situacion = 50
       WHERE id_derechohabiente = arr_aceptados[a].id_derechohabiente
         AND diagnostico = 1
         AND situacion = 30
      --DISPLAY "arr 2",arr_rch[a].*
      LET a = a+1
   END FOREACH

   IF arr_aceptados[arr_aceptados.getLength()].nss IS NULL THEN
      CALL arr_aceptados.deleteElement(arr_aceptados.getLength())
   END IF

   IF arr_aceptados.getLength() >= 1 THEN

      FOR a = 1 TO arr_aceptados.getLength()
         LET arr_aceptados[a].f_proceso = arr_aceptados[a].f_proceso USING "YYYYMMDD"

         LET v_detalle = arr_aceptados[a].nss                            CLIPPED,"¿",
                         arr_aceptados[a].cve_ent_financiera             CLIPPED,"¿",  
                         arr_aceptados[a].paterno                        CLIPPED," ",
                         arr_aceptados[a].materno                        CLIPPED," ",
                         arr_aceptados[a].nombre                         CLIPPED,"¿",
                         --arr_aceptados[a].f_publicacion      CLIPPED,"¿",
                         arr_aceptados[a].f_liquidacion USING "yyyymmdd" CLIPPED,"¿",
                         arr_aceptados[a].diagnostico                    CLIPPED,"¿",
                         arr_aceptados[a].rechazo_cod                    CLIPPED,"¿",
                         arr_aceptados[a].rechazo_desc                   CLIPPED,"¿",
                         arr_aceptados[a].f_proceso   USING"yyyymmdd"    CLIPPED
         CALL v_arch_salida.write([v_detalle])
      END FOR

   END IF
   CALL v_arch_salida.close()

   --   se crea comando que elimina diagonales
      LET v_s_comando = "sed 's/¿/|/g' ",v_nom_arch_ace," > ",v_nom_arch_ace2
      RUN v_s_comando

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
   END IF
END FUNCTION


PRIVATE FUNCTION fn_configura_ws()

   DEFINE v_consulta    STRING
   DEFINE cve_cliente   CHAR(10)

   #La clave 'cre_1' del catálogo de clientes de webServices corresponde a la solicitud de marca
   LET v_consulta = "SELECT   ruta_servidor, 
                              usuario, 
                              password, 
                              num_reintento 
                     FROM     wsv_cliente 
                     WHERE    cve_cliente = ?"

   PREPARE exe_consulta FROM v_consulta

   LET cve_cliente = "cre_3"

   EXECUTE exe_consulta USING cve_cliente INTO v_url_servidor,
                                            v_usuario,
                                            v_password,
                                            v_intentos

END FUNCTION

FUNCTION fn_recupera_marca()

   DEFINE soapStatus            INTEGER
   DEFINE v_qry                 STRING
   DEFINE a                     INTEGER

   DEFINE arr_verifica DYNAMIC ARRAY OF RECORD
          nss CHAR (11),
          id_derechohabiente decimal(9,0),
          id_cre_acreditado   DECIMAL (9,0),
          paterno CHAR(40),
          materno CHAR(40),
          nombre CHAR(40)
   END RECORD

   DEFINE v_aivs97             DECIMAL(18,6)
   DEFINE v_aivs92             DECIMAL(18,6)
   DEFINE v_marca              SMALLINT
   DEFINE v_cadena             STRING
   DEFINE v_arch_salida        STRING
   DEFINE v_ruta_envio         LIKE seg_modulo.ruta_envio

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "cta"

   LET v_qry = "SELECT afi.nss,
                       afi.id_derechohabiente,
                       a.id_cre_acreditado,
                       afi.ap_paterno_af,
                       afi.ap_materno_af,
                       afi.nombre_af
                  FROM afi_derechohabiente afi, safre_tmp:tmp_nss_cred_liq a
                 WHERE a.nss = afi.nss"

   PREPARE prp_verifica FROM v_qry
   DECLARE cur_verifica CURSOR FOR prp_verifica

   LET a = 1

   FOREACH cur_verifica INTO arr_verifica[a].*
      LET a=a+1
   END FOREACH

   --CALL fn_mensaje ("Transferencia Archivo","punto de control 1","information")

   IF arr_verifica[arr_verifica.getLength()].id_derechohabiente IS NULL THEN
      CALL arr_verifica.deleteElement(arr_verifica.getLength())
   END IF

   FOR a = 1 TO arr_verifica.getLength()
   --CALL fn_mensaje ("Transferencia Archivo","punto de control 2","information")

      -- Se invoca a la función que ejecuta el web service
      CALL consultaSaldo(v_url_servidor CLIPPED,
                         v_usuario,
                         v_password,
                         arr_verifica[a].materno CLIPPED,
                         arr_verifica[a].paterno CLIPPED,
                         arr_verifica[a].nombre CLIPPED,
                         arr_verifica[a].nss)
               RETURNING soapStatus,
                         ConsultaSaldoRespVO.apeMaternoBD,
                         ConsultaSaldoRespVO.apePaternoBD,
                         ConsultaSaldoRespVO.diagProceso,
                         ConsultaSaldoRespVO.nombresBD,
                         ConsultaSaldoRespVO.nss,
                         ConsultaSaldoRespVO.numAIVS92,
                         ConsultaSaldoRespVO.numAIVS97,
                         ConsultaSaldoRespVO.origenTipoCredito,
                         ConsultaSaldoRespVO.resultOperacion,
                         ConsultaSaldoRespVO.tramiteJudicial
-- Si no hay ningún error, se despliegan los datos obtenidos del WS en la forma
      IF soapStatus = 0 THEN
         LET v_marca  = ConsultaSaldoRespVO.origenTipoCredito
         LET v_aivs92 = ConsultaSaldoRespVO.numAIVS92
         LET v_aivs97 = ConsultaSaldoRespVO.numAIVS97

         INSERT INTO  safre_tmp:tmp_ws_cred_liq VALUES (arr_verifica[a].nss,
                                                        arr_verifica[a].id_derechohabiente,
                                                        arr_verifica[a].id_cre_acreditado,
                                                        v_marca,
                                                        v_aivs92,
                                                        v_aivs97)
      END IF
   END FOR
{
   LET v_arch_salida = v_ruta_envio CLIPPED,"/marca_procesar.unl"
   LET v_cadena = "Puede veriricar el archivo en \n",v_arch_salida
   UNLOAD TO v_arch_salida SELECT * FROM safre_tmp:tmp_ws_cred_liq
   CALL fn_mensaje ("Transferencia Archivo",v_cadena,"information")}

END FUNCTION

FUNCTION fn_tablas_temporales()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_nss_cred_liq
      DROP TABLE tmp_ws_cred_liq
      DROP TABLE tmp_cred_liq_rch

   WHENEVER ERROR STOP

  --  se crea tabla temporal para guardar registros de cifras control
   CREATE TABLE tmp_nss_cred_liq ( id_cre_acreditado DECIMAL(9,0),
                                   nss CHAR(11))

   CREATE TABLE tmp_cred_liq_rch ( id_derechohabiente  DECIMAL(9,0),
                                   nss                 CHAR(11),
                                   num_credito         DECIMAL(10,0),
                                   estado_cta          CHAR(4),
                                   producto            CHAR(1),
                                   cve_ent_financiera  SMALLINT,
                                   nss_conyuge         CHAR(11),
                                   num_credito_conyuge DECIMAL (10,0),
                                   producto_conyuge    CHAR(1),
                                   diagnostico         SMALLINT)

   CREATE TABLE tmp_ws_cred_liq ( nss CHAR(11),
                                 id_derechohabiente DECIMAL(9,0),
                                 marca SMALLINT,
                                 aivs92 DECIMAL(12,2),
                                 aivs97 DECIMAL(12,2))
DATABASE safre_viv

END FUNCTION
