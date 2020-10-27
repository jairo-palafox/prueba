##################################################################################################
#Modulo       => AGR                                                                             #
#Programa     => AGRP33                                                                          #
#Objetivo     => Consulta de Movimientos de Cuenta Individual                                    #
#Autor        => Jose Eduardo Ventura Bonola                                                     #
#Fecha inicio => 07/ABRIL/2015                                                                   #
##################################################################################################

DATABASE safre_viv

GLOBALS
   DEFINE v_pid                    DECIMAL(9,0)
   DEFINE g_proceso_cod            INTEGER
   DEFINE g_opera_cod              INTEGER
   DEFINE g_usuario                CHAR(20)
   DEFINE v_estado                 SMALLINT
   DEFINE v_hora                   DATETIME HOUR TO SECOND
   DEFINE v_entrada                STRING
   DEFINE v_nss                    CHAR(11)
   DEFINE v_archivo                STRING
--***************************************************************
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate
--***************************************************************
   DEFINE r_bandera                SMALLINT
   DEFINE v_extension              STRING
   DEFINE v_nom_arch               STRING
   DEFINE v_arch_mov               STRING
   DEFINE i                        SMALLINT
   DEFINE v_consulta               CHAR (116)
   DEFINE v_detalle                STRING
   DEFINE v_arch_salida            base.Channel
   DEFINE v_arch_salida_mov        base.Channel
   DEFINE v_valida_nss             INTEGER
   DEFINE v_dtos_arh               INTEGER
   DEFINE v_qry_rechazos           STRING
   DEFINE r_folio                  CHAR(10)
--***************************************************************
   DEFINE r_reporte_rechazos        RECORD
         nss                       CHAR(11)
   END RECORD
END GLOBALS

MAIN

-- parametros que vienen de lanzador
   LET g_usuario       = ARG_VAL (1)
   LET v_pid           = ARG_VAL (2)
   LET g_proceso_cod   = ARG_VAL (3)
   LET g_opera_cod     = ARG_VAL (4)
   LET r_bandera       = ARG_VAL (5)
   LET v_entrada       = ARG_VAL (6)

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP33.log")

   -- se obtienen rutas
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

--**************************
--Condición para ejecución *
--**************************
   LET v_estado = 0

   IF v_estado = 0 THEN

      LET v_hora = CURRENT

      DISPLAY ""
      DISPLAY "INICIÓ ETAPA  PARA CONSULTA DE MOVIMIENTOS DE CUENTA INDIVIDUAL"
      DISPLAY "FECHA :  ",TODAY USING "DD/MM/YYYY"
      DISPLAY "HORA  :  ",v_hora
      DISPLAY ""
      DISPLAY ""

      CALL fn_tablas_temporales()

        CALL fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario) RETURNING r_folio

      LET v_nom_arch = v_ruta_envio CLIPPED,"/det_credito" CLIPPED,TODAY USING "DDMMYYYY" CLIPPED,"_",r_folio CLIPPED,".cmcip"
      LET v_arch_mov = v_ruta_envio CLIPPED ,"/det_movimientos",TODAY USING "DDMMYYYY" CLIPPED,"_",r_folio CLIPPED,".cmcip"

      IF r_bandera = 1 THEN
         LET v_nss = v_entrada

         SELECT COUNT (*)
           INTO v_valida_nss
           FROM afi_derechohabiente afi
          WHERE afi.nss = v_nss

         IF v_valida_nss >= 1 THEN
            DELETE FROM safre_tmp:tmp_nss_mci
            INSERT INTO safre_tmp:tmp_nss_mci VALUES (v_nss)
            CALL fn_consulta_vector()
            DISPLAY ""
            DISPLAY ""
         ELSE
             DISPLAY "No se encontraron registros relacionados a NSS ingresado"
         END IF
      END IF

      IF r_bandera = 2 THEN
         LET v_archivo = v_entrada

         DELETE FROM safre_tmp:tmp_nss_mci
         CALL fn_busca_datos(v_archivo)
         DISPLAY ""
         DISPLAY ""
         DISPLAY ""
      END IF

      DISPLAY ""
      DISPLAY "PROCESO EJECUTADO CORRECTAMENTE..."
      DISPLAY ""

      CALL fn_actualiza_opera_fin(v_pid,
                                  g_proceso_cod,
                                  g_opera_cod)
                        RETURNING v_estado
   ELSE
--Si ocurrió un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF

END MAIN

--**********************************************************************
-- Función que extrae datos de archivo para dejarlos en tabla temporal *
--**********************************************************************
FUNCTION fn_busca_datos(v_archivo)

   DEFINE s                        CHAR (21)  -- variable para leer líneas del archivo
   DEFINE ch                       base.Channel 
   DEFINE tok                      base.StringTokenizer
   DEFINE buf                      base.StringBuffer
   DEFINE cadena                   CHAR (21)  -- variable para rescatar líneas del archivo
   DEFINE v_ruta_carga             STRING
   DEFINE v_archivo                STRING

   LET v_ruta_carga = v_archivo
   LET ch = base.Channel.create()
   CALL ch.openFile(v_ruta_carga,"r")
   LET buf = base.StringBuffer.create()

   LET i = 1
   WHILE TRUE
      LET s = ch.readLine()
      LET tok = base.StringTokenizer.create(s," ")
      WHILE tok.hasMoreTokens()
         LET cadena = tok.nextToken()

         INSERT INTO safre_tmp:tmp_nss_mci_general VALUES (cadena)
      END WHILE

      IF ch.isEof() THEN EXIT WHILE
      END IF
         LET i = i + 1
   END WHILE
   CALL ch.close()

   SELECT COUNT(*)
     INTO v_dtos_arh
     FROM safre_tmp:tmp_nss_mci_general

   IF v_dtos_arh >= 1 THEN
      CALL fn_consulta_vector()
   ELSE
      DISPLAY "EL ARCHIVO NO CONTIENE NINGÚN REGISTRO VÁLIDO"
   END IF

END FUNCTION

FUNCTION fn_consulta_vector()

   DEFINE v_nss                    CHAR(11)
   DEFINE a                        SMALLINT
   DEFINE v_query_vector           STRING
   DEFINE v_s_comando              STRING
   DEFINE v_nom_arch1              STRING
   DEFINE v_arch_mov1              STRING

   LET v_arch_salida = base.Channel.create()
   CALL v_arch_salida.openFile(v_nom_arch,"w" )
   CALL v_arch_salida.setDelimiter("|")

   LET v_arch_salida_mov = base.Channel.create()
   CALL v_arch_salida_mov.openFile(v_arch_mov,"w" )
   CALL v_arch_salida_mov.setDelimiter("|")

   INSERT INTO safre_tmp:tmp_nss_mci
   SELECT t.nss
     FROM afi_derechohabiente afi,
          safre_tmp:tmp_nss_mci_general t
    WHERE t.nss = afi.nss

   LET v_query_vector = "SELECT nss
                           FROM safre_tmp:tmp_nss_mci"

   PREPARE prp_vector FROM v_query_vector
   DECLARE cur_vector CURSOR FOR prp_vector

   LET a = 1
   FOREACH cur_vector INTO v_nss
      --DISPLAY "nss :",v_nss
      CALL fn_nss(v_nss)
      CALL fn_det_movimientos(v_nss)
      LET a = a+1
   END FOREACH

   CALL v_arch_salida.close()
   CALL v_arch_salida_mov.close()

   LET v_nom_arch1 = v_ruta_envio CLIPPED,"/det_credito.cmcip"
   LET v_arch_mov1 = v_ruta_envio CLIPPED ,"/det_movimientos.cmcip"

   -- se crea comando que elimina el delimitador
   LET v_s_comando = "sed 's/|//g' ",v_nom_arch," > ",v_nom_arch1
   RUN v_s_comando

   LET v_s_comando = "sed 's/|//g' ",v_arch_mov," > ",v_arch_mov1
   RUN v_s_comando

   LET v_s_comando = "mv ",v_nom_arch1," ",v_nom_arch
   RUN v_s_comando

   LET v_s_comando = "mv ",v_arch_mov1," ",v_arch_mov
   RUN v_s_comando
   
   DISPLAY "Nombre de archivo para detalle de crédito"
   DISPLAY "det_credito.cmcip"
   DISPLAY ""
   DISPLAY ""
   DISPLAY "Nombre de archivo para detalle de movimientos"
   DISPLAY "det_movimientos.cmcip"
   DISPLAY ""
   DISPLAY ""
   DISPLAY "Ruta de archivo para detalle de crédito"
   DISPLAY v_nom_arch
   DISPLAY ""
   DISPLAY ""
   DISPLAY "Ruta de archivo para detalle de movimientos"
   DISPLAY v_arch_mov

   CALL fn_genera_reporte()

END FUNCTION


FUNCTION fn_nss(v_nss)

   DEFINE v_nss                    CHAR(11)
   DEFINE v_id_derechohabiente     DECIMAL(9,0)
   DEFINE tpo_registro             INTEGER
   DEFINE v_precio_fondo           DECIMAL(19,14)
   DEFINE v_qry_cmci               STRING
   DEFINE i                        INTEGER
   DEFINE v_fondo72                DECIMAL(12,2)
   DEFINE v_acciones92             DECIMAL(12,2)
   DEFINE v_pesos92                DECIMAL(12,2)
   DEFINE v_pesos97                DECIMAL(12,2)
   DEFINE v_acciones97             DECIMAL(12,2)
   DEFINE v_cta_estado             INTEGER
   DEFINE v_edo_credito            SMALLINT
   DEFINE v_qry_fecha              STRING

   DEFINE arr_cmci DYNAMIC ARRAY OF RECORD
          tpo_registro CHAR(1),
          id_cre_acreditado INTEGER,
          nss          CHAR (11),
          tpo_credito  CHAR(3),
          num_credito  CHAR(10),
          f_otorga     DATE,
          f_liquida    DATE,
          entidad      SMALLINT
   END RECORD

      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_nss

      LET tpo_registro = 1

      LET v_qry_cmci = "SELECT ",tpo_registro,"
                                ,c.id_cre_acreditado,a.nss,
                                c.tpo_credito,
                                c.num_credito,
                                c.f_otorga,
                                e.entidad
                           FROM afi_derechohabiente a,
                                cre_acreditado c,
                          OUTER cat_maq_credito e
                          WHERE c.id_derechohabiente = a.id_derechohabiente
                            AND a.id_derechohabiente =",v_id_derechohabiente,
                          " AND c.estado = e.estado "

      LET i = 1
      PREPARE prp_cmci FROM v_qry_cmci
      DECLARE cur_cmci CURSOR FOR prp_cmci

      CALL arr_cmci.clear()
      FOREACH cur_cmci INTO arr_cmci[i].tpo_registro,arr_cmci[i].id_cre_acreditado,arr_cmci[i].nss,
                            arr_cmci[i].tpo_credito,arr_cmci[i].num_credito,
                            arr_cmci[i].f_otorga, arr_cmci[i].entidad

      IF arr_cmci[i].entidad = 1 THEN
         LET arr_cmci[i].f_liquida = '0000/00/00'
      ELSE
         SELECT COUNT (*)
           INTO v_cta_estado
           FROM cre_acreditado c,cat_maq_credito q
          WHERE id_derechohabiente = v_id_derechohabiente
            AND c.id_cre_acreditado = arr_cmci[i].id_cre_acreditado
            AND c.estado = q.estado
            AND q.entidad = 2

         IF v_cta_estado > 0 THEN

            LET v_qry_fecha ='SELECT FIRST 1 (f_actualiza)
                                FROM cta_his_credito his,cre_acreditado cre
                               WHERE cre.id_derechohabiente = ?
                                 AND his.id_derechohabiente = cre.id_derechohabiente
                                 AND cre.id_cre_acreditado = ?
                                 AND his.tpo_credito = ?
                                 AND his.num_credito = ?'

            PREPARE prp_qry_fecha FROM v_qry_fecha
            EXECUTE prp_qry_fecha USING v_id_derechohabiente,
                                        arr_cmci[i].id_cre_acreditado,
                                        arr_cmci[i].tpo_credito,
                                        arr_cmci[i].num_credito
                                  INTO arr_cmci[i].f_liquida
                                        
                                        

            IF ((arr_cmci[i].f_liquida IS NULL) OR
               (arr_cmci[i].f_liquida = ' ') OR 
               (arr_cmci[i].f_liquida ='')) THEN

               SELECT MIN(his.f_proceso)
                 INTO arr_cmci[i].f_liquida
                 FROM cre_his_acreditado his, cat_maq_credito ent
                WHERE his.id_cre_acreditado = arr_cmci[i].id_cre_acreditado
                  AND his.estado = ent.estado
                  AND ent.entidad = 2

               IF (arr_cmci[i].f_liquida IS NULL) OR
               (arr_cmci[i].f_liquida = ' ') OR
               (arr_cmci[i].f_liquida ='') THEN

               SELECT MAX(f_fin)
                 INTO arr_cmci[i].f_liquida
                 FROM sfr_marca_historica mrh,
                      cat_tipo_credito ctc
                WHERE mrh.id_derechohabiente = v_id_derechohabiente
                  AND mrh.marca =ctc.marca_inf

         IF (arr_cmci[i].f_liquida IS NULL) OR
               (arr_cmci[i].f_liquida = ' ') OR 
               (arr_cmci[i].f_liquida ='') THEN
            LET arr_cmci[i].f_liquida = TODAY

         END IF

               END IF
            END IF
         END IF

         IF (arr_cmci[i].f_liquida IS NULL) OR
               (arr_cmci[i].f_liquida = ' ') OR 
               (arr_cmci[i].f_liquida ='') THEN
            LET arr_cmci[i].f_liquida = TODAY

         END IF

      END IF
      LET i = i + 1
   END FOREACH

   IF arr_cmci[arr_cmci.getLength()].nss IS NULL THEN
      CALL arr_cmci.deleteElement(arr_cmci.getLength())
   END IF

      IF arr_cmci.getLength() = 0 THEN
         CALL arr_cmci.appendElement()
         LET arr_cmci[1].tpo_registro = 1
         LET arr_cmci[1].nss          = v_nss 
         LET arr_cmci[1].tpo_credito  = 0
         LET arr_cmci[1].num_credito  = 0
         LET arr_cmci[1].f_otorga     = '0000/00/00'
         LET arr_cmci[1].f_liquida    = '0000/00/00'
      END IF

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY

   SELECT SUM(monto_acciones),
          ((SUM(monto_acciones))*v_precio_fondo)
     INTO v_acciones97, v_pesos97
     FROM cta_movimiento
    WHERE id_derechohabiente =v_id_derechohabiente
      AND subcuenta in(4,44)
      AND fondo_inversion = 11

   SELECT SUM(monto_acciones),
          ((SUM(monto_acciones))*v_precio_fondo)
     INTO v_acciones92, v_pesos92
     FROM cta_movimiento
    WHERE id_derechohabiente =v_id_derechohabiente
      AND subcuenta in (8,42)
      AND fondo_inversion = 11

   SELECT SUM(importe)
     INTO v_fondo72
     FROM cta_fondo72
    WHERE id_afi_fondo72 IN (select id_afi_fondo72 from afi_fondo72
    WHERE id_derechohabiente =v_id_derechohabiente)

   IF v_acciones92 IS NULL THEN
      LET  v_acciones92 = 0
   END IF
   IF v_pesos92 IS NULL THEN
      LET v_pesos92 = 0
   END IF
   IF v_acciones97 IS NULL THEN
      LET v_acciones97 = 0
   END IF
   IF v_pesos97 IS NULL THEN
      LET v_pesos97 = 0
   END IF
   IF v_fondo72 IS NULL THEN
      LET v_fondo72 = 0
   END IF

   INSERT INTO safre_tmp:tmp_sdos VALUES (v_nss,v_pesos92,v_pesos97)

   FOR i = 1 TO arr_cmci.getLength()
      LET v_detalle = arr_cmci[i].tpo_registro,
                      arr_cmci[i].nss,
                      arr_cmci[i].tpo_credito    USING "&&&",
                      arr_cmci[i].num_credito    USING "&&&&&&&&&&",
                      arr_cmci[i].f_otorga       USING "DDMMYYYY",
                      arr_cmci[i].f_liquida      USING "DDMMYYYY",
                      (v_acciones97*100)         USING "-&&&&&&&&&&&&&&",
                      (v_pesos97*100)            USING "-&&&&&&&&&&&&&&",
                      (v_acciones92*100)         USING "-&&&&&&&&&&&&&&",
                      (v_pesos92*100)            USING "-&&&&&&&&&&&&&&",
                      (v_fondo72*100)            USING "-&&&&&&&&&&&&&&"
         CALL v_arch_salida.write([v_detalle])
   END FOR
END FUNCTION

FUNCTION fn_det_movimientos(v_nss)

   DEFINE v_qry_tab   STRING
   DEFINE arr_tab_mov DYNAMIC ARRAY OF RECORD
          v_nom_tabla  CHAR (16)
   END RECORD

   DEFINE arr_nss DYNAMIC ARRAY OF RECORD
          tpo_registro CHAR(1),
          nss          CHAR(11),
          folio        DECIMAL(9,0),
          mov_desc     CHAR(40),
          f_mov        DATE,
          monto        DECIMAL(12,2),
          acciones     DECIMAL(12,2),
          categoria    CHAR(12)
   END RECORD

   DEFINE a                        INTEGER
   DEFINE v_nss                    CHAR(11)
   DEFINE v_query_mov              STRING
   DEFINE v_detalle_mov            STRING
   DEFINE b                        INTEGER
   DEFINE v_bandera                SMALLINT
   DEFINE v_bnd_bco                SMALLINT
   DEFINE v_bnd_dato               SMALLINT

   LET v_bnd_dato = 0

   LET v_qry_tab = "SELECT tabla
                      FROM cat_tab_movimiento"

   PREPARE prp_tab_mov FROM v_qry_tab
   DECLARE cur_tab_mov CURSOR FOR prp_tab_mov

   LET b = 1
   FOREACH cur_tab_mov INTO arr_tab_mov[b].*
      LET b = b+1
   END FOREACH

   LET arr_tab_mov[arr_tab_mov.getLength()].v_nom_tabla = "cta_movimiento"

   FOR b = 1 TO arr_tab_mov.getLength()

      LET v_bnd_bco = 1
      
      LET v_bandera = 0
      
      LET v_query_mov = "
      select '2',","'",v_nss,"'",",
             folio_liquida,
             d.movimiento_desc,
             f_liquida,
             monto_pesos,
             monto_acciones,
             categoria
        from ",arr_tab_mov[b].v_nom_tabla," c,
             cat_movimiento d
       where id_derechohabiente in(
      select id_derechohabiente
        from afi_derechohabiente
       where nss =","'",v_nss,"'","
         and c.movimiento = d.movimiento
         and d.categoria in(1,2,3,9))"
      
      PREPARE prp_mov FROM v_query_mov
      DECLARE cur_mov CURSOR FOR prp_mov
      LET a = 1
      FOREACH cur_mov INTO arr_nss[a].*
         LET a = a+1
      END FOREACH
      
      IF arr_nss[arr_nss.getLength()].nss IS NULL THEN
         CALL arr_nss.deleteElement(arr_nss.getLength())
      END IF
      
      IF arr_nss.getLength() = 0 THEN
         LET v_bandera = 0
      ELSE
         LET v_bandera = 1
         LET v_bnd_bco = 0
      END IF
      
      IF b <= arr_tab_mov.getLength() THEN
      
         IF v_bandera = 1 THEN

            FOR a = 1 TO arr_nss.getLength()
      
               LET v_detalle_mov = arr_nss[a].tpo_registro,
                                   arr_nss[a].nss,
                                   arr_nss[a].folio             USING "&&&&&&",
                                   arr_nss[a].mov_desc,
                                   arr_nss[a].f_mov             USING "DDMMYYYY",
                                   ((arr_nss[a].monto)*100)     USING "-&&&&&&&&&&&&&&",
                                   ((arr_nss[a].acciones)*100)  USING "&&&&&&&&&&&&&&&",
                                   arr_nss[a].categoria         USING "&&&&&&&&&&&&"
               CALL v_arch_salida_mov.write([v_detalle_mov])
               LET v_bnd_dato = 1
            END FOR
            CALL arr_nss.clear()
         END IF
      
      ELSE
         IF v_bnd_dato = 0 THEN

         IF v_bnd_bco = 1 THEN
            CALL arr_nss.appendElement()
            LET arr_nss[1].tpo_registro = 2
            LET arr_nss[1].nss = v_nss
            LET arr_nss[1].mov_desc = ' '
            LET arr_nss[1].f_mov = '0000/00/00'
            LET arr_nss[1].categoria = ' '
            LET arr_nss[1].monto = ' '
            LET arr_nss[1].acciones = ' '
         END IF
      
            FOR a = 1 TO arr_nss.getLength()
            
               LET v_detalle_mov = arr_nss[a].tpo_registro,
                                   arr_nss[a].nss,
                                   arr_nss[a].folio             USING "&&&&&&",
                                   arr_nss[a].mov_desc,
                                   arr_nss[a].f_mov             USING "DDMMYYYY",
                                   ((arr_nss[a].monto)*100)     USING "-&&&&&&&&&&&&&&",
                                   ((arr_nss[a].acciones)*100)  USING "-&&&&&&&&&&&&&&",
                                   arr_nss[a].categoria         USING "&&&&&&&&&&&&"
               CALL v_arch_salida_mov.write([v_detalle_mov])
               LET v_bnd_dato = 1
            END FOR
            CALL arr_nss.clear()
         END IF
      END IF
   END FOR
END FUNCTION

FUNCTION fn_tablas_temporales()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_nss_mci
      DROP TABLE tmp_nss_mci_general
      DROP TABLE tmp_sdos

   WHENEVER ERROR STOP

  --  se crea tabla temporal para guardar registros de cifras control
   CREATE TABLE tmp_nss_mci ( nss CHAR(11))
   CREATE TABLE tmp_nss_mci_general ( nss CHAR(11))
   CREATE TABLE tmp_sdos (nss CHAR(11),
                          sdo_97 DECIMAL(12,2),
                          sdo_92 DECIMAL(12,2))

DATABASE safre_viv

END FUNCTION

--*******************************
-- Función para generar reporte *
--*******************************
FUNCTION fn_genera_reporte()

   DEFINE v_reporte           STRING   -- Variable para nombre del reporte
   DEFINE v_ruta_reporte      STRING   -- Variable para ruta final del reporte
   DEFINE v_excepcion         SMALLINT
   DEFINE v_query             STRING   -- Variable para consulta de saldos para reporte
   DEFINE v_ruta_listados     CHAR (40)-- Variable para ruta de salida del reporte
   DEFINE report_handler      om.SaxDocumentHandler


   DEFINE r_reporte RECORD
          v_cuenta INTEGER
   END RECORD

   LET v_reporte = "AGRP331.4rp"

-- ruta para guardar el reporte
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        g_usuario CLIPPED , "-", -- usuario
                        "AGRP33","-",--".pdf" -- programa
                        v_pid USING "&&&&&","-", -- PID
                        g_proceso_cod USING "&&&&&", "-", -- código del proceso
                        g_opera_cod   USING "&&&&&",".pdf" -- código de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "[ SAFRE EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   IF NOT v_excepcion THEN

-- Consulta saldos para reporte
      LET v_query = "SELECT COUNT(*)
                             FROM safre_tmp:tmp_sdos"

      PREPARE prp_reporte FROM v_query
      DECLARE cur_resultados CURSOR FOR  prp_reporte

-- Inicia reporte
      START REPORT rep_resultados TO XML HANDLER report_handler

      FOREACH cur_resultados INTO r_reporte.*
      -- Salida de reporte
            OUTPUT TO REPORT rep_resultados(r_reporte.*)
      END FOREACH

-- Finaliza reporte
    FINISH REPORT rep_resultados

    END IF
END FUNCTION

--******************************************
-- Se cachan datos para generar el reporte *
--******************************************
REPORT rep_resultados(p_reporte)

   DEFINE p_reporte RECORD
          v_cuenta INTEGER
          --sdo_viv92                DECIMAL (12,2),
          --sdo_viv97                DECIMAL (12,2)
   END RECORD

   DEFINE v_fecha_reporte          DATE
   DEFINE v_usuario                CHAR (20)
   DEFINE v_cuenta_rechazos        INTEGER

   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_reporte = TODAY
         LET v_usuario       = g_usuario

         PRINTX v_fecha_reporte USING "DD-MM-YYYY"
         PRINTX v_usuario
         PRINTX v_nom_arch
         PRINTX v_arch_mov

      ON EVERY ROW

         IF p_reporte.v_cuenta IS NULL THEN
            LET p_reporte.v_cuenta = 0
         END IF

         PRINTX p_reporte.*

      ON LAST ROW

         SELECT COUNT(*)
           INTO v_cuenta_rechazos
           FROM safre_tmp:tmp_nss_mci_general tmp
          WHERE tmp.nss NOT IN (SELECT nss FROM afi_derechohabiente)

         IF (v_cuenta_rechazos IS NULL) OR (v_cuenta_rechazos < 1) THEN
            LET v_cuenta_rechazos = 0
         END IF

         PRINTX v_cuenta_rechazos

         LET v_qry_rechazos = "SELECT nss
                                 FROM safre_tmp:tmp_nss_mci_general tmp
                                WHERE tmp.nss NOT IN (SELECT nss FROM afi_derechohabiente)"

         PREPARE prp_rechazos FROM v_qry_rechazos
         DECLARE cur_rechazos CURSOR FOR  prp_rechazos

         FOREACH cur_rechazos INTO r_reporte_rechazos.*
            PRINTX r_reporte_rechazos.*
         END FOREACH
END REPORT