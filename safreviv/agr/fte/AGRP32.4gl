##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRP32                                             #
#Objetivo          => Programa que selecciona archivo que trae como      #
#                     datos NSS y NCI para obtener saldos                #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => Octubre 2014                                       #
##########################################################################
IMPORT OS 

DATABASE safre_viv

GLOBALS

   DEFINE v_nom_archivo            STRING   -- Variable para validar que el nombre de archivo comienze con "SOLSDOES"
   DEFINE v_nom_archivo_salida     STRING   -- Nombre de archivo en ruta envio
   DEFINE v_extension              STRING   -- Variable para validar que la extensión sea ".txt"
   DEFINE v_ruta_carga             CHAR (50)-- Variable que indica ruta y nombre con los que se dejara el archivo
   DEFINE g_titulo                 STRING   -- Variable para título de ventana
   DEFINE g_usuario                CHAR(20) -- Variable para recuperar nombre de usuario
   DEFINE p_tipo_ejecucion         SMALLINT -- Forma como ejecutará el programa
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio -- Ruta donde se descargara el archivo final
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate -- Ruta donde se deja archivo rescatado del usuario
   DEFINE v_ejecuta_sh             STRING
   DEFINE v_precio_fondo           STRING -- Consulta para obtener precio de fondo
   DEFINE v_precio_f               DECIMAL(19,14)

END GLOBALS

MAIN

   LET g_usuario          =   ARG_VAL  (1)
   LET p_tipo_ejecucion   =   ARG_VAL  (2)
   LET g_titulo           =   ARG_VAL  (3)

-- Se asigna el título de la ventana
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

-- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP32.log")

CALL fn_transfiere_archivo()

END MAIN

--**********************************************
-- Función  para transferir archivo a servidor *
--**********************************************
FUNCTION fn_transfiere_archivo()

   DEFINE v_archivo_d              STRING   -- Variable para saber ruta y nombre donde se dejará el archivo
   DEFINE v_archivo                STRING   -- Nombre de archivo seleccionado
   DEFINE v_pos                    INTEGER  -- Posición donde inicia la extensión ".txt"
   DEFINE cant                     INTEGER  -- Cantidad de caracteres que tiene el nombre del archivo
   DEFINE v_pos_1                  INTEGER  -- Posición donde inicia el nombre del archivo solsdoes
   DEFINE v_pos_2                  INTEGER  -- Posición donde inicia el nombre del archivo Solsdoes
   DEFINE v_pos_3                  INTEGER  -- Posición donde inicia el nombre del archivo SOLSDOES
   DEFINE v_pos_nom1               INTEGER  -- Posición donde termina nombre de v_pos_3
   DEFINE v_pos_nom2               INTEGER  -- Posición donde termina nombre de v_pos_2
   DEFINE v_pos_nom3               INTEGER  -- Posición donde termina nombre de v_pos_3
   DEFINE buf                      base.StringBuffer
   --DEFINE w                      ui.Window
   --DEFINE f                      ui.Form

   CLOSE WINDOW SCREEN

   OPEN WINDOW sel_archivo WITH FORM "AGRP321" {ATTRIBUTES(TEXT="SALDO")
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   CALL w.setText("Consulta")}

   INPUT BY NAME  v_archivo ATTRIBUTES (UNBUFFERED)

      AFTER INPUT

      ON ACTION ACCEPT

--*************************************************************
--Se valida que el archivo tenga nombre y extensión correctos *
--*************************************************************
--**********************************************************************************************************
         IF v_archivo IS NULL THEN
            CALL fn_mensaje ("Archivo","Debe de seleccionar un archivo","information")
            NEXT FIELD v_archivo
         END IF

         IF v_archivo.getIndexOf(" ", 1) THEN
            LET v_archivo = ""
            DISPLAY BY NAME v_archivo
            NEXT FIELD v_archivo
         END IF

         IF v_archivo IS NOT NULL THEN 
         CALL fn_mensaje ("Transferencia" ,v_archivo,"information")
            LET buf = base.StringBuffer.create()
            CALL buf.append(v_archivo)

            LET cant         = LENGTH(v_archivo)
            LET v_pos        = buf.getIndexof(".txt",1)
            LET v_extension  = buf.subString(v_pos,cant)
            LET v_pos_1      = buf.getIndexof("solsdoes",1)
            LET v_pos_2      = buf.getIndexof("Solsdoes",1)
            LET v_pos_3      = buf.getIndexof("SOLSDOES",1)
            LET v_pos_nom1   = (v_pos_1 + 7 )
            LET v_pos_nom2   = (v_pos_2 + 7 )
            LET v_pos_nom3   = (v_pos_3 + 7 )

            IF v_pos_1 <> 0  THEN
               LET v_nom_archivo = buf.subString (v_pos_1,v_pos_nom1)
            END IF

            IF v_pos_2 <> 0  THEN
               LET v_nom_archivo = buf.subString (v_pos_2,v_pos_nom2)
            END IF

            IF v_pos_3 <> 0  THEN
               LET v_nom_archivo = buf.subString (v_pos_3,v_pos_nom3)
            END IF

            IF v_pos_1 = 0 AND v_pos_2 = 0 AND v_pos_3 =0 THEN 
            CALL fn_mensaje ("Transferencia Archivo","El nombre del archivo no es correcto  \n
                           debe iniciar con SOLSDOES y \n
                           debe tener extensión txt","information")
                           LET v_archivo = ""
               DISPLAY BY NAME v_archivo
               NEXT FIELD v_archivo

            END IF 
            --CALL fn_mensaje ("Transferencia" ,v_nom_archivo,"information")

            IF v_nom_archivo = ("SOLSDOES") 
           OR  v_nom_archivo = ("solsdoes")
           OR  v_nom_archivo = ("Solsdoes")
           AND v_extension   = (".txt") THEN

            LET v_archivo_d = v_nom_archivo
            --DISPLAY "archivo :",v_archivo_d
--**********************************************************************************************************

--***************************************************
--Se  recupera el archivo y se deja en ruta rescate *
--***************************************************

-- Se recuperan las rutas de rescate y envio para el archivo
            SELECT ruta_rescate,
                   ruta_envio
              INTO v_ruta_rescate,
                   v_ruta_envio
              FROM seg_modulo
             WHERE modulo_cod ="agr"

            LET v_archivo_d = v_ruta_rescate CLIPPED,"/",v_nom_archivo||v_extension

            TRY

               CALL FGL_GETFILE(v_archivo,v_archivo_d)
               --MESSAGE "ARCHIVO TRANSFERIDO CORRRECTAMENTE"
               CALL fn_busca_datos()

         CONTINUE INPUT

            CATCH
               ERROR "NO SE PUDO TRANSFERIR"
               CONTINUE INPUT
            END TRY
         EXIT INPUT
      CLOSE WINDOW sel_archivo

      END IF
    END IF

      EXIT INPUT

      CLOSE WINDOW sel_archivo

      END INPUT

END FUNCTION

--**********************************************************************
-- Función que extrae datos de archivo para dejarlos en tabla temporal *
--**********************************************************************
FUNCTION fn_busca_datos()

   DEFINE v_cant                   INTEGER    -- variable que verifica que sean 21 caracteres en los datos de la tabla temporal
   DEFINE i                        INTEGER    -- Variable para utilizar de contador
   DEFINE s                        CHAR (21)  -- variable para leer lineas del archivo
   DEFINE ch                       base.Channel 
   DEFINE tok                      base.StringTokenizer
   DEFINE buf                      base.StringBuffer
   DEFINE cadena                   CHAR (21)  -- variable para rescatar lineas del archivo
   DEFINE nss_nci                  CHAR (21)  -- Obtiene los primeros 21 caracteres de cada linea del archivo

   CALL fn_tablas_temporales()

   LET v_ruta_carga = v_ruta_rescate CLIPPED,"/",v_nom_archivo||v_extension

   -- CALL fn_mensaje ("Archivo",v_ruta_carga,"information")

   LET ch = base.Channel.create()
   CALL ch.openFile(v_ruta_carga,"r")
   LET buf = base.StringBuffer.create()

   LET i = 1
   WHILE TRUE
      LET s = ch.readLine()
      LET tok = base.StringTokenizer.create(s," ")
      WHILE tok.hasMoreTokens()
         --DISPLAY "token:", tok.nextToken()
         LET cadena = tok.nextToken()

         INSERT INTO tmp_nss_nci VALUES (cadena)
         -- DISPLAY "VALORES INSERTADOS EN TABLA  ",cadena
      END WHILE

      IF ch.isEof() THEN EXIT WHILE
      END IF
         LET i = i + 1
   END WHILE
      CALL ch.close()
      CALL fn_consulta_saldos()

END FUNCTION

--*******************************************************************
-- Función que consulta saldos con los datos obtenidos del archivo  *
--*******************************************************************
FUNCTION fn_consulta_saldos()

   DEFINE v_query_aivs_viv92       STRING         -- Consulta para obtener aivs_viv92
   DEFINE j                        INTEGER        -- Variable para usar de contador
   DEFINE v_query_aivs_viv97       STRING         -- Consulta para obtener aivs_viv97
   DEFINE v_cant_2                 INTEGER        -- Cantidad de registros a los cual se les busco saldo
   DEFINE v_ruta_envio_final       CHAR(60)       -- Ruta para salida de archivo
   DEFINE v_datos_para_descarga    STRING         -- Consulta para datos de archivo de salida
   DEFINE v_extension_salida       CHAR(20)       -- Extensión con la que va el archivo de salida
   DEFINE v_fecha                  CHAR(8)        -- Fecha para el reporte
   DEFINE v_registros              STRING         -- Consulta para llenar arreglo
   DEFINE v_cant                   INTEGER        -- variable que verifica que sean 11 caracteres para NSS
   DEFINE v_cant2                  INTEGER        -- variable que verifica que sean 10 caracteres para NCI
   DEFINE v_arch_sdo               base.channel  -- manejador de apuntador hacia archivo
   DEFINE registros                STRING
   DEFINE v_tpo_cred               SMALLINT
   DEFINE v_sdo92                  DECIMAL(12,2)
   DEFINE v_sdo97                  DECIMAL(12,2)

   DEFINE r_registros              RECORD -- almacena datos para llenar tabla temporal para descarga de archivo final
          id                       DECIMAL(9,0),
          nss                      CHAR(11),
          nci                      CHAR(10),
          viv92                    CHAR(15),
          viv97                    CHAR(15),
          tpo_credito              CHAR(3)
    END RECORD

    LET v_fecha                  = TODAY USING "ddmmyyyy"

   SELECT extension
     INTO v_extension_salida
     FROM cat_operacion
    WHERE proceso_cod = "324"
      AND opera_cod   = "2"

   LET v_nom_archivo_salida = "Resp_cons_ag" ||v_fecha||"."||v_extension_salida CLIPPED

   LET v_ruta_envio_final = v_ruta_envio CLIPPED,"/",v_nom_archivo_salida
    -- se hace consulta para separar NSS y NCI de los datos obtenidos del archivo
      -- se crea el manejador de archivo
   LET v_arch_sdo = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_arch_sdo.openFile(v_ruta_envio_final, "w" )

    SELECT nss_nci[1,11] nss,
           nss_nci[12,21] nci
      FROM tmp_nss_nci
    INTO TEMP tmp_reg

    SELECT COUNT (*)
    INTO v_cant
    FROM tmp_reg
    WHERE length (nss) <> 11
    --CALL fn_mensaje ("Archivo",v_cant,"information")

    SELECT COUNT (*)
    INTO v_cant2
    FROM tmp_reg
    WHERE length (nci) <> 10
    --CALL fn_mensaje ("Archivo",v_cant2,"information")

    IF v_cant = 0 AND v_cant2 = 0 THEN

--se consulta el precio de acción para el día de hoy
   SELECT precio_fondo
   INTO v_precio_f
    FROM glo_valor_fondo
    WHERE fondo = 11
    AND f_valuacion = TODAY

    DISPLAY "precio de fondo   : ",v_precio_f
--consulta para obtener ID para cada registro de NSS y se ingresan a tabla temporal
   SELECT a.id_derechohabiente,
          a.nss,
          t.nci
     FROM afi_derechohabiente a,
          tmp_reg t
    WHERE a.nss = t.nss
   INTO TEMP tmp_datos

   LET v_registros = " SELECT a.id_derechohabiente,
                              a.nss,
                              a.nci
                        FROM  tmp_datos a
                     ORDER BY id_derechohabiente ASC"

   PREPARE prp_registros FROM v_registros
   DECLARE cur_registros CURSOR FOR prp_registros

-- consulta para obtener suma de acciones para cada ID
    LET v_query_aivs_viv92 = "SELECT sum (m.monto_acciones)
                                FROM cta_movimiento m
                               WHERE m.id_derechohabiente = ?
                                 AND m.subcuenta IN (8,42)"
                                 
    PREPARE prp_viv92 FROM v_query_aivs_viv92

    LET v_query_aivs_viv97 = "SELECT sum (m.monto_acciones)
                               FROM cta_movimiento m
                               WHERE m.id_derechohabiente = ?
                                 AND subcuenta IN (4,44)"

    PREPARE prp_viv97 FROM v_query_aivs_viv97

   LET j = 1

      FOREACH cur_registros INTO r_registros.id, r_registros.nss, r_registros.nci
      EXECUTE prp_viv92 USING r_registros.id INTO v_sdo92
      EXECUTE prp_viv97 USING r_registros.id INTO v_sdo97 

      LET r_registros.viv92 = v_sdo92 * v_precio_f * 100 USING "&&&&&&&&&&&&&&&"
      LET r_registros.viv97 = v_sdo97 * v_precio_f * 100 USING "&&&&&&&&&&&&&&&"

      DECLARE cur_tpo_credito CURSOR FOR SELECT  tpo_credito
                                            FROM cre_acreditado ac
                                           WHERE ac.id_derechohabiente = r_registros.id
                                        ORDER BY f_otorga DESC, estado,
                                                 ac.id_derechohabiente ASC

      LET j = 1

      FOREACH cur_tpo_credito INTO v_tpo_cred
        LET r_registros.tpo_credito = v_tpo_cred USING "&&&"
        LET j = j + 1
        EXIT FOREACH
      END FOREACH

-- se guardan registros obtenidos en consultas

   LET registros = 2||r_registros.nci,
                      r_registros.nss,
                      r_registros.tpo_credito,
                      r_registros.viv92,
                      r_registros.viv97

   --se escribe el registro en el archivo

   CALL v_arch_sdo.writeLine([registros])

      INSERT INTO tmp_tabla_descarga
      VALUES (2,
               r_registros.nci,
               r_registros.nss,
               r_registros.tpo_credito,
               ROUND(v_sdo92 * v_precio_f,2),
               ROUND(v_sdo97 * v_precio_f,2))
   END FOREACH

   CALL v_arch_sdo.close()

   CALL fn_mensaje ("Archivo","El archivo fue creado correctamente","information")
   
   CALL fn_mensaje ("Archivo"," Ejecutando envío interfaz para SAS","information") 

   LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/SASAG_RESP_CONS_.sh"
   RUN v_ejecuta_sh

   CALL fn_genera_reporte()

   ELSE
      IF v_cant <> 0 THEN
         CALL fn_mensaje ("Archivo","Algún(os) NSS no contiene(n) 11 caracteres ","information")
      END IF

      IF v_cant2 <> 0 THEN
         CALL fn_mensaje ("Archivo","Algún(os) NCI no contiene(n) 10 caracteres ","information")
      END IF 

   END IF

END FUNCTION

--***********************************************
-- Función que permite borrar tablas temporales *
--***********************************************
FUNCTION fn_tablas_temporales()

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_nss_nci
      DROP TABLE tmp_datos
      DROP TABLE tmp_tabla_descarga
      DROP TABLE tmp_reg

   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_nss_nci ( nss_nci CHAR(21))
   
   -- se crea tabla temporal para guardar registros consultados
      CREATE TEMP TABLE tmp_tabla_descarga ( tpo_registro CHAR(1),
                                             num_credito  CHAR(10),
                                             nss          CHAR(11),
                                             tpo_credito  CHAR(3),
                                             sdo_viv92    DECIMAL(12,2),
                                             sdo_viv97    DECIMAL(12,2))

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

   DEFINE r_reporte RECORD
          sdo_viv92 DECIMAL (12,2),
          sdo_viv97 DECIMAL (12,2)
    END RECORD

   DEFINE report_handler      om.SaxDocumentHandler

   LET v_reporte = "AGRP321.4rp"

-- ruta para guardar el reporte
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'agr'


   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        g_usuario CLIPPED , "-", -- usuario
                        "AGRP32",".pdf" -- programa
                       -- v_pid USING "&&&&&","-", -- PID
                      --  g_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                       -- g_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación


   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN -- if  the file loaded OK
      CALL fgl_report_selectPreview(1)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "[ SAFRE EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   IF NOT v_excepcion THEN

-- Consulta saldos para reporte
      LET v_query = "SELECT sum(sdo_viv92),sum(sdo_viv97)
                             FROM tmp_tabla_descarga"

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
          sdo_viv92                DECIMAL (12,2),
          sdo_viv97                DECIMAL (12,2)
   END RECORD

   DEFINE v_total_general_reporte  RECORD 
          total_general            INTEGER
   END RECORD

   DEFINE v_fecha_reporte          DATE
   DEFINE v_usuario                CHAR (20)
   DEFINE v_total_general          STRING

   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_reporte = TODAY
         LET v_usuario       = g_usuario

         PRINTX v_fecha_reporte USING "DD-MM-YYYY"
         PRINTX v_usuario
         PRINTX v_nom_archivo
         PRINTX v_nom_archivo_salida

      ON EVERY ROW
         PRINTX p_reporte.sdo_viv92
         PRINTX p_reporte.sdo_viv97

      ON LAST ROW

         --PRINTX p_reporte.*

         LET v_total_general = 'SELECT count(*)
                                 FROM  tmp_tabla_descarga'

         DECLARE cur_total CURSOR FROM v_total_general
         FOREACH cur_total INTO v_total_general_reporte.*
         END FOREACH

         PRINTX v_total_general_reporte.*

END REPORT