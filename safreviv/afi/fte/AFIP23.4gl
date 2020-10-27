##################################################################################################
#Modulo       => AFI                                                                             #
#Programa     => AFIP23                                                                          #
#Objetivo     => Generación de archivos para actualización de contactos en BUC                   #
#Autor        => Jose Eduardo Ventura Bonola                                                     #
#Fecha inicio => 06/ENERO/2016                                                                   #
##################################################################################################


DATABASE safre_viv

GLOBALS
--variable con intervalos de fecha para generción de archivos
   DEFINE f_inicial                DATE           --fecha inicial
   DEFINE f_final                  DATE           --fecha final
--variables globales que se envian de lanzador
   DEFINE v_pid                    DECIMAL(9,0) 
   DEFINE g_proceso_cod            INTEGER
   DEFINE g_opera_cod              INTEGER
   DEFINE g_usuario                CHAR(20)
--***********************************************************************
   DEFINE v_nombre_archivo         STRING         -- variable de direcciÓn para generaciÓn de archivo K 
   DEFINE v_estado                 SMALLINT       -- variable que regresa función opera_fin  
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
  --DEFINE  v_consulta              STRING
   DEFINE v_cadena                 CHAR(7)        --variable con caracteres generales para información de archivos
   DEFINE r_b_valida               SMALLINT       --variable que regresa función opera_ini
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados
   DEFINE ch2 base.Channel
 -- variables para nuevo archivo nohup 
   DEFINE v_nohup                  STRING
 --Varibles con información de displays para log
   DEFINE v_cadena1                STRING
-- Variable para nombrar la copia de archivo de descarga
   DEFINE v_nombre_cpk             STRING
   DEFINE v_nombre_cpk2            STRING
--variable para contabiliar registros descargados
   DEFINE v_contabiliza_k          INTEGER
--banderas para validar descargas de información
   DEFINE v_bandera1               SMALLINT
   DEFINE s                        CHAR (1)
   DEFINE v_s_comando              STRING
   DEFINE v_cuenta_reg             INTEGER
   DEFINE v_fecha                  DATE
   DEFINE v_val                    INTEGER
   DEFINE f_ejecucion              DATE
   DEFINE v_hora                   DATETIME HOUR TO SECOND
   DEFINE ch                       base.Channel
   DEFINE bnd_estado               SMALLINT
   DEFINE v_f_max                  DATE

END GLOBALS

MAIN

-- parametros que vienen de lanzador
   LET g_usuario       = ARG_VAL (1)
   LET v_pid           = ARG_VAL (2)
   LET g_proceso_cod   = ARG_VAL (3)
   LET g_opera_cod     = ARG_VAL (4)
   LET f_inicial       = ARG_VAL (5)
   LET f_final         = ARG_VAL (6)
   LET v_estado = 0

   CALL STARTLOG(g_usuario CLIPPED|| ".AFIP23.log")

   LET v_cadena = "SACI"
   LET v_cadena1 ="DER"

   -- se obtienen rutas
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

    LET s = "|"

--**********************************************
--Condición para ejecución de forma automática *

   CALL fn_tablas_temporales()

   IF (f_inicial IS NULL) AND
      (f_final  IS NULL ) THEN

      SELECT MAX (f_proceso)
        INTO v_f_max
        FROM afi_arh_buc_adm

         LET f_inicial = (v_f_max)
         LET f_final   = (TODAY -1)

   END IF

   --CALL fn_display_proceso(0,"GENERA ARCHIVOS BUC")
   LET v_hora = CURRENT

   DISPLAY ""
   DISPLAY "INICIO ETAPA  : GENERA ARCHIVO BUC Actulización Datos Maestros"
   DISPLAY "FECHA :  ",TODAY USING "DD/MM/YYYY"
   DISPLAY "HORA  :  ",v_hora
   DISPLAY ""
   DISPLAY "FECHAS DE BÚSQUEDA"
   DISPLAY ""
   DISPLAY "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY"
   DISPLAY ""
   DISPLAY "Fecha Final: ", f_final USING "DD-MM-YYYY"
   DISPLAY ""

   CALL fn_genera_archivo_k()

   --DISPLAY "bandera estado ",bnd_estado 

   IF v_estado = 0 THEN
      IF bnd_estado = 0 THEN
         DISPLAY ""
         DISPLAY "PROCESO EJECUTADO CORRECTAMENTE..."
         DISPLAY ""
         DISPLAY "El archivo fue generado en ",v_nombre_cpk2
      ELSE
         DISPLAY ""
         DISPLAY "PROCESO EJECUTADO CORRECTAMENTE..."
         DISPLAY ""
         DISPLAY "No se generó archivo ya que no se encontraron registros en el intervalo de fechas indicado "
      END IF
         --DISPLAY v_pid," ",g_proceso_cod," ",g_opera_cod

     CALL fn_actualiza_opera_fin( v_pid,
                                  g_proceso_cod,
                                  g_opera_cod)
                        RETURNING v_estado
      --DISPLAY v_estado
   ELSE
 --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF
   CALL fn_display_proceso(1,"GENERA ARCHIVO")
END MAIN

FUNCTION fn_genera_archivo_k()

   DEFINE v_concepto1              CHAR(1)        -- concepto para relacionar información a derechohabientes
   DEFINE v_cp_k                   STRING         -- cadena con nombre para copia de archivo de salida
   DEFINE v_constante              STRING         -- constante con concepto para realcionar infromacióna  derechohabiete
   DEFINE v_arch_cck               STRING         -- cadena con ruta y nombre de archivo de cifrass control
   DEFINE v_query_cuentak          STRING         -- cadena con cadena y cantidad de registros encontrados
   DEFINE v_cc_k                   CHAR(27)       -- valor ingresado a tabla temporal para archivo de cifras control
   DEFINE v_rm_k                   STRING
   DEFINE v_cuenta_arh_k           STRING
   DEFINE v_total1                 STRING
   DEFINE k                        CHAR(40)
   DEFINE v_nombre_cpk1            STRING
   DEFINE v_arch_cck1              STRING
   DEFINE v_qry                    STRING
   DEFINE v_qry2                   STRING
   DEFINE v_detalle                STRING
   DEFINE v_nss                    CHAR(11)
   DEFINE v_arch_salida_mov        base.Channel
   DEFINE v_nss_unificador         CHAR(11)
   DEFINE v_id_unificador          DECIMAL(9,0)
   DEFINE v_diag                   SMALLINT

   DEFINE arr_id_his DYNAMIC ARRAY OF RECORD
          id_derechohabiente DECIMAL(9,0),
          paterno            CHAR(40),
          materno            CHAR(40),
          nombre             CHAR(40),
          nombre_imss        CHAR(40),
          rfc                CHAR(13),
          curp               CHAR(18),
          f_nacimiento       DATE,
          sexo               CHAR(1)
   END RECORD

   DEFINE arr_afi_derechohabiente DYNAMIC ARRAY OF RECORD
          nss                CHAR(11),
          paterno            CHAR(40),
          materno            CHAR(40),
          nombre             CHAR(40),
          nombre_imss        CHAR(40),
          rfc                CHAR(13),
          curp               CHAR(18),
          f_nacimiento       DATE,
          sexo               CHAR(1)
   END RECORD

   DEFINE a                        INTEGER

   LET v_concepto1 = "K"
   LET v_nombre_archivo = v_ruta_envio CLIPPED ,"/CATMAESTRODH",TODAY USING "DDMMYYYY",".bcdh"
   LET v_nombre_cpk = v_ruta_envio CLIPPED ,"/CATMAESTRODH",".bcdh"
   LET v_nombre_cpk1 = v_ruta_envio CLIPPED ,"/CATMAESTRODH",".txtx"
   LET v_nombre_cpk2 = v_ruta_envio CLIPPED ,"/CATMAESTRODH",".txt"
   LET v_constante = v_cadena CLIPPED,v_cadena1 CLIPPED,v_concepto1 CLIPPED
   LET v_arch_cck = v_ruta_envio CLIPPED ,"/CATMAESTRODHCC",".bcdh"
   LET v_arch_cck1 = v_ruta_envio CLIPPED ,"/CATMAESTRODHCC",".txt"
   LET v_cp_k = v_ruta_envio CLIPPED ,"/CATMAESTRODH",".bcml"
   LET v_total1 = v_ruta_envio CLIPPED ,"/CUENTATOTALK",".txt"

   LET bnd_estado = 0

   IF f_inicial IS NOT NULL THEN

      LET v_arch_salida_mov = base.Channel.create()
      CALL v_arch_salida_mov.openFile(v_nombre_archivo,"w" )
      CALL v_arch_salida_mov.setDelimiter("=")

      SELECT id_derechohabiente,
             ind_modifica
        FROM afi_his_derechohabiente
       WHERE id_derechohabiente > 0
         AND f_modifica BETWEEN f_inicial AND f_final
   INTO TEMP afi_ctr_adm

      DELETE FROM afi_ctr_adm
            WHERE ind_modifica IN (3,4,7,8,14,15)

      INSERT INTO afi_ctr_adm
      SELECT id_derechohabiente,'0' AS ind_modifica
        FROM afi_derechohabiente
       WHERE id_derechohabiente > 0
         AND f_apertura BETWEEN f_inicial AND f_final

      SELECT COUNT (*)
        INTO v_contabiliza_k
        FROM afi_ctr_adm

      IF v_contabiliza_k > 0 THEN

         --DISPLAY "cuenta temporal ",v_contabiliza_k

         LET v_qry = "select unique id_derechohabiente
                        from afi_ctr_adm"

         PREPARE v_qry FROM v_qry
         DECLARE cur_qry CURSOR FROM v_qry

         LET a = 1
         FOREACH cur_qry INTO arr_id_his[a].id_derechohabiente
          --DISPLAY "arreglo his ",arr_id_his[a].*
            LET a = a+1
         END FOREACH

         CLOSE cur_qry
         FREE cur_qry
         

         IF arr_id_his[a].id_derechohabiente IS NULL THEN
            CALL arr_id_his.deleteElement(arr_id_his.getLength())
         END IF

         FOR a = 1 TO arr_id_his.getLength()

            LET v_bandera1 = 1

            SELECT nss,
                   ap_paterno_af,
                   ap_materno_af,
                   nombre_af,
                   nombre_imss,
                   rfc,
                   curp,
                   f_nacimiento,
                   sexo
              INTO v_nss,
                   arr_id_his[a].paterno,
                   arr_id_his[a].materno,
                   arr_id_his[a].nombre,
                   arr_id_his[a].nombre_imss,
                   arr_id_his[a].rfc,
                   arr_id_his[a].curp,
                   arr_id_his[a].f_nacimiento,
                   arr_id_his[a].sexo
              FROM afi_derechohabiente
             WHERE id_derechohabiente = arr_id_his[a].id_derechohabiente

             IF (arr_id_his[a].sexo = 1 ) OR
                (arr_id_his[a].sexo = 2) THEN
                LET arr_id_his[a].sexo = arr_id_his[a].sexo
             ELSE
                IF (length(arr_id_his[a].curp) = 18) THEN
                   LET arr_id_his[a].sexo = arr_id_his[a].curp[11,11]
                      CASE
                         WHEN arr_id_his[a].sexo = "M"
                            LET arr_id_his[a].sexo = 2

                         WHEN arr_id_his[a].sexo = "H"
                            LET arr_id_his[a].sexo = 1

                         OTHERWISE
                            LET arr_id_his[a].sexo = 0
                      END CASE
                ELSE 
                   LET arr_id_his[a].sexo = 0
                END IF
             END IF

            LET v_detalle = "SACI","¿",
                            "DER","¿",
                            "K","¿",
                            TODAY USING "YYYYMMDD" CLIPPED,"¿",
                            v_nss CLIPPED,"¿",
                            arr_id_his[a].paterno CLIPPED,"¿",
                            arr_id_his[a].materno CLIPPED,"¿",
                            arr_id_his[a].nombre CLIPPED,"¿",
                            arr_id_his[a].nombre_imss CLIPPED,"¿",
                            arr_id_his[a].rfc CLIPPED,"¿",
                            arr_id_his[a].curp CLIPPED,"¿",
                            arr_id_his[a].sexo CLIPPED,"¿",
                            arr_id_his[a].f_nacimiento USING "YYYYMMDD" CLIPPED,"¿"

            -- DISPLAY "detalle : ",v_detalle

            CALL v_arch_salida_mov.write([v_detalle])
         END FOR
         CALL v_arch_salida_mov.close()
      ELSE
         --DISPLAY "cuenta temporal ",v_contabiliza_k
         LET bnd_estado = 0 -- bandera pra generación de archivo, si es 1, no se genra archivo cuando no trae datos
         LET v_bandera1 = 1 -- bandera para generar archivo con nombre y extensión correcta
      END IF
   ELSE

      LET v_arch_salida_mov = base.Channel.create()
      CALL v_arch_salida_mov.openFile(v_nombre_archivo,"w" )
      CALL v_arch_salida_mov.setDelimiter("=")

      LET v_qry = "SELECT nss,
                          ap_paterno_af,
                          ap_materno_af,
                          nombre_af,
                          nombre_imss,
                          rfc,
                          curp,
                          f_nacimiento,
                          decode(sexo,'1','1','2','2','0')
                     FROM afi_derechohabiente
                    WHERE f_apertura < ","'",f_final,"'"

         PREPARE prp_qry_afi FROM v_qry
         DECLARE cur_qry_afi CURSOR FOR prp_qry_afi

         LET a = 1
         FOREACH cur_qry_afi INTO arr_afi_derechohabiente[a].*
          --DISPLAY "arreglo his ",arr_id_his[a].*
            LET a = a+1
         END FOREACH

         CLOSE cur_qry_afi
         FREE cur_qry_afi

         IF arr_afi_derechohabiente[a].nss IS NULL THEN
            CALL arr_afi_derechohabiente.deleteElement(arr_afi_derechohabiente.getLength())
         END IF

         LET v_bandera1 = 1

         FOR a = 1 TO arr_afi_derechohabiente.getLength()

            LET v_detalle = "SACI","¿",
                            "DER","¿",
                            "K","¿",
                            TODAY USING "YYYYMMDD" CLIPPED,"¿",
                            arr_afi_derechohabiente[a].nss CLIPPED,"¿",
                            arr_afi_derechohabiente[a].paterno CLIPPED,"¿",
                            arr_afi_derechohabiente[a].materno CLIPPED,"¿",
                            arr_afi_derechohabiente[a].nombre CLIPPED,"¿",
                            arr_afi_derechohabiente[a].nombre_imss CLIPPED,"¿",
                            arr_afi_derechohabiente[a].rfc CLIPPED,"¿",
                            arr_afi_derechohabiente[a].curp CLIPPED,"¿",
                            arr_afi_derechohabiente[a].sexo CLIPPED,"¿",
                            arr_afi_derechohabiente[a].f_nacimiento USING "YYYYMMDD" CLIPPED,"¿"

            CALL v_arch_salida_mov.write([v_detalle])
         END FOR
         CALL v_arch_salida_mov.close()
   END IF

   IF v_bandera1 = 1 THEN

--    se crea comando que elimina pipes
      LET v_s_comando = "sed 's/|//g' ",v_nombre_archivo," > ",v_cp_k
      RUN v_s_comando
--    se crea comando que elimina espacios en blanco
      LET v_s_comando = "sed '/./!d' ",v_cp_k," > ",v_nombre_cpk
      RUN v_s_comando
--    se crea comando que cuenta líneas de archivo
      LET v_cuenta_arh_k ="cat ",v_nombre_cpk,"|wc -l"," > ",v_total1
      RUN v_cuenta_arh_k

      LET ch2 = base.Channel.create()
      CALL ch2.openFile(v_total1,"r")
      LET k = ch2.readLine()
      CALL ch2.close()
      LET v_contabiliza_k = k

      LET v_query_cuentak = v_constante CLIPPED,TODAY USING "YYYYMMDD",v_contabiliza_k USING "&&&&&&&&&&&"
      LET v_cc_k = v_query_cuentak

      INSERT INTO safre_tmp:tmp_cifras_control_adm (cc_dh) VALUES (v_cc_k)

      UNLOAD TO v_arch_cck SELECT cc_dh [1,4],
                                  cc_dh [5,7],
                                  cc_dh [8,8],
                                  cc_dh [9,16],
                                  cc_dh [17,27] 
                             FROM safre_tmp:tmp_cifras_control_adm
                            WHERE cc_dh IS NOT NULL

      INSERT INTO afi_arh_buc_adm VALUES ("K",
                                           TODAY,
                                           v_contabiliza_k,
                                           g_usuario)
-- comando para borrar archivo
      LET v_rm_k = "rm ",v_cp_k
      RUN v_rm_k
      
--    se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_nombre_cpk," > ",v_cp_k
      RUN v_s_comando
      
--    se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_arch_cck," > ",v_arch_cck1
      RUN v_s_comando
      
      LET v_rm_k = "rm ",v_total1," ",v_arch_cck," ",v_nombre_cpk
      RUN v_rm_k

--se crea comando que sustutiye igual por pipes
      LET v_s_comando = "sed 's/¿/|/g' ",v_cp_k," > ",v_nombre_cpk1
      RUN v_s_comando

--se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_nombre_cpk1," > ",v_nombre_cpk2
      RUN v_s_comando

-- comando para borrar archivo
      LET v_rm_k = "rm ",v_cp_k, " ",v_nombre_cpk1
      RUN v_rm_k

      DISPLAY " Ejecutando envío de archivo "

      LET v_s_comando = "\n sh /opt/Interpel/Scripts/variables/BUC/SACI.sh"
      RUN v_s_comando

      DISPLAY ""
      DISPLAY " Finaliza envió de archivo "

      CALL fn_genera_reporte()
   END IF

END FUNCTION

--***********************************************
-- Función que permite borrar tablas temporales *
--***********************************************
FUNCTION fn_tablas_temporales()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_cifras_control_adm
      

   WHENEVER ERROR STOP

   -- se crea tabla temporal para guardar registros de cifras control
      CREATE TEMP TABLE tmp_cifras_control_adm ( cc_dh CHAR(27))

 DATABASE safre_viv
END FUNCTION

--*******************************
-- Función para generar reporte *
--*******************************
FUNCTION fn_genera_reporte()

   DEFINE v_reporte                STRING         -- Variable para nombre del reporte
   DEFINE v_ruta_reporte           STRING         -- Variable para ruta final del reporte
   DEFINE v_excepcion              SMALLINT
   DEFINE v_ruta_listados          CHAR (40)      -- Variable para ruta de salida del reporte
   DEFINE report_handler           om.SaxDocumentHandler
   DEFINE v_ruta_bin               LIKE seg_modulo.ruta_bin

   


   SELECT ruta_bin
     INTO v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = "afi"

   LET v_reporte = v_ruta_bin CLIPPED,"/AFIP23.4rp"

-- ruta para guardar el reporte
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        g_usuario CLIPPED , "-", -- usuario
                        "AFIP23", "-", -- programa
                        v_pid USING "&&&&&","-", -- PID
                        g_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        g_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET report_handler = fgl_report_commitCurrentSettings()  -- commit the file settings
   ELSE
      DISPLAY "[ SAFRE EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   IF NOT v_excepcion THEN

-- Inicia reporte
      START REPORT rep_resultados TO XML HANDLER report_handler

-- Salida de reporte
      OUTPUT TO REPORT rep_resultados(v_contabiliza_k)
-- Finaliza reporte

      FINISH REPORT rep_resultados

   END IF

END FUNCTION

--******************************************
-- Se cachan datos para generar el reporte *
--******************************************
REPORT rep_resultados(v_contabiliza_k)


   DEFINE v_fecha_reporte          DATE
   DEFINE v_usuario                CHAR (20)
   DEFINE v_contabiliza_k          INTEGER

   FORMAT

      FIRST PAGE HEADER

      LET v_fecha_reporte = TODAY
      LET v_usuario       = g_usuario

      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX v_usuario
      PRINTX f_inicial USING "DD-MM-YYYY"
      PRINTX f_final USING "DD-MM-YYYY"

   ON LAST ROW

      PRINTX v_contabiliza_k

END REPORT