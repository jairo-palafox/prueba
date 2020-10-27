
##################################################################################################
#Modulo       => AFI                                                                             #
#Programa     => AFIP17                                                                          #
#Objetivo     => Generación de archivos para actualización de contactos en BUC                   #
#Autor        => Jose Eduardo Ventura Bonola                                                     #
#Fecha inicio => 14/ENERO/2015                                                                   #
##################################################################################################


DATABASE safre_viv

GLOBALS
--variable con intervalos de fecha para generción de archivos
   DEFINE        f_inicial         DATE           --fecha inicial
   DEFINE        f_final           DATE           --fecha final
--variables globales que se envian de lanzador
   DEFINE v_pid                    DECIMAL(9,0) 
   DEFINE g_proceso_cod            INTEGER
   DEFINE g_opera_cod              INTEGER
   DEFINE g_opera_cod2             INTEGER
   DEFINE g_opera_cod3             INTEGER
   DEFINE g_opera_cod4             INTEGER
   DEFINE g_opera_cod5             INTEGER
   DEFINE g_usuario                CHAR(20)
--***********************************************************************
   DEFINE v_nombre_archivo         STRING         -- variable de direcciÓn para generaciÓn de archivo K 
   DEFINE v_estado                 SMALLINT       -- variable que regresa función opera_fin  
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
  --DEFINE  v_consulta              STRING
   DEFINE v_cadena                 CHAR(7)        --variable con caracteres generales para información de archivos
   DEFINE r_b_valida               SMALLINT       --variable que regresa función opera_ini
   DEFINE v_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE ch2 base.Channel
 -- variables para nuevo archivo nohup 
   DEFINE v_nohup                  STRING
   DEFINE v_nohup2                 STRING
   DEFINE v_nohup3                 STRING
   DEFINE v_nohup4                 STRING
   DEFINE v_nohup5                 STRING
 --Varibles con información de displays para log
   DEFINE v_cadena1                STRING
   DEFINE v_cadena2                STRING
   DEFINE v_cadena3                STRING
   DEFINE v_cadena4                STRING
   DEFINE v_cadena5                STRING
-- Variable para nombrar la copia de archivo de descarga
   DEFINE v_nombre_cpk             STRING
   DEFINE v_nombre_cpd             STRING 
   DEFINE v_nombre_cpt             STRING
   DEFINE v_nombre_cpc             STRING
   DEFINE v_nombre_cpr             STRING
--variable para contabiliar registros descargados
   DEFINE v_contabiliza_k          INTEGER
   DEFINE v_contabiliza_d          INTEGER
   DEFINE v_contabiliza_t          INTEGER
   DEFINE v_contabiliza_c          INTEGER
   DEFINE v_contabiliza_r          INTEGER
--banderas para validar descargas de información
   DEFINE v_bandera1               SMALLINT
   DEFINE v_bandera2               SMALLINT
   DEFINE v_bandera3               SMALLINT
   DEFINE v_bandera4               SMALLINT
   DEFINE v_bandera5               SMALLINT
   DEFINE s                        CHAR (1)

   DEFINE v_s_comando              STRING
   DEFINE v_cuenta_reg             INTEGER
   DEFINE v_fecha                  DATE
   DEFINE v_val                    INTEGER
   DEFINE f_ejecucion              DATE
   DEFINE v_hora                   DATETIME HOUR TO SECOND

   DEFINE ch                       base.Channel

END GLOBALS

MAIN

-- parametros que vienen de lanzador
   LET g_usuario       = ARG_VAL (1)
   LET v_pid           = ARG_VAL (2)
   LET g_proceso_cod   = ARG_VAL (3)
   LET g_opera_cod     = ARG_VAL (4)
   LET f_inicial       = ARG_VAL (5)
   LET f_final         = ARG_VAL (6)

   CALL STARTLOG(g_usuario CLIPPED|| ".AFIP17.log")

   LET v_cadena = "SACI"
   LET v_cadena1 ="DER"
   LET g_opera_cod2 = 2
   LET g_opera_cod3 = 3
   LET g_opera_cod4 = 4
   LET g_opera_cod5 = 5

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
--**********************************************

   IF f_inicial IS NULL AND f_final IS NULL THEN
   
      CALL fn_tablas_temporales()

      SELECT COUNT (*)
        INTO v_cuenta_reg
        FROM afi_ctr_arh_buc
   
         IF v_cuenta_reg > 0 THEN
            SELECT MAX (f_proceso)
              INTO f_inicial
              FROM afi_ctr_arh_buc
             WHERE concepto = "K"
   
         ELSE
   
            LET f_inicial = NULL
         END IF
   
      LET v_val       = 1
      LET v_fecha     = TODAY
      LET f_ejecucion = v_fecha
      LET f_final     = v_fecha-(v_val) UNITS DAY
   
   
   END IF
--****************************************************
   LET v_estado = 0

   CALL fn_tablas_temporales()

   --CALL fn_display_proceso(0,"GENERA ARCHIVOS BUC")
   LET v_hora = CURRENT

   DISPLAY ""
   DISPLAY "INICIO ETAPA  : GENERA ARCHIVOS BUC"
   DISPLAY "FECHA :  ",TODAY USING "DD/MM/YYYY"
   DISPLAY "HORA  :  ",v_hora
   DISPLAY ""
   DISPLAY "FECHAS DE BÚSQUEDA"
   DISPLAY "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY"
   DISPLAY "Fecha Final:   ", f_final   USING "DD-MM-YYYY"
   DISPLAY ""
   CALL fn_genera_archivo_k()

   IF v_estado = 0 THEN

   IF v_bandera1 = 1 THEN 

      DISPLAY ""
      DISPLAY "PROCESO EJECUTADO CORRECTAMENTE..."
      DISPLAY ""
      DISPLAY "El archivo DERECHOHABIENTES fue generado en ",v_nombre_cpk

   END IF
   IF v_bandera1 = 2 THEN

      DISPLAY "No existe nueva información para generar el archivo BUCDHCATA.bcdh"
      
   END IF

   CALL fn_actualiza_opera_fin(v_pid,
                               g_proceso_cod,
                               g_opera_cod)
                     RETURNING v_estado

      ELSE
 --Si ocurrio un error se actualiza el estatus como erroneo
         CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF

--******************************************************************************
--Datos para log de operación 2  "Generación archivo domicilios"               *
--******************************************************************************
   CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                               g_opera_cod2,"",
                               "AFIP17","",
                               g_usuario)  RETURNING r_b_valida

   LET ch = base.Channel.create()

   LET v_nohup2 = v_ruta_listados CLIPPED,"/nohup:",
                  v_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod2 USING "&&&&&"

   CALL ch.openFile(v_nohup2,"w")

   IF v_estado = 0 THEN
      CALL fn_genera_archivo_d ()

      IF v_bandera2 = 1 THEN

         LET v_cadena2 = "FECHA DE EJECUCIÓN : ", TODAY USING "DD-MM-YYYY","\n",
                         "FECHAS DE BÚSQUEDA \n",
                         "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY","\n",
                         "Fecha Final:   ", f_final   USING "DD-MM-YYYY","\n",
                         "","\n",
                         "OPERACIÓN EJECUTADA CORRECTAMENTE...","\n",
                         "","\n",
                         "El archivo DOMICILIOS fue generado en ", v_nombre_cpd

      END IF

      IF v_bandera2 = 2 THEN

      
          LET v_cadena2 = "","\n",
                          "FECHAS DE BÚSQUEDA","\n",
                          "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY","\n",
                          "Fecha Final:   ", f_final   USING "DD-MM-YYYY","\n",
                          "","\n",
                          "No existe nueva información para generar el archivo BUCDHDIR.bcdm"
         
      END IF

      CALL ch.write([v_cadena2])
      CALL ch.close()
      CALL fn_actualiza_opera_fin(v_pid,
                                  g_proceso_cod,
                                  g_opera_cod2) RETURNING v_estado

      ELSE

 --Si ocurrio un error se actualiza el estatus como erroneo
         CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod2)  RETURNING v_estado
   END IF
 --Se ejecutan los displays
 --*******CALL fn_display_proceso(1,"GENERA ARCHIVOS PARA ACTUALIZACIÓN DE DATOS DE CONTACTO")

--******************************************************************************
--Datos para log de operación 3 "generación archivo telefonos"                                          
--******************************************************************************

    CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                                g_opera_cod3,"",
                                "AFIP17","",
                                g_usuario)  RETURNING r_b_valida

   LET ch = base.Channel.create()

   LET v_nohup3 = v_ruta_listados CLIPPED,"/nohup:",v_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod3 USING "&&&&&"

   CALL ch.openFile(v_nohup3,"w")

   IF v_estado = 0 THEN
      CALL fn_genera_archivo_t ()

      IF v_bandera3 = 1 THEN

         LET v_cadena3 = "FECHA DE EJECUCIÓN : ", TODAY USING "DD-MM-YYYY","\n",
                         "FECHAS DE BÚSQUEDA \n",
                         "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY","\n",
                         "Fecha Final:   ", f_final   USING "DD-MM-YYYY","\n",
                         "","\n",
                         "OPERACIÓN EJECUTADA CORRECTAMENTE...","\n",
                         "","\n",
                         "El archivo TELÉFONOS fue generado en ", v_nombre_cpt
      END IF

      IF v_bandera3 = 2 THEN
      
         LET v_cadena3 = "","\n",
                          "FECHAS DE BÚSQUEDA","\n",
                          "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY","\n",
                          "Fecha Final:   ", f_final   USING "DD-MM-YYYY","\n",
                          "","\n",
                          "No existe nueva información para generar el archivo BUCDHTEL.bctl"
      END IF

      CALL ch.write([v_cadena3])

      CALL ch.close()

      CALL fn_actualiza_opera_fin(v_pid,
                                  g_proceso_cod,
                                  g_opera_cod3) RETURNING v_estado
   ELSE

 --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod3)  RETURNING v_estado
   END IF

--******************************************************************************
--Datos para log de operación 4 "Generación archivo contacto electronico"      *
--******************************************************************************

   CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                               g_opera_cod4,"",
                               "AFIP17","",
                               g_usuario)  RETURNING r_b_valida

   LET ch = base.Channel.create()

   LET v_nohup4 = v_ruta_listados CLIPPED,
                  "/nohup:",v_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod4 USING "&&&&&"

   CALL ch.openFile(v_nohup4,"w")

   IF v_estado = 0 THEN
      CALL fn_genera_archivo_c ()

      IF v_bandera4 = 1 THEN

         LET v_cadena4 = "FECHA DE EJECUCIÓN : ", TODAY USING "DD-MM-YYYY","\n",
                         "FECHAS DE BÚSQUEDA \n",
                         "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY","\n",
                         "Fecha Final:   ", f_final   USING "DD-MM-YYYY","\n",
                         "","\n",
                         "OPERACCIÓN EJECUTADA CORRECTAMENTE...","\n",
                         "","\n",
                         "El archivo CONTACTOS ELECTRONICOS fue generado en ", v_nombre_cpc
      END IF

      IF v_bandera4 = 2 THEN

         LET v_cadena4 =  "","\n",
                          "FECHAS DE BÚSQUEDA","\n",
                          "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY","\n",
                          "Fecha Final:   ", f_final   USING "DD-MM-YYYY","\n",
                          "","\n", "No existe nueva información para generar el archivo BUCDHMAIL.bcml"

      END IF

      CALL ch.write([v_cadena4])

      CALL ch.close()

      CALL fn_actualiza_opera_fin(v_pid,
                                  g_proceso_cod,
                                  g_opera_cod4) RETURNING v_estado
   ELSE

 --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod4)  RETURNING v_estado
   END IF

--******************************************************************************
--Datos para log de operación 5 "generación archivo Relaciones laborales"      *
--******************************************************************************

    CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                                g_opera_cod5,"",
                                "AFIP17","",
                                g_usuario)  RETURNING r_b_valida

   LET ch = base.Channel.create()

   LET v_nohup5 = v_ruta_listados CLIPPED,"/nohup:",
                  v_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod5 USING "&&&&&"

   CALL ch.openFile(v_nohup5,"w")

   IF v_estado = 0 THEN
      CALL fn_genera_archivo_r ()

      IF v_bandera5 = 1 THEN 

         LET v_cadena5 = "FECHA DE EJECUCIÓN : ", TODAY USING "DD-MM-YYYY","\n",
                         "FECHAS DE BÚSQUEDA \n",
                         "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY","\n",
                         "Fecha Final:   ", f_final   USING "DD-MM-YYYY","\n",
                         "","\n",
                         "OPERACIÓN EJECUTADA CORRECTAMENTE...","\n",
                         "","\n",
                         "El archivo RELACIONES LABORALES fue generado en ", v_nombre_cpr

      END IF 

      IF v_bandera5 = 2 THEN 

         LET v_cadena5 =  "","\n",
                          "FECHAS DE BÚSQUEDA","\n",
                          "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY","\n",
                          "Fecha Final:   ", f_final   USING "DD-MM-YYYY","\n",
                          "","\n", "No existe información para generar el archivo BUCDHCATAREL.bcrl"

      END IF

      CALL ch.write([v_cadena5])

      CALL ch.close()

      CALL fn_actualiza_opera_fin(v_pid,
                                  g_proceso_cod,
                                  g_opera_cod5) RETURNING v_estado
   ELSE

--Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod5)  RETURNING v_estado
   END IF

   CALL fn_genera_reporte()

END MAIN

--**********************************************************
--Función que genera archivo "CATÁLOGO DERECHOHABIENTES"  *
--**********************************************************

FUNCTION fn_genera_archivo_k()

   DEFINE v_concepto1              CHAR(1)        -- concepto para relacionar información a derechohabientes
   DEFINE v_query_k                STRING         -- cadena con información requerida para generación de archivo
   DEFINE v_cp_k                   STRING         -- cadena con nombre para copia de archivo de salida
   DEFINE v_constante              STRING         -- constante con concepto para realcionar infromacióna  derechohabiete
   DEFINE v_arch_cck               STRING         -- cadena con ruta y nombre de archivo de cifrass control
   DEFINE v_query_cuentak          STRING         -- cadena con cadena y cantidad de registros encontrados
   DEFINE v_cc_k                   CHAR(27)       -- valor ingresado a tabla temporal para archivo de cifras control
   DEFINE v_rm_k                   STRING
   DEFINE v_cuenta_arh_k           STRING
   DEFINE v_total1                 STRING
   DEFINE k                        CHAR(40)
   DEFINE v_cadena_prueba          STRING
   DEFINE v_nombre_cpk1            STRING
   DEFINE v_arch_cck1              STRING
   
   LET v_concepto1 = "K"
   LET v_nombre_archivo = v_ruta_envio CLIPPED ,"/BUCDH",TODAY USING "DDMMYYYY",".bcdh"
   LET v_nombre_cpk = v_ruta_envio CLIPPED ,"/BUCDHCATA",".bcdh"
   LET v_nombre_cpk1 = v_ruta_envio CLIPPED ,"/BUCDHCATA",".txt"
   LET v_constante = v_cadena CLIPPED,v_cadena1 CLIPPED,v_concepto1 CLIPPED
   LET v_arch_cck = v_ruta_envio CLIPPED ,"/BUCDHCATACC",".bcdh"
   LET v_arch_cck1 = v_ruta_envio CLIPPED ,"/BUCDHCATACC",".txt"
   LET v_cp_k = v_ruta_envio CLIPPED ,"/CATADH",".bcml"
   LET v_total1 = v_ruta_envio CLIPPED ,"/CUENTATOTALK",".txt"

   INSERT INTO safre_tmp:tmp_cadena_saci VALUES ("SACIDERK")

   
   IF f_inicial IS NOT NULL THEN

      SELECT COUNT (*)
        INTO v_contabiliza_k
        FROM afi_derechohabiente
       WHERE nss <> ' '
         AND length(nss) = 11
         AND f_apertura > f_inicial
         AND f_apertura < TODAY

      IF v_contabiliza_k <> 0 THEN

      UPDATE afi_derechohabiente
         SET sexo = decode(curp[11],"H",1,"M",2,0)
       WHERE (sexo not in(1,2)
          OR  sexo is null)
         AND curp is not NULL
         AND curp not MATCHES " *"
         AND f_apertura > f_inicial
         AND f_apertura < TODAY

         LET v_bandera1 = 1
         LET v_query_k = "SELECT tmp.cadena[1,4],tmp.cadena[5,7],tmp.cadena[8,8],",TODAY USING "YYYYMMDD","
                                  ,nss,
                                  ap_paterno_af,
                                  NVL(ap_materno_af,'                                        '),
                                  nombre_af,
                                  nombre_imss,
                                  NVL(rfc,'             '),
                                  NVL(curp,'                  '),
                                  tipo_trabajador,
                                  decode(sexo,'1','1','2','2','0')
                             FROM afi_derechohabiente,
                             safre_tmp:tmp_cadena_saci tmp
                            WHERE nss <> ' '
                              AND length(nss) = 11
                              AND f_apertura >","'",f_inicial,"'","
                              AND f_apertura <","'",TODAY,"'"

      ELSE

         LET v_bandera1 = 2
         --LET v_contabiliza_k = 0
         INSERT INTO afi_ctr_arh_buc VALUES (v_concepto1,TODAY,v_contabiliza_k,g_usuario)

      END IF

   END IF 

   IF f_inicial IS NULL THEN

      UPDATE afi_derechohabiente
         SET sexo = decode(curp[11],"H",1,"M",2,0)
       WHERE (sexo not in(1,2)
          OR  sexo is null)
         AND curp is not NULL
         AND curp not MATCHES " *"
         AND f_apertura < TODAY
   
      LET v_bandera1 = 1
      LET v_query_k = "SELECT tmp.cadena[1,4],tmp.cadena[5,7],tmp.cadena[8,8],",TODAY USING "YYYYMMDD","
                              ,nss,
                              ap_paterno_af,
                              NVL(ap_materno_af,'                                        '),
                              nombre_af,
                              nombre_imss,
                              NVL(rfc,'             '),
                              NVL(curp,'                  '),
                              tipo_trabajador,
                              decode(sexo,'1','1','2','2','0')
                         FROM afi_derechohabiente,
                         safre_tmp:tmp_cadena_saci tmp
                        WHERE nss <> ' '
                          AND length(nss) = 11
                          AND f_apertura < ", "'",TODAY,"'"

   END IF

   IF v_bandera1 = 1 THEN
      
         UNLOAD TO  v_nombre_archivo DELIMITER '=' v_query_k
      
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
      
      INSERT INTO safre_tmp:tmp_cifras_control (cc_dh) VALUES (v_cc_k)
      
      UNLOAD TO v_arch_cck SELECT cc_dh [1,4],
                                  cc_dh [5,7],
                                  cc_dh [8,8],
                                  cc_dh [9,16],
                                  cc_dh [17,27] FROM safre_tmp:tmp_cifras_control
      WHERE cc_dh IS NOT NULL
      
      INSERT INTO afi_ctr_arh_buc VALUES (v_concepto1,TODAY,v_contabiliza_k,g_usuario)
      
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
      LET v_s_comando = "sed 's/=/|/g' ",v_cp_k," > ",v_nombre_cpk1
      RUN v_s_comando

-- comando para borrar archivo
      LET v_rm_k = "rm ",v_cp_k
      RUN v_rm_k

   END IF

END FUNCTION

--*******************************************
--Función que genera archivo "DOMICILIOS"   *
--*******************************************

FUNCTION fn_genera_archivo_d ()

   DEFINE v_query_d                STRING         -- cadena para obtener información requerida
   DEFINE v_pais                   CHAR (40)      -- especificación de nombre de país
   DEFINE v_espacio                CHAR(10)       -- variable que contiene 10 espacios vacios
   DEFINE v_cp_d                   STRING         -- cadena con cnombre para copia de archivo
   DEFINE v_constante_d            STRING         -- cadena con concepto que relaciona la información a domicilios
   DEFINE v_arch_ccd               STRING         -- cadena con ruta y nombre de archivo para cifras control
   DEFINE v_query_cuentad          STRING         -- cadena con concepto y total de registros encontrados
   DEFINE v_concepto2              CHAR(1)        -- concepto que relaciona archivo con infromación de domicilos
   DEFINE v_cc_dm                  CHAR(27)       -- valor ingresado a tabla temporal para archivo de cifras control
   DEFINE v_nom_arch_d             STRING         -- cadena con ruta y nombre de archivo de salida principal
   DEFINE v_rm_d                   STRING
   DEFINE v_cuenta_arh_d           STRING
   DEFINE v_total2                 STRING
   DEFINE d                        CHAR(40)
   DEFINE v_nombre_cpd1            STRING
   DEFINE v_arch_ccd1              STRING

   
   LET v_concepto2 = "D"
   LET v_espacio = "          "
   LET v_pais = "MÉXICO                                  "
   LET v_nom_arch_d = v_ruta_envio CLIPPED ,"/BUCDM",TODAY USING "DDMMYYYY",".bcdm"
   LET v_nombre_cpd = v_ruta_envio CLIPPED ,"/BUCDHDIR",".bcdm"
   LET v_nombre_cpd1 = v_ruta_envio CLIPPED ,"/BUCDHDIR",".txt"
   LET v_arch_ccd = v_ruta_envio CLIPPED ,"/BUCDHDIRCC",".bcdm"
   LET v_arch_ccd1 = v_ruta_envio CLIPPED ,"/BUCDHDIRCC",".txt"
   LET v_constante_d = v_cadena CLIPPED,v_cadena1 CLIPPED,v_concepto2 CLIPPED
   LET v_cp_d = v_ruta_envio CLIPPED ,"/CATADOM",".bcml"
   LET v_total2 = v_ruta_envio CLIPPED ,"/CUENTATOTALD",".txt"


   IF f_inicial IS NOT NULL THEN

    SELECT COUNT (*)
           INTO v_contabiliza_d
           FROM  afi_derechohabiente afi,
                 afi_domicilio dom,
                 cat_cp cp,
                 cat_municipio m,
                 cat_entidad_federativa ef
           WHERE dom.id_derechohabiente = afi.id_derechohabiente
             AND afi.nss <> " "
             AND length(afi.nss) = 11
             AND cp.cp = dom.cp
             AND cp.municipio = m.municipio
             AND m.entidad_federativa =  ef.entidad_federativa
             AND dom.f_actualiza > f_inicial
             AND dom.f_actualiza < TODAY

      IF v_contabiliza_d <> 0 THEN

         LET v_bandera2 = 1
         LET v_query_d = "SELECT afi.nss,
         	                       dom.tpo_domicilio,
                                 dom.calle,
                                 dom.num_exterior,
                                 NVL(dom.num_interior,'                         '),
                                 dom.colonia,
                                 m.municipio_desc,
                                 ef.entidad_desc_larga,",
                                 "'",v_pais,"'",",dom.cp,",TODAY USING "YYYYMMDD","
                            FROM afi_derechohabiente afi,
                                 afi_domicilio dom,
                                 cat_cp cp,
                                 cat_municipio m,
                                 cat_entidad_federativa ef
                           WHERE dom.id_derechohabiente = afi.id_derechohabiente
                             AND afi.nss <> ' '
                             AND length(afi.nss) = 11
                             AND cp.cp = dom.cp
                             AND cp.municipio = m.municipio
                             AND m.entidad_federativa =  ef.entidad_federativa
                             AND dom.f_actualiza >","'",f_inicial,"'","
                             AND dom.f_actualiza < ","'",TODAY,"'"

      ELSE

         LET v_bandera2 = 2
         INSERT INTO afi_ctr_arh_buc VALUES (v_concepto2,TODAY,v_contabiliza_d,g_usuario)

      END IF

      END IF

   IF f_inicial IS NULL THEN

      LET v_bandera2 = 1
      LET v_query_d = "SELECT afi.nss,
      	                      dom.tpo_domicilio,
                              dom.calle,
                              dom.num_exterior,
                              NVL(dom.num_interior,'                         '),
                              dom.colonia,
                              m.municipio_desc,
                              ef.entidad_desc_larga,",
                              "'",v_pais,"'",",dom.cp,",TODAY USING "YYYYMMDD","
                         FROM afi_derechohabiente afi,
                              afi_domicilio dom,
                              cat_cp cp,
                              cat_municipio m,
                              cat_entidad_federativa ef
                        WHERE dom.id_derechohabiente = afi.id_derechohabiente
                          AND afi.nss <> ' '
                          AND length(afi.nss) = 11
                          AND cp.cp = dom.cp
                          AND cp.municipio = m.municipio
                          AND m.entidad_federativa =  ef.entidad_federativa
                          AND dom.f_actualiza < ", "'",TODAY,"'"

   END IF

   IF v_bandera2 = 1 THEN 

      UNLOAD TO  v_nom_arch_d DELIMITER '=' v_query_d

--    se crea comando que elimina pipes
      LET v_s_comando = "sed 's/|//g' ",v_nom_arch_d," > ",v_cp_d
      RUN v_s_comando
--    se crea comando que elimina espacios en blanco
      LET v_s_comando = "sed '/./!d' ",v_cp_d," > ",v_nombre_cpd
      RUN v_s_comando
--    se crea comando que cuenta líneas de archivo
      LET v_cuenta_arh_d ="cat ",v_nombre_cpd,"|wc -l"," > ",v_total2
      RUN v_cuenta_arh_d
      
      LET ch2 = base.Channel.create()
      CALL ch2.openFile(v_total2,"r")
      LET d = ch2.readLine()
      CALL ch2.close()
      LET v_contabiliza_d = d
 
      LET v_query_cuentad = v_constante_d CLIPPED,TODAY USING "YYYYMMDD",v_contabiliza_d USING "&&&&&&&&&&&"
      LET v_cc_dm = v_query_cuentad

      INSERT INTO safre_tmp:tmp_cifras_control (cc_dm) VALUES (v_cc_dm)

      UNLOAD TO v_arch_ccd SELECT cc_dm [1,4],
                                  cc_dm [5,7],
                                  cc_dm [8,8],
                                  cc_dm [9,16],
                                  cc_dm [17,27] FROM safre_tmp:tmp_cifras_control
      WHERE cc_dm IS NOT NULL

      INSERT INTO afi_ctr_arh_buc VALUES (v_concepto2,TODAY,v_contabiliza_d,g_usuario)

      LET v_rm_d = "rm ",v_cp_d
      RUN v_rm_d

      -- se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_nombre_cpd," > ",v_cp_d
      RUN v_s_comando

      -- se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_arch_ccd," > ",v_arch_ccd1
      RUN v_s_comando

      LET v_rm_d = "rm ",v_total2," ",v_nombre_cpd," ",v_arch_ccd
      RUN v_rm_d

      --se crea comando que sustutiye igual por pipes
      LET v_s_comando = "sed 's/=/|/g' ",v_cp_d," > ",v_nombre_cpd1
      RUN v_s_comando
      
      LET v_rm_d = "rm ",v_cp_d
      RUN v_rm_d

   END IF 

END FUNCTION

--*****************************************
--Función que genera archivo "TELÉFONOS"  *
--*****************************************

FUNCTION fn_genera_archivo_t ()

   DEFINE v_query_t                STRING         -- cadena para consulta de información requerida
   DEFINE v_nom_arch_t             STRING         -- ruta y nombre de archivo de salda principal 
   DEFINE v_cp_t                   STRING         -- nombre para copia de archivo
   DEFINE v_constante_t            STRING         -- cadena con concepto para descarga de archivo TELEFONOS
   DEFINE v_arch_cct               STRING         -- ruta y nombre de archivo de cifras control
   DEFINE v_query_cuentat          STRING         -- cadena con infromación para archivo de cifras control
   DEFINE v_concepto3              CHAR(1)        -- concepto que relaciona archivo a información de TELÉFONOS
   DEFINE v_cc_tl                  CHAR(27)      -- vaor ingresado a tabla temporal para archivo de cifras control
   DEFINE v_rm_t                   STRING
   DEFINE v_cuenta_arh_t           STRING
   DEFINE v_total3                 STRING
   DEFINE te                       CHAR(40)
   DEFINE  v_nombre_cpt1           STRING
   DEFINE v_arch_cct1              STRING
   
   LET v_concepto3 = "T"
   LET v_nom_arch_t = v_ruta_envio CLIPPED ,"/BUCTL",TODAY USING "DDMMYYYY",".bctl"
   LET v_nombre_cpt = v_ruta_envio CLIPPED ,"/BUCDHTEL",".bctl"
   LET v_nombre_cpt1 = v_ruta_envio CLIPPED ,"/BUCDHTEL",".txt"
   LET v_arch_cct = v_ruta_envio CLIPPED ,"/BUCDHTELCC",".bctl"
   LET v_arch_cct1 = v_ruta_envio CLIPPED ,"/BUCDHTELCC",".txt"
   LET v_constante_t = v_cadena CLIPPED,v_cadena1 CLIPPED,v_concepto3 CLIPPED
   LET v_cp_t = v_ruta_envio CLIPPED ,"/CATATEL",".bcml"
   LET v_total3 = v_ruta_envio CLIPPED ,"/CUENTATOTALT",".txt"

   IF f_inicial IS NOT NULL THEN

     SELECT COUNT (*)
           INTO v_contabiliza_t
           FROM afi_derechohabiente afi, afi_telefono t,glo_folio g
          WHERE t.id_derechohabiente = afi.id_derechohabiente
            AND t.telefono <> ' '
            AND afi.nss <> ' '
            AND length(afi.nss) = 11
            AND t.folio_lote = g.folio
            AND g.f_actualiza > f_inicial
            AND g.f_actualiza < TODAY

      IF v_contabiliza_t <> 0 THEN

         LET v_bandera3 = 1
         LET v_query_t = " SELECT afi.nss,
      	                          TRIM(NVL(t.tpo_telefono,' ')),
      	                          NVL(t.cve_lada,'   '),
      	                          t.telefono,
      	                          NVL(t.extension,'          '),",TODAY USING "YYYYMMDD","
                             FROM afi_derechohabiente afi, afi_telefono t,glo_folio g
                            WHERE t.id_derechohabiente = afi.id_derechohabiente
                              AND t.telefono <> ' '
                              AND afi.nss <> ' '
                              AND length(afi.nss) = 11
                              AND t.folio_lote = g.folio
                              AND g.f_actualiza >","'",f_inicial,"'","
                              AND g.f_actualiza < ","'",TODAY,"'"

      ELSE

         LET v_bandera3 = 2
         INSERT INTO afi_ctr_arh_buc VALUES (v_concepto3,TODAY,v_contabiliza_t,g_usuario)

      END IF

      END IF
      
    IF f_inicial IS NULL THEN

      LET v_bandera3 = 1
      LET v_query_t = " SELECT afi.nss,
      	                       TRIM(NVL(t.tpo_telefono,' ')),
      	                       NVL(t.cve_lada,'   '),
      	                       t.telefono,
      	                       NVL(t.extension,'          '),",TODAY USING "YYYYMMDD","
                          FROM afi_derechohabiente afi, afi_telefono t,glo_folio g
                         WHERE t.id_derechohabiente = afi.id_derechohabiente
                              AND t.telefono <> ' '
                              AND afi.nss <> ' '
                              AND length(afi.nss) = 11
                              AND t.folio_lote = g.folio
                              AND g.f_actualiza < ","'",TODAY,"'"

   END IF

   IF v_bandera3 = 1 THEN

      UNLOAD TO  v_nom_arch_t DELIMITER '=' v_query_t

-- se crea comando que elimina pipes
      LET v_s_comando = "sed 's/|//g' ",v_nom_arch_t," > ",v_cp_t
      RUN v_s_comando
--    se crea comando que elimina espacios en blanco
      LET v_s_comando = "sed '/./!d' ",v_cp_t," > ",v_nombre_cpt
      RUN v_s_comando
--    se crea comando que cuenta líneas de archivo
      LET v_cuenta_arh_t ="cat ",v_nombre_cpt,"|wc -l"," > ",v_total3
      RUN v_cuenta_arh_t
     
      LET ch2 = base.Channel.create()
      CALL ch2.openFile(v_total3,"r")
      LET te = ch2.readLine()
      CALL ch2.close()
      LET v_contabiliza_t= te

      LET v_query_cuentat = v_constante_t CLIPPED,TODAY USING "YYYYMMDD",v_contabiliza_t USING "&&&&&&&&&&&"
      LET v_cc_tl = v_query_cuentat

      INSERT INTO safre_tmp:tmp_cifras_control (cc_tl) VALUES (v_cc_tl)

      UNLOAD TO v_arch_cct SELECT cc_tl [1,4],
                                  cc_tl [5,7],
                                  cc_tl [8,8],
                                  cc_tl [9,16],
                                  cc_tl [17,27] FROM safre_tmp:tmp_cifras_control
      WHERE cc_tl IS NOT NULL

      INSERT INTO afi_ctr_arh_buc VALUES (v_concepto3,TODAY,v_contabiliza_t,g_usuario)

      LET v_rm_t = "rm ",v_cp_t
      RUN v_rm_t

      -- se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_nombre_cpt," > ",v_cp_t
      RUN v_s_comando
      
      -- se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_arch_cct," > ",v_arch_cct1
      RUN v_s_comando

      LET v_rm_t = "rm ",v_total3," ",v_nombre_cpt," ",v_arch_cct
      RUN v_rm_t

--se crea comando que sustutiye igual por pipes
      LET v_s_comando = "sed 's/=/|/g' ",v_cp_t," > ",v_nombre_cpt1
      RUN v_s_comando
      
      LET v_rm_t = "rm ",v_cp_t
      RUN v_rm_t

   END IF

END FUNCTION

--****************************************
--Función que genera archivo "CORREOS"   *
--****************************************

FUNCTION fn_genera_archivo_c ()

   DEFINE v_query_c                STRING         -- cadena para seleccioanr información requerida
   DEFINE v_nom_arch_c             STRING         -- ruta y nombre de archivo de salida principal
   DEFINE v_cp_c                   STRING         -- cadena con nombre para copia de archivo
   DEFINE v_constante_c            STRING         -- cadena con concepto especificado para contacto electronico
   DEFINE v_arch_ccc               STRING         -- cadena con nombre de archivo para cifras control
   DEFINE v_query_cuentac          STRING         -- cadena con constante y cantidad de registros encontrados
   DEFINE v_concepto4              CHAR(1)        -- concepto que relaciona información a contactos electronicos
   DEFINE v_cc_ce                  CHAR(27)       -- valor ingresado a tabla temporal para archivo de cifras control
   DEFINE v_rm_c                   STRING
   DEFINE v_cuenta_arh_c           STRING
   DEFINE v_total4                 STRING
   DEFINE c                        CHAR(40)
   DEFINE v_nombre_cpc1            STRING
   DEFINE v_arch_ccc1              STRING
   
   LET v_concepto4 = "C" 
   LET v_nom_arch_c = v_ruta_envio CLIPPED ,"/BUCMAIL",TODAY USING "DDMMYYYY",".bcml"
   LET v_nombre_cpc = v_ruta_envio CLIPPED ,"/BUCDHMAIL",".bcml"
   LET v_nombre_cpc1 = v_ruta_envio CLIPPED ,"/BUCDHMAIL",".txt"
   LET v_arch_ccc = v_ruta_envio CLIPPED ,"/BUCDHMAILCC",".bcml"
   LET v_arch_ccc1 = v_ruta_envio CLIPPED ,"/BUCDHMAILCC",".txt"
   LET v_constante_c = v_cadena CLIPPED,v_cadena1 CLIPPED,v_concepto4 CLIPPED
   LET v_cp_c = v_ruta_envio CLIPPED ,"/CATAMAIL",".bcml"
   LET v_total4 = v_ruta_envio CLIPPED ,"/CUENTATOTALC",".txt"

   IF f_inicial IS NOT NULL THEN

     SELECT COUNT (*)
           INTO v_contabiliza_c
           FROM afi_derechohabiente afi,afi_contacto_electronico ce
          WHERE ce.id_derechohabiente = afi.id_derechohabiente
            AND afi.nss <> " "
            AND length(afi.nss) = 11
            AND ce.f_actualiza > f_inicial
            AND ce.f_actualiza < TODAY
            
      IF v_contabiliza_c <> 0 THEN

         LET v_bandera4 = 1

         LET v_query_c = "SELECT afi.nss,
         	                       ce.tpo_correo,
         	                       ce.valor,",TODAY USING "YYYYMMDD","
                            FROM afi_derechohabiente afi,afi_contacto_electronico ce
                           WHERE ce.id_derechohabiente = afi.id_derechohabiente
                            AND afi.nss <> ' '
                            AND length(afi.nss) = 11
                             AND ce.f_actualiza > ","'",f_inicial,"'","
                             AND ce.f_actualiza < ","'",TODAY,"'"

      ELSE

         LET v_bandera4 = 2
         INSERT INTO afi_ctr_arh_buc VALUES (v_concepto4,TODAY,v_contabiliza_c,g_usuario)

      END IF

      END IF 
      
   IF f_inicial IS NULL THEN

      LET v_bandera4 = 1
      LET v_query_c = "SELECT afi.nss,
      	                      ce.tpo_correo,
      	                      ce.valor,",TODAY USING "YYYYMMDD","
                         FROM afi_derechohabiente afi,afi_contacto_electronico ce
                        WHERE ce.id_derechohabiente = afi.id_derechohabiente
                        AND afi.nss <> ' '
                        AND length(afi.nss) = 11
                        AND ce.f_actualiza < ", "'",TODAY,"'"

   END IF

   IF v_bandera4 = 1 THEN

      UNLOAD TO  v_nom_arch_c DELIMITER '=' v_query_c

-- se crea comando que elimina pipes
      LET v_s_comando = "sed 's/|//g' ",v_nom_arch_c," > ",v_cp_c
      RUN v_s_comando
--    se crea comando que elimina espacios en blanco
      LET v_s_comando = "sed '/./!d' ",v_cp_c," > ",v_nombre_cpc
      RUN v_s_comando
--    se crea comando que cuenta líneas de archivo
      LET v_cuenta_arh_c ="cat ",v_nombre_cpc,"|wc -l"," > ",v_total4
      RUN v_cuenta_arh_c
      
      LET ch2 = base.Channel.create()
      CALL ch2.openFile(v_total4,"r")
      LET c = ch2.readLine()
      CALL ch2.close()
      LET v_contabiliza_c = c

      LET v_query_cuentac = v_constante_c CLIPPED,TODAY USING "YYYYMMDD",v_contabiliza_c USING "&&&&&&&&&&&"
      LET v_cc_ce = v_query_cuentac

      INSERT INTO safre_tmp:tmp_cifras_control (cc_ce) VALUES (v_cc_ce)

      UNLOAD TO v_arch_ccc SELECT cc_ce [1,4],
                                  cc_ce [5,7],
                                  cc_ce [8,8],
                                  cc_ce [9,16],
                                  cc_ce [17,27] FROM safre_tmp:tmp_cifras_control
      WHERE cc_ce IS NOT NULL

      INSERT INTO afi_ctr_arh_buc VALUES (v_concepto4,TODAY,v_contabiliza_c,g_usuario)

      LET v_rm_c = "rm ",v_cp_c
      RUN v_rm_c
      
      -- se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_nombre_cpc," > ",v_cp_c
      RUN v_s_comando
      
      -- se crea comando que elimina el último delimitador
      LET v_s_comando = "sed 's/.$//g' ",v_arch_ccc," > ",v_arch_ccc1
      RUN v_s_comando

      LET v_rm_c = "rm ",v_total4," ",v_nombre_cpc," ",v_arch_ccc
      RUN v_rm_c

      --se crea comando que sustutiye igual por pipes
      LET v_s_comando = "sed 's/=/|/g' ",v_cp_c," > ",v_nombre_cpc1
      RUN v_s_comando
      
      LET v_rm_c = "rm ",v_cp_c
      RUN v_rm_c


   END IF

END FUNCTION

--*****************************************************
--Función que genera archivo "RELACIONES LABORALES"   *
--*****************************************************

FUNCTION fn_genera_archivo_r ()

   DEFINE v_query_cuentar          STRING         -- Variable para generar cadena de cifras control
   DEFINE v_cuenta_r               INTEGER        -- variable que cuenta registros de relación laboral en tabla
   DEFINE v_nom_arch_r             STRING         -- cadena con ruta y nombre de archivo principal de salida
   DEFINE v_cp_r                   STRING         -- variable con cadena para cipoa de archivo pincipal
   DEFINE v_constante_r            STRING         -- cadena que incluye concepto para información de archivo
   DEFINE v_arch_ccr               STRING         -- ruta y nombre de archivo de cifras control
   DEFINE v_concepto5              CHAR(1)        -- concepto manejado para archivo de relación laboral
   DEFINE v_cc_rl                  CHAR(27)      -- cadena de cifras control para tabla temporal
   DEFINE v_rm_r                   STRING
   DEFINE v_cuenta_arh_r           STRING
   DEFINE v_total5                 STRING
   DEFINE r                        CHAR(40)
   DEFINE v_s_Txt                  STRING
   DEFINE v_query_rl               STRING
   DEFINE v_nombre_cpr1            STRING
   DEFINE v_arch_ccr1              STRING

    LET v_concepto5 = "R"
   LET v_nom_arch_r = v_ruta_envio CLIPPED ,"/BUCRL",TODAY USING "YYYYMMDD",".bcrl"
   LET v_nombre_cpr = v_ruta_envio CLIPPED ,"/BUCDHCATAREL",".bcrl"
   LET v_nombre_cpr1 = v_ruta_envio CLIPPED ,"/BUCDHCATAREL",".txt"
   LET v_arch_ccr = v_ruta_envio CLIPPED ,"/BUCDHCATARELCC",".bcrl"
   LET v_arch_ccr1 = v_ruta_envio CLIPPED ,"/BUCDHCATARELCC",".txt"
   LET v_constante_r = v_cadena CLIPPED,v_cadena1 CLIPPED,v_concepto5 CLIPPED
   LET v_cp_r = v_ruta_envio CLIPPED ,"/CATAREL",".bcrl"
   LET v_total5 = v_ruta_envio CLIPPED ,"/CUENTATOTALR",".txt"


   SELECT COUNT (*)
     INTO v_cuenta_r
     FROM afi_relacion_laboral
     
   IF v_cuenta_r <> 0 THEN

      LET v_bandera5 = 1
      LET v_query_rl = "SELECT afi_derechohabiente.nss,
                                     afi_relacion_laboral.nrp,
                                     afi_relacion_laboral.ind_relacion,
                                     lpad(YEAR(afi_relacion_laboral.f_alta_nrp),4,0)||
                                     lpad (MONTH(afi_relacion_laboral.f_alta_nrp),2,0)||
                                     lpad (DAY(afi_relacion_laboral.f_alta_nrp),2,0),",TODAY USING "YYYYMMDD","
                                FROM afi_derechohabiente,afi_relacion_laboral
                               WHERE afi_relacion_laboral.id_derechohabiente = afi_derechohabiente.id_derechohabiente
                               AND afi_derechohabiente.nss <> ' '
                               AND length(afi_derechohabiente.nss) = 11"

      UNLOAD TO  v_nom_arch_r DELIMITER '=' v_query_rl

-- se crea comando que elimina pipes
     LET v_s_comando = "sed 's/|//g' ",v_nom_arch_r," > ",v_cp_r
     RUN v_s_comando
   --RUN v_s_comando
-- se crea comando que elimina espacios en blanco
     LET v_s_comando = "sed '/./!d' ",v_cp_r," > ",v_nombre_cpr
     RUN v_s_comando
--   se crea comando que cuenta líneas de archivo
     LET v_cuenta_arh_r ="cat ",v_nombre_cpr,"|wc -l"," > ",v_total5
     RUN v_cuenta_arh_r
     
     LET v_s_Txt = "unix2dos "||" "||v_ruta_envio CLIPPED||" "||"BUCDHCATAREL.txt"
     RUN v_s_Txt
     
     LET ch2 = base.Channel.create()
     CALL ch2.openFile(v_total5,"r")
     LET r = ch2.readLine()
     CALL ch2.close()
     LET v_contabiliza_r = r
     
     LET v_query_cuentar = v_constante_r CLIPPED,TODAY USING "YYYYMMDD",v_contabiliza_r USING "&&&&&&&&&&&"
     LET v_cc_rl = v_query_cuentar
     
     INSERT INTO safre_tmp:tmp_cifras_control (cc_rl) VALUES (v_cc_rl)
     
     UNLOAD TO v_arch_ccr SELECT cc_rl [1,4],
                                 cc_rl [5,7],
                                 cc_rl [8,8],
                                 cc_rl [9,16],
                                 cc_rl [17,27] FROM safre_tmp:tmp_cifras_control
     WHERE cc_rl IS NOT NULL 
     
     INSERT INTO afi_ctr_arh_buc VALUES (v_concepto5,TODAY,v_contabiliza_r,g_usuario)

     LET v_rm_r = "rm ",v_cp_r
     RUN v_rm_r
     
  -- se crea comando que elimina el último delimitador
     LET v_s_comando = "sed 's/.$//g' ",v_nombre_cpr," > ",v_cp_r
     RUN v_s_comando
     
     -- se crea comando que elimina el último delimitador
     LET v_s_comando = "sed 's/.$//g' ",v_arch_ccr," > ",v_arch_ccr1
     RUN v_s_comando
     
     LET v_rm_r = "rm ",v_total5," ",v_nombre_cpr," ",v_arch_ccr
     RUN v_rm_r

     --se crea comando que sustutiye igual por pipes
     LET v_s_comando = "sed 's/=/|/g' ",v_cp_r," > ",v_nombre_cpr1
     RUN v_s_comando
      
     LET v_rm_r = "rm ",v_cp_r
     RUN v_rm_r

   ELSE

      LET v_bandera5 = 2 

   END IF

END FUNCTION

--***********************************************
-- Función que permite borrar tablas temporales *
--***********************************************
FUNCTION fn_tablas_temporales()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_cifras_control
      DROP TABLE tmp_cadena_saci
      

   WHENEVER ERROR STOP

   -- se crea tabla temporal para guardar registros de cifras control
      CREATE TEMP TABLE tmp_cifras_control ( cc_dh CHAR(27),
                                             cc_dm CHAR(27),
                                             cc_tl CHAR(27),
                                             cc_ce CHAR(27),
                                             cc_rl CHAR(27))

      CREATE TABLE tmp_cadena_saci (cadena CHAR(8))

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

   LET v_reporte = v_ruta_bin CLIPPED,"/AFIP17.4rp"

-- ruta para guardar el reporte
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        g_usuario CLIPPED , "-", -- usuario
                        "AFIP17", "-", -- programa
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
      OUTPUT TO REPORT rep_resultados(v_contabiliza_k,v_contabiliza_d,v_contabiliza_t,v_contabiliza_c,v_contabiliza_r)
-- Finaliza reporte

      FINISH REPORT rep_resultados

   END IF

END FUNCTION

--******************************************
-- Se cachan datos para generar el reporte *
--******************************************
REPORT rep_resultados(v_contabiliza_k,v_contabiliza_d,v_contabiliza_t,v_contabiliza_c,v_contabiliza_r)


   DEFINE v_fecha_reporte          DATE
   DEFINE v_usuario                CHAR (20)
   DEFINE v_contabiliza_k          INTEGER
   DEFINE v_contabiliza_d          INTEGER
   DEFINE v_contabiliza_t          INTEGER
   DEFINE v_contabiliza_c          INTEGER
   DEFINE v_contabiliza_r          INTEGER

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
      PRINTX v_contabiliza_d
      PRINTX v_contabiliza_t
      PRINTX v_contabiliza_c
      PRINTX v_contabiliza_r

END REPORT