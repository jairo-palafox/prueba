################################################################################
#Proyecto          => SACI                                                     #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Módulo            => CAT                                                      #
#Programa          => CATM17                                                   #
#Objetivo          => Consultar trámites de créditos cofinanciados             #
#Autor             => Edgar Damian Estrada Rivera, EFP                         #
#Fecha Inicio      => 23 de Diciembre de 2017                                  #
################################################################################

DATABASE safre_viv

   DEFINE g_usuario               LIKE seg_usuario.usuario  -- usuario firmado al sistema
   DEFINE g_tipo_carga            SMALLINT  -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE g_nom_prog              VARCHAR(30)  -- nombre del programa
   DEFINE v_e_nss                 CHAR(11)
   DEFINE var_id_derechohabiente  DECIMAL (9,0)
   DEFINE v_respuesta             SMALLINT
   DEFINE v_mensaje               STRING 
   DEFINE v_aux_tiempo            CHAR(8) --para la función fn_extractor_trámite
   DEFINE v_tiempo                CHAR(6) --para la función fn_extractor_trámite

   DEFINE arr_cre_tramite DYNAMIC ARRAY OF RECORD
      nss                         CHAR(11),
      id_cre_tramite              DECIMAL(9,0),
      num_credito                 DECIMAL(10,0),
      estado_desc                 CHAR(30)
   END RECORD

   DEFINE arr_his_tramite DYNAMIC ARRAY OF RECORD
      nss                         CHAR(11),
      num_credito                 DECIMAL(10,0),
      estado_desc                 CHAR(30),
      diagnostico                 CHAR(4),
      diagnostico_desc            CHAR(100),
      f_vigencia                  DATE 
   END RECORD

MAIN

   -- se asignan los parámetros que vienen del fglrun
   LET g_usuario      =   ARG_VAL(1)
   LET g_tipo_carga   =   ARG_VAL(2)
   LET g_nom_prog     =   ARG_VAL(3)

   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF

   CLOSE WINDOW SCREEN
    
   -- se inicia el LOG
   CALL STARTLOG(g_usuario CLIPPED||'.CATM17.log')
   CALL fn_proceso_principal()

END MAIN

-- Función principal del programa
FUNCTION fn_proceso_principal()

   DEFINE v_ind_num    BOOLEAN

   -- se abre la ventana de consulta
   OPEN WINDOW w1 WITH FORM "CATM171"

     INPUT BY NAME v_e_nss ATTRIBUTE(UNBUFFERED,CANCEL = FALSE, ACCEPT =FALSE)
     
        ON ACTION ACEPTAR 
           LET var_id_derechohabiente = NULL
           IF v_e_nss IS NULL THEN
              CALL fn_mensaje(" ","El NSS no puede ser nulo"," ")
              NEXT FIELD v_e_nss
           END IF
           IF LENGTH(v_e_nss) < 11 THEN 
              CALL fn_mensaje(" ","El NSS debe ser de 11 dígitos"," ")
              NEXT FIELD v_e_nss
           END IF
           LET v_ind_num = fn_es_numerico(v_e_nss)
           IF v_ind_num = 1 THEN 
              CALL fn_mensaje(" ","El NSS debe ser númerico"," ")
               NEXT FIELD v_e_nss
           END IF 

           -- se recupera ID_DERECHOHABIENTE de acuerdo al NSS buscado.
           SELECT id_derechohabiente
             INTO var_id_derechohabiente
             FROM afi_derechohabiente
            WHERE nss = v_e_nss

           IF var_id_derechohabiente IS NULL THEN 
              CALL fn_mensaje (" ","El NSS ingresado no existe en la base de derechohabientes"," ")
              NEXT FIELD v_e_nss
           ELSE 
              --llama la función de detalle
              CALL fn_detalle_tramite()   
           END IF

        ON ACTION CANCELAR 
           EXIT INPUT

         ON ACTION extractor
         LET v_respuesta = fn_ventana_confirma("","¿Desea generar el archivo de salida?","")

         IF(v_respuesta = 0) THEN
            CALL fn_mensaje("","Se ha cancelado la operación","") 
         ELSE 
           -- Ejecuta funcion que genera archivo de salida
           CALL fn_extractor_tramite()

           LET v_mensaje = "Se ha generado el archivo de salida con nombre : T",TODAY USING "yyyymmdd_",v_tiempo,".vtmt \n y se encuentra en la ruta /safreviv_int/agr/envio"
              CALL fn_mensaje("",v_mensaje,"")
        END IF 

     END INPUT 
           
   CLOSE WINDOW w1 
   
END FUNCTION

FUNCTION fn_es_numerico(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN
   
   LET p_cadena = p_cadena CLIPPED
   
   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE
         LET indicador = 1
         EXIT FOR
      END IF
   END FOR

   RETURN indicador

END FUNCTION

FUNCTION fn_detalle_tramite()

   DEFINE v_pos  SMALLINT

   CALL carga_tramite_credito()
   
   OPEN WINDOW w2 WITH FORM "CATM172"

   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY arr_cre_tramite TO record3.*
         BEFORE DISPLAY 
         IF arr_cre_tramite.getLength() = 0 THEN 
            CALL fn_mensaje(" ","No existen tramites para el NSS ingresado"," ")
            EXIT DIALOG 
         END IF 
         
         BEFORE ROW 
            LET v_pos = ARR_CURR() 
            CALL carga_his_credito(arr_cre_tramite[v_pos].id_cre_tramite)         
       END DISPLAY 

       DISPLAY ARRAY arr_his_tramite TO record2.*
       END DISPLAY
      
       ON ACTION Nuevo
          LET v_e_nss = NULL
          EXIT DIALOG 
         
      
       ON ACTION salir  
          EXIT PROGRAM

   END DIALOG

   CLOSE WINDOW w2

END FUNCTION 

FUNCTION carga_tramite_credito()

   DEFINE v_query    STRING
   DEFINE i          INTEGER

   LET v_query = "SELECT a.nss, 
                         c.id_cre_tramite,
                         c.num_credito, 
                         t.estado_desc
                    FROM afi_derechohabiente a,
                         cre_tramite c, 
                         cat_maq_credito t
                   WHERE a.id_derechohabiente = ", var_id_derechohabiente,
                   " AND a.id_derechohabiente = c.id_derechohabiente
                     AND c.estado             = t.estado"

   PREPARE prp_cre_tramite FROM v_query
   DECLARE cur_cre_tramite CURSOR FOR prp_cre_tramite

   LET i = 1
   CALL arr_cre_tramite.clear()

   FOREACH cur_cre_tramite INTO arr_cre_tramite[i].*
      LET i = i + 1
   END FOREACH

   IF arr_cre_tramite[arr_cre_tramite.getLength()].nss IS NULL AND 
      arr_cre_tramite[arr_cre_tramite.getLength()].id_cre_tramite IS NULL THEN 
      CALL arr_cre_tramite.deleteElement(arr_cre_tramite.getLength())
   END IF 

END FUNCTION 

FUNCTION carga_his_credito(p_id_tramite)

   DEFINE p_id_tramite       DECIMAL(9,0)
   DEFINE v_cadena           STRING 
   DEFINE k                  INTEGER
   DEFINE long_diagnostico   INTEGER
   DEFINE ini_diagnostico    CHAR(1)

   LET v_cadena = "SELECT a.nss,
                          h.num_credito,
                          m.estado_desc,
                          h.diagnostico,
                          '',
                          h.f_vigencia
                     FROM cre_his_tramite h,
                          cre_tramite c,
                          afi_derechohabiente a,
                          cat_maq_credito m
                    WHERE h.id_cre_tramite = ",p_id_tramite,
                      "AND h.id_cre_tramite = c.id_cre_tramite
                      AND c.id_derechohabiente = a.id_derechohabiente
                      AND h.estado = m.estado
                    ORDER BY h.f_vigencia desc;"

   PREPARE prp_his_tramite FROM v_cadena
   DECLARE cur_his_tramite CURSOR FOR prp_his_tramite

   LET k = 1
   CALL arr_his_tramite.clear()

   FOREACH cur_his_tramite INTO arr_his_tramite[k].*

   LET long_diagnostico = LENGTH (arr_his_tramite[k].diagnostico)
   LET ini_diagnostico  = arr_his_tramite[k].diagnostico[1,1]

      CALL fn_desc_diagnostico(long_diagnostico,
                               ini_diagnostico,
                               arr_his_tramite[k].diagnostico) 
        RETURNING arr_his_tramite[k].diagnostico_desc
           
      LET k = k + 1
   END FOREACH
   
END FUNCTION

FUNCTION fn_desc_diagnostico(p_long_diagnostico,
                             p_ini_diagnostico,
                             p_caracter_diagnostico)

DEFINE p_long_diagnostico       INTEGER
DEFINE p_ini_diagnostico        CHAR(1)
DEFINE descripcion_diagnostico  CHAR(100)
DEFINE p_caracter_diagnostico   CHAR(4)
DEFINE v_almacena               CHAR(3)

INITIALIZE v_almacena TO NULL  

   CASE p_long_diagnostico 
      WHEN 3 
         CASE  p_caracter_diagnostico
           WHEN "208"
              LET descripcion_diagnostico =  "CUENTA SALDO CERO"
           WHEN "211" 
              LET descripcion_diagnostico =  "TRABAJADOR NO EXISTE"
           WHEN "212"
              LET descripcion_diagnostico =  "CUENTA CON CRÉDITO VIGENTE"
           WHEN "225" 
             LET descripcion_diagnostico =  "NUMERO DE CREDITO NO EXISTE"
           WHEN "229" 
             LET descripcion_diagnostico =  "REGISTRO SIN TRÁMITE DE INSCRIPCIÓN"
           WHEN "230" 
             LET descripcion_diagnostico =  "REGISTRO NO ESTÁ MARCADO"
           WHEN "234"
             LET descripcion_diagnostico =  "MARCA PROCEDENTE"
           WHEN "3-1"
             LET descripcion_diagnostico = "ERROR DE CONEXIÓN CON PROCESAR"
         END CASE 
       WHEN 4
          CASE p_ini_diagnostico
             WHEN 2 
                CASE p_caracter_diagnostico
                   WHEN "2000"
                      LET descripcion_diagnostico = "CUENTA LIBRE"
                   WHEN "2213"
                      LET descripcion_diagnostico = "MARCA CRÉDITO EN TRÁMITE"
                   WHEN "2234"
                      LET descripcion_diagnostico = "MARCA PROCEDENTE"
                   OTHERWISE
                    LET v_almacena = p_caracter_diagnostico[2,4] 
                    DISPLAY 'v_almacena ',v_almacena
                       SELECT rch_desc
                         INTO descripcion_diagnostico
                         FROM cat_rch_marca
                        WHERE rch_cod = v_almacena;
                 END CASE
             WHEN 3
                CASE p_caracter_diagnostico
                   WHEN "3000"
                     LET descripcion_diagnostico = "MARCA PROCEDENTE"
                   OTHERWISE
                     LET v_almacena = p_caracter_diagnostico[2,4]
                       SELECT desc_rechazo
                         INTO descripcion_diagnostico
                          FROM cat_rechazo
                         WHERE tpo_rechazo = "RCH"
                           AND cod_rechazo = v_almacena;
                 END CASE
              END CASE 
    END CASE

RETURN descripcion_diagnostico

END FUNCTION  

FUNCTION fn_extractor_tramite()

   DEFINE v_query_extract    STRING
   DEFINE v_ruta_envio       CHAR(40)
   DEFINE v_arh_salida       STRING
   DEFINE ch                 base.Channel
   DEFINE v_detalle          STRING
   DEFINE cont               INTEGER

   DEFINE rec_nss RECORD
      nss                    CHAR(11),
      f_vigencia             DATE
   END RECORD

   --Se obtiene la ruta donde se dejará el archivo generado
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

    LET v_aux_tiempo =  TIME (CURRENT)
    LET v_tiempo = v_aux_tiempo[1,2],v_aux_tiempo[4,5],v_aux_tiempo[7,8]
    
   -- crea nombre del archivo de salida
   LET v_arh_salida = v_ruta_envio CLIPPED,"/T",TODAY USING "yyyymmdd","_",v_tiempo,".vtmt"
   LET ch = base.Channel.create() #Se crea el objeto base.channel
   LET v_detalle = NULL

   -- extracción de tramites
   LET v_query_extract = "SELECT a.nss, 
                                 max(h.f_vigencia)
                            FROM afi_derechohabiente a,
                                 cre_tramite t, 
                                 cre_his_tramite h
                            WHERE a.id_derechohabiente = t.id_derechohabiente
                              AND t.estado = 18
                              AND t.id_cre_tramite = h.id_cre_tramite
                           GROUP BY 1"

   CALL ch.openFile(v_arh_salida,"w")

   PREPARE prp_extrac FROM v_query_extract
   DECLARE  crs_extrac CURSOR FOR prp_extrac

   LET cont = 1
   INITIALIZE rec_nss.* TO NULL
   LET v_detalle = NULL 

   FOREACH crs_extrac INTO rec_nss.*

      LET v_detalle = rec_nss.nss,"|",rec_nss.f_vigencia,"|"
      CALL ch.writeLine(v_detalle)
      LET cont = cont + 1

   END FOREACH 

   IF(cont = 1) THEN
      LET v_detalle = "No se encontraron tramites con el NSS ingresado"
      CALL ch.writeLine(v_detalle) 
   END IF 

   CALL ch.close()

END FUNCTION 