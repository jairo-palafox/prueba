--=============================================================================
##################################################################################################
#Módulo       => AGRC                                                                            #
#Programa     => AGRC12                                                                          #
#Objetivo     => Consulta de la interacción del WebService de marca y desmarca por NSS           #
#Autor        => José Eduardo Ventura                                                            #
#Fecha inicio =>                                                                                 #
##################################################################################################

DATABASE safre_viv

GLOBALS

   DEFINE v_condicion         STRING
   DEFINE v_nss               STRING
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod -- clave del usuario
   DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutará el programa
   DEFINE p_s_titulo          STRING -- título de la ventana
   DEFINE nss                 INTEGER

END GLOBALS

MAIN

   -- se recupera la clave de usuario desde parámetro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".AGRC12.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW AGRC12 WITH FORM "AGRC121"
   DIALOG ATTRIBUTES(UNBUFFERED)

   CONSTRUCT v_condicion ON nss
                       FROM nss

      ON ACTION ACCEPT
         LET v_nss = GET_FLDBUF(nss)

         IF v_nss IS NULL THEN
            CALL fn_mensaje ("AVISO","No se ingresó NSS para consulta ","stop")
            NEXT FIELD nss
         ELSE
            CALL fn_valida_numero()
      CLEAR FORM
         END IF

      ON ACTION CLOSE

         EXIT DIALOG

   END CONSTRUCT

   END DIALOG

   CLOSE WINDOW AGRC12

END MAIN

--función principal para consulta por NSS
FUNCTION fn_consulta_nss_ws()

   DEFINE v_query          STRING
   DEFINE v_total          INTEGER
   DEFINE i                INTEGER
   DEFINE p_nss            CHAR(11)

   DEFINE arr_his_ws DYNAMIC ARRAY OF RECORD
      nss                  CHAR(11),
      tpo_credito          SMALLINT,
      num_credito          DECIMAL(10,0),
      marca_procesar       CHAR(2),
      situacion            SMALLINT,
      f_actualiza          DATE,
      diagnostico          CHAR(3)
   END RECORD

   OPEN WINDOW AGRC122 WITH FORM "AGRC122"

   CALL fn_crea_tablas_temporales()

   --se cargan datos en una tabla temporal
   LET v_query = " SELECT nss, id_derechohabiente ",
                 " FROM afi_derechohabiente ",
                 " WHERE " , v_condicion , " INTO TEMP tmp_lista_nss"

   PREPARE prp_query FROM v_query
   EXECUTE prp_query

        --se valida que existan registros para nss
   SELECT COUNT(*)
     INTO v_total
     FROM tmp_lista_nss

        --si existen registros, se realiza la consulta
   IF v_total <> 0 THEN
      SELECT afi.nss,
            his.tpo_credito,
            his.num_credito,
            his.marca_procesar,
            his.situacion,
            his.f_actualiza,
            his.diagnostico
      FROM cta_his_marca_ws his, tmp_lista_nss afi
      WHERE his.id_derechohabiente = afi.id_derechohabiente
      INTO TEMP nss_historica

      INSERT INTO nss_historica
         SELECT afi.nss,
                cre.tpo_credito,
                cre.num_credito,
                cre.marca_procesar,
                cre.situacion,
                cre.f_solicita,
                cre.diagnostico
         FROM cta_marca_ws cre, tmp_lista_nss afi
         WHERE cre.id_derechohabiente = afi.id_derechohabiente

      DECLARE cur_his_marca CURSOR FOR SELECT * FROM nss_historica 
      ORDER BY f_actualiza DESC

      LET i = 1

      FOREACH cur_his_marca INTO arr_his_ws[i].*
         LET p_nss = arr_his_ws[i].nss
         LET i = i + 1
      END FOREACH

      IF p_nss IS NULL THEN
         CALL fn_mensaje ("AVISO","No hay registros de marcaje ligados al NSS","stop")
      ELSE
         --se muestran datos de consultas en tabla
         DISPLAY ARRAY arr_his_ws TO datos1.* ATTRIBUTES (ACCEPT = FALSE)

            ON ACTION CANCEL
               EXIT DISPLAY
         END DISPLAY
      END IF
   ELSE
      --si no existen registros, se manda mensaje de advertencia  
      CALL fn_mensaje ("AVISO","El NSS no se encuentra en el catálogo de derechohabientes","stop")
      END IF

   CLOSE WINDOW AGRC122

END FUNCTION
--función que borra tablas temporales
FUNCTION fn_crea_tablas_temporales()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_lista_nss
      DROP TABLE nss_historica
   WHENEVER ERROR STOP

END FUNCTION

--función que valida que el NSS ingresado sea un valor numerico
FUNCTION fn_valida_numero ()

   DEFINE v_funcion           STRING  --variable para función fn_es_numero()
   DEFINE v_cadena            CHAR(1) --cadena de valores ingresados a NSS
   DEFINE a                   INTEGER 
   DEFINE r_valida            SMALLINT--valor regresado de la función fn_es_numero(), 0 ó 1

   LET v_funcion = "EXECUTE FUNCTION fn_es_numero (?)"

   PREPARE prp_es_numero FROM v_funcion
      FOR a=1 TO v_nss.getLength()
         LET v_cadena = v_nss.getCharAt(a)
   EXECUTE prp_es_numero USING v_cadena INTO r_valida

   IF r_valida =1 THEN
      CALL fn_consulta_nss_ws()--función que realiza consulta por NSS
      EXIT FOR
   ELSE
      CALL fn_mensaje ("AVISO","NSS no es un valor numerico","stop")
      EXIT  FOR
   END IF
      END FOR

END FUNCTION 
