--===============================================================

##########################################################################
#Módulo       => ACR                                                     #
#Programa     => ACRP39                                                  #
#Objetivo     => Liquidar deudores                                       #
#Autor        => Jose Eduardo Ventura                                    #
#Fecha inicio =>                                                         #
##########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE cant                     INTEGER       -- Cantidad de digitos ingresados en NSS
   DEFINE cant_cre                 INTEGER       -- Cantidad de digitos ingresados en número de credito
   DEFINE p_id_derechohabiente     DECIMAL(9,0)  -- Valor de id_derechohabiente para NSS consultado
   DEFINE v_nss                    CHAR(11)      -- NSS ingresado para consulta
   DEFINE v_num_credito            DECIMAL(10,0) -- Número de credito ingresado para consulta
   DEFINE v_monto                  DECIMAL(12,2) -- Monto ingresado para consulta
   DEFINE i                        INTEGER       -- constante para secuencia de tabla credito
   DEFINE v_salir                  INTEGER       -- Variable para salir de la consulta
   DEFINE v_total                  INTEGER       -- Valor de variable que almacena la cuenta de registros existentes
   DEFINE p_usuario                CHAR(20)      -- Obtiene dato de usuario
   DEFINE p_tipo_ejecucion         SMALLINT      -- Forma como ejecutará el programa
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE v_id_cre_acreditado      INTEGER

--Arreglo para llenar datos de tabla_1 (información de credito)
   DEFINE arr_tabla_credito        DYNAMIC ARRAY OF RECORD
          id_cre_acreditado        DECIMAL(9,0),
          tipo_credito             CHAR(40),
          numero_credito           DECIMAL(10,0),
          estado_infonavit         CHAR(40),
          estado_procesar          CHAR(40),
          fecha_otorga             DATE,
          saldo_deudor             DECIMAL(12,2),
          edo_infonavit_clave      INTEGER,
          edo_procesar_clave       INTEGER,
          checkbox                 SMALLINT
   END RECORD

--Arreglo para llenar datos de tabla_2 (información de saldo)
   DEFINE arr_tabla_saldo          DYNAMIC ARRAY OF RECORD
          subcuenta                CHAR(40),
          aivs                     DECIMAL(18,6),
          pesos                    DECIMAL(18,2)
   END RECORD

--Arreglo para llenar datos de tabla_3 (información deudor)
   DEFINE arr_tabla_deudor         DYNAMIC ARRAY OF RECORD
          folio_referencia         DECIMAL(9,0),
          movimiento               CHAR(40),
          fecha_movimiento         DATE,
          monto_aivs               DECIMAL(18,6),
          monto_pesos              DECIMAL(18,2),
          fecha_proceso            DATE
   END RECORD

END GLOBALS

MAIN

   -- se recupera la clave de usuario desde parámetro
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".ACRP39.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

-- Abre ventana para consulta por NSS y monto

   OPEN WINDOW consulta_acreditado WITH FORM "ACRP391"

   INPUT BY NAME v_nss, v_num_credito, v_monto ATTRIBUTES (UNBUFFERED)

      ON ACTION ACCEPT
         LET v_salir = 0

         CALL fn_validaciones ()

         IF v_salir = 1 THEN
            CALL fn_mensaje("Aviso","No existen datos relacionados con los datos ingresados","info")
            NEXT FIELD v_nss
         END IF

         IF v_salir = 2 THEN
            EXIT INPUT
         END IF

         LET v_nss         = ''
         LET v_monto       = ''
         LET v_num_credito = ''

         DISPLAY BY NAME v_nss,v_num_credito, v_monto

      ON ACTION CANCEL
         EXIT INPUT

    END INPUT

END MAIN

--Funcion que realiza validaciones a datos ingresados
FUNCTION fn_validaciones ()
   
   --validaciones incluyentes
   IF v_nss IS NULL AND v_monto IS NULL AND v_num_credito IS NULL THEN
      CALL fn_mensaje("Aviso","Se debe capturar el NSS, monto a liquidar y número de credito","info")
   END IF

   --validaciones para NSS
   IF v_nss IS NULL AND v_monto IS NOT NULL THEN
      CALL fn_mensaje("Aviso","NSS nulo ó vacío, se debe ingresar un NSS válido","info")
   END IF

   LET cant = LENGTH (v_nss)

   IF v_nss IS NOT NULL AND cant < 11 THEN
      CALL fn_mensaje("Aviso","NSS incompleto, se debe ingresar un NSS correcto","info")
   END IF

   --validaciones para número de credito
     LET cant_cre = LENGTH (v_num_credito)
     
   IF v_nss IS NOT NULL AND v_num_credito IS NULL THEN 
      CALL fn_mensaje("Aviso","Número de credito nulo ó vacío, se debe ingresar un número de crédito válido","info")
   END IF

   IF v_nss IS NOT NULL AND cant = 11 AND v_num_credito IS NOT NULL AND cant_cre > 10 THEN 
      CALL fn_mensaje("Aviso","Número de crédito incorrecto, debe ser un máximo de 10 dígitos","info")
   END IF

   --validaciones para saldo
   IF cant = 11 AND cant_cre < 11 AND v_monto = 0 THEN
      CALL fn_mensaje("Aviso","El monto no puede ser 0, el saldo a liquidar debe ser mayor a 0","info")
   END IF

   IF cant = 11 AND cant_cre < 11 AND v_monto IS NULL THEN
      CALL fn_mensaje("Aviso","Monto no puede ser nulo, el saldo a liquidar debe ser mayor a 0","info")
   END IF

   IF v_monto < 0 AND cant = 11 THEN
      CALL fn_mensaje("Aviso","El monto no puede ser negativo, el saldo a liquidar debe ser mayor a 0","info")
   END IF

   -- Si valores de NSS y monto son correctos, se llama a función que realiza consulta
   IF cant = 11 AND cant_cre < 11 AND v_monto > 0 THEN
      CALL fn_valida_numero_nss()
   END IF

END FUNCTION

--Función para consulta por NSS y Monto
FUNCTION fn_consulta_nss_monto ()

   OPEN WINDOW registros WITH FORM "ACRP392"

-- Función que borra tablas temporales
      CALL fn_crea_tablas_temporales()

-- Se buscan registros relacionados a los valores ingresados

      SELECT id_cre_acreditado
      FROM  cre_acreditado c,
            afi_derechohabiente a,
            cat_maq_credito m,
            cat_tipo_credito t
      WHERE c.id_derechohabiente= a.id_derechohabiente
        AND c.tpo_originacion in(1,4)
        AND c.estado = m.estado
        AND m.entidad = 1
        AND c.tpo_credito = t.tpo_credito
        AND a.nss = v_nss
        AND c.num_credito = v_num_credito
     INTO TEMP tmp_id_cre_acreditado

--se valida que existan registros para datos ingresados

   SELECT COUNT (*)
      INTO v_total
      FROM tmp_id_cre_acreditado

      --IF {cant = 11 AND cant_cre = 10 AND v_monto > 0 AND }v_total = 0 THEN
      IF v_total = 0 THEN
         LET v_salir =1
      END IF

      IF v_total <> 0 THEN
      --Se llama a funciones que llenan datos requeridos
          CALL fn_datos_personales()
          CALL fn_tabla_credito()
          CALL fn_tabla_saldo_ssv()
          CALL fn_despliega_datos ()
      END IF

   CLOSE WINDOW registros

END FUNCTION

--Función que obtiene datos personales para NSS ingresado
FUNCTION fn_datos_personales()

  -- Valores que regresa la consulta por NSS y monto
   DEFINE p_nombre              CHAR(40)
   DEFINE p_curp                CHAR(18)
   DEFINE p_rfc                 CHAR(13)
   DEFINE p_ap_paterno          CHAR(40)
   DEFINE p_ap_materno          CHAR(40)

 -- Variables para llenar campos de datos personales
   DEFINE v_nombre              CHAR(40)
   DEFINE nss                   CHAR(11)
   DEFINE v_curp                CHAR(18)
   DEFINE v_rfc                 CHAR(13)
   DEFINE v_ap_paterno          CHAR(40)
   DEFINE v_ap_materno          CHAR(40)

   SELECT id_derechohabiente,
          curp,
          rfc,
          ap_paterno_af,
          ap_materno_af,
          nombre_af
     INTO p_id_derechohabiente,
          p_curp,
          p_rfc,
          p_ap_paterno,
          p_ap_materno,
          p_nombre  
     FROM afi_derechohabiente
    WHERE afi_derechohabiente.nss = v_nss

   LET nss                  = v_nss
   LET v_curp               = p_curp
   LET v_rfc                = p_rfc
   LET v_ap_paterno         = p_ap_paterno
   LET v_ap_materno         = p_ap_materno
   LET v_nombre             = p_nombre

   DISPLAY BY NAME nss,
                   v_curp,
                   v_rfc,
                   v_ap_paterno,
                   v_ap_materno,
                   v_nombre

END FUNCTION

-- Funcion para obtener datos de la tabla credito
FUNCTION fn_tabla_credito()

   DEFINE v_query                  STRING

      LET v_query = 
        "SELECT  c.id_cre_acreditado,
                 t.desc_credito,
                 c.num_credito,
                 m.estado_desc,
                 q.estado_desc,
                 c.f_otorga,
                 c.sdo_deudor,
                 c.estado,
                 c.edo_procesar
         FROM    cre_acreditado c,
                 cat_maq_credito m,
                 cat_maq_credito q,
                 cat_tipo_credito t
         WHERE   c.id_derechohabiente = ",p_id_derechohabiente,
           " AND c.tpo_originacion in(1,4)
             AND c.estado = m.estado
             AND c.edo_procesar = q.estado
             AND c.tpo_credito = t.tpo_credito
             AND c.num_credito = ",v_num_credito,
           " AND c.tpo_originacion = t.tpo_originacion
             AND m.entidad = 1 "

      PREPARE prp_tab_credito FROM v_query
      DECLARE cur_tab_credito CURSOR FOR prp_tab_credito

      LET i = 1

      FOREACH cur_tab_credito INTO arr_tabla_credito[i].*
         LET arr_tabla_credito[i].checkbox = 0
         LET i = i + 1
      END FOREACH

      IF arr_tabla_credito[arr_tabla_credito.getLength()].id_cre_acreditado IS NULL AND
         i > 1 THEN
         CALL arr_tabla_credito.deleteElement(arr_tabla_credito.getLength())
      END IF

END FUNCTION

--Función que despliega datos para las tablas
FUNCTION fn_tabla_saldo_ssv()

   DEFINE x                    INTEGER

      DECLARE cur_tab_saldo CURSOR FOR
      SELECT  b.subcuenta_desc,
              sum(m.monto_acciones),
              (sum(m.monto_acciones)*g.precio_fondo)
       FROM   glo_valor_fondo g, cat_subcuenta b,
              cta_movimiento m
       WHERE  g.f_valuacion = TODAY
         AND  g.fondo = m.fondo_inversion
         AND  m.id_derechohabiente= p_id_derechohabiente
         AND  m.subcuenta IN (4,8,42,44)
         AND  m.subcuenta = b.subcuenta
     GROUP BY b.subcuenta_desc,g.precio_fondo

      LET x = 1

      FOREACH cur_tab_saldo INTO arr_tabla_saldo[x].*
         LET x = x + 1
      END FOREACH

      IF arr_tabla_saldo[arr_tabla_saldo.getLength()].subcuenta IS NULL AND
         x > 1 THEN
         CALL arr_tabla_saldo.deleteElement(arr_tabla_saldo.getLength())
      END IF

END FUNCTION

--Función para obtener datos de la tabla deudor
FUNCTION fn_despliega_datos ()

   DEFINE v_pos                    SMALLINT
   DEFINE j                        SMALLINT

      DIALOG ATTRIBUTES (UNBUFFERED )
      INPUT ARRAY arr_tabla_credito FROM tabla_1.* ATTRIBUTE (WITHOUT DEFAULTS,
                                                              APPEND ROW = FALSE,
                                                              DELETE ROW = FALSE,
                                                              INSERT ROW = FALSE)
        BEFORE ROW
           LET v_pos = ARR_CURR()
           CALL fn_tabla_deudor (arr_tabla_credito[v_pos].id_cre_acreditado)

        ON CHANGE checkbox

           IF arr_tabla_credito[v_pos].checkbox = 1 THEN
              FOR j = 1 TO arr_tabla_credito.getLength()
                 IF j <> v_pos THEN
                    LET arr_tabla_credito[j].checkbox = 0
                 END IF
              END FOR
           END IF
      END INPUT

      DISPLAY ARRAY arr_tabla_saldo TO tabla_2.*
      END DISPLAY

      DISPLAY ARRAY arr_tabla_deudor TO tabla_3.*
      END DISPLAY

      ON ACTION ACCEPT
         CALL fn_valida_seleccion()
         EXIT DIALOG

      ON ACTION CLOSE
         EXIT DIALOG

      ON ACTION salir
         LET v_salir = 2
         EXIT DIALOG

      END DIALOG

END FUNCTION

--Función que valida estado y estado procesar para actualización de tablas
FUNCTION fn_valida_seleccion()

   DEFINE v_estado_clave         INTEGER  -- clave de estado infonavit consultado
   DEFINE  v_edo_procesar_clave  INTEGER  -- clave de estado procesar consultado
   DEFINE r_valida_edo_procesar  INTEGER  -- valida que el estado procesar sea únicamente 120
   DEFINE r_valida_edo_infonavit INTEGER  -- valida que el estado debe ser: 140,145,220,900
   DEFINE v_opc_selec            SMALLINT -- valida si seleccionó un registro

   LET v_opc_selec = 0

   FOR i = 1 TO arr_tabla_credito.getLength()
      IF arr_tabla_credito[i].checkbox = 0 THEN
         LET v_opc_selec = 0
         --CALL fn_mensaje("Aviso","Ninguna opción fue seleccionada","info")
      ELSE 
         --FOR i = 1 TO arr_tabla_credito.getLength()
            IF arr_tabla_credito[i].checkbox = 1 THEN
               LET v_estado_clave       = arr_tabla_credito[i].edo_infonavit_clave
               LET v_edo_procesar_clave = arr_tabla_credito[i].edo_procesar_clave
               LET v_id_cre_acreditado  = arr_tabla_credito[i].id_cre_acreditado
               LET v_opc_selec          = 1

              IF (v_estado_clave = 140 OR
                  v_estado_clave = 145 OR
                  v_estado_clave = 220 OR
                  v_estado_clave = 900) THEN
                  LET r_valida_edo_infonavit = 1
               ELSE
                  LET r_valida_edo_infonavit = 0
               END IF

               IF v_edo_procesar_clave = 120 THEN
                  LET r_valida_edo_procesar = 1
               ELSE
                  LET r_valida_edo_procesar = 0
               END IF

               IF (r_valida_edo_infonavit = 1) AND (r_valida_edo_procesar = 0) THEN
                  CALL fn_mensaje("Aviso","Registro pendiente de enviar o responder por Procesar","info")
               END IF

               IF (r_valida_edo_infonavit = 0 )AND (r_valida_edo_procesar = 1 ) THEN
                  CALL fn_mensaje("Aviso","Registro pendiente de liquidar deudor original","info")
               END IF

               IF (r_valida_edo_infonavit = 0 )AND (r_valida_edo_procesar = 0 ) THEN
                  CALL fn_mensaje("Aviso","Registro pendiente de liquidar deudor original y\n Registro pendiente de enviar o responder por Procesar","info")
               END IF

               IF (r_valida_edo_infonavit = 1) AND (r_valida_edo_procesar = 1) THEN 

               --CALL fn_mensaje("Aviso","antes de insert","info")
               --DISPLAY  "id_cre_acreditado",v_id_cre_acreditado 

                  INSERT INTO cre_acred_deudor
                  VALUES(v_id_cre_acreditado,
                         v_estado_clave,
                         v_edo_procesar_clave,
                         TODAY,
                         p_usuario);

                 --CALL fn_mensaje("Aviso","Registros insertados correctamente","info")
                 UPDATE STATISTICS FOR TABLE cre_acred_deudor

                  UPDATE cre_acreditado
                     SET sdo_deudor = v_monto,
                         estado = 20,
                         edo_procesar = 70
                   WHERE id_cre_acreditado = v_id_cre_acreditado;

                  UPDATE cre_saldo_deudor
                     SET monto_pesos = v_monto
                   WHERE id_cre_acreditado = v_id_cre_acreditado
                     AND movimiento = 181;

                     CALL fn_mensaje("Aviso","Datos actualizados correctamente","info")
               END IF
            END IF
         --END FOR
      END IF  
   END FOR 

   IF v_opc_selec = 0 THEN
      CALL fn_mensaje("Aviso","Ninguna opción fue seleccionada","info")
   END IF

END FUNCTION

--Función que borra tablas temporales existentes
FUNCTION fn_crea_tablas_temporales()

      WHENEVER ERROR CONTINUE
      DROP TABLE tmp_id_cre_acreditado
      DROP TABLE tmp_credito
      WHENEVER ERROR STOP

END FUNCTION

--Función para llenar tabla deudor dependendo la selección de la tabla credito
FUNCTION fn_tabla_deudor(p_id_cre_acreditado)

   DEFINE p_id_cre_acreditado      DECIMAL(9,0)  -- Valor de id_cre_acreditado para NSS consultado
   DEFINE b                        SMALLINT

   CALL arr_tabla_deudor.clear()

   DECLARE cur_tab_deudor CURSOR FOR
   SELECT  s.folio_referencia,
           c.movimiento_desc,
           s.f_movimiento,
           s.monto_aivs,
           s.monto_pesos,
           s.f_proceso
   FROM    cre_saldo_deudor s,cat_movimiento c
   WHERE   s.id_cre_acreditado = p_id_cre_acreditado
     AND   s.movimiento = c.movimiento

    LET b = 1

   FOREACH cur_tab_deudor INTO arr_tabla_deudor[b].*
      LET b = b + 1
   END FOREACH

   IF arr_tabla_deudor[arr_tabla_saldo.getLength()].folio_referencia IS NULL
       AND b > 1 THEN
       CALL arr_tabla_deudor.deleteElement(arr_tabla_deudor.getLength())
    END IF

END FUNCTION

--función que valida que el NSS ingresado sea un valor numerico
FUNCTION fn_valida_numero_nss()

   DEFINE v_funcion                STRING --variable para función fn_es_numero()
   DEFINE v_cadena                 CHAR(1) --cadena de valores ingresados a NSS
   DEFINE a                        INTEGER
   DEFINE r_valida                 SMALLINT--valor regresado de la función fn_es_numero(), 0 ó 1

   LET v_funcion = "EXECUTE FUNCTION fn_es_numero (?)"

   PREPARE prp_es_numero FROM v_funcion
      FOR a=1 TO LENGTH (v_nss)
         LET v_cadena =  v_nss[a,a]

         EXECUTE prp_es_numero USING v_cadena INTO r_valida

         IF r_valida = 0 THEN
            EXIT FOR
         END IF
      END FOR

      IF r_valida = 1 THEN
         CALL fn_consulta_nss_monto()
      ELSE
         CALL fn_mensaje ("AVISO","NSS no es un valor numerico","stop")
      END IF

END FUNCTION