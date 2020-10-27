--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#####################################################################
#Modulo            => PRT                                           #
#Programa          => PRTL24                                        #
#Objetivo          => Programa que realiza la preliquidación        #
#                     de aportaciones para portabilidad             #
#Autor             => Hecto F. Jiménez Lara                         #
#Fecha inicio      => 05 de Agosto del 2015                         #
#####################################################################

DATABASE safre_viv

   DEFINE arr_arbol_pre DYNAMIC ARRAY OF RECORD
      subcuenta_desc             CHAR(30),
      siefore                    SMALLINT,
      monto_pesos                DECIMAL(28,6),
      monto_acciones             DECIMAL(28,6),
      subcuenta                  SMALLINT,
      padre_id                   STRING,
      id                         STRING,
      nivel                      SMALLINT
   END RECORD

MAIN

   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod              -- clave del usuario firmado
   DEFINE p_tipo_ejecucion       SMALLINT                                  -- forma como ejecutara el programa
   DEFINE p_v_nom_prog           VARCHAR(30)                               -- nombre del programa
   DEFINE v_i_estado             SMALLINT                                  -- estado al que se va a actualizar
   DEFINE v_s_qryTxt             STRING                                    -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_proceso_cod        LIKE cat_proceso.proceso_cod              -- código del proceso
   DEFINE v_i_opera_cod          LIKE cat_operacion.opera_cod              -- código de operacion
   DEFINE v_folio                DECIMAL (9,0)
   DEFINE v_d_pid                DECIMAL(9,0)                              -- identificador del proceso
   DEFINE v_c_programa_cod       LIKE bat_ctr_operacion.programa_cod       -- nombre del programa
   DEFINE v_v_nom_archivo        LIKE glo_ctr_archivo.nombre_archivo       -- nombre del archivo en proceso
   DEFINE v_si_tpo_originacion   LIKE cre_acreditado.tpo_originacion       -- tipo de originación
   DEFINE v_c_tpo_transferencia  LIKE cre_uso_garantia.tpo_transferencia   -- tipo de transferencia
   DEFINE v_ban_salir            SMALLINT
   DEFINE v_cta_reg              INTEGER
   DEFINE v_s_comando            STRING                                    -- contiene al comando a correr
   DEFINE v_s_mensaje            STRING                                    -- mensaje a mostrar al usuario
   DEFINE v_c_ruta_list_bat      LIKE seg_modulo.ruta_listados             -- ruta listados de bat
   DEFINE r_c_ruta_bin           LIKE seg_modulo.ruta_bin                  -- ruta del bin del módulo
   DEFINE r_ruta_listados        LIKE seg_modulo.ruta_listados             -- ruta de listados del módulo
   DEFINE r_b_valida             SMALLINT                                  -- booleana que indica si el proceso se puede ejecutar o no

   -- se asignan los parametros recibidos
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_v_nom_prog     = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".PRTL24.log")

   CLOSE WINDOW SCREEN
   
   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inializan las variables
   LET v_i_proceso_cod       = 2810  -- liquidación amortización
   LET v_i_opera_cod         = 1     -- preliquidación
   LET v_d_pid               = 0
   LET v_v_nom_archivo       = "N/A"
   LET v_c_programa_cod      = "PRTL24"
   LET v_folio               = NULL
   LET v_si_tpo_originacion  = 4     -- Anualidades Garantizadas
   LET v_c_tpo_transferencia = "43"  -- Anualidades Garantizadas
   LET v_i_estado            = 130   -- estado preliquidado

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("prt") RETURNING r_c_ruta_bin, r_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operación en proceso es válida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continúa
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   LET v_s_qryTxt = "SELECT COUNT(*)
                       FROM cta_movimiento
                       WHERE id_derechohabiente > 0
                       AND subcuenta = 60
                       AND movimiento IN (1601,1672)"

   PREPARE prp_cuenta_peliq FROM v_s_qryTxt
   EXECUTE prp_cuenta_peliq INTO v_cta_reg

   -- se verifica si se encontró información para preliquidar
   IF v_cta_reg = 0 THEN
      LET v_s_mensaje = "No es posible realizar la preliquidación, ya que no se encontró\n",
                        "información para preliquidar"
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")
      EXIT PROGRAM
   END IF

   -- se genera el folio
   LET v_folio = fn_genera_folio(v_i_proceso_cod,v_i_opera_cod,p_usuario_cod)

   -- llama a la funcion que muestra la información a preliquidar
   CALL fn_folio_preliquidar(p_usuario_cod, v_i_proceso_cod, v_i_opera_cod,v_folio,v_si_tpo_originacion) RETURNING v_folio, v_ban_salir

   DISPLAY "FOLIO A PRELIQUIDAR: ", v_folio
   IF v_ban_salir = TRUE THEN
      EXIT PROGRAM
   END IF

   -- se invoca la función que genera el pid
   LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_usuario_cod)

   -- se invoca la funcion que inicializa el proceso
   LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod, v_folio, v_c_programa_cod, v_v_nom_archivo, p_usuario_cod)

   -- en caso de error se muestra un mensaje a usuario
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,v_i_proceso_cod,v_i_opera_cod,
                                           v_folio, v_c_programa_cod,
                                           v_v_nom_archivo, p_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF r_b_valida = 0 THEN
      -- se crea el comando que ejecuta el modulo que reliza la insercion del registro de control
      LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/PRTP01 ",
                                              p_usuario_cod, " ",
                                              v_d_pid, " ",
                                              v_i_proceso_cod, " ",
                                              v_i_opera_cod, " ",
                                              v_folio, " ",
                                              v_v_nom_archivo, " 1> ",
                                              v_c_ruta_list_bat CLIPPED,
                                              "/nohup:",v_d_pid USING "&&&&&",":",
                                              v_i_proceso_cod USING "&&&&&",":",
                                              v_i_opera_cod USING "&&&&&",
                                              " 2>&1 &"

      DISPLAY v_s_comando
      RUN v_s_comando

      -- se asigna el mensaje a mostrar al usuario
      LET v_s_mensaje = "Se ha enviado la preliquidación con PID: ",v_d_pid CLIPPED,
                        ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
      CALL fn_mensaje("Preliquidación",v_s_mensaje,"information")
   END IF

END MAIN

#Objetivo: Pantalla que muestra el folio para realizar la preliquidación
FUNCTION fn_folio_preliquidar(p_usuario, p_proceso_cod, p_opera_cod, p_folio, p_si_tpo_originacion)

   DEFINE p_usuario              CHAR(20)
   DEFINE p_proceso_cod          SMALLINT
   DEFINE p_opera_cod            SMALLINT
   DEFINE p_folio                DECIMAL(9,0)
   DEFINE p_si_tpo_originacion   LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE i                      SMALLINT
   DEFINE v_folio                DECIMAL(9,0)

   DEFINE  vr_folios_preliquidar  RECORD
      folio                      DECIMAL(9,0),
      fecha_liquidacion          DATE,
      fecha_proceso              DATE,
      activa                     SMALLINT
   END RECORD

   DEFINE arr_folios_preliquidar DYNAMIC ARRAY OF RECORD
      folio                      DECIMAL(9,0),
      fecha_liquidacion          DATE,
      fecha_proceso              DATE,
      activa                     SMALLINT
   END RECORD

   DEFINE arr_folios_pre DYNAMIC ARRAY OF RECORD
      folio                      DECIMAL(9,0),
      fecha_liquidacion          DATE,
      fecha_proceso              DATE
   END RECORD

   DEFINE v_tabla_preliq         CHAR(30)
   DEFINE v_ban_salir            SMALLINT

   CALL arr_arbol_pre.clear()
   CALL arr_folios_pre.clear()

   LET vr_folios_preliquidar.folio = p_folio
   LET vr_folios_preliquidar.fecha_liquidacion = TODAY
   LET vr_folios_preliquidar.fecha_proceso = TODAY
   LET vr_folios_preliquidar.activa = 1
   LET i = 1
   LET arr_folios_preliquidar[i].* = vr_folios_preliquidar.*

   OPEN WINDOW w_selec_pre WITH FORM "PRTL241"

   DIALOG ATTRIBUTES(UNBUFFERED)
      INPUT ARRAY arr_folios_preliquidar FROM arr_folios.* ATTRIBUTES(
                                                          APPEND ROW = FALSE,
                                                          INSERT ROW = FALSE,
                                                          DELETE ROW = FALSE,
                                                          AUTO APPEND= FALSE)

         BEFORE INPUT
           CALL DIALOG.setactionhidden("close",1)
           CALL fn_despliega_desc_pre(p_proceso_cod, p_opera_cod)

         BEFORE ROW
            LET i       = ARR_CURR()
            LET v_folio = arr_folios_preliquidar[i].folio

            SELECT nombre_tabla
              INTO v_tabla_preliq
              FROM cat_preliquida
             WHERE proceso_cod = p_proceso_cod
               AND opera_cod = 1

            CALL fn_llena_arbol_montos_pre(p_folio, v_tabla_preliq,p_si_tpo_originacion)
            CALL ui.Interface.refresh()
            DISPLAY vr_folios_preliquidar.folio TO folio_liquida
            DISPLAY vr_folios_preliquidar.fecha_proceso TO f_registro
            DISPLAY vr_folios_preliquidar.fecha_liquidacion TO f_liquida
         END INPUT

         DISPLAY ARRAY arr_arbol_pre TO scr1.*

         END DISPLAY

         ON ACTION ACCEPT
            LET v_ban_salir = FALSE
            EXIT DIALOG

         ON ACTION CANCEL
          LET v_ban_salir = TRUE
          EXIT DIALOG
      END DIALOG

   CLOSE WINDOW w_selec_pre

   RETURN vr_folios_preliquidar.folio,v_ban_salir

   DISPLAY "FOLIO: ",vr_folios_preliquidar.folio

END FUNCTION

# Objetivo: Función que muestra a usuario los montos a preliquidar agrupados por subcuenta.
FUNCTION fn_llena_arbol_montos_pre(p_folio, v_tabla, p_si_tpo_originacion)

   DEFINE p_folio               INTEGER
   DEFINE v_tabla               STRING
   DEFINE p_si_tpo_originacion  LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE qry_string            STRING
   DEFINE i                     INTEGER
   DEFINE j                     INTEGER
   DEFINE k                     INTEGER
   DEFINE cont_arbol            INTEGER

   DEFINE arr_nivel1 DYNAMIC ARRAY OF RECORD
          grupo_regimen         SMALLINT,
          desc_larga            CHAR(40),
          monto_pesos           DECIMAL(22,6),
          monto_acciones        DECIMAL(22,6)
   END RECORD

   DEFINE arr_nivel2 DYNAMIC ARRAY OF RECORD
          subcuenta             SMALLINT,
          descripcion           CHAR(40),
          siefore               SMALLINT,
          pesos                 DECIMAL(22,6),
          acciones              DECIMAL(22,6)
   END RECORD

   DEFINE v_d_valor_accion      DECIMAL(22,6)
   DEFINE v_d_monto_pesos       DECIMAL(22,6)
   DEFINE v_d_monto_aivs        DECIMAL(22,6)
   DEFINE v_d_monto_deudor      DECIMAL(22,6)
   DEFINE v_d_monto_accion      DECIMAL(22,6)

   -- se obtiene el precio de acción para el día de hoy
   SELECT precio_fondo
     INTO v_d_valor_accion
     FROM glo_valor_fondo
    WHERE fondo = 0
      AND f_valuacion = TODAY

   -- se inicializan variables
   LET i = 1
   LET j = 1
   LET k = 1
   LET cont_arbol = 1
   CALL arr_arbol_pre.clear()

   -- se obtiene el total en pesos a preliquidar
   SELECT SUM(monto_pesos)
     INTO v_d_monto_deudor
     FROM cta_movimiento
    WHERE subcuenta = 60
      AND movimiento IN (1601,1672)

   LET v_d_monto_pesos = v_d_monto_deudor;

   -- se calculan las aivs
   LET v_d_monto_aivs =  v_d_monto_pesos / v_d_valor_accion

DISPLAY "Pesos : ",v_d_monto_pesos
DISPLAY "Aivs  : ",v_d_monto_aivs

   -- se asigna el primer registro
   LET arr_arbol_pre[1].subcuenta      = ""
   LET arr_arbol_pre[1].subcuenta_desc = "ABONO TRANSF SALDO FOV-INF"
   LET arr_arbol_pre[1].monto_pesos    = v_d_monto_pesos
   LET arr_arbol_pre[1].monto_acciones = v_d_monto_aivs
   LET arr_arbol_pre[1].id             = ""
   LET arr_arbol_pre[1].nivel          = 1
   LET arr_arbol_pre[1].padre_id       = ""
   LET arr_arbol_pre[1].siefore        = 0
   LET cont_arbol                      = cont_arbol + 1

   LET v_d_monto_aivs  = 0;
   LET v_d_monto_pesos = 0;

   -- se asigna el segundo registro
   LET arr_arbol_pre[2].subcuenta      = ""
   LET arr_arbol_pre[2].subcuenta_desc = "INFONAVIT"
   LET arr_arbol_pre[2].monto_pesos    = 0
   LET arr_arbol_pre[2].monto_acciones = 0
   LET arr_arbol_pre[2].id             = ""
   LET arr_arbol_pre[2].nivel          = 1
   LET arr_arbol_pre[2].padre_id       = ""
   LET arr_arbol_pre[2].siefore        = 0
   LET cont_arbol                      = cont_arbol + 1

   LET v_d_monto_aivs  = 0;
   LET v_d_monto_pesos = 0;

   -- se obtiene la subcuenta y descripción para las subcuentas requeridas (4,8,42,44,49)
   LET qry_string = " SELECT ts.subcuenta, ts.subcuenta||' '||ts.subcuenta_desc ",
                    "   FROM cat_subcuenta ts ",
                    "  WHERE ts.subcuenta = 60 "

    PREPARE prp_nivel2 FROM qry_string
    DECLARE cur_nivel2 CURSOR FOR prp_nivel2

    FOREACH cur_nivel2 INTO arr_nivel2[j].subcuenta, arr_nivel2[j].descripcion
      -- se calculan los pesos de las aivs obtenidas
      LET v_d_monto_pesos = v_d_monto_aivs * v_d_valor_accion

      LET arr_arbol_pre[cont_arbol].subcuenta      = arr_nivel2[j].subcuenta
      LET arr_arbol_pre[cont_arbol].subcuenta_desc = arr_nivel2[j].descripcion
      LET arr_arbol_pre[cont_arbol].monto_pesos    = 0
      LET arr_arbol_pre[cont_arbol].monto_acciones = 0
      LET arr_arbol_pre[cont_arbol].id             = arr_nivel1[i].grupo_regimen USING"<<",".",
                                                     arr_nivel2[j].subcuenta USING"<<"
      LET arr_arbol_pre[cont_arbol].nivel          = 2
      LET arr_arbol_pre[cont_arbol].padre_id       = arr_nivel1[i].grupo_regimen USING"<<"
      LET arr_arbol_pre[cont_arbol].siefore        = 11
      LET cont_arbol = cont_arbol + 1

      -- se incrementa el indice del arreglo
      LET j = j + 1
      LET v_d_monto_accion = 0;
   END FOREACH

   CLOSE cur_nivel2

   LET i = i + 1

END FUNCTION

# Objetivo: Función que muestra a usuario la descripción del proceso y operacion
FUNCTION fn_despliega_desc_pre(p_proceso_cod, p_opera_cod)

   DEFINE p_proceso_cod  SMALLINT
   DEFINE p_opera_cod    SMALLINT
   DEFINE v_proceso_desc CHAR(40)
   DEFINE v_opera_desc   CHAR(40)

   -- se obtiene la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   -- se obtiene la descripción de la operacion
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   -- se despliegan las descripciones
   DISPLAY BY NAME v_proceso_desc
   DISPLAY BY NAME v_opera_desc

END FUNCTION
