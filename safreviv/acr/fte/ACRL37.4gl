--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

##########################################################################
#Modulo       => ACR                                                     #
#Programa     => ACRL37                                                  #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la #
#                preliquidacion para DSE devoluciones                    #
#Autor        =>Ivan Vega, EFP                                           #
#Fecha inicio => Febrero 03, 2012                                        #
#Modificado por    => Mauro Muñiz Cabllero, EFP                          #
#Modificación      => Eliminación de adelantos                           #
#Fecha modifica    => 15 de noviembre de 2016                            #
##########################################################################

DATABASE safre_viv
GLOBALS "ACRG10.4gl"

GLOBALS

   DEFINE g_pid                     LIKE bat_ctr_proceso.pid --  ID del proceso
   DEFINE g_proceso_cod             LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod               LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE g_ruta_bin                CHAR(40)
   DEFINE g_ruta_listados           CHAR(40)
   DEFINE v_f_valuacion             LIKE glo_valor_fondo.f_valuacion -- fecha de valuación

   DEFINE g_arr_arbol_pre DYNAMIC ARRAY OF RECORD
      subcuenta_desc                CHAR(30)     ,
      siefore                       SMALLINT     ,
      monto_pesos                   DECIMAL(28,6),
      monto_acciones                DECIMAL(28,6),
      subcuenta                     SMALLINT     ,
      padre_id                      STRING       ,
      id                            STRING       ,
      nivel                         SMALLINT
   END RECORD

END GLOBALS

MAIN

   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutará el programa
   DEFINE p_s_titulo                STRING -- título de la ventana
   DEFINE v_folio                   LIKE deo_preliquida.folio_liquida -- folio de liquidación
   DEFINE v_c_tpo_transf            LIKE dse_agrupa_devolucion.tpo_transferencia -- tipo de transferencia
   DEFINE v_precio_fondo            LIKE glo_valor_fondo.precio_fondo -- precio de acción
   DEFINE v_num_regs                INTEGER -- número de registros encontrados
   DEFINE v_ban_salir               SMALLINT -- bandera que indica si el usuario salió
   DEFINE v_s_mensaje               STRING -- mensaje a mostrar a usuario
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar

   -- se recuperan las claves desde parámetro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el título, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".ACRL37.log")

   -- se asigna proceso y operacion
   LET g_proceso_cod  = g_proc_cod_acr_liquida_dse -- Preliquidacion de DSE Devoluciones
   LET g_opera_cod    = 1  -- preliquidacion
   LET v_c_tpo_transf = 15 -- 15-DSE Transferencia de Acreditados

   -- se obtienen las rutas de control del módulo
   CALL fn_rutas("acr") RETURNING g_ruta_bin, g_ruta_listados

   -- se crea la fecha de valuación. El primer día del mes
   --LET v_f_valuacion = TODAY - DAY(TODAY) + 1
   --LET v_f_valuacion = TODAY

   SELECT UNIQUE f_movimiento
     INTO v_f_valuacion
     FROM safre_tmp:tmp_confirmacion_saldos

   DISPLAY "Fecha proyectada de liquidación: ", v_f_valuacion USING "DD-MM-YYYY"
   DISPLAY ""

   -- validar si existe registros a preliquidar
   LET v_s_qryTxt = " SELECT precio_fondo\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11\n",
                    "    AND f_valuacion = '",v_f_valuacion,"'"

   PREPARE prp_precio_fondo FROM v_s_qryTxt
   EXECUTE prp_precio_fondo INTO v_precio_fondo

   -- se valida el precio fondo
   IF v_precio_fondo IS NULL THEN
      LET v_s_mensaje = "No es posible realizar la preliquidación, ya que no se encuentra\n",
                        "el precio de acción para el primer dia ",v_f_valuacion CLIPPED
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

      EXIT PROGRAM
   END IF

   -- validar si existe registros a preliquidar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_agrupa_devolucion\n",
                    "  WHERE tpo_transferencia = ",v_c_tpo_transf,"\n",
                    "    AND estado = 20\n",
                    "    AND edo_procesar IN(7,120)"

   PREPARE prp_cuenta_peliq FROM v_s_qryTxt
   EXECUTE prp_cuenta_peliq INTO v_num_regs

   -- se verifica si se encontró información para preliquidar
   IF v_num_regs = 0 THEN
      LET v_s_mensaje = "No es posible realizar la preliquidación, ya que no se encontró\n",
                        "información para preliquidar"
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

      EXIT PROGRAM
   END IF    

   -- se invoca la funcion que obtiene el folio de liquidación
   LET v_folio = fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)

   -- llama a la funcion que muestra la información a preliquidar
   CALL fn_folio_preliquidar(p_usuario_cod, g_proceso_cod, g_opera_cod, v_folio,
                             v_c_tpo_transf) RETURNING v_folio, v_ban_salir

   DISPLAY "FOLIO A PRELIQUIDAR: ", v_folio
   IF v_ban_salir = TRUE THEN
      EXIT PROGRAM  
   END IF 

   -- se invoca la función que realiza la preliquidación
   CALL fn_dse_ejecuta_preliquidacion(v_folio, p_usuario_cod)
END MAIN

#Objetivo: Ejecuta la preliquidacion de una DSE devoluciones para un folio dado
FUNCTION fn_dse_ejecuta_preliquidacion(p_folio, p_usuario_cod)

   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod -- usuario que ejecuta el programa
   DEFINE p_folio                   LIKE glo_folio.folio -- folio para preliquidar
   DEFINE v_s_comando               STRING -- cadena con una instrucciÓn de consola
   DEFINE v_nombre_archivo          LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   DEFINE v_b_resultado             SMALLINT -- resultado de validar la operaciÓn
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta de listados de bat
   DEFINE v_s_qryTxt                STRING -- contiene una sentencia sql a ejecutar

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat2 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat2 INTO v_c_ruta_list_bat

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "N/A"

   -- el PID es cero porque esta etapa inicia un proceso
   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
                            RETURNING v_b_resultado

   -- se verifica si se puede continuar con la operacion
   IF ( v_b_resultado = 0 ) THEN
      -- se genera el PID del proceso
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)
                         RETURNING g_pid

      -- se inicia el proceso
      CALL fn_inicializa_proceso(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod,
                                 p_folio,
                                 "ACRL37",
                                 v_nombre_archivo,
                                 p_usuario_cod)
                                 RETURNING v_b_resultado

      IF v_b_resultado <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(v_b_resultado)

         RETURN
      END IF

      -- Inicio operacion
      CALL fn_actualiza_opera_ini(g_pid,
                                  g_proceso_cod,
                                  g_opera_cod,
                                  p_folio,
                                  "ACRL37",
                                  v_nombre_archivo,
                                  p_usuario_cod)
                                  RETURNING v_b_resultado

      IF v_b_resultado <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(v_b_resultado)

         RETURN
      END IF

      -- se invoca la ejecución del programa lanzado. los parámetros se envían 
      -- en orden segun lo acordado el dia 12/Enero/2012 con equipo EFP
      -- usuario, pid, proceso_cod, opera_cod, folio y archivo
      LET v_s_comando = " nohup time fglrun ",g_ruta_bin CLIPPED,"/ACRP37 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          v_f_valuacion," ",
                          " 1>", v_c_ruta_list_bat CLIPPED ,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"

      DISPLAY v_s_comando
      RUN v_s_comando
      CALL fn_mensaje("Atención","Se ha enviado la preliquidación.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
   ELSE
      -- se recupera la descripcion del codigo de error
      LET v_s_comando = fn_recupera_inconsis_opera(v_b_resultado)
      CALL fn_mensaje("Atención", v_s_comando.trim(),"information")
   END IF

END FUNCTION

#Objetivo: Pantalla que muestra el folio para realizar la preliquidación
FUNCTION fn_folio_preliquidar(p_usuario, p_proceso_cod, p_opera_cod, p_folio, p_c_tpo_transferencia)

   DEFINE p_usuario                 CHAR(20)
   DEFINE p_proceso_cod              SMALLINT
   DEFINE p_opera_cod                SMALLINT
   DEFINE p_folio                    DECIMAL(9,0)
   DEFINE p_c_tpo_transferencia      LIKE dse_agrupa_devolucion.tpo_transferencia -- tipo de transferencia
   DEFINE i                          SMALLINT
   DEFINE v_folio                    DECIMAL(9,0)

   DEFINE vr_folios_preliquidar RECORD
      folio                         DECIMAL(9,0),
      fecha_liquidacion             DATE,
      fecha_proceso                 DATE,
      activa                        SMALLINT
   END RECORD

   DEFINE arr_folios_preliquidar DYNAMIC ARRAY OF RECORD
      folio                         DECIMAL(9,0),
      fecha_liquidacion             DATE,
      fecha_proceso                 DATE,
      activa                        SMALLINT
   END RECORD

   DEFINE arr_folios_pre DYNAMIC ARRAY OF RECORD
      folio                         DECIMAL(9,0),
      fecha_liquidacion             DATE,
      fecha_proceso                 DATE
   END RECORD

   DEFINE v_ban_salir               SMALLINT

   -- se inicializan variables
   CALL g_arr_arbol_pre.clear()
   CALL arr_folios_pre.clear()

   LET vr_folios_preliquidar.folio             = p_folio
   LET vr_folios_preliquidar.fecha_liquidacion = v_f_valuacion
   LET vr_folios_preliquidar.fecha_proceso     = TODAY
   LET vr_folios_preliquidar.activa            = 1

   LET i = 1

   LET arr_folios_preliquidar[i].* = vr_folios_preliquidar.*

   OPEN WINDOW w_selec_pre WITH FORM "ACRL191"

   DIALOG ATTRIBUTES(UNBUFFERED)
      INPUT ARRAY arr_folios_preliquidar FROM arr_folios.*
      ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, AUTO APPEND= FALSE)
         BEFORE INPUT
           CALL DIALOG.setactionhidden("close",1)
           CALL fn_despliega_desc_pre(p_proceso_cod, p_opera_cod)

         BEFORE ROW
            LET i       = ARR_CURR()
            LET v_folio = arr_folios_preliquidar[i].folio

            CALL fn_llena_arbol_montos_pre_dseacr(p_folio, p_c_tpo_transferencia)
            CALL ui.Interface.refresh()
            DISPLAY vr_folios_preliquidar.folio TO folio_liquida
            DISPLAY vr_folios_preliquidar.fecha_proceso TO f_registro
            DISPLAY vr_folios_preliquidar.fecha_liquidacion TO f_liquida
         END INPUT

         DISPLAY ARRAY g_arr_arbol_pre TO scr1.*
         END DISPLAY

         ON ACTION ACCEPT
            LET v_ban_salir = FALSE
            EXIT DIALOG

         ON ACTION CANCEL
          LET v_ban_salir = TRUE
          EXIT DIALOG
      END DIALOG
   CLOSE WINDOW w_selec_pre

   DISPLAY "FOLIO: ",vr_folios_preliquidar.folio
   RETURN vr_folios_preliquidar.folio, v_ban_salir
END FUNCTION


FUNCTION fn_llena_arbol_montos_pre_dseacr(p_folio, p_c_tpo_transferencia)

   DEFINE p_folio                   INTEGER
   DEFINE p_c_tpo_transferencia     LIKE dse_agrupa_devolucion.tpo_transferencia -- tipo de transferencia
   DEFINE qry_string                STRING
   DEFINE i                         INTEGER
   DEFINE j                         INTEGER
   DEFINE k                         INTEGER
   DEFINE v_i_indice                INTEGER -- índice del árbol

   DEFINE arr_nivel1 DYNAMIC ARRAY OF RECORD
          grupo_regimen             SMALLINT,
          desc_larga                CHAR(40),
          monto_pesos               DECIMAL(28,6),
          monto_acciones            DECIMAL(28,6)
   END RECORD

   DEFINE arr_nivel2 DYNAMIC ARRAY OF RECORD
          subcuenta                 SMALLINT,
          descripcion               CHAR(40),
          siefore                   SMALLINT,
          pesos                     DECIMAL(28,6),
          acciones                  DECIMAL(28,6)
   END RECORD

   DEFINE v_d_valor_accion          DECIMAL(28,6)
   DEFINE v_d_monto_pesos           DECIMAL(28,6)
   DEFINE v_d_monto_aivs            DECIMAL(28,6)
   DEFINE v_d_monto_aivs92          DECIMAL(28,6)
   DEFINE v_d_monto_aivs97          DECIMAL(28,6)

   -- se inicializan variables
   LET i          = 1
   LET j          = 1
   LET k          = 1
   LET v_i_indice = 1
   CALL g_arr_arbol_pre.clear()

   SELECT precio_fondo
     INTO v_d_valor_accion
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = v_f_valuacion

   LET qry_string = " SELECT ts.subcuenta, ts.subcuenta||' '||ts.subcuenta_desc ",
                    "   FROM cat_grp_subcta_regimen tasr, cat_subcuenta ts ",
                    "  WHERE ts.subcuenta IN (4,8,42,44) ",
                    "    AND ts.subcuenta = tasr.subcuenta "

   PREPARE prp_nivel2 FROM qry_string
   DECLARE cur_nivel2 CURSOR FOR prp_nivel2

  {
   SELECT NVL(SUM(aivs97),0), NVL(SUM(aivs92),0)
     INTO v_d_monto_aivs97, v_d_monto_aivs92
     FROM dse_agrupa_devolucion
    WHERE tpo_transferencia = p_c_tpo_transferencia
      AND estado = 20
      AND edo_procesar IN(5,20)
  }

   LET v_d_monto_aivs97 = 0
   LET v_d_monto_aivs92 = 0

   LET v_d_monto_aivs  = v_d_monto_aivs97 + v_d_monto_aivs92
   LET v_d_monto_pesos = v_d_monto_aivs * v_d_valor_accion

   LET g_arr_arbol_pre[v_i_indice].subcuenta      = ""
   LET g_arr_arbol_pre[v_i_indice].subcuenta_desc = "DEVOLUCIÓN SALDOS EXCED."
   LET g_arr_arbol_pre[v_i_indice].monto_pesos    = v_d_monto_pesos
   LET g_arr_arbol_pre[v_i_indice].monto_acciones = v_d_monto_aivs
   LET g_arr_arbol_pre[v_i_indice].id             = ""
   LET g_arr_arbol_pre[v_i_indice].nivel          = 1
   LET g_arr_arbol_pre[v_i_indice].padre_id       = ""
   LET g_arr_arbol_pre[v_i_indice].siefore        = 11
   LET v_i_indice = v_i_indice + 1

   FOREACH cur_nivel2 INTO arr_nivel2[j].subcuenta, arr_nivel2[j].descripcion
      IF arr_nivel2[j].subcuenta = 4 OR arr_nivel2[j].subcuenta = 44 THEN

        {
         SELECT NVL(SUM(aivs97),0)
           INTO v_d_monto_aivs
           FROM dse_agrupa_devolucion
          WHERE id_derechohabiente IN(
                SELECT id_derechohabiente
                  FROM dse_devolucion  
                 WHERE tpo_transferencia = p_c_tpo_transferencia
                   AND estado = 15
                   AND subcuenta = arr_nivel2[j].subcuenta)
         }

         LET v_d_monto_aivs  = 0
         LET v_d_monto_pesos = v_d_monto_aivs * v_d_valor_accion

         LET g_arr_arbol_pre[v_i_indice].subcuenta      = arr_nivel2[j].subcuenta
         LET g_arr_arbol_pre[v_i_indice].subcuenta_desc = arr_nivel2[j].descripcion
         LET g_arr_arbol_pre[v_i_indice].monto_pesos    = v_d_monto_pesos
         LET g_arr_arbol_pre[v_i_indice].monto_acciones = v_d_monto_aivs
         LET g_arr_arbol_pre[v_i_indice].id             = arr_nivel1[i].grupo_regimen USING"<<",".",
                                                          arr_nivel2[j].subcuenta USING"<<"
         LET g_arr_arbol_pre[v_i_indice].nivel          = 2
         LET g_arr_arbol_pre[v_i_indice].padre_id       = arr_nivel1[i].grupo_regimen USING"<<"
         LET g_arr_arbol_pre[v_i_indice].siefore        = 11
         LET v_i_indice = v_i_indice + 1

         LET j = j + 1
      ELSE
        {
         SELECT NVL(SUM(aivs92),0)
           INTO v_d_monto_aivs
           FROM dse_agrupa_devolucion
          WHERE id_derechohabiente IN(
                SELECT id_derechohabiente
                  FROM dse_devolucion  
                 WHERE tpo_transferencia = p_c_tpo_transferencia
                   AND estado = 15
                   AND subcuenta = arr_nivel2[j].subcuenta)
        }

         LET v_d_monto_aivs  = 0
         LET v_d_monto_pesos = v_d_monto_aivs * v_d_valor_accion

         LET g_arr_arbol_pre[v_i_indice].subcuenta      = arr_nivel2[j].subcuenta
         LET g_arr_arbol_pre[v_i_indice].subcuenta_desc = arr_nivel2[j].descripcion
         LET g_arr_arbol_pre[v_i_indice].monto_pesos    = v_d_monto_pesos
         LET g_arr_arbol_pre[v_i_indice].monto_acciones = v_d_monto_aivs
         LET g_arr_arbol_pre[v_i_indice].id             = arr_nivel1[i].grupo_regimen USING"<<",".",
                                                          arr_nivel2[j].subcuenta USING"<<"
         LET g_arr_arbol_pre[v_i_indice].nivel          = 2
         LET g_arr_arbol_pre[v_i_indice].padre_id       = arr_nivel1[i].grupo_regimen USING"<<"
         LET g_arr_arbol_pre[v_i_indice].siefore        = 11
         LET v_i_indice = v_i_indice + 1

         LET j = j + 1
      END IF
   END FOREACH

   LET g_arr_arbol_pre[v_i_indice].subcuenta      = ""
   LET g_arr_arbol_pre[v_i_indice].subcuenta_desc = "VIVIENDA INFONAVIT"
   LET g_arr_arbol_pre[v_i_indice].monto_pesos    = 0
   LET g_arr_arbol_pre[v_i_indice].monto_acciones = 0
   LET g_arr_arbol_pre[v_i_indice].id             = ""
   LET g_arr_arbol_pre[v_i_indice].nivel          = 1
   LET g_arr_arbol_pre[v_i_indice].padre_id       = ""
   LET g_arr_arbol_pre[v_i_indice].siefore        = 11
   LET v_i_indice = v_i_indice + 1

   FOREACH cur_nivel2 INTO arr_nivel2[j].subcuenta, arr_nivel2[j].descripcion
      LET g_arr_arbol_pre[v_i_indice].subcuenta      = arr_nivel2[j].subcuenta
      LET g_arr_arbol_pre[v_i_indice].subcuenta_desc = arr_nivel2[j].descripcion
      LET g_arr_arbol_pre[v_i_indice].monto_pesos    = 0
      LET g_arr_arbol_pre[v_i_indice].monto_acciones = 0
      LET g_arr_arbol_pre[v_i_indice].id             = arr_nivel1[i].grupo_regimen USING"<<",".",
                                                       arr_nivel2[j].subcuenta USING"<<"
      LET g_arr_arbol_pre[v_i_indice].nivel          = 2
      LET g_arr_arbol_pre[v_i_indice].padre_id       = arr_nivel1[i].grupo_regimen USING"<<"
      LET g_arr_arbol_pre[v_i_indice].siefore        = 11
      LET v_i_indice = v_i_indice + 1

      LET j = j + 1
   END FOREACH
   CLOSE cur_nivel2

   LET i = i + 1    
END FUNCTION

FUNCTION fn_despliega_desc_pre(p_proceso_cod, p_opera_cod)

   DEFINE p_proceso_cod             SMALLINT
   DEFINE p_opera_cod               SMALLINT
   DEFINE v_proceso_desc            CHAR(40)
   DEFINE v_opera_desc              CHAR(40)

   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   DISPLAY BY NAME v_proceso_desc
   DISPLAY BY NAME v_opera_desc

END FUNCTION