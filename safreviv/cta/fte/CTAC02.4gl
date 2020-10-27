###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC02                                                  #
#Objetivo          => CONSULTA DE MARCAS                                      #
#Fecha Inicio      => 20-MARZO-2012                                           #
###############################################################################
-------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                             --
-- Fec Mod.     => 15 de Agisto de 2017.                                     --
-- Modificación => Agregar consulta de detalle de AGR                        --
-- Clave cambio => saci2017-1                                                --
-- Observación  => Apoyo a requerimiento de Mauro Muñiz Caballero            --
-- Autor mod.   => Emilio Abarca Sánchez                                     --
-- Modificación =< Agregar motivo de la desmarca para casos especiales       --
-------------------------------------------------------------------------------


DATABASE safre_viv

GLOBALS

    DEFINE total_registro_a   INTEGER
    DEFINE total_registro_h   INTEGER
    DEFINE cont               INTEGER
    DEFINE capt               INTEGER
    DEFINE vn_referencia      INTEGER

    DEFINE vmarca             SMALLINT
    DEFINE tmarca             SMALLINT
    DEFINE pos                SMALLINT
    DEFINE pos2               SMALLINT
    DEFINE ch1                SMALLINT
    DEFINE ch2                SMALLINT

    DEFINE g_enter            CHAR(1)
    DEFINE g_hora             CHAR(8)
    DEFINE vnss               CHAR(11)
    DEFINE tnss               CHAR(11)
    DEFINE g_usuario          CHAR(12)
    DEFINE vdescripcion_marca CHAR(50)
    DEFINE cla_where          CHAR(200)
    DEFINE g_lista            CHAR(300)
    DEFINE g_impre            CHAR(300)
    DEFINE sel_where          STRING --CHAR(1000)
    DEFINE sel_hist           STRING --CHAR(1000)

    DEFINE g_hoy              DATE
    DEFINE vf_inicio          DATE
    DEFINE tf_inicio          DATE
    DEFINE f_inicio           DATE
    DEFINE v_bandera          SMALLINT

    DEFINE g_parametro RECORD
        ruta_listados         CHAR(40)
    END RECORD
    
    DEFINE l_record2 DYNAMIC ARRAY OF RECORD
        id_registro_a         INTEGER,
        nss2                  CHAR(11),
        marca2                SMALLINT,
        descripcion_marca2    CHAR(50),
        f_inicio2             DATE,
        h_inicio2             DATETIME HOUR TO SECOND,
        n_referencia2         INTEGER,
        usuario_marca2        CHAR(8)
    END RECORD

    DEFINE l_record3 DYNAMIC ARRAY OF RECORD
        id_registro_h         INTEGER,
        nss3                  CHAR(11),
        marca3                SMALLINT,
        descripcion_marca3    CHAR(50),
        f_inicio3             DATE,
        h_inicio3             DATETIME HOUR TO SECOND,
        f_fin3                DATE,
        estado_marca3         CHAR(25),
        rch_cod3              CHAR(25),
        marca_causa3          SMALLINT,
        causa_desc3           CHAR(50),
        f_causa3              DATE,
        n_referencia3         INTEGER,
        motivo_desmarca       CHAR(50),
        usuario_marca3        CHAR(8),
        usuario_desmarca      CHAR(8)
    END RECORD

    DEFINE reg_tipo RECORD
        tipo_marca            SMALLINT,
        desc_tipo             CHAR(20)
    END RECORD

    DEFINE reg_marca RECORD
        marca                 SMALLINT,
        descripcion_marca     CHAR(50)
    END RECORD

    DEFINE lc_qry_act         STRING
    DEFINE id_marca           STRING
    DEFINE desc_marca         STRING

    DEFINE f_w                ui.form
    DEFINE w                  ui.window

    DEFINE cb_marca           ui.combobox

    DEFINE p_tipo_proc          CHAR(1)
    DEFINE p_nombre_menu        CHAR(50)
    DEFINE g_id_derechohabiente DECIMAL(9,0)
    DEFINE v_msj_alerta         STRING
--******************************************************************************
DEFINE r_valida_nss SMALLINT
DEFINE v_paso_nss STRING
--******************************************************************************

END GLOBALS

MAIN

    LET g_usuario            = ARG_VAL(1)
    LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
    LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
    LET g_id_derechohabiente = ARG_VAL(4)

    CALL STARTLOG(g_usuario CLIPPED ||".CTAC02.log")

    CLOSE WINDOW SCREEN

    CALL fn_alertamiento()
       RETURNING v_msj_alerta
    CALL fn_inicio()
    CALL fn_proceso()

END MAIN

FUNCTION fn_inicio()
#función de inicialización de variables

  LET g_hoy = TODAY
  LET ch1   = 1

  SELECT g.ruta_listados
    INTO g_parametro.ruta_listados
    FROM seg_modulo g
   WHERE g.modulo_cod = 'cta'

END FUNCTION

FUNCTION fn_proceso()

   DEFINE v_valida_g_nss SMALLINT
   DEFINE diasemana      SMALLINT
   
   #función principal del programa

   OPEN WINDOW ctac021 WITH FORM "CTAC021" --ATTRIBUTE(STYLE="dialog")

   LET w = ui.Window.forName("ctac021")
   LET p_nombre_menu = "Consulta de Marcas"

   CALL ui.Interface.setText(p_nombre_menu)

   LET f_w = w.getForm()
   CALL f_w.setElementHidden("group3",1)
   CALL f_w.setElementHidden("group2",1)
   CALL f_w.setElementHidden("cancel",1)

   CALL ui.interface.refresh()

   LET cb_marca = ui.combobox.forname("marca")
   CALL cb_marca.clear()

   LET lc_qry_act = " SELECT marca, TRIM(descripcion_marca) ",
                    " FROM sfr_marca ",
                    --" WHERE marca > 0 ",
                    " ORDER BY marca"

   PREPARE prp_marca_act FROM lc_qry_act
   DECLARE cur_marca_act CURSOR FOR prp_marca_act

   FOREACH cur_marca_act INTO reg_marca.*
       LET id_marca   = reg_marca.marca
       LET desc_marca = id_marca CLIPPED, " ", 
                        reg_marca.descripcion_marca CLIPPED
       CALL cb_marca.additem(id_marca, desc_marca)
   END FOREACH
   CLOSE cur_marca_act
   FREE cur_marca_act

   #Se valida si llegó como parámetro el id_derechohabiente para saber si se ejecuta
   #Directo la consulta a marcas activas
   IF g_id_derechohabiente IS NOT NULL AND g_id_derechohabiente <> 0 THEN
      LET cla_where = "a.id_derechohabiente = ", g_id_derechohabiente

      SELECT nss
      INTO tnss
      FROM afi_derechohabiente 
      WHERE id_derechohabiente = g_id_derechohabiente
      AND   tipo_trabajador <> "V" -- nss virtual
      AND   origen_afiliacion <> "S"

--******************************************************************************
      LET v_paso_nss = tnss
      CALL fn_valida_nss()
      
      IF r_valida_nss = 0 THEN
--******************************************************************************
         #DISPLAY g_hoy TO f_inicio
         DISPLAY tnss TO nss
      
         CALL f_w.setElementHidden("group3",0)
         CALL f_w.setElementHidden("group2",0)
         CALL extrae_datos(1)
         CALL f_w.setElementHidden("group3",1)
         CALL f_w.setElementHidden("group2",1)
--******************************************************************************
      ELSE 
         CALL fn_mensaje("Atención",v_msj_alerta,"stop")
      END IF 
   
   ELSE #INICIO NO LLEGAN PARAMETROS

      DIALOG ATTRIBUTES(UNBUFFERED)

         CONSTRUCT cla_where ON c.nss, a.marca, a.f_inicio
                            FROM nss, marca, f_inicio
         
            BEFORE CONSTRUCT
              LET tnss   = NULL
              LET tmarca = NULL
              LET tf_inicio = NULL
         
              LET f_inicio = ""
              DISPLAY BY NAME f_inicio
              CALL FGL_DIALOG_SETBUFFER(f_inicio)
         
              CALL dialog.setActionHidden("cancel",1)
              CALL dialog.setActionHidden("close",1)
         
              CALL l_record2.clear()
              CALL l_record3.clear()
         
         END CONSTRUCT
         
         INPUT BY NAME ch1, ch2
            BEFORE FIELD ch1
            CALL dialog.setActionHidden("cancel",1)
            CALL dialog.setActionHidden("close",1)
         END INPUT
         
         ON ACTION ACCEPT
            LET cla_where = cla_where CLIPPED
--*****************************************************************************
            DISPLAY cla_where
            
            LET v_paso_nss = GET_FLDBUF(nss)
            
            IF v_paso_nss IS NOT NULL THEN
               IF fn_valida_caracteres(v_paso_nss) <> 0 THEN
                  CALL fn_mensaje("Consulta Marcas", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
                  NEXT FIELD nss
               END IF
            
               CALL fn_valida_nss()
            ELSE
               CALL fn_mensaje("Consulta Marcas", "Debe de ingresar el NSS de búsqueda.", "about")
               NEXT FIELD nss
            END IF
            
            IF r_valida_nss = 0 THEN
--******************************************************************************
            
               IF NOT FIELD_TOUCHED(nss) THEN
                  LET tnss = FGL_DIALOG_GETBUFFER()
               END IF
            
               IF NOT FIELD_TOUCHED(marca) THEN
                  LET tmarca = FGL_DIALOG_GETBUFFER()
               END IF
            
               IF NOT FIELD_TOUCHED(f_inicio) THEN
                  LET tf_inicio = FGL_DIALOG_GETBUFFER()
               END IF
            
               IF tnss       IS NULL AND
                  tmarca     IS NULL AND
                  tf_inicio IS NULL THEN
                  IF cla_where IS NULL OR 
                     cla_where LIKE '1=1' THEN
                     CALL fn_mensaje("Consulta de Marcas", "Debe capturar un criterio de búsqueda", "about")
                     NEXT FIELD nss
                  END IF
               END IF
            
               IF NOT tnss       AND
                  NOT tmarca     AND
                  NOT tf_inicio THEN
                  CALL fn_mensaje("Consulta de Marcas", "Debe capturar un criterio de búsqueda", "about")
                  NEXT FIELD nss
               ELSE
                  IF NOT ch1 AND
                     NOT ch2 THEN
                     CALL fn_mensaje("Consulta de Marcas", "Debe seleccionar un Tipo de Marca", "about")
                     NEXT FIELD nss
                  END IF
               END IF
            
               IF tnss       IS NULL        AND
                  tmarca     = 0            AND
                  tf_inicio = '12/31/1899' THEN
                  CALL fn_mensaje("Consulta de Marcas", "Debe capturar un criterio de búsqueda", "about")
                  NEXT FIELD nss
               END IF
            
               IF tnss       IS NULL        AND
                  tmarca     IS NULL         AND
                  tf_inicio = '12/31/1899' THEN
                  CALL fn_mensaje("Consulta de Marcas", "Debe capturar un criterio de búsqueda", "about")
                  NEXT FIELD nss
               END IF
            
               IF tnss       IS NULL AND
                  tmarca     IS NULL AND
                  tf_inicio IS NULL AND
                  NOT ch1            AND
                  NOT ch2            THEN
                  CALL fn_mensaje("Consulta de Marcas", "Debe seleccionar una Marca", "about")
                  NEXT FIELD nss
               END IF
            
               IF NOT ch1 AND
                  NOT ch2 THEN
                  CALL fn_mensaje("Consulta de Marcas", "Debe seleccionar una Marca", "about") 
                  NEXT FIELD nss
               END IF
            
               IF ch1 AND
                  ch2 THEN
            
                  IF GET_FLDBUF(nss) IS NOT NULL THEN
                     CALL f_w.setElementHidden("group3",0)
                     CALL f_w.setElementHidden("group2",0)
                     LET v_bandera = 0
                     CALL extrae_datos(1)
                     CALL f_w.setElementHidden("group3",1)
                     CALL f_w.setElementHidden("group2",1)
                  ELSE
                     CALL f_w.setElementHidden("group3",0)
                     CALL f_w.setElementHidden("group2",0) 
                     LET vf_inicio = TODAY -3
                     LET v_bandera = 1
                     CALL extrae_datos(1)
                     CALL f_w.setElementHidden("group3",1)
                     CALL f_w.setElementHidden("group2",1)
                  END IF
               ELSE
                  IF ch1 --THEN
                     AND NOT ch2 THEN
                     IF GET_FLDBUF(nss) IS NOT NULL THEN
                        CALL f_w.setElementHidden("group3",0)
                        LET v_bandera = 0
                        CALL extrae_datos(2)
                        CALL f_w.setElementHidden("group3",1)
                     ELSE
                        CALL f_w.setElementHidden("group3",0)
                        LET vf_inicio = TODAY -3
                        LET v_bandera = 1
                        CALL extrae_datos(2)
                        CALL f_w.setElementHidden("group3",1)
                     END IF
                    
                  END IF
            
                  IF NOT ch1 --THEN
                     AND ch2 THEN
                     IF GET_FLDBUF(nss) IS NOT NULL THEN
                        CALL f_w.setElementHidden("group2",0)
                        LET v_bandera = 0
                        CALL extrae_datos(3)
                        CALL f_w.setElementHidden("group2",1)
                     ELSE 
                        CALL f_w.setElementHidden("group2",0)
                        LET vf_inicio = TODAY -3
                        LET v_bandera = 1
                        CALL extrae_datos(3)
                        CALL f_w.setElementHidden("group2",1)
                    END IF
                  END IF
               END IF
                
               LET f_inicio  = ""
            
               DISPLAY BY NAME f_inicio
            
               LET tnss       = NULL
               LET tmarca     = NULL
               LET tf_inicio = NULL
--******************************************************************************
            ELSE 
               CALL fn_mensaje("Atención",v_msj_alerta,"stop")
               LET INT_FLAG = TRUE
                         EXIT DIALOG
            END IF
--******************************************************************************
         ON ACTION CANCEL
            LET f_inicio  = ""
         
            DISPLAY BY NAME f_inicio
         
            LET tnss      = NULL
            LET tmarca    = NULL
            LET tf_inicio = NULL
         
            NEXT FIELD nss
         
         ON ACTION salir
            LET INT_FLAG = TRUE
            EXIT DIALOG

      END DIALOG
    
   END IF  #FIN no llego parametro

   CLOSE WINDOW ctac021

END FUNCTION

FUNCTION extrae_datos(tipo_cons)

   DEFINE tipo_cons           SMALLINT

   DISPLAY "tipo consulta",tipo_cons

   IF tipo_cons = 1 THEN
    
      IF v_bandera = 1 THEN
         CALL consulta_activas1()
         CALL historico11()
         IF total_registro_a = 0 AND
            total_registro_h = 0 THEN
            CALL fn_mensaje("Marcas Activas", "No existen marcas activas ni históricas para ese criterio", "about")
            RETURN
         END IF

         DIALOG ATTRIBUTES(UNBUFFERED)

            DISPLAY ARRAY l_record2 TO tb2.* ATTRIBUTES(COUNT=l_record2.getLength())
               BEFORE DISPLAY
                  CALL dialog.setactionhidden("close",1)
            END DISPLAY

            DISPLAY ARRAY l_record3 TO tb1.* ATTRIBUTES(COUNT=l_record3.getLength())
            END DISPLAY

            ON ACTION cancel
                CALL l_record2.clear()
                CALL l_record3.clear()
                CLEAR FORM
                EXIT DIALOG
         END DIALOG
      END IF

      IF v_bandera = 0 THEN
         CALL consulta_activas()
         CALL historico()
         IF total_registro_a = 0 AND
            total_registro_h = 0 THEN
            CALL fn_mensaje("Marcas Activas", "No existen marcas activas ni históricas para ese criterio", "about")
            RETURN
         END IF

         DIALOG ATTRIBUTES(UNBUFFERED)

            DISPLAY ARRAY l_record2 TO tb2.* ATTRIBUTES(COUNT=l_record2.getLength())
               BEFORE DISPLAY
                  CALL dialog.setactionhidden("close",1)
                
               ON ACTION ACCEPT                                                 --saci2017-1
                  IF l_record2[arr_curr()].marca2 = 221 OR                      --saci2017-1
                     l_record2[arr_curr()].marca2 = 223 OR                      --saci2017-1
                     l_record2[arr_curr()].marca2 = 225 THEN                    --saci2017-1
                     CALL fn_consulta_sol_saldo(l_record2[arr_curr()].marca2,
                                                l_record2[arr_curr()].n_referencia2,
                                                l_record2[arr_curr()].f_inicio2)   --saci2017-1
                  END IF                                                           --saci2017-1
                  
               --ON ACTION CANCEL                                                 --saci2017-1
               --   EXIT DIALOG                                                    --saci2017-1
                  
            END DISPLAY

            DISPLAY ARRAY l_record3 TO tb1.* ATTRIBUTES(COUNT=l_record3.getLength())
            END DISPLAY

            ON ACTION cancel
                CALL l_record2.clear()
                CALL l_record3.clear()
                CLEAR FORM
                EXIT DIALOG
         END DIALOG

      END IF
   END IF
        

   IF tipo_cons = 2 THEN
      IF v_bandera = 0 THEN
         CALL consulta_activas()

         IF (pos-1) >= 1 THEN
            CALL SET_COUNT(pos-1)
         
            DISPLAY ARRAY l_record2 TO tb2.* ATTRIBUTES(ACCEPT=FALSE)
               ON ACTION ACCEPT                                                 --saci2017-1
                  IF l_record2[arr_curr()].marca2 = 221 OR                      --saci2017-1
                     l_record2[arr_curr()].marca2 = 223 OR                      --saci2017-1
                     l_record2[arr_curr()].marca2 = 225 THEN                    --saci2017-1
                     CALL fn_consulta_sol_saldo(l_record2[arr_curr()].marca2,
                                                l_record2[arr_curr()].n_referencia2,
                                                l_record2[arr_curr()].f_inicio2)      --saci2017-1
                  END IF                                                              --saci2017-1
                  
               ON ACTION cancel
                  CALL l_record2.clear()
                  CLEAR FORM
                  EXIT DISPLAY
         
            END DISPLAY
         ELSE
            CALL fn_mensaje("Marcas Activas", "No existen marcas activas para ese criterio", "about")
         END IF
      END IF

      IF v_bandera = 1 THEN
         CALL consulta_activas1()
      
         IF (pos-1) >= 1 THEN
            CALL SET_COUNT(pos-1)
      
            DISPLAY ARRAY l_record2 TO tb2.* ATTRIBUTES(ACCEPT=FALSE)
            ON ACTION cancel
               CALL l_record2.clear()
               CLEAR FORM
               EXIT DISPLAY
      
               END DISPLAY
         ELSE
            CALL fn_mensaje("Marcas Activas", "No existen marcas activas para ese criterio", "about")
         END IF
      END IF
   END IF

   IF tipo_cons = 3 THEN
      IF v_bandera = 0 THEN
         CALL historico()

         IF (pos2-1) >= 1 THEN
            CALL SET_COUNT(pos2-1)

            DISPLAY ARRAY l_record3 TO tb1.* ATTRIBUTES(ACCEPT=FALSE)
               ON ACTION cancel
                  CALL l_record3.clear()
                  CLEAR FORM
                  EXIT DISPLAY
            END DISPLAY
         ELSE
            CALL fn_mensaje("Marcas Históricas", "No existen marcas históricas para ese criterio", "about")
         END IF
      END IF

      IF v_bandera = 1 THEN
         CALL historico11()
      
           IF (pos2-1) >= 1 THEN
               CALL SET_COUNT(pos2-1)
      
               DISPLAY ARRAY l_record3 TO tb1.* ATTRIBUTES(ACCEPT=FALSE)
                  ON ACTION cancel
                      CALL l_record3.clear()
                      CLEAR FORM
                      EXIT DISPLAY
               END DISPLAY
           ELSE
              CALL fn_mensaje("Marcas Históricas", "No existen marcas históricas para ese criterio", "about")
           END IF
      END IF
   END IF

END FUNCTION

--Función modifica: Emilio Abarca
FUNCTION fn_consulta_sol_saldo(p_marca,p_referencia,p_f_inicio)  --saci2017-1

   DEFINE p_marca         SMALLINT 
   DEFINE p_referencia    DECIMAL(9,0)
   DEFINE p_f_inicio      DATE
   DEFINE bnd_sol         SMALLINT 

   DEFINE arr_det_sol DYNAMIC ARRAY OF RECORD
       nss            CHAR(11),
       marca          SMALLINT,
       f_marca        DATE,
       ssv            CHAR(15),
       aivs           DECIMAL(16,2),
       pesos          DECIMAL(12,2),
       estado_desc    CHAR(50),
       f_solicita     DATE,
       respuesta_prcr CHAR(50),
       causal         CHAR(5)
   END RECORD

   DEFINE r_saldo    RECORD
      subcuenta  SMALLINT,
      aivs       DECIMAL(16,2),
      pesos      DECIMAL(12,2)
   END RECORD
   DEFINE r_respuesta_procesar RECORD
      edo_procesar   SMALLINT,
      diagnostico    CHAR(3)
   END RECORD  

   DEFINE v_id_derechohabiente DECIMAL(9,0)
   DEFINE v_folio_liquida      DECIMAL(9,0)
   DEFINE v_aux_edo_procesar   SMALLINT
   DEFINE v_qry_tab            STRING
   DEFINE v_tabla              CHAR(20)
   DEFINE v_afecta_92          CHAR(8)
   DEFINE v_afecta_97          CHAR(8)
   DEFINE v_periodo_pago       CHAR(6)
   DEFINE v_importe_v97        DECIMAL(12,2)
   DEFINE v_precio_fondo       DECIMAL(19,14)
   DEFINE v_modulo_cod         CHAR(20)
   DEFINE v_mensaje            STRING
   DEFINE v_cadena             STRING  
   
   LET bnd_sol              = 0 --No existen solicitud de saldo a PROCESAR
   LET v_id_derechohabiente = NULL 
   LET v_folio_liquida      = 0
   
   OPEN WINDOW ctac022 WITH FORM "CTAC022" ATTRIBUTE(STYLE="dialog")

      --Limipia arreglo
      CALL arr_det_sol.clear()
      INITIALIZE r_respuesta_procesar.* TO NULL

      LET r_saldo.subcuenta = 4
      LET r_saldo.aivs      = 0
      LET r_saldo.pesos     = 0
      LET v_afecta_92       = NULL 
      LET v_afecta_97       = NULL
      LET v_periodo_pago    = NULL 
      LET v_importe_v97     = 0

      --Asigna datos de sfr_marca_Activa
      LET arr_det_sol[1].marca   = p_marca
      LET arr_det_sol[1].f_marca = p_f_inicio
      LET arr_det_sol[1].aivs    = 0
      LET arr_det_sol[1].pesos   = 0
      LET arr_det_sol[1].ssv     = "VIV97"

      CASE
         --TRANSFERENCIA SALDO ACREDITADO
         WHEN p_marca = 221

            #Búsca el registro de la solicitud de saldo
            #70 - Por reenviar
            #80 - Saldo solicitado envío
            #85 - Saldo solicitado reenvío
            #120 - Saldo transferido (Aceptado)

            SELECT afi.nss,
                   acr.id_derechohabiente,
                   acr.folio_liquida,
                   acr.edo_procesar
             INTO arr_det_sol[1].nss,
                   v_id_derechohabiente,
                   v_folio_liquida,
                   v_aux_edo_procesar
             FROM cre_acreditado acr,
                  afi_derechohabiente afi
            WHERE acr.id_cre_acreditado  = p_referencia
              AND acr.id_derechohabiente = afi.id_derechohabiente
              AND edo_procesar IN (70,80,85,120);

            IF(v_id_derechohabiente IS NULL) THEN
               LET bnd_sol = 0 --No existe solicitud de saldo a PROCESAR
            ELSE 
               LET bnd_sol = 1 --Existe solicitud de saldo a PROCESAR

               SELECT estado||"-"||estado_desc
                 INTO arr_det_sol[1].estado_desc
                 FROM cat_maq_credito
                WHERE estado = v_aux_edo_procesar;

               IF v_folio_liquida > 0 THEN
                  --Si es mayor a 0 quiere decir que se liquidó
                  #Búsca la tabla de movimientos para el saldo
                  LET v_qry_tab = "EXECUTE procedure fn_tab_movimiento(0,",v_folio_liquida,",NULL) "

                  PREPARE tab_ta FROM v_qry_tab
                  EXECUTE tab_ta INTO v_tabla

                  --Query para obtener saldo 92/97
                  LET v_qry_tab = " SELECT subcuenta,",
                                   " SUM(monto_acciones),",
                                   " SUM(monto_pesos) ",
                              " FROM ",v_tabla,
                             " WHERE folio_liquida = ",v_folio_liquida,
                               " AND id_derechohabiente = ",v_id_derechohabiente,
                               " AND subcuenta IN (4,8)",
                               " AND fondo_inversion = 11",
                               " GROUP BY subcuenta;"

                  PREPARE saldo_ta FROM v_qry_tab
                  DECLARE cur_ta CURSOR FOR saldo_ta

                  FOREACH cur_ta INTO r_saldo.subcuenta,
                                       r_saldo.aivs,
                                       r_saldo.pesos

                     LET arr_det_sol[1].aivs  = arr_det_sol[1].aivs  + r_saldo.aivs
                     LET arr_det_sol[1].pesos = arr_det_sol[1].pesos + r_saldo.pesos

                     --verifica la subcuenta afectada
                     IF(r_saldo.subcuenta = 4) THEN
                        LET v_afecta_97 = "VIV97"
                     ELSE 
                        LET v_afecta_92 = "VIV92" 
                     END IF 
                     
                  END FOREACH
               ELSE 
                  --Ya que no fué liquidado, obtiene el saldo que tiene actualmente en ssv
                  DECLARE crs_saldo_ta CURSOR FOR 
                  SELECT subcuenta,
                          SUM (monto_acciones),
                          SUM (monto_pesos)
                    FROM cta_movimiento
                   WHERE id_derechohabiente = v_id_derechohabiente
                     AND subcuenta IN (4,8)
                     AND fondo_inversion = 11
                     GROUP BY subcuenta;

                  FOREACH crs_saldo_ta INTO r_saldo.subcuenta,
                                       r_saldo.aivs,
                                       r_saldo.pesos
                                       
                     LET arr_det_sol[1].aivs  = arr_det_sol[1].aivs  + r_saldo.aivs
                     LET arr_det_sol[1].pesos = arr_det_sol[1].pesos + r_saldo.pesos

                     --verifica la subcuenta afectada
                     IF(r_saldo.subcuenta = 4) THEN
                        LET v_afecta_97 = "VIV97"
                     ELSE 
                        LET v_afecta_92 = "VIV92" 
                     END IF 
                     
                  END FOREACH 
                  
               END IF
 
               --Evalúa las subcuentas afectadas
               CASE 
                  WHEN v_afecta_92 = "VIV92" AND v_afecta_97 = "VIV97"
                     LET arr_det_sol[1].ssv = "VIV92 / VIV97"
                     
                  WHEN v_afecta_92 = "VIV92" AND v_afecta_97 = NULL 
                     LET arr_det_sol[1].ssv = "VIV92"
                     
                  WHEN v_afecta_92 = NULL AND v_afecta_97 = "VIV97"
                     LET arr_det_sol[1].ssv = "VIV97"
               END CASE 

               --Obtiene la última fecha de cuando se hizo la solicitud a PROCESAR
               SELECT MAX(f_proceso)
                 INTO arr_det_sol[1].f_solicita
                 FROM cre_his_solic_sdo
                WHERE id_referencia      = p_referencia
                  AND id_derechohabiente = v_id_derechohabiente
                  AND modulo_cod = 'TA';

               #Búsca respuesta por parte de PROCESAR,
               #En caso no no obtenerla es porque el registro se está solicitando recientemente
               SELECT FIRST 1 edo_procesar,diagnostico
                 INTO r_respuesta_procesar.edo_procesar,r_respuesta_procesar.diagnostico
                 FROM crE_his_acreditado
                WHERE id_cre_acreditado  = p_referencia
                  AND edo_procesar IN (10,90,100,110,120)
                  AND f_proceso >= arr_det_sol[1].f_solicita
                  AND id_cre_ctr_archivo IN (SELECT id_cre_ctr_archivo FROM cre_ctr_archivo WHERE id_proceso = 201)
                  ORDER BY f_proceso DESC;   

               IF(r_respuesta_procesar.edo_procesar IS NOT NULL) THEN

                  #BUsca descripción
                  SELECT estado||"-"||estado_desc
                    INTO arr_det_sol[1].respuesta_prcr
                    FROM cat_maq_credito
                    WHERE estado = r_respuesta_procesar.edo_procesar;

                  CASE
                     WHEN r_respuesta_procesar.edo_procesar = 120
                        LET arr_det_sol[1].respuesta_prcr   = "120 - ACEPTADO" --resetea la respuesta procesar
                        LET arr_det_sol[1].causal           = NULL
                     
                     WHEN r_respuesta_procesar.edo_procesar = 90 OR 
                          r_respuesta_procesar.edo_procesar  = 100 --SALDO RECHAZADO
                        LET arr_det_sol[1].causal = r_respuesta_procesar.diagnostico;
                     
                     WHEN r_respuesta_procesar.edo_procesar = 110 --NO ATENDIDA
                        LET arr_det_sol[1].causal          = NULL
                  END CASE 
               ELSE 
                  -- Si es nulo no se ha realizado la solicitud a PROCESAR
                  LET arr_det_sol[1].respuesta_prcr = NULL
                  LET arr_det_sol[1].causal         = NULL
               END IF 
            END IF -- Fin condición marca 221

         #(223) USO DE GARANTÍA 43 BIS/ (225)USO DE ANUALIDAD
         WHEN p_marca = 223 OR p_marca = 225

            #Búsca el registro de la solicitud de saldo
            --70 - Por reenviar
            --80 - Saldo solicitado envío
            --85 - Saldo solicitado reenvío
            --120 - Saldo transferido (Aceptado)
            SELECT afi.nss,
                    cre.id_derechohabiente,
                    cre.folio_liquida,
                    cre.periodo_pago,
                    cre.importe_v97,
                    cre.edo_procesar
              INTO arr_det_sol[1].nss,
                    v_id_derechohabiente,
                    v_folio_liquida,
                    v_periodo_pago,
                    v_importe_v97,
                    v_aux_edo_procesar
             FROM  cre_uso_garantia cre,
                   afi_derechohabiente afi
            WHERE cre.id_cre_uso_garantia = p_referencia
              AND cre.id_derechohabiente  = afi.id_derechohabiente
              AND cre.edo_procesar IN (10,70,80,85,120);

            IF(v_id_derechohabiente IS NULL) THEN
               LET bnd_sol = 0 --No existe solicitud de saldo a PROCESAR
            ELSE 
               LET bnd_sol = 1 --Existe solicitud de saldo a PROCESAR

               SELECT estado||"-"||estado_desc
                 INTO arr_det_sol[1].estado_desc
                 FROM cat_maq_credito
                WHERE estado = v_aux_edo_procesar;

               SELECT precio_fondo
                 INTO v_precio_fondo
                 FROM glo_valor_fondo
                WHERE f_valuacion = arr_det_sol[1].f_marca
                  AND fondo       = 11;

               --Si ya fué liquidado
               IF(v_folio_liquida > 0) THEN

                  IF(p_marca = 223) AND (v_periodo_pago IS NOT NULL) THEN
                     #Hay que verificar si el registro no se cambió a de 225 a 223 en la tabla cre_cambio_marca
                     #Porque si viene de la 225 en cta_movimientoxx de id_referencia se hace el filtro por el id_cre_uso_garantia
                    { SELECT COUNT(*)
                       INTO v_total
                       FROM cre_cambio_marca
                      WHERE id_referencia      = p_referencia
                        AND id_derechohabiente = v_id_derechohabiente
                        AND marca_origen = 225
                        AND marca_final  = 223;

                        IF(v_total >= 1) THEN 
                           LET v_cadena = " AND id_referencia = ",p_referencia CLIPPED
                        ELSE 
                           LET v_cadena =  " AND id_referencia = ",v_periodo_pago CLIPPED
                        END IF }
                        
                     LET v_cadena =  " AND id_referencia = ",v_periodo_pago CLIPPED
                  ELSE 
                     LET v_cadena = NULL
                  END IF 
                  #Búsca la tabla de movimientos para el saldo
                  LET v_qry_tab = "EXECUTE procedure fn_tab_movimiento(0,",v_folio_liquida,",NULL) "

                  PREPARE tab_ag FROM v_qry_tab
                  EXECUTE tab_ag INTO v_tabla

                  --LET v_mensaje = "id_cre:",p_referencia," id_dh",v_id_derechohabiente," folio = ",v_folio_liquida," tabla",v_tabla
                  --CALL fn_mensaje("",v_mensaje,"")
                  
                  --Query para obtener saldo 92/97
                  LET v_qry_tab = " SELECT subcuenta,",
                                   " SUM(monto_acciones),",
                                   " SUM(monto_pesos) ",
                              " FROM ",v_tabla,
                             " WHERE folio_liquida = ",v_folio_liquida,
                               v_cadena,
                               " AND id_derechohabiente = ",v_id_derechohabiente,
                               " AND subcuenta IN (4,8)",
                               " AND fondo_inversion = 11",
                               " GROUP BY subcuenta;"

                  PREPARE saldo_ag FROM v_qry_tab
                  DECLARE cur_ag CURSOR FOR saldo_ag

                  FOREACH cur_ag INTO r_saldo.subcuenta,
                                       r_saldo.aivs,
                                       r_saldo.pesos

                     LET arr_det_sol[1].aivs  = arr_det_sol[1].aivs  + r_saldo.aivs
                     LET arr_det_sol[1].pesos = arr_det_sol[1].pesos + r_saldo.pesos

                     --verifica la subcuenta afectada
                     IF(r_saldo.subcuenta = 4) THEN
                        LET v_afecta_97 = "VIV97"
                     ELSE 
                        LET v_afecta_92 = "VIV92" 
                     END IF 
                     
                  END FOREACH

                  IF(arr_det_sol[1].aivs = 0) AND (arr_det_sol[1].pesos = 0) THEN
                     LET arr_det_sol[1].pesos = v_importe_v97
                     LET arr_det_sol[1].aivs = (arr_det_sol[1].pesos / v_precio_fondo)
                     LET v_afecta_92 = NULL
                     LET v_afecta_97 = "VIV97"
                  END IF 
               ELSE 
                  -- Le pasa el monto original que se está solicitando
                  LET arr_det_sol[1].pesos = v_importe_v97
                  LET arr_det_sol[1].aivs = (arr_det_sol[1].pesos / v_precio_fondo)
                  
                  LET v_afecta_92 = NULL
                  LET v_afecta_97 = "VIV97"
                  
               END IF

               --Evalúa las subcuentas afectadas
               CASE 
                  WHEN v_afecta_92 = "VIV92" AND v_afecta_97 = "VIV97"
                     LET arr_det_sol[1].ssv = "VIV92 / VIV97"
                     
                  WHEN v_afecta_92 = "VIV92" AND v_afecta_97 = NULL 
                     LET arr_det_sol[1].ssv = "VIV92"
                     
                  WHEN v_afecta_92 = NULL AND v_afecta_97 = "VIV97"
                     LET arr_det_sol[1].ssv = "VIV97"
               END CASE 

               IF(p_marca = 223) THEN 
                  LET v_modulo_cod = "IN ('UG','AG');"
               ELSE 
                  LET v_modulo_cod = "= 'UA';"
               END IF 

               --Obtiene la última fecha de cuando se hizo la solicitud a PROCESAR
               LET v_qry_tab = " SELECT MAX(f_proceso)
                                   FROM cre_his_solic_sdo
                                  WHERE id_referencia      = ",p_referencia,
                                  " AND id_derechohabiente = ",v_id_derechohabiente,
                                  " AND modulo_cod ",v_modulo_cod
                                  
               PREPARE prp_f_solic FROM v_qry_tab
               EXECUTE prp_f_solic INTO arr_det_sol[1].f_solicita
               
               #Búsca respuesta por parte de PROCESAR,
               #En caso no no obtenerla es porque el registro se está solicitando recientemente
               SELECT FIRST 1 edo_procesar,diagnostico
                 INTO r_respuesta_procesar.edo_procesar,r_respuesta_procesar.diagnostico
                 FROM crE_uso_garantia
                WHERE id_derechohabiente  = v_id_derechohabiente
                  AND edo_procesar IN (90,100,110,115,120)
                  AND f_proceso >= arr_det_sol[1].f_solicita
                  AND periodo_pago = v_periodo_pago
                  AND id_cre_ctr_archivo IN (SELECT id_cre_ctr_archivo FROM cre_ctr_archivo 
                                               WHERE id_proceso IN (301,1202)
                                                 AND nom_archivo NOT MATCHES '*.rcu'
                                                 AND nom_archivo NOT MATCHES '*.usog')
                  ORDER BY f_proceso,edo_procesar;

               IF(r_respuesta_procesar.edo_procesar IS NOT NULL) THEN

               #BUsca descripción
               SELECT estado||" - "||estado_desc
                 INTO arr_det_sol[1].respuesta_prcr
                 FROM cat_maq_credito
                 WHERE estado = r_respuesta_procesar.edo_procesar;

               CASE
                  WHEN r_respuesta_procesar.edo_procesar = 120
                     LET arr_det_sol[1].respuesta_prcr  = "120 - ACEPTADO" --resetea la respuesta procesar
                     LET arr_det_sol[1].causal          = NULL
                     
                  WHEN r_respuesta_procesar.edo_procesar = 90 OR 
                       r_respuesta_procesar.edo_procesar  = 100 --SALDO RECHAZADO
                     LET arr_det_sol[1].causal = r_respuesta_procesar.diagnostico;
                     
                  WHEN r_respuesta_procesar.edo_procesar = 110 --NO ATENDIDA
                     LET arr_det_sol[1].causal          = NULL
               END CASE 

               ELSE 
                  -- Si es nulo no se ha realizado la solicitud a PROCESAR
                  LET arr_det_sol[1].respuesta_prcr = NULL
                  LET arr_det_sol[1].causal         = NULL
               END IF
            END IF 
            
      END CASE 

   IF(bnd_sol = 0) THEN
      CALL fn_mensaje("","No se encontró la solicitud de saldo a PROCESAR","")
   ELSE 
      DISPLAY ARRAY arr_det_sol TO table1.* ATTRIBUTES(COUNT=arr_det_sol.getLength(),ACCEPT = FALSE)
            
         ON ACTION CANCEL
            EXIT DISPLAY
  
      END DISPLAY
      
   END IF 

   CLOSE WINDOW ctac022
   
END FUNCTION

FUNCTION consulta_activas()
#función para buscat marcas activas

    LET sel_where = "SELECT '',c.nss,a.marca,b.descripcion_marca,a.f_inicio, ",
                    "       a.h_inicio,a.n_referencia,a.usuario_marca        ",
                    "FROM   sfr_marca_activa    a,                           ",
                    "       sfr_marca           b,                           ",
                    "       afi_derechohabiente c                            ",
                    "WHERE  ",cla_where,
                    "  AND  c.id_derechohabiente = a.id_derechohabiente      ",
                    "  AND  a.marca = b.marca                                ",
                    "  AND  c.tipo_trabajador   <> 'V'                       ",
                    "  AND  c.origen_afiliacion <> 'S'                       ",
                    "ORDER BY f_inicio DESC, marca, nss "

    {LET sel_where = "SELECT '', c.nss, a.marca, b.descripcion_marca, ",
                          " a.f_inicio, a.h_inicio, ",
                          " a.n_referencia, a.usuario_marca ",
                     " FROM sfr_marca_activa a, sfr_marca b, afi_derechohabiente c ",
                    " WHERE ", cla_where CLIPPED,
                      " AND a.id_derechohabiente = c.id_derechohabiente ", 
                      " AND a.marca = b.marca ",
                      " AND c.tipo_trabajador <> 'V' ",   -- nss virtual
                      " AND c.origen_afiliacion <> 'S' ",
                     --- " AND a.f_inicio = '", vf_inicio, "' ",
                   " ORDER BY f_inicio DESC, marca, nss "}

--LET sel_where = "echo ",sel_where clipped, " > y.sql"
--RUN sel_where

    PREPARE qry_act FROM sel_where
    DECLARE cursor_a CURSOR FOR qry_act

    LET pos = 1

    FOREACH cursor_a INTO l_record2[pos].*
        LET l_record2[pos].id_registro_a = pos
        LET pos                          = pos + 1

        IF pos >= 36767 THEN
            LET total_registro_a = pos - 1

            DISPLAY BY NAME total_registro_a

            CALL fn_mensaje("Marcas Activas", "Fue sobrepasada la cantidad máxima del arreglo", "about")

            LET INT_FLAG = TRUE
            RETURN
        END IF
    END FOREACH

    LET total_registro_a = pos - 1
    DISPLAY BY NAME total_registro_a

    LET cont = l_record2.getlength()

    IF l_record2[cont].nss2 IS NULL THEN
        CALL l_record2.deleteelement(cont)
    END IF

END FUNCTION

{FUNCTION historico1()
#función para buscar marcas históricas

    CALL l_record3.clear()

    LET sel_hist = "SELECT '',g.nss,a.marca,c.descripcion_marca,a.f_inicio,   ",
                   "       a.h_inicio,a.f_fin,e.estado_marca_desc,f.rch_desc, ",
                   "       a.marca_causa,d.descripcion_marca,a.f_marca_causa, ",
                   "       a.n_referencia,a.usuario_marca, a.usuario_desmarca ",
                   "FROM   sfr_marca_historica    a,                          ",
                   "       sfr_marca              c,                          ",
                   "       sfr_marca              d,                          ",
                   "       afi_derechohabiente    g,                          ",
                   "       OUTER cat_estado_marca e,                          ",
                   "       OUTER cat_rch_marca    f                           ",
                   "WHERE  a.id_derechohabiente = g.id_derechohabiente        ",
                   "  AND  a.marca              = c.marca                     ",
                   "  AND  a.marca_causa        = d.marca                     ",
                   "  AND  a.estado_marca       = e.estado_marca              ",
                   "  AND  a.rch_cod            = f.rch_cod                   ",
                   "  AND  a.f_fin IS NOT NULL                                ",
                   "  AND  g.nss          = '", vnss         ,"' ",
                   "  AND  a.marca        = " , vmarca       ,
                   "  AND  a.f_inicio     = '", vf_inicio    ,"' ",
                   "  AND  a.n_referencia = " , vn_referencia,
                   "ORDER BY f_inicio desc, marca, nss "

    LET sel_hist = "SELECT  '', g.nss, a.marca, c.descripcion_marca, ",
                         " a.f_inicio, a.h_inicio, a.f_fin, ",
                         " e.estado_marca_desc, f.rch_desc, a.marca_causa, ",
                         " d.descripcion_marca, a.f_causa, a.n_referencia, ",
                         " a.usuario_marca, a.usuario_desmarca ", 
                    " FROM sfr_marca_historica a, sfr_marca c, sfr_marca d, afi_derechohabeinte g ",
                   " OUTER cat_estado_marca e, OUTER cat_rch_marca f ",
                   " WHERE g.nss = '", vnss, "' ",
                     " AND a.id_derechohabiente = g.id_derechohabiente ",
                     " AND a.marca = ", vmarca,
                     " AND a.f_inicio = '", vf_inicio, "' ",
                     " AND a.n_referencia = ", vn_referencia,
                     " AND a.marca = c.marca ",
                     " AND a.marca_causa = d.marca ",
                     " AND a.f_fin IS NOT NULL ",
                     " AND a.estado_marca = e.marca_estado ",
                     " AND a.rechazo_cod = f.rechazo_cod ",
                 " ORDER BY f_inicio desc, marca, nss "

    PREPARE qry_hisnh FROM sel_hist
    DECLARE cursor_nh CURSOR FOR qry_hisnh

    LET pos2 = 1

    FOREACH cursor_nh INTO l_record3[pos2].*
        LET l_record3[pos2].id_registro_h = pos2

        IF l_record3[pos2].estado_marca3 LIKE 'IMPROCEDENTE%' OR l_record3[pos2].estado_marca3 LIKE "RECHAZO%" THEN
            IF l_record3[pos2].rch_cod3 = 'PROCEDENTE' THEN
                LET l_record3[pos2].rch_cod3 = 'IMPROCEDENTE'
            END IF
        END IF

        LET pos2 = pos2 + 1

        IF pos2 >= 36767 THEN
            LET total_registro_h = pos2 - 1

            DISPLAY BY NAME total_registro_h

            CALL fn_mensaje("Marcas Históricas", "Fue sobrepasada la cantidad máxima del arreglo", "about")

            LET INT_FLAG = TRUE

            RETURN
        END IF

    END FOREACH

    LET total_registro_h = pos2 - 1

    DISPLAY BY NAME total_registro_h

    LET cont = l_record3.getlength()

    IF l_record3[cont].nss3 IS NULL THEN
        CALL l_record3.deleteelement(cont)
    END IF

END FUNCTION}

FUNCTION historico()
#h------------------

    LET sel_hist = "SELECT '',c.nss,a.marca,g.descripcion_marca,a.f_inicio,  ",
                   "       a.h_inicio,a.f_fin,e.estado_marca_desc,f.rch_desc, ",
                   "       a.marca_causa,d.descripcion_marca,a.f_marca_causa, ",
                   "       a.n_referencia,'',a.usuario_marca,a.usuario_desmarca ",
                   "FROM   sfr_marca_historica    a,                          ",
                   "       afi_derechohabiente    c,                          ",
                   "       OUTER sfr_marca        g,                          ",
                   "       OUTER sfr_marca        d,                          ",
                   "       OUTER cat_estado_marca e,                          ",
                   "       OUTER cat_rch_marca    f                           ",
                   "WHERE ",cla_where CLIPPED,
                   "  AND  c.id_derechohabiente = a.id_derechohabiente        ",
                   "  AND  a.marca              = g.marca                     ",
                   "  AND  a.marca_causa        = d.marca                     ",
                   "  AND  a.estado_marca       = e.estado_marca              ",
                   "  AND  a.rch_cod            = f.rch_cod                   ",
                   "  AND  a.f_fin IS NOT NULL                                ",
                   --"  AND a.f_inicio = '", vf_inicio, "' ",
                   "  AND  c.tipo_trabajador   <> 'V'                         ",
                   "  AND  c.origen_afiliacion <> 'S'                         ",
                   "ORDER  BY f_inicio desc, marca, nss                       "

    {LET sel_hist = "SELECT '', c.nss, a.marca, g.descripcion_marca, ",
                         " a.f_inicio, a.h_inicio, a.f_fin, ",
                         " e.estado_marca_desc, f.rch_desc, a.marca_causa, ",
                         " d.descripcion_marca, a.f_marca_causa, a.n_referencia, ",
                         " a.usuario_marca, a.usuario_desmarca ", 
                    " FROM sfr_marca_historica a, afi_derechohabiente c, ",
                    " OUTER sfr_marca g, OUTER sfr_marca d, ",
                    " OUTER cat_estado_marca e, OUTER cat_rch_marca f ",
                   " WHERE ",cla_where CLIPPED,
                     " AND a.id_derechohabiente = c.id_derechohabiente ",
                     " AND a.marca = g.marca ",
                    -- " AND a.f_inicio = '", vf_inicio, "' ",
                     " AND a.marca_causa = d.marca ",
                     " AND a.f_fin IS NOT NULL ",
                     " AND a.estado_marca = e.estado_marca ",
                     " AND a.rch_cod = f.rch_cod ",
                     " AND c.tipo_trabajador <> 'V' ",   -- nss virtual
                     " AND c.origen_afiliacion <> 'S' ",
                   " ORDER BY f_inicio desc, marca, nss "}

--LET sel_hist = "echo ",sel_hist clipped, " > x.sql"
--RUN sel_hist

    PREPARE qry_his FROM sel_hist
    DECLARE cursor_h CURSOR FOR qry_his

    LET pos2 = 1

    FOREACH cursor_h INTO l_record3[pos2].*
        LET l_record3[pos2].id_registro_h = pos2

        IF l_record3[pos2].estado_marca3 LIKE 'IMPROCEDENTE%' OR l_record3[pos2].estado_marca3 LIKE "RECHAZO%" THEN
            IF l_record3[pos2].rch_cod3 = 'PROCEDENTE' THEN
                LET l_record3[pos2].rch_cod3 = 'IMPROCEDENTE'
            END IF
        END IF

        #Obtiene el motivo de la desmarca en caso de ser un solicitud especial
        SELECT MAX(cat.motivo_desc)
          INTO l_record3[pos2].motivo_desmarca
          FROM cre_sol_marca_esp esp,
               cat_motivo_marcaje_esp cat
         WHERE esp.n_referencia = l_record3[pos2].n_referencia3
           AND esp.marca        = l_record3[pos2].marca3
           AND esp.f_ini_marca  = l_record3[pos2].f_inicio3
           AND esp.motivo       = cat.motivo;
           
        LET pos2 = pos2 + 1

        IF pos2 >= 1001 THEN
            LET total_registro_h = pos2 - 1

            DISPLAY BY NAME total_registro_h

            CALL fn_mensaje("Marcas Históricas", "Fue sobrepasada la cantidad máxima del arreglo", "about")

            LET INT_FLAG = TRUE

            RETURN
        END IF

    END FOREACH

    LET total_registro_h = pos2 - 1
    DISPLAY BY NAME total_registro_h

    LET cont = l_record3.getlength()

    IF l_record3[cont].nss3 IS NULL THEN
        CALL l_record3.deleteelement(cont)
    END IF

END FUNCTION

FUNCTION fn_valida_nss()

   DEFINE v_funcion_nss             STRING
   DEFINE v_cadena                  CHAR(11)
   DEFINE v_tpo_consulta            SMALLINT

   LET v_tpo_consulta = 5 

   LET v_funcion_nss = "EXECUTE PROCEDURE sp_valida_nss_rojo(?,?,?)"

   PREPARE prp_valida_nss FROM v_funcion_nss
   --FOR a= 1 TO v_paso_nss.getLength()
   LET v_cadena = v_paso_nss
   EXECUTE prp_valida_nss USING v_cadena, v_tpo_consulta, g_usuario
                           INTO r_valida_nss
   --END FOR 

END FUNCTION

FUNCTION consulta_activas1()
DISPLAY "consulta_activas1"
#función para buscat marcas activas

    
    LET sel_where = "SELECT '',c.nss,a.marca,b.descripcion_marca,a.f_inicio, ",
                    "       a.h_inicio,a.n_referencia,a.usuario_marca        ",
                    "FROM   sfr_marca_activa    a,                           ",
                    "       sfr_marca           b,                           ",
                    "       afi_derechohabiente c                            ",
                    "WHERE ", cla_where CLIPPED,
                    "  AND  c.id_derechohabiente = a.id_derechohabiente      ",
                    "  AND  a.marca = b.marca                                ",
                    "  AND  c.tipo_trabajador   <> 'V'                       ",
                    "  AND  c.origen_afiliacion <> 'S'                       ",
                    --" AND a.f_inicio >= '", vf_inicio, "' ",
                    "ORDER BY f_inicio DESC, marca, nss "

    {LET sel_where = "SELECT '', c.nss, a.marca, b.descripcion_marca, ",
                          " a.f_inicio, a.h_inicio, ",
                          " a.n_referencia, a.usuario_marca ",
                     " FROM sfr_marca_activa a, sfr_marca b, afi_derechohabiente c ",
                    " WHERE ", cla_where CLIPPED,
                      " AND a.id_derechohabiente = c.id_derechohabiente ", 
                      " AND a.marca = b.marca ",
                      " AND c.tipo_trabajador <> 'V' ", -- nss virtual
                      " AND c.origen_afiliacion <> 'S' ",                      
                    -- " AND a.f_inicio >= '", vf_inicio, "' ",
                   " ORDER BY f_inicio DESC, marca, nss "}

--LET sel_where = "echo ",sel_where clipped, " > y.sql"
--RUN sel_where

    PREPARE qry_act1 FROM sel_where
    DECLARE cursor_a1 CURSOR FOR qry_act1

    LET pos = 1

    FOREACH cursor_a1 INTO l_record2[pos].*
        LET l_record2[pos].id_registro_a = pos
        LET pos                          = pos + 1


        IF pos >= 36767 THEN
        --IF pos >= 32767 THEN

            LET total_registro_a = pos - 1

            DISPLAY BY NAME total_registro_a

            CALL fn_mensaje("Marcas Activas", "Fue sobrepasada la cantidad máxima del arreglo", "about")

            LET INT_FLAG = TRUE
            RETURN
        END IF
    END FOREACH

    LET total_registro_a = pos - 1
    DISPLAY BY NAME total_registro_a

    LET cont = l_record2.getlength()

    IF l_record2[cont].nss2 IS NULL THEN
        CALL l_record2.deleteelement(cont)
    END IF

END FUNCTION

FUNCTION historico11()
DISPLAY "historico11"
#h------------------

    LET sel_hist = "SELECT '',c.nss,a.marca,g.descripcion_marca,a.f_inicio,   ",
                   "       a.h_inicio,a.f_fin,e.estado_marca_desc,f.rch_desc,  ",
                   "       a.marca_causa,d.descripcion_marca,a.f_marca_causa,  ",
                   "       a.n_referencia,'',a.usuario_marca,a.usuario_desmarca ",
                   "FROM   sfr_marca_historica    a,                          ",
                   "       afi_derechohabiente    c,                          ",
                   "       OUTER sfr_marca        g,                          ",
                   "       OUTER sfr_marca        d,                          ",
                   "       OUTER cat_estado_marca e,                          ",
                   "       OUTER cat_rch_marca    f                           ",
                   "WHERE ",cla_where CLIPPED,
                   "  AND  c.id_derechohabiente = a.id_derechohabiente        ",
                   "  AND  a.marca        = g.marca                           ",
                   "  AND  a.marca_causa  = d.marca                           ",
                   "  AND  a.estado_marca = e.estado_marca                    ",
                   "  AND  a.rch_cod      = f.rch_cod                         ",
                   "  AND  a.f_fin IS NOT NULL                                ",
                   "  AND  c.tipo_trabajador   <> 'V'                         ",
                   "  AND  c.origen_afiliacion <> 'S'                         ",
                   --"  AND a.f_inicio >= '", vf_inicio, "' ",
                   " ORDER BY f_inicio desc, marca, nss                       "

    {LET sel_hist = "SELECT '', c.nss, a.marca, g.descripcion_marca, ",
                         " a.f_inicio, a.h_inicio, a.f_fin, ",
                         " e.estado_marca_desc, f.rch_desc, a.marca_causa, ",
                         " d.descripcion_marca, a.f_marca_causa, a.n_referencia, ",
                         " a.usuario_marca, a.usuario_desmarca ", 
                    " FROM sfr_marca_historica a, afi_derechohabiente c, ",
                    " OUTER sfr_marca g, OUTER sfr_marca d, ",
                    " OUTER cat_estado_marca e, OUTER cat_rch_marca f ",
                   " WHERE ",cla_where CLIPPED,
                     " AND a.id_derechohabiente = c.id_derechohabiente ",
                     " AND a.marca = g.marca ",
                     --" AND a.f_inicio >= '", vf_inicio, "' ",
                     " AND a.marca_causa = d.marca ",
                     " AND a.f_fin IS NOT NULL ",
                     " AND a.estado_marca = e.estado_marca ",
                     " AND a.rch_cod = f.rch_cod ",
                     " AND c.tipo_trabajador <> 'V' ", -- nss virtual
                     " AND c.origen_afiliacion <> 'S' ",
                   " ORDER BY f_inicio desc, marca, nss "}

--LET sel_hist = "echo ",sel_hist clipped, " > x.sql"
--RUN sel_hist

    PREPARE qry_his1 FROM sel_hist
    DECLARE cursor_h1 CURSOR FOR qry_his1

    LET pos2 = 1

    FOREACH cursor_h1 INTO l_record3[pos2].*
        LET l_record3[pos2].id_registro_h = pos2

        IF l_record3[pos2].estado_marca3 LIKE 'IMPROCEDENTE%' OR l_record3[pos2].estado_marca3 LIKE "RECHAZO%" THEN
            IF l_record3[pos2].rch_cod3 = 'PROCEDENTE' THEN
                LET l_record3[pos2].rch_cod3 = 'IMPROCEDENTE'
            END IF
        END IF

        #Obtiene el motivo de la desmarca en caso de ser un solicitud especial
        SELECT MAX(cat.motivo_desc)
          INTO l_record3[pos2].motivo_desmarca
          FROM cre_sol_marca_esp esp,
               cat_motivo_marcaje_esp cat
         WHERE esp.n_referencia = l_record3[pos2].n_referencia3
           AND esp.marca        = l_record3[pos2].marca3
           AND esp.f_ini_marca  = l_record3[pos2].f_inicio3
           AND esp.motivo       = cat.motivo;
           
        LET pos2 = pos2 + 1

        IF pos2 >= 1001 THEN
            LET total_registro_h = pos2 - 1

            DISPLAY BY NAME total_registro_h

            CALL fn_mensaje("Marcas Históricas", "Fue sobrepasada la cantidad máxima del arreglo", "about")

            LET INT_FLAG = TRUE

            RETURN
        END IF

    END FOREACH

    LET total_registro_h = pos2 - 1
    DISPLAY BY NAME total_registro_h

    LET cont = l_record3.getlength()

    IF l_record3[cont].nss3 IS NULL THEN
        CALL l_record3.deleteelement(cont)
    END IF

END FUNCTION

PRIVATE FUNCTION fn_valida_caracteres(p_campo)

   DEFINE p_campo                   STRING

   RETURN p_campo.getIndexOf("*",1)

END FUNCTION
