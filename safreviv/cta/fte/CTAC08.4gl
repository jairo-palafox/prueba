###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC08                                                  #
#Objetivo          => CONSULTA DE MARCAS                                      #
#Fecha Inicio      => 30-ABRIL-2015                                           #
###############################################################################
--=============================================================================
-- Modificacion
-- Fecha ultima modificacion: 25/09/2015, solo se consultaran las marcas 802 y 201
DATABASE safre_viv

GLOBALS

    DEFINE total_registro_a   INTEGER
    DEFINE total_registro_h   INTEGER
    DEFINE cont               INTEGER

    DEFINE vmarca             SMALLINT
    DEFINE tmarca             SMALLINT
    DEFINE pos                SMALLINT
    DEFINE pos2               SMALLINT
    DEFINE ch1                SMALLINT
    DEFINE ch2                SMALLINT

    DEFINE vrfc               CHAR(13)
    DEFINE trfc               CHAR(13)
    DEFINE vnss               CHAR(11)
    DEFINE tnss               CHAR(11)
    
    DEFINE g_usuario          CHAR(12)
    DEFINE cla_where          CHAR(200)
    DEFINE sel_where          STRING --CHAR(1000)
    DEFINE sel_hist           STRING --CHAR(1000)

    DEFINE g_hoy              DATE
    DEFINE tf_inicio          DATE
    DEFINE f_inicio           DATE
    DEFINE tf_fin             DATE
    DEFINE f_fin              DATE

    DEFINE g_parametro RECORD
        ruta_listados         CHAR(40)
    END RECORD

    DEFINE l_record2 DYNAMIC ARRAY OF RECORD
        id_registro_a         INTEGER,
        rfc2                  CHAR(13),
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
        rfc3                  CHAR(13),
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
        usuario_marca3        CHAR(8),
        usuario_desmarca      CHAR(8)
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
DEFINE r_valida_rfc SMALLINT
DEFINE v_paso_rfc STRING
DEFINE r_valida_nss SMALLINT
DEFINE v_paso_nss STRING
--******************************************************************************

END GLOBALS

MAIN

    LET g_usuario            = ARG_VAL(1)
    LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
    LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
    LET g_id_derechohabiente = ARG_VAL(4)

    #CALL STARTLOG(g_usuario CLIPPED ||".CTAC08.log")

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

    #función principal del programa

    --OPEN WINDOW ctac081 WITH FORM "CTAC081" ATTRIBUTE(STYLE="dialog")
   OPEN WINDOW ctac081 WITH FORM "CTAC081" ATTRIBUTE(STYLE="")

    LET w = ui.Window.forName("ctac081")
    
    
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
                       " WHERE marca in (201,802)",
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

    #Se valida si llego como parametro el id_derechohabiente para saber si se ejecuta
    #Directo la consulta a marcas activas
    IF g_id_derechohabiente IS NOT NULL AND g_id_derechohabiente <> 0 THEN
      LET cla_where = "a.id_derechohabiente = ", g_id_derechohabiente
   
      SELECT
         rfc, nss
      INTO
         trfc, tnss
      FROM afi_fondo72 
      WHERE id_derechohabiente = g_id_derechohabiente
--******************************************************************************
      LET v_paso_rfc = trfc
      LET v_paso_nss = tnss
      CALL fn_valida_rfc()
      CALL fn_valida_nss()

      IF r_valida_rfc = 0 THEN
--******************************************************************************
      

          #DISPLAY g_hoy TO f_inicio
          DISPLAY trfc TO rfc
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

       CONSTRUCT cla_where ON c.rfc, a.marca, c.nss, a.f_inicio, a.f_inicio
                         FROM rfc, marca, nss, f_inicio, f_fin

           BEFORE CONSTRUCT
             LET trfc      = NULL
             LET tnss      = NULL
             LET tmarca    = NULL
             LET tf_inicio = NULL
             LET tf_fin    = NULL 

             LET f_inicio = TODAY
             LET f_fin    = TODAY 
             DISPLAY BY NAME f_inicio
             DISPLAY BY NAME f_fin 
             CALL FGL_DIALOG_SETBUFFER(f_inicio)
             CALL FGL_DIALOG_SETBUFFER(f_fin)

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

       ON ACTION accept
           LET cla_where = cla_where CLIPPED
--******************************************************************************
           DISPLAY cla_where

           LET v_paso_rfc = GET_FLDBUF(rfc)
           LET v_paso_nss = GET_FLDBUF(nss)

           IF v_paso_rfc IS NOT NULL THEN
               DISPLAY "V_PASO_RFC>", v_paso_rfc
               CALL fn_valida_rfc()
           END IF
           IF v_paso_nss IS NOT NULL THEN
               DISPLAY "V_PASO_NSS>", v_paso_nss
               CALL fn_valida_nss()
           END IF 

           IF r_valida_rfc = 0 OR r_valida_nss = 0 THEN
--******************************************************************************

               --IF NOT FIELD_TOUCHED(rfc) THEN
                   --LET trfc = FGL_DIALOG_GETBUFFER()
                   --DISPLAY "FGL_DIALOG_GETBUFFER RFC>", trfc
               --END IF
               --LET trfc =  GET_FLDBUF(rfc)
               --IF NOT FIELD_TOUCHED(nss) THEN
                   --LET tnss = FGL_DIALOG_GETBUFFER()
                   --DISPLAY "FGL_DIALOG_GETBUFFER NSS>", tnss
               --END IF
--
               --IF NOT FIELD_TOUCHED(marca) THEN
                   --LET tmarca = FGL_DIALOG_GETBUFFER()
                   --DISPLAY "FGL_DIALOG_GETBUFFER MARCA>", tmarca
               --END IF
--
               --IF NOT FIELD_TOUCHED(f_inicio) THEN
                   --LET tf_inicio = FGL_DIALOG_GETBUFFER()
                   --DISPLAY "FGL_DIALOG_GETBUFFER INICIO>", tf_inicio
               --END IF
--
               --IF NOT FIELD_TOUCHED(f_fin) THEN
                   --LET tf_fin = FGL_DIALOG_GETBUFFER()
                   --DISPLAY "FGL_DIALOG_GETBUFFER FIN>", tf_fin
               --END IF

               LET trfc      =  GET_FLDBUF(rfc)
               LET tnss      =  GET_FLDBUF(nss)
               LET tf_inicio =  GET_FLDBUF(f_inicio)
               LET tf_fin    =  GET_FLDBUF(f_fin)
               
               IF trfc       IS NULL AND
                  tnss       IS NULL AND
                   tmarca    IS NULL AND
                   tf_inicio IS NULL AND
                   tf_fin    IS NULL THEN
                   IF cla_where IS NULL OR 
                       cla_where LIKE '1=1' THEN
                       CALL fn_mensaje("Consulta de Marcas", "Debe capturar un criterio de búsqueda", "about")
                       NEXT FIELD rfc
                   END IF
               END IF

               IF NOT trfc        AND
                  NOT tnss        AND
                   NOT tmarca     AND
                   NOT tf_inicio  AND
                   NOT tf_fin  THEN
                   CALL fn_mensaje("Consulta de Marcas", "Debe capturar un criterio de búsqueda", "about")
                   NEXT FIELD rfc
               ELSE
                   IF NOT ch1 AND
                       NOT ch2 THEN
                       CALL fn_mensaje("Consulta de Marcas", "Debe seleccionar un Tipo de Marca", "about")
                       NEXT FIELD rfc
                   END IF
               END IF

               IF trfc       IS NULL        AND
                  tnss       IS NULL        AND
                   tmarca     = 0           AND
                   tf_inicio = '12/31/1899' AND
                   tf_fin    = '12/31/1899' THEN
                   CALL fn_mensaje("Consulta de Marcas", "Debe capturar un criterio de búsqueda", "about")
                   NEXT FIELD rfc
               END IF

               IF trfc       IS NULL        AND
                  tnss       IS NULL        AND
                   tmarca     IS NULL       AND
                   tf_inicio = '12/31/1899' AND
                   tf_fin    = '12/31/1899' THEN
                   CALL fn_mensaje("Consulta de Marcas", "Debe capturar un criterio de búsqueda", "about")
                   NEXT FIELD rfc
               END IF

               IF trfc       IS NULL AND
                  tnss       IS NULL AND
                   tmarca    IS NULL AND
                   tf_inicio IS NULL AND
                   tf_fin    IS NULL AND
                   NOT ch1            AND
                   NOT ch2            THEN
                   CALL fn_mensaje("Consulta de Marcas", "Debe seleccionar una Marca", "about")
                   NEXT FIELD rfc
               END IF

               IF NOT ch1 AND
                   NOT ch2 THEN
                   CALL fn_mensaje("Consulta de Marcas", "Debe seleccionar una Marca", "about") 
                   NEXT FIELD rfc
               END IF

               IF ch1 AND
                   ch2 THEN
                   CALL f_w.setElementHidden("group3",0)
                   CALL f_w.setElementHidden("group2",0)
                   CALL extrae_datos(1)
                   CALL f_w.setElementHidden("group3",1)
                   CALL f_w.setElementHidden("group2",1)
               ELSE
                   IF ch1 THEN
                       CALL f_w.setElementHidden("group3",0)
                       CALL extrae_datos(2)
                       CALL f_w.setElementHidden("group3",1)
                   END IF

                   IF ch2 THEN
                       CALL f_w.setElementHidden("group2",0)
                       CALL extrae_datos(3)
                       CALL f_w.setElementHidden("group2",1)
                   END IF
               END IF

               LET f_inicio  = TODAY
               LET f_fin     = TODAY

               DISPLAY BY NAME f_inicio
               DISPLAY BY NAME f_fin

               LET trfc       = NULL
               LET tnss       = NULL
               LET tmarca     = NULL
               LET tf_inicio  = NULL
               LET tf_fin     = NULL
--******************************************************************************
           ELSE 
               CALL fn_mensaje("Atención",v_msj_alerta,"stop")
               LET INT_FLAG = TRUE
               EXIT DIALOG
           END IF
--******************************************************************************
       ON ACTION cancel
           LET f_inicio  = TODAY
           LET f_fin     = TODAY

           DISPLAY BY NAME f_inicio
           DISPLAY BY NAME f_fin

           LET trfc      = NULL
           LET tnss      = NULL 
           LET tmarca    = NULL
           LET tf_inicio = NULL
           LET tf_fin    = NULL

           NEXT FIELD rfc

       ON ACTION salir
           LET INT_FLAG = TRUE
           EXIT DIALOG

       END DIALOG
    
    END IF  #FIN no llego parametro

    CLOSE WINDOW ctac081

END FUNCTION

FUNCTION extrae_datos(tipo_cons)

  DEFINE tipo_cons           SMALLINT

    IF tipo_cons = 1 THEN
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

    IF tipo_cons = 2 THEN
        CALL consulta_activas()

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

    IF tipo_cons = 3 THEN
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

END FUNCTION

FUNCTION consulta_activas()
#función para buscar marcas activas

    LET sel_where = "SELECT '', c.rfc, c.nss, a.marca, b.descripcion_marca, ",
                          " a.f_inicio, a.h_inicio, ",
                          " a.n_referencia, a.usuario_marca ",
                     " FROM sfr_marca_activa a, sfr_marca b, afi_fondo72 c ",
                    " WHERE 1 = 1",
                    " AND a.marca IN (201,802)"
    IF trfc IS NOT NULL THEN 
        LET sel_where = sel_where CLIPPED, " AND c.rfc = '", trfc, "'"
    END IF 
    IF tnss IS NOT NULL THEN 
        LET sel_where = sel_where CLIPPED, " AND c.nss = '", tnss, "'"
    END IF 
    IF tmarca IS NOT NULL AND tmarca <> 0 THEN 
        LET sel_where = sel_where CLIPPED, " AND a.marca = ", tmarca
    END IF 
    IF tf_inicio IS NOT NULL AND tf_fin IS NOT NULL THEN 
        LET sel_where = sel_where CLIPPED, " AND a.f_inicio between '", tf_inicio, "' AND '", tf_fin, "'"
    END IF  
    LET sel_where = sel_where CLIPPED, " AND a.id_derechohabiente = c.id_derechohabiente ",
                                       " AND a.marca = b.marca ",
                                       " ORDER BY f_inicio DESC, marca, rfc "

    DISPLAY "Consulta> ", sel_where
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

FUNCTION historico()
#h------------------

    LET sel_hist = "SELECT '1', c.rfc, c.nss, a.marca, g.descripcion_marca, ",
                         " a.f_inicio, a.h_inicio, a.f_fin, ",
                         " e.estado_marca_desc, f.rch_desc, a.marca_causa, ",
                         " d.descripcion_marca, a.f_marca_causa, a.n_referencia, ",
                         " a.usuario_marca, a.usuario_desmarca ", 
                    " FROM sfr_marca_historica a, afi_fondo72 c, ",
                    " OUTER sfr_marca g, OUTER sfr_marca d, ",
                    " OUTER cat_estado_marca e, OUTER cat_rch_marca f ",
                   " WHERE 1 = 1 ",
                   " AND a.marca IN (201,802)"
    IF trfc IS NOT NULL THEN 
        LET sel_hist = sel_hist CLIPPED, " AND c.rfc = '", trfc, "'"
    END IF 
    IF tnss IS NOT NULL THEN 
        LET sel_hist = sel_hist CLIPPED, " AND c.nss = '", tnss, "'"
    END IF 
    IF tmarca IS NOT NULL AND tmarca <> 0 THEN 
        LET sel_hist = sel_hist CLIPPED, " AND a.marca = ", tmarca
    END IF 
    IF tf_inicio IS NOT NULL AND tf_fin IS NOT NULL THEN 
        LET sel_hist = sel_hist CLIPPED, " AND a.f_inicio between '", tf_inicio, "' AND '", tf_fin, "'"
    END IF  
    LET sel_hist = sel_hist CLIPPED, " AND a.id_derechohabiente = c.id_derechohabiente ",
                                     " AND a.marca = g.marca ",
                                     " AND a.marca_causa = d.marca ",
                                     " AND a.f_fin IS NOT NULL ",
                                     " AND a.estado_marca = e.estado_marca ",
                                     " AND a.rch_cod = f.rch_cod ",
                                     " ORDER BY a.f_inicio desc, a.marca, c.rfc, c.nss "
    DISPLAY "Consulta Historica>", sel_hist
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

FUNCTION fn_valida_rfc()

  LET  r_valida_rfc = 0

END FUNCTION
FUNCTION fn_valida_nss()

  LET  r_valida_nss = 0

END FUNCTION