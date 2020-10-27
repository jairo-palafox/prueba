######################################################################
#Proyecto          => INFONAVIT (MEXICO)                             #
#Propietario       => E.F.P.                                         #
#Programa AFIC02   => PROGRAMA DE CONSULTA DE DERECHOHABIENTES       #
#Sistema           => AfI                                            #
#Autor             => MAURO MUÑIZ CABALLERO                          #
#Modifico          => Mauricio Sanchez
#Fecha             => 27 DE MARZO DE 2012                            #
######################################################################

DATABASE safre_viv

GLOBALS
    DEFINE pos            SMALLINT

    DEFINE g_enter        CHAR(1)
    DEFINE g_usuario      CHAR(12)

    DEFINE g_hoy          DATE
    DEFINE g_v_nom_prog   VARCHAR(30) -- nombre del programa

    DEFINE reg_derechohabiente RECORD
        nss                CHAR(11),
        id_derechohabiente CHAR(9),
        rfc                CHAR(13),
        curp               CHAR(18),
        f_nacimiento       DATE,
        nombre_imss        CHAR(50),
        ap_paterno_af      CHAR(40),
        ap_materno_af      CHAR(40),
        nombre_af          CHAR(40),
        tipo_trabajador    CHAR(1),
        desc_tipo_trab     CHAR(20),
        origen_afiliacion  CHAR(1),
        desc_origen        CHAR(20),
        id_credito         SMALLINT,
        desc_credito       CHAR(30)
    END RECORD

    DEFINE g_reg_modulo RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
    END RECORD

    DEFINE g_mensaje      STRING
    DEFINE g_titulo       STRING
    DEFINE g_imagen       STRING
    DEFINE sel_where      STRING
    DEFINE cla_where      STRING
    DEFINE comma          STRING

    DEFINE w_pp           ui.Window
    DEFINE f_pp           ui.Form

END GLOBALS

MAIN
   DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
          p_s_titulo       STRING -- titulo de la ventana   
   CLOSE WINDOW SCREEN

     -- se asignan los parametros que vienen del fglrun
     LET g_usuario        = ARG_VAL(1)
     LET p_tipo_ejecucion = ARG_VAL(2)
     LET p_s_titulo       = ARG_VAL(3)
    --CALL ui.Interface.setType("child")
    --CALL ui.interface.setContainer("mdi")
      -- si se obtuvo el titulo, se pone como titulo de programa
     IF ( p_s_titulo IS NOT NULL ) THEN
        CALL ui.Interface.setText(p_s_titulo)
     END IF
     
    CALL fn_inicio()

    CALL STARTLOG(g_usuario CLIPPED||'.AFIC02.log')

    CALL fn_proceso_principal()

END MAIN

FUNCTION fn_inicio()
#función de inicialización de variables

    LET g_hoy    = TODAY
    LET g_titulo = "Información"
    LET g_imagen = "information"

    SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados, USER
      INTO g_reg_modulo.*, g_usuario
      FROM seg_modulo s
     WHERE modulo_cod = 'afi'

END FUNCTION

FUNCTION fn_inicializa()
#inicializa variables

   INITIALIZE reg_derechohabiente.* TO NULL

   CALL f_pp.setElementHidden("gb9",1)

END FUNCTION

FUNCTION fn_proceso_principal()
#función principal del programa

   DEFINE v_flg       SMALLINT
   
   -- se asigna el titulo del programa
   IF ( g_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_v_nom_prog)
   END IF
   
    OPEN WINDOW w2 WITH FORM ("AFIC022")

    LET w_pp = ui.window.getcurrent()
    LET w_pp = ui.window.forName("w2")

    CALL w_pp.settext("INFORMACIÓN DE DERECHOHABIENTES")

    LET f_pp = w_pp.getform()

    CALL f_pp.setElementHidden("gb9",1)

    --MENU "DERECHOHABIENTES"
     --   COMMAND "Consulta" "Consulta de Derechohabientes"
    CALL fn_preconsulta() RETURNING v_flg

    IF v_flg THEN
       CALL fn_consulta_afi()
    END IF

    CALL fn_inicializa()

     --   COMMAND "Salir" "Salir de Programa"
     --       EXIT MENU
    --END MENU

    CLOSE WINDOW w2

END FUNCTION

FUNCTION fn_preconsulta()
#funcion para elegir la información del derechohabiente a desplegar

    DEFINE cont         INTEGER
    DEFINE x_flg        SMALLINT

    DEFINE lc_condicion STRING
    DEFINE lc_qry       STRING

    DEFINE arr_busqueda DYNAMIC ARRAY OF RECORD
        nss             CHAR(11),
        rfc             CHAR(13),
        curp            CHAR(18),
        ap_paterno_af   CHAR(40),
        ap_materno_af   CHAR(40),
        nombres_af      CHAR(40)
    END RECORD

    LET x_flg = 0

    DIALOG ATTRIBUTES(UNBUFFERED)

    CONSTRUCT lc_condicion ON a.nss, a.rfc, a.curp, a.ap_paterno_af, a.ap_materno_af, a.nombre_af
                         FROM nss, rfc, curp, ap_paterno_af, ap_materno_af, nombre_af

        BEFORE CONSTRUCT
            CALL arr_busqueda.clear()

            CALL dialog.setActionHidden("close",1)

        ON ACTION ACCEPT
            LET lc_qry = " SELECT a.nss      ,",
                                " a.rfc      ,",
                                " a.curp     ,",
                                " a.ap_paterno_af  ,",
                                " a.ap_materno_af  ,",
                                " a.nombre_af    ",
                         " FROM afi_derechohabiente a ",
                         " WHERE ",lc_condicion CLIPPED

            PREPARE prp_pre_busqueda FROM lc_qry
            DECLARE cur_pre_busqueda CURSOR FOR prp_pre_busqueda

            LET cont= 1

            FOREACH cur_pre_busqueda INTO arr_busqueda[cont].*
                LET cont = 1 + cont

                IF cont > 32767 THEN
                    CALL fn_mensaje("Aviso","SE SOBREPASÓ LA CAPACIDAD MÁXIMA DEL ARREGLO","exclamation")
                    LET INT_FLAG = TRUE
                    EXIT DIALOG
                END IF
            END FOREACH

            IF cont = 1 THEN
                CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
                LET INT_FLAG = TRUE
                EXIT DIALOG
            END IF

            LET INT_FLAG = FALSE
            EXIT DIALOG

        ON ACTION cancel
            LET INT_FLAG = TRUE
            EXIT DIALOG

    END CONSTRUCT

    END DIALOG

    CALL f_pp.setElementHidden("gb9",0)
    CALL set_count(cont-1)

    IF (cont-1) >= 1 THEN
        DISPLAY ARRAY arr_busqueda TO tb2.*

        ON ACTION accept
            LET reg_derechohabiente.nss = arr_busqueda[ARR_CURR()].nss
            LET INT_FLAG = FALSE
            EXIT DISPLAY

        ON ACTION cancel
            LET INT_FLAG = TRUE
            EXIT DISPLAY

        END DISPLAY

        IF NOT INT_FLAG THEN
            LET x_flg = 1
            LET INT_FLAG = FALSE
        ELSE
            LET x_flg = 0
        END IF
    END IF

    CLEAR FORM

    RETURN x_flg

END FUNCTION

FUNCTION fn_consulta_afi()
#función que despliega la información del asignado elegido
  
    OPEN WINDOW w1 WITH FORM ("AFIC021")

    LET w_pp = ui.window.getcurrent()
    LET w_pp = ui.window.forName("w2")
    LET INT_FLAG = TRUE 

    CALL w_pp.settext("INFORMACIÓN DE DERECHOHABIENTES")

    LET f_pp = w_pp.getform()

    LET sel_where = " SELECT a.nss               ,",
                           " a.id_derechohabiente,",
                           " a.rfc               ,",
                           " a.curp              ,",
                           " a.f_nacimiento      ,",
                           " a.nombre_imss       ,",
                           " a.ap_paterno_af     ,",
                           " a.ap_materno_af     ,",
                           " a.nombre_af         ,",
                           " a.tipo_trabajador   ,",
                           " DECODE (a.tipo_trabajador,'I','IMSS','S','SOLO INFONAVIT') desc_tipo_trab ,",
                           " a.origen_afiliacion ,",
                           " DECODE (a.origen_afiliacion,'A','AFILIACIÓN','R','RECAUDACIÓN','S','SEPARACIÓN','U','UNIFICACIÓN') desc_origen ,",
                           " a.id_credito        ,",
                           " DECODE (a.id_credito,0,'SIN CRÉDITO',1,'ACREDITADO',2,'CRÉDITO EN GARANTÍA 43 BIS',3,'ANUALIDAD GARANTIZADA') desc_credito ",
                    " FROM afi_derechohabiente a  ",
                    " WHERE a.nss = '",reg_derechohabiente.nss,"'"

DISPLAY sel_where

    PREPARE qry_asig FROM sel_where

    EXECUTE qry_asig INTO reg_derechohabiente.*
    
    DISPLAY BY NAME reg_derechohabiente.*

    MENU "Consulta"
        BEFORE MENU
            CALL ui.Interface.refresh()

        ON ACTION salir
            CALL ui.Interface.refresh()
            EXIT MENU

        ON ACTION saldo
            CALL fn_eje_consulta(1)

    END MENU

    CLOSE WINDOW w1

END FUNCTION

FUNCTION fn_eje_consulta(p_pgm)
#función para desplegar pantalla de saldos del asignado

    DEFINE p_pgm           SMALLINT

    DEFINE v_pgm           CHAR(6)
    DEFINE l_ruta_bin      CHAR(40)

    INITIALIZE comma TO NULL

    SELECT ct.ruta_bin
      INTO l_ruta_bin
      FROM seg_modulo ct
     WHERE modulo_cod = 'cta'

    IF p_pgm = 1 THEN
        LET v_pgm = 'CTAC01'
    END IF

    LET comma = "cd ",l_ruta_bin CLIPPED,"/; fglrun ", v_pgm," ",reg_derechohabiente.nss

    CALL ui.interface.refresh()

    LET comma = comma CLIPPED
    RUN comma

END FUNCTION
