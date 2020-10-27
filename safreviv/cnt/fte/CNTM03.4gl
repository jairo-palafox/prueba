#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CNT                                                     #
#Programa          => CNTM03                                                  #
#Objetivo          => PROGRAMA DE MANTENIMIENTO PARA EL CATALOGO DE CUENTAS   #
#                     CONTABLES                                               #
#Fecha Inicio      => 12-SEPTIEMBRE-2012                                      #
###############################################################################
DATABASE safre_viv

GLOBALS "CNTM03.inc"

PRIVATE DEFINE p_usuario_cod              CHAR(20)
PRIVATE DEFINE p_tipo_proc                SMALLINT
PRIVATE DEFINE p_titulo                   STRING

#Lista para las cuentas contables
PRIVATE DEFINE v_lista_cuenta DYNAMIC ARRAY OF cat_cuenta

#Registro a manipular
PRIVATE DEFINE item_catalogo              cat_cuenta
PRIVATE DEFINE v_tpo_operacion             SMALLINT

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana                       ui.Window
PRIVATE DEFINE forma                         ui.Form

MAIN
   DEFINE v_ciclo          SMALLINT

   LET v_ciclo = 1
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tipo_proc     = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_titulo        = ARG_VAL(3) -- Recibe el nombre del programa
   
   CALL STARTLOG(p_usuario_cod CLIPPED ||".CNTM03.log")

   CLOSE WINDOW SCREEN

   -- se asigna el titulo del programa
   LET p_titulo = "Cuentas Contables"
   CALL ui.Interface.setText(p_titulo)

   OPEN WINDOW vtn_cntm031 WITH FORM "CNTM031"
      LET ventana = ui.Window.forName("vtn_cntm031")
      LET forma = ventana.getForm()
      WHILE v_ciclo = 1
         CALL fn_consulta_cuenta_contable() RETURNING v_ciclo 
      END WHILE
   CLOSE WINDOW vtn_cntm031
END MAIN

PRIVATE FUNCTION fn_consulta_cuenta_contable()
   DEFINE i                      INTEGER
   DEFINE v_consulta_cuenta      STRING

   LET v_consulta_cuenta = "SELECT ",
                              "cta_contable, ",
                              "desc_cta_contable, ",
                              "f_actualiza, ",
                              "usuario ",
                           "FROM cat_cuenta_contable ",
                           "ORDER BY f_actualiza DESC"
   PREPARE exe_consulta_cuenta FROM v_consulta_cuenta
   DECLARE cur_consulta_cuenta CURSOR FOR exe_consulta_cuenta

   LET i = 1
   FOREACH cur_consulta_cuenta INTO v_lista_cuenta[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_cuenta.deleteElement(v_lista_cuenta.getLength())
   CLOSE cur_consulta_cuenta
   FREE cur_consulta_cuenta

   DISPLAY ARRAY v_lista_cuenta TO lista_cuenta.*
      BEFORE DISPLAY
         CALL DIALOG.setactionhidden("close",1)
         
      ON ACTION ACCEPT 
         LET item_catalogo.cta_contable = v_lista_cuenta[ARR_CURR()].cta_contable
         LET item_catalogo.desc_cta_contable = v_lista_cuenta[ARR_CURR()].desc_cta_contable
         LET v_tpo_operacion = ACTUALIZA
         CALL fn_modifica_catalogo()
         RETURN 1
         
      ON ACTION insertar
         INITIALIZE item_catalogo       TO NULL
         LET v_tpo_operacion = INSERTA
         CALL fn_modifica_catalogo()
         RETURN 1
      
      ON ACTION CANCEL
         INITIALIZE item_catalogo       TO NULL
         EXIT DISPLAY
         RETURN 0
   END DISPLAY
      
END FUNCTION

PRIVATE FUNCTION fn_modifica_catalogo()
   DEFINE vnt                       ui.Window
   DEFINE frm                       ui.Form
   DEFINE v_valida_tamaño           STRING
   OPEN WINDOW vtn_cntm032 WITH FORM "CNTM032" ATTRIBUTES (STYLE="dialog")
      LET vnt = ui.Window.forName("vtn_cntm032")
      LET frm = ventana.getForm()
      CALL vnt.setText("Cuenta Contable")
      INPUT item_catalogo.cta_contable, item_catalogo.desc_cta_contable 
      FROM  cta_contable, desc_cta_contable ATTRIBUTES (WITHOUT DEFAULTS)
         BEFORE INPUT
            DISPLAY item_catalogo.cta_contable        TO cta_contable
            DISPLAY item_catalogo.desc_cta_contable   TO desc_cta_contable
            CALL DIALOG.setFieldActive( "cta_contable", (v_tpo_operacion == INSERTA) )
            
         ON ACTION ACCEPT
            LET item_catalogo.cta_contable    = GET_FLDBUF(cta_contable)
            LET item_catalogo.desc_cta_contable    = GET_FLDBUF(desc_cta_contable)
            LET v_valida_tamaño = item_catalogo.cta_contable CLIPPED
            IF v_valida_tamaño.getLength() <> 10 THEN
               CALL fn_mensaje("Cuenta Contable", "La clave de la Cuenta Contable debe ser de 10 caracteres", "about")
               NEXT FIELD cta_contable
            END IF
            CALL fn_aplica_cambio()
            EXIT INPUT

         ON ACTION CANCEL
            CALL fn_mensaje("Cuenta Contable", "Se cancelo la operación...", "about")
            EXIT INPUT
         
      END INPUT
   CLOSE WINDOW vtn_cntm032
END FUNCTION

PRIVATE FUNCTION fn_aplica_cambio()
   DEFINE v_inserta           STRING
   DEFINE v_valida_cuenta     STRING 
   DEFINE v_actualiza         STRING

   DEFINE v_fecha             DATE
   DEFINE v_cuenta            CHAR(10)

   LET v_fecha = TODAY
   IF v_tpo_operacion = INSERTA THEN
      #Primero se valida que la clave de la cuenta no este dada de alta
      LET v_valida_cuenta = "SELECT cta_contable FROM cat_cuenta_contable WHERE cta_contable = ?"
      PREPARE exe_valida_cuenta FROM v_valida_cuenta
      EXECUTE exe_valida_cuenta USING item_catalogo.cta_contable INTO v_cuenta
      IF v_cuenta IS NOT NULL THEN
         CALL fn_mensaje("Cuenta Contable", "La cuenta contable capturada ya existe en el sistema \n", "about")
      ELSE
         LET v_inserta = "INSERT INTO cat_cuenta_contable VALUES(?,?,?,?)"
         PREPARE exe_inserta FROM v_inserta
         EXECUTE exe_inserta USING item_catalogo.cta_contable,
                                    item_catalogo.desc_cta_contable,
                                    v_fecha,
                                    p_usuario_cod
      END IF
   END IF
   
   IF v_tpo_operacion = ACTUALIZA THEN
      LET v_actualiza = "UPDATE cat_cuenta_contable SET ",
                           "desc_cta_contable = ? , ",
                           "f_actualiza = ? , ",
                           "usuario = ? ",
                        "WHERE cta_contable = ?"
      PREPARE exe_actualiza FROM v_actualiza
      EXECUTE exe_actualiza USING   item_catalogo.desc_cta_contable,
                                    v_fecha,
                                    p_usuario_cod,
                                    item_catalogo.cta_contable
   END IF
END FUNCTION