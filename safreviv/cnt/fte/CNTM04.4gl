#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CNT                                                     #
#Programa          => CNTM04                                                  #
#Objetivo          => PROGRAMA DE MANTENIMIENTO PARA EL CATALOGO DE PROCESOS  #
#                     CONTABLES                                               #
#Fecha Inicio      => 13-SEPTIEMBRE-2012                                      #
###############################################################################
DATABASE safre_viv

GLOBALS "CNTM04.inc"

PRIVATE DEFINE p_usuario_cod              CHAR(20)
PRIVATE DEFINE p_tipo_proc                SMALLINT
PRIVATE DEFINE p_titulo                   STRING

#Lista para las cuentas contables
PRIVATE DEFINE v_lista_proceso            DYNAMIC ARRAY OF cat_proceso

#Registro a manipular
PRIVATE DEFINE item_catalogo              cat_proceso
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
   
   CALL STARTLOG(p_usuario_cod CLIPPED ||".CNTM04.log")

   CLOSE WINDOW SCREEN

   -- se asigna el titulo del programa
   LET p_titulo = "Procesos Contables"
   CALL ui.Interface.setText(p_titulo)

   OPEN WINDOW vtn_cntm041 WITH FORM "CNTM041"
      LET ventana = ui.Window.forName("vtn_cntm041")
      LET forma = ventana.getForm()
      WHILE v_ciclo = 1
         CALL fn_consulta_proceso_contable() RETURNING v_ciclo 
      END WHILE
   CLOSE WINDOW vtn_cntm041
END MAIN

PRIVATE FUNCTION fn_consulta_proceso_contable()
   DEFINE i                      INTEGER
   DEFINE v_consulta_proceso     STRING

   LET v_consulta_proceso =   "SELECT ",
                              "proc.cod_proceso_cnt, ",
                              "proc.desc_proceso_cnt, ",
                              "proc.referencia_cnt, ",
                              "proc.desc_proc_corta_cnt, ",
                              "nat.desc_naturaleza_mov, ",
                              "proc.cod_naturaleza_mov, ",
                              "sub_cta.subcuenta_desc, ",
                              "proc.cod_periodo ",
                              "FROM cat_proceso_cnt proc ",
                              "INNER JOIN cat_subcuenta sub_cta ON sub_cta.subcuenta = proc.cod_periodo ",
                              "INNER JOIN cat_naturaleza_mov nat ON nat.cod_naturaleza_mov = proc.cod_naturaleza_mov ",
                              "ORDER BY proc.cod_proceso_cnt ASC"
   PREPARE exe_consulta_proceso FROM v_consulta_proceso
   DECLARE cur_consulta_proceso CURSOR FOR exe_consulta_proceso

   LET i = 1
   FOREACH cur_consulta_proceso INTO v_lista_proceso[i].*
      LET i = i + 1
   END FOREACH
   CALL v_lista_proceso.deleteElement(v_lista_proceso.getLength())
   CLOSE cur_consulta_proceso
   FREE cur_consulta_proceso

   DISPLAY ARRAY v_lista_proceso TO lista_proceso.*
      BEFORE DISPLAY
         CALL DIALOG.setactionhidden("close",1)
         
      ON ACTION ACCEPT 
         LET item_catalogo.cod_proceso_cnt = v_lista_proceso[ARR_CURR()].cod_proceso_cnt
         LET item_catalogo.desc_proceso_cnt = v_lista_proceso[ARR_CURR()].desc_proceso_cnt
         LET item_catalogo.referencia_cnt = v_lista_proceso[ARR_CURR()].referencia_cnt
         LET item_catalogo.desc_proc_corta_cnt = v_lista_proceso[ARR_CURR()].desc_proc_corta_cnt
         LET item_catalogo.cod_naturaleza_mov = v_lista_proceso[ARR_CURR()].cod_naturaleza_mov
         LET item_catalogo.cod_periodo = v_lista_proceso[ARR_CURR()].cod_periodo
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
   
   DEFINE v_naturaleza           STRING
   DEFINE v_cat_naturaleza       catalogo
   DEFINE cb_naturaleza          ui.combobox

   DEFINE v_periodo              STRING
   DEFINE v_cat_periodo          catalogo
   DEFINE cb_periodo             ui.combobox

   OPEN WINDOW vtn_cntm042 WITH FORM "CNTM042" ATTRIBUTES (STYLE="dialog")
      LET vnt = ui.Window.forName("vtn_cntm042")
      LET frm = ventana.getForm()
      CALL vnt.setText("Proceso Contable")

      #Se llena el combo de naturaleza
      LET cb_naturaleza = ui.combobox.forname("cod_naturaleza_mov")
      CALL cb_naturaleza.clear()
      LET v_naturaleza =   "SELECT ",
                              "cod_naturaleza_mov, ",
                              "desc_naturaleza_mov ",
                           "FROM cat_naturaleza_mov ",
                           "ORDER BY cod_naturaleza_mov"
      PREPARE exe_naturaleza FROM v_naturaleza
      DECLARE cur_naturaleza CURSOR FOR exe_naturaleza
      FOREACH cur_naturaleza INTO v_cat_naturaleza.*
         IF v_cat_naturaleza.clave IS NOT NULL THEN
            CALL cb_naturaleza.additem(v_cat_naturaleza.clave, v_cat_naturaleza.descripcion)
         END IF
      END FOREACH
      CLOSE cur_naturaleza
      FREE cur_naturaleza

      #Se llena el combo de periodo
      LET cb_periodo = ui.combobox.forname("cod_periodo")
      CALL cb_periodo.clear()
      LET v_periodo =   "SELECT ",
                        "subcuenta, ",
                        "subcuenta_desc  ",
                        "FROM cat_subcuenta ",
                        "ORDER BY subcuenta"
      PREPARE exe_periodo FROM v_periodo
      DECLARE cur_periodo CURSOR FOR exe_periodo
      FOREACH cur_periodo INTO v_cat_periodo.*
         IF v_cat_periodo.clave IS NOT NULL THEN
            CALL cb_periodo.additem(v_cat_periodo.clave, v_cat_periodo.descripcion)
         END IF
      END FOREACH
      CLOSE cur_periodo
      FREE cur_periodo
      
      INPUT    item_catalogo.cod_proceso_cnt,
               item_catalogo.desc_proceso_cnt,
               item_catalogo.desc_proc_corta_cnt,
               item_catalogo.referencia_cnt,
               item_catalogo.cod_naturaleza_mov,
               item_catalogo.cod_periodo
      FROM     cod_proceso_cnt,
               desc_proceso_cnt,
               desc_proc_corta_cnt,
               referencia_cnt,
               cod_naturaleza_mov,
               cod_periodo          ATTRIBUTES (WITHOUT DEFAULTS)
         BEFORE INPUT
            DISPLAY item_catalogo.cod_proceso_cnt        TO cod_proceso_cnt
            DISPLAY item_catalogo.desc_proceso_cnt       TO desc_proceso_cnt
            DISPLAY item_catalogo.desc_proc_corta_cnt    TO desc_proc_corta_cnt
            DISPLAY item_catalogo.referencia_cnt         TO referencia_cnt
            DISPLAY item_catalogo.cod_naturaleza_mov     TO cod_naturaleza_mov
            DISPLAY item_catalogo.cod_periodo            TO cod_periodo
            CALL DIALOG.setFieldActive( "cod_proceso_cnt", FALSE)
            CALL DIALOG.setFieldActive( "referencia_cnt", v_tpo_operacion == INSERTA)
            
         ON ACTION ACCEPT
            LET item_catalogo.cod_proceso_cnt        = GET_FLDBUF(cod_proceso_cnt)
            LET item_catalogo.desc_proceso_cnt       = GET_FLDBUF(desc_proceso_cnt)
            LET item_catalogo.desc_proc_corta_cnt    = GET_FLDBUF(desc_proc_corta_cnt)
            LET item_catalogo.referencia_cnt         = GET_FLDBUF(referencia_cnt)
            LET item_catalogo.cod_naturaleza_mov     = GET_FLDBUF(cod_naturaleza_mov)
            LET item_catalogo.cod_periodo            = GET_FLDBUF(cod_periodo)

            CALL fn_aplica_cambio()
            EXIT INPUT

         ON ACTION CANCEL
            CALL fn_mensaje("Proceso Contable", "Se cancelo la operación...", "about")
            EXIT INPUT
         
      END INPUT
   CLOSE WINDOW vtn_cntm042
END FUNCTION

PRIVATE FUNCTION fn_aplica_cambio()
   DEFINE v_actualiza         STRING

   DEFINE v_fecha             DATE

   LET v_fecha = TODAY
   IF v_tpo_operacion = INSERTA THEN
      INSERT INTO cat_proceso_cnt (
                  cod_proceso_cnt,
                  desc_proceso_cnt,
                  referencia_cnt,
                  desc_proc_corta_cnt,
                  cod_naturaleza_mov,
                  cod_periodo,
                  f_actualiza,
                  usuario) 
         VALUES  (seq_cat_proceso_cnt.nextval,
                  item_catalogo.desc_proceso_cnt,
                  item_catalogo.referencia_cnt,
                  item_catalogo.desc_proc_corta_cnt,
                  item_catalogo.cod_naturaleza_mov,
                  item_catalogo.cod_periodo,
                  v_fecha,
                  p_usuario_cod);

   END IF
   
   IF v_tpo_operacion = ACTUALIZA THEN
      LET v_actualiza = "UPDATE cat_proceso_cnt SET ",
                        "desc_proceso_cnt = ?, ",
                        "referencia_cnt = ?, ",
                        "desc_proc_corta_cnt = ?, ",
                        "cod_naturaleza_mov = ?, ",
                        "cod_periodo = ?, ",
                        "f_actualiza = ?, ",
                        "usuario = ? ",
                     "WHERE cod_proceso_cnt = ? "
      
      PREPARE exe_actualiza FROM v_actualiza
      EXECUTE exe_actualiza USING   item_catalogo.desc_proceso_cnt,
                                    item_catalogo.referencia_cnt,
                                    item_catalogo.desc_proc_corta_cnt,
                                    item_catalogo.cod_naturaleza_mov,
                                    item_catalogo.cod_periodo,
                                    v_fecha,
                                    p_usuario_cod,
                                    item_catalogo.cod_proceso_cnt
   END IF
END FUNCTION