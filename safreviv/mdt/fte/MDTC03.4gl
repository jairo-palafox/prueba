--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09-04-2012
--===============================================================

####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTC03                                        #
#Objetivo          =>Consulta de prelaciones registrados           #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>13 Febrero 2012                               #
####################################################################

DATABASE safre_viv

DEFINE v_s_qryTxt        STRING
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      VARCHAR(30)              -- nombre del programa
DEFINE cmbtpoprelac      LIKE mdt_cat_prelacion.tpo_prelacion
DEFINE v_ar_prelacion     DYNAMIC ARRAY OF RECORD -- arreglo que contiene las prelaciones
          prelacion        LIKE mdt_det_prelacion.prelacion,
          tpo_mandato      LIKE mdt_tpo_mandato.tpo_mandato,
          desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
          detalle          BOOLEAN            
       END RECORD
DEFINE v_ar_preAux RECORD
         prelacion        LIKE mdt_det_prelacion.prelacion,
         tpo_mandato      LIKE mdt_tpo_mandato.tpo_mandato,
         desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
         detalle          BOOLEAN            
       END RECORD
DEFINE cb ui.ComboBox
DEFINE v_ejecuta         STRING
DEFINE vstatus           SMALLINT,
       v_ventana         ui.Window

MAIN

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF
   
   -- se abre la ventana de consulta
   OPEN WINDOW w_con_prelacion WITH FORM "MDTC03"

   IF(p_v_nom_prog IS NOT NULL)THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF

   INPUT BY NAME cmbtpoprelac
     ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
      BEFORE INPUT
         IF fn_verifica_privilegios("MDTM06",p_v_usuario) THEN
            CALL DIALOG.setActionHidden( "modificar", FALSE)
         ELSE
            CALL DIALOG.setActionHidden( "modificar", TRUE)
         END IF
         
         -- Inicializa el combo de mandatos
         CALL inicializa_prelacion()
         -- Carga el arrerglo de prelaciones-detalle por clave seleccionada
         CALL fn_carga_arr_prel(cmbtpoprelac)
   
      ON CHANGE cmbtpoprelac
         LET cmbtpoprelac = DIALOG.getFieldBuffer("cmbtpoprelac")
      
      ON ACTION modificar
         IF cmbtpoprelac IS NULL OR cmbtpoprelac = 0 THEN
            CALL fn_mensaje("Advertencia","Es requerido que capture la prelación","error")
         ELSE
            LET v_ejecuta = "fglrun MDTM06 ", p_v_usuario CLIPPED," ",
                                              p_b_tipo_carga," ",
                                              "Modificar ", 
                                              cmbtpoprelac
            RUN v_ejecuta RETURNING vstatus  
         END IF

      ON ACTION aceptar
         -- Carga el arrerglo de prelaciones-detalle por clave seleccionada
         CALL fn_carga_arr_prel(cmbtpoprelac)
         -- Despliega arreglo de prelaciones-detalle
         
         --INPUT ARRAY v_ar_prelacion WITHOUT DEFAULTS FROM r_det_prelacion.*
         --                                   ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
         DISPLAY ARRAY v_ar_prelacion TO r_det_prelacion.*
            -- ON ACTION bndetprel
            AFTER DISPLAY
               -- consulta de detalle de mandatos
               IF v_ar_prelacion[ARR_CURR()].tpo_mandato > 0 THEN
                  LET v_ejecuta = "fglrun MDTC02 ", p_v_usuario CLIPPED," ",
                                                    p_b_tipo_carga," ",
                                                    "Consulta ", 
                                                    v_ar_prelacion[ARR_CURR()].tpo_mandato
                  RUN v_ejecuta RETURNING vstatus  
               END IF
               CONTINUE DISPLAY
   
            ON ACTION cancel
               CALL v_ar_prelacion.clear()
               DISPLAY ARRAY v_ar_prelacion TO r_det_prelacion.*
                  BEFORE DISPLAY 
                     EXIT DISPLAY
               END DISPLAY
               EXIT DISPLAY
               
         END DISPLAY
         
         CONTINUE INPUT
         
      ON ACTION close
         EXIT INPUT
      	
   END INPUT
   
   CLOSE WINDOW w_con_prelacion 

END MAIN

#############################################################################
# Funcion           => fn_carga_arr_prel - Carga detalle del prelaciones    #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 13 Febrero 2012                                      #
#############################################################################
FUNCTION fn_carga_arr_prel(p_tpoprelacion)
DEFINE p_mandato      LIKE mdt_cat_mandato.id_cat_mandato
DEFINE p_tpoprelacion LIKE mdt_cat_mandato.tpo_mandato
DEFINE v_i_count      INTEGER

   IF p_tpoprelacion IS NULL OR LENGTH(p_tpoprelacion CLIPPED) = 0 THEN
      RETURN
   END IF

   LET v_s_qryTxt = "SELECT prelacion, m.tpo_mandato, desc_tpo_mandato",
                    "  FROM mdt_det_prelacion m,",
                    "       mdt_tpo_mandato n",
                    " WHERE m.tpo_mandato = n.tpo_mandato",
                    "   AND m.tpo_prelacion = ", p_tpoprelacion,
                    " ORDER BY prelacion"

   PREPARE EnuDetPrelacion FROM v_s_qryTxt
   DECLARE CurDetPrelacion CURSOR FOR EnuDetPrelacion
   
   CALL v_ar_prelacion.clear()
   
   LET v_i_count = 1
   FOREACH CurDetPrelacion INTO v_ar_preAux.* -- INTO v_ar_prelacion[v_i_count].*
      CALL v_ar_prelacion.appendElement()
      LET v_ar_prelacion[v_ar_prelacion.getLength()].* = v_ar_preAux.*
      
      LET v_i_count = v_i_count + 1
   END FOREACH
   
END FUNCTION 

#############################################################################
# Funcion           => inicializa_prelaciones - Definicion e inicializacion #
#                      de los combos dinamicos del modulo de prelaciones    #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 13 Febrero 2012                                      #
#############################################################################
FUNCTION inicializa_prelacion()
 
   -- Inicializa combo de bancos en funcion a su tabla de base de datos
   CALL init_combo_tipoprelacion()

END FUNCTION

#############################################################################
# Funcion           => init_combo_tipoprelacion - Inicializa el combo tpopre#
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 13 Febrero 2012                                      #
#############################################################################
FUNCTION init_combo_tipoprelacion()

   DEFINE prelacion         LIKE mdt_cat_prelacion.tpo_prelacion
   DEFINE descripcion       LIKE mdt_cat_prelacion.desc_prelacion
   DEFINE desc_combo        CHAR(50)

   LET cb   = ui.combobox.forname("cmbtpoprelac")
   CALL cb.clear()
   
   DECLARE cur_tpoprelacion CURSOR FOR
    SELECT tb.tpo_prelacion, tb.desc_prelacion
      FROM mdt_cat_prelacion tb
     ORDER  BY 1
   
   FOREACH cur_tpoprelacion INTO prelacion, descripcion
       LET desc_combo= prelacion       USING "&&&", " ",
                       descripcion CLIPPED
       CALL cb.additem( prelacion, desc_combo )
   END FOREACH

END FUNCTION