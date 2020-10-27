####################################################################
#Modulo            =>DSE                                           #
#Programa          =>DSEP01                                        #
#Objetivo          =>Programa de restituci�n a cuentas individuales#
#                    de los M�dulos ACR,AGR,GRT                    #
#Autor             =>Jos� Eduardo Ventura                          #
#Fecha inicio      =>04 Noviembre 2015                             #
####################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario                     CHAR(20)
   DEFINE p_s_titulo                    CHAR(20)
   DEFINE bnd_dato                      SMALLINT
   DEFINE a                             SMALLINT
   DEFINE v_nss                         CHAR(11)
   DEFINE v_dato                        CHAR(1)
   DEFINE w                             ui.Window
   DEFINE f                             ui.Form
   DEFINE p_tipo_ejecucion              SMALLINT
   DEFINE v_num_credito                 DECIMAL (10,0)
   DEFINE v_cta                         INTEGER
   DEFINE v_cta_marca                   INTEGER
   DEFINE v_valor_fondo                 DECIMAL(19,6)
   DEFINE cb                            ui.ComboBox

END GLOBALS

MAIN

   DEFINE v_nombre                      CHAR(40)
   DEFINE v_ap_paterno                  CHAR(40)
   DEFINE v_ap_materno                  CHAR(40)
   DEFINE v_nombre_completo             STRING
   DEFINE v_aivs92                      DECIMAL(19,2)
   DEFINE v_aivs97                      DECIMAL(19,2)
   DEFINE v_tpo_transferencia           CHAR(2)
   DEFINE v_s_qryTxt                    STRING
   DEFINE v_folio                       INTEGER 
   DEFINE arr_modulo                    DYNAMIC ARRAY OF RECORD LIKE seg_modulo.*
   DEFINE v_combo                       CHAR(3)
   DEFINE v_pesos97                     DECIMAL(12,2)
   DEFINE v_pesos92                     DECIMAL(12,2)
   DEFINE bnd_importe                   SMALLINT
   DEFINE v_minimo                      DECIMAL(19,2)
   DEFINE v_cmd                         STRING

   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

     -- se asigna el titulo de la ventana
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".ACRP40.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW DSEP01 WITH FORM "ACRP40"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   CALL f.setFieldHidden  ("v_combo",1)
   CALL f.setFieldHidden  ("v_nombre_completo",1)
   CALL f.setElementHidden("lb_pesos97",1)
   CALL f.setFieldHidden  ("v_pesos97",1)
   CALL f.setElementHidden("lb_modulo",1)
   CALL f.setElementHidden("lb_valor_fondo",1)
   CALL f.setFieldHidden  ("v_valor_fondo",1)
   CALL f.setElementHidden("lb_aivs97",1)
   CALL f.setFieldHidden  ("v_aivs97",1)
   CALL f.setElementHidden("lb_nombre",1)
   CALL f.setElementHidden("lb_pesos92",1)
   CALL f.setFieldHidden  ("v_pesos92",1)
   CALL f.setElementHidden("lb_aivs92",1)
   CALL f.setFieldHidden  ("v_aivs92",1)
   CALL f.setElementHidden("lb_minimo",1)
   CALL f.setFieldHidden  ("v_minimo",1)

   DIALOG ATTRIBUTES (UNBUFFERED)

      INPUT BY NAME v_nss , v_num_credito --ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      ON ACTION ACCEPT

         IF v_nss IS NULL THEN
            CALL fn_mensaje("Aviso","Debe ingresar un NSS para continuar","stop")
         END IF

         IF (LENGTH(v_nss) <> 11) AND
            (v_nss IS NOT NULL ) THEN
            CALL fn_mensaje("Aviso","Cantidad de d�gitos de NSS no es v�lido","stop")
         END IF

         IF (LENGTH(v_nss) = 11) THEN
            LET a=1
            FOR a=1 TO LENGTH(v_nss)
               LET v_dato = v_nss[a,a]
               CALL fn_valida_numero(v_dato)
               IF bnd_dato= 0 THEN
                  CALL fn_mensaje("Aviso","NSS ingresado es incorrecto,\n debe ingresar un NSS v�lido","stop")
                  LET v_nss = ""
                  EXIT FOR
               END IF
            END FOR
         END IF

         LET v_cta = 0

         SELECT COUNT(*)
           INTO v_cta
           FROM afi_derechohabiente
          WHERE nss = v_nss

         IF (LENGTH(v_nss) = 11) AND
            (bnd_dato = 1) AND
            (v_cta <= 0) THEN
            CALL fn_mensaje("Aviso","No se encontraron registros con NSS ingresado","stop")
            LET v_nss = ""
            LET v_num_credito = ""
            NEXT FIELD v_nss
         END IF

         IF (bnd_dato = 1) AND
            (v_cta >= 1)   AND
            (v_num_credito IS NULL) THEN
            CALL fn_mensaje("Aviso","Debe ingresar un n�mero de cr�dito","stop")
            NEXT FIELD v_num_credito
         END IF

         SELECT COUNT(*)
           INTO v_cta_marca
           FROM afi_derechohabiente afi,sfr_marca_activa m
          WHERE afi.id_derechohabiente = m.id_derechohabiente
            AND marca in (150,160)
            AND nss = v_nss

         IF (bnd_dato = 1) AND
            (v_cta > 0)   AND
            (v_num_credito IS NOT NULL) AND 
            (v_cta_marca > 0) THEN
            CALL fn_mensaje("Aviso","No es posible desplegar informaci�n para NSS ingresado","stop")
            EXIT DIALOG
         END IF

         IF ( bnd_dato = 1) AND
            (v_num_credito IS NOT NULL) AND 
            (v_cta > 0 )                AND 
            (v_cta_marca = 0) THEN

            SELECT precio_fondo
              INTO v_valor_fondo
              FROM glo_valor_fondo
             WHERE f_valuacion = TODAY
               AND fondo = 11

            IF (v_valor_fondo IS NULL) OR
               (v_valor_fondo < 0) THEN
               CALL fn_mensaje("Aviso","No ex�ste precio de aiv para el d�a de hoy","stop")
            ELSE

               SELECT ap_paterno_af,
                      ap_materno_af,
                      nombre_af
                 INTO v_ap_paterno,
                      v_ap_materno,
                      v_nombre
                 FROM afi_derechohabiente
                WHERE nss = v_nss

               LET v_nombre_completo = v_ap_paterno CLIPPED," ",v_ap_materno CLIPPED," ",v_nombre CLIPPED
               --DISPLAY BY NAME v_nombre_completo

               LET cb = ui.ComboBox.forName("v_combo")
            
               DECLARE cur_modulo CURSOR FOR SELECT modulo_cod,modulo_desc 
                                               FROM seg_modulo
                                              WHERE modulo_cod IN ("acr","agr","grt")
               LET a= 1
               FOREACH cur_modulo INTO arr_modulo[a].*
                  CALL cb.addItem(arr_modulo[a].modulo_cod,arr_modulo[a].modulo_cod)
                  LET a = a+1
               END FOREACH

               CALL arr_modulo.deleteElement(a)
               LET v_combo = arr_modulo[1].modulo_cod

               CALL DIALOG.setFieldActive("v_nss", FALSE)
               CALL DIALOG.setFieldActive("v_num_credito", FALSE)
               CALL f.setElementHidden      ("lb_nombre",0)
               CALL f.setFieldHidden ("v_nombre_completo",0)

               DISPLAY BY NAME v_nombre_completo

               CALL f.setElementHidden("lb_modulo",0)
               CALL f.setFieldHidden("v_combo",0)
               CALL f.setElementHidden("lb_pesos97",0)
               CALL f.setFieldHidden  ("v_pesos97",0)
               CALL f.setElementHidden("lb_pesos92",0)
               CALL f.setFieldHidden  ("v_pesos92",0)
               CALL f.setElementHidden("lb_minimo",0)
               CALL f.setFieldHidden  ("v_minimo",0)

               LET v_minimo = v_valor_fondo
               DISPLAY BY NAME v_minimo

               NEXT FIELD v_pesos97
            END IF
         END IF

   ON ACTION CLOSE
      EXIT DIALOG
   END INPUT


    INPUT BY NAME v_combo , v_pesos97 ,v_pesos92 --ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

   BEFORE INPUT
      CALL DIALOG.setActionHidden( "confirmar", 1 )

   ON ACTION ACCEPT

      IF (v_pesos97 IS NULL) AND
         (v_pesos92 IS NULL) THEN
         LET bnd_importe = 0
         CALL fn_mensaje("Aviso","Debe ingresar al menos un importe para continuar","stop")
      END IF

      IF (v_pesos97 IS NULL) AND
         (v_pesos92 IS NOT NULL) AND
         (v_pesos92 >= v_valor_fondo)THEN
         LET bnd_importe = 1
      END IF

      IF (v_pesos92 IS NULL) AND
         (v_pesos97 IS NOT NULL) AND
         (v_pesos97 >= v_valor_fondo) THEN
         LET bnd_importe = 1
      END IF

      IF (v_pesos92 IS NOT NULL) AND
         (v_pesos97 IS NOT NULL) AND
         (v_pesos97 >= v_valor_fondo)AND
         (v_pesos92 >= v_valor_fondo)THEN
         LET bnd_importe = 1
      END IF

      IF (bnd_importe = 1) AND
         (v_combo IS NOT NULL)THEN

         LET v_aivs92 = (v_pesos92 / v_valor_fondo)
         LET v_aivs97 = (v_pesos97 / v_valor_fondo)

         CALL f.setElementHidden    ("lb_valor_fondo",0)
         CALL f.setFieldHidden      ("v_valor_fondo",0)
         CALL f.setElementHidden    ("lb_aivs92",0)
         CALL f.setFieldHidden      ("v_aivs92",0)
         CALL f.setElementHidden    ("lb_aivs97",0)
         CALL f.setFieldHidden      ("v_aivs97",0)
         CALL DIALOG.setActionHidden( "confirmar", 0 )
         CALL DIALOG.setActionHidden( "accept", 1 )

         DISPLAY BY NAME v_valor_fondo
         DISPLAY BY NAME v_aivs97
         DISPLAY BY NAME v_aivs92

         --NEXT FIELD v_combo
         CALL DIALOG.setFieldActive("v_pesos92", FALSE)
         CALL DIALOG.setFieldActive("v_pesos97", FALSE)
      ELSE
         CALL fn_mensaje("Aviso","Valor de importe menor al minimo","stop")
         NEXT FIELD v_pesos97
      END IF

      CALL cb.clear()
      CALL cb.addItem(v_combo,v_combo)
      NEXT FIELD v_combo

   ON ACTION confirmar

      IF v_combo = "acr" THEN
         LET v_tpo_transferencia = 15
      END IF

      IF v_combo = "agr" THEN
         LET v_tpo_transferencia = 43
      END IF

      IF v_combo = "grt" THEN
         LET v_tpo_transferencia = 19
         LET v_num_credito = 0
      END IF

      LET v_folio = fn_genera_folio(202, 1, g_usuario)

      LET v_s_qryTxt = "EXECUTE PROCEDURE sp_inserta_dse(?,?,?,?,?,?,?,?)"

display "nss : ",v_nss
display "tpo trans : ",v_tpo_transferencia
display "nci: ",v_num_credito
display "folio : ",v_folio
display "pesos97 :", v_pesos97
display "aivs97 : ",v_aivs97
display "pesos92 : ",v_pesos92
display "aivs92 : ",v_aivs92


      PREPARE prp_inserta FROM  v_s_qryTxt
      EXECUTE prp_inserta USING v_nss,
                                v_tpo_transferencia,
                                v_num_credito,
                                v_folio,
                                v_pesos97,
                                v_aivs97,
                                v_pesos92,
                                v_aivs92
      LET v_cmd = "Los datos fueron almacenados de forma correcta \n","Folio de referencia  ",v_folio
      CALL fn_mensaje        ("Aviso",v_cmd,"stop")
      CALL f.setFieldHidden  ("v_combo",1)
      CALL f.setFieldHidden  ("v_nombre_completo",1)
      CALL f.setElementHidden("lb_pesos97",1)
      CALL f.setFieldHidden  ("v_pesos97",1)
      CALL f.setElementHidden("lb_modulo",1)
      CALL f.setElementHidden("lb_valor_fondo",1)
      CALL f.setFieldHidden  ("v_valor_fondo",1)
      CALL f.setElementHidden("lb_aivs97",1)
      CALL f.setFieldHidden  ("v_aivs97",1)
      CALL f.setElementHidden("lb_nombre",1)
      CALL f.setElementHidden("lb_pesos92",1)
      CALL f.setFieldHidden  ("v_pesos92",1)
      CALL f.setElementHidden("lb_aivs92",1)
      CALL f.setFieldHidden  ("v_aivs92",1)
      CALL f.setElementHidden("lb_num_credito",1)
      CALL f.setFieldHidden  ("v_num_credito",1)
      CALL f.setElementHidden("lb_nss",1)
      CALL f.setFieldHidden  ("v_nss",1)

   ON ACTION CANCEL
      CALL f.setFieldHidden  ("v_combo",1)
      CALL f.setFieldHidden  ("v_nombre_completo",1)
      CALL f.setElementHidden("lb_pesos97",1)
      CALL f.setFieldHidden  ("v_pesos97",1)
      CALL f.setElementHidden("lb_modulo",1)
      CALL f.setElementHidden("lb_valor_fondo",1)
      CALL f.setFieldHidden  ("v_valor_fondo",1)
      CALL f.setElementHidden("lb_aivs97",1)
      CALL f.setFieldHidden  ("v_aivs97",1)
      CALL f.setElementHidden("lb_nombre",1)
      CALL f.setElementHidden("lb_pesos92",1)
      CALL f.setFieldHidden  ("v_pesos92",1)
      CALL f.setElementHidden("lb_aivs92",1)
      CALL f.setFieldHidden  ("v_aivs92",1)
      CALL f.setElementHidden("lb_num_credito",1)
      CALL f.setFieldHidden  ("v_num_credito",1)
      CALL f.setElementHidden("lb_nss",1)
      CALL f.setFieldHidden  ("v_nss",1)
   END INPUT

   END DIALOG

CLOSE WINDOW DSEP01

END MAIN

FUNCTION fn_valida_numero(v_dato)

   DEFINE v_dato                        CHAR(1)

   LET bnd_dato = 0

   IF (v_dato MATCHES '[0-9]*') THEN
      LET bnd_dato = 1
   ELSE
      LET bnd_dato = 0
   END IF

END FUNCTION