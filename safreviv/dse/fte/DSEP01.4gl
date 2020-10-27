####################################################################
#Modulo            =>DSE                                           #
#Programa          =>DSEP01                                        #
#Objetivo          =>Programa de restitución a cuentas individuales#
#                    de los Módulos ACR,AGR,GRT                    #
#Autor             =>José Eduardo Ventura                          #
#Fecha inicio      =>04 Noviembre 2015                             #
####################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario CHAR(20)
   DEFINE p_s_titulo CHAR(20)
   DEFINE bnd_dato SMALLINT
   DEFINE v_nombre CHAR (40)
   DEFINE v_ap_paterno CHAR (40)
   DEFINE v_ap_materno CHAR(40)
   DEFINE v_nombre_completo STRING
   DEFINE a SMALLINT
   DEFINE v_nss CHAR(11)
   DEFINE v_dato CHAR(1)
   DEFINE cb  ui.ComboBox
   --DEFINE rec_modulo RECORD LIKE seg_modulo.*
   DEFINE arr_modulo DYNAMIC ARRAY OF RECORD LIKE seg_modulo.*
   DEFINE v_combo CHAR (3)
   DEFINE v_pesos97 DECIMAL(12,4)
   DEFINE v_pesos92 DECIMAL(12,4)
   DEFINE w ui.Window
   DEFINE f ui.Form
   DEFINE v_num_credito DECIMAL (10,0)


END GLOBALS

MAIN

   LET g_usuario    = ARG_VAL(1)

   CALL STARTLOG(g_usuario CLIPPED|| ".DSEP01.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW DSEP01 WITH FORM "DSEP01"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   CALL f.setFieldHidden("v_combo",1)
   CALL f.setFieldHidden("v_nombre_completo",1)
   CALL f.setElementHidden("lb_pesos97",1)
   CALL f.setFieldHidden("v_pesos97",1)
   CALL f.setElementHidden("lb_modulo",1)
   CALL f.setElementHidden("lb_valor_fondo",1)
   CALL f.setFieldHidden("v_valor_fondo",1)
   CALL f.setElementHidden("lb_aivs97",1)
   CALL f.setFieldHidden("v_aivs97",1)
   CALL f.setElementHidden("lb_nombre",1)
   CALL f.setElementHidden("lb_pesos92",1)
   CALL f.setFieldHidden("v_pesos92",1)
   CALL f.setElementHidden("lb_aivs92",1)
   CALL f.setFieldHidden("v_aivs92",1)


   DIALOG ATTRIBUTES (UNBUFFERED)
   
   INPUT BY NAME v_nss , v_num_credito

   ON ACTION ACCEPT
      IF (LENGTH(v_nss) > 11) OR
         (LENGTH(v_nss) < 11) OR
         (v_nss IS NULL ) THEN
      CALL fn_mensaje("Aviso","Cantidad de caracteres de NSS no es válido o NSS es nulo ","stop")
   END IF

   IF (LENGTH(v_nss) = 11) AND (v_num_credito IS NULL) THEN
      CALL fn_mensaje("Aviso","Debe ingresar un número de crédito","stop")
   END IF

   IF (LENGTH(v_nss) = 11) AND (v_num_credito IS NOT NULL) THEN
      LET a=1
      FOR a=1 TO LENGTH(v_nss)
         LET v_dato = v_nss[a,a]
         CALL fn_valida_numero(v_dato)
         IF bnd_dato= 0 THEN
            EXIT FOR
            CALL fn_mensaje("Aviso","Error en valdación de NSS,\n debe ingresar un NSS válido","stop")
         END IF
      END FOR
   END IF

   IF bnd_dato = 1 THEN
      CALL fn_1()
   END IF

   ON ACTION CLOSE
      EXIT DIALOG
      END INPUT 

END DIALOG

CLOSE WINDOW DSEP01
   
END MAIN

FUNCTION fn_valida_numero(v_dato)

   DEFINE v_dato CHAR(1)


   LET bnd_dato = 0

   IF (v_dato MATCHES '[0-9]*') THEN
      LET bnd_dato = 1
   ELSE
      LET bnd_dato = 0
   END IF

END FUNCTION

FUNCTION fn_1()

   DEFINE v_aivs92 DECIMAL (19,14)
   DEFINE v_aivs97 DECIMAL (19,14)
   DEFINE v_valor_fondo DECIMAL (19,14)
   DEFINE v_tpo_transferencia    CHAR (2)
   DEFINE v_s_qryTxt   STRING
   DEFINE v_folio      INTEGER

   SELECT ap_paterno_af,
          ap_materno_af,
          nombre_af
     INTO v_ap_paterno,
          v_ap_materno,
          v_nombre
     FROM afi_derechohabiente
    WHERE nss = v_nss

   LET v_nombre_completo = v_ap_paterno CLIPPED," ",v_ap_materno CLIPPED," ",v_nombre CLIPPED
   DISPLAY BY NAME v_nombre_completo

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
   
   CALL f.setFieldHidden("v_combo",0)
   CALL f.setFieldHidden("v_nombre_completo",0)
   CALL f.setElementHidden("lb_pesos97",0)
   CALL f.setFieldHidden("v_pesos97",0)
   CALL f.setElementHidden("lb_modulo",0)
   CALL f.setElementHidden("lb_nombre",0)
   CALL f.setElementHidden("lb_pesos92",0)
   CALL f.setFieldHidden("v_pesos92",0)

   LET v_combo = arr_modulo[1].modulo_cod

   INPUT BY NAME v_combo , v_pesos97 ,v_pesos92 ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

   BEFORE INPUT
      CALL DIALOG.setActionHidden( "confirmar", 1 ) 
      --DISPLAY BY NAME v_combo
   ON ACTION ACCEPT
      IF (v_pesos97 IS NOT NULL ) AND
         (v_pesos97 > 0 ) AND
         (v_pesos92 IS NOT NULL ) AND
         (v_pesos92 > 0 ) AND
         (v_combo IS NOT NULL) THEN

      SELECT precio_fondo
        INTO v_valor_fondo
        FROM glo_valor_fondo
       WHERE f_valuacion = TODAY
         AND fondo = 11

       IF (v_valor_fondo IS NULL) OR
          (v_valor_fondo < 0) THEN
           CALL fn_mensaje("Aviso","No exíste precio de fondo para el día de hoy","stop")
           CALL f.setFieldHidden("v_combo",1)
           CALL f.setFieldHidden("v_pesos97",1)
           CALL f.setFieldHidden("v_pesos92",1)
           CALL f.setFieldHidden("v_nombre_completo",1)
           CALL f.setElementHidden("lb_nombre",1)
           CALL f.setElementHidden("lb_pesos97",1)
           CALL f.setElementHidden("lb_pesos92",1)
           CALL f.setElementHidden("lb_modulo",1)
       END IF

       IF v_valor_fondo > 0 THEN
          LET v_aivs92 = (v_pesos92 * v_valor_fondo)
          LET v_aivs97 = (v_pesos97 * v_valor_fondo)
          
          CALL f.setElementHidden("lb_valor_fondo",0)
          CALL f.setFieldHidden("v_valor_fondo",0)
          CALL f.setElementHidden("lb_aivs92",0)
          CALL f.setFieldHidden("v_aivs92",0)
          CALL f.setElementHidden("lb_aivs97",0)
          CALL f.setFieldHidden("v_aivs97",0)
          
          DISPLAY BY NAME v_valor_fondo
          DISPLAY BY NAME v_aivs97
          DISPLAY BY NAME v_aivs92
          CALL DIALOG.setActionHidden( "confirmar", 0 )
          CALL DIALOG.setActionHidden( "accept", 1 )
       END IF

       ELSE
          CALL fn_mensaje("Aviso","Necesita ingresar un importe válido para continuar","stop")

      END IF

      ON ACTION confirmar

         IF v_combo = "acr" THEN
            LET v_tpo_transferencia = 15
         END IF

         IF v_combo = "agr" THEN
            LET v_tpo_transferencia = 43
         END IF

         IF v_combo = "grt" THEN
            LET v_tpo_transferencia = 19
         END IF

         {DISPLAY "NSS : "       ,v_nss
         DISPLAY "Num Crédito"  ,v_num_credito
         DISPLAY "TPO TRNAS :"  ,v_tpo_transferencia
         DISPLAY "PESOS97 :"    ,v_pesos97
         DISPLAY "PESOS92 :"    ,v_pesos92
         DISPLAY "AIVS92 :"     ,v_aivs92
         DISPLAY "AIVS97 :"     ,v_aivs97}

         LET v_folio = fn_genera_folio(202, 1, g_usuario)
         --DISPLAY "folio",v_folio

         LET v_s_qryTxt = "EXECUTE PROCEDURE sp_inserta_dse(?,?,?,?,?,?,?,?)"

         PREPARE prp_inserta FROM  v_s_qryTxt
         EXECUTE prp_inserta USING v_nss,
                                              v_tpo_transferencia,
                                              v_num_credito,
                                              v_folio,
                                              v_pesos97,
                                              v_aivs97,
                                              v_pesos92,
                                              v_aivs92

         CALL fn_mensaje("Aviso","Los datos fueron almacenados de forma correcta","stop")

        CALL f.setFieldHidden("v_combo",1)
   CALL f.setFieldHidden("v_nombre_completo",1)
   CALL f.setElementHidden("lb_pesos97",1)
   CALL f.setFieldHidden("v_pesos97",1)
   CALL f.setElementHidden("lb_modulo",1)
   CALL f.setElementHidden("lb_valor_fondo",1)
   CALL f.setFieldHidden("v_valor_fondo",1)
   CALL f.setElementHidden("lb_aivs97",1)
   CALL f.setFieldHidden("v_aivs97",1)
   CALL f.setElementHidden("lb_nombre",1)
   CALL f.setElementHidden("lb_pesos92",1)
   CALL f.setFieldHidden("v_pesos92",1)
   CALL f.setElementHidden("lb_aivs92",1)
   CALL f.setFieldHidden("v_aivs92",1)
   CALL f.setElementHidden("lb_num_credito",1)
   CALL f.setFieldHidden("v_num_credito",1)
   CALL f.setElementHidden("lb_nss",1)
   CALL f.setFieldHidden("v_nss",1)

      ON ACTION CANCEL
         CALL f.setFieldHidden("v_combo",1)
   CALL f.setFieldHidden("v_nombre_completo",1)
   CALL f.setElementHidden("lb_pesos97",1)
   CALL f.setFieldHidden("v_pesos97",1)
   CALL f.setElementHidden("lb_modulo",1)
   CALL f.setElementHidden("lb_valor_fondo",1)
   CALL f.setFieldHidden("v_valor_fondo",1)
   CALL f.setElementHidden("lb_aivs97",1)
   CALL f.setFieldHidden("v_aivs97",1)
   CALL f.setElementHidden("lb_nombre",1)
   CALL f.setElementHidden("lb_pesos92",1)
   CALL f.setFieldHidden("v_pesos92",1)
   CALL f.setElementHidden("lb_aivs92",1)
   CALL f.setFieldHidden("v_aivs92",1)

   END INPUT
END FUNCTION