#################################################################################
#Modulo              => OCG                                                     #
#Programa            => OCGC10                                                  #
#Objetivo            => PANTALLA DE CONSULTA PARA DEVOLUCIONES                  #
#Autor               => JOSÉ EDUARDO VENTURA                                    #
#Fecha inicio        => 27 de JULIO del 2016                                    #
#################################################################################

DATABASE safre_viv

   DEFINE v_qry             STRING
   DEFINE v_cadena          STRING
   DEFINE p_usuario         CHAR(20)
   DEFINE p_tpo_ejecucion   SMALLINT
   DEFINE p_s_titulo        CHAR(20)
   DEFINE v_nss             CHAR(11)
   DEFINE v_cb_edo          SMALLINT
   DEFINE v_f_devolucion    DATE
   DEFINE v_cve_ef          SMALLINT
   DEFINE v_periodo         CHAR(8)
   DEFINE cb                ui.ComboBox
   DEFINE cb2               ui.ComboBox
   DEFINE v_id              DECIMAL(9,0)

MAIN
      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC12.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW OCGC121 WITH FORM "OCGC121"

   INPUT BY NAME v_nss,
                 v_cve_ef,
                 v_f_devolucion,
                 v_periodo,
                 v_cb_edo  ATTRIBUTES(UNBUFFERED)

   BEFORE INPUT
      LET cb  = ui.ComboBox.forName("v_cve_ef")
      LET cb2 = ui.ComboBox.forName("v_cb_edo")
      CALL fn_llena_combo()

   ON ACTION ACCEPT

   LET v_cadena = " "
{
   DISPLAY "nss : ",v_nss
   DISPLAY v_cve_ef
   DISPLAY v_f_devolucion
   DISPLAY v_periodo
   DISPLAY v_cb_edo
}
   IF v_nss IS NOT NULL THEN
      SELECT id_derechohabiente
        INTO v_id
        FROM afi_derechohabiente
       WHERE nss = v_nss
      LET v_cadena = v_cadena," AND a.id_derechohabiente = ",v_id
      --DISPLAY v_cadena
   END IF

   IF v_cve_ef IS NOT NULL THEN
      LET v_cadena = v_cadena," AND a.cve_ent_financiera = ",v_cve_ef
      --DISPLAY v_cadena
   END IF

   IF v_f_devolucion IS NOT NULL THEN
      LET v_cadena = v_cadena," AND a.f_deposito = ","'",v_f_devolucion,"'"
      --DISPLAY v_cadena
   END IF

   IF v_periodo IS NOT NULL THEN
      LET v_cadena = v_cadena," AND a.periodo_pago = ","'",v_periodo,"'"
     -- DISPLAY v_cadena
   END IF

   IF v_cb_edo IS NOT NULL THEN
      LET v_cadena = v_cadena,"AND a.edo_registro = ",v_cb_edo
      --DISPLAY v_cadena
   END IF
--DISPLAY "cadena busqueda : ",v_cadena
      CALL fn_consulta_dev()
   --EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   
   CLOSE WINDOW OCGC121

END MAIN

FUNCTION fn_consulta_dev()

   DEFINE a          INTEGER

   DEFINE arr_cons_devolucion DYNAMIC ARRAY OF RECORD
      nss                    CHAR(11),
      periodo                CHAR(20),
      ent_financiera         CHAR(40),
      monto_aportacion       DECIMAL(12,2),
      monto_uso_garantia     DECIMAL(12,2),
      monto_pendiente        DECIMAL(12,2),
      f_devolucion           DATE,
      estado                 CHAR(40)
   END RECORD

   LET v_qry = "SELECT afi.nss,  
                       a.periodo_pago,
                       b.ent_financiera_desc,
                       a.importe_subsec_devuelto,
                       a.importe_ocg_devuelto,
                       a.importe_pendiente,
                       a.f_deposito,
                       c.edo_devolucion_desc
                  FROM ocg_devolucion a,
                       cat_entidad_financiera b,
                       cat_edo_devolucion c,
                       afi_derechohabiente afi
                 WHERE a.cve_ent_financiera = b.cve_ent_financiera
                   AND a.edo_registro       = c.edo_devolucion
                   AND a.id_derechohabiente = afi.id_derechohabiente ",v_cadena

   --DISPLAY " qry : ",v_qry

   PREPARE prp_cons_dev FROM v_qry
   DECLARE cur_cons_dev CURSOR FOR prp_cons_dev

   LET a = 1

   FOREACH cur_cons_dev INTO arr_cons_devolucion[a].*
      LET a = a+1
   END FOREACH

   CALL arr_cons_devolucion.deleteElement(arr_cons_devolucion.getLength())

   IF arr_cons_devolucion.getLength() >= 1 THEN

      OPEN WINDOW OCGC122 WITH FORM "OCGC122"
         DISPLAY ARRAY arr_cons_devolucion TO tab_devolucion.* ATTRIBUTES (UNBUFFERED, CANCEL = FALSE)
         ON ACTION ACCEPT
         EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW OCGC122
   ELSE
      CALL fn_mensaje ("Archivo","No se encontró información para parámetro de búsqueda ingresado","information")
      --CLOSE WINDOW OCGC122
   END IF

   CALL arr_cons_devolucion.clear()

   LET v_nss           = NULL
   LET v_cve_ef        = NULL
   LET v_f_devolucion  = NULL
   LET v_periodo       = NULL
   LET v_cb_edo        = NULL

END FUNCTION

FUNCTION fn_llena_combo()

   DEFINE v_qry    STRING
   DEFINE v_cnt    SMALLINT

   DEFINE arr_cb_entidad DYNAMIC ARRAY OF RECORD
      entidad       SMALLINT,
      entidad_desc  CHAR(40)
   END RECORD

   DEFINE arr_cb_devolucion DYNAMIC ARRAY OF RECORD
      estado        SMALLINT,
      estado_desc   CHAR(40)
   END RECORD

   LET v_qry = "SELECT cve_ent_financiera,
                       cve_ent_financiera||'-'||ent_financiera_desc
                  FROM cat_entidad_financiera
                 WHERE estado_ef = 10 ORDER BY cve_ent_financiera asc"

   PREPARE prp_combo FROM v_qry
   DECLARE cur_combo CURSOR FOR prp_combo

   LET v_cnt = 1

   FOREACH cur_combo INTO arr_cb_entidad[v_cnt].*
      CALL cb.addItem(arr_cb_entidad[v_cnt].entidad, arr_cb_entidad[v_cnt].entidad_desc)
      LET v_cnt = v_cnt + 1
   END FOREACH

   LET v_qry = "SELECT edo_devolucion,
                       edo_devolucion_desc
                  FROM cat_edo_devolucion
                 WHERE edo_devolucion in (160,170,180,190,200)
                  ORDER BY edo_devolucion asc"

   PREPARE prp_devolucion FROM v_qry
   DECLARE cur_devolucion CURSOR FOR prp_devolucion

   LET v_cnt = 1

   FOREACH cur_devolucion INTO arr_cb_devolucion[v_cnt].*
      CALL cb2.addItem(arr_cb_devolucion[v_cnt].estado, arr_cb_devolucion[v_cnt].estado_desc)
      LET v_cnt = v_cnt + 1
   END FOREACH

END FUNCTION