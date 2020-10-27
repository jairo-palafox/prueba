#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC14                                        #
#Objetivo            => PANTALLAS DE CONSULTA DE CONCILIACIÓN         #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 12 de ENERO de 2017                           #
#######################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_usuario        CHAR(20)
   DEFINE p_tpo_ejecucion  SMALLINT
   DEFINE p_s_titulo       CHAR(20)
   DEFINE w ui.Window
   DEFINE f ui.Form
   DEFINE ch               base.Channel
END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parámetro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC14.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   CALL fn_conciliacion()

END MAIN

FUNCTION fn_conciliacion()

   DEFINE v_qry          STRING
   DEFINE b              INTEGER
   DEFINE v_pos          SMALLINT
   DEFINE v_cnt_dev      INTEGER
   DEFINE v_nom_arh      STRING
   DEFINE v_detalle      STRING
   DEFINE a              INTEGER
   DEFINE v_ruta_envio   LIKE seg_modulo.ruta_envio
   DEFINE v_mensaje      STRING
   DEFINE v_devuelto     STRING
   DEFINE v_devolucion   STRING
   DEFINE v_disponible   STRING

   DEFINE arr_conc DYNAMIC ARRAY OF RECORD
      lote                 CHAR(11),
      cve_ent_financiera   CHAR(3),
      monto_devuelto       DECIMAL(12,5),
      monto_devolucio      DECIMAL(12,5),
      monto_disponible     DECIMAL(12,5),
      f_devolucion         DATE,
      f_proceso            DATE,
      estado               CHAR(40)
   END RECORD

   DEFINE arr_detalle DYNAMIC ARRAY OF RECORD
      nss                     CHAR(11),
      cve_ent_financiera      SMALLINT,
      num_ctr_int_ef          CHAR(18),
      importe_subsec_devuelto DECIMAL(12,5),
      importe_ocg_devuelto    DECIMAL(12,5),
      importe_pendiente       DECIMAL(12,5),
      f_deposito              DATE,
      edo_registro            CHAR(40)
   END RECORD

   LET v_qry = "SELECT a.lote,
                       a.cve_ent_financiera,
                       a.monto_devuelto,
                       a.monto_devolucion,
                       '',
                       a.f_devolucion,
                       a.f_proceso,
                       b.edo_devolucion_desc
                       FROM ocg_devolucion_ef a, 
                            cat_edo_devolucion b
                      WHERE a.estado = b.edo_devolucion "

   PREPARE prp_conc FROM v_qry
   DECLARE cur_conc CURSOR FOR prp_conc

   LET b = 1
   FOREACH cur_conc INTO arr_conc[b].*
      LET b = b+1
   END FOREACH

   CALL arr_conc.deleteElement(arr_conc.getLength())

   OPEN WINDOW OCGC14 WITH FORM "OCGC141"
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()

      SELECT ruta_envio
        INTO v_ruta_envio
        FROM seg_modulo
       WHERE modulo_cod = 'ocg'

      LET v_nom_arh = v_ruta_envio CLIPPED ,"/consulta_conciliacion",".con"

      CALL f.setElementHidden("table2",1)

   DISPLAY ARRAY arr_conc TO tab_conciliacion.*

   ON ACTION archivo

      LET ch = base.Channel.create()
      CALL ch.openFile(v_nom_arh,"w" )
      CALL ch.setDelimiter("|")

      LET ch = base.Channel.create()
      CALL ch.openFile(v_nom_arh,"w" )
      CALL ch.setDelimiter("|")

      FOR a = 1 TO arr_conc.getLength()
         LET v_devuelto   = arr_conc[a].monto_devuelto   CLIPPED
         LET v_devolucion = arr_conc[a].monto_devolucio  CLIPPED
         LET v_disponible = arr_conc[a].monto_disponible CLIPPED

         LET v_detalle = arr_conc[a].lote               CLIPPED,'|',
                         arr_conc[a].cve_ent_financiera CLIPPED,'|',
                         v_devuelto.trim()                     ,'|',
                         v_devolucion.trim()                   ,'|',
                         v_disponible.trim()                   ,'|',
                         arr_conc[a].f_proceso          USING "yyyymmdd",'|',
                         arr_conc[a].estado             CLIPPED,'|'

         CALL ch.writeLine([v_detalle])
      END FOR
      CALL ch.close()
      LET v_mensaje = "Archivo generado de forma correcta en : ",v_nom_arh
      CALL fn_mensaje ("Archivo",v_mensaje,"information")

   ON ACTION ACCEPT
      LET v_pos = ARR_CURR()

      SELECT COUNT(*)
         INTO v_cnt_dev
         FROM ocg_devolucion d,
              afi_derechohabiente afi
        WHERE d.id_derechohabiente = afi.id_derechohabiente 
          AND d.cve_ent_financiera = arr_conc[v_pos].cve_ent_financiera
          AND d.f_deposito = arr_conc[v_pos].f_devolucion

   IF v_cnt_dev >= 1 THEN

      CALL f.setElementHidden("table1",1)
      CALL f.setElementHidden("table2",0)

      LET v_qry =

      "SELECT a.nss,
              d.cve_ent_financiera,
              d.num_ctr_int_ef,
              d.importe_subsec_devuelto,
              d.importe_ocg_devuelto,
              d.importe_pendiente,
              d.f_deposito,
              d.edo_registro
         FROM ocg_devolucion d,
              afi_derechohabiente a
        WHERE d.id_derechohabiente = a.id_derechohabiente 
          AND d.cve_ent_financiera = ",arr_conc[v_pos].cve_ent_financiera,"
          AND d.f_deposito = '",arr_conc[v_pos].f_devolucion,"'
     ORDER BY edo_registro"

   PREPARE prp_detalle FROM v_qry
   DECLARE cur_detalle CURSOR FOR prp_detalle

   LET b = 1
   FOREACH cur_detalle INTO arr_detalle[b].*
      LET b = b+1
   END FOREACH

   CALL arr_detalle.deleteElement(arr_detalle.getLength())

   DISPLAY ARRAY arr_detalle TO tab_detalle.*
      END DISPLAY
      CALL f.setElementHidden("table2",1)
      CALL f.setElementHidden("table1",0)

   ELSE
      CALL fn_mensaje("Alerta","No se encontraron resultados para registro seleccionado","stop")
   END IF

   --EXIT DISPLAY

   ON  ACTION CANCEL
   EXIT DISPLAY

   END DISPLAY
   CLOSE WINDOW OCGC14

END FUNCTION