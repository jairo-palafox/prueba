#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC18                                        #
#Objetivo            => consulta rechazo solicitud uso garantía       #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 09 de Noviembre de 2017                       #
#######################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario        CHAR(20)
   DEFINE p_tpo_ejecucion  SMALLINT
   DEFINE p_s_titulo       CHAR(20)
   DEFINE v_f_proceso      DATE
   DEFINE v_nss            CHAR(11)
   DEFINE v_s_qry          STRING
   DEFINE a                INTEGER

   DEFINE arr_rch_gtia DYNAMIC ARRAY OF RECORD
      nss                 CHAR(11),
      periodo             CHAR(8),
      tpo_uso             CHAR(3),
      imp_solicitado      DECIMAL(15,2),
      f_proceso           DATE,
      estado              SMALLINT,
      estado_desc         CHAR(40),
      edo_procesar        SMALLINT,
      edo_desc            CHAR(40),
      diagnostico         SMALLINT,
      diagnostico_desc    CHAR(40)
   END RECORD

END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parámetro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC21.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW OCGC201 WITH FORM "OCGC211"

   INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      LET v_s_qry = "
select a.nss,
       c.periodo_pago,
       c.tpo_uso,
       c.importe_v97,
       c.f_proceso,
       c.estado,
       d.estado_desc,
       c.edo_procesar,
       e.estado_desc,
       c.diagnostico,
       f.desc_estado
  from cre_uso_garantia c,
       afi_derechohabiente a,
       cat_maq_credito d,
       cat_maq_credito e,
       cat_rch_garantia f
 where a.id_derechohabiente = c.id_derechohabiente
   and c.estado = d.estado
   and c.edo_procesar = e.estado
   and c.diagnostico = f.estado
   and c.estado = 240
   and c.tpo_transferencia = '18'
   and c.f_proceso > '01/01/2017'
   and a.nss = '",v_nss,"'"

   PREPARE prp_rch_gtia1 FROM v_s_qry
   DECLARE cur_rch_gtia1 CURSOR FOR prp_rch_gtia1

   LET a = 1

   FOREACH cur_rch_gtia1 INTO arr_rch_gtia[a].*
      LET a = a+1
   END FOREACH

   LET v_s_qry = "
select a.nss,
       c.periodo_pago,
       c.tpo_uso,
       c.importe_v97,
       c.f_proceso,
       c.estado,
       d.estado_desc,
       c.edo_procesar,
       e.estado_desc,
       c.diagnostico,
       f.rch_desc
  from cre_uso_garantia c,
       afi_derechohabiente a,
       cat_maq_credito d,
       cat_maq_credito e,
       cat_rch_marca f
 where a.id_derechohabiente = c.id_derechohabiente
   and c.estado = d.estado
   and c.edo_procesar = e.estado
   and c.diagnostico = f.rch_cod
   and c.estado = 150
   and tpo_transferencia = '18'
   and c.f_proceso > '01/01/2017'
   and a.nss = '",v_nss,"'"

   PREPARE prp_rch_gtia2 FROM v_s_qry
   DECLARE cur_rch_gtia2 CURSOR FOR prp_rch_gtia2

   FOREACH cur_rch_gtia2 INTO arr_rch_gtia[a].*
      LET a = a+1
   END FOREACH

   LET v_s_qry = "
select a.nss,
       c.periodo_pago,
       c.tpo_uso,
       c.importe_v97,
       c.f_proceso,
       c.estado,
       d.estado_desc,
       c.edo_procesar,
       e.estado_desc,
       c.diagnostico,
       f.desc_rechazo
  from cre_uso_garantia c,
       afi_derechohabiente a,
       cat_maq_credito d,
       cat_maq_credito e,
       cat_rechazo f
 where a.id_derechohabiente = c.id_derechohabiente
   and c.estado = d.estado
   and c.edo_procesar = e.estado
   and c.estado NOT IN(240,150,142)
   and f.tpo_rechazo = 'RCH'
   and c.diagnostico = f.cod_rechazo
   and tpo_transferencia = '18'
   and c.f_proceso > '01/01/2017'
   and a.nss = '",v_nss,"'"

   PREPARE prp_rch_gtia3 FROM v_s_qry
   DECLARE cur_rch_gtia3 CURSOR FOR prp_rch_gtia3

   FOREACH cur_rch_gtia3 INTO arr_rch_gtia[a].*
      LET a = a+1
   END FOREACH

   CALL arr_rch_gtia.deleteElement(a)

   IF  arr_rch_gtia.getLength() >= 1 THEN
      OPEN WINDOW OCGC212 WITH FORM "OCGC212"

      DISPLAY ARRAY arr_rch_gtia TO tab_rch_gtia.*
      
      ON ACTION archivo
         CALL fn_arch_rch()

      END DISPLAY
      CLOSE WINDOW OCGC212
   ELSE
      CALL fn_mensaje("Alerta","No se encontraron rechazos para NSS ingresado","stop")
   END IF

   ON ACTION CANCEL

   EXIT INPUT
   END INPUT

      CLOSE WINDOW OCGC211

END MAIN

FUNCTION fn_arch_rch()

   DEFINE v_detalle        STRING
   DEFINE a                INTEGER
   DEFINE v_mensaje        STRING
   DEFINE ch               base.Channel
   DEFINE v_nom_arh        STRING
   DEFINE v_ruta_envio     LIKE seg_modulo.ruta_envio

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
     WHERE modulo_cod = 'ocg'

   LET v_nom_arh = v_ruta_envio CLIPPED ,"/consulta_rechazos_garantia",".rch"

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nom_arh,"w" )
   CALL ch.setDelimiter("|")

   LET a = 1

   FOR a = 1 TO arr_rch_gtia.getLength()

   LET v_detalle =  arr_rch_gtia[a].nss              CLIPPED,"|",
                    arr_rch_gtia[a].periodo          CLIPPED,"|",
                    arr_rch_gtia[a].tpo_uso          CLIPPED,"|",
                    arr_rch_gtia[a].imp_solicitado   CLIPPED,"|",
                    arr_rch_gtia[a].f_proceso        CLIPPED,"|",
                    arr_rch_gtia[a].estado           CLIPPED,"|",
                    arr_rch_gtia[a].estado_desc      CLIPPED,"|",
                    arr_rch_gtia[a].edo_procesar     CLIPPED,"|",
                    arr_rch_gtia[a].edo_desc         CLIPPED,"|",
                    arr_rch_gtia[a].diagnostico      CLIPPED,"|",
                    arr_rch_gtia[a].diagnostico_desc CLIPPED

   CALL ch.writeLine([v_detalle])

   END FOR

   CALL ch.close()

    LET v_mensaje = "Archivo generado de forma correcta en : ",v_nom_arh
    CALL fn_mensaje ("Archivo",v_mensaje,"information")

END FUNCTION
