--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>OCG                                           #
#Programa          =>OCGS08                                        #
#Objetivo          =>Programa que ejecuta prospectos de devolución #
#Autor             =>José Eduardo Ventura                          #
#Fecha inicio      =>16 febrero 2016                               #
####################################################################

DATABASE safre_viv

GLOBALS

   DEFINE v_f_proceso       DATE
   DEFINE p_usuario         CHAR(20)
   DEFINE v_name            STRING

END GLOBALS

MAIN
   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGS09.log")

   -- se recuperan los parametros
   LET p_usuario         = ARG_VAL(1)
   OPEN WINDOW OCGS09 WITH FORM "OCGS09"

      CLOSE WINDOW SCREEN
      INPUT BY NAME v_f_proceso ATTRIBUTES (UNBUFFERED)

      ON ACTION ACCEPT
      CALL fn_prospecto(v_f_proceso)
      EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT

       END INPUT

   CLOSE WINDOW OCGS09

END MAIN

FUNCTION fn_prospecto(v_f_proceso)

   DEFINE v_s_qry             STRING
   DEFINE v_cnt_pros          INTEGER
   DEFINE a                   INTEGER
   DEFINE z                   INTEGER
   DEFINE v_aiv_eq_1          DECIMAL(13,5)
   DEFINE v_aiv_eq_2          DECIMAL(13,5)
   DEFINE v_f_transaccion     DATE
   DEFINE v_f_trans           CHAR(10)
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nom_arch          STRING
   DEFINE v_nom_arch1         STRING
   DEFINE ch                  base.Channel
   DEFINE v_imp_devolucion    DECIMAL(13,2)
   DEFINE v_f_liberacion_gtia CHAR(8)
   DEFINE v_f_pago            CHAR(8)
   DEFINE v_paterno           CHAR(40)
   DEFINE v_materno           CHAR(40)
   DEFINE v_nombre            CHAR(40)
   DEFINE v_penalizacion      DECIMAL(13,2)
   DEFINE v_f_actual          DATE
   DEFINE v_f_desde           DATE
   DEFINE v_dias              INTEGER
   DEFINE v_detalle           STRING
   DEFINE v_cmd               STRING
   DEFINE v_f_proceso         DATE
   DEFINE v_qry               STRING
   DEFINE v_periodo_dis       CHAR(6)
   DEFINE v_imp_ap_pat        DECIMAL(12,2)
   DEFINE v_precio_fondo_today DECIMAL(19,14)
   DEFINE v_precio_fondo_15   DECIMAL(19,14)
   DEFINE v_aiv_ap_pat        DECIMAL(18,6)
   DEFINE v_imp_revaluado_15  DECIMAL(13,2)
   DEFINE v_f_transaccion_d   DATE
   DEFINE v_f_libera_garantia_d DATE
   DEFINE v_dia               SMALLINT
   DEFINE v_id_transaccion    DECIMAL(9,0)
   DEFINE v_id_dh             DECIMAL(9,0)
   DEFINE v_nss               CHAR(11)
   DEFINE v_cve_ent_financiera SMALLINT
   DEFINE v_num_ctrl_ef       CHAR(18)
   DEFINE v_id_detalle        DECIMAL(9,0)
   DEFINE v_cnt_prospecto     INTEGER
   DEFINE v_f_libera_garantia DATE
   DEFINE v_id_ocg_formalizacion DECIMAL(9,0)
   DEFINE x                   SMALLINT
   DEFINE v_dato              CHAR(1)

   DEFINE arr_pros_dev DYNAMIC ARRAY OF RECORD
          id_ocg_ctr_transaccion DECIMAL(9,0),
          id_ocg_formalizacion  DECIMAL(9,0),
          id_derechohabiente    DECIMAL(9,0),
          paterno               CHAR(40),
          materno               CHAR(40),
          nombre                CHAR(40),
          nss                   CHAR(11),
          cve_ent_financiera    SMALLINT,
          num_ctr_int_ef        char(18),
          f_pago                DATE,
          bimestre_ap_subsec    char(6),
          aiv_ap_pat            DECIMAL(18,2),
          importe_ap_pat        DECIMAL(13,2),
          imp_rev_f_proeceso    DECIMAL(18,2),
          imp_revaluado_15      DECIMAL(13,6),
          f_transaccion         CHAR(8),
          f_factura             CHAR(10),
          f_liberacion_gtia     CHAR(8)
   END RECORD

   DEFINE arr_id_liquidado DYNAMIC ARRAY OF RECORD
      nss                  CHAR(11),
      id_derechohabiente   DECIMAL(9,0),
      id_ocg_detalle       DECIMAL(9,0),
      id_ocg_liquidacion   DECIMAL(9,0),
      f_liberacion_gtia    DATE
   END RECORD

   DEFINE arr_transaccion  DYNAMIC ARRAY OF RECORD
      id_transaccion       DECIMAL(9,0),
      f_transaccion        DATE,
      cve_ent_financiera   SMALLINT,
      num_ctr_int_ef       CHAR(18)
   END RECORD

   CALL fn_crea_tmp()

   DATABASE safre_viv

   SELECT COUNT(*)
     INTO v_cnt_prospecto
     FROM ocg_ctr_prospecto
    WHERE f_busqueda = v_f_proceso

   IF v_cnt_prospecto >= 1 THEN
      CALL fn_mensaje("Aviso","Ya se generó archivo para fecha solicitada","info")
   ELSE

   LET v_qry ="SELECT d.nss,
                      d.id_derechohabiente,
                      d.id_ocg_detalle,
                      l.id_ocg_liquidacion,
                      l.f_liberacion_gtia
                 FROM ocg_detalle d,
                      ocg_liquidacion l
                WHERE d.id_ocg_detalle = l.id_ocg_detalle
                  AND l.situacion in (140,150,160)
                  AND f_proceso = '",v_f_proceso,"'"

   --DISPLAY v_qry
   PREPARE prp_dev FROM v_qry
   DECLARE cur_dev CURSOR FOR prp_dev

   LET z= 1

   FOREACH cur_dev INTO arr_id_liquidado[z].*
     -- DISPLAY arr_id_liquidado[z].*
      LET z = z+1
   END FOREACH

   CALL arr_id_liquidado.deleteElement(z)
{
   IF arr_id_liquidado.getLength() <= 0 THEN
       CALL fn_mensaje("Aviso","No se encontraron registros para fecha solicitada","info")
   END IF
}
   IF arr_id_liquidado.getLength() >= 1 THEN

   FOR z = 1 TO arr_id_liquidado.getLength()

      LET v_qry =" SELECT id_ocg_ctr_transaccion,
                          f_transaccion,
                          cve_ent_financiera,
                          num_ctr_int_ef
                     FROM ocg_ctr_transaccion
                    WHERE nss                = ",arr_id_liquidado[z].nss,
                    " AND id_derechohabiente = ",arr_id_liquidado[z].id_derechohabiente,
                    " AND f_transaccion      >= '",arr_id_liquidado[z].f_liberacion_gtia,"'",
                    " AND estado in (60,70,80)",
                    " AND concepto not in (508,608) "

      --DISPLAY v_qry
      LET a= 1

      PREPARE prp_transaccion FROM v_qry
      DECLARE cur_transaccion CURSOR FOR prp_transaccion

      FOREACH cur_transaccion INTO arr_transaccion[a].*
         --DISPLAY "transacción  : ",arr_transaccion[a].*
         LET a = a+1
      END FOREACH

      --DISPLAY "Punto de control 1 :"

      CALL arr_transaccion.deleteElement(a)

      FOR a = 1 TO arr_transaccion.getLength()
         IF v_f_transaccion IS NOT NULL THEN

            SELECT periodo_pago,
                   f_pago,
                   vivienda_97,
                   f_transaccion,
                   id_ocg_formalizacion
              INTO v_periodo_dis,
                   v_f_pago,
                   v_imp_ap_pat,
                   v_f_transaccion,
                   v_id_ocg_formalizacion
              FROM ocg_ctr_transaccion
             WHERE id_ocg_ctr_transaccion = arr_transaccion[a].id_transaccion;

             --DISPLAY ""
             --DISPLAY "ocg_transaccion :",arr_transaccion[a].id_transaccion
             --DISPLAY "f_transaccion   : ",v_f_transaccion

            SELECT precio_fondo
              INTO v_precio_fondo_today
              FROM glo_valor_fondo
             WHERE f_valuacion = v_f_transaccion
               AND fondo = 11;

               --DISPLAY "precio fondo : ",v_precio_fondo_today
               --DISPLAY ""

            LET v_aiv_ap_pat = (v_imp_ap_pat/v_precio_fondo_today);
            LET v_imp_revaluado_15 = v_imp_ap_pat;
            LET v_precio_fondo_15  = v_precio_fondo_today;

            LET v_f_transaccion_d = (v_f_transaccion + 16 UNITS DAY);

            --DISPLAY "transaccion : ",v_f_transaccion
            --DISPLAY "más 16      : ",v_f_transaccion_d

            IF v_f_transaccion_d <= v_f_proceso THEN

              -- DISPLAY "punto de control 1"

               SELECT precio_fondo
                 INTO v_precio_fondo_15
                 FROM glo_valor_fondo
                WHERE f_valuacion = v_f_proceso
                  AND fondo = 11;

               LET v_imp_revaluado_15 = (v_aiv_ap_pat*v_precio_fondo_15);
            END IF
            --DISPLAY "importe       : ",v_imp_ap_pat
            --DISPLAY "imp revaluado : ",v_imp_revaluado_15

            LET v_f_transaccion_d = (v_f_transaccion - 16);
            LET v_dia = DAY(arr_id_liquidado[z].f_liberacion_gtia)
            LET v_f_libera_garantia_d = (arr_id_liquidado[z].f_liberacion_gtia);
            LET v_f_libera_garantia_d = (v_f_libera_garantia_d + 1 );
            LET v_f_libera_garantia_d = (v_f_libera_garantia_d - v_dia );
            LET v_f_libera_garantia   = arr_id_liquidado[z].f_liberacion_gtia --USING "yyyymmdd"
            LET v_id_transaccion     = arr_transaccion[a].id_transaccion
            LET v_id_dh              = arr_id_liquidado[z].id_derechohabiente
            LET v_nss                = arr_id_liquidado[z].nss
            LET v_cve_ent_financiera = arr_transaccion[a].cve_ent_financiera
            LET v_num_ctrl_ef        = arr_transaccion[a].num_ctr_int_ef
            LET v_id_detalle         = arr_id_liquidado[z].id_ocg_detalle

         -- Se inserta en la tabla ocg_devolucion_prospecto
{
            DISPLAY v_id_transaccion
            DISPLAY v_id_detalle
            DISPLAY v_id_dh
            DISPLAY v_nss
            DISPLAY v_cve_ent_financiera
            DISPLAY v_num_ctrl_ef
            DISPLAY v_periodo_dis
            DISPLAY v_imp_ap_pat
            DISPLAY v_aiv_ap_pat
            DISPLAY v_imp_revaluado_15
            DISPLAY v_precio_fondo_15
            DISPLAY v_f_transaccion
            DISPLAY "f libera",v_f_libera_garantia
}
            INSERT INTO safre_tmp:tmp_ocg_devolucion_prospecto
                 VALUES(0,
                        v_id_transaccion,
                        v_id_detalle,
                        v_id_ocg_formalizacion,
                        v_id_dh,
                        v_nss,
                        v_cve_ent_financiera,
                        v_num_ctrl_ef,
                        v_periodo_dis,
                        v_imp_ap_pat,
                        v_aiv_ap_pat,
                        v_imp_revaluado_15,
                        v_precio_fondo_15,
                        v_f_transaccion,
                        v_f_libera_garantia,
                        1,
                        30,
                        140);
         END IF
      END FOR
      CALL arr_transaccion.clear()
   END FOR
   END IF

   -- comentado para no generar prospectos de devolución por acuerdo temporal con usuario 13/12/2016
   SELECT COUNT(*)
     INTO v_cnt_pros
     FROM safre_tmp:tmp_ocg_devolucion_prospecto p

   --LET v_cnt_pros = 0

   IF v_cnt_pros >= 1 THEN

            LET v_s_qry ="SELECT p.id_referencia_dis,
                                 p.id_ocg_formalizacion,
                                 p.id_derechohabiente,
                                 a.ap_paterno_af,
                                 a.ap_materno_af,
                                 a.nombre_af,
                                 p.nss,
                                 p.cve_ent_financiera,
                                 p.num_ctr_int_ef,
                                 p.bimestre_ap_subsec,
                                 p.aiv_ap_pat,
                                 p.importe_ap_pat,
                                 p.imp_revaluado_15,
                                 p.aiv_revaluado_15,
                                 lpad(year(p.f_transaccion),4,0)||
                                 lpad(month(p.f_transaccion),2,0)||
                                 lpad(day(p.f_transaccion),2,0),
                                 lpad(year(p.f_factura),4,0)||
                                 lpad(month(p.f_factura),2,0)||
                                 lpad(day(p.f_factura),2,0)
                            FROM safre_tmp:tmp_ocg_devolucion_prospecto p,
                                 afi_derechohabiente a
                           WHERE p.id_derechohabiente = a.id_derechohabiente"

      PREPARE prp_pros_dev FROM v_s_qry
      DECLARE cur_pros_dev CURSOR FOR prp_pros_dev

      LET a = 1
      FOREACH cur_pros_dev INTO arr_pros_dev[a].id_ocg_ctr_transaccion,
                           arr_pros_dev[a].id_ocg_formalizacion,
                           arr_pros_dev[a].id_derechohabiente,
                           arr_pros_dev[a].paterno,
                           arr_pros_dev[a].materno,
                           arr_pros_dev[a].nombre,
                           arr_pros_dev[a].nss,
                           arr_pros_dev[a].cve_ent_financiera,
                           arr_pros_dev[a].num_ctr_int_ef,
                           arr_pros_dev[a].bimestre_ap_subsec,
                           arr_pros_dev[a].aiv_ap_pat,
                           arr_pros_dev[a].importe_ap_pat,
                           arr_pros_dev[a].imp_rev_f_proeceso,
                           arr_pros_dev[a].imp_revaluado_15,
                           arr_pros_dev[a].f_transaccion,
                           arr_pros_dev[a].f_liberacion_gtia

         LET v_f_trans       = arr_pros_dev[a].f_transaccion
         --LET v_f_transaccion = v_f_trans[5,6],"/",v_f_trans[7,8],"/",v_f_trans[1,4]
         LET v_f_trans       = v_f_trans[5,6],"/",v_f_trans[7,8],"/",v_f_trans[1,4]
         
         SELECT UNIQUE f_pago
           INTO arr_pros_dev[a].f_pago
           FROM ocg_ctr_transaccion
          WHERE id_ocg_formalizacion = arr_pros_dev[a].id_ocg_formalizacion
            AND id_derechohabiente   = arr_pros_dev[a].id_derechohabiente
            AND periodo_pago         = arr_pros_dev[a].bimestre_ap_subsec
            AND f_transaccion        = v_f_trans
            AND id_ocg_ctr_transaccion = arr_pros_dev[a].id_ocg_ctr_transaccion

            --DISPLAY "fecha pago : ",arr_pros_dev[a].f_pago

         IF arr_pros_dev[a].cve_ent_financiera IS NULL THEN 
            LET arr_pros_dev[a].cve_ent_financiera = 0 
         END IF

         IF arr_pros_dev[a].num_ctr_int_ef     IS NULL THEN 
            LET arr_pros_dev[a].num_ctr_int_ef     = 0
         END IF

         IF arr_pros_dev[a].bimestre_ap_subsec IS NULL THEN 
            LET arr_pros_dev[a].bimestre_ap_subsec = " "
         END IF 

         IF arr_pros_dev[a].aiv_ap_pat         IS NULL THEN 
            LET arr_pros_dev[a].aiv_ap_pat         = 0
         END IF

         IF arr_pros_dev[a].importe_ap_pat     IS NULL THEN 
            LET arr_pros_dev[a].importe_ap_pat     = 0
         END IF 

         IF arr_pros_dev[a].imp_rev_f_proeceso IS NULL THEN 
            LET arr_pros_dev[a].imp_rev_f_proeceso = 0
         END IF 

         IF arr_pros_dev[a].imp_revaluado_15   IS NULL THEN 
            LET arr_pros_dev[a].imp_revaluado_15   = 0
         END IF

         IF arr_pros_dev[a].f_transaccion      IS NULL THEN 
            LET arr_pros_dev[a].f_transaccion      = " "
         END IF                                  

         IF arr_pros_dev[a].f_factura          IS NULL THEN 
            LET arr_pros_dev[a].f_factura          = " "
         END IF

         LET a = a +1
      END FOREACH

      CALL arr_pros_dev.deleteElement(a)

   END IF
   END IF

      IF arr_pros_dev.getLength() >= 1 THEN

         SELECT ruta_envio
           INTO v_ruta_envio
           FROM seg_modulo
          WHERE modulo_cod = 'ocg'

         LET v_nom_arch = v_ruta_envio CLIPPED,"/det_pros_dev" CLIPPED,v_f_proceso USING "DDMMYYYY" CLIPPED,".dev"
         LET v_nom_arch1 = v_ruta_envio CLIPPED,"/det_pros_dev" CLIPPED,".dev"
         LET ch = base.Channel.create()
         CALL ch.openFile(v_nom_arch,"w" )
         CALL ch.setDelimiter("")

         FOR a=1 TO arr_pros_dev.getLength()

            LET v_f_trans       = arr_pros_dev[a].f_transaccion
            LET v_f_trans       = v_f_trans[5,6],"/",v_f_trans[7,8],"/",v_f_trans[1,4]

            SELECT precio_fondo
              INTO v_precio_fondo_today
              FROM glo_valor_fondo
             WHERE f_valuacion = v_f_trans
               AND fondo = 11;

            LET v_penalizacion      = (arr_pros_dev[a].imp_rev_f_proeceso - arr_pros_dev[a].importe_ap_pat)
            LET v_imp_devolucion    = (arr_pros_dev[a].importe_ap_pat + v_penalizacion)
            LET v_f_liberacion_gtia = arr_pros_dev[a].f_liberacion_gtia --USING "YYYYMMDD"
            LET v_f_pago            = arr_pros_dev[a].f_pago USING "YYYYDDMM"
            LET v_aiv_eq_1          = v_precio_fondo_today --(arr_pros_dev[a].importe_ap_pat/arr_pros_dev[a].aiv_ap_pat)
            LET v_aiv_eq_2          = v_precio_fondo_15   --(arr_pros_dev[a].importe_ap_pat/arr_pros_dev[a].imp_revaluado_15)
            LET v_paterno           = arr_pros_dev[a].paterno            CLIPPED
            LET v_materno           = arr_pros_dev[a].materno            CLIPPED
            LET v_nombre            = arr_pros_dev[a].nombre             CLIPPED

            LET v_nombre = v_nombre CLIPPED

            LET v_name = ''

            LET x = 1
            FOR x = 1 TO LENGTH(v_nombre)
               LET v_dato = v_nombre[x,x]
               IF v_dato = ' ' THEN
                  LET v_dato = '¿'
               END IF
               LET v_name = v_name,v_dato

               --DISPLAY "Nombre : ",v_name
            END FOR

            LET v_name = v_name CLIPPED

            LET v_dias = 0
            LET v_f_actual = TODAY
            --DISPLAY "f actual : ",v_f_actual
            LET v_f_desde  = arr_pros_dev[a].f_pago
            --DISPLAY "f desde : ",v_f_desde
            -- Se ejecuta la función que realiza cálculo de días hábiles
            LET v_s_qry = "EXECUTE FUNCTION fn_cuenta_habil(?,?)"

            PREPARE prp_exe_cuenta_habil  FROM v_s_qry
            EXECUTE prp_exe_cuenta_habil USING v_f_actual,
                                               v_f_desde
                                          INTO v_dias

            --DISPLAY "días trascurridos ",v_dias
            -- se agrega entidad financiera en detalle y se actualiza fecha para revaluación
            LET v_detalle = arr_pros_dev[a].cve_ent_financiera CLIPPED,"|",
                            arr_pros_dev[a].nss                CLIPPED,"|",
                            v_paterno                                 ,"¿",
                            v_materno                                 ,"¿",
                            v_name                                    ,"|",
                            arr_pros_dev[a].bimestre_ap_subsec CLIPPED,"|",
                            v_f_liberacion_gtia                CLIPPED,"|",
                            arr_pros_dev[a].importe_ap_pat     CLIPPED,"|",
                            arr_pros_dev[a].f_transaccion      CLIPPED,"|",
                            v_f_proceso                        USING "YYYYMMDD","|",
                            v_dias                                    ,"|",
                            --v_f_pago                         CLIPPED,"|",
                            arr_pros_dev[a].aiv_ap_pat         CLIPPED,"|",
                            v_aiv_eq_1                         CLIPPED,"|",
                            arr_pros_dev[a].imp_revaluado_15   CLIPPED,"|",
                            --v_aiv_eq_2                         CLIPPED,"|",
                            --arr_pros_dev[a].imp_rev_f_proeceso CLIPPED,"|",
                            v_penalizacion                     CLIPPED,"|",
                            v_imp_devolucion                   CLIPPED

            CALL ch.write([v_detalle])
         END FOR
         CALL ch.close()
         --    se crea comando que elimina espacios en blanco
         LET v_cmd = "sed 's/ //g' ",v_nom_arch," > ",v_nom_arch1
         RUN v_cmd

         --    se crea comando que elimina signos en nombre
         LET v_cmd = "sed 's/¿/ /g' ",v_nom_arch1," > ",v_nom_arch
         RUN v_cmd

          --    se crea comando para dejar archivos iguales
         LET v_cmd = "sed 's/ / /g' ",v_nom_arch," > ",v_nom_arch1
         RUN v_cmd

         LET v_cnt_prospecto = arr_pros_dev.getLength()

         --DISPLAY v_f_proceso
         --DISPLAY v_cnt_prospecto
         --DISPLAY TODAY
         --DISPLAY p_usuario
         
         INSERT INTO ocg_ctr_prospecto
              VALUES (v_f_proceso,
                      v_cnt_prospecto,
                      TODAY,
                      p_usuario)

         CALL fn_mensaje("Aviso","Archivo generado de forma correcta","info")
      ELSE
         CALL fn_mensaje("Aviso","No se encontraron registros para fecha solicitada","info")
      END IF
END FUNCTION

FUNCTION fn_crea_tmp()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

DROP TABLE tmp_ocg_devolucion_prospecto;

   WHENEVER ERROR STOP

   create table tmp_ocg_devolucion_prospecto(
   id_ocg_devolucion     decimal(9,0),
   id_referencia_dis     decimal(9,0),
   id_ocg_detalle        decimal(9,0),
   id_ocg_formalizacion  decimal(9,0),
   id_derechohabiente    decimal(9,0),
   nss                   CHAR(11),
   cve_ent_financiera    SMALLINT,
   num_ctr_int_ef        char(18),
   bimestre_ap_subsec    char(6),
   importe_ap_pat        decimal(18,6),
   aiv_ap_pat            decimal(18,6),
   imp_revaluado_15      decimal(18,6),
   aiv_revaluado_15      decimal(18,6),
   f_transaccion         DATE,
   f_factura             DATE,
   diagnostico           char(2),
   estado                SMALLINT,
   situacion             SMALLINT)

   DATABASE safre_viv

END FUNCTION