################################################################################
#Modulo            => OCG                                                      #
#Programa          => OCGP104                                                  #
#Objetivo          => Programa que realiza las validaciones del subproceso     #
#                     (5) liquidación                                          #
#Autor             => Héctor F. Jiménez Lara                                   #
#Fecha inicio      => 19 Noviembre 2015                                        #
################################################################################
DATABASE safre_viv

   DEFINE p_usuario             LIKE seg_usuario.usuario       -- nombre del usuario
   DEFINE v_opera_cod           LIKE cat_operacion.opera_cod   -- coódigo de la operacion de la etapa
   DEFINE p_v_arch_proceso      VARCHAR(100)                   -- nombre del archivo a integrar
   DEFINE v_s_qry               STRING
   DEFINE p_pid                 DECIMAL(9,0)
   DEFINE v_proceso_cod         LIKE cat_proceso.proceso_cod   -- código del proceso
   
MAIN

   DEFINE v_id_ocg_ctr_arch     LIKE ocg_ctr_archivo.id_ocg_ctr_archivo
   DEFINE v_programa_cod        LIKE seg_programa.programa_cod
   DEFINE v_sp_error            SMALLINT
   DEFINE r_b_valida            SMALLINT
   DEFINE v_subproceso          SMALLINT
   DEFINE v_cnt_aceptados       INTEGER
   DEFINE v_cnt_rechazados      INTEGER
   DEFINE v_sb5_aceptados       INTEGER 
   DEFINE v_sb5_rechazados      INTEGER 
   DEFINE v_estado              SMALLINT
   DEFINE a                     SMALLINT
   DEFINE v_detalle             STRING
   DEFINE ch                    base.Channel
   DEFINE v_nom_arch            STRING
   DEFINE v_ruta_envio          LIKE seg_modulo.ruta_envio
   DEFINE v_nom_arch1           STRING
   DEFINE v_penalizacion        DECIMAL(13,2)
   DEFINE v_cmd                 STRING

   DEFINE v_cant_aceptados    INTEGER
   DEFINE v_cant_rechazados   INTEGER
   DEFINE v_f_deposito        CHAR(8)
   DEFINE v_f_libera_garantia CHAR(8)
   DEFINE v_f_envio2          CHAR(8)
   DEFINE v_imp_devolucion    DECIMAL(13,2)
   DEFINE v_f_liberacion_gtia CHAR(8)
   DEFINE v_f_pago            CHAR(8)
   DEFINE v_paterno           CHAR(40)
   DEFINE v_materno           CHAR(40)
   DEFINE v_nombre            CHAR(40)
   DEFINE v_f_actual          DATE
   DEFINE v_f_desde           DATE
   DEFINE v_dias              INTEGER
   
   DEFINE v_aiv_eq_1          DECIMAL(13,5)
   DEFINE v_aiv_eq_2          DECIMAL(13,2)
   DEFINE v_f_transaccion     STRING
   DEFINE v_f_trans           CHAR(10)
   DEFINE v_cnt_pros          INTEGER
   
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
          aiv_ap_pat            decimal(18,2),
          importe_ap_pat        decimal(13,2),
          imp_rev_f_proeceso    decimal(18,2),
          imp_revaluado_15      decimal(13,5),
          f_transaccion         CHAR(8),
          f_factura             CHAR(10),
          f_liberacion_gtia     char(8)
   END RECORD

   DEFINE v_rec_datos RECORD
      nss                        CHAR(11),
      cve_ef                     CHAR(3),
      ctl_int_ef                 CHAR(18),
      bim_aportacion_subsecuente CHAR(6),
      imp_aportacion_devuelto    CHAR(15),
      causa_liquida              CHAR(1),
      f_deposito                 DATE,
      cred_convenidos            CHAR(1),
      f_libera_garantia          DATE,
      importe_garantia_devuelto  CHAR(15)
   END RECORD

   DEFINE v_tot_rch_sp5_val     INTEGER

   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario        = ARG_VAL(1)
   LET p_v_arch_proceso = ARG_VAL(2)
   LET p_pid            = ARG_VAL(3)

   LET v_proceso_cod     = 3907
   LET v_opera_cod       = 1
   LET v_programa_cod    = "OCGP04"
   LET v_id_ocg_ctr_arch = 0 
   LET p_v_arch_proceso  = p_v_arch_proceso CLIPPED     -- Se eliminan los espacios en blanco
   LET v_subproceso      = 1
   LET v_cnt_aceptados   = 0
   LET v_cnt_rechazados  = 0
   LET v_sb5_rechazados  = 0
   LET v_sb5_aceptados   = 0
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGP04.log")

   DISPLAY "=INICIA OCGP04="
   DISPLAY "   USUARIO    : ",p_usuario
   DISPLAY "   PID        : ",p_pid 

   -- Se obtiene el id del archivo 
   LET v_s_qry = " SELECT id_ocg_ctr_archivo
                     FROM ocg_ctr_archivo
                    WHERE nom_archivo = ? "

   PREPARE prp_obt_id_arch FROM v_s_qry
   EXECUTE prp_obt_id_arch INTO v_id_ocg_ctr_arch 
                          USING p_v_arch_proceso

   CALL fn_crea_tmp()

   SLEEP 45

   -- Se ejecuta la función que realiza el proceso
   LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_sp5_liquidacion(?)"

   PREPARE prp_exe_fn_proceso FROM v_s_qry
   EXECUTE prp_exe_fn_proceso USING v_id_ocg_ctr_arch 
                               INTO v_sp_error,
                                    v_cnt_aceptados,
                                    v_cnt_rechazados 

   -- Recupera registros rechazados
   SELECT COUNT(*)
      INTO v_sb5_rechazados
      FROM ocg_liquidacion li,
           ocg_detalle dt
     WHERE li.id_ocg_detalle = dt.id_ocg_detalle
       AND li.diagnostico = 02
       AND dt.id_ocg_ctr_archivo = v_id_ocg_ctr_arch
       AND dt.subproceso = 005
       AND dt.f_proceso = TODAY;

   -- Recupera registros aceptados
   SELECT COUNT(*)
      INTO v_sb5_aceptados
      FROM ocg_liquidacion li,
           ocg_detalle dt
     WHERE li.id_ocg_detalle = dt.id_ocg_detalle
       AND li.diagnostico = 01
       AND dt.id_ocg_ctr_archivo = v_id_ocg_ctr_arch
       AND dt.subproceso = 005
       AND dt.f_proceso = TODAY;

   --Rechazos sp5 en validación
   SELECT tot_sp5_rch_val
     INTO v_tot_rch_sp5_val
     FROM safre_tmp:tmp_ocg_cifras;

   DISPLAY "\n=> TOTAL ACEPTADOS:",v_sb5_aceptados             --v_cnt_aceptados
   DISPLAY "=> TOTAL RECHAZOS OPERATIVOS:",v_sb5_rechazados    --v_cnt_rechazados
   DISPLAY "=> TOTAL RECHAZOS VALIDACIÓN:",v_tot_rch_sp5_val

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF v_sp_error <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE LIQUIDACIÓN: ",v_sp_error

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_pid, v_proceso_cod, v_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      CALL fn_finaliza_procesos()

      EXIT PROGRAM
   END IF

   -- Se actualiza el estado del SP002 a Finalizado
   LET v_s_qry = " UPDATE ocg_ctr_proceso
                      SET fin_sp5 = 1 
                    WHERE id_ocg_ctr_archivo = ? "

   PREPARE prp_upd_edo FROM v_s_qry
   EXECUTE prp_upd_edo USING v_id_ocg_ctr_arch

   IF v_estado = 0 THEN

       UPDATE tmp_ctr_subproceso
                        SET sp005 = 1
                      WHERE id_ocg_ctr_arch = v_id_ocg_ctr_arch

   -- comentado para no generar prospectos de devolución por acuerdo temporal con usuario 13/12/2016
   --SELECT COUNT(*)
   --  INTO v_cnt_pros
   --  FROM safre_tmp:tmp_ocg_devolucion_prospecto p

   LET v_cnt_pros = 0

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

      PREPARE prp_dev FROM v_s_qry
      DECLARE cur_dev CURSOR FOR prp_dev

      LET a = 1
      FOREACH cur_dev INTO arr_pros_dev[a].id_ocg_ctr_transaccion,
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
{
         SELECT l.f_liberacion_gtia
           INTO arr_pros_dev[a].f_liberacion_gtia
           FROM ocg_liquidacion l
          WHERE l.id_ocg_formalizacion = arr_pros_dev[a].id_ocg_formalizacion
            AND l.id_derechohabiente   =  arr_pros_dev[a].id_derechohabiente
}
            DISPLAY "f_liberacion_gtia :",arr_pros_dev[a].f_liberacion_gtia

        
         LET v_f_trans = arr_pros_dev[a].f_transaccion
         LET v_f_transaccion = v_f_trans[5,6],"/",v_f_trans[7,8],"/",v_f_trans[1,4]
         LET v_f_trans = v_f_transaccion

         DISPLAY "periodo_pago  :",arr_pros_dev[a].bimestre_ap_subsec
         DISPLAY "f_transaccion :",v_f_trans

         SELECT UNIQUE f_pago
           INTO arr_pros_dev[a].f_pago
           FROM ocg_ctr_transaccion
          WHERE id_ocg_formalizacion = arr_pros_dev[a].id_ocg_formalizacion
            AND id_derechohabiente   = arr_pros_dev[a].id_derechohabiente
            AND periodo_pago         = arr_pros_dev[a].bimestre_ap_subsec
            AND f_transaccion        = v_f_trans


            DISPLAY "f_pago :",arr_pros_dev[a].f_pago
            DISPLAY "id_ocg_f",arr_pros_dev[a].id_ocg_formalizacion
            DISPLAY "dh",arr_pros_dev[a].id_derechohabiente
            DISPLAY "periodo",arr_pros_dev[a].bimestre_ap_subsec
            DISPLAY "f transacción",v_f_trans

   
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

        --DISPLAY "arreglo : ",arr_pros_dev[a].*
         LET a = a +1
      END FOREACH

      CALL arr_pros_dev.deleteElement(a)
   END IF

      IF arr_pros_dev.getLength() >= 1 THEN

         SELECT ruta_envio
           INTO v_ruta_envio
           FROM seg_modulo
          WHERE modulo_cod = 'ocg'

         LET v_nom_arch = v_ruta_envio CLIPPED,"/det_pros_dev" CLIPPED,TODAY USING "DDMMYYYY" CLIPPED,".dev"
         LET v_nom_arch1 = v_ruta_envio CLIPPED,"/det_pros_dev" CLIPPED,".dev"
         LET ch = base.Channel.create()
         CALL ch.openFile(v_nom_arch,"w" )
         CALL ch.setDelimiter("")

         FOR a=1 TO arr_pros_dev.getLength()

            LET v_penalizacion      = (arr_pros_dev[a].imp_rev_f_proeceso - arr_pros_dev[a].importe_ap_pat)
            LET v_imp_devolucion    = (arr_pros_dev[a].importe_ap_pat + v_penalizacion)
            LET v_f_liberacion_gtia = arr_pros_dev[a].f_liberacion_gtia --USING "YYYYMMDD"
            LET v_f_pago            = arr_pros_dev[a].f_pago USING "YYYYDDMM"
            LET v_aiv_eq_1   = (arr_pros_dev[a].importe_ap_pat/arr_pros_dev[a].aiv_ap_pat)
            LET v_aiv_eq_2   = (arr_pros_dev[a].importe_ap_pat/arr_pros_dev[a].imp_revaluado_15)

            LET v_paterno = arr_pros_dev[a].paterno            CLIPPED
            LET v_materno = arr_pros_dev[a].materno            CLIPPED
            LET v_nombre  = arr_pros_dev[a].nombre             CLIPPED

            LET v_dias = 0
            LET v_f_actual = TODAY
            LET v_f_desde  = arr_pros_dev[a].f_pago
            -- Se ejecuta la función que realiza cálculo de días hábiles
            LET v_s_qry = "EXECUTE FUNCTION fn_cuenta_habil(?,?)"

            PREPARE prp_exe_cuenta_habil  FROM v_s_qry
            EXECUTE prp_exe_cuenta_habil USING v_f_actual,
                                               v_f_desde
                                          INTO v_dias

            LET v_detalle = arr_pros_dev[a].nss                CLIPPED,"|",
                            v_paterno                                 ,"¿",
                            v_materno                                 ,"¿",
                            v_nombre                                  ,"|",
                            arr_pros_dev[a].bimestre_ap_subsec CLIPPED,"|",
                            v_f_liberacion_gtia                CLIPPED,"|",
                            arr_pros_dev[a].importe_ap_pat     CLIPPED,"|",
                            arr_pros_dev[a].f_transaccion      CLIPPED,"|",
                            TODAY USING "YYYYMMDD","|",
                            v_dias,"|",
                            --v_f_pago                           CLIPPED,"|",
                            arr_pros_dev[a].aiv_ap_pat         CLIPPED,"|",
                            v_aiv_eq_1                         CLIPPED,"|",
                            arr_pros_dev[a].imp_revaluado_15   CLIPPED,"|",
                            v_aiv_eq_2                         CLIPPED,"|",
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

      END IF

      CALL fn_actualiza_opera_fin(p_pid,
                                  v_proceso_cod,
                                  v_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(p_pid, v_proceso_cod, v_opera_cod)  RETURNING v_estado
   END IF

   DISPLAY "=FIN="
END MAIN


#Objetivo: Función que finaliza las operaciones del proceso
FUNCTION fn_finaliza_procesos()
   DEFINE v_i_proceso_cod  LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_i_opera_cod    LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE v_s_qryTxt       STRING -- se asigna una sentencia sql a ejecutar
   DEFINE v_dt_tiempo      DATETIME YEAR TO SECOND -- variable con fecha y hora
   DEFINE r_b_valida       SMALLINT -- status de registro de las funciones de actualización

   -- se asigna la fecha y hora actual
   LET v_dt_tiempo = CURRENT

   -- se actializa la operación como erronea
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET fecha_fin   = '",v_dt_tiempo,"',\n",
                    "        estado_cod  = 3\n",
                    "  WHERE pid         = ",p_pid,"\n",
                    "    AND proceso_cod = ",v_proceso_cod,"\n",
                    "    AND opera_cod   = ",v_opera_cod

   PREPARE prp_act_error_opera FROM v_s_qryTxt
   EXECUTE prp_act_error_opera

   -- se actualiza el proceso como erronea
   LET v_s_qryTxt = " UPDATE bat_ctr_proceso\n",
                    "    SET fecha_fin   = TODAY,\n",
                    "        estado_cod  = 3\n",
                    "  WHERE pid         = ",p_pid,"\n",
                    "    AND proceso_cod = ",v_proceso_cod

   PREPARE prp_act_error_proceso FROM v_s_qryTxt
   EXECUTE prp_act_error_proceso

END FUNCTION

FUNCTION fn_crea_tmp()
 DATABASE safre_tmp

drop table if exists tmp_ocg_devolucion_prospecto;

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
