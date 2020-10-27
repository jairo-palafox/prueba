###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGE02                                                  #
#Objetivo          => Programa Integrador que funciona como lanzador          #
#                     para realizar las integraciones de todos                #
#                     los subrprocesos de otorgamiento de crédito 43 bis      #
#Autor             => Héctor F. Jiménez Lara                                  #
#Fecha inicio      => 08 Octube 2015                                          #
###############################################################################

DATABASE safre_viv
   DEFINE v_s_cmd             STRING 
   DEFINE v_s_qry             STRING 
   DEFINE v_estado            SMALLINT
   DEFINE p_v_usuario         LIKE seg_usuario.usuario
   DEFINE v_id_ocg_ctr_arch   DECIMAL(9,0)

MAIN
   DEFINE p_pid               DECIMAL(9,0)
   
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_d_folio           LIKE glo_folio.folio
   DEFINE v_nomArch_proc      LIKE glo_ctr_archivo.nombre_archivo
   DEFINE r_b_valida          SMALLINT 
   DEFINE v_c_ruta_bin        LIKE seg_modulo.ruta_bin        -- ruta del bin del módulo
   DEFINE v_c_ruta_rescate    LIKE seg_modulo.ruta_rescate    -- ruta rescate del módulo
   DEFINE v_c_ruta_listados   LIKE seg_modulo.ruta_listados   -- ruta listados del módulo
   DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados   -- ruta listados de bat

   DEFINE r_total_cifras RECORD
      tot_reg_arh  INTEGER, 
      tot_sp1_arh  INTEGER,
      tot_sp2_arh  INTEGER,
      tot_sp3_arh  INTEGER,
      tot_sp5_arh  INTEGER 
   END RECORD 
 
   DEFINE v_v_nom_reporte     STRING
   DEFINE v_c_programa_cod    LIKE seg_programa.programa_cod
   DEFINE v_existe_error      BOOLEAN
   DEFINE v_arch              STRING
   DEFINE v_p_pid             STRING

   DEFINE m                   SMALLINT
   DEFINE v_cnt_sub           SMALLINT
   
   LET p_v_usuario    = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET v_d_folio      = ARG_VAL(5)
   LET v_nomArch_proc = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".OCGE02.log")

   DISPLAY "=INICIA OCGE02="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_pid

   -- se obtienen las rutas de control del modulo
   LET v_s_qry = " SELECT ruta_bin,\n         ",
                 "        ruta_rescate,\n     ",
                 "        ruta_listados\n     ",
                 "    FROM seg_modulo\n       ",
                 "   WHERE modulo_cod = 'ocg' "

   PREPARE prp_slc_rutas_mod FROM v_s_qry
   EXECUTE prp_slc_rutas_mod INTO v_c_ruta_bin,
                                  v_c_ruta_rescate,
                                  v_c_ruta_listados

   sleep 40

   LET v_s_qry =  " SELECT ruta_listados\n",
                  "   FROM seg_modulo\n",
                  "  WHERE modulo_cod = 'bat'"

   PREPARE prp_cons_r_lst_bat FROM v_s_qry
   EXECUTE prp_cons_r_lst_bat INTO v_c_ruta_list_bat

   -- Se obtiene el id del archivo
--   LET v_s_qry = " 
   SELECT id_ocg_ctr_archivo
     INTO v_id_ocg_ctr_arch
     FROM ocg_ctr_archivo
     WHERE nom_archivo = v_nomArch_proc
--? "

--   PREPARE prp_cons_id_arch FROM v_s_qry
  -- EXECUTE prp_cons_id_arch INTO v_id_ocg_ctr_arch
    --                       USING v_nomArch_proc

   DISPLAY "Nom Archivo :",v_nomArch_proc

   DISPLAY "ID archivo : ",v_id_ocg_ctr_arch

   -- Obtiene cifras control
   LET v_s_qry = "SELECT tot_reg_archivo,
                         tot_sp1_arh,
                         tot_sp2_arh,
                         tot_sp3_arh,
                         tot_sp5_arh
                    FROM safre_tmp:tmp_ocg_cifras;"

   PREPARE prp_cifras_arh FROM v_s_qry
   EXECUTE prp_cifras_arh INTO r_total_cifras.tot_reg_arh,
                                r_total_cifras.tot_sp1_arh,
                                r_total_cifras.tot_sp2_arh,
                                r_total_cifras.tot_sp3_arh,
                                r_total_cifras.tot_sp5_arh
                            
   DISPLAY ""   
   DISPLAY "TOTAL REGISTROS EN ARCHIVO:",r_total_cifras.tot_reg_arh
   
   DISPLAY "\n=> TOTAL REGISTROS SUBPROCESO 1:",r_total_cifras.tot_sp1_arh
   DISPLAY "=> TOTAL REGISTROS SUBPROCESO 2:",r_total_cifras.tot_sp2_arh
   DISPLAY "=> TOTAL REGISTROS SUBPROCESO 3:",r_total_cifras.tot_sp3_arh
   DISPLAY "=> TOTAL REGISTROS SUBPROCESO 5:",r_total_cifras.tot_sp5_arh
 
   DISPLAY "\nSE LANZA EL PRIMER SUBPROCESO (SOLICITUD DE TRÁMITE)"
   DISPLAY "SE LANZA EL SEGUNDO SUBPROCESO (FORMALIZACIÓN)"
   DISPLAY "SE LANZA EL TERCER SUBPROCESO (SOLICITUD USO GARANTÍA)"
   DISPLAY "SE LANZA EL QUINTO SUBPROCESO (LIQUIDACIÓN)\n"

   LET v_p_pid = p_pid
   LET v_p_pid = v_p_pid.trim()
   LET v_arch = v_nomArch_proc CLIPPED,"_",TODAY USING "ddmmyyyy","_",v_p_pid
   LET v_arch = v_arch CLIPPED

-- se comenta para no cambiar nombre de archivo en automático

{
  LET v_s_qry = "UPDATE glo_ctr_archivo
                     SET estado         = 2,
                         nombre_archivo = '",v_arch,"'
                   WHERE nombre_archivo = ? 
                     AND proceso_cod    = ?"
}
--se deja query para no cambiar nombre de archivo en automático

   LET v_s_qry = "UPDATE glo_ctr_archivo
                     SET estado         = 2
                   WHERE nombre_archivo = ? 
                     AND proceso_cod    = ?"

   PREPARE prp_upd_edo_arch FROM v_s_qry
   EXECUTE prp_upd_edo_arch USING v_nomArch_proc,
                                  p_proceso_cod

--***********************comentado para no cambiar nombre de archivo
{
   LET v_s_qry = "UPDATE ocg_ctr_archivo
                     SET nom_archivo = '",v_arch,"'
                   WHERE id_ocg_ctr_archivo = ?"

   --DISPLAY v_s_qry
   --DISPLAY v_nomArch_proc
   PREPARE prp_upd_arch FROM v_s_qry
   EXECUTE prp_upd_arch USING v_id_ocg_ctr_arch
}
--*****************************************************************************
   # EN CASO DE ERROR SE LLAMA A LA FUNCION ERROR OPERA 
   -- ocurrió un error y se marca como rechazado la operación
   --LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
     
   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
   END IF
   
   CALL fn_crea_tmp()
   CALL fn_lanza_sp1_tramite(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   CALL fn_lanza_sp2_formalizacion(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   CALL fn_lanza_sp3_ug(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   CALL fn_lanza_sp5_liqudacion(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)

   LET m = 1
   --DISPLAY m
   FOR m = 1 TO 480
      --LET m = m+1

      --DISPLAY "contador : ",m

      SELECT count(*)
        INTO v_cnt_sub
        FROM tmp_ctr_subproceso
       WHERE sp001 = 1
         AND sp002 = 1
         AND sp003 = 1
         AND sp005 = 1

      --DISPLAY "v_cnt_sub : ",v_cnt_sub

      IF v_cnt_sub = 1 THEN
         CALL fn_lanza_arch_salida()
         EXIT FOR
      ELSE
         SLEEP 15
         CONTINUE FOR
      END IF
   END FOR

   DISPLAY " GENERA REPORTE"

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_proceso_cod , p_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-",v_c_programa_cod CLIPPED,"-", p_pid USING "&&&&&", "-", p_proceso_cod USING "&&&&&", "-", p_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt(v_c_ruta_listados,v_v_nom_reporte,r_total_cifras.*,v_nomArch_proc) RETURNING v_existe_error

   
   DISPLAY "=FIN="
END MAIN

FUNCTION f_genera_rpt(p_c_ruta_listados, p_v_nom_reporte,p_rec_totales,p_nomArch_proc)
   DEFINE p_c_ruta_listados       LIKE seg_modulo.ruta_listados  -- ruta listados cta
   DEFINE p_v_nom_reporte         VARCHAR(80)  -- nombre del reporte
   DEFINE v_manejador_rpt         OM.SaxDocumentHandler  # Contenedor de Documentos para el reporte
   DEFINE v_b_existe_err          BOOLEAN -- indica si ocurrió error durante la generación del reporte
   DEFINE v_tot_viv97             DECIMAL(15,2)
   DEFINE v_tot_rech_viv97        DECIMAL(15,2)
   DEFINE p_rec_totales    RECORD
      v_cnt_tot               INTEGER, 
      v_cnt_tot_sp1           INTEGER,
      v_cnt_tot_sp2           INTEGER,
      v_cnt_tot_sp3           INTEGER,
      v_cnt_tot_sp5           INTEGER 
   END RECORD 
   DEFINE p_nomArch_proc      LIKE glo_ctr_archivo.nombre_archivo


   -- se inicializan variables
   LET v_b_existe_err   = FALSE -- se asume que no ocurrirá error
   LET v_tot_viv97      = 0
   LET v_tot_rech_viv97 = 0

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("OCGE021.4rp") THEN

      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(p_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "ERROR: No fue posible abrir plantilla del reporte"
      -- se indica que ha ocurrido un error y no continua
      LET v_b_existe_err = TRUE

      RETURN v_b_existe_err
   END IF

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_cifras_ctl TO XML HANDLER v_manejador_rpt

   -- salida del reporte
   OUTPUT TO REPORT reporte_cifras_ctl(p_rec_totales.*,p_nomArch_proc)

   -- finaliza el reporte
   FINISH REPORT reporte_cifras_ctl

   RETURN v_b_existe_err
END FUNCTION

REPORT reporte_cifras_ctl(p_rec_totales,p_nomArch_proc)
   DEFINE p_nomArch_proc      LIKE glo_ctr_archivo.nombre_archivo
   DEFINE p_rec_totales    RECORD
      v_cnt_tot               INTEGER, 
      v_cnt_tot_sp1           INTEGER,
      v_cnt_tot_sp2           INTEGER,
      v_cnt_tot_sp3           INTEGER,
      v_cnt_tot_sp5           INTEGER 
   END RECORD
   DEFINE v_fecha_reporte      DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY 
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX p_v_usuario
      PRINTX p_nomArch_proc
      PRINTX p_rec_totales.v_cnt_tot
      PRINTX p_rec_totales.v_cnt_tot_sp1
      PRINTX p_rec_totales.v_cnt_tot_sp2
      PRINTX p_rec_totales.v_cnt_tot_sp3
      PRINTX p_rec_totales.v_cnt_tot_sp5

END REPORT

-- Función que lanza el SP001
FUNCTION fn_lanza_sp1_tramite(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   DEFINE p_proceso_cod     INTEGER    
   DEFINE p_opera_cod       INTEGER 
   DEFINE p_v_usuario       CHAR (20)
   DEFINE r_b_valida        SMALLINT
   DEFINE v_d_folio         LIKE glo_folio.folio
   DEFINE v_nomArch_proc    LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_pid             DECIMAL(9,0)
   DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados
   
   LET p_proceso_cod = 3902
   LET p_opera_cod   = 1

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   --Obtiene ruta listados batch
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   LET v_d_folio = fn_genera_folio(p_proceso_cod, p_opera_cod, p_v_usuario)

   CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_v_usuario) RETURNING v_pid

   CALL fn_valida_operacion( v_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN  
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operacion"
      LET r_b_valida = fn_error_opera(v_pid, p_proceso_cod, p_opera_cod)
   ELSE
      CALL fn_inicializa_proceso(v_pid,p_proceso_cod,p_opera_cod,
                                 "","OCGP01","",p_v_usuario) RETURNING r_b_valida

      LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               v_d_folio,
                                               "OCGP01",
                                               v_nomArch_proc,
                                               p_v_usuario )

      LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,"/OCGP01 ",
                                          p_v_usuario, " ",
                                          v_nomArch_proc," ",
                                          v_pid," 1> ",
                                          v_ruta_listados CLIPPED,
                                          "/nohup:",v_pid USING "&&&&&",":",
                                          p_proceso_cod USING "&&&&&",":",
                                          p_opera_cod USING "&&&&&",
                                          " 2>&1 &"
      DISPLAY "comando en OCGE02 :",v_s_cmd
      RUN v_s_cmd
   END IF
{
   LET v_estado = 0
   
   IF v_estado = 0 THEN

      CALL fn_actualiza_opera_fin(v_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF
}
END FUNCTION

-- Función que lanza al SP002 Formalización
FUNCTION fn_lanza_sp2_formalizacion(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   DEFINE p_proceso_cod     INTEGER    
   DEFINE p_opera_cod       INTEGER 
   DEFINE p_v_usuario       CHAR (20)
   DEFINE r_b_valida        SMALLINT
   DEFINE v_d_folio         LIKE glo_folio.folio
   DEFINE v_nomArch_proc    LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_pid             DECIMAL(9,0)
   DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados
   
   LET p_proceso_cod = 3903
   LET p_opera_cod   = 1

   -- Se obtiene la ruta de los binarios
   LET v_s_qry = " SELECT ruta_bin
                     FROM seg_modulo
                    WHERE modulo_cod = 'ocg' "

   PREPARE prp_r_exe FROM v_s_qry
   EXECUTE prp_r_exe INTO v_ruta_ejecutable

   --Obtiene ruta listados batch
   LET v_s_qry = " SELECT ruta_listados
                     FROM seg_modulo
                    WHERE modulo_cod = 'bat' "

   PREPARE prp_r_lst FROM v_s_qry
   EXECUTE prp_r_lst INTO v_ruta_listados 

   -- Se genera el folio
   LET v_d_folio = fn_genera_folio(p_proceso_cod, p_opera_cod, p_v_usuario)

   -- Se genera el PId
   CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_v_usuario) RETURNING v_pid

   CALL fn_valida_operacion( v_pid,p_proceso_cod,p_opera_cod)   RETURNING r_b_valida

   -- se verifica si la operación en proceso es valida
   IF r_b_valida <> 0 THEN  
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operación"
   ELSE
      CALL fn_inicializa_proceso(v_pid,p_proceso_cod,p_opera_cod,
                                 "","OCGP02","",p_v_usuario) RETURNING r_b_valida

      LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               v_d_folio,
                                               "OCGP02",
                                               v_nomArch_proc,
                                               p_v_usuario )

      LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,"/OCGP02 ",
                                          p_v_usuario, " ",
                                          v_nomArch_proc,
                                          v_pid," 1> ",
                                          v_ruta_listados CLIPPED,
                                          "/nohup:",v_pid USING "&&&&&",":",
                                          p_proceso_cod USING "&&&&&",":",
                                          p_opera_cod USING "&&&&&",
                                          " 2>&1 &"
      RUN v_s_cmd
   END IF
{
   LET v_estado = 0
   
   IF v_estado = 0 THEN

      CALL fn_actualiza_opera_fin(v_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF
}
END FUNCTION

-- Función que lanza al SP003 Solicitud de UG
FUNCTION fn_lanza_sp3_ug(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   DEFINE p_proceso_cod     INTEGER    
   DEFINE p_opera_cod       INTEGER 
   DEFINE p_v_usuario       CHAR (20)
   DEFINE r_b_valida        SMALLINT
   DEFINE v_d_folio         LIKE glo_folio.folio
   DEFINE v_nomArch_proc    LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_pid             DECIMAL(9,0)
   DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados
   
   LET p_proceso_cod = 3906
   LET p_opera_cod   = 1

   -- Se obtiene la ruta de los binarios
   LET v_s_qry = " SELECT ruta_bin
                     FROM seg_modulo
                    WHERE modulo_cod = 'ocg' "

   PREPARE prp_r_exe_bin FROM v_s_qry
   EXECUTE prp_r_exe_bin INTO v_ruta_ejecutable

   --Obtiene ruta listados batch
   LET v_s_qry = " SELECT ruta_listados
                     FROM seg_modulo
                    WHERE modulo_cod = 'bat' "

   PREPARE prp_r_exe_lst FROM v_s_qry
   EXECUTE prp_r_exe_lst INTO v_ruta_listados 

   -- Se genera el folio
   LET v_d_folio = fn_genera_folio(p_proceso_cod, p_opera_cod, p_v_usuario)

   -- Se genera el PId
   CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_v_usuario) RETURNING v_pid

   CALL fn_valida_operacion( v_pid,p_proceso_cod,p_opera_cod)   RETURNING r_b_valida

   -- se verifica si la operación en proceso es valida
   IF r_b_valida <> 0 THEN  
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operacion"
   ELSE
      CALL fn_inicializa_proceso( v_pid,
                                  p_proceso_cod,
                                  p_opera_cod,
                                  "",
                                  "OCGP03",
                                  "",
                                  p_v_usuario) RETURNING r_b_valida

      LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               v_d_folio,
                                               "OCGP03",
                                               v_nomArch_proc,
                                               p_v_usuario )

      LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,"/OCGP03 ",
                                          p_v_usuario, " ",
                                          v_nomArch_proc,
                                          v_pid," 1> ",
                                          v_ruta_listados CLIPPED,
                                          "/nohup:",v_pid USING "&&&&&",":",
                                          p_proceso_cod USING "&&&&&",":",
                                          p_opera_cod USING "&&&&&",
                                          " 2>&1 &"
      RUN v_s_cmd
   END IF
{
   LET v_estado = 0
   
   IF v_estado = 0 THEN
      CALL fn_actualiza_opera_fin(v_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF
}
END FUNCTION

-- Función que lanza al SP005 Liquidación
FUNCTION fn_lanza_sp5_liqudacion(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   DEFINE p_proceso_cod     INTEGER    
   DEFINE p_opera_cod       INTEGER 
   DEFINE p_v_usuario       CHAR(20)
   DEFINE r_b_valida        SMALLINT
   DEFINE v_d_folio         LIKE glo_folio.folio
   DEFINE v_nomArch_proc    LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_pid             DECIMAL(9,0)
   DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados
   
   LET p_proceso_cod = 3907
   LET p_opera_cod   = 1

   -- Se obtiene la ruta de los binarios
   LET v_s_qry = " SELECT ruta_bin
                     FROM seg_modulo
                    WHERE modulo_cod = 'ocg' "

   PREPARE prp_r_exe_bin5 FROM v_s_qry
   EXECUTE prp_r_exe_bin5 INTO v_ruta_ejecutable

   --Obtiene ruta listados batch
   LET v_s_qry = " SELECT ruta_listados
                     FROM seg_modulo
                    WHERE modulo_cod = 'bat' "

   PREPARE prp_r_exe_lst5 FROM v_s_qry
   EXECUTE prp_r_exe_lst5 INTO v_ruta_listados 
   -- Se genera el folio
   LET v_d_folio = fn_genera_folio(p_proceso_cod, p_opera_cod, p_v_usuario)

   -- Se genera el PId
   CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_v_usuario) RETURNING v_pid

   CALL fn_valida_operacion( v_pid,p_proceso_cod,p_opera_cod)   RETURNING r_b_valida

   -- se verifica si la operación en proceso es valida
   IF r_b_valida <> 0 THEN  
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operacion"
   ELSE
      CALL fn_inicializa_proceso( v_pid,
                                  p_proceso_cod,
                                  p_opera_cod,
                                  "",
                                  "OCGP04",
                                  "",
                                  p_v_usuario) RETURNING r_b_valida

      LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               v_d_folio,
                                               "OCGP04",
                                               v_nomArch_proc,
                                               p_v_usuario )

      LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,"/OCGP04 ",
                                          p_v_usuario, " ",
                                          v_nomArch_proc,
                                          v_pid," 1> ",
                                          v_ruta_listados CLIPPED,
                                          "/nohup:",v_pid USING "&&&&&",":",
                                          p_proceso_cod USING "&&&&&",":",
                                          p_opera_cod USING "&&&&&",
                                          " 2>&1 &"
      RUN v_s_cmd
   END IF
{
   LET v_estado = 0
   
   IF v_estado = 0 THEN
      CALL fn_actualiza_opera_fin(v_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF
}
END FUNCTION

FUNCTION fn_lanza_arch_salida()

   DEFINE v_f_generacion      DATE
   DEFINE v_f_publicacion     DATE
   DEFINE r_b_valida          SMALLINT
   DEFINE v_comando           STRING
   DEFINE v_estado            SMALLINT
   DEFINE v_arch              STRING
   DEFINE v_valida_arch       CHAR(40)
   DEFINE bnd_arch            SMALLINT
   DEFINE v_qry               STRING
   DEFINE a                   INTEGER
   DEFINE g_usuario           CHAR(40)
   DEFINE g_proceso_cod       SMALLINT
   DEFINE g_opera_cod         SMALLINT
   DEFINE v_pid              DECIMAL(9,0)
   DEFINE v_s_comando        STRING
   DEFINE v_ruta_ejecutable  LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados

   LET g_usuario       = p_v_usuario
   LET g_proceso_cod   = 3909   -- numero de proceso correspondiente   
   LET g_opera_cod     = 1       -- numero de operacion correspondiente 

   LET v_arch = "Art43bis",TODAY USING "DDMMYYYY",".Sal"
   LET bnd_arch = 0

   IF bnd_arch = 0 THEN

      LET v_f_generacion    = TODAY

      LET v_comando = "EXECUTE FUNCTION fn_cal_habil_siguiente(?)"

      PREPARE prp_fn_habil FROM v_comando

      EXECUTE prp_fn_habil USING v_f_generacion INTO v_f_publicacion

      ---- se invoca la funcion que valida la operacion
      CALL fn_valida_operacion( 0,g_proceso_cod,g_opera_cod)   RETURNING r_b_valida
      -- se verifica si la operacion en proceso es valida
      IF r_b_valida <> 0 THEN  
      -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(r_b_valida)
         DISPLAY "ERROR en fn_valida_operacion"
      ELSE 
      -- se llama funcion que genera el archivo
         SELECT ruta_bin
           INTO v_ruta_ejecutable
           FROM seg_modulo
          WHERE modulo_cod = 'ocg'

   --Obtiene ruta listados batch
         SELECT ruta_listados
           INTO v_ruta_listados
           FROM seg_modulo
          WHERE modulo_cod = 'bat'

         CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid

         CALL fn_inicializa_proceso(v_pid,g_proceso_cod,
                                    g_opera_cod,
                                    "","OCGS01","",
                                    g_usuario)  RETURNING r_b_valida

         CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                                     g_opera_cod,"",
                                     "OCGS01","",
                                     g_usuario)  RETURNING r_b_valida

         LET v_s_comando = "nohup fglrun ",
                           v_ruta_ejecutable CLIPPED,
                           "/OCGS01 ",
                           g_usuario," ",
                           v_pid," ",
                           g_proceso_cod," ",
                           g_opera_cod," '",
                           v_f_generacion,"'" ,
                           " '", v_f_publicacion, "'",
                           " ' '  1>", v_ruta_listados CLIPPED ,
                           "/nohup:",v_pid USING "&&&&&",":",
                           g_proceso_cod USING "&&&&&",":",
                           g_opera_cod USING "&&&&&" ," 2>&1 &"

         RUN v_s_comando

         --DISPLAY "v_s_comando", v_s_comando  

         --LET v_s_comando = "Se ejecutó la generación de archivo de salida 43BIS"," ",
           --                "Verificar en el monitor de proceso la ejecución el PID ", v_pid USING "<<<<<<<<<"
         --CALL fn_mensaje("Cuentas",v_s_comando,"information")

         DISPLAY "Archivo generado de forma correcta"
         DISPLAY ""
         DISPLAY "Verificar archivo en /safreviv_int/ocg/envio/",v_arch

         LET v_estado = 0

         IF v_estado = 0 THEN

            CALL fn_actualiza_opera_fin(v_pid,
                                        g_proceso_cod,
                                        g_opera_cod)
                              RETURNING v_estado

         ELSE
         --Si ocurrio un error se actualiza el estatus como erroneo
            CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
         END IF
      END IF
   END IF
END FUNCTION

FUNCTION fn_crea_tmp()

   DROP TABLE IF EXISTS tmp_ctr_subproceso

   CREATE TABLE tmp_ctr_subproceso
                (id_ocg_ctr_arch DECIMAL(9,0),
                 sp001             SMALLINT,
                 sp002             SMALLINT,
                 sp003             SMALLINT,
                 sp005             SMALLINT,
                 usuario           CHAR(40)
)

   INSERT INTO tmp_ctr_subproceso
       VALUES (v_id_ocg_ctr_arch,
               0,
               0,
               0,
               0,
               p_v_usuario )

END FUNCTION
