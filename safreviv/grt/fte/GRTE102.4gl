###############################################################################
#Modulo            => GRT                                                     #
#Programa          => GRTE102                                                 #
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

MAIN
   DEFINE p_pid               DECIMAL(9,0)
   DEFINE v_id_grt_ctr_arch   DECIMAL(9,0)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_d_folio           LIKE glo_folio.folio
   DEFINE v_nomArch_proc      LIKE glo_ctr_archivo.nombre_archivo
   DEFINE r_b_valida          SMALLINT 
   DEFINE v_c_ruta_bin        LIKE seg_modulo.ruta_bin        -- ruta del bin del módulo
   DEFINE v_c_ruta_rescate    LIKE seg_modulo.ruta_rescate    -- ruta rescate del módulo
   DEFINE v_c_ruta_listados   LIKE seg_modulo.ruta_listados   -- ruta listados del módulo
   DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados   -- ruta listados de bat
   DEFINE v_rec_totales    RECORD
      v_cnt_tot               INTEGER, 
      v_cnt_tot_sp1           INTEGER,
      v_cnt_tot_sp2           INTEGER,
      v_cnt_tot_sp3           INTEGER,
      v_cnt_tot_sp5           INTEGER 
   END RECORD 
   DEFINE v_cnt_tot           INTEGER 
   DEFINE v_cnt_tot_sp1       INTEGER
   DEFINE v_cnt_tot_sp2       INTEGER 
   DEFINE v_cnt_tot_sp3       INTEGER 
   DEFINE v_cnt_tot_sp5       INTEGER  
   DEFINE v_v_nom_reporte     STRING
   DEFINE v_c_programa_cod    LIKE seg_programa.programa_cod
   DEFINE v_existe_error      BOOLEAN

   LET p_v_usuario    = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET v_d_folio      = ARG_VAL(5)
   LET v_nomArch_proc = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTE102.log")

   DISPLAY "=INICIA GRTE102="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_pid

   -- se obtienen las rutas de control del modulo
   LET v_s_qry = " SELECT ruta_bin,\n         ",
                 "        ruta_rescate,\n     ",
                 "        ruta_listados\n     ",
                 "    FROM seg_modulo\n       ",
                 "   WHERE modulo_cod = 'grt' "

   PREPARE prp_slc_rutas_mod FROM v_s_qry
   EXECUTE prp_slc_rutas_mod INTO v_c_ruta_bin,
                                  v_c_ruta_rescate,
                                  v_c_ruta_listados

   LET v_s_qry =  " SELECT ruta_listados\n",
                  "   FROM seg_modulo\n",
                  "  WHERE modulo_cod = 'bat'"

   PREPARE prp_cons_r_lst_bat FROM v_s_qry
   EXECUTE prp_cons_r_lst_bat INTO v_c_ruta_list_bat

   -- Se obtiene el id del archivo
   LET v_s_qry = " SELECT id_grt_ctr_archivo
                     FROM grt_ctr_archivo
                     WHERE nom_archivo = ? "

   PREPARE prp_cons_id_arch FROM v_s_qry
   EXECUTE prp_cons_id_arch INTO v_id_grt_ctr_arch
                           USING v_nomArch_proc

   -- Se obtienen el total de registros
   LET v_s_qry = " SELECT COUNT(*)
                     FROM safre_tmp:tmp_rec_det_grt43 "

   PREPARE prp_cnt_tot FROM v_s_qry
   EXECUTE prp_cnt_tot INTO v_rec_totales.v_cnt_tot

   -- Se obtienen el total de registros
   LET v_s_qry = " SELECT COUNT(*)
                     FROM safre_tmp:tmp_rec_det_grt43
                    WHERE subproceso = 1 "

   PREPARE prp_cnt_sp1 FROM v_s_qry
   EXECUTE prp_cnt_sp1 INTO v_rec_totales.v_cnt_tot_sp1

   -- Se obtienen el total de registros
   LET v_s_qry = " SELECT COUNT(*)
                     FROM safre_tmp:tmp_rec_det_grt43
                    WHERE subproceso = 2 "

   PREPARE prp_cnt_sp2 FROM v_s_qry
   EXECUTE prp_cnt_sp2 INTO v_rec_totales.v_cnt_tot_sp2

   -- Se obtienen el total de registros
   LET v_s_qry = " SELECT COUNT(*)
                     FROM safre_tmp:tmp_rec_det_grt43
                    WHERE subproceso = 3 "

   PREPARE prp_cnt_sp3 FROM v_s_qry
   EXECUTE prp_cnt_sp3 INTO v_rec_totales.v_cnt_tot_sp3

   -- Se obtienen el total de registros
   LET v_s_qry = " SELECT COUNT(*)
                     FROM safre_tmp:tmp_rec_det_grt43
                    WHERE subproceso = 5 "

   PREPARE prp_cnt_sp5 FROM v_s_qry
   EXECUTE prp_cnt_sp5 INTO v_rec_totales.v_cnt_tot_sp5

   DISPLAY "TOTAL REGISTROS EN ARCHIVO  : ",v_rec_totales.v_cnt_tot
   DISPLAY "\nTOTAL REGISTROS SUBPROCESO 1: ",v_rec_totales.v_cnt_tot_sp1
   DISPLAY "TOTAL REGISTROS SUBPROCESO 2: ",v_rec_totales.v_cnt_tot_sp2
   DISPLAY "TOTAL REGISTROS SUBPROCESO 3: ",v_rec_totales.v_cnt_tot_sp3
   DISPLAY "TOTAL REGISTROS SUBPROCESO 5: ",v_rec_totales.v_cnt_tot_sp5

   DISPLAY "\nSE LANZA EL PRIMER SUBPROCESO (SOLICITUD DE TRÁMITE)"
   DISPLAY "SE LANZA EL SEGUNDO SUBPROCESO (FORMALIZACIÓN)"
   DISPLAY "SE LANZA EL TERCER SUBPROCESO (SOLICITUD USO GARANTÍA)"
   DISPLAY "SE LANZA EL QUINTO SUBPROCESO (LIQUIDACIÓN)\n"

   LET v_s_qry = "UPDATE glo_ctr_archivo
                     SET estado = 2 
                   WHERE nombre_archivo = ? 
                     AND proceso_cod    = ?"

   PREPARE prp_upd_edo_arch FROM v_s_qry
   EXECUTE prp_upd_edo_arch USING v_nomArch_proc,
                                  p_proceso_cod

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
   
   CALL fn_lanza_sp1_tramite(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   CALL fn_lanza_sp2_formalizacion(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   CALL fn_lanza_sp3_ug(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)
   CALL fn_lanza_sp5_liqudacion(p_proceso_cod,p_opera_cod,p_v_usuario,v_nomArch_proc)



   
   DISPLAY " GENERA REPORTE"

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_proceso_cod , p_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-",v_c_programa_cod CLIPPED,"-", p_pid USING "&&&&&", "-", p_proceso_cod USING "&&&&&", "-", p_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt(v_c_ruta_listados,v_v_nom_reporte,v_rec_totales.*,v_nomArch_proc) RETURNING v_existe_error

   
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
   IF fgl_report_loadCurrentSettings("GRTE1021.4rp") THEN

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
    WHERE modulo_cod = 'grt'

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
                                 "","GRTP101","",p_v_usuario) RETURNING r_b_valida

      LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               v_d_folio,
                                               "GRTP101",
                                               v_nomArch_proc,
                                               p_v_usuario )

      LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,"/GRTP101 ",
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
                    WHERE modulo_cod = 'grt' "

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
                                 "","GRTP102","",p_v_usuario) RETURNING r_b_valida

      LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               v_d_folio,
                                               "GRTP102",
                                               v_nomArch_proc,
                                               p_v_usuario )

      LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,"/GRTP102 ",
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
                    WHERE modulo_cod = 'grt' "

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
                                  "GRTP103",
                                  "",
                                  p_v_usuario) RETURNING r_b_valida

      LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               v_d_folio,
                                               "GRTP103",
                                               v_nomArch_proc,
                                               p_v_usuario )

      LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,"/GRTP103 ",
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
                    WHERE modulo_cod = 'grt' "

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
                                  "GRTP104",
                                  "",
                                  p_v_usuario) RETURNING r_b_valida

      LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                               p_proceso_cod,
                                               p_opera_cod,
                                               v_d_folio,
                                               "GRTP104",
                                               v_nomArch_proc,
                                               p_v_usuario )

      LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,"/GRTP104 ",
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