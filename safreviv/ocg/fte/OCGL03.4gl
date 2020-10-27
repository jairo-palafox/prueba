##################################################################################
#Modulo             => OCG                                                       #
#Programa           => OCGL03                                                    #
#Objetivo           => Programa Lanzador del subproceso 1 (independiente)        #
#                      del proceso de otorgamiento de créditos en garantía 43 bis#
#Autor              => Héctor F. Jiménez Lara                                    #
#Fecha inicio       => 16 de Octubre del 2015                                    #
##################################################################################
DATABASE safre_viv

   DEFINE v_s_qryTxt        STRING      -- Variable para almacenar cadenas de texto 
   DEFINE v_s_cmd           STRING
   DEFINE v_estado          SMALLINT
   DEFINE v_msj_confirma    SMALLINT

MAIN
   DEFINE p_proceso_cod     INTEGER    
   DEFINE p_opera_cod       INTEGER 
   DEFINE p_v_usuario       CHAR(20)
   DEFINE r_b_valida        SMALLINT
   DEFINE v_d_folio         LIKE glo_folio.folio
   DEFINE v_nomArch_proc    LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_pid             DECIMAL(9,0)
   DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados
   DEFINE p_b_tipo_carga    SMALLINT                              -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_nom_prog      VARCHAR(30)                           -- nombre del programa
   DEFINE v_r_id_ocg_arch   DECIMAL(9,0)
   DEFINE v_r_nom_ocg_arch  CHAR(40)
   DEFINE v_s_mensaje       STRING 

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   LET p_proceso_cod     = 3902
   LET p_opera_cod       = 1

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   --Obtiene ruta listados batch
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   -- Se el folio 
   LET v_d_folio = fn_genera_folio(p_proceso_cod, p_opera_cod, p_v_usuario)

   -- Se genera el PID
   LET v_pid = fn_genera_pid(p_proceso_cod,p_opera_cod,p_v_usuario)

   CALL fn_valida_operacion( v_pid,p_proceso_cod,p_opera_cod)   RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN  
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operación"
   ELSE
      -- Se invoca a la función que se encarga de validar que exista un sp pendiente
      CALL fn_recupera_archivo() RETURNING v_r_id_ocg_arch, v_r_nom_ocg_arch

      CALL fn_ventana_confirma("Alerta","Se ejecutara el SP001 Trámite.\n¿Está seguro que desea continuar?","") RETURNING v_msj_confirma

      IF v_msj_confirma = 1 THEN
         LET v_s_mensaje = "Se ejecuta el SP001 con PID: ",v_pid CLIPPED,
                           ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"

         CALL fn_mensaje("Alerta",v_s_mensaje,"information")

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
        DISPLAY "comando :",v_s_cmd

         RUN v_s_cmd

         LET v_estado = 0
      ELSE
         -- Cancela ejecución y sale del programa
         EXIT PROGRAM 
      END IF
   END IF

   IF v_estado = 0 THEN

      CALL fn_actualiza_opera_fin(v_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF
END MAIN


# Función que se encarga de validar que existan subprocesos pendientes y recuperar el nombre del archivo en caso de existir 
FUNCTION fn_recupera_archivo()
   DEFINE v_id_ocg_ctr_arch   DECIMAL(9,0)
   DEFINE v_nom_arch          CHAR(40)
   DEFINE v_cnt_regs          SMALLINT

   LET v_s_qryTxt = " SELECT id_ocg_ctr_archivo,
                             COUNT(*)
                        FROM ocg_ctr_proceso
                       WHERE fin_sp1 = 0 
                       GROUP BY 1 "

   PREPARE prp_cons_id_arch FROM v_s_qryTxt
   EXECUTE prp_cons_id_arch INTO v_id_ocg_ctr_arch,
                                 v_cnt_regs

   IF v_cnt_regs > 0 THEN
      LET v_s_qryTxt = " SELECT nom_archivo
                           FROM ocg_ctr_archivo
                          WHERE id_ocg_ctr_archivo = ? "

      PREPARE prp_cons_nom FROM v_s_qryTxt
      EXECUTE prp_cons_nom INTO v_nom_arch
                          USING v_id_ocg_ctr_arch

      -- Se eliminan los espacios en blanco
      LET v_nom_arch = v_nom_arch CLIPPED
   ELSE 
         CALL fn_mensaje("Alerta","No es posible ejecutar el subproceso debído a que ya fue ejecutado","stop")
         EXIT PROGRAM
   END IF

   RETURN v_id_ocg_ctr_arch, v_nom_arch
END FUNCTION
