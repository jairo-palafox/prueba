##################################################################################
#Modulo             => OCG                                                       #
#Programa           => OCGL15                                                    #
#Objetivo           => Programa Lanzador para devolución de créditos liquidados  #
#Autor              => J. Eduardo Ventura                                        #
#Fecha inicio       => 23 Septiembre del 2016                                    #
##################################################################################
DATABASE safre_viv

   DEFINE v_s_qryTxt        STRING      -- Variable para almacenar cadenas de texto 
   DEFINE v_s_cmd           STRING
   DEFINE v_estado          SMALLINT
   DEFINE v_msj_confirma    SMALLINT

MAIN
   DEFINE p_proceso_cod     INTEGER    
   DEFINE p_opera_cod       INTEGER 
   DEFINE p_usuario_cod     CHAR(20)
   DEFINE p_tipo_ejecucion  SMALLINT
   DEFINE p_s_titulo        CHAR(20)
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
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET p_proceso_cod = 3916
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

   -- Se genera el folio 
   LET v_d_folio = fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario_cod)

   -- Se genera el PID
   LET v_pid = fn_genera_pid(p_proceso_cod,p_opera_cod,p_usuario_cod)

   CALL fn_valida_operacion( v_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   -- se verifica si la operación en proceso es válida
   IF r_b_valida <> 0 THEN  
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_valida_operación"
   ELSE
         CALL fn_inicializa_proceso(v_pid,p_proceso_cod,p_opera_cod,
                                    "","OCGP13","",p_usuario_cod) RETURNING r_b_valida

         LET r_b_valida = fn_actualiza_opera_ini( v_pid,
                                                  p_proceso_cod,
                                                  p_opera_cod,
                                                  v_d_folio,
                                                  "OCGP13",
                                                  v_nomArch_proc,
                                                  p_usuario_cod )

        DISPLAY "tipo ejecución :",p_tipo_ejecucion

         LET v_s_cmd = "nohup time fglrun ", v_ruta_ejecutable CLIPPED,
                                             "/OCGP13 ",
                                             p_usuario_cod, " ",
                                             v_pid," ",
                                             p_proceso_cod," ",
                                             p_opera_cod," ",
                                             p_tipo_ejecucion," ",
                                             p_s_titulo," 1> ",
                                             v_ruta_listados CLIPPED,
                                             "/nohup:",v_pid USING "&&&&&",":",
                                             p_proceso_cod USING "&&&&&",":",
                                             p_opera_cod USING "&&&&&",
                                             " 2>&1 &"

         RUN v_s_cmd

         LET v_estado = 0
         -- Cancela ejecución y sale del programa
         EXIT PROGRAM 
      END IF
{   
   IF v_estado = 0 THEN

      CALL fn_actualiza_opera_fin(v_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF
   }
END MAIN