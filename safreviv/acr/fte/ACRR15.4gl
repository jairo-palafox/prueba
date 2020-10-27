--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRR15                                        #
#Objetivo          =>Programa que ejecuta el reverso del archivo   # 
#                    de salida de Solicitud de Devolucion          #
#Autor             =>Daniel Buendia, EFP                           # 
#Fecha inicio      =>29 Febrero 2012                               #
####################################################################

DATABASE safre_viv

MAIN
   DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- usuario firmado al sistema
          p_d_pid             DECIMAL(9,0), -- identificador del proceso
          p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          p_i_opera_cod       LIKE cat_operacion.opera_cod, -- operación que llama la funcion 
          p_d_folio           LIKE bat_ctr_operacion.folio, -- folio
          p_v_nom_archivo     LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_c_tpo_transf      LIKE dse_ctr_archivo.tpo_transferencia, -- tipo de transferencia
          v_s_qryTxt          STRING, -- contiene una sentencia sql a ejecutar
          r_b_valida          SMALLINT -- status que regresa una función externa

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario     = ARG_VAL(1)
   LET p_d_pid         = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod   = ARG_VAL(4)
   LET p_d_folio       = ARG_VAL(5)
   LET p_v_nom_archivo = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRR15.log")

   DISPLAY "=INICIA ACRR15="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_nom_archivo

   -- se inicializan variables
   LET v_c_tpo_transf = "15" -- Devolución de Saldos Excedentes ACR

   DISPLAY " EJECUTA REVERSO SALIDA DSE"
   -- se ejecuta el Procedure que realiza el reverso del archivo de salida Solicitud de Devolucion
   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_act_dse_dev_reverso_salida(?,?)"

   PREPARE prp_reversa_archSal_dse FROM v_s_qryTxt
   EXECUTE prp_reversa_archSal_dse USING v_c_tpo_transf, p_d_folio

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF

   DISPLAY "=FIN="
END MAIN