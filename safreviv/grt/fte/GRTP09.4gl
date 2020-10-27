--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo:           =>GRT                                                #
#Programa:         =>GRTP09                                             #
#Objetivo:         =>Programa para generar las marcas de los registros  #
#                    aceptados en el paso de integración de Uso de      #
#                    Garantía del módulo Uso de Garantía                #
#Autor:            =>Daniel Buendia, EFP                                #
#Fecha inicio:     =>24 Abril 2012                                      #
#########################################################################

DATABASE safre_viv

MAIN
  DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
         p_d_pid             LIKE bat_ctr_proceso.pid, -- pid
         p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
         p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion
         p_d_folio           LIKE cre_ctr_archivo.folio_archivo, -- numero de folio
         p_v_arch_proceso    VARCHAR(100), -- nombre del archivo a integrar
         p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador de la tabla de control
         v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
         v_i_tot_reg_marca   INTEGER, -- total de registros marcados
         v_i_edo_marcaje     SMALLINT, -- estado de marcaje
         v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
         r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
        
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)
   LET p_d_id_cre_ctr_arch = ARG_VAL(7)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTP09.log")

   DISPLAY "=INICIA GRTP09="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso
   DISPLAY " ID CTR ARCHIVO: ",p_d_id_cre_ctr_arch

   -- se inicializan variables
   LET v_c_programa_cod = "GRTP09"

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_d_pid,p_i_proceso_cod,p_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operación fue o no valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_d_pid,p_i_proceso_cod,p_i_opera_cod,
                                           p_d_folio, v_c_programa_cod,
                                           p_v_arch_proceso, p_v_usuario)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   DISPLAY " SE PROCESA LA MARCA"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_uso_procesa_marca_cuenta(?,?,?,?)"

   PREPARE prp_procesa_marca_cuenta FROM v_s_qryTxt
   EXECUTE prp_procesa_marca_cuenta USING p_v_usuario,
                                          p_d_folio,
                                          p_d_id_cre_ctr_arch,
                                          p_i_proceso_cod
                                     INTO v_i_edo_marcaje

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF v_i_edo_marcaje <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE MARCAJE: ",v_i_edo_marcaje

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se realiza el conteo de los registros marcados
   SELECT COUNT(*)
     INTO v_i_tot_reg_marca
     FROM sfr_marca_activa
    WHERE id_derechohabiente IN (
          SELECT UNIQUE id_derechohabiente
            FROM cre_uso_garantia
           WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch)

   DISPLAY "TOTAL DE REGISTROS MARCADOS: ",v_i_tot_reg_marca

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      --EXIT PROGRAM
   END IF
   DISPLAY "=FIN="
END MAIN
