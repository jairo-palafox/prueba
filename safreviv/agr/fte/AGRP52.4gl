################################################################################
#Modulo            => AGR                                                      #
#Programa          => AGRP52                                                   #
#Objetivo          => Programa para el registro control de archivo del proceso #
#                  => de Actualización de marcas para solicitud de saldo.      #
#Autor             => Emilio Abarca, EFP                                       #
#Fecha inicio      => 27/Agosto/2018                                           #
################################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario       CHAR(20)
   DEFINE p_pid           LIKE bat_ctr_proceso.pid
   DEFINE p_proceso_cod   LIKE cat_proceso.proceso_cod
   DEFINE p_opera_cod     LIKE cat_operacion.opera_cod
   DEFINE p_folio         DECIMAL(10,0)
   DEFINE p_nom_archivo   VARCHAR(100)
   DEFINE v_ruta_rescate  CHAR(40)
   DEFINE v_estado_cod    SMALLINT
   DEFINE v_estado_arh    SMALLINT
   DEFINE v_query         STRING
   DEFINE v_estatus       SMALLINT
   DEFINE v_tot_registros INTEGER
   DEFINE r_b_valida      SMALLINT

END GLOBALS 

MAIN 

   LET p_usuario      = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET p_folio        = ARG_VAL(5)
   LET p_nom_archivo  = ARG_VAL(6)

   CALL STARTLOG(p_usuario CLIPPED|| ".AGRP52.log")

   DISPLAY ""
   DISPLAY " = INICIA AGRP52 = "
   DISPLAY " USUARIO         : ",p_usuario

   -- Nombre del archivo
   IF(p_nom_archivo = "NA") OR (p_nom_archivo = "N/A") THEN
      -- Invoca función que obtiene el nombre del archivo cargado
      LET p_nom_archivo = fn_recupera_arch_cargado(p_proceso_cod, p_opera_cod)
   END IF

   DISPLAY " ARCHIVO         : ",p_nom_archivo

   -- Obtiene PID
   IF(p_pid = 0) THEN
      LET p_pid = fn_max_pid(p_proceso_cod, p_opera_cod)
      IF(p_pid = 0) OR (p_pid IS NULL) THEN
         DISPLAY ""
         DISPLAY " ERROR         : No fue posible obtener el PID del proceso"
         EXIT PROGRAM
         DISPLAY ""
      END IF
   END IF

   DISPLAY " PID             : ",p_pid USING "<<<<<<<<<"
   DISPLAY ""

   -- Verifica el estatus del proceso de carga
   SELECT estado_cod
     INTO v_estado_cod
     FROM bat_ctr_operacion
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod;

   -- Si el estado de la operación = 4 (FINALIZADO) del proceso de carga
   IF(v_estado_cod = 4) THEN
      -- Asigna el estado para el archivo
      LET v_estado_arh = 10
   ELSE
      LET v_estado_arh = 30
      DISPLAY " ERROR         : No se procesaron todos los registros correctamente"
   END IF

   -- Invoca función para el registro control en cre_ctr_archivo
   LET v_query = "EXECUTE FUNCTION fn_agr_ins_ctr_arh_sspr(?,?,?)"

   PREPARE prp_fn FROM v_query
   EXECUTE prp_fn USING p_nom_archivo,
                        v_estado_arh,
                        p_usuario
                   INTO v_estatus,
                        v_tot_registros

   IF(v_estatus = 0) THEN

      DISPLAY " TOTAL REG. DETALLE: ",v_tot_registros
      DISPLAY " SE PROCESÓ LA VALIDACIÓN CORRECTAMENTE"
      DISPLAY " "

      -- Finaliza la operación como correcta
      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   ELSE
      IF(v_estatus = 1) THEN

         DISPLAY " ERROR         : El archivo no contiene Detalle"

         -- Finaliza como errónea la operación
         LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)

         -- Reversa el registro disponible para la integración en glo_ctr_archivo
         CALL fn_act_edo_archivo(p_nom_archivo, p_folio, 3, p_usuario) RETURNING r_b_valida
         DISPLAY " BAND REVERSO  : ",r_b_valida

      END IF
   END IF

   DISPLAY " "
   DISPLAY " = FIN AGRP52"
   DISPLAY " "

END MAIN 

