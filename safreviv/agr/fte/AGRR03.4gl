--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>AGRR03                                        #
#Objetivo          =>Programa que ejecuta el reverso de archivos   #
#                    de salida (Solicitud de saldos) para el módulo#
#                    de Anualidades Garantizadas                   #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>05 Junio 2012                                 #
#Actualización     =>Héctor Jiménez                                #
#Fecha Act.        =>06 Abril 2015                                 #
#Descripcion       =>Se elimina la especifificacion de la BD en la #
#                    ejecución de las funciones                    #
####################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

MAIN
   DEFINE p_v_usuario          LIKE seg_usuario.usuario                -- usuario firmado al sistema
   DEFINE p_d_pid              DECIMAL(9,0)                            -- identificador del proceso
   DEFINE p_i_proceso_cod      LIKE cat_proceso.proceso_cod            -- proceso que llama las funciones
   DEFINE p_i_opera_cod        LIKE cat_operacion.opera_cod            -- operación que llama la funcion 
   DEFINE p_v_nom_archivo      LIKE bat_ctr_operacion.nom_archivo      -- nombre del archivo
   DEFINE p_d_folio            LIKE bat_ctr_operacion.folio            -- folio
   DEFINE v_s_qryTxt           STRING                                  -- contiene una sentencia sql a ejecutar
   DEFINE p_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE r_si_cod_error       SMALLINT                                -- codigo de error en caso de excepción
   DEFINE r_b_valida           SMALLINT                                -- indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario          = ARG_VAL(1)
   LET p_d_pid              = ARG_VAL(2)
   LET p_i_proceso_cod      = ARG_VAL(3)
   LET p_i_opera_cod        = ARG_VAL(4)
   LET p_d_folio            = ARG_VAL(5)
   LET p_v_nom_archivo      = ARG_VAL(6)
   LET p_id_cre_ctr_archivo = ARG_VAL(7)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRR03.log")

   DISPLAY "=INICIA AGRR03="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_nom_archivo

   -- se crea tabla temporal la cual identifica de que proceso corresponde un NSS
   CALL fn_crea_tmp_solic_sdo_agr()

   DISPLAY " EJECUTA REVERSO SOLICITUD DE SALDOS"
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_agr_reversa_arch_salida(?)"
   PREPARE prp_reversa_arch_sal FROM v_s_qryTxt
   EXECUTE prp_reversa_arch_sal USING p_id_cre_ctr_archivo
                                 INTO r_si_cod_error

   IF r_si_cod_error <> 0 THEN
      DISPLAY " Ocurrió un error durante el proceso de reverso: ",r_si_cod_error

      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se invoca la funcion que realiza los reversos del proceso
   CALL fn_reversa_tbls_gen()

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

#Objetivo: Función que actualiza el proceso a reversado
FUNCTION fn_reversa_tbls_gen()
   DEFINE v_d_pid_rev         LIKE bat_ctr_proceso.pid     -- pid
   DEFINE v_i_proceso_cod_rev LIKE cat_proceso.proceso_cod -- proceso a reversar
   DEFINE v_i_opera_cod_rev   LIKE cat_operacion.opera_cod -- operación a reversar

   -- se asigna el proceso y operacion del proceso a reversar
   LET v_i_proceso_cod_rev = g_proc_cod_agr_arch_solic
   LET v_i_opera_cod_rev = 1

   -- se obtiene el del proceso y operación a reversar
   LET v_d_pid_rev = fn_max_pid(v_i_proceso_cod_rev, v_i_opera_cod_rev)

   -- se eliminan el procesos y las operaciones para el proceso_cod correspondiente
   DELETE
     FROM bat_ctr_proceso
    WHERE pid = v_d_pid_rev
      AND proceso_cod = v_i_proceso_cod_rev

   -- se eliminan las operaciones para el proceso_cod correspondiente
   DELETE FROM bat_ctr_operacion
    WHERE pid = v_d_pid_rev
      AND proceso_cod = v_i_proceso_cod_rev
      AND opera_cod = v_i_opera_cod_rev
END FUNCTION

#Objetivo: Función que crea tabla temporal la cual tiene por objetivo diferenciar el módulo
#          correspondiente a cada NSS que llega de Infonavit
FUNCTION fn_crea_tmp_solic_sdo_agr()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   -- en caso de error continua
   WHENEVER ERROR CONTINUE

   -- al encontrar un error detiene el programa

   DROP TABLE IF EXISTS tmp_agr_solic_sdo;

   CREATE TABLE tmp_agr_solic_sdo
   (
      nss                CHAR(11),
      id_derechohabiente DECIMAL(9,0),
      modulo_cod         CHAR(2),
      f_proceso          DATE,
      id_referencia      DECIMAL(9,0),
      aivs97             DECIMAL(12,2),
      aivs92             DECIMAL(12,2)
   );

   DROP TABLE IF EXISTS tmp_agr_solic_sdo_ua;

   CREATE TABLE tmp_agr_solic_sdo_ua
   (
      nss                CHAR(11),
      id_derechohabiente DECIMAL(9,0),
      modulo_cod         CHAR(2),
      f_proceso          DATE,
      id_referencia      DECIMAL(9,0),
      aivs97             DECIMAL(12,2),
      aivs92             DECIMAL(12,2)
   );

   WHENEVER ERROR STOP

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv

END FUNCTION