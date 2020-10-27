################################################################################################
# Modulo       => AEX                                                                          #
# Programa     => AEXP01                                                                       #
# Objetivo     => Programa que realiza la lectura del archivo para aportaciones extraordinarias#
# Autor        => Omnisys                                                                      #
# Fecha        => 30/Julio/2020                                                                #
################################################################################################

DATABASE safre_viv

   DEFINE g_usuario              CHAR (20)
   DEFINE g_pid                  DECIMAL (9,0)  --ID del proceso
   DEFINE g_proceso_cod          SMALLINT       --código del proceso
   DEFINE g_opera_cod            SMALLINT       --código de operacion
   DEFINE v_nom_archivo          STRING 
   DEFINE v_ruta_rescate         CHAR(40)
   DEFINE g_folio                DECIMAL(10,0)
   DEFINE v_status               SMALLINT
   DEFINE a                      INTEGER
   DEFINE v_s_qry                STRING
   DEFINE v_bnd_nss              SMALLINT
   DEFINE v_bnd_folio            SMALLINT
   DEFINE v_cod_rechazo          SMALLINT
   DEFINE v_id_derechohabiente   DECIMAL(9,0)
   DEFINE v_s_comando            STRING
   DEFINE v_folio_lote           INTEGER    
   DEFINE v_estado_solicitud     SMALLINT
   DEFINE v_ind_rechazo          SMALLINT
   DEFINE v_fecha_pago           DATE
   DEFINE v_desc_detalle         DECIMAL(10,0)
   

   DEFINE arr_aex DYNAMIC ARRAY OF RECORD
      nss                 CHAR (11),
      consecutivo         DECIMAL(2,0),
      referencia          CHAR(27),
      secuencia_pago      DECIMAL(2,0),
      monto_pesos         CHAR(14),
      f_pago              CHAR(8),
      desc_detalle        CHAR(50)
   END RECORD

MAIN
   DEFINE v_comando              STRING
   DEFINE v_ruta_envio           CHAR(40)
   DEFINE nom_archivo            CHAR (40)
   DEFINE v_ruta_nom             STRING
   DEFINE v_nom_arch_sin_ext     base.StringBuffer
   DEFINE v_cnt_totales          INTEGER
   -- Record para la inserción en la historica

   LET g_usuario        = ARG_VAL (1)
   LET g_pid            = ARG_VAL (2)
   LET g_proceso_cod    = ARG_VAL (3)
   LET g_opera_cod      = ARG_VAL (4)
   LET g_folio          = ARG_VAL (5)
   LET v_nom_archivo    = ARG_VAL (6)

   -- Se obtiene el nombre del archivo sin extensión 
   LET v_nom_arch_sin_ext = base.StringBuffer.create()
   CALL v_nom_arch_sin_ext.append(v_nom_archivo)--bu.append(v_nom_archivo)
   CALL v_nom_arch_sin_ext.replace(".apext","",0)

   CALL STARTLOG(g_usuario CLIPPED||".AEXP01.log")

{   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario)
        RETURNING g_folio
}

   LET v_comando = "SELECT ruta_rescate, ruta_envio
                      FROM seg_modulo
                     WHERE modulo_cod = 'aex' "

   PREPARE prp_modulo FROM v_comando
   EXECUTE prp_modulo INTO v_ruta_rescate,
                           v_ruta_envio

   IF (v_nom_archivo IS NULL) THEN
      DISPLAY "El archivo no existe"
   ELSE
      LET nom_archivo = v_nom_archivo CLIPPED

      LET v_ruta_nom = v_ruta_rescate CLIPPED ||"/"||v_nom_archivo

      DISPLAY "RUTA ARCHIVO : ",v_ruta_nom

      CALL fn_crea_tmp_aex()

      -- generar tabla temporal para vlidar datos de entrada--generada

      -- Se realiza el load a la tabla temporal
      LOAD FROM v_ruta_nom
      INSERT INTO safre_tmp:tmp_arch_aex

      --generar fn para validar datos de entrada

   database safre_tmp
        LET v_comando = " alter table tmp_arch_aex add f_carga date "

   PREPARE prp_agrega_campo FROM v_comando
   EXECUTE prp_agrega_campo

   UPDATE safre_tmp:tmp_arch_aex SET f_carga = TODAY
    WHERE f_carga IS NULL

   database safre_viv

      -- Se obtiene el total de registros en el archivo
      LET v_comando = "SELECT COUNT(*)
                         FROM safre_tmp:tmp_arch_aex"

      PREPARE prp_totales FROM v_comando
      EXECUTE prp_totales INTO v_cnt_totales

      CALL fn_valida_datos()

END IF

   DISPLAY "FOLIO: ",g_folio
   DISPLAY "Registros en archivo  : ",v_cnt_totales
   DISPLAY ''
   DISPLAY "Archivo cargado       : ",v_nom_archivo
   DISPLAY ''
   DISPLAY 'El proceso de carga ha finalizado correctamente'

   LET nom_archivo = v_nom_archivo CLIPPED 

   -- Se inserta el archivo en glo_ctr_archivo
   INSERT INTO glo_ctr_archivo
        VALUES(g_proceso_cod, 1, nom_archivo, g_folio, 2, TODAY, g_usuario)

   -- Se actualiza la operación a finalizado
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_status


END MAIN

--##############################################################################
-- función que valida datos de entrada
FUNCTION fn_valida_datos()

   DEFINE v_f_pago      STRING

   LET v_s_qry =  'SELECT *
                    FROM safre_tmp:tmp_arch_aex'

   PREPARE prp_arr_aex FROM v_s_qry
   DECLARE cur_arr_aex CURSOR FOR prp_arr_aex

   LET a = 1

   FOREACH cur_arr_aex INTO arr_aex[a].*
      LET arr_aex[a].desc_detalle = arr_aex[a].desc_detalle CLIPPED
      LET v_desc_detalle = arr_aex[a].desc_detalle
   LET a = a + 1
   --DISPLAY 'a',a
   END FOREACH

   IF arr_aex[a].nss IS NULL THEN
      CALL arr_aex.deleteElement(arr_aex.getLength())

      LET a = a - 1

   END IF

   FOR a = 1 TO arr_aex.getLength()

      LET v_f_pago = arr_aex[a].f_pago[5,6],arr_aex[a].f_pago[7,8],arr_aex[a].f_pago[1,4]
      LET v_fecha_pago = v_f_pago

      CALL fn_valida_nss (arr_aex[a].nss) RETURNING v_bnd_nss
      --DISPLAY 'valida nss : ',arr_aex[a].nss

      IF v_bnd_nss = 1 THEN -- nss si existe en derechohabientes

         CALL fn_valida_llave (arr_aex[a].consecutivo,arr_aex[a].referencia,arr_aex[a].secuencia_pago) RETURNING v_bnd_folio

         IF v_bnd_folio = 1 THEN --folio duplicado
            LET v_ind_rechazo = 2   --2 para folio rechazado según catálogo de rechazos
            --DISPLAY 'rechazo folio: ',a
            CALL fn_rechazo(v_ind_rechazo,a)

         ELSE
            --DISPLAY 'carga : ',a
            CALL fn_carga_registro(a)
         END IF

      ELSE
         LET v_ind_rechazo = 1 --1 cuando nss no exíste en derechohabientes según catálogo de rechazos
         --DISPLAY 'rechazo nss: ',a
         CALL fn_rechazo (v_ind_rechazo,a)
      END IF

   END FOR

END FUNCTION
--##############################################################################

--******************************************************************************
-- función que valida que nss exista en afi_derechohabiente
FUNCTION fn_valida_nss(v_nss)

   DEFINE v_nss      CHAR(11)
   DEFINE v_cnt_nss  SMALLINT

   SELECT COUNT(*)
     INTO v_cnt_nss
     FROM afi_derechohabiente 
    WHERE nss = v_nss

    IF v_cnt_nss >= 1 THEN

       LET v_bnd_nss = 1   --nss existe en afi_derechohabiente
    ELSE
       LET v_bnd_nss = 0   --nss no existe en afi_derechobabiente
    END IF

    RETURN v_bnd_nss

END FUNCTION
--******************************************************************************

--##############################################################################
--función que valida que folio no exista en tabla maestra de aex como aceptado
FUNCTION fn_valida_llave(v_consecutivo,v_referencia,v_secuencia_pago)

   DEFINE v_consecutivo     DECIMAL(2,0)
   DEFINE v_referencia      CHAR(27)
   DEFINE v_secuencia_pago  DECIMAL(2,0)
   DEFINE v_llave           STRING
   DEFINE v_llave_unica     CHAR(54)
   DEFINE v_cnt_llave       INTEGER

{
   LET v_llave = v_consecutivo CLIPPED,v_referencia CLIPPED,v_secuencia_pago CLIPPED
   LET v_llave = v_llave CLIPPED

   LET v_llave_unica = v_llave
}

   SELECT COUNT(*)
     INTO v_cnt_llave
     FROM aex_solicitud_pago
    WHERE llave_consecutivo    = v_consecutivo
      AND llave_referencia     = v_referencia
      AND llave_secuencia_pago = v_secuencia_pago
      AND estado_solicitud     <> 100

   IF v_cnt_llave >= 1 THEN
      LET v_bnd_folio = 1  -- folio duplicado
   ELSE
      LET v_bnd_folio = 0  -- no exíste folio
   END IF

   RETURN v_bnd_folio

END FUNCTION
--##############################################################################

--******************************************************************************
--función que ingresa el rechazo correspondiente en tabla 
FUNCTION fn_rechazo (v_cod_rch,b)

   DEFINE v_cod_rch   SMALLINT
   DEFINE b           INTEGER
   DEFINE v_id_dh     DECIMAL(9,0)

   IF v_cod_rch = 1 THEN
   ELSE
      SELECT id_derechohabiente
        INTO v_id_dh
        FROM afi_derechohabiente
       WHERE nss = arr_aex[b].nss
   END IF

   --DISPLAY 'valida rch',v_cod_rch

   CASE v_cod_rch

      WHEN 1
         LET v_id_derechohabiente   = 0
         LET v_folio_lote           = g_folio
         LET v_estado_solicitud     = 100
         LET v_cod_rechazo          = 1

      WHEN 2
         LET v_id_derechohabiente   = v_id_dh
         LET v_folio_lote           = g_folio
         LET v_estado_solicitud     = 100
         LET v_cod_rechazo          = 2

   END CASE

         INSERT INTO aex_solicitud_pago
            VALUES (
                    seq_aex_solicitud_pago.nextval,
                    v_id_derechohabiente,
                    v_estado_solicitud,
                    v_folio_lote,
                    arr_aex[a].nss,
                    arr_aex[a].consecutivo,
                    arr_aex[a].referencia,
                    arr_aex[a].secuencia_pago,
                    arr_aex[a].monto_pesos,
                    v_fecha_pago,
                    TODAY, --f_lote
                    arr_aex[a].desc_detalle,
                    v_cod_rechazo
                   )
END FUNCTION
--******************************************************************************


--##############################################################################
-- carga registro aceptado en tabla maestra de aex
FUNCTION fn_carga_registro(c)

   DEFINE c               INTEGER
   DEFINE v_id_dh         DECIMAL(9,0)

   SELECT id_derechohabiente
     INTO v_id_dh
     FROM afi_derechohabiente
    WHERE nss = arr_aex[c].nss

   LET v_id_derechohabiente   = v_id_dh
   LET v_folio_lote           = g_folio
   LET v_estado_solicitud     = 40
   LET v_cod_rechazo          = ''

   INSERT INTO aex_solicitud_pago
        VALUES (
                seq_aex_solicitud_pago.nextval,
                v_id_derechohabiente,
                v_estado_solicitud,
                v_folio_lote,
                arr_aex[a].nss,
                arr_aex[a].consecutivo,
                arr_aex[a].referencia,
                arr_aex[a].secuencia_pago,
                arr_aex[a].monto_pesos,
                v_fecha_pago,
                TODAY, --f_lote
                v_desc_detalle,
                v_cod_rechazo
                )

END FUNCTION
--##############################################################################

--******************************************************************************
--función que borra y crea tabla temporal
FUNCTION fn_crea_tmp_aex()

DATABASE safre_tmp

DROP TABLE IF EXISTS tmp_arch_aex

CREATE TABLE tmp_arch_aex 
(nss                 CHAR (11),
llave_consecutivo    DECIMAL(2,0),
llave_referencia     CHAR(27),
llave_secuencia_pago DECIMAL(2,0),
monto_pesos          CHAR(14),
f_pago               CHAR(8),
desc_detalle         CHAR(50)
)


-- crea funcion de rechazos en entrada safre_tmp_tmp_rch_aex

DROP TABLE IF EXISTS tmp_rch_aex

CREATE TABLE tmp_rch_aex
(nss                 CHAR (11),
llave_consecutivo    CHAR(2),
llave_referencia     CHAR(27),
llave_secuencia_pago CHAR(2),
monto_pesos          CHAR(14),
f_pago               CHAR(8),
desc_detalle         CHAR(50)
)

DATABASE safre_viv

END FUNCTION
--******************************************************************************