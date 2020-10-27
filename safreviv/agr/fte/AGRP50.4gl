###############################################################################
#Modulo            => AGR                                                     #
#Programa          => AGRP50                                                  #
#Objetivo          => Genera reporte de actualización de conciliación         #
#                     de marcas a la entrada del recurrente marca de crédito. #
#Autor             => Emilio Abarca, EFP                                      #
#Fecha inicio      => 31/Julio/2018                                           #
###############################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario           CHAR(20)
   DEFINE p_proceso_cod       SMALLINT  
   DEFINE p_opera_cod         SMALLINT
   DEFINE p_pid               DECIMAL(9,0)
   DEFINE r_b_valida          SMALLINT
   DEFINE v_ind_rpt           SMALLINT
   DEFINE p_f_inicio          DATE
   DEFINE p_f_fin             DATE

   TYPE rec_total             RECORD
      total          INTEGER,
      porcentaje     CHAR(12)
   END RECORD

   TYPE rec_conciliacion     RECORD
      marca_ini        SMALLINT,
      marca_act        SMALLINT,
      marca_concatena  CHAR(20),
      total            INTEGER,
      porcentaje       CHAR(12)
   END RECORD

   TYPE rec_rch_conciliacion  RECORD
      total         INTEGER,
      estado        SMALLINT,
      desc_estado   CHAR(40)
   END RECORD

   TYPE arr_global DYNAMIC ARRAY OF rec_conciliacion --Arreglo global

   TYPE arr_rch_global DYNAMIC ARRAY OF rec_rch_conciliacion -- Arreglo global para los rechazos

   -- Records globales RECURRENTE
   DEFINE r_recu_total_g    rec_total
   DEFINE r_recu_total_acep rec_total
   DEFINE r_recu_total_canc rec_total

   -- Arreglos globales RECURRENTE
   DEFINE arr_recu       arr_global
   DEFINE arr_recu_acep  arr_global
   DEFINE arr_recu_canc  arr_global

   --Arreglos globales RECHAZOS RECURRENTE
   DEFINE arr_rch_rec_223  arr_rch_global
   DEFINE arr_rch_rec_225  arr_rch_global

   -- Records globales MICROFLUJO
   DEFINE r_micro_total_g    rec_total
   DEFINE r_micro_total_acep rec_total
   DEFINE r_micro_total_canc rec_total

   -- Arreglos globales MICROFLUJO
   DEFINE arr_micro       arr_global
   DEFINE arr_micro_acep  arr_global
   DEFINE arr_micro_canc  arr_global

   --Arreglos globales RECHAZOS MICROFLUJO
   DEFINE arr_rch_mic_223  arr_rch_global

   -- Records globales 43BIS
   DEFINE r_43_total_g    rec_total
   DEFINE r_43_total_acep rec_total
   DEFINE r_43_total_canc rec_total

   -- Arreglos globales 43BIS
   DEFINE arr_43bis       arr_global
   DEFINE arr_43bis_acep  arr_global
   DEFINE arr_43bis_canc  arr_global

   --Arreglos globales RECHAZOS 43BIS
   DEFINE arr_rch_43  DYNAMIC ARRAY OF RECORD 
      marca_ini     SMALLINT,
      marca_fin     SMALLINT,
      estado        SMALLINT,
      desc_estado   CHAR(40),
      total         INTEGER
   END RECORD 

   DEFINE v_aux_porcentaje  DECIMAL(6,2)

   -- variables para el archivo de salida
   DEFINE v_nombre_archivo  STRING
   DEFINE v_ruta_envio      CHAR(40)

END GLOBALS

MAIN

   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_f_inicio       = ARG_VAL(5)
   LET p_f_fin          = ARG_VAL(6)

   -- Log en caso de errores
   CALL STARTLOG(p_usuario CLIPPED|| ".AGRP50.log")

   CALL fn_display_proceso(0,"GENERACIÓN ARCHIVO CONCILIACIÓN MARCAS")

   DISPLAY " "
   DISPLAY " FECHA INICIO: ",p_f_inicio USING "dd/mm/yyyy"
   DISPLAY " FECHA FIN   : ",p_f_fin USING "dd/mm/yyyy"
   DISPLAY " "

   LET v_ind_rpt = genera_rpt_pdf()

   -- Genera archivo de salida
   CALL gen_arh_salida()

   IF (v_ind_rpt = 0) THEN
      DISPLAY " GENERA REPORTE PDF ...COMPLETADO"
      DISPLAY ""
      DISPLAY " ARCHIVO DE SALIDA ...COMPLETADO"
      DISPLAY " El archivo puede ser recuperado en la ruta /safreviv_int/agr/envio"
      DISPLAY " con nombre: ",v_nombre_archivo

      -- Finaliza proceso
      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

      IF r_b_valida <> 0 THEN
         LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
         -- en caso de error se muestra un mensaje a usuario
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

   ELSE
      DISPLAY " HA OCURRIDO UN ERROR AL GENERAR EL REPORTE PDF ..."
       LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
    END IF

   CALL fn_display_proceso(1,"GENERACIÓN ARCHIVO CONCILIACIÓN MARCAS")

END MAIN

FUNCTION genera_rpt_pdf()

   DEFINE v_reporte_bin    STRING
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_ruta_lst       CHAR(40)
   DEFINE v_ruta_rpt       STRING
   DEFINE v_manejador_rpt  OM.SaxDocumentHandler
   DEFINE bnd_rpt          SMALLINT
   DEFINE v_k              INTEGER

   SELECT ruta_bin,ruta_listados,ruta_envio
     INTO v_ruta_bin,v_ruta_lst,v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr';

   ########################################
   ####          RECURRENTE         #######
   ########################################

   -- Recupera información de todos los registross
   DECLARE crs_recurrente CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 301
      AND marca_fin IS NOT NULL
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   LET v_aux_porcentaje = 0

   -- Inicializa records de totales globales
   LET r_recu_total_g.total = 0

   --inicializa el arreglos
   LET arr_recu[1].marca_concatena = "Sin registros"
   LET arr_recu[1].marca_act  = 0
   LET arr_recu[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_recurrente INTO arr_recu[v_k].marca_ini,
                               arr_recu[v_k].marca_act,
                               arr_recu[v_k].total

      -- Incrementa total global recurrente
      LET r_recu_total_g.total = r_recu_total_g.total + arr_recu[v_k].total

      -- Concatena marca en conciliación
      LET arr_recu[v_k].marca_concatena = "Marca C.",arr_recu[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_recu[arr_recu.getLength()].marca_ini IS NULL) THEN
         CALL arr_recu.deleteElement(arr_recu.getLength())
      END IF
   END IF


   -- Porcentaje total global recurrente
   LET v_aux_porcentaje = (r_recu_total_g.total / r_recu_total_g.total) * 100
   LET r_recu_total_g.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Recupera información de todos los registros ACEPTADOS
   DECLARE crs_rec_aceptados CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 301
      AND marca_fin IS NOT NULL
      AND estado IN (18,20)
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   -- Inicializa records de totales globales
   LET r_recu_total_acep.total = 0

   --inicializa el arreglos
   LET arr_recu_acep[1].marca_concatena = "Sin registros"
   LET arr_recu_acep[1].marca_act  = 0
   LET arr_recu_acep[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_rec_aceptados INTO arr_recu_acep[v_k].marca_ini,
                                  arr_recu_acep[v_k].marca_act,
                                  arr_recu_acep[v_k].total

      -- Incrementa total global recurrente
      LET r_recu_total_acep.total = r_recu_total_acep.total + arr_recu_acep[v_k].total

      -- Concatena marca en conciliación
      LET arr_recu_acep[v_k].marca_concatena = "Marca C.",arr_recu_acep[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_recu_acep[arr_recu_acep.getLength()].marca_ini IS NULL) THEN
         CALL arr_recu_acep.deleteElement(arr_recu_acep.getLength())
      END IF
   END IF

   -- Porcentaje total global recurrente
   LET v_aux_porcentaje = (r_recu_total_acep.total / r_recu_total_acep.total) * 100
   LET r_recu_total_acep.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Recupera información de todos los registros RECHAZADOS
   DECLARE crs_rec_rechazados CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 301
      AND marca_fin IS NOT NULL
      AND estado NOT IN (18,20)
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   -- Inicializa records de totales globales
   LET r_recu_total_canc.total = 0

   --inicializa el arreglos
   LET arr_recu_canc[1].marca_concatena = "Sin registros"
   LET arr_recu_canc[1].marca_act  = 0
   LET arr_recu_canc[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_rec_rechazados INTO arr_recu_canc[v_k].marca_ini,
                                   arr_recu_canc[v_k].marca_act,
                                   arr_recu_canc[v_k].total

      -- Incrementa total global recurrente
      LET r_recu_total_canc.total = r_recu_total_canc.total + arr_recu_canc[v_k].total

      -- Concatena marca en conciliación
      LET arr_recu_canc[v_k].marca_concatena = "Marca C.",arr_recu_canc[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_recu_canc[arr_recu_canc.getLength()].marca_ini IS NULL) THEN
         CALL arr_recu_canc.deleteElement(arr_recu_canc.getLength())
      END IF
   END IF

   -- Porcentaje total global recurrente
   LET v_aux_porcentaje = (r_recu_total_canc.total / r_recu_total_canc.total) * 100
   LET r_recu_total_canc.porcentaje = v_aux_porcentaje CLIPPED,"%"

   ## DETALLE RECHAZOS RECURRENTE

   -- Inicializa arreglo

   LET arr_rch_rec_223[1].total  = 0
   LET arr_rch_rec_223[1].estado = 0
   LET arr_rch_rec_223[1].desc_estado = "---"

   LET arr_rch_rec_225[1].total  = 0
   LET arr_rch_rec_225[1].estado = 0
   LET arr_rch_rec_225[1].desc_estado = "---"


   DECLARE crs_det_rch_223_225 CURSOR FOR
   SELECT c.estado,
          m.estado_desc,
          COUNT(*)
     FROM cre_marca_conciliacion c,
          cat_maq_credito m
    WHERE c.id_proceso = 301
      AND c.estado NOT IN (18,20)
      AND c.estado = m.estado
      AND c.f_proceso BETWEEN p_f_inicio AND p_f_fin
      AND marca_ini = 223
      AND marca_fin = 225
      GROUP BY 1,2;

    LET v_k = 1

    FOREACH crs_det_rch_223_225 INTO arr_rch_rec_223[v_k].estado,
                                     arr_rch_rec_223[v_k].desc_estado,
                                     arr_rch_rec_223[v_k].total

       LET v_k = v_k + 1

    END FOREACH

    DECLARE crs_det_rch_225_223 CURSOR FOR
    SELECT c.estado,
           m.estado_desc,
           COUNT(*)
      FROM cre_marca_conciliacion c,
           cat_maq_credito m
     WHERE c.id_proceso = 301
       AND c.estado NOT IN (18,20)
       AND c.estado = m.estado
       AND c.f_proceso BETWEEN p_f_inicio AND p_f_fin
       AND marca_ini = 225
       AND marca_fin = 223
       GROUP BY 1,2;

    LET v_k = 1

    FOREACH crs_det_rch_225_223 INTO arr_rch_rec_225[v_k].estado,
                                     arr_rch_rec_225[v_k].desc_estado,
                                     arr_rch_rec_225[v_k].total

       LET v_k = v_k + 1

    END FOREACH

   ########################################
   #######        MICROFLUJO       ########
   ########################################

   -- Recupera información de todos los registross
   DECLARE crs_microflujo CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 201  -- Recepción recurrente acreditados
      AND marca_fin IS NOT NULL
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   LET v_aux_porcentaje = 0

   -- Inicializa records de totales globales
   LET r_micro_total_g.total = 0

   --inicializa el arreglos
   LET arr_micro[1].marca_concatena = "Sin registros"
   LET arr_micro[1].marca_act  = 0
   LET arr_micro[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_microflujo INTO arr_micro[v_k].marca_ini,
                               arr_micro[v_k].marca_act,
                               arr_micro[v_k].total

      -- Incrementa total global microflujo
      LET r_micro_total_g.total = r_micro_total_g.total + arr_micro[v_k].total

      -- Concatena marca en conciliación
      LET arr_micro[v_k].marca_concatena = "Marca C.",arr_micro[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_micro[arr_micro.getLength()].marca_ini IS NULL) THEN
         CALL arr_micro.deleteElement(arr_micro.getLength())
      END IF
   END IF


   -- Porcentaje total global microflujo
   LET v_aux_porcentaje = (r_micro_total_g.total / r_micro_total_g.total) * 100
   LET r_micro_total_g.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Recupera información de todos los registros ACEPTADOS
   DECLARE crs_micro_aceptados CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 201
      AND marca_fin IS NOT NULL
      AND estado IN (18,20)
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   -- Inicializa records de totales globales
   LET r_micro_total_acep.total = 0

   --inicializa el arreglos
   LET arr_micro_acep[1].marca_concatena = "Sin registros"
   LET arr_micro_acep[1].marca_act  = 0
   LET arr_micro_acep[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_micro_aceptados INTO arr_micro_acep[v_k].marca_ini,
                                    arr_micro_acep[v_k].marca_act,
                                    arr_micro_acep[v_k].total

      -- Incrementa total global microflujo
      LET r_micro_total_acep.total = r_micro_total_acep.total + arr_micro_acep[v_k].total

      -- Concatena marca en conciliación
      LET arr_micro_acep[v_k].marca_concatena = "Marca C.",arr_micro_acep[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_micro_acep[arr_micro_acep.getLength()].marca_ini IS NULL) THEN
         CALL arr_micro_acep.deleteElement(arr_micro_acep.getLength())
      END IF
   END IF

   -- Porcentaje total global microflujo
   LET v_aux_porcentaje = (r_micro_total_acep.total / r_micro_total_acep.total) * 100
   LET r_micro_total_acep.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Recupera información de todos los registros CANCELADOS
   DECLARE crs_micro_rechazados CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 201
      AND marca_fin IS NOT NULL
      AND estado NOT IN (18,20)
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   -- Inicializa records de totales globales
   LET r_micro_total_canc.total = 0

   --inicializa el arreglos
   LET arr_micro_canc[1].marca_concatena = "Sin registros"
   LET arr_micro_canc[1].marca_act  = 0
   LET arr_micro_canc[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_micro_rechazados INTO arr_micro_canc[v_k].marca_ini,
                                     arr_micro_canc[v_k].marca_act,
                                     arr_micro_canc[v_k].total

      -- Incrementa total global microflujo
      LET r_micro_total_canc.total = r_micro_total_canc.total + arr_micro_canc[v_k].total

      -- Concatena marca en conciliación
      LET arr_micro_canc[v_k].marca_concatena = "Marca C.",arr_micro_canc[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_micro_canc[arr_micro_canc.getLength()].marca_ini IS NULL) THEN
         CALL arr_micro_canc.deleteElement(arr_micro_canc.getLength())
      END IF
   END IF

   -- Porcentaje total global microflujo
   LET v_aux_porcentaje = (r_micro_total_canc.total / r_micro_total_canc.total) * 100
   LET r_micro_total_canc.porcentaje = v_aux_porcentaje CLIPPED,"%"

   ## DETALLE RECHAZOS MICROFLUJO

   -- Inicializa arreglo

   LET arr_rch_mic_223[1].total  = 0
   LET arr_rch_mic_223[1].estado = 0
   LET arr_rch_mic_223[1].desc_estado = "---"

   DECLARE crs_det_mic_223_225 CURSOR FOR
   SELECT c.estado,
          m.estado_desc,
          COUNT(*)
     FROM cre_marca_conciliacion c,
          cat_maq_credito m
    WHERE c.id_proceso = 201
      AND c.estado NOT IN (18,20)
      AND c.estado = m.estado
      AND c.f_proceso BETWEEN p_f_inicio AND p_f_fin
      AND marca_ini = 223
      AND marca_fin = 225
      GROUP BY 1,2;

    LET v_k = 1

    FOREACH crs_det_mic_223_225 INTO arr_rch_mic_223[v_k].estado,
                                     arr_rch_mic_223[v_k].desc_estado,
                                     arr_rch_mic_223[v_k].total

       LET v_k = v_k + 1

    END FOREACH

   ########################################
   #######         43BIS           ########
   ########################################

   -- Recupera información de todos los registross
   DECLARE crs_43bis CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 1229  -- Generación de marca crédito 43bis
      AND marca_fin  = 223
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   LET v_aux_porcentaje = 0

   -- Inicializa records de totales globales
   LET r_43_total_g.total = 0

   --inicializa el arreglos
   LET arr_43bis[1].marca_concatena = "Sin registros"
   LET arr_43bis[1].marca_act  = 0
   LET arr_43bis[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_43bis INTO arr_43bis[v_k].marca_ini,
                          arr_43bis[v_k].marca_act,
                          arr_43bis[v_k].total

      -- Incrementa total global 43bis
      LET r_43_total_g.total = r_43_total_g.total + arr_43bis[v_k].total

      -- Concatena marca en conciliación
      LET arr_43bis[v_k].marca_concatena = "Marca C.",arr_43bis[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_43bis[arr_43bis.getLength()].marca_ini IS NULL) THEN
         CALL arr_43bis.deleteElement(arr_43bis.getLength())
      END IF
   END IF

   -- Porcentaje total global 43bis
   LET v_aux_porcentaje = (r_43_total_g.total / r_43_total_g.total) * 100
   LET r_43_total_g.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Recupera información de todos los registros ACEPTADOS
   DECLARE crs_43_aceptados CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 1229  -- Generación de marca crédito 43bis
      AND marca_fin  = 223
      AND estado IN (18,20)
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   -- Inicializa records de totales globales
   LET r_43_total_acep.total = 0

   --inicializa el arreglos
   LET arr_43bis_acep[1].marca_concatena = "Sin registros"
   LET arr_43bis_acep[1].marca_act  = 0
   LET arr_43bis_acep[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_43_aceptados INTO arr_43bis_acep[v_k].marca_ini,
                                 arr_43bis_acep[v_k].marca_act,
                                 arr_43bis_acep[v_k].total

      -- Incrementa total global 43bis
      LET r_43_total_acep.total = r_43_total_acep.total + arr_43bis_acep[v_k].total

      -- Concatena marca en conciliación
      LET arr_43bis_acep[v_k].marca_concatena = "Marca C.",arr_43bis_acep[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_43bis_acep[arr_43bis_acep.getLength()].marca_ini IS NULL) THEN
         CALL arr_43bis_acep.deleteElement(arr_43bis_acep.getLength())
      END IF
   END IF

   -- Porcentaje total global 43bis
   LET v_aux_porcentaje = (r_43_total_acep.total / r_43_total_acep.total) * 100
   LET r_43_total_acep.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Recupera información de todos los registros RECHAZADOS
   DECLARE crs_43_rechazados CURSOR FOR
   SELECT marca_ini,marca_fin,COUNT(*)
     FROM cre_marca_conciliacion
    WHERE id_proceso = 1229  -- Generación de marca crédito 43bis
      AND marca_fin  = 223
      AND estado NOT IN (18,20)
      AND f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2
      ORDER BY 1,2;

   -- Inicializa records de totales globales
   LET r_43_total_canc.total = 0

   --inicializa el arreglos
   LET arr_43bis_canc[1].marca_concatena = "Sin registros"
   LET arr_43bis_canc[1].marca_act  = 0
   LET arr_43bis_canc[1].total      = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_43_rechazados INTO arr_43bis_canc[v_k].marca_ini,
                                  arr_43bis_canc[v_k].marca_act,
                                  arr_43bis_canc[v_k].total

      -- Incrementa total global 43bis
      LET r_43_total_canc.total = r_43_total_canc.total + arr_43bis_canc[v_k].total

      -- Concatena marca en conciliación
      LET arr_43bis_canc[v_k].marca_concatena = "Marca C.",arr_43bis_canc[v_k].marca_ini CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_43bis_canc[arr_43bis_canc.getLength()].marca_ini IS NULL) THEN
         CALL arr_43bis_canc.deleteElement(arr_43bis_canc.getLength())
      END IF
   END IF

   -- Porcentaje total global 43bis
   LET v_aux_porcentaje = (r_43_total_canc.total / r_43_total_canc.total) * 100
   LET r_43_total_canc.porcentaje = v_aux_porcentaje CLIPPED,"%"

   ## DETALLE RECHAZOS 43BIS

   DECLARE crs_rch_43Bis CURSOR FOR 
   SELECT c.marca_ini,
          c.marca_fin,
          c.estado,
          m.estado_desc,
          COUNT(*)
     FROM cre_marca_conciliacion c,
          cat_maq_credito m
    WHERE c.id_proceso = 1229  -- Generación de marca crédito 43bis
      AND c.estado NOT IN (18,20)
      AND c.estado = m.estado
      AND c.f_proceso BETWEEN p_f_inicio AND p_f_fin
      GROUP BY 1,2,3,4
      ORDER BY c.marca_ini,c.marca_fin;

   -- Inicializa arreglo
   LET arr_rch_43[1].marca_ini = 0
   LET arr_rch_43[1].marca_fin = 0
   LET arr_rch_43[1].estado    = 0
   LET arr_rch_43[1].desc_estado = NULL 
   LET arr_rch_43[1].total     = 0

   LET v_k = 1

   FOREACH crs_rch_43Bis INTO arr_rch_43[v_k].marca_ini,
                              arr_rch_43[v_k].marca_fin,
                              arr_rch_43[v_k].estado,
                              arr_rch_43[v_k].desc_estado,
                              arr_rch_43[v_k].total

      LET v_k = v_k + 1

   END FOREACH

   #################################################
   #   CONFIGURACION PARA SALIDA DEL REPORTE PDF   #
   #################################################

   LET bnd_rpt = 0 --OK

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP501.4rp"
   LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",
                       p_usuario CLIPPED,"-AGRP50-",
                       p_pid USING "&&&&&","-",
                       p_proceso_cod USING "&&&&&","-",
                       p_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN

         START REPORT genera_PDF TO XML HANDLER v_manejador_rpt

            OUTPUT TO REPORT genera_PDF()

         FINISH REPORT genera_PDF

      END IF
   ELSE
       DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
       LET bnd_rpt  = 1
   END IF

   RETURN bnd_rpt

END FUNCTION

REPORT genera_PDF()

   DEFINE v_f_presentacion DATE
   DEFINE f                INTEGER
   DEFINE v_rango          CHAR(30)

   FORMAT
   FIRST PAGE HEADER
      LET v_f_presentacion = TODAY
      LET v_rango = p_f_inicio USING "dd/mm/yyyy", " al ", p_f_fin USING "dd/mm/yyyy"

      #ENCABEZADO
      PRINTX p_usuario
      PRINTX v_f_presentacion USING "dd/mm/yyyy"
      PRINTX v_rango 

      --> Recurrente
      PRINTX r_recu_total_g.total
      PRINTX r_recu_total_g.porcentaje
      PRINTX r_recu_total_acep.total
      PRINTX r_recu_total_acep.porcentaje
      PRINTX r_recu_total_canc.total
      PRINTX r_recu_total_canc.porcentaje

      --> Microflujo
      PRINTX r_micro_total_g.total
      PRINTX r_micro_total_g.porcentaje
      PRINTX r_micro_total_acep.total
      PRINTX r_micro_total_acep.porcentaje
      PRINTX r_micro_total_canc.total
      PRINTX r_micro_total_canc.porcentaje

      --> 43BIS
      PRINTX r_43_total_g.total
      PRINTX r_43_total_g.porcentaje
      PRINTX r_43_total_acep.total
      PRINTX r_43_total_acep.porcentaje
      PRINTX r_43_total_canc.total
      PRINTX r_43_total_canc.porcentaje

   ON EVERY ROW

      LET v_aux_porcentaje = 0

      ######### ---> ARREGLO RECURENTE <--- #########
      -- Total global
      FOR f = 1 TO arr_recu.getLength()
         PRINTX arr_recu[f].marca_concatena
         PRINTX arr_recu[f].marca_act
         PRINTX arr_recu[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_recu[f].total / r_recu_total_g.total) * 100
         LET arr_recu[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_recu[f].porcentaje
      END FOR

      -- Total Aceptados
      FOR f = 1 TO arr_recu_acep.getLength()
         PRINTX arr_recu_acep[f].marca_concatena
         PRINTX arr_recu_acep[f].marca_act
         PRINTX arr_recu_acep[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_recu_acep[f].total / r_recu_total_acep.total) * 100
         LET arr_recu_acep[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_recu_acep[f].porcentaje
      END FOR

      -- Total Rechazados
      FOR f = 1 TO arr_recu_canc.getLength()
         PRINTX arr_recu_canc[f].marca_concatena
         PRINTX arr_recu_canc[f].marca_act
         PRINTX arr_recu_canc[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_recu_canc[f].total / r_recu_total_canc.total) * 100
         LET arr_recu_canc[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_recu_canc[f].porcentaje
      END FOR

      --Detalle rechazos
      FOR f = 1 TO arr_rch_rec_223.getLength()
         PRINTX arr_rch_rec_223[f].total
         PRINTX arr_rch_rec_223[f].estado
         PRINTX arr_rch_rec_223[f].desc_estado
      END FOR

      FOR f = 1 TO arr_rch_rec_225.getLength()
         PRINTX arr_rch_rec_225[f].total
         PRINTX arr_rch_rec_225[f].estado
         PRINTX arr_rch_rec_225[f].desc_estado
      END FOR

      ######### --->  ARREGLO MICROFLUJO  <--- ##########
      -- Total global
      FOR f = 1 TO arr_micro.getLength()
         PRINTX arr_micro[f].marca_concatena
         PRINTX arr_micro[f].marca_act
         PRINTX arr_micro[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_micro[f].total / r_micro_total_g.total) * 100
         LET arr_micro[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_micro[f].porcentaje
      END FOR

      -- Total Aceptados
      FOR f = 1 TO arr_micro_acep.getLength()
         PRINTX arr_micro_acep[f].marca_concatena
         PRINTX arr_micro_acep[f].marca_act
         PRINTX arr_micro_acep[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_micro_acep[f].total / r_micro_total_acep.total) * 100
         LET arr_micro_acep[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_micro_acep[f].porcentaje
      END FOR

      -- Total Rechazados
      FOR f = 1 TO arr_micro_canc.getLength()
         PRINTX arr_micro_canc[f].marca_concatena
         PRINTX arr_micro_canc[f].marca_act
         PRINTX arr_micro_canc[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_micro_canc[f].total / r_micro_total_canc.total) * 100
         LET arr_micro_canc[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_micro_canc[f].porcentaje
      END FOR

      -- Detalle Rechazos
      FOR f = 1 TO arr_rch_mic_223.getLength()
         PRINTX arr_rch_mic_223[f].total
         PRINTX arr_rch_mic_223[f].estado
         PRINTX arr_rch_mic_223[f].desc_estado
      END FOR

      ######### --->  ARREGLO 43BIS  <--- ##########

      -- Total global
      FOR f = 1 TO arr_43bis.getLength()
         PRINTX arr_43bis[f].marca_concatena
         PRINTX arr_43bis[f].marca_act
         PRINTX arr_43bis[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_43bis[f].total / r_43_total_g.total) * 100
         LET arr_43bis[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_43bis[f].porcentaje
      END FOR

       -- Total Aceptados
      FOR f = 1 TO arr_43bis_acep.getLength()
         PRINTX arr_43bis_acep[f].marca_concatena
         PRINTX arr_43bis_acep[f].marca_act
         PRINTX arr_43bis_acep[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_43bis_acep[f].total / r_43_total_acep.total) * 100
         LET arr_43bis_acep[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_43bis_acep[f].porcentaje
      END FOR

       -- Total Rechazados
      FOR f = 1 TO arr_43bis_canc.getLength()
         PRINTX arr_43bis_canc[f].marca_concatena
         PRINTX arr_43bis_canc[f].marca_act
         PRINTX arr_43bis_canc[f].total

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_43bis_canc[f].total / r_43_total_canc.total) * 100
         LET arr_43bis_canc[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_43bis_canc[f].porcentaje
      END FOR

      -- Detalle Rechazos
      FOR f = 1 TO arr_rch_43.getLength()
         PRINTX arr_rch_43[f].marca_ini
         PRINTX arr_rch_43[f].marca_fin
         PRINTX arr_rch_43[f].total
         PRINTX arr_rch_43[f].estado
         PRINTX arr_rch_43[f].desc_estado
      END FOR
      
END REPORT

FUNCTION gen_arh_salida()

   DEFINE v_cadena             STRING
   DEFINE v_aux_id_referencia  DECIMAL(9,0)
   DEFINE v_aux_id_dh          DECIMAL(9,0)
   DEFINE v_id_proceso         SMALLINT
   DEFINE v_aux_estado         SMALLINT
   DEFINE archivo              base.channel
   DEFINE v_salida_arh         STRING
   DEFINE v_detalle            STRING

   DEFINE r_archivo    RECORD
      nss              CHAR(11),
      f_genera         DATE,
      proceso_entrada  CHAR(1),
      f_originacion    DATE,
      marca_encontrada CHAR(3),
      tpo_credito      CHAR(3),
      marca_actualiza  CHAR(3),
      estado           CHAR(1),
      causal           CHAR(3)
   END RECORD

   -- Inicializa variables

   LET v_nombre_archivo = TODAY USING "yyyymmdd","_mc.ctl" CLIPPED
   LET v_salida_arh = v_ruta_envio CLIPPED,"/",v_nombre_archivo
   LET archivo = base.Channel.create()

   LET v_aux_id_referencia = NULL
   LET v_aux_id_dh         = NULL
   LET v_id_proceso        = NULL
   LET v_aux_estado        = NULL

   INITIALIZE r_archivo.* TO NULL

    -- Abre archivo de salida para escritura
   CALL archivo.openFile(v_salida_arh,"w")

   LET v_cadena = "SELECT f.nss,
                          c.id_referencia,
                          c.id_derechohabiente,
                          c.id_proceso,
                          c.marca_ini,
                          c.marca_fin,
                          c.estado,
                          c.f_proceso
                     FROM cre_marca_conciliacion c,
                          afi_derechohabiente f
                    WHERE c.id_derechohabiente = f.id_derechohabiente
                      AND marca_fin IS NOT NULL
                      AND c.f_proceso BETWEEN ","'",p_f_inicio,"'"," AND " ,"'",p_f_fin,"'"

   PREPARE prp_arh_salida FROM v_cadena
   DECLARE crs_arh_salida CURSOR FOR prp_arh_salida

   LET r_archivo.estado   = 1
   LET v_detalle          = NULL
   LET r_archivo.f_genera = TODAY

   FOREACH crs_arh_salida INTO r_archivo.nss,
                               v_aux_id_referencia,
                               v_aux_id_dh,
                               v_id_proceso,
                               r_archivo.marca_encontrada,
                               r_archivo.marca_actualiza,
                               v_aux_estado,
                               r_archivo.f_originacion

      LET r_archivo.causal = "   " -- causal de rechazo

      CASE 
         WHEN v_id_proceso = 301
            LET r_archivo.proceso_entrada = 'R'
         WHEN v_id_proceso = 201
            LET r_archivo.proceso_entrada = 'M'
         WHEN v_id_proceso = 1201
            LET r_archivo.proceso_entrada = 'S'
      END CASE

      -- Obtenemos el tipo de crédito
      SELECT MAX(tpo_credito)
        INTO r_archivo.tpo_credito
        FROM cre_acreditado
       WHERE id_derechohabiente = v_aux_id_dh
         AND tpo_originacion IN (1,2,4);

      -- Aceptado
      IF(v_aux_estado = 10) OR
        (v_aux_estado = 20) THEN
         LET r_archivo.estado = 1
      ELSE
         -- Rechazado
         LET r_archivo.estado = 2
         LET r_archivo.causal = v_aux_estado
      END IF

      LET v_detalle = r_archivo.nss,
                      r_archivo.f_genera USING "yyyymmdd",
                      r_archivo.proceso_entrada,
                      r_archivo.f_originacion USING "yyyymmdd",
                      r_archivo.marca_encontrada,
                      r_archivo.tpo_credito USING "&&&",
                      r_archivo.marca_actualiza,
                      r_archivo.estado,
                      r_archivo.causal USING "&&&"

      CALL archivo.writeLine(v_detalle)

   END FOREACH

   --cierra archivo
   CALL archivo.close()

END FUNCTION




