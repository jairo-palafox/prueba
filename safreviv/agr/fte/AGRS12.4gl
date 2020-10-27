###############################################################################
#Modulo            => AGR                                                     #
#Programa          => AGRS12                                                  #
#Objetivo          => Lanzado genera PDF y extractor recurrente informe.      #
#Autor             => Emilio Abarca EFP                                       #
#Fecha inicio      => 12/Junio/2017                                           #
###############################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE g_usuario        CHAR(20)   
   DEFINE p_pid            DECIMAL(9,0)
   DEFINE p_proceso_cod    INTEGER
   DEFINE p_opera_cod      INTEGER
   DEFINE p_fecha_ejecuta  DATE
   DEFINE v_ruta_envio     CHAR(40)
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_ruta_listados  CHAR(40)
   DEFINE v_titulo_correo  STRING
   --DEFINE v_arh_correo     STRING 
   DEFINE v_mens_correo    STRING
   DEFINE v_estatus_pdf    BOOLEAN
   DEFINE v_estatus_extr   BOOLEAN
   DEFINE v_estatus        BOOLEAN
   DEFINE v_ruta_reporte   STRING 

   -- Arrays
   DEFINE arr_origen DYNAMIC ARRAY OF RECORD
      entidad                SMALLINT,
      tipo_credito           SMALLINT,
      desc_credito           CHAR(30),
      concatena_credito      CHAR(35),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_credito            CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_liquida DYNAMIC ARRAY OF RECORD
      entidad                SMALLINT,
      tipo_credito           SMALLINT,
      desc_credito           CHAR(30),
      concatena_credito      CHAR(35),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_credito            CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   
   DEFINE arr_cancela DYNAMIC ARRAY OF RECORD
      entidad                SMALLINT,
      tipo_credito           SMALLINT,
      desc_credito           CHAR(30),
      concatena_credito      CHAR(35),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_credito            CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_reactiva DYNAMIC ARRAY OF RECORD
      entidad                SMALLINT,
      tipo_credito           SMALLINT,
      desc_credito           CHAR(30),
      concatena_credito      CHAR(35),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_credito            CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD

   DEFINE arr_rch_origina DYNAMIC ARRAY OF RECORD
      tipo_registro          SMALLINT,
      tipo_credito           SMALLINT,
      desc_credito           CHAR(30),
      concatena_credito      CHAR(35),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_credito            CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_rch_reactiva DYNAMIC ARRAY OF RECORD
      tipo_registro          SMALLINT,
      tipo_credito           SMALLINT,
      desc_credito           CHAR(30),
      concatena_credito      CHAR(35),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_credito            CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_rch_liquida DYNAMIC ARRAY OF RECORD
      tipo_registro          SMALLINT,
      tipo_credito           SMALLINT,
      desc_credito           CHAR(30),
      concatena_credito      CHAR(35),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_credito            CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_rch_cancela DYNAMIC ARRAY OF RECORD
      tipo_registro          SMALLINT,
      tipo_credito           SMALLINT,
      desc_credito           CHAR(30),
      concatena_credito      CHAR(35),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_credito            CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_causal_origina DYNAMIC ARRAY OF RECORD
      tipo_registro          SMALLINT,
      estado_rechazo         SMALLINT,
      desc_estado            CHAR(40),
      concatena_causal       CHAR(45),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_causal             CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_causal_reactiva DYNAMIC ARRAY OF RECORD
      tipo_registro          SMALLINT,
      estado_rechazo         SMALLINT,
      desc_estado            CHAR(40),
      concatena_causal       CHAR(45),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_causal             CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_causal_liquida DYNAMIC ARRAY OF RECORD
      tipo_registro          SMALLINT,
      estado_rechazo         SMALLINT,
      desc_estado            CHAR(40),
      concatena_causal       CHAR(45),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_causal             CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   DEFINE arr_causal_cancela DYNAMIC ARRAY OF RECORD
      tipo_registro          SMALLINT,
      estado_rechazo         SMALLINT,
      desc_estado            CHAR(40),
      concatena_causal       CHAR(45),
      total_registros        INTEGER ,
      total_reg_global       INTEGER ,
      prc_causal             CHAR(8) ,
      prc_global             CHAR(8) ,
      total_deudor           INTEGER ,
      total_global_deudor    INTEGER ,
      total_no_deudor        INTEGER ,
      total_global_no_deudor INTEGER, 
      total_sdo_deudor       DECIMAL(14,2),
      total_global_saldo     DECIMAL(14,2)
   END RECORD 

   -- Variables del reporte 
   DEFINE v_suma_reg       INTEGER
   DEFINE v_suma_reg_lq    INTEGER 
   DEFINE v_suma_reg_c     INTEGER 
   DEFINE v_suma_reg_r     INTEGER 
   DEFINE v_suma_rch_o     INTEGER 
   DEFINE v_suma_rch_r     INTEGER 
   DEFINE v_suma_rch_l     INTEGER 
   DEFINE v_suma_rch_c     INTEGER 
   DEFINE v_suma_causal_o  INTEGER 
   DEFINE v_suma_causal_r  INTEGER 
   DEFINE v_suma_causal_l  INTEGER 
   DEFINE v_suma_causal_c  INTEGER 
   DEFINE v_suma_deudor    INTEGER
   DEFINE v_suma_saldo     DECIMAL(14,2) 
   DEFINE v_suma_no_deudor INTEGER 

   --Variables para la fecha de ejecución
   DEFINE f                INTEGER
   DEFINE v_dia_inhabil    INTEGER
   DEFINE v_dia            DATE 
   DEFINE v_ind_dia        SMALLINT  
   
END GLOBALS 

MAIN

   LET g_usuario       = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = 340
   LET p_opera_cod     = 1
   LET p_fecha_ejecuta = ARG_VAL(5)

   CALL STARTLOG(g_usuario CLIPPED || ".AGRS12.log")

   --Se obtiene la ruta donde se alojará el extractor
   SELECT ruta_envio,ruta_bin
     INTO v_ruta_envio,
        v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   -- Se obtiene la ruta donde se alojará el .PDF
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   --Para la ejecución automática, se obtiene la fecha de ejecución
   --ya que esa fecha la envía el lanzador.

   LET v_dia = TODAY 
   
   IF(p_fecha_ejecuta IS NULL) THEN

      --Verifica que el dia anterior sea un día hábil
      FOR f = 1 TO 10
      
         SELECT COUNT(*)
            INTO v_dia_inhabil
            FROM cat_feriado
           WHERE feriado_fecha = v_dia -f

         -- Si el día es inhabil
         IF (v_dia_inhabil >= 1) THEN 
            CONTINUE FOR 
         ELSE 
            -- Verifica que no sea un domingo
            LET v_ind_dia = WEEKDAY(v_dia -f)

            -- 0 (Domingo)
            IF(V_ind_dia = 0) THEN
               CONTINUE FOR
            ELSE 
               --Asigna ese día hábil que no sea domingo y se sale del FOR 
               LET p_fecha_ejecuta = v_dia -f  
               
               EXIT FOR  
               
            END IF
            
         END IF
         
      END FOR 
      
   END IF 
   
   DISPLAY " INICIA GENERACIÓN DEL INFORME RECURRENTE AG"
   DISPLAY " Fecha de generación : ", p_fecha_ejecuta USING "dd/mm/yyyy"
   
   CALL fn_gen_extractor() RETURNING v_estatus_extr
   CALL fn_genera_pdf() RETURNING v_estatus_pdf

   IF (v_estatus_pdf <> 0) AND (v_estatus_extr <> 1)THEN
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_estatus
   ELSE 
      DISPLAY ""
      DISPLAY " => El archivo PDF y el extractor recurrente informe se ha generado correctamente"
      DISPLAY " => Los archivos de encuentran en la ruta /safreviv_int/agr/envio"
      DISPLAY ""
      DISPLAY " => Envío de correo del reporte y extractor generado"
      DISPLAY ""

      LET v_titulo_correo = "Proceso: RECURRENTE INFORME SACI "
      --LET v_arh_correo    = v_ruta_listados CLIPPED,"/",g_usuario CLIPPED,"-","AGRS12",".pdf"
      LET v_mens_correo   = "Pid proceso  : ",p_pid,"\n",
                         "Proceso      : RECURRENTE INFORME \n",
                         "Operacion    : GENERA RECURRENTE INFORME\n"
                         
      -- se invoca la función que envía por correo el elemento generado
      CALL fn_correo_proceso(p_pid,
                             p_proceso_cod,
                             p_opera_cod,
                             v_ruta_reporte,
                             v_titulo_correo,
                             v_mens_correo)
                              
      DISPLAY ""
      DISPLAY " PROCESO EJECUTADO CORRECTAMENTE"

      CALL fn_display_proceso(0,"FIN DE GENERACIÓN DEL RECURRENTE INFORME")
      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_estatus
   END IF 
      
END MAIN 

FUNCTION fn_genera_pdf()

   DEFINE reporte       om.SaxDocumentHandler -- Objeto para reporte PDF
   DEFINE v_reporte      STRING 
   DEFINE v_nombre_pdf   STRING 
   DEFINE v_indicador    BOOLEAN
   DEFINE v_query_report STRING 
   DEFINE c              INTEGER

   LET v_reporte    = v_ruta_bin CLIPPED,"/AGRS12.4rp"
   LET v_nombre_pdf = "Recurrente_informe.pdf"

#--------------------- Originaciones (Entidad 1) --------------------------

   -- Inicializa arreglo en 0, en caso de no encontrar información para el reporte
   LET arr_origen[1].entidad                = 0
   LET arr_origen[1].tipo_credito           = 0
   LET arr_origen[1].total_registros        = 0
   LET arr_origen[1].total_reg_global       = 0
   LET arr_origen[1].prc_credito            = 0
   LET arr_origen[1].prc_global             = 0
   LET arr_origen[1].total_deudor           = 0
   LET arr_origen[1].total_global_deudor    = 0
   LET arr_origen[1].total_no_deudor        = 0
   LET arr_origen[1].total_global_no_deudor = 0
   LET arr_origen[1].total_sdo_deudor       = 0
   LET arr_origen[1].total_global_saldo     = 0
    
   LET v_query_report = "SELECT entidad,
                                tpo_credito,
                                desc_credito,
                                COUNT(*)
                          FROM safre_tmp:tmp_rec_informe
                         WHERE entidad = 1
                           AND identificador = 'originacion'
                           GROUP BY 1,2,3
                           ORDER BY 4 DESC;"


   PREPARE prp_originaciones FROM v_query_report 
   DECLARE crs_originaciones CURSOR FOR prp_originaciones 

   LET c = 1
   LET v_suma_reg       = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_originaciones INTO  arr_origen[c].entidad,
                                    arr_origen[c].tipo_credito,
                                    arr_origen[c].desc_credito,
                                    arr_origen[c].total_registros
                                    
      
      LET arr_origen[c].concatena_credito = arr_origen[c].tipo_credito USING "&&" CLIPPED,"-",
                                             arr_origen[c].desc_credito CLIPPED

      -- Obtiene total de registros
      LET v_suma_reg = v_suma_reg + arr_origen[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_origen[c].total_sdo_deudor,
               arr_origen[c].total_deudor
         FROM safre_tmp:tmp_rec_informe
        WHERE entidad = 1
          AND tpo_credito = arr_origen[c].tipo_credito
          AND identificador = "originacion"
          AND sdo_deudor > 0;

      IF(arr_origen[c].total_sdo_deudor IS NULL) THEN
         LET arr_origen[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_origen[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_origen[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_origen[c].total_no_deudor
         FROM safre_tmp:tmp_rec_informe
        WHERE entidad = 1
          AND tpo_credito = arr_origen[c].tipo_credito
          AND identificador = "originacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_origen[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_origen[c-1].total_reg_global = v_suma_reg 

      --Total deudores
      LET arr_origen[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_origen[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_origen[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_origen[arr_origen.getLength()].entidad IS NULL) AND 
      (arr_origen[arr_origen.getLength()].tipo_credito IS NULL) THEN

      CALL arr_origen.deleteElement(arr_origen.getLength())
      
   END IF 

#--------------------- Liquidaciones (Entidad 2) -----------------------------------

   LET arr_liquida[1].entidad                = 0
   LET arr_liquida[1].tipo_credito           = 0
   LET arr_liquida[1].total_registros        = 0
   LET arr_liquida[1].total_reg_global       = 0
   LET arr_liquida[1].prc_credito            = 0
   LET arr_liquida[1].prc_global             = 0
   LET arr_liquida[1].total_deudor           = 0
   LET arr_liquida[1].total_global_deudor    = 0
   LET arr_liquida[1].total_no_deudor        = 0
   LET arr_liquida[1].total_global_no_deudor = 0
   LET arr_liquida[1].total_sdo_deudor       = 0
   LET arr_liquida[1].total_global_saldo     = 0

   LET v_query_report = "SELECT entidad,
                                tpo_credito,
                                desc_credito,
                                COUNT(*)
                          FROM safre_tmp:tmp_rec_informe
                         WHERE entidad = 2
                           AND identificador = 'liquidacion'
                           GROUP BY 1,2,3
                           ORDER BY 4 DESC;"


   PREPARE prp_liquidaciones FROM v_query_report 
   DECLARE crs_liquidaciones CURSOR FOR prp_liquidaciones

   LET c = 1
   LET v_suma_reg_lq    = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_liquidaciones INTO  arr_liquida[c].entidad,
                                    arr_liquida[c].tipo_credito,
                                    arr_liquida[c].desc_credito,
                                    arr_liquida[c].total_registros
                                    
      
      LET arr_liquida[c].concatena_credito = arr_liquida[c].tipo_credito USING "&&" CLIPPED,"-",
                                              arr_liquida[c].desc_credito CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_reg_lq = v_suma_reg_lq + arr_liquida[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_liquida[c].total_sdo_deudor,
               arr_liquida[c].total_deudor
         FROM safre_tmp:tmp_rec_informe
        WHERE entidad = 2
          AND tpo_credito = arr_liquida[c].tipo_credito
          AND identificador = "liquidacion"
          AND sdo_deudor > 0;

      IF(arr_liquida[c].total_sdo_deudor IS NULL) THEN
         LET arr_liquida[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_liquida[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_liquida[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_liquida[c].total_no_deudor
         FROM safre_tmp:tmp_rec_informe
        WHERE entidad = 2
          AND tpo_credito = arr_liquida[c].tipo_credito
          AND identificador = "liquidacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_liquida[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_liquida[c-1].total_reg_global = v_suma_reg_lq

      --Total deudores
      LET arr_liquida[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_liquida[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_liquida[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_liquida[arr_liquida.getLength()].entidad IS NULL) AND 
      (arr_liquida[arr_liquida.getLength()].tipo_credito IS NULL) THEN

      CALL arr_liquida.deleteElement(arr_liquida.getLength())
      
   END IF 

#--------------------- Cancelaciones (Entidad 5) -----------------------------

   LET arr_cancela[1].entidad                = 0
   LET arr_cancela[1].tipo_credito           = 0
   LET arr_cancela[1].total_registros        = 0
   LET arr_cancela[1].total_reg_global       = 0
   LET arr_cancela[1].prc_credito            = 0
   LET arr_cancela[1].prc_global             = 0
   LET arr_cancela[1].total_deudor           = 0
   LET arr_cancela[1].total_global_deudor    = 0
   LET arr_cancela[1].total_no_deudor        = 0
   LET arr_cancela[1].total_global_no_deudor = 0
   LET arr_cancela[1].total_sdo_deudor       = 0
   LET arr_cancela[1].total_global_saldo     = 0

   LET v_query_report = "SELECT entidad,
                                tpo_credito,
                                desc_credito,
                                COUNT(*)
                           FROM safre_tmp:tmp_rec_informe
                          WHERE entidad = 5
                            AND identificador = 'cancelacion'
                            GROUP BY 1,2,3
                            ORDER BY 4 DESC;"


   PREPARE prp_cancelacion FROM v_query_report 
   DECLARE crs_cancelacion CURSOR FOR prp_cancelacion

   LET c = 1
   LET v_suma_reg_c     = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_cancelacion INTO arr_cancela[c].entidad,
                                  arr_cancela[c].tipo_credito,
                                  arr_cancela[c].desc_credito,
                                  arr_cancela[c].total_registros
                                    
      
      LET arr_cancela[c].concatena_credito = arr_cancela[c].tipo_credito USING "&&" CLIPPED,"-",
                                              arr_cancela[c].desc_credito CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_reg_c = v_suma_reg_c + arr_cancela[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_cancela[c].total_sdo_deudor,
               arr_cancela[c].total_deudor
         FROM safre_tmp:tmp_rec_informe
        WHERE entidad = 5
          AND tpo_credito = arr_cancela[c].tipo_credito
          AND identificador = "cancelacion"
          AND sdo_deudor > 0;

      IF(arr_cancela[c].total_sdo_deudor IS NULL) THEN
         LET arr_cancela[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_cancela[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_cancela[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_cancela[c].total_no_deudor
         FROM safre_tmp:tmp_rec_informe
        WHERE entidad = 5
          AND tpo_credito = arr_cancela[c].tipo_credito
          AND identificador = "cancelacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_cancela[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_cancela[c-1].total_reg_global = v_suma_reg_c

      --Total deudores
      LET arr_cancela[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_cancela[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_cancela[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_cancela[arr_cancela.getLength()].entidad IS NULL) AND 
      (arr_cancela[arr_cancela.getLength()].tipo_credito IS NULL) THEN

      CALL arr_cancela.deleteElement(arr_cancela.getLength())
      
   END IF 

#--------------------- Reactivaciones (Entidad 1) -------------------------

   LET arr_reactiva[1].entidad                = 0
   LET arr_reactiva[1].tipo_credito           = 0
   LET arr_reactiva[1].total_registros        = 0
   LET arr_reactiva[1].total_reg_global       = 0
   LET arr_reactiva[1].prc_credito            = 0
   LET arr_reactiva[1].prc_global             = 0
   LET arr_reactiva[1].total_deudor           = 0
   LET arr_reactiva[1].total_global_deudor    = 0
   LET arr_reactiva[1].total_no_deudor        = 0
   LET arr_reactiva[1].total_global_no_deudor = 0
   LET arr_reactiva[1].total_sdo_deudor       = 0
   LET arr_reactiva[1].total_global_saldo     = 0


   LET v_query_report = "SELECT entidad,
                                tpo_credito,
                                desc_credito,
                                COUNT(*)
                           FROM safre_tmp:tmp_rec_informe
                          WHERE entidad = 1
                            AND identificador = 'reactivacion'
                            GROUP BY 1,2,3
                            ORDER BY 4 DESC;"


   PREPARE prp_reactivacion FROM v_query_report 
   DECLARE crs_reactivacion CURSOR FOR prp_reactivacion

   LET c = 1
   LET v_suma_reg_r     = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_reactivacion INTO arr_reactiva[c].entidad,
                                  arr_reactiva[c].tipo_credito,
                                  arr_reactiva[c].desc_credito,
                                  arr_reactiva[c].total_registros
                                    
      
      LET arr_reactiva[c].concatena_credito = arr_reactiva[c].tipo_credito USING "&&" CLIPPED,"-",
                                              arr_reactiva[c].desc_credito CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_reg_r = v_suma_reg_r + arr_reactiva[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_reactiva[c].total_sdo_deudor,
               arr_reactiva[c].total_deudor
         FROM safre_tmp:tmp_rec_informe
        WHERE entidad = 1
          AND tpo_credito = arr_reactiva[c].tipo_credito
          AND identificador = "reactivacion"
          AND sdo_deudor > 0;

      IF(arr_reactiva[c].total_sdo_deudor IS NULL) THEN
         LET arr_reactiva[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_reactiva[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_reactiva[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_reactiva[c].total_no_deudor
         FROM safre_tmp:tmp_rec_informe
        WHERE entidad = 1
          AND tpo_credito = arr_reactiva[c].tipo_credito
          AND identificador = "reactivacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_reactiva[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_reactiva[c-1].total_reg_global = v_suma_reg_r

      --Total deudores
      LET arr_reactiva[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_reactiva[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_reactiva[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_reactiva[arr_reactiva.getLength()].entidad IS NULL) AND 
      (arr_reactiva[arr_reactiva.getLength()].tipo_credito IS NULL) THEN

      CALL arr_reactiva.deleteElement(arr_reactiva.getLength())
      
   END IF 

#--------------------- Rechazos originaciones (tipo registro: 1,20,4) incluye reactivaciones -------------------------

   LET arr_rch_origina[1].tipo_registro          = 0
   LET arr_rch_origina[1].tipo_credito           = 0
   LET arr_rch_origina[1].total_registros        = 0
   LET arr_rch_origina[1].total_reg_global       = 0
   LET arr_rch_origina[1].prc_credito            = 0
   LET arr_rch_origina[1].prc_global             = 0
   LET arr_rch_origina[1].total_deudor           = 0
   LET arr_rch_origina[1].total_global_deudor    = 0
   LET arr_rch_origina[1].total_no_deudor        = 0
   LET arr_rch_origina[1].total_global_no_deudor = 0
   LET arr_rch_origina[1].total_sdo_deudor       = 0
   LET arr_rch_origina[1].total_global_saldo     = 0

   LET v_query_report = "SELECT tpo_credito,
                                desc_credito,
                                COUNT(*)
                           FROM safre_tmp:tmp_rch_informe
                          WHERE tipo_registro IN (1,20,4)
                            AND identificador = 'rch_originacion'
                            GROUP BY 1,2
                            ORDER BY 3 DESC;"


   PREPARE prp_rch_origina FROM v_query_report 
   DECLARE crs_rch_origina CURSOR FOR prp_rch_origina

   LET c = 1
   LET v_suma_rch_o     = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_rch_origina INTO arr_rch_origina[c].tipo_credito,
                                 arr_rch_origina[c].desc_credito,
                                 arr_rch_origina[c].total_registros
                                    
      LET arr_rch_origina[c].concatena_credito = arr_rch_origina[c].tipo_credito USING "&&" CLIPPED,"-",
                                                  arr_rch_origina[c].desc_credito CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_rch_o = v_suma_rch_o + arr_rch_origina[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_rch_origina[c].total_sdo_deudor,
               arr_rch_origina[c].total_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro IN (1,20,4)
          AND tpo_credito = arr_rch_origina[c].tipo_credito
          AND identificador = "rch_originacion"
          AND sdo_deudor > 0;

      IF(arr_rch_origina[c].total_sdo_deudor IS NULL) THEN
         LET arr_rch_origina[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_rch_origina[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_rch_origina[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_rch_origina[c].total_no_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro IN (1,20,4)
          AND tpo_credito = arr_rch_origina[c].tipo_credito
          AND identificador = "rch_originacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_rch_origina[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_rch_origina[c-1].total_reg_global = v_suma_rch_o

      --Total deudores
      LET arr_rch_origina[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_rch_origina[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_rch_origina[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_rch_origina[arr_rch_origina.getLength()].tipo_registro IS NULL) AND 
      (arr_rch_origina[arr_rch_origina.getLength()].tipo_credito IS NULL) THEN

      CALL arr_rch_origina.deleteElement(arr_rch_origina.getLength())
      
   END IF
  
#--------------------- Rechazos reactivaciones (tipo registro: 4) -------------------------

   LET arr_rch_reactiva[1].tipo_credito           = 0
   LET arr_rch_reactiva[1].tipo_credito           = 0
   LET arr_rch_reactiva[1].total_registros        = 0
   LET arr_rch_reactiva[1].total_reg_global       = 0 
   LET arr_rch_reactiva[1].prc_credito            = 0
   LET arr_rch_reactiva[1].prc_global             = 0
   LET arr_rch_reactiva[1].total_deudor           = 0
   LET arr_rch_reactiva[1].total_global_deudor    = 0
   LET arr_rch_reactiva[1].total_no_deudor        = 0
   LET arr_rch_reactiva[1].total_global_no_deudor = 0
   LET arr_rch_reactiva[1].total_sdo_deudor       = 0
   LET arr_rch_reactiva[1].total_global_saldo     = 0

   
  
   LET v_query_report = "SELECT tpo_credito,
                                desc_credito,
                                COUNT(*)
                           FROM safre_tmp:tmp_rch_informe
                          WHERE tipo_registro = 4
                            AND identificador = 'rch_originacion'
                            GROUP BY 1,2
                            ORDER BY 3 DESC;"


   PREPARE prp_rch_reactiva FROM v_query_report 
   DECLARE crs_rch_reactiva CURSOR FOR prp_rch_reactiva

   LET c = 1
   LET v_suma_rch_r     = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_rch_reactiva INTO arr_rch_reactiva[c].tipo_credito,
                                  arr_rch_reactiva[c].desc_credito,
                                  arr_rch_reactiva[c].total_registros

      LET arr_rch_reactiva[c].concatena_credito = arr_rch_reactiva[c].tipo_credito USING "&&" CLIPPED,"-",
                                                   arr_rch_reactiva[c].desc_credito CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_rch_r = v_suma_rch_r + arr_rch_reactiva[c].total_registros
     
      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_rch_reactiva[c].total_sdo_deudor,
               arr_rch_reactiva[c].total_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 4
          AND tpo_credito = arr_rch_reactiva[c].tipo_credito
          AND identificador = "rch_originacion"
          AND sdo_deudor > 0;

      IF(arr_rch_reactiva[c].total_sdo_deudor IS NULL) THEN
         LET arr_rch_reactiva[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_rch_reactiva[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_rch_reactiva[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_rch_reactiva[c].total_no_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 4
          AND tpo_credito = arr_rch_reactiva[c].tipo_credito
          AND identificador = "rch_originacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_rch_reactiva[c].total_no_deudor
     
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_rch_reactiva[c-1].total_reg_global = v_suma_rch_r

      --Total deudores
      LET arr_rch_reactiva[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_rch_reactiva[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_rch_reactiva[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 
 
   -- Elimina la última fila en blanco
   IF (arr_rch_reactiva[arr_rch_reactiva.getLength()].tipo_registro IS NULL) AND 
      (arr_rch_reactiva[arr_rch_reactiva.getLength()].tipo_credito IS NULL)  AND 
      (arr_rch_reactiva[arr_rch_reactiva.getLength()].total_registros IS NULL)THEN
      
      CALL arr_rch_reactiva.deleteElement(arr_rch_reactiva.getLength())
      
   END IF 

#--------------------- Rechazos liquidaciones (tipo registro: 11) -------------------------

   LET arr_rch_liquida[1].tipo_registro          = 0
   LET arr_rch_liquida[1].tipo_credito           = 0
   LET arr_rch_liquida[1].total_registros        = 0
   LET arr_rch_liquida[1].total_reg_global       = 0
   LET arr_rch_liquida[1].prc_credito            = 0
   LET arr_rch_liquida[1].prc_global             = 0
   LET arr_rch_liquida[1].total_deudor           = 0
   LET arr_rch_liquida[1].total_global_deudor    = 0
   LET arr_rch_liquida[1].total_no_deudor        = 0
   LET arr_rch_liquida[1].total_global_no_deudor = 0
   LET arr_rch_liquida[1].total_sdo_deudor       = 0
   LET arr_rch_liquida[1].total_global_saldo     = 0

   LET v_query_report = "SELECT tpo_credito,
                                desc_credito,
                                COUNT(*)
                           FROM safre_tmp:tmp_rch_informe
                          WHERE tipo_registro = 11
                            AND identificador = 'rch_liquidacion'
                            GROUP BY 1,2
                            ORDER BY 3 DESC;"


   PREPARE prp_rch_liquida FROM v_query_report 
   DECLARE crs_rch_liquida CURSOR FOR prp_rch_liquida

   LET c = 1
   LET v_suma_rch_l     = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_rch_liquida INTO arr_rch_liquida[c].tipo_credito,
                                 arr_rch_liquida[c].desc_credito,
                                 arr_rch_liquida[c].total_registros
                                    
      LET arr_rch_liquida[c].concatena_credito = arr_rch_liquida[c].tipo_credito USING "&&" CLIPPED,"-",
                                                  arr_rch_liquida[c].desc_credito CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_rch_l = v_suma_rch_l + arr_rch_liquida[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_rch_liquida[c].total_sdo_deudor,
               arr_rch_liquida[c].total_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 11
          AND tpo_credito = arr_rch_liquida[c].tipo_credito
          AND identificador = "rch_liquidacion"
          AND sdo_deudor > 0;

      IF(arr_rch_liquida[c].total_sdo_deudor IS NULL) THEN
         LET arr_rch_liquida[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_rch_liquida[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_rch_liquida[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_rch_liquida[c].total_no_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 11
          AND tpo_credito = arr_rch_liquida[c].tipo_credito
          AND identificador = "rch_liquidacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_rch_liquida[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_rch_liquida[c-1].total_reg_global = v_suma_rch_l

      --Total deudores
      LET arr_rch_liquida[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_rch_liquida[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_rch_liquida[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_rch_liquida[arr_rch_liquida.getLength()].tipo_registro IS NULL) AND 
      (arr_rch_liquida[arr_rch_liquida.getLength()].tipo_credito IS NULL) THEN

      CALL arr_rch_liquida.deleteElement(arr_rch_liquida.getLength())
      
   END IF 


#--------------------- Rechazos cancelaciones (tipo registro: 5) -------------------------

   LET arr_rch_cancela[1].tipo_registro          = 0
   LET arr_rch_cancela[1].tipo_credito           = 0
   LET arr_rch_cancela[1].total_registros        = 0
   LET arr_rch_cancela[1].total_reg_global       = 0 
   LET arr_rch_cancela[1].prc_credito            = 0
   LET arr_rch_cancela[1].prc_global             = 0
   LET arr_rch_cancela[1].total_deudor           = 0
   LET arr_rch_cancela[1].total_global_deudor    = 0
   LET arr_rch_cancela[1].total_no_deudor        = 0
   LET arr_rch_cancela[1].total_global_no_deudor = 0
   LET arr_rch_cancela[1].total_sdo_deudor       = 0
   LET arr_rch_cancela[1].total_global_saldo     = 0
   
   LET v_query_report = "SELECT tpo_credito,
                                desc_credito,
                                COUNT(*)
                           FROM safre_tmp:tmp_rch_informe
                          WHERE tipo_registro = 5
                            AND identificador = 'rch_cancelacion'
                            GROUP BY 1,2
                            ORDER BY 3 DESC;"


   PREPARE prp_rch_cancela FROM v_query_report 
   DECLARE crs_rch_cancela CURSOR FOR prp_rch_cancela

   LET c = 1
   LET v_suma_rch_c     = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_rch_cancela INTO arr_rch_cancela[c].tipo_credito,
                                 arr_rch_cancela[c].desc_credito,
                                 arr_rch_cancela[c].total_registros
                                    
      LET arr_rch_cancela[c].concatena_credito = arr_rch_cancela[c].tipo_credito USING "&&" CLIPPED,"-",
                                                  arr_rch_cancela[c].desc_credito CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_rch_c = v_suma_rch_c + arr_rch_cancela[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_rch_cancela[c].total_sdo_deudor,
               arr_rch_cancela[c].total_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 5
          AND tpo_credito = arr_rch_cancela[c].tipo_credito
          AND identificador = "rch_cancelacion"
          AND sdo_deudor > 0;

      IF(arr_rch_cancela[c].total_sdo_deudor IS NULL) THEN
         LET arr_rch_cancela[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_rch_cancela[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_rch_cancela[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_rch_cancela[c].total_no_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 5
          AND tpo_credito = arr_rch_cancela[c].tipo_credito
          AND identificador = "rch_cancelacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_rch_cancela[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_rch_cancela[c-1].total_reg_global = v_suma_rch_c

      --Total deudores
      LET arr_rch_cancela[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_rch_cancela[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_rch_cancela[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_rch_cancela[arr_rch_cancela.getLength()].tipo_registro IS NULL) AND 
      (arr_rch_cancela[arr_rch_cancela.getLength()].tipo_credito IS NULL) AND 
      (arr_rch_cancela[arr_rch_cancela.getLength()].total_registros IS NULL)THEN

      CALL arr_rch_cancela.deleteElement(arr_rch_cancela.getLength())
      
   END IF 

#--------------------- Causal de rechazo originacion no exitosa (tipo registro: 1,4,20) -------------------------

   LET arr_causal_origina[1].tipo_registro          = 0
   LET arr_causal_origina[1].estado_rechazo         = 0
   LET arr_causal_origina[1].total_registros        = 0
   LET arr_causal_origina[1].total_reg_global       = 0 
   LET arr_causal_origina[1].prc_causal             = 0
   LET arr_causal_origina[1].prc_global             = 0
   LET arr_causal_origina[1].total_deudor           = 0
   LET arr_causal_origina[1].total_global_deudor    = 0
   LET arr_causal_origina[1].total_no_deudor        = 0
   LET arr_causal_origina[1].total_global_no_deudor = 0
   LET arr_causal_origina[1].total_sdo_deudor       = 0
   LET arr_causal_origina[1].total_global_saldo     = 0

   LET v_query_report = "SELECT clave_edo,
                                 edo_desc,
                                 COUNT(*)
                            FROM safre_tmp:tmp_rch_informe
                           WHERE tipo_registro  IN (1,4,20)
                             AND identificador = 'rch_originacion'
                             GROUP BY 1,2
                             ORDER BY 3 DESC;"


   PREPARE prp_causal_origina FROM v_query_report 
   DECLARE crs_causal_origina CURSOR FOR prp_causal_origina
   
   LET c = 1
   LET v_suma_causal_o  = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_causal_origina INTO  arr_causal_origina[c].estado_rechazo,
                                     arr_causal_origina[c].desc_estado,
                                     arr_causal_origina[c].total_registros
                                    
      LET arr_causal_origina[c].concatena_causal =  arr_causal_origina[c].estado_rechazo USING "&&" CLIPPED,"-",
                                                     arr_causal_origina[c].desc_estado CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_causal_o = v_suma_causal_o + arr_causal_origina[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_causal_origina[c].total_sdo_deudor,
               arr_causal_origina[c].total_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro IN (1,4,20)
          AND clave_edo = arr_causal_origina[c].estado_rechazo
          AND identificador = "rch_originacion"
          AND sdo_deudor > 0;

      IF(arr_causal_origina[c].total_sdo_deudor IS NULL) THEN
         LET arr_causal_origina[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_causal_origina[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_causal_origina[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_causal_origina[c].total_no_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro IN (1,4,20)
          AND clave_edo = arr_causal_origina[c].estado_rechazo
          AND identificador = "rch_originacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_causal_origina[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_causal_origina[c-1].total_reg_global = v_suma_causal_o

      --Total deudores
      LET arr_causal_origina[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_causal_origina[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_causal_origina[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_causal_origina[arr_causal_origina.getLength()].tipo_registro IS NULL) AND 
      (arr_causal_origina[arr_causal_origina.getLength()].estado_rechazo IS NULL) THEN

      CALL arr_causal_origina.deleteElement(arr_causal_origina.getLength())
      
   END IF 

#--------------------- Causal de rechazo reactivación no exitosa (tipo registro: 4) -------------------------

   LET arr_causal_reactiva[1].tipo_registro          = 0
   LET arr_causal_reactiva[1].estado_rechazo         = 0
   LET arr_causal_reactiva[1].total_registros        = 0
   LET arr_causal_reactiva[1].total_reg_global       = 0 
   LET arr_causal_reactiva[1].prc_causal             = 0
   LET arr_causal_reactiva[1].prc_global             = 0
   LET arr_causal_reactiva[1].total_deudor           = 0
   LET arr_causal_reactiva[1].total_global_deudor    = 0
   LET arr_causal_reactiva[1].total_no_deudor        = 0
   LET arr_causal_reactiva[1].total_global_no_deudor = 0
   LET arr_causal_reactiva[1].total_sdo_deudor       = 0
   LET arr_causal_reactiva[1].total_global_saldo     = 0

   LET v_query_report = "SELECT clave_edo,
                                 edo_desc,
                                 COUNT(*)
                            FROM safre_tmp:tmp_rch_informe
                           WHERE tipo_registro = 4
                             AND identificador = 'rch_originacion'
                             GROUP BY 1,2
                             ORDER BY 3 DESC;"


   PREPARE prp_causal_reactiva FROM v_query_report 
   DECLARE crs_causal_reactiva CURSOR FOR prp_causal_reactiva
   
   LET c = 1
   LET v_suma_causal_r  = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_causal_reactiva INTO arr_causal_reactiva[c].estado_rechazo,
                                     arr_causal_reactiva[c].desc_estado,
                                     arr_causal_reactiva[c].total_registros
                                    
      LET arr_causal_reactiva[c].concatena_causal = arr_causal_reactiva[c].estado_rechazo USING "&&" CLIPPED,"-",
                                                     arr_causal_reactiva[c].desc_estado CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_causal_r = v_suma_causal_r + arr_causal_reactiva[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_causal_reactiva[c].total_sdo_deudor,
               arr_causal_reactiva[c].total_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 4
          AND clave_edo = arr_causal_reactiva[c].estado_rechazo
          AND identificador = "rch_originacion"
          AND sdo_deudor > 0;

      IF(arr_causal_reactiva[c].total_sdo_deudor IS NULL) THEN
         LET arr_causal_reactiva[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_causal_reactiva[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_causal_reactiva[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_causal_reactiva[c].total_no_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 4
          AND clave_edo = arr_causal_reactiva[c].estado_rechazo
          AND identificador = "rch_originacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_causal_reactiva[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_causal_reactiva[c-1].total_reg_global = v_suma_causal_r

      --Total deudores
      LET arr_causal_reactiva[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_causal_reactiva[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_causal_reactiva[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_causal_reactiva[arr_causal_reactiva.getLength()].tipo_registro IS NULL) AND 
      (arr_causal_reactiva[arr_causal_reactiva.getLength()].estado_rechazo IS NULL) AND 
      (arr_causal_reactiva[arr_causal_reactiva.getLength()].total_registros IS NULL) THEN

      CALL arr_causal_reactiva.deleteElement(arr_causal_reactiva.getLength())
      
   END IF 

#--------------------- Causal de rechazo liquidacion no exitosa (tipo registro: 11) -------------------------

   LET arr_causal_liquida[1].tipo_registro          = 0
   LET arr_causal_liquida[1].estado_rechazo         = 0
   LET arr_causal_liquida[1].total_registros        = 0
   LET arr_causal_liquida[1].total_reg_global       = 0 
   LET arr_causal_liquida[1].prc_causal             = 0
   LET arr_causal_liquida[1].prc_global             = 0
   LET arr_causal_liquida[1].total_deudor           = 0
   LET arr_causal_liquida[1].total_global_deudor    = 0
   LET arr_causal_liquida[1].total_no_deudor        = 0
   LET arr_causal_liquida[1].total_global_no_deudor = 0
   LET arr_causal_liquida[1].total_sdo_deudor       = 0
   LET arr_causal_liquida[1].total_global_saldo     = 0

   LET v_query_report = "SELECT clave_edo,
                                 edo_desc,
                                 COUNT(*)
                            FROM safre_tmp:tmp_rch_informe
                           WHERE tipo_registro = 11
                             AND identificador = 'rch_liquidacion'
                             GROUP BY 1,2
                             ORDER BY 3 DESC;"


   PREPARE prp_causal_liquida FROM v_query_report 
   DECLARE crs_causal_liquida CURSOR FOR prp_causal_liquida
   
   LET c = 1
   LET v_suma_causal_l  = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_causal_liquida INTO arr_causal_liquida[c].estado_rechazo,
                                     arr_causal_liquida[c].desc_estado,
                                     arr_causal_liquida[c].total_registros
                                    
      LET arr_causal_liquida[c].concatena_causal = arr_causal_liquida[c].estado_rechazo USING "&&" CLIPPED,"-",
                                                    arr_causal_liquida[c].desc_estado CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_causal_l = v_suma_causal_l + arr_causal_liquida[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_causal_liquida[c].total_sdo_deudor,
               arr_causal_liquida[c].total_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 11
          AND clave_edo = arr_causal_liquida[c].estado_rechazo
          AND identificador = "rch_liquidacion"
          AND sdo_deudor > 0;

      IF(arr_causal_liquida[c].total_sdo_deudor IS NULL) THEN
         LET arr_causal_liquida[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_causal_liquida[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_causal_liquida[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_causal_liquida[c].total_no_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 11
          AND clave_edo = arr_causal_liquida[c].estado_rechazo
          AND identificador = "rch_liquidacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_causal_liquida[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_causal_liquida[c-1].total_reg_global = v_suma_causal_l

      --Total deudores
      LET arr_causal_liquida[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_causal_liquida[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_causal_liquida[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_causal_liquida[arr_causal_liquida.getLength()].tipo_registro IS NULL) AND 
      (arr_causal_liquida[arr_causal_liquida.getLength()].estado_rechazo IS NULL) THEN

      CALL arr_causal_liquida.deleteElement(arr_causal_liquida.getLength())
      
   END IF 

#--------------------- Causal de rechazo cancelacion no exitosa (tipo registro: 5) -------------------------

   LET arr_causal_cancela[1].tipo_registro          = 0
   LET arr_causal_cancela[1].estado_rechazo         = 0
   LET arr_causal_cancela[1].total_registros        = 0
   LET arr_causal_cancela[1].total_reg_global       = 0 
   LET arr_causal_cancela[1].prc_causal             = 0
   LET arr_causal_cancela[1].prc_global             = 0
   LET arr_causal_cancela[1].total_deudor           = 0
   LET arr_causal_cancela[1].total_global_deudor    = 0
   LET arr_causal_cancela[1].total_no_deudor        = 0
   LET arr_causal_cancela[1].total_global_no_deudor = 0
   LET arr_causal_cancela[1].total_sdo_deudor       = 0
   LET arr_causal_cancela[1].total_global_saldo     = 0

   LET v_query_report = "SELECT clave_edo,
                                 edo_desc,
                                 COUNT(*)
                            FROM safre_tmp:tmp_rch_informe
                           WHERE tipo_registro = 5
                             AND identificador = 'rch_cancelacion'
                             GROUP BY 1,2
                             ORDER BY 3 DESC;"


   PREPARE prp_causal_cancela FROM v_query_report 
   DECLARE crs_causal_cancela CURSOR FOR prp_causal_cancela
   
   LET c = 1
   LET v_suma_causal_c  = 0
   LET v_suma_deudor    = 0
   LET v_suma_saldo     = 0
   LET v_suma_no_deudor = 0

   FOREACH crs_causal_cancela INTO arr_causal_cancela[c].estado_rechazo,
                                     arr_causal_cancela[c].desc_estado,
                                     arr_causal_cancela[c].total_registros
                                    
      LET arr_causal_cancela[c].concatena_causal = arr_causal_cancela[c].estado_rechazo USING "&&" CLIPPED,"-",
                                                    arr_causal_cancela[c].desc_estado CLIPPED
 
      -- Obtiene total de registros
      LET v_suma_causal_c = v_suma_causal_c + arr_causal_cancela[c].total_registros

      -- Obtiene total deudores y su saldo
      SELECT SUM(sdo_deudor),COUNT(*)
         INTO arr_causal_cancela[c].total_sdo_deudor,
               arr_causal_cancela[c].total_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 5
          AND clave_edo = arr_causal_cancela[c].estado_rechazo
          AND identificador = "rch_cancelacion"
          AND sdo_deudor > 0;

      IF(arr_causal_cancela[c].total_sdo_deudor IS NULL) THEN
         LET arr_causal_cancela[c].total_sdo_deudor = 0.00
      END IF 

      -- Suma total de deudores
      LET v_suma_deudor = v_suma_deudor + arr_causal_cancela[c].total_deudor

      -- suma saldo deudor
      LET v_suma_saldo = v_suma_saldo + arr_causal_cancela[c].total_sdo_deudor

      -- Obtiene total No deudores
      SELECT COUNT(*)
         INTO arr_causal_cancela[c].total_no_deudor
         FROM safre_tmp:tmp_rch_informe
        WHERE tipo_registro = 5
          AND clave_edo = arr_causal_cancela[c].estado_rechazo
          AND identificador = "rch_cancelacion"
          AND (sdo_deudor = 0
           OR  sdo_deudor IS NULL 
           OR  sdo_deudor = '')
           
      -- Suma total no deudores
      LET v_suma_no_deudor = v_suma_no_deudor + arr_causal_cancela[c].total_no_deudor
      
      LET c = c + 1
      
   END FOREACH 

   --Si el arreglo contiene registros
   IF(c > 1) THEN
      -- Total registros 
      LET arr_causal_cancela[c-1].total_reg_global = v_suma_causal_c

      --Total deudores
      LET arr_causal_cancela[c-1].total_global_deudor = v_suma_deudor

      -- Total saldo deudores
      LET arr_causal_cancela[c-1].total_global_saldo = v_suma_saldo

      -- Total no deudor
      LET arr_causal_cancela[c-1].total_global_no_deudor = v_suma_no_deudor
      
   END IF 

   -- Elimina la última fila en blanco
   IF (arr_causal_cancela[arr_causal_cancela.getLength()].tipo_registro IS NULL) AND 
      (arr_causal_cancela[arr_causal_cancela.getLength()].estado_rechazo IS NULL) AND 
      (arr_causal_cancela[arr_causal_cancela.getLength()].total_registros IS NULL)THEN

      CALL arr_causal_cancela.deleteElement(arr_causal_cancela.getLength())
      
   END IF 

#--------------------- Configuración del reporte --------------------------
   # Carga del Reporte PDF
   LET v_ruta_reporte = v_ruta_listados CLIPPED,"/",
                        g_usuario CLIPPED,"-",
                        "AGRS12","-",
                        p_pid USING "&&&&&","-",
                        p_proceso_cod USING "&&&&&","-",
                        p_opera_cod USING "&&&&&",".pdf"
    
   IF (fgl_report_loadCurrentSettings(v_reporte)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()

      IF (reporte IS NOT NULL) THEN

         LET v_indicador = 0
          
         START REPORT resultados TO XML HANDLER reporte

            OUTPUT TO REPORT resultados()

         FINISH REPORT resultados  

      END IF 

   END IF 

   RETURN v_indicador
   
END FUNCTION 

REPORT resultados ()

   DEFINE z            INTEGER
   DEFINE v_porcentaje DECIMAL(4,1)--SMALLINT se había puesto así para manejar puros enteros.
   DEFINE v_total_prc  DECIMAL(4,0)--SMALLINT 
 
   FORMAT 

      FIRST PAGE HEADER 
        PRINTX g_usuario
        PRINTX p_fecha_ejecuta USING "dd/mm/yyyy"

     ON EVERY ROW 

         # ORIGINACIONES
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  

         FOR z = 1 TO arr_origen.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_origen[z].total_registros / v_suma_reg) * 100
            LET arr_origen[z].prc_credito  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_origen[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_origen[z].concatena_credito
            PRINTX arr_origen[z].total_registros
            PRINTX arr_origen[z].total_reg_global -- total de registros
            PRINTX arr_origen[z].prc_credito
            PRINTX arr_origen[z].prc_global
            PRINTX arr_origen[z].total_deudor
            PRINTX arr_origen[z].total_global_deudor
            PRINTX arr_origen[z].total_no_deudor
            PRINTX arr_origen[z].total_global_no_deudor
            PRINTX arr_origen[z].total_sdo_deudor
            PRINTX arr_origen[z].total_global_saldo
         END FOR 

         # LIQUIDACIONES
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  

         FOR z = 1 TO arr_liquida.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_liquida[z].total_registros / v_suma_reg_lq) * 100
            LET arr_liquida[z].prc_credito  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

             -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
         
            LET arr_liquida[z].prc_global = v_total_prc  CLIPPED,"%"
            
            PRINTX arr_liquida[z].concatena_credito
            PRINTX arr_liquida[z].total_registros
            PRINTX arr_liquida[z].total_reg_global -- total de registros
            PRINTX arr_liquida[z].prc_credito
            PRINTX arr_liquida[z].prc_global
            PRINTX arr_liquida[z].total_deudor
            PRINTX arr_liquida[z].total_global_deudor
            PRINTX arr_liquida[z].total_no_deudor
            PRINTX arr_liquida[z].total_global_no_deudor
            PRINTX arr_liquida[z].total_sdo_deudor
            PRINTX arr_liquida[z].total_global_saldo
            
         END FOR 

         # CANCELACIONES
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  

         FOR z = 1 TO arr_cancela.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_cancela[z].total_registros / v_suma_reg_c) * 100
            LET arr_cancela[z].prc_credito  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

             -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_cancela[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_cancela[z].concatena_credito
            PRINTX arr_cancela[z].total_registros
            PRINTX arr_cancela[z].total_reg_global -- total de registros
            PRINTX arr_cancela[z].prc_credito
            PRINTX arr_cancela[z].prc_global
            PRINTX arr_cancela[z].total_deudor
            PRINTX arr_cancela[z].total_global_deudor
            PRINTX arr_cancela[z].total_no_deudor
            PRINTX arr_cancela[z].total_global_no_deudor
            PRINTX arr_cancela[z].total_sdo_deudor
            PRINTX arr_cancela[z].total_global_saldo
            
         END FOR 

          # REACTIVACIONES
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  

         FOR z = 1 TO arr_reactiva.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_reactiva[z].total_registros / v_suma_reg_r) * 100
            LET arr_reactiva[z].prc_credito  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF

            LET arr_reactiva[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_reactiva[z].concatena_credito
            PRINTX arr_reactiva[z].total_registros
            PRINTX arr_reactiva[z].total_reg_global -- total de registros
            PRINTX arr_reactiva[z].prc_credito
            PRINTX arr_reactiva[z].prc_global
            PRINTX arr_reactiva[z].total_deudor
            PRINTX arr_reactiva[z].total_global_deudor
            PRINTX arr_reactiva[z].total_no_deudor
            PRINTX arr_reactiva[z].total_global_no_deudor
            PRINTX arr_reactiva[z].total_sdo_deudor
            PRINTX arr_reactiva[z].total_global_saldo
            
         END FOR 

         
          # Rechazos originaciones
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  

         FOR z = 1 TO arr_rch_origina.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_rch_origina[z].total_registros / v_suma_rch_o) * 100
            LET arr_rch_origina[z].prc_credito  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_rch_origina[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_rch_origina[z].concatena_credito
            PRINTX arr_rch_origina[z].total_registros
            PRINTX arr_rch_origina[z].total_reg_global -- total de registros
            PRINTX arr_rch_origina[z].prc_credito
            PRINTX arr_rch_origina[z].prc_global
            PRINTX arr_rch_origina[z].total_deudor
            PRINTX arr_rch_origina[z].total_global_deudor
            PRINTX arr_rch_origina[z].total_no_deudor
            PRINTX arr_rch_origina[z].total_global_no_deudor
            PRINTX arr_rch_origina[z].total_sdo_deudor
            PRINTX arr_rch_origina[z].total_global_saldo
            
         END FOR

         # Rechazos reactivaciones
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0

         FOR z = 1 TO arr_rch_reactiva.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_rch_reactiva[z].total_registros / v_suma_rch_r) * 100
            LET arr_rch_reactiva[z].prc_credito  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_rch_reactiva[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_rch_reactiva[z].concatena_credito
            PRINTX arr_rch_reactiva[z].total_registros
            PRINTX arr_rch_reactiva[z].total_reg_global -- total de registros
            PRINTX arr_rch_reactiva[z].prc_credito
            PRINTX arr_rch_reactiva[z].prc_global
            PRINTX arr_rch_reactiva[z].total_deudor
            PRINTX arr_rch_reactiva[z].total_global_deudor
            PRINTX arr_rch_reactiva[z].total_no_deudor
            PRINTX arr_rch_reactiva[z].total_global_no_deudor
            PRINTX arr_rch_reactiva[z].total_sdo_deudor
            PRINTX arr_rch_reactiva[z].total_global_saldo
            
         END FOR

         # Rechazos liquidaciones
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  
         
         FOR z = 1 TO arr_rch_liquida.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_rch_liquida[z].total_registros / v_suma_rch_l) * 100
            LET arr_rch_liquida[z].prc_credito  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_rch_liquida[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_rch_liquida[z].concatena_credito
            PRINTX arr_rch_liquida[z].total_registros
            PRINTX arr_rch_liquida[z].total_reg_global -- total de registros
            PRINTX arr_rch_liquida[z].prc_credito
            PRINTX arr_rch_liquida[z].prc_global
            PRINTX arr_rch_liquida[z].total_deudor
            PRINTX arr_rch_liquida[z].total_global_deudor
            PRINTX arr_rch_liquida[z].total_no_deudor
            PRINTX arr_rch_liquida[z].total_global_no_deudor
            PRINTX arr_rch_liquida[z].total_sdo_deudor
            PRINTX arr_rch_liquida[z].total_global_saldo
            
         END FOR  

         # Rechazos cancelaciones
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0 

         FOR z = 1 TO arr_rch_cancela.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_rch_cancela[z].total_registros / v_suma_rch_c) * 100
            LET arr_rch_cancela[z].prc_credito  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_rch_cancela[z].prc_global =  v_total_prc  CLIPPED,"%"
            
            PRINTX arr_rch_cancela[z].concatena_credito
            PRINTX arr_rch_cancela[z].total_registros
            PRINTX arr_rch_cancela[z].total_reg_global -- total de registros
            PRINTX arr_rch_cancela[z].prc_credito
            PRINTX arr_rch_cancela[z].prc_global
            PRINTX arr_rch_cancela[z].total_deudor
            PRINTX arr_rch_cancela[z].total_global_deudor
            PRINTX arr_rch_cancela[z].total_no_deudor
            PRINTX arr_rch_cancela[z].total_global_no_deudor
            PRINTX arr_rch_cancela[z].total_sdo_deudor
            PRINTX arr_rch_cancela[z].total_global_saldo
            
         END FOR  

         # Causal de rechazo originaciones
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  

         FOR z = 1 TO arr_causal_origina.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_causal_origina[z].total_registros / v_suma_causal_o) * 100
            LET arr_causal_origina[z].prc_causal  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_causal_origina[z].prc_global =  v_total_prc  CLIPPED,"%"
            
            PRINTX arr_causal_origina[z].concatena_causal
            PRINTX arr_causal_origina[z].total_registros
            PRINTX arr_causal_origina[z].total_reg_global -- total de registros
            PRINTX arr_causal_origina[z].prc_causal
            PRINTX arr_causal_origina[z].prc_global
            PRINTX arr_causal_origina[z].total_deudor
            PRINTX arr_causal_origina[z].total_global_deudor
            PRINTX arr_causal_origina[z].total_no_deudor
            PRINTX arr_causal_origina[z].total_global_no_deudor
            PRINTX arr_causal_origina[z].total_sdo_deudor
            PRINTX arr_causal_origina[z].total_global_saldo
            
         END FOR 

         # Causal de rechazo reactivaciones
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  

         FOR z = 1 TO arr_causal_reactiva.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_causal_reactiva[z].total_registros / v_suma_causal_r) * 100
            LET arr_causal_reactiva[z].prc_causal  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_causal_reactiva[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_causal_reactiva[z].concatena_causal
            PRINTX arr_causal_reactiva[z].total_registros
            PRINTX arr_causal_reactiva[z].total_reg_global -- total de registros
            PRINTX arr_causal_reactiva[z].prc_causal
            PRINTX arr_causal_reactiva[z].prc_global
            PRINTX arr_causal_reactiva[z].total_deudor
            PRINTX arr_causal_reactiva[z].total_global_deudor
            PRINTX arr_causal_reactiva[z].total_no_deudor
            PRINTX arr_causal_reactiva[z].total_global_no_deudor
            PRINTX arr_causal_reactiva[z].total_sdo_deudor
            PRINTX arr_causal_reactiva[z].total_global_saldo
            
         END FOR 

          # Causal de rechazo liquidaciones
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0 

         FOR z = 1 TO arr_causal_liquida.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_causal_liquida[z].total_registros / v_suma_causal_l) * 100
            LET arr_causal_liquida[z].prc_causal  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_causal_liquida[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_causal_liquida[z].concatena_causal
            PRINTX arr_causal_liquida[z].total_registros
            PRINTX arr_causal_liquida[z].total_reg_global -- total de registros
            PRINTX arr_causal_liquida[z].prc_causal
            PRINTX arr_causal_liquida[z].prc_global
            PRINTX arr_causal_liquida[z].total_deudor
            PRINTX arr_causal_liquida[z].total_global_deudor
            PRINTX arr_causal_liquida[z].total_no_deudor
            PRINTX arr_causal_liquida[z].total_global_no_deudor
            PRINTX arr_causal_liquida[z].total_sdo_deudor
            PRINTX arr_causal_liquida[z].total_global_saldo
            
         END FOR 

         # Causal de rechazo cancelaciones
         -- Inicializa variables
         LET v_porcentaje = 0
         LET v_total_prc  = 0  

         FOR z = 1 TO arr_causal_cancela.getLength()

            -- Porcentaje por crédito
            LET v_porcentaje = (arr_causal_cancela[z].total_registros / v_suma_causal_c) * 100
            LET arr_causal_cancela[z].prc_causal  = v_porcentaje CLIPPED,"%"

            -- Porcentaje gobal
            LET v_total_prc = v_total_prc + v_porcentaje

            -- Redondeando el porcentaje global a 100
            IF(v_total_prc > 95) THEN
               LET v_total_prc = 100 
            END IF
            
            LET arr_causal_cancela[z].prc_global = v_total_prc CLIPPED,"%"
            
            PRINTX arr_causal_cancela[z].concatena_causal
            PRINTX arr_causal_cancela[z].total_registros
            PRINTX arr_causal_cancela[z].total_reg_global -- total de registros
            PRINTX arr_causal_cancela[z].prc_causal
            PRINTX arr_causal_cancela[z].prc_global
            PRINTX arr_causal_cancela[z].total_deudor
            PRINTX arr_causal_cancela[z].total_global_deudor
            PRINTX arr_causal_cancela[z].total_no_deudor
            PRINTX arr_causal_cancela[z].total_global_no_deudor
            PRINTX arr_causal_cancela[z].total_sdo_deudor
            PRINTX arr_causal_cancela[z].total_global_saldo
            
         END FOR 

END REPORT

FUNCTION fn_gen_extractor()

   DEFINE ch               base.channel
   DEFINE v_query_ext      STRING
   DEFINE v_arh_salida     STRING 
   DEFINE v_retorno        BOOLEAN 
   DEFINE k                INTEGER
   DEFINE r                INTEGER 
   DEFINE cont_rch         INTEGER 
   DEFINE v_detalle        STRING
   DEFINE v_filler         CHAR(34)
   DEFINE i                INTEGER
   DEFINE cont_arh         INTEGER   
   DEFINE v_id_archivo     DECIMAL(9,0)

   DEFINE r_recupera_datos RECORD
      entidad             CHAR(2),
      nss                 CHAR(11),
      id_derechohabiente  CHAR(9),
      num_credito         CHAR(10),
      tpo_credito         CHAR(3),
      desc_credito        CHAR(30),
      estado_credito      CHAR(3),
      sdo_deudor          CHAR(14),
      f_otorga            DATE,
      f_proceso           DATE,
      clave               CHAR(3),
      clave_desc          CHAR(40),
      identificador       CHAR(20)
   END RECORD 

   DEFINE r_rechazos RECORD
      tipo_registro       CHAR(2),
      nss                 CHAR(11),
      num_credito         CHAR(10),
      tpo_credito         CHAR(3),
      desc_credito        CHAR(30),
      estado_credito      CHAR(3),
      sdo_deudor          CHAR(14),
      f_otorga            DATE,
      f_proceso           DATE,
      clave_edo           CHAR(3),
      edo_desc            CHAR(40),
      identificador       CHAR(20)
   END RECORD 

   LET v_arh_salida = v_ruta_envio CLIPPED,"/Recurrente_informe_",TODAY USING "yyyymmdd",".txt" 
   LET v_retorno = 0 -- Bandera extractor no se ha generado

   -- crea tablas temporales
   CALL crea_temporales()
   
   LET ch = base.Channel.create()

   -- Abre archivo de salida para escritura
   CALL ch.openFile(v_arh_salida,"w")
   
   # Recupera los ID´S de lor archivos procesados del día anterior
   # Por lo general los archivos son cargados en 2 procesos:
     --> Solicitud de Desmarca AG
     --> Recepción recurrente originación AG
     
   LET v_query_ext = "SELECT UNIQUE id_cre_ctr_archivo
                        FROM cre_ctr_archivo
                       WHERE id_proceso = 301
                         AND nom_archivo NOT MATCHES 'ORIGINA_MF*'
                         AND f_proceso = ",'"',p_fecha_ejecuta,'"'

   PREPARE prp_id_archivos FROM v_query_ext
   DECLARE crs_id_archivos CURSOR FOR prp_id_archivos

   LET v_id_archivo = NULL 
   LET cont_arh = 1
   LET k = 1
   
   INITIALIZE r_recupera_datos.* TO NULL 
   
   FOREACH crs_id_archivos INTO v_id_archivo

      ########################################
      ## Registros Solicitud de desmarca AG ##
      ########################################

      --Recupera Liquidaciones aceptadas y cancelaciones aceptadas
      DECLARE crs_sol_desmarca CURSOR FOR 
         SELECT m.entidad,
                 d.nss,
                 c.id_derechohabiente,
                 c.num_credito,
                 c.tpo_credito,
                 t.desc_credito,
                 c.edo_credito ,
                 c.sdo_deudor ,
                 c.f_otorga,
                 '' f_proceso,
                 '000'clave,
                 'ACEPTADO' clave_desc,
                 '' identifica_entidad
            FROM cre_acreditado c,
                 afi_derechohabiente d,
                 cre_his_acreditado h,
                 cat_maq_credito m,
                 cat_tipo_credito t
           WHERE c.id_derechohabiente = d.id_derechohabiente
             AND c.id_cre_acreditado  = h.id_cre_acreditado
             AND h.id_cre_ctr_archivo = v_id_archivo
             AND c.estado = m.estado
             AND m.entidad IN (2,5)
             AND c.tpo_credito = t.tpo_credito
             AND c.tpo_originacion = t.tpo_originacion
           ORDER BY m.entidad;

      --Limpia record de recuperación de datos
      INITIALIZE r_recupera_datos.* TO NULL
      LET v_detalle = NULL 

      FOREACH crs_sol_desmarca INTO r_recupera_datos.entidad,
                                     r_recupera_datos.nss,
                                     r_recupera_datos.id_derechohabiente,
                                     r_recupera_datos.num_credito,
                                     r_recupera_datos.tpo_credito,
                                     r_recupera_datos.desc_credito,
                                     r_recupera_datos.estado_credito,
                                     r_recupera_datos.sdo_deudor,
                                     r_recupera_datos.f_otorga,
                                     r_recupera_datos.f_proceso,
                                     r_recupera_datos.clave,
                                     r_recupera_datos.clave_desc,
                                     r_recupera_datos.identificador
          --coloca identificador
         IF(r_recupera_datos.entidad = 2) THEN
            LET r_recupera_datos.identificador = "liquidacion"
         END IF 
         
         --coloca identificador
         IF(r_recupera_datos.entidad = 5) THEN
            LET r_recupera_datos.identificador = "cancelacion" 
         END IF 

         LET v_detalle =  r_recupera_datos.entidad CLIPPED,"|",
                          r_recupera_datos.nss,"|",
                          r_recupera_datos.num_credito CLIPPED,"|",
                          r_recupera_datos.tpo_credito CLIPPED,"|",
                          r_recupera_datos.estado_credito CLIPPED,"|",
                          r_recupera_datos.sdo_deudor CLIPPED,"|",
                          r_recupera_datos.sdo_deudor CLIPPED,"|",
                          r_recupera_datos.f_otorga USING "yyyymmdd","|",
                          p_fecha_ejecuta USING "yyyymmdd","|",
                          r_recupera_datos.clave,"|",
                          r_recupera_datos.clave_desc CLIPPED,"|"

         CALL ch.writeLine(v_detalle)
         
         -- guarda registro en temporal para el reporte PDF
         INSERT INTO safre_tmp:tmp_rec_informe
            VALUES (r_recupera_datos.entidad,
                     r_recupera_datos.nss,
                     r_recupera_datos.id_derechohabiente,
                     r_recupera_datos.tpo_credito,
                     r_recupera_datos.desc_credito,
                     r_recupera_datos.sdo_deudor,
                     r_recupera_datos.identificador);
                     
         LET k = k + 1
      
      END FOREACH 

      ########################################
      ##     Registros Originaciones AG     ##
      ########################################
      #Recupera Originaciones aceptadas

      DECLARE crs_aceptados CURSOR FOR 
         SELECT m.entidad,
                d.nss,
                c.id_derechohabiente,
                c.num_credito,
                c.tpo_credito,
                t.desc_credito,
                c.edo_credito ,
                c.sdo_deudor ,
                c.f_otorga,
                '' f_proceso,
                '000'clave,
                'ACEPTADO' clave_desc,
                '' identifica_entidad
           FROM cre_acreditado c,
                cat_maq_credito m,
                cat_tipo_credito t,
                afi_derechohabiente d
         WHERE c.id_derechohabiente = d.id_derechohabiente
           AND c.id_cre_ctr_archivo = v_id_archivo
           AND c.estado = m.estado
           AND m.entidad IN (1,2,5)
           AND c.tpo_credito = t.tpo_credito
           AND c.tpo_originacion = t.tpo_originacion
           ORDER BY m.entidad;

      INITIALIZE r_recupera_datos.* TO NULL
      LET k = 1  --INicializa contador
      LET v_detalle = NULL
      
      FOREACH crs_aceptados INTO  r_recupera_datos.entidad,
                                   r_recupera_datos.nss,
                                   r_recupera_datos.id_derechohabiente,
                                   r_recupera_datos.num_credito,
                                   r_recupera_datos.tpo_credito,
                                   r_recupera_datos.desc_credito,
                                   r_recupera_datos.estado_credito,
                                   r_recupera_datos.sdo_deudor,
                                   r_recupera_datos.f_otorga,
                                   r_recupera_datos.f_proceso,
                                   r_recupera_datos.clave,
                                   r_recupera_datos.clave_desc,
                                   r_recupera_datos.identificador

         --Coloca identificador
         IF(r_recupera_datos.entidad = 1) THEN
            LET r_recupera_datos.identificador = "originacion" 
         END IF 
         
         --coloca identificador
         IF(r_recupera_datos.entidad = 2) THEN
            LET r_recupera_datos.identificador = "liquidacion"
         END IF 
         
         --coloca identificador
         IF(r_recupera_datos.entidad = 5) THEN
            LET r_recupera_datos.identificador = "cancelacion" 
         END IF 
         
         
         LET v_detalle =  r_recupera_datos.entidad CLIPPED,"|",
                          r_recupera_datos.nss,"|",
                          r_recupera_datos.num_credito CLIPPED,"|",
                          r_recupera_datos.tpo_credito CLIPPED,"|",
                          r_recupera_datos.estado_credito CLIPPED,"|",
                          r_recupera_datos.sdo_deudor CLIPPED,"|",
                          r_recupera_datos.sdo_deudor CLIPPED,"|",
                          r_recupera_datos.f_otorga USING "yyyymmdd","|",
                          p_fecha_ejecuta USING "yyyymmdd","|",
                          r_recupera_datos.clave,"|",
                          r_recupera_datos.clave_desc CLIPPED,"|"
         
         CALL ch.writeLine(v_detalle)
         
         -- guarda registro en temporal para el reporte PDF
         INSERT INTO safre_tmp:tmp_rec_informe
            VALUES (r_recupera_datos.entidad,
                     r_recupera_datos.nss,
                     r_recupera_datos.id_derechohabiente,
                     r_recupera_datos.tpo_credito,
                     r_recupera_datos.desc_credito,
                     r_recupera_datos.sdo_deudor,
                     r_recupera_datos.identificador);
                     
         LET k = k + 1 --contador para registros aceptados
         
      END FOREACH 

      --Inicializa el record para las Reactivaciones
      INITIALIZE r_recupera_datos.* TO NULL

      DECLARE crs_reactivaciones CURSOR FOR 
         SELECT '1',
                 d.nss,
                 c.id_derechohabiente,
                 c.num_credito,
                 c.tpo_credito,
                 t.desc_credito,
                 c.edo_credito ,
                 c.sdo_deudor ,
                 c.f_otorga,
                 a.f_proceso,
                 '000' clave,
                 'ACEPTADO' clave_desc,
                 'reactivacion' identifica_entidad
            FROM cre_ctr_archivo a,
                 cat_tipo_credito t,
                 cre_his_acreditado h,
                 cre_acreditado c,
                 afi_derechohabiente d
          WHERE a.id_cre_ctr_archivo = h.id_cre_ctr_archivo
            AND a.id_proceso = 301
            AND h.id_cre_acreditado > 0
            AND h.estado IN (20,140)
            AND h.id_cre_acreditado = c.id_cre_acreditado
            AND c.id_derechohabiente = d.id_derechohabiente
            AND c.tpo_credito = t.tpo_credito
            AND c.tpo_originacion = t.tpo_originacion
            AND a.nom_archivo MATCHES '*dma*'
            AND a.f_proceso = p_fecha_ejecuta;

     LET r = 1

     FOREACH crs_reactivaciones INTO r_recupera_datos.entidad,
                                        r_recupera_datos.nss,
                                        r_recupera_datos.id_derechohabiente,
                                        r_recupera_datos.num_credito,
                                        r_recupera_datos.tpo_credito,
                                        r_recupera_datos.desc_credito,
                                        r_recupera_datos.estado_credito,
                                        r_recupera_datos.sdo_deudor,
                                        r_recupera_datos.f_otorga,
                                        r_recupera_datos.f_proceso,
                                        r_recupera_datos.clave,
                                        r_recupera_datos.clave_desc,
                                        r_recupera_datos.identificador

         LET v_detalle =  r_recupera_datos.entidad CLIPPED,"|",
                          r_recupera_datos.nss,"|",
                          r_recupera_datos.num_credito CLIPPED,"|",
                          r_recupera_datos.tpo_credito CLIPPED,"|",
                          r_recupera_datos.estado_credito CLIPPED,"|",
                          r_recupera_datos.sdo_deudor CLIPPED,"|",
                          r_recupera_datos.sdo_deudor CLIPPED,"|",
                          r_recupera_datos.f_otorga USING "yyyymmdd","|",
                          r_recupera_datos.f_proceso USING "yyyymmdd","|",
                          r_recupera_datos.clave,"|",
                          r_recupera_datos.clave_desc CLIPPED,"|"

         CALL ch.writeLine(v_detalle)

         -- guarda registro en temporal para las reactivaciones aceptadas
         INSERT INTO safre_tmp:tmp_rec_informe
            VALUES (r_recupera_datos.entidad,
                     r_recupera_datos.nss,
                     r_recupera_datos.id_derechohabiente,
                     r_recupera_datos.tpo_credito,
                     r_recupera_datos.desc_credito,
                     r_recupera_datos.sdo_deudor,
                     r_recupera_datos.identificador);
                                        
         LET r = r + 1 --contador para las reactivaciones aceptadas
         
      END FOREACH 

      ########################################
      ## RECHAZOS :                         ##
      ## Registros solicitud desmarca AG    ##
      ## Registros Originaciones AG         ##
      ########################################
      
      INITIALIZE r_rechazos.* TO NULL

      DECLARE crs_rechazos CURSOR FOR 
         SELECT h.tpo_registro,
                 h.nss,
                 h.num_credito,
                 h.tpo_credito,
                 t.desc_credito,
                 h.edo_credito,
                 h.sdo_deudor,
                 h.f_otorga,
                 ''f_proceso,
                 h.estado,
                 c.desc_estado,
                 ''identificador
            FROM cre_rch_acreditado h,
                 cat_tipo_credito t,
                 cat_rch_acreditado c
           WHERE h.id_cre_ctr_archivo = v_id_archivo
             AND h.tpo_credito = t.tpo_credito
             AND h.tpo_originacion = t.tpo_originacion
             AND h.estado = c.estado
             AND h.tpo_registro IN ('01','20','04','11','05')
             ORDER BY h.tpo_registro;

      LET cont_rch = 1
      
      FOREACH crs_rechazos INTO r_rechazos.tipo_registro,
                                 r_rechazos.nss,
                                 r_rechazos.num_credito,
                                 r_rechazos.tpo_credito,
                                 r_rechazos.desc_credito,
                                 r_rechazos.estado_credito,
                                 r_rechazos.sdo_deudor,
                                 r_rechazos.f_otorga,
                                 r_rechazos.f_proceso,
                                 r_rechazos.clave_edo,
                                 r_rechazos.edo_desc

         LET v_detalle = r_rechazos.tipo_registro CLIPPED,"|",
                         r_rechazos.nss,"|",
                         r_rechazos.num_credito CLIPPED,"|",
                         r_rechazos.tpo_credito CLIPPED,"|",
                         r_rechazos.estado_credito CLIPPED,"|",
                         r_rechazos.sdo_deudor CLIPPED,"|",
                         r_rechazos.sdo_deudor CLIPPED,"|",
                         r_rechazos.f_otorga USING "yyyymmdd","|",
                         p_fecha_ejecuta USING "yyyymmdd","|",
                         r_rechazos.clave_edo CLIPPED,"|",
                         r_rechazos.edo_desc CLIPPED,"|"

         IF(r_rechazos.tipo_registro = '01') OR 
           (r_rechazos.tipo_registro = '04') OR 
           (r_rechazos.tipo_registro = '20') THEN

           LET r_rechazos.identificador = "rch_originacion"  
           
         END IF

         IF(r_rechazos.tipo_registro = '11') THEN 

           LET r_rechazos.identificador = "rch_liquidacion"  
           
         END IF 

         IF(r_rechazos.tipo_registro = '05') THEN 

           LET r_rechazos.identificador = "rch_cancelacion"  
           
         END IF 

         --graba en el archivo de salida
         CALL ch.writeLine(v_detalle)

         -- guarda registro en temporal para las reactivaciones aceptadas
         INSERT INTO safre_tmp:tmp_rch_informe
            VALUES (r_rechazos.tipo_registro,
                     r_rechazos.nss,
                     r_rechazos.tpo_credito,
                     r_rechazos.desc_credito,
                     r_rechazos.sdo_deudor,
                     r_rechazos.clave_edo,
                     r_rechazos.edo_desc,
                     r_rechazos.identificador);

         LET cont_rch = cont_rch + 1 -- contador para los rechazos

      END FOREACH 

      --Rechazos para las originaciones con estado = 240 (RECHAZADA), también deben ser contempladas como rechazos
      DECLARE crs_rch_240_orig CURSOR FOR 
         SELECT c.tpo_registro, 
                d.nss,
                c.num_credito,
                c.tpo_credito,
                t.desc_credito,
                c.edo_credito ,
                c.sdo_deudor ,
                c.f_otorga,
                '' f_proceso,
                c.estado,
                m.estado_desc,
                '' identificador
          FROM cre_acreditado c,
                afi_derechohabiente d,
                cat_maq_credito m,
                cat_tipo_credito t
         WHERE c.id_derechohabiente = d.id_derechohabiente
           AND c.id_cre_ctr_archivo = v_id_archivo
           AND c.estado  = 240
           AND c.estado  = m.estado
           AND c.tpo_credito = t.tpo_credito
           AND c.tpo_originacion = t.tpo_originacion;

      LET cont_rch = 1
      INITIALIZE r_rechazos.* TO NULL
      LET v_detalle = NULL 

      FOREACH crs_rch_240_orig INTO r_rechazos.tipo_registro,
                                     r_rechazos.nss,
                                     r_rechazos.num_credito,
                                     r_rechazos.tpo_credito,
                                     r_rechazos.desc_credito,
                                     r_rechazos.estado_credito,
                                     r_rechazos.sdo_deudor,
                                     r_rechazos.f_otorga,
                                     r_rechazos.f_proceso,
                                     r_rechazos.clave_edo,
                                     r_rechazos.edo_desc

                                 
         LET v_detalle = r_rechazos.tipo_registro CLIPPED,"|",
                         r_rechazos.nss,"|",
                         r_rechazos.num_credito CLIPPED,"|",
                         r_rechazos.tpo_credito CLIPPED,"|",
                         r_rechazos.estado_credito CLIPPED,"|",
                         r_rechazos.sdo_deudor CLIPPED,"|",
                         r_rechazos.sdo_deudor CLIPPED,"|",
                         r_rechazos.f_otorga USING "yyyymmdd","|",
                         p_fecha_ejecuta USING "yyyymmdd","|",
                         r_rechazos.clave_edo CLIPPED,"|",
                         r_rechazos.edo_desc CLIPPED,"|"

         IF(r_rechazos.tipo_registro = '01') OR 
           (r_rechazos.tipo_registro = '04') OR 
           (r_rechazos.tipo_registro = '20') THEN

           LET r_rechazos.identificador = "rch_originacion"  
         END IF

          --graba en el archivo de salida
         CALL ch.writeLine(v_detalle)

         -- guarda registro en temporal para las reactivaciones aceptadas
         INSERT INTO safre_tmp:tmp_rch_informe
            VALUES (r_rechazos.tipo_registro,
                     r_rechazos.nss,
                     r_rechazos.tpo_credito,
                     r_rechazos.desc_credito,
                     r_rechazos.sdo_deudor,
                     r_rechazos.clave_edo,
                     r_rechazos.edo_desc,
                     r_rechazos.identificador);

         LET cont_rch = cont_rch + 1
         
      END FOREACH 
        
      LET cont_arh = cont_arh + 1 -- contador para los archivos procesados
      
   END FOREACH  

   -- Crea el filler e las 30 posiciones
   LET v_filler = " "

   FOR i=1 TO 33
      LET v_filler = v_filler," "
   END FOR 

    -- Escribe filler
   CALL ch.writeLine([v_filler])

   -- Cierra archivo
   CALL ch.close()
   
   LET v_retorno = 1  -- Extractor generado
   
   RETURN v_retorno 
   
END FUNCTION 

FUNCTION crea_temporales()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_rec_informe
      DROP TABLE tmp_rch_informe

   WHENEVER ERROR STOP
      CREATE TABLE tmp_rec_informe( entidad           SMALLINT, 
                                     nss                CHAR(11),
                                     id_derechohabiente DECIMAL(9,0),
                                     tpo_credito        SMALLINT,
                                     desc_credito       CHAR(30),
                                     sdo_deudor         DECIMAL(12,2),
                                     identificador      CHAR(20) 
                                     );

      CREATE TABLE tmp_rch_informe(tipo_registro      SMALLINT, 
                                     nss                CHAR(11),
                                     tpo_credito        SMALLINT,
                                     desc_credito       CHAR(30),
                                     sdo_deudor         DECIMAL(12,2),
                                     clave_edo          SMALLINT,
                                     edo_desc           CHAR(40),
                                     identificador      CHAR(20)
                                     );      

DATABASE safre_viv
   
END FUNCTION 
