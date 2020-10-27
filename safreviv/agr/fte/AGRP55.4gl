###########################################################################
#Modulo            => AGR                                                 #
#Programa          => AGRP55                                              #
#Objetivo          => Genera extractor y reporte de adelandos vs Op.09.   #
#Autor             => Emilio Abarca, EFP                                  #
#Fecha inicio      => 09/Noviembre/2018                                   #
###########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario           CHAR(20)
   DEFINE p_proceso_cod       SMALLINT  
   DEFINE p_opera_cod         SMALLINT
   DEFINE p_pid               DECIMAL(9,0)
   DEFINE p_fecha             DATE
   DEFINE v_ruta_bin          CHAR(40)
   DEFINE v_ruta_envio        CHAR(40)
   DEFINE v_ruta_lst          CHAR(40)
   DEFINE r_b_valida          SMALLINT

   -- Variables para el PDF
   TYPE rec_arreglo           RECORD
      variacion    CHAR(20),
      total        INTEGER,
      aivs92       DECIMAL(16,2),
      aivs97       DECIMAL(16,2),
      aivs_total   DECIMAL(16,6),
      porcentaje   CHAR(12)
   END RECORD
   TYPE rec_total             RECORD
      total        INTEGER,
      aivs92       DECIMAL(16,2),
      aivs97       DECIMAL(16,2),
      aivs_total   DECIMAL(16,6),
      porcentaje   CHAR(12)
   END RECORD 
   -- Arreglo global
   TYPE arr_global DYNAMIC ARRAY OF rec_arreglo 
   -- Arreglos
   DEFINE arreglo_var_ag      arr_global
   DEFINE arreglo_var_ta      arr_global
   DEFINE arreglo_var_ug      arr_global
   -- Record totales globales
   DEFINE r_total_ag          rec_total
   DEFINE r_total_ta          rec_total
   DEFINE r_total_ug          rec_total
   DEFINE v_aux_porcentaje    DECIMAL(6,2)
   -- Variables para el archivo de salida
   DEFINE v_salida_arh        STRING 
   
END GLOBALS

MAIN
   -- Parámetros que envía AGRL76
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_fecha       = ARG_VAL(5)

   -- Log en caso de errores
   CALL STARTLOG(p_usuario CLIPPED|| ".AGRP55.log")

   SELECT ruta_bin,
          ruta_envio,
          ruta_listados
     INTO v_ruta_bin,
          v_ruta_envio,
          v_ruta_lst
     FROM seg_modulo
    WHERE modulo_cod = 'agr'
  
   CALL fn_display_proceso(0,"ADELANTOS VS OP.09" CLIPPED)

   -- Genera extractor Adelantos vs Op.09
   CALL fn_genera_extractor()
   DISPLAY ""
   DISPLAY " GENERA EXTRACTOR ...COMPLETADO"
   DISPLAY " El extractor se ha generado en: ",v_salida_arh
   
   -- Obtiene información para el PDF
   CALL fn_informacion_rpt()
   DISPLAY ""
   DISPLAY " GENERA REPORTE PDF ...COMPLETADO."
   DISPLAY ""

   -- Finaliza proceso
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   IF(r_b_valida <> 0) THEN
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_desplega_inc_operacion(r_b_valida)
   END IF
   
   CALL fn_display_proceso(1,"ADELANTOS VS OP.09" CLIPPED)

END MAIN

FUNCTION fn_genera_extractor()

   DEFINE archivo          base.channel
   DEFINE v_detalle        STRING
   DEFINE v_cadena         STRING 
   DEFINE r_inf_adelanto   RECORD
      nss            CHAR(11),
      f_adelanto     DATE,
      tpo_adelanto   CHAR(3),
      subcuenta      CHAR(1),
      f_liquida      DECIMAL(10,0),
      aivs92         DECIMAL(12,2),
      aivs97         DECIMAL(12,2),
      monto_solic    CHAR(24),
      arh_op09       CHAR(3),
      f_arh_op09     DATE,
      sub_op09       CHAR(1),
      aivs92_09      DECIMAL(12,2),
      aivs97_09      DECIMAL(12,2),
      monto_op09     CHAR(24),
      variacion      CHAR(8)
   END RECORD 
   
   LET v_salida_arh = v_ruta_envio CLIPPED,"/adecon09.txt"
   LET archivo = base.Channel.create()

   -- Abre archivo de salida para escritura
   CALL archivo.openFile(v_salida_arh,"w")

   -- Query global
   LET v_cadena = "SELECT x.nss,
                          x.f_liquida,
                          DECODE(x.modulo_orig,'AG','AGR','TA','ACR','UG','GRT'),
                          x.folio_liquida,
                          x.aivs92,
                          x.aivs97,
                          DECODE(x.modulo_cod,'AG','AGR','TA','ACR','UG','GRT'),
                          a.f_proceso,
                          x.aivs92_09,
                          x.aivs97_09,
                          x.porcentaje_var
                     FROM cre_extr_adelanto x,
                          cre_ctr_archivo a
                    WHERE x.id_cre_ctr_archivo = a.id_cre_ctr_archivo"

   PREPARE prp_extractor_adelantos FROM v_cadena
   DECLARE crs_extractor_adelantos CURSOR FOR prp_extractor_adelantos
   
   INITIALIZE r_inf_adelanto.* TO NULL 

   FOREACH crs_extractor_adelantos INTO r_inf_adelanto.nss,
                                        r_inf_adelanto.f_adelanto,
                                        r_inf_adelanto.tpo_adelanto,
                                        r_inf_adelanto.f_liquida,
                                        r_inf_adelanto.aivs92,
                                        r_inf_adelanto.aivs97,
                                        r_inf_adelanto.arh_op09,
                                        r_inf_adelanto.f_arh_op09,
                                        r_inf_adelanto.aivs92_09,
                                        r_inf_adelanto.aivs97_09,
                                        r_inf_adelanto.variacion
                                       
      IF(r_inf_adelanto.aivs92 <> 0) THEN
         LET r_inf_adelanto.subcuenta   = 8
         LET r_inf_adelanto.monto_solic = r_inf_adelanto.aivs92
      ELSE 
         IF(r_inf_adelanto.aivs97 <> 0) THEN
            LET r_inf_adelanto.subcuenta   = 4
            LET r_inf_adelanto.monto_solic = r_inf_adelanto.aivs97
         END IF 
      END IF

      IF(r_inf_adelanto.aivs92_09 <> 0) THEN
         LET r_inf_adelanto.sub_op09   = 8
         LET r_inf_adelanto.monto_op09 = r_inf_adelanto.aivs92_09
      ELSE 
         IF(r_inf_adelanto.aivs97_09 <> 0) THEN
            LET r_inf_adelanto.sub_op09   = 4
            LET r_inf_adelanto.monto_op09 = r_inf_adelanto.aivs97_09
         END IF 
      END IF 
      
      LET v_detalle = r_inf_adelanto.nss,
                      r_inf_adelanto.f_adelanto  USING "yyyymmdd",
                      r_inf_adelanto.tpo_adelanto,
                      r_inf_adelanto.subcuenta,
                      r_inf_adelanto.monto_solic USING "&&&&&&&&&&&&&&&&&&.&&&&&",
                      r_inf_adelanto.arh_op09,
                      r_inf_adelanto.f_arh_op09  USING "yyyymm",
                      r_inf_adelanto.sub_op09,
                      r_inf_adelanto.monto_op09  USING "&&&&&&&&&&&&&&&&&&.&&&&&",
                      r_inf_adelanto.variacion
                      
      -- Escribe
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH 

   CALL archivo.close()
   
END FUNCTION 

FUNCTION fn_informacion_rpt()

   DEFINE v_f            INTEGER
   DEFINE r_adelanto     RECORD
      modulo_orig  CHAR(2),
      aivs92       DECIMAL(16,2),
      aivs97       DECIMAL(16,2),
      aivs_total   DECIMAL(16,6),
      variacion    DECIMAL(6,2),
      total        INTEGER
   END RECORD
   DEFINE v_reporte_bin   STRING
   DEFINE v_ruta_rpt      STRING
   DEFINE object_rpt      om.SaxDocumentHandler 

   -- Inicializa arreglos
   LET arreglo_var_ag[1].variacion  = "   <=  a -101%"
   LET arreglo_var_ag[2].variacion  = "-100% a -51%"
   LET arreglo_var_ag[3].variacion  = " -50% a -26%"
   LET arreglo_var_ag[4].variacion  = " -25% a -11%"
   LET arreglo_var_ag[5].variacion  = "- 10% a -1%"
   LET arreglo_var_ag[6].variacion  = "-0.9% a  0.9%"
   LET arreglo_var_ag[7].variacion  = "   1%  a  10%"
   LET arreglo_var_ag[8].variacion  = "  11% a  25%"
   LET arreglo_var_ag[9].variacion  = "  26% a  50%"
   LET arreglo_var_ag[10].variacion = "  51% a  100%"
   LET arreglo_var_ag[11].variacion = "   >=  a  101%"

   LET arreglo_var_ta[1].variacion  = "   <=  a -101%"
   LET arreglo_var_ta[2].variacion  = "-100% a -51%"
   LET arreglo_var_ta[3].variacion  = " -50% a -26%"
   LET arreglo_var_ta[4].variacion  = " -25% a -11%"
   LET arreglo_var_ta[5].variacion  = "- 10% a -1%"
   LET arreglo_var_ta[6].variacion  = "-0.9% a  0.9%"
   LET arreglo_var_ta[7].variacion  = "   1%  a  10%"
   LET arreglo_var_ta[8].variacion  = "  11% a  25%"
   LET arreglo_var_ta[9].variacion  = "  26% a  50%"
   LET arreglo_var_ta[10].variacion = "  51% a  100%"
   LET arreglo_var_ta[11].variacion = "   >=  a  101%"

   LET arreglo_var_ug[1].variacion  = "   <=  a -101%"
   LET arreglo_var_ug[2].variacion  = "-100% a -51%"
   LET arreglo_var_ug[3].variacion  = " -50% a -26%"
   LET arreglo_var_ug[4].variacion  = " -25% a -11%"
   LET arreglo_var_ug[5].variacion  = "- 10% a -1%"
   LET arreglo_var_ug[6].variacion  = "-0.9% a  0.9%"
   LET arreglo_var_ug[7].variacion  = "   1%  a  10%"
   LET arreglo_var_ug[8].variacion  = "  11% a  25%"
   LET arreglo_var_ug[9].variacion  = "  26% a  50%"
   LET arreglo_var_ug[10].variacion = "  51% a  100%"
   LET arreglo_var_ug[11].variacion = "   >=  a  101%"

   -- Inicializa todas las filas del arreglo
   FOR v_f = 1 TO 11
      -- setea valores AG
      LET arreglo_var_ag[v_f].total  = 0
      LET arreglo_var_ag[v_f].aivs92 = 0
      LET arreglo_var_ag[v_f].aivs97 = 0
      LET arreglo_var_ag[v_f].aivs_total = 0

      -- setea valores TA
      LET arreglo_var_ta[v_f].total  = 0
      LET arreglo_var_ta[v_f].aivs92 = 0
      LET arreglo_var_ta[v_f].aivs97 = 0
      LET arreglo_var_ta[v_f].aivs_total = 0

      -- setea valores UG
      LET arreglo_var_ug[v_f].total  = 0
      LET arreglo_var_ug[v_f].aivs92 = 0
      LET arreglo_var_ug[v_f].aivs97 = 0
      LET arreglo_var_ug[v_f].aivs_total = 0
   END FOR 

   -- Inicializa record totales globales
   LET r_total_ag.total  = 0
   LET r_total_ag.aivs92 = 0
   LET r_total_ag.aivs97 = 0
   LET r_total_ag.aivs_total = 0

   LET r_total_ta.total  = 0
   LET r_total_ta.aivs92 = 0
   LET r_total_ta.aivs97 = 0
   LET r_total_ta.aivs_total = 0

   LET r_total_ug.total  = 0
   LET r_total_ug.aivs92 = 0
   LET r_total_ug.aivs97 = 0
   LET r_total_ug.aivs_total = 0
   LET v_aux_porcentaje = 0

   DECLARE crs_variaciones CURSOR FOR 
   SELECT modulo_orig,
          SUM(aivs92_09),
          SUM(aivs97_09),
          SUM(aivs92_09 + aivs97_09),
          porcentaje_var,
          COUNT(*)
     FROM cre_extr_adelanto
    GROUP BY 1,5;

   INITIALIZE r_adelanto.* TO NULL

   LET v_f =  1

   FOREACH crs_variaciones INTO r_adelanto.modulo_orig,
                                r_adelanto.aivs92,
                                r_adelanto.aivs97,
                                r_adelanto.aivs_total,
                                r_adelanto.variacion,
                                r_adelanto.total

      IF(r_adelanto.modulo_orig = 'AG') THEN
         CASE 
            WHEN r_adelanto.variacion  >= 1 AND r_adelanto.variacion <= 10.99
               LET arreglo_var_ag[7].total      = arreglo_var_ag[7].total + r_adelanto.total
               LET arreglo_var_ag[7].aivs92     = arreglo_var_ag[7].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[7].aivs97     = arreglo_var_ag[7].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[7].aivs_total = arreglo_var_ag[7].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 11 AND r_adelanto.variacion <= 25.99
               LET arreglo_var_ag[8].total      = arreglo_var_ag[8].total + r_adelanto.total
               LET arreglo_var_ag[8].aivs92     = arreglo_var_ag[8].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[8].aivs97     = arreglo_var_ag[8].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[8].aivs_total = arreglo_var_ag[8].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 26 AND r_adelanto.variacion <= 50.99
               LET arreglo_var_ag[9].total      = arreglo_var_ag[9].total + r_adelanto.total
               LET arreglo_var_ag[9].aivs92     = arreglo_var_ag[9].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[9].aivs97     = arreglo_var_ag[9].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[9].aivs_total = arreglo_var_ag[9].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 51 AND r_adelanto.variacion <= 100.99
               LET arreglo_var_ag[10].total      = arreglo_var_ag[10].total + r_adelanto.total
               LET arreglo_var_ag[10].aivs92     = arreglo_var_ag[10].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[10].aivs97     = arreglo_var_ag[10].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[10].aivs_total = arreglo_var_ag[10].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 101
               LET arreglo_var_ag[11].total      = arreglo_var_ag[11].total + r_adelanto.total
               LET arreglo_var_ag[11].aivs92     = arreglo_var_ag[11].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[11].aivs97     = arreglo_var_ag[11].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[11].aivs_total = arreglo_var_ag[11].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -0.9 AND r_adelanto.variacion <= 0.9
               LET arreglo_var_ag[6].total      = arreglo_var_ag[6].total + r_adelanto.total
               LET arreglo_var_ag[6].aivs92     = arreglo_var_ag[6].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[6].aivs97     = arreglo_var_ag[6].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[6].aivs_total = arreglo_var_ag[6].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -10.99 AND r_adelanto.variacion <= -1
               LET arreglo_var_ag[5].total      = arreglo_var_ag[5].total + r_adelanto.total
               LET arreglo_var_ag[5].aivs92     = arreglo_var_ag[5].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[5].aivs97     = arreglo_var_ag[5].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[5].aivs_total = arreglo_var_ag[5].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -25.99 AND r_adelanto.variacion <= -11
               LET arreglo_var_ag[4].total      = arreglo_var_ag[4].total + r_adelanto.total
               LET arreglo_var_ag[4].aivs92     = arreglo_var_ag[4].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[4].aivs97     = arreglo_var_ag[4].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[4].aivs_total = arreglo_var_ag[4].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -50.99 AND r_adelanto.variacion <= -26
               LET arreglo_var_ag[3].total      = arreglo_var_ag[3].total + r_adelanto.total
               LET arreglo_var_ag[3].aivs92     = arreglo_var_ag[3].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[3].aivs97     = arreglo_var_ag[3].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[3].aivs_total = arreglo_var_ag[3].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -100.99 AND r_adelanto.variacion <= -51
               LET arreglo_var_ag[2].total      = arreglo_var_ag[2].total + r_adelanto.total
               LET arreglo_var_ag[2].aivs92     = arreglo_var_ag[2].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[2].aivs97     = arreglo_var_ag[2].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[2].aivs_total = arreglo_var_ag[2].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  <= -101
               LET arreglo_var_ag[1].total      = arreglo_var_ag[1].total + r_adelanto.total
               LET arreglo_var_ag[1].aivs92     = arreglo_var_ag[1].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ag[1].aivs97     = arreglo_var_ag[1].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ag[1].aivs_total = arreglo_var_ag[1].aivs_total + r_adelanto.aivs_total
         END CASE 
         
         -- Incrementa total global
         LET r_total_ag.total  = r_total_ag.total  + r_adelanto.total
         LET r_total_ag.aivs92 = r_total_ag.aivs92 + r_adelanto.aivs92
         LET r_total_ag.aivs97 = r_total_ag.aivs97 + r_adelanto.aivs97
         LET r_total_ag.aivs_total = r_total_ag.aivs_total + r_adelanto.aivs_total
   
      END IF 
      
      IF(r_adelanto.modulo_orig = 'TA') THEN
         CASE 
            WHEN r_adelanto.variacion  >= 1 AND r_adelanto.variacion <= 10.99
               LET arreglo_var_ta[7].total      = arreglo_var_ta[7].total + r_adelanto.total
               LET arreglo_var_ta[7].aivs92     = arreglo_var_ta[7].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[7].aivs97     = arreglo_var_ta[7].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[7].aivs_total = arreglo_var_ta[7].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 11 AND r_adelanto.variacion <= 25.99
               LET arreglo_var_ta[8].total      = arreglo_var_ta[8].total + r_adelanto.total
               LET arreglo_var_ta[8].aivs92     = arreglo_var_ta[8].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[8].aivs97     = arreglo_var_ta[8].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[8].aivs_total = arreglo_var_ta[8].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 26 AND r_adelanto.variacion <= 50.99
               LET arreglo_var_ta[9].total      = arreglo_var_ta[9].total + r_adelanto.total
               LET arreglo_var_ta[9].aivs92     = arreglo_var_ta[9].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[9].aivs97     = arreglo_var_ta[9].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[9].aivs_total = arreglo_var_ta[9].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 51 AND r_adelanto.variacion <= 100.99
               LET arreglo_var_ta[10].total      = arreglo_var_ta[10].total + r_adelanto.total
               LET arreglo_var_ta[10].aivs92     = arreglo_var_ta[10].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[10].aivs97     = arreglo_var_ta[10].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[10].aivs_total = arreglo_var_ta[10].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 101
               LET arreglo_var_ta[11].total      = arreglo_var_ta[11].total + r_adelanto.total
               LET arreglo_var_ta[11].aivs92     = arreglo_var_ta[11].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[11].aivs97     = arreglo_var_ta[11].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[11].aivs_total = arreglo_var_ta[11].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -0.9 AND r_adelanto.variacion <= 0.9
               LET arreglo_var_ta[6].total      = arreglo_var_ta[6].total + r_adelanto.total
               LET arreglo_var_ta[6].aivs92     = arreglo_var_ta[6].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[6].aivs97     = arreglo_var_ta[6].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[6].aivs_total = arreglo_var_ta[6].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -10.99 AND r_adelanto.variacion <= -1
               LET arreglo_var_ta[5].total      = arreglo_var_ta[5].total + r_adelanto.total
               LET arreglo_var_ta[5].aivs92     = arreglo_var_ta[5].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[5].aivs97     = arreglo_var_ta[5].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[5].aivs_total = arreglo_var_ta[5].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -25.99 AND r_adelanto.variacion <= -11
               LET arreglo_var_ta[4].total      = arreglo_var_ta[4].total + r_adelanto.total
               LET arreglo_var_ta[4].aivs92     = arreglo_var_ta[4].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[4].aivs97     = arreglo_var_ta[4].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[4].aivs_total = arreglo_var_ta[4].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -50.99 AND r_adelanto.variacion <= -26
               LET arreglo_var_ta[3].total      = arreglo_var_ta[3].total + r_adelanto.total
               LET arreglo_var_ta[3].aivs92     = arreglo_var_ta[3].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[3].aivs97     = arreglo_var_ta[3].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[3].aivs_total = arreglo_var_ta[3].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -100.99 AND r_adelanto.variacion <= -51
               LET arreglo_var_ta[2].total      = arreglo_var_ta[2].total + r_adelanto.total
               LET arreglo_var_ta[2].aivs92     = arreglo_var_ta[2].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[2].aivs97     = arreglo_var_ta[2].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[2].aivs_total = arreglo_var_ta[2].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  <= -101
               LET arreglo_var_ta[1].total      = arreglo_var_ta[1].total + r_adelanto.total
               LET arreglo_var_ta[1].aivs92     = arreglo_var_ta[1].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ta[1].aivs97     = arreglo_var_ta[1].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ta[1].aivs_total = arreglo_var_ta[1].aivs_total + r_adelanto.aivs_total
         END CASE
         
         -- Incrementa total global
         LET r_total_ta.total  = r_total_ta.total  + r_adelanto.total
         LET r_total_ta.aivs92 = r_total_ta.aivs92 + r_adelanto.aivs92
         LET r_total_ta.aivs97 = r_total_ta.aivs97 + r_adelanto.aivs97
         LET r_total_ta.aivs_total = r_total_ta.aivs_total + r_adelanto.aivs_total
         
      END IF

      IF(r_adelanto.modulo_orig = 'UG') THEN
         CASE 
            WHEN r_adelanto.variacion  >= 1 AND r_adelanto.variacion <= 10.99
               LET arreglo_var_ug[7].total      = arreglo_var_ug[7].total + r_adelanto.total
               LET arreglo_var_ug[7].aivs92     = arreglo_var_ug[7].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[7].aivs97     = arreglo_var_ug[7].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[7].aivs_total = arreglo_var_ug[7].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 11 AND r_adelanto.variacion <= 25.99
               LET arreglo_var_ug[8].total      = arreglo_var_ug[8].total + r_adelanto.total
               LET arreglo_var_ug[8].aivs92     = arreglo_var_ug[8].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[8].aivs97     = arreglo_var_ug[8].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[8].aivs_total = arreglo_var_ug[8].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 26 AND r_adelanto.variacion <= 50.99
               LET arreglo_var_ug[9].total      = arreglo_var_ug[9].total + r_adelanto.total
               LET arreglo_var_ug[9].aivs92     = arreglo_var_ug[9].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[9].aivs97     = arreglo_var_ug[9].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[9].aivs_total = arreglo_var_ug[9].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 51 AND r_adelanto.variacion <= 100.99
               LET arreglo_var_ug[10].total      = arreglo_var_ug[10].total + r_adelanto.total
               LET arreglo_var_ug[10].aivs92     = arreglo_var_ug[10].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[10].aivs97     = arreglo_var_ug[10].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[10].aivs_total = arreglo_var_ug[10].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= 101
               LET arreglo_var_ug[11].total      = arreglo_var_ug[11].total + r_adelanto.total
               LET arreglo_var_ug[11].aivs92     = arreglo_var_ug[11].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[11].aivs97     = arreglo_var_ug[11].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[11].aivs_total = arreglo_var_ug[11].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -0.9 AND r_adelanto.variacion <= 0.9
               LET arreglo_var_ug[6].total      = arreglo_var_ug[6].total + r_adelanto.total
               LET arreglo_var_ug[6].aivs92     = arreglo_var_ug[6].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[6].aivs97     = arreglo_var_ug[6].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[6].aivs_total = arreglo_var_ug[6].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -10.99 AND r_adelanto.variacion <= -1
               LET arreglo_var_ug[5].total      = arreglo_var_ug[5].total + r_adelanto.total
               LET arreglo_var_ug[5].aivs92     = arreglo_var_ug[5].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[5].aivs97     = arreglo_var_ug[5].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[5].aivs_total = arreglo_var_ug[5].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -25.99 AND r_adelanto.variacion <= -11
               LET arreglo_var_ug[4].total      = arreglo_var_ug[4].total + r_adelanto.total
               LET arreglo_var_ug[4].aivs92     = arreglo_var_ug[4].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[4].aivs97     = arreglo_var_ug[4].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[4].aivs_total = arreglo_var_ug[4].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -50.99 AND r_adelanto.variacion <= -26
               LET arreglo_var_ug[3].total      = arreglo_var_ug[3].total + r_adelanto.total
               LET arreglo_var_ug[3].aivs92     = arreglo_var_ug[3].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[3].aivs97     = arreglo_var_ug[3].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[3].aivs_total = arreglo_var_ug[3].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  >= -100.99 AND r_adelanto.variacion <= -51
               LET arreglo_var_ug[2].total      = arreglo_var_ug[2].total + r_adelanto.total
               LET arreglo_var_ug[2].aivs92     = arreglo_var_ug[2].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[2].aivs97     = arreglo_var_ug[2].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[2].aivs_total = arreglo_var_ug[2].aivs_total + r_adelanto.aivs_total
               
            WHEN r_adelanto.variacion  <= -101
               LET arreglo_var_ug[1].total      = arreglo_var_ug[1].total + r_adelanto.total
               LET arreglo_var_ug[1].aivs92     = arreglo_var_ug[1].aivs92 + r_adelanto.aivs92
               LET arreglo_var_ug[1].aivs97     = arreglo_var_ug[1].aivs97 + r_adelanto.aivs97
               LET arreglo_var_ug[1].aivs_total = arreglo_var_ug[1].aivs_total + r_adelanto.aivs_total
         END CASE
         
         -- Incrementa total global
         LET r_total_ug.total  = r_total_ug.total  + r_adelanto.total
         LET r_total_ug.aivs92 = r_total_ug.aivs92 + r_adelanto.aivs92
         LET r_total_ug.aivs97 = r_total_ug.aivs97 + r_adelanto.aivs97
         LET r_total_ug.aivs_total = r_total_ug.aivs_total + r_adelanto.aivs_total
         
      END IF
      
      LET v_f = v_f + 1
      
   END FOREACH 

   -- Porcentaje totales globales
   --AG
   LET v_aux_porcentaje = (r_total_ag.aivs_total / r_total_ag.aivs_total) * 100
   LET r_total_ag.porcentaje = v_aux_porcentaje CLIPPED,"%"
   --TA
   LET v_aux_porcentaje = (r_total_ta.aivs_total / r_total_ta.aivs_total) * 100
   LET r_total_ta.porcentaje = v_aux_porcentaje CLIPPED,"%"
   --UG
   LET v_aux_porcentaje = (r_total_ug.aivs97 / r_total_ug.aivs97) * 100
   LET r_total_ug.porcentaje = v_aux_porcentaje CLIPPED,"%"
   
    # ~~~~~~~~ Configuración del reporte PDF ~~~~~~~~~~ #

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP551.4rp"
   LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",p_usuario CLIPPED,"-AGRP55-",
                       p_pid USING "&&&&&","-",p_proceso_cod USING "&&&&&","-",
                       p_opera_cod USING "&&&&&",".pdf"
 
   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0) -- Se muestra en automático
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET object_rpt = fgl_report_commitCurrentSettings()

      IF (object_rpt IS NOT NULL) THEN
         
         START REPORT imprime_pdf TO XML HANDLER object_rpt

            OUTPUT TO REPORT imprime_pdf()

         FINISH REPORT imprime_pdf

      END IF
   ELSE
      DISPLAY "ERROR: No fué posible abrir la platilla del reporte"
   END IF
   
END FUNCTION 

REPORT imprime_pdf()

   DEFINE v_k     INTEGER

   FORMAT
      FIRST PAGE HEADER
         
         #Encabezado
         PRINTX p_usuario
         PRINTX p_fecha USING "dd/mm/yyyy"

         -- AG
         PRINTX r_total_ag.total  
         PRINTX r_total_ag.aivs92 
         PRINTX r_total_ag.aivs97 
         PRINTX r_total_ag.aivs_total
         PRINTX r_total_ag.porcentaje
         -- TA
         PRINTX r_total_ta.total 
         PRINTX r_total_ta.aivs92
         PRINTX r_total_ta.aivs97
         PRINTX r_total_ta.aivs_total
         PRINTX r_total_ta.porcentaje
         -- UA
         PRINTX r_total_ug.total 
         PRINTX r_total_ug.aivs97
         PRINTX r_total_ug.porcentaje
   
      ON EVERY ROW 
         -- Arreglo AG
         FOR v_k = 1 TO arreglo_var_ag.getLength()
            PRINTX arreglo_var_ag[v_k].variacion
            PRINTX arreglo_var_ag[v_k].total
            PRINTX arreglo_var_ag[v_k].aivs92
            PRINTX arreglo_var_ag[v_k].aivs97
            PRINTX arreglo_var_ag[v_k].aivs_total
            -- Obtiene porcentaje
            LET v_aux_porcentaje = (arreglo_var_ag[v_k].aivs_total / r_total_ag.aivs_total) * 100
            LET arreglo_var_ag[v_k].porcentaje = v_aux_porcentaje CLIPPED,"%"
            PRINTX arreglo_var_ag[v_k].porcentaje
         END FOR

         -- Arreglo TA
         FOR v_k = 1 TO arreglo_var_ta.getLength()
            PRINTX arreglo_var_ta[v_k].variacion
            PRINTX arreglo_var_ta[v_k].total
            PRINTX arreglo_var_ta[v_k].aivs92
            PRINTX arreglo_var_ta[v_k].aivs97
            PRINTX arreglo_var_ta[v_k].aivs_total
            -- Obtiene porcentaje
            LET v_aux_porcentaje = (arreglo_var_ta[v_k].aivs_total / r_total_ta.aivs_total) * 100
            LET arreglo_var_ta[v_k].porcentaje = v_aux_porcentaje CLIPPED,"%"
            PRINTX arreglo_var_ta[v_k].porcentaje
         END FOR
       
         -- Arreglo UG
         FOR v_k = 1 TO arreglo_var_ug.getLength()
            PRINTX arreglo_var_ug[v_k].variacion
            PRINTX arreglo_var_ug[v_k].total
            PRINTX arreglo_var_ug[v_k].aivs97
            -- Obtiene porcentaje
            LET v_aux_porcentaje = (arreglo_var_ug[v_k].aivs97 / r_total_ug.aivs97) * 100
            LET arreglo_var_ug[v_k].porcentaje = v_aux_porcentaje CLIPPED,"%"
            PRINTX arreglo_var_ug[v_k].porcentaje
         END FOR
         
END REPORT







