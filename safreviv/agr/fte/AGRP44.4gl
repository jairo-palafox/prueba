--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: Nov 1, 2017
--==============================================================================
################################################################################
#Modulo       => AGR                                                           #
#Programa     => AGRL64                                                        #
#Objetivo     => Lanzado integración archivo solicitud de saldos               #
#Elaboró      => Antonio Gómez                                                 #
#Fecha inicio => Nov 30, 2017                                                   #
################################################################################

DATABASE safre_viv
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod       LIKE cat_operacion.opera_cod -- codigo de operacion
DEFINE p_pid            LIKE bat_ctr_operacion.pid -- PID del proceso
       ,p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod      LIKE bat_ctr_operacion.opera_cod -- codigo de la operacion
       ,p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,v_folio          DECIMAL(9,0)
       ,p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
       ,v_f_ini_opera    LIKE bat_ctr_operacion.fecha_ini
       ,v_f_fin_opera    LIKE bat_ctr_operacion.fecha_fin
       
DEFINE g_reg_modulo   RECORD  --Almacena las rutas de archivos 
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40),
        ruta_envio       CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
       
   DEFINE v_ruta_rpt    STRING
   --Record's para el reporte pdf
   DEFINE r_total_cargado RECORD
      total_g      INTEGER,
      aivs92_g     DECIMAL(16,2),
      aivs97_g     DECIMAL(16,2),
      prc_g        CHAR(12),
      --var marca1
      total_m1     INTEGER,
      aivs92_m1    DECIMAL(16,2),
      aivs97_m1    DECIMAL(16,2),
      prc_m1       CHAR(12),
      --var marca2
      total_m2     INTEGER,
      aivs92_m2    DECIMAL(16,2),
      aivs97_m2    DECIMAL(16,2),
      prc_m2       CHAR(12),
      --var marca4
      total_m4     INTEGER,
      aivs92_m4    DECIMAL(16,2),
      aivs97_m4    DECIMAL(16,2),
      prc_m4       CHAR(12)
   END RECORD

   DEFINE r_total_aceptado RECORD
      total_g      INTEGER,
      aivs92_g     DECIMAL(16,2),
      aivs97_g     DECIMAL(16,2),
      prc_g        CHAR(12),
      --var marca1
      total_m1     INTEGER,
      aivs92_m1    DECIMAL(16,2),
      aivs97_m1    DECIMAL(16,2),
      prc_m1       CHAR(12),
      --var marca2
      total_m2     INTEGER,
      aivs92_m2    DECIMAL(16,2),
      aivs97_m2    DECIMAL(16,2),
      prc_m2       CHAR(12),
      --var marca4
      total_m4     INTEGER,
      aivs92_m4    DECIMAL(16,2),
      aivs97_m4    DECIMAL(16,2),
      prc_m4       CHAR(12)
   END RECORD 

   DEFINE r_total_rechazado RECORD
      total_g      INTEGER,
      aivs92_g     DECIMAL(16,2),
      aivs97_g     DECIMAL(16,2),
      prc_g        CHAR(12),
      --var marca1
      total_m1     INTEGER,
      aivs92_m1    DECIMAL(16,2),
      aivs97_m1    DECIMAL(16,2),
      prc_m1       CHAR(12),
      --var marca2
      total_m2     INTEGER,
      aivs92_m2    DECIMAL(16,2),
      aivs97_m2    DECIMAL(16,2),
      prc_m2       CHAR(12),
      --var marca4
      total_m4     INTEGER,
      aivs92_m4    DECIMAL(16,2),
      aivs97_m4    DECIMAL(16,2),
      prc_m4       CHAR(12)
   END RECORD 
   --Array para rechazos
   DEFINE arr_rch_marca1 DYNAMIC ARRAY OF RECORD
      total       INTEGER,
      aivs92      DECIMAL(16,2),
      aivs97      DECIMAL(16,2),
      cve_causal  SMALLINT,
      desc_causal CHAR(40)
   END RECORD
   DEFINE arr_rch_marca2 DYNAMIC ARRAY OF RECORD
      total       INTEGER,
      aivs92      DECIMAL(16,2),
      aivs97      DECIMAL(16,2),
      cve_causal  SMALLINT,
      desc_causal CHAR(40)
   END RECORD 
   DEFINE arr_rch_marca4 DYNAMIC ARRAY OF RECORD
      total       INTEGER,
      aivs92      DECIMAL(16,2),
      aivs97      DECIMAL(16,2),
      cve_causal  SMALLINT,
      desc_causal CHAR(40)
   END RECORD
   --variables para el archivo de salida
   DEFINE v_arh_salida    STRING 
   DEFINE v_ruta_archivo  STRING
   
END GLOBALS

MAIN
DEFINE v_s_sql              STRING -- cadena con una instruccion SQL
       ,v_resultado        INTEGER -- resultado del proceso
       ,r_bnd_fin_oper       SMALLINT
      -- ,v_msj_sql            CHAR(200)
      -- ,p_titulo             STRING -- titulo del mensaje enviado en el correo
      -- ,p_mensaje            STRING -- cuerpo del mensaje enviado
       ,v_layout             LIKE cat_operacion.layout_cod
       ,v_ruta_rescate       STRING
       ,v_usuario            LIKE seg_modulo.usuario
       ,v_proceso_desc       LIKE cat_proceso.proceso_desc
       ,v_extension          LIKE cat_operacion.extension
       ,v_opera_desc         LIKE cat_operacion.opera_desc
       ,v_ruta_listados      LIKE seg_modulo.ruta_listados
     --  ,v_s_comando          STRING 
     --  ,v_mensaje            CHAR(150)
     -- ,v_tot_solicitudes    INTEGER 
     --  ,v_tot_actualizacion  INTEGER

        DEFINE v_isam_err INTEGER
        DEFINE err_txt CHAR(200)
        DEFINE v_tot_regs_insertados INTEGER
        DEFINE v_total_rechazados INTEGER
        DEFINE v_total_aceptados INTEGER


   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| "AGRP44.log")
       
   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET V_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,
                                         v_extension, 
                                         v_opera_desc,
                                         v_layout, 
                                         v_ruta_rescate,
                                         v_ruta_listados,
                                         v_usuario
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = 344 --Nuevo modelo de unificación IMSS
   LET g_opera_cod   = 2    --Integración

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados,s.ruta_envio
   INTO   g_reg_modulo.ruta_bin,g_reg_modulo.ruta_rescate,g_reg_modulo.ruta_listados,
          g_reg_modulo.ruta_envio
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'agr'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
   
   --WHENEVER ERROR CONTINUE
   
   LET v_resultado = 0

   --Genera el folio de la integración 
   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
   RETURNING v_folio

   DISPLAY " "
   DISPLAY "USUARIO ", p_usuario_cod
   DISPLAY "PROCESO ", p_proceso_cod
   DISPLAY "ARCHIVO ", p_nombre_archivo
   DISPLAY "FOLIO   ", v_folio  
   DISPLAY "PID     ", p_pid
   DISPLAY " "            

   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION  fn_agr_integra_sol_saldo_ag_ta_43b(?, ?, ?, ?, ?)"
--DISPLAY v_s_sql
   PREPARE prp_integra FROM v_s_sql
   EXECUTE prp_integra USING p_usuario_cod, 
                             p_proceso_cod, 
                             p_nombre_archivo, 
                             v_folio, 
                             p_pid
                       INTO v_resultado,
                             v_isam_err,
                             err_txt,
                             v_tot_regs_insertados,
                             v_total_rechazados,
                             v_total_aceptados
                             
--DISPLAY  "v_resultado,           ", v_resultado          
--DISPLAY  "v_isam_err,            ", v_isam_err           
--DISPLAY  "err_txt,               ", err_txt   
--DISPLAY " "           
--DISPLAY  "Total registros ", v_tot_regs_insertados
--DISPLAY  "Total aceptados ", v_total_aceptados
--DISPLAY  "Total rechazados ", v_total_rechazados
--DISPLAY " "

   IF (v_resultado = 0) THEN
      DISPLAY ""
      DISPLAY "-- La integración se terminó completamente. --"
      DISPLAY ""
      DISPLAY "   Folio lote o de integración: "||v_folio
      DISPLAY "   Total de Registros     : ",v_tot_regs_insertados
      DISPLAY "   Total de Aceptados     : ",v_total_aceptados
      DISPLAY "   Total de Rechazados    : ",v_total_rechazados

      SELECT COUNT(*)
        INTO v_total_rechazados
        FROM cre_solic_sdo 
       WHERE cod_resultado = 2
         AND folio = v_folio

      --Si existe algún registro con rechazos
      IF v_total_rechazados > = 1 THEN 
         CALL archivo_salida()
         DISPLAY ""
         DISPLAY "  GENERA ARCHIVO DE RECHAZOS ...COMPLETADO"
         DISPLAY "  El archivo de ha generado en /safreviv_int/agr/envio con nombre:"
         DISPLAY "  ",v_arh_salida
      ELSE 
         DISPLAY "   No existen registros para el archivo de rechazos"
      END IF 

      #Finaliza operación
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_bnd_fin_oper

      SELECT fecha_ini,fecha_fin
        INTO v_f_ini_opera, v_f_fin_opera
        FROM bat_ctr_operacion
       WHERE pid = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod   = p_opera_cod

      #GENERA PDF
      DISPLAY " "
      CALL reporte_pdf()
      DISPLAY "  GENERA PDF ...COMPLETADO"
      DISPLAY " "
      
   ELSE 
      DISPLAY ""
      DISPLAY "-- La integración no terminó correctamente. --" 
      DISPLAY "   Resultado ",v_resultado
      DISPLAY "   isam_err ",v_isam_err
      DISPLAY "   err_txt ",err_txt

      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_bnd_fin_oper
   END IF 

END MAIN

FUNCTION reporte_pdf()

   DEFINE v_manejador_rpt   OM.SaxDocumentHandler  
   DEFINE v_aux_porcentaje  DECIMAL(6,2)
   DEFINE r_sol_saldo       RECORD
      marca_procesar   SMALLINT,
      cod_resultado    SMALLINT,
      aivs92           DECIMAL(16,2),
      aivs97           DECIMAL(16,2),
      total            INTEGER 
   END RECORD
   DEFINE r_rechazos  RECORD
      marca_pcr   SMALLINT,
      causal      SMALLINT,
      causal_desc CHAR(40),
      aivs92      DECIMAL(16,2),
      aivs97      DECIMAL(16,2),
      total       INTEGER 
   END RECORD
   DEFINE v_reporte_bin  STRING
   --contadores para rechazos
   DEFINE m1            INTEGER 
   DEFINE m2            INTEGER
   DEFINE m4            INTEGER
   
   # Inicializa record total cargado
   LET r_total_cargado.total_g   = 0
   LET r_total_cargado.aivs92_g  = 0
   LET r_total_cargado.aivs97_g  = 0
   LET r_total_cargado.total_m1  = 0
   LET r_total_cargado.aivs92_m1 = 0
   LET r_total_cargado.aivs97_m1 = 0
   LET r_total_cargado.total_m2  = 0
   LET r_total_cargado.aivs92_m2 = 0
   LET r_total_cargado.aivs97_m2 = 0
   LET r_total_cargado.total_m4  = 0
   LET r_total_cargado.aivs92_m4 = 0
   LET r_total_cargado.aivs97_m4 = 0

   LET r_total_aceptado.total_g   = 0
   LET r_total_aceptado.aivs92_g  = 0
   LET r_total_aceptado.aivs97_g  = 0
   LET r_total_aceptado.total_m1  = 0
   LET r_total_aceptado.aivs92_m1 = 0
   LET r_total_aceptado.aivs97_m1 = 0
   LET r_total_aceptado.total_m2  = 0
   LET r_total_aceptado.aivs92_m2 = 0
   LET r_total_aceptado.aivs97_m2 = 0
   LET r_total_aceptado.total_m4  = 0
   LET r_total_aceptado.aivs92_m4 = 0
   LET r_total_aceptado.aivs97_m4 = 0

   LET r_total_rechazado.total_g   = 0
   LET r_total_rechazado.aivs92_g  = 0
   LET r_total_rechazado.aivs97_g  = 0
   LET r_total_rechazado.total_m1  = 0
   LET r_total_rechazado.aivs92_m1 = 0
   LET r_total_rechazado.aivs97_m1 = 0
   LET r_total_rechazado.total_m2  = 0
   LET r_total_rechazado.aivs92_m2 = 0
   LET r_total_rechazado.aivs97_m2 = 0
   LET r_total_rechazado.total_m4  = 0
   LET r_total_rechazado.aivs92_m4 = 0
   LET r_total_rechazado.aivs97_m4 = 0

   ##################################
   #####  TOTALES GLOBALES    #######
   ##################################
   
   #cursor que obtiene información procesada
   DECLARE crs_rpt CURSOR FOR 
   SELECT marca_procesar,
           cod_resultado,
           SUM(aivs92),
           SUM(aivs97),
           COUNT(*)
     FROM cre_solic_sdo
    WHERE folio = v_folio
    GROUP BY 1,2;

   INITIALIZE r_sol_saldo.* TO NULL
   
   FOREACH crs_rpt INTO r_sol_saldo.marca_procesar,
                         r_sol_saldo.cod_resultado,
                         r_sol_saldo.aivs92,
                         r_sol_saldo.aivs97,
                         r_sol_saldo.total

      # TOTAL GLOBAL
      LET r_total_cargado.total_g   = r_total_cargado.total_g  + r_sol_saldo.total
      LET r_total_cargado.aivs92_g  = r_total_cargado.aivs92_g + r_sol_saldo.aivs92
      LET r_total_cargado.aivs97_g  = r_total_cargado.aivs97_g + r_sol_saldo.aivs97

      CASE
         WHEN r_sol_saldo.marca_procesar = 1
            LET r_total_cargado.total_m1  = r_total_cargado.total_m1  + r_sol_saldo.total
            LET r_total_cargado.aivs92_m1 = r_total_cargado.aivs92_m1 + r_sol_saldo.aivs92
            LET r_total_cargado.aivs97_m1 = r_total_cargado.aivs97_m1 + r_sol_saldo.aivs97

         WHEN r_sol_saldo.marca_procesar = 2
            LET r_total_cargado.total_m2  = r_total_cargado.total_m2  + r_sol_saldo.total
            LET r_total_cargado.aivs92_m2 = r_total_cargado.aivs92_m2 + r_sol_saldo.aivs92
            LET r_total_cargado.aivs97_m2 = r_total_cargado.aivs97_m2 + r_sol_saldo.aivs97
            
         WHEN r_sol_saldo.marca_procesar = 4
            LET r_total_cargado.total_m4  = r_total_cargado.total_m4  + r_sol_saldo.total
            LET r_total_cargado.aivs92_m4 = r_total_cargado.aivs92_m4 + r_sol_saldo.aivs92
            LET r_total_cargado.aivs97_m4 = r_total_cargado.aivs97_m4 + r_sol_saldo.aivs97
      END CASE 

      #TOTAL ACEPTADOS
      IF(r_sol_saldo.cod_resultado = 1) THEN

         LET r_total_aceptado.total_g   = r_total_aceptado.total_g  + r_sol_saldo.total
         LET r_total_aceptado.aivs92_g  = r_total_aceptado.aivs92_g + r_sol_saldo.aivs92
         LET r_total_aceptado.aivs97_g  = r_total_aceptado.aivs97_g + r_sol_saldo.aivs97
         
         CASE
            WHEN r_sol_saldo.marca_procesar = 1
               LET r_total_aceptado.total_m1  = r_total_aceptado.total_m1  + r_sol_saldo.total
               LET r_total_aceptado.aivs92_m1 = r_total_aceptado.aivs92_m1 + r_sol_saldo.aivs92
               LET r_total_aceptado.aivs97_m1 = r_total_aceptado.aivs97_m1 + r_sol_saldo.aivs97

            WHEN r_sol_saldo.marca_procesar = 2
               LET r_total_aceptado.total_m2  = r_total_aceptado.total_m2  + r_sol_saldo.total
               LET r_total_aceptado.aivs92_m2 = r_total_aceptado.aivs92_m2 + r_sol_saldo.aivs92
               LET r_total_aceptado.aivs97_m2 = r_total_aceptado.aivs97_m2 + r_sol_saldo.aivs97
            
            WHEN r_sol_saldo.marca_procesar = 4
               LET r_total_aceptado.total_m4  = r_total_aceptado.total_m4  + r_sol_saldo.total
               LET r_total_aceptado.aivs92_m4 = r_total_aceptado.aivs92_m4 + r_sol_saldo.aivs92
               LET r_total_aceptado.aivs97_m4 = r_total_aceptado.aivs97_m4 + r_sol_saldo.aivs97
         END CASE
      ELSE 
         #TOTAL RECHAZADOS
         LET r_total_rechazado.total_g   = r_total_rechazado.total_g  + r_sol_saldo.total
         LET r_total_rechazado.aivs92_g  = r_total_rechazado.aivs92_g + r_sol_saldo.aivs92
         LET r_total_rechazado.aivs97_g  = r_total_rechazado.aivs97_g + r_sol_saldo.aivs97
         
         CASE
            WHEN r_sol_saldo.marca_procesar = 1
               LET r_total_rechazado.total_m1  = r_total_rechazado.total_m1  + r_sol_saldo.total
               LET r_total_rechazado.aivs92_m1 = r_total_rechazado.aivs92_m1 + r_sol_saldo.aivs92
               LET r_total_rechazado.aivs97_m1 = r_total_rechazado.aivs97_m1 + r_sol_saldo.aivs97

            WHEN r_sol_saldo.marca_procesar = 2
               LET r_total_rechazado.total_m2  = r_total_rechazado.total_m2  + r_sol_saldo.total
               LET r_total_rechazado.aivs92_m2 = r_total_rechazado.aivs92_m2 + r_sol_saldo.aivs92
               LET r_total_rechazado.aivs97_m2 = r_total_rechazado.aivs97_m2 + r_sol_saldo.aivs97
            
            WHEN r_sol_saldo.marca_procesar = 4
               LET r_total_rechazado.total_m4  = r_total_rechazado.total_m4  + r_sol_saldo.total
               LET r_total_rechazado.aivs92_m4 = r_total_rechazado.aivs92_m4 + r_sol_saldo.aivs92
               LET r_total_rechazado.aivs97_m4 = r_total_rechazado.aivs97_m4 + r_sol_saldo.aivs97
         END CASE
      END IF 

   END FOREACH 

   LET v_aux_porcentaje = 0
   #CALCULA % REGISTROS CARGADOS
   LET v_aux_porcentaje = (r_total_cargado.total_g / r_total_cargado.total_g) * 100
   LET r_total_cargado.prc_g = v_aux_porcentaje CLIPPED,"%"
   --marca 1
   LET v_aux_porcentaje = (r_total_cargado.total_m1 / r_total_cargado.total_g) * 100
   LET r_total_cargado.prc_m1 = v_aux_porcentaje CLIPPED,"%"
   --marca 2
   LET v_aux_porcentaje = (r_total_cargado.total_m2 / r_total_cargado.total_g) * 100
   LET r_total_cargado.prc_m2 = v_aux_porcentaje CLIPPED,"%"
   --marca 4
   LET v_aux_porcentaje = (r_total_cargado.total_m4 / r_total_cargado.total_g) * 100
   LET r_total_cargado.prc_m4 = v_aux_porcentaje CLIPPED,"%"

   #CALCULA % REGISTROS ACEPTADOS
   LET v_aux_porcentaje = (r_total_aceptado.total_g / r_total_aceptado.total_g) * 100
   LET r_total_aceptado.prc_g = v_aux_porcentaje CLIPPED,"%"
   --marca 1
   LET v_aux_porcentaje = (r_total_aceptado.total_m1 / r_total_aceptado.total_g) * 100
   LET r_total_aceptado.prc_m1 = v_aux_porcentaje CLIPPED,"%"
   --marca 2
   LET v_aux_porcentaje = (r_total_aceptado.total_m2 / r_total_aceptado.total_g) * 100
   LET r_total_aceptado.prc_m2 = v_aux_porcentaje CLIPPED,"%"
   --marca 4
   LET v_aux_porcentaje = (r_total_aceptado.total_m4 / r_total_aceptado.total_g) * 100
   LET r_total_aceptado.prc_m4 = v_aux_porcentaje CLIPPED,"%"

   #CALCULA % REGISTROS rechazados
   LET v_aux_porcentaje = (r_total_rechazado.total_g / r_total_rechazado.total_g) * 100
   LET r_total_rechazado.prc_g = v_aux_porcentaje CLIPPED,"%"
   --marca 1
   LET v_aux_porcentaje = (r_total_rechazado.total_m1 / r_total_rechazado.total_g) * 100
   LET r_total_rechazado.prc_m1 = v_aux_porcentaje CLIPPED,"%"
   --marca 2
   LET v_aux_porcentaje = (r_total_rechazado.total_m2 / r_total_rechazado.total_g) * 100
   LET r_total_rechazado.prc_m2 = v_aux_porcentaje CLIPPED,"%"
   --marca 4
   LET v_aux_porcentaje = (r_total_rechazado.total_m4 / r_total_rechazado.total_g) * 100
   LET r_total_rechazado.prc_m4 = v_aux_porcentaje CLIPPED,"%"

   ##################################
   #####        RECHAZOS      #######
   ##################################
   
   INITIALIZE r_rechazos.* TO NULL
   
   CALL arr_rch_marca1.clear()
   CALL arr_rch_marca2.clear()
   CALL arr_rch_marca4.clear()

   DECLARE crs_rch CURSOR FOR 
   SELECT s.marca_procesar,
           s.diagnostico,
           c.desc_estado,
           SUM(s.aivs92),
           SUM(s.aivs97),
           COUNT(*)
     FROM cre_solic_sdo s,
           cat_rch_acreditado c
    WHERE s.folio = v_folio
      AND s.cod_resultado = 2
      AND s.diagnostico   = c.estado
    GROUP BY 1,2,3;

   LET m1 = 1
   LET m2 = 1
   LET m4 = 1
   
   FOREACH crs_rch INTO r_rechazos.marca_pcr,
                         r_rechazos.causal,
                         r_rechazos.causal_desc,
                         r_rechazos.aivs92,
                         r_rechazos.aivs97,
                         r_rechazos.total

      #Llena arreglos por marca
      CASE
         WHEN r_rechazos.marca_pcr = 1
            LET arr_rch_marca1[m1].total       = r_rechazos.total
            LET arr_rch_marca1[m1].aivs92      = r_rechazos.aivs92
            LET arr_rch_marca1[m1].aivs97      = r_rechazos.aivs97
            LET arr_rch_marca1[m1].cve_causal  = r_rechazos.causal
            LET arr_rch_marca1[m1].desc_causal = r_rechazos.causal_desc

            LET m1 = m1 + 1  --Incrementa su contador
            
         WHEN r_rechazos.marca_pcr = 2
            LET arr_rch_marca2[m2].total       = r_rechazos.total
            LET arr_rch_marca2[m2].aivs92      = r_rechazos.aivs92
            LET arr_rch_marca2[m2].aivs97      = r_rechazos.aivs97
            LET arr_rch_marca2[m2].cve_causal  = r_rechazos.causal
            LET arr_rch_marca2[m2].desc_causal = r_rechazos.causal_desc

            LET m2 = m2 + 1  --Incrementa su contador
            
         WHEN r_rechazos.marca_pcr = 4
            LET arr_rch_marca4[m4].total       = r_rechazos.total
            LET arr_rch_marca4[m4].aivs92      = r_rechazos.aivs92
            LET arr_rch_marca4[m4].aivs97      = r_rechazos.aivs97
            LET arr_rch_marca4[m4].cve_causal  = r_rechazos.causal
            LET arr_rch_marca4[m4].desc_causal = r_rechazos.causal_desc

            LET m4 = m4 + 1  --Incrementa su contador
            
      END CASE 
      
   END FOREACH 

   #Elimina filas en blanco
   IF(m1 > 1) THEN 
      IF(arr_rch_marca1[arr_rch_marca1.getLength()].cve_causal IS NULL) THEN
         CALL arr_rch_marca1.deleteElement(arr_rch_marca1.getLength())
      END IF
    END IF 

   IF(m2 > 1) THEN 
      IF(arr_rch_marca2[arr_rch_marca2.getLength()].cve_causal IS NULL) THEN
         CALL arr_rch_marca2.deleteElement(arr_rch_marca2.getLength())
      END IF
   END IF 

   IF(m4 > 1) THEN 
      IF(arr_rch_marca4[arr_rch_marca4.getLength()].cve_causal IS NULL) THEN
         CALL arr_rch_marca4.deleteElement(arr_rch_marca4.getLength())
      END IF
   END IF 
   
   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = g_reg_modulo.ruta_bin CLIPPED,"/AGRP441.4rp"
   LET v_ruta_rpt    = g_reg_modulo.ruta_listados CLIPPED,"/",
                       p_usuario_cod CLIPPED,"-AGRL64-",
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
       DISPLAY "ERROR: No fue posible abrir plantilla del reporte"
   END IF 

END FUNCTION

REPORT genera_PDF()

   DEFINE v_f_presenta     DATE
   DEFINE v_desc_opera     CHAR(40)
   DEFINE f                INTEGER 

   FORMAT 
   FIRST PAGE HEADER
      LET v_f_presenta = TODAY
      LET v_desc_opera = "Carga Solicitud Saldos PROCESAR"

      #ENCABEZADO
      PRINTX p_usuario_cod
      PRINTX v_folio
      PRINTX v_f_presenta USING "dd-mm-yyyy"
      #RESUMEN
      PRINTX p_nombre_archivo            --Nombre del archivo
      PRINTX v_f_ini_opera               --fecha inicio proceso
      PRINTX v_f_fin_opera               --fecha fin 
      PRINTX v_desc_opera                --Desc. operación
      #Totales globales
      PRINTX r_total_cargado.total_g   
      PRINTX r_total_cargado.aivs92_g  
      PRINTX r_total_cargado.aivs97_g 
      PRINTX r_total_cargado.prc_g
      
      PRINTX r_total_cargado.total_m1  
      PRINTX r_total_cargado.aivs92_m1 
      PRINTX r_total_cargado.aivs97_m1 
      PRINTX r_total_cargado.prc_m1
      
      PRINTX r_total_cargado.total_m2  
      PRINTX r_total_cargado.aivs92_m2 
      PRINTX r_total_cargado.aivs97_m2 
      PRINTX r_total_cargado.prc_m2
      
      PRINTX r_total_cargado.total_m4  
      PRINTX r_total_cargado.aivs92_m4 
      PRINTX r_total_cargado.aivs97_m4
      PRINTX r_total_cargado.prc_m4
      --ACEPTADOS
      PRINTX r_total_aceptado.total_g   
      PRINTX r_total_aceptado.aivs92_g  
      PRINTX r_total_aceptado.aivs97_g 
      PRINTX r_total_aceptado.prc_g
      
      PRINTX r_total_aceptado.total_m1  
      PRINTX r_total_aceptado.aivs92_m1 
      PRINTX r_total_aceptado.aivs97_m1 
      PRINTX r_total_aceptado.prc_m1
      
      PRINTX r_total_aceptado.total_m2  
      PRINTX r_total_aceptado.aivs92_m2 
      PRINTX r_total_aceptado.aivs97_m2 
      PRINTX r_total_aceptado.prc_m2
      
      PRINTX r_total_aceptado.total_m4  
      PRINTX r_total_aceptado.aivs92_m4 
      PRINTX r_total_aceptado.aivs97_m4
      PRINTX r_total_aceptado.prc_m4
      --RECHAZADOS
      PRINTX r_total_rechazado.total_g   
      PRINTX r_total_rechazado.aivs92_g  
      PRINTX r_total_rechazado.aivs97_g 
      PRINTX r_total_rechazado.prc_g
      
      PRINTX r_total_rechazado.total_m1  
      PRINTX r_total_rechazado.aivs92_m1 
      PRINTX r_total_rechazado.aivs97_m1 
      PRINTX r_total_rechazado.prc_m1
      
      PRINTX r_total_rechazado.total_m2  
      PRINTX r_total_rechazado.aivs92_m2 
      PRINTX r_total_rechazado.aivs97_m2 
      PRINTX r_total_rechazado.prc_m2
      
      PRINTX r_total_rechazado.total_m4  
      PRINTX r_total_rechazado.aivs92_m4 
      PRINTX r_total_rechazado.aivs97_m4
      PRINTX r_total_rechazado.prc_m4


   ON EVERY ROW
      #Detalle Rechazos
      --rechazos marca 1
      FOR f=1 TO arr_rch_marca1.getLength()
         PRINTX arr_rch_marca1[f].total 
         PRINTX arr_rch_marca1[f].aivs92       
         PRINTX arr_rch_marca1[f].aivs97        
         PRINTX arr_rch_marca1[f].cve_causal  
         PRINTX arr_rch_marca1[f].desc_causal
      END FOR 
      --marca 2
      FOR f=1 TO arr_rch_marca2.getLength()
         PRINTX arr_rch_marca2[f].total 
         PRINTX arr_rch_marca2[f].aivs92       
         PRINTX arr_rch_marca2[f].aivs97        
         PRINTX arr_rch_marca2[f].cve_causal  
         PRINTX arr_rch_marca2[f].desc_causal
      END FOR 
      -- marca 4
      FOR f=1 TO arr_rch_marca4.getLength()
         PRINTX arr_rch_marca4[f].total 
         PRINTX arr_rch_marca4[f].aivs92       
         PRINTX arr_rch_marca4[f].aivs97        
         PRINTX arr_rch_marca4[f].cve_causal  
         PRINTX arr_rch_marca4[f].desc_causal
      END FOR 
      
END REPORT

FUNCTION archivo_salida()

   DEFINE v_qry_salida    STRING
   DEFINE v_detalle       STRING 
   DEFINE archivo         base.Channel
   DEFINE r_rechazo       RECORD
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      aivs92             CHAR(15),
      aivs97             CHAR(15),
      periodo_pago       CHAR(6),
      tpo_solicitud      CHAR(2),
      marca_procesar     CHAR(1),
      causal             CHAR(3)
   END RECORD

   LET archivo = base.Channel.create()
   
   LET v_arh_salida   = "CSSP_",TODAY USING "yyyymmdd","_resp.cti" CLIPPED 
   LET v_ruta_archivo = g_reg_modulo.ruta_envio CLIPPED,"/",v_arh_salida

   CALL archivo.openFile(v_ruta_archivo,"w")

   LET v_qry_salida = "\n SELECT id_derechohabiente, ",
                      "\n        nss, ",
                      "\n        aivs92, ",
                      "\n        aivs97, ",
                      "\n        periodo_pago, ",
                      "\n        tpo_solicitud, ",
                      "\n        marca_procesar, ",
                      "\n        diagnostico ",
                      "\n FROM   cre_solic_sdo ",
                      "\n WHERE  folio = ", v_folio ,
                      "\n AND    cod_resultado = 2;"

   PREPARE prp_arh_rch FROM v_qry_salida
   DECLARE crs_arh_rch CURSOR FOR  prp_arh_rch

   FOREACH crs_arh_rch INTO r_rechazo.id_derechohabiente,
                             r_rechazo.nss,
                             r_rechazo.aivs92,
                             r_rechazo.aivs97,
                             r_rechazo.periodo_pago,
                             r_rechazo.tpo_solicitud,
                             r_rechazo.marca_procesar,
                             r_rechazo.causal

      LET v_detalle =   r_rechazo.nss,
                        r_rechazo.aivs92 USING "&&&&&&&&&.&&&&&",
                        r_rechazo.aivs97 USING "&&&&&&&&&.&&&&&",
                        r_rechazo.periodo_pago USING "&&&&&&",
                        r_rechazo.tpo_solicitud,
                        r_rechazo.marca_procesar,
                        r_rechazo.causal USING "&&&"

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH 

   CALL archivo.close()
   
END FUNCTION 