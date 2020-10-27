--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

##############################################################################
#Módulo          => TIA                                                      #
#Programa        => TIAS11.4gl                                               #
#Objetivo        => Programa que genera los archivos de salida de la         #
#                   consulta de movimientos de decreto                       #
#Fecha Inicio    => 05 Septiembre 2013                                       #
#Registro de cambios                                                         #
#Autor            Fecha                Descripcion                           #
#Ivan Vega     12 Dic 2013        - se cambia la fecha de proceso por fecha  #
#                                   la fecha valor en las consultas, a fin   #
#                                   de que los movimientos reflejen la fecha #
#                                   de negocio y no la de proceso            #
#Registro de cambios                                                         #
#Autor              Fecha               Descripción                          #
#Carlos Benitez    23 Oct 2015    - Se incluyen validaciones para que cuadren#
#                                    los valores entre el PDF y los Archivos #
#                                    TXT                                     #
##############################################################################
DATABASE safre_viv

MAIN
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod,
       p_pid              LIKE bat_ctr_proceso.pid,
       p_proceso_cod      LIKE cat_proceso.proceso_cod,
       p_opera_cod        LIKE cat_operacion.opera_cod,
       p_folio            LIKE glo_folio.folio,
       p_nom_archivo      LIKE glo_ctr_archivo.nombre_archivo,
       v_sql              STRING, -- cadena de consulta
       v_movimiento       SMALLINT, -- clave de movimiento
       v_conteo           DECIMAL(9,0), -- conteo de registros
       v_suma_aivs        DECIMAL(24,6), -- suma de AIVs
       v_ano_buscado      SMALLINT,
       v_mes_buscado      SMALLINT,
       v_consistencia     SMALLINT,  -- indicador de consistencia/inconsistencia
       v_fecha_tope       DATE, -- fecha maxima de consulta
       v_fecha_inicio     DATE, -- fecha maxima de consulta
       v_fecha_fin        DATE, -- fecha maxima de consulta

       
       v_registro_archivo    RECORD -- registro de casos sin retiro
        edad                        VARCHAR(4),
        num_casos_consistente       DECIMAL(9,0),
        saldo_inicial_consistente   DECIMAL(24,6),
        movimientos_consistente     DECIMAL(24,6),
        saldo_total_consistente     DECIMAL(24,6),
        num_casos_inconsistente     DECIMAL(9,0),
        saldo_inicial_inconsistente DECIMAL(24,6),
        movimientos_inconsistente   DECIMAL(24,6),
        saldo_total_inconsistente   DECIMAL(24,6)
       END RECORD,
       v_aivs_saldo_anterior     DECIMAL(24,6),
       v_pesos_saldo_anterior    DECIMAL(22,2),
       v_encabezado_archivo      STRING,
       v_channel                 base.channel, -- apuntador de archivo
       v_ruta_envio              VARCHAR(40),
       v_ruta_completa           STRING,
       v_ano_nacimiento          VARCHAR(2),
       v_ano_calculo             SMALLINT,
       v_ano_nss                 CHAR(2),
       v_nss                     CHAR(11),
       v_nss_tmp                 STRING,
       v_ano_rfc                 CHAR(2),
       v_consulta                STRING,
       r_resultado_opera         SMALLINT,
       v_tipo_reporte            STRING, # tipo de reporte que se esta generado
       v_nombre_reporte          STRING,   # nomre reporte pdf
       v_ruta_listados           LIKE seg_modulo.ruta_listados,
       v_ruta_ejecutable         LIKE seg_modulo.ruta_listados,
       v_manejador_rpt           om.SaxDocumentHandler,
       v_conteo_registros        INTEGER

       DEFINE v_tpo_transaccion CHAR(1)


    

   -- se recuperan los parametros
   LET p_usuario_cod  = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET p_folio        = ARG_VAL(5)
   LET p_nom_archivo  = ARG_VAL(6)
   LET v_fecha_inicio = ARG_VAL(7)
   LET v_fecha_fin    = ARG_VAL(8)

   DISPLAY "fecha inicio: ",  v_fecha_inicio USING "dd-mm-yyyy"
   DISPLAY "fecha fin   : ",  v_fecha_fin    USING "dd-mm-yyyy"

   -- se da prioridad a la consulta
   DISPLAY "Preparando motor para ejecución de consulta..."
   EXECUTE IMMEDIATE "SET PDQPRIORITY HIGH"

   LET v_conteo_registros = 0
   SELECT COUNT(*)
     INTO v_conteo_registros
     FROM cta_decreto
    WHERE f_valor BETWEEN v_fecha_inicio AND v_fecha_fin

   IF(v_conteo_registros = 0)THEN
      DISPLAY "No se encontraron registros para fecha inicio ",v_fecha_inicio USING "dd-mm-yyyy"," y fecha fin ",v_fecha_fin USING "dd-mm-yyyy"
      EXIT PROGRAM
   END IF
      
   -- pasadas todas las validaciones, se procede a generar la consulta
   -- se obtiene la ruta envio
   SELECT ruta_envio,
          ruta_bin,
          ruta_listados
     INTO v_ruta_envio,
          v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "tia"

   IF(fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED||"/TIAS111.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nombre_reporte = p_usuario_cod CLIPPED, "-",
                             "TIAS11-", 
                             p_pid USING "&&&&&", "-", 
                             p_proceso_cod USING "&&&&&", "-", 
                             p_opera_cod USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nombre_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE         
      DISPLAY "No fué posible cargar plantilla de reporte"
      EXIT PROGRAM 
   END IF

   START REPORT rpt_analisis_decretos TO XML HANDLER v_manejador_rpt 
   
      -- se crea el nombre del archivo
      LET v_ruta_completa = v_ruta_envio CLIPPED, "/", "movimientos_decreto.txt"
      
      DISPLAY "Archivo creado: ", v_ruta_completa
      
      -- se crea el apuntador
      LET v_channel = base.Channel.create()   
      CALL v_channel.setDelimiter("|")   
      -- se abre el archivo
      CALL v_channel.openFile( v_ruta_completa, "w" )
         
      -- se prepara la consulta de montos historicos. Se busca aportaciones
      LET v_fecha_tope = v_fecha_fin         
      -- se realiza el ciclo de busqueda
      LET v_ano_buscado = YEAR(v_fecha_inicio)
      LET v_mes_buscado = MONTH(v_fecha_inicio)   
      -- el saldo anterior al iniciar es cero
      LET v_aivs_saldo_anterior  = 0
      LET v_pesos_saldo_anterior = 0
      
      
      
      
--    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- ==================================================================================================
      --
      -- CONSULTA DE NSS SIN RETIRO
      --
      -- ==================================================================================================
      
      LET v_tipo_reporte = "ABONOS" # Reporte de saldos/cargos iniciales y abonos positivos
      -- encabezado de casos sin retiro
      LET v_encabezado_archivo =  "Edad"
                                 ,"|Num. casos consistente"
                                 ,"|Saldo inicial consistente"
                                 ,"|Movimientos consistente"
                                 ,"|Saldo final consistente"
                                 ,"|Num. casos inconsistente"
                                 ,"|Saldo inicial inconsistente"
                                 ,"|Movimientos inconsistente"
                                 ,"|Saldo final inconsistente"
            
      -- se escribe el encabezado
      CALL v_channel.writeLine(v_encabezado_archivo)
        
      -- id_decreto que no tienen retiro/traspaso
      DISPLAY "Generando tabla temporal para NSS sin retiro"
      DISPLAY "\nInicio: ", TODAY,
              "\nHora  : ", CURRENT HOUR TO MINUTE
      LET v_sql = "DROP TABLE IF EXISTS tmp_afi_decreto_sin_retiro"
      EXECUTE IMMEDIATE v_sql
      
      LET v_sql = "CREATE TABLE tmp_afi_decreto_sin_retiro (id_decreto DECIMAL(9,0), ind_consistencia SMALLINT, nss varchar(11), rfc varchar(13)) IN tmp_2_dbs"
      EXECUTE IMMEDIATE v_sql
         
      DISPLAY "Obteniendo NSS sin retiro"
      LET v_sql = "\n INSERT INTO tmp_afi_decreto_sin_retiro",
                  "\n SELECT DISTINCT a.id_decreto, a.ind_consistencia, a.nss, a.rfc",
                  "\n FROM afi_decreto a,",
                  "\n      cta_decreto b ",
                  "\n WHERE a.id_decreto = b.id_decreto",
                  "\n AND   b.movimiento NOT IN (202,662,862,1052,362,592)", -- sin retiros
                  --"\n AND   b.movimiento IN (999,71,1009)", -- para optimisar solo buscamos los demás
                  "\n AND   b.f_valor BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
                  "\n AND   b.monto_acciones >=0 "
                     
      PREPARE sid_sinretiro FROM v_sql
      EXECUTE sid_sinretiro
      EXECUTE IMMEDIATE "UPDATE STATISTICS FOR TABLE tmp_afi_decreto_sin_retiro"
            
      -- se inicia el registro de casos sin retiro
      LET v_registro_archivo.edad                        = "0"
      LET v_registro_archivo.num_casos_consistente       = 0
      LET v_registro_archivo.saldo_inicial_consistente   = 0
      LET v_registro_archivo.movimientos_consistente     = 0
      LET v_registro_archivo.saldo_total_consistente     = 0
      LET v_registro_archivo.num_casos_inconsistente     = 0
      LET v_registro_archivo.saldo_inicial_inconsistente = 0
      LET v_registro_archivo.movimientos_inconsistente   = 0
      LET v_registro_archivo.saldo_total_inconsistente   = 0
      
      -- se crea la tabla temporal para calculo de saldo por edad
      DISPLAY "Generando tabla temporal para cálculo de saldo por edad para NSS sin retiro"
      EXECUTE IMMEDIATE "DROP TABLE IF EXISTS tmp_tia;"
      EXECUTE IMMEDIATE "create table tmp_tia (ano VARCHAR(2), movimiento smallint, consistencia smallint, conteo DECIMAL(9,0), aivs DECIMAL(24,6)) in tmp_2_dbs;"
      
      
      
      -- ===========================================================================
      -- APORTACIONES Y CARGA INICIAL DE DERECHOHABIENTES SIN RETIRO/TRASPASO
      -- ===========================================================================
      DISPLAY "Obteniendo saldo de NSS sin retiro..."
      LET v_sql = "\n SELECT                              ",
                  --"\n  d.nss[5,6],                        ",
                  "\n  d.nss,                        ",
                  "\n  d.rfc[5,6],                        ",
                  "\n  a.movimiento,                      ",
                  "\n  d.ind_consistencia,                ",
                  "\n  NVL(COUNT(*),0),                   ",
                  "\n  NVL(SUM(a.monto_acciones),0)       ",
                  "\n FROM                                ",
                  "\n  cta_decreto a,                     ",
                  "\n  tmp_afi_decreto_sin_retiro d       ",
                  "\n WHERE                               ",
                  "\n  a.f_valor between '", v_fecha_inicio, "' AND '", v_fecha_fin, "'", -- corte por ano
                  "\n AND                                 ",
                  "\n  a.id_decreto = d.id_decreto        ", -- derechohabientes que no tienen retiro/traspaso
                  "\n AND                                 ",
                  "\n  a.movimiento IN (999,71,1009)      ", -- saldo inicial, Aportaciones SAR92 y carag inicial decreto 
                  "\n AND                                 ",
                  "\n  a.monto_acciones >=0               ", -- solo positivos
                  "\n GROUP BY 1,2,3,4                    "
      
      -- se prepara y ejecuta
      PREPARE sid_aportaciones FROM v_sql
      
      DECLARE cur_aportaciones CURSOR FOR sid_aportaciones
      LET v_consulta = "INSERT INTO tmp_tia", 
                       " VALUES (?,?,?,?,?)"
      PREPARE prp_almacena_tmp_tia FROM v_consulta
            
      -- para cada registro de aportaciones encontrado
      FOREACH cur_aportaciones INTO --v_ano_nss      ,
                                    v_nss          ,
                                    v_ano_rfc      ,
                                    v_movimiento   ,
                                    v_consistencia ,
                                    v_conteo       ,
                                    v_suma_aivs    

         # valida la longitud del NSS a 11 posiciones, de lo contrario rellena con ceros a la izquierda
         # se recupera el año de nacimiento
         LET v_nss_tmp = v_nss
         LET v_nss_tmp = v_nss_tmp.trim()
         IF(v_nss_tmp.getLength() = 11)THEN
            LET v_ano_nss = v_nss[5,6]
         ELSE
            LET v_nss = v_nss_tmp USING "&&&&&&&&&&&"
            LET v_ano_nss = v_nss[5,6]
         END IF
         
         -- se verifica si se pudo obtener ano del NSS
         IF ( v_ano_nss IS NOT NULL AND f_cadena_es_numerica(v_ano_nss) ) THEN
            # verificamos que el año del rfc sea número
            IF ( v_ano_rfc IS NOT NULL AND f_cadena_es_numerica(v_ano_rfc)) THEN
               # compara si los años de nss y rfc son iguales y asigna el año del nss
               # de lo contrario se calcula la edad con el año del rfc
               IF(v_ano_nss = v_ano_rfc)THEN
                  LET v_registro_archivo.edad = v_ano_nss
               ELSE
                  LET v_registro_archivo.edad = v_ano_rfc
               END IF               
            ELSE
               LET v_registro_archivo.edad = v_ano_nss
            END IF
         ELSE
            -- no se pudo obtener el ano del NSS, se prueba con el del RFC
            IF ( v_ano_rfc IS NOT NULL AND f_cadena_es_numerica(v_ano_rfc) ) THEN
               LET v_registro_archivo.edad = v_ano_rfc
            ELSE
               -- no se pudo determinar
               LET v_registro_archivo.edad = "NA"
            END IF
         END IF
         
         -- se inserta el registro en la tabla temporal para consultar por edad    
         EXECUTE prp_almacena_tmp_tia USING v_registro_archivo.edad,
                                            v_movimiento,
                                            v_consistencia,
                                            v_conteo,
                                            v_suma_aivs
      
      END FOREACH
      FREE cur_aportaciones

      EXECUTE IMMEDIATE "CREATE INDEX ix_tmp_tia ON tmp_tia(ano);"
      EXECUTE IMMEDIATE "UPDATE STATISTICS FOR TABLE tmp_tia;"      
      
      LET v_consulta = "SELECT movimiento, consistencia, sum(conteo), sum(aivs)",
                       "  FROM tmp_tia",
                       " WHERE ano = ?",
                       " GROUP BY 1,2",
                       " ORDER BY 1,2"
      PREPARE prp_recupera_tmp_tia FROM v_consulta
      DECLARE cur_nsssinretiro CURSOR FOR prp_recupera_tmp_tia
      
      DECLARE cur_nssedad CURSOR FOR
      SELECT DISTINCT ano
      FROM tmp_tia
      ORDER BY ano
      
      FOREACH cur_nssedad INTO v_ano_nacimiento
      
         -- se inicia el registro de archivo
         LET v_registro_archivo.edad                        = "0"
         LET v_registro_archivo.num_casos_consistente       = 0
         LET v_registro_archivo.saldo_inicial_consistente   = 0
         LET v_registro_archivo.movimientos_consistente     = 0
         LET v_registro_archivo.saldo_total_consistente     = 0
         LET v_registro_archivo.num_casos_inconsistente     = 0
         LET v_registro_archivo.saldo_inicial_inconsistente = 0
         LET v_registro_archivo.movimientos_inconsistente   = 0
         LET v_registro_archivo.saldo_total_inconsistente   = 0
      
         
         FOREACH cur_nsssinretiro USING v_ano_nacimiento
                                   INTO v_movimiento    ,
                                        v_consistencia  ,
                                        v_conteo        ,
                                        v_suma_aivs
         
            -- se calcula la edad
            IF ( v_ano_nacimiento = "NA" ) THEN
               LET v_registro_archivo.edad = v_ano_nacimiento
            ELSE
               LET v_ano_calculo = YEAR(v_fecha_fin) - (v_ano_nacimiento + 1900)
               LET v_registro_archivo.edad = v_ano_calculo
            END IF
            
            -- numero de casos consistente
            IF ( v_consistencia = 1 ) THEN
               LET v_registro_archivo.num_casos_consistente = v_registro_archivo.num_casos_consistente + v_conteo
            ELSE
               LET v_registro_archivo.num_casos_inconsistente = v_registro_archivo.num_casos_inconsistente + v_conteo
            END IF
                    
            -- 999  saldo inicial
            -- 1009 carga inicial decreto
            IF ( v_movimiento = 999 OR v_movimiento = 1009 ) THEN
               -- consistente
               IF ( v_consistencia = 1 ) THEN
                  LET v_registro_archivo.saldo_inicial_consistente = v_registro_archivo.saldo_inicial_consistente + v_suma_aivs
               ELSE
                  LET v_registro_archivo.saldo_inicial_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_suma_aivs
               END IF
            ELSE
               -- suma de movimientos consistente
               IF ( v_consistencia = 1 ) THEN
                  LET v_registro_archivo.movimientos_consistente = v_registro_archivo.movimientos_consistente + v_suma_aivs
               ELSE
                  LET v_registro_archivo.movimientos_inconsistente = v_registro_archivo.movimientos_inconsistente + v_suma_aivs
               END IF
            END IF    

          

                -- saldo total por edad
            LET v_registro_archivo.saldo_total_consistente   = v_registro_archivo.saldo_inicial_consistente + v_registro_archivo.movimientos_consistente
            LET v_registro_archivo.saldo_total_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_registro_archivo.movimientos_inconsistente
                   
         END FOREACH

         # se envia a reporte pdf
         OUTPUT TO REPORT rpt_analisis_decretos(v_tipo_reporte,v_fecha_inicio,v_fecha_fin,v_registro_archivo.*,p_usuario_cod)
         -- se escribe en archivo
         CALL v_channel.write([v_registro_archivo.*])
      
      END FOREACH
      FREE cur_nsssinretiro
      FREE cur_nssedad
      
      -- se cierra el archivo
      CALL v_channel.close()
      
      DISPLAY "FIN: ", TODAY,
              "\nHora  : ", CURRENT HOUR TO MINUTE,
              "\n"
      
      
--    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- ==================================================================================================
      --
      -- CONSULTA DE NSS CON RETIRO PERO SIN SALDOS NEGATIVOS
      --
      -- ==================================================================================================
      
      LET v_tipo_reporte = "RETIROS" # Reporte de retiros sin saldos/cargos iniciales negativos 
      -- se crea el nombre del archivo
      LET v_ruta_completa = v_ruta_envio CLIPPED, "/", "retiros_decreto.txt"  
      DISPLAY "Archivo creado: ", v_ruta_completa
      
      -- se abre el archivo
      CALL v_channel.openFile( v_ruta_completa, "w" )
      
      -- encabezado de casos sin retiro
      LET v_encabezado_archivo =  "Edad"
                                 ,"|Num. casos consistente"
                                 ,"|Saldo inicial consistente"
                                 ,"|Movimientos consistente"
                                 ,"|Saldo final consistente"
                                 ,"|Num. casos inconsistente"
                                 ,"|Saldo inicial inconsistente"
                                 ,"|Movimientos inconsistente"
                                 ,"|Saldo final inconsistente"
            
      -- se escribe el encabezado
      CALL v_channel.writeLine(v_encabezado_archivo)
        
      -- id_decreto que no tienen retiro/traspaso
      DISPLAY "Generando tabla temporal para NSS con retiro"
      DISPLAY "\nInicio: ", TODAY,
              "\nHora  : ", CURRENT HOUR TO MINUTE
              
      LET v_sql = "DROP TABLE IF EXISTS tmp_afi_decreto_con_retiro"
      EXECUTE IMMEDIATE v_sql
      LET v_sql = "CREATE TABLE tmp_afi_decreto_con_retiro (id_decreto DECIMAL(9,0), ind_consistencia SMALLINT, nss varchar(11), rfc varchar(13)) IN tmp_2_dbs"
      EXECUTE IMMEDIATE v_sql
      
         
      DISPLAY "Obteniendo NSS con retiro"
      LET v_sql = "\n INSERT INTO tmp_afi_decreto_con_retiro",
                  "\n SELECT DISTINCT a.id_decreto, a.ind_consistencia, a.nss, a.rfc",
                  "\n FROM afi_decreto a,",
                  "\n      cta_decreto b ",
                  "\n WHERE a.id_decreto = b.id_decreto",
                  "\n AND   b.movimiento IN (202,662,862,1052,362,592)",
                  "\n AND   b.f_valor BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
                     
      PREPARE sid_conretiro FROM v_sql
      EXECUTE sid_conretiro
      EXECUTE IMMEDIATE "UPDATE STATISTICS FOR TABLE tmp_afi_decreto_con_retiro"
            
      -- se inicia el registro de casos sin retiro
      LET v_registro_archivo.edad                        = "0"
      LET v_registro_archivo.num_casos_consistente       = 0
      LET v_registro_archivo.saldo_inicial_consistente   = 0
      LET v_registro_archivo.movimientos_consistente     = 0
      LET v_registro_archivo.saldo_total_consistente     = 0
      LET v_registro_archivo.num_casos_inconsistente     = 0
      LET v_registro_archivo.saldo_inicial_inconsistente = 0
      LET v_registro_archivo.movimientos_inconsistente   = 0
      LET v_registro_archivo.saldo_total_inconsistente   = 0
      
      -- se crea la tabla temporal para calculo de saldo por edad
      DISPLAY "Generando tabla temporal para cálculo de saldo por edad para NSS con retiro"
      EXECUTE IMMEDIATE "DROP TABLE IF EXISTS tmp_tia;"
      EXECUTE IMMEDIATE "create table tmp_tia (ano VARCHAR(2), movimiento smallint, consistencia smallint, conteo DECIMAL(9,0), aivs DECIMAL(24,6)) in tmp_2_dbs;"

      
      
      -- ===========================================================================
      -- APORTACIONES, CARGA INICIAL Y RETIROS Y TRASPADOS DE DERECHOHABIENTES QUE DESDE UN INICIO TUVIERON SALDO POSITIVO
      -- ===========================================================================
      DISPLAY "Obteniendo saldo de NSS con retiro..."
      LET v_sql = "\n SELECT                              ",
                  --"\n  d.nss[5,6],                        ",
                  "\n  d.nss,                        ",
                  "\n  d.rfc[5,6],                        ",
                  "\n  a.movimiento,                      ",
                  "\n  d.ind_consistencia,                ",
                  "\n  NVL(COUNT(*),0),                   ",
                  "\n  NVL(SUM(a.monto_acciones),0)       ",
                  "\n FROM                                ",
                  "\n  cta_decreto a,                     ",
                  "\n  tmp_afi_decreto_con_retiro d       ",
                  "\n WHERE                               ",
                  "\n  a.f_valor between '", v_fecha_inicio, "' AND '", v_fecha_fin, "'", -- corte por ano
                  "\n AND                                 ",
                  "\n  a.id_decreto = d.id_decreto        ", -- derechohabientes que no tienen retiro/traspaso 
                  "\n AND                                 ",
                  "\n  a.movimiento IN (1009,999,71,202,662,862,1052,362,592)           ", -- Aportaciones SAR92 y carga inicial
                  "\n GROUP BY 1,2,3,4                    "
      
      
      -- se prepara y ejecuta
      PREPARE sid_retiros FROM v_sql   
      DECLARE cur_retiros CURSOR FOR sid_retiros
      
      LET v_consulta = "INSERT INTO tmp_tia ",
                       "VALUES (?,?,?,?,?)"
      PREPARE prp_almacena_tmp_tia2 FROM v_consulta
            
      -- para cada registro de aportaciones encontrado
      FOREACH cur_retiros INTO --v_ano_nss      ,
                               v_nss          ,
                               v_ano_rfc      ,
                               v_movimiento   ,
                               v_consistencia ,
                               v_conteo       ,
                               v_suma_aivs

         # valida la longitud del NSS a 11 posiciones, de lo contrario rellena con ceros a la izquierda
         LET v_nss_tmp = v_nss
         LET v_nss_tmp = v_nss_tmp.trim()
         IF(v_nss_tmp.getLength() = 11)THEN
            LET v_ano_nss = v_nss[5,6]
         ELSE
            LET v_nss = v_nss_tmp USING "&&&&&&&&&&&"
            LET v_ano_nss = v_nss[5,6]
         END IF    
      
         -- se verifica si se pudo obtener ano del NSS
         IF ( v_ano_nss IS NOT NULL AND f_cadena_es_numerica(v_ano_nss) ) THEN
            # verificamos que el año del rfc sea número
            IF ( v_ano_rfc IS NOT NULL AND f_cadena_es_numerica(v_ano_rfc)) THEN
               # compara si los años de nss y rfc son iguales y asigna el año del nss
               # de lo contrario se calcula la edad con el año del rfc
               IF(v_ano_nss = v_ano_rfc)THEN
                  LET v_registro_archivo.edad = v_ano_nss
               ELSE
                  LET v_registro_archivo.edad = v_ano_rfc
               END IF               
            ELSE
               LET v_registro_archivo.edad = v_ano_nss
            END IF
         ELSE
            -- no se pudo obtener el ano del NSS, se prueba con el del RFC
            IF ( v_ano_rfc IS NOT NULL AND f_cadena_es_numerica(v_ano_rfc) ) THEN
               LET v_registro_archivo.edad = v_ano_rfc
            ELSE
               -- no se pudo determinar
               LET v_registro_archivo.edad = "NA"
            END IF
         END IF
      
         EXECUTE prp_almacena_tmp_tia2 USING v_registro_archivo.edad,
                                             v_movimiento   ,
                                             v_consistencia ,
                                             v_conteo       ,
                                             v_suma_aivs
      
      END FOREACH
      FREE cur_retiros

      EXECUTE IMMEDIATE "CREATE INDEX ix_tmp_tia ON tmp_tia(ano);"
      EXECUTE IMMEDIATE "UPDATE STATISTICS FOR TABLE tmp_tia;"
      
      LET v_consulta = "SELECT movimiento, consistencia, sum(conteo), sum(aivs)",
                       "  FROM tmp_tia",
                       " WHERE ano = ?",
                       " GROUP BY 1,2",
                       " ORDER BY 1,2"
      PREPARE prp_recupera_tmp_tia2 FROM v_consulta
      DECLARE cur_nssconretiro CURSOR FOR prp_recupera_tmp_tia2
      
      DECLARE cur_nssedadretiro CURSOR FOR
      SELECT DISTINCT ano
      FROM tmp_tia
      ORDER BY ano
      
      FOREACH cur_nssedadretiro INTO v_ano_nacimiento
      
         -- se inicia el registro de archivo
         LET v_registro_archivo.edad                        = "0"
         LET v_registro_archivo.num_casos_consistente       = 0
         LET v_registro_archivo.saldo_inicial_consistente   = 0
         LET v_registro_archivo.movimientos_consistente     = 0
         LET v_registro_archivo.saldo_total_consistente     = 0
         LET v_registro_archivo.num_casos_inconsistente     = 0
         LET v_registro_archivo.saldo_inicial_inconsistente = 0
         LET v_registro_archivo.movimientos_inconsistente   = 0
         LET v_registro_archivo.saldo_total_inconsistente   = 0
      
         FOREACH cur_nssconretiro USING v_ano_nacimiento
                                   INTO v_movimiento    ,
                                        v_consistencia  ,
                                        v_conteo        ,
                                        v_suma_aivs
         
            -- se calcula la edad
            IF ( v_ano_nacimiento = "NA" ) THEN
               LET v_registro_archivo.edad = v_ano_nacimiento
            ELSE
               LET v_ano_calculo = YEAR(v_fecha_fin) - (v_ano_nacimiento + 1900)
               LET v_registro_archivo.edad = v_ano_calculo
            END IF
            
            -- numero de casos consistente
            IF ( v_consistencia = 1 ) THEN
               # sólo contamos los movimientos que son retiros
               IF (v_movimiento = 592 OR v_movimiento = 362 OR v_movimiento = 662 OR v_movimiento = 202)THEN
                  LET v_registro_archivo.num_casos_consistente = v_registro_archivo.num_casos_consistente + v_conteo
               END IF
            ELSE
               # sólo contamos los movimientos que son retiros
               IF (v_movimiento = 592 OR v_movimiento = 362 OR v_movimiento = 662 OR v_movimiento = 202)THEN
                  LET v_registro_archivo.num_casos_inconsistente = v_registro_archivo.num_casos_inconsistente + v_conteo
               END IF
            END IF
            
            # 999  saldo inicial
            # 71   abono SAR
            # 1009 caraga inicial decreto
            IF ( v_movimiento = 999 OR v_movimiento = 1009 OR v_movimiento=71) THEN--Se añadio el 71 ya que es abono y no estaba contemplado CABC
               -- consistente
               IF ( v_consistencia = 1 ) THEN
                  LET v_registro_archivo.saldo_inicial_consistente = 0 --g- v_registro_archivo.saldo_inicial_consistente + v_suma_aivs
                 --   LET v_registro_archivo.saldo_inicial_consistente = v_registro_archivo.saldo_inicial_consistente + v_suma_aivs
                ELSE
                  LET v_registro_archivo.saldo_inicial_inconsistente = 0 --g- v_registro_archivo.saldo_inicial_inconsistente + v_suma_aivs
                 --   LET v_registro_archivo.saldo_inicial_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_suma_aivs
                  END IF
            ELSE
               -- suma de movimientos
               -- consistente
               IF ( v_consistencia = 1 ) THEN
                  LET v_registro_archivo.movimientos_consistente = v_registro_archivo.movimientos_consistente + v_suma_aivs
               ELSE
                  LET v_registro_archivo.movimientos_inconsistente = v_registro_archivo.movimientos_inconsistente + v_suma_aivs

               END IF
            END IF


                     
            -- saldo total por edad
            LET v_registro_archivo.saldo_total_consistente   = v_registro_archivo.saldo_inicial_consistente + v_registro_archivo.movimientos_consistente
            LET v_registro_archivo.saldo_total_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_registro_archivo.movimientos_inconsistente


            
         END FOREACH

         # se envia a reporte pdf
         OUTPUT TO REPORT rpt_analisis_decretos(v_tipo_reporte,v_fecha_inicio,v_fecha_fin,v_registro_archivo.*,p_usuario_cod)
         -- se escribe en archivo
   
          LET v_registro_archivo.saldo_inicial_consistente=0 --Añadido por CABC para consistencia entre PDF y txt
          LET v_registro_archivo.saldo_inicial_inconsistente=0 --Añadido por CABC para consistencia entre PDF y txt
          
         CALL v_channel.write([v_registro_archivo.*])
      
      END FOREACH



      
      FREE cur_nssconretiro
      FREE cur_nssedadretiro
      
      -- se cierra el archivo
      CALL v_channel.close()
      
      DISPLAY "FIN: ", TODAY,
              "\nHora  : ", CURRENT HOUR TO MINUTE,
              "\n"
      
      
--    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- ==================================================================================================
      --
      -- CONSULTA DE NSS CON SALDO NEGATIVO DESDE ORIGEN
      --
      -- ==================================================================================================
      
      LET v_tipo_reporte = "NEGATIVOS" # Reporte saldos/cargas iniciales negativos
      -- se crea el nombre del archivo
      LET v_ruta_completa = v_ruta_envio CLIPPED, "/", "saldos_negativos_decreto.txt"  
      DISPLAY "Archivo creado: ", v_ruta_completa
      
      -- se abre el archivo
      CALL v_channel.openFile( v_ruta_completa, "w" )
      
      -- encabezado de casos sin retiro
      LET v_encabezado_archivo =  "Edad"
                                 ,"|Num. casos consistente"
                                 ,"|Saldo inicial consistente"
                                 ,"|Movimientos consistente"
                                 ,"|Saldo final consistente"
                                 ,"|Num. casos inconsistente"
                                 ,"|Saldo inicial inconsistente"
                                 ,"|Movimientos inconsistente"
                                 ,"|Saldo final inconsistente"
            
      -- se escribe el encabezado
      CALL v_channel.writeLine(v_encabezado_archivo)
        
      -- id_decreto que no tienen retiro/traspaso
      DISPLAY "Generando tabla temporal para NSS con saldo negativo desde origen"
      DISPLAY "\nInicio: ", TODAY,
              "\nHora  : ", CURRENT HOUR TO MINUTE
      LET v_sql = "DROP TABLE IF EXISTS tmp_afi_decreto_negativo"
      EXECUTE IMMEDIATE v_sql
      
      LET v_sql = "CREATE TABLE tmp_afi_decreto_negativo (id_decreto DECIMAL(9,0), ind_consistencia SMALLINT, nss varchar(11), rfc varchar(13)) IN tmp_2_dbs"
      EXECUTE IMMEDIATE v_sql
         
      DISPLAY "Obteniendo NSS con saldo negativo desde origen"
      LET v_sql = "\n INSERT INTO tmp_afi_decreto_negativo",
                  "\n SELECT DISTINCT a.id_decreto, a.ind_consistencia, a.nss, a.rfc",
                  "\n FROM afi_decreto a,",
                  "\n      cta_decreto b ",
                  "\n WHERE a.id_decreto = b.id_decreto",
                  "\n AND   b.movimiento IN (999,1009)",
                  "\n AND   b.monto_acciones < 0",
                  "\n AND   b.f_valor BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
                     
      PREPARE sid_connegativo FROM v_sql
      EXECUTE sid_connegativo
      EXECUTE IMMEDIATE "UPDATE STATISTICS FOR TABLE tmp_afi_decreto_negativo"
            
      -- se inicia el registro de casos sin retiro
      LET v_registro_archivo.edad                        = "0"
      LET v_registro_archivo.num_casos_consistente       = 0
      LET v_registro_archivo.saldo_inicial_consistente   = 0
      LET v_registro_archivo.movimientos_consistente     = 0
      LET v_registro_archivo.saldo_total_consistente     = 0
      LET v_registro_archivo.num_casos_inconsistente     = 0
      LET v_registro_archivo.saldo_inicial_inconsistente = 0
      LET v_registro_archivo.movimientos_inconsistente   = 0
      LET v_registro_archivo.saldo_total_inconsistente   = 0
      
      -- se crea la tabla temporal para calculo de saldo por edad
      DISPLAY "Generando tabla temporal para cálculo de saldo de NSS con saldo negativo"
      EXECUTE IMMEDIATE "DROP TABLE IF EXISTS tmp_tia;"
      EXECUTE IMMEDIATE "create table tmp_tia (ano VARCHAR(2), movimiento smallint, consistencia smallint, conteo DECIMAL(9,0), aivs DECIMAL(24,6)) in tmp_2_dbs;"

      
      
      -- ===========================================================================
      -- CARGA INICIAL DE NSS QUE VIENEN CON SALDO NEGATIVO DESDE ORIGEN
      -- ===========================================================================
      DISPLAY "Obteniendo saldo de NSS con saldo negativo..."
      LET v_sql = "\n SELECT                              ",
                  --"\n  d.nss[5,6],                        ",
                  "\n  d.nss,                        ",
                  "\n  d.rfc[5,6],                        ",
                  "\n  a.movimiento,                      ",
                  "\n  d.ind_consistencia,                ",
                  "\n  NVL(COUNT(*),0),                   ",
                  "\n  NVL(SUM(a.monto_acciones),0)       ",
                  "\n FROM                                ",
                  "\n  cta_decreto a,                     ",
                  "\n  tmp_afi_decreto_negativo d         ",
                  "\n WHERE                               ",
                  "\n  a.f_valor between '", v_fecha_inicio, "' AND '", v_fecha_fin, "'", -- corte por ano
                  "\n AND                                 ",
                  "\n  a.id_decreto = d.id_decreto        ", -- derechohabientes que no tienen retiro/traspaso
                  "\n AND                                 ",
                  "\n  a.monto_acciones < 0               ",
                  "\n AND                                 ",
                  "\n  a.movimiento IN (999,1009)         ", -- saldo inicial y carga inicial decreto
                  "\n GROUP BY 1,2,3,4                    "
      
                  
      -- se despliega la consulta de decreto
      --DISPLAY v_sql
      
      -- se prepara y ejecuta
      PREPARE sid_negativos FROM v_sql
      
      DECLARE cur_negativos CURSOR FOR sid_negativos
      
      LET v_consulta = "INSERT INTO tmp_tia ",
                       "VALUES (?,?,?,?,?)"
      PREPARE prp_almacena_tmp_tia3 FROM v_consulta 
                                     
      -- para cada registro de aportaciones encontrado
      FOREACH cur_negativos INTO --v_ano_nss      ,
                                 v_nss          ,
                                 v_ano_rfc      ,
                                 v_movimiento   ,
                                 v_consistencia ,
                                 v_conteo       ,
                                 v_suma_aivs    
      
         # valida la longitud del NSS a 11 posiciones, de lo contrario rellena con ceros a la izquierda
         LET v_nss_tmp = v_nss
         LET v_nss_tmp = v_nss_tmp.trim()
         IF(v_nss_tmp.getLength() = 11)THEN
            LET v_ano_nss = v_nss[5,6]
         ELSE
            LET v_nss = v_nss_tmp USING "&&&&&&&&&&&"
            LET v_ano_nss = v_nss[5,6]
         END IF    
      
         -- se verifica si se pudo obtener ano del NSS
         IF ( v_ano_nss IS NOT NULL AND f_cadena_es_numerica(v_ano_nss) ) THEN
            # verificamos que el año del rfc sea número
            IF ( v_ano_rfc IS NOT NULL AND f_cadena_es_numerica(v_ano_rfc)) THEN
               # compara si los años de nss y rfc son iguales y asigna el año del nss
               # de lo contrario se calcula la edad con el año del rfc
               IF(v_ano_nss = v_ano_rfc)THEN
                  LET v_registro_archivo.edad = v_ano_nss
               ELSE
                  LET v_registro_archivo.edad = v_ano_rfc
               END IF               
            ELSE
               LET v_registro_archivo.edad = v_ano_nss
            END IF
         ELSE
            -- no se pudo obtener el ano del NSS, se prueba con el del RFC
            IF ( v_ano_rfc IS NOT NULL AND f_cadena_es_numerica(v_ano_rfc) ) THEN
               LET v_registro_archivo.edad          = v_ano_rfc
            ELSE
               -- no se pudo determinar
               LET v_registro_archivo.edad          = "NA"
            END IF
         END IF
      
         EXECUTE prp_almacena_tmp_tia3 USING v_registro_archivo.edad,
                                             v_movimiento   ,
                                             v_consistencia ,
                                             v_conteo       ,
                                             v_suma_aivs
      
      END FOREACH
      FREE cur_negativos

      EXECUTE IMMEDIATE "CREATE INDEX ix_tmp_tia ON tmp_tia(ano);"
      EXECUTE IMMEDIATE "UPDATE STATISTICS FOR TABLE tmp_tia;"
      
      DECLARE cur_nssnegativo CURSOR FOR
      SELECT DISTINCT ano
      FROM tmp_tia
      ORDER BY ano
      
      LET v_consulta = "SELECT movimiento, consistencia, sum(conteo), sum(aivs)",
                       "  FROM tmp_tia",
                       " WHERE ano = ?",
                       " GROUP BY 1,2",
                       " ORDER BY 1,2"
      PREPARE prp_recupera_tmp_tia3 FROM v_consulta
      DECLARE cur_nssconnegativo CURSOR FOR prp_recupera_tmp_tia3
      
      FOREACH cur_nssnegativo INTO v_ano_nacimiento
      
         -- se inicia el registro de archivo
         LET v_registro_archivo.edad                        = "0"
         LET v_registro_archivo.num_casos_consistente       = 0
         LET v_registro_archivo.saldo_inicial_consistente   = 0
         LET v_registro_archivo.movimientos_consistente     = 0
         LET v_registro_archivo.saldo_total_consistente     = 0
         LET v_registro_archivo.num_casos_inconsistente     = 0
         LET v_registro_archivo.saldo_inicial_inconsistente = 0
         LET v_registro_archivo.movimientos_inconsistente   = 0
         LET v_registro_archivo.saldo_total_inconsistente   = 0
      
         FOREACH cur_nssconnegativo USING v_ano_nacimiento
                                     INTO v_movimiento    ,
                                          v_consistencia  ,
                                          v_conteo        ,
                                          v_suma_aivs
         
            -- se calcula la edad
            IF ( v_ano_nacimiento = "NA" ) THEN
               LET v_registro_archivo.edad = v_ano_nacimiento
            ELSE
               LET v_ano_calculo = YEAR(v_fecha_fin) - (v_ano_nacimiento + 1900)
               LET v_registro_archivo.edad = v_ano_calculo
            END IF
            
            -- numero de casos
            -- consistente
            IF ( v_consistencia = 1 ) THEN
               LET v_registro_archivo.num_casos_consistente = v_registro_archivo.num_casos_consistente + v_conteo
            ELSE
               LET v_registro_archivo.num_casos_inconsistente = v_registro_archivo.num_casos_inconsistente + v_conteo
            END IF
            
            -- el saldo inicial es su carga inicial
            IF ( v_movimiento = 999 OR v_movimiento = 1009) THEN
               -- consistente
               IF ( v_consistencia = 1 ) THEN
                  LET v_registro_archivo.saldo_inicial_consistente = v_registro_archivo.saldo_inicial_consistente + v_suma_aivs
               ELSE
                  LET v_registro_archivo.saldo_inicial_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_suma_aivs
               END IF
            ELSE
               -- suma de movimientos
               -- consistente
               IF ( v_consistencia = 1 ) THEN
                  LET v_registro_archivo.movimientos_consistente = v_registro_archivo.movimientos_consistente + v_suma_aivs
               ELSE
                  LET v_registro_archivo.movimientos_inconsistente = v_registro_archivo.movimientos_inconsistente + v_suma_aivs
               END IF
            END IF

          

                     
            -- saldo total por edad
            LET v_registro_archivo.saldo_total_consistente   = v_registro_archivo.saldo_inicial_consistente + v_registro_archivo.movimientos_consistente
            LET v_registro_archivo.saldo_total_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_registro_archivo.movimientos_inconsistente
                   
         END FOREACH

         # se envia a reporte pdf
         OUTPUT TO REPORT rpt_analisis_decretos(v_tipo_reporte,v_fecha_inicio,v_fecha_fin,v_registro_archivo.*,p_usuario_cod)
         -- se escribe en archivo
         CALL v_channel.write([v_registro_archivo.*])
      
      END FOREACH
      FREE cur_nssconnegativo
      FREE cur_nssnegativo
      
      -- se cierra el archivo
      CALL v_channel.close()
      
      DISPLAY "FIN: ", TODAY,
              "\nHora  : ", CURRENT HOUR TO MINUTE,
              "\n"
      
      
--    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- ==================================================================================================
      --
      -- RESUMEN DE RECURSOS DISPONIBLES POR EDAD
      --
      -- ==================================================================================================
      
      LET v_tipo_reporte = "DISPONIBLE" # Reporte de saldos disponibles
      -- se crea el nombre del archivo
      LET v_ruta_completa = v_ruta_envio CLIPPED, "/", "saldo_disponible_decreto.txt"  
      DISPLAY "Archivo creado: ", v_ruta_completa
      
      -- se abre el archivo
      CALL v_channel.openFile( v_ruta_completa, "w" )
      
      -- encabezado de casos sin retiro
      LET v_encabezado_archivo =  "Edad"
                                 ,"|Num. casos consistente"
                                 ,"|Saldo inicial consistente"
                                 ,"|Movimientos consistente"
                                 ,"|Saldo final consistente"
                                 ,"|Num. casos inconsistente"
                                 ,"|Saldo inicial inconsistente"
                                 ,"|Movimientos inconsistente"
                                 ,"|Saldo final inconsistente"
            
      -- se escribe el encabezado
      CALL v_channel.writeLine(v_encabezado_archivo)
        
      -- id_decreto que no tienen retiro/traspaso
      DISPLAY "Generando tabla temporal para NSS con saldo disponible"
      DISPLAY "\nInicio: ", TODAY,
              "\nHora  : ", CURRENT HOUR TO MINUTE
            
      -- se inicia el registro de casos sin retiro
      LET v_registro_archivo.edad                        = "0"
      LET v_registro_archivo.num_casos_consistente       = 0
      LET v_registro_archivo.saldo_inicial_consistente   = 0
      LET v_registro_archivo.movimientos_consistente     = 0
      LET v_registro_archivo.saldo_total_consistente     = 0
      LET v_registro_archivo.num_casos_inconsistente     = 0
      LET v_registro_archivo.saldo_inicial_inconsistente = 0
      LET v_registro_archivo.movimientos_inconsistente   = 0
      LET v_registro_archivo.saldo_total_inconsistente   = 0
      
      -- se crea la tabla temporal para calculo de saldo por edad
      DISPLAY "Generando tabla temporal para cálculo de saldo disponible por edad"
      EXECUTE IMMEDIATE "DROP TABLE IF EXISTS tmp_tia;"
      EXECUTE IMMEDIATE "create table tmp_tia (ano VARCHAR(2), movimiento smallint, consistencia smallint, conteo DECIMAL(9,0), aivs DECIMAL(24,6),tipo_transaccion CHAR(1)) in tmp_2_dbs;"

      
      
      -- ===========================================================================
      -- SALDO DISPONIBLE POR EDAD
      -- ===========================================================================

--===============Modificado por CABC para identificar solo los movimientos de retiro, abonos y negativos ================================--      
      DISPLAY "Obteniendo saldo disponible por edad..."
      LET v_sql = "\n SELECT                              ",
                  --"\n  d.nss[5,6],                        ",
                  "\n  d.nss,                        ",
                  "\n  d.rfc[5,6],                        ",
                  "\n  a.movimiento,                      ",
                  "\n  d.ind_consistencia,                ",
                  "\n  NVL(COUNT(*),0),                   ",
                  "\n  NVL(SUM(a.monto_acciones),0),       ",
                    "\n CASE WHEN a.monto_acciones<0      ",
                   "\n AND (A.MOVIMIENTO=999 OR A.MOVIMIENTO=1009) THEN 'N' ",
                    "\n WHEN A.MONTO_ACCIONES>=0 AND (A.MOVIMIENTO=999 OR A.MOVIMIENTO=1009 OR ",
                    "\n A.MOVIMIENTO=71) THEN 'A' WHEN (A.MOVIMIENTO=202 OR A.MOVIMIENTO=662 OR A.MOVIMIENTO=862  ",
                    "\n OR A.MOVIMIENTO=1052 OR A.MOVIMIENTO=362 OR A.MOVIMIENTO=592) THEN 'R' ",
                    "\n ELSE 'P' ",
                    "\n END ",
                  "\n FROM                                ",
                  "\n  cta_decreto a,                     ",
                  "\n  afi_decreto d                      ",
                  "\n WHERE                               ",
                  "\n  a.f_valor between '", v_fecha_inicio, "' AND '", v_fecha_fin, "'", -- corte por ano
                  "\n AND                                 ",
                  "\n  a.id_decreto = d.id_decreto        ", -- derechohabientes que no tienen retiro/traspaso
                  "\n GROUP BY 1,2,3,4,7                    "

--===============Modificado por CABC para identificar solo los movimientos de retiro, abonos y negativos ================================--                  
      -- se prepara y ejecuta
      PREPARE sid_saldos    FROM v_sql
      
      DECLARE cur_saldos    CURSOR FOR sid_saldos
      
      LET v_consulta = "INSERT INTO tmp_tia ",
                       "VALUES (?,?,?,?,?,?)"
      PREPARE prp_almacena_tmp_tia4 FROM v_consulta
            
      -- para cada registro de aportaciones encontrado
      FOREACH cur_saldos INTO --v_ano_nss      ,
                              v_nss          ,
                              v_ano_rfc      ,
                              v_movimiento   ,
                              v_consistencia ,
                              v_conteo       ,
                              v_suma_aivs     ,
                              v_tpo_transaccion
                              
      
         # valida la longitud del NSS a 11 posiciones, de lo contrario rellena con ceros a la izquierda
         LET v_nss_tmp = v_nss
         LET v_nss_tmp = v_nss_tmp.trim()
         IF(v_nss_tmp.getLength() = 11)THEN
            LET v_ano_nss = v_nss[5,6]
         ELSE
            LET v_nss = v_nss_tmp USING "&&&&&&&&&&&"
            LET v_ano_nss = v_nss[5,6]
         END IF    
      
         -- se verifica si se pudo obtener ano del NSS
         IF ( v_ano_nss IS NOT NULL AND f_cadena_es_numerica(v_ano_nss) ) THEN
            # verificamos que el año del rfc sea número
            IF ( v_ano_rfc IS NOT NULL AND f_cadena_es_numerica(v_ano_rfc)) THEN
               # compara si los años de nss y rfc son iguales y asigna el año del nss
               # de lo contrario se calcula la edad con el año del rfc
               IF(v_ano_nss = v_ano_rfc)THEN
                  LET v_registro_archivo.edad = v_ano_nss
               ELSE
                  LET v_registro_archivo.edad = v_ano_rfc
               END IF               
            ELSE
               LET v_registro_archivo.edad = v_ano_nss
            END IF
         ELSE
            -- no se pudo obtener el ano del NSS, se prueba con el del RFC
            IF ( v_ano_rfc IS NOT NULL AND f_cadena_es_numerica(v_ano_rfc) ) THEN
               LET v_registro_archivo.edad          = v_ano_rfc
            ELSE
               -- no se pudo determinar
               LET v_registro_archivo.edad          = "NA"
            END IF
         END IF
      
         -- se inserta el registro en la tabla temporal para consultar por edad
         EXECUTE prp_almacena_tmp_tia4 USING v_registro_archivo.edad,
                                             v_movimiento   ,
                                             v_consistencia ,
                                             v_conteo       ,
                                             v_suma_aivs    ,
                                             v_tpo_transaccion
      
      END FOREACH
      FREE cur_saldos

      EXECUTE IMMEDIATE "CREATE INDEX ix_tmp_tia ON tmp_tia(ano);"
      EXECUTE IMMEDIATE "UPDATE STATISTICS FOR TABLE tmp_tia;"
      
      DECLARE cur_nsssaldos CURSOR FOR
      SELECT DISTINCT ano
      FROM tmp_tia
      ORDER BY ano
      
      LET v_consulta = "SELECT movimiento, consistencia, sum(conteo), sum(aivs),tipo_transaccion",
                       "  FROM tmp_tia",
                       " WHERE ano = ?",
                       " GROUP BY 1,2,5",
                       " ORDER BY 1,2,5"
      PREPARE prp_recupera_tmp_tia4 FROM v_consulta
      DECLARE cur_nsssaldosdisp CURSOR FOR prp_recupera_tmp_tia4 
      
      FOREACH cur_nsssaldos INTO v_ano_nacimiento
      
         -- se inicia el registro de archivo
         LET v_registro_archivo.edad                        = "0"
         LET v_registro_archivo.num_casos_consistente       = 0
         LET v_registro_archivo.saldo_inicial_consistente   = 0
         LET v_registro_archivo.movimientos_consistente     = 0
         LET v_registro_archivo.saldo_total_consistente     = 0
         LET v_registro_archivo.num_casos_inconsistente     = 0
         LET v_registro_archivo.saldo_inicial_inconsistente = 0
         LET v_registro_archivo.movimientos_inconsistente   = 0
         LET v_registro_archivo.saldo_total_inconsistente   = 0
      
         -- se lee la tabla de nss sin retiros
         FOREACH cur_nsssaldosdisp USING v_ano_nacimiento
                                    INTO v_movimiento    ,
                                         v_consistencia  ,
                                         v_conteo        ,
                                         v_suma_aivs     ,
                                         v_tpo_transaccion
         
            -- se calcula la edad
            IF ( v_ano_nacimiento = "NA" ) THEN
               LET v_registro_archivo.edad = v_ano_nacimiento
            ELSE
               LET v_ano_calculo = YEAR(v_fecha_fin) - (v_ano_nacimiento + 1900)
               LET v_registro_archivo.edad = v_ano_calculo
            END IF
{
            IF ( v_consistencia = 1 ) THEN # Consistente
               # no se cuentan los sobregiros como otro registro independiente al del retiro, pero si se cuenta su saldo negativo
               IF(v_movimiento <> 862 AND v_movimiento <> 1052)THEN
                  # para el número de casos se cuenta DISPONIBLE = ABONOS + (- RETIRNOS) + (- NEGATIVOS)
                  # los saldos de los retiros y negativos son negativos
                  # para saldo_inicial_inconsistente, movimientos_inconsistente y saldo_total_consistente no seraliza la misam distincion, ya que como tal 
                  # son saldos y se manejan sus negativos
                  IF(v_movimiento = 999 OR v_movimiento = 1009 OR v_movimiento = 71)THEN
                     LET v_registro_archivo.num_casos_consistente = v_registro_archivo.num_casos_consistente + v_conteo
                  ELSE
                     LET v_registro_archivo.num_casos_consistente = v_registro_archivo.num_casos_consistente - v_conteo                  
                  END IF
               END IF
               # carga y saldo inicial
               IF( v_movimiento = 999 OR v_movimiento = 1009 ) THEN
                  LET v_registro_archivo.saldo_inicial_consistente = v_registro_archivo.saldo_inicial_consistente + v_suma_aivs
               ELSE
                 LET v_registro_archivo.movimientos_consistente = v_registro_archivo.movimientos_consistente + v_suma_aivs
               END IF
            ELSE # inconsistente
               # no se cuentan los sobregiros como otro registro independiente al del retiro, pero si se cuenta su saldo negativo
               IF(v_movimiento <> 862 AND v_movimiento <> 1052)THEN
                  # para el número de casos se cuenta DISPONIBLE = ABONOS + (- RETIRNOS) + (- NEGATIVOS)
                  # los saldos de los retiros y negativos son negativos
                  # para saldo_inicial_inconsistente, movimientos_inconsistente y saldo_total_consistente no seraliza la misam distincion, ya que como tal 
                  # son saldos y se manejan sus negativos
                  IF(v_movimiento = 999 OR v_movimiento = 1009 OR v_movimiento = 71)THEN
                     LET v_registro_archivo.num_casos_inconsistente = v_registro_archivo.num_casos_inconsistente + v_conteo
                  ELSE
                     LET v_registro_archivo.num_casos_inconsistente = v_registro_archivo.num_casos_inconsistente - v_conteo                  
                  END IF
               END IF
               # carga y saldo inicial
               IF( v_movimiento = 999 OR v_movimiento = 1009) THEN
                  LET v_registro_archivo.saldo_inicial_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_suma_aivs
               ELSE                  
                  LET v_registro_archivo.movimientos_inconsistente = v_registro_archivo.movimientos_inconsistente + v_suma_aivs
               END IF
            END IF   
            -- saldo total por edad
            LET v_registro_archivo.saldo_total_consistente   = v_registro_archivo.saldo_inicial_consistente + v_registro_archivo.movimientos_consistente
            LET v_registro_archivo.saldo_total_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_registro_archivo.movimientos_inconsistente
            }

--===============Líneas agregadas por CABC para congruencia entre registros consistentes e inconsistentes del PDF y los txt==============================================-- -            
            -- numero de casos
            -- consistente
            
            IF ( v_consistencia = 1 ) THEN
            
               # no se cuentan los sobregiros
               IF(v_movimiento <> 862 AND v_movimiento <> 1052)THEN                
                  IF((v_movimiento == 999 OR v_movimiento == 1009 OR v_movimiento == 71) AND v_tpo_transaccion=='A')THEN
                  --  DISPLAY "ENTRO1 IF "||v_movimiento||" AIVS "||v_suma_aivs
                  --  DISPLAY "CASOS CONSISTENTES ANTES "||v_registro_archivo.num_casos_consistente
                    LET v_registro_archivo.num_casos_consistente = v_registro_archivo.num_casos_consistente + v_conteo
                  --  DISPLAY "CASOS CONSISTENTES DESPUES "||v_registro_archivo.num_casos_consistente
                  --  DISPLAY "TRANSACCION 1  "||v_tpo_transaccion
                ELSE
                  IF v_tpo_transaccion=='R' OR v_tpo_transaccion=='N' THEN 
                    --DISPLAY "ENTRO1 ELSE "||v_movimiento||" AIVS "||v_suma_aivs
                    --DISPLAY "CASOS CONSISTENTES ANTES "||v_registro_archivo.num_casos_consistente
                     LET v_registro_archivo.num_casos_consistente = v_registro_archivo.num_casos_consistente - v_conteo                  
                    --DISPLAY "CASOS CONSISTENTES DESPUES "||v_registro_archivo.num_casos_consistente
                    --DISPLAY "TRANSACCION 2  "||v_tpo_transaccion
                   END IF 
                     END IF
               END IF
            ELSE
               # no se cuentan los sobregiros
               IF(v_movimiento <> 862 AND v_movimiento <> 1052)THEN
                  IF((v_movimiento == 999 OR v_movimiento == 1009 OR v_movimiento == 71) AND v_tpo_transaccion=='A')THEN
                  --  DISPLAY "ENTRO2 IF "||v_movimiento||" AIVS "||v_suma_aivs
                  --  DISPLAY "CASOS INCONSISTENTES ANTES "||v_registro_archivo.num_casos_inconsistente

                    LET v_registro_archivo.num_casos_inconsistente = v_registro_archivo.num_casos_inconsistente + v_conteo
                   -- DISPLAY "CASOS INCONSISTENTES DESPUES "||v_registro_archivo.num_casos_inconsistente
                   -- DISPLAY "TRANSACCION 3  "||v_tpo_transaccion
                ELSE
                    IF v_tpo_transaccion=='R' OR v_tpo_transaccion=='N' THEN 
                     --   DISPLAY "ENTRO2 ELSE "||v_movimiento||" AIVS "||v_suma_aivs
                    --DISPLAY "CASOS INCONSISTENTES ANTES "||v_registro_archivo.num_casos_inconsistente
                     LET v_registro_archivo.num_casos_inconsistente = v_registro_archivo.num_casos_inconsistente - v_conteo                  
                    --DISPLAY "CASOS INCONSISTENTES DESPUES "||v_registro_archivo.num_casos_inconsistente
                    --DISPLAY "TRANSACCION 4  "||v_tpo_transaccion
                    END IF
                     END IF
                END IF
            END IF

--===============Líneas agregadas por CABC para congruencia entre registros consistentes e inconsistentes del PDF y los txt==============================================--            

--===============Lineas agregadas por CABC para tomar solo los movimientos de retiro, abonos y negativos================================--
        IF v_tpo_transaccion<>'P' THEN
            -- el saldo inicial y carga inicial decreto
            IF ( v_movimiento = 999 OR v_movimiento = 1009) THEN
               -- consistente
               IF ( v_consistencia = 1 ) THEN
                  LET v_registro_archivo.saldo_inicial_consistente = v_registro_archivo.saldo_inicial_consistente + v_suma_aivs
               ELSE
                  LET v_registro_archivo.saldo_inicial_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_suma_aivs
               END IF
            ELSE
               -- suma de movimientos
               -- consistente
               IF ( v_consistencia = 1 ) THEN
                  LET v_registro_archivo.movimientos_consistente = v_registro_archivo.movimientos_consistente + v_suma_aivs
               ELSE
                  LET v_registro_archivo.movimientos_inconsistente = v_registro_archivo.movimientos_inconsistente + v_suma_aivs
               END IF
            END IF

            -- saldo total por edad
            LET v_registro_archivo.saldo_total_consistente   = v_registro_archivo.saldo_inicial_consistente + v_registro_archivo.movimientos_consistente
            LET v_registro_archivo.saldo_total_inconsistente = v_registro_archivo.saldo_inicial_inconsistente + v_registro_archivo.movimientos_inconsistente
        END IF

--===============Lineas agregadas por CABC para tomar solo los movimientos de retiro, abonos y negativos================================--
         END FOREACH

         # se envia a reporte pdf
        -- OUTPUT TO REPORT rpt_analisis_decretos(v_tipo_reporte,v_fecha_inicio,v_fecha_fin,v_registro_archivo.*,p_usuario_cod)
         -- se escribe en archivo
         CALL v_channel.write([v_registro_archivo.*])
      
      END FOREACH
       FREE cur_nsssaldos
      
      -- se cierra el archivo
      CALL v_channel.close()
      
      DISPLAY "FIN: ", TODAY,
              "\nHora  : ", CURRENT HOUR TO MINUTE,
              "\n",
              "\n"
   FINISH REPORT rpt_analisis_decretos
   
   DISPLAY "Consulta finalizada..."

   CALL fn_actualiza_opera_fin(p_pid,
                               p_proceso_cod,
                               p_opera_cod) RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   ELSE
      # Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '',
                             'Reportes decreto',
                             'ID Proceso   : '||p_pid||
                             'Proceso      : '||p_proceso_cod||
                             'Operacion    : '||p_opera_cod||
                             'Fecha Inicio : '||DATE||
                             'Fecha Fin    : '||DATE
                             )
      
   END IF
 
   
END MAIN

{
======================================================================
Clave: 
Nombre: f_cadena_es_numerica
Fecha creacion: Febrero 28, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si una cadena contiene una cifra numerica

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_cadena_es_numerica(v_cadena)
DEFINE v_cadena STRING,
       v_indice SMALLINT,
       v_es_numerica SMALLINT -- booleana que indica si es numerica
       
   -- se asume que es numerica
   LET v_es_numerica = TRUE
       
   -- se verifica si cada caracter de la cadena es numerica
   FOR v_indice = 1 TO v_cadena.getLength()
      -- si el caracter leido no es un numero
      IF ( v_cadena.getCharAt(v_indice) < "0" OR v_cadena.getCharAt(v_indice) > "9" ) THEN
         LET v_es_numerica = FALSE
         EXIT FOR
      END IF
   END FOR
   
   -- se devuelve el resultado del analisis
   RETURN v_es_numerica
END FUNCTION

{===============================================================================
Clave: 
Nombre: rpt_analisis_decreto
Fecha creacion: 01 Noviembre 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
 genera reporte analisis de saldos decretos

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Gerardo Vega   28-jul-2015   Busacar cadea --g- para ver el cambio
                             tenia en duro el valor cero en las variables:
                             - v_total_tpo_rpt.saldo_inicial_consistente
                             - v_total_tpo_rpt.saldo_inicial_inconsistente
                             Además igualaba el total a las variables:
                             - v_total_tpo_rpt.saldo_total_consistente
                             - v_total_tpo_rpt.saldo_total_inconsistente
================================================================================}
REPORT rpt_analisis_decretos(p_tpo_reporte,p_fecha_inicio,p_fecha_fin,p_registro_archivo,p_usuario_cod)
DEFINE p_tpo_reporte         STRING,
       p_fecha_inicio        DATE,
       p_fecha_fin           DATE,
       p_registro_archivo    RECORD -- registro de casos sin retiro
        edad                        VARCHAR(4),
        num_casos_consistente       DECIMAL(9,0),
        saldo_inicial_consistente   DECIMAL(24,6),
        movimientos_consistente     DECIMAL(24,6),
        saldo_total_consistente     DECIMAL(24,6),
        num_casos_inconsistente     DECIMAL(9,0),
        saldo_inicial_inconsistente DECIMAL(24,6),
        movimientos_inconsistente   DECIMAL(24,6),
        saldo_total_inconsistente   DECIMAL(24,6)
       END RECORD,
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       v_total_tpo_rpt RECORD
          num_casos_consistente       DECIMAL(9,0),
          saldo_inicial_consistente   DECIMAL(24,6),
          movimientos_consistente     DECIMAL(24,6),
          saldo_total_consistente     DECIMAL(24,6),
          num_casos_inconsistente     DECIMAL(9,0),
          saldo_inicial_inconsistente DECIMAL(24,6),
          movimientos_inconsistente   DECIMAL(24,6),
          saldo_total_inconsistente   DECIMAL(24,6)
       END RECORD,
       v_total_saldos RECORD # suma de abonos(saldos/cargos iniciales y abonos), retiros y saldos iniciales negativos
          tpo_reporte                 STRING,
          num_casos_consistente       DECIMAL(9,0),
          saldo_inicial_consistente   DECIMAL(24,6),
          movimientos_consistente     DECIMAL(24,6),
          saldo_total_consistente     DECIMAL(24,6),
          num_casos_inconsistente     DECIMAL(9,0),
          saldo_inicial_inconsistente DECIMAL(24,6),
          movimientos_inconsistente   DECIMAL(24,6),
          saldo_total_inconsistente   DECIMAL(24,6)
       END RECORD,
       v_fecha_actual DATE,
       v_pagina       SMALLINT

   FORMAT

      FIRST PAGE HEADER
         LET v_total_tpo_rpt.num_casos_consistente       = 0
         LET v_total_tpo_rpt.saldo_inicial_consistente   = 0
         LET v_total_tpo_rpt.movimientos_consistente     = 0
         LET v_total_tpo_rpt.saldo_total_consistente     = 0
         LET v_total_tpo_rpt.num_casos_inconsistente     = 0
         LET v_total_tpo_rpt.saldo_inicial_inconsistente = 0
         LET v_total_tpo_rpt.movimientos_inconsistente   = 0
         LET v_total_tpo_rpt.saldo_total_inconsistente   = 0

         LET v_total_saldos.tpo_reporte                 = "DISPONIBLE"
         LET v_total_saldos.num_casos_consistente       = 0
         LET v_total_saldos.saldo_inicial_consistente   = 0
         LET v_total_saldos.movimientos_consistente     = 0
         LET v_total_saldos.saldo_total_consistente     = 0
         LET v_total_saldos.num_casos_inconsistente     = 0
         LET v_total_saldos.saldo_inicial_inconsistente = 0
         LET v_total_saldos.movimientos_inconsistente   = 0
         LET v_total_saldos.saldo_total_inconsistente   = 0
         
         LET v_fecha_actual = TODAY
         PRINTX p_fecha_inicio  USING "dd-mm-yyyy",
                p_fecha_fin     USING "dd-mm-yyyy",
                p_usuario_cod,
                v_fecha_actual  USING "dd-mm-yyyy"

      AFTER GROUP OF p_tpo_reporte # Por cada tipo de reporte se obtienen los totales
         CASE p_tpo_reporte
            WHEN "ABONOS"
               LET v_total_tpo_rpt.num_casos_consistente       = GROUP SUM (p_registro_archivo.num_casos_consistente)
               LET v_total_tpo_rpt.saldo_inicial_consistente   = GROUP SUM (p_registro_archivo.saldo_inicial_consistente) 
               LET v_total_tpo_rpt.movimientos_consistente     = GROUP SUM (p_registro_archivo.movimientos_consistente)
               LET v_total_tpo_rpt.saldo_total_consistente     = GROUP SUM (p_registro_archivo.saldo_total_consistente)

               LET v_total_tpo_rpt.num_casos_inconsistente     = GROUP SUM (p_registro_archivo.num_casos_inconsistente)
               LET v_total_tpo_rpt.saldo_inicial_inconsistente = GROUP SUM (p_registro_archivo.saldo_inicial_inconsistente)
               LET v_total_tpo_rpt.movimientos_inconsistente   = GROUP SUM (p_registro_archivo.movimientos_inconsistente)
               LET v_total_tpo_rpt.saldo_total_inconsistente   = GROUP SUM (p_registro_archivo.saldo_total_inconsistente)

               # se acumula los registros para obtener el saldo disponible

            
               
               LET v_total_saldos.num_casos_consistente       = v_total_saldos.num_casos_consistente       + v_total_tpo_rpt.num_casos_consistente 
               LET v_total_saldos.saldo_inicial_consistente   = v_total_saldos.saldo_inicial_consistente   + v_total_tpo_rpt.saldo_inicial_consistente
               LET v_total_saldos.movimientos_consistente     = v_total_saldos.movimientos_consistente     + v_total_tpo_rpt.movimientos_consistente
               LET v_total_saldos.saldo_total_consistente     = v_total_saldos.saldo_total_consistente     + v_total_tpo_rpt.saldo_total_consistente
               
               LET v_total_saldos.num_casos_inconsistente     = v_total_saldos.num_casos_inconsistente     + v_total_tpo_rpt.num_casos_inconsistente
               LET v_total_saldos.saldo_inicial_inconsistente = v_total_saldos.saldo_inicial_inconsistente + v_total_tpo_rpt.saldo_inicial_inconsistente
               LET v_total_saldos.movimientos_inconsistente   = v_total_saldos.movimientos_inconsistente   + v_total_tpo_rpt.movimientos_inconsistente
               LET v_total_saldos.saldo_total_inconsistente   = v_total_saldos.saldo_total_inconsistente   + v_total_tpo_rpt.saldo_total_inconsistente


      
               
            WHEN "RETIROS"
               # para el caso de retiros no se toma en cuenta la columna de saldos iniciales, ya que ese monto se considera en abonos
               # y el salto total se toma como movimientos (retiros en este caso)
               LET v_total_tpo_rpt.num_casos_consistente       = GROUP SUM (p_registro_archivo.num_casos_consistente)
               LET v_total_tpo_rpt.saldo_inicial_consistente   = 0 --GROUP SUM (p_registro_archivo.saldo_inicial_consistente) 
               -- LET v_total_tpo_rpt.saldo_inicial_consistente   = GROUP SUM (p_registro_archivo.saldo_inicial_consistente) 
               LET v_total_tpo_rpt.movimientos_consistente     = GROUP SUM (p_registro_archivo.movimientos_consistente)
               LET v_total_tpo_rpt.saldo_total_consistente     = v_total_tpo_rpt.movimientos_consistente --GROUP SUM (p_registro_archivo.saldo_total_consistente)

               LET v_total_tpo_rpt.num_casos_inconsistente     = GROUP SUM (p_registro_archivo.num_casos_inconsistente)
               LET v_total_tpo_rpt.saldo_inicial_inconsistente = 0 --GROUP SUM (p_registro_archivo.saldo_inicial_inconsistente)
            --    LET v_total_tpo_rpt.saldo_inicial_inconsistente = GROUP SUM (p_registro_archivo.saldo_inicial_inconsistente)
               LET v_total_tpo_rpt.movimientos_inconsistente   = GROUP SUM (p_registro_archivo.movimientos_inconsistente)
               LET v_total_tpo_rpt.saldo_total_inconsistente   = v_total_tpo_rpt.movimientos_inconsistente --GROUP SUM (p_registro_archivo.saldo_total_inconsistente)


             
               
               # en el caso de los retiros se resta el número de casos, para determinar los disponible, no aplica con los montos ya que su origen es negativo
               LET v_total_saldos.num_casos_consistente       = v_total_saldos.num_casos_consistente       - v_total_tpo_rpt.num_casos_consistente 
               LET v_total_saldos.saldo_inicial_consistente   = v_total_saldos.saldo_inicial_consistente   + v_total_tpo_rpt.saldo_inicial_consistente
               LET v_total_saldos.movimientos_consistente     = v_total_saldos.movimientos_consistente     + v_total_tpo_rpt.movimientos_consistente
               LET v_total_saldos.saldo_total_consistente     = v_total_saldos.saldo_total_consistente     + v_total_tpo_rpt.saldo_total_consistente
               
               LET v_total_saldos.num_casos_inconsistente     = v_total_saldos.num_casos_inconsistente     - v_total_tpo_rpt.num_casos_inconsistente
               LET v_total_saldos.saldo_inicial_inconsistente = v_total_saldos.saldo_inicial_inconsistente + v_total_tpo_rpt.saldo_inicial_inconsistente
               LET v_total_saldos.movimientos_inconsistente   = v_total_saldos.movimientos_inconsistente   + v_total_tpo_rpt.movimientos_inconsistente
               LET v_total_saldos.saldo_total_inconsistente   = v_total_saldos.saldo_total_inconsistente   + v_total_tpo_rpt.saldo_total_inconsistente

            
               
            WHEN "NEGATIVOS"
               # movimientos negativos desde origen
               LET v_total_tpo_rpt.num_casos_consistente       = GROUP SUM (p_registro_archivo.num_casos_consistente)
               LET v_total_tpo_rpt.saldo_inicial_consistente   = GROUP SUM (p_registro_archivo.saldo_inicial_consistente) 
               LET v_total_tpo_rpt.movimientos_consistente     = GROUP SUM (p_registro_archivo.movimientos_consistente)
               LET v_total_tpo_rpt.saldo_total_consistente     = GROUP SUM (p_registro_archivo.saldo_total_consistente)

               LET v_total_tpo_rpt.num_casos_inconsistente     = GROUP SUM (p_registro_archivo.num_casos_inconsistente)
               LET v_total_tpo_rpt.saldo_inicial_inconsistente = GROUP SUM (p_registro_archivo.saldo_inicial_inconsistente)
               LET v_total_tpo_rpt.movimientos_inconsistente   = GROUP SUM (p_registro_archivo.movimientos_inconsistente)
               LET v_total_tpo_rpt.saldo_total_inconsistente   = GROUP SUM (p_registro_archivo.saldo_total_inconsistente)


             

               # en el caso de los negativos se resta el número de casos, para determinar los disponible, no aplica con los montos ya que su origen es negativo
               LET v_total_saldos.num_casos_consistente       = v_total_saldos.num_casos_consistente       - v_total_tpo_rpt.num_casos_consistente 
               LET v_total_saldos.saldo_inicial_consistente   = v_total_saldos.saldo_inicial_consistente   + v_total_tpo_rpt.saldo_inicial_consistente
               LET v_total_saldos.movimientos_consistente     = v_total_saldos.movimientos_consistente     + v_total_tpo_rpt.movimientos_consistente
               LET v_total_saldos.saldo_total_consistente     = v_total_saldos.saldo_total_consistente     + v_total_tpo_rpt.saldo_total_consistente
               
               LET v_total_saldos.num_casos_inconsistente     = v_total_saldos.num_casos_inconsistente     - v_total_tpo_rpt.num_casos_inconsistente
               LET v_total_saldos.saldo_inicial_inconsistente = v_total_saldos.saldo_inicial_inconsistente + v_total_tpo_rpt.saldo_inicial_inconsistente
               LET v_total_saldos.movimientos_inconsistente   = v_total_saldos.movimientos_inconsistente   + v_total_tpo_rpt.movimientos_inconsistente
               LET v_total_saldos.saldo_total_inconsistente   = v_total_saldos.saldo_total_inconsistente   + v_total_tpo_rpt.saldo_total_inconsistente

           
               
         END CASE 
            
            
         {IF ( p_tpo_reporte <> "RETIROS" ) THEN
            LET v_total_tpo_rpt.num_casos_consistente       = GROUP SUM (p_registro_archivo.num_casos_consistente)
            LET v_total_tpo_rpt.saldo_inicial_consistente   = GROUP SUM (p_registro_archivo.saldo_inicial_consistente) 
            LET v_total_tpo_rpt.movimientos_consistente     = GROUP SUM (p_registro_archivo.movimientos_consistente)
            LET v_total_tpo_rpt.saldo_total_consistente     = GROUP SUM (p_registro_archivo.saldo_total_consistente)

            LET v_total_tpo_rpt.num_casos_inconsistente     = GROUP SUM (p_registro_archivo.num_casos_inconsistente)
            LET v_total_tpo_rpt.saldo_inicial_inconsistente = GROUP SUM (p_registro_archivo.saldo_inicial_inconsistente)
            LET v_total_tpo_rpt.movimientos_inconsistente   = GROUP SUM (p_registro_archivo.movimientos_inconsistente)
            LET v_total_tpo_rpt.saldo_total_inconsistente   = GROUP SUM (p_registro_archivo.saldo_total_inconsistente)
         ELSE
            # para el caso de retiros no se toma en cuenta la columna de saldos iniciales, ya que ese monto se considera en abonos
            # y el salto total se toma como movimientos (retiros en este caso)
            LET v_total_tpo_rpt.num_casos_consistente       = GROUP SUM (p_registro_archivo.num_casos_consistente)
            LET v_total_tpo_rpt.saldo_inicial_consistente   = 0 --GROUP SUM (p_registro_archivo.saldo_inicial_consistente) 
            LET v_total_tpo_rpt.movimientos_consistente     = GROUP SUM (p_registro_archivo.movimientos_consistente)
            LET v_total_tpo_rpt.saldo_total_consistente     = v_total_tpo_rpt.movimientos_consistente --GROUP SUM (p_registro_archivo.saldo_total_consistente)

            LET v_total_tpo_rpt.num_casos_inconsistente     = GROUP SUM (p_registro_archivo.num_casos_inconsistente)
            LET v_total_tpo_rpt.saldo_inicial_inconsistente = 0 --GROUP SUM (p_registro_archivo.saldo_inicial_inconsistente)
            LET v_total_tpo_rpt.movimientos_inconsistente   = GROUP SUM (p_registro_archivo.movimientos_inconsistente)
            LET v_total_tpo_rpt.saldo_total_inconsistente   = v_total_tpo_rpt.movimientos_inconsistente --GROUP SUM (p_registro_archivo.saldo_total_inconsistente)

         END IF}
		 
         {IF( p_tpo_reporte <> "DISPONIBLE" )THEN
            LET v_total_saldos.num_casos_consistente       = v_total_tpo_rpt.num_casos_consistente 
            LET v_total_saldos.saldo_inicial_consistente   = v_total_tpo_rpt.saldo_inicial_consistente
            LET v_total_saldos.movimientos_consistente     = v_total_tpo_rpt.movimientos_consistente
            LET v_total_saldos.saldo_total_consistente     = v_total_tpo_rpt.saldo_total_consistente
            LET v_total_saldos.num_casos_inconsistente     = v_total_tpo_rpt.num_casos_inconsistente
            LET v_total_saldos.saldo_inicial_inconsistente = v_total_tpo_rpt.saldo_inicial_inconsistente
            LET v_total_saldos.movimientos_inconsistente   = v_total_tpo_rpt.movimientos_inconsistente
            LET v_total_saldos.saldo_total_inconsistente   = v_total_tpo_rpt.saldo_total_inconsistente
         END IF}

         PRINTX p_tpo_reporte,
                v_total_tpo_rpt.*
         

      ON EVERY ROW
                  

      ON LAST ROW
         LET v_pagina = PAGENO
         PRINTX v_pagina,v_total_saldos.*
         
         

END REPORT
