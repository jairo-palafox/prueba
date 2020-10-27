--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>OCG                                           #
#Programa          =>OCGS08                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo part 50006 de 43BIS formalizados   #
#                    y liquidados.                                 #
#Autor             =>José Eduardo Ventura                          #
#Modificado        =>Emilio Abarca                                 # 
#Fecha inicio      =>16 febrero 2016                               #
#Fecha modifica    =>17 Enero 2017                                 #
####################################################################

DATABASE safre_viv

GLOBALS  "OCGW07.inc"

GLOBALS

   DEFINE p_usuario           LIKE seg_usuario.usuario            -- nombre del usuario
   DEFINE p_pid               LIKE bat_ctr_proceso.pid            -- pid
   DEFINE p_proceso_cod       LIKE cat_proceso.proceso_cod        -- codigo del proceso
   DEFINE p_opera_cod         LIKE cat_operacion.opera_cod        -- codigo de la operacion de la etapa
   DEFINE p_f_inicial         CHAR(10)
   DEFINE p_f_final           CHAR(10)
   DEFINE v_aux_f_ini         CHAR(10)
   DEFINE v_aux_f_fin         CHAR(10)
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nom_arh           STRING
   DEFINE v_folio             CHAR(10)
   DEFINE ch                  base.Channel
   DEFINE v_detalle           STRING
   DEFINE a                   SMALLINT
   DEFINE v_cta_t             INTEGER
   DEFINE v_cta_f             INTEGER
   DEFINE bnd_datos1          SMALLINT
   DEFINE bnd_datos2          SMALLINT
   DEFINE v_dia               CHAR(8)
   DEFINE v_nom_arh_resp      STRING
   DEFINE v_s_comando         STRING
   DEFINE v_f_proceso         DATE
   DEFINE f_otorga_ws         char(8)
   DEFINE v_bnd_ws            SMALLINT
   DEFINE v_bnd_escribe       SMALLINT
   DEFINE v_edo_ws            INTEGER
--se agrega para WS
   DEFINE v_ws_status         SMALLINT
 

   DEFINE arr_part_50006 DYNAMIC ARRAY OF RECORD
      id_ocg_formalizacion DECIMAL(9,0),
      id_ocg_tramite     DECIMAL(9,0),
      subproceso         CHAR(1),
      cve_entidad_ef     SMALLINT,
      nss                CHAR(11),
      paterno            CHAR(13),
      materno            CHAR(13),
      nombre             CHAR(14),
      nom_patron         CHAR(30),
      viv97              DECIMAL(10,2),
      salario_diario     DECIMAL(6,2), -- deben ser ocho posiciones
      tpo_moneda         CHAR(1),
      monto_credito      DECIMAL(12,2),
      valor_conversion   CHAR(10),
      monto_cred_pesos   DECIMAL(12,2),
      valor_avaluo       DECIMAL(12,2),
      f_credito_liq      DATE,
      f_otorgamiento     DATE,
      tasa_base          CHAR(20),
      margen             CHAR(20),
      plazo_meses        CHAR(3),
      entidad_inmueble   CHAR(2),
      mun_inmueble       CHAR(3),
      tpo_credito        CHAR(1),
      genero             CHAR(1),
      f_liquidacion      DATE
   END RECORD
   
   DEFINE v_tasa_ini     CHAR(20)
   DEFINE v_tasa_fovi    CHAR(13)
   DEFINE v_tasa_base    CHAR(6)
   DEFINE v_fz_margen    CHAR(6)
   DEFINE v_estado       SMALLINT 

END GLOBALS

MAIN

   -- se recuperan los parametros
   LET p_usuario         = ARG_VAL(1)
   LET p_pid             = ARG_VAL(2)
   LET p_proceso_cod     = ARG_VAL(3)
   LET p_opera_cod       = ARG_VAL(4)
   LET p_f_inicial       = ARG_VAL(5)
   LET p_f_final         = ARG_VAL(6)

   -- Si la f_inicial y f_inicial no se reciben del lanzador se interpreta
   -- que se ejecutó en autmático y se recupera en rango de fechas.
   
   IF (p_f_inicial IS NULL) AND (p_f_final IS NULL) THEN

      SELECT FIRST 1 f_proceso
          INTO v_f_proceso
          FROM ocg_ctr_arh_dwh
          ORDER BY f_proceso DESC

      LET p_f_inicial = v_f_proceso 
      LET p_f_final   = TODAY -1  
      
   ELSE 
      LET p_f_inicial =  p_f_inicial[4,5],"/",p_f_inicial[1,2],"/",p_f_inicial[7,10]
      LET p_f_final   =  p_f_final[4,5],"/",p_f_final[1,2],"/",p_f_final[7,10]

   END IF 

   -- Variables para mostrar fechas al usuario formato "dd/mm/yyyy"
   LET v_aux_f_ini = p_f_inicial[4,5],"/",p_f_inicial[1,2],"/",p_f_inicial[7,10]
   LET v_aux_f_fin = p_f_final[4,5],"/",p_f_final[1,2],"/",p_f_final[7,10]
   
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   LET v_dia = TODAY USING "ddmmyyyy"

   LET v_nom_arh = v_ruta_envio CLIPPED ,"/ARP50006",".txt"
   LET v_nom_arh_resp = v_ruta_envio CLIPPED ,"/ARP50006_",v_dia,".tdwh"

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGS08.log")

   CALL fn_display_proceso(0,"INICIA GENERACIÓN ARCHIVO 50006")
   DISPLAY " USUARIO      : ",p_usuario
   DISPLAY " PID          : ",p_pid
   DISPLAY " Fecha inicial: ",v_aux_f_ini
   DISPLAY " Fecha final  : ",v_aux_f_fin
   DISPLAY ""

   -- se ejecuta la función que genera el archivo de salida de liquidación
   CALL fn_archivo_part_50007() RETURNING v_estado

   IF (v_estado = 1) THEN 
   
      DISPLAY " Archivo generado de forma correcta en : "
      DISPLAY " ",'"',v_nom_arh,'"'
      DISPLAY ""
      DISPLAY " => PROCESO EJECUTADO CORRECTAMENTE"
      DISPLAY ""
      CALL fn_display_proceso(1,"FIN GENERACIÓN ARCHIVO DE 50006")

      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod)
                        RETURNING v_estado
   ELSE 
      DISPLAY "Ha ocurrido un error al generar el archivo 50006"
      DISPLAY ""
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF 
         
END MAIN

FUNCTION fn_archivo_part_50007()

   DEFINE v_qry_fz          STRING
   DEFINE v_qry_lq          STRING 
   DEFINE v_f_liquida_cofi  DATE
   DEFINE v_comando_transf  STRING
   DEFINE v_indicador       BOOLEAN 
   DEFINE v_env_comando    STRING
   DEFINE v_ind_envio      BOOLEAN

   LET v_indicador = 0 -- Indicador de que aún no se ha generado el archivo
   LET ch = base.Channel.create()
   CALL ch.openFile(v_nom_arh,"w" )
   CALL ch.setDelimiter(NULL)

  -- CALL arr_part_liquidados.clear()
   CALL arr_part_50006.clear()
  
   # 1 sentencia-obtiene registros formalizados
   # 2 sentencia-obtiene registros liquidados

   LET v_f_liquida_cofi = NULL 
   
   LET v_qry_fz = "SELECT f.id_ocg_formalizacion,
                          f.id_ocg_tramite,
                          '0',
                          f.cve_ent_financiera,
                          d.nss,
                          f.ap_paterno,
                          f.ap_materno,
                          f.nombre,
                          '',
                          f.viv97,
                          '',
                          f.tpo_moneda,
                          f.monto_credito,
                          '',
                          f.monto_credito,
                          f.valor_avaluo,
                          a.f_formalizacion,
                          f.f_otorga_ent_fin,
                          f.tasa_base,
                          f.margen,
                          f.plazo_credito,
                          f.ent_fed_inmueble,
                          f.mcpio_inmueble,
                          f.tpo_credito,
                          f.genero,
                          ''
                     FROM ocg_formalizacion f,
                          ocg_detalle d,
                          ocg_acreditado a
                    WHERE f.id_ocg_detalle = d.id_ocg_detalle
                      AND f.id_ocg_formalizacion = a.id_ocg_formalizacion
                      AND f.situacion IN (55,60,70,80)
                      AND f.tpo_credito IN('A','C')
                      AND d.f_proceso between ","'",p_f_inicial,"'"," AND ","'",p_f_final,"'", "
                    ORDER BY a.f_formalizacion ASC;"
              
   LET v_qry_lq = "SELECT l.id_ocg_formalizacion,
                          l.id_ocg_tramite,
                          '5',
                          f.cve_ent_financiera,
                          d.nss,
                          f.ap_paterno,
                          f.ap_materno,
                          f.nombre,
                          '',
                          '',
                          '',
                          '',
                          '',
                          '',
                          '',
                          '',
                          a.f_formalizacion,
                          f.f_otorga_ent_fin,
                          '',
                          '',
                          '',
                          '',
                          '',
                          '',
                          '',
                          a.f_liquida_credito
                     FROM ocg_formalizacion f,
                          ocg_detalle d,
                          ocg_acreditado a,
                          ocg_liquidacion l
                    WHERE l.id_ocg_formalizacion = f.id_ocg_formalizacion
                      AND l.id_ocg_formalizacion = a.id_ocg_formalizacion
                      AND l.situacion   IN (140,150,160)
                      AND f.tpo_credito IN ('A','C')
                      AND l.id_ocg_detalle = d.id_ocg_detalle
                      AND d.f_proceso between ","'",p_f_inicial,"'"," AND ","'",p_f_final,"'", "
                      ORDER BY a.f_liquida_credito;"
              
     --DISPLAY "Query global: ", v_qry

--******************************************************************************
LET bnd_datos1 = 0
LET bnd_datos2 = 0
--******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_t
     FROM ocg_formalizacion f,ocg_detalle d,ocg_fecha_mig mig
    WHERE f.estado IN (20,30)
      AND f.diagnostico = 1
      AND f.situacion IN (55,60,70,80,140,150,160)
      AND f.id_ocg_detalle = d.id_ocg_detalle
      AND f.id_ocg_formalizacion = mig.id_ocg_referencia
      AND mig.subproceso = 2
      AND d.f_proceso BETWEEN p_f_inicial AND p_f_final

   CALL fn_crea_tmp_ws()

   IF v_cta_t > 0 THEN

      PREPARE prp_part_50006_fz FROM v_qry_fz
      DECLARE cur_part_50006_fz CURSOR FOR prp_part_50006_fz

      LET a = 1

      CALL arr_part_50006.clear()
      
      # Registros formalizaos
      FOREACH cur_part_50006_fz INTO arr_part_50006[a].*

         IF (arr_part_50006[a].f_credito_liq IS NULL) AND (arr_part_50006[a].id_ocg_tramite IS NOT NULL) THEN

            -- Se recupera del subproceso 001
            SELECT f_liquida_cofi
               INTO v_f_liquida_cofi
               FROM ocg_fecha_mig
              WHERE id_ocg_referencia = arr_part_50006[a].id_ocg_tramite
                AND subproceso = 1

           LET arr_part_50006[a].f_credito_liq = v_f_liquida_cofi
          
         END IF 
      
         LET v_tasa_ini = arr_part_50006[a].tasa_base

         LET v_tasa_fovi = v_tasa_ini[1,13]

         IF(v_tasa_fovi = " T. BASE FOVI") THEN
            LET arr_part_50006[a].tasa_base = v_tasa_ini[14,20]
         END IF 
         
         --Valida que la tasa base no contenga caracteres raros 
         LET v_tasa_base = fn_numerico(arr_part_50006[a].tasa_base)
         
         -- Se valida que el margen no contenga caracteres raros
         LET v_fz_margen = fn_numerico(arr_part_50006[a].margen)


--*******************************************************************************
-- 21062018 se agrega consulta a WS para saber el salario diario integrado

LET f_otorga_ws = arr_part_50006[a].f_otorgamiento USING "yyyymmdd"
CALL fn_sdi(arr_part_50006[a].nss,f_otorga_ws) RETURNING v_ws_status

--*******************************************************************************
         
         --CALL fn_escribe()
         INSERT INTO safre_tmp:tmp_dwh_liq VALUES (
         arr_part_50006[a].id_ocg_formalizacion,
         arr_part_50006[a].id_ocg_tramite,      
         arr_part_50006[a].subproceso,          
         arr_part_50006[a].cve_entidad_ef,      
         arr_part_50006[a].nss,                 
         arr_part_50006[a].paterno,             
         arr_part_50006[a].materno,             
         arr_part_50006[a].nombre,              
         arr_part_50006[a].nom_patron,          
         arr_part_50006[a].viv97,               
         arr_part_50006[a].salario_diario,      
         arr_part_50006[a].tpo_moneda,          
         arr_part_50006[a].monto_credito,       
         arr_part_50006[a].valor_conversion,    
         arr_part_50006[a].monto_cred_pesos,    
         arr_part_50006[a].valor_avaluo,        
         arr_part_50006[a].f_credito_liq,       
         arr_part_50006[a].f_otorgamiento,      
         v_tasa_base,                           
         v_fz_margen,                           
         arr_part_50006[a].plazo_meses,         
         arr_part_50006[a].entidad_inmueble,    
         arr_part_50006[a].mun_inmueble,        
         arr_part_50006[a].tpo_credito,         
         arr_part_50006[a].genero,              
         arr_part_50006[a].f_liquidacion,
         v_bnd_ws) 

         LET a = a + 1
      END FOREACH

      PREPARE prp_part_50006_lq FROM v_qry_lq
      DECLARE cur_part_50006_lq CURSOR FOR prp_part_50006_lq

      # Registros Liquidados
      FOREACH cur_part_50006_lq INTO arr_part_50006[a].*

         IF (arr_part_50006[a].f_credito_liq IS NULL) THEN

             -- Se recupera del subproceso 002
            SELECT f_liquida_cofi
               INTO v_f_liquida_cofi
               FROM ocg_fecha_mig
              WHERE id_ocg_referencia = arr_part_50006[a].id_ocg_formalizacion
                AND subproceso = 2
                
            LET arr_part_50006[a].f_credito_liq = v_f_liquida_cofi

           IF (v_f_liquida_cofi IS NULL) THEN

               -- Se recupera del subproceso 001
               SELECT f_liquida_cofi
                 INTO v_f_liquida_cofi
                 FROM ocg_fecha_mig
                WHERE id_ocg_referencia = arr_part_50006[a].id_ocg_tramite
                  AND subproceso = 1 

              LET arr_part_50006[a].f_credito_liq = v_f_liquida_cofi
              
           END IF 
         
         END IF
{
--*******************************************************************************
-- 21062018 se agrega consulta a WS para saber el salario diario integrado

LET f_otorga_ws = arr_part_50006[a].f_otorgamiento USING "yyyymmdd"
CALL fn_sdi(arr_part_50006[a].nss,f_otorga_ws) RETURNING v_ws_status

--******************************************************************************* 
}
         --CALL fn_escribe()

INSERT INTO safre_tmp:tmp_dwh_liq VALUES (
arr_part_50006[a].id_ocg_formalizacion,
arr_part_50006[a].id_ocg_tramite,
arr_part_50006[a].subproceso,
arr_part_50006[a].cve_entidad_ef,
arr_part_50006[a].nss,
arr_part_50006[a].paterno,
arr_part_50006[a].materno,
arr_part_50006[a].nombre,
arr_part_50006[a].nom_patron,
arr_part_50006[a].viv97,
arr_part_50006[a].salario_diario,
arr_part_50006[a].tpo_moneda,
arr_part_50006[a].monto_credito,
arr_part_50006[a].valor_conversion,
arr_part_50006[a].monto_cred_pesos,
arr_part_50006[a].valor_avaluo,
arr_part_50006[a].f_credito_liq,
arr_part_50006[a].f_otorgamiento,
v_tasa_base,
v_fz_margen,
arr_part_50006[a].plazo_meses,
arr_part_50006[a].entidad_inmueble,
arr_part_50006[a].mun_inmueble,
arr_part_50006[a].tpo_credito,
arr_part_50006[a].genero,
arr_part_50006[a].f_liquidacion,
v_bnd_ws)

         LET a = a + 1
      END FOREACH 

IF arr_part_50006[a].nss IS NULL THEN
   CALL arr_part_50006.deleteElement(arr_part_50006.getLength())
   LET a = a - 1
END IF

      LET v_edo_ws = ""

      SELECT COUNT(*)
        INTO v_edo_ws
        FROM safre_tmp:tmp_dwh_liq
       WHERE estado_ws = 0

      IF v_edo_ws = 0 THEN
         FOR a = 1 TO arr_part_50006.getLength()
         CALL fn_escribe()
      END FOR
   -- Genera sumario al archivo
      CALL fn_crea_sumario()
      LET bnd_datos1 = 0
   ELSE
      LET bnd_datos1 = 2
   END IF

      -- Genera sumario al archivo
      --CALL fn_crea_sumario()
      
   ELSE
      LET bnd_datos1 = 1
   END IF

  IF (bnd_datos1 >= 1) THEN
    CALL fn_sin_datos()
  END IF

  CALL ch.close()

    -- Guarda el registro de la ejecución en la tabla de control
    INSERT INTO ocg_ctr_arh_dwh
          VALUES(p_f_inicial,
                 p_f_final,
                 TODAY,
                 a,
                 p_usuario);

    -- se crea comando para copia de archivo
    LET v_s_comando = "cp ",v_nom_arh," ",v_nom_arh_resp
    RUN v_s_comando

    -- Pasa de formato UNIX a DOS
    LET v_s_comando = "sed 's/$/\r/' ",v_nom_arh_resp," > ",v_nom_arh
    RUN v_s_comando

    LET v_indicador =  1
    
    -- Ejecución del Script de envío a TRM
    
    LET v_env_comando = "sh ","/opt/Interpel/Scripts/SOA16032.sh" #Envío de Producción
   
    RUN v_env_comando RETURNING v_ind_envio

    IF(v_ind_envio = 0) THEN
       DISPLAY " Archivo part 50006 de 43BIS formalizados y liquidados"
       DISPLAY ""
    ELSE 
       DISPLAY " No se pudo enviar el archivo 43BIS formalizados y liquidados a TRM"
       DISPLAY ""
    END IF
    
    RETURN v_indicador
    
END FUNCTION 

FUNCTION fn_escribe()

   DEFINE v_detalle           STRING
   DEFINE v_nombre_completo   CHAR(40)
   DEFINE v_salario           DECIMAL(10,2)
   DEFINE v_margen            DECIMAL(8,2)
   DEFINE v_curp              CHAR(18)
   DEFINE v_aux_genero        CHAR(1)
   DEFINE v_sexo              CHAR(1)

   LET v_nombre_completo = arr_part_50006[a].paterno CLIPPED," ",
                           arr_part_50006[a].materno CLIPPED," ",
                           arr_part_50006[a].nombre

   # Si los registros pertenecen al subproceso 002 (formalizados)
  IF (arr_part_50006[a].subproceso = 0) THEN 
  
   IF arr_part_50006[a].tpo_moneda = 1 THEN
      LET arr_part_50006[a].monto_cred_pesos = arr_part_50006[a].monto_credito
      LET arr_part_50006[a].valor_conversion = 1
   END IF

   IF arr_part_50006[a].tpo_moneda = 2 THEN
      LET v_salario =(73.04)*(30.4)
      LET arr_part_50006[a].monto_cred_pesos = v_salario * arr_part_50006[a].monto_credito
      LET arr_part_50006[a].valor_conversion = v_salario
   END IF

   IF arr_part_50006[a].tpo_moneda = 3 THEN
      LET v_salario = v_salario * (5.556329)
      LET arr_part_50006[a].monto_cred_pesos = v_salario * arr_part_50006[a].monto_credito
      LET arr_part_50006[a].valor_conversion = 5.556329
   END IF

   # Regla para calcular la tasa base
   IF (v_tasa_base IS NULL) OR (v_tasa_base = "      ") THEN
      LET v_tasa_base = "  0.00"
   ELSE 

      IF (v_tasa_base >= 99.99) AND (v_tasa_base <= 9999.99) THEN 
         LET v_tasa_base = v_tasa_base / 100
      END IF 
  
      IF (v_tasa_base > 9999.99) AND (v_tasa_base < 99999.99) THEN 
         LET v_tasa_base = v_tasa_base / 1000
      END IF 
            
      IF (v_tasa_base > 99999.99) AND (v_tasa_base < 999999.99) THEN 
         LET v_tasa_base = v_tasa_base / 10000
      END IF

      IF(v_tasa_base > 999999.99) AND (v_tasa_base < 9999999.99) THEN
         LET v_tasa_base = v_tasa_base / 100000 
      END IF 
      
   END IF 

   # Regla para calcular el margen
   IF (v_fz_margen = "      ") OR (v_fz_margen IS NULL) THEN
      LET v_fz_margen = "  0.00"
   ELSE
      LET v_margen = v_fz_margen

      IF (v_margen > 99.99) AND (v_margen < 9999.99) THEN
         LET v_margen = v_margen / 100
         LET v_fz_margen = v_margen
      END IF

      IF (v_margen > 9999.99) AND (v_margen < 99999.99) THEN
         LET v_margen = v_margen / 1000
         LET v_fz_margen = v_margen
      END IF 

      IF (v_margen > 99999.99) AND (v_margen < 999999.99) THEN
         LET v_margen = v_margen / 10000
         LET v_fz_margen = v_margen
      END IF
      
   END IF

   IF (arr_part_50006[a].tpo_credito = "A") THEN
      LET arr_part_50006[a].tpo_credito = " "
   END IF 
   
   IF (arr_part_50006[a].genero IS NULL) OR (arr_part_50006[a].genero = " ") THEN
      -- Recupera la curp del dh para obtener el género
      SELECT curp
        INTO v_curp
        FROM afi_derechohabiente
       WHERE nss = arr_part_50006[a].nss

      IF(v_curp IS NOT NULL) THEN 
       
         LET v_aux_genero = v_curp[11]
       
         -- En caso de que sea Femenino
         IF (v_aux_genero = "M") THEN
            LET v_aux_genero = "F"
         ELSE 
            -- En caso de que sea Mansculino
            IF(v_aux_genero = "H") THEN 
               LET v_aux_genero = "M"
            END IF 
         END IF
      END IF 

      IF (v_curp IS NULL) OR 
         (v_curp = "                  ") THEN
         
         -- Se recupera el sexo
         SELECT sexo
            INTO v_sexo
            FROM afi_derechohabiente
           WHERE nss = arr_part_50006[a].nss
    
         LET v_aux_genero = v_sexo

         IF (v_aux_genero = "1") THEN
            LET v_aux_genero = "M"
         ELSE 
            IF (v_aux_genero = "2") THEN
               LET v_aux_genero = "F"
            END IF 
         END IF
      END IF

      -- Se asigna valor del género al arreglo
      LET arr_part_50006[a].genero = v_aux_genero
   
   END IF 

      LET v_detalle = 
         arr_part_50006[a].subproceso     USING "&" ,
         arr_part_50006[a].cve_entidad_ef USING "&&&" ,
         arr_part_50006[a].nss               ,
         v_nombre_completo,
         arr_part_50006[a].nom_patron        ,
         arr_part_50006[a].viv97             USING "#,###,##&.&&",
         arr_part_50006[a].salario_diario    USING "#,##&.&&",
         arr_part_50006[a].tpo_moneda        ,
         arr_part_50006[a].monto_credito     USING "###,###,##&.&&",
         arr_part_50006[a].valor_conversion  USING "######&.&&",
         arr_part_50006[a].monto_cred_pesos  USING "###,###,##&.&&",
         arr_part_50006[a].valor_avaluo      USING "###,###,##&.&&",
         arr_part_50006[a].f_credito_liq     USING "yyyy/mm/dd",
         arr_part_50006[a].f_otorgamiento    USING "yyyy/mm/dd",
         v_tasa_base                         USING "#&.&&&",
         v_fz_margen                         USING "#&.&&&",
         v_tasa_base                         USING "#&.&&&",
         arr_part_50006[a].plazo_meses       ,
         arr_part_50006[a].entidad_inmueble  USING "&&",
         arr_part_50006[a].mun_inmueble      USING "&&&",
         arr_part_50006[a].tpo_credito       ,
         arr_part_50006[a].genero            ,
         arr_part_50006[a].f_liquidacion     USING "yyyy/mm/dd"
  ELSE 
     # Si los registros pertenecen al Subproceso 5 (Liquidados)
     IF (arr_part_50006[a].subproceso = 5) THEN

        LET v_tasa_base = "      "
        LET v_fz_margen = "      "
        
        LET v_detalle = 
           arr_part_50006[a].subproceso        USING "&" ,
           arr_part_50006[a].cve_entidad_ef    USING "&&&" ,
           arr_part_50006[a].nss               ,
           v_nombre_completo,
           arr_part_50006[a].nom_patron        ,
           arr_part_50006[a].viv97             ,
           arr_part_50006[a].salario_diario    ,
           arr_part_50006[a].tpo_moneda        ,
           arr_part_50006[a].monto_credito     ,
           arr_part_50006[a].valor_conversion  ,
           arr_part_50006[a].monto_cred_pesos  ,
           arr_part_50006[a].valor_avaluo      ,
           arr_part_50006[a].f_credito_liq     USING "yyyy/mm/dd",
           arr_part_50006[a].f_otorgamiento    USING "yyyy/mm/dd",
           v_tasa_base                         ,
           v_fz_margen                         ,
           v_tasa_base                         ,
           arr_part_50006[a].plazo_meses       ,
           arr_part_50006[a].entidad_inmueble  ,
           arr_part_50006[a].mun_inmueble      ,
           arr_part_50006[a].tpo_credito       ,
           arr_part_50006[a].genero            ,
           arr_part_50006[a].f_liquidacion     USING "yyyy/mm/dd"
     END IF 
  END IF 
    -- Se escribe en el archivo de detalle
    CALL ch.writeLine([v_detalle])
    
END FUNCTION

FUNCTION fn_crea_sumario()

   --Vars para sumario
   DEFINE v_sumario           CHAR(216)
   DEFINE v_filler            CHAR(216)
   DEFINE v_pos_ini           INTEGER 
   DEFINE k                   INTEGER 
   DEFINE v_tot_reg           STRING 
      
   LET v_tot_reg = a  -- total de registros
   
   LET v_tot_reg = v_tot_reg.trim()
   
   LET v_pos_ini = v_tot_reg.getLength() + 16   -- Obtiene la ultima posición de la cadena "APOYO INFONAVIT" más el total de registros
                                                -- para empezar a contar desde ahí 
   FOR k = v_pos_ini TO 216       
         LET v_filler[k] = " "                         
   END FOR 
  
   LET v_sumario = "APOYO INFONAVIT ",v_tot_reg,v_filler

   CALL  ch.writeLine([v_sumario])

END FUNCTION 

FUNCTION fn_sin_datos()

   LET v_detalle = "NO SE ENCONTRARON REGISTROS CON LOS PARÁMETROS DE FECHA INGRESADOS"

   CALL ch.writeLine([v_detalle])
END FUNCTION

FUNCTION fn_numerico(p_cadena)

   DEFINE p_cadena      STRING
   DEFINE v_idx         INTEGER
   DEFINE v_idx2        INTEGER  
   DEFINE aux           STRING
   DEFINE pos_final     INTEGER 
   DEFINE v_tasa        STRING 
   DEFINE r_tasa        STRING   
 
   -- Eliminamos espacios izquierdos en blanco
   LET aux = p_cadena.trimLeft()

   LET pos_final = aux.getLength()
      
      FOR v_idx = 1 TO pos_final
      
        IF(aux.subString(v_idx,v_idx) MATCHES '[1-9]') THEN  
           LET v_tasa = aux.subString(v_idx,pos_final)
           EXIT FOR 
        ELSE 
           IF(aux.subString(v_idx,v_idx) MATCHES '[.]') THEN
              LET v_tasa = aux.subString(v_idx -1,pos_final)
              EXIT FOR 
           END IF 
        END IF 
        
      END FOR

   -- Se valida que la cadena no contenga caracteres raros
   LET v_tasa = v_tasa CLIPPED 
   LET r_tasa = NULL 
   
   FOR v_idx2 = 1 TO v_tasa.getLength()
      
      IF(v_tasa.subString(v_idx2,v_idx2) MATCHES '[0-9]') OR  
        (v_tasa.subString(v_idx2,v_idx2) MATCHES '[.]') THEN

         LET r_tasa = r_tasa CLIPPED , v_tasa.subString(v_idx2,v_idx2) 
         
      END IF 

   END FOR 

   RETURN r_tasa
    
END FUNCTION

FUNCTION fn_sdi(v_sdi_nss,v_sdi_f_otorga)

   DEFINE v_sdi_nss            CHAR(11)
   DEFINE v_sdi_ws_status      SMALLINT -- estatus de ejecución del ws
   DEFINE v_sdi_indice         INTEGER
   DEFINE v_sdi_f_ini          char(10)
   DEFINE v_sdi_nrp            VARCHAR(100)
   DEFINE v_sdi_f_fin          char(10)
   DEFINE v_sdi_idx            SMALLINT
   DEFINE v_sdi_nombre         VARCHAR(100)
   DEFINE v_sdi_salario        DECIMAL(12,2)
   DEFINE v_sdi_f_otorga       char(8)
   DEFINE v_sdi_qry            STRING
   DEFINE v_ruta               VARCHAR(300)

   LET v_sdi_f_ini   = ""
   LET v_sdi_f_fin   = ""
   LET v_sdi_nrp     = ""
   LET v_sdi_nombre  = ""
   LET v_sdi_salario = ""

   LET v_sdi_idx       = 1
   LET v_sdi_ws_status = 0

   --DISPLAY "NSS  : ",v_sdi_nss
   --DISPLAY "Fecha: ",v_sdi_f_otorga

   LET MT_SDI_req.I_NSS    = v_sdi_nss
   LET MT_SDI_req.I_FECHA  = v_sdi_f_otorga

      LET v_ruta = ""
      CALL SI_SalarioDiarioIntegrado_SO_g(v_ruta) RETURNING v_sdi_ws_status

      -- si el webservice NO se ejecuto correctamente
      IF ( v_sdi_ws_status <> 0 ) THEN

         DISPLAY "ERROR al invocar webservice de consulta de pago"
         DISPLAY "CODE       : ", wsError.code
         DISPLAY "CODENS     : ", wsError.codeNS
         DISPLAY "DESRIPTION : ", wsError.description
         DISPLAY "ACTION     : ", wsError.action

          -- se devuelve el codigo de error del WS y fecha nula
         RETURN wsError.code
         LET v_bnd_ws = 0
      ELSE
         LET v_bnd_ws = 1
      END IF

      FOR v_sdi_indice = 1 TO MT_SDI_res.ET_DATA.getLength()

         --se formatea la fecha

         LET v_sdi_f_ini          = MT_SDI_res.ET_DATA[v_sdi_indice].FECHAF
         LET v_sdi_f_fin          = MT_SDI_res.ET_DATA[v_sdi_indice].FECHAT
         LET v_sdi_nrp            = MT_SDI_res.ET_DATA[v_sdi_indice].NRP
         LET v_sdi_nombre         = MT_SDI_res.ET_DATA[v_sdi_indice].NOMBRE
         LET v_sdi_salario        = MT_SDI_res.ET_DATA[v_sdi_indice].SALARIO

      IF v_sdi_nombre IS NOT NULL THEN
         EXIT FOR
      END IF

      END FOR

      LET arr_part_50006[a].nom_patron     = v_sdi_nombre

      IF (v_sdi_salario IS NULL) OR (v_sdi_salario = 0) THEN
         LET v_sdi_salario = (88.36 * 3)
         LET arr_part_50006[a].salario_diario = v_sdi_salario
      ELSE
         LET arr_part_50006[a].salario_diario = v_sdi_salario
      END IF


      --DISPLAY "Nombre patron : ", arr_part_50006[a].nom_patron
      --DISPLAY "SDI           : ",  arr_part_50006[a].salario_diario

   RETURN v_sdi_ws_status
END FUNCTION

FUNCTION fn_crea_tmp_ws()

DATABASE safre_tmp

DROP TABLE IF EXISTS tmp_dwh_liq

CREATE TABLE tmp_dwh_liq (
id_ocg_formalizacion DECIMAL(9,0),
id_ocg_tramite     DECIMAL(9,0),
subproceso         CHAR(1),
cve_entidad_ef     SMALLINT,
nss                CHAR(11),
paterno            CHAR(13),
materno            CHAR(13),
nombre             CHAR(14),
nom_patron         CHAR(30),
viv97              DECIMAL(10,2),
salario_diario     DECIMAL(6,2), --
tpo_moneda         CHAR(1),
monto_credito      DECIMAL(12,2),
valor_conversion   CHAR(10),
monto_cred_pesos   DECIMAL(12,2),
valor_avaluo       DECIMAL(12,2),
f_credito_liq      DATE,
f_otorgamiento     DATE,
tasa_base          CHAR(20),
margen             CHAR(20),
plazo_meses        CHAR(3),
entidad_inmueble   CHAR(2),
mun_inmueble       CHAR(3),
tpo_credito        CHAR(1),
genero             CHAR(1),
f_liquidacion      DATE,
estado_ws          SMALLINT
)

DATABASE safre_viv

END FUNCTION

