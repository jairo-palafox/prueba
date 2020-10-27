--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>OCG                                           #
#Programa          =>OCGS02                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo part 50007 de 43BIS pendientes por #
#                    formalizar.                                   #
#Autor             =>José Eduardo Ventura                          #
#Modificado        =>Emilio Abarca Sánchez                         #
#Fecha inicio      =>16 febrero 2016                               #
#Fecha modifica    =>12 Enero 2017                                 #
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
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nom_arh           STRING
   DEFINE v_folio             CHAR(10)
   DEFINE ch                  base.Channel
   DEFINE v_detalle           STRING
   DEFINE a                   INTEGER
   DEFINE v_cta_t             INTEGER
   DEFINE v_cta_f             INTEGER
   DEFINE bnd_datos1          SMALLINT
   DEFINE bnd_datos2          SMALLINT
   DEFINE v_dia               CHAR(8)
   DEFINE v_nom_arh_resp      STRING
   DEFINE v_s_comando         STRING
   DEFINE f_otorga_ws         char(8)
   DEFINE v_bnd_ws            SMALLINT
   DEFINE v_bnd_escribe       SMALLINT
   DEFINE v_edo_ws            INTEGER

--se agrega para WS
   DEFINE v_ws_status         SMALLINT

   DEFINE arr_part_liquidados DYNAMIC ARRAY OF RECORD
      subproceso              CHAR(1),
      cve_entidad_ef          SMALLINT,
      nss                     CHAR(11),
      paterno                 CHAR(13),
      materno                 CHAR(13),
      nombre                  CHAR(14),
      nom_patron              CHAR(30),
       viv97                  DECIMAL(10,2),
      salario_diario          DECIMAL(6,2), --deben ser ocho posiciones
      tpo_moneda              CHAR(1),
      monto_credito           DECIMAL(12,2),
      valor_conversion        CHAR(10),
      monto_cred_pesos        DECIMAL(12,2),
      valor_avaluo            DECIMAL(12,2),
      f_credito_liq           DATE,
      f_otorgamiento          DATE,
      tasa_base               CHAR(6),
      margen                  CHAR(20),
      tasa                    CHAR(6),
      plaso_meses             CHAR(3),
      entidad_inmueble        char(2),
      mun_inmueble            CHAR(3),
      tpo_credito             CHAR(1),
      genero                  CHAR(1)
   END RECORD

   DEFINE v_tasa_ini          CHAR(20)
   DEFINE v_tasa_fovi          CHAR(13)
   DEFINE v_tasa_base         CHAR(20)
   DEFINE v_aux_tasa_base     CHAR(6)
   DEFINE v_margen            CHAR(20)
   DEFINE v_estado            SMALLINT

END GLOBALS

MAIN

   -- se recuperan los parametros
   LET p_usuario         = ARG_VAL(1)
   LET p_pid             = ARG_VAL(2)
   LET p_proceso_cod     = 3917
   LET p_opera_cod       = 1
   LET p_f_inicial       = ARG_VAL(3)
   LET p_f_final         = ARG_VAL(4)

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   LET v_dia = TODAY USING "ddmmyyyy"

   LET v_nom_arh      = v_ruta_envio CLIPPED ,"/ARP50007",".txt"
   LET v_nom_arh_resp = v_ruta_envio CLIPPED ,"/ARP50007_",v_dia,".ldwh"

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGS07.log")

   CALL fn_display_proceso(0,"INICIA GENERACIÓN ARCHIVO 50007")
   DISPLAY " USUARIO      : ",p_usuario
   DISPLAY " PID          : ",p_pid USING "<<<<<<<<<"
   DISPLAY ""
   
   -- se ejecuta la función que genera el archivo de salida de liquidación
   CALL fn_archivo_part_liquidados() RETURNING v_estado

   IF (v_estado = 1) THEN
   
       DISPLAY " Archivo generado de forma correcta en la ruta: "
       DISPLAY " ",'"',v_nom_arh,'"'
       DISPLAY ""
       DISPLAY " => PROCESO EJECUTADO CORRECTAMENTE"
       DISPLAY ""
       CALL fn_display_proceso(1,"FIN GENERACIÓN ARCHIVO DE 50007")

       -- Actualiza la Operación a exitosa
       CALL fn_actualiza_opera_fin(p_pid,
                                   p_proceso_cod,
                                   p_opera_cod)
                         RETURNING v_estado
   ELSE
      DISPLAY "Ha ocurrido un error al generar el archivo 50007"
      DISPLAY ""
      --Actualiza la operación con estatus de erróneo
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
      
   END IF 

END MAIN

FUNCTION fn_archivo_part_liquidados()

   DEFINE v_qry         STRING
   DEFINE v_indicador   BOOLEAN 
   DEFINE v_env_comando    STRING
   DEFINE v_ind_envio      BOOLEAN

   LET v_indicador = 0  -- Indicador de que no se ha generado el archivo
   LET p_f_inicial = p_f_inicial[4,5],"/",p_f_inicial[1,2],"/",p_f_inicial[7,10]
   LET p_f_final = p_f_final[4,5],"/",p_f_final[1,2],"/",p_f_final[7,10]

  -- DISPLAY "fecha inicial :",p_f_inicial
  -- DISPLAY "fecha final   :",p_f_final

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nom_arh,"w" )
   CALL ch.setDelimiter(NULL)

   CALL arr_part_liquidados.clear()
   -- cadena para archivo de salida con detalle para SP001 tramite
   LET v_qry = "SELECT '1',
                       d.cve_ent_financiera,
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
                       '',
                       f.f_otorga_ent_fin,
                       trim(f.tasa_base),
                       f.margen,
                       f.plazo_credito,
                       f.ent_fed_inmueble,
                       f.mcpio_inmueble,
                       f.tpo_credito,
                       f.genero
                  FROM ocg_formalizacion f,
                       ocg_detalle d
                 WHERE d.id_ocg_detalle = f.id_ocg_detalle
                   AND f.situacion = 50
                   AND f.diagnostico = 1
                   AND f.tpo_credito IN('A','C')
                   ORDER BY f.cve_ent_financiera asc"   
                   ---AND d.f_proceso between ","'",p_f_inicial,"'"," and ","'",p_f_final,"'", "

              --DISPLAY "Query global: ",v_qry

--******************************************************************************
LET a = 1
LET bnd_datos1 = 0
LET bnd_datos2 = 0
--******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_t
     FROM ocg_formalizacion f,
          ocg_detalle d
    WHERE d.id_ocg_detalle = f.id_ocg_detalle
      AND f.diagnostico = 1
      AND f.situacion = 50
      AND f.tpo_credito IN("A","C")
      --AND d.f_proceso BETWEEN p_f_inicial AND p_f_final

   CALL fn_crea_tmp_ws()

   IF v_cta_t > 0 THEN

      -- se llena arreglo con datos de tabla tramite
      PREPARE prp_part_liquidados FROM v_qry
      DECLARE cur_part_liquidados CURSOR FOR prp_part_liquidados

      FOREACH cur_part_liquidados INTO arr_part_liquidados[a].subproceso,
                                       arr_part_liquidados[a].cve_entidad_ef,
                                       arr_part_liquidados[a].nss,
                                       arr_part_liquidados[a].paterno,
                                       arr_part_liquidados[a].materno,
                                       arr_part_liquidados[a].nombre,
                                       arr_part_liquidados[a].nom_patron,
                                       arr_part_liquidados[a].viv97,
                                       arr_part_liquidados[a].salario_diario,
                                       arr_part_liquidados[a].tpo_moneda,
                                       arr_part_liquidados[a].monto_credito,
                                       arr_part_liquidados[a].valor_conversion,
                                       arr_part_liquidados[a].monto_cred_pesos,
                                       arr_part_liquidados[a].valor_avaluo,
                                       arr_part_liquidados[a].f_credito_liq,
                                       arr_part_liquidados[a].f_otorgamiento,
                                       v_tasa_base,
                                       arr_part_liquidados[a].margen,
                                       arr_part_liquidados[a].plaso_meses,
                                       arr_part_liquidados[a].entidad_inmueble,
                                       arr_part_liquidados[a].mun_inmueble,
                                       arr_part_liquidados[a].tpo_credito,
                                       arr_part_liquidados[a].genero

--*******************************************************************************
-- 21062018 se agrega consulta a WS para saber el salario diario integrado
LET f_otorga_ws = arr_part_liquidados[a].f_otorgamiento USING "yyyymmdd" 
CALL fn_sdi(arr_part_liquidados[a].nss,f_otorga_ws) RETURNING v_ws_status

--*******************************************************************************

        LET v_tasa_ini  = v_tasa_base
        LET v_tasa_fovi = v_tasa_ini[1,13]

        IF(v_tasa_fovi = " T. BASE FOVI") THEN
           LET v_tasa_base = v_tasa_ini[14,20]
        END IF 
        
         --  Función numérica para estraer la tasa base
         LET v_aux_tasa_base = fn_numerico(v_tasa_base)

         -- Valida el margen
         LET v_margen = fn_numerico(arr_part_liquidados[a].margen)

        -- CALL fn_escribe()

         INSERT INTO safre_tmp:tmp_ws_dwh VALUES (
         a,
         arr_part_liquidados[a].subproceso,
         arr_part_liquidados[a].cve_entidad_ef,
         arr_part_liquidados[a].nss,
         arr_part_liquidados[a].paterno,
         arr_part_liquidados[a].materno,
         arr_part_liquidados[a].nombre,
         arr_part_liquidados[a].nom_patron,
         arr_part_liquidados[a].viv97,
         arr_part_liquidados[a].salario_diario,
         arr_part_liquidados[a].tpo_moneda,
         arr_part_liquidados[a].monto_credito,
         arr_part_liquidados[a].valor_conversion,
         arr_part_liquidados[a].monto_cred_pesos,
         arr_part_liquidados[a].valor_avaluo,
         arr_part_liquidados[a].f_credito_liq,
         arr_part_liquidados[a].f_otorgamiento,
         v_tasa_base,
         arr_part_liquidados[a].margen,
         arr_part_liquidados[a].plaso_meses,
         arr_part_liquidados[a].entidad_inmueble,
         arr_part_liquidados[a].mun_inmueble,
         arr_part_liquidados[a].tpo_credito,
         arr_part_liquidados[a].genero,
         v_bnd_ws)

         LET a = a+1
         
      END FOREACH

      IF arr_part_liquidados[a].nss IS NULL THEN
         CALL arr_part_liquidados.deleteElement(arr_part_liquidados.getLength())
         LET a = a - 1
      END IF

      LET v_edo_ws = ""

      SELECT COUNT(*)
        INTO v_edo_ws
        FROM safre_tmp:tmp_ws_dwh
       WHERE estado_ws = 0

      IF v_edo_ws = 0 THEN
         FOR a = 1 TO arr_part_liquidados.getLength()
            CALL fn_escribe()
         END FOR
         -- Genera sumario al archivo
         CALL fn_crea_sumario()
         LET bnd_datos1 = 0
      ELSE
         LET bnd_datos1 = 2
      END IF

   ELSE
      LET bnd_datos1 = 1
   END IF

   IF (bnd_datos1 >= 1) THEN
      CALL fn_sin_datos()
   END IF
   
   CALL ch.close()
   
      -- se crea comando para copia de archivo
      LET v_s_comando = "cp ",v_nom_arh," ",v_nom_arh_resp
      --DISPLAY v_s_comando
      RUN v_s_comando

      -- Convierte formato UNIX a DOS
      LET v_s_comando = "sed 's/$/\r/' ",v_nom_arh_resp," > ",v_nom_arh
      RUN v_s_comando

      LET v_indicador = 1 -- Ha terminado de generarse el archivo
      
         -- Ejecución del Script de envío a TRM
      LET v_env_comando = "sh ","/opt/Interpel/Scripts/SOA16032.sh" #Envío de Producción
   
      RUN v_env_comando RETURNING v_ind_envio

      IF(v_ind_envio = 0) THEN
         DISPLAY " Archivo de 43BIS pendientes por formalizar enviado."
         DISPLAY ""
      ELSE 
         DISPLAY " No se pudo enviar el archivo 43BIS pendientes por formalizar a TRM"
         DISPLAY ""
      END IF

      RETURN v_indicador

END FUNCTION 

FUNCTION fn_escribe()

   --DEFINE v_detalle           STRING
   DEFINE v_nombre_completo   CHAR(40)
   DEFINE v_tasa              STRING
   DEFINE v_cnt_tasa          INTEGER
   DEFINE v_tasa_int          DECIMAL(4,2)
   DEFINE v_salario           DECIMAL(10,2)
   DEFINE v_curp              CHAR(18)
   DEFINE v_aux_genero        CHAR(1)
   DEFINE v_sexo              CHAR(1)

 {
   LET v_tasa = arr_part_liquidados[a].tasa

   CALL v_tasa.getIndexOf('%',1) RETURNING v_cnt_tasa

   IF v_cnt_tasa IS NOT NULL OR
      v_cnt_tasa > 0 THEN
      CALL v_tasa.subString(1,v_cnt_tasa) RETURNING arr_part_liquidados[a].tasa
   END IF
}
   --LET v_tasa_int = arr_part_liquidados[a].tasa

   IF arr_part_liquidados[a].tpo_moneda = 1 THEN
      LET arr_part_liquidados[a].monto_cred_pesos = arr_part_liquidados[a].monto_credito
      LET arr_part_liquidados[a].valor_conversion = 1
   END IF

   IF arr_part_liquidados[a].tpo_moneda = 2 THEN
      LET v_salario =(73.04)*(30.4)
      LET arr_part_liquidados[a].monto_cred_pesos = v_salario * arr_part_liquidados[a].monto_credito
      LET arr_part_liquidados[a].valor_conversion = v_salario
   END IF

   IF arr_part_liquidados[a].tpo_moneda = 3 THEN
      LET v_salario = v_salario * (5.556329)
      LET arr_part_liquidados[a].monto_cred_pesos = v_salario * arr_part_liquidados[a].monto_credito
      LET arr_part_liquidados[a].valor_conversion = 5.556329
   END IF

    LET v_nombre_completo = arr_part_liquidados[a].paterno CLIPPED," ",
                           arr_part_liquidados[a].materno CLIPPED," ",
                           arr_part_liquidados[a].nombre

   # Regla para la tasa base
   IF(v_aux_tasa_base IS NULL) OR (v_aux_tasa_base = "      ") THEN
      LET v_aux_tasa_base = "  0.00"
   ELSE 
      IF (v_aux_tasa_base > 99.99) AND (v_aux_tasa_base < 9999.99) THEN 
         LET v_aux_tasa_base = v_aux_tasa_base / 100
      END IF 
  
      IF (v_aux_tasa_base > 9999.99) AND (v_aux_tasa_base < 99999.99) THEN 
         LET v_aux_tasa_base = v_aux_tasa_base / 1000
      END IF 
            
      IF (v_aux_tasa_base > 99999.99) AND (v_aux_tasa_base < 999999.99) THEN 
         LET v_aux_tasa_base = v_aux_tasa_base / 10000
      END IF

      IF(v_aux_tasa_base > 999999.99) AND (v_aux_tasa_base < 9999999.99) THEN
         LET v_aux_tasa_base = v_aux_tasa_base / 100000 
      END IF 

      IF (v_aux_tasa_base > 9999999.99) AND (v_aux_tasa_base < 99999999.99) THEN
         LET v_aux_tasa_base = v_aux_tasa_base / 1000000  
      END IF 

      IF (v_aux_tasa_base > 99999999.99  ) AND (v_aux_tasa_base < 999999999.99) THEN
         LET v_aux_tasa_base = v_aux_tasa_base / 10000000  
      END IF 
      
   END IF 

   #Regla para el margen
   IF(v_margen IS NULL) OR (v_margen = "      ") THEN
      LET v_margen = "  0.00"
   ELSE 
      IF(v_margen > 99.99) AND (v_margen < 9999.99) THEN 
         LET v_margen = v_margen / 100
      END IF 

      IF (v_margen > 9999.99) AND (v_margen < 99999.99) THEN
         LET v_margen = v_margen / 1000
      END IF 

      IF (v_margen > 99999.99) AND (v_margen < 999999.99) THEN
         LET v_margen = v_margen / 10000
      END IF

      IF (v_tasa_base > 999999.99) AND (v_margen < 9999999.99) THEN 
         LET v_margen = v_margen / 100000 
      END IF

      IF(v_margen > 9999999.99) AND (v_margen < 99999999.99) THEN 
         LET v_margen = v_margen / 1000000 
      END IF 

      IF(v_margen > 99999999.99) AND (v_margen < 999999999.99) THEN
         LET v_margen = v_margen /  10000000 
      END IF 
      
   END IF 
   
   IF (arr_part_liquidados[a].tpo_credito = "A") THEN
      LET arr_part_liquidados[a].tpo_credito = " "
   END IF 
   
   IF (arr_part_liquidados[a].genero IS NULL) OR (arr_part_liquidados[a].genero = " ") THEN
      -- Recupera el género de afi_derechohabiente
     SELECT curp
        INTO v_curp
        FROM afi_derechohabiente
       WHERE nss = arr_part_liquidados[a].nss

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
            WHERE nss = arr_part_liquidados[a].nss
    
          LET v_aux_genero = v_sexo

          IF (v_aux_genero = "1") THEN
             LET v_aux_genero = "M"
          ELSE 
             IF (v_aux_genero = "2") THEN
                LET v_aux_genero = "F"
             END IF 
          END IF 
       END IF

       
      -- Se asigna valor del género al areglo
      LET arr_part_liquidados[a].genero = v_aux_genero
       
   END IF 

   --LET arr_part_liquidados[a].salario_diario =(73.04)*(3)

   LET v_detalle = 
      arr_part_liquidados[a].subproceso        USING "&",
      arr_part_liquidados[a].cve_entidad_ef    USING "&&&",
      arr_part_liquidados[a].nss               ,
      v_nombre_completo                        ,
      arr_part_liquidados[a].nom_patron        ,
      arr_part_liquidados[a].viv97             USING "#,###,##&.&&",
      arr_part_liquidados[a].salario_diario    USING "#,##&.&&",
      arr_part_liquidados[a].tpo_moneda        ,
      arr_part_liquidados[a].monto_credito     USING "###,###,##&.&&",
      arr_part_liquidados[a].valor_conversion  USING "######&.&&",
      arr_part_liquidados[a].monto_cred_pesos  USING "###,###,##&.&&",
      arr_part_liquidados[a].valor_avaluo      USING "###,###,##&.&&",
      arr_part_liquidados[a].f_credito_liq     USING "yyyy/mm/dd", -- fecha de respuesta 
      arr_part_liquidados[a].f_otorgamiento    USING "yyyy/mm/dd", -- fecha de otorgamiento entidad financiera
      v_aux_tasa_base                          USING "#&.&&&",
      v_margen                                 USING "#&.&&&",
      v_aux_tasa_base                          USING "#&.&&&",
      arr_part_liquidados[a].plaso_meses       ,
      arr_part_liquidados[a].entidad_inmueble  USING "&&",
      arr_part_liquidados[a].mun_inmueble      USING "&&&",
      arr_part_liquidados[a].tpo_credito       ,
      arr_part_liquidados[a].genero

      CALL ch.writeLine([v_detalle])
    
END FUNCTION

FUNCTION fn_crea_sumario()

   --Vars para sumario
   DEFINE v_sumario           CHAR(206)
   DEFINE v_filler            CHAR(206)
   DEFINE v_pos_ini           INTEGER 
   DEFINE k                   INTEGER 
   DEFINE v_tot_reg           STRING 
      
   LET v_tot_reg = a  -- total de registros
   
   LET v_tot_reg = v_tot_reg.trim()
  
   LET v_pos_ini = v_tot_reg.getLength() + 16   -- Obtiene la ultima posición de la cadena "APOYO INFONAVIT" más el total de registros
                                                -- para empezar a contar desde ahí 
   FOR k = v_pos_ini TO 206       
         LET v_filler[k] = " "                         
   END FOR 
  
   LET v_sumario = "APOYO INFONAVIT ",v_tot_reg,v_filler

   CALL  ch.writeLine([v_sumario])

END FUNCTION 

FUNCTION fn_sin_datos()

   IF bnd_datos1 = 1 THEN 
      LET v_detalle = "NO SE ENCONTRARON REGISTROS CON LOS PARÁMETROS DE FECHA INGRESADOS"
   END IF

   IF bnd_datos1 = 2 THEN
      LET v_detalle = "NO SE PUDO CONSUMIR WEB SERVICE"
   END IF

   CALL ch.writeLine([v_detalle])
END FUNCTION

FUNCTION fn_sin_ws()

   LET v_detalle = "NO SE PUSO CONECTAR CON WS"

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
         --LET v_bnd_ws = 1

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

         LET arr_part_liquidados[a].nom_patron     = v_sdi_nombre

         IF (v_sdi_salario IS NULL) OR (v_sdi_salario = 0) THEN
            LET v_sdi_salario = (88.36 * 3)
            LET arr_part_liquidados[a].salario_diario = v_sdi_salario
         ELSE
            LET arr_part_liquidados[a].salario_diario = v_sdi_salario
         END IF

   RETURN v_sdi_ws_status
END FUNCTION

FUNCTION fn_crea_tmp_ws()

DATABASE safre_tmp

DROP TABLE IF EXISTS tmp_ws_dwh

CREATE TABLE tmp_ws_dwh (
 id_registro             INTEGER,
 subproceso              CHAR(1),
 cve_entidad_ef          SMALLINT,
 nss                     CHAR(11),
 paterno                 CHAR(13),
 materno                 CHAR(13),
 nombre                  CHAR(14),
 nom_patron              CHAR(30),
  viv97                  DECIMAL(10,2),
 salario_diario          DECIMAL(6,2), 
 tpo_moneda              CHAR(1),
 monto_credito           DECIMAL(12,2),
 valor_conversion        CHAR(10),
 monto_cred_pesos        DECIMAL(12,2),
 valor_avaluo            DECIMAL(12,2),
 f_credito_liq           DATE,
 f_otorgamiento          DATE,
 tasa_base               CHAR(6),
 margen                  CHAR(20),
 plaso_meses             CHAR(3),
 entidad_inmueble        char(2),
 mun_inmueble            CHAR(3),
 tpo_credito             CHAR(1),
 genero                  CHAR(1),
 estado_ws               SMALLINT)
DATABASE safre_viv

END FUNCTION
