--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14/02/2017
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
#Modulo            => DAE                                                      #
#Programa          => DAES08                                                   #
#Objetivo          => Lanzado del extractor de Detalle de Ajustes DAE          #
#Fecha inicio      => 14/02/2017                                               #
################################################################################

DATABASE safre_viv

GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_condicion   STRING
END GLOBALS
MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       p_d_inicial      DATE, -- Fecha inicial del periodo para el archivo de salida 
       p_d_final        DATE, -- Fecha final del periodo para el archivo de salida
       r_bnd_fin_oper   SMALLINT,
       i                INTEGER

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_condicion      = ARG_VAL(7)
--DISPLAY p_condicion
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DAES08.log")
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
   -- Llamado a funci�n que genera el archivo de salida
   CALL fn_consulta_registros(p_usuario_cod, p_d_inicial, p_d_final, p_folio)
   RETURNING i

   IF i >= 1 THEN 
      CALL fn_archivo_salida(p_usuario_cod, p_d_inicial, p_d_final, p_folio)
   END IF

   -- se invoca la finalizacion de la operacion
   CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
        RETURNING r_bnd_fin_oper
         
   IF r_bnd_fin_oper <> 0 THEN
         CALL fn_muestra_inc_operacion(r_bnd_fin_oper)
         DISPLAY "ERROR AL FINALIZAR LA OPERACI�N", r_bnd_fin_oper
      END IF

END MAIN

#OBJETIVO: Generar el archivo de salida de DAE en base al folio seleccionado
FUNCTION fn_consulta_registros(p_usuario_cod, v_d_inicial, v_d_final, p_folio_liquida)
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
       v_d_final                DATE, -- Fecha final del periodo para el archivo de salida 
       i     INTEGER, -- Total de trabajadores solo registros 03
       v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
       v_folio_lote             DECIMAL(9,0),
       p_folio_liquida          DECIMAL(9,0),
       v_QryTxt                 STRING,
       v_tot_monto_pesos        DECIMAL(16,6),
       v_tot_monto_aivs         DECIMAL(16,6)

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD
          v_id_derechohabiente DECIMAL(9,0),
          v_num_credito        CHAR(10),
          v_fecha_pago         DATE,
          v_periodo_pago       CHAR(4),
          v_fec_reg_pag        CHAR(8),
          v_delegacion         CHAR(2),
          v_imp_amortizacion   DECIMAL(16,6),
          v_monto_aivs         DECIMAL(16,6),
          v_tipo_pago          CHAR(3),
          v_nss                CHAR(11),
          v_ent_receptora      CHAR(3),
          v_folio_liquida      DECIMAL(9,0),
          v_fec_ing_saci       DATE,
          v_ruta_nombre        STRING
END RECORD 

   LET v_cont_dia = 1

   LET  v_folio_lote =  p_folio_liquida

   DELETE 
   FROM safre_tmp:dae_tmp_detalles_extractor;

   DISPLAY "\n   Inicia consulta de registros restituidos."

         LET v_QryTxt = "\n SELECT a.id_derechohabiente,", 
                        "\n        a.num_credito, ",
                        "\n        a.fecha_pago, ",
                        "\n        a.periodo_pago, ",
                        "\n        a.registro_pago, ",
                        "\n        a.delegacion, ",
                        "\n        c.monto_pesos, ",
                        "\n        c.monto_acciones, ",
                        "\n        a.tipo_pago, ",
                        "\n        a.nss, ",
                        "\n        a.entidad_receptora,",
                        "\n        c.folio_liquida, ",
                        "\n        c.fecha_liquida ",
                        "\n FROM   dae_det_solicitud a ",
                        "\n INNER JOIN dae_aceptados_ajuste c ",
                        "\n ON     a.id_dae_referencia = c.id_dae_referencia ",
                        --"\n WHERE  a.status_retiro = 2 ",
                        --"\n AND    a.folio_ajuste IS NOT NULL "
                        "\n WHERE a.folio_ajuste IS NOT NULL "
      IF p_condicion <> " 1=1" THEN
         LET v_QryTxt = v_QryTxt , "\n AND    ", p_condicion  
      END IF
--DISPLAY v_QryTxt
      PREPARE prp_detalles_12 FROM v_QryTxt
      DECLARE cur_detalles_12 CURSOR FOR prp_detalles_12 

      LET i = 1
      LET v_tot_monto_pesos = 0;
      LET v_tot_monto_aivs  = 0;

      FOREACH cur_detalles_12 INTO arr_detalles[i].v_id_derechohabiente,
                                   arr_detalles[i].v_num_credito,
                                   arr_detalles[i].v_fecha_pago,
                                   arr_detalles[i].v_periodo_pago,
                                   arr_detalles[i].v_fec_reg_pag,
                                   arr_detalles[i].v_delegacion,
                                   arr_detalles[i].v_imp_amortizacion ,
                                   arr_detalles[i].v_monto_aivs,
                                   arr_detalles[i].v_tipo_pago,
                                   arr_detalles[i].v_nss,
                                   arr_detalles[i].v_ent_receptora,
                                   arr_detalles[i].v_folio_liquida,
                                   arr_detalles[i].v_fec_ing_saci

         INSERT INTO safre_tmp:dae_tmp_detalles_extractor
                (id_derechohabiente,
                 num_credito,
                 fecha_pago,
                 periodo_pago,
                 fec_reg_pag,
                 delegacion,
                 imp_amortizacion,
                 monto_aivs,
                 tipo_pago,
                 nss,
                 ent_receptora,
                 folio_liquida,
                 fec_ing_saci)
         VALUES (arr_detalles[i].v_id_derechohabiente,
                 arr_detalles[i].v_num_credito,
                 arr_detalles[i].v_fecha_pago,
                 arr_detalles[i].v_periodo_pago,
                 arr_detalles[i].v_fec_reg_pag,
                 arr_detalles[i].v_delegacion,
                 arr_detalles[i].v_imp_amortizacion ,
                 arr_detalles[i].v_monto_aivs,
                 arr_detalles[i].v_tipo_pago,
                 arr_detalles[i].v_nss,
                 arr_detalles[i].v_ent_receptora,
                 arr_detalles[i].v_folio_liquida,
                 arr_detalles[i].v_fec_ing_saci)
     
         LET i = i+1;
   END FOREACH

   IF i  < 1 THEN
      DISPLAY "   No existe informaci�n para los par�metros solicitados"
   ELSE
      DISPLAY "   Finaliza consulta de registros restituidos."
   END IF

   RETURN i

END FUNCTION --fn_archivo_salida

#Objetivo: genera el n�mero consecutivo por d�a para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio,p_busca_nom_archivo)
DEFINE p_ruta_envio     LIKE seg_modulo.ruta_envio,
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(19)
DEFINE ch base.Channel

    LET v_cmd = "ls -lrt ",p_ruta_envio CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

    LET ch = base.Channel.create()
    CALL ch.setDelimiter(".")
    CALL ch.openPipe(v_cmd,"r")
    WHILE ch.read([fn])
       LET v_consecutivo = fn[14,15]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo

END FUNCTION

#OBJETIVO: Generar el archivo de salida de DAE en base al folio seleccionado
FUNCTION fn_archivo_salida(p_usuario_cod, v_d_inicial, v_d_final, p_folio_liquida)
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_v_nom_archivo       CHAR(40), -- nombre del archivo de salida
       v_nom_archivo_axway   CHAR(40), -- nombre para transferenia de AXWAY
       v_v_ruta_nomarch      VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_dpe      LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf   BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
       v_d_final                DATE, -- Fecha final del periodo para el archivo de salida 
       i     INTEGER, -- Total de trabajadores solo registros 03
       v_s_registro             STRING, -- registro a insertar
       v_d_hoy                  CHAR(8),
       v_busca_archivo          STRING,
       v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia                SMALLINT, -- Parametro consecutivo de registro por dia
       v_folio_lote             DECIMAL(9,0),
       p_folio_liquida          DECIMAL(9,0),
       v_QryTxt                 STRING,
       v_tot_monto_pesos        DECIMAL(16,6),
       v_tot_monto_aivs         DECIMAL(16,6),
       v_s_registro_sum         STRING

DEFINE arr_detalles RECORD
          v_id_derechohabiente DECIMAL(9,0),
          v_num_credito        CHAR(10),
          v_fecha_pago         DATE,
          v_periodo_pago       CHAR(4),
          v_fec_reg_pag        CHAR(8),
          v_delegacion         CHAR(2),
          v_imp_amortizacion   DECIMAL(16,6),
          v_monto_aivs         DECIMAL(16,6),
          v_tipo_pago          CHAR(3),
          v_nss                CHAR(11),
          v_ent_receptora      CHAR(3),
          v_folio_liquida      DECIMAL(9,0),
          v_fec_ing_saci       DATE,
          v_ruta_nombre        STRING
END RECORD 

   LET v_cont_dia = 1

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_dpe
   FROM   seg_modulo
   WHERE  modulo_cod = 'dae'

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta  
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "ERR_"||v_d_hoy||"_"

   --Obtine consecutivo para archivo por d�a
   CALL fn_crea_nombre_archivo(v_c_ruta_env_dpe,v_busca_archivo)
        RETURNING v_cont_dia
   
   LET v_reg_dia = v_cont_dia USING "&&"
   LET v_v_nom_archivo = "/"||v_busca_archivo||v_reg_dia||".sdae"
   LET v_v_ruta_nomarch = v_c_ruta_env_dpe CLIPPED || v_v_nom_archivo
   --
   --Se crea nombre de archivo para transferencia de AXWAY 
   LET v_nom_archivo_axway =  "dae_"
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )

   LET  v_folio_lote =  p_folio_liquida
   
   LET v_QryTxt = "\n SELECT * ",
                  "\n FROM   safre_tmp:dae_tmp_detalles_extractor "

      -- Se llena el detalle del trabajador
      PREPARE prp_detalles FROM v_QryTxt
      DECLARE cur_detalles CURSOR FOR prp_detalles 

         LET i = 1
         LET v_tot_monto_pesos = 0
         LET v_tot_monto_aivs = 0

         FOREACH cur_detalles 
            INTO arr_detalles.v_id_derechohabiente,
                 arr_detalles.v_num_credito,
                 arr_detalles.v_fecha_pago,
                 arr_detalles.v_periodo_pago,
                 arr_detalles.v_fec_reg_pag,
                 arr_detalles.v_delegacion,
                 arr_detalles.v_imp_amortizacion ,
                 arr_detalles.v_monto_aivs,
                 arr_detalles.v_tipo_pago,
                 arr_detalles.v_nss,
                 arr_detalles.v_ent_receptora,
                 arr_detalles.v_folio_liquida,
                 arr_detalles.v_fec_ing_saci
                 
            LET v_s_registro = arr_detalles.v_num_credito,
                               "|",arr_detalles.v_fecha_pago USING "YYYYMMDD",
                               "|",arr_detalles.v_periodo_pago,
                               "|",arr_detalles.v_fec_reg_pag,
                               "|",arr_detalles.v_delegacion,
                               "|",arr_detalles.v_imp_amortizacion ,
                               "|",arr_detalles.v_monto_aivs,
                               "|",arr_detalles.v_tipo_pago,
                               "|",arr_detalles.v_nss,
                               "|",arr_detalles.v_ent_receptora,
                               "|",arr_detalles.v_folio_liquida,
                               "|",arr_detalles.v_fec_ing_saci USING "YYYYMMDD",
                               "|"
            LET v_s_registro = v_s_registro.trim()

            LET v_tot_monto_pesos = v_tot_monto_pesos +  arr_detalles.v_imp_amortizacion 

            IF arr_detalles.v_monto_aivs IS NOT NULL THEN 
               LET v_tot_monto_aivs  = v_tot_monto_aivs + arr_detalles.v_monto_aivs
            END IF 

            CALL v_ch_arch_solTransf.writeline(v_s_registro)
            
            LET i = i + 1
--DISPLAY i
         END FOREACH -- Detalle trabajador

         LET i = i -1
         
         LET v_s_registro_sum = "TOTAL REGISTROS", i,
                                "| TOTAL MONTO PESOS ",v_tot_monto_pesos,
                                "| TOTAL MONTO ACCIONES ",v_tot_monto_aivs

         CALL v_ch_arch_solTransf.writeline(v_s_registro_sum)

   IF i  < 1 THEN
      DISPLAY " No existe informaci�n para los par�metros solicitados"
      DELETE FROM safre_tmp:dae_tmp_detalles_extractor; 
   ELSE
     DELETE FROM safre_tmp:dae_tmp_detalles_extractor; 

     DISPLAY "\n   El archivo se creo satisfactoriamente dentro de la siguiente ruta: \n   ",
              v_v_ruta_nomarch, "\n"

     DISPLAY "   Total registros      :", i , "\n",
             "   Total monto pesos    :",v_tot_monto_pesos, "\n",
             "   Total monto acciones :",v_tot_monto_aivs, "\n"
   END IF
   
   CALL v_ch_arch_solTransf.close()
   
END FUNCTION --fn_archivo_salida