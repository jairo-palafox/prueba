--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/Nov/2013
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => DAE                                                           #
#Programa     => DAEL20                                                        #
#Objetivo     => Programa lanzado  para generar el archivode Ajuste            #
#                Amortizaciones Excedentes                                     #
#Fecha inicio => 28/Nov/2013                                                   #
################################################################################

--LANZADOR: DAEL20

DATABASE safre_viv

GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       p_d_inicial      DATE, -- Fecha inicial del periodo para el archivo de salida 
       p_d_final        DATE, -- Fecha final del periodo para el archivo de salida
       r_bnd_fin_oper   SMALLINT
       
   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion 
   LET g_opera_cod   = p_opera_cod   -- genera archivo procesar
  
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
    
      -- Llamado a función que genera el archivo de salida
      CALL fn_archivo_salida(p_usuario, p_d_inicial, p_d_final, p_folio)

      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
         RETURNING r_bnd_fin_oper
         
      IF r_bnd_fin_oper <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         --CALL fn_muestra_inc_operacion(r_bnd_fin_oper)
         DISPLAY "ERROR en fn_actualiza_opera_fin"
      END IF

END MAIN

#OBJETIVO: Generar el archivo de salida de DAE en base al folio seleccionado
FUNCTION fn_archivo_salida(p_usuario, v_d_inicial, v_d_final, p_folio_liquida)
DEFINE p_usuario             LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_v_nom_archivo     CHAR(40), -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_dpe    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
       v_d_final                DATE, -- Fecha final del periodo para el archivo de salida 
       v_s_tot_trabajadores     INTEGER, -- Total de trabajadores solo registros 03
       v_s_tot_registros        INTEGER, -- Total de registros solo registros <> 01 y 09
       v_s_registro             STRING, -- registro a insertar
       v_d_hoy                  CHAR(8),
       v_busca_archivo          STRING,
       v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia                SMALLINT, -- Parametro consecutivo de registro por dia
       v_folio_lote             DECIMAL(9,0),
       p_folio_liquida          DECIMAL(9,0),
       v_convierte_archivo      STRING,
       v_nombre_destino         STRING,
       v_copia_archivo          STRING,
       v_ejecuta_sh             STRING,
       v_consulta               STRING,
       v_QryTxt_1               STRING
       
DEFINE rec_detalles_ajuste RECORD
          v_id_dae_referencia  DECIMAL(9,0) ,
          v_id_derechohabiente DECIMAL(9,0) ,
          v_nss                CHAR(11)     ,
          v_monto_pesos        DECIMAL(16,6),
          v_monto_acciones     DECIMAL(16,6),
          v_folio_liquida      DECIMAL(9,0) ,
          v_fecha_valor        DATE         ,
          v_fecha_liquida      DATE         ,
          v_resul_operacion    SMALLINT     ,
          v_diagnostico        SMALLINT     ,
          v_desc_resul_op      CHAR(10)     , 
          v_desc_diagnostico   CHAR(30)     ,
          v_nombre_dh          CHAR(40)     ,
          v_rendimiento        DECIMAL(16,6),
          v_num_credito        CHAR(10),
          v_periodo_pago       CHAR(4)
END RECORD 

   --
   LET v_cont_dia = 1

   -- se crear el archivo log
   CALL STARTLOG(p_usuario        CLIPPED|| ".DAES07.log")

-- se obtienen la ruta envio del modulo
    SELECT ruta_envio 
      INTO v_c_ruta_env_dpe
      FROM seg_modulo
     WHERE modulo_cod = 'dae'
     
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta  
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "AJAE_"||v_d_hoy||"_"
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_dpe,v_busca_archivo)
        RETURNING v_cont_dia
   
   LET v_reg_dia = v_cont_dia USING "&"
   LET v_v_nom_archivo = "/"||v_busca_archivo||v_reg_dia||".ajdae"
   LET v_v_ruta_nomarch = v_c_ruta_env_dpe CLIPPED || v_v_nom_archivo

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )

   LET  v_folio_lote =  p_folio_liquida
   DISPLAY "Folio Lote       : ", v_folio_lote

   LET v_consulta = "\n SELECT b.id_dae_referencia, ",
                    "\n        a.id_derechohabiente,",
                    "\n        a.nss,               ",
                    "\n        b.monto_pesos,       ",
                    "\n        b.monto_acciones,    ",
                    "\n        b.folio_liquida,     ",
                    "\n        b.fecha_valor,       ",
                    "\n        b.fecha_liquida,     ",
                    "\n        a.resul_operacion,   ",
                    "\n        a.diagnostico        ",
                    "\n FROM   dae_det_ajuste a     ",
                    "\n LEFT OUTER JOIN dae_aceptados_ajuste b ",
                    "\n ON     a.id_dae_ref_ajuste = b.id_dae_ref_ajuste ",
                    "\n WHERE a.folio_lote = ", v_folio_lote ,
                    "\n ORDER BY 3,7 "

      -- Se llena el detalle del trabajador
      PREPARE prp_dae_det_solicitud FROM v_consulta
      DECLARE cur_dae_det_solicitud CURSOR FOR prp_dae_det_solicitud
         
         
         LET v_s_tot_trabajadores = 0

         FOREACH cur_dae_det_solicitud INTO rec_detalles_ajuste.v_id_dae_referencia , 
                                            rec_detalles_ajuste.v_id_derechohabiente,
                                            rec_detalles_ajuste.v_nss               ,
                                            rec_detalles_ajuste.v_monto_pesos       ,
                                            rec_detalles_ajuste.v_monto_acciones    ,
                                            rec_detalles_ajuste.v_folio_liquida     ,
                                            rec_detalles_ajuste.v_fecha_valor       ,
                                            rec_detalles_ajuste.v_fecha_liquida     ,
                                            rec_detalles_ajuste.v_resul_operacion   ,
                                            rec_detalles_ajuste.v_diagnostico  

            IF rec_detalles_ajuste.v_resul_operacion = 1 THEN
               IF rec_detalles_ajuste.v_id_derechohabiente IS NOT NULL OR rec_detalles_ajuste.v_id_dae_referencia IS NOT NULL THEN

                  SELECT num_credito, periodo_pago
                  INTO   rec_detalles_ajuste.v_num_credito,
                         rec_detalles_ajuste.v_periodo_pago
                  FROM   dae_det_solicitud 
                  WHERE  id_derechohabiente =  rec_detalles_ajuste.v_id_derechohabiente
                  AND    id_dae_referencia  = rec_detalles_ajuste.v_id_dae_referencia
               END IF
            END IF

            LET v_s_registro = "02",
                               rec_detalles_ajuste.v_nss,
                               --/PRODINF-228
                               rec_detalles_ajuste.v_num_credito USING "&&&&&&&&&&",
                               rec_detalles_ajuste.v_periodo_pago USING "&&&&",
                               --PRODINF-228/
                               rec_detalles_ajuste.v_monto_pesos *100 USING "&&&&&&&&", 
                               rec_detalles_ajuste.v_monto_acciones  * 1000000 USING "&&&&&&&&&&&&&&&&",
                               rec_detalles_ajuste.v_folio_liquida USING "&&&&&&&&&",
                               rec_detalles_ajuste.v_fecha_valor USING "YYYYMMDD",
                               rec_detalles_ajuste.v_fecha_liquida USING "YYYYMMDD",
                               rec_detalles_ajuste.v_resul_operacion USING "&&",
                               rec_detalles_ajuste.v_diagnostico USING "&&"

            LET v_s_tot_trabajadores = v_s_tot_trabajadores + 1

            LET v_s_tot_registros = v_s_tot_registros + 1

            CALL v_ch_arch_solTransf.writeline(v_s_registro)

         END FOREACH -- Detalle trabajador

   DISPLAY "Total de Registros  : ", v_s_tot_trabajadores

   IF v_s_tot_trabajadores  < 1 THEN
      DISPLAY "No existe información para el folio solicitado"
   ELSE
     -- Actualiza los registros totales a 8 enviados a procesar

     DISPLAY "El archivo se creo satisfactoriamente"
     DISPLAY "Dentro de la siguiente ruta: \n", v_v_ruta_nomarch
     
      --Convierte archivo a DOS
      LET v_v_nom_archivo = v_busca_archivo||v_reg_dia CLIPPED||".ajdae"
      LET v_convierte_archivo = "unix2dos "||" "||v_c_ruta_env_dpe CLIPPED||" "||v_v_nom_archivo
      RUN v_convierte_archivo 

      DISPLAY "\n Convierte archivo a DOS:", v_convierte_archivo

      -- Genera copia a archivo fijo
      LET v_nombre_destino = "AJAE_1.sdae"
      LET v_copia_archivo = "\n cp "||v_c_ruta_env_dpe CLIPPED||"/"||v_v_nom_archivo CLIPPED|| " " ||v_c_ruta_env_dpe CLIPPED||"/"||v_nombre_destino
      RUN v_copia_archivo 
      
      DISPLAY "\n Copia Archivo para Cartera:", v_copia_archivo, "\n"

      --LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/DAE_1.sh"
      --RUN v_ejecuta_sh

      --DISPLAY "\n Se ejecutó la transaferencia a cartera"
   END IF
   
   CALL v_ch_arch_solTransf.close()
   
END FUNCTION --fn_archivo_salida

#Objetivo: genera el número consecutivo por día para el archivo de salida
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
       LET v_consecutivo = fn[15,15]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo

END FUNCTION