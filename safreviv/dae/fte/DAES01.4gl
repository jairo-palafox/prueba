################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
#Modulo            => DAE                                                      #
#Programa          => DAES01                                                   #
#Objetivo          => Programa lanzado para seleccionar el folio para generar  #
#                     el archivo de Devolución de Amortizaciones Excedentes    #
#Fecha inicio      => 05/05/2013                                               #
################################################################################
--==============================================================================
-- Version                    => 1.0.0
-- Fecha ultima modificacion  => 05/05/2013
-- 05/05/2013 --Se agrega ejecución de script para transferir archivos -AG
--==============================================================================

--Lanzado: DAEL05

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
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       p_d_inicial      DATE, -- Fecha inicial del periodo para el archivo de salida 
       p_d_final        DATE, -- Fecha final del periodo para el archivo de salida
       r_bnd_fin_oper   SMALLINT
       
   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
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
      CALL fn_archivo_salida(p_usuario_cod, p_d_inicial, p_d_final, p_folio)

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
FUNCTION fn_archivo_salida(p_usuario_cod, v_d_inicial, v_d_final, p_folio_liquida)
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_v_nom_archivo       CHAR(40), -- nombre del archivo de salida
       v_nom_archivo_axway   CHAR(40), -- nombre para transferenia de AXWAY
       v_v_ruta_nomarch      VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_dpe      LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf   BASE.CHANNEL, -- manejador de apuntador hacia archivo
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
       v_ejecuta_sh             STRING

DEFINE rec_dae_det_solicitud RECORD 
       V_tipo_registro        CHAR(2),
       v_id_dae_referencia    DECIMAL(9,0) ,
       v_id_derechohabiente   DECIMAL(9,0) ,
       v_folio                DECIMAL(9,0) ,
       v_num_credito          CHAR(10)     ,
       v_fecha_pago           DATE         ,
       v_periodo_pago         CHAR(4)      ,
       v_registro_pago        CHAR(8)      ,
       v_origen               CHAR(1)      ,
       v_delegacion           CHAR(2)      ,
       v_importe_amort        DECIMAL(16,6),
       v_total_importe        DECIMAL(16,6),
       v_precio_aivs          DECIMAL(16,6),
       v_monto_aivs           DECIMAL(16,6),
       v_tipo_pago            CHAR(3)      ,
       v_nss                  CHAR(11)     ,
       v_entidad_receptora    CHAR(3)      ,
       v_folio_liquida        DECIMAL(9,0) ,
       v_fecha_liquida        DATE         ,
       v_estado               SMALLINT     ,
       v_resul_opera          CHAR(2)      ,
       v_motivo_rechazo       SMALLINT
END RECORD

   --
   LET v_cont_dia = 1

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DAES01.log")

-- se obtienen la ruta envio del modulo
    SELECT ruta_envio 
      INTO v_c_ruta_env_dpe
      FROM seg_modulo
     WHERE modulo_cod = 'dae'
     
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta  
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "DAE_"||v_d_hoy||"_"

   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_dpe,v_busca_archivo)
        RETURNING v_cont_dia
   
   LET v_reg_dia = v_cont_dia USING "&"
   LET v_v_nom_archivo = "/"||v_busca_archivo||v_reg_dia||".sdae"
   LET v_v_ruta_nomarch = v_c_ruta_env_dpe CLIPPED || v_v_nom_archivo
   --
   --Se crea nombre de archivo para transferencia de AXWAY 
   --LET v_d_hoy  = TODAY  USING "YYYYMMDD"
   --LET v_nom_archivo_axway =  "dae_" ||v_d_hoy ||"_"||v_reg_dia||".sdae"
   LET v_nom_archivo_axway =  "dae_"
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )

   LET  v_folio_lote =  p_folio_liquida
   DISPLAY " Folio Lote       : ", v_folio_lote
   
      -- Se llena el detalle del trabajador
      DECLARE cur_dae_det_solicitud CURSOR FOR 
            SELECT a.id_dae_referencia ,
                   a.id_derechohabiente,
                   a.folio,
                   a.num_credito,
                   a.fecha_pago,
                   a.periodo_pago,
                   a.registro_pago,
                   a.origen,
                   a.delegacion,
                   a.importe_amort,
                   a.total_importe,
                   a.precio_aivs,
                   a.monto_aivs,
                   a.tipo_pago,
                   a.nss,
                   a.entidad_receptora ,
                   a.folio_liquida,
                   a.fecha_liquida,
                   a.id_origen,
                   a.resul_opera,
                   a.motivo_rechazo
              FROM dae_det_solicitud a
             WHERE a.folio = v_folio_lote

         LET v_s_tot_trabajadores = 0

         FOREACH cur_dae_det_solicitud 
            INTO rec_dae_det_solicitud.v_id_dae_referencia ,
                 rec_dae_det_solicitud.v_id_derechohabiente,
                 rec_dae_det_solicitud.v_folio,
                 rec_dae_det_solicitud.v_num_credito,
                 rec_dae_det_solicitud.v_fecha_pago,
                 rec_dae_det_solicitud.v_periodo_pago,
                 rec_dae_det_solicitud.v_registro_pago,
                 rec_dae_det_solicitud.v_origen,
                 rec_dae_det_solicitud.v_delegacion,
                 rec_dae_det_solicitud.v_importe_amort,
                 rec_dae_det_solicitud.v_total_importe,
                 rec_dae_det_solicitud.v_precio_aivs,
                 rec_dae_det_solicitud.v_monto_aivs,
                 rec_dae_det_solicitud.v_tipo_pago,
                 rec_dae_det_solicitud.v_nss,
                 rec_dae_det_solicitud.v_entidad_receptora,
                 rec_dae_det_solicitud.v_folio_liquida,
                 rec_dae_det_solicitud.v_fecha_liquida,
                 rec_dae_det_solicitud.v_estado,
                 rec_dae_det_solicitud.v_resul_opera,
                 rec_dae_det_solicitud.v_motivo_rechazo

                 --Recupera NSS cuando origen es Dispersión
                 IF rec_dae_det_solicitud.v_estado = 2 THEN 
                    SELECT nss 
                    INTO   rec_dae_det_solicitud.v_nss 
                    FROM   afi_derechohabiente 
                    WHERE  id_derechohabiente = rec_dae_det_solicitud.v_id_derechohabiente
                 END IF   

                 IF rec_dae_det_solicitud.v_motivo_rechazo IS NULL THEN
                    LET rec_dae_det_solicitud.v_motivo_rechazo = 0  
                 END IF

            LET v_s_registro = "01",
                               rec_dae_det_solicitud.v_num_credito,
                               rec_dae_det_solicitud.v_fecha_pago USING "YYYYMMDD",
                               rec_dae_det_solicitud.v_periodo_pago ,
                               rec_dae_det_solicitud.v_registro_pago,
                               rec_dae_det_solicitud.v_origen,
                               rec_dae_det_solicitud.v_delegacion USING "&&",
                               rec_dae_det_solicitud.v_importe_amort * 100 USING "&&&&&&&&",
                               rec_dae_det_solicitud.v_total_importe * 100 USING "&&&&&&&&&&",
                               rec_dae_det_solicitud.v_tipo_pago,
                               rec_dae_det_solicitud.v_nss,
                               rec_dae_det_solicitud.v_entidad_receptora USING "&&&",
                               rec_dae_det_solicitud.v_folio_liquida USING "&&&&&&&&&",
                               rec_dae_det_solicitud.v_fecha_liquida USING "YYYYMMDD",
                               rec_dae_det_solicitud.v_resul_opera   USING "&&",
                               rec_dae_det_solicitud.v_motivo_rechazo USING "&&",
                               rec_dae_det_solicitud.v_precio_aivs * 1000000 USING "&&&&&&&&&&",
                               rec_dae_det_solicitud.v_monto_aivs * 1000000 USING "&&&&&&&&&&&&&&&&"

            LET v_s_tot_trabajadores = v_s_tot_trabajadores + 1

            LET v_s_tot_registros = v_s_tot_registros + 1

            CALL v_ch_arch_solTransf.writeline(v_s_registro)

         END FOREACH -- Detalle trabajador

   DISPLAY " Total de Registros  : ", v_s_tot_trabajadores

   IF v_s_tot_trabajadores  < 1 THEN
      DISPLAY " No existe información para el folio solicitado"
   ELSE
     -- Actualiza los registros totales a 8 enviados a procesar
     UPDATE dae_det_solicitud 
     SET    estado = 5
     WHERE  folio = v_folio_lote
     AND    estado <> 6

     DISPLAY "\n El archivo se creo satisfactoriamente dentro de la siguiente ruta: \n   ",
              v_v_ruta_nomarch
      
      --Convierte archivo a DOS
      LET v_v_nom_archivo = v_busca_archivo||v_reg_dia CLIPPED||".sdae"

      -- Genera copia a archivo fijo
      LET v_nombre_destino = "DAE_1.sdae"
      LET v_copia_archivo = "\n cp "||v_c_ruta_env_dpe CLIPPED||"/"||v_v_nom_archivo CLIPPED|| " " ||v_c_ruta_env_dpe CLIPPED||"/"||v_nombre_destino
      RUN v_copia_archivo 

      DISPLAY "\n Copia Archivo para Cartera: \n   ", v_copia_archivo

      LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/DAE_1.sh"
      RUN v_ejecuta_sh

      DISPLAY "\n Se ejecutó la transaferencia a cartera"

      DISPLAY " -------------------------------- "

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
       LET v_consecutivo = fn[14,14]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo

END FUNCTION