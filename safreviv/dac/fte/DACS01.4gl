--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION: 05/03/2014
--===============================================================

################################################################################
#Modulo       => DAC                                                           #
#Programa     => DACS01                                                        #
#Objetivo     => Lanzado Generar Archivo de Salida  Devolución de Amortización #
#                Mejora tu Casa                                                #
#Fecha inicio => 05/03/2014                                                    #
################################################################################
DATABASE safre_viv

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       v_layout       LIKE cat_operacion.layout_cod,
       v_ruta_rescate STRING,
       v_usuario      LIKE seg_modulo.usuario,
       v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_extension    LIKE cat_operacion.extension,
       v_opera_desc   LIKE cat_operacion.opera_desc,
       v_ruta_listados     LIKE seg_modulo.ruta_listados
END GLOBALS
MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       r_bnd_fin_oper   SMALLINT

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion 
   LET g_opera_cod   = p_opera_cod   -- genera archivo procesar

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida_MTC(p_usuario_cod, p_folio)
      
   -- se invoca la finalizacion de la operacion
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) 
   RETURNING r_bnd_fin_oper

END MAIN

#OBJETIVO: Genera el archivo de salida para Devolución de Amortización Mejora Tu Casa
FUNCTION fn_archivo_salida_MTC(p_usuario_cod, p_folio)
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio             LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_titulo            STRING, -- titulo del mensaje enviado en el correo
       p_mensaje           STRING, -- cuerpo del mensaje enviado
       r_bnd_fin_oper      SMALLINT,
       v_v_nom_archivo     CHAR(40), -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_ruta_envio_dac    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_tot_registros   INTEGER, -- Total de registros solo registros <> 01 y 09
       v_s_registro        STRING, -- registro a insertar
       v_d_hoy             CHAR(8),
       v_busca_archivo     STRING,
       v_cont_dia          SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia           SMALLINT,
       v_convierte_archivo STRING,
       v_nombre_destino    STRING,
       v_copia_archivo     STRING,
       v_ejecuta_sh        STRING, 
       v_nrp_string        STRING
--Encabezado '01'
DEFINE v_tipo_registro_01 CHAR(2),
       v_fecha_archivo DATE
--Detalle '02'
DEFINE arr_detalles_dac RECORD 
       v_tipo_registro_02 CHAR(2)      ,
       v_num_credito      DECIMAL(10,0),
       v_nss              CHAR(11)     ,
       v_periodo_pago     DECIMAL(4)   ,
       v_imp_amortizacion DECIMAL(16,6) ,
       v_folio_sua        DECIMAL(6,0) ,
       v_nrp              CHAR(11)     ,
       v_id_sdd           CHAR(6)      ,
       v_resul_opera      SMALLINT     ,
       v_diagnostico      SMALLINT 
END RECORD 
--Sumario
DEFINE v_tipo_registro_09      CHAR(2),
       v_total_solicitud       INTEGER,
       v_total_aceptados       INTEGER,
       v_total_rechazados      INTEGER,
       v_total_monto           DECIMAL(16,6),
       v_total_monto_aceptado  DECIMAL(16,6),
       v_total_monto_rechazado DECIMAL(16,6)

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DACS01.log")

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_ruta_envio_dac
   FROM   seg_modulo
   WHERE  modulo_cod = 'dac'

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "DAC"||v_d_hoy

   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_ruta_envio_dac,v_busca_archivo)
        RETURNING v_cont_dia

   LET v_reg_dia = v_cont_dia USING "&&"     
   LET v_v_nom_archivo = "/" ||v_busca_archivo||v_reg_dia||".sdac"
   LET v_v_ruta_nomarch = v_ruta_envio_dac CLIPPED || v_v_nom_archivo

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")
--------------------   
   -- Se llena el encabezado de la solicitud
   LET v_tipo_registro_01 = "01"
   LET v_fecha_archivo = TODAY
      
   LET v_s_registro = v_tipo_registro_01 USING "&&",
                      v_fecha_archivo USING "YYYYMMDD" 
      
   CALL v_ch_arch_solTransf.write([v_s_registro])
--------------------      
   -- Se llena el detalle de cada solicitud
   DECLARE cur_detalles_registro CURSOR FOR 
      SELECT "02",
             num_credito,
             nss,
             periodo_pago,
             imp_amortizacion * 100,
             folio_sua,
             nrp,
             id_sdd,
             resul_opera,
             diagnostico
      FROM   dac_det_solicitud 
      WHERE  folio_integracion = p_folio;
   
   FOREACH cur_detalles_registro INTO arr_detalles_dac.*

      LET v_nrp_string = arr_detalles_dac.v_nrp  
      
      LET v_s_registro = arr_detalles_dac.v_tipo_registro_02 USING "&&"
                         , arr_detalles_dac.v_num_credito USING "&&&&&&&&&&"
                         , arr_detalles_dac.v_nss
                         , arr_detalles_dac.v_periodo_pago USING "&&&&"
                         , arr_detalles_dac.v_imp_amortizacion USING "&&&&&&&&"
                         , arr_detalles_dac.v_folio_sua USING "&&&&&&"
                         , v_nrp_string
                         , arr_detalles_dac.v_id_sdd USING "&&&&&&"
                         , arr_detalles_dac.v_resul_opera USING "&&"
                         , arr_detalles_dac.v_diagnostico USING "&&&"

      LET v_s_tot_registros = v_s_tot_registros + 1

      CALL v_ch_arch_solTransf.writeline(v_s_registro)
   END FOREACH 
-------------------
   --Se llena sumario 

   LET v_tipo_registro_09 = "09"
   
   --Obtiene el total de ACEPTADOS 
   SELECT COUNT(id_dac_solicitud),
          SUM(imp_amortizacion)
   INTO   v_total_aceptados, 
          v_total_monto_aceptado
   FROM   dac_det_solicitud
   WHERE  folio_integracion = p_folio
   AND    resul_opera = 1;

   --Obtiene el total de RECHAZADOS
   SELECT COUNT(id_dac_solicitud),
          SUM(imp_amortizacion)
   INTO   v_total_rechazados, 
          v_total_monto_rechazado
   FROM   dac_det_solicitud
   WHERE  folio_integracion = p_folio
   AND    resul_opera = 2;

   --Obtiene el total de registros
   SELECT COUNT(id_dac_solicitud),
          SUM(imp_amortizacion)
   INTO   v_total_solicitud, 
          v_total_monto
   FROM   dac_det_solicitud
   WHERE  folio_integracion = p_folio;

   LET v_s_registro =  v_tipo_registro_09 USING "&&",
                       v_total_solicitud  USING "&&&&",
	                   v_total_aceptados  USING "&&&&",
                       v_total_rechazados USING "&&&&",
                       v_total_monto      USING "&&&&&&&&.##",
                       v_total_monto_aceptado USING "&&&&&&&&.##",
                       v_total_monto_rechazado USING "&&&&&&&&.##"

   CALL v_ch_arch_solTransf.writeline(v_s_registro)
-------
   IF v_s_tot_registros = 1 THEN
      DISPLAY "No existe información para generar el archivo"
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                  RETURNING r_bnd_fin_oper
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de generación de archivo no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   ELSE
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    "
      DISPLAY "# El archivo se creo satisfactoriamente"
      DISPLAY "#    "
      DISPLAY "# Ruta y nombre del archivo: ",v_v_ruta_nomarch
      DISPLAY "#    "
      DISPLAY "# Numero de solicitudes: "||v_total_solicitud
      DISPLAY "# Total de Aceptados   : "||v_total_aceptados
      DISPLAY "# Total de Rechazados  : "||v_total_rechazados
      DISPLAY "#    "
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"

      --Convierte archivo a DOS
      LET v_v_nom_archivo = v_busca_archivo||v_reg_dia CLIPPED||".sdac"
      LET v_convierte_archivo = "unix2dos "||" "||v_ruta_envio_dac CLIPPED||" "||v_v_nom_archivo
      RUN v_convierte_archivo 

      DISPLAY "\n Convierte archivo a DOS:", v_convierte_archivo

      -- Genera copia a archivo fijo
      LET v_nombre_destino = "saci_dpieamo.sdac"
      LET v_copia_archivo = "\n cp "||v_ruta_envio_dac CLIPPED||"/"||v_v_nom_archivo CLIPPED|| " " ||v_ruta_envio_dac CLIPPED||"/"||v_nombre_destino
      RUN v_copia_archivo 

      DISPLAY "\n Copia Archivo para Cartera:", v_copia_archivo, "\n"

      --LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/DAE_1.sh"
      --RUN v_ejecuta_sh
--
      --DISPLAY "\n Se ejecutó la transaferencia a cartera"

      
      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # #",
                      "#    ",
                      "# El archivo se creo satisfactoriamente",
                      "#    ",
                      "# Ruta y nombre del archivo: ",v_v_ruta_nomarch,
                      "#    ",
                      "# Numero de solicitudes: "||v_total_solicitud,
                      "# Total de Aceptados   : "||v_total_aceptados,
                      "# Total de Rechazados  : "||v_total_rechazados,
                      "#    ",     
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   END IF
   
   CALL v_ch_arch_solTransf.close()
   
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - GENERA ARCHIVO"
   
END FUNCTION --fn_archivo_salida_MTC

#OBJETIVO: Generar el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio,p_busca_nom_archivo)
DEFINE p_ruta_envio     LIKE seg_modulo.ruta_envio,
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(20)
DEFINE ch base.Channel

    LET v_cmd = "ls -lrt ",p_ruta_envio CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

    LET ch = base.Channel.create()
    CALL ch.setDelimiter(".")
    CALL ch.openPipe(v_cmd,"r")
    WHILE ch.read([fn])
       LET v_consecutivo = fn[12,13]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo

END FUNCTION