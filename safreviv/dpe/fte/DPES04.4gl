--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 03/05/2012
--===============================================================

####################################################################
#Modulo            =>DPE                                           #
#Programa          =>DPES02                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo solo INFONAVIT                     #
#Fecha inicio      => 03/05/2012                                   #
####################################################################
GLOBALS "DPEG01.4gl"
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
       v_s_sql          STRING, -- cadena con una instruccion SQL
       v_i_resultado    INTEGER, -- resultado del proceso
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
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
   --AND
   -- estado_cod = 2 --
    
   ---- Inicio operacion.
   --CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"DPES02",
   --                              "",p_usuario_cod)
   --   RETURNING r_bnd_fin_oper
   --IF (r_bnd_fin_oper = 0) THEN
      -- Llamado a función que genera el archivo de salida
      CALL fn_archivo_salida_INFONAVIT(p_usuario_cod, p_folio)
      
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
           g_proceso_cod, --- Clave del proceso
           g_opera_cod) --- Clave de la operación
         RETURNING r_bnd_fin_oper
   --ELSE
   --   -- Error al iniciar operacioin
   --   DISPLAY "No se inició la operación."
   --   DISPLAY "r_bnd_fin_oper: ",r_bnd_fin_oper
   --END IF

END MAIN

{
======================================================================
Clave: 
Nombre: fn_archivo_salida_INFONAVIT
Fecha creacion: 03/05/2012
Narrativa del proceso que realiza:
Genera el archivo de salida para dpe solo INFONAVIT
======================================================================
}
FUNCTION fn_archivo_salida_INFONAVIT(p_usuario_cod, p_folio)
DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio        LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_titulo       STRING, -- titulo del mensaje enviado en el correo
       p_mensaje      STRING, -- cuerpo del mensaje enviado
       r_bnd_fin_oper SMALLINT,
       v_tot_solicitudes    SMALLINT,
       v_tot_aportacion_reg DECIMAL(11,2),
       v_tot_amortiza_reg   DECIMAL(11,2),
       v_tot_aportacion_sol DECIMAL(11,2),
       v_tot_amortiza_sol   DECIMAL(11,2),
       v_r_dpe_ini_trasaccion RECORD --Record almacena registro inicial transaccion
          v_tpo_registro DECIMAL(1,0),
          v_fec_registro CHAR(10),
          v_filler       CHAR(94)
       END RECORD,
       v_r_dpe_cza_transaccion RECORD --Record almacena encabezado de la transaccion
          v_tpo_registro           DECIMAL(1,0),
          v_transacciones          DECIMAL(4,0),
          v_fec_generacion_archivo CHAR(10),
          v_filler                 CHAR(90)
       END RECORD,
       v_r_dpe_det_transaccion RECORD --Record almacena detalle de la transaccion
          v_tpo_registro          DECIMAL(1,0),
          v_nss                   CHAR(11),
          v_credito               DECIMAL(10,0),
          v_periodo_pago          CHAR(6),
          v_fec_pago              CHAR(10),
          v_enti_recaudadora      DECIMAL(3,0),
          v_nrp                   CHAR(11),
          v_aportacion_registrada DECIMAL(11,2),
          v_amortiza_registrada   DECIMAL(11,2),
          v_aportacion_solicitada DECIMAL(11,2),
          v_amortiza_solicitada   DECIMAL(11,2),
          v_llave_sdd             DECIMAL(6,0),
          v_folio_sua             DECIMAL(6,0),
          v_cve_rechazo           DECIMAL(5,0)
       END RECORD,
       v_r_dpe_sum_transaccion RECORD --Record almacena sumario de la transaccion
          v_tpo_registro               DECIMAL(1,0),
          v_transacciones              DECIMAL(4,0),
          v_tot_registros_transaccion  DECIMAL(10,0),
          v_imp_tot_aporta_amortizar_1 DECIMAL(14,2),
          v_imp_tot_amortizacion_1     DECIMAL(14,2),
          v_imp_tot_aporta_amortizar_2 DECIMAL(14,2),
          v_imp_tot_amortizacion_2     DECIMAL(14,2),
          v_filler                     CHAR(40)
       END RECORD,
       v_r_dpe_fin_transaccion RECORD -- Record almacena registro final transaccion
          v_tpo_registro               DECIMAL(1,0),
          v_tot_reg_contenido_arch     DECIMAL(10,0),
          v_filler_1                   DECIMAL(4,0),
          v_imp_tot_aporta_amortizar_1 DECIMAL(14,2),
          v_imp_tot_amortizacion_1     DECIMAL(14,2),
          v_imp_tot_aporta_amortizar_2 DECIMAL(14,2),
          v_imp_tot_amortizacion_2     DECIMAL(14,2),
          v_filler_2                   CHAR(40)
       END RECORD,
       v_v_nom_archivo     CHAR(40), -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_dpe    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_sql                  VARCHAR(1000),-- Cadena que contiene las consultas a ejecutar
       cont_cza_solicitud       SMALLINT, -- Contador de encabezado de solicitudes
       v_s_tot_solicitudes      SMALLINT, -- Total de patrones solo registros 02
       v_s_tot_trabajadores     SMALLINT, -- Total de trabajadores solo registros 03
       v_s_tot_trabajadores_aux SMALLINT,-- Total de trabajadores solo registros 03
       v_s_tot_registros        SMALLINT, -- Total de registros solo registros <> 01 y 09
       v_s_registro             STRING, -- registro a insertar
       v_d_hoy                  CHAR(8),
       v_busca_archivo          STRING,
       v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia                SMALLINT

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DPES02.log")

-- se obtienen la ruta envio del modulo
    SELECT ruta_envio 
      INTO v_c_ruta_env_dpe
      FROM seg_modulo
     WHERE modulo_cod = 'dpe'
     
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "RES"||v_d_hoy
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_dpe,v_busca_archivo)
        RETURNING v_cont_dia
    
   LET v_reg_dia = v_cont_dia USING "&&"     
   LET v_v_nom_archivo = "/" ||v_busca_archivo||v_reg_dia||".dpeinf"
   LET v_v_ruta_nomarch = v_c_ruta_env_dpe CLIPPED || v_v_nom_archivo
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")
   
   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_cza_solicitud = 1
   
   -- Se llena el encabezado de la solicitud
      DECLARE cur_ini_transaccion CURSOR FOR
       SELECT fecha_registro
         FROM safre_viv:dpe_soloinfonavit_arch
        WHERE folio = p_folio
   
   -- Comienza el llenado de el registro inicial
   FOREACH cur_ini_transaccion 
      INTO v_r_dpe_ini_trasaccion.v_fec_registro
      
      -- Se llena la cadena para escribir en el archivo
      LET v_r_dpe_ini_trasaccion.v_tpo_registro = 0
      LET v_s_registro = v_r_dpe_ini_trasaccion.v_tpo_registro USING "&",
                         v_r_dpe_ini_trasaccion.v_fec_registro[7,10], -- Año fecha registro
                         v_r_dpe_ini_trasaccion.v_fec_registro[1,2], -- Dia fecha registro
                         v_r_dpe_ini_trasaccion.v_fec_registro[4,5], -- Mes fecha registro
                         v_r_dpe_ini_trasaccion.v_filler
      
      CALL v_ch_arch_solTransf.write([v_s_registro])
      
      LET cont_cza_solicitud = cont_cza_solicitud + 1
      
      --LET v_s_tot_solicitudes = 0 -- Se inicializa el contador de patrones
      
      -- Se llena el encabezado de la transaccion
        DECLARE cur_cza_transaccion CURSOR FOR 
         SELECT transacciones, fecha_archivo
           FROM safre_viv:dpe_soloinfonavit_arch
          WHERE folio = p_folio
      
      --LET v_s_tot_registros = 0
      
      -- Comienza el llenado del encabezado de la transaccion
      FOREACH cur_cza_transaccion 
         INTO v_r_dpe_cza_transaccion.v_transacciones,
              v_r_dpe_cza_transaccion.v_fec_generacion_archivo

         LET v_r_dpe_cza_transaccion.v_tpo_registro = 1
         LET v_s_registro = v_r_dpe_cza_transaccion.v_tpo_registro USING "&",
                            v_r_dpe_cza_transaccion.v_transacciones USING "&&&&",
                            v_r_dpe_cza_transaccion.v_fec_generacion_archivo[7,10],
                            v_r_dpe_cza_transaccion.v_fec_generacion_archivo[1,2],
                            v_r_dpe_cza_transaccion.v_fec_generacion_archivo[4,5],
                            v_r_dpe_cza_transaccion.v_filler
         
            CALL v_ch_arch_solTransf.write([v_s_registro])
            
            --LET v_s_tot_registros = v_s_tot_registros + 1
         
         -- Se llena el detalle de la transaccion
         DECLARE cur_det_transaccion CURSOR FOR 
            SELECT nss, credito, periodo_pago, fecha_pago,
                   ent_recaudadora, nrp, aportacion_reg,
                   amortizacion_reg, aportacion_sol, amortizacion_sol,
                   llave_sdd, folio_sua, clave_rechazo
              FROM safre_viv:dpe_sol_soloinfonavit
             WHERE folio = p_folio
         
         LET v_s_tot_trabajadores = 0
         
         FOREACH cur_det_transaccion 
            INTO v_r_dpe_det_transaccion.v_nss,
                 v_r_dpe_det_transaccion.v_credito,
                 v_r_dpe_det_transaccion.v_periodo_pago,
                 v_r_dpe_det_transaccion.v_fec_pago,
                 v_r_dpe_det_transaccion.v_enti_recaudadora,
                 v_r_dpe_det_transaccion.v_nrp,
                 v_r_dpe_det_transaccion.v_aportacion_registrada,
                 v_r_dpe_det_transaccion.v_amortiza_registrada,
                 v_r_dpe_det_transaccion.v_aportacion_solicitada,
                 v_r_dpe_det_transaccion.v_amortiza_solicitada,
                 v_r_dpe_det_transaccion.v_llave_sdd,
                 v_r_dpe_det_transaccion.v_folio_sua,
                 v_r_dpe_det_transaccion.v_cve_rechazo
            
            LET v_r_dpe_det_transaccion.v_tpo_registro = 2
            LET v_s_registro = v_r_dpe_det_transaccion.v_tpo_registro USING "&",
                               v_r_dpe_det_transaccion.v_nss,
                               v_r_dpe_det_transaccion.v_credito USING "&&&&&&&&&&",
                               v_r_dpe_det_transaccion.v_periodo_pago,
                               v_r_dpe_det_transaccion.v_fec_pago[7,10],
                               v_r_dpe_det_transaccion.v_fec_pago[1,2],
                               v_r_dpe_det_transaccion.v_fec_pago[4,5],
                               v_r_dpe_det_transaccion.v_enti_recaudadora USING "&&&",
                               v_r_dpe_det_transaccion.v_nrp,
                               v_r_dpe_det_transaccion.v_aportacion_registrada * 100 USING "&&&&&&&&&",
                               v_r_dpe_det_transaccion.v_amortiza_registrada * 100 USING "&&&&&&&&&",
                               v_r_dpe_det_transaccion.v_aportacion_solicitada * 100 USING "&&&&&&&&&",
                               v_r_dpe_det_transaccion.v_amortiza_solicitada * 100 USING "&&&&&&&&&",
                               v_r_dpe_det_transaccion.v_llave_sdd USING "&&&&&&",
                               v_r_dpe_det_transaccion.v_folio_sua USING "&&&&&&",
                               v_r_dpe_det_transaccion.v_cve_rechazo USING "&&&&&"
                               
            LET v_s_tot_trabajadores = v_s_tot_trabajadores + 1

            --LET v_s_tot_registros = v_s_tot_registros + 1
            
            CALL v_ch_arch_solTransf.write([v_s_registro])

         END FOREACH -- Detalle transaccion
         
         --LET v_s_tot_trabajadores_aux = v_s_tot_trabajadores_aux + v_s_tot_trabajadores
         
            -- Se llena el sumario de la transaccion
            DECLARE cur_sum_transaccion CURSOR FOR 
               SELECT SUM(tot_aportacion_reg), SUM(tot_amortizacion_reg),
                      SUM(tot_aportacion_sol), SUM(tot_amortizacion_sol)
                 FROM safre_viv:dpe_soloinfonavit_arch
                WHERE folio = p_folio
            
            FOREACH cur_sum_transaccion 
               INTO v_r_dpe_sum_transaccion.v_imp_tot_aporta_amortizar_1,
                    v_r_dpe_sum_transaccion.v_imp_tot_amortizacion_1,
                    v_r_dpe_sum_transaccion.v_imp_tot_aporta_amortizar_2,
                    v_r_dpe_sum_transaccion.v_imp_tot_amortizacion_2,
                    v_r_dpe_sum_transaccion.v_filler
               
               LET v_r_dpe_sum_transaccion.v_tpo_registro = 3
               LET v_r_dpe_sum_transaccion.v_transacciones = v_r_dpe_cza_transaccion.v_transacciones
               LET v_r_dpe_sum_transaccion.v_tot_registros_transaccion = v_s_tot_trabajadores + 2
               LET v_s_registro = v_r_dpe_sum_transaccion.v_tpo_registro USING "&",
                                  v_r_dpe_sum_transaccion.v_transacciones USING "&&&&",
                                  v_r_dpe_sum_transaccion.v_tot_registros_transaccion USING "&&&&&&&&&&",
                                  v_r_dpe_sum_transaccion.v_imp_tot_aporta_amortizar_1 * 100 USING "&&&&&&&&&&&&",
                                  v_r_dpe_sum_transaccion.v_imp_tot_amortizacion_1 * 100 USING "&&&&&&&&&&&&",
                                  v_r_dpe_sum_transaccion.v_imp_tot_aporta_amortizar_2 * 100 USING "&&&&&&&&&&&&",
                                  v_r_dpe_sum_transaccion.v_imp_tot_amortizacion_2 * 100 USING "&&&&&&&&&&&&",
                                  v_r_dpe_sum_transaccion.v_filler
                                  
               CALL v_ch_arch_solTransf.write([v_s_registro])
            
            END FOREACH -- Sumario transaccion
         
         --LET v_s_tot_solicitudes = v_s_tot_solicitudes + 1
         --LET v_s_tot_registros = v_s_tot_registros + 1
         
      END FOREACH -- Encabezado transaccion
      
        -- Se llena el sumario de la solicitud en exceso
        DECLARE cur_fin_transaccion CURSOR FOR 
           SELECT SUM(tot_aportacion_reg), SUM(tot_amortizacion_reg),
                  SUM(tot_aportacion_sol), SUM(tot_amortizacion_sol)
             FROM safre_viv:dpe_soloinfonavit_arch
            WHERE folio = p_folio

        FOREACH cur_fin_transaccion 
           INTO v_r_dpe_fin_transaccion.v_imp_tot_aporta_amortizar_1,
                v_r_dpe_fin_transaccion.v_imp_tot_amortizacion_1,
                v_r_dpe_fin_transaccion.v_imp_tot_aporta_amortizar_2,
                v_r_dpe_fin_transaccion.v_imp_tot_amortizacion_2,
                v_r_dpe_fin_transaccion.v_filler_2
              
              LET v_r_dpe_fin_transaccion.v_tpo_registro = 4
              LET v_r_dpe_fin_transaccion.v_tot_reg_contenido_arch = 
                  v_r_dpe_sum_transaccion.v_tot_registros_transaccion + 4
              LET v_r_dpe_fin_transaccion.v_filler_1 = 0
              LET v_s_registro = v_r_dpe_fin_transaccion.v_tpo_registro USING "&",
                                 v_r_dpe_fin_transaccion.v_tot_reg_contenido_arch USING "&&&&&&&&&&",
                                 v_r_dpe_fin_transaccion.v_filler_1 USING "&&&&",
                                 v_r_dpe_fin_transaccion.v_imp_tot_aporta_amortizar_1 * 100 USING "&&&&&&&&&&&&",
                                 v_r_dpe_fin_transaccion.v_imp_tot_amortizacion_1 * 100 USING "&&&&&&&&&&&&",
                                 v_r_dpe_fin_transaccion.v_imp_tot_aporta_amortizar_2 * 100 USING "&&&&&&&&&&&&",
                                 v_r_dpe_fin_transaccion.v_imp_tot_amortizacion_2 * 100 USING "&&&&&&&&&&&&",
                                 v_r_dpe_fin_transaccion.v_filler_2              
           
              CALL v_ch_arch_solTransf.write([v_s_registro])
           
        END FOREACH -- fin transaccion
      
   END FOREACH -- registro inicial transaccion
   
   LET v_s_sql = "SELECT COUNT(*),",
                 "\n SUM(d.aportacion_reg),",
                 "\n SUM(d.amortizacion_reg),",
                 "\n SUM(d.aportacion_sol),",
                 "\n SUM(d.amortizacion_sol)",
                 "\n FROM dpe_sol_soloinfonavit d",
                 "\n WHERE d.folio = ", p_folio
   
   PREPARE Prpr_ObtResumen_archivo FROM v_s_sql CLIPPED
   EXECUTE Prpr_ObtResumen_archivo INTO v_tot_solicitudes,
                                        v_tot_aportacion_reg,
                                        v_tot_amortiza_reg,
                                        v_tot_aportacion_sol,
                                        v_tot_amortiza_sol              
   
             
   IF cont_cza_solicitud = 1 THEN
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
      DISPLAY "El archivo se creo satisfactoriamente"
      DISPLAY "#    "
      DISPLAY "Ruta y nombre del archivo: ",v_v_ruta_nomarch
      DISPLAY "#    "
      DISPLAY "Numero de solicitudes: "||v_tot_solicitudes
      DISPLAY "Aportación registrada: "||v_tot_aportacion_reg
      DISPLAY "Amortización registrada: "||v_tot_amortiza_reg
      DISPLAY "Aportación solicitada: "||v_tot_aportacion_sol
      DISPLAY "Amortización solicitada: "||v_tot_amortiza_sol
      DISPLAY "#    "
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      
      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                      "#   \n ",
                      "El archivo se creo satisfactoriamente \n",
                      "#  \n",
                      "Ruta y nombre del archivo: ",v_v_ruta_nomarch,"\n",
                      "#   \n",
                      "Numero de solicitudes: "||v_tot_solicitudes," \n",
                      "Aportación registrada: "||v_tot_aportacion_reg," \n",
                      "Amortización registrada: "||v_tot_amortiza_reg," \n",
                      "Aportación solicitada: "||v_tot_aportacion_sol," \n",
                      "Amortización solicitada: "||v_tot_amortiza_sol," \n",
                      "#  \n",
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      
      UPDATE safre_viv:dpe_sol_soloinfonavit
         SET estado_solicitud = 5
       WHERE folio = p_folio
         AND estado_solicitud = 4
   END IF
   
   CALL v_ch_arch_solTransf.close()
   
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - GENERA ARCHIVO"
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   
END FUNCTION --fn_archivo_salida_INFONAVIT

#Objetivo: genera el número consecutivo por día para el archivo de salida
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