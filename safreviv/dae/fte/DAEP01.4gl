################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEP01                                                   #
#Objetivo          => Programa que ejecuta el stored procedure que realiza la  #
#                     integracion de Devolución de Amortizaciones Excedentes   #
#Fecha inicio      => 19/04/2012                                               #
################################################################################

--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 25/02/2015 --AG 
--==============================================================================
--LANZADOR: DAEL02

DATABASE safre_viv 

GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                      LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod ,-- codigo del proceso
       p_opera_cod                LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod              LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_monto_valida             DECIMAL(16,6), --Monto validación para integrar
       v_s_sql                    STRING ,-- cadena con una instruccion SQL
       v_i_resultado              INTEGER ,-- resultado del proceso
       r_bnd_fin_oper             SMALLINT,
       v_si_correcto_integra      SMALLINT,
       p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo,-- nombre dle archivo
       v_si_solicitudes_totales   INTEGER,
       v_msj_sql                  CHAR(200),
       v_folio                    LIKE deo_preliquida.folio_liquida,
       v_si_status_detalle_trabaj SMALLINT,
       p_titulo                   STRING ,-- titulo del mensaje enviado en el correo
       p_mensaje                  STRING ,-- cuerpo del mensaje enviado
       v_layout                   LIKE cat_operacion.layout_cod,
       v_ruta_rescate             STRING,
       v_usuario                  LIKE seg_modulo.usuario,
       v_proceso_desc             LIKE cat_proceso.proceso_desc,
       v_extension                LIKE cat_operacion.extension,
       v_opera_desc               LIKE cat_operacion.opera_desc,
       v_ruta_listados            LIKE seg_modulo.ruta_listados,
       v_total_rechazados         INTEGER,
       v_total_aceptadas          INTEGER,
       v_total_pendientes         INTEGER,
       r_bnd_ini_oper             SMALLINT,
       v_isam_error               INTEGER,
       v_tot_rch_ff               INTEGER,
       r_ruta_nomarch             STRING

   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_monto_valida   = ARG_VAL(7)
   
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
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion
   
   WHENEVER ERROR CONTINUE

   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
        RETURNING v_folio

   DISPLAY "#   FOLIO              : ",v_folio
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_dae_integra_det(?, ?, ?, ?, ?, ?) "

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integra_dae FROM v_s_sql
   EXECUTE sid_integra_dae USING p_usuario_cod,
                                 g_pid,
                                 p_nombre_archivo, 
                                 v_folio,
                                 g_proceso_cod, 
                                 p_monto_valida
                            INTO v_i_resultado, 
                                 v_isam_error,
                                 v_msj_sql, 
                                 v_si_correcto_integra,
                                 v_si_solicitudes_totales,
                                 v_total_rechazados,
                                 v_total_aceptadas,
                                 v_total_pendientes,
                                 v_tot_rch_ff
   CASE
      WHEN (v_i_resultado = 0)
         DISPLAY "#   La integración se terminó completamente."
         DISPLAY "#"
         DISPLAY "#   RESULTADO INTEGRACION : ",v_i_resultado 
         DISPLAY "#   ISAM                  : ",v_isam_error
         DISPLAY "#   MENSAJE               : ",v_msj_sql  
         DISPLAY "#"
         DISPLAY "#   Integración realizada con exito"
         DISPLAY "#"
         DISPLAY "#   Total de aceptadas   : ",v_total_aceptadas
         DISPLAY "#   Total de rechazadas  : ",v_total_rechazados
         DISPLAY "#   Total de pendientes  : ",v_total_pendientes
         DISPLAY "#   Total de erroneas    : ",v_tot_rch_ff
         DISPLAY "#   Total de solicitudes : ",v_si_solicitudes_totales
         
         IF v_tot_rch_ff THEN
            CALL fn_genera_archivo_rch_fecha(v_folio)
                 RETURNING r_ruta_nomarch
            DISPLAY "#"
            DISPLAY "#   Se generó un archivo de registros con error en formato de fecha " 
            DISPLAY "#   Dentro de la siguiente ruta : "
            DISPLAY "#   ", r_ruta_nomarch
            DISPLAY "#"

            CALL fn_consulta_erroneos(v_folio)
         END IF
         
         IF ( v_si_correcto_integra = 0 AND v_si_solicitudes_totales > 0 ) THEN
            CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                RETURNING r_bnd_fin_oper

             --Si no hay registros ACEPTADOS finaliza la operación  
             IF v_total_aceptadas = 0 THEN
                CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,3,0,"DAEL03","",p_usuario_cod)
                RETURNING r_bnd_ini_oper
                CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,3)
                RETURNING r_bnd_fin_oper
                CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,4,0,"DAEL04","",p_usuario_cod)
                RETURNING r_bnd_ini_oper
                CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,4)
                RETURNING r_bnd_fin_oper
             ELSE             
                DISPLAY "#  Ya se puede Continuar con la Preliquidación"
             END IF
             -- Genera cifras control por registro de patron.
             CALL fn_obtiene_cifras_control(v_folio)
             
          ELSE
             CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                RETURNING r_bnd_fin_oper
                LET p_mensaje = " --- ERROR ---\n",
                          " El proceso de integración no terminó correctamente.\n",
                          " Código de error : ", r_bnd_fin_oper,"\n ",
                          " FECHA           : ",TODAY,"\n",
                          " HORA            : ",CURRENT HOUR TO SECOND,"\n"
             DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
             IF(v_i_resultado <> 0)THEN
                DISPLAY "#  Error. No se integró ninguna solicitud"
             END IF
             
             LET p_mensaje = " --- ERROR ---\n",
                          " El proceso de integración no terminó correctamente.\n",
                          " Código de error : ", r_bnd_fin_oper,"\n ",
                          " FECHA           : ",TODAY,"\n",
                          " HORA            : ",CURRENT HOUR TO SECOND,"\n"           
          END IF
    
       WHEN (v_i_resultado = NOTFOUND)
          DISPLAY "NOT FOUND"
          CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
             RETURNING r_bnd_fin_oper
          DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
          DISPLAY "#  Error. No se integró ninguna solicitud"
          LET p_mensaje = " --- ERROR ---\n",
                          " El proceso de integración no terminó correctamente.\n",
                          " Código de error : ", r_bnd_fin_oper,"\n ",
                          " FECHA           : ",TODAY,"\n",
                          " HORA            : ",CURRENT HOUR TO SECOND,"\n"
       WHEN (v_i_resultado < 0)
          DISPLAY SQLERRMESSAGE
          CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
             RETURNING r_bnd_fin_oper
          DISPLAY "Codigo Error SQL :",v_i_resultado
          DISPLAY "ISAM             : ",v_isam_error
          DISPLAY "Mensaje          : ",v_msj_sql  
          DISPLAY " "
          DISPLAY "Error al procesar la integración"
          DISPLAY "No se puede continuar..."
          LET p_mensaje = " --- ERROR ---\n",
                          " El proceso de integración no terminó correctamente.\n",
                          " Código de error : ", r_bnd_fin_oper,"\n ",
                          " FECHA           : ",TODAY,"\n",
                          " HORA            : ",CURRENT HOUR TO SECOND,"\n"
    END CASE

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
      
   
   WHENEVER ERROR STOP
END MAIN

# OBJETIVO: Obtener la descripción del error de la validacion y la muestra en mensaje para suario.
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera
   -- Muestra el mensaje encontrado
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

#OBJETIVO: Obtener cifras control para monitor de procesos
FUNCTION fn_obtiene_cifras_control(p_folio)
DEFINE p_folio  LIKE deo_preliquida.folio_liquida,
       v_total_solicitudes      INTEGER,          
       v_suma_importes_amort    DECIMAL(18,6),
       v_suma_aceptados_amort   DECIMAL(18,6),      
       v_total_aceptados        INTEGER,
       v_suma_rechazados_amort  DECIMAL(18,6),      
       v_total_rechazados       INTEGER,
       v_suma_pendientes_amort  DECIMAL(18,6),      
       v_total_pendientes       INTEGER,
       v_s_qry                  STRING

    LET v_s_qry = "\n SELECT SUM(importe_amort),",  
                  "\n        COUNT(nss)",
                  "\n FROM   dae_det_solicitud",
                  "\n WHERE  folio = ", p_folio

   PREPARE prp_cifras_glob FROM v_s_qry CLIPPED
   DECLARE cur_cifras_glob CURSOR FOR prp_cifras_glob 
   DISPLAY "#"
   DISPLAY "#  -    CIFRAS GLOBALES    -"
   DISPLAY "#"
   DISPLAY "#  Folio :",p_folio
   DISPLAY "#"
   
   FOREACH cur_cifras_glob INTO v_suma_importes_amort, 
                                v_total_solicitudes

       SELECT SUM(importe_amort),
              COUNT(nss)
       INTO   v_suma_aceptados_amort, 
              v_total_aceptados
       FROM   dae_det_solicitud
       WHERE  folio = p_folio
       AND    resul_opera = "01";
       
       SELECT SUM(importe_amort),
              COUNT(nss)
       INTO   v_suma_rechazados_amort, 
              v_total_rechazados
       FROM   dae_det_solicitud
       WHERE  folio = p_folio
       AND    resul_opera = "02";

       SELECT SUM(importe_amort),
              COUNT(nss)
       INTO   v_suma_pendientes_amort,
              v_total_pendientes
       FROM   dae_det_solicitud
       WHERE  folio = p_folio
       AND    resul_opera = "03";

      DISPLAY "#  Total importe amortización Aceptadas: ",v_suma_aceptados_amort   
      DISPLAY "#  Total solicitudes Aceptadas         : ",v_total_aceptados
      DISPLAY "#"                                       
      DISPLAY "#  Total importe amortización Rechazadas: ",v_suma_rechazados_amort  
      DISPLAY "#  Total solicitudes Rechazadas         : ",v_total_rechazados
      DISPLAY "#"
      DISPLAY "#  Total importe amortización Pendientes: ",v_suma_pendientes_amort  
      DISPLAY "#  Total solicitudes Pendientes         : ",v_total_pendientes
      DISPLAY "#"
      DISPLAY "#  Total importe total Amortización : ",v_suma_importes_amort
      DISPLAY "#  Total solicitudes                : ", v_total_solicitudes      
   END FOREACH

   FREE cur_cifras_glob 

END FUNCTION -- fn_obtiene_cifras_control


#OBJETIVO: Generar un archivo de salida que contenga los registros rechazados 
#          por error en formato de fecha
FUNCTION fn_genera_archivo_rch_fecha(p_folio)
DEFINE p_folio DECIMAL (9,0),
       v_referencia DECIMAL (9,0),
       v_i           INTEGER,
       v_v_nom_archivo       CHAR(40), -- nombre del archivo de salida
       v_nom_archivo_axway   CHAR(40), -- nombre para transferenia de AXWAY
       v_v_ruta_nomarch      VARCHAR(100), -- ruta y nombre del archivo de salida
       v_ruta_envio          LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf   BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_registro             STRING, -- registro a insertar
       v_d_hoy                  CHAR(8),
       v_busca_archivo          STRING,
       v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia                SMALLINT,
       v_campo_valor            CHAR(70)
       
   LET v_cont_dia = 1


   SELECT ruta_envio 
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = 'dae'
     
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta  
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "RDAE"||v_d_hoy||"_"

   --Obtine consecutivo para archivo por día
   CALL fn_consulta_consecutivo(v_ruta_envio,v_busca_archivo)
        RETURNING v_cont_dia
   
   LET v_reg_dia = v_cont_dia USING "&"
   LET v_v_nom_archivo = "/"||v_busca_archivo||v_reg_dia||".rdae"
   LET v_v_ruta_nomarch = v_ruta_envio CLIPPED || v_v_nom_archivo

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )

   
    LET v_i = 1   

    DECLARE cur_rch_ff CURSOR FOR SELECT campo_valor
                                  FROM   dae_rch_archivo 
                                  WHERE  folio       = p_folio
                                  AND    diagnostico = 3 
    FOREACH cur_rch_ff INTO v_campo_valor
       LET v_s_registro = v_campo_valor
       CALL v_ch_arch_solTransf.writeline(v_s_registro)
       LET v_i = v_i + 1
    END FOREACH

    RETURN v_v_ruta_nomarch
END FUNCTION

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_consulta_consecutivo(p_ruta_envio,p_busca_nom_archivo)
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

FUNCTION fn_consulta_erroneos(p_folio)
DEFINE p_folio DECIMAL(9,0),
       v_err   SMALLINT
DEFINE rec_detalles DYNAMIC ARRAY OF RECORD 
       v_nss         CHAR(11),
       v_campo_valor CHAR(70),
       v_diagnostico SMALLINT
END RECORD 

   DECLARE cur_erroneos CURSOR FOR SELECT a.nss, 
                                          b.campo_valor,
                                          b.diagnostico      
                                   FROM   dae_det_solicitud a, 
                                          dae_rch_archivo b
                                   WHERE  a.folio = b.folio
                                   AND    a.id_dae_referencia = b.id_dae_referencia
                                   AND    b.folio = p_folio
                                   AND    b.resul_opera = "04"
                                   AND    b.diagnostico IN (3,4)

   LET v_err = 1 ;
   DISPLAY "# " 
   DISPLAY "#  -   Detalles de registros con error en fecha #"
   DISPLAY "# NSS         |   Valor error                   #"

   FOREACH cur_erroneos INTO rec_detalles[v_err].*   
      IF rec_detalles[v_err].v_diagnostico = 3 THEN -- Error en fecha_pago
         DISPLAY "# ", rec_detalles[v_err].v_nss, " - Fecha pago   :  ", rec_detalles[v_err].v_campo_valor[13,20]
      ELSE -- Error en registro_pago
         DISPLAY "# ", rec_detalles[v_err].v_nss, " - Registro pago:  ", rec_detalles[v_err].v_campo_valor[21,29]
      END IF

      LET v_err = v_err + 1;
   END FOREACH 
END FUNCTION 