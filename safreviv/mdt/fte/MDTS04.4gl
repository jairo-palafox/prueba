--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
--===============================================================

####################################################################
#Modulo            =>MDTS                                          #
#Programa          =>MDTS04                                        #
#Objetivo          =>Programa que genera el archivo de salida de   # 
#                    mandatos                                      #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>14 FEBRERO 2012                               #
####################################################################
DATABASE safre_viv

MAIN
   DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid              LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          --p_f_lote             LIKE mdt_solicitud_mandato.f_lote, -- numero de folio
          p_folio              LIKE mdt_solicitud_mandato.folio, -- numero de folio
          p_v_arch_proceso     VARCHAR(100), -- nombre del archivo a integrar
          v_v_nom_archivo      VARCHAR(80), -- nombre del archivo de salida
          v_ch_arch_solTransf  BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_folio_liquidacion  LIKE cta_movimiento.folio_liquida,
          v_f_liquida          LIKE cta_movimiento.f_liquida, -- fecha de liquidacion
          v_r_detalle          RECORD
             --CAMPO	LONG.	POSC. DE	POSC. A	TIPO TRANSACCION	REQUERIDO	OBSERVACIONES
             tipo_registro       CHAR(2) , -- 2   1   a 2   
             nss                 CHAR(11), -- 11  3   a 13
             num_credito         CHAR(10), -- 10  14  a 23
             consecutivo         CHAR(3) , -- 3   24  a 26
             tipo_operacion      CHAR(3) , -- 3   27  a 29 
             f_inicio_mdt        CHAR(8) , -- 8   30  a 37
             f_culmina_mdt       CHAR(8) , -- 8   38  a 45
             id_mandato          CHAR(7) , -- 7   46  a 52             
             tipo_descuento      CHAR(1) , -- 1   53  a 53
             valor_descuento     CHAR(8) , -- 8   54  a 61
             referencia          CHAR(40), -- 40  62  a 101
             cve_mandato         CHAR(18), -- 18 102  a 119
             resultado_operacion CHAR(2) , -- 2  120  a 121
             diagnostico         CHAR(3)   -- 3  122  a 124
          END RECORD,
          v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, -- ID del derechohabiente
          v_pesos              LIKE cta_movimiento.monto_pesos, -- monto en pesos
          v_deudor_txt         CHAR(13), -- para obtener cifras del saldo
          v_nss                LIKE afi_derechohabiente.nss, -- NSS del derechohabiente
          --v_num_credito        LIKE acr_transferencia.num_credito, -- numero del credito
          v_s_registro         STRING, -- registro a insertar
          v_c_ruta_env_mdt     LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_i_contrador_reg    INTEGER, -- contrador de registros
          v_c_programa_cod     LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_r_mandato          RECORD LIKE mdt_solicitud_mandato.* 

   DEFINE v_i_tot_reg             INTEGER
   DEFINE v_diagnostico           CHAR(3)
   DEFINE v_tipo_operacion        CHAR(1)
   DEFINE v_c_valor_desc          VARCHAR(15)
   DEFINE v_c_valor_desc1         VARCHAR(15)
   DEFINE v_c_valor_desc2         VARCHAR(15)
   DEFINE v_i_valor               INTEGER -- valor entero para la salida a archivo
   DEFINE v_i_valor_desc          INTEGER -- valor decimal para la salida a archivo
   DEFINE v_f_lote                CHAR(9)
   DEFINE p_fec_ejecucion         DATE
   DEFINE v_f_proceso LIKE mdt_lote_mandato.f_proceso
   
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario = ARG_VAL(1)
   LET p_d_pid = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod = ARG_VAL(4)
   LET p_folio = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)
SLEEP 5
   -- se crear el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".MDTS04.log")
   
   DISPLAY "Iniciando Generación de Archivo"
   DISPLAY "Fecha de Proceso ",today

   LET p_fec_ejecucion = DATE
   
   LET v_s_qryTxt = " SELECT diagnostico, tipo_operacion, NVL(count(*),0) ",
                    "   FROM mdt_solicitud_mandato ",
                    "  WHERE estado = 106 ",
                    "    AND id_origen = 1",
                    "    and folio = ?",
                    "    AND tipo_operacion IN ('A','B','M')",
                    "  GROUP BY 1,2 ORDER BY 1,2"
                    
   PREPARE EnuTotReg FROM v_s_qryTxt
   DECLARE CurTotReg CURSOR FOR EnuTotReg
   
   FOREACH CurTotReg USING p_folio INTO v_diagnostico, v_tipo_operacion, v_i_tot_reg
      
      CASE v_tipo_operacion
         WHEN 'A'
            DISPLAY "DIAGNOSTICO: ",v_diagnostico , " TOTAL ALTAS: ",v_i_tot_reg
         WHEN 'B'
            DISPLAY "DIAGNOSTICO: ",v_diagnostico , " TOTAL BAJAS: ",v_i_tot_reg
         WHEN 'M'
            DISPLAY "DIAGNOSTICO: ",v_diagnostico , " TOTAL MODIFICACIONES: ",v_i_tot_reg
      END CASE
      
   END FOREACH

   -- asigna el folio en la variable de folio liquidación
   --LET v_folio_liquidacion = p_d_pid
   
   LET v_c_programa_cod = "MDTS04"

   {SELECT MIN (a.f_lote)
   INTO   p_f_lote 
   FROM   mdt_solicitud_mandato a 
   WHERE  a.estado = 106}
   
   UPDATE bat_ctr_operacion 
      SET nom_archivo = p_v_arch_proceso ,
          folio  = p_folio 
    WHERE pid = p_d_pid 
      AND proceso_cod = p_i_proceso_cod 
      AND opera_cod   = p_i_opera_cod   
   
   -- se invoca la función que deja la operación en estado Procesando
--   LET r_b_valida = fn_actualiza_opera_ini(p_d_pid, p_i_proceso_cod, p_i_opera_cod,
--                                            p_d_pid, v_c_programa_cod,
--                                            p_v_arch_proceso, p_v_usuario)

   -- se verifica si fue posible inicializar la operacion
   --IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      --CALL fn_muestra_inc_operacion(r_b_valida)
      --DISPLAY "ERROR en fn_actualiza_opera_ini"
      --EXIT PROGRAM
   --END IF

   -- se obtienen la ruta envio del modulo
   LET v_s_qryTxt = " SELECT ruta_envio\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'mdt'"

   SELECT f_proceso
     INTO v_f_proceso
     FROM mdt_lote_mandato
    WHERE folio = p_folio

   PREPARE prp_slc_rutasAcr1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutasAcr1 INTO v_c_ruta_env_mdt

   -- se crea el nombre del archivo
   LET v_f_lote = year(v_f_proceso) 
   LET v_f_lote = v_f_lote CLIPPED, month(v_f_proceso) USING "&&" 
   LET v_f_lote = v_f_lote CLIPPED, day(v_f_proceso) USING "&&" 
   LET v_v_nom_archivo = v_c_ruta_env_mdt CLIPPED || "/S" || p_folio ||"_rch_recurrente.mdt"

   -- se inicializa el contador de registros
   LET v_i_contrador_reg = 0

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   DISPLAY "Archivo generado: ", v_v_nom_archivo

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_nom_archivo, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("") 

   -- se consultan los datos que componen el cuerpo del archivo de salida
   -- del archivo de liquidacion de deudor
   LET v_s_qryTxt = " SELECT * ",
                    "   FROM mdt_solicitud_mandato ",
                    "  WHERE estado = 106",
                    "    AND id_origen = 1",
                    "    and folio = ?",
                    "    AND tipo_operacion IN ('A','B','M')"
   --DISPLAY v_s_qryTxt
                    
   PREPARE sid_mandato       FROM v_s_qryTxt
   DECLARE cur_mandato       CURSOR FOR sid_mandato
   
   FOREACH cur_mandato USING p_folio INTO v_r_mandato.*
      -- se incrementa el contador de registro
      LET v_i_contrador_reg = v_i_contrador_reg + 1

      -- se asignan los valores en el registro detalle

      LET v_r_detalle.tipo_registro       = "30"  -- Constante
      LET v_r_detalle.nss                 = v_r_mandato.nss
      LET v_r_detalle.num_credito         = v_r_mandato.id_credito USING"&&&&&&&&&&"
      LET v_r_detalle.consecutivo         = "   "    --v_r_mandato.id_solicitud_mandato USING "&&&&&"
      CASE v_r_mandato.tipo_operacion
      WHEN "A" 
           LET v_r_detalle.tipo_operacion = "015"
           EXIT CASE           
      WHEN "B" 
           LET v_r_detalle.tipo_operacion = "016"
           EXIT CASE           
      WHEN "M" 
           LET v_r_detalle.tipo_operacion = "017"
           EXIT CASE           
      END CASE           
      LET v_r_detalle.f_inicio_mdt        = v_r_mandato.f_inicio_mandato USING "yyyymmdd"
      LET v_r_detalle.f_culmina_mdt       = v_r_mandato.f_culmina_mandato USING "yyyymmdd"
      LET v_r_detalle.id_mandato          = "       "
      LET v_r_detalle.tipo_descuento      = v_r_mandato.tpo_descuento_mandato
      LET v_r_detalle.valor_descuento     = v_r_mandato.valor_descuento_mandato * 100 USING"&&&&&&&&"      
     -- LET v_c_valor_desc                  = v_r_mandato.valor_descuento_mandato
      --LET v_c_valor_desc1                 = v_c_valor_desc[1,length(v_c_valor_desc CLIPPED)-3]
      --LET v_c_valor_desc2                 = v_c_valor_desc[length(v_c_valor_desc CLIPPED)-1,length(v_c_valor_desc CLIPPED)]
      --LET v_c_valor_desc                  = v_c_valor_desc1 USING "&&&&&&&&&"||v_c_valor_desc2||"0000"
      --LET v_r_detalle.valor_descuento     = v_c_valor_desc
      LET v_r_detalle.referencia          = v_r_mandato.referencia
      LET v_r_detalle.cve_mandato         = v_r_mandato.cve_mandato
      LET v_r_detalle.resultado_operacion = '02'
      LET v_r_detalle.diagnostico         = v_r_mandato.diagnostico

      -- se concatenan los campos a insertar
      LET v_s_registro = v_r_detalle.tipo_registro      ,
                         v_r_detalle.nss                ,
                         v_r_detalle.num_credito        ,
                         v_r_detalle.consecutivo        ,
                         v_r_detalle.tipo_operacion     ,
                         v_r_detalle.f_inicio_mdt       ,
                         v_r_detalle.f_culmina_mdt      ,
                         v_r_detalle.id_mandato         ,                         
                         v_r_detalle.tipo_descuento     ,
                         v_r_detalle.valor_descuento    ,
                         v_r_detalle.referencia         ,
                         v_r_detalle.cve_mandato        ,    
                         v_r_detalle.resultado_operacion,
                         v_r_detalle.diagnostico        
                         
      DISPLAY v_s_registro

      -- se escribe el registro (detalle) en el archivo
      CALL v_ch_arch_solTransf.write([v_s_registro])

      -- Actualiza
      UPDATE mdt_solicitud_mandato
         SET estado = 107
       WHERE folio = p_folio
       AND   id_solicitud_mandato = v_r_mandato.id_solicitud_mandato

   END FOREACH

   UPDATE mdt_lote_mandato
      SET estado = 103
    WHERE folio = p_folio

   LET v_s_qryTxt = " SELECT diagnostico, tipo_operacion, NVL(count(*),0) ",
                    "   FROM mdt_solicitud_mandato ",
                    "  WHERE estado = 107 ",
                    "    AND id_origen = 1",
                    "    and folio = ?",
                    "    AND tipo_operacion IN ('A','B','M')",
                    "  GROUP BY 1,2 ORDER BY 1,2"
                    
   PREPARE EnuTotRegInc FROM v_s_qryTxt
   DECLARE CurTotRegInc CURSOR FOR EnuTotRegInc
   
   FOREACH CurTotRegInc USING p_folio INTO v_diagnostico, v_tipo_operacion, v_i_tot_reg
      
      CASE v_tipo_operacion
         WHEN 'A'
            IF(v_diagnostico <> "000")THEN            
               DISPLAY "DIAGNOSTICO: ",v_diagnostico , " TOTAL ALTAS INCLUIDAS: ",v_i_tot_reg
            END IF
         WHEN 'B'
            IF(v_diagnostico <> "000")THEN            
              DISPLAY "DIAGNOSTICO: ",v_diagnostico , " TOTAL BAJAS INCLUIDAS: ",v_i_tot_reg
            END IF
         WHEN 'M'
            IF(v_diagnostico <> "000")THEN            
               DISPLAY "DIAGNOSTICO: ",v_diagnostico , " TOTAL MODIFICACIONES INCLUIDAS: ",v_i_tot_reg
            END IF
      END CASE
      
   END FOREACH
   DISPLAY "ARCHIVO GENERADO EN: ",v_c_ruta_env_mdt

   -- se cierra el manejador de lectura
   CALL v_ch_arch_solTransf.close()

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      DISPLAY "ERROR en fn_actualiza_opera_fin"
      --EXIT PROGRAM
   ELSE
      -- Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(p_d_pid, 
                             p_i_proceso_cod, 
                             p_i_opera_cod, 
                             '', -- TMP AHM adjunto ?
                             'Generación de archivo de rechazadas a recurrente de acreditados',
                             'ID Proceso   : '||p_d_pid||
                             'Proceso      : '||p_i_proceso_cod||
                             'Operacion    : '||p_i_opera_cod||
                             'Fecha Inicio : '||p_fec_ejecucion||
                             'Fecha Fin    : '||DATE
                             )

   END IF
END MAIN
