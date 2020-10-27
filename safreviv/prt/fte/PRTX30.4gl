################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTX30                                                     #
#Objetivo        => Programa Lanzador Generacion Archivo Contingente           #
#                   Portabilidad Viv 92                                        #
#Fecha Inicio    => Noviembre 22 2018                                          #
################################################################################

DATABASE safre_viv
DEFINE g_enter smallint
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_cad_ventana     STRING,
       v_ruta            LIKE seg_modulo.ruta_bin,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_movimientos     DYNAMIC ARRAY OF RECORD
         v_consecutivo   INTEGER,
         v_id_solicitud  LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
         nss             CHAR(11),
         saldo_insoluto        LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste ,
         pesos_aplicados_viv97 LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
         saldo_insoluto_actual LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
         pesos_viv92_infonavit LIKE prt_solicitud_cedente.pesos_saldo_viv92_infonavit, 
         pesos_viv92_infonavit_solicitar LIKE prt_solicitud_cedente.pesos_saldo_viv92_infonavit 
       END RECORD,
       v_monto_total     LIKE sep_batch_contabilidad.monto,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       r_pid             LIKE bat_ctr_operacion.pid,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       v_opera_cod       LIKE cat_operacion.opera_cod,
       v_folio           LIKE glo_folio.folio,
       v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo

MAIN

    LET p_usuario_cod   = ARG_VAL(1)
    LET p_tpo_ejecucion = ARG_VAL(2)
    LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_genera_archivo_ajustes()
END MAIN



FUNCTION fn_genera_archivo_ajustes()
DEFINE v_continua BOOLEAN,
       r_confirma BOOLEAN,
       v_consulta BOOLEAN

   
   LET v_proceso_cod = 2817  -- Generación Archivo Contingente Portabilidad Viv 92
   LET v_opera_cod   = 1
   LET v_continua = FALSE

   CALL fn_rutas("prt") RETURNING v_ruta_ejecutable,v_ruta
   CALL fn_rutas("bat") RETURNING v_ruta,v_ruta_listados

   OPEN WINDOW vtna_genera_ajustes WITH FORM v_ruta_ejecutable CLIPPED||"/PRTX301"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      DISPLAY ARRAY v_movimientos TO sr_movimientos.*
              ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY 
            # Recupera registros de ajustes
            CALL fn_recupera_registros() RETURNING v_consulta
            IF NOT(v_consulta)THEN
               CALL fn_mensaje(p_cad_ventana,"No se encontraron registros para contingente portabilidad viv 92 ","information")
               EXIT DISPLAY
            END IF

         ON ACTION aceptar
            CALL fn_ventana_confirma(p_cad_ventana,"Confirmar Generar Archivo","question")
                   RETURNING r_confirma
            IF(r_confirma)THEN

               CALL fn_registra_solicitudes_v92()
            
               CALL fn_lanza_batch_contingente() RETURNING v_continua
               IF(v_continua)THEN
                  CONTINUE DISPLAY
               ELSE
                  EXIT DISPLAY
               END IF
            ELSE
               CONTINUE DISPLAY
            END IF

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_genera_ajustes

END FUNCTION

FUNCTION fn_registra_solicitudes_v92()

DEFINE v_txt STRING
DEFINE i INTEGER 
DEFINE l_seq_sol_prt DEC(10,0)

DEFINE rl_prt_solicitud_cedente RECORD LIKE prt_solicitud_cedente.*

   FOR i = 1 TO v_movimientos.getLength()

       SELECT * 
       INTO rl_prt_solicitud_cedente.*
       FROM prt_solicitud_cedente 
       WHERE id_prt_solicitud_cedente = v_movimientos[i].v_id_solicitud

       LET v_txt = " SELECT seq_prt_solicitud_cedente.nextval 
                     FROM   systables 
                     WHERE  tabname = 'systables' "

       PREPARE qry_seq FROM v_txt
       EXECUTE qry_seq INTO l_seq_sol_prt
                     
     INSERT INTO prt_solicitud_cedente
       (id_prt_solicitud_cedente,
        nss ,
        curp ,
        paterno ,
        materno ,
        nombre ,
        id_credito_fovissste ,
        correo_e ,
        telefono ,
        saldo_insoluto_credito_fovissste ,
        f_originacion_fovissste ,
        f_consulta_credito ,
        f_ini_tramite ,
        tipo_portabilidad ,
        n_caso ,
        estado ,
        resultado_operacion ,
        diagnostico_interno ,
        folio_procesar )
     VALUES        
       (l_seq_sol_prt,     
        rl_prt_solicitud_cedente.nss ,
        rl_prt_solicitud_cedente.curp ,
        rl_prt_solicitud_cedente.paterno ,
        rl_prt_solicitud_cedente.materno ,
        rl_prt_solicitud_cedente.nombre ,
        rl_prt_solicitud_cedente.id_credito_fovissste ,
        rl_prt_solicitud_cedente.correo_e ,
        rl_prt_solicitud_cedente.telefono ,
        v_movimientos[i].saldo_insoluto_actual ,
        rl_prt_solicitud_cedente.f_originacion_fovissste ,
        rl_prt_solicitud_cedente.f_consulta_credito ,
        TODAY,
        "1",
        rl_prt_solicitud_cedente.n_caso ,
        "40",
        rl_prt_solicitud_cedente.resultado_operacion ,
        rl_prt_solicitud_cedente.diagnostico_interno ,
        rl_prt_solicitud_cedente.folio_procesar )

   END FOR


END FUNCTION


FUNCTION fn_recupera_registros()

DEFINE r_prt_solicitud_cedente RECORD LIKE prt_solicitud_cedente.*
DEFINE l_acciones_viv92 LIKE cta_movimiento.monto_pesos,
       l_precio_fondo LIKE glo_valor_fondo.precio_fondo,
       l_dif          LIKE cta_movimiento.monto_pesos

DEFINE v_consulta STRING,
       v_indice   INTEGER

   WHENEVER ERROR CONTINUE 
   LET v_indice = 1
   CALL v_movimientos.clear()
   LET v_monto_total = 0

   LET v_consulta = " SELECT a.* 
                        FROM prt_solicitud_cedente a  
                        WHERE a.estado =  80 
                        AND a.nss in (select b.nss from prt_salida_final_contingente b) "

   PREPARE prp_recupera_consulta FROM v_consulta
   DECLARE cur_recupera_consulta CURSOR FOR prp_recupera_consulta
   
   FOREACH cur_recupera_consulta INTO r_prt_solicitud_cedente.*
   
      LET v_movimientos[v_indice].v_consecutivo   = v_indice
      LET v_movimientos[v_indice].v_id_solicitud  = r_prt_solicitud_cedente.id_prt_solicitud_cedente      
      LET v_movimientos[v_indice].nss             = r_prt_solicitud_cedente.nss
      LET v_movimientos[v_indice].saldo_insoluto  = r_prt_solicitud_cedente.saldo_insoluto_credito_fovissste

      --SELECT SUM(mto_pesos_infonavit97_cedido) 
      --INTO   v_movimientos[v_indice].pesos_aplicados_viv97
      --FROM prt_traspaso_cedente 
      --WHERE id_prt_solicitud_cedente = r_prt_solicitud_cedente.id_prt_solicitud_cedente
      --AND   estado = 40

      --LET v_movimientos[v_indice].saldo_insoluto_actual = r_prt_solicitud_cedente.saldo_insoluto_credito_fovissste -  
                                                          --v_movimientos[v_indice].pesos_aplicados_viv97 
      SELECT a.saldo_insoluto 
      INTO   v_movimientos[v_indice].saldo_insoluto_actual
      FROM   prt_salida_final_contingente a
      WHERE  a.nss = r_prt_solicitud_cedente.nss
      
      LET l_acciones_viv92 = 0
      LET l_precio_fondo   = 0
      
      SELECT a.precio_fondo
      INTO   l_precio_fondo
      FROM   glo_valor_fondo a
      WHERE  a.fondo = 11
      AND    a.f_valuacion = "02/01/2019"
      
      SELECT sum(a.monto_acciones) 
      INTO   l_acciones_viv92      
      FROM cta_movimiento  a ,
           afi_derechohabiente b 
      WHERE b.nss = r_prt_solicitud_cedente.nss
      AND   b.id_derechohabiente = a.id_derechohabiente 
      AND   a.subcuenta = 8      

      LET v_movimientos[v_indice].pesos_viv92_infonavit = l_acciones_viv92 * l_precio_fondo

      LET l_dif = 0
      
      LET l_dif = v_movimientos[v_indice].saldo_insoluto_actual - 
                  v_movimientos[v_indice].pesos_viv92_infonavit 


      IF l_dif <= 0 THEN 
         LET v_movimientos[v_indice].pesos_viv92_infonavit_solicitar = v_movimientos[v_indice].saldo_insoluto_actual
      ELSE 
         LET v_movimientos[v_indice].pesos_viv92_infonavit_solicitar = v_movimientos[v_indice].pesos_viv92_infonavit
      END IF 
      
      LET v_indice = v_indice + 1
      
   END FOREACH
   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY SQLCA.SQLCODE 
   END IF
   FREE cur_recupera_consulta

   IF(v_movimientos.getLength() > 0)THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF 
END FUNCTION


FUNCTION fn_lanza_batch_contingente()
DEFINE v_comando   STRING,
       v_resultado SMALLINT,
       v_continuar BOOLEAN

   LET v_continuar = FALSE

   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING v_resultado
   # Se verifica si la operacion es valida
   IF(v_resultado <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_resultado)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF

   LET v_folio = 0
   LET v_nom_archivo = "NA"
   CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod) 
          RETURNING r_pid 
   CALL fn_inicializa_proceso(r_pid,v_proceso_cod,v_opera_cod,0,
                              "PRTX30",v_nom_archivo,p_usuario_cod)
              RETURNING v_resultado
   IF( v_resultado <> 0 ) THEN
      CALL fn_muestra_inc_operacion(v_resultado)
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   
   CALL fn_actualiza_opera_ini(r_pid,v_proceso_cod,v_opera_cod,v_folio,"PRTX30",
                               v_nom_archivo,p_usuario_cod) RETURNING v_resultado
   IF(v_resultado <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_resultado)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   
   LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/PRTX31.42r ",
                                   p_usuario_cod CLIPPED, " ",r_pid, " ",
                                   v_proceso_cod," ",v_opera_cod," ",
                                   v_folio, " ",v_nom_archivo,
                   " 1> ", v_ruta_listados CLIPPED,
                   "/nohup:",r_pid USING "&&&&&",":",
                             v_proceso_cod USING "&&&&&",":",
                             v_opera_cod USING "&&&&&",
                    " 2>&1 &"
   DISPLAY v_comando
   RUN v_comando
   IF(STATUS)THEN
      LET v_continuar = TRUE
      CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la operación","about")
   ELSE
      CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos","about")
      LET v_continuar = FALSE
   END IF

   RETURN v_continuar  
END FUNCTION