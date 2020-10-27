--===============================================================
-- Version: 1.0.0
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC436                                                                #
#Objetivo     => Consulta Diferencias SSV Pensionados                                   #
#Fecha inicio => Junio 20, 2017                                                         # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
   DEFINE g_ruta_bitacora    CHAR(40) 
   DEFINE g_archivo_bitacora CHAR(1000) 
   DEFINE g_comando          CHAR(1000) 
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
PRIVATE DEFINE ventana     ui.Window
PRIVATE DEFINE forma       ui.Form
MAIN
DEFINE --p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   IF p_usuario_cod  IS NULL THEN 
      LET p_usuario_cod = 'OPSISSACI'
   END IF 

  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   SELECT ruta_listados
   INTO   g_ruta_bitacora
   FROM   seg_modulo
   WHERE  modulo_cod = 'bat'

   --Se inicia el log del programa
   CALL STARTLOG (p_usuario_cod CLIPPED||".RETC436.log")
   
   -- consulta de informacion recibida 
   CALL fn_consulta_detalle()

END MAIN

{ ============================================================================
Clave: RETC436
Nombre: fn_consulta_detalle
Fecha creacion: Junio 20, 2017
Registro de modificaciones:
Descrip: Consulta Detalle Diferencias SSV Pensionados
==============================================================================
}
FUNCTION fn_consulta_detalle()
DEFINE 
      --p_usuario_cod      LIKE seg_usuario.usuario_cod, -- clave del usuario
      v_folio            DECIMAL(9,0), -- folio
      v_f_inicial        DATE,
      v_f_final          DATE,
      
      v_archivo          LIKE glo_ctr_archivo.nombre_archivo,
      v_contador         INTEGER,
      v_cbx_folios       ui.ComboBox, -- combo de folios
      cmb_folios         SMALLINT,
      v_resultado        SMALLINT,
      v_aivs_viv92       DECIMAL(24,6), -- saldo AIVs de viv92
      v_aivs_viv97       DECIMAL(24,6), -- saldo AIVs de viv97
      v_pesos_viv92      DECIMAL(22,2), -- saldo pesos de viv92
      v_pesos_viv97      DECIMAL(22,2), -- saldo pesos de viv97
      v_cod_rechazo      SMALLINT,  
      v_diagnostico      INTEGER,       --diagnostico de la consulta del saldo en la afore
      v_estatus          SMALLINT,        -- estatus de la cuenta individual segun la consulta del saldo en la Afore

      v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
      v_subcta_viv97     SMALLINT,
      v_subcta_viv92     SMALLINT,
      v_hoy              DATE,     
      v_nombre_af        LIKE afi_derechohabiente.nombre_af,
      v_ap_paterno_af    LIKE afi_derechohabiente.ap_paterno_af,
      v_ap_materno_af    LIKE afi_derechohabiente.ap_materno_af,
      
      v_r_grupo   DYNAMIC ARRAY OF RECORD -- registro de despliegue del agrupador
         id_datamart        LIKE ret_datamart.id_datamart,
         f_actualiza        DATE,
         nss                CHAR(11),
         paterno            CHAR(40),
         materno            CHAR(40),
         nombre             CHAR(40),
         aivs_92_saci       DECIMAL(22,2),
         aivs_97_saci       DECIMAL(22,2)
      END RECORD,

      v_r_grupo_indiv   RECORD -- registro de despliegue del agrupador
         id_datamart        LIKE ret_datamart.id_datamart,
         f_actualiza        DATE,
         nss                CHAR(11)
      END RECORD,

      v_arr_reg_detalle         RECORD
         ed_tot_reg               INTEGER                   ,
         ed_pesos_debita          DECIMAL(14,2)                                 ,
         ed_aivs_debita           DECIMAL(18,6),
         ed_total_pagos           INTEGER, 
         ed_sin_dif               INTEGER,
         ed_fch_carga             DATE,
         ed_aivs_viv_97           DECIMAL(18,6),
         ed_aivs_viv_92           DECIMAL(18,6),
         ed_pesos_viv_97          DECIMAL(14,2),
         ed_pesos_viv_92          DECIMAL(14,2),
         ed_nss_no_loc            INTEGER,
         ed_pesos_nss_no_loc      DECIMAL(14,2),
         ed_aivs_nss_no_loc       DECIMAL(18,6)
       END RECORD,
       v_query                       STRING, -- detalle
       v_indice                      SMALLINT, -- indice de arreglo
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo

       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC4361"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   LET  forma = v_ventana.getForm()
   CALL v_ventana.SETTEXT("Consulta Cifras Control")

   LET v_query = "EXECUTE FUNCTION fn_saldo_dia(?,?,?,?)"
   -- se prepara la ejecucion del stored procedure para la consulta del saldo en SACI
   PREPARE sid_saldo_saci FROM v_query
   LET v_subcta_viv97 = 4
   LET v_subcta_viv92 = 8
   LET v_hoy = TODAY 
   LET v_id_derechohabiente = NULL
   

   INPUT v_f_inicial, v_f_final
      FROM d_inicial, d_final
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_f_inicial = TODAY 
         LET v_f_final   = TODAY 
         CALL v_r_grupo.clear() 
         CALL forma.setElementHidden("procesar",1)

      ON ACTION ACCEPT

--        DISPLAY "variables capturadas"
--        DISPLAY "@..", v_folio, v_afore, v_nss, v_f_inicial, v_f_final

         -- se borran los arreglos de despliegue
         CALL v_r_grupo.clear()
         DELETE FROM ret_dif_ssv_pensionado WHERE 1 = 1

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF ( (v_f_inicial IS NULL AND v_f_final IS NULL ) ) THEN
            CALL fn_mensaje("Consulta","Debe de ingresar un rango de fechas a consultar","about")
            CONTINUE INPUT
         ELSE 
            IF v_f_inicial > TODAY OR v_f_final > TODAY THEN 
               CALL fn_mensaje("Consulta","Las fechas no pueden ser mayores a la fecha actual","about")
               CONTINUE INPUT 
            ELSE 
               IF v_f_inicial > v_f_final THEN
                  CALL fn_mensaje("Consulta","La fecha inicial no puede ser mayor a la fecha final","about")
                  CONTINUE INPUT 
               ELSE 
                  IF (v_f_final - v_f_inicial) > 31 THEN 
                     CALL fn_mensaje("Consulta","El periodo de consulta no puede ser mayor a 31 días","about")
                     CONTINUE INPUT 
                  ELSE 
                     LET v_query = "\n SELECT DISTINCT 1 as id_datamart, b.f_actualiza, a.nss                            ",
                                   "\n FROM   ret_datamart a, glo_folio b                                    ",
                                   "\n WHERE  a.folio = b.folio                                              ",
                                   "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'",
                                   "\n AND    a.porcentaje_valuacion > 50                                    ",
                                   "\n AND    a.tpo_prestacion <> '03'                                       "
                     -- consulta
                     DISPLAY "Consulta:\n", v_query
                     --- se buscan parámetros para la consulta del servicio

                     -- se llena el arreglo 
                     PREPARE sid_query FROM v_query
                     DECLARE cur_query CURSOR FOR sid_query
            
                     LET v_indice = 1
            
                     -- se transfieren los datos al arreglo de despliegue agrupador
                     FOREACH cur_query INTO v_r_grupo_indiv.*
                        LET v_r_grupo[v_indice].id_datamart = v_r_grupo_indiv.id_datamart
                        LET v_r_grupo[v_indice].f_actualiza = v_r_grupo_indiv.f_actualiza
                        LET v_r_grupo[v_indice].nss         = v_r_grupo_indiv.nss
                           -- se ejecuta el stored procedure
                        SELECT nombre_af, ap_paterno_af, ap_materno_af, id_derechohabiente
                        INTO   v_nombre_af, v_ap_paterno_af, v_ap_materno_af, v_id_derechohabiente
                        FROM   afi_derechohabiente
                        WHERE  nss = v_r_grupo_indiv.nss
                        LET v_r_grupo[v_indice].paterno = v_ap_paterno_af
                        LET v_r_grupo[v_indice].materno = v_ap_materno_af
                        LET v_r_grupo[v_indice].nombre  = v_nombre_af

                           
                        EXECUTE sid_saldo_saci USING v_r_grupo_indiv.nss, v_id_derechohabiente, v_subcta_viv97, v_hoy 
                                 INTO v_resultado, v_aivs_viv97, v_pesos_viv97
                        LET v_r_grupo[v_indice].aivs_97_saci = v_aivs_viv97
                        EXECUTE sid_saldo_saci USING v_r_grupo_indiv.nss, v_id_derechohabiente, v_subcta_viv92, v_hoy 
                                 INTO v_resultado, v_aivs_viv92, v_pesos_viv92
                        LET v_r_grupo[v_indice].aivs_92_saci = v_aivs_viv92         
                        -- Se obtiene el id_solicitud
                        
                        INSERT INTO ret_dif_ssv_pensionado 
                               (fch_inicio,fch_fin,nss,aivs_viv92_saci,aivs_viv97_saci,
                                ape_paterno, ape_materno, nombre)
                               VALUES 
                               (v_f_inicial, v_f_final,v_r_grupo_indiv.nss,
                                v_aivs_viv92,v_aivs_viv97, v_ap_paterno_af, v_ap_materno_af, v_nombre_af);
                        
                        -- se incrementa el indice
                        LET v_indice = v_indice + 1
                     END FOREACH
                     IF v_indice > 1 THEN
                        CALL forma.setElementHidden("procesar",0)
                     END IF 
                     DISPLAY ARRAY v_r_grupo TO tbl_detalle.*
                     ON ACTION Procesar
                        IF v_indice > 1 THEN 
                           CALL fn_procesa() RETURNING v_resultado;
                           IF v_resultado <> 0 THEN
                              CALL fn_mensaje("Consulta","Hubo un problema al generar el archivo","about")
                           END IF 
                        ELSE 
                           CALL fn_mensaje("Consulta","La consulta no produjo resultados, \n no hay archivo que generar.","about")
                        END IF 
                     ON ACTION CANCEL 
                        CALL forma.setElementHidden("procesar",1)
                        EXIT DISPLAY 
                     END DISPLAY 
                  END IF
               END IF 
            END IF 
         END IF 
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_saldos

END FUNCTION



--Función que exporta datos a un archivo
FUNCTION fn_procesa()
DEFINE --p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_mensaje         STRING,
       v_resultado       INTEGER, -- resultado del proceso
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio           LIKE glo_folio.folio
DEFINE v_ruta      RECORD
      bin            LIKE seg_modulo.ruta_bin,
      listados       LIKE seg_modulo.ruta_listados
END RECORD 
   -- este proceso inicia en automático, no tiene archivo
   LET v_nombre_archivo = "NA"

   -- Obtiene las rutas necesarias 
   SELECT ruta_bin
   INTO   v_ruta.bin
   FROM   seg_modulo
   WHERE  modulo_cod = 'ret'

   SELECT ruta_listados
   INTO   v_ruta.listados
   FROM   seg_modulo
   WHERE  modulo_cod = 'bat'
   
   LET g_proceso_cod = g_proceso_det_dif_ssv_pensionado -- Retiro por fondo ahorro 
   LET g_opera_cod   = g_opera_dif_ssv_pensionados_genera -- recepcion de archivo
   
   -- el folio se generara en el programa lanzado
   LET p_folio = 0

   -- se verifica si se puede continuar con la operacion
   LET v_resultado = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   -- se valida que se pueda iniciar la operacion
   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING v_resultado

   -- si se pudo validar
   IF ( v_resultado = 0 ) THEN
      	
      -- se genera el pid 
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid
      
      -- Se inicializa el proceso
      CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 p_folio           ,
                                 "RETC436"          ,
                                 v_nombre_archivo  ,
                                 p_usuario_cod)  RETURNING v_resultado
      
      -- si se pudo iniciar la operacion
      IF ( v_resultado = 0 ) THEN
         
         -- inicia la operacion
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETC436","NA",p_usuario_cod)
         RETURNING v_resultado
         
                                       
         LET v_s_comando = " nohup time fglrun ",v_ruta.bin CLIPPED,"/RETP438 ",
                           p_usuario_cod CLIPPED, " ",
                           g_pid  , " " ,
                           g_proceso_cod , " " ,
                           g_opera_cod ," ",
                           p_folio ," ",
                           v_nombre_archivo CLIPPED," ",
                           " 1>",v_ruta.listados CLIPPED,
                           "/nohup:",g_pid USING "&&&&&",":",
                           g_proceso_cod USING "&&&&&",":",
                           g_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"
         
         DISPLAY v_s_comando                        
         RUN v_s_comando
         
         --Se envía mensaje de advertencia indicando que se ha comenzado con la generación del archivo
         CALL fn_mensaje("Atención", "Se ha enviado la generación del archivo de salida.\nPodrá revisar el resultado en el monitor de ejecución de procesos", "information")
      ELSE
         CALL fn_mensaje("Atención","No se puede iniciar la operación.","stop")
      END IF
   ELSE
      -- se muestra en pantalla por que no se puede enviar el proceso
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
      
   END IF
   RETURN v_resultado
END FUNCTION
