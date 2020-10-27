#####################################################################
#Ultima Modificación => 28/07/2017                                  #
#Modificado Por    => Cecilia Angel Ceballos                        #
#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Owner             => E.F.P.                                        #
#Programa          => Extractor Créditos en Trámite                 #
#Fecha             => 15/06/2017                                    #
#By                => CARLOS OMAR CRUZ CUVILLOS                     #
#Sistema           => DIS                                           #
#####################################################################
DATABASE safre_viv
  ----DEFINICION DE VARIABLES GLOBALES, PARAMETROS ENVIADOS DESDE EL MENÚ
  DEFINE g_usuario_cod          LIKE seg_usuario.usuario_cod -- usuario que ejecuta el programa
  DEFINE g_pid                  LIKE bat_ctr_proceso.pid     -- ID del proceso
  DEFINE g_tipo_proceso         SMALLINT
  DEFINE g_nom_ventana          STRING
  DEFINE g_proceso_cod          LIKE cat_proceso.proceso_cod -- Codigo del proceso
  DEFINE g_opera_cod            LIKE cat_operacion.opera_cod -- Codigo de operacion
  DEFINE g_folio_liquida        STRING
  DEFINE g_nss                  STRING
    
  DEFINE r_info_extractor       RECORD
    v_nss                       CHAR(11),
    v_nrp                       CHAR(11),
    periodo_pago                CHAR(6),
    folio_sua                   CHAR(6),
    acciones                    DECIMAL(18,6),
    apor_pesos                  DECIMAL(12,2),
    imp_amo_cre                 DECIMAL(12,2),
    f_pago                      DATE ,
    folio_reg_pagos             CHAR(10),
    folio_disp                  CHAR(10),
    num_cred                    CHAR(10)
  END RECORD

  --Variables para las sumas totales
  DEFINE v_tot_acciones         DECIMAL(18,6)
  DEFINE v_tot_pesos            DECIMAL(12,2)
  DEFINE v_tot_imp_am           DECIMAL(12,2)

MAIN
   CALL fn_extractor()
END MAIN
    
FUNCTION fn_extractor()
  DEFINE v_estado               SMALLINT
  DEFINE r_cod_error            SMALLINT
  DEFINE r_mensaje_error        VARCHAR(255)
    
  DEFINE v_ruta_envio           CHAR(40)
  DEFINE v_reporte              STRING
  DEFINE v_nombreReporte        STRING
  DEFINE v_query                STRING
  DEFINE v_condicion            STRING
  DEFINE v_indice               INTEGER
  DEFINE v_encabezado           STRING 
  DEFINE v_archivo              base.Channel
  DEFINE v_detalle              STRING 
  --DEFINE v_query2               STRING 
    
  LET g_usuario_cod   = ARG_VAL(1)
  LET g_pid           = ARG_VAL(2)
  LET g_proceso_cod   = ARG_VAL(3)
  LET g_opera_cod     = ARG_VAL(4)                
  LET g_nss           = ARG_VAL(5)                
  LET g_folio_liquida = ARG_VAL(6)  

  SELECT ruta_envio
  INTO   v_ruta_envio
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_reporte = v_ruta_envio CLIPPED, "/","tramite_restituidos_",TODAY USING "YYYYMMDD",".dis"
  LET v_archivo = base.Channel.create() 
    
  --SI LOS ARGUMENTOS NSS Y FOLIO NO SON NULLOS SE AGREGA A LA CADENA v_condcion
  LET v_condicion = " "

  IF (g_nss != "null") THEN 
      LET v_condicion = " AND NSS = '",g_nss,"'"
  END IF

  IF (g_folio_liquida != "null") THEN
     LET v_condicion = v_condicion || " AND folio_liquida = "||g_folio_liquida        
  END IF
    
  LET v_encabezado = " NSS|NRP|PERIODO PAGO|FOLIO SUA|AIVS|PESOS|AMORTIZACIÓN|FECHA PAGO|FOLIO PAGO|FOLIO DISPERSIÓN|NUMERO CRÉDITO|"
   
  CALL v_archivo.openFile(v_reporte,"w")
  CALL v_archivo.setDelimiter("")
  CALL v_archivo.write(v_encabezado)

  LET v_query ="SELECT b.nss, \n"
                    ," a.nrp, \n"
                    ," a.periodo_pago,\n"                        
                    ," a.folio_sua, \n"                           
                    ," a.aiv_ap_pat, \n"                        
                    ," a.imp_ap_pat, \n"                         
                    ," a.imp_am_cre, \n"                             
                    ," a.f_pago, \n" 
                    ," a.folio_reg_pagos, \n"
                    ," a.folio_liquida, \n"
                    ," a.num_crd_ifv \n"                          
              ," FROM  dis_crd_tramite a, afi_derechohabiente b \n"    
              ," WHERE 1=1 \n", v_condicion
           ," \n AND   a.id_derechohabiente = b.id_derechohabiente \n"
                     
  PREPARE prp_extractor FROM v_query
  DECLARE crs_extractor CURSOR FOR prp_extractor

  LET v_indice  = 1
  LET v_detalle = NULL

  INITIALIZE r_info_extractor TO NULL 
   
  LET v_tot_acciones = 0
  LET v_tot_pesos    = 0
  LET v_tot_imp_am   = 0
   
  FOREACH crs_extractor INTO r_info_extractor.v_nss,
                             r_info_extractor.v_nrp,
                             r_info_extractor.periodo_pago,
                             r_info_extractor.folio_sua,
                             r_info_extractor.acciones,
                             r_info_extractor.apor_pesos,
                             r_info_extractor.imp_amo_cre,
                             r_info_extractor.f_pago,
                             r_info_extractor.folio_reg_pagos,
                             r_info_extractor.folio_disp,
                             r_info_extractor.num_cred
                              
    LET v_detalle = r_info_extractor.v_nss CLIPPED,"|",
                    r_info_extractor.v_nrp CLIPPED,"|",
                    r_info_extractor.periodo_pago CLIPPED,"|",
                    r_info_extractor.folio_sua CLIPPED,"|",
                    r_info_extractor.acciones CLIPPED,"|",
                    r_info_extractor.apor_pesos CLIPPED,"|",
                    r_info_extractor.imp_amo_cre CLIPPED,"|",
                    r_info_extractor.f_pago CLIPPED,"|",
                    r_info_extractor.folio_reg_pagos CLIPPED,"|",
                    r_info_extractor.folio_disp CLIPPED,"|",
                    r_info_extractor.num_cred CLIPPED,"|"
                           
    -- Escribe en el archivo el detalle
    CALL v_archivo.write([v_detalle])
            
    --Suma AIVS
    LET v_tot_acciones = v_tot_acciones +  r_info_extractor.acciones
            
    --Suma Aportaciones Patronales (Pesos)
    LET v_tot_pesos    = v_tot_pesos    + r_info_extractor.apor_pesos
            
    --Suma Amortización
    LET v_tot_imp_am   = v_tot_imp_am   + r_info_extractor.imp_amo_cre
    
    LET v_indice       = v_indice       + 1

  END FOREACH 

  LET v_indice = v_indice - 1

  --LET v_encabezado = " NSS |NRP |PP |FOLIO SUA |ACC |PESOS |AMO |FECHA PAGO |FOLIO PAGO |FOLIO DISP |NUM CRED |"
  --LET v_detalle = "Total registros: ",v_indice,"  ","Total acciones: ",v_tot_acciones,"  ","Total pesos: ",v_tot_pesos,"  ","Total amortización: ",v_tot_imp_am
  LET v_detalle = "Total: |",v_indice,"| | |",v_tot_acciones, "|", v_tot_pesos, "|", v_tot_imp_am
  CALL v_archivo.write([v_detalle])
    
  -- Cierra el archivo creado
  CALL v_archivo.close()
 
  IF SQLCA.SQLCODE != 0 THEN
     LET r_cod_error     = SQLCA.SQLCODE
     LET r_mensaje_error = SQLCA.SQLERRM
     DISPLAY "Error al generar el extractor: Problema con la tabla temporal de consulta: ",SQLCA.SQLCODE,r_cod_error,r_mensaje_error 
     CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)
     RETURNING v_estado
  ELSE
     DISPLAY "Reporte GENERADO EN " 
     DISPLAY v_reporte

     IF (r_cod_error == 0) THEN
        --########### TERMINA PROCESO SE ENVIAN DATOS AL MONITOR
        CALL fn_actualiza_opera_fin(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod)
        RETURNING g_pid

        DISPLAY "Generación exitosa del extractor Créditos en Trámite"
        CALL fn_display_proceso(1,"GENERACIÓN DE REPORTE TRAMITE RESTITUIDO")
     ELSE
        --El uno indca que ocurrio un error al ejecutarse
        DISPLAY "\n [SAFREVIV EXCEPCION ] "
        DISPLAY "Se presentó el siguiente error de ejecución : ",r_cod_error
        DISPLAY "Descripción del error: ",r_mensaje_error,"\n"
        CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  
        RETURNING v_estado
     END IF
  END IF

END FUNCTION