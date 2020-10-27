###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGP                                                    #
#Objetivo          => Lanzado genera archivo de saldos de créditos Apoyo      #
#                     Infonavit vigentes.                                     #
#Autor             => Emilio Abarca EFP                                       #
#Fecha inicio      => 12 Enero 2016                                           #
###############################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE p_usuario                CHAR(20)   # Usuario logueado
   DEFINE p_pid                    DECIMAL(9,0)
   DEFINE p_proceso_cod            INTEGER
   DEFINE p_opera_cod              INTEGER   
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   DEFINE v_return                 BOOLEAN 
   
END GLOBALS 

MAIN

   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGP21.log")

   --Se obtiene la ruta donde se alojará el archivo de salida
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   CALL fn_display_proceso(0,"INICIA GENERACIÓN ARCHIVO DE SALDOS APOYO INFONAVIT")
   
   CALL genera_arh_saldos() RETURNING v_return

   IF (v_return <>  0) THEN
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_return 
   ELSE 
     DISPLAY "" 
     DISPLAY " => Se ha generado el archivo de saldos AI con nombre : SDO43VIG.txt "
     DISPLAY " => El archivo de encuentra en la ruta /safreviv_int/ocg/envio "
     DISPLAY " => El respaldo del archivo SDO43VIG.txt se ha realizado correctamente "
     DISPLAY " "

     DISPLAY " PROCESO EJECUTADO CORRECTAMENTE"
     
     CALL fn_display_proceso(1,"FIN GENERACIÓN ARCHIVO DE SALDOS APOYO INFONAVIT") 

     CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_return 
   END IF 
   
END MAIN 

FUNCTION genera_arh_saldos()

   DEFINE v_query           STRING
   DEFINE v_precio_fondo    DECIMAL(19,14)
   DEFINE k                 INTEGER
   DEFINE v_arh_salida      STRING
   DEFINE v_arh_sal_resp    STRING 
   DEFINE i                 INTEGER
   DEFINE v_monto_pesos     DECIMAL(12,2)
   DEFINE ch                base.Channel
   DEFINE v_detalle         STRING
   DEFINE v_cp_comand       STRING
   DEFINE v_conv_DOS        STRING 
   DEFINE v_i_cp            BOOLEAN
   DEFINE v_i_conversion    BOOLEAN 
   
   DEFINE arr_arh_saldos DYNAMIC ARRAY OF RECORD
      nss                 CHAR(11) ,
      id_derechohabiente  DECIMAL(9,0),
      cve_ent_financiera  CHAR(3),
      num_ctr_int_ef      CHAR(18)
   END RECORD   
    
   LET v_arh_salida   = v_ruta_envio CLIPPED,"/SDO43VIG.txt"
   LET v_arh_sal_resp = v_ruta_envio CLIPPED,"/SDO43VIG_" CLIPPED, TODAY USING "ddmmyyyy" CLIPPED,".txt"  
            
   -- Se obtiene el valor del precio fondo
   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = TODAY 
      AND fondo       = 11;
               
   LET v_query = "SELECT UNIQUE d.nss,
                                f.id_derechohabiente,
                                f.cve_ent_financiera,
                                f.num_ctr_int_ef
                           FROM ocg_formalizacion f,
                                ocg_acreditado a,
                                ocg_detalle d
                          WHERE f.id_ocg_formalizacion = a.id_ocg_formalizacion
                            AND f.id_ocg_detalle       = d.id_ocg_detalle
                            AND f.tpo_credito IN ('A','C')
                            AND a.situacion IN (55,60,70,80)
                            AND d.subproceso = 002"
   
   PREPARE prp_consulta FROM v_query
   DECLARE crsr_consulta CURSOR FOR prp_consulta
   
   LET k = 1
   
   CALL arr_arh_saldos.clear()
   
   FOREACH crsr_consulta INTO arr_arh_saldos[k].nss,
                              arr_arh_saldos[k].id_derechohabiente,
                              arr_arh_saldos[k].cve_ent_financiera,
                              arr_arh_saldos[k].num_ctr_int_ef
 
      LET k = k + 1    
      
   END FOREACH
   
   LET ch = base.Channel.create()
   CALL ch.openFile(v_arh_salida,"w")
            
   -- Se lee el arreglo completo
   FOR i = 1 TO arr_arh_saldos.getLength()
            
      IF(arr_arh_saldos[i].id_derechohabiente IS NOT NULL) THEN

         -- Obtiene precio monto pesos
         SELECT ROUND(SUM(monto_acciones) * v_precio_fondo,2)
            INTO v_monto_pesos
            FROM cta_movimiento
           WHERE id_derechohabiente = arr_arh_saldos[i].id_derechohabiente
             AND subcuenta       = 4
             AND fondo_inversion = 11

         IF (v_monto_pesos IS NULL) THEN
            LET v_monto_pesos = 0.00 
         END IF 
               
         LET v_detalle = arr_arh_saldos[i].cve_ent_financiera USING "&&&","|",
                         arr_arh_saldos[i].nss,"|",
                         arr_arh_saldos[i].num_ctr_int_ef,"|", 
                         v_monto_pesos,"|",
                         TODAY USING "yyyymmdd","|"

          CALL ch.writeLine(v_detalle)

      END IF 
               
   END FOR 

   CALL ch.close()
            
   -- Realizando copia del archivo
   LET v_cp_comand = "cp"," ",v_arh_salida," ",v_arh_sal_resp
   RUN v_cp_comand RETURNING v_i_cp

   -- Convirtiendo a formato DOS
   LET v_conv_DOS = "sed 's/$/\r/' ",v_arh_sal_resp," > ",v_arh_salida
   RUN v_conv_DOS RETURNING v_i_conversion

   RETURN v_i_cp
         
END FUNCTION

