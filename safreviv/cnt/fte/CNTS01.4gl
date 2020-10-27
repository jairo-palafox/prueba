################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 16/05/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => fn_genera_poliza_cnt                                     #
#Objetivo          => Generar el archivo de la póliza contable                 #
#Fecha inicio      => 16/05/2012                                               #
################################################################################
IMPORT os
DATABASE safre_viv
GLOBALS "CNTG01.4gl"

GLOBALS 
   DEFINE 
      p_pid                   DECIMAL (9,0),
      v_archivo_copia         VARCHAR (25),
      v_tipo_transaccion      SMALLINT, -- 0 es registro normal, 1 es reversado
      v_cod_transaccion_cnt   SMALLINT, -- 0 es registro normal, 1 es reversado
      v_fecha_formato         CHAR (8)


END GLOBALS 
MAIN 

   CALL fn_genera_poliza_cnt()

END MAIN 
#Objetivo: Generar el archivo con la póliza contable de los procesos 
#          registrados en el día.
FUNCTION fn_genera_poliza_cnt()
DEFINE v_ch_arch_salida    BASE.CHANNEL,
       v_folio_proc_cnt    DECIMAL(9,0),
       v_ruta_envio_dis    CHAR(40), --LIKE seg_modulo.ruta_envio,
       v_modulo_cod        CHAR(3),--LIKE seg_modulo.modulo_cod,
       v_ruta_nomarch      VARCHAR(100), -- ruta y nombre del archivo de salida
       v_nom_archivo       VARCHAR(40), -- nombre del archivo de salida
       v_ddmmaaaa          VARCHAR(08), -- fecha del archivo de salida
       v_aaaammdd          VARCHAR(08), -- fecha del archivo de salida
       v_cont_dia          SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia           CHAR(02), -- Parametro consecutivo de registro por dia
       v_ins_reg           STRING,  -- almacena cadena a insertar en el archivo
       v_qry_txt           STRING, -- cadena para preparar consultas
       v_qry_txt1          STRING, -- cadena para preparar consultas  
       v_busca_nom_archivo STRING, -- busca nombre de archivo
       p_proceso_cod       LIKE cat_proceso.proceso_cod,
       p_opera_cod         LIKE cat_operacion.opera_cod,
       v_cuenta_poliza     INTEGER,
       p_b_despliegue_pantalla SMALLINT,
       v_folio_liquida     LIKE cnt_transaccion.folio_liquida,
       v_fecha_liquida     LIKE cnt_transaccion.f_liquida,
       --g_usuario CHAR(40),
       
       p_programa          CHAR(10),
       r_bandera           SMALLINT,
       v_indice            SMALLINT,
       v_id_funcion        SMALLINT,
       v_cta_contable      LIKE cnt_transaccion.cta_contable,
       v_importe           LIKE cnt_transaccion.importe,
       v_cod_naturaleza_cta LIKE cnt_transaccion.cod_naturaleza_cta,
       v_cod_proceso_cnt   LIKE cnt_transaccion.cod_proceso_cnt,
       v_desc_proceso_cnt  LIKE cat_proceso_cnt.desc_proceso_cnt,
       v_referencia_cnt    LIKE cat_proceso_cnt.referencia_cnt,
       v_folio_proc_cnt_char CHAR(6),
       v_cod_proceso_cnt_char CHAR(3),
       p_folio_cnt         DECIMAL(9,0),
       v_fecha_emi_man     DATE,
       r_error_bnd         SMALLINT, --bandera para funciones generales
       p_transaccion       SMALLINT,
       v_control           INTEGER

DEFINE indicador CHAR(2),
       fec_docto CHAR(8),
       clase_docto CHAR(2),
       sociedad CHAR(4),
       fec_contable CHAR(8),
       mes_contable CHAR(2),
       moneda CHAR(5),
       referencia CHAR(16),
       fecha_ref CHAR(6)

DEFINE indicador1 CHAR(2),
       cve_contable CHAR(2),
       cliente_acr_mayor CHAR(10),
       monto CHAR(13),
       division CHAR(4),
       texto_posicion CHAR(50)
       
   --Asignación de parametros generales 
   LET g_usuario = ARG_VAL(1)
   LET p_pid = ARG_VAL(2) 
   LET p_proceso_cod = ARG_VAL(3) 
   LET p_opera_cod = ARG_VAL(4)
   LET v_folio_proc_cnt = ARG_VAL(5) 
   LET v_nom_archivo = ARG_VAL(6)



   
   LET p_proceso_cod = 601 --Codigo del Proceso de Contabilidad 601
   LET p_opera_cod = 1 --Código de la operación de Generación de póliza contable
   LET v_modulo_cod = "cnt"
   LET v_cont_dia   = 1
   LET v_fecha_formato = TODAY USING "yyyymmdd"

      #Valida que exista información para poder generar la póliza

      SELECT count(*)  
      INTO v_cuenta_poliza
      FROM cnt_transaccion ct, cat_proceso_cnt cp 
      WHERE ct.cod_proceso_cnt = cp.cod_proceso_cnt 
      AND estado = 10


      IF v_cuenta_poliza IS NULL OR v_cuenta_poliza = 0 THEN
         DISPLAY "Info: Imposible generar la póliza contable debido a falta de información o a que ya se ha enviado."

         DISPLAY "Pid: ",p_pid
         DISPLAY "Proceso: ",g_proceso_cod
         DISPLAY "Operación: ",g_opera_cod1
         
         CALL fn_error_opera(p_pid,g_proceso_cod,g_opera_cod1)
                      RETURNING r_error_bnd

         --DISPLAY "r_error_bnd -- ",r_error_bnd

         IF r_error_bnd <> 0 THEN 
            CALL fn_desplega_inc_operacion(r_error_bnd)
            --DISPLAY "Exit"
         END IF 
                      
         EXIT PROGRAM 
      END IF 

      SELECT  ind_tipo_ejecucion 
      INTO p_transaccion
      FROM  bat_ctr_operacion 
      WHERE proceso_cod = g_proceso_cod   
      AND  pid = p_pid
      AND opera_cod = g_opera_cod1

      

      CALL fn_genera_folio(p_proceso_cod, p_opera_cod,g_usuario)
         RETURNING v_folio_proc_cnt

      -- se obtienen la ruta envio del módulo
      SELECT ruta_envio 
        INTO v_ruta_envio_dis
        FROM seg_modulo
       WHERE modulo_cod = v_modulo_cod

      -- se crea el nombre del archivo y posteriormente se concatena con la ruta
      LET v_nom_archivo = "/"
      LET v_ddmmaaaa = TODAY USING "ddmmyyyy"
      LET v_busca_nom_archivo = v_ddmmaaaa 
   
      --Obtine consecutivo para archivo por día
      CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
           RETURNING v_cont_dia
      LET v_reg_dia = v_cont_dia USING "&&"

      --Asigna el nombre del archivo a ddmmaaaa_######_&&.cnt
      --LET v_nom_archivo = v_ddmmaaaa || "_"|| v_folio_proc_cnt || "_" ||  v_reg_dia||"." 
      --                                                        || v_modulo_cod

      --LET v_aaaammdd = TODAY USING "yyyymmdd"
      --LET v_nom_archivo = "SAFRE_",v_aaaammdd,".cnt"

      LET v_nom_archivo = 'poliza_cont.cnt'
                                                              
      LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || "/"|| v_nom_archivo

      -- se crea el manejador de archivo
      LET v_ch_arch_salida = base.Channel.create()
      
      -- se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
      CALL v_ch_arch_salida.setDelimiter("")
 
   ####Si la poliza fue manual
IF p_transaccion = 0 THEN 

   LET p_b_despliegue_pantalla = 1
   
   LET v_id_funcion = 2 --Para que actualice el estado a 70 en cnt_ctr_proceso

	--Consulta campos para ENCABEZADO
   LET v_qry_txt = "\n SELECT ct.cod_proceso_cnt, cp.referencia_cnt, ct.f_emision, ct.folio_liquida, ct.f_liquida  ",
                   "\n   FROM cnt_transaccion ct, cat_proceso_cnt cp",
                   "\n  WHERE ct.cod_proceso_cnt = cp.cod_proceso_cnt",
                   "\n    AND estado = 10",
                   "\n  GROUP BY 1,2,3,4,5",
                   "\n  ORDER BY 1"
				   
	PREPARE prp_consulta_campos_enc_man FROM v_qry_txt

   LET v_indice = 1

    DECLARE cur_consulta_campos_enc_man CURSOR FOR prp_consulta_campos_enc_man
    FOREACH cur_consulta_campos_enc_man INTO v_cod_proceso_cnt, v_referencia_cnt, v_fecha_emi_man, v_folio_liquida, v_fecha_liquida

      ####################################
      #SE ARMA EL ENCABEZADO DEL ARCHIVO #
      ####################################
      LET indicador = "01"
      --LET fec_docto = v_fecha_emi_man USING "yyyymmdd"
      LET fec_docto = v_fecha_liquida USING "yyyymmdd"
      LET clase_docto = "ZL"
      LET sociedad  = "INFO"
      LET fec_contable = v_fecha_liquida USING "yyyymmdd"  --Se cambia a fecha de liquidación
      LET mes_contable = MONTH(v_fecha_liquida) USING "&&"  --A partir de la fecha de liquidación
      LET moneda = "MXN  "
      --LET fecha_ref = v_fecha_emi_man USING "yymmdd"  --Se cambiará por 000000    
      LET fecha_ref = "000000"
      LET v_folio_proc_cnt_char = v_folio_liquida USING"&&&&&&"    --Se cambia a folio de liquidación en lugar de folio contable
      LET referencia = v_referencia_cnt||fecha_ref||v_folio_proc_cnt_char

      LET v_ins_reg = indicador,fec_docto,clase_docto,sociedad,
                      fec_contable,mes_contable,moneda,referencia

      CALL v_ch_arch_salida.write([v_ins_reg])                              


		--Consulta campos para DETALLE
          LET v_qry_txt1 = "\n SELECT ct.cta_contable, ct.importe, ct.cod_naturaleza_cta,",
                           --"\n        ct.cod_proceso_cnt, cp.desc_proc_corta_cnt, cp.referencia_cnt ",
                           "\n        ct.cod_proceso_cnt, cp.desc_proc_corta_cnt, cp.referencia_cnt, tpo_transaccion, cod_transaccion_cnt ",
                           "\n   FROM cnt_transaccion ct, cat_proceso_cnt cp",
                           "\n  WHERE ct.cod_proceso_cnt = cp.cod_proceso_cnt",
                           "\n    AND ct.cod_proceso_cnt = ",v_cod_proceso_cnt,
                           "\n    AND ct.folio_liquida = ",v_folio_liquida,
                           "\n    AND estado = 10",
                           --"\n    AND f_emision = '",v_fecha_emi_man,"' ",
                           "\n  GROUP BY 1,2,3,4,5,6,7,8",
                           "\n  ORDER BY 1,2,3,4,5,6,7,8"

        PREPARE prp_consulta_campos_man FROM v_qry_txt1     
        DECLARE cur_consulta_campos_man CURSOR FOR prp_consulta_campos_man


		FOREACH cur_consulta_campos_man INTO v_cta_contable, v_importe, v_cod_naturaleza_cta,
                                             --v_cod_proceso_cnt, v_desc_proceso_cnt, v_referencia_cnt
                                             v_cod_proceso_cnt, v_desc_proceso_cnt, v_referencia_cnt,v_tipo_transaccion, v_cod_transaccion_cnt

                 LET indicador1 = "02"
                 
                 IF v_cod_naturaleza_cta = 1 THEN 
                    LET cve_contable = 50
                 ELSE 
                    LET cve_contable = 40
                 END IF
                 
                 LET cliente_acr_mayor = v_cta_contable
                 LET monto = v_importe USING "&&&&&&&&&&.&&"
                 LET division = "09"
                 LET v_cod_proceso_cnt_char = v_cod_proceso_cnt USING "&&&"

                 --Evalúa si el registro es normal o ya tiene un reverso
                 IF v_tipo_transaccion = 1 THEN -- 1 es reversado
                    LET texto_posicion = "REV-"||fecha_ref||v_folio_proc_cnt_char||v_cod_proceso_cnt_char||v_desc_proceso_cnt
                 ELSE
                    LET texto_posicion = v_referencia_cnt||fecha_ref||v_folio_proc_cnt_char||v_cod_proceso_cnt_char||v_desc_proceso_cnt
                 END IF    

                 ###### Actualización para proceso 20 para la cuenta 2403011600
                 IF v_cod_proceso_cnt = 20 AND v_cod_transaccion_cnt = 12 AND v_cod_naturaleza_cta = 1 THEN 
                     LET texto_posicion = "ER170_"||v_fecha_formato||"_TRASPASO DE SSV"
                 END IF 

                 IF v_cod_proceso_cnt = 20 AND v_cod_transaccion_cnt = 12 AND v_cod_naturaleza_cta = 2 THEN 
                     LET texto_posicion = "ER207_"||v_fecha_formato||"_CARGO A CAPITAL"
                 END IF

                 IF v_cod_proceso_cnt = 20 AND v_cod_transaccion_cnt = 2 AND v_cod_naturaleza_cta = 1 THEN 
                     LET texto_posicion = "ER171_"||v_fecha_formato||"_TRASPASO FONDO AHORRO"
                 END IF

                 ################################################################ 
                 
                 LET v_ins_reg = indicador1,cve_contable,cliente_acr_mayor,
                                 monto,division,texto_posicion

                  CALL v_ch_arch_salida.write([v_ins_reg])

                  LET v_indice = v_indice + 1              
            END FOREACH

                  	--DISPLAY "Póliza Manual - Actualiza folio contable al proceso ",v_cod_proceso_cnt
                     UPDATE cnt_transaccion 
                        SET folio_cnt = v_folio_proc_cnt
                      WHERE estado = 10
                      AND folio_liquida = v_folio_liquida
                      --  AND f_emision = v_fecha_emi_man 
                        --AND cod_proceso_cnt = v_cod_proceso_cnt


                    --DISPLAY "Póliza Manual - Actualiza estado a Enviado SAP-FICO"
                    UPDATE cnt_transaccion 
                       SET estado = 70
                     WHERE estado = 10
                     AND folio_liquida = v_folio_liquida
                     --  AND f_emision = v_fecha_emi_man 
                       --AND cod_proceso_cnt = v_cod_proceso_cnt

                   LET fec_docto = v_fecha_emi_man USING "mmddyyyy"
                       
                   

                  SELECT COUNT (*)
                  INTO v_control
                  FROM cnt_ctr_proceso
                  WHERE folio_liquida = v_folio_liquida
                  AND folio_cnt = v_folio_proc_cnt
                  --AND f_liquida = v_fecha_liquida
                  --AND nombre_archivo = v_nom_archivo
                  --AND f_emision = v_fecha_emi_man
                  
                  IF v_control <= 0 THEN 
                           --Inserta un registro en la tabla de control por cada encabezado
                           PREPARE prp_fn_transaccion2_man
                           FROM "EXECUTE PROCEDURE safre_viv:sp_cnt_Transaccion2(?,?,?,?,?,?)"
                           EXECUTE prp_fn_transaccion2_man USING  v_folio_proc_cnt,
                                                                  v_folio_liquida,
                                                                  v_fecha_liquida,
                                                                  v_nom_archivo,
                                                                  fec_docto,
                                                                  v_id_funcion  
                  
                  END IF 
   
                                                         
                       
                   LET v_indice = v_indice + 1
     END FOREACH 

      

####Si la poliza fue automática
ELSE 

   LET p_b_despliegue_pantalla = 0


   LET v_id_funcion = 1 --Para que actualice el estado a 20

   --Consulta campos para ENCABEZADO
   LET v_qry_txt = "\n SELECT ct.cod_proceso_cnt, cp.referencia_cnt, ct.f_emision, ct.folio_liquida, ct.f_liquida  ",
                   "\n   FROM cnt_transaccion ct, cat_proceso_cnt cp",
                   "\n  WHERE ct.cod_proceso_cnt = cp.cod_proceso_cnt",
                   "\n    AND estado = 10",
                   "\n  GROUP BY 1,2,3,4,5",
                   "\n  ORDER BY 1"

   PREPARE prp_consulta_campos_enc FROM v_qry_txt

   LET v_indice = 1

    DECLARE cur_consulta_campos_enc CURSOR FOR prp_consulta_campos_enc
    FOREACH cur_consulta_campos_enc INTO v_cod_proceso_cnt, v_referencia_cnt, v_fecha_emi_man, v_folio_liquida, v_fecha_liquida

      ####################################
      #SE ARMA EL ENCABEZADO DEL ARCHIVO #
      ####################################
      LET indicador = "01"
      --LET fec_docto = TODAY USING "yyyymmdd"
      LET fec_docto = v_fecha_liquida USING "yyyymmdd"
      LET clase_docto = "ZL"
      LET sociedad  = "INFO"
      LET fec_contable = v_fecha_liquida USING "yyyymmdd"  --Se cambia a fecha de liquidación
      LET mes_contable = MONTH(v_fecha_liquida) USING "&&"  --A partir de la fecha de liquidación
      LET moneda = "MXN  "
      --LET fecha_ref = v_fecha_emi_man USING "yymmdd"  --Se cambiará por 000000    
      LET fecha_ref = "000000"
      LET v_folio_proc_cnt_char = v_folio_liquida USING"&&&&&&" --Se cambia a folio de liquidación   
      LET referencia = v_referencia_cnt||fecha_ref||v_folio_proc_cnt_char

      LET v_ins_reg = indicador,fec_docto,clase_docto,sociedad,
                      fec_contable,mes_contable,moneda,referencia

      CALL v_ch_arch_salida.write([v_ins_reg])                                         


          --Consulta campos para DETALLE
          LET v_qry_txt1 = "\n SELECT ct.cta_contable, ct.importe, ct.cod_naturaleza_cta,",
                           --"\n        ct.cod_proceso_cnt, cp.desc_proc_corta_cnt, cp.referencia_cnt ",
                           "\n        ct.cod_proceso_cnt, cp.desc_proc_corta_cnt, cp.referencia_cnt, tpo_transaccion, cod_transaccion_cnt ",
                           "\n   FROM cnt_transaccion ct, cat_proceso_cnt cp",
                           "\n  WHERE ct.cod_proceso_cnt = cp.cod_proceso_cnt",
                           "\n    AND ct.cod_proceso_cnt =",v_cod_proceso_cnt,
                           "\n    AND ct.folio_liquida =",v_folio_liquida,
                           "\n    AND estado = 10",
                           
                           "\n  GROUP BY 1,2,3,4,5,6,7,8",
                           "\n  ORDER BY 1,2,3,4,5,6,7,8"

        PREPARE prp_consulta_campos FROM v_qry_txt1     
        DECLARE cur_consulta_campos CURSOR FOR prp_consulta_campos
        
            FOREACH cur_consulta_campos INTO v_cta_contable, v_importe, v_cod_naturaleza_cta,
                                             v_cod_proceso_cnt, v_desc_proceso_cnt, v_referencia_cnt, v_tipo_transaccion, v_cod_transaccion_cnt

                 LET indicador1 = "02"
                 
                 IF v_cod_naturaleza_cta = 1 THEN 
                    LET cve_contable = 50
                 ELSE 
                    LET cve_contable = 40
                 END IF
                 
                 LET cliente_acr_mayor = v_cta_contable
                 LET monto = v_importe USING "&&&&&&&&&&.&&"
                 LET division = "09"
                 LET v_cod_proceso_cnt_char = v_cod_proceso_cnt USING "&&&"
                 
                 --LET texto_posicion = v_referencia_cnt||fecha_ref||v_folio_proc_cnt_char||v_cod_proceso_cnt_char||v_desc_proceso_cnt

                 --Evalúa si el registro es normal o ya tiene un reverso
                 IF v_tipo_transaccion = 1 THEN -- 1 es reversado
                    LET texto_posicion = "REV-"||fecha_ref||v_folio_proc_cnt_char||v_cod_proceso_cnt_char||v_desc_proceso_cnt
                 ELSE
                    LET texto_posicion = v_referencia_cnt||fecha_ref||v_folio_proc_cnt_char||v_cod_proceso_cnt_char||v_desc_proceso_cnt
                 END IF  

                 ###### Actualización para proceso 20 para la cuenta 2403011600
                 IF v_cod_proceso_cnt = 20 AND v_cod_transaccion_cnt = 12 AND v_cod_naturaleza_cta = 1 THEN 
                     LET texto_posicion = "ER170_"||v_fecha_formato||"_TRASPASO DE SSV"
                 END IF 

                 IF v_cod_proceso_cnt = 20 AND v_cod_transaccion_cnt = 12 AND v_cod_naturaleza_cta = 2 THEN 
                     LET texto_posicion = "ER207_"||v_fecha_formato||"_CARGO A CAPITAL"
                 END IF

                 IF v_cod_proceso_cnt = 20 AND v_cod_transaccion_cnt = 2 AND v_cod_naturaleza_cta = 1 THEN 
                     LET texto_posicion = "ER171_"||v_fecha_formato||"_TRASPASO FONDO AHORRO"
                 END IF

                 ################################################################ 
                 
                 LET v_ins_reg = indicador1,cve_contable,cliente_acr_mayor,
                                 monto,division,texto_posicion

                  CALL v_ch_arch_salida.write([v_ins_reg])

                  LET v_indice = v_indice + 1              
            END FOREACH

                  --Actualiza FOLIO a los campos de la póliza
                     UPDATE cnt_transaccion 
                        SET folio_cnt = v_folio_proc_cnt
                      WHERE folio_liquida = v_folio_liquida
                      AND estado = 10
                        --AND f_emision = TODAY 
                        --AND cod_proceso_cnt = v_cod_proceso_cnt
                        

               --DISPLAY "--Actualiza ESTADO a los campos de la póliza Automático"
                 UPDATE cnt_transaccion 
                    SET estado = 20
                  WHERE folio_liquida = v_folio_liquida
                  AND estado = 10
                    --AND f_emision = TODAY 
                    --AND cod_proceso_cnt = v_cod_proceso_cnt
                    

               LET fec_docto = TODAY USING "mmddyyyy"
               --LET fec_docto = v_fecha_liquida USING "mmddyyyy"

               --DISPLAY "Folio liquidación: ",v_folio_liquida

               SELECT COUNT (*)
               INTO v_control
               FROM cnt_ctr_proceso
               WHERE folio_liquida = v_folio_liquida
               AND folio_cnt = v_folio_proc_cnt
               --AND f_liquida = v_fecha_liquida
               --AND nombre_archivo = v_nom_archivo
               --AND f_emision = fec_docto

               
               
               IF v_control <= 0 THEN 
                     --Inserta un registro en la tabla de control por cada encabezado
                     PREPARE prp_fn_transaccion2_auto
                     FROM "EXECUTE PROCEDURE safre_viv:sp_cnt_Transaccion2(?,?,?,?,?,?)"
                     EXECUTE prp_fn_transaccion2_auto USING  v_folio_proc_cnt,
                                                            v_folio_liquida,
                                                            v_fecha_liquida,
                                                            v_nom_archivo,
                                                            fec_docto,
                                                            v_id_funcion 
               
               END IF 
               
             
                        
                   LET v_indice = v_indice + 1
     END FOREACH 
                                        
END IF 

    --Genera una copia de la póliza contable añadiendo la fecha del día al nombre
    CALL fn_genera_copia_poliza_cnt(v_ruta_nomarch,v_ruta_envio_dis)

        
    CALL fn_rpt_genera_pol(g_usuario, v_folio_proc_cnt, v_nom_archivo,p_b_despliegue_pantalla)

    CALL fn_actualiza_opera_fin(p_pid, g_proceso_cod,g_opera_cod1) RETURNING r_bandera
                  --DISPLAY "Actualiza opera fin -->",r_bandera

         IF r_bandera = 0 THEN 
               DISPLAY "Se generó la póliza contable de manera exitosa." 
               
               DISPLAY "El archivo de copia generado es: ",v_archivo_copia CLIPPED 
               --Actualiza folio contable en la operación y nombre del archivo generado
               UPDATE bat_ctr_operacion
               SET folio = v_folio_proc_cnt, nom_archivo = v_nom_archivo
               WHERE proceso_cod = g_proceso_cod   
               AND  pid = p_pid
               AND opera_cod = g_opera_cod1
         ELSE 
         
               CALL fn_desplega_inc_operacion(r_bandera)

         END IF 

###########################################3
 

END FUNCTION

FUNCTION fn_genera_copia_poliza_cnt(p_archivo_envio,p_ruta_destino)

    DEFINE 
        v_cmd                STRING,
        p_archivo_envio      VARCHAR(100),
        p_ruta_destino       VARCHAR(40)
 

    LET v_archivo_copia = "poliza_cont_",TODAY USING "ddmmyyyy"
    LET v_archivo_copia = v_archivo_copia CLIPPED,".cnt" 
    --DISPLAY "v_archivo_copia -- ",v_archivo_copia
    
    LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED 

    --DISPLAY "v_cmd -- ",v_cmd

    RUN v_cmd

    
    
END FUNCTION 


#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
DEFINE p_ruta_envio_dis     CHAR(40),--LIKE seg_modulo.ruta_envio, 
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(22)
DEFINE ch base.Channel

    LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

    LET ch = base.Channel.create()
    CALL ch.setDelimiter(".")
    CALL ch.openPipe(v_cmd,"r")
    WHILE ch.read([fn])
       LET v_consecutivo = fn[15,16]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo
END FUNCTION

#OBJETIVO: Generar el reporte de la Consulta
FUNCTION fn_rpt_genera_pol(p_usuario, p_folio_cnt, p_nom_archivo,p_b_despliegue_pantalla) 

DEFINE p_usuario CHAR(20),
       v_Qry_Txt STRING,
       p_folio_cnt  LIKE cnt_transaccion.folio_cnt,
       v_indice SMALLINT,
       v_tot_cargo LIKE cnt_transaccion.importe, 
       v_tot_abono LIKE cnt_transaccion.importe,
       v_ruta_listados  STRING, -- ruta de los listados
       v_ruta_ejecutable STRING, -- ruta del ejecutable
       p_b_despliegue_pantalla SMALLINT,  
       v_ruta_rep       STRING,
       v_origen_datos   STRING,
       v_ind_ini        INTEGER,
       v_tot_reg_imp    INTEGER,
       p_nom_archivo    LIKE glo_ctr_archivo.nombre_archivo,
       manejador_rpt    om.SaxDocumentHandler  -- Contenedor documentos reporte
DEFINE arr_fn_reporte DYNAMIC ARRAY OF RECORD
            arr_po_fecha         LIKE cnt_transaccion.f_liquida,
            arr_po_folio_liquida LIKE cnt_transaccion.folio_liquida,
            arr_po_folio         LIKE cnt_transaccion.folio_cnt,
            arr_po_proceso       CHAR(42),
            arr_po_cuenta        LIKE cnt_transaccion.cta_contable,
            arr_po_descripcion   LIKE cat_cuenta_contable.desc_cta_contable,
            arr_po_cargo         LIKE cnt_transaccion.importe,
            arr_po_abono         LIKE cnt_transaccion.importe,
            arr_po_estado        CHAR(27),
            arr_po_cod_pro       LIKE cnt_transaccion.cod_proceso_cnt,
            arr_po_id_cuenta     LIKE cnt_transaccion.id_cuenta_contable      
END RECORD

    SELECT SUM(importe) AS CARGO 
      INTO v_tot_cargo
      FROM cnt_transaccion 
     WHERE cod_naturaleza_cta = 1
       AND folio_cnt = p_folio_cnt
     
    SELECT SUM(importe) AS ABONO
      INTO v_tot_abono
      FROM cnt_transaccion 
     WHERE cod_naturaleza_cta = 2
       AND folio_cnt = p_folio_cnt

    LET v_Qry_Txt =  "\n SELECT  T.f_liquida, ",
                     "\n		   T.folio_liquida, ",
                     "\n		   T.folio_cnt, ",
                     "\n		   T.cod_proceso_cnt || '-' || P.desc_proceso_cnt Proceso, ",
                     "\n		   T.cta_contable, ",
                     "\n		   C.desc_cta_contable, ",
                     "\n		   T.importe , ", --Cargo
                     "\n		   0, ", --Abono
                     "\n		   T.estado || '-' || E.desc_estado_cnt Estado, ",
                     "\n		   T.cod_proceso_cnt, ",
                     "\n		   T.id_cuenta_contable",
                     "\n  FROM    cnt_transaccion T, ",
                     "\n	      cat_proceso_cnt P, ",
                     "\n		   cat_cuenta_contable C, ",
                     "\n		   cat_estado_cnt E ",
                     "\n  WHERE 	P.cod_proceso_cnt = T.cod_proceso_cnt ",
                     "\n			AND C.cta_contable = T.cta_contable ",
                     "\n			AND E.cod_estado_cnt = T.estado ",
                     "\n			AND T.cod_naturaleza_cta = 2 ",
                     "\n           AND T.folio_cnt = ",p_folio_cnt,
                     "\n UNION ",
                     "\n SELECT  T.f_liquida, ",
                     "\n		   T.folio_liquida, ",
                     "\n		   T.folio_cnt, ",
                     "\n		   T.cod_proceso_cnt || '-' || P.desc_proceso_cnt Proceso, ",
                     "\n		   T.cta_contable, ",
                     "\n		   C.desc_cta_contable, ",
                     "\n		   0, ", --Cargo
                     "\n		   T.importe, ", --Abono
                     "\n		   T.estado || '-' || E.desc_estado_cnt Estado, ",
                     "\n		   T.cod_proceso_cnt, ",
                     "\n		   T.id_cuenta_contable",
                     "\n  FROM    cnt_transaccion T, ",
                     "\n	      cat_proceso_cnt P, ",
                     "\n		   cat_cuenta_contable C, ",
                     "\n		   cat_estado_cnt E ",
                     "\n  WHERE 	P.cod_proceso_cnt = T.cod_proceso_cnt ",
                     "\n			AND C.cta_contable = T.cta_contable ",
                     "\n			AND E.cod_estado_cnt = T.estado ",
                     "\n			AND T.cod_naturaleza_cta = 1 ",
                     "\n           AND T.folio_cnt = ",p_folio_cnt,
                     "\n ORDER BY 1,2,11 " --Ordenado por fecha e id de la cuenta

                  
                    
PREPARE prp_qry_rpt FROM v_Qry_Txt
DECLARE cur_qry_rpt CURSOR FOR  prp_qry_rpt

    LET v_origen_datos = g_usuario                  
    -- se construye la ruta donde se creará el reporte PDF
    CALL fn_rutas("cnt") RETURNING v_ruta_ejecutable, v_ruta_listados
    --LET v_ruta_rep = v_ruta_listados.trim(),
                         --"/",
                         --v_origen_datos.trim(),"_",
                         --p_nom_archivo[1,16],".pdf"

    LET v_ruta_rep = v_ruta_listados.trim(),
                         "/",
                         v_origen_datos.trim(),"-",
                         "CNTS01","-",
                         p_pid USING "&&&&&","-",
                         g_proceso_cod USING "&&&&&","-",
                         g_opera_cod1 USING "&&&&&",".pdf"



--DISPLAY "Ruta del reporte -- ", v_ruta_rep

    
  --Se asigna la plantilla para generar el reporte
   IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/CNTS011.4rp") THEN

          CALL fgl_report_selectDevice ("PDF")
          
          CALL fgl_report_selectPreview(0)

          CALL fgl_report_setOutputFileName(v_ruta_rep)
       
          LET manejador_rpt = fgl_report_commitCurrentSettings()

               
             
            LET v_indice = 1

               FOREACH cur_qry_rpt INTO arr_fn_reporte[v_indice].*

                  --Valida que no vengan nulos
                  IF arr_fn_reporte[v_indice].arr_po_cargo IS NULL THEN 
                     LET arr_fn_reporte[v_indice].arr_po_cargo = 0.00
                  END IF 

                  IF arr_fn_reporte[v_indice].arr_po_abono IS NULL THEN 
                     LET arr_fn_reporte[v_indice].arr_po_abono = 0.00
                  END IF 
                     
               
                  LET v_indice = v_indice + 1   
               END FOREACH 

               --Borra el registro vacío del arreglo
            CALL arr_fn_reporte.deleteElement(v_indice)

               
            --Inicia el reporte de registros con rechazo
            START REPORT rpt_genera_poliza TO XML HANDLER manejador_rpt
               FOR v_ind_ini = 1 TO  arr_fn_reporte.getLength()
                  --DISPLAY "Cargo -- ",arr_fn_reporte[v_ind_ini].arr_po_cargo
                  OUTPUT TO REPORT rpt_genera_poliza(arr_fn_reporte[v_ind_ini].*,
                                                     p_usuario, p_folio_cnt,
                                                     v_tot_cargo, v_tot_abono)
                  --LET v_ind_ini = v_ind_ini + 1 
               END FOR    
            FINISH REPORT rpt_genera_poliza
       
    ELSE
               DISPLAY "Error al generar el reporte"

               CALL fn_error_opera(p_pid,g_proceso_cod,g_opera_cod1)
                      RETURNING v_indice
               
               --EXIT PROGRAM
   END IF
   
   
END FUNCTION

#OBJETIVO: Se arma la estructura del reporte 
REPORT rpt_genera_poliza(arr_fn_reporte, p_usuario1, p_folio_cnt,
                         p_tot_cargo, p_tot_abono)

DEFINE arr_fn_reporte RECORD          
         arr_po_fecha         LIKE cnt_transaccion.f_liquida,
         arr_po_folio_liquida LIKE cnt_transaccion.folio_liquida,
         arr_po_folio         CHAR (10),
         arr_po_proceso       CHAR(42),
         arr_po_cuenta        LIKE cnt_transaccion.cta_contable,
         arr_po_descripcion   LIKE cat_cuenta_contable.desc_cta_contable,
         arr_po_cargo         LIKE cnt_transaccion.importe,
         arr_po_abono         LIKE cnt_transaccion.importe,
         arr_po_estado        CHAR(27),
         arr_po_cod_pro       LIKE cnt_transaccion.cod_proceso_cnt,
         arr_po_id_cuenta       LIKE cnt_transaccion.id_cuenta_contable     
END RECORD

DEFINE v_fecha_reporte DATE,
       p_usuario1 LIKE seg_modulo.usuario,
       p_folio_cnt CHAR(9),--LIKE cnt_transaccion.folio_cnt,
       v_fecha_proceso DATE,
       p_tot_cargo LIKE cnt_transaccion.importe, 
       p_tot_abono LIKE cnt_transaccion.importe

FORMAT

   FIRST PAGE HEADER

      LET v_fecha_reporte = TODAY 
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      LET v_fecha_proceso = TODAY 
      PRINTX v_fecha_proceso USING "dd-mm-yyyy"
      PRINTX p_usuario1
      PRINTX p_folio_cnt

   ON EVERY ROW 
      PRINTX arr_fn_reporte.arr_po_fecha USING "dd-mm-yyyy" 
      PRINTX arr_fn_reporte.arr_po_folio
      PRINTX arr_fn_reporte.arr_po_folio_liquida
      PRINTX arr_fn_reporte.arr_po_proceso
      PRINTX arr_fn_reporte.arr_po_estado
      PRINTX arr_fn_reporte.arr_po_cuenta
      PRINTX arr_fn_reporte.arr_po_descripcion
      PRINTX arr_fn_reporte.arr_po_cargo
      PRINTX arr_fn_reporte.arr_po_abono

   ON LAST ROW
      PRINTX p_tot_cargo
      PRINTX p_tot_abono
END REPORT