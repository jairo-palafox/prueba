--===============================================================
-- VERSION: 1.0.0
--===============================================================

--------------------------------------------------------------------------------
-- Modulo       => TIA
-- Programa     => TIAC14
-- Objetivo     => Consulta de AIVS en cero
-- Fecha inicio => 02 de Febrero de 2016
-- Autor        => Carlos Benitez Cabriales
-------------------------------------------------------------------------------- 
IMPORT os
DATABASE safre_viv

DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       p_usuario_cod     LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion  SMALLINT, # forma como ejecutara el programa
       p_titulo_ventana  STRING,
       v_c_ruta_env_acr LIKE seg_modulo.ruta_envio

DEFINE  v_ch_arch_solTransf             BASE.CHANNEL,
               v_continua_pantalla SMALLINT,
                  v_v_ruta_nomarch STRING

MAIN 

            # se recuperan los argumentos de la linea de comandos
       LET p_usuario_cod    = ARG_VAL(1)
       LET p_tipo_ejecucion = ARG_VAL(2)
       LET p_titulo_ventana = ARG_VAL(3)

       -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo_ventana)
   END IF

        SELECT ruta_envio,ruta_listados,ruta_bin
         INTO  v_c_ruta_env_acr,v_ruta_listados,v_ruta_ejecutable
         FROM seg_modulo
        WHERE modulo_cod = "tia"

        OPEN WINDOW w_consulta_tia  WITH FORM "TIAC141"

              CALL fn_consulta_aivs() 
        CLOSE WINDOW w_consulta_tia

END MAIN
--Función que realiza el query principal, AIVS en cero
FUNCTION fn_consulta_aivs()
     DEFINE    v_registros   RECORD
                      folio LIKE tia_det_traspaso.folio,
                      nss          LIKE tia_det_traspaso.nss_afo_recep,
                      curp         LIKE tia_det_traspaso.curp,
                      nombre       LIKE tia_det_traspaso.nombres_afo_recep,
                      paterno      LIKE tia_det_traspaso.paterno_afo_recep,
                      materno      LIKE tia_det_traspaso.materno_afo_recep,
                      niu          LIKE afi_decreto.consec_cuenta,
                      sdo_viv      LIKE tia_det_traspaso.sdo_viv92,
                      aivs_viv     LIKE tia_det_traspaso.aivs_viv92,
                      result_operacion     CHAR(50)
                      
    END RECORD
    DEFINE v_sql_query STRING,
                  v_contruct   STRING,
                  v_cadena STRING,
                  v_existe_registro SMALLINT,
                  v_ruta_reporte  STRING,
                  v_manejador_rpt om.SaxDocumentHandler,
                  v_folio_string STRING,
                  v_aivs_viv_string STRING,
                  v_sdo_viv_string STRING,
                  v_sql_query_existe STRING,
                  v_nombre_reporte STRING,
                  v_ruta_listados_string STRING,
                  v_estado SMALLINT,
                  v_folio STRING,
                  v_f_presentacion STRING
                  
                  
                  

    CONSTRUCT v_contruct ON tia.folio,tia.f_presentacion
    FROM folio,f_presentacion

        ON ACTION ACCEPT

                LET v_folio=GET_FLDBUF(folio)
                LET v_f_presentacion=GET_FLDBUF(f_presentacion)
                
               IF v_folio IS NULL AND v_f_presentacion IS NULL THEN
                    LET v_folio="1=1"
               ELSE
                    IF v_f_presentacion IS NOT NULL AND v_folio IS NULL THEN
                        LET v_folio=v_f_presentacion
                    ELSE
                        LET v_folio=v_folio,"\n",v_f_presentacion
                    END IF
               END IF
           
        ACCEPT CONSTRUCT

        ON ACTION CANCEL
        EXIT PROGRAM

    END CONSTRUCT

    LET v_sql_query= "\n SELECT tia.folio,",
                                    "\n        tia.nss_afo_recep, ",
                                    "\n        tia.curp,",
                                    "\n        tia.nombres_afo_recep,",
                                    "\n        tia.paterno_afo_recep,",
                                    "\n        tia.materno_afo_recep,",
                                    "\n        afi.consec_cuenta,",
                                    "\n        tia.sdo_viv92,",
                                    "\n        tia.aivs_viv92, ",
                                     "\n       CASE tia.result_operacion ",
                                     "\n       WHEN '05' THEN '5 AIV EN CERO, NO SE LIQUIDARA' ",
                                     "\n        ELSE  '3 MTO SOLIC EN AIVS NO ENCONTRADO' ",
                                     "\n        END CASE",
                                    "\n   FROM tia_det_traspaso tia LEFT OUTER JOIN afi_decreto afi",
                                    "\n     ON tia.id_decreto = afi.id_decreto",
                                    "\n  WHERE ",v_contruct,
                                    "\n    AND tia.result_operacion in ('03','05') ",
                                    "\n  ORDER BY tia.result_operacion "

                                    DISPLAY v_sql_query

    PREPARE smt_sql_query FROM v_sql_query
    DECLARE cur_aivs_cero CURSOR FOR smt_sql_query

   -- LET v_existe_registro=0

    LET v_sql_query_existe="SELECT COUNT(*)  FROM TIA_DET_TRASPASO TIA  WHERE RESULT_OPERACION IN ('03','05') AND ",v_contruct
    
    PREPARE smt_sql_query_existe FROM v_sql_query_existe

    EXECUTE smt_sql_query_existe INTO v_existe_registro
    
    IF v_existe_registro==0 THEN
        CALL fn_mensaje("Atención","No existen registros con el \n criterio de búsqueda","stop")
        CALL fn_consulta_aivs()
    END IF

    CALL fn_inicia_reporte_txt(v_folio,v_registros.*) RETURNING v_estado
        IF v_estado THEN
            CALL fn_mensaje("Atención","Hubo un error al crear el archivo TXT.","stop")
        ELSE 
            CALL fn_mensaje("Atención","El reporte TXT se generó correctamente.","information")
        END IF

    CALL fn_inicia_reporte_pdf(v_folio,v_registros.*) RETURNING v_estado
        IF v_estado THEN
            CALL fn_mensaje("Atención","Hubo un error al generar el reporte PDF.","stop")
        ELSE 
            CALL fn_mensaje("Atención","El reporte PDF se generó correctamente.","information")
        END IF

    CALL fn_consulta_aivs()


END FUNCTION

--Función que genera el reporte en formato TXT
FUNCTION fn_inicia_reporte_txt(p_folio,v_registros)
    DEFINE v_v_nom_archi STRING,
                  p_folio STRING
                 

    DEFINE    v_registros   RECORD
                      folio LIKE tia_det_traspaso.folio,
                      nss          LIKE tia_det_traspaso.nss_afo_recep,
                      curp         LIKE tia_det_traspaso.curp,
                      nombre       LIKE tia_det_traspaso.nombres_afo_recep,
                      paterno      LIKE tia_det_traspaso.paterno_afo_recep,
                      materno      LIKE tia_det_traspaso.materno_afo_recep,
                      niu          LIKE afi_decreto.consec_cuenta,
                      sdo_viv      LIKE tia_det_traspaso.sdo_viv92,
                      aivs_viv     LIKE tia_det_traspaso.aivs_viv92,
                      result_operacion     CHAR(50)
    END RECORD

    DEFINE v_sql_query STRING,
                  v_contruct   STRING,
                  v_cadena STRING,
                  v_existe_registro SMALLINT,
                  v_ruta_reporte  STRING,
                  v_manejador_rpt om.SaxDocumentHandler,
                  v_aivs_viv_string STRING,
                  v_sdo_viv_string STRING,
                  v_sql_query_existe STRING,
                  v_nombre_reporte STRING,
                  v_ruta_listados_string STRING,
                   v_estado SMALLINT
                  
                    --==============Se agregan lineas para escribir en archivo de texto CABC=================
        LET v_v_nom_archi="Reporte_AIVS_En_cero",TODAY USING "ddmmyyyy"

       LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi||".tia"
  


       --DISPLAY v_v_ruta_nomarch
       -- se crea el manejador de archivo
       LET v_ch_arch_solTransf = base.Channel.create()
       LET v_estado=0

       TRY 
            -- se crea archivo y se indica que se escribira en el mismo
           CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
           CALL v_ch_arch_solTransf.setDelimiter("")
           CALL v_ch_arch_solTransf.write("Folio|"||"NSS|"||"CURP|"||"NIU|"||"Ap. Paterno|"
                                                                    ||"Ap. Materno|"||"Nombre|"||"AIVS|"||"SDO VIV 92|"||"Resultado")


               FOREACH cur_aivs_cero INTO v_registros.*
                LET v_aivs_viv_string=v_registros.aivs_viv
                LET v_sdo_viv_string=v_registros.sdo_viv
                
                LET v_cadena=v_registros.folio CLIPPED,"|",
                                            v_registros.nss CLIPPED,"|",
                                            v_registros.curp CLIPPED,"|",
                                            v_registros.niu CLIPPED,"|",
                                            v_registros.paterno CLIPPED,"|",
                                            v_registros.materno CLIPPED,"|",
                                            v_registros.nombre CLIPPED,"|",
                                            v_aivs_viv_string CLIPPED,"|",
                                            v_sdo_viv_string CLIPPED,"|",
                                            v_registros.result_operacion

                CALL v_ch_arch_solTransf.writeLine(v_cadena) 
            END FOREACH
           CALL v_ch_arch_solTransf.close()
       CATCH
            LET v_estado=1
       END TRY      
       
       RETURN v_estado
           --==========================================================================
END FUNCTION
--Función que crea el reporte
FUNCTION fn_inicia_reporte_pdf(p_folio,v_registros)
     DEFINE v_v_nom_archi STRING,
                  p_folio STRING
                 

    DEFINE    v_registros   RECORD
                      folio  LIKE tia_det_traspaso.folio,
                      nss          LIKE tia_det_traspaso.nss_afo_recep,
                      curp         LIKE tia_det_traspaso.curp,
                      nombre       LIKE tia_det_traspaso.nombres_afo_recep,
                      paterno      LIKE tia_det_traspaso.paterno_afo_recep,
                      materno      LIKE tia_det_traspaso.materno_afo_recep,
                      niu          LIKE afi_decreto.consec_cuenta,
                      sdo_viv      LIKE tia_det_traspaso.sdo_viv92,
                      aivs_viv     LIKE tia_det_traspaso.aivs_viv92,
                      result_operacion     CHAR(50)
    END RECORD

    DEFINE v_sql_query STRING,
                  v_contruct   STRING,
                  v_cadena STRING,
                  v_existe_registro SMALLINT,
                  v_ruta_reporte  STRING,
                  v_manejador_rpt om.SaxDocumentHandler,
                  v_aivs_viv_string STRING,
                  v_sdo_viv_string STRING,
                  v_sql_query_existe STRING,
                  v_nombre_reporte STRING,
                  v_ruta_listados_string STRING,
                   v_estado SMALLINT,
                   v_cuenta_registro SMALLINT
                  



                   IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/TIAC14.4rp") THEN 

                        CALL fgl_report_selectDevice ("PDF")
                        LET v_nombre_reporte=p_usuario_cod CLIPPED, "-",
                                                 "TIAC14-", 
                                                 p_folio USING "&&&&&&&"

        
         
        LET v_ruta_listados_string=v_ruta_listados
        LET v_ruta_listados_string=v_ruta_listados_string CLIPPED
                      
              --LET v_ruta_reporte = v_ruta_listados CLIPPED,"/",v_nombre_reporte
              --DISPLAY "Ruta de reporte: ", v_ruta_reporte   
             --DISPLAY  v_ruta_listados_string||"/"||v_nombre_reporte||"HELLO"
              CALL fgl_report_setOutputFileName(v_ruta_listados_string||"/"||v_nombre_reporte CLIPPED)
              CALL fgl_report_selectPreview(1)
              LET v_manejador_rpt = fgl_report_commitCurrentSettings()

              LET v_estado=0

            START REPORT fn_genera_rpt_insidencias TO XML HANDLER v_manejador_rpt
            LET v_cuenta_registro=0
            FOREACH cur_aivs_cero INTO v_registros.* 
                OUTPUT TO REPORT fn_genera_rpt_insidencias(p_folio, v_registros.*)
                LET v_cuenta_registro=v_cuenta_registro+1
            END FOREACH

            IF v_cuenta_registro==0 THEN
                OUTPUT TO REPORT fn_genera_rpt_insidencias(p_folio, v_registros.*)
            END IF

            FINISH REPORT fn_genera_rpt_insidencias

            ELSE
                LET v_estado=2

            END IF

        RETURN v_estado
  
END FUNCTION

--Función que imprime datos al reporte
REPORT fn_genera_rpt_insidencias(p_folio, p_registros)
DEFINE p_folio STRING
   DEFINE p_registros     RECORD
          folio LIKE tia_det_traspaso.folio,
          nss          LIKE tia_det_traspaso.nss_afo_recep,
          curp         LIKE tia_det_traspaso.curp,
          nombre       LIKE tia_det_traspaso.nombres_afo_recep,
          paterno      LIKE tia_det_traspaso.paterno_afo_recep,
          materno      LIKE tia_det_traspaso.materno_afo_recep,
          niu          LIKE afi_decreto.consec_cuenta,
          sdo_viv      LIKE tia_det_traspaso.sdo_viv92,
          aivs_viv     LIKE tia_det_traspaso.aivs_viv92,
          resultado    VARCHAR(40)
       END RECORD,
       p_nombre_archivo      LIKE glo_ctr_archivo.nombre_archivo,
       v_fecha               STRING, -- fecha de emision del reporte
       v_nombre_usuario      VARCHAR(100),
       v_f_presentacion      LIKE tia_cza_traspaso.f_presentacion,
       v_cadena              STRING, -- cadena auxiliar
       v_periodo_archivo     STRING -- para enviar el periodo del archivo
          
FORMAT

   FIRST PAGE HEADER
     
      # se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      
      # se obtiene el nombre del usuario
      SELECT usuario_desc
        INTO v_nombre_usuario
        FROM seg_usuario
       WHERE usuario_cod = p_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED


      DISPLAY p_folio, p_usuario_cod, v_fecha, v_nombre_usuario, v_periodo_archivo, p_nombre_archivo
      PRINTX p_folio, p_usuario_cod, v_fecha, v_nombre_usuario, v_periodo_archivo, p_nombre_archivo

   PAGE HEADER
      PRINTX p_folio, p_usuario_cod, v_fecha, v_nombre_usuario
      
   ON EVERY ROW
      PRINTX p_registros.*
    

END REPORT
