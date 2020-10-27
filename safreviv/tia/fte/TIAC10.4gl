--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14-05-2013
--==============================================================================
################################################################################
#Modulo       => TIA                                                           #
#Programa     => TIAC10                                                        #
#Objetivo     => Consulta Incidencias TIA                                      #
#Fecha inicio => 14 Mayo 2013                                                  #
################################################################################
IMPORT os
DATABASE safre_viv
GLOBALS "TIAG01.4gl" # archivo de variables globales proceso_cod, opera_cod

DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       p_usuario_cod     LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion  SMALLINT, # forma como ejecutara el programa
       p_titulo_ventana  STRING,
       v_c_ruta_env_acr LIKE seg_modulo.ruta_envio
          
MAIN

   # se recuperan los argumentos de la linea de comandos
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)

   SELECT ruta_bin,
          ruta_listados,
          ruta_envio
     INTO v_ruta_ejecutable,
          v_ruta_listados,
          v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = "tia"

   CALL fn_obtiene_folio()

END MAIN

{ ======================================================================
Clave: TIAC10
Nombre: fn_obtiene_folio
Fecha creacion: 14 Mayo 2013
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================}
FUNCTION fn_obtiene_folio()
DEFINE v_folio       LIKE glo_folio.folio,
       v_combo_folio ui.ComboBox,
       r_estado      SMALLINT,
       v_mensaje     STRING,
       v_ventana     ui.Window,
       v_nombre_reporte STRING,
       v_v_ruta_nomarch STRING

   OPEN WINDOW vtna_obtiene_folio WITH FORM v_ruta_ejecutable CLIPPED ||"/TIAC101"
   
      LET v_ventana = ui.Window.getCurrent()
      # si se obtuvo el titulo, se pone como titulo de programa
      IF ( p_titulo_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_titulo_ventana)
         CALL v_ventana.setText(p_titulo_ventana)
      END IF

      INPUT v_folio WITHOUT DEFAULTS FROM folio 
      ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE INPUT 
            LET v_combo_folio = ui.ComboBox.forName("folio")
            CALL fn_llena_combo_folio(v_combo_folio)

         ON ACTION ACEPTAR
            IF ( v_folio IS NOT NULL ) THEN
               CALL fn_recupera_registros(v_folio) RETURNING r_estado, v_nombre_reporte,v_v_ruta_nomarch
               LET v_mensaje="Reporte  PDF generado correctamente"
               CASE r_estado
                  WHEN 1
                     LET v_mensaje = "No se encontraron registros para el folio ",v_folio CLIPPED
                  WHEN 2
                     LET v_mensaje = "No se pudo generar el reporte"
               END CASE
               
               CALL fn_mensaje("Aviso",v_mensaje,"information")
            ELSE
               CALL fn_mensaje("Atención","Debe capturar un folio","stop")
               CONTINUE INPUT
            END IF

         ON ACTION CANCELAR
            EXIT INPUT

      END INPUT


   CLOSE WINDOW vtna_obtiene_folio

END FUNCTION

{ =======================================================================
Clave: TIAC10
Nombre: fn_llena_combo_folio
Fecha creacion: 14 Mayo 2013
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================}
FUNCTION fn_llena_combo_folio(p_combo_folio)
DEFINE p_combo_folio  ui.ComboBox,
       v_consulta     STRING,
       v_folio        LIKE glo_folio.folio,
       v_cadena_folio STRING

   CALL p_combo_folio.clear()

   LET v_consulta = "\n SELECT folio",
                    "\n   FROM glo_folio",
                    "\n  WHERE proceso_cod = ?",
                    "\n    AND status IN (1, 2)", # preliquidado o liquidado
                    "\n  ORDER BY folio DESC"
   PREPARE prp_recupera_folios FROM v_consulta
   DECLARE cur_recupera_folios CURSOR FOR prp_recupera_folios

   FOREACH cur_recupera_folios USING g_proceso_cod_tia
                                INTO v_folio
      LET v_cadena_folio = v_folio CLIPPED
      CALL p_combo_folio.addItem(v_folio,v_cadena_folio)

   END FOREACH
   FREE cur_recupera_folios
   

END FUNCTION

{ ======================================================================
Clave: TIAC10
Nombre: fn_recupera_registros
Fecha creacion: 14 Mayo 2013
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================}
FUNCTION fn_recupera_registros(p_folio)
DEFINE p_folio         LIKE glo_folio.folio,
       v_consulta      STRING,
       v_registros     DYNAMIC ARRAY OF RECORD
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
       vr_registros   RECORD
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
       v_indice        INTEGER,
       v_ruta_reporte  STRING,
       v_manejador_rpt om.SaxDocumentHandler,
       v_estado        SMALLINT
       DEFINE v_ch_arch_solTransf             BASE.CHANNEL,
                     v_v_ruta_nomarch STRING,
                     v_v_nom_archi        STRING,
                     v_cadena_txt           STRING,
                     v_aivs_viv_string STRING,
                     v_sdo_viv_string STRING,
                     v_ban_crea_txt SMALLINT

   LET v_indice = 1
   LET v_estado = 0 # Se generó correctamente el reporte
   LET v_consulta = "\n SELECT tia.nss_afo_recep,",
                    "\n        tia.curp,",
                    "\n        tia.nombres_afo_recep,",
                    "\n        tia.paterno_afo_recep,",
                    "\n        tia.materno_afo_recep,",
                    "\n        afi.consec_cuenta,",
                    "\n        tia.sdo_viv92,",
                    "\n        tia.aivs_viv92,",
                    "\n        CASE tia.result_operacion",
                    "\n          WHEN '02' THEN '2 SIN CONSECUTIVO ÚNICO'",
                    "\n          WHEN '03' THEN '3 MTO SOLIC EN AIVS NO ENCONTRADO'",
                    "\n          WHEN '04' THEN '4 NSS AFORE RECEPTORA NO ENCONTRADO EN AFI'",
                    "\n          WHEN '05' THEN '5 AIV EN CERO, NO SE LIQUIDADA'",                    
                    "\n          WHEN '08' THEN '8 SIN CURP EN AFILIATORIO'",
                    "\n          WHEN '10' THEN '10 AIV EN HISTÓRICO, EN ESPERA AUTORIZAR'",                    
                    "\n          ELSE '11 SN'",
                    "\n        END CASE",
                    "\n   FROM tia_det_traspaso tia LEFT OUTER JOIN afi_decreto afi",
                    "\n     ON tia.id_decreto = afi.id_decreto",
                    "\n  WHERE tia.folio = ?",
                    "\n    AND tia.result_operacion not in ('01','99') ",
                    "\n  ORDER BY tia.result_operacion "
                    
   PREPARE prp_recupera_registros FROM v_consulta
   DECLARE cur_recupera_registros CURSOR FOR prp_recupera_registros
   FOREACH cur_recupera_registros USING p_folio
                                   INTO vr_registros.*

      -- se transfieren los registros al arreglo de despliegue
      LET v_registros[v_indice].* = vr_registros.*

      -- se incrementa el indice
      LET v_indice = v_indice + 1

   END FOREACH

   FREE cur_recupera_registros

   IF ( v_registros.getLength() = 0 ) THEN
      LET v_estado = 1 # no se recuperaron registros para el folio
      RETURN v_estado, v_ruta_reporte
   END IF


         --==============Se agregan lineas para escribir en archivo de texto CABC=================
         LET v_v_nom_archi="Incidencias"||p_folio


       LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi||".tia"

       --DISPLAY v_v_ruta_nomarch
       -- se crea el manejador de archivo
       LET v_ch_arch_solTransf = base.Channel.create()

       TRY
            -- se crea archivo y se indica que se escribira en el mismo
       CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
       CALL v_ch_arch_solTransf.setDelimiter("")

       CALL v_ch_arch_solTransf.write("Folio|"||"NSS|"||"CURP|"||"NIU|"||"Ap. Paterno|"
                                                                    ||"Ap. Materno|"||"Nombre|"||"AIVS|"||"SDO VIV 92|"||"Resultado")

        FOR v_indice = 1 TO v_registros.getLength()
             LET v_aivs_viv_string=v_registros[v_indice].aivs_viv
             LET v_sdo_viv_string=v_registros[v_indice].sdo_viv

                 LET v_cadena_txt=p_folio ,"|",
                                                                 v_registros[v_indice].nss CLIPPED,"|",
                                                                 v_registros[v_indice].curp CLIPPED,"|",
                                                                 v_registros[v_indice].niu ,"|",
                                                                 v_registros[v_indice].paterno CLIPPED,"|",
                                                                 v_registros[v_indice].materno CLIPPED,"|",
                                                                 v_registros[v_indice].nombre CLIPPED,"|",
                                                                 v_aivs_viv_string CLIPPED  ,"|",
                                                                 v_sdo_viv_string CLIPPED,"|",
                                                                 v_registros[v_indice].resultado CLIPPED
                CALL v_cadena_txt.trim() RETURNING v_cadena_txt
                CALL v_ch_arch_solTransf.writeLine(v_cadena_txt)--Lineas agregadas para escribir en archivo de etxto CABC
            END FOR
            CALL v_ch_arch_solTransf.close()

               IF os.Path.exists(v_v_ruta_nomarch) THEN
                    CALL fn_mensaje("Atención","Reporte texto generado correctamente","stop")
               ELSE 
                    CALL fn_mensaje("Atención","Hubo un error al crear el archivo TXT.","stop")
               END IF
                
            
       CATCH
           CALL fn_mensaje("Atención","Hubo un error al crear el archivo TXT.","stop")
       END TRY
          --==========================================================================       
      

   # Genera reporte
   IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/TIAC10.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
                      
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","incidencias_tia.pdf"
      DISPLAY "Ruta de reporte: ", v_ruta_reporte    
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()


      START REPORT fn_genera_rpt_insidencias TO XML HANDLER v_manejador_rpt

         FOR v_indice = 1 TO v_registros.getLength()
                             OUTPUT TO REPORT fn_genera_rpt_insidencias(p_folio, v_registros[v_indice].*)
         END FOR

         
         -- se finaliza el reporte
      FINISH REPORT fn_genera_rpt_insidencias
   ELSE     
      LET v_estado = 2 # no se pudo generar reporte
      DISPLAY "No fue posible generar el reporte. No se encuentra la plantilla TIAC10.4rp"
      RETURN v_estado, v_ruta_reporte
   END IF

   RETURN v_estado, v_ruta_reporte,v_v_ruta_nomarch
END FUNCTION
 

{ ======================================================================
Clave: TIAC10
Nombre: fn_genera_rpt_insidencias
Fecha creacion: 14 Mayo 2013
Narrativa del proceso que realiza:
Genera el reporte de tia

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT fn_genera_rpt_insidencias(p_folio, p_registros)
DEFINE p_folio         LIKE glo_folio.folio,
       p_registros     RECORD
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

      -- el periodo es mes y ano de la fecha de presentacion
      SELECT f_presentacion
      INTO   v_f_presentacion
      FROM   tia_cza_traspaso
      WHERE  folio = p_folio

      -- ano a 4 cifras
      LET v_cadena = YEAR(v_f_presentacion) USING "&&&&"
      LET v_periodo_archivo = fn_mes_texto(MONTH(v_f_presentacion)), " ", v_cadena 

      -- se obtiene el nombre del archivo
      SELECT nombre_archivo
      INTO   p_nombre_archivo
      FROM   glo_ctr_archivo
      WHERE  proceso_cod = 1701
      AND    folio = p_folio

      DISPLAY p_folio, p_usuario_cod, v_fecha, v_nombre_usuario, v_periodo_archivo, p_nombre_archivo
      PRINTX p_folio, p_usuario_cod, v_fecha, v_nombre_usuario, v_periodo_archivo, p_nombre_archivo

   PAGE HEADER
      PRINTX p_folio, p_usuario_cod, v_fecha, v_nombre_usuario
      
   ON EVERY ROW
      PRINTX p_registros.*
    
END REPORT


FUNCTION fn_mes_texto(p_mes)
DEFINE p_mes       SMALLINT,
       v_mes_texto STRING

   -- se conforma la palabra del mes
   CASE p_mes

      WHEN 1
         LET v_mes_texto = "Enero"

      WHEN 2
         LET v_mes_texto = "Febrero"

      WHEN 3
         LET v_mes_texto = "Marzo"

      WHEN 4
         LET v_mes_texto = "Abril"

      WHEN 5
         LET v_mes_texto = "Mayo"

      WHEN 6
         LET v_mes_texto = "Junio"

      WHEN 7
         LET v_mes_texto = "Julio"

      WHEN 8
         LET v_mes_texto = "Agosto"

      WHEN 9
         LET v_mes_texto = "Septiembre"

      WHEN 10
         LET v_mes_texto = "Octubre"

      WHEN 11
         LET v_mes_texto = "Noviembre"

      WHEN 12
         LET v_mes_texto = "Diciembre"

   END CASE

   RETURN v_mes_texto
END FUNCTION