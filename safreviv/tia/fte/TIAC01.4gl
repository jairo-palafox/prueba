--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => TIA                                                                    #
#Programa     => TIAC01                                                                 #
#Objetivo     => Consulta Totales de archivo de Traspasos I-A                           #
#Fecha inicio => Marzo 26, 2012                                                         # 
#Autor        => Ilhuitemoc Ricardo Ortiz                                               #
#########################################################################################
IMPORT os
DATABASE safre_viv

GLOBALS
    DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
END GLOBALS 
DEFINE v_bandera_reportes SMALLINT

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida LQINFO
   CALL fn_consulta_folio(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: TIAC01
Nombre: fn_consulta_folio
Fecha creacion: Marzo 26, 2012
Autor: Ilhuitemoc Ricardo Ortiz
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_folio(p_usuario_cod)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio                  DECIMAL(9,0), -- folio
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_s_cadena               STRING, -- cadena de texto
       v_r_glo_folio            RECORD LIKE glo_folio.*

   OPEN WINDOW w_consulta_folio WITH FORM "TIAC011"
   
   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()

   
   INPUT v_folio WITHOUT DEFAULTS
   FROM cmb_folio ATTRIBUTES ( UNBUFFERED )
  
      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio = NULL
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a,
                tia_sum_traspaso b --acl_sum_sc_nss
         WHERE  a.proceso_cod = 1701 -- Traspasos Infonavit Afore
         AND    a.folio       = b.folio
         ORDER  BY a.folio

         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
        
      ON ACTION ACCEPT
         -- se envian los parametros de consulta
         IF ( v_folio IS NOT NULL ) THEN
            CALL fn_consulta_archivo(v_folio) 
         ELSE
            -- se debe elegir un folio
            CALL fn_mensaje("Atención", "Debe elegir un folio para consultar","stop")
            CONTINUE INPUT
         END IF
         EXIT INPUT
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_folio

END FUNCTION

{
======================================================================
Clave: TIAC01
Nombre: fn_consulta_archivo
Fecha creacion: Marzo 07, 2012
Autor: Ilhuitemoc Ricardo Ortiz
Narrativa del proceso que realiza:
Muestra en pantalla el resultado de la consulta de cifras de control de carga de archivo
de Traspasos Infonavit Afore

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      01Abr2014              - Se corrige el numero de cifras decimales de los campos
                                        que contienen AIVs a 6 cifras
======================================================================
}
FUNCTION fn_consulta_archivo(p_folio)
DEFINE p_folio              DECIMAL(9,0),
       vindice              SMALLINT,
       reg_cifras               RECORD
         registros_detalle       LIKE tia_sum_traspaso.registros_detalle
         ,tot_aivs               LIKE tia_sum_traspaso.tot_aivs
         ,tot_sdo_viv92          LIKE tia_sum_traspaso.tot_sdo_viv92
       END RECORD

   -- se obtienen los datos del sumario cargado para el folio elegido
   SELECT registros_detalle
          ,tot_aivs
          ,tot_sdo_viv92
   INTO   reg_cifras.*
   FROM   tia_sum_traspaso
   WHERE  folio = p_folio

   --LET reg_cifras.tot_aivs=NULL

   -- se inicia el contador
   LET vindice = 1

   -- se abre la ventana de consulta de datos
   OPEN WINDOW w_consulta WITH FORM "TIAC012"

   -- se muestran los datos en pantalla
   DISPLAY p_folio TO num_folio
   DISPLAY "reg_cifras.registros_detalle: ",reg_cifras.registros_detalle
   DISPLAY "reg_cifras.tot_sdo_viv92: ",reg_cifras.tot_sdo_viv92
   DISPLAY "reg_cifras.tot_aivs: ",reg_cifras.tot_aivs
   DISPLAY BY NAME reg_cifras.*

   -- se presentan las opciones para salir o emitir reporte
   MENU
      COMMAND "Aceptar"
         EXIT MENU

      COMMAND "Reporte"
          LET v_bandera_reportes=0
          CALL fn_genera_reporte_txt(reg_cifras.*,p_folio)
          CALL fn_reporte_carga_archivo(p_folio, reg_cifras.*, TRUE)
          IF v_bandera_reportes==0 THEN
                CALL fn_mensaje("Atención","El reporte se generó correctamente.","stop")
          END IF
   END MENU
   
   CLOSE WINDOW w_consulta

END FUNCTION

{=========================================
Nombre: fn_genera_reporte_txt
Fecha creacion: Febrero 02, 2016
Autor: Carlos Benitez
Narrativa del proceso que realiza:
Genera el reporte en formato TXT  
=========================================}

FUNCTION fn_genera_reporte_txt(reg_cifras,v_folio)
    DEFINE reg_cifras               RECORD
         registros_detalle       LIKE tia_sum_traspaso.registros_detalle
         ,tot_aivs               LIKE tia_sum_traspaso.tot_aivs
         ,tot_sdo_viv92          LIKE tia_sum_traspaso.tot_sdo_viv92
       END RECORD
    DEFINE v_c_ruta_env_acr LIKE seg_modulo.ruta_envio,
                  v_v_ruta_nomarch STRING,
                  v_v_nom_archi        STRING,
                  v_folio                       LIKE glo_folio.folio,
                  v_ch_arch_solTransf             BASE.CHANNEL,
                  v_cadena STRING,
                  v_folio_string STRING,
                  v_total_string STRING,
                  v_total_aivs STRING,
                  v_total_sdo STRING

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
   INTO   v_c_ruta_env_acr
   FROM   seg_modulo
   WHERE  modulo_cod = 'tia'

   LET v_v_nom_archi="CifrasCarga"||v_folio


   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi||".tia"

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   LET v_cadena="Folio|",
                               "Total de Registros|",
                               "Tot AIVS VIV 92|",
                               "Tot SDO VIV 92 "
    LET v_folio_string=v_folio
    LET v_total_string=reg_cifras.registros_detalle
    LET v_total_aivs=reg_cifras.tot_aivs
    LET v_total_sdo=reg_cifras.tot_sdo_viv92

   CALL v_ch_arch_solTransf.writeLine(v_cadena)

   LET v_cadena= NULL

   LET v_cadena=v_folio_string CLIPPED ,"|",
                              v_total_string CLIPPED,"|",
                              v_total_aivs CLIPPED,"|",
                              v_total_sdo CLIPPED

    LET v_cadena=v_cadena CLIPPED
   
   CALL v_ch_arch_solTransf.writeLine(v_cadena)
    
   CALL v_ch_arch_solTransf.close()

   IF NOT os.Path.exists(v_v_ruta_nomarch) THEN
        LET v_bandera_reportes=1
        CALL fn_mensaje("Atención","Hubo un error al crear el archivo.","stop")
   END IF--
END FUNCTION 




{
======================================================================
Nombre: fn_reporte_carga_archivo
Fecha creacion: Marzo 07, 2012
Autor: Ilhuitemoc Ricardo Ortiz
Narrativa del proceso que realiza:
Genera el reporte en formato PDF de cifras de control de carga de archivo
de Traspasos Infonavit Afore

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      01Abr2014              - Se corrige el numero de cifras decimales de los campos
                                        que contienen AIVs a 6 cifras
======================================================================
}
FUNCTION fn_reporte_carga_archivo(p_folio, reg_cifras, p_b_despliegue_pantalla)
DEFINE p_folio                  INTEGER,
       p_b_despliegue_pantalla  SMALLINT, -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
       reg_cifras               RECORD
         registros_detalle       LIKE tia_sum_traspaso.registros_detalle
         ,tot_aivs               LIKE tia_sum_traspaso.tot_aivs
         ,tot_sdo_viv92          LIKE tia_sum_traspaso.tot_sdo_viv92
       END RECORD,
       manejador_rpt            om.SaxDocumentHandler,
       v_ruta_reporte           STRING, -- ruta del archivo del reporte
       v_ruta_listados          STRING, -- ruta de los listados
       v_ruta_ejecutable        STRING, -- ruta del ejecutable
       v_nombre_usuario         LIKE seg_usuario.usuario_desc,
       v_nombre_archivo         LIKE glo_ctr_archivo.nombre_archivo
 
    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("tia") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados

    --Se consulta el nombre del usuario para despliegue en reporte 
    SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario 
     WHERE usuario_cod = p_usuario_cod                                      
     
   -- se consulta el nombre del archivo que se carga para despliegue en reporte
    SELECT nombre_archivo 
      INTO v_nombre_archivo
      FROM glo_ctr_archivo
     WHERE folio = p_folio
       AND proceso_cod = 1701 
                                                
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("TIAC01.4rp") THEN 
       CALL fgl_report_selectDevice ("PDF")
                    
       LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","carga_traspasos_ia"
       CALL fgl_report_setOutputFileName(v_ruta_reporte)
       CALL fgl_report_selectPreview(1)
       LET manejador_rpt = fgl_report_commitCurrentSettings()

       -- Inicia el reporte de registros con rechazo
       START REPORT rpt_carga_sin_cambios TO XML HANDLER manejador_rpt

       -- se envian los datos al reporte
       OUTPUT TO REPORT rpt_carga_sin_cambios(reg_cifras.*, p_folio, v_nombre_usuario, v_nombre_archivo)                                                                

       FINISH REPORT rpt_carga_sin_cambios 

    ELSE 
        LET v_bandera_reportes=1
       CALL fn_mensaje("Atención","No se puede abrir la plantilla TIAC01.4rp.\nNo se puede generar el reporte.","stop")
    END IF   
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_carga_sin_cambios(reg_cifras, p_folio, p_nombre_usuario, p_nombre_archivo)
DEFINE p_folio               DECIMAL(9,0),
       reg_cifras            RECORD
         registros_detalle       LIKE tia_sum_traspaso.registros_detalle
        ,tot_aivs           LIKE tia_sum_traspaso.tot_aivs
        ,tot_sdo_viv92        LIKE tia_sum_traspaso.tot_sdo_viv92
        END RECORD,
       v_fecha_reporte       DATE,
       p_nombre_usuario      LIKE seg_usuario.usuario_desc,
       p_nombre_archivo      LIKE glo_ctr_archivo.nombre_archivo,
       v_f_presentacion      LIKE tia_cza_traspaso.f_presentacion,
       v_cadena              STRING, -- cadena auxiliar
       v_periodo_archivo     STRING -- para enviar el periodo del archivo
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER                                                                          
      -- se imprimen los datos del encabezado
      LET v_fecha_reporte = TODAY CLIPPED

      -- el periodo es mes y ano de la fecha de presentacion
      SELECT f_presentacion
      INTO   v_f_presentacion
      FROM   tia_cza_traspaso
      WHERE  folio = p_folio

      -- ano a 4 cifras
      LET v_cadena = YEAR(v_f_presentacion) USING "&&&&"
      LET v_periodo_archivo = fn_mes_texto(MONTH(v_f_presentacion)), " ", v_cadena 
      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_folio
      PRINTX p_nombre_usuario
      PRINTX p_nombre_archivo
      PRINTX p_usuario_cod
      PRINTX v_periodo_archivo
      
   ON EVERY ROW
      -- en el detalle van las cifras control   
      PRINTX reg_cifras.*
                                                                                           
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