################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 19/02/2013                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTC10                                                   #
#Objetivo          => Programa de consulta de la bitácora de reversos          #
#Fecha inicio      => 19/02/2013                                               #
################################################################################
IMPORT os
DATABASE
   safre_viv
GLOBALS "CNTG01.4gl"

GLOBALS
  DEFINE
    f_fecha_inicial          DATE,
    f_fecha_final            DATE,
    f_folio_cnt              LIKE cnt_transaccion.folio_cnt,
    f_folio_liquida          LIKE cnt_transaccion.folio_liquida

  DEFINE  
    v_arr_valida_proceso     DYNAMIC ARRAY OF RECORD
      f_ejecuta              DATE, 
      h_ejecuta              CHAR(8),
      usuario                CHAR(15),
      tpo_reverso            CHAR(45),
      folio_liquida          DECIMAL(9,0),
      f_liquida              DATE,
      folio_cnt              DECIMAL(9,0),
      f_emision              DATE,
      proceso                CHAR(40),
      estado                 CHAR(35)
  END RECORD,
   
  v_aux_valida_proceso       DYNAMIC ARRAY OF DECIMAL(22,2)
    
END GLOBALS

MAIN  
  CALL fn_main_cntc05()-- Función que inicia la  forma
END MAIN

FUNCTION fn_main_cntc05()
  DEFINE
    f_ventana                ui.Window, -- Define las propìedades de la Ventana
    f_forma                  ui.Form,   -- Define las propiedades de la forma
    f_fecha                  DATE,
    v_registros              SMALLINT 
   
  -- se asignan los parametros que vienen del fglrun
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  -- se asigna el titulo del programa
  IF (g_nom_prog IS NOT NULL) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  CLOSE WINDOW SCREEN

  OPEN WINDOW wMain WITH FORM "CNTC101"
    DIALOG ATTRIBUTES (UNBUFFERED)
      INPUT BY NAME v_cmb_proceso, f_fecha_inicial, f_fecha_final, f_folio_cnt, f_folio_liquida 
        BEFORE INPUT                    
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL f_forma.setElementHidden("grp_valida_proceso",TRUE)
          CALL fn_llena_combo_proceso()
          --CALL DIALOG.setActionHidden("reporte",1)
             
          ON ACTION ACCEPT
             IF (v_cmb_proceso   IS NULL) AND 
                (f_fecha_inicial IS NULL) AND 
                (f_fecha_final   IS NULL) AND 
                (f_folio_cnt     IS NULL) AND 
                (f_folio_liquida IS NULL) THEN
                CALL fn_mensaje("Error","Ingrese al menos un criterio de búsqueda","information")
                NEXT FIELD v_cmb_proceso
             ELSE 
                IF ((f_fecha_inicial IS NULL)      AND 
                    (f_fecha_final   IS NOT NULL)) OR 
                   ((f_fecha_inicial IS NOT NULL)  AND 
                    (f_fecha_final   IS NULL))     THEN
                   CALL fn_mensaje("Error","Debe ingresar fecha inicial y final para realizar la búsqueda","information")
                   NEXT FIELD f_fecha_inicial
                END IF
             END IF 

             CALL f_forma.setElementHidden("grp_valida_proceso",FALSE)
             CALL v_arr_valida_proceso.clear()

             --Realiza la consulta por proceso y/o rango de periodo
             CALL fn_valida_proceso() RETURNING v_registros  

             IF v_registros <= 1 THEN
                CALL fn_mensaje("INFORMACIÓN","No existe información de reversos","info")
                CALL v_arr_valida_proceso.clear()
                CALL f_forma.setElementHidden("grp_valida_proceso",TRUE)
                --CALL DIALOG.setActionHidden("reporte",1)
                NEXT FIELD v_cmb_proceso
             END IF 

             --Habilita el botón para ejecutar el reporte
             --CALL DIALOG.setActionHidden("reporte",0)

             --Despliega la consulta en la tabla
             DISPLAY ARRAY v_arr_valida_proceso TO scr_valida_procesos.* 
             ATTRIBUTES (ACCEPT = FALSE)
                ON ACTION CANCEL 
                   EXIT PROGRAM 
       
                {ON ACTION Reporte
                   CALL fn_reporte_valida_proceso(f_fecha_inicial, f_fecha_final)}

                ON ACTION Archivo
                   CALL fn_genera_archivo_bitacora()
             END DISPLAY                
      END INPUT 

      ON ACTION Cancelar 
         EXIT DIALOG  

    END DIALOG 
  CLOSE WINDOW wMain
END FUNCTION

FUNCTION fn_oculta_campos (campo,valor,tipo)
  DEFINE valor               SMALLINT
  DEFINE tipo                INTEGER 
  DEFINE campo               STRING
  DEFINE win                 ui.Window, 
         fm                  ui.Form

  LET win = ui.Window.getCurrent()
  LET fm  = win.getForm()
  
  IF tipo = 1 THEN 
     CALL fm.setElementHidden(campo, valor)
  END IF 

  IF tipo = 2 THEN
     --CALL fm.setFieldHidden("customer.custid",1)
     CALL fm.setElementHidden("label_custid",1)
  END  IF 
  
END FUNCTION

FUNCTION fn_oculta_camposInicio()     
  CALL fn_oculta_campos ("grp_valida_proceso",1,1)
END FUNCTION 

FUNCTION fn_limpia_encabezado ()--- Limpia los títulos  del cuadro validación  por proceso
  DEFINE 
    win                      ui.Window, 
    fm                       ui.Form,
    v_incremento             INTEGER,
    v_titulo                 STRING 

  LET win = ui.Window.getCurrent()
  LET fm = win.getForm()    

  FOR v_incremento = 1 TO 5
      LET  v_titulo = "formonly.tb_importe"||v_incremento
      CALL fm.setElementText(v_titulo," " )
  END FOR 
END FUNCTION 

--Función para obtener las cuentas contables, folios y documentos contables y desplegarla en la tabla
FUNCTION fn_valida_proceso() 
  DEFINE
    consulta                 STRING,
    indx                     INT

  LET consulta = "\n SELECT T.f_ejecuta, ",
                 "\n        T.h_ejecuta, ",
                 "\n        T.usuario, ",
                 "\n        T.tpo_reverso || '-' || ER.desc_edo_cnt_rev Reverso, ",
                 "\n        T.folio_liquida, ",
                 "\n        T.f_liquida, ",
                 "\n        T.folio_cnt, ",
                 "\n        T.f_emision, ",
                 "\n        T.cod_proceso_cnt || '-' || PC.desc_proceso_cnt Proceso, ",
                 --"\n        P.num_poliza, ",
                 "\n        T.estado || '-' || E.desc_estado_cnt Estado ",
                 "\n FROM   cnt_ctr_reverso T ", 
                 "\n INNER  JOIN cat_estado_cnt E ",
                 "\n ON     E.cod_estado_cnt  = T.estado ",
                 "\n INNER  JOIN cat_proceso_cnt PC ",
                 "\n ON     T.cod_proceso_cnt = PC.cod_proceso_cnt",
                 "\n INNER  JOIN cat_edo_cnt_reverso ER ",
                 "\n ON     T.tpo_reverso     = ER.cod_edo_cnt_rev",
                 {"\n LEFT   OUTER JOIN cnt_ctr_proceso P ",
                 "\n ON     T.folio_liquida   = P.folio_liquida  ",}
                 "\n WHERE  T.folio_liquida  IS NOT NULL "
                     
  --Se arma el query dinámico
  IF (v_cmb_proceso IS NOT NULL) THEN 
     LET consulta = consulta || "\n AND T.cod_proceso_cnt = ", v_cmb_proceso
  END IF 
      
  IF (f_fecha_inicial IS NOT NULL) AND (f_fecha_final IS NOT NULL) THEN 
     LET consulta = consulta || "\n AND T.f_ejecuta BETWEEN '", f_fecha_inicial, "' AND '",f_fecha_final, "'"
  END IF

  IF (f_folio_cnt IS NOT NULL) THEN 
     LET consulta = consulta || "\n AND T.folio_cnt = ", f_folio_cnt
  END IF  

  IF (f_folio_liquida IS NOT NULL) THEN 
     LET consulta = consulta || "\n AND T.folio_liquida = ", f_folio_liquida
  END IF
      
  LET consulta = consulta, "\n GROUP BY 1,2,3,4,5,6,7,8,9,10 ORDER BY 1 DESC, 2 DESC, 3"
     
  DISPLAY "La consulta:  \n ", consulta
                          
  LET indx = 1

  DECLARE valida_proceso CURSOR FROM consulta 
  FOREACH valida_proceso INTO v_arr_valida_proceso[indx].f_ejecuta,
                              v_arr_valida_proceso[indx].h_ejecuta,  
                              v_arr_valida_proceso[indx].usuario, 
                              v_arr_valida_proceso[indx].tpo_reverso,
                              v_arr_valida_proceso[indx].folio_liquida,
                              v_arr_valida_proceso[indx].f_liquida,
                              v_arr_valida_proceso[indx].folio_cnt,
                              v_arr_valida_proceso[indx].f_emision,
                              v_arr_valida_proceso[indx].proceso,
                              v_arr_valida_proceso[indx].estado 
                                 
    IF v_arr_valida_proceso[indx].folio_cnt IS NULL THEN 
       LET v_arr_valida_proceso[indx].folio_cnt = 0
    END IF 
                                    
    LET indx = indx + 1
  END FOREACH

  CALL v_arr_valida_proceso.deleteElement(v_arr_valida_proceso.getLength())

  DISPLAY "El index de la consulta -- ", indx
       
  RETURN indx
    
END FUNCTION

--Genera un archivo de salida en texto plano con la información de la bitácora de reversos
FUNCTION fn_genera_archivo_bitacora()
  DEFINE 
    v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
    v_ruta_envio_cnt         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_recorre_arreglo        INTEGER,
    v_archivo_copia          VARCHAR(50),
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING

  LET v_nom_archivo = "/bitacora_reversos.cnt"

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_cnt
  FROM   seg_modulo
  WHERE  modulo_cod = "cnt"

  LET v_ruta_nomarch = v_ruta_envio_cnt CLIPPED || v_nom_archivo CLIPPED 

  DISPLAY "Ruta: ",v_ruta_nomarch

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")
  LET v_encabezado = ""

  --Imprime encabezado del archivo
  IF v_cmb_proceso IS NOT NULL THEN
     LET v_encabezado = v_encabezado,"|Proceso:",v_arr_valida_proceso[1].proceso
  END IF 

  IF f_fecha_inicial IS NOT NULL THEN
     LET v_encabezado = v_encabezado,"|Fecha Inicial:",f_fecha_inicial USING "dd-mm-yyyy"
  END IF 

  IF f_fecha_final IS NOT NULL THEN
     LET v_encabezado = v_encabezado,"|Fecha Final:",f_fecha_final USING "dd-mm-yyyy"
  END IF 

  IF f_folio_cnt IS NOT NULL THEN
     LET v_encabezado = v_encabezado,"|Folio Contable:",f_folio_cnt
  END IF 

  IF f_folio_liquida IS NOT NULL THEN
     LET v_encabezado = v_encabezado,"|Folio Liquidación:",f_folio_liquida
  END IF 

  CALL v_ch_arch_salida.write([v_encabezado])
  LET v_encabezado = "Fecha Reverso|Hora Reverso|Usuario|Tipo Reverso|Folio Liquidación|Fecha Liquidación|Folio Contable|Fecha Contable|Proceso|Estado"
  CALL v_ch_arch_salida.write([v_encabezado])
   
  FOR v_recorre_arreglo = 1 TO v_arr_valida_proceso.getLength()
      LET v_detalle = v_arr_valida_proceso[v_recorre_arreglo].f_ejecuta USING "dd-mm-yyyy", "|",
                      v_arr_valida_proceso[v_recorre_arreglo].h_ejecuta, "|",
                      v_arr_valida_proceso[v_recorre_arreglo].usuario CLIPPED, "|",
                      v_arr_valida_proceso[v_recorre_arreglo].tpo_reverso CLIPPED, "|",
                      v_arr_valida_proceso[v_recorre_arreglo].folio_liquida USING "&&&&&&&&&", "|",
                      v_arr_valida_proceso[v_recorre_arreglo].f_liquida USING "dd-mm-yyyy", "|",
                      v_arr_valida_proceso[v_recorre_arreglo].folio_cnt USING "&&&&&&&&&", "|",
                      v_arr_valida_proceso[v_recorre_arreglo].f_emision USING "dd-mm-yyyy", "|",
                      v_arr_valida_proceso[v_recorre_arreglo].proceso CLIPPED, "|",
                      v_arr_valida_proceso[v_recorre_arreglo].estado CLIPPED      
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR 

  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_cnt CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de la bitácora de reversos\n en la ruta"||v_ruta_nomarch,"information")
   
END FUNCTION 

FUNCTION fn_reporte_valida_proceso(f_fecha_inicial, f_fecha_final)
  DEFINE 
    i                        INTEGER,
    f_fecha_inicial          DATE,
    f_fecha_final            DATE 
   
  DEFINE 
    manejador_rpt            om.SaxDocumentHandler  -- Contenedor documentos reporte

  -- Carga el nombre del archivo y el tipo de documento a generar
  IF fgl_report_loadCurrentSettings("CNTC052.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  DISPLAY "Usuario -- ", g_usuario
  --Inicia el reporte 
  START REPORT rpt_valida_procesos TO XML HANDLER manejador_rpt 
    FOR i = 1 TO   v_arr_valida_proceso.getLength() 
        OUTPUT TO REPORT rpt_valida_procesos(v_arr_valida_proceso[i].*, f_fecha_inicial, f_fecha_final)
    END FOR      
  FINISH REPORT rpt_valida_procesos

END FUNCTION

REPORT rpt_valida_procesos(v_arr_valida_proceso, f_fecha_inicial, f_fecha_final)
  DEFINE 
    v_arr_valida_proceso     RECORD
      f_ejecuta              DATE, 
      h_ejecuta              CHAR(8),
      usuario                CHAR(15),
      tpo_reverso            CHAR(45),
      folio_liquida          DECIMAL(9,0),
      f_liquida              DATE,
      folio_cnt              DECIMAL(9,0),
      f_emision              DATE,
      proceso                CHAR(40),
      estado                 CHAR(35)
  END RECORD
   
  DEFINE 
    v_fecha_reporte          DATE,
    f_fecha_inicial          DATE,
    f_fecha_final            DATE 

  FORMAT
    FIRST PAGE HEADER
       LET v_fecha_reporte = TODAY CLIPPED

       PRINTX g_usuario
       PRINTX v_fecha_reporte USING "dd-mm-yyyy"
       PRINTX f_fecha_inicial USING "dd-mm-yyyy"
       PRINTX f_fecha_final   USING "dd-mm-yyyy"
      
    ON EVERY ROW
       PRINTX v_arr_valida_proceso.f_ejecuta USING "dd-mm-yyyy" 
       PRINTX v_arr_valida_proceso.h_ejecuta
       PRINTX v_arr_valida_proceso.usuario
       PRINTX v_arr_valida_proceso.tpo_reverso
       PRINTX v_arr_valida_proceso.folio_liquida
       PRINTX v_arr_valida_proceso.f_liquida USING "dd-mm-yyyy"
       PRINTX v_arr_valida_proceso.folio_cnt
       PRINTX v_arr_valida_proceso.f_emision USING "dd-mm-yyyy"
       PRINTX v_arr_valida_proceso.proceso
       PRINTX v_arr_valida_proceso.estado

END REPORT