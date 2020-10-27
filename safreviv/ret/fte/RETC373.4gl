--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10 Octubre, 2012
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC373                                                                #
#Objetivo     => Consulta de Indicadores de Devoluciones                                #
#Fecha inicio => Julio 07, 2015                                                       # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     -- forma como ejecutara el programa
       p_s_titulo       STRING                        -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida 
   CALL fn_indicador_devoluciones(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC373
Nombre: fn_indicador_devoluciones
Fecha creacion: Julio 07, 2015
Registro de modificaciones:
Descrip: CONSULTA DE INDICARDORES DE DEVOLUCIONES
==============================================================================
}
FUNCTION fn_indicador_devoluciones(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio          DECIMAL(9,0), -- folio
       v_f_ini          LIKE ret_cza_transferencia.f_carga,
       v_f_fin          LIKE ret_cza_transferencia.f_carga,
       v_c_ruta_env     LIKE seg_modulo.ruta_envio,
       v_extension_txt  STRING, -- extension del archivo de salida
       v_nom_archivo    STRING, -- nombre del archivo de salida
       v_archivo_txt    STRING, -- nombre y extension del archivo con el detalle
       v_v_ruta_nomarch STRING, -- ruta y nombre del archivo de salida
       v_mensaje_archivo STRING, -- mensaje de generacion de archivo
       v_ch_arch_ret_generico BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       v_conteo         INTEGER, -- contador de registros
       v_s_detalle      STRING,
       arr_reg_detalle     DYNAMIC ARRAY OF RECORD
         v_perdiodo              CHAR(20),
         v_tot_dev               INTEGER,
         v_tot_dev_dif           INTEGER,
         v_porcent_tot_dev_dif   DECIMAL(9,5),
         v_monto_dev             DECIMAL(24,2), 
         v_monto_dev_dif         DECIMAL(24,2), 
         v_porcent_monto_dev_dif DECIMAL(9,5)
       END RECORD,
       arr_reg_det_mes_anio    DYNAMIC ARRAY OF RECORD
         v_mes                   SMALLINT,
         v_anio                  SMALLINT
       END RECORD,
       arr_folios              DYNAMIC ARRAY OF RECORD 
         v_periodo               CHAR(20)    ,
         v_folio                 DECIMAL(9,0),
         v_num_reg               INTEGER     ,
         v_mes                   SMALLINT    ,
         v_anio                  SMALLINT    ,
         v_num_reg_dev_dif       INTEGER     ,
         v_por_reg_dev_dif       DECIMAL(9,5),
         v_monto_dev             DECIMAL(24,2),
         v_monto_dif_dev         DECIMAL(24,2),
         v_por_monto_dif_dev     DECIMAL(9,5)
       END RECORD, 
       v_query                       STRING, -- detalle
       v_indice                      SMALLINT, -- indice de arreglo
       v_ruta_reporte                STRING ,-- ruta del archivo del reporte       
       v_ruta_listados               STRING ,-- ruta de los listados
       v_ruta_ejecutable             STRING ,-- ruta del ejecutable
       manejador_rpt                 om.SaxDocumentHandler ,
       v_indice_reporte              SMALLINT,
       v_contador_tmp                INTEGER,
       v_tabla                       LIKE glo_his_folio.tabla
 
       
   OPEN WINDOW w_consulta_indicadores WITH FORM "RETC3731"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Indicador Devoluciones del Saldo de la Subcuenta de Vivienda")

   INPUT v_f_ini, v_f_fin 
      FROM v_f_ini, v_f_fin
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_f_ini   = TODAY 
         LET v_f_fin   = TODAY 
        
      ON ACTION ACCEPT

         -- se borran los arreglos de despliegue
         CALL arr_reg_detalle.clear()
         WHENEVER ERROR CONTINUE
             DROP TABLE tmp_indicador_devoluciones;
         WHENEVER ERROR STOP

         CREATE TEMP TABLE tmp_indicador_devoluciones(periodo            CHAR(20)    ,
                                                      folio              DECIMAL(9,0),
                                                      num_reg            INTEGER     ,
                                                      mes                SMALLINT    ,
                                                      anio               SMALLINT    ,
                                                      num_reg_dev_dif    INTEGER     ,
                                                      por_reg_dev_dif    DECIMAL(5,1),
                                                      monto_dev          DECIMAL(24,2),
                                                      monto_dif_dev      DECIMAL(24,2),
                                                      por_monto_dif_dev  DECIMAL(5,1))

                                                      
         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF ( (v_f_ini IS NULL ) OR 
              (v_f_fin IS NULL ) ) THEN

            CALL fn_mensaje("Información","Debe indicar el periodo de búsqueda","stop")
            CONTINUE INPUT  
         END IF 
         IF v_f_ini > v_f_fin THEN 
             CALL fn_mensaje("Alto","La fecha inicial no puede ser mayor que la fecha final","stop")
             LET v_f_ini   = TODAY 
             LET v_f_fin   = TODAY 
             CONTINUE INPUT 
         END IF 

         --- Se obtiene el minimo del folio para realizar la consulta
         LET v_query = "\n SELECT COUNT(*), MONTH(f_liquida), YEAR(f_liquida), TO_CHAR(f_liquida, '%B-%Y'), folio ",
                       "\n FROM (SELECT DISTINCT f_liquida, id_referencia, folio                                  ",
		               "\n       FROM   glo_folio b, cta_movimiento a                                               ",
		               "\n       WHERE  b.f_actualiza between '", v_f_ini, "' AND '", v_f_fin, "'                   ",
		               "\n       AND    b.proceso_cod in (1502, 1504, 1519)                                         ",
		               "\n       AND    b.status = 3                                                                ",
                     "\n       AND    b.folio = a.folio_liquida                                                   ",
                     "\n       UNION ALL                                                                          ",
                     "\n       SELECT DISTINCT f_liquida, id_referencia, folio                                    ",
		               "\n       FROM   glo_folio b, cta_movimiento14 a                                             ",
		               "\n       WHERE  b.f_actualiza between '", v_f_ini, "' AND '", v_f_fin, "'                   ",
		               "\n       AND    b.proceso_cod in (1502, 1504, 1519)                                         ",
		               "\n       AND    b.status = 3                                                                ",
                     "\n       AND    b.folio = a.folio_liquida                                                   ",
                     "\n       UNION ALL                                                                          ",
                     "\n       SELECT DISTINCT f_liquida, id_referencia, folio                                    ",
		               "\n       FROM   cta_movimiento13 a, glo_folio b                                             ",
		               "\n       WHERE  b.f_actualiza between '", v_f_ini, "' AND '", v_f_fin, "'                   ",
		               "\n       AND    b.proceso_cod in (1502, 1504, 1519)                                         ",
		               "\n       AND    b.status = 3                                                                ",
                     "\n       AND    b.folio = a.folio_liquida                                                   ",
                     "\n       UNION ALL                                                                          ",
                     "\n       SELECT DISTINCT f_liquida, id_referencia, folio                                    ",
		               "\n       FROM   cta_movimiento12 a, glo_folio b                                             ",
		               "\n       WHERE  b.f_actualiza between '", v_f_ini, "' AND '", v_f_fin, "'                   ",
		               "\n       AND    b.proceso_cod in (1502, 1504, 1519)                                         ",
		               "\n       AND    b.status = 3                                                                ",
                     "\n       AND    b.folio = a.folio_liquida                                                   ",
                     "\n      )                                                                                   ",
                     "\n GROUP BY 2, 3, 4, 5                                                                      ",
                     "\n ORDER BY 2, 3                                                                            "

         --DISPLAY "El Query \n",v_query; 
         PREPARE sfolios FROM v_query
         DECLARE cur_folios  CURSOR FOR sfolios
                     
                     
         --llena el arreglo        
         LET v_indice = 1
         
         FOREACH cur_folios INTO 
             arr_folios[v_indice].v_num_reg ,
             arr_folios[v_indice].v_mes     ,
             arr_folios[v_indice].v_anio    ,
             arr_folios[v_indice].v_periodo ,
             arr_folios[v_indice].v_folio            
             -- Busca los casos con diferencias
             SELECT COUNT(*) 
             INTO   arr_folios[v_indice].v_num_reg_dev_dif
             FROM  (SELECT DISTINCT id_solicitud 
                    FROM   ret_his_saldo 
                    WHERE  folio = arr_folios[v_indice].v_folio);
             IF (arr_folios[v_indice].v_num_reg_dev_dif IS NOT NULL AND arr_folios[v_indice].v_num_reg_dev_dif <> 0) THEN
                 LET arr_folios[v_indice].v_por_reg_dev_dif = ((arr_folios[v_indice].v_num_reg_dev_dif * 100) / arr_folios[v_indice].v_num_reg)
             ELSE
                 LET arr_folios[v_indice].v_por_reg_dev_dif = 0
             END IF 
             -- Busca los montos de las devoluciones totales
               -- Busca la tabla donde se encuentran los movimientos
                  SELECT tabla
                  INTO   v_tabla
                  FROM   glo_his_folio
                  WHERE  folio = arr_folios[v_indice].v_folio
             IF v_tabla IS NULL THEN 
                 LET v_tabla = "cta_movimiento"
             END IF 

             LET v_query = "\n SELECT NVL(SUM(ABS(monto_pesos)),0)                    ",
                           "\n FROM   ", v_tabla                                       ,  
                           "\n WHERE  folio_liquida = ", arr_folios[v_indice].v_folio  , -- el trabajador
                           "\n AND    movimiento IN (212,222,822,832,852,842)         " 
             --DISPLAY "El query para obtener el monto total:"
             --DISPLAY ">",v_query, "<"
             PREPARE smonto_disp FROM v_query
             EXECUTE smonto_disp INTO arr_folios[v_indice].v_monto_dev
             
             -- Busca los montos con diferencia de las devoluciones
             LET v_query = "\n SELECT NVL(SUM(ABS(monto_pesos)),0)                    ",
                           "\n FROM   ", v_tabla                                       ,  
                           "\n WHERE  folio_liquida = ", arr_folios[v_indice].v_folio  , -- el trabajador
                           "\n AND    movimiento IN (832,852,842,1011)                " 
             --DISPLAY "El query para obtener el monto de las devoluciones total:"
             --DISPLAY ">",v_query, "<"
             PREPARE smonto_disp_dif FROM v_query
             EXECUTE smonto_disp_dif INTO arr_folios[v_indice].v_monto_dif_dev
            
             IF (arr_folios[v_indice].v_monto_dif_dev IS NOT NULL AND arr_folios[v_indice].v_monto_dif_dev <> 0) THEN 
                 LET arr_folios[v_indice].v_por_monto_dif_dev = ((arr_folios[v_indice].v_monto_dif_dev * 100) / arr_folios[v_indice].v_monto_dev)
             ELSE
                 LET arr_folios[v_indice].v_por_monto_dif_dev = 0
             END IF 
             --DISPLAY "Periodo:", arr_folios[v_indice].v_periodo
             --DISPLAY "  folio:", arr_folios[v_indice].v_folio
             --DISPLAY "num_reg:", arr_folios[v_indice].v_num_reg
             --DISPLAY "    mes:", arr_folios[v_indice].v_mes
             --DISPLAY "   anio:", arr_folios[v_indice].v_anio
             --DISPLAY "   nrdd:", arr_folios[v_indice].v_num_reg_dev_dif
             --DISPLAY "monto d:", arr_folios[v_indice].v_monto_dev
             --DISPLAY "mto d d:", arr_folios[v_indice].v_monto_dif_dev
             --
             INSERT INTO tmp_indicador_devoluciones
                  VALUES (arr_folios[v_indice].v_periodo,
                          arr_folios[v_indice].v_folio,
                          arr_folios[v_indice].v_num_reg,
                          arr_folios[v_indice].v_mes,
                          arr_folios[v_indice].v_anio,
                          arr_folios[v_indice].v_num_reg_dev_dif,
                          0, --arr_folios[v_indice].v_por_reg_dev_dif,
                          arr_folios[v_indice].v_monto_dev,
                          arr_folios[v_indice].v_monto_dif_dev,
                          0 --arr_folios[v_indice].v_por_monto_dif_dev
                         );
             LET v_indice = v_indice + 1
             
         END FOREACH
         SELECT COUNT(*) 
         INTO   v_contador_tmp
         FROM   tmp_indicador_devoluciones
         DISPLAY "Los registros insertados son>",v_contador_tmp
         LET v_query = "\n SELECT mes, anio, periodo, SUM(num_reg), SUM(num_reg_dev_dif), SUM(monto_dev), SUM(monto_dif_dev) ",
                       "\n FROM   tmp_indicador_devoluciones                                                                 ",
                       "\n GROUP BY mes, anio, periodo                                                                       ",
                       "\n ORDER BY anio, mes                                                                                "

         --DISPLAY "El Query \n",v_query; 
         PREPARE sid_detalle FROM v_query
         DECLARE cur_detalle  CURSOR FOR sid_detalle

         --llena el arreglo        
         LET v_indice = 1
         
         FOREACH cur_detalle INTO 
             arr_reg_det_mes_anio[v_indice].v_mes,
             arr_reg_det_mes_anio[v_indice].v_anio,
             arr_reg_detalle[v_indice].v_perdiodo,
             arr_reg_detalle[v_indice].v_tot_dev,
             arr_reg_detalle[v_indice].v_tot_dev_dif,
             arr_reg_detalle[v_indice].v_monto_dev,
             arr_reg_detalle[v_indice].v_monto_dev_dif
             IF arr_reg_detalle[v_indice].v_tot_dev_dif IS NOT NULL AND arr_reg_detalle[v_indice].v_tot_dev_dif > 0 THEN 
                 LET arr_reg_detalle[v_indice].v_porcent_tot_dev_dif = ((arr_reg_detalle[v_indice].v_tot_dev_dif * 100) / arr_reg_detalle[v_indice].v_tot_dev)
             ELSE
                 LET arr_reg_detalle[v_indice].v_porcent_tot_dev_dif = 0
             END IF 
             IF arr_reg_detalle[v_indice].v_monto_dev_dif IS NOT NULL AND arr_reg_detalle[v_indice].v_monto_dev_dif > 0 THEN 
                 LET arr_reg_detalle[v_indice].v_porcent_monto_dev_dif = ((arr_reg_detalle[v_indice].v_monto_dev_dif * 100) / arr_reg_detalle[v_indice].v_monto_dev)
             ELSE
                 LET arr_reg_detalle[v_indice].v_porcent_monto_dev_dif = 0
             END IF 
             LET v_indice = v_indice + 1
         END FOREACH          
         -- se borra el ultimo registro
         CALL arr_reg_detalle.deleteElement(arr_reg_detalle.getLength())
         
            
         -- se abre la ventana de resultados
         OPEN WINDOW w_detalle_indicadores_devoluciones WITH FORM "RETC3732"
            
         -- se abre un dialog para realizar el despliegue de los resultados
        DIALOG
            ATTRIBUTES ( UNBUFFERED )
            
               
               DISPLAY ARRAY arr_reg_detalle TO r_det_indicador.*
               END DISPLAY
               
               ON ACTION regresar
                  EXIT DIALOG

               ON ACTION Exportar
                   -- se obtiene la ruta de envio y ejecutable
                   SELECT ruta_envio
                   INTO   v_c_ruta_env 
                   FROM   seg_modulo
                   WHERE  modulo_cod = "ret"

                   -- las extensiones del archivo son TXT para el detalle
                   LET v_extension_txt = ".csv"

                   -- los nombres son todo en mayusculas con la siguiente mascara
                   -- SG_USUARIO_AAAAMMDD.TXT
                   LET v_nom_archivo = "Indicador_devoluciones_", p_usuario_cod CLIPPED, "_", TODAY USING "yyyymmdd"
                   LET v_archivo_txt = v_nom_archivo, v_extension_txt
                   
                   -- el archivo con ruta destino que contiene el detalle
                   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt
                   LET v_mensaje_archivo = "Se generara el archivo:", v_v_ruta_nomarch
                   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
                   -- nombre de archivo generado
                   DISPLAY "~~~~~~~~~~~"
                   DISPLAY "Archivo generado: ", v_v_ruta_nomarch

                   -- se crea el manejador de archivo
                   LET v_ch_arch_ret_generico = base.Channel.create()
                   CALL v_ch_arch_ret_generico.setDelimiter(NULL)
                   
                   -- se crea archivo y se indica que se escribira en el mismo
                   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
                   -- escribe el encabezado
                   LET v_s_detalle = "PERIODO,TOTAL CASOS DEVOLUCIÓN,CASOS DEVUELTOS CON DIFERENCIAS,% CASOS DEVUELTOS CON DIFERENCIAS,",
                                     "MONTO TOTAL DEVUELTO(pesos),MONTO DE DIFERENCIAS DEVUELTO (pesos),%MONTO DE DIFERENCIAS DEVUELTO"
                   
                   CALL v_ch_arch_ret_generico.write(v_s_detalle)
                   -- se inicia el contador de registros
                   LET v_conteo = 0
                  
                   -- llena el arreglo
                   LET v_indice = 1
                   FOR v_indice = 1 TO arr_reg_detalle.getLength()

                         LET v_s_detalle = arr_reg_detalle[v_indice].v_perdiodo              ,",",
                                           arr_reg_detalle[v_indice].v_tot_dev               ,",",
                                           arr_reg_detalle[v_indice].v_tot_dev_dif           ,",",
                                           arr_reg_detalle[v_indice].v_porcent_tot_dev_dif  USING "&&.&&&&&" ,",",
                                           arr_reg_detalle[v_indice].v_monto_dev             ,",",
                                           arr_reg_detalle[v_indice].v_monto_dev_dif         ,",",
                                           arr_reg_detalle[v_indice].v_porcent_monto_dev_dif USING "&&.&&&&&"
                         CALL v_ch_arch_ret_generico.write(v_s_detalle)
                   END FOR 

                   -- se cierra el archivo
                   CALL v_ch_arch_ret_generico.close()
                   LET v_mensaje_archivo = "Archivo generado exitosamente:", v_v_ruta_nomarch
                   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
                 
               ON ACTION reporte                      

                  -- Recupera la ruta de listados en el que se enviara el archivo
                  CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
    
                  -- Se asigna la plantilla para generar el reporte
                  IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETC373.4rp") ) THEN 
                     CALL fgl_report_selectDevice ("PDF")
                       
                     LET v_ruta_reporte = v_ruta_ejecutable CLIPPED,"/","detalle_indicadores_devoluciones"                
                     CALL fgl_report_setOutputFileName(v_ruta_reporte)
                     CALL fgl_report_selectPreview(1)
                     LET manejador_rpt = fgl_report_commitCurrentSettings()
                  ELSE         
                     CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla RETC373.4rp", "stop")
                     CONTINUE DIALOG
                  END IF   

                  --Inicia el reporte de los indicadores de devoluciones
                  START REPORT rpt_detalle_ind_devol TO XML HANDLER manejador_rpt
                  
                  FOR v_indice_reporte = 1 TO arr_reg_detalle.getLength()
                     
                     OUTPUT TO REPORT rpt_detalle_ind_devol(v_indice_reporte, arr_reg_detalle[v_indice_reporte].*, p_usuario_cod)
                  END FOR
                  
                  FINISH REPORT rpt_detalle_ind_devol

            END DIALOG
            
            CLOSE WINDOW w_detalle_indicadores_devoluciones
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_indicadores

END FUNCTION

{ ======================================================================
Clave: 
Nombre: rpt_detalle_ind_devol
Fecha creacion: Julio 09, 2015
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Genera el reporte de los indicadores de las devoluciones

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_detalle_ind_devol(p_indice, v_r_despliegue, p_usuario_cod) 
DEFINE   v_r_despliegue          RECORD -- registro de consulta
             v_perdiodo              CHAR(20),
             v_tot_dev               INTEGER,
             v_tot_dev_dif           INTEGER,
             v_porcent_tot_dev_dif   DECIMAL(9,5),
             v_monto_dev             DECIMAL(24,2), 
             v_monto_dev_dif         DECIMAL(24,2), 
             v_porcent_monto_dev_dif DECIMAL(9,5)
         END RECORD,
          p_indice                DECIMAL(9,0),
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          v_texto_total           CHAR(20), 
          v_tot_dev               INTEGER,
          v_tot_dev_dif           INTEGER,
          v_porcent_tot_dev_dif   DECIMAL(9,5),
          v_monto_dev             DECIMAL(24,2), 
          v_monto_dev_dif         DECIMAL(24,2), 
          v_porcent_monto_dev_dif DECIMAL(9,5)
          
FORMAT

   FIRST PAGE HEADER
      
      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"

      LET v_tot_dev       = 0
      LET v_tot_dev_dif   = 0
      LET v_monto_dev     = 0
      LET v_monto_dev_dif = 0

      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod

      LET v_nombre_usuario = v_nombre_usuario CLIPPED
      
      PRINTX p_usuario_cod, v_fecha, v_nombre_usuario
      
 
   ON EVERY ROW
      PRINTX v_r_despliegue.*
--      DISPLAY v_r_despliegue.*
      LET v_tot_dev       = v_tot_dev       + v_r_despliegue.v_tot_dev
      LET v_tot_dev_dif   = v_tot_dev_dif   + v_r_despliegue.v_tot_dev_dif
      LET v_monto_dev     = v_monto_dev     + v_r_despliegue.v_monto_dev
      LET v_monto_dev_dif = v_monto_dev_dif + v_r_despliegue.v_monto_dev_dif
   ON LAST ROW 
      LET v_texto_total = "Acumulado"
      IF v_tot_dev_dif > 0 THEN 
          LET v_porcent_tot_dev_dif   = ((v_tot_dev_dif   * 100) / v_tot_dev)  
      ELSE
          LET v_porcent_tot_dev_dif   = 0
      END IF 
      IF v_monto_dev_dif > 0 THEN 
          LET v_porcent_monto_dev_dif = ((v_monto_dev_dif * 100) / v_monto_dev)
      ELSE 
          LET v_porcent_monto_dev_dif = 0
      END IF 
      PRINTX v_texto_total               ,
             v_tot_dev                   ,
             v_tot_dev_dif               ,
             v_porcent_tot_dev_dif       ,
             v_monto_dev                 ,
             v_monto_dev_dif             ,
             v_porcent_monto_dev_dif             
END REPORT
