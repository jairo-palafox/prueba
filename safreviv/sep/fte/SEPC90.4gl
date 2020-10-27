################################################################################
#Modulo           => SEP                                                       #
#Programa         => SEPC90                                                    #
#Objetivo         => Consulta de Cuentas Virtuales de Separacion Liquidadas    #
#Fecha de Inicio  => 5 de Octubre de 2015                                      #
################################################################################
DATABASE "safre_viv"

DEFINE p_usuario_cod     CHAR(20),
       p_tpo_ejecucion   INTEGER,
       v_ventana         ui.Window,
       p_cad_ventana     STRING,
       v_forma           ui.Form,
       c_tipo            ui.ComboBox,
       c_tipo_value      SMALLINT,
       v_ruta_listados   CHAR(40),
       v_ruta_bin        CHAR(40),
       v_valor_aiv       DECIMAL(12,2)

--Tabla de resultados
DEFINE t_resultados DYNAMIC ARRAY OF RECORD
      v_t_invadido   CHAR(11),
      v_t_virtual    CHAR(11),
      v_t_f_apertura CHAR(10),
      v_t_descripcion CHAR(40)
END RECORD
DEFINE t_resultados_liq DYNAMIC ARRAY OF RECORD
      v_tl_invadido   CHAR(11),
      v_tl_virtual    CHAR(11),
      v_tl_f_apertura CHAR(10),
      v_tl_f_liquida  CHAR(10),
      v_tl_total_pesos DECIMAL(12,2),
      v_tl_sv92       DECIMAL(12,2),
      v_tl_sv_dia_92  DECIMAL(12,2),
      v_tl_aiv92      DECIMAL(12,2),
      v_tl_sv97       DECIMAL(12,2),
      v_tl_sv_dia_97  DECIMAL(12,2),
      v_tl_aiv97      DECIMAL(12,2),
      v_tl_descripcion CHAR(40)
END RECORD
--Variables de entrada
DEFINE filtro RECORD
          v_invadido        CHAR(11),
          v_virtual         CHAR(11),
          v_f_ini_apertura  CHAR(10),
          v_f_fin_apertura  CHAR(10)
END RECORD

DEFINE filtro_liq RECORD
          vl_invadido        CHAR(11),
          vl_virtual         CHAR(11),
          vl_f_ini_apertura  CHAR(10),
          vl_f_fin_apertura  CHAR(10),
          vl_f_ini_liquida   CHAR(10),
          vl_f_fin_liquida   CHAR(10)
END RECORD

MAIN

   DEFINE v_total_reg   INTEGER
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   --Se obtiene la ruta de control del módulo
   SELECT l.ruta_listados,l.ruta_bin
      INTO v_ruta_listados,v_ruta_bin
      FROM seg_modulo l
      WHERE l.modulo_cod = 'sep'

   CALL STARTLOG(v_ruta_listados CLIPPED ||"/"||p_usuario_cod CLIPPED||".SEPC90.log")

   OPEN WINDOW vtna_modificacion WITH FORM v_ruta_bin CLIPPED||"/SEPC901"
   #Se asigna el titulo de la ventana y toma el control de la forma
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         LET v_forma = v_ventana.getForm()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

    LET  c_tipo = ui.ComboBox.forName("c_tipo")
    CALL c_tipo.clear()
    CALL c_tipo.addItem(1,"Virtuales")
    CALL c_tipo.addItem(2,"Virtuales Liquidados")

      --CREANDO DIALOG
      DIALOG ATTRIBUTES(UNBUFFERED)

         INPUT c_tipo_value FROM c_tipo
            --ON ACTION aceptar
            ON CHANGE c_tipo
               --EVALÚA EL MENÚ QUE SE MOSTRARA DEPENDIENDO DEL TIPO CONSULTA
               IF c_tipo_value == 1 THEN
                  CALL v_forma.setElementHidden("filtrar",0)
                  CALL v_forma.setElementHidden("filtrar_liq",1)
                  NEXT FIELD v_invadido                  
               END IF
               
               IF c_tipo_value == 2 THEN
                  CALL v_forma.setElementHidden("filtrar",1)
                  CALL v_forma.setElementHidden("filtrar_liq",0)
                  NEXT FIELD vl_invadido                  
               END IF

               IF c_tipo_value IS NULL THEN
                  CALL v_forma.setElementHidden("filtrar",1)
                  CALL v_forma.setElementHidden("filtrar_liq",1)
               END IF
               CALL v_forma.setElementHidden("tabla_res",1)
               CALL v_forma.setElementHidden("tabla_res_liq",1)
               DISPLAY "" TO lbl_reporte_liq
               DISPLAY "" TO obj_reporte_liq
               DISPLAY "" TO lbl_reporte
               DISPLAY "" TO obj_reporte
         END INPUT

         --Captura de datos de entrada 
         INPUT BY NAME filtro.*
            ON ACTION aceptar
            CALL fn_llena_tabla() RETURNING v_total_reg
            IF v_total_reg > 0 THEN
               CALL v_forma.setElementHidden("tabla_res",0)
               DISPLAY "" TO lbl_reporte
               DISPLAY "" TO obj_reporte
            ELSE 
               CALL v_forma.setElementHidden("tabla_res",1)
               CALL v_forma.setElementHidden("tabla_res_liq",1)
               CALL fn_mensaje("AVISO","No se encontraron registros","information")
               DISPLAY "" TO lbl_reporte
               DISPLAY "" TO obj_reporte
            END IF
         END INPUT

        --Captura datos de entrada para liquidados
        INPUT BY NAME filtro_liq.*
            ON ACTION aceptar
            CALL fn_llena_tabla() RETURNING v_total_reg
            IF v_total_reg > 0 THEN
               CALL v_forma.setElementHidden("tabla_res_liq",0)
            ELSE 
               CALL v_forma.setElementHidden("tabla_res",1)
               CALL v_forma.setElementHidden("tabla_res_liq",1)
               CALL fn_mensaje("AVISO","No se encontraron registros","information")
            END IF
            DISPLAY "" TO lbl_reporte_liq
            DISPLAY "" TO obj_reporte_liq
         END INPUT

         --Despliegue de resultados
         DISPLAY ARRAY t_resultados TO tabla_res.*
            ON ACTION reporte
               CALL fn_genera_reporte()
         END DISPLAY
         
         DISPLAY ARRAY t_resultados_liq TO tabla_res_liq.*
            ON ACTION reporte
               CALL fn_genera_reporte()
         END DISPLAY

         BEFORE DIALOG  --Oculta todos los elementos del layout
            CALL v_forma.setElementHidden("tabla_res",1)
            CALL v_forma.setElementHidden("tabla_res_liq",1)
            CALL v_forma.setElementHidden("filtrar",1)
            CALL v_forma.setElementHidden("filtrar_liq",1)

         --ON ACTION aceptar
           
         ON ACTION cancelar
            EXIT DIALOG
            
      END DIALOG  

   CLOSE WINDOW vtna_modificacion

END MAIN

--*//////////////////////////////////////////////
FUNCTION fn_llena_tabla()

   DEFINE i         INTEGER
   DEFINE v_query   STRING
   DEFINE v_query_1 STRING
   DEFINE v_query_2 STRING
   DEFINE tmp_f_ini_apertura CHAR(10)
   DEFINE tmp_f_fin_apertura CHAR(10)
   DEFINE tmp_f_ini_liquida  CHAR(10)
   DEFINE tmp_f_fin_liquida  CHAR(10)

   --Cada que se inicia la funcion se eliminan las tablas temporales
   DROP TABLE IF EXISTS tmp_registro_1e;
   DROP TABLE IF EXISTS tmp_tabla_1e;

   --Limpia tabla y consultas
   LET v_query   = ""
   LET v_query_1 = ""
   LET v_query_2 = ""
   CALL t_resultados_liq.clear()
   CALL t_resultados.clear()

   IF c_tipo_value == 1 THEN
      LET tmp_f_ini_apertura = filtro.v_f_ini_apertura[4,5],"/",filtro.v_f_ini_apertura[1,2],"/",filtro.v_f_ini_apertura[7,10]
      LET tmp_f_fin_apertura = filtro.v_f_fin_apertura[4,5],"/",filtro.v_f_fin_apertura[1,2],"/",filtro.v_f_fin_apertura[7,10]     
      --Realizando Query para extraer los id_derechohabiente que se analizarán
      LET v_query = "SELECT a.invadido,b.asociado,c.f_apertura,e.descripcion ", 
                 "FROM sep_det_02_op27 a,sep_det_03_op27 b,afi_derechohabiente c,",
                 "sfr_marca_activa d, sep_estado_op27 e ",
                 "WHERE c.id_derechohabiente = b.id_derechohabiente_asociado ",
                 "AND a.estado = e.estado ",
                 "AND a.id_det_02_op27 = b.id_det_02_op27 ",
                 "AND d.id_derechohabiente = c.id_derechohabiente ",
                 "AND d.marca = 161 ",
                 "AND a.clasifica_separacion = 'E' "

      IF filtro.v_invadido IS NOT NULL THEN
         LET v_query = v_query , " AND a.invadido = ",filtro.v_invadido CLIPPED
      END IF
      IF filtro.v_virtual IS NOT NULL THEN
         LET v_query = v_query , " AND b.asociado = ",filtro.v_virtual CLIPPED
      END IF
      IF filtro.v_f_ini_apertura IS NOT NULL AND filtro.v_f_fin_apertura IS NULL THEN
         LET v_query = v_query , " AND c.f_apertura > '",tmp_f_ini_apertura,"'" CLIPPED
      END IF
      IF filtro.v_f_ini_apertura IS NULL AND filtro.v_f_fin_apertura IS NOT NULL THEN
         LET v_query = v_query , " AND c.f_apertura < '",tmp_f_fin_apertura,"'" CLIPPED
      END IF
      IF filtro.v_f_ini_apertura IS NOT NULL AND filtro.v_f_fin_apertura IS NOT NULL THEN
         LET v_query = v_query , " AND c.f_apertura BETWEEN '",tmp_f_ini_apertura,"' AND '",tmp_f_fin_apertura,"'" CLIPPED
      END IF
      LET v_query = v_query , " INTO TEMP tmp_registro_1e " CLIPPED  
      EXECUTE IMMEDIATE v_query 

      --Llenando el record con los resultados
      LET v_query_2 = "SELECT invadido,asociado,TO_CHAR(f_apertura,'%d/%m/%Y'),descripcion FROM tmp_registro_1e ORDER BY 1 "
      PREPARE prp_tabla_res FROM v_query_2
      DECLARE cur_tabla_res CURSOR FOR prp_tabla_res
      LET i = 1
      FOREACH cur_tabla_res INTO t_resultados[i].*
         LET i = i+1
      END FOREACH
      CALL t_resultados.deleteElement(i)
      LET i = i-1
      CLOSE cur_tabla_res
      FREE  cur_tabla_res

      RETURN i

   END IF

   IF c_tipo_value == 2 THEN
      --Obteniendo el valor al día de los aivs
      SELECT precio_fondo
         INTO v_valor_aiv 
         FROM glo_valor_fondo
         WHERE f_valuacion = TODAY
         AND fondo = 11
      LET tmp_f_ini_apertura = filtro_liq.vl_f_ini_apertura[4,5],"/",filtro_liq.vl_f_ini_apertura[1,2],"/",filtro_liq.vl_f_ini_apertura[7,10]
      LET tmp_f_fin_apertura = filtro_liq.vl_f_fin_apertura[4,5],"/",filtro_liq.vl_f_fin_apertura[1,2],"/",filtro_liq.vl_f_fin_apertura[7,10]     
      LET tmp_f_ini_liquida  = filtro_liq.vl_f_ini_liquida[4,5],"/",filtro_liq.vl_f_ini_liquida[1,2],"/",filtro_liq.vl_f_ini_liquida[7,10]
      LET tmp_f_fin_liquida  = filtro_liq.vl_f_fin_liquida[4,5],"/",filtro_liq.vl_f_fin_liquida[1,2],"/",filtro_liq.vl_f_fin_liquida[7,10]
      --Realizando Query para extraer los id_derechohabiente que se analizarán
      LET v_query = "SELECT c.id_derechohabiente,a.invadido,b.asociado,c.f_apertura,e.descripcion ", 
                 "FROM sep_det_02_op28 a,sep_det_03_op28 b,afi_derechohabiente c,",
                 "sfr_marca_activa d, sep_estado_det_op28 e ",
                 "WHERE c.id_derechohabiente = b.id_derechohabiente_asociado ",
                 "AND a.estado = e.estado ",
                 "AND a.id_det_02_op28 = b.id_det_02_op28 ",
                 "AND d.id_derechohabiente = c.id_derechohabiente ",
                 "AND d.marca = 161 ",
                 "AND a.clasifica_separacion = 'E' "
      IF filtro_liq.vl_invadido IS NOT NULL THEN
         LET v_query = v_query , " AND a.invadido = ",filtro_liq.vl_invadido CLIPPED
      END IF
      IF filtro_liq.vl_virtual IS NOT NULL THEN
         LET v_query = v_query , " AND b.asociado = ",filtro_liq.vl_virtual CLIPPED
      END IF
      IF filtro_liq.vl_f_ini_apertura IS NOT NULL AND filtro_liq.vl_f_fin_apertura IS NULL THEN
         LET v_query = v_query , " AND c.f_apertura > '",tmp_f_ini_apertura,"'" CLIPPED
      END IF
      IF filtro_liq.vl_f_ini_apertura IS NULL AND filtro_liq.vl_f_fin_apertura IS NOT NULL THEN
         LET v_query = v_query , " AND c.f_apertura < '",tmp_f_fin_apertura,"'" CLIPPED
      END IF
      IF filtro_liq.vl_f_ini_apertura IS NOT NULL AND filtro_liq.vl_f_fin_apertura IS NOT NULL THEN
         LET v_query = v_query , " AND c.f_apertura BETWEEN '",tmp_f_ini_apertura,"' AND '",tmp_f_fin_apertura,"'" CLIPPED
      END IF
      LET v_query = v_query , " INTO TEMP tmp_registro_1e " CLIPPED  
      --Imprime la cadena para validar el query 
      EXECUTE IMMEDIATE v_query 

      --Se procede a buscar los saldos y la fecha de liquidacion
      LET v_query_1 = "SELECT a.invadido,a.asociado,a.f_apertura, ",
                   "fn_sep_fliquida_mov(a.id_derechohabiente,61) AS f_liquida,",
                   "fn_sep_suma_saldos(a.id_derechohabiente,62)  AS saldo_virtual_92, ",
                   "fn_sep_suma_aivs(a.id_derechohabiente,62)    AS aivs_virtual_92, ",
                   "fn_sep_suma_saldos(a.id_derechohabiente,61)  AS saldo_virtual_97, ",
                   "fn_sep_suma_aivs(a.id_derechohabiente,61)    AS aivs_virtual_97, ",
                   "a.descripcion ",
                   "FROM tmp_registro_1E a ",
                   "INTO TEMP tmp_tabla_1e "
      EXECUTE IMMEDIATE v_query_1

      --Llenando la tabla temporal con los resultados
      LET v_query_2 = "SELECT invadido,asociado,TO_CHAR(f_apertura,'%d/%m/%Y'),TO_CHAR(f_liquida,'%d/%m/%Y'),saldo_virtual_92,aivs_virtual_92,saldo_virtual_97,aivs_virtual_97,descripcion FROM tmp_tabla_1e WHERE 1=1 "
      IF filtro_liq.vl_f_ini_liquida IS NOT NULL AND filtro_liq.vl_f_fin_liquida IS NOT NULL THEN
         LET v_query_2 = v_query_2 , " AND f_liquida BETWEEN '",tmp_f_ini_liquida,"' AND '",tmp_f_fin_liquida,"'" CLIPPED  
      END IF
      IF filtro_liq.vl_f_ini_liquida IS NOT NULL AND filtro_liq.vl_f_fin_liquida IS NULL THEN
         LET v_query_2 = v_query_2 , " AND f_liquida > '",tmp_f_ini_liquida,"'" CLIPPED  
      END IF
      IF filtro_liq.vl_f_ini_liquida IS NULL AND filtro_liq.vl_f_fin_liquida IS NOT NULL THEN
         LET v_query_2 = v_query_2 , " AND f_liquida < '",tmp_f_fin_liquida,"'" CLIPPED  
      END IF
      LET v_query_2 = v_query_2 , " ORDER BY 1 " CLIPPED
      
      PREPARE prp_tabla_res_liq FROM v_query_2
      DECLARE cur_tabla_res_liq CURSOR FOR prp_tabla_res_liq
      LET i = 1
      FOREACH cur_tabla_res_liq INTO t_resultados_liq[i].v_tl_invadido,
                                     t_resultados_liq[i].v_tl_virtual,
                                     t_resultados_liq[i].v_tl_f_apertura,
                                     t_resultados_liq[i].v_tl_f_liquida,
                                     t_resultados_liq[i].v_tl_sv92,
                                     t_resultados_liq[i].v_tl_aiv92,
                                     t_resultados_liq[i].v_tl_sv97,
                                     t_resultados_liq[i].v_tl_aiv97,
                                     t_resultados_liq[i].v_tl_descripcion
         LET t_resultados_liq[i].v_tl_sv_dia_92 = t_resultados_liq[i].v_tl_aiv92 * v_valor_aiv
         LET t_resultados_liq[i].v_tl_sv_dia_97 = t_resultados_liq[i].v_tl_aiv97 * v_valor_aiv
         LET t_resultados_liq[i].v_tl_total_pesos = t_resultados_liq[i].v_tl_sv_dia_97 + t_resultados_liq[i].v_tl_sv_dia_92
         LET i = i+1
      END FOREACH
      CALL t_resultados_liq.deleteElement(i)
      LET i = i-1
      CLOSE cur_tabla_res_liq
      FREE  cur_tabla_res_liq
      RETURN i
   END IF
   
END FUNCTION

--**////////////////////////////////////////////
FUNCTION fn_genera_reporte()

   DEFINE v_report_handler om.SaxDocumentHandler
   DEFINE v_nomb_reporte    STRING
   DEFINE c                 INTEGER
   DEFINE r_total_cuentas   INTEGER
   DEFINE r_total_v92       DECIMAL(12,2)
   DEFINE r_total_v97       DECIMAL(12,2)

   IF c_tipo_value == 1 THEN
      SELECT COUNT(*)
         INTO r_total_cuentas
         FROM tmp_registro_1e

      IF(fgl_report_loadCurrentSettings(v_ruta_bin CLIPPED||"/SEPC902.4rp"))THEN
         LET v_nomb_reporte = p_usuario_cod CLIPPED, "-SEPC90-", 
                             "00000" USING "&&&&&", "-", 
                             "00000" USING "&&&&&", "-", 
                             "00002" USING "&&&&&",
                             ".pdf"
         #Ruta de salida del reporte
         CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nomb_reporte)
            # COnfigura la salida en tipo PDF
         CALL fgl_report_selectDevice("PDF")
         # Indica que no hay previsualizacion
         CALL fgl_report_selectPreview(0)
         # Asigna la configuración en el menejador del reporte
         LET v_report_handler = fgl_report_commitCurrentSettings()

         START REPORT fn_reporte_virtuales TO XML HANDLER v_report_handler
            FOR c = 1 TO r_total_cuentas
               OUTPUT TO REPORT fn_reporte_virtuales(t_resultados[c].*,r_total_cuentas)
            END FOR
         FINISH REPORT fn_reporte_virtuales
      
         DISPLAY "Reporte:" TO lbl_reporte
         DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nomb_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>"  TO obj_reporte
      
      ELSE
         CALL fn_mensaje("AVISO","Ocurrió un error al cargar plantilla de reporte","information")
      END IF
   END IF
   
   --Obteniendo datos del reporte
   --GENERANDO REPORTE TIPO 2
   IF c_tipo_value == 2 THEN
      SELECT COUNT(*)
         INTO r_total_cuentas
         FROM tmp_tabla_1e
      SELECT SUM(saldo_virtual_92),SUM(saldo_virtual_97)
         INTO r_total_v92,r_total_v97
         FROM tmp_tabla_1e

      IF(fgl_report_loadCurrentSettings(v_ruta_bin CLIPPED||"/SEPC901.4rp"))THEN
         LET v_nomb_reporte = p_usuario_cod CLIPPED, "-SEPC90-", 
                             "00000" USING "&&&&&", "-", 
                             "00000" USING "&&&&&", "-", 
                             "00001" USING "&&&&&",
                             ".pdf"
         #Ruta de salida del reporte
         CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nomb_reporte)
            # COnfigura la salida en tipo PDF
         CALL fgl_report_selectDevice("PDF")
         # Indica que no hay previsualizacion
         CALL fgl_report_selectPreview(0)
         # Asigna la configuración en el menejador del reporte
         LET v_report_handler = fgl_report_commitCurrentSettings()

         START REPORT fn_reporte_virtuales_liq TO XML HANDLER v_report_handler
            FOR c = 1 TO r_total_cuentas
               OUTPUT TO REPORT fn_reporte_virtuales_liq(t_resultados_liq[c].*,r_total_cuentas,r_total_v92,r_total_v97)
            END FOR
         FINISH REPORT fn_reporte_virtuales_liq
      
         DISPLAY "Reporte:" TO lbl_reporte_liq
         DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nomb_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>"  TO obj_reporte_liq
      
      ELSE
         CALL fn_mensaje("AVISO","Ocurrió un error al cargar plantilla de reporte","information")
      END IF
   END IF
   
END FUNCTION

--****************************************
REPORT fn_reporte_virtuales_liq(v_tabla_resultado,r_total_cuentas,r_total_v92,r_total_v97)
   DEFINE v_tabla_resultado RECORD
      v_r_invadido   CHAR(11),
      v_r_virtual    CHAR(11),
      v_r_f_apertura CHAR(10),
      v_r_f_liquida  CHAR(10),
      v_r_total_pesos DECIMAL(12,2),
      v_r_viv92      DECIMAL(12,2),
      v_r_viv92_dia  DECIMAL(12,2),
      v_r_aiv92      DECIMAL(12,2),
      v_r_viv97      DECIMAL(12,2),
      v_r_viv97_dia  DECIMAL(12,2),
      v_r_aiv97      DECIMAL(12,2),
      v_r_descripcion CHAR(40)
   END RECORD
   DEFINE r_f_emision       CHAR(11)
   DEFINE r_total_cuentas   INTEGER
   DEFINE r_total_v92       DECIMAL(12,2)
   DEFINE r_total_v97       DECIMAL(12,2)
   --DEFINE max_pages         INTEGER   
   FORMAT 
      FIRST PAGE HEADER
         LET r_f_emision = TODAY USING "dd/mm/yyyy"
         --LET max_pages = (r_total_cuentas / 32.00000000000001) + 1 --ATENCION: Recalcular si se modifica el archivo
         PRINTX filtro_liq.*,r_f_emision,r_total_cuentas,r_total_v92,r_total_v97
      
      ON EVERY ROW
            PRINTX v_tabla_resultado.*

END REPORT
--****************************************
REPORT fn_reporte_virtuales(v_tabla_resultado,r_total_cuentas)
   DEFINE v_tabla_resultado RECORD
      v_r_invadido   CHAR(11),
      v_r_virtual    CHAR(11),
      v_r_f_apertura CHAR(10),
      v_r_descripcion CHAR(40)
   END RECORD
   DEFINE r_total_cuentas   INTEGER

   DEFINE r_f_emision       CHAR(11)
   DEFINE max_pages         INTEGER
   
   FORMAT 
      FIRST PAGE HEADER
         LET r_f_emision = TODAY USING "dd/mm/yyyy"
         LET max_pages = (r_total_cuentas / 32.00000000000001) + 1 --ATENCION: Recalcular si se modifica el archivo
         PRINTX filtro.*,r_f_emision,r_total_cuentas,max_pages
      
      ON EVERY ROW
            PRINTX v_tabla_resultado.v_r_invadido,
                   v_tabla_resultado.v_r_virtual,
                   v_tabla_resultado.v_r_f_apertura,
                   v_tabla_resultado.v_r_descripcion

END REPORT