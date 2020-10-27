################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 30/03/2016                                      #
################################################################################
################################################################################
#Proyecto          => INFONAVIT                                                #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL57                                                   #
#Objetivo          => Consulta de Histórico de Aportaciones Subsecuentes Sin   #
#                     Conciliar                                                #
#Fecha inicio      => 30/03/2016                                               #
################################################################################
DATABASE safre_viv

GLOBALS

   DEFINE g_sql_txt       STRING,      --Consultas
          g_usuario       VARCHAR(30), --Almacena al usuario
          g_proceso_cod   LIKE cat_proceso.proceso_cod, --codigo del proceso
          g_opera_cod     LIKE cat_operacion.opera_cod, --codigo de operacion
          g_pid           DECIMAL(9,0),
          g_folio         DECIMAL(9,0),
          opera_cod       INTEGER 
   
   DEFINE v_mensaje       STRING
   DEFINE f_ventana       ui.Window,   --Define las propìedades de la Ventana
          f_forma         ui.Form      --Define las propiedades de la forma         
   DEFINE v_indice        SMALLINT

   --Datos de entrada
   DEFINE p_nss           CHAR(11)      
   
   DEFINE v_arr_his DYNAMIC ARRAY OF RECORD
      nss            CHAR(11),
      nrp            CHAR(11),  
      periodo_pago   CHAR(6),
      f_pago         DATE,
      folio_sua      DECIMAL(6,0),
      folio_liquida  DECIMAL(9,0), 
      folio_ap_subs  DECIMAL(9,0),
      imp_apo_aivs   DECIMAL(18,6),
      imp_apo_pat    DECIMAL(12,2),      
      des_concilia   CHAR(50)
   END RECORD

   DEFINE v_comando_dos            STRING

END GLOBALS

MAIN

   DEFINE v_tipo_proceso      SMALLINT,    --Forma como ejecutara el programa 
          v_nom_prog          VARCHAR(30), --Almacena opción del menú 
          v_nom_archivo       STRING       --Nombre del archivo          
 
   DEFINE bnd_consulta        SMALLINT
   DEFINE v_bnd_existe        INTEGER
   DEFINE tpo_ajuste          SMALLINT
   DEFINE p_programa          CHAR(10)     

   CALL STARTLOG (g_usuario CLIPPED|| ".DISL57.log")
          
   --Recibe valores de argumentos
   LET p_programa     = "DISL57"
   LET g_usuario      = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)

   
   LET g_proceso_cod  = 933
   LET tpo_ajuste = 0 

   LET v_bnd_existe = 0   

   --se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   LET v_nom_archivo = ""   
   LET bnd_consulta  = 0   

   CLOSE WINDOW SCREEN   

   OPEN WINDOW w1 WITH FORM "DISL571"
      DIALOG ATTRIBUTES(UNBUFFERED)      

      INPUT BY NAME p_nss
         ON ACTION ACCEPT
            IF p_nss IS NULL THEN 
               CALL fn_mensaje("Aviso","Debe capturar un NSS","stop")
               NEXT FIELD p_f_final 
            END IF
            
            LET v_bnd_existe = 0            
            CALL fn_consultar_info(p_nss) RETURNING v_bnd_existe

            --DISPLAY "v_bnd_existe: ",v_bnd_existe
            
            IF v_bnd_existe >= 1 THEN
               CALL f_forma.setElementHidden("gr_respuesta",0)  --Muestra el resultado de la consulta
               DISPLAY ARRAY v_arr_his TO r_respuesta.*
                  ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)            
                  
                  ON ACTION cancelar 
                     EXIT PROGRAM

                  ON ACTION reporte
                     CALL fn_generar_reporte(p_nss) 

                  ON ACTION archivo
                     CALL fn_genera_interfase()    
                     
               END DISPLAY                

            ELSE
               CALL fn_mensaje("Aviso","No hay registros disponibles.","stop") 
               EXIT PROGRAM
            END IF
            

            BEFORE INPUT 
               LET f_ventana = ui.Window.getCurrent()
               LET f_forma   = f_ventana.getForm()

               CALL f_forma.setElementHidden("gr_respuesta",1)  --Oculta el resultado de la consulta               
        
      END INPUT
          
      
      ON ACTION cancelar
         EXIT DIALOG
          
    END DIALOG 
  CLOSE WINDOW w1 

END MAIN 


FUNCTION fn_consultar_info(v_nss) 
   DEFINE v_nss        CHAR(11)
   DEFINE i_int        INTEGER
   DEFINE ind_concilia SMALLINT

  -- Obtiene las cifras globales de los registros integrados
  LET g_sql_txt = "\n SELECT af.nss, ",
                  "\n        ds.nrp, ",
                  "\n        ds.periodo_pago, ",
                  "\n        ds.f_pago, ",
                  "\n        ds.folio_sua, ",
                  "\n        ds.folio_liquida, ",
                  "\n        ds.folio_ap_subs, ",
                  "\n        ds.imp_apo_aivs, ",
                  "\n        ds.imp_apo_pat, ",
                  "\n        ds.ind_concilia ", 
                  "\n   FROM afi_derechohabiente af, ", 
                  "\n        dis_as_sin_conciliar ds ",
                  "\n  WHERE af.id_derechohabiente = ds.id_derechohabiente "

   IF v_nss IS NOT NULL THEN
      LET g_sql_txt = g_sql_txt, "\n    AND af.nss = '",v_nss,"'"
   END IF 
  
   LET g_sql_txt = g_sql_txt, "\n ORDER BY ds.periodo_pago DESC "

   --DISPLAY "g_sql_txt: -",g_sql_txt
   PREPARE ps_his FROM g_sql_txt

 
   DECLARE cur_his CURSOR FOR ps_his

   LET i_int = 1

   FOREACH cur_his INTO v_arr_his[i_int].nss, 
                        v_arr_his[i_int].nrp,
                        v_arr_his[i_int].periodo_pago,
                        v_arr_his[i_int].f_pago,
                        v_arr_his[i_int].folio_sua,
                        v_arr_his[i_int].folio_liquida,
                        v_arr_his[i_int].folio_ap_subs, 
                        v_arr_his[i_int].imp_apo_aivs, 
                        v_arr_his[i_int].imp_apo_pat, 
                        ind_concilia

      CASE ind_concilia
         WHEN 1
            LET v_arr_his[i_int].des_concilia = ind_concilia, " - CONCILIADO"
         WHEN 2
            LET v_arr_his[i_int].des_concilia = ind_concilia, " - CARGADO EN ARCHIVO PARA CONCILIAR"
         WHEN 3 
            LET v_arr_his[i_int].des_concilia = ind_concilia, " - PRELIQUIDADO"
         WHEN 4 
            LET v_arr_his[i_int].des_concilia = ind_concilia, " - LIQUIDADO"
      END CASE
                               
      LET i_int = i_int + 1
   END FOREACH

   LET i_int = i_int - 1   
   CALL v_arr_his.deleteElement(v_arr_his.getLength()) 

   CLOSE cur_his
   FREE cur_his

   RETURN i_int
END FUNCTION


#Objetivo: Genera reporte en pdf de la Bitácora de Validación Telefónica
FUNCTION fn_generar_reporte(v_nss) 
   DEFINE v_nss        CHAR(11)          

   DEFINE  manejador_rpt      om.SaxDocumentHandler, --Contenedor documentos reporte
           v_rep_indice       INTEGER,
           v_fecha_actual     DATE
  
   LET v_rep_indice = 1  
  
   -- Botón para generar el reporte en PDF de la consulta
   IF fgl_report_loadCurrentSettings("DISL571.4rp") THEN 
      CALL fgl_report_selectDevice ("PDF")
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   END IF

   LET v_fecha_actual = TODAY  

   --Inicia el reporte de registros con rechazo
   START REPORT rep_bitacora TO XML HANDLER manejador_rpt
  
   FOR v_rep_indice = 1 TO  v_arr_his.getLength()
     
      OUTPUT TO REPORT rep_bitacora(g_usuario,
                                    v_fecha_actual,
                                    v_nss,                                    
                                    v_arr_his[v_rep_indice].*,
                                    v_rep_indice)
   END FOR
  
  FINISH REPORT rep_bitacora
END FUNCTION



#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_bitacora(g_usuario,
                    v_fecha_actual,
                    v_nss,                                     
                    v_arr_his,
                    v_rep_indice)

   DEFINE v_nss      CHAR(11)

   DEFINE v_arr_his RECORD
      nss            CHAR(11),
      nrp            CHAR(11),  
      periodo_pago   CHAR(6),
      f_pago         DATE,
      folio_sua      DECIMAL(6,0),
      folio_liquida  DECIMAL(9,0), 
      folio_ap_subs  DECIMAL(9,0),
      imp_apo_aivs   DECIMAL(18,6),
      imp_apo_pat    DECIMAL(12,2),      
      des_concilia   CHAR(50)
   END RECORD

   DEFINE v_rep_indice  INTEGER,
          v_tp_rep      SMALLINT

  DEFINE g_usuario         VARCHAR(30), --Almacena al usuario
         v_fecha_actual    DATE

  FORMAT

   FIRST PAGE HEADER
      PRINTX g_usuario
      PRINTX v_fecha_actual USING "dd-mm-yyyy"
      PRINTX v_nss

   ON EVERY ROW
      PRINTX v_arr_his.nss
      PRINTX v_arr_his.nrp
      PRINTX v_arr_his.periodo_pago
      PRINTX v_arr_his.f_pago USING "dd-mm-yyyy"
      PRINTX v_arr_his.folio_sua USING "&&&&&&"
      PRINTX v_arr_his.folio_liquida USING "&&&&&&&&&"
      PRINTX v_arr_his.folio_ap_subs USING "&&&&&&&&&"
      PRINTX v_arr_his.imp_apo_aivs USING "###,###,###,##&.&&&&&&"
      PRINTX v_arr_his.imp_apo_pat USING "#,###,###,##&.&&"
      PRINTX v_arr_his.des_concilia      

   ON LAST ROW 
      PRINTX v_rep_indice

END REPORT


# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_interfase()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_indice                 INTEGER

  DEFINE mensaje STRING

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_as_sc_his_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_as_sc_his_" || v_ddmmaaaa  --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA|",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = " NSS|",p_nss
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = " NSS |NRP |PERIODO PAGO |FECHA PAGO |FOLIO SUA |FOLIO LIQUIDA |FOLIO CARGA |IMPORTE EN AIVS |IMPORTE EN PESOS |ESTADO |"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_indice = 1
 
  FOR v_indice = 1 TO v_arr_his.getLength()
      --Concatenación del detalle
      LET v_detalle = v_arr_his[v_indice].nss, "|",
                      v_arr_his[v_indice].nrp, "|",
                      v_arr_his[v_indice].periodo_pago, "|",
                      v_arr_his[v_indice].f_pago USING "dd-mm-yyyy", "|",                     
                      v_arr_his[v_indice].folio_sua USING "#####&","|",
                      v_arr_his[v_indice].folio_liquida USING "########&", "|",
                      v_arr_his[v_indice].folio_ap_subs USING "########&", "|",
                      v_arr_his[v_indice].imp_apo_aivs USING "###,###,###,##&.&&&&&&", "|",
                      v_arr_his[v_indice].imp_apo_pat USING "#,###,###,##&.&&", "|",
                      v_arr_his[v_indice].des_concilia CLIPPED, "|"
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR

  --Escribe el sumario
  LET v_sumario = " TOTAL DE REGISTROS |",v_indice,"| "
  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  LET mensaje = "Se ha generado el archivo Histórico de Aportaciones Subsecuentes Sin Conciliar\nen la ruta ",v_ruta_nomarch

  CALL fn_mensaje("Aviso", mensaje, "stop")
  
END FUNCTION 

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,       --Cadena de comando a ejecutar
    v_consecutivo            INTEGER       --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(22)      --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[16,18] --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION