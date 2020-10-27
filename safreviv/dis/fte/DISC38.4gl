#####################################################################
#Ultima Modificación => 22/03/2018                                  #
#Modificado Por    => Cecilia Angel Ceballos                        #
#####################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Owner             => E.F.P.                                        #
#Programa          => Créditos en Trámite                           #
#Fecha             => 15/06/2017                                    #
#By                => CARLOS OMAR CRUZ CUVILLOS                     #
#Sistema           => DIS                                           #
#####################################################################
DATABASE safre_viv
  DEFINE w ui.Window
  DEFINE f ui.Form

----DEFINICION DE VARIABLES GLOBALES, PARAMETROS ENVIADOS DESDE EL MENÚ
  DEFINE  g_pid                 LIKE bat_ctr_proceso.pid      -- ID del proceso
  DEFINE  g_proceso_cod         LIKE cat_proceso.proceso_cod  -- Codigo del proceso
  DEFINE  g_opera_cod           LIKE cat_operacion.opera_cod  -- Codigo de operacion
  DEFINE  g_usuario             LIKE seg_usuario.usuario_cod  -- Clave del usuario
  DEFINE  g_tipo_ejecucion      SMALLINT                      -- Tipo Ejecución
  DEFINE  g_nom_ventana         STRING                        -- Titulo de la ventana
  DEFINE  g_programa            STRING
  DEFINE  g_ruta_bitacora       CHAR(40)
  DEFINE  g_ruta_bin            CHAR(40)
  DEFINE  g_mensaje             STRING
  DEFINE  g_total_AIVS          DECIMAL
  DEFINE  g_total_pesos         DECIMAL
  DEFINE  g_total_Amorti        DECIMAL

  DEFINE arr_consulta           DYNAMIC ARRAY OF RECORD
    id_derechohabiente          LIKE afi_derechohabiente.id_derechohabiente,
    nss                         LIKE afi_derechohabiente.nss,
    nrp                         LIKE dis_crd_tramite.nrp,
    periodo_pago                LIKE dis_crd_tramite.periodo_pago,
    folio_sua                   LIKE dis_crd_tramite.folio_sua,
    aiv_ap_pat                  LIKE dis_crd_tramite.aiv_ap_pat,
    imp_ap_pat                  LIKE dis_crd_tramite.imp_ap_pat,
    imp_am_cre                  LIKE dis_crd_tramite.imp_am_cre,--decimal(12,2),
    f_pago                      LIKE dis_crd_tramite.f_pago,
    folio_reg_pagos             LIKE dis_crd_tramite.folio_reg_pagos,
    folio_liquida               LIKE dis_crd_tramite.folio_liquida,
    num_crd_ifv                 LIKE dis_crd_tramite.num_crd_ifv
  END RECORD

  DEFINE
    g_sql_txt                   STRING,
    v_proc_entra                SMALLINT,
    v_proc_val                  SMALLINT,
    v_cod_conv                  SMALLINT,
    v_desc_proc_val             CHAR(40),
    v_mensaje_val               STRING,
    p_proceso_cod               SMALLINT
 
MAIN
  --Varaibles Locales
  DEFINE v_archivo_log          STRING
  DEFINE  id_derechohabiente    CHAR(11)
  DEFINE  folio                 INTEGER

  ---SE INCORPORA COMO PARAMETROS ENVIADOS DESDE EL MENU EL PROCESO Y CODIGO DE OPERACION
  LET g_usuario        = ARG_VAL(1)
  LET g_tipo_ejecucion = ARG_VAL(2)
  LET g_nom_ventana    = ARG_VAL(3)
  --LET g_proceso_cod    = ARG_VAL(4)
  --LET g_opera_cod      = ARG_VAL(5)
  LET g_programa       = "DISC38"
  LET p_proceso_cod    = 935

  SELECT ruta_listados,ruta_bin
  INTO   g_ruta_bitacora,g_ruta_bin
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"
  
  LET v_archivo_log = g_ruta_bitacora CLIPPED ,"/",g_usuario CLIPPED, ".",g_programa,".log"
  CALL STARTLOG(v_archivo_log)
  DISPLAY "Ruta Bitacora " || v_archivo_log
  DISPLAY "RUTA BINARIOS"  || g_ruta_bin
   
  CALL ui.Interface.setText(g_nom_ventana)
  ---SE INTEGRAN PARA INDICAR QUE LA APLICACIÓN DEPENDE DEL CONTAINER “MenuSAFRE”
  --CLOSE WINDOW SCREEN
  --CALL ui.Interface.setName(v_programa)
   
  --CALL ui.Interface.setType("child")
  --CALL ui.Interface.setContainer("MenuSAFRE")
  
  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
                  "AND    a.cod_proceso_valida = b.proceso_cod ",
                  "AND    a.cod_convivencia    = 0             ",
                  "ORDER BY cod_proceso_valida   "
  PREPARE ps_val_proc FROM g_sql_txt
  DECLARE cur_val_proc CURSOR FOR ps_val_proc
  FOREACH cur_val_proc INTO v_proc_entra,
                            v_proc_val,
                            v_desc_proc_val
    IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH

  LET id_derechohabiente = 1
  WHILE id_derechohabiente <> -1
    CALL fn_busqueda_cliente() RETURN id_derechohabiente ,folio
  END WHILE 
   
END MAIN

#FUNCION PARA OBTENER LOS VALORES BUSCADOS POR EL CLIENTE
FUNCTION fn_busqueda_cliente()
  --Varibles de retorno
  DEFINE r_id_derechohabiente   DECIMAL(9,0)
  DEFINE r_folio_sua            DECIMAL(6,0)
  DEFINE v_condicion            STRING
  DEFINE v_contador             INTEGER
  DEFINE v_forma                STRING
  DEFINE v_query                STRING 
  DEFINE v_nss                  CHAR(11)
  DEFINE v_folio_liquida        STRING
   
  LET v_forma         = g_ruta_bin CLIPPED, "/",g_programa
  LET v_nss           = "null"
  LET v_folio_liquida = "null"
  LET g_total_AIVS    = 0
  LET g_total_pesos   = 0
  LET g_total_Amorti  = 0

  OPEN WINDOW g_programa WITH FORM v_forma ATTRIBUTES(TEXT="Búsqueda Cliente")
  --OPEN WINDOW DISC38 WITH FORM "DISC38" ATTRIBUTES(TEXT="Búsqueda Cliente")
    LET w = ui.Window.getCurrent()
    LET f = w.getForm()
    CALL f.setElementHidden("group2",1) 
    
    DIALOG ATTRIBUTES (UNBUFFERED)
      CONSTRUCT BY NAME v_condicion ON nss,folio_liquida 
      END CONSTRUCT

      DISPLAY ARRAY arr_consulta TO tb_busqueda.*
      END DISPLAY 

      --DESHABILITO ALGUNAS ACCIONES PARA QUE NO SE ENVIEN HASTA ASEGURARSE QUE EXISTAN DATOS
      BEFORE DIALOG
        CALL DIALOG.setactionhidden("close",1)
        CALL DIALOG.setActionActive("extractor",0)
        CALL DIALOG.setActionActive("reporte",0)
        
        ON ACTION ACCEPT
           CALL arr_consulta.CLEAR()
           LET v_query ="SELECT "
                      ||" b.id_derechohabiente,"
                      ||" b.nss, "
                      ||" a.nrp, "
                      ||" a.periodo_pago,"                        
                      ||" a.folio_sua, "                           
                      ||" a.aiv_ap_pat, "                        
                      ||" a.imp_ap_pat, "                         
                      ||" a.imp_am_cre, "                             
                      ||" a.f_pago, "                              
                      ||" a.folio_reg_pagos, "
                      ||" a.folio_liquida, "
                      ||" a.num_crd_ifv" 
                      ||" FROM dis_crd_tramite a, afi_derechohabiente b "    
                      ||" WHERE "                                       
                      || v_condicion
                      ||" AND a.id_derechohabiente = b.id_derechohabiente "
           --DISPLAY v_query
           PREPARE ppr_consulta FROM v_query
           DECLARE cur_consulta CURSOR FOR ppr_consulta

           LET v_contador = 1
           FOREACH cur_consulta INTO arr_consulta[v_contador].*
             LET arr_consulta[v_contador].num_crd_ifv = arr_consulta[v_contador].num_crd_ifv USING "&&&&&&&&&&"
             --TOTALES PARA EL REPORTE
             LET g_total_AIVS   = g_total_AIVS   + arr_consulta[v_contador].aiv_ap_pat
             LET g_total_pesos  = g_total_pesos  + arr_consulta[v_contador].imp_ap_pat
             LET g_total_Amorti = g_total_Amorti + arr_consulta[v_contador].imp_am_cre
             LET v_contador     = v_contador     + 1

             {IF v_contador > 100 THEN
                 CALL fn_mensaje ("", "Extender el criterio de búsqueda, \n"||
                                      "Se excedio el limite de registros a mostrar" ,"")
                 EXIT FOREACH
             END IF}
           END FOREACH

           CALL arr_consulta.deleteElement(v_contador)

           IF v_contador = 1 THEN
              CALL fn_mensaje("","No existen elementos con el criterio de busqueda seleccionado", "")
              LET r_id_derechohabiente = -1
              LET r_folio_sua          = -1
              CONTINUE  DIALOG
           ELSE 
              IF v_contador > 1 THEN
                 LET  v_nss           = GET_FLDBUF(nss)
                 LET  v_folio_liquida = GET_FLDBUF(folio_liquida )
                 --SI EXISTEN DATOS SE MUESTRA La TABLA Y SE ACTIVAN TODAS LAS OPCIONES.
                 CALL f.setElementHidden("group2",0)
                 CALL DIALOG.setActionActive("extractor",1)
                 CALL DIALOG.setActionActive("reporte",1)
              END IF    
           END IF 

        ON ACTION EXTRACTOR 
           CALL fn_ejecuta_extractor(g_usuario,v_nss,v_folio_liquida )

        ON ACTION REPORTE 
           CALL fn_ejecuta_reporte()
                
        ON ACTION CANCEL
           LET r_id_derechohabiente = -1
           LET r_folio_sua = -1
          EXIT DIALOG
            
    END DIALOG
  CLOSE WINDOW DISC38
END FUNCTION

FUNCTION fn_ejecuta_extractor(p_usuario_cod,v_nss,v_folio_liquida )
  DEFINE v_s_comando          STRING
  DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod    -- usuario que ejecuta el programa
  DEFINE v_i_resultado        INTEGER          
  DEFINE v_proceso            LIKE cat_proceso.proceso_cod 
  DEFINE v_operacion          LIKE cat_operacion.opera_cod 
  DEFINE v_folio              INTEGER
  DEFINE v_reporte            STRING
  DEFINE r_resultado_opera    SMALLINT  --codigo de error fn_actualiza_opera_ini
  DEFINE v_nss                STRING
  DEFINE v_folio_liquida      STRING
  DEFINE v_mensaje            STRING 
  DEFINE  v_spid              STRING

  LET g_pid       = 0
  LET v_proceso   = 935
  LET v_operacion = 1
  LET v_folio     = 0
  LET v_reporte   = "NA"

  LET v_nss           = v_nss.trim()
  LET v_folio_liquida = v_folio_liquida .trim()

  IF ( v_nss.equals("") )THEN
     LET v_nss = "null"
  END IF

  IF ( v_folio_liquida .equals(""))THEN
     LET v_folio_liquida = "null"
  END IF  

  -- se verifica si se puede iniciar la operacion
  CALL fn_valida_operacion(g_pid,v_proceso,v_operacion) RETURNING v_i_resultado

  IF ( v_i_resultado == 0 ) THEN--Valida resultado operacion
     --Se genera un pid para el proceso
     CALL fn_genera_pid(v_proceso, 1, g_usuario) RETURNING g_pid
     DISPLAY "PID Generado ",g_pid

     CALL fn_inicializa_proceso(g_pid,v_proceso,v_operacion,v_folio,"DISC38",v_reporte,p_usuario_cod) 
     RETURNING v_i_resultado
        
     DISPLAY "incializa Proceso :" , v_i_resultado
        
     IF v_i_resultado == 0 THEN -- Incia el proceso
        -- Inicio operacion.
        CALL fn_actualiza_opera_ini(g_pid,v_proceso,v_operacion,v_folio,"DISC38",v_reporte,p_usuario_cod)
        RETURNING r_resultado_opera

        IF (r_resultado_opera  == 0) THEN -- incio de la operacion
           --Se construye la cedena de ejecución del programa lanzado del extractor
           LET v_spid = g_pid
           LET v_s_comando = "nohup time fglrun ",g_ruta_bin CLIPPED,"/DISS46 ",
                             p_usuario_cod CLIPPED       , " " ,
                             g_pid                       , " " ,
                             v_proceso                   , " " ,
                             v_operacion                 , " " ,
                             v_nss                       , " " ,
                             v_folio_liquida                   , " " ,
                             --v_condicion                 , " ",
                             " 1>","/safreviv_lst/bat" CLIPPED,
                             "/nohup:",v_spid  CLIPPED,":",-- USING "&&&&&&&&&",":",
                             v_proceso      USING "&&&&&"   ,":",
                             v_operacion    USING "&&&&&"       ,
                             " 2>&1 &"
           DISPLAY v_s_comando
           RUN v_s_comando

           LET g_mensaje = "Se ha enviado el extractor Créditos en Trámite.",
                           " Puede revisar el avance del proceso en el monitor de ejecución de procesos \n",
                           "con el PID ",g_pid USING "<<<<<<<&"
           CALL fn_mensaje("Atención",g_mensaje,"information")    
        END IF -- incio de la operacion
      END IF -- Valida inicia procesp

      CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
      --CALL fn_mensaje("Atención", v_mensaje, "stop")
  END IF --Valida resultado operacion
END FUNCTION 

FUNCTION fn_ejecuta_reporte()
  DEFINE v_manejador_rpt        om.SaxDocumentHandler --Manejador del documento
  DEFINE v_plantilla_rpt        STRING
  DEFINE v_lengthArray          INTEGER
  DEFINE v_count                INTEGER
    
  LET v_lengthArray   = arr_consulta.getLength()
  LET v_plantilla_rpt = "DISC381.4rp"

  IF fgl_report_loadCurrentSettings (v_plantilla_rpt) THEN
     CALL fgl_report_selectDevice("PDF") 
     CALL fgl_report_selectPreview(1)
     LET  v_manejador_rpt = fgl_report_commitCurrentSettings()

     START REPORT rpt_tramite_restituidos TO XML HANDLER v_manejador_rpt
        FOR v_count = 1 TO v_lengthArray
            OUTPUT TO REPORT rpt_tramite_restituidos (arr_consulta[v_count].*,v_lengthArray)
        END FOR
     FINISH REPORT rpt_tramite_restituidos   
  ELSE 
     CALL fgl_winmessage ("Info", "La plantilla  no puede ser cargada","about")
  END IF 
END FUNCTION 

REPORT rpt_tramite_restituidos(p_rec_consulta,v_registros)
  DEFINE p_rec_consulta         RECORD
         id_derechohabiente     DECIMAL(9,0),
         nss                    CHAR(11),
         nrp                    CHAR(11),
         periodo_pago           CHAR(6),
         folio_sua              DECIMAL(6,0),
         aiv_ap_pat             DECIMAL(18,6),
         imp_ap_pat             DECIMAL(12,2),
         imp_am_cre             DECIMAL(12,2),
         f_pago                 DATE,
         folio_reg_pagos        DECIMAL(9,0),
         folio_liquida          DECIMAL(9,0),
         num_crd_ifv            DECIMAL(10,0)
  END RECORD 

  DEFINE v_fecha                DATE
  DEFINE v_titulo               STRING
  DEFINE v_registros            INTEGER

  FORMAT
    FIRST PAGE HEADER
      LET v_fecha  = TODAY
      LET v_titulo = "Reporte créditos en trámite"
      PRINTX g_usuario,
             v_fecha USING "dd-mm-yyyy", 
             v_titulo,
             v_registros,
             g_total_AIVS,
             g_total_pesos,
             g_total_Amorti
        
    ON EVERY ROW
       --PRINTX p_rec_consulta.*
       PRINTX p_rec_consulta.id_derechohabiente
       PRINTX p_rec_consulta.nss
       PRINTX p_rec_consulta.nrp
       PRINTX p_rec_consulta.periodo_pago
       PRINTX p_rec_consulta.folio_sua
       PRINTX p_rec_consulta.aiv_ap_pat
       PRINTX p_rec_consulta.imp_ap_pat
       PRINTX p_rec_consulta.imp_am_cre
       PRINTX p_rec_consulta.f_pago USING 'dd-mm-yyyy'
       PRINTX p_rec_consulta.folio_reg_pagos
       PRINTX p_rec_consulta.folio_liquida
       PRINTX p_rec_consulta.num_crd_ifv
END REPORT