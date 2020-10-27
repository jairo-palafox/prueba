################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 24/06/2012                                      #
################################################################################

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => ACR                                                      #
#Programa          => ACRL22                                                   #
#Objetivo          => Programa para la liquidacion de saldo de fondo de ahorro #
#Fecha inicio      => 24/06/2012                                               #
################################################################################
DATABASE safre_viv
GLOBALS "ACRG10.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
DEFINE arr_arbol_pre         DYNAMIC ARRAY OF RECORD
          subcuenta_desc     CHAR(30)     ,
          siefore            SMALLINT     ,
          monto_pesos        DECIMAL(28,6),
          monto_acciones     DECIMAL(28,6),
          subcuenta          SMALLINT     ,
          padre_id           STRING       ,
          id                 STRING       ,
          nivel              SMALLINT
       END RECORD
DEFINE v_d_pid                DECIMAL(9,0), -- identificador del proceso
       v_i_opera_cod          LIKE cat_operacion.opera_cod, -- codigo de operacion
       v_i_proceso_cod        LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_folio                DECIMAL (9,0),
       p_v_nom_prog           VARCHAR(30) -- nombre del programa

MAIN 
DEFINE p_tipo_ejecucion       SMALLINT, -- forma como ejecutara el programa
       v_i_estado             SMALLINT, --estado al que se va a actualizar
       v_s_qryTxt             STRING, -- guarda una sentencia SQL a ejecutar
       v_c_programa_cod       LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
       v_v_nom_archivo        LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
       v_si_tpo_originacion   LIKE cre_acreditado.tpo_originacion, -- tipo de originación
       v_cuenta_accion        SMALLINT, --variable que guarda si existe valor de una accion en la fecha actual
       v_ban_salir            SMALLINT,
       v_cta_reg              INTEGER,
       v_s_mensaje            STRING, -- mensaje a mostrar al usuario
       v_c_ruta_list_bat      LIKE seg_modulo.ruta_listados, -- ruta listados de bat
       r_c_ruta_bin           LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
       r_ruta_listados        LIKE seg_modulo.ruta_listados, -- ruta de listados del módulo
       r_b_valida             SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
       r_valida               SMALLINT, -- indica si el stored tuvo error
       v_s_comando            STRING -- comando para ser ejecutado
          
   -- se asignan los parametros recibidos
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_v_nom_prog     = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".ACRL22.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inializan las variables
   LET v_i_proceso_cod = g_proc_cod_acr_liquida_f72 -- liquidación fondo 72
   LET v_i_opera_cod = 1 -- liquidación fondo 72
   LET v_d_pid = 0
   LET v_v_nom_archivo = "N/A"
   LET v_c_programa_cod = "ACRL22"
   LET v_folio   = NULL
   LET v_cta_reg = 0
   LET v_i_estado = 140 -- liquidado

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("acr") RETURNING r_c_ruta_bin, r_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat
   
   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM      
   END IF 

   -- valida si existe precio de fondo de inversión
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM glo_valor_fondo\n",
                    "  WHERE fondo = 11\n",
                    "    AND f_valuacion = TODAY"

   PREPARE prp_glo_val_fondo FROM v_s_qryTxt
   EXECUTE prp_glo_val_fondo INTO v_cuenta_accion

   DISPLAY "v_cuenta_accion ",v_cuenta_accion
   -- se verifica si se encontró precio de fondo para el día de hoy
   IF v_cuenta_accion = 0 THEN
      LET v_s_mensaje = "No es posible realizar la liquidación, ya que no hay precios\n",
                        "de fondo de inversión"
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")   
      EXIT PROGRAM
   END IF    
   
   -- validar si existe registros a liquidar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_restitucion_fondo72\n",
                    "  WHERE estado = 10"

   PREPARE prp_cuenta_peliq FROM v_s_qryTxt
   EXECUTE prp_cuenta_peliq INTO v_cta_reg

   DISPLAY "v_cta_reg ",v_cta_reg

   -- se verifica si se encontró información para liquidar
   IF v_cta_reg = 0 THEN
      LET v_s_mensaje = "No es posible realizar la liquidación, ya que no se encontró\n",
                        "información para liquidar "
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")
      EXIT PROGRAM
   END IF    

   --Se obtiene el folio de restitución
   SELECT UNIQUE (folio) INTO v_folio
     FROM dse_restitucion_fondo72
    WHERE estado = 10

   DISPLAY "Folio de dse_restitucion_fondo72 -- ",v_folio
 
   
   -- llama a la función que muestra la información a liquidar
   CALL fn_folio_liquidar72(p_usuario_cod, v_i_proceso_cod, v_i_opera_cod, v_folio,v_si_tpo_originacion) 
   RETURNING v_ban_salir, v_folio

   --DISPLAY "FOLIO DE LIQUIDACION: ", v_folio_liquida
   DISPLAY "Folio input  ",v_folio
   IF v_ban_salir = TRUE THEN
      EXIT PROGRAM  
   END IF

   -- se invoca la funcion que genera el pid
   LET v_d_pid = fn_genera_pid(v_i_proceso_cod, v_i_opera_cod, p_usuario_cod)
   DISPLAY "v_d_pid  ",v_d_pid

   -- se invoca la funcion que inicializa el proceso
   LET r_b_valida = fn_inicializa_proceso(v_d_pid, v_i_proceso_cod, v_i_opera_cod, v_folio, v_c_programa_cod, "N/A", p_usuario_cod)
   DISPLAY "fn_inicializa_proceso  ",r_b_valida

   
   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(v_d_pid,v_i_proceso_cod,v_i_opera_cod,
                                           v_folio, v_c_programa_cod,
                                           v_v_nom_archivo, p_usuario_cod)

   -- se verifica si fue posible inicializar la operacion
   IF ( r_b_valida = 0 ) THEN
   
      -- aqui se invoca la ejecucion del programa lanzado
      LET v_s_comando = "nohup time fglrun ",r_c_ruta_bin CLIPPED,"/ACRP31 ",
                        p_usuario_cod   CLIPPED  , " ",
                        v_d_pid                  , " ",
                        v_i_proceso_cod          , " ",
                        v_i_opera_cod            , " ",
                        v_folio                  , " ",
                        v_v_nom_archivo CLIPPED  , " ",
                        " 1>",v_c_ruta_list_bat CLIPPED,
                        "/nohup:",v_d_pid USING "&&&&&",":",
                        v_i_proceso_cod   USING "&&&&&",":",
                        v_i_opera_cod     USING "&&&&&" ,
                        " 2>&1 &"
      DISPLAY v_s_comando                        
      RUN v_s_comando

      
      LET v_s_mensaje = "Se ha enviado la liquidación con PID: ",v_d_pid CLIPPED,
                           ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
         
      CALL fn_mensaje("Liquidación",v_s_mensaje,"information")
   ELSE
      CALL fn_mensaje("Atención", "No se pudo iniciar la operación","stop")
   END IF 
   
END MAIN 

#Función para mostrar la información en pantalla
FUNCTION fn_folio_liquidar72(p_usuario, p_proceso_cod, p_opera_cod, p_folio,p_si_tpo_originacion) 
   DEFINE p_usuario              CHAR(20),
          p_proceso_cod          SMALLINT,
          p_opera_cod            SMALLINT,
          p_folio                DECIMAL(9,0),          
          p_si_tpo_originacion   LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_index                SMALLINT,
          v_tabla_preliq         CHAR(30),
          v_ban_salir            SMALLINT  

   DEFINE 
      vr_folios_liquidar  RECORD
             folio               DECIMAL(9,0),
             fecha_liquidacion   DATE,
             fecha_proceso       DATE,
             activa              SMALLINT                    
      END RECORD
   DEFINE 
      arr_folios_liquidar DYNAMIC ARRAY OF RECORD
          folio               DECIMAL(9,0),
          fecha_liquidacion   DATE,
          fecha_proceso       DATE,
          activa              SMALLINT               
      END RECORD
   DEFINE 
       arr_folios_pre        DYNAMIC ARRAY OF RECORD
          folio              DECIMAL(9,0),
          fecha_liquidacion  DATE,
          fecha_proceso      DATE
       END RECORD

   CALL arr_arbol_pre.clear()
   CALL arr_folios_pre.clear()

   LET vr_folios_liquidar.folio = p_folio
   LET vr_folios_liquidar.fecha_liquidacion = TODAY
   LET vr_folios_liquidar.fecha_proceso = TODAY  
   LET vr_folios_liquidar.activa = 1
   LET v_index = 1
   LET arr_folios_liquidar[v_index].* = vr_folios_liquidar.*
   
   OPEN WINDOW w_selec_pre WITH FORM "ACRL221"

   DIALOG ATTRIBUTES(UNBUFFERED)
         INPUT ARRAY arr_folios_liquidar FROM arr_folios.* ATTRIBUTES(
                                                          APPEND ROW = FALSE,
                                                          INSERT ROW = FALSE,
                                                          DELETE ROW = FALSE,
                                                          AUTO APPEND = FALSE)
         BEFORE INPUT
           CALL DIALOG.setactionhidden("close",1)
           CALL fn_despliega_desc_pre(p_proceso_cod, p_opera_cod)

         BEFORE ROW
            LET v_index       = ARR_CURR()
            LET v_folio = arr_folios_liquidar[v_index].folio

            LET v_tabla_preliq = "cta_fondo72"                    
            
            CALL fn_llena_arbol_montos_pre(p_folio, v_tabla_preliq,p_si_tpo_originacion)
            RETURNING p_folio
            CALL ui.Interface.refresh()
            DISPLAY vr_folios_liquidar.folio TO folio_liquida
            DISPLAY vr_folios_liquidar.fecha_proceso TO f_registro               
            DISPLAY vr_folios_liquidar.fecha_liquidacion TO f_liquida
               
         END INPUT

         DISPLAY ARRAY arr_arbol_pre TO scr1.*   END DISPLAY
         

         ON ACTION ACCEPT
            LET v_ban_salir = FALSE            
            EXIT DIALOG
            

         ON ACTION CANCEL            
          LET v_ban_salir = TRUE           
          EXIT DIALOG
          
      END DIALOG
      
   CLOSE WINDOW w_selec_pre
       
   RETURN v_ban_salir, v_folio
   
   DISPLAY "FOLIO dse -- ", v_folio

   
END FUNCTION 

#Función para llenar la tabla de montos a liquidar
FUNCTION fn_llena_arbol_montos_pre(p_folio, v_tabla, p_si_tpo_originacion)
    
    DEFINE p_folio               INTEGER
    DEFINE v_tabla               STRING
    DEFINE p_si_tpo_originacion  LIKE cre_acreditado.tpo_originacion -- tipo de originación
    DEFINE qry_string            STRING
    DEFINE v_subc                SMALLINT
    DEFINE v_index               INTEGER
    DEFINE v_arbol               INTEGER
    DEFINE cont_arbol            INTEGER
    

    DEFINE arr_nivel2 DYNAMIC ARRAY OF RECORD
        subcuenta            SMALLINT,
        descripcion          CHAR(40),
        siefore              SMALLINT,
        pesos                DECIMAL(28,6),
        acciones             DECIMAL(28,6)
    END RECORD,
        v_d_valor_accion     DECIMAL(28,6)


   SELECT precio_fondo
     INTO v_d_valor_accion
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY




    LET v_index    = 1
    LET v_arbol    = 1
    LET cont_arbol = 4  --Número de subcuentas a evaluar

    CALL arr_arbol_pre.clear()
   
    LET qry_string = " SELECT 40, sum(importe) importe\n",
                     "   FROM dse_restitucion_fondo72\n",
                     "  WHERE folio = ",v_folio

    DISPLAY "Consulta -- ",qry_string
                     
    PREPARE prp_nivel2 FROM qry_string
    EXECUTE prp_nivel2 INTO arr_nivel2[1].subcuenta, arr_nivel2[v_index].acciones
    
    --Obtiene descripción de la subcuenta
    SELECT subcuenta_desc
      INTO arr_nivel2[v_index].descripcion
      FROM cat_subcuenta
     WHERE subcuenta = 40 -- fondo de ahorro
       
    LET arr_nivel2[1].subcuenta = 40
    LET arr_nivel2[1].pesos = arr_nivel2[1].acciones / v_d_valor_accion

    --Llena el arbol a mostrar
    DISPLAY "Tamaño ", arr_nivel2.getLength()
    FOR v_arbol = 1 TO arr_nivel2.getLength()

        LET arr_arbol_pre[v_arbol].subcuenta      = arr_nivel2[v_arbol].subcuenta
        LET arr_arbol_pre[v_arbol].subcuenta_desc = arr_nivel2[v_arbol].subcuenta || "-" || arr_nivel2[v_arbol].descripcion
        LET arr_arbol_pre[v_arbol].monto_pesos    = arr_nivel2[v_arbol].pesos
        LET arr_arbol_pre[v_arbol].monto_acciones = arr_nivel2[v_arbol].acciones
        LET arr_arbol_pre[v_arbol].id             = ""
        LET arr_arbol_pre[v_arbol].nivel          = 1
        LET arr_arbol_pre[v_arbol].padre_id       = ""
        LET arr_arbol_pre[v_arbol].siefore        = 11

    
    END FOR 

    RETURN p_folio
        
END FUNCTION 

#Función para desplegar descripciones de procesos y operaciones
FUNCTION fn_despliega_desc_pre(p_proceso_cod, p_opera_cod)
   DEFINE p_proceso_cod       SMALLINT
   DEFINE p_opera_cod         SMALLINT
   DEFINE v_proceso_desc      CHAR(40)
   DEFINE v_opera_desc      CHAR(40)
   
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   DISPLAY BY NAME v_proceso_desc
   DISPLAY BY NAME v_opera_desc
END FUNCTION 