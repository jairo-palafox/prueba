################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 06/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISL39                                                    #
#Objetivo         => Programa Lanzador Extractor Dispersión de Pagos.          #
#                                                                              #
#Fecha de Inicio  => 07/07/2015                                                #
################################################################################
--Base que se utilizará
DATABASE safre_viv

--Definición de variables globales
GLOBALS
  DEFINE g_sql_txt           STRING,                       --Consultas
         g_usuario           VARCHAR(30),                  --Almacena al usuario
         g_proceso_cod       LIKE cat_proceso.proceso_cod, --Código del proceso
         g_opera_cod         LIKE cat_operacion.opera_cod, --Código de operación
         g_pid               DECIMAL(9,0),
         g_folio             DECIMAL(9,0)

  DEFINE l_comando           STRING
  DEFINE v_ruta_ejecutable   CHAR(40)
  DEFINE v_ruta_listados     CHAR(40)
  DEFINE v_mensaje           STRING

  DEFINE p_programa          CHAR(10),  
         r_bandera           SMALLINT,
         r_nom_archivo       CHAR(40)
         
  DEFINE v_indice            SMALLINT
  DEFINE g_folio             LIKE dis_det_avance_pago.folio,
         g_sql_txt           STRING,
         v_proc_entra        SMALLINT,
         v_proc_val          SMALLINT,
         v_cod_conv          SMALLINT,
         v_desc_proc_val     CHAR(40),
         v_mensaje_val       STRING
  
END GLOBALS

MAIN
  DEFINE v_tipo_proceso      SMALLINT,    --Forma como ejecutara el programa 
         v_nom_prog          VARCHAR(30), --Almacena opción del menú 
         v_nom_archivo       STRING       --Nombre del archivo
    
  --Datos de entrada
  DEFINE v_folio_dis         DECIMAL(9,0) --Folio de liquidación de la dispersión
  DEFINE v_cb_destino        SMALLINT    

  --DEFINE cb                ui.ComboBox
  DEFINE v_ind_llena_cb      SMALLINT
  
  DEFINE bnd_consulta        SMALLINT, 
         f_ventana           ui.Window,   --Define las propìedades de la Ventana
         f_forma             ui.Form      --Define las propiedades de la forma

  DEFINE v_bnd_existe_info   INTEGER

  CALL STARTLOG (g_usuario CLIPPED|| ".DISL39.log")
          
  --Recibe valores de argumentos
  LET g_usuario         = ARG_VAL(1)
  LET v_tipo_proceso    = ARG_VAL(2)
  LET v_nom_prog        = ARG_VAL(3)

  LET g_proceso_cod     = 925
  LET g_opera_cod       = 1

  LET v_bnd_existe_info = 0

  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  --Obtiene las rutas ejecutable
  SELECT ruta_bin 
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  --Se asigna el titulo del programa
  IF ( v_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(v_nom_prog)
  END IF

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", g_proceso_cod,
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

  CALL fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario)
  RETURNING g_folio

  LET v_nom_archivo  = ""
   
  LET bnd_consulta   = 0
  LET v_folio_dis    = 0
  LET v_cb_destino   = 0
  LET v_ind_llena_cb = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISL39"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio_dis, v_cb_destino
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL fn_llenar_combo_destino() RETURNING v_cb_destino, v_ind_llena_cb

          LET v_cb_destino = v_cb_destino

          NEXT FIELD v_folio_dis
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro       
           IF v_folio_dis IS NULL THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Ingrese el folio de dispersión",
                              "about")
              NEXT FIELD v_folio_dis 
           END IF

           IF v_cb_destino IS NULL THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Seleccione el destino de la dispersión.",
                              "about")
              NEXT FIELD v_cb_destino 
           END IF

           IF (v_folio_dis IS NOT NULL AND v_cb_destino IS NOT NULL) THEN
              LET v_bnd_existe_info = 0
              CALL fn_verificar_info_disp(v_folio_dis, v_cb_destino) RETURNING v_bnd_existe_info
           END IF

           DISPLAY "v_bnd_existe_info: ",v_bnd_existe_info

           IF v_bnd_existe_info >= 1 THEN
              --Si se acepta la ejecución se genera PID del proceso
              CALL fn_genera_pid (g_proceso_cod, g_opera_cod, g_usuario) RETURNING g_pid

              IF (fn_valida_operacion(g_pid, g_proceso_cod, 1) = 0 ) THEN
                 --Enlaza el folio referencia 
                 LET g_folio = 0;
                 CALL fn_genera_folio_dis(g_proceso_cod, g_opera_cod, 0, g_usuario)
                 RETURNING g_folio

                 LET p_programa    = "DISS38"
                 LET r_nom_archivo = ""

                 --DISPLAY "Folio -- ",g_folio
                 --Ejecuta la funcion de preliquida y asigna estatus de LISTO
                 CALL fn_inicializa_proceso (g_pid, g_proceso_cod, 1, g_folio, 
                                             p_programa, r_nom_archivo, g_usuario)
                 RETURNING r_bandera

                 --Inicia la operación asignando el estatus de PROCESANDO
                 CALL fn_actualiza_opera_ini(g_pid, g_proceso_cod, g_opera_cod, g_folio, 
                                             p_programa, r_nom_archivo, g_usuario)
                 RETURNING r_bandera

                 --DISPLAY "r_bandera: ", r_bandera
                 --DISPLAY "ANTES DE MANDAR nohup DISS38"

                 --Validaciones 
                 LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS38.42r ",
                                 v_folio_dis," ",
                                 v_cb_destino," ",
                                 g_pid," ",
                                 g_opera_cod," ",
                                 g_proceso_cod," ",
                                 g_usuario, " ",
                                 --f_folio,
                                 " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                 g_pid USING "&&&&&",":",
                                 g_proceso_cod USING "&&&&&",":",
                                 1 USING "&&&&&" ," 2>&1 &"
                 RUN l_comando

                 --CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                 --RETURNING r_bandera
         
                 LET v_mensaje = "Se ha enviado el extractor de dispersión: ",
                                 g_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"

                 CALL fn_mensaje("Extractor de Dispersión", v_mensaje, "information")
                            
                 EXIT DIALOG
              ELSE
                 CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid, g_proceso_cod, 1))
              END IF  --De valida operacion
           ELSE
              CALL fn_mensaje("Aviso","No existe información del folio y destino de la dispersión seleccionados.","stop") 
              EXIT PROGRAM
           END IF  
      END INPUT
      
      ON ACTION cancelar
        EXIT DIALOG      
          
    END DIALOG 
  CLOSE WINDOW w1
END MAIN 

#Objetivo: Genera el ComboBox para los destinos/tipo de dispesion
FUNCTION fn_llenar_combo_destino()              
  DEFINE arr_edo_apo         DYNAMIC ARRAY OF RECORD --Arreglo para almacenar el estado
         cod_edo_aps         SMALLINT,               --Campo de codigo de estado
         desc_edo_aps        VARCHAR(40)
         END RECORD
  
  DEFINE v_cb_destino        DECIMAL(9,0), --Almacena folio en el combobox
         p_fecha             DATE,         --Parámetro de fecha
         v_indice            INTEGER,      --Variable del indice  
         v_QryTxt            STRING,       --Cadena para almacenar Query
         cb                  ui.ComboBox   --Variable de Combobox

  DEFINE i                   SMALLINT
  
  LET cb = ui.ComboBox.forName("v_cb_destino") --Asignación del combo a la forma

  --Validación si el combo es nulo 
  IF cb IS NULL THEN
     ERROR "Form field not found in current form"
     EXIT PROGRAM
  END IF

  LET p_fecha  = TODAY
  LET v_indice = 1
   
  {--Validación del tamaño de la fecha si es 0 no la incluye para el Query
  LET v_QryTxt = "\n SELECT ce.cod_edo_aps,",
                 "\n        ce.cod_edo_aps||'-'||ce.desc_edo_aps",
                 "\n FROM   cat_edo_aps ce",
                 "\n ORDER BY 1 "

                 
  --DISPLAY "Llena combo ",v_QryTxt

  --Prepara la consulta para obtener folios liquidados
  PREPARE ps_estados_apo_sub FROM v_QryTxt
   
  --Limpia el combo
  CALL cb.clear()

  --Declara el cursor para la consulta 
  DECLARE cur_estados_apo_sub CURSOR FOR ps_estados_apo_sub
  FOREACH cur_estados_apo_sub INTO arr_edo_apo[v_indice].cod_edo_aps,
                                   arr_edo_apo[v_indice].desc_edo_aps
    --Agrega elementos al combobox
    CALL cb.addItem(arr_edo_apo[v_indice].cod_edo_aps, arr_edo_apo[v_indice].desc_edo_aps)
      
    --DISPLAY arr_edo_apo[v_indice].cod_edo_aps, " - ", arr_edo_apo[v_indice].desc_edo_aps
    LET v_indice = v_indice + 1
  END FOREACH}

  FOR i = 1 TO 7
      CASE i
        WHEN 1
          CALL cb.addItem("1", "Pago Real")
        WHEN 2
          CALL cb.addItem("2", "Entidades Financieras")
        WHEN 3
          CALL cb.addItem("3", "Inconsistencias")
        WHEN 4
          CALL cb.addItem("4", "Aportaciones Subsecuentes")
        --WHEN 5
        --  CALL cb.addItem("5", "Portabilidad")
        WHEN 6
          CALL cb.addItem("6", "Avances Compensados")

        OTHERWISE
      END CASE

      LET v_indice = v_indice + 1
  END FOR

  --CALL arr_edo_apo.deleteElement(v_indice)
  LET v_indice = v_indice - 1
         
  RETURN v_cb_destino, v_indice
END FUNCTION

FUNCTION fn_verificar_info_disp(p_folio_dis, p_destino) 
DEFINE p_destino             SMALLINT
DEFINE p_folio_dis           DECIMAL(9,0)
DEFINE v_tot_reg             INTEGER
DEFINE ls_query              STRING
DEFINE v_folio_reg_pag       DECIMAL(9,0)

  LET v_tot_reg        = 0       
  LET v_folio_reg_pag  = 0

  LET g_sql_txt = " \n SELECT COUNT(*) ",
                  " \n FROM "

  CASE p_destino
    WHEN 1 --Pago Real
      LET g_sql_txt = g_sql_txt, "          dis_interface_hs hs, ",  
                                 " \n OUTER dis_det_avance_pago avp ",
                                 " \n WHERE hs.id_derechohabiente = avp.id_derechohabiente ",
                                 " \n AND   hs.periodo_pago       = avp.periodo_pago ",
                                 " \n AND   hs.nrp                = avp.nrp ",
                                 " \n AND   avp.tpo_avance        = 181 ",
                                 " \n AND   avp.estado 		      IN (30,40,50,70) ",
                                 " \n AND   hs.folio_liquida      = ", p_folio_dis
    WHEN 2 --Entidades Financieras
      LET g_sql_txt = g_sql_txt, "        dis_interface_ef ef ",
                                    " \n WHERE ef.folio_liquida         = ", p_folio_dis
    WHEN 3 --Inconsistencias
      LET ls_query = " \n SELECT UNIQUE(glo.folio_referencia) ",
                     " \n FROM   dis_info_inconsistente dii, ",
                     " \n        glo_folio glo ",
                     " \n WHERE  dii.folio_liquida = glo.folio ",
                     " \n AND    dii.folio_liquida = ",p_folio_dis

      DISPLAY ls_query
      PREPARE ps_folio_ref FROM ls_query
      EXECUTE ps_folio_ref INTO v_folio_reg_pag

      LET g_sql_txt = g_sql_txt, "          dis_info_inconsistente dii, ",
                                 " \n       cta_his_pagos pag ",
                                 " \n WHERE dii.id_derechohabiente = pag.id_derechohabiente ",
                                 " \n AND   dii.id_referencia      = pag.id_referencia ",
                                 " \n AND   pag.folio              =  ",v_folio_reg_pag,
                                 " \n AND   dii.folio_liquida      = ", p_folio_dis
                                 
    WHEN 4   -- Aportaciones Subsecuentes
      LET g_sql_txt = g_sql_txt, "          dis_ap_subsecuente aps, ",
                                 " \n OUTER dis_interface_ef ef ",
                                 " \n WHERE aps.id_dis_interface_ef = ef.id_dis_interface_ef ",
                                 " \n AND   aps.folio_liquida       = ef.folio_liquida ",
                                 " \n AND   aps.folio               = ", p_folio_dis
    WHEN 5   -- Portabilidad 
      LET g_sql_txt = g_sql_txt, " \n       dis_interface_prt prt ",
                                 " \n WHERE prt.folio_liquida       = ", p_folio_dis

    WHEN 6   -- Avances Compensados
      LET g_sql_txt = g_sql_txt, " \n       dis_compensa_avance ava ",
                                 " \n WHERE ava.folio_dis           = ", p_folio_dis
      
    OTHERWISE
  END CASE

  DISPLAY g_sql_txt
  PREPARE ps_existe_info FROM g_sql_txt
  --EXECUTE ps_existe_info USING p_folio_dis INTO v_tot_reg
  EXECUTE ps_existe_info INTO v_tot_reg

  RETURN v_tot_reg            
END FUNCTION