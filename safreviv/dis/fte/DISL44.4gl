################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 06/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISL44                                                    #
#Objetivo         => Programa Lanzador Extractor Avance Abierto por NRP.       #
#                                                                              #
#Fecha de Inicio  => 18/08/2015                                                #
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
  DEFINE g_folio             LIKE dis_det_avance_pago.folio
  DEFINE
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  DEFINE v_tipo_proceso      SMALLINT,    --Forma como ejecutara el programa 
         v_nom_prog          VARCHAR(30), --Almacena opción del menú 
         v_nom_archivo       STRING       --Nombre del archivo
    
  --Datos de entrada
  DEFINE v_periodo_pago      CHAR(06)     --Periodo de Pago  

  --DEFINE cb                ui.ComboBox
  DEFINE v_ind_llena_cb      SMALLINT

  DEFINE bnd_consulta        SMALLINT, 
         f_ventana           ui.Window,   --Define las propìedades de la Ventana
         f_forma             ui.Form      --Define las propiedades de la forma

  DEFINE v_bnd_existe_info   INTEGER

  CALL STARTLOG (g_usuario CLIPPED|| ".DISL44.log")
          
  --Recibe valores de argumentos
  LET g_usuario         = ARG_VAL(1)
  LET v_tipo_proceso    = ARG_VAL(2)
  LET v_nom_prog        = ARG_VAL(3)
   
  LET g_proceso_cod     = 929
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

  LET v_nom_archivo    = ""
  
  LET bnd_consulta     = 0
  LET v_periodo_pago   = 0
  LET v_ind_llena_cb   = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISL44"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_periodo_pago                    
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          NEXT FIELD v_periodo_pago
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           LET v_bnd_existe_info = 0
           CALL fn_verificar_info_ava_nrp(v_periodo_pago) RETURNING v_bnd_existe_info

           DISPLAY "v_bnd_existe_info: ",v_bnd_existe_info
           IF v_bnd_existe_info >= 1 THEN
              --Si se acepta la ejecución se genera PID del proceso
              CALL fn_genera_pid (g_proceso_cod, g_opera_cod, g_usuario) RETURNING g_pid

              IF (fn_valida_operacion(g_pid, g_proceso_cod, 1) = 0 ) THEN
                 --Enlaza el folio referencia 
                 LET g_folio = 0;
                 CALL fn_genera_folio_dis(g_proceso_cod, g_opera_cod, 0, g_usuario)
                 RETURNING g_folio

                 LET p_programa    = "DISS43"
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
                 IF v_periodo_pago IS NULL THEN
                    LET v_periodo_pago = 0
                 END IF

                 --Validaciones 
                 LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS43.42r ",
                                 v_periodo_pago," ",
                                 --v_cb_destino," ",
                                 g_pid," ",
                                 g_opera_cod," ",
                                 g_proceso_cod," ",
                                 v_bnd_existe_info," ",
                                 g_usuario, " ",
                                 --f_folio,
                                 " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                 g_pid USING "&&&&&",":",
                                 g_proceso_cod USING "&&&&&",":",
                                 1 USING "&&&&&" ," 2>&1 &"
                 RUN l_comando

                 --CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                 --RETURNING r_bandera
         
                 LET v_mensaje = "Se ha enviado el extractor de avance por NRP ",
                                 g_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"

                 CALL fn_mensaje("Extractor de Avance por NRP", v_mensaje, "information")
                            
                 EXIT DIALOG
              ELSE
                 CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid, g_proceso_cod, 1))
              END IF  --De valida operacion
           ELSE
              CALL fn_mensaje("Aviso","No existe información de avance por NRP.","stop") 
              EXIT PROGRAM
           END IF
      END INPUT
      
      ON ACTION cancelar
        EXIT DIALOG      
          
    END DIALOG 
  CLOSE WINDOW w1
END MAIN 

FUNCTION fn_verificar_info_ava_nrp(p_periodo_pag) 
DEFINE p_destino             SMALLINT
DEFINE p_periodo_pag         CHAR(06)
DEFINE v_tot_reg             INTEGER
DEFINE ls_query              STRING
DEFINE v_folio_reg_pag       DECIMAL(9,0)

  LET v_tot_reg        = 0       
  LET v_folio_reg_pag  = 0

  LET g_sql_txt = " \n SELECT COUNT(*) ",
                  " \n FROM   dis_det_avance_pago avp ",
                  " \n WHERE  avp.estado     = 30 ",
                  " \n AND    avp.tpo_avance = 181 "

  IF p_periodo_pag IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt, " \n AND avp.periodo_pago  = '", p_periodo_pag,"'"
  END IF

  DISPLAY g_sql_txt
  PREPARE ps_existe_info_ava FROM g_sql_txt
  --EXECUTE ps_existe_info USING p_folio_dis INTO v_tot_reg
  EXECUTE ps_existe_info_ava INTO v_tot_reg

  RETURN v_tot_reg            
END FUNCTION