################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 06/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISL40                                                    #
#Objetivo         => Programa Lanzador Extractor Avance de Pagos por Perido.   #
#                                                                              #
#Fecha de Inicio  => 17/07/2015                                                #
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
  DEFINE v_periodo_pago      CHAR(6)      --Periodo de pago  
 
  DEFINE bnd_consulta        SMALLINT, 
         f_ventana           ui.Window,   --Define las propìedades de la Ventana
         f_forma             ui.Form      --Define las propiedades de la forma

  DEFINE v_bnd_existe_info   INTEGER
  DEFINE f_periodo1          INTEGER      --Periodo pago inicio 
  DEFINE r_bnd_periodo       SMALLINT

  CALL STARTLOG (g_usuario CLIPPED|| ".DISL40.log")
          
  --Recibe valores de argumentos
  LET g_usuario         = ARG_VAL(1)
  LET v_tipo_proceso    = ARG_VAL(2)
  LET v_nom_prog        = ARG_VAL(3)
   
  LET g_proceso_cod     = 926
  LET g_opera_cod       = 1

  LET v_bnd_existe_info = 0
  LET r_bnd_periodo     = 0
  LET f_periodo1        = ""

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

  --DISPLAY "g_folio: -",g_folio,"-"

  LET v_nom_archivo = ""   
  LET bnd_consulta  = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISL401"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_periodo_pago
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm() 

          NEXT FIELD v_periodo_pago
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro       
           IF v_periodo_pago IS NULL THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Ingrese el periodo de pago",
                              "about")
              NEXT FIELD v_periodo_pago 
           END IF

           IF LENGTH(v_periodo_pago) <> 6 THEN  
              CALL fn_mensaje ("ATENCION", 
                               "El periodo de pago debe ser de 6 caracteres", 
                               "stop")
              NEXT FIELD v_periodo_pago
           END IF 
                     
           --DISPLAY "v_periodo_pago -",v_periodo_pago,"-"
           PREPARE prp_verifica_periodo FROM "EXECUTE FUNCTION fn_valida_formato_periodo_pago(?)"
           EXECUTE prp_verifica_periodo USING v_periodo_pago INTO f_periodo1, r_bnd_periodo

           --La función "fn_valida_formato_periodo_pago" regresa 2 parámetros, uno el periodo y dos el estatus
           --Si el estatus es 0, no hay error y el periodo es válido
           --Si el estatus es 1, entonces el año del periodo es inválido, es decir, es mayor al año actual
           --Si el estatus es 2, entonces el mes es incorrecto
           IF r_bnd_periodo = 0 THEN 
              DISPLAY "Correcto periodo 1"
           ELSE 
              IF r_bnd_periodo = 1 THEN 
                 CALL fn_mensaje ("ATENCION", 
                                  "Verifique el periodo de pago. El año es incorrecto", 
                                  "stop")
                 NEXT FIELD v_periodo_pago
              END IF

              IF r_bnd_periodo = 2 THEN 
                 CALL fn_mensaje ("ATENCION", 
                                  "Verifique el periodo de pago. El mes es incorrecto", 
                                  "stop")
                 NEXT FIELD v_periodo_pago
              END IF
           END IF               

           IF v_periodo_pago IS NOT NULL THEN
              LET v_bnd_existe_info = 0
              CALL fn_verificar_info_disp(v_periodo_pago) RETURNING v_bnd_existe_info
           END IF

           --DISPLAY "v_bnd_existe_info: ",v_bnd_existe_info
           IF v_bnd_existe_info >= 1 THEN
              --Si se acepta la ejecución se genera PID del proceso
              CALL fn_genera_pid (g_proceso_cod, g_opera_cod, g_usuario) RETURNING g_pid            
               
              IF (fn_valida_operacion(g_pid, g_proceso_cod, 1) = 0 ) THEN
                 --Enlaza el folio referencia 
                 LET g_folio = 0;
                 CALL fn_genera_folio_dis(g_proceso_cod, g_opera_cod, 0, g_usuario)
                 RETURNING g_folio

                 LET p_programa    = "DISS40"
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
                 --DISPLAY "ANTES DE MANDAR nohup DISS40"

                 --Validaciones 
                 LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS40.42r ",
                                 v_periodo_pago," ",                                 
                                 g_folio," ",                                 
                                 g_usuario, " ",                                 
                                 " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                 g_pid USING "&&&&&",":",
                                 g_proceso_cod USING "&&&&&",":",
                                 1 USING "&&&&&" ," 2>&1 &"
                 RUN l_comando

                 CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                 RETURNING r_bandera
         
                 LET v_mensaje = "Se ha enviado el Extractor Avance Pagos PP: ",
                                 g_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"

                 CALL fn_mensaje("Extractor de Avance Pagos PP", v_mensaje, "information")
                                                        
                 EXIT DIALOG
              ELSE
                 CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid, g_proceso_cod, 1))
              END IF  --De valida operacion
           ELSE
              CALL fn_mensaje("Aviso","No existe información del Avance de Pagos del periodo de pago capturado.","stop") 
              EXIT PROGRAM
           END IF  
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG      
          
    END DIALOG 
  CLOSE WINDOW w1 
END MAIN 

FUNCTION fn_verificar_info_disp(p_periodo_pago) 
  DEFINE p_periodo_pago      CHAR(6)
  DEFINE v_tot_reg           INTEGER

  LET v_tot_reg = 0           

  LET g_sql_txt = "\n SELECT count(*) ",
                  "\n FROM   dis_det_avance_pago ap, ", 
                  "\n        afi_derechohabiente ad, ",
                  "\n        cat_edo_avance_pago ce, ", 
                  "\n        glo_folio gf ",              
                  "\n WHERE  ap.id_derechohabiente = ad.id_derechohabiente ",
                  "\n AND    ap.estado             = ce.cod_edo_avance ",
                  "\n AND    gf.folio              = ap.folio ",
                  "\n AND    ap.estado            IN (30,50,70,80,85) ",
                  "\n AND   ap.periodo_pago        = '", p_periodo_pago,"'"                   

  --DISPLAY g_sql_txt
  PREPARE ps_existe_info FROM g_sql_txt   
  EXECUTE ps_existe_info INTO v_tot_reg

  RETURN v_tot_reg            
END FUNCTION