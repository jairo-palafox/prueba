################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 06/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISL405                                                   #
#Objetivo         => Programa Lanzador Extractor de Dispersión de Aclaratorios #
#                    con Cambio de NSS.                                        #
#                                                                              #
#Fecha de Inicio  => 03/09/2015                                                #
################################################################################
-- Base que se utilizará
DATABASE safre_viv

-- Definición de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,                       --Consultas
    g_usuario                VARCHAR(30),                  --Almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Código del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Código de operación
    g_pid                    DECIMAL(9,0),
    g_folio                  DECIMAL(9,0)

  DEFINE l_comando           STRING
  DEFINE v_ruta_ejecutable   CHAR(40)
  DEFINE v_ruta_listados     CHAR(40)
  DEFINE v_mensaje           STRING

  DEFINE 
    p_programa               CHAR(10),  
    r_bandera                SMALLINT,
    r_nom_archivo            CHAR(40)
         
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
  DEFINE 
    v_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa 
    v_nom_prog               VARCHAR(30), --Almacena opción del menú 
    v_nom_archivo            STRING       --Nombre del archivo
    
  --Datos de entrada
  DEFINE v_folio             DECIMAL(9,0) --Folio  
 
  DEFINE 
    bnd_consulta             SMALLINT, 
    f_ventana                ui.Window,   --Define las propìedades de la Ventana
    f_forma                  ui.Form      --Define las propiedades de la forma

  DEFINE v_bnd_existe_info   INTEGER
  DEFINE f_periodo1          INTEGER      --Periodo pago inicio 
  DEFINE r_bnd_periodo       SMALLINT

  CALL STARTLOG (g_usuario CLIPPED|| ".DISL45.log")
          
  --Recibe valores de argumentos
  LET g_usuario         = ARG_VAL(1)
  LET v_tipo_proceso    = ARG_VAL(2)
  LET v_nom_prog        = ARG_VAL(3)
   
  LET g_proceso_cod     = 930
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

  --Se asigna el título del programa
  IF (v_nom_prog IS NOT NULL) THEN
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

  OPEN WINDOW w1 WITH FORM "DISL451"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm() 
          
          NEXT FIELD v_folio
          CALL ui.interface.refresh()

          ON ACTION ACCEPT 
             DISPLAY "v_folio -",v_folio,"-"
            
             LET v_bnd_existe_info = 0
             CALL fn_verificar_info_disp(v_folio) 
             RETURNING v_bnd_existe_info            

             DISPLAY "v_bnd_existe_info: ",v_bnd_existe_info
             
             IF v_bnd_existe_info >= 1 THEN
                --Si se acepta la ejecución se genera PID del proceso
                CALL fn_genera_pid (g_proceso_cod, g_opera_cod, g_usuario) 
                RETURNING g_pid                 
               
                IF (fn_valida_operacion(g_pid, g_proceso_cod, 1) = 0 ) THEN               
                   --Enlaza el folio referencia 
                   LET g_folio = 0;
                   CALL fn_genera_folio_dis(g_proceso_cod, g_opera_cod, 0, g_usuario)
                   RETURNING g_folio

                   LET p_programa    = "DISS44"
                   LET r_nom_archivo = ""

                   DISPLAY "Folio -- ",g_folio
                   --Ejecuta la funcion de preliquida y asigna estatus de LISTO
                   CALL fn_inicializa_proceso (g_pid, g_proceso_cod, 1, g_folio, 
                                               p_programa, r_nom_archivo, g_usuario)
                   RETURNING r_bandera

                   --Inicia la operación asignando el estatus de PROCESANDO
                   CALL fn_actualiza_opera_ini(g_pid, g_proceso_cod, g_opera_cod, g_folio, 
                                               p_programa, r_nom_archivo, g_usuario)
                   RETURNING r_bandera

                   DISPLAY "r_bandera: ", r_bandera
                   DISPLAY "ANTES DE MANDAR nohup DISS44"

                   IF v_folio IS NULL THEN
                      LET v_folio = 0
                   END IF

                   --Validaciones  
                   LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS44.42r ",
                                   v_folio," ",  
                                   g_pid," ",                               
                                   g_folio," ",                                 
                                   g_usuario, " ",                                 
                                   " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                   g_pid USING "&&&&&",":",
                                   g_proceso_cod USING "&&&&&",":",
                                   1 USING "&&&&&" ," 2>&1 &"
                   RUN l_comando

                   LET v_mensaje = "Se ha enviado el Extractor Dispersión Aclaratorios con Cambio de NSS: ",
                                   g_pid CLIPPED,
                                   ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"

                   CALL fn_mensaje("Extractor Dispersión Aclaratorios con Cambio de NSS", v_mensaje, "information")
                                                        
                   EXIT DIALOG
                ELSE
                  CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid, g_proceso_cod, 1))
                END IF  --De valida operacion
             ELSE
               CALL fn_mensaje("Aviso","No existe información.","stop") 
               EXIT PROGRAM
             END IF  
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG      
          
    END DIALOG 
  CLOSE WINDOW w1 
END MAIN 

FUNCTION fn_verificar_info_disp(p_folio) 
  DEFINE p_folio             DECIMAL(9,0)
  DEFINE v_tot_reg           INTEGER

  LET v_tot_reg = 0           

  LET g_sql_txt = "\n SELECT count(unique a.folio) ",
                  "\n FROM   glo_folio a ",
                  "\n WHERE  a.proceso_cod       = 901 ",
                  "\n AND    a.status            = 2 ",
                  "\n AND    a.folio_referencia IN (SELECT b.folio ",
                  "\n                               FROM   glo_folio b ",
                  "\n                               WHERE  b.proceso_cod = 103 "
                   
  IF p_folio IS NOT NULL THEN                    
     LET g_sql_txt = g_sql_txt CLIPPED, "\n AND b.folio = ", p_folio," "
  END IF      
                   
  LET g_sql_txt = g_sql_txt CLIPPED, "\n AND b.status = 2) "

  DISPLAY "g_sql_txt: -",g_sql_txt
  PREPARE ps_existe_info FROM g_sql_txt   
  EXECUTE ps_existe_info INTO v_tot_reg

  RETURN v_tot_reg            
END FUNCTION