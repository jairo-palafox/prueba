################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 06/04/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL42                                                   #
#Objetivo          => Programa lanzador para integrar archivo Consulta         #
#                     Pagos Históricos   927                                   #
#Fecha inicio      => 22/07/2015                                               #
################################################################################
DATABASE safre_viv 

GLOBALS
  DEFINE 
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Código del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Código de operación
    v_usuario                VARCHAR(30),                  --Almacena al usuario
    g_folio                  LIKE dis_det_avance_pago.folio,
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

  --se asignan los parametros que vienen del fglrun
  LET v_usuario      = ARG_VAL(1)
  LET v_tipo_proceso = ARG_VAL(2)
  LET v_nom_prog     = ARG_VAL(3)
  LET g_proceso_cod  = 927
  LET g_opera_cod    = 2

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

  CALL STARTLOG (v_usuario CLIPPED|| ".DISL42.log")
   
  --Obtiene el nombre del archivo
  LET v_nom_archivo = ""

  DISPLAY "g_proceso_cod ", g_proceso_cod, 
          " g_opera_cod", g_opera_cod,
          " v_usuario ", v_usuario
  --Se obtiene el número de folio
  --PREPARE prp_numFolio FROM "EXECUTE FUNCTION fn_genera_folio(?, ?, ?)"
  --EXECUTE prp_numFolio USING g_proceso_cod, g_opera_cod,v_usuario
                         --INTO g_folio
  CALL fn_genera_folio(g_proceso_cod, g_opera_cod, v_usuario)
  RETURNING g_folio
                         
  DISPLAY "g_folio", g_folio
  CALL fn_drgandrop_cons_pag_hist()
   
END MAIN

FUNCTION fn_drgandrop_cons_pag_hist()
  DEFINE 
    l_arr_arch_pend          DYNAMIC ARRAY OF VARCHAR(100),--Archivos pendientes 
    l_arr_arch_int           DYNAMIC ARRAY OF VARCHAR(100),--Archivos a integrar
    l_arr_integrados         DYNAMIC ARRAY OF RECORD --Detalle archivos a integrar
    folio                    LIKE dis_det_avance_pago.folio,
    nom_archivo              VARCHAR(100),
    --tot_aportaciones         DECIMAL(12,2),
    --tot_amortizaciones       DECIMAL(12,2),
    tot_reg                  INTEGER 
    END RECORD,
    l_v_arch_proceso         VARCHAR(100),
    l_dnd                    ui.DragDrop, --Manejador del (drag and drop)
    l_drag_index             INT,         --Índice del drag
    l_drop_index             INT,         --Índice del drop
    l_drag_source            STRING,      --Fuente del drag
    l_drag_value             STRING,      --alor del drag
    l_i_num_arch             INTEGER,     --Numero de archivos a integrar
    l_i_iter                 INTEGER,     --Variable usada para iteracion
    l_i_indice               SMALLINT,--Índice del arrego de archivos pendientes
    l_i_tot_reg              BIGINT,      --Total de registros en archivo
    --l_i_tot_apo              DECIMAL(22,2),--Total de aportaciones en el archivo
    --l_i_tot_amo              DECIMAL(22,2),--Total de amortizaciones en el archivo       
    l_s_qryTxt               STRING,      --Guarda una sentencia SQL a ejecutar
    l_comando                STRING,
    v_ruta_ejecutable        CHAR(40),
    v_ruta_listados          LIKE seg_modulo.ruta_listados, --Ruta del log
    v_max_pid                LIKE bat_ctr_proceso.pid,
    v_s_mensaje              STRING,
    r_bandera                INTEGER,
    r_b_valida               INTEGER,
    l_i_tot_aiv              DECIMAL (22,2)

  CONSTANT l_nom_tbl_pend = "tbl_pendientes" --Tabla de archivos pendientes
  CONSTANT l_nom_tbl_int  = "tbl_integrar"   --Tabla de archivos a integrar

  --Obtiene el ultimo pid correspondiente al proceso
  CALL fn_max_pid(g_proceso_cod, 1) 
  RETURNING v_max_pid

  --Obtiene las rutas ejecutable
  SELECT ruta_bin 
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  --Obtiene ruta listados
  SELECT ruta_listados 
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'
   
  --Se crea la sentencia que busca los archivos disponibles por integrar
  LET l_s_qryTxt = " SELECT nombre_archivo \n",
                   " FROM   glo_ctr_archivo \n",
                   " WHERE  opera_cod   = 1 \n",
                   " AND    proceso_cod = 927 \n",
                   " AND    estado      = 1"
  PREPARE prp_archivos_val FROM l_s_qryTxt

  --Se inicializa el indice del arreglo
  LET l_i_indice = 1

  DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 
  FOREACH cur_archivos_val INTO l_arr_arch_pend[l_i_indice]
    --Se incrementa el indice del arreglo
    LET l_i_indice = l_i_indice + 1
  END FOREACH

  --Se borra el ultimo indice del arreglo porque es nulo
  CALL l_arr_arch_pend.deleteElement(l_i_indice)
      
  --Se abre la ventana para elejir los archivos a integrar
  CLOSE WINDOW SCREEN 

  OPEN WINDOW w_cons_pag_hist WITH FORM "DISL421"
    DIALOG ATTRIBUTE(UNBUFFERED)
      DISPLAY ARRAY l_arr_arch_pend TO tbl_pendientes.*
        ON DRAG_START(l_dnd)
           LET l_drag_source = l_nom_tbl_pend
           LET l_drag_index  = arr_curr()
           LET l_drag_value  = l_arr_arch_pend[l_drag_index]
             
        ON DRAG_FINISHED(l_dnd)
           INITIALIZE l_drag_source TO NULL

        ON DRAG_ENTER(l_dnd)
           IF l_drag_source IS NULL THEN
              CALL l_dnd.setOperation(NULL)
           END IF
             
        ON DROP(l_dnd)
           IF l_drag_source == l_nom_tbl_pend THEN
              CALL l_dnd.dropInternal()
           ELSE
              LET l_drop_index = l_dnd.getLocationRow()
              CALL DIALOG.insertRow(l_nom_tbl_pend, l_drop_index)
              CALL DIALOG.setCurrentRow(l_nom_tbl_pend, l_drop_index)
              LET l_arr_arch_pend[l_drop_index] = l_drag_value
              CALL DIALOG.deleteRow(l_nom_tbl_int, l_drag_index)
           END IF
      END DISPLAY
      
      DISPLAY ARRAY l_arr_arch_int TO tbl_integrar.*
        ON DRAG_START(l_dnd)
           LET l_drag_source = l_nom_tbl_int
           LET l_drag_index  = arr_curr()
           LET l_drag_value  = l_arr_arch_int[l_drag_index]
            
        ON DRAG_FINISHED(l_dnd)
           INITIALIZE l_drag_source TO NULL

        ON DRAG_ENTER(l_dnd)
           IF l_drag_source IS NULL THEN
              CALL l_dnd.setOperation(NULL)
           END IF

        ON DROP(l_dnd)
           IF l_drag_source == l_nom_tbl_int THEN
              CALL l_dnd.dropInternal()
           ELSE
              LET l_drop_index = l_dnd.getLocationRow()
              CALL DIALOG.insertRow(l_nom_tbl_int, l_drop_index)
              CALL DIALOG.setCurrentRow(l_nom_tbl_int, l_drop_index)
              LET l_arr_arch_int[l_drop_index] = l_drag_value
              CALL DIALOG.deleteRow(l_nom_tbl_pend, l_drag_index)
           END IF

           CALL DIALOG.setActionHidden("accept",0)
           --CALL DIALOG.setActionHidden("deshacer",0)
      END DISPLAY
      
      DISPLAY ARRAY l_arr_integrados TO tbl_integrados.*
      END DISPLAY

      BEFORE DIALOG
        CALL DIALOG.setActionHidden("accept",1)
        --CALL DIALOG.setActionHidden("deshacer",0)
        CALL DIALOG.setActionHidden("close",1)
        CALL DIALOG.setActionHidden("integrar",1)

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION accept
         --Se obtiene el numero de archivos a integrar
         LET l_i_num_arch = l_arr_arch_int.getLength()

         --Debe de haber al menos archivo a procesar
         IF l_i_num_arch = 0 THEN
            CALL fn_mensaje("Aviso",
                            "Debe arrastrar al menos un archivo a integrar",
                            "stop")
            CONTINUE DIALOG
         END IF

         --Se limpia el arreglo de los archivos ya integrados
         CALL l_arr_integrados.clear()

         --Se procesan los archivos seleccionados para integrar
         FOR l_i_iter = 1 TO l_i_num_arch
             --Se asigna el nombre del archivo en la variable paramentro 
             LET l_v_arch_proceso = l_arr_arch_int[l_i_iter]
             DISPLAY "l_v_arch_proceso: ", l_v_arch_proceso 

             LET l_s_qryTxt = "\n SELECT COUNT(cph.nss) ",
                              --"\n        SUM(ava.ava_aportacion), ",
                              --"\n        SUM(ava.ava_amortizacion) ",
                              "\n FROM   safre_tmp:tmp_dis_cons_pag_his2 cph"
             PREPARE pre_sql_total_registros_archivo FROM l_s_qryTxt
             EXECUTE pre_sql_total_registros_archivo INTO l_i_tot_reg --,
                                                          --l_i_tot_apo,
                                                          --l_i_tot_amo

             --LET l_i_tot_apo                                   = l_i_tot_apo/100
             --LET l_i_tot_amo                                   = l_i_tot_amo/100
             --Se asigna la informacion del archivo integrado
             LET l_arr_integrados[l_i_iter].folio              = g_folio
             LET l_arr_integrados[l_i_iter].nom_archivo        = l_v_arch_proceso
             --LET l_arr_integrados[l_i_iter].tot_aportaciones   = l_i_tot_apo
             --LET l_arr_integrados[l_i_iter].tot_amortizaciones = l_i_tot_amo
             LET l_arr_integrados[l_i_iter].tot_reg            = l_i_tot_reg

             --DISPLAY l_i_iter, " - ", l_arr_integrados[l_i_iter].*
         END FOR

         --Se limpia el arreglo
         CALL l_arr_arch_int.clear()
         CALL DIALOG.setActionHidden("accept",1)
         --CALL DIALOG.setActionHidden("deshacer",0)
         CALL DIALOG.setActionHidden("integrar",0)

         CONTINUE DIALOG

      {ON ACTION deshacer
         CALL l_arr_arch_int.clear()
         CALL DIALOG.setActionHidden("integrar",1)
         CALL DIALOG.setActionHidden("deshacer",0)

         DECLARE cur_integra_arch CURSOR FOR prp_archivos_val
         LET l_i_indice = 1
         FOREACH cur_integra_arch INTO l_arr_arch_pend[l_i_indice]
           --Se incrementa el indice del arreglo
           LET l_i_indice = l_i_indice + 1
         END FOREACH
         CONTINUE DIALOG}

      ON ACTION integrar
         --Valida Operación
         CALL fn_valida_operacion(v_max_pid, g_proceso_cod, g_opera_cod)
         RETURNING r_bandera
         IF r_bandera = 0 THEN  
            --Se ejecuta la funcion que inserta la operacion a ejecutar
            --LET l_s_qryTxt = " EXECUTE FUNCTION fn_actualiza_opera_ini(?,?,?,?,?,?,?)"
            --PREPARE prp_fn_actualiza_opera_ini FROM l_s_qryTxt
            --EXECUTE prp_fn_actualiza_opera_ini USING v_max_pid,g_proceso_cod,
                                                       --g_opera_cod,g_folio,"DISE05",
                                                       --l_v_arch_proceso,v_usuario
                                                  --INTO r_b_valida

            CALL fn_actualiza_opera_ini(v_max_pid,
                                        g_proceso_cod, 
                                        g_opera_cod,
                                        g_folio,
                                        "DISL42",
                                        l_v_arch_proceso,
                                        v_usuario)
            RETURNING r_b_valida
                                                
            IF r_b_valida = 0 THEN
               LET l_comando = "nohup fglrun ", 
                               v_ruta_ejecutable CLIPPED, "/DISE22.42r ",
                               v_usuario, " ",
                               v_max_pid, " ",
                               g_proceso_cod, " ",
                               g_opera_cod, " ",
                               g_folio, " ",
                               l_v_arch_proceso, " '",
                               l_v_arch_proceso, "' 1>", v_ruta_listados CLIPPED,
                               "/nohup:", v_max_pid     USING "&&&&&", ":",
                                          g_proceso_cod USING "&&&&&", ":",
                                          g_opera_cod   USING "&&&&&", " 2>&1 &"
               DISPLAY l_comando
               RUN l_comando

               CALL l_arr_integrados.clear()
               CALL DIALOG.setActionHidden("integrar",1)
               --Se asigna el mensaje a mostrar al usuario
               LET v_s_mensaje = "Se ha enviado la integración con PID: ",
                                 v_max_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
               CALL fn_mensaje("Integración", v_s_mensaje, "information")
               EXIT DIALOG
            END IF
         ELSE
            --Envía mensaje con la descripcin del Error
            CALL fn_muestra_inc_operacion(r_bandera)
         END IF
         EXIT DIALOG
    END DIALOG
  CLOSE WINDOW w_cons_pag_hist
END FUNCTION