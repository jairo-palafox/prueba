################################################################################
#Version                    => 1.0.1                                           #
#Fecha ultima modificacion  => 12/02/2016                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL53                                                   #
#Objetivo          => Programa lanzador para integrar archivos de aportaciones #
#                     subsecuentes sin conciliar.                              #
#Fecha inicio      => 12/02/2016                                               #
################################################################################
DATABASE safre_viv 

GLOBALS
  DEFINE 
    g_proceso_cod                  LIKE cat_proceso.proceso_cod, --codigo del proceso
    g_opera_cod                    LIKE cat_operacion.opera_cod, --codigo de operacion
    v_usuario                       VARCHAR(30),                  --almacena al usuario
    g_folio                        LIKE dis_det_avance_pago.folio
END GLOBALS

MAIN
  DEFINE 
    v_tipo_proceso                 SMALLINT,    --forma como ejecutara el programa 
    v_nom_prog                     VARCHAR(30), --almacena opci�n del men� 
    v_nom_archivo                  STRING       --nombre del archivo

  --Se asignan los parametros que vienen del fglrun
  LET v_usuario      = ARG_VAL(1)
  LET v_tipo_proceso = ARG_VAL(2)
  LET v_nom_prog     = ARG_VAL(3)
  LET g_proceso_cod  = 933
  LET g_opera_cod    = 3

  --Se asigna el titulo del programa
  IF (v_nom_prog IS NOT NULL) THEN
     CALL ui.Interface.setText(v_nom_prog)
  END IF

  CALL STARTLOG (v_usuario CLIPPED|| ".DISL53.log")
   
  --Obtiene el nombre del archivo
  LET v_nom_archivo = ""

  --DISPLAY "g_proceso_cod ",g_proceso_cod," g_opera_cod", g_opera_cod," v_usuario ",v_usuario
 
  CALL fn_genera_folio(g_proceso_cod, g_opera_cod,v_usuario)
  RETURNING g_folio
                         
  --DISPLAY "g_folio", g_folio
  
  CALL fn_drgandrop_avancepago()
   
END MAIN

FUNCTION fn_drgandrop_avancepago()
  DEFINE 
    l_arr_arch_pend                DYNAMIC ARRAY OF VARCHAR(100), --archivos pendientes 
    l_arr_arch_int                 DYNAMIC ARRAY OF VARCHAR(100), --archivos a integrar
    l_arr_as_sc                    DYNAMIC ARRAY OF RECORD --detalle archivos a integrar
      folio                        LIKE dis_det_avance_pago.folio,
      nom_archivo                  VARCHAR(100),
      f_presenta                   DATE,
      tot_reg                      DECIMAL(10,0),   
      imp_pesos                    DECIMAL(10,2), 
      imp_aivs                     DECIMAL(15,6) 
    END RECORD,

    l_v_arch_proceso               VARCHAR(100),
    l_dnd                          ui.DragDrop,   --manejador del (drag and drop)
    l_drag_index                   INT,           --indice del drag
    l_drop_index                   INT,           --indice del drop
    l_drag_source                  STRING,        --fuente del drag
    l_drag_value                   STRING,        --valor del drag
    l_i_num_arch                   INTEGER,       --numero de archivos a integrar
    l_i_iter                       INTEGER,       --variable usada para iteracion
    l_i_indice                     SMALLINT,      --indice del arrego de archivos pendientes
    l_tot_reg                      DECIMAL(10,0),      --N�mero de Seguridad Social
    l_imp_pesos                    DECIMAL(10,2), --total de amortizaciones en el archivo       
    l_s_qryTxt                     STRING,        --guarda una sentencia SQL a ejecutar
    l_comando                      STRING,
    v_ruta_ejecutable              CHAR(40),
    v_ruta_listados                LIKE seg_modulo.ruta_listados, --rute del log
    v_max_pid                      LIKE bat_ctr_proceso.pid,
    v_s_mensaje                    STRING,
    r_bandera                      INTEGER,
    r_b_valida                     INTEGER,   
    l_imp_aivs                     DECIMAL(15,6)

    CONSTANT l_nom_tbl_pend = "tbl_pendientes" --tabla de archivos pendientes
    CONSTANT l_nom_tbl_int  = "tbl_integrar"   --tabla de archivos a integrar

  -- Obtiene el ultimo pid correspondiente al proceso
  CALL fn_max_pid(g_proceso_cod, 1) RETURNING v_max_pid

  --DISPLAY "v_max_pid: -",v_max_pid,"-"

  -- Obtiene las rutas ejecutable
  SELECT ruta_bin 
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  -- Obtiene ruta listados
  SELECT ruta_listados 
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'
   
  -- Se crea la sentencia que busca los archivos disponibles por integrar
  LET l_s_qryTxt = " SELECT nombre_archivo \n",
                   " FROM   glo_ctr_archivo \n",
                   " WHERE  opera_cod   = 2 \n",
                   " AND    proceso_cod = 933 \n",
                   " AND    estado      = 1"
  PREPARE prp_archivos_val FROM l_s_qryTxt

  -- Se inicializa el indice del arreglo
  LET l_i_indice = 1

  DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 
  FOREACH cur_archivos_val INTO l_arr_arch_pend[l_i_indice]
    -- Se incrementa el indice del arreglo
    LET l_i_indice = l_i_indice + 1
  END FOREACH

  -- Se borra el ultimo indice del arreglo porque es nulo
  CALL l_arr_arch_pend.deleteElement(l_i_indice)
      
  -- Se abre la ventana para elejir los archivos a integrar
  CLOSE WINDOW SCREEN 

  OPEN WINDOW w_inte_acred WITH FORM "DISL531"
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
      
      DISPLAY ARRAY l_arr_as_sc TO tbl_integrados.*
      END DISPLAY

      BEFORE DIALOG
        CALL DIALOG.setActionHidden("accept",1)
        --CALL DIALOG.setActionHidden("deshacer",0)
        CALL DIALOG.setActionHidden("close",1)
        CALL DIALOG.setActionHidden("integrar",1)

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION accept
         -- Se obtiene el numero de archivos a integrar
         LET l_i_num_arch = l_arr_arch_int.getLength()

         -- Debe de haber al menos archivo a procesar
         IF l_i_num_arch = 0 THEN
            CALL fn_mensaje("Aviso",
                            "Debe arrastrar al menos un archivo a integrar",
                            "stop")
            CONTINUE DIALOG
         END IF

         -- Se limpia el arreglo de los archivos ya integrados
         CALL l_arr_as_sc.clear()

         -- se procesan los archivos seleccionados para integrar
         FOR l_i_iter = 1 TO l_i_num_arch
             -- Se asigna el nombre del archivo en la variable paramentro 
             LET l_v_arch_proceso = l_arr_arch_int[l_i_iter]
             --DISPLAY "l_v_arch_proceso: ",l_v_arch_proceso 

             LET l_s_qryTxt = "\n SELECT COUNT(*), ",
                              "\n        SUM(imp_pesos)/100, SUM(imp_aivs)/1000000",
                              "\n FROM   safre_tmp:tmp_dis_aposubs_sc"
             PREPARE pre_sql_total_registros_archivo FROM l_s_qryTxt
             EXECUTE pre_sql_total_registros_archivo INTO l_tot_reg,                                                         
                                                          l_imp_pesos, 
                                                          l_imp_aivs



             -- Se asigna la informacion del archivo integrado
             LET l_arr_as_sc[l_i_iter].folio              = g_folio
             LET l_arr_as_sc[l_i_iter].nom_archivo        = l_v_arch_proceso
             LET l_arr_as_sc[l_i_iter].f_presenta         = TODAY
             LET l_arr_as_sc[l_i_iter].tot_reg            = l_tot_reg 
             LET l_arr_as_sc[l_i_iter].imp_pesos          = l_imp_pesos
             LET l_arr_as_sc[l_i_iter].imp_aivs           = l_imp_aivs
         END FOR

         -- Se limpia el arreglo
         CALL l_arr_arch_int.clear()
         CALL DIALOG.setActionHidden("accept",1)
         --CALL DIALOG.setActionHidden("deshacer",0)
         CALL DIALOG.setActionHidden("integrar",0)

         CONTINUE DIALOG


      ON ACTION integrar
         -- Valida Operaci�n
         CALL fn_valida_operacion(v_max_pid, g_proceso_cod, g_opera_cod)
         RETURNING r_bandera

         --DISPLAY "r_bandera: -",r_bandera,"-"

         IF r_bandera = 0 THEN             
            CALL fn_actualiza_opera_ini(v_max_pid, g_proceso_cod, g_opera_cod, g_folio, "DISE24", l_v_arch_proceso, v_usuario)
            RETURNING r_b_valida

            --DISPLAY "r_b_valida: -",r_b_valida,"-"
                                                
            IF r_b_valida = 0 THEN
               LET l_comando = "nohup fglrun ", v_ruta_ejecutable CLIPPED, "/DISE24.42r ",
                               v_usuario, " ",
                               v_max_pid, " ",
                               g_proceso_cod, " ",
                               g_opera_cod, " ",
                               g_folio, " ",
                               l_v_arch_proceso, " '",
                               l_v_arch_proceso, "' 1>", v_ruta_listados CLIPPED,
                               "/nohup:", v_max_pid USING "&&&&&",":",
                               g_proceso_cod USING "&&&&&",":",
                               g_opera_cod USING "&&&&&" ," 2>&1 &"

               DISPLAY l_comando
               RUN l_comando
               CALL l_arr_as_sc.clear()
               CALL DIALOG.setActionHidden("integrar",1)
               -- Se asigna el mensaje a mostrar al usuario
               LET v_s_mensaje = "Se ha enviado la integraci�n con PID: ",
                                 v_max_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecuci�n de procesos"
               CALL fn_mensaje("Integraci�n", v_s_mensaje, "information")
               EXIT DIALOG
            END IF
         ELSE
            -- Env�a mensaje con la descripcin del Error
            CALL fn_muestra_inc_operacion(r_bandera)
         END IF

         EXIT DIALOG
    END DIALOG
  CLOSE WINDOW w_inte_acred
END FUNCTION
