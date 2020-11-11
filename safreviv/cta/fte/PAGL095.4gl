{===============================================================================
Proyecto          => SISTEMA DE AFORE( SAFRE )
Propietario       => E.F.P.
Modulo            => PAG
Programa          => PAGL065
Descripcion       => Programa Lanzador Integración Archivo de Patrones
Fecha creacion    => Enero 23, 2020.
================================================================================}

DATABASE safre_viv
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD


    DEFINE g_total       INTEGER
    DEFINE g_salida      INTEGER
    DEFINE drop_index    INTEGER
    DEFINE i             INTEGER

    DEFINE g_bnd         SMALLINT
    DEFINE g_afore_cod   SMALLINT
    DEFINE g_lote        SMALLINT

    DEFINE g_proceso_cod SMALLINT
    DEFINE g_opera_cod   SMALLINT
    DEFINE accion        SMALLINT

    DEFINE g_enter       CHAR(1)
    DEFINE archivo       CHAR(25)
    DEFINE g_usuario     CHAR(20)

    DEFINE g_hoy         DATE
    DEFINE g_f_cza       DATE

    DEFINE reg_asi_ctr_arh RECORD
        f_lote           DATE    ,
        lote             SMALLINT,
        f_asignacion     DATE    ,
        f_apertura       DATE    ,
        estado           SMALLINT,
        usuario          CHAR(12)
    END RECORD

    DEFINE arr_fuente DYNAMIC ARRAY OF RECORD
        archivo          STRING ,
        total_aceptados  INTEGER
    END RECORD

    DEFINE arr_destino DYNAMIC ARRAY OF RECORD
        archivo          STRING ,
        total_aceptados  INTEGER
    END RECORD

    DEFINE arr_folio DYNAMIC ARRAY OF RECORD
        archivo           STRING,
        f_lote            DATE,
        lote              SMALLINT,
        f_asignacion      DATE,
        f_apertura        DATE,
        total_solicitudes INTEGER
    END RECORD


    DEFINE g_mensaje     STRING
    DEFINE g_titulo      STRING
    DEFINE g_imagen      STRING
    DEFINE eje_cadena    STRING
    DEFINE drag_source   STRING
    DEFINE g_txt         STRING

    DEFINE dnd           ui.DragDrop

    DEFINE w             ui.window
    DEFINE f_w           ui.form
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   DISPLAY g_usuario        
   DISPLAY p_tipo_ejecucion 
   DISPLAY p_s_titulo       
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
 
   -- se asigna proceso y operacion
   LET g_proceso_cod = 1420
   LET g_opera_cod   = 2

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'pag'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   CALL fn_drgandrop_integra_devol_exceso(g_usuario)

END MAIN

FUNCTION fn_drgandrop_integra_devol_exceso(p_usuario_cod)
DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       l_arr_arch_pend    DYNAMIC ARRAY OF VARCHAR(100), -- Archivos pendientes 
       l_arr_arch_int     DYNAMIC ARRAY OF VARCHAR(100), -- Archivos a integrar
       l_arr_integrados   DYNAMIC ARRAY OF RECORD -- Detalle archivos a integrar
            nom_archivo      VARCHAR(100),
            f_presenta       DATE,
            total_solictudes INTEGER
       END RECORD,
       l_v_arch_proceso   VARCHAR(100),
       l_dnd              ui.DragDrop, -- manejador del (drag and drop)
       l_drag_index       INT, -- indice del drag
       l_drop_index       INT, -- indice del drop
       l_drag_source      STRING, -- fuente del drag
       l_drag_value       STRING, -- valor del drag
       l_i_num_arch       SMALLINT, -- numero de archivos a integrar
       l_i_iter           SMALLINT, -- variable usada para iteracion
       l_i_indice         SMALLINT, -- indice del arrego de archivos pendientes
       l_i_tot_reg        INTEGER, -- total de registros en archivo
       vQryTxt            STRING, -- guarda una sentencia SQL a ejecutar
       v_ruta_ejecutable  CHAR(40),
       v_respuesta        SMALLINT
       --
       ,v_r_arch_pend     VARCHAR(100) -- Archivos pendientes
       ,v_s_qry           STRING
       ,v_dte_fechaArchiv DATE 

   CONSTANT l_nom_tbl_pend = "archfuente" -- Tabla de archivos pendientes
   CONSTANT l_nom_tbl_int = "archdestino" -- Tabla de archivos a integrar

   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo
   WHERE modulo_cod = "dpe"
   
   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET vQryTxt = "SELECT nombre_archivo, f_actualiza"
                    ,"\n FROM glo_ctr_archivo "
                    ,"\n WHERE proceso_cod = ",g_proceso_cod
                    ,"\n   AND opera_cod  =1"--,g_opera_cod --,g_opera_cod que tiene la carga 
                    ,"\n   AND estado = 1" -- archivos pendientes de integrar

   --DISPLAY vQryTxt
   PREPARE Prpr_ObtArchVal FROM vQryTxt CLIPPED
   DECLARE Curr_ObtArchVal CURSOR FOR Prpr_ObtArchVal 
   	
   -- se inicializa el indice del arreglo
   LET l_i_indice = 0
   	
   FOREACH Curr_ObtArchVal  INTO v_r_arch_pend, v_dte_fechaArchiv
      -- se incrementa el indice del arreglo
      LET l_i_indice = l_i_indice + 1
      LET l_arr_arch_pend[l_i_indice] = v_r_arch_pend

   END FOREACH
   IF(l_i_indice<1)THEN
      CALL fn_mensaje("Atención",
           "No existen archivos cargados pendientes de integrar","info")
      RETURN
   END IF

   -- se abre la ventana para elejir los archivos a integrar
   OPEN WINDOW w_integra WITH FORM "PAGL0951"
   DIALOG ATTRIBUTE(UNBUFFERED)
      DISPLAY ARRAY l_arr_arch_pend TO archfuente.*
         ON DRAG_START(l_dnd)
            LET l_drag_source = l_nom_tbl_pend
            LET l_drag_index = arr_curr()
            LET l_drag_value = l_arr_arch_pend[l_drag_index]
             
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
      
      DISPLAY ARRAY l_arr_arch_int TO archdestino.*
         ON DRAG_START(l_dnd)
            LET l_drag_source = l_nom_tbl_int
            LET l_drag_index = arr_curr()
            LET l_drag_value = l_arr_arch_int[l_drag_index]
            
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
            CALL DIALOG.setActionHidden("deshacer",1)
      END DISPLAY
      
      DISPLAY ARRAY l_arr_integrados TO tbl_integrados.*
      END DISPLAY

      BEFORE DIALOG
         CALL DIALOG.setActionHidden("integrar",1)
         CALL DIALOG.setActionHidden("deshacer",1)
         CALL DIALOG.setActionHidden("close",1)
         CALL DIALOG.setActionHidden("accept",1)

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION ACCEPT
         -- se obtiene el numero de archivos a integrar
         LET l_i_num_arch = l_arr_arch_int.getLength()

         -- Debe de haber al menos archivo a procesar
         IF l_i_num_arch = 0 THEN
            CALL fn_mensaje("Aviso",
                            "Debe arrastrar al menos un archivo a integrar",
                            "stop")
            CONTINUE DIALOG
         END IF

         -- se limpia el arreglo de los archivos ya integrados
         CALL l_arr_integrados.clear()

         -- se procesan los archivos seleccionados para integrar
         FOR l_i_iter = 1 TO l_i_num_arch
            -- se asigna el nombre del archivo en la variable paramentro 
            LET l_v_arch_proceso = l_arr_arch_int[l_i_iter]

            -- se asigna la informacion del archivo integrado
            --LET l_arr_integrados[l_i_iter].folio = g_folio
            LET l_arr_integrados[l_i_iter].nom_archivo = l_v_arch_proceso
            LET l_arr_integrados[l_i_iter].f_presenta = v_dte_fechaArchiv

            -- calcula total de solicitudes
            LET l_i_tot_reg = 0
            LET v_s_qry = "\n SELECT COUNT(*)"
                            ,"\n FROM   safre_tmp:tmp_arch_patrones"
            PREPARE pre_sql_total_registros_archivo FROM v_s_qry
            EXECUTE pre_sql_total_registros_archivo INTO l_i_tot_reg
            -- se asigna el total de solicitudes
           LET l_arr_integrados[l_i_iter].total_solictudes = l_i_tot_reg
         END FOR

         -- se limpia el arreglo
         CALL l_arr_arch_int.clear()
         CALL DIALOG.setActionHidden("integrar",0)
         CALL DIALOG.setActionHidden("deshacer",1)
         CALL DIALOG.setActionHidden("accept",1)

         CONTINUE DIALOG

      ON ACTION deshacer
         CALL l_arr_integrados.clear()
         CONTINUE DIALOG

      ON ACTION integrar

         --Solicita confirmar(1) o cancelar(0) la operación de Registro
         CALL fn_ventana_confirma("Atención",
              "¿Desea ejecutar el proceso de Integración del Archivo de patrones?",
              "quest") RETURNING v_respuesta

        IF v_respuesta = 1 THEN
           CALL fn_dpe_ejecuta_integracion(p_usuario_cod, l_v_arch_proceso)
           EXIT DIALOG
        END IF

   END DIALOG
CLOSE WINDOW w_integra
END FUNCTION

#OBJETIVO: Ejecutar la integración del archivo seleccionado 
FUNCTION fn_dpe_ejecuta_integracion(p_usuario_cod, p_nombre_archivo)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_s_comando      STRING,
       v_i_resultado    INTEGER,
       v_mensaje        STRING,
       r_bnd_fin_oper   SMALLINT
   -- Se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
        RETURNING v_i_resultado

   IF ( v_i_resultado = 0 ) THEN  	  
   	   -- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"PAGL095","",p_usuario_cod)
           RETURNING r_bnd_fin_oper
      IF (r_bnd_fin_oper = 0) THEN
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/PAGP064" CLIPPED," ",
                            p_usuario_cod CLIPPED, " ",
                            g_pid, " " ,
                            g_proceso_cod , " " ,
                            g_opera_cod ," ",
                            0, " ", --Folio
                            "'",p_nombre_archivo CLIPPED, "' ",                                                          
                            " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                            "/nohup:",g_pid USING "&&&&&",":",
                            g_proceso_cod USING "&&&&&",":",
                            g_opera_cod   USING "&&&&&" ,
                            " 2>&1 &"
         DISPLAY v_s_comando                        
         RUN v_s_comando
         CALL fn_mensaje("Atención",
                         "Se ha enviado la integración. \n"||
                         "Podrá revisar el resultado en el monitor de ejecución de procesos",
                         "information")
      ELSE
         CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
         CALL fn_mensaje("Atención",v_mensaje,"stop")
      END IF     
   ELSE
      CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atención",v_mensaje,"stop")
   END IF
END FUNCTION