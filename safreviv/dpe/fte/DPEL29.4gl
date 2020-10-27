--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 03/05/2012
--===============================================================

#########################################################################################
#Modulo       => DPE                                                                    #
#Programa     => DPEL14                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                del detalle de la devolucion de pagos indebidos o en exceso INFONAVIT  #
#Fecha inicio => 24/04/2012                                                             #
#########################################################################################
GLOBALS "DPEG01.4gl"
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

   -- DEFINE arr_fuente       DYNAMIC ARRAY OF STRING
   -- DEFINE arr_destino      DYNAMIC ARRAY OF STRING

    DEFINE arr_fuente DYNAMIC ARRAY OF RECORD
        archivo          STRING ,
        total_aceptados  INTEGER
    END RECORD

    DEFINE arr_destino DYNAMIC ARRAY OF RECORD
        archivo          STRING ,
        total_aceptados  INTEGER
    END RECORD

    DEFINE arr_folio DYNAMIC ARRAY OF RECORD
        archivo          STRING,
        f_lote           DATE ,
        lote             SMALLINT,
        f_asignacion     DATE ,
        f_apertura       DATE  ,
        total_patrones  INTEGER,
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
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   LET g_usuario = p_usuario_cod
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_dpe_infonavit -- Devolucion Pagos indebidos INFONAVIT
   LET g_opera_cod   = 2 -- Integracion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
   --AND
   -- estado_cod = 2 --

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'dpe'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   --CALL fn_dpe_ejecuta_integracion_INFONAVIT(p_usuario_cod)
   CALL fn_drgandrop_integra_devol_exceso(p_usuario_cod)
END MAIN

FUNCTION fn_drgandrop_integra_devol_exceso(p_usuario_cod)
DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       l_arr_arch_pend    DYNAMIC ARRAY OF VARCHAR(100), -- Archivos pendientes 
       l_arr_arch_int     DYNAMIC ARRAY OF VARCHAR(100), -- Archivos a integrar
       l_arr_integrados   DYNAMIC ARRAY OF RECORD -- Detalle archivos a integrar
            folio             LIKE dis_det_avance_pago.folio,
            nom_archivo       VARCHAR(100),
            f_presenta        DATE,
            v_f_gen_archivo   DATE,
            total_solicitudes INTEGER,
            total_aporta_solicitada   DECIMAL(11,2),
            total_amortiza_solicitada DECIMAL(11,2)
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
       l_i_tot_reg        SMALLINT, -- total de registros en archivo
       l_s_qryTxt         STRING, -- guarda una sentencia SQL a ejecutar
       l_comando          STRING,
       v_ruta_ejecutable  CHAR(40),
       v_respuesta        SMALLINT
       --
       ,v_r_arch_pend     VARCHAR(100) -- Archivos pendientes
       ,v_s_qry           STRING
       ,v_dte_fechaArchiv DATE
       ,v_aportacion_solicitada DECIMAL(11,0)
       ,v_amortiza_solicitada   DECIMAL(11,0)
       ,v_f_gen_archivo         CHAR(8)
       ,v_date_gen_archivo      DATE

   CONSTANT l_nom_tbl_pend = "archfuente" -- Tabla de archivos pendientes
   CONSTANT l_nom_tbl_int = "archdestino" -- Tabla de archivos a integrar

   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo
   WHERE modulo_cod = "dpe"
   
   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET l_s_qryTxt = "SELECT nombre_archivo, f_actualiza"
                    ,"\n FROM glo_ctr_archivo "
                    ,"\n WHERE proceso_cod = ",g_proceso_cod
                    ,"\n   AND opera_cod  =1"--,g_opera_cod --,g_opera_cod que tiene la carga 
                    ,"\n   AND estado = 1" -- archivos pendientes de integrar

   --DISPLAY l_s_qryTxt
   PREPARE Prpr_ObtArchVal FROM l_s_qryTxt CLIPPED
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
   OPEN WINDOW w_inte_devol WITH FORM "DPEL290"
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
            CALL DIALOG.setActionHidden("deshacer",1)-- ocultar JTSO 03/05/2012
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
                            ,"\n FROM safre_tmp:tmp_det_recauda_hipoteca"
            PREPARE pre_sql_total_registros_archivo FROM v_s_qry
            EXECUTE pre_sql_total_registros_archivo INTO l_i_tot_reg
            -- se asigna el total de solicitudes
           LET l_arr_integrados[l_i_iter].total_solicitudes = l_i_tot_reg

           -- calcula total de montos solicitados
           LET l_i_tot_reg = 0
           LET v_s_qry =
               "SELECT SUM(aportacion_solicitada), SUM(amortiza_solicitada)",
               "\n  FROM safre_tmp:tmp_det_recauda_hipoteca"
           PREPARE Prpr_ObtTotalPatrones FROM v_s_qry CLIPPED
           EXECUTE Prpr_ObtTotalPatrones INTO v_aportacion_solicitada, v_amortiza_solicitada
           -- se asigna el total de montos solicitados
           LET l_arr_integrados[l_i_iter].total_aporta_solicitada = v_aportacion_solicitada / 100
           LET l_arr_integrados[l_i_iter].total_amortiza_solicitada = v_amortiza_solicitada / 100
           
           LET v_s_qry =
               "SELECT fec_generacion_archivo",
               "\n  FROM safre_tmp:tmp_cza_recauda_hipoteca"
           PREPARE Prpr_ObtFecGenArchivo FROM v_s_qry CLIPPED
           EXECUTE Prpr_ObtFecGenArchivo INTO v_f_gen_archivo
           
           LET v_s_qry = "EXECUTE PROCEDURE safre_viv:sp_cambia_formato_fecha(?)"
           
           PREPARE Prpr_Procedurefecha FROM v_s_qry
           EXECUTE Prpr_Procedurefecha USING v_f_gen_archivo INTO v_date_gen_archivo;
           
           LET l_arr_integrados[l_i_iter].v_f_gen_archivo = v_date_gen_archivo
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
              "¿Desea ejecutar el proceso de Integración de Carga de Archivo\n"||
              " de Devoluciones de Pago Indebidos o en Exceso?",
              "quest") RETURNING v_respuesta

        IF v_respuesta = 1 THEN
           CALL fn_dpe_ejecuta_integracion_INFONAVIT(p_usuario_cod, l_v_arch_proceso)
           EXIT DIALOG
        END IF

   END DIALOG
CLOSE WINDOW w_inte_devol
END FUNCTION -- fn_drgandrop_integra_devol_exceso

{
======================================================================
Clave: 
Nombre: fn_dpe_ejecuta_integracion_INFONAVIT
Fecha creacion: 25/04/2012
Narrativa: Ejecuta el proceso de la integración DPE INFONAVIT
======================================================================
}
FUNCTION fn_dpe_ejecuta_integracion_INFONAVIT(p_usuario_cod, p_nombre_archivo)
DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod -- usuario que ejecuta el programa
       ,p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
       --
       ,v_s_comando   STRING -- cadena con una instruccion de consola
       ,v_i_resultado INTEGER -- resultado del proceso
       ,v_folio          LIKE deo_preliquida.folio_liquida
       ,v_s_sql STRING
       ,r_bnd_fin_oper  SMALLINT
       ,v_mensaje        STRING

       -- Se verifica si se puede continuar con la operacion
       CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
          RETURNING v_i_resultado
       IF ( v_i_resultado = 0 ) THEN
          
          -- Inicio operacion.
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"DPEL29",
                                    "",p_usuario_cod)
              RETURNING r_bnd_fin_oper
         IF (r_bnd_fin_oper = 0) THEN
         	   
             CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
                RETURNING v_folio
          
          LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/DPEP06 ",
                            p_usuario_cod, " ",
                            g_pid  , " " ,
                            g_proceso_cod , " " ,
                            g_opera_cod ," ",                            
                            v_folio, " ",
                            "'",p_nombre_archivo, "' ",                                                          
                            " 1>",seg_modulo_bat.ruta_listados clipped ,
                            "/nohup:",g_pid        USING "&&&&&",":",
                            g_proceso_cod USING "&&&&&",":",
                            g_opera_cod   USING "&&&&&" ,
                            " 2>&1 &"
          DISPLAY v_s_comando                        
          RUN v_s_comando
          CALL fn_mensaje("Atención",
               "Se ha enviado la integración."||"Folio lote "||v_folio||"\n"||
               "Podrá revisar el resultado en el monitor de ejecución de procesos",
                "information")
         ELSE
            CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje

            CALL fn_mensaje("Atención", v_mensaje, "stop")
         END IF       
       ELSE
          DISPLAY "v_i_resultado:",v_i_resultado
          CALL fn_mensaje("Atención",fn_mues_desc_valida(v_i_resultado),"stop")
       END IF
       
 
END FUNCTION -- fn_dpe_ejecuta_integracion_INFONAVIT


{

 Obtiene la descripcion del error de la validacion y
  la muestra en mensaje para suario.
}
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   --CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida


