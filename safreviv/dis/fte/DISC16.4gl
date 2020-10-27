################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 21/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC16                                                   #
#Objetivo          => Programa para consultar o identificar los registros de   #
#                     casos de excepción a dispersar.                          #
#Fecha inicio      => 08/04/2014                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_proceso_cod            SMALLINT,    --codigo del proceso
    p_opera_cod              SMALLINT,    --codigo de operacion
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_tot_registros          DECIMAL(9,0), -- Total de registros
    v_sum_aportacion         DECIMAL(12,2), 
    v_sum_amortizacion       DECIMAL(12,2)

  DEFINE v_arr_casos         DYNAMIC ARRAY OF RECORD
    v_arr_folio              DECIMAL(9,0),
    v_arr_nss                CHAR(11),
    v_arr_nombre_completo    VARCHAR(50),
    v_arr_num_credito        DECIMAL(10,0),
    v_arr_periodo_pago       CHAR(6),
    v_arr_f_pago             DATE,
    v_arr_ent_recauda        CHAR(3),
    v_arr_nrp                CHAR(11),
    v_arr_aportacion         DECIMAL(12,2),
    v_arr_amortizacion       DECIMAL(12,2), 
    v_arr_folio_sua          DECIMAL(6,0),
    v_arr_estado             VARCHAR(50),
    v_arr_f_registro         DATE,
    v_arr_usuario            CHAR(20),
    v_arr_id_derecho         DECIMAL(9,0)
  END RECORD

  DEFINE 
    v_nombre_completo        VARCHAR(50), --Nombre del derechohabiente
    v_periodo_cvt1           VARCHAR(6),
    v_periodo_cvt2           VARCHAR(6)

  DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin      --ruta del bin del módulo
  DEFINE r_ruta_listados     LIKE seg_modulo.ruta_listados --ruta de listados del módulo
  DEFINE v_ruta_envio_dis    LIKE seg_modulo.ruta_envio    --ruta del archivo de salida
  DEFINE v_nombre_int_hs     VARCHAR(100)
  DEFINE v_usuario_hs        CHAR(20)
  DEFINE v_proc_entra        SMALLINT,
         v_proc_val          SMALLINT,
         v_cod_conv          SMALLINT,
         v_desc_proc_val     CHAR(40),
         v_mensaje_val       STRING

END GLOBALS

MAIN
  DEFINE 
    f_folio                  DECIMAL(9,0),  --Folio de la liquidación de la dispersión
    f_nss                    CHAR(11),      --NSS trabajador
    r_registros              DECIMAL(9,0),  --Bandera de consulta avance de pago
    r_sum_aportacion         DECIMAL(12,2),
    r_sum_amortizacion       DECIMAL(12,2) 
  
  DEFINE 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form ,  --Define las propiedades de la forma
    v_ruta_listados          CHAR(40),
    v_cuenta_derechohabiente SMALLINT,
    v_cuenta_folio           INTEGER,
    v_tot_ctas_casos         DECIMAL(9,0),
    bnd_consulta             SMALLINT,
    r_bnd_periodo            SMALLINT,
    v_qwery_ibx              STRING 

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
  LET p_proceso_cod  = 917
  LET p_opera_cod    = 1 
  LET r_bnd_periodo  = 0

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####
   
  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance
   
  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = 'dis'

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

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

  LET bnd_consulta = 0

  CLOSE WINDOW SCREEN
   
  OPEN WINDOW vtn_inconsistente WITH FORM "DISC161"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_folio, f_nss
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_nombre", TRUE ) --Oculta el nombre del trabajador
          CALL f_forma.setElementHidden("gr_detalle", TRUE ) --Oculta detalle de la consulta
          CALL ui.interface.refresh()
       
        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro
           IF (f_folio IS NULL) AND 
              (f_nss   IS NULL) THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar un criterio para la búsqueda",
                              "about")
              NEXT FIELD f_folio   
           END IF  

           IF f_folio IS NOT NULL THEN
              --Valida que exista el folio
                 SELECT COUNT(*) 
                 INTO   v_cuenta_folio
                 FROM   dis_caso_excepcion
                 WHERE  folio = f_folio
                 IF v_cuenta_folio  = 0    OR 
                    v_cuenta_folio IS NULL THEN 
                    CALL fn_mensaje("Atención","No existen registros para el folio capturado",
                                    "about")
                    NEXT FIELD f_folio
                 END IF 
           END IF
               
           IF f_nss IS NOT NULL THEN 
              IF LENGTH(f_nss) = 11 THEN 
                 --Valida que exista el nss
                 SELECT COUNT(id_derechohabiente) 
                 INTO   v_cuenta_derechohabiente
                 FROM   afi_derechohabiente
                 WHERE  nss = f_nss
                 IF v_cuenta_derechohabiente = 0     OR 
                    v_cuenta_derechohabiente IS NULL THEN 
                    CALL fn_mensaje("Atención","El NSS no existe",
                                    "about")
                    NEXT FIELD f_nss
                 END IF 
              ELSE 
                 CALL fn_mensaje("Atención","El NSS debe ser de 11 caracteres",
                                 "about")
                 NEXT FIELD f_nss
              END IF 
           END IF  

           DISPLAY "f_nss -- ",f_nss
           DISPLAY "f_folio -- ",f_folio
            
           CALL fn_consulta_casos_excepcion(f_folio,
                                            f_nss)
           RETURNING r_registros,
                     r_sum_aportacion,
                     r_sum_amortizacion

           DISPLAY "r_registros -- ",r_registros

           IF r_registros > 0 THEN
              CALL f_forma.setElementHidden("gr_detalle", FALSE ) --muestra detalle de la consulta
               
              DISPLAY ARRAY v_arr_casos TO rcd_detalle.* 
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE ROW
                  CALL fn_obtiene_nombre_derechohabiente(v_arr_casos[ARR_CURR()].v_arr_id_derecho) 
               
                BEFORE DISPLAY 
                  DISPLAY r_registros        TO f_total_registros
                  DISPLAY r_sum_aportacion   TO f_suma_aportaciones
                  DISPLAY r_sum_amortizacion TO f_suma_amortizaciones

                AFTER DISPLAY 
                  CALL ui.interface.refresh()
                  CALL DIALOG.setActionHidden("reporte",0)
                  CALL ui.interface.refresh()

                ON ACTION cancelar 
                   EXIT PROGRAM 
                    
                ON ACTION reporte
                   CALL fn_reporte_casos_exc(f_folio,
                                             f_nss)
                --Genera archivo
                ON ACTION archivo
                   SELECT COUNT(*)
                   INTO   v_tot_ctas_casos
                   FROM   dis_caso_excepcion ce
                   WHERE  ce.folio  = f_folio
                   AND    ce.estado = 0
                   IF v_tot_ctas_casos <> 0 THEN
                      CALL fn_rutas("dis") RETURNING r_c_ruta_bin, r_ruta_listados

                      LET g_sql_txt = "fglrun ", r_c_ruta_bin CLIPPED, "/DISS29.42r ", f_folio, " ", g_usuario
                      RUN g_sql_txt

                      SELECT dc.nombre_archivo
                      INTO   v_nombre_int_hs
                      FROM   dis_ctr_caso_excepcion dc
                      WHERE  dc.folio = f_folio

                      --LET v_nombre_int_hs = v_ruta_envio_dis || v_nombre_int_hs

                      CALL fn_mensaje("Información",
                                      "Se ha generado la interface de HS para los Casos de Excepción\n en la ruta "||v_nombre_int_hs,
                                      "information")

                   ELSE
                      CALL fn_mensaje("ATENCIÓN",
                                      "No se encontraron registros o interface ya generada.",
                                      "about")
                      CALL ui.interface.refresh()
                   END IF
              END DISPLAY                    
           ELSE
              CALL fn_mensaje("ATENCIÓN",
                              "No se encontraron registros",
                              "about")
              CALL ui.interface.refresh()
           END IF
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG
          
    END DIALOG 
  CLOSE WINDOW vtn_inconsistente 

END MAIN

#Objetivo: Consulta para verificar si existe información con los parametros 
#          capturados
FUNCTION fn_consulta_casos_excepcion(f_folio, f_nss)
  DEFINE 
    f_folio                  DECIMAL(9,0), --Folio de liquidación dispersión
    f_nss                    CHAR(11),     --NSS trabajador
    v_derechohabiente        DECIMAL(9,0), --Id
    v_indice                 INTEGER 

  LET g_sql_txt = "\n SELECT ce.folio, ",
                  "\n        ce.nss, ",
                  "\n        rtrim(af.nombre_af) ||' '|| ",
                  "\n        rtrim(af.ap_paterno_af) ||' '|| ",
                  "\n        rtrim(af.ap_materno_af), ",
                  "\n        ce.num_credito, ",
                  "\n        ce.periodo_pago, ",
                  "\n        ce.f_pago, ",
                  "\n        ce.ent_recaudadora, ",
                  "\n        ce.nrp, ",
                  "\n        ce.aportacion, ",
                  "\n        ce.amortizacion, ",
                  "\n        ce.folio_sua, ",
                  "\n        ce.estado ||'-'|| ",
                  "\n        rtrim(ca.desc_edo_caso_exc), ",
                  "\n        ce.f_registro, ",
                  "\n        ce.usuario, ",
                  "\n        ce.id_derechohabiente ",
                  "\n FROM   dis_caso_excepcion ce, ",
                  "\n        afi_derechohabiente af, ",
                  "\n        cat_edo_caso_excepcion ca ",
                  "\n WHERE  ce.id_derechohabiente = af.id_derechohabiente ",
                  "\n AND    ce.estado             = ca.cod_edo_caso_exc "

  -- Valida si se capturo el folio de casos de excepción
  IF f_folio IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt||"\n AND ce.folio = ", f_folio
  END IF

  -- Valida si se capturo el nss (id_derechohabiente)
  IF f_nss IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt||"\n AND ce.nss = ", f_nss
  END IF

  LET g_sql_txt = g_sql_txt||"\n ORDER BY 1,5 desc,12,2"

  DISPLAY "g_sql_txt -- ",g_sql_txt
  PREPARE prp_sql_casos_exc FROM g_sql_txt
  
  LET v_indice           = 1
  LET v_tot_registros    = 1
  LET v_sum_aportacion   = 0.00
  LET v_sum_amortizacion = 0.00

  --Iteración de registros con base en la consulta temporal
  DECLARE cur_casos_exc CURSOR FOR prp_sql_casos_exc
  FOREACH cur_casos_exc INTO v_arr_casos[v_indice].v_arr_folio,
                             v_arr_casos[v_indice].v_arr_nss,
                             v_arr_casos[v_indice].v_arr_nombre_completo,
                             v_arr_casos[v_indice].v_arr_num_credito,
                             v_arr_casos[v_indice].v_arr_periodo_pago,
                             v_arr_casos[v_indice].v_arr_f_pago,
                             v_arr_casos[v_indice].v_arr_ent_recauda,
                             v_arr_casos[v_indice].v_arr_nrp,
                             v_arr_casos[v_indice].v_arr_aportacion,
                             v_arr_casos[v_indice].v_arr_amortizacion,
                             v_arr_casos[v_indice].v_arr_folio_sua,
                             v_arr_casos[v_indice].v_arr_estado,
                             v_arr_casos[v_indice].v_arr_f_registro,
                             v_arr_casos[v_indice].v_arr_usuario,
                             v_arr_casos[v_indice].v_arr_id_derecho
                                   
    LET v_tot_registros    = v_tot_registros    + 1
    LET v_sum_aportacion   = v_sum_aportacion   + v_arr_casos[v_indice].v_arr_aportacion
    LET v_sum_amortizacion = v_sum_amortizacion + v_arr_casos[v_indice].v_arr_amortizacion
    LET v_indice           = v_indice           + 1
  END FOREACH

  CALL v_arr_casos.deleteElement(v_indice)
  LET v_tot_registros = v_tot_registros - 1

  RETURN v_tot_registros,
         v_sum_aportacion,
         v_sum_amortizacion

END FUNCTION

#Objetivo: Genera reporte de casos de excepción a dispersar
FUNCTION fn_reporte_casos_exc(v_rfolio, v_rnss)
  DEFINE 
    v_rfolio                 DECIMAL(9,0), --Folio de liquidación de dispersión
    v_rnss                   CHAR(11), --NSS trabajador
    manejador_rpt            om.SaxDocumentHandler, --Contenedor documentos reporte
    v_rep_indice             INTEGER
  
  LET v_rep_indice = 1
  
  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC161.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  -- Extrae nombre de la interface de HS y el usuario que lo genero
  IF v_rfolio IS NOT NULL THEN
     SELECT ctr.nombre_archivo, ctr.usuario_genera
     INTO   v_nombre_int_hs, v_usuario_hs
     FROM   dis_ctr_caso_excepcion ctr
     WHERE  ctr.folio = v_rfolio
     IF STATUS = NOTFOUND THEN
        LET v_nombre_int_hs = 'Sin generar'
        LET v_usuario_hs    = ' '
     END IF
  ELSE
        LET v_nombre_int_hs = ' '
        LET v_usuario_hs    = ' '
  END IF  

  --Inicia el reporte de registros
  START REPORT rp_casos_excep TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO v_arr_casos.getLength()
        OUTPUT TO REPORT rp_casos_excep(v_arr_casos[v_rep_indice].*,
                                        v_tot_registros,
                                        v_sum_aportacion,
                                        v_sum_amortizacion,
                                        g_usuario,
                                        v_rfolio,
                                        v_nombre_int_hs,
                                        v_usuario_hs)
    END FOR 
  FINISH REPORT rp_casos_excep
END FUNCTION

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_derechohabiente(v_id_consulta)
  DEFINE   
    v_id_consulta            DECIMAL(9,0)

  SELECT   rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO     v_nombre_completo
  FROM     afi_derechohabiente
  WHERE    id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO v_nombre
   
END FUNCTION 

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rp_casos_excep(v_rep_casos, 
                      v_rep_tot_registros, 
                      v_rep_sum_aportacion,
                      v_rep_sum_amortizacion, 
                      v_usuario,
                      v_rep_folio,
                      v_rep_nombre_int_hs,
                      v_rep_usuario_hs)

  DEFINE v_rep_casos         RECORD
    v_rep_folio              DECIMAL(9,0),
    v_rep_nss                CHAR(11),
    v_rep_nombre_completo    VARCHAR(30),
    v_rep_num_credito        DECIMAL(10,0),
    v_rep_periodo_pago       CHAR(6),
    v_rep_f_pago             DATE,
    v_rep_ent_recauda        CHAR(3),
    v_rep_nrp                CHAR(11),
    v_rep_aportaciones       DECIMAL(12,2),
    v_rep_amortizaciones     DECIMAL(12,2),
    v_rep_folio_sua          DECIMAL(6,0),
    v_rep_estado             VARCHAR(50),
    v_rep_f_registro         DATE,
    v_rep_usuario            CHAR(20),
    v_rep_id                 DECIMAL(9,0)
  END RECORD

  DEFINE 
    v_fecha_consulta         DATE, -- Fecha de proceso
    v_usuario                VARCHAR(30), -- Almacena al usuario
    v_rep_tot_registros      DECIMAL(9,0), -- Total de registros
    v_rep_sum_aportacion     DECIMAL(12,2),
    v_rep_sum_amortizacion   DECIMAL(12,2),
    v_rep_folio              DECIMAL(9,0),
    v_rep_nombre_int_hs      VARCHAR(100),
    v_rep_usuario_hs         CHAR(20)

  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

      PRINTX v_rep_nombre_int_hs
      PRINTX v_rep_usuario_hs

    ON EVERY ROW
      PRINTX v_rep_casos.v_rep_folio
      PRINTX v_rep_casos.v_rep_nss
      PRINTX v_rep_casos.v_rep_nombre_completo
      PRINTX v_rep_casos.v_rep_num_credito
      PRINTX v_rep_casos.v_rep_periodo_pago
      PRINTX v_rep_casos.v_rep_f_pago USING "dd-mm-yyyy"
      PRINTX v_rep_casos.v_rep_ent_recauda
      PRINTX v_rep_casos.v_rep_nrp
      PRINTX v_rep_casos.v_rep_aportaciones
      PRINTX v_rep_casos.v_rep_amortizaciones
      PRINTX v_rep_casos.v_rep_folio_sua
      PRINTX v_rep_casos.v_rep_estado
      PRINTX v_rep_casos.v_rep_f_registro USING "dd-mm-yyyy"
      PRINTX v_rep_casos.v_rep_usuario

    ON LAST ROW
      PRINTX v_rep_tot_registros
      PRINTX v_rep_sum_aportacion
      PRINTX v_rep_sum_amortizacion

END REPORT