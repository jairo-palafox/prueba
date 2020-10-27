--===============================================================
-- Version: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Módulo            => AGR                                           #
#Programa          => AGRC10                                        #
#Objetivo          => Programa que realiza la consulta de marcas    #
#                     de WS con Procesar                            #
#Autor             => Mauro Muñiz Caballero                         #
#Fecha creación    => 9 septiembre 2012                             #
#####################################################################

IMPORT os

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE g_ban_salir       SMALLINT
   DEFINE g_tpo_originacion LIKE cre_acreditado.tpo_originacion --tipo de originacion
   DEFINE p_tipo_ejecucion  SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo        STRING -- titulo de la ventana

   DEFINE arr_arbol_marca DYNAMIC ARRAY OF RECORD
      desc_situacion CHAR(8),
      --desc_credito   LIKE sfr_marca.descripcion_marca,
      fecha          LIKE sfr_marca_activa.f_inicio,
      tot_marcas     SMALLINT,
      padre_id       STRING,
      id             STRING,
      nivel          SMALLINT
   END RECORD

   DEFINE r_detsol DYNAMIC ARRAY OF RECORD
      solicitud      CHAR(25),
      total_sol      INTEGER
   END RECORD

END GLOBALS

MAIN

   -- se recuperan las claves desde parámetro
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".AGRC10.log")

   -- si se obtuvo el título, se pone como título de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --se inicializan variables
   LET g_ban_salir = FALSE

   --se llama a la funcion donde se selecciona el tipo de consulta
   CALL fn_muestra_marcas(2)
   CALL arr_arbol_marca.clear()

END MAIN

## ventana que muestra las marcas consultadas ##
FUNCTION fn_muestra_marcas(p_opt_cons)
#fmm----------------------------------

   DEFINE p_opt_cons       SMALLINT --parámetro que indica el tipo de consulta
   DEFINE v_indice         SMALLINT
   DEFINE v_manejador_rpt  OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE r_ruta_bin       LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte    VARCHAR(80) -- nombre del reporte
   DEFINE r_ruta_listados  LIKE seg_modulo.ruta_listados
   DEFINE f_w              ui.form
   DEFINE w                ui.window
   DEFINE v_existe_archivo INTEGER
   DEFINE v_indice_rep     SMALLINT
   DEFINE v_situacion_d    SMALLINT
   DEFINE v_fecha_d        DATE
   DEFINE v_pos            SMALLINT
   DEFINE resp_visualiza   SMALLINT

   OPEN WINDOW w_consulta_marca WITH FORM "AGRC102"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()

      DISPLAY ARRAY arr_arbol_marca TO scr1.*

         BEFORE DISPLAY
            LET g_ban_salir = FALSE
            IF p_opt_cons = 1 THEN --si la consulta es global
              --se llama a la funcion que llena el arbol a mostrar 
              CALL fn_llena_arbol_marcas(p_opt_cons,TODAY,NULL)
            ELSE   --la consulta es por fecha
              --llama a la funcion donde se captura la fecha y el tipo de credito
              CALL fn_cons_fecha(p_opt_cons)
            END IF

            --se valida que si se cancelo la consulta cierre la ventana
            IF g_ban_salir = TRUE THEN 
               EXIT DISPLAY 
            END IF

            IF arr_arbol_marca[1].desc_situacion IS NULL AND arr_arbol_marca[1].fecha IS NULL THEN
               CALL fn_mensaje("Aviso","No existen registros de solicitudes de marca o demarca activas","info")
               EXIT DISPLAY
            END IF

         BEFORE ROW
            LET v_pos = ARR_CURR()

            IF arr_arbol_marca[v_pos].nivel > 1 THEN
               IF arr_arbol_marca[v_pos].desc_situacion = 'MARCA' THEN
                  LET v_situacion_d = 2
               ELSE
                  LET v_situacion_d = 0
               END IF

               LET v_fecha_d = arr_arbol_marca[v_pos].fecha
               
               CALL fn_despliega_detalle_sol(v_situacion_d, v_fecha_d)
                    RETURNING resp_visualiza

               IF NOT resp_visualiza THEN
                  CALL r_detsol.clear()
               ELSE
                  CALL r_detsol.deleteelement(r_detsol.getlength())
               END IF
            ELSE
               CALL r_detsol.clear()
            END IF

         ON ACTION ACCEPT
            EXIT DISPLAY

         ON ACTION CANCEL
            EXIT DISPLAY

         ON ACTION reporte
            -- CALL fn_reporte_marcas()
            LET v_indice = arr_arbol_marca.getLength()

            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

            # Se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("AGRC101.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               LET v_nom_reporte = g_usuario_cod CLIPPED || "-AGRC10-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)

               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_marca TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice
                   OUTPUT TO REPORT reporte_marca(arr_arbol_marca[v_indice_rep].*)
               END FOR

               #Finaliza el reporte
               FINISH REPORT reporte_marca
               LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  # se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               # si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF

               # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
               CALL ui.Interface.refresh()        

               CALL fn_mensaje("Aviso","Se ha generado el reporte de marcas","info") 
            ELSE
               DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
            END IF
         END DISPLAY

         DISPLAY ARRAY r_detsol TO scr2.*

   CLOSE WINDOW w_consulta_marca

END FUNCTION 

## Función que llena el árbol de marcas a desplegar ##
FUNCTION fn_llena_arbol_marcas(p_opt_cons,p_fec_marca,p_marca_prc)
#flam-------------------------------------------------------------

    DEFINE qry_string       STRING
    DEFINE i                INTEGER
    DEFINE j                INTEGER
    DEFINE cont_arbol       INTEGER
    DEFINE v_situacion      SMALLINT
    DEFINE v_d_situacion    CHAR(8)
    DEFINE v_marca_prc      SMALLINT
    DEFINE v_desc_credito   LIKE cat_tipo_credito.desc_credito
    DEFINE v_marca          LIKE sfr_marca_activa.marca
    DEFINE p_fec_marca      LIKE sfr_marca_activa.f_inicio
    DEFINE p_marca_prc      SMALLINT
    DEFINE p_opt_cons       SMALLINT --parametro que indica el tipo de consulta
    DEFINE v_fecha_ini      LIKE sfr_marca_activa.f_inicio
    DEFINE v_tot_marca      INTEGER
    DEFINE v_sum_marca      INTEGER
    DEFINE v_marca_tmp      LIKE sfr_marca_activa.marca
    DEFINE v_fec_marca_tmp  LIKE sfr_marca_activa.f_inicio

    IF p_opt_cons = 1 THEN --consulta global
        LET qry_string = " SELECT DISTINCT s.situacion \n",
                         "   FROM cta_his_marca_ws s \n"
    ELSE --consulta por fecha
       LET qry_string =  " SELECT DISTINCT s.situacion \n",
                         "   FROM cta_his_marca_ws s \n",
                         "  WHERE  s.f_actualiza = '",p_fec_marca,"'\n"
    END IF

    DISPLAY "consulta crédito", qry_string
    PREPARE prp_nivel1 FROM qry_string
    DECLARE cur_nivel1 CURSOR FOR prp_nivel1

    LET i           = 1
    LET j           = 1
    LET cont_arbol  = 1
    LET v_sum_marca = 0
    LET v_tot_marca = 0

     --se limpia el arreglo que se va a desplegar
    CALL arr_arbol_marca.clear()

    FOREACH cur_nivel1 INTO v_situacion, v_marca_prc, v_desc_credito
        LET arr_arbol_marca[cont_arbol].desc_situacion = fn_desc_situacion(v_situacion)
        LET arr_arbol_marca[cont_arbol].fecha          = ""

        IF p_opt_cons = 1 THEN --consulta global 
           LET qry_string = " SELECT UNIQUE s.situacion, s.f_actualiza, COUNT(*)\n ",
                            "  FROM cta_his_marca_ws s \n",
                            " WHERE s.situacion = ",v_situacion,"\n",
                            " GROUP BY 1,2 \n",
                            " ORDER BY 1 desc, 2 DESC "
        ELSE
           LET qry_string = " SELECT UNIQUE s.situacion, s.f_actualiza, COUNT(*)\n ",
                            "   FROM cta_his_marca_ws s \n",
                            "  WHERE s.f_actualiza = '",p_fec_marca,"'\n",
                            "    AND s.situacion = ",v_situacion,"\n",
                            " GROUP BY 1,2 \n",
                            " ORDER BY 1 desc, 2 DESC "
        END IF

        LET v_sum_marca = 0
        DISPLAY "MARCAS ", qry_string
        PREPARE prp_cta_marcas FROM qry_string
        DECLARE cur_cta_marcas CURSOR FOR prp_cta_marcas
        FOREACH cur_cta_marcas INTO v_situacion, v_fec_marca_tmp, v_tot_marca
           LET v_sum_marca = v_sum_marca + v_tot_marca
        END FOREACH

        LET arr_arbol_marca[cont_arbol].tot_marcas = v_sum_marca
        LET arr_arbol_marca[cont_arbol].id = v_marca_prc
        LET arr_arbol_marca[cont_arbol].nivel = 1
        LET arr_arbol_marca[cont_arbol].padre_id = ""
        LET cont_arbol = cont_arbol + 1

        IF p_opt_cons = 1 THEN --consulta global 
           LET qry_string = " SELECT UNIQUE DECODE(s.situacion,0,'Desmarca',2,'Marca'), s.f_actualiza, COUNT(*)\n ",
                            "  FROM cta_his_marca_ws s \n",
                            " WHERE s.situacion = ",v_situacion,"\n",
                            " GROUP BY 1, 2 \n",
                            " ORDER BY 1 desc, 2 DESC "
        ELSE
           LET qry_string = " SELECT UNIQUE DECODE(s.situacion,0,'Desmarca',2,'Marca'), s.f_actualiza, COUNT(*)\n ",
                            "   FROM cta_his_marca_ws s \n",
                            "  WHERE s.f_actualiza = '",p_fec_marca,"'\n",
                            "   AND s.situacion = ",v_situacion,"\n",
                            " GROUP BY 1, 2\n",
                            " ORDER BY 1 desc, 2 DESC "
        END IF 

        DISPLAY "CONSULTA MARCAS N2", qry_string
        PREPARE prp_nivel2 FROM qry_string
        DECLARE cur_nivel2 CURSOR FOR prp_nivel2
        FOREACH cur_nivel2 INTO v_d_situacion, v_fecha_ini, v_tot_marca

           LET arr_arbol_marca[cont_arbol].desc_situacion = v_d_situacion
           LET arr_arbol_marca[cont_arbol].fecha          = v_fecha_ini
           LET arr_arbol_marca[cont_arbol].tot_marcas     = v_tot_marca
           LET arr_arbol_marca[cont_arbol].id             = v_marca_prc USING"<<",".",
                                                            v_marca USING"<<"
           LET arr_arbol_marca[cont_arbol].nivel          = 2
           LET arr_arbol_marca[cont_arbol].padre_id       = v_marca_prc USING"<<"
           LET cont_arbol = cont_arbol + 1

           LET j = j + 1
        END FOREACH

        CLOSE cur_nivel2
        LET i = i + 1
    END FOREACH

    CLOSE cur_nivel1

END FUNCTION

## Funcion que obtiene la descripcion de una marca ##
FUNCTION fn_desc_marca(p_marca)

   DEFINE p_marca       LIKE sfr_marca_activa.marca
   DEFINE v_desc_marca  LIKE sfr_marca.descripcion_marca

   SELECT descripcion_marca
     INTO v_desc_marca
     FROM sfr_marca
    WHERE marca = p_marca
   
   RETURN  v_desc_marca

END FUNCTION

## Funcion que obtiene la descripcion de la situación##
FUNCTION fn_desc_situacion(p_situacion)

   DEFINE p_situacion      SMALLINT
   DEFINE v_desc_situacion CHAR(8)

   IF p_situacion = 2 THEN
      LET v_desc_situacion = 'MARCA'
   ELSE
      LET v_desc_situacion = 'DESMARCA'
   END IF

   RETURN  v_desc_situacion

END FUNCTION

## funcion que permite capturar la fecha y el tipo de credito a consultar ##
FUNCTION fn_cons_fecha(p_opt_cons)
#fcf------------------------------

   DEFINE v_fecha        DATE
   DEFINE v_marca_prc    SMALLINT
   DEFINE cb             ui.ComboBox
   DEFINE v_marca        SMALLINT
   DEFINE v_desc_credito CHAR(50)
   DEFINE v_sqlqry       STRING
   DEFINE p_opt_cons     SMALLINT --parametro que indica el tipo de consulta

   OPEN WINDOW w_op_fecha WITH FORM "AGRC101"

      --INPUT v_fecha, v_marca_prc FROM fecha, marca_prc ATTRIBUTE (UNBUFFERED)

      LET v_marca_prc = ""

      INPUT v_fecha FROM fecha ATTRIBUTE (UNBUFFERED)
         BEFORE INPUT 
            LET v_fecha = TODAY
            DISPLAY v_fecha TO fecha

            NEXT FIELD fecha

         ON ACTION ACCEPT 
           IF v_fecha IS NULL THEN 
              CALL fn_mensaje("Aviso","Se requiere ingresar la fecha de consulta","stop")
              CONTINUE INPUT
           ELSE
              --se llama a la función que llena el árbol a mostrar
              CALL fn_llena_arbol_marcas(p_opt_cons,v_fecha,v_marca_prc)
              EXIT INPUT
           END IF 
 
         ON ACTION CANCEL
            --se canceló y se cierra la ventana de despliegue de registros
            LET g_ban_salir = TRUE
            EXIT INPUT

      END INPUT
   CLOSE WINDOW w_op_fecha

END FUNCTION

#OBJETIVO: Genera el reporte de consulta de deudor
REPORT reporte_marca(v_rec_marca)

   DEFINE v_rec_marca     RECORD 
      desc_situacion CHAR(8),
      fecha          LIKE sfr_marca_activa.f_inicio,
      tot_marcas     SMALLINT,
      padre_id       STRING,
      id             STRING,
      nivel          SMALLINT
   END RECORD

   DEFINE v_fecha_reporte   DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX g_usuario_cod
                    
   ON EVERY ROW
      PRINTX v_rec_marca.desc_situacion
      PRINTX v_rec_marca.fecha USING "dd-mm-yyyy"
      PRINTX v_rec_marca.tot_marcas
      PRINTX v_rec_marca.nivel
END REPORT

FUNCTION fn_despliega_detalle_sol(p_situacion, p_fecha)
#fdds--------------------------------------------------

   DEFINE p_situacion   SMALLINT
   DEFINE i             SMALLINT
   DEFINE v_tot         DECIMAL(6,0)
   DEFINE v_sol         DECIMAL(6,0)

   DEFINE p_fecha       DATE

   ---CALL r_detsol.clear()

   LET i = 1
   LET v_sol = 0
   LET v_tot = 0

   ---Solicitudes aceptadas
   SELECT count(*)
     INTO v_sol
     FROM cta_his_marca_ws ha
    WHERE ha.situacion = p_situacion
      AND ha.f_actualiza = p_fecha
      AND ha.diagnostico = 1

   LET r_detsol[i].solicitud = "ACEPTADAS"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   LET i = i + 1
   LET v_sol = 0

   ---Rechazadas actual
   SELECT count(*)
     INTO v_sol
     FROM cta_marca_ws ra
    WHERE ra.situacion = p_situacion
      AND ra.f_solicita = p_fecha
      AND ra.diagnostico is not null

   LET r_detsol[i].solicitud = "RECHAZADAS ACTUAL"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   LET i = i + 1
   LET v_sol = 0

   ---Enviadas

   LET r_detsol[i].solicitud = "ENVIADAS"
   LET r_detsol[i].total_sol = v_tot

   LET i = i + 1
   LET v_sol = 0

   ---Rechazadas anteriores
   SELECT count(*)
     INTO v_sol
     FROM cta_marca_ws rh
    WHERE rh.situacion = p_situacion
      AND rh.f_solicita < p_fecha
      AND rh.diagnostico is not null

   LET r_detsol[i].solicitud = "RECHAZADAS ANTERIORES"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   LET i = i + 1
   LET v_sol = 0

   ---Por enviar
   SELECT count(*)
     INTO v_sol
     FROM cta_marca_ws ev
    WHERE ev.f_solicita = p_fecha
      AND ev.situacion = p_situacion
      AND ev.diagnostico is null

   LET r_detsol[i].solicitud = "POR ENVIAR"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   IF v_tot = 0 THEN
      RETURN 0
   ELSE
      RETURN 1
   END IF

END FUNCTION