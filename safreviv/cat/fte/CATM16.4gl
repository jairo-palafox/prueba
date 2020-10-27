###############################################################################
#Proyecto          => SACI                                                    #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CAT                                                     #
#Programa          => CATM16                                                  #
#Objetivo          => Catalogo Causales Rechazo                               #
#Autor             => Edgar Damian Estrada Rivera, EFP                        #
#Fecha Inicio      => 27 de noviembre del 2017                                #
###############################################################################
IMPORT OS 

DATABASE safre_viv

   DEFINE g_usuario               LIKE seg_usuario.usuario  -- usuario firmado al sistema
   DEFINE g_tipo_carga            SMALLINT  -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE g_nom_prog              VARCHAR(30)  -- nombre del programa
   DEFINE g_hoy                   DATE 

    DEFINE g_arr_causal  DYNAMIC ARRAY OF RECORD
        causal      char(3),
        descripcion char(200),
        entidad     char(3),
        f_actualiza DATE,
        usuario     char(20)
    END RECORD

    DEFINE g_rec_causal RECORD
        causal      char(3),
        descripcion char(200),
        entidad     char(3),
        f_actualiza DATE,
        usuario     char(20)
    END RECORD

    DEFINE g_mensaje   STRING
    DEFINE g_imagen    STRING
    DEFINE g_titulo    STRING

    DEFINE w           ui.Window
    DEFINE f           ui.Form

    DEFINE cb_origen   ui.ComboBox

    # Variables para información del reporte PDF    
   DEFINE v_manejador_rpt  om.SaxDocumentHandler  -- Variable para manejar cambios en el reporte
   DEFINE v_reporte_bin    STRING -- variable para el nombre de la plantilla
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_titulo         CHAR(30)
   DEFINE v_visibilidad    INTEGER
   DEFINE v_ruta_envio     CHAR(40)
   DEFINE v_ruta_lst       CHAR(40)
   DEFINE v_ruta_rpt       STRING
   DEFINE v_existe_archivo INTEGER 
   DEFINE v_nombre_reporte VARCHAR(80)

MAIN

     -- se asignan los parámetros que vienen del fglrun
     LET g_usuario    = ARG_VAL(1)
     LET g_tipo_carga = ARG_VAL(2)
     LET g_nom_prog   = ARG_VAL(3)
     LET g_hoy        = TODAY

    
    CALL fn_inicio()
    
    CALL STARTLOG(g_usuario CLIPPED ||".CATM16.log")
    CALL fn_menu()
        SELECT ruta_bin,
               ruta_listados,
               ruta_envio
          INTO v_ruta_bin,
               v_ruta_lst,
               v_ruta_envio
          FROM seg_modulo
          WHERE modulo_cod = 'cat'
    CALL fn_busqueda(1)
    
END MAIN

FUNCTION fn_inicio()
#función de inicialización de variables

    LET g_hoy    = TODAY
    LET g_titulo = "Causales Rechazo"
    LET g_imagen = "about"

    CALL ui.Interface.setText(g_titulo)

    SELECT USER
      INTO g_usuario
      FROM seg_modulo
     WHERE modulo_cod = "cat"

END FUNCTION

FUNCTION fn_menu()
#función de menú con las opciones de mantenimiento para el catálogo de causales rechazo.

    --CALL ui.interface.settype("child")
    --CALL ui.interface.setcontainer("mdi")
    
    CLOSE WINDOW SCREEN

    OPEN WINDOW CATM161 WITH FORM "CATM161"
    LET w = ui.Window.getCurrent()
    LET f = w.getForm()
    CALL f.setElementHidden("group2",1)

END FUNCTION

FUNCTION fn_busqueda(p_opc)
#función para consultar y eliminar

    DEFINE p_opc        SMALLINT
    DEFINE x_flg        SMALLINT
    DEFINE v_pos        INTEGER
    DEFINE i            INTEGER
    DEFINE v_confirma   BOOLEAN
    DEFINE v_qry        STRING
    DEFINE v_entidad    CHAR(3)

    LET x_flg = 0

    DIALOG ATTRIBUTES(UNBUFFERED)

    INPUT BY NAME v_entidad

        BEFORE INPUT
            CALL DIALOG.setActionHidden("reporte",1)
            CALL DIALOG.setActionHidden("close",1)
            
    END INPUT

    DISPLAY ARRAY g_arr_causal TO rec1.*

    END DISPLAY

    ON ACTION ACCEPT
    
       IF v_entidad IS NOT NULL THEN 
       
            LET v_qry = "SELECT causal,      ",
                             "  desc_causal, ",
                             "  entidad,     ",
                             "  f_actualiza, ",
                             "  usuario      ",
                        "  FROM cat_rechazo_causal",
                        " WHERE entidad = '", v_entidad CLIPPED,"'"

               DISPLAY "condicion ", v_entidad
           ELSE 
           LET v_qry = "SELECT causal,      ",
                            "  desc_causal, ",
                            "  entidad,     ",
                            "  f_actualiza, ",
                            "  usuario      ",
                       "  FROM cat_rechazo_causal",
                       " WHERE 1=1"
        END IF

            LET i = 1

            CALL g_arr_causal.clear()

            PREPARE prp_factor FROM v_qry
            DECLARE cur_factor CURSOR FOR prp_factor

            FOREACH cur_factor INTO g_arr_causal[i].*
                LET i = i + 1
            END FOREACH

          {  IF g_arr_causal[i].causal IS NULL THEN
                CALL g_arr_causal.deleteElement(i)
            END IF}

            IF i > 1 THEN
                CALL f.setElementHidden("group2",0)
                CALL DIALOG.setActionHidden("reporte",0)
            ELSE
                LET g_mensaje = "No existen registros con el criterio de búsqueda"

                CALL fn_mensaje(g_titulo,g_mensaje,g_imagen)
                CALL g_arr_causal.clear()

                INITIALIZE g_rec_causal.* TO NULL
           --   LET v_pos = NULL
           --   CALL f.setElementHidden("group2",1)
            END IF
            
    ON ACTION reporte 

       IF v_entidad IS NOT NULL THEN 
       
            LET v_qry = "SELECT causal,      ",
                             "  desc_causal, ",
                             "  entidad,     ",
                             "  f_actualiza, ",
                             "  usuario      ",
                        "  FROM cat_rechazo_causal",
                        " WHERE entidad = '", v_entidad CLIPPED,"'"

               DISPLAY "condicion ", v_entidad
           ELSE 
           LET v_qry = "SELECT causal,      ",
                            "  desc_causal, ",
                            "  entidad,     ",
                            "  f_actualiza, ",
                            "  usuario      ",
                       "  FROM cat_rechazo_causal",
                       " WHERE 1=1"
        END IF

        DISPLAY "condicion ", v_entidad

           CASE v_entidad 
               WHEN 'PRC' 
                 LET v_titulo = 'Rechazos PROCESAR'
                 LET v_visibilidad = 1
               WHEN 'AFO' 
                 LET v_titulo = 'Rechazos AFORE'
                 LET v_visibilidad = 1
               WHEN 'INF'
                 LET v_titulo = 'Rechazos INFONAVIT'
                 LET v_visibilidad = 1
               OTHERWISE 
                 LET v_visibilidad = 0
                 
           END CASE   

            LET i = 1

            CALL g_arr_causal.clear()

            PREPARE prp_report FROM v_qry
            DECLARE cur_report CURSOR FOR prp_report

            CALL configura_salida_reporte()

    ON ACTION CANCEL
        CALL g_arr_causal.clear()
        INITIALIZE g_rec_causal.* TO NULL
      --  LET v_pos = NULL
      --  DISPLAY BY NAME g_rec_causal.*
        CALL f.setElementHidden("group2",1)
        EXIT DIALOG

    END DIALOG

END FUNCTION

FUNCTION configura_salida_reporte()

   # -----> CONFIGURACIÓN DEL REPORTE PDF <-----

      LET v_reporte_bin = v_ruta_bin CLIPPED,"/CATM1611.4rp"
      LET v_nombre_reporte = g_usuario CLIPPED,"-CATM1611"
      LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",g_usuario CLIPPED,"-CATM1611",".pdf"

    --LET v_reporte_bin = "CATM1611.4rp"
    
   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF") -- formato de salida del reporte
      CALL fgl_report_selectPreview(0) -- visualizable
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings() -- manda los cambios de la configuración a la plantilla

      IF (v_manejador_rpt IS NOT NULL) THEN
         
         START REPORT genera_PDF TO XML HANDLER v_manejador_rpt -- se le envia la salida al manejador del reporte

            FOREACH cur_report INTO g_rec_causal.*
               OUTPUT TO REPORT genera_PDF(
                                  g_rec_causal.*,
                                  v_titulo)
            END FOREACH

         FINISH REPORT genera_PDF 

               IF(LENGTH(v_ruta_lst) > 0)THEN -- se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(v_ruta_rpt CLIPPED) RETURNING v_existe_archivo
               END IF
DISPLAY "archivo", v_existe_archivo

               -- si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f.setElementHidden("lbl_ruta_reporte",1)
               ELSE
                  CALL f.setElementHidden("lbl_ruta_reporte",0)
               END IF

               -- muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nombre_reporte CLIPPED||".pdf"||"','cat')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte

                 CALL ui.Interface.refresh()
                 CALL fn_mensaje("Aviso","Se ha generado el reporte por archivo","info") 

      END IF
   ELSE CALL fn_mensaje('error','el reporte no pudo generarse',g_imagen)
   END IF 

END FUNCTION 

REPORT genera_PDF(p_causal,
                  p_titulo)

   DEFINE p_causal RECORD
          causal      char(3),
          descripcion char(200),
          entidad     char(3),
          f_actualiza DATE,
          usuario     char(20)
      END RECORD 

   DEFINE p_titulo CHAR(30) 

   FORMAT
   
      FIRST PAGE HEADER 
      
         # ENCABEZADO
         PRINTX g_usuario
         PRINTX g_hoy USING "dd/mm/yyyy"
         PRINTX p_titulo 
         PRINTX v_visibilidad

         DISPLAY "variable visibilidad",v_visibilidad

      ON EVERY ROW 
         PRINTX  p_causal.* 
         
END REPORT 
