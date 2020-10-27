--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC274                                                                #
#OBJETIVO     => Consulta de marcas tramite judicial                                    #
#Fecha inicio => 08 Julio 2014                                                          #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
{
======================================================================
Clave: 
Nombre: main
Fecha creacion: 08 de julio de 2014
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Consulta las cuentas con marca de tramite judicial en un periodo


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Jose Luis       25/08/2015              Debido a la separacion de la marca 590 
                                        en 595, 596 y 597. Estas marcas se agregaron
                                        al reporte y a la pantalla de consulta.

======================================================================
}
MAIN 
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
       p_s_titulo          STRING,   -- titulo de la ventana
       v_c_nss               LIKE ret_solicitud_generico.nss,
       v_f_inicio          DATE,
       v_f_final           DATE,
       v_f_inicio_des      DATE,
       v_f_final_des       DATE,
       v_chk_activa        BOOLEAN,
       v_chk_historica     BOOLEAN,
       v_chk_595           BOOLEAN,
       v_chk_591           BOOLEAN,
       -- La marca 592 se dividio en 593 y 594
       --v_chk_592           BOOLEAN ,
       v_chk_593           BOOLEAN ,
       v_chk_594           BOOLEAN ,
       --Marcas nuevas requerimiento 812--
       v_chk_596           BOOLEAN,
       v_chk_597           BOOLEAN,
       v_chk_814           BOOLEAN,
       v_chk_822           BOOLEAN,
       -----------------------------------
       v_i_indice          INTEGER, 
       v_poner_coma        SMALLINT,
       v_s_qryTxt_act      STRING, -- guarda una sentencia SQL a ejecutar
       v_s_qryTxt_hist     STRING,
       v_s_qryTxt          STRING,

       v_arr_marcas    DYNAMIC ARRAY OF RECORD -- arreglo que contiene los detalles de los nss marcados
           marca_tpo_marca     CHAR(10),
           marca_marca         CHAR (50),       
           marca_nss           LIKE afi_derechohabiente.nss,
           marca_nombre        CHAR (50),
           marca_f_inicio      LIKE sfr_marca_historica.f_inicio,
           marca_f_final       LIKE sfr_marca_historica.f_fin,
           marca_referencia    LIKE sfr_marca_activa.n_referencia,
           marca_usuario       LIKE sfr_marca_historica.usuario_marca,
           marca_usuario_desc  LIKE seg_usuario.usuario_desc
       END RECORD,


        f_ventana        ui.Window,   -- Define las propìedades de la Ventana
        f_forma          ui.Form     -- Define las propiedades de la forma
       
        DEFINE v_reporte         STRING
        DEFINE report_handler    om.SaxDocumentHandler -- Librería de Genero para la generación de reportes
        DEFINE v_excepcion       SMALLINT
        DEFINE i                 SMALLINT
       
   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETC274.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   -- se abre la ventana de consulta
   OPEN WINDOW w_parametros WITH FORM "RETC2741"
      -- se capturan los datos de la consulta
        DIALOG ATTRIBUTE(UNBUFFERED)
            INPUT v_f_inicio,
                  v_f_final,
                  v_f_inicio_des,
                  v_f_final_des,
                  v_chk_activa,
                  v_chk_historica,
                  v_c_nss,
                  v_chk_591,
                  v_chk_593,
                  v_chk_594,
                  v_chk_595,
                  v_chk_596,
                  v_chk_597,
                  v_chk_814,
                  v_chk_822
            FROM v_fecha_inicio,
                 v_fecha_fin,
                 v_f_inicio_desmarca,
                 v_f_fin_desmarca,
                 Chk_Activa,
                 Chk_Historica,
                 v_nss,
                 Chk_591,
                 Chk_593,
                 Chk_594,
                 Chk_595,
                 Chk_596,
                 Chk_597,
                 chk_814,
                 chk_822
               
            BEFORE INPUT   
                LET f_ventana = ui.Window.getCurrent()
                LET f_forma = f_ventana.getForm()
                CALL f_forma.setElementHidden("Grp_Detalle", 1) --Oculta la Sección de Detalles
                CALL f_forma.setElementHidden("Grid3", 1) --Oculta la Sección de Detalles
                CALL f_forma.setElementHidden("tbl_det_marcas", 1) --Oculta la Sección de Detalles
              
            END INPUT    


          
            -- Botón cancel que da salida a la consulta y terminar la captura de los parámetros
            ON ACTION cancelar
                EXIT DIALOG 
            -- Botón aceptar que realiza la consulta en base a folio y fecha
                
            ON ACTION ACCEPT  
                --DISPLAY "Por validar las fechas"
                IF (( v_c_nss IS NULL AND v_f_inicio IS NULL AND v_f_final IS NULL AND v_f_inicio_des IS NULL AND v_f_final_des IS NULL AND NOT v_chk_activa AND NOT v_chk_historica AND NOT v_chk_595 AND NOT v_chk_591 AND NOT v_chk_593 AND NOT v_chk_594 AND NOT v_chk_596 AND NOT v_chk_597) OR 
                   (NOT v_chk_activa AND NOT v_chk_historica) OR 
                   (NOT v_chk_595 AND NOT v_chk_591 AND NOT v_chk_593 AND NOT v_chk_594 AND 
                    NOT v_chk_596 AND NOT v_chk_597 AND NOT v_chk_814 AND NOT v_chk_822) OR
                   (v_chk_historica AND v_c_nss IS NULL AND v_f_inicio IS NULL AND v_f_final IS NULL AND v_f_inicio_des IS NULL AND v_f_final_des IS NULL) OR 
                   (v_chk_activa AND v_c_nss IS NULL AND v_f_inicio IS NULL AND v_f_final IS NULL )) THEN
                   IF ( v_c_nss IS NULL AND v_f_inicio IS NULL AND v_f_final IS NULL AND v_f_inicio_des IS NULL AND v_f_final_des IS NULL AND
                       NOT v_chk_activa AND NOT v_chk_historica AND 
                       NOT v_chk_595 AND NOT v_chk_591 AND NOT v_chk_593 AND NOT v_chk_594 AND 
                       NOT v_chk_596 AND NOT v_chk_597 AND NOT v_chk_814 AND NOT v_chk_822) THEN 
                       CALL fn_mensaje("Atención","Debe incluir al menos un criterio de busqueda","stop")
                       CONTINUE DIALOG  
                   ELSE 
                       IF (NOT v_chk_activa AND NOT v_chk_historica) THEN 
                            CALL fn_mensaje("Atención","Debe seleccionar un tipo de marca","stop")
                            CONTINUE DIALOG 
                        END IF 
                        IF (NOT v_chk_595 AND NOT v_chk_591 AND NOT v_chk_593 AND NOT v_chk_594 AND 
                            NOT v_chk_596 AND NOT v_chk_597 AND NOT v_chk_814 AND NOT v_chk_822) THEN 
                            CALL fn_mensaje("Atención","Debe seleccionar al menos una marca","stop")
                            CONTINUE DIALOG   
                        END IF 
                        IF (v_chk_historica AND v_c_nss IS NULL AND v_f_inicio IS NULL AND v_f_final IS NULL AND v_f_inicio_des IS NULL AND v_f_final_des IS NULL) THEN 
                            CALL fn_mensaje("Atención","Si desea consultar el histórico de marcas debe incluir los periodos de busqueda","stop")
                            CONTINUE DIALOG   
                        END IF 
                        IF (v_chk_activa AND v_c_nss IS NULL AND v_f_inicio IS NULL AND v_f_final IS NULL ) THEN 
                            CALL fn_mensaje("Atención","Si desea consultar las marcas activas debe indicar el periodo de busqueda","stop")
                            CONTINUE DIALOG   
                        END IF 
                   END IF 
                ELSE 
                    DISPLAY "Se buscara informacion con los criterios proporcionados"
     
                    -- se inicializa el indice del arreglo
                    --CALL f_forma.setElementHidden("Grp_detalle_archivo", 0) --Se muestra la Sección de Detalles
                    CALL f_forma.setElementHidden("tbl_det_marcas", 0) --Se muestra la Sección de Detalles
                    --DISPLAY ">>Se acepto el folio, inicia la consulta de la informacion para el lote " || v_folio_lote || "<<"
                    LET v_i_indice = 1

                    -- se crea la sentencia que busca los archivos disponibles por integrar

                    LET v_s_qryTxt_act = ""
                    LET v_s_qryTxt_hist = ""
                    LET v_s_qryTxt = ""
                    LET v_s_qryTxt_act = "SELECT 'ACTIVA' AS tpo_marca, sm.marca || ' ' || TRIM(sm.descripcion_marca) AS desc_marca, ",
                                     "       ad.nss, TRIM(ad.nombre_af) || ' ' || TRIM(ad.ap_paterno_af) || ' ' || TRIM(ad.ap_materno_af) AS nombre, ",
                                     "       sma.f_inicio, sma.f_inicio AS f_fin, sma.n_referencia, sma.usuario_marca AS usuario, su.usuario_desc as usr_name ",
                                     "  FROM sfr_marca_activa AS sma INNER JOIN sfr_marca AS sm ",
                                     "                                       ON sma.marca = sm.marca ",
                                     "                               INNER JOIN afi_derechohabiente AS ad ",
                                     "                                       ON sma.id_derechohabiente = ad.id_derechohabiente ",
                                     "                               LEFT OUTER JOIN seg_usuario AS su ",
                                     "                                            ON upper(sma.usuario_marca) = su.usuario_cod ",
                                     " where 1 = 1 "
                    IF v_c_nss IS NOT NULL THEN 
                        LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " AND ad.nss = '" || v_c_nss || "'"
                    END IF 
                    IF v_f_inicio IS NOT NULL AND v_f_final IS NOT NULL THEN 
                        LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " AND sma.f_inicio between '" || v_f_inicio || "' and '" || v_f_final || "'" 
                    END IF 
                    IF v_chk_595 OR v_chk_591 OR v_chk_593 OR v_chk_594 OR v_chk_596 OR v_chk_597 OR v_chk_814 OR v_chk_822 THEN
                        LET v_poner_coma = 0 
                        LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " AND sma.marca in ( "
                        IF v_chk_595 THEN 
                            LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " 595 "
                            LET v_poner_coma = 1
                        END IF 
                        IF v_chk_591 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " 591 "
                            LET v_poner_coma = 1
                        END IF 
                        IF v_chk_593 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " 593 "
                            LET v_poner_coma = 1
                        END IF
                        IF v_chk_594 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " 594 "
                            LET v_poner_coma = 1
                        END IF
                        IF v_chk_596 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " 596 "
                            LET v_poner_coma = 1
                        END IF
                        IF v_chk_597 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " 597 "
                        END IF
                        IF v_chk_814 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " 814 "
                        END IF
                        IF v_chk_822 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " 822 "
                        END IF
                        LET v_s_qryTxt_act = v_s_qryTxt_act.trim() || " ) "
                    END IF
                
                    LET v_s_qryTxt_hist = "SELECT 'HISTORICO' AS tpo_marca, sm.marca || ' ' || TRIM(sm.descripcion_marca) AS desc_marca, ",
                                     "       ad.nss, TRIM(ad.nombre_af) || ' ' || TRIM(ad.ap_paterno_af) || ' ' || TRIM(ad.ap_materno_af) AS nombre,  ",
                                     "       smh.f_inicio, smh.f_fin, smh.n_referencia, nvl(smh.usuario_desmarca,smh.usuario_marca) AS usuario, nvl(sud.usuario_desc,su.usuario_desc) as usr_name ",
                                     "  FROM sfr_marca_historica AS smh INNER JOIN sfr_marca AS sm  ",
                                     "                                          ON smh.marca = sm.marca ",
                                     "                                  INNER JOIN afi_derechohabiente AS ad ",
                                     "                                          ON smh.id_derechohabiente = ad.id_derechohabiente",
                                     "                                  LEFT OUTER JOIN seg_usuario AS su ",
                                     "                                               ON upper(smh.usuario_marca) = su.usuario_cod ",
                                     "                                  LEFT OUTER JOIN seg_usuario AS sud ",
                                     "                                               ON upper(smh.usuario_desmarca) = sud.usuario_cod ",
                                     " where 1 = 1 "
                    IF v_c_nss IS NOT NULL THEN 
                        LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " AND ad.nss = '" || v_c_nss || "'"
                    END IF 
                    IF v_f_inicio IS NOT NULL AND v_f_final IS NOT NULL THEN 
                        LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " AND smh.f_inicio between '" || v_f_inicio || "' and '" || v_f_final || "' " 
                    END IF 
                    IF v_f_inicio_des IS NOT NULL AND v_f_final_des IS NOT NULL THEN 
                        LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " AND smh.f_fin between '" || v_f_inicio_des || "' and '" || v_f_final_des || "' "
                    END IF
                    LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " AND smh.f_fin IS NOT NULL " 
                    
                    IF v_chk_595 OR v_chk_591 OR v_chk_593 OR v_chk_594 OR v_chk_596 OR v_chk_597 OR v_chk_814 OR v_chk_822 THEN
                        LET v_poner_coma = 0 
                        LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " AND smh.marca in ( "
                        IF v_chk_595 THEN 
                            LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " 595 "
                            LET v_poner_coma = 1
                        END IF 
                        IF v_chk_591 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " 591 "
                            LET v_poner_coma = 1
                        END IF 
                        IF v_chk_593 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " 593 "
                            LET v_poner_coma = 1
                        END IF
                        IF v_chk_594 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " 594 "
                            LET v_poner_coma = 1
                        END IF
                        IF v_chk_596 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " 596 "
                            LET v_poner_coma = 1
                        END IF
                        IF v_chk_597 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " 597 "
                        END IF 
                        IF v_chk_814 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " 814 "
                        END IF 
                        IF v_chk_822 THEN 
                            IF v_poner_coma = 1 THEN 
                                LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " , "
                            END IF 
                            LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " 822 "
                        END IF 
                        LET v_s_qryTxt_hist = v_s_qryTxt_hist.trim() || " ) "
                    END IF

                    IF v_chk_activa AND v_chk_historica THEN
                        LET v_s_qryTxt = v_s_qryTxt_act.trim() || " union all " || v_s_qryTxt_hist.trim() 
                    ELSE 
                        IF v_chk_activa THEN 
                            LET v_s_qryTxt = v_s_qryTxt_act.trim()
                        ELSE 
                            LET v_s_qryTxt = v_s_qryTxt_hist.trim() 
                        END IF 
                    END IF 
                    DISPLAY "El query >" || v_s_qryTxt || "<"
                    PREPARE prp_detalle_marca FROM v_s_qryTxt
                    DECLARE cur_detalle_marca CURSOR FOR prp_detalle_marca
                    --DISPLAY v_s_qryTxt
                    FOREACH cur_detalle_marca INTO v_arr_marcas[v_i_indice].*
                        -- se incrementa el indice del arreglo
                        IF v_arr_marcas[v_i_indice].marca_tpo_marca = "ACTIVA" THEN 
                            LET v_arr_marcas[v_i_indice].marca_f_final = NULL
                        END IF 
                        LET v_i_indice = v_i_indice + 1
                    END FOREACH

                    CALL v_arr_marcas.deleteElement(v_i_indice)
                    IF v_i_indice = 1 THEN
                        DISPLAY "No existen registros con estos criterios de búsqueda"
                        CALL fn_mensaje(p_s_titulo,"No existen registros con estos criterios de búsqueda","about")
                        CALL f_forma.setElementHidden("tbl_det_marcas", 1) --Se oculta la Sección de Detalles por no existir informacion
                    END IF 

                    -- se limpia el arreglo que contiene los archivos a integrar
                    --CALL v_arr_archivos.clear()
                    --CALL v_arr_aceptados.clear()
                    --CALL v_arr_rechazados.clear()
                    --CONTINUE DIALOG
            END IF
            DISPLAY ARRAY v_arr_marcas TO tbl_det_marcas.* 
            ON ACTION reporte
              LET v_reporte = "RETC274.4rp"

              IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- Verifica que el archivo se hay cargado correctamente y exista dentro de la ruta especificada.
                CALL fgl_report_selectPreview(1) -- Despliega el reporte directamente en pantalla.
                LET report_handler = fgl_report_commitCurrentSettings() -- Se anexa a la nomenclatura XML para que se apliquen los cambios realizados.
              ELSE
                DISPLAY "[ SAFRE EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: "|| v_reporte
                LET v_excepcion = 1
              END IF

              IF NOT v_excepcion THEN
                DISPLAY "a_datos_solicitud.getLength(): ",v_arr_marcas.getLength()
                START REPORT rpt_cifras_control TO XML HANDLER report_handler
                FOR i = 1 TO v_arr_marcas.getLength() 
                  DISPLAY i 
                  OUTPUT TO REPORT rpt_cifras_control(v_arr_marcas[i].*, 
                                                      p_usuario_cod,
                                                      v_f_inicio,
                                                      v_f_final,
                                                      v_f_inicio_des,
                                                      v_f_final_des,
                                                      v_chk_activa,
                                                      v_chk_historica,
                                                      v_c_nss,
                                                      v_chk_591,
                                                      v_chk_593,
                                                      v_chk_594,
                                                      v_chk_595,
                                                      v_chk_596,
                                                      v_chk_597,
                                                      v_chk_814,
                                                      v_chk_822)
                END FOR
                FINISH REPORT rpt_cifras_control
              END IF



            END DISPLAY
            -- Se borra el arreglo y la informacion en pantall
            CALL v_arr_marcas.clear()
            DISPLAY ARRAY v_arr_marcas TO tbl_det_marcas.*
                BEFORE DISPLAY
                    EXIT DISPLAY
            END DISPLAY
    END DIALOG 
END MAIN

REPORT rpt_cifras_control(v_arr_marcas_r, 
                          p_usuario,
                          v_f_ini,
                          v_f_fin,
                          v_f_ini_des,
                          v_f_fin_des,
                          v_chk_activa,
                          v_chk_historica,
                          v_nss,
                          v_chk_591,
                          v_chk_593,
                          v_chk_594,
                          v_chk_595,
                          v_chk_596,
                          v_chk_597,
                          v_chk_814,
                          v_chk_822)
       DEFINE v_arr_marcas_r    RECORD -- arreglo que contiene las cifras control
           rep_tpo_marca     CHAR(10),
           rep_marca         CHAR (50),
           rep_nss           LIKE afi_derechohabiente.nss,
           rep_nombre        CHAR (50),
           rep_f_inicio      LIKE sfr_marca_historica.f_inicio,
           rep_f_final       LIKE sfr_marca_historica.f_fin,
           rep_referencia    LIKE sfr_marca_activa.n_referencia,
           rep_usuario       LIKE sfr_marca_historica.usuario_marca,
           rep_usuario_desc  LIKE seg_usuario.usuario_desc
        END RECORD
        
        DEFINE p_usuario           LIKE seg_usuario.usuario
        DEFINE v_f_ini             DATE 
        DEFINE v_f_fin             DATE
        DEFINE v_f_ini_des         DATE 
        DEFINE v_f_fin_des         DATE
        
        DEFINE v_chk_activa        BOOLEAN 
        DEFINE v_chk_historica     BOOLEAN 
        DEFINE v_nss               LIKE afi_derechohabiente.nss
        DEFINE v_chk_591           BOOLEAN
        --DEFINE v_chk_592           BOOLEAN
        DEFINE v_chk_593           BOOLEAN
        DEFINE v_chk_594           BOOLEAN
        DEFINE v_chk_595           BOOLEAN
        DEFINE v_chk_596           BOOLEAN
        DEFINE v_chk_597           BOOLEAN
        DEFINE v_chk_814           BOOLEAN
        DEFINE v_chk_822           BOOLEAN
        DEFINE v_fecha             DATE

   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY         
         PRINTX v_fecha USING "dd/mm/yyyy", p_usuario, 
                v_f_ini USING "dd/mm/yyyy", v_f_fin USING "dd/mm/yyyy", 
                v_f_ini_des USING "dd/mm/yyyy", v_f_fin_des USING "dd/mm/yyyy", 
                v_chk_activa, v_chk_historica, v_nss, v_chk_591, v_chk_593, v_chk_594, v_chk_595,
                v_chk_596, v_chk_597, v_chk_814, v_chk_822

      ON EVERY ROW
         DISPLAY "SALIDA REPORTE CIFRAS CONTROL",v_arr_marcas_r.*
         PRINTX v_arr_marcas_r.rep_tpo_marca,
                v_arr_marcas_r.rep_marca,
                v_arr_marcas_r.rep_nss,
                v_arr_marcas_r.rep_nombre,
                v_arr_marcas_r.rep_f_inicio USING "dd/mm/yyyy",
                v_arr_marcas_r.rep_f_final USING "dd/mm/yyyy",
                v_arr_marcas_r.rep_referencia,
                v_arr_marcas_r.rep_usuario,
                v_arr_marcas_r.rep_usuario_desc
         
END REPORT
