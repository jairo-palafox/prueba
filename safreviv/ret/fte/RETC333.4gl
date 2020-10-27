--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10 Octubre, 2012
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC333                                                                #
#Objetivo     => Consulta de Solicitudes de Retiros del Archivo CANINT                  #
#Fecha inicio => Febrero 19, 2015                                                       # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     -- forma como ejecutara el programa
       p_s_titulo       STRING                        -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida 
   CALL fn_consulta_solicitudes(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC333
Nombre: fn_consulta_solicitudes
Fecha creacion: Febrero 19, 2015
Registro de modificaciones:
Descrip: CONSULTA SOLICITUDES DE RETIRO DEL ARCHIVO CANINT
==============================================================================
}
FUNCTION fn_consulta_solicitudes(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio          DECIMAL(9,0), -- folio
       v_nss            CHAR(11), 
       v_reg_ini        SMALLINT,
       v_reg_fin        SMALLINT,
       v_min_consec_det INTEGER,
       v_f_inicial      LIKE ret_cza_transferencia.f_carga,
       v_f_final        LIKE ret_cza_transferencia.f_carga,
       v_cbx_folios     ui.ComboBox, -- combo de folios
       v_cbx_afores     ui.ComboBox, -- combo de afores
       v_s_cadena       STRING, -- cadena de texto
       v_c_ruta_env     LIKE seg_modulo.ruta_envio,
       v_extension_txt  STRING, -- extension del archivo de salida
       v_nom_archivo    STRING, -- nombre del archivo de salida
       v_archivo_txt    STRING, -- nombre y extension del archivo con el detalle
       v_v_ruta_nomarch STRING, -- ruta y nombre del archivo de salida
       v_mensaje_archivo STRING, -- mensaje de generacion de archivo
       v_ch_arch_ret_generico BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       v_conteo         INTEGER, -- contador de registros
       v_s_detalle      STRING,
       v_r_glo_folio    RECORD LIKE glo_folio.*,
       arr_reg_detalle     DYNAMIC ARRAY OF RECORD
         v_folio                LIKE glo_folio.folio,
         v_nss                  LIKE afi_derechohabiente.nss,
         v_desc_movto           CHAR(18),
         v_f_liquidacion        LIKE glo_folio.f_actualiza,
         v_tpo_movto            CHAR(2), 
         v_f_canint             LIKE glo_folio.f_actualiza,
         v_desc_tpo_movto       CHAR(30),
         v_aivs                 DECIMAL(22,6),
         v_edo_sol_canint       CHAR(50),
         v_cod_rech_canint      CHAR(50),
         v_f_solicitud          LIKE ret_disposicion.f_solicitud,
         v_folio_solicitud      LIKE glo_folio.folio,
         v_tpo_retiro           CHAR(30),
         v_aivs92_sol           DECIMAL(22,6),
         v_aivs97_sol           DECIMAL(22,6),
         v_pesos92_sol          DECIMAL(24,2),
         v_pesos97_sol          DECIMAL(24,2),
         v_estado_solicitud     CHAR(50),        
         v_cod_rechazo          CHAR(50) 
       END RECORD,
       arr_reporte              DYNAMIC ARRAY OF RECORD
         v_folio                LIKE glo_folio.folio,
         v_nss                  LIKE afi_derechohabiente.nss,
         v_desc_movto           CHAR(18),
         v_f_liquidacion        LIKE glo_folio.f_actualiza,
         v_tpo_movto            CHAR(2), 
         v_f_canint             LIKE glo_folio.f_actualiza,
         v_desc_tpo_movto       CHAR(30),
         v_aivs                 DECIMAL(22,6),
         v_edo_sol_canint       CHAR(50),
         v_cod_rech_canint      CHAR(50),
         v_f_solicitud          LIKE ret_disposicion.f_solicitud,
         v_folio_solicitud      LIKE glo_folio.folio,
         v_tpo_retiro           CHAR(30),
         v_aivs92_sol           DECIMAL(22,6),
         v_aivs97_sol           DECIMAL(22,6),
         v_pesos92_sol          DECIMAL(24,2),
         v_pesos97_sol          DECIMAL(24,2),
         v_estado_solicitud     CHAR(50),        
         v_cod_rechazo          CHAR(50) 
       END RECORD,
       arr_export              DYNAMIC ARRAY OF RECORD
         v_folio                LIKE glo_folio.folio,
         v_nss                  LIKE afi_derechohabiente.nss,
         v_desc_movto           CHAR(18),
         v_f_liquidacion        LIKE glo_folio.f_actualiza,
         v_tpo_movto            CHAR(2), 
         v_f_canint             LIKE glo_folio.f_actualiza,
         v_desc_tpo_movto       CHAR(30),
         v_aivs                 DECIMAL(22,6),
         v_edo_sol_canint       CHAR(50),
         v_cod_rech_canint      CHAR(50),
         v_f_solicitud          LIKE ret_disposicion.f_solicitud,
         v_folio_solicitud      LIKE glo_folio.folio,
         v_tpo_retiro           CHAR(30),
         v_aivs92_sol           DECIMAL(22,6),
         v_aivs97_sol           DECIMAL(22,6),
         v_pesos92_sol          DECIMAL(24,2),
         v_pesos97_sol          DECIMAL(24,2),
         v_estado_solicitud     CHAR(50),        
         v_cod_rechazo          CHAR(50) 
       END RECORD,
       v_query                       STRING, -- detalle
       v_id_derechohabiente          LIKE afi_derechohabiente.id_derechohabiente,
       v_indice                      SMALLINT, -- indice de arreglo
       v_ruta_reporte                STRING ,-- ruta del archivo del reporte       
       v_ruta_listados               STRING ,-- ruta de los listados
       v_ruta_ejecutable             STRING ,-- ruta del ejecutable
       manejador_rpt                 om.SaxDocumentHandler ,
       v_indice_reporte              SMALLINT,
       v_total_reg_folio             INTEGER,   -- registros del folio
       v_id_solicitud                LIKE ret_disposicion.id_solicitud
 
       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC3331"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta Solicitudes Archivo CANINT")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_afores = ui.ComboBox.forName("formonly.cmb_afore")   
  
   -- se inician los combobox en blanco
   CALL v_cbx_folios.clear()

   INPUT v_folio, v_nss, v_reg_ini, v_reg_fin 
      FROM cmb_folio, e_nss, ed_inicial, ed_final    
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_folio     = NULL    
         LET v_nss       = NULL
         LET v_reg_ini   = 1
         LET v_reg_fin   = 2500    
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a
         WHERE  a.proceso_cod = g_proceso_canint
         AND    a.status <> -1
         ORDER BY folio DESC

         FOREACH cur_folios INTO v_r_glo_folio.*
           -- LET v_s_cadena = v_r_glo_folio.folio
            SELECT total_registros
            INTO   v_total_reg_folio
            FROM   ret_cza_canint
            WHERE  folio = v_r_glo_folio.folio;
            LET v_s_cadena = v_r_glo_folio.folio, "-", v_total_reg_folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
        
      ON ACTION ACCEPT

         -- se borran los arreglos de despliegue
         CALL arr_reg_detalle.clear()
         CALL arr_reporte.clear()

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF ( (v_folio     IS NULL OR v_folio <= 0) AND 
              (v_nss       IS NULL OR v_nss   <= 0) ) THEN

            CALL fn_mensaje("Consulta","La consulta se hará buscando todos los folios existentes","about")
         END IF 
         IF v_reg_ini > v_reg_fin THEN 
             CALL fn_mensaje("Consulta","El registro inicial no puede ser mayo que el final","about")
             LET v_reg_ini = 1
             LET v_reg_fin = 2500
         END IF 
         IF v_reg_fin - v_reg_ini > 2500 THEN 
             CALL fn_mensaje("Consulta","La diferencia entre el registro inicial y el final no debe ser mayor a 2500","about")
             LET v_reg_ini = 1
             LET v_reg_fin = 2500
         END IF 

         --- Se obtiene el minimo del folio para realizar la consulta
         LET v_min_consec_det = 0;
         IF v_folio IS NOT NULL AND v_folio > 0 THEN 
             SELECT MIN(consec_detalle)
             INTO   v_min_consec_det
             FROM   ret_det_canint
             WHERE  folio = v_folio;
         ELSE 
             SELECT MIN(consec_detalle)
             INTO   v_min_consec_det
             FROM   ret_det_canint;
         END IF 
         LET v_query = "\n SELECT distinct canint.folio                       , ",
                           "\n        canint.nss                              , ",
                           "\n        canint.desc_movto                       , ",
                           "\n        canint.f_liquidacion                    , ",
                           "\n        canint.tpo_movto                        , ",
                           "\n        czacanint.f_operacion                   , ",
                           "\n        ccanint.desc_tpo_movto                  , ",
                           "\n        canint.aivs                             , ",
                           "\n        est.des_corta                           , ",
                           "\n        rch.des_larga                           , ",
                           "\n        sol.f_solicitud                         , ",
                           "\n        sol.folio_solicitud                     , ",
                           "\n        sol.tipo_retiro                         , ",
                           "\n        sol.aivs92                              , ",
                           "\n        sol.aivs97                              , ",
                           "\n        sol.aivs92 * nvl(gvf.precio_fondo, 0)   , ",
                           "\n        sol.aivs97 * nvl(gvf.precio_fondo, 0)   , ",
                           "\n        sol.estado_sol                          , ",
                           "\n        sol.cod_rech                              ",
                           "\n FROM   ret_cza_canint czacanint                , ",
                           "\n        ret_det_canint canint                     ",
                           "\n        LEFT OUTER JOIN glo_valor_fondo gvf                                      ",
                           "\n                     ON canint.f_liquidacion = gvf.f_valuacion                   ",
                           "\n                    AND gvf.fondo            = 11                                ",
                           "\n        LEFT OUTER JOIN ret_estado_solicitud est                                 ",
                           "\n                     ON canint.estado_solicitud = est.estado_solicitud           ",
                           "\n        LEFT OUTER JOIN ret_rechazo rch                                          ",
                           "\n                     ON canint.cod_rechazo      = rch.cod_rechazo                ",
                           "\n        LEFT OUTER JOIN afi_derechohabiente afi                                  ",
                           "\n                     ON canint.nss       = afi.nss                               ",
                           "\n        LEFT OUTER JOIN ret_cat_tpo_movto_canint ccanint                         ", 
                           "\n                     ON canint.tpo_movto = ccanint.tpo_movto                     ",
                           "\n        LEFT OUTER JOIN                                                          ",
                           "\n        (SELECT a.id_derechohabiente                     AS id_derechohabiente , ",
                           "\n                a.f_solicitud                            AS f_solicitud        , ",
                           "\n                a.folio                                  AS folio_solicitud    , ",
                           "\n                c.des_corta                              AS tipo_retiro        , ",
                           "\n                a.aivs_viv92                             AS aivs92             , ",
                           "\n                a.aivs_viv97                             AS aivs97             , ",
                           "\n                0                                        AS pesos92            , ",
                           "\n                0                                        AS pesos97            , ",
                           "\n                a.estado_solicitud || '-' || d.des_corta AS estado_sol         , ",
                           "\n                a.cod_rechazo || '-' || e.des_corta      AS cod_rech             ",
                           "\n         FROM   ret_disposicion a                                                ",
                           "\n                LEFT OUTER JOIN ret_matriz_derecho b                             ",
                           "\n                             ON a.id_ret_matriz_derecho = b.id_ret_matriz_derecho",
                           "\n                LEFT OUTER JOIN ret_tipo_retiro c                                ",
                           "\n                             ON b.tpo_retiro            = c.tpo_retiro           ",
                           "\n                LEFT OUTER JOIN ret_estado_solicitud d                           ",
                           "\n                             ON a.estado_solicitud      = d.estado_solicitud     ",
                           "\n                LEFT OUTER JOIN ret_rechazo e                                    ",
                           "\n                             ON a.cod_rechazo           = e.cod_rechazo          ",
                           "\n         UNION ALL                                                               ",
                           "\n         SELECT a.id_derechohabiente                     AS id_derechohabiente , ",
                           "\n                f.f_actualiza                            AS f_solicitud        , ",
                           "\n                a.folio                                  AS folio_solicitud    , ",
                           "\n                c.des_corta                              AS tipo_retiro        , ",
                           "\n                0                                        AS aivs92             , ",
                           "\n                a.aivs_viv97                             AS aivs97             , ",
                           "\n                0                                        AS pesos92            , ",
                           "\n                0                                        AS pesos97            , ",
                           "\n                a.estado_solicitud || '-' || d.des_corta AS estado_sol         , ",
                           "\n                a.cod_rechazo || '-' || e.des_corta      AS cod_rech             ",
                           "\n         FROM   ret_transferencia a                                              ",
                           "\n                LEFT OUTER JOIN ret_matriz_derecho b                             ",
                           "\n                             ON a.id_ret_matriz_derecho = b.id_ret_matriz_derecho",
                           "\n                LEFT OUTER JOIN ret_tipo_retiro c                                ",
                           "\n                             ON b.tpo_retiro            = c.tpo_retiro           ",
                           "\n                LEFT OUTER JOIN ret_estado_solicitud d                           ",
                           "\n                             ON a.estado_solicitud      = d.estado_solicitud     ",
                           "\n                LEFT OUTER JOIN ret_rechazo e                                    ",
                           "\n                             ON a.cod_rechazo           = e.cod_rechazo          ",
                           "\n                LEFT OUTER JOIN glo_folio f                                      ",
                           "\n                             ON a.folio                 = f.folio ) as sol       ",
                           "\n                     ON afi.id_derechohabiente = sol.id_derechohabiente          ",
                           "\n WHERE  czacanint.folio      = canint.folio                                      "


                    -- si se recibio nss 
                     IF ( v_nss IS NOT NULL ) THEN 
                        LET v_query = v_query, "\n AND    canint.nss = '", v_nss, "'"
                     END IF
                     IF (v_folio IS NOT NULL ) THEN 
                         LET v_query = v_query, "\n AND    canint.folio = ", v_folio,
                           "\n AND    canint.consec_detalle BETWEEN ", v_min_consec_det + (v_reg_ini - 1), 
                           "\n                                  and ", v_min_consec_det + (v_reg_fin - 1)
                     ELSE 
                         IF v_nss IS NULL THEN 
                             LET v_query = v_query, "\n AND    canint.consec_detalle BETWEEN ", v_min_consec_det + (v_reg_ini - 1), 
                                                    "\n                                  and ", v_min_consec_det + (v_reg_fin - 1)
                         END IF 
                     END IF 
                     LET v_query = v_query, "\n ORDER BY 1,2,10 "
                     DISPLAY "El Query \n",v_query; 
                     PREPARE sid_detalle FROM v_query
                     DECLARE cur_detalle  CURSOR FOR sid_detalle
                     
                     
                     --llena el arreglo        
                     LET v_indice = 1
                     
                     FOREACH cur_detalle INTO 
                         arr_reg_detalle[v_indice].v_folio                ,
                         arr_reg_detalle[v_indice].v_nss                  ,
                         arr_reg_detalle[v_indice].v_desc_movto           ,
                         arr_reg_detalle[v_indice].v_f_liquidacion        ,
                         arr_reg_detalle[v_indice].v_tpo_movto            ,
                         arr_reg_detalle[v_indice].v_f_canint             ,
                         arr_reg_detalle[v_indice].v_desc_tpo_movto       ,
                         arr_reg_detalle[v_indice].v_aivs                 ,
                         arr_reg_detalle[v_indice].v_edo_sol_canint       ,
                         arr_reg_detalle[v_indice].v_cod_rech_canint      ,
                         arr_reg_detalle[v_indice].v_f_solicitud          ,
                         arr_reg_detalle[v_indice].v_folio_solicitud      ,
                         arr_reg_detalle[v_indice].v_tpo_retiro           ,
                         arr_reg_detalle[v_indice].v_aivs92_sol           ,
                         arr_reg_detalle[v_indice].v_aivs97_sol           ,
                         arr_reg_detalle[v_indice].v_pesos92_sol          ,
                         arr_reg_detalle[v_indice].v_pesos97_sol          ,
                         arr_reg_detalle[v_indice].v_estado_solicitud     ,
                         arr_reg_detalle[v_indice].v_cod_rechazo
                         
                        LET v_indice = v_indice + 1
                     END FOREACH
                     
                     -- se borra el ultimo registro
                     CALL arr_reg_detalle.deleteElement(arr_reg_detalle.getLength())
         
            
         -- se abre la ventana de resultados
         OPEN WINDOW w_detalle_consulta_saldos WITH FORM "RETC3332"
            
         -- se abre un dialog para realizar el despliegue de los resultados
        DIALOG
            ATTRIBUTES ( UNBUFFERED )
            
               
               DISPLAY ARRAY arr_reg_detalle TO r_det_canint.*
               END DISPLAY
               
               ON ACTION regresar
                  EXIT DIALOG

               ON ACTION Exportar
                   -- se obtiene la ruta de envio y ejecutable
                   SELECT ruta_envio
                   INTO   v_c_ruta_env 
                   FROM   seg_modulo
                   WHERE  modulo_cod = "ret"

                   -- las extensiones del archivo son TXT para el detalle
                   LET v_extension_txt = ".txt"

                   -- los nombres son todo en mayusculas con la siguiente mascara
                   -- SG_USUARIO_AAAAMMDD.TXT
                   LET v_nom_archivo = "CANINT_", p_usuario_cod CLIPPED, "_", TODAY USING "yyyymmdd"
                   LET v_archivo_txt = v_nom_archivo, v_extension_txt
                   
                   -- el archivo con ruta destino que contiene el detalle
                   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt
                   LET v_mensaje_archivo = "Se generara el archivo:", v_v_ruta_nomarch
                   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
                   -- nombre de archivo generado
                   DISPLAY "~~~~~~~~~~~"
                   DISPLAY "Archivo generado: ", v_v_ruta_nomarch

                   -- se crea el manejador de archivo
                   LET v_ch_arch_ret_generico = base.Channel.create()
                   CALL v_ch_arch_ret_generico.setDelimiter(NULL)
                   
                   -- se crea archivo y se indica que se escribira en el mismo
                   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
                   -- escribe el encabezado
                   --LET v_s_detalle = TODAY USING "yyyymmdd", "|CVE AFORE|NSS|NOMBRE|AIVS 92 SOLIC|AIVS 97 SOLIC|PESOS 92 SOLIC|",
                   --                  "PESOS 97 SOLIC|AIVS 92 SALDO|AIVS 97 SALDO|PESOS 92 SALDO|PESOS 97 SALDO|",
                   --                  "AIVS 92 DIF|AIVS 97 DIF|PESOS 92 DIF|PESOS 97 DIF|"
                   
                   --CALL v_ch_arch_ret_generico.write(v_s_detalle)
                   -- se inicia el contador de registros
                   LET v_conteo = 0
                  
                   -- llena el arreglo
                   LET v_indice = 1
                   FOR v_indice = 1 TO arr_reg_detalle.getLength()

                         LET v_s_detalle = arr_reg_detalle[v_indice].v_folio                                  ,"|",
                                           arr_reg_detalle[v_indice].v_nss                                    ,"|",
                                           arr_reg_detalle[v_indice].v_desc_movto                             ,"|",
                                           arr_reg_detalle[v_indice].v_f_liquidacion USING "yyyymmdd"         ,"|",
                                           arr_reg_detalle[v_indice].v_tpo_movto                              ,"|",
                                           arr_reg_detalle[v_indice].v_f_canint      USING "yyyymmdd"         ,"|",
                                           arr_reg_detalle[v_indice].v_desc_tpo_movto                         ,"|",
                                           arr_reg_detalle[v_indice].v_aivs          USING "&&&&&&&&&.&&&&&&" ,"|",
                                           arr_reg_detalle[v_indice].v_edo_sol_canint                         ,"|",
                                           arr_reg_detalle[v_indice].v_cod_rech_canint                        ,"|",
                                           arr_reg_detalle[v_indice].v_f_solicitud   USING "yyyymmdd"         ,"|",
                                           arr_reg_detalle[v_indice].v_folio_solicitud                        ,"|",
                                           arr_reg_detalle[v_indice].v_tpo_retiro                             ,"|",
                                           arr_reg_detalle[v_indice].v_aivs92_sol    USING "&&&&&&&&&.&&&&&&" ,"|",
                                           arr_reg_detalle[v_indice].v_aivs97_sol    USING "&&&&&&&&&.&&&&&&" ,"|",
                                           arr_reg_detalle[v_indice].v_pesos92_sol   USING "&&&&&&&&&.&&"     ,"|",
                                           arr_reg_detalle[v_indice].v_pesos97_sol   USING "&&&&&&&&&.&&"     ,"|",
                                           arr_reg_detalle[v_indice].v_estado_solicitud                       ,"|",
                                           arr_reg_detalle[v_indice].v_cod_rechazo                            ,"|"          
                         
                                         
                         CALL v_ch_arch_ret_generico.write(v_s_detalle)
                   END FOR 
                   -- escribe el sumario
                   --LET v_s_detalle = TODAY USING "yyyymmdd", "|",(v_indice - 1) USING "&&&&&&&&&&" ,"|"
                   --CALL v_ch_arch_ret_generico.write(v_s_detalle)

                   -- se cierra el archivo
                   CALL v_ch_arch_ret_generico.close()
                   LET v_mensaje_archivo = "Archivo generado exitosamente:", v_v_ruta_nomarch
                   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
                 
               ON ACTION reporte                      

                  -- Recupera la ruta de listados en el que se enviara el archivo
                  CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
    
                  -- Se asigna la plantilla para generar el reporte
                  IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETC333.4rp") ) THEN 
                     CALL fgl_report_selectDevice ("PDF")
                       
                     LET v_ruta_reporte = v_ruta_ejecutable CLIPPED,"/","detalle_sol_canint"                
                     CALL fgl_report_setOutputFileName(v_ruta_reporte)
                     CALL fgl_report_selectPreview(1)
                     LET manejador_rpt = fgl_report_commitCurrentSettings()
                  ELSE         
                     CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla RETC333.4rp", "stop")
                     CONTINUE DIALOG
                  END IF   

                  --Inicia el reporte de registros con rechazo
                  START REPORT rpt_detalle_sol_canint TO XML HANDLER manejador_rpt
                  
                  FOR v_indice_reporte = 1 TO arr_reg_detalle.getLength()
                     --LET v_agrupador_folio_fecha_Afore = arr_reporte[v_indice_reporte].v_afore, arr_reporte[v_indice_reporte].v_folio USING "&&&&&&&&&", arr_reporte[v_indice_reporte].v_f_carga USING "ddmmyyyy"
                     
                     OUTPUT TO REPORT rpt_detalle_sol_canint(v_indice_reporte, arr_reg_detalle[v_indice_reporte].*, p_usuario_cod)
                  END FOR
                  
                  FINISH REPORT rpt_detalle_sol_canint

            END DIALOG
            
            CLOSE WINDOW w_detalle_consulta_saldos
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_saldos

END FUNCTION

{ ======================================================================
Clave: 
Nombre: rpt_detalle_sol_canint
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de los saldos insuficientes 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_detalle_sol_canint(p_indice, v_r_despliegue, p_usuario_cod) 
DEFINE   v_r_despliegue          RECORD -- registro de consulta
              v_folio                LIKE glo_folio.folio,
              v_nss                  LIKE afi_derechohabiente.nss,
              v_desc_movto           CHAR(18),
              v_f_liquidacion        LIKE glo_folio.f_actualiza,
              v_tpo_movto            CHAR(2), 
              v_f_canint             LIKE glo_folio.f_actualiza,
              v_desc_tpo_movto       CHAR(30),
              v_aivs                 DECIMAL(22,6),
              v_edo_sol_canint       CHAR(50),
              v_cod_rech_canint      CHAR(50),
              v_f_solicitud          LIKE ret_disposicion.f_solicitud,
              v_folio_solicitud      LIKE glo_folio.folio,
              v_tpo_retiro           CHAR(30),
              v_aivs92_sol           DECIMAL(22,6),
              v_aivs97_sol           DECIMAL(22,6),
              v_pesos92_sol          DECIMAL(24,2),
              v_pesos97_sol          DECIMAL(24,2),
              v_estado_solicitud     CHAR(50),        
              v_cod_rechazo          CHAR(50) 
         END RECORD,
          p_indice                DECIMAL(9,0),
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100)
          
FORMAT

   FIRST PAGE HEADER
      
      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod

      LET v_nombre_usuario = v_nombre_usuario CLIPPED
      
      PRINTX p_usuario_cod, v_fecha, v_nombre_usuario
      
 
   ON EVERY ROW
      PRINTX v_r_despliegue.*
--      DISPLAY v_r_despliegue.*

END REPORT
