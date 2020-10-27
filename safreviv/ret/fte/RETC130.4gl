--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10 Octubre, 2012
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC130                                                                #
#Objetivo     => Consulta sdos insuficientes en solicitudes de retiro Ley 73 Contingente#
#Fecha inicio => Febrero 27, 2013                                                       # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
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

   --Se inicia el log del programa
   CALL STARTLOG (p_usuario_cod CLIPPED||".RETC130.log")
   
   -- consulta de informacion recibida 
   CALL fn_consulta_saldos(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC130
Nombre: fn_consulta_saldos
Fecha creacion: Febrero 27, 2013
Registro de modificaciones:
Descrip: CONSULTA SALDOS INSUFICIENTES EN SOLICITUDES DE RETIRO LEY 73 CONTINGENTE
==============================================================================
}
FUNCTION fn_consulta_saldos(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio          DECIMAL(9,0), -- folio
       v_afore          INTEGER,
       v_nss            CHAR(11), 
       v_f_inicial      LIKE ret_cza_transferencia.f_carga,
       v_f_final        LIKE ret_cza_transferencia.f_carga,
       v_cbx_folios     ui.ComboBox, -- combo de folios
       v_s_cadena       STRING, -- cadena de texto
       v_r_glo_folio    RECORD LIKE glo_folio.*,
       v_r_cat_afore    RECORD 
         v_afore_cod      LIKE cat_afore.afore_cod,
         v_afore_desc     LIKE cat_afore.afore_desc
        END RECORD,
       v_r_agrupador   RECORD -- registro de despliegue del agrupador
        folio            LIKE glo_folio.folio,
        f_carga          DATE
       END RECORD,
       v_arr_agrupador  DYNAMIC ARRAY OF RECORD -- arreglo de despliegue del agrupador
        folio            LIKE glo_folio.folio,
        f_carga          DATE
       END RECORD,
       arr_reg_saldos           DYNAMIC ARRAY OF RECORD
         v_folio                  LIKE glo_folio.folio,
         v_f_carga                LIKE ret_cza_transferencia.f_carga,
         v_nss                    LIKE afi_derechohabiente.nss,
         v_nombre_trabajador      STRING, 
         v_aivs92_sol             DECIMAL(24,6),
         v_aivs97_sol             DECIMAL(24,6),
         v_aivs92_sol_pesos       DECIMAL(22,2),
         v_aivs97_sol_pesos       DECIMAL(22,2),
         v_aivs92_sdo             DECIMAL(24,6),
         v_aivs97_sdo             DECIMAL(24,6),
         v_aivsvol_sdo            DECIMAL(24,6),
         v_aivssum97vol_sdo       DECIMAL(24,6),
         v_aivs92_sdo_pesos       DECIMAL(22,2),
         v_aivs97_sdo_pesos       DECIMAL(22,2),
         v_aivsvol_sdo_pesos      DECIMAL(22,2),
         v_aivssum97vol_sdo_pesos DECIMAL(22,2),
         v_aivs92_dif             DECIMAL(24,6),
         v_aivs97_dif             DECIMAL(24,6),
         v_aivs92_dif_pesos       DECIMAL(22,2),        
         v_aivs97_dif_pesos       DECIMAL(22,2)
       END RECORD,
       arr_reporte              DYNAMIC ARRAY OF RECORD
         v_folio                  LIKE glo_folio.folio,
         v_f_carga                LIKE ret_cza_transferencia.f_carga,
         v_nss                    LIKE afi_derechohabiente.nss,
         v_nombre_trabajador      STRING, 
         v_aivs92_sol             DECIMAL(24,6),
         v_aivs97_sol             DECIMAL(24,6),
         v_aivs92_sol_pesos       DECIMAL(22,2),
         v_aivs97_sol_pesos       DECIMAL(22,2),
         v_aivs92_sdo             DECIMAL(24,6),
         v_aivs97_sdo             DECIMAL(24,6),
         v_aivsvol_sdo            DECIMAL(24,6),
         v_aivssum97vol_sdo       DECIMAL(24,6),
         v_aivs92_sdo_pesos       DECIMAL(22,2),
         v_aivs97_sdo_pesos       DECIMAL(22,2),
         v_aivsvol_sdo_pesos      DECIMAL(22,2),
         v_aivssum97vol_sdo_pesos DECIMAL(22,2),
         v_aivs92_dif             DECIMAL(24,6),
         v_aivs97_dif             DECIMAL(24,6),
         v_aivs92_dif_pesos       DECIMAL(22,2),        
         v_aivs97_dif_pesos       DECIMAL(22,2)
       END RECORD,
       v_query                       STRING, -- detalle
       v_query_agrupador             STRING, -- agrupador por folio y afore
       v_id_derechohabiente          LIKE afi_derechohabiente.id_derechohabiente,
       v_indice                      SMALLINT, -- indice de arreglo
       v_afore_cod                   LIKE cat_afore.afore_cod,
       v_afore_desc                  LIKE cat_afore.afore_desc, 
       v_nombre_trab                 LIKE ret_transferencia.nombre_afore,
       v_paterno_trab                LIKE ret_transferencia.paterno_afore,
       v_materno_trab                LIKE ret_transferencia.materno_afore,
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo,
       v_ruta_reporte                STRING ,-- ruta del archivo del reporte       
       v_ruta_listados               STRING ,-- ruta de los listados
       v_ruta_ejecutable             STRING ,-- ruta del ejecutable
       manejador_rpt                 om.SaxDocumentHandler ,
       v_indice_reporte              SMALLINT,
       v_id_solicitud                LIKE ret_disposicion.id_solicitud,
       v_agrupador_folio_fecha_Afore STRING

       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC1301"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta Saldos Insuficientes Ley 73")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
 
   -- se inician los combobox en blanco
   CALL v_cbx_folios.clear()

   INPUT v_folio, v_nss, v_f_inicial, v_f_final 
      FROM cmb_folio, e_nss, date_f_inicial, date_f_final    
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_folio     = NULL    
         LET v_afore     = NULL    
         LET v_nss       = NULL    
         LET v_f_inicial = NULL      
         LET v_f_final   = NULL   
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a
         WHERE  a.proceso_cod = g_proceso_cod_ret_Ley73_arch
         AND    a.status >= 1
         ORDER BY folio DESC

         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
                 
      ON ACTION ACCEPT

--        DISPLAY "variables capturadas"
--        DISPLAY "@..", v_folio, v_afore, v_nss, v_f_inicial, v_f_final

         -- se borran los arreglos de despliegue
         CALL v_arr_agrupador.clear()
         CALL arr_reg_saldos.clear()
         CALL arr_reporte.clear()

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF ( (v_folio     IS NULL OR v_folio <= 0) AND 
              (v_nss       IS NULL OR v_nss   <= 0) AND 
              (v_f_inicial IS NULL                ) AND
              (v_f_final IS NULL                  ) ) THEN

            CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de búsqueda","about")
            CONTINUE INPUT
         ELSE 
            IF (v_f_inicial IS NOT NULL) AND (v_f_final IS NULL)  THEN
               CALL fn_mensaje("Consulta", "Debe capturar fecha inicial y fecha final", "about")
               CONTINUE INPUT
            END IF
            IF (v_f_final IS NOT NULL) AND (v_f_inicial IS NULL) THEN
               CALL fn_mensaje("Consulta","Debe capturar fecha inicial y fecha final", "about")
               CONTINUE INPUT
            END IF 

            IF ( v_f_final > TODAY ) OR (v_f_inicial > TODAY)  THEN
               CALL fn_mensaje("Consulta", "Las fechas inicial y final deben ser iguales o menores al dia de hoy", "about")
               CONTINUE INPUT
            END IF 
            
            -- se verifica si se recibio NSS
            IF ( v_nss IS NOT NULL ) THEN
               -- se obtiene el id_derechohabiente para realizar la búsqueda
               SELECT id_derechohabiente
                 INTO v_id_derechohabiente
                 FROM afi_derechohabiente
                WHERE nss = v_nss
            
               IF ( v_id_derechohabiente IS NULL ) THEN 
                  CALL fn_mensaje("Atención", "El NSS proporcionado no existe en la base de datos", "stop")
                  CONTINUE INPUT 
               END IF 
            END IF 

             
            -- query para obtener los grupos
            LET v_query_agrupador = "\n SELECT DISTINCT b.folio                           ,",
                                    "\n        g.f_actualiza                               ",                                  
                                    "\n FROM ret_ley73 b                                  ,",                          
                                    "\n      afi_derechohabiente afi                      ,",
                                    "\n      ret_his_saldo       d                        ,",
                                    "\n      glo_folio           g                         ",
                                    "\n WHERE afi.id_derechohabiente = b.id_derechohabiente",
                                    "\n AND d.folio                  = b.folio             ",
                                    "\n AND d.id_solicitud           = b.id_solicitud      ",
                                    "\n AND g.folio                  = b.folio             "
                                    

            -- si se recibio el folio como parametro
            IF ( v_folio IS NOT NULL AND v_folio > 0 ) THEN
               -- agrupador
               LET v_query_agrupador = v_query_agrupador, "\n AND b.folio= ", v_folio                   
            END IF

            -- si se recibieron la fecha inicial y la fecha final como parametro
            IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
               -- agrupador
               LET v_query_agrupador = v_query_agrupador, "\n AND g.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
            END IF

            IF ( v_id_derechohabiente IS NOT NULL ) THEN 
               LET v_query_agrupador = v_query_agrupador, "\n AND afi.id_derechohabiente = ", v_id_derechohabiente
            END IF 


            LET v_query_agrupador = v_query_agrupador, "\n ORDER BY b.folio"

            -- consulta del agrupador
            DISPLAY "AGRUPADOR:\n", v_query_agrupador

            -- se llena el arreglo de agrupacion por folio, fecha de carga y afore
            PREPARE sid_grupoafore FROM v_query_agrupador
            DECLARE cur_grupoafore CURSOR FOR sid_grupoafore
            
            LET v_indice = 1
            
            -- se transfieren los datos al arreglo de despliegue agrupador
            FOREACH cur_grupoafore INTO v_r_agrupador.*
               LET v_arr_agrupador[v_indice].* = v_r_agrupador.*
               
               -- se incrementa el indice
               LET v_indice = v_indice + 1
            END FOREACH
            
            -- se cuentan cuantos registros hay
            IF ( v_arr_agrupador.getLength() < 1 ) THEN
               CALL fn_mensaje("Atención","No se encontraron datos con los parámetros dados","exclamation")
               CONTINUE INPUT
            END IF
            
            
            -- se abre la ventana de resultados
            OPEN WINDOW w_detalle_consulta_saldos WITH FORM "RETC1302"
            
            -- se abre un dialog para realizar el despliegue de los resultados
            DIALOG
            ATTRIBUTES ( UNBUFFERED )
            
               DISPLAY ARRAY v_arr_agrupador TO tbl_grupo_afore.*
                  BEFORE ROW
                     -- se limpia el arreglo de despligue del detalle
                     CALL arr_reg_saldos.clear()
                     
                     -- se obtiene el indice del arreglo agrupador
                     LET v_indice = ARR_CURR()
                     
                     -- se obtienee el registro
                     LET v_r_agrupador.* = v_arr_agrupador[v_indice].*

                     --Se crea tabla temporal
                     WHENEVER ERROR CONTINUE
                        DROP TABLE temp_saldo_ins
                     WHENEVER ERROR STOP
                     LET v_query =  "\n SELECT b.folio,b.id_solicitud,g.f_actualiza,a.nss,a.nombre_af,",
                                    "\n a.ap_paterno_af,a.ap_materno_af,b.aivs_viv92,0 as aivs_viv97,c.precio_fondo",
                                    "\n FROM afi_derechohabiente a,ret_ley73 b,glo_valor_fondo c,ret_his_saldo d,glo_folio g",
                                    "\n WHERE b.id_derechohabiente = a.id_derechohabiente ",
                                    "\n AND b.id_solicitud = d.id_solicitud ",              
                                    "\n AND c.f_valuacion = b.f_valuacion ",
                                    "\n AND g.folio       = b.folio ",
                                    "\n AND b.folio = ", v_r_agrupador.folio,
                                    "\n AND c.fondo = 11 ",
                                    "\n AND d.subcuenta IN (8) "
                     
                     -- si se recibio nss 
                     IF ( v_id_derechohabiente IS NOT NULL ) THEN 
                        LET v_query = v_query, "\n AND b.id_derechohabiente = ", v_id_derechohabiente
                     END IF

                     LET v_query = v_query,"\n UNION ",
                                       "\n SELECT b.folio,b.id_solicitud,g.f_actualiza,a.nss,a.nombre_af,a.ap_paterno_af,",
                                       "\n a.ap_materno_af,0,b.aivs_viv97,c.precio_fondo ",
                                       "\n FROM afi_derechohabiente a,ret_ley73 b,glo_valor_fondo c,ret_his_saldo d,glo_folio g",
                                       "\n WHERE b.id_derechohabiente = a.id_derechohabiente ",
                                       "\n AND b.id_solicitud = d.id_solicitud ",
                                       "\n AND c.f_valuacion = b.f_valuacion ",
                                       "\n AND g.folio       = b.folio ",
                                       "\n AND b.folio = ", v_r_agrupador.folio,
                                       "\n AND c.fondo = 11 ",
                                       "\n AND d.subcuenta IN (4) "

                     -- si se recibio nss 
                     IF ( v_id_derechohabiente IS NOT NULL ) THEN 
                        LET v_query = v_query, "\n AND b.id_derechohabiente = ", v_id_derechohabiente
                     END IF

                     LET v_query = v_query, "\n INTO TEMP temp_saldo_ins "

                     --DISPLAY "Query: "
                     --DISPLAY v_query

                     PREPARE sid_reporte_temp_1 FROM v_query
                     EXECUTE sid_reporte_temp_1

                     
                        -- se consulta del detalle de este agrupador
                        LET v_query =  "\n SELECT folio,id_solicitud,f_actualiza,nss,nombre_af,ap_paterno_af,ap_materno_af,",
                                       "\n SUM(aivs_viv92),SUM(aivs_viv97),precio_fondo ",
                                       "\n FROM temp_saldo_ins ",
                                       "\n GROUP BY folio,id_solicitud,f_actualiza,nss,nombre_af,ap_paterno_af,ap_materno_af,precio_fondo",
                                       "\n ORDER BY folio, nss"
                     
                     -- se obtienen los registros para el reporte
                     PREPARE sid_detalle FROM v_query
                     DECLARE cur_detalle CURSOR FOR sid_detalle

                     {
                     -- se consulta del detalle de este agrupador
                     LET v_query = "\n SELECT distinct b.folio                                    ,",
                                   "\n        b.id_solicitud                             ,",
                                   "\n        g.f_actualiza                              ,",
                                   "\n        a.nss                                      ,",  
                                   "\n        a.nombre_af                                ,",
                                   "\n        a.ap_paterno_af                            ,",
                                   "\n        a.ap_materno_af                            ,",
                                   "\n        b.aivs_viv92                               ,",
                                   "\n        b.aivs_viv97                               ,",
                                   "\n        c.precio_fondo                              ",
                                   "\n FROM afi_derechohabiente a                        ,",
                                   "\n      ret_ley73       b                            ,",
                                   "\n      glo_valor_fondo c                            ,",
                                   "\n      ret_his_saldo d                              ,",
                                   "\n      glo_folio     g                               ",
                                   "\n WHERE b.id_derechohabiente = a.id_derechohabiente  ",
                                   "\n AND b.id_solicitud = d.id_solicitud                ",
                                   "\n AND c.f_valuacion = b.f_valuacion                  ",
                                   "\n AND g.folio       = b.folio                        ",
                                   "\n AND b.folio = ", v_r_agrupador.folio                ,
                                   "\n AND c.fondo = 11                                   "
                     
                     -- si se recibio nss 
                     IF ( v_id_derechohabiente IS NOT NULL ) THEN 
                        LET v_query = v_query, "\n AND b.id_derechohabiente = ", v_id_derechohabiente
                     END IF
                     
                     DISPLAY v_query
                     PREPARE sid_detalle FROM v_query
                     DECLARE cur_detalle  CURSOR FOR sid_detalle
                     }
                     
                     
                     --llena el arreglo        
                     LET v_indice = 1
                     
                     FOREACH cur_detalle INTO 
                           arr_reg_saldos[v_indice].v_folio             ,
                           v_id_solicitud                               ,
                           arr_reg_saldos[v_indice].v_f_carga           ,
                           arr_reg_saldos[v_indice].v_nss               ,
                           v_nombre_trab                                ,
                           v_paterno_trab                               ,
                           v_materno_trab                               ,
                           arr_reg_saldos[v_indice].v_aivs92_sol        ,
                           arr_reg_saldos[v_indice].v_aivs97_sol        ,
                           v_precio_fondo                               

                        -- se obtiene el saldo de la subcuenta de vivienda 92 de la tabla de insuficiencia
                        SELECT saldo_acciones,
                               saldo_pesos
                        INTO   arr_reg_saldos[v_indice].v_aivs92_sdo      ,
                               arr_reg_saldos[v_indice].v_aivs92_sdo_pesos
                        FROM   ret_his_saldo
                        WHERE  id_solicitud = v_id_solicitud
                        AND    folio        = arr_reg_saldos[v_indice].v_folio
                        AND    subcuenta    = 8
                        
                        -- se obtiene el saldo de la subcuenta de vivienda 97 de la tabla de insuficiencia
                        SELECT saldo_acciones,
                               saldo_pesos
                        INTO   arr_reg_saldos[v_indice].v_aivs97_sdo      ,
                               arr_reg_saldos[v_indice].v_aivs97_sdo_pesos
                        FROM   ret_his_saldo
                        WHERE  id_solicitud = v_id_solicitud
                        AND    folio        = arr_reg_saldos[v_indice].v_folio
                        AND    subcuenta    = 4
                        
                        -- se obtiene el saldo de la subcuenta de aportaciones voluntarias de la tabla de insuficiencia
                        SELECT saldo_acciones,
                               saldo_pesos
                        INTO   arr_reg_saldos[v_indice].v_aivsvol_sdo      ,
                               arr_reg_saldos[v_indice].v_aivsvol_sdo_pesos
                        FROM   ret_his_saldo
                        WHERE  id_solicitud = v_id_solicitud
                        AND    folio        = arr_reg_saldos[v_indice].v_folio
                        AND    subcuenta    = 45
                        
                        -- se verifica si se obtuvieron datos
                        IF ( arr_reg_saldos[v_indice].v_aivs92_sdo IS NULL ) THEN
                           LET arr_reg_saldos[v_indice].v_aivs92_sdo = 0
                           LET arr_reg_saldos[v_indice].v_aivs92_sdo_pesos = 0
                        ELSE
                           LET arr_reg_saldos[v_indice].v_aivs92_sdo_pesos = arr_reg_saldos[v_indice].v_aivs92_sdo * v_precio_fondo
                        END IF


                        IF ( arr_reg_saldos[v_indice].v_aivs97_sdo IS NULL ) THEN
                           LET arr_reg_saldos[v_indice].v_aivs97_sdo = 0
                           LET arr_reg_saldos[v_indice].v_aivs97_sdo_pesos = 0
                        ELSE
                           LET arr_reg_saldos[v_indice].v_aivs97_sdo_pesos = arr_reg_saldos[v_indice].v_aivs97_sdo * v_precio_fondo
                        END IF
                        
                        IF ( arr_reg_saldos[v_indice].v_aivsvol_sdo IS NULL ) THEN
                           LET arr_reg_saldos[v_indice].v_aivsvol_sdo = 0
                           LET arr_reg_saldos[v_indice].v_aivsvol_sdo_pesos = 0
                        ELSE
                           LET arr_reg_saldos[v_indice].v_aivsvol_sdo_pesos = arr_reg_saldos[v_indice].v_aivsvol_sdo * v_precio_fondo
                        END IF

                        LET arr_reg_saldos[v_indice].v_nombre_trabajador =  v_nombre_trab CLIPPED||" "||v_paterno_trab CLIPPED ||" "||v_materno_trab CLIPPED   

                        LET arr_reg_saldos[v_indice].v_aivssum97vol_sdo_pesos = arr_reg_saldos[v_indice].v_aivs97_sdo_pesos + arr_reg_saldos[v_indice].v_aivsvol_sdo_pesos
                        LET arr_reg_saldos[v_indice].v_aivssum97vol_sdo       = arr_reg_saldos[v_indice].v_aivs97_sdo       + arr_reg_saldos[v_indice].v_aivsvol_sdo
                        
                        -- se calculan los pesos de las AIVs solicitadas
                        LET arr_reg_saldos[v_indice].v_aivs92_sol_pesos  = (arr_reg_saldos[v_indice].v_aivs92_sol * v_precio_fondo)
                        LET arr_reg_saldos[v_indice].v_aivs97_sol_pesos  = (arr_reg_saldos[v_indice].v_aivs97_sol * v_precio_fondo)
                        
                        -- se calculan las diferencias
                        LET arr_reg_saldos[v_indice].v_aivs92_dif        = arr_reg_saldos[v_indice].v_aivs92_sol       - arr_reg_saldos[v_indice].v_aivs92_sdo     
                        LET arr_reg_saldos[v_indice].v_aivs92_dif_pesos  = arr_reg_saldos[v_indice].v_aivs92_sol_pesos - arr_reg_saldos[v_indice].v_aivs92_sdo_pesos
                        
                        LET arr_reg_saldos[v_indice].v_aivs97_dif        = arr_reg_saldos[v_indice].v_aivs97_sol       - arr_reg_saldos[v_indice].v_aivssum97vol_sdo     
                        LET arr_reg_saldos[v_indice].v_aivs97_dif_pesos  = arr_reg_saldos[v_indice].v_aivs97_sol_pesos - arr_reg_saldos[v_indice].v_aivssum97vol_sdo_pesos
                     
                        LET v_indice = v_indice + 1
                     END FOREACH
                     
                     -- se borra el ultimo registro
                     CALL arr_reg_saldos.deleteElement(arr_reg_saldos.getLength())
               END DISPLAY
               
               DISPLAY ARRAY arr_reg_saldos TO r_saldos_insuf.*
               END DISPLAY
               
               ON ACTION regresar
                  EXIT DIALOG
                  
               ON ACTION reporte                      
                  -- se consulta del detalle de este agrupador
                     --Se crea tabla temporal
                     WHENEVER ERROR CONTINUE
                        DROP TABLE temp_saldo_ins
                     WHENEVER ERROR STOP
                     LET v_query =  "\n SELECT b.folio,b.id_solicitud,g.f_actualiza,a.nss,a.nombre_af,",
                                    "\n a.ap_paterno_af,a.ap_materno_af,b.aivs_viv92,0 as aivs_viv97,c.precio_fondo",
                                    "\n FROM afi_derechohabiente a,ret_ley73 b,glo_valor_fondo c,ret_his_saldo d,glo_folio g",
                                    "\n WHERE b.id_derechohabiente = a.id_derechohabiente ",
                                    "\n AND b.id_solicitud = d.id_solicitud ",              
                                    "\n AND c.f_valuacion = b.f_valuacion ",
                                    "\n AND g.folio       = b.folio ",
                                    "\n AND c.fondo = 11 ",
                                    "\n AND d.subcuenta IN (8) "
                     
                  -- si se recibio el folio como parametro
                  IF ( v_folio IS NOT NULL AND v_folio > 0 ) THEN
                     -- agrupador
                     LET v_query = v_query, "\n AND b.folio= ", v_folio                   
                  END IF
                                    
                  -- si se recibieron la fecha inicial y la fecha final como parametro
                  IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
                     -- agrupador
                     LET v_query = v_query, "\n AND g.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
                  END IF
                  
                  IF ( v_id_derechohabiente IS NOT NULL ) THEN 
                     LET v_query = v_query, "\n AND a.id_derechohabiente = ", v_id_derechohabiente
                  END IF

                  LET v_query = v_query,"\n UNION ",
                                    "\n SELECT b.folio,b.id_solicitud,g.f_actualiza,a.nss,a.nombre_af,a.ap_paterno_af,",
                                    "\n a.ap_materno_af,0,b.aivs_viv97,c.precio_fondo ",
                                    "\n FROM afi_derechohabiente a,ret_ley73 b,glo_valor_fondo c,ret_his_saldo d,glo_folio g",
                                    "\n WHERE b.id_derechohabiente = a.id_derechohabiente ",
                                    "\n AND b.id_solicitud = d.id_solicitud ",
                                    "\n AND c.f_valuacion = b.f_valuacion ",
                                    "\n AND g.folio       = b.folio ",
                                    "\n AND c.fondo = 11 ",
                                    "\n AND d.subcuenta IN (4) "

                  -- si se recibio el folio como parametro
                  IF ( v_folio IS NOT NULL AND v_folio > 0 ) THEN
                     -- agrupador
                     LET v_query = v_query, "\n AND b.folio= ", v_folio                   
                  END IF
                                    
                  -- si se recibieron la fecha inicial y la fecha final como parametro
                  IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
                     -- agrupador
                     LET v_query = v_query, "\n AND g.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
                  END IF
                  
                  IF ( v_id_derechohabiente IS NOT NULL ) THEN 
                     LET v_query = v_query, "\n AND a.id_derechohabiente = ", v_id_derechohabiente
                  END IF

                  LET v_query = v_query, "\n INTO TEMP temp_saldo_ins "

                  --DISPLAY "Query: "
                  --DISPLAY v_query

                  PREPARE sid_reporte_temp FROM v_query
                  EXECUTE sid_reporte_temp

                  
                     -- se consulta del detalle de este agrupador
                     LET v_query =  "\n SELECT folio,id_solicitud,f_actualiza,nss,nombre_af,ap_paterno_af,ap_materno_af,",
                                    "\n SUM(aivs_viv92),SUM(aivs_viv97),precio_fondo ",
                                    "\n FROM temp_saldo_ins ",
                                    "\n GROUP BY folio,id_solicitud,f_actualiza,nss,nombre_af,ap_paterno_af,ap_materno_af,precio_fondo",
                                    "\n ORDER BY folio, nss"
                  
                  -- se obtienen los registros para el reporte
                  PREPARE sid_reporte FROM v_query
                  DECLARE cur_reporte CURSOR FOR sid_reporte
                  
                  -- llena el arreglo
                  LET v_indice = 1
                  
                  FOREACH cur_reporte INTO 
                        arr_reporte[v_indice].v_folio            ,
                        v_id_solicitud                           ,
                        arr_reporte[v_indice].v_f_carga          ,
                        arr_reporte[v_indice].v_nss              ,
                        v_nombre_trab                            ,
                        v_paterno_trab                           ,
                        v_materno_trab                           ,
                        arr_reporte[v_indice].v_aivs92_sol       ,
                        arr_reporte[v_indice].v_aivs97_sol       ,
                        v_precio_fondo                           
                        
                     -- se obtiene el saldo de la subcuenta de vivienda 92 de la tabla de insuficiencia
                     SELECT saldo_acciones,
                            saldo_pesos
                     INTO   arr_reporte[v_indice].v_aivs92_sdo      ,
                            arr_reporte[v_indice].v_aivs92_sdo_pesos
                     FROM   ret_his_saldo
                     WHERE  id_solicitud = v_id_solicitud
                     AND    folio        = arr_reporte[v_indice].v_folio
                     AND    subcuenta    = 8
                     
                     -- se obtiene el saldo de la subcuenta de vivienda 97 de la tabla de insuficiencia
                     SELECT saldo_acciones,
                            saldo_pesos
                     INTO   arr_reporte[v_indice].v_aivs97_sdo      ,
                            arr_reporte[v_indice].v_aivs97_sdo_pesos
                     FROM   ret_his_saldo
                     WHERE  id_solicitud = v_id_solicitud
                     AND    folio        = arr_reporte[v_indice].v_folio
                     AND    subcuenta    = 4

                     -- se obtiene el saldo de la subcuenta de vivienda 97 de la tabla de insuficiencia
                     SELECT saldo_acciones,
                            saldo_pesos
                     INTO   arr_reporte[v_indice].v_aivsvol_sdo      ,
                            arr_reporte[v_indice].v_aivsvol_sdo_pesos
                     FROM   ret_his_saldo
                     WHERE  id_solicitud = v_id_solicitud
                     AND    folio        = arr_reporte[v_indice].v_folio
                     AND    subcuenta    = 45
                     
                     -- se verifica si se obtuvieron datos
                     IF ( arr_reporte[v_indice].v_aivs92_sdo IS NULL ) THEN
                        LET arr_reporte[v_indice].v_aivs92_sdo = 0
                        LET arr_reporte[v_indice].v_aivs92_sdo_pesos = 0
                     ELSE
                        LET arr_reporte[v_indice].v_aivs92_sdo_pesos = arr_reporte[v_indice].v_aivs92_sdo * v_precio_fondo
                     END IF

                     IF ( arr_reporte[v_indice].v_aivs97_sdo IS NULL ) THEN
                        LET arr_reporte[v_indice].v_aivs97_sdo = 0
                        LET arr_reporte[v_indice].v_aivs97_sdo_pesos = 0
                     ELSE
                        LET arr_reporte[v_indice].v_aivs97_sdo_pesos = arr_reporte[v_indice].v_aivs97_sdo * v_precio_fondo
                     END IF

                     IF ( arr_reporte[v_indice].v_aivsvol_sdo IS NULL ) THEN
                        LET arr_reporte[v_indice].v_aivsvol_sdo = 0
                        LET arr_reporte[v_indice].v_aivsvol_sdo_pesos = 0
                     ELSE
                        LET arr_reporte[v_indice].v_aivsvol_sdo_pesos = arr_reporte[v_indice].v_aivsvol_sdo * v_precio_fondo
                     END IF
                        
                     LET arr_reporte[v_indice].v_nombre_trabajador = v_nombre_trab CLIPPED||" "||v_paterno_trab CLIPPED ||" "||v_materno_trab CLIPPED   

                     LET arr_reporte[v_indice].v_aivssum97vol_sdo_pesos  = (arr_reporte[v_indice].v_aivs97_sdo_pesos + arr_reporte[v_indice].v_aivsvol_sdo_pesos  * v_precio_fondo)
                     LET arr_reporte[v_indice].v_aivssum97vol_sdo        = (arr_reporte[v_indice].v_aivs97_sdo + arr_reporte[v_indice].v_aivsvol_sdo)

                     
                     -- se calculan los pesos de las AIVs solicitadas
                     LET arr_reporte[v_indice].v_aivs92_sol_pesos  = (arr_reporte[v_indice].v_aivs92_sol * v_precio_fondo)
                     LET arr_reporte[v_indice].v_aivs97_sol_pesos  = (arr_reporte[v_indice].v_aivs97_sol * v_precio_fondo)
                     
                     -- se calculan las diferencias
                     LET arr_reporte[v_indice].v_aivs92_dif        = arr_reporte[v_indice].v_aivs92_sol       - arr_reporte[v_indice].v_aivs92_sdo     
                     LET arr_reporte[v_indice].v_aivs92_dif_pesos  = arr_reporte[v_indice].v_aivs92_sol_pesos - arr_reporte[v_indice].v_aivs92_sdo_pesos
                     
                     LET arr_reporte[v_indice].v_aivs97_dif        = arr_reporte[v_indice].v_aivs97_sol       - arr_reporte[v_indice].v_aivssum97vol_sdo     
                     LET arr_reporte[v_indice].v_aivs97_dif_pesos  = arr_reporte[v_indice].v_aivs97_sol_pesos - arr_reporte[v_indice].v_aivssum97vol_sdo_pesos
                  
                     LET v_indice = v_indice + 1
                  END FOREACH

                  -- elimina ultimo renglon en blanco
                  CALL arr_reporte.deleteElement(arr_reporte.getLength())

                  -- Recupera la ruta de listados en el que se enviara el archivo
                  CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
    
                  -- Se asigna la plantilla para generar el reporte
                  IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETC130.4rp") ) THEN 
                     CALL fgl_report_selectDevice ("PDF")
                       
                     LET v_ruta_reporte = v_ruta_ejecutable CLIPPED,"/","detalle_saldos_insuf"                
                     CALL fgl_report_setOutputFileName(v_ruta_reporte)
                     CALL fgl_report_selectPreview(1)
                     LET manejador_rpt = fgl_report_commitCurrentSettings()
                  ELSE         
                     CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla RETC130.4rp", "stop")
                     CONTINUE DIALOG
                  END IF   

                  --Inicia el reporte de registros con rechazo
                  START REPORT rpt_detalle_saldos_insuf TO XML HANDLER manejador_rpt
                  
                  FOR v_indice_reporte = 1 TO arr_reporte.getLength()
                     LET v_agrupador_folio_fecha_Afore = arr_reporte[v_indice_reporte].v_folio USING "&&&&&&&&&", arr_reporte[v_indice_reporte].v_f_carga USING "ddmmyyyy"
                     
                     OUTPUT TO REPORT rpt_detalle_saldos_insuf(v_indice_reporte, arr_reporte[v_indice_reporte].*, p_usuario_cod, v_agrupador_folio_fecha_Afore)
                  END FOR
                  
                  FINISH REPORT rpt_detalle_saldos_insuf

            END DIALOG
            
            CLOSE WINDOW w_detalle_consulta_saldos
         END IF           
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_saldos

END FUNCTION

{ ======================================================================
Clave: 
Nombre: rpt_detalle_saldos_insuf
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de los saldos insuficientes 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_detalle_saldos_insuf(p_indice, v_r_despliegue, p_usuario_cod, p_agrupador_folio_fecha_Afore)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
           v_folio                  LIKE ret_transferencia.folio,
           v_f_carga                LIKE ret_cza_transferencia.f_carga,
           v_nss                    LIKE afi_derechohabiente.nss,
           v_nombre_trabajador      STRING, 
           v_aivs92_sol             DECIMAL(24,6),
           v_aivs97_sol             DECIMAL(24,6),
           v_aivs92_sol_pesos       DECIMAL(22,2),
           v_aivs97_sol_pesos       DECIMAL(22,2),
           v_aivs92_sdo             DECIMAL(24,6),
           v_aivs97_sdo             DECIMAL(24,6),
           v_aivsvol_sdo            DECIMAL(24,6),
           v_aivssum97vol_sdo       DECIMAL(24,6),
           v_aivs92_sdo_pesos       DECIMAL(22,2),
           v_aivs97_sdo_pesos       DECIMAL(22,2),
           v_aivsvol_sdo_pesos      DECIMAL(22,2),
           v_aivssum97vol_sdo_pesos DECIMAL(22,2),
           v_aivs92_dif             DECIMAL(24,6),
           v_aivs97_dif             DECIMAL(24,6),
           v_aivs92_dif_pesos       DECIMAL(22,2),        
           v_aivs97_dif_pesos       DECIMAL(22,2)
          END RECORD,
          p_indice                DECIMAL(9,0),
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para acumular TOTAL PARCIAL
          v_total_afore_aivs92_sol             DECIMAL(24,6),
          v_total_afore_aivs97_sol             DECIMAL(24,6),
          v_total_afore_aivs92_sol_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_sol_pesos       DECIMAL(22,2),
          v_total_afore_aivs92_sdo             DECIMAL(24,6),
          v_total_afore_aivs97_sdo             DECIMAL(24,6),
          v_total_afore_aivsvol_sdo            DECIMAL(24,6),
          v_total_afore_aivssum97vol_sdo       DECIMAL(24,6),
          v_total_afore_aivs92_sdo_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_sdo_pesos       DECIMAL(22,2),
          v_total_afore_aivsvol_sdo_pesos      DECIMAL(22,2),
          v_total_afore_aivssum97vol_sdo_pesos DECIMAL(22,2),
          v_total_afore_aivs92_dif             DECIMAL(24,6),
          v_total_afore_aivs97_dif             DECIMAL(24,6),
          v_total_afore_aivs92_dif_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_dif_pesos       DECIMAL(22,2),
          p_total_afore_regs                   DECIMAL(9,0),
          -- variables para acumular GRAN TOTAL
          v_total_aivs92_sol             DECIMAL(24,6),
          v_total_aivs97_sol             DECIMAL(24,6),
          v_total_aivs92_sol_pesos       DECIMAL(22,2),
          v_total_aivs97_sol_pesos       DECIMAL(22,2),
          v_total_aivs92_sdo             DECIMAL(24,6),
          v_total_aivs97_sdo             DECIMAL(24,6),
          v_total_aivsvol_sdo            DECIMAL(24,6),
          v_total_aivssum97vol_sdo       DECIMAL(24,6),
          v_total_aivs92_sdo_pesos       DECIMAL(22,2),
          v_total_aivs97_sdo_pesos       DECIMAL(22,2),
          v_total_aivsvol_sdo_pesos      DECIMAL(22,2),
          v_total_aivssum97vol_sdo_pesos DECIMAL(22,2),
          v_total_aivs92_dif             DECIMAL(24,6),
          v_total_aivs97_dif             DECIMAL(24,6),
          v_total_aivs92_dif_pesos       DECIMAL(22,2),
          v_total_aivs97_dif_pesos       DECIMAL(22,2),
          p_total_regs                   DECIMAL(9,0),
          v_fecha_carga                  STRING,
          p_agrupador_folio_fecha_Afore  STRING
          
FORMAT

   FIRST PAGE HEADER
      
      -- variables para acumular gran total
      LET v_total_afore_aivs92_sol             = 0
      LET v_total_afore_aivs97_sol             = 0
      LET v_total_afore_aivs92_sol_pesos       = 0
      LET v_total_afore_aivs97_sol_pesos       = 0
      LET v_total_afore_aivs92_sdo             = 0
      LET v_total_afore_aivs97_sdo             = 0
      LET v_total_afore_aivsvol_sdo            = 0
      LET v_total_afore_aivssum97vol_sdo       = 0
      LET v_total_afore_aivs92_sdo_pesos       = 0
      LET v_total_afore_aivs97_sdo_pesos       = 0
      LET v_total_afore_aivsvol_sdo_pesos      = 0
      LET v_total_afore_aivssum97vol_sdo_pesos = 0
      LET v_total_afore_aivs92_dif             = 0
      LET v_total_afore_aivs97_dif             = 0
      LET v_total_afore_aivs92_dif_pesos       = 0
      LET v_total_afore_aivs97_dif_pesos       = 0
      LET p_total_afore_regs                   = 0
      
      -- variables para acumular por afore, fecha y folio
      LET v_total_aivs92_sol              = 0
      LET v_total_aivs97_sol              = 0
      LET v_total_aivs92_sol_pesos        = 0
      LET v_total_aivs97_sol_pesos        = 0
      LET v_total_aivs92_sdo              = 0
      LET v_total_aivs97_sdo              = 0
      LET v_total_aivsvol_sdo             = 0
      LET v_total_aivssum97vol_sdo        = 0
      LET v_total_aivs92_sdo_pesos        = 0
      LET v_total_aivs97_sdo_pesos        = 0
      LET v_total_aivsvol_sdo_pesos       = 0
      LET v_total_aivssum97vol_sdo_pesos  = 0
      LET v_total_aivs92_dif              = 0
      LET v_total_aivs97_dif              = 0
      LET v_total_aivs92_dif_pesos        = 0
      LET v_total_aivs97_dif_pesos        = 0
      LET p_total_regs                    = 0

      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod

      LET v_nombre_usuario = v_nombre_usuario CLIPPED
      
      PRINTX p_usuario_cod, v_fecha, v_nombre_usuario
      
   --BEFORE GROUP OF v_r_despliegue.v_afore
   BEFORE GROUP OF p_agrupador_folio_fecha_Afore
      LET v_fecha_carga = v_r_despliegue.v_f_carga USING "dd-mm-yyyy"
      
      -- se reinician los totales por afore, fecha y folio   
      LET v_total_afore_aivs92_sol             = 0
      LET v_total_afore_aivs97_sol             = 0
      LET v_total_afore_aivs92_sol_pesos       = 0
      LET v_total_afore_aivs97_sol_pesos       = 0
      LET v_total_afore_aivs92_sdo             = 0
      LET v_total_afore_aivs97_sdo             = 0
      LET v_total_afore_aivsvol_sdo            = 0
      LET v_total_afore_aivssum97vol_sdo       = 0
      LET v_total_afore_aivs92_sdo_pesos       = 0
      LET v_total_afore_aivs97_sdo_pesos       = 0
      LET v_total_afore_aivsvol_sdo_pesos      = 0
      LET v_total_afore_aivssum97vol_sdo_pesos = 0
      LET v_total_afore_aivs92_dif             = 0
      LET v_total_afore_aivs97_dif             = 0
      LET v_total_afore_aivs92_dif_pesos       = 0
      LET v_total_afore_aivs97_dif_pesos       = 0
      LET p_total_afore_regs                   = 0
   
      PRINTX v_r_despliegue.v_folio,
             v_fecha_carga
             

   ON EVERY ROW
      PRINTX v_r_despliegue.*
      DISPLAY v_r_despliegue.*
      
      -- se acumulan los montos para total por afore, folio y fecha de carga
      LET v_total_afore_aivs92_sol             = v_total_afore_aivs92_sol             + v_r_despliegue.v_aivs92_sol      
      LET v_total_afore_aivs97_sol             = v_total_afore_aivs97_sol             + v_r_despliegue.v_aivs97_sol      
      LET v_total_afore_aivs92_sol_pesos       = v_total_afore_aivs92_sol_pesos       + v_r_despliegue.v_aivs92_sol_pesos
      LET v_total_afore_aivs97_sol_pesos       = v_total_afore_aivs97_sol_pesos       + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_afore_aivs92_sdo             = v_total_afore_aivs92_sdo             + v_r_despliegue.v_aivs92_sdo      
      LET v_total_afore_aivs97_sdo             = v_total_afore_aivs97_sdo             + v_r_despliegue.v_aivs97_sdo      
      LET v_total_afore_aivsvol_sdo            = v_total_afore_aivsvol_sdo            + v_r_despliegue.v_aivsvol_sdo      
      LET v_total_afore_aivssum97vol_sdo       = v_total_afore_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_afore_aivs92_sdo_pesos       = v_total_afore_aivs92_sdo_pesos       + v_r_despliegue.v_aivs92_sdo_pesos
      LET v_total_afore_aivs97_sdo_pesos       = v_total_afore_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_afore_aivsvol_sdo_pesos      = v_total_afore_aivsvol_sdo_pesos      + v_r_despliegue.v_aivsvol_sdo_pesos
      LET v_total_afore_aivssum97vol_sdo_pesos = v_total_afore_aivssum97vol_sdo_pesos + v_r_despliegue.v_aivssum97vol_sdo_pesos
      LET v_total_afore_aivs92_dif             = v_total_afore_aivs92_dif             + v_r_despliegue.v_aivs92_dif      
      LET v_total_afore_aivs97_dif             = v_total_afore_aivs97_dif             + v_r_despliegue.v_aivs97_dif      
      LET v_total_afore_aivs92_dif_pesos       = v_total_afore_aivs92_dif_pesos       + v_r_despliegue.v_aivs92_dif_pesos
      LET v_total_afore_aivs97_dif_pesos       = v_total_afore_aivs97_dif_pesos       + v_r_despliegue.v_aivs97_dif_pesos
      LET p_total_afore_regs                   = p_total_afore_regs                   + 1
      
      
      -- se acumulan los montos para gran total
      LET v_total_aivs92_sol             = v_total_aivs92_sol             + v_r_despliegue.v_aivs92_sol      
      LET v_total_aivs97_sol             = v_total_aivs97_sol             + v_r_despliegue.v_aivs97_sol      
      LET v_total_aivs92_sol_pesos       = v_total_aivs92_sol_pesos       + v_r_despliegue.v_aivs92_sol_pesos
      LET v_total_aivs97_sol_pesos       = v_total_aivs97_sol_pesos       + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_aivs92_sdo             = v_total_aivs92_sdo             + v_r_despliegue.v_aivs92_sdo      
      LET v_total_aivs97_sdo             = v_total_aivs97_sdo             + v_r_despliegue.v_aivs97_sdo     
      LET v_total_aivsvol_sdo            = v_total_aivsvol_sdo            + v_r_despliegue.v_aivsvol_sdo     
      LET v_total_aivssum97vol_sdo       = v_total_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_aivs92_sdo_pesos       = v_total_aivs92_sdo_pesos       + v_r_despliegue.v_aivs92_sdo_pesos
      LET v_total_aivs97_sdo_pesos       = v_total_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_aivsvol_sdo_pesos      = v_total_aivsvol_sdo_pesos      + v_r_despliegue.v_aivsvol_sdo_pesos
      LET v_total_aivssum97vol_sdo_pesos = v_total_aivssum97vol_sdo_pesos + v_r_despliegue.v_aivssum97vol_sdo_pesos
      LET v_total_aivs92_dif             = v_total_aivs92_dif             + v_r_despliegue.v_aivs92_dif      
      LET v_total_aivs97_dif             = v_total_aivs97_dif             + v_r_despliegue.v_aivs97_dif      
      LET v_total_aivs92_dif_pesos       = v_total_aivs92_dif_pesos       + v_r_despliegue.v_aivs92_dif_pesos
      LET v_total_aivs97_dif_pesos       = v_total_aivs97_dif_pesos       + v_r_despliegue.v_aivs97_dif_pesos
      LET p_total_regs                   = p_total_regs                   + 1

   --AFTER GROUP OF v_r_despliegue.v_afore
   AFTER GROUP OF p_agrupador_folio_fecha_Afore
      PRINTX v_total_afore_aivs92_sol             ,
             v_total_afore_aivs97_sol             ,
             v_total_afore_aivs92_sol_pesos       ,
             v_total_afore_aivs97_sol_pesos       ,
             v_total_afore_aivs92_sdo             ,
             v_total_afore_aivs97_sdo             ,
             v_total_afore_aivsvol_sdo            ,
             v_total_afore_aivssum97vol_sdo       ,
             v_total_afore_aivs92_sdo_pesos       ,
             v_total_afore_aivs97_sdo_pesos       ,
             v_total_afore_aivsvol_sdo_pesos      ,
             v_total_afore_aivssum97vol_sdo_pesos ,
             v_total_afore_aivs92_dif             ,
             v_total_afore_aivs97_dif             ,
             v_total_afore_aivs92_dif_pesos       ,
             v_total_afore_aivs97_dif_pesos       ,
             p_total_afore_regs            
                                                          
   
   ON LAST ROW 
      PRINTX p_total_regs                   ,
             v_total_aivs92_sol             ,
             v_total_aivs97_sol             ,
             v_total_aivs92_sol_pesos       ,
             v_total_aivs97_sol_pesos       ,
             v_total_aivs92_sdo             ,
             v_total_aivs97_sdo             ,
             v_total_aivsvol_sdo            ,
             v_total_aivssum97vol_sdo       ,
             v_total_aivs92_sdo_pesos       ,
             v_total_aivs97_sdo_pesos       ,
             v_total_aivsvol_sdo_pesos      ,
             v_total_aivssum97vol_sdo_pesos ,
             v_total_aivs92_dif             ,
             v_total_aivs97_dif             ,
             v_total_aivs92_dif_pesos       ,
             v_total_aivs97_dif_pesos 

END REPORT
