--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14 Diciembre, 2015
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC33                                                                 #
#Objetivo     => Consulta sdos insuficientes en solicitudes de retiro por transferencia #
#Fecha inicio => Octubre 10, 2012                                                       # 
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

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETC33.log")
   
   -- consulta de informacion recibida 
   CALL fn_consulta_saldos(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC33
Nombre: fn_consulta_saldos
Fecha creacion: Octubre 10, 2012
Registro de modificaciones:
Descrip: CONSULTA SALDOS INSUFICIENTES EN SOLICITUDES DE RETIRO POR TRANSFERENCIA

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Luis Prieto     14/12/2015              Se integra combo para solicitar Variación
==============================================================================
}
FUNCTION fn_consulta_saldos(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio          DECIMAL(9,0), -- folio
       v_afore          INTEGER,
       v_nss            CHAR(11),
       v_variacion      SMALLINT,
       v_f_inicial      LIKE ret_cza_transferencia.f_carga,
       v_f_final        LIKE ret_cza_transferencia.f_carga,
       v_cbx_folios     ui.ComboBox, -- combo de folios
       v_cbx_afores     ui.ComboBox, -- combo de afores
       v_cbx_variacion  ui.ComboBox, -- Combo de variaciones
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
       v_r_cat_afore    RECORD 
         v_afore_cod      LIKE cat_afore.afore_cod,
         v_afore_desc     LIKE cat_afore.afore_desc
        END RECORD,
       v_r_agrupador   RECORD -- registro de despliegue del agrupador
        folio            LIKE glo_folio.folio,
        f_carga          DATE,
        afore_cod        LIKE cat_afore.afore_cod,
        afore_desc       LIKE cat_afore.afore_desc
       END RECORD,
       v_arr_agrupador  DYNAMIC ARRAY OF RECORD -- arreglo de despliegue del agrupador
        folio            LIKE glo_folio.folio,
        f_carga          DATE,
        afore_cod        LIKE cat_afore.afore_cod,
        afore_desc       LIKE cat_afore.afore_desc
       END RECORD,
       arr_reg_sds               RECORD
         folio                   LIKE ret_transferencia.folio,              
         f_carga                 LIKE ret_cza_transferencia.f_carga,
         afore_cod               LIKE cat_afore.afore_cod,
         afore_desc              LIKE cat_afore.afore_desc,
         nss                     CHAR(11),
         nombre_trab             LIKE ret_transferencia.nombre_afore,
         paterno_trab            LIKE ret_transferencia.paterno_afore,
         materno_trab            LIKE ret_transferencia.materno_afore,
         aivs97_sol              LIKE ret_transferencia.aivs_viv97,   
         precio_fondo            LIKE ret_cza_transferencia.precio_fondo,
         aivs97_sdo              DECIMAL (19,6),
         aivs97_sdo_pesos        DECIMAL (19,6),
         aivsvol_sdo             DECIMAL (19,6),
         aivsvol_sdo_pesos       DECIMAL (19,6)
       END RECORD,
       arr_reg_saldos,
       arr_reporte,
       arr_export                 DYNAMIC ARRAY OF RECORD
         v_folio                  LIKE ret_transferencia.folio,
         v_f_carga                LIKE ret_cza_transferencia.f_carga,
         v_afore                  STRING,
         v_nss                    LIKE afi_derechohabiente.nss,
         v_nombre_trabajador      STRING, 
         v_aivs97_sol             LIKE ret_transferencia.aivs_viv97,
         v_aivs97_sol_pesos       DECIMAL (19,6),
         v_aivs97_sdo             DECIMAL (19,6),
         v_aivs97_sdo_pesos       DECIMAL (19,6),
         v_aivsvol_sdo            DECIMAL (19,6),
         v_aivsvol_sdo_pesos      DECIMAL (19,6),
         v_aivssum97vol_sdo       DECIMAL (19,6),
         v_aivssum97vol_sdo_pesos DECIMAL (19,6),
         v_aivs97_dif             DECIMAL (19,6),
         v_aivs97_dif_pesos       DECIMAL (19,6)
       END RECORD,
       v_query              STRING, -- detalle
       v_query_agrupador    STRING, -- agrupador por folio y afore
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_indice             SMALLINT, -- indice de arreglo
       v_afore_cod      LIKE cat_afore.afore_cod,
       v_afore_desc     LIKE cat_afore.afore_desc, 
       v_nombre_trab    LIKE ret_transferencia.nombre_afore,
       v_paterno_trab   LIKE ret_transferencia.paterno_afore,
       v_materno_trab   LIKE ret_transferencia.materno_afore,
       v_precio_fondo   LIKE ret_cza_transferencia.precio_fondo,
       v_ruta_reporte            STRING ,-- ruta del archivo del reporte       
       v_ruta_listados           STRING ,-- ruta de los listados
       v_ruta_ejecutable         STRING ,-- ruta del ejecutable
       manejador_rpt            om.SaxDocumentHandler ,
       v_indice_reporte          SMALLINT,
       v_agrupador_folio_fecha_Afore STRING,
       v_es_valido               SMALLINT,
       v_aivs_diferencia         DECIMAL(19,6)


       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC331"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta Saldos Insuficientes Transferencia")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_afores = ui.ComboBox.forName("formonly.cmb_afore")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_variacion = ui.ComboBox.forName("formonly.cmb_variacion")
  
   -- se inician los combobox en blanco
   CALL v_cbx_folios.clear()
   CALL v_cbx_afores.clear()
   CALL v_cbx_variacion.clear()

   INPUT v_folio, v_afore, v_nss, v_variacion, v_f_inicial, v_f_final 
      FROM cmb_folio, cmb_afore, e_nss, cmb_variacion, date_f_inicial, date_f_final    
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
         WHERE  a.proceso_cod = g_proceso_cod_ret_transferencia
         AND    a.status >= 0
         ORDER BY folio DESC

         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
        
         -- se llena el arreglo de afores
         DECLARE cur_afores CURSOR FOR
          SELECT a.afore_cod, a.afore_desc
            FROM cat_afore a
        ORDER BY 1, 2

         FOREACH cur_afores INTO v_r_cat_afore.v_afore_cod, v_r_cat_afore.v_afore_desc
            LET v_s_cadena = v_r_cat_afore.v_afore_cod, "  -  ", v_r_cat_afore.v_afore_desc
            CALL v_cbx_afores.addItem(v_r_cat_afore.v_afore_cod, v_s_cadena)
         END FOREACH

         FREE cur_afores

         -- se llena el arreglo de variaciones
         CALL v_cbx_variacion.addItem(1, "Todos")
         CALL v_cbx_variacion.addItem(2, "Menor a 1000 AIVS")
         CALL v_cbx_variacion.addItem(3, "Mayor igual a 1000 AIVS")
         LET v_variacion = 1
         
      ON ACTION ACCEPT

         --DISPLAY "variables capturadas"
         --DISPLAY "@..", v_folio, v_afore, v_nss, v_f_inicial, v_f_final

         -- se borran los arreglos de despliegue
         CALL v_arr_agrupador.clear()
         CALL arr_reg_saldos.clear()
         CALL arr_reporte.clear()

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF (v_folio   IS NULL OR v_folio <= 0)
         AND (v_afore   IS NULL OR v_afore <= 0) 
         AND (v_nss IS NULL OR v_nss   <= 0) 
         AND (v_f_inicial IS NULL)
         AND (v_f_final IS NULL) THEN

            CALL fn_mensaje("Consulta",
            "Debe de ingresar al menos un criterio de búsqueda","about")
            CONTINUE INPUT

         END IF

         --
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

         IF v_variacion IS NULL THEN
            CALL fn_mensaje("Consulta", "Requiere seleccionar un tipo de variación.", "about")
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
         LET v_query_agrupador = "\n SELECT DISTINCT b.folio                         ,",
                                 "\n        c.f_carga                                ,",                                  
                                 "\n        e.afore_cod, e.afore_desc                 ",                  
                                 "\n FROM ret_transferencia b                        ,",                          
                                 "\n      ret_cza_transferencia c                    ,",
                                 "\n      cat_afore e                                ,",
                                 "\n      afi_derechohabiente afi                    ,",
                                 "\n      glo_folio gf                                ",
                                 "\n WHERE b.cve_afore = e.afore_cod                  ",
                                 "\n AND b.folio = c.folio                            ",
                                 "\n AND afi.id_derechohabiente = b.id_derechohabiente",
                                 "\n AND b.folio = gf.folio                           ",
                                 "\n AND gf.proceso_cod = ", g_proceso_cod_ret_transferencia,
                                 "\n AND gf.status >= 0                               "

         -- si se recibio el folio como parametro
         IF ( v_folio IS NOT NULL AND v_folio > 0 ) THEN
            -- agrupador
            LET v_query_agrupador = v_query_agrupador, "\n AND b.folio= ", v_folio                   
         END IF

         -- si se recibio el afore como parametro
         IF ( v_afore IS NOT NULL AND v_afore > 0 ) THEN
            -- agrupador
            LET v_query_agrupador = v_query_agrupador, "\n AND b.cve_afore= ", v_afore                   
         END IF

         -- si se recibieron la fecha inicial y la fecha final como parametro
         IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
            -- agrupador
            LET v_query_agrupador = v_query_agrupador, "\n AND c.f_carga BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
         END IF

         IF ( v_id_derechohabiente IS NOT NULL ) THEN 
            LET v_query_agrupador = v_query_agrupador, "\n AND afi.id_derechohabiente = ", v_id_derechohabiente
         END IF


         LET v_query_agrupador = v_query_agrupador, "\n ORDER BY b.folio, e.afore_cod"

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
         OPEN WINDOW w_detalle_consulta_saldos WITH FORM "RETC332"
         
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
                  
                  -- se consulta del detalle de este agrupador
                  LET v_query = "\n SELECT b.folio                                    ,",                                    
                                "\n        c.f_carga                                  ,",                                  
                                "\n        e.afore_cod, e.afore_desc                  ,",                  
                                "\n        a.nss                                      ,",                                      
                                "\n        b.nombre_afore                             ,",                             
                                "\n        b.paterno_afore                            ,",                            
                                "\n        b.materno_afore                            ,",                           
                                "\n        b.aivs_viv97                               ,",                               
                                "\n        c.precio_fondo                             ,",                             
                                "\n        NVL(d.saldo_acciones, 0)                   ,",                      
                                "\n        NVL(d.saldo_pesos, 0)                      ,",
                                "\n        NVL(f.saldo_acciones, 0)                   ,",                      
                                "\n        NVL(f.saldo_pesos, 0)                       ",
                                "\n FROM afi_derechohabiente a                        ,",                        
                                "\n      ret_transferencia b                          ,",                          
                                "\n      ret_cza_transferencia c                      ,",                      
                                "\n      ret_his_saldo d                               ",                            
                                "\n      LEFT OUTER JOIN ret_his_saldo f               ",
                                "\n                   ON f.id_solicitud = d.id_solicitud ",
                                "\n                  AND f.subcuenta = 55             ,",
                                "\n      cat_afore e                                   ",
                                "\n WHERE b.id_derechohabiente = a.id_derechohabiente  ",
                                "\n AND b.cve_afore = e.afore_cod                      ",
                                "\n AND b.folio = c.folio                              ",
                                "\n AND b.id_solicitud = d.id_solicitud                ",
                                "\n AND d.subcuenta = 4                                ",
                                "\n AND b.folio = ", v_r_agrupador.folio                ,
                                "\n AND e.afore_cod = ", v_r_agrupador.afore_cod        ,
                                "\n AND c.f_carga = '", v_r_agrupador.f_carga,       "'"
                  
                  -- si se recibio nss 
                  IF ( v_id_derechohabiente IS NOT NULL ) THEN 
                     LET v_query = v_query, "\n AND b.id_derechohabiente = ", v_id_derechohabiente
                  END IF

                  PREPARE sid_detalle FROM v_query
                  DECLARE cur_detalle  CURSOR FOR sid_detalle
                  
                  
                  --llena el arreglo        
                  LET v_indice = 1
                  
                  FOREACH cur_detalle INTO arr_reg_sds.*

                     LET v_es_valido = FALSE
                     --Reviso que el registro sea valido en base a la condicion de variacion
                     IF v_variacion = 2 OR v_variacion = 3 THEN
                        LET v_aivs_diferencia = arr_reg_sds.aivs97_sol - (arr_reg_sds.aivs97_sdo + arr_reg_sds.aivsvol_sdo)
                        --DISPLAY "NSS: ",arr_reg_sds.nss, " Diferencia: ",v_aivs_diferencia
                        CASE v_variacion
                           WHEN 2
                              IF v_aivs_diferencia < 1000 THEN
                                 LET v_es_valido = TRUE
                              END IF
                           WHEN 3
                              IF v_aivs_diferencia >= 1000 THEN
                                 LET v_es_valido = TRUE
                              END IF
                        END CASE
                     ELSE
                        LET v_es_valido = TRUE
                     END IF

                     IF v_es_valido THEN
                  
                        LET arr_reg_saldos[v_indice].v_folio                     = arr_reg_sds.folio              
                        LET arr_reg_saldos[v_indice].v_f_carga                   = arr_reg_sds.f_carga
                        LET v_afore_cod                                          = arr_reg_sds.afore_cod
                        LET v_afore_desc                                         = arr_reg_sds.afore_desc
                        LET arr_reg_saldos[v_indice].v_nss                       = arr_reg_sds.nss
                        LET v_nombre_trab                                        = arr_reg_sds.nombre_trab
                        LET v_paterno_trab                                       = arr_reg_sds.paterno_trab
                        LET v_materno_trab                                       = arr_reg_sds.materno_trab
                        LET arr_reg_saldos[v_indice].v_aivs97_sol                = arr_reg_sds.aivs97_sol
                        LET v_precio_fondo                                       = arr_reg_sds.precio_fondo
                        LET arr_reg_saldos[v_indice].v_aivs97_sdo                = arr_reg_sds.aivs97_sdo
                        LET arr_reg_saldos[v_indice].v_aivs97_sdo_pesos          = arr_reg_sds.aivs97_sdo_pesos
                        LET arr_reg_saldos[v_indice].v_aivsvol_sdo               = arr_reg_sds.aivsvol_sdo
                        LET arr_reg_saldos[v_indice].v_aivsvol_sdo_pesos         = arr_reg_sds.aivsvol_sdo_pesos

                        LET arr_reg_saldos[v_indice].v_aivs97_sol_pesos          = arr_reg_saldos[v_indice].v_aivs97_sol  * v_precio_fondo  
                        LET arr_reg_saldos[v_indice].v_aivs97_sdo_pesos          = arr_reg_saldos[v_indice].v_aivs97_sdo  * v_precio_fondo
                        LET arr_reg_saldos[v_indice].v_aivsvol_sdo_pesos         = arr_reg_saldos[v_indice].v_aivsvol_sdo * v_precio_fondo

                        LET arr_reg_saldos[v_indice].v_aivssum97vol_sdo          = arr_reg_saldos[v_indice].v_aivs97_sdo       + arr_reg_saldos[v_indice].v_aivsvol_sdo
                        LET arr_reg_saldos[v_indice].v_aivssum97vol_sdo_pesos    = arr_reg_saldos[v_indice].v_aivs97_sdo_pesos + arr_reg_saldos[v_indice].v_aivsvol_sdo_pesos
                        LET arr_reg_saldos[v_indice].v_afore                     = v_afore_cod ||" - "|| v_afore_desc
                        LET arr_reg_saldos[v_indice].v_nombre_trabajador         = v_nombre_trab CLIPPED||" "||v_paterno_trab CLIPPED ||" "||v_materno_trab CLIPPED   
                        LET arr_reg_saldos[v_indice].v_aivs97_dif                = arr_reg_saldos[v_indice].v_aivs97_sol       - arr_reg_saldos[v_indice].v_aivssum97vol_sdo
                        LET arr_reg_saldos[v_indice].v_aivs97_dif_pesos          = arr_reg_saldos[v_indice].v_aivs97_sol_pesos - arr_reg_saldos[v_indice].v_aivssum97vol_sdo_pesos
                     
                        LET v_indice = v_indice + 1
                        
                     END IF
                     
                  END FOREACH
                  
                  -- se borra el ultimo registro
                  --CALL arr_reg_saldos.deleteElement(arr_reg_saldos.getLength())
            END DISPLAY
            
            DISPLAY ARRAY arr_reg_saldos TO r_saldos_insuf.*
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
                LET v_nom_archivo = "TRAN_", p_usuario_cod CLIPPED, "_", TODAY USING "yyyymmdd"
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
                LET v_s_detalle = TODAY USING "yyyymmdd", "|CVE AFORE|NSS|NOMBRE|AIVS 97 SOLIC|",
                                  "PESOS 97 SOLIC|AIVS 97 SALDO|AIVS VOL SALDO|AIVS 97 Y VOL SALDO|PESOS 97 SALDO|",
                                  "PESOS VOL SALDO|PESOS 97 Y VOL SALDO|AIVS 97 DIF|PESOS 97 DIF|"

                CALL v_ch_arch_ret_generico.write(v_s_detalle)
                -- se inicia el contador de registros
                LET v_conteo = 0
                --- Se deben exportar todos los agrupadores al archivo

                LET v_query = "\n SELECT b.folio                                    ,",                                    
                             "\n        c.f_carga                                  ,",                                  
                             "\n        e.afore_cod, e.afore_desc                  ,",                  
                             "\n        a.nss                                      ,",                                      
                             "\n        b.nombre_afore                             ,",                             
                             "\n        b.paterno_afore                            ,",                            
                             "\n        b.materno_afore                            ,",                           
                             "\n        b.aivs_viv97                               ,",                               
                             "\n        c.precio_fondo                             ,",                             
                             "\n        NVL(d.saldo_acciones, 0)                   ,",                      
                             "\n        NVL(d.saldo_pesos, 0)                      ,",
                             "\n        NVL(f.saldo_acciones, 0)                   ,",                      
                             "\n        NVL(f.saldo_pesos, 0)                       ",
                             "\n FROM afi_derechohabiente a                        ,",                        
                             "\n      ret_transferencia b                          ,",                          
                             "\n      ret_cza_transferencia c                      ,",                      
                             "\n      ret_his_saldo d                               ",                            
                             "\n      LEFT OUTER JOIN ret_his_saldo f               ",
                             "\n                   ON f.id_solicitud = d.id_solicitud ",
                             "\n                  AND f.subcuenta = 55             ,",
                             "\n      cat_afore e                                   ",
                             "\n WHERE b.id_derechohabiente = a.id_derechohabiente  ",
                             "\n AND b.cve_afore = e.afore_cod                      ",
                             "\n AND b.folio = c.folio                              ",
                             "\n AND d.subcuenta = 4                                ",
                             "\n AND b.id_solicitud = d.id_solicitud                "
               
               -- si se recibio el folio como parametro
               IF ( v_folio IS NOT NULL AND v_folio > 0 ) THEN
                  -- agrupador
                  LET v_query = v_query, "\n AND b.folio= ", v_folio                   
               END IF
               
               -- si se recibio el afore como parametro
               IF ( v_afore IS NOT NULL AND v_afore > 0 ) THEN
                  -- agrupador
                  LET v_query = v_query, "\n AND b.cve_afore= ", v_afore                   
               END IF
               
               -- si se recibieron la fecha inicial y la fecha final como parametro
               IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
                  -- agrupador
                  LET v_query = v_query, "\n AND c.f_carga BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
               END IF
               
               IF ( v_id_derechohabiente IS NOT NULL ) THEN 
                  LET v_query = v_query, "\n AND afi.id_derechohabiente = ", v_id_derechohabiente
               END IF 
               
               -- se ordenan los datos
               LET v_query = v_query, "\n ORDER BY b.folio, c.f_carga, e.afore_cod, a.nss"
               
               -- se obtienen los registros para la exportacion
               PREPARE sid_export FROM v_query
               DECLARE cur_export CURSOR FOR sid_export
               
               -- llena el arreglo
               LET v_indice = 1
               
               FOREACH cur_export INTO arr_reg_sds.*

                  LET v_es_valido = FALSE
                  --Reviso que el registro sea valido en base a la condicion de variacion
                  IF v_variacion = 2 OR v_variacion = 3 THEN
                     LET v_aivs_diferencia = arr_reg_sds.aivs97_sol - (arr_reg_sds.aivs97_sdo + arr_reg_sds.aivsvol_sdo)
                     --DISPLAY "NSS: ",arr_reg_sds.nss, " Diferencia: ",v_aivs_diferencia
                     CASE v_variacion
                        WHEN 2
                           IF v_aivs_diferencia < 1000 THEN
                              LET v_es_valido = TRUE
                           END IF
                        WHEN 3
                           IF v_aivs_diferencia >= 1000 THEN
                              LET v_es_valido = TRUE
                           END IF
                     END CASE
                  ELSE
                     LET v_es_valido = TRUE
                  END IF

                  IF v_es_valido THEN

                     LET arr_export[v_indice].v_folio                      = arr_reg_sds.folio              
                     LET arr_reg_saldos[v_indice].v_f_carga                = arr_reg_sds.f_carga
                     LET v_afore_cod                                       = arr_reg_sds.afore_cod
                     LET v_afore_desc                                      = arr_reg_sds.afore_desc
                     LET arr_export[v_indice].v_nss                        = arr_reg_sds.nss
                     LET v_nombre_trab                                     = arr_reg_sds.nombre_trab
                     LET v_paterno_trab                                    = arr_reg_sds.paterno_trab
                     LET v_materno_trab                                    = arr_reg_sds.materno_trab
                     LET arr_export[v_indice].v_aivs97_sol                 = arr_reg_sds.aivs97_sol
                     LET v_precio_fondo                                    = arr_reg_sds.precio_fondo
                     LET arr_export[v_indice].v_aivs97_sdo                 = arr_reg_sds.aivs97_sdo
                     LET arr_export[v_indice].v_aivs97_sdo_pesos           = arr_reg_sds.aivs97_sdo_pesos
                     LET arr_export[v_indice].v_aivsvol_sdo                = arr_reg_sds.aivsvol_sdo
                     LET arr_export[v_indice].v_aivsvol_sdo_pesos          = arr_reg_sds.aivsvol_sdo_pesos

                     LET arr_export[v_indice].v_aivs97_sol_pesos  = arr_export[v_indice].v_aivs97_sol  * v_precio_fondo  
                     LET arr_export[v_indice].v_aivs97_sdo_pesos  = arr_export[v_indice].v_aivs97_sdo  * v_precio_fondo
                     LET arr_export[v_indice].v_aivsvol_sdo_pesos = arr_export[v_indice].v_aivsvol_sdo * v_precio_fondo


                     LET arr_export[v_indice].v_aivssum97vol_sdo        = arr_export[v_indice].v_aivs97_sdo       + arr_export[v_indice].v_aivsvol_sdo
                     LET arr_export[v_indice].v_aivssum97vol_sdo_pesos  = arr_export[v_indice].v_aivs97_sdo_pesos + arr_export[v_indice].v_aivsvol_sdo_pesos
                        
                     LET arr_export[v_indice].v_afore             = v_afore_cod ||" - "|| v_afore_desc
                     LET arr_export[v_indice].v_nombre_trabajador = v_nombre_trab CLIPPED||" "||v_paterno_trab CLIPPED ||" "||v_materno_trab CLIPPED   
                     LET arr_export[v_indice].v_aivs97_dif        = arr_export[v_indice].v_aivs97_sol       - arr_export[v_indice].v_aivssum97vol_sdo
                     LET arr_export[v_indice].v_aivs97_dif_pesos  = arr_export[v_indice].v_aivs97_sol_pesos - arr_export[v_indice].v_aivssum97vol_sdo_pesos
               
                 
                
                     LET v_s_detalle = v_afore_cod                                                        ,"|",
                                       arr_export[v_indice].v_nss                                         ,"|",
                                       arr_export[v_indice].v_nombre_trabajador                           ,"|",
                                       arr_export[v_indice].v_aivs97_sol USING "&&&&&&&&&.&&&&&&"         ,"|",
                                       arr_export[v_indice].v_aivs97_sol_pesos USING "&&&&&&&&&.&&"       ,"|",
                                       arr_export[v_indice].v_aivs97_sdo USING "&&&&&&&&&.&&&&&&"         ,"|",
                                       arr_export[v_indice].v_aivsvol_sdo USING "&&&&&&&&&.&&&&&&"        ,"|",
                                       arr_export[v_indice].v_aivssum97vol_sdo USING "&&&&&&&&&.&&&&&&"   ,"|",
                                       arr_export[v_indice].v_aivs97_sdo_pesos USING "&&&&&&&&&.&&"       ,"|",
                                       arr_export[v_indice].v_aivsvol_sdo_pesos USING "&&&&&&&&&.&&"      ,"|",
                                       arr_export[v_indice].v_aivssum97vol_sdo_pesos USING "&&&&&&&&&.&&" ,"|",
                                       arr_export[v_indice].v_aivs97_dif USING "&&&&&&&&&.&&&&&&"         ,"|",
                                       arr_export[v_indice].v_aivs97_dif_pesos USING "&&&&&&&&&.&&"       ,"|"
                                      
                     CALL v_ch_arch_ret_generico.write(v_s_detalle)

                     LET v_indice = v_indice + 1

                  END IF
                 
               END FOREACH

                -- escribe el sumario
                LET v_s_detalle = TODAY USING "yyyymmdd", "|",(v_indice - 1) USING "&&&&&&&&&&" ,"|"
                CALL v_ch_arch_ret_generico.write(v_s_detalle)

                -- se cierra el archivo
                CALL v_ch_arch_ret_generico.close()
                LET v_mensaje_archivo = "Archivo generado exitosamente:", v_v_ruta_nomarch
                CALL fn_mensaje("Atención", v_mensaje_archivo, "information")



               
            ON ACTION reporte                      
               -- se consulta del detalle de este agrupador
               LET v_query = "\n SELECT b.folio                                    ,",                                    
                             "\n        c.f_carga                                  ,",                                  
                             "\n        e.afore_cod, e.afore_desc                  ,",                  
                             "\n        a.nss                                      ,",                                      
                             "\n        b.nombre_afore                             ,",                             
                             "\n        b.paterno_afore                            ,",                            
                             "\n        b.materno_afore                            ,",                           
                             "\n        b.aivs_viv97                               ,",                               
                             "\n        c.precio_fondo                             ,",                             
                             "\n        NVL(d.saldo_acciones, 0)                   ,",                      
                             "\n        NVL(d.saldo_pesos, 0)                      ,",
                             "\n        NVL(f.saldo_acciones, 0)                   ,",                      
                             "\n        NVL(f.saldo_pesos, 0)                       ",
                             "\n FROM afi_derechohabiente a                        ,",                        
                             "\n      ret_transferencia b                          ,",                          
                             "\n      ret_cza_transferencia c                      ,",                      
                             "\n      ret_his_saldo d                               ",                            
                             "\n      LEFT OUTER JOIN ret_his_saldo f               ",
                             "\n                   ON f.id_solicitud = d.id_solicitud ",
                             "\n                  AND f.subcuenta = 55             ,",
                             "\n      cat_afore e                                   ",
                             "\n WHERE b.id_derechohabiente = a.id_derechohabiente  ",
                             "\n AND b.cve_afore = e.afore_cod                      ",
                             "\n AND b.folio = c.folio                              ",
                             "\n AND b.id_solicitud = d.id_solicitud                "
               
               -- si se recibio el folio como parametro
               IF ( v_folio IS NOT NULL AND v_folio > 0 ) THEN
                  -- agrupador
                  LET v_query = v_query, "\n AND b.folio= ", v_folio                   
               END IF
               
               -- si se recibio el afore como parametro
               IF ( v_afore IS NOT NULL AND v_afore > 0 ) THEN
                  -- agrupador
                  LET v_query = v_query, "\n AND b.cve_afore= ", v_afore                   
               END IF
               
               -- si se recibieron la fecha inicial y la fecha final como parametro
               IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
                  -- agrupador
                  LET v_query = v_query, "\n AND c.f_carga BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
               END IF
               
               IF ( v_id_derechohabiente IS NOT NULL ) THEN 
                  LET v_query = v_query, "\n AND afi.id_derechohabiente = ", v_id_derechohabiente
               END IF 
               
               -- se ordenan los datos
               LET v_query = v_query, "\n ORDER BY b.folio, c.f_carga, e.afore_cod, a.nss"
               
               -- se obtienen los registros para el reporte
               PREPARE sid_reporte FROM v_query
               DECLARE cur_reporte CURSOR FOR sid_reporte
               
               -- llena el arreglo
               LET v_indice = 1

               FOREACH cur_reporte INTO arr_reg_sds.*

                  LET v_es_valido = FALSE
                  --Reviso que el registro sea valido en base a la condicion de variacion
                  IF v_variacion = 2 OR v_variacion = 3 THEN
                     LET v_aivs_diferencia = arr_reg_sds.aivs97_sol - (arr_reg_sds.aivs97_sdo + arr_reg_sds.aivsvol_sdo)
                     --DISPLAY "NSS: ",arr_reg_sds.nss, " Diferencia: ",v_aivs_diferencia
                     CASE v_variacion
                        WHEN 2
                           IF v_aivs_diferencia < 1000 THEN
                              LET v_es_valido = TRUE
                           END IF
                        WHEN 3
                           IF v_aivs_diferencia >= 1000 THEN
                              LET v_es_valido = TRUE
                           END IF
                     END CASE
                  ELSE
                     LET v_es_valido = TRUE
                  END IF

                  IF v_es_valido THEN

                     LET arr_reporte[v_indice].v_folio                     = arr_reg_sds.folio              
                     LET arr_reporte[v_indice].v_f_carga                   = arr_reg_sds.f_carga
                     LET v_afore_cod                                       = arr_reg_sds.afore_cod
                     LET v_afore_desc                                      = arr_reg_sds.afore_desc
                     LET arr_reporte[v_indice].v_nss                       = arr_reg_sds.nss
                     LET v_nombre_trab                                     = arr_reg_sds.nombre_trab
                     LET v_paterno_trab                                    = arr_reg_sds.paterno_trab
                     LET v_materno_trab                                    = arr_reg_sds.materno_trab
                     LET arr_reporte[v_indice].v_aivs97_sol                = arr_reg_sds.aivs97_sol
                     LET v_precio_fondo                                    = arr_reg_sds.precio_fondo
                     LET arr_reporte[v_indice].v_aivs97_sdo                = arr_reg_sds.aivs97_sdo
                     LET arr_reporte[v_indice].v_aivs97_sdo_pesos          = arr_reg_sds.aivs97_sdo_pesos
                     LET arr_reporte[v_indice].v_aivsvol_sdo               = arr_reg_sds.aivsvol_sdo
                     LET arr_reporte[v_indice].v_aivsvol_sdo_pesos         = arr_reg_sds.aivsvol_sdo_pesos

                     LET arr_reporte[v_indice].v_aivs97_sol_pesos  = arr_reporte[v_indice].v_aivs97_sol  * v_precio_fondo  
                     LET arr_reporte[v_indice].v_aivs97_sdo_pesos  = arr_reporte[v_indice].v_aivs97_sdo  * v_precio_fondo
                     LET arr_reporte[v_indice].v_aivsvol_sdo_pesos = arr_reporte[v_indice].v_aivsvol_sdo * v_precio_fondo


                     LET arr_reporte[v_indice].v_aivssum97vol_sdo        = arr_reporte[v_indice].v_aivs97_sdo       + arr_reporte[v_indice].v_aivsvol_sdo
                     LET arr_reporte[v_indice].v_aivssum97vol_sdo_pesos  = arr_reporte[v_indice].v_aivs97_sdo_pesos + arr_reporte[v_indice].v_aivsvol_sdo_pesos

                     
                     LET arr_reporte[v_indice].v_afore             = v_afore_cod ||" - "|| v_afore_desc
                     LET arr_reporte[v_indice].v_nombre_trabajador = v_nombre_trab CLIPPED||" "||v_paterno_trab CLIPPED ||" "||v_materno_trab CLIPPED   
                     LET arr_reporte[v_indice].v_aivs97_dif        = arr_reporte[v_indice].v_aivs97_sol       - arr_reporte[v_indice].v_aivssum97vol_sdo
                     LET arr_reporte[v_indice].v_aivs97_dif_pesos  = arr_reporte[v_indice].v_aivs97_sol_pesos - arr_reporte[v_indice].v_aivssum97vol_sdo_pesos
               
                     LET v_indice = v_indice + 1

                  END IF
               
               END FOREACH

               -- Recupera la ruta de listados en el que se enviara el archivo
               CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
 
               -- Se asigna la plantilla para generar el reporte
               IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETC33.4rp") ) THEN 
                   CALL fgl_report_selectDevice ("PDF")
                     
                   LET v_ruta_reporte = v_ruta_ejecutable CLIPPED,"/","detalle_saldos_insuf"                
                   CALL fgl_report_setOutputFileName(v_ruta_reporte)
                   CALL fgl_report_selectPreview(1)
                   LET manejador_rpt = fgl_report_commitCurrentSettings()
               ELSE         
                   CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla RETC33.4rp", "stop")
                   CONTINUE DIALOG
               END IF   

               --Inicia el reporte de registros con rechazo
               START REPORT rpt_detalle_saldos_insuf TO XML HANDLER manejador_rpt
               
               FOR v_indice_reporte = 1 TO arr_reporte.getLength()
               
                   LET v_agrupador_folio_fecha_Afore = arr_reporte[v_indice_reporte].v_afore, arr_reporte[v_indice_reporte].v_folio USING "&&&&&&&&&", arr_reporte[v_indice_reporte].v_f_carga USING "ddmmyyyy"
               
                   OUTPUT TO REPORT rpt_detalle_saldos_insuf(v_indice_reporte, arr_reporte[v_indice_reporte].*, p_usuario_cod, v_agrupador_folio_fecha_Afore)
               END FOR
               
               FINISH REPORT rpt_detalle_saldos_insuf

         END DIALOG
         
         CLOSE WINDOW w_detalle_consulta_saldos
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_saldos

END FUNCTION

{ ======================================================================
Clave: PAGC05
Nombre: rpt_detalle_saldos_insuf
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de los saldos insuficientes 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_detalle_saldos_insuf(p_total_regs, v_r_despliegue, p_usuario_cod, p_agrupador_folio_fecha_Afore)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
           v_folio                  LIKE ret_transferencia.folio,
           v_f_carga                LIKE ret_cza_transferencia.f_carga,
           v_afore                  STRING,
           v_nss                    LIKE afi_derechohabiente.nss,
           v_nombre_trabajador      STRING, 
           v_aivs97_sol             DECIMAL(24,6),
           v_aivs97_sol_pesos       DECIMAL(22,2),
           v_aivs97_sdo             DECIMAL(24,6),
           v_aivs97_sdo_pesos       DECIMAL(22,2),
           v_aivsvol_sdo            DECIMAL(24,6),
           v_aivsvol_sdo_pesos      DECIMAL(22,2),
           v_aivssum97vol_sdo       DECIMAL(24,6),
           v_aivssum97vol_sdo_pesos DECIMAL(22,2),
           v_aivs97_dif             DECIMAL(24,6),
           v_aivs97_dif_pesos       DECIMAL(22,2)
          END RECORD,
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para acumular TOTAL PARCIAL
          v_total_afore_aivs97_sol             DECIMAL(24,6),
          v_total_afore_aivs97_sol_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_sdo             DECIMAL(24,6),
          v_total_afore_aivs97_sdo_pesos       DECIMAL(22,2),
          v_total_afore_aivsvol_sdo            DECIMAL(24,6),
          v_total_afore_aivsvol_sdo_pesos      DECIMAL(22,2),
          v_total_afore_aivssum97vol_sdo       DECIMAL(24,6),
          v_total_afore_aivssum97vol_sdo_pesos DECIMAL(22,2),
          v_total_afore_aivs97_dif             DECIMAL(24,6),
          v_total_afore_aivs97_dif_pesos       DECIMAL(22,2),
          p_total_afore_regs                   DECIMAL(9,0),
          -- variables para acumular GRAN TOTAL
          v_total_aivs97_sol             DECIMAL(24,6),
          v_total_aivs97_sol_pesos       DECIMAL(22,2),
          v_total_aivs97_sdo             DECIMAL(24,6),
          v_total_aivs97_sdo_pesos       DECIMAL(22,2),
          v_total_aivsvol_sdo            DECIMAL(24,6),
          v_total_aivsvol_sdo_pesos      DECIMAL(22,2),
          v_total_aivssum97vol_sdo       DECIMAL(24,6),
          v_total_aivssum97vol_sdo_pesos DECIMAL(22,2),
          v_total_aivs97_dif             DECIMAL(24,6),
          v_total_aivs97_dif_pesos       DECIMAL(22,2),
          p_total_regs                   DECIMAL(9,0),
          v_fecha_carga                  STRING,
          p_agrupador_folio_fecha_Afore  STRING
          
FORMAT

   FIRST PAGE HEADER
      
      -- variables para acumular gran total
      LET v_total_aivs97_sol             = 0 
      LET v_total_aivs97_sol_pesos       = 0
      LET v_total_aivs97_sdo             = 0
      LET v_total_aivs97_sdo_pesos       = 0
      LET v_total_aivsvol_sdo            = 0
      LET v_total_aivsvol_sdo_pesos      = 0
      LET v_total_aivssum97vol_sdo       = 0
      LET v_total_aivssum97vol_sdo_pesos = 0
      LET v_total_aivs97_dif             = 0
      LET v_total_aivs97_dif_pesos       = 0
      
      -- variables para acumular por afore, fecha y folio
      LET v_total_afore_aivs97_sol             = 0
      LET v_total_afore_aivs97_sol_pesos       = 0
      LET v_total_afore_aivs97_sdo             = 0
      LET v_total_afore_aivs97_sdo_pesos       = 0
      LET v_total_afore_aivsvol_sdo            = 0
      LET v_total_afore_aivsvol_sdo_pesos      = 0
      LET v_total_afore_aivssum97vol_sdo       = 0
      LET v_total_afore_aivssum97vol_sdo_pesos = 0
      LET v_total_afore_aivs97_dif             = 0
      LET v_total_afore_aivs97_dif_pesos       = 0
      LET p_total_afore_regs                   = 0

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
      LET v_total_afore_aivs97_sol             = 0
      LET v_total_afore_aivs97_sol_pesos       = 0
      LET v_total_afore_aivs97_sdo             = 0
      LET v_total_afore_aivs97_sdo_pesos       = 0
      LET v_total_afore_aivsvol_sdo            = 0
      LET v_total_afore_aivsvol_sdo_pesos      = 0
      LET v_total_afore_aivssum97vol_sdo       = 0
      LET v_total_afore_aivssum97vol_sdo_pesos = 0
      LET v_total_afore_aivs97_dif             = 0
      LET v_total_afore_aivs97_dif_pesos       = 0
      LET p_total_afore_regs                   = 0
   
      PRINTX v_r_despliegue.v_afore,
             v_r_despliegue.v_folio,
             v_fecha_carga
             

   ON EVERY ROW
      PRINTX v_r_despliegue.*
      DISPLAY v_r_despliegue.*
      
      -- se acumulan los montos para total por afore, folio y fecha de carga
      LET v_total_afore_aivs97_sol             = v_total_afore_aivs97_sol             + v_r_despliegue.v_aivs97_sol      
      LET v_total_afore_aivs97_sol_pesos       = v_total_afore_aivs97_sol_pesos       + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_afore_aivs97_sdo             = v_total_afore_aivs97_sdo             + v_r_despliegue.v_aivs97_sdo      
      LET v_total_afore_aivs97_sdo_pesos       = v_total_afore_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_afore_aivsvol_sdo            = v_total_afore_aivsvol_sdo            + v_r_despliegue.v_aivsvol_sdo      
      LET v_total_afore_aivsvol_sdo_pesos      = v_total_afore_aivsvol_sdo_pesos      + v_r_despliegue.v_aivsvol_sdo_pesos
      LET v_total_afore_aivssum97vol_sdo       = v_total_afore_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_afore_aivssum97vol_sdo_pesos = v_total_afore_aivssum97vol_sdo_pesos + v_r_despliegue.v_aivssum97vol_sdo_pesos
      LET v_total_afore_aivs97_dif             = v_total_afore_aivs97_dif             + v_r_despliegue.v_aivs97_dif      
      LET v_total_afore_aivs97_dif_pesos       = v_total_afore_aivs97_dif_pesos       + v_r_despliegue.v_aivs97_dif_pesos
      LET p_total_afore_regs                   = p_total_afore_regs                   + 1
      
      
      -- se acumulan los montos para gran total
      LET v_total_aivs97_sol             = v_total_aivs97_sol             + v_r_despliegue.v_aivs97_sol      
      LET v_total_aivs97_sol_pesos       = v_total_aivs97_sol_pesos       + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_aivs97_sdo             = v_total_aivs97_sdo             + v_r_despliegue.v_aivs97_sdo      
      LET v_total_aivs97_sdo_pesos       = v_total_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_aivsvol_sdo            = v_total_aivsvol_sdo            + v_r_despliegue.v_aivsvol_sdo      
      LET v_total_aivsvol_sdo_pesos      = v_total_aivsvol_sdo_pesos      + v_r_despliegue.v_aivsvol_sdo_pesos
      LET v_total_aivssum97vol_sdo       = v_total_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_aivssum97vol_sdo_pesos = v_total_aivssum97vol_sdo_pesos + v_r_despliegue.v_aivssum97vol_sdo_pesos
      LET v_total_aivs97_dif             = v_total_aivs97_dif             + v_r_despliegue.v_aivs97_dif      
      LET v_total_aivs97_dif_pesos       = v_total_aivs97_dif_pesos       + v_r_despliegue.v_aivs97_dif_pesos

   --AFTER GROUP OF v_r_despliegue.v_afore
   AFTER GROUP OF p_agrupador_folio_fecha_Afore
      PRINTX v_total_afore_aivs97_sol             ,
             v_total_afore_aivs97_sol_pesos       ,
             v_total_afore_aivs97_sdo             ,
             v_total_afore_aivs97_sdo_pesos       ,
             v_total_afore_aivsvol_sdo            ,
             v_total_afore_aivsvol_sdo_pesos      ,
             v_total_afore_aivssum97vol_sdo       ,
             v_total_afore_aivssum97vol_sdo_pesos ,
             v_total_afore_aivs97_dif             ,
             v_total_afore_aivs97_dif_pesos       ,
             p_total_afore_regs            
                                                          
   
   ON LAST ROW
      PRINTX p_total_regs                   ,
             v_total_aivs97_sol             ,
             v_total_aivs97_sol_pesos       ,
             v_total_aivs97_sdo             ,
             v_total_aivs97_sdo_pesos       ,
             v_total_aivsvol_sdo            ,
             v_total_aivsvol_sdo_pesos      ,
             v_total_aivssum97vol_sdo       ,
             v_total_aivssum97vol_sdo_pesos ,
             v_total_aivs97_dif             ,
             v_total_aivs97_dif_pesos 

END REPORT
