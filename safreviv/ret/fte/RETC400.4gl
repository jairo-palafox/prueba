################################################################################
#Modulo        => RET                                                          #
#Programa      => RETC398                                                      #
#Ojetivo       => Realizar consultas de solicitudes de Retiro Ley 73 Grupo 1,  #
#                 de notificacion                                              #
#Fecha inicio  => Noviembre, 2015.                                             #
#Requerimiento => PRODINF-845                                                  #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

MAIN

   -- Parametros pasados al mandar llamar el programa
   DEFINE p_usuario            CHAR(20)    

   -- Variables usadas para realizar la busqueda
   DEFINE v_nss                STRING
   DEFINE v_ventanilla         SMALLINT
   DEFINE v_rechazo            SMALLINT
   DEFINE v_afore              SMALLINT
   DEFINE v_fecha_not_inicio   DATE
   DEFINE v_fecha_not_fin      DATE
   DEFINE v_fecha_pago_inicio  DATE
   DEFINE v_fecha_pago_fin     DATE

   -- Variables auxiliares
   DEFINE v_descripcion_rechazo  CHAR(40)
   DEFINE v_descripcion_afore    CHAR(40)
   DEFINE v_sql                  STRING
   DEFINE v_contador             INTEGER

   -- Combobox para la busqueda
   DEFINE cbx_ventanilla       ui.ComboBox
   DEFINE cbx_rechazo          ui.ComboBox
   DEFINE cbx_afore            ui.ComboBox

   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo          STRING   -- titulo de la ventana 

   --Se obtienen los valores de la ejecucion
   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   --Se llama a la funcion que desplegara la ventana para introducir los 
   --parametros de la consulta
   OPEN WINDOW busqueda_marcas WITH FORM "RETC4001"

      -- Se preparan los combobox
      LET cbx_ventanilla = ui.ComboBox.forName("v_ventanilla")
      CALL cbx_ventanilla.clear()
      LET cbx_rechazo = ui.ComboBox.forName("v_rechazo")
      CALL cbx_rechazo.clear()
      LET cbx_afore = ui.ComboBox.forName("v_afore")
      CALL cbx_afore.clear()

      -- Se establece ventanilla
      CALL cbx_ventanilla.addItem(101 ,"INFONAVIT")
      CALL cbx_ventanilla.addItem(201 ,"AFORE"    )
      CALL cbx_ventanilla.addItem(NULL,"AMBAS"    )

      -- Se obtienen los codigos de rechazo       
      LET v_sql = "SELECT (cod_rechazo-1000),des_corta  "||
                 "FROM   ret_rechazo_generico          "||
                 "WHERE  cod_rechazo > 1000            "||
                 "AND    (cod_rechazo - 1000) IN (102, "||
                 "119,129,130,131,132,133,134,135,136, "||
                 "137,138,139,141,144,145,146,147,148, "||
                 "149,150,151,203,998,999)             "||
                 "ORDER  BY cod_rechazo ASC            "

      PREPARE prp_rechazo FROM v_sql
      DECLARE cur_rechazo CURSOR FOR prp_rechazo

      LET v_contador = 0

      FOREACH cur_rechazo INTO v_rechazo,v_descripcion_rechazo
         CALL cbx_rechazo.addItem(v_rechazo,v_rechazo||" - "||v_descripcion_rechazo)
         LET v_contador = v_contador + 1
      END FOREACH
      IF v_contador > 0  THEN
         CALL cbx_rechazo.removeItem(v_contador)
      END IF
      CALL cbx_rechazo.addItem(208,'208 - SIN MARCA DE DISPOSICIÓN DE RECURSOS')

      CALL cbx_rechazo.addItem(101   ,"101 - SOLICITUD ACEPTADA")
      CALL cbx_rechazo.addItem(NULL,"TODAS"   )

      -- Se obtienen las afores
      LET v_sql = "SELECT afore_cod,afore_desc  "||
                 "FROM   cat_afore             "||
                 "WHERE  afore_cod IN (530,534,"||
                 "       538,544,550,552,556,  "||
                 "       562,564,568,578)      "||
                 "ORDER  BY afore_cod ASC      "

      PREPARE prp_afore FROM v_sql
      DECLARE cur_afore CURSOR FOR prp_afore

      LET v_contador = 0

      FOREACH cur_afore INTO v_ventanilla,v_descripcion_afore
         CALL cbx_afore.addItem(v_ventanilla,v_ventanilla||" - "||v_descripcion_afore)
         LET v_contador = v_contador + 1
      END FOREACH

      IF v_contador > 0  THEN
         CALL cbx_afore.removeItem(v_contador)
      END IF

      CALL cbx_afore.addItem(NULL,"TODAS")

      INPUT BY NAME  v_nss,v_ventanilla,v_rechazo,v_afore,v_fecha_not_inicio,
                       v_fecha_not_fin,v_fecha_pago_inicio,v_fecha_pago_fin
                       ATTRIBUTES(UNBUFFERED,CANCEL = 0)

         ON ACTION ACCEPT
            --Se valida que el NSS sea un numero de 11 digitos
            IF v_nss IS NULL AND 
               v_ventanilla IS NULL AND 
               v_rechazo IS NULL AND 
               v_afore IS NULL AND 
               v_fecha_not_inicio IS NULL AND
               v_fecha_not_fin IS NULL AND 
               v_fecha_pago_inicio IS NULL AND 
               v_fecha_pago_fin THEN 
               CALL fn_mensaje("Atención",
                              "Debe al menos ingresar valor en un filtro para realizar la consulta.",
                              "stop")
               NEXT FIELD v_nss
            END IF 
            {DISPLAY v_nss
            DISPLAY v_ventanilla
            DISPLAY v_rechazo
            DISPLAY v_afore
            DISPLAY v_fecha_inicio
            DISPLAY v_fecha_fin}

            CALL fn_busca_solicitudes(v_nss,v_ventanilla,v_rechazo,v_afore,
                                    v_fecha_not_inicio,v_fecha_not_fin,
                                    v_fecha_pago_inicio,v_fecha_pago_fin)
         ON ACTION Salir
            IF fn_ventana_confirma("Atención","¿Desea salir?","quest") = 1 THEN
               EXIT INPUT
            END IF

      END INPUT

   CLOSE WINDOW busqueda_marcas

END MAIN

#Funcion que valida si la cadena que se le pasa es un numero
FUNCTION fn_numero_valido(p_cadena)

   DEFINE p_cadena     STRING
   DEFINE v_entero     BIGINT

   TRY
      LET v_entero = p_cadena
      RETURN TRUE
   CATCH
      RETURN FALSE
   END TRY

END FUNCTION

#Funcion que realiza la busqueda de las solicitudes y las imprime
FUNCTION fn_busca_solicitudes(p_nss,p_ventanilla,p_rechazo,p_afore,
                              p_fecha_not_inicio,p_fecha_not_fin,
                              p_fecha_pago_inicio,p_fecha_pago_fin)

   -- Parametros para realizar la busqueda
   DEFINE p_nss                STRING
   DEFINE p_ventanilla         SMALLINT
   DEFINE p_rechazo            SMALLINT
   DEFINE p_afore              SMALLINT
   DEFINE p_fecha_not_inicio   DATE
   DEFINE p_fecha_not_fin      DATE
   DEFINE p_fecha_pago_inicio  DATE
   DEFINE p_fecha_pago_fin     DATE
   DEFINE v_resultado          SMALLINT 
   DEFINE v_porcentaje         SMALLINT

   -- Variables auxiliares
   DEFINE v_contador       INTEGER
   DEFINE v_sql            STRING

   -- Resultado de la busqueda
   DEFINE v_detalle_solicitud DYNAMIC ARRAY OF RECORD
                nss                     CHAR(11),
                estado_pago             CHAR(10),
                rechazo                 CHAR(35),
                rfc                     CHAR(13),
                curp                    CHAR(18),
                grupo_ventanilla        CHAR(15),
                afore                   CHAR(45),
                clabe                   CHAR(18),
                ind_beneficiario        CHAR(12),
                nombre_pensionado       CHAR(40),
                ap_paterno_pensionado   CHAR(40),
                ap_materno_pensionado   CHAR(40),
                rfc_beneficiario        CHAR(13),
                curp_beneficiario       CHAR(18),
                f_pago                  DATE,
                aivs_viv92              DECIMAL(13,2),
                aivs_viv97              DECIMAL(13,2),
                pesos_viv92             DECIMAL(13,2),
                pesos_viv97             DECIMAL(13,2),
                total_avis              DECIMAL(13,2),
                total_pesos             DECIMAL(13,2),
                f_notificacion          DATE
   END RECORD
   -- Resultado de la busqueda
   DEFINE v_detalle_solicitud_comp DYNAMIC ARRAY OF RECORD
             id_solicitud            DECIMAL(10,0),
             id_derechohabiente      DECIMAL(10,0)
   END RECORD
   DEFINE v_rec_detalle_solicitud RECORD
                nss                     CHAR(11),
                estado_pago             CHAR(10),
                rechazo                 CHAR(35),
                rfc                     CHAR(13),
                curp                    CHAR(18),
                grupo_ventanilla        CHAR(15),
                afore                   CHAR(45),
                clabe                   CHAR(18),
                ind_beneficiario        CHAR(12),
                nombre_pensionado       CHAR(40),
                ap_paterno_pensionado   CHAR(40),
                ap_materno_pensionado   CHAR(40),
                rfc_beneficiario        CHAR(13),
                curp_beneficiario       CHAR(18),
                f_pago                  DATE,
                aivs_viv92              DECIMAL(13,2),
                aivs_viv97              DECIMAL(13,2),
                pesos_viv92             DECIMAL(13,2),
                pesos_viv97             DECIMAL(13,2),
                total_avis              DECIMAL(13,2),
                total_pesos             DECIMAL(13,2),
                f_notificacion          DATE,
                id_solicitud            DECIMAL(10,0),
                id_derechohabiente      DECIMAL(10,0)
   END RECORD
   DEFINE v_string         base.StringBuffer
   LET v_string = base.StringBuffer.create()

   -- Se hace le cruce entre ret_ley73_generico y la tabla ret_ws_sol_retiro_vent_afore,
   -- se obtiene el grupo de ventanilla de ret_solicitud_generico y 
   LET v_sql =  "SELECT DISTINCT rnp.nss, rnp.estado_pago, '',                                        \n"||
--                "       CASE rnp.estado_pago WHEN 208 THEN 'SIN MARCA DE DISPOSICIÓN DE RECURSOS'  \n"||
--                "                            WHEN 101 THEN 'SOLICITUD ACEPTADA'                    \n"||
--                "                            ELSE UPPER(rrg.des_larga) END ,                       \n"||
                "       ad.rfc,ad.curp,CASE rsme.medio_entrega WHEN 5 THEN '0201-AFORE'            \n"||
                "                                                ELSE '0101-INFONAVIT' END,        \n"||
                "       rws.cve_afore||' - '||ca.afore_desc, '', '', '','','','','','',            \n"||
--                "       MDY(SUBSTR(rcpf.rsp_f_pago,5,2),SUBSTR(rcpf.rsp_f_pago,7,2),SUBSTR(rcpf.rsp_f_pago,1,4)) as f_pago, \n"||
                "       0,0,0,0,0,0,rnp.f_notifica,                                                \n"||
                "       rnp.id_solicitud, ad.id_derechohabiente                                    \n"||
                "FROM   ret_ws_notifica_procesar rnp,                                              \n"||
                "       ret_beneficiario_generico rbg,                                             \n"||
                "       afi_derechohabiente ad,                                                    \n"||
                "       ret_sol_medio_entrega rsme,                                                \n"||
                "       ret_solicitud_generico rsg                                                 \n"||
                "       LEFT OUTER JOIN ret_ws_sol_retiro_vent_afore rws                           \n"||
                "                    ON rsg.id_solicitud = rws.id_solicitud                        \n"||
                "       LEFT OUTER JOIN cat_afore ca                                               \n"||
                "                    ON rws.cve_afore = ca.afore_cod                               \n"||
                "WHERE  rnp.id_solicitud = rbg.id_solicitud                                        \n"||
                "AND    rnp.nss = ad.nss                                                           \n"||
                "AND    rnp.id_solicitud = rsme.id_solicitud                                       \n"|| 
                "AND    rnp.id_solicitud = rsg.id_solicitud                                        \n"
                
   -- Se verifica ventanilla
   IF p_ventanilla IS NOT NULL THEN
      IF p_ventanilla = 101 THEN 
         LET v_sql = v_sql||"AND    rsme.medio_entrega   <> 5 \n"
      ELSE 
         LET v_sql = v_sql||"AND    rsme.medio_entrega   = 5 \n"
      END IF 
   END IF

   IF p_nss IS NOT NULL THEN 
      DROP TABLE IF EXISTS tmp_nss;
      CREATE TEMP TABLE tmp_nss (dato CHAR(11));
      LET v_sql = v_sql, " AND rnp.nss IN (SELECT dato FROM tmp_nss) \n"
      CALL fn_llena_tabla (p_nss, "tmp_nss") RETURNING v_resultado
   END IF 
 
   -- Se verifica codigo de rechazo
   IF p_rechazo IS NOT NULL THEN
      LET v_sql = v_sql||"AND    rnp.estado_pago = "|| p_rechazo || "\n"
   END IF

   -- Se verifica afore
   IF p_afore IS NOT NULL THEN
      LET v_sql = v_sql||"AND    rws.cve_afore = "||p_afore || "\n"
   END IF

   -- Se verifica el rango de fechas de notificacion
   IF p_fecha_not_inicio IS NOT NULL THEN
      IF p_fecha_not_fin IS NOT NULL THEN
         LET v_sql = v_sql||"AND (DATE(rnp.f_notifica) BETWEEN '"||p_fecha_not_inicio||
                            "' AND '"||p_fecha_not_fin||"') \n"
      ELSE
         LET v_sql = v_sql||" AND DATE(rnp.f_notifica) > '"||p_fecha_not_inicio||"' \n"
      END IF
   ELSE
      IF p_fecha_not_fin IS NOT NULL THEN
         LET v_sql = v_sql||" AND DATE(rnp.f_notifica) < '"||p_fecha_not_fin||"' \n"
      END IF
   END IF

   DISPLAY v_sql
   CALL v_detalle_solicitud.clear()
   CALL v_detalle_solicitud_comp.clear()

   PREPARE prp_detalle_solicitudes FROM v_sql
   DECLARE cur_detalle_solicitudes CURSOR FOR prp_detalle_solicitudes

   LET v_contador = 1

   FOREACH cur_detalle_solicitudes INTO v_rec_detalle_solicitud.* --,v_detalle_solicitud_comp[v_contador].* 
      CALL v_string.clear()
      CALL v_string.append(v_rec_detalle_solicitud.rechazo)

      IF v_string.getIndexOf("-1000",1) <> 0 THEN
         LET v_rec_detalle_solicitud.rechazo = "0 - "
      END IF
      CALL fn_busca_fecha_pago(v_rec_detalle_solicitud.id_solicitud,p_fecha_pago_inicio,p_fecha_pago_fin) RETURNING v_rec_detalle_solicitud.f_pago
      IF v_rec_detalle_solicitud.f_pago IS NOT NULL THEN 
         IF v_rec_detalle_solicitud.estado_pago = 208 THEN 
            LET v_rec_detalle_solicitud.rechazo = 'SIN MARCA DE DISPOSICIÓN DE RECURSOS'
         ELSE
            IF v_rec_detalle_solicitud.estado_pago = 101 THEN 
               LET v_rec_detalle_solicitud.rechazo = 'SOLICITUD ACEPTADA'
            ELSE
               SELECT des_larga
               INTO   v_rec_detalle_solicitud.rechazo
               FROM   ret_rechazo_generico
               WHERE  (v_rec_detalle_solicitud.estado_pago+1000) = cod_rechazo
            END IF 
         END IF 
         IF v_rec_detalle_solicitud.grupo_ventanilla = '0201' THEN  -- Se busca la información faltante en ret_ws_sol_retiro_vent_afore 
            SELECT ind_beneficiario,nombre_pensionado,                               
                   ap_paterno_pensionado,ap_materno_pensionado,rfc_beneficiario, 
                   curp_beneficiario,aivs_viv92,aivs_viv97,           
                   pesos_viv92,pesos_viv97,(aivs_viv92+aivs_viv97),          
                   (pesos_viv92+pesos_viv97)
            INTO v_rec_detalle_solicitud.ind_beneficiario,
                 v_rec_detalle_solicitud.nombre_pensionado,
                 v_rec_detalle_solicitud.ap_paterno_pensionado,
                 v_rec_detalle_solicitud.ap_materno_pensionado,
                 v_rec_detalle_solicitud.rfc_beneficiario,
                 v_rec_detalle_solicitud.curp_beneficiario,
                 v_rec_detalle_solicitud.aivs_viv92,
                 v_rec_detalle_solicitud.aivs_viv97,
                 v_rec_detalle_solicitud.pesos_viv92,
                 v_rec_detalle_solicitud.pesos_viv97,
                 v_rec_detalle_solicitud.total_avis,
                 v_rec_detalle_solicitud.total_pesos
            FROM   ret_ws_sol_retiro_vent_afore 
            WHERE  id_solicitud = v_rec_detalle_solicitud.id_solicitud
         ELSE 
            SELECT CASE tpo_beneficiario WHEN 1 THEN 2 ELSE 1 END, nombre,                               
                   ap_paterno,ap_materno
            INTO v_rec_detalle_solicitud.ind_beneficiario,
                 v_rec_detalle_solicitud.nombre_pensionado,
                 v_rec_detalle_solicitud.ap_paterno_pensionado,
                 v_rec_detalle_solicitud.ap_materno_pensionado
            FROM   ret_beneficiario_generico 
            WHERE  id_solicitud = v_rec_detalle_solicitud.id_solicitud
            AND    consec_beneficiario = 1

            LET v_rec_detalle_solicitud.rfc_beneficiario = v_rec_detalle_solicitud.rfc
            LET v_rec_detalle_solicitud.curp_beneficiario = v_rec_detalle_solicitud.curp

            -- Se obtienen los montos de la liquidación
            SELECT NVL(SUM(monto_acciones),0),NVL(SUM(monto_pesos),0)
            INTO  v_rec_detalle_solicitud.aivs_viv92,
                  v_rec_detalle_solicitud.pesos_viv92
            FROM   ret_preliquida
            WHERE  id_referencia = v_rec_detalle_solicitud.id_solicitud
            AND    subcuenta     = 8
            AND    id_derechohabiente     = v_rec_detalle_solicitud.id_derechohabiente           

            SELECT NVL(SUM(monto_acciones),0),NVL(SUM(monto_pesos),0)
            INTO  v_rec_detalle_solicitud.aivs_viv97,
                  v_rec_detalle_solicitud.pesos_viv97
            FROM   ret_preliquida
            WHERE  id_referencia = v_rec_detalle_solicitud.id_solicitud
            AND    subcuenta     = 4
            AND    id_derechohabiente     = v_rec_detalle_solicitud.id_derechohabiente
            LET v_porcentaje = 100
            IF v_rec_detalle_solicitud.ind_beneficiario = "1" THEN 
               SELECT SUM(a.porcentaje)
               INTO   v_porcentaje
               FROM   ret_beneficiario_generico a,                  
                      ret_beneficiario_juridico b                   
               WHERE  a.id_solicitud = b.id_solicitud               
               AND    a.consec_beneficiario = b.consec_beneficiario 
               AND    a.id_solicitud = v_rec_detalle_solicitud.id_solicitud                            
               AND    b.estado_solicitud IN (72,73)
            END IF 
            LET v_rec_detalle_solicitud.aivs_viv92  = v_rec_detalle_solicitud.aivs_viv92  * (v_porcentaje / 100)
            LET v_rec_detalle_solicitud.pesos_viv92 = v_rec_detalle_solicitud.pesos_viv92 * (v_porcentaje / 100)
            LET v_rec_detalle_solicitud.aivs_viv97  = v_rec_detalle_solicitud.aivs_viv97  * (v_porcentaje / 100)
            LET v_rec_detalle_solicitud.pesos_viv97 = v_rec_detalle_solicitud.pesos_viv97 * (v_porcentaje / 100)

            
            LET v_rec_detalle_solicitud.total_avis = v_rec_detalle_solicitud.aivs_viv97 + v_rec_detalle_solicitud.aivs_viv92 
            LET v_rec_detalle_solicitud.total_pesos = v_rec_detalle_solicitud.pesos_viv97 + v_rec_detalle_solicitud.pesos_viv92 
         END IF 
        
         -- Beneficiario
         IF v_rec_detalle_solicitud.ind_beneficiario = "1" THEN
            LET v_rec_detalle_solicitud.ind_beneficiario = "Beneficiario"
         END IF
         -- Titular
         IF v_rec_detalle_solicitud.ind_beneficiario = "2" THEN
            LET v_rec_detalle_solicitud.ind_beneficiario = "Titular"
         END IF
           

        -- Aceptados
--        IF v_detalle_solicitud[v_contador].estado_pago = "101" OR 
--           v_detalle_solicitud[v_contador].estado_pago = "203" THEN
--            LET v_detalle_solicitud[v_contador].estado_pago = "ACEPTADO"
--        -- Rechazados
--        ELSE 
--            LET v_detalle_solicitud[v_contador].estado_pago = "RECHAZADO"
--        END IF
          LET v_detalle_solicitud[v_contador].afore                   = v_rec_detalle_solicitud.afore  
          LET v_detalle_solicitud[v_contador].aivs_viv92              = v_rec_detalle_solicitud.aivs_viv92  
          LET v_detalle_solicitud[v_contador].aivs_viv97              = v_rec_detalle_solicitud.aivs_viv97  
          LET v_detalle_solicitud[v_contador].ap_materno_pensionado   = v_rec_detalle_solicitud.ap_materno_pensionado  
          LET v_detalle_solicitud[v_contador].ap_paterno_pensionado   = v_rec_detalle_solicitud.ap_paterno_pensionado  
          LET v_detalle_solicitud[v_contador].clabe                   = v_rec_detalle_solicitud.clabe  
          LET v_detalle_solicitud[v_contador].curp                    = v_rec_detalle_solicitud.curp  
          LET v_detalle_solicitud[v_contador].curp_beneficiario       = v_rec_detalle_solicitud.curp_beneficiario  
          LET v_detalle_solicitud[v_contador].estado_pago             = v_rec_detalle_solicitud.estado_pago  
          LET v_detalle_solicitud[v_contador].f_notificacion          = v_rec_detalle_solicitud.f_notificacion  
          LET v_detalle_solicitud[v_contador].f_pago                  = v_rec_detalle_solicitud.f_pago  
          LET v_detalle_solicitud[v_contador].grupo_ventanilla        = v_rec_detalle_solicitud.grupo_ventanilla  
          LET v_detalle_solicitud[v_contador].ind_beneficiario        = v_rec_detalle_solicitud.ind_beneficiario  
          LET v_detalle_solicitud[v_contador].nombre_pensionado       = v_rec_detalle_solicitud.nombre_pensionado  
          LET v_detalle_solicitud[v_contador].nss                     = v_rec_detalle_solicitud.nss  
          LET v_detalle_solicitud[v_contador].pesos_viv92             = v_rec_detalle_solicitud.pesos_viv92  
          LET v_detalle_solicitud[v_contador].pesos_viv97             = v_rec_detalle_solicitud.pesos_viv97  
          LET v_detalle_solicitud[v_contador].rechazo                 = v_rec_detalle_solicitud.rechazo  
          LET v_detalle_solicitud[v_contador].rfc                     = v_rec_detalle_solicitud.rfc  
          LET v_detalle_solicitud[v_contador].rfc_beneficiario        = v_rec_detalle_solicitud.rfc_beneficiario  
          LET v_detalle_solicitud[v_contador].total_avis              = v_rec_detalle_solicitud.total_avis
          LET v_detalle_solicitud[v_contador].total_pesos             = v_rec_detalle_solicitud.total_pesos
          LET v_detalle_solicitud_comp[v_contador].id_solicitud       = v_rec_detalle_solicitud.id_solicitud
          LET v_detalle_solicitud_comp[v_contador].id_derechohabiente = v_rec_detalle_solicitud.id_derechohabiente
          INITIALIZE v_rec_detalle_solicitud TO NULL
          LET v_contador = v_contador + 1
      END IF 
   END FOREACH

   -- Se elimina el elemento nulo
   IF v_contador > 1 THEN
      CALL v_detalle_solicitud.deleteElement(v_contador)

      OPEN WINDOW solicitudes_detalle WITH FORM "RETC4002"

         DISPLAY ARRAY v_detalle_solicitud
                    TO solicitud_detalle.* ATTRIBUTES(ACCEPT = FALSE,CANCEL = FALSE)

            ON ACTION exportar
               CALL fn_genera_archivo(v_detalle_solicitud)

            ON ACTION salir
               EXIT DISPLAY

         END DISPLAY

      CLOSE WINDOW solicitudes_detalle
   ELSE
      CALL fn_mensaje("Atención","No se encontro ningún resultado.","stop")
   END IF

END FUNCTION

#Funcion que genera un archivo plano con el resultado de la consulta,
#separados por pipes.
FUNCTION fn_genera_archivo(p_detalle_solicitud)

   -- Valores a insertar en el archivo de salida
   DEFINE p_detalle_solicitud DYNAMIC ARRAY OF RECORD
             nss                     CHAR(11),
             estado_pago             CHAR(10),
             rechazo                 CHAR(35),
             rfc                     CHAR(13),
             curp                    CHAR(18),
             grupo_ventanilla        CHAR(15),
             afore                   CHAR(45),
             clabe                   CHAR(18),
             ind_beneficiario        CHAR(12),
             nombre_pensionado       CHAR(40),
             ap_paterno_pensionado   CHAR(40),
             ap_materno_pensionado   CHAR(40),
             rfc_beneficiario        CHAR(13),
             curp_beneficiario       CHAR(18),
             f_pago                  DATE,
             aivs_viv92              DECIMAL(13,2),
             aivs_viv97              DECIMAL(13,2),
             pesos_viv92             DECIMAL(13,2),
             pesos_viv97             DECIMAL(13,2),
             total_avis              DECIMAL(13,2),
             total_pesos             DECIMAL(13,2),
             f_notificacion          DATE
   END RECORD

   -- Variables auxiliares
   DEFINE v_channel_sal    base.Channel -- Creacion del archivo de salida
   DEFINE v_texto          STRING
   DEFINE v_archivo        STRING
   DEFINE v_ruta_env_arch  LIKE seg_modulo.ruta_envio -- Ruta de envio
   DEFINE v_ruta_arch      STRING -- Ruta de archivo
   DEFINE v_contador       INTEGER
   DEFINE v_string         base.StringBuffer

   --Se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio, ruta_bin
   INTO   v_ruta_env_arch
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- Se prepara el archivo de salida
   LET v_channel_sal = base.Channel.create()

   LET v_string = base.StringBuffer.create()
   LET v_archivo = TIME(CURRENT)
   CALL v_string.append(v_archivo)
   CALL v_string.replace(":","",2)

   -- Establecemos el nombre del archivo a partir de la fecha
   LET v_archivo = "consulta_notificacion_"||(TODAY USING "ddmmyyyy_")||v_string.toString()||".rmdnp"

   LET v_ruta_arch = v_ruta_env_arch CLIPPED,"/",v_archivo
   CALL v_channel_sal.openFile(v_ruta_arch,"w")

   -- Se escribe el encabezado
   LET v_texto =  "NSS|"                                  ||
                  "Estado de pago|"                       ||
                  "Codigo de retorno y descripcion|"      ||
                  "RFC Trabajador|"                       ||
                  "CURP Trabajador|"                      ||
                  "Ventanilla|"                           ||
                  "Clave AFORE|"                          ||
                  "Clabe Interbancaria|"                  ||
                  "Indicador de (Beneficiario o Titular)|"||
                  "Nombre del Beneficiario|"              ||
                  "Apellido Paterno del Beneficiario|"    ||
                  "Apellido Materno del Beneficiario|"    ||
                  "RFC Beneficiario|CURP Beneficiario|"   ||
                  "Fecha de Pago|"                        ||
                  "Vivienda 92 AIVS|"                     ||
                  "Vivienda 97 AIVS|"                     ||
                  "Vivienda 92 PESOS|"                    ||
                  "Vivienda 97 PESOS|"                    ||
                  "Total AIVS|"                           ||
                  "Total PESOS|"                          ||
                  "Fecha de envio a notificacion"
   CALL v_channel_sal.writeLine(v_texto)

   -- Se escribe el detalle
   FOR v_contador = 1 TO p_detalle_solicitud.getLength()
      LET v_texto =  p_detalle_solicitud[v_contador].nss                                           ,"|",
                    p_detalle_solicitud[v_contador].estado_pago                                   ,"|",
                    p_detalle_solicitud[v_contador].rechazo                                       ,"|",
                    p_detalle_solicitud[v_contador].rfc                                           ,"|",
                    p_detalle_solicitud[v_contador].curp                                          ,"|",
                    p_detalle_solicitud[v_contador].grupo_ventanilla                              ,"|",
                    p_detalle_solicitud[v_contador].afore                                         ,"|",
                    p_detalle_solicitud[v_contador].clabe                                         ,"|",
                    p_detalle_solicitud[v_contador].ind_beneficiario                              ,"|",
                    p_detalle_solicitud[v_contador].nombre_pensionado                             ,"|",
                    p_detalle_solicitud[v_contador].ap_paterno_pensionado                         ,"|",
                    p_detalle_solicitud[v_contador].ap_materno_pensionado                         ,"|",
                    p_detalle_solicitud[v_contador].rfc_beneficiario                              ,"|",
                    p_detalle_solicitud[v_contador].curp_beneficiario                             ,"|",
                    (p_detalle_solicitud[v_contador].f_pago               USING "dd/mm/yyyy"     ),"|",
                    (p_detalle_solicitud[v_contador].aivs_viv92           USING "&&&&&&&&&&.&&&&"),"|",
                    (p_detalle_solicitud[v_contador].aivs_viv97           USING "&&&&&&&&&&.&&&&"),"|",
                    (p_detalle_solicitud[v_contador].pesos_viv92          USING "&&&&&&&&&&.&&"  ),"|",
                    (p_detalle_solicitud[v_contador].pesos_viv97          USING "&&&&&&&&&&.&&"  ),"|",
                    (p_detalle_solicitud[v_contador].total_avis           USING "&&&&&&&&&&.&&&&"),"|",
                    (p_detalle_solicitud[v_contador].total_pesos          USING "&&&&&&&&&&.&&"  ),"|",
                    (p_detalle_solicitud[v_contador].f_notificacion       USING "dd/mm/yyyy"     )
      CALL v_channel_sal.writeLine(v_texto)
   END FOR

   CALL v_channel_sal.close()

   CALL v_string.clear()
   CALL v_string.append(v_ruta_env_arch)
   CALL v_string.replace(" ","",0)

   LET v_texto = v_string.toString()

   CALL fn_mensaje("Atención","Ruta de archivo de salida: "||v_texto||"/"||v_archivo,"stop")

END FUNCTION

FUNCTION fn_llena_tabla(p_dato,p_tabla)
DEFINE  p_dato      STRING
DEFINE  p_tabla     CHAR(25)
DEFINE  v_respuesta SMALLINT 
DEFINE  i           INTEGER 
DEFINE  v_dato       CHAR(25)
DEFINE  v_num_reg   INTEGER 
DEFINE  v_query     STRING 


   LET v_dato = ""
   LET v_num_reg = 0
   LET v_query = "INSERT INTO ", p_tabla CLIPPED, " VALUES (?)"
   PREPARE prp_inserta_datos FROM v_query

   LET v_query = "SELECT COUNT(*)",
                  " FROM ", p_tabla
                  
   PREPARE prp_cuenta_insertados FROM v_query

   FOR i = 1 TO p_dato.getLength()
      IF i = 1 THEN
         LET v_dato = v_dato CLIPPED, p_dato.subString(i,i)
      ELSE
         IF p_dato.subString(i,i) = "\n" THEN 
            --DISPLAY "EL DATO A INSERTAR >",v_dato, "<"
            EXECUTE prp_inserta_datos USING  v_dato
            LET v_dato = ""
         ELSE 
            LET v_dato = v_dato CLIPPED, p_dato.subString(i,i)
         END IF 
      END IF 
   END FOR 
   IF i > 1 AND p_dato.subString(i,i) <> "\n" THEN 
      EXECUTE prp_inserta_datos USING  v_dato
   END IF 

   EXECUTE prp_cuenta_insertados INTO v_num_reg 

RETURN v_respuesta
END FUNCTION 

FUNCTION fn_busca_fecha_pago(p_id_solicitud, p_f_pago_ini, p_f_pago_fin)
DEFINE p_id_solicitud DECIMAL(9,0)
DEFINE p_f_pago_ini   DATE
DEFINE p_f_pago_fin   DATE
DEFINE v_f_pago       DATE
DEFINE v_consec       SMALLINT
DEFINE v_f_paso_ini   CHAR(10)
DEFINE v_f_paso_fin   CHAR(10)
DEFINE v_tpo_benef    SMALLINT
DEFINE v_sql          STRING
DEFINE v_f_pago_paso  CHAR(8)

   SELECT DISTINCT tpo_beneficiario
   INTO   v_tpo_benef
   FROM   ret_beneficiario_generico
   WHERE  id_solicitud = p_id_solicitud

   LET v_sql = "SELECT DISTINCT rsp_f_pago       \n"||
               "FROM   ret_ws_consulta_pago_fico \n"||
               "WHERE  rsp_estatus IN (2,4)      \n"
    
   IF p_f_pago_ini IS NOT NULL THEN
      IF p_f_pago_fin IS NOT NULL THEN
         LET v_sql = v_sql||" AND (MDY(SUBSTR(rsp_f_pago,5,2),SUBSTR(rsp_f_pago,7,2),SUBSTR(rsp_f_pago,1,4)) BETWEEN '"||p_f_pago_ini||
                            "' AND '"||p_f_pago_fin||"') \n"
      ELSE
         LET v_sql = v_sql||" AND MDY(SUBSTR(rsp_f_pago,5,2),SUBSTR(rsp_f_pago,7,2),SUBSTR(rsp_f_pago,1,4)) > '"||p_f_pago_ini||"' \n"
      END IF
   ELSE
      IF p_f_pago_fin IS NOT NULL THEN
         LET v_sql = v_sql||" AND MDY(SUBSTR(rsp_f_pago,5,2),SUBSTR(rsp_f_pago,7,2),SUBSTR(rsp_f_pago,1,4)) < '"||p_f_pago_fin||"' \n"
      END IF
   END IF
   
   IF v_tpo_benef = 1 THEN
      LET v_sql = v_sql|| "AND id_solicitud = "|| p_id_solicitud || " \n"
   ELSE 
      LET v_sql = v_sql|| "AND id_solicitud IN (SELECT id_solicitud||consec_beneficiario \n"||
                          "                     FROM   ret_beneficiario_generico         \n"||
                          "                     WHERE  id_solicitud = "|| p_id_solicitud ||")\n"
   END IF 
   INITIALIZE v_f_pago_paso TO NULL
   PREPARE prp_detalle_pago FROM v_sql
   EXECUTE prp_detalle_pago INTO v_f_pago_paso
   IF v_f_pago_paso IS NOT NULL THEN
      LET v_f_pago = MDY(v_f_pago_paso[5,6],v_f_pago_paso[7,8],v_f_pago_paso[1,4])
   END IF 
   FREE prp_detalle_pago

RETURN v_f_pago
END FUNCTION