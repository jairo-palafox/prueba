###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => PAGOS                                                   #
#Programa          => PAGHC001                                                #
#Objetivo          => CONSULTA DE HISTORICO DE PAGOS                          #
#Fecha Inicio      => 15-MARZO-2018                                           #
###############################################################################

DATABASE safre_viv

GLOBALS "PAGHC001.inc"

PRIVATE DEFINE p_usuario            CHAR(10)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        CHAR(50)
PRIVATE DEFINE p_id_derechohabiente DECIMAL(9,0)

PRIVATE DEFINE v_ind_historico      INTEGER

PRIVATE DEFINE g_ciclo              SMALLINT
PRIVATE DEFINE v_hoy                DATE

PRIVATE DEFINE v_msj_alerta         STRING --Mensaje de alerta para consultas rojas

PRIVATE DEFINE v_forma              ui.form
PRIVATE DEFINE v_ventana            ui.window

PRIVATE DEFINE v_nss                CHAR(11)
PRIVATE DEFINE v_rfc                CHAR(13)
PRIVATE DEFINE v_arr_busqueda       DYNAMIC ARRAY OF datos_generales

PRIVATE DEFINE v_datos_generales    datos_consulta
PRIVATE DEFINE v_lista_his_trm      DYNAMIC ARRAY OF historico_trm
PRIVATE DEFINE v_lista_his_anu      DYNAMIC ARRAY OF historico_anual
PRIVATE DEFINE v_lista_his_ads      DYNAMIC ARRAY OF historico_ads
PRIVATE DEFINE v_lista_pensiones    DYNAMIC ARRAY OF historico_pensiones


MAIN

   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_id_derechohabiente = ARG_VAL(4)

   LET v_hoy = TODAY

   CALL STARTLOG("/safreviv_log/pag/" || p_usuario CLIPPED ||".PAGHC001.log")

   CLOSE WINDOW SCREEN

   LET p_nombre_menu = "Consulta Historica de Pagos"
   CALL ui.Interface.setText(p_nombre_menu)


   OPEN WINDOW paghc0011 WITH FORM "PAGHC0011"

   CALL fn_alertamiento() RETURNING v_msj_alerta

      LET v_ventana = ui.Window.forName("paghc0011")
      LET v_forma = v_ventana.getForm()

      LET g_ciclo = TRUE

   WHILE g_ciclo
      IF p_id_derechohabiente IS NULL THEN
         LET p_id_derechohabiente = 0
         WHILE p_id_derechohabiente = 0
            CALL fn_busca_cliente() RETURNING p_id_derechohabiente
         END WHILE
      ELSE
         LET g_ciclo = FALSE
      END IF

      IF p_id_derechohabiente > 0 THEN
         CALL fn_consulta_historico()
      END IF

      LET p_id_derechohabiente = NULL
   END WHILE

   CLOSE WINDOW paghc0011

END MAIN

FUNCTION fn_busca_cliente()

   --DEFINE v_nss                 CHAR(11)
   --DEFINE v_rfc                 CHAR(13)
   DEFINE v_condicion           STRING
   DEFINE v_query               STRING
   DEFINE i                     SMALLINT
   DEFINE v_id_derechohabiente  DECIMAL (9,0)
   DEFINE v_valida_rojo         SMALLINT
   DEFINE v_valida              INTEGER

   INITIALIZE v_arr_busqueda TO NULL
   INITIALIZE v_lista_his_trm TO NULL
   INITIALIZE v_lista_his_anu TO NULL
   INITIALIZE v_lista_his_ads TO NULL
   INITIALIZE v_datos_generales.* TO NULL
   INITIALIZE v_valida TO NULL
   INITIALIZE v_nss TO NULL
   INITIALIZE v_rfc TO NULL
   INITIALIZE v_ind_historico TO NULL

   CALL v_forma.setElementHidden("group2",1)

   CONSTRUCT v_condicion ON nss, rfc#, curp
                         FROM v_nss, rfc#, v_curp
      BEFORE CONSTRUCT
         CLEAR FORM

      ON ACTION ACCEPT
         LET v_nss    = GET_FLDBUF(v_nss)
         LET v_rfc    = GET_FLDBUF(rfc)
         LET INT_FLAG = FALSE

         IF v_nss IS NULL AND
            v_rfc IS NULL  THEN
            CALL fn_mensaje("Consulta Historica", "Debe de ingresar algún campo de búsqueda.", "about")
            NEXT FIELD v_nss
         END IF

         IF v_nss IS NOT NULL THEN
            IF fn_valida_caracteres(v_nss) <> 0 THEN
               CALL fn_mensaje("Consulta Saldo", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD v_nss
            END IF
         END IF

         IF v_rfc IS NOT NULL THEN
            IF fn_valida_caracteres(v_rfc) <> 0 THEN
               CALL fn_mensaje("Consulta Saldo", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD v_rfc
            END IF
         END IF
         
         ACCEPT CONSTRUCT
         
         LET g_ciclo  = FALSE
         LET INT_FLAG = TRUE
         EXIT CONSTRUCT

      ON ACTION CANCEL
         LET g_ciclo  = FALSE
         LET INT_FLAG = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT
   
   IF NOT INT_FLAG THEN
      LET v_query = "SELECT id_derechohabiente, ",
                          " nss,  ",
                          " rfc,  ",
                          " curp, ",
                          " TRIM(nombre_af)||' '|| ",
                          " TRIM(ap_paterno_af)||' '|| ",
                          " TRIM(ap_materno_af) ",
                     " FROM afi_derechohabiente ",
                    " WHERE " #, v_condicion CLIPPED
      IF v_nss IS NOT NULL THEN
         LET v_query = v_query, " nss = '", v_nss, "'"
      ELSE
         IF LENGTH(v_rfc CLIPPED) = 10 THEN
            LET v_query = v_query CLIPPED, " rfc[1,10] = '", v_rfc CLIPPED, "'"
         ELSE
            LET v_query = v_query CLIPPED, " rfc = '", v_rfc CLIPPED, "'"
         END IF
      END IF
      PREPARE prp_busqueda FROM v_query
      DECLARE cur_busqueda CURSOR FOR prp_busqueda

      LET i = 1
      FOREACH cur_busqueda INTO v_arr_busqueda[i].*
         LET i = i + 1
         IF i > 5 THEN
            CALL fn_mensaje("Consulta Historica",
                            "Acotar más el criterio de búsqueda. \n"||
                            "Se tomaran sólo los primeros 3 NSS´s asociados",
                            "about")
            EXIT FOREACH
         END IF
      END FOREACH

      IF i > 1 THEN
         IF i = 2 THEN
            LET v_id_derechohabiente = v_arr_busqueda[1].id_derechohabiente
         ELSE
            FOR i = 1 TO 4
               IF v_arr_busqueda[i].nss IS NOT NULL AND v_arr_busqueda[i].nss[1] <> ' ' THEN
                  CALL fn_valida_nss(v_arr_busqueda[i].nss) RETURNING v_valida_rojo
                  IF v_valida_rojo <> 0 THEN
                     LET v_id_derechohabiente = -1
                     CALL fn_mensaje("Atención",v_msj_alerta, "about")
                     RETURN v_id_derechohabiente
                  ELSE
                     IF i = 1 THEN
                        LET v_id_derechohabiente = v_arr_busqueda[1].id_derechohabiente
                     END IF
                  END IF
               END IF
            END FOR
         END IF
      ELSE
         #No se encontro informacion en SACI, se valida la existencia de registros en los historicos
         CALL valida_his(v_nss,v_rfc) RETURNING v_valida
         IF v_valida <> 0 THEN
            #Se encontro registros en los historicos
            LET v_id_derechohabiente = 1
            LET v_ind_historico = 1
         ELSE
            #No existen registros
            IF v_nss IS NULL THEN
               CALL fn_mensaje("Consulta Saldo","No existe información del RFC consultado. \n","about")
            ELSE
               CALL fn_mensaje("Consulta Saldo","No existe información del NSS consultado. \n","about")
            END IF
         END IF
      END IF
   ELSE
      LET v_id_derechohabiente = -1
   END IF

   RETURN v_id_derechohabiente

END FUNCTION

FUNCTION fn_consulta_historico()
   DEFINE v_valida_rojo         SMALLINT
   DEFINE v_confirma            SMALLINT
   DEFINE v_mensaje             STRING
--******************************************************************************
   IF p_id_derechohabiente IS NOT NULL AND v_ind_historico IS NULL THEN
      SELECT nss
      INTO v_nss
      FROM afi_derechohabiente
      WHERE id_derechohabiente = p_id_derechohabiente
        AND   tipo_trabajador <> "V"    -- tipo nss virtual
        AND   origen_afiliacion <> "S"  -- origen separación de cuentas  

      CALL fn_valida_nss(v_nss) RETURNING v_valida_rojo
   END IF
   IF v_valida_rojo <> 0 THEN
      CALL fn_mensaje("Atención",v_msj_alerta,"stop")
   ELSE    
--******************************************************************************

   LET v_mensaje = "La presente información se transmite sin prejuzgar sobre la \n",
                     "evidencia de su contenido, no creando derechos ni obligaciones \n",
                     "diversas a las comprendidas en las disposiciones y \n",
                     "ordenamientos legales en vigor."

   CALL fn_ventana_confirma("Atención",v_mensaje,"quest") RETURNING v_confirma

   IF v_confirma = 1 THEN
      CALL fn_datos_generales()

      INITIALIZE v_lista_his_trm TO NULL
      INITIALIZE v_lista_his_anu TO NULL
      INITIALIZE v_lista_his_ads TO NULL
      INITIALIZE v_lista_pensiones TO NULL
   
      CALL fn_consulta_historico_trm()

      CALL fn_consulta_pensiones_ads()

      CALL fn_consulta_historico_anual()

      CALL fn_consulta_historico_ads()

      CALL v_forma.setElementHidden("group2",0)

      DISPLAY v_datos_generales.nss TO v_nss
      DISPLAY BY NAME v_datos_generales.nss2,
                      v_datos_generales.nss3,
                      v_datos_generales.nss4,
                      v_datos_generales.rfc,
                      v_datos_generales.nombre_completo

      DIALOG ATTRIBUTES(UNBUFFERED)

          DISPLAY ARRAY v_lista_his_anu TO v_lista_his_anu.*
            ON ACTION ACCEPT
               EXIT DIALOG
         END DISPLAY

         DISPLAY ARRAY v_lista_his_ads TO v_lista_his_ads.*
            ON ACTION ACCEPT
               EXIT DIALOG
         END DISPLAY

         DISPLAY ARRAY v_lista_pensiones TO v_lista_pensiones.*
            ON ACTION ACCEPT
               EXIT DIALOG
         END DISPLAY
         
         DISPLAY ARRAY v_lista_his_trm TO v_lista_his_trm.*
            ON ACTION ACCEPT
               EXIT DIALOG
         END DISPLAY

      END DIALOG
       
   END IF 
END IF

END FUNCTION

FUNCTION fn_datos_generales()
   DEFINE v_query          STRING
   LET v_query = " SELECT FIRST 1 afi.nss       ,",
                    "        afi.rfc               ,",
                    "        afi.curp              ,",
                    "        TRIM(afi.nombre_af)||' '||",
                    "        TRIM(afi.ap_paterno_af)||' '||",
                    "        NVL(TRIM(afi.ap_materno_af),' ')",
                    " FROM afi_derechohabiente afi ",
                    " WHERE afi.id_derechohabiente = ? "

   IF v_ind_historico IS NULL THEN
      PREPARE exe_consulta_cuenta FROM v_query
      EXECUTE exe_consulta_cuenta USING   p_id_derechohabiente 
                                    INTO  v_datos_generales.nss,
                                          v_datos_generales.rfc,
                                          v_datos_generales.curp,
                                          v_datos_generales.nombre_completo
      LET v_datos_generales.id_derechohabiente = p_id_derechohabiente
      IF v_rfc IS NOT NULL THEN
         LET v_datos_generales.rfc = v_rfc
      END IF
   ELSE
      LET v_datos_generales.nss = v_arr_busqueda[1].nss
      LET v_datos_generales.rfc = v_arr_busqueda[1].rfc
      LET v_datos_generales.nombre_completo = v_arr_busqueda[1].nombre_completo
   END IF

   IF v_arr_busqueda[2].id_derechohabiente IS NOT NULL AND v_arr_busqueda[2].id_derechohabiente > 0 THEN
      LET v_datos_generales.nss2 = v_arr_busqueda[2].nss
   END IF 

   IF v_arr_busqueda[3].id_derechohabiente IS NOT NULL AND v_arr_busqueda[3].id_derechohabiente > 0 THEN
      LET v_datos_generales.nss3 = v_arr_busqueda[3].nss
   END IF 

   IF v_arr_busqueda[4].id_derechohabiente IS NOT NULL AND v_arr_busqueda[4].id_derechohabiente > 0 THEN
      LET v_datos_generales.nss4 = v_arr_busqueda[4].nss
   END IF 

END FUNCTION

PRIVATE FUNCTION fn_consulta_historico_trm()
   DEFINE v_query          STRING
   DEFINE v_lista_nss      STRING
   DEFINE i                INTEGER

   LET v_lista_nss = "'",v_datos_generales.nss, "'"
   
   IF v_datos_generales.nss2 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss2, "'"
   END IF

   IF v_datos_generales.nss3 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss3, "'"
   END IF

   IF v_datos_generales.nss4 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss4, "'"
   END IF

   LET v_query =  "SELECT ", 
                     "nss, ",
                     "nrp, ",
                     "f_pago, ",
                     "periodo_pago, ",
                     "folio_sua, ",
                     "cve_aportacion, ",
                     "aportacion, ",
                     "cve_amortizacion, ",
                     "amortizacion, ",
                     "tpo_aclaracion ",
                  "FROM safre_his@vivht_tcp:his_pag_bim_trm ",
                  "WHERE nss IN (", v_lista_nss, ") ",
                  "ORDER BY nss, f_pago DESC, periodo_pago DESC"

   #CALL fn_mensaje("TRM",v_query,"about")
   PREPARE exe_cunsulta_his_trm FROM v_query
   DECLARE cur_cunsulta_his_trm CURSOR FOR exe_cunsulta_his_trm

   LET i = 1
   FOREACH cur_cunsulta_his_trm INTO v_lista_his_trm[i].*
      LET i = i + 1
   END FOREACH
   
END FUNCTION

PRIVATE FUNCTION fn_consulta_historico_anual()
   DEFINE v_query          STRING
   DEFINE v_lista_nss      STRING
   DEFINE i                INTEGER
   
   LET v_lista_nss = "'",v_datos_generales.nss, "'"
   
   IF v_datos_generales.nss2 IS NOT NULL THEN
      LET v_lista_nss = v_lista_nss, ",'", v_datos_generales.nss2, "'"
   END IF

   IF v_datos_generales.nss3 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss3, "'"
   END IF

   IF v_datos_generales.nss4 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss4, "'"
   END IF

   LET v_query =  "SELECT ", 
                     "nss, ",
                     "rfc, ",
                     "nombre, ",
                     "ano_pago, ",
                     "clave, ",
                     "patron, ",
                     "bimestres, ",
                     "importe ",
                  "FROM safre_his@vivht_tcp:his_pag_anual ",
                  "WHERE nss IN (", v_lista_nss, ") "

   IF v_datos_generales.rfc IS NOT NULL THEN
      IF LENGTH(v_datos_generales.rfc CLIPPED) = 10 THEN
         LET v_query = v_query, " OR rfc[1,10] = '", v_datos_generales.rfc CLIPPED, "' "
      ELSE
         IF LENGTH(v_datos_generales.rfc CLIPPED) = 13 THEN
            LET v_query = v_query, " OR rfc = '", v_datos_generales.rfc CLIPPED, "' "
         END IF
      END IF
   END IF

   LET v_query = v_query, "ORDER BY nss, ano_pago DESC "

   #CALL fn_mensaje("Anual",v_query,"about")
   PREPARE exe_cunsulta_his_anu FROM v_query
   DECLARE cur_cunsulta_his_anu CURSOR FOR exe_cunsulta_his_anu

   LET i = 1
   FOREACH cur_cunsulta_his_anu INTO v_lista_his_anu[i].*
      LET i = i + 1
   END FOREACH

END FUNCTION

PRIVATE FUNCTION fn_consulta_historico_ads()
   DEFINE v_query          STRING
   DEFINE v_lista_nss      STRING
   DEFINE i                INTEGER
   
   LET v_lista_nss = "'",v_datos_generales.nss, "'"
   
   IF v_datos_generales.nss2 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss2, "'"
   END IF

   IF v_datos_generales.nss3 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss3, "'"
   END IF

   IF v_datos_generales.nss4 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss4, "'"
   END IF

   LET v_query =  "SELECT ", 
                     "nss, ",
                     "rfc, ",
                     "nombre, ",
                     "ano_pago, ",
                     "bimestre, ",
                     "f_pago, ",
                     "clave, ",
                     "patron, ",
                     "importe ",
                  "FROM safre_his@vivht_tcp:his_pag_bim_ads ",
                  "WHERE nss IN (", v_lista_nss, ") "

   IF v_datos_generales.rfc IS NOT NULL THEN
      IF LENGTH(v_datos_generales.rfc CLIPPED) = 10 THEN
         LET v_query = v_query, " OR rfc[1,10] = '", v_datos_generales.rfc CLIPPED, "' "
      ELSE
         IF LENGTH(v_datos_generales.rfc CLIPPED) = 13 THEN
            LET v_query = v_query, " OR rfc = '", v_datos_generales.rfc CLIPPED, "' "
         END IF 
      END IF
   END IF
   
   LET v_query = v_query, "ORDER BY nss, f_pago DESC, bimestre DESC "

   #CALL fn_mensaje("Bimestral",v_query,"about")
   PREPARE exe_cunsulta_his_ads FROM v_query
   DECLARE cur_cunsulta_his_ads CURSOR FOR exe_cunsulta_his_ads

   LET i = 1
   FOREACH cur_cunsulta_his_ads INTO v_lista_his_ads[i].*
      LET i = i + 1
   END FOREACH

END FUNCTION

PRIVATE FUNCTION fn_consulta_pensiones_ads()
   DEFINE v_query          STRING
   DEFINE v_lista_nss      STRING
   DEFINE i                INTEGER
   
   LET v_lista_nss = "'",v_datos_generales.nss, "'"
   
   IF v_datos_generales.nss2 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss2, "'"
   END IF

   IF v_datos_generales.nss3 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss3, "'"
   END IF

   IF v_datos_generales.nss4 IS NOT NULL THEN
      LET v_lista_nss =v_lista_nss, ",'", v_datos_generales.nss4, "'"
   END IF

   LET v_query =  "SELECT ", 
                     "nss, ",
                     "rfc, ",
                     "nombre, ",
                     "sdi, ",
                     "bimestre, ",
                     "f_pago, ",
                     "clave, ",
                     "patron, ",
                     "aportacion, ",
                     "amortizacion, ",
                     "folio_sua ",
                  "FROM safre_his@vivht_tcp:his_pag_pen_ads ",
                  "WHERE nss IN (", v_lista_nss, ") "

   IF v_datos_generales.rfc IS NOT NULL THEN
      IF LENGTH(v_datos_generales.rfc CLIPPED) = 10 THEN
         LET v_query = v_query, " OR rfc[1,10] = '", v_datos_generales.rfc CLIPPED, "' "
      ELSE
         IF LENGTH(v_datos_generales.rfc CLIPPED) = 13 THEN
            LET v_query = v_query, " OR rfc = '", v_datos_generales.rfc CLIPPED, "' "
         END IF
      END IF
   END IF
   
   LET v_query = v_query, "ORDER BY nss, f_pago DESC, bimestre DESC "

   #CALL fn_mensaje("Pensiones",v_query,"about")
   PREPARE exe_cunsulta_pen_ads FROM v_query
   DECLARE cur_cunsulta_pen_ads CURSOR FOR exe_cunsulta_pen_ads

   LET i = 1
   FOREACH cur_cunsulta_pen_ads INTO v_lista_pensiones[i].*
      LET i = i + 1
   END FOREACH

END FUNCTION

PRIVATE FUNCTION fn_valida_nss(p_tmp_nss)
   
   DEFINE p_tmp_nss                    CHAR(11)
   DEFINE v_tpo_consulta               SMALLINT
   DEFINE v_funcion_nss                STRING
   DEFINE v_resultado                  SMALLINT

   LET v_tpo_consulta = 2
   
   LET v_funcion_nss = "EXECUTE PROCEDURE sp_valida_nss_rojo(?,?,?)"

   PREPARE exe_valida_nss FROM v_funcion_nss
   EXECUTE exe_valida_nss USING p_tmp_nss,v_tpo_consulta,p_usuario
                           INTO v_resultado
   RETURN v_resultado

END FUNCTION

PRIVATE FUNCTION fn_valida_caracteres(p_campo)
   DEFINE p_campo       STRING
   RETURN p_campo.getIndexOf("*",1)
END FUNCTION

PRIVATE FUNCTION valida_his(p_nss,p_rfc)
   DEFINE p_nss            CHAR(11)
   DEFINE p_rfc            CHAR(13)
   
   DEFINE v_respuesta      INTEGER
   #DEFINE i                INTEGER
   DEFINE v_query          STRING
   DEFINE v_nss_t          CHAR(11)
   DEFINE v_rfc_t          CHAR(13)
   DEFINE v_nombre_t       VARCHAR(40)

   LET v_respuesta = 0

   #Primero se busca por nss
   IF p_nss IS NOT NULL THEN
      #Se busca el nss en la tabla de saldos anuales ads
      LET v_query = "SELECT FIRST 1 nss,  ",
                          " rfc,  ",
                          " TRIM(nombre) ",
                     " FROM safre_his@vivht_tcp:his_pag_anual ",
                    " WHERE nss = ?"
      PREPARE exe_busca_nss_anual FROM v_query

      EXECUTE exe_busca_nss_anual USING p_nss 
                                   INTO v_nss_t,
                                        v_rfc_t,
                                        v_nombre_t
      IF v_nss_t IS NOT NULL THEN
         LET v_respuesta = v_respuesta + 1
         LET v_arr_busqueda[1].nss = v_nss_t
         LET v_arr_busqueda[1].rfc = v_rfc_t
         LET v_arr_busqueda[1].nombre_completo = v_nombre_t
      END IF

      IF v_respuesta = 0 THEN
         #Si no se encontro en saldos anuales, de busca el nss en los bimestrales
         LET v_query = "SELECT FIRST 1 nss,  ",
                             " rfc,  ",
                             " TRIM(nombre) ",
                        " FROM safre_his@vivht_tcp:his_pag_bim_ads ",
                       " WHERE nss = ?"
         PREPARE exe_busca_nss_bim FROM v_query

         EXECUTE exe_busca_nss_bim USING p_nss 
                                      INTO v_nss_t,
                                           v_rfc_t,
                                           v_nombre_t
         IF v_nss_t IS NOT NULL THEN
            LET v_respuesta = v_respuesta + 1
            LET v_arr_busqueda[1].nss = v_nss_t
            LET v_arr_busqueda[1].rfc = v_rfc_t
            LET v_arr_busqueda[1].nombre_completo = v_nombre_t
         END IF

         IF v_respuesta = 0 THEN
            #Si no se encontro en saldos bimestrales, se busca en pensiones
            LET v_query = "SELECT FIRST 1 nss,  ",
                                " rfc,  ",
                                " TRIM(nombre) ",
                           " FROM safre_his@vivht_tcp:his_pag_pen_ads ",
                          " WHERE nss = ?"
            PREPARE exe_busca_nss_pen FROM v_query

            EXECUTE exe_busca_nss_pen USING p_nss 
                                         INTO v_nss_t,
                                              v_rfc_t,
                                              v_nombre_t
            IF v_nss_t IS NOT NULL THEN
               LET v_respuesta = v_respuesta + 1
               LET v_arr_busqueda[1].nss = v_nss_t
               LET v_arr_busqueda[1].rfc = v_rfc_t
               LET v_arr_busqueda[1].nombre_completo = v_nombre_t
            END IF
         END IF
      END IF
   END IF

   IF p_rfc IS NOT NULL THEN
      IF v_respuesta = 0 THEN
         #Se busca el rfc en la tabla de saldos anuales ads
         LET v_query = "SELECT nss ",
                        " FROM safre_his@vivht_tcp:his_pag_anual "
         IF LENGTH(p_rfc CLIPPED) = 10 THEN
            LET v_query = v_query CLIPPED, " WHERE rfc[1,10] = ?"
         ELSE
            LET v_query = v_query CLIPPED, " WHERE rfc = ?"
         END IF
         PREPARE exe_busca_rfc_anual FROM v_query
         DECLARE cur_busca_rfc_anual CURSOR FOR exe_busca_rfc_anual
         FOREACH cur_busca_rfc_anual USING p_rfc
                                     INTO v_nss_t
            IF v_nss_t IS NOT NULL THEN
               LET v_respuesta = v_respuesta + 1
               LET v_arr_busqueda[v_respuesta].nss = v_nss_t
               LET v_arr_busqueda[v_respuesta].rfc = p_rfc
            END IF
         END FOREACH

         IF v_respuesta = 0 THEN
            #Se busca el rfc en la tabla de saldos bimestrales ads
            LET v_query = "SELECT nss ",
                           " FROM safre_his@vivht_tcp:his_pag_bim_ads "
            IF LENGTH(p_rfc CLIPPED) = 10 THEN
               LET v_query = v_query CLIPPED, " WHERE rfc[1,10] = ?"
            ELSE
               LET v_query = v_query CLIPPED, " WHERE rfc = ?"
            END IF
            PREPARE exe_busca_rfc_bim FROM v_query
            DECLARE cur_busca_rfc_bim CURSOR FOR exe_busca_rfc_bim
            FOREACH cur_busca_rfc_bim USING p_rfc
                                        INTO v_nss_t
               IF v_nss_t IS NOT NULL THEN
                  LET v_respuesta = v_respuesta + 1
                  LET v_arr_busqueda[v_respuesta].nss = v_nss_t
                  LET v_arr_busqueda[v_respuesta].rfc = p_rfc
               END IF
            END FOREACH

            IF v_respuesta = 0 THEN
               #Se busca el rfc en la tabla de pensiones
               LET v_query = "SELECT nss ",
                              " FROM safre_his@vivht_tcp:his_pag_pen_ads "
               IF LENGTH(p_rfc CLIPPED) = 10 THEN
                  LET v_query = v_query CLIPPED, " WHERE rfc[1,10] = ?"
               ELSE
                  LET v_query = v_query CLIPPED, " WHERE rfc = ?"
               END IF
               PREPARE exe_busca_rfc_pen FROM v_query
               DECLARE cur_busca_rfc_pen CURSOR FOR exe_busca_rfc_pen
               FOREACH cur_busca_rfc_pen USING p_rfc
                                           INTO v_nss_t
                  IF v_nss_t IS NOT NULL THEN
                     LET v_respuesta = v_respuesta + 1
                     LET v_arr_busqueda[v_respuesta].nss = v_nss_t
                     LET v_arr_busqueda[v_respuesta].rfc = p_rfc
                  END IF
               END FOREACH
               IF v_respuesta > 0 THEN
                  LET v_query = "SELECT FIRST 1 TRIM(nombre) ",
                                 " FROM safre_his@vivht_tcp:his_pag_pen_ads "
                  IF LENGTH(p_rfc CLIPPED) = 10 THEN
                     LET v_query = v_query CLIPPED, " WHERE rfc[1,10] = ?"
                  ELSE
                     LET v_query = v_query CLIPPED, " WHERE rfc = ?"
                  END IF
                  PREPARE exe_busca_nombre_pen FROM v_query
                  EXECUTE exe_busca_nombre_pen USING p_rfc
                                                 INTO  v_arr_busqueda[1].nombre_completo
               END IF
            ELSE
               LET v_query = "SELECT FIRST 1 TRIM(nombre) ",
                              " FROM safre_his@vivht_tcp:his_pag_bim_ads "
               IF LENGTH(p_rfc CLIPPED) = 10 THEN
                  LET v_query = v_query CLIPPED, " WHERE rfc[1,10] = ?"
               ELSE
                  LET v_query = v_query CLIPPED, " WHERE rfc = ?"
               END IF
               PREPARE exe_busca_nombre_bim FROM v_query
               EXECUTE exe_busca_nombre_bim USING p_rfc
                                              INTO  v_arr_busqueda[1].nombre_completo
            END IF
         ELSE
            LET v_query = "SELECT FIRST 1 TRIM(nombre) ",
                           " FROM safre_his@vivht_tcp:his_pag_anual "
            IF LENGTH(p_rfc CLIPPED) = 10 THEN
               LET v_query = v_query CLIPPED, " WHERE rfc[1,10] = ?"
            ELSE
               LET v_query = v_query CLIPPED, " WHERE rfc = ?"
            END IF
            PREPARE exe_busca_nombre_anual FROM v_query
            EXECUTE exe_busca_nombre_anual USING p_rfc
                                           INTO  v_arr_busqueda[1].nombre_completo
         END IF
      END IF
   END IF
   
   RETURN v_respuesta
END FUNCTION