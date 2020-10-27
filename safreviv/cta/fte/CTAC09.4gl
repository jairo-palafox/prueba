###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC09                                                  #
#Objetivo          => CONSULTA DE DECRETO  EN HISTORICO LIQUIDADOS                                   #
#Fecha Inicio      => 06-AGOSTO-2015                                           #
###############################################################################
DATABASE safre_viv

GLOBALS "CTAC09.inc"
--******************************************************************************
DEFINE r_valida_nss                 SMALLINT
DEFINE v_busca_nss                  CHAR(11)
DEFINE v_msj_alerta                 STRING
   
--******************************************************************************
#Variables para capturar los parametros que recibe la consulta
PRIVATE DEFINE p_usuario            CHAR(10)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        CHAR(50)
PRIVATE DEFINE p_nombre_submenu        CHAR(50)
PRIVATE DEFINE p_id_decreto         DECIMAL(9,0)

PRIVATE DEFINE v_saldo_total        DECIMAL(22,2)
PRIVATE DEFINE v_acciones_total     DECIMAL(22,2)
PRIVATE DEFINE v_precio             DECIMAL(22,6)
PRIVATE DEFINE v_fecha              CHAR
PRIVATE DEFINE v_fecha_string       STRING
PRIVATE DEFINE v_bandera_fecha      INT
PRIVATE DEFINE v_bandera_aceptar      INT
PRIVATE DEFINE v_cuenta_usuarios     INT
PRIVATE DEFINE v_cuenta_existe_cliente     INT

PRIVATE DEFINE v_id_decreto     INT

PRIVATE DEFINE string_nss     STRING
PRIVATE DEFINE string_consec_cuenta     STRING
PRIVATE DEFINE string_rfc     STRING
PRIVATE DEFINE string_curp     STRING
PRIVATE DEFINE string_nombre     STRING
PRIVATE DEFINE string_fecha     STRING
PRIVATE DEFINE string_folio     STRING
PRIVATE DEFINE nss_aux          CHAR(11)

DEFINE v_lista_clientes         DYNAMIC ARRAY OF datos_generales

#Variables para el filtro de busqueda
PRIVATE DEFINE v_datos                       datos_generales

#Lista para los movimientos de decreto

PRIVATE DEFINE v_lista_decreto DYNAMIC ARRAY OF movimientos_decreto
PRIVATE DEFINE v_lista_clientes_cursor DYNAMIC ARRAY OF datos_generales_cursor

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana                       ui.Window
PRIVATE DEFINE forma                         ui.Form

MAIN
   DEFINE v_ciclo          SMALLINT

   LET v_ciclo = 1
   
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_id_decreto         = ARG_VAL(4)

   IF p_id_decreto IS NOT NULL THEN
      LET v_datos.id_decreto = p_id_decreto
END IF

   --CALL STARTLOG(p_usuario CLIPPED ||".CTAC09.log")

   CLOSE WINDOW SCREEN

   -- se asigna el titulo del programa
   LET p_nombre_menu = "Ejecutados"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW vtn_ctac091 WITH FORM "CTAC091"

   CALL fn_alertamiento()
      RETURNING v_msj_alerta
   
      LET ventana = ui.Window.forName("vtn_ctac091")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos.id_decreto IS NULL THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo             
         END IF
         
         IF v_datos.id_decreto IS NOT NULL THEN

                 
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE

   CLOSE WINDOW vtn_ctac091
END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   DEFINE v_consulta_cliente              STRING
   DEFINE v_condicion                     STRING

   DEFINE i                               INTEGER
   DEFINE cuenta                          INTEGER
   

   #Se inicializan las valiables del filtro
   INITIALIZE v_datos               TO NULL
   INITIALIZE v_lista_decreto       TO NULL
   INITIALIZE v_saldo_total         TO NULL

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group3",1)
   CALL forma.setElementHidden("group2",1)
   CALL forma.setElementHidden("group5",1)



   CONSTRUCT v_condicion ON consec_cuenta,nss_afo_recep,rfc_afo_recep,tia.curp, nombre_trabajador,folio,f_movimiento
                         FROM consec_cuenta, nss, rfc, curp, nombre,folio,fecha_movimiento
      BEFORE CONSTRUCT
         CLEAR FORM

      ON ACTION ACCEPT
         LET v_bandera_aceptar=1
                               
         LET v_datos.consec_cuenta  = GET_FLDBUF(consec_cuenta)
         LET v_datos.nss            = GET_FLDBUF(nss)
         LET v_datos.rfc            = GET_FLDBUF(rfc)
         LET v_datos.curp           = GET_FLDBUF(tia)
         LET v_datos.nombre_trabajador= GET_FLDBUF(nombre)
         LET v_datos.folio= GET_FLDBUF(folio)
         LET v_datos.f_movimiento= GET_FLDBUF(f_movimiento)
         LET INT_FLAG = FALSE
--******************************************************************************



         LET string_nss=GET_FLDBUF(nss)
         LET string_consec_cuenta=GET_FLDBUF(consec_cuenta)
         LET string_curp=GET_FLDBUF(curp)
         LET string_rfc=GET_FLDBUF(rfc)
         LET string_nombre=GET_FLDBUF(nombre)
         LET string_fecha=GET_FLDBUF(fecha_movimiento)
         LET string_folio=GET_FLDBUF(folio)

       
          
        
         IF string_nss.equals('*') OR string_consec_cuenta.equals('*') OR string_rfc.equals('*') OR string_curp.equals('*') OR
           string_nombre.equals('*') OR string_fecha.equals('*') OR string_folio.equals('*') THEN
                LET v_condicion=' 1=1'
                LET v_datos.consec_cuenta=NULL
                LET v_datos.nss=NULL
                LET v_datos.rfc=NULL
                -- LET v_datos.curp=NULL
                LET v_datos.nombre_trabajador=NULL
                LET v_datos.folio=NULL
                LET v_datos.f_movimiento=NULL
         END IF

         IF GET_FLDBUF(fecha_liquida) IS NOT NULL AND v_datos.f_movimiento IS NULL THEN
                CALL fn_mensaje("Consulta Decreto",
                                "No existen registros con el criterio de búsqueda. \n",
                                "about")
                RETURN 1
         END IF

   IF v_datos.nss IS NOT NULL THEN
        LET v_consulta_cliente=" SELECT FIRST 1 nss,id_decreto 
                              FROM afi_decreto
                              WHERE nss=?"

        PREPARE exe_consulta FROM v_consulta_cliente
        EXECUTE exe_consulta INTO nss_aux,v_id_decreto USING v_datos.nss

        IF  nss_aux IS NULL AND (v_id_decreto IS NULL OR v_id_decreto==0) THEN

            LET v_consulta_cliente=" SELECT FIRST 1 nss_afo_recep,id_decreto 
                              FROM tia_det_traspaso
                              WHERE nss_afo_recep=?"                              

                PREPARE exe_consulta_tia_nss FROM v_consulta_cliente
                EXECUTE exe_consulta_tia_nss INTO nss_aux,v_id_decreto USING v_datos.nss               
                
            IF  nss_aux IS NULL AND (v_id_decreto IS NULL OR v_id_decreto==0) THEN

                CALL fn_mensaje("Consulta Decreto",
                            "No existe el criterio de búsqueda \n en la base de datos \n",
                            "about")
                LET INT_FLAG = 1           
            RETURN 1

            ELSE

               CALL fn_valida_nss()
            END IF
        ELSE
            CALL fn_valida_nss()
        END IF
   END IF

   IF v_datos.rfc IS NOT NULL THEN
        LET v_consulta_cliente=" SELECT FIRST 1 nss,id_decreto 
                                 FROM afi_decreto
                                 WHERE rfc=?"

        PREPARE exe_consulta_rfc FROM v_consulta_cliente
        EXECUTE exe_consulta_rfc INTO v_datos.nss,v_id_decreto USING v_datos.rfc
      
        IF v_datos.nss IS NULL AND (v_id_decreto IS NULL OR v_id_decreto==0) THEN
                         
            LET v_consulta_cliente=" SELECT FIRST 1 nss_afo_recep,id_decreto 
                              FROM tia_det_traspaso
                              WHERE rfc_afo_recep=?"

                PREPARE exe_consulta_tia_rfc FROM v_consulta_cliente
                EXECUTE exe_consulta_tia_rfc INTO v_datos.nss,v_id_decreto USING v_datos.rfc

            IF  v_datos.nss IS NULL AND (v_id_decreto IS NULL OR v_id_decreto==0) THEN

                CALL fn_mensaje("Consulta Decreto",
                            "No existe el criterio de búsqueda \n en la base de datos \n",
                            "about")
                LET INT_FLAG = 1           
            
            RETURN 1

            ELSE
               CALL fn_valida_nss()
            END IF
        ELSE
            CALL fn_valida_nss()
        END IF
   END IF


   IF v_datos.folio IS NOT NULL THEN
        LET v_consulta_cliente=" SELECT COUNT(*) AS cuenta
                                 FROM afi_decreto afi,tia_det_traspaso tia
                                 WHERE tia.folio=? AND afi.id_decreto=tia.id_decreto"

        PREPARE exe_consulta_folio_liquida_cuenta FROM v_consulta_cliente
        EXECUTE exe_consulta_folio_liquida_cuenta INTO cuenta USING v_datos.folio

        IF cuenta==0 THEN
                CALL fn_mensaje("Consulta Decreto",
                                "No existe el criterio de búsqueda \n en la base de datos \n",
                                "about")
                RETURN 1
        END IF      

      IF cuenta==1 THEN
            LET v_consulta_cliente=" SELECT afi.nss
                                     FROM afi_decreto afi,tia_det_traspaso tia
                                     WHERE tia.folio=? AND afi.id_decreto=tia.id_decreto"

            PREPARE exe_consulta_folio_liquida FROM v_consulta_cliente
            EXECUTE exe_consulta_folio_liquida INTO v_datos.nss USING v_datos.folio
      
            CALL fn_valida_nss()
      END IF  
   END IF
   
   IF GET_FLDBUF(fecha_movimiento) IS NOT NULL THEN
        LET v_datos.f_movimiento=string_fecha
  
        LET v_consulta_cliente=" SELECT COUNT(*) AS cuenta
                                 FROM afi_decreto afi,tia_det_traspaso tia
                                 WHERE tia.f_movimiento=? AND afi.id_decreto=tia.id_decreto"

        PREPARE exe_consulta_fecha_liquida_cuenta FROM v_consulta_cliente
        EXECUTE exe_consulta_fecha_liquida_cuenta INTO cuenta USING v_datos.f_movimiento
     
      IF cuenta==0 THEN
            CALL fn_mensaje("Consulta Decreto",
                         "No existe el criterio de búsqueda \n en la base de datos \n",
                         "about")
            RETURN 1
      END IF

      IF cuenta==1 THEN
            LET v_consulta_cliente=" SELECT afi.nss
                                     FROM afi_decreto afi,tia_det_traspaso tia
                                     WHERE tia.f_movimiento=? AND afi.id_decreto=tia.id_decreto"

              PREPARE exe_consulta_fecha_liquida FROM v_consulta_cliente
              EXECUTE exe_consulta_fecha_liquida INTO v_datos.nss USING v_datos.f_movimiento
              CALL fn_valida_nss()
      END IF
   END IF

    IF v_datos.consec_cuenta IS NOT NULL THEN
         LET v_consulta_cliente=" SELECT FIRST 1 nss,id_decreto 
                                  FROM afi_decreto 
                                  WHERE consec_cuenta=?"

      PREPARE exe_consulta_consec_cuuenta FROM v_consulta_cliente
      EXECUTE exe_consulta_consec_cuuenta INTO v_datos.nss,v_id_decreto USING v_datos.consec_cuenta

      IF v_datos.nss IS NULL AND (v_id_decreto IS NULL OR v_id_decreto==0) IS NULL THEN

      CALL fn_mensaje("Consulta Decreto",
                         "No existe el criterio de búsqueda \n en la base de datos \n",
                         "about")

       { LET v_consulta_cliente=" SELECT FIRST 1 nss_afo_recep,id_decreto 
                              FROM tia_det_traspaso
                              WHERE id_decreto=?"

                PREPARE exe_consulta_tia_id FROM v_consulta_cliente
                EXECUTE exe_consulta_tia_id INTO v_datos.nss,v_id_decreto USING v_datos.consec_cuenta}

      {      IF  v_datos.nss IS NULL AND (v_id_decreto IS NULL OR v_id_decreto==0) THEN

                CALL fn_mensaje("Consulta Decreto",
                            "No existe el criterio de búsqueda \n en la base de datos \n",
                            "about")
                LET INT_FLAG = 1           
            
            RETURN 1

            ELSE
                           
               CALL fn_valida_nss()
            END IF}
      ELSE
                            
         CALL fn_valida_nss()
      END IF
   END IF

   IF v_datos.curp IS NOT NULL AND v_datos.curp<>'*' THEN
        LET v_consulta_cliente=" SELECT FIRST 1 nss,id_decreto FROM afi_decreto
                                 WHERE curp=?"

        LET v_datos.curp=string_curp

        PREPARE exe_consulta_curp FROM v_consulta_cliente
        EXECUTE exe_consulta_curp INTO v_datos.nss,v_id_decreto USING v_datos.curp  

        IF v_datos.nss IS NULL AND (v_id_decreto IS NULL OR v_id_decreto==0) THEN
            LET v_consulta_cliente=" SELECT FIRST 1 nss_afo_recep,id_decreto 
                              FROM tia_det_traspaso
                              WHERE curp=?"

                PREPARE exe_consulta_tia_curp FROM v_consulta_cliente
                EXECUTE exe_consulta_tia_curp INTO v_datos.nss,v_id_decreto USING v_datos.curp

            IF  v_datos.nss IS NULL AND (v_id_decreto IS NULL OR v_id_decreto==0) THEN

                CALL fn_mensaje("Consulta Decreto",
                            "No existe el criterio de búsqueda \n en la base de datos \n",
                            "about")
                LET INT_FLAG = 1           
            
            RETURN 1

            ELSE
               CALL fn_valida_nss()
            END IF
      ELSE
         CALL fn_valida_nss()
      END IF
   END IF

 
   IF r_valida_nss <> 0 THEN
  -- CALL fn_mensaje("Atención","antes de error", "stop")
      CALL fn_mensaje("Atención",v_msj_alerta,"stop")

         LET INT_FLAG = 1
         RETURN 1
    ELSE  
                        
       { IF v_bandera_fecha==1 AND v_datos.consec_cuenta IS NULL AND 
           v_datos.nss IS NULL AND v_datos.rfc IS NULL AND v_datos.curp
          IS NULL AND v_datos.nombre_trabajador IS NULL
          AND v_datos.folio IS NULL THEN
                CALL fn_mensaje("Consulta Decreto",
                         "No existen registros con el criterio de búsqueda. \n",
                         "about")
                RETURN 1
        END IF}

        
--******************************************************************************
         IF string_nss.equals('') AND string_consec_cuenta.equals('') AND string_curp.equals('') AND
            string_rfc.equals('') AND string_nombre.equals('') AND string_fecha.equals('') AND
            string_folio.equals('') THEN
            CALL fn_mensaje("Consulta Decreto",
                            "Debe de ingresar algún campo de búsqueda.",
                            "about")
            RETURN 1
         END IF
         ACCEPT CONSTRUCT

   END IF
      ON ACTION CANCEL
     
        INITIALIZE v_lista_clientes TO NULL
        INITIALIZE v_lista_clientes_cursor TO NULL
        INITIALIZE v_datos       TO NULL
        

        LET INT_FLAG = 1
        EXIT CONSTRUCT
   END CONSTRUCT

   #Si en la seccion de parametros de busqueda se selecciono aceptar pinta las siguientes secciones
   IF NOT INT_FLAG THEN

      #Primero se valida que exista el precio de accion
      SELECT precio_fondo 
      INTO v_precio
      FROM glo_valor_fondo 
      WHERE fondo = 11 AND 
      f_valuacion = TODAY

     { IF v_precio IS NULL OR v_precio = 0 THEN
         CALL fn_mensaje("Consulta Decreto",
                           "No se encontro el precio de accion para el dia de hoy.",
                            "about")
         INITIALIZE v_datos TO NULL
        RETURN 1
      END IF}


   
  {  IF (v_datos.consec_cuenta IS NOT NULL OR 
        v_datos.curp IS NOT NULL OR v_datos.nss IS NOT NULL OR v_datos.rfc IS NOT NULL) AND
        (string_fecha.equals('') AND string_folio.equals('')) THEN
    
    LET v_consulta_cliente =   "SELECT COUNT(*) ",
                                 "FROM afi_decreto ",
                                 "WHERE ", v_condicion 

        PREPARE exe_consulta_existe_usuario FROM v_consulta_cliente
        EXECUTE exe_consulta_existe_usuario INTO v_cuenta_usuarios}

       

    {    IF v_cuenta_usuarios==0 AND (v_datos.consec_cuenta IS NOT NULL OR 
        v_datos.curp IS NOT NULL OR v_datos.nss IS NOT NULL OR v_datos.rfc IS NOT NULL) THEN

                         
           CALL fn_mensaje("Consulta Decreto",
                         "No existe el criterio de búsqueda \n en la base de datos \n",
                         "about") 
            RETURN 1
        END IF}
  {  END IF}

   IF v_datos.curp IS NOT NULL AND v_datos.curp=='*' THEN
       LET v_condicion="AFI.CURP MATCHES '*'"
   END IF

      LET v_consulta_cliente =" SELECT FIRST 2 afi.id_decreto  \n",
                            " FROM AFI_DECRETO afi JOIN CTA_DECRETO CTA ON CTA.ID_DECRETO=AFI.ID_DECRETO \n",
                            " JOIN TIA_DET_TRASPASO TIA ON CTA.ID_DECRETO=TIA.ID_DECRETO \n",
                            " AND CTA.ID_REFERENCIA=TIA.ID_REFERENCIA \n",
                            " WHERE TIA.RESULT_OPERACION IN('01','99') AND  CTA.ID_DECRETO IN (SELECT id_decreto FROM cta_his_decreto) AND ",v_condicion

      PREPARE exe_consulta_existe_cliente FROM v_consulta_cliente
   --   EXECUTE exe_consulta_existe_cliente INTO v_cuenta_existe_cliente
      DECLARE cur_exe_consulta_existe_cliente CURSOR FOR exe_consulta_existe_cliente

      LET v_cuenta_existe_cliente=0

      FOREACH cur_exe_consulta_existe_cliente
                     LET v_cuenta_existe_cliente = v_cuenta_existe_cliente + 1
      END FOREACH

      IF v_cuenta_existe_cliente==1 THEN
        
        LET v_consulta_cliente ="SELECT ",
                                    "tia.folio, \n",
                                    "afi.consec_cuenta, \n",
                                    "afi.id_decreto,\n" ,
                                    "CASE WHEN tia.nss_afo_recep LIKE '%     %' THEN \n",
                                    "afi.nss ELSE tia.nss_afo_recep END CASE, \n",
                                    "CASE WHEN tia.rfc_afo_recep LIKE '%     %' THEN \n",
                                    "afi.rfc ELSE tia.rfc_afo_recep END CASE, \n",
                                    "CASE WHEN tia.curp LIKE '%     %' THEN \n",
                                    "afi.curp ELSE tia.curp END CASE, \n",
                                    "TRIM(tia.nombres_icefa)||' '||TRIM(tia.paterno_icefa)||' '||TRIM(tia.materno_icefa) as nombre_trabajador, \n",
                                    "afi.cve_icefa, \n",
                                    "afi.nombre_patron, \n",
                                    "tia.f_movimiento \n",                                     
                                    "FROM AFI_DECRETO afi JOIN CTA_DECRETO CTA ON CTA.ID_DECRETO=AFI.ID_DECRETO \n",
                                    " JOIN TIA_DET_TRASPASO TIA ON CTA.ID_DECRETO=TIA.ID_DECRETO \n",
                                    "AND CTA.ID_REFERENCIA=TIA.ID_REFERENCIA \n",
                                    " WHERE TIA.RESULT_OPERACION IN('01','99') AND CTA.ID_DECRETO IN (SELECT id_decreto FROM cta_his_decreto) ",
                                    " AND ",v_condicion

            PREPARE exe_consulta_existe_cliente_cursor FROM v_consulta_cliente
            DECLARE cur_consulta_existe_cliente CURSOR FOR exe_consulta_existe_cliente_cursor

                  LET i = 1
                  FOREACH cur_consulta_existe_cliente INTO v_lista_clientes[i].*
                     LET i = i + 1
                  END FOREACH

                  CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())
                  CLOSE cur_consulta_existe_cliente
                  FREE cur_consulta_existe_cliente

            LET v_datos.id_decreto           = v_lista_clientes[1].id_decreto
            LET v_datos.consec_cuenta        = v_lista_clientes[1].consec_cuenta
            LET v_datos.nss                  = v_lista_clientes[1].nss
            LET v_datos.rfc                  = v_lista_clientes[1].rfc
            LET v_datos.curp                 = v_lista_clientes[1].curp
            LET v_datos.nombre_trabajador    = v_lista_clientes[1].nombre_trabajador
            LET v_datos.cve_icefa            = v_lista_clientes[1].cve_icefa
            LET v_datos.nombre_patron        = v_lista_clientes[1].nombre_patron
            LET v_datos.f_movimiento        = v_lista_clientes[1].f_movimiento
            LET v_datos.folio       = v_lista_clientes[1].folio

            RETURN 1
      END IF

      IF v_cuenta_existe_cliente >= 1 THEN
      
            IF v_bandera_aceptar==1 THEN
                LET v_consulta_cliente ="SELECT ",
                                   "tia.folio, " ,
                                    "afi.consec_cuenta, ",                                    
                                    "tia.origen_traspaso, ",     
                                     "CASE WHEN tia.curp LIKE '%     %' THEN \n",
                                    "afi.curp ELSE tia.curp END CASE, \n",
                                    "TRIM(tia.nombres_afo_recep)||' '||TRIM(tia.paterno_afo_recep)||' '||TRIM(tia.materno_afo_recep) as nombre_trabajador, \n",
                                    "tia.nss_afo_recep, ",
                                    "tia.rfc_afo_recep, ",
                                    "CASE WHEN tia.nss_afo_recep LIKE '%     %' THEN \n",
                                    "afi.nss ELSE tia.nss_afo_recep END CASE, \n",
                                     "CASE WHEN tia.rfc_afo_recep LIKE '%     %' THEN \n",
                                    "afi.rfc ELSE tia.rfc_afo_recep END CASE, \n",
                                    "tia.nci_icefa, ",
                                    "tia.sdo_viv92, ",
                                    "tia.int_viv92, ", 
                                    "tia.result_operacion, ", 
                                    "tia.aivs_viv92, ",
                                    "tia.f_movimiento, ",                                    
                                    "afi.id_decreto, " ,                                    
                                    "afi.cve_icefa, ", 
                                    "afi.nombre_patron ",
                                    "FROM AFI_DECRETO AFI JOIN CTA_DECRETO CTA ON CTA.ID_DECRETO=AFI.ID_DECRETO ",
                                    " JOIN TIA_DET_TRASPASO TIA ON CTA.ID_DECRETO=TIA.ID_DECRETO ",
                                    "AND CTA.ID_REFERENCIA=TIA.ID_REFERENCIA ",
                                    " WHERE TIA.RESULT_OPERACION IN('01','99')  AND CTA.ID_DECRETO IN (SELECT id_decreto FROM cta_his_decreto) AND ",v_condicion

            PREPARE exe_consulta_existe_cliente_cursor_2 FROM v_consulta_cliente
            DECLARE cur_consulta_existe_cliente_2 CURSOR FOR exe_consulta_existe_cliente_cursor_2

                  LET i = 1

               --   TRY 
                  FOREACH cur_consulta_existe_cliente_2 INTO v_lista_clientes_cursor[i].*            
                             
                     LET i = i + 1

                  END FOREACH
                 -- CATCH
                  --END TRY
                 
                  CALL v_lista_clientes_cursor.deleteElement(v_lista_clientes_cursor.getLength())
                  CLOSE cur_consulta_existe_cliente_2
                  FREE cur_consulta_existe_cliente_2

            #Si se encotro mas de un cliente con el filtro de busqueda se muestra la lista para que el usuario seleccione a un cliente
            IF v_bandera_aceptar==1 THEN
            OPEN WINDOW vtn_ctac092 WITH FORM "CTAC092" --ATTRIBUTES (--STYLE="dialog",
                                               -- TEXT="Consulta Histórico Ejecutados")
            DISPLAY ARRAY v_lista_clientes_cursor TO lista_clientes.* 

                  ON ACTION ACCEPT 
                     LET v_bandera_aceptar=0
                     LET INT_FLAG = FALSE
                     
                    LET v_datos.consec_cuenta = v_lista_clientes_cursor[ARR_CURR()].consec_cuenta
                     LET v_datos.id_decreto = v_lista_clientes_cursor[ARR_CURR()].id_decreto
                     LET v_datos.folio = v_lista_clientes_cursor[ARR_CURR()].folio
                     LET v_datos.f_movimiento = v_lista_clientes_cursor[ARR_CURR()].f_movimiento
                     LET v_datos.cve_icefa=v_lista_clientes_cursor[ARR_CURR()].cve_icefa
                     LET v_datos.nss=v_lista_clientes_cursor[ARR_CURR()].nss_afo_recep
                    LET v_datos.rfc=v_lista_clientes_cursor[ARR_CURR()].rfc_afo_recep
                    LET v_datos.cve_icefa=v_lista_clientes_cursor[ARR_CURR()].cve_icefa
                    LET v_datos.nombre_trabajador=v_lista_clientes_cursor[ARR_CURR()].nombre_trabajador
                    LET v_datos.nombre_patron=v_lista_clientes_cursor[ARR_CURR()].nombre_patron
                    LET v_datos.curp=v_lista_clientes_cursor[ARR_CURR()].curp
                         
                     EXIT DISPLAY
                     
                  ON ACTION CANCEL

                     INITIALIZE v_lista_clientes_cursor TO NULL
                     INITIALIZE v_datos       TO NULL
                     EXIT DISPLAY                   
               END DISPLAY
            CLOSE WINDOW vtn_ctac092
            RETURN 1
             END IF
            END IF
            RETURN 1
         END IF

      IF v_cuenta_existe_cliente==0 AND NOT string_nss.equals('*') AND NOT string_rfc.equals('*') AND NOT string_curp.equals('*') AND
         NOT string_consec_cuenta.equals('*') AND NOT string_nombre.equals('*') AND NOT string_folio.equals('*') AND NOT string_fecha.equals('*')  THEN  

         LET v_consulta_cliente =" SELECT FIRST 2 * \n",
                                 "FROM afi_decreto afi JOIN tia_det_traspaso tia ON afi.id_decreto=tia.id_decreto \n",
                                 "WHERE tia.result_operacion = '10' AND  ", v_condicion
                                 
          PREPARE exe_consulta_existe_cliente_1 FROM v_consulta_cliente
       --   EXECUTE exe_consulta_existe_cliente INTO v_cuenta_existe_cliente
          DECLARE cur_exe_consulta_existe_cliente_1 CURSOR FOR exe_consulta_existe_cliente_1

          LET v_cuenta_existe_cliente=0

          FOREACH cur_exe_consulta_existe_cliente_1
                         LET v_cuenta_existe_cliente = v_cuenta_existe_cliente + 1
          END FOREACH

          IF v_cuenta_existe_cliente==2 THEN
                CALL fn_mensaje("Consulta Decreto",
                         "Se encontraron registros pendientes de \n ejecutar con el criterio de búsqueda. \n",
                         "about")
                RETURN 1
          END IF

          IF v_cuenta_existe_cliente==1 THEN
                CALL fn_mensaje("Consulta Decreto",
                         "Existe un registro pendiente de ejecutar \n con el criterio de búsqueda. \n",
                         "about")
          ELSE
              LET v_consulta_cliente =" SELECT FIRST 1 * ",
                            "FROM afi_decreto afi ",
                            "WHERE ", v_condicion

    TRY
              PREPARE exe_consulta_existe_cliente_2 FROM v_consulta_cliente
       --   EXECUTE exe_consulta_existe_cliente INTO v_cuenta_existe_cliente
              DECLARE cur_exe_consulta_existe_cliente_2 CURSOR FOR exe_consulta_existe_cliente_2

              LET v_cuenta_existe_cliente=0

              FOREACH cur_exe_consulta_existe_cliente_2
                             LET v_cuenta_existe_cliente = v_cuenta_existe_cliente + 1
              END FOREACH
    CATCH
                                   
            LET v_consulta_cliente =" SELECT FIRST 1 afi.id_decreto \n",
                            "FROM TIA_DET_TRASPASO tia, AFI_DECRETO afi \n",
                            "WHERE tia.id_decreto=afi.id_decreto AND ", v_condicion

            PREPARE exe_consulta_existe_cliente_3 FROM v_consulta_cliente
       --   EXECUTE exe_consulta_existe_cliente INTO v_cuenta_existe_cliente
              DECLARE cur_exe_consulta_existe_cliente_3 CURSOR FOR exe_consulta_existe_cliente_3

              LET v_cuenta_existe_cliente=0

              FOREACH cur_exe_consulta_existe_cliente_3
                             LET v_cuenta_existe_cliente = v_cuenta_existe_cliente + 1
              END FOREACH

              IF v_cuenta_existe_cliente==1 THEN
                    CALL fn_mensaje("Consulta Decreto",
                         "No existen registros ejecutados  \n con el criterio de búsqueda \n",
                         "about")
                ELSE
 
                   CALL fn_mensaje("Consulta Decreto",
                         "No existe el criterio de búsqueda \n en la base de datos \n",
                         "about") 
                END IF

                RETURN 1

    END TRY

                IF v_cuenta_existe_cliente==1 THEN
                    CALL fn_mensaje("Consulta Decreto",
                         "No existen registros ejecutados  \n con el criterio de búsqueda \n",
                         "about")
                ELSE
 
                   CALL fn_mensaje("Consulta Decreto",
                         "No existe el criterio de búsqueda \n en la base de datos \n",
                         "about") 
                END IF
          END IF
        
     {    CALL fn_mensaje("Consulta Decreto",
                         "El registro aún no ha sido ejecutado. \n",
                         "about")}
         RETURN 1
      END IF
      CALL fn_mensaje("Consulta Decreto",
                         "No existen registros ejecutados  \n con el criterio de búsqueda \n",
                         "about")
        RETURN 1
   ELSE
      RETURN 0
   END IF
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()

--******************************************************************************
         SELECT nss
      INTO v_datos.nss
      FROM afi_decreto
      WHERE id_decreto = p_id_decreto

      CALL fn_valida_nss()
      
      IF r_valida_nss = 0 THEN
--******************************************************************************
   #primero se buscan los datos generales del cliente
   --CALL fn_datos_generales()

   #Se ejecuta la funcion que consulta los movimientos de decreto
   CALL fn_consulta_decreto()

   #Se muestran los datos en pantalla
   DIALOG   ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_decreto TO lista_decreto.* END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group3",0)
         CALL forma.setElementHidden("group2",0)
         CALL forma.setElementHidden("group5",0)

         
         DISPLAY v_datos.consec_cuenta    TO consec_cuenta
         DISPLAY v_datos.nss              TO nss
         DISPLAY v_datos.rfc              TO rfc
         DISPLAY v_datos.curp             TO curp
         DISPLAY v_datos.nombre_trabajador  TO nombre
         DISPLAY v_datos.cve_icefa        TO icefa
         DISPLAY v_datos.nombre_patron    TO patron
         DISPLAY v_saldo_total            TO saldo
         DISPLAY v_acciones_total         TO acciones_total
         DISPLAY v_precio                 TO precio
         DISPLAY v_datos.f_movimiento    TO fecha_movimiento
         DISPLAY v_datos.folio    TO folio
        

      ON ACTION ACCEPT

      IF v_bandera_aceptar==0 AND v_lista_clientes_cursor.getLength()==0 AND v_lista_clientes.getLength()<>0  THEN
        OPEN WINDOW vtn_ctac092 WITH FORM "CTAC092" --ATTRIBUTES (--STYLE="dialog",
                                                                   -- TEXT="Consulta Histórico Ejecutados")
               DISPLAY ARRAY v_lista_clientes TO lista_clientes.*
                  ON ACTION ACCEPT 
                        
                     LET INT_FLAG = FALSE
                     LET v_datos.consec_cuenta = v_lista_clientes_cursor[ARR_CURR()].consec_cuenta
                     LET v_datos.id_decreto = v_lista_clientes_cursor[ARR_CURR()].id_decreto
                     LET v_datos.folio = v_lista_clientes_cursor[ARR_CURR()].folio
                     LET v_datos.f_movimiento = v_lista_clientes_cursor[ARR_CURR()].f_movimiento
                     LET v_datos.cve_icefa=v_lista_clientes_cursor[ARR_CURR()].cve_icefa
                     LET v_datos.nss=v_lista_clientes_cursor[ARR_CURR()].nss_afo_recep
                    LET v_datos.nombre_trabajador=v_lista_clientes_cursor[ARR_CURR()].nombre_trabajador
                    LET v_datos.rfc=v_lista_clientes_cursor[ARR_CURR()].rfc_afo_recep
                    LET v_datos.nombre_patron=v_lista_clientes_cursor[ARR_CURR()].nombre_patron
                    LET v_datos.cve_icefa=v_lista_clientes_cursor[ARR_CURR()].cve_icefa                    
                    LET v_datos.curp=v_lista_clientes_cursor[ARR_CURR()].curp
                     EXIT DISPLAY

                  ON ACTION CANCEL
                     INITIALIZE v_lista_clientes TO NULL
                     INITIALIZE v_lista_clientes_cursor TO NULL
                     INITIALIZE v_datos       TO NULL

                     EXIT DISPLAY
               END DISPLAY
            CLOSE WINDOW vtn_ctac092
    END IF

    IF v_bandera_aceptar==0 AND v_lista_clientes_cursor.getLength()<>0 AND v_lista_clientes.getLength()==0  THEN
        OPEN WINDOW vtn_ctac092 WITH FORM "CTAC092" --ATTRIBUTES (--STYLE="dialog",
                                                                   -- TEXT="Consulta Histórico Ejecutados")
               DISPLAY ARRAY v_lista_clientes_cursor TO lista_clientes.*
                  ON ACTION ACCEPT 
                        
                     LET INT_FLAG = FALSE
                     LET v_datos.consec_cuenta = v_lista_clientes_cursor[ARR_CURR()].consec_cuenta
                     LET v_datos.id_decreto = v_lista_clientes_cursor[ARR_CURR()].id_decreto
                     LET v_datos.folio = v_lista_clientes_cursor[ARR_CURR()].folio
                     LET v_datos.f_movimiento = v_lista_clientes_cursor[ARR_CURR()].f_movimiento
                     LET v_datos.cve_icefa=v_lista_clientes_cursor[ARR_CURR()].cve_icefa
                     LET v_datos.nss=v_lista_clientes_cursor[ARR_CURR()].nss_afo_recep
                    LET v_datos.nombre_trabajador=v_lista_clientes_cursor[ARR_CURR()].nombre_trabajador
                    LET v_datos.rfc=v_lista_clientes_cursor[ARR_CURR()].rfc_afo_recep                   
                    LET v_datos.cve_icefa=v_lista_clientes_cursor[ARR_CURR()].cve_icefa                   
                    LET v_datos.curp=v_lista_clientes_cursor[ARR_CURR()].curp
                    LET v_datos.nombre_patron=v_lista_clientes_cursor[ARR_CURR()].nombre_patron
                     EXIT DISPLAY

                  ON ACTION CANCEL
                     INITIALIZE v_lista_clientes_cursor TO NULL
                     INITIALIZE v_datos       TO NULL

                     EXIT DISPLAY
               END DISPLAY
            CLOSE WINDOW vtn_ctac092
    END IF

         WHENEVER ERROR CONTINUE
         DROP TABLE tmp_decreto
         WHENEVER ERROR STOP

     --    RETURN 1
         
         EXIT DIALOG
        
      ON ACTION cancelar
       INITIALIZE v_lista_clientes TO NULL
       INITIALIZE v_lista_clientes_cursor TO NULL
                     INITIALIZE v_datos       TO NULL
         LET v_datos.id_decreto = NULL
    
         WHENEVER ERROR CONTINUE
         DROP TABLE tmp_decreto
         WHENEVER ERROR STOP         
         
         RETURN 1
         EXIT DIALOG

   END DIALOG

   RETURN 1
--******************************************************************************
   ELSE 
        
      CALL fn_mensaje("Atención",v_msj_alerta, "stop")
      LET v_datos.id_decreto = NULL
         RETURN 1
END IF
--******************************************************************************
END FUNCTION

{PRIVATE FUNCTION fn_datos_generales()



                            SELECT
                                    id_decreto, 
                                    consec_cuenta,
                                    nss, 
                                    rfc, 
                                    curp, 
                                    nombre_trabajador, 
                                    cve_icefa, 
                                    nombre_patron       
                                    INTO v_datos.*
                                   FROM afi_decreto  
                                    WHERE id_decreto = v_datos.id_decreto

                        
END FUNCTION
}

PRIVATE FUNCTION fn_consulta_decreto()
   DEFINE v_consulta_decreto     STRING

   DEFINE i                      INTEGER
   
   LET v_consulta_decreto =   "SELECT ",
                                 "mov.id_decreto, ",
                                 "mov.f_liquida, ",
                                 "mov.folio_liquida, ",
                                 "mov.movimiento || ' - ' || TRIM(cat.movimiento_desc), ",
                                 "mov.origen, ",
                                 "mov.monto_acciones, ",
                                 "mov.monto_pesos, ",
                                 "mov.fondo_inversion ",
                              "FROM cta_decreto mov JOIN cat_movimiento cat on ",
                              "mov.movimiento=cat.movimiento JOIN tia_det_traspaso tia on ",
                              "tia.id_referencia=mov.id_referencia AND mov.id_decreto=tia.id_decreto ",
                              "WHERE mov.id_decreto = ? AND tia.result_operacion IN('01','99') AND  ",
                             "mov.id_decreto IN (SELECT id_decreto FROM cta_his_decreto) "
                              
   PREPARE exe_consulta_decreto FROM v_consulta_decreto
   DECLARE cur_consulta_decreto CURSOR FOR exe_consulta_decreto

   LET i = 1
   FOREACH cur_consulta_decreto USING v_datos.id_decreto INTO v_lista_decreto[i].*
DISPLAY v_lista_decreto[i].*

      LET i = i + 1
      IF i > MAX_REGISTROS THEN
         CALL fn_mensaje("Consulta Decreto",
                         "Solo se mostrara los ultimos " || MAX_REGISTROS || " movimientos de decreto.",
                         "about")
      END IF
   END FOREACH
   CALL v_lista_decreto.deleteElement(v_lista_decreto.getLength())
   CLOSE cur_consulta_decreto
   FREE cur_consulta_decreto

   SELECT SUM(mov.monto_acciones * gf.precio_fondo), SUM(mov.monto_acciones)
   INTO v_saldo_total, v_acciones_total
   FROM cta_decreto mov
   INNER JOIN glo_valor_fondo gf ON (gf.fondo = mov.fondo_inversion AND gf.f_valuacion = TODAY)
   WHERE mov.fondo_inversion <> 0
   AND mov.id_decreto = v_datos.id_decreto

END FUNCTION

FUNCTION fn_valida_nss()

   DEFINE v_funcion_nss                STRING
   DEFINE v_cadena                     CHAR(11)
   DEFINE v_tpo_consulta               SMALLINT

   LET v_tpo_consulta = 3 

   LET v_funcion_nss = "EXECUTE PROCEDURE sp_valida_nss_rojo(?,?,?)"

   PREPARE prp_valida_nss FROM v_funcion_nss
   LET v_cadena = v_datos.nss
   EXECUTE prp_valida_nss USING v_cadena, v_tpo_consulta, p_usuario
                           INTO r_valida_nss

END FUNCTION