--=============================================================================
-----------------------------------------------------------------------------------------
-- Modulo       => TIA                                                                    
-- Programa     => TIAC15                                                                 
-- Objetivo     => Consulta de aceptados y rechazados de carga de archivo y liquidación
-- Autor        => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio => 29 de Agosto de 2017                                                  
-----------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                             
-- Fec Mod.     => 28 de Septiembre de 2017.                                     
-- Modificación => Agregar botón que genera archivo.
-- Clave cambio => proinfxvii-81
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS
	 DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod,
          v_ventana    ui.WINDOW,
          v_forma      ui.form 

   DEFINE
       arr_aceptados        DYNAMIC ARRAY OF RECORD
          f_liquidacion     DATE,
          nombre_archivo    CHAR(40),
          folio             DECIMAL(9,0),        
          nss_afo_recep     CHAR(11),
          curp              CHAR(18),
          nombre            CHAR(120),
          consec_cuenta     DECIMAL(11,0),
          monto_pesos       DECIMAL(12,2),
          monto_acciones    DECIMAL(16,6),
          result_operacion  CHAR(45)
       END RECORD,
       arr_rechazados       DYNAMIC ARRAY OF RECORD
          f_liquidacion     DATE,
          nombre_archivo    CHAR(40),
          folio             DECIMAL(9,0),        
          nss_afo_recep     CHAR(11),
          curp              CHAR(18),
          nombre            CHAR(120),
          consec_cuenta     DECIMAL(11,0),
          monto_pesos       DECIMAL(12,2),
          monto_acciones    DECIMAL(16,6),
          result_operacion  CHAR(45)
       END RECORD

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
   
   -- consulta de informacion recibida 
   CALL fn_consulta_registros(p_usuario_cod)

END MAIN

FUNCTION fn_consulta_registros(p_usuario_cod)

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod,
       v_nss       CHAR(11),
       v_id_unico  DECIMAL(11,0),
       v_rfc       CHAR(13),
       v_radio     CHAR(15),
       v_f_inicial DATE,
       v_f_final   DATE,
--       arr_reporte,
       v_resultado SMALLINT
       
   CLOSE WINDOW SCREEN     
   OPEN WINDOW w_consulta WITH FORM "TIAC151"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   LET v_forma = v_ventana.getForm()
   
   CALL v_ventana.SETTEXT("Consulta Registros Aceptados y Rechazados")

   DIALOG ATTRIBUTES (UNBUFFERED)
   INPUT v_nss, v_id_unico, v_rfc, v_radio,  v_f_inicial, v_f_final
      FROM  e_nss, e_id_unico, e_rfc, radiogroup1,  date_f_inicial, date_f_final

      BEFORE INPUT 
         -- se limpian las variables
         LET v_nss       = NULL    
         LET v_id_unico  = NULL 
         LET v_rfc       = NULL 
         LET v_radio     = NULL 
         LET v_f_inicial = NULL      
         LET v_f_final   = NULL
         LET v_id_unico  = NULL
         
   END INPUT   

      ON ACTION ACCEPT

         -- se borran los arreglos de despliegue
         CALL arr_rechazados.clear()
         CALL arr_aceptados.clear()
--         CALL arr_reporte.clear()

         IF (  (v_id_unico   IS NULL ) AND 
               (v_nss       IS NULL ) AND
               (v_rfc       IS NULL ) AND 
               (v_f_inicial IS NULL ) AND 
               (v_f_final   IS NULL ) AND 
               (v_radio     IS NULL ) ) THEN

            CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de búsqueda","about")
            NEXT FIELD e_nss
         END IF

         IF (v_f_inicial IS NOT NULL AND v_f_final IS NOT NULL) AND v_radio IS NULL THEN
            CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de Aceptados o Rechazados o Ambos","about")
            NEXT FIELD e_nss

         END IF
         
         IF v_radio = 1 THEN
            CALL Busca_aceptados(v_nss,v_id_unico,v_rfc,v_f_inicial,v_f_final)
         END IF

         IF v_radio = 2 THEN
            CALL Busca_rechazados(v_nss,v_id_unico,v_rfc,v_f_inicial,v_f_final)
         END IF
   
         IF v_radio = 3 THEN
            CALL Busca_ambos(v_nss,v_id_unico,v_rfc,v_f_inicial,v_f_final)
         END IF

         -- se abre la ventana de resultados
         OPEN WINDOW w_detalle_consulta WITH FORM "TIAC152"
         
         -- se abre un dialog para realizar el despliegue de los resultados
         DIALOG ATTRIBUTES (UNBUFFERED)
         
            DISPLAY ARRAY arr_aceptados TO tbl_aceptados.*
            END DISPLAY
            
            DISPLAY ARRAY arr_rechazados TO tbl_rechazados.*
            END DISPLAY
            
            ON ACTION regresar
               EXIT DIALOG
               
            ON ACTION exportar                   --proinfxvii-81
               CALL fn_exporta_archivo(arr_aceptados, arr_rechazados, v_radio) RETURNING v_resultado --proinfxvii-81

         END DIALOG
         
         CLOSE WINDOW w_detalle_consulta
         
      ON ACTION CANCEL
         EXIT DIALOG
   
   END DIALOG
   
   CLOSE WINDOW w_consulta

END FUNCTION

FUNCTION Busca_aceptados(v_nss,v_id_unico,v_rfc,v_f_inicial,v_f_final)

   DEFINE arr_aceptados_p   RECORD
          f_liquidacion     DATE,
          nombre_archivo    CHAR(40),
          folio             DECIMAL(9,0),        
          nss_afo_recep     CHAR(11),
          curp              CHAR(18),
          paterno           CHAR(40),
          materno           CHAR(40),
          nombres           CHAR(40),
          consec_cuenta     DECIMAL(11,0),
          monto_pesos       DECIMAL(12,2),
          monto_acciones    DECIMAL(16,6),
          result_operacion  CHAR(45)
   END RECORD
   
   DEFINE v_f_inicial DATE
   DEFINE v_f_final   DATE
   DEFINE v_nss       CHAR(11)
   DEFINE v_id_unico  DECIMAL(11,0)
   DEFINE v_rfc       CHAR(13)
   DEFINE v_indice    INTEGER
   DEFINE v_query     STRING
   DEFINE v_contador  INTEGER

   LET v_query = "SELECT mov.f_liquida,",
                          "glo.nombre_archivo,",
                          "det.folio,",
                          "det.nss_afo_recep,",
                          "det.curp,",
                          "det.paterno_afo_recep,",
                          "det.materno_afo_recep,",
                          "det.nombres_afo_recep,",
                          "afi.consec_cuenta,",
                          "mov.monto_pesos,",
                          "mov.monto_acciones,",
                          "CASE det.result_operacion ",
                          "   WHEN '01' THEN 'ACEPTADO' ",
                          "   WHEN '99' THEN 'ACEPTADO' ",
                          "END ",
                   "FROM   tia_det_traspaso det,",
                          "glo_ctr_archivo  glo,",
                          "afi_decreto      afi,",
                          "cta_decreto      mov",
                  " WHERE  det.folio = mov.folio_liquida ",
                  " AND    det.id_referencia = mov.id_referencia ",
                  " AND    glo.folio = det.folio ",
                  " AND    glo.proceso_cod = 1701 ",
                  " AND    det.id_decreto = afi.id_decreto ",
                  " AND    det.result_operacion IN ('01','99') "
                  
   IF v_f_inicial IS NOT NULL AND v_f_finaL IS NOT NULL THEN
   	  LET v_query = v_query," AND mov.f_liquida BETWEEN '",v_f_inicial, "' AND '",v_f_final, "'"
   END IF
   
   IF v_nss IS NOT NULL THEN
      LET v_query = v_query, "\n AND    det.nss_afo_recep = '", v_nss, "'"
   END IF

   IF v_id_unico IS NOT NULL THEN
      LET v_query = v_query, "\n AND    afi.consec_cuenta = ", v_id_unico," "
   END IF

   IF v_rfc IS NOT NULL THEN
      LET v_query = v_query, "\n AND    afi.rfc = '", v_rfc, "'"
   END IF   

   LET v_indice = 1
   PREPARE consulta FROM v_query
   DECLARE cur_consulta CURSOR FOR consulta
   FOREACH cur_consulta INTO arr_aceptados_p.* 
      LET arr_aceptados[v_indice].f_liquidacion    = arr_aceptados_p.f_liquidacion
      LET arr_aceptados[v_indice].nombre_archivo   = arr_aceptados_p.nombre_archivo
      LET arr_aceptados[v_indice].folio            = arr_aceptados_p.folio
      LET arr_aceptados[v_indice].nss_afo_recep    = arr_aceptados_p.nss_afo_recep
      LET arr_aceptados[v_indice].curp             = arr_aceptados_p.curp
      LET arr_aceptados[v_indice].nombre           = arr_aceptados_p.paterno CLIPPED," ",arr_aceptados_p.materno CLIPPED," ",arr_aceptados_p.nombres CLIPPED
      LET arr_aceptados[v_indice].consec_cuenta    = arr_aceptados_p.consec_cuenta
      LET arr_aceptados[v_indice].monto_pesos      = arr_aceptados_p.monto_pesos
      LET arr_aceptados[v_indice].monto_acciones   = arr_aceptados_p.monto_acciones
      LET arr_aceptados[v_indice].result_operacion = arr_aceptados_p.result_operacion

      LET v_indice = v_indice + 1
   END FOREACH

END FUNCTION

FUNCTION Busca_rechazados(v_nss,v_id_unico,v_rfc,v_f_inicial,v_f_final)
   DEFINE arr_rechazados_p   RECORD
          f_liquidacion     DATE,
          nombre_archivo    CHAR(40),
          folio             DECIMAL(9,0),        
          nss_afo_recep     CHAR(11),
          curp              CHAR(18),
          paterno           CHAR(40),
          materno           CHAR(40),
          nombres           CHAR(40),
          consec_cuenta     DECIMAL(11,0),
          monto_pesos       DECIMAL(12,2),
          monto_acciones    DECIMAL(16,6),
          result_operacion  CHAR(45)
   END RECORD
   
   DEFINE v_f_inicial DATE
   DEFINE v_f_final   DATE
   DEFINE v_nss       CHAR(11)
   DEFINE v_id_unico  DECIMAL(11,0)
   DEFINE v_rfc       CHAR(13)
   DEFINE v_indice    INTEGER
   DEFINE v_query     STRING
   DEFINE v_contador  INTEGER

   LET v_query = "SELECT glo.f_actualiza,",
                          "glo.nombre_archivo,",
                          "det.folio,",
                          "det.nss_afo_recep,",
                          "det.curp,",
                          "det.paterno_afo_recep,",
                          "det.materno_afo_recep,",
                          "det.nombres_afo_recep,",
                          "afi.consec_cuenta,",
                          "det.sdo_viv92,",
                          "det.aivs_viv92,",
                          "CASE det.result_operacion ",
                          "   WHEN '02' THEN '2 SIN CONSECUTIVO ÚNICO' ",
                          "   WHEN '03' THEN '3 MTO SOLIC EN AIVS NO ENCONTRADO' ",
                          "   WHEN '04' THEN '4 NSS AFORE RECEPTORA NO ENCONTRADO EN AFORE' ",                          
                          "   WHEN '05' THEN '5 AIV EN CERO, NO SE LIQUIDADA' ",
                          "   WHEN '08' THEN '8 SIN CURP EN AFILIATORIO' ",
                          "   WHEN '10' THEN '10 AIV EN HISTÓRICO, EN ESPERA AUTORIZAR' ",
                          "END ",                          
                   "FROM   tia_det_traspaso det,",
                          "glo_ctr_archivo  glo,",
                          "afi_decreto      afi ",
                  " WHERE  glo.folio = det.folio ",
                  " AND    glo.proceso_cod = 1701 ",
                  " AND    det.id_decreto = afi.id_decreto ",
                  " AND    det.result_operacion NOT IN ('01','99') "

   IF v_f_inicial IS NOT NULL AND v_f_finaL IS NOT NULL THEN
   	  LET v_query = v_query," AND glo.f_actualiza BETWEEN '",v_f_inicial, "' AND '",v_f_final, "'"
   END IF
   
   IF v_nss IS NOT NULL THEN
      LET v_query = v_query, "\n AND    det.nss_afo_recep = '", v_nss, "'"
   END IF

   IF v_id_unico IS NOT NULL THEN
      LET v_query = v_query, "\n AND    afi.consec_cuenta = ", v_id_unico," "
   END IF

   IF v_rfc IS NOT NULL THEN
      LET v_query = v_query, "\n AND    afi.rfc = '", v_rfc, "'"
   END IF   

   LET v_indice = 1
   PREPARE consulta_r FROM v_query
   DECLARE cur_consulta_r CURSOR FOR consulta_r
   FOREACH cur_consulta_r INTO arr_rechazados_p.* 
      LET arr_rechazados[v_indice].f_liquidacion    = arr_rechazados_p.f_liquidacion
      LET arr_rechazados[v_indice].nombre_archivo   = arr_rechazados_p.nombre_archivo
      LET arr_rechazados[v_indice].folio            = arr_rechazados_p.folio
      LET arr_rechazados[v_indice].nss_afo_recep    = arr_rechazados_p.nss_afo_recep
      LET arr_rechazados[v_indice].curp             = arr_rechazados_p.curp
      LET arr_rechazados[v_indice].nombre           = arr_rechazados_p.paterno CLIPPED," ",arr_rechazados_p.materno CLIPPED," ",arr_rechazados_p.nombres CLIPPED
      LET arr_rechazados[v_indice].consec_cuenta    = arr_rechazados_p.consec_cuenta
      LET arr_rechazados[v_indice].monto_pesos      = arr_rechazados_p.monto_pesos
      LET arr_rechazados[v_indice].monto_acciones   = arr_rechazados_p.monto_acciones
      LET arr_rechazados[v_indice].result_operacion = arr_rechazados_p.result_operacion

      LET v_indice = v_indice + 1
   END FOREACH

END FUNCTION

FUNCTION Busca_ambos(v_nss,v_id_unico,v_rfc,v_f_inicial,v_f_final)

   DEFINE arr_aceptados_p   RECORD
          f_liquidacion     DATE,
          nombre_archivo    CHAR(40),
          folio             DECIMAL(9,0),        
          nss_afo_recep     CHAR(11),
          curp              CHAR(18),
          paterno           CHAR(40),
          materno           CHAR(40),
          nombres           CHAR(40),
          consec_cuenta     DECIMAL(11,0),
          monto_pesos       DECIMAL(12,2),
          monto_acciones    DECIMAL(16,6),
          result_operacion  CHAR(45)
   END RECORD
   
   DEFINE arr_rechazados_p   RECORD
          f_liquidacion     DATE,
          nombre_archivo    CHAR(40),
          folio             DECIMAL(9,0),        
          nss_afo_recep     CHAR(11),
          curp              CHAR(18),
          paterno           CHAR(40),
          materno           CHAR(40),
          nombres           CHAR(40),
          consec_cuenta     DECIMAL(11,0),
          monto_pesos       DECIMAL(12,2),
          monto_acciones    DECIMAL(16,6),
          result_operacion  CHAR(45)
   END RECORD
   
   DEFINE v_f_inicial DATE
   DEFINE v_f_final   DATE
   DEFINE v_nss       CHAR(11)
   DEFINE v_id_unico  DECIMAL(11,0)
   DEFINE v_rfc       CHAR(13)
   DEFINE v_indice    INTEGER
   DEFINE v_query     STRING
   DEFINE v_contador  INTEGER

   LET v_query = "SELECT mov.f_liquida,",
                          "glo.nombre_archivo,",
                          "det.folio,",
                          "det.nss_afo_recep,",
                          "det.curp,",
                          "det.paterno_afo_recep,",
                          "det.materno_afo_recep,",
                          "det.nombres_afo_recep,",
                          "afi.consec_cuenta,",
                          "mov.monto_pesos,",
                          "mov.monto_acciones,",
                          "CASE det.result_operacion ",
                          "   WHEN '01' THEN 'ACEPTADO' ",
                          "   WHEN '99' THEN 'ACEPTADO' ",
                          "END ",                          
                   "FROM   tia_det_traspaso det,",
                          "glo_ctr_archivo  glo,",
                          "afi_decreto      afi,",
                          "cta_decreto      mov",
                  " WHERE  det.folio = mov.folio_liquida ",
                  " AND    det.id_referencia = mov.id_referencia ",
                  " AND    glo.folio = det.folio ",
                  " AND    glo.proceso_cod = 1701 ",
                  " AND    det.id_decreto = afi.id_decreto ",
                  " AND    det.result_operacion IN ('01','99') "
                  
   IF v_f_inicial IS NOT NULL AND v_f_finaL IS NOT NULL THEN
   	  LET v_query = v_query," AND mov.f_liquida BETWEEN '",v_f_inicial, "' AND '",v_f_final, "'"
   END IF
   
   IF v_nss IS NOT NULL THEN
      LET v_query = v_query, "\n AND    det.nss_afo_recep = '", v_nss, "'"
   END IF

   IF v_id_unico IS NOT NULL THEN
      LET v_query = v_query, "\n AND    afi.consec_cuenta = ", v_id_unico," "
   END IF

   IF v_rfc IS NOT NULL THEN
      LET v_query = v_query, "\n AND    afi.rfc = '", v_rfc, "'"
   END IF   

   LET v_indice = 1
   PREPARE consulta_a2 FROM v_query
   DECLARE cur_consulta_a2 CURSOR FOR consulta_a2
   FOREACH cur_consulta_a2 INTO arr_aceptados_p.* 
      LET arr_aceptados[v_indice].f_liquidacion    = arr_aceptados_p.f_liquidacion
      LET arr_aceptados[v_indice].nombre_archivo   = arr_aceptados_p.nombre_archivo
      LET arr_aceptados[v_indice].folio            = arr_aceptados_p.folio
      LET arr_aceptados[v_indice].nss_afo_recep    = arr_aceptados_p.nss_afo_recep
      LET arr_aceptados[v_indice].curp             = arr_aceptados_p.curp
      LET arr_aceptados[v_indice].nombre           = arr_aceptados_p.paterno CLIPPED," ",arr_aceptados_p.materno CLIPPED," ",arr_aceptados_p.nombres CLIPPED
      LET arr_aceptados[v_indice].consec_cuenta    = arr_aceptados_p.consec_cuenta
      LET arr_aceptados[v_indice].monto_pesos      = arr_aceptados_p.monto_pesos
      LET arr_aceptados[v_indice].monto_acciones   = arr_aceptados_p.monto_acciones
      LET arr_aceptados[v_indice].result_operacion = arr_aceptados_p.result_operacion

      LET v_indice = v_indice + 1
   END FOREACH
   
--=======================
   
   LET v_query = "SELECT glo.f_actualiza,",
                          "glo.nombre_archivo,",
                          "det.folio,",
                          "det.nss_afo_recep,",
                          "det.curp,",
                          "det.paterno_afo_recep,",
                          "det.materno_afo_recep,",
                          "det.nombres_afo_recep,",
                          "afi.consec_cuenta,",
                          "det.sdo_viv92,",
                          "det.aivs_viv92,",                          
                          "CASE det.result_operacion ",
                          "   WHEN '02' THEN '2 SIN CONSECUTIVO ÚNICO' ",
                          "   WHEN '03' THEN '3 MTO SOLIC EN AIVS NO ENCONTRADO' ",
                          "   WHEN '04' THEN '4 NSS AFORE RECEPTORA NO ENCONTRADO EN AFORE' ",                          
                          "   WHEN '05' THEN '5 AIV EN CERO, NO SE LIQUIDADA' ",
                          "   WHEN '08' THEN '8 SIN CURP EN AFILIATORIO' ",
                          "   WHEN '10' THEN '10 AIV EN HISTÓRICO, EN ESPERA AUTORIZAR' ",
                          "END ",                          
                   "FROM   tia_det_traspaso det,",
                          "glo_ctr_archivo  glo,",
                          "afi_decreto      afi ",
                  " WHERE  glo.folio = det.folio ",
                  " AND    glo.proceso_cod = 1701 ",
                  " AND    det.id_decreto = afi.id_decreto ",
                  " AND    det.result_operacion NOT IN ('01','99') "
                  
   IF v_f_inicial IS NOT NULL AND v_f_finaL IS NOT NULL THEN
   	  LET v_query = v_query," AND glo.f_actualiza BETWEEN '",v_f_inicial, "' AND '",v_f_final, "'"
   END IF
   
   IF v_nss IS NOT NULL THEN
      LET v_query = v_query, "\n AND    det.nss_afo_recep = '", v_nss, "'"
   END IF

   IF v_id_unico IS NOT NULL THEN
      LET v_query = v_query, "\n AND    afi.consec_cuenta = ", v_id_unico," "
   END IF

   IF v_rfc IS NOT NULL THEN
      LET v_query = v_query, "\n AND    afi.rfc = '", v_rfc, "'"
   END IF   

   LET v_indice = 1
   PREPARE consulta_r2 FROM v_query
   DECLARE cur_consulta_r2 CURSOR FOR consulta_r2
   FOREACH cur_consulta_r2 INTO arr_rechazados_p.* 
      LET arr_rechazados[v_indice].f_liquidacion    = arr_rechazados_p.f_liquidacion
      LET arr_rechazados[v_indice].nombre_archivo   = arr_rechazados_p.nombre_archivo
      LET arr_rechazados[v_indice].folio            = arr_rechazados_p.folio
      LET arr_rechazados[v_indice].nss_afo_recep    = arr_rechazados_p.nss_afo_recep
      LET arr_rechazados[v_indice].curp             = arr_rechazados_p.curp
      LET arr_rechazados[v_indice].nombre           = arr_rechazados_p.paterno CLIPPED," ",arr_rechazados_p.materno CLIPPED," ",arr_rechazados_p.nombres CLIPPED
      LET arr_rechazados[v_indice].consec_cuenta    = arr_rechazados_p.consec_cuenta
      LET arr_rechazados[v_indice].monto_pesos      = arr_rechazados_p.monto_pesos
      LET arr_rechazados[v_indice].monto_acciones   = arr_rechazados_p.monto_acciones
      LET arr_rechazados[v_indice].result_operacion = arr_rechazados_p.result_operacion

      LET v_indice = v_indice + 1
   END FOREACH   


END FUNCTION

FUNCTION fn_exporta_archivo(p_arr_aceptados, p_arr_rechazados, p_exporta_opcion)  --proinfxvii-81

   DEFINE p_arr_aceptados DYNAMIC ARRAY OF RECORD
          f_liquidacion    DATE,
          nombre_archivo   CHAR(40),
          folio            DECIMAL(9,0),
          nss_afo_recep    CHAR(11),
          curp             CHAR(18),
          nombre           CHAR(120),
          consec_cuenta    DECIMAL(11,0),
          monto_pesos      DECIMAL(12,2),
          monto_acciones   DECIMAL(16,6),
          result_operacion CHAR(45)
   END RECORD
   
   DEFINE p_arr_rechazados DYNAMIC ARRAY OF RECORD
          f_liquidacion    DATE,
          nombre_archivo   CHAR(40),
          folio            DECIMAL(9,0),
          nss_afo_recep    CHAR(11),
          curp             CHAR(18),
          nombre           CHAR(120),
          consec_cuenta    DECIMAL(11,0),
          monto_pesos      DECIMAL(12,2),
          monto_acciones   DECIMAL(16,6),
          result_operacion CHAR(45)
   END RECORD

   DEFINE p_exporta_opcion CHAR(15)
   
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_v_ruta_nomarch2       STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_cuenta                INTEGER,
      v_solicitud             DECIMAL(9,0),
      v_tipo                  CHAR(1),
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_contador              INTEGER

   LET v_regresa = FALSE
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "tia"

   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo de Rechazos
   LET v_nom_archivo = "Consulta_Rechazos_TIA", TODAY USING "yyyymmdd", "_", v_hora[1,2], v_hora[4,5], v_hora[7,8]
   LET v_archivo_txt = v_nom_archivo, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se genera el nombre del archivo de Aceptados
   LET v_nom_archivo = "Consulta_Aceptados_TIA", TODAY USING "yyyymmdd", "_", v_hora[1,2], v_hora[4,5], v_hora[7,8]
   LET v_archivo_txt = v_nom_archivo, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Aceptados
   LET v_v_ruta_nomarch2 = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   IF p_exporta_opcion = 2 THEN 
      LET v_mensaje_archivo = "Se generará el archivo:\n\n\t", v_v_ruta_nomarch
   ELSE 
      IF p_exporta_opcion = 1 THEN 
         LET v_mensaje_archivo = "Se generará el archivos:\n\n\t", v_v_ruta_nomarch2
      ELSE 
         LET v_mensaje_archivo = "Se generaran los siguientes archivos:\n\n\t", v_v_ruta_nomarch, "\n\t", v_v_ruta_nomarch2
      END IF 
   END IF 
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado
   DISPLAY "~~~~~~~~~~~"
   DISPLAY "Archivo de rechazados generado: ", v_v_ruta_nomarch

   IF p_exporta_opcion = 2 OR p_exporta_opcion = 3 THEN 
      -- Se crea el manejador de archivo
      LET v_ch_arch_ret_generico = base.Channel.create()
      CALL v_ch_arch_ret_generico.setDelimiter(NULL)

      -- Se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
      -- Escribe el encabezado del archivo de Rechazados
      LET v_s_detalle = "FECHA RECHAZO|NOMBRE ARCHIVO|FOLIO|NSS|CURP|",
                        "NOMBRE DEL TRABAJADOR|",
                        "ID UNICO|SALDO|AIVS|RESULTADO"

      CALL v_ch_arch_ret_generico.write(v_s_detalle)

      -- Ahora imprimo todos los registros Rechazados

      FOR v_contador = 1 TO p_arr_rechazados.getLength()
         LET v_s_detalle = p_arr_rechazados[v_contador].f_liquidacion USING "dd/mm/yyyy","|",
                           p_arr_rechazados[v_contador].nombre_archivo CLIPPED, "|",
                           p_arr_rechazados[v_contador].folio, "|",
                           p_arr_rechazados[v_contador].nss_afo_recep, "|",
                           p_arr_rechazados[v_contador].curp CLIPPED, "|",
                           p_arr_rechazados[v_contador].nombre, "|",
                           p_arr_rechazados[v_contador].consec_cuenta CLIPPED, "|",
                           p_arr_rechazados[v_contador].monto_pesos USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_rechazados[v_contador].monto_acciones USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_rechazados[v_contador].result_operacion, "|"

                           
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
      END FOR 

      -- Se cierra el archivo de Rechazados
      CALL v_ch_arch_ret_generico.close()
   END IF 

   IF p_exporta_opcion = 1 OR p_exporta_opcion = 3 THEN 

      -- Se crea el manejador de archivo
      LET v_ch_arch_ret_generico = base.Channel.create()
      CALL v_ch_arch_ret_generico.setDelimiter(NULL)

      -- Se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch2, "w" )
      -- Escribe el encabezado del archivo de Aceptados
      LET v_s_detalle = "FECHA ACEPTADO|NOMBRE ARCHIVO|FOLIO|NSS|CURP|",
                        "NOMBRE DEL TRABAJADOR|",
                        "ID UNICO|SALDO|AIVS|RESULTADO"

      CALL v_ch_arch_ret_generico.write(v_s_detalle)

      -- Ahora imprimo todos los registros Aceptados

      FOR v_contador = 1 TO p_arr_aceptados.getLength()

         LET v_s_detalle = p_arr_aceptados[v_contador].f_liquidacion USING "dd/mm/yyyy","|",
                           p_arr_aceptados[v_contador].nombre_archivo CLIPPED, "|",
                           p_arr_aceptados[v_contador].folio, "|",
                           p_arr_aceptados[v_contador].nss_afo_recep, "|",
                           p_arr_aceptados[v_contador].curp CLIPPED, "|",
                           p_arr_aceptados[v_contador].nombre, "|",
                           p_arr_aceptados[v_contador].consec_cuenta CLIPPED, "|",
                           p_arr_aceptados[v_contador].monto_pesos USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_aceptados[v_contador].monto_acciones USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_aceptados[v_contador].result_operacion, "|"      	
                          
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
      END FOR 
   END IF 

   -- Se cierra el archivo de Aceptados
   CALL v_ch_arch_ret_generico.close()
   IF p_exporta_opcion = 2 THEN 
      LET v_mensaje_archivo = "Archivos generado exitosamente:\n\n\t", v_v_ruta_nomarch
   ELSE 
      IF p_exporta_opcion = 1 THEN 
         LET v_mensaje_archivo = "Archivos generado exitosamente:\n\n\t", v_v_ruta_nomarch2
      ELSE 
         LET v_mensaje_archivo = "Los archivos fueron generados exitosamente:\n\n\t", v_v_ruta_nomarch, "\n\t", v_v_ruta_nomarch2
      END IF 
   END IF 

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION
