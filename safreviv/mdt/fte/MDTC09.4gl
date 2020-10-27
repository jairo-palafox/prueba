--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
--===============================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTC09                                                        #
#Objetivo     =>                                                               #
#Fecha inicio => Marzo 14, 2012                                                #
################################################################################
DATABASE safre_viv
DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_folios   DYNAMIC ARRAY OF RECORD
        v_folio_dispersion  LIKE glo_folio.folio,
        v_fecha_dispersion  DATE,
        v_folio_pago        LIKE glo_folio.folio_referencia,
        v_fecha_pago        DATE,
        v_usuario           LIKE glo_folio.usuario,
        v_id_derechohabinte LIKE afi_derechohabiente.id_derechohabiente
       END RECORD,
       p_usuario         LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_cad_ventana     STRING,
       v_ventana         ui.Window,
       v_instrucciones_aplicadas DYNAMIC ARRAY OF RECORD
        v_descripcion            STRING,
        v_padre                  STRING,
        v_id                     STRING,
        v_extendido              SMALLINT,
        --v_aivs                   LIKE mdt_det_aplica_monto.monto_acciones,
        v_referencia             LIKE cta_movimiento.id_referencia,
        v_pesos                  LIKE mdt_det_aplica_monto.monto_pesos
       END RECORD

MAIN
DEFINE v_filtro            STRING,
       v_continua          BOOLEAN,
       v_continua_consulta BOOLEAN,
       v_rec_instr_aplic   BOOLEAN -- Variable para indicar si se ha recuperado instrucciones de mandatos aplicadas al NSS seleccionado

   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   
   # Recupera la ruta ejecutable de mandatos
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "mdt"

   LET v_continua_consulta = TRUE
   
   OPEN WINDOW vtn_consulta_preliquidacion WITH FORM v_ruta_ejecutable CLIPPED||"/MDTC091"
      WHILE v_continua_consulta
         #Se asigna el titulo de la ventana
         IF(p_cad_ventana IS NOT NULL)THEN
            CALL ui.Interface.setText(p_cad_ventana)
            LET v_ventana = ui.Window.getCurrent()
            CALL v_ventana.setText(p_cad_ventana)
         END IF
         # Construye el filtro de busqueda
         CALL fn_construye_filtro_consulta() RETURNING v_filtro, v_continua
         IF NOT(v_continua)THEN
            LET v_continua_consulta = FALSE      
            --EXIT PROGRAM
         ELSE
            CALL v_folios.clear()
            # Realiza la consulta de de los folios según los criterios de búsqueda
            CALL fn_recupera_folios(v_filtro) RETURNING v_continua
            IF NOT(v_continua)THEN
               # No se ha encontrado algun resultado
               CALL fn_mensaje(p_cad_ventana,"No se encontraron registros con criterio dado","about")
            ELSE
               DIALOG  ATTRIBUTES (UNBUFFERED , FIELD ORDER FORM)
                  DISPLAY ARRAY v_folios TO sr_folios.*
                                   --ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
  
                     BEFORE DISPLAY

                    BEFORE ROW
                        # Recupera las instrucciones aplicadas para determinado folio
                        CALL fn_recupera_instrucciones_aplicadas(v_folios[ARR_CURR()].v_folio_dispersion,v_folios[ARR_CURR()].v_folio_pago,v_folios[ARR_CURR()].v_id_derechohabinte)
                                                            RETURNING v_rec_instr_aplic


                  END DISPLAY
                  DISPLAY ARRAY v_instrucciones_aplicadas TO tree_instrucciones.*
                                         --ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

                  END DISPLAY

                  ON ACTION aceptar
                     ACCEPT DIALOG

                  ON ACTION cancelar
                     LET v_continua_consulta = FALSE 
                     EXIT DIALOG
                  
               END DIALOG
            END IF 
         END IF
      END WHILE
   CLOSE WINDOW vtn_consulta_preliquidacion
END MAIN

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTC09                                                  #
#Objetivo          => Elabora el filtro de busqueda de folios                 #
#Autor             => Hugo César Ramírez Gracía                               #
#Fecha Inicio      => Marzo 14, 2012                                          #
###############################################################################
FUNCTION fn_construye_filtro_consulta()
DEFINE v_filtro           STRING,
       v_continua         BOOLEAN,
       v_fecha_inicio     DATE,
       v_folio_dispersion STRING,--LIKE glo_folio.folio,
       v_fecha_fin        DATE,
       v_folio_pago       STRING,--LIKE glo_folio.folio_referencia,
       v_nss              LIKE afi_derechohabiente.nss


   {CONSTRUCT v_filtro ON vxcv,fdgfdg  FROM dtedi_fecha_inicio, edi_folio_dispersion, 
                                dtedi_fecha_fin, edi_folio_pago, edi_nss
      BEFORE CONSTRUCT
         # Inicializa variables
         LET v_continua = TRUE
         LET v_filtro   = " 1=1 "

      AFTER CONSTRUCT
         IF NOT(LENGTH(GET_FLDBUF(edi_nss)) > 0)THEN
           ERROR "Capture NSS" ATTRIBUTE(REVERSE, BOLD, YELLOW)
           NEXT FIELD edi_nss 
        END IF

      AFTER FIELD edi_nss 
        IF NOT(LENGTH(GET_FLDBUF(edi_nss)) > 0)THEN
           ERROR "Capture NSS" ATTRIBUTE(REVERSE, BOLD, YELLOW)
           NEXT FIELD edi_nss 
        END IF
        

      ON ACTION ACCEPT
         ACCEPT CONSTRUCT

      ON ACTION CANCEL
         # Se cancela la consulta
         LET v_continua = TRUE
         EXIT CONSTRUCT

   END CONSTRUCT}
   INITIALIZE v_fecha_inicio, v_folio_dispersion, v_fecha_fin, v_folio_pago, v_nss TO NULL
   INPUT v_fecha_inicio, 
         v_folio_dispersion, 
         v_fecha_fin, 
         v_folio_pago, 
         v_nss 
    FROM dtedi_fecha_inicio, 
         edi_folio_dispersion, 
         dtedi_fecha_fin, 
         edi_folio_pago, 
         edi_nss
         
         ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE, WITHOUT DEFAULTS) 

      BEFORE INPUT
         # Inicializa variables
         LET v_continua = TRUE
         LET v_filtro   = "\n "

      AFTER FIELD edi_nss 
         CALL GET_FLDBUF(edi_nss) RETURNING v_nss 
         # Revisa que se haya capturado el NSS
         {IF NOT(LENGTH(GET_FLDBUF(edi_nss)) > 0)THEN
            ERROR "Capture NSS" ATTRIBUTE(REVERSE, BOLD, YELLOW)
            NEXT FIELD edi_nss 
         END IF}

      AFTER FIELD edi_folio_dispersion
         CALL GET_FLDBUF(edi_folio_dispersion) RETURNING v_folio_dispersion
         # Valida que sea numerico
         {IF(v_folio_dispersion MATCHES "*[^0-9]*")THEN
            MESSAGE "Capture folio de dispersión válido" ATTRIBUTE(REVERSE, BOLD)
            NEXT FIELD edi_folio_dispersion 
         END IF}

      AFTER FIELD edi_folio_pago
         CALL GET_FLDBUF(edi_folio_pago) RETURNING v_folio_pago
         # Valida que sea numerico
         {IF(v_folio_dispersion MATCHES "*[^0-9]*")THEN
            MESSAGE "Capture folio de pago válido" ATTRIBUTE(REVERSE, BOLD)
            NEXT FIELD v_folio_pago 
         END IF}

      AFTER FIELD dtedi_fecha_fin
         CALL GET_FLDBUF(dtedi_fecha_inicio) RETURNING v_fecha_inicio
         CALL GET_FLDBUF(dtedi_fecha_fin) RETURNING v_fecha_fin
         # Se valida que la fecha fin sea mayor que la fecha inicio
         {IF( v_fecha_inicio > v_fecha_fin)THEN
            MESSAGE "Fecha fin debe ser mayor que fecha inicio" ATTRIBUTE(REVERSE, BOLD)
            NEXT FIELD dtedi_fecha_fin
         END IF}

      ON ACTION aceptar
         CALL GET_FLDBUF(edi_folio_pago) RETURNING v_folio_pago
         CALL GET_FLDBUF(edi_folio_dispersion) RETURNING v_folio_dispersion
         CALL GET_FLDBUF(dtedi_fecha_inicio) RETURNING v_fecha_inicio
         CALL GET_FLDBUF(dtedi_fecha_fin) RETURNING v_fecha_fin
         
         IF( v_fecha_inicio > v_fecha_fin)THEN
            CALL fn_mensaje("AVISO","Fecha fin debe ser mayor que fecha inicio","information")
            --MESSAGE "Fecha fin debe ser mayor que fecha inicio" ATTRIBUTE(REVERSE, BOLD)
            NEXT FIELD dtedi_fecha_fin
         END IF
         IF(v_folio_dispersion MATCHES "*[^0-9]*")THEN
            CALL fn_mensaje("AVISO","Capture folio de pago válido","information")
            --MESSAGE "Capture folio de pago válido" ATTRIBUTE(REVERSE, BOLD)
            NEXT FIELD v_folio_pago 
         END IF
         IF(v_folio_dispersion MATCHES "*[^0-9]*")THEN
            CALL fn_mensaje("AVISO","Capture folio de dispersión válido","information")
            --MESSAGE "Capture folio de dispersión válido" ATTRIBUTE(REVERSE, BOLD)
            NEXT FIELD edi_folio_dispersion 
         END IF
         # Revisa que se haya capturado el NSS
         IF NOT(LENGTH(GET_FLDBUF(edi_nss) CLIPPED) > 0)THEN
            CALL fn_mensaje("AVISO","Capture NSS","information")
            --ERROR "Capture NSS" ATTRIBUTE(REVERSE, BOLD, YELLOW)
            NEXT FIELD edi_nss 
         END IF
         ACCEPT INPUT


      ON ACTION cancelar
         LET v_continua = FALSE
         EXIT INPUT

      AFTER INPUT         
         IF(LENGTH(v_fecha_inicio CLIPPED) > 0 AND LENGTH(v_fecha_fin CLIPPED) > 0)THEN
            # Hace el between entre fechas
            LET v_filtro = "\n AND glo.f_actualiza BETWEEN '"||v_fecha_inicio||"' AND '"||v_fecha_fin||"'"
         ELSE
            IF(LENGTH(v_fecha_inicio CLIPPED) > 0)THEN
               # donde la fecha sea la de inicio
               LET v_filtro = "\n AND glo.f_actualiza = '"||v_fecha_inicio||"'"
            END IF
            IF(LENGTH(v_fecha_fin CLIPPED) > 0)THEN
               # donde la fecha sea la de fin
               LET v_filtro = "\n AND glo.f_actualiza = '"||v_fecha_fin||"'"
            END IF
         END IF
         IF(LENGTH(v_folio_dispersion CLIPPED) > 0)THEN
            # donde sea el folio de dispersion
            LET v_filtro = v_filtro||"\n AND glo.folio = "||v_folio_dispersion
         END IF
         IF(LENGTH(v_folio_pago CLIPPED) > 0)THEN
            # donde sea el folio de referencia
            LET v_filtro = v_filtro||"\n AND glo.folio_referencia = "||v_folio_pago
         END IF
         LET v_filtro = v_filtro||"\n AND afi.nss = '"||v_nss||"'"

   END INPUT
   
   RETURN v_filtro, v_continua
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTC09                                                  #
#Objetivo          => Recupera los folios según el filtro de búsqueda         #
#Autor             => Hugo César Ramírez Gracía                               #
#Fecha Inicio      => Marzo 14, 2012                                          #
###############################################################################
FUNCTION fn_recupera_folios(v_filtro)
DEFINE v_filtro    STRING,
       v_consulta  STRING,
       v_indice    SMALLINT

   WHENEVER ERROR CONTINUE
   CALL v_folios.clear()
   INITIALIZE v_folios[1].* TO NULL
   # Recupera los folios relacionados a un NSS que se les ha aplicado la amortizacion de instrucciones de mandatos
   {LET v_consulta = "\n SELECT glo.folio, glo.f_actualiza, glo.folio_referencia, ",
                    "\n        mdt.periodo_pago, glo.usuario, afi.id_derechohabiente",
                    "\n   FROM glo_folio glo JOIN mdt_det_aplica_mandato mdt",
                    "\n     ON mdt.folio_dispersion = glo.folio",
                    "\n    AND mdt.folio_pago = glo.folio_referencia",
                    "\n        JOIN afi_derechohabiente afi",
                    "\n     ON mdt.id_derechohabiente = afi.id_derechohabiente",
                    "\n  WHERE 1=1 ",v_filtro,
                    "\n GROUP BY glo.folio, glo.f_actualiza, glo.folio_referencia,mdt.periodo_pago,",
                    "\n          mdt.periodo_pago,glo.usuario, afi.id_derechohabiente"}
   LET v_consulta = "\n SELECT DISTINCT glo.folio, glo.f_actualiza, glo.folio_referencia,",
                    "\n        ctr.f_liquida_pago, glo.usuario, afi.id_derechohabiente",
                    "\n   FROM glo_folio glo JOIN mdt_det_aplica_mandato mdt",
                    "\n     ON mdt.folio_dispersion = glo.folio",
                    "\n    AND mdt.folio_pago = glo.folio_referencia",
                    "\n        JOIN afi_derechohabiente afi",
                    "\n     ON mdt.id_derechohabiente = afi.id_derechohabiente",
                    "\n        JOIN mdt_ctr_aplica_mandato ctr",
                    "\n     ON mdt.id_ctr_aplica_mandato = ctr.id_ctr_aplica_mandato",
                    "\n  WHERE 1=1 ",v_filtro,
                    "\n GROUP BY glo.folio, glo.f_actualiza, glo.folio_referencia,mdt.periodo_pago,",
                    "\n          ctr.f_liquida_pago,glo.usuario, afi.id_derechohabiente"
                    
   PREPARE prp_rec_folios FROM v_consulta
   DECLARE cur_rec_folios CURSOR FOR prp_rec_folios
   LET v_indice = 1
   FOREACH cur_rec_folios INTO v_folios[v_indice].*
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_folios
   # Elimina el ultimo registro si es nulo
   IF(v_folios[v_folios.getLength()].v_folio_dispersion IS NULL)THEN
      CALL v_folios.deleteElement(v_folios.getLength())
   END IF
   IF(v_folios.getLength() > 0)THEN
      # Se recuperó información y se continua con la consulta   
      RETURN TRUE
   ELSE
      # No se recuperó información y no se continua con la consulta   
      RETURN FALSE
   END IF
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTC09                                                  #
#Objetivo          => Recupera las instrucciones que se han aplicado para     #
#                     el NSS y folio respectivos                              #
#Autor             => Hugo César Ramírez Gracía                               #
#Fecha Inicio      => Marzo 15, 2012                                          #
###############################################################################
FUNCTION fn_recupera_instrucciones_aplicadas(p_folio_dispersion, p_folio_pago,p_id_derechohabiente)
DEFINE p_folio_dispersion LIKE glo_folio.folio,
       p_folio_pago       LIKE glo_folio.folio_referencia,
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_consulta         STRING,
       v_indice           SMALLINT,
       v_instrucciones     RECORD
        v_tpo_mandato      LIKE mdt_tpo_mandato.tpo_mandato,
        v_desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
        v_id_cat_mandato   LIKE mdt_cat_mandato.id_cat_mandato,
        v_desc_mandato     LIKE mdt_cat_mandato.desc_mandato,
        v_monto_acciones   LIKE mdt_det_aplica_monto.monto_acciones,
        v_monto_pesos      LIKE mdt_det_aplica_monto.monto_pesos
       END RECORD,
       v_arbol  RECORD
        v_id_referencia      LIKE cta_movimiento.id_referencia,
        v_amortizacion_bruta LIKE cta_movimiento.monto_pesos,
        v_desc_bruta         LIKE cta_movimiento.origen,
        v_amortizacion_real  LIKE cta_movimiento.monto_pesos,
        v_mandato            LIKE cta_movimiento.origen,
        v_monto_mandato      LIKE cta_movimiento.monto_pesos
       END RECORD,
       v_arbol_tmp  RECORD
        v_id_referencia      LIKE cta_movimiento.id_referencia,
        v_amortizacion_bruta LIKE cta_movimiento.monto_pesos,
        v_desc_bruta         LIKE cta_movimiento.origen,
        v_amortizacion_real  LIKE cta_movimiento.monto_pesos,
        v_mandato            LIKE cta_movimiento.origen,
        v_monto_mandato      LIKE cta_movimiento.monto_pesos
       END RECORD,
       v_inserta_mto_real    BOOLEAN,
       v_padre_aux         LIKE cta_movimiento.id_referencia,
       v_agrega_amto_real  BOOLEAN
       
   WHENEVER ERROR CONTINUE
   LET v_agrega_amto_real = FALSE
   CALL v_instrucciones_aplicadas.clear()
   INITIALIZE v_instrucciones_aplicadas[1].* TO NULL
   LET v_padre_aux = 0
   
   LET v_consulta = "\n select unique",
                    "\n        a.id_referencia     RefPago     ,",
                    "\n        b.monto_pesos       PagoBruto   ,",
                    "\n        a.origen            descBruta   ,",
                    "\n        a.monto_pesos       Real        ,",
                    "\n        f.origen            Mandato     ,",
                    "\n        f.monto_pesos       pesMandato",
                    "\n from cta_movimiento         a  ,",
                    "\n      cta_movimiento         b  ,",
                    "\n      mdt_ctr_aplica_mandato c  ,",
                    "\n      mdt_det_aplica_monto   d  ,",
                    "\n      cta_movimiento         f",
                    "\n where a.folio_liquida      = ?",
                    "\n and   a.id_derechohabiente = ?",
                    "\n and   c.folio_dispersion   = a.folio_liquida",
                    "\n and   c.folio_pago         = b.folio_liquida",
                    "\n and   a.id_derechohabiente = b.id_derechohabiente",
                    --"\n and   a.id_derechohabiente = ?",
                    --"\n and   a.id_referencia      = b.id_referencia",
                    "\n and   a.folio_liquida      = f.folio_liquida",
                    "\n and   a.id_derechohabiente = f.id_derechohabiente",
                    "\n and   b.id_derechohabiente = f.id_derechohabiente",
                    "\n and   f.id_referencia      = d.id_det_aplica_monto",
                    "\n and   b.subcuenta = 41",
                    "\n and   b.monto_pesos = abs(a.monto_pesos + f.monto_pesos)"
   PREPARE prp_rec_instrucciones_aplicadas FROM v_consulta 
   DECLARE cur_rec_instrucciones_aplicadas CURSOR FOR prp_rec_instrucciones_aplicadas
   LET v_indice = 1
   LET v_inserta_mto_real = TRUE
      
   FOREACH cur_rec_instrucciones_aplicadas USING p_folio_dispersion,
                                                 p_id_derechohabiente
                                                 --p_id_derechohabiente
                                           INTO v_arbol.*
      
      IF(v_padre_aux <> v_arbol.v_id_referencia)THEN
         DISPLAY "Padre:",v_arbol.v_id_referencia, "  Hijo:",v_arbol.v_mandato
         
         # ultimo nodo para cado nodo raiz
         IF(v_arbol_tmp.v_id_referencia IS NOT NULL)THEN        
            LET v_instrucciones_aplicadas[v_indice].v_descripcion = "AMORTIZACIÓN REAL"
            LET v_instrucciones_aplicadas[v_indice].v_padre       = v_arbol_tmp.v_id_referencia
            LET v_instrucciones_aplicadas[v_indice].v_id          = v_arbol_tmp.v_id_referencia CLIPPED,".AMORTIZACIÓN REAL"
            LET v_instrucciones_aplicadas[v_indice].v_extendido   = 1
            --LET v_instrucciones_aplicadas[v_indice].v_aivs        = v_instrucciones.v_monto_acciones
            LET v_instrucciones_aplicadas[v_indice].v_pesos       = v_arbol_tmp.v_amortizacion_real
            LET v_indice = v_indice + 1 
         END IF
         # Nodo raiz
         LET v_instrucciones_aplicadas[v_indice].v_descripcion = v_arbol.v_desc_bruta
         LET v_instrucciones_aplicadas[v_indice].v_padre       = NULL
         LET v_instrucciones_aplicadas[v_indice].v_id          = v_arbol.v_id_referencia
         LET v_instrucciones_aplicadas[v_indice].v_extendido   = 1
         LET v_instrucciones_aplicadas[v_indice].v_referencia  = v_arbol.v_id_referencia
         LET v_instrucciones_aplicadas[v_indice].v_pesos       = v_arbol.v_amortizacion_bruta
            
         LET v_indice = v_indice + 1
         LET v_instrucciones_aplicadas[v_indice].v_descripcion = v_arbol.v_mandato
         LET v_instrucciones_aplicadas[v_indice].v_padre       = v_arbol.v_id_referencia
         LET v_instrucciones_aplicadas[v_indice].v_id          = v_arbol.v_id_referencia CLIPPED,".",v_arbol.v_mandato CLIPPED
         LET v_instrucciones_aplicadas[v_indice].v_extendido   = 1
         --LET v_instrucciones_aplicadas[v_indice].v_aivs        = v_instrucciones.v_monto_acciones
         LET v_instrucciones_aplicadas[v_indice].v_pesos       = v_arbol.v_monto_mandato
            
      ELSE
         DISPLAY "Hijo:",v_arbol.v_mandato
         
         LET v_instrucciones_aplicadas[v_indice].v_descripcion = v_arbol.v_mandato
         LET v_instrucciones_aplicadas[v_indice].v_padre       = v_arbol.v_id_referencia
         LET v_instrucciones_aplicadas[v_indice].v_id          = v_arbol.v_id_referencia CLIPPED,".",v_arbol.v_mandato CLIPPED
         LET v_instrucciones_aplicadas[v_indice].v_extendido   = 1
         --LET v_instrucciones_aplicadas[v_indice].v_aivs        = v_instrucciones.v_monto_acciones
         LET v_instrucciones_aplicadas[v_indice].v_pesos       = v_arbol.v_monto_mandato
      END IF
      
      LET v_arbol_tmp.* = v_arbol.*      
      LET v_indice = v_indice + 1
      LET v_padre_aux = v_arbol.v_id_referencia
   END FOREACH
   
   LET v_instrucciones_aplicadas[v_indice].v_descripcion = "AMORTIZACIÓN REAL"
   LET v_instrucciones_aplicadas[v_indice].v_padre       = v_arbol_tmp.v_id_referencia
   LET v_instrucciones_aplicadas[v_indice].v_id          = v_arbol_tmp.v_id_referencia CLIPPED,".AMORTIZACIÓN REAL"
   LET v_instrucciones_aplicadas[v_indice].v_extendido   = 1
   --LET v_instrucciones_aplicadas[v_indice].v_aivs        = v_instrucciones.v_monto_acciones
   LET v_instrucciones_aplicadas[v_indice].v_pesos       = v_arbol_tmp.v_amortizacion_real
            
   # Libera cursor
   FREE cur_rec_instrucciones_aplicadas
   --FREE cur_rec_desglose_instrucciones
   
   # Elimina la ultima posicion si es nula
   IF NOT(LENGTH(v_instrucciones_aplicadas[v_instrucciones_aplicadas.getLength()].v_descripcion) > 0)THEN
      CALL v_instrucciones_aplicadas.deleteElement(v_instrucciones_aplicadas.getLength())
   END IF
   # Elimina 'AMORTIZACIÓN REAL' si es que no se recuperó información
   IF(v_indice = 1)THEN
      CALL v_instrucciones_aplicadas.deleteElement(1)
   END IF
   # Se indica si se recupero información
   IF NOT(v_instrucciones_aplicadas.getLength() > 0)THEN
      RETURN FALSE
   ELSE
      RETURN TRUE
   END IF
END FUNCTION