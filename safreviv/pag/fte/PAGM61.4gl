--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 29/05/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGM61                                                        #
#Objetivo     => Cambio de importes a nss                                      #
#Fecha inicio => 29 Mayo de 2013                                               #
################################################################################
DATABASE safre_viv

DEFINE v_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_tipo_ejecucion  SMALLINT,
       v_titulo_vtna     STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ventana         ui.Window,
       v_detalle_nss     DYNAMIC ARRAY OF RECORD
         v_nss            LIKE afi_derechohabiente.nss,
         v_f_pago         LIKE pag_det_apvol.f_pago,
         v_imp_ap_vol     LIKE pag_det_apvol.imp_ap_vol,
         v_nom_trabajador LIKE pag_det_apvol.nom_trabajador,
         v_folio          LIKE pag_det_apvol.folio
       END RECORD
   
   DEFINE v_folio_nuevo DECIMAL(9,0),
          v_mensaje STRING
   
MAIN

   -- se recuperan los argumentos de la linea de comandos
   LET v_usuario_cod    = ARG_VAL(1)
   LET v_tipo_ejecucion = ARG_VAL(2)
   LET v_titulo_vtna    = ARG_VAL(3)

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "pag"

   CALL fn_cambia_nss_importes()

END MAIN

{===============================================================================
Nombre: fn_cambia_nss_importes
Fecha creacion: 29 Mayo del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Captura nss a cambiar por nuevo nss para los importes recuperados.
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_cambia_nss_importes()
DEFINE r_continuar  BOOLEAN,
       r_nss_actual LIKE afi_derechohabiente.nss,
       r_nss_nuevo  LIKE afi_derechohabiente.nss,
       r_folio      LIKE pag_det_apvol.folio,
       r_confirma   BOOLEAN,
       r_folio_liquidado BOOLEAN,
       v_error      BOOLEAN

   # se abre la ventana
   OPEN WINDOW vtna_cambio_nss WITH FORM v_ruta_ejecutable CLIPPED||"/PAGM611"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      IF(v_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(v_titulo_vtna)         
         CALL v_ventana.setText(v_titulo_vtna)
      END IF
      LET r_continuar = TRUE
      WHILE r_continuar
         CALL fn_captura_nss() RETURNING r_nss_actual,r_folio, r_continuar
         IF(r_continuar)THEN
            DISPLAY ARRAY v_detalle_nss TO sr_detalle_nss.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE,UNBUFFERED)
            
               ON ACTION cambio
                  CALL fn_valida_folio_liquidado(r_folio) RETURNING r_folio_liquidado
                  IF NOT(r_folio_liquidado)THEN
                     CALL fn_mensaje("Aviso","Folio no esta liquidado","information")
                     CALL v_detalle_nss.clear()
                     LET r_continuar = TRUE
                     EXIT DISPLAY
                  END IF
                  CALL fn_catptura_nss_nuevo() RETURNING r_nss_nuevo, r_continuar
                  IF(r_continuar)THEN
                     CALL fn_ventana_confirma("Confimar","¿Cambiar detalle de NSS actual "||r_nss_actual||" a NSS "||r_nss_nuevo||" para el folio "||r_folio||"?","info") RETURNING r_confirma
                     IF( r_confirma )THEN
                        CALL fn_actualiza_detalle_nss(r_nss_nuevo,r_nss_actual,r_folio) RETURNING v_error
                        IF(v_error)THEN
                           CALL fn_mensaje("Aviso","Ocurrió un error al actualizar información","information")
                           CONTINUE DISPLAY
                        ELSE
                           LET v_mensaje = "Cambio realizado correctamente con folio = ",v_folio_nuevo CLIPPED
--                           CALL fn_mensaje("Aviso","Cambio realizado correctamente"," information")
                           CALL fn_mensaje("Aviso",v_mensaje," information")
                           CALL v_detalle_nss.clear()
                           EXIT DISPLAY
                        END IF                        
                     ELSE
                        CONTINUE DISPLAY
                     END IF
                  ELSE
                     CONTINUE DISPLAY
                  END IF

               ON ACTION cancelar
                  CALL v_detalle_nss.clear()
                  EXIT DISPLAY

            END DISPLAY 
            
         END IF

      END WHILE

   CLOSE WINDOW vtna_cambio_nss

END FUNCTION

{===============================================================================
Nombre: fn_catptura_nss_nuevo
Fecha creacion: 29 Mayo del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Captura nss nuevo para cambio
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_catptura_nss_nuevo()
DEFINE v_nss_nuevo  LIKE afi_derechohabiente.nss,
       v_capturo    BOOLEAN,
       v_consulta   STRING,
       v_existe_nss BOOLEAN

   LET v_capturo = FALSE
   # se abre la ventana
   OPEN WINDOW vtna_captura_nss_nuevo WITH FORM v_ruta_ejecutable CLIPPED||"/PAGM612" ATTRIBUTE(STYLE = "dialog")
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      IF(v_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(v_titulo_vtna)         
         CALL v_ventana.setText(v_titulo_vtna)
      END IF

      INPUT v_nss_nuevo FROM nss_nuevo ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

         ON ACTION aceptar
            IF(v_nss_nuevo CLIPPED IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture NSS nuevo","information")
               CONTINUE INPUT
            END IF
            INITIALIZE v_existe_nss TO NULL
            # Compruba si existe nss nuevo en afi
            LET v_consulta = "\n SELECT FIRST 1 1", 
                             "\n   FROM afi_derechohabiente",
                             "\n  WHERE nss = ?"
            PREPARE prp_existe_nss FROM v_consulta
            EXECUTE prp_existe_nss USING v_nss_nuevo
                                    INTO v_existe_nss
            IF(v_existe_nss IS NULL)THEN
               CALL fn_mensaje("Aviso","No existe NSS nuevo en catálogos","information")
               CONTINUE INPUT
            END IF
            LET v_capturo = TRUE
            EXIT INPUT

         ON ACTION cancelar
           LET v_capturo = FALSE
           EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_captura_nss_nuevo

   RETURN v_nss_nuevo, v_capturo

END FUNCTION

{===============================================================================
Nombre: fn_captura_nss
Fecha creacion: 29 Mayo del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Captura nss a cambiar y recupera información
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_captura_nss()
DEFINE v_nss_actual LIKE afi_derechohabiente.nss,
       v_folio      LIKE pag_det_apvol.folio,
       v_continuar  BOOLEAN

   LET v_continuar = FALSE 

   INPUT v_nss_actual,
         v_folio 
    FROM nss_actual,
         folio ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

      ON ACTION aceptar
         IF(v_nss_actual CLIPPED IS NULL)THEN
            CALL fn_mensaje("Aviso", "Capture NSS","information")
            NEXT FIELD nss_actual
         END IF
         IF(v_folio CLIPPED IS NULL)THEN
            CALL fn_mensaje("Aviso", "Capture folio","information")
            NEXT FIELD folio
         END IF
         
         CALL fn_recupera_importes(v_nss_actual,v_folio) RETURNING v_continuar
         IF NOT(v_continuar)THEN
            CALL fn_mensaje("Aviso","No se encontraron registros con criterio dado","information")
            CONTINUE INPUT
         ELSE
            EXIT INPUT
         END IF
         

      ON ACTION cancelar
         LET v_continuar = FALSE
         EXIT INPUT 

   END INPUT

   RETURN v_nss_actual,v_folio, v_continuar
END FUNCTION

{===============================================================================
Nombre: fn_recupera_importes
Fecha creacion: 29 Mayo del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Recupera los importes de ap voluntarias para el nss capturado
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_recupera_importes(p_nss_actual,p_folio)
DEFINE p_nss_actual LIKE afi_derechohabiente.nss,
       p_folio      LIKE pag_det_apvol.folio,
       v_consulta   STRING,
       v_registros  RECORD
         v_nss            LIKE afi_derechohabiente.nss,
         v_f_pago         LIKE pag_det_apvol.f_pago,
         v_imp_ap_vol     LIKE pag_det_apvol.imp_ap_vol,
         v_nom_trabajador LIKE pag_det_apvol.nom_trabajador,
         v_folio          LIKE pag_det_apvol.folio
       END RECORD,
       v_indice   SMALLINT,
       v_encontro BOOLEAN

   LET v_indice = 1
   LET v_encontro = FALSE
   CALL v_detalle_nss.clear()
   LET v_consulta = "\n SELECT afi.nss,",
                    "\n        pag.f_pago,",
                    "\n        SUM(pag.imp_ap_vol),",
                    "\n        pag.nom_trabajador,",
                    "\n        pag.folio",
                    "\n   FROM pag_det_apvol pag JOIN afi_derechohabiente afi",
                    "\n     ON afi.id_derechohabiente = pag.id_derechohabiente",
                    "\n  WHERE afi.nss = ?",
                    "\n    AND pag.folio = ?",
                    "\n  GROUP BY 1,2,4,5"
                    
   PREPARE prp_rec_importes_nss FROM v_consulta
   DECLARE cur_rec_importes_nss CURSOR FOR prp_rec_importes_nss
   FOREACH cur_rec_importes_nss USING p_nss_actual,
                                      p_folio
                                 INTO v_registros.*
      LET v_encontro = TRUE
      LET v_detalle_nss[v_indice].v_nss            = v_registros.v_nss
      LET v_detalle_nss[v_indice].v_f_pago         = v_registros.v_f_pago USING "dd-mm-yy"
      LET v_detalle_nss[v_indice].v_imp_ap_vol     = v_registros.v_imp_ap_vol
      LET v_detalle_nss[v_indice].v_nom_trabajador = v_registros.v_nom_trabajador
      LET v_detalle_nss[v_indice].v_folio          = v_registros.v_folio

      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_rec_importes_nss

   RETURN v_encontro

END FUNCTION

{===============================================================================
Nombre: fn_actualiza_detalle_nss
Fecha creacion: 29 Mayo del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Actualiza el detalle del nss actual al nuevo nss capturado
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_actualiza_detalle_nss(p_nss_nuevo, p_nss_actual,p_folio)
   DEFINE p_nss_nuevo  LIKE afi_derechohabiente.nss,
          p_nss_actual LIKE afi_derechohabiente.nss,
          p_folio      LIKE pag_det_apvol.folio,
          v_id_derechohabiente_nuevo  LIKE afi_derechohabiente.id_derechohabiente,
          v_id_derechohabiente_actual LIKE afi_derechohabiente.id_derechohabiente,
          v_curp_nuevo    LIKE afi_derechohabiente.curp,
          v_nombre_nuevo  LIKE afi_derechohabiente.nombre_imss,
          v_ocurrio_error BOOLEAN

   DEFINE r_liq_vol RECORD
      f_liquida      DATE,                   
      id_derecho     decimal(9,0),           
      subcuenta      SMALLINT,               
      fondo          SMALLINT,               
      movimiento     SMALLINT,               
      folio_liquida  decimal(9,0),           
      id_referencia  decimal(9,0),           
      monto_acciones decimal(16,6),          
      monto_pesos    decimal(12,2),          
      f_valor        DATE,                   
      f_registro     DATE,                   
      h_registro     datetime hour to SECOND,
      origen         char(20)               
   END RECORD
   
   DEFINE v_id_referencia   DECIMAL(9,0),
          v_f_pago          DATE,
          v_pesos           DECIMAL(12,2),
          v_acciones        DECIMAL(16,6),
          v_fondo           SMALLINT,
          v_hoy             DATE,
          v_cve_proceso_cnt SMALLINT,
          v_cve_proceso     SMALLINT,
          v_transa          SMALLINT,
          v_error           SMALLINT,
          v_precio          DECIMAL(19,14),
          v_proceso_cod     SMALLINT,
          v_opera_cod       SMALLINT
   
   DEFINE v_query STRING
       
   LET v_ocurrio_error = FALSE
   LET v_fondo = 11
   LET v_hoy = TODAY
   LET v_cve_proceso_cnt = 52
   LET v_transa = 73
   LET v_proceso_cod = 1407
   LET v_opera_cod = 4
   
   # recupera id de nuevo nss
   SELECT id_derechohabiente,
          curp,
          nombre_imss          
   INTO   v_id_derechohabiente_nuevo,
          v_curp_nuevo,
          v_nombre_nuevo
   FROM   afi_derechohabiente
   WHERE  nss = p_nss_nuevo

    
   # recupera id de actual nss
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente_actual
   FROM   afi_derechohabiente
   WHERE  nss = p_nss_actual
  
   -- obtiene datos del nss a cargar --
   SELECT id_referencia,
          monto_acciones 
   INTO   v_id_referencia,
          v_acciones
   FROM   cta_movimiento
   WHERE  folio_liquida = p_folio
   AND    id_derechohabiente = v_id_derechohabiente_actual

            
   -- obtiene datos del proceso para contabilidad
   SELECT proceso_cod
   INTO   v_cve_proceso
   FROM   glo_folio
   WHERE  folio = p_folio
  DISPLAY "proceso ",  v_cve_proceso
{
   --se invoca a función que calcula el monto en acciones
   LET v_query = "EXECUTE FUNCTION fn_consulta_precio_fondo (?,?,?)"
   PREPARE c_fondo FROM v_query
   EXECUTE c_fondo USING v_pesos,v_hoy,v_fondo INTO v_acciones
}
   --revalua pesos
   SELECT precio_fondo
   INTO   v_precio 
   FROM   glo_valor_fondo
   WHERE  fondo = v_fondo
   AND    f_valuacion = v_hoy

   IF v_precio IS NULL THEN 
   	  LET v_pesos = 0
   ELSE
   	  LET v_pesos = (v_acciones * v_precio)
   END IF
   
   LET v_query = " EXECUTE PROCEDURE sp_liquida_cambio_nss_vol(?,?,?,?,?,?,?,?,?,?,?,?,?) "
   PREPARE c_liq_cnss_vol FROM v_query

   -- SE OBTIENE FOLIO NUEVO --
   PREPARE c_folio_nuevo FROM "EXECUTE FUNCTION safre_viv:fn_genera_folio(?, ?, ?)"
   EXECUTE c_folio_nuevo USING v_proceso_cod, v_opera_cod, v_usuario_cod INTO v_folio_nuevo   
   IF sqlca.sqlcode <> 0 THEN

      DISPLAY "Ocurrió un error al generar folio: ",sqlca.sqlcode
      LET v_ocurrio_error = TRUE
      RETURN v_ocurrio_error
   END IF
 
   -- MOVIMIENTO DE CARGO --
   LET r_liq_vol.f_liquida       = TODAY
   LET r_liq_vol.id_derecho      = v_id_derechohabiente_actual
   LET r_liq_vol.subcuenta       = 45
   LET r_liq_vol.fondo           = 11
   LET r_liq_vol.movimiento      = 1522
   LET r_liq_vol.folio_liquida   = v_folio_nuevo
   LET r_liq_vol.id_referencia   = v_id_referencia
   LET r_liq_vol.monto_acciones  = v_acciones * -1
   LET r_liq_vol.monto_pesos     = v_pesos * -1
   LET r_liq_vol.f_valor         = v_f_pago      
   LET r_liq_vol.f_registro      = TODAY
   LET r_liq_vol.h_registro      = CURRENT HOUR TO SECOND
   LET r_liq_vol.origen          = "CARGO X CNSS VOL"
   
   EXECUTE c_liq_cnss_vol 
   USING  r_liq_vol.*

   -- MOVIMIENTO DE ABONO --
   LET r_liq_vol.f_liquida       = TODAY
   LET r_liq_vol.id_derecho      = v_id_derechohabiente_nuevo
   LET r_liq_vol.subcuenta       = 45
   LET r_liq_vol.fondo           = 11
   LET r_liq_vol.movimiento      = 491
   LET r_liq_vol.folio_liquida   = v_folio_nuevo
   LET r_liq_vol.id_referencia   = v_id_referencia
   LET r_liq_vol.monto_acciones  = v_acciones
   LET r_liq_vol.monto_pesos     = v_pesos
   LET r_liq_vol.f_valor         = v_f_pago      
   LET r_liq_vol.f_registro      = TODAY
   LET r_liq_vol.h_registro      = CURRENT HOUR TO SECOND
   LET r_liq_vol.origen          = "ABONO X CNSS VOL"
   
   EXECUTE c_liq_cnss_vol 
   USING  r_liq_vol.*

   -- executa contabilidad
   LET v_query = " EXECUTE PROCEDURE fn_apo_vol_cnt52 (?,?,?,?,?) "
   PREPARE c_conta_vol FROM v_query   
   EXECUTE c_conta_vol USING v_folio_nuevo,
                             v_hoy,
                             v_cve_proceso_cnt,
                             v_cve_proceso,
                             v_transa
                        INTO v_error 

   -- Se comenta por correo de Hamir del 28-ENERO-2015
   -- con asutno Aportaciones voluntarias, en donde 
   -- solicita cargo al nss cedente y abono al nss receptor
{    
   # cambia el detalle del nss actual al nuevo nss
   UPDATE pag_det_apvol
   SET    id_derechohabiente = v_id_derechohabiente_nuevo,
          curp = v_curp_nuevo,
          nom_trabajador = v_nombre_nuevo,
          ind_cambio_nss = 1,
          f_cambio_nss   = TODAY
   WHERE  id_derechohabiente = v_id_derechohabiente_actual
   AND    folio = p_folio

   UPDATE pag_preliquida
   SET    id_derechohabiente = v_id_derechohabiente_nuevo
   WHERE  id_derechohabiente = v_id_derechohabiente_actual
   AND    folio_liquida = p_folio

   UPDATE cta_movimiento
   SET    id_derechohabiente = v_id_derechohabiente_nuevo
   WHERE  id_derechohabiente = v_id_derechohabiente_actual
   AND    folio_liquida = p_folio


   IF(STATUS)THEN
      # Ocurrió error en la actualización
      LET v_ocurrio_error = TRUE
   END IF
}
   IF v_error = 0 THEN
      LET v_ocurrio_error = TRUE
   END IF
   
   RETURN v_ocurrio_error
END FUNCTION

{===============================================================================
Nombre: fn_valida_folio_liquidado
Fecha creacion: 09 Junio 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Valida que el folio ya esté liquidado
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_valida_folio_liquidado(p_folio)
   DEFINE p_folio    LIKE glo_folio.folio,
          v_estado   LIKE glo_folio.status,
          v_continua BOOLEAN,
          v_consulta STRING

   INITIALIZE v_estado TO NULL

   LET v_consulta = "\n SELECT status",
                    "\n   FROM glo_folio",
                    "\n WHERE folio = ?"

   PREPARE prp_rec_estado_folio FROM v_consulta
   EXECUTE prp_rec_estado_folio USING p_folio
                                 INTO v_estado

   IF(v_estado = 2)THEN # 2 - liquidado
      LET v_continua = TRUE
   ELSE
      LET v_continua = FALSE
   END IF 

   RETURN v_continua
END FUNCTION
