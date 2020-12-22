--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 27/04/2012
--===============================================================

#################################################################################
#Modulo       => DPE                                                            #
#Programa     => DPEG01                                                         #
#Objetivo     => Ejecutar funciones, variales y constantes globales del modulo  #
#                de devolucion de pagos indebidos o en exceso                   #
#Fecha inicio => Marzo 06, 2012                                                 #
#################################################################################

DATABASE safre_viv
GLOBALS
   CONSTANT  g_proceso_cod_dpe_disposicion    SMALLINT = 1001 -- devolucion de pagos indebidos o en exceso
   CONSTANT  g_proceso_cod_dpe_procesar       SMALLINT = 1002 -- devolucion de pagos indebidos procesar recibe
   CONSTANT  g_proceso_cod_dpe_credito        SMALLINT = 1003 -- devolucion de pagos indebidos origen créditos
   CONSTANT  g_proceso_cod_dpe_procesar_gen   SMALLINT = 1008 -- devolucion de pagos indebidos procesar genera
   CONSTANT  g_proceso_cod_dpe_infonavit      SMALLINT = 1005 -- devolucion de pagos indebidos origen infonavit

   CONSTANT  g_opera_cod_dpe_carga            SMALLINT = 1,  -- carga de archivo
             g_opera_cod_dpe_integracion      SMALLINT = 2,  -- integracion de
             g_opera_cod_dpe_preliquidacion   SMALLINT = 3,  -- preliquidacion
             g_opera_cod_dpe_liquidacion      SMALLINT = 4,  -- liquidacion
             g_opera_cod_dpe_genera_procesar  SMALLINT = 5   -- genera archivo salida procesar

DEFINE r_detmov DYNAMIC ARRAY OF RECORD
          fecha_mov           DATE,
          tpo_mov             INTEGER,
          desc_mov            CHAR(80),
          fondo               SMALLINT,
          pesos               DECIMAL(16,6),
          acciones            DECIMAL(16,6),
          f_valor             DATE,
          folio               DECIMAL(10,0),
          origen              CHAR(20)
       END RECORD,
       w ui.Window,
       f ui.Form

END GLOBALS

#OBJETIVO: Funcion que muestra información del derechohabiente
FUNCTION fn_preconsulta(p_nss,p_usuario_cod,
                                p_tipo_ejecucion, p_s_titulo)
DEFINE p_nss   CHAR(11),
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_s_titulo CHAR(25),
       lc_qry  STRING,
       cont    INTEGER,
       x_flg   SMALLINT,
       arr_busqueda DYNAMIC ARRAY OF RECORD
          nss             CHAR(11),
          rfc             CHAR(13),
          curp            CHAR(18),
          ap_paterno_af   CHAR(40),
          ap_materno_af   CHAR(40),
          nombres_af      CHAR(40)
       END RECORD,
       v_c_nss CHAR(11)

   LET x_flg = 0
   CALL arr_busqueda.clear()
   
   LET lc_qry = "SELECT a.nss,",
                "       a.rfc,",
                "       a.curp,",
                "       a.ap_paterno_af,",
                "       a.ap_materno_af,",
                "       a.nombre_af",
                "  FROM afi_derechohabiente a ",
                " WHERE nss = ","'",p_nss CLIPPED,"'"

            PREPARE prp_pre_busqueda FROM lc_qry
            DECLARE cur_pre_busqueda CURSOR FOR prp_pre_busqueda
            
            LET cont= 1

            FOREACH cur_pre_busqueda INTO arr_busqueda[cont].*
                LET cont = 1 + cont

                IF cont > 32767 THEN
                    CALL fn_mensaje("Aviso","SE SOBREPASÓ LA CAPACIDAD MÁXIMA DEL ARREGLO","exclamation")
                    LET INT_FLAG = TRUE
                END IF
            END FOREACH
            
            IF cont = 1 THEN
                CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
                LET INT_FLAG = TRUE
            ELSE
               CALL fn_consulta_afi(p_nss,p_usuario_cod,
                                p_tipo_ejecucion, p_s_titulo)	    
            END IF
END FUNCTION

#OBJETIVO: Función para desplegar pantalla de saldos del asignado
FUNCTION fn_eje_consulta_saldo(p_nss)
DEFINE p_nss   CHAR(11),
       v_query STRING,
       v_count SMALLINT

   LET v_query = "  SELECT COUNT(*)",
                 "\n  FROM afi_derechohabiente",
                 "\n WHERE nss = '",p_nss,"'"

      PREPARE prp_consulta_saldo FROM v_query
      EXECUTE prp_consulta_saldo INTO v_count
      
      IF v_count <> 0 THEN
         CALL fn_saldo_id_derechohabiente(p_nss)	
      ELSE	
            CALL fn_mensaje("Consulta Saldo",
                            "No existen datos con el criterio dado",
                            "about")
      END IF

END FUNCTION

#OBJETIVO: Función para desplegar pantalla de saldos del asignado
FUNCTION fn_saldo_id_derechohabiente(p_nss)
DEFINE p_nss  CHAR(11),
       v_r_datos_generales RECORD
          v_nss              CHAR(11),
          v_rfc              CHAR(13),
          v_curp             CHAR(18),
          nombre_completo    CHAR(120),
          id_derechohabiente DECIMAL(9,0)
       END RECORD,
       arr_arbol DYNAMIC ARRAY OF RECORD
          subcuenta_desc     CHAR(60),
          siefore            SMALLINT,
          monto_pesos        DECIMAL(16,6),
          monto_acciones     DECIMAL(16,6),
          subcuenta          SMALLINT,
          movimiento         SMALLINT,
          padre_id           CHAR(40),
          id                 CHAR(40),
          nivel              SMALLINT
       END RECORD,
       arr_precios  DYNAMIC ARRAY OF RECORD
          precio_cod         SMALLINT,
          precio_sie         CHAR(30),
          precio_al_dia      DECIMAL(19,14)
       END RECORD,
       v_query               STRING,
       v_hoy                 DATE,
       i                     SMALLINT,
       vsdo_fin              DECIMAL(22,2),
       vsdo_ini              DECIMAL(22,2),
       vrendimiento          DECIMAL(22,2),
       v_subcuenta           SMALLINT,
       v_siefore             SMALLINT,
       v_movimiento          SMALLINT,
       resp_visualiza        SMALLINT,
       v_pos                 SMALLINT,
       v_exe_edo_cuenta      STRING --Variable para establecer la instruccion que genera el Estado de cuenta

   -- inicializa variables y crea tablas temporales para el procesos
   CALL fn_inicializa()
   
   -- Obtiene los datos personales del id derechohabiente
   SELECT nss,
          rfc,
          curp,
          TRIM(nombre_af)||' '||
          TRIM(ap_paterno_af)||' '||
          TRIM(ap_materno_af),
          id_derechohabiente
     INTO v_r_datos_generales.*
     FROM afi_derechohabiente
    WHERE nss = p_nss
    
    PREPARE prp_proc FROM "EXECUTE PROCEDURE sp_consulta_saldo(?)"
    EXECUTE prp_proc USING v_r_datos_generales.id_derechohabiente
    
    LET v_hoy = TODAY
    LET vsdo_fin = 0
    
    LET v_query = "SELECT gvf.fondo,",
                  "       TRIM(cfl.razon_social),",
                  "       gvf.precio_fondo",
                  "  FROM glo_valor_fondo gvf, ",
                  "       cat_fondo_local cfl ",
                  " WHERE gvf.f_valuacion = ? ",
                  "   AND cfl.fondo       = gvf.fondo "  
   
      PREPARE prp_precio FROM v_query
      DECLARE cur_precio CURSOR FOR prp_precio
      LET i = 1
      FOREACH cur_precio USING v_hoy INTO arr_precios[i].*
         LET i = i + 1
      END FOREACH
      CLOSE cur_precio
      FREE cur_precio
      
      CALL arr_precios.deleteElement(i)
      	
      PREPARE prp_arbol FROM "SELECT * FROM tmp_arbol_saldo ORDER BY id "
      DECLARE cur_arbol CURSOR FOR prp_arbol
      
      LET i = 1
      FOREACH cur_arbol INTO arr_arbol[i].*
         IF arr_arbol[i].monto_pesos IS NULL THEN
            LET arr_arbol[i].monto_pesos = 0
         END IF
         
         IF arr_arbol[i].monto_acciones IS NULL THEN
            LET arr_arbol[i].monto_acciones = 0
         END IF
         
         IF arr_arbol[i].subcuenta = 0 THEN
            LET vsdo_fin   = vsdo_fin + arr_arbol[i].monto_pesos
         END IF
         
         LET i = i + 1
      END FOREACH
      CLOSE cur_arbol
      FREE cur_arbol
      
      CALL arr_arbol.deleteElement(i)
      LET i = i - 1 
      
   OPEN WINDOW saldo_id_derechohabiente WITH FORM "DPEC016" ATTRIBUTE(STYLE="dialog")
      LET w = ui.Window.forName("saldo_id_derechohabiente")
      LET f = w.getForm()
      LET INT_FLAG = TRUE 
      CALL fgl_settitle("CONSULTA DE SALDO")
      CALL ui.Interface.refresh()
      
      DISPLAY BY NAME v_r_datos_generales.v_nss,
                      v_r_datos_generales.v_rfc,
                      v_r_datos_generales.v_curp,
                      v_r_datos_generales.nombre_completo
      
      DIALOG ATTRIBUTES(UNBUFFERED)
         DISPLAY ARRAY arr_arbol TO tree.*
         BEFORE DISPLAY
            CALL DIALOG.setactionhidden("close",1)
         BEFORE ROW
            LET v_pos = ARR_CURR() 
               IF arr_arbol[v_pos].nivel > 1 THEN
                  LET v_subcuenta = arr_arbol[v_pos].subcuenta
                  LET v_siefore   = arr_arbol[v_pos].siefore
                  LET v_movimiento= arr_arbol[v_pos].movimiento 
               
                  CALL despliega_detalle_mov(v_subcuenta,v_siefore, v_movimiento)
                       RETURNING resp_visualiza
               
                  IF NOT resp_visualiza THEN
                     CALL r_detmov.clear()
                  ELSE
                     CALL r_detmov.deleteelement(r_detmov.getlength())
                  END IF
               ELSE
                  CALL r_detmov.clear()
               END IF
               
               IF vsdo_fin IS NULL THEN
                  LET vsdo_fin = 0
               END IF

               LET vrendimiento = vsdo_fin - vsdo_ini

               DISPLAY vsdo_fin TO sdo_fin
               
         #Boton que invoca la generacion del estado de cuenta
         ON ACTION edo_cuenta
            LET v_exe_edo_cuenta = "cd /ds/safreviv/srv/bin/;fglrun SRVP09.42r '", v_r_datos_generales.v_nss,"'"
            RUN v_exe_edo_cuenta
            
         ON ACTION CANCEL
            EXIT DIALOG      
         
         END DISPLAY
         
         DISPLAY ARRAY r_detmov TO detalle.*
            ON ACTION ACCEPT
               EXIT DIALOG
            
            ON ACTION CANCEL
               EXIT DIALOG
         
         END DISPLAY
         
         DISPLAY ARRAY arr_precios TO precios.*
           ON ACTION CLOSE
              EXIT DIALOG
         
         END DISPLAY

      END DIALOG   

   CLOSE WINDOW saldo_id_derechohabiente   

END FUNCTION

-- Despliega el detalle del movimiento
FUNCTION despliega_detalle_mov(p_subcuenta,p_fondo, p_movimiento)
DEFINE i            INTEGER,
       p_subcuenta  SMALLINT,
       p_fondo      SMALLINT,
       p_movimiento SMALLINT,
       v_query      STRING
 
 CALL r_detmov.clear()     

    LET v_query = " SELECT t.f_liquida, ",
                         " t.tipo_movimiento, ",
                         " c.movimiento_desc, ",
                         " t.fondo_inversion, ",
                         " t.monto_pesos, ",
                         " t.monto_acciones, ",
                         " t.f_valor, ",
                         " t.folio_liquida, ",
                         " t.origen ",
                    " FROM tmp_movimientos_saldo t, ",
                         " cat_movimiento c ",
                   " WHERE t.subcuenta       = ", p_subcuenta,
                     " AND t.fondo_inversion = ", p_fondo
                     
   IF p_movimiento > 0 THEN
      LET v_query = v_query , " AND t.tipo_movimiento = ",p_movimiento 
   END IF

   LET v_query = v_query , " AND c.movimiento      = t.tipo_movimiento ",
                         " ORDER BY f_liquida DESC "
                          
    CALL r_detmov.clear()

    PREPARE prp_mov FROM v_query
    DECLARE cur_mov CURSOR FOR prp_mov

    LET i = 1

    FOREACH cur_mov INTO r_detmov[i].*
        LET i = i + 1
    END FOREACH
    CLOSE cur_mov

    IF i = 1 THEN
        RETURN 0
    ELSE
        RETURN 1
    END IF
END FUNCTION
#OBJETIVO: Crear tablas temporales para la carga de archivos 
FUNCTION fn_inicializa()
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_movimientos_saldo
      DROP TABLE tmp_arbol_saldo
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_movimientos_saldo
    (id_derechohabiente DECIMAL(9,0),
     subcuenta          SMALLINT,
     fondo_inversion    SMALLINT,
     f_liquida          DATE, 
     tipo_movimiento    INTEGER,
     monto_pesos        DECIMAL(16,6),
     monto_acciones     DECIMAL(16,6),
     f_valor            DATE,
     folio_liquida      DECIMAL(10,0),
     origen             CHAR(20))

   CREATE TEMP TABLE tmp_arbol_saldo
    (subcuenta_desc     CHAR(70),
     siefore            SMALLINT,
     monto_pesos        DECIMAL(22,2),
     monto_acciones     DECIMAL(22,2),
     subcuenta          SMALLINT,
     movimiento         SMALLINT,
     padre_id           CHAR(40),
     id                 CHAR(40),
     nivel              SMALLINT)

END FUNCTION

#OBJETIVO: Función que despliega la información del asignado elegido
FUNCTION fn_consulta_afi(p_nss,p_usuario_cod,p_tipo_ejecucion, p_s_titulo)
DEFINE reg_derechohabiente RECORD
          nss              CHAR(11),
          rfc              CHAR(13),
          curp             CHAR(18),
          f_nacimiento     DATE,
          ap_paterno_af    CHAR(40),
          ap_materno_af    CHAR(40),
          nombre_af        CHAR(40),
          nombre_imss      CHAR(50),
          desc_tipo_trab   CHAR(20),
          desc_origen      CHAR(20),
          desc_ind_credito CHAR(30),
          num_credito      DECIMAL(10,0),
          desc_tpo_credito CHAR(30)
       END RECORD,
       domicilio_1 DYNAMIC ARRAY OF RECORD
          dom_cod            SMALLINT,
          dom_desc           CHAR(15),
          envio_desc         CHAR(15)
       END RECORD,
       domicilio_2 DYNAMIC ARRAY OF RECORD
          id_domicilio       INTEGER,
          ind_envio          CHAR(1)
       END RECORD,
       correo_1 DYNAMIC ARRAY OF RECORD
          correo_elect       CHAR(50)
       END RECORD,
       tel_1 DYNAMIC ARRAY OF RECORD
          tel_cod            SMALLINT,
          tel_desc           CHAR(15),
          cve_lada           CHAR(3),
          telefono           CHAR(40),
          extension          CHAR(5),
          pais_cod           CHAR(3)
       END RECORD,
       v_id_derechohabiente DECIMAL(9,0),
       p_nss CHAR(11),
       lc_qry  STRING,
       i, ii, iii SMALLINT,
       cur_row    SMALLINT,
       scr_row    SMALLINT,
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_s_titulo CHAR(25),
       v_id_derechohabiente_saldo DECIMAL(9,0)
       
       


   OPEN WINDOW DespliInfo WITH FORM ("DPEC015") ATTRIBUTE(STYLE="dialog")
      LET w = ui.window.getcurrent()
      LET w = ui.window.forName("DespliInfo")
      LET INT_FLAG = TRUE 
      
      CALL fgl_settitle("INFORMACIÓN DE DERECHOHABIENTES")
      
      LET lc_qry = " SELECT afi.id_derechohabiente,",
                           " afi.nss,",
                           " afi.rfc,",
                           " afi.curp,",
                           " afi.f_nacimiento,",
                           " afi.ap_paterno_af,",
                           " afi.ap_materno_af,",
                           " afi.nombre_af,",
                           " afi.nombre_imss,",
                           " DECODE(afi.tipo_trabajador,'S','SOLO INFONAVIT','I','IMSS'),",
                           " DECODE(afi.origen_afiliacion,'R','RECAUDACIÓN','U','UNIFICACIÓN','A','REGISTRADO','S','SEPARACIÓN'),",
                           " DECODE(afi.id_credito,'0','SIN CRÉDITO','CON CRÉDITO'),",
                           " cre.num_credito,",
                           " cat.desc_credito",
                      " FROM afi_derechohabiente afi",
                      " LEFT JOIN cta_credito cre ON afi.id_derechohabiente = cre.id_derechohabiente",
                      " LEFT JOIN cat_tipo_credito cat on cre.tpo_credito = cat.tpo_credito",
                     " WHERE afi.nss = '", p_nss CLIPPED,"'"
    
    PREPARE qry_asig FROM lc_qry
    EXECUTE qry_asig INTO v_id_derechohabiente, reg_derechohabiente.*
    
    DISPLAY BY NAME reg_derechohabiente.*
    
    DECLARE cur_dom CURSOR FOR
    SELECT dom1.tpo_domicilio,
           dom1.id_domicilio,
           DECODE(tpo_domicilio,1,"PARTICULAR","OTRO"),
           dom1.ind_envio
    FROM   afi_domicilio dom1
    WHERE  dom1.id_derechohabiente = v_id_derechohabiente
    ORDER BY dom1.ind_envio DESC
    
    LET i = 1
    
    FOREACH cur_dom INTO domicilio_1[i].dom_cod,
                         domicilio_2[i].id_domicilio,
                         domicilio_1[i].dom_desc,
                         domicilio_2[i].ind_envio
    
       IF domicilio_2[i].ind_envio = "1" THEN
          LET domicilio_1[i].envio_desc = "CORRESPONDENCIA"
       ELSE
          LET domicilio_1[i].envio_desc = ""
       END IF
    
       LET i = i + 1
    END FOREACH
    
    DECLARE cur_correo CURSOR FOR
      SELECT corr1.valor
        FROM afi_contacto_electronico corr1
       WHERE corr1.id_derechohabiente = v_id_derechohabiente
    ORDER BY corr1.valor
    
    LET iii = 1
    FOREACH cur_correo INTO correo_1[iii].*
       LET iii = iii + 1
    END FOREACH
    
    IF (i) >= 1 THEN
    
       CALL SET_COUNT(i-1)
    
       CALL domicilio_1.deleteelement(domicilio_1.getlength())
       CALL tel_1.deleteelement(tel_1.getlength())
       CALL correo_1.deleteelement(correo_1.getlength())
    
       DIALOG ATTRIBUTES(UNBUFFERED,FIELD ORDER FORM)
          DISPLAY ARRAY domicilio_1 TO tb5.* 
    
          BEFORE DISPLAY
             CALL DIALOG.setactionhidden("close",1)
                MENU "Consulta"
                   BEFORE MENU
                      CALL ui.Interface.refresh()
                   ON ACTION saldo
                      --CALL fn_saldo_id_derechohabiente(p_nss)
                      SELECT id_derechohabiente
                        INTO v_id_derechohabiente_saldo
                        FROM afi_derechohabiente
                        WHERE nss = p_nss
            
                            CALL fn_eje_consulta(1,p_usuario_cod,v_id_derechohabiente_saldo, 
                                                 p_tipo_ejecucion, p_s_titulo)

                   ON ACTION regresar
                      CALL ui.Interface.refresh()
                      EXIT MENU
                END MENU
          BEFORE ROW
             LET cur_row = ARR_CURR()
             LET scr_row = SCR_LINE()
    
             CALL despliega_domicilio(v_id_derechohabiente,
                                      domicilio_2[cur_row].id_domicilio)
    
             ON ACTION cancel
                EXIT DIALOG
          END DISPLAY
    
          DISPLAY ARRAY correo_1 TO tb7.*
             BEFORE DISPLAY
             BEFORE ROW

             LET cur_row = ARR_CURR()
             LET scr_row = SCR_LINE()

             ON ACTION CANCEL
                EXIT DIALOG
          END DISPLAY    
       END DIALOG
    ELSE
       ERROR "DERECHOHABIENTE NO TIENE DOMICILIO REGISTRADO"
    END IF
    
    --CLOSE WINDOW w1
    
    CLOSE WINDOW DespliInfo

END FUNCTION

#función para desplegar los domicilios del derechohabiente
FUNCTION despliega_domicilio(p_id_derechohabiente, p_id_domicilio)
DEFINE p_id_derechohabiente DECIMAL(9,0)
DEFINE p_id_domicilio       SMALLINT
DEFINE domicilio RECORD
          calle              CHAR(60),
          num_ext            CHAR(25),
          num_int            CHAR(25),
          colonia_desc       CHAR(50),
          cp                 CHAR(5),
          delegacion_desc    CHAR(50),
          ciudad_desc        CHAR(50),
          estado_desc        CHAR(50)
       END RECORD

   SELECT dom.calle,
          TRIM(dom.num_exterior),
          TRIM(dom.num_interior),
          colonia.colonia_desc,
          dom.cp,
          munic.municipio_desc,
          ciudad.ciudad_desc,
          estado.entidad_desc_larga
     INTO domicilio.calle,
          domicilio.num_ext,
          domicilio.num_int,
          domicilio.colonia_desc,
          domicilio.cp,
          domicilio.delegacion_desc,
          domicilio.ciudad_desc,
          domicilio.estado_desc
     FROM afi_domicilio dom
    INNER JOIN cat_colonia colonia ON colonia.colonia = dom.colonia
    INNER JOIN cat_cp codigo ON codigo.cp = dom.cp
    INNER JOIN cat_municipio munic ON munic.municipio = codigo.municipio
    INNER JOIN cat_ciudad ciudad ON ciudad.ciudad = codigo.ciudad
    INNER JOIN cat_entidad_federativa estado ON estado.entidad_federativa = codigo.entidad_federativa
    WHERE dom.id_derechohabiente = p_id_derechohabiente
      AND dom.id_domicilio = p_id_domicilio

   DISPLAY domicilio.calle,
           domicilio.num_ext,
           domicilio.num_int,
           domicilio.colonia_desc,
           domicilio.cp,
           domicilio.delegacion_desc,
           domicilio.ciudad_desc,
           domicilio.estado_desc
        TO
           calle,
           num_ext,
           num_int,
           colonia_desc,
           codpos,
           delegacion_desc,
           ciudad_desc,
           estado_desc

END FUNCTION

#OBJETIVO: Función para desplegar pantalla de saldos del asignado              
FUNCTION fn_eje_consulta(p_pgm,p_usuario_cod,p_id_derechohabiente, p_tipo_ejecucion, p_s_titulo)
DEFINE p_pgm                SMALLINT,
       p_usuario_cod        LIKE seg_usuario.usuario_cod,
       p_id_derechohabiente DECIMAL(9,0),
       p_tipo_ejecucion SMALLINT,
       p_s_titulo CHAR(25),
       comma STRING

    DEFINE v_pgm           CHAR(6)
    DEFINE l_ruta_bin      CHAR(40)

    INITIALIZE comma TO NULL

    SELECT ct.ruta_bin
      INTO l_ruta_bin
      FROM seg_modulo ct
     WHERE modulo_cod = 'cta'

    IF p_pgm = 1 THEN
        LET v_pgm = 'CTAC01'
    END IF

    LET comma = "cd ",l_ruta_bin CLIPPED,"/; fglrun ", v_pgm," '",p_usuario_cod,
                "' '",p_tipo_ejecucion, "' '",p_s_titulo, "' '",p_id_derechohabiente,"'"

    CALL ui.interface.refresh()

    LET comma = comma CLIPPED
    RUN comma

END FUNCTION