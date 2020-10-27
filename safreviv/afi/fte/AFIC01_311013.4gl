######################################################################
#Proyecto          => INFONAVIT (MEXICO)                             #
#Propietario       => E.F.P.                                         #
#Programa AFIC01   => PROGRAMA DE CONSULTA DE DERECHOHABIENTES       #
#Sistema           => SAFRE-VIVIENDA                                 #
#Autor             => MAURO MUÑIZ CABALLERO                          #
#Fecha             => 27 DE MARZO DE 2012                            #
######################################################################

DATABASE safre_viv

GLOBALS "AFIG01.4gl"

   DEFINE p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo       STRING -- titulo de la ventana
   DEFINE g_nss            CHAR(11) -- nss   

MAIN

   -- se asignan los parametros que vienen del fglrun
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_nss            = ARG_VAL(4) -- si se recibe, se envia directo a la consulta de datos

   -- se asigna el titulo de la ventana
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se inicia el LOG
   CALL STARTLOG(g_usuario CLIPPED||'.AFIC01.log')
   
   -- se inician las variables globales
   LET g_hoy    = TODAY
   LET g_titulo = "Información"
   LET g_imagen = "information"

   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados, USER
   INTO   g_reg_modulo.*, g_usuario
   FROM   seg_modulo s
   WHERE  modulo_cod = 'afi'
   
   CALL fn_proceso_principal()

END MAIN

{
======================================================================
Nombre: fn_proceso_principal
Fecha creacion: Marzo 27, 2012
Autor: Mauro Muniz, EFP
Narrativa del proceso que realiza:
Abre el proceso principal de consulta de datos del derechohabiente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega      Junio 22, 2012      - Se agrega NSS como parametro. Si se recibe y es valid
                                     se abre directamente la ventana con resultados.
                                   - Si no se recibe se abre la ventana de captura de parametros
                                     de busqueda
======================================================================
}
-- Funcion principal del programa
FUNCTION fn_proceso_principal()
DEFINE v_flg                 SMALLINT,
       v_mensaje             STRING  ,
       v_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente

   -- se abre la ventana de consulta
   OPEN WINDOW w2 WITH FORM ("../../afi/bin/AFIC012")
   
   IF ( g_nss IS NOT NULL ) THEN
      -- se valida si el NSS existe en la lista de derechohabientes
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = g_nss

      -- si no existe el derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se indica al usuario en pantalla que no fue encontrado
         LET v_mensaje = "El derechohabiente con NSS [" || g_nss || "] no existe"
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      ELSE
         -- si se recibio el NSS desde parametro, se envia directamente a la consulta
         CALL fn_consulta_afi(g_nss)
      END IF
   ELSE
      -- si no se recibio el NSS desde parametro, se habilita la captura de datos de busqueda
      LET w_criterio = ui.window.getcurrent()
      LET w_criterio = ui.window.forName("w2")
 
      CALL w_criterio.settext("INFORMACIÓN DE DERECHOHABIENTES")

      LET f_criterio = w_criterio.getform()

      -- se abre el ciclo
      LET v_flg = TRUE
      WHILE ( v_flg )
         CALL fn_preconsulta() RETURNING v_flg
         -- si no se cancelo la consulta
         IF ( v_flg ) THEN
           CALL fn_consulta_afi(NULL)
         END IF
         
         -- se reinicia el registro de derechohabiente
         INITIALIZE reg_derechohabiente.* TO NULL
      END WHILE
   
   END IF
   
   CLOSE WINDOW w2
END FUNCTION

-- Funcion para elegir la información del derechohabiente a desplegar
FUNCTION fn_preconsulta()
DEFINE cont              INTEGER     , -- contador
       x_flg             SMALLINT    ,
       lc_condicion      STRING      , -- query de condiciones para la consulta
       lc_qry            STRING      , -- query de consulta
       v_nss             VARCHAR(11) , -- nss
       v_rfc             VARCHAR(13) , -- rfc
       v_curp            VARCHAR(18) , -- curp
       v_folio           DECIMAL(9,0), -- folio del archivo
       arr_busqueda      DYNAMIC ARRAY OF RECORD
           nss             CHAR(11)    ,
           rfc             CHAR(13)    ,
           curp            CHAR(18)    ,
           ap_paterno_af   CHAR(40)    ,
           ap_materno_af   CHAR(40)    ,
           nombres_af      CHAR(40)    ,
           folio           DECIMAL(9,0)
       END RECORD

    LET x_flg = 0

    LET v_ind_nss  = FALSE
    LET v_ind_rfc  = FALSE
    LET v_ind_curp = FALSE

    -- se inician las variables
    LET v_nss   = NULL
    LET v_rfc   = NULL
    LET v_curp  = NULL
    LET v_folio = NULL 

    -- se abre el dialog para obtener parametros de consulta    
    DIALOG ATTRIBUTES(UNBUFFERED)

       CONSTRUCT lc_condicion ON a.nss, a.rfc, a.curp, a.folio_lote
                            FROM nss, rfc, curp, folio
       
          BEFORE CONSTRUCT
             CALL arr_busqueda.clear()
       
             CALL dialog.setActionHidden("close",1)
             CALL f_criterio.setelementhidden("gb_identifica",1)
             CALL f_criterio.setelementhidden("gb_credito",1)
             CALL f_criterio.setelementhidden("gb_contacto",1)
       
          AFTER FIELD nss
             LET v_ind_nss = FGL_BUFFERTOUCHED()

          AFTER FIELD rfc
             LET v_ind_rfc = FGL_BUFFERTOUCHED()

          AFTER FIELD curp
             LET v_ind_curp = FGL_BUFFERTOUCHED()

          AFTER FIELD folio
             LET v_ind_folio = FGL_BUFFERTOUCHED()
       
          ON ACTION ACCEPT
              LET lc_qry = " SELECT a.nss  ,",
                                  " a.rfc  ,",
                                  " a.curp ,",
                                  " a.ap_paterno_af ,",
                                  " a.ap_materno_af ,",
                                  " a.nombre_af, ",
                                  " a.folio_lote ",
                           " FROM afi_derechohabiente a ",
                           " WHERE ",lc_condicion CLIPPED
                           
              DISPLAY lc_qry
                           
              PREPARE prp_pre_busqueda FROM lc_qry
              DECLARE cur_pre_busqueda CURSOR FOR prp_pre_busqueda
       
              LET cont= 1
       
              FOREACH cur_pre_busqueda INTO arr_busqueda[cont].*
                  LET cont = 1 + cont
       
                  IF ( cont > 32767 ) THEN
                      CALL fn_mensaje("Aviso","SE SOBREPASÓ LA CAPACIDAD MÁXIMA DEL ARREGLO","exclamation")
                      LET INT_FLAG = TRUE
                      EXIT DIALOG
                  END IF
              END FOREACH
       
              IF ( cont = 1 ) THEN
                  CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
                  LET INT_FLAG = TRUE
                  --EXIT DIALOG
              END IF
       
              LET INT_FLAG = FALSE
              EXIT DIALOG
       
          ON ACTION cancel
              LET INT_FLAG = TRUE
              EXIT DIALOG
       
          AFTER CONSTRUCT
             IF ((NOT v_ind_nss  ) AND
                 (NOT v_ind_rfc  ) AND
                 (NOT v_ind_curp ) AND
                 (NOT v_ind_folio)) THEN
                CALL fn_mensaje("Aviso","Debe indicar un criterio de búsqueda obligatoriamente","information")
                NEXT FIELD nss
             ELSE
                LET int_flag = FALSE
                EXIT DIALOG
             END IF
          
          ON KEY (INTERRUPT)
             CALL fn_mensaje("Aviso","Búsqueda cancelada","information")
             RETURN
       
       END CONSTRUCT

    END DIALOG

    CALL f_criterio.setelementhidden("gb_identifica",0)
    CALL f_criterio.setelementhidden("gb_credito",0)
    CALL f_criterio.setelementhidden("gb_contacto",0)

    -- se borra el ultimo renglon del arreglo porque el FOREACH lo deja en blanco
    CALL arr_busqueda.deleteElement(cont)

    IF ( (cont-1) >= 1 ) THEN
       DISPLAY ARRAY arr_busqueda TO tb2.*

          ON ACTION accept
             LET reg_derechohabiente.nss = arr_busqueda[ARR_CURR()].nss
             LET INT_FLAG = FALSE
             EXIT DISPLAY
          
          ON ACTION cancel
             LET INT_FLAG = TRUE
             EXIT DISPLAY

       END DISPLAY

       IF NOT INT_FLAG THEN
          LET x_flg = 1
          LET INT_FLAG = FALSE
       ELSE
          LET x_flg = 0
       END IF
    END IF

    CLEAR FORM

    RETURN x_flg

END FUNCTION

#función que despliega la información del asignado elegido
FUNCTION fn_consulta_afi(p_nss)
DEFINE p_nss             LIKE afi_derechohabiente.nss,
       v_ruta_formulario STRING, -- ruta del formulario
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       vr_afi_telefono   RECORD LIKE afi_telefono.* -- registro de telefono

   -- se obtiene la ruta bin del modulo
   SELECT ruta_bin
   INTO v_ruta_ejecutable
   FROM seg_modulo
   WHERE modulo_cod = "afi"
   
   -- se construye el nombre del formulario
   LET v_ruta_formulario = v_ruta_ejecutable CLIPPED || "/AFIC01"
   DISPLAY "v_ruta_formulario: ", v_ruta_formulario

   -- si se recibe como parametro el nss se pone en el registro global de consulta
   -- y que la consulta muestre directamente los datos
   IF ( p_nss IS NOT NULL ) THEN
      LET reg_derechohabiente.nss = p_nss
   END IF

   --OPEN WINDOW DespliInfo WITH FORM ("AFIC01")
   OPEN WINDOW DespliInfo WITH FORM v_ruta_formulario

   LET w_criterio = ui.window.getcurrent()
   LET w_criterio = ui.window.forName("DespliInfo")
   LET INT_FLAG = TRUE
   CALL fgl_settitle("INFORMACIÓN DE DERECHOHABIENTES")

   CALL fn_inicializa()
   
   PREPARE qry_asig FROM g_consulta

   EXECUTE qry_asig INTO g_id_derechohabiente, reg_derechohabiente.*

   DISPLAY BY NAME reg_derechohabiente.*

   CALL f_relacion_derechohabiente(g_id_derechohabiente)
   CALL f_credito_vivienda(g_id_derechohabiente)

   DECLARE cur_dom CURSOR FOR
   SELECT dom1.tpo_domicilio,
          dom1.id_domicilio,
          DECODE(tpo_domicilio,1,"PARTICULAR","OTRO"),
          dom1.ind_envio
   FROM   afi_domicilio dom1
   WHERE  dom1.id_derechohabiente = g_id_derechohabiente
   ORDER BY dom1.ind_envio DESC

   LET i = 1

   FOREACH cur_dom INTO domicilio_1[i].dom_cod,
                        domicilio_2[i].id_domicilio,
                        domicilio_1[i].dom_desc,
                        domicilio_2[i].ind_envio

      IF domicilio_2[i].ind_envio = "X" THEN
         LET domicilio_1[i].envio_desc = "CORRESPONDENCIA"
      ELSE
         LET domicilio_1[i].envio_desc = ""
      END IF

      --DISPLAY "g_id_derechohabiente: ", g_id_derechohabiente
      --DISPLAY "dom_desc: ", domicilio_1[i].dom_desc
      
      LET i = i + 1
   END FOREACH

   DECLARE cur_correo CURSOR FOR
     SELECT corr1.valor
       FROM afi_contacto_electronico corr1
      WHERE corr1.id_derechohabiente = g_id_derechohabiente
   ORDER BY corr1.valor

   LET iii = 1
   FOREACH cur_correo INTO correo_1[iii].*
      LET iii = iii + 1
   END FOREACH

   -- se leen los numeros telefononicos del derechohabiente
   DECLARE cur_afitel CURSOR FOR
   SELECT id_derechohabiente   ,
          id_telefono          ,
          cve_lada             ,
          extension            ,
          telefono             ,
          tpo_telefono         ,
          folio_lote           
     FROM afi_telefono afitel
    WHERE afitel.id_derechohabiente = g_id_derechohabiente
   ORDER BY id_telefono 

   -- se inicia el contador
   LET i = 1

   -- se borra el arreglo de telefonos
   CALL arr_telefono.clear()
   
   -- se transfieren los datos al arreglo
   FOREACH cur_afitel INTO vr_afi_telefono.*
      LET arr_telefono[i].tel_cod = vr_afi_telefono.tpo_telefono

      -- se indica el tipo de telefono
      CASE vr_afi_telefono.tpo_telefono
         WHEN 1
            LET arr_telefono[i].tel_desc = "PARTICULAR"
         WHEN 2
            LET arr_telefono[i].tel_desc = "OFICINA"
         WHEN 3
            LET arr_telefono[i].tel_desc = "CELULAR"
         OTHERWISE
            LET arr_telefono[i].tel_desc = "OTRO"
      END CASE

      LET arr_telefono[i].telefono = vr_afi_telefono.telefono
      LET arr_telefono[i].pais_cod = 52
      LET arr_telefono[i].cve_lada = vr_afi_telefono.cve_lada
      LET arr_telefono[i].extension = vr_afi_telefono.extension

      -- se incrementa el indice
      LET i = i + 1
   END FOREACH

   IF (i) >= 1 THEN
   
      -- se borra el ultimo registro que se genero por causa del foreach
      CALL domicilio_1.deleteelement(domicilio_1.getlength())
      CALL correo_1.deleteelement(correo_1.getlength())

      DIALOG ATTRIBUTES(UNBUFFERED,FIELD ORDER FORM)
         -- despligue de créditos de vivienda
         DISPLAY ARRAY credito_1 TO tb_cred_viv.*

            BEFORE DISPLAY
               CALL DIALOG.setactionhidden("close",1)
   
         END DISPLAY

         -- despliegue de domicilios
         DISPLAY ARRAY domicilio_1 TO tb5.* 
            --BEFORE DISPLAY
               --CALL DIALOG.setactionhidden("close",1)
            
            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()
            
               CALL despliega_domicilio(g_id_derechohabiente,
                                        domicilio_2[cur_row].id_domicilio)
            
            ON ACTION cancel
               EXIT DIALOG
         END DISPLAY
         
         -- despligue de relación laboral
         DISPLAY ARRAY arr_relacion_derech TO t_relacion_laboral.*
         END DISPLAY 
         
         -- despliegue de correo electrónico
         DISPLAY ARRAY correo_1 TO tb7.*
            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()
            ON ACTION cancel
               EXIT DIALOG
         END DISPLAY
         
         -- despligue de números telefónicos
         DISPLAY ARRAY arr_telefono TO tb_telefono.*
         END DISPLAY

         -- solicitud de consulta de saldo
         ON ACTION saldo
                CALL fn_eje_consulta(1)

         ON ACTION CANCEL
            EXIT DIALOG
      END DIALOG
   ELSE
      CALL fn_mensaje("Atención","DERECHOHABIENTE NO TIENE DOMICILIO REGISTRADO","stop")
   END IF

   CLOSE WINDOW DespliInfo

END FUNCTION


-- Funcion para desplegar los domicilios del derechohabiente
FUNCTION despliega_domicilio(p_id_derechohabiente, p_id_domicilio)

   DEFINE p_id_derechohabiente DECIMAL(9,0)
   DEFINE p_id_domicilio       SMALLINT

   SELECT dom.calle,
          TRIM(dom.num_exterior),
          TRIM(dom.num_interior),
          dom.colonia ,
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

-- Función para desplegar pantalla de saldos del asignado
FUNCTION fn_eje_consulta(p_pgm)

    DEFINE p_pgm           SMALLINT

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

    LET comma = "cd ",l_ruta_bin CLIPPED,"/; fglrun ", v_pgm," '",g_usuario,
                "' '",p_tipo_ejecucion, "' '",p_s_titulo, "' '",g_id_derechohabiente,"'"

    CALL ui.interface.refresh()

    LET comma = comma CLIPPED
    RUN comma

END FUNCTION

-- Se realiza consulta para visualizar las relaciones laborales del derechohabiente
FUNCTION f_relacion_derechohabiente(p_id_derechohabiente)

   DEFINE lc_qry               STRING
   DEFINE p_id_derechohabiente DECIMAL(9,0)
   DEFINE v_cont               SMALLINT

   CALL arr_relacion_derech.CLEAR()

   {
   SELECT id_derechohabiente 
     INTO p_id_derechohabiente
     FROM afi_derechohabiente 
    WHERE nss = reg_derechohabiente.nss
   }

   LET lc_qry = " SELECT id_derechohabiente, ",
                       " nrp, ",            
                       " f_alta_nrp,",        
                       " ind_relacion, ",     
                       " folio_lote, ",     
                       " f_actualiza, ",          
                       " usuario   ",          
                       " FROM afi_relacion_laboral WHERE id_derechohabiente = ", p_id_derechohabiente

   DISPLAY lc_qry

   PREPARE prp_relacion_lab FROM lc_qry
   DECLARE cur_relacion_lab CURSOR FOR prp_relacion_lab

   LET v_cont= 1

   FOREACH cur_relacion_lab INTO arr_relacion_derech[v_cont].*
       DISPLAY "array", arr_relacion_derech[v_cont].*
       LET v_cont = v_cont + 1
   END FOREACH
END FUNCTION

FUNCTION f_credito_vivienda(p_id_derechohabiente)

   DEFINE lv_qry                STRING
   DEFINE lv_qry_tc             STRING
   DEFINE p_id_derechohabiente  DECIMAL(9,0)
   DEFINE v_cv                  SMALLINT

   DEFINE v_originacion         CHAR(40)
   DEFINE v_credito             CHAR(40)
   DEFINE v_no_credito          DECIMAL(10,0)
   DEFINE v_edo_cred            CHAR(20)
   DEFINE v_edo_infonavit       CHAR(30)
   DEFINE v_estado              SMALLINT
   DEFINE v_d_procesar          CHAR(30)
   DEFINE v_f_otorgamiento      DATE
   DEFINE v_f_culmina           DATE
   DEFINE v_tpo_credito         SMALLINT

   LET v_cv = 1

   CALL credito_1.CLEAR()

   DECLARE cur_cred CURSOR FOR
   SELECT o.originacion_desc,
          --t.desc_credito,
          c.num_credito,
          DECODE(c.estado,10,'VIGENTE',20,'VIGENTE',140,'VIGENTE',145,'VIGENTE',150,'VIGENTE',170,'LIQUIDADO',220,'VIGENTE',230,'LIQUIDADO',280,'LIQUIDADO',900,'VIGENTE',910,'LIQUIDADO',920,'SIN ORIGINACIÓN'),
          v.estado_desc,
          c.estado,
          p.estado_desc,
          c.f_otorga,
          c.tpo_credito
     FROM cre_acreditado c,
    OUTER cat_cre_originacion o,
    --OUTER cat_tipo_credito t,
    OUTER cat_maq_credito v,
    OUTER cat_maq_credito p
    WHERE c.id_derechohabiente = p_id_derechohabiente
      AND c.estado = v.estado
      AND c.edo_procesar = p.estado
      --AND c.tpo_credito = t.tpo_credito
      AND c.tpo_originacion = o.tpo_originacion
      --AND t.f_actualiza <= c.f_otorga
   ORDER BY c.f_otorga DESC, estado

   FOREACH cur_cred INTO v_originacion,    
                         --v_credito    ,   
                         v_no_credito ,    
                         v_edo_cred   ,
                         v_edo_infonavit,  
                         v_estado     ,    
                         v_d_procesar ,    
                         v_f_otorgamiento,
                         v_tpo_credito

      LET lv_qry_tc = "SELECT FIRST 1 t.desc_credito ",
                       " FROM cat_tipo_credito t ",
                      " WHERE t.tpo_credito = ", v_tpo_credito,
                        " AND t.f_actualiza <= ",v_f_otorgamiento

      PREPARE qry_tc FROM lv_qry_tc
      EXECUTE qry_tc INTO v_credito

      LET lv_qry = "SELECT FIRST 1 h.f_actualiza ",
                    " FROM cta_his_credito h",
                   " WHERE h.id_derechohabiente = ", p_id_derechohabiente,
                     " AND h.num_credito        = ", v_no_credito,
                     " AND h.tpo_credito        = ", v_tpo_credito,
                     " AND h.f_credito          = '", v_f_otorgamiento,"'",
                     " ORDER BY h.f_actualiza DESC "

      PREPARE qry_liq FROM lv_qry
      EXECUTE qry_liq INTO v_f_culmina

      IF v_f_culmina IS NULL OR
         v_f_culmina = "12/31/1899" THEN
         LET v_f_culmina = "        "
      END IF

      LET credito_1[v_cv].desc_originacion = v_originacion    
      LET credito_1[v_cv].desc_credito     = v_credito
      LET credito_1[v_cv].v_num_credito    = v_no_credito
      LET credito_1[v_cv].v_edo_cred       = v_edo_cred
      LET credito_1[v_cv].v_infonavit      = v_edo_infonavit
      LET credito_1[v_cv].estado           = v_estado
      LET credito_1[v_cv].v_procesar       = v_d_procesar
      LET credito_1[v_cv].v_f_otorga       = v_f_otorgamiento 
      LET credito_1[v_cv].v_f_liquida      = v_f_culmina

      -- se incrementa el indice
      LET v_cv = v_cv + 1
   END FOREACH

   CALL credito_1.deleteelement(correo_1.getlength())

END FUNCTION