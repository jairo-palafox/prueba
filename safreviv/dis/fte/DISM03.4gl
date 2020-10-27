################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 24/08/2016                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISM03                                                    #
#Objetivo         => Programa de consulta de Estado de la Factura              #
#                                                                              #
#Fecha de Inicio  => 05/08/2016                                                #
################################################################################
-- Base que se utilizará
DATABASE safre_viv

GLOBALS 
--parametros enviados desde el menu
  DEFINE 
    g_sql_txt                      STRING,      --Consultas
    g_usuario                      VARCHAR(30), --Almacena al usuario
    g_tipo_proceso                 SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog                     VARCHAR(30), --Almacena opción del menú
    p_pid                          DECIMAL(9,0)

DEFINE g_tipo_proceso SMALLINT
DEFINE g_nom_ventana  STRING
DEFINE g_proceso_cod  INTEGER
DEFINE g_opera_cod    INTEGER

--recor para el llenado del combobox
DEFINE rec_tpo_credito RECORD 
    id_tpo_credito_ocg SMALLINT,
    tpo_credito_ocg    SMALLINT,
    desc_credito_ocg   CHAR(25)
END RECORD

--arreglo para el Detalle
DEFINE array_detalle DYNAMIC ARRAY OF RECORD
    a_phantom_tpo_credito  SMALLINT, 
    a_tpo_credito          CHAR(30), --concatenado
    a_phantom_cve_entidad  SMALLINT,
    a_entidad_financiera   CHAR(70), --concatenado
    a_banco_interlocutor   CHAR(4),  --LIKE cat_cta_cnt_ocg.banco_interlocutor,
    a_clabe                CHAR(18),
    a_rfc_ent_financiera   CHAR(12),
    a_cuenta_contable      CHAR(10),
    a_phantom_cve_bloqueo  SMALLINT,
    a_cve_cloqueo          CHAR(20)  --concatenado
END RECORD  
DEFINE v_prueba SMALLINT 

END GLOBALS 

MAIN
DEFINE v_ruta_bitacora  CHAR(40)
DEFINE v_archivo_log    STRING
DEFINE v_programa       STRING
DEFINE v_front          STRING

---se incorpora como parametros enviados desde el menu el proceso y codigo de la operación
   --LET g_usuario      = ARG_VAL(1)
   --LET g_tipo_proceso = ARG_VAL(2)
   --LET g_nom_ventana  = ARG_VAL(3)
   --LET g_proceso_cod  = ARG_VAL(4)
   --LET g_opera_cod    = ARG_VAL(5)

   {LET v_programa = "DISM03"}

-- Se genera el archivo log en la ruta bitacora de la tabla seg_modulo_sfr
   {CALL STARTLOG("DISM03.log")

   -- se indica que la aplicación depende del container "MenuSAFRE"
   CALL ui.Interface.setName(v_programa)
   CALL ui.Interface.setType("child")
   CALL ui.Interface.setContainer("MenuSAFRE")}

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
   
  CALL STARTLOG (g_usuario CLIPPED||".DISM03.log")

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW win1 WITH FORM "DISM031"
     INPUT BY NAME rec_tpo_credito.tpo_credito_ocg ATTRIBUTE(UNBUFFERED, WITHOUT DEFAULTS)
        BEFORE INPUT 
        CALL carga_cbx_tpo_credito()    
         
        ON ACTION ACCEPT
            CALL ventana_detalle_ent_financiera()
            
        ON ACTION CANCEL 
            EXIT INPUT 
     END INPUT 
    
   CLOSE WINDOW WIN1
END MAIN 

FUNCTION carga_cbx_tpo_credito()
DEFINE cbx_tpo_credito ui.ComboBox
DEFINE v_desc_tpo_credito CHAR(30)
    
    --llenado del cbx_tpo_credito
    LET cbx_tpo_credito = ui.ComboBox.forName("tpo_credito_ocg")
    
    DECLARE cur_tpo_credito CURSOR FOR SELECT id_tpo_credito_ocg,tpo_credito_ocg,desc_credito_ocg FROM cat_tpo_credito_ocg
                                        WHERE ind_activo = 1
                                        ORDER BY 1;
   
    CALL cbx_tpo_credito.clear()
    FOREACH cur_tpo_credito INTO rec_tpo_credito.*   
        LET v_desc_tpo_credito = rec_tpo_credito.tpo_credito_ocg || " - ", rec_tpo_credito.desc_credito_ocg
        CALL cbx_tpo_credito.addItem(rec_tpo_credito.tpo_credito_ocg,v_desc_tpo_credito)
    END FOREACH 
       
END FUNCTION 

FUNCTION ventana_detalle_ent_financiera()
    OPEN WINDOW win2 WITH FORM "DISM032"       
        DISPLAY ARRAY array_detalle TO detalle.* ATTRIBUTE(UNBUFFERED,ACCEPT = FALSE )
            BEFORE DISPLAY 
            CALL carga_detalle_ent_financiera(rec_tpo_credito.tpo_credito_ocg)
                
            ON ACTION insertar
                CALL captura_datos()
              
            ON ACTION modificar
                -- Le enviamos los datos que se obtienen por la posición del arreglo
                CALL modificar_det_ent_financiera(array_detalle[ARR_CURR()].*)
                
            ON ACTION bloquear
                CALL bloquear_ent_financiera(array_detalle[ARR_CURR()].*)

            ON ACTION desbloquear
                CALL Desbloquear_ent_financiera(array_detalle[ARR_CURR()].*)

            ON ACTION archivo
                CALL Archivo_ent_financiera(rec_tpo_credito.tpo_credito_ocg)
                
            ON ACTION CANCEL 
                EXIT DISPLAY 
        
        END DISPLAY 
    CLOSE WINDOW win2

END FUNCTION 

FUNCTION carga_detalle_ent_financiera(p_tpo_credito)
DEFINE p_tpo_credito      SMALLINT
DEFINE rec_detalle RECORD
    r_tpo_credito         SMALLINT,
    r_desc_credto_ocg     CHAR(25),
    r_cve_ent_financiera  SMALLINT,
    r_ent_financiera_desc CHAR(60),
    r_banco_interlocutor  CHAR(4),--LIKE cat_cta_cnt_ocg.banco_interlocutor,
    r_clabe               CHAR(18),
    r_rfc_ent_financiera  CHAR(12),
    r_cuenta_contable     CHAR(10),
    r_cve_cloqueo         SMALLINT
END RECORD 

DEFINE v_cont             INT 
    
    DECLARE cur_detalle CURSOR FOR SELECT  tpo.tpo_credito_ocg, tpo.desc_credito_ocg, det.cve_ent_financiera, det.ent_financiera_desc,
                                           det.banco_interlocutor, det.clabe, det.rfc_ent_financiera, det.cta_contable, det.cve_bloqueo
                                   FROM    cat_tpo_credito_ocg tpo, cat_cta_cnt_ocg det
                                   WHERE   tpo.tpo_credito_ocg = det.tpo_credito AND 
                                           det.tpo_credito = p_tpo_credito
                                   ORDER BY 1,3
                
    --limpiamos el arreglo 
    CALL array_detalle.clear()
    
    FOREACH cur_detalle INTO rec_detalle.*
        LET v_cont = v_cont + 1
        LET array_detalle[v_cont].a_phantom_tpo_credito = rec_detalle.r_tpo_credito
        LET array_detalle[v_cont].a_tpo_credito         = rec_detalle.r_tpo_credito ||" - ",rec_detalle.r_desc_credto_ocg
        LET array_detalle[v_cont].a_phantom_cve_entidad = rec_detalle.r_cve_ent_financiera
        LET array_detalle[v_cont].a_entidad_financiera  = rec_detalle.r_cve_ent_financiera USING "&&&" ," - " ,rec_detalle.r_ent_financiera_desc
        LET array_detalle[v_cont].a_banco_interlocutor  = rec_detalle.r_banco_interlocutor USING "&&&&"
        LET array_detalle[v_cont].a_clabe               = rec_detalle.r_clabe
        LET array_detalle[v_cont].a_rfc_ent_financiera  = rec_detalle.r_rfc_ent_financiera
        LET array_detalle[v_cont].a_cuenta_contable     = rec_detalle.r_cuenta_contable
        LET array_detalle[v_cont].a_phantom_cve_bloqueo = rec_detalle.r_cve_cloqueo
        
        IF (rec_detalle.r_cve_cloqueo == 0) THEN
            LET array_detalle[v_cont].a_cve_cloqueo    = rec_detalle.r_cve_cloqueo ||" - ","SIN BLOQUEO"
        ELSE 
           IF (rec_detalle.r_cve_cloqueo == 1) THEN 
               LET array_detalle[v_cont].a_cve_cloqueo = rec_detalle.r_cve_cloqueo ||" - ","CON BLOQUEO"
           END IF 
        END IF 
    END FOREACH 
END FUNCTION 

FUNCTION captura_datos()
DEFINE cbx_ent_financiera ui.ComboBox
DEFINE cbx_tipo_credito   ui.ComboBox

--combo entidad financiera
DEFINE rec_entidad_financiera RECORD 
    r_cve_ent_financiera      SMALLINT,
    r_ent_financiera_desc     CHAR(60)
END RECORD 

--combo tipo de credito
DEFINE rec_tipo_credito  RECORD
    r_id_tpo_credito_ocg SMALLINT,
    r_tpo_credito_ocg    SMALLINT,
    r_desc_credito_ocg   CHAR(25)
END RECORD 

DEFINE v_ent_financiera_desc   CHAR(70)
DEFINE v_tipo_credito_desc     CHAR(30)
DEFINE v_banco_inter           CHAR(4)--LIKE cat_cta_cnt_ocg.banco_interlocutor
DEFINE v_clabe                 CHAR(18)
DEFINE v_rfc_ent               CHAR(12)
DEFINE v_cuenta_contable       CHAR(10)
DEFINE v_rfc_ent_fin_pref      CHAR(3)
DEFINE v_count_ent             INTEGER
DEFINE txt_ent_financiera_desc CHAR(60)
DEFINE v_bandera               BOOLEAN 
    
    OPEN WINDOW win3 WITH FORM "DISM033"  
        LET cbx_ent_financiera = ui.ComboBox.forName("r_cve_ent_financiera")
        LET cbx_tipo_credito   = ui.ComboBox.forName("r_tpo_credito_ocg")

        ---llenado del combo r_ent_financiera_desc
        DECLARE cur_ent_financiera CURSOR FOR SELECT cve_ent_financiera, ent_financiera_desc 
                                          FROM cat_entidad_financiera
                                          ORDER BY 1;
        FOREACH cur_ent_financiera INTO rec_entidad_financiera.*
            LET v_ent_financiera_desc = rec_entidad_financiera.r_cve_ent_financiera USING "&&&" ," - ",rec_entidad_financiera.r_ent_financiera_desc
            CALL cbx_ent_financiera.addItem(rec_entidad_financiera.r_cve_ent_financiera,v_ent_financiera_desc)
        END FOREACH 

        --llenado del combo r_desc_credito_ocg
        DECLARE cur_tipo_credito CURSOR FOR SELECT id_tpo_credito_ocg,tpo_credito_ocg,desc_credito_ocg FROM cat_tpo_credito_ocg
                                        WHERE ind_activo = 1
                                        ORDER BY 1;   
        FOREACH cur_tipo_credito INTO rec_tipo_credito.*
            LET v_tipo_credito_desc = rec_tipo_credito.r_tpo_credito_ocg ||" - ", rec_tipo_credito.r_desc_credito_ocg
            CALL cbx_tipo_credito.addItem(rec_tipo_credito.r_tpo_credito_ocg,v_tipo_credito_desc)
        END FOREACH 
        
        INPUT BY NAME rec_entidad_financiera.r_cve_ent_financiera,rec_tipo_credito.r_tpo_credito_ocg
                      ,v_banco_inter, v_clabe, v_rfc_ent, v_cuenta_contable      ATTRIBUTE(UNBUFFERED)
                         
            ON ACTION ACCEPT 
                IF (rec_entidad_financiera.r_cve_ent_financiera IS NULL ) THEN
                    CALL fgl_winmessage("Información","La Entidad Financiera No puede ser Nula","exclamation")
                    NEXT FIELD r_cve_ent_financiera
                    ELSE 
                      --Recupera la descripcion de la entidad financiera
                    SELECT ent_financiera_desc INTO txt_ent_financiera_desc FROM cat_entidad_financiera
                                            WHERE cve_ent_financiera = rec_entidad_financiera.r_cve_ent_financiera
                 END IF 
                 
                --validaciòn tipo de credito
                IF (rec_tipo_credito.r_tpo_credito_ocg) IS NULL THEN
                    CALL fgl_winmessage("Información","El tipo de Credito no puede ser Nulo","exclamation")
                    NEXT FIELD r_tpo_credito_ocg
                END IF

                 --validacion banco interlocutor
                IF (v_banco_inter IS NULL) THEN
                    --NEXT FIELD v_banco_inter
                ELSE 
                    LET v_bandera = fn_es_numero_banco_inter(v_banco_inter)
                    IF (v_bandera == 1) THEN 
                        CALL fgl_winmessage("Información","El Banco Interlocutor debe ser Numerico","exclamation")
                        NEXT FIELD v_banco_inter
                    END IF 
                END IF 
                
                --validacion clabe
                IF (v_clabe IS NULL ) THEN 
                   -- NEXT FIELD v_clabe
                ELSE 
                    IF LENGTH(v_clabe)< 18 THEN 
                        CALL fgl_winmessage("información","La CLABE no puede tener menos de 18 digitos","exclamation")
                        NEXT FIELD v_clabe
                    ELSE 
                       LET v_bandera= fn_evalua_es_numero(v_clabe)
                      IF (v_bandera == 1) THEN 
                          CALL fgl_winmessage("información","La CLABE debe ser Numerico","exclamation")
                          NEXT FIELD v_clabe 
                       END IF 
                    END IF 
                END IF 

                   --Validacion RFC Entidad Finaciera
                IF LENGTH(v_rfc_ent) < 12 THEN
                    CALL fgl_winmessage("Informaciòn","El RFC de la Entidad no puede ser menor a 12 digitos ","exclamation")
                    NEXT FIELD v_rfc_ent
                ELSE 
                    LET v_bandera= fn_es_alfanumerico(v_rfc_ent)
                    IF (v_bandera == 1) THEN 
                        CALL fgl_winmessage("Información","El RFC debe ser Alfanumerico","exclamation")
                        NEXT FIELD v_rfc_ent
                    ELSE 
                      -- Genera prefijo del RFC de la entidad financiera
                      LET v_rfc_ent_fin_pref = v_rfc_ent[10,12]
                    END IF 
                END IF 

                --validacion cuenta contable
                IF (v_cuenta_contable IS NULL) THEN 
                   -- NEXT FIELD v_cuenta_contable
                ELSE 
                    IF LENGTH(v_cuenta_contable) < 10 THEN
                        CALL fgl_winmessage("información","La Cuenta Contable no puede ser menor a 10 digitos","exclamation")
                        NEXT FIELD v_cuenta_contable
                    ELSE 
                       LET v_bandera = fn_evalua_es_numero(v_cuenta_contable)
                       IF (v_bandera == 1) THEN
                         CALL fgl_winmessage("Información","La Cuenta contable debe ser Numerico","exclamation")
                         NEXT FIELD v_cuenta_contable
                       END IF 
                    END IF 
                END IF 

                -- Antes de Insertar se verifica que el registro no exista
                  SELECT COUNT(*) INTO v_count_ent FROM cat_cta_cnt_ocg
                                WHERE cve_ent_financiera = rec_entidad_financiera.r_cve_ent_financiera
                                AND tpo_credito = rec_tipo_credito.r_tpo_credito_ocg
                  IF (v_count_ent >= 1) THEN 
                     CALL fgl_winmessage("Información","Entidad Financiera ya está dada de alta en el catálogo para este tipo de crédito","exclamation")
                  ELSE 
                    INSERT INTO cat_cta_cnt_ocg(
                                            cve_ent_financiera,
                                            ent_financiera_desc,
                                            banco_interlocutor,
                                            clabe,
                                            rfc_ent_financiera,
                                            rfc_ent_fin_pref,
                                            cta_contable,
                                            tpo_credito,
                                            cve_bloqueo)
                                     VALUES(rec_entidad_financiera.r_cve_ent_financiera, --cve ent_financiera
                                            txt_ent_financiera_desc,                     --descripcion de la entidad
                                            v_banco_inter,                               --Banco interlocutor
                                            v_clabe,                                     --CLABE
                                            v_rfc_ent,                                   -- RFC Ent Financiera
                                            v_rfc_ent_fin_pref,                          --Prefijo RFC Ent Financiera
                                            v_cuenta_contable,                           --Cuenta Contable
                                            rec_tipo_credito.r_tpo_credito_ocg,          --Tpo Credito
                                            0);                                          -- Cve Boqueo O sin bloqueo
                                            
                    IF SQLCA.sqlcode = 0 THEN
                        CALL fgl_winmessage("Información"," El registro se ha Insertado exitosamente.","information")
                        CALL carga_detalle_ent_financiera(rec_tipo_credito.r_tpo_credito_ocg)
                        EXIT INPUT 
                    ELSE 
                      CALL fgl_winmessage("Información","No se pudo ingresar el registro correctamente ","exclamation")
                    END IF 
                END IF 
                
                {DISPLAY "Entidad financiera", " : ",rec_entidad_financiera.r_cve_ent_financiera
                DISPLAY "Descripcion Entidad", " : ", txt_ent_financiera_desc
                DISPLAY "Tipo Credito", " : " , rec_tipo_credito.r_tpo_credito_ocg
                DISPLAY "Banco interlocutor", " : ",v_banco_inter
                DISPLAY "CLABE", " : ", v_clabe
                DISPLAY "RFC Entidad", " : ", v_rfc_ent
                DISPLAY "Cuenta Contable", " : " , v_cuenta_contable 
                DISPLAY "Prefijo RFC"," : ",v_rfc_ent_fin_pref}
                
            ON ACTION CANCEL
                EXIT INPUT 
        END INPUT 
    CLOSE WINDOW Win3
END FUNCTION 

FUNCTION modificar_det_ent_financiera(p_mod_ent_financiera)

DEFINE p_mod_ent_financiera RECORD 
    p_phantom_tpo_cred   SMALLINT,
    p_tpo_credito        CHAR(30), --concatenado
    p_phantom_cve_ent    SMALLINT,
    p_ent_financiera     CHAR(70),--concatendado
    p_banco_inter        CHAR(4),--LIKE cat_cta_cnt_ocg.banco_interlocutor,
    p_clabe              CHAR(18),
    p_rfc_ent_financiera CHAR(12),
    p_cuenta_contable    CHAR(10),
    p_phantom_cve_bloq   SMALLINT,
    p_cve_bloqueo        CHAR(20)--concatenado
END RECORD 

--variables para loe elements del formulario
DEFINE m_phantom_tpo_credito SMALLINT
DEFINE m_tpo_credito         CHAR(30) --concatenado
DEFINE m_phantom_cve_ent     SMALLINT
DEFINE m_ent_financiera      CHAR(70) --concatenado
DEFINE m_banco_inter         CHAR(4)--LIKE cat_cta_cnt_ocg.banco_interlocutor
DEFINE m_clabe               CHAR(18)
DEFINE m_rfc_ent_financiera  CHAR(12)
DEFINE m_cuenta_contable     CHAR(10)
DEFINE v_rfc_ent_fin_pref2   CHAR(3)
DEFINE v_bandera             BOOLEAN 

    LET m_phantom_tpo_credito = p_mod_ent_financiera.p_phantom_tpo_cred
    LET m_phantom_cve_ent = p_mod_ent_financiera.p_phantom_cve_ent
    LET m_ent_financiera = p_mod_ent_financiera.p_ent_financiera
    LET m_tpo_credito    = p_mod_ent_financiera.p_tpo_credito
    LET m_banco_inter    = p_mod_ent_financiera.p_banco_inter
    LET m_clabe          = p_mod_ent_financiera.p_clabe
    LET m_rfc_ent_financiera = p_mod_ent_financiera.p_rfc_ent_financiera
    LET m_cuenta_contable = p_mod_ent_financiera.p_cuenta_contable
    
    OPEN WINDOW win4 WITH FORM "DISM034"
        INPUT BY NAME m_banco_inter, m_clabe,m_rfc_ent_financiera, m_cuenta_contable ATTRIBUTE(UNBUFFERED, WITHOUT DEFAULTS)
            BEFORE INPUT 
                DISPLAY BY NAME m_ent_financiera,
                                m_tpo_credito,
                                m_banco_inter,
                                m_clabe,
                                m_rfc_ent_financiera,
                                m_cuenta_contable
                         
            ON ACTION ACCEPT 
                --valida la entidad
                IF (m_ent_financiera IS NULL ) THEN
                    CALL fgl_winmessage("Información","La Entidad Financiera No puede ser Nula","exclamation")
                    NEXT FIELD m_ent_financiera
                END IF 

                --validaciòn tipo de credito
                IF (m_tpo_credito) IS NULL THEN
                    CALL fgl_winmessage("Información","El tipo de Credito no puede ser Nulo","exclamation")
                    NEXT FIELD m_tpo_credito
                END IF
               
                 --validacion banco interlocutor
                IF (m_banco_inter IS NULL) THEN
                   -- NEXT FIELD v_banco_inter
                ELSE 
                    LET v_bandera = fn_es_numero_banco_inter(m_banco_inter)
                    IF (v_bandera == 1) THEN 
                        CALL fgl_winmessage("Información","El Banco Interlocutor debe ser Numerico","exclamation")
                        NEXT FIELD m_banco_inter
                    END IF 
                END IF 
                
                 --validacion clabe
                IF (m_clabe IS NULL ) THEN 
                   -- NEXT FIELD v_clabe
                ELSE 
                    IF LENGTH(m_clabe)< 18 THEN 
                        CALL fgl_winmessage("información","La CLABE no puede tener menos de 18 digitos","exclamation")
                        NEXT FIELD m_clabe
                    ELSE 
                       LET v_bandera= fn_evalua_es_numero(m_clabe)
                      IF (v_bandera == 1) THEN 
                          CALL fgl_winmessage("información","La CLABE debe ser Numerico","exclamation")
                          NEXT FIELD m_clabe 
                       END IF 
                    END IF 
                END IF 

                   --Validacion RFC Entidad Finaciera
                IF LENGTH(m_rfc_ent_financiera) < 12 THEN
                    CALL fgl_winmessage("Informaciòn","El RFC de la Entidad no puede ser menor a 12 digitos ","exclamation")
                    NEXT FIELD m_rfc_ent_financiera
                ELSE 
                    LET v_bandera= fn_es_alfanumerico(m_rfc_ent_financiera)
                    IF (v_bandera == 1) THEN 
                        CALL fgl_winmessage("Información","El RFC debe ser Alfanumerico","exclamation")
                        NEXT FIELD m_rfc_ent_financiera
                    ELSE 
                      -- Genera prefijo del RFC de la entidad financiera
                      LET v_rfc_ent_fin_pref2 = m_rfc_ent_financiera[10,12]
                    END IF 
                END IF 

                --validacion cuenta contable
                IF (m_cuenta_contable IS NULL) THEN                  
                ELSE 
                    IF LENGTH(m_cuenta_contable) < 10 THEN
                        CALL fgl_winmessage("información","La Cuenta Contable no puede ser menor a 10 digitos","exclamation")
                        NEXT FIELD m_cuenta_contable
                    ELSE 
                       LET v_bandera = fn_evalua_es_numero(m_cuenta_contable)
                       IF (v_bandera == 1) THEN
                         CALL fgl_winmessage("Información","La Cuenta contable debe ser Numerico","exclamation")
                         NEXT FIELD m_cuenta_contable
                       END IF 
                    END IF 
                END IF 

               { DISPLAY "Entidad : ",  m_ent_financiera
                DISPLAY "tipo de credito cve : ",m_phantom_tpo_credito 
                DISPLAY "tpo_credito : ",  m_tpo_credito
                DISPLAY "banco inter : ",  m_banco_inter
                DISPLAY "clabe : ",  m_clabe
                DISPLAY "RFC Ent : ",  m_rfc_ent_financiera
                DISPLAY "Prefijo :" , v_rfc_ent_fin_pref2
                DISPLAY "Cuenta Contable : ",  m_cuenta_contable}

                -- si todo lo anterios es correcto, se Actualiza
                  UPDATE cat_cta_cnt_ocg 
                    SET banco_interlocutor = m_banco_inter,
                        clabe              = m_clabe,
                        rfc_ent_financiera = m_rfc_ent_financiera,
                        rfc_ent_fin_pref   = v_rfc_ent_fin_pref2,
                        cta_contable       = m_cuenta_contable
                    WHERE 
                        cve_ent_financiera     = m_phantom_cve_ent AND 
                        tpo_credito            = m_phantom_tpo_credito
                    IF (SQLCA.sqlcode == 0) THEN
                        CALL fgl_winmessage("Información"," El registro ha sido modificado exitosamente.","information")
                        CALL carga_detalle_ent_financiera(m_phantom_tpo_credito)
                        EXIT INPUT
                    ELSE 
                         CALL fgl_winmessage("Información","No se pudo modificar el registro ","exclamation")
                    END IF 

            ON ACTION CANCEL 
                EXIT INPUT 
                
        END INPUT 
    CLOSE WINDOW WIN4
    
END FUNCTION

FUNCTION bloquear_ent_financiera(pb_bloq_ent_financiera)

DEFINE pb_bloq_ent_financiera RECORD
    b_phantom_tpo_cred   SMALLINT,
    b_tpo_credito        CHAR(30),--concatenado
    b_phantom_cve_ent    SMALLINT,
    b_ent_financiera     CHAR(70),--concatenado
    b_banco_inter        CHAR(4),--LIKE cat_cta_cnt_ocg.banco_interlocutor,
    b_clabe              CHAR(18),
    b_rfc_ent_financiera CHAR(12),
    b_cuenta_contable    CHAR(10),
    b_phantom_cve_bloq   SMALLINT,
    b_bloqueo            CHAR(20)--concatenado
END RECORD  

    OPEN WINDOW win5 WITH FORM "DISM035"
        MENU ""
            BEFORE MENU 
            DISPLAY BY NAME pb_bloq_ent_financiera.b_ent_financiera,
                            pb_bloq_ent_financiera.b_tpo_credito,
                            pb_bloq_ent_financiera.b_banco_inter,
                            pb_bloq_ent_financiera.b_clabe,
                            pb_bloq_ent_financiera.b_rfc_ent_financiera,
                            pb_bloq_ent_financiera.b_cuenta_contable,
                            pb_bloq_ent_financiera.b_bloqueo
                        
            ON ACTION ACCEPT
                IF (pb_bloq_ent_financiera.b_phantom_cve_bloq == 1) THEN
                    CALL fgl_winmessage("información","Este registro actualmente ya se encuentra bloqueado","exclamation")
                    EXIT MENU 
                ELSE 
                  UPDATE cat_cta_cnt_ocg
                     SET cve_bloqueo = 1
                   WHERE cve_ent_financiera = pb_bloq_ent_financiera.b_phantom_cve_ent AND 
                        tpo_credito        = pb_bloq_ent_financiera.b_phantom_tpo_cred
                        IF (SQLCA.sqlcode == 0) THEN 
                            CALL fgl_winmessage("información","El registro se ha bloqueado exitosamente","information")
                            CALL carga_detalle_ent_financiera(pb_bloq_ent_financiera.b_phantom_tpo_cred)
                            EXIT MENU 
                        ELSE 
                            CALL fgl_winmessage("información","No se pudo bloquear el registro","exclamation")
                            EXIT MENU 
                        END IF 
                END IF 
                
            ON ACTION CANCEL 
                EXIT MENU 
        END MENU 
    CLOSE WINDOW win5 
END FUNCTION 

FUNCTION Desbloquear_ent_financiera(pd_bloq_ent_financiera)

DEFINE pd_bloq_ent_financiera RECORD
    b_phantom_tpo_cred   SMALLINT,
    b_tpo_credito        CHAR(30),--concatenado
    b_phantom_cve_ent    SMALLINT,
    b_ent_financiera     CHAR(70),--concatenado
    b_banco_inter        CHAR(4),--LIKE cat_cta_cnt_ocg.banco_interlocutor,
    b_clabe              CHAR(18),
    b_rfc_ent_financiera CHAR(12),
    b_cuenta_contable    CHAR(10),
    b_phantom_cve_bloq   SMALLINT,
    b_bloqueo            CHAR(20)--concatenado
END RECORD  

    OPEN WINDOW win5 WITH FORM "DISM036"
        MENU ""
            BEFORE MENU 
            DISPLAY BY NAME pd_bloq_ent_financiera.b_ent_financiera,
                            pd_bloq_ent_financiera.b_tpo_credito,
                            pd_bloq_ent_financiera.b_banco_inter,
                            pd_bloq_ent_financiera.b_clabe,
                            pd_bloq_ent_financiera.b_rfc_ent_financiera,
                            pd_bloq_ent_financiera.b_cuenta_contable,
                            pd_bloq_ent_financiera.b_bloqueo
                        
            ON ACTION ACCEPT
                IF (pd_bloq_ent_financiera.b_phantom_cve_bloq == 0) THEN
                    CALL fgl_winmessage("información","Este registro actualmente ya se encuentra Desbloqueado","exclamationt")
                    EXIT MENU 
                ELSE 
                  UPDATE cat_cta_cnt_ocg
                     SET cve_bloqueo = 0
                     WHERE  cve_ent_financiera = pd_bloq_ent_financiera.b_phantom_cve_ent AND 
                            tpo_credito        = pd_bloq_ent_financiera.b_phantom_tpo_cred
                        IF (SQLCA.sqlcode == 0) THEN 
                            CALL fgl_winmessage("información","El registro se ha Desbloqueado exitosamente","information")
                            CALL carga_detalle_ent_financiera(pd_bloq_ent_financiera.b_phantom_tpo_cred)
                            EXIT MENU 
                        ELSE 
                            CALL fgl_winmessage("información","No se pudo Desbloquear el registro","exclamation")
                            EXIT MENU 
                        END IF 
                END IF 
                
            ON ACTION CANCEL 
                EXIT MENU 
        END MENU 
    CLOSE WINDOW win5 
END FUNCTION 

FUNCTION fn_evalua_es_numero(p_valor)
   DEFINE p_valor       STRING    
   DEFINE v_numero      SMALLINT
   DEFINE v_espacio     SMALLINT 
   DEFINE i             INTEGER 

   LET v_numero = 0  
    
   FOR i=1 TO p_valor.getLength()
     IF (p_valor.subString(i,i) == 0 OR p_valor.subString(i,i) == 1 OR 
         p_valor.subString(i,i) == 2 OR p_valor.subString(i,i) == 3 OR 
         p_valor.subString(i,i) == 4 OR p_valor.subString(i,i) == 5 OR 
         p_valor.subString(i,i) == 6 OR p_valor.subString(i,i) == 7 OR 
         p_valor.subString(i,i) == 8 OR p_valor.subString(i,i) == 9 ) THEN  
        LET v_numero = 0
     ELSE 
        LET v_numero = 1
        IF (v_numero == 1) THEN 
            EXIT FOR 
        END IF 
     END IF       
   END FOR 
  
   RETURN v_numero
END FUNCTION

FUNCTION fn_es_alfanumerico(p_valor)
DEFINE p_valor       STRING    
DEFINE v_numero      SMALLINT
DEFINE i             INTEGER 

   LET v_numero = 0
  
   FOR i=1 TO p_valor.getLength()
     IF (p_valor.subString(i,i) == 0 OR p_valor.subString(i,i) == 1 OR 
         p_valor.subString(i,i) == 2 OR p_valor.subString(i,i) == 3 OR 
         p_valor.subString(i,i) == 4 OR p_valor.subString(i,i) == 5 OR 
         p_valor.subString(i,i) == 6 OR p_valor.subString(i,i) == 7 OR 
         p_valor.subString(i,i) == 8 OR p_valor.subString(i,i) == 9 OR 
         p_valor.subString(i,i) == "A" OR p_valor.subString(i,i) == "B" OR 
         p_valor.subString(i,i) == "C" OR p_valor.subString(i,i) == "D" OR 
         p_valor.subString(i,i) == "E" OR p_valor.subString(i,i) == "F" OR 
         p_valor.subString(i,i) == "G" OR p_valor.subString(i,i) == "H" OR 
         p_valor.subString(i,i) == "I" OR p_valor.subString(i,i) == "J" OR 
         p_valor.subString(i,i) == "K" OR p_valor.subString(i,i) == "L" OR 
         p_valor.subString(i,i) == "M" OR p_valor.subString(i,i) == "N" OR
         p_valor.subString(i,i) == "Ñ" OR p_valor.subString(i,i) == "O" OR
         p_valor.subString(i,i) == "P" OR p_valor.subString(i,i) == "Q" OR 
         p_valor.subString(i,i) == "R" OR p_valor.subString(i,i) == "S" OR
         p_valor.subString(i,i) == "T" OR p_valor.subString(i,i) == "U" OR
         p_valor.subString(i,i) == "V" OR p_valor.subString(i,i) == "W" OR
         p_valor.subString(i,i) == "X" OR p_valor.subString(i,i) == "Y" OR
         p_valor.subString(i,i) == "Z"  ) THEN  
        
        LET v_numero = 0
     ELSE 
        LET v_numero = 1
        IF (v_numero == 1) THEN 
            EXIT FOR 
        END IF 
    
     END IF 
   END FOR 
  
   RETURN v_numero
END FUNCTION

FUNCTION fn_es_numero_banco_inter(p_valor)
   DEFINE p_valor       STRING    
   DEFINE v_numero      SMALLINT
   DEFINE v_espacio     SMALLINT 
   DEFINE i             INTEGER 

   LET v_numero = 0  
   
   FOR i=1 TO p_valor.getLength()
     IF (p_valor.subString(i,i) == 0 OR p_valor.subString(i,i) == 1 OR 
         p_valor.subString(i,i) == 2 OR p_valor.subString(i,i) == 3 OR 
         p_valor.subString(i,i) == 4 OR p_valor.subString(i,i) == 5 OR 
         p_valor.subString(i,i) == 6 OR p_valor.subString(i,i) == 7 OR 
         p_valor.subString(i,i) == 8 OR p_valor.subString(i,i) == 9 OR 
         p_valor.subString(i,i) == " ") THEN  
        LET v_numero = 0
     ELSE 
        LET v_numero = 1
        IF (v_numero == 1) THEN 
            EXIT FOR 
         END IF 
     END IF     
   END FOR 
  
   RETURN v_numero
END FUNCTION

FUNCTION Archivo_ent_financiera(v_tpo_credito)
  DEFINE 
    v_tpo_credito                  SMALLINT

  DEFINE arr_cat_cnt               DYNAMIC ARRAY OF RECORD
    tpo_credito                    SMALLINT, 
    desc_tpo_credito               CHAR(30),
    cve_entidad                    SMALLINT,
    desc_entidad_financiera        CHAR(70),
    banco_interlocutor             CHAR(4),
    clabe                          CHAR(18),
    rfc_ent_financiera             CHAR(12),
    cuenta_contable                CHAR(10),
    cve_bloqueo                    SMALLINT,
    desc_cve_bloqueo               CHAR(20)
  END RECORD
    
  DEFINE 
    v_nom_archivo                  VARCHAR(60),  --Nombre del archivo de salida
    v_ruta_envio_dis               CHAR(60),
    v_ruta_nomarch                 VARCHAR(100), --Ruta y nombre del archivo de salida
    v_ch_arch_salida               BASE.CHANNEL,
    v_comando_dos                  STRING,
    v_titulos                      STRING,                 
    v_encabezado                   STRING,
    v_detalle                      STRING,
    v_sumario                      STRING

  DEFINE 
    v_fecha_archivo                DATE,  
    v_hora_archivo                 DATETIME HOUR TO HOUR ,
    v_min_archivo                  DATETIME MINUTE TO MINUTE,
    v_sec_archivo                  DATETIME SECOND TO SECOND,
    v_hora                         STRING,
    v_indice                       SMALLINT

  DEFINE v_desc_credito            VARCHAR(50)
  DEFINE v_interface               CHAR(2)
  DEFINE v_tot_registros           SMALLINT 

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".ocg"
  LET v_nom_archivo   = "/fact_cta_cnt_acree_", v_hora

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --Se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  SELECT a.desc_credito_ocg   ---Tipo de Crédito
  INTO   v_desc_credito 
  FROM   cat_tpo_credito_ocg a
  WHERE  a.tpo_credito_ocg = v_tpo_credito
  AND    a.ind_activo  = 1

  LET v_encabezado = "Tipo Crédito: ", v_tpo_credito || " - ", v_desc_credito
  CALL v_ch_arch_salida.write([v_encabezado])
   
  LET g_sql_txt = "\n SELECT tpo.tpo_credito_ocg, ",
                  "\n        tpo.desc_credito_ocg, ",
                  "\n        det.cve_ent_financiera, ",
                  "\n        det.ent_financiera_desc, ",
                  "\n        det.banco_interlocutor, ",
                  "\n        det.clabe, ",
                  "\n        det.rfc_ent_financiera, ",
                  "\n        det.cta_contable, ",
                  "\n        det.cve_bloqueo ",
                  "\n FROM   cat_tpo_credito_ocg tpo, ",
                  "\n        cat_cta_cnt_ocg det ",                  
                  "\n WHERE  tpo.tpo_credito_ocg = det.tpo_credito ",
                  "\n AND    det.tpo_credito     = ", v_tpo_credito

  LET g_sql_txt = g_sql_txt,"\n ORDER BY 1, 3 "
                
  DISPLAY "g_sql_txt: -",g_sql_txt,"-" 
   
  PREPARE ps_arch_cta_cnt_acree FROM g_sql_txt
  DECLARE cur_arch_cta_cnt_acree CURSOR FOR ps_arch_cta_cnt_acree
  
  LET v_indice     = 1

  LET v_encabezado = " "
  CALL v_ch_arch_salida.write([v_encabezado])
      
  LET v_titulos = "TIPO CRÉDITO |ENTIDAD FINANCIERA |BANCO INTERLOCUTOR |CLABE |RFC ENTIDAD FINANCIERA |CUENTA CONTABLE |CLAVE BLOQUEO |FECHA ARCHIVO |INTERFACE "
  CALL v_ch_arch_salida.write([v_titulos])
  
  FOREACH cur_arch_cta_cnt_acree INTO arr_cat_cnt[v_indice].tpo_credito,
                                      v_desc_credito,
                                      arr_cat_cnt[v_indice].cve_entidad,  
                                      arr_cat_cnt[v_indice].desc_entidad_financiera,
                                      arr_cat_cnt[v_indice].banco_interlocutor,
                                      arr_cat_cnt[v_indice].clabe,
                                      arr_cat_cnt[v_indice].rfc_ent_financiera,
                                      arr_cat_cnt[v_indice].cuenta_contable,
                                      arr_cat_cnt[v_indice].cve_bloqueo

    IF v_tpo_credito = 2 THEN
       LET v_interface = 'AS'
    END IF

    IF v_tpo_credito = 3 THEN
       LET v_interface = 'UG'
    END IF

    IF v_tpo_credito = 5 THEN
       LET v_interface = 'AS'
    END IF

    LET v_desc_credito                                = arr_cat_cnt[v_indice].tpo_credito || " - ", v_desc_credito
    LET arr_cat_cnt[v_indice].desc_entidad_financiera = arr_cat_cnt[v_indice].cve_entidad USING "&&&" ," - ", arr_cat_cnt[v_indice].desc_entidad_financiera
    LET arr_cat_cnt[v_indice].banco_interlocutor      = arr_cat_cnt[v_indice].banco_interlocutor USING "&&&&"
        
    IF (arr_cat_cnt[v_indice].cve_bloqueo == 0) THEN
       LET arr_cat_cnt[v_indice].desc_cve_bloqueo    = arr_cat_cnt[v_indice].cve_bloqueo ||" - ","SIN BLOQUEO"
    ELSE 
       IF (arr_cat_cnt[v_indice].cve_bloqueo == 1) THEN 
          LET arr_cat_cnt[v_indice].desc_cve_bloqueo = arr_cat_cnt[v_indice].cve_bloqueo ||" - ","CON BLOQUEO"
       END IF 
    END IF 

    LET v_detalle = v_desc_credito CLIPPED, "|",
                    arr_cat_cnt[v_indice].desc_entidad_financiera CLIPPED, "|",
                    arr_cat_cnt[v_indice].banco_interlocutor, "|",
                    arr_cat_cnt[v_indice].clabe, "|",
                    arr_cat_cnt[v_indice].rfc_ent_financiera, "|",
                    arr_cat_cnt[v_indice].cuenta_contable, "|",
                    arr_cat_cnt[v_indice].desc_cve_bloqueo CLIPPED, "|",
                    TODAY USING "dd-mm-yyyy", "|",
                    v_interface

    CALL v_ch_arch_salida.write([v_detalle])
        
    LET v_indice         = v_indice         + 1  
  END FOREACH

  FREE cur_arch_cta_cnt_acree
  
  CALL arr_cat_cnt.deleteElement(v_indice)
  LET v_indice        = v_indice - 1 
  LET v_tot_registros =  v_indice

  LET v_sumario = "TOTAL REGISTROS: | ",v_tot_registros

  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Cuentas Contables - Acreedor \n en la ruta "||v_ruta_nomarch,"information") 
  
END FUNCTION