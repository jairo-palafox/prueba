################################################################################
#Modulo            => OCG                                                      #
#Programa          => OCGP28                                                   #
#Objetivo          => Programa que genera SP-005 POR PORTABILIDAD              #
#Autor             => Franco Ulloa Videla                                      #
#Fecha inicio      => 22 de Noviembre del 2018                                 #
################################################################################
DATABASE safre_viv
     DEFINE HOY                   DATE
     DEFINE v_nss                 CHAR(11)
     DEFINE v_cb1                 ui.ComboBox
     DEFINE v_cb2                 ui.ComboBox
     DEFINE v_f_liquida           DATE

     DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod
     DEFINE p_s_titulo            STRING   -- titulo de la ventana
     DEFINE p_tipo_ejecucion      SMALLINT -- forma como ejecutara el programa

     DEFINE v_entidad_financiera  CHAR(3)
     DEFINE v_tipo_credito        CHAR(1)
     

MAIN
    CALL init() #i
    
    -- se recupera la clave de usuario desde parametro 
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)
     
    -- se crea el archivo log
    CALL STARTLOG(p_usuario_cod CLIPPED|| ".OCGP28.log")

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF

    OPEN WINDOW w_principal WITH FORM "OCGP281"
       LET v_cb1 = ui.ComboBox.forName("v_entidad_financiera")
       LET v_cb2 = ui.ComboBox.forName("v_tipo_credito")
       
       INPUT BY NAME v_nss,v_entidad_financiera, v_tipo_credito, v_f_liquida ATTRIBUTES (UNBUFFERED)

           BEFORE INPUT
               CALL primer_paso()  --pp--Llena combos
                      
           ON ACTION ACCEPT
               CALL segundo_paso() --pp--Valida parámetros de entrada
               EXIT INPUT

           ON ACTION CANCEL
               EXIT INPUT
       END INPUT
    CLOSE WINDOW w_principal
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
   DEFINE v_arr_entidad_financiera DYNAMIC ARRAY OF RECORD
      cve_ent_financiera     CHAR(3),
      descripcion            CHAR(80)
   END RECORD

   DEFINE v_arr_tipo_credito DYNAMIC ARRAY OF RECORD
      cve_tipo_credito       CHAR(1),
      descripcion            CHAR(80)
   END RECORD
   
   DEFINE cont_1             SMALLINT

   DECLARE cur_1 CURSOR FOR
   SELECT cve_ent_financiera,
          cve_ent_financiera|  |ent_financiera_desc
   FROM   cat_entidad_financiera

   LET cont_1 = 1
   FOREACH cur_1 INTO v_arr_entidad_financiera[cont_1].*
       CALL v_cb1.addItem(v_arr_entidad_financiera[cont_1].cve_ent_financiera,v_arr_entidad_financiera[cont_1].descripcion)
       LET cont_1 = cont_1 + 1
   END FOREACH

   LET v_arr_tipo_credito[1].cve_tipo_credito   = "A"
   LET v_arr_tipo_credito[1].descripcion        = "A  - APOYO INFONAVIT"
   CALL v_cb2.addItem(v_arr_tipo_credito[1].cve_tipo_credito, v_arr_tipo_credito[1].descripcion)
               
   LET v_arr_tipo_credito[2].cve_tipo_credito   = "C"
   LET v_arr_tipo_credito[2].descripcion        = "C  - CONVENIDO"
   CALL v_cb2.addItem(v_arr_tipo_credito[2].cve_tipo_credito, v_arr_tipo_credito[2].descripcion)
               
   LET v_arr_tipo_credito[3].cve_tipo_credito   = "7"
   LET v_arr_tipo_credito[3].descripcion        = "7  - COFINANCIADO TIPO 7"
   CALL v_cb2.addItem(v_arr_tipo_credito[3].cve_tipo_credito, v_arr_tipo_credito[3].descripcion)
               
   LET v_arr_tipo_credito[4].cve_tipo_credito   = "8"
   LET v_arr_tipo_credito[4].descripcion        = "8  - COFINANCIADO TIPO 8"
   CALL v_cb2.addItem(v_arr_tipo_credito[4].cve_tipo_credito, v_arr_tipo_credito[4].descripcion)
   
END FUNCTION

FUNCTION segundo_paso()
#sp-------------------
    DEFINE v_s_qry           STRING
    DEFINE v_sp_error        SMALLINT
    DEFINE v_f_formalizacion DATE
    
    SELECT "OK"
    FROM   afi_derechohabiente A
    WHERE  A.nss = v_nss
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        CALL fn_mensaje("Alterta", "NSS Invalido","stop")
    ELSE
        IF v_entidad_financiera IS NULL THEN
            CALL fn_mensaje("Alterta", "Debes seleccionar una Entidad Financiera","stop")
        ELSE
            IF v_tipo_credito IS NULL THEN
                CALL fn_mensaje("Alterta", "Debes seleccionar un tipo de crédito","stop")
            ELSE
                IF v_f_liquida IS NULL THEN
                    CALL fn_mensaje("Alterta", "Debes de ingresar fecha de liquidación","stop")
                ELSE
                    --Se verifica si el trabajador tiene un crédito vigente en la EF que se desea liquidar--
                    SELECT "OK"
                    FROM   ocg_formalizacion A, ocg_detalle B
                    WHERE  B.nss                = v_nss
                    AND    B.subproceso         = 2
                    AND    B.id_ocg_detalle     = A.id_ocg_detalle
                    AND    A.cve_ent_financiera = v_entidad_financiera
                    AND    A.situacion         IN(55,60,70,80)
                    GROUP BY 1

                    IF STATUS =  NOTFOUND THEN
                        CALL fn_mensaje("Alterta", "No puedes liquidar un crédito vigente","stop")
                    ELSE
                        LET v_f_formalizacion = NULL
                        --Si el trabajador no cuenta con otro crédito vigente en otra EF      --
                        --o si la fecha de liquidación ingresada es menor a un crédito vigente--
                        --con otra EF, ejecuta la función                                     --
                        
                        SELECT C.f_formalizacion
                        INTO   v_f_formalizacion
                        FROM   ocg_formalizacion A, ocg_detalle B, ocg_acreditado C
                        WHERE  B.nss                  = v_nss
                        AND    B.subproceso           = 2
                        AND    B.id_ocg_detalle       = A.id_ocg_detalle
                        AND    A.cve_ent_financiera  <> v_entidad_financiera
                        AND    A.id_ocg_formalizacion = C.id_ocg_formalizacion
                        AND    A.situacion           IN(55,60,70,80)
                    
                        #####FALTA PONER UN ELSE######
                        IF v_f_formalizacion IS NULL OR v_f_formalizacion > v_f_liquida THEN
                            -- Se ejecuta la función que realiza el proceso
                            LET v_s_qry = "EXECUTE FUNCTION fn_ocg_sp005_portabilidad(?,?,?,?)"
                
                            PREPARE prp_exe_fn_proceso FROM v_s_qry
                            EXECUTE prp_exe_fn_proceso USING v_nss,v_entidad_financiera,v_tipo_credito,v_f_liquida INTO v_sp_error
                            IF v_sp_error = 0 THEN
                                CALL fn_mensaje("Alterta", "Ejecución exitosa","stop")
                            ELSE
                                CALL fn_mensaje("Alterta", "No fue posible concluir la ejecución","stop")
                            END IF
                        END IF
                    END IF
                END IF
            END IF
        END IF
    END IF
END FUNCTION
