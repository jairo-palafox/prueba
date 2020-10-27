################################################################################
#Modulo            => OCG                                                      #
#Programa          => OCGP29                                                   #
#Objetivo          => Programa que genera SP-005 POR PORTABILIDAD              #
#Autor             => Eduardo Ventura Bonola                                   #
#Fecha inicio      => 27 de Noviembre del 2018                                 #
################################################################################
DATABASE safre_viv
     DEFINE HOY                   DATE
     DEFINE v_nss                 CHAR(11)
     DEFINE v_cb1                 ui.ComboBox
     DEFINE v_cb2                 ui.ComboBox
     DEFINE v_num_ctr_int_ef      CHAR(18)
     DEFINE v_bimestre_ap_subsec  CHAR(6)
     DEFINE v_importe_devolver    DECIMAL(13,2)
    -- DEFINE v_imp_subsec_devuelto DECIMAL(13,2)
    -- DEFINE v_imp_ocg_devuelto    DECIMAL(13,2)
     DEFINE v_cb3                 ui.ComboBox
    -- DEFINE f_deposito            DATE
     
     DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod
     DEFINE p_s_titulo            STRING   -- titulo de la ventana
     DEFINE p_tipo_ejecucion      SMALLINT -- forma como ejecutara el programa

--     DEFINE v_entidad_financiera  SMALLINT
     DEFINE v_tipo_credito        CHAR(1)
     DEFINE v_tipo_devolucion     SMALLINT
     DEFINE v_today               char(10)

DEFINE v_entidad_financiera char(3)
DEFINE v_imp_subsec_devuelto char(15)
DEFINE v_imp_ocg_devuelto char(15)
DEFINE f_deposito date
DEFINE v_f_deposito char(8)
DEFINE v_cant_aceptados INTEGER
DEFINE v_cant_rechazados INTEGER
MAIN
    CALL init() #i
    
     -- se recupera la clave de usuario desde parametro 
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)
     
    -- se crea el archivo log
    CALL STARTLOG(p_usuario_cod CLIPPED|| ".OCGP29.log")

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF

    OPEN WINDOW w_principal WITH FORM "OCGP291"
       LET v_cb1 = ui.ComboBox.forName("v_entidad_financiera")
       LET v_cb2 = ui.ComboBox.forName("v_tipo_credito")
       LET v_cb3 = ui.ComboBox.forName("v_tipo_devolucion")
       
       INPUT BY NAME v_nss,v_entidad_financiera, v_tipo_credito,v_num_ctr_int_ef, v_bimestre_ap_subsec, v_importe_devolver, v_tipo_devolucion, f_deposito  ATTRIBUTES (UNBUFFERED)

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
   
    DEFINE v_arr_tipo_devolucion DYNAMIC ARRAY OF RECORD
      cve_tipo_devolucion    CHAR(1),
      descripcion            CHAR(80)
   END RECORD
   
   DEFINE cont_1             SMALLINT

   DECLARE cur_1 CURSOR FOR
   SELECT cve_ent_financiera,
          cve_ent_financiera||" - "||ent_financiera_desc
   FROM   cat_entidad_financiera

   --Combo Entidad Financiera--
   LET cont_1 = 1
   FOREACH cur_1 INTO v_arr_entidad_financiera[cont_1].*
       CALL v_cb1.addItem(v_arr_entidad_financiera[cont_1].cve_ent_financiera,v_arr_entidad_financiera[cont_1].descripcion)
       LET cont_1 = cont_1 + 1
   END FOREACH

   --Combo tipo de crédito--
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
   
   --Combo Tipo Devolución--
   LET v_arr_tipo_devolucion[1].cve_tipo_devolucion = "1"
   LET v_arr_tipo_devolucion[1].descripcion         = "1  - APORTACIONES SUBSECUENTE"
   CALL v_cb3.addItem(v_arr_tipo_devolucion[1].cve_tipo_devolucion, v_arr_tipo_devolucion[1].descripcion)
               
   LET v_arr_tipo_devolucion[2].cve_tipo_devolucion = "2"
   LET v_arr_tipo_devolucion[2].descripcion         = "2  - GARANTÍA"
   CALL v_cb3.addItem(v_arr_tipo_devolucion[2].cve_tipo_devolucion, v_arr_tipo_devolucion[2].descripcion)
   
END FUNCTION

FUNCTION segundo_paso()

   IF v_nss IS NULL THEN
      CALL fn_mensaje("Alterta", "Debe de ingresar NSS","stop")
   ELSE

      IF v_entidad_financiera IS NULL THEN
         CALL fn_mensaje("Alterta", "Debe de ingresar Entidad Financiera","stop")
      ELSE

         IF v_tipo_credito IS NULL THEN
            CALL fn_mensaje("Alterta", "Debe de ingresar Tipo de Crédito","stop")
         ELSE

            ---IF v_num_ctr_int_ef IS NULL THEN  -- si no se ingresa dato se deja en cero 
               ---CALL fn_mensaje("Alterta", "Debe de ingresar Entidad Financiera","stop")
            ---ELSE

               IF v_bimestre_ap_subsec IS NULL THEN
                  CALL fn_mensaje("Alterta", "Debe de ingresar bimestre","stop")
               ELSE

                  IF v_importe_devolver IS NULL THEN
                     CALL fn_mensaje("Alterta", "Debe de ingresar Importe","stop")
                  ELSE

                     IF v_tipo_devolucion IS NULL THEN
                        CALL fn_mensaje("Alterta", "Debe de ingresar tipo devolución","stop")
                     ELSE

                        IF f_deposito IS NULL THEN
                           CALL fn_mensaje("Alterta", "Debe de ingresar fecha de depósito","stop")
                        ELSE
                           CALL fn_sp005()
                        END IF

                     END IF

                  END IF

               END IF

            ---END IF

         END IF

      END IF

   END IF

END FUNCTION

FUNCTION fn_sp005()

   DEFINE v_s_qry          STRING
   DEFINE v_sp_error       SMALLINT
   DEFINE v_id_ctr_archivo SMALLINT

   SELECT "OK"
     FROM ocg_liquidacion
    WHERE situacion = 145
      AND diagnostico = 1
      AND estado = 70
      AND id_derechohabiente =
  (SELECT id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss = v_nss)
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        CALL fn_mensaje("Alterta", "No se encontró liquidación para los datos ingresados","stop")
    ELSE
       --se borra tabla y se realiza insert en temporal de carga de archivo 43bis
        DATABASE safre_tmp
       delete from safre_tmp:tmp_rec_det_ocg43
             where 1 = 1
       DATABASE  safre_viv

   IF v_tipo_devolucion = 1 THEN
      LET v_imp_subsec_devuelto = v_importe_devolver
      LET v_imp_ocg_devuelto    = ''
   END IF

   IF v_tipo_devolucion = 2 THEN
      LET v_imp_ocg_devuelto = v_importe_devolver
      LET v_imp_subsec_devuelto = ''
   END IF

    DATABASE safre_tmp

    LET v_f_deposito = f_deposito USING 'yyyymmdd'

    INSERT INTO tmp_rec_det_ocg43 VALUES (
'0',
'0',
'005',
'0',
'',
v_entidad_financiera,
v_nss,
v_num_ctr_int_ef,
'',
'',
'',
'',
'',
'',
'',
'',
'',
'1',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
'',
v_bimestre_ap_subsec,
v_imp_subsec_devuelto,
'',
v_imp_ocg_devuelto,
'',
v_f_deposito,
v_tipo_credito,
'',
'',
'')

       LET v_today = today USING 'yyyymmdd'

       UPDATE tmp_rec_det_ocg43
       SET f_envio = v_today
       DATABASE safre_viv

       -- se deja indice de archivo en cero y s ejecuta función que valida SP005
       LET v_id_ctr_archivo = 0
       LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_sp5_liquidacion(?)"
       PREPARE prp_exe_fn_proceso FROM v_s_qry
       EXECUTE prp_exe_fn_proceso USING v_id_ctr_archivo INTO v_sp_error,
                                                              v_cant_aceptados,
                                                              v_cant_rechazados

      IF v_sp_error = 0 THEN
         CALL fn_mensaje("Alterta", "Ejecución exitosa","stop")
      ELSE
         CALL fn_mensaje("Alterta", "No fue posible concluir la ejecución","stop")
       END IF
    END IF

END FUNCTION
