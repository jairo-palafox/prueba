IMPORT FGL WSHelper

DATABASE safre_viv

GLOBALS "SI_ValidaPago_SOService.inc"

MAIN
DEFINE wsstatus          INTEGER
DEFINE gr_sql            STRING

DEFINE p_ejercicio       CHAR(04)
DEFINE p_documento       CHAR(10)
DEFINE p_referencia      CHAR(16)
DEFINE v_f_factura       DATE

DEFINE v_sociedad        CHAR(04)

DEFINE v_arr_val_pag_res DYNAMIC ARRAY OF RECORD
       v_documento       CHAR(10),
       v_ejercicio       CHAR(04),
       v_estatus         CHAR(02),
       v_monto           CHAR(13),
       v_wt_withcd       STRING,
       v_referencia      CHAR(16),
       v_f_pago          CHAR(10)
END RECORD 

DEFINE i                 INTEGER
DEFINE v_existe_info     INTEGER
DEFINE f_actual          DATE

DEFINE s_sociedad        STRING
DEFINE s_ejercicio       STRING
DEFINE s_documento       STRING
DEFINE v_f_pago          CHAR(10)

   -- Se asignan los parametros que vienen del fglrun
   LET p_referencia     = ARG_VAL(1)
   LET p_documento      = ARG_VAL(2)

   -- Se asignan las variables de default
   LET v_sociedad = 'INFO'   

   LET gr_sql = "\n SELECT UNIQUE f_factura ",
                "\n FROM   dis_ctr_factura_aps ",                     
                "\n WHERE  referencia = ? ",
                "\n AND    num_poliza = ? ";

   PREPARE ps_sl_fac FROM gr_sql
   EXECUTE ps_sl_fac USING p_referencia, p_documento INTO v_f_factura

   IF SQLCA.SQLCODE < 0 THEN    	 	 
      DISPLAY "1. Error al seleccionar la información en la Base de Datos."
      DISPLAY "\n p_referencia: ",p_referencia
      RETURN
   END IF

   LET p_ejercicio = YEAR(v_f_factura)

   LET s_sociedad  = v_sociedad
   LET s_ejercicio = s_ejercicio
   LET s_documento = s_documento

   LET s_sociedad  = s_sociedad.trim()
   LET s_ejercicio = s_ejercicio.trim()
   LET s_documento = s_documento.trim()

   -- Se asignan los valores a las variables de salida
   LET ZFICO_CONSULTAPAGOTABLET.SOCIEDAD  = v_sociedad
   LET ZFICO_CONSULTAPAGOTABLET.EJERCICIO = p_ejercicio
   LET ZFICO_CONSULTAPAGOTABLET.DOCUMENTO = p_documento   

   DISPLAY "\n Referencia: ",p_referencia

   DISPLAY "\n Parámetros para la llamada al WebService Valida Pago "
   DISPLAY "Sociedad : ",ZFICO_CONSULTAPAGOTABLET.SOCIEDAD   
   DISPLAY "Ejercicio: ",ZFICO_CONSULTAPAGOTABLET.EJERCICIO
   DISPLAY "Documento: ",ZFICO_CONSULTAPAGOTABLET.DOCUMENTO

   LET wsstatus = SI_ConsultarEstatusPago_SO_g()
   IF wsstatus != 0 THEN 
      IF wsstatus = -15553 THEN
        DISPLAY "wsstatus: -15553, Error en conexión al Web Service. "  
      ELSE 
        DISPLAY "-----------------------"
        DISPLAY "wsError.code         : ",wsError.code 		
        DISPLAY "wsError.codeNS       : ",wsError.codeNS		
        DISPLAY "wsError.description  : ",wsError.description
        DISPLAY "wsError.action		  : ",wsError.action
        DISPLAY "-----------------------"
      END IF 
      ERROR wsError.description      
   ELSE
      LET gr_sql = "\n UPDATE dis_ctr_factura_aps ",  
                   "\n SET    f_pago     = ? ", 
                   "\n WHERE  referencia = ? ";

      PREPARE ps_up_fac FROM gr_sql

      LET gr_sql = "\n SELECT count(*) ",
                   "\n FROM dis_ctr_edo_factura_aps ",
                   "\n WHERE num_poliza = ?", 
                   "\n AND referencia   = ? ",
                   "\n AND tpo_estado   = 2";

      PREPARE ps_sl_cnt FROM gr_sql

      LET gr_sql = "\n INSERT INTO dis_ctr_edo_factura_aps ", 
                   "\n VALUES(seq_dis_edo_factura_aps.NEXTVAL,?,?,2,?,TODAY)"; 

      PREPARE ps_ins_ef FROM gr_sql

      LET gr_sql = "\n UPDATE dis_ctr_edo_factura_aps ", 
                   "\n SET    estado      = ?, ",
                   "\n        f_actualiza = ? ",
                   --"\n WHERE  num_poliza  = ? ",
                   --"\n AND    referencia  = ? ",
                   "\n WHERE  referencia  = ? ",
                   "\n AND    tpo_estado  = 2";

      PREPARE ps_upd_ef FROM gr_sql

      DISPLAY "\n Parámetros que regresa el WebService Valida Pago "
      DISPLAY "\n Registros encontrados: ",ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item.getLength()

         {LET MT_ValidaPago_res.ValidaPagoResponseData[1].DOCU = "12345"
         LET MT_ValidaPago_res.ValidaPagoResponseData[1].EJER = "2016"
         LET MT_ValidaPago_res.ValidaPagoResponseData[1].ESTATUS = "27"
         LET MT_ValidaPago_res.ValidaPagoResponseData[1].MONTO = "215.10"
         LET MT_ValidaPago_res.ValidaPagoResponseData[1].WT_WITHCD = "WWWW" 
         LET MT_ValidaPago_res.ValidaPagoResponseData[1].REFPAGO = "PRUEBADE25CARACTERES12345"
         LET MT_ValidaPago_res.ValidaPagoResponseData[1].FECHPAGO = "01042016"}

      FOR i = 1 TO ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item.getLength()
         -- Se asignan los valores de entrada a las variables 
         LET v_arr_val_pag_res[i].v_documento  = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[i].DOCU
         LET v_arr_val_pag_res[i].v_ejercicio  = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[i].EJER
         LET v_arr_val_pag_res[i].v_estatus    = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[i].ESTATUS
         LET v_arr_val_pag_res[i].v_monto      = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[i].MONTO
         LET v_arr_val_pag_res[i].v_wt_withcd  = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[i].WT_WITHCD
         LET v_arr_val_pag_res[i].v_referencia = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[i].REFPAGO
         LET v_arr_val_pag_res[i].v_f_pago     = ZFICO_CONSULTAPAGOTABLET_Response.T_MESS_PAGO.item[i].FECHPAGO         
         
         DISPLAY "\n Documento: ", v_arr_val_pag_res[i].v_documento
         DISPLAY "Ejercicio : ", v_arr_val_pag_res[i].v_ejercicio
         DISPLAY "Estatus   : ", v_arr_val_pag_res[i].v_estatus
         DISPLAY "Monto     : ", v_arr_val_pag_res[i].v_monto
         DISPLAY "Wt_withcd : ", v_arr_val_pag_res[i].v_wt_withcd
         DISPLAY "Referencia: ", v_arr_val_pag_res[i].v_referencia
         DISPLAY "Fecha pago: ", v_arr_val_pag_res[i].v_f_pago,"\n"

         --IF v_arr_val_pag_res[i].v_referencia IS NOT NULL THEN
         --   LET p_referencia = v_arr_val_pag_res[i].v_referencia   
         --   LET p_referencia = p_referencia[1,13]
         --END IF

         DISPLAY "p_referencia:  ", p_referencia

         IF v_arr_val_pag_res[i].v_f_pago = '0000-00-00' THEN
            LET v_arr_val_pag_res[i].v_f_pago = ''
         ELSE
            LET v_f_pago = v_arr_val_pag_res[i].v_f_pago[6,7] ||"/"|| v_arr_val_pag_res[i].v_f_pago[9,10] ||"/"|| v_arr_val_pag_res[i].v_f_pago[1,4]
            LET v_arr_val_pag_res[i].v_f_pago = v_f_pago
         END IF
         
         EXECUTE ps_up_fac USING v_arr_val_pag_res[i].v_f_pago, 
                                 p_referencia
                                    
         IF SQLCA.SQLCODE < 0 THEN
            DISPLAY "2. Error al actualizar información en la Base de Datos."
            DISPLAY "\n v_arr_val_pag_res[i].v_f_pago: ",v_arr_val_pag_res[i].v_f_pago            
            DISPLAY "p_referencia: ",p_referencia            
            RETURN
         END IF 

         EXECUTE ps_sl_cnt USING v_arr_val_pag_res[i].v_documento,
                                 p_referencia
                            INTO v_existe_info

         IF v_existe_info = 0 THEN
            EXECUTE ps_ins_ef USING v_arr_val_pag_res[i].v_documento,
                                    p_referencia,
                                    v_arr_val_pag_res[i].v_estatus
         ELSE
            LET f_actual = TODAY
            
            EXECUTE ps_upd_ef USING v_arr_val_pag_res[i].v_estatus,
                                    f_actual,
                                    --v_arr_val_pag_res[i].v_documento, 
                                    p_referencia
         END IF

         IF SQLCA.SQLCODE < 0 THEN    	 	 
            DISPLAY "3. Error al insertar o actualizar la información en la Base de Datos."
            DISPLAY "\n v_existe_info                   : ",v_existe_info
            DISPLAY "\n v_arr_val_pag_res[i].v_documento: ",v_arr_val_pag_res[i].v_documento
            DISPLAY "\n p_referencia                    : ",p_referencia
            DISPLAY "\n v_arr_val_pag_res[i].v_estatus  : ",v_arr_val_pag_res[i].v_estatus            
            RETURN
         END IF
      END FOR
   END IF

   EXIT PROGRAM wsstatus  
END MAIN