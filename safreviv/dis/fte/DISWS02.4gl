IMPORT FGL WSHelper

DATABASE safre_viv

GLOBALS "SI_GeneraPoliza_SOService.inc"

MAIN
DEFINE wsstatus              INTEGER
DEFINE gr_sql                STRING

DEFINE p_referencia          CHAR(16)
DEFINE p_tpo_credito         SMALLINT
DEFINE p_cve_ent_financiera  SMALLINT
DEFINE p_clabe               CHAR(18)
DEFINE p_cta_contable        CHAR(10)
DEFINE p_banco_interlocutor  SMALLINT
DEFINE p_importe             DECIMAL(22,2)

DEFINE v_cliente             STRING,
       v_concepto            CHAR(02),
       v_delegacion          CHAR(02),
       v_cuenta_pro_acre     CHAR(10),
       v_monto               CHAR(13),
       v_num_cheque          STRING,
       v_tipo_banco          CHAR(04),
       v_tipo_doc            CHAR(01),
       v_banco_interloc      CHAR(04),
       v_clabe               CHAR(18)

DEFINE v_doc_contable        CHAR(10),
       v_ejercicio           CHAR(04),
       v_estatus             CHAR(02),
       v_referencia          CHAR(16)

DEFINE v_tpo_credito         SMALLINT

DEFINE s_concepto            STRING
DEFINE s_delegacion          STRING
DEFINE s_cuenta_pro_acre     STRING
DEFINE s_monto               STRING
DEFINE s_referencia          STRING
DEFINE s_banco_interloc      STRING
DEFINE s_tipo_doc            STRING

   -- Se asignan los parametros que vienen del fglrun
   LET p_tpo_credito        = ARG_VAL(1)
   LET p_referencia         = ARG_VAL(2)
   LET p_clabe              = ARG_VAL(3)
   LET p_cta_contable       = ARG_VAL(4)
   LET p_banco_interlocutor = ARG_VAL(5)
   LET p_importe            = ARG_VAL(6)

   -- Se asiganan las variables de default
   LET v_cliente    = '' 
   LET v_delegacion = '09' 
   LET v_num_cheque = '00000000' 
   LET v_tipo_doc   = 'T' 

   -- Se busca la información de la factura
   {LET gr_sql = "\n SELECT tpo_credito, ", 
                "\n        importe, ",
                "\n        cve_ent_financiera, ",
                "\n        banco_interlocutor  ",                
                "\n   FROM dis_ctr_factura_aps ", 
                "\n  WHERE referencia = ? ";   

   PREPARE ps_sl_fac FROM gr_sql
   EXECUTE ps_sl_fac INTO v_tpo_credito, 
                          v_monto,
                          v_tipo_banco,
                          v_banco_interloc
                    USING p_referencia}

   LET v_clabe          = p_clabe
   LET v_tpo_credito    = p_tpo_credito
   LET v_banco_interloc = p_banco_interlocutor
   LET v_monto          = p_importe
   
   -- Se busca la información de la factura
   {LET gr_sql = "\n SELECT UNIQUE clabe, ", 
                "\n        tpo_credito, ",
                "\n        banco_interlocutor, ",
                "\n        SUM(importe)  ",                
                "\n FROM   dis_ctr_factura_aps ", 
                "\n WHERE  referencia = ? ",
                "\n GROUP BY 1,2,3 ";   

   PREPARE ps_sl_fac FROM gr_sql
   EXECUTE ps_sl_fac INTO v_clabe,
                          v_tpo_credito, 
                          v_banco_interloc,
                          v_monto
                    USING p_referencia
               
   IF SQLCA.SQLCODE < 0 THEN    	 	 
      DISPLAY "1. Error al obtener información de la Base de Datos."
      DISPLAY "\n gr_sql      : ",gr_sql      
      DISPLAY "\n p_referencia: ",p_referencia
      RETURN
   END IF}

   IF v_tpo_credito = 2 THEN
      LET v_concepto = '06' -- Apoyo INFONAVIT
   END IF
   IF v_tpo_credito = 5 THEN 
      LET v_concepto = '07' -- Apoyo INFONAVIT (COFINAVIT)
   END IF
   IF v_tpo_credito = 3 THEN 
      LET v_concepto = '07' -- Uso de Garantía
   END IF

   LET v_cuenta_pro_acre = p_cta_contable
   
   -- Se busca la información de la cuenta contable
   {LET gr_sql = "\n SELECT UNIQUE cta_contable ",   
                "\n FROM   cat_cta_cnt_ocg ", 
                "\n WHERE  clabe       = ? ",  
                "\n AND    tpo_credito = ? ";

   PREPARE ps_sl_ef FROM gr_sql
   EXECUTE ps_sl_ef INTO v_cuenta_pro_acre
                    USING v_clabe, 
                          v_tpo_credito
                
   IF SQLCA.SQLCODE < 0 THEN    	 	 
      DISPLAY "2. Error al obtener información de la Base de Datos."
      DISPLAY "\n -",gr_sql,"-"
      DISPLAY "\n v_clabe      : -",v_clabe,"-"
      DISPLAY "\n v_tpo_credito: -",v_tpo_credito,"-"
      RETURN
   END IF}

   LET s_concepto        = v_concepto
   LET s_delegacion      = v_delegacion
   LET s_cuenta_pro_acre = v_cuenta_pro_acre
   LET s_monto           = v_monto
   LET s_referencia      = p_referencia
   LET s_banco_interloc  = v_banco_interloc USING "&&&&"
   LET s_tipo_doc        = v_tipo_doc

   LET s_concepto        = s_concepto.trim()
   LET s_delegacion      = s_delegacion.trim()
   LET s_cuenta_pro_acre = s_cuenta_pro_acre.trim()
   LET s_monto           = s_monto.trim()
   LET s_referencia      = s_referencia.trim()
   LET s_banco_interloc  = s_banco_interloc.trim()
   LET s_tipo_doc        = s_tipo_doc.trim()
 
   
   -- Se asignan los valores a las variables de salida
   LET MT_GeneraPoliza_req.cliente         = v_cliente 
   LET MT_GeneraPoliza_req.concepto        = s_concepto 
   LET MT_GeneraPoliza_req.delegacion      = s_delegacion 
   LET MT_GeneraPoliza_req.cuentaProAcre   = s_cuenta_pro_acre 
   LET MT_GeneraPoliza_req.monto           = s_monto 
   LET MT_GeneraPoliza_req.numCheque       = v_num_cheque 
   LET MT_GeneraPoliza_req.doctoReferencia = s_referencia 
   --LET MT_GeneraPoliza_req.tipoBanco       = v_tipo_banco USING "&&&&"
   LET MT_GeneraPoliza_req.tipoBanco       = s_banco_interloc
   LET MT_GeneraPoliza_req.tipoDoc         = s_tipo_doc


   {LET MT_GeneraPoliza_req.cliente = ''
   LET MT_GeneraPoliza_req.concepto = '06'
   LET MT_GeneraPoliza_req.delegacion = '09'
   LET MT_GeneraPoliza_req.cuentaProAcre = '2000146213'
   LET MT_GeneraPoliza_req.monto = '5351.96'
   LET MT_GeneraPoliza_req.numCheque = '00000000'
   LET MT_GeneraPoliza_req.doctoReferencia = 'AP08042016SQ8'
   LET MT_GeneraPoliza_req.tipoBanco = '908'  USING "&&&&"
   LET MT_GeneraPoliza_req.tipoDoc = 'T'}

   DISPLAY "\n Parámetros para la llamada al WebService Genera Poliza " 
   DISPLAY "Cliente       : ",MT_GeneraPoliza_req.cliente
   DISPLAY "Concepto      : ",MT_GeneraPoliza_req.concepto
   DISPLAY "Delegación    : ",MT_GeneraPoliza_req.delegacion 
   DISPLAY "Cuenta ProAcre: ",MT_GeneraPoliza_req.cuentaProAcre
   DISPLAY "Monto         : ",MT_GeneraPoliza_req.monto   
   DISPLAY "Cheque        : ",MT_GeneraPoliza_req.numCheque
   DISPLAY "Referencia    : ",MT_GeneraPoliza_req.doctoReferencia
   DISPLAY "Tipo Banco    : ",MT_GeneraPoliza_req.tipoBanco
   DISPLAY "Tipo Docto    : ",MT_GeneraPoliza_req.tipoDoc,"\n"
   
   LET wsstatus = SI_GeneraPoliza_SO_g()

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
      -- Se asignan los valores de entrada a las variables 
      LET v_doc_contable = MT_GeneraPoliza_res.doctoContable
      LET v_ejercicio    = MT_GeneraPoliza_res.ejercicio
      LET v_estatus      = MT_GeneraPoliza_res.estatus
      LET v_referencia   = MT_GeneraPoliza_res.referencia
      
      --LET v_referencia = v_referencia[1,13]

      DISPLAY "\n Parámetros que regresa el WebService Genera Poliza "
      DISPLAY "Doc contable: ", v_doc_contable
      DISPLAY "Ejercicio   : ", v_ejercicio
      DISPLAY "Estatus     : ", v_estatus
      DISPLAY "Referencia  : ", v_referencia,"\n"
            
      LET gr_sql = "\n UPDATE dis_ctr_factura_aps ",  
                   "\n SET    num_poliza         = ? ", 
                   "\n WHERE  referencia         = ? ",
                   "\n AND    tpo_credito        = ? ",
                   "\n AND    banco_interlocutor = ? ",
                   "\n AND    clabe              = ? ",
                   "\n AND    cta_contable       = ? ";

      PREPARE ps_up_fac FROM gr_sql
      IF v_referencia IS NULL THEN 
         EXECUTE ps_up_fac USING v_doc_contable, 
                                 v_referencia,
                                 p_tpo_credito,
                                 p_banco_interlocutor,
                                 p_clabe,
                                 p_cta_contable
      ELSE
         EXECUTE ps_up_fac USING v_doc_contable, 
                                 p_referencia,
                                 p_tpo_credito,
                                 p_banco_interlocutor,
                                 p_clabe,
                                 p_cta_contable
      END IF 
                
      IF SQLCA.SQLCODE < 0 THEN    	 	 
         DISPLAY "3. Error al actualizar información en la Base de Datos."
         DISPLAY "\n gr_sql:",gr_sql
         DISPLAY "\n v_doc_contable: ",v_doc_contable
         DISPLAY "\n v_referencia  : ",v_referencia
         RETURN
      END IF

      IF v_doc_contable IS NULL THEN
         LET v_doc_contable = 0
      END IF
      
      LET gr_sql = "\n INSERT INTO dis_ctr_edo_factura_aps ", 
                   "\n VALUES(seq_dis_edo_factura_aps.NEXTVAL,?,?,1,?,TODAY)"; 
      PREPARE ps_ins_ef FROM gr_sql
      
      IF v_referencia IS NULL THEN 
         EXECUTE ps_ins_ef USING v_doc_contable, 
                                 p_referencia, 
                                 v_estatus
      ELSE
         EXECUTE ps_ins_ef USING v_doc_contable, 
                                 v_referencia, 
                                 v_estatus
      END IF

       IF SQLCA.SQLCODE < 0 THEN    	 	 
         DISPLAY "4. Error al insertar información en la Base de Datos."
         DISPLAY "\n gr_sql: ",gr_sql
         DISPLAY "\n v_doc_contable: ",v_doc_contable
         DISPLAY "\n v_referencia  : ",v_referencia
         DISPLAY "\n v_estatus     : ",v_estatus
         RETURN
      END IF  
      
   END IF
  
   EXIT PROGRAM wsstatus   
END MAIN 