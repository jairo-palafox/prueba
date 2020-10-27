--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION: 
--===============================================================

--------------------------------------------------------------------------------
-- Modulo       => TIA
-- Programa     => TIAC13
-- Objetivo     => Extractor del decreto
-- Fecha inicio => 3 de Julio de 2015
-- Autor        => GERARDO ALFONSO VEGA PAREDES
-------------------------------------------------------------------------------- 

DATABASE safre_viv

GLOBALS 
   DEFINE p_registros RECORD
   	  nss                 char(11),
      rfc                 char(13),     
      nci                 char(30),     
      tpo_entidad         char(2),      
      clave_icefa         char(3),      
      curp                char(18),     
      nombre_trab         char(120),    
      f_nacimiento        char(10),     
      clave_credito       char(1),      
      clave_retiro        char(1),      
      rfc_patron          char(13),     
      nss_patron          char(11),     
      nombre_patron       char(40),     
      exp_infonavit       char(9),      
      saldo_ini_imss      decimal(17,6),
      saldo_ini_infonavit decimal(17,2),
      saldo_act_imss      decimal(17,6),
      saldo_act_infonavit decimal(17,2),
      cve_criterio        char(2),      
      cve_mov_realizado   char(2),      
      cve_mov_actual      char(2),      
      f_ultimo_movto      char(10),     
      f_solic_movto       char(10),           
      f_cancel_movto      char(10),     
      monto_ap_imss       decimal(17,6),
      monto_ap_inf        decimal(17,2),
      monto_liq_imss      decimal(17,2),
      monto_liq_inf       decimal(17,2),
      monto_inc_ret_imss  decimal(17,2),
      monto_inc_viv_inf   decimal(17,2),
      consec_cuenta       char(11),     
      saldo_inf_pesos     decimal(17,2),
      saldo_inf_aivs      decimal(17,2),
      ind_consistencia    smallint,
      f_registro          date,
      f_liquida           date,
      ind_historico       CHAR(1)    
   END RECORD
   
   DEFINE g_ruta_envio     LIKE seg_modulo.ruta_envio,
          g_nom_archivo    VARCHAR(50),
          g_origen_archivo SMALLINT

  DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_tipo_ejecucion SMALLINT,                     -- forma como ejecutara el programa
          p_s_titulo       STRING  
          
END GLOBALS

MAIN

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_origen_archivo = 5
   
   -- consulta de informacion Reporte de Inconsistencias 
   CALL fn_extractor(p_usuario_cod, p_s_titulo)
   
END MAIN

FUNCTION fn_extractor(p_usuario_cod, p_s_titulo)

   DEFINE v_construct STRING,
          v_continua SMALLINT,
          manejador_rpt     om.SaxDocumentHandler
          
   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod, -- clave del usuario
          p_s_titulo                STRING,                       -- titulo de la ventana
          v_query                   STRING,
          v_registros               INTEGER

   DEFINE v_nomarch      VARCHAR(59),
          v_ruta_nomarch STRING,
          v_ruta_rescate LIKE seg_modulo.ruta_rescate,
          v_ruta_envio   LIKE seg_modulo.ruta_envio

   DEFINE v_acelera CHAR(20)
   
   DEFINE v_ch_arch_solTransf BASE.CHANNEL

   DEFINE v_detalle STRING
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN 
   --se abre la ventana de  forma de captura de parametros de busqueda
   
   OPEN WINDOW ventana_rep WITH FORM "TIAC131"

   LET v_continua = 1

WHILE v_continua
	
   CONSTRUCT v_construct ON cta.folio_liquida, cta.f_liquida
      FROM f_folio, f_registro
   
      ON ACTION CANCEL
         LET INT_FLAG = TRUE
         LET v_continua = 0
         EXIT CONSTRUCT
   
      ON ACTION ACCEPT
         LET v_continua = 1
         ACCEPT CONSTRUCT
   
   END CONSTRUCT

   IF NOT INT_FLAG THEN           

      LET v_acelera = "set pdqpriority high"
      PREPARE acelera FROM v_acelera
      EXECUTE acelera
   
      LET v_query = "\n SELECT COUNT(*) ",
                    "\n FROM cta_decreto cta, tia_det_traspaso det, ", --OUTER ",
                    "\n      cta_bd92_decreto bd, afi_decreto afi   ",
                    "\n WHERE ",v_construct,
                    "\n AND    cta.folio_liquida = det.folio         ",
                    "\n AND    det.id_referencia = cta.id_referencia ",                    
                    "\n AND    det.id_decreto    = afi.id_decreto    ",
                    "\n AND    afi.consec_cuenta = bd.consec_cuenta  "



      PREPARE prp_conteo FROM v_query
      EXECUTE prp_conteo INTO v_registros
      
      --valida que se econtrarón registros
      IF v_registros > 0 THEN
      
         -- se obtienen la ruta envio del modulo
         SELECT ruta_envio,
                ruta_rescate 
         INTO   v_ruta_envio,
                v_ruta_rescate
         FROM   seg_modulo
         WHERE  modulo_cod = 'tia'

         LET v_nomarch = "Extractor_decreto_",TODAY USING 'DD-MM-YYYY',".tia"
         LET v_ruta_nomarch = v_ruta_envio CLIPPED,"/"|| v_nomarch

         -- se crea el manejador de archivo
         LET v_ch_arch_solTransf = base.Channel.create()
   
         -- se crea archivo y se indica que se escribira en el mismo
         CALL v_ch_arch_solTransf.openFile(v_ruta_nomarch, "w" )
         CALL v_ch_arch_solTransf.setDelimiter("")

         CALL v_ch_arch_solTransf.write("NSS|"||"RFC|"||"NCI|"||"Tpo Entidad|"||"Cve Icefa|"||"CURP|"
                                                                            ||"Nombre Trabajador|"||"Fecha Nacimiento|"||"Cve Credito|"
                                                                            ||"Cve Retiro|"||"RFC Patron|"||"NSS Patron|"||"Nombre Patron|"
                                                                            ||"Exp Infonavit|"||"Saldo Ini IMSS| "||"Saldo Ini Infonavit|"||
                                                                            "Saldo Act IMSS|"||"Saldo Act Infonavit|"||"Cve Criterio|"||"Cve Mov Realizado|"||
                                                                            "Cve Mov Actual|"||"Fecha Ult Mov|"||"Fecha Solic Mov|"||"Fecha Cancel Mov|"
                                                                            ||"Monto Ap Imss|"||"Monto Ap Infonavit|"||"Monto Liq IMSS|"||"Monto Liq Infonavit|"
                                                                            ||"Monto Inc Ret IMSS|"||"Monto Inc Viv Infonavit|"||"Consecutivo Cuenta|"||"SDO Infonavit Pesos|"
                                                                            ||"Sdo Infonavit AIVS|"||"Ind Consistencia|"||"Fecha Registro|"||
                                                                            "Fecha Liquida|"||"Ind Historico")
         
            LET v_query = "\n SELECT bd.nss,                                ",        
                          "\n bd.rfc,                                       ",
                          "\n bd.nci,                                       ",
                          "\n bd.tpo_entidad,                               ",
                          "\n bd.clave_icefa,                               ",
                          "\n bd.curp,                                      ",
                          "\n bd.nombre_trab,                               ",
                          "\n bd.f_nacimiento,                              ",
                          "\n bd.clave_credito,                             ",
                          "\n bd.clave_retiro,                              ",
                          "\n bd.rfc_patron,                                ",
                          "\n bd.nss_patron,                                ",
                          "\n bd.nombre_patron,                             ",
                          "\n bd.exp_infonavit,                             ",
                          "\n bd.saldo_ini_imss,                            ",
                          "\n bd.saldo_ini_infonavit,                       ",
                          "\n bd.saldo_act_imss,                            ",
                          "\n bd.saldo_act_infonavit,                       ",
                          "\n bd.cve_criterio,                              ",
                          "\n CASE bd.cve_mov_realizado                     ", 
                          "\n   WHEN '01' THEN                              ",
                          "\n     'HI'                                      ",
                          "\n   WHEN '02' THEN                              ",
                          "\n     'HI'                                      ",
                          "\n   ELSE bd.cve_mov_realizado                   ",
                          "\n END,                                          ",
                          "\n bd.cve_mov_actual,                            ",
                          "\n bd.f_ultimo_movto,                            ",
                          "\n bd.f_solic_movto,                             ",
                          "\n bd.f_cancel_movto,                            ",
                          "\n bd.monto_ap_imss,                             ",
                          "\n bd.monto_ap_inf,                              ",
                          "\n bd.monto_liq_imss,                            ",
                          "\n bd.monto_liq_inf,                             ",
                          "\n bd.monto_inc_ret_imss,                        ",
                          "\n bd.monto_inc_viv_inf,                         ",
                          "\n bd.consec_cuenta,                             ",
                          "\n bd.saldo_inf_pesos,                           ",
                          "\n bd.saldo_inf_aivs,                            ",
                          "\n bd.ind_consistencia,                          ",
                          "\n cta.f_registro,                               ",   
                          "\n cta.f_liquida                                 ",    
                          "\n FROM cta_decreto cta, tia_det_traspaso det,   ", --OUTER ",
                          "\n      cta_bd92_decreto bd, afi_decreto afi   ",
                          "\n WHERE ",v_construct,
                          "\n AND    cta.folio_liquida = det.folio         ",
                          "\n AND    det.id_referencia = cta.id_referencia ",                    
                          "\n AND    det.id_decreto    = afi.id_decreto    ",
                          "\n AND    afi.consec_cuenta = bd.consec_cuenta  "

--                          "\n FROM cta_bd92_decreto bd,                     ",
--                          "\n      afi_decreto afi, OUTER                   ",
--                          "\n      (tia_det_traspaso det,                   ",
--                          "\n      cta_decreto cta)                         ",
--                          "\n WHERE ",v_construct, 
--                          "\n AND    afi.consec_cuenta = bd.consec_cuenta   ",
--                          "\n AND    det.id_decreto    = afi.id_decreto     ",
--                          "\n AND    cta.folio_liquida = det.folio          ",
--                          "\n AND    det.id_referencia = cta.id_referencia  "

                         
            PREPARE prp_cur_folio FROM v_query

            DECLARE cur_folio CURSOR FOR prp_cur_folio
           
            FOREACH cur_folio INTO p_registros.*
               LET v_detalle = p_registros.nss,"|",                
                               p_registros.rfc,"|",                 
                               p_registros.nci,"|",                 
                               p_registros.tpo_entidad,"|",         
                               p_registros.clave_icefa,"|",         
                               p_registros.curp,"|",                
                               p_registros.nombre_trab,"|",         
                               p_registros.f_nacimiento, "|",       
                               p_registros.clave_credito,"|",       
                               p_registros.clave_retiro,"|",        
                               p_registros.rfc_patron,"|",          
                               p_registros.nss_patron,"|",          
                               p_registros.nombre_patron,"|",       
                               p_registros.exp_infonavit, "|",      
                               p_registros.saldo_ini_imss,"|",      
                               p_registros.saldo_ini_infonavit,"|", 
                               p_registros.saldo_act_imss, "|",     
                               p_registros.saldo_act_infonavit,"|", 
                               p_registros.cve_criterio,"|",        
                               p_registros.cve_mov_realizado, "|",  
                               p_registros.cve_mov_actual,"|",      
                               p_registros.f_ultimo_movto, "|",     
                               p_registros.f_solic_movto,"|",       
                               p_registros.f_cancel_movto, "|",     
                               p_registros.monto_ap_imss, "|",      
                               p_registros.monto_ap_inf,"|",        
                               p_registros.monto_liq_imss,"|",      
                               p_registros.monto_liq_inf, "|",      
                               p_registros.monto_inc_ret_imss, "|", 
                               p_registros.monto_inc_viv_inf, "|",  
                               p_registros.consec_cuenta, "|",      
                               p_registros.saldo_inf_pesos, "|",    
                               p_registros.saldo_inf_aivs,"|",      
                               p_registros.ind_consistencia,"|",    
                               p_registros.f_registro, "|",         
                               p_registros.f_liquida, "|",          
                               p_registros.ind_historico      
            
               CALL v_ch_arch_solTransf.writeLine([v_detalle])

            END FOREACH

            CALL fn_mensaje("Consulta","Archivo generado, revisar en ruta envio","about")
      ELSE
         CALL fn_mensaje("Consulta","No existen registros con los criterios dados.","about")   
      END IF --IF v_registros > 0 THEN

   END IF  --IF NOT INT_FLAG THEN

   IF v_continua = 0 THEN
   	  EXIT WHILE
   END IF

END WHILE

{   IF v_continua = 1 THEN
      MENU 
         ON ACTION CANCEL --EXIT --CLOSE
            EXIT MENU
      END MENU
   END IF
} 
   
   CLOSE WINDOW ventana_rep

END FUNCTION

{
REPORT rpt_aclsc_inconsistenca(p_registros)
   DEFINE p_registros RECORD
   	  nss                 char(11),
      rfc                 char(13),     
      nci                 char(30),     
      tpo_entidad         char(2),      
      clave_icefa         char(3),      
      curp                char(18),     
      nombre_trab         char(120),    
      f_nacimiento        char(10),     
      clave_credito       char(1),      
      clave_retiro        char(1),      
      rfc_patron          char(13),     
      nss_patron          char(11),     
      nombre_patron       char(40),     
      exp_infonavit       char(9),      
      saldo_ini_imss      decimal(17,6),
      saldo_ini_infonavit decimal(17,2),
      saldo_act_imss      decimal(17,6),
      saldo_act_infonavit decimal(17,2),
      cve_criterio        char(2),      
      cve_mov_realizado   char(2),      
      cve_mov_actual      char(2),      
      f_ultimo_movto      char(10),     
      f_solic_movto       char(10),           
      f_cancel_movto      char(10),     
      monto_ap_imss       decimal(17,6),
      monto_ap_inf        decimal(17,2),
      monto_liq_imss      decimal(17,2),
      monto_liq_inf       decimal(17,2),
      monto_inc_ret_imss  decimal(17,2),
      monto_inc_viv_inf   decimal(17,2),
      consec_cuenta       char(11),     
      saldo_inf_pesos     decimal(17,2),
      saldo_inf_aivs      decimal(17,2),
      ind_consistencia    smallint,
      f_registro          date,
      f_liquida           date,
      ind_historico       CHAR(1)    
   END RECORD

   DEFINE v_fecha DATE
   DEFINE v_nombre_usuario VARCHAR(100)
                                                                                                                                                                                          
   FORMAT                                                                                        
      FIRST PAGE HEADER
         LET v_fecha = TODAY USING "dd-mm-yyyy" 

         SELECT usuario_desc
         INTO v_nombre_usuario
         FROM seg_usuario
         WHERE usuario_cod = p_usuario_cod

         LET v_nombre_usuario = v_nombre_usuario CLIPPED

         PRINTX v_fecha, v_nombre_usuario, p_usuario_cod

      ON EVERY ROW
         PRINTX p_registros.*
                                                                                           
END REPORT          
}