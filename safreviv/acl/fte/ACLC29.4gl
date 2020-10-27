--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLC29                                                                 #
#Objetivo     => consultas de aclaraciones aceptadas y no liquidadas                    #
#Fecha inicio => Agosto 16, 2012                                                        # 
#########################################################################################

DATABASE safre_viv

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
         ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
         ,p_s_titulo       STRING -- titulo de la ventana
END GLOBALS

MAIN
DEFINE v_tipo char(1)
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --se abre ventana para seleccionar la consulta deseada aceptadas o no liquidadas
   OPEN WINDOW w_consulta_acep WITH FORM "ACLC291"
   INPUT BY NAME v_tipo WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
      ON ACTION ACCEPT
         CALL fn_consultas(v_tipo)
      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_consulta_acep

END MAIN
{
======================================================================
Clave: ACLC29
Nombre: fn_consultas
Fecha creacion: Febrero 07, 2012
Autor: Eneas Adan Armas Osorio
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_consultas(p_tipo)
DEFINE p_tipo char(1)
,v_consulta STRING
,v_ruta_envio LIKE seg_modulo.ruta_envio

   SELECT ruta_envio
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'acl'

   IF p_tipo == "A" THEN
      --query de alcaraciones aceptadas:
      LET v_consulta = 
          "unload to '",v_ruta_envio CLIPPED, "/ext_acl_aceptados-",TODAY USING "ddmmyy" ,".txt' "
         ,"\nselect his.tpo_aclaracion, "
         ,"\n       his.cve_ent_receptora, "
         ,"\n       his.f_pago, "
         ,"\n       his.nrp, "
         ,"\n       his.periodo_pago, "
         ,"\n       his.folio_sua, "
         ,"\n       afi.nss, "
         ,"\n       his.imp_ap_pat, "
         ,"\n       his.aiv_ap_pat, "
         ,"\n       his.imp_am_cre, "
         ,"\n       his.int_gen_pgo_ext, "
         ,"\n       his.aiv_gen_pgo_ext, "
         ,"\n       folio "
         ,"\nfrom   cta_his_pagos his, "
         ,"\n       afi_derechohabiente afi "
         ,"\nwhere  origen_archivo not in (1,4)"
         ,"\nand    ind_liquidacion = 5 "
         ,"\nand    afi.id_derechohabiente = his.id_derechohabiente "
   ELSE
      --Query de Aclaraciones no liquidadas.
      LET v_consulta = 
          "unload to '",v_ruta_envio CLIPPED, "/ext_acl_no_liq-",TODAY USING "ddmmyy" ,".txt' "
         ,"\nselect his.tpo_aclaracion, "
         ,"\n       his.cve_ent_receptora, "
         ,"\n       his.f_pago, "
         ,"\n       his.nrp, "
         ,"\n       his.periodo_pago, "
         ,"\n       his.folio_sua, "
         ,"\n       afi.nss, "
         ,"\n       his.imp_ap_pat, "
         ,"\n       his.aiv_ap_pat, "
         ,"\n       his.imp_am_cre, "
         ,"\n       his.int_gen_pgo_ext, "
         ,"\n       his.aiv_gen_pgo_ext, "
         ,"\n       folio "
         ,"\nfrom   cta_his_pagos his, "
         ,"\n       afi_derechohabiente afi "
         ,"\nwhere  origen_archivo in (1,4) "
         ,"\nand    ind_liquidacion = 1 "
         ,"\nand    afi.id_derechohabiente = his.id_derechohabiente "

   END IF
DISPLAY v_consulta
   PREPARE consultaA FROM v_consulta
   EXECUTE consultaA
   
END FUNCTION
