################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 26/12/2013                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => Eneas Adan Armas Osorio E.F.P.                           #
--------------------------------------------------------------------------------
#Modulo            => ACL                                                      #
#Programa          => ACLS36                                                   #
#Objetivo          => Lanzado de consulta de aclaraciones aceptadas            #
#                     y no liquidadas                                          #
#Fecha inicio      => 12/11/2013                                               #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#
#
DATABASE safre_viv

GLOBALS 
   DEFINE
      g_tipo       char(1),
      v_modulo_cod           LIKE seg_modulo.modulo_cod

   DEFINE
      p_usuario            LIKE seg_usuario.usuario_cod,
      p_pid                DECIMAL (9,0),
      p_proceso_cod        LIKE cat_proceso.proceso_cod,
      p_opera_cod          LIKE cat_operacion.opera_cod

   DEFINE 
      v_archivo_copia      VARCHAR (25),
      v_comando_dos        STRING 
END GLOBALS 
     
MAIN
DEFINE r_bandera      SMALLINT 
      ,v_consulta STRING
      ,v_ruta_envio LIKE seg_modulo.ruta_envio
      ,v_ruta_bin LIKE seg_modulo.ruta_bin
      ,v_archivo_exe STRING
  
   --Asignación de parametros generales 
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2) 
   LET p_proceso_cod    = ARG_VAL(3) 
   LET p_opera_cod      = ARG_VAL(4)
   LET g_tipo           = ARG_VAL(5) -- Valor de argumento uno de DISL18 
   
   LET v_modulo_cod = "acl"
   

   SELECT ruta_envio,ruta_bin
   INTO v_ruta_envio,v_ruta_bin
   FROM seg_modulo
   WHERE modulo_cod = 'acl'

   LET v_archivo_exe = v_ruta_bin CLIPPED,"/ext_acl_",TODAY USING "ddmmyy" ,".sql "

   IF g_tipo == "A" THEN
      --query de alcaraciones aceptadas:
      LET v_consulta = 
          "echo \""
         ,"unload to '",v_ruta_envio CLIPPED, "/ext_acl_aceptados-",TODAY USING "ddmmyy" ,".txt' "
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
         ,"\" > ",v_archivo_exe
   ELSE
      --Query de Aclaraciones no liquidadas.
      LET v_consulta = 
          "echo \""
         ,"unload to '",v_ruta_envio CLIPPED, "/ext_acl_no_liq-",TODAY USING "ddmmyy" ,".txt' "
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
         ,"\" > ",v_archivo_exe

   END IF
   RUN v_consulta

   LET v_consulta = "$INFORMIXDIR/bin/dbaccess safre_viv ",v_archivo_exe
   RUN v_consulta
   
   --Finaliza la operación
   CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
   RETURNING r_bandera
  
   IF r_bandera = 0 THEN 
      DISPLAY "Se ha realizado la generación del archivo."
      EXIT PROGRAM 
   ELSE --Si ocurrió error 
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
      CALL fn_desplega_inc_operacion(r_bandera)
      EXIT PROGRAM 
   END IF
   
END MAIN
