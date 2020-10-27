DATABASE safre_viv
GLOBALS
    DEFINE reg_1 RECORD
        nss                   CHAR(11)     ,
        id_derechohabiente    DECIMAL(9,0) ,
        estatus_ssv           DECIMAL(9,0)
    END RECORD;
    
    DEFINE reg_3 RECORD
       ruta_rescate           CHAR(40) ,
       ruta_envio             CHAR(40)
    END RECORD

    
    DEFINE
        v_archivo_entrada     CHAR(100) ,
        v_archivo_salida      CHAR(100) ,
        v_sql                 CHAR(200)
        
    DEFINE
        enter                 CHAR(1)  ,
        v_estado_marca_activa SMALLINT ,
        v_estado_solicitud    SMALLINT ,
        v_resultado           SMALLINT 
END GLOBALS

MAIN
    DEFINE v_estado           DECIMAL (4,0)
    DEFINE v_cod_rechazo      DECIMAL (4,0)
    DEFINE v_marca_activa     SMALLINT
    
    SELECT ruta_rescate ,
           ruta_envio
    INTO   reg_3.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
         
    WHENEVER ERROR CONTINUE
        DROP TABLE ret_marca_embargos;
    WHENEVER ERROR STOP 
     
    CREATE TABLE ret_marca_embargos
    (
     id_derechohabiente      DECIMAL (9,0),
     nss                     CHAR    (11) NOT NULL ,
     estatus_ssv             CHAR    (13) NOT NULL ,
     desc_ssv                CHAR    (40),
     estatus_jfca            CHAR    (14),
     desc_jfca               CHAR    (40),
     tpo_proceso             CHAR    (15),
     desc_tpo_proceso        CHAR    (40),
     nombre_afore            CHAR    (40),
     ap_pat_afore            CHAR    (40),
     ap_mat_afore            CHAR    (40),
     benefi_tit              CHAR    (26),
     nombre_benefi           CHAR    (40),
     ap_pat_benefi           CHAR    (40),
     ap_mat_benefi           CHAR    (40),
     curp                    CHAR    (20),
     rfc                     CHAR    (15),
     ent_fed                 CHAR    (18),
     f_ini_tramite           CHAR    (26),
     f_aut_pago              CHAR    (26),
     num_doc_cta_fico        CHAR    (25),
     eje_fisc_cta_fico       CHAR    (25),
     no_docto_pago_fico      CHAR    (26),
     f_pago_fico             CHAR    (15),
     imp_pago_fico           CHAR    (20),
     ref_pago_fico           CHAR    (23),
     caso_adai               CHAR    (19),
     num_laudo               CHAR    (15),
     num_junta_esp           CHAR    (24),
     imp_pago_ant            CHAR    (26),
     f_pago_ant              CHAR    (26),
     cve_banco               CHAR    (14),
     cta_bancaria            CHAR    (18),
     imp_transf_ssv          CHAR    (30),
     f_transf_ssv            CHAR    (28),
     ssv_dif                 CHAR    (29),
     f_marca_tj              CHAR    (21),
     ssv_error               CHAR    (30),
     ssv_cve_afore           CHAR    (15),
     estatus_marca_ant       CHAR    (23),
     aivs_ssv_97             CHAR    (24),
     imp_ssv_92              CHAR    (23),
     aivs_ssv_92             CHAR    (24),
     env_tesofe              CHAR    (29),
     pagado_tesofe           CHAR    (25),
     rec_env_tesofe          CHAR    (24),
     rec_pag_tesofe          CHAR    (24),
     estado_registro         DECIMAL (4,0),
     codigo_rechazo          DECIMAL (4,0)
    );
    LET v_archivo_entrada = reg_3.ruta_rescate CLIPPED,"/","REPORTE_201407231156162V2-CARGA.dat"
    --prompt "v_archivo_entrada",v_archivo_entrada for char enter REPORTE_201407231156162V2-CARGA
    LOAD FROM v_archivo_entrada INSERT INTO ret_marca_embargos (nss,
                                                                estatus_ssv,
                                                                desc_ssv,
                                                                estatus_jfca,
                                                                desc_jfca,
                                                                tpo_proceso,
                                                                desc_tpo_proceso,
                                                                nombre_afore,
                                                                ap_pat_afore,
                                                                ap_mat_afore,
                                                                benefi_tit,
                                                                nombre_benefi,
                                                                ap_pat_benefi,
                                                                ap_mat_benefi,
                                                                curp,
                                                                rfc,
                                                                ent_fed,
                                                                f_ini_tramite,
                                                                f_aut_pago,
                                                                num_doc_cta_fico,
                                                                eje_fisc_cta_fico,
                                                                no_docto_pago_fico,
                                                                f_pago_fico,
                                                                imp_pago_fico,
                                                                ref_pago_fico,
                                                                caso_adai,
                                                                num_laudo,
                                                                num_junta_esp,
                                                                imp_pago_ant,
                                                                f_pago_ant,
                                                                cve_banco,
                                                                cta_bancaria,
                                                                imp_transf_ssv,
                                                                f_transf_ssv,
                                                                ssv_dif,
                                                                f_marca_tj,
                                                                ssv_error,
                                                                ssv_cve_afore,
                                                                estatus_marca_ant,
                                                                aivs_ssv_97,
                                                                imp_ssv_92,
                                                                aivs_ssv_92,
                                                                env_tesofe,
                                                                pagado_tesofe,
                                                                rec_env_tesofe,
                                                                rec_pag_tesofe)

    DECLARE cur_1 CURSOR FOR
    SELECT A.nss ,
           B.id_derechohabiente,
           A.estatus_ssv  
    FROM   ret_marca_embargos A LEFT OUTER JOIN afi_derechohabiente B
                                             ON  A.nss = B.nss
    
    
    FOREACH cur_1 INTO reg_1.*
        IF reg_1.estatus_ssv = "0027" THEN 
            LET v_marca_activa = 593
        END IF 
        IF reg_1.estatus_ssv = "0028" THEN 
            LET v_marca_activa = 594
        END IF 
        IF reg_1.id_derechohabiente IS NOT NULL THEN 
            LET v_sql = "\nEXECUTE FUNCTION fn_marca_cuenta(","\n",reg_1.id_derechohabiente, ",",
                                                                 "\n",v_marca_activa,  ",",
                                                                 "\n 0,",
                                                                 "\n 0,",
                                                                 "\n 0,",
                                                                 "\n 0,",
                                                                 "\n 0,",
                                                                 "\n NULL,",
                                                                 "\n",'"SAFREVIV"',",",
                                                                 "\n 1515)"  --- Proceso de RETIROS FONDO DE AHORRO ARCH
            PREPARE v_marca FROM v_sql
            EXECUTE v_marca INTO v_resultado

            LET v_estado = 0
            LET v_cod_rechazo = 0       -- no hubo problemas con la marca
            
            IF v_resultado <> 0 THEN 
                LET v_estado = 100
                LET v_cod_rechazo   = v_resultado
            END IF 
        ELSE 
            LET v_estado = 100
            LET v_cod_rechazo = 999 -- no existe el trabajador
        END IF 
        IF reg_1.id_derechohabiente IS NOT NULL THEN 
            UPDATE ret_marca_embargos 
               SET estado_registro    = v_estado,
                   codigo_rechazo     = v_cod_rechazo,
                   id_derechohabiente = reg_1.id_derechohabiente
             WHERE nss = reg_1.nss
               AND estatus_ssv = reg_1.estatus_ssv
        ELSE 
            UPDATE ret_marca_embargos 
               SET estado_registro    = v_estado,
                   codigo_rechazo     = v_cod_rechazo
             WHERE nss = reg_1.nss
               AND estatus_ssv = reg_1.estatus_ssv
        END IF 
        
    END FOREACH
    
END MAIN