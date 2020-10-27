##########################################################################
#Modulo            => OCG                                                #
#Programa          => OCGP25                                             #
#Objetivo          => Programa para ambientación de pruebas 43 bis       #
#Autor             => Jose Eduardo Ventura , EFP                         #
#Fecha inicio      => 08/Agosto/2017                                      #
##########################################################################

DATABASE safre_viv

   DEFINE g_usuario         CHAR(20)
   DEFINE g_tipo_proceso    SMALLINT
   DEFINE g_nom_ventana     STRING
   DEFINE v_nss             CHAR(11)
   DEFINE v_s_qry           STRING
   DEFINE v_f_envio         DATE
   DEFINE v_f_envio_t       CHAR(8)
   DEFINE v_f_liq_t         CHAR(8)
   DEFINE v_f_dep_t         CHAR(8)

   DEFINE v_nss1            CHAR(11)
   DEFINE v_nss2            CHAR(11)
   DEFINE v_nss3            CHAR(11)
   DEFINE v_nss4            CHAR(11)
   DEFINE v_nss5            CHAR(11)

   DEFINE v_cv_ef   smallint    ;
   DEFINE v_tc      char(1)     ;
   DEFINE v_nci     char(18)    ;
   DEFINE v_bim     char(6)     ;
   DEFINE v_imp_ap  CHAR(9);
   DEFINE v_f_liq   date        ;
   DEFINE v_imp_gtia CHAR(9);
   DEFINE v_causa   smallint    ;
   DEFINE v_f_dep   date        ;
   DEFINE v_sol     smallint    ;
   
   
   DEFINE v_cv_ef1     smallint    ;
   DEFINE v_tc1        char(1)     ;
   DEFINE v_nci1       char(18)    ;
   DEFINE v_bim1       char(6)     ;
   DEFINE v_imp_ap1    CHAR(9);
   DEFINE v_f_liq1     date        ;
   DEFINE v_imp_gtia1  CHAR(9);
   DEFINE v_causa1     smallint    ;
   DEFINE v_f_dep1     date        ;
   DEFINE v_sol1       smallint    ;
                                   
   DEFINE v_cv_ef2     smallint    ;
   DEFINE v_tc2        char(1)     ;
   DEFINE v_nci2       char(18)    ;
   DEFINE v_bim2       char(6)     ;
   DEFINE v_imp_ap2    CHAR(9);
   DEFINE v_f_liq2     date        ;
   DEFINE v_imp_gtia2  CHAR(9);
   DEFINE v_causa2     smallint    ;
   DEFINE v_f_dep2     date        ;
   DEFINE v_sol2       smallint    ;
                                   
   DEFINE v_cv_ef3     smallint    ;
   DEFINE v_tc3        char(1)     ;
   DEFINE v_nci3       char(18)    ;
   DEFINE v_bim3       char(6)     ;
   DEFINE v_imp_ap3    CHAR(9);
   DEFINE v_f_liq3     date        ;
   DEFINE v_imp_gtia3  CHAR(9);
   DEFINE v_causa3     smallint    ;
   DEFINE v_f_dep3     date        ;
   DEFINE v_sol3       smallint    ;
                                   
   DEFINE v_cv_ef4     smallint    ;
   DEFINE v_tc4        char(1)     ;
   DEFINE v_nci4       char(18)    ;
   DEFINE v_bim4       char(6)     ;
   DEFINE v_imp_ap4    CHAR(9);
   DEFINE v_f_liq4     date        ;
   DEFINE v_imp_gtia4  CHAR(9);
   DEFINE v_causa4     smallint    ;
   DEFINE v_f_dep4     date        ;
   DEFINE v_sol4       smallint    ;
                                   
   DEFINE v_cv_ef5     smallint    ;
   DEFINE v_tc5        char(1)     ;
   DEFINE v_nci5       char(18)    ;
   DEFINE v_bim5       char(6)     ;
   DEFINE v_imp_ap5    CHAR(9);
   DEFINE v_f_liq5     date        ;
   DEFINE v_imp_gtia5  CHAR(9);
   DEFINE v_causa5     smallint    ;
   DEFINE v_f_dep5     date        ;
   DEFINE v_sol5       smallint    ;


   DEFINE arr_sp001 DYNAMIC ARRAY OF RECORD
      v_nss         CHAR(11),
      incons_1      SMALLINT,
      incons_2      SMALLINT,
      incons_3      SMALLINT,
      incons_4      SMALLINT,
      incons_5      SMALLINT
   END RECORD

   DEFINE v_qry_sp001       STRING
   DEFINE a                 SMALLINT

MAIN
   -- Se reciben parámetros eviados por el menú
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_ventana  = ARG_VAL(3)

   CALL STARTLOG(g_usuario CLIPPED|| ".OCGP25.log")

   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF


   --CLOSE WINDOW SCREEN
   MENU

      ON ACTION SP001
      ON ACTION SP002
      ON ACTION SP003
      ON ACTION SP005
         CALL fn_ambienta_sp005()
   EXIT MENU
   END MENU

   --CALL fn_ambienta_sp001() 
   
END MAIN

FUNCTION fn_ambienta_sp001()

OPEN WINDOW OCGP251 WITH FORM "OCGP251"

   INPUT ARRAY arr_sp001 FROM tab_sp001.* ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS,
                                                                 APPEND ROW = FALSE,
                                                                 DELETE ROW = FALSE,
                                                                 INSERT ROW = FALSE)

   BEFORE INPUT

      LET v_qry_sp001 = "SELECT FIRST 5(nss)
                           FROM afi_derechohabiente"

      PREPARE prp_sp001 FROM v_qry_sp001
      DECLARE cur_sp001 CURSOR FOR prp_sp001

      LET a = 1

      FOREACH cur_sp001 INTO arr_sp001[a].v_nss

         LET a = a+1
      END FOREACH

      ON ACTION ACCEPT
         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT

   END INPUT
CLOSE WINDOW OCGP251
END FUNCTION

FUNCTION fn_ambienta_sp005()

   DEFINE arr_sp005 DYNAMIC ARRAY OF RECORD
      v_nss         CHAR(11),
      incons_1      SMALLINT,
      incons_2      SMALLINT,
      incons_3      SMALLINT,
      incons_4      SMALLINT,
      incons_5      SMALLINT
   END RECORD

   DEFINE arr_id_detalle DYNAMIC ARRAY OF RECORD
      v_id_detalle  DECIMAL (9,0)
   END RECORD

   DEFINE v_qry_sp005       STRING
   DEFINE a                 SMALLINT
   DEFINE v_bnd_01          SMALLINT
   DEFINE v_bnd_02          SMALLINT
   DEFINE v_bnd_26          SMALLINT
   DEFINE v_bnd_42          SMALLINT
   DEFINE v_bnd_43          SMALLINT
   DEFINE v_bnd_46          SMALLINT
   DEFINE v_bnd_51          SMALLINT
   DEFINE v_bnd_53          SMALLINT
   DEFINE v_cve_ent         SMALLINT

   DEFINE v_sp_error        SMALLINT
   DEFINE v_cant_aceptados  INTEGER
   DEFINE v_cant_rechazados INTEGER
   DEFINE v_s_msj           STRING

   LET v_qry_sp005 = "SELECT FIRST 5(nss),id_ocg_detalle
                           FROM ocg_detalle
                          WHERE id_ocg_detalle in (
                         SELECT id_ocg_detalle
                           FROM ocg_formalizacion
                          WHERE situacion = 80
                            AND diagnostico = 1)
                            AND subproceso = 2"

   PREPARE prp_sp005 FROM v_qry_sp005
   DECLARE cur_sp005 CURSOR FOR prp_sp005

   LET a = 1

   FOREACH cur_sp005 INTO arr_sp005[a].v_nss,arr_id_detalle[a].v_id_detalle

      LET a = a+1
   END FOREACH

   CALL arr_sp005.deleteElement(a)

   IF arr_sp005[1].v_nss IS NULL THEN
      LET v_s_msj = "No existen registros para liquidar"
      CALL fn_mensaje("Alerta",v_s_msj,"stop") 
   ELSE

      LET v_nss1 = arr_sp005[1].v_nss
      LET v_nss2 = arr_sp005[2].v_nss
      LET v_nss3 = arr_sp005[3].v_nss
      LET v_nss4 = arr_sp005[4].v_nss
      LET v_nss5 = arr_sp005[5].v_nss

   OPEN WINDOW OCGP254 WITH FORM "OCGP254"

      DISPLAY BY NAME v_nss1,
                      v_nss2,
                      v_nss3,
                      v_nss4,
                      v_nss5

   INPUT BY NAME v_cv_ef1   ,
                 v_tc1      ,
                 v_nci1     ,
                 v_bim1     ,
                 v_imp_ap1  ,
                 v_f_liq1   ,
                 v_imp_gtia1,
                 v_causa1   ,
                 v_f_dep1   ,
                 v_sol1     ,
                 v_cv_ef2   ,
                 v_tc2      ,
                 v_nci2     ,
                 v_bim2     ,
                 v_imp_ap2  ,
                 v_f_liq2   ,
                 v_imp_gtia2,
                 v_causa2   ,
                 v_f_dep2   ,
                 v_sol2     ,
                 v_cv_ef3   ,
                 v_tc3      ,
                 v_nci3     ,
                 v_bim3     ,
                 v_imp_ap3  ,
                 v_f_liq3   ,
                 v_imp_gtia3,
                 v_causa3   ,
                 v_f_dep3   ,
                 v_sol3     ,
                 v_cv_ef4   ,
                 v_tc4      ,
                 v_nci4     ,
                 v_bim4     ,
                 v_imp_ap4  ,
                 v_f_liq4   ,
                 v_imp_gtia4,
                 v_causa4   ,
                 v_f_dep4   ,
                 v_sol4     ,
                 v_cv_ef5   ,
                 v_tc5      ,
                 v_nci5     ,
                 v_bim5     ,
                 v_imp_ap5  ,
                 v_f_liq5   ,
                 v_imp_gtia5,
                 v_causa5   ,
                 v_f_dep5   ,
                 v_sol5     ATTRIBUTES(UNBUFFERED)--,CANCEL = FALSE)

      --BEFORE INPUT

      ON ACTION ACCEPT

         CALL fn_borra_tmp()
         CALL fn_borra_arch()
         CALL fn_crea_tmp()

         INPUT ARRAY arr_sp005 FROM tab_sp005.* ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS,
                                                                 APPEND ROW = FALSE,
                                                                 DELETE ROW = FALSE,
                                                                 INSERT ROW = FALSE)

      ON ACTION ACCEPT
         FOR a= 1 TO arr_sp005.getLength()

            LET v_cv_ef    = 0
            LET v_tc       = 0
            LET v_nci      = 0
            LET v_bim      = 0
            LET v_imp_ap   = 0
            LET v_f_liq    = 0
            LET v_imp_gtia = 0
            LET v_causa    = 0
            LET v_f_dep    = 0
            LET v_sol      = 0

            CASE
               WHEN a = 1
                  LET v_nss      = v_nss1
                  LET v_cv_ef    = v_cv_ef1   
                  LET v_tc       = v_tc1      
                  LET v_nci      = v_nci1     
                  LET v_bim      = v_bim1     
                  LET v_imp_ap   = v_imp_ap1  
                  LET v_f_liq    = v_f_liq1     
                  LET v_imp_gtia = v_imp_gtia1
                  LET v_causa    = v_causa1   
                  LET v_f_dep    = v_f_dep1   
                  LET v_sol      = v_sol1     

               WHEN a = 2
                  LET v_cv_ef    = v_cv_ef2    
                  LET v_tc       = v_tc2       
                  LET v_nci      = v_nci2      
                  LET v_bim      = v_bim2      
                  LET v_imp_ap   = v_imp_ap2   
                  LET v_f_liq    = v_f_liq2      
                  LET v_imp_gtia = v_imp_gtia2 
                  LET v_causa    = v_causa2    
                  LET v_f_dep    = v_f_dep2    
                  LET v_sol      = v_sol2      

               WHEN a = 3
                  LET v_cv_ef    = v_cv_ef3    
                  LET v_tc       = v_tc3       
                  LET v_nci      = v_nci3      
                  LET v_bim      = v_bim3      
                  LET v_imp_ap   = v_imp_ap3   
                  LET v_f_liq    = v_f_liq3      
                  LET v_imp_gtia = v_imp_gtia3 
                  LET v_causa    = v_causa3    
                  LET v_f_dep    = v_f_dep3    
                  LET v_sol      = v_sol3      

               WHEN  a = 4
                  LET v_cv_ef    =  v_cv_ef4   
                  LET v_tc       =  v_tc4      
                  LET v_nci      =  v_nci4     
                  LET v_bim      =  v_bim4     
                  LET v_imp_ap   =  v_imp_ap4  
                  LET v_f_liq    =  v_f_liq4     
                  LET v_imp_gtia =  v_imp_gtia4
                  LET v_causa    =  v_causa4   
                  LET v_f_dep    =  v_f_dep4   
                  LET v_sol      =  v_sol4     

               WHEN a = 5
                  LET v_cv_ef    = v_cv_ef5   
                  LET v_tc       = v_tc5      
                  LET v_nci      = v_nci5     
                  LET v_bim      = v_bim5     
                  LET v_imp_ap   = v_imp_ap5  
                  LET v_f_liq    = v_f_liq5     
                  LET v_imp_gtia = v_imp_gtia5
                  LET v_causa    = v_causa5   
                  LET v_f_dep    = v_f_dep5   
                  LET v_sol      = v_sol5     

            END CASE

            LET v_bnd_01 = 0
            LET v_bnd_02 = 0
            LET v_bnd_26 = 0

            CASE arr_sp005[a].incons_1

               WHEN "01"
                  LET v_bnd_01 = 1

               WHEN "02"
                  LET v_bnd_02 = 1

               WHEN "26"
                  LET v_bnd_26 = 1

               WHEN "28"
               WHEN "43"
                  LET v_bnd_43 = 1

               WHEN "46"
                  LET v_bnd_46 = 1

               WHEN "51"
                  LET v_bnd_51 = 1

               WHEN "53"
                  LET v_bnd_53 = 1

               WHEN "54"
               WHEN "58"
               WHEN "60"
               WHEN "61"

               OTHERWISE
               
            END CASE

            CALL fn_incons_01(arr_sp005[a].v_nss,
                              v_bnd_01,
                              v_cv_ef,
                              v_tc,
                              v_nci,
                              v_bim,
                              v_imp_ap,
                              v_f_liq,
                              v_imp_gtia,
                              v_causa,
                              v_f_dep,
                              v_sol ) RETURNING v_nss,
                                                v_cv_ef,
                                                v_tc,
                                                v_nci,
                                                v_bim,
                                                v_imp_ap,
                                                v_f_liq,
                                                v_imp_gtia,
                                                v_causa,
                                                v_f_dep,
                                                v_sol,
                                                v_f_envio
            DISPLAY "datos salida 01 : ",v_nss,",",v_cv_ef,",",v_tc,",",
                    v_nci,",",v_bim,",",v_imp_ap,",",v_f_liq,",",
                    v_imp_gtia,",",v_causa,",",v_f_dep,",",v_sol,",",v_f_envio

            CALL fn_incons_02(arr_sp005[a].v_nss,
                              v_bnd_02,
                              v_cv_ef,
                              v_tc,
                              v_nci,
                              v_bim,
                              v_imp_ap,
                              v_f_liq,
                              v_imp_gtia,
                              v_causa,
                              v_f_dep,
                              v_sol) RETURNING v_nss,
                                                  v_cv_ef,
                                                  v_tc,
                                                  v_nci,
                                                  v_bim,
                                                  v_imp_ap,
                                                  v_f_liq,
                                                  v_imp_gtia,
                                                  v_causa,
                                                  v_f_dep,
                                                  v_sol,
                                                  v_f_envio

            DISPLAY "datos salida 02 : ",v_nss,",",v_cv_ef,",",v_tc,",",
                    v_nci,",",v_bim,",",v_imp_ap,",",v_f_liq,",",
                    v_imp_gtia,",",v_causa,",",v_f_dep,",",v_sol,",",v_f_envio

            CALL fn_incons_26(arr_sp005[a].v_nss,
                              v_bnd_26,
                              v_cv_ef,
                              v_tc,
                              v_nci,
                              v_bim,
                              v_imp_ap,
                              v_f_liq,
                              v_imp_gtia,
                              v_causa,
                              v_f_dep,
                              v_sol ) RETURNING v_nss,
                                                v_cv_ef,
                                                v_tc,
                                                v_nci,
                                                v_bim,
                                                v_imp_ap,
                                                v_f_liq,
                                                v_imp_gtia,
                                                v_causa,
                                                v_f_dep,
                                                v_sol,
                                                v_f_envio

            DISPLAY "datos salida 26 : ",v_nss,",",v_cv_ef,",",v_tc,",",
                    v_nci,",",v_bim,",",v_imp_ap,",",v_f_liq,",",
                    v_imp_gtia,",",v_causa,",",v_f_dep,",",v_sol,",",v_f_envio

            CALL fn_incons_43(arr_sp005[a].v_nss,
                              v_bnd_43,
                              v_cv_ef,
                              v_tc,
                              v_nci,
                              v_bim,
                              v_imp_ap,
                              v_f_liq,
                              v_imp_gtia,
                              v_causa,
                              v_f_dep,
                              v_sol ) RETURNING v_nss,
                                                v_cv_ef,
                                                v_tc,
                                                v_nci,
                                                v_bim,
                                                v_imp_ap,
                                                v_f_liq,
                                                v_imp_gtia,
                                                v_causa,
                                                v_f_dep,
                                                v_sol,
                                                v_f_envio

            DISPLAY "datos salida 43 : ",v_nss,",",v_cv_ef,",",v_tc,",",
                    v_nci,",",v_bim,",",v_imp_ap,",",v_f_liq,",",
                    v_imp_gtia,",",v_causa,",",v_f_dep,",",v_sol,",",v_f_envio

            CALL fn_incons_46(arr_sp005[a].v_nss,
                              v_bnd_46,
                              v_cv_ef,
                              v_tc,
                              v_nci,
                              v_bim,
                              v_imp_ap,
                              v_f_liq,
                              v_imp_gtia,
                              v_causa,
                              v_f_dep,
                              v_sol ) RETURNING v_nss,
                                                v_cv_ef,
                                                v_tc,
                                                v_nci,
                                                v_bim,
                                                v_imp_ap,
                                                v_f_liq,
                                                v_imp_gtia,
                                                v_causa,
                                                v_f_dep,
                                                v_sol,
                                                v_f_envio

            DISPLAY "datos salida 46 : ",v_nss,",",v_cv_ef,",",v_tc,",",
                    v_nci,",",v_bim,",",v_imp_ap,",",v_f_liq,",",
                    v_imp_gtia,",",v_causa,",",v_f_dep,",",v_sol,",",v_f_envio

            CALL fn_incons_51(arr_sp005[a].v_nss,
                              v_bnd_51,
                              v_cv_ef,
                              v_tc,
                              v_nci,
                              v_bim,
                              v_imp_ap,
                              v_f_liq,
                              v_imp_gtia,
                              v_causa,
                              v_f_dep,
                              v_sol ) RETURNING v_nss,
                                                v_cv_ef,
                                                v_tc,
                                                v_nci,
                                                v_bim,
                                                v_imp_ap,
                                                v_f_liq,
                                                v_imp_gtia,
                                                v_causa,
                                                v_f_dep,
                                                v_sol,
                                                v_f_envio

            DISPLAY "datos salida 51 : ",v_nss,",",v_cv_ef,",",v_tc,",",
                    v_nci,",",v_bim,",",v_imp_ap,",",v_f_liq,",",
                    v_imp_gtia,",",v_causa,",",v_f_dep,",",v_sol,",",v_f_envio

            CALL fn_incons_53(arr_sp005[a].v_nss,
                              v_bnd_53,
                              v_cv_ef,
                              v_tc,
                              v_nci,
                              v_bim,
                              v_imp_ap,
                              v_f_liq,
                              v_imp_gtia,
                              v_causa,
                              v_f_dep,
                              v_sol ) RETURNING v_nss,
                                                v_cv_ef,
                                                v_tc,
                                                v_nci,
                                                v_bim,
                                                v_imp_ap,
                                                v_f_liq,
                                                v_imp_gtia,
                                                v_causa,
                                                v_f_dep,
                                                v_sol,
                                                v_f_envio

            DISPLAY "datos salida 53 : ",v_nss,",",v_cv_ef,",",v_tc,",",
                    v_nci,",",v_bim,",",v_imp_ap,",",v_f_liq,",",
                    v_imp_gtia,",",v_causa,",",v_f_dep,",",v_sol,",",v_f_envio

            LET v_f_envio_t = v_f_envio USING "yyyymmdd"
            LET v_f_dep_t   = v_f_dep   USING  "yyyymmdd"
            LET v_f_liq_t   = v_f_liq   USING  "yyyymmdd"

            INSERT INTO safre_tmp:tmp_rec_det_ocg43(tpo_registro,
                                                    subproceso,
                                                    nss,
                                                    tpo_envio,
                                                    f_envio,
                                                    cve_ent_financiera,
                                                    bim_apor_subsec,
                                                    imp_subsec_devuelto,
                                                    causa_liquidacion,
                                                    f_deposito,
                                                    cred_convenidos,
                                                    f_libera_garantia,
                                                    imp_ocg_devuelto)
                                             VALUES("2",
                                                    "005",
                                                    v_nss,
                                                    "E",
                                                    v_f_envio_t,
                                                    v_cv_ef,
                                                    v_bim,
                                                    v_imp_ap,
                                                    v_causa,
                                                    v_f_dep_t,
                                                    v_tc,
                                                    v_f_liq_t,
                                                    v_imp_gtia)
         END FOR

            EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT

      END INPUT

{
      -- Se ejecuta la función que realiza el proceso
         DISPLAY "Se ejecuta el proceso"
         LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_sp5_liquidacion(0)"

         PREPARE prp_exe_fn_sp005 FROM v_s_qry
         EXECUTE prp_exe_fn_sp005 INTO v_sp_error,
                                       v_cant_aceptados,
                                       v_cant_rechazados

         DISPLAY "Respuesta del proceso : ",v_sp_error
         DISPLAY "SQLCA : ",sqlca.sqlcode
}
        LET sqlca.sqlcode = 0  -- provicional
        LET v_sp_error    = 0  -- provicional

        IF sqlca.sqlcode = 0 AND v_sp_error >= 0 THEN 
            CALL fn_mensaje("Alerta","El registro se proceso correctamente","stop")
         ELSE 
            LET v_s_msj = "Error "|| v_sp_error || " al procesar el registro"
            CALL fn_mensaje("Alerta",v_s_msj,"stop") 
         END IF

         EXIT INPUT
      END INPUT
   CLOSE WINDOW OCGP254

   END IF

END FUNCTION


FUNCTION fn_borra_tmp()
   LET v_s_qry = "DELETE
                    FROM safre_tmp:tmp_rec_det_ocg43 
                   WHERE 1=1"

   PREPARE prp_del_tmp FROM v_s_qry
   EXECUTE prp_del_tmp
END FUNCTION


FUNCTION fn_borra_arch()
      LET v_s_qry = "DELETE
                       FROM ocg_ctr_archivo 
                      WHERE id_ocg_ctr_archivo = 0"
      PREPARE prp_del_tabla_ctr FROM v_s_qry
      EXECUTE prp_del_tabla_ctr

      LET v_s_qry = "INSERT INTO ocg_ctr_archivo VALUES(0,0,today,3901,2,'pruebas',0,0,0,0,0,0,10,today ,'SAFREVIV')"
      PREPARE prp_ins_arch FROM v_s_qry
      EXECUTE prp_ins_arch
END FUNCTION


FUNCTION fn_crea_tmp()
   DATABASE safre_tmp

   drop table if exists tmp_ocg_devolucion_prospecto;

   create table tmp_ocg_devolucion_prospecto(
   id_ocg_devolucion     decimal(9,0),
   id_referencia_dis     decimal(9,0),
   id_ocg_detalle        decimal(9,0),
   id_ocg_formalizacion  decimal(9,0),
   id_derechohabiente    decimal(9,0),
   nss                   CHAR(11),
   cve_ent_financiera    SMALLINT,
   num_ctr_int_ef        char(18),
   bimestre_ap_subsec    char(6),
   importe_ap_pat        decimal(18,6),
   aiv_ap_pat            decimal(18,6),
   imp_revaluado_15      decimal(18,6),
   aiv_revaluado_15      decimal(18,6),
   f_transaccion         DATE,
   f_factura             DATE,
   diagnostico           char(2),
   estado                SMALLINT,
   situacion             SMALLINT)

   DATABASE safre_viv
  
END FUNCTION