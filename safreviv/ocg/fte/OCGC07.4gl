#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC07                                        #
#Objetivo            => PANTALLAS DE CONSULTA DE ACREDITADOS          #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 25 de ENERO de 2016                           #
#######################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_usuario           LIKE seg_usuario.usuario_cod
   DEFINE p_tpo_ejecucion     SMALLINT
   DEFINE p_s_titulo          STRING
   DEFINE w ui.Window
   DEFINE f ui.Form

   DEFINE v_bnd_nss           SMALLINT

   DEFINE v_nss               CHAR(11)
   DEFINE v_rfc               CHAR (13)
   DEFINE v_curp              CHAR (18)
   DEFINE v_ctr_ef            CHAR (18)
   DEFINE v_nss_asoc          CHAR (11)
   DEFINE v_marca_asoc        CHAR (1)
   DEFINE v_paterno           CHAR (40)
   DEFINE v_materno           CHAR (40)
   DEFINE v_nombre            CHAR (40)
   DEFINE v_bimestre          SMALLINT
   DEFINE v_marca             CHAR (30)
   DEFINE v_97                DECIMAL(12,2)
   DEFINE v_f_credito         DATE
   DEFINE v_f_subcta          DATE
   DEFINE v_formalizacion     DATE
   DEFINE v_ejercicio         CHAR (4)
   DEFINE v_f_proceso         CHAR(10)--DATE
   DEFINE v_situacion         SMALLINT
   DEFINE v_subproceso        CHAR (3)
   DEFINE v_ent_financiera    CHAR (3)
   DEFINE v_producto          CHAR (11)

   DEFINE v_notario           DECIMAL(4,0)
   DEFINE v_edo_notario       SMALLINT
   DEFINE v_escritura         DECIMAL(8,0)
   DEFINE v_rpp               CHAR (15)
   DEFINE v_folio             DECIMAL(8,0)
   DEFINE v_partida           DECIMAL(6,0)
   DEFINE v_foja              DECIMAL(8,0)
   DEFINE v_volumen           DECIMAL(6,0)
   DEFINE v_libro             DECIMAL(6,0)
   DEFINE v_tomo              DECIMAL(6,0)
   DEFINE v_seccion           DECIMAL(6,0)
   DEFINE v_ent_inmueble      SMALLINT
   DEFINE v_domicilio         CHAR (30)
   DEFINE v_avaluo            DECIMAL(15,2)
   DEFINE v_monto_credito     DECIMAL(15,2)
   DEFINE v_plazo_credito     DECIMAL(5,0)
   DEFINE v_moneda            SMALLINT
   DEFINE v_tasa              CHAR (20)
   DEFINE v_margen            CHAR (20)
   DEFINE v_oto_ef            DATE
   DEFINE v_reg_carta         DATE
   DEFINE v_usuario_reg       CHAR (20)
   DEFINE v_genero            CHAR(1)
   DEFINE v_mcpio_inmueble    DECIMAL(5,0)
   DEFINE v_f_sol_mca_prcr    DATE
   DEFINE v_f_conf_mca_prcr   DATE
   DEFINE v_f_sol_desm_prcr   DATE
   DEFINE v_f_conf_desm_prcr  DATE
   DEFINE v_ent_fed_notario   SMALLINT
   DEFINE v_f_liquida_cofi    DATE
                              
   DEFINE v_criterio          SMALLINT
   DEFINE bnd_qry             SMALLINT
                              
   DEFINE p_nss               CHAR(11)
   DEFINE p_subproceso        CHAR(3)
   DEFINE p_ent_financiera    CHAR(3)
   DEFINE p_f_proceso         DATE
   DEFINE p_situacion         CHAR(3)
   DEFINE p_producto          CHAR(10)
   DEFINE a                   INTEGER
   DEFINE v_nom               STRING
   DEFINE bnd_datos           SMALLINT
   DEFINE bnd_ant             SMALLINT
   DEFINE v_igual             CHAR(3)
   DEFINE v_comilla           CHAR(1)
   DEFINE v_causa_liquida     SMALLINT

   DEFINE arr_detalle DYNAMIC ARRAY OF RECORD
          id_ocg_detalle      DECIMAL(9,0),
          id_derechohabiente  DECIMAL(9,0),
          subproceso          CHAR(3),
          f_proceso           DATE,
          cve_ent_financiera  SMALLINT,
          nss                 CHAR(11)
   END RECORD

     DEFINE arr_tramite DYNAMIC ARRAY OF RECORD
          id_ocg_tramite      DECIMAL(9,0),
          id_ocg_detalle      DECIMAL(9,0),
          rfc                 CHAR (13)    ,
          curp                CHAR (18)    ,
          cve_ef              CHAR (18)    ,
          paterno             CHAR (40)    ,
          materno             CHAR (40)    ,
          nombre              CHAR (40)    ,
          bimestre            SMALLINT     ,
          viv97               DECIMAL(12,2),
          f_credito           DATE         ,
          f_subcta            CHAR(4)      ,
          tpo_credito         CHAR(3)
   END RECORD

   DEFINE arr_formalizacion DYNAMIC ARRAY OF RECORD
          id_formalizacion    DECIMAL(9,0),
          id_derechohabiente  DECIMAL(9,0),
          id_detalle          DECIMAL(9,0),
          id_tramite          DECIMAL(9,0),
          paterno             CHAR(40),
          materno             CHAR(40),
          nombre              CHAR(40),
          rfc                 CHAR (13)    ,
          curp                CHAR (18)    ,
          viv97               DECIMAL(12,2),
          f_saldo             DATE,
          tpo_credito         CHAR(2),
          notario             DECIMAL(4,0)  ,
          ent_fed_notario     SMALLINT      ,
          mun_notario         SMALLINT      ,
          escritura           DECIMAL(8,0)  ,
          rpp                 CHAR (15)     ,
          folio               DECIMAL(8,0)  ,
          partida             DECIMAL(6,0)  ,
          foja                DECIMAL(8,0)  ,
          volumen             DECIMAL(6,0)  ,
          libro               DECIMAL(6,0)  ,
          tomo                DECIMAL(6,0)  ,
          seccion             DECIMAL(6,0)  ,
          ent_inmueble        SMALLINT      ,
          domicilio           CHAR (30)     ,
          avaluo              DECIMAL(15,2) ,
          monto_credito       DECIMAL(15,2) ,
          plazo_credito       DECIMAL(5,0)  ,
          moneda              SMALLINT      ,
          tasa                CHAR (20)     ,
          margen              CHAR (20)     ,
          oto_ef              DATE          ,
          reg_carta           DATE          ,
          usuario_reg         CHAR (20)     ,
          diagnostico         CHAR(3)       ,
          situacion           CHAR(3)       ,
          genero              CHAR(1)       ,
          mcpio_inmueble      DECIMAL(5,0)  ,
          f_formalizacion1      DATE        ,
          f_solic_marca_prcr    DATE        ,
          f_conf_marca_prcr     DATE        ,
          f_solic_desmarca_prcr DATE        ,
          f_conf_desmarca_prcr  DATE        ,
          f_liquida_credito     DATE        ,
          num_ctr_int_ef        CHAR(18)
   END RECORD

   DEFINE arr_liquidacion DYNAMIC ARRAY OF RECORD
          id_ocg_liquidacion   DECIMAL(9,0),
          id_ocg_formalizacion DECIMAL(9,0),
          id_ocg_detalle       DECIMAL(9,0),
          id_derechohabiente   DECIMAL(9,0),
          bimestre_ap_subsec   CHAR(6),
          importe_ap_subsec    DECIMAL(13,2),
          f_liberacion_gtia    DATE,
          importe_devuelto     DECIMAL(13,2),
          id_causa_liquida     SMALLINT,
          f_deposito           DATE,
          situacion            SMALLINT
   END RECORD

   DEFINE arr_tabla DYNAMIC ARRAY OF RECORD
          situacion           CHAR(3),
          subproceso          CHAR(3),
          ent_financiera      CHAR(3),
          diagnostico         CHAR(3),
          f_proceso           DATE,
          nss                 CHAR(11),
          num_ctr_interno     CHAR(18),
          nombre              CHAR(40)
   END RECORD

END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parámetro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC07.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   CALL fn_consulta_gral()

END MAIN

FUNCTION fn_consulta_gral()

   DEFINE p_busqueda,
          p_busqueda1   STRING
   DEFINE v_tabla             VARCHAR(25)
   DEFINE v_tipo_busqueda,
          v_tipo_subproceso   SMALLINT
    
   --DEFINE v_pos INTEGER

   OPEN WINDOW consulta WITH FORM "OCGC072"

   DIALOG ATTRIBUTES (UNBUFFERED)

      CONSTRUCT p_busqueda ON g.nss,
                              g.subproceso,
                              g.cve_ent_financiera,
                              g.f_proceso,
                              fz.situacion,
                              fz.tpo_credito
                         FROM p_nss,
                              p_subproceso,
                              p_ent_financiera,
                              p_f_proceso ,
                              p_situacion,
                              p_producto

      ON ACTION ACCEPT
         LET v_bnd_nss = 0
         --LET p_f_proceso = NULL 
         CALL fn_limpia_variables()

         LET v_nss            = GET_FLDBUF(p_nss)
         LET v_subproceso     = GET_FLDBUF(p_subproceso)
         LET v_ent_financiera = GET_FLDBUF(p_ent_financiera)
         LET v_f_proceso      = GET_FLDBUF(p_f_proceso)
         LET v_situacion      = GET_FLDBUF(p_situacion)
         LET v_producto       = GET_FLDBUF(p_producto)

         --DISPLAY "PRODUCTO ",v_producto

         LET v_f_proceso = v_f_proceso [4,5], v_f_proceso[1,2], v_f_proceso[7,10]
         
         --DISPLAY "Fecha de proceso: ", v_f_proceso

         
         --LET v_subproceso = v_subproceso USING "&&&"
         --LET p_situacion  = p_situacion  USING "&&&"

         IF (v_nss            IS NULL) AND
            (v_subproceso     IS NULL) AND
            (v_ent_financiera IS NULL) AND
            (v_f_proceso      IS NULL OR "          ") AND 
            (v_situacion      IS NULL) AND
            (v_producto       IS NULL) THEN
            CALL fn_mensaje ("Archivo","Debe ingresar al menos un parámetro de búsqueda adicional","information")
         ELSE

     IF (v_situacion IS NULL) AND
            (v_producto  IS NULL) THEN
         IF
            (v_nss            IS NOT NULL) OR
            (v_subproceso     IS NOT NULL) OR
            (v_ent_financiera IS NOT NULL) OR
            (v_f_proceso      IS NOT NULL) THEN

            IF (v_subproceso IS NOT NULL)  THEN
               IF (LENGTH (v_subproceso) <> 3) THEN 
                   CALL fn_mensaje ("Archivo","El proceso debe ser ingresado a 3 dígitos","information") 
               ELSE 
                  IF v_subproceso = "005" THEN
                     LET v_tabla = "ocg_liquidacion "
                     LET v_tipo_subproceso = 5
                  ELSE
                     LET v_tabla = "ocg_formalizacion"
                     LET v_tipo_subproceso = 2
                  END IF
                  LET a = 1
                  LET p_busqueda = "  g.id_ocg_detalle in (select id_ocg_detalle from ",v_tabla," where situacion in (55,60,70,80,140,150,153,155,158,160,190)) ",
                                   " AND ", p_busqueda -- OR \n
                  CALL fn_detalle(p_busqueda,'',a,v_tipo_subproceso)
   
                  IF arr_detalle.getLength()  > = 1 THEN
                     LET v_tipo_busqueda = 1
   
                     LET bnd_datos = 1
                  ELSE
                     LET bnd_datos = 0
                     CALL fn_mensaje ("Archivo","No se encontraron regístros con los parámetros ingresados","information")
                     LET v_nss            = ""
                     LET v_subproceso     = ""
                     LET v_ent_financiera = ""
                     LET v_f_proceso      = ""
                     LET v_situacion      = ""
                     LET v_producto       = ""
                     CLEAR FORM
                  END IF
               END IF
            ELSE

               LET a = 1
               LET p_busqueda1 = " g.id_ocg_detalle in (select id_ocg_detalle from ocg_liquidacion where situacion in (55,60,70,80,140,150,153,155,158,160,190)) ",
                                " AND g.subproceso IN (2,5) AND ", p_busqueda 
               LET p_busqueda = " g.id_ocg_detalle in (select id_ocg_detalle from ocg_formalizacion where situacion in (55,60,70,80,140,150,153,155,158,160,190)) ",
                                " AND g.subproceso IN (2,5) AND ", p_busqueda 
               CALL fn_detalle(p_busqueda,p_busqueda1,a,3)

               IF arr_detalle.getLength()  > = 1 THEN
                  LET v_tipo_busqueda = 1

                  LET bnd_datos = 1
               ELSE
                  LET bnd_datos = 0
                  CALL fn_mensaje ("Archivo","No se encontraron regístros con los parámetros ingresados","information")
                  LET v_nss            = ""
                  LET v_subproceso     = ""
                  LET v_ent_financiera = ""
                  LET v_f_proceso      = ""
                  LET v_situacion      = ""
                  LET v_producto       = ""
                  CLEAR FORM
               END IF
            END IF
      END IF
     END IF
--***************************************************************************************************
     IF (v_nss            IS NULL) AND
            (v_subproceso     IS NULL) AND
            (v_ent_financiera IS NULL) AND
            (v_f_proceso      IS NULL) AND
            (v_situacion      IS NULL) AND
            (v_producto       IS NOT NULL) THEN

            --DISPLAY "producto : ",v_producto
           
            IF (v_producto = "AI" ) THEN
               LET p_busqueda = " fz.tpo_credito in ('A','C')"
            END IF

            IF (v_producto = "CO" ) THEN
               LET p_busqueda = " fz.tpo_credito in (7,8)"
            END IF

            IF v_producto = 7 OR 
               v_producto = 8 OR 
               v_producto = "A" OR
               v_producto = "C" THEN
               LET p_busqueda = "fz.tpo_credito in ('",v_producto CLIPPED,"')"
            END IF

            LET a = 1
            LET p_busqueda = p_busqueda," AND fz.situacion in (55,60,70,80,140,150,153,155,158,160,190)"
            
            CALL fn_formalizacion(p_busqueda,a)
            --DISPLAY "tramite ",arr_tramite[a].*

            IF arr_formalizacion.getLength() > = 1 THEN
            
               LET v_tipo_busqueda = 2
               
               --CALL fn_despliega_datos()
               --EXIT DIALOG
               LET bnd_datos = 1
            ELSE
               LET bnd_datos = 0
               CALL fn_mensaje ("Archivo","No se encontraron regístros con los parámetros ingresados","information")
               LET v_nss            = ""
               LET v_subproceso     = ""
               LET v_ent_financiera = ""
               LET v_f_proceso      = ""
               LET v_situacion      = ""
               LET v_producto       = ""
               CLEAR FORM
            END IF
     END IF
--***************************************************************************************************
     IF (v_producto       IS NOT NULL) AND
            (v_situacion      IS NULL) THEN
            IF (v_nss            IS NOT NULL) OR
               (v_subproceso     IS NOT NULL) OR
               (v_ent_financiera IS NOT NULL) OR
               (v_f_proceso      IS NOT NULL) THEN

               IF v_nss IS NOT NULL THEN
                   LET p_busqueda = " g.nss = ","' ",v_nss,"'"
                END IF

                IF v_subproceso IS NOT NULL THEN
                   LET p_busqueda = " g.subproceso = ","' ",v_subproceso,"'"
                END IF

                IF v_ent_financiera IS NOT NULL THEN
                   LET p_busqueda = " g.cve_ent_financiera = ","' ",v_ent_financiera,"'"
                END IF

                IF v_f_proceso = "          " THEN 
                ELSE 
                   IF v_f_proceso IS NOT NULL THEN
                      LET p_busqueda = "g.f_proceso = ","' ",v_f_proceso,"'"
                   END IF
                END IF 

                LET v_igual = " = "
                LET v_comilla = "'"

                IF (v_producto = "AI" ) THEN
                   LET v_producto = " ('A','C')"
                   LET v_igual = " "
                   LET v_comilla = " "
                END IF

                IF (v_producto = "CO" ) THEN
                   LET v_producto = " (7,8)"
                   LET v_igual = " "
                   LET v_comilla = " "
                END IF

                IF v_producto = 7 OR 
                   v_producto = 8 OR 
                   v_producto = "A" OR
                   v_producto = "C" THEN
                   LET v_producto = "('",v_producto CLIPPED,"')"
                   DISPLAY "PRODUCTO FINAL" ,v_producto
                END IF

                --DISPLAY "producto : ",v_producto
                --DISPLAY "comilla : ",v_comilla
                --DISPLAY "igual : ",v_igual

                LET a = 1
                IF v_subproceso = "005" THEN
                   LET v_tabla = "ocg_liquidacion"
                   LET v_tipo_subproceso = 5
                ELSE
                   IF v_subproceso = "002" THEN
                      LET v_tabla = "ocg_formalizacion"
                      LET v_tipo_subproceso = 2
                   ELSE
                      LET v_tipo_subproceso = 3
                   END IF
                END IF

                IF v_tipo_subproceso = 2 OR
                   v_tipo_subproceso = 5 THEN
                   LET p_busqueda =  "  g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,",
                                                     v_tabla," ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND oa.situacion in (55,60,70,80,140,150,153,155,158,160,190)
                                                 AND ocf.situacion = oa.situacion
                                                 AND tpo_credito in ",v_producto,") AND g.subproceso IN (2,5) AND ", p_busqueda
                   CALL fn_detalle(p_busqueda,'',a,v_tipo_subproceso)
                ELSE
                   LET p_busqueda1 =  "  g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,
                                                     ocg_liquidacion ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND oa.situacion in (55,60,70,80,140,150,153,155,158,160,190)
                                                 AND ocf.situacion = oa.situacion
                                                 AND tpo_credito in ",v_producto,") AND g.subproceso IN (2,5) AND ", p_busqueda
                   LET p_busqueda =  "  g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,
                                                     ocg_formalizacion ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND oa.situacion in (55,60,70,80,140,150,153,155,158,160,190)
                                                 AND ocf.situacion = oa.situacion
                                                 AND tpo_credito in ",v_producto,") AND g.subproceso = 2 AND ", p_busqueda
                   CALL fn_detalle(p_busqueda,p_busqueda1,a,v_tipo_subproceso)
                END IF
                --DISPLAY "detalle ",arr_detalle[a].*

                --IF arr_detalle[arr_detalle.getLength()].id_ocg_detalle IS NULL THEN
                --   CALL arr_detalle.deleteElement(arr_detalle.getLength())
               -- END IF

                IF arr_detalle.getLength() > = 1 THEN
                   LET v_tipo_busqueda = 1
                  LET bnd_datos = 1
               ELSE
                  LET bnd_datos = 0
                  CALL fn_mensaje ("Archivo","No se encontraron regístros con los parámetros ingresados","information")
                  LET v_nss            = ""
                  LET v_subproceso     = ""
                  LET v_ent_financiera = ""
                  LET v_f_proceso      = ""
                  LET v_situacion      = ""
                  LET v_producto       = ""
                  CLEAR FORM
               END IF
            END IF
     END IF
--***************************************************************************************************
     IF (v_nss            IS NULL) AND
        (v_subproceso     IS NULL) AND
        (v_ent_financiera IS NULL) AND
        (v_f_proceso      IS NULL) AND
        (v_producto       IS NULL) AND
        (v_situacion      IS NOT NULL) THEN

            --DISPLAY "situacion ", v_situacion
            --DISPLAY "fecha proceso " , v_f_proceso
            --DISPLAY "busqueda ",p_busqueda
            --DISPLAY "situacion o estado ", v_situacion

            LET a = 1
            LET p_busqueda = " cre.situacion = ","'",v_situacion,"'"
            CALL fn_formalizacion(p_busqueda,a)
            --DISPLAY "formalizacion  ",arr_formalizacion[a].*

            --IF arr_formalizacion[arr_formalizacion.getLength()].id_tramite IS NULL THEN
              --   CALL arr_formalizacion.deleteElement(arr_formalizacion.getLength())
            --END IF

            IF arr_formalizacion.getLength() > = 1 THEN
               LET v_tipo_busqueda = 3
               FOR a = 1 TO arr_formalizacion.getLength()
                  IF arr_formalizacion[a].id_detalle IS NOT NULL THEN
                     LET p_busqueda = " g.id_derechohabiente = ",arr_formalizacion[a].id_derechohabiente," "
                     CALL fn_detalle_situacion(p_busqueda,a)
                     IF arr_formalizacion[a].id_tramite IS NOT NULL THEN
                        LET p_busqueda = " t.id_ocg_tramite = ",arr_formalizacion[a].id_tramite," "
                        CALL fn_tramite(p_busqueda,a)
                        --DISPLAY "tramite : ",arr_tramite[a].*
                     END IF
                  END IF
                  CALL fn_arreglo(a)
               END FOR
               LET bnd_datos = 1
            ELSE
               LET bnd_datos = 0
               CALL fn_mensaje ("Archivo","No se encontraron regístros con los parámetros ingresados","information")
               LET v_nss            = ""
               LET v_subproceso     = ""
               LET v_ent_financiera = ""
               LET v_f_proceso      = ""
               LET v_situacion      = ""
               LET v_producto       = ""
               CLEAR FORM
            END IF
      END IF
--**************************************************************************************************
         IF (v_situacion IS NOT NULL) AND
            (v_producto IS NULL) THEN
            IF (v_nss            IS NOT NULL) OR
               (v_subproceso     IS NOT NULL) OR
               (v_ent_financiera IS NOT NULL) OR
               (v_f_proceso      IS NOT NULL) THEN

                --DISPLAY "detalle y formalizacion"
               
               IF v_nss IS NOT NULL THEN
                   LET p_busqueda = " g.nss = ","'",v_nss,"'"
                END IF

                IF v_subproceso IS NOT NULL THEN
                      LET p_busqueda = " g.subproceso = ","'",v_subproceso,"'"
                END IF
                
                IF v_ent_financiera IS NOT NULL THEN
                   LET p_busqueda = " g.cve_ent_financiera = ","'",v_ent_financiera,"'"
                END IF

                IF v_f_proceso = "          " THEN 
                ELSE 
                   IF v_f_proceso IS NOT NULL THEN
                      LET p_busqueda = " g.f_proceso = ","'",v_f_proceso,"'"
                   END IF
                END IF 
                
                LET a = 1
                
                IF v_subproceso = "005" THEN
                   LET v_tabla = "ocg_liquidacion"
                   LET v_tipo_subproceso = 5
                ELSE
                   IF v_subproceso = "002" THEN
                      LET v_tabla = "ocg_formalizacion"
                      LET v_tipo_subproceso = 2
                   ELSE
                      LET v_tipo_subproceso = 3
                   END IF
                END IF
                
                IF v_tipo_subproceso = 2 OR
                   v_tipo_subproceso = 5 THEN
                   
                   LET p_busqueda =  "  g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,",
                                                     v_tabla, " ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND ocf.situacion = ",v_situacion," AND oa.situacion = ocf.situacion
                                                 AND ocf.cve_ent_financiera = g.cve_ent_financiera)",
                                                 " AND g.subproceso IN (2,5) AND ", p_busqueda

                   CALL fn_detalle(p_busqueda,'',a,v_tipo_subproceso)
                ELSE
                   LET p_busqueda1 =  "  g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,
                                                     ocg_liquidacion ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND ocf.situacion = ",v_situacion," AND oa.situacion = ocf.situacion
                                                 AND ocf.cve_ent_financiera = g.cve_ent_financiera)",
                                                 " AND g.subproceso IN (2,5) AND ", p_busqueda

                   LET p_busqueda =  "  g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,
                                                     ocg_formalizacion ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND ocf.situacion = ",v_situacion," AND oa.situacion = ocf.situacion
                                                 AND ocf.cve_ent_financiera = g.cve_ent_financiera)",
                                                 " AND g.subproceso IN (2,5) AND ", p_busqueda
                   CALL fn_detalle(p_busqueda,p_busqueda1,a,v_tipo_subproceso)
                END IF
                --DISPLAY "detalle ",arr_detalle[a].*

                --IF arr_detalle[arr_detalle.getLength()].id_ocg_detalle IS NULL THEN
                  -- CALL arr_detalle.deleteElement(arr_detalle.getLength())
                --END IF

                IF arr_detalle.getLength() > = 1 THEN
                   LET v_tipo_busqueda = 1
                  LET bnd_datos = 1
               ELSE
                  LET bnd_datos = 0
                  CALL fn_mensaje ("Archivo","No se encontraron regístros con los parámetros ingresados","information")
                  LET v_nss            = ""
                  LET v_subproceso     = ""
                  LET v_ent_financiera = ""
                  LET v_f_proceso      = ""
                  LET v_situacion      = ""
                  LET v_producto       = ""
                  CLEAR FORM
               END IF
            END IF
         END IF
--**************************************************************************************************
         IF (v_nss            IS NULL) AND
            (v_subproceso     IS NULL) AND
            (v_ent_financiera IS NULL) AND
            (v_f_proceso      IS NULL) AND 
            (v_situacion      IS NOT NULL) AND
            (v_producto       IS NOT NULL)THEN

            LET a = 1
            --LET p_busqueda = "fz.situacion = ",v_situacion
            LET p_busqueda = " fz.situacion = ",v_situacion," and fz.tpo_credito = ","'",v_producto CLIPPED,"'"

            IF (v_producto = "AI" ) THEN
               LET p_busqueda =" fz.situacion = ",v_situacion," and fz.tpo_credito in ('A','C')"
            END IF

            IF (v_producto = "CO" ) THEN
               LET p_busqueda = " fz.situacion = ",v_situacion," and fz.tpo_credito in (7,8)"
            END IF

            CALL fn_formalizacion(p_busqueda,a)
           -- DISPLAY "formalizacion ",arr_formalizacion[a].*
            
           --- IF arr_formalizacion[arr_formalizacion.getLength()].id_detalle IS NULL THEN
              --- CALL arr_formalizacion.deleteElement(arr_formalizacion.getLength())
            ---END IF

            IF arr_formalizacion.getLength() > = 1 THEN
               LET v_tipo_busqueda = 3
               FOR a = 1 TO arr_formalizacion.getLength()
                   IF arr_formalizacion[a].id_detalle IS NOT NULL THEN
                      LET p_busqueda = " g.id_derechohabiente = ",arr_formalizacion[a].id_derechohabiente," "
                      CALL fn_detalle_situacion(p_busqueda,a)
                      --DISPLAY "detalle ", arr_detalle[a].*
                      IF arr_formalizacion[a].id_tramite IS NOT NULL THEN
                         LET p_busqueda = " t.id_ocg_tramite = ",arr_formalizacion[a].id_tramite
                         CALL fn_tramite(p_busqueda,a)
                         --DISPLAY "tramite ",arr_tramite[a].*
                      END IF
                   END IF
                   CALL fn_arreglo(a)
               END FOR

              -- CALL fn_despliega_datos()
               --EXIT DIALOG
               LET bnd_datos = 1
            ELSE
               LET bnd_datos = 0
               CALL fn_mensaje ("Archivo","No se encontraron regístros con los parámetros ingresados","information")
               LET v_nss            = ""
               LET v_subproceso     = ""
               LET v_ent_financiera = ""
               LET v_f_proceso      = ""
               LET v_situacion      = ""
               LET v_producto       = ""
               CLEAR FORM
            END IF
         END IF
--***************************************************************************************************
         IF (v_situacion      IS NOT NULL) AND
            (v_producto       IS NOT NULL)THEN
            IF (v_nss            IS NULL) OR
               (v_subproceso     IS NULL) OR 
               (v_ent_financiera IS NULL) OR
               (v_f_proceso      IS NULL) THEN

               IF v_nss IS NOT NULL THEN
                   LET p_busqueda = "g.nss = ","'",v_nss,"'"
                END IF

                IF v_subproceso IS NOT NULL THEN
                   LET p_busqueda = " g.subproceso = ","'",v_subproceso,"'"
                END IF

                IF v_ent_financiera IS NOT NULL THEN
                   LET p_busqueda = " g.cve_ent_financiera = ","'",v_ent_financiera,"'"
                END IF

                IF v_f_proceso <> "          " OR v_f_proceso <> NULL THEN
                   LET p_busqueda = " g.f_proceso = ","'",v_f_proceso,"'"
                END IF

                LET v_igual = " = "
                LET v_comilla = "'"

                IF (v_producto = "AI" ) THEN
                   LET v_producto = " ('A','C')"
                   LET v_igual = " "
                   LET v_comilla = " "
                END IF

                IF (v_producto = "CO" ) THEN
                   LET v_producto = " (7,8)"
                   LET v_igual = " "
                   LET v_comilla = " "
                END IF

                IF v_producto = 7 OR
                   v_producto = 8 OR
                   v_producto = "A" OR 
                   v_producto = "C" THEN
                   LET v_producto = "('",v_producto CLIPPED,"')"
                END IF

                --DISPLAY "producto 2: ",v_producto

                LET a = 1
                
                IF v_subproceso = "005" THEN
                   LET v_tabla = "ocg_liquidacion"
                   LET v_tipo_subproceso = 5
                ELSE
                   IF v_subproceso = "002" THEN
                      LET v_tabla = "ocg_formalizacion"
                      LET v_tipo_subproceso = 2
                   ELSE
                      LET v_tipo_subproceso = 3
                   END IF
                      
                END IF

                IF v_tipo_subproceso = 2 OR
                   v_tipo_subproceso = 5 THEN
                   LET p_busqueda = " g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,",
                                                     v_tabla," ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND oa.situacion = ",v_situacion,"
                                                 AND ocf.tpo_credito in ",v_producto,") AND g.subproceso IN (2,5) AND ", p_busqueda
                   CALL fn_detalle(p_busqueda,'',a,v_tipo_subproceso)
                ELSE
                   LET p_busqueda1 = " g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,
                                                     ocg_liquidacion ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND oa.situacion = ",v_situacion,"
                                                 AND ocf.tpo_credito in ",v_producto,") AND g.subproceso IN (2,5) AND ", p_busqueda
                   LET p_busqueda = " g.id_ocg_detalle in
                                             (select ocf.id_ocg_detalle
                                                FROM ocg_acreditado oa,
                                                     ocg_formalizacion ocf
                                               WHERE ocf.id_ocg_formalizacion = oa.id_ocg_formalizacion
                                                 AND oa.situacion = ",v_situacion,"
                                                 AND ocf.tpo_credito in ",v_producto,") AND g.subproceso IN (2,5) AND ", p_busqueda
                   CALL fn_detalle(p_busqueda,p_busqueda1,a,v_tipo_subproceso)
                END IF

                                   


                IF arr_detalle.getLength() > = 1 THEN
                   LET v_tipo_busqueda = 1
                  LET bnd_datos = 1
               ELSE
                  LET bnd_datos = 0
                  CALL fn_mensaje ("Archivo","No se encontraron regístros con los parámetros ingresados","information")
                  LET v_nss            = ""
                  LET v_subproceso     = ""
                  LET v_ent_financiera = ""
                  LET v_f_proceso      = ""
                  LET v_situacion      = ""
                  LET v_producto       = ""
                  CLEAR FORM
               END IF
            END IF
         END IF
--**************************************************************************************************
         IF bnd_datos = 1 THEN
            CALL fn_despliega_datos(v_tipo_busqueda)
         END IF

         END IF

         LET v_nss            = ""
         LET v_subproceso     = ""
         LET v_ent_financiera = ""
         LET v_f_proceso      = ""
         LET v_situacion      = ""
         LET v_producto       = ""
         CLEAR FORM
   ON ACTION CLOSE
      CALL fn_limpia_variables()
      EXIT DIALOG
   END CONSTRUCT
END DIALOG
CLOSE WINDOW consulta

END FUNCTION

FUNCTION fn_detalle(p_busqueda,p_busqueda1,a,l_tipo_subproceso)

   DEFINE v_qry_detalle STRING
   DEFINE p_busqueda,
          p_busqueda1 STRING
   DEFINE a INTEGER
   DEFINE l_tipo_subproceso SMALLINT

   LET a = 1
   --DISPLAY "p_busqueda: ", p_busqueda
   --DISPLAY "p_busqueda1:", p_busqueda1


   IF l_tipo_subproceso = 5 THEN
   
      LET v_qry_detalle = " SELECT g.id_ocg_detalle,
                               g.id_derechohabiente,
                               g.subproceso,
                               g.f_proceso,
                               g.cve_ent_financiera,
                               g.nss,
                               fz.diagnostico,
                               nvl(trim(fz.ap_paterno),' ')||' '||nvl(trim(fz.ap_materno),' ')||' '||nvl(trim(fz.nombre),' '),
                               fz.num_ctr_int_ef,
                               fz.situacion
                          FROM ocg_detalle g ,ocg_formalizacion fz, ocg_liquidacion oli 
                         WHERE oli.id_ocg_detalle = g.id_ocg_detalle 
                           AND fz.id_ocg_formalizacion = oli.id_ocg_formalizacion AND ",p_busqueda
   ELSE
      IF l_tipo_subproceso = 2 THEN
         LET v_qry_detalle = " SELECT g.id_ocg_detalle,
                               g.id_derechohabiente,
                               g.subproceso,
                               g.f_proceso,
                               g.cve_ent_financiera,
                               g.nss,
                               fz.diagnostico,
                               nvl(trim(fz.ap_paterno),' ')||' '||nvl(trim(fz.ap_materno),' ')||' '||nvl(trim(fz.nombre),' '),
                               fz.num_ctr_int_ef,
                               fz.situacion
                          FROM ocg_detalle g ,ocg_formalizacion fz 
                         WHERE g.id_ocg_detalle = fz.id_ocg_detalle AND ",p_busqueda
      ELSE
         LET v_qry_detalle = " SELECT g.id_ocg_detalle,
                               g.id_derechohabiente,
                               g.subproceso,
                               g.f_proceso,
                               g.cve_ent_financiera,
                               g.nss,
                               fz.diagnostico,
                               nvl(trim(fz.ap_paterno),' ')||' '||nvl(trim(fz.ap_materno),' ')||' '||nvl(trim(fz.nombre),' '),
                               fz.num_ctr_int_ef,
                               fz.situacion
                          FROM ocg_detalle g ,ocg_formalizacion fz, ocg_liquidacion oli 
                         WHERE oli.id_ocg_detalle = g.id_ocg_detalle 
                           AND fz.id_ocg_formalizacion = oli.id_ocg_formalizacion AND ",p_busqueda1,
                        " UNION ALL ",
                       " SELECT g.id_ocg_detalle,
                               g.id_derechohabiente,
                               g.subproceso,
                               g.f_proceso,
                               g.cve_ent_financiera,
                               g.nss,
                               fz.diagnostico,
                               nvl(trim(fz.ap_paterno),' ')||' '||nvl(trim(fz.ap_materno),' ')||' '||nvl(trim(fz.nombre),' '),
                               fz.num_ctr_int_ef,
                               fz.situacion
                          FROM ocg_detalle g ,ocg_formalizacion fz 
                         WHERE g.id_ocg_detalle = fz.id_ocg_detalle AND ",p_busqueda
      END IF
   END IF

   --DISPLAY "p_busqueda global: ",p_busqueda
   --DISPLAY "v_qry detalle global: ", v_qry_detalle

   PREPARE prp_detalle FROM v_qry_detalle
   DECLARE cur_detalle CURSOR FOR prp_detalle
   
   FOREACH cur_detalle INTO arr_detalle[a].*,
   
                            arr_tabla[a].diagnostico,
                            arr_tabla[a].nombre,
                            arr_tabla[a].num_ctr_interno,
                            arr_tabla[a].situacion
                            
       LET arr_tabla[a].ent_financiera = arr_detalle[a].cve_ent_financiera USING "&&&"
       LET arr_tabla[a].f_proceso      = arr_detalle[a].f_proceso --USING "DD-MM-YYYY"
       LET arr_tabla[a].nss            = arr_detalle[a].nss
       LET arr_tabla[a].subproceso     = arr_detalle[a].subproceso USING "&&&"
       
       LET a = a+1
      
   END FOREACH
   
   CALL arr_detalle.deleteElement(arr_detalle.getLength())
   CALL arr_tabla.deleteElement(arr_tabla.getLength())
   
   --DISPLAY "cantidad : ",arr_detalle.getLength()
END FUNCTION

FUNCTION fn_detalle_situacion(p_busqueda,a)

   DEFINE v_qry_detalle STRING
   DEFINE p_busqueda STRING
   DEFINE a INTEGER

   LET v_qry_detalle = " SELECT g.id_ocg_detalle,
                               g.id_derechohabiente,
                               g.subproceso,
                               g.f_proceso,
                               g.cve_ent_financiera,
                               g.nss
                          FROM ocg_detalle g 
                          WHERE ",
                          p_busqueda

   --DISPLAY "busqueda : ",p_busqueda
    DISPLAY "detalle segunda pantalla: ", v_qry_detalle

   PREPARE prp_detalle1 FROM v_qry_detalle
   DECLARE cur_detalle1 CURSOR FOR prp_detalle1

   FOREACH cur_detalle1 INTO arr_detalle[a].*
      LET a = a+1
   END FOREACH
   CALL arr_detalle.deleteElement(arr_detalle.getLength())
   --DISPLAY "cantidad : ",arr_detalle.getLength()
END FUNCTION




FUNCTION fn_tramite(p_busqueda,a)

   DEFINE p_busqueda STRING
   DEFINE v_qry_tramite STRING
   DEFINE a INTEGER

   LET v_qry_tramite = 
     "SELECT t.id_ocg_tramite,
             g.id_ocg_detalle,
             t.rfc,
             t.curp,
             t.cve_ent_financiera,
             t.ap_paterno,
             t.ap_materno,
             t.nombre,
             t.num_bimestres,
             t.viv97,
             t.f_saldo,
             YEAR(t.f_saldo),
             t.tpo_credito
       FROM ocg_tramite t,ocg_detalle g
      WHERE t.id_ocg_detalle = g.id_ocg_detalle
        AND ",p_busqueda

   --DISPLAY v_qry_tramite
   PREPARE prp_tramite FROM v_qry_tramite
   DECLARE cur_tramite CURSOR FOR prp_tramite

   FOREACH cur_tramite INTO arr_tramite[a].*
     -- DISPLAY arr_tramite[a].*
      LET a = a+1
   END FOREACH

   CALL arr_tramite.deleteElement(arr_tramite.getLength())
END FUNCTION

FUNCTION fn_formalizacion(p_busqueda,a)

   DEFINE p_busqueda          STRING
   DEFINE v_qry_formalizacion STRING
   DEFINE a                   INTEGER
   DEFINE v_s_qry             STRING

   LET v_qry_formalizacion = 

  " SELECT fz.id_ocg_formalizacion,
           fz.id_derechohabiente,
           fz.id_ocg_detalle,
           fz.id_ocg_tramite,
           fz.ap_paterno,
           fz.ap_materno,
           fz.nombre,
           fz.rfc,
           fz.curp,
           fz.viv97,
           fz.f_saldo,
           fz.tpo_credito,
           fz.notario,           
           fz.ent_fed_notario,
           fz.mcpio_notario ,     
           fz.num_escritura,     
           fz.num_rpp,           
           fz.folio_real,        
           fz.partida,           
           fz.foja,              
           fz.volumen,           
           fz.libro,             
           fz.tomo,              
           fz.seccion,           
           fz.ent_fed_inmueble,  
           fz.domicilio_inmueble,
           fz.valor_avaluo,      
           fz.monto_credito,     
           fz.plazo_credito,     
           fz.tpo_moneda,        
           fz.tasa_base,         
           fz.margen,            
           fz.f_otorga_ent_fin,  
           fz.f_registro_carta,
           fz.usuario_reg_carta,
           fz.diagnostico,
           cre.situacion,
           fz.genero,
           fz.mcpio_inmueble,
           cre.f_formalizacion,
           cre.f_solic_marca_prcr,
           cre.f_conf_marca_prcr,
           cre.f_solic_desmarca_prcr,
           cre.f_conf_desmarca_prcr,
           cre.f_liquida_credito,
           fz.num_ctr_int_ef
      FROM ocg_formalizacion fz,ocg_acreditado cre
     WHERE ", p_busqueda, " AND fz.id_ocg_formalizacion = cre.id_ocg_formalizacion"

      --DISPLAY "consulta formalizacion : ",v_qry_formalizacion
      -- "AND fz.situacion IN (55,60,70,80,140,150,153,155,158,160,190)"
    --DISPLAY p_busqueda

   PREPARE prp_formalizacion FROM v_qry_formalizacion
   DECLARE cur_formalizacion CURSOR FOR prp_formalizacion

   FOREACH cur_formalizacion INTO arr_formalizacion[a].*
      --display "cuenta acred : ",a
      CALL fn_liquidacion(arr_formalizacion[a].id_formalizacion,a)
       --DISPLAY arr_formalizacion[a].id_formalizacion
      LET a = a+1
   END FOREACH

   CALL arr_formalizacion.deleteElement(arr_formalizacion.getLength())


END FUNCTION

FUNCTION fn_liquidacion(l_id_ocg_formalizacion,a)

   DEFINE l_id_ocg_formalizacion DECIMAL(9,0)
   DEFINE v_qry_liq            STRING
   DEFINE a                    INTEGER

   LET v_qry_liq = "SELECT liq.id_ocg_liquidacion,
                           liq.id_ocg_formalizacion,
                           liq.id_ocg_detalle      ,
                           liq.id_derechohabiente  ,
                           liq.bimestre_ap_subsec  ,
                           liq.importe_ap_subsec   ,
                           acre.f_liquida_credito  ,
                           liq.importe_devuelto    ,
                           liq.id_causa_liquida    ,
                           liq.f_deposito          ,
                           liq.situacion
                      FROM ocg_liquidacion liq, 
                           ocg_formalizacion fz,
                           ocg_acreditado acre
                     WHERE liq.id_ocg_formalizacion = ", l_id_ocg_formalizacion,"
                       AND fz.id_ocg_formalizacion = liq.id_ocg_formalizacion
                       AND acre.id_ocg_formalizacion = liq.id_ocg_formalizacion
                       AND liq.situacion between 140 and 180
                       AND liq.estado in (50,30)"

   PREPARE prp_liquidacion FROM v_qry_liq
   DECLARE cur_liquidacion CURSOR FOR prp_liquidacion

   FOREACH cur_liquidacion INTO arr_liquidacion[a].*
      --display "cuenta liq : ",a
      --DISPLAY arr_liquidacion[a].*
    --  LET a = a+1
   END FOREACH

   --CALL arr_liquidacion.deleteElement(arr_liquidacion.getLength())

END FUNCTION

FUNCTION fn_arreglo(a)

   DEFINE a INTEGER

   --DISPLAY "pos a : ",a

   LET arr_tabla[a].diagnostico      = arr_formalizacion[a].diagnostico USING "&&"
   LET arr_tabla[a].ent_financiera   = arr_detalle[a].cve_ent_financiera USING "&&&"
   LET arr_tabla[a].f_proceso        = arr_detalle[a].f_proceso
   LET v_nom = arr_formalizacion[a].paterno CLIPPED," ",
               arr_formalizacion[a].materno CLIPPED," ",
               arr_formalizacion[a].nombre  CLIPPED
   LET arr_tabla[a].nombre           = v_nom
   LET arr_tabla[a].nss              = arr_detalle[a].nss
   LET arr_tabla[a].num_ctr_interno  = arr_formalizacion[a].num_ctr_int_ef
   LET arr_tabla[a].situacion        = arr_formalizacion[a].situacion
   LET arr_tabla[a].subproceso       = arr_detalle[a].subproceso USING "&&&"

   IF arr_tabla[a].situacion IS NULL THEN
      LET arr_tabla[a].situacion = arr_liquidacion[a].situacion
   END IF
   
END FUNCTION

FUNCTION fn_despliega_datos(l_tipo_busqueda)

   DEFINE v_pos,a              INTEGER
   DEFINE v_s_qry              STRING
   DEFINE v_f_formaliza        DATE
   DEFINE v_f_formalizacion    CHAR(8)
   DEFINE l_tipo_busqueda      SMALLINT
   DEFINE p_busqueda           STRING
   DEFINE ch                   base.Channel
   DEFINE v_ruta_envio         LIKE seg_modulo.ruta_envio 
   DEFINE v_nom_arh            STRING
   DEFINE v_detalle            STRING
   DEFINE v_mensaje            STRING

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'
    
   LET v_nom_arh = v_ruta_envio CLIPPED ,"/consulta_acreditados",".acre"
   
    OPEN WINDOW arreglo WITH FORM "OCGC071"
       LET w = ui.Window.getCurrent()
       LET f = w.getForm()

       LET v_criterio = 1
       CALL fn_oculta1(v_criterio)
       --LET v_criterio = 1
       CALL fn_oculta2(v_criterio)
       CALL fn_oculta3(v_criterio)
       DISPLAY ARRAY arr_tabla TO tab_detalle.* ATTRIBUTE (ACCEPT = TRUE, CANCEL=  FALSE)

      ON ACTION archivo

         LET ch = base.Channel.create()
         CALL ch.openFile(v_nom_arh,"w" )
         CALL ch.setDelimiter("|")
         FOR a = 1 TO arr_tabla.getLength()

            LET v_detalle = arr_tabla[a].diagnostico     CLIPPED,"|",
                            arr_tabla[a].ent_financiera  CLIPPED,"|",
                            arr_tabla[a].f_proceso       USING "yyyymmdd","|",
                            arr_tabla[a].nombre          CLIPPED,"|",
                            arr_tabla[a].nss             CLIPPED,"|",
                            arr_tabla[a].num_ctr_interno CLIPPED,"|",
                            arr_tabla[a].situacion       CLIPPED,"|",
                            arr_tabla[a].subproceso      CLIPPED,"|"

            CALL ch.writeLine([v_detalle])

         END FOR

         CALL ch.close()

         LET v_mensaje = "Archivo generado de forma correcta en :",v_nom_arh
         CALL fn_mensaje ("Archivo",v_mensaje,"information")
       
       ON ACTION ACCEPT
          LET v_pos = arr_curr()
          
    IF l_tipo_busqueda = 1 THEN
           IF arr_detalle[v_pos].subproceso = 2 THEN
              LET p_busqueda = " fz.id_ocg_detalle = ",arr_detalle[v_pos].id_ocg_detalle ," "," AND fz.situacion IN (55,60,70,80,140,150,153,155,158,160,190)"
           ELSE
              SELECT id_ocg_formalizacion
                INTO arr_formalizacion[v_pos].id_formalizacion
                FROM ocg_liquidacion
               WHERE id_ocg_detalle  = arr_detalle[v_pos].id_ocg_detalle
   --******************************************************************************
              IF arr_formalizacion[v_pos].id_formalizacion IS NULL THEN
                 CALL arr_detalle.deleteElement(v_pos)
                 CALL arr_formalizacion.deleteElement(v_pos)
                 LET v_pos  = v_pos -1
               END IF
   --******************************************************************************
               LET p_busqueda = " fz.id_ocg_formalizacion = ",arr_formalizacion[v_pos].id_formalizacion
           END IF
           CALL fn_formalizacion(p_busqueda,v_pos)
             
           IF arr_formalizacion[v_pos].id_tramite IS NOT NULL THEN
              LET p_busqueda = " t.id_ocg_tramite = ",arr_formalizacion[v_pos].id_tramite," "
              CALL fn_tramite(p_busqueda,v_pos)
              --DISPLAY "tramite ",arr_tramite[a].*
           END IF
           CALL fn_arreglo(v_pos)
    ELSE
       IF l_tipo_busqueda = 2 THEN
          IF arr_formalizacion[v_pos].id_tramite IS NOT NULL THEN
             LET p_busqueda = " t.id_ocg_tramite = ",arr_formalizacion[v_pos].id_tramite," "," AND t.situacion IN (55,60,70,80,140,150,153,155,158,160,190)"
             CALL fn_tramite(p_busqueda,v_pos)
          --DISPLAY "formalizacion ",arr_detalle[a].*
          END IF
          IF arr_formalizacion[v_pos].id_detalle IS NOT NULL THEN
                     --DISPLAY "consulta detalle :"
             LET p_busqueda = " g.id_derechohabiente = ",arr_formalizacion[v_pos].id_derechohabiente," "
             CALL fn_detalle(p_busqueda,p_busqueda,v_pos,3)
                        --DISPLAY "formalizacion : ",arr_formalizacion[a].*
          END IF
          CALL fn_arreglo(v_pos)
       ELSE --l_tipo_busqueda = 3
          IF arr_formalizacion[v_pos].id_detalle IS NOT NULL THEN
             LET p_busqueda = " g.id_derechohabiente = ",arr_formalizacion[v_pos].id_derechohabiente," "
             CALL fn_detalle(p_busqueda,p_busqueda,v_pos,3)
                      --DISPLAY "detalle ", arr_detalle[a].*
             IF arr_formalizacion[v_pos].id_tramite IS NOT NULL THEN
                LET p_busqueda = " t.id_ocg_tramite = ",arr_formalizacion[v_pos].id_tramite
                CALL fn_tramite(p_busqueda,v_pos)
                         --DISPLAY "tramite ",arr_tramite[a].*
             END IF
          END IF
          CALL fn_arreglo(v_pos)
       END IF
    END IF
    

         -- DISPLAY "pos arreglo : ",v_pos

          --DISPLAY arr_formalizacion[v_pos].*


          LET v_formalizacion = arr_formalizacion[v_pos].f_formalizacion1
          LET v_nss            = arr_detalle[v_pos].nss
          LET v_rfc            = arr_formalizacion[v_pos].rfc
          LET v_curp           = arr_formalizacion[v_pos].curp
          LET v_ctr_ef         = arr_formalizacion[v_pos].num_ctr_int_ef
          LET v_paterno        = arr_formalizacion[v_pos].paterno
          LET v_materno        = arr_formalizacion[v_pos].materno
          LET v_nombre         = arr_formalizacion[v_pos].nombre
          LET v_bimestre       = arr_tramite[v_pos].bimestre
          LET v_97             = arr_formalizacion[v_pos].viv97
          LET v_f_subcta       = arr_formalizacion[v_pos].f_saldo
          LET v_ejercicio      = YEAR (arr_formalizacion[v_pos].f_formalizacion1)
          LET v_situacion      = arr_formalizacion[v_pos].situacion
          LET v_ent_financiera = arr_detalle[v_pos].cve_ent_financiera USING "&&&"
          LET v_f_proceso      = arr_detalle[v_pos].f_proceso
          LET v_subproceso     = arr_detalle[v_pos].subproceso USING "&&&"
          LET v_notario        = arr_formalizacion[v_pos].notario
          LET v_ent_fed_notario=arr_formalizacion[v_pos].ent_fed_notario
          LET v_edo_notario    = arr_formalizacion[v_pos].mun_notario
          LET v_escritura      = arr_formalizacion[v_pos].escritura
          LET v_rpp            = arr_formalizacion[v_pos].rpp
          LET v_folio          = arr_formalizacion[v_pos].folio
          LET v_partida        = arr_formalizacion[v_pos].partida
          LET v_foja           = arr_formalizacion[v_pos].foja
          LET v_volumen        = arr_formalizacion[v_pos].volumen
          LET v_libro          = arr_formalizacion[v_pos].libro
          LET v_tomo           = arr_formalizacion[v_pos].tomo
          LET v_seccion        = arr_formalizacion[v_pos].seccion
          LET v_ent_inmueble   = arr_formalizacion[v_pos].ent_inmueble
          LET v_domicilio      = arr_formalizacion[v_pos].domicilio
          LET v_avaluo         = arr_formalizacion[v_pos].avaluo
          LET v_monto_credito  = arr_formalizacion[v_pos].monto_credito
          LET v_plazo_credito  = arr_formalizacion[v_pos].plazo_credito
          LET v_moneda         = arr_formalizacion[v_pos].moneda
          LET v_tasa           = arr_formalizacion[v_pos].tasa
          LET v_margen         = arr_formalizacion[v_pos].margen
          LET v_oto_ef         = arr_formalizacion[v_pos].oto_ef
          LET v_reg_carta      = arr_formalizacion[v_pos].reg_carta
          LET v_usuario_reg    = arr_formalizacion[v_pos].usuario_reg
          LET v_producto       = arr_formalizacion[v_pos].tpo_credito
          LET v_genero         = arr_formalizacion[v_pos].genero
          LET v_mcpio_inmueble = arr_formalizacion[v_pos].mcpio_inmueble

          LET v_f_sol_mca_prcr   = arr_formalizacion[v_pos].f_solic_marca_prcr
          LET v_f_conf_mca_prcr  = arr_formalizacion[v_pos].f_conf_marca_prcr
          LET v_f_sol_desm_prcr  = arr_formalizacion[v_pos].f_solic_desmarca_prcr
          LET v_f_conf_desm_prcr = arr_formalizacion[v_pos].f_conf_desmarca_prcr

          LET v_f_credito     = arr_liquidacion[v_pos].f_liberacion_gtia
          LET v_causa_liquida = arr_liquidacion[v_pos].id_causa_liquida

--******************************************************************************
-- recuepra datos de NSS conyuge
IF (arr_formalizacion[v_pos].tpo_credito = 7) OR
   (arr_formalizacion[v_pos].tpo_credito = 8) THEN

   LET v_s_qry = "SELECT MAX(f_liquidacion)
                    FROM ocg_liquidacion_cofi
                   WHERE nss = ","'",arr_detalle[v_pos].nss,"'",
                   " AND f_liquidacion  < ","'",arr_formalizacion[v_pos].f_formalizacion1,"'",
                   " AND cve_ent_financiera = ",arr_detalle[v_pos].cve_ent_financiera
              --  DISPLAY "qry conyuge : ",v_s_qry
   PREPARE prp_f_liquida FROM v_s_qry
   EXECUTE prp_f_liquida INTO v_f_liquida_cofi

END IF

IF (v_f_liquida_cofi IS NULL) OR
   (v_f_liquida_cofi = "12/31/1899") THEN

   IF arr_detalle[v_pos].subproceso = 2 THEN

LET v_s_qry = "SELECT tm.f_liquida_cofi
                             FROM ocg_formalizacion f,                            \n
                                  ocg_fecha_mig tm                                \n
                            WHERE tm.id_derechohabiente = f.id_derechohabiente    \n
                              AND tm.id_ocg_detalle     = f.id_ocg_detalle        \n
                              AND tm.id_ocg_referencia  = f.id_ocg_formalizacion  \n
                              AND tm.subproceso =  2                              \n
                              AND f.id_ocg_formalizacion= ? "

            PREPARE prp_cons_dat_migra FROM v_s_qry
            EXECUTE prp_cons_dat_migra USING arr_formalizacion[v_pos].id_formalizacion
                                        INTO v_f_liquida_cofi

     IF (v_f_liquida_cofi IS NULL) OR
        (v_f_liquida_cofi = "12/31/1899") THEN

        LET v_s_qry = "SELECT tm.f_liquida_cofi
                             FROM ocg_formalizacion f,                            \n
                                  ocg_fecha_mig tm                                \n
                            WHERE tm.id_derechohabiente = f.id_derechohabiente    \n
                              AND tm.id_ocg_referencia  = f.id_ocg_tramite        \n
                              AND tm.subproceso =  1                              \n
                              AND f.id_ocg_formalizacion= ? "

            PREPARE prp_cons_migra FROM v_s_qry
            EXECUTE prp_cons_migra USING arr_formalizacion[v_pos].id_formalizacion
                                        INTO v_f_liquida_cofi
     END IF

   ELSE
      LET v_s_qry = "SELECT tm.f_liquida_cofi
                             FROM ocg_liquidacion l,                              \n
                                  ocg_fecha_mig tm                                \n
                            WHERE tm.id_derechohabiente = l.id_derechohabiente    \n
                              AND tm.id_ocg_detalle     = l.id_ocg_detalle        \n
                              AND tm.id_ocg_referencia  = l.id_ocg_liquidacion    \n
                              AND tm.subproceso =  5                              \n
                              AND l.id_ocg_liquidacion = ? "

            PREPARE prp_cons_dat FROM v_s_qry
            EXECUTE prp_cons_dat USING arr_liquidacion[v_pos].id_ocg_liquidacion
                                        INTO v_f_liquida_cofi

            SELECT f_formalizacion
              INTO v_f_formaliza
              FROM ocg_acreditado
             WHERE id_ocg_formalizacion = arr_liquidacion[v_pos].id_ocg_formalizacion

            LET arr_formalizacion[v_pos].f_formalizacion1 = v_f_formaliza
                              
   END IF
END IF

   LET v_f_formalizacion = arr_formalizacion[v_pos].f_formalizacion1 USING "mmddyyyy"

   -- se modifica para mostrar cuando el tipo de crédito sea solamente cofinanciado 15/06/2017
   --***************************************************************************
   IF (arr_formalizacion[v_pos].tpo_credito = 7) OR
      (arr_formalizacion[v_pos].tpo_credito = 8) THEN

      LET v_s_qry = "SELECT FIRST 1
                            nss_conyuge,
                            marca_conyuge
                       FROM ocg_liquidacion_cofi
                      WHERE nss = ","'",arr_detalle[v_pos].nss,"'",
                      " AND f_liquidacion  < ","'",v_f_formalizacion,"'", --arr_formalizacion[v_pos].f_formalizacion1
                      " AND cve_ent_financiera = ",arr_detalle[v_pos].cve_ent_financiera,
                      "ORDER BY 1 DESC"

      PREPARE prp_cons_conyuge FROM v_s_qry
      EXECUTE prp_cons_conyuge INTO v_nss_asoc,
                                    v_marca_asoc
   END IF

--DISPLAY "posición     : ",v_pos
--DISPLAY "id_formaliza : ",arr_formalizacion[v_pos].id_formalizacion
--DISPLAY "fliquida cofi: ",v_f_liquida_cofi
--DISPLAY "id liquida    :",arr_liquidacion[v_pos].id_ocg_liquidacion

                 
IF v_f_liquida_cofi IS NULL OR
   v_f_liquida_cofi = "12/31/1899" THEN
   LET v_f_liquida_cofi = NULL
END IF
   
--******************************************************************************

          --DISPLAY "situacion : ",v_situacion

          IF v_situacion = 55 THEN
             LET v_marca = "10   ACREDITADO"
          END IF

          IF v_situacion = 60 THEN
             LET v_marca = "20   MARCADO INFONAVIT"
          END IF
          
          IF v_situacion = 70 THEN
             IF v_f_sol_mca_prcr IS NULL THEN
                 LET v_marca = "20   MARCADO INFONAVIT"
             ELSE
                LET v_marca = "30   EN PROCESO MARCA PROCESAR"
             END IF
          END IF
          
          IF v_situacion = 80 THEN
             LET v_marca = "40   MARCA CONFIRMADA PROCESAR"
          END IF
          
          IF v_situacion = 120 THEN
             LET v_marca = "VENCIDO"
          END IF
          
          IF v_situacion = 140 THEN
             LET v_marca = "50   LIQUIDADO ENTIDAD FINANCIERA"
          END IF
          
          IF v_situacion = 150 THEN
             IF v_f_sol_desm_prcr IS NULL THEN
                LET v_marca = "60   DESMARCADO INFONAVIT"
             ELSE
                LET v_marca = "EN PROCESO DESMARCA PROCESAR"
             END IF
          END IF
          
          IF v_situacion = 160 THEN
                LET v_marca = "70   DESMARCADO PROCESAR"
          END IF


          --DISPLAY "MARCA     : ",v_marca

          LET bnd_ant = 0
          CALL fn_respuesta()
          --EXIT DISPLAY

      ON ACTION CLOSE
         CALL fn_limpia_variables()
         EXIT DISPLAY
         
      END DISPLAY

      CLOSE WINDOW arreglo
END FUNCTION

FUNCTION fn_respuesta()

DEFINE bnd_acciones SMALLINT

   CALL f.setElementHidden("table1",1)

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   LET v_criterio = 0
   CALL fn_oculta1(v_criterio)

   --LET v_criterio = 1
   --CALL fn_oculta2(v_criterio)
   
    
   LET v_f_proceso = v_f_proceso [4,5],"-",v_f_proceso[1,2],"-",v_f_proceso[7,10]
   
   DISPLAY BY NAME          v_nss           ,
                            v_rfc           ,
                            v_curp          ,
                            v_ctr_ef        ,
                            v_paterno       ,
                            v_materno       ,
                            v_nombre        ,
                            v_bimestre      ,
                            v_97            ,
                            v_f_subcta      ,
                            v_ejercicio     ,
                            v_situacion     ,
                            v_ent_financiera,
                            v_f_proceso     ,
                            v_subproceso    ,
                            v_producto      ,
                            v_formalizacion ,
                            v_marca         ,
                            v_genero        ,
                            v_nss_asoc      ,
                            v_marca_asoc    ,
                            v_f_liquida_cofi,
                            v_f_credito     ,
                            v_causa_liquida

   MENU
   ON ACTION Siguiente

      LET v_criterio = 1
      LET v_bnd_nss  = 1
      CALL fn_oculta1(v_criterio)
      --CALL fn_formalizacion()
      LET v_criterio = 0
      CALL fn_oculta2(v_criterio)

      DISPLAY BY NAME v_nss            ,
                      v_notario        ,
                      v_ent_fed_notario,
                      v_edo_notario    ,
                      v_escritura      ,
                      v_rpp            ,
                      v_folio          ,
                      v_partida        ,
                      v_foja           ,
                      v_volumen        ,
                      v_libro          ,
                      v_tomo           ,
                      v_seccion        ,
                      v_ent_inmueble   ,
                      v_domicilio      ,
                      v_avaluo         ,
                      v_monto_credito  ,
                      v_plazo_credito  ,
                      v_moneda         ,
                      v_tasa           ,
                      v_margen         ,
                      v_oto_ef         ,
                      v_reg_carta      ,
                      v_usuario_reg    ,
                      v_mcpio_inmueble

      CALL fn_menu2()

      IF bnd_ant = 1 THEN
         EXIT MENU
      END IF

   ON ACTION CANCEL

   LET v_criterio = 1
   LET v_bnd_nss  = 0
      CALL fn_oculta2(v_criterio)

      LET v_criterio = 1
      CALL fn_oculta1(v_criterio)

      CALL f.setElementHidden("table1",0)
   EXIT MENU

   END MENU

END FUNCTION

FUNCTION fn_menu2()

   MENU
      ON ACTION Anterior

      LET v_criterio = 1
      LET v_bnd_nss  = 1
      CALL fn_oculta2(v_criterio)

      LET v_criterio = 0
      --CALL fn_tramite()
      CALL fn_oculta1(v_criterio)

      DISPLAY BY NAME v_nss           ,
                      v_rfc           ,
                      v_curp          ,
                      v_ctr_ef        ,
                      v_paterno       ,
                      v_materno       ,
                      v_nombre        ,
                      v_bimestre      ,
                      v_97            ,
                      v_f_subcta      ,
                      v_ejercicio     ,
                      v_situacion     ,
                      v_marca         ,
                      v_ent_financiera,
                      v_f_proceso     ,
                      v_subproceso    ,
                      v_producto      ,
                      v_formalizacion

      EXIT MENU

   ON ACTION Siguiente
      LET v_criterio = 1
      LET v_bnd_nss  = 1 
      CALL fn_oculta1(v_criterio)
      CALL fn_oculta2(v_criterio)
      LET v_criterio = 0
      CALL fn_oculta3(v_criterio)
      
      DISPLAY BY NAME v_f_sol_mca_prcr        
      DISPLAY BY NAME v_f_conf_mca_prcr 
      DISPLAY BY NAME v_f_sol_desm_prcr 
      DISPLAY BY NAME v_f_conf_desm_prcr
      
      CALL fn_menu3()

      IF bnd_ant = 1 THEN
         EXIT MENU
      END IF
      --EXIT MENU

   ON ACTION CANCEL

      LET v_criterio = 1
      LET v_bnd_nss  = 0
      CALL fn_oculta2(v_criterio)
      CALL fn_oculta1(v_criterio)

      CALL f.setElementHidden("table1",0)

      LET bnd_ant = 1
      EXIT MENU

 END MENU
 
END FUNCTION

FUNCTION fn_menu3()
     MENU
   ON ACTION Anterior

      LET v_criterio = 1
      LET v_bnd_nss  = 1
      CALL fn_oculta1(v_criterio)
      CALL fn_oculta3(v_criterio)
      --CALL fn_formalizacion()
      LET v_criterio = 0
      CALL fn_oculta2(v_criterio)

      DISPLAY BY NAME v_nss            ,
                      v_notario        ,
                      v_edo_notario    ,
                      v_escritura      ,
                      v_rpp            ,
                      v_folio          ,
                      v_partida        ,
                      v_foja           ,
                      v_volumen        ,
                      v_libro          ,
                      v_tomo           ,
                      v_seccion        ,
                      v_ent_inmueble   ,
                      v_domicilio      ,
                      v_avaluo         ,
                      v_monto_credito  ,
                      v_plazo_credito  ,
                      v_moneda         ,
                      v_tasa           ,
                      v_margen         ,
                      v_oto_ef         ,
                      v_reg_carta      ,
                      v_usuario_reg    ,
                      v_mcpio_inmueble
      EXIT MENU
      LET bnd_ant = 1

      CALL fn_menu2()

      -- IF bnd_ant = 1 THEN
      --   EXIT MENU
      -- END IF

   ON ACTION CANCEL

      LET v_criterio = 1
      LET v_bnd_nss  = 0
      CALL fn_oculta2(v_criterio)
      CALL fn_oculta1(v_criterio)
      CALL fn_oculta3(v_criterio)

      CALL f.setElementHidden("table1",0)
      LET bnd_ant = 1 
   EXIT MENU
   END MENU

END FUNCTION

FUNCTION fn_oculta1(v_criterio)

   DEFINE v_criterio SMALLINT

   CALL f.setElementHidden("label2",v_criterio)
   CALL f.setElementHidden("label3",v_criterio)
   CALL f.setElementHidden("label4",v_criterio)
   CALL f.setElementHidden("label5",v_criterio)
   CALL f.setElementHidden("label6",v_criterio)
   CALL f.setElementHidden("label7",v_criterio)
   CALL f.setElementHidden("label8",v_criterio)
   CALL f.setElementHidden("label9",v_criterio)
   CALL f.setElementHidden("label10",v_criterio)
   CALL f.setElementHidden("label11",v_criterio)
   CALL f.setElementHidden("label12",v_criterio)
   CALL f.setElementHidden("label13",v_criterio)
   CALL f.setElementHidden("label14",v_criterio)
   CALL f.setElementHidden("label15",v_criterio)
   CALL f.setElementHidden("label16",v_criterio)
   CALL f.setElementHidden("label17",v_criterio)
   CALL f.setElementHidden("label18",v_criterio)
   CALL f.setElementHidden("label19",v_criterio)
   CALL f.setElementHidden("label20",v_criterio)
   CALL f.setElementHidden("label21",v_criterio)
   CALL f.setElementHidden("label77",v_criterio)
   CALL f.setElementHidden("label22",v_criterio)
   CALL f.setElementHidden("label23",v_criterio)
   CALL f.setElementHidden("label79",v_criterio)
   CALL f.setFieldHidden("v_rfc",v_criterio)
   CALL f.setFieldHidden("v_curp",v_criterio)
   CALL f.setFieldHidden("v_ctr_ef",v_criterio)
   CALL f.setFieldHidden("v_nss_asoc",v_criterio)
   CALL f.setFieldHidden("v_paterno",v_criterio)
   CALL f.setFieldHidden("v_materno",v_criterio)
   CALL f.setFieldHidden("v_nombre",v_criterio)
   CALL f.setFieldHidden("v_bimestre",v_criterio)
   CALL f.setFieldHidden("v_marca",v_criterio)
   CALL f.setFieldHidden("v_97",v_criterio)
   CALL f.setFieldHidden("v_f_credito",v_criterio)
   CALL f.setFieldHidden("v_f_subcta",v_criterio)
   CALL f.setFieldHidden("v_formalizacion",v_criterio)
   CALL f.setFieldHidden("v_ejercicio",v_criterio)
   CALL f.setFieldHidden("v_f_proceso",v_criterio)
   CALL f.setFieldHidden("v_situacion",v_criterio)
   CALL f.setFieldHidden("v_subproceso",v_criterio)
   CALL f.setFieldHidden("v_ent_financiera",v_criterio)
   CALL f.setFieldHidden("v_producto",v_criterio)
   CALL f.setFieldHidden("v_genero",v_criterio)
   CALL f.setFieldHidden("v_causa_liquida",v_criterio)
   CALL f.setFieldHidden("v_f_liquida_cofi",v_criterio)
   CALL f.setFieldHidden("v_nss_asoc",v_criterio)
   CALL f.setFieldHidden("v_marca_asoc",v_criterio)

   IF v_bnd_nss  = 1 THEN
      CALL f.setElementHidden("label1",0)
      CALL f.setFieldHidden("v_nss",0)
   ELSE
      CALL f.setElementHidden("label1",v_criterio)
      CALL f.setFieldHidden("v_nss",v_criterio)
   END IF


END FUNCTION

FUNCTION fn_oculta2(v_criterio)

   DEFINE v_criterio SMALLINT

   CALL f.setElementHidden("label24",v_criterio)
   CALL f.setElementHidden("label25",v_criterio)
   CALL f.setElementHidden("label26",v_criterio)
   CALL f.setElementHidden("label27",v_criterio)
   CALL f.setElementHidden("label28",v_criterio)
   CALL f.setElementHidden("label29",v_criterio)
   CALL f.setElementHidden("label30",v_criterio)
   CALL f.setElementHidden("label31",v_criterio)
   CALL f.setElementHidden("label32",v_criterio)
   CALL f.setElementHidden("label33",v_criterio)
   CALL f.setElementHidden("label34",v_criterio)
   CALL f.setElementHidden("label35",v_criterio)
   CALL f.setElementHidden("label36",v_criterio)
   CALL f.setElementHidden("label37",v_criterio)
   CALL f.setElementHidden("label38",v_criterio)
   CALL f.setElementHidden("label39",v_criterio)
   CALL f.setElementHidden("label40",v_criterio)
   CALL f.setElementHidden("label41",v_criterio)
   CALL f.setElementHidden("label42",v_criterio)
   CALL f.setElementHidden("label43",v_criterio)
   CALL f.setElementHidden("label44",v_criterio)
   CALL f.setElementHidden("label45",v_criterio)
   CALL f.setElementHidden("label46",v_criterio)
   CALL f.setElementHidden("label63",v_criterio)
   CALL f.setElementHidden("label78",v_criterio)
   CALL f.setFieldHidden("v_notario",v_criterio)
   CALL f.setFieldHidden("v_ent_fed_notario",v_criterio)
   CALL f.setFieldHidden("v_edo_notario",v_criterio)
   CALL f.setFieldHidden("v_escritura",v_criterio)
   CALL f.setFieldHidden("v_rpp",v_criterio)
   CALL f.setFieldHidden("v_folio",v_criterio)
   CALL f.setFieldHidden("v_partida",v_criterio)
   CALL f.setFieldHidden("v_foja",v_criterio)
   CALL f.setFieldHidden("v_volumen",v_criterio)
   CALL f.setFieldHidden("v_libro",v_criterio)
   CALL f.setFieldHidden("v_tomo",v_criterio)
   CALL f.setFieldHidden("v_seccion",v_criterio)
   CALL f.setFieldHidden("v_ent_inmueble",v_criterio)
   CALL f.setFieldHidden("v_domicilio",v_criterio)
   CALL f.setFieldHidden("v_avaluo",v_criterio)
   CALL f.setFieldHidden("v_monto_credito",v_criterio)
   CALL f.setFieldHidden("v_plazo_credito",v_criterio)
   CALL f.setFieldHidden("v_moneda",v_criterio)
   CALL f.setFieldHidden("v_tasa",v_criterio)
   CALL f.setFieldHidden("v_margen",v_criterio)
   CALL f.setFieldHidden("v_oto_ef",v_criterio)
   CALL f.setFieldHidden("v_reg_carta",v_criterio)
   CALL f.setFieldHidden("v_usuario_reg",v_criterio)
   CALL f.setFieldHidden("v_mcpio_inmueble",v_criterio)

END FUNCTION

FUNCTION fn_oculta3(v_criterio)

   DEFINE v_criterio SMALLINT

   CALL f.setElementHidden("label48",v_criterio)
   CALL f.setElementHidden("label49",v_criterio)
   CALL f.setElementHidden("label50",v_criterio)
   CALL f.setElementHidden("label51",v_criterio)
   CALL f.setElementHidden("label52",v_criterio)
   CALL f.setElementHidden("label53",v_criterio)
   CALL f.setElementHidden("label54",v_criterio)
   CALL f.setElementHidden("label55",v_criterio)
   CALL f.setElementHidden("label56",v_criterio)
   CALL f.setElementHidden("label57",v_criterio)
   CALL f.setElementHidden("label58",v_criterio)
   CALL f.setElementHidden("label59",v_criterio)
   CALL f.setElementHidden("label60",v_criterio)
   CALL f.setElementHidden("label61",v_criterio)
   CALL f.setElementHidden("label62",v_criterio)
   --CALL f.setElementHidden("label63",v_criterio)
   CALL f.setElementHidden("label64",v_criterio)
   CALL f.setElementHidden("label65",v_criterio)
   CALL f.setElementHidden("label66",v_criterio)
   CALL f.setElementHidden("label67",v_criterio)
   CALL f.setElementHidden("label68",v_criterio)
   CALL f.setElementHidden("label69",v_criterio)
   CALL f.setElementHidden("label70",v_criterio)
   CALL f.setElementHidden("label71",v_criterio)
   CALL f.setElementHidden("label72",v_criterio)
   CALL f.setElementHidden("label73",v_criterio)
   CALL f.setElementHidden("label74",v_criterio)
   CALL f.setElementHidden("label75",v_criterio)
   CALL f.setElementHidden("label76",v_criterio)

   CALL f.setFieldHidden("edit9",v_criterio)
   CALL f.setFieldHidden("edit10",v_criterio)
   CALL f.setFieldHidden("edit11",v_criterio)
   CALL f.setFieldHidden("edit12",v_criterio)
   CALL f.setFieldHidden("edit13",v_criterio)
   CALL f.setFieldHidden("edit14",v_criterio)
   CALL f.setFieldHidden("edit15",v_criterio)
   CALL f.setFieldHidden("edit16",v_criterio)
   CALL f.setFieldHidden("edit17",v_criterio)
   CALL f.setFieldHidden("edit18",v_criterio)
   CALL f.setFieldHidden("edit19",v_criterio)
   CALL f.setFieldHidden("v_f_sol_mca_prcr",v_criterio)
   CALL f.setFieldHidden("v_f_conf_mca_prcr",v_criterio)
   CALL f.setFieldHidden("edit22",v_criterio)
   CALL f.setFieldHidden("edit23",v_criterio)
   CALL f.setFieldHidden("edit24",v_criterio)
   CALL f.setFieldHidden("v_f_sol_desm_prcr",v_criterio)
   CALL f.setFieldHidden("v_f_conf_desm_prcr",v_criterio)
   CALL f.setFieldHidden("v_causa",v_criterio)
   CALL f.setFieldHidden("edit28",v_criterio)
   CALL f.setFieldHidden("edit29",v_criterio)
   CALL f.setFieldHidden("edit30",v_criterio)
   CALL f.setFieldHidden("edit31",v_criterio)
   CALL f.setFieldHidden("edit32",v_criterio)

END FUNCTION

FUNCTION fn_limpia_variables()
LET v_nss                = NULL
LET v_rfc                = NULL
LET v_curp               = NULL
LET v_ctr_ef             = NULL
LET v_nss_asoc           = NULL
LET v_paterno            = NULL
LET v_materno            = NULL
LET v_nombre             = NULL
LET v_bimestre           = NULL
LET v_marca              = NULL
LET v_97                 = NULL
LET v_f_credito          = NULL
LET v_f_subcta           = NULL
LET v_formalizacion      = NULL
LET v_ejercicio          = NULL
LET v_f_proceso          = NULL
LET v_situacion          = NULL
LET v_subproceso         = NULL
LET v_ent_financiera     = NULL
LET v_producto           = NULL
LET v_genero             = NULL
LET v_causa_liquida      = NULL
LET v_f_liquida_cofi     = NULL
LET v_nss_asoc           = NULL
LET v_marca_asoc         = NULL
LET v_notario            = NULL
LET v_ent_fed_notario    = NULL
LET v_edo_notario        = NULL
LET v_escritura          = NULL
LET v_rpp                = NULL
LET v_folio              = NULL
LET v_partida            = NULL
LET v_foja               = NULL
LET v_volumen            = NULL
LET v_libro              = NULL
LET v_tomo               = NULL
LET v_seccion            = NULL
LET v_ent_inmueble       = NULL
LET v_domicilio          = NULL
LET v_avaluo             = NULL
LET v_monto_credito      = NULL
LET v_plazo_credito      = NULL
LET v_moneda             = NULL
LET v_tasa               = NULL
LET v_margen             = NULL
LET v_oto_ef             = NULL
LET v_reg_carta          = NULL
LET v_usuario_reg        = NULL
LET v_mcpio_inmueble     = NULL
LET v_f_sol_mca_prcr     = NULL
LET v_f_conf_mca_prcr    = NULL
LET v_f_sol_desm_prcr    = NULL
LET v_f_conf_desm_prcr   = NULL

   
   CALL arr_tramite.clear()
   CALL arr_formalizacion.clear()
   CALL arr_tabla.clear()
   CALL arr_detalle.clear()
   CALL arr_liquidacion.clear()
END FUNCTION
