#######################################################################
#Modulo              => GRT                                           #
#Programa            => GRTC107                                       #
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

   DEFINE v_nss               CHAR(11)
   DEFINE v_rfc               CHAR (13)
   DEFINE v_curp              CHAR (18)
   DEFINE v_ctr_ef            CHAR (18)
   DEFINE v_nss_asoc          CHAR (11)
   DEFINE v_paterno           CHAR (40)
   DEFINE v_materno           CHAR (40)
   DEFINE v_nombre            CHAR (40)
   DEFINE v_bimestre          SMALLINT
   DEFINE v_marca             CHAR (3)
   DEFINE v_97                DECIMAL(12,2)
   DEFINE v_f_credito         DATE
   DEFINE v_f_subcta          DATE
   DEFINE v_formalizacion     DATE
   DEFINE v_ejercicio         CHAR (4)
   DEFINE v_f_proceso         DATE
   DEFINE v_situacion         CHAR (3)
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
   DEFINE v_avaluo            DECIMAL(15,0)
   DEFINE v_monto_credito     DECIMAL(15,0)
   DEFINE v_plazo_credito     DECIMAL(5,0)
   DEFINE v_moneda            SMALLINT
   DEFINE v_tasa              CHAR (20)
   DEFINE v_margen            CHAR (20)
   DEFINE v_oto_ef            DATE
   DEFINE v_reg_carta         DATE
   DEFINE v_usuario_reg       CHAR (20)
                              
   DEFINE v_criterio          SMALLINT
   DEFINE bnd_qry             SMALLINT
                              
   DEFINE p_nss               CHAR(11)
   DEFINE p_subproceso        CHAR(3)
   DEFINE p_ent_financiera    CHAR(3)
   DEFINE p_f_proceso         DATE
   DEFINE p_situacion         CHAR(2)
   DEFINE p_producto          CHAR(10)
   DEFINE a                   SMALLINT
   DEFINE v_nom               STRING
   DEFINE bnd_datos           SMALLINT
   DEFINE bnd_ant             SMALLINT
   DEFINE v_igual             CHAR(3)
   DEFINE v_comilla           CHAR(1)

   DEFINE arr_detalle DYNAMIC ARRAY OF RECORD
          id_grt_detalle      DECIMAL(9,0),
          id_derechohabiente  DECIMAL(9,0),
          subproceso          CHAR(3),
          f_proceso           DATE,
          cve_ent_financiera  SMALLINT,
          nss                 CHAR(11)
   END RECORD

     DEFINE arr_tramite DYNAMIC ARRAY OF RECORD
          id_grt_tramite      DECIMAL(9,0),
          id_grt_detalle      DECIMAL(9,0),
          rfc                 CHAR (13)    ,
          curp                CHAR (18)    ,
          cve_ef              CHAR (18)    ,
          paterno             CHAR (40)    ,
          materno             CHAR (40)    ,
          nombre              CHAR (40)    ,
          bimestre            SMALLINT     ,
          viv97               DECIMAL(12,2),
          f_credito           DATE         ,
          f_subcta            CHAR(4)
   END RECORD

   DEFINE arr_formalizacion DYNAMIC ARRAY OF RECORD
          id_detalle          DECIMAL(9,0),
          id_tramite          DECIMAL(9,0),
          notario             DECIMAL(4,0)  ,
          edo_notario         SMALLINT      ,
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
          avaluo              DECIMAL(15,0) ,
          monto_credito       DECIMAL(15,0) ,
          plazo_credito       DECIMAL(5,0)  ,
          moneda              SMALLINT      ,
          tasa                CHAR (20)     ,
          margen              CHAR (20)     ,
          oto_ef              DATE          ,
          reg_carta           DATE          ,
          usuario_reg         CHAR (20)     ,
          diagnostico         CHAR(3)       ,
          estado              CHAR(3)
   END RECORD

   DEFINE arr_tabla DYNAMIC ARRAY OF RECORD
          situacion           CHAR(3),
          subproceso          CHAR(3),
          ent_financiera      CHAR(3),
          diagnostico         CHAR(3),
          f_proceso           DATE,
          nss                 CHAR(11),
          rfc                 CHAR(13),
          nombre              CHAR(40)
   END RECORD

END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".GRTC107.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   CALL fn_consulta_gral()

END MAIN

FUNCTION fn_consulta_gral()

   DEFINE p_busqueda          STRING
   --DEFINE v_pos INTEGER

   OPEN WINDOW consulta WITH FORM "GRTC1072"

   DIALOG ATTRIBUTES (UNBUFFERED)

      CONSTRUCT p_busqueda ON g.nss,
                              g.subproceso,
                              g.cve_ent_financiera,
                              g.f_proceso,
                              fz.situacion,
                              t.tpo_credito
                         FROM p_nss,
                              p_subproceso,
                              p_ent_financiera,
                              p_f_proceso,
                              p_situacion,
                              p_producto

      ON ACTION ACCEPT

      DISPLAY "parametro de busqueda inicial : ", p_busqueda

      CALL fn_limpia_variables()

         LET v_nss            = GET_FLDBUF(p_nss)
         LET v_subproceso     = GET_FLDBUF(p_subproceso)
         LET v_ent_financiera = GET_FLDBUF(p_ent_financiera)
         LET v_f_proceso      = GET_FLDBUF(p_f_proceso)
         LET v_situacion      = GET_FLDBUF(p_situacion)
         LET v_producto       = GET_FLDBUF(p_producto)

         --LET v_subproceso = v_subproceso USING "&&&"
         --LET p_situacion  = p_situacion  USING "&&&"

         IF (v_nss            IS NULL) AND
            (v_subproceso     IS NULL) AND
            (v_ent_financiera IS NULL) AND
            (v_f_proceso      IS NULL) AND
            (v_situacion      IS NULL) AND
            (v_producto       IS NULL) THEN
            CALL fn_mensaje ("Archivo","Debe ingresar al menos un parametro de búsqueda","information")
         ELSE

         IF (v_situacion IS NULL) AND
            (v_producto  IS NULL) THEN
            IF
            (v_nss            IS NOT NULL) OR
            (v_subproceso     IS NOT NULL) OR
            (v_ent_financiera IS NOT NULL) OR
            (v_f_proceso      IS NOT NULL) THEN

            --DISPLAY "2 prueba:",v_subproceso

            IF (v_subproceso IS NOT NULL) AND (LENGTH (v_subproceso) <> 3) THEN 
               CALL fn_mensaje ("Archivo","El proceso debe ser ingresado a 3 dígitos","information")
            ELSE

               --IF (v_subproceso IS NULL ) OR (v_subproceso = "002") THEN
               --IF (v_subproceso = "002") THEN
               LET a = 1
               LET p_busqueda = p_busqueda,
               " AND g.id_grt_detalle in (select id_grt_detalle from grt_formalizacion where estado in (60,140))" 

               CALL fn_detalle(p_busqueda,a)

             --  IF arr_detalle[arr_detalle.getLength()].id_grt_detalle IS NULL THEN
               --   CALL arr_detalle.deleteElement(arr_detalle.getLength())
              -- END IF

               IF arr_detalle.getLength()  > = 1 THEN
                  FOR a = 1 TO arr_detalle.getLength()
                     LET p_busqueda = "fz.id_grt_detalle = ",arr_detalle[a].id_grt_detalle ," ","AND fz.estado IN (60,140)"
                     DISPLAY "detalle",arr_detalle[a].*
                     CALL fn_formalizacion(p_busqueda,a)
                     ---DISPLAY "formalizacion : ",arr_formalizacion[a].*
                     IF arr_formalizacion[a].id_tramite IS NOT NULL THEN
                        LET p_busqueda = "t.id_grt_tramite = ",arr_formalizacion[a].id_tramite," "
                        CALL fn_tramite(p_busqueda,a)
                        --DISPLAY "tramite ",arr_tramite[a].*
                     END IF
                     CALL fn_arreglo(a)
                  END FOR
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
            END IF
         END IF
--***************************************************************************************************
         IF (v_nss            IS NULL) AND
            (v_subproceso     IS NULL) AND
            (v_ent_financiera IS NULL) AND
            (v_f_proceso      IS NULL) AND
            (v_situacion      IS NULL) AND
            (v_producto       IS NOT NULL) THEN

            IF (v_producto = "AI" ) THEN
               LET p_busqueda = "t.tpo_credito in ('A','I')"
            END IF

            IF (v_producto = "CO" ) THEN
               LET p_busqueda = "t.tpo_credito in (7,8)"
            END IF

            LET a = 1
            LET p_busqueda = p_busqueda,"AND t.id_grt_tramite in
                                         (select id_grt_tramite 
                                            FROM grt_formalizacion
                                            WHERE estado in (60,140))"
            CALL fn_tramite(p_busqueda,a)
            --DISPLAY "tramite ",arr_tramite[a].*

            IF arr_tramite.getLength() > = 1 THEN
               --IF arr_tramite[arr_tramite.getLength()].id_grt_tramite IS NULL THEN
              --    CALL arr_tramite.deleteElement(arr_tramite.getLength())
               --END IF
               FOR a = 1 TO arr_tramite.getLength()
                  IF arr_tramite[a].id_grt_tramite IS NOT NULL THEN
                     LET p_busqueda = "fz.id_grt_tramite = ",arr_tramite[a].id_grt_tramite," ","AND fz.estado IN (60,140)"
                     CALL fn_formalizacion(p_busqueda,a)
                     --DISPLAY "formalizacion ",arr_detalle[a].*
                     IF arr_formalizacion[a].id_detalle IS NOT NULL THEN
                        LET p_busqueda = "g.id_grt_detalle = ",arr_formalizacion[a].id_detalle," "
                        CALL fn_detalle(p_busqueda,a)
                        --DISPLAY "formalizacion : ",arr_formalizacion[a].*
                     END IF
                  END IF
                  CALL fn_arreglo(a)
               END FOR
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

               --DISPLAY "detalle y tramite"

               IF v_nss IS NOT NULL THEN
                   LET p_busqueda = "g.nss = ","'",v_nss,"'"
                END IF

                IF v_subproceso IS NOT NULL THEN
                   LET p_busqueda = "g.subproceso = ","'",v_subproceso,"'"
                END IF

                IF v_ent_financiera IS NOT NULL THEN
                   LET p_busqueda = "g.cve_ent_financiera = ","'",v_ent_financiera,"'"
                END IF

                IF v_f_proceso IS NOT NULL THEN
                   LET p_busqueda = "g.f_proceso = ","'",v_f_proceso,"'"
                END IF

                LET v_igual = " = "
                LET v_comilla = "'"

                IF (v_producto = "AI" ) THEN
                   LET v_producto = "in('A','I')"
                   LET v_igual = " "
                   LET v_comilla = " "
                END IF

                IF (v_producto = "CO" ) THEN
                   LET v_producto = "in(7,8)"
                   LET v_igual = " "
                   LET v_comilla = " "
                END IF

                --DISPLAY "producto : ",v_producto
                --DISPLAY "comilla : ",v_comilla
                --DISPLAY "igual : ",v_igual

                LET a = 1
                LET p_busqueda = p_busqueda,"AND g.id_grt_detalle in
                                             (select id_grt_detalle
                                                FROM grt_formalizacion
                                               WHERE estado in (60,140)
                                                 AND id_grt_tramite in
                                                     (select id_grt_tramite
                                                        from grt_tramite
                                                       where tpo_credito ",v_igual,v_comilla,v_producto CLIPPED,v_comilla,"))"
                CALL fn_detalle(p_busqueda,a)
                --DISPLAY "detalle ",arr_detalle[a].*

                --IF arr_detalle[arr_detalle.getLength()].id_grt_detalle IS NULL THEN
                --   CALL arr_detalle.deleteElement(arr_detalle.getLength())
               -- END IF

                IF arr_detalle.getLength() > = 1 THEN

                   FOR a = 1 TO arr_detalle.getLength()
                      IF arr_detalle[a].id_grt_detalle IS NOT NULL THEN
                         LET p_busqueda = "fz.id_grt_detalle = ",arr_detalle[a].id_grt_detalle," ","AND fz.estado IN (60,140)"
                         CALL fn_formalizacion(p_busqueda,a)
                        -- DISPLAY "formalizacion ", arr_formalizacion[a].*
                         IF arr_formalizacion[a].id_tramite IS NOT NULL THEN
                            LET p_busqueda = "t.id_grt_tramite = ",arr_formalizacion[a].id_tramite
                            CALL fn_tramite(p_busqueda,a)
                            --DISPLAY "tramite ",arr_tramite[a].*
                         END IF
                      END IF
                      CALL fn_arreglo(a)
                   END FOR
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
         END IF
--***************************************************************************************************
         IF (v_nss            IS NULL) AND
            (v_subproceso     IS NULL) AND
            (v_ent_financiera IS NULL) AND
            (v_f_proceso      IS NULL) AND
            (v_producto       IS NULL) AND
            (v_situacion      IS NOT NULL) THEN

            --DISPLAY "busqueda ",p_busqueda
            --DISPLAY "situacion o estado ", v_situacion

            LET a = 1
            LET p_busqueda = "fz.estado = ","'",v_situacion,"'"
            CALL fn_formalizacion(p_busqueda,a)
            --DISPLAY "formalizacion  ",arr_formalizacion[a].*

            --IF arr_formalizacion[arr_formalizacion.getLength()].id_tramite IS NULL THEN
              --   CALL arr_formalizacion.deleteElement(arr_formalizacion.getLength())
            --END IF

            IF arr_formalizacion.getLength() > = 1 THEN

               FOR a = 1 TO arr_formalizacion.getLength()
                  IF arr_formalizacion[a].id_detalle IS NOT NULL THEN
                     LET p_busqueda = "g.id_grt_detalle = ",arr_formalizacion[a].id_detalle," "
                     CALL fn_detalle(p_busqueda,a)
                     --DISPLAY "detalle ",arr_detalle[a].*
                     IF arr_formalizacion[a].id_detalle IS NOT NULL THEN
                        LET p_busqueda = "t.id_grt_tramite = ",arr_formalizacion[a].id_tramite," "
                        CALL fn_tramite(p_busqueda,a)
                        --DISPLAY "tramite : ",arr_tramite[a].*
                     END IF
                  END IF
                  CALL fn_arreglo(a)
               END FOR
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
--**************************************************************************************************
        IF (v_situacion IS NOT NULL) AND
            (v_producto IS NULL) THEN
            IF (v_nss            IS NOT NULL) OR
               (v_subproceso     IS NOT NULL) OR
               (v_ent_financiera IS NOT NULL) OR
               (v_f_proceso      IS NOT NULL) THEN

                --DISPLAY "detalle y formalizacion"

               IF v_nss IS NOT NULL THEN
                   LET p_busqueda = "g.nss = ","'",v_nss,"'"
                END IF

                IF v_subproceso IS NOT NULL THEN
                   LET p_busqueda = "g.subproceso = ","'",v_subproceso,"'"
                END IF

                IF v_ent_financiera IS NOT NULL THEN
                   LET p_busqueda = "g.cve_ent_financiera = ","'",v_ent_financiera,"'"
                END IF

                IF v_f_proceso IS NOT NULL THEN
                   LET p_busqueda = "g.f_proceso = ","'",v_f_proceso,"'"
                END IF

                LET a = 1
                LET p_busqueda = p_busqueda,"AND g.id_grt_detalle in
                                             (select id_grt_detalle
                                                FROM grt_formalizacion
                                               WHERE estado in (60,140)
                                                 AND id_grt_tramite in
                                                     (select id_grt_tramite
                                                        from grt_tramite)
                                                 AND estado = ",v_situacion,")"

                CALL fn_detalle(p_busqueda,a)
                --DISPLAY "detalle ",arr_detalle[a].*

                --IF arr_detalle[arr_detalle.getLength()].id_grt_detalle IS NULL THEN
                  -- CALL arr_detalle.deleteElement(arr_detalle.getLength())
                --END IF

                IF arr_detalle.getLength() > = 1 THEN
                   FOR a = 1 TO arr_detalle.getLength()
                      IF arr_detalle[a].id_grt_detalle IS NOT NULL THEN
                         LET p_busqueda = "fz.id_grt_detalle = ",arr_detalle[a].id_grt_detalle," ","AND fz.estado IN (60,140)"
                         CALL fn_formalizacion(p_busqueda,a)
                         --DISPLAY "formalizacion ", arr_formalizacion[a].*
                         IF arr_formalizacion[a].id_tramite IS NOT NULL THEN
                            LET p_busqueda = "t.id_grt_tramite = ",arr_formalizacion[a].id_tramite
                            CALL fn_tramite(p_busqueda,a)
                            --DISPLAY "tramite ",arr_tramite[a].*
                         END IF
                      END IF
                      CALL fn_arreglo(a)
                   END FOR
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
         END IF
--**************************************************************************************************
         IF (v_nss            IS NULL) AND
            (v_subproceso     IS NULL) AND
            (v_ent_financiera IS NULL) AND
            (v_f_proceso      IS NULL) AND 
            (v_situacion      IS NOT NULL) AND
            (v_producto       IS NOT NULL)THEN

            LET a = 1
            --LET p_busqueda = "fz.estado = ",v_situacion
            LET p_busqueda = "fz.estado = ",v_situacion," and fz.id_grt_tramite in (select id_grt_tramite from grt_tramite where tpo_credito = ","'",v_producto CLIPPED,"'",")"

            IF (v_producto = "AI" ) THEN
               LET p_busqueda ="fz.estado = ",v_situacion," and fz.id_grt_tramite in (select id_grt_tramite from grt_tramite where tpo_credito in ('A','I')",")"
            END IF

            IF (v_producto = "CO" ) THEN
               LET p_busqueda = "fz.estado = ",v_situacion," and fz.id_grt_tramite in (select id_grt_tramite from grt_tramite where tpo_credito in (7,8)",")"
            END IF

            CALL fn_formalizacion(p_busqueda,a)
           -- DISPLAY "formalizacion ",arr_formalizacion[a].*
            
           --- IF arr_formalizacion[arr_formalizacion.getLength()].id_detalle IS NULL THEN
              --- CALL arr_formalizacion.deleteElement(arr_formalizacion.getLength())
            ---END IF

            IF arr_formalizacion.getLength() > = 1 THEN
               FOR a = 1 TO arr_formalizacion.getLength()
                   IF arr_formalizacion[a].id_detalle IS NOT NULL THEN
                      LET p_busqueda = "g.id_grt_detalle = ",arr_formalizacion[a].id_detalle," "
                      CALL fn_detalle(p_busqueda,a)
                      --DISPLAY "detalle ", arr_detalle[a].*
                      IF arr_formalizacion[a].id_tramite IS NOT NULL THEN
                         LET p_busqueda = "t.id_grt_tramite = ",arr_formalizacion[a].id_tramite
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
                   LET p_busqueda = "g.subproceso = ","'",v_subproceso,"'"
                END IF

                IF v_ent_financiera IS NOT NULL THEN
                   LET p_busqueda = "g.cve_ent_financiera = ","'",v_ent_financiera,"'"
                END IF

                IF v_f_proceso IS NOT NULL THEN
                   LET p_busqueda = "g.f_proceso = ","'",v_f_proceso,"'"
                END IF

                LET v_igual = " = "
                LET v_comilla = "'"

                IF (v_producto = "AI" ) THEN
                   LET v_producto = "in('A','I')"
                   LET v_igual = " "
                   LET v_comilla = " "
                END IF

                IF (v_producto = "CO" ) THEN
                   LET v_producto = "in(7,8)"
                   LET v_igual = " "
                   LET v_comilla = " "
                END IF

                LET a = 1
                LET p_busqueda = p_busqueda,"AND g.id_grt_detalle in
                                             (select id_grt_detalle
                                                FROM grt_formalizacion
                                               WHERE estado in (60,140)
                                                 AND id_grt_tramite in
                                                     (select id_grt_tramite
                                                        from grt_tramite
                                                       where tpo_credito ",v_igual,v_comilla,v_producto CLIPPED,v_comilla, " )
                                                 AND estado = ",v_situacion,")"

                CALL fn_detalle(p_busqueda,a)
                --DISPLAY "detalle ",arr_detalle[a].*
                
                --IF arr_detalle[arr_detalle.getLength()].id_grt_detalle IS NULL THEN
                  -- CALL arr_detalle.deleteElement(arr_detalle.getLength())
                --END IF

                IF arr_detalle.getLength() > = 1 THEN
                   FOR a = 1 TO arr_detalle.getLength()
                      IF arr_detalle[a].id_grt_detalle IS NOT NULL THEN
                         LET p_busqueda = "fz.id_grt_detalle = ",arr_detalle[a].id_grt_detalle," ","AND fz.estado IN (60,140)"
                         CALL fn_formalizacion(p_busqueda,a)
                         --DISPLAY "formalizacion ", arr_formalizacion[a].*
                         IF arr_formalizacion[a].id_tramite IS NOT NULL THEN
                            LET p_busqueda = "t.id_grt_tramite = ",arr_formalizacion[a].id_tramite
                            CALL fn_tramite(p_busqueda,a)
                            --DISPLAY "tramite ",arr_tramite[a].*
                         END IF
                      END IF
                      CALL fn_arreglo(a)
                   END FOR
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
         END IF
--**************************************************************************************************
         IF bnd_datos = 1 THEN
            CALL fn_despliega_datos()

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
      EXIT DIALOG
   END CONSTRUCT
END DIALOG
CLOSE WINDOW consulta

END FUNCTION

FUNCTION fn_detalle(p_busqueda,a)

   DEFINE v_qry_detalle STRING
   DEFINE p_busqueda STRING
   DEFINE a INTEGER

   LET v_qry_detalle = "SELECT g.id_grt_detalle,
                                       g.id_derechohabiente,
                                       g.subproceso,
                                       g.f_proceso,
                                       g.cve_ent_financiera,
                                       g.nss
                                  FROM grt_detalle g
                                 WHERE ",p_busqueda,"
                                   AND subproceso = 2"
   DISPLAY "consulta detalle",v_qry_detalle

   PREPARE prp_detalle FROM v_qry_detalle
   DECLARE cur_detalle CURSOR FOR prp_detalle
   
   FOREACH cur_detalle INTO arr_detalle[a].*
      LET a = a+1
   END FOREACH
   
   CALL arr_detalle.deleteElement(arr_detalle.getLength())
END FUNCTION

FUNCTION fn_tramite(p_busqueda,a)

   DEFINE p_busqueda STRING
   DEFINE v_qry_tramite STRING
   DEFINE a INTEGER

   LET v_qry_tramite = 
     "SELECT t.id_grt_tramite,
             t.id_grt_detalle,
             t.rfc,
             t.curp,
             t.cve_ent_financiera,
             t.ap_paterno,
             t.ap_materno,
             t.nombre,
             t.num_bimestres,
             t.viv97,
             t.f_saldo,
             YEAR(t.f_saldo)
       FROM grt_tramite t
      WHERE ",p_busqueda

   --DISPLAY v_qry_tramite
   PREPARE prp_tramite FROM v_qry_tramite
   DECLARE cur_tramite CURSOR FOR prp_tramite

   FOREACH cur_tramite INTO arr_tramite[a].*
      --DISPLAY arr_tramite[a].*
      LET a = a+1
   END FOREACH

   CALL arr_tramite.deleteElement(arr_tramite.getLength())
END FUNCTION

FUNCTION fn_formalizacion(p_busqueda,a)

   DEFINE p_busqueda STRING
   DEFINE v_qry_formalizacion STRING
   DEFINE a INTEGER

   LET v_qry_formalizacion = 

  " SELECT fz.id_grt_detalle,
           fz.id_grt_tramite,
           fz.notario,           
           fz.mcpio_notario,     
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
           fz.estado
      FROM grt_formalizacion fz
     WHERE ",p_busqueda--,

     --DISPLAY "consulta formalizacion : ",v_qry_formalizacion
      -- "AND fz.estado IN (60,140)"

   PREPARE prp_formalizacion FROM v_qry_formalizacion
   DECLARE cur_formalizacion CURSOR FOR prp_formalizacion

   FOREACH cur_formalizacion INTO arr_formalizacion[a].*
      LET a = a+1
   END FOREACH

   CALL arr_formalizacion.deleteElement(arr_formalizacion.getLength())

END FUNCTION

FUNCTION fn_arreglo(a)

   DEFINE a SMALLINT

   LET arr_tabla[a].diagnostico      = arr_formalizacion[a].diagnostico USING "&&&"
   LET arr_tabla[a].ent_financiera   = arr_detalle[a].cve_ent_financiera USING "&&&"
   LET arr_tabla[a].f_proceso        = arr_detalle[a].f_proceso
   LET v_nom = arr_tramite[a].paterno CLIPPED," ",
               arr_tramite[a].materno CLIPPED," ",
               arr_tramite[a].nombre  CLIPPED
   LET arr_tabla[a].nombre           = v_nom
   LET arr_tabla[a].nss              = arr_detalle[a].nss
   LET arr_tabla[a].rfc              = arr_tramite[a].rfc
   LET arr_tabla[a].situacion        = arr_formalizacion[a].estado USING "&&&"
   LET arr_tabla[a].subproceso       = arr_detalle[a].subproceso
END FUNCTION

FUNCTION fn_despliega_datos()

    DEFINE v_pos INTEGER

    --DISPLAY "cantidades"
    --DISPLAY arr_detalle.getLength()
   -- DISPLAY arr_formalizacion.getLength()
   -- DISPLAY arr_tramite.getLength()

    OPEN WINDOW arreglo WITH FORM "GRTC1071"
       LET w = ui.Window.getCurrent()
       LET f = w.getForm()

       CALL fn_oculta3(1)

       LET v_criterio = 1
       CALL fn_oculta1(v_criterio)
       LET v_criterio = 1
       CALL fn_oculta2(v_criterio)
       DISPLAY ARRAY arr_tabla TO tab_detalle.* ATTRIBUTE (ACCEPT = TRUE, CANCEL=  FALSE)
       
       ON ACTION ACCEPT 
          LET v_pos = arr_curr()
          DISPLAY "pos arreglo : ",v_pos

          LET v_nss            = arr_detalle[v_pos].nss
          LET v_rfc            = arr_tramite[v_pos].rfc
          LET v_curp           = arr_tramite[v_pos].curp
          LET v_ctr_ef         = ""--arr_tramite[v_pos].cve_ef
          LET v_paterno        = arr_tramite[v_pos].paterno
          LET v_materno        = arr_tramite[v_pos].materno
          LET v_nombre         = arr_tramite[v_pos].nombre
          LET v_bimestre       = arr_tramite[v_pos].bimestre
          LET v_97             = arr_tramite[v_pos].viv97
          LET v_f_subcta       = arr_tramite[v_pos].f_subcta
          LET v_ejercicio      = arr_tramite[v_pos].f_credito
          LET v_situacion      = arr_formalizacion[v_pos].estado USING "&&&"
          LET v_ent_financiera = arr_detalle[v_pos].cve_ent_financiera USING "&&&"
          LET v_f_proceso      = arr_detalle[v_pos].f_proceso
          LET v_subproceso     = arr_detalle[v_pos].subproceso USING "&&&"
          LET v_notario        = arr_formalizacion[v_pos].notario
          LET v_edo_notario    = arr_formalizacion[v_pos].edo_notario
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

          LET bnd_ant = 0
          CALL fn_respuesta()
          --EXIT DISPLAY

      ON ACTION CLOSE
         EXIT DISPLAY
         CALL fn_limpia_variables()
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

   LET v_criterio = 1
   CALL fn_oculta2(v_criterio)

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
                            v_f_proceso ,
                            v_subproceso

   MENU
   ON ACTION Siguiente

      LET v_criterio = 1
      CALL fn_oculta1(v_criterio)
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
                      v_usuario_reg    

      CALL fn_menu2()

      IF bnd_ant = 1 THEN
         EXIT MENU
      END IF

   ON ACTION CANCEL

   LET v_criterio = 1
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
                      v_ent_financiera,
                      v_f_proceso ,
                      v_subproceso

      EXIT MENU
   ON ACTION CANCEL

      LET v_criterio = 1
      CALL fn_oculta2(v_criterio)

      LET v_criterio = 1
      --CALL fn_tramite()
      CALL fn_oculta1(v_criterio)

      CALL f.setElementHidden("table1",0)

      LET bnd_ant = 1

      EXIT MENU
 END MENU
 
END FUNCTION

FUNCTION fn_oculta1(v_criterio)

   DEFINE v_criterio SMALLINT

   CALL f.setElementHidden("label1",v_criterio)
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
   CALL f.setElementHidden("label22",v_criterio)
   CALL f.setElementHidden("label23",v_criterio)
   CALL f.setFieldHidden("v_nss",v_criterio)
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
   CALL f.setFieldHidden("v_notario",v_criterio)
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
   CALL f.setElementHidden("label63",v_criterio)
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
   CALL f.setFieldHidden("edit20",v_criterio)
   CALL f.setFieldHidden("edit21",v_criterio)
   CALL f.setFieldHidden("edit22",v_criterio)
   CALL f.setFieldHidden("edit23",v_criterio)
   CALL f.setFieldHidden("edit24",v_criterio)
   CALL f.setFieldHidden("edit25",v_criterio)
   CALL f.setFieldHidden("edit26",v_criterio)
   CALL f.setFieldHidden("edit27",v_criterio)
   CALL f.setFieldHidden("edit28",v_criterio)
   CALL f.setFieldHidden("edit29",v_criterio)
   CALL f.setFieldHidden("edit30",v_criterio)
   CALL f.setFieldHidden("edit31",v_criterio)
   CALL f.setFieldHidden("edit32",v_criterio)

END FUNCTION

FUNCTION fn_limpia_variables()
   LET v_nss            = ""
   LET v_rfc            = ""
   LET v_curp           = ""
   LET v_ctr_ef         = ""
   LET v_paterno        = ""
   LET v_materno        = ""
   LET v_nombre         = ""
   LET v_bimestre       = ""
   LET v_97             = ""
   LET v_f_subcta       = ""
   LET v_ejercicio      = ""
   LET v_situacion      = ""
   LET v_ent_financiera = ""
   LET v_f_proceso      = ""
   LET v_subproceso     = ""
   LET v_notario        = ""
   LET v_edo_notario    = ""
   LET v_escritura      = ""
   LET v_rpp            = ""
   LET v_folio          = ""
   LET v_partida        = ""
   LET v_foja           = ""
   LET v_volumen        = ""
   LET v_libro          = ""
   LET v_tomo           = ""
   LET v_seccion        = ""
   LET v_ent_inmueble   = ""
   LET v_domicilio      = ""
   LET v_avaluo         = ""
   LET v_monto_credito  = ""
   LET v_plazo_credito  = ""
   LET v_moneda         = ""
   LET v_tasa           = ""
   LET v_margen         = ""
   LET v_oto_ef         = ""
   LET v_reg_carta      = ""
   LET v_usuario_reg    = ""
   
   CALL arr_tramite.clear()
   CALL arr_formalizacion.clear()
   CALL arr_tabla.clear()
   CALL arr_detalle.clear()
END FUNCTION