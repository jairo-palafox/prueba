
###################################################################################
#Modulo       => AFI                                                              #
#Programa     => AFIL28                                                           #      
#Objetivo     => Lanzador para baja logica de NSS rojos individual o por archivo  # 
#Fecha Mod.   => 01/Junio/2017                                                    #
#Autor Mod.   => Emilio Abarca,EFP                                                # 
###################################################################################

DATABASE safre_viv

   DEFINE        g_proceso_cod   INTEGER    
   DEFINE        g_opera_cod     INTEGER 
   DEFINE        g_usuario       CHAR (20)
   DEFINE        p_nom_ventana   STRING  
   DEFINE        p_tpo_ejecucion SMALLINT 
   DEFINE        v_archivo_d     STRING     -- Variable para saber ruta y nombre donde se dejará el archivo
   DEFINE        i               INTEGER
   DEFINE w ui.Window
   DEFINE f ui.Form

MAIN 
    LET g_usuario       = ARG_VAL (1)
    LET p_tpo_ejecucion = ARG_val (2)
    LET p_nom_ventana   = ARG_VAL (3)
 -- LET g_proceso_cod   = 1823    -- numero de proceso correspondiente   
 -- LET g_opera_cod     = 1       -- numero de operacion correspondiente 

    -- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".AFIL28.log")
   
        -- se abre la ventana de consulta
    OPEN WINDOW AFIL28 WITH FORM "AFIL281"
    CALL ui.Interface.setText ( p_nom_ventana )

    CALL fn_crea_tmp()

    LET w = ui.Window.getCurrent()
    LET f = w.getForm()

    CALL f.setElementHidden("g_nss",1)
    CALL f.setElementHidden("g_archivo",1)

    --se ponen botenes de selección

    MENU
       ON ACTION individual
          CALL f.setElementHidden("g_nss",0)
          CALL fn_individual()
          EXIT MENU

       ON ACTION Archivo
          CALL f.setElementHidden("g_archivo",0)
          CALL fn_archivo()
          EXIT MENU

       ON ACTION CANCEL
          EXIT MENU

    END MENU

    CLOSE WINDOW AFIL28

END MAIN

FUNCTION fn_individual()

   DEFINE v_nss          CHAR(11)
   DEFINE v_existe_baja  INTEGER  
   DEFINE v_ind_numerico BOOLEAN
   DEFINE v_existe_rojo  INTEGER  
   
   INPUT BY NAME v_nss ATTRIBUTES ( UNBUFFERED )

      ON ACTION ACCEPT

          IF v_nss IS NULL THEN
             CALL fn_mensaje("Cuentas","Debe ingresar un NSS para continuar ","information")
             NEXT FIELD v_nss
          END IF 
          
          IF LENGTH(v_nss) < 11 THEN
             CALL fn_mensaje("","El Nss debe ser de 11 dígitos","")
             NEXT FIELD v_nss
          END IF  

          -- Valida que el nss sea numerico
          LET v_ind_numerico = fn_valida_numero(v_nss)

          IF(v_ind_numerico = 1) THEN
             CALL fn_mensaje("","NSS ingresado no es un dato válido","") 
             NEXT FIELD v_nss
          END IF 

          -- Verifica que sea afiliado rojo
          SELECT COUNT(*)
            INTO v_existe_rojo
            FROM afi_nss_rojo
           WHERE nss = v_nss

          IF(v_existe_rojo = 0) THEN
             CALL fn_mensaje("","No se encontraron registros para actualizar ligados al NSS ingresado","")
             NEXT FIELD v_nss
          ELSE 
             -- Verifica que no esté dado de baja
             SELECT COUNT(*)
               INTO v_existe_baja
               FROM afi_nss_rojo
              WHERE nss = v_nss
                AND estado_rojo = 2

             IF (v_existe_baja  > = 1) THEN 
                CALL fn_mensaje("","El registro ya se encuentra dado de baja lógica","")
                NEXT FIELD v_nss
             ELSE 
                INSERT INTO safre_tmp:nss_rojo
                   VALUES (v_nss)

                -- Muestra información del trabajador para actualizar
                CALL fn_datos_trabajador(v_nss)
                
             END IF    
          END IF 
           

      EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT

      END INPUT

END FUNCTION

FUNCTION fn_datos_trabajador(p_nss)

   DEFINE v_query    STRING
   DEFINE p_nss      CHAR(11)
   DEFINE v_resp     BOOLEAN 
   
   DEFINE r_inf_dh   RECORD 
      curp           CHAR(18), 
      rfc            CHAR(13),
      nombre_af      CHAR(40),
      ap_paterno_af  CHAR(40),
      ap_materno_af  CHAR(40),
      categoria_desc CHAR(60),
      estado_desc    CHAR(20),
      f_actualiza    DATE 
   END RECORD 

   DEFINE v_cont_nombre CHAR(120)

OPEN WINDOW vnt_datos WITH FORM "AFIL282"

   INITIALIZE r_inf_dh TO NULL 
   
   LET v_query = "SELECT afi.curp,
                         afi.rfc,
                         afi.nombre_af,
                         afi.ap_paterno_af,
                         afi.ap_materno_af,
                         car.categoria_desc,
                         cae.estado_rojo_desc,
                         roj.f_actualiza
                    FROM afi_nss_rojo roj,
                         afi_derechohabiente afi,
                         cat_categoria_roja car,
                         cat_estado_rojo cae
                   WHERE roj.id_derechohabiente = afi.id_derechohabiente
                     AND roj.nss = afi.nss
                     AND roj.nss = ",p_nss,
                    "AND roj.categoria   = car.categoria
                     AND roj.estado_rojo = cae.estado_rojo"

   PREPARE prp_informacion FROM v_query
   EXECUTE prp_informacion INTO r_inf_dh.curp,
                                r_inf_dh.rfc,
                                r_inf_dh.nombre_af,
                                r_inf_dh.ap_paterno_af,
                                r_inf_dh.ap_materno_af,
                                r_inf_dh.categoria_desc,
                                r_inf_dh.estado_desc,
                                r_inf_dh.f_actualiza

   LET v_cont_nombre = r_inf_dh.ap_paterno_af CLIPPED," ",
                       r_inf_dh.ap_materno_af CLIPPED," ",
                       r_inf_dh.nombre_af CLIPPED 

   MENU ""
      BEFORE MENU 
         DISPLAY v_cont_nombre TO e_nombre
         DISPLAY p_nss         TO e_nss
         DISPLAY r_inf_dh.curp TO e_curp
         DISPLAY r_inf_dh.rfc  TO e_rfc
         DISPLAY r_inf_dh.categoria_desc TO e_categoria
         DISPLAY r_inf_dh.estado_desc    TO e_estado
         DISPLAY r_inf_dh.f_actualiza  USING "dd/mm/yyyy"  TO e_fecha

      ON ACTION ACCEPT
          LET v_resp = fn_ventana_confirma("","¿Está seguro dar de baja este registro?","")
          IF(v_resp = 0) THEN 
             CALL fn_mensaje("","Se ha cancelado la operación","")
             EXIT MENU
          ELSE 
            -- Actualiza a baja lógica
            UPDATE afi_nss_rojo
               SET estado_rojo = 2
             WHERE nss IN (SELECT nss
                           FROM safre_tmp:nss_rojo);

             CALL fn_mensaje ("" ,"Los registros fueron actualizados correctamente como baja lógica","")
             EXIT MENU 
          END IF 


      ON ACTION CANCEL 
         EXIT MENU 

      ON ACTION CLOSE 
         EXIT MENU 
         
   END MENU 
CLOSE WINDOW vtn_datos

END FUNCTION 

FUNCTION fn_archivo()

   DEFINE v_archivo  STRING
   DEFINE v_cnt_nss  SMALLINT
   DEFINE v_pos                    INTEGER  -- Posición donde inicia la extensión ".txt"
   DEFINE cant                     INTEGER  -- Cantidad de caracteres que tiene el nombre del archivo
   DEFINE buf                      base.StringBuffer
   DEFINE v_extension              STRING
   DEFINE v_ruta_explorador        INTEGER
   DEFINE v_bandera                SMALLINT
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate


      INPUT BY NAME v_archivo ATTRIBUTES ( UNBUFFERED )

      ON ACTION ACCEPT

      --CALL fn_mensaje ("Validación de archivo" ,v_archivo,"information")
--********************************************************
--Se valida que el archivo tenga extensión correcta      *
--********************************************************
      IF v_archivo IS NULL THEN
         CALL fn_mensaje ("Archivo","Debe de seleccionar un archivo","information")
         NEXT FIELD v_archivo
      END IF

      IF v_archivo.getIndexOf(" ", 1) THEN
         CALL fn_mensaje ("Archivo","Nombre de archivo no debe contener espacios","information")
         LET v_archivo = ""
         DISPLAY BY NAME v_archivo
         NEXT FIELD v_archivo
      END IF

      IF v_archivo IS NOT NULL THEN

         SELECT ruta_rescate
           INTO v_ruta_rescate
           FROM seg_modulo
          WHERE modulo_cod = 'afi'

         --CALL fn_mensaje ("Validación de archivo" ,v_archivo,"information")
         LET buf = base.StringBuffer.create()
         CALL buf.append(v_archivo)

         LET cant         = LENGTH(v_archivo)    # cantidad de caracteres de nombre de archivo
         LET v_pos        = buf.getIndexof(".",1)# posición del punto para validar extensión
         LET v_pos = v_pos + 1
         LET v_extension  = buf.subString(v_pos,cant)
         LET v_ruta_explorador = buf.getIndexof("C:",1)

         --CALL fn_mensaje ("Transferencia Archivo", v_archivo,"information")

         IF v_ruta_explorador >= 1 THEN
            LET v_archivo = buf.subString(13,cant)
         END IF

         --CALL fn_mensaje ("Transferencia Archivo", v_archivo,"information")

         LET v_bandera = 0

         IF v_extension = "ptco"  THEN
            LET v_bandera = 1
         END IF

         IF v_extension = "econ"  THEN
            LET v_bandera = 2
         END IF

         #IF v_extension = "ssva" THEN
            #LET v_bandera = 3
         #END IF

         IF v_bandera = 0 THEN
            CALL fn_mensaje ("","Extensión del archivo no es correcta. \n El archivo debe tener extensión '.ptco' o '.econ' ","")
            LET v_archivo = ""
            DISPLAY BY NAME v_archivo
            NEXT FIELD v_archivo
         END IF 

         IF (v_bandera = 1) OR
            (v_bandera = 2) THEN 
            #(v_bandera = 3) THEN
--***************************************************
--Se  recupera el archivo y se deja en ruta rescate *
--***************************************************
            LET v_archivo_d = v_ruta_rescate CLIPPED,"/",v_archivo
            TRY

            CALL FGL_GETFILE(v_archivo,v_archivo_d)
            CALL fn_mensaje ("Transferencia" ,"ARCHIVO TRANSFERIDO CORRRECTAMENTE","information")
            CALL fn_obtiene_datos()
            CALL fn_mensaje ("" ,"Los registros fueron actualizados correctamente como baja lógica","")
            EXIT INPUT

            --CONTINUE INPUT

            CATCH
               ERROR "NO SE PUDO TRANSFERIR"
               CONTINUE INPUT
               END TRY
               EXIT INPUT
         END IF

      END IF

      ON ACTION CANCEL
         EXIT INPUT

      END INPUT

END FUNCTION


FUNCTION fn_obtiene_datos()

   DEFINE s                        CHAR (11)  -- variable para leer lineas del archivo
   DEFINE ch1                      base.Channel 
   DEFINE tok                      base.StringTokenizer
   DEFINE buf1                     base.StringBuffer
   DEFINE cadena                   CHAR (11)  -- variable para rescatar lineas del archivo

   LET ch1 = base.Channel.create()
   CALL ch1.openFile(v_archivo_d,"r")
   LET buf1 = base.StringBuffer.create()

   LET i = 1
   WHILE TRUE
      LET s = ch1.readLine()
      LET tok = base.StringTokenizer.create(s," ")
      WHILE tok.hasMoreTokens()
         --DISPLAY "token:", tok.nextToken()
         LET cadena = tok.nextToken()
         
         INSERT INTO safre_tmp:nss_rojo
              VALUES (cadena)

        UPDATE afi_nss_rojo
            SET estado_rojo = 2
          WHERE nss IN ( SELECT nss
                           FROM safre_tmp:nss_rojo)

      END WHILE

      IF ch1.isEof() THEN EXIT WHILE
      END IF
         LET i = i + 1
      END WHILE
   CALL ch1.close()
END FUNCTION


FUNCTION fn_crea_tmp()
   DATABASE safre_tmp
      DROP TABLE IF EXISTS nss_rojo
      CREATE TABLE nss_rojo (nss CHAR(11))
   DATABASE safre_viv
END FUNCTION

FUNCTION fn_valida_numero(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE
         LET indicador = 1
         IF(indicador == 1) THEN
            EXIT FOR
         END IF
      END IF
   END FOR

   RETURN indicador

END FUNCTION
