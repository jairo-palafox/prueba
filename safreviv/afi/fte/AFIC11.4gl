######################################################################
#Proyecto          => INFONAVIT (MEXICO)                             #
#Propietario       => E.F.P.                                         #
#Programa AFIC01   => PROGRAMA DE MODIFICA LA INFORMACIÓN DE         #
#                     DERECHOHABIENTES                               #
#Sistema           => SAFRE VIVIENDA                                 #
#Autor             => HÉCTOR JIMÉNEZ LARA                            #
#Fecha             => 02 DE JUNIO DE 2015                            #
######################################################################
IMPORT os

DATABASE safre_viv

GLOBALS "AFIG01.4gl"
GLOBALS "AFIC011.inc"

   DEFINE p_tipo_ejecucion              SMALLINT -- forma como ejecutará el programa
   DEFINE p_s_titulo                    STRING   -- título de la ventana
   DEFINE g_nss                         CHAR(11) -- nss
   DEFINE r_valida_nss                  SMALLINT
   DEFINE r_valida                      SMALLINT
   DEFINE v_paso_nss                    STRING
   DEFINE v_msj_alerta                  STRING
   DEFINE v_box                         SMALLINT
   DEFINE v_box_curp                    SMALLINT
   DEFINE v_box_rfc                     SMALLINT
   DEFINE v_confirma_paterno            SMALLINT
   DEFINE v_confirma_materno            SMALLINT
   DEFINE p_nombre_modificado           CHAR(40)
   DEFINE v_nombre_es_correcto          SMALLINT
   DEFINE v_qry                         STRING

   DEFINE bnd_nombre                    SMALLINT
   DEFINE bnd_rfc                       SMALLINT
   DEFINE bnd_curp                      SMALLINT
   DEFINE bnd_ws                        SMALLINT
   DEFINE bnd_act                       CHAR(1)
   DEFINE v_s_comando                   STRING
   DEFINE v_bnd                         SMALLINT
   DEFINE v_cta                         SMALLINT
   DEFINE v_partner                     CHAR(1)
   DEFINE bnd_act_n                     SMALLINT

   DEFINE rec_act_afi               RECORD
     v_act_rfc                          CHAR(13),
     v_act_curp                         CHAR(18),
     v_act_paterno                      CHAR(40),
     v_act_materno                      CHAR(40),
     v_act_nombre                       CHAR(40)
   END RECORD

   DEFINE rec_editar    RECORD
      nss                               CHAR(11),
      rfc                               CHAR(13),
      curp                              CHAR(18),
      ap_paterno_af                     CHAR(40),
      ap_materno_af                     CHAR(40),
      nombres_af                        CHAR(40)
   END RECORD

MAIN

   -- se asignan los parámetros que vienen del fglrun
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_nss            = ARG_VAL(4) -- si se recibe, se envia directo a la consulta de datos

   -- se asigna el titulo de la ventana
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se inicia el LOG
   CALL STARTLOG(g_usuario CLIPPED||'.AFIC01.log')
   
   -- se inician las variables globales
   LET g_hoy    = TODAY
   LET g_titulo = "Información"
   LET g_imagen = "information"

   CALL fn_alertamiento()
   RETURNING v_msj_alerta

   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE modulo_cod = 'afi'

   CALL fn_proceso_principal()

END MAIN

-- Función principal del programa
FUNCTION fn_proceso_principal()
   DEFINE v_flg                         SMALLINT
   DEFINE v_mensaje                     STRING
   DEFINE v_id_derechohabiente          LIKE afi_derechohabiente.id_derechohabiente

   CLOSE WINDOW SCREEN

   -- se abre la ventana de consulta
   OPEN WINDOW w2 WITH FORM "AFIC11"

   IF ( g_nss IS NOT NULL ) AND (r_valida = 0)  THEN
      -- se valida si el NSS existe en la lista de derechohabientes
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = g_nss

      -- si no existe el derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se indica al usuario en pantalla que no fue encontrado
         LET v_mensaje = "El derechohabiente con NSS [" || g_nss || "] no existe"
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF
   ELSE
      -- si no se recibio el NSS desde parametro, se habilita la captura de datos de busqueda
      LET w_criterio = ui.window.getcurrent()
      LET w_criterio = ui.window.forName("w2")

      CALL w_criterio.settext("INFORMACIÓN DE DERECHOHABIENTES")

      LET f_criterio = w_criterio.getform()

      -- se abre el ciclo
      LET v_flg = TRUE
      WHILE ( v_flg )
         CALL fn_preconsulta() RETURNING v_flg

         -- se reinicia el registro de derechohabiente
         INITIALIZE reg_derechohabiente.* TO NULL
      END WHILE
   END IF
   CLOSE WINDOW w2
END FUNCTION

-- Funcion para elegir la información del derechohabiente a desplegar
FUNCTION fn_preconsulta()
   DEFINE cont                          INTEGER     -- contador
   DEFINE x_flg                         SMALLINT
   DEFINE lc_condicion                  STRING      -- query de condiciones para la consulta
   DEFINE lc_qry                        STRING      -- query de consulta
   DEFINE v_nss                         VARCHAR(11) -- nss
   DEFINE v_rfc                         VARCHAR(13) -- rfc
   DEFINE v_curp                        VARCHAR(18) -- curp
   DEFINE v_folio                       DECIMAL(9,0)-- folio del archivo
   DEFINE v_edo_cuenta                  SMALLINT

   DEFINE arr_busqueda DYNAMIC ARRAY OF RECORD
      nss                               CHAR(11),
      rfc                               CHAR(13),
      curp                              CHAR(18),
      ap_paterno_af                     CHAR(40),
      ap_materno_af                     CHAR(40),
      nombres_af                        CHAR(40),
      folio                             DECIMAL(9,0)
   END RECORD
   
    LET x_flg      = 0
    LET v_ind_nss  = FALSE
    LET v_ind_rfc  = FALSE
    LET v_ind_curp = FALSE

    -- se inician las variables
    LET v_nss   = NULL
    LET v_rfc   = NULL
    LET v_curp  = NULL
    LET v_folio = NULL 

    -- se abre el dialog para obtener parámetros de consulta
    DIALOG ATTRIBUTES(UNBUFFERED)


       CONSTRUCT lc_condicion ON a.nss
                            FROM nss

          BEFORE CONSTRUCT
             CALL arr_busqueda.clear()

             CALL dialog.setActionHidden("close",1)
             CALL f_criterio.setelementhidden("gb_identifica",1)
             CALL f_criterio.setelementhidden("gb_credito",1)
             CALL f_criterio.setelementhidden("gb_contacto",1)

          AFTER FIELD nss
             LET v_ind_nss = FGL_BUFFERTOUCHED()

          ON ACTION ACCEPT
          LET v_paso_nss = GET_FLDBUF(nss)
          IF v_paso_nss IS NULL THEN
             CALL fn_mensaje("Aviso"," Obligatoriamente debe indicar un NSS para búsqueda","information")
          END IF
          IF ( v_paso_nss IS NOT NULL ) THEN

            IF fn_valida_caracteres(v_paso_nss) <> 0 THEN
               CALL fn_mensaje("Consulta Saldo", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD nss
            END IF

             IF r_valida_nss = 0 THEN 
          
              LET lc_qry = " SELECT a.nss  ,",
                                  " a.rfc  ,",
                                  " a.curp ,",
                                  " a.ap_paterno_af ,",
                                  " a.ap_materno_af ,",
                                  " a.nombre_af, ",
                                  " a.folio_lote ",
                           " FROM afi_derechohabiente a ",
                           " WHERE ",lc_condicion CLIPPED


              PREPARE prp_pre_busqueda FROM lc_qry
              DECLARE cur_pre_busqueda CURSOR FOR prp_pre_busqueda

              LET cont= 1

              FOREACH cur_pre_busqueda INTO arr_busqueda[cont].*
                  LET cont = 1 + cont

                  IF ( cont > 32767 ) THEN
                      CALL fn_mensaje("Aviso","SE SOBREPASÓ LA CAPACIDAD MÁXIMA DEL ARREGLO","exclamation")
                      LET INT_FLAG = TRUE
                      EXIT DIALOG
                  END IF
              END FOREACH

              IF ( cont = 1 ) THEN
                  CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
                  LET INT_FLAG = TRUE
              END IF

              LET INT_FLAG = FALSE

              EXIT DIALOG

              ELSE
                 CALL fn_mensaje("Atención",v_msj_alerta,"stop")

             END IF
          END IF
          ON ACTION CANCEL
              LET INT_FLAG = TRUE
              EXIT DIALOG

          AFTER CONSTRUCT
             IF ((NOT v_ind_nss  ) AND
                 (NOT v_ind_rfc  ) AND
                 (NOT v_ind_curp ) AND
                 (NOT v_ind_folio)) THEN
                CALL fn_mensaje("Aviso","Debe indicar un criterio de búsqueda obligatoriamente","information")
                NEXT FIELD nss
             ELSE
                LET int_flag = FALSE
                EXIT DIALOG
             END IF

          ON KEY (INTERRUPT)
             CALL fn_mensaje("Aviso","Búsqueda cancelada","information")
             RETURN

       END CONSTRUCT

    END DIALOG

    CALL f_criterio.setelementhidden("gb_identifica",0)
    CALL f_criterio.setelementhidden("gb_credito",0)
    CALL f_criterio.setelementhidden("gb_contacto",0)

    -- se borra el ultimo renglon del arreglo porque el FOREACH lo deja en blanco
    CALL arr_busqueda.deleteElement(cont)

    IF ( (cont-1) >= 1 ) THEN
       DISPLAY ARRAY arr_busqueda TO tb2.*

          ON ACTION ACCEPT
             LET rec_editar.nss           = arr_busqueda[arr_curr()].nss
             LET rec_editar.rfc           = arr_busqueda[arr_curr()].rfc
             LET rec_editar.curp          = arr_busqueda[arr_curr()].curp
             LET rec_editar.ap_paterno_af = arr_busqueda[arr_curr()].ap_paterno_af
             LET rec_editar.ap_materno_af = arr_busqueda[arr_curr()].ap_materno_af
             LET rec_editar.nombres_af    = arr_busqueda[arr_curr()].nombres_af

             LET v_s_qryTxt = "SELECT ind_estado_cuenta
                                 FROM afi_derechohabiente 
                                WHERE nss = ?"
             PREPARE prp_cons_cuenta FROM v_s_qryTxt
             EXECUTE prp_cons_cuenta INTO v_edo_cuenta USING rec_editar.nss

             IF v_edo_cuenta = 0 THEN
                CALL fn_edit_dh(rec_editar.*)
             ELSE
               CALL fn_mensaje("Error","Cuenta inactiva, no puede actualizar su información","error")
             END IF

          ON ACTION CANCEL
             LET INT_FLAG = TRUE
             EXIT DISPLAY

       END DISPLAY

       IF NOT INT_FLAG THEN
          LET x_flg = 1
          LET INT_FLAG = FALSE
       ELSE
          LET x_flg = 0
       END IF
    END IF

    CLEAR FORM

    RETURN x_flg

END FUNCTION

FUNCTION fn_edit_dh(p_rec_busqueda)
   DEFINE v_nombre_imss                 CHAR(50)
   DEFINE v_f_nacimiento                DATE
   DEFINE v_id_dh                       DECIMAL(9,0)
   DEFINE v_folio_lote                  DECIMAL(9,0)
   DEFINE v_ind_nrp                     CHAR (1)
   DEFINE v_confirma_rfc                SMALLINT
   DEFINE v_confirma_curp               SMALLINT
   DEFINE v_confirma_nombre             SMALLINT
   DEFINE v_arr_datos                   DYNAMIC ARRAY OF SMALLINT
   DEFINE v_cnt                         SMALLINT
   DEFINE v_cadena                      STRING
   DEFINE v_arr_nombres                 DYNAMIC ARRAY OF STRING
   DEFINE v_continua                    SMALLINT
   DEFINE v_arr_envia                   DYNAMIC ARRAY OF STRING
   DEFINE v_curp_si_ceros               SMALLINT -- bandera para conocer si el curp es de 18 ceros
   DEFINE p_rec_busqueda            RECORD
      nss                               CHAR(11),
      rfc                               CHAR(13),
      curp                              CHAR(18),
      ap_paterno_af                     CHAR(40),
      ap_materno_af                     CHAR(40),
      nombres_af                        CHAR(40)
   END RECORD

   DEFINE v_rec_inserta             RECORD
     id_dh                              DECIMAL(9,0),
     folio_lote                         DECIMAL(9,0),
     curp                               CHAR(18),
     rfc                                CHAR(11),
     ind_nrp                            CHAR(1),
     f_nacimiento                       DATE ,
     nombre_imss                        CHAR(50),
     nombres_af                         CHAR(40),
     ap_paterno_af                      CHAR(40),
     ap_materno_af                      CHAR(40)
   END RECORD
   DEFINE v_datos STRING

   OPEN WINDOW w_edit WITH FORM "AFIC112"

   LET v_qry = " SELECT id_derechohabiente,
                        nombre_imss,
                        f_nacimiento,
                        folio_lote,
                        ind_nrp
                   FROM afi_derechohabiente
                  WHERE nss = ? "

   PREPARE prp_nom_fec FROM v_qry
   EXECUTE prp_nom_fec USING  p_rec_busqueda.nss INTO v_id_dh,
                                                      v_nombre_imss,
                                                      v_f_nacimiento,
                                                      v_folio_lote,
                                                      v_ind_nrp

   -- se despliega en la forma
   DISPLAY p_rec_busqueda.nss           TO nss
   DISPLAY p_rec_busqueda.rfc           TO rfc
   DISPLAY p_rec_busqueda.curp          TO curp
   DISPLAY p_rec_busqueda.ap_paterno_af TO ap_paterno
   DISPLAY p_rec_busqueda.ap_materno_af TO ap_materno
   DISPLAY p_rec_busqueda.nombres_af    TO nombre
   DISPLAY v_nombre_imss                TO nom_imss
   DISPLAY v_f_nacimiento               TO f_nacimiento

   INPUT BY NAME v_box,
                 v_box_curp,
                 v_box_rfc,
                 rec_act_afi.v_act_rfc,
                 rec_act_afi.v_act_curp,
                 rec_act_afi.v_act_paterno,
                 rec_act_afi.v_act_materno,
                 rec_act_afi.v_act_nombre
                 ATTRIBUTES (UNBUFFERED, ACCEPT=FALSE, CANCEL=FALSE)
    
      ON ACTION Aceptar
         DISPLAY "Datos a actualizar : ",rec_act_afi.*

         -- Check box para el apellido materno nulo
         IF v_box = 1 THEN
            LET rec_act_afi.v_act_materno = ""
            LET v_arr_datos[4]            = 1
            LET v_arr_nombres[4]          = "ap_materno_af"
         END IF

         -- verfiica cambios en rfc
         IF v_box_rfc = 1 THEN
            LET rec_act_afi.v_act_rfc = ""
            LET v_arr_datos[1]        = 1
            LET v_arr_nombres[1]      = "rfc"
         ELSE
            IF (p_rec_busqueda.rfc IS NOT NULL AND
               (p_rec_busqueda.rfc <> rec_act_afi.v_act_rfc)) OR
               (p_rec_busqueda.rfc IS NULL AND
                rec_act_afi.v_act_rfc IS NOT NULL) THEN

               CALL fn_valida_rfc_alt(rec_act_afi.v_act_rfc)
               RETURNING v_confirma_rfc

               -- tiene que ser 1 para que sea correcto
               IF v_confirma_rfc = 1 THEN
                  -- se prende la bandera en el arreglo
                  LET v_arr_datos[1] = 1
                  LET v_arr_nombres[1] = "rfc"
               ELSE
                  LET rec_act_afi.v_act_rfc = ""
                  LET v_arr_datos[1] = 0
                  CONTINUE INPUT
               END IF
            END IF
         END IF

         -- Checkbox para el CURP nulo, puede ser nulo solo cuando el CURP actual son 18 ceros
         IF v_box_curp = 1 THEN
           --CALL fn_valida_curp_nulo(p_rec_busqueda.nss,p_rec_busqueda.curp) RETURNING v_curp_si_ceros
           LET v_curp_si_ceros = 1
           -- Si la bandera es uno quiere decir que cumple con la condicion de que sean 18 ceros
           IF v_curp_si_ceros = 1 THEN
              LET v_arr_datos[2]   = 1
              LET v_arr_nombres[2] = "curp"
           ELSE
              CALL fn_mensaje("Error", "El CURP actual no es de 18 ceros" ,"stop")
              LET rec_act_afi.v_act_curp = ""
              LET v_arr_datos[2]         = 0
              CONTINUE INPUT
           END IF
         ELSE
            -- Verifica cambios en CURP
            IF (p_rec_busqueda.curp IS NOT NULL AND
               (p_rec_busqueda.curp <> rec_act_afi.v_act_curp)) OR
               (p_rec_busqueda.curp IS NULL AND
                rec_act_afi.v_act_curp IS NOT NULL) THEN


               CALL valida_curp_alt(rec_act_afi.v_act_curp)
               RETURNING v_confirma_curp

               IF v_confirma_curp = 1 THEN
                  LET v_arr_datos[2]   = 1
                  LET v_arr_nombres[2] = "curp"
               ELSE
                  LET rec_act_afi.v_act_curp = ""
                  LET v_arr_datos[2]         = 0
                  CONTINUE INPUT
               END IF
           END IF
        END IF -- se  cierra con la condicion incluida del checkbox

        -- Verifica cambios en ap paterno
        IF p_rec_busqueda.ap_paterno_af <> rec_act_afi.v_act_paterno THEN
            CALL fn_valida_nombre(rec_act_afi.v_act_paterno,"P")
            RETURNING v_confirma_paterno, p_nombre_modificado
            DISPLAY " display de prueba "
            IF v_confirma_paterno = 1 THEN
               LET v_arr_datos[3] = 1
               LET v_arr_nombres[3] = "ap_paterno_af"
            ELSE
               LET rec_act_afi.v_act_paterno = ""
               LET v_arr_datos[3] = 0
               CONTINUE INPUT
            END IF
         END IF

         --- Verifica cambios en ap materno
         IF (p_rec_busqueda.ap_materno_af IS NOT NULL AND
            (p_rec_busqueda.ap_materno_af <> rec_act_afi.v_act_materno)) OR
            (p_rec_busqueda.ap_materno_af IS NULL AND
             rec_act_afi.v_act_materno IS NOT NULL) THEN

            CALL fn_valida_nombre(rec_act_afi.v_act_materno,"M")
            RETURNING v_confirma_materno, p_nombre_modificado

            IF v_confirma_materno = 1 THEN
               IF v_box = 1 THEN
                  LET rec_act_afi.v_act_materno = ""
                  LET v_arr_datos[4] = 1
                  LET v_arr_nombres[4] = "ap_materno_af"
               END IF
               IF v_box = 0 THEN
                  LET v_arr_datos[4] = 1
                  LET v_arr_nombres[4] = "ap_materno_af"
               END IF
            ELSE
               LET rec_act_afi.v_act_materno = ""
               LET v_arr_datos[4] = 0
               CONTINUE INPUT
            END IF
         END IF

         -- verifica cambios al nombre
         IF p_rec_busqueda.nombres_af <> rec_act_afi.v_act_nombre THEN
            CALL fn_valida_nombre(rec_act_afi.v_act_nombre,"N")
            RETURNING v_confirma_nombre, p_nombre_modificado
            IF v_confirma_nombre = 1 THEN
               LET v_arr_datos[5] = 1
               LET v_arr_nombres[5] = "nombre_af"
            ELSE
               LET rec_act_afi.v_act_nombre = ""
               LET v_arr_datos[5] = 0
               CONTINUE INPUT
            END IF
         END IF

         LET v_cadena = "Se modificaron los datos: "

         FOR v_cnt = 1 TO 5
            IF v_arr_datos[v_cnt] = 1 THEN
               DISPLAY "Hubo cambios y fueron validados en la posición: " || v_cnt
               LET v_cadena = v_cadena ||","||v_arr_nombres[v_cnt]
               LET v_continua = 1
               LET v_arr_envia[v_cnt] = v_arr_nombres[v_cnt]
               DISPLAY "POSICIÓN "||v_cnt||" : " ,v_arr_envia[v_cnt]

               LET v_datos = v_datos || v_arr_nombres[v_cnt] || " "
               DISPLAY "V DATOS : ", v_datos
            ELSE
               DISPLAY "No existen cambios en la posición : " || v_cnt
            END IF
         END FOR

         DISPLAY v_cadena

         LET v_rec_inserta.id_dh          = v_id_dh
         LET v_rec_inserta.folio_lote     = v_folio_lote
         LET v_rec_inserta.curp           = p_rec_busqueda.curp
         LET v_rec_inserta.rfc            = p_rec_busqueda.rfc
         LET v_rec_inserta.ind_nrp        = v_ind_nrp
         LET v_rec_inserta.f_nacimiento   = v_f_nacimiento
         LET v_rec_inserta.nombre_imss    = v_nombre_imss
         LET v_rec_inserta.nombres_af     = p_rec_busqueda.nombres_af
         LET v_rec_inserta.ap_paterno_af  = p_rec_busqueda.ap_paterno_af
         LET v_rec_inserta.ap_materno_af  = p_rec_busqueda.ap_materno_af

         -- En caso de que cualquier dato no este validado
         IF v_arr_datos[1] = 0 OR
            v_arr_datos[2] = 0 OR
            v_arr_datos[3] = 0 OR
            v_arr_datos[4] = 0 OR
            v_arr_datos[5] = 0 THEN
            EXIT INPUT
         ELSE

            CALL fn_exe_actualiza_dh(v_arr_envia, v_cadena, v_rec_inserta.*, rec_act_afi.*, p_rec_busqueda.nss)
            EXIT INPUT
         END IF

      AFTER FIELD v_act_nombre
         NEXT FIELD v_act_rfc

    ON ACTION salir
       EXIT INPUT

   END INPUT

   CLOSE WINDOW w_edit
END FUNCTION

{**
 Función que realiza la actualización
 del derechohabiente después de haber realizado
 las validaciones correspondientes
**}
FUNCTION fn_exe_actualiza_dh(p_arr_envia,p_cadena,p_rec_inserta,p_rec_act_afi,p_nss)
   DEFINE p_arr_envia                   DYNAMIC ARRAY OF STRING
   DEFINE v_arr_nuevo                   DYNAMIC ARRAY OF STRING
   DEFINE p_nss                         CHAR(11)
   DEFINE p_cadena                      STRING
   DEFINE v_i_cnt                       SMALLINT
   DEFINE i                             SMALLINT
   DEFINE j                             SMALLINT
   DEFINE v_confirma                    SMALLINT
   DEFINE p_rec_inserta             RECORD
      id_dh                             DECIMAL(9,0),
      folio_lote                        DECIMAL(9,0),
      curp                              CHAR(18),
      rfc                               CHAR(11),
      ind_nrp                           CHAR(1),
      f_nacimiento                      DATE ,
      nombre_imss                       CHAR(50),
      nombres_af                        CHAR(40),
      ap_paterno_af                     CHAR(40),
      ap_materno_af                     CHAR(40)
   END RECORD
   DEFINE p_rec_act_afi             RECORD
      v_act_rfc                         CHAR(13),
      v_act_curp                        CHAR(18),
      v_act_paterno                     CHAR(40),
      v_act_materno                     CHAR(40),
      v_act_nombre                      CHAR(40)
   END RECORD
   DEFINE v_cnt_curp                    SMALLINT
   DEFINE v_cnt_rfc                     SMALLINT
   DEFINE v_cnt_nombre                  SMALLINT
   DEFINE v_cnt_paterno                 SMALLINT
   DEFINE v_cnt_materno                 SMALLINT
   DEFINE v_cnt_ind                     SMALLINT
   DEFINE v_ax_ind_modifica             SMALLINT
   DEFINE v_ax_ret_nom_imss             CHAR(50) -- Contiene el nombre_imss mmodificado

   LET v_cnt_curp        = 0
   LET v_cnt_rfc         = 0
   LET v_cnt_nombre      = 0
   LET v_cnt_paterno     = 0
   LET v_cnt_materno     = 0
   LET v_cnt_ind         = 0
   LET v_ax_ind_modifica = 0

   IF v_box_curp = 1 THEN
      CALL fn_ventana_confirma("Información","El CURP se almacenará sin información\n¿Está seguro que desea actualizar la información?","information")
      RETURNING v_confirma
   END IF

   IF v_box_rfc = 1 THEN
      CALL fn_ventana_confirma("Información","El RFC se almacenará sin información\n¿Está seguro que desea actualizar la información?","information")
      RETURNING v_confirma   
   END IF

   IF v_box = 1 THEN
      CALL fn_ventana_confirma("Información","Apellido materno se almacenará sin información\n¿Está seguro que desea actualizar la información?","information")
      RETURNING v_confirma
   ELSE
      CALL fn_ventana_confirma("Información","¿Está seguro que desea actualizar la información?","information")
      RETURNING v_confirma
   END IF

   IF v_confirma = 1 THEN

      -- Contadores
      LET j = 1
      LET i = 1

      --se ordena el arreglo
      FOR v_i_cnt = 1 TO p_arr_envia.getLength()
         IF p_arr_envia[v_i_cnt] IS NOT NULL THEN
            FOR i = j TO 5
               LET v_arr_nuevo[i] = p_arr_envia[v_i_cnt]
               DISPLAY "NUEVO ARREGLO : "|| v_arr_nuevo[i] || " Posición :" || i

               IF v_arr_nuevo[i] = "curp" THEN
                  LET v_cnt_curp = 1
               END IF

               IF v_arr_nuevo[i] = "rfc" THEN
                  LET v_cnt_rfc = 2
               END IF

               IF v_arr_nuevo[i] = "nombre_af" OR  
                  v_arr_nuevo[i] = "ap_paterno_af" OR
                  v_arr_nuevo[i] = "ap_materno_af"     THEN
                  LET v_cnt_nombre = 9
               END IF               

               LET j = j + 1
               EXIT FOR 
            END FOR
         ELSE
            CONTINUE FOR 
         END IF
      END FOR

      LET v_cnt_ind = v_cnt_curp + v_cnt_rfc + v_cnt_nombre
      DISPLAY "TOT CNT INDICADOR : " , v_cnt_ind
      CASE v_cnt_ind
         WHEN 1
            LET v_ax_ind_modifica = 1
         WHEN 2
            LET v_ax_ind_modifica = 2
         WHEN 3
            LET v_ax_ind_modifica = 16
         WHEN 9
            LET v_ax_ind_modifica = 9
         WHEN 10
            LET v_ax_ind_modifica = 17
         WHEN 11
            LET v_ax_ind_modifica = 11
         WHEN 12
            LET v_ax_ind_modifica = 18
         OTHERWISE
            LET v_ax_ind_modifica = 0
      END CASE

      DISPLAY "INDICADOR PARA ACTUALIZACIÓN : ",v_ax_ind_modifica

      LET v_s_qryTxt = "INSERT INTO afi_his_derechohabiente
                             VALUES(?,TODAY,?,?,?,?,?,?,?,?,?,?)"
      
      PREPARE prp_ins_his FROM v_s_qryTxt
      EXECUTE prp_ins_his USING p_rec_inserta.id_dh,
                                p_rec_inserta.folio_lote ,
                                v_ax_ind_modifica,
                                p_rec_inserta.curp ,
                                p_rec_inserta.rfc ,
                                p_rec_inserta.ind_nrp ,
                                p_rec_inserta.f_nacimiento ,
                                p_rec_inserta.nombre_imss ,
                                p_rec_inserta.nombres_af ,
                                p_rec_inserta.ap_paterno_af ,
                                p_rec_inserta.ap_materno_af

      LET bnd_act_n = 0
      FOR v_i_cnt = 1 TO v_arr_nuevo.getLength()
         CASE
            WHEN v_arr_nuevo[v_i_cnt] = "rfc"
               LET v_s_qryTxt = "UPDATE afi_derechohabiente\n
                                    SET rfc = ?
                                  WHERE nss = ?"
               PREPARE prp_upd_rfc FROM v_s_qryTxt
               EXECUTE prp_upd_rfc USING p_rec_act_afi.v_act_rfc,
                                         p_nss

               DISPLAY p_rec_act_afi.v_act_rfc TO rfc

               LET bnd_ws  = 1
               LET bnd_act = "R"
               LET v_partner = " "
               IF rec_act_afi.v_act_rfc IS NULL THEN
                  LET rec_act_afi.v_act_rfc = " "
               END IF
               CALL fn_ws_adm(bnd_ws,bnd_act," ",rec_act_afi.v_act_rfc," "," "," ",p_nss)

            WHEN v_arr_nuevo[v_i_cnt] = "curp"
               LET v_s_qryTxt = "UPDATE afi_derechohabiente\n
                                    SET curp = ?
                                  WHERE nss = ?"
               PREPARE prp_upd_curp FROM v_s_qryTxt
               EXECUTE prp_upd_curp USING p_rec_act_afi.v_act_curp,
                                          p_nss

               DISPLAY p_rec_act_afi.v_act_curp TO curp

               LET bnd_ws    = 1
               LET bnd_act   = "C"
               LET v_partner = " "
               IF rec_act_afi.v_act_curp IS NULL THEN
                  LET rec_act_afi.v_act_curp = " "
               END IF
               CALL fn_ws_adm(bnd_ws,bnd_act,rec_act_afi.v_act_curp," "," "," "," ",p_nss)

            WHEN v_arr_nuevo[v_i_cnt] = "ap_paterno_af"
               LET v_s_qryTxt = "UPDATE afi_derechohabiente\n
                                    SET ap_paterno_af = ?
                                  WHERE nss = ? "
               PREPARE prp_upd_paterno FROM v_s_qryTxt
               EXECUTE prp_upd_paterno USING p_rec_act_afi.v_act_paterno,
                                             p_nss

               DISPLAY p_rec_act_afi.v_act_paterno TO ap_paterno

               LET bnd_ws     = 1
               LET bnd_act    = "N"
               LET v_partner  = " "
               LET bnd_act_n = 1

               -- Se invoca a la función que actualiza nombre_imss
               CALL fn_actualiza_nombre_imss(p_nss) RETURNING v_ax_ret_nom_imss
               --CALL fn_actualiza_nombre_imss(p_nss,p_rec_act_afi.v_act_paterno,"P")
               DISPLAY v_ax_ret_nom_imss TO nom_imss

            WHEN v_arr_nuevo[v_i_cnt] = "ap_materno_af"
               LET v_s_qryTxt = "UPDATE afi_derechohabiente\n
                                    SET ap_materno_af = ?
                                  WHERE nss = ?"
               PREPARE prp_upd_materno FROM v_s_qryTxt
               EXECUTE prp_upd_materno USING p_rec_act_afi.v_act_materno,
                                             p_nss

               DISPLAY p_rec_act_afi.v_act_materno TO ap_materno

               LET bnd_ws     = 1
               LET bnd_act    = "N"
               LET v_partner  = " "
               LET bnd_act_n = 1

               IF rec_act_afi.v_act_materno IS NULL THEN
                  LET rec_act_afi.v_act_materno = " "
               END IF

               -- Se invoca a la función que actualiza nombre_imss
               CALL fn_actualiza_nombre_imss(p_nss) RETURNING v_ax_ret_nom_imss
               --CALL fn_actualiza_nombre_imss(p_nss,p_rec_act_afi.v_act_paterno,"M")
               DISPLAY v_ax_ret_nom_imss TO nom_imss

            WHEN v_arr_nuevo[v_i_cnt] = "nombre_af"
               LET v_s_qryTxt = "UPDATE afi_derechohabiente\n
                                    SET nombre_af = ?
                                  WHERE nss = ?"
               PREPARE prp_upd_nombre FROM v_s_qryTxt
               EXECUTE prp_upd_nombre USING p_rec_act_afi.v_act_nombre,
                                            p_nss

               DISPLAY p_rec_act_afi.v_act_nombre TO nombre

               LET bnd_ws    = 1
               LET bnd_act   = "N"
               LET v_partner = " "
               LET bnd_act_n = 1

               -- Se invoca a la función que actualiza nombre_imss
               CALL fn_actualiza_nombre_imss(p_nss) RETURNING v_ax_ret_nom_imss
               --CALL fn_actualiza_nombre_imss(p_nss, p_rec_act_afi.v_act_paterno,"N")
               DISPLAY v_ax_ret_nom_imss TO nom_imss

         END CASE
      END FOR
      IF bnd_act_n = 1 THEN
         CALL fn_ws_adm(bnd_ws,bnd_act," "," ",rec_act_afi.v_act_nombre,rec_act_afi.v_act_paterno,rec_act_afi.v_act_materno,p_nss)
      END IF
--******************************************************************************
{
      IF rec_act_afi.v_act_rfc IS NOT NULL THEN
         LET bnd_ws  = 1
         LET bnd_act = "R"
         LET v_partner = " "
         IF rec_act_afi.v_act_curp IS NULL THEN
            LET rec_act_afi.v_act_curp = " "
         END IF
         CALL fn_ws_adm(bnd_ws,bnd_act,rec_act_afi.v_act_curp,rec_act_afi.v_act_rfc," "," "," ",p_nss)
      ELSE
         LET bnd_ws  = 1
         LET bnd_act = "R"
         LET v_partner = " "
         LET rec_act_afi.v_act_rfc = " "
         IF rec_act_afi.v_act_curp IS NULL THEN
            LET rec_act_afi.v_act_curp = " "
         END IF
         CALL fn_ws_adm(bnd_ws,bnd_act,rec_act_afi.v_act_curp,rec_act_afi.v_act_rfc," "," "," ",p_nss)
      END IF

      IF (rec_act_afi.v_act_curp IS NOT NULL) AND
          (rec_act_afi.v_act_curp <> "a") THEN
         LET bnd_ws   = 1
         LET bnd_act = "C"
         LET v_partner = " "
         IF rec_act_afi.v_act_rfc IS NULL THEN
            LET rec_act_afi.v_act_rfc = " "
         END IF
         CALL fn_ws_adm(bnd_ws,bnd_act,rec_act_afi.v_act_curp,rec_act_afi.v_act_rfc," "," "," ",p_nss)
      ELSE
         LET bnd_ws   = 1
         LET bnd_act = "C"
         LET v_partner = " "
         LET rec_act_afi.v_act_curp = " "
         IF rec_act_afi.v_act_rfc IS NULL THEN
            LET rec_act_afi.v_act_rfc = " "
         END IF
         CALL fn_ws_adm(bnd_ws,bnd_act,rec_act_afi.v_act_curp,rec_act_afi.v_act_rfc," "," "," ",p_nss)
      END IF

      IF (rec_act_afi.v_act_nombre IS NOT NULL) OR
         (rec_act_afi.v_act_paterno IS NOT NULL) OR
         (rec_act_afi.v_act_materno IS NOT NULL) THEN
         LET bnd_ws     = 1
         LET bnd_act = "N"
         LET v_partner = " "
         
         IF rec_act_afi.v_act_curp IS NULL THEN
            LET rec_act_afi.v_act_curp = " "
         END IF
         
         IF rec_act_afi.v_act_rfc IS NULL THEN
            LET rec_act_afi.v_act_rfc = " "
         END IF

         IF rec_act_afi.v_act_materno IS NULL THEN
            LET rec_act_afi.v_act_materno = " "
         END IF

          CALL fn_ws_adm(bnd_ws,bnd_act,rec_act_afi.v_act_curp,rec_act_afi.v_act_rfc,rec_act_afi.v_act_nombre,rec_act_afi.v_act_paterno,rec_act_afi.v_act_materno,p_nss)
      END IF
}
--******************************************************************************
   END IF
END FUNCTION

--Función que valida el nombre modificado
FUNCTION fn_valida_nombre(p_nombre_modificado,p_param)
   DEFINE p_nombre_modificado           CHAR(40)
   DEFINE v_cadena                      STRING                -- cadena para analizar el nombre
   DEFINE v_mensaje                     STRING                -- mensaje para el usuario en caso de error
   DEFINE v_nombre_aux                  STRING                -- nombre auxiliar
   DEFINE v_tokenizer                   base.StringTokenizer  -- para analizar nombre y verificar espacios
   DEFINE v_indice                      SMALLINT
   DEFINE p_param                       CHAR(1)

   -- se asume que el NOMBRE esta correcto
   LET v_nombre_es_correcto = 1

   -- se asigna el nombre a la cadena
   LET v_cadena = p_nombre_modificado CLIPPED

   DISPLAY "NOMBRE CADENA EN FUNCIÓN  : ", v_cadena
   -- se revisa que no se tengan caracteres especiales
   FOR v_indice = 1 TO v_cadena.getLength()
      IF v_cadena.getCharAt(v_indice) NOT MATCHES "[A-Z]" AND 
         v_cadena.getCharAt(v_indice) NOT MATCHES "[Ñ]"  AND
         v_cadena.getCharAt(v_indice) NOT MATCHES "[#]"  AND
         v_cadena.getCharAt(v_indice) NOT MATCHES "  " THEN

         CASE
            WHEN p_param = "N"
               LET v_mensaje = "El nombre modificado contiene caracteres especiales o inválidos."
            WHEN p_param = "P"
               LET v_mensaje = "El apellido paterno modificado contiene caracteres especiales o inválidos."
            WHEN p_param = "M"
               LET v_mensaje = "El apellido materno modificado contiene caracteres especiales o inválidos."
         END CASE

         LET v_nombre_es_correcto = 0
         EXIT FOR
      END IF
   END FOR

   -- si paso la validación de caracteres especiales
   IF v_nombre_es_correcto = 1 THEN
      -- se verifica que el nombre no tenga mas de un espacio entre sus componentes
      LET v_tokenizer = base.StringTokenizer.create(v_cadena," ")

      -- se construye el nombre auxiliar quitando espacios extra que pudieran haber sido quitados por el tokenizer
      LET v_indice = 1
      WHILE ( v_tokenizer.hasMoreTokens() )
         IF ( v_indice = 1 ) THEN
            -- se concatena el nombre
            LET v_nombre_aux = v_tokenizer.nextToken()
         ELSE
            LET v_nombre_aux = v_nombre_aux || " " || v_tokenizer.nextToken()
         END IF

         LET v_indice = v_indice + 1
      END WHILE

      -- se verifica si el nombre auxiliar es igual al modificado, si no, es porque habia espacios en medio
      IF ( v_cadena <> v_nombre_aux ) THEN
         LET v_mensaje = "El nombre modificado contiene más de un espacio entre sus componentes",
                         "\nModificado: ", v_cadena,
                         "\n\nSe sugiere la siguiente corrección:",
                         "\n", v_nombre_aux
                         --"\n\n¿Desea aplicar la corrección sugerida?"

         MENU "Cambio de nombre"
         ATTRIBUTES ( Style="dialog", COMMENT = v_mensaje, IMAGE = "question" )
            COMMAND "Aceptar"
               LET p_nombre_modificado = v_nombre_aux
               LET v_nombre_es_correcto = TRUE
            EXIT MENU

            COMMAND "Cancelar"
               LET v_mensaje = "El nombre modificado contiene más de un espacio entre sus componentes"
               LET v_nombre_es_correcto = 0
              EXIT MENU
         END MENU
      END IF
   END IF

   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF v_nombre_es_correcto = 0 THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_nombre_es_correcto, p_nombre_modificado
END FUNCTION

-- Función que valida la estructura del RFC
FUNCTION fn_valida_rfc_alt(p_rfc)
   DEFINE p_rfc                         STRING
   DEFINE v_cnt                         SMALLINT
   DEFINE v_rfc_sbs                     STRING
   DEFINE v_bandera                     SMALLINT

   LET v_rfc_sbs = p_rfc.trim()
   DISPLAY "tamaño cadena " || p_rfc.getLength()
   DISPLAY "tamaño cadena trim " || v_rfc_sbs.getLength()

   -- Se valida que el RFC sea de 10 o 13 posiciones
   IF v_rfc_sbs.getLength() <> 10 AND v_rfc_sbs.getLength() <> 13 THEN
      CALL fn_mensaje("Error","El tamaño del RFC debe de ser de 10 o 13 caracteres","")
      LET v_bandera = 0
   ELSE
      -- Se enciende la bandera ne uno indicando que el RFC cumple con la condición de ser de 10 o 13 posiciones
      LET v_bandera = 1
   END IF

   IF v_bandera = 1 THEN
      -- se valida si son 10 o 13 posiciones
      IF v_rfc_sbs.getLength() = 10 THEN
         FOR v_cnt = 1 TO 4
            IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[A-Z]" THEN
               CALL fn_mensaje("Error","Estructura del RFC incorrecta","")
               -- bandera cero que hay error y no pasa a la segunda validacion
               LET v_bandera = 0
               EXIT FOR
            ELSE
               LET v_bandera = 1
            END IF
         END FOR

         IF v_bandera = 1 THEN
            FOR v_cnt = 5 TO 10
               IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[0-9]"  THEN
                  CALL fn_mensaje("Error","Estructura del RFC incorrecta","")
                  -- bandera cero que hay error y no pasa a la segunda validacion
                  LET v_bandera = 0
                  EXIT FOR
               ELSE
                  LET v_bandera = 1
               END IF
            END FOR
         END IF
      ELSE
         FOR v_cnt = 1 TO 4
            IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[A-Z]" THEN
               CALL fn_mensaje("Error","Estructura del RFC incorrecta","")
               -- bandera cero que hay error y no pasa a la segunda validacion
               LET v_bandera = 0
               EXIT FOR
            ELSE
               LET v_bandera = 1
            END IF
         END FOR

         IF v_bandera = 1 THEN
            FOR v_cnt = 5 TO 10
               IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[0-9]"  THEN
                  CALL fn_mensaje("Error","Estructura del RFC incorrecta","")
                  -- bandera cero que hay error y no pasa a la segunda validacion
                  LET v_bandera = 0
                  EXIT FOR
               ELSE
                  LET v_bandera = 1
               END IF
            END FOR
         END IF

         IF v_bandera = 1 THEN
            FOR v_cnt = 11 TO 13
                IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[0-9]" AND
                   p_rfc.getCharAt(v_cnt) NOT MATCHES "[A-Z]"  THEN
                  CALL fn_mensaje("Error","Estructura del RFC incorrecta","")
                  -- bandera cero que hay error y no pasa a la segunda validacion
                  LET v_bandera = 0
                  EXIT FOR
               ELSE
                  LET v_bandera = 1
               END IF
            END FOR
         END IF

      END IF
   END IF
   RETURN v_bandera
END FUNCTION

-- Función que valida el CURP
FUNCTION valida_curp_alt(p_curp)
   DEFINE p_curp                        STRING
   DEFINE v_cnt                         SMALLINT
   DEFINE v_curp_sbs                    STRING
   DEFINE v_bandera                     SMALLINT
   DEFINE v_err_msj                     STRING

   LET v_curp_sbs = p_curp.trim()

   IF v_curp_sbs.getLength() <> 18 THEN
      CALL fn_mensaje("Error","Estructura de la CURP incorrecta, debe tener 18 caracteres","")
   ELSE
      -- inicia primera validacion de la posición 1 a 4
      FOR v_cnt = 1 TO 4
         IF p_curp.getCharAt(v_cnt) NOT MATCHES "[A-Z]" THEN
            LET v_err_msj = "Estructura de la CURP incorrecta en la posición: "||v_cnt
            CALL fn_mensaje("Error",v_err_msj,"")
            -- bandera cero que hay error y no pasa a la segunda validación
            LET v_bandera = 0
            EXIT FOR
         ELSE
            LET v_bandera = 1
         END IF
      END FOR

      -- si no hay error con la primera validación inicia la segunda de la posicion 5 al 10
      IF v_bandera = 1 THEN
         FOR v_cnt = 5 TO 10
            IF p_curp.getCharAt(v_cnt) NOT MATCHES "[0-9]"  THEN
               LET v_err_msj = "Estructura de la CURP incorrecta en la posición: "||v_cnt
               CALL fn_mensaje("Error",v_err_msj,"")
               -- bandera cero que hay error y no pasa a la tercera validacion
               LET v_bandera = 0
               EXIT FOR
            ELSE
               LET v_bandera = 1
            END IF
         END FOR
      END IF

      -- si no hay error en la sgeunda validaciín incia validación de la posición 11 a 16
      IF v_bandera = 1 THEN
         FOR v_cnt = 11 TO 16
            IF p_curp.getCharAt(v_cnt) NOT MATCHES "[A-Z]" THEN
               LET v_err_msj = "Estructura de la CURP incorrecta en la posición: " || v_cnt
               CALL fn_mensaje("Error",v_err_msj,"")
               -- bandera cero que hay error y no pasa a la segunda validación
               LET v_bandera = 0
               EXIT FOR
            ELSE
               LET v_bandera = 1
            END IF
         END FOR
      END IF

      -- si no hay error en la tercera validación incia validación de la posición 17
      IF v_bandera = 1 THEN
         IF p_curp.getCharAt(17) NOT MATCHES "[A-Z]" AND p_curp.getCharAt(17) NOT MATCHES "[0-9]"  THEN
            LET v_err_msj = "Estructura de la CURP incorrecta en la posición: " || 17
            CALL fn_mensaje("Error",v_err_msj,"")

            -- bandera cero que hay error y no pasa a la segunda validación
            LET v_bandera = 0
         ELSE
            LET v_bandera = 1
         END IF
      END IF

      -- si no hay error en la cuarta validación incia validación de la posición 18
      IF v_bandera = 1 THEN
         IF p_curp.getCharAt(18) NOT MATCHES "[0-9]"  THEN
            LET v_err_msj = "Estructura de la CURP incorrecta en la posición: " || 18
            CALL fn_mensaje("Error",v_err_msj,"")

            -- bandera cero que hay error y no pasa a la segunda validación
            LET v_bandera = 0
         ELSE
            LET v_bandera = 1
         END IF
      END IF

   END IF
   RETURN v_bandera
END FUNCTION

{FUNCTION fn_valida_curp_nulo(p_nss,p_curp_nulo)
   DEFINE p_curp_nulo      CHAR(18)
   DEFINE p_nss            CHAR(11)
   DEFINE v_ax_curp_nulo   CHAR(18)
   DEFINE v_si_curp_ceros  SMALLINT

   LET v_si_curp_ceros = 0
   LET v_qry = "SELECT curp
                  FROM afi_derechohabiente
                 WHERE nss = ? "
                  --WHERE nss = ",p_nss

   PREPARE prp_curp_nulo FROM v_qry
   EXECUTE prp_curp_nulo USING p_nss INTO v_ax_curp_nulo

   -- Si esl curp es igual a 18 ceros se actualiza
   IF v_ax_curp_nulo = "000000000000000000" THEN
      LET v_si_curp_ceros = 1
   END IF

   -- Se regresa la bandera, si es uno cumple la estructura de 18 ceros
   RETURN v_si_curp_ceros
END FUNCTION
}

#  Función para actualizar el nombre imss en base
#  a las modificaciones de nombre y/o paterno y/o materno
FUNCTION fn_actualiza_nombre_imss(p_nss) --,p_nombre,p_param)
   DEFINE p_nss                         CHAR(11)
   DEFINE v_nom_imss                    CHAR(40)
   DEFINE v_pat_imss                    CHAR(40)
   DEFINE v_mat_imss                    CHAR(40)
   DEFINE v_ax_nom_imss                 CHAR(50)

   -- Se inicializan las variables
   LET v_nom_imss = " "
   LET v_pat_imss = " "
   LET v_mat_imss = " "

   -- Consulta para armar el nombre_imss
   LET v_s_qryTxt = "SELECT nombre_af,
                            ap_paterno_af,
                            ap_materno_af
                       FROM afi_derechohabiente
                      WHERE nss = ?"

   PREPARE prp_imss FROM v_s_qryTxt
   EXECUTE prp_imss USING p_nss INTO v_nom_imss,
                                     v_pat_imss,
                                     v_mat_imss

   CASE
      WHEN v_nom_imss IS NULL
         LET v_nom_imss = " "
      WHEN v_pat_imss IS NULL
         LET v_pat_imss = " "
      WHEN v_mat_imss IS NULL
         LET v_mat_imss = " "
   END CASE

   -- Se arma el nombre imss
   LET v_ax_nom_imss = v_pat_imss CLIPPED || "$" || v_mat_imss CLIPPED || "$" || v_nom_imss

   LET v_s_qryTxt = "UPDATE afi_derechohabiente 
                        SET nombre_imss = ?
                      WHERE nss         = ? "

   PREPARE prp_exe_imss FROM v_s_qryTxt
   EXECUTE prp_exe_imss USING v_ax_nom_imss,
                              p_nss

   RETURN v_ax_nom_imss
END FUNCTION

PRIVATE FUNCTION fn_valida_caracteres(p_campo)
   DEFINE p_campo                       STRING
   RETURN p_campo.getIndexOf("*",1)
END FUNCTION

FUNCTION fn_ws_adm(bnd_ws,bnd_act,v_curp,v_rfc,v_act_nombre,v_act_paterno,v_act_materno,v_nss)

   DEFINE bnd_ws        SMALLINT
   DEFINE bnd_act       CHAR(1)
   DEFINE v_curp        CHAR(18)
   DEFINE v_rfc         CHAR(13)
   DEFINE v_act_nombre  CHAR(40)
   DEFINE v_act_paterno CHAR(40)
   DEFINE v_act_materno CHAR(40)
   DEFINE v_nss         CHAR(11)

   IF bnd_ws = 1 THEN
   LET v_bnd = 1
   LET v_partner = "1"

   LET v_s_comando = " nohup time fglrun ",
                     "AFIW06"," ",
                     "'",v_bnd         CLIPPED,"'"," ",
                     "'",bnd_act       CLIPPED,"'"," ",
                     "'",v_partner     CLIPPED," ","'"," ",
                     "'",v_rfc         CLIPPED," ","'"," ",
                     "'",v_curp        CLIPPED," ","'"," ",
                     "'",v_act_nombre  CLIPPED," ","'"," ",
                     "'",v_act_paterno CLIPPED," ","'"," ",
                     "'",v_act_materno CLIPPED," ","'"," ",
                     "'",v_nss         CLIPPED,"'"," "--, " ",
                     --" 1>",seg_modulo_bat.ruta_listados clipped,
                     --"/nohup:",g_pid USING "&&&&&",":",
                     --g_proceso_cod   USING "&&&&&",":",
                     --g_opera_cod     USING "&&&&&" ,
                     --" 2>&1 &"
         DISPLAY "comando : ",v_s_comando
         RUN v_s_comando
   END IF
END FUNCTION