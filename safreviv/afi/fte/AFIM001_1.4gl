DATABASE safre_viv

GLOBALS "AFIM001.inc"

PRIVATE DEFINE p_usuario                  VARCHAR(20)
PRIVATE DEFINE p_tipo_proc                CHAR(1)
PRIVATE DEFINE p_nombre_menu              CHAR(50)
PRIVATE DEFINE p_id_derechohabiente       DECIMAL(9,0)
PRIVATE DEFINE v_ciclo                    SMALLINT

PRIVATE DEFINE v_datos_saci               datos_saci
PRIVATE DEFINE v_datos_renapo             datos_renapo

PRIVATE DEFINE v_forma                    ui.form
PRIVATE DEFINE v_ventana                  ui.window

   
MAIN

   DEFINE v_ciclo             BOOLEAN
   
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_id_derechohabiente = ARG_VAL(4)

   #CALL STARTLOG(g_usuario CLIPPED ||".CTAC01.log")

   CLOSE WINDOW SCREEN

   #Se limpian los arreglos para evitar el arrastre de datos
   
   INITIALIZE v_datos_saci.* TO NULL
   INITIALIZE v_datos_renapo.* TO NULL

   LET p_nombre_menu = "Actualización en línea"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW afim0011 WITH FORM "AFIM0011"

      LET v_ventana = ui.Window.forName("afim0011")
      LET v_forma = v_ventana.getForm()

      LET v_ciclo = TRUE

      WHILE v_ciclo
         IF p_id_derechohabiente IS NULL OR p_id_derechohabiente <= 0 THEN
            LET p_id_derechohabiente = 0
            WHILE p_id_derechohabiente = 0
               CALL fn_busca_cliente() RETURNING p_id_derechohabiente
            END WHILE
         ELSE
            LET v_ciclo = FALSE
         END IF

         IF p_id_derechohabiente > 0 THEN
            CALL fn_busca_datos_saci()

            CALL captura_curp_renapo()
         END IF

         LET p_id_derechohabiente = NULL
      END WHILE
   CLOSE WINDOW ctac011

END MAIN

PRIVATE FUNCTION fn_busca_cliente()

   DEFINE v_nss                 CHAR(11)
   DEFINE v_curp                CHAR(18)
   DEFINE v_rfc                 CHAR(13)
   DEFINE v_condicion           STRING
   DEFINE v_query               STRING
   DEFINE i                     SMALLINT
   DEFINE v_id_derechohabiente  DECIMAL (9,0)
   DEFINE v_arr_busqueda        DYNAMIC ARRAY OF datos_saci

   INITIALIZE v_arr_busqueda TO NULL

   CALL v_forma.setElementHidden("group2",1)
   CALL v_forma.setElementHidden("group3",1)
   CALL v_forma.setElementHidden("group4",1)
   CALL v_forma.setElementHidden("group5",1)

   CONSTRUCT v_condicion ON nss, rfc, curp
                         FROM nss, rfc, curp
      BEFORE CONSTRUCT
         CLEAR FORM

      ON ACTION ACCEPT
         LET v_nss    = GET_FLDBUF(nss)
         LET v_rfc    = GET_FLDBUF(rfc)
         LET v_curp   = GET_FLDBUF(curp)
         LET INT_FLAG = FALSE

         IF v_nss IS NULL AND
            v_rfc IS NULL AND
            v_curp IS NULL THEN
            CALL fn_mensaje("Actualización en línea", "Debe de ingresar algún campo de búsqueda.", "about")
            NEXT FIELD v_nss
         END IF

         IF v_nss IS NOT NULL THEN
            IF fn_valida_caracteres(v_nss) <> 0 THEN
               CALL fn_mensaje("Actualización en línea", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD v_nss
            END IF
         END IF

         IF v_rfc IS NOT NULL THEN
            IF fn_valida_caracteres(v_rfc) <> 0 THEN
               CALL fn_mensaje("Actualización en línea", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD v_rfc
            END IF
         END IF

         IF v_curp IS NOT NULL THEN
            IF fn_valida_caracteres(v_curp) <> 0 THEN
               CALL fn_mensaje("Actualización en línea", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD v_curp
            END IF
         END IF
         
         LET v_ciclo  = FALSE
         LET INT_FLAG = FALSE
         ACCEPT CONSTRUCT

      ON ACTION CANCEL
         LET v_ciclo  = FALSE
         LET INT_FLAG = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT INT_FLAG THEN
      LET v_query = "SELECT id_derechohabiente, ",
                          " nss,  ",
                          " rfc,  ",
                          " curp, ",
                          " nombre_af, ",
                          " ap_paterno_af, ",
                          " ap_materno_af ",
                     " FROM afi_derechohabiente ",
                    " WHERE ", v_condicion CLIPPED ,
                    " AND tipo_trabajador <> 'V' ",
                    " AND origen_afiliacion <> 'S' "

      PREPARE prp_busqueda FROM v_query
      DECLARE cur_busqueda CURSOR FOR prp_busqueda

      LET i = 1
      FOREACH cur_busqueda INTO  v_arr_busqueda[i].id_derechohabiente,
                                 v_arr_busqueda[i].nss,
                                 v_arr_busqueda[i].rfc,
                                 v_arr_busqueda[i].curp,
                                 v_arr_busqueda[i].nombre,
                                 v_arr_busqueda[i].paterno,
                                 v_arr_busqueda[i].materno
         LET i = i + 1
         IF i > 50 THEN
            CALL fn_mensaje("Actualización en línea",
                            "Acotar más el criterio de búsqueda. \n"||
                            "Se muestran sólo los primeros 50 registros",
                            "about")
            EXIT FOREACH
         END IF
      END FOREACH

      IF i > 1 THEN
         IF i = 2 THEN
            LET v_id_derechohabiente = v_arr_busqueda[1].id_derechohabiente
         ELSE
            OPEN WINDOW afim0012 WITH FORM "AFIM0012" ATTRIBUTES (STYLE="dialog")
               DISPLAY ARRAY v_arr_busqueda TO v_arreglo.*
                  ON ACTION ACCEPT 
                     LET INT_FLAG = FALSE
                     LET v_id_derechohabiente = v_arr_busqueda[ARR_CURR()].id_derechohabiente
                     EXIT DISPLAY

                  ON ACTION CANCEL
                     LET v_id_derechohabiente = 0
                     EXIT DISPLAY
               END DISPLAY
            CLOSE WINDOW afim0012
         END IF
      ELSE
         CALL fn_mensaje("Actualización en línea",
                         "No existe registros con el criterio de búsqueda. \n",
                         "about")
      END IF
   ELSE
      LET v_id_derechohabiente = -1
   END IF

   RETURN v_id_derechohabiente

END FUNCTION

PRIVATE FUNCTION fn_busca_datos_saci()

   DEFINE v_query                STRING

   LET v_query =  "SELECT ", 
                  "afi.nss, ",
                  "afi.rfc, ",
                  "afi.curp, ",
                  "afi.ap_paterno_af, ",
                  "afi.ap_materno_af, ",
                  "afi.nombre_af, ",
                  "afi.f_nacimiento, ",
                  "afi.sexo ",
                  "FROM afi_derechohabiente afi ",
                  "WHERE afi.id_derechohabiente = ? "
   PREPARE exe_consulta_saci FROM v_query

   EXECUTE exe_consulta_saci USING  p_id_derechohabiente
                              INTO  v_datos_saci.nss,
                                    v_datos_saci.rfc,
                                    v_datos_saci.curp,
                                    v_datos_saci.paterno,
                                    v_datos_saci.materno,
                                    v_datos_saci.nombre,
                                    v_datos_saci.f_nacimiento,
                                    v_datos_saci.genero

   CALL v_forma.setElementHidden("group2",0)
   CALL v_forma.setElementHidden("group3",0)

   DISPLAY v_datos_saci.nss                              TO nss
   DISPLAY v_datos_saci.rfc                              TO rfc
   DISPLAY v_datos_saci.curp                             TO curp
   DISPLAY v_datos_saci.paterno                          TO paterno
   DISPLAY v_datos_saci.materno                          TO materno
   DISPLAY v_datos_saci.nombre                           TO nombre
   DISPLAY v_datos_saci.f_nacimiento USING 'yyyy-mm-dd'  TO f_nacimiento
   DISPLAY v_datos_saci.genero                           TO genero
   
END FUNCTION

PRIVATE FUNCTION captura_curp_renapo()

   CALL v_forma.setElementHidden("lbl_curp_renapo",1)
   CALL v_forma.setElementHidden("estatus_renapo",1)
   CALL v_forma.setElementHidden("cve_estatus_curp",1)
   CALL v_forma.setElementHidden("des_estatus_curp",1)

   CALL fn_mensaje("TPM","ESPERA","about")
   
END FUNCTION

PRIVATE FUNCTION fn_valida_caracteres(p_campo)
   DEFINE p_campo       STRING
   RETURN p_campo.getIndexOf("*",1)
END FUNCTION