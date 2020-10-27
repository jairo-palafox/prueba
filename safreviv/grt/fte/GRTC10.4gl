--=======================================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--=======================================================================

#########################################################################
#Módulo             => GRT                                              #
#Programa           => GRTC10                                           #
#Objetivo           => Programa que realiza la validación de usuario    #
#                      mediante el RFC para poder consultar información #
#                      de trabajadores con crédito Apoyo Infonavit.     #
#Autor              => Emilio Abarca, EFP                               #
#Fecha inicio       => 26/Diciembre/2018                                #
#########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario_cod         CHAR(20)
   DEFINE g_tipo_ejecucion      SMALLINT 
   DEFINE g_s_titulo            STRING
   DEFINE g_ruta_bin            CHAR(40)

END GLOBALS

MAIN

   -- Parámetros recibidos
   LET g_usuario_cod        = ARG_VAL(1)
   LET g_tipo_ejecucion     = ARG_VAL(2)
   LET g_s_titulo           = ARG_VAL(3)

   CLOSE WINDOW SCREEN

   -- se crea el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".GRTC10.log")

   -- si se obtuvo el título, se pone como título de programa
   IF ( g_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_s_titulo)
   END IF

   SELECT ruta_bin
     INTO g_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'grt'

   CALL fn_valida_usuario()

END MAIN

FUNCTION fn_valida_usuario()

   DEFINE v_in_usuario_cod   CHAR(20)
   DEFINE v_rfc              CHAR(13)
   DEFINE v_indicador        SMALLINT
   DEFINE v_s_comando        STRING
   DEFINE r_acceso           RECORD
      usuario_cod        CHAR(20),
      rfc                CHAR(13),
      cve_ent_financiera SMALLINT,
      estado             SMALLINT
   END RECORD 

   OPEN WINDOW vtn_acceso WITH FORM "GRTC101"
      INPUT BY NAME v_in_usuario_cod,v_rfc ATTRIBUTES(UNBUFFERED,WITHOUT DEFAULTS)

         BEFORE INPUT
            LET v_in_usuario_cod = NULL
            LET v_rfc = NULL 

         ON ACTION ACCEPT

            INITIALIZE r_acceso.* TO NULL

            -- Validación de campos
            IF(v_in_usuario_cod IS NULL) THEN
               CALL fn_mensaje("","Favor de ingresar su usuario","")
               NEXT FIELD v_in_usuario_cod
            ELSE
               LET v_indicador = fn_valida_campo(v_in_usuario_cod)
               IF(v_indicador = 1) THEN
                  CALL fn_mensaje("","El usuario no debe contener caracteres especiales","")
                  NEXT FIELD v_in_usuario_cod
               END IF 
            END IF 

            LET v_indicador = 0

            IF(v_rfc IS NULL) THEN
               CALL fn_mensaje("","Favor de ingresar su RFC","")
               NEXT FIELD v_rfc
            ELSE
               LET v_indicador = fn_valida_campo(v_rfc)
               IF(v_indicador = 1) THEN
                  CALL fn_mensaje("","El RFC no debe contener caracteres especiales","")
                  NEXT FIELD v_rfc
               END IF 
            END IF

            -- Valida que exista el usuario.
            SELECT s.usuario_cod,
                   s.rfc,
                   s.cve_ent_financiera,
                   c.estado
              INTO r_acceso.usuario_cod,
                   r_acceso.rfc,
                   r_acceso.cve_ent_financiera,
                   r_acceso.estado
              FROM safre_cpb:seg_usuario_ef s,
                   cat_usuario_ef c
             WHERE s.usuario_cod = v_in_usuario_cod
               AND s.rfc = v_rfc
               AND s.usuario_cod = c.usuario_cod
               AND s.rfc = c.rfc;

            IF(r_acceso.usuario_cod IS NULL) THEN
               CALL fn_mensaje("","EL USUARIO NO ESTÁ REGISTRADO","")
               NEXT FIELD v_in_usuario_cod
            END IF

            IF(r_acceso.estado <> 10) THEN
               CALL fn_mensaje("","EL USUARIO ESTÁ DADO DE BAJA","")
               NEXT FIELD v_in_usuario_cod
            END IF

            -- Ejecuta consulta de acreditados Apoyo Infonavit
            LET v_s_comando = "fglrun ",g_ruta_bin CLIPPED,"/GRTC09 ",g_usuario_cod," ",g_tipo_ejecucion," ",
                                        "'",g_s_titulo CLIPPED,"'"," ",r_acceso.rfc," ",r_acceso.cve_ent_financiera

            RUN v_s_comando

         ON ACTION CANCEL
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtn_acceso

END FUNCTION

FUNCTION fn_valida_campo(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE v_idx      INTEGER
   DEFINE v_r_ind    BOOLEAN

   LET p_cadena = p_cadena CLIPPED

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') OR 
        (p_cadena.subString(v_idx,v_idx) MATCHES '[A-Z]') THEN
         LET v_r_ind = 0
      ELSE
         LET v_r_ind = 1
         EXIT FOR
      END IF
   END FOR

   RETURN v_r_ind

END FUNCTION
