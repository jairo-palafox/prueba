{===============================================================================
Proyecto          => SISTEMA DE AFORE( SAFRE )
Propietario       => E.F.P.
Modulo            => NOT
Programa          => NOTC02
Descripcion       => Programa de consulta y modificación de campañas
Fecha creacion    => Enero 23, 2020.
================================================================================}

DATABASE safre_viv
GLOBALS
DEFINE arr_campanas DYNAMIC ARRAY OF RECORD
       v_nombre CHAR(40),
       v_fecha_ini DATE,
       v_fecha_fin DATE,
       v_descripcion CHAR(255),
       v_fecha_alta DATE,
       v_usuario_alta CHAR(20),
       v_fecha_baja DATE,
       v_usuario_baja CHAR(20)
END RECORD

END GLOBALS
MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   CALL fn_captura(p_usuario_cod)

END MAIN

FUNCTION fn_captura(p_usuario_cod)
DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod
DEFINE v_fecha DATE,
       v_nombre CHAR(40),
       v_fecha_ini DATE,
       v_fecha_fin DATE,
       v_descripcion CHAR(255),
       bnd_activa SMALLINT,
       bnd_confirma SMALLINT
DEFINE v_r_nombre CHAR(40),
       v_r_fecha_ini DATE,
       v_r_fecha_fin DATE,
       v_r_descripcion CHAR(250),
       v_r_fecha DATE,
       v_r_usuario_cod CHAR(20),
       v_estado SMALLINT,
       v_tot_act SMALLINT

   OPEN WINDOW w_campania WITH FORM "NOTC011.4fd"

      LET v_fecha = TODAY
      LET v_fecha_ini = TODAY
      LET v_fecha_fin = TODAY
      -- 
      INPUT BY NAME v_nombre,
                    v_fecha_ini,
                    v_fecha_fin,
                    v_descripcion
      WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED, CANCEL = FALSE, ACCEPT = FALSE)


      BEFORE INPUT
         SELECT nombre,
                fecha_ini,
                fecha_fin,
                descripcion,
                COUNT(*)
         INTO v_nombre,
              v_fecha_ini,
              v_fecha_fin,
              v_descripcion,
              v_tot_act
         FROM   not_campana
         WHERE estado = 1
         GROUP BY 1,2,3,4;

         IF (v_tot_act = 0) THEN
            CALL fn_mensaje ("Atención", "No existen campañas activas en este momento", "info")
         ELSE
            DISPLAY BY NAME v_fecha,v_nombre,
                            v_fecha_ini,
                            v_fecha_fin,
                            v_descripcion
         END IF

            CALL fn_valida_campanas()
            CALL fn_consulta_todas()

            DISPLAY ARRAY arr_campanas TO scr_campanas.*
            END DISPLAY 

            ON ACTION ACCEPT
               IF (v_nombre IS NULL) OR
                  (v_fecha_ini IS NULL) OR
                  (v_fecha_fin IS NULL) OR
                  (v_descripcion IS NULL) THEN

                  CALL fn_mensaje ("Atención", "Todos los campos son requeridos", "info")
                  CONTINUE INPUT
                  NEXT FIELD v_fecha_ini

               ELSE
                  IF (v_fecha_fin < v_fecha_ini) THEN
                     CALL fn_mensaje ("Atención", "La fecha final debe ser posterior a la fecha inicial", "info")
                     CONTINUE INPUT
                     NEXT FIELD v_fecha_fin
                  ELSE
                     IF (v_fecha_fin < v_fecha_ini) THEN
                        CALL fn_mensaje ("Atención", "La fecha final debe ser posterior a la fecha inicial", "info")
                        CONTINUE INPUT
                        NEXT FIELD v_fecha_fin
                     ELSE
                        CALL fn_ventana_confirma ("Atención", "¿Desea modificar la campaña vigente?", "stop")
                        RETURNING bnd_confirma
                        --Cancala = 0
                        --Confirma 1
                        DISPLAY "confirma : ", bnd_confirma
                        IF (bnd_confirma = 1) THEN  
                           LET v_descripcion = v_descripcion CLIPPED
                           --Estado 1-Activo, 2-Pendiente
                           LET v_estado = 1;

                           SELECT nombre,
                                  fecha_ini,
                                  fecha_fin,
                                  descripcion,
                                  fecha_modifica,
                                  usuario_modifica
                           INTO   v_r_nombre,
                                  v_r_fecha_ini,
                                  v_r_fecha_fin,
                                  v_r_descripcion,
                                  v_r_fecha,
                                  v_r_usuario_cod
                           FROM   not_campana
                           WHERE  estado = 1;

                           INSERT INTO not_campana_bitacora
                           VALUES (v_r_nombre,
                                  v_r_fecha_ini,
                                  v_r_fecha_fin,
                                  v_r_descripcion,
                                  v_r_fecha,
                                  v_r_usuario_cod,
                                  TODAY,
                                  p_usuario_cod)

                           DELETE FROM not_campana
                           WHERE  estado = 1;

                           INSERT INTO not_campana 
                           VALUES (v_nombre,
                                   v_fecha_ini,
                                   v_fecha_fin,
                                   v_descripcion,
                                   1,
                                   v_fecha,
                                   p_usuario_cod)

                           IF (SQLCA.SQLCODE = 0) THEN
                              CALL fn_mensaje ("Atención", "La campaña se ha modificado correctamente", "info")
                              EXIT INPUT
                           ELSE
                              CALL fn_mensaje ("Atención", "Ha ocurrido un error y la campaña no fue modificada", "info")
                           END IF

                           EXIT INPUT 
                        END IF
                     END IF
                  END IF
               END IF

         ON ACTION cancelar
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_campania

END FUNCTION

FUNCTION fn_consulta_todas()
DEFINE bnd_activa SMALLINT
DEFINE i INTEGER

   LET bnd_activa = 0

   DECLARE cur_vig CURSOR FOR SELECT nombre,
                                     fecha_ini,
                                     fecha_fin       ,
                                     descripcion     ,
                                     fecha_modifica  ,
                                     usuario_modifica,
                                     "",
                                     ""
                              FROM   not_campana
   LET i = 1;
   FOREACH cur_vig INTO arr_campanas[i].*
   DISPLAY  arr_campanas[i].*, " - ", i 
      LET i = i +1;
   END FOREACH
DISPLAY "TOTAL DE VIGENTES : ", i
   --IF i > 1 THEN
      --LET i = i - 1;
   --END IF

   DECLARE cur_ant CURSOR FOR SELECT nombre,
                                     fecha_ini,
                                     fecha_fin,
                                     descripcion, 
                                     fecha_alta,
                                     usuario_alta,
                                     fecha_baja,
                                     usuario_baja
                              FROM   not_campana_bitacora

   FOREACH cur_ant INTO arr_campanas[i].*
      LET i = i +1;
   END FOREACH

   LET i = i - 1;

END FUNCTION 

FUNCTION fn_valida_campanas()
DEFINE arr_valida RECORD 
       v_r_nombre CHAR(40),
       v_r_fecha_ini DATE,
       v_r_fecha_fin DATE,
       v_r_descripcion CHAR(250),
       v_r_fecha DATE,
       v_r_usuario_cod CHAR(20)
END RECORD

DEFINE v_r_nombre CHAR(40),
       v_r_fecha_ini DATE,
       v_r_fecha_fin DATE,
       v_r_descripcion CHAR(250),
       v_r_fecha DATE,
       v_r_usuario_cod CHAR(20)

DEFINE v_fecha_hoy DATE,
       i INTEGER,
       v_tot_pendientes INTEGER, 
       v_fecha_ini_p DATE

   LET v_fecha_hoy = TODAY

   DECLARE cur_valida CURSOR FOR SELECT nombre,
                                        fecha_ini,
                                        fecha_fin,
                                        descripcion,
                                        fecha_modifica,
                                        usuario_modifica
                                 FROM   not_campana

   FOREACH cur_valida INTO arr_valida.*
   DISPLAY "arr_valida.v_r_fecha_fin : ",arr_valida.v_r_fecha_fin 
   DISPLAY "v_fecha_hoy :",v_fecha_hoy 

      IF (arr_valida.v_r_fecha_fin < v_fecha_hoy ) THEN
         SELECT nombre,
                fecha_ini,
                fecha_fin,
                descripcion,
                fecha_modifica,
                usuario_modifica
         INTO   v_r_nombre,
                v_r_fecha_ini,
                v_r_fecha_fin,
                v_r_descripcion,
                v_r_fecha,
                v_r_usuario_cod
         FROM   not_campana
         WHERE  estado = 1;

         INSERT INTO not_campana_bitacora
         VALUES (v_r_nombre,
                 v_r_fecha_ini,
                 v_r_fecha_fin,
                 v_r_descripcion,
                 v_r_fecha,
                 v_r_usuario_cod,
                 TODAY,
                 "OPISACI");

         DELETE FROM not_campana
         WHERE  estado = 1;

         SELECT COUNT(*)
         INTO   v_tot_pendientes
         FROM   not_campana
         WHERE  estado = 2;

         IF (v_tot_pendientes >= 1) THEN
            SELECT MIN(fecha_ini)
            INTO   v_fecha_ini_p
            FROM   not_campana
            WHERE  estado = 2

            UPDATE not_campana
            SET estado = 1
            WHERE fecha_ini = v_fecha_ini_p
         END IF
      ELSE
         DISPLAY "La fecha fin NO es MENOR a la fecha de hoy"
         DISPLAY arr_valida.v_r_fecha_fin 
         DISPLAY  v_fecha_hoy 
      END IF

      LET i = i +1;
   END FOREACH

END FUNCTION