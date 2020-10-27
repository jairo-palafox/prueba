{===============================================================================
Proyecto          => SISTEMA DE AFORE( SAFRE )
Propietario       => E.F.P.
Modulo            => NOT
Programa          => NOTL07
Descripcion       => Programa Lanzado Captura de Campañas
Fecha creacion    => Enero 23, 2020.
================================================================================}

DATABASE safre_viv

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
       v_estado SMALLINT,
       v_descripcion CHAR(255),
       bnd_activa SMALLINT,
       bnd_confirma SMALLINT
DEFINE v_r_nombre CHAR(40),
       v_r_fecha_ini DATE,
       v_r_fecha_fin DATE,
       v_r_descripcion CHAR(250),
       v_r_fecha DATE,
       v_r_usuario_cod CHAR(20),
       v_tot_act SMALLINT

   OPEN WINDOW w_campania WITH FORM "NOTL071.4fd"

      LET v_fecha = TODAY
      LET v_fecha_ini = TODAY
      LET v_fecha_fin = TODAY

      INPUT BY NAME v_nombre,
                    v_fecha_ini,
                    v_fecha_fin,
                    v_descripcion
      WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED, CANCEL = FALSE, ACCEPT = FALSE)

      BEFORE INPUT       
         DISPLAY BY NAME v_fecha, v_fecha_ini, v_fecha_fin 

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
                  IF (v_fecha_ini < v_fecha) THEN
                     CALL fn_mensaje("Atención", "La fecha inicial debe ser posterior a la fecha de hoy", "stop")
                     CONTINUE INPUT
                     NEXT FIELD v_fecha_ini
                  ELSE
                     CALL fn_valida_campana_activa(v_fecha_ini, v_fecha_fin)
                     RETURNING bnd_activa
                     IF (bnd_activa = 1) THEN
                        CALL fn_ventana_confirma ("Atención", "Existe una campaña vigente \n ¿Desea reemplazarla?", "stop")
                        RETURNING bnd_confirma
                        -- Cancela = 0
                        --Confirma 1
                        IF (bnd_confirma = 1) THEN
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
                                   v_estado,
                                   v_fecha,
                                   p_usuario_cod)

                            IF (SQLCA.SQLCODE = 0) THEN
                               CALL fn_mensaje ("Atención", "La campaña se ha actualizado correctamente", "info")
                               EXIT INPUT
                            ELSE
                               CALL fn_mensaje ("Atención", "Ha ocurrido un error y la campaña no fue actualizada", "info")
                            END IF

                            EXIT INPUT
                         END IF
                     ELSE
                     --Si no existe campaña activa, se da de alta nueva campaña, se valida que exista 
                     --campaña pendiente 
                        SELECT COUNT(*)
                        INTO   v_tot_act
                        FROM   not_campana
                        WHERE  estado = 1;

                        --Si la fecha de inicio capturada es mayor a la fecha fin vigente
                        --se queda como pendiente
                        LET v_descripcion = v_descripcion CLIPPED
                        DISPLAY "fecha capturada :", v_fecha_ini 
                        DISPLAY "fecha fin activa :", v_r_fecha_fin
                        IF (v_tot_act > 0) THEN
                            DISPLAY "Se guarda campaña como pendiente"
                            LET v_estado = 2
                        ELSE
                            DISPLAY "Se guarda nueva campaña"
                            LET v_estado = 1
                        END IF

                            INSERT INTO not_campana 
                            VALUES (v_nombre,
                                    v_fecha_ini,
                                    v_fecha_fin,
                                    v_descripcion,
                                    v_estado,
                                    v_fecha,
                                    p_usuario_cod)
                            IF (SQLCA.SQLCODE = 0) THEN
                               CALL fn_mensaje ("Atención", "La campaña se ha capturado correctamente", "info")
                               EXIT INPUT
                            ELSE
                               CALL fn_mensaje ("Atención", "Ha ocurrido un error y la campaña no fue ingresada", "info")
                            END IF
                     END IF
                  END IF
               END IF
            END IF
         ON ACTION cancelar
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_campania
END FUNCTION

FUNCTION fn_valida_campana_activa(p_fecha_ini, p_fecha_fin)
DEFINE p_fecha_ini, p_fecha_fin DATE
DEFINE v_fecha_ini, v_fecha_fin DATE
DEFINE bnd_activa SMALLINT

   LET bnd_activa = 0

   DECLARE cur_fechas CURSOR FOR SELECT fecha_ini, fecha_fin
                                 FROM   not_campana
                                 WHERE  estado = 1
                                 ORDER BY 1,2

   FOREACH cur_fechas INTO v_fecha_ini, v_fecha_fin

      IF (p_fecha_ini <= v_fecha_fin) THEN
         LET bnd_activa = 1
         EXIT FOREACH
      END IF
   END FOREACH

   RETURN bnd_activa
END FUNCTION 