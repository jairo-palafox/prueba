{===============================================================================
Proyecto          => SISTEMA DE AFORE( SAFRE )
Propietario       => E.F.P.
Modulo            => NOT
Programa          => NOTC02
Descripcion       => Programa de consulta de la bitacora de las campañas
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

   CALL fn_consulta_bitacora(p_usuario_cod)

END MAIN

#OBJETIVO: Consultar las campañas anteriores, registradas en la bitácora.
FUNCTION fn_consulta_bitacora(p_usuario_cod)
DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod
DEFINE v_fecha DATE
DEFINE i INTEGER
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

   OPEN WINDOW w_camp_bitacora WITH FORM "NOTC021.4fd"

   DECLARE cur_campanas CURSOR FOR SELECT nombre,
                                          fecha_ini,
                                          fecha_fin,
                                          descripcion, 
                                          fecha_alta,
                                          usuario_alta,
                                          fecha_baja,
                                          usuario_baja
                                   FROM   not_campana_bitacora

   LET i = 1;
   FOREACH cur_campanas INTO arr_campanas[i].*
      LET i = i +1;
   END FOREACH

   LET i = i - 1;

   DISPLAY ARRAY arr_campanas TO scr_anteriores.*
   END DISPLAY 
END FUNCTION 