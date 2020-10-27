#######################################################################
#Modulo              => GRT                                           #
#Programa            => GRTC103                                       #
#Objetivo            => Programa que realiza la consulta general      #
#                       de ubicación de registros Garantía 43 bis     #
#                       del SP002 Formaliación                        #
#Autor               => Héctor F. Jiménez Lara                        #
#Fecha inicio        => 27 Octubre 2015                               #
#######################################################################
DATABASE safre_viv

MAIN
   DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo          STRING   -- titulo de la ventana
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod
   DEFINE v_cmd               STRING
   DEFINE v_bnd_sp            CHAR(3)
   DEFINE v_cod_programa      LIKE seg_programa.programa_cod

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTC103.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   LET v_cod_programa = "GRTC101"
   LET v_bnd_sp       = "002"

   -- Se ejecuta el programa que realiza la consulta 
   LET v_cmd = "fglrun " || v_cod_programa   CLIPPED ||" "
                         || p_usuario_cod    CLIPPED ||" "
                         || p_tipo_ejecucion CLIPPED ||" "
                   ||'"' || p_s_titulo       CLIPPED ||'" '
                         || v_bnd_sp                 || "\n\n"

   DISPLAY v_cmd
   RUN v_cmd

END MAIN