--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01/05/2012
--===============================================================
DATABASE safre_viv

MAIN
DEFINE p_entidad             CHAR(40),
       p_id_registro_entidad INTEGER


   LET p_entidad = ARG_VAL(1)
   LET p_id_registro_entidad = ARG_VAL(2) 
   CALL fn_consulta_historicos(p_entidad, p_id_registro_entidad)

END MAIN



################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPG01                                                   #
#Descripcion       => Consulta el historicos de modificaciones                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 07 Mayo 2012                                             #
################################################################################
FUNCTION fn_consulta_historicos(p_entidad, p_id_registro_entidad)
DEFINE p_entidad             CHAR(40),
       p_id_registro_entidad INTEGER,
       v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,
       v_registros           DYNAMIC ARRAY OF RECORD
         v_num               INTEGER,
         v_etiqueta          LIKE sep_cat_dato_actualizado.etiqueta,
         v_f_modificacion    DATE,
         v_valor_modificado  VARCHAR(40),
         v_valor_actual      VARCHAR(40),
        v_usuario            VARCHAR(20)
       END RECORD,
       v_ventana ui.Window
       
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep' 

   OPEN WINDOW vtna_consulta_historico WITH FORM v_ruta_ejecutable CLIPPED||"/SEPG011"
      ATTRIBUTE(STYLE = "dialog")
      #Se asigna el titulo de la ventana
      CALL ui.Interface.setText("Historia de Modificaciones")
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText("Historia de Modificaciones")
      

      DISPLAY ARRAY v_registros TO sr_historia.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE DISPLAY 
            CALL fn_recupera_historicos(p_entidad, p_id_registro_entidad) RETURNING v_registros

         ON ACTION aceptar
            ACCEPT DISPLAY 

      END DISPLAY

   CLOSE WINDOW vtna_consulta_historico


END FUNCTION


################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPG01                                                   #
#Descripcion       => Recupera el historicos de modificaciones                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 07 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_historicos(p_entidad, p_id_registro_entidad)
DEFINE p_entidad             CHAR(40),
       p_id_registro_entidad INTEGER,
       v_consulta STRING,
       v_con_registros     RECORD
        v_ind              SMALLINT,
        v_diag             CHAR(3),
        v_etiqueta         LIKE sep_cat_dato_actualizado.etiqueta,
        v_f_modificacion   DATE,
        v_valor_modificado VARCHAR(40),
        v_valor_actual     VARCHAR(40),
        v_usuario          VARCHAR(20)
       END RECORD,
       v_registros DYNAMIC ARRAY OF RECORD
        v_num              INTEGER,
        v_etiqueta         LIKE sep_cat_dato_actualizado.etiqueta,
        v_f_modificacion   DATE,
        v_valor_modificado VARCHAR(40),
        v_valor_actual     VARCHAR(40),
        v_usuario          VARCHAR(20)
       END RECORD,
       v_indice INTEGER
       

   WHENEVER ERROR CONTINUE
   # se ejecuta el SP que recupera la historia de modificaciones para determinada entidad
   LET v_consulta = "EXECUTE FUNCTION fn_sep_recupera_historia(?,?)"
   PREPARE prp_recupera_historia FROM v_consulta
   DECLARE cur_recupera_historia CURSOR FOR prp_recupera_historia
   LET v_indice = 1
   # solo se conservan los campos que se desplegaran
   FOREACH cur_recupera_historia USING p_entidad, p_id_registro_entidad
                                  INTO v_con_registros.*
                                  
      LET v_registros[v_indice].v_num              = v_indice
      LET v_registros[v_indice].v_etiqueta         = v_con_registros.v_etiqueta
      LET v_registros[v_indice].v_f_modificacion   = v_con_registros.v_f_modificacion
      LET v_registros[v_indice].v_valor_actual     = v_con_registros.v_valor_actual
      LET v_registros[v_indice].v_valor_modificado = v_con_registros.v_valor_modificado
      LET v_registros[v_indice].v_usuario          = v_con_registros.v_usuario
      
     LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_historia
   
   RETURN v_registros
END FUNCTION