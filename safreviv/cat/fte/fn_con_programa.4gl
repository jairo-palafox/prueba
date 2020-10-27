FUNCTION fn_consulta_programa(p_modulo_cod)
   DEFINE p_modulo_cod   LIKE seg_programa.modulo_cod
   DEFINE marr_programa  DYNAMIC ARRAY OF RECORD
             programa_cod  LIKE seg_programa.programa_cod ,
             programa_desc LIKE seg_programa.programa_desc
          END RECORD
   DEFINE v_cont         INTEGER
   DEFINE v_pos_sel      INTEGER
   DEFINE v_sql          STRING
   
   IF LENGTH(p_modulo_cod CLIPPED) = 0 THEN
      CALL fgl_winmessage("Advertencia","EL MODULO ES REQUERIDO PARA ESTA CONSULTA,\nVERIFICARLO CON EL ADMINISTRADOR","exclamation")
      RETURN NULL
   END IF
   
   LET v_sql = "SELECT programa_cod, programa_desc",
               "  FROM seg_programa",
               " WHERE modulo_cod = '",p_modulo_cod,"'",
               " ORDER BY 1"
   
   PREPARE EnuConProg FROM v_sql
   DECLARE CurConProg CURSOR FOR EnuConProg
   
   LET v_cont    = 1   
   LET v_pos_sel = 0
   FOREACH CurConProg INTO marr_programa[v_cont].*
      LET v_cont = v_cont + 1
   END FOREACH

   IF(marr_programa[marr_programa.getLength()].programa_cod IS NULL OR 
      marr_programa[marr_programa.getLength()].programa_desc IS NULL)THEN
      CALL marr_programa.deleteElement(marr_programa.getLength())
   END IF
   
   IF v_cont > 1 THEN
      
      OPEN WINDOW vtn_con_programa WITH FORM "CATM014"
      
      DISPLAY ARRAY marr_programa TO tbl_programa.*
         ATTRIBUTES (UNBUFFERED)
         
         ON ACTION accept
            LET v_pos_sel = ARR_CURR()
            EXIT DISPLAY
         ON ACTION cancel
            EXIT DISPLAY
      END DISPLAY
      
      CLOSE WINDOW vtn_con_programa
      
   END IF
   
   IF v_pos_sel > 0 THEN
      RETURN marr_programa[v_pos_sel].programa_cod
   ELSE
      RETURN NULL
   END IF
   
END FUNCTION