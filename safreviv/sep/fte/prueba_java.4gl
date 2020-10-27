IMPORT JAVA java.util.regex.Pattern

MAIN
DEFINE patron Pattern

   LET patron = Pattern.compile("[a-zA-Z]+")
   
   IF( patron.matcher("AbCC").matches() )THEN
     DISPLAY "Coincide"
   ELSE
     DISPLAY "No Coincide"
   END IF

END MAIN