

FUNCTION SelArchAnu()
DEFINE w_sw,i           SMALLINT,
       w_sw10,
       w_sw11           VARCHAR(150),
       w_len            smallint,      
       ch               base.Channel,
       w_nomarch        varchar(150)
     

   call winopendir("C:/expimpanu","Archivos de texto")
      RETURNING w_sw10

   IF w_sw10 IS NULL THEN
      RETURN NULL
   END IF 

   DISPLAY w_sw10 TO ruta
   
   LET w_len = length(w_sw10)
   FOR i = w_len TO 1 STEP -1
       IF w_sw10[i] = ":" THEN
          LET w_nomarch = w_sw10[i+1,w_len]
          EXIT FOR
       END IF 
   END FOR 
   
   RETURN w_nomarch                        
END FUNCTION