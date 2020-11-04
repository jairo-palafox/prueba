
IMPORT com
GLOBALS "AEXWS02.inc"

#
# USER GLOBALS VARIABLES
#
DATABASE safre_viv
GLOBALS
  DEFINE
    solicitud RECORD
      fecha string
    END RECORD,
    datos_out DYNAMIC ARRAY OF RECORD
      cod_segmento  SMALLINT,
      des_corta     CHAR(30)
    END RECORD
     
  DEFINE
    divide_by_zero String ATTRIBUTES (XMLName="DividedByZero")
   
END GLOBALS

DEFINE serverURL STRING
DEFINE screen    BOOLEAN          

&define display_status(status) \
  IF NOT screen THEN \
    DISPLAY status \
  ELSE \
    DISPLAY status TO MSG \
    MENU ON idle 1 EXIT MENU ON ACTION close EXIT PROGRAM END MENU \
  END IF

#
# MAIN
#
MAIN
    DEFINE ret INTEGER

    CALL CreateSegService()
  
    DISPLAY "Starting server..."   

    CALL com.WebServiceEngine.Start()

    DISPLAY "The server is listening."

    WHILE TRUE
      # Process each incoming requests (infinite loop)
      LET ret = com.WebServiceEngine.ProcessServices(-1)

      DISPLAY "Processing request..."

      CASE ret
          WHEN 0
              DISPLAY "Request processed." 
          WHEN -1
              DISPLAY "Timeout reached."
          WHEN -2
              DISPLAY "Disconnected from application server."
              EXIT PROGRAM 
          WHEN -3
              DISPLAY "Client Connection lost."
          WHEN -4
              DISPLAY "Server interrupted with Ctrl-C."
          WHEN -10
              DISPLAY "Internal server error."
      END CASE

      IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
      END IF 
    END WHILE

    DISPLAY "Server stopped"
END MAIN

#
# Create Nss RPC/Literal service
#

FUNCTION CreateSegService()
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  

    
  LET serv = com.WebService.CreateWebService("ConsultaSegmentos","http://www.infonavit.org.mx/")

  LET op = com.WebOperation.CreateDOCStyle("fn_consulta_segmentos","ConsultaSegmentos",solicitud,datos_out)
  CALL serv.publishOperation(op,NULL)

  CALL com.WebServiceEngine.RegisterService(serv) 

  
END FUNCTION



#
# USER PUBLIC FUNCTIONS
#
-- funcion que consulta un NSS y regresa los datos Base CURP
FUNCTION fn_consulta_segmentos()
DEFINE v_sql             VARCHAR(1000)
DEFINE lref_CatSegmentos RECORD
       cod_segmento      SMALLINT,
       des_corta         CHAR(30)
       END RECORD
DEFINE v_resul_op        CHAR(02),
       li_pos       INTEGER

    -- inicio de variables
   
    -- se busca monto maximo y minimo
    CALL datos_out.CLEAR()
    LET v_sql = "SELECT cod_segmento, des_corta FROM aex_cat_segmento WHERE f_baja IS NULL"

               
    DISPLAY "Consulta Formada:", v_sql
   
    PREPARE pre_obt_datos FROM v_sql
    LET li_pos = 0
    DECLARE Crs_ObtRegCatSegmentos CURSOR FOR pre_obt_datos
         FOREACH Crs_ObtRegCatSegmentos INTO lref_CatSegmentos.*
            LET li_pos = li_pos + 1
            LET datos_out[li_pos].* = lref_CatSegmentos.* 
         END FOREACH


    -- se verfica si existe informacion
    IF SQLCA.SQLERRD[3] = 0 THEN
       LET v_resul_op = '02' -- no existe informacion a procesar
    ELSE
       LET v_resul_op = '01'
    END IF
    



    

END FUNCTION
