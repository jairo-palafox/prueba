######################################################################
#Modulo        => AFI                                                #
#Programa      => AFIC017                                            #
#Descripción   => Alertamiento de consultas rojas                    #
#Autor         => Jose Eduardo Ventura Bonola                        #
#Fecha         => 07/MARZO DE 2015                                   #
######################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario                CHAR(20)      -- Obtiene dato de usuario
   DEFINE p_tipo_ejecucion         SMALLINT      -- Forma como ejecutará el programa
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE w ui.Window
   DEFINE f ui.Form
   DEFINE v_alerta_roja            CHAR(500)     --guarda datos de afi_alerta_roja

END GLOBALS

MAIN
   -- se recupera la clave de usuario desde parámetro
   LET p_usuario          = ""
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".AFIC10.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   MENU "ACCIÓN A REALIZAR"

   ON ACTION nuevo
      CALL fn_nva_alerta()
      EXIT MENU

   ON ACTION consulta
      CALL fn_consulta_alerta()
      EXIT MENU

   ON ACTION CANCEL
      EXIT MENU

   END MENU

END MAIN

FUNCTION fn_nva_alerta()
   
   OPEN WINDOW alerta WITH FORM "AFIC101"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   CALL f.setElementHidden("tabla",1)
   CALL f.setElementHidden("et_his",1)

   INPUT BY NAME v_alerta_roja ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      IF v_alerta_roja IS NULL THEN
         CALL fn_mensaje("Aviso","Debe ingresar algún texto de alerta para consultas rojas","info")
         NEXT FIELD v_alerta_roja
      END IF
      INSERT INTO afi_alerta_roja VALUES (v_alerta_roja,TODAY,p_usuario)
      CALL fn_mensaje("Aviso","Mensaje de alerta fue almacenado de forma correcta","info")
      EXIT INPUT

   ON ACTION CANCEL 
      EXIT INPUT
   END INPUT
   CLOSE WINDOW alerta   
END FUNCTION

FUNCTION fn_consulta_alerta()

   DEFINE v_qry_alerta STRING
   DEFINE i            INTEGER
   DEFINE arr_consulta_alerta DYNAMIC ARRAY OF RECORD
             alertamiento CHAR(500),
             f_actualiza  DATE,
             usuario      CHAR(20)
             END RECORD

   OPEN WINDOW alerta WITH FORM "AFIC101"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   CALL f.setFieldHidden("v_alerta_roja",1)
   CALL f.setElementHidden("nva_alerta",1)
   
   LET v_qry_alerta = "SELECT * FROM afi_alerta_roja"

   PREPARE prp_alerta FROM v_qry_alerta
   DECLARE cur_alerta CURSOR FOR prp_alerta

   LET i = 1
   FOREACH cur_alerta INTO arr_consulta_alerta[i].*
   LET i = i + 1
   END FOREACH

   IF arr_consulta_alerta[arr_consulta_alerta.getLength()].alertamiento IS NULL AND
      i > 1 THEN
      CALL arr_consulta_alerta.deleteElement(arr_consulta_alerta.getLength())
   END IF
   
   DISPLAY ARRAY arr_consulta_alerta TO registros.*

   ON ACTION ACCEPT 
      EXIT DISPLAY

   ON ACTION CLOSE
      EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW alerta

END FUNCTION