##################################################################################
#Modulo             => OCG                                                       #
#Programa           => OCGL15                                                    #
#Objetivo           => Programa para consulta de unificación                     #
#Autor              => J. Eduardo Ventura                                        #
#Fecha inicio       => 13 Junio del 2018                                         #
##################################################################################
DATABASE safre_viv

GLOBALS

DEFINE p_usuario         CHAR(40)
DEFINE p_tpo_ejecucion   SMALLINT
DEFINE p_s_titulo        CHAR(40)

DEFINE v_f_ini           DATE
DEFINE v_f_fin           DATE
DEFINE v_s_qry           STRING
DEFINE a                 INTEGER
DEFINE v_desc            SMALLINT

DEFINE arr_unifica DYNAMIC ARRAY OF RECORD
   nss_unificador  CHAR(11),
   nss_unificado   CHAR(11),
   f_unificacion   DATE,
   ind_recaudacion char(40),
   folio_unificacion INTEGER
END RECORD

END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parámetro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC23.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW OCGC231 WITH FORM "OCGC231"

   INPUT BY NAME v_f_ini,v_f_fin ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

   IF v_f_ini IS NULL THEN
    LET v_f_ini = '01/01/2018'
   END IF

   IF v_f_fin IS NULL THEN
      LET v_f_fin = TODAY
   END IF

   LET v_s_qry = "SELECT o.nss_unificador,
          o.nss_unificado,
          o.f_unificacion,
          c.ind_desc,
          o.folio_unificacion
     FROM ocg_unificacion o,
          cat_ocg_ind_recaudacion c
    WHERE c.ind_recaudacion = o.ind_recaudacion
      AND f_unificacion BETWEEN ", "'",v_f_ini ,"'"," AND ","'",v_f_fin,"'"

   PREPARE prp_unifica FROM v_s_qry
   DECLARE cur_unifica CURSOR FOR prp_unifica

   LET a = 1

   FOREACH cur_unifica INTO arr_unifica[a].nss_unificador,
                            arr_unifica[a].nss_unificado,
                            arr_unifica[a].f_unificacion,
                            arr_unifica[a].ind_recaudacion,
                            arr_unifica[a].folio_unificacion

      LET a = a + 1
       
   END FOREACH

   CALL arr_unifica.deleteElement(a)   

   OPEN WINDOW OCGC232 WITH FORM "OCGC232"

   DISPLAY ARRAY arr_unifica TO tab_unificacion.*

   ON ACTION CANCEL

   EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW OCGC232

   ON ACTION CANCEL

   EXIT INPUT 
   END INPUT

   CLOSE WINDOW OCGC231

END MAIN
