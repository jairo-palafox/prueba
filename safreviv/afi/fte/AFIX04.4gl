###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIX04                                                  #
#Objetivo          => CARGA INICIAL                                           #
#Fecha Inicio      => 26/03/2012                                              #
###############################################################################
DEFINE p_archivo     STRING
DEFINE p_modulo      STRING
DEFINE p_usuario     CHAR(20)

MAIN
   LET p_usuario     = ARG_VAL(1)
   LET p_modulo      = ARG_VAL(2)
   LET p_archivo     = ARG_VAL(3)

   DATABASE safre_tmp
   CALL fn_carga_hpl()

   DATABASE safre_viv
   CALL fn_carga_mov()
END MAIN

FUNCTION fn_carga_hpl()
   DEFINE v_comando        STRING

   LET v_comando = "export DBDATE='Y2MD'; ",               
                   "onpladm run job ", p_archivo CLIPPED, ".job -fl -p ",
                    p_modulo
   DISPLAY v_comando
   DISPLAY "INICIA CARGA DE INFORMACIÓN EN TABLA DE PASO"
   RUN v_comando
   DISPLAY "FINALIZA CARGA DE INFORMACIÓN"
END FUNCTION

FUNCTION fn_carga_mov()
   DEFINE v_query    STRING

   LET v_query = "EXECUTE PROCEDURE sp_carga_saldo72()"
   PREPARE v_carga_mov FROM v_query

   DISPLAY "INICIA CARGA DE INFORMACIÓN EN TABLA DE SALDOS72"
   EXECUTE v_carga_mov
   DISPLAY "FINALIZA CARGA DE INFORMACIÓN"
END FUNCTION

