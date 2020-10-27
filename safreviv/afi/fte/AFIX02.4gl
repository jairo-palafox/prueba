###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIX01                                                  #
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

   DISPLAY "INICIA CARGA DE INFORMACIÓN EN TABLA DE PASO"
   RUN v_comando
   DISPLAY "FINALIZA CARGA DE INFORMACIÓN"
END FUNCTION

FUNCTION fn_carga_mov()
   DEFINE v_query    STRING
   DEFINE v_folio    DECIMAL(9,0)

   LET v_query = "EXECUTE PROCEDURE sp_carga_inicial_mov()"
   PREPARE v_carga_mov FROM v_query

   DISPLAY "INICIA CARGA DE INFORMACIÓN EN TABLA DE MOVIMIENTOS"
   EXECUTE v_carga_mov INTO v_folio
   DISPLAY "FINALIZA CARGA DE INFORMACIÓN  FOLIO: ",v_folio
END FUNCTION