######################################################################
#Modulo            =>SRV                                             #
#Programa          =>SRVP07                                          #
#Objetivo          =>Programa que genera en un archivo el estado de  #
#                    cuenta del afiliado                             #
#Fecha inicio      =>21 Marzo 2012                                   #
######################################################################
DATABASE safre_tmp

MAIN
   DEFINE p_ruta              STRING

   LET p_ruta = ARG_VAL(1)
   
   CALL fn_genera_edo_cuenta_masivo(p_ruta)
END MAIN