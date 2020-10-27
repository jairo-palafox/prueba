######################################################################
#Modulo            =>SRV                                             #
#Programa          =>SRVP09                                          #
#Objetivo          =>Programa que genera en un archivo el estado de  #
#                    cuenta del afiliado                             #
#                    esta pantalla solo es para que se pueda ejecutar#
#                    desde la pantalla de saldos                     #
#Fecha inicio      =>21 Marzo 2012                                   #
######################################################################

DATABASE safre_viv

MAIN
   DEFINE p_nss               LIKE afi_derechohabiente.nss
   #DEFINE p_ruta              STRING
   DEFINE v_archivo           STRING
   LET p_nss = ARG_VAL(1)
   #LET p_ruta = ARG_VAL(2)

   #OPEN WINDOW srvp09 WITH FORM "SRVP09"
   #CALL ui.Interface.setText("Detalle de Movimientos")
   #CALL ui.interface.refresh()
   
   #CALL fn_archivo_edo_cuenta(p_nss, p_ruta) RETURNING v_archivo
   CALL fn_pantalla_edo_cuenta(p_nss) RETURNING v_archivo
   
   #CLOSE WINDOW srvp09
END MAIN