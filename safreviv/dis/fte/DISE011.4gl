################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 24/05/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE011                                                  #
#Objetivo          => Registro de avance de pagos en batch                     #
#Fecha inicio      => 24/09/2012                                               #
################################################################################

DATABASE safre_viv

GLOBALS 

   
      p_folio              DECIMAL (9,0),
      p_nombre_archivo     VARCHAR(40)
      
      

END GLOBALS 

MAIN
   CALL fn_registro_avances_pago()
END MAIN


--Función que ejecuta el registro de avances pagos y genera la interfaz de pago real
FUNCTION fn_registro_avances_pago()

   --Asignación de parametros generales 
   LET p_usuario = ARG_VAL(1)
   LET p_pid = ARG_VAL(2) 
   LET p_proceso_cod = ARG_VAL(3) 
   LET p_opera_cod = ARG_VAL(4)
   LET p_folio = ARG_VAL(5) 
   LET p_nombre_archivo = ARG_VAL(6)

   -- Se ejecuta el sp_dis_avance_pago3, que actualiza el estado a 30
   WHENEVER ERROR CONTINUE

   PREPARE prp_registro FROM "EXECUTE PROCEDURE safre_viv:sp_dis_avances_pago3(?,?)"
   EXECUTE prp_registro USING v_edit_Folio, p_usuario
   DISPLAY "FOLIO PARA REGISTRO ", v_edit_Folio, " -USUARIO ",p_usuario                  
   WHENEVER ERROR STOP
   
   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "Codigo de error: ",SQLCA.sqlcode
   END IF 

   --Ejecuta la interfaz a Hipotecaria Social
   {LET l_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/DISS02.42r ",
                    v_edit_folio}

   LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                "/DISS02.42r ",p_usuario," ",p_pid," ",
                p_proceso_cod," ",p_opera_cod," ",v_edit_Folio," ",
                " '",r_nom_archivo,"' 1>", v_ruta_listados CLIPPED ,
                "/nohup:",p_pid USING "&&&&&",":",
                          p_proceso_cod USING "&&&&&",":",
                          p_opera_cod USING "&&&&&" ," 2>&1 &"


                    
   DISPLAY "v_comando = \n",l_comando
   
   RUN l_comando
   
   --Se marca la operación como FINALIZADO
   CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
   RETURNING r_bandera
  
   IF r_bandera = 0 THEN 
         CLEAR FORM 
         CALL fn_mensaje("INFO", "Se ha realizado el registro", "information")
         EXIT PROGRAM 
   END IF

   

END FUNCTION 