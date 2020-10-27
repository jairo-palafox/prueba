#################################################################################
#Nombre del Programa => CARINFXVI35R                                            #
#Descripción         => REVERSO CARINFXVI35                                     #
#Fecha creacion      => 28 de diciembre de 2016                                 #
#Por                 => RICARDO PEREZ RAMIREZ                                   #
#Requerimiento       => CARINFXVI-35                                            #
#################################################################################
DATABASE safre_viv
GLOBALS
    DEFINE
        HOY                  DATE,
        enter                CHAR(1)
END GLOBALS

MAIN
    CALL init()         --Se inicializan variables
    CALL primer_paso()  --Elimina el movimiento de restitución de la cuenta 
--    CALL segundo_paso() --Deja el estado solicitud original
--    CALL tercer_paso()  --Marca nuevamente las cuentas
    DISPLAY "Se ejecuto el proceso correctamente."
END MAIN

FUNCTION init()
--------------
    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso()
----------------------   
	   DELETE
		FROM  cta_movimiento
		WHERE id_derechohabiente IN(28797016,43793629,4381129,15628640,21969696)
		AND   movimiento         = 251
		AND   subcuenta          = 46
      AND   origen             = "RESTITUCION"
END FUNCTION

FUNCTION segundo_paso()
DEFINE v_solicitud DECIMAL(9,0)
---------------------
   LET v_solicitud = 0

      UPDATE ret_solicitud_generico
      SET    estado_solicitud = 100 ,
             cod_rechazo      = 54   
      WHERE  id_solicitud IN (v_solicitud)
      
      UPDATE ret_amort_excedente
      SET    estado_solicitud = 100 ,
             cod_rechazo      = 54   
      WHERE  id_solicitud IN (v_solicitud)

END FUNCTION

FUNCTION tercer_paso()
#---------------------
DEFINE v_solicitud DECIMAL(9,0)

    LET v_solicitud = 0    
    DELETE
    FROM   sfr_marca_activa
    WHERE  n_referencia IN(v_solicitud)
    AND    marca         = 810

    
    DELETE
    FROM   sfr_marca_historica
    WHERE  n_referencia IN(v_solicitud)
    AND    marca         = 810
    
--    LOAD FROM "sfr_marca_activa.CARINFXVI26.unl" INSERT INTO sfr_marca_activa
--    LOAD FROM "sfr_marca_historica.CARINFXVI26.unl" INSERT INTO sfr_marca_historica  
END FUNCTION
