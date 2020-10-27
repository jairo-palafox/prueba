#################################################################################
#Nombre del Programa => RETXV49R                                                #
#Descripción         => REVERSO RETXV49.4gl                                     #
#Fecha creacion      => 24 de Septiembre del 2015                               #
#Por                 => Luis Felipe Prieto Cano                                 #
#Requerimiento       => PRODINFXV-49                                            #
#################################################################################
DATABASE safre_viv
GLOBALS
    DEFINE #DATE
        HOY                  DATE
               
    DEFINE #CHAR
        enter                CHAR(1)
END GLOBALS

MAIN
    CALL init()         --Se inicializan variables
    CALL primer_paso()  --Elimina el movimiento de restitución de la cuenta 
    CALL segundo_paso() --Deja el estado solicitud original
    CALL tercer_paso()  --Marca nuevamente las cuentas
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
		WHERE id_derechohabiente IN(13770452,30930406,32192932)
		AND   movimiento         = 251
		AND   subcuenta          = 46
      AND   origen             = "RESTITUCION"

END FUNCTION

FUNCTION segundo_paso()
-----------------------
   
   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 100 ,
          cod_rechazo      = 800   
   WHERE  id_solicitud in(5349574)

   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 100 ,
          cod_rechazo      = 54   
   WHERE  id_solicitud in(7109769)

   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 211 ,
          cod_rechazo      = 65   
   WHERE  id_solicitud in(7546293,7351434,7353992,7400247,7402893,7545030,
                          7579838,7656407,7657089,7662304,7686516,7566627)

   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 214 ,
          cod_rechazo      = 65   
   WHERE  id_solicitud in(5085937)

   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 214 ,
          cod_rechazo      = 64   
   WHERE  id_solicitud in(5088679,7155348,7279446)

   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 700 ,
          cod_rechazo      = 0   
   WHERE  id_solicitud in(7268731)
-----------------------------------------

   UPDATE ret_amort_excedente
   SET    estado_solicitud = 100 ,
          cod_rechazo      = 54   
   WHERE  id_solicitud in(7109769)

   UPDATE ret_amort_excedente
   SET    estado_solicitud = 210 ,
          cod_rechazo      = 65   
   WHERE  id_solicitud in(7351434,7353992,7400247,7402893,7545030,7546293,
                          7579838,7656407,7657089,7662304,7686516,7566627)

   UPDATE ret_amort_excedente
   SET    estado_solicitud = 214 ,
          cod_rechazo      = 65   
   WHERE  id_solicitud in(5085937)

   UPDATE ret_amort_excedente
   SET    estado_solicitud = 214 ,
          cod_rechazo      = 64   
   WHERE  id_solicitud in(5088679,7155348,7279446)

   UPDATE ret_amort_excedente
   SET    estado_solicitud = 700 ,
          cod_rechazo      = 0   
   WHERE  id_solicitud in(7268731,5349574)

END FUNCTION

FUNCTION tercer_paso()
#---------------------
    DELETE
    FROM   sfr_marca_activa
    WHERE  n_referencia IN(7155348,7353992,7400247,7268731,7351434,
                           7279446,7402893,7353992,7109769,5349574,5085937,7656407,5088679,
                           7579838,7546293,7662304,7545030,7657089,7686516,7566627)
    AND    marca         = 810

    
    DELETE
    FROM   sfr_marca_historica
    WHERE  n_referencia IN(7155348,7353992,7400247,7268731,7351434,
                           7279446,7402893,7353992,7109769,5349574,5085937,7656407,5088679,
                           7579838,7546293,7662304,7545030,7657089,7686516,7566627)
    AND    marca         = 810
    
    LOAD FROM "sfr_marca_activa.XV49.unl" INSERT INTO sfr_marca_activa
    LOAD FROM "sfr_marca_historica.XV49.unl" INSERT INTO sfr_marca_historica  
END FUNCTION
