#################################################################################
#Nombre del Programa => CARINFXV7R                                              #
#Descripción         => REVERSO CARINFXV7.4gl                                   #
#Fecha creacion      => 17 de Noviembre del 2015                                #
#Por                 => Luis Felipe Prieto Cano                                 #
#Requerimiento       => CARINFXV-7                                              #
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
    CALL cuarto_paso()  --Cambio estado en dae_det_solicitud a como estaba antes
    DISPLAY "Se ejecuto el proceso correctamente."
END MAIN

FUNCTION init()
--------------
    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso()
----------------------   
    --Se eliminan movimientos de restitución
    --No existen movimientos para esta solicitud
END FUNCTION

FUNCTION segundo_paso()
-----------------------
   --Se cambian las solicitudes a los estados/codigos de rechazo originales

   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 211 ,
          cod_rechazo      = 65   
   WHERE  id_solicitud IN    (7287682,7437195,7662491,7688775,7690267,7690541,7710862,7712911,7713383,
                              7713537,7713817,7715047,7716092,7716330,7719896,7720786,7721062,7721234,
                              7721348,7721444,7722178,7722197,7722785,7722945,7722965,7723041,7723722,
                              7723816,7723830,7723847,7723852,7723882,7723922,7724084,7724654,7724660,
                              7724993,7725027,7725173,7725481,7726070,7735011,7735089,7739117,7739130,
                              7746060,7748159,7749583,7749699,7752005,7752327,7752418,7752942,7752946,
                              7752999,7753035,7753277,7753542,7754159,7754462,7754983,7756096,7756262,
                              7756440,7756489,7756563,7756610,7757157,7762686,7763111,7763132,7763162,
                              7763178,7763182,7764089,7764102,7764200,7764431,7764527,7764579,7764590,
                              7765119,7765211,7765314,7765460,7765469,7765488,7766349,7766358,7766367,
                              7766435,7766439,7766487,7766545,7766593,7766652,7766666,7766681,7766685,
                              7766695,7766708,7766836,7766863,7766880,7766934,7767347,7767432,7767506,
                              7767522,7767578,7767645,7767691,7767697,7767803,7767881,7767941,7767961,
                              7768212,7768224,7768243,7768247,7768333,7768368,7768387,7768527,7768556,
                              7768686,7768698,7768741,7768815,7769600,7769685)
-----------------------------------------

   UPDATE ret_amort_excedente
   SET    estado_solicitud = 210 ,
          cod_rechazo      = 65   
   WHERE  id_solicitud IN    (7287682,7437195,7662491,7688775,7690267,7690541,7710862,7712911,7713383,
                              7713537,7713817,7715047,7716092,7716330,7719896,7720786,7721062,7721234,
                              7721348,7721444,7722178,7722197,7722785,7722945,7722965,7723041,7723722,
                              7723816,7723830,7723847,7723852,7723882,7723922,7724084,7724654,7724660,
                              7724993,7725027,7725173,7725481,7726070,7735011,7735089,7739117,7739130,
                              7746060,7748159,7749583,7749699,7752005,7752327,7752418,7752942,7752946,
                              7752999,7753035,7753277,7753542,7754159,7754462,7754983,7756096,7756262,
                              7756440,7756489,7756563,7756610,7757157,7762686,7763111,7763132,7763162,
                              7763178,7763182,7764089,7764102,7764200,7764431,7764527,7764579,7764590,
                              7765119,7765211,7765314,7765460,7765469,7765488,7766349,7766358,7766367,
                              7766435,7766439,7766487,7766545,7766593,7766652,7766666,7766681,7766685,
                              7766695,7766708,7766836,7766863,7766880,7766934,7767347,7767432,7767506,
                              7767522,7767578,7767645,7767691,7767697,7767803,7767881,7767941,7767961,
                              7768212,7768224,7768243,7768247,7768333,7768368,7768387,7768527,7768556,
                              7768686,7768698,7768741,7768815,7769600,7769685)

END FUNCTION

FUNCTION tercer_paso()
#---------------------
    DELETE
    FROM   sfr_marca_activa
    WHERE  n_referencia IN   (7287682,7437195,7662491,7688775,7690267,7690541,7710862,7712911,7713383,
                              7713537,7713817,7715047,7716092,7716330,7719896,7720786,7721062,7721234,
                              7721348,7721444,7722178,7722197,7722785,7722945,7722965,7723041,7723722,
                              7723816,7723830,7723847,7723852,7723882,7723922,7724084,7724654,7724660,
                              7724993,7725027,7725173,7725481,7726070,7735011,7735089,7739117,7739130,
                              7746060,7748159,7749583,7749699,7752005,7752327,7752418,7752942,7752946,
                              7752999,7753035,7753277,7753542,7754159,7754462,7754983,7756096,7756262,
                              7756440,7756489,7756563,7756610,7757157,7762686,7763111,7763132,7763162,
                              7763178,7763182,7764089,7764102,7764200,7764431,7764527,7764579,7764590,
                              7765119,7765211,7765314,7765460,7765469,7765488,7766349,7766358,7766367,
                              7766435,7766439,7766487,7766545,7766593,7766652,7766666,7766681,7766685,
                              7766695,7766708,7766836,7766863,7766880,7766934,7767347,7767432,7767506,
                              7767522,7767578,7767645,7767691,7767697,7767803,7767881,7767941,7767961,
                              7768212,7768224,7768243,7768247,7768333,7768368,7768387,7768527,7768556,
                              7768686,7768698,7768741,7768815,7769600,7769685)
    AND    marca         = 810

    
    DELETE
    FROM   sfr_marca_historica
    WHERE  n_referencia IN   (7287682,7437195,7662491,7688775,7690267,7690541,7710862,7712911,7713383,
                              7713537,7713817,7715047,7716092,7716330,7719896,7720786,7721062,7721234,
                              7721348,7721444,7722178,7722197,7722785,7722945,7722965,7723041,7723722,
                              7723816,7723830,7723847,7723852,7723882,7723922,7724084,7724654,7724660,
                              7724993,7725027,7725173,7725481,7726070,7735011,7735089,7739117,7739130,
                              7746060,7748159,7749583,7749699,7752005,7752327,7752418,7752942,7752946,
                              7752999,7753035,7753277,7753542,7754159,7754462,7754983,7756096,7756262,
                              7756440,7756489,7756563,7756610,7757157,7762686,7763111,7763132,7763162,
                              7763178,7763182,7764089,7764102,7764200,7764431,7764527,7764579,7764590,
                              7765119,7765211,7765314,7765460,7765469,7765488,7766349,7766358,7766367,
                              7766435,7766439,7766487,7766545,7766593,7766652,7766666,7766681,7766685,
                              7766695,7766708,7766836,7766863,7766880,7766934,7767347,7767432,7767506,
                              7767522,7767578,7767645,7767691,7767697,7767803,7767881,7767941,7767961,
                              7768212,7768224,7768243,7768247,7768333,7768368,7768387,7768527,7768556,
                              7768686,7768698,7768741,7768815,7769600,7769685)
    AND    marca         = 810
    
    LOAD FROM "sfr_marca_activa.CARINFXV7.unl" INSERT INTO sfr_marca_activa
    LOAD FROM "sfr_marca_historica.CARINFXV7.unl" INSERT INTO sfr_marca_historica  
END FUNCTION

FUNCTION cuarto_paso()
   UPDATE dae_det_solicitud
   SET folio_retiro=NULL,id_ret_solicitud=NULL,
   ctr_folios_ret_rest=NULL,status_retiro = 3
   WHERE id_derechohabiente IN (13770452,30930406,32192932)
   AND status_retiro = 4
   AND id_dae_referencia IN (1582888,6822,7564,216391,425016,425218,633626,633719,331136)
   AND nss IN (11905500069,43856526694,53725003569)
END FUNCTION
 