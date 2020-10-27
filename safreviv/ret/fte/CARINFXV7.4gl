#################################################################################
#Nombre del Programa => CARINFXV7                                               #
#Descripción         => Cambia el estado de la solicitud del trabajador a 100,  #
#                       desmarca la cuenta y restituir                          #
#Fecha creacion      => 17 de Noviembre del 2015                                #
#Por                 => Luis Felipe Prieto Cano                                 #
#Requerimiento       => CARINFXV-7                                              #
#################################################################################
DATABASE safre_viv 
GLOBALS
   DEFINE
      HOY                  DATE
   DEFINE
      enter                CHAR(1)
   DEFINE
      v_precio_fondo       LIKE glo_valor_fondo.precio_fondo
END GLOBALS

MAIN
    CALL init()         --Se inicializan variables
    CALL primer_paso()  --Restituye la cuenta
    CALL segundo_paso() --Cambia el estado de la solicitud de retiro a rechazada  Desmarca las cuentas
    CALL tercer_paso()  --Desmarca las cuentas
    CALL cuarto_paso()  --Cambio estado en dae_det_solicitud
    DISPLAY "Se ejecuto el proceso correctamente."
END MAIN

FUNCTION init()
--------------
    LET HOY = TODAY
    
    SELECT precio_fondo
    INTO   v_precio_fondo
    FROM   glo_valor_fondo
    WHERE  fondo       = 11
    AND    f_valuacion = HOY
END FUNCTION

FUNCTION primer_paso()
----------------------
    --Restitución de solicitudes
    --En este caso no existen solicitudes a restituir
END FUNCTION

FUNCTION segundo_paso()
---------------------
    --Cambio de estado y codigo de rechazo para las solicitudes
      UPDATE ret_solicitud_generico
      SET    estado_solicitud = 100 ,
             cod_rechazo      = 54   
      WHERE  id_solicitud IN (7287682,7437195,7662491,7688775,7690267,7690541,7710862,7712911,7713383,
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
      
      UPDATE ret_amort_excedente
      SET    estado_solicitud = 100 ,
             cod_rechazo      = 54   
      WHERE  id_solicitud IN (7287682,7437195,7662491,7688775,7690267,7690541,7710862,7712911,7713383,
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
    --Desmarcar las cuentas
    DEFINE
        v_sql                CHAR(200)
    
    DEFINE
        v_resultado          SMALLINT 
        
    DEFINE
        reg_2                RECORD LIKE sfr_marca_activa.*
        
    DECLARE cur_2 CURSOR FOR
    SELECT *
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

        
    FOREACH cur_2 INTO reg_2.*
        LET v_sql = "\nEXECUTE FUNCTION fn_desmarca_cuenta(","\n",reg_2.id_derechohabiente, ",",
                                                             "\n",reg_2.marca, ",",
                                                             "\n",reg_2.n_referencia,",",
                                                             "\n 0,",
                                                             "\n",reg_2.marca_causa,",",
                                                             "\n",'"SAFREVIV"',",",
                                                             "\n",reg_2.proceso_marca,")"
        PREPARE v_desmarca FROM v_sql
        EXECUTE v_desmarca INTO v_resultado                
    END FOREACH
END FUNCTION

FUNCTION cuarto_paso()
   UPDATE dae_det_solicitud
   SET folio_retiro=43085,id_ret_solicitud=7109769,
   ctr_folios_ret_rest="43085-1540|",status_retiro = 3
   WHERE id_derechohabiente = 13770452
   AND status_retiro = 4
   AND id_dae_referencia IN (1582888,6822,7564,216391,425016,425218,633626,633719,331136)
   AND nss IN (11905500069,43856526694,53725003569)

   UPDATE dae_det_solicitud
   SET folio_retiro=43085,id_ret_solicitud=5349574,
   ctr_folios_ret_rest="43085-1540|",status_retiro = 3
   WHERE id_derechohabiente = 30930406
   AND status_retiro = 4
   AND id_dae_referencia IN (1582888,6822,7564,216391,425016,425218,633626,633719,331136)
   AND nss IN (11905500069,43856526694,53725003569)

   UPDATE dae_det_solicitud
   SET folio_retiro=43085,id_ret_solicitud=7268731,
   ctr_folios_ret_rest="43085-1540|",status_retiro = 3
   WHERE id_derechohabiente = 32192932
   AND status_retiro = 4
   AND id_dae_referencia IN (1582888,6822,7564,216391,425016,425218,633626,633719,331136)
   AND nss IN (11905500069,43856526694,53725003569)
END FUNCTION
