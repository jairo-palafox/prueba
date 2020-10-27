####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS03.4gl                                   #
#Objetivo          =>Publicación del Servicio Web para recibir los #
#                    indicadores de notificaciones                 #
#Fecha inicio      =>ENERO 2015                                    #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "AFIWS03.inc"

MAIN
   DEFINE servicio     INTEGER
   DEFINE respuesta    INTEGER

   CALL CreateMarcaNotificacionesServices() RETURNING servicio

   CALL com.WebServiceEngine.Start()

   DISPLAY("The server is listening.")

   WHILE TRUE
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      CASE respuesta
         WHEN 0
            --DISPLAY "Request processed."
         WHEN -1
            DISPLAY "Timeout reached."
         WHEN -2
            DISPLAY "Disconnected from application server."
            EXIT PROGRAM
         WHEN -3
            DISPLAY "Client Connection lost."
         WHEN -4
            DISPLAY "Server interrupted with Ctrl-C."
         WHEN -10
            DISPLAY "Internal server error."
     END CASE

     IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
     END IF
   END WHILE
END MAIN

#-------------------------------------------------------------------------------
# Service: CreateMarcaNotificacionesServices
# Port:    MarcaNotificacionesServices
#-------------------------------------------------------------------------------
#
# FUNCTION CreateMarcaNotificacionesServices
#   RETURNING soapstatus
#
FUNCTION CreateMarcaNotificacionesServices()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("marcaNotificacionesServices","http://services.safre.efp.com")


    #
    # Operation: consultarHomonimia
    #

    # Publish Operation : consultarHomonimia
    LET operation = com.WebOperation.CreateDOCStyle("marcaNotificaciones","marcaNotificaciones",marcaNotificacion,marcaNotificacionRespuesta)
    CALL service.publishOperation(operation,"")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION marcaNotificaciones()
   DEFINE v_query          STRING
   DEFINE v_resultado      SMALLINT
   DEFINE v_tpo_notifica   SMALLINT
   DEFINE v_folio          SMALLINT
   DEFINE v_cod_fuente     SMALLINT


   LET v_folio = 0

   LET marcaNotificacionRespuesta.result_opera = "00"

   LET v_query = "EXECUTE PROCEDURE fn_afi_activa_notifica (?,?,?,?,?) "
   PREPARE prp_indicador FROM v_query

   LET v_query = "EXECUTE PROCEDURE fn_afi_bloquea_notifica (?,?,?,?,?) "
   PREPARE prp_bloq_ind FROM v_query


   ---BLOQUE INTERNO CORREO ELECTRONICO----- 
   IF marcaNotificacion.indicador_correo IS NOT NULL AND
      (marcaNotificacion.indicador_correo = 0 OR
       marcaNotificacion.indicador_correo = 1) THEN

      LET v_tpo_notifica = 2

      SELECT fuente
        INTO v_cod_fuente
        FROM cat_afi_notif_fuente
       WHERE desc_fuente = marcaNotificacion.fuente_correo
       
      IF v_cod_fuente IS NULL OR
         v_cod_fuente = 0 THEN
         SELECT MAX(fuente) + 1
           INTO v_cod_fuente
           FROM cat_afi_notif_fuente

         IF v_cod_fuente IS NULL THEN
            LET v_cod_fuente = 1100
         END IF

         INSERT INTO cat_afi_notif_fuente VALUES (v_cod_fuente, marcaNotificacion.fuente_correo, TODAY, "OPSISSACI")
      END IF
      
      EXECUTE prp_indicador USING marcaNotificacion.nss,
                                  v_tpo_notifica,
                                  marcaNotificacion.indicador_correo,
                                  v_folio,
                                  v_cod_fuente
                             INTO v_resultado
      IF v_resultado <> 0 THEN
         LET marcaNotificacionRespuesta.result_opera = "01"
      END IF
   END IF

   -----NOTIFICACION SMS-------
   IF marcaNotificacion.indicador_sms IS NOT NULL AND
      (marcaNotificacion.indicador_sms  = 0 OR
       marcaNotificacion.indicador_sms  = 1) THEN

      LET v_tpo_notifica = 1
      SELECT fuente
        INTO v_cod_fuente
        FROM cat_afi_notif_fuente
       WHERE desc_fuente = marcaNotificacion.fuente_sms
       
      IF v_cod_fuente IS NULL OR
         v_cod_fuente = 0 THEN
         SELECT MAX(fuente) + 1
           INTO v_cod_fuente
           FROM cat_afi_notif_fuente

         IF v_cod_fuente IS NULL THEN
            LET v_cod_fuente = 1100
         END IF
         INSERT INTO cat_afi_notif_fuente VALUES (v_cod_fuente, marcaNotificacion.fuente_sms, TODAY, "OPSISSACI")
      END IF

      EXECUTE prp_indicador USING marcaNotificacion.nss,
                                  v_tpo_notifica,
                                  marcaNotificacion.indicador_sms ,
                                  v_folio,
                                  v_cod_fuente
                             INTO v_resultado
      IF v_resultado <> 0 THEN
         LET marcaNotificacionRespuesta.result_opera = "01"
      END IF
   END IF

   ---BLOQUEO INTERNO CORREO ELECTRONICO----- 
   IF marcaNotificacion.ind_bloqueo_correo IS NOT NULL AND
       marcaNotificacion.ind_bloqueo_correo = 1 THEN

      LET v_tpo_notifica = 3
      
      SELECT fuente
        INTO v_cod_fuente
        FROM cat_afi_notif_fuente
       WHERE desc_fuente = marcaNotificacion.fuente_correo
       
      IF v_cod_fuente IS NULL OR
         v_cod_fuente = 0 THEN
         SELECT MAX(fuente) + 1
           INTO v_cod_fuente
           FROM cat_afi_notif_fuente

         IF v_cod_fuente IS NULL THEN
            LET v_cod_fuente = 1100
         END IF

         INSERT INTO cat_afi_notif_fuente VALUES (v_cod_fuente, marcaNotificacion.fuente_correo, TODAY, "OPSISSACI")
      END IF

      EXECUTE prp_bloq_ind USING marcaNotificacion.nss,
                                 v_tpo_notifica,
                                 marcaNotificacion.ind_bloqueo_correo,
                                 v_folio,
                                 v_cod_fuente
                            INTO v_resultado
      IF v_resultado <> 0 THEN
         LET marcaNotificacionRespuesta.result_opera = "01"
      END IF
   END IF

   -----BLOQUEO INTERNO SMS-------
   IF marcaNotificacion.ind_bloqueo_sms IS NOT NULL AND
       marcaNotificacion.ind_bloqueo_sms  = 1 THEN

      LET v_tpo_notifica = 4

      SELECT fuente
        INTO v_cod_fuente
        FROM cat_afi_notif_fuente
       WHERE desc_fuente = marcaNotificacion.fuente_sms
       
      IF v_cod_fuente IS NULL OR
         v_cod_fuente = 0 THEN
         SELECT MAX(fuente) + 1
           INTO v_cod_fuente
           FROM cat_afi_notif_fuente

         IF v_cod_fuente IS NULL THEN
            LET v_cod_fuente = 1100
         END IF

         INSERT INTO cat_afi_notif_fuente VALUES (v_cod_fuente, marcaNotificacion.fuente_sms, TODAY, "OPSISSACI")
      END IF

      EXECUTE prp_bloq_ind USING marcaNotificacion.nss,
                                 v_tpo_notifica,
                                 marcaNotificacion.ind_bloqueo_sms ,
                                 v_folio,
                                 v_cod_fuente
                            INTO v_resultado
      IF v_resultado <> 0 THEN
         LET marcaNotificacionRespuesta.result_opera = "01"
      END IF
   END IF
END FUNCTION