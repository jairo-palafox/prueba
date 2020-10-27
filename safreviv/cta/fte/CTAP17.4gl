#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#Modulo            => CTA                                                     #
#Programa          => CTAP17                                                  #
#Objetivo          => PROGRAMA PARA GENERAR INFORMACION  PARA DWH FASE2       #
#Fecha Inicio      => DICIEMBRE-2018                                          #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
DEFINE p_pid                      DECIMAL(9,0)
DEFINE p_proceso_cod              SMALLINT    
DEFINE p_opera_cod                SMALLINT    
DEFINE p_usuario_cod              CHAR(20)    
DEFINE p_nombre_archivo           CHAR(40)    
DEFINE p_folio                    DECIMAL(9,0)
DEFINE v_f_corte                   DATE

DEFINE v_proceso_desc             CHAR(40)
DEFINE v_opera_desc               CHAR(40)

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_fecha_actual                  DATE
   DEFINE v_query                         STRING

   DEFINE v_error                         INTEGER
   DEFINE v_isam                          INTEGER
   DEFINE v_descripcion                   CHAR(200)

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   #Encabezado para el archivo de monitoreo
   LET v_fecha_actual = TODAY
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   WHENEVER ERROR CONTINUE
   DISPLAY "CREANDO LAS TABLAS DE EXTRACCIÓN "
   DATABASE safre_sdo@vivws_tcp
   LET v_query = "EXECUTE PROCEDURE sp_dwh_crea_tablas_02()"
   PREPARE prp_crea_tablas FROM v_query
   EXECUTE prp_crea_tablas INTO v_error, v_isam, v_descripcion

   IF SQLCA.SQLCODE <> 0 THEN
      DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_crea_tablas_02"
      DISPLAY "ERROR       ",v_error
      DISPLAY "ISAM ERROR  ",v_isam
      DISPLAY "DESCRIPCION ",v_descripcion
      DATABASE safre_viv
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
   ELSE
      DATABASE safre_viv
      DISPLAY ""
      DISPLAY "SACI-DWH SE CREARON LAS TABLAS DEL PROCESO CORRECTAMENTE"

      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN CONCILACIÓN "
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_extractor_conciliacion() "
      PREPARE prp_ext_concilia FROM v_query
      EXECUTE prp_ext_concilia INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_extractor_conciliacion"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF


      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE REMANENTES "
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_extractor_remanente() "
      PREPARE prp_ext_remanente FROM v_query
      EXECUTE prp_ext_remanente INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_extractor_remanente"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF

      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE RESTITUCIÓN SSV "
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_extractor_restitucion_ssv() "
      PREPARE prp_ext_ssv FROM v_query
      EXECUTE prp_ext_ssv INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_extractor_restitucion_ssv"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF

      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE REVERSOS OPERATIVOS "
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_extractor_reverso_opera() "
      PREPARE prp_ext_reverso FROM v_query
      EXECUTE prp_ext_reverso INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_extractor_reverso_opera"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF

      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE SEPARACIÓN"
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_extractor_separacion() "
      PREPARE prp_ext_separa FROM v_query
      EXECUTE prp_ext_separa INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_extractor_separacion"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF

      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE TRASPASO INFONAVIT-AFORE"
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_extractor_traspaso_ifv_afore() "
      PREPARE prp_ext_tia FROM v_query
      EXECUTE prp_ext_tia INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_extractor_traspaso_ifv_afore"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF

      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE UNIFICACIÓN"
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_extractor_unificacion() "
      PREPARE prp_ext_unif FROM v_query
      EXECUTE prp_ext_unif INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_extractor_unificacion"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF

      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE MARCAS OPERATIVAS"
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_marca() "
      PREPARE prp_ext_marca FROM v_query
      EXECUTE prp_ext_marca INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_marca"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF

      DISPLAY ""
      DISPLAY ""
      DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE CONSULTAS ROJAS"
      DISPLAY "FECHA    : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY "HORA     : ",TIME(CURRENT)
      LET v_query = "EXECUTE PROCEDURE sp_dwh_consulta_roja() "
      PREPARE prp_ext_roja FROM v_query
      EXECUTE prp_ext_roja INTO v_error, v_isam, v_descripcion
      IF v_error <> 0 THEN
         DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_consulta_roja"
         DISPLAY "ERROR       ",v_error
         DISPLAY "ISAM ERROR  ",v_isam
         DISPLAY "DESCRIPCION ",v_descripcion
      END IF

      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN         
         DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF
      DISPLAY ""
      DISPLAY "*******************************************************************"
      DISPLAY "SACI-DWH TERMINO CORRECTAMENTE LA EXTRACCIÓN DE INDICADORES"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY "*******************************************************************"
   END IF
END MAIN
