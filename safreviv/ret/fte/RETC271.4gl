--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC271                                                                #
#OBJETIVO     => Consulta de rendimiento                                                #
#Fecha inicio => 31 Octubre 2013                                                        #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
{
======================================================================
Clave: 
Nombre: main
Fecha creacion: 31 de octubre de 2013
Autor: Eneas Armas, EFP
Narrativa del proceso que realiza:
Abre la ventana de captura de datos para realizar la consulta de rendimiento


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
MAIN 
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
       p_s_titulo          STRING,   -- titulo de la ventana
       v_folio_restitucion LIKE ret_solicitud_generico.folio_restitucion
   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   -- se abre la ventana de consulta
   OPEN WINDOW w_parametros WITH FORM "RETC2711"
      -- se capturan los datos de la consulta
      INPUT BY NAME v_folio_restitucion-- ,

      WITHOUT DEFAULTS
      ATTRIBUTES ( UNBUFFERED )

      ON ACTION cancel
         EXIT INPUT
         
      ON ACTION ACCEPT
      --se valida que no venga el campo en blanco
         IF ( v_folio_restitucion IS NULL) THEN
            CALL fn_mensaje("Atención","No se puede dejar el parametro en blanco","stop")
            CONTINUE INPUT
         END IF
         --se envian parametros para generar la consulta
         CALL fn_consulta_acu(v_folio_restitucion,p_s_titulo,p_usuario_cod)


   END INPUT

  CLOSE WINDOW w_parametros
END MAIN

FUNCTION fn_consulta_acu(v_folio_restitucion,p_origen_datos,p_usuario_cod)
DEFINE v_folio_restitucion  LIKE ret_solicitud_generico.folio_restitucion,
       v_indice             INTEGER, -- contador       
       v_sql                STRING, -- cadena con instruccion sql
       v_arr_despliegue     DYNAMIC ARRAY OF RECORD
          folio_liquida      LIKE ret_rendimiento_restitucion.folio_liquida,
          subcuenta          LIKE ret_rendimiento_restitucion.subcuenta,
          fondo_inversion    LIKE ret_rendimiento_restitucion.fondo_inversion,
          monto_acciones     LIKE ret_rendimiento_restitucion.monto_acciones,
          pesos_liquidacion  LIKE ret_rendimiento_restitucion.pesos_liquidacion,
          pesos_restitucion  LIKE ret_rendimiento_restitucion.pesos_restitucion,
          rendimiento        LIKE ret_rendimiento_restitucion.rendimiento
       END RECORD,
       v_tot_monto_acciones     LIKE ret_rendimiento_restitucion.monto_acciones,
       v_tot_pesos_liquidacion  LIKE ret_rendimiento_restitucion.pesos_liquidacion,
       v_tot_pesos_restitucion  LIKE ret_rendimiento_restitucion.pesos_restitucion,
       v_tot_rendimiento        LIKE ret_rendimiento_restitucion.rendimiento,
       indice                   INTEGER,
       p_origen_datos           STRING,
       p_r_encabezado RECORD
          p_folio               INTEGER,
          p_usuario_cod         STRING,
          p_fecha               DATE
       END RECORD,
       p_nombre_usuario         char(40),
       p_usuario_cod            LIKE seg_usuario.usuario_cod
DEFINE reportxml om.SaxDocumentHandler

INITIALIZE reportxml TO NULL

   LET p_r_encabezado.p_folio = v_folio_restitucion
   LET p_r_encabezado.p_usuario_cod = p_usuario_cod
   LET p_r_encabezado.p_fecha = TODAY

   SELECT usuario_desc
   INTO p_nombre_usuario
   FROM seg_usuario
   WHERE usuario_cod = p_usuario_cod

   -- ===================================================================================
   -- ===================================================================================
   -- Acumulado de rendimiento
   -- ===================================================================================
   -- se construye la cadena de consulta
   LET v_sql = "\n SELECT folio_liquida,subcuenta, fondo_inversion,   ",
               "\n        sum(monto_acciones),sum(pesos_liquidacion), ",
               "\n        sum(pesos_restitucion),sum(rendimiento)     ",
               "\n FROM ret_rendimiento_restitucion                   ",
               "\n WHERE folio_restitucion = ",v_folio_restitucion,
               "\n GROUP BY folio_liquida,subcuenta, fondo_inversion  "
--select subcuenta, fondo_inversion,sum(monto_acciones),
--sum(pesos_liquidacion), sum(pesos_restitucion)
--from ret_rendimiento_restitucion
--where folio_restitucion
--GROUP BY subcuenta, fondo_inversion

   -- se prepara la consulta
   PREPARE sid_acumulado FROM v_sql
   
   DECLARE cur_acumulado CURSOR FOR sid_acumulado
   
   LET v_indice = 1
   LET v_tot_monto_acciones     = 0
   LET v_tot_pesos_liquidacion  = 0
   LET v_tot_pesos_restitucion  = 0
   LET v_tot_rendimiento        = 0
   
   FOREACH cur_acumulado INTO v_arr_despliegue[v_indice].*
      LET v_tot_monto_acciones     = v_tot_monto_acciones+v_arr_despliegue[v_indice].monto_acciones
      LET v_tot_pesos_liquidacion  = v_tot_pesos_liquidacion+v_arr_despliegue[v_indice].pesos_liquidacion
      LET v_tot_pesos_restitucion  = v_tot_pesos_restitucion+v_arr_despliegue[v_indice].pesos_restitucion
      LET v_tot_rendimiento        = v_tot_rendimiento+v_arr_despliegue[v_indice].rendimiento
      LET v_indice = v_indice + 1
   END FOREACH

   -- si la longitud del arreglo es 1, entonces se selecciona por omision
   --IF ( v_arr_despliegue.getLength() = 1 ) THEN
      --LET v_arr_despliegue[1].elegir = 1
   --END IF

   -- se abre la ventana de consulta detallada
   OPEN WINDOW w_consulta WITH FORM "RETC2712"

   DIALOG ATTRIBUTE (UNBUFFERED)
      DISPLAY ARRAY v_arr_despliegue TO tbl_despliegue.*
      END DISPLAY
      BEFORE DIALOG
         DISPLAY v_folio_restitucion TO v_d_folio_restitucion
         DISPLAY BY NAME v_tot_monto_acciones,v_tot_pesos_liquidacion,
                         v_tot_pesos_restitucion,v_tot_rendimiento
      ON ACTION cancelar
         EXIT DIALOG
      ON ACTION reporte
         IF fgl_report_loadCurrentSettings("RETC271.4rp") THEN
            CALL fgl_report_selectDevice("SVG")
            CALL fgl_report_selectPreview(TRUE)
            LET reportxml = fgl_report_commitCurrentSettings()
         ELSE
            EXIT PROGRAM
         END IF
    
         START REPORT reporte_con_grw TO XML HANDLER reportxml
         FOR indice = 1 TO v_arr_despliegue.getLength()
            OUTPUT TO REPORT reporte_con_grw(p_origen_datos,p_r_encabezado.*,p_nombre_usuario
            ,v_arr_despliegue[indice].*,v_tot_monto_acciones,v_tot_pesos_liquidacion
            ,v_tot_pesos_restitucion,v_tot_rendimiento)
         END FOR
         FINISH REPORT reporte_con_grw

   END DIALOG
   
   CLOSE WINDOW w_consulta


END FUNCTION

REPORT reporte_con_grw(p_origen_datos,p_r_encabezado,p_nombre_usuario
,r_datos,v_tot_monto_acciones,v_tot_pesos_liquidacion
,v_tot_pesos_restitucion,v_tot_rendimiento)
DEFINE  p_origen_datos       STRING,
      p_r_encabezado RECORD
          p_folio            INTEGER,
          p_usuario_cod      STRING,
          p_fecha            DATE
      END RECORD,
      p_nombre_usuario       char(40),
      r_datos      RECORD
          folio_liquida      LIKE ret_rendimiento_restitucion.folio_liquida,
          subcuenta          LIKE ret_rendimiento_restitucion.subcuenta,
          fondo_inversion    LIKE ret_rendimiento_restitucion.fondo_inversion,
          monto_acciones     LIKE ret_rendimiento_restitucion.monto_acciones,
          pesos_liquidacion  LIKE ret_rendimiento_restitucion.pesos_liquidacion,
          pesos_restitucion  LIKE ret_rendimiento_restitucion.pesos_restitucion,
          rendimiento        LIKE ret_rendimiento_restitucion.rendimiento
       END RECORD,
       v_tot_monto_acciones     LIKE ret_rendimiento_restitucion.monto_acciones,
       v_tot_pesos_liquidacion  LIKE ret_rendimiento_restitucion.pesos_liquidacion,
       v_tot_pesos_restitucion  LIKE ret_rendimiento_restitucion.pesos_restitucion,
       v_tot_rendimiento        LIKE ret_rendimiento_restitucion.rendimiento

FORMAT
   FIRST PAGE HEADER
      PRINTX p_origen_datos,p_r_encabezado.*,p_nombre_usuario
   ON EVERY ROW
      PRINTX r_datos.*
   ON LAST ROW
      PRINTX v_tot_monto_acciones,v_tot_pesos_liquidacion
            ,v_tot_pesos_restitucion,v_tot_rendimiento

END REPORT



