--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => RET                                                     #
#Programa          => RETM471                                                 #
#Objetivo          => Mantenimiento a los Avisos de la Devolición Automática  #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv

GLOBALS
----DEFINICION DE VARIABLES GLOBALES, PARAMETROS ENVIADOS DESDE EL MENÚ

DEFINE g_usuario      CHAR(20)
DEFINE g_tipo_proceso SMALLINT
DEFINE g_nom_ventana  STRING
DEFINE g_pid          LIKE bat_ctr_proceso.pid --  ID del proceso
DEFINE g_proceso_cod  LIKE cat_proceso.proceso_cod -- codigo del proceso
DEFINE g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion


DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod -- clave del usuario firmado
DEFINE v_ventana       ui.WINDOW
DEFINE v_folio         DECIMAL(11,0)


END GLOBALS

PRIVATE DEFINE ventana     ui.Window
PRIVATE DEFINE forma       ui.Form

MAIN
    DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
           p_s_titulo       STRING, -- titulo de la ventana
           lc_where         CHAR(200)

    DEFINE v_ruta_bitacora        CHAR(40)
    DEFINE v_archivo_log          STRING
    DEFINE v_programa             STRING
    DEFINE v_front                STRING
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1me refiero 
    ---SE INCORPORA COMO PARAMETROS ENVIADOS DESDE EL MENU EL PROCESO Y CODIGO DE OPERACION
    LET g_usuario      = ARG_VAL(1)
    LET g_tipo_proceso = ARG_VAL(2)
    LET g_nom_ventana  = ARG_VAL(3)
    LET g_proceso_cod  = ARG_VAL(4)
    LET g_opera_cod    = ARG_VAL(5)

    LET v_programa     = "RETM471" 
    
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF
   

    SELECT ruta_bitacora
      INTO v_ruta_bitacora
      FROM seg_modulo
     WHERE modulo_cod = "ret"

    LET v_archivo_log = v_ruta_bitacora CLIPPED ,"/",g_usuario CLIPPED, ".",v_programa,".log"
    CALL STARTLOG(v_archivo_log)

            -- Arma la consulta de los registros a verificar
    CALL fn_consulta_datos()

END MAIN

{ ============================================================================
Clave: XXXXXXX
Nombre: fn_consulta_datos
Fecha creacion: 22 junio, 2018
Registro de modificaciones:
Descrip: CONSULTA AVISOS PARA LA DEVOLUCIÓN AUTOMÁTICA
==============================================================================
}
PRIVATE FUNCTION fn_consulta_datos()
       
   DEFINE arr_reg_avisos       DYNAMIC ARRAY OF RECORD
         id_aviso         DECIMAL(10,0) ,
         f_vig_inicio     DATE          ,
         f_vig_final      DATE          ,
         estado_aviso     CHAR(20)      ,
         aviso            CHAR(255)     ,
         usuario          CHAR(20)      ,
         f_alta           DATE
   END RECORD

   DEFINE v_query                  STRING -- detalle
   DEFINE v_indice                 DECIMAL(9,0) -- indice de arreglo       
   DEFINE v_ruta_reporte           STRING -- ruta del archivo del reporte       
   DEFINE v_ruta_listados          STRING -- ruta de los listados
   DEFINE v_ruta_ejecutable        STRING -- ruta del ejecutable
   DEFINE manejador_rpt            om.SaxDocumentHandler 
   DEFINE v_indice_reporte         SMALLINT
   DEFINE v_id_solicitud           DECIMAL(9,0)
   DEFINE v_usuario_liquida        CHAR(20)
   DEFINE v_f_vig_inicio           DATE
   DEFINE v_f_vig_final            DATE
   DEFINE v_f_vig_max              DATE
   DEFINE  v_id_aviso              INTEGER
   DEFINE v_aviso                  CHAR(255)
   DEFINE v_msg                    STRING
   

     
   OPEN WINDOW w_consulta_avisos WITH FORM "RETM4711" 
   LET ventana = ui.Window.getCurrent()
   LET forma   = ventana.getForm()
   LET v_indice = 1

   DIALOG ATTRIBUTES(FIELD ORDER FORM, UNBUFFERED)

      # Tabla de procesos
      DISPLAY ARRAY arr_reg_avisos TO tbl_avisos.*
      END DISPLAY

      BEFORE DIALOG 

         DECLARE cur_avisos CURSOR FOR
                                       SELECT id_aviso, f_vig_inicio, f_vig_final, 
                                              CASE WHEN estado_aviso = 'B' THEN 'BAJA' ELSE 'ACTIVO' END AS estado,
                                              aviso, usuario, f_alta
                                       FROM   ret_aviso_pdf_ssv
                                       ORDER BY f_vig_final DESC; 
         FOREACH cur_avisos INTO arr_reg_avisos[v_indice].*
            IF arr_reg_avisos[v_indice].f_vig_final < TODAY AND arr_reg_avisos[v_indice].estado_aviso = "ACTIVO" THEN 
               LET arr_reg_avisos[v_indice].estado_aviso = "INACTIVO"
            END IF 
            LET v_indice = v_indice + 1
         END FOREACH 
         IF v_indice > 0  THEN
            CALL arr_reg_avisos.deleteElement(v_indice)
         END IF    

      ON ACTION ACCEPT
         EXIT DIALOG 
      ON ACTION cerrar 
         EXIT DIALOG   
      ON ACTION nuevo 
         OPEN WINDOW w_alta_aviso WITH FORM "RETM4712"
            INPUT v_f_vig_inicio, v_f_vig_final, v_aviso
            FROM  date_f_vig_inicio, date_f_vig_final, txed_aviso
            ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)
            BEFORE INPUT
               --- Busca la máxima fecha de vigencia final para colocar el siguiente día como fecha inicial
               SELECT MAX(f_vig_final)
               INTO   v_f_vig_max
               FROM   ret_aviso_pdf_ssv 
               WHERE  estado_aviso = 'A'
               IF v_f_vig_max IS NULL OR v_f_vig_max = '12/31/1899' THEN 
                  LET v_f_vig_inicio = TODAY 
               ELSE 
                  LET v_f_vig_inicio = v_f_vig_max + 1 
               END IF 
               LET v_f_vig_final = v_f_vig_inicio
               DISPLAY v_f_vig_inicio TO date_f_vig_inicio
               DISPLAY v_f_vig_final TO date_f_vig_final
            ON ACTION ACCEPT 
               IF v_f_vig_inicio IS NULL OR 
                  v_f_vig_final IS NULL OR 
                  v_aviso IS NULL THEN
                  LET v_msg = "Debe ingresar la información solicitada para dar de alta el aviso" 
                  CALL fn_mensaje("Atención", v_msg CLIPPED , "information")
                  NEXT FIELD date_f_vig_inicio
                  CONTINUE INPUT 
               END IF 
               IF v_f_vig_inicio <= v_f_vig_max THEN 
                  LET v_msg = "La fecha de vigencia inicial es inválida, no puede estar dentro de un periodo existente" 
                  CALL fn_mensaje("Atención", v_msg CLIPPED , "information")
                  NEXT FIELD date_f_vig_inicio
                  CONTINUE INPUT 
               END IF 
               IF v_f_vig_inicio > v_f_vig_final THEN 
                  LET v_msg = "La fecha de vigencia inicial no puede ser mayor a la fecha de vigencia final" 
                  CALL fn_mensaje("Atención", v_msg CLIPPED , "information")
                  NEXT FIELD date_f_vig_inicio
                  CONTINUE INPUT 
               END IF 
               --- Se inserta el aviso en la tabla ret_aviso_pdf_ssv
               --- Obtenemos el id_aviso de la secuencia
               SELECT seq_ret_aviso_pdf_ssv.nextval 
               INTO   v_id_aviso
               FROM   systables 
               WHERE  tabid = 1;
               INSERT INTO ret_aviso_pdf_ssv 
                    VALUES(v_id_aviso,v_f_vig_inicio, v_f_vig_final,v_aviso,
                           'A',g_usuario, TODAY );
               CALL arr_reg_avisos.clear()
               LET v_indice = 1
               DECLARE cur_avisos_nuevo CURSOR FOR
                                             SELECT id_aviso, f_vig_inicio, f_vig_final,  
                                                    CASE WHEN estado_aviso = 'B' THEN 'BAJA' ELSE 'ACTIVO' END AS estado,
                                                    aviso,usuario, f_alta
                                             FROM   ret_aviso_pdf_ssv
                                             ORDER BY f_vig_final DESC; 
               FOREACH cur_avisos_nuevo INTO arr_reg_avisos[v_indice].*
                  LET v_indice = v_indice + 1
               END FOREACH 
               IF v_indice > 0  THEN
                  CALL arr_reg_avisos.deleteElement(v_indice)
               END IF    
--               DISPLAY ARRAY arr_reg_avisos TO tab_avisos.*
--               END DISPLAY
               LET v_msg = "Se dio de alta el registro exitósamente" 
               CALL fn_mensaje("Atención", v_msg CLIPPED , "information")
               EXIT INPUT 
            ON ACTION CANCEL 
               EXIT INPUT 
            END INPUT
         CLOSE WINDOW w_alta_aviso
      ON ACTION baja
         LET v_msg = "Se dará de baja el aviso de fecha vigencia inicial ", arr_reg_avisos[ARR_CURR()].f_vig_inicio USING "dd/mm/yyyy",
                      "\n y fecha de vigencia final ", arr_reg_avisos[ARR_CURR()].f_vig_final USING "dd/mm/yyyy",
                      "\n ¿Está de acuerdo?"
         CALL fn_mensaje("Atención", v_msg CLIPPED , "information")
         DELETE 
         FROM   ret_aviso_pdf_ssv
         WHERE  id_aviso = arr_reg_avisos[ARR_CURR()].id_aviso
         LET v_indice = 1
         DECLARE cur_avisos_baja CURSOR FOR
                                       SELECT id_aviso, f_vig_inicio, f_vig_final,  
                                              CASE WHEN estado_aviso = 'B' THEN 'BAJA' ELSE 'ACTIVO' END AS estado,
                                              aviso,usuario, f_alta
                                       FROM   ret_aviso_pdf_ssv
                                       ORDER BY f_vig_final DESC; 
         FOREACH cur_avisos_baja INTO arr_reg_avisos[v_indice].*
            LET v_indice = v_indice + 1
         END FOREACH 
         IF v_indice > 0  THEN
            CALL arr_reg_avisos.deleteElement(v_indice)
         END IF    

   END DIALOG       
   
   CLOSE WINDOW w_consulta_avisos

END FUNCTION 