--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => pag                                                                    #
#Programa     => PAGC21                                                                 #
#Objetivo     => Consulta Regsitros de NSS sin cuenta                                   #
#Fecha inicio => ABRIL 11, 2012                                                         # 
#Autor        => Ilhuitemoc Ricardo Ortiz                                               #
#########################################################################################

DATABASE safre_viv


MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida LQINFO
   CALL fn_consulta_registros(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: PAGC21
Nombre: fn_consulta_registros
Fecha creacion: ABRIL 11, 2012
Autor: ILHUITEMOC RICARDO ORTIZ
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_registros(p_usuario_cod)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_fecha                  DATE,
       v_bandera                SMALLINT,
       v_registros              INTEGER

   LET v_bandera = 0
   
   OPEN WINDOW w_consulta_registros WITH FORM "PAGC211"
   
   INPUT v_fecha WITHOUT DEFAULTS
      FROM ed_fecha    ATTRIBUTES (UNBUFFERED)
  
      BEFORE INPUT
         LET v_fecha = TODAY
        
      ON ACTION ACCEPT
         SELECT COUNT(*)
         INTO   v_registros
         FROM   pag_nss_sin_cuenta
         WHERE  f_alta = v_fecha

         IF v_registros > 0 THEN
            LET v_bandera = 1
         ELSE
            LET v_bandera = 2
         END IF
         EXIT INPUT
         
      ON ACTION CANCEL
         LET v_bandera = 3
         EXIT INPUT

    END INPUT

    CASE v_bandera
    WHEN 1
        CALL fn_consulta_detalle(v_fecha)
    WHEN 2
        CALL fn_mensaje("Sin Registros",
                        "No se encontraron registros",   
                        "information")

    END CASE
   
   CLOSE WINDOW w_consulta_registros

END FUNCTION

{
======================================================================
Clave: PAGC21
Nombre: fn_consulta_detalle
Fecha creacion: ABRIL 11, 2012
Autor: Ilhuitemoc Quetxalcoatl Ricardo Ortíz
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_consulta_detalle(v_fecha)

   DEFINE 
      v_fecha  DATE,

      reg_nss DYNAMIC ARRAY OF RECORD
         v_nss        DECIMAL(18), 
         v_f_alta     DATE,
         v_id_origem  LIKE pag_tpo_archivo.archivo_descripcion
      END RECORD,
      v_s_qryTxt      STRING,
      v_i_indice      INTEGER,
      v_id_origen     SMALLINT

   DEFINE manejador_rpt OM.SaxDocumentHandler,
          v_ruta_ejecutable, 
          v_ruta_listados, 
          v_ruta_reporte STRING,
          v_indice INTEGER
   
   LET v_i_indice = 1
   -- se crea la sentencia que busca los archivos disponibles por integrar
   LET v_s_qryTxt = " SELECT nss, f_alta, id_origen \n",
                    "   FROM pag_nss_sin_cuenta \n",
                    "  WHERE f_alta  = '",v_fecha,"' "

    --DISPLAY v_s_qryTxt
   PREPARE prp_registros FROM v_s_qryTxt
   DECLARE cur_registros CURSOR FOR prp_registros 

   FOREACH cur_registros INTO reg_nss[v_i_indice].v_nss,reg_nss[v_i_indice].v_f_alta, v_id_origen
   	  SELECT archivo_descripcion
   	    INTO reg_nss[v_i_indice].v_id_origem
   	    FROM pag_tpo_archivo
   	   WHERE archivo_cod = v_id_origen
      -- se incrementa el indice del arreglo
      LET v_i_indice = v_i_indice + 1
   END FOREACH
   
    --elimina ultimo renglon en blanco        
    CALL reg_nss.deleteElement(reg_nss.getLength())
   
    DISPLAY ARRAY reg_nss TO tbl_registros.*
    
    ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE)
        ON ACTION CANCEL 
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_listados

            --Se asigna la plantilla para generar el reporte
            IF fgl_report_loadCurrentSettings("rpt_nss_sin_cuenta.4rp") THEN 
               CALL fgl_report_selectDevice ("PDF")
                            
               LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","nss_sin_cuenta"
               CALL fgl_report_setOutputFileName(v_ruta_reporte)
               CALL fgl_report_selectPreview(1)
               LET manejador_rpt = fgl_report_commitCurrentSettings() 
                
            ELSE         
               DISPLAY "no fue posible generar el reporte"
               EXIT DISPLAY 
            END IF
            --Inicia el reporte de registros 
            START REPORT rpt_nss_sin_cuenta TO XML HANDLER manejador_rpt
            FOR v_indice = 1 TO reg_nss.getLength()
               OUTPUT TO REPORT rpt_nss_sin_cuenta(reg_nss[v_indice].*)
            END FOR
            FINISH REPORT rpt_nss_sin_cuenta
            
      
    END DISPLAY
END FUNCTION


REPORT rpt_nss_sin_cuenta(reg_nss)
DEFINE reg_nss RECORD
       v_nss        DECIMAL(18,0), 
       v_f_alta     DATE,
       v_id_origem  LIKE pag_tpo_archivo.archivo_descripcion
      END RECORD 
DEFINE v_fecha DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY 
      PRINTX v_fecha USING "DD-MM-YYYY"
      
   ON EVERY ROW
      PRINTX reg_nss.v_nss
      PRINTX reg_nss.v_f_alta USING "DD-MM-YYYY" 
      PRINTX reg_nss.v_id_origem
   

END REPORT