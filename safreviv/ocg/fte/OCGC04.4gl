#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC04                                        #
#Objetivo            => Programa que realiza la consulta general      #
#                       de archivos                                   #
#Autor               => Héctor F. Jiménez Lara                        #
#Fecha inicio        => 23 Octubre 2015                               #
#######################################################################
DATABASE safre_viv

   DEFINE v_s_qry             STRING 

MAIN
   DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo          STRING   -- titulo de la ventana
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".OCGC04.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL fn_consulta_archivos()

END MAIN

FUNCTION fn_consulta_archivos()
   DEFINE v_arr_arch DYNAMIC ARRAY OF RECORD
      nombre_arch CHAR (40)
   END RECORD
   DEFINE v_nom_arch CHAR(40)
   DEFINE v_cnt       SMALLINT
   DEFINE v_pos       SMALLINT

   CALL v_arr_arch.clear()
   LET v_cnt = 1
   LET v_s_qry = " SELECT nom_archivo
                     FROM ocg_ctr_archivo "

   PREPARE prp_cons_arch FROM v_s_qry
   DECLARE cur_c_arch CURSOR FOR prp_cons_arch

   FOREACH cur_c_arch INTO v_arr_arch[v_cnt].*
      LET v_cnt = v_cnt + 1 
   END FOREACH 
   CALL v_arr_arch.deleteElement(v_arr_arch.getLength())

   OPEN WINDOW w_arch WITH FORM "OCGC041"
      DISPLAY ARRAY v_arr_arch TO r_archivo.*
         BEFORE ROW 
            LET v_pos = ARR_CURR()
            --LET v_rec_arch.* = v_arr_arch[v_pos].*
            LET v_nom_arch = v_arr_arch[v_pos].*
         ON ACTION ACCEPT 
            CALL fn_consulta_detalle(v_nom_arch)
      END DISPLAY
   CLOSE WINDOW w_arch 
END FUNCTION

FUNCTION fn_consulta_detalle(p_nom_arch)
   DEFINE p_nom_arch CHAR(40)
   DEFINE v_rec_det_arch RECORD
      f_proceso DATE,
      tot_sp1 DECIMAL(10,0),
      tot_sp2 DECIMAL(10,0),
      tot_sp3 DECIMAL(10,0),
      tot_sp4 DECIMAL(10,0),
      tot_sp5 DECIMAL(10,0),
      tot_registros DECIMAL(10,0)
   END RECORD 
   DEFINE v_id_ocg_ctr_archivo DECIMAL(9,0)

   SELECT id_ocg_ctr_Archivo 
     INTO v_id_ocg_ctr_archivo
     FROM ocg_ctr_archivo 
    WHERE nom_archivo = p_nom_arch 

   LET v_s_qry = " SELECT f_proceso,
                          tot_sp1,
                          tot_sp2,
                          tot_sp3,
                          tot_sp4,
                          tot_sp5,
                          tot_registros
                     FROM ocg_ctr_archivo 
                    WHERE nom_archivo = ? "

   PREPARE prp_det FROM v_s_qry
   EXECUTE prp_det USING p_nom_arch INTO v_rec_det_arch.*

   OPEN WINDOW w_det WITH FORM "OCGC042"
      DISPLAY BY NAME p_nom_arch
      DISPLAY BY NAME v_rec_det_arch.*
      MENU 
         ON ACTION detalle 
           CALL fn_det_arch(v_id_ocg_ctr_archivo)
         ON ACTION CANCEL 
            EXIT MENU 
      END MENU 
   CLOSE WINDOW w_det
END FUNCTION 


FUNCTION fn_det_arch(p_id_arch)
   DEFINE p_id_arch    INTEGER
   DEFINE v_qry        STRING
   DEFINE v_idx        INTEGER
   DEFINE v_arr_detalle DYNAMIC ARRAY OF RECORD
      v_edo            SMALLINT,
      v_sp             SMALLINT,
      v_ent_financiera INTEGER ,
      v_diagnostico    INTEGER ,
      v_f_proceso      DATE    ,
      v_nss            CHAR(11),
      v_rfc            CHAR(13),
      v_nombre         CHAR(120)
   END RECORD

   LET v_qry = "SELECT c.estado,                                  
                       b.subproceso,                              
                       b.cve_ent_financiera,                      
                       a.diagnostico,                             
                       b.f_proceso,                               
                       b.nss,a.rfc,                               
                  TRIM (a.ap_paterno)                             
                       ||' ' ||                                   
                  TRIM (a.ap_materno)                             
                       ||' '||                                    
                  TRIM (a.nombre) as nombre                      
                  FROM ocg_tramite a,                             
                       ocg_detalle b,                             
                       ocg_ctr_archivo c                          
                 WHERE b.id_ocg_ctr_archivo = ",p_id_arch,"                 
                   AND c.id_ocg_ctr_archivo = b.id_ocg_ctr_archivo
                   AND a.id_ocg_detalle = b.id_ocg_detalle"

   PREPARE prp_detalle FROM v_qry
   DECLARE cur_det CURSOR FOR prp_detalle

   LET v_idx = 1
   
   FOREACH cur_det INTO v_arr_detalle[v_idx].*
      LET v_idx = v_idx + 1
   END FOREACH

   CALL v_arr_detalle.deleteElement(v_arr_detalle.getLength())

   OPEN WINDOW w_lalito WITH FORM "OCGC043"
      DISPLAY ARRAY v_arr_detalle TO tab_detalle.*

   CLOSE WINDOW w_lalito
END FUNCTION

