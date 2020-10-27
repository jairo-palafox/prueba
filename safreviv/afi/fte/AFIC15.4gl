######################################################################
#Modulo            => AFI                                            #
#Programa          => AFIC15                                         #
#Objetivo          => Consulta de actualizacion de informacion de    #
#                     un NSS en específico.                          #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 27/Marzo/2018                                  #
######################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_nss               CHAR(11)
END GLOBALS 

MAIN

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF
    
   CALL STARTLOG(g_usuario CLIPPED|| ".AFIC15.log")

   CLOSE WINDOW SCREEN

   CALL consulta_movimientos()
   
END MAIN 

FUNCTION consulta_movimientos()

   DEFINE v_ind_numero  SMALLINT
   DEFINE v_conteo_dh   INTEGER
   
   OPEN WINDOW vtn1 WITH FORM "AFIC151"

      INPUT BY NAME v_nss ATTRIBUTE(UNBUFFERED,WITHOUT DEFAULTS)
         BEFORE INPUT 
            LET v_nss = NULL
            
         ON ACTION ACCEPT
            # VALIDA CAPTURA
            IF(v_nss IS NULL) THEN
               CALL fn_mensaje("","Debe ingresar el nss","")
               NEXT FIELD v_nss
            END IF 
            IF(LENGTH (v_nss) <  11) THEN  
               CALL fn_mensaje("","El nss debe ser de 11 dígitos","")
               NEXT FIELD v_nss
            END IF
            #Valida que el nss sea numérico
            LET v_ind_numero = fn_es_numerico(v_nss)
            IF(v_ind_numero = 1) THEN
               CALL fn_mensaje("","El nss debe ser numérico","")
               NEXT FIELD v_nss
            END IF
            
            LET v_conteo_dh = 0
            
            SELECT COUNT(*)
              INTO v_conteo_dh
              FROM afi_derechohabiente
              WHERE nss = v_nss
              AND ind_estado_cuenta = 0; --Marca cuenta activa
            
            IF(v_conteo_dh = 0) THEN
               CALL fn_mensaje("","No existe el registro con cuenta activa en la base de derechohabientes","")
               NEXT FIELD v_nss
            END IF 

            # INFORMACIÓN DEL DERECHOHABIENTE
            CALL informacion_derechohabiente()
            
         ON ACTION CANCEL
            LET v_nss = NULL  
            EXIT INPUT 

      END INPUT 
   
   CLOSE WINDOW vtn1
   
END FUNCTION

FUNCTION informacion_derechohabiente()

   DEFINE v_qry_his     STRING 
   DEFINE c             INTEGER
   DEFINE r_dh          RECORD
      nss          CHAR(11),
      rfc          CHAR(13),
      curp         CHAR(18),
      ap_paterno   CHAR(40),
      ap_materno   CHAR(40),
      nombre       CHAR(40),
      nombre_imss  CHAR(50),
      f_nacimiento DATE 
   END RECORD 
   
   --arreglo historia actualizaciones
   DEFINE arr_historia DYNAMIC ARRAY OF RECORD
      nss            CHAR(11),
      proceso_desc   CHAR(40),
      usuario        CHAR(20),
      usuario_desc   CHAR(40),
      modifica_desc  CHAR(30),
      nombre         CHAR(40),
      ap_paterno     CHAR(40),
      ap_materno     CHAR(40),
      rfc            CHAR(13),
      curp           CHAR(18),
      f_nacimiento   DATE,
      f_modifica     DATE
   END RECORD 

   # Información actual del trabajador
   LET v_qry_his = "SELECT nss,
                            rfc,
                            curp,
                            ap_paterno_af,
                            ap_materno_af,
                            nombre_af,
                            nombre_imss,
                            f_nacimiento
                       FROM afi_derechohabiente
                      WHERE nss = ","'",v_nss,"'"

   INITIALIZE r_dh.* TO NULL 
   
   PREPARE prp_ind_dh FROM v_qry_his
   EXECUTE prp_ind_dh INTO r_dh.* 

   # Carga arreglo de la historia de actualizaciones
   LET v_qry_his = "SELECT afi.nss,
                           cat.proceso_desc,
                           glo.usuario,
                       NVL (seg.usuario_desc,'Infonavit'),
                           ind.ind_modifica_desc,
                           his.nombre_af,
                           his.ap_paterno_af,
                           his.ap_materno_af,
                           his.rfc,
                           his.curp,
                           his.f_nacimiento,
                           his.f_modifica
                      FROM afi_derechohabiente afi,
                           afi_his_derechohabiente his,
                           glo_folio glo,
                           cat_proceso cat,
                     OUTER seg_usuario seg,
                           cat_afi_ind_modifica ind
                     WHERE afi.nss = ","'",v_nss,"'","
                       AND afi.id_derechohabiente  = his.id_derechohabiente
                       AND his.f_modifica          = glo.f_actualiza
                       AND his.folio_lote_modifica = glo.folio
                       AND glo.proceso_cod IN (1803,1819,1820,1821,1823,1824,1825)
                       AND glo.proceso_cod = cat.proceso_cod
                       AND glo.usuario = seg.usuario_cod
                       AND his.ind_modifica = ind.ind_modifica
                       ORDER BY his.f_modifica DESC"
   
   PREPARE prp_historia FROM v_qry_his
   DECLARE crs_historia CURSOR FOR prp_historia

   CALL arr_historia.clear()
   
   LET c = 1

   FOREACH crs_historia INTO arr_historia[c].nss,
                              arr_historia[c].proceso_desc,
                              arr_historia[c].usuario,
                              arr_historia[c].usuario_desc,
                              arr_historia[c].modifica_desc,
                              arr_historia[c].nombre,
                              arr_historia[c].ap_paterno,
                              arr_historia[c].ap_materno,
                              arr_historia[c].rfc,
                              arr_historia[c].curp,
                              arr_historia[c].f_nacimiento,
                              arr_historia[c].f_modifica

      IF(arr_historia[c].usuario = "SAFREVIV") OR 
        (arr_historia[c].usuario = "safreviv") OR 
        (arr_historia[c].usuario = "infonavit") THEN
        LET arr_historia[c].usuario = "INFONAVIT"
      END IF 
                             
      LET c = c + 1
               
   END FOREACH 

   # Elimina fila en blanco
   IF(arr_historia[arr_historia.getLength()].nss IS NULL) THEN
      CALL arr_historia.deleteElement(arr_historia.getLength()) 
   END IF 

   OPEN WINDOW vtn2 WITH FORM "AFIC152"

      DISPLAY r_dh. nss         TO e_nss         
      DISPLAY r_dh.rfc          TO e_rfc         
      DISPLAY r_dh.curp         TO e_curp        
      DISPLAY r_dh.ap_paterno   TO e_ap_paterno  
      DISPLAY r_dh.ap_materno   TO e_ap_materno  
      DISPLAY r_dh.nombre       TO e_nombre      
      DISPLAY r_dh.nombre_imss  TO e_nombre_imss 
      DISPLAY r_dh.f_nacimiento TO e_f_nacimiento
              
      DISPLAY ARRAY arr_historia TO record2.* ATTRIBUTE(ACCEPT = FALSE, CANCEL = FALSE)

            
         ON ACTION Salir
            LET v_nss = NULL  
            EXIT DISPLAY   
            
      END DISPLAY 
       
   CLOSE WINDOW vtn2
   
END FUNCTION 

FUNCTION fn_es_numerico(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN

   LET p_cadena = p_cadena CLIPPED

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE
         LET indicador = 1
         EXIT FOR
      END IF
   END FOR

   RETURN indicador

END FUNCTION



