--GLOBALS "consultaRelLaboral.dir/ret_ws_cons_rel_laboral.inc" -- consulta de pago a FICO

GLOBALS "OCGW03.inc"

DEFINE v_s_qry          STRING
{
MAIN 
   DEFINE v_nss CHAR(11)
   CALL startlog("/safreviv/ocg/bin/OCGW04.log")
   OPEN WINDOW w_1 WITH FORM "OCGW041"
      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED)
         ON ACTION ACCEPT 
            CALL fn_fecha_ultima_relacion_laboral(v_nss)
      END INPUT
   CLOSE WINDOW w_1
END MAIN 
}
--FUNCTION fn_fecha_ultima_relacion_laboral(p_nss)
MAIN 
   DEFINE p_nss            CHAR(11)
   DEFINE v_ws_status      SMALLINT -- estatus de ejecución de un webservice
   DEFINE v_indice         INTEGER
   DEFINE v_fecha_fin      DATE
   DEFINE v_fecha_f        DATE
   DEFINE v_fecha_c        STRING
   DEFINE v_con_sin_rl     SMALLINT
   DEFINE v_nrp            VARCHAR(100)
   DEFINE v_f_alta         DATE
   DEFINE v_f_alta_format  STRING
   DEFINE v_f_baja         DATE
   DEFINE v_delegacion     DATE
   DEFINE v_nom_delegacion VARCHAR(100)
   DEFINE v_r_nss          CHAR(11)

   LET ns1consultarTrabajador.nss = p_nss

   -- se invoca la consulta
   CALL consultarTrabajador_g() RETURNING v_ws_status

   -- si el webservice NO se ejecuto correctamente
   IF ( v_ws_status <> 0 ) THEN      

      DISPLAY "ERROR al invocar webservice de consulta de pago"
      DISPLAY "CODE       : ", wsError.code
      DISPLAY "CODENS     : ", wsError.codeNS
      DISPLAY "DESRIPTION : ", wsError.description
      DISPLAY "ACTION     : ", wsError.action
	  
	  -- se devuelve el codigo de error del WS y fecha nula
      RETURN wsError.code, NULL
   END IF

   {
Cuando la fecha de baja de relación laboral sea: 9999-12-31, quiere decir que la 
relación laboral aún se encuentra vigente.

En los casos que el servicio conteste el campo NRP, como blanco o ceros, y los campos 
de fecha de alta y fecha de baja tengan el formato 1900-01-01, significa que para ese 
NSS no existe relación laboral.
   }
   
   --fecha de referencia inicial para la comparación
   LET v_fecha_fin = DATE ( "01/01/1899" )
   LET v_con_sin_rl = 0   -- Inicializa el indicador 

   -- Se invoca a la funcion que crea la tabla temporal 
   CALL fn_crea_temporal()
   FOR v_indice = 1 TO ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab.getLength()
      --se formatea la fecha
	   CALL ERRORLOG("Fecha encontrada en WS de relacion laboral")
      LET v_fecha_c = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja
	   CALL ERRORLOG(ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja)
      LET v_fecha_c = v_fecha_c.subString(6,7),"/",v_fecha_c.subString(9,10),"/",v_fecha_c.subString(1,4)
      LET v_fecha_f = DATE(v_fecha_c)

      --consulta fecha alta
      LET v_f_alta_format = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaAlta
      LET v_f_alta_format = v_f_alta_format.subString(6,7),"/",v_f_alta_format.subString(9,10),"/",v_f_alta_format.subString(1,4)
      LET v_f_alta = DATE(v_f_alta_format)
{
	   -- se selecciona la fecha mas alta
      IF ( v_fecha_f > v_fecha_fin ) THEN
         LET v_fecha_fin = v_fecha_f
      END IF
}

      LET v_r_nss          = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.nss
      LET v_delegacion     = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].delegacion
      LET v_nom_delegacion = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nomDelegacion
      LET v_nrp            = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nrp

      LET v_s_qry = "INSERT INTO safre_tmp:tmp_relacion_laboral values(?,?,?);"

      PREPARE prp_ins_tmp FROM v_s_qry
      EXECUTE prp_ins_tmp USING v_r_nss,
                                v_f_alta,
                                v_fecha_f

   END FOR

   -- si la fecha mayor es 9999-12-31 entonces aun hay relacion laboral
   IF ( v_fecha_fin = "12/31/9999" ) THEN
      -- no se devuelve fecha
      LET v_fecha_fin = NULL
      LET v_con_sin_rl = 1  -- Con Relacion Laboral
   END IF
   
   -- si la fecha es 1900-01-01, entonces no hay relacion laboral
   IF ( v_fecha_fin = "01/01/1900" ) THEN
      LET v_fecha_fin = NULL
      LET v_con_sin_rl = 2   -- Sin Relacion Laboral
   END IF

   -- se devuelve el resultado de la ejecucion
   --RETURN v_ws_status, v_fecha_fin, v_con_sin_rl
END MAIN

FUNCTION fn_crea_temporal()
      DATABASE safre_tmp
      LET v_s_qry = "DROP TABLE IF EXISTS tmp_relacion_laboral; \n
                     CREATE TABLE tmp_relacion_laboral(
                        nss     char(11),
                        f_alta  date,
                        f_baja  date,
                        );"

      PREPARE prp_Crea_t FROM v_s_qry
      EXECUTE prp_Crea_t

      IF sqlca.sqlcode = 0 THEN
         DISPLAY "Se crea la tabla temporal de relaciones laborales" 
      END IF 
END FUNCTION 