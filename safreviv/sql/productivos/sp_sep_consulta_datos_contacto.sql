






CREATE FUNCTION "safreviv".sp_sep_consulta_datos_contacto(p_estado        smallint ,
                                               p_ind_contacto  smallint ,
                                               p_contactado    smallint )

RETURNING SMALLINT     ,   -- v_ind
          CHAR(3)      ,   -- vi_diag
          INTEGER      ,   -- v_sql_error
          DECIMAL(9,0) ,   -- id_expediente
          DECIMAL(9,0) ,   -- caso_adai
          CHAR(40)     ,   -- estado
          CHAR(11)     ,   -- nss
          CHAR(40)     ,   -- tipo_trabajador
          CHAR(120)    ,   -- nombre
          DEC(10,0)    ,   -- tel1
          DEC(10,0)    ,   -- tel2
          DEC(10,0)    ,   -- cel
          CHAR(40)     ,   -- correo_e
          CHAR(40)     ;   -- contactado de nss_expediente

--
--variables de datos de contacto a regresar
--
DEFINE v_id_expediente     DECIMAL(9,0)  ;
DEFINE v_caso_adai         DECIMAL(9,0)  ;
DEFINE v_estado            CHAR(40)      ;
DEFINE v_nss               CHAR(11)      ;
DEFINE v_tipo_trabajador   CHAR(40)      ;
DEFINE v_nombre            CHAR(120)     ;
DEFINE v_tel1              DECIMAL(10,0) ;
DEFINE v_tel2              DECIMAL(10,0) ;
DEFINE v_cel               DECIMAL(10,0) ;
DEFINE v_correo_e          CHAR(40)      ;
DEFINE v_contactado        CHAR(40)      ;

--
--variables para qrys dinamicos
--

DEFINE v_txt               CHAR(1000)    ;
DEFINE v_where_estado      CHAR(30)      ;
DEFINE v_where_indicador   CHAR(30)      ;
DEFINE v_where_contactado  CHAR(30)      ;

--
--variables de control
--

DEFINE v_ind               SMALLINT      ;
DEFINE v_diag              CHAR(003)     ;
DEFINE v_sql_error         INTEGER       ;

   ON EXCEPTION SET v_sql_error

      LET v_ind  = -1   ;

      RETURN v_ind             ,
             v_diag            ,
             v_sql_error       ,
             v_id_expediente   ,
             v_caso_adai       ,
             v_estado          ,
             v_nss             ,
             v_tipo_trabajador ,
             v_nombre          ,
             v_tel1            ,
             v_tel2            ,
             v_cel             ,
             v_correo_e        ,
             v_contactado      ;

   END EXCEPTION WITH RESUME

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_consulta_datos_contacto.trace';

      LET v_txt               = ''     ;
      LET v_where_estado      = ''     ;
      LET v_where_indicador   = ''     ;
      LET v_where_contactado  = ''     ;

      LET v_id_expediente     = 0;
      LET v_caso_adai         = '';
      LET v_estado            = '';
      LET v_nss               = '';
      LET v_tipo_trabajador   = '';
      LET v_nombre            = '';
      LET v_tel1              = '';
      LET v_tel2              = '';
      LET v_cel               = '';
      LET v_correo_e          = '';
      LET v_contactado        = '';

      LET v_ind               = 0      ;
      LET v_diag              = "000"  ;
      LET v_sql_error         = 0      ;

   --TRACE '1';
--
-- CONSTURCCION DE CONSTRUCT de estado 
--

      IF (p_estado = 0 ) THEN
       LET v_where_estado = " 1=1 ";
      END IF;

      IF (p_estado <> 0 ) THEN
       LET v_where_estado = " a.estado = "||p_estado;
      END IF;

--
-- CONSTURCCION DE CONSTRUCT de indicador de contacto
--

      IF ( p_ind_contacto = 9 ) THEN
       LET v_where_indicador = " 1=1 ";
      END IF;

      IF ( p_ind_contacto <> 9 ) THEN
       LET v_where_indicador = " a.ind_contacto = "||p_ind_contacto;
      END IF;
--
-- CONSTURCCION DE CONSTRUCT de contactado
--

      IF (p_contactado = 0) THEN
       LET v_where_contactado = " 1=1 ";
      END IF;

      IF (p_contactado = 1) THEN
       LET v_where_contactado = " b.contactado = 0 ";
      END IF;

      IF (p_contactado = 2) THEN
       LET v_where_contactado = " b.contactado = 1 ";
      END IF;

      IF (p_contactado = 3) THEN
       LET v_where_contactado = " b.contactado in (0,1) ";
      END IF;

--
--asignacion inicial de variables para evitar el error -696
--

--TRACE '2';

      LET v_caso_adai         = '';
      LET v_estado            = '';
      LET v_nss               = '';
      LET v_tipo_trabajador   = '';
      LET v_nombre            = '';
      LET v_tel1              = '';
      LET v_tel2              = '';
      LET v_cel               = '';
      LET v_correo_e          = '';
      LET v_contactado        = '';

--
-- construccion de consulta dinamica para expedientes por estado y tipo de flujo
--

LET v_txt= " select a.id_expediente, a.caso_adai, c.descripcion, b.nss, "||
           " CASE WHEN b.tipo_trabajador = 1 THEN 'ACREDITADO' WHEN b.tipo_trabajador = 2 "||
           " THEN 'TRABAJADOR' ELSE 'SIN CLASIFICAR' END CASE "||
           " ,b.nombre, b.tel_contacto1, "||
           " b.tel_contacto2, b.tel_celular, b.correo_e, "||
           " CASE WHEN b.contactado = 0 THEN 'SIN CONTACTAR' WHEN b.contactado = 2 "||
           " THEN 'CONTACTADO' ELSE 'SIN INDICADOR' END CASE "||
           " from sep_expediente a, sep_nss_expediente b, sep_estado_expediente c "||
           " where "||v_where_estado||" and "||v_where_indicador||" and a.id_expediente = b.id_expediente and a.estado = c.estado "||
           " and "||v_where_contactado|| ";";
--TRACE '3';

      PREPARE prp_consulta FROM v_txt;
      ----TRACE '4';
      DECLARE cur_consulta CURSOR FOR prp_consulta;
      --TRACE '5';
      OPEN cur_consulta;
      --TRACE '6';
      WHILE (SQLCODE ==0)

      FETCH cur_consulta INTO v_id_expediente    ,
                              v_caso_adai        ,
                              v_estado           ,
                              v_nss              ,
                              v_tipo_trabajador  ,
                              v_nombre           ,
                              v_tel1             ,
                              v_tel2             ,
                              v_cel              ,
                              v_correo_e         ,
                              v_contactado       ;

   --TRACE '7';

-- regresa datos encontrados mientras existan

   IF (SQLCODE == 0) THEN

     RETURN v_ind             ,
            v_diag            ,
            v_sql_error       ,
            v_id_expediente   ,
            v_caso_adai       ,
            v_estado          ,
            v_nss             ,
            v_tipo_trabajador ,
            v_nombre          ,
            v_tel1            ,
            v_tel2            ,
            v_cel             ,
            v_correo_e        ,
            v_contactado      WITH RESUME;
   END IF

END WHILE

END FUNCTION;


