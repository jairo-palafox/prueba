######################################################################
#Modulo        => AFI                                                #
#Programa      => AFIC017                                            #
#Descripción   => Consulta Prospecto de Unificación                  #
#Autor         => Jose Eduardo Ventura Bonola                        #
#Fecha         =>17 DE FEBRERO DE 2015                               #
######################################################################

DATABASE safre_viv

   DEFINE p_usuario                CHAR(20)      -- Obtiene dato de usuario
   DEFINE p_tipo_ejecucion         SMALLINT      -- Forma como ejecutará el programa
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE v_extension              STRING        -- Variable para validar que la extensión sea ".txt"
   DEFINE v_nom_archivo            STRING        -- Variable para validar que el nombre de archivo comienze con ""
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate -- Ruta donde se deja archivo rescatado del usuario
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio-- Ruta donde se deja archivo rescatado del usuario
   DEFINE v_nom_arh_rescate        STRING        -- Nombre de archivo en ruta rescate
   DEFINE y                        INTEGER       -- Contador para arreglo de unificados
   DEFINE v_nss                    CHAR(11)      -- id derechohabiente para nss consultado
   DEFINE v_cta_unificado          INTEGER       -- variable valida existencia de NSS en tabla unificados
   DEFINE v_cta_unificador         INTEGER       -- variable valida existencia en tabla unificador
   DEFINE r_bandera                INTEGER       -- Bandera para llamar llenado de tabla 
   DEFINE v_id_pre_unificador      INTEGER       -- variable donde se allmacena id_pre_identificador
   DEFINE v_query_unificado        STRING        -- Consulta de unificados
   DEFINE c                        INTEGER       -- Contador General para llenar tabla de porspectos de unificación
   DEFINE ch                       base.Channel 
   DEFINE tok                      base.StringTokenizer
   DEFINE buf                      base.StringBuffer

   DEFINE arr_tabla_prospecto DYNAMIC ARRAY OF RECORD -- arreglo para desplegar datos de unificador y unificado
        nss                       CHAR(11),
        tipo                      CHAR(20),
        marca                     SMALLINT,
        mov_imss                  CHAR(20),
        f_movimiento              DATE,
        op_21_disp                CHAR(30),
        op_21_confronta           CHAR(30),
        op_22_conclucion          CHAR(30)
   END RECORD

   DEFINE arr_tabla DYNAMIC ARRAY OF RECORD -- aareglo para almacenar datos de nss unificado
          nss                      CHAR(11),
          estado                   SMALLINT,
          f_ap_unificado           DATE
    END RECORD

   DEFINE arr_tabla_tmp_unificador DYNAMIC ARRAY OF RECORD --a arreglo para almacenar datos de nss unificador
        nss                        CHAR(11),
        id                         INTEGER ,
        f_apertura                 DATE,
        id_pre_unificador          INTEGER,
        estado                     SMALLINT
   END RECORD


MAIN
   -- se recupera la clave de usuario desde parámetro
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".AFIC07.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   MENU "Seleccionar modo de entrada"

   ON ACTION NSS
      CALL fn_nss()
      EXIT MENU

   ON ACTION ARCHIVO
      CALL fn_archivo()
      EXIT MENU

   ON ACTION CANCEL
      EXIT MENU

   END MENU

END MAIN

FUNCTION fn_consulta_registros(p_id_derechohabiente)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       v_id_unificador      DECIMAL (9,0),
       v_id_unificado       DECIMAL (9,0),
       v_tipo_nss           SMALLINT

   --Identificar si es unificador o unificado
   SELECT id_unificador
   INTO   v_id_unificador
   FROM   uni_det_unificador
   WHERE  id_derechohabiente = p_id_derechohabiente
   AND    estado_familia = 1 

   IF v_id_unificador IS NOT NULL THEN
      SELECT id_unificado
      INTO   v_id_unificado 
      FROM   uni_det_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    estado_unificacion = 1

      IF v_id_unificado IS NOT NULL THEN
         LET v_tipo_nss = 3
         CALL fn_mensaje ("Archivo","El NSS es unificador y unificado","information")
      ELSE 
         LET v_tipo_nss = 1
         CALL fn_mensaje ("Archivo","El NSS es unificador","information")
      END IF
   ELSE
      SELECT id_unificado
      INTO   v_id_unificado 
      FROM   uni_det_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    estado_unificacion = 1
      
      IF v_id_unificado IS NOT NULL THEN
         LET v_tipo_nss = 2
         CALL fn_mensaje ("Archivo","El NSS es unificado","information")
      END IF
   END IF 

   RETURN v_tipo_nss
END FUNCTION 

FUNCTION fn_detalles_nss(p_id_derechohabiente)
DEFINE p_id_derechohabiente DECIMAL (9,0),
       v_id_unificador      DECIMAL (9,0),
       v_id_unificado       DECIMAL (9,0),
       v_tipo_nss           SMALLINT

   --Identificar si es unificador o unificado
   SELECT id_unificador
   INTO   v_id_unificador
   FROM   uni_det_unificador
   WHERE  id_derechohabiente = p_id_derechohabiente
   AND    estado_familia = 1 

   IF v_id_unificador IS NOT NULL THEN
      SELECT id_unificado
      INTO   v_id_unificado 
      FROM   uni_det_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    estado_unificacion = 1

      IF v_id_unificado IS NOT NULL THEN
         LET v_tipo_nss = 3
         CALL fn_mensaje ("Archivo","El NSS es unificador y unificado","information")
      ELSE 
         LET v_tipo_nss = 1
         CALL fn_mensaje ("Archivo","El NSS es unificador","information")
      END IF
   ELSE
      SELECT id_unificado
      INTO   v_id_unificado 
      FROM   uni_det_unificado 
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    estado_unificacion = 1
      
      IF v_id_unificado IS NOT NULL THEN
         LET v_tipo_nss = 2
         CALL fn_mensaje ("Archivo","El NSS es unificado","information")
      END IF
   END IF 
          
END FUNCTION

 
FUNCTION fn_nss()

   CALL fn_tablas_temporales()

   OPEN WINDOW nss WITH FORM "AFIC072"

   INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT
      IF length(v_nss) <> 11 THEN
         CALL fn_mensaje ("Archivo","NSS no contiene 11 caracteres","information")
         LET v_nss = NULL

      ELSE
         SELECT afi.nss,afi.id_derechohabiente,
                afi.f_apertura
           FROM afi_derechohabiente afi
          WHERE afi.nss = v_nss
         INTO TEMP tmp_nss

         SELECT COUNT (*)
           INTO v_cta_unificador
           FROM uni_pre_unificador a,
                tmp_nss t
          WHERE a.id_derechohabiente = t.id_derechohabiente

         IF v_cta_unificador > 0 THEN  
            LET r_bandera = 1
            SELECT t.nss,
                   t.id_derechohabiente,
                   t.f_apertura,
                   a.id_pre_unificador,
                   a.estado
              FROM uni_pre_unificador a,
                   tmp_nss t
             WHERE a.id_derechohabiente = t.id_derechohabiente
            INTO TEMP tmp_unificador

            CALL fn_datos_arreglo()

         ELSE

            SELECT COUNT (*)
              INTO v_cta_unificado
              FROM uni_pre_unificado b,
                   tmp_nss t
             WHERE b.id_derechohabiente = t.id_derechohabiente

            IF v_cta_unificado > 0 THEN 
               LET r_bandera = 1
               SELECT MAX (id_pre_unificador)
                 INTO v_id_pre_unificador
                 FROM uni_pre_unificado b,
                      tmp_nss t
                WHERE b.id_derechohabiente = t.id_derechohabiente

               SELECT a.nss_correcto,
                      t.id_derechohabiente,
                      t.f_apertura,
                      a.id_pre_unificador,
                      a.estado
                 FROM uni_pre_unificador a,
                      tmp_nss t
                WHERE a.id_pre_unificador = v_id_pre_unificador
               INTO TEMP tmp_unificador

               CALL fn_datos_arreglo()

            END IF 

            IF v_cta_unificado = 0 
            AND v_cta_unificador = 0 THEN 

               LET arr_tabla_prospecto[1].nss              = v_nss
               LET arr_tabla_prospecto[1].tipo             = ""
               LET arr_tabla_prospecto[1].marca            = ""
               LET arr_tabla_prospecto[1].mov_imss         = "NO ASOCIADO"
               LET arr_tabla_prospecto[1].f_movimiento     = ""
               LET arr_tabla_prospecto[1].op_21_disp       = ""
               LET arr_tabla_prospecto[1].op_21_confronta  = ""
               LET arr_tabla_prospecto[1].op_22_conclucion = ""

            END IF
         END IF
      END IF

      IF r_bandera = 1 THEN
         CALL fn_despliega_tabla()
         EXIT INPUT
      END IF

      ON ACTION CLOSE
         EXIT INPUT
      END INPUT
   CLOSE WINDOW nss

END FUNCTION
--************************************************************
--Funcion que despliega datos de tabla principal de consulta *
--************************************************************
FUNCTION fn_despliega_tabla()

   OPEN WINDOW tabla WITH FORM "AFIC073"

   DISPLAY ARRAY arr_tabla_prospecto TO tab_uni.*

   ON ACTION ACCEPT
      EXIT DISPLAY

   ON ACTION CLOSE
      EXIT DISPLAY

   END DISPLAY

   CLOSE WINDOW tabla
END FUNCTION

--**************************************
--Función para validar archivo con NSS *
--**************************************
FUNCTION fn_archivo()

   DEFINE v_archivo                STRING   --Nombre de archivo seleccionado
   DEFINE v_pos                    INTEGER  -- Posición donde inicia la extensión ".txt"
   DEFINE cant                     INTEGER  -- Cantidad de caracteres que tiene el nombre del archivo
   DEFINE v_pos_1                  INTEGER  -- Posición donde inicia el nombre del archivo
   DEFINE v_pos_2                  INTEGER  -- Posición donde inicia el nombre del archivo
   DEFINE v_pos_3                  INTEGER  -- Posición donde inicia el nombre del archivo
   DEFINE v_pos_nom1               INTEGER  -- Posición donde termina nombre de v_pos_3
   DEFINE v_pos_nom2               INTEGER  -- Posición donde termina nombre de v_pos_2
   DEFINE v_pos_nom3               INTEGER  -- Posición donde termina nombre de v_pos_3

   OPEN WINDOW archivo WITH FORM "AFIC071"

      -- Se recuperan las rutas de rescate y envio para el archivo
      SELECT ruta_rescate,
             ruta_envio
      INTO   v_ruta_rescate,
             v_ruta_envio             
      FROM   seg_modulo
      WHERE  modulo_cod ="afi"

      DISPLAY "ruta rescate : ",v_ruta_rescate 

   
   INPUT BY NAME v_archivo ATTRIBUTES (UNBUFFERED)

   AFTER INPUT
      ON ACTION ACCEPT

      --*************************************************************
      --Se valida que el archivo tenga nombre y extensión correctos *
      --*************************************************************

      IF v_archivo IS NULL THEN
         CALL fn_mensaje ("Archivo","Debe de seleccionar un archivo","information")
         NEXT FIELD v_archivo
      END IF

      IF v_archivo.getIndexOf(" ", 1) THEN
         LET v_archivo = ""
         DISPLAY BY NAME v_archivo
         NEXT FIELD v_archivo
      END IF

      IF v_archivo IS NOT NULL THEN 
         CALL fn_mensaje ("Transferencia" ,v_archivo,"information")
         LET buf = base.StringBuffer.create()
         CALL buf.append(v_archivo)

         LET cant         = LENGTH(v_archivo)
         LET v_pos        = buf.getIndexof(".uni",1)
         LET v_extension  = buf.subString(v_pos,cant)
         LET v_pos_1      = buf.getIndexof("prosunifica",1)
         LET v_pos_2      = buf.getIndexof("Prosunifica",1)
         LET v_pos_3      = buf.getIndexof("PROSUNIFICA",1)
         LET v_pos_nom1   = (v_pos_1 + 10 )
         LET v_pos_nom2   = (v_pos_2 + 10 )
         LET v_pos_nom3   = (v_pos_3 + 10 )

         IF v_pos_1 <> 0  THEN
            LET v_nom_archivo = buf.subString (v_pos_1,v_pos_nom1)
         END IF

         IF v_pos_2 <> 0  THEN
            LET v_nom_archivo = buf.subString (v_pos_2,v_pos_nom2)
         END IF

         IF v_pos_3 <> 0  THEN
            LET v_nom_archivo = buf.subString (v_pos_3,v_pos_nom3)
         END IF

         IF v_pos_1 = 0 AND v_pos_2 = 0 AND v_pos_3 =0 THEN 
            CALL fn_mensaje ("Transferencia Archivo","El nombre del archivo no es correcto  \n
                              debe iniciar con PROSUNIFICA y \n
                              debe tener extensión uni","information")
            LET v_archivo = ""
            DISPLAY BY NAME v_archivo
            NEXT FIELD v_archivo
         END IF 

         IF v_nom_archivo = ("PROSUNIFICA") 
         OR v_nom_archivo = ("prosunifica")
         OR v_nom_archivo = ("Prosunifica")
        AND v_extension   = (".uni") THEN

--***************************************************
--Se  recupera el archivo y se deja en ruta rescate *
--***************************************************
            LET v_nom_arh_rescate = v_ruta_rescate CLIPPED,"/",v_nom_archivo||v_extension
DISPLAY v_nom_arh_rescate
            TRY
    
               CALL FGL_GETFILE(v_archivo,v_nom_arh_rescate)
               MESSAGE "ARCHIVO TRANSFERIDO CORRRECTAMENTE"
               CALL fn_obtiene_nss()
               EXIT INPUT
    
            CONTINUE INPUT
    
            CATCH
               ERROR "NO SE PUDO TRANSFERIR"
               CONTINUE INPUT
            END TRY
         EXIT INPUT
    
      END IF
   END IF

   EXIT INPUT
   ON ACTION CANCEL
      EXIT INPUT
      END INPUT
   CLOSE WINDOW archivo

END FUNCTION
--****************************************************
--Función que deja NSS de archivo en tabla temporal  *
--****************************************************
FUNCTION fn_obtiene_nss()

   DEFINE x                        INTEGER    -- Variable para utilizar de contador
   DEFINE s                        CHAR (21)  -- variable para leer lineas del archivo
   DEFINE cadena                   CHAR (11)  -- variable para rescatar lineas del archivo

   CALL fn_tablas_temporales()

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nom_arh_rescate,"r")
   LET buf = base.StringBuffer.create()

   LET x = 1
   WHILE TRUE
      LET s = ch.readLine()
      LET tok = base.StringTokenizer.create(s," ")
      WHILE tok.hasMoreTokens()
         --DISPLAY "token:", tok.nextToken()
         LET cadena = tok.nextToken()

         INSERT INTO tmp_nss_arh VALUES (cadena)
          DISPLAY "VALORES INSERTADOS EN TABLA  ",cadena
      END WHILE

      IF ch.isEof() THEN EXIT WHILE
      END IF
         LET x = x + 1
   END WHILE
      CALL ch.close()
      CALL fn_busca_nss()

      IF r_bandera = 1 THEN
    CALL fn_despliega_tabla()
     --EXIT INPUT
   END IF

END FUNCTION
--*****************************************************
--Función para desplegar resultados de NSS de archivo *
--*****************************************************
FUNCTION fn_busca_nss()

   DEFINE v_cta_tot_arh            INTEGER

   SELECT afi.nss,
          afi.id_derechohabiente,
          afi.f_apertura
     FROM afi_derechohabiente afi,
          tmp_nss_arh rh
    WHERE afi.nss = rh.nss
INTO TEMP tmp_nss

   SELECT COUNT (*)
    INTO v_cta_unificador
    FROM uni_pre_unificador a,
         tmp_nss t
   WHERE a.id_derechohabiente = t.id_derechohabiente

   SELECT COUNT (*)
     INTO v_cta_unificado
     FROM uni_pre_unificado b,
          tmp_nss t
    WHERE b.id_derechohabiente = t.id_derechohabiente

   SELECT COUNT (*) 
     INTO v_cta_tot_arh
     FROM tmp_nss_arh
-- Consulta para cuando NSS son Unificadores
   IF v_cta_tot_arh = v_cta_unificador THEN

      LET r_bandera = 1

      SELECT t.nss,
             t.id_derechohabiente,
             t.f_apertura,
             a.id_pre_unificador,
             a.estado
        FROM uni_pre_unificador a,
             tmp_nss t
       WHERE a.id_derechohabiente = t.id_derechohabiente
   INTO TEMP tmp_unificador

      CALL fn_datos_arreglo()
      
   END IF
-- Consulta cuando en NSS hay Unificadores y Unificados
   IF v_cta_unificador > 0 AND v_cta_unificado > 0 THEN

      LET r_bandera = 1

      
      SELECT t.nss,
             t.id_derechohabiente,
             t.f_apertura,
             a.id_pre_unificador,
             a.estado
        FROM uni_pre_unificador a,
             tmp_nss t
       WHERE a.id_derechohabiente = t.id_derechohabiente
   INTO TEMP tmp_unificador

      SELECT b.id_pre_unificador
        FROM uni_pre_unificado b,
             tmp_nss t
       WHERE b.nss = t.nss
   INTO TEMP tmp_id_unificador
   
   
      SELECT a.nss_correcto
        FROM uni_pre_unificador a,
             tmp_id_unificador tu
       WHERE a.id_pre_unificador = tu.id_pre_unificador
   INTO TEMP tmp_nss_unificador

      DROP TABLE tmp_nss

      SELECT afi.nss,
             afi.id_derechohabiente,
             afi.f_apertura
        FROM afi_derechohabiente afi,
             tmp_nss_unificador uni
       WHERE afi.nss = uni.nss_correcto
   INTO TEMP tmp_nss

      INSERT INTO tmp_unificador SELECT t.nss,
                                        t.id_derechohabiente,
                                        t.f_apertura,
                                        a.id_pre_unificador,
                                        a.estado
                                   FROM uni_pre_unificador a,
                                        tmp_nss t
                                  WHERE a.id_derechohabiente = t.id_derechohabiente

      CALL fn_datos_arreglo()
   END IF
-- Consulta para cuando NSS son Unificados
   IF v_cta_unificador = 0 AND v_cta_unificado > 0 THEN
      LET r_bandera = 1
  
      SELECT b.id_pre_unificador
        FROM uni_pre_unificado b,
             tmp_nss t
       WHERE b.nss = t.nss
   INTO TEMP tmp_id_unificador

      SELECT a.nss_correcto
        FROM uni_pre_unificador a,
             tmp_id_unificador tu
       WHERE a.id_pre_unificador = tu.id_pre_unificador
   INTO TEMP tmp_nss_unificador

      DROP TABLE tmp_nss

      SELECT afi.nss,afi.id_derechohabiente,afi.f_apertura
        FROM afi_derechohabiente afi, tmp_nss_unificador uni
       WHERE afi.nss = uni.nss_correcto
   INTO TEMP tmp_nss

      SELECT t.nss,t.id_derechohabiente,t.f_apertura,a.id_pre_unificador,a.estado
        FROM uni_pre_unificador a,tmp_nss t
       WHERE a.id_derechohabiente = t.id_derechohabiente
   INTO TEMP tmp_unificador

      CALL fn_datos_arreglo()
   END IF

END FUNCTION

--***********************************************
--Función que llena tabla principal de consulta *
--***********************************************
FUNCTION fn_datos_arreglo()

   DEFINE z INTEGER

   DECLARE cur_unificador CURSOR FOR SELECT *
                                       FROM tmp_unificador 

   LET y = 1

   FOREACH cur_unificador INTO arr_tabla_tmp_unificador[y].*
      LET y = y + 1
   END FOREACH

   IF arr_tabla_tmp_unificador[arr_tabla_tmp_unificador.getLength()].nss IS NULL AND
      y > 1 THEN
      CALL arr_tabla_tmp_unificador.deleteElement(arr_tabla_tmp_unificador.getLength())
   END IF

   FOR y = 1 TO arr_tabla_tmp_unificador.getLength()

      LET c = arr_tabla_prospecto.getLength()
      LET c = c+1
      CALL arr_tabla_prospecto.appendElement()
      CASE

         WHEN arr_tabla_tmp_unificador[y].estado < 20
            LET arr_tabla_prospecto[c].nss = arr_tabla_tmp_unificador[y].nss
            LET arr_tabla_prospecto[c].tipo = "UNIFICADOR"
            LET arr_tabla_prospecto[c].marca = ""
            LET arr_tabla_prospecto[c].mov_imss = "ASOCIADO"
            LET arr_tabla_prospecto[c].f_movimiento = arr_tabla_tmp_unificador[y].f_apertura
            LET arr_tabla_prospecto[c].op_21_disp = ""
            LET arr_tabla_prospecto[c].op_21_confronta = ""
            LET arr_tabla_prospecto[c].op_22_conclucion = ""

         WHEN arr_tabla_tmp_unificador[y].estado = 20
            LET arr_tabla_prospecto[c].nss = arr_tabla_tmp_unificador[y].nss
            LET arr_tabla_prospecto[c].tipo = "UNIFICADOR"
            LET arr_tabla_prospecto[c].marca = "501"
            LET arr_tabla_prospecto[c].mov_imss = "ASOCIADO"
            LET arr_tabla_prospecto[c].f_movimiento = arr_tabla_tmp_unificador[y].f_apertura
            LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
            LET arr_tabla_prospecto[c].op_21_confronta = ""
            LET arr_tabla_prospecto[c].op_22_conclucion = ""

         WHEN arr_tabla_tmp_unificador[y].estado = 30
            LET arr_tabla_prospecto[c].nss = arr_tabla_tmp_unificador[y].nss
            LET arr_tabla_prospecto[c].tipo = "UNIFICADOR"
            LET arr_tabla_prospecto[c].marca = "501"
            LET arr_tabla_prospecto[c].mov_imss = "ASOCIADO"
            LET arr_tabla_prospecto[c].f_movimiento = arr_tabla_tmp_unificador[y].f_apertura
            LET arr_tabla_prospecto[c].op_21_disp = "NO PROCEDENTE"
            LET arr_tabla_prospecto[c].op_21_confronta = ""
            LET arr_tabla_prospecto[c].op_22_conclucion = ""

         WHEN arr_tabla_tmp_unificador[y].estado = 40
            LET arr_tabla_prospecto[c].nss = arr_tabla_tmp_unificador[y].nss
            LET arr_tabla_prospecto[c].tipo = "UNIFICADOR"
            LET arr_tabla_prospecto[c].marca = "501"
            LET arr_tabla_prospecto[c].mov_imss = "ASOCIADO"
            LET arr_tabla_prospecto[c].f_movimiento = arr_tabla_tmp_unificador[y].f_apertura
            LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
            LET arr_tabla_prospecto[c].op_21_confronta = "PROCEDENTE"
            LET arr_tabla_prospecto[c].op_22_conclucion = ""

         WHEN arr_tabla_tmp_unificador[y].estado = 50 
            LET arr_tabla_prospecto[c].nss = arr_tabla_tmp_unificador[y].nss
            LET arr_tabla_prospecto[c].tipo = "UNIFICADOR"
            LET arr_tabla_prospecto[c].marca = "501"
            LET arr_tabla_prospecto[c].mov_imss = "ASOCIADO"
            LET arr_tabla_prospecto[c].f_movimiento = arr_tabla_tmp_unificador[y].f_apertura
            LET arr_tabla_prospecto[c].op_21_disp = "NO PROCEDENTE"
            LET arr_tabla_prospecto[c].op_21_confronta = "NO PROCEDENTE"
            LET arr_tabla_prospecto[c].op_22_conclucion = ""

         WHEN arr_tabla_tmp_unificador[y].estado = 60
            LET arr_tabla_prospecto[c].nss = arr_tabla_tmp_unificador[y].nss
            LET arr_tabla_prospecto[c].tipo = "UNIFICADOR"
            LET arr_tabla_prospecto[c].marca = "501"
            LET arr_tabla_prospecto[c].mov_imss = "ASOCIADO"
            LET arr_tabla_prospecto[c].f_movimiento = arr_tabla_tmp_unificador[y].f_apertura
            LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
            LET arr_tabla_prospecto[c].op_21_confronta = "PENDIENTE"
            LET arr_tabla_prospecto[c].op_22_conclucion = ""
 
         WHEN arr_tabla_tmp_unificador[y].estado = 70
            LET arr_tabla_prospecto[c].nss = arr_tabla_tmp_unificador[y].nss
            LET arr_tabla_prospecto[c].tipo = "UNIFICADOR"
            LET arr_tabla_prospecto[c].marca = "501"
            LET arr_tabla_prospecto[c].mov_imss = "ASOCIADO"
            LET arr_tabla_prospecto[c].f_movimiento = arr_tabla_tmp_unificador[y].f_apertura
            LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
            LET arr_tabla_prospecto[c].op_21_confronta ="UNIFICADO"
            LET arr_tabla_prospecto[c].op_22_conclucion = ""

         WHEN arr_tabla_tmp_unificador[y].estado = 80
            LET arr_tabla_prospecto[c].nss = arr_tabla_tmp_unificador[y].nss
            LET arr_tabla_prospecto[c].tipo = "UNIFICADOR"
            LET arr_tabla_prospecto[c].marca = "501"
            LET arr_tabla_prospecto[c].mov_imss = "ASOCIADO"
            LET arr_tabla_prospecto[c].f_movimiento = arr_tabla_tmp_unificador[y].f_apertura
            LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
            LET arr_tabla_prospecto[c].op_21_confronta = "UNIFICADO"
            LET arr_tabla_prospecto[c].op_22_conclucion = "CONCUÍDO"

      END CASE

      LET v_id_pre_unificador = arr_tabla_tmp_unificador[y].id_pre_unificador

      LET v_query_unificado = "SELECT b.nss,
                                      b.estado,
                                      afi.f_apertura 
                                 FROM uni_pre_unificado b,
                                      afi_derechohabiente afi
                                WHERE b.id_pre_unificador = ","'",v_id_pre_unificador,"'","
                                  AND b.id_derechohabiente = afi.id_derechohabiente
                             ORDER BY estado DESC"

      PREPARE prp_unificado_p FROM v_query_unificado
      DECLARE cur_unificado_p CURSOR FOR prp_unificado_p

      LET z = 1

      FOREACH cur_unificado_p INTO arr_tabla[z].*
         LET z = z + 1
      END FOREACH

      IF arr_tabla[arr_tabla.getLength()].nss IS NULL AND
         z > 1 THEN
         CALL arr_tabla.deleteElement(arr_tabla.getLength())
      END IF

      FOR z = 1 TO arr_tabla.getLength()
         LET c = arr_tabla_prospecto.getLength()
         LET c = c+1
         CALL arr_tabla_prospecto.appendElement()

         CASE 

            WHEN arr_tabla[z].estado < 20
               LET arr_tabla_prospecto[c].op_21_disp = ""
               LET arr_tabla_prospecto[c].op_21_confronta =""
               LET arr_tabla_prospecto[c].op_22_conclucion =""
               LET arr_tabla_prospecto[c].tipo ="UNIFICADO"
               LET arr_tabla_prospecto[c].mov_imss ="ASOCIADO"
               LET arr_tabla_prospecto[c].marca = ""
               LET arr_tabla_prospecto[c].nss = arr_tabla[z].nss
               LET arr_tabla_prospecto[c].f_movimiento = arr_tabla[z].f_ap_unificado

            WHEN arr_tabla[z].estado = 20
               LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
               LET arr_tabla_prospecto[c].op_21_confronta =""
               LET arr_tabla_prospecto[c].op_22_conclucion =""
               LET arr_tabla_prospecto[c].tipo ="UNIFICADO"
               LET arr_tabla_prospecto[c].mov_imss ="ASOCIADO"
               LET arr_tabla_prospecto[c].marca = "502"
               LET arr_tabla_prospecto[c].nss = arr_tabla[z].nss
               LET arr_tabla_prospecto[c].f_movimiento = arr_tabla[z].f_ap_unificado

            WHEN arr_tabla[z].estado = 30
               LET arr_tabla_prospecto[c].op_21_disp = "NO PROCEDENTE"
               LET arr_tabla_prospecto[c].op_21_confronta =""
               LET arr_tabla_prospecto[c].op_22_conclucion =""
               LET arr_tabla_prospecto[c].tipo ="UNIFICADO"
               LET arr_tabla_prospecto[c].mov_imss ="ASOCIADO"
               LET arr_tabla_prospecto[c].marca = "502"
               LET arr_tabla_prospecto[c].nss = arr_tabla[z].nss
               LET arr_tabla_prospecto[c].f_movimiento = arr_tabla[z].f_ap_unificado

            WHEN arr_tabla[z].estado = 40
               LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
               LET arr_tabla_prospecto[c].op_21_confronta ="PROCEDENTE"
               LET arr_tabla_prospecto[c].op_22_conclucion =""
               LET arr_tabla_prospecto[c].tipo ="UNIFICADO"
               LET arr_tabla_prospecto[c].mov_imss ="ASOCIADO"
               LET arr_tabla_prospecto[c].marca = "502"
               LET arr_tabla_prospecto[c].nss = arr_tabla[z].nss
               LET arr_tabla_prospecto[c].f_movimiento = arr_tabla[z].f_ap_unificado

            WHEN arr_tabla[z].estado = 50
               LET arr_tabla_prospecto[c].op_21_disp = "NO PROCEDENTE"
               LET arr_tabla_prospecto[c].op_21_confronta ="NO PROCEDENTE"
               LET arr_tabla_prospecto[c].op_22_conclucion =""
               LET arr_tabla_prospecto[c].tipo ="UNIFICADO"
               LET arr_tabla_prospecto[c].mov_imss ="ASOCIADO"
               LET arr_tabla_prospecto[c].marca = "502"
               LET arr_tabla_prospecto[c].nss = arr_tabla[z].nss
               LET arr_tabla_prospecto[c].f_movimiento = arr_tabla[z].f_ap_unificado

            WHEN arr_tabla[z].estado = 60
               LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
               LET arr_tabla_prospecto[c].op_21_confronta ="PENDIENTE"
               LET arr_tabla_prospecto[c].op_22_conclucion =""
               LET arr_tabla_prospecto[c].tipo ="UNIFICADO"
               LET arr_tabla_prospecto[c].mov_imss ="ASOCIADO"
               LET arr_tabla_prospecto[c].marca = "502"
               LET arr_tabla_prospecto[c].nss = arr_tabla[z].nss
               LET arr_tabla_prospecto[c].f_movimiento = arr_tabla[z].f_ap_unificado

            WHEN arr_tabla[z].estado = 70
               LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
               LET arr_tabla_prospecto[c].op_21_confronta ="UNIFICADO"
               LET arr_tabla_prospecto[c].op_22_conclucion =""
               LET arr_tabla_prospecto[c].tipo ="UNIFICADO"
               LET arr_tabla_prospecto[c].mov_imss ="ASOCIADO"
               LET arr_tabla_prospecto[c].marca = "502"
               LET arr_tabla_prospecto[c].nss = arr_tabla[z].nss
               LET arr_tabla_prospecto[c].f_movimiento = arr_tabla[z].f_ap_unificado

            WHEN arr_tabla[z].estado = 80
               LET arr_tabla_prospecto[c].op_21_disp = "PROCEDENTE"
               LET arr_tabla_prospecto[c].op_21_confronta ="UNIFICADO"
               LET arr_tabla_prospecto[c].op_22_conclucion ="CONCLUÍDO"
               LET arr_tabla_prospecto[c].tipo ="UNIFICADO"
               LET arr_tabla_prospecto[c].mov_imss ="ASOCIADO"
               LET arr_tabla_prospecto[c].marca = "502"
               LET arr_tabla_prospecto[c].nss = arr_tabla[z].nss
               LET arr_tabla_prospecto[c].f_movimiento = arr_tabla[z].f_ap_unificado

         END CASE

      END FOR

      CALL arr_tabla.clear()
      LET c = arr_tabla_prospecto.getLength()
      LET c = c+1
      CALL arr_tabla_prospecto.insertElement(c)

   END FOR

   IF arr_tabla_prospecto[arr_tabla_prospecto.getLength()].nss IS NULL AND
      c > 1 THEN
      CALL arr_tabla_prospecto.deleteElement(arr_tabla_prospecto.getLength())
   END IF

END FUNCTION

--***************************************
--Función para borrar tablas temporales *
--***************************************
FUNCTION fn_tablas_temporales()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_nss
      DROP TABLE tmp_nss_arh
      DROP TABLE tmp_id_unificador
      DROP TABLE tmp_unificador
      DROP TABLE tmp_nss_unificador
   WHENEVER ERROR STOP
   CREATE TEMP TABLE tmp_nss_arh ( nss CHAR(11))

END FUNCTION