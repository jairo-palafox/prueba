######################################################################
#Modulo        => AFI                                                #
#Programa      => AFIC12                                             #
#Descripción   => Generación de reporte para actualización           #
#                 de datos maestros por WS                           #
#Autor         => Jose Eduardo Ventura Bonola                        #
#Fecha         =>27 DE AGOSTO DE  2015                               #
######################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_usuario                CHAR(20)      -- Obtiene dato de usuario
   DEFINE p_tipo_ejecucion         SMALLINT      -- Forma como ejecutará el programa
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE v_f_inicial              DATE          -- Fecha inicial de búsqueda para generación de reporte
   DEFINE v_f_final                DATE          -- Fecha final de búsqueda para generación de reporte
   DEFINE v_query                  STRING        -- Datos de consulta para reporte
   DEFINE v_dia                    DATE
   DEFINE v_ruta_reporte           STRING
   DEFINE v_mensaje                STRING
   DEFINE v_msj                    STRING
   DEFINE cta_curp                 INTEGER
   DEFINE cta_rfc                  INTEGER
   DEFINE cta_nombre               INTEGER
END GLOBALS

MAIN
    -- se recupera la clave de usuario desde parámetro
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET v_dia = TODAY

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".AFIC12.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW AFIC12 WITH FORM "AFIC121"

   INPUT BY NAME v_f_inicial,v_f_final ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      CALL fn_genera_reporte(v_f_inicial,v_f_final)
      LET v_mensaje = "Reporte generado correctamente \ lo puede revisar en :",v_ruta_reporte
      --CALL fn_mensaje("Mensaje","Reporte generado correctamente \ lo puede revisar en :","information")
       CALL fn_mensaje("Mensaje",v_mensaje,"information")
      
      EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT
      END INPUT
   CLOSE WINDOW AFIC12

END MAIN

FUNCTION fn_genera_reporte(v_f_inicial,v_f_final)

   DEFINE v_reporte           STRING
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_excepcion         SMALLINT
   DEFINE v_query_reporte     STRING
   DEFINE v_consulta_reporte  STRING
   DEFINE v_consulta_ultimo   STRING
   DEFINE v_f_inicial         DATE
   DEFINE v_f_final           DATE
   DEFINE a                   INTEGER
   DEFINE v_f_inicial1        DATETIME YEAR TO SECOND
   DEFINE v_f_final1          DATETIME YEAR TO SECOND
   DEFINE v_fecha             CHAR(20)
   DEFINE v_query_ultimo      STRING
   DEFINE i                   INTEGER

   DEFINE report_handler      om.SaxDocumentHandler

   DEFINE r_reporte          DYNAMIC ARRAY OF RECORD
          nss                CHAR(11),
          rfc                CHAR(13),
          curp               CHAR(18),
          ap_paterno_af      CHAR(40),
          ap_materno_af      CHAR(40),
          nombre_af          CHAR(40),
          f_modifica         DATETIME YEAR TO SECOND,
          ind_modifica_desc  CHAR(50)
   END RECORD

    DEFINE p_reporte         RECORD
          nss                CHAR(11),
          rfc                CHAR(13),
          curp               CHAR(18),
          ap_paterno_af      CHAR(40),
          ap_materno_af      CHAR(40),
          nombre_af          CHAR(40),
          f_modifica         DATETIME YEAR TO SECOND,
          ind_modifica_desc  CHAR(50)
   END RECORD

   DEFINE r_ultimo           DYNAMIC ARRAY OF RECORD
          id                 DECIMAL(9,0),
          fecha              DATETIME YEAR TO SECOND
   END RECORD

   DEFINE bnd                SMALLINT

   LET v_f_inicial1 = v_f_inicial 
   LET v_f_final1 = v_f_final
   LET v_fecha = TIME (CURRENT)
   DISPLAY "fecha y hora : ",v_fecha

   DISPLAY v_f_inicial1
   DISPLAY v_f_final1

   LET v_reporte = "AFIC12.4rp"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'afi'


   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        p_usuario CLIPPED , "-", -- usuario
                        "AFIC12", "-", -- programa
                        v_fecha[1,2],v_fecha[4,5],v_fecha[7,8],
                        ".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(1)
      CALL fgl_report_setOutputFileName(v_ruta_reporte) -- si se tiene selectPreview(1) no se almacena en la ruta lst
      DISPLAY v_ruta_reporte
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "[ SAFRE EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   IF NOT v_excepcion THEN

   LET v_query_ultimo = "
   select distinct id_derechohabiente,max(f_modifica)
     from afi_his_modifica_ws his
"

--si solo existe fecha final,se realiza carga de todos los datos anteriores o iguales a fecha final
       IF v_f_inicial IS NULL AND v_f_final IS NOT NULL THEN
          LET v_f_final1 = v_f_final1 + (1 UNITS DAY)
          LET v_consulta_ultimo = v_query_ultimo , " WHERE his.f_modifica <", "'",v_f_final1,"'"," GROUP BY id_derechohabiente"
    END IF

   --si solo existe fecha inicial,se genera carga de datos mayores o iguales a fecha inicial
       IF v_f_inicial IS NOT NULL AND  v_f_final IS NULL THEN
           LET v_consulta_ultimo = v_query_ultimo , " WHERE his.f_modifica >= ", "'",v_f_inicial1,"'"," GROUP BY id_derechohabiente"
    END IF

    --carga archivos entre fecha inical y final ingresadas
       IF v_f_inicial IS NOT NULL AND v_f_final IS NOT NULL THEN
          IF v_f_inicial = v_f_final THEN
             LET v_f_final1 = v_f_inicial1 + (1 UNITS DAY)
             LET v_consulta_ultimo = v_query_ultimo , " WHERE his.f_modifica >= '", v_f_inicial1, "' and his.f_modifica < '", v_f_final1,"'"," GROUP BY id_derechohabiente"
          ELSE
           LET v_consulta_ultimo = v_query_ultimo , " WHERE his.f_modifica BETWEEN '", v_f_inicial1, "' and '", v_f_final1,"'"," GROUP BY id_derechohabiente"
          END IF
       END IF

       IF v_f_inicial IS NULL AND v_f_final IS NULL THEN
          LET v_consulta_ultimo = v_query_ultimo," GROUP BY id_derechohabiente "
       END IF

       --DISPLAY v_query_ultimo
       --DISPLAY v_consulta_ultimo
       
    PREPARE prp_ultimo FROM v_consulta_ultimo
      DECLARE cur_ultimo CURSOR FOR  prp_ultimo

      LET a= 1
      FOREACH cur_ultimo INTO r_ultimo[a].*
      DISPLAY "id :",r_ultimo[a].id
      DISPLAY "fecha :",r_ultimo[a].fecha
      DISPLAY""
         LET a = a+1
      END FOREACH

      DISPLAY ""

      IF r_ultimo[r_ultimo.getLength()].id IS NULL THEN
         CALL r_ultimo.deleteElement(r_ultimo.getLength())
      END IF

      --LET a = 1
      LET i=1
      FOR a=1 TO r_ultimo.getLength()

      LET v_query_reporte = '
   SELECT afi.nss,
          his.rfc,
          his.curp,
          TRIM(his.ap_paterno_af),
          TRIM(his.ap_materno_af),
          TRIM(his.nombre_af),
          his.f_modifica,
          ind.ind_modifica_desc
    FROM afi_derechohabiente afi,
         afi_his_modifica_ws his,
         cat_afi_ind_modifica ind
   WHERE his.id_derechohabiente =',r_ultimo[a].id,'
     and his.f_modifica = "',r_ultimo[a].fecha,'"
     and his.id_derechohabiente =afi.id_derechohabiente
     AND his.ind_modifica = ind.ind_modifica'

     --DISPLAY "consulta : ",v_query_reporte
     
     PREPARE prp_reporte FROM v_query_reporte
      DECLARE cur_resultados CURSOR FOR  prp_reporte
      FOREACH cur_resultados INTO r_reporte[i].*
      DISPLAY "r_reporte :",r_reporte[i].ap_materno_af
      DISPLAY "r_reporte :",r_reporte[i].ap_paterno_af
      DISPLAY "r_reporte :",r_reporte[i].curp
      DISPLAY "r_reporte :",r_reporte[i].f_modifica
      DISPLAY "r_reporte :",r_reporte[i].ind_modifica_desc
      DISPLAY "r_reporte :",r_reporte[i].nombre_af
      DISPLAY "r_reporte :",r_reporte[i].nss
      DISPLAY "r_reporte :",r_reporte[i].rfc
         LET i= i+1
      END FOREACH

      IF r_reporte[r_reporte.getLength()].nss IS NULL THEN
         CALL r_reporte.deleteElement(r_reporte.getLength())
      END IF

      END FOR

       START REPORT rep_resultados TO XML HANDLER report_handler

      LET cta_curp   = 0
      LET cta_rfc    = 0
      LET cta_nombre = 0

        --DISPLAY "cant datos :",r_reporte.getLength()
        IF r_reporte.getLength() = 0 THEN
           LET v_msj = "No se encontraron registros para el intervalo de fecha ingresado"
           LET p_reporte.ap_materno_af =""
           LET p_reporte.ap_paterno_af=""
           LET p_reporte.curp=""
           LET p_reporte.f_modifica=""
           LET p_reporte.ind_modifica_desc=""
           LET p_reporte.nombre_af=""
           LET p_reporte.nss=""
           LET p_reporte.rfc=""
            OUTPUT TO REPORT rep_resultados(p_reporte.*)
        ELSE
           LET v_msj =""
      FOR i = 1 TO r_reporte.getLength()
         CASE
            WHEN 
               r_reporte[i].ind_modifica_desc CLIPPED = "CAMBIO CURP"
               LET cta_curp = cta_curp + 1
            WHEN
               r_reporte[i].ind_modifica_desc CLIPPED = "CAMBIO RFC"
               LET cta_rfc = cta_rfc + 1
            WHEN
               r_reporte[i].ind_modifica_desc CLIPPED = "CAMBIO NOMBRE AFORE"
               LET cta_nombre = cta_nombre + 1
         END CASE

         INITIALIZE p_reporte.* TO NULL
         LET p_reporte.* = r_reporte[i].*
            OUTPUT TO REPORT rep_resultados(p_reporte.*)
            DISPLAY "datos :",p_reporte.*
      END FOR
      END IF

      FINISH REPORT rep_resultados

    END IF
END FUNCTION

REPORT rep_resultados(p_reporte)
   

   DEFINE p_reporte          RECORD
          nss                CHAR(11),
          rfc                CHAR(13),
          curp               CHAR(18),
          ap_paterno_af      CHAR(40),
          ap_materno_af      CHAR(40),
          nombre_af          CHAR(40),
          f_modifica         DATETIME YEAR TO SECOND,
          ind_modifica_desc  CHAR(50)
   END RECORD

   DEFINE v_cad_nom          STRING
   DEFINE v_f_mod            CHAR(20)
   DEFINE v_f_mod1           STRING
   DEFINE v_fecha_reporte    DATE

   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_reporte = TODAY

         PRINTX v_fecha_reporte USING "DD-MM-YYYY"
         PRINTX p_usuario
         PRINTX v_msj

      ON EVERY ROW
         LET v_f_mod = p_reporte.f_modifica
         LET v_f_mod1 = v_f_mod[9,10],"-",v_f_mod[6,7],"-",v_f_mod[1,4]," ",v_f_mod[12,19]
         LET v_cad_nom = p_reporte.ap_paterno_af CLIPPED," ",p_reporte.ap_materno_af CLIPPED," ",p_reporte.nombre_af CLIPPED
         PRINTX v_cad_nom
         PRINTX v_f_mod1
         PRINTX p_reporte.*

      ON LAST ROW
         PRINTX cta_curp
         PRINTX cta_rfc
         PRINTX cta_nombre

END REPORT