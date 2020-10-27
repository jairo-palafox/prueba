###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIX03                                                  #
#Objetivo          => CARGA INICIAL SALDO72                                   #
#Fecha Inicio      => 29/03/2012                                              #
###############################################################################
IMPORT os

SCHEMA safre_viv

DEFINE g_archivo             STRING
DEFINE g_layout              SMALLINT
DEFINE g_registro            CHAR(2)
DEFINE g_usuario             CHAR(20)
DEFINE g_archivo_ruta        STRING
DEFINE g_archivo_nombre      STRING
DEFINE g_tabla               STRING

MAIN

   LET g_archivo  = ARG_VAL(1)
   LET g_layout   = 58
   LET g_registro = '00'

   CALL STARTLOG ("AFIX03.log")

   IF g_archivo IS NULL THEN
      DISPLAY "EJECUTAR EL PROGRAMA: fglrun ASIX03 nombre_archivo"
   ELSE
      CALL fn_crea_tabla()
      CALL fn_carga_bdnsviv()
   END IF
END MAIN

FUNCTION fn_crea_tabla()
   DEFINE v_sql_consulta      STRING
   DEFINE v_sql_crea_tablas   STRING
   DEFINE v_sql_campos        STRING
   DEFINE v_reg_layout        RECORD LIKE cat_layout.*
   DEFINE v_reg_campos        RECORD LIKE cat_campo.*
   DEFINE v_bnd_cursor        SMALLINT

   DATABASE safre_tmp
   LET v_sql_consulta = "\n SELECT * ",
                        "\n   FROM safre_viv:cat_layout",
                        "\n  WHERE layout_cod = ?"
   PREPARE prp_recupera_tablas FROM v_sql_consulta 
   DECLARE cur_recupera_tablas CURSOR FOR prp_recupera_tablas

   LET v_sql_consulta = "\n SELECT * ",
                        "\n   FROM safre_viv:cat_campo ",
                        "\n  WHERE layout_cod   = ?",
                        "\n    AND registro = ? ",
                        "\n  ORDER BY campo_cod "
   PREPARE prp_recupera_campos FROM v_sql_consulta
   DECLARE cur_recupera_campos CURSOR FOR prp_recupera_campos

   FOREACH cur_recupera_tablas USING g_layout INTO v_reg_layout.*
      LET g_tabla = v_reg_layout.tabla
      LET v_sql_crea_tablas = "CREATE TABLE "||v_reg_layout.tabla|| "( "
      LET v_bnd_cursor = FALSE
      FOREACH cur_recupera_campos USING v_reg_layout.layout_cod, v_reg_layout.registro
                                   INTO v_reg_campos.*
         CASE v_reg_campos.tipo_dato
            WHEN "X"
               LET v_sql_campos = v_reg_campos.campo_desc CLIPPED||
                                 " CHAR ("||v_reg_campos.longitud CLIPPED||")"
            WHEN "F" 
               LET v_sql_campos = v_reg_campos.campo_desc CLIPPED||
                                 " DATE "
            WHEN "#"
               LET v_sql_campos = v_reg_campos.campo_desc CLIPPED ||
                                  " DECIMAL ("||v_reg_campos.longitud||
                                  ","|| v_reg_campos.precision ||")"
         END CASE
         DISPLAY  v_reg_campos.num_valida = 0
         IF NOT(v_bnd_cursor)THEN
            IF v_reg_campos.num_valida = 0 THEN
               LET v_sql_crea_tablas = v_sql_crea_tablas || v_sql_campos
               LET v_bnd_cursor = TRUE
            END IF
         ELSE
            IF v_reg_campos.num_valida = 0 THEN
               LET v_sql_crea_tablas = v_sql_crea_tablas || " , " || v_sql_campos
            END IF
         END IF
      END FOREACH
      LET v_sql_crea_tablas = v_sql_crea_tablas || 
                               " ) fragment by round robin in bdnsv_1_dbs , ",
                               "bdnsv_2_dbs , bdnsv_3_dbs , bdnsv_4_dbs; "
        
      WHENEVER ERROR CONTINUE
         LET v_sql_consulta = "DROP TABLE "|| v_reg_layout.tabla
         PREPARE prp_drop FROM v_sql_consulta
         EXECUTE prp_drop
      WHENEVER ERROR STOP

      DISPLAY v_sql_crea_tablas
      PREPARE prp_crea_tabla FROM v_sql_crea_tablas
      EXECUTE prp_crea_tabla
   END FOREACH
END FUNCTION

FUNCTION fn_carga_bdnsviv()

   IF fn_valida_archivo() THEN
      DISPLAY "VALIDACIÓN ARCHIVO EXITOSA"
      CALL fn_genera_hpl()
   END IF
END FUNCTION

FUNCTION fn_valida_archivo()
   DEFINE v_bandera        SMALLINT

   LET v_bandera = TRUE

   IF os.Path.exists(g_archivo) THEN
      IF os.Path.readable(g_archivo) THEN
         LET g_archivo_ruta   = os.Path.dirname(g_archivo)
         LET g_archivo_nombre = os.Path.basename(g_archivo)
      ELSE
         LET v_bandera = FALSE
         DISPLAY "EL ARCHIVO NO TIENE PERMISOS DE LECTURA"
      END IF
   ELSE
      LET v_bandera = FALSE
      DISPLAY "EL ARCHIVO NO EXISTE"
   END IF

   RETURN v_bandera
END FUNCTION

FUNCTION fn_genera_hpl()
DEFINE v_modulo_cod     CHAR(3),
       v_archivo_format STRING,
       v_archivo_map    STRING,
       v_tipo_dato      STRING,
       v_comando        STRING,
       v_reg_campos     RECORD LIKE cat_campo.*,
       v_ch_format      base.Channel,
       v_ch_map         base.Channel,
       v_sql_consulta   STRING,
       v_bytes          SMALLINT,
       v_offset         SMALLINT

   SELECT a.modulo_cod, USER
     INTO v_modulo_cod, g_usuario
     FROM safre_viv:cat_layout a
    WHERE a.layout_cod = g_layout
      AND a.registro   = g_registro

   CALL fn_genera_device()

###GENERA ARCHIVO FORMAT 
   LET v_archivo_format = g_archivo_ruta CLIPPED||"/"||g_usuario CLIPPED||
                          "."||g_archivo_nombre CLIPPED||".format"
   
   DISPLAY v_archivo_format
   LET v_ch_format = base.Channel.create()
   CALL v_ch_format.openFile(v_archivo_format,"w")
   CALL v_ch_format.writeLine('BEGIN OBJECT FIXEDFORMAT '||g_archivo_nombre CLIPPED||".format \n")
   CALL v_ch_format.writeLine('PROJECT      '|| v_modulo_cod)
   CALL v_ch_format.writeLine('CHARACTERSET  ASCII ')
   CALL v_ch_format.writeLine('MACHINE       Intel \n')
   
###GENERA ARCHIVO MAP 
   LET v_archivo_map = g_archivo_ruta CLIPPED||"/"||g_usuario CLIPPED||"."||
                       g_archivo_nombre CLIPPED||".map"
   LET v_ch_map = base.Channel.create()
   CALL v_ch_map .openFile(v_archivo_map,"w")
   CALL v_ch_map .writeLine('BEGIN OBJECT LOADMAP '||g_archivo_nombre CLIPPED||".map \n")
   CALL v_ch_map .writeLine('PROJECT      '|| v_modulo_cod)
   CALL v_ch_map .writeLine('FORMAT       '|| g_archivo_nombre CLIPPED||'.format ')
   CALL v_ch_map .writeLine('DATABASE     safre_tmp')
   CALL v_ch_map .writeLine('TABLE        '|| g_tabla||' \n')


   LET v_sql_consulta = "\n SELECT *",
                        "\n   FROM safre_viv:cat_campo",
                        "\n  WHERE layout_cod = ?",
                        "\n    AND registro = ?",
                        "\n  ORDER BY campo_cod"
                        
   PREPARE prp_recupera_campos_hpl FROM v_sql_consulta                        
   DECLARE cur_hpl CURSOR FOR prp_recupera_campos_hpl
   
   FOREACH cur_hpl USING g_layout, g_registro INTO v_reg_campos.*
      CASE v_reg_campos.tipo_dato
         WHEN "F" 
            LET v_tipo_dato = "Date"
         WHEN "#"
            LET v_tipo_dato = "Float"
         OTHERWISE
            LET v_tipo_dato = "Chars"
      END CASE
      LET v_offset = v_reg_campos.pos_inicial - 1
      LET v_bytes  = v_reg_campos.pos_final - v_offset 

      ###LLENA SECUENCIA FORMAT
      CALL v_ch_format.writeLine('BEGIN SEQUENCE \n')
      CALL v_ch_format.writeLine('FIELDNAME   '||v_reg_campos.campo_desc CLIPPED)
      CALL v_ch_format.writeLine('DATATYPE    '||v_tipo_dato)
      CALL v_ch_format.writeLine('BYTES       '||v_bytes)
      CALL v_ch_format.writeLine('DECIMALS    '||v_reg_campos.precision)
      CALL v_ch_format.writeLine('OFFSET      '||v_offset)
      CALL v_ch_format.writeLine('END SEQUENCE \n')

      IF v_reg_campos.num_valida = 0 THEN
         ###LLENA SECUENCIA MAP
         CALL v_ch_map.writeLine('BEGIN  SEQUENCE ')
         CALL v_ch_map.writeLine('COLUMNNAME     '||v_reg_campos.campo_desc CLIPPED)
         CALL v_ch_map.writeLine('FIELDNAME      '||v_reg_campos.campo_desc CLIPPED)
         CALL v_ch_map.writeLine('JUSTIFICATION  ')
         CALL v_ch_map.writeLine('CASECONVERT    ') 
         CALL v_ch_map.writeLine('DEFAULTVALUE   ')
         CALL v_ch_map.writeLine('TRANSFERBYTES  ')
         CALL v_ch_map.writeLine('COLUMNOFFSET   ')
         CALL v_ch_map.writeLine('FIELDOFFSET    ')
         CALL v_ch_map.writeLine('FIELDMINIMUM   ')
         CALL v_ch_map.writeLine('FIELDMAXIMUM   ')
         CALL v_ch_map.writeLine('FILLCHARACTER  ')
         CALL v_ch_map.writeLine('PICTURE        ')
         CALL v_ch_map.writeLine('FUNCTION       ')
         CALL v_ch_map.writeLine('STORAGECODING  ')
         CALL v_ch_map.writeLine('BLOBCOLUMN     ')
         CALL v_ch_map.writeLine('END  SEQUENCE \n')
      END IF
   END FOREACH

   CALL v_ch_format.writeLine('BEGIN SEQUENCE      ')
   CALL v_ch_format.writeLine('FIELDNAME   dummyCR ')
   CALL v_ch_format.writeLine('DATATYPE    Chars   ')
   CALL v_ch_format.writeLine('BYTES       1       ')
   CALL v_ch_format.writeLine('DECIMALS            ')
   CALL v_ch_format.writeLine('OFFSET      '||v_reg_campos.pos_final)
   CALL v_ch_format.writeLine('END SEQUENCE \n')
   CALL v_ch_format.writeLine('END OBJECT \n')

   
   CALL v_ch_map.writeLine   ('END OBJECT \n')

   CALL fn_crea_project(v_modulo_cod)
   DISPLAY "CREACIÓN DEL PROYECTO " ,v_modulo_cod, " DE HPL EXITOSO"
   
   CALL fn_crea_objeto(v_archivo_format, g_archivo_nombre CLIPPED||".format", "format", v_modulo_cod)
   DISPLAY "CREACIÓN DEL OBJETO FORMAT " ,g_archivo_nombre CLIPPED||".format DE HPL EXITOSO"

   LET v_comando = "onpladm delete map ",g_archivo_nombre CLIPPED||".map -fl -p ",
                   v_modulo_cod
   RUN v_comando

   CALL fn_crea_objeto(v_archivo_map, g_archivo_nombre CLIPPED||".map", "map", v_modulo_cod )
   DISPLAY "CREACIÓN DEL OBJETO MAP " ,g_archivo_nombre CLIPPED||".map DE HPL EXITOSO"

   CALL fn_genera_job(g_archivo_ruta,g_archivo_nombre,g_usuario,v_modulo_cod)

   CALL fn_ejecuta_hpl(v_modulo_cod)
END FUNCTION

FUNCTION fn_crea_project(p_proyecto)
   DEFINE p_proyecto   STRING
   DEFINE v_comando    STRING
   DEFINE r_ejecucion  SMALLINT

   LET v_comando = "onpladm describe project ", p_proyecto
   RUN v_comando RETURNING r_ejecucion

   IF r_ejecucion <> 0 THEN
      LET v_comando = "onpladm create project ",p_proyecto
      RUN v_comando
   END IF
   
END FUNCTION

FUNCTION fn_genera_job(g_archivo_ruta,g_archivo_nombre,g_usuario, p_modulo_cod)

DEFINE g_archivo_ruta   STRING,--LIKE seg_modulo.ruta_rescate,
       g_archivo_nombre LIKE cat_layout.archivo,
       g_usuario        LIKE seg_modulo.usuario,
       p_modulo_cod     CHAR(3),
       v_str_archivo    STRING,
       v_canal          base.Channel

   LET v_str_archivo = g_archivo_ruta CLIPPED||"/"||g_usuario CLIPPED||"."||
                       g_archivo_nombre CLIPPED||".job"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   CALL v_canal.writeLine('BEGIN OBJECT LOADJOB '||g_archivo_nombre CLIPPED||".job")
   CALL v_canal.writeLine('PROJECT           '||p_modulo_cod CLIPPED)
   CALL v_canal.writeLine('DEVICE            '||g_archivo_nombre CLIPPED||'.device ')
   CALL v_canal.writeLine('MAP               '||g_archivo_nombre CLIPPED||'.map ')
   CALL v_canal.writeLine('FILTER            ')
   CALL v_canal.writeLine('SERVER            ') 
   CALL v_canal.writeLine('DATABASE          safre_tmp ')
   CALL v_canal.writeLine('FLTFILE           '||
                           g_archivo_ruta CLIPPED||'/'||g_usuario CLIPPED||'.'||
                           g_archivo_nombre CLIPPED||'.flt ')
   CALL v_canal.writeLine('REJECTFILE        '||
                           g_archivo_ruta CLIPPED||'/'||g_usuario CLIPPED||'.'||
                           g_archivo_nombre CLIPPED||'.rjt ')
   CALL v_canal.writeLine('LOGFILE           '||
                           g_archivo_ruta CLIPPED||'/'||g_usuario CLIPPED||'.'||
                           g_archivo_nombre CLIPPED||'.log ')
   CALL v_canal.writeLine('RUNMODE           D ')
   CALL v_canal.writeLine('GENERATEVIORECS   Y ')
   CALL v_canal.writeLine('TAPES             0 ')
   CALL v_canal.writeLine('NUMRECORDS        0 ')
   CALL v_canal.writeLine('STARTRECORD       0 ')
   CALL v_canal.writeLine('MAXERRORS         1000 ')
   CALL v_canal.writeLine('END  OBJECT      \n')

   CALL v_canal.close()

   CALL fn_crea_objeto(v_str_archivo, g_archivo_nombre CLIPPED||".job", "job", p_modulo_cod )
   DISPLAY "CREACIÓN DEL JOB " ,g_archivo_nombre CLIPPED||".job DE HPL EXITOSO"
END FUNCTION

FUNCTION fn_genera_device()
   DEFINE v_str_archivo    STRING
   DEFINE v_canal          base.Channel

   LET v_str_archivo = g_archivo_ruta CLIPPED,"/",g_usuario CLIPPED,".",g_archivo_nombre CLIPPED,".device"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   CALL v_canal.writeLine('BEGIN OBJECT DEVICEARRAY '||g_archivo_nombre CLIPPED||".device \n")
   CALL v_canal.writeLine('BEGIN SEQUENCE ')
   CALL v_canal.writeLine('TYPE           FILE ')
   CALL v_canal.writeLine('FILE          '||g_archivo CLIPPED)
   CALL v_canal.writeLine('TAPEBLOCKSIZE  0 ')
   CALL v_canal.writeLine('TAPEDEVICESIZE 0 ')
   CALL v_canal.writeLine('PIPECOMMAND      ')
   CALL v_canal.writeLine('END  SEQUENCE    \n')
   CALL v_canal.writeLine('END  OBJECT      ')

   CALL v_canal.close()

   CALL fn_crea_objeto(v_str_archivo, g_archivo_nombre CLIPPED||".device", "device", "")

   DISPLAY "CREACIÓN DEL DEVICE ",g_archivo_nombre CLIPPED||".device DE HPL EXITOSO"

END FUNCTION

FUNCTION fn_crea_objeto(p_str_archivo, p_objeto, p_tipo, p_proyecto)
   DEFINE p_tipo            STRING
   DEFINE p_objeto          STRING
   DEFINE p_str_archivo     STRING
   DEFINE p_proyecto        STRING
   DEFINE v_comando         STRING
   DEFINE r_ejecucion       SMALLINT

   #Se ejecuta primero el create y despues el modify, para no verificar si ya
   #existe el objeto

   CASE p_tipo
      WHEN "device"
         LET v_comando = "onpladm describe device ", p_objeto
      WHEN "job"
         LET v_comando = "onpladm describe job ", p_objeto,
                         " -fl -p ", p_proyecto
      WHEN "format"
         LET v_comando = "onpladm describe format ", p_objeto,
                         " -p ",p_proyecto
     WHEN "map"
         LET v_comando = "onpladm describe map ", p_objeto,
                         " -fl -p ",p_proyecto 
   END CASE

   RUN v_comando RETURNING r_ejecucion

   IF r_ejecucion <> 0 THEN
      LET v_comando = "onpladm create object -F ",
                      p_str_archivo
   ELSE
      LET v_comando = "onpladm modify object -F ",
                      p_str_archivo
   END IF
   
   RUN v_comando
END FUNCTION

FUNCTION fn_ejecuta_hpl(p_modulo_cod)
DEFINE p_modulo_cod     CHAR(3),
       v_comando        STRING,
       v_pos_final      SMALLINT,
       v_long_archivo   SMALLINT,
       v_canal          base.Channel

   SELECT MAX(pos_final)
     INTO v_pos_final
     FROM safre_viv:cat_campo
    WHERE layout_cod = g_layout
      AND registro   = g_registro 

   LET v_comando = "wc -L ",g_archivo ,"|  awk '{print $1}' ", 
                   " > ", g_archivo.trim(),".long"
   DISPLAY v_comando
   RUN v_comando

   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(g_archivo.trim()||".long","r")
   LET v_long_archivo = v_canal.readLine()
   CALL v_canal.close()

   DISPLAY v_pos_final
   DISPLAY v_long_archivo
   IF v_pos_final <> v_long_archivo THEN
      DISPLAY "LA LONGITUD DEL ARCHIVO ES DIFERENTE RESPECTO AL LAYOUT. CARGA CANCELADA"
   ELSE
      LET v_comando = "nohup fglrun AFIX04 "|| g_usuario|| " "||
                      p_modulo_cod, " "||
                      g_archivo_nombre ||" 1>AFIX04.sal 2>AFIX04.log &"
      DISPLAY ""
      DISPLAY "SE EJECUTA POR NOHUP LA CARGA DEL ARCHIVO."
      DISPLAY "VERFICAR LOS ARCHIVOS DE SALIDA AFIX04.sal y AFIX04.log"
      DISPLAY v_comando
      DISPLAY g_archivo_nombre
      RUN v_comando
   END IF
END FUNCTION
