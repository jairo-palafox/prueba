IMPORT os
GLOBALS
DEFINE Respuesta DYNAMIC ARRAY OF RECORD
     modificador STRING
     ,tabla STRING
     ,nombre STRING
     ,lineaNumero STRING
     ,ArchivoRuta STRING
     END RECORD
   DEFINE ind INTEGER
   DEFINE ch_out base.Channel

   DEFINE hayFrom BOOLEAN
   DEFINE haySelect BOOLEAN
   DEFINE hayComentario BOOLEAN

   DEFINE linea STRING

END GLOBALS
MAIN
   LET ch_out = base.Channel.create()
   CALL ch_out.setDelimiter("|")
   --archivo en el que se guarda la respuesta
   CALL ch_out.openFile("/ds/safreviv/ret/fte/fileout.unl","w")
   DISPLAY "**inicia**"

   
   -- directorio que se analiza
   CALL showDir("/ds/safreviv/ret/fte")

   
   DISPLAY "**termina**"
   CALL ch_out.close()
END MAIN

FUNCTION showDir(path)
   DEFINE path STRING
   DEFINE child STRING
   DEFINE h INTEGER
   DEFINE archivo STRING
   DEFINE compara STRING
   IF NOT os.Path.exists(path) THEN
      RETURN
   END IF
   IF NOT os.Path.isdirectory(path) THEN
      LET archivo = os.Path.basename(path)
      LET compara = archivo.subString( archivo.getLength()-3, archivo.getLength() )
      IF compara = ".4gl" THEN
         --DISPLAY " ", os.Path.basename(path)
         CALL ArchivoLeer(path)
      END IF
      RETURN
   END IF
   DISPLAY "[", path, "]"
   CALL os.Path.dirsort("name", 1)
   LET h = os.Path.diropen(path)
   WHILE h > 0
      LET child = os.Path.dirnext(h)
      IF child IS NULL THEN EXIT WHILE END IF
      IF child == "." OR child == ".." THEN CONTINUE WHILE END IF
      
      
      
      CALL showDir( os.Path.join( path, child ) )
   END WHILE
   CALL os.Path.dirclose(h)
END FUNCTION

FUNCTION  ArchivoLeer(Archivo)
   DEFINE archivo STRING
   DEFINE ch base.Channel
   DEFINE posI INTEGER
   DEFINE posF INTEGER
   DEFINE i INTEGER
   DEFINE lineaNum integer
   
   LET ch = base.Channel.create()

   CALL Respuesta.CLEAR()
   LET ind = 1
   LET hayFrom = FALSE
   LET haySelect = FALSE
   LET hayComentario = FALSE

   IF NOT os.Path.readable(Archivo) THEN
      DISPLAY "No se puede leer: ",Archivo
      RETURN
   END IF

   CALL ch.openFile(Archivo,"r")

   LET lineaNum = 0
   WHILE TRUE
      LET linea = ch.readLine()
      LET linea = linea.toLowerCase()
      LET linea = linea.trim()
      LET lineaNum = lineaNum + 1
      IF ch.isEof() THEN EXIT WHILE END IF

      --no se consideran comentarios
      IF hayComentario THEN
         -- cuan es llave que cierra, el resto de la linea es valido
         LET posI = linea.getIndexOf("}",1)
         IF posI > 0 THEN
            LET linea = linea.subString(posI+1,linea.getLength())
            LET linea = linea.trim()
            LET hayComentario = FALSE
         ELSE
            CONTINUE WHILE
         END IF
      END IF

      -- lo anterior a los caracteres -- # { no es comentario
      LET posF = linea.getIndexOf("--",1)
      IF posF > 0 THEN
         LET linea = linea.subString(1,posF-1)
      END IF
      LET posF = linea.getIndexOf("#",1)
      IF posF > 0 THEN
         LET linea = linea.subString(1,posF-1)
      END IF
      LET posF = linea.getIndexOf("{",1)
      IF posF > 0 THEN
         LET linea = linea.subString(1,posF-1)
         LET hayComentario = true
      END IF
      LET linea = linea.trim()
      IF linea.getLength() <= 0 THEN
         CONTINUE WHILE
      END IF

      --reinician banderas de select
      CALL ReIniSelectPor("where")
      CALL ReIniSelectPor("order")
      CALL ReIniSelectPor("group")
      CALL ReIniSelectPor("input array")
      CALL ReIniSelectPor("prepare")
      CALL ReIniSelectPor("let")
      CALL ReIniSelectPor("call")
      CALL ReIniSelectPor("foreach")
      CALL ReIniSelectPor("when")
      CALL ReIniSelectPor("otherwise")
      CALL ReIniSelectPor("end")

      
      --se analiza cuando hay insert into
      LET posI = linea.getIndexOf("insert into",1)
      IF posI > 0 THEN
         LET posI = sigNoEspacio(linea,posI+12)
         LET posF = linea.getIndexOf(" ",posI)-1
         IF posF =-1 THEN
            LET posF = linea.getIndexOf("(",posI)-1
            IF posF =-1 THEN
               LET posF = linea.getLength()
            END IF 
         END IF
         CALL registraResp("insert",os.Path.basename(Archivo),linea.subString(posI,posF),lineaNum,os.Path.dirname(Archivo))
      END IF

      --se analiza cuando hay update
      LET posI = linea.getIndexOf("update",1)
      IF posI > 0 THEN
         LET posI = sigNoEspacio(linea,posI+7)
         LET posF = linea.getIndexOf(" ",posI)-1
         IF posF =-1 THEN
            LET posF = linea.getIndexOf("\"",posI)-1
            IF posF =-1 THEN
               LET posF = linea.getLength()
            END IF 
         END IF
         CALL registraResp("update",os.Path.basename(Archivo),linea.subString(posI,posF),lineaNum,os.Path.dirname(Archivo))
      END IF

      IF hayFrom THEN
         -- lineas subsecuentes del form
         LET linea = TextoBorrar(linea, ",\"\\n")
         LET linea = TextoBorrar(linea, "\"\\n")

         LET posI = sigNoEspacio(linea,1)
         LET posF = sigEspacio(linea,posI+1)
         IF posF == 0 THEN
            LET posF = linea.getLength()
         END IF
         CALL registraResp("select",os.Path.basename(Archivo),linea.subString(posI,posF),lineaNum,os.Path.dirname(Archivo))
      END IF

      IF linea.subString(1,7) == "select "  THEN
         LET haySelect = TRUE
      END IF
      LET posI = linea.getIndexOf(" select",1)
      IF posI > 0 THEN
         LET haySelect = TRUE
      END IF
      LET posI = linea.getIndexOf("\"select",1)
      IF posI > 0 THEN
         LET haySelect = TRUE
      END IF
      LET posI = linea.getIndexOf("\nselect",1)
      IF posI > 0 THEN
         LET haySelect = TRUE
      END IF
      
      --se analiza la primer linea del from
      LET posI = linea.getIndexOf("from",1)
      IF posI > 0 AND (haySelect OR hayFrom) THEN
         LET haySelect = FALSE
         LET hayFrom = TRUE
         LET posI = sigNoEspacio(linea,posI+5)
         LET posF = linea.getIndexOf(" ",posI)-1
         IF posF =-1 THEN
            LET posF = linea.getIndexOf("\"",posI)-1
            IF posF =-1 THEN
               LET posF = linea.getLength()
            END IF 
         END IF
         IF linea.subString(posI,posF) = "" OR linea.subString(posI,posF) IS NULL THEN
            LET posF = 1
         END IF 
         CALL registraResp("select",os.Path.basename(Archivo),linea.subString(posI,posF),lineaNum,os.Path.dirname(Archivo))
      END IF

      -- se analizan lineas con join
      LET posI = linea.getIndexOf("join",1)
      IF posI > 0 AND (haySelect OR hayFrom) THEN
         LET haySelect = FALSE
         LET hayFrom = TRUE
         LET posI = sigNoEspacio(linea,posI+5)
         LET posF = linea.getIndexOf(" ",posI)-1
         IF posF =-1 THEN
            LET posF = linea.getIndexOf("\"",posI)-1
            IF posF =-1 THEN
               LET posF = linea.getLength()
            END IF 
         END IF
         IF linea.subString(posI,posF) = "" OR linea.subString(posI,posF) IS NULL THEN
            LET posF = 1
         END IF 
         CALL registraResp("select",os.Path.basename(Archivo),linea.subString(posI,posF),lineaNum,os.Path.dirname(Archivo))
      END IF
      
   END WHILE
   CALL ch.close()

   -- se pasa el resultado del analisis al archivo
   FOR i = 1 TO Respuesta.getLength()
      CALL ch_out.write([Respuesta[i].modificador,Respuesta[i].nombre,Respuesta[i].ArchivoRuta,Respuesta[i].tabla,Respuesta[i].lineaNumero])
   END FOR
END FUNCTION


FUNCTION sigNoEspacio(Entra,Inicio)
DEFINE Entra STRING
DEFINE Inicio INTEGER
DEFINE i INTEGER
   FOR i = inicio TO Entra.getLength()
      IF Entra.getCharAt(i) <> " " THEN
         RETURN i
      END IF
   END FOR 
   RETURN 0

END FUNCTION

FUNCTION sigEspacio(Entra,Inicio)
DEFINE Entra STRING
DEFINE Inicio INTEGER
DEFINE i INTEGER
   FOR i = inicio TO Entra.getLength()
      IF Entra.getCharAt(i) = " " THEN
         RETURN i
      END IF
   END FOR
   RETURN 0

END FUNCTION

FUNCTION registraResp(Modi,ArchivoNombre,Nom,Num,ArchiRuta)
DEFINE i INTEGER
DEFINE Modi,ArchivoNombre,Nom,Num,ArchiRuta STRING

   LET Nom = TextoBorrar(Nom, ",")
   LET Nom = TextoBorrar(Nom, "|")
   LET Nom = TextoBorrar(Nom, "\"")
   LET Nom = TextoBorrar(Nom, "\\n")
   LET Nom = TextoBorrar(Nom, "(")
   LET Nom = TextoBorrar(Nom, ")")
   LET Nom = TextoBorrar(Nom, " ")

   IF "inner" == Nom THEN
      RETURN
   END IF
   IF "and" == Nom THEN
      RETURN
   END IF
   IF "on" == Nom THEN
      RETURN
   END IF
   IF "or" == Nom THEN
      RETURN
   END IF
   IF "select" == Nom THEN
      RETURN
   END IF
   IF "from" == Nom THEN
      RETURN
   END IF
   IF "into" == Nom THEN
      RETURN
   END IF
   IF "left" == Nom THEN
      RETURN
   END IF

   FOR i = 1 TO Respuesta.getLength()
      IF Respuesta[i].modificador = Modi AND
         Respuesta[i].tabla = ArchivoNombre AND
         Respuesta[i].nombre = Nom THEN
         RETURN
      END IF
   END FOR
   LET Respuesta[ind].modificador = Modi
   LET Respuesta[ind].tabla = ArchivoNombre
   LET Respuesta[ind].nombre = Nom
   LET Respuesta[ind].lineaNumero = Num
   LET Respuesta[ind].ArchivoRuta = ArchiRuta
   LET ind = ind + 1

END FUNCTION

FUNCTION ReIniSelectPor(Referencia)
DEFINE referencia STRING
DEFINE posI integer
   LET posI = linea.getIndexOf(Referencia,1)
   IF posI > 0 THEN
      LET haySelect = FALSE
      LET hayFrom = FALSE
   END IF
END FUNCTION

FUNCTION TextoBorrar(Entra, Elimina)
DEFINE Entra STRING
DEFINE Elimina STRING
DEFINE posI INTEGER

   LET posI = Entra.getIndexOf(Elimina,1)
   WHILE posI>0

      LET Entra = Entra.subString(1,posI-1),Entra.subString(posI+Elimina.getLength(),Entra.getLength())

      LET posI = Entra.getIndexOf(Elimina,1)
   END WHILE
   RETURN Entra
   
END FUNCTION



