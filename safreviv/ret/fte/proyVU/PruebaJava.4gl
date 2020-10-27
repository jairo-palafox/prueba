IMPORT JAVA com.fourjs.fgl.lang.FglByteBlob
MAIN
DEFINE jbyte com.fourjs.fgl.lang.FglByteBlob

DISPLAY "java importado sin miedo"

  LET jbyte = FglByteBlob.valueOf("0FA5617BDE")
  DISPLAY jbyte.toString()
END MAIN