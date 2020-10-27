DATABASE safre_viv
MAIN
DEFINE ls_path STRING,
       ls_default_path STRING,
       ls_caption STRING,
       ls_file_type STRING,
       ls_file_type_description STRING,
       v_rfc CHAR(11),
       V_CONTEO SMALLINT,
       elHash base.Channel,
       v_comando STRING,
       v_hash   string,
       lst_tokenizer base.StringTokenizer

       LET elHash = base.Channel.create()

       

       LET v_comando = "sha1sum RETC14.4gl"
       
       CALL elhash.openPipe( v_comando, "u")

       CALL elHash.setDelimiter(" ")
       
       WHILE elhash.read([v_hash])
         DISPLAY "Hash calculado: "

         LET lst_tokenizer = base.StringTokenizer.create(v_hash," ")
         
         IF ( lst_tokenizer.hasMoreTokens() ) THEN
           DISPLAY "HASH: ", lst_tokenizer.nextToken()
           DISPLAY "Archivo: ", lst_tokenizer.nextToken()
         END IF
       END WHILE
       
{
  -- default path and dialog caption
  LET ls_default_path = "C:\\"
  LET ls_file_type = "*.docx"
  LET ls_file_type_description = "MS Word documents"

  OPEN WINDOW w WITH FORM "OPENDIR"
  
  MENU
    ON ACTION opendir
        LET ls_caption = "Choose a directory"
      CALL ui.Interface.frontCall( "standard", "opendir", [ ls_Default_path, ls_caption], [ls_path])

      IF ( ls_path IS NOT NULL ) THEN
        CALL fgl_winmessage("Choosen path", ls_path, "info")
      END IF

    ON ACTION openfile
      LET ls_Caption = "Choose a file" 
      CALL ui.Interface.frontCall( "standard", "openfile", [ ls_Default_path, ls_file_type_Description, ls_file_type, ls_caption], [ls_path])

      IF ( ls_path IS NOT NULL ) THEN
        CALL fgl_winmessage("Choosen file", ls_path, "info")
      END IF

    ON ACTION CLOSE
      EXIT MENU

    ON ACTION exit
      EXIT MENU

      
  END MENU

  CLOSE WINDOW w
  }
  DISPLAY CURRENT HOUR TO SECOND

  SELECT nss
  INTO v_rfc
  FROM afi_derechohabiente
  WHERE nss IN (   "94949494945")

{
  SELECT COUNT(*)
  INTO V_CONTEO
  FROM afi_derechohabiente
  WHERE nss = "94949494945"
  }
{
 UPDATE afi_derechohabiente
SET rfc = "RFCA112233AE1"
WHERE nss IN (   "94949494949", "49494949494")
}
  
  IF ( STATUS <> NOTFOUND ) THEN
  --IF ( SQLCA.sqlcode <> NOTFOUND ) THEN
  --IF ( v_conteo < 1 ) THEN
     DISPLAY STATUS
     DISPLAY "si lo encontro"
     DISPLAY SQLCA.sqlcode
     FOR V_CONTEO = 1 TO SQLCA.sqlerrd.getLength()
        DISPLAY "SQLCA.sqlerrd[ ", v_conteo, "]: ", SQLCA.sqlerrd[v_conteo]
     END FOR
     DISPLAY "SQLCA.sqlerrm ", SQLCA.sqlerrm
     DISPLAY "SQLCA.sqlerrp ", SQLCA.sqlerrp
  ELSE
     DISPLAY STATUS
     DISPLAY "no lo encontro"
     DISPLAY SQLCA.sqlcode
     FOR V_CONTEO = 1 TO SQLCA.sqlerrd.getLength()
        DISPLAY "SQLCA.sqlerrd[ ", v_conteo, "]: ", SQLCA.sqlerrd[v_conteo]
     END FOR
     DISPLAY "SQLCA.sqlerrm ", SQLCA.sqlerrm
     DISPLAY "SQLCA.sqlerrp ", SQLCA.sqlerrp

  END IF

  
  
  DISPLAY CURRENT HOUR TO SECOND
  
END MAIN