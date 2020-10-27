database safre_viv@vivd_tcp
globals

DEFINE reg_bat    record 
       pid         integer ,
       proceso_cod integer ,
       opera_cod   integer ,
       folio       integer ,
       tipo        integer
END RECORD

define vfolio integer
    
end globals

main

LET reg_bat.pid         = ARG_VAL(1)
let reg_bat.proceso_cod = arg_val(2)
let reg_bat.opera_cod   = arg_val(3)
let reg_bat.folio       = arg_val(4)
let reg_bat.tipo        = arg_val(5)

let vfolio = reg_bat.folio

display reg_bat.pid
display reg_bat.proceso_cod
display reg_bat.opera_cod
display reg_bat.tipo


case reg_bat.tipo
when 1
  display "iniciando operacion 5"
  display "finalizando operacion 5"
  call actualiza_bat()
  exit case
when 2
  display "iniciando operacion 7"
  display "finalizando operacion 7"
    call actualiza_bat_f()
  exit case 
end case

end main

FUNCTION actualiza_bat()
#ab-----------------------

    DEFINE y              INTEGER

    DEFINE vv_prog        CHAR(10)
    DEFINE vv_fecha_log   CHAR(30)
    DEFINE lproc          CHAR(50)
    DEFINE lopera         CHAR(50)
    DEFINE paso           CHAR(100)
    DEFINE lformato       CHAR(150)
    DEFINE l_asunto       CHAR(200)
    DEFINE v_cat          CHAR(600)
    DEFINE l_correo       CHAR(1000)

    DEFINE v_fecha_log DATETIME YEAR TO SECOND

    DEFINE reg_ruta RECORD LIKE seg_modulo.*

    SELECT A.*
      INTO reg_ruta.*
      FROM seg_modulo A
     WHERE modulo_cod = "bat"

    UPDATE bat_ctr_operacion
    SET    folio      = vfolio,
           estado_cod = 4,
           fecha_fin  = TODAY
           --fecha_fin  = CURRENT
    WHERE  pid         = reg_bat.pid
    AND    proceso_cod = reg_bat.proceso_cod
    AND    opera_cod   = reg_bat.opera_cod

   -- UPDATE bat_ctr_proceso
    --SET    folio       = vfolio,
      --     estado_cod  = 4,
       --    fecha_fin   = CURRENT
   -- WHERE  pid         = reg_bat.pid
    --AND    proceso_cod = reg_bat.proceso_cod

    UPDATE bat_ctr_predecesor
    SET    bandera_ejecuta  = 1
    WHERE  pid_prod         = reg_bat.pid
    AND    proceso_cod_prod = reg_bat.proceso_cod
    AND    opera_cod_prod   = reg_bat.opera_cod

    LET v_fecha_log  = CURRENT
    LET vv_fecha_log = v_fecha_log

    SELECT A.programa_cod
      INTO vv_prog
      FROM bat_ctr_operacion A
     WHERE A.pid = reg_bat.pid
       AND A.proceso_cod = reg_bat.proceso_cod
       AND A.opera_cod   = reg_bat.opera_cod

    LET paso = "nohup:"            ,
               reg_bat.pid         USING"&&&&&",":",
               reg_bat.proceso_cod USING"&&&&&",":",
               reg_bat.opera_cod   USING"&&&&&"

    LET v_cat = "echo '"                ,
                vv_fecha_log[1,4]       ,
                vv_fecha_log[6,7]       ,
                vv_fecha_log[9,10]      ,
                vv_fecha_log[12,13]     ,
                vv_fecha_log[15,16]     ,
                vv_fecha_log[18,19]     ,
                "|"                    ,
                vv_prog  CLIPPED        ,
                "|"                     ,
                "FINOK"                ,
                "|"                     ,
                reg_ruta.ruta_listados CLIPPED,
                "/"                     ,
                paso CLIPPED            ,
                "'"                     ,
                " >> "                  ,
                reg_ruta.ruta_envio CLIPPED ,
                "/"                     ,
                "aad_safre.log"

    LET v_cat = v_cat CLIPPED
    RUN v_cat

-----correo electronico

    SELECT a.proceso_desc, b.opera_desc
      INTO lproc,lopera
      FROM cat_proceso a ,cat_operacion b
     WHERE a.proceso_cod = reg_bat.proceso_cod
       AND a.proceso_cod = b.proceso_cod
       AND b.opera_cod = reg_bat.opera_cod

    LET l_asunto = lproc clipped,"-",lopera clipped

    FOR y = 1 to  LENGTH(l_asunto)
        IF l_asunto[y]= " " THEN
            LET l_asunto[y]="_"
        END IF    END FOR

    LET lformato = "sed -f /safre_lst/bat/ins_nl.sed ",
                    " /safre_lst/bat/",paso clipped, 
                    " > /safre_lst/bat/correo_e.",paso clipped

    --RUN lformato

    LET l_correo = "mutt -s ",
        l_asunto CLIPPED,
        " mmuniz@efp.com.mx -c curzua@efp.com.mx -c jyanez@efp.com.mx -c mmuniz@efp.com.mx -a /safre/img/efp.jpg < ",
        " /safre_lst/bat/correo_e.",paso CLIPPED

    LET l_correo = l_correo CLIPPED
    --RUN l_correo

END FUNCTION
FUNCTION actualiza_bat_f()
#ab-----------------------

    DEFINE y              INTEGER

    DEFINE vv_prog        CHAR(10)
    DEFINE vv_fecha_log   CHAR(30)
    DEFINE lproc          CHAR(50)
    DEFINE lopera         CHAR(50)
    DEFINE paso           CHAR(100)
    DEFINE lformato       CHAR(150)
    DEFINE l_asunto       CHAR(200)
    DEFINE v_cat          CHAR(600)
    DEFINE l_correo       CHAR(1000)

    DEFINE v_fecha_log DATETIME YEAR TO SECOND

    DEFINE reg_ruta RECORD LIKE seg_modulo.*

    SELECT A.*
      INTO reg_ruta.*
      FROM seg_modulo A
     WHERE modulo_cod = "bat"

    UPDATE bat_ctr_operacion
    SET    folio      = vfolio,
           estado_cod = 4,
           --fecha_fin  = CURRENT
           fecha_fin  = TODAY
    WHERE  pid         = reg_bat.pid
    AND    proceso_cod = reg_bat.proceso_cod
    AND    opera_cod   = reg_bat.opera_cod

    UPDATE bat_ctr_proceso
    SET    folio       = vfolio,
           estado_cod  = 4,
           --fecha_fin   = CURRENT
           fecha_fin   =  TODAY
    WHERE  pid         = reg_bat.pid
    AND    proceso_cod = reg_bat.proceso_cod

    UPDATE bat_ctr_predecesor
    SET    bandera_ejecuta  = 1
    WHERE  pid_prod         = reg_bat.pid
    AND    proceso_cod_prod = reg_bat.proceso_cod
    AND    opera_cod_prod   = reg_bat.opera_cod

    LET v_fecha_log  = CURRENT
    LET vv_fecha_log = v_fecha_log

    SELECT A.programa_cod
      INTO vv_prog
      FROM bat_ctr_operacion A
     WHERE A.pid = reg_bat.pid
       AND A.proceso_cod = reg_bat.proceso_cod
       AND A.opera_cod   = reg_bat.opera_cod

    LET paso = "nohup:"            ,
               reg_bat.pid         USING"&&&&&",":",
               reg_bat.proceso_cod USING"&&&&&",":",
               reg_bat.opera_cod   USING"&&&&&"

    LET v_cat = "echo '"                ,
                vv_fecha_log[1,4]       ,
                vv_fecha_log[6,7]       ,
                vv_fecha_log[9,10]      ,
                vv_fecha_log[12,13]     ,
                vv_fecha_log[15,16]     ,
                vv_fecha_log[18,19]     ,
                "|"                    ,
                vv_prog  CLIPPED        ,
                "|"                     ,
                "FINOK"                ,
                "|"                     ,
                reg_ruta.ruta_listados CLIPPED,
                "/"                     ,
                paso CLIPPED            ,
                "'"                     ,
                " >> "                  ,
                reg_ruta.ruta_envio CLIPPED ,
                "/"                     ,
                "aad_safre.log"

    LET v_cat = v_cat CLIPPED
    RUN v_cat

-----correo electronico

    SELECT a.proceso_desc, b.opera_desc
      INTO lproc,lopera
      FROM cat_proceso a ,cat_operacion b
     WHERE a.proceso_cod = reg_bat.proceso_cod
       AND a.proceso_cod = b.proceso_cod
       AND b.opera_cod = reg_bat.opera_cod

    LET l_asunto = lproc clipped,"-",lopera clipped

    FOR y = 1 to  LENGTH(l_asunto)
        IF l_asunto[y]= " " THEN
            LET l_asunto[y]="_"
        END IF    END FOR

    LET lformato = "sed -f /safre_lst/bat/ins_nl.sed ",
                    " /safre_lst/bat/",paso clipped, 
                    " > /safre_lst/bat/correo_e.",paso clipped

    RUN lformato

    LET l_correo = "mutt -s ",
        l_asunto CLIPPED,
        " mmuniz@efp.com.mx -c curzua@efp.com.mx -c jyanez@efp.com.mx -c mmuniz@efp.com.mx -a /safre/img/efp.jpg < ",
        " /safre_lst/bat/correo_e.",paso CLIPPED

    LET l_correo = l_correo CLIPPED
    RUN l_correo

END FUNCTION
