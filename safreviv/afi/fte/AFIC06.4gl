#########################################################################
#Proyecto          => INFONAVIT (MEXICO)                                #
#Propietario       => E.F.P.                                            #
#Programa AFIC04   => Consulta de DH que tuvieron cambio de datos       #
#MODULO            => AFIC06                                            #
#Fecha             => 03 Agosto 2013                                    #
#########################################################################
DATABASE safre_viv

GLOBALS
DEFINE g_i_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_usuario_cod   LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_v_nom_prog    VARCHAR(30) -- nombre del programa

END GLOBALS

MAIN
--Sección de variables UI
DEFINE v_fecha          DATE,
       v_sql            STRING, -- cadena SQL de consulta
       v_registro_salida RECORD
         nss        CHAR(11),
         f_proceso  CHAR(8) ,
         rfc        CHAR(13),
         curp       CHAR(18),
         ap_paterno CHAR(50),
         ap_materno CHAR(50),
         nombre     CHAR(50)
       END RECORD,
       v_registro_consulta RECORD
         nss               LIKE afi_derechohabiente.nss,
         f_modifica        LIKE afi_his_derechohabiente.f_modifica,
         rfc               LIKE afi_derechohabiente.rfc,
         curp              LIKE afi_derechohabiente.curp,
         ap_paterno_af     LIKE afi_derechohabiente.ap_paterno_af,
         ap_materno_af     LIKE afi_derechohabiente.ap_materno_af,
         nombre_af         LIKE afi_derechohabiente.nombre_af,
         ind_modifica      LIKE cat_afi_ind_modifica.ind_modifica,
         ind_modifica_desc LIKE cat_afi_ind_modifica.ind_modifica_desc,
         folio             LIKE glo_folio.folio
       END RECORD,
       v_cadena          STRING,
       v_ruta_salida     LIKE seg_modulo.ruta_envio,
       v_ruta_archivo    STRING,
       v_archivo_salida base.channel -- archivo de salida

   -- se recuperan los parametros 
   LET g_usuario_cod          = ARG_VAL(1) -- Recibe la variable de usuario
   LET g_i_proceso_cod        = ARG_VAL(2) -- Recibe el tipo de proceso
   LET g_v_nom_prog           = ARG_VAL(3) -- Recibe el nombre del programa

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_v_nom_prog)
   END IF

   OPEN WINDOW w_consulta WITH FORM "AFIC061"

   -- como fecha por omision se usa la fecha del dia
   LET v_fecha = TODAY
      
   -- se obtienen los parametros de consulta
   INPUT BY NAME v_fecha WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )
   
   
      ON ACTION ACCEPT
         -- la fecha de consulta no puede ser posterior a la actual
         IF ( v_fecha > TODAY ) THEN
            CALL fn_mensaje("Atención","La fecha de consulta no puede ser posterior a la fecha actual","stop")
            CONTINUE INPUT
         END IF
         
         -- es necesario capturar la fecha
         IF ( v_fecha IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar una fecha de consulta","stop")
            CONTINUE INPUT
         END IF

         -- se da alta prioridad
         EXECUTE IMMEDIATE "SET PDQPRIORITY HIGH"
         
{
1 NSS 11 001 - 011 NSS
2 Fecha proceso 08 012 - 019 Fecha procesamiento archivo formato AAAAMMDD
3 RFC 13 020 - 032 RFC Derechohabiente
4 CURP 18 033 - 050 CURP Derechohabiente
5 Apellido Paterno 50 051 - 100 Apellido paterno derechohabiente
6 Apellido Materno 50 101 - 150 Apellido materno derechohabiente
7 Nombre del Campo 50 151 - 200 Nombre derechohabiente
}
         
         -- se consultan los datos
         LET v_sql = "\n SELECT DISTINCT       ",
                     "\n afi.nss              ,",
                     "\n his.f_modifica       ,",
                     "\n afi.rfc              ,",
                     "\n afi.curp             ,",
                     "\n afi.ap_paterno_af    ,",
                     "\n afi.ap_materno_af    ,",
                     "\n afi.nombre_af        ,",
                     "\n his.ind_modifica     ,",
                     "\n cat.ind_modifica_desc,",
                     "\n fol.folio             ",
                     "\n FROM afi_derechohabiente afi,",
                     "\n      afi_his_derechohabiente his,",
                     "\n      glo_folio               fol,",
                     "\n      cat_afi_ind_modifica    cat ",
                     "\n WHERE fol.f_actualiza = ?",
                     "\n AND fol.proceso_cod IN (1801,1802,1803,1808)", -- procesos de IMSS,SOLO INF, OPT75, cambio RFC
                     "\n AND fol.status >= 0", -- solo folios validos
                     "\n AND his.folio_lote_modifica = fol.folio", -- los folios que coincidan
                     "\n AND afi.id_derechohabiente = his.id_derechohabiente",
                     "\n AND cat.ind_modifica = his.ind_modifica",
                     "\n AND cat.ind_modifica IN (1,2,5,6,9,10,11,12,13,14)" -- cambios necesarios

{
           1 CAMBIO CURP
           2 CAMBIO RFC
           5 CAMBIO NOMBRE IMSS
           6 CAMBIO NOMBRE AFORE
           9 CAMBIO NOMBRE INFNVT
          10 CAMBIO RFC NSS
          11 CAMBIO RFC NOMBRE
          12 CAMBIO NSS NOMBRE
          13 CAMBIO RFC NSS NOMB
          14 CAMBIO EDO-MUNICIPIO
}
                     
         -- consulta
         DISPLAY v_sql           

         -- se prepara y ejecuta la consulta
         PREPARE sid_consultacambio FROM v_sql
         
         DECLARE cur_consultacambio CURSOR FOR sid_consultacambio
         
         -- se crea el archivo
         LET v_archivo_salida = base.channel.create()
         
         CALL v_archivo_salida.setDelimiter("|")
         
         SELECT ruta_envio
         INTO   v_ruta_salida
         FROM   seg_modulo
         WHERE  modulo_cod = "afi"
         
         -- se crea la ruta del archivo
         LET v_cadena = v_fecha USING "yyyymmdd"
         LET v_ruta_archivo = v_ruta_salida CLIPPED, "/CAMBIOS_", v_cadena, ".txt"
         
         CALL v_archivo_salida.openFile(v_ruta_archivo, "w")
         
         FOREACH cur_consultacambio USING v_fecha
         INTO v_registro_consulta.*
       
           
            -- se transfieren y formatean los datos en el registro de salida
            LET v_registro_salida.nss        = v_registro_consulta.nss
            LET v_registro_salida.f_proceso  = v_fecha USING "yyyymmdd"
            LET v_registro_salida.rfc        = v_registro_consulta.rfc
            LET v_registro_salida.curp       = v_registro_consulta.curp
            LET v_registro_salida.ap_paterno = v_registro_consulta.ap_paterno_af
            LET v_registro_salida.ap_materno = v_registro_consulta.ap_materno_af
            LET v_registro_salida.nombre     = v_registro_consulta.nombre_af
            
            -- se concatenan
            LET v_cadena = v_registro_salida.nss       ,
                           v_registro_salida.f_proceso ,
                           v_registro_salida.rfc       ,
                           v_registro_salida.curp      ,
                           v_registro_salida.ap_paterno,
                           v_registro_salida.ap_materno,
                           v_registro_salida.nombre    

            -- se escribe el registro
            CALL v_archivo_salida.writeLine(v_cadena)
         END FOREACH       
                           
         -- se libera el cursor
         FREE cur_consultacambio
         
         -- se cierra el archivo
         CALL v_archivo_salida.close()

         -- se devuelve la prioridad
         EXECUTE IMMEDIATE "SET PDQPRIORITY LOW"
         
         -- se indica al usuario en que archivo quedo su consulta
         LET v_cadena = "Consulta finalizada\n\n",
                        "El resultado de la consulta se encuentra en el archivo:\n",
                        v_ruta_archivo
         
         -- mensaje a pantalla
         CALL fn_mensaje("Atención", v_cadena, "information")
         EXIT INPUT

      ON ACTION Cancel
         EXIT INPUT
   END INPUT 
      
   CLOSE WINDOW w_consulta
   

END MAIN