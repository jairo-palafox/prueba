IMPORT JAVA efp.ClienteInfonavitSat
IMPORT JAVA com.efp.safre.client.ClientLDAP
IMPORT os

GLOBALS "CPBW011.inc"

DATABASE safre_cpb

DEFINE sm1 om.DomNode
DEFINE aui om.DomNode

PRIVATE CONSTANT USUARIO_VALIDO          SMALLINT = 1
PRIVATE CONSTANT USUARIO_SFR_NO_EXISTE   SMALLINT = 2
PRIVATE CONSTANT USUARIO_SFR_INACTIVO    SMALLINT = 3
PRIVATE CONSTANT USUARIO_LDAP_NO_EXISTE  SMALLINT = 4
PRIVATE CONSTANT PSW_ERRONEO             SMALLINT = 5
PRIVATE CONSTANT LDAP_NO_DISPONIBLE      SMALLINT = 6
PRIVATE CONSTANT USUARIO_CONECTADO       SMALLINT = 7
PRIVATE CONSTANT USUARIO_CADUCADO        SMALLINT = 8
PRIVATE CONSTANT USUARIO_SFR_BOQUEADO    SMALLINT = 9
PRIVATE CONSTANT USUARIO_SFR_BAJA        SMALLINT = 10


MAIN
    DEFINE v_archivo_cer STRING
    DEFINE v_archivo_key STRING
    DEFINE v_password_key STRING
    DEFINE v_resp_cliente STRING
    DEFINE v_rfc CHAR(13)
    DEFINE v_estado_transf_archivo SMALLINT
    DEFINE v_usuario CHAR(20)
    DEFINE v_tipo_ejecucion SMALLINT
    DEFINE v_titulo VARCHAR(30)
    DEFINE v_comando STRING
    DEFINE v_ruta_archivo_server_cer STRING
    DEFINE v_ruta_archivo_server_key STRING
    DEFINE v_user_id CHAR(20)
    DEFINE v_password CHAR(20)
    DEFINE v_grupo CHAR(20)
    DEFINE r_estado_autenticacion SMALLINT
    DEFINE f_ventana            ui.Window,   --Define las propìedades de la Ventana
           f_forma              ui.Form      --Define las propiedades de la forma 
    DEFINE v_entra_menu    SMALLINT
    DEFINE ls_bnd_confirma SMALLINT
    DEFINE lc_des_usu      STRING
    DEFINE lv_cadena       STRING
    DEFINE v_resultado_validacion SMALLINT
    DEFINE v_rfc_login char(13)
    DEFINE v_cve_ent_financiera_login SMALLINT
    DEFINE v_ind_existe_usuario_login SMALLINT
    DEFINE v_codigo                   SMALLINT
    DEFINE v_cadena                   STRING
           
    LET v_usuario          = ARG_VAL  (1)
    LET v_tipo_ejecucion   = ARG_VAL  (2)
    LET v_titulo           = ARG_VAL  (3)

    LET v_titulo="Infonavit Consultas"
    CALL ui.Interface.setText(v_titulo)

    CLOSE WINDOW SCREEN

    OPEN WINDOW aciweb WITH 80 ROWS, 80 COLUMNS
    ATTRIBUTE (STYLE="menu", TEXT="SACI")

        OPEN WINDOW vtn1 WITH FORM "CPBL001"
            LET f_ventana = ui.Window.getCurrent()
            CALL f_ventana.setText("Login")
            LET f_forma = f_ventana.getForm()

          DIALOG ATTRIBUTES (UNBUFFERED)

####SE SOLICITÓ CAMBIO PARA NO VALIDAR EL USUARIO DE TIVOLI SOLO SE VALIDARA EL 
####RFC QUE DEVUELVA EL SERVICIO DE LA FIEL

{              INPUT BY NAME v_user_id,v_password,v_grupo  
                BEFORE INPUT
                #el grupo sera fijo
                LET v_grupo="SACIEXTQA"
                
                ON ACTION boton_aceptar
                    IF LENGTH(v_user_id CLIPPED)==0 THEN
                        CALL fn_mensaje("","Ingrese el Usuario ID","information")
                        CONTINUE DIALOG
                    END IF

                    IF LENGTH(v_password CLIPPED)==0 THEN
                        CALL fn_mensaje("","Ingrese el Password","information")
                        CONTINUE DIALOG
                    END IF

                    IF LENGTH(v_grupo CLIPPED)==0 THEN
                        CALL fn_mensaje("","Ingrese el grupo","information")
                        CONTINUE DIALOG
                    END IF

                    #se verifica si es interno o externo
                    IF v_user_id MATCHES "IN*" OR v_user_id MATCHES "IE*" THEN
                        #Interno se valida normal
                        CALL fn_valida_acceso(v_user_id, v_password) RETURNING v_resultado_validacion
                        IF v_resultado_validacion = 1 THEN
                            #se accesa normal al menu
                            LET v_entra_menu=1
                            EXIT DIALOG
                        ELSE
                            CONTINUE DIALOG
                        END IF
                    ELSE
                        #externo se valida por WS
                        CALL fn_valida_datos_externo(v_user_id,v_password,v_grupo) 
                        RETURNING r_estado_autenticacion

                        DISPLAY "r_estado_autenticacion",r_estado_autenticacion
                     
                        IF r_estado_autenticacion==0 THEN
                            #se ejecuta el siguiente login
                            CALL f_forma.setElementHidden("group1",1)
                            CALL f_forma.setElementHidden("group2",0)
                        END IF

                        IF r_estado_autenticacion==2 THEN
                            CALL fn_mensaje("","Hubo un error al ejecutar el WS","information")
                            CONTINUE DIALOG
                        END IF

                        IF r_estado_autenticacion<>0 AND r_estado_autenticacion<>2 THEN
                            CALL fn_mensaje("","Datos incorrectos","information")
                            CONTINUE DIALOG
                        END IF
                        
                    END IF
 
                ON ACTION boton_cancelar
                    EXIT DIALOG

            --    BEFORE DIALOG
                    
              END INPUT 
}
              INPUT BY NAME v_archivo_cer,v_archivo_key,v_password_key

                ON ACTION boton_aceptar_key
                    IF v_archivo_cer IS NULL THEN
                        CALL fn_mensaje("","Selecciona el archivo cer a cargar","about")
                        CONTINUE DIALOG
                    END IF

                    IF v_archivo_key IS NULL THEN
                        CALL fn_mensaje("","Selecciona el archivo key a cargar","about")
                        CONTINUE DIALOG
                    END IF

                    IF v_password_key IS NULL THEN
                        CALL fn_mensaje("","Ingrese el password","about")
                        CONTINUE DIALOG
                    END IF

                 --   LET v_archivo_cer="C:\Users\sugr6704057g7.cer"
                 --   LET v_archivo_key="C:\Users\Clavepriva.key"

                    CALL fn_recupera_archivo(v_archivo_cer,v_archivo_key)
                    RETURNING v_estado_transf_archivo,
                              v_ruta_archivo_server_cer,
                              v_ruta_archivo_server_key

                    DISPLAY "v_estado_transf_archivo",v_estado_transf_archivo
                    DISPLAY "v_ruta_archivo_server_cer",v_ruta_archivo_server_cer
                    DISPLAY "v_ruta_archivo_server_key",v_ruta_archivo_server_key

                    #se limpian variables de archivos
                    IF v_estado_transf_archivo==1 OR 
                       v_estado_transf_archivo==2 OR
                       v_estado_transf_archivo==3 THEN
                       
                        LET v_archivo_cer=NULL
                        LET v_archivo_key=NULL
                        LET v_password_key=NULL
                        LET v_ruta_archivo_server_cer=NULL
                        LET v_ruta_archivo_server_key=NULL
                    END IF
                    
                    IF v_estado_transf_archivo==1 THEN
                        CALL fn_mensaje("","Hubo un error con la transferencia \n de archivos","about")
                        CONTINUE DIALOG
                    END IF

                    IF v_estado_transf_archivo==2 THEN
                        CALL fn_mensaje("","La extensión del archivo .cer es incorrecta","about")
                        CONTINUE DIALOG
                    END IF

                    IF v_estado_transf_archivo==3 THEN
                        CALL fn_mensaje("","La extensión del archivo .key es incorrecta","about")
                        CONTINUE DIALOG
                    END IF

                    DISPLAY "Ejecutando jar"
                    CALL fn_ejecuta_cliente(v_ruta_archivo_server_cer,
                                            v_ruta_archivo_server_key,
                                            v_password_key) 
                    RETURNING v_resp_cliente
                    DISPLAY "Fin Ejecutando jar"
                    
                    #se eliminan los archivos
                    CALL fn_elimina_archivos(v_ruta_archivo_server_cer,v_ruta_archivo_server_key)

                    IF v_resp_cliente.subString(1,2)=='00' THEN
                        DISPLAY "El WS se ejecuto correctamente y se aceptaron los certificados"
                        #SE obtiene el rfc
                        LET v_rfc=v_resp_cliente.subString(3,15)
                        DISPLAY "RFC:",v_rfc

{                        #se verifica que el RFC del login y el del REST sean iguales
                        CALL fn_obtiene_rfc(v_user_id) RETURNING v_rfc_login,
                                                        v_cve_ent_financiera_login,
                                                        v_ind_existe_usuario_login

                        DISPLAY "v_rfc_login",v_rfc_login
                        DISPLAY "v_rfc",v_rfc
                        DISPLAY "v_cve_ent_financiera_login",v_cve_ent_financiera_login
}
                        CALL fn_valida_rfc_usuario(v_rfc) RETURNING v_codigo, v_cadena,v_cve_ent_financiera_login
                        IF v_codigo = 1 THEN
                            --CALL fn_mensaje("","El WS se ejecutó correctamente y la Efirma es válida","about")
                            #Se accesa al menu
                            LET v_entra_menu=1
                            EXIT DIALOG
                        ELSE
                            --CALL fn_mensaje("","El WS se ejecutó correctamente, \n no coinciden datos de login y Efirma","about")
                            CALL fn_mensaje (v_rfc,
                                             v_cadena,
                                             "about")
                            #se impide el acceso
                            LET v_archivo_cer=NULL
                            LET v_archivo_key=NULL
                            LET v_password_key=NULL
                            LET v_ruta_archivo_server_cer=NULL
                            LET v_ruta_archivo_server_key=NULL
                            CONTINUE DIALOG
                        END IF

                    END IF

                    IF v_resp_cliente.subString(1,2)=='01' THEN
                        DISPLAY "El WS se ejecuto correctamente y se rechazaron los certificados"
                        CALL fn_mensaje("","El WS se ejecutó correctamente y la Efirma es inválida","about")

                        LET v_archivo_cer=NULL
                        LET v_archivo_key=NULL
                        LET v_password_key=NULL
                        LET v_ruta_archivo_server_cer=NULL
                        LET v_ruta_archivo_server_key=NULL
                        
                        CONTINUE DIALOG
                    END IF

                    IF v_resp_cliente.subString(1,2)=='10' THEN
                        DISPLAY "El WS se ejecuto incorrectamente"
                        CALL fn_mensaje("","El WS no se ejecutó correctamente","about")

                        LET v_archivo_cer=NULL
                        LET v_archivo_key=NULL
                        LET v_password_key=NULL
                        LET v_ruta_archivo_server_cer=NULL
                        LET v_ruta_archivo_server_key=NULL
                        
                        CONTINUE DIALOG
                    END IF

              
                ON ACTION boton_cancelar_key 
                    EXIT DIALOG 
              END INPUT

              BEFORE DIALOG
                CALL f_forma.setElementHidden("group1",1)
                CALL f_forma.setElementHidden("group2",0)
                LET v_entra_menu=0
          END DIALOG
       CLOSE WINDOW vtn1

    IF v_entra_menu==1 THEN
        LET v_user_id = v_rfc
        LET aui = ui.Interface.getRootNode()                           
        LET sm1 = aui.createChild("StartMenu")
        CALL f_starmenu(v_rfc,v_rfc,v_cve_ent_financiera_login) 

        -- se consulta la descripcion del usuario
           LET lc_des_usu = f_usuario_desc(v_user_id)
          
           LET lv_cadena = "Usuario: ",lc_des_usu CLIPPED," Fecha: ",
                                       TODAY USING "DD-MM-YYYY"

           DISCONNECT CURRENT
           -- Menu para mantener ventana activa
           MENU lv_cadena
            
              ON ACTION close
                 IF ui.Interface.getChildCount()>0 THEN
                    CALL fn_mensaje("Atención", "Debe cerrar todas las pestañas antes de salir","stop")
                 ELSE
                    CALL fn_ventana_confirma("Atención", "Se cerrarán todas las pestañas abiertas ¿Desea salir? ","stop")
                    RETURNING  ls_bnd_confirma

                    IF ls_bnd_confirma = 1 THEN 
                       EXIT MENU
                    ELSE 
                       CONTINUE MENU
                    END IF 
                 END IF

           END MENU
    END IF

END MAIN

FUNCTION fn_valida_rfc_usuario(p_rfc)
   DEFINE p_rfc         CHAR(13)
   DEFINE v_cadena      STRING
   DEFINE v_codigo      SMALLINT
   DEFINE v_entidad     SMALLINT
   DEFINE v_ind_activo  SMALLINT
   DEFINE v_usuario_cod CHAR(20)

   LET v_entidad = NULL

   SELECT rfc, cve_ent_financiera
     INTO v_usuario_cod, v_entidad
     FROM safre_viv:cat_usuario_ef
    WHERE rfc = p_rfc

   IF v_usuario_cod IS NULL THEN
      LET v_codigo = 2
      LET v_cadena = "El usuario no está dado de alta para entrar al sistema."
   ELSE
      LET v_codigo = 1
      LET v_cadena = v_usuario_cod CLIPPED
   END IF
   
   RETURN v_codigo, v_cadena, v_entidad
END FUNCTION


FUNCTION fn_ejecuta_cliente(p_archivo_cer,p_archivo_key,p_password)
    DEFINE p_archivo_cer STRING
    DEFINE p_archivo_key STRING
    DEFINE p_password STRING
    DEFINE v_resp_jar STRING
    DEFINE obj_cliente ClienteInfonavitSat

    LET obj_cliente=ClienteInfonavitSat.create()
    LET v_resp_jar=obj_cliente.ClienteInfonavitSat(p_archivo_cer,p_archivo_key,p_password)

    DISPLAY "v_resp_jar",v_resp_jar

    RETURN v_resp_jar

END FUNCTION

FUNCTION fn_recupera_archivo(p_archivo_cer,p_archivo_key)
    DEFINE p_archivo_cer STRING
    DEFINE p_archivo_key STRING
    DEFINE v_ruta_binaria VARCHAR(100)
    DEFINE v_ruta_rescate VARCHAR(100)
    DEFINE v_ruta_archivo_cer VARCHAR(100)
    DEFINE v_ruta_archivo_key VARCHAR(100)
    DEFINE v_band_trans_arch SMALLINT
    DEFINE v_buffer_cer base.StringBuffer
    DEFINE v_buffer_key base.StringBuffer
    DEFINE v_ext_cer CHAR(4)
    DEFINE v_ext_key CHAR(4)
    DEFINE v_pos_ext SMALLINT
    DEFINE v_long_nom_archivo INTEGER
    DEFINE v_nombre_archivo_cer STRING
    DEFINE v_nombre_archivo_key STRING

    SELECT ruta_bin,ruta_rescate
    INTO v_ruta_binaria,v_ruta_rescate
    FROM seg_modulo
    WHERE modulo_cod = 'cpb'

    DISPLAY "p_archivo_cer",p_archivo_cer
    DISPLAY "p_archivo_key",p_archivo_key

    #se crea buffer del archivo
    LET v_buffer_cer = base.StringBuffer.create()   # Se crea objeto StringBuffer 
    CALL v_buffer_cer.append(p_archivo_cer)

    #se crea buffer del archivo
    LET v_buffer_key = base.StringBuffer.create()   # Se crea objeto StringBuffer 
    CALL v_buffer_key.append(p_archivo_key)

    #no hubo error con la transferencia
    LET v_band_trans_arch=0
    #hubo error con la trasferencia
 --   LET v_band_trans_arch=1
    #el archivo .cer no tiene la extension
 --   LET v_band_trans_arch=2
    #el archivo .key no tiene la extension
 --   LET v_band_trans_arch=3
  --  LET v_band_trans_arch=4
 --   LET v_band_trans_arch=5

    #se encuetra la extension del .cer y .key
    LET v_long_nom_archivo=LENGTH(p_archivo_cer)
    LET v_pos_ext=v_buffer_cer.getIndexOf(".",1)
    LET v_ext_cer = v_buffer_cer.subString(v_pos_ext,v_long_nom_archivo)

    #se obtiene solo el nombre del archivo
    LET v_nombre_archivo_cer=v_buffer_cer.subString(13,v_long_nom_archivo) 
    
    LET v_long_nom_archivo=LENGTH(p_archivo_key)
    LET v_pos_ext=v_buffer_key.getIndexOf(".",1)
    LET v_ext_key=v_buffer_key.subString(v_pos_ext,v_long_nom_archivo)

    #se obtiene solo el nombre del archivo
    LET v_nombre_archivo_key=v_buffer_key.subString(13,v_long_nom_archivo) 

    LET v_ruta_archivo_cer = v_ruta_rescate CLIPPED,"/",v_nombre_archivo_cer
    LET v_ruta_archivo_key = v_ruta_rescate CLIPPED,"/",v_nombre_archivo_key

    DISPLAY "v_ruta_archivo_cer",v_ruta_archivo_cer
    DISPLAY "v_ruta_archivo_key",v_ruta_archivo_key
    
    DISPLAY "v_ext_cer",v_ext_cer
    DISPLAY "v_ext_key",v_ext_key

    IF v_ext_cer<>'.cer' THEN
        LET v_band_trans_arch=2
    END IF

    IF v_ext_key<>'.key' THEN
        LET v_band_trans_arch=3
    END IF

    IF v_band_trans_arch==0 THEN
        #se pasa el archivo a la ruta rescate
        TRY 
            CALL FGL_GETFILE(p_archivo_cer,v_ruta_archivo_cer)
            CALL FGL_GETFILE(p_archivo_key,v_ruta_archivo_key)
        CATCH
            DISPLAY "No se pudo realizar la carga del archivo"
         #   CALL fn_mensaje("","No se pudo realizar la carga del archivo","")
            LET v_band_trans_arch=1 
        END TRY 
    END IF

    RETURN v_band_trans_arch,v_ruta_archivo_cer,v_ruta_archivo_key

END FUNCTION

FUNCTION fn_elimina_archivos(p_ruta_archivo_server_cer,p_ruta_archivo_server_key)
    DEFINE p_ruta_archivo_server_cer STRING
    DEFINE p_ruta_archivo_server_key STRING
    DEFINE v_comando STRING

    LET v_comando="rm ",p_ruta_archivo_server_cer
    RUN v_comando
    LET v_comando="rm ",p_ruta_archivo_server_key
    RUN v_comando

END FUNCTION

FUNCTION fn_valida_datos_externo(p_user_id,p_password,p_grupo)
    DEFINE p_user_id CHAR(20)
    DEFINE p_password CHAR(20)
    DEFINE p_grupo CHAR(20)
    DEFINE v_arg1_resp STRING
    DEFINE v_arg2_resp STRING
    DEFINE v_estado SMALLINT
    DEFINE v_estado_autenticacion SMALLINT

    LET autenticate.arg0=p_user_id CLIPPED
    LET autenticate.arg1=p_password CLIPPED
    LET autenticate.arg2=p_grupo CLIPPED

   DISPLAY "\n DATOS ENVIADOS"
   DISPLAY "\n CUERPO"
   DISPLAY "v_user_id: -",autenticate.arg0,"-"
   DISPLAY "v_password: -",autenticate.arg1,"-"
   DISPLAY "v_grupo: -",autenticate.arg2,"-"

   #CABC
 #   LET LdapServices_LdapServicesPortEndpoint.Address.Uri=fn_obtiene_url_uri(v_servicio_cod)
    LET LdapServices_LdapServicesPortEndpoint.Address.Uri="alias://ExternosTivoli"
    LET v_estado = 0

    LET v_estado=autenticate_g()

       DISPLAY "\n ESTADO: ", v_estado
   
    IF v_estado = 0 THEN   -- Ejecucion correcta
        LET v_arg1_resp=autenticateResponse.return.code
        LET v_arg2_resp=autenticateResponse.return.message

      DISPLAY "\n --DATOS RECIBIDOS"
      DISPLAY "return.code: -",v_arg1_resp,"-"
      DISPLAY "return.message: -",v_arg2_resp,"-"
      
    ELSE

      LET v_estado_autenticacion=2
      DISPLAY "wsError.code         : ",wsError.code
      DISPLAY "wsError.codeNS		   : ",wsError.codeNS
      DISPLAY "wsError.description  : ",wsError.description	
      DISPLAY "wsError.action		   : ",wsError.action

    END IF

    #CABC falta comparar los datos con otros

    #se verifica si los datos son correctos
    IF v_arg1_resp==0 THEN
        LET v_estado_autenticacion=0
    END IF

    RETURN v_estado_autenticacion

END FUNCTION

{=========================================================================
Clave:
Nombre: f_starmenu
Fecha Creacion: 14/12/2011
Autor: Hugo César Ramírez Garcia, Benito Téllez Sánchez
Narrativa Del Proceso que Realiza:
Funcion que construye el starmenu deacuerdo al usuario
Parametros: 
   Entrada: p_cod_usu
   Salida:  N/A
Registro de Modificaciones:
Autor Fecha Descrip. cambio
=========================================================================}
FUNCTION f_starmenu(p_cod_usu,p_rfc,p_cve_ent_financiera)
DEFINE p_cod_usu      LIKE seg_usuario.usuario_cod,
       lc_des_usu     LIKE seg_usuario.usuario_desc,
       li_contador    INTEGER,
       lc_menu        LIKE seg_menu.menu_cod,
       lc_padre       LIKE seg_menu.padre_cod,
       lc_opcion      LIKE seg_menu.opcion,
       li_tipo        LIKE seg_menu.tipo,   --tipo = 0 > Grupo y tipo = 1 > Comando
       lc_programa    LIKE seg_programa.programa_cod,
       lc_ruta        LIKE seg_modulo.ruta_bin,
       nl             om.NodeList,
       n              om.DomNode,
       i              INTEGER,
       s              om.DomNode,
       ls_qryTxt      STRING,
       v_comando      STRING,
       p_rfc CHAR(13),
       p_cve_ent_financiera SMALLINT

   -- se ejecuta sentencia sql que obtiene la descripcion del usuario
   LET ls_qryTxt = " SELECT usuario_desc\n",
                   " FROM seg_usuario\n",
                   " WHERE usuario_cod = ?"
   
   PREPARE prp_infoModulo FROM ls_qryTxt
   EXECUTE prp_infoModulo USING p_cod_usu INTO lc_des_usu
   
   LET ls_qryTxt = "\n SELECT DISTINCT sm.menu_cod,sm.padre_cod,sm.opcion,sm.tipo,",
                   "\n                 spo.programa_cod,smo.ruta_bin",
                   "\n FROM seg_usuario_perfil sup",
                   "\n JOIN seg_privilegio sp",
                   "\n   ON usuario_cod = ?",   --'safre'
                   "\n  AND sp.perfil_cod = sup.perfil_cod",
                   "\n JOIN seg_menu sm",
                   "\n   ON sm.menu_cod = sp.menu_cod",
                   "\n      LEFT OUTER JOIN seg_programa spo",
                   "\n   ON spo.programa_cod = sm.programa_cod",
                   "\n      LEFT OUTER JOIN seg_modulo smo",
                   "\n   ON spo.modulo_cod = smo.modulo_cod",
                   "\n ORDER BY sm.menu_cod"
    --DISPLAY ls_qryTxt
   PREPARE prp_sql_rec_menu FROM ls_qryTxt
   
   -- se despliega en el start menu la clave de usuario
   CALL sm1.setAttribute("text",p_cod_usu)
   
   CALL UI.INTERFACE.REFRESH()
   
   -- se crea sentencia sql que obtiene los grupos y comandos pertenecientes al usuario en proceso
   DECLARE cur_modulo2 CURSOR FOR prp_sql_rec_menu
   FOREACH cur_modulo2 USING p_cod_usu
                       INTO lc_menu,lc_padre,lc_opcion,li_tipo,lc_programa,lc_ruta
      --DISPLAY "MENU: ",lc_menu,lc_padre,lc_opcion,li_tipo,lc_programa,lc_ruta
      -- method allow you to search for children nodes according to a tag name
      LET nl = sm1.selectByTagName("StartMenuGroup")
      LET i = 1
      
      LET li_contador = 0
      
      FOR i = 1 to nl.getLength()
        
         -- This method returns the DomNode that is at the specified position in the list
         LET n = nl.item(i)
         
         -- Crea grupo/comando de arbol     
         IF ( lc_padre = n.getAttribute("name") ) THEN
            LET li_contador = 1
            
            IF li_tipo = 0 THEN
               LET s = n.createChild("StartMenuGroup")
               CALL s.setAttribute("text",lc_opcion)
               CALL s.setAttribute("name",lc_menu)
                              
            ELSE
               LET s = n.createChild("StartMenuCommand")
               CALL s.setAttribute("text",lc_opcion)
               CALL s.setAttribute("name",lc_menu)
               LET v_comando = "cd " || lc_ruta CLIPPED || "; fglrun "
               LET v_comando = v_comando || lc_programa CLIPPED || ".42r "|| p_cod_usu
               LET v_comando = v_comando || " 0 " || " '" || lc_opcion CLIPPED || "' "||p_rfc||" "||p_cve_ent_financiera

               DISPLAY v_comando
               --DISPLAY "COMANDO: ", v_comando
               --CALL fn_mensaje("COMANDO", v_comando, "about")
               CALL s.setAttribute("exec",v_comando)
               CALL s.setAttribute("image","icono_safre.png")
               
            END IF
         END IF
      END FOR
      
      -- Grupo hijo del StartMenu
      IF li_contador = 0 THEN
         --DISPLAY sm1.getAttribute("name")
         IF li_tipo = 0 THEN           
            LET s = sm1.createChild("StartMenuGroup")
            CALL s.setAttribute("text",lc_opcion || " »")
            CALL s.setAttribute("name",lc_menu)
         ELSE
            LET s = sm1.createChild("StartMenuCommand")
            CALL s.setAttribute("text",lc_opcion)
            CALL s.setAttribute("name",lc_menu)
            LET v_comando="cd "||lc_ruta CLIPPED||"; fglrun "
                                   ||lc_programa CLIPPED||".42r "||p_cod_usu
                                   ||" 0 "||" '"||lc_opcion||"' "||p_rfc||" "||p_cve_ent_financiera

            DISPLAY v_comando
            CALL s.setAttribute("exec",v_comando)
            CALL s.setAttribute("image","icono_safre.png")
         END IF

      END IF
      --DISPLAY "Ruta ", lc_ruta CLIPPED||"; fglrun "||lc_programa CLIPPED||".42r "||
      --                 p_cod_usu||" 0 "||" '"||lc_opcion||"'"
   END FOREACH
   
   CALL UI.INTERFACE.REFRESH()
 
END FUNCTION

{=========================================================================
Clave:
Nombre: f_usuario_desc
Fecha Creacion: 19/12/2011
Autor: Antonio Gomez
Narrativa Del Proceso que Realiza:
Obtiene la descripcion del usuario de acuerdo con la 
clave de usuario que se reciba

Parametros: 
   Entrada: p_usuario_cod - clave del usuario
   Salida:  usuario_desc  - descripcion del usuario
Registro de Modificaciones:
Autor Fecha Descrip. cambio
=========================================================================}
FUNCTION f_usuario_desc(p_usuario_cod)
   DEFINE p_usuario_cod    CHAR(13)
   DEFINE v_usuario_desc   STRING
   DEFINE v_nombre         CHAR(50)
   DEFINE v_paterno        CHAR(50)
   DEFINE v_materno        CHAR(50)
 
   -- se consulta la descripcion del usuario
   SELECT nombre,
          primer_apellido,
          segundo_apellido  
     INTO v_nombre,
          v_paterno,
          v_materno
     FROM safre_viv:cat_usuario_ef
    WHERE rfc = p_usuario_cod

   LET v_usuario_desc = v_nombre CLIPPED, " ",
                        v_paterno CLIPPED, " ",
                        v_materno CLIPPED
   -- si no existe el usuario
   IF ( v_usuario_desc IS NULL ) THEN
      -- se indica que no se encontro al usuario
      LET v_usuario_desc = "Usuario no reconocido"
   END IF
   
   -- se devuelve el resultado de la consulta
   RETURN v_usuario_desc
END FUNCTION

PRIVATE FUNCTION fn_valida_acceso(p_usuario, p_contrasena)
   DEFINE p_usuario                       VARCHAR(20) -- login del usuario
   DEFINE p_contrasena                    VARCHAR(50) -- contrasena del usuario

   DEFINE v_resultado_ldap                INTEGER
   DEFINE v_resultado_safre               SMALLINT
   DEFINE v_resultado_validacion          SMALLINT

   DEFINE v_host_ip                       VARCHAR(20)
   DEFINE v_id_session_web                VARCHAR(60)

   DEFINE v_sp_registra_bitacora_acceso   STRING

   #Se obtienen los valores del cliente que intenta conectarse
   LET v_host_ip = fgl_getenv("FGL_WEBSERVER_REMOTE_ADDR" )
   LET v_id_session_web = fgl_getenv("FGL_VMPROXY_SESSION_ID" )

   #Primero se valida que exista el usuario en Safre
   CALL fn_valida_usuario_safre(p_usuario) RETURNING v_resultado_safre
   IF v_resultado_safre = '1' THEN   #Valida Usuario activo en SAFRE
      #Se valida la firma en LDAP
      CALL fn_valida_usuario_ldap(p_usuario, p_contrasena) RETURNING v_resultado_ldap
      IF v_resultado_ldap = 0 THEN  #Firma en LDAP exitosa
         IF fn_usuario_sesion(p_usuario) THEN
            CALL fn_mensaje("SAFRE-Vivienda","El usuario ya esta conectado en \n el sistema SAFRE Vivienda-SACI","stop")
            LET v_resultado_validacion = USUARIO_CONECTADO
         ELSE
            IF fn_usuario_inactividad(p_usuario) THEN
               CALL fn_mensaje("SAFRE-Vivienda","El usuario ha excedido el periodo de \n inactividad del sistema SAFRE Vivienda-SACI","stop")
               LET v_resultado_validacion = USUARIO_CADUCADO
            ELSE
               LET v_resultado_validacion = USUARIO_VALIDO
            END IF
         END IF
      ELSE  #Firma en LDAP con error
         LET v_resultado_ldap = 2
         IF v_resultado_ldap = -1 THEN    #El servidor LDAP no esta disponible
            CALL fn_mensaje("Validación por LDAP","El servidor de seguridad no está disponible","stop")
            LET v_resultado_validacion = LDAP_NO_DISPONIBLE
         ELSE  #El servidor contesto que la firma esta mal
            IF v_resultado_ldap = 1 THEN  #El usuario no existe en LDAP
               CALL fn_mensaje("Validación por LDAP","El usuario no ha sido identificado en \n el servidor de seguridad","stop")
               LET v_resultado_validacion = USUARIO_LDAP_NO_EXISTE
            ELSE  #Contraseña incorrecta en LDAP
               CALL fn_mensaje("Validación por LDAP","Contraseña incorrecta","stop")
               LET v_resultado_validacion = PSW_ERRONEO
            END IF #FIN valida firma LDAP
         END IF   #FIN valida servidor LDAP disponible
      END IF   #FIN Valida firma LDAP
   ELSE  #Error en Usuario Safre
      CASE v_resultado_safre
         WHEN '0' #Si v_resultado_safre = 0 el usuario esta inactivo
            CALL fn_mensaje("SAFRE-Vivienda","El usuario está inactivo en \n el sistema SAFRE Vivienda-SACI","stop")
            LET v_resultado_validacion = USUARIO_SFR_INACTIVO

         WHEN '2'
            CALL fn_mensaje("SAFRE-Vivienda","El usuario está dado de baja en \n el sistema SAFRE Vivienda-SACI","stop")
            LET v_resultado_validacion = USUARIO_SFR_BAJA

         WHEN '3'
            CALL fn_mensaje("SAFRE-Vivienda","El usuario está bloqueado en \n el sistema SAFRE Vivienda-SACI","stop")
            LET v_resultado_validacion = USUARIO_SFR_BOQUEADO

         WHEN '99' #SI el usuario no existe en Safre
            CALL fn_mensaje("SAFRE-Vivienda","El usuario no ha sido identificado en \n el sistema SAFRE Vivienda-SACI","stop")
            LET v_resultado_validacion = USUARIO_SFR_NO_EXISTE

      END CASE
      
   END IF   #FIN Valida Usuario en SAFRE

   #Se registra en bitacora el acceso
   LET v_sp_registra_bitacora_acceso = "CALL safre_viv:sp_registra_bitacora_acceso(?,?,?,?)"
   PREPARE exe_sp_registra_bitacora FROM v_sp_registra_bitacora_acceso
   EXECUTE exe_sp_registra_bitacora USING p_usuario,
                                          v_resultado_validacion,
                                          v_host_ip,
                                          v_id_session_web
   
   RETURN v_resultado_validacion
END FUNCTION

{=========================================================================
Clave:
Nombre: fn_valida_usuario_safre
Fecha Creacion: 05/07/2012
Autor: Jaime Galeno
Narrativa Del Proceso que Realiza:
Valida el usuario en el sistema de safre

Parametros: 
   Entrada: p_usuario_cod - clave del usuario
   Salida:  v_indicador      - Si el indicador = 0 el usuario esta inactivo
                               Si el indicador = 1 el usuario esta activo
                               Si el indicador = 2 el usuario no existe en SAFRE
Registro de Modificaciones:
Autor             Fecha                         Descrip. cambio
=========================================================================}
FUNCTION fn_valida_usuario_safre(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_indicador      SMALLINT  -- Indicador del usuario

   -- se consulta el estado del usuario
   SELECT ind_activo
     INTO v_indicador
     FROM safre_viv:seg_usuario
    WHERE usuario_cod = p_usuario_cod
    
   #Si el indicador es nulo significa que el usuario_cod no esta registrado y se asignara indicador = 3
   #Si el indicador = 2 el usuario esta bloqueado
   #Si el indicador = 1 el usuario esta activo
   #Si el indicador = 0 el usuario esta inactivo
   IF SQLCA.sqlcode = 100 THEN
      LET v_indicador = 99         #99 significa que el usuario no existe en SAFRE
   END IF
   
   -- se devuelve el resultado de la consulta
   DISPLAY "INDICADOR  ",v_indicador
   RETURN v_indicador
END FUNCTION

FUNCTION fn_usuario_sesion(p_usuario)

   DEFINE p_usuario     CHAR(20)
   DEFINE v_sesion      CHAR(60)
   DEFINE v_bandera     SMALLINT
   DEFINE v_comando     STRING
   DEFINE ch            base.Channel
   DEFINE v_cadena      STRING
   DEFINE v_archivo_sal STRING
   DEFINE v_archivo_log STRING
   

   LET v_bandera = FALSE
   DECLARE cur_sesion_usuario CURSOR FOR SELECT id_session_web
                                           FROM safre_viv:seg_bitacora_acceso
                                          WHERE usuario_cod = p_usuario
                                            AND estado_acceso = 1
                                          ORDER BY f_acceso DESC, h_acceso DESC 

   LET v_archivo_sal = v_sesion CLIPPED,".sal"
   LET v_archivo_log = v_sesion CLIPPED,".log"
                                          
   LET ch = base.Channel.create()
   
   FOREACH cur_sesion_usuario INTO v_sesion
      LET v_comando = "gasadmin -x ",v_sesion, " -d fastcgidispatch ",
                      "1>",v_archivo_sal,
                      "2>",v_archivo_log
      RUN v_comando

      CALL ch.openFile(v_archivo_log,"r")

      LET v_cadena = ch.readLine()
      LET v_bandera = v_cadena.getIndexOf("OK",20)
      CALL ch.close()
      
      EXIT FOREACH
   END FOREACH

   CLOSE cur_sesion_usuario
   FREE cur_sesion_usuario
  
   RETURN v_bandera
END FUNCTION

FUNCTION fn_usuario_inactividad(p_usuario)
   DEFINE p_usuario     CHAR(20)
   DEFINE v_bandera     SMALLINT
   DEFINE v_vigencia    SMALLINT
   DEFINE v_diferencia  INTEGER
   DEFINE v_dias_act    INTEGER
   DEFINE v_estatus_usu_antrior SMALLINT

   LET v_vigencia = 60

   DECLARE cur_usr_inactivo CURSOR FOR SELECT TODAY - f_acceso
                                         FROM safre_viv:seg_bitacora_acceso
                                        WHERE usuario_cod = p_usuario
                                          AND estado_acceso = 1
                                        ORDER BY f_acceso DESC, h_acceso DESC 
   
   FOREACH cur_usr_inactivo INTO v_diferencia
      IF v_diferencia > v_vigencia THEN
         SELECT TODAY - f_modifica
           INTO v_dias_act
           FROM safre_viv:seg_usuario
          WHERE usuario_cod = p_usuario
          
         IF v_dias_act > v_vigencia THEN
            LET v_bandera = TRUE
            SELECT ind_activo
              INTO v_estatus_usu_antrior
              FROM safre_viv:seg_usuario
             WHERE usuario_cod = p_usuario
            
            UPDATE safre_viv:seg_usuario
               SET ind_activo = 0,
                   f_modifica = TODAY,
                   usuario_modifica = p_usuario
             WHERE usuario_cod = p_usuario

            CALL fn_registra_bitacora_usuario(p_usuario,             # Usuario al que se le realiza la modificación
                                              "MODIFICACIÓN DE ESTATUS DE USUARIO", # Descripción de la modificación
                                              0,                 # Valor actual - NULL por ser nuevo registro de usuario
                                              v_estatus_usu_antrior)                # Valor anterior - usuario nuevo

         ELSE
            LET v_bandera = FALSE
         END IF
      ELSE
         LET v_bandera = FALSE
      END IF
      EXIT FOREACH
   END FOREACH

   CLOSE cur_usr_inactivo
   FREE cur_usr_inactivo

   
   RETURN v_bandera
END FUNCTION

FUNCTION fn_registra_bitacora_usuario(p_registro_bitacora_usu)
DEFINE p_registro_bitacora_usu RECORD
         v_usuario_mod         LIKE seg_bitacora_catalogo_usuario.usuario_mod,
         v_desc_evento         LIKE seg_bitacora_catalogo_usuario.desc_evento,
         v_valor_actual        LIKE seg_bitacora_catalogo_usuario.valor_actual,
         v_valor_anterior      LIKE seg_bitacora_catalogo_usuario.valor_anterior         
       END RECORD,
       v_f_modifica          LIKE seg_bitacora_catalogo_usuario.f_modifica,
       v_hora_mod            LIKE seg_bitacora_catalogo_usuario.hora_mod,
       v_usuario_log         LIKE seg_bitacora_catalogo_usuario.usuario_log

   DEFINE QryTxt            STRING

   LET v_f_modifica  = TODAY
   LET v_hora_mod    = CURRENT HOUR TO SECOND
   LET v_usuario_log = p_registro_bitacora_usu.v_usuario_mod

   LET QryTxt = "\n INSERT INTO safre_viv:seg_bitacora_catalogo_usuario",
                "\n (usuario_mod,",
                "\n  desc_evento,",
                "\n  valor_actual,",
                "\n  valor_anterior,",
                "\n  f_modifica,",
                "\n  hora_mod,",
                "\n  usuario_log)",
                "\n VALUES(?,?,?,?,?,?,?)"
   PREPARE prp_registra_bitacora_usuario FROM QryTxt

   EXECUTE prp_registra_bitacora_usuario USING p_registro_bitacora_usu.v_usuario_mod,
                                               p_registro_bitacora_usu.v_desc_evento,
                                               p_registro_bitacora_usu.v_valor_actual,
                                               p_registro_bitacora_usu.v_valor_anterior,
                                               v_f_modifica,
                                               v_hora_mod,
                                               v_usuario_log
   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "Error en consulta con código: ",SQLCA.SQLCODE
      DISPLAY "Mensaje: ",SQLCA.SQLERRM
   END IF
END FUNCTION

FUNCTION fn_valida_usuario_ldap(p_id_usuario, p_password)
   DEFINE p_id_usuario          STRING
   DEFINE p_password            STRING
   DEFINE v_resultado           INTEGER

   LET v_resultado = com.efp.safre.client.ClientLDAP.validaAcceso(p_id_usuario, p_password)
   #La funcion regresa como resultado un entero con el siguiente codigo:
   
      #v_resultado = 0     El usuario y el password es correcto
      #v_resultado = 1     El usuario no existe
      #v_resultado = 2     El password es incorrecto
      #v_resultado = -1    Ocurrio un error en la comunicacion con el servidor
    DISPLAY "LDAP, RESULTADO:",v_resultado
   RETURN v_resultado
END FUNCTION

FUNCTION fn_obtiene_rfc(p_usuario_cod)
    DEFINE p_usuario_cod CHAR(20)
    DEFINE v_rfc CHAR(13)
    DEFINE v_cve_ent_financiera SMALLINT
    DEFINE v_ind_existe_usuario SMALLINT

    #se obtiene el usuario
    SELECT usref.rfc,usref.cve_ent_financiera,1
    INTO v_rfc,
         v_cve_ent_financiera,
         v_ind_existe_usuario
    FROM seg_usuario usr,
         seg_usuario_ef usref
    WHERE usr.usuario_cod=usref.usuario_cod
      AND usr.usuario_cod=p_usuario_cod
      AND usr.ind_activo=1

    RETURN v_rfc,v_cve_ent_financiera,v_ind_existe_usuario

END FUNCTION


