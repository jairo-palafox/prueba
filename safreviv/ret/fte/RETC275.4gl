--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIR005                                                                #
#Objetivo     => Programa lanzador del reverso de carga e integracion del archivo       #
#                de Posibles Duplicados Administradora Responsable POSDUP               #
#Fecha inicio => Abril 21, 2014                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

END GLOBALS

MAIN
DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion      SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo            STRING -- titulo de la ventana
       ,v_folio               LIKE glo_folio.folio
       ,v_s_cadena            STRING -- cadena de texto
       ,v_cbx_folios          ui.ComboBox -- combo de afores
       ,v_i_conArch           INTEGER
       ,v_i_indice            INTEGER
       ,v_r_glo_ctr_archivo   RECORD
          nombre_archivo        LIKE glo_ctr_archivo.nombre_archivo
          ,folio                LIKE afi_sol_posible_duplicado.folio
       END RECORD
       ,v_r_glo_cifras_archivo DYNAMIC ARRAY OF RECORD
          edo_solicitud        CHAR (50)
          ,tot_x_edo           INTEGER
          ,folio               LIKE afi_sol_posible_duplicado.folio
       END RECORD       
       ,v_proceso_desc        LIKE cat_proceso.proceso_desc
       ,v_opera_desc          LIKE cat_operacion.opera_desc

        ,f_ventana        ui.Window,   -- Define las propìedades de la Ventana
        f_forma          ui.Form     -- Define las propiedades de la forma


       -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_afi_pd_posdup -- Intereses en Transito ISSSTE
   LET g_opera_cod   = g_opera_cod_afi_pd_integracion  -- integracion

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'

  -- se abre la ventana que envia el reverso de la integracion de Posibles Duplicados
   OPEN WINDOW w_folio_integra WITH FORM "RETC2751"
   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   LET f_ventana = ui.Window.getCurrent()
   LET f_forma = f_ventana.getForm()   
   CALL f_forma.setElementHidden("Grp_Cifras", 1) --Oculta la Sección de Detalles
   CALL f_forma.setElementHidden("tbl_cifras", 1) --Oculta la Sección de Detalles
   

   -- se captura el folio
   INPUT
    v_folio
   WITHOUT DEFAULTS
   FROM
    cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio                  = NULL
         CALL DIALOG.setActionHidden("reverso",    1) --Se oculta el boton de Reverso

         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.nombre_archivo, g.folio
           FROM glo_folio g LEFT OUTER JOIN glo_ctr_archivo a
             ON a.folio = g.folio
          WHERE g.proceso_cod = g_proceso_cod
            AND g.status = 0 -- integrado
            AND g.folio IS NOT NULL
          ORDER BY g.folio desc

         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(
                 v_r_glo_ctr_archivo.folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         
         IF ( v_i_conArch < 1 ) THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente Integrados","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT

         IF ( v_folio IS NULL OR v_folio = -1) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         ELSE

             -- se llena el arreglo de cifras por estado de solicitud
             DECLARE cur_cifras CURSOR FOR
              SELECT aspd.edo_solicitud || ' - ' || cepd.desc_estado AS EstadoSol, COUNT(*), aspd.folio
                FROM afi_sol_posible_duplicado AS aspd INNER JOIN cat_edo_pos_dup AS cepd on aspd.edo_solicitud = cepd.cod_estado
               WHERE folio = v_folio
               GROUP BY EstadoSol, folio
               ORDER BY EstadoSol;
             LET v_i_indice = 1;
             FOREACH cur_cifras INTO v_r_glo_cifras_archivo[v_i_indice].*
                LET v_i_indice = v_i_indice + 1
             END FOREACH
             DISPLAY ARRAY v_r_glo_cifras_archivo TO tbl_cifras.*
             CALL f_forma.setElementHidden("Grp_Cifras", 0) --Se muestra la Sección de Detalles
             CALL f_forma.setElementHidden("tbl_cifras", 0) --Se muestra la Sección de Detalles
             CALL DIALOG.setActionHidden("reverso",    0) -- Se muestra el boton de Reverso
             CALL DIALOG.setActionHidden("accept",     1) -- Se oculta el boton de Aceptar
         END IF
      
       ON ACTION REVERSO

         IF ( v_folio IS NULL OR v_folio = -1) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         ELSE 
            CALL fn_Confirma_Reverso(v_folio, p_usuario_cod)
         END IF
      
         -- se invoca la ejecucion del stored procedure

         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_integra
END MAIN

{
======================================================================
Clave: 
Nombre: fn_afi_posdup_reverso_integracion
Fecha creacion: Abril 21, 2014
Autor: Ricardo Perez
Narrativa del proceso que realiza:
Ejecuta el reverso de la integracion del archivo de Posibles Duplicados Administradora Responsable POSDUP
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_afi_posdup_reverso_integracion(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_resultado       SMALLINT,
       r_bandera         SMALLINT,
       v_mensaje         STRING,
       v_ls_SqlQry       STRING


   -- se obtiene el nombre del archivo
  SELECT a.nombre_archivo
    INTO v_nombre_archivo
    FROM glo_ctr_archivo a
   WHERE a.folio = p_folio

   -- se obtiene el pid en base al folio seleccionado
   SELECT pid 
     INTO g_pid 
     FROM bat_ctr_proceso
    WHERE proceso_cod =  g_proceso_cod
      AND folio       =  p_folio
   DISPLAY "se llamo a la funcion de reverso"
   -- se verifica si se puede continuar con la operacion
   LET v_resultado = fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod)
   
   IF ( v_resultado = 0 ) THEN
      -- se invoca la ejecucion del programa lanzado. los parametros se envian 


       CALL STARTLOG (p_usuario_cod CLIPPED|| ".RECR005.log")

       DISPLAY "Ejecutando rutina de reverso..."
   
       -- se ejecuta el SP de reverso de integracion de los Intereses en Transito ISSSTE
       LET v_ls_SqlQry = "EXECUTE PROCEDURE sp_afi_reversa_integra_pos_dup(?)"
       PREPARE sid_reverso FROM v_ls_SqlQry
       EXECUTE sid_reverso USING p_folio
      
       IF ( r_bandera = 0 ) THEN
          DISPLAY v_mensaje
       ELSE
          DISPLAY "Ocurrió un error al reversar la carga e integración"
          DISPLAY "Código: ", r_bandera
          DISPLAY "Mensaje :", v_mensaje
          EXIT PROGRAM
       END IF
   
       CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bandera

       IF ( r_bandera = 0 ) THEN

          UPDATE glo_ctr_archivo
             SET estado = 1, folio = NULL -- cargado
           WHERE folio  = p_folio
             AND estado = 2; -- integrado
          
          -- se reversa la carga
          CALL fn_corrige_reg_carga_archivo(v_nombre_archivo)

          -- Reversa operacion de carga
          CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod_afi_pd_carga)
                                    RETURNING r_bandera
                                    
          DISPLAY "Operación lista para volver a generarse."
       ELSE
        -- Muestra el error ocurrido
          DISPLAY fn_recupera_inconsis_opera(r_bandera)
       END IF



{      
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/AFIR005 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"

                         DISPLAY v_s_comando
                         
       RUN v_s_comando
       CALL fn_mensaje("Atención","Se ha enviado el reverso de la Integración.\n"||
            "Puede revisar el avance del proceso en el monitor de ejecución de procesos"
            ,"information")
}
   ELSE
      CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   END IF
 
END FUNCTION

{
   Funcion : fn_corrige_reg_carga_archivo
   Fecha   : Abril 23, 2014
   Descrip : corrige datos adicionales de reverso de integracion
}
FUNCTION fn_corrige_reg_carga_archivo(p_nombre_archivo)
  DEFINE 
   p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   --    
   ,v_s_qry          STRING

   LET v_s_qry =
     "DELETE FROM glo_ctr_archivo"
    ,"\n WHERE nombre_archivo = ?"
    ,"\n   AND proceso_cod = ", g_proceso_cod
    ,"\n   AND estado = 1"
   --DISPLAY v_s_qry 
   PREPARE Prpr_LimpiaCtrArchvo FROM v_s_qry 
   EXECUTE Prpr_LimpiaCtrArchvo USING  p_nombre_archivo

END FUNCTION -- fn_corrige_reg_carga_archivo

FUNCTION fn_Confirma_Reverso(p_folio,p_usuario_cod)

DEFINE p_folio          DECIMAL(9,0), # folio
       p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_acep_rech      STRING,
       p_proceso        DECIMAL(9,0),
       v_s_qryTxt       STRING, -- guarda una sentencia SQL a ejecutar
       v_l_n_condicion  DECIMAL(1,0),
       v_i_indice       SMALLINT,
       v_l_r_int DYNAMIC ARRAY OF RECORD -- indice del arrego de archivos pendientes
           folio INTEGER
       END RECORD 

       LET v_l_r_int[1].folio = p_folio
       DISPLAY "En la funcion de confirmacion del reverso"
    OPEN WINDOW w_consulta WITH FORM "AFIR0052"
        DISPLAY ARRAY v_l_r_int TO tbl_paso.* 
            ON ACTION CANCEL
                EXIT DISPLAY 
            ON ACTION ACCEPT 
                CALL fn_afi_posdup_reverso_integracion(p_folio, p_usuario_cod)
                EXIT DISPLAY 
        END DISPLAY 
    CLOSE WINDOW w_consulta
    DISPLAY "Termino el despliegue de la forma"
    
END FUNCTION