--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETF310                                                                #
#OBJETIVO     => Consulta de nss proceso judicial                                       #
#Fecha inicio => 30 Diciembre 2013                                                      #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE  g_usuario_cod       LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   ,g_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
   ,g_s_titulo          STRING   -- titulo de la ventana 
   ,g_proceso_cod       INT

   ,v_nombre    LIKE afi_derechohabiente.nombre_imss  -- nombre completo
   ,v_rfc       LIKE afi_derechohabiente.rfc
   ,v_curp      LIKE afi_derechohabiente.curp
   ,v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente -- ID Derechoabiente
   ,pos  INTEGER
   ,pos2  INTEGER
DEFINE l_record2 DYNAMIC ARRAY OF RECORD
    id_registro_a        INTEGER,
    nss                  CHAR(11),
    marca                SMALLINT,
    descripcion_marca    CHAR(50),
    f_inicio             DATE,
    h_inicio             DATETIME HOUR TO SECOND,
    n_referencia         INTEGER,
    usuario_marca        CHAR(8)
END RECORD

DEFINE l_record3 DYNAMIC ARRAY OF RECORD
    id_registro_h        INTEGER,
    nss                  CHAR(11),
    marca                SMALLINT,
    descripcion_marca    CHAR(50),
    f_inicio             DATE,
    h_inicio             DATETIME HOUR TO SECOND,
    f_fin                DATE,
    estado_marca         CHAR(25),
    rch_cod              CHAR(25),
    marca_causa          SMALLINT,
    causa_desc           CHAR(50),
    f_causa              DATE,
    n_referencia         INTEGER,
    usuario_marca        CHAR(8),
    usuario_desmarca     CHAR(8)
END RECORD
DEFINE l_record3com DYNAMIC ARRAY OF RECORD
    folio                LIKE sfr_marca_activa.folio
END RECORD

END GLOBALS
{
======================================================================
Clave: 
Nombre: main
Fecha creacion: Diciembre 30, 2013
Autor: Eneas Armas, EFP
Narrativa del proceso que realiza:
Abre la ventana de captura de busqueda de NSS para la marca y desmarca 
proceso judicial

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
MAIN
DEFINE v_forma ui.Form
DEFINE v_nss    LIKE afi_derechohabiente.nss          -- numero de seguridad social
   ,v_sql       STRING
   ,v_resultado VARCHAR (100)
   ,v_marca_a            LIKE sfr_marca_activa.marca                 -- 
   ,v_marca_d            LIKE sfr_marca_activa.marca                 -- 
   
   -- se obtienen los parametros de ejecucion
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_tipo_ejecucion = ARG_VAL(2)
   LET g_s_titulo       = ARG_VAL(3)
   LET g_proceso_cod    = g_proceso_cod_tramite_juridico_desmarca

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   
   -- se abre la ventana de marca
   OPEN WINDOW w_marca WITH FORM "RETF3101"
   INPUT BY NAME v_nss
      WITHOUT DEFAULTS ATTRIBUTES ( UNBUFFERED )
      BEFORE INPUT

      -- cancelar
      ON ACTION CANCEL
         EXIT INPUT
         
      ON ACTION ACCEPT--búsqueda por nss para procesar la marca
         --validaciones
         IF ( v_nss IS NULL ) THEN
            CALL fn_mensaje("Atención","El NSS no puede estar en blanco","stop")
            CONTINUE INPUT
         END IF
         IF ( LENGTH(v_nss) <> 11 ) THEN
            CALL fn_mensaje("Atención","El NSS debe tener 11 caracteres","stop")
            CONTINUE INPUT
         END IF

         --se busca el nss y se obtinen datos generales
         SELECT id_derechohabiente,  nombre_imss, rfc,  curp
         INTO v_id_derechohabiente,v_nombre,    v_rfc,v_curp
         FROM afi_derechohabiente
         WHERE NSS = v_nss

         IF v_id_derechohabiente IS NULL THEN
            CALL fn_mensaje("Atención","No se encuentra el NSS","stop")
            CONTINUE INPUT
         END IF
         
         --se busca en las marcas activas
         LET v_marca_a = NULL
         LET v_marca_d = NULL

         SELECT FIRST 1 marca
         INTO   v_marca_a
         FROM   sfr_marca_activa
         WHERE  marca IN (593,594,595,596,597,591,822,814)  -- Se solicito incluir la 591 2017-09-25
           AND  id_derechohabiente = v_id_derechohabiente

         --busca en las marcas historicas
         LET v_sql =  "   SELECT FIRST 1 marca,n_referencia,folio,f_inicio "
                     ,"\n FROM sfr_marca_historica                         "
                     ,"\n WHERE marca IN (591,593,594,595,596,597,822,814)         "
                     ,"\n AND id_derechohabiente = ?                       "
--                     ,"\n AND proceso_desmarca = ?                         "
         PREPARE sid_marcahist FROM v_sql
         EXECUTE sid_marcahist
         USING v_id_derechohabiente --,g_proceso_cod
         INTO v_marca_d

         IF NOT v_marca_a IS NULL OR NOT v_marca_d IS NULL THEN
            --se abre la ventana de detalle de marca
            CALL fn_detalle()
         ELSE 
            IF fn_ventana_confirma("Confirma","No se tiene marcas para el NSS ¿Desea registrar una marca?","question") THEN
               CALL fn_detalle()
            END IF
         END IF
         
   END INPUT

   CLOSE WINDOW w_marca
END MAIN
{
======================================================================
Clave: 
Nombre: main
Fecha creacion: Enero 09, 2014
Autor: Eneas Armas, EFP
Narrativa del proceso que realiza:
Abre la ventana para modificar la marca y desmarca del
proceso judicial

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_detalle()
   DEFINE v_sql   STRING
   ,v_resultado   VARCHAR (100)
   ,v_cursor_m    INTEGER
   ,v_cursor_d    INTEGER
   ,v_referencia  DECIMAL(9,0)
   ,v_cbx_marcas  ui.ComboBox -- combo de marcas
   ,v_marcas      SMALLINT
   ,v_r_sfr_marca RECORD LIKE sfr_marca.*
   ,v_s_cadena    STRING
   ,v_pos_combo   INTEGER
   ,v_mensaje     STRING 
   
   -- se abre la ventana detalle de marca
   OPEN WINDOW w_det_marca WITH FORM "RETF3102"
   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY l_record2 TO tb2.* ATTRIBUTES(COUNT=l_record2.getLength())
         BEFORE ROW
            LET v_cursor_m = arr_curr()
         ON ACTION desmarcar
            --validaciones
            IF v_cursor_m IS NULL OR v_cursor_m = 0 THEN
               CALL fn_mensaje("Atención","Falta seleccionar registro a desmarcar","stop")
               CONTINUE DIALOG
            END IF
            IF NOT fn_ventana_confirma("Confirma","confirma que desea desmarcar este nss.","question") THEN
               CONTINUE DIALOG
            END IF
            
            -- se desmarca la solicitud
            LET v_sql = "\nEXECUTE FUNCTION fn_desmarca_cuenta(",
                        "\n",v_id_derechohabiente, ",",   --id derechohabiente
                        "\n ",l_record2[v_cursor_m].marca,",",                -- marca de disposicion
                        "\n",l_record2[v_cursor_m].n_referencia,",",  -- identificador de registro de archivo o lote
                        "\n 40,",                         -- estado marca / rechazo validacion
                        "\n ",l_record2[v_cursor_m].marca,",",                -- marca causa / rechazo por validacion
                        "\n'",g_usuario_cod CLIPPED, "',",-- usuario
                        "\n",g_proceso_cod, ")"           --´proceso cod

            DISPLAY "Desmarcar >", v_sql, "<"
                        -- se prepara y ejecuta la desmarcar
            PREPARE sid_desmarca FROM v_sql
            EXECUTE sid_desmarca INTO v_resultado

            DISPLAY "Resultado desmarca >", v_resultado, "<"

            IF v_resultado = 0 THEN
               --CALL DIALOG.setActionActive("desmarcar" , 0)
               CALL fn_mensaje("Atención","Se ha desmarcado el registro","information")
            ELSE 
               CALL fn_mensaje("Atención","No se puede desmarcar el registro","stop")
            END IF
            CALL fn_Ini_Detalle()
      END DISPLAY

      DISPLAY ARRAY l_record3 TO tb1.* ATTRIBUTES(COUNT=l_record3.getLength())
         BEFORE ROW
            LET v_cursor_d = arr_curr()
         ON ACTION marcar

            IF v_cursor_d IS NULL OR v_cursor_d = 0 THEN
               LET v_cursor_d = 1
               CALL fn_muestra_marcas() RETURNING v_marcas
               IF v_marcas <> 0 THEN 
                  LET l_record3[v_cursor_d].marca = v_marcas
                  LET l_record3com[v_cursor_d].folio = 0
               END IF 
            
               --CALL fn_mensaje("Atención","Falta seleccionar registro a marcar","stop")
               --CONTINUE DIALOG
            END IF
            --validaciones
            LET v_mensaje = "Se marcará con la clave ",l_record3[v_cursor_d].marca, ", si desea marcar la cuenta con otra clave haga clic en cancelar."
            IF NOT fn_ventana_confirma("Confirma",v_mensaje,"question") THEN
               CALL fn_muestra_marcas() RETURNING v_marcas
               IF v_marcas <> 0 THEN 
                  LET l_record3[v_cursor_d].marca = v_marcas
                  LET l_record3com[v_cursor_d].folio = 0
               ELSE 
                  CONTINUE DIALOG 
               END IF 
            END IF

            LET v_sql = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
            -- obtiene el maximo n_referencia para marcar, el usuario solicitó que se 
            -- pueda marcar y desmarcar las veces que fueran necearias por dia
            SELECT NVL(MAX(n_referencia),0)
              INTO v_referencia
              FROM sfr_marca_historica
             WHERE id_derechohabiente = v_id_derechohabiente
               AND marca = l_record3[v_cursor_d].marca

            LET v_referencia = v_referencia + 1                             
            DISPLAY "Datos para dar de alta la marca:"
            DISPLAY " Id derechohabiente >",v_id_derechohabiente
            DISPLAY " Marca              >", l_record3[v_cursor_d].marca
            DISPLAY " Referencia         >", v_referencia
            DISPLAY " Folio              >", l_record3com[v_cursor_d].folio
            DISPLAY " Usuario            >", g_usuario_cod
            DISPLAY " Proceso            >", g_proceso_cod

            -- se prepara y ejecuta la funcion de marcaje    		                 
            PREPARE stm_marcaje FROM v_sql
            EXECUTE stm_marcaje USING v_id_derechohabiente,
                                      l_record3[v_cursor_d].marca       ,
                                      v_referencia      ,
                                      l_record3com[v_cursor_d].folio             ,
                                      "0"      ,
                                      "0"    ,
                                      "0"       ,
                                      ""       ,
                                      g_usuario_cod           ,
                                      g_proceso_cod 
                                INTO v_resultado

            DISPLAY "Marcar v_resultado >", v_resultado, "< id_derchohabiente, marca, referencia, folio, usuario, proceso >", 
                     v_id_derechohabiente, "< >",
                     l_record3[v_cursor_d].marca       , "< >",
                     v_referencia      , "< >",
                     l_record3com[v_cursor_d].folio             , "< >",
                     g_usuario_cod           , "< >",
                     g_proceso_cod, "< "
            IF v_resultado = 0 THEN
               --CALL DIALOG.setActionActive("desmarcar" , 0)
               CALL fn_mensaje("Atención","Se ha marcado el registro","information")
            ELSE 
               CALL fn_mensaje("Atención","No se puede marcar el registro","stop")
            END IF
            --- Se cambia el llamado a la funcion, en lugar de utiliazr la funcion sp_reversa_desmarca, se utiliza la funcion fn_marca_cuenta
            --- segun el requerimiento PRODINF-126            
            -- se remarcar la cuenta
            --LET v_sql = "EXECUTE PROCEDURE sp_reversa_desmarca(?,?,?,?)"
            --PREPARE sid_marcacuenta FROM v_sql
            
            --EXECUTE sid_marcacuenta  
            --USING v_id_derechohabiente                -- id_derechohabiente
            --      ,l_record3[v_cursor_d].marca        -- marca_entra
            --      ,l_record3[v_cursor_d].n_referencia -- n_referencia
            --      ,l_record3com[v_cursor_d].folio     -- folio

            
            --CALL DIALOG.setActionActive("marcar" , 0)
            --CALL fn_mensaje("Atención","Se ha marcado el registro","information")
            CALL fn_Ini_Detalle()
      END DISPLAY

      BEFORE DIALOG
         DISPLAY BY NAME v_nombre,v_rfc,v_curp
         CALL fn_Ini_Detalle()
         IF pos = 1 THEN
            NEXT FIELD id_registro_h
         END IF
      ON ACTION CANCEL
         CALL l_record2.clear()
         CALL l_record3.clear()
         CLEAR FORM
         EXIT DIALOG
   END DIALOG
   CLOSE WINDOW w_det_marca
END FUNCTION
{
======================================================================
Clave: 
Nombre: main
Fecha creacion: Enero 09, 2014
Autor: Eneas Armas, EFP
Narrativa del proceso que realiza:
llena los listados para modificar la marca y desmarca del
proceso judicial

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_Ini_Detalle()
   DEFINE v_sql   STRING
      
   CALL l_record2.clear()
   CALL l_record3.clear()
   --se busca las marcas actuales
   LET v_sql =
       " SELECT '', c.nss, a.marca, b.descripcion_marca,            "
      ,"           a.f_inicio, a.h_inicio,                          "
      ,"           a.n_referencia, a.usuario_marca                  "
      ," FROM sfr_marca_activa a, sfr_marca b, afi_derechohabiente c"
      ," WHERE a.id_derechohabiente = ?                             "
      ,"    AND a.marca IN (591,593,594,595,596,597,822,814)                "
      ,"    AND a.id_derechohabiente = c.id_derechohabiente         " 
      ,"    AND a.marca = b.marca                                   "
      ," ORDER BY f_inicio DESC, marca, nss                         "

   PREPARE qry_act FROM v_sql 
   DECLARE cursor_a CURSOR FOR qry_act 

   LET pos = 1

   FOREACH cursor_a USING v_id_derechohabiente INTO l_record2[pos].*
       LET l_record2[pos].id_registro_a = pos
       LET pos                          = pos + 1
   END FOREACH

   DISPLAY pos - 1 TO total_registro_a

   IF l_record2[l_record2.getlength()].nss IS NULL THEN
       CALL l_record2.deleteelement(l_record2.getlength())
   END IF

   --se busca las marcas históricas
   LET v_sql = 
       " SELECT '', c.nss, a.marca, g.descripcion_marca,            "
      ,"       a.f_inicio, a.h_inicio, a.f_fin,                     "
      ,"       e.estado_marca_desc, f.rch_desc, a.marca_causa,      "
      ,"       d.descripcion_marca, a.f_marca_causa, a.n_referencia,"
      ,"       a.usuario_marca, a.usuario_desmarca , a.folio        " 
      ," FROM sfr_marca_historica a, afi_derechohabiente c,         "
      ," OUTER sfr_marca g, OUTER sfr_marca d,                      "
      ," OUTER cat_estado_marca e, OUTER cat_rch_marca f            "
      ," WHERE a.id_derechohabiente = ?                             "
      ,"    AND a.marca IN (591,593,594,595,596,597,822,814)        "
--      ,"    AND proceso_desmarca = ?                              "
      ,"    AND a.id_derechohabiente = c.id_derechohabiente         "
      ,"    AND a.marca = g.marca                                   "
      ,"    AND a.marca_causa = d.marca                             "
      ,"    AND a.f_fin IS NOT NULL                                 "
      ,"    AND a.estado_marca = e.estado_marca                     "
      ,"    AND a.rch_cod = f.rch_cod                               "
      ," ORDER BY f_inicio desc, marca, nss                         "

      PREPARE qry_his FROM v_sql
   DECLARE cursor_h CURSOR FOR qry_his

   LET pos2 = 1

   FOREACH cursor_h USING v_id_derechohabiente --,g_proceso_cod
   INTO l_record3[pos2].*,l_record3com[pos2].*
       LET l_record3[pos2].id_registro_h = pos2

       IF l_record3[pos2].estado_marca LIKE 'IMPROCEDENTE%' OR l_record3[pos2].estado_marca LIKE "RECHAZO%" THEN
           IF l_record3[pos2].rch_cod = 'PROCEDENTE' THEN
               LET l_record3[pos2].rch_cod = 'IMPROCEDENTE'
           END IF
       END IF

       LET pos2 = pos2 + 1
   END FOREACH

   DISPLAY pos2 - 1 TO  total_registro_h

   IF l_record3[l_record3.getlength()].nss IS NULL THEN
       CALL l_record3.deleteelement(l_record3.getlength())
   END IF
END FUNCTION 

FUNCTION fn_muestra_marcas()

DEFINE   
    v_cbx_marcas ui.ComboBox -- combo de marcas
   ,v_marcas       SMALLINT
   ,v_r_sfr_marca  RECORD LIKE sfr_marca.*
   ,v_s_cadena     STRING
   ,v_pos_combo    INTEGER 

      OPEN WINDOW w_marcas WITH FORM "RETF3103"
         LET v_cbx_marcas = ui.ComboBox.forName("formonly.cmb_marcas")
         CALL v_cbx_marcas.clear()
         INPUT v_marcas
         FROM cmb_marcas
         ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

         BEFORE INPUT
              LET v_marcas = NULL
              -- se llena el arreglo de los estados de solicitud
              DECLARE cur_marcas CURSOR FOR
              SELECT A.marca ,              
                     A.descripcion_marca
              FROM   sfr_marca A
              WHERE  marca IN (591,593,594,595,596,597,822,814) 
              ORDER BY 2;


              FOREACH cur_marcas INTO v_r_sfr_marca.marca, v_r_sfr_marca.descripcion_marca
                 LET v_s_cadena = v_r_sfr_marca.marca, " - ", v_r_sfr_marca.descripcion_marca
                 CALL v_cbx_marcas.addItem(v_r_sfr_marca.marca, v_s_cadena)
              END FOREACH

              FREE cur_marcas
         ON ACTION ACCEPT 
            DISPLAY "Marca ", v_marcas
            EXIT INPUT 
         ON ACTION CANCEL
            LET v_marcas = 0
            EXIT INPUT 
         END INPUT 
      CLOSE WINDOW w_marcas

   RETURN v_marcas 

END FUNCTION
 

