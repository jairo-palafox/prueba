###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGP07                                                  #
#Objetivo          => Programa para captura de montos para devoluciones por   #
#                     entidad financiera                                      #
#Autor             => José Eduardo Ventura                                    #
#Fecha inicio      => 21 JUNIO 2016                                           #
###############################################################################
DATABASE safre_viv


   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod
   DEFINE v_qry                     STRING
   DEFINE v_cb_ef                   SMALLINT
   DEFINE w ui.Window
   DEFINE f ui.Form

MAIN

   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo                STRING   -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".OCGP07.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se invoca la función de consulta
   CALL fn_dev()

END MAIN

FUNCTION fn_dev()

   DEFINE v_monto_dev    DECIMAL(12,2)
   DEFINE v_monto_sol    DECIMAL(12,2)
   DEFINE v_dif          DECIMAL(12,2)
   DEFINE v_f_dev        DATE
   DEFINE cb             ui.ComboBox
   DEFINE a              SMALLINT
   DEFINE v_arma_lote    STRING
   DEFINE v_lote         CHAR(11)
   DEFINE v_sum_ap       DECIMAL(12,2)
   DEFINE v_sum_dev      DECIMAL(12,2)
   DEFINE v_bnd_verifica SMALLINT
   DEFINE v_bnd_salir    SMALLINT
   --DEFINE v_sum_tot   DECIMAL(12,2)
   DEFINE v_monto        DECIMAL(12,5)
   DEFINE v_fecha        DATE
   DEFINE v_ef           SMALLINT
   DEFINE bnd_ficha      SMALLINT
   
   DEFINE arr_entidad DYNAMIC ARRAY OF RECORD
      clave        SMALLINT,
      entidad_desc CHAR(50)
   END RECORD

   CLOSE WINDOW SCREEN
   OPEN WINDOW OCGP07 WITH FORM "OCGP071"
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()

   LET cb = ui.ComboBox.forName("v_cb_ef")

         LET v_qry = "SELECT cve_ent_financiera,
                          cve_ent_financiera||'-'||ent_financiera_desc
                     FROM cat_entidad_financiera
                    WHERE estado_ef = 10
                     order by cve_ent_financiera asc"

      PREPARE prp_entidad FROM v_qry
      DECLARE cur_entidad CURSOR FOR prp_entidad

      LET a = 1 
      FOREACH cur_entidad INTO arr_entidad[a].*
         CALL cb.addItem(arr_entidad[a].clave, arr_entidad[a].entidad_desc)
         LET a = a+1
      END FOREACH

      CALL arr_entidad.deleteElement(a)

   INPUT BY NAME v_cb_ef,
                 v_monto_dev,
                 v_f_dev ATTRIBUTES (UNBUFFERED)
{
   BEFORE INPUT

      LET v_qry = "SELECT cve_ent_financiera,
                          cve_ent_financiera||'-'||ent_financiera_desc
                     FROM cat_entidad_financiera
                    WHERE estado_ef = 10
                     order by cve_ent_financiera asc"

      PREPARE prp_entidad FROM v_qry
      DECLARE cur_entidad CURSOR FOR prp_entidad

      LET a = 1 
      FOREACH cur_entidad INTO arr_entidad[a].*
         CALL cb.addItem(arr_entidad[a].clave, arr_entidad[a].entidad_desc)
         LET a = a+1
      END FOREACH

      CALL arr_entidad.deleteElement(a)
}
   ON ACTION ACCEPT
      DISPLAY v_cb_ef
      IF (v_monto_dev IS NULL) OR
         (v_cb_ef     IS NULL) OR
         (v_f_dev     IS NULL) THEN
         CALL fn_mensaje("Alerta", "Se deben ingresar todos los parámetros solicitados para continuar","stop")
          LET bnd_ficha = 2
      ELSE

         SELECT cve_ent_financiera,
                monto_devolucion,
                f_devolucion
           INTO v_ef,
                v_monto,
                v_fecha
           FROM ocg_devolucion_ef
          WHERE estado IN (100,110,150)
            AND cve_ent_financiera = v_cb_ef
            AND f_devolucion = v_f_dev
            AND monto_devolucion = v_monto_dev

         IF (v_cb_ef = v_ef) AND
            (v_f_dev = v_fecha) AND
            (v_monto_dev = v_monto) THEN
            LET bnd_ficha = 1
        ELSE
           LET bnd_ficha = 0
        END IF

   END IF

      IF bnd_ficha = 0 THEN
         --DISPLAY "fecha :",v_f_dev

         LET v_arma_lote = v_f_dev USING "ddmmyyyy"
         LET v_arma_lote = v_arma_lote,v_cb_ef USING "&&&"
         LET v_lote      = v_arma_lote

        -- DISPLAY "combo :",v_cb_ef
{
         SELECT SUM ( importe_subsec_devuelto),SUM (importe_ocg_devuelto)
           INTO v_sum_ap,v_sum_dev
           FROM ocg_devolucion
          WHERE f_deposito = v_f_dev
            AND cve_ent_financiera = v_cb_ef

         LET v_monto_sol = (v_sum_ap + v_sum_dev)

         IF v_monto_sol IS NULL THEN
            LET v_monto_sol = 0
         END IF

         LET v_dif = (v_monto_sol - v_monto_dev)

         DISPLAY BY NAME v_monto_sol
         DISPLAY BY NAME v_dif

         LET v_bnd_verifica = 1

         MENU
            ON ACTION ACCEPT

            IF v_bnd_verifica = 1 THEN
}
               INSERT INTO ocg_devolucion_ef
                   VALUES (v_lote,
                           v_cb_ef,
                           v_monto_dev,
                           "",
                           v_f_dev,
                           "",
                           TODAY,
                           100,
                           p_usuario_cod)

            CALL fn_mensaje("Alerta", "Monto almacenado de forma correcta en espera de autorización","stop")
            LET v_bnd_salir = 1
          --  END IF
          --     EXIT MENU

            --ON ACTION CANCEL
              -- LET v_bnd_salir = 0
               --EXIT MENU
        -- END MENU
      ELSE
         IF bnd_ficha < 2 THEN
            CALL fn_mensaje("Alerta", "Ya existe ficha con los datos ingresados ","stop")
         END IF
      END IF

      IF v_bnd_salir = 1 THEN
         EXIT INPUT
      ELSE
         CONTINUE INPUT
      END IF

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   CLOSE WINDOW OCGP07
END FUNCTION
