################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 19/03/2020                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CTA                                                      #
#Programa          => CTAC015                                                  #
#Objetivo          => Servicio web para la publicacion del detalle de          #
#                     movimientos                                              #
#Fecha inicio      => MARZO 2020                                               #
################################################################################
IMPORT SECURITY
DATABASE safre_viv


PRIVATE DEFINE g_nss                VARCHAR(11),
               g_opcion             SMALLINT,
               g_plataforma         SMALLINT,
               g_ticket             VARCHAR(255),
               g_nom_reporte        STRING,
               g_ruta_lst           LIKE seg_modulo.ruta_listados,
               g_hoy                DATE
               
PRIVATE DEFINE v_trabajador         RECORD
               id_derechohabiente   DECIMAL(9,0),
               nss                  CHAR(11),
               rfc                  CHAR(13),
               curp                 CHAR(18),
               nombre               VARCHAR(40),
               a_paterno            VARCHAR(40),
               a_materno            VARCHAR(40)
END RECORD

PRIVATE DEFINE g_mov_aclara         DYNAMIC ARRAY OF RECORD
               v_bimestre           VARCHAR(7),
               v_reg_patronal       VARCHAR(11),
               v_nombre_patron      VARCHAR(160),
               v_f_pago             DATE,
               v_aportacion         DECIMAL(16,2),
               v_amortizacion       DECIMAL(16,2)
END RECORD


PRIVATE DEFINE g_datos_personales   RECORD
                v_nombre            VARCHAR(40),
                v_paterno           VARCHAR(40),
                v_materno           VARCHAR(40),
                v_nss               CHAR(11),
                v_curp              CHAR(18),
                v_rfc               CHAR(13)
END RECORD

PRIVATE DEFINE  g_periodo_busqueda  RECORD
                v_f_ini             DATE,
                v_f_fin             DATE
END RECORD

PRIVATE DEFINE  g_movimientos       DYNAMIC ARRAY OF RECORD
                v_f_aplicacion      DATE,
                v_cve_movimiento    CHAR(5),
                v_desc_concepto     STRING,
                v_tpo_movimiento    STRING,
                v_cargo             DECIMAL(16,2),
                v_abono             DECIMAL(16,2)
END RECORD

PRIVATE DEFINE  g_adicionales       RECORD
                v_aviso             VARCHAR(255),
                v_cadena_original   STRING,
                v_sello_digital     STRING
END RECORD

FUNCTION fn_genera_detalle_movimientos(p_nss,p_opcion,p_f_inicio,p_f_fin,p_plataforma,p_ticket)
   DEFINE   p_nss                VARCHAR(11),
            p_opcion             SMALLINT,
            p_f_inicio           DATE,
            p_f_fin              DATE,
            p_plataforma         SMALLINT,
            p_ticket             VARCHAR(255)

CONSTANT c_edo_campania_activa SMALLINT = 1
CONSTANT c_ind_baja SMALLINT = 0

   LET g_nss                      = p_nss
   LET g_opcion                   = p_opcion
   LET g_periodo_busqueda.v_f_ini = p_f_inicio
   LET g_periodo_busqueda.v_f_fin = p_f_fin
   LET g_plataforma               = p_plataforma
   LET g_ticket                   = p_ticket
   LET g_hoy                      = TODAY

   CALL g_movimientos.clear()
   CALL g_mov_aclara.clear()
   
   LET g_adicionales.v_sello_digital = "ESTA DEBERIA SER LA CADENA DEL SELLO DIGITAL"
   INITIALIZE g_adicionales.v_aviso TO NULL
   SELECT FIRST 1 TRIM(descripcion)
   INTO g_adicionales.v_aviso
   FROM not_campana
   WHERE estado = c_edo_campania_activa
   AND   ind_baja = c_ind_baja
   
   CALL consultarMovimientos()
   
   DISPLAY "\n****************************INICIO MOVIMIENTOS****************************"
   DISPLAY "MOVIMIENTOS NORMALES:      ",g_movimientos.getLength()
   DISPLAY "MOVIMIENTOS EN ACLARACION: ",g_mov_aclara.getLength()
   DISPLAY "\n****************************FIN MOVIMIENTOS****************************" 

   --SI NO HAY MOVIMIENTOS EN NINGUNA CATERORIA O ACLARATORIO ENVIA EL MENSAJE
   IF  g_movimientos.getLength() = 0 AND g_mov_aclara.getLength() = 0 AND g_plataforma = 2 THEN
      DISPLAY "NO HAY MOVIMIENTOS PARA ESTE DERECHOHABIENTE"
      RETURN "", ""
   END IF
  
   CASE g_opcion
      WHEN 1
         CALL fn_genera_rpt_movimientos_4cat(g_datos_personales.*,g_periodo_busqueda.*,g_movimientos,g_mov_aclara,g_adicionales.* ) 
        RETURNING g_ruta_lst, g_nom_reporte
      WHEN 2
         CALL fn_genera_rpt_movimientos_comp(g_datos_personales.*,g_periodo_busqueda.*,g_movimientos,g_adicionales.* ) 
        RETURNING g_ruta_lst, g_nom_reporte
      WHEN 3
         CALL fn_genera_rpt_aportacion_amotizacion(g_datos_personales.*,g_periodo_busqueda.*,g_movimientos,g_adicionales.* ) 
         RETURNING g_ruta_lst, g_nom_reporte
      WHEN 4
         CALL fn_genera_rpt_movimientos_comp(g_datos_personales.*,g_periodo_busqueda.*,g_movimientos,g_adicionales.* ) 
        RETURNING g_ruta_lst, g_nom_reporte
      WHEN 5
         CALL fn_genera_rpt_movimientos_aclara(g_datos_personales.*,g_periodo_busqueda.*,g_mov_aclara,g_adicionales.* ) 
        RETURNING g_ruta_lst, g_nom_reporte
      WHEN 6
         CALL fn_genera_rpt_movimientos_comp(g_datos_personales.*,g_periodo_busqueda.*,g_movimientos,g_adicionales.* ) 
        RETURNING g_ruta_lst, g_nom_reporte
   END CASE   
   
   DISPLAY "\n****************************INICIO ARCHIVO GENERADO****************************"
   DISPLAY "NOMBRE REPORTE: ",g_nom_reporte
   DISPLAY "RUTA REPORTE: ",g_ruta_lst
   DISPLAY "\n****************************FIN ARCHIVO GENERADO****************************\n\n" 
   
   RETURN g_ruta_lst, g_nom_reporte
END FUNCTION

FUNCTION consultarMovimientos()
   DEFINE v_query         STRING
   DEFINE movimientos DYNAMIC ARRAY OF RECORD
      fLiquidacion DATE,
      movimiento CHAR(5),
      movimiento_desc STRING,
      subcuenta STRING,
      abono DECIMAL(16,2),
      cargo DECIMAL(16,2)
    END RECORD
    DEFINE v_cont, v_cont2    INTEGER
    DEFINE v_manejador_hash   security.Digest
    DEFINE v_c_plataforma     CHAR(1)

   --Consulta que busca el nss recibido
   INITIALIZE v_trabajador.* TO NULL
   LET v_query =  "SELECT ",
                     "id_derechohabiente, ",
                     "nss, ",
                     "rfc, ",
                     "curp, ",
                     "trim(nombre_af), ",
                     "trim(NVL(ap_paterno_af,' ')), ",
                     "trim(NVL(ap_materno_af,' ')) ",
                  "FROM afi_derechohabiente ",
                  "WHERE nss = ? "
   PREPARE exe_consulta_trabajador FROM v_query
   EXECUTE exe_consulta_trabajador USING g_nss INTO v_trabajador.*
 
 --Se evalua que exista NSS
   IF v_trabajador.id_derechohabiente IS NOT NULL THEN -- Existe NSS

      LET g_datos_personales.v_nombre  = v_trabajador.nombre
      LET g_datos_personales.v_paterno = v_trabajador.a_paterno
      LET g_datos_personales.v_materno = v_trabajador.a_materno
      LET g_datos_personales.v_nss     = v_trabajador.nss
      LET g_datos_personales.v_curp    = v_trabajador.curp
      LET g_datos_personales.v_rfc     = v_trabajador.rfc

      IF g_plataforma = 2 THEN
        LET v_c_plataforma = "s"
      ELSE
        LET v_c_plataforma = "m"
      END if

      -- SE GENERA LA CADENA OFICIAL UNIENDO LOS PARAMETROS CON PIPES
      LET g_adicionales.v_cadena_original = v_trabajador.nss,"|",TODAY USING 'dd/mm/yyyy',"|",v_trabajador.curp,"|",v_trabajador.rfc,"|",v_c_plataforma

      --GENERAMOS EL SELLO DIGITAL APLICANDOLE SHA1
      LET v_manejador_hash = security.Digest.CreateDigest("SHA1")
      CALL v_manejador_hash.AddStringData(g_adicionales.v_cadena_original)
      LET g_adicionales.v_sello_digital = v_manejador_hash.DoHexBinaryDigest()

      LET v_cont = 1

      CALL movimientos.clear() --Se limpia arreglo temporal de movimientos

      --Se consulta el periodo de movimientos mientras la fecha fin sea mayor al 31 de octubre de 2012     
--      IF ( g_periodo_busqueda.v_f_fin > MDY(10,31,2012) ) OR
--         ( (g_opcion = 1 OR g_opcion = 5) AND ( g_periodo_busqueda.v_f_fin <= MDY(10,31,2012) ) ) THEN 
      IF ( g_periodo_busqueda.v_f_fin > MDY(09,30,2012) ) OR
         ( (g_opcion = 1 OR g_opcion = 5) AND ( g_periodo_busqueda.v_f_fin <= MDY(09,30,2012) ) ) THEN 
         DISPLAY "ENTRA CTAC013"
         --Realiza la consulta del periodo historico siempre y cuando la fecha de inicio sea menor al 31 de octubre de 2012
         CALL datos_reportes(g_opcion,v_trabajador.id_derechohabiente,g_periodo_busqueda.v_f_ini, g_periodo_busqueda.v_f_fin) RETURNING movimientos,g_mov_aclara
                        
         FOR v_cont2 = 1 TO movimientos.getLength() --Carga el arreglo historico en el arreglo general
            LET g_movimientos[v_cont].v_f_aplicacion = movimientos[v_cont2].fLiquidacion
            LET g_movimientos[v_cont].v_cve_movimiento = movimientos[v_cont2].movimiento
            LET g_movimientos[v_cont].v_desc_concepto = movimientos[v_cont2].movimiento_desc
            LET g_movimientos[v_cont].v_tpo_movimiento = movimientos[v_cont2].subcuenta
            LET g_movimientos[v_cont].v_cargo = movimientos[v_cont2].cargo
            LET g_movimientos[v_cont].v_abono = movimientos[v_cont2].abono
            LET v_cont = v_cont + 1
         END FOR
      END IF

      CALL movimientos.clear() --Se limpia arreglo temporal de movimientos

      --Se consulta el periodo de movimientos historicos mientras la fecha inicio sea menor al 31 de octubre de 2012
      --IF g_periodo_busqueda.v_f_ini <= MDY(10,31,2012)  THEN
      IF g_periodo_busqueda.v_f_ini <= MDY(9,30,2012)  THEN
         --Realiza la consulta del periodo historico siempre y cuando la fecha de inicio sea menor al 31 de octubre de 2012
         CALL fn_consulta_hist(v_trabajador.*,g_nss,g_opcion,g_periodo_busqueda.v_f_ini,
                        g_periodo_busqueda.v_f_fin) RETURNING movimientos
                        
         FOR v_cont2 = 1 TO movimientos.getLength() --Carga el arreglo historico en el arreglo general
            LET g_movimientos[v_cont].v_f_aplicacion = movimientos[v_cont2].fLiquidacion
            LET g_movimientos[v_cont].v_cve_movimiento = movimientos[v_cont2].movimiento
            LET g_movimientos[v_cont].v_desc_concepto = movimientos[v_cont2].movimiento_desc
            LET g_movimientos[v_cont].v_tpo_movimiento = movimientos[v_cont2].subcuenta
            LET g_movimientos[v_cont].v_cargo = movimientos[v_cont2].cargo
            LET g_movimientos[v_cont].v_abono = movimientos[v_cont2].abono
            LET v_cont = v_cont + 1
         END FOR
      END IF
      
   ELSE
      DISPLAY "NO EXISTE INFORMACIÓN DEL NSS"
   END IF 
   
END FUNCTION 



# Convert Base64 safe URL to Base64 format
FUNCTION Base64Url2Base64(s)
  DEFINE  s   STRING
  DEFINE  buf base.stringBuffer
  LET buf = base.StringBuffer.create()
  CALL buf.append(s)
  CALL buf.replace("-","+",0)
  CALL buf.replace("_","/",0)
  CASE (s.getLength() MOD 4)
    WHEN 1
      RETURN NULL # ERROR (Illegal base64url string!)
    WHEN 2
      CALL buf.append("==")
    WHEN 3
      CALL buf.append("=")     
  END CASE
  RETURN buf.toString()
END FUNCTION 

# Convert Base64 to Base64 safe url format
FUNCTION Base642Base64Url(s)
  DEFINE  s   STRING
  DEFINE  buf base.stringBuffer
  LET buf = base.StringBuffer.create()
  CALL buf.append(s)
  CALL buf.replace("+","-",0)
  CALL buf.replace("/","_",0)
  CALL buf.replace("=",NULL,0)
  RETURN buf.toString()
END FUNCTION
