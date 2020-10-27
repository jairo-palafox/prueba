--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16-04-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => MDT                                                     #
#Programa          => MDTG01                                                  #
#Objetivo          => LIBRERIA DE FUNCIONES GENERALES DE MANDATOS             #
#Fecha Inicio      => 08-MAR-2012                                             #
###############################################################################
IMPORT os   
GLOBALS "MDTG02.4gl"
DATABASE safre_viv


################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG02                                                   #
#Descripcion       => Extrae el ultimo token de una cadena                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_extrae_ultimo_token(p_cadena)
DEFINE p_cadena     STRING,
       v_cadena_aux STRING,
       v_elemento   STRING
DEFINE v_cad_busqueda    base.StringTokenizer,
       v_cad_reemplazo   base.StringBuffer

   LET v_cad_reemplazo = base.StringBuffer.create()
   CALL v_cad_reemplazo.append(p_cadena CLIPPED)
   # reemplaza los slash por pipes, para  poder hacer busqueda por toquen(para el caso de ruta windows)
   CALL v_cad_reemplazo.replace("\\","|",0)
   LET v_cadena_aux = v_cad_reemplazo.toString()
   # limpia buffer
   CALL v_cad_reemplazo.clear()
   CALL v_cad_reemplazo.append(v_cadena_aux CLIPPED)
   # reemplaza las diagonales por pipes, para  poder hacer busqueda por toquen(para el caso de ruta linux)
   CALL v_cad_reemplazo.replace("/","|",0)
   LET v_cadena_aux = v_cad_reemplazo.toString()
   # divide la cadena segun los pipes encontrados
   LET v_cad_busqueda = base.StringTokenizer.create(v_cadena_aux,"|")
   WHILE v_cad_busqueda.hasMoreTokens()
      # recupera cada elemento hasta el ultimo
      LET v_elemento = v_cad_busqueda.nextToken()
   END WHILE
   # retorna el ultimo elemento encontrado
   RETURN v_elemento
END FUNCTION 

#############################################################################
# Funcion           => fn_transfiere_archivo - Copiar archivos de un equipo #
#                      remoto al servidor                                   #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_r_imagen - Registro que identifica la iamgen       #
#                      v_operacion - Tipo operacion, inoperable actualmente #
#                      v_nombre_original - NOmbre original del archivo,     #
#                      actualmente inoperable                               #
# Salida:           => v_NomArcDepT - nombre del archivo almacenado         #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 02 Marzo 2012                                        #
#############################################################################
FUNCTION fn_transfiere_archivo(p_r_imagen, v_operacion, v_nombre_original)
   DEFINE p_r_imagen     RECORD
             id_imagen_docto   LIKE mdt_imagen_docto.id_imagen_docto,
             id_gpo_mandato    LIKE mdt_imagen_docto.id_gpo_mandato ,
             id_cat_gpo        LIKE mdt_cat_gpo.id_cat_gpo,
             nombre_imagen     STRING, -- TMP AHM Cambiará tipo dato LIKE mdt_imagen_docto.nombre_imagen  ,
             v_documento_int   STRING,
             v_documento       STRING,
             desc_imagen       LIKE mdt_imagen_docto.desc_imagen
          END RECORD,
          v_operacion       CHAR(1),
          v_nombre_original STRING
            
   DEFINE v_NomArcDep    VARCHAR(500)
   DEFINE v_NomArcDepT   STRING
   DEFINE v_ruta_docto   LIKE seg_modulo.ruta_docto
   DEFINE v_gpo          STRING
            
   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
            
   {DISPLAY "NomArc - ",p_r_imagen.nombre_imagen CLIPPED
   CALL os.Path.basename(p_r_imagen.nombre_imagen) RETURNING v_NomArcDep
   DISPLAY "os.Path.basename - ", v_NomArcDep}
   # funcion que extrae el ultimo elemento de una cadena ruta
   CALL fn_extrae_ultimo_token(p_r_imagen.nombre_imagen) RETURNING v_NomArcDep

   SLEEP 5
   
   CALL fn_depura_archivo(v_NomArcDep) RETURNING v_NomArcDep
            
   LET v_gpo = p_r_imagen.id_cat_gpo USING "&&&&"
   LET v_gpo = v_gpo.trim()
   LET v_NomArcDepT = "tmp_", v_gpo, "_", v_NomArcDep
            
   DISPLAY "Origen : ",p_r_imagen.nombre_imagen
   DISPLAY "Destino: ",v_ruta_docto CLIPPED, v_NomArcDepT
   CALL fgl_getfile(p_r_imagen.nombre_imagen, v_ruta_docto CLIPPED||v_NomArcDepT)
            
   RETURN v_NomArcDepT
            
END FUNCTION
            
#############################################################################
# Funcion           => fn_depura_archivo - Depurar nombre de archivo, en    #
#                      especifico los espacios del archivo                  #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_NomArc - Archivo a subir del local al servidor     #
# Salida:           => p_NomArcDep - Archivo depurado con guiones en lugar  #
#                      de espacios                                          #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 02 Marzo 2012                                        #
#############################################################################
FUNCTION fn_depura_archivo(p_NomArc)
  DEFINE p_NomArc      VARCHAR(500)
  DEFINE p_NomArcDep   VARCHAR(500)
  DEFINE v_cadena  STRING
  DEFINE v_longitud INTEGER

            
  CALL fn_sustituye_cadena (p_NomArc," ","_") RETURNING p_NomArcDep
  --LET v_longitud = LENGTH(p_NomArcDep)
  --LET p_NomArcDep = "temp_",p_NomArcDep[5,v_longitud]
  --DISPLAY p_NomArcDep[5,v_longitud]
  --CALL fn_sustituye_cadena (p_NomArcDep,"tmp_","temp_") RETURNING p_NomArcDep
            
  RETURN p_NomArcDep
END FUNCTION
            

#############################################################################
# Funcion           => fn_admon_archivo_mdt - Mantenimiento a los archivos  #
#                      subidos al servidor de forma temporal                #
# Propietario       => E.F.P                                                #
# Sistema           => GLO                                                  #
# Entrada:          => p_archivo - Nombre del archivo a dar mantenimiento   #
#                      p_id_imagen_docto - Clave de imagen para referencia  #
#                      p_tpo_ren - Tipo de mantenimiento:                   #
#                                  'A' renombrar archivo temporal en la alta#
#                                  'B' borrar archivo al cancelar o confir- #
#                                  mar su eliminación                       #
# Salida:           => v_archivo - Nombre del archivo o notificación de man-#
#                                  tenimiento:                              #
#                                  'A' Archivo renombrado de temporal por su#
#                                  nombre definitivo o leyenda NO_MODIFICADO#
#                                  'B' leyenda de ELIMINADO o NO_ELIMINADO  #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 06 Marzo 2012                                        #
#############################################################################
FUNCTION fn_admon_archivo_mdt(p_archivo,p_id_imagen_docto, p_tpo_ren)
   DEFINE p_archivo         STRING,
          p_id_imagen_docto LIKE mdt_imagen_docto.id_imagen_docto,
          p_tpo_ren         CHAR(1),  -- Renombrar para Alta 'A' o modificación 'M'
          v_archivo         STRING,
          v_buf             base.StringBuffer
   DEFINE v_res             SMALLINT
   DEFINE v_ruta_docto      LIKE seg_modulo.ruta_docto
   
   IF LENGTH(p_archivo CLIPPED) = 0 THEN
      DISPLAY "Archivo nulo, no procede el guardado ...."
      RETURN v_archivo
   END IF 
          
   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
   
   DISPLAY "fn_admon_archivo_mdt - parametros:"
   DISPLAY "p_archivo         - ", p_archivo        
   DISPLAY "p_id_imagen_docto - ", p_id_imagen_docto
   DISPLAY "p_tpo_ren         - ", p_tpo_ren        
   DISPLAY "Ruta origen  - ",v_ruta_docto CLIPPED||p_archivo
   
   LET v_buf = base.StringBuffer.create()
            
   IF p_tpo_ren = 'A' THEN
      CALL v_buf.append(p_archivo)
      CALL v_buf.replace("tmp", p_id_imagen_docto USING "&&&&", 1)
      LET v_archivo = v_buf.toString()
      DISPLAY "Ruta destino - ",v_ruta_docto CLIPPED||v_archivo CLIPPED
      CALL os.Path.rename(v_ruta_docto CLIPPED||p_archivo CLIPPED, v_ruta_docto CLIPPED||v_archivo CLIPPED) RETURNING v_res
      IF NOT v_res THEN
         LET v_archivo = "NO MODIFICADO"
      END IF 
   ELSE     
      IF p_tpo_ren = 'B' THEN
         CALL os.Path.delete(v_ruta_docto CLIPPED||p_archivo CLIPPED) RETURNING v_res
         DISPLAY "os.Path.delete - '", v_ruta_docto CLIPPED||p_archivo  CLIPPED, "' - resultado: ",v_res
         IF v_res THEN
            LET v_archivo = "ELIMINADO"
         ELSE
            LET v_archivo = "NO ELIMINADO"
         END IF 
      END IF
   END IF   
            
   RETURN v_archivo
END FUNCTION

###############################################################################
# Funcion           => fn_admon_archivo_mdt_modificaciones - Mantenimiento a  #
#                      los archivos subidos al servidor de forma temporal     #
#                      durante la modificación de mandatos                    #
# Propietario       => E.F.P                                                  #
# Sistema           => GLO                                                    #
# Entrada:          => p_archivo_org - Nombre del archivo a dar manto orginal #
#                      p_archivo_nvo - Nombre del archivo a dar manto nuevo   #
#                      p_id_imagen_docto - Clave de imagen para referencia    #
#                      p_tpo_ren - Tipo de mantenimiento:                     #
#                                  'A' renombrar archivo temporal en la alta  #
#                                  'B' borrar archivo al cancelar o confir-   #
#                                  mar su eliminación                         #
#                                  'M' Realizar las 2 operaciones anteriores  #
# Salida:           => v_archivo - Nombre del archivo o notificación de man-  #
#                                  tenimiento:                                #
#                                  'A' Archivo renombrado de temporal por su  #
#                                  nombre definitivo o leyenda NO MODIFICADO  #
#                                  'B' Archivo renombrado de temporal por su  #
#                                  nombre definitivo o leyenda de NO ELIMINADO#
# Modificacion      => Alexandro Hollmann, EFP                                #
# Fecha             => 06 Marzo 2012                                          #
###############################################################################
FUNCTION fn_admon_archivo_mdt_modificaciones(p_archivo_org,p_archivo_nvo, p_id_imagen_docto, p_tpo_ren)
   DEFINE p_archivo_org     STRING
   DEFINE p_archivo_nvo     STRING
   DEFINE p_id_imagen_docto LIKE mdt_imagen_docto.id_imagen_docto
   DEFINE p_tpo_ren         CHAR(1)
   DEFINE v_archivo         STRING
   DEFINE v_buf             base.StringBuffer
   DEFINE v_res             SMALLINT
   DEFINE v_ruta_docto      LIKE seg_modulo.ruta_docto

   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
   
   DISPLAY "fn_admon_archivo_mdt_modificaciones - parametros:"
   DISPLAY "p_archivo_org     - ", p_archivo_org        
   DISPLAY "p_archivo_nvo     - ", p_archivo_nvo
   DISPLAY "p_tpo_ren         - ", p_tpo_ren        

   IF p_tpo_ren = 'A' OR p_tpo_ren = 'M' THEN

      IF LENGTH(p_archivo_nvo CLIPPED) = 0 THEN
         DISPLAY "Archivo nulo, no procede el guardado ...."
         RETURN v_archivo
      END IF 
       LET v_buf = base.StringBuffer.create()
      CALL v_buf.append(p_archivo_nvo)
      CALL v_buf.replace("tmp", p_id_imagen_docto USING "&&&&", 1)
      LET v_archivo = v_buf.toString()
      DISPLAY "os.Path.rename - Ruta origen  - ",v_ruta_docto CLIPPED||p_archivo_nvo CLIPPED
      DISPLAY "os.Path.rename - Ruta destino - ",v_ruta_docto CLIPPED||v_archivo CLIPPED
      CALL os.Path.rename(v_ruta_docto CLIPPED||p_archivo_nvo CLIPPED, v_ruta_docto CLIPPED||v_archivo CLIPPED) RETURNING v_res
      IF NOT v_res THEN
         LET v_archivo = "NO MODIFICADO"
         RETURN v_archivo
      END IF 
   END IF

   IF p_tpo_ren = 'B' OR p_tpo_ren = 'M' THEN
      --IF p_archivo_org <> p_archivo_nvo AND p_archivo_nvo.substring THEN
      
      --END IF
      CALL os.Path.delete(v_ruta_docto CLIPPED||p_archivo_org CLIPPED) RETURNING v_res
      DISPLAY "os.Path.delete - '", v_ruta_docto CLIPPED||p_archivo_org  CLIPPED, "' - resultado: ",v_res
      IF NOT v_res THEN
         LET v_archivo = "NO ELIMINADO"
      END IF 
   END IF

   RETURN v_archivo
END FUNCTION

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG01                                                   #
#Objetivo          => Reporte de solicitudes de mandatos                       #
#Autor             => Hugo César Ramírez Gracía                                #
#Modifico          => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 03/04/2012                                               #
#Fecha Modificacion=> 04/04/2012                                               #
################################################################################
REPORT rpt_solicitudes_mandatos(v_datos_generales,v_det_operacion,v_detalle_rechazos)
DEFINE v_datos_generales RECORD 
        v_origen         LIKE mdt_cat_origen.des_origen,
        v_proceso        LIKE cat_operacion.opera_desc,
        v_lote_mdt       LIKE mdt_solicitud_mandato.folio,
        v_fecha_lote     CHAR(10),--LIKE mdt_solicitud_mandato.f_lote,
        v_altas          INTEGER,
        v_bajas          INTEGER,
        v_modificaciones INTEGER
       END RECORD,
       v_det_operacion RECORD
        v_altas_aceptadas          INTEGER,
        v_bajas_aceptadas          INTEGER,
        v_modificaciones_aceptadas INTEGER,
        v_altas_rechazadas         INTEGER,
        v_bajas_rechazadas         INTEGER,
        v_mdificaciones_rechazadas INTEGER
       END RECORD,
       v_detalle_rechazos RECORD
        v_nss         LIKE afi_derechohabiente.nss,
        v_mandato     LIKE mdt_cat_mandato.desc_mandato,
        v_diagnostico LIKE mdt_solicitud_mandato.diagnostico
       END RECORD,
       v_total                INTEGER,
       v_total_aceptadas      INTEGER,
       v_total_rechazadas     INTEGER,
       v_total_det_aceptadas  INTEGER,
       v_total_det_rechazadas INTEGER,       
       v_auxiliar1       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar2       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar3       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar4       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar5       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar6       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar7,
       v_auxiliar7_1,
       v_auxiliar7_2,
       v_auxiliar7_3,
       v_auxiliar7_4,
       v_auxiliar7_5       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar8       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar9       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar10       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar11       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar12       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar13       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar14,
       v_auxiliar14_1,
       v_auxiliar14_2,
       v_auxiliar14_3,
       v_auxiliar14_4,
       v_auxiliar14_5       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_pagina          SMALLINT,
       v_diagnostico     VARCHAR(40)

   OUTPUT
      PAGE LENGTH 30
      
         
   FORMAT

      FIRST PAGE HEADER
         PRINTX v_datos_generales.v_origen
         PRINTX v_datos_generales.v_proceso
         PRINTX v_datos_generales.v_lote_mdt
         PRINTX v_datos_generales.v_fecha_lote
         PRINTX v_datos_generales.v_altas
         PRINTX v_datos_generales.v_bajas
         
         PRINTX v_datos_generales.v_modificaciones
         LET v_total = v_datos_generales.v_altas + v_datos_generales.v_bajas + v_datos_generales.v_modificaciones    
         PRINTX v_total
         PRINTX v_det_operacion.v_altas_aceptadas
         PRINTX v_det_operacion.v_bajas_aceptadas
         PRINTX v_det_operacion.v_modificaciones_aceptadas
         LET v_total_aceptadas = v_det_operacion.v_altas_aceptadas + v_det_operacion.v_bajas_aceptadas + v_det_operacion.v_modificaciones_aceptadas
         PRINTX v_total_aceptadas
         PRINTX v_det_operacion.v_altas_rechazadas
         PRINTX v_det_operacion.v_bajas_rechazadas
         PRINTX v_det_operacion.v_mdificaciones_rechazadas
         
         LET v_total_rechazadas = v_det_operacion.v_altas_rechazadas + v_det_operacion.v_bajas_rechazadas + v_det_operacion.v_mdificaciones_rechazadas
         PRINTX v_total_rechazadas

         #Desglose de aceptadas
         LET v_total_det_aceptadas = 0
         LET v_auxiliar1.* = v_r_rpt_aceptadas[1].*
            
         PRINTX v_auxiliar1.tpo_mandato
         PRINTX v_auxiliar1.operacion
         PRINTX v_auxiliar1.total_mdt
            
            {PRINTX v_r_rpt_aceptadas[1].tpo_mandato
            PRINTX v_r_rpt_aceptadas[1].operacion
            PRINTX v_r_rpt_aceptadas[1].total_mdt}
         IF v_r_rpt_aceptadas[1].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[1].total_mdt 
         END IF
         --END FOR
         {PRINTX v_r_rpt_aceptadas[2].tpo_mandato
         PRINTX v_r_rpt_aceptadas[2].operacion
         PRINTX v_r_rpt_aceptadas[2].total_mdt}
         LET v_auxiliar2.* = v_r_rpt_aceptadas[2].*
            
         PRINTX v_auxiliar2.tpo_mandato
         PRINTX v_auxiliar2.operacion
         PRINTX v_auxiliar2.total_mdt
         IF v_r_rpt_aceptadas[2].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[2].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[3].tpo_mandato
         PRINTX v_r_rpt_aceptadas[3].operacion
         PRINTX v_r_rpt_aceptadas[3].total_mdt}
         LET v_auxiliar3.* = v_r_rpt_aceptadas[3].*
            
         PRINTX v_auxiliar3.tpo_mandato
         PRINTX v_auxiliar3.operacion
         PRINTX v_auxiliar3.total_mdt
         IF v_r_rpt_aceptadas[3].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[3].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[4].tpo_mandato
         PRINTX v_r_rpt_aceptadas[4].operacion
         PRINTX v_r_rpt_aceptadas[4].total_mdt}
         LET v_auxiliar4.* = v_r_rpt_aceptadas[4].*
            
         PRINTX v_auxiliar4.tpo_mandato
         PRINTX v_auxiliar4.operacion
         PRINTX v_auxiliar4.total_mdt
         IF v_r_rpt_aceptadas[4].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[4].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[5].tpo_mandato
         PRINTX v_r_rpt_aceptadas[5].operacion
         PRINTX v_r_rpt_aceptadas[5].total_mdt}
         LET v_auxiliar5.* = v_r_rpt_aceptadas[5].*
            
         PRINTX v_auxiliar5.tpo_mandato
         PRINTX v_auxiliar5.operacion
         PRINTX v_auxiliar5.total_mdt
         IF v_r_rpt_aceptadas[5].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[5].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[6].tpo_mandato
         PRINTX v_r_rpt_aceptadas[6].operacion
         PRINTX v_r_rpt_aceptadas[6].total_mdt}
         LET v_auxiliar6.* = v_r_rpt_aceptadas[6].*
            
         PRINTX v_auxiliar6.tpo_mandato
         PRINTX v_auxiliar6.operacion
         PRINTX v_auxiliar6.total_mdt
         IF v_r_rpt_aceptadas[6].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[6].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[7].tpo_mandato
         PRINTX v_r_rpt_aceptadas[7].operacion
         PRINTX v_r_rpt_aceptadas[7].total_mdt}
         LET v_auxiliar7.* = v_r_rpt_aceptadas[7].*
            
         PRINTX v_auxiliar7.tpo_mandato
         PRINTX v_auxiliar7.operacion
         PRINTX v_auxiliar7.total_mdt
         IF v_r_rpt_aceptadas[7].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[7].total_mdt            
         END IF

         LET v_auxiliar7_1.* = v_r_rpt_aceptadas[8].*
            
         PRINTX v_auxiliar7_1.tpo_mandato
         PRINTX v_auxiliar7_1.operacion
         PRINTX v_auxiliar7_1.total_mdt
         IF v_r_rpt_aceptadas[8].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[8].total_mdt            
         END IF

         LET v_auxiliar7_2.* = v_r_rpt_aceptadas[9].*
            
         PRINTX v_auxiliar7_2.tpo_mandato
         PRINTX v_auxiliar7_2.operacion
         PRINTX v_auxiliar7_2.total_mdt
         IF v_r_rpt_aceptadas[9].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[9].total_mdt            
         END IF

         LET v_auxiliar7_3.* = v_r_rpt_aceptadas[10].*
            
         PRINTX v_auxiliar7_3.tpo_mandato
         PRINTX v_auxiliar7_3.operacion
         PRINTX v_auxiliar7_3.total_mdt
         IF v_r_rpt_aceptadas[10].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[10].total_mdt            
         END IF

         LET v_auxiliar7_4.* = v_r_rpt_aceptadas[11].*
            
         PRINTX v_auxiliar7_4.tpo_mandato
         PRINTX v_auxiliar7_4.operacion
         PRINTX v_auxiliar7_4.total_mdt
         IF v_r_rpt_aceptadas[11].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[11].total_mdt            
         END IF

         LET v_auxiliar7_5.* = v_r_rpt_aceptadas[12].*
            
         PRINTX v_auxiliar7_5.tpo_mandato
         PRINTX v_auxiliar7_5.operacion
         PRINTX v_auxiliar7_5.total_mdt
         IF v_r_rpt_aceptadas[12].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[12].total_mdt            
         END IF

         PRINTX v_total_det_aceptadas


         # Desgloce de rechazadas
         LET v_total_det_rechazadas = 0
         LET v_auxiliar8.* = v_r_rpt_canceladas[1].*
            
         PRINTX v_auxiliar8.tpo_mandato
         PRINTX v_auxiliar8.operacion
         PRINTX v_auxiliar8.total_mdt
         IF v_r_rpt_canceladas[1].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[1].total_mdt  
         END IF
         --END FOR
         -- TMP AHM PRINTX v_r_rpt_canceladas[2].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[2].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[2].total_mdt
         LET v_auxiliar9.* = v_r_rpt_canceladas[2].*
            
         PRINTX v_auxiliar9.tpo_mandato
         PRINTX v_auxiliar9.operacion
         PRINTX v_auxiliar9.total_mdt
         IF v_r_rpt_canceladas[2].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[2].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[3].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[3].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[3].total_mdt
         LET v_auxiliar10.* = v_r_rpt_canceladas[3].*
            
         PRINTX v_auxiliar10.tpo_mandato
         PRINTX v_auxiliar10.operacion
         PRINTX v_auxiliar10.total_mdt
         IF v_r_rpt_canceladas[3].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[3].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[4].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[4].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[4].total_mdt
         LET v_auxiliar11.* = v_r_rpt_canceladas[4].*
            
         PRINTX v_auxiliar11.tpo_mandato
         PRINTX v_auxiliar11.operacion
         PRINTX v_auxiliar11.total_mdt
         IF v_r_rpt_canceladas[4].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[4].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[5].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[5].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[5].total_mdt
         LET v_auxiliar12.* = v_r_rpt_canceladas[5].*
            
         PRINTX v_auxiliar12.tpo_mandato
         PRINTX v_auxiliar12.operacion
         PRINTX v_auxiliar12.total_mdt
         IF v_r_rpt_canceladas[5].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[5].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[6].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[6].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[6].total_mdt
         LET v_auxiliar13.* = v_r_rpt_canceladas[6].*
            
         PRINTX v_auxiliar13.tpo_mandato
         PRINTX v_auxiliar13.operacion
         PRINTX v_auxiliar13.total_mdt
         IF v_r_rpt_canceladas[6].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[6].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[7].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[7].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[7].total_mdt
         LET v_auxiliar14.* = v_r_rpt_canceladas[7].*
            
         PRINTX v_auxiliar14.tpo_mandato
         PRINTX v_auxiliar14.operacion
         PRINTX v_auxiliar14.total_mdt
         IF v_r_rpt_canceladas[7].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[7].total_mdt
         END IF

         LET v_auxiliar14_1.* = v_r_rpt_canceladas[8].*
            
         PRINTX v_auxiliar14_1.tpo_mandato
         PRINTX v_auxiliar14_1.operacion
         PRINTX v_auxiliar14_1.total_mdt
         IF v_r_rpt_canceladas[8].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[8].total_mdt
         END IF

         LET v_auxiliar14_2.* = v_r_rpt_canceladas[9].*
            
         PRINTX v_auxiliar14_2.tpo_mandato
         PRINTX v_auxiliar14_2.operacion
         PRINTX v_auxiliar14_2.total_mdt
         IF v_r_rpt_canceladas[9].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[9].total_mdt
         END IF

         LET v_auxiliar14_3.* = v_r_rpt_canceladas[10].*
            
         PRINTX v_auxiliar14_3.tpo_mandato
         PRINTX v_auxiliar14_3.operacion
         PRINTX v_auxiliar14_3.total_mdt
         IF v_r_rpt_canceladas[10].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[10].total_mdt
         END IF

         LET v_auxiliar14_4.* = v_r_rpt_canceladas[11].*
            
         PRINTX v_auxiliar14_4.tpo_mandato
         PRINTX v_auxiliar14_4.operacion
         PRINTX v_auxiliar14_4.total_mdt
         IF v_r_rpt_canceladas[11].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[11].total_mdt
         END IF

         LET v_auxiliar14_5.* = v_r_rpt_canceladas[12].*
            
         PRINTX v_auxiliar14_5.tpo_mandato
         PRINTX v_auxiliar14_5.operacion
         PRINTX v_auxiliar14_5.total_mdt
         IF v_r_rpt_canceladas[12].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[12].total_mdt
         END IF

         PRINTX v_total_det_rechazadas
         
      
      ON EVERY ROW
         CALL fn_recupera_desc_diagnostico(v_detalle_rechazos.v_diagnostico) RETURNING v_diagnostico
         IF LENGTH(v_detalle_rechazos.v_nss CLIPPED) >0 THEN
            IF LENGTH(v_diagnostico) = 0 THEN
               LET v_diagnostico = v_detalle_rechazos.v_diagnostico,"-SIN DESCRIPCION DE CATALOGO"
            ELSE
               LET v_diagnostico = v_detalle_rechazos.v_diagnostico,"-",v_diagnostico
            END IF
         ELSE
            LET v_diagnostico = " "
         END IF
         PRINTX v_detalle_rechazos.v_nss," ",v_detalle_rechazos.v_mandato," ",v_diagnostico
         {PRINTX v_detalle_rechazos.v_mandato
         PRINTX v_detalle_rechazos.v_diagnostico}

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         
         

END REPORT

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG01                                                   #
#Objetivo          => Función para recuperar descripciones de diagnosticos     #
#Autor             => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 19/04/2012                                               #
#Fecha Modificacion=>                                                          #
################################################################################
FUNCTION fn_recupera_desc_diagnostico(p_diagnostico)
DEFINE p_diagnostico      LIKE mdt_solicitud_mandato.diagnostico,
       v_desc_diagnostico LIKE mdt_cat_rechazo_inst.descripcion
   
   SELECT descripcion INTO v_desc_diagnostico
     FROM mdt_cat_rechazo_inst
   WHERE diagnostico = p_diagnostico
   
   IF STATUS = NOTFOUND THEN
      RETURN ""
   ELSE
      RETURN v_desc_diagnostico
   END IF 
   
END FUNCTION