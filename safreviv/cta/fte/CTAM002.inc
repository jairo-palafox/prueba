-----------------------------------------------------------------------------------------
-- Modulo        => CTA
-- Componente    => CTAM002
-- Funcionalidad => Actualizar catálogo de aclaraciones (causales).
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 23 de enero de 2020.
-- Requerimiento =>  
-----------------------------------------------------------------------------------------

GLOBALS
   CONSTANT MAX_REGISTROS SMALLINT = 200
   
   TYPE movimiento RECORD
      movimiento      SMALLINT,
      movimiento_desc CHAR(150),
      desc_ciudadana  VARCHAR(150)
   END RECORD

   TYPE bitacora_mov RECORD 
      movimiento      SMALLINT,
      movimiento_desc VARCHAR(150),
      desc_ciudadana  VARCHAR(150),
      f_modifica      DATE,
      usuario         VARCHAR(20),
      tpo_modifica    SMALLINT
   END RECORD

END GLOBALS