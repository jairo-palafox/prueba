-----------------------------------------------------------------------------------------
-- Modulo        => CTA
-- Componente    => CTAC011
-- Funcionalidad => Consulta catálogo de aclaraciones (causales).
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 23 de enero de 2020.
-- Requerimiento =>  
-----------------------------------------------------------------------------------------
GLOBALS
   CONSTANT MAX_REGISTROS     SMALLINT = 200
   
   TYPE movimiento RECORD
      movimiento        SMALLINT,
      movimiento_desc   VARCHAR(40),
      tipo_mov          VARCHAR(20),
      ind_visible       VARCHAR(2),
      desc_ciudadana    VARCHAR(60),
      f_modifica        DATE,
      usuario           VARCHAR(40),
      tipo_modifica     VARCHAR(60)
   END RECORD

END GLOBALS