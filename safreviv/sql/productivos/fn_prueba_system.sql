






create procedure "safreviv".fn_prueba_system(v_contador_servicio_02 char(10),
                                  v_contador_servicio_03 char(10),
                                  p_archivo char(40));

       SYSTEM "sh /safreviv_int/sep/rescate/.sepUbicaNEdop27 "||"0"||" "||"0"||" "||p_archivo||"1";
       SYSTEM "sh /safreviv_int/sep/rescate/.sepUbicaNEdop27 "||v_contador_servicio_02||" "||v_contador_servicio_03||" "||p_archivo||"2";
       SYSTEM "sh /safreviv_int/sep/rescate/.sepUbicaNEdop27 "||"0"||" "||"0"||" "||p_archivo||"9";

end procedure;


