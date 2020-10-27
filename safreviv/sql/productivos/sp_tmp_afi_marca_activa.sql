






CREATE PROCEDURE "safreviv".sp_tmp_afi_marca_activa()

        DEFINE v_nss                 CHAR(11);
        DEFINE v_marca_fecha       CHAR(255);
        DEFINE v_marca_fecha_aux   CHAR(255);
        DEFINE v_ind_estado_cuenta SMALLINT;

        -- NSS | Clave de la marca activa | Fecha de inicio de la marca activa | Cuenta inactiva o activa

        /*CREATE TABLE tmp_afi_sin_marca_activa(
                nss                         CHAR(11) ,
                marca_fecha       CHAR(10),
                ind_estado_cuenta SMALLINT
        );

        CREATE UNIQUE INDEX tmp_afi_sin_marca_activa1 ON tmp_afi_sin_marca_activa(nss);*/

        CREATE TEMP TABLE tmp_afi_marca_activa(
                nss                         CHAR(11) ,
                marca_fecha       CHAR(255),
                ind_estado_cuenta SMALLINT
        );

        CREATE UNIQUE INDEX tmp_afi_marca_activa1 ON tmp_afi_marca_activa(nss);

        --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_tmp_afi_marca_activa.trace';
        --TRACE ON;

        -- Se llena los id_derechohabiente con marcas
        FOREACH
                SELECT ad.nss,sma.marca||"|"||sma.f_inicio,ad.ind_estado_cuenta
                INTO   v_nss,v_marca_fecha,v_ind_estado_cuenta
                FROM   afi_derechohabiente ad,
                       sfr_marca_activa    sma
                WHERE  ad.id_derechohabiente = sma.id_derechohabiente

                LET v_marca_fecha_aux = "";

                SELECT TRIM(marca_fecha)
                INTO   v_marca_fecha_aux
                FROM   tmp_afi_marca_activa
                WHERE  nss = v_nss;

                IF v_marca_fecha_aux <> "" THEN
                        LET v_marca_fecha_aux = TRIM(v_marca_fecha_aux)||"|"||TRIM(v_marca_fecha);
                        --TRACE v_marca_fecha_aux;
                        -- Si ya se guardo, se actualiza el campo concatenando la marca
                        UPDATE tmp_afi_marca_activa
                        SET    marca_fecha = v_marca_fecha_aux
                        WHERE  nss = v_nss;
                ELSE
                        -- Si aun no se guarda
                        INSERT INTO tmp_afi_marca_activa(nss  ,marca_fecha  ,ind_estado_cuenta  )
                                            VALUES(v_nss,v_marca_fecha,v_ind_estado_cuenta);
                END IF

        END FOREACH;

        -- Se llena los id_derechohabiente sin marcas
        FOREACH
                SELECT ad.nss,"|||",ad.ind_estado_cuenta
                INTO   v_nss,v_marca_fecha,v_ind_estado_cuenta
                FROM   afi_derechohabiente ad
                       LEFT JOIN sfr_marca_activa sma
                       ON ad.id_derechohabiente = sma.id_derechohabiente
                WHERE  sma.n_referencia IS NULL

                --INSERT INTO tmp_afi_sin_marca_activa(nss  ,marca_fecha  ,ind_estado_cuenta  )
                INSERT INTO tmp_afi_marca_activa(nss  ,marca_fecha  ,ind_estado_cuenta  )
                                          VALUES(v_nss,v_marca_fecha,v_ind_estado_cuenta);

        END FOREACH;

        --TRACE OFF;

END PROCEDURE;


