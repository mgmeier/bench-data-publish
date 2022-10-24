
-- start schema from scratch
DROP SCHEMA IF EXISTS api_prototype CASCADE;
CREATE SCHEMA api_prototype;
COMMENT ON SCHEMA api_prototype
    IS 'schema prototype for benchmarking cluster run data';

SET search_path TO api_prototype;


--
-- tables containing data
--

-- describes a cluster run
CREATE TABLE cluster_run
    ( id SERIAL PRIMARY KEY
    , run_profile TEXT NOT NULL
    , run_batch TEXT NOT NULL
    , run_at TIMESTAMPTZ NOT NULL
    , run_published BOOLEAN NOT NULL DEFAULT false

    , CONSTRAINT un_run_profile UNIQUE (run_profile, run_batch, run_at)
);

CREATE TABLE run_info
    ( meta JSON NOT NULL
    , run_id INTEGER NOT NULL

    , CONSTRAINT un_info_run_id UNIQUE (run_id)
    , CONSTRAINT fk_info_run_id FOREIGN KEY (run_id) REFERENCES cluster_run (id)
        ON DELETE CASCADE
);

CREATE TABLE run_result
    ( blockprop JSONB
    , clusterperf JSONB
    , run_id INTEGER NOT NULL

    , CONSTRAINT un_result_run_id UNIQUE (run_id)
    , CONSTRAINT fk_result_run_id FOREIGN KEY (run_id) REFERENCES cluster_run (id)
        ON DELETE CASCADE
);


--
-- views for the API
--

CREATE VIEW runs AS
    SELECT
        cr.id AS run_id,
        cr.run_profile,
        cr.run_batch,
        cr.run_at
    FROM
        cluster_run cr
    WHERE
        cr.run_published = true;

CREATE VIEW run_meta AS
    SELECT
        rv.*,
        ri.meta
    FROM 
        runs rv
        INNER JOIN run_info ri ON rv.run_id = ri.run_id
    LIMIT 1;

CREATE VIEW test_cdfavg AS
    SELECT
        cr.id AS run_id,
        cr.run_profile,
        cr.run_batch,
        cr.run_at,
        (rr.clusterperf -> 'sBlocklessCDF'::text) -> 'cdfAverage'::text AS cdfaverage
    FROM cluster_run cr
        INNER JOIN run_result rr ON cr.id = rr.run_id
    WHERE
        cr.run_published = true;
