--
-- tables containing data
--

-- describes a cluster run
CREATE TABLE cluster_run
    ( id SERIAL PRIMARY KEY
    , run_profile TEXT NOT NULL
    , run_batch TEXT NOT NULL
    , run_at TIMESTAMPTZ NOT NULL
    , run_published BOOLEAN NOT NULL DEFAULT true

    , CONSTRAINT un_run_profile UNIQUE (run_profile, run_batch, run_at)
);
-- FIXME: during development, publish all runs by default;
-- set run_published DEFAUL false for production

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
        cr.run_at,
        ri.meta #>> '{meta,pins,cardano-node}' as commit_id
    FROM
        cluster_run cr
        JOIN run_info ri ON cr.id = ri.run_id
    WHERE
        cr.run_published = true;

CREATE VIEW run_meta AS
    SELECT
        rv.*,
        ri.meta
    FROM 
        runs rv
        JOIN run_info ri ON rv.run_id = ri.run_id;

CREATE VIEW blockprop_cdf AS
    WITH cte AS (
        SELECT * FROM run_result WHERE blockprop IS NOT NULL
    ) 
    SELECT
        cte.run_id,
        obj.key AS cdfName,
        obj.value AS cdf 
    FROM
        cte,
        jsonb_each(cte.blockprop) AS obj
    WHERE
        obj.value ? 'cdfSize';
