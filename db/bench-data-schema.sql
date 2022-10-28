--
-- tables containing data
--

-- describes a cluster run
CREATE TABLE cluster_run
    ( id SERIAL PRIMARY KEY
    , run_profile TEXT NOT NULL
    , run_commit TEXT NOT NULL
    , run_at TIMESTAMPTZ NOT NULL
    , run_published BOOLEAN NOT NULL DEFAULT true

    , CONSTRAINT un_run_profile UNIQUE (run_profile, run_commit, run_at)
);
-- FIXME: during development, publish all runs by default;
-- set run_published DEFAULT false for production

CREATE TABLE run_info
    ( meta JSONB NOT NULL
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

-- a base view containing every attribute viable for filtering all published runs
CREATE MATERIALIZED VIEW run AS
    SELECT
        cr.id AS run_id,
        cr.run_profile,
        cr.run_commit,
        cr.run_at,
        ri.meta #>> '{meta,batch}' AS run_name,
		ri.meta #>> '{meta,profile_content,generator,era}' AS run_era,
        CASE 
            WHEN (ri.meta #> '{meta,profile_content,generator,plutusMode}') :: BOOLEAN
			    OR (ri.meta #> '{meta,profile_content,generator,plutusAutoMode}') :: BOOLEAN
			THEN 'plutus'
            ELSE 'value'
        END AS run_workload
    FROM
        cluster_run cr
        JOIN run_info ri ON cr.id = ri.run_id
    WHERE
        cr.run_published = true;

COMMENT ON MATERIALIZED VIEW run IS
    'All runs on the benchmarking cluster';

-- this view facilitates access to the raw JSON data that was imported
CREATE VIEW run_raw AS
    SELECT
        r.*,
        ri.meta,
        rr.blockprop,
        rr.clusterperf
    FROM 
        run r
        JOIN run_info ri USING (run_id)
        LEFT JOIN run_result rr USING (run_id);

COMMENT ON VIEW run_raw IS
    'Raw JSON data for each benchmarking cluster run, as originally imported';

-- a view over all existing blockprop results from published runs
CREATE VIEW blockprop AS
    WITH cte AS (
        SELECT
            r.run_id,
            rr.blockprop
        FROM
            run r
            JOIN run_result rr USING (run_id)
        WHERE
            rr.blockprop IS NOT NULL
    ) 
    SELECT
        cte.run_id,
        obj.key AS cdf_name,
        cdf.key AS cdf_prop,
        cdf.value AS cdf_value
    FROM
        cte,
        jsonb_each(cte.blockprop) AS obj,
		jsonb_each(obj.value) as cdf
    WHERE
        obj.value ? 'cdfSize';

-- a list of all known blockprop metrics
CREATE VIEW blockprop_metric AS
    SELECT
        DISTINCT cdf_name
    FROM
        blockprop
    ORDER BY
        cdf_name;

-- a view over all existing clsuterperf results from published runs
CREATE VIEW clusterperf AS
    WITH cte AS (
        SELECT
            r.run_id,
            rr.clusterperf
        FROM
            run r
            JOIN run_result rr USING (run_id)
        WHERE
            rr.clusterperf IS NOT NULL
    ) 
    SELECT
        cte.run_id,
        obj.key AS cdf_name,
        cdf.key AS cdf_prop,
        cdf.value AS cdf_value
    FROM
        cte,
        jsonb_each(cte.clusterperf) AS obj,
		jsonb_each(obj.value) as cdf
    WHERE
        obj.value ? 'cdfSize';

-- a list of all known clusterperf metrics
CREATE VIEW clusterperf_metric AS
    SELECT
        DISTINCT cdf_name
    FROM
        clusterperf
    ORDER BY
        cdf_name;

-- a composition of run attributes and the view on blockprop results
CREATE VIEW run_blockprop AS
   SELECT
        r.*,
        b.cdf_name,
        b.cdf_prop,
        b.cdf_value
    FROM
        run r
        JOIN blockprop b USING (run_id);

-- a composition of run attributes and the view on clusterperf results
CREATE VIEW run_clusterperf AS
   SELECT
        r.*,
        c.cdf_name,
        c.cdf_prop,
        c.cdf_value
    FROM
        run r
        JOIN clusterperf c USING (run_id);
