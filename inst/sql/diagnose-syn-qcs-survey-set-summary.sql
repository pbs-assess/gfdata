/*
   Diagnostic query for the SYN QCS survey-set differences.

   Run with:
     gfdata::run_sql(
       "GFBioSQL",
       gfdata:::read_sql("diagnose-syn-qcs-survey-set-summary.sql")
     )
*/

;WITH target_events AS (
    SELECT *
    FROM (VALUES
        (502005, 2004,   2, 'old_only'),
        (886068, 2005,   3, 'old_only'),
        (1506720, 2007, 121, 'old_only'),
        (1506801, 2007, 121, 'old_only'),
        (1925699, 2009, 167, 'old_only'),
        (2536358, 2011, 304, 'old_only'),
        (2536402, 2011, 304, 'old_only'),
        (2536428, 2011, 304, 'old_only'),
        (2536445, 2011, 304, 'old_only'),
        (3234278, 2013, 424, 'old_only'),
        (3705897, 2015, 448, 'new_only'),
        (3706014, 2015, 448, 'old_only'),
        (3706111, 2015, 448, 'old_only'),
        (5099897, 2019, 539, 'old_only')
    ) AS v(
        fishing_event_id,
        cache_year,
        expected_survey_id,
        cache_side
    )
)
SELECT
    x.cache_side,
    x.cache_year,
    x.fishing_event_id,
    x.expected_survey_id,

    CASE WHEN fe.fishing_event_id IS NULL THEN 0 ELSE 1 END
        AS event_exists,

    fe.trip_id,
    fe.fe_parent_event_id,
    fe.fe_major_level_id,
    fe.grouping_code AS fe_grouping_code,
    fe.gear_code,
    fe.major_stat_area_code,
    fe.minor_stat_area_code,

    YEAR(t.trip_start_date) AS legacy_year,

    YEAR(COALESCE(
        fe.fe_begin_bottom_contact_time,
        fe.fe_end_bottom_contact_time,
        fe.fe_end_deployment_time,
        fe.fe_begin_retrieval_time,
        fe.fe_begin_deployment_time,
        fe.fe_end_retrieval_time,
        t.trip_start_date
    )) AS new_year,

    qcs.qcs_link_count,
    qcs.qcs_survey_id_min,
    qcs.qcs_survey_id_max,
    qcs_expected.expected_survey_link_count,
    qcs.original_ind_min,
    qcs.original_ind_max,

    grp.feg_grouping_count,
    grp_match.legacy_matching_group_count,
    grp.feg_grouping_code_min,
    grp.feg_grouping_code_max,

    spec.trawl_specs_rows,
    spec.usability_code_min,
    spec.usability_code_max,
    COALESCE(spec.usability_code_min, 1) AS legacy_usability_code,
    COALESCE(spec.usability_code_min, 0) AS new_usability_code,

    cat.cod_catch_rows,
    COALESCE(cat.cod_catch_weight, 0) AS cod_catch_weight,
    COALESCE(cat.cod_catch_count, 0) AS cod_catch_count,

    CASE
        WHEN fe.fishing_event_id IS NOT NULL
         AND qcs_expected.expected_survey_link_count > 0
         AND fe.fe_parent_event_id IS NULL
         AND COALESCE(spec.usability_code_min, 1) IN (0, 1, 2, 6)
         AND grp_match.legacy_matching_group_count > 0
        THEN 1 ELSE 0
    END AS legacy_candidate_today,

    CASE
        WHEN fe.fishing_event_id IS NOT NULL
         AND qcs.qcs_link_count > 0
         AND (fe.fe_major_level_id < 700 OR fe.fe_major_level_id IS NULL)
         AND COALESCE(spec.usability_code_min, 0) IN (0, 1, 2, 6)
        THEN 1 ELSE 0
    END AS new_event_candidate_today,

    CASE
        WHEN fe.fishing_event_id IS NOT NULL
         AND qcs.qcs_link_count > 0
         AND fe.fe_parent_event_id IS NULL
         AND fe.fe_major_level_id < 700
         AND cat.cod_catch_rows > 0
        THEN 1 ELSE 0
    END AS new_species_seed_row,

    CASE
        WHEN fe.fishing_event_id IS NOT NULL
         AND qcs.qcs_link_count > 0
         AND fe.fe_parent_event_id IS NULL
         AND fe.fe_major_level_id < 700
         AND (
             COALESCE(cat.cod_catch_weight, 0) > 0
             OR COALESCE(cat.cod_catch_count, 0) > 0
         )
        THEN 1 ELSE 0
    END AS new_positive_species_seed_row

FROM target_events AS x
LEFT JOIN fishing_event AS fe
    ON fe.fishing_event_id = x.fishing_event_id
LEFT JOIN trip AS t
    ON t.trip_id = fe.trip_id

OUTER APPLY (
    SELECT
        COUNT(*) AS qcs_link_count,
        MIN(s.survey_id) AS qcs_survey_id_min,
        MAX(s.survey_id) AS qcs_survey_id_max,
        MIN(s.original_ind) AS original_ind_min,
        MAX(s.original_ind) AS original_ind_max
    FROM trip_survey AS ts
    INNER JOIN survey AS s
        ON s.survey_id = ts.survey_id
    WHERE ts.trip_id = fe.trip_id
      AND s.survey_series_id = 1
) AS qcs

OUTER APPLY (
    SELECT COUNT(*) AS expected_survey_link_count
    FROM trip_survey AS ts
    INNER JOIN survey AS s
        ON s.survey_id = ts.survey_id
    WHERE ts.trip_id = fe.trip_id
      AND s.survey_series_id = 1
      AND s.survey_id = x.expected_survey_id
) AS qcs_expected

OUTER APPLY (
    SELECT
        COUNT(DISTINCT feg.grouping_code) AS feg_grouping_count,
        MIN(feg.grouping_code) AS feg_grouping_code_min,
        MAX(feg.grouping_code) AS feg_grouping_code_max
    FROM fishing_event_grouping AS feg
    WHERE feg.fishing_event_id = fe.fishing_event_id
) AS grp

OUTER APPLY (
    SELECT COUNT(DISTINCT feg.grouping_code)
        AS legacy_matching_group_count
    FROM fishing_event_grouping AS feg
    INNER JOIN survey_grouping AS sg
        ON sg.grouping_code = feg.grouping_code
    WHERE feg.fishing_event_id = fe.fishing_event_id
      AND sg.survey_id = x.expected_survey_id
) AS grp_match

OUTER APPLY (
    SELECT
        COUNT(*) AS trawl_specs_rows,
        MIN(trsp.usability_code) AS usability_code_min,
        MAX(trsp.usability_code) AS usability_code_max
    FROM trawl_specs AS trsp
    WHERE trsp.fishing_event_id = fe.fishing_event_id
) AS spec

OUTER APPLY (
    SELECT
        COUNT(*) AS cod_catch_rows,
        SUM(ISNULL(c.catch_weight, 0)) AS cod_catch_weight,
        SUM(ISNULL(c.catch_count, 0)) AS cod_catch_count
    FROM fishing_event_catch AS fec
    INNER JOIN catch AS c
        ON c.catch_id = fec.catch_id
    WHERE fec.fishing_event_id = fe.fishing_event_id
      AND c.species_code = '222' -- Pacific cod
) AS cat

ORDER BY x.cache_year, x.fishing_event_id;
