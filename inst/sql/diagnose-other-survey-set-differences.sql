/*
   Diagnostic query for the six events currently present in the new cache but
   absent from the fresh get_survey_sets() output.

   Run with:
     gfdata::run_sql(
       "GFBioSQL",
       gfdata:::read_sql("diagnose-other-survey-set-differences.sql")
     )

   The old trawl procedure requires a matching FISHING_EVENT_GROUPING /
   SURVEY_GROUPING row. The old longline procedure additionally requires the
   parent/sublevel event hierarchy and a GROUPING row for the parent event.
*/

;WITH target_events AS (
    SELECT *
    FROM (VALUES
        (1720127, 126,  4,  'SYN WCVI'),
        (1507016, 294, 39,  'HBLL INS N'),
        (1507010, 294, 39,  'HBLL INS N'),
        (1131552, 293, 40,  'HBLL INS S'),
        (1945230, 295, 40,  'HBLL INS S'),
        (1945203, 295, 40,  'HBLL INS S')
    ) AS v(
        fishing_event_id,
        expected_survey_id,
        expected_series_id,
        expected_survey
    )
)
SELECT
    x.expected_survey,
    x.expected_survey_id,
    x.expected_series_id,
    x.fishing_event_id,

    CASE WHEN fe.fishing_event_id IS NULL THEN 0 ELSE 1 END
        AS event_exists,
    fe.trip_id,
    fe.fe_parent_event_id,
    fe.fe_major_level_id,
    fe.fe_minor_level_id,
    fe.grouping_code AS fe_grouping_code,
    fe.gear_code,

    survey_link.expected_survey_link_count,

    trsp.trawl_specs_rows,
    trsp.trawl_usability_code_min,
    trsp.trawl_usability_code_max,

    llsp.longline_specs_rows,
    llsp.longline_usability_code_min,
    llsp.longline_usability_code_max,

    CASE
        WHEN fe.gear_code IN (1, 6, 8, 11, 14, 16)
            THEN COALESCE(trsp.trawl_usability_code_min, 0)
        WHEN fe.gear_code = 5
            THEN COALESCE(llsp.longline_usability_code_min, 0)
        ELSE 0
    END AS new_usability_code,

    grp.global_grouping_rows,
    feg.feg_grouping_count,
    feg_match.survey_matching_feg_count,
    ll_hierarchy.ll_hierarchy_rows,

    cat.cod_catch_rows,
    COALESCE(cat.cod_catch_weight, 0) AS cod_catch_weight,
    COALESCE(cat.cod_catch_count, 0) AS cod_catch_count,

    CASE
        WHEN x.expected_series_id NOT IN (39, 40)
         AND fe.fishing_event_id IS NOT NULL
         AND survey_link.expected_survey_link_count > 0
         AND fe.fe_parent_event_id IS NULL
         AND COALESCE(trsp.trawl_usability_code_min, 1) IN (0, 1, 2, 6)
         AND feg_match.survey_matching_feg_count > 0
        THEN 1 ELSE 0
    END AS old_trawl_candidate_today,

    CASE
        WHEN x.expected_series_id IN (39, 40)
         AND fe.fishing_event_id IS NOT NULL
         AND ll_hierarchy.ll_hierarchy_rows > 0
         AND survey_link.expected_survey_link_count > 0
         AND COALESCE(llsp.longline_usability_code_min, 1) IN (0, 1, 2, 6)
         AND grp.global_grouping_rows > 0
        THEN 1 ELSE 0
    END AS old_longline_candidate_today

FROM target_events AS x
LEFT JOIN fishing_event AS fe
    ON fe.fishing_event_id = x.fishing_event_id

OUTER APPLY (
    SELECT COUNT(*) AS expected_survey_link_count
    FROM trip_survey AS ts
    INNER JOIN survey AS s
        ON s.survey_id = ts.survey_id
    WHERE ts.trip_id = fe.trip_id
      AND s.survey_id = x.expected_survey_id
) AS survey_link

OUTER APPLY (
    SELECT
        COUNT(*) AS trawl_specs_rows,
        MIN(ts.usability_code) AS trawl_usability_code_min,
        MAX(ts.usability_code) AS trawl_usability_code_max
    FROM trawl_specs AS ts
    WHERE ts.fishing_event_id = fe.fishing_event_id
) AS trsp

OUTER APPLY (
    SELECT
        COUNT(*) AS longline_specs_rows,
        MIN(ls.usability_code) AS longline_usability_code_min,
        MAX(ls.usability_code) AS longline_usability_code_max
    FROM longline_specs AS ls
    WHERE ls.fishing_event_id = fe.fishing_event_id
) AS llsp

OUTER APPLY (
    SELECT COUNT(*) AS global_grouping_rows
    FROM [grouping] AS g
    WHERE g.grouping_code = fe.grouping_code
) AS grp

OUTER APPLY (
    SELECT COUNT(DISTINCT feg.grouping_code) AS feg_grouping_count
    FROM fishing_event_grouping AS feg
    WHERE feg.fishing_event_id = fe.fishing_event_id
) AS feg

OUTER APPLY (
    SELECT COUNT(DISTINCT feg.grouping_code) AS survey_matching_feg_count
    FROM fishing_event_grouping AS feg
    INNER JOIN survey_grouping AS sg
        ON sg.grouping_code = feg.grouping_code
    WHERE feg.fishing_event_id = fe.fishing_event_id
      AND sg.survey_id = x.expected_survey_id
) AS feg_match

OUTER APPLY (
    SELECT COUNT(*) AS ll_hierarchy_rows
    FROM trip_survey AS ta
    INNER JOIN fishing_event AS a
        ON a.trip_id = ta.trip_id
    INNER JOIN (
        SELECT
            tb.survey_id,
            b.fe_parent_event_id
        FROM trip_survey AS tb
        INNER JOIN fishing_event AS b
            ON b.trip_id = tb.trip_id
        WHERE b.fe_minor_level_id IS NOT NULL
        GROUP BY tb.survey_id, b.fe_parent_event_id
    ) AS b
        ON b.survey_id = ta.survey_id
       AND b.fe_parent_event_id = a.fishing_event_id
    WHERE ta.survey_id = x.expected_survey_id
      AND a.fe_parent_event_id = fe.fishing_event_id
      AND a.fe_minor_level_id IS NULL
      AND a.fe_parent_event_id IS NOT NULL
) AS ll_hierarchy

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

ORDER BY x.expected_survey, x.fishing_event_id;
