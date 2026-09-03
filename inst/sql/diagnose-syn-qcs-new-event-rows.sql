/*
   Diagnostic query showing the individual rows seen by get-event-data.sql
   for the SYN QCS event IDs that differ between the caches.

   Run with:
     run_sql("GFBioSQL", read_sql("diagnose-syn-qcs-new-event-rows.sql"))
*/

;WITH target_events AS (
    SELECT *
    FROM (VALUES
        (502005), (886068), (1506720), (1506801), (1925699),
        (2536358), (2536402), (2536428), (2536445), (3234278),
        (3705897), (3706014), (3706111), (5099897)
    ) AS v(fishing_event_id)
)
SELECT DISTINCT
    x.fishing_event_id,
    fe.trip_id,
    fe.fe_parent_event_id,
    fe.fe_major_level_id,
    fe.grouping_code AS fe_grouping_code,
    fe.gear_code,
    fe.major_stat_area_code,
    fe.minor_stat_area_code,
    ts.survey_id,
    s.survey_series_id,
    s.original_ind,
    trsp.usability_code,
    CASE
        WHEN fe.gear_code IN (1, 6, 8, 11, 14, 16)
            THEN ISNULL(trsp.usability_code, 0)
        WHEN fe.gear_code = 2
            THEN 0
        ELSE 0
    END AS new_usability_code
FROM target_events AS x
INNER JOIN fishing_event AS fe
    ON fe.fishing_event_id = x.fishing_event_id
INNER JOIN trip_survey AS ts
    ON ts.trip_id = fe.trip_id
INNER JOIN survey AS s
    ON s.survey_id = ts.survey_id
LEFT JOIN trawl_specs AS trsp
    ON trsp.fishing_event_id = fe.fishing_event_id
WHERE s.survey_series_id = 1
  AND (fe.fe_major_level_id < 700 OR fe.fe_major_level_id IS NULL)
ORDER BY x.fishing_event_id, ts.survey_id;
