-- Adapted from query written by Norm Olsen for Marie-Pierre Etienne (Yelloweye Rockfish outside hbll and iphc surveys)
   SELECT FE.FISHING_EVENT_ID,
    FE.FE_MAJOR_LEVEL_ID,
    T.TRIP_ID,
    S.SURVEY_ID,
    S.SURVEY_SERIES_ID,
    YEAR(T.TRIP_START_DATE) AS TRIP_YEAR,
      CAST(MAJOR_STAT_AREA_CODE AS SMALLINT) AS major,
      Nall count_all_species,
      Nb count_bait_only,
      Ne count_empty_hooks,
      Nbr count_bent_broken,
      CASE MAJOR_STAT_AREA_CODE WHEN '01' THEN '4B' WHEN '03' THEN '3C'
         WHEN '04' THEN '3D' WHEN '05' THEN '5A/B' WHEN '06' THEN '5A/B'
         WHEN '07' THEN '5C/D/E' WHEN '08' THEN '5C/D/E' WHEN '09' THEN '5C/D/E' ELSE NULL END AS areagrp
   FROM SURVEY S
      INNER JOIN TRIP_SURVEY TS ON
      S.SURVEY_ID = TS.SURVEY_ID
      INNER JOIN TRIP T ON
      TS.TRIP_ID = T.TRIP_ID
      INNER JOIN FISHING_EVENT FE ON
      T.TRIP_ID = FE.TRIP_ID
      INNER JOIN (
            SELECT TRIP_ID,
               VESSEL_ID,
               FE_MAJOR_LEVEL_ID,
               SUM(NT) AS NT,
               SUM(NNT) AS NNT,
               SUM(Ne) AS Ne,
               SUM(Nb) AS Nb,
               SUM(Nbr) AS Nbr
            FROM (
               SELECT T.TRIP_ID,
                  T.VESSEL_ID,
                  FE.FE_MAJOR_LEVEL_ID,
                  FE.FISHING_EVENT_ID,
                  SUM(CASE SPECIES_CODE IS NOT NULL THEN 1 ELSE 0 END) AS Nall, -- all species recorded
                  SUM(CASE HOOK_YIELD_CODE WHEN 1 THEN 1 ELSE 0 END) AS Ne,
                  SUM(CASE HOOK_YIELD_CODE WHEN 2 THEN 1 WHEN 6 THEN 1 ELSE 0 END) AS Nb, -- bait only and bait skin
                  SUM(CASE WHEN HOOK_CONDITION_CODE IN (1,2,3,7) THEN 1 ELSE 0 END) AS Nbr
                  --SUM(CASE HOOK_YIELD_CODE WHEN 2 THEN 1 ELSE 0 END) AS Nb (only 'Bait only' vs. bait only and bait skin)
               FROM SURVEY S
                  INNER JOIN TRIP_SURVEY TS ON
                  S.SURVEY_ID = TS.SURVEY_ID
                  INNER JOIN TRIP T ON
                  TS.TRIP_ID = T.TRIP_ID
                  INNER JOIN FISHING_EVENT FE ON
                  T.TRIP_ID = FE.TRIP_ID
                  LEFT JOIN FISHING_EVENT_CATCH FEC ON
                  FE.FISHING_EVENT_ID = FEC.FISHING_EVENT_ID
                  LEFT JOIN CATCH C ON
                  FEC.CATCH_ID = C.CATCH_ID
                  LEFT JOIN HOOK_SPECS HS ON
                  FE.FISHING_EVENT_ID = HS.FISHING_EVENT_ID
                  LEFT JOIN LONGLINE_SPECS LS
                  ON FE.FISHING_EVENT_ID = LS.FISHING_EVENT_ID
               WHERE FE_PARENT_EVENT_ID IS NOT NULL AND
                  FE_MINOR_LEVEL_ID IS NOT NULL AND
                  -- insert ssid here
                  ISNULL(LS.USABILITY_CODE,0) IN (0,1,2,6)
               GROUP BY T.TRIP_ID, T.VESSEL_ID, FE.FE_MAJOR_LEVEL_ID, FE.FISHING_EVENT_ID) T
            GROUP BY TRIP_ID, VESSEL_ID, FE_MAJOR_LEVEL_ID) C ON
      T.TRIP_ID = C.TRIP_ID AND T.VESSEL_ID = C.VESSEL_ID AND FE.FE_MAJOR_LEVEL_ID = C.FE_MAJOR_LEVEL_ID
   WHERE FE_PARENT_EVENT_ID IS NULL
      -- insert ssid here