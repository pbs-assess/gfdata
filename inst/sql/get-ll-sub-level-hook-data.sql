-- Adapted from query written by Norm Olsen for Marie-Pierre Etienne (Yelloweye Rockfish outside hbll and iphc surveys)
   SELECT FE.FISHING_EVENT_ID,
    FE.FE_MAJOR_LEVEL_ID,
    FE.FE_SUB_LEVEL_ID,
    T.TRIP_ID,
    Nall count_all_animals,
    Nsp count_all_species,
    Nb count_bait_only,
    Ne count_empty_hooks,
    Nnf count_not_fishing,
    Nbr count_bent_broken
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
               FE_SUB_LEVEL_ID,
               SUM(Nall) AS Nall,
               SUM(Nsp) AS Nsp,
               SUM(Ne) AS Ne,
               SUM(Nb) AS Nb,
               SUM(Nnf) AS Nnf,
               SUM(Nbr) AS Nbr
            FROM (
               SELECT T.TRIP_ID,
                  T.VESSEL_ID,
                  FE.FE_MAJOR_LEVEL_ID,
                  FE.FE_SUB_LEVEL_ID,
                  FE.FISHING_EVENT_ID,
                  SUM(CASE WHEN SPECIES_CODE IS NOT NULL THEN 1 ELSE 0 END) AS Nsp, -- all species recorded
                  SUM(CASE WHEN HOOK_YIELD_CODE IN (3,4,5,8) THEN 1 ELSE 0 END) AS Nall, -- all animals
                  SUM(CASE HOOK_YIELD_CODE WHEN 1 THEN 1 ELSE 0 END) AS Ne,
                  SUM(CASE HOOK_YIELD_CODE WHEN 2 THEN 1 WHEN 6 THEN 1 ELSE 0 END) AS Nb, -- bait only and bait skin
                  SUM(CASE WHEN HOOK_CONDITION_CODE IN (1,2,3,7) THEN 1 ELSE 0 END) AS Nbr,
                  SUM(CASE HOOK_YIELD_CODE WHEN 9 THEN 1 ELSE 0 END) AS Nnf -- not fishing
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
                  FE_MINOR_LEVEL_ID IS NOT NULL
                  -- insert ssid here
               GROUP BY T.TRIP_ID, T.VESSEL_ID, FE.FE_MAJOR_LEVEL_ID, FE.FISHING_EVENT_ID, FE.FE_SUB_LEVEL_ID) T
            GROUP BY TRIP_ID, VESSEL_ID, FE_MAJOR_LEVEL_ID, FE_SUB_LEVEL_ID) C ON
      T.TRIP_ID = C.TRIP_ID AND T.VESSEL_ID = C.VESSEL_ID AND FE.FE_MAJOR_LEVEL_ID = C.FE_MAJOR_LEVEL_ID
      AND FE.FE_SUB_LEVEL_ID = C.FE_SUB_LEVEL_ID
   WHERE FE_MINOR_LEVEL_ID IS NULL
      -- insert ssid here
