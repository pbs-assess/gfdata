-- Adapted from query written by Norm Olsen for Marie-Pierre Etienne (Yelloweye Rockfish outside hbll and iphc surveys)
   SELECT FE.FISHING_EVENT_ID,
    FE.FE_MAJOR_LEVEL_ID,
    FE.FE_SUB_LEVEL_ID,
    T.TRIP_ID,
    SPECIES_CODE,
    NT AS catch_count,
   FROM SURVEY S
      INNER JOIN TRIP_SURVEY TS ON
      S.SURVEY_ID = TS.SURVEY_ID
      INNER JOIN TRIP T ON
      TS.TRIP_ID = T.TRIP_ID
      INNER JOIN FISHING_EVENT FE ON
      T.TRIP_ID = FE.TRIP_ID
      INNER JOIN (
            SELECT TRIP_ID,
               FE.FISHING_EVENT_ID,
               FE_MAJOR_LEVEL_ID,
               FE_SUB_LEVEL_ID,
               SUM(NT) AS NT,
            FROM (
               SELECT T.TRIP_ID,
                  T.VESSEL_ID,
                  FE.FE_MAJOR_LEVEL_ID,
                  FE.FE_SUB_LEVEL_ID,
                  FE.FISHING_EVENT_ID,
                  SUM(CASE SPECIES_CODE WHEN
				          -- insert species here
				   THEN 1 ELSE 0 END) AS NT, -- count target
               FROM TRIP T
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
                  -- insert fe_vector here
               GROUP BY T.TRIP_ID, T.VESSEL_ID, FE.FE_MAJOR_LEVEL_ID, FE.FISHING_EVENT_ID, FE.FE_SUB_LEVEL_ID) T
            GROUP BY TRIP_ID, VESSEL_ID, FE_MAJOR_LEVEL_ID, FE_SUB_LEVEL_ID) C ON
      T.TRIP_ID = C.TRIP_ID AND T.VESSEL_ID = C.VESSEL_ID AND FE.FE_MAJOR_LEVEL_ID = C.FE_MAJOR_LEVEL_ID
      AND FE.FE_SUB_LEVEL_ID = C.FE_SUB_LEVEL_ID
   WHERE FE_MINOR_LEVEL_ID IS NULL
      -- insert fe_vector here
