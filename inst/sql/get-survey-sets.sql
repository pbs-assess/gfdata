   SELECT
      FE.FISHING_EVENT_ID AS FISHING_EVENT_ID,
      FE.FE_MAJOR_LEVEL_ID,
      T.TRIP_ID,
      S.SURVEY_SERIES_ID,
      S.SURVEY_ID,
      C.SPECIES_CODE,
      SUM(ISNULL(CATCH_WEIGHT,0)) AS CATCH_WEIGHT,
      SUM(ISNULL(CATCH_COUNT,0)) AS CATCH_COUNT
   FROM SURVEY S
      INNER JOIN SURVEY_SERIES ss ON
      S.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
      INNER JOIN TRIP_SURVEY TRS ON
      S.SURVEY_ID = TRS.SURVEY_ID
      INNER JOIN TRIP T ON
      TRS.TRIP_ID = T.TRIP_ID
      INNER JOIN FISHING_EVENT FE ON
      T.TRIP_ID = FE.TRIP_ID
      INNER JOIN FISHING_EVENT_CATCH FEC ON
      FEC.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
      INNER JOIN CATCH C ON
      FEC.CATCH_ID = C.CATCH_ID
      INNER JOIN SPECIES SP ON
      SP.SPECIES_CODE = C.SPECIES_CODE
   WHERE FE.FE_PARENT_EVENT_ID IS NULL
      AND S.SURVEY_SERIES_ID <> 0
      AND FE.FE_MAJOR_LEVEL_ID < 700
      -- insert species here
      -- insert ssid here
      -- insert fe vector here
   GROUP BY S.SURVEY_SERIES_ID, S.SURVEY_ID, T.TRIP_ID, FE.FISHING_EVENT_ID,
      FE.FE_PARENT_EVENT_ID, FE.FE_MAJOR_LEVEL_ID, T.TRIP_START_DATE,
      C.SPECIES_CODE, FE.GEAR_CODE