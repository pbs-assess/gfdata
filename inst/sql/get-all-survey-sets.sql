   SELECT
      C.SPECIES_CODE,
      S.SURVEY_SERIES_ID,
      S.SURVEY_ID,
      T.TRIP_ID,
      ISNULL(FE.FE_PARENT_EVENT_ID, FE.FISHING_EVENT_ID) AS FISHING_EVENT_ID,
      FE.FE_PARENT_EVENT_ID,
      FE.FE_MAJOR_LEVEL_ID,
      FE.FE_SUB_LEVEL_ID,
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
      LEFT JOIN TRIP_ACTIVITY TA ON
      TA.TRIP_ID = FE.TRIP_ID
      LEFT JOIN ACTIVITY A ON
      A.ACTIVITY_CODE = TA.ACTIVITY_CODE
   WHERE S.SURVEY_SERIES_ID <> 0
      AND FE.FE_MAJOR_LEVEL_ID < 700
      AND FE.FE_MINOR_LEVEL_ID IS NULL
      -- insert species here
      -- insert ssid here
      -- insert fe_vector here
      -- insert major here
   GROUP BY C.SPECIES_CODE, FE.GEAR_CODE, A.ACTIVITY_DESC, TA.ACTIVITY_CODE,
      S.SURVEY_SERIES_ID, S.SURVEY_ID, T.TRIP_ID, FE.FISHING_EVENT_ID,
      FE.FE_PARENT_EVENT_ID, FE.FE_MAJOR_LEVEL_ID, FE.FE_SUB_LEVEL_ID,
      T.TRIP_START_DATE, S.ORIGINAL_IND