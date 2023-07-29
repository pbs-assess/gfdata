SELECT
    FE.FISHING_EVENT_ID,
    FE.FE_PARENT_EVENT_ID,
    S.SURVEY_ID,
    S.SURVEY_SERIES_ID,
    YEAR(T.TRIP_START_DATE) AS TRIP_YEAR,
    YEAR(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME,
      FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME, T.TRIP_START_DATE)) AS YEAR,
    MONTH(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME,
      FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS MONTH,
    DAY(COALESCE (FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME,
      FE_BEGIN_RETRIEVAL_TIME, FE_BEGIN_DEPLOYMENT_TIME, FE_END_RETRIEVAL_TIME)) AS DAY,
    ISNULL(FE_BEGIN_BOTTOM_CONTACT_TIME, FE_END_DEPLOYMENT_TIME) AS TIME_DEPLOYED,
    ISNULL(FE_END_BOTTOM_CONTACT_TIME, FE_BEGIN_RETRIEVAL_TIME) AS TIME_RETRIEVED,
    FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
    -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE,
    FE_END_LATTITUDE_DEGREE + FE_END_LATTITUDE_MINUTE / 60 AS LATITUDE_END,
    -(FE_END_LONGITUDE_DEGREE + FE_END_LONGITUDE_MINUTE / 60) AS LONGITUDE_END,
    FE.MAJOR_STAT_AREA_CODE,
    FE.MINOR_STAT_AREA_CODE,
    FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
    FE_END_BOTTOM_DEPTH AS DEPTH_END,
    FE_BOTTOM_WATER_TEMPERATURE,
    T.VESSEL_ID AS VESSEL_ID,
    T.CAPTAIN_ID AS CAPTAIN_ID,
    ISNULL(DATEDIFF(MI,FE_BEGIN_BOTTOM_CONTACT_TIME,
      FE_END_BOTTOM_CONTACT_TIME),
         DATEDIFF(MI,FE_END_DEPLOYMENT_TIME,
      FE_BEGIN_RETRIEVAL_TIME)) AS DURATION_MIN,
    NULLIF(FE_DISTANCE_TRAVELLED,0) * 1000.0 AS TOW_LENGTH_M,
    ISNULL(NULLIF(TRSP.TRLSP_MOUTH_OPENING_WIDTH,0), ISNULL(BD.doorspread,0)) AS MOUTH_WIDTH_M,
    ISNULL(NULLIF(TRSP.TRLSP_DOORSPREAD,0), ISNULL(BD.doorspread,0)) AS DOORSPREAD_M,
    ISNULL(NULLIF(TRSP.TRLSP_SPEED,0) * 16.66667, ISNULL(speed,0) * 16.66667) AS SPEED_MPM,
    CASE WHEN FE.GEAR_CODE IN (5) THEN LLSP.HOOK_CODE
      WHEN FE.GEAR_CODE IN (4) THEN HLSP.HOOK_CODE
      ELSE 0 END AS HOOK_CODE,
    LGLSP_HOOK_COUNT,
    SKATE_COUNT,
    0.0024384 * 0.009144 * LGLSP_HOOK_COUNT AS HOOK_AREA_SWEPT_KM2,
    CASE WHEN FE.GEAR_CODE IN (1, 6, 11) THEN ISNULL(TRSP.USABILITY_CODE, 0)
      WHEN FE.GEAR_CODE IN (2) THEN ISNULL(TPSP.USABILITY_CODE, 0)
      WHEN FE.GEAR_CODE IN (5) THEN ISNULL(LLSP.USABILITY_CODE, 0)
      WHEN FE.GEAR_CODE IN (4) THEN ISNULL(HLSP.USABILITY_CODE, 0)
      ELSE 0 END AS USABILITY_CODE,
    FE.GROUPING_CODE,
    FEG.GROUPING_CODE AS GROUPING_CODE_TRAWL,
    GROUPING_DESC,
    GROUPING_DEPTH_ID
    FROM FISHING_EVENT FE
      LEFT JOIN TRAWL_SPECS TRSP ON TRSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
      LEFT JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
      LEFT JOIN TRAP_SPECS TPSP ON TPSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
      LEFT JOIN HANDLINE_SPECS HLSP ON HLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
      LEFT JOIN TRIP T ON T.TRIP_ID = FE.TRIP_ID
      LEFT JOIN TRIP_SURVEY TRS ON TRS.TRIP_ID = T.TRIP_ID
      LEFT JOIN SURVEY S ON S.SURVEY_ID = TRS.SURVEY_ID
      LEFT JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
      LEFT JOIN FISHING_EVENT_GROUPING FEG ON FEG.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
      LEFT JOIN SURVEY_GROUPING SG ON SG.SURVEY_ID = S.SURVEY_ID AND  SG.GROUPING_CODE = FEG.GROUPING_CODE
      LEFT JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
      LEFT JOIN BOOT_DEFAULTS BD ON BD.SURVEY_ID = S.SURVEY_ID
