SELECT
  SS.SURVEY_SERIES_ID,
  A.ACTIVITY_DESC,
  TA.ACTIVITY_CODE,
  SM.FISHING_EVENT_ID,
  YEAR(TRIP_START_DATE) AS YEAR,
  TRIP_START_DATE,
  SM.SPECIES_CODE,
  SPP.SPECIES_COMMON_NAME,
  SPP.SPECIES_SCIENCE_NAME,
  SP.SPECIMEN_ID,
  SM.SAMPLE_ID,
  SP.SPECIMEN_SEX_CODE AS SEX,
  CASE WHEN SC.SPECIMEN_COLLECTED_IND = 'Y' OR SC.SPECIMEN_COLLECTED_IND = 'y' THEN 1 ELSE 0 END AS AGE_SPECIMEN_COLLECTED,
  SPECIMEN_AGE AS AGE,
  CASE WHEN SPECIES_CATEGORY_CODE IN (1, 5, 6, 7) AND (SAMPLE_SOURCE_CODE IS NULL OR SAMPLE_SOURCE_CODE = 1)
		THEN 'UNSORTED'
	WHEN SPECIES_CATEGORY_CODE IN(1, 2) AND SAMPLE_SOURCE_CODE = 2
		THEN 'KEEPERS'
	WHEN SPECIES_CATEGORY_CODE = 3 AND (SAMPLE_SOURCE_CODE IS NULL OR SAMPLE_SOURCE_CODE IN(1, 2))
		THEN 'KEEPERS'
	WHEN SPECIES_CATEGORY_CODE = 1 AND SAMPLE_SOURCE_CODE = 3
		THEN 'DISCARDS'
	WHEN SPECIES_CATEGORY_CODE = 4 AND SAMPLE_SOURCE_CODE IN(1, 3)
		THEN 'DISCARDS'
	ELSE 'TBD' END AS SAMPLING_DESC,
	AGEING_METHOD_CODE,
	-- insert lengths here
  ROUND_WEIGHT AS WEIGHT,
  SP.MATURITY_CODE,
  MD.MATURITY_NAME,
  MD.MATURITY_DESC,
  SM.MATURITY_CONVENTION_CODE,
  MC.MATURITY_CONVENTION_DESC,
  MC.MATURITY_CONVENTION_MAXVALUE,
  SM.MAJOR_STAT_AREA_CODE,
  MSA.MAJOR_STAT_AREA_NAME,
  SM.MINOR_STAT_AREA_CODE,
  FE.GEAR_CODE AS GEAR,
  H.HOOK_DESC,
  HSZ.HOOKSIZE_DESC,
  R.REASON_DESC,
  TRIP_SUB_TYPE_CODE,
  SM.FE_MAJOR_LEVEL_ID,
  SM.FE_SUB_LEVEL_ID,
  SM.SAMPLE_TYPE_CODE,
  SM.SPECIES_CATEGORY_CODE,
  SM.SAMPLE_SOURCE_CODE,
  DNA.STORAGE_CONTAINER_TYPE_CODE AS DNA_SAMPLE_TYPE,
  DNA.STORAGE_CONTAINER_SUB_ID AS DNA_CONTAINER_ID,
  CASE WHEN SM.GEAR_CODE IN (1, 6, 11) THEN ISNULL(TRSP.USABILITY_CODE, 0)
  WHEN SM.GEAR_CODE IN (2) THEN ISNULL(TPSP.USABILITY_CODE, 0)
  WHEN SM.GEAR_CODE IN (5) THEN ISNULL(LLSP.USABILITY_CODE, 0)
  WHEN SM.GEAR_CODE IN (4) THEN ISNULL(HLSP.USABILITY_CODE, 0)
  ELSE 0 END AS USABILITY_CODE,
  SM.GROUPING_CODE AS SAMPLE_GROUPING_CODE,
  SSG.GROUPING_CODE AS SURVEY_GROUPING_CODE,
  G.GROUPING_DESC AS FE_GROUPING_DESC,
  S.ORIGINAL_IND
  FROM TRIP T
  INNER JOIN FISHING_EVENT FE ON F.TRIP_ID = T.TRIP_ID
  INNER JOIN GFBioSQL.dbo.B21_Samples SM ON SM.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  INNER JOIN GFBioSQL.dbo.B22_Specimens SP ON SM.SAMPLE_ID = SP.SAMPLE_ID
  INNER JOIN GFBioSQL.dbo.SPECIES SPP ON SPP.SPECIES_CODE = SM.SPECIES_CODE
  LEFT JOIN GFBioSQL.dbo.GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
  LEFT JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = G.SURVEY_SERIES_ID
  LEFT JOIN GFBioSQL.dbo.SURVEY S ON S.SURVEY_SERIES_ID = G.SURVEY_SERIES_ID
  LEFT JOIN GFBioSQL.dbo.SURVEY_SERIES_TYPE SST ON SST.SURVEY_SERIES_TYPE_CODE = SS.SURVEY_SERIES_TYPE_CODE
  LEFT JOIN (
    SELECT SURVEY_SERIES_ID, GROUPING_CODE
    FROM GFBioSQL.dbo.SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_GROUPING SG ON S.SURVEY_ID = SG.SURVEY_ID
    GROUP BY SURVEY_SERIES_ID, GROUPING_CODE
  ) SSG ON SSG.GROUPING_CODE = FE.GROUPING_CODE AND SSG.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
  LEFT JOIN GFBioSQL.dbo.REASON R ON FE.REASON_CODE = R.REASON_CODE
  LEFT JOIN GFBioSQL.dbo.TRIP_ACTIVITY TA ON TA.TRIP_ID = SM.TRIP_ID
  LEFT JOIN GFBioSQL.dbo.ACTIVITY A ON A.ACTIVITY_CODE = TA.ACTIVITY_CODE
  LEFT JOIN GFBioSQL.dbo.Maturity_Convention MC ON SM.MATURITY_CONVENTION_CODE = MC.MATURITY_CONVENTION_CODE
  LEFT JOIN GFBioSQL.dbo.MAJOR_STAT_AREA MSA ON SM.MAJOR_STAT_AREA_CODE = MSA.MAJOR_STAT_AREA_CODE
  LEFT JOIN GFBioSQL.dbo.MATURITY_DESCRIPTION MD ON SM.MATURITY_CONVENTION_CODE = MD.MATURITY_CONVENTION_CODE AND SP.MATURITY_CODE = MD.MATURITY_CODE AND SP.SPECIMEN_SEX_CODE = MD.SPECIMEN_SEX_CODE
  LEFT JOIN GFBioSQL.dbo.TRAWL_SPECS TRSP ON TRSP.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN GFBioSQL.dbo.TRAP_SPECS TPSP ON TPSP.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN GFBioSQL.dbo.LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN GFBioSQL.dbo.HANDLINE_SPECS HLSP ON HLSP.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN HOOK H ON H.HOOK_CODE = LLSP.HOOK_CODE
  LEFT JOIN HOOKSIZE HSZ ON HSZ.HOOKSIZE_CODE = LLSP.HOOKSIZE_CODE
  LEFT JOIN (SELECT SAMPLE_ID, MIN(SPECIMEN_ID) AS SPECIMEN_ID, COLLECTED_ATTRIBUTE_CODE, STORAGE_CONTAINER_TYPE_CODE, STORAGE_CONTAINER_SUB_ID
		FROM GFBioSQL.dbo.SPECIMEN_COLLECTED
		WHERE COLLECTED_ATTRIBUTE_CODE BETWEEN 3.5 AND 4.5
		GROUP BY SAMPLE_ID, SPECIMEN_ID, COLLECTED_ATTRIBUTE_CODE, STORAGE_CONTAINER_TYPE_CODE, STORAGE_CONTAINER_SUB_ID) DNA ON SP.SPECIMEN_ID = DNA.SPECIMEN_ID AND SP.SAMPLE_ID = DNA.SAMPLE_ID
  LEFT JOIN (SELECT SAMPLE_ID, MIN(SPECIMEN_ID) AS SPECIMEN_ID, SPECIMEN_COLLECTED_IND
		FROM GFBioSQL.dbo.SPECIMEN_COLLECTED
		WHERE COLLECTED_ATTRIBUTE_CODE BETWEEN 20 AND 25
		GROUP BY SAMPLE_ID, SPECIMEN_ID, SPECIMEN_COLLECTED_IND) SC ON SP.SPECIMEN_ID = SC.SPECIMEN_ID AND SP.SAMPLE_ID = SC.SAMPLE_ID
WHERE FE.FE_PARENT_EVENT_ID IS NULL AND TRIP_SUB_TYPE_CODE IN (2, 3) AND
  SS.SURVEY_SERIES_ID <> 0
-- insert species here
-- insert ssid here
-- insert major here
ORDER BY SS.SURVEY_SERIES_ID, YEAR(TRIP_START_DATE)
