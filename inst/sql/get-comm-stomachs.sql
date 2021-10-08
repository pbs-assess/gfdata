SELECT TRIP_START_DATE,
  SM.TRIP_ID,
  SM.FISHING_EVENT_ID,
  SM.GEAR_CODE,
  GEAR_DESC,
  SM.SPECIES_CODE,
  SPP.SPECIES_COMMON_NAME,
  SPP.SPECIES_SCIENCE_NAME,
  ST.SAMPLE_ID,
  ST.SPECIMEN_ID,
  SP.SPECIMEN_SEX_CODE AS SEX,
  SPECIMEN_AGE AS AGE,
  AGEING_METHOD_CODE,
  ROUND_WEIGHT AS WEIGHT,
  ST.SPECIES_CODE AS PREY_CODE,
  SP2.SPECIES_COMMON_NAME AS PREY_SPECIES,
  SP2.SPECIES_SCIENCE_NAME AS PREY_SCIENCE_NAME,
  ST.DIGESTION_STATE_CODE,
  ST.PREY_VOLUME,
  ST.PREY_COUNT,
  ST.PREY_WEIGHT,
  ST.PREY_LENGTH_MM,
  ST.SPECIMEN_STOMACH_COMMENT,
  ST.MODIFIED_BY,
  SP.MATURITY_CODE,
  SM.MATURITY_CONVENTION_CODE,
  MC.MATURITY_CONVENTION_DESC,
  SM.TRIP_SUB_TYPE_CODE,
  TRIP_SUB_TYPE_DESC,
  SM.SAMPLE_TYPE_CODE,
  SM.SAMPLE_WEIGHT,
  SM.SPECIES_CATEGORY_CODE,
  SM.SAMPLE_SOURCE_CODE,
  SM.CATCH_WEIGHT,
  SM.CATCH_COUNT,
  SM.MAJOR_STAT_AREA_CODE,
  MAJOR_STAT_AREA_NAME,
  SM.MINOR_STAT_AREA_CODE,
  CASE WHEN SM.GEAR_CODE IN (1, 6, 8, 11) THEN ISNULL(TRSP.USABILITY_CODE, 0)
  WHEN SM.GEAR_CODE IN (2) THEN ISNULL(TPSP.USABILITY_CODE, 0)
  WHEN SM.GEAR_CODE IN (5) THEN ISNULL(LLSP.USABILITY_CODE, 0)
  WHEN SM.GEAR_CODE IN (4) THEN ISNULL(HLSP.USABILITY_CODE, 0)
  ELSE 0 END AS USABILITY_CODE,
  CASE WHEN SPECIES_CATEGORY_CODE IN (0, 1, 5, 6) AND (SAMPLE_SOURCE_CODE IS NULL OR SAMPLE_SOURCE_CODE = 1)
		THEN 'UNSORTED'
	WHEN SPECIES_CATEGORY_CODE = 1 AND SAMPLE_SOURCE_CODE = 0
		THEN 'UNSORTED'
	WHEN SPECIES_CATEGORY_CODE IN(1, 2) AND SAMPLE_SOURCE_CODE = 2
		THEN 'KEEPERS'
	WHEN SPECIES_CATEGORY_CODE = 3 AND (SAMPLE_SOURCE_CODE IS NULL OR SAMPLE_SOURCE_CODE IN(1, 2))
		THEN 'KEEPERS'
	WHEN SPECIES_CATEGORY_CODE = 1 AND SAMPLE_SOURCE_CODE = 3
		THEN 'DISCARDS'
	WHEN SPECIES_CATEGORY_CODE = 4 AND SAMPLE_SOURCE_CODE IN(1, 3)
		THEN 'DISCARDS'
	ELSE 'UNKNOWN' END AS SAMPLING_DESC,
	VESSEL_ID
FROM GFBioSQL.dbo.SPECIMEN_STOMACH ST
  LEFT JOIN GFBioSQL.dbo.B22_Specimens SP ON SP.SPECIMEN_ID = ST.SPECIMEN_ID
	LEFT JOIN GFBioSQL.dbo.B21_Samples SM ON SM.SAMPLE_ID = ST.SAMPLE_ID
  LEFT JOIN GFBioSQL.dbo.SPECIES SPP ON SPP.SPECIES_CODE = SM.SPECIES_CODE
  LEFT JOIN GFBioSQL.dbo.SPECIES SP2 ON SP2.SPECIES_CODE = ST.SPECIES_CODE
  LEFT JOIN GFBioSQL.dbo.Maturity_Convention MC ON SM.MATURITY_CONVENTION_CODE = MC.MATURITY_CONVENTION_CODE
  LEFT JOIN GFBioSQL.dbo.MATURITY_DESCRIPTION MD ON SM.MATURITY_CONVENTION_CODE = MD.MATURITY_CONVENTION_CODE AND SP.MATURITY_CODE = MD.MATURITY_CODE AND SP.SPECIMEN_SEX_CODE = MD.SPECIMEN_SEX_CODE
  LEFT JOIN GFBioSQL.dbo.MAJOR_STAT_AREA MSA ON MSA.MAJOR_STAT_AREA_CODE = SM.MAJOR_STAT_AREA_CODE
	LEFT JOIN GFBioSQL.dbo.FISHING_EVENT FE ON FE.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN GFBioSQL.dbo.GEAR G ON G.GEAR_CODE = SM.GEAR_CODE
  LEFT JOIN GFBioSQL.dbo.TRAWL_SPECS TRSP ON TRSP.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN GFBioSQL.dbo.TRAP_SPECS TPSP ON TPSP.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN GFBioSQL.dbo.LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN GFBioSQL.dbo.HANDLINE_SPECS HLSP ON HLSP.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
  LEFT JOIN GFBioSQL.dbo.TRIP_SUB_TYPE TST ON TST.TRIP_SUB_TYPE_CODE = SM.TRIP_SUB_TYPE_CODE
-- insert major here
ORDER BY SAMPLING_DESC,
YEAR(TRIP_START_DATE)
