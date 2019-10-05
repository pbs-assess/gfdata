SELECT
  DATABASE_NAME,
  MC.TRIP_ID,
  MC.FISHING_EVENT_ID,
  FISHERY_SECTOR,
  TRIP_CATEGORY,
  GEAR,
  BEST_DATE,
  FE_START_DATE,
  FE_END_DATE,
  LAT,
  LON,
  BEST_DEPTH,
  MC.SPECIES_CODE,
  SPECIES_SCIENTIFIC_NAME,
  SPECIES_COMMON_NAME,
  LANDED_KG,
  CASE WHEN GEAR IN ('TRAP', 'HOOK AND LINE', 'MIDWATER TRAWL') AND YEAR(BEST_DATE) < 2006 THEN 0
	WHEN GEAR IN ('BOTTOM TRAWL', 'UNKNOWN TRAWL') AND YEAR(BEST_DATE) <1996 THEN 0
	WHEN TRIP_CATEGORY IN ('OPT B') AND YEAR(BEST_DATE) <2006 THEN 0
	ELSE DISCARDED_KG END AS DISCARDED_KG,
  LANDED_PCS,
  DISCARDED_PCS,
  MC.MAJOR_STAT_AREA_CODE,
  MC.MINOR_STAT_AREA_CODE,
	MSA.MAJOR_STAT_AREA_NAME,
  VESSEL_NAME,
  VESSEL_REGISTRATION_NUMBER
FROM GFFOS.dbo.GF_MERGED_CATCH MC
	INNER JOIN GFFOS.dbo.SPECIES SP ON SP.SPECIES_CODE = MC.SPECIES_CODE
	INNER JOIN GFBioSQL.dbo.MAJOR_STAT_AREA MSA ON MC.MAJOR_STAT_AREA_CODE = MSA.MAJOR_STAT_AREA_CODE
	LEFT JOIN (SELECT
		TRIP_ID
		,TRIP_CATEGORY
		,FISHING_EVENT_ID
		,LAT
		,LON
	FROM GFFOS.dbo.GF_D_OFFICIAL_FE_CATCH
		GROUP BY TRIP_ID, TRIP_CATEGORY, FISHING_EVENT_ID, LAT, LON) C ON CAST (MC.TRIP_ID AS VARCHAR) = CAST (C.TRIP_ID AS VARCHAR) AND MC.FISHING_EVENT_ID = C.FISHING_EVENT_ID
-- insert species here
ORDER BY BEST_DATE, SPECIES_COMMON_NAME

