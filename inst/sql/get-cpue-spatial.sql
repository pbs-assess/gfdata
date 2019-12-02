SELECT YEAR(BEST_DATE) AS YEAR, 
	BEST_DATE,
	MAJOR_STAT_AREA_CODE,
	TRIP_ID, 
	FISHING_EVENT_ID,
	LAT, 
	LON, 
	C.VESSEL_REGISTRATION_NUMBER,
	SPECIES_SCIENTIFIC_NAME, 
	SPECIES_COMMON_NAME,
	(ISNULL(LANDED_ROUND_KG,0) + ISNULL(TOTAL_RELEASED_ROUND_KG,0)) /
		(DATEDIFF(N, START_DATE, END_DATE) / 60.0) AS cpue
FROM GFFOS.dbo.GF_D_OFFICIAL_FE_CATCH C
	INNER JOIN GFFOS.dbo.SPECIES SP ON SP.SPECIES_CODE = C.SPECIES_CODE
WHERE LAT BETWEEN 47.8 AND 55 AND
  LON BETWEEN -135 AND -122 AND
  FISHERY_SECTOR = 'GROUNDFISH TRAWL' AND
  ISNULL(LANDED_ROUND_KG,0) + ISNULL(TOTAL_RELEASED_ROUND_KG,0) > 0 AND
  END_DATE > START_DATE AND 
  YEAR(START_DATE) = YEAR(END_DATE)
-- insert species here
ORDER BY YEAR, SPECIES_COMMON_NAME

