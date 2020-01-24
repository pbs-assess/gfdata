SELECT BEST_DATE
	,YEAR(BEST_DATE) YEAR
	,MONTH(BEST_DATE) MONTH
	,VESSEL_NAME
	,VESSEL_REGISTRATION_NUMBER
	,FISHERY_SECTOR
	,TRIP_ID
	,FISHING_EVENT_ID
	,FE_START_DATE
	,FE_END_DATE
	,SPECIES_CODE
	,GEAR
	,BEST_DEPTH
	,MAJOR_STAT_AREA_CODE
	,MINOR_STAT_AREA_CODE
	,LOCALITY_CODE
	,LATITUDE
	,LONGITUDE
	,ISNULL(LANDED_KG, 0) AS LANDED_KG
	,ISNULL(DISCARDED_KG, 0) AS DISCARDED_KG
FROM GFFOS.dbo.GF_MERGED_CATCH MC
WHERE MC.MAJOR_STAT_AREA_CODE IN ('03', '04', '05','06','07','08','09')
		AND MC.GEAR IN ('HOOK AND LINE')
		AND FISHERY_SECTOR IN ('ZN', 'ROCKFISH OUTSIDE', 'HALIBUT', 'HALIBUT AND SABLEFISH', 'SABLEFISH')
-- insert filters here
-- insert major here


