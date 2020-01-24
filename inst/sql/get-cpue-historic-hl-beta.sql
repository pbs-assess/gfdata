SELECT MC.BEST_DATE
	,MC.DATABASE_NAME
	,VESSEL_NAME
	,VESSEL_REGISTRATION_NUMBER
	,FISHERY_SECTOR
	,MC.TRIP_ID
	,MC.FISHING_EVENT_ID
	,MC.MAJOR_STAT_AREA_CODE
	,MC.MINOR_STAT_AREA_CODE
	,MC.LOCALITY_CODE
	,LATITUDE
	,LONGITUDE
	,MC.GEAR
	,BEST_DEPTH
	,MC.BEST_DEPTH AS BEST_DEPTH_M
	,MC.SPECIES_CODE
	,ISNULL(LANDED_KG, 0) AS LANDED_KG
	,ISNULL(DISCARDED_KG, 0) AS DISCARDED_KG
FROM GFFOS.dbo.GF_MERGED_CATCH MC
	LEFT JOIN GFFOS.dbo.LOCALITY L ON MC.LOCALITY_CODE = L.LOCALITY_CODE AND MC.MAJOR_STAT_AREA_CODE = L.MAJOR_STAT_AREA_CODE AND MC.MINOR_STAT_AREA_CODE = L.MINOR_STAT_AREA_CODE
WHERE
	MC.MAJOR_STAT_AREA_CODE IN ('03', '04', '05','06','07','08','09')
		AND MC.GEAR IN ('HOOK AND LINE')
		AND FISHERY_SECTOR IN ('ZN', 'ROCKFISH OUTSIDE', 'HALIBUT', 'HALIBUT AND SABLEFISH', 'SABLEFISH')

	-- insert species here
	-- insert major here
