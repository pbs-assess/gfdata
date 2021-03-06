SELECT YEAR(BEST_DATE) YEAR
	,FISHERY_SECTOR
	,VESSEL_REGISTRATION_NUMBER VRN
	,VESSEL_NAME
	,GEAR
	,LONGLINE_GEAR_TYPE_NAME
	,LLGS.FISHING_EVENT_ID
	,BEST_DEPTH
	,MAJOR_STAT_AREA_CODE
	,MINOR_STAT_AREA_CODE
	,LOCALITY_CODE
	,LATITUDE
	,LONGITUDE
	,NUMBER_GEAR_SET - ISNULL(NUMBER_GEAR_LOST, 0) skates_obs
	,HOOK_TRAP_SPACING_FT ft_per_hook
	,SKATE_LENGTH_FT skate_ft
	,HOOKS_TRAPS_PER_SKATE
	,SKATE_LENGTH_FT/HOOK_TRAP_SPACING_FT calc_hooks_per_skate
	,CASE WHEN HOOKS_TRAPS_PER_SKATE IS NOT NULL 
		THEN HOOKS_TRAPS_PER_SKATE
		ELSE SKATE_LENGTH_FT/HOOK_TRAP_SPACING_FT 
		END	hooks_per_skate
		--below calculates the hooks per fe (set) by multiplying the observed skates per set (skates set - skates lost) by the reported hooks per skate
		--when number of hooks per skate isn't given, it is calculated by the skate length in ft / hook spacing (ft per hook) if available
	,(NUMBER_GEAR_SET - ISNULL(NUMBER_GEAR_LOST, 0)) * CAST(
		CASE WHEN HOOKS_TRAPS_PER_SKATE IS NOT NULL 
			THEN HOOKS_TRAPS_PER_SKATE
			ELSE SKATE_LENGTH_FT/HOOK_TRAP_SPACING_FT 
			END AS BIGINT) hooks_per_fe
FROM GFFOS.dbo.GF_FE_LONGLINE_GEAR_SPECS LLGS
	LEFT JOIN GFFOS.dbo.GF_MERGED_CATCH C ON LLGS.FISHING_EVENT_ID = C.FISHING_EVENT_ID
	INNER JOIN GFFOS.dbo.LONGLINE_GEAR_TYPE LLGT ON LLGT.LONGLINE_GEAR_TYPE_CODE = LLGS.LONGLINE_GEAR_TYPE_CODE
WHERE GEAR = 'HOOK AND LINE'
-- insert filters here
GROUP BY YEAR(BEST_DATE)
	,FISHERY_SECTOR
	,VESSEL_REGISTRATION_NUMBER
	,VESSEL_NAME
	,GEAR
	,LONGLINE_GEAR_TYPE_NAME
	,LLGS.FISHING_EVENT_ID
	,BEST_DEPTH
	,MAJOR_STAT_AREA_CODE
	,MINOR_STAT_AREA_CODE
	,LOCALITY_CODE
	,LATITUDE
	,LONGITUDE
	,NUMBER_GEAR_SET 
	,NUMBER_GEAR_LOST 
	,HOOKS_TRAPS_PER_SKATE 
	,HOOK_TRAP_SPACING_FT 
	,SKATE_LENGTH_FT 

	ORDER BY skates_obs DESC
