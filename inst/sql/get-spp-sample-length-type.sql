SELECT SPECIES_CODE
	,CASE WHEN Fork_Length IS NOT NULL THEN 'Fork_Length'
		WHEN Total_Length IS NOT NULL THEN 'Total_Length'
		WHEN Second_Dorsal_Length IS NOT NULL THEN 'Second_Dorsal_Length'
		WHEN Standard_Length IS NOT NULL THEN 'Standard_Length'
		ELSE NULL END LENGTH_TYPE
    ,COUNT([SPECIMEN_ID]) COUNT
  FROM [GFBioSQL].[dbo].[B22_Specimens] 
  -- insert species here
  GROUP BY SPECIES_CODE
	,CASE WHEN Fork_Length IS NOT NULL THEN 'Fork_Length'
	WHEN Total_Length IS NOT NULL THEN 'Total_Length'
	WHEN Second_Dorsal_Length IS NOT NULL THEN 'Second_Dorsal_Length'
	WHEN Standard_Length IS NOT NULL THEN 'Standard_Length'
	ELSE NULL END
