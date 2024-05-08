SELECT SPECIES_CODE
    ,COUNT(Fork_Length) Fork_Length
		,COUNT(Standard_Length) Standard_Length
		,COUNT(Total_Length) Total_Length
		,COUNT(Second_Dorsal_Length) Second_Dorsal_Length
	FROM B22_Specimens
	-- insert species here
  GROUP BY SPECIES_CODE
