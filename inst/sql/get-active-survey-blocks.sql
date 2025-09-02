SELECT
	SS.SURVEY_SERIES_ID,
	SSS.SURVEY_SERIES_NAME,
	SS.BLOCK_DESIGNATION,
	SS.GROUPING_CODE,
	SS.GEOM.STPointN(1).Lat AS PT1_LAT,
	SS.GEOM.STPointN(1).Long AS PT1_LON,
	SS.GEOM.STPointN(2).Lat AS PT2_LAT,
	SS.GEOM.STPointN(2).Long AS PT2_LON,
	SS.GEOM.STPointN(3).Lat AS PT3_LAT,
	SS.GEOM.STPointN(3).Long AS PT3_LON,
	SS.GEOM.STPointN(4).Lat AS PT4_LAT,
	SS.GEOM.STPointN(4).Long AS PT4_LON,
	SS.LATITUDE,
	SS.LONGITUDE,
	SS.DEPTH_M AS SURVEY_SITE_DEPTH_M,
	SS.SELECTION_IND
	--SS.FE_MAJOR_LEVEL_ID,
	--SS.STATUS_CODE -- maybe add this when adding in history of blocks?
FROM SURVEY_SITE ss
INNER JOIN SURVEY_SERIES SSS
        ON SS.SURVEY_SERIES_ID=SSS.SURVEY_SERIES_ID
-- insert ssid here
-- AND SS.SELECTION_IND = 1 /*this line selects only the currently active blocks*/
/*Notes:
	- 2 (Hecate Strait Multispecies Assemblage Bottom Trawl) is not in SURVEY_SITES
	- 6 and 7 (Multispecies bottom trawl (QCS and WCVI)) are input as SURVEY_SERIES_ID = 67
		but this grid is best generated from sampling points if to be used in modelling
	- 14 (IPHC) has only 4 large polygons spanning IPHC survey area, not particularly useful
*/
