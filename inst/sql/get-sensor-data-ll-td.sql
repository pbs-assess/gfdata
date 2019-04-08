--This query only extracts depth and temperature data from sensors deployed (up to 3) on hooks
-- on the HBLL Inside surveys.  CTD data are extracted separately, and are collected
-- approximately 5 m off the bottom (whereas all data collected with sensors deployed directly 
-- on trawl and long line survey gear are considered bottom data). CTD data can be linked 
-- to sensor data from fishing sets in the same block by block designation.

-- Attribute values are averaged over all measurments recorded between 
-- when gear deployment was complete and gear retrieval began.
SELECT YEAR(FE_BEGIN_RETRIEVAL_TIME) AS YEAR
	,SURVEY_SERIES_ID AS SSID
	,SURVEY_DESC 
	,BLOCK_DESIGNATION
	,LINE AS FISHING_EVENT_ID
	,SENSOR_DATA_ATTRIBUTE_DESC AS ATTRIBUTE
	,MIN(SD.TIME_STAMP) AS START_TIME
	,MAX(SD.TIME_STAMP) AS END_TIME
	,MIN(SD.SENSOR_DATA_VALUE) AS MIN
	,AVG(SD.SENSOR_DATA_VALUE) AS AVG
	,MAX(SD.SENSOR_DATA_VALUE) AS MAX
	,COUNT(SD.SENSOR_DATA_VALUE) AS COUNT
	,CASE WHEN SENSOR_DATA_UNIT_ABBR = '(�C)' THEN 'C' ELSE SENSOR_DATA_UNIT_ABBR END AS UNIT
FROM FISHING_EVENT FE

--SKATE ID
	INNER JOIN (
		SELECT FE_PARENT_EVENT_ID AS LINE,
			FISHING_EVENT_ID AS SKATE
		FROM FISHING_EVENT FE
		WHERE FE_MINOR_LEVEL_ID IS NULL AND FE_PARENT_EVENT_ID IS NOT NULL
		) SKATE ON FE.FISHING_EVENT_ID = SKATE.LINE

--HOOK ID FOR HOOK SENSORS WITH SKATE ID
	INNER JOIN (
		SELECT FESD.FISHING_EVENT_ID AS HOOK,
			FE_PARENT_EVENT_ID AS SKATE
		FROM FISHING_EVENT FE
			INNER JOIN FISHING_EVENT_SENSOR_DATAFILE FESD ON FE.FISHING_EVENT_ID = FESD.FISHING_EVENT_ID
		WHERE FE_MINOR_LEVEL_ID IS NOT NULL
		) H ON SKATE.SKATE = H.SKATE
		
	INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
	INNER JOIN SURVEY S ON TS.SURVEY_ID = S.SURVEY_ID
	INNER JOIN FISHING_EVENT_SENSOR_DATAFILE FESD ON H.HOOK = FESD.FISHING_EVENT_ID
	INNER JOIN SENSOR_DATA SD ON FESD.SENSOR_DATAFILE_NAME = SD.SENSOR_DATAFILE_NAME
	INNER JOIN SENSOR_DATA_ATTRIBUTE A ON SD.SENSOR_DATA_ATTRIBUTE_CODE = A.SENSOR_DATA_ATTRIBUTE_CODE
	INNER JOIN SENSOR_DATA_UNIT U ON SD.SENSOR_DATA_UNIT_CODE = U.SENSOR_DATA_UNIT_CODE
WHERE SURVEY_SERIES_ID IN (14, 22, 36, 39, 40)  
-- insert ssid here
-- insert attribute here
	AND SD.SENSOR_DATA_UNIT_CODE NOT IN(3) -- Remove volts as possible DO unit
	AND TIME_STAMP BETWEEN FE_END_DEPLOYMENT_TIME AND FE_BEGIN_RETRIEVAL_TIME
	AND ORIGINAL_IND = 'Y'
GROUP BY FE_BEGIN_RETRIEVAL_TIME 
	,SURVEY_SERIES_ID
	,SURVEY_DESC
	,FE.TRIP_ID
	,LINE	
	,SENSOR_DATA_ATTRIBUTE_DESC
	,SENSOR_DATA_UNIT_ABBR
	,BLOCK_DESIGNATION
ORDER BY FISHING_EVENT_ID

