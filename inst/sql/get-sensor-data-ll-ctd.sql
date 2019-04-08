-- This query only extracts data from CTD's deployed on the HBLL Inside surveys.
-- The CTD's are not deployed directly with a particular fishing event, and so have
-- their own unique fishing event id and are collected approximately 5 m off the bottom
-- (whereas all data collected with sensors deployed directly 
-- on trawl and long line survey gear are considered bottom data). Sensor data
-- from sensors deployed on hooks with actual fishing events are extracted separately.

-- Attributes are averaged over the measurements between maximum depth as recorded
-- by the CTD and 5 m shallower.
SELECT YEAR(FE_END_DEPLOYMENT_TIME) AS YEAR
	,SURVEY_SERIES_ID AS SSID
	,SURVEY_DESC
	,BLOCK_DESIGNATION 
	,FE.FISHING_EVENT_ID
	,SENSOR_DATA_ATTRIBUTE_DESC AS ATTRIBUTE
	,MIN(SD.TIME_STAMP) AS START_TIME
	,MAX(SD.TIME_STAMP) AS END_TIME
	,MIN(SD.SENSOR_DATA_VALUE) AS MIN
	,AVG(SD.SENSOR_DATA_VALUE) AS AVG
	,MAX(SD.SENSOR_DATA_VALUE) AS MAX
	,COUNT(SD.SENSOR_DATA_VALUE) AS COUNT
	,CASE SENSOR_DATA_UNIT_ABBR WHEN '(�C)' THEN 'C' WHEN 'ml/L' THEN 'mlpL' WHEN 'S/m' THEN 'Spm' ELSE SENSOR_DATA_UNIT_ABBR END AS UNIT
	,MAX_SENSOR_DEPTH
	,[FE_MAX_BOTTOM_DEPTH]
FROM FISHING_EVENT FE	
	INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
	INNER JOIN SURVEY S ON TS.SURVEY_ID = S.SURVEY_ID
	INNER JOIN FISHING_EVENT_SENSOR_DATAFILE FESD ON FE.FISHING_EVENT_ID = FESD.FISHING_EVENT_ID
	INNER JOIN SENSOR_DATA SD ON FESD.SENSOR_DATAFILE_NAME = SD.SENSOR_DATAFILE_NAME
	INNER JOIN SENSOR_DATA_ATTRIBUTE A ON SD.SENSOR_DATA_ATTRIBUTE_CODE = A.SENSOR_DATA_ATTRIBUTE_CODE
	INNER JOIN SENSOR_DATA_UNIT U ON SD.SENSOR_DATA_UNIT_CODE = U.SENSOR_DATA_UNIT_CODE

	-- SELECT THE TIME STAMPS FOR WHICH SENSOR READINGS ARE WITHIN 5 M FROM MAX_DEPTH
	INNER JOIN (
		SELECT FESD.FISHING_EVENT_ID, TIME_STAMP, MAX_SENSOR_DEPTH
		FROM SENSOR_DATA SD
			INNER JOIN FISHING_EVENT_SENSOR_DATAFILE FESD ON SD.SENSOR_DATAFILE_NAME = FESD.SENSOR_DATAFILE_NAME
			
			-- CALCULATE MAX DEPTH OF THE SENSOR FOR EACH FISHING EVENT
			LEFT JOIN (
				SELECT FISHING_EVENT_ID,
					MAX(SENSOR_DATA_VALUE) AS MAX_SENSOR_DEPTH
				FROM FISHING_EVENT_SENSOR_DATAFILE FESD
					INNER JOIN SENSOR_DATA SD ON FESD.SENSOR_DATAFILE_NAME = SD.SENSOR_DATAFILE_NAME
				WHERE SENSOR_DATA_ATTRIBUTE_CODE = 2
				GROUP BY FISHING_EVENT_ID 
				) D ON FESD.FISHING_EVENT_ID = D.FISHING_EVENT_ID
			
		WHERE SD.SENSOR_DATA_ATTRIBUTE_CODE = 2 AND SENSOR_DATA_VALUE > MAX_SENSOR_DEPTH-5 AND TIME_STAMP IS NOT NULL
		GROUP BY FESD.FISHING_EVENT_ID, TIME_STAMP, MAX_SENSOR_DEPTH

		) TD ON SD.TIME_STAMP = TD.TIME_STAMP AND FESD.FISHING_EVENT_ID = TD.FISHING_EVENT_ID
	
		
WHERE SURVEY_SERIES_ID IN (14, 22, 36, 39, 40)
	AND FE_PARENT_EVENT_ID IS NULL
-- insert ssid here
-- insert attribute here
	AND SD.SENSOR_DATA_UNIT_CODE NOT IN(3) -- Remove volts as possible DO unit
	AND ORIGINAL_IND = 'Y' 
GROUP BY FE_END_DEPLOYMENT_TIME 
	,SURVEY_SERIES_ID
	,SURVEY_DESC
	,FE.FISHING_EVENT_ID
	,BLOCK_DESIGNATION
	,SENSOR_DATA_ATTRIBUTE_DESC
	,SENSOR_DATA_UNIT_ABBR
	,MAX_SENSOR_DEPTH
	,FE_MAX_BOTTOM_DEPTH
ORDER BY FISHING_EVENT_ID

