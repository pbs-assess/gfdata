SELECT SURVEY_DESC,
	FESD.FISHING_EVENT_ID, 
	SENSOR_DATA_ATTRIBUTE_DESC AS ATTRIBUTE,
	SENSOR_NAME,
	TIME_STAMP,
	SD.SENSOR_DATA_VALUE,
	CASE SENSOR_DATA_UNIT_ABBR WHEN '(�C)' THEN 'C' WHEN 'ml/L' THEN 'mlpL' WHEN 'S/m' THEN 'Spm' ELSE SENSOR_DATA_UNIT_ABBR END AS UNIT
FROM  FISHING_EVENT_SENSOR_DATAFILE FESD 
	INNER JOIN SENSOR_DATA SD ON FESD.SENSOR_DATAFILE_NAME = SD.SENSOR_DATAFILE_NAME
	INNER JOIN FISHING_EVENT FE ON FESD.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
	INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
	INNER JOIN SURVEY S ON TS.SURVEY_ID = S.SURVEY_ID
	INNER JOIN SENSOR_DATA_ATTRIBUTE A ON SD.SENSOR_DATA_ATTRIBUTE_CODE = A.SENSOR_DATA_ATTRIBUTE_CODE
	INNER JOIN SENSOR_DATA_UNIT U ON SD.SENSOR_DATA_UNIT_CODE = U.SENSOR_DATA_UNIT_CODE
	INNER JOIN SENSOR_FILE_TYPE SFT ON SD.SENSOR_FILE_TYPE_CODE = SFT.SENSOR_FILE_TYPE_CODE
	INNER JOIN SENSOR SC ON SFT.SENSOR_CODE = SC.SENSOR_CODE	
WHERE SURVEY_SERIES_ID IN (1, 2, 3, 4, 16, 80, 88)  
	AND ORIGINAL_IND = 'Y'
	AND TIME_STAMP 
		BETWEEN CASE WHEN SURVEY_SERIES_ID IN (80, 88) 
					THEN FE_END_DEPLOYMENT_TIME ELSE FE_BEGIN_BOTTOM_CONTACT_TIME END 
		AND CASE WHEN SURVEY_SERIES_ID IN (80, 88)
					THEN FE_BEGIN_RETRIEVAL_TIME ELSE FE_END_BOTTOM_CONTACT_TIME  END
-- insert fishing event id here
-- insert attribute here
-- insert sensor name here
	AND SD.SENSOR_DATA_UNIT_CODE NOT IN(3) -- Remove volts as possible DO unit
GROUP BY YEAR(FE_BEGIN_RETRIEVAL_TIME), 
	SURVEY_SERIES_ID,
	SURVEY_DESC,
	FESD.FISHING_EVENT_ID, 
	SENSOR_DATA_ATTRIBUTE_DESC,
	SENSOR_NAME,
	SENSOR_DATA_UNIT_ABBR,
	TIME_STAMP,
	SENSOR_DATA_VALUE

