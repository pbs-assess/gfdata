USE [GFBioSQL]
GO

/****** Object:  StoredProcedure [dbo].[proc_new_boot_2011]    Script Date: 11/21/2017 12:08:01 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




/****** Object:  Stored Procedure dbo.proc_new_boot    Script Date: 10/23/2008 7:57:10 AM ******/

CREATE PROCEDURE [dbo].[proc_new_boot_2011] @survey_id INT, @desc VARCHAR(100)
AS
   SET NOCOUNT ON

   DECLARE @boot_id AS INT
   DECLARE @speed AS FLOAT
   DECLARE @doorspread AS FLOAT
   SELECT @doorspread = doorspread,
      @speed = speed
   FROM BOOT_DEFAULTS
   WHERE SURVEY_ID = @survey_id

   DECLARE @survey_series_id INT
   DECLARE @survey_year INT
   SELECT TOP 1 @survey_series_id = S.SURVEY_SERIES_ID,
      @survey_year = YEAR(TRIP_START_DATE)
   FROM SURVEY S
      INNER JOIN TRIP_SURVEY TS ON
      S.SURVEY_ID = TS.SURVEY_ID
      INNER JOIN TRIP T ON
      TS.TRIP_ID = T.TRIP_ID
   WHERE S.SURVEY_ID = @survey_id

	INSERT INTO BOOT_HEADER (SURVEY_ID, DEFAULT_DOORSPREAD, DEFAULT_SPEED, RUN_DESC, SURVEY_SERIES_ID, SURVEY_YEAR)
      VALUES (@survey_id, @doorspread, @speed, @desc, @survey_series_id, @survey_year)

   SET @boot_id = @@IDENTITY
   INSERT INTO BOOT_GROUPING (BOOT_ID, GROUPING_CODE, AREA_KM2)
   SELECT @boot_id AS BOOT_ID, G.GROUPING_CODE, G.AREA_KM2
   FROM SURVEY_GROUPING SG INNER JOIN [GROUPING] G ON
      SG.GROUPING_CODE = G.GROUPING_CODE
   WHERE SG.SURVEY_ID = @survey_id

   SELECT @boot_id AS BOOT_ID



GO

