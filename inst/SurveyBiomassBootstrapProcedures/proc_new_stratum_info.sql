USE [GFBioSQL]
GO

/****** Object:  StoredProcedure [dbo].[proc_new_stratum_info]    Script Date: 11/21/2017 12:08:20 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/****** Object:  Stored Procedure dbo.proc_Stratum_Info    Script Date: 10/23/2008 7:57:09 AM ******/

CREATE PROCEDURE [dbo].[proc_new_stratum_info] @sid INT
AS
   SET NOCOUNT ON
   SELECT SG.SURVEY_ID,
      SG.GROUPING_CODE,
      SG.TARGET_ALLOCATION,
      G.AREA_KM2
   FROM SURVEY_GROUPING SG
      INNER JOIN GROUPING G ON
      SG.GROUPING_CODE= G.GROUPING_CODE
   WHERE SG.SURVEY_ID = @sid

GO

