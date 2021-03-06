/****** Script for SelectTopNRows command from SSMS  ******/

-- =========================================================================================================
-- Create date:      September 2019
-- Database origin:  Sablefish.dbo.om_landCatchMatrix  Procedure: Build_Operating_Model_Data
-- Description:	     Sablefish Landings by Year
-- Fields:           YEAR        - year 
---                  tStep       - time step
---                  Trap        - landings by trap gear
---                  Longline    - landings by longline gear
---                  Trawl       - landings by trawl gear
---                  Trawl_SubL  - new in 2017, sublegal sablefish landed in OPT A - HAKE QUOTA (SHORESIDE)
---                                fishery, classified in GFFOS in catch category ‘other’
---                  ResTrapStd  - landings from trap survey on standardized sets
---                  ResTrapStrs - landings from trap survey on Random Stratified sets
-- =========================================================================================================

SELECT [YEAR]
      ,[tStep]
      ,[Trap]
      ,[Longline]
      ,[Trawl]
      ,[Trawl_SubL]
      ,[ResTrapStd]
      ,[ResTrapStrs]
  FROM [Sablefish].[dbo].[om_landCatchMatrix]
