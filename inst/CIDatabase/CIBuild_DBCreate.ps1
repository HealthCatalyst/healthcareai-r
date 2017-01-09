# Push artifacts up
Push-AppveyorArtifact inst/extdata/HCRDiabetesClinical.csv

# Use csv file to create SAM db and populate tables
$csvFile = "c:\projects\healthcareai-r\inst\extdata\HCRDiabetesClinical.csv"

sqlcmd -b -S "(local)\SQL2012SP1" -Q "CREATE DATABASE [SAM_test_for_R]"
sqlcmd -S "(local)\SQL2012SP1" -Q "exec sp_databases"

# Write the tables
sqlcmd -S "(local)\SQL2012SP1" -Q "
USE SAM_test_for_R;
CREATE TABLE HCRDeployClassificationBASE(
    [BindingID] [int] NULL,
    [BindingNM] [varchar](255) NULL,
    [LastLoadDTS] [datetime2](7) NULL,
    [PatientEncounterID] [decimal](38, 0) NULL,
    [PredictedProbNBR] [decimal](38, 2) NULL,
    [Factor1TXT] [varchar](255) NULL,
    [Factor2TXT] [varchar](255) NULL,
    [Factor3TXT] [varchar](255) NULL
)

CREATE TABLE [SAM_test_for_R].[dbo].[HCRDeployRegressionBASE](
    [BindingID] [int] NULL,
    [BindingNM] [varchar](255) NULL,
    [LastLoadDTS] [datetime2](7) NULL,
    [PatientEncounterID] [decimal](38, 0) NULL,
    [PredictedValueNBR] [decimal](38, 2) NULL,
    [Factor1TXT] [varchar](255) NULL,
    [Factor2TXT] [varchar](255) NULL,
    [Factor3TXT] [varchar](255) NULL
)

CREATE TABLE [SAM_test_for_R].[dbo].[HCRDiabetesClinical](
    [PatientEncounterID] [float] NULL,
    [PatientID] [float] NULL,
    [SystolicBPNBR] [float] NULL,
    [LDLNBR] [float] NULL,
    [A1CNBR] [float] NULL,
    [GenderFLG] [nvarchar](255) NULL,
    [ThirtyDayReadmitFLG] [nvarchar](255) NULL,
    [InTestWindowFLG] [nvarchar](255) NULL
)

CREATE TABLE [SAM_test_for_R].[dbo].[HCRWriteData](
    [a] [float] NULL,
    [b] [float] NULL,
    [c] [varchar](255) NULL
)"

sqlcmd -S "(local)\SQL2012SP1" -Q "exec sp_tables"


# Look at contents of a few relevant directories
Get-ChildItem -Path c:\projects\healthcareai-r\inst\CIDatabase
Get-ChildItem -Path c:\projects\healthcareai-r
Get-ChildItem -Path c:\projects\

