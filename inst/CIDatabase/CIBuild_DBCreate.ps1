# Push artifacts up
Push-AppveyorArtifact inst/extdata/HCRDiabetesClinical.csv

# Use csv file to create SAM db and populate tables
$csvFile = "c:\projects\healthcareai-r\inst\extdata\HCRDiabetesClinical.csv"

sqlcmd -b -S "(local)\SQL2012SP1" -Q "CREATE DATABASE [SAM_test_for_R]"
sqlcmd -S "(local)\SQL2012SP1" -Q "exec sp_databases"

# Look at contents of a few relevant directories
Get-ChildItem -Path c:\projects\healthcareai-r\inst\CIDatabase
Get-ChildItem -Path c:\projects\healthcareai-r
Get-ChildItem -Path c:\projects\

