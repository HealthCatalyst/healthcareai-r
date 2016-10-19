# Push artifacts up
Push-AppveyorArtifact inst/CIDatabase/SAM.mdf
Push-AppveyorArtifact inst/CIDatabase/SAM_log.ldf

# Use mdf/ldf to create SAM db
$mdfFile = "c:\projects\HCRTools\inst\CIDatabase\SAM.mdf"
$ldfFile = "c:\projects\HCRTools\inst\CIDatabase\SAM_log.ldf"

sqlcmd -b -S "(local)\SQL2012SP1" -Q "CREATE DATABASE [SAM] ON (FILENAME = '$mdfFile'), (FILENAME = '$ldfFile') for ATTACH"

# Look at contents of a few relevant directories
Get-ChildItem -Path c:\projects\HCRTools\inst\CIDatabase
Get-ChildItem -Path c:\projects\HCRTools
Get-ChildItem -Path c:\projects\

