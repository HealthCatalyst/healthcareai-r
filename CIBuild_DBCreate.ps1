
# Push artifacts up
Push-AppveyorArtifact C:\Source\DataScience\HCRTools\SAM.mdf
Push-AppveyorArtifact C:\Source\DataScience\HCRTools\SAM_log.ldf

# Attach mdf to local instance
$mdfFile = "C:\projects\HCRTools\SAM.mdf"
$ldfFile = "C:\projects\HCRTools\SAM_log.ldf"

sqlcmd -S "(local)\SQL2012SP1" -Q "CREATE DATABASE [SAM2] ON (FILENAME = '$mdfFile'), (FILENAME = '$ldfFile') for ATTACH"
