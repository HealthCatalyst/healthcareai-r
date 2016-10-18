
# Push artifacts up
Push-AppveyorArtifact inst/CIDatabase/SAM.mdf
Push-AppveyorArtifact inst/CIDatabase/SAM_log.ldf

# Attach mdf to local instance
$mdfFile = "inst\CIDatabase\SAM.mdf"
$ldfFile = "inst\CIDatabase\SAM_log.ldf"

sqlcmd -S "(local)\SQL2012SP1" -Q "CREATE DATABASE [SAM] ON (FILENAME = '$mdfFile'), (FILENAME = '$ldfFile') for ATTACH"
