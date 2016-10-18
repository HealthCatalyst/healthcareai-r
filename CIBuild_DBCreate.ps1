
# Push artifacts up
#Push-AppveyorArtifact SAM.mdf
#Push-AppveyorArtifact SAM_log.ldf

$env:APPVEYOR_BUILD_FOLDER="C:\projects\HCRTools\"

# Attach mdf to local instance
$mdfFile = "C:\projects\HCRTools\SAM.mdf"
$ldfFile = "C:\projects\HCRTools\SAM_log.ldf"

sqlcmd -S "(local)\SQL2012SP1" -Q "CREATE DATABASE [SAM2] ON (FILENAME = '$mdfFile'), (FILENAME = '$ldfFile') for ATTACH"
