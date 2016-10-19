
# Push artifacts up
#Push-AppveyorArtifact inst/CIDatabase/SAM.mdf
#Push-AppveyorArtifact inst/CIDatabase/SAM_log.ldf

# Attach mdf to local instance
#$mdfFile = "c:\projects\HCRTools\inst\CIDatabase\SAM.mdf"
#$ldfFile = "c:\projects\HCRTools\inst\CIDatabase\SAM_log.ldf"

$mdfFile = "c:\projects\HCRTools\SAM.mdf"
$ldfFile = "c:\projects\HCRTools\SAM_log.ldf"

sqlcmd -b -S "(local)\SQL2012SP1" -Q "CREATE DATABASE [SAM] ON (FILENAME = '$mdfFile'), (FILENAME = '$ldfFile') for ATTACH"

