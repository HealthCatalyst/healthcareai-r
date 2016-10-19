
# Push artifacts up
Push-AppveyorArtifact inst/CIDatabase/SAM.mdf
Push-AppveyorArtifact inst/CIDatabase/SAM_log.ldf

Push-AppveyorArtifact inst/CIDatabase/SAM2.mdf
Push-AppveyorArtifact inst/CIDatabase/SAM_log2.ldf

# Attach mdf to local instance
$mdfFile = "c:\projects\HCRTools\inst\CIDatabase\SAM.mdf"
$ldfFile = "c:\projects\HCRTools\inst\CIDatabase\SAM_log.ldf"

#foreach ($artifactName in $artifacts.keys) {
#  $artifacts[$artifactName]
#}

#$mdfFile = $artifacts[$SAM.mdf].path
#$ldfFile = $artifacts[$SAM_log.ldf].path

sqlcmd -b -S "(local)\SQL2012SP1" -Q "CREATE DATABASE [SAM] ON (FILENAME = '$mdfFile'), (FILENAME = '$ldfFile') for ATTACH"

Copy-Item inst/CIDatabase/SAM2.mdf inst/CIDatabase/SAM.mdf
Copy-Item inst/CIDatabase/SAM_log2.ldf inst/CIDatabase/SAM_log.ldf
