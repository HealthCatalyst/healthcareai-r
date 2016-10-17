$dbName = "SAM"
$sqlInstance = "(local)\SQL2012SP1"

# attach mdf to local instance
$mdfFile = "SAM.mdf"
$ldfFile = "SAM_log.ldf"
sqlcmd -S $sqlInstance -Q "Use [master]; CREATE DATABASE [$dbName] ON (FILENAME = $mdfFile),(FILENAME = $ldfFile) for ATTACH"
