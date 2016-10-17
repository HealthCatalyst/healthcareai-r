$startPath = "C:\Program Files\Microsoft SQL Server\MSSQL11.MSSQLSERVER\MSSQL\DATA"
$sqlInstance = "(local)\SQL2012SP1"
$dbName = "SAM"

# replace the db connection with the local instance
$config = join-path $startPath "SAM.Tests.dll.config"
$doc = (gc $config) -as [xml]
$doc.SelectSingleNode('//connectionStrings/add[@name="store"]').connectionString = "Server=$sqlInstance; Database=$dbName; Trusted_connection=true"
$doc.Save($config)

# attach mdf to local instance
$mdfFile = join-path $startPath "SAM.mdf"
$ldfFile = join-path $startPath "SAM_log.ldf"
sqlcmd -S "$sqlInstance" -Q "Use [master]; CREATE DATABASE [$dbName] ON (FILENAME = '$mdfFile'),(FILENAME = '$ldfFile') for ATTACH"
