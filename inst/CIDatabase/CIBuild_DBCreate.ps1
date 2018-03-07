sqlcmd -b -S "(local)\SQL2012SP1" -Q "
CREATE DATABASE testSAM
GO

CREATE TABLE testSAM.dbo.hcai_unit_tests(
	id INT NULL,
	word_of_day varchar(255) NULL
)
GO

INSERT INTO testSAM.dbo.hcai_unit_tests VALUES
(1, 'bagel'),
(2, 'box'),
(3, 'toaster')
GO



USE testSAM
GO

CREATE SCHEMA test_schema
	CREATE TABLE hcai_unit_tests(
		id INT NULL,
		word_of_day varchar(255) NULL
	)
GO

INSERT INTO testSAM.test_schema.hcai_unit_tests VALUES
	(4, 'lentil'),
	(5, 'automobile'),
	(6, 'towel')
GO
"

