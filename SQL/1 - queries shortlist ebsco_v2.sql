/* 	Scripts below are optimized for Microsoft SQL Server, not tested in other relational databases.
	IMPORTANT: Database setup was done using Case Insensitive and Accent Insensitive configuration.
 */

/* STEP 1 - Import EBSCOHost XML results file through your preferred import method.
			In this example, XML was imported in a table called MeuXML as raw data, in a single line.
			This was done in order to avoid processing issues due to combination of file import and data extration in a single step.
 */

/* STEP 2 - Convert raw data into different lines - 1 row per ocurrence result*/
IF EXISTS (SELECT 1 FROM sys.tables WHERE name = 'Dados')
	TRUNCATE TABLE Dados
ELSE
	CREATE TABLE [Dados](
		[rec] [xml] NULL
	)
GO

INSERT INTO Dados
SELECT Dados.rec.query('.') rec FROM 
	(SELECT Dados FROM MeuXml) AS MeuXml(Dados)
	 CROSS APPLY Dados.nodes('records/rec') AS Dados(rec)

/* STEP 3 - Extract metadata from XML row results */
IF EXISTS (SELECT 1 FROM sys.tables WHERE name = 'resultados')
	TRUNCATE TABLE resultados
ELSE
	CREATE TABLE [dbo].[resultados](
		[resultTd] [int] NULL,
		[title] [varchar](500) NULL,
		[author] [varchar](max) NULL,
		[journalName] [varchar](200) NULL,
		[journalISSN] [varchar](30) NULL,
		[pubYear] [int] NULL,
		[refType] [varchar](10) NULL,
		[refID] [varchar](50) NULL,
		[abstract] [varchar](max) NULL,
		[urlEbsco] [varchar](max) NULL,
		[isMA] [bit] NULL,
		[isShortList] [int] NULL
	)
GO

INSERT INTO resultados
SELECT 
	Dados.rec.value('(rec/@resultID)[1]','int') resultTd,

	Dados.rec.value('(rec/header/controlInfo/artinfo/tig/atl/text())[1]','varchar(500)') title,
	REPLACE(REPLACE(REPLACE(CAST(Dados.rec.query('rec/header/controlInfo/artinfo/aug/au') AS NVARCHAR(MAX)),'</au><au>','; '),'</au>',''),'<au>','') author,
	--Dados.rec.query('rec/header/controlInfo/artinfo/aug/au') author,
	--CAST(Dados.rec.query('rec/header/controlInfo/artinfo/aug/au') AS NVARCHAR(4000)) author2,
	Dados.rec.value('(rec/header/controlInfo/jinfo/jtl/text())[1]','varchar(200)') journalName,
	Dados.rec.value('(rec/header/controlInfo/jinfo/issn/text())[1]','varchar(30)') journalISSN,

	Dados.rec.value('(rec/header/controlInfo/pubinfo/dt/@year)[1]','int') pubYear,
	Dados.rec.value('(rec/header/controlInfo/artinfo/ui/@type)[1]','varchar(10)') refType,
	Dados.rec.value('(rec/header/controlInfo/artinfo/ui/text())[1]','varchar(50)') refID,

	Dados.rec.value('(rec/header/controlInfo/artinfo/ab/text())[1]','varchar(MAX)') abstract,

	Dados.rec.value('(rec/header/displayInfo/pLink/url/text())[1]','varchar(MAX)') urlEbsco

	--INTO teste1
	--MeuXml.rec.header.displayinfo.plink.query('url'),
	--MeuXml.* 
FROM 
	Dados
WHERE
	1 = 1
	AND Dados.rec.exist('rec/header/controlInfo/jinfo/issn[@type cast as xs:string ? eq "print"]') <> 1
	AND Dados.rec.exist('rec/header/controlInfo/artinfo/pubtype[text()[1] cast as xs:string ? eq "Academic Journal"]') = 1

-- STEP 4 - Add column to filter results not related to M&A, Merger and/or Acquisition keywords
ALTER TABLE resultados ADD isMA BIT default 1

UPDATE resultados SET isMA = 1

-- Database set as Case Insensitive and Accent Insensitive
UPDATE resultados SET isMA = 0
WHERE (title NOT LIKE '%merger%' AND title NOT LIKE '%m&a%' AND title NOT like '%acquisition%') OR (abstract NOT LIKE '%merge%' AND abstract NOT LIKE '%M&A%' AND abstract NOT LIKE '%acquisition%'

-- STEP 5 - Adjust journal names between EBSCO results and SCImago results
UPDATE scimagojr2020 SET title = replace(title, '"','')

UPDATE resultados SET 
	journalName = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(journalName,' (John Wiley & Sons, Inc.)',''),' (Wiley-Blackwell)',''),' (Wiley)',''),' (English Edition)',''),' (2076-3387)',''),' (John Wiley & Sons, Inc)','')

UPDATE resultados SET
	journalName = 'MIS Quarterly: Management Information Systems'
WHERE
	journalName = 'MIS Quarterly'


UPDATE 
	resultados 
SET 
	journalName = 'Journal of Artificial Societies & Social Simulation (JASSS)'
WHERE
	journalName = 'Journal of Artificial Societies & Social Simulation'



UPDATE 
	resultados 
SET 
	journalName = 'Journal of Engineering & Technology Management - JET-M'
WHERE
	journalName = 'Journal of Engineering & Technology Management'

UPDATE 
	resultados 
SET 
	journalName = 'Trziste'
WHERE
	journalName = 'Trziste / Market'


UPDATE scimagojr2020 SET title = 'InterEULawEast: Journal for International & European Law, Economics & Market Integrations' WHERE title = 'InterEULawEast'