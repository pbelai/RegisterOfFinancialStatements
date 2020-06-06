
CREATE TABLE financial_statement (
	id_financial_statement VARCHAR (10) PRIMARY KEY,  --! EJ
	obdobieOd DATE,
	obdobieDo DATE,
	datumPodania DATE,
	datumZostavenia DATE,
	datumSchvalenia DATE,
	datumZostaveniaK DATE,
	datumPrilozeniaSpravyAuditora DATE,
	nazovUJ VARCHAR (500),
	ico CHAR (8),
	dic CHAR (10),
	nazovFondu VARCHAR (500),
	leiKod VARCHAR (20),
	idUJ VARCHAR (10),
	konsolidovana BOOLEAN,
	konsolidovanaZavierkaUstrednejStatnejSpravy BOOLEAN,
	suhrnnaUctovnaZavierkaVerejnejSpravy BOOLEAN,
	typ VARCHAR (15),
	zdrojDat VARCHAR (30),
	datumPoslednejUpravy DATE 
);


CREATE TABLE accounting_entity (
	id_accounting_entity VARCHAR (10) PRIMARY KEY,  --! EJ
	ico CHAR (8),
	dic CHAR (10),
	sid VARCHAR (5),
	nazovUJ VARCHAR (500),
	mesto VARCHAR (200),
	ulica VARCHAR (200),
	psc VARCHAR (10),
	datumZalozenia DATE,
	datumZrusenia DATE,
	pravnaForma VARCHAR (100),
	skNace VARCHAR (100),
	velkostOrganizacie VARCHAR (100),
	druhVlastnictva VARCHAR (100),
	kraj VARCHAR (100),
	okres VARCHAR (100),
	sidlo VARCHAR (100),
	konsolidovana bool,
	idUctovnychZavierok VARCHAR (10),
	idVyrocnychSprav VARCHAR (10),
	zdrojDat VARCHAR (30),
	datumPoslednejUpravy DATE 
);

CREATE TABLE financial_statement_for_accounting_entity (
	id serial PRIMARY KEY,
	id_accounting_entity VARCHAR (10),
	id_financial_statement VARCHAR (10)
);


CREATE TABLE financial_report_for_financial_statement (
	id serial PRIMARY KEY,
	id_financial_statement VARCHAR (10),
	id_financial_report VARCHAR (10)
);

CREATE TABLE financial_report_title (
	id serial PRIMARY KEY,
    id_financial_report VARCHAR (10),
    nazovUctovnejJednotky VARCHAR (500),
    ico VARCHAR (8),
    dic VARCHAR (10),             
    adresa VARCHAR (500),
    skNace VARCHAR (100),
    typZavierky VARCHAR (30),
    obdobieOd DATE,
    obdobieDo DATE,
    predchadzajuceObdobieOd DATE,
    predchadzajuceObdobieDo DATE,
    datumSchvalenia DATE,
    datumZostavenia DATE,
    oznacenieObchodnehoRegistra VARCHAR (100),
    type VARCHAR (3)
);

CREATE TABLE financial_report_base (
	id serial PRIMARY KEY,
    id_financial_report VARCHAR (10),
    pristupnostDat VARCHAR (20),
	datumPoslednejUpravy DATE,
	zdrojDat VARCHAR(20),
	kodDanovehoUradu VARCHAR(20),
	idSablony VARCHAR(20)
);

DO language 'plpgsql'
$$
DECLARE mujAkt text := 'CREATE TABLE financial_report_assets_muj(id serial PRIMARY KEY, id_financial_report varchar(10),'
	|| string_agg('V' || i::text || ' integer', ',') || ');'
	FROM generate_series(1,46) As i;
DECLARE mujPas text := 'CREATE TABLE financial_report_lae_muj(id serial PRIMARY KEY, id_financial_report varchar(10),'
	|| string_agg('V' || i::text || ' integer', ',') || ');'
	FROM generate_series(1,44) As i;
DECLARE mujZS text := 'CREATE TABLE financial_report_is_muj(id serial PRIMARY KEY, id_financial_report varchar(10),'
	|| string_agg('V' || i::text || ' integer', ',') || ');'
	FROM generate_series(1,76) As i;

DECLARE podAkt text := 'CREATE TABLE financial_report_assets_pod(id serial PRIMARY KEY, id_financial_report varchar(10),'
	|| string_agg('V' || i::text || ' integer', ',') || ');'
	FROM generate_series(1,312) As i;
DECLARE podPas text := 'CREATE TABLE financial_report_lae_pod(id serial PRIMARY KEY, id_financial_report varchar(10),'
	|| string_agg('V' || i::text || ' integer', ',') || ');'
	FROM generate_series(1,134) As i;
DECLARE podZS text := 'CREATE TABLE financial_report_is_pod(id serial PRIMARY KEY, id_financial_report varchar(10),'
	|| string_agg('V' || i::text || ' integer', ',') || ');'
	FROM generate_series(1,122) As i;


BEGIN
    EXECUTE mujAkt;
	EXECUTE mujPas;
	EXECUTE mujZS;
	
	EXECUTE podAkt;
	EXECUTE podPas;
	EXECUTE podZS;
END;
$$ ;
