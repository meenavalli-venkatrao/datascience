FILENAME REFFILE '/folders/myfolders/Retail Analysis_Dataset.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.Retail_Analysis;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.Retail_Analysis; 
RUN;

PROC SQL;
		 CREATE TABLE REATIL_ANALYSIS AS
		 SELECT *, (SALES * QUANTITY ) AS TOTAL_SALES FROM WORK.Retail_Analysis;
QUIT;

PROC MEANS DATA = REATIL_ANALYSIS;
RUN;

PROC REG DATA = REATIL_ANALYSIS;
	 MODEL TOTAL_SALES = PROFIT QUANTITY SHIPPING_COST;
	 VAR TOTAL_SALES;
RUN;

PROC REG DATA = REATIL_ANALYSIS;
	 MODEL TOTAL_SALES = PROFIT QUANTITY;
	 VAR TOTAL_SALES;
RUN;
