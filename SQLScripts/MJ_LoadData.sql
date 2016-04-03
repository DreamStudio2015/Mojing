/**
* File Description: the file contains all table definitions for Mojing Project 
*
* Author: Davy Zhu
* E-mail: dreamstudio@vip.163.com
* Date  : Apri. 3rd, 2016
*
*/

load data infile '/tmp/PPD_Training_Master_GBK_3_1_Training_Set.csv'
into table Mojing.ppd_Training_master_dataSet fields terminated by ',' lines terminated by '\n';

SELECT * FROM Mojing.ppd_Training_master_dataSet;

show variables like 'char%';
set character_set_database=utf8;
set character_set_results=latin1;