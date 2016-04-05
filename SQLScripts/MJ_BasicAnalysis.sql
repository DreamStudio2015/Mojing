/**
* File Description: the file contains all table definitions for Mojing Project 
*
* Author: Davy Zhu
* E-mail: dreamstudio@vip.163.com
* Date  : Apri. 3rd, 2016
*
*/

### Overall Analysis
## number of records 
# --30000   --580551   --372463
select count(*) from Mojing.ppd_Training_master_dataSet;
select count(*) from Mojing.ppd_loginfo_3_1_training_set;
select count(*) from Mojing.ppd_userupdate_info_3_1_training_set;

## number of columns
# --228  --5  --4
select * from Mojing.ppd_Training_master_dataSet limit 100;
select * from Mojing.ppd_loginfo_3_1_training_set limit 100;
select * from Mojing.ppd_userupdate_info_3_1_training_set limit 100;

## ppd_Training_master_dataSet  Idx 30000
## ppd_loginfo_3_1_training_set  Idx 28987
## ppd_userupdate_info_3_1_training_set Idx 29995
select count(distinct(Idx)) from Mojing.ppd_loginfo_3_1_training_set;
select count(distinct(Idx)) from Mojing.ppd_userupdate_info_3_1_training_set;
select * from Mojing.ppd_loginfo_3_1_training_set where Idx=10001 order by LogInfo3;

## without log Info: 1013
## without userupdate Info: 5
## 1013 + 5 = 1018
select count(*) from
(
select Idx from (select distinct(Idx) Idx from Mojing.ppd_Training_master_dataSet) T 
	where Idx not in 
		(select distinct(A.Idx) Idx from Mojing.ppd_Training_master_dataSet A,
			(select distinct(Idx) Idx from Mojing.ppd_loginfo_3_1_training_set) B
			where A.Idx=B.Idx)
) T1;
select count(*) from
(
select Idx from (select distinct(Idx) Idx from Mojing.ppd_Training_master_dataSet) T 
	where Idx not in 
		(select distinct(A.Idx) Idx from Mojing.ppd_Training_master_dataSet A,
			(select distinct(Idx) Idx from Mojing.ppd_userupdate_info_3_1_training_set) B
			where A.Idx=B.Idx)
) T1;

select count(*) from
(
select Idx from (select distinct(Idx) Idx from Mojing.ppd_Training_master_dataSet) T 
	where Idx not in 
		(select distinct(A.Idx) Idx from Mojing.ppd_Training_master_dataSet A,
			(select distinct(Idx) Idx from Mojing.ppd_loginfo_3_1_training_set) B
			where A.Idx=B.Idx)
union all
select Idx from (select distinct(Idx) Idx from Mojing.ppd_Training_master_dataSet) T 
	where Idx not in 
		(select distinct(A.Idx) Idx from Mojing.ppd_Training_master_dataSet A,
			(select distinct(Idx) Idx from Mojing.ppd_userupdate_info_3_1_training_set) B
			where A.Idx=B.Idx)
) T;