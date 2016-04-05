/**
* File Description: the file contains all table definitions for Mojing Project 
*
* Author: Davy Zhu
* E-mail: dreamstudio@vip.163.com
* Date  : Apri. 3rd, 2016
*
*/
select count(*) from
(
select UserInfo_1,UserInfo_2,UserInfo_3,UserInfo_4 from Mojing.ppd_Training_master_dataSet 
	group by UserInfo_1,UserInfo_2,UserInfo_3,UserInfo_4
) T;
