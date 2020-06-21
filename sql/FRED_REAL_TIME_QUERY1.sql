set nocount on
if object_id('tempdb..#results') is not null drop table #results
declare @5minutesAgo bigint
declare @ms_in_day bigint = 60 * 60 * 24 * 1000 /* constant - milliseconds in a day */
select @5minutesAgo=round((datediff(day,'1970-01-01',getutcdate())*@ms_in_day - datediff(millisecond,getutcdate(),convert(date,getutcdate())))-420000,-4)  /* go 5 minutes back */
declare @5minutes as table(tenseconds_epoch bigint)
declare @i int = 0
while (@i<=41)
begin
   insert into @5minutes values (@5minutesAgo+(@i*10000))
   set @i = @i + 1
end

select FiveMinutes.tenseconds_epoch,historian_rpm.tagvalue as historian_rpm,historian_blockheight.tagvalue as historian_blockheight,historian_hookload.tagvalue as historian_hookload,historian_standpipe_pressure.tagvalue as historian_standpipe_pressure,historian_mp1_spm.tagvalue as historian_mp1_spm
      ,historian_mp2_spm.tagvalue as historian_mp2_spm,historian_hole_depth.tagvalue as historian_hole_depth,historian_bit_depth.tagvalue as historian_bit_depth,historian_torque.tagvalue as historian_torque,historian_wob.tagvalue as historian_wob,historian_rop.tagvalue as historian_rop
  into #results
  from @5minutes FiveMinutes
       left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=96727) historian_rpm on FiveMinutes.tenseconds_epoch=historian_rpm.nearest10seconds_epoch
       left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=98147) historian_blockheight on FiveMinutes.tenseconds_epoch=historian_blockheight.nearest10seconds_epoch
	   left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=97342) historian_hookload on FiveMinutes.tenseconds_epoch=historian_hookload.nearest10seconds_epoch
	   left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=98643) historian_standpipe_pressure on FiveMinutes.tenseconds_epoch=historian_standpipe_pressure.nearest10seconds_epoch
	   left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=98756) historian_mp1_spm on FiveMinutes.tenseconds_epoch=historian_mp1_spm.nearest10seconds_epoch
       left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=98705) historian_mp2_spm on FiveMinutes.tenseconds_epoch=historian_mp2_spm.nearest10seconds_epoch
	   left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=96471) historian_hole_depth on FiveMinutes.tenseconds_epoch=historian_hole_depth.nearest10seconds_epoch
	   left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=96932) historian_bit_depth on FiveMinutes.tenseconds_epoch=historian_bit_depth.nearest10seconds_epoch
	   left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=96395) historian_torque on FiveMinutes.tenseconds_epoch=historian_torque.nearest10seconds_epoch
	   left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=97818) historian_wob on FiveMinutes.tenseconds_epoch=historian_wob.nearest10seconds_epoch
	   left outer join (select round(t_stamp,-4) as nearest10seconds_epoch,isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where t_stamp>=@5minutesAgo and tagid=97638) historian_rop on FiveMinutes.tenseconds_epoch=historian_rop.nearest10seconds_epoch

update #results set historian_rpm=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=96727 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=96727 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_rpm is null
update #results set historian_blockheight=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=98147 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=98147 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_blockheight is null
update #results set historian_hookload=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=97342 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=97342 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_hookload is null
update #results set historian_standpipe_pressure=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=98643 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=98643 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_standpipe_pressure is null
update #results set historian_mp1_spm=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=98756 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=98756 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_mp1_spm is null
update #results set historian_mp2_spm=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=98705 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=98705 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_mp2_spm is null
update #results set historian_hole_depth=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=96471 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=96471 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_hole_depth is null
update #results set historian_bit_depth=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=96932 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=96932 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_bit_depth is null
update #results set historian_torque=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=96395 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=96395 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_torque is null
update #results set historian_wob=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=97818 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=97818 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_wob is null
update #results set historian_rop=(select isnull(floatvalue,intvalue) as tagvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=97638 and t_stamp=(select max(t_stamp) from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=97638 and t_stamp<=@5minutesAgo)) where tenseconds_epoch=@5minutesAgo and historian_rop is null


declare @bigint_counter bigint = @5minutesAgo
declare @previous_rpm float,@previous_blockheight float,@previous_hookload float,@previous_standpipe_pressure float,@previous_mp1_spm float,@previous_mp2_spm float,@previous_hole_depth float,@previous_bit_depth float,@previous_torque float,@previous_wob float,@previous_rop float
while (@bigint_counter<=@5minutesAgo + 420000)
begin
   update #results set historian_rpm=@previous_rpm where tenseconds_epoch=@bigint_counter and historian_rpm is null
   update #results set historian_blockheight=@previous_blockheight where tenseconds_epoch=@bigint_counter and historian_blockheight is null
   update #results set historian_hookload=@previous_hookload where tenseconds_epoch=@bigint_counter and historian_hookload is null
   update #results set historian_standpipe_pressure=@previous_standpipe_pressure where tenseconds_epoch=@bigint_counter and historian_standpipe_pressure is null
   update #results set historian_mp1_spm=@previous_mp1_spm where tenseconds_epoch=@bigint_counter and historian_mp1_spm is null
   update #results set historian_mp2_spm=@previous_mp2_spm where tenseconds_epoch=@bigint_counter and historian_mp2_spm is null
   update #results set historian_hole_depth=@previous_hole_depth where tenseconds_epoch=@bigint_counter and historian_hole_depth is null
   update #results set historian_bit_depth=@previous_bit_depth where tenseconds_epoch=@bigint_counter and historian_bit_depth is null
   update #results set historian_torque=@previous_torque where tenseconds_epoch=@bigint_counter and historian_torque is null
   update #results set historian_wob=@previous_wob where tenseconds_epoch=@bigint_counter and historian_wob is null
   update #results set historian_rop=@previous_rop where tenseconds_epoch=@bigint_counter and historian_rop is null
   select @previous_bit_depth=historian_bit_depth
         ,@previous_blockheight=historian_blockheight
		 ,@previous_hole_depth=historian_hole_depth
		 ,@previous_hookload=historian_hookload
		 ,@previous_mp1_spm=historian_mp1_spm
		 ,@previous_mp2_spm=historian_mp2_spm
		 ,@previous_rpm=historian_rpm
		 ,@previous_standpipe_pressure=historian_standpipe_pressure
		 ,@previous_torque=historian_torque
		 ,@previous_wob=historian_wob
		 ,@previous_rop=historian_rop
    from #results where tenseconds_epoch=@bigint_counter

   set @bigint_counter=@bigint_counter+10000
end

select * from #results order by 1

