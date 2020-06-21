/******************************************************************/
/* Test SQL QUERY taken from KNIME code from late 2015 */
/* FHS March 2, 2017 */
/* Rig 156 has drvId = 2 */
/* Rig 148 has drvId = 4 */
/******************************************************************/

declare @drvId int = 4             
declare @startDateTime datetime = '2015-10-15 12:00:01' 
declare @endDateTime datetime = '2015-10-15 13:00:00'

/******************************************************************/

select
    tagid,
    tagpath  
into #TAGIDS
from vTags
where tagpath in (
/* Rig 156 Tags */
'ensign_ac_rig/dw/block_height',
'ensign_ac_rig/dw/autodriller_wob_act',
'ensign_ac_rig/hmi_ai/hookload',
'ensign_ac_rig/hmi_ai/standpipe_press',

/* Rig 148 Tags */
'ensign_ac_rig/dw/block_height',
'ensign_ac_rig/dw/rig_hookload',
'ensign_ac_rig/dw/autodriller_wob_act',
'ensign_ac_rig/mp/spp_psi'
)
and drvid=@drvId  

/******************************************************************/

SELECT 
	V.nice_name,
	V.datetime_t_stamp as time_stamp,
	V.tagpath,
    V.intvalue,
    V.floatvalue,
    V.stringvalue,
    V.datevalue
into #RawData
from dbo.vData V
join #TAGIDS T on V.tagid = T.tagId
where dataintegrity=192
and drvId=@drvId
and V.datetime_t_stamp between @startDateTime and @endDateTime
order by V.datetime_t_stamp

/******************************************************************/

select 
	nice_name as rig,
	time_stamp,
	case tagpath
		/* Rig 156 Tags */
		when 'ensign_ac_rig/dw/block_height' then 'block_height'
		when 'ensign_ac_rig/dw/autodriller_wob_act' then 'wob'		
		when 'ensign_ac_rig/hmi_ai/hookload' then 'hookload'
		when 'ensign_ac_rig/hmi_ai/standpipe_press' then 'standpipe_press'

		/* Rig 148 Tags */
		when 'ensign_ac_rig/dw/block_height' then 'block_height'
		when 'ensign_ac_rig/dw/rig_hookload' then 'hookload'
		when 'ensign_ac_rig/dw/autodriller_wob_act' then 'wob'		
		when 'ensign_ac_rig/mp/spp_psi' then 'standpipe_press'

		else 'YYYYYYYYYYYYYYYYYYYYYYYYYYY'
	end as tagpath,
	case tagpath
		when 'ensign_ac_rig/dw/block_height' then floatvalue    /* 148 and 156 */
		when 'ensign_ac_rig/dw/autodriller_wob_act' then floatvalue /* 148 and 156 */		
		when 'ensign_ac_rig/hmi_ai/hookload' then intvalue  /* 156 only */
		when 'ensign_ac_rig/hmi_ai/standpipe_press' then intvalue /* 156 only */

		when 'ensign_ac_rig/dw/rig_hookload' then floatvalue  /* 148 only */
		when 'ensign_ac_rig/mp/spp_psi' then intvalue /* 148 only */
		else 'XXXXXXXXXXXXXXXXXXXXXXX'
	end as tagvalue
 from #RawData order by time_stamp

/******************************************************************/

drop table #RawData
drop table #TAGIDS

