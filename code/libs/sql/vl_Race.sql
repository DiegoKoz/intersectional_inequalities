



-- combination of both race and gender
-- using given names and family names


drop table #DFP
select a.cluster_ID, min(ID_Art) as MIN
into #DFP
from BDVincent.[dbo].[cwts_clusters] as a 
left join article as ar on a.UT = ar.ItemID
group by a.Cluster_ID

drop table #SFP
select distinct d.cluster_ID, Ediscipline, ESpecialite
into #SFP
from #DFP as d
inner join article as a on a.id_Art = d.min
inner join Liste_Revue as LR ON a.code_revue = lr.code_revue
inner join liste_Discipline as ld on lr.Code_Discipline = ld.Code_Discipline

-- ordre auteur (first)
drop table #ordre
select au.id_art, au.ordre, MIN(aa.ordre_tr) as ordre_TR
into #ordre
from auteur as au
INNER JOIN Adresse_Auteur as aa on au.ID_Art = aa.ID_Art and
									au.Ordre = aa.ordre_auteur
group by au.id_art, au.ordre

-- country of first publication
drop table #CFP
select a.cluster_ID, min(ID_Art) as MIN
into #CFP
from BDVincent.[dbo].[cwts_clusters] as a 
left join article as ar on a.UT = ar.ItemID
group by a.Cluster_ID

drop table #ifp
select distinct a.cluster_ID, A.Ordre, b.min
into #ifp
from BDVincent.[dbo].[cwts_clusters] as a 
left join article as ar on a.UT = ar.ItemID
inner join #CFP as b on a.Cluster_ID = b.Cluster_ID and ar.ID_Art = b.MIN

drop table #fco
select distinct d.cluster_ID, LP.ERegroupement
into #fco
from #IFP as d
LEFT JOIN #ordre as aa on D.Min = aa.ID_Art and
									D.ordre = aa.ordre
LEFT JOIN Adresse AS AD ON aa.ID_Art = AD.ID_Art and
									 aa.Ordre_TR = AD.Ordre_tr		
LEFT JOIN outil.dbo.Liste_Pays as LP ON AD.pays = LP.pays_ISI

drop table #yfp
select a.cluster_ID, min(Annee_Bibliographique) as YFP
into #yfp
from BDVincent.[dbo].[cwts_clusters] as a 
left join article as ar on a.UT = ar.ItemID
group by a.Cluster_ID

drop table #pro
select Cluster_ID, count(distinct ut) as NPap
into #pro
from BDVincent.[dbo].[cwts_clusters] 
group by cluster_ID


select top 1000 * from Revue_Log

-- select top 100 * from Bdvincent.dbo.race2020 where nom = 'murray-DS' order by annee_bibliographique

-- take all cluster ids that have been associaed with a country
-- period: 2008-2017
drop table Bdvincent.dbo.race2020
Select distinct A.cluster_ID, ar.Annee_Bibliographique, y.yfp, ar.id_art, au.Prenom, au.nom,
aa.ordre, ar.nb_auteur, DL.EDiscipline, DL.ESpecialite, cr.cit_rel_all_IAC, a.ordre as ordre_auteur, ad.Province,
cc.Ediscipline as disc_origin, cc.ESpecialite as spec_origin, fc.ERegroupement as count_origin, ge.gender
/*, 
,g.white as White_Surname, g.black as Black_Surname, g.api as API_Surname, g.asian as Asian_Surname, g.hispanic as Hispanic_Surname,  -- family names
f.white as White_givenname, F.black as Black_givenname, F.native as native_givenname, F.asian as Asian_givenname, F.hispanic as Hispanic_givenname Given names
*/
into Bdvincent.dbo.race2020
from BDVincent.[dbo].[cwts_clusters] as a 
inner join article as ar on a.UT = ar.ItemID
inner join auteur as au on ar.id_Art = au.id_Art and 
									a.Ordre = au.Ordre
INNER JOIN #ordre as aa on au.ID_Art = aa.ID_Art and
									au.ordre = aa.ordre
INNER JOIN Adresse AS AD ON aa.ID_Art = AD.ID_Art and
									aa.Ordre_TR = AD.Ordre_tr		
INNER JOIN Liste_Revue as LR ON ar.Code_Revue = lr.code_Revue
INNER JOIN Liste_DIscipline as DL ON lr.Code_Discipline = dl.Code_Discipline
left join bdvincent.dbo.first as ge on au.nom = ge.nom and
									au.prenom = ge.given
inner join #yfp as y on a.cluster_ID = y.cluster_ID					-- year of first publication
left join #fco as fc on a.Cluster_ID = fc.Cluster_ID					-- first country
inner join #SFP as cc on a.Cluster_ID = cc.Cluster_ID				-- first specialty
left join citations_relatives as cr on ar.ID_Art = cr.id_Art
--left join BDVincent.dbo.race_percent as g on au.Nom like g.name_wildcard
--left join BDVincent.[dbo].[Race_firstnames] as F on au.Prenom like f.[Firstname_wildcard]
where ad.pays = 'USA' and ge.eregroupement = 'United States' and ar.Annee_Bibliographique between 2008 and 2019

drop table #pro
select Cluster_ID, count(distinct id_Art) as NPap
into #pro
from Bdvincent.dbo.race2020
group by cluster_ID

drop table bdvincent.dbo.race_extra
select distinct id_Art, ITEMID, titre
into bdvincent.dbo.race_extra
from pub_expanded.dbo.article
where id_Art in (Select distinct id_Art from  Bdvincent.dbo.race2020)

select * into  bdvincent.dbo.race_abstracts
from Pub_Expanded.dbo.abstract 
where id_Art in (Select distinct id_Art from  Bdvincent.dbo.race2020)

select * into  bdvincent.dbo.race_kw
from Pub_Expanded.dbo.Descripteur_ISI 
where id_Art in (Select distinct id_Art from  Bdvincent.dbo.race2020)



drop table bdvincent.dbo.race_double_final
Select distinct r.cluster_ID, Annee_Bibliographique, yfp, id_art, Prenom, ordre, nb_auteur, EDiscipline, ESpecialite, cit_rel_all_IAC, Province,
disc_origin, spec_origin, count_origin, gender, p.npap,
round (white_Surname,1) white_Surname, round (black_Surname,1) black_Surname, round (asian_Surname,1) asian_Surname, round (hispanic_Surname,1) hispanic_Surname,
round (white_givenname,1) white_givenname, round (black_givenname,1) black_givenname, round (asian_givenname,1) asian_givenname, round (hispanic_givenname,1) hispanic_givenname
into bdvincent.dbo.race_double_final
from Bdvincent.dbo.race2020 as r 
inner join #pro as p on r.cluster_ID = p.cluster_ID




select Ediscipline, Especialite, gender, white_Surname, white_givenname, 'White', 'White',AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, white_Surname, white_givenname
UNION ALL
select Ediscipline, Especialite, gender, black_Surname, white_givenname, 'Black', 'White', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, black_Surname, white_givenname
UNION ALL
select Ediscipline, Especialite, gender, asian_Surname, white_givenname, 'Asian', 'White', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
Group by Ediscipline, Especialite, gender, asian_Surname, white_givenname
UNION ALL
select Ediscipline, Especialite, gender, hispanic_Surname, white_givenname, 'Hispanic', 'White', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, hispanic_Surname, white_givenname
UNION ALL
select Ediscipline, Especialite, gender, white_Surname, black_givenname, 'White', 'Black', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, white_Surname, black_givenname
UNION ALL
select Ediscipline, Especialite, gender, black_Surname, black_givenname, 'Black', 'Black', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, black_Surname, black_givenname
UNION ALL
select Ediscipline, Especialite, gender, asian_Surname, black_givenname, 'Asian', 'Black', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
Group by Ediscipline, Especialite, gender, asian_Surname, black_givenname
UNION ALL
select Ediscipline, Especialite, gender, hispanic_Surname, black_givenname, 'Hispanic', 'Black', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, hispanic_Surname, black_givenname
UNION ALL
select Ediscipline, Especialite, gender, white_Surname, asian_givenname, 'White', 'Asian', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, white_Surname, asian_givenname
UNION ALL
select Ediscipline, Especialite, gender, black_Surname, asian_givenname, 'Black', 'Asian', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, black_Surname, asian_givenname
UNION ALL
select Ediscipline, Especialite, gender, asian_Surname, asian_givenname, 'Asian', 'Asian', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
Group by Ediscipline, Especialite, gender, asian_Surname, asian_givenname
UNION ALL
select Ediscipline, Especialite, gender, hispanic_Surname, asian_givenname, 'Hispanic', 'Asian', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, hispanic_Surname, asian_givenname
UNION ALL
select Ediscipline, Especialite, gender, white_Surname, hispanic_givenname, 'White', 'Hispanic', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, white_Surname, hispanic_givenname
UNION ALL
select Ediscipline, Especialite, gender, black_Surname, hispanic_givenname, 'Black', 'Hispanic', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
group by Ediscipline, Especialite, gender, black_Surname, hispanic_givenname
UNION ALL
select Ediscipline, Especialite, gender, asian_Surname, hispanic_givenname, 'Asian', 'Hispanic', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur
Group by Ediscipline, Especialite, gender, asian_Surname, hispanic_givenname
UNION ALL
select Ediscipline, Especialite, gender, hispanic_Surname, hispanic_givenname, 'Hispanic', 'Hispanic', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final where  ordre = 1 or ordre = Nb_Auteur 
group by Ediscipline, Especialite, gender, hispanic_Surname, hispanic_givenname


select Ediscipline, Especialite, gender, white_Surname, white_givenname, 'White', 'White',AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, white_Surname, white_givenname
UNION ALL
select Ediscipline, Especialite, gender, black_Surname, white_givenname, 'Black', 'White', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, black_Surname, white_givenname
UNION ALL
select Ediscipline, Especialite, gender, asian_Surname, white_givenname, 'Asian', 'White', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
Group by Ediscipline, Especialite, gender, asian_Surname, white_givenname
UNION ALL
select Ediscipline, Especialite, gender, hispanic_Surname, white_givenname, 'Hispanic', 'White', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, hispanic_Surname, white_givenname
UNION ALL
select Ediscipline, Especialite, gender, white_Surname, black_givenname, 'White', 'Black', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, white_Surname, black_givenname
UNION ALL
select Ediscipline, Especialite, gender, black_Surname, black_givenname, 'Black', 'Black', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, black_Surname, black_givenname
UNION ALL
select Ediscipline, Especialite, gender, asian_Surname, black_givenname, 'Asian', 'Black', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
Group by Ediscipline, Especialite, gender, asian_Surname, black_givenname
UNION ALL
select Ediscipline, Especialite, gender, hispanic_Surname, black_givenname, 'Hispanic', 'Black', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, hispanic_Surname, black_givenname
UNION ALL
select Ediscipline, Especialite, gender, white_Surname, asian_givenname, 'White', 'Asian', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, white_Surname, asian_givenname
UNION ALL
select Ediscipline, Especialite, gender, black_Surname, asian_givenname, 'Black', 'Asian', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, black_Surname, asian_givenname
UNION ALL
select Ediscipline, Especialite, gender, asian_Surname, asian_givenname, 'Asian', 'Asian', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
Group by Ediscipline, Especialite, gender, asian_Surname, asian_givenname
UNION ALL
select Ediscipline, Especialite, gender, hispanic_Surname, asian_givenname, 'Hispanic', 'Asian', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, hispanic_Surname, asian_givenname
UNION ALL
select Ediscipline, Especialite, gender, white_Surname, hispanic_givenname, 'White', 'Hispanic', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, white_Surname, hispanic_givenname
UNION ALL
select Ediscipline, Especialite, gender, black_Surname, hispanic_givenname, 'Black', 'Hispanic', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
group by Ediscipline, Especialite, gender, black_Surname, hispanic_givenname
UNION ALL
select Ediscipline, Especialite, gender, asian_Surname, hispanic_givenname, 'Asian', 'Hispanic', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final 
Group by Ediscipline, Especialite, gender, asian_Surname, hispanic_givenname
UNION ALL
select Ediscipline, Especialite, gender, hispanic_Surname, hispanic_givenname, 'Hispanic', 'Hispanic', AVG(convert(dec,Npap)), count(distinct id_Art)Npap
from bdvincent.dbo.race_double_final  
group by Ediscipline, Especialite, gender, hispanic_Surname, hispanic_givenname




select top 100 * from Citations_Relatives
-- to solve the coverage issue 
-- assign to each authorship a percentage of their race
select top 100 * from BDVincent.dbo.race_percent
select top 100 * from BDVincent.dbo.race_firstnames



-- ok: compilation of race data 
-- at the level of corresponding authors
-- and at the level of all US authors
-- i.e. those who have a link with a US institution

select top 100 * from Adresse_Reimpression where id_Art = 42920187
select top 100 * from auteur where id_Art = 42920187

-- 1. corresponding authors
drop table #race_percent
select distinct A.ID_Art, ad.Province, LD.especialite, LD.EDiscipline, g.white, g.black, g.api,
 g.asian, g.tworace, g.hispanic, g.dominant_race, aut.ordre, aut.prenom, aut.nom,
CR.Cit_Rel_ALL_iac, cr.CIT_ALL_IAC, '1' as N, revue, a.annee_bibliographique, a.Nb_Auteur
into #race_percent
from Adresse_Reimpression as au
left join BDVincent.dbo.race_percent as g on au.auteur like g.name_wildcard
INNER JOIN article as a on au.id_art = a.id_art
INNER JOIN Adresse_Reimpression AS AD ON a.ID_Art = AD.ID_Art								
INNER JOIN OUTIL.DBO.Liste_Pays AS LP ON Ad.Pays = LP.Pays_ISI
INNER JOIN Citations_relatives AS CR ON A.ID_Art = CR.ID_Art
INNER JOIN Liste_Revue as LR ON A.Code_Revue = Lr.Code_Revue 
INNER JOIN Liste_Discipline AS LD ON LR.Code_Discipline = LD.Code_Discipline
inner join auteur as aut on au.id_Art = aut.id_Art and au.auteur = aut.Nom
WHERE A.Annee_Bibliographique BETWEEN 1973 and 2018 and ad.pays = 'USA' 
 
/*sample 
select distinct AU.*, g.white, g.black, g.api,
 g.asian, g.tworace, g.hispanic, g.dominant_race
from Adresse_Reimpression as au
left join BDVincent.dbo.race_percent as g on au.auteur like g.name_wildcard
INNER JOIN article as a on au.id_art = a.id_art
WHERE annee_bibliographique in (1980,2018) and au.pays = 'USA' 
 
 */

 select annee_bibliographique, count(distinct id_Art)
 from #race_percent 
group by Annee_Bibliographique

-- Créer des classes 
-- first authors
-- select top 100 * from bdvincent.dbo.race_corr_final
drop table bdvincent.dbo.race_corr_final
select r.*, g.gender, round (white,1) Rwhite, round (black,1) Rblack, round (asian,1) Rasian, round (hispanic,1) Rhispanic
into bdvincent.dbo.race_corr_final 
from #race_percent as r
left join bdvincent.dbo.gender_2018 as g on r.Prenom = g.given

select count(distinct id_Art) from bdvincent.dbo.race_corr_final 
select count(distinct id_Art) from #race_percent 

-- for impact
select Ediscipline, Especialite, gender, rwhite, 'White', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_corr_final
where Annee_Bibliographique between 2006 and 2018
group by Ediscipline, Especialite, gender, rwhite
UNION ALL
select Ediscipline, Especialite, gender, Rblack, 'Black', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_corr_final
where Annee_Bibliographique between 2006 and 2018
group by Ediscipline, Especialite, gender, Rblack
UNION ALL
select Ediscipline, Especialite, gender, Rasian, 'Asian', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_corr_final
where Annee_Bibliographique between 2006 and 2018
Group by Ediscipline, Especialite, gender, Rasian
UNION ALL
select Ediscipline, Especialite, gender, Rhispanic, 'Hispanic', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_corr_final
where Annee_Bibliographique between 2006 and 2018
group by Ediscipline, Especialite, gender, Rhispanic

-- over time
Select Ediscipline, Especialite, 'ZZ_All_US', annee_bibliographique, Sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic
from bdvincent.dbo.race_corr_final as rf
group by Ediscipline, Especialite, annee_bibliographique
union all
Select Ediscipline, Especialite, province, annee_bibliographique, Sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic
from bdvincent.dbo.race_corr_final as rf
group by Ediscipline, Especialite, province, annee_bibliographique


select annee_bibliographique, gender, ediscipline, Sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic
from bdvincent.dbo.race_corr_final
group by annee_bibliographique, gender, ediscipline

-- descripteur auteurs
select distinct d.Descripteur
into #topk
from bdvincent.dbo.race_corr_final as c
inner join Descripteur_Auteur as d on c.id_Art= d.id_Art
where annee_bibliographique between 2006 and 2018
group by d.Descripteur
having count(distinct c.id_Art) > 49

select d.Descripteur, gender, sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic, count(distinct c.id_art)
from bdvincent.dbo.race_corr_final as c
inner join Descripteur_Auteur as d on c.id_Art= d.id_Art
inner join #topk as t on d.Descripteur = t.descripteur
where annee_bibliographique between 2006 and 2018
group by d.Descripteur, gender

-- descripteur ISI
-- select distinct ediscipline from bdvincent.dbo.race_corr_final
select distinct d.Keyword
into #topkw
from bdvincent.dbo.race_corr_final as c
inner join Descripteur_ISI as d on c.id_Art= d.id_Art
where annee_bibliographique between 2006 and 2018
group by d.Keyword
having count(distinct c.id_Art) > 49

select d.Keyword, 'SSH', gender, sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic, count(distinct c.id_art)
from bdvincent.dbo.race_corr_final as c
inner join Descripteur_ISI as d on c.id_Art= d.id_Art
inner join #topkw as t on d.Keyword = t.Keyword
where annee_bibliographique between 2006 and 2018 and 
Ediscipline IN ('Arts','Humanities','Professional Fields','Psychology','Social Sciences')
group by d.Keyword, gender
UNION ALL
select d.Keyword, 'MED', gender, sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic, count(distinct c.id_art)
from bdvincent.dbo.race_corr_final as c
inner join Descripteur_ISI as d on c.id_Art= d.id_Art
inner join #topkw as t on d.Keyword = t.Keyword
where annee_bibliographique between 2006 and 2018 and 
Ediscipline IN ('Biomedical Research', 'Health', 'Clinical Medicine')
group by d.Keyword, gender
UNION ALL
select d.Keyword, 'NSE', gender, sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic, count(distinct c.id_art)
from bdvincent.dbo.race_corr_final as c
inner join Descripteur_ISI as d on c.id_Art= d.id_Art
inner join #topkw as t on d.Keyword = t.Keyword
where annee_bibliographique between 2006 and 2018 and 
Ediscipline IN ('Biology', 'Chemistry', 'Earth and Space', 'Engineering and Technology',  'Mathematics', 'Physics')
group by d.Keyword, gender






-- for all authors
-- but only US based
drop table #race_percent
select distinct A.ID_Art, ad.Province, LD.especialite, LD.EDiscipline, g.white, g.black, g.api,
 g.asian, g.tworace, g.hispanic, g.dominant_race, au.ordre, au.prenom,
CR.Cit_Rel_ALL_iac, cr.CIT_ALL_IAC, '1' as N, revue, a.annee_bibliographique, a.Nb_Auteur
into #race_percent
from auteur as au
left join BDVincent.dbo.race_percent as g on au.Nom like g.name_wildcard
INNER JOIN article as a on au.id_art = a.id_art
INNER JOIN Adresse_Reimpression AS AD ON a.ID_Art = AD.ID_Art								
INNER JOIN OUTIL.DBO.Liste_Pays AS LP ON Ad.Pays = LP.Pays_ISI
INNER JOIN Citations_relatives AS CR ON A.ID_Art = CR.ID_Art
INNER JOIN Liste_Revue as LR ON A.Code_Revue = Lr.Code_Revue 
INNER JOIN Liste_Discipline AS LD ON LR.Code_Discipline = LD.Code_Discipline
WHERE A.Annee_Bibliographique BETWEEN 1973 and 2018 and ad.pays = 'USA' 

select id_Art, min(ordre) as ordre
into #min
from #race_percent
group by id_Art



select top 100 * from bdvincent.dbo.race_corr_final


select Ediscipline, Especialite, gender, rwhite, 'White', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_corr_final
where ordre = 1
group by Ediscipline, Especialite, gender, rwhite
UNION ALL
select Ediscipline, Especialite, gender, Rblack, 'Black', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_corr_final
where ordre = 1
group by Ediscipline, Especialite, gender, Rblack
UNION ALL
select Ediscipline, Especialite, gender, Rasian, 'Asian', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_corr_final
where ordre = 1
Group by Ediscipline, Especialite, gender, Rasian
UNION ALL
select Ediscipline, Especialite, gender, Rhispanic, 'Hispanic', AVG(Cit_REL_ALL_IAC), count(distinct id_Art)Npap
from bdvincent.dbo.race_corr_final
where ordre = 1
group by Ediscipline, Especialite, gender, Rhispanic

select top 100 * from bdvincent.dbo.race_Final where rwhite is null






-- percentage of first authorships that have this name
select count(distinct id_Art),  from #race_percent where ordre = 1
select count(distinct id_Art) from #race_percent where ordre = 1 and white is not null


Select /*Ediscipline, Especialite,*/ ar.institution, /*annee_bibliographique,*/ Sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic, 'all author'
from bdvincent.dbo.race_Final as rf
inner join adresse_reimpression as ar on rf.id_Art = ar.id_Art 
group by /*Ediscipline, Especialite,*/ ar.institution/*, annee_bibliographique*/
UNION ALL
Select /*Ediscipline, Especialite,*/ ar.institution, /*annee_bibliographique,*/ Sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic, 'first author'
from bdvincent.dbo.race_Final as rf
inner join adresse_reimpression as ar on rf.id_Art = ar.id_Art
WHERE rf.ordre = 1
group by /*Ediscipline, Especialite,*/ ar.institution/*, annee_bibliographique*/
UNION ALL
Select /*Ediscipline, Especialite,*/ ar.institution, /*annee_bibliographique,*/ Sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic, 'last author'
from bdvincent.dbo.race_Final as rf
inner join adresse_reimpression as ar on rf.id_Art = ar.id_Art
WHERE rf.ordre = Nb_Auteur
group by /*Ediscipline, Especialite,*/ ar.institution/*, annee_bibliographique*/
UNION ALL
Select /*Ediscipline, Especialite,*/ ar.institution, /*annee_bibliographique,*/ Sum(white) white, sum(black) black, sum(Asian) Asian, sum(hispanic) Hispanic, 'middle author'
from bdvincent.dbo.race_Final as rf
inner join adresse_reimpression as ar on rf.id_Art = ar.id_Art 
WHERE rf.ordre != Nb_Auteur and  rf.ordre != 1
group by /*Ediscipline, Especialite, */ar.institution/*, annee_bibliographique*/

select a.dominant_race as Race_First, a.gender as Gender_First, b.dominant_race as Race_Last, b.gender as Gender_Last, avg(a.CIT_REL_ALL_IAC), count(distinct a.id_Art)
from bdvincent.dbo.race_Final as a
inner join bdvincent.dbo.race_Final as b on a.id_Art = b.ID_Art
where a.ordre = 1 and (b.ordre = b.NB_auteur) 
group by a.dominant_race, a.gender, b.dominant_race, b.gender

select top 100 * from bdvincent.dbo.race_Final 



-- third data
-- all Leiden DIsambiguated researchers with a US Address

select top 100 * 
into #clusters_2
from SaCWTS.dbo.cwts_clusters as ac
inner join article as 
inner join adresse as ad on ar.id_Art = ac
left join bdvincent.dbo.gender_2018












-- first data

drop table #race
select distinct A.ID_Art, ad.pays, LD.especialite, LD.EDiscipline, g.race, au.ordre, 
CR.Cit_Rel_ALL_iac, cr.CIT_ALL_IAC, '1' as N, revue, a.annee_bibliographique, a.Nb_Auteur
into #race
from auteur as au
left join BDVincent.dbo.race_surname as g on au.Nom like g.nom_famille_wild
INNER JOIN article as a on au.id_art = a.id_art
INNER JOIN Adresse_Reimpression AS AD ON a.ID_Art = AD.ID_Art								
INNER JOIN OUTIL.DBO.Liste_Pays AS LP ON Ad.Pays = LP.Pays_ISI
INNER JOIN Citations_relatives AS CR ON A.ID_Art = CR.ID_Art
INNER JOIN Liste_Revue as LR ON A.Code_Revue = Lr.Code_Revue 
INNER JOIN Liste_Discipline AS LD ON LR.Code_Discipline = LD.Code_Discipline
WHERE A.Annee_Bibliographique BETWEEN 1973 and 2018 and ad.pays = 'USA'


select r.race, r.annee_bibliographique, r.ESpecialite, R.EDiscipline, count(distinct id_Art), avg(cit_rel_all_IAC)
from #race as r
where ordre = 1
group by r.race, r.annee_bibliographique,r.ESpecialite, R.EDiscipline


Select distinct r.*, au.prenom
into #ba
from #race as r
inner join auteur as au on r.id_Art = au.id_Art and 
							r.ordre = au.Ordre

select distinct r.*, g.gender
into #race_gen
from #ba as r
left join bdvincent.dbo.gender_2018 as g on r.Prenom = g.given

select r.race, R.gender, r.annee_bibliographique, r.ESpecialite, R.EDiscipline, count(distinct id_Art), avg(cit_rel_all_IAC)
from #race_gen as r
where ordre = 1
group by r.race, R.gender, r.annee_bibliographique,r.ESpecialite, R.EDiscipline
