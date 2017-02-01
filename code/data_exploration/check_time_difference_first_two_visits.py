#!/usr/bin/python3

import pandas

subjects_dates={}

#read only the first three columns(Subject_id, enummer, datum)
VIP_data=pandas.read_csv('VIP_161102.csv', low_memory=False,usecols=[0,1,2])

#get the list of subjects with at least two visits
subjects_2plus_visits=pandas.read_csv('Subject_ids_2plus_visits', low_memory=False, header=None)
subjects_2plus_visits=list(subjects_2plus_visits[0])

#for each subject, extract the dates in a list and put in the dictionary of subjects.
for subject_id in range(0,len(VIP_data['Subject_id'])):
    if VIP_data['Subject_id'][subject_id] in subjects_2plus_visits:
        visit_date_year=int(VIP_data['datum'][subject_id][6:11])
        if VIP_data['Subject_id'][subject_id] in subjects_dates:
            subjects_dates[VIP_data['Subject_id'][subject_id]].append(visit_date_year)
        else:
            subjects_dates[VIP_data['Subject_id'][subject_id]]=[visit_date_year]
    

with open('temporary_file_checking_visits_time','wt') as visits_file:
    for key, value in subjects_dates.items():
        print(value)
        value=sorted([int(x) for x in value])
        if len(value)==2:
            visits_file.write("Subject id: "+str(key)+"\n")
            visits_file.write("\t first visit: "+str(value[0])+"\n")
            visits_file.write("\t second visit: "+str(value[1])+"\n")
            visits_file.write("\t difference in years, between the first two visits: "+str(value[1]-value[0])+"\n")
        elif len(value)==3:  
            visits_file.write("Subject id: "+str(key)+"\n")
            visits_file.write("\t first visit: "+str(value[0])+"\n")
            visits_file.write("\t second visit: "+str(value[1])+"\n")
            visits_file.write("\t third visit: "+str(value[2])+"\n")            
            visits_file.write("\t difference in years, between the first two visits: "+str(value[1]-value[0])+"\n")
            visits_file.write("\t difference in years, between the second two visits: "+str(value[2]-value[1])+"\n")
            visits_file.write("\t difference in years, between the first and third visits: "+str(value[2]-value[0])+"\n")
        else:
            visits_file.write("Subject id: "+str(key)+"\n")
            visits_file.write("\t first visit: "+str(value[0])+"\n")
            visits_file.write("\t second visit: "+str(value[1])+"\n")
            visits_file.write("\t third visit: "+str(value[2])+"\n")
            visits_file.write("\t fourth visit: "+str(value[3])+"\n")          
            visits_file.write("\t difference in years, between the first two visits: "+str(value[1]-value[0])+"\n")
            visits_file.write("\t difference in years, between the second two visits: "+str(value[2]-value[1])+"\n")
            visits_file.write("\t difference in years, between the first and third visits: "+str(value[2]-value[0])+"\n")
            visits_file.write("\t difference in years, between the third two visits: "+str(value[3]-value[2])+"\n")
            visits_file.write("\t difference in years, between the second and fourth visits: "+str(value[3]-value[1])+"\n")
            visits_file.write("\t difference in years, between the first and fourth visits: "+str(value[3]-value[0])+"\n")
    
    