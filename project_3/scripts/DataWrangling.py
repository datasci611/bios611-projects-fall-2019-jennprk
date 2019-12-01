
# coding: utf-8

# In[104]:


# Import packages 
import numpy as np
import pandas as pd
import sys
import math


# # Demographics of Clients at UMD

# In[105]:


client = pd.read_csv("../data/CLIENT_191102.tsv", sep="\t")
client.head()


# In[106]:


# Check the original column names and change to neater version
for col in client.columns: 
    print(col) 
    
client.columns = ['provider','EUID','CUID', 'ClientID','AgeEntry','AgeExit','Gender','Race','Ethnicity','Veteran']


# In[107]:


# Filter the variables of interest
client2 = client[['ClientID','AgeEntry','AgeExit','Gender','Race','Ethnicity','Veteran']]


# In[108]:


# Checking Missingness
## Generate the number of missing values in the demographic variables
nullvals =  ["" for x in range(len(client2.columns))]
nullvals = pd.DataFrame(nullvals).T
nullvals.columns = client2.columns

for col in client2.columns:
    nullval = client2[col].isnull()
    nullvals[col] = sum(nullval)
    
print(nullvals)


# In[109]:


client2 = client2.dropna()


# In[110]:


# function for unique values
def unique(list1): 
      
    # insert the list to the set 
    list_set = set(list1) 
    # convert the set to the list 
    unique_list = (list(list_set)) 
    for x in unique_list: 
        print(x)


# In[111]:


# unique values of categorical variables
cat = ['Gender', 'Race', 'Ethnicity', 'Veteran']

unique(client2[cat[0]])
print()
unique(client2[cat[1]])
print()
unique(client2[cat[2]])
print()
unique(client2[cat[3]])

#for col in cat:
#    ls = list(unique(client2[col]))
#    unqlist.append(ls)


# ### 1. Age Demographics (Entry)

# In[112]:


round(client2.AgeEntry.mean(),1) # average entry age


# In[113]:


client2.AgeEntry.min() # minimum entry age


# In[114]:


client2.AgeEntry.median() # median entry age


# In[115]:


client2.AgeEntry.max() # maximum entry age


# In[116]:


round(client2.AgeEntry.std(),1) # standard deviation of entry age


# ### 2. Age Demographics (Exit)

# In[117]:


round(client2.AgeExit.mean(),1) # average entry age


# In[118]:


client2.AgeExit.min() # minimum exit age


# In[119]:


client2.AgeExit.max() # minimum exit age


# In[120]:


round(client2.AgeExit.std(),1) # standard deviation of exit age


# ### 3. Gender Demographics

# In[121]:


client2['Gender'].value_counts()


# ### 4. Race Demographics

# In[122]:


client2['Race'].value_counts()


# ### 5. Ethnicity Demographics

# In[123]:


client2['Ethnicity'].value_counts()


# ### 6. Veteran Demographics

# In[124]:


client2["Veteran"].value_counts()


# In[125]:


for col in ['Race','Ethnicity',"Veteran"]:
    client2[col] = client2[col].apply(lambda x: x.replace(" (HUD)",""))


# # Income Dataset

# In[126]:


incomeEntry = pd.read_csv("../data/INCOME_ENTRY_191102.tsv", sep='\t')


# In[127]:


# Check the original column names and change to neater version
for col in incomeEntry.columns: 
    print(col) 
    
incomeEntry.columns = ['provider','EUID','CUID', 'ClientID','IncomeEntry','IncomeSourceEntry','MonthlyIncomeEntry','IncomeStartDateEntry','IncomeEndDateEntry','recordsetID','ProviderID','dateadded']


# In[128]:


# Filter the variables of interest
incomeEntry2 = incomeEntry[['ClientID','IncomeEntry','IncomeSourceEntry','MonthlyIncomeEntry','IncomeStartDateEntry', 'IncomeEndDateEntry']]
incomeEntry2.head()


# In[129]:


# Filter out the rows with incomes
temp = pd.DataFrame(columns=incomeEntry2.columns)
clientID = set(incomeEntry2.ClientID)
clientID = [*clientID, ] 

for id in clientID:
    subdat = incomeEntry2[incomeEntry2.ClientID==id]
    subdat = pd.DataFrame(subdat)
    select_indices = list(np.where(subdat["IncomeEntry"] == "Yes")[0])
    if len(select_indices)>0:
        temp = temp.append(subdat.iloc[select_indices])
        
temp = temp.reset_index()
incomeEntry2 = temp


# In[130]:


incomeEntry2_noindex = incomeEntry2[incomeEntry2.columns.difference(['index'])]
incomeEntry2_unq = incomeEntry2_noindex.drop_duplicates()
incomeEntry2 = incomeEntry2_unq


# In[131]:


# Income Exit
incomeExit = pd.read_csv("../data/INCOME_EXIT_191102.tsv", sep='\t')


# In[132]:


# Check the original column names and change to neater version
for col in incomeExit.columns: 
    print(col) 
    
incomeExit.columns = ['provider','EUID','CUID', 'ClientID','IncomeExit','IncomeSourceExit','MonthlyIncomeExit','IncomeStartDateExit','IncomeEndDateExit','recordsetID','ProviderID','dateadded']


# In[133]:


# Filter the variables of interest
incomeExit2 = incomeExit[['ClientID','IncomeExit','IncomeSourceExit','MonthlyIncomeExit','IncomeStartDateExit', 'IncomeEndDateExit']]


# In[134]:


# Filter out the rows with incomes
temp = pd.DataFrame(columns=incomeExit2.columns)
clientID = set(incomeExit2.ClientID)
clientID = [*clientID, ] 

for id in clientID:
    subdat = incomeExit2[incomeExit2.ClientID==id]
    subdat = pd.DataFrame(subdat)
    select_indices = list(np.where(subdat["IncomeExit"] == "Yes")[0])
    if len(select_indices)>0:
        temp = temp.append(subdat.iloc[select_indices])
        
temp = temp.reset_index()
incomeExit2 = temp


# In[135]:


incomeExit2_noindex = incomeExit2[incomeExit2.columns.difference(['Index','index'])]
incomeExit2_unq = incomeExit2_noindex.drop_duplicates()
incomeExit2 = incomeExit2_unq


# # Noncash Dataset

# In[136]:


noncashEntry = pd.read_csv("../data/NONCASH_ENTRY_191102.tsv", sep='\t')


# In[137]:


# Check the original column names and change to neater version
for col in noncashEntry.columns: 
    print(col) 
    
noncashEntry.columns = ['provider','EUID','CUID', 'ClientID','ReceivingBenefitEntry','NoncashSourceEntry','NoncashStartDateEntry','NoncashEndDateEntry','recordsetID','ProviderID','dateadded']


# In[138]:


# Filter the variables of interest
noncashEntry2 = noncashEntry[['ClientID','ReceivingBenefitEntry','NoncashSourceEntry','NoncashStartDateEntry', 'NoncashEndDateEntry']]
noncashEntry2.head()


# In[139]:


# Filter out the rows with incomes
temp = pd.DataFrame(columns=noncashEntry2.columns)
clientID = set(noncashEntry2.ClientID)
clientID = [*clientID, ] 

for id in clientID:
    subdat = noncashEntry2[noncashEntry2.ClientID==id]
    subdat = pd.DataFrame(subdat)
    select_indices = list(np.where(subdat["ReceivingBenefitEntry"] == "Yes")[0])
    if len(select_indices)>0:
        temp = temp.append(subdat.iloc[select_indices])
        
temp = temp.reset_index()
noncashEntry2 = temp


# In[143]:


noncashEntry2_noindex = noncashEntry2[noncashEntry2.columns.difference(['index'])]
noncashEntry2_unq = noncashEntry2_noindex.drop_duplicates()
noncashEntry2 = noncashEntry2_unq


# In[146]:


# Non-cash Exit
noncashExit = pd.read_csv("../data/NONCASH_EXIT_191102.tsv", sep='\t')


# In[147]:


# Check the original column names and change to neater version
for col in noncashExit.columns: 
    print(col) 
    
noncashExit.columns = ['provider','EUID','CUID', 'ClientID','ReceivingBenefitExit','NoncashSourceExit','NoncashStartDateExit','NoncashEndDateExit','recordsetID','ProviderID','dateadded']


# In[95]:


# Filter the variables of interest
noncashExit2 = noncashExit[['ClientID','ReceivingBenefitExit','NoncashSourceExit','NoncashStartDateExit','NoncashEndDateExit']]


# In[96]:


# Filter out the rows with incomes
temp = pd.DataFrame(columns=noncashExit2.columns)
clientID = set(noncashExit2.ClientID)
clientID = [*clientID, ] 

for id in clientID:
    subdat = noncashExit2[noncashExit2.ClientID==id]
    subdat = pd.DataFrame(subdat)
    select_indices = list(np.where(subdat["ReceivingBenefitExit"] == "Yes")[0])
    if len(select_indices)>0:
        temp = temp.append(subdat.iloc[select_indices])
        
temp = temp.reset_index()
noncashExit2 = temp


# In[103]:


noncashExit2_noindex = noncashExit2[noncashExit2.columns.difference(['index'])]
noncashExit2_unq = noncashExit2_noindex.drop_duplicates()
noncashExit2 = noncashExit2_unq


# # Entry and Exit dates

# In[383]:


EEdate = pd.read_csv("../data/ENTRY_EXIT_191102.tsv", sep='\t')


# In[386]:


# Check the original column names and change to neater version
for col in EEdate.columns: 
    print(col) 
    
EEdate.columns = ['provider','EUID','CUID', 'ClientID','EEgroupID', 'EEhouseholdID', 'unnamed6','EntryDate', 'moveindate','ExitDate','Destination', 'ReasonforLeaving', 'EEtype',"EEadded",'EEupdatedDate']


# In[387]:


EEdate2 = EEdate[['ClientID','EntryDate','moveindate','ExitDate','ReasonforLeaving']]


# # Merge Datasets

# In[372]:


final_dat = client2.merge(incomeEntry2, on=['ClientID'], how='left')


# In[373]:


final_dat = final_dat.merge(incomeExit2, on=['ClientID'], how='left')


# In[374]:


final_dat = final_dat.merge(noncashEntry2, on=['ClientID'], how='left')


# In[375]:


final_dat = final_dat.merge(noncashExit2, on=['ClientID'], how='left')


# In[389]:


final_dat = final_dat.merge(EEdate2, on=['ClientID'], how='left')


# In[390]:


final_dat.to_csv("../data/final.csv") # Save final data for visualization and statistical analysis in R


# In[381]:


# Save individual files
# client2.to_csv("../data/client2.csv")
# incomeEntry2.to_csv("../data/incomeEntry2.csv")
# incomeExit2.to_csv("../data/incomeExit2.csv")
# HIEntry2.to_csv("../data/HIEntry2.csv")
# HIExit2.to_csv("../data/HIExit2.csv")
# DisEntry2.to_csv("../data/DisEntry2.csv")
# DisExit2.to_csv("../data/DisExit2.csv")

