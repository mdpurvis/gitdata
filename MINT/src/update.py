#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import os
from datetime import datetime as dt
import re




file_loc = '../data/raw/Transactional Data/'
file_loc1 = '../data/raw/Budget Template.xlsx'


def combine_csv_files(file_loc):
    
    listofnames = [f for f in os.listdir(file_loc) if f.endswith('.csv')]
    
    print('Opening: ')
    for ea in listofnames:
        
        print(ea)
        try:
            df = df.append(pd.read_csv(file_loc + ea))
        except:
            df = pd.read_csv(file_loc + ea)
    
    return df


# In[2]:


fin = combine_csv_files(file_loc)
fin.columns = fin.columns.str.replace(r"[ ]","_")
fin['Date'] = pd.to_datetime(fin['Date'])
fin['Month'] = fin['Date']




fin = fin.groupby(['Date', 'Month', 'Category', 'Description', 
                   'Account_Name','Transaction_Type'], 
                  as_index=False).agg({'Amount':'sum', 
                                        'Original_Description':"count"})

fin.rename({'Original_Description':'Count'}, axis=1, inplace=True)
fin.rename({'Category':'Sub_Category'}, axis=1, inplace=True)





fin = fin[(fin['Date'] > '2017-06-30')]




fin['Sub_Category'].replace('Credit Card Payment', 'Transfer', inplace=True)
fin = fin[~fin['Sub_Category'].str.contains('Wedding Reimbursement', na=False)]
fin = fin[~fin['Sub_Category'].str.contains('Wedding expenses', na=False)]
fin1 = fin




fin1 = fin1[~fin1['Sub_Category'].str.contains('Transfer', na=False)]


# In[3]:


exp = pd.read_excel(file_loc1, sheet_name='Exp Ref')
exp.rename({'Sub-Category':'Sub_Category'}, axis=1, inplace=True)
fin1 = fin1.set_index('Sub_Category')
exp.set_index('Sub_Category', inplace=True)
exp.columns = exp.columns.str.replace(r"[ ]","_")




fin1 = fin1.join(exp, how="left").fillna("Research")
fin1.reset_index(inplace=True)


# In[4]:


fin2 = fin1.loc[fin1['Expense_Type_Factor']=='Research']
print(fin2[['Date', 'Sub_Category', 'Description', 'Category', 'Amount']].head(25))


input("ERROR UPDATE: IF LINE ITEM APPEARS, GO BACK TO MINT.COM AND MAKE CORRECTIONS \n"
      "Otherwise, Press Enter to Continue:")


# In[5]:


fin1.loc[fin1['Budget']=='Est', 'Forecast_Amount'] = fin1['Amount']
fin1.loc[fin1['Budget']!='Est', 'Forecast_Amount'] = 0

fin1['Forecast_Amount'] = pd.to_numeric(fin1['Forecast_Amount'].astype(str).replace('[^0-9]]', "").copy())

fin1['Expense_Type_Factor'] = pd.to_numeric(fin1['Expense_Type_Factor'].astype(str).replace('[^0-9]]', "").copy())


# In[6]:


fin1['Budget_Amount'] = fin1['Expense_Type_Factor']*fin1['Forecast_Amount']


fin1.loc[fin1['Transaction_Type']=='credit', 'Amount'] = fin1['Amount']
fin1.loc[fin1['Transaction_Type']=='debit', 'Amount'] = fin1['Amount']*-1


# In[7]:


fin1.rename({'Sub_Category':'Sub-Category'}, axis=1, inplace=True)

fin1.set_index('Date', inplace=True)
fin1.columns = fin1.columns.str.replace(r"[_]"," ")




fin1 = fin1[['Month', 'Sub-Category', 'Description', 'Account Name',
       'Transaction Type', 'Count', 'Category Number',
       'Sub-Category Number', 'Category', 'Budget', 'Expense Type',
       'Expense Type Factor', 'Forecast Amount', 'Budget Amount', 'Amount']]


# In[8]:


trfr = fin[(fin.Sub_Category == "Transfer")]

trfr['Amount'] = trfr['Amount'].abs()


# In[9]:


trfr.loc[trfr['Transaction_Type']!='debit', 'Transfer_Type'] = str('In')
trfr.loc[trfr['Transfer_Type']!='In', 'Transfer_Type'] = str('Out')


# In[10]:


trfr.set_index('Date', inplace=True)
trfr.columns = trfr.columns.str.replace(r"[_]"," ")


# In[11]:


trfr.rename({'Sub Category':'Sub-Category'}, axis=1, inplace=True)


df = trfr[['Month', 'Sub-Category', 'Description', 'Account Name',
       'Transaction Type', 'Count', 'Amount', 'Transfer Type']]


# In[12]:


amt = fin1
amt.reset_index(inplace=True)
amt = amt.groupby(['Date'], 
                  as_index=False).agg({'Amount':'sum'})
# amt.Date.value_counts()
amt.head(3)


# In[13]:


amt['Cash'] = amt.Amount.shift(1)
beg_balance = 9255.58
amt.Cash.fillna(beg_balance, inplace=True)
amt.head(3)


# In[14]:


L = amt['Cash'].tolist()

def add_one_by_one(L):
    new_L = []
    for elt in L:
        if len(new_L)>0:
            new_L.append(new_L[-1]+elt)
        else:
            new_L.append(elt)
    return new_L

new_L = add_one_by_one(L)

amt['Cash'] = new_L
amt.head(3)


# In[15]:


amt.set_index('Date',inplace=True)
fin1.set_index('Date', inplace=True)
fin = fin1.join(amt['Cash']).fillna('Research')


# In[16]:


fin_writer = pd.ExcelWriter('../data/final/financial_transactions.xlsx', engine = 'xlsxwriter',
                            datetime_format = 'mm/dd/yyyy')


fin.to_excel(fin_writer, 'Data')
workbook = fin_writer.book
worksheet = fin_writer.sheets['Data']
curFormat = workbook.add_format({'num_format': '$#,##0.00_);[Red]($#,##0.00)'})
worksheet.set_column('D:D', None, curFormat)
fin_writer.save()


# In[17]:


trfr_writer = pd.ExcelWriter('../data/final/transfer_report.xlsx', engine = 'xlsxwriter',
                            datetime_format = 'mm/dd/yyyy')

df.to_excel(trfr_writer, 'Data')
workbook = trfr_writer.book
worksheet = trfr_writer.sheets['Data']
curFormat = workbook.add_format({'num_format': '$#,##0.00_);[Red]($#,##0.00)'})
worksheet.set_column('D:D', None, curFormat)
trfr_writer.save()


# In[18]:


input("Press Enter to Close:")


# In[ ]:




