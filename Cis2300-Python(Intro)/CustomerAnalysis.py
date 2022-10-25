#!/usr/bin/env python
# coding: utf-8

# In[ ]:


#Main
Customer_List = {}
print("Hi, Welcome to Customer Analyst")
print()
print("Please Enter as 9 if it starts 9am, Please Enter 16 if it closes at 4pm")
print()
StartTime = input("Please Enter Your Business Opening time: " )

while StartTime.isdigit() == False or int(StartTime) >24:
  print("Please Enter Interger")
  StartTime = input("Please Enter Your Business Opening time: " )
print()

CloseTime = input("Please Enter Your Business Closing time: " )
while CloseTime.isdigit() == False or int(CloseTime) >24:
  print("Please Enter Interger")
  CloseTime = input("Please Enter Your Business Closing time: " )
print()

for i in range(int(CloseTime) - int(StartTime)):
  Customer_Num = input("Please Enter the Customer Number between {} and {}: ".format(int(StartTime), int(StartTime)+1))
  while Customer_Num.isdigit() == False:
    print("Please Enter Interger")
    Customer_Num = input("Please Enter Your Business Closing time: " )
  x = int(StartTime)
  y = int(StartTime) +1
  Customer_List[x] = int(Customer_Num)
  StartTime = int(StartTime) + 1
  print()

#MAX, MIN, AVG
MaxValue = max(Customer_List.values())
MaxKey = max(Customer_List, key = Customer_List.get)
print()
print("The Highest hour is between",MaxKey,'to',MaxKey + 1, "There are",MaxValue)
print()

MinValue = min(Customer_List.values())
MinKey = min(Customer_List, key = Customer_List.get)
print()
print("The lowest hour is between",MinKey,'to',MinKey + 1, "There are",MinValue)
print()

AVG = sum(Customer_List.values())/(i+1)
print('The Average Number of Customers in a day is ', AVG)

print('Thanks You for Using!')

#Graph
import matplotlib.pyplot as plt 
keys = Customer_List.keys()
values = Customer_List.values()

bars = plt.bar(keys, values, color='b', width = 1.0, align = 'edge',ec = 'black')
plt.ylabel('Numbers of Customer')
plt.title('Customer Analyst')
plt.xlabel('Time Period')
for bar in bars:
  yval = bar.get_height()
  plt.text(bar.get_x(), yval+0.05, yval)
plt.show()


# In[ ]:




