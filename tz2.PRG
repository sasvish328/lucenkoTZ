* 1) sql-������� ����� � ����� �����.
* 2) ���� �� ��� �����, customer_i, �� ������������
*    ��������� � ������� ������, �.�. FoxPro ������������
*    ��� ���� ������ 10. customer_id ����� ����� 11 ��������.
* 3) ����� � ������� � ����� ����� - ������ ����������� ���������
*    �� ��������� ������ � FoxPro. 
*    sql-��� ��-�� ����� ���������� ������� �������������.

CLOSE ALL
CLEAR
CLEAR ALL

SET default TO E:\DOCS\zadania\lucenko\
SET PATH TO E:\DOCS\zadania\lucenko\
SET talk OFF

USE CUSTOMER INDEX cust1 in 1
*INDEX on id TO cust1

USE ORDER INDEX order1 in 2
*INDEX on customer_i TO order1

SELECT Customer

*!*	 SET RELATION TO id INTO Order
*!*	 BROWSE FONT "Courier New", 16;
*!*	  FIELDS order.customer_i, customer.name, order.amount


* ============ SQL-������ =============
* =====================================

SELECT c.name, sum(amount) ;
  FROM customer as c;
  left JOIN order as o;
    ON o.customer_i = c.id; 
  GROUP BY c.name;
  where o.customer_i is not null
* --------- �������� ����� ���� ������� �� ������� ����������.

* ------------------
SELECT c.name, sum(amount) ;
  FROM customer as c;
  left JOIN order as o;
    ON o.customer_i = c.id; 
  GROUP BY c.name; 
  where o.customer_i is null
* --------- ����� ����������� ��� ������.
