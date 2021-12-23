* 1) sql-запросы ближе к концу файла.
* 2) одно из имён полей, customer_i, не соответстует
*    заданному в условии задачи, т.к. FoxPro ограничивает
*    имя поля длиной 10. customer_id имеет длину 11 символов.
* 3) точка с запятой в конце строк - символ продолжения оператора
*    на следующей строке в FoxPro. 
*    sql-код из-за этого получается немного нестандартным.

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


* ============ SQL-раздел =============
* =====================================

SELECT c.name, sum(amount) ;
  FROM customer as c;
  left JOIN order as o;
    ON o.customer_i = c.id; 
  GROUP BY c.name;
  where o.customer_i is not null
* --------- получили вывод сумм заказов по каждому покупателю.

* ------------------
SELECT c.name, sum(amount) ;
  FROM customer as c;
  left JOIN order as o;
    ON o.customer_i = c.id; 
  GROUP BY c.name; 
  where o.customer_i is null
* --------- Нашли покупателей без заказа.
