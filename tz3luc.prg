* 3-� ������� �������. �� ��. 

CLOSE ALL
CLEAR ALL
CLEAR

��������� ����� ����������
��������� 4� ���


DO init 
DO newmen1

PROCEDURE newmen1
  SET SYSMENU SAVE
  DEFINE PAD ordpad OF _MSYSMENU  PROMPT '������' ;
    MESSAGE "������������� ������ � ��������" 
  DEFINE PAD servpad OF _MSYSMENU  PROMPT '������������' ;
    MESSAGE "������������ ������ ���� ����� � ��" 
  ON PAD ordpad OF _MSYSMENU  ACTIVATE POPUP ordBars
  ON PAD servpad OF _MSYSMENU  ACTIVATE POPUP servBars
  DEFINE POPUP ordBars MARGIN RELATIVE 

  DEFINE BAR 1 OF ordBars PROMPT '�������� ������ �� �������' 
  DEFINE BAR 2 OF ordBars PROMPT '������� �����'
  DEFINE BAR 3 OF ordBars PROMPT '������� �����'
  DEFINE BAR 4 OF ordBars PROMPT '�������� �����'
  DEFINE BAR 5 OF ordBars PROMPT '����� ������ �� �����' 
  ON SELECTION POPUP ordBars ;
    DO ordMnuChoice WITH bar()
  DEFINE POPUP servBars MARGIN RELATIVE 
  DEFINE BAR 1 OF servBars PROMPT '��������� �������' 
  DEFINE BAR 2 OF servBars PROMPT '������� ��������� ������' 
  DEFINE BAR 3 OF servBars PROMPT '����� �� ����' 
  ON SELECTION POPUP servBars;
    DO servMnuChoice WITH bar()

  IF gMenuInd && ��� ����������� ������������ ���� �� ����,
      && ���� ����� �� ����� ��� ������ �� �. ���� "�����".
      * ��������� �. ���� ����� �� ��������.
    ACTIVATE MENU _MSYSMENU 
  ELSE
    DEACTIVATE MENU _MSYSMENU 
    RELEASE MENU _MSYSMENU  EXTENDED
    SET SYSMENU TO DEFAULT
  ENDIF
ENDPROC


PROCEDURE ordMnuChoice
  PARAMETERS chBar
  DO CASE 
    CASE chBar = 1 && �������� ������ �� �������
      = ordSelection(@gBeginDate, @gEndDate)
    CASE chBar = 2 && ������� �����
      = ordCreation() &&  �������� ����� ���������� ���-� ���������� .t. ��� .f.
    CASE chBar = 3 && ������� �����
      DO ordDeletion
    CASE chBar = 4 && �������� �����
      DO ordModification
    CASE chBar = 5 && ����� ������ �� �����
      DO ordPrnt1
  ENDCASE
ENDPROC


PROCEDURE servMnuChoice
  PARAMETERS chBar
  DO CASE 
    CASE chBar = 1
      DO service
    CASE chBar = 2
      DO fillOrders WITH 30, 16
    CASE chBar = 3
      DO armExit
  ENDCASE
ENDPROC


PROCEDURE ordSelection
  parameters gBeginDate, gEndDate, lOrdId

  CLEAR 

  SET CENTURY ON && �������� ������� ����, ���� �� ��������

  @ 1,40 say "�� ������ �������� ������."
  @ 2,40 say "������� ������ ������."
  @ 3,40 say "������:" get gBeginDate
  @ 4,40 say "�����:" get gEndDate
  @ 5,40 say "Read ��������, ���� ��� ������� ������ ���� -"
  @ 6,40 say "��� ����������� ������� Ctrl-End ��� Ctrl-W"
  read CYCLE
  CLEAR gets

  SET CENTURY OFF 

  *!*	 ��� �� SET RELATION ����� �������� CUSTOMER.name � �� CUSTOMER_I

  SELECT order
  * ���� ��� SQL-������. ���� ������ ������ ������ ����� ����� ���������� FoxPro
  SELECT dateoper, customer_i, amount, id ;
    FROM order;
    WHERE dateoper between gBeginDate and gEndDate; 
    INTO CURSOR periodData READWRITE

  * ������ � periodData ����� ������ �� ��� ������.  
  * ��������, � ���� ���� ������ ���� ���������� �����

  LOCAL titleStr
  titleStr =  "�������� ��������� �������� �����, ����� ������� ^-W - �����."
  BROWSE FONT "Courier New", 16  TITLE titleStr
  lOrdId = id
  USE && ������� ��������� ������ periodData, ���� �� � ��� ���������

  * ��� ���������������� ������ ��������� �����, ������ �� ��
  * �������, � �� order.dbf
  SELECT order
  LOCATE FOR ALLTRIM(id) == ALLTRIM(lOrdId)
  CLEAR 
  DO hatPrnt 
  DO bodyPrnt WITH lOrdId

ENDPROC

PROCEDURE ordCreation
  LOCAL  lOrdSum, ;
    lId, lQuantity, lPrice
  PUBLIC gCurrCust

  CLEAR && �������������� ������� ������

  SELECT customer
  gCurrCust = custChoice()

  lId = qualityInputC()
  IF ALLTRIM(lId) != ALLTRIM(gFalseOrdId)
    = goodsToOrdItems(createConst, lId)  
    DO ordPrnt2 WITH lId
  ELSE
    ? "����� �� ������."
    ? "�������� �� ���� ������� �� ������ ���������� ����������."
  ENDIF
  *!*	 gFalseOrdId ���������� � ������ ����� ����� ����������
  *!*	 �� ���������, ����� ����� ��� = 0 
ENDPROC

FUNCTION custChoice
  LOCAL titleStr
  titleStr =  "�������� ��������� ��������� ����������, ����� ������� ^-W - �����."
  BROWSE FONT "Courier New" NOEDIT FIELDS name TITLE titleStr
  RETURN id  && ����-�� ��� ����� ������������� Esc.
ENDFUNC

PROCEDURE service
  LOCAL lId, cnt22, lItemsAmount, lAmount

  CLOSE ALL

  = op_all_dbf(.t.)
  sele CUSTOMER
  REINDEX
  PACK

  sele ORDER
  REINDEX
  PACK

  sele ORDITEMS
  REINDEX
  PACK

  sele GOODS
  REINDEX
  PACK

  * - - - - ���� "�"
  SELECT orditems && � ������� ������, � ������� ���-�� =0
    * ����-��, ����� ���� �� ������, �� � �������� ����������
    * ����� ����������. � ���������� ���� ������ � ��������� ������
    * � order, � ��������� ���-��� ������� � orditems, �� ������������
  DELETE ALL FOR quantity = 0
  PACK
  * - - - - ����� ����� "�"

  * - -- - ������ ���� ����� ����� "�"
  && �������� � ����-� ������, � ������� ��� ������� ������
  && ����� ����� ���������� ��� ������������ ���������, 
  && ��������� ��� �������
  SELECT order
  SCAN
    lId = id
    SELECT orditems
    COUNT FOR ALLTRIM(order_id) == ALLTRIM(lId) TO cnt22
    SELECT order
    IF cnt22 = 0
      DELETE
    ENDIF
  ENDSCAN
  * - - - -

  SELECT orditems && ��������� ������������ �������������� ����
  SCAN 
    lAmount = quantity * price 
    IF lAmount != amount
      ? "������ ����� � orditems. order_id=", order_id
      REPLACE amount WITH lAmount
    ENDIF
  ENDSCAN && - - - - - 
  
  SELECT order && �������� � ����-� ������, 
    && � ��� ����� = 0
  DELETE FOR amount = 0
  PACK && - - - - -

  SELECT orditems && � ������� ������, � �������
    && ��� ������� ����� - ������ � ������� order
  SCAN
    lId = order_id
    SELECT order
    LOCATE FOR ALLTRIM(id) == ALLTRIM(lId)
    IF !FOUND()
      DELETE IN orditems
    ENDIF
  ENDSCAN
  PACK  && - - - - -

  * ��� ������, ��� ������� ����� ������ 
  * order.amount � �������� ������ �� orditems
  * ��� � ���� ������ ���� ������, � ������ ���������
  * order.amount. ���� ����, ��� 0, �� ����� ����� ������.
  SELECT order
  SCAN 
    lId = id
    lAmount = amount
    SELECT orditems
    SUM amount FOR ALLTRIM(order_id) == ALLTRIM(lId) TO lItemsAmount
    SELECT order
    IF lItemsAmount = 0
      DELETE ALL FOR ALLTRIM(order_id) == ALLTRIM(lId) IN orditems
      DELETE && ������� ����� �����
      do ordPrnt2 with lId
      ? "-------- ����� -------"
    ENDIF
      REPLACE amount WITH lItemsAmount
    IF lItemsAmount != amount
      do ordPrnt2 with lId
      ? "-------- �������� ����� -------"
    ENDIF
  ENDSCAN
  PACK IN order
  PACK IN orditems

ENDPROC && service


PROCEDURE ordModification
  * �������� �����. ������ ���-�� ���������� �������.
  * ���������� �� "�������� �������� ��������� ������" 
  * ��������� �� ����. �.�. ����� �������� ���������� � ������
  * ��������� ������ �������� ������ ���� ������. �������� ����������?
  * ������� ��������. ���������� ���, ������� ����������� ���� ��?
  * ����������. 
  LOCAL titleStr
  LOCAL lOrdSum, ;
    lId, lQuantity, lPrice, lNameGoods, lQuantityGoods
    
  gCurrCust = SPACE(10)

  SELECT order 
  titleStr = '�������� ���������� ����� ��������� � ������� ^-W'
  BROWSE FONT "Courier New", 16 TITLE titleStr LAST
  IF EMPTY(id) .or. readkey() = 12 && ��������, ������ Esc
    ? "����� ������ ��� ��������� �������"
    RETURN && ���� �� �-� ��������� ������
  ENDIF

  gCurrCust = customer_i
  lId = id

  SELECT goods
  REPLACE workQuan WITH 0 ALL

  SELECT orditems 
  SCAN FOR ALLTRIM(order_id) == ALLTRIM(lId)
    * ��� �� ������ �������� ������� �� orditems � goods
    SELECT orditems 
    lNameGoods = ""
    lQuantityGoods = 0
    = itemStorage(@lNameGoods, @lQuantityGoods) && ��������� ���� �����\�����
    SELECT goods
    = ordItemsAddToGoods(lNameGoods, lQuantityGoods) && ������� ���-�� �� ������ 
      && (�� orditems) �� �����, � ������� goods
    = itemNotice(lNameGoods, lQuantityGoods) 
      && �.�. �� ���������� �����, �� �������� 
      * � �� � ����. � workQuan �������� ������ ��������
  ENDSCAN
  
  SELECT orditems 
  DELETE ALL FOR ALLTRIM(order_id) == ALLTRIM(lId)
  PACK && ������� �������� ������ ������

  lInputResult = qualityInputM() && ��������� ���-� ���-� �������
  IF ALLTRIM(lInputResult) == ALLTRIM(gFalseOrdId) 
    && �.�. ��� ������� = 0, ������ ������ ���
    SELECT order 
    DELETE && ������� �����, ��� ��������
    do delDeleted 
  ELSE
    = goodsToOrdItemsM(lId)  
    SELECT order 
    ? "�� �������� �����."
    DO ordPrnt2 WITH lId
  ENDIF
ENDPROC && ordModification


PROCEDURE itemStorage(lNameGoods, pQuantityGoods)
  lNameGoods = namegoods
  pQuantityGoods = quantity
ENDPROC

FUNCTION ordChoice
  LOCAL titleStr, ;
    lDateOper, lOrdSum
  titleStr =  "�������� ��������� �������� �����, ����� ������� ^-W"  

  SELECT order
  BROWSE FONT "Courier New", 16 ;
    FIELDS dateoper, customer_i, amount  ;
    TITLE titleStr 
  RETURN id
ENDFUNC

PROCEDURE ordPrnt1 && ��� �. ���� "����� ������ �� �����"
  LOCAL lOrdId, lOrdSum

  lOrdId = ordChoice()

  IF EMPTY(lOrdId) .or. readkey() = 12 && ������ Esc
    ? "����� ������ ��� ������ ������, �� ������ Esc."
    RETURN 
  ENDIF

  CLEAR 
  DO hatPrnt
  DO bodyPrnt WITH lOrdId
ENDPROC && ordPrnt1

PROCEDURE ordPrnt2  && ��� �. ���� "������� �����" � ��� ���-���
  PARAMETERS lOrdId

  SELECT order
  LOCATE FOR ALLTRIM(id) == ALLTRIM(lOrdId)
  IF !FOUND()  
    ? "error of order search"
    RETURN "error!!"
  ENDIF

  CLEAR 
  DO hatPrnt
  DO bodyPrnt WITH lOrdId
  SELECT order && �� ������ ������
ENDPROC && ordPrnt2

PROCEDURE bodyPrnt
  * ������ ���� (�������) ������
  PARAMETERS lOrdId
  LOCAL condCount

  SELECT orditems 
  condCount = 0
  COUNT FOR ALLTRIM(order_id) == ALLTRIM(lOrdId) TO condCount

  IF condCount > 0
      ? "lOrdId =", lOrdId
    SCAN ALL FOR ALLTRIM(order_id) == ALLTRIM(lOrdId)
      ? namegoods, " ", price, " ", quantity
    ENDSCAN
  ELSE
    ? "�������� ������� � ������ ���"
    ? "����� �������� ��������"
  ENDIF
ENDPROC && bodyPrnt


FUNCTION qualityInputM()
  * ���-�� � ordModification
  LOCAL titleStr, validErrStr

  titleStr =  "� Zakaz ������� �������� ���-�� ������. ^-W - �����."
  validErrStr = "����� ������� ���-�� >= 0 � <= ��������� ���-��"

  SELECT goods

  BROWSE FONT "Courier New", 16 freeze workQuan;
    FIELDS name, price, quantity,;
      workQuan :H="Zakaz",;
      amount = price * workQuan :9;
    VALID :F workQuan >= 0 .and. workQuan <= quantity;
    ERROR validErrStr TITLE titleStr

  SUM workQuan * price ALL FOR workQuan > 0 TO lOrdSum

  SELECT order

  IF lOrdSum = 0 && �.�. ������ �� ��������, ��� ���-�� =0
    RETURN gFalseOrdId
  ENDIF

  && ����� ��� ����������� ������ �� �������� ����.
  && � �� �������� ����� ������ ������������, ��������
  && ��������, ��� ����� ���� ����������, � id ����������

  REPL amount WITH lOrdSum
  REPL customer_i WITH gCurrCust
  RETURN id
ENDFUNC && qualityInputM


FUNCTION qualityInputC()
  * ���-�� � � ordCreation 

  LOCAL titleStr, validErrStr

  titleStr =  "� Zakaz ������� �������� ���-�� ������. ^-W - �����."
  validErrStr = "����� ������� ���-�� >= 0 � <= ��������� ���-��"

  SELECT goods
  REPL workQuan WITH 0 ALL

  BROWSE FONT "Courier New", 16 freeze workQuan;
    FIELDS name, price, quantity,;
      workQuan :H="Zakaz",;
      amount = price * workQuan :9;
    VALID :F workQuan >= 0 .and. workQuan <= quantity;
    ERROR validErrStr TITLE titleStr

  SUM workQuan * price ALL FOR workQuan > 0 TO lOrdSum

  SELECT order

  IF lOrdSum = 0 && �.�. ������ �� ��������, ��� ���-�� =0
    ? "����� ", id, " ������ ���, ��� ��� ���������� = 0."
    ? "�� ��� �������."
    DELETE
    DO delDeleted
    RETURN gFalseOrdId
  ENDIF

  APPEND BLANK
  REPL dateoper WITH Date()
  REPL amount WITH lOrdSum
  REPL customer_i WITH gCurrCust

  REPL id WITH sys(2015)  
    && sys(2015) ���������� ��������� ��� � ������ �������� �����
    * � ����� ������ ��� �������� ��� ��������� ����������� �������������� ������

  RETURN id
ENDFUNC && qualityInputC

PROCEDURE goodsToOrdItemsM(pId)
  * ���-�� � ordModification
  LOCAL lQuantity, lPrice, lGoodsName

  && ��������� ������ ������ �� goods � orditems
  SELECT goods
  SCAN FOR workQuan > 0 
    lQuantity = workQuan
    lPrice = price

    REPL quantity WITH quantity - workQuan
    REPL workQuan WITH 0
      * �� 2 ������, ��� ����, ������ ���������� �������� � �������.
      * ������� ������ ��� ��. ������ ����� ����������, ������ �������������, ������ ������ ��������� � �.�.

    lGoodsName = name && � ���� �� �������� ������ ��������� ��� 
    * �����. ��� ��������. � ���������� ��������� �� ���������� 
    * ����������� ����, ��������� ��� pk-��, ��������� ������� �� ���� � �.�.

    SELECT orditems
    APPEND BLANK
    REPL namegoods WITH lGoodsName
    REPL price WITH lPrice
    REPL quantity WITH lQuantity
    REPL amount WITH price * quantity && ��� ������������ ����� ������ 
    REPL order_id WITH pId

    SELECT goods
  ENDSCAN
ENDPROC && goodsToOrdItemsM 


PROCEDURE itemNotice(pNameGoods, pQuantityGoods)
  SELECT goods &&- - - - ������ ����, �.�. �� ��� ������
  && � ����� ������, �� ��� ������� ����� �����
  LOCATE FOR ALLTRIM(name) == ALLTRIM(pNameGoods)
  IF !FOUND()
    ? "�������, �� �����", name, " �� ������."
    RETURN
  ENDIF && - - - -
  ? "��������� ������ ", name, "� goods.workQuan ", pQuantityGoods, " ����"
  REPL workQuan WITH pQuantityGoods
ENDPROC && itemNotice

FUNCTION custNameFromId
  * �� id ���������� ����� ��� ���
  PARAMETERS pId
  LOCAL lArea, lName
  lArea = SELECT()
  SELECT customer
  LOCATE FOR ALLTRIM(id) == ALLTRIM(pId)
  IF !FOUND()  
    ? "error of customer search" && log info
    SELECT (lArea)
    RETURN "error!! 111"
  ENDIF
  lName = name
  SELECT (lArea)
  RETURN lName
ENDFUNC && custNameFromId

PROCEDURE hatPrnt
  ? "====================="
  ? "����� ", id, " �� ", dateoper, 
  ?? " ���������� =", custNameFromId( customer_i)
  ?? " ����� ������ =", amount
  ? "----------------------"
ENDPROC


PROCEDURE ordItemsAddToGoods(pNameGoods, pQuantityGoods)
  LOCATE FOR ALLTRIM(name) == ALLTRIM(pNameGoods) && �� � goods
  IF !FOUND()
    ? "����� '", pNameGoods, "' �� ������"
    RETURN && ������ �������
  ENDIF
  REPL quantity WITH pQuantityGoods + quantity
ENDPROC && ordItemsAddToGoods


PROCEDURE ordDeletion
  LOCAL titleStr, lId, lNameGoods, lQuantityGoods,;
    lDateOper, lOrdSum
  
  titleStr = '^-T - ��������\�������� ����� ��� ��������. �� ����� ������� "*" ��� ��������'

  SELECT order
  BROWSE FONT "Courier New", 16 ;
    FIELDS del=iif(deleted(), "*"," "), dateoper, customer_i, amount  ;
    TITLE titleStr 

  CLEAR 
  DO delDeleted
ENDPROC


PROCEDURE delDeleted
  && ������� ������, ���������� � �������� � orders
  LOCAL cnt22

  SELECT order

  && - - - - ���������, ��� ���� ��� �������
  COUNT FOR DELETED() TO cnt22 
  IF cnt22 = 0
    ? "������� ������. (��� ���������� � ����-� �������)"
    RETURN 
  ENDIF && - - - -

  SCAN FOR DELETED() && ���� �� order
    lId = id

    ? "�� �������:" &&- - - -  �������������� �����
    DO hatPrnt
    DO bodyPrnt WITH lId &&- - - - 

    SELECT orditems
    SCAN FOR ALLTRIM(order_id) == ALLTRIM(lId) && ���� �� orditems
      * ��� �� ������ �������� ������� �� orditems � goods
      SELECT orditems 
      lNameGoods = ""
      lQuantityGoods = 0
      = itemStorage(@lNameGoods, @lQuantityGoods) && ��������� ���� �����\�����

      SELECT goods
      = ordItemsAddToGoods(@lNameGoods, @lQuantityGoods) && ������� ���-�� �� ������ 
        && �� �����, � ������� goods
    ENDSCAN
    
    SELECT orditems 
    DELETE ALL FOR ALLTRIM(order_id) == ALLTRIM(lId)

    SELECT order
  ENDSCAN

  PACK IN orditems && ������� �������� ������ ������
  PACK IN order
  * �������� ��� �� ����� ������� SQL-���������. 
    *!*	 DELETE FROM orditems WHERE order_id IN 
    *!*	   ( "xxxxxxxxxx", "yyyyyyyyy")
ENDPROC && delDeleted


PROCEDURE init
  * ��� ���������� ���-� �� ����� ������� ������!!!! (CLEAR ALL)
  public gBeginDate, gEndDate, modifyConst, createConst, gFalseOrdId,;
  gCurrCust, withIndex, withoutIndex, gMenuInd

  *!*	 gBeginDate = Date()
  *!*	 gEndDate = Date()
  gBeginDate = CTOD("01.11.21") && ���� ��������, ���� ���� ������ ������� � ������ ��� ������������
  gEndDate = CTOD("31.12.21")
  modifyConst = .f.
  createConst = .t.
  withIndex = .t.
  withoutIndex = .f.
  gFalseOrdId = "1234567890"
  gCurrCust = SPACE(10)
  gMenuInd = .T.

  SET CONFIRM ON
  SET default TO E:\DOCS\zadania\lucenko\
  SET PATH TO E:\DOCS\zadania\lucenko\
  SET talk OFF
  set PROCEDURE to tz3luc

  =op_all_dbf(.t.) && ������� � ���������
ENDPROC && init


PROCEDURE armExit
  gMenuInd = .f. && ����� ���� �� ������ ��������� ��� .f. � .t.
  DEACTIVATE POPUP ordBars
  DEACTIVATE POPUP servBars

  RELEASE POPUPS ordBars, servBars EXTENDED 
  RELEASE PAD ordpad OF _MSYSMENU
  RELEASE PAD servpad OF _MSYSMENU

  * SET SYSMENU TO DEFAULT
  CLOSE ALL
  CLEAR ALL
ENDPROC && armExit


PROCEDURE op_all_dbf(ind)
  * ��������� ��� �������, ��� � �������
  * ���� � ���������, ���� ���.
  * � ������ ������� ������� �� ���������.
  * �� �������� � ����. 

  IF !empty(ind) and ind
    sele 0
    USE CUSTOMER INDEX cust1 
    *INDEX on id TO cust1

    sele 0
    USE ORDER INDEX order1 
    *INDEX on customer_i TO order1

    sele 0
    USE ORDITEMS INDEX ORDITEMS 
    * INDEX on ORDER_ID TO ORDITEMS

    sele 0
    USE GOODS INDEX GOODS
    * INDEX on name TO GOODS
  ELSE
    sele 0
    USE ORDER 

    sele 0
    USE ORDITEMS

    sele 0
    USE GOODS

    sele 0
    USE CUSTOMER
  ENDIF
ENDPROC && op_all_dbf


PROCEDURE fillOrders(qOrd, maxQItems)
  * ���������� ������� ORDER.DBF ���������� ��������
  * qOrd ������� � maxQItems ������������ ���-� �������
  * ������� � ������ ������. �� ����� ���� ���-��, �� ������ maxQItems.
  LOCAL randCust, maxRandCust, k, wAmount, randDate

  SELECT CUSTOMER
  maxRandCust = RECCOUNT()

  FOR k = 1 TO qOrd
    randCust = randNatural(maxRandCust)
    * ��������, ��� CUSTOMER.DBF �� ������
    * ������� ���������� ����������

    SELECT customer
    go top
    skip randCust-1 && ������� � ������� ������� ����������
        && ����� ���� �� �������� ��������� �-����

    SELECT order
    APPEND BLANK
    randDate = Date()-randNatural(20)
    REPL dateoper WITH randDate && ���� ������� ���� ������ ���������
    REPL id WITH orderIdHash(customer.id, randDate)
    REPL customer_i WITH customer.id

    SELECT ORDITEMS
    wAmount = genRandOrderItems(order.id, maxQItems)

    SELECT order
    REPL amount WITH wAmount
  ENDFOR

  SELECT order
  DELETE ALL for amount = 0
  PACK && ��� ������, ��� ����� 0. ����������� ��� � ������� � �� ����� 0
ENDPROC  && fillOrders


FUNCTION orderIdHash(id2, date)
  LOCAL d, k, mod22, h, C, x, s, ls, t, i
  RETURN sys(2015) && ����������� ����. ��� ���, ��� ���� � ����,
  * ��� �� ����������. 

  d = DTOC(date)
  d = substr(d,1,2)+substr(d,4,2)+substr(d,7,2)
  d = lower(alltrim(id2))+d  && lower - ���� ��������� ������� � k
  d = d

  * ---- ��� �����, ��� ����, ���� � ����. �� ��� �����. ----
  * ������ ���� ��������� � � C++ ������

  *!*	 k = 36 �.�. 26 ���� ��� �������� + 10 ����
  k = 36
  mod22 = 1e10+7
  h = 0 
  for i = 1 to len(d)
    c = substr(d,i,1)
    x = asc(c) - asc('a') + 1
    h = (h + k * x) % mod22
  ENDFOR
  * ����� ������������� ----
  s = str(h)
  ls = len(s)
  IF ls < 10 && 10 - ������ ���� order.id
    s = s+replicate('5', 10 - ls)
  ENDIF
  IF ls > 10 
    s = right(s, 10)
  ENDIF
  RETURN ltrim(s)
ENDPROC && orderIdHash


FUNCTION genItem
  * ���������� 1 ����� ������ �� ������
  * ��� genRandOrderItems. 
  parameters goodsName, goodsQuantity, goodsPrice
  LOCAL maxGoodsItem

  maxGoodsItem = RECCOUNT()
  randGoodsItem = randNatural(maxGoodsItem)
  go top
  skip randGoodsItem-1 && ������� � ������� goods
        && ��������� �����

  IF quantity = 0 && �� ����, ��� �������� 
    * ����� ������ ������� ���� �����. 
    * ��� �� ������ � � �����������
    RETURN .f.
  ENDIF

  goodsQuantity = randNatural(round(quantity / 4, 0)) && /4 ���� �� �������� ���� ������� ������
  * repl quantity with  quantity - goodsQuantity 
    && ����� ��������� ����� �� ������
    && �� � ������ ��������� ���� ������� ����� ����� �� ������

  goodsName = name
  goodsPrice = price
  RETURN .t.
ENDFUNC && genItem


PROCEDURE genRandOrderItems(pId, maxQItems)
  * ���������� � ������ (pId) ���� ���-�� 
  * ������� ������
  LOCAL qItems, j, goodsName, goodsQuantity, goodsPrice, goodsGenOk, totalOrdSum
  goodsName = SPACE(10)
  goodsQuantity = 0
  goodsPrice = 0
  goodsGenOk = .f.
  totalOrdSum = 0

  qItems = randNatural(maxQItems) && maxQItems - �������� �������� ����� ������� 
      && � ����� ������. 

  FOR j = 1 to qItems
    SELECT goods
    goodsGenOk = genItem(@goodsName, @goodsQuantity, @goodsPrice)

    SELECT orditems
    IF !goodsGenOk && �� �������������� ��-�� 0-�� ���-��
      LOOP
    ENDIF  

    * ���������, ���� 2� ��� �� ���� �� ������ �� ��� ���������
    IF alredyBe(goodsName, pId) && �����-�� ����� 
        && � �����-�� ������ 
      LOOP
    ENDIF  

    APPEND BLANK
    REPL order_id WITH pId
    REPL namegoods WITH goodsName
    REPL price WITH goodsPrice
    REPL quantity WITH goodsQuantity
    REPL amount WITH goodsQuantity * goodsPrice
    totalOrdSum = totalOrdSum + amount
  ENDFOR
  RETURN totalOrdSum
ENDPROC && genRandOrderItems


FUNCTION alredyBe(gName, ordId)
  * ��� genRandOrderItems. 
  * ���� ���������� � orditems
  LOCAL count11
  count11 = 0
  COUNT FOR ALLTRIM(NAMEGOODS) == ALLTRIM(gName);
    and ALLTRIM(order_id) == ALLTRIM(ordId) TO count11
  IF count11 > 0
    RETURN .t.
  ELSE
    RETURN .F.
  ENDIF
ENDFUNC && alredyBe


PROCEDURE fillGoods 
  PARAMETERS pQuanGoods
  * ��������� ���� ������� ������� 
  * ���������� ���������� ��������
  * � ��������� ����������
  sele 0
  USE GOODS INDEX GOODS 
  *INDEX on name TO GOODS

  LOCAL k, lastNumber

  FOR k = 1 TO pQuanGoods
    APPEND BLANK
    REPL name WITH "�����" + alltrim(STR(k))
    REPL price WITH priceGen(5)
    REPL quantity WITH round(randDiapazon(0, 1000), 0)
  ENDFOR
  CLOSE ALL
  CLEAR ALL
  CLEAR
ENDPROC && fillGoods 


FUNCTION priceGen(maxRazrad)
  * ���������� ���� ���� 
  LOCAL koef, result, randRazrad, i

  randRazrad = randNatural(maxRazrad)
  && �������� ���� ��� ��������

  result = 0
  FOR i = 1 to randRazrad
    koef = 1
    FOR j = 1 to i
      koef = koef * 10
    ENDFOR
    result = result + koef * rand()
  ENDFOR

  RETURN result
ENDFUNC && priceGen


FUNCTION randDiapazon(a, b)
  * ����� �-���. ����������� ���� �������� �� ��������
  * ���������� ������������ �����, �.�. �� �����
  LOCAL l
  l = ABS(a - b)
  RETURN  a + l * rand()
ENDFUNC


FUNCTION randNatural(b)
  * ���������� ����� �� 1 �� b. �� 0!!!
  RETURN round(randDiapazon(1, b), 0)
ENDFUNC

* ====== SQL-������ (�� 2, ��� ���� ��������) 
* =============== ��� ��������������� ===========

*!*	 SELECT c.name, sum(amount) ;
*!*	   FROM customer as c;
*!*	   left JOIN order as o;
*!*	     ON o.customer_i = c.id; 
*!*	   GROUP BY c.name;
*!*	   where o.customer_i is not null
* --------- �������� ����� ���� ������� �� ������� ����������.

* ------------------
*!*	 SELECT c.name, sum(amount) ;
*!*	   FROM customer as c;
*!*	   left JOIN order as o;
*!*	     ON o.customer_i = c.id; 
*!*	   GROUP BY c.name; 
*!*	   where o.customer_i is null
* --------- ����� ����������� ��� ������.

* =====================================
* ���� �����, ��� �� ���-�� � ��� ����������.
* ���� �����-�� ����� � �������� �������� ����,
* ����� ��������� �� �������������. 
* ������� �� ������ ������
* -------------------------------




PROCEDURE goodsToOrdItems(isCreate,pId)
  *!*	 isCreate ��� ������ �� �� �����, ��� � � qualityInput
  * isCreate = .t. - ����� ��� ��� ordCreation
  * isCreate = .f. - ��� ordModification
  * � init ��� ��� ����� ���������

  * �������������� � ordCreation � ordModification
  * �������� �� ����, �� �� ����� ��������

  LOCAL lQuantity, lPrice, lGoodsName

  && ��������� ������ ������ �� goods � orditems
  SELECT goods
  SCAN FOR workQuan > 0 
    lQuantity = workQuan
    lPrice = price

    REPL quantity WITH quantity - workQuan
    REPL workQuan WITH 0
      * �� 2 ������, ��� ����, ������ ���������� �������� � �������.
      * ������� ������ ��� ��. ������ ����� ����������, ������ �������������, ������ ������ ��������� � �.�.

    lGoodsName = name && � ���� �� �������� ������ ��������� ��� 
    * �����. ��� ��������. � ���������� ��������� �� ���������� 
    * ����������� ����, ��������� ��� pk-��, ��������� ������� �� ���� � �.�.

    SELECT orditems
    IF isCreate
      APPEND BLANK
    ENDIF
    REPL namegoods WITH lGoodsName
    REPL price WITH lPrice
    REPL quantity WITH lQuantity
    REPL amount WITH price * quantity && ��� ������������ ����� ������ 
    REPL order_id WITH pId

    SELECT goods
  ENDSCAN
ENDPROC


PROCEDURE onePointPrint
  ? namegoods, " ", price, " ", quantity
ENDPROC


PROCEDURE demoOrdPrnt
  * ������ �������� ����� � ���� ������ �� ����
  * ��� ��� �������� ������������ ������
  = op_all_dbf(.t.)
  SELECT order
  SCAN
    DO hatPrnt
    lId = id
    SELECT orditems
    SET FILTER TO ALLTRIM(order_id) == ALLTRIM(lId) 
    go top
    DO onePointPrint
    SET FILTER TO
    go top
    LOCATE FOR ALLTRIM(order_id) == ALLTRIM(lId) 
    IF !FOUND()
      ?? '----    ----'
    ELSE
      ?? '---- OK ----'
    ENDIF
  ENDSCAN
ENDPROC && demoOrdPrnt


PROCEDURE invertDemoOrdPrnt
  * �� �� ����� ��� � demoOrdPrnt
  * �� ������ ��������, �� orditems

  = op_all_dbf(.t.)
  SELECT orditems
  SCAN
    DO onePointPrint
    lId = order_id

    SELECT order
    LOCATE FOR ALLTRIM(id) == ALLTRIM(lId) 
    IF !FOUND()  
      ?? '----    ----'
      DELETE IN orditems
    ELSE
      ?? '---- OK ----'
    ENDIF
  ENDSCAN
ENDPROC && invertDemoOrdPrnt


PROCEDURE viewRelDB
  * �������� ������� ����� order � orditems
  = op_all_dbf(.t.)
  SELECT namegoods, dateoper, customer_i ;
    FROM orditems as it;
    left JOIN order as o;
      ON it.order_id = o.id
ENDPROC && viewRelDB


PROCEDURE delCalc
  * ��� �������� �������, ���������� � ��������, � ���
  * ����� ���� �� ������������ COUNT, �� � ���� ���� ��������
  *!*	 parameters strDbf
  delCnt = 0
  normCnt = 0
  *!*	 SELECT &strDbf
  scan
    if deleted()
      delCnt = delCnt + 1
    else 
      normCnt = normCnt + 1
    ENDIF
  endscan
  ? 'delCnt =' , delCnt, 'normCnt =', normCnt
ENDPROC


PROCEDURE genRecall
  * ������� ��� ������. �������������� �� ����� �������
  * ����� ��������� ��������� �������.
  CLOSE ALL

  use orditems
  DELETE ALL
  PACK

  use order
  DELETE ALL
  PACK

  CLOSE ALL
ENDPROC && genRecall

