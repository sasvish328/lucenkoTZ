* 3-е задание Луценко. По БД. 

CLOSE ALL
CLEAR ALL
CLEAR

лимаренко ольга михайловна
павлоград 4е отд


DO init 
DO newmen1

PROCEDURE newmen1
  SET SYSMENU SAVE
  DEFINE PAD ordpad OF _MSYSMENU  PROMPT 'Заказы' ;
    MESSAGE "Разнообразная работа с заказами" 
  DEFINE PAD servpad OF _MSYSMENU  PROMPT 'Обслуживание' ;
    MESSAGE "Сравнительно редкие виды работ с БД" 
  ON PAD ordpad OF _MSYSMENU  ACTIVATE POPUP ordBars
  ON PAD servpad OF _MSYSMENU  ACTIVATE POPUP servBars
  DEFINE POPUP ordBars MARGIN RELATIVE 

  DEFINE BAR 1 OF ordBars PROMPT 'отобрать заказы по периоду' 
  DEFINE BAR 2 OF ordBars PROMPT 'создать заказ'
  DEFINE BAR 3 OF ordBars PROMPT 'удалить заказ'
  DEFINE BAR 4 OF ordBars PROMPT 'изменить заказ'
  DEFINE BAR 5 OF ordBars PROMPT 'Вывод заказа на экран' 
  ON SELECTION POPUP ordBars ;
    DO ordMnuChoice WITH bar()
  DEFINE POPUP servBars MARGIN RELATIVE 
  DEFINE BAR 1 OF servBars PROMPT 'Обслужить таблицы' 
  DEFINE BAR 2 OF servBars PROMPT 'Создать фиктивные заказы' 
  DEFINE BAR 3 OF servBars PROMPT 'Выход из АРМа' 
  ON SELECTION POPUP servBars;
    DO servMnuChoice WITH bar()

  IF gMenuInd && эта конструкция обеспечивает цикл по меню,
      && чтоб выход из проги шёл только по п. меню "Выход".
      * Остальные п. меню прогу не обрывают.
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
    CASE chBar = 1 && отобрать заказы по периоду
      = ordSelection(@gBeginDate, @gEndDate)
    CASE chBar = 2 && создать заказ
      = ordCreation() &&  теоретич можно возвращать рез-т успешности .t. или .f.
    CASE chBar = 3 && удалить заказ
      DO ordDeletion
    CASE chBar = 4 && изменить заказ
      DO ordModification
    CASE chBar = 5 && Вывод заказа на экран
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

  SET CENTURY ON && неудобно вводить дату, если не включено

  @ 1,40 say "Вы хотите отобрать заказы."
  @ 2,40 say "Введите нужный период."
  @ 3,40 say "Начало:" get gBeginDate
  @ 4,40 say "Конец:" get gEndDate
  @ 5,40 say "Read зациклен, если нет желания менять даты -"
  @ 6,40 say "для продолжения нажмите Ctrl-End или Ctrl-W"
  read CYCLE
  CLEAR gets

  SET CENTURY OFF 

  *!*	 тут дб SET RELATION чтобы показать CUSTOMER.name а не CUSTOMER_I

  SELECT order
  * ниже идёт SQL-запрос. Хотя решить данную задачу можно чисто средствами FoxPro
  SELECT dateoper, customer_i, amount, id ;
    FROM order;
    WHERE dateoper between gBeginDate and gEndDate; 
    INTO CURSOR periodData READWRITE

  * дальше с periodData можно делать всё что угодно.  
  * Например, я чуть ниже выберу один конкретный заказ

  LOCAL titleStr
  titleStr =  "Выберите стрелками желаемый заказ, затем нажмите ^-W - выход."
  BROWSE FONT "Courier New", 16  TITLE titleStr
  lOrdId = id
  USE && закрыли временный курсор periodData, хотя он и сам закроется

  * для иллюстративности вывожу выбранный заказ, только не из
  * курсора, а из order.dbf
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

  CLEAR && необязательная очистка экрана

  SELECT customer
  gCurrCust = custChoice()

  lId = qualityInputC()
  IF ALLTRIM(lId) != ALLTRIM(gFalseOrdId)
    = goodsToOrdItems(createConst, lId)  
    DO ordPrnt2 WITH lId
  ELSE
    ? "Заказ не создан."
    ? "Возможно не было введено ни одного ненулевого количества."
  ENDIF
  *!*	 gFalseOrdId посылается в случае когда заказ фактически
  *!*	 не состоялся, когда сумма его = 0 
ENDPROC

FUNCTION custChoice
  LOCAL titleStr
  titleStr =  "Выберите стрелками желаемого покупателя, затем нажмите ^-W - выход."
  BROWSE FONT "Courier New" NOEDIT FIELDS name TITLE titleStr
  RETURN id  && теор-ки тут можно анализировать Esc.
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

  * - - - - блок "Х"
  SELECT orditems && б удалять пункты, у которых кол-во =0
    * теор-ти, таких быть не должно, но в процессе разработки
    * могли возникнуть. В результате есть заказы с ненулевой суммой
    * в order, с ненулевым кол-вом записей в orditems, но неправильные
  DELETE ALL FOR quantity = 0
  PACK
  * - - - - конец блока "Х"

  * - -- - должно идти после блока "Х"
  && помечаем д удал-я заказы, у которых нет пунктов заказа
  && такие могли возникнуть при тестировании программы, 
  && аварийные так сказать
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

  SELECT orditems && проверяем правильность сформированных сумм
  SCAN 
    lAmount = quantity * price 
    IF lAmount != amount
      ? "Замена суммы в orditems. order_id=", order_id
      REPLACE amount WITH lAmount
    ENDIF
  ENDSCAN && - - - - - 
  
  SELECT order && помечаем д удал-я заказы, 
    && у кот сумма = 0
  DELETE FOR amount = 0
  PACK && - - - - -

  SELECT orditems && б удалять пункты, у которых
    && нет главной части - строки в таблице order
  SCAN
    lId = order_id
    SELECT order
    LOCATE FOR ALLTRIM(id) == ALLTRIM(lId)
    IF !FOUND()
      DELETE IN orditems
    ENDIF
  ENDSCAN
  PACK  && - - - - -

  * это модуль, что сличает сумму заказа 
  * order.amount с реальной суммой по orditems
  * что с этим делать пока неясно, я просто исправляю
  * order.amount. Если вижу, что 0, то такой заказ удаляю.
  SELECT order
  SCAN 
    lId = id
    lAmount = amount
    SELECT orditems
    SUM amount FOR ALLTRIM(order_id) == ALLTRIM(lId) TO lItemsAmount
    SELECT order
    IF lItemsAmount = 0
      DELETE ALL FOR ALLTRIM(order_id) == ALLTRIM(lId) IN orditems
      DELETE && удаляем такой заказ
      do ordPrnt2 with lId
      ? "-------- удалён -------"
    ENDIF
      REPLACE amount WITH lItemsAmount
    IF lItemsAmount != amount
      do ordPrnt2 with lId
      ? "-------- изменена сумма -------"
    ENDIF
  ENDSCAN
  PACK IN order
  PACK IN orditems

ENDPROC && service


PROCEDURE ordModification
  * изменяет заказ. Только кол-во заказанных позиций.
  * Требование ТЗ "изменить основные параметры заказа" 
  * выполнять не стал. Т.к. таким основным параметром в рамках
  * выбранной модели является только дата заказа. Поменять покупателя?
  * Странно выглядит. Уникальный код, который суррогатный ключ БД?
  * Аналогично. 
  LOCAL titleStr
  LOCAL lOrdSum, ;
    lId, lQuantity, lPrice, lNameGoods, lQuantityGoods
    
  gCurrCust = SPACE(10)

  SELECT order 
  titleStr = 'Выберите изменяемый заказ стрелками и нажмите ^-W'
  BROWSE FONT "Courier New", 16 TITLE titleStr LAST
  IF EMPTY(id) .or. readkey() = 12 && например, нажали Esc
    ? "выбор заказа для изменения оборван"
    RETURN && надо бы к-л обработку делать
  ENDIF

  gCurrCust = customer_i
  lId = id

  SELECT goods
  REPLACE workQuan WITH 0 ALL

  SELECT orditems 
  SCAN FOR ALLTRIM(order_id) == ALLTRIM(lId)
    * тут мы делаем обратный перенос из orditems в goods
    SELECT orditems 
    lNameGoods = ""
    lQuantityGoods = 0
    = itemStorage(@lNameGoods, @lQuantityGoods) && запомнили один пункт\товар
    SELECT goods
    = ordItemsAddToGoods(lNameGoods, lQuantityGoods) && вернули кол-во из заказа 
      && (из orditems) на место, в таблицу goods
    = itemNotice(lNameGoods, lQuantityGoods) 
      && т.к. мы исправляем заказ, то работать 
      * б не с нуля. в workQuan запомним старое значение
  ENDSCAN
  
  SELECT orditems 
  DELETE ALL FOR ALLTRIM(order_id) == ALLTRIM(lId)
  PACK && удаляем ненужные пункты заказа

  lInputResult = qualityInputM() && изменение кол-в зак-х товаров
  IF ALLTRIM(lInputResult) == ALLTRIM(gFalseOrdId) 
    && т.е. все позиции = 0, фактич заказа нет
    SELECT order 
    DELETE && удаляем заказ, что изменяли
    do delDeleted 
  ELSE
    = goodsToOrdItemsM(lId)  
    SELECT order 
    ? "Вы изменили заказ."
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
  titleStr =  "Выберите стрелками желаемый заказ, затем нажмите ^-W"  

  SELECT order
  BROWSE FONT "Courier New", 16 ;
    FIELDS dateoper, customer_i, amount  ;
    TITLE titleStr 
  RETURN id
ENDFUNC

PROCEDURE ordPrnt1 && для п. меню "Вывод заказа на экран"
  LOCAL lOrdId, lOrdSum

  lOrdId = ordChoice()

  IF EMPTY(lOrdId) .or. readkey() = 12 && нажали Esc
    ? "Выбор заказа для вывода отменён, вы нажали Esc."
    RETURN 
  ENDIF

  CLEAR 
  DO hatPrnt
  DO bodyPrnt WITH lOrdId
ENDPROC && ordPrnt1

PROCEDURE ordPrnt2  && для п. меню "создать заказ" и ещё кое-где
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
  SELECT order && на всякий случай
ENDPROC && ordPrnt2

PROCEDURE bodyPrnt
  * печать тела (пунктов) заказа
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
    ? "реальных пунктов в заказе нет"
    ? "заказ подлежит удалению"
  ENDIF
ENDPROC && bodyPrnt


FUNCTION qualityInputM()
  * исп-ся в ordModification
  LOCAL titleStr, validErrStr

  titleStr =  "В Zakaz введите желаемое кол-во товара. ^-W - выход."
  validErrStr = "Нужно вводить кол-во >= 0 и <= наличного кол-ва"

  SELECT goods

  BROWSE FONT "Courier New", 16 freeze workQuan;
    FIELDS name, price, quantity,;
      workQuan :H="Zakaz",;
      amount = price * workQuan :9;
    VALID :F workQuan >= 0 .and. workQuan <= quantity;
    ERROR validErrStr TITLE titleStr

  SUM workQuan * price ALL FOR workQuan > 0 TO lOrdSum

  SELECT order

  IF lOrdSum = 0 && т.е. ничего не заказано, все кол-ва =0
    RETURN gFalseOrdId
  ENDIF

  && решил при исправлении заказа не изменять дату.
  && А то хлопотно потом искать исправленный, особенно
  && учитывая, что сумма тоже изменяется, а id малочитаем

  REPL amount WITH lOrdSum
  REPL customer_i WITH gCurrCust
  RETURN id
ENDFUNC && qualityInputM


FUNCTION qualityInputC()
  * исп-ся и в ordCreation 

  LOCAL titleStr, validErrStr

  titleStr =  "В Zakaz введите желаемое кол-во товара. ^-W - выход."
  validErrStr = "Нужно вводить кол-во >= 0 и <= наличного кол-ва"

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

  IF lOrdSum = 0 && т.е. ничего не заказано, все кол-ва =0
    ? "Заказ ", id, " изменён так, что все количества = 0."
    ? "Мы его удаляем."
    DELETE
    DO delDeleted
    RETURN gFalseOrdId
  ENDIF

  APPEND BLANK
  REPL dateoper WITH Date()
  REPL amount WITH lOrdSum
  REPL customer_i WITH gCurrCust

  REPL id WITH sys(2015)  
    && sys(2015) генерирует случайное имя д нового рабочего файла
    * В нашем случае оно подходит для генерации уникального идентификатора заказа

  RETURN id
ENDFUNC && qualityInputC

PROCEDURE goodsToOrdItemsM(pId)
  * исп-ся в ordModification
  LOCAL lQuantity, lPrice, lGoodsName

  && переносим пункты заказа из goods в orditems
  SELECT goods
  SCAN FOR workQuan > 0 
    lQuantity = workQuan
    lPrice = price

    REPL quantity WITH quantity - workQuan
    REPL workQuan WITH 0
      * те 2 строки, что выше, сильно упрощённая операция с данными.
      * Годится только для ТЗ. Заказы могут отменяться, товары откладываться, старые заказы храниться и т.п.

    lGoodsName = name && в этой БД название товара выступает его 
    * кодом. Для простоты. В реальности следовало бы определить 
    * суррогатный ключ, назначить его pk-ем, связывать таблицы по нему и т.д.

    SELECT orditems
    APPEND BLANK
    REPL namegoods WITH lGoodsName
    REPL price WITH lPrice
    REPL quantity WITH lQuantity
    REPL amount WITH price * quantity && эта избыточность очень удобна 
    REPL order_id WITH pId

    SELECT goods
  ENDSCAN
ENDPROC && goodsToOrdItemsM 


PROCEDURE itemNotice(pNameGoods, pQuantityGoods)
  SELECT goods &&- - - - лишний блок, т.к. мы это делаем
  && в проге раньше, но для отладки пусть будет
  LOCATE FOR ALLTRIM(name) == ALLTRIM(pNameGoods)
  IF !FOUND()
    ? "Странно, но товар", name, " не найден."
    RETURN
  ENDIF && - - - -
  ? "Перенесли товара ", name, "в goods.workQuan ", pQuantityGoods, " штук"
  REPL workQuan WITH pQuantityGoods
ENDPROC && itemNotice

FUNCTION custNameFromId
  * по id покупателя выдаёт его имя
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
  ? "Заказ ", id, " от ", dateoper, 
  ?? " покупателя =", custNameFromId( customer_i)
  ?? " сумма заказа =", amount
  ? "----------------------"
ENDPROC


PROCEDURE ordItemsAddToGoods(pNameGoods, pQuantityGoods)
  LOCATE FOR ALLTRIM(name) == ALLTRIM(pNameGoods) && мы в goods
  IF !FOUND()
    ? "товар '", pNameGoods, "' не найден"
    RETURN && плохой возврат
  ENDIF
  REPL quantity WITH pQuantityGoods + quantity
ENDPROC && ordItemsAddToGoods


PROCEDURE ordDeletion
  LOCAL titleStr, lId, lNameGoods, lQuantityGoods,;
    lDateOper, lOrdSum
  
  titleStr = '^-T - пометить\отменить заказ для удаления. Он будет отмечен "*" или пробелом'

  SELECT order
  BROWSE FONT "Courier New", 16 ;
    FIELDS del=iif(deleted(), "*"," "), dateoper, customer_i, amount  ;
    TITLE titleStr 

  CLEAR 
  DO delDeleted
ENDPROC


PROCEDURE delDeleted
  && удаляет заказы, помеченные д удаления в orders
  LOCAL cnt22

  SELECT order

  && - - - - проверяем, что есть что удалять
  COUNT FOR DELETED() TO cnt22 
  IF cnt22 = 0
    ? "удалять нечего. (нет помеченных д удал-я заказов)"
    RETURN 
  ENDIF && - - - -

  SCAN FOR DELETED() && цикл по order
    lId = id

    ? "Вы удалили:" &&- - - -  необязательный вывод
    DO hatPrnt
    DO bodyPrnt WITH lId &&- - - - 

    SELECT orditems
    SCAN FOR ALLTRIM(order_id) == ALLTRIM(lId) && цикл по orditems
      * тут мы делаем обратный перенос из orditems в goods
      SELECT orditems 
      lNameGoods = ""
      lQuantityGoods = 0
      = itemStorage(@lNameGoods, @lQuantityGoods) && запомнили один пункт\товар

      SELECT goods
      = ordItemsAddToGoods(@lNameGoods, @lQuantityGoods) && вернули кол-во из заказа 
        && на место, в таблицу goods
    ENDSCAN
    
    SELECT orditems 
    DELETE ALL FOR ALLTRIM(order_id) == ALLTRIM(lId)

    SELECT order
  ENDSCAN

  PACK IN orditems && удаляем ненужные пункты заказа
  PACK IN order
  * Примерно это же можно сделать SQL-запросами. 
    *!*	 DELETE FROM orditems WHERE order_id IN 
    *!*	   ( "xxxxxxxxxx", "yyyyyyyyy")
ENDPROC && delDeleted


PROCEDURE init
  * это объявлению пер-х дб после очистки памяти!!!! (CLEAR ALL)
  public gBeginDate, gEndDate, modifyConst, createConst, gFalseOrdId,;
  gCurrCust, withIndex, withoutIndex, gMenuInd

  *!*	 gBeginDate = Date()
  *!*	 gEndDate = Date()
  gBeginDate = CTOD("01.11.21") && врем заглушки, чтоб было меньше проблем с вводом при тестировании
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

  =op_all_dbf(.t.) && открыли с индексами
ENDPROC && init


PROCEDURE armExit
  gMenuInd = .f. && можно было бы ввести константы для .f. и .t.
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
  * открывает все таблицы, что в проекте
  * либо с индексами, либо без.
  * В данном проекте индексы не использую.
  * Но открываю с ними. 

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
  * наполнение таблицы ORDER.DBF случайными заказами
  * qOrd заказов с maxQItems максимальным кол-м пунктов
  * товаров в каждом заказе. Их будет случ кол-во, но меньше maxQItems.
  LOCAL randCust, maxRandCust, k, wAmount, randDate

  SELECT CUSTOMER
  maxRandCust = RECCOUNT()

  FOR k = 1 TO qOrd
    randCust = randNatural(maxRandCust)
    * полагаем, что CUSTOMER.DBF не пустая
    * выбрали случайного покупателя

    SELECT customer
    go top
    skip randCust-1 && подвели в таблице нужного покупателя
        && можно было бы оформить отдельной ф-цией

    SELECT order
    APPEND BLANK
    randDate = Date()-randNatural(20)
    REPL dateoper WITH randDate && дату покупки тоже делаем случайной
    REPL id WITH orderIdHash(customer.id, randDate)
    REPL customer_i WITH customer.id

    SELECT ORDITEMS
    wAmount = genRandOrderItems(order.id, maxQItems)

    SELECT order
    REPL amount WITH wAmount
  ENDFOR

  SELECT order
  DELETE ALL for amount = 0
  PACK && это заказы, где сумма 0. Предполагаю что и пунктов в нём будет 0
ENDPROC  && fillOrders


FUNCTION orderIdHash(id2, date)
  LOCAL d, k, mod22, h, C, x, s, ls, t, i
  RETURN sys(2015) && вынужденная мера. Тот хэш, что взял в сети,
  * мне не понравился. 

  d = DTOC(date)
  d = substr(d,1,2)+substr(d,4,2)+substr(d,7,2)
  d = lower(alltrim(id2))+d  && lower - чтоб уменьшить алфавит и k
  d = d

  * ---- эту часть, что ниже, взял в сети. Не сам писал. ----
  * только чуть переделал и с C++ перевёл

  *!*	 k = 36 т.к. 26 букв лат алфавита + 10 цифр
  k = 36
  mod22 = 1e10+7
  h = 0 
  for i = 1 to len(d)
    c = substr(d,i,1)
    x = asc(c) - asc('a') + 1
    h = (h + k * x) % mod22
  ENDFOR
  * конец заимствования ----
  s = str(h)
  ls = len(s)
  IF ls < 10 && 10 - ширина поля order.id
    s = s+replicate('5', 10 - ls)
  ENDIF
  IF ls > 10 
    s = right(s, 10)
  ENDIF
  RETURN ltrim(s)
ENDPROC && orderIdHash


FUNCTION genItem
  * генерирует 1 пункт товара из заказа
  * для genRandOrderItems. 
  parameters goodsName, goodsQuantity, goodsPrice
  LOCAL maxGoodsItem

  maxGoodsItem = RECCOUNT()
  randGoodsItem = randNatural(maxGoodsItem)
  go top
  skip randGoodsItem-1 && подвели в таблице goods
        && случайный товар

  IF quantity = 0 && по идее, эту проверку 
    * нужно делать снаружи этой процы. 
    * Это не входит в её обязанности
    RETURN .f.
  ENDIF

  goodsQuantity = randNatural(round(quantity / 4, 0)) && /4 чтоб не забирать весь остаток товара
  * repl quantity with  quantity - goodsQuantity 
    && сразу уменьшили запас на складе
    && но в режиме генерации случ заказов этого можно не делать

  goodsName = name
  goodsPrice = price
  RETURN .t.
ENDFUNC && genItem


PROCEDURE genRandOrderItems(pId, maxQItems)
  * генерируем д заказа (pId) случ кол-во 
  * пунктов заказа
  LOCAL qItems, j, goodsName, goodsQuantity, goodsPrice, goodsGenOk, totalOrdSum
  goodsName = SPACE(10)
  goodsQuantity = 0
  goodsPrice = 0
  goodsGenOk = .f.
  totalOrdSum = 0

  qItems = randNatural(maxQItems) && maxQItems - условный максимум видов товаров 
      && в одном заказе. 

  FOR j = 1 to qItems
    SELECT goods
    goodsGenOk = genItem(@goodsName, @goodsQuantity, @goodsPrice)

    SELECT orditems
    IF !goodsGenOk && не сформировалось из-за 0-го кол-ва
      LOOP
    ENDIF  

    * проверяем, чтоб 2й раз по тому же товару не шла генерация
    IF alredyBe(goodsName, pId) && такой-то товар 
        && в таком-то заказе 
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
  * для genRandOrderItems. 
  * Дела происходят в orditems
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
  * наполняет нашу таблицу товаров 
  * случайными фиктивными товарами
  * в случайном количестве
  sele 0
  USE GOODS INDEX GOODS 
  *INDEX on name TO GOODS

  LOCAL k, lastNumber

  FOR k = 1 TO pQuanGoods
    APPEND BLANK
    REPL name WITH "Товар" + alltrim(STR(k))
    REPL price WITH priceGen(5)
    REPL quantity WITH round(randDiapazon(0, 1000), 0)
  ENDFOR
  CLOSE ALL
  CLEAR ALL
  CLEAR
ENDPROC && fillGoods 


FUNCTION priceGen(maxRazrad)
  * генерирует случ цену 
  LOCAL koef, result, randRazrad, i

  randRazrad = randNatural(maxRazrad)
  && получаем случ кол разрядов

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
  * общая ф-ция. Растягивает случ величину на диапазон
  * возвращает вещественное число, т.е. не целое
  LOCAL l
  l = ABS(a - b)
  RETURN  a + l * rand()
ENDFUNC


FUNCTION randNatural(b)
  * возвращает целое от 1 до b. Не 0!!!
  RETURN round(randDiapazon(1, b), 0)
ENDFUNC

* ====== SQL-раздел (ТЗ 2, оно есть отдельно) 
* =============== тут закоментировано ===========

*!*	 SELECT c.name, sum(amount) ;
*!*	   FROM customer as c;
*!*	   left JOIN order as o;
*!*	     ON o.customer_i = c.id; 
*!*	   GROUP BY c.name;
*!*	   where o.customer_i is not null
* --------- получили вывод сумм заказов по каждому покупателю.

* ------------------
*!*	 SELECT c.name, sum(amount) ;
*!*	   FROM customer as c;
*!*	   left JOIN order as o;
*!*	     ON o.customer_i = c.id; 
*!*	   GROUP BY c.name; 
*!*	   where o.customer_i is null
* --------- Нашли покупателей без заказа.

* =====================================
* ниже процы, кот не исп-ся в тек реализации.
* были зачем-то нужны в процессе создания АРМа,
* затем исплючены из использования. 
* Оставил на всякий случай
* -------------------------------




PROCEDURE goodsToOrdItems(isCreate,pId)
  *!*	 isCreate тут значит то же самое, что и в qualityInput
  * isCreate = .t. - вызов был для ordCreation
  * isCreate = .f. - для ordModification
  * в init ввёл для этого константы

  * использовалась в ordCreation и ordModification
  * путаницы не было, но всё равно разделил

  LOCAL lQuantity, lPrice, lGoodsName

  && переносим пункты заказа из goods в orditems
  SELECT goods
  SCAN FOR workQuan > 0 
    lQuantity = workQuan
    lPrice = price

    REPL quantity WITH quantity - workQuan
    REPL workQuan WITH 0
      * те 2 строки, что выше, сильно упрощённая операция с данными.
      * Годится только для ТЗ. Заказы могут отменяться, товары откладываться, старые заказы храниться и т.п.

    lGoodsName = name && в этой БД название товара выступает его 
    * кодом. Для простоты. В реальности следовало бы определить 
    * суррогатный ключ, назначить его pk-ем, связывать таблицы по нему и т.д.

    SELECT orditems
    IF isCreate
      APPEND BLANK
    ENDIF
    REPL namegoods WITH lGoodsName
    REPL price WITH lPrice
    REPL quantity WITH lQuantity
    REPL amount WITH price * quantity && эта избыточность очень удобна 
    REPL order_id WITH pId

    SELECT goods
  ENDSCAN
ENDPROC


PROCEDURE onePointPrint
  ? namegoods, " ", price, " ", quantity
ENDPROC


PROCEDURE demoOrdPrnt
  * просто печатаем заказ и одну строку из него
  * это для проверки правильности таблиц
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
  * то же самое что и demoOrdPrnt
  * но печать наоборот, от orditems

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
  * пытаемся увидеть связь order и orditems
  = op_all_dbf(.t.)
  SELECT namegoods, dateoper, customer_i ;
    FROM orditems as it;
    left JOIN order as o;
      ON it.order_id = o.id
ENDPROC && viewRelDB


PROCEDURE delCalc
  * для подсчёта записей, помеченных д удаления, в лоб
  * можно было бы использовать COUNT, но у меня были сомнения
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
  * удаляет все заказы. Использовалась на этапе отладки
  * процы генерации случайных заказов.
  CLOSE ALL

  use orditems
  DELETE ALL
  PACK

  use order
  DELETE ALL
  PACK

  CLOSE ALL
ENDPROC && genRecall

