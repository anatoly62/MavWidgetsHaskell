initGui::String->Int->Int->IO (HWND, GuiPtr)
loadGui::String->Int->Int->IO (HWND, GuiPtr)
loopGui :: IO ()
tryE::String->IO()->IO()
message:: String->IO ()

example: --for main window
(mainWindow,root)<-initGui "file://c:/sciter/index.html" 800 500
.......
windowShow mainWindow
......
loopGui

example: --for child window
(childWindow,root)<-loadGui "file://c:/sciter/docSimple.html" 500 170
windowShow childWindow
tryE "Some error" $ do //if error message "Some error" ++ error will be shown 
  .....
  .....

example --wait dataReady
 (childWindow,root)<-loadGui "file://D:/sciter/docWithAgents.html" 520 170
  windowShow childWindow
  windowOnEvent mainWindow $ \_ _ evtg param->do
    when (evtg==handleBehaviorEvent) $ do
      cmd<-peekByteOff param 0 ::IO UINT
      when (cmd==documentReady) $ do
		message "data is ready here"
		.....
	return False
 return() 	

___________________________________button______________________________________
button::GuiPtr->String->IO GuiPtr
buttonGetText::GuiPtr->IO ByteString
buttontSetText::GuiPtr->ByteString->IO ()
buttonOnClick:: GuiPtr->IO ()->IO()

example: 
<input id="buttonAdd" type="button" value="old caption"/>

btAdd<-button root "buttonAdd"
buttontSetText btAdd "new caption"
buttonOnClick btAdd $ print $ buttonGetText btAdd
_______________________________________________________________________________

_________________________________checkButton___________________________________
checkButton::GuiPtr->String->IO GuiPtr
checkButtonIsChecked::GuiPtr->IO Bool
checkButtonCheck::GuiPtr->Bool->IO ()
checkButtonOnChange:: GuiPtr->IO ()->IO()
_______________________________________________________________________________

___________________________________combo_______________________________________
combo::GuiPtr->String->IO GuiPtr
comboGetText::GuiPtr->IO ByteString
comboSetText::GuiPtr->ByteString->IO UINT
comboGetIndex::GuiPtr->IO Int
comboSetIndex::GuiPtr->Int->IO ()  
comboFill::GuiPtr->[ByteString]->IO () 
comboGetItems::GuiPtr->IO[ByteString]
comboSearch::GuiPtr->[ByteString]->ByteString->Int->IO()
comboOnChange:: GuiPtr->IO ()->IO()

example: 
<select id="comboUsers"></select>

cbUsers<-combo root "comboUsers"
comboFill cbUsers ["Ann","Ivan","Julia"]
comboSetText  cbUsers "Julia" -- or comboSetIndex  cbUsers 2
comboOnChange cbUsers $ print $ show $ comboGetIndex cbUsers
_______________________________________________________________________________
_________________________________datePick______________________________________
datePick::GuiPtr->String->IO GuiPtr
datePickGetText::GuiPtr->IO ByteString
datePickSetText::GuiPtr->ByteString->IO ()
datePickOnChange:: GuiPtr->IO ()->IO()

example: 
<input id="curDate" type="date" lang="uk-Ukr" />

dt<-datePick root "curDate"
datePickSetText dt "2019-11-08"
datePickOnChange $ print $ datePickGetText dt
_______________________________________________________________________________
_________________________________flatButton____________________________________
flatButton::GuiPtr->String->IO GuiPtr
flatButtonOnClick:: GuiPtr->IO ()->IO()

example: 
<img id="tbFind"src="btFind.png" />

tbFind<-flatButton root "tbFind"
flatButtonOnClick tbFind $ print "clicked"
_______________________________________________________________________________
__________________________________menuItem_____________________________________
menuItem::GuiPtr->String->IO GuiPtr
menuItemOnClick:: GuiPtr->IO ()->IO()
_______________________________________________________________________________

____________________________________menu_______________________________________
menu::GuiPtr->String->IO GuiPtr
menuShow::GuiPtr->GuiPtr->IO ()
_______________________________________________________________________________

___________________________________table_______________________________________
table::GuiPtr->String->IO GuiPtr
tableGetIndex::GuiPtr->IO Int
tableSetIndex::GuiPtr->Int->IO()
tableScroll::GuiPtr->IO()  
tableScrollTo::GuiPtr->Int->IO()
tableAddRow::GuiPtr->c->[ c->  ByteString]->IO ()
tableAddRowM::GuiPtr->c->[ c-> IO ByteString]->IO ()
tableChangeRow::GuiPtr->c->[ c-> ByteString]->IO ()
tableChangeRowM::GuiPtr->c->[ c-> IO ByteString]->IO ()
tableDeleteRow::GuiPtr->IO ()
tableFill::GuiPtr->[c]->[ c-> ByteString]-> IO UINT
tableFillM::GuiPtr->[c]->[ c->IO ByteString]-> IO ()
tableGetRow::GuiPtr->Word32->IO Addr
tableSetHeaderText::GuiPtr->Int->ByteString->IO()  
tableSearch::GuiPtr->[ByteString]->ByteString->Int->IO()
tableOnClick:: GuiPtr->(Int->IO())->IO()
tableOnDblClick:: GuiPtr->(Int->IO())->IO()
tableOnMenu:: GuiPtr->(Int->IO())->IO()

example: 
<table  border="0" >
<thead ><tr><th>Id</th><th>Name</th></tr></thead>
<tbody id="tblUsers" >
</tbody>
</table>

data User=User{userId:: !Int,userName:: !ByteString}
userStore=[User{userId=1,userName="Ann"},User{userId=2,userName="Ivan"}]
func=[fromInt.userId,userName]
tbl<table root "tblUsers"
tableFill tbl userStore func
tableSetIndex tbl 1
tableOnClick tbl $ \n-> print $ "clicked in line "++ show n
_______________________________________________________________________________

_________________________________text__________________________________________
text::GuiPtr->String->IO GuiPtr
textGetText::GuiPtr->IO ByteString
textSetText::GuiPtr->ByteString->IO ()
textSelectAll::GuiPtr->IO()
textOnChange:: GuiPtr->IO ()->IO()
textOnKey:: GuiPtr->(Int->IO())->IO()

example:
<input id="textName" type="text"/>

textName<text root "textName"
textSetText textName "Ann"
textOnChange textName $ print $textGetText textName
textOnKey textName $ \n-> when(n==13) $ print "enter pressed"
_______________________________________________________________________________
___________________________________window______________________________________
windowShow::HWND->IO() 
windowDestroy::HWND->IO() 
windowSetText::HWND->ByteString->IO() 

___________________________________widget______________________________________
widgetSetFocus::GuiPtr->IO()
widgetEnable::GuiPtr->IO()
widgetDisable::GuiPtr->IO()
widgetShow::GuiPtr->IO()
widgetHide::GuiPtr->IO()
_______________________________________________________________________________

   

