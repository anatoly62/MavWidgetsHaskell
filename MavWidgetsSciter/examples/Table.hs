{-# LANGUAGE OverloadedStrings #-}
module Table where

import Data.ByteString(ByteString)
import Data.IORef 
import System.IO.Unsafe

import Convert
import MavWidgetsSc
import Control.Monad (unless)

data Contact=Contact {contactId ::Int,contactName :: !ByteString, contactGender :: !Int}
contactsStore :: IORef [Contact]

contactsList=[Contact{contactId=1,contactName="Ann",contactGender=1},
              Contact{contactId=2,contactName="Stive",contactGender=0},
              Contact{contactId=3,contactName="Helen",contactGender=1},
              Contact{contactId=4,contactName="Ivan",contactGender=0}]

{-# NOINLINE contactsStore #-}
contactsStore = unsafePerformIO (newIORef contactsList) 

mainT= tryE "Load error" $ do
  (mainWindow,root)<-initGui "file://D:/sciter/table.html" 500 350
  windowShow mainWindow
  btAdd <- button root "btAdd"
  btChange <- button root "btChange"
  btDelete <- button root "btDelete"
  btClear <- button root "btClear"
  textId <- text root "textId"
  textName <- text root "textName"
  cbGender <- combo  root "cbGender"
  tableContacts <-table root "table"
  let tblFunc=[fromInt.contactId,contactName,fromInt.contactGender]
  
  store<-readIORef contactsStore
  tableFill tableContacts store tblFunc
  
  tableOnClick tableContacts $ \n->do
    store <-readIORef contactsStore 
    let item=store !! n
    textSetText textId $ (fromInt.contactId) item
    textSetText textName $ contactName item
    comboSetIndex cbGender $ contactGender item

  buttonOnClick btAdd $ tryE "Add error" $ do
    valId<-textGetText textId
    valName<-textGetText textName
    valGender<-comboGetIndex cbGender
    let item=Contact{contactId=toInt valId,contactName=valName, contactGender=valGender}
    modifyIORef'  contactsStore $ flip (<>) [item]
    tableAddRow tableContacts item tblFunc
    
  buttonOnClick btChange $ tryE "Change error" $ do
      valId<-textGetText textId
      valName<-textGetText textName
      valGender<-comboGetIndex cbGender
      let id=toInt valId 
      let item=Contact{contactId=toInt valId,contactName=valName, contactGender=valGender}
      modifyIORef' contactsStore $ map $ \el-> if  contactId el==id then item else el
      tableChangeRow tableContacts item tblFunc  

  buttonOnClick btDelete $ tryE "Delete error" $ do
    l <- tableGetIndex tableContacts
    store <- readIORef  contactsStore 
    unless (null store || l<0) $ do
            let item=store !! l
            let id = contactId item
            modifyIORef' contactsStore $  filter $ \el-> contactId el/=id
            tableDeleteRow tableContacts
            
  buttonOnClick btClear $ do
    textSetText textId "0"
    textSetText textName ""
    comboSetIndex cbGender 0
              
  loopGui
