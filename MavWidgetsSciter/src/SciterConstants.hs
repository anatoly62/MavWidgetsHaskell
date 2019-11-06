module SciterConstants where

import Foreign
import GHC.Windows (UINT)

ptrLen=4::Int --4 for 32-bit app and 8 for 64-bit
dllName="sciter.dll"

scDomOk= 0
scDomInvalidHwnd= 1
scDomInvalidHandle= 2
scDomPassiveHandle= 3
scDomInvalidParameter= 4
scDomOperationFailed= 5
scDomOkNotHandled= -1

handleMouse = 0x0001::Word32          
handleKey = 0x0002::Word32 
handleBehaviorEvent = 0x0100::Word32 

mainMouseButton=1::Word16
propMouseButton=2::Word16

mouseUp=3::Word16
mouseDblClick=5::Word16

documentReady=0xc3::UINT

numSciterLoadFile=7::Int
numSciterSetCallback=9::Int 
numSciterGetRootElement=33::Int
numSciterGetCount=36::Int
numSciterGetNthChild=37::Int
numSciterGetParent=38::Int
numSciterGetText=40::Int
numSciterSetText=41::Int
numSciterGetAttribute=45::Int
numSciterSetAttribute=46::Int
numSciterGetIndex=48::Int
numSciterGetType=49::Int
numSciterGetStyle=51::Int
numSciterSetStyle=52::Int
numSciterScrollToView=54::Int
numSciterUpdateElem=55::Int
numSciterSelectElement=62::Int
numSciterSetHtml=65::Int
numSciterShowPopup=68::Int
numSciterSetState=72::Int
numSciterCreateElem=73::Int
numSciterInsertElem =75::Int
numSciterDeleteElem=77::Int
numSciterAttachEventHandler=80::Int
numSciterAttachWindowEventHandler=81::Int
numSciterScriptMethod=97::Int
numSciterGetValue=102::Int
numSciterSetValue=103::Int
numValueStringData=133::Int
numValueIntData=135::Int
numValueIntDataSet=136::Int
numValueInt64Data=137::Int
numValueInt64DataSet=138::Int
numValueFromString=151::Int
