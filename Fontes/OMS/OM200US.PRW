#INCLUDE "PROTHEUS.CH"
 
User Function OM200US()
 
        Local aRotina := ParamIxb
 
        Aadd(aRotina, {OemToAnsi("* NFE SEFAZ"), "SPEDNFE", 0 , 5, 0, NIL})
        Aadd(aRotina, {OemToAnsi("* Relatório Cargas"), "U_LATFAR01()", 0 , 7, 0, NIL})
        Aadd(aRotina, {OemToAnsi("* Impressao P.V."), "U_LATFAR02()", 0 , 7, 0, NIL})
        
Return(aRotina)
