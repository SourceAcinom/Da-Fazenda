#INCLUDE "PROTHEUS.CH"



// Ponto de entrada responsável por incluir colunas no aCols e no aHeader da tela de empenhos
USER FUNCTION MTA650AC()
    Local aArea := GetArea()
    Local aArray := {}

    aadd(aArray,{"Produto Pai", "D4_PRODUTO", "@!", 100, TamSX3("D4_PRODUTO")[2], "", .T., "C", "", " "})
    aadd(aArray,"SC2->C2_PRODUTO+' - '+POSICIONE('SB1',1,xFilial('SB1')+SC2->C2_PRODUTO, 'B1_DESC')")

    RestArea(aArea)
    
RETURN aArray
