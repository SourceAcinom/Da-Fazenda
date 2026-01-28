#INCLUDE "PROTHEUS.CH"



// Ponto de entrada responsável por incluir colunas no aCols e no aHeader da tela de empenhos
USER FUNCTION MTA650AC()
    Local aArray := {}

    // Adiciona Produto Pai na tela de empenhos
    aadd(aArray,{"Produto Pai", "D4_PRODUTO", "@!", TamSX3("D4_PRODUTO")[1], "", "", .T., "C", "", " "})
    aadd(aArray,"SD4->D4_PRODUTO")

    // aadd(aArray,{"Descrição Produto Pai", "B1_DESC", "@!", TamSX3("B1_DESC")[1], TamSX3("B1_DESC")[2], "", .T., "C", "", " "})
    // aadd(aArray,"SB1->B1_DESC")
    
RETURN aArray
