#include "totvs.ch"

/*/{Protheus.doc} VLDCOND
Funcao para retornar .T. caso o cliente tenha Desconto Financeiro informado no cadastro do Cliente. Pedido de vendas ira priorizar o desconto financeiro do Cadastro do Cliente sob a condicao de pgto.
Utilizado no gatilho C5_CONDPAG - 501
@type Function
@author Matheus Oliveira
@since 01/09/2025
/*/
User Function VLDCOND(xCliLj)
	Local c_Cliente := xCliLj
	Local lRet   := .f.
        Local aArea  := getArea()

	dbSelectArea("SA1")
	dbSetOrder(1)
	if dbseek(xFilial("SA1")+c_Cliente)
		if SA1->A1_X_DESCF > 0
			lRet := .t.
		endIf
	endIF

        restArea(aArea)

Return lRet
