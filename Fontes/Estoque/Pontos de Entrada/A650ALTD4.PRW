#Include "rwmake.ch"
#Include "protheus.ch"
#Include "totvs.ch"

/*---------------------------------------------------------------------------------------------------------------*
 | P.E.      : A650ALTD4                                                                                         |
 | Descrição : Ponto de Entrada executado na rotina de inclusão de OP, antes da apresentação da tela de empenhos |
 | Chamada   : ExecBlock("A650ALTD4",.F.,.F.,{cStrOpc})                                                          |
 | Link      : https://tdn.totvs.com/display/public/mp/A650ALTD4                                                 |
 *---------------------------------------------------------------------------------------------------------------*/
User Function A650ALTD4()
	Local aBkpArea := GetArea()
	Local aSB1Area := SB1->(GetArea())

	Local nPosProd	:= GdFieldPos("G1_COMP")
	Local nPosLoc   := GdFieldPos("D4_LOCAL")
	Local nZ

	Local cProd

	// Alterar o armazém dos empenhos

	SB1->(DbSetOrder(1)) // Filial + Produto

	// Percorre todos os itens empenhados
	For nZ := 1 To Len(aCols)
		cProd := aCols[nZ, nPosProd]

		If SB1->(DbSeek(FWxFilial("SB1") + cProd))
			// Se está preenchido o armazém de empenho no cadastro de produtos, considera este armazém para gerar o empenho para produção
			If !Empty(SB1->B1__LOCEMP)
				aCols[nZ, nPosLoc] := SB1->B1__LOCEMP
			EndIf
		EndIf
	Next nZ

	RestArea(aSB1Area)
	RestArea(aBkpArea)
Return
