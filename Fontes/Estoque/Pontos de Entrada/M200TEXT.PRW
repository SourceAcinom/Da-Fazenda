#Include "rwmake.ch"
#Include "protheus.ch"
#Include "totvs.ch"

/*-------------------------------------------------------------------------------------------------------------------------------------*
 | P.E.      : M200TEXT                                                                                                                |
 | Descrição : Ponto de Entrada para exibir a descrição do produto na árvore da estrutura                                              |
 | Chamada   : cM200TEXT := ExecBlock("M200TEXT", .F., .F., {cPrompt,;	            // Texto original                                  |
 |													  P200RetInf(cCargo, "PAI"),;   // Codigo do item PAI                              |
 |													  cTrtComp,;                    // TRT                                             |
 |													  P200RetInf(cCargo, "COMP"),;  // Codigo do componente/item inserido na estrutura |
 |													  nQtdComp})                    // Qtde. do item na estrutura                      |
 | Link      : https://tdn.totvs.com.br/pages/releaseview.action?pageId=6087873                                                        |
 *-------------------------------------------------------------------------------------------------------------------------------------*/
User Function M200TEXT()
    Local aArea    := GetArea()
    Local aAreaSB1 := SB1->(GetArea())

    Local cQuant   := ""
    Local cTextOri := ParamIXB[1] // Texto original, já com o TRT e a QTDE adicionados por padrão do sistema
    Local cPai     := ParamIXB[2] // Código do item pai
    Local cTRT     := ParamIXB[3] // Sequencia TRT do item na estrutura
    Local cComp    := ParamIXB[4] // Código do componente que está sendo inserido na árvore
    Local cReturn  := ""          // retorno do PE

    Local nQuant   := ParamIXB[5] // Quantidade do item na estrutura
    Local nTamCod  := TamSX3("B1_COD")
    Local nTamQtd  := TamSX3("G1_QUANT")

    If !(Empty(cComp))
        SB1->(dbSetOrder(1))

        If SB1->(dbSeek(xFilial("SB1") + cComp, .F.)) // Posiciona no produto para obter sua descrição
            cReturn := AllTrim(cComp) + " - " + SB1->B1_DESC + Space(nTamCod[1] - Len(AllTrim(cComp)))
        EndIf

        If !Empty(cTRT)
            cTRT := " | "+cTRT
        EndIf

        cQuant := " | "+AllTrim(Str(nQuant, nTamQtd[1], nTamQtd[2]))

        If !(Empty(cPai)) .And. !(cPai == cComp) // itens internos da estrutura
            cReturn := Pad(AllTrim(cReturn)+cQuant+cTRT, Len(cReturn+cQuant+cTRT))
        EndIf
    Else
        cReturn := cTextOri
    EndIf// Restaura as áreas originais

    RestArea(aAreaSB1)
    RestArea(aArea)
Return cReturn // novo texto a ser apresentado na árvore da estrutura
