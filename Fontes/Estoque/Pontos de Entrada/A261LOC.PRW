#Include "rwmake.ch"
#Include "protheus.ch"
#Include "totvs.ch"

/*----------------------------------------------------------------------------------*
 | P.E.      : A261LOC                                                              |
 | Descrição : Permite validar o armazém origem e destino antes da validação padrão |
 | Chamada   : ExecBlock("A261LOC",.F.,.F., {aCols[n,nPosCODOri],cLocal,nOrigDest}) |
 | Link      : https://tdn.totvs.com/pages/releaseview.action?pageId=6087513        |
 *----------------------------------------------------------------------------------*/
User Function A261LOC()
    Local aBkpArea := GetArea()
    Local aSB2Area := SB2->(GetArea())

    Local c_Produto := ParamIXB[1]
    Local c_Armazem := ParamIXB[2]
    //Local c_Origem  := ParamIXB[3] // 1 = Armazém origem / 2 = Armazém destino

    // Cria o produto no armazém caso não exista
    DbSelectArea("SB2")
    DbSetOrder(1)
    If !DbSeek(FWxFilial("SB2") + PadR(c_Produto, TamSX3("B2_COD")[1]) + PadR(c_Armazem, TamSX3("B2_LOCAL")[1]))
        CriaSB2(c_Produto, c_Armazem)
    EndIf

    RestArea(aSB2Area)
    RestArea(aBkpArea)
Return
