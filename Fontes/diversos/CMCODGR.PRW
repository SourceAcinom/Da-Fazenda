#include "topconn.ch"
#include "protheus.ch"
#include "totvs.ch"


 /*/{Protheus.doc} CMCODGR
    Funcao para retornar o proximo codigo de produto disponivel
    @type  Function
    @author Matheus Oliveira
    @since 19/11/2024
    /*/
User Function CMCODGR(xGrupo) //(xGrupo)
    Local cGrupo  := xGrupo
    Local cRet    := ""
    Local cQuery  := ""

    if empty(cGrupo)
        Alert("Grupo é informação obrigatória")
        Return cRet
    endIf

    cQuery := "SELECT MAX(SUBSTR(B1_COD,5,3)) PROX FROM " + retSqlName("SB1")
    cQuery += "     WHERE SUBSTR(B1_COD ,1,4) = '" + cGrupo + "' " 
    cQuery += "     AND D_E_L_E_T_ = ' ' "
    cQuery += "     AND B1_FILIAL = '" + xFilial("SB1") + "' "
    if Select("_PRO") > 0
        _PRO->(DBCloseArea())
    endIF
    tcQuery cQuery new Alias "_PRO"
    DBSelectArea("_PRO")
    dbGoTop()

    if !_PRO->(eof()) .and. val(_PRO->PROX) > 0
        cRet := cGrupo+alltrim(StrZero(val(_PRO->PROX)+1,3))
    else
        cRet := cGrupo+StrZero(1,3)
    endIF
    
    _PRO->(DBCloseArea())

Return cRet
