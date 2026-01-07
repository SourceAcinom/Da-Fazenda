#Include "rwmake.ch"
#Include "protheus.ch"
#Include "totvs.ch"
#Include "topconn.ch"

/*------------------------------------------------------------------------*
 | P.E.      : MA650EMP                                                   |
 | Descrição : Ponto de Entrada para manipular informações de empenhos    |
 | Chamada   : ExecBlock('MA650EMP',.F.,.F.)                              |
 | Link      : https://tdn.totvs.com/pages/viewpage.action?pageId=6089387 |
 *------------------------------------------------------------------------*/
User Function MA650EMP()
    Local aBkpArea := GetArea()
    Local aSD4Area := SD4->(GetArea())
    Local aSB1Area := SB1->(GetArea())

    Local cArmazem := ""
    Local cSql     := ""

	cSql := "   SELECT R_E_C_N_O_ REC "
    cSql += "     FROM " + RetSQLName("SD4")
	cSql += "    WHERE D_E_L_E_T_ = ' ' "
	cSql += "      AND D4_FILIAL  = '" + FWxFilial("SD4") + "'"
	cSql += "      AND SUBSTR(D4_OP, 1, 6) = '" + SC2->C2_NUM + "'"
    cSql += " ORDER BY REC "

	TcQuery cSql New Alias "Q_SD4"
    DbSelectArea("Q_SD4")
    DbGoTop()

    Do While !Eof()
        cArmazem := Space(TamSX3("D4_LOCAL")[1])

        DbSelectArea("SD4")
        DbGoto(Q_SD4->REC)

        DbSelectArea("SB1")
        DbSetOrder(1) // Filial + Código

        If DbSeek(FWxFilial("SB1") + SD4->D4_COD)
            cArmazem := SB1->B1__LOCEMP
        EndIf

        If !Empty(cArmazem) .And. cArmazem <> SD4->D4_LOCAL
            DbSelectArea("SD4")

            If RecLock("SD4", .F.)
                Replace SD4->D4_LOCAL With cArmazem
                MsUnLock()
            EndIf
        EndIf

        DbSelectArea("Q_SD4")
        DbSkip()
    EndDo

    Q_SD4->(DbCloseArea())

    RestArea(aSB1Area)
    RestArea(aSD4Area)
    RestArea(aBkpArea)
Return
