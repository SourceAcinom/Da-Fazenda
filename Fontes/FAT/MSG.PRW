#include "totvs.ch"


user function msg(xTipo,xForms)

        Local aArea := getArea()
        Local cNota := ""
        Local cSerie := ""
        Local cFornece := ""
        Local cLoja   := ""
        Local cMsg := ""
        Local nAlq := 0
        Local nTot := 0
        Local aForms   := {}
        Local nX := 0
        Local lLoop := .t.

        cNota := SF1->F1_DOC
        cSerie := SF1->F1_SERIE
        cFornece := SF1->F1_FORNECE
        cLoja := SF1->F1_LOJA

        if xTipo == "FUN"
                DBSelectArea("SD1")
                DBSetOrder(1)
                DBSeek(xFilial("SD1")+SF1->F1_DOC+SF1->F1_SERIE+SF1->F1_FORNECE+SF1->F1_LOJA)
                while(!eof() .and. SD1->D1_DOC == cNota .and. SD1->D1_SERIE ==  cSerie .and. cFornece == SD1->D1_FORNECE .and. cLoja == SD1->D1_LOJA)
                        nAlq := SD1->D1_ALIQINS+SD1->D1_ALIQFUN+SD1->D1_ALSENAR
                        nTot += SD1->D1_VALINS+SD1->D1_VLSENAR+SD1->D1_VALFUN
                        SD1->(dbSkip())
                endDo
                cMsg += " " + ALLTRIM(TRANSFORM(nAlq,"@E 999.99"))+"% = R$ "+ALLTRIM(TRANSFORM(nTot,"@E 999,999,999.99"))
        elseif xTipo == "LEG"
                If ! SF2->F2_TIPO $ "B-D"
                        DbSelectArea("SM4")
                        DbSetOrder(1)
                        If SM4->(MsSeek(xFilial("SM4") + '999'))
                                aForms := Separa(xForms, '-')
                                For nX := 1 to Len(aForms)
                                        lLoop := .t.
                                        DbSelectArea("SM4")
                                        DbSetOrder(1)
                                        If SM4->(MsSeek(xFilial("SM4") + aForms[nX]))
                                                DBSelectArea("SD2")
                                                DBSetOrder(3)
                                                if DBSeek(xFilial("SD2")+SF2->(F2_DOC+F2_SERIE+F2_CLIENTE+F2_LOJA))
                                                        while(!eof() .and. xFilial("SD2")+SD2->(D2_DOC+D2_SERIE+D2_CLIENTE+D2_LOJA) == xFilial("SF2")+SF2->(F2_DOC+F2_SERIE+F2_CLIENTE+F2_LOJA) .and. lLoop )
                                                                if alltrim(SD2->D2_COD) $ alltrim(SM4->M4_FORMULA) .AND. SD2->D2_TP == 'PA'
                                                                        cMsg += " * " + alltrim(SM4->M4_DESCR)
                                                                        lLoop := .f.
                                                                EndIf
                                                                SD2->(dbSkip())
                                                        endDo
                                                ENDiF
                                        endIf
                                Next nX
                        EndIf
                EndIf
                
        endIF
        restArea(aArea)

return(cMsg)
