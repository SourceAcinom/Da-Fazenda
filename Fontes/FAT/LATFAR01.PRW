//Bibliotecas
#Include "Protheus.ch"
#Include "TopConn.ch"
#Include "RPTDef.ch"
#Include "FWPrintSetup.ch"
#Include "Ap5Mail.ch"

//Alinhamentos
#Define PAD_LEFT    0
#Define PAD_RIGHT   1
#Define PAD_CENTER  2

//Colunas
#Define POS_QTDLIB 0010 //QUANT LIBERADA.
#Define POS_QUAN 0040 //QUANT.
#Define POS_COD  0080 //COD. PROD
#Define POS_DESC 0140 //DESCRIÇÃO
#Define POS_CAIXA 0450 //POS CAIXA
#Define POS_BANCADA 0450 //POS BANCADA

//Variáveis estáticas
Static nTamFundo  := 15
Static cEmpEmail  := Alltrim(SuperGetMV('MV_X_EMAIL', .F., ""))
Static cEmpSite   := Alltrim(SuperGetMV('MV_X_HPAGE', .F., ""))
Static nCorAzul   := RGB(062,179,206)

/*=======================================================================
Impressão Pedido Venda Expedição, utilzado para separação dos produtos
==============================-------====================================*/

User Function LATFAR01()

    Local aArea      := GetArea()
    Private cLogoEmp := fLogoEmp()
    Private cCarDe   := ""
    Private cCarAt   := ""
    Private cTipo    := ""
    
    //Cria o grupo de perguntas
    //FValidPerg(cPerg)

    private aPergs := {;
                        {1, "Carga De"   , CRIAVAR("DAK_COD",.F.) , "@!", '.T.', "DAK", '.T.', 80, .T.},;
                        {1, "Carga Ate"  , CRIAVAR("DAK_COD",.F.) , "@!", '.T.', "DAK", '.T.', 80, .T.},;
                        {1, "Emissao De" , CRIAVAR("DAK_DATA",.F.), "@D", '.T.', ""   , '.T.', 80, .T.},;
                        {1, "Emissao Ate", CRIAVAR("DAK_DATA",.F.), "@D", '.T.', ""   , '.T.', 80, .T.}}

    if isincallstack("OMS200")
        cCarDe := DAK->DAK_COD
        cCarAt := DAK->DAK_COD
        Processa({|| fMontaRel()}, "Processando...", "Aguarde gerando relatório",.F.)
    elseIf Parambox(aPergs, "Parametros para impressão das Cargas")
        cCarDe := MV_PAR01
        cCarAt := MV_PAR02
        dDatDe := MV_PAR03
        dDatAte := MV_PAR04
        Processa({|| fMontaRel()}, "Processando...", "Aguarde gerando relatório",.F.)
    endif

    //Processa({|| fMontaRel()}, "Processando...", "Aguarde gerando relatório",.F.)
    //iif(SC5->C5_X_TPPV = '2',U_RVFATR04(),nil)
    RestArea(aArea)
Return

/*---------------------------------------------------------------------*
 | Func:  fMontaRel                                                    |
 | Desc:  Função principal que monta o relatório                       |
 *---------------------------------------------------------------------*/

Static Function fMontaRel()

    Local cQryPed     := ""
    Local cQryIte     := ""
    Local nCarAtu     := 0
    //Local cCarg       := ""
    Local cQryCar     := ""
    Local cCarga      := ""
    Private cHoraEx   := Time()
    Private cNomeRel    := "CarExp_H"+STRTRAN(cHoraEx, ":", "")
    Private nPagAtu   := 1
    //Linhas e colunas
    Private nLinAtu   := 0
    Private nLinFin   := 820
    Private nColIni   := 010
    Private nColFin   := 550
    Private nLinRod2  := 0
    Private nColMeio  := (nColFin-nColIni)/2
    Private oPrintPvt
    //Declarando as fontes
    Private cNomeFont := "Arial"
    Private oFontDet   := TFont():New(cNomeFont, 9, -7,  .T., .F., 5, .T., 5, .T., .F.)
    Private oFontDet2 := TFont():New(cNomeFont, 5, -7,  .T., .F., 5, .T., 5, .T., .F.)
    //Private oFontDet3 := TFont():New(cNomeFont, 6, -10,  .T., .F., 5, .T., 5, .T., .F.)
    //Private oFontDetN := TFont():New(cNomeFont, 9, -7,  .T., .T., 5, .T., 5, .T., .F.)
    Private oFontDet3 := TFont():New(cNomeFont, 6, -11,  .T., .F., 5, .T., 5, .T., .F.)
    Private oFontDetN := TFont():New(cNomeFont, 9, -9,  .T., .T., 5, .T., 5, .T., .F.)
    Private oFontRod := TFont():New(cNomeFont, 9, -6,  .T., .F., 5, .T., 5, .T., .F.)
    Private oFontTit := TFont():New(cNomeFont, 9, -13, .T., .T., 5, .T., 5, .T., .F.)
    Private oFontCab := TFont():New(cNomeFont, 9, -9,  .T., .F., 5, .T., 5, .T., .F.)
    Private oFontCabN := TFont():New(cNomeFont, 9, -9,  .T., .T., 5, .T., 5, .T., .F.)

    Private nLinCab     := 025
    Private nLinCabOrig := 0
    Private cCodBar     := ""

    DbSelectArea('SA1')
    SA1->(DbSetOrder(1))
    SA1->(DbGoTop())
    DbSelectArea('SB1')
    SB1->(DbSetOrder(1))
    SB1->(DbGoTop())

    oPrintPvt := FWMSPrinter():New(cNomeRel, IMP_PDF, .F., /*cStartPath*/, .T., , @oPrintPvt, , , , , .T.)

    oPrintPvt:cPathPDF := GetTempPath()
    oPrintPvt:SetResolution(72)
    oPrintPvt:SetPortrait()
    oPrintPvt:SetPaperSize(DMPAPER_A4)
    oPrintPvt:SetMargin(60, 60, 60, 60)

    cQryPed := " SELECT  "
    cQryPed += "    DAK_CAMINH, "
    cQryPed += "    DAK_COD, "
    cQryPed += "    C5_FILIAL, "
    cQryPed += "    C5_NUM, "
    cQryPed += "    C5_NOTA, "
    cQryPed += "    DAK_DATA, "
    cQryPed += "    C5_CLIENTE, "
    cQryPed += "    C5_LOJACLI, "
    cQryPed += "    ISNULL(A1_NOME, '') AS A1_NOME, "
    cQryPed += "    ISNULL(A1_NREDUZ, '') AS A1_NREDUZ, "
    cQryPed += "    ISNULL(A1_PESSOA, '') AS A1_PESSOA, "
    cQryPed += "    ISNULL(A1_CGC, '') AS A1_CGC, "
    cQryPed += "    ISNULL(A1_INSCR, '') AS A1_INSCR, "
    cQryPed += "    ISNULL(A1_PFISICA, '') AS A1_PFISICA, "
    cQryPed += "    C5_CONDPAG, "
    cQryPed += "    ISNULL(E4_DESCRI, '') AS E4_DESCRI, "
    cQryPed += "    C5_VEND1, "
    cQryPed += "    A1_END, "
    cQryPed += "    A1_BAIRRO, "
    cQryPed += "    A1_EST, "
    cQryPed += "    A1_MUN, "
    cQryPed += "    A1_DDD, "
    cQryPed += "    A1_TEL, "
    cQryPed += "    A1_CONTATO, "
    cQryPed += "    A1_EMAIL "
    cQryPed += " FROM "
    cQryPed += "    "+RetSQLName('DAK')+" DAK "
    cQryPed += "    INNER JOIN " + RetSQLName("SC9") + " SC9 ON ( "
    cQryPed += "        C9_FILIAL = '" + FWxFilial("SC9") + "' "
    cQryPed += "        AND C9_CARGA = DAK.DAK_COD "
    cQryPed += "        AND C9_SEQCAR = DAK.DAK_SEQCAR "
    cQryPed += "        AND SC9.D_E_L_E_T_ = ' ' " 
    cQryPed += "    ) "
    cQryPed += "    INNER JOIN " + RetSQLName("SC5") + " SC5 ON ( "
    cQryPed += "        C5_FILIAL = '" + FWxFilial("SC5") + "' "
    cQryPed += "        AND SC5.D_E_L_E_T_ = ' ' "
    cQryPed += "        AND SC5.C5_NUM = SC9.C9_PEDIDO "
    cQryPed += "    )"
    cQryPed += "    INNER JOIN "+RetSQLName('SE4')+" SE4 ON ( "
    cQryPed += "        E4_FILIAL = '"+FWxFilial('SE4')+"' "
    cQryPed += "        AND E4_CODIGO = SC5.C5_CONDPAG "
    cQryPed += "        AND SE4.D_E_L_E_T_ = ' ' "
    cQryPed += "    ) "
    cQryPed += "    INNER JOIN "+RetSQLName('SA1')+" SA1 ON ( "
    cQryPed += "        A1_FILIAL = '"+FWxFilial('SA1')+"' "
    cQryPed += "        AND A1_COD = SC5.C5_CLIENTE "
    cQryPed += "        AND A1_LOJA = SC5.C5_LOJACLI "
    cQryPed += "        AND SA1.D_E_L_E_T_ = ' ' "
    cQryPed += "    ) "
    cQryPed += " WHERE "
    cQryPed += "    DAK_FILIAL = '" + FWxFilial("DAK") +"' "
    cQryPed += " AND DAK_COD BETWEEN  '" + cCarDe + "' AND '" + cCarAt + "' "
    cQryPed += " AND DAK_DATA BETWEEN '" + dTOS(dDatDe) + "' AND '" + dToS(dDatAte) + "' "
    cQryPed += " AND DAK.D_E_L_E_T_ = ' ' " 
    cQryPed += " GROUP BY DAK_CAMINH, "
    cQryPed += "    DAK_COD, "
    cQryPed += "    C5_FILIAL, "
    cQryPed += "    C5_NUM, "
    cQryPed += "    C5_NOTA, "
    cQryPed += "    DAK_DATA, "
    cQryPed += "    C5_CLIENTE, "
    cQryPed += "    C5_LOJACLI, "
    cQryPed += "    ISNULL(A1_NOME, ''), "
    cQryPed += "    ISNULL(A1_NREDUZ, '') , "
    cQryPed += "    ISNULL(A1_PESSOA, '') , "
    cQryPed += "    ISNULL(A1_CGC, '') , "
    cQryPed += "    ISNULL(A1_INSCR, '') , "
    cQryPed += "    ISNULL(A1_PFISICA, ''), "
    cQryPed += "    C5_CONDPAG, "
    cQryPed += "    ISNULL(E4_DESCRI, ''), "
    cQryPed += "    C5_VEND1, "
    cQryPed += "    A1_END, "
    cQryPed += "    A1_BAIRRO, "
    cQryPed += "    A1_EST, "
    cQryPed += "    A1_MUN, "
    cQryPed += "    A1_DDD, "
    cQryPed += "    A1_TEL, "
    cQryPed += "    A1_CONTATO, "
    cQryPed += "    A1_EMAIL "
    cQryPed += "    ORDER BY C5_NUM "
    cQryPed := ChangeQuery(cQryPed)
    TCQuery cQryPed New Alias "QRY_DAD"
    //MemoWrite("C:\cob\query_sql1.txt", cQryPed)
    TCSetField("QRY_DAD", "DAK_DATA", "D")
    
    Count To nTotCar
    ProcRegua(nTotCar)
    
    QRY_DAD->(DbGoTop())
    
    If nTotCar != 0
        nLinCab := fImpCab()
        nPagAtu := 1
        While ! QRY_DAD->(EoF())
            nCarAtu++
            IncProc("Processando carga "+cValToChar(nCarAtu)+" de "+cValToChar(nTotCar)+"...")

            If nLinCab+180 >= nLinFin
                fImpRod()
                nLinCab := 025
                nLinCab := fImpCab()
            EndIf
            //Dados do Cliente
            oPrintPvt:Box(nLinCab, nColIni, nLinCab+65, nColFin)
            nLinCab += 5
            oPrintPvt:SayAlign(nLinCab, nColIni+5,  "Cliente:", oFontCabN, 060, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab-2, nColIni+34, /*QRY_DAD->C5_CLIENTE+"/"+QRY_DAD->C5_LOJACLI+" - "+*/alltrim(QRY_DAD->A1_NREDUZ), oFontDet3, 320, 05, , PAD_LEFT,  )

            oPrintPvt:SayAlign(nLinCab, nColIni+300,  "CNPJ:", oFontCabN, 060, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab, nColIni+340, Alltrim(Transform(QRY_DAD->A1_CGC,"@R 99.999.999/9999-99")) , oFontCab, 120, 05, , PAD_LEFT,  )
            nLinCab += 10
            oPrintPvt:SayAlign(nLinCab, nColIni+5 , "Endereço:", oFontCabN, 060, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab, nColIni+48 , alltrim(QRY_DAD->A1_END) + "    -    " + alltrim(QRY_DAD->A1_BAIRRO), oFontCab, 250, 05, , PAD_LEFT,  )
            nLinCab += 10
            oPrintPvt:SayAlign(nLinCab, nColIni+5 , "Cidade:", oFontCabN, 060, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab, nColIni+48 , alltrim(QRY_DAD->A1_MUN) +" - "+ QRY_DAD->A1_EST, oFontCab, 200, 05, , PAD_LEFT,  )

            oPrintPvt:SayAlign(nLinCab, POS_BANCADA-5, "_______________________",  oFontCabN, 200, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab+10, POS_BANCADA+10, "     Conferente      ",  oFontCabN, 200, 05, , PAD_LEFT,  )
            nLinCab += 10

            oPrintPvt:SayAlign(nLinCab, nColIni+5, "Pedido:", oFontTit, 060, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab, nColIni+60, QRY_DAD->C5_NUM, oFontTit, 200, 05, , PAD_LEFT,  )
            cCodBar := QRY_DAD->C5_NUM
            oPrintPvt:Code128C(nLinCab+32,nColIni+60, cCodBar, 20.35)

            oPrintPvt:SayAlign(nLinCab, nColIni+200, "Nota Fiscal: " + alltrim(QRY_DAD->C5_NOTA), oFontTit, 300, 05, , PAD_CENTER,  )
            
            nLinCab += 10

            nLinCab += 7

            nLinCab += 10
            oPrintPvt:Box(nLinCab, nColIni, nLinCab + nTamFundo, nColFin)
            nLinCab += nTamFundo - 5
            oPrintPvt:SayAlign(nLinCab-10, nColIni, "Detalhamento do Pedido", oFontTit, nColFin-nColIni, nTamFundo, nCorAzul, PAD_CENTER, )
            nLinCab += 10

            oPrintPvt:SayAlign(nLinCab, POS_QUAN-25, "Qtd.Aux",    oFontDetN, 030, 05, , PAD_LEFT, )
           // oPrintPvt:SayAlign(nLinCab, POS_QUAN+5, "UM",    oFontDetN, 030, 05, , PAD_LEFT, )
            oPrintPvt:SayAlign(nLinCab, POS_QUAN+35, "Quant.",    oFontDetN, 030, 05, , PAD_LEFT, )
            oPrintPvt:SayAlign(nLinCab, POS_QUAN+65, "UM",    oFontDetN, 030, 05, , PAD_LEFT, )
            oPrintPvt:SayAlign(nLinCab, POS_COD+55,  "Cód.Prod.", oFontDetN, 040, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab, POS_DESC+65, "Descrição", oFontDetN, 200, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab, POS_CAIXA+20, "Ok?",    oFontDetN, 030, 05, , PAD_RIGHT, )
            nLinCab += 15
            oPrintPvt:Line(nLinCab,   nColIni, nLinCab,   nColFin)

            nLinAtu := nLinCab + 5

            SA1->(DbSeek(FWxFilial('SA1') + QRY_DAD->C5_CLIENTE + QRY_DAD->C5_LOJACLI))

            cQryIte := " SELECT "
            cQryIte += "    C6_ITEM, "
            cQryIte += "    C6_PRODUTO, "
            cQryIte += "    ISNULL(B1_DESC, '') AS B1_DESC, "
            cQryIte += "    C6_UM, "
            cQryIte += "    C6_SEGUM, "
            cQryIte += "    C6_TES, "
            cQryIte += "    C6_X_QTAUX, "
            cQryIte += "    C6_QTDVEN, "
            cQryIte += "    C6_QTDENT,"
            cQryIte += "    C6_LOTECTL, "
            cQryIte += "    C6_ENDPAD "
            cQryIte += " FROM "
            cQryIte += "    "+RetSQLName('SC6')+" SC6 "
            cQryIte += "    INNER JOIN "+RetSQLName('SB1')+" SB1 ON ( "
            cQryIte += "         B1_COD = SC6.C6_PRODUTO "
            cQryIte += "        AND SB1.D_E_L_E_T_ = ' ' "
            cQryIte += "    ) "
            cQryIte += " WHERE "
            cQryIte += "    C6_FILIAL = '"+FWxFilial('SC6')+"' "
            cQryIte += "    AND C6_NUM = '"+QRY_DAD->C5_NUM+"' "
            cQryIte += "    AND SC6.D_E_L_E_T_ = ' ' "
            cQryIte += " GROUP BY C6_PRODUTO, B1_DESC, C6_UM, C6_TES,C6_X_QTAUX, C6_QTDVEN, C6_LOTECTL, C6_ENDPAD, C6_ITEM, C6_QTDENT,C6_SEGUM   "
            cQryIte += " ORDER BY "
            cQryIte += "    C6_ITEM,C6_PRODUTO "
            cQryIte := ChangeQuery(cQryIte)
            TCQuery cQryIte New Alias "QRY_ITE"
            //MemoWrite("C:\cob\query_item.txt", cQryIte)
            nValorTot := 0
            nTValIPI := 0
            nTValICMST := 0
            Count to nToIt
            QRY_ITE->(dbGoTop())
            If nLinAtu+(nToIt*50) >= nLinFin
                    fImpRod()
                    nLinCab := 025
                    nLinCab := fImpCab()
                    nLinAtu := nLinCab
            EndIf

            While ! QRY_ITE->(EoF())
                SB1->(DbSeek(FWxFilial("SB1")+QRY_ITE->C6_PRODUTO))

                oPrintPvt:SayAlign(nLinAtu, POS_QUAN-25, Alltrim(Transform(QRY_ITE->C6_X_QTAUX, "@E 999,999.99")),   oFontDet3, 040, 05, , PAD_LEFT, )
               // oPrintPvt:SayAlign(nLinAtu, POS_QUAN+5,  QRY_ITE->C6_SEGUM, oFontDet3, 070, 05, , PAD_LEFT,  )
                oPrintPvt:SayAlign(nLinAtu, POS_QUAN +35, Alltrim(Transform(QRY_ITE->C6_QTDVEN, "@E 999,999.99")),   oFontDet3, 040, 05, , PAD_LEFT, )
                oPrintPvt:SayAlign(nLinAtu, POS_QUAN+65,  QRY_ITE->C6_UM, oFontDet3, 070, 05, , PAD_LEFT,  )
                oPrintPvt:SayAlign(nLinAtu, POS_COD+55,  QRY_ITE->C6_PRODUTO, oFontDet3, 070, 05, , PAD_LEFT,  )

                cDescPrd := Alltrim(QRY_ITE->B1_DESC) 
            
                oPrintPvt:SayAlign(nLinAtu, POS_DESC+65, substr(cDescPrd,1,80), oFontDet3, 200, 05, , PAD_LEFT,  )
                If len(cDescPrd) > 80
                    nLinAtu += 12
                    oPrintPvt:SayAlign(nLinAtu, POS_DESC+65, substr(cDescPrd,81), oFontDet3, 200, 05, , PAD_LEFT,  )
                    nLinAtu -= 12
                Endif
                oPrintPvt:SayAlign(nLinAtu, POS_CAIXA+20, "________________",  oFontDet3, 200, 05, , PAD_LEFT,  )
                If len(cDescPrd) > 80
                    nLinAtu += 15
                Endif
                nLinAtu += 15
               
                QRY_ITE->(DbSkip())
            EndDo
            QRY_ITE->(DbCloseArea())

           // nLinAtu += 5
           // oPrintPvt:Line(nLinAtu,   nColIni, nLinAtu,   nColFin)
            nLinAtu += 10

            oPrintPvt:SayAlign(nLinAtu, nColIni, "Expediçao  :________________________________________", oFontDet3, 250, 05, , PAD_LEFT, )
            oPrintPvt:SayAlign(nLinAtu, nColMeio,"Volumes :________________________________________", oFontDet3, 250, 05, , PAD_LEFT, )
            nLinAtu += 15
            oPrintPvt:SayAlign(nLinAtu, nColIni, "Conferencia:________________________________________", oFontDet3, 250, 05, , PAD_LEFT, )
            oPrintPvt:SayAlign(nLinAtu, nColMeio,"Peso        :________________________________________", oFontDet3, 250, 05, , PAD_LEFT, )
            nLinAtu += 30

            nLinCab := nLinAtu
            If nLinAtu >= nLinFin
                fImpRod()
                nLinCab := 025
                nLinCab := fImpCab()
                nLinAtu := nLinCab
            EndIf
            QRY_DAD->(DbSkip())
        EndDo
        QRY_DAD->(DbCloseArea())

        fImpRod()

        cQryCar := " SELECT "
        cQryCar += "    DAK_CAMINH, "
        cQryCar += "    DAK_COD, "
        cQryCar += "    DAK_DATA, "
        cQryCar += "    C6_PRODUTO, 
        cQryCar += "    B1_DESC AS B1_DESC, "
        cQryCar += "    SUM(C6_X_QTAUX) QTDAUX, "
        cQryCar += "    C6_SEGUM, "
        cQryCar += "    SUM(C6_QTDVEN) QTDVEN, "
        cQryCar += "    C6_UM "
        cQryCar += " FROM "
        cQryCar += "    " + RetSQLName("DAK")+ " DAK "
        cQryCar += "INNER JOIN " + RetSQLName("SC9") + " SC9 ON ( "
        cQryCar += "    C9_FILIAL = '" +xFilial("SC9") + "' "
        cQryCar += "    AND C9_CARGA = DAK.DAK_COD "
        cQryCar += "    AND C9_SEQCAR = DAK.DAK_SEQCAR "
        cQryCar += "    AND SC9.D_E_L_E_T_ = ' ' "
        cQryCar += ") "
        cQryCar += " INNER JOIN " + RetSQLName("SB1") + " SB1 ON ( "
        cQryCar += "    B1_FILIAL  = '" +xFilial("SB1") + "' "
        cQryCar += "    AND B1_COD = C9_PRODUTO "
        cQryCar += "    AND SB1.D_E_L_E_T_ = ' ' "
        cQryCar += ") "
        cQryCar += "INNER JOIN " + RetSQLName("SC6") + " SC6 ON( "
        cQryCar += "    C6_FILIAL = '" +xFilial("SC6") + "' "
        cQryCar += "    AND C6_PRODUTO = SC9.C9_PRODUTO "
        cQryCar += "    AND C6_ITEM = SC9.C9_ITEM "
        cQryCar += "    AND C6_NUM = SC9.C9_PEDIDO "
        cQryCar += "    AND SC6.D_E_L_E_T_ = ' ' "
        cQryCar += " ) "
        cQryCar += "    WHERE "
        cQryCar += "       DAK_FILIAL = '" +xFilial("DAK") + "' "
        cQryCar += "       AND DAK_COD BETWEEN '" + cCarDe + "' AND '" + cCarAt + "' "
        cQryCar += "       AND DAK_DATA BETWEEN '" + dTOS(dDatDe) + "' AND '" + dToS(dDatAte) + "' "
        cQryCar += "       AND DAK.D_E_L_E_T_ = ' ' "
        cQryCar += "     GROUP BY DAK_CAMINH, " 
        cQryCar += "                DAK_COD, "
        cQryCar += "                DAK_DATA, "
        cQryCar += "                C6_PRODUTO, "
        cQryCar += "                B1_DESC, "
        cQryCar += "                C6_SEGUM, "
        cQryCar += "                C6_UM"
        cQryCar += "    ORDER BY DAK_CAMINH,DAK_COD,C6_PRODUTO,B1_DESC "
        TCQuery cQryCar new Alias "_CAR"
        DbSelectArea("_CAR")
        dbGoTop()

        nLinCab := 060
        //nLinCab := fImpCab()
        oPrintPvt:StartPage()
                
        oPrintPvt:SayAlign(nLinCab-10, nColIni, "Total das Cargas", oFontTit, nColFin-nColIni, nTamFundo, nCorAzul, PAD_CENTER, )
        nLinCab += 10
        oPrintPvt:Line(nLinCab,   nColIni, nLinCab,   nColFin)
        nLinCab += 15
        cCarga := ""
        while(!eof())
            if cCarga <> _CAR->DAK_COD        
                nLinCab += 15
                oPrintPvt:SayAlign(nLinCab, nColIni+5 , "Placa: " + _CAR->DAK_CAMINH,    oFontTit, 150, 05, , PAD_LEFT, )
                oPrintPvt:SayAlign(nLinCab, nColIni+200, "Carga: " + _CAR->DAK_COD,    oFontTit, 150, 05, , PAD_LEFT, )
                nLinCab += 15
                cCarga := _CAR->DAK_COD   

                oPrintPvt:SayAlign(nLinCab, 10+POS_QUAN-25, "Qtd.Aux",    oFontDetN, 030, 05, , PAD_LEFT, )
                //oPrintPvt:SayAlign(nLinCab, 20+POS_QUAN+5, "UM",    oFontDetN, 030, 05, , PAD_LEFT, )
                oPrintPvt:SayAlign(nLinCab, 20+POS_QUAN+35, "Quant.",    oFontDetN, 030, 05, , PAD_LEFT, )
                oPrintPvt:SayAlign(nLinCab, 25+POS_QUAN+65, "UM",    oFontDetN, 030, 05, , PAD_LEFT, )
                oPrintPvt:SayAlign(nLinCab, 20+POS_COD+55,  "Cód.Prod.", oFontDetN, 040, 05, , PAD_LEFT,  )
                oPrintPvt:SayAlign(nLinCab, 20+POS_DESC+65, "Descrição", oFontDetN, 200, 05, , PAD_LEFT,  )
                oPrintPvt:SayAlign(nLinCab, 20+POS_CAIXA+20, "Ok?",    oFontDetN, 030, 05, , PAD_RIGHT, )
                nLinCab += 10
                oPrintPvt:Line(nLinCab,   nColIni, nLinCab,   nColFin)
            EndIf

            oPrintPvt:SayAlign(nLinCab, 10+POS_QUAN-25, Alltrim(Transform(_CAR->QTDAUX, "@E 999,999.99")),   oFontDet3, 040, 05, , PAD_LEFT, )
           // oPrintPvt:SayAlign(nLinCab, 20+POS_QUAN+5,  _CAR->C6_SEGUM, oFontDet3, 070, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab, 20+POS_QUAN +35, Alltrim(Transform(_CAR->QTDVEN, "@E 999,999.99")),   oFontDet3, 040, 05, , PAD_LEFT, )
            oPrintPvt:SayAlign(nLinCab, 25+POS_QUAN+65,  _CAR->C6_UM, oFontDet3, 070, 05, , PAD_LEFT,  )
            oPrintPvt:SayAlign(nLinCab, 20+POS_COD+55,  _CAR->C6_PRODUTO, oFontDet3, 070, 05, , PAD_LEFT,  )

            cDescPrd := Alltrim(_CAR->B1_DESC) 
        
            oPrintPvt:SayAlign(nLinCab, 20+POS_DESC+65, substr(cDescPrd,1,80), oFontDet3, 200, 05, , PAD_LEFT,  )
            If len(cDescPrd) > 80
                nLinCab += 12
                oPrintPvt:SayAlign(nLinCab, 20+POS_DESC+65, substr(cDescPrd,81), oFontDet3, 200, 05, , PAD_LEFT,  )
                nLinCab -= 12
            Endif
            oPrintPvt:SayAlign(nLinCab, POS_CAIXA+20, "________________",  oFontDet3, 200, 05, , PAD_LEFT,  )
            If len(cDescPrd) > 80
                nLinCab += 15
            Endif
            nLinCab += 15

            If nLinCab >= nLinFin
                fImpRod()
                nLinCab := 025
                oPrintPvt:StartPage()
            EndIf

            _CAR->(dbSkip())
        endDo
        _CAR->(dbCloseArea())
        
        fImpRod()
        oPrintPvt:Preview()
    Else
        QRY_DAD->(DbCloseArea())
        MsgStop("Não há Cargas!", "Atenção")
    EndIf

Return

/*---------------------------------------------------------------------*
 | Func:  fImpCab                                                      |
 | Desc:  Função que imprime o cabeçalho                               |
 *---------------------------------------------------------------------*/

Static Function fImpCab()

    //Dados da empresa
    Local cEmpresa    := Iif(Empty(SM0->M0_NOMECOM), Alltrim(SM0->M0_NOME), Alltrim(SM0->M0_NOMECOM))
    Local cEmpTel     := "(14) 3602-7100"//Alltrim(Transform(SubStr(SM0->M0_TEL,3,Len(SM0->M0_TEL)),"@R (99) 9999-9999"))
    Local cEmpCidade  := AllTrim(SM0->M0_CIDENT)+" / "+SM0->M0_ESTENT
    Local cEmpCnpj    := Alltrim(Transform(SM0->M0_CGC,"@R 99.999.999/9999-99"))
    Local cEmpCep     := Alltrim(Transform(SM0->M0_CEPENT,"@R 99999-999"))
    Local nLinCabOrig := nLinCab
    Local nLinBk      := nLinCab

    if len(alltrim(SM0->M0_CGC)) < 14
        cEmpresa    := 'IND. E COM. DE LATICINOS'
        cEmpTel     := "(14) 3602-7100"//Alltrim(Transform(SubStr(SM0->M0_TEL,3,Len(SM0->M0_TEL)),"@R (99) 9999-9999"))
        cEmpCidade  := 'LUTECIA/SP'
    endIf

    //Iniciando Página
    oPrintPvt:StartPage()

    //Dados da Empresa
    oPrintPvt:Box(nLinCab, nColIni,    nLinCab + 075, nColMeio+27)
    //oPrintPvt:Line(nLinCab+nTamFundo,   nColIni, nLinCab+nTamFundo,   nColMeio-30)
    nLinCab += nTamFundo - 5
    //oPrintPvt:SayAlign(nLinCab-10, nColIni+5, "Empresa", oFontTit, 060, 05, nCorAzul, PAD_LEFT,  )
    //nLinCab += 5
    oPrintPvt:SayBitmap(nLinCab+10, nColIni+5, cLogoEmp, 054, 031)
    nLinCab -= 5
    oPrintPvt:SayAlign(nLinCab, nColIni+65,  "Empresa:", oFontCabN, 060, 05, , PAD_LEFT,  )
    oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpresa, oFontCab, 320, 05, , PAD_LEFT,  )
    nLinCab += 9
    oPrintPvt:SayAlign(nLinCab, nColIni+65,  "CNPJ:", oFontCabN, 060, 05, , PAD_LEFT,  )
    oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpCnpj, oFontCab, 120, 05, , PAD_LEFT,  )
    nLinCab += 9
    oPrintPvt:SayAlign(nLinCab, nColIni+65,  "Cidade:", oFontCabN, 060, 05, , PAD_LEFT,  )
    oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpCidade, oFontCab, 250, 05, , PAD_LEFT,  )
    nLinCab += 9
    oPrintPvt:SayAlign(nLinCab, nColIni+65,  "CEP:", oFontCabN, 060, 05, , PAD_LEFT,  )
    oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpCep, oFontCab, 120, 05, , PAD_LEFT,  )
    nLinCab += 9
    oPrintPvt:SayAlign(nLinCab, nColIni+65,  "Telefone:", oFontCabN, 060, 05, , PAD_LEFT,  )
    oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpTel, oFontCab, 120, 05, , PAD_LEFT,  )
    nLinCab += 9
    oPrintPvt:SayAlign(nLinCab, nColIni+65,  "E-mail:", oFontCabN, 060, 05, , PAD_LEFT,  )
    oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpEmail, oFontCab, 120, 05, , PAD_LEFT,  )
    nLinCab += 9
    oPrintPvt:SayAlign(nLinCab, nColIni+65,  "Home Page:", oFontCabN, 060, 05, , PAD_LEFT,  )
    oPrintPvt:SayAlign(nLinCab, nColIni+110,  cEmpSite, oFontCab, 120, 05, , PAD_LEFT,  )
    nLinCab += 7

    nLinBk := nLinCab
    //Dados do Carga
    nLinCab := nLinCabOrig
    oPrintPvt:Box(nLinCab, nColMeio+30, nLinCab + 075, nColFin)
    oPrintPvt:Line(nLinCab+nTamFundo,   nColMeio+30, nLinCab+nTamFundo,   nColFin)
    nLinCab += nTamFundo - 5

    oPrintPvt:SayAlign(nLinCab-8, nColMeio+38, "ORDEM CARREGAMENTO" , oFontTit, 400, 05, nCorAzul, PAD_LEFT,  )

    nLinCab += nTamFundo
    oPrintPvt:SayAlign(nLinCab-5, nColMeio+38, "Carga: " + alltrim(QRY_DAD->DAK_COD) , oFontTit, 400, 05, nCorAzul, PAD_LEFT,  )
    nLinCab += nTamFundo

    oPrintPvt:SayAlign(nLinCab, nColMeio+38, "Emissão: " + dToC(QRY_DAD->DAK_DATA) , oFontTit, 400, 05, nCorAzul, PAD_LEFT,  )
    nLinCab += nTamFundo
    oPrintPvt:SayAlign(nLinCab+5, nColMeio+38, "Placa: " + QRY_DAD->DAK_CAMINH , oFontTit, 400, 05, nCorAzul, PAD_LEFT,  )

    //Código de barras
    nLinCab := nLinCabOrig

    cCodBar := QRY_DAD->DAK_COD

    //cCodBar := QRY_DAD->C5_NUM //QRY_DAD->C5_FILIAL+QRY_DAD->C5_NUM
    oPrintPvt:Code128C(nLinCab+38+nTamFundo, nColFin-80, cCodBar, 28)
    oPrintPvt:SayAlign(nLinCab+40+nTamFundo, nColFin-100, cCodBar, oFontRod, 080, 05, , PAD_CENTER,  )
    //oPrintPvt:SayAlign(nLinCab+40+nTamFundo, nColFin-80, cCodBar, oFontRod, 080, 05, , PAD_CENTER,  )
    
    nLinCab := nLinBk+20

Return nLinCab

/*---------------------------------------------------------------------*
 | Func:  fImpRod                                                      |
 | Desc:  Função que imprime o rodapé                                  |
 *---------------------------------------------------------------------*/

Static Function fImpRod()
Local nLinRod:= nLinFin + 10
Local cTexto := ''

//Linha Separatória
oPrintPvt:Line(nLinRod,   nColIni, nLinRod,   nColFin)
nLinRod += 3

//Dados da Esquerda
cTexto := dToC(dDataBase)+"     "+cHoraEx+"     "+FunName()+"     "+cUserName
oPrintPvt:SayAlign(nLinRod, nColIni, cTexto, oFontRod, 250, 05, , PAD_LEFT, )

//Direita
cTexto := "Página "+cValToChar(nPagAtu)
oPrintPvt:SayAlign(nLinRod, nColFin-40, cTexto, oFontRod, 040, 05, , PAD_RIGHT, )

//Finalizando a página e somando mais um
oPrintPvt:EndPage()
nPagAtu++
Return

/*---------------------------------------------------------------------*
 | Func:  fLogoEmp                                                     |
 | Desc:  Função que retorna o logo da empresa (igual a DANFE)         |
 *---------------------------------------------------------------------*/

Static Function fLogoEmp()
	Local cGrpCompany	:= AllTrim(FWGrpCompany())
	Local cCodEmpGrp	:= AllTrim(FWCodEmp())
	Local cUnitGrp		:= AllTrim(FWUnitBusiness())
	Local cFilGrp		:= AllTrim(FWFilial())
	Local cLogo			:= ""
	Local cCamFim		:= GetTempPath()
	Local cStart		:= GetSrvProfString("Startpath","")

	//Se tiver filiais por grupo de empresas
	If !Empty(cUnitGrp)
		cDescLogo := cGrpCompany + cCodEmpGrp + cUnitGrp + cFilGrp
		
	//Senão, será apenas, empresa + filial
	Else
		cDescLogo	:= cEmpAnt + cFilAnt
	EndIf
	
	//Pega a imagem
	//cLogo := GetSrvProfString("Startpath","") + "lgrl" + cDescLogo + ".BMP" //"logo_pv.bmp" //cStart + "DANFE" + cDescLogo + ".BMP"
	cLogo	:= cStart + "DANFE" + cDescLogo + ".BMP"
	
	//Se o arquivo não existir, pega apenas o da empresa, desconsiderando a filial
	If !File(cLogo)
		cLogo	:= cStart + "DANFE" + cDescLogo + ".BMP"
	EndIf
	
	//Copia para a temporária do s.o.
	CpyS2T(cLogo, cCamFim)
	cLogo := cCamFim + StrTran(cLogo, cStart, '')
	
	//Se o arquivo não existir na temporária, espera meio segundo para terminar a cópia
	If !File(cLogo)
		Sleep(500)
	EndIf
Return cLogo
