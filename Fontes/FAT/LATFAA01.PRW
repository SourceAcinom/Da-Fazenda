#include "RWMAKE.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "Report.ch"
#INCLUDE "TOTVS.CH"
#define MB_OK                       0
#define MB_OKCANCEL                 1
#define MB_YESNO                    4
#define MB_ICONHAND                 16
#define MB_ICONQUESTION             32
#define MB_ICONEXCLAMATION          48
#define MB_ICONASTERISK             64

User Function LATFATA01  ()
	Local aArea	:= GetArea()
	Private cCadastro:= "Pesagem de Produtos"
	Private oDlg
	Private oOk  := LoadBitmap( GetResources(), "LBOK" )
	Private oNo  := LoadBitmap( GetResources(), "LBNO" )
	Private nCell     := 10   /// posicao da celula que é permitido alterar
	Private aItens    := {}
	
	TelaLA()

	RestArea( aArea )

Return


Static Function TelaLA()
	
	Local aSize
	Local bOk := {||}
	Local bCancel := {||}
	Local nLin := 00
	Local nCol := 00
	Private	aMatTit:= {}
	Private oLbxIte
	Private oGrpPed
	Private oSayEmis
	Private oEmis
	Private oSayDCar
	Private oCli
	Private oGrpIte
	Private oSayProd
	Private oProd
	Private oSayDesc
	Private oDesc
	Private oSayPes
	Private oPeso
	Private oGrpTIte
	Private oTotGer
	Private oTotSel
	Private oNValor
	Private oSayTotGer
	Private oSayTotSel
	Private oSayNValor
	Private oCheck_Vl
	Private oSayQtdAux
	Private oQtdAud
	Private lCheck_Vl := .f.
	Private cCheck_Vl := "Gravar Novo Valor ?"
	Private nTotGer := 0
	Private nTotSel := 0
	Private dEmiss := cToD("  /   /  ")
	Private cDscCarga := ""
	Private cProd    := ""
	Private cDescr   := ""
	Private nPeso    := 0
	Private nQtdAx   := 0
	Private nTotPes  := 0
	Private cCarga  := space(6)
	Private oPedido
	

	///                   nome   tam alt bold itali  under
	oFont10n := TFont():New("VERDANA", 10, 10, .T., .F., , , , .T., .F.)
	oFont14  := TFont():New("VERDANA", 14, 14,    , .F., , , , .T., .F.)
	oFont14n := TFont():New("VERDANA", 14, 14, .T., .F., , , , .T., .F.)
	oFont16  := TFont():New("VERDANA", 16, 16,    , .F., , , , .T., .F.)
	oFont16N := TFont():New("VERDANA", 16, 16, .T., .T., , , , .T., .F.)
	oFont24N := TFont():New("VERDANA", 24, 24, .T., .T., , , , .T., .F.)

	/*DEFINE MSDIALOG oDlg TITLE "Teste Foco" FROM 000, 000 TO 300, 500 COLORS 0, 16777215 PIXEL

 @ 038, 065 SAY oSay1 PROMPT "Código 1" SIZE 049, 007 OF oDlg COLORS 0, 16777215 PIXEL
 @ 048, 065 MSGET oGet1 VAR cGet1 SIZE 060, 010 OF oDlg COLORS 0, 16777215 PIXEL
 
 @ 075, 065 SAY oSay2 PROMPT "Código 2" SIZE 049, 007 OF oDlg COLORS 0, 16777215 PIXEL
 @ 085, 065 MSGET oGet2 VAR cGet2 SIZE 060, 010 OF oDlg COLORS 0, 16777215 PIXEL 
 
 SetKey(VK_F12, {|| oGet2:SetFocus() }) //Foco no código 2

ACTIVATE MSDIALOG oDlg CENTERED
*/

	//Processa({|| RunProc()})

	//aSize := MsAdvSize(.F.)
	
	//DEFINE MSDIALOG oDlg TITLE cCadastro STYLE DS_MODALFRAME  From aSize[7],0 To aSize[6],aSize[5]   OF oMainWnd PIXEL
/// linha  coluna
	@ 000,000 	to 600,1099 	dialog oDlg 	title cCadastro

	@ 003+nLin, 003+nCol GROUP oGrpPed 	TO 043+nLin, 450+nCol 	PROMPT "Pedido " OF oDlg COLOR 0, 16777215 PIXEL

	@ 015+nLin,012+nCol MSGET  oPedido VAR cCarga   VALID   vldPed()   SIZE 060,20    FONT oFont24N PICTURE "999999" OF oDlg COLOR 0, 16777215 PIXEL

	@ 015+nLin,080+nCol SAY oSayEmis PROMPT dEmiss SIZE 100,030 FONT oFont14n OF oDlg COLOR 0, 16777215 PIXEL

	@ 027+nLin,080+nCol SAY oSayDCar PROMPT cDscCarga SIZE 350,030 FONT oFont14n OF oDlg COLOR 0, 16777215 PIXEL

	@ 047+nLin, 003+nCol GROUP oGrpIte TO 100+nLin, 450+nCol PROMPT "Item " OF oDlg COLOR 0, 16777215 PIXEL

	@ 060+nLin,012+nCol SAY oSayProd PROMPT "Produto" SIZE 060,020 OF oDlg COLOR 0, 16777215 PIXEL
	@ 070+nLin,012+nCol MSGET oProd VAR cProd WHEN .F. SIZE 060,020 FONT oFont16n PICTURE "@!" OF oDlg COLOR 0, 16777215 PIXEL

	@ 060+nLin,080+nCol SAY oSayDesc PROMPT "Descrição" SIZE 100, 020 OF oDlg COLOR 0, 16777215 PIXEL
	@ 070+nLin,080+nCol MSGET oDesc VAR cDescr WHEN .F. SIZE 200, 020 FONT oFont16n PICTURE "@!" OF oDlg COLOR 0, 16777215 PIXEL

	@ 060+nLin,300+nCol SAY oSayQtdAux PROMPT "Qtd. Aux." SIZE 100, 020 OF oDlg COLOR 0, 16777215 PIXEL
	@ 070+nLin,300+nCol MSGET oQtdAud VAR nQtdAx  SIZE 060, 020 FONT oFont16n PICTURE "999,999" OF oDlg COLOR 0, 16777215 PIXEL

	@ 060+nLin,365+nCol SAY oSayPes PROMPT "Peso" SIZE 080, 020 OF oDlg COLOR 0, 16777215 PIXEL
	@ 070+nLin,365+nCol MSGET oPeso VAR nPeso SIZE 080, 020 FONT oFont16n PICTURE "@9 999,999.9999" OF oDlg COLOR 0, 16777215 PIXEL

	@ 105+nLin, 003+nCol GROUP oGrpTIte TO 230+nLin, 450+nCol PROMPT "Ítens do Pedido " OF oDlg COLOR 0, 16777215 PIXEL
	@ 200+nLin,380+nCol SAY oSayTPes PROMPT "Peso Total" SIZE 080, 030 FONT oFont16N OF oDlg COLOR 0, 16777215 PIXEL
	@ 210+nLin,355+nCol SAY oTotGer PROMPT nTotPes SIZE 120, 030 FONT oFont16N PICTURE "@9 99,999,999.9999" OF oDlg COLOR 0, 16777215 PIXEL

	//ListBox Com os Itens do pedido
	@ 115+nLin,005+nCol LISTBOX oLbxIte VAR cVar FIELDS HEADER " " SIZE 350,110 OF oDlg PIXEL ON dblClick(MarcaGrid(@oLbxIte,@aMatTit),oLbxIte:Refresh())
	oLbxIte:AddColumn(TCColumn():New(" "          , {|| If(aMatTit[oLbxIte:nAt,01],oOk,oNo)}, "@BMP"                       ,    ,    ,        , 1  , .T., .F.))
	oLbxIte:AddColumn(TcColumn():New("Produto"    , {|| aMatTit[oLbxIte:nAt][02]}           , PesqPict("SC6", "C6_PRODUTO"), nil, nil, "LEFT" , nil, .F., .F. , nil, nil, nil, .F., nil))
	oLbxIte:AddColumn(TcColumn():New("Descrição"  , {|| aMatTit[oLbxIte:nAt][03]}           , PesqPict("SC6", "C6_DESCRI") , nil, nil, "LEFT" , nil, .F., .F. , nil, nil, nil, .F., nil))
	oLbxIte:AddColumn(TcColumn():New("UM"         , {|| aMatTit[oLbxIte:nAt][04]}           , PesqPict("SC6", "C6_UM")     , nil, nil, "LEFT", nil, .F., .F. , nil, nil, nil, .F., nil))
	oLbxIte:AddColumn(TcColumn():New("Qtd. Aux."  , {|| aMatTit[oLbxIte:nAt][05]}           , PesqPict("SC6", "C6_X_QTAUX"), nil, nil, "RIGTH", nil, .F., .F. , nil, nil, nil, .F., nil))
	oLbxIte:AddColumn(TcColumn():New("Qtd. (Peso)", {|| aMatTit[oLbxIte:nAt][06]}           , PesqPict("SC6", "C6_QTDVEN") , nil, nil, "RIGTH", nil, .F., .F. , nil, nil, nil, .F., nil))

	oLbxIte:SetArray(aMatTit)
	oLbxIte:Refresh()

	@ 260,010 Button OemToAnsi("Marcar Tudo")			Size 48,14 Action (Marcar(.T.,oLbxIte)) 	Object btnMar
	@ 260,064 Button OemToAnsi("Desmarcar Tudo")		Size 48,14 Action (Marcar(.F.,oLbxIte)) 	Object btnDes

	@ 260,320 Button OemToAnsi("Alterar Valor") 		Size 40,12 Action (AltVlr(oDlg)) 	Object btnRel2

	oCheck_Vl := TCheckBox():New(265,220,cCheck_Vl,bSETGET(lCheck_Vl),oDlg,90,60,,,,,,,,.T.)

	@ 260,320 Button OemToAnsi("Alterar Vencto") 		Size 40,12 Action (AltVcto(oDlg)) Object btnRel2
	@ 260,380 Button OemToAnsi("Relatório")    			Size 40,12 Action (ImpRel())   		Object btnRel2
	@ 260,440 Button OemToAnsi("Confirmar")    			Size 40,12 Action (GravaConf(oDlg)) Object btnRel2
	@ 260,500 Button OemToAnsi("Voltar") 				Size 40,12 Action (Close(oDlg)) 	Object btnFechar2

	//oDlg:=lMaximized := .T.
	oDlg:Refresh()

	// Exibir formulario
	//Activate MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,Bok,bCancel) CENTERED
	Activate MSDIALOG oDlg  CENTERED

Return


Static Function Marcar(lMarca,oLbxIte)

	Local nInd := 0

	For nInd:=1 To Len(aMatTit)
		if aMatTit[nInd][1] <> lMarca
			if !aMatTit[nInd][1]
				//nTotSel := nTotSel + aMatTit[nInd][9]
			else
				//nTotSel := nTotSel - aMatTit[nInd][9]
			endIf
		endIf
		aMatTit[nInd][1]:= lMarca
	Next

	nNValor := 0
	For nInd:=1 To Len(aMatTit)
		if aMatTit[nInd][1] <> .f.
			if aMatTit[nInd][1]
				///nNValor := nNValor + aMatTit[nInd][10]
			endIf
		endIf
	Next

	oLbxIte:SetArray(aMatTit)
	oLbxIte:Refresh()

Return


Static Function MarcaGrid(oLbxIte,aMatTit)

	Local nInd := 0

	/*if oLbxIte:ColPos() <> nCell
		if aMatTit[oLbxIte:nAt][1]
			//nTotSel := nTotSel - aMatTit[oLbxIte:nAt][9]
		else
			//nTotSel := nTotSel + aMatTit[oLbxIte:nAt][9]
		endIf
		aMatTit[oLbxIte:nAt,1] := !aMatTit[oLbxIte:nAt,1]
	else
		lEditCell(@aMatTit,oLbxIte,"@9 9,999,999.9999",nCell,Nil,Nil,{|U| validP(&(__ReadVar)) })
	endIf

	nNValor := 0
	For nInd:=1 To Len(aMatTit)
		if aMatTit[nInd][1] <> .f.
			if aMatTit[nInd][1]
				nNValor := nNValor + aMatTit[nInd][10]
			endIf
		endIf
	Next*/

	oLbxIte:SetArray(aMatTit)
	oLbxIte:Refresh()
	oTotSel:Refresh()
	oNValor:Refresh()

Return

static function vldPed()

	Local cQuery := ""

	dbSelectArea("DAK")
	dbSetOrder(1)
	if dbSeek(xFilial("DAK")+cCarga)
		dEmiss := DAK->DAK_DATA
		oSayEmis:refresh()
		if DAK->DAK_ROTEIR == '999999'
			cDscCarga := "Sem roteiro"
		else
			dbSelectArea("DA8")
			dbSetOrder(1)
			if dbSeek(xFilial("DA8")+DAK->DAK_ROTEIR)
				cDscCarga := DA8->DA8_DESC
			endIf
		EndIf
		oSayDCar:refresh()
	endIf
	
    	cQuery := "SELECT "
	//cQuery += "	C6_NUM, "
      	//cQuery += "	C6_ITEM, "
      	cQuery += "	C6_PRODUTO, "
      	cQuery += "	C6_DESCRI, "
      	cQuery += "	C6_UM, "
      	cQuery += "	SUM(C6_X_QTAUX) QTDAUX, "
      	cQuery += "	SUM(C6_QTDVEN) QTDE "
  	cQuery += "		FROM "
      	cQuery += "			" + retSqlName("SC5") + " A "
      	cQuery += "INNER JOIN " 	  + retSqlName("SC6") + " B ON B.C6_FILIAL = A.C5_FILIAL  "
      	cQuery += "	AND B.C6_NUM = A.C5_NUM "
      	cQuery += "	AND B.D_E_L_E_T_ = ' ' "
	cQuery += "	AND B.C6_UM = 'KG' "  // SOMENTO PRODUTOS EM KG SERAO EXIBIDOS NA GRID
      	cQuery += "INNER JOIN " 	  + retSqlName("DAK") + " C ON C.DAK_FILIAL = '" + xFilial("DAK") + "' "
      	cQuery += "	AND DAK_COD = '" + strzero(val(cCarga),6) + "' "
	cQuery += "INNER JOIN "           + retSqlName("SC9") + " D ON D.C9_FILIAL = '" + xFilial("SC9") + "' "
      	cQuery += "	AND D.C9_PEDIDO = B.C6_NUM "
      	cQuery += "	AND D.C9_ITEM = B.C6_ITEM "
      	cQuery += "	AND D.C9_PRODUTO = B.C6_PRODUTO "
      	cQuery += "	AND D.C9_CARGA = C.DAK_COD "
      	cQuery += "	AND D.C9_SEQCAR = C.DAK_SEQCAR "
	cQuery += "	AND D.C9_X_PESOU <> 'S' "
  	cQuery += "WHERE "
      	cQuery += "	A.D_E_L_E_T_ = ' ' "
  	cQuery += "GROUP BY C6_PRODUTO, C6_DESCRI, C6_UM  "
	cQuery += "ORDER BY B.C6_PRODUTO "
	tcQuery cQuery new Alias "_PED"
	dbSelectArea("_PED")
	dbGoTop()


	aMatTit := {}
	while(!eof()) ////     1      2                      3           4            5          6      7
		aadd(aMatTit,{.F.,_PED->C6_PRODUTO,_PED->C6_DESCRI,_PED->C6_UM,_PED->QTDAUX,_PED->QTDE,Nil})
		dbSkip()
	endDO
	_PED->(dbCloseArea())


	cProd  := aMatTit[oLbxIte:nAt][2]
	cDescr := aMatTit[oLbxIte:nAt][3]
	nQtdAx := aMatTit[oLbxIte:nAt][5]
	nPeso  := aMatTit[oLbxIte:nAt][6]

	oProd:refresh()
	oSayDesc:refresh()
	oQtdAud:refresh()
	oPeso:refresh()

	oLbxIte:SetArray(aMatTit)
	oLbxIte:Refresh()
	/*oTotSel:Refresh()
	oNValor:Refresh()*/

return()


static function validP(xMat)
return Positivo(xMat)

Static Function AltVcto(oDlg)
	Local nInd := 0

	If MsgYesNo("Confirma a alteração do vencimento dos títulos selecionados?")
		For nInd:=1 To Len(aMatTit)
			If aMatTit[nInd][1]

			EndIf
		Next

		MsgAlert("Vencimentos alterados no Grid!" + chr(13)+chr(10) + "Clique em Confirmar para gravar as alterações no banco de dados")

		oLbxIte:Refresh()

	EndIf

Return

Static Function GravaConf(oDlg)
	Local nInd := 0
	Local lChk := .f.

	If MsgYesNo("Confirma a atualização dos títulos selecionados ?")

		if lCheck_Vl
			if MsgYesNo("Confirma alteração do Novo Valor ?","ATENÇÃO!!!")
				lChk := .t.
			endIf
		endIf

		For nInd:=1 To Len(aMatTit)

			If aMatTit[nInd][1]

				MsUnLock()

			EndIf
		Next

		MsgAlert("Dados gravados com sucesso!")

	EndIf


	Close(oDlg)

Return



Static Function RunProc()

	
	If Len(aMatTit)==0

		AADD(aMatTit,;
			{	.F.			,; //01
			""			,; //02
			""	 		,; //03
			""			,; //04
			0.0000			,; //06
			0.0000			,;
			Nil}) //07
	EndIf	
Return



Static Function ImpRel()
	Local	oReport
	Local	aArea	:= GetArea()
	Private cTitulo	:= "Alterar Vencimentos dos Títulos a Pagar"
	Private lRelNew	:= .F.
	Private lFechar := .F.

	lRelNew := .T.

	oReport := ReportDef()
	oReport:PrintDialog()

	RestArea( aArea )

Return


Static Function ReportDef()
	Local oReport
	Local oSection1
	Local cDesc:= "Alterar Vencimentos dos Títulos a Pagar"
	Local aOrd := {"Emissão"}

	//-- Inicio definicao do Relatorio
	DEFINE REPORT oReport NAME cPerg TITLE "Alterar Vencimentos dos Títulos a Pagar" ACTION {|oReport| PrintReport(oReport)} DESCRIPTION cDesc

	//-- Section de Informacoes
	DEFINE SECTION oSection1 OF oReport ORDERS aOrd TITLE "Alterar Vencimentos dos Títulos a Pagar" PAGE BREAK
	oSection1:SetHeaderPage(.T.)
	oSection1:SetPageBreak(.T.)

	DEFINE CELL NAME "XX_MARCA"  	OF oSection1 ALIAS "   " TITLE "   " 				PICTURE "@!"				SIZE 020 PIXEL
	DEFINE CELL NAME "C6_PRODUTO"  	OF oSection1 ALIAS "   " TITLE "Produto"   			PICTURE PesqPict("SC6", "C6_PRODUTO") 		SIZE 050 PIXEL
	DEFINE CELL NAME "C6_DESCRI"  	OF oSection1 ALIAS "   " TITLE "Descrição"   			PICTURE PesqPict("SC6", "C6_DESCRI") 		SIZE 160 PIXEL
	DEFINE CELL NAME "C6_UM"  	OF oSection1 ALIAS "   " TITLE "UM"   				PICTURE PesqPict("SC6", "C6_UM") 		SIZE 030 PIXEL
	DEFINE CELL NAME "QTDAUX"  	OF oSection1 ALIAS "   " TITLE "Qtde Aux."   			PICTURE PesqPict("SC6", "C6_X_QTAUX")  		SIZE 030 PIXEL
	DEFINE CELL NAME "QTDE"  	OF oSection1 ALIAS "   " TITLE "Qtde"   			PICTURE PesqPict("SC6", "C6_QTDVEN")  		SIZE 030 PIXEL


Return oReport

Static Function PrintReport(oReport)
	Local oSection1:= oReport:Section(1)
	Local nInd := 0

	cTitulo  := If(AllTrim(oReport:Title()) == AllTrim(cTitulo), cTitulo, oReport:Title())
	oReport:SetTitle(cTitulo)
	oSection1:INIT(.F.)

	oReport:SetMeter(Len(aMatTit))


	For nInd:=1 To Len(aMatTit)

		oReport:IncMeter()
		If oReport:Cancel()
			@Prow()+1,0 PSAY cCancel
			Exit
		EndIf

		/*oSection1:Cell("XX_MARCA"):SetValue(IIF(aMatTit[nInd][01]," X ", "  "))
		oSection1:Cell("E2_FILIAL"):SetValue(aMatTit[nInd][02])
		oSection1:Cell("E2_PREFIXO"):SetValue(aMatTit[nInd][03])
		oSection1:Cell("E2_NUM"):SetValue(aMatTit[nInd][04])
		oSection1:Cell("E2_FORNECE"):SetValue(aMatTit[nInd][05])
		oSection1:Cell("E2_LOJA"):SetValue(aMatTit[nInd][06])
		oSection1:Cell("A2_NOME"):SetValue(aMatTit[nInd][07])
		oSection1:Cell("E2_EMISSAO"):SetValue(aMatTit[nInd][08])
		oSection1:Cell("E2_VALOR"):SetValue(aMatTit[nInd][09])
		oSection1:Cell("XX_N_VALOR"):SetValue(aMatTit[nInd][10])
		oSection1:Cell("E2_VENCTO"):SetValue(aMatTit[nInd][11])
		oSection1:Cell("E2_VENCREA"):SetValue(aMatTit[nInd][12])
		oSection1:Cell("XX_N_VENC"):SetValue(aMatTit[nInd][13])
		oSection1:Cell("XX_N_VENCREA"):SetValue(aMatTit[nInd][14])
	oSection1:PrintLine()*/

	Next


	oSection1:Finish()

Return

