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
	Local nLin := 00
	Local nCol := 00
	Private	aMatTit:= {}
	Private oLbxIte
	Private oGrpCar
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
	Private oTotPed
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
	Private nTotPed  := 0
	Private cCarga  := space(6)
	Private cGrCar  := space(6)
	Private cPedido := space(6)
	Private oCarga
	Private oPedido
	Private aCarga  := {}


	///                   nome   tam alt bold itali  under
	oFont10n := TFont():New("VERDANA", 10, 10, .T., .F., , , , .T., .F.)
	oFont14  := TFont():New("VERDANA", 14, 14,    , .F., , , , .T., .F.)
	oFont14n := TFont():New("VERDANA", 14, 14, .T., .F., , , , .T., .F.)
	oFont16  := TFont():New("VERDANA", 16, 16,    , .F., , , , .T., .F.)
	oFont16N := TFont():New("VERDANA", 16, 16, .T., .T., , , , .T., .F.)
	oFont24N := TFont():New("VERDANA", 24, 24, .T., .T., , , , .T., .F.)

	Processa({|| RunProc()})

	aSize := MsAdvSize(.F.)

	DEFINE MSDIALOG oDlg TITLE cCadastro STYLE DS_MODALFRAME  From aSize[7],0 To aSize[6],aSize[5]   OF oMainWnd PIXEL
/// linha  coluna
	@ 000,000 	to 510,900 	dialog oDlg 	title cCadastro

	@ 003+nLin, 003+nCol GROUP oGrpCar 	TO 043+nLin, 350+nCol 	PROMPT "Carga " OF oDlg COLOR 0, 16777215 PIXEL

	@ 003+nLin, 353+nCol GROUP oGrpPed 	TO 043+nLin, 450+nCol 	PROMPT "Pedido " OF oDlg COLOR 0, 16777215 PIXEL

	@ 015+nLin,012+nCol MSGET  oCarga VAR cCarga   VALID   vldCar()  SIZE 060,20    FONT oFont24N PICTURE "999999" OF oDlg COLOR 0, 16777215 PIXEL

	@ 015+nLin,080+nCol SAY oSayEmis PROMPT dEmiss SIZE 100,030 FONT oFont14n OF oDlg COLOR 0, 16777215 PIXEL

	@ 027+nLin,080+nCol SAY oSayDCar PROMPT cDscCarga SIZE 350,030 FONT oFont14n OF oDlg COLOR 0, 16777215 PIXEL

	@ 015+nLin,363+nCol MSGET  oPedido VAR cPedido   VALID   vldPed()   SIZE 060,20    FONT oFont24N PICTURE "999999" OF oDlg COLOR 0, 16777215 PIXEL

	@ 047+nLin, 003+nCol GROUP oGrpIte TO 100+nLin, 450+nCol PROMPT "Item " OF oDlg COLOR 0, 16777215 PIXEL

	@ 060+nLin,012+nCol SAY oSayProd PROMPT "Produto" SIZE 060,020 OF oDlg COLOR 0, 16777215 PIXEL
	@ 070+nLin,012+nCol MSGET oProd VAR cProd WHEN .F. SIZE 060,020 FONT oFont16n PICTURE "@!" OF oDlg COLOR 0, 16777215 PIXEL

	@ 060+nLin,080+nCol SAY oSayDesc PROMPT "Descrição" SIZE 100, 020 OF oDlg COLOR 0, 16777215 PIXEL
	@ 070+nLin,080+nCol MSGET oDesc VAR cDescr WHEN .F. SIZE 200, 020 FONT oFont16n PICTURE "@!" OF oDlg COLOR 0, 16777215 PIXEL

	@ 060+nLin,300+nCol SAY oSayQtdAux PROMPT "Qtd. Aux." SIZE 100, 020 OF oDlg COLOR 0, 16777215 PIXEL
	@ 070+nLin,300+nCol MSGET oQtdAud VAR nQtdAx  WHEN .F. SIZE 060, 020 FONT oFont16n PICTURE "999,999" OF oDlg COLOR 0, 16777215 PIXEL

	@ 060+nLin,365+nCol SAY oSayPes PROMPT "Peso" SIZE 080, 020 OF oDlg COLOR 0, 16777215 PIXEL
	@ 070+nLin,365+nCol MSGET oPeso VAR nPeso   VALID   vldPeso()    SIZE 080, 020 FONT oFont16n PICTURE "@9 999,999.9999" OF oDlg COLOR 0, 16777215 PIXEL

	@ 105+nLin, 003+nCol GROUP oGrpTIte TO 230+nLin, 450+nCol PROMPT "Ítens do Pedido " OF oDlg COLOR 0, 16777215 PIXEL

	@ 200+nLin,365+nCol SAY oSayTPPes PROMPT "Peso Pedido" SIZE 080, 030 FONT oFont16N OF oDlg COLOR 0, 16777215 PIXEL
	@ 210+nLin,355+nCol SAY oTotPed PROMPT nTotPed SIZE 120, 030 FONT oFont16N PICTURE "@9 99,999,999.9999" OF oDlg COLOR 0, 16777215 PIXEL

	//ListBox Com os Itens do pedido
	@ 115+nLin,005+nCol LISTBOX oLbxIte VAR cVar FIELDS HEADER " " SIZE 350,110 OF oDlg PIXEL ON dblClick(MarcaGrid(@oLbxIte,@aMatTit),oLbxIte:Refresh())
	//oLbxIte:AddColumn(TCColumn():New(" "          , {|| If(aMatTit[oLbxIte:nAt][01],oOk,oNo)}, "@BMP"                       ,    ,    ,        , 1  , .T., .F.))
	oLbxIte:AddColumn(TcColumn():New("Produto"    , {|| aMatTit[oLbxIte:nAt][02]}           , PesqPict("SC6", "C6_PRODUTO"), nil, nil, "LEFT" , nil, .F., .F. , nil, nil, nil, .F., nil))
	oLbxIte:AddColumn(TcColumn():New("Descrição"  , {|| aMatTit[oLbxIte:nAt][03]}           , PesqPict("SC6", "C6_DESCRI") , nil, nil, "LEFT" , nil, .F., .F. , nil, nil, nil, .F., nil))
	oLbxIte:AddColumn(TcColumn():New("UM"         , {|| aMatTit[oLbxIte:nAt][04]}           , PesqPict("SC6", "C6_UM")     , nil, nil, "LEFT" , nil, .F., .F. , nil, nil, nil, .F., nil))
	oLbxIte:AddColumn(TcColumn():New("Qtd. Aux."  , {|| aMatTit[oLbxIte:nAt][05]}           , PesqPict("SC6", "C6_X_QTAUX"), nil, nil, "RIGTH", nil, .F., .F. , nil, nil, nil, .F., nil))
	oLbxIte:AddColumn(TcColumn():New("Qtd. (Peso)", {|| aMatTit[oLbxIte:nAt][06]}           , PesqPict("SC6", "C6_QTDVEN") , nil, nil, "RIGTH", nil, .F., .F. , nil, nil, nil, .F., nil))

	oLbxIte:SetArray(aMatTit)
	oLbxIte:Refresh()

	@ 237,010 Button OemToAnsi("Grava Pedido") 			Size 60,12 Action (GrvPed(oDlg)) Object btnRel2
	@ 237,075 Button OemToAnsi("Voltar") 				Size 60,12 Action (Close(oDlg)) 	Object btnFechar2
	oDlg:Refresh()

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

	IF MsgNoYes("Deseja alterar o peso do ítem selecionado?","ATENÇÃO!!!")
		cProd  := aMatTit[oLbxIte:nAt][2]
		cDescr := aMatTit[oLbxIte:nAt][3]
		nQtdAx := aMatTit[oLbxIte:nAt][5]
		nPeso  := aMatTit[oLbxIte:nAt][6]
		cGrCar := aMatTit[oLbxIte:nAt][7]
		cPedido:= aMatTit[oLbxIte:nAt][8]
		cPesou := aMatTit[oLbxIte:nAt][9]
		nSeq   := aMatTit[oLbxIte:nAt][10]

		oProd:refresh()
		oSayDesc:refresh()
		oQtdAud:refresh()
		oPeso:refresh()
		oPeso:SetFocus()

	endIf

Return

static function vldPeso()

	if !empty(cPedido) .and. !empty(cCarga)
		if MsgNoYes("Confirma Pesagem?","CONFIRMA")
			aMatTit[oLbxIte:nAt][6] := nPeso
			aMatTit[oLbxIte:nAt][9] := 'S'
			
			atuPes()
			
			if oLbxIte:nAt < len(aMatTit)
				oLbxIte:nAt := oLbxIte:nAt + 1
				cProd  := aMatTit[oLbxIte:nAt][2]
				cDescr := aMatTit[oLbxIte:nAt][3]
				nQtdAx := aMatTit[oLbxIte:nAt][5]
				nPeso  := aMatTit[oLbxIte:nAt][6]
				cGrCar := aMatTit[oLbxIte:nAt][7]
				cPedido:= aMatTit[oLbxIte:nAt][8]
				cPesou := aMatTit[oLbxIte:nAt][9]
				nSeq   := aMatTit[oLbxIte:nAt][10]

				oProd:refresh()
				oSayDesc:refresh()
				oQtdAud:refresh()
				oPeso:refresh()
				oTotPed:refresh()
				//oTotGer:refresh()

				oLbxIte:SetArray(aMatTit)
				oLbxIte:Refresh()
				oPeso:SetFocus()
			
			else
				atuPes()

				oLbxIte:nAt := 1
				cProd  := aMatTit[oLbxIte:nAt][2]
				cDescr := aMatTit[oLbxIte:nAt][3]
				nQtdAx := aMatTit[oLbxIte:nAt][5]
				nPeso  := aMatTit[oLbxIte:nAt][6]
				cGrCar := aMatTit[oLbxIte:nAt][7]
				cPedido:= aMatTit[oLbxIte:nAt][8]
				cPesou := aMatTit[oLbxIte:nAt][9]
				nSeq   := aMatTit[oLbxIte:nAt][10]

				oProd:refresh()
				oSayDesc:refresh()
				oQtdAud:refresh()
				oPeso:refresh()

				oTotPed:refresh()
				//oTotGer:refresh()

				oLbxIte:SetArray(aMatTit)
				oLbxIte:Refresh()
			endIF

		else
			oPeso:SetFocus()
		endIf
	endIf

return()

static function atuPes()

	Local nX := 0
	nTotPes := 0
	nTotPed := 0

	For nX := 1 to Len(aMatTit)
		if aMatTit[nX][9] == 'S'
			nTotPed := nTotPed + aMatTit[nX][6]
		endIf
	Next Nx

return()

static function vldCar()

	dbSelectArea("DAK")
	dbSetOrder(1)
	if dbSeek(xFilial("DAK")+cCarga)
		aCarga := {}
		dbSelectArea("DAI")
		dbSetOrder(1)
		dbSeek(xFilial("DAK")+DAK->DAK_COD)
		while(!eof() .and. DAK->DAK_COD == DAI->DAI_COD)
			aadd(aCarga,{DAI->DAI_FILIAL,DAI->DAI_COD,DAI->DAI_SEQCAR,DAI->DAI_PERCUR,DAI->DAI_ROTA,DAI->DAI_ROTEIR,DAI->DAI_DATA,DAI->DAI_HORA,DAI->DAI_PEDIDO,DAK->DAK_CAMINH,DAK->DAK_MOTORI,DAK->DAK_JUNTOU,DAK->DAK_ACECAR,DAK->DAK_ACEVAS,DAK->DAK_ACEFIN,DAK->DAK_FLGUNI,DAK->DAK_TRANSP})
			DAI->(dbSkip())
		endDo
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
		cPedido := retPed(cCarga)
		oPedido:SetFocus()
		oPedido:Refresh()
	else
		if !empty(cCarga)
			MsgInfo("Carga inexistente","ATENÇÃO")
			oCarga:SetFocus()
		endIF
	endIf
	
return()

static function vldPed()

	Local cQuery := ""
	Local nSeq   := 0

	dbSelectArea("DAI")
	dbSetOrder(4)
	if dbSeek(xFilial("DAI")+cPedido+cCarga)
	
		cQuery := "SELECT "
		cQuery += "	C.DAK_COD, "
		cQuery += "	C6_NUM, "
		//cQuery += "	C6_ITEM, "
		cQuery += "	C6_PRODUTO, "
		cQuery += "	C6_DESCRI, "
		cQuery += "	C6_UM, "
		cQuery += "	SUM(C6_X_QTAUX) QTDAUX, "
		cQuery += "	SUM(C6_QTDVEN) QTDE, "
		cQuery += "	'N' PESOU, "
		cQuery += "	B.R_E_C_N_O_ RECNO"
		cQuery += "		FROM "
		cQuery += "			" + retSqlName("SC5") + " A "
		cQuery += "INNER JOIN " 	  + retSqlName("SC6") + " B ON B.C6_FILIAL = A.C5_FILIAL  "
		cQuery += "	AND B.C6_NUM = A.C5_NUM "
		cQuery += "	AND B.D_E_L_E_T_ = ' ' "
		cQuery += "	AND B.C6_X_PESOU <> 'S' "
		cQuery += "	AND B.C6_UM = 'KG' "  // SOMENTO PRODUTOS EM KG SERAO EXIBIDOS NA GRID
		cQuery += "INNER JOIN " 	  + retSqlName("DAK") + " C ON C.DAK_FILIAL = '" + xFilial("DAK") + "' AND C.D_E_L_E_T_ = ' ' AND DAK_FEZNF = '2'  "
		cQuery += "	AND DAK_COD = '" + strzero(val(cCarga),6) + "' "
		cQuery += "INNER JOIN "           + retSqlName("SC9") + " D ON D.C9_FILIAL = '" + xFilial("SC9") + "' "
		cQuery += "	AND D.C9_PEDIDO = B.C6_NUM "
		cQuery += "	AND D.C9_ITEM = B.C6_ITEM "
		cQuery += "	AND D.C9_PRODUTO = B.C6_PRODUTO "
		cQuery += "	AND D.C9_CARGA = C.DAK_COD "
		cQuery += "	AND D.C9_SEQCAR = C.DAK_SEQCAR "
		cQuery += "WHERE "
		cQuery += "	A.D_E_L_E_T_ = ' ' "
		cQuery += "	AND A.C5_NUM = '" + strzero(val(cPedido),6) + "' "
		cQuery += "GROUP BY DAK_COD, C6_NUM, C6_PRODUTO, C6_DESCRI, C6_UM,B.R_E_C_N_O_  "
		cQuery += "ORDER BY C.DAK_COD,C6_NUM,B.C6_PRODUTO "
		tcQuery cQuery new Alias "_PED"
		dbSelectArea("_PED")
		dbGoTop()

		aMatTit := {}
		while(!eof()) ////     1           2                3           4          5              6           7          8           9        10   11
			aadd(aMatTit,{.F.,_PED->C6_PRODUTO,_PED->C6_DESCRI,_PED->C6_UM,_PED->QTDAUX,_PED->QTDE,_PED->DAK_COD,_PED->C6_NUM,_PED->PESOU,_PED->RECNO,Nil})
			dbSkip()
		endDO
		_PED->(dbCloseArea())

		if !Len(aMatTit)==0
			cProd  := aMatTit[oLbxIte:nAt][2]
			cDescr := aMatTit[oLbxIte:nAt][3]
			nQtdAx := aMatTit[oLbxIte:nAt][5]
			nPeso  := aMatTit[oLbxIte:nAt][6]
			cGrCar := aMatTit[oLbxIte:nAt][7]
			cPedido:= aMatTit[oLbxIte:nAt][8]
			cPesou := aMatTit[oLbxIte:nAt][9]
			nSeq   := aMatTit[oLbxIte:nAt][10]
		else
			AADD(aMatTit,;
				{	.F.			,; //01
				""			,; //02
				""	 		,; //03
				""			,; //04
				0.0000			,; //05
				0.0000			,; //06
				""			,; //07
				""	 		,; //08
				"N"	 		,; //09
				0	 		,; //10
				Nil}) 			 //11
		endIf

		

		oProd:refresh()
		oSayDesc:refresh()
		oQtdAud:refresh()
		oPeso:refresh()

		oLbxIte:SetArray(aMatTit)
		oLbxIte:Refresh()

	else
		if !empty(cPedido)
			MsgInfo("Pedido inexistente nessa carga","ATENÇÃO")
			oPedido:SetFocus()
		endIF
	endIF
	/*oTotSel:Refresh()
	oNValor:Refresh()*/

return()

Static Function GrvPed(oDlg)
	Local nInd := 0

	If MsgYesNo("Gravar pesagem do pedido número: " + cPedido + " ?")
		For nInd:=1 To Len(aMatTit)
			DBSelectArea("SC6")
			SC6->(dbGoTO(aMatTit[nInd][10]))
			recLock("SC6",.f.)
			SC6->C6_QTDVEN 	:= aMatTit[nInd][6]
			SC6->C6_VALOR 	:= aMatTit[nInd][6]*SC6->C6_PRCVEN
			SC6->C6_X_PESOU := 'S'
			MsUnLock()

			DBSelectArea("SC9")
			SC9->(DBSetOrder(13))
			DBSeek(xFilial("SC9")+SC6->C6_NUM+SC6->C6_ITEM+SC6->C6_PRODUTO)
			SC9->(a460Estorna(.T.))

			/*MaLibDoFat(;
				SC6->(RecNo()),; //nRegSC6
				SC6->C6_QTDVEN,; //nQtdaLib
				,;               //lCredito
				,;               //lEstoque
				.T.,;            //lAvCred
				.T.,;            //lAvEst
				.F.,;            //lLibPar
				.F.;             //lTrfLocal
			)*/
		Next

		dbSelectArea("SC5")
		dbSetOrder(1)
		dbSeek(xFilial("SC5")+cPedido)
		RecLock("SC5", .F.)
            	SC5->C5_LIBEROK := 'S'
        	SC5->(MsUnlock())

		For nInd:=1 To Len(aMatTit)
			DBSelectArea("SC6")
			SC6->(dbGoTO(aMatTit[nInd][10]))
			
			MaLibDoFat(;
				SC6->(RecNo()),; //nRegSC6
				SC6->C6_QTDVEN,; //nQtdaLib
				,;               //lCredito
				,;               //lEstoque
				.T.,;            //lAvCred
				.T.,;            //lAvEst
				.F.,;            //lLibPar
				.F.;             //lTrfLocal
			)
		Next

		aMatTit := {}
		AADD(aMatTit,;
			{	.F.			,; //01
			""			,; //02
			""	 		,; //03
			""			,; //04
			0.0000			,; //05
			0.0000			,; //06
			""			,; //07
			""	 		,; //08
			"N"	 		,; //09
			0	 		,; //10
			Nil}) 			 //11

		cProd  := ""
		cDescr := ""
		nQtdAx := ""
		nPeso  := 0
		cGrCar := ""
		cPedido:= ""
		cPesou := ""
		nSeq   := 0

		oProd:refresh()
		oSayDesc:refresh()
		oQtdAud:refresh()
		oPeso:refresh()

		oLbxIte:SetArray(aMatTit)
		oLbxIte:Refresh()

		cPedido := retPed(cCarga)
		oPedido:SetFocus()
		oPedido:Refresh()

		if Empty(cPedido)
			MyOMS200()  // chamada da funcao para recriar a carga
		endIf
	EndIf

Return

static function retPed(xCarga)

	Local c_Pedido := ""
	Local cQuery := ""

	cQuery := "SELECT "
	cQuery += "	C6_NUM PEDIDO "
	cQuery += "		FROM "
	cQuery += "			" + retSqlName("SC5") + " A "
	cQuery += "INNER JOIN " 	  + retSqlName("SC6") + " B ON B.C6_FILIAL = A.C5_FILIAL  "
	cQuery += "	AND B.C6_NUM = A.C5_NUM "
	cQuery += "	AND B.D_E_L_E_T_ = ' ' "
	cQuery += "	AND B.C6_X_PESOU <> 'S' "
	cQuery += "	AND B.C6_UM = 'KG' "  // SOMENTO PRODUTOS EM KG SERAO EXIBIDOS NA GRID
	cQuery += "INNER JOIN " 	  + retSqlName("DAK") + " C ON C.DAK_FILIAL = '" + xFilial("DAK") + "' AND C.D_E_L_E_T_ = ' ' AND DAK_FEZNF = '2' "
	cQuery += "	AND DAK_COD = '" + strzero(val(xCarga),6) + "' "
	cQuery += "INNER JOIN "           + retSqlName("SC9") + " D ON D.C9_FILIAL = '" + xFilial("SC9") + "' "
	cQuery += "	AND D.C9_PEDIDO = B.C6_NUM "
	cQuery += "	AND D.C9_ITEM = B.C6_ITEM "
	cQuery += "	AND D.C9_PRODUTO = B.C6_PRODUTO "
	cQuery += "	AND D.C9_CARGA = C.DAK_COD "
	cQuery += "	AND D.C9_SEQCAR = C.DAK_SEQCAR "
	cQuery += "WHERE "
	cQuery += "	A.D_E_L_E_T_ = ' ' AND A.C5_FILIAL = '"  + xFilial("SC5") + "' "
	cQuery += "GROUP BY C6_NUM "
	cQuery += "ORDER BY C6_NUM "
	tcQuery cQuery new Alias "_PD"
	dbSelectArea("_PD")
	dbGoTop()
	if !eof()
		c_Pedido := _PD->PEDIDO
	endIf
	_PD->(dbCloseArea())

return(c_Pedido)

Static Function RunProc()

	
	If Len(aMatTit)==0

		AADD(aMatTit,;
			{	.F.			,; //01
			""			,; //02
			""	 		,; //03
			""			,; //04
			0.0000			,; //05
			0.0000			,; //06
			""			,; //07
			""	 		,; //08
			"N"	 		,; //09
			0	 		,; //10
			Nil}) //11
	EndIf	
Return

static Function MyOMS200()
	Local aCab      := {}   // Array do Cabeçalho da Carga
	Local aItem     := {}   // Array dos Pedidos da Carga
	Local cPedido   := ""
	Local nX := 1 
	Local cCarga  := ""
	Local cSeqCar := ""
   	Private lMsHelpAuto := .T. //Variavel de controle interno do ExecAuto
   	Private lMsErroAuto := .F. //Variavel que informa a ocorrência de erros no ExecAuto
 	
	//GETSX8NUM("DAK","DAK_COD")
	//                       1            2               3               4              5            6              7              8             9                10              11                12              13             14               15             16             17
	//aadd(aCarga,{DAI->DAI_FILIAL,DAI->DAI_COD,DAI->DAI_SEQCAR,DAI->DAI_PERCUR,DAI->DAI_ROTA,DAI->DAI_ROTEIR,DAI->DAI_DATA,DAI->DAI_HORA,DAI->DAI_PEDIDO,DAK->DAK_CAMINH,DAK->DAK_MOTORI,DAK->DAK_JUNTOU,DAK->DAK_ACECAR,DAK->DAK_ACEVAS,DAK->DAK_ACEFIN,DAK->DAK_FLGUNI,DAK->DAK_TRANSP})
   	aCab := {;   
		{"DAK_FILIAL", aCarga[1,1], Nil},;
		{"DAK_COD"   , aCarga[1,2], Nil},; //Campo com inicializador padrão para pegar GESX8NUM
		{"DAK_SEQCAR", aCarga[1,3], Nil},;
		{"DAK_ROTEIR", aCarga[1,6], Nil},;
		{"DAK_CAMINH", aCarga[1,10], Nil},;
		{"DAK_MOTORI", aCarga[1,11], Nil},;
		{"DAK_PESO"  , 0          , Nil},; // Calculado pelo OMSA200
		{"DAK_DATA"  , aCarga[1,7], Nil},;
		{"DAK_HORA"  , aCarga[1,8], Nil},;
		{"DAK_JUNTOU", aCarga[1,12], Nil},;
		{"DAK_ACECAR", aCarga[1,13], Nil},;
		{"DAK_ACEVAS", aCarga[1,14], Nil},;
		{"DAK_ACEFIN", aCarga[1,15], Nil},;
		{"DAK_FLGUNI", aCarga[1,16], Nil},; //Campo com inicializador padrão  - 2
		{"DAK_TRANSP", aCarga[1,17], Nil};
   	}
   
	cPedido := ""
  	For nX := 1 to Len(aCarga)
		// Posiciona no primeiro pedido de venda
		
		if cPedido <> aCarga[nX,9]
			cPedido := aCarga[nX,9]
			SC5->(DbSetOrder(1))
			SC5->(DbSeek(xFilial("SC5")+cPedido))
			// Posiciona no cliente do primeiro pedido
			SA1->(DbSetOrder(1))
			SA1->(DbSeek(xFilial("SA1")+SC5->C5_CLIENTE))
			// Informações do primeiro pedido
			// Este array não tem o formato padrão de execuções automáticas
			Aadd(aItem, {;
			aCarga[nX,2],; // 01 - Código da carga
			aCarga[nX,6] ,; // 02 - Código da Rota - 999999 (Genérica)
			aCarga[nX,4] ,; // 03 - Código da Zona - 999999 (Genérica)
			aCarga[nX,5],; // 04-  Código do Setor - 999999 (Genérico)
			SC5->C5_NUM   ,; // 05 - Código do Pedido Venda
			SA1->A1_COD   ,; // 06 - Código do Cliente
			SA1->A1_LOJA  ,; // 07 - Loja do Cliente
			SA1->A1_NOME  ,; // 08 - Nome do Cliente
			SA1->A1_BAIRRO,; // 09 - Bairro do Cliente
			SA1->A1_MUN   ,; // 10 - Município do Cliente
			SA1->A1_EST   ,; // 11 - Estado do Cliente
			SC5->C5_FILIAL,; // 12 - Filial do Pedido Venda
			SA1->A1_FILIAL,; // 13 - Filial do Cliente
			0             ,; // 14 - Peso Total dos Itens
			0             ,; // 15 - Volume Total dos Itens
			aCarga[nX,8]  ,; // 16 - Hora Chegada
			"0001:00"     ,; // 17 - Time Service
			Nil           ,; // 18 - Não Usado
			aCarga[nX,7]  ,; // 19 - Data Chegada
			aCarga[nX,7]  ,; // 20 - Data Saída
			Nil           ,; // 21 - Não Usado
			Nil           ,; // 22 - Não Usado
			0             ,; // 23 - Valor do Frete
			0             ,; // 24- Frete Autonomo
			0             ,; // 25 - Valor Total dos Itens (Calculado pelo OMSA200)
			0             ,; // 26 - Quantidade Total dos Itens (Calculado pelo OMSA200)
			Nil           ,; // 27 - Não usado
			""      }) // 28 - Transportadora redespachante (não obrigatório)
		endIf
 	Next nX
	
	/// irei realizar o estorno da carga primeito
	cCarga  := aCarga[1,2]
   	cSeqCar := aCarga[1,3]
    
	dbSelectArea("DAK")
	dbSetOrder(1)
	If DAK->(dbSeek(xFilial("DAK")+cCarga+cSeqCar))
		MSExecAuto( { |x, y, z| OMSA200(x, y, z) }, aCab, aItem, 6 )
		If lMsErroAuto
			Alert("Carga " + cCarga + " não está apta a ser estornada. Verifique os pedidos e refaça o processo!!!")
			cMsgErro := MostraErro()
			DisarmTransaction()
			//Alert(cMsgErro)
		EndIf
	EndIf

	// gera carga novamente
   	MSExecAuto( { |x, y, z| OMSA200(x, y, z) }, aCab, aItem, 3 )
 
   	If lMsErroAuto
		Alert("Carga " + cCarga + " não está apta a ser faturada. Verifique os pedidos e refaça o processo!!!")
		cMsgErro := MostraErro()
		DisarmTransaction()
		//Alert(cMsgErro)
	EndIf

	For nX := 1 to len(aItem)

		DBSelectArea("SC9")
		SC9->(DBSetOrder(1))
		DBSeek(xFilial("SC9")+aItem[nX,5])
		if empty(SC9->C9_CARGA)
			dbSelectArea("SC6")
			SC6->(DBSetOrder(1))
			dbSeek(xFilial("SC6")+aItem[nX,5])
			recLock("SC6",.f.)
			SC6->C6_X_PESOU := "N"
			MsUnlock()
		endIf
	Next nX
 
Return
