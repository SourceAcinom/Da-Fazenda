#include "rwmake.ch"
#include "topconn.ch"
#include "colors.ch"
#include 'Ap5Mail.ch'
#INCLUDE "TbiConn.ch"
#INCLUDE "Protheus.ch"
#INCLUDE "TOTVS.ch"

#Define CLR_AZUL		RGB(058,074,119)								//Cor Azul
#Define STR_PULA		Chr(13)+Chr(10)                     			//Pula linha

Static COL_T1 	:= 001				//Primeira Coluna da tela
Static COL_T2 	:= 123				//Segunda Coluna da tela
Static COL_T3 	:= 245				//Terceira Coluna da tela
Static COL_T4 	:= 367				//Quarta Coluna da tela
Static ESP_CAMPO	:= 038				//Espaçamento do campo para coluna

User Function THFATBOL(cNotaDe, cNotaAte, cSerie)
	Local nRegs:= 0
	Local lEmite
	Local cMsg
	Private oPrint
	Private cBitmap:= "\system\.BMP"
	Private cNossoNum
	Private cNossoNumImpressao

	Private cAgeEmp
	Private cCtaEmp
	Private cDigEmp
	Private cDigAge

	Private cCorrNom  // Nome do banco correspondente
	Private cCorrCNPJ // CNPJ do banco correspondente
	Private cCorrEnd  // Endereço do banco correspondente

	Private cCdBarra
	Private cRepNum
	Private cCNPJ
	Private aLog:= {}
	Private cCartEmp
	// Private cVariaCart                                     //Customizado: Variável criada para armazenar Variação da Carteira, campo criado EE_X_VARIA, #Rafael Achôa 17-07-2014
	Private nDiasProtesto								   //Customizado: Variável criada para armazenar dias de Protesto da conta (campo EE_DIASPRT), #Rafael Achôa 17-07-2014

	Private cLayOut := "LASER"
	Private nOpcaoemail

	Private strIT01
	Private strIT02
	Private strIT03
	Private strIT04
	Private strIT05
	Private strIT06
	Private strIT07
	Private strIT08
	Private strIT09
	Private strIT10
	Private strIT11
	Private strIT12

	Private cInst01 := ""
	Private cInst02 := "" //mv_par10+mv_par11
	Private cInst03 := "" //mv_par12+mv_par13
	Private cInst04 := "" //mv_par14+mv_par15
	
	Private oTmpTable1
	
	private cBcSel := ''
	private cAgSel := ''
	private cCtSel := ''

	cPerg:= "THFATBOL"

	ValidPerg()

	If !Pergunte(cPerg, .t.)
		Return
	EndIf

	mv_par08 := Space(TamSx3("F2_CARGA")[1])
	mv_par09 := Space(TamSx3("F2_CARGA")[1])
	mv_par10 := 1

	If cNotaDe <> Nil .and. cSerie <> Nil
		MV_PAR04 := cNotaDe
		MV_PAR05 := cNotaAte
		MV_PAR06 := cSerie
	EndIf

	If !mv_par01 $ "237,341,033,399,001,748,104,422,084,246,001"
		MsgBox(OemToAnsi("PROCESSO CANCELADO: Não existe layout de boleto preparado para o banco parametrizado!"), "Mensagem", "ALERT")
		Return
	EndIf

	cBcSel := mv_par01
	cAgSel := mv_par02
	cCtSel := mv_par03

	// Posicionar o banco
	DbSelectArea("SA6")
	DbSetOrder(1)
	If !DbSeek(xFilial() + mv_par01 + mv_par02 + mv_par03)
		MsgBox(OemToAnsi("PROCESSO CANCELADO: Banco + Agencia + Conta não cadastrado no sistema!"), "Mensagem", "ALERT")
		Return
	EndIf

	// Posiciona no nosso numero (Parametros dos bancos)
	DbSelectArea("SEE")
	DbSetOrder(1)
	if !DbSeek(xFilial("SEE")+Pad(mv_par01,3) + Pad(mv_par02,5) + Pad(mv_par03,10))
		MsgBox(OemToAnsi("PROCESSO CANCELADO: Preencha primeiramente os parametros do banco selecionado!"), "Mensagem", "ALERT")
		Return
	EndIf
	// Aqui vou carregar as mensagens dos boletos, se houver nos parametros ele da preferencia aos parametros.
	If Empty(alltrim(cInst01)) .and. !empty(SEE->EE_FORMEN1)
		cInst01 := &(SEE->EE_FORMEN1)
	EndIf
	If Empty(alltrim(cInst02)) .and. !empty(SEE->EE_FORMEN2)
		cInst02 := &(SEE->EE_FORMEN2)
	EndIf
	If Empty(alltrim(cInst03)) .and. !empty(SEE->EE_FOREXT1)
		cInst03 := &(SEE->EE_FOREXT1)
	EndIf
	If Empty(alltrim(cInst04)) .and. !empty(SEE->EE_FOREXT2)
		cInst04 := aLLTRIM(&(SEE->EE_FOREXT2))
	EndIf

	strIT01 := Substr(AllTrim(cInst01),1,83)
	strIT02 := Substr(AllTrim(cInst01),84,83)
	strIT03 := Substr(AllTrim(cInst01),166,83)

	strIT04 := Substr(AllTrim(cInst02),1,83)
	strIT05 := Substr(AllTrim(cInst02),84,83)
	strIT06 := Substr(AllTrim(cInst02),166,83)

	strIT07 := Substr(AllTrim(cInst03),1,83)
	strIT08 := Substr(AllTrim(cInst03),84,83)
	strIT09 := Substr(AllTrim(cInst03),166,83)

	strIT10 := Substr(AllTrim(cInst04),1,83)
	strIT11 := Substr(AllTrim(cInst04),84,83)
	strIT12 := Substr(AllTrim(cInst04),166,83)

	Do Case
		Case SA6->A6_COD == "237" // Bradesco
		cAgeEmp:= Padl(Right(alltrim(StrTran(SEE->EE_AGENCIA, '-', '')), 4), 4, '0')
		cDigAge:= Alltrim(SEE->EE_DVAGE)
		cCtaEmp:= Padl(Right(alltrim(StrTran(SEE->EE_CONTA, '-', '')), 7), 7, '0')
		cDigEmp:= alltrim(SEE->EE_DVCTA)
		cCartEmp:= AllTrim(SEE->EE_CODCART)
		nDiasProtesto := Val(Alltrim(SEE->EE_DIASPRT))
		If cCtaEmp == "0457167"  															//ESSA CONTA SE REFERE A BANCO CORRESPONDENTE
			cCorrNom:="UNIPRIME NORTE DO PR - COOP.DE ECONOMIA"  							// Nome do banco correspondente
			cCorrCNPJ:="CNPJ: 02.398.976/0001-90"											// CNPJ do banco correspondente
			cCorrEnd:="AV. RIO DE JANEIRO, 1758 - CENTRO - 86010-150 LONDRINA(PR)"  	    // Endereço do banco correspondente
		EndIf

		Case SA6->A6_COD == "084" // Uniprime
		cAgeEmp:= Padl(Right(alltrim(StrTran(SEE->EE_AGENCIA, '-', '')), 4), 4, '0')
		cDigAge:= Alltrim(SEE->EE_DVAGE)
		cCtaEmp:= Padl(Right(alltrim(StrTran(SEE->EE_CONTA, '-', '')), 7), 7, '0')
		cDigEmp:= alltrim(SEE->EE_DVCTA)
		cCartEmp:= AllTrim(SEE->EE_CODCART)
		nDiasProtesto := Val(Alltrim(SEE->EE_DIASPRT))
		/*If cCtaEmp == "0457167"  															//ESSA CONTA SE REFERE A BANCO CORRESPONDENTE
			cCorrNom:="UNIPRIME NORTE DO PR - COOP.DE ECONOMIA"  							// Nome do banco correspondente
			cCorrCNPJ:="CNPJ: 02.398.976/0001-90"											// CNPJ do banco correspondente
			cCorrEnd:="AV. RIO DE JANEIRO, 1758 - CENTRO - 86010-150 LONDRINA(PR)"  	    // Endereço do banco correspondente
		EndIf*/
		
		Case SA6->A6_COD == "422" // SAFRA
		cAgeEmp:= Padl(Right(alltrim(StrTran(SEE->EE_AGENCIA, '-', '')), 4), 4, '0')
		cDigAge:= Alltrim(SEE->EE_DVAGE)
		cCtaEmp:= Padl(Right(alltrim(StrTran(SEE->EE_CONTA, '-', '')), 7), 7, '0')
		cDigEmp:= alltrim(SEE->EE_DVCTA)
		cCartEmp:= AllTrim(SEE->EE_CODCART)
		nDiasProtesto := Val(Alltrim(SEE->EE_DIASPRT))
		
		Case SA6->A6_COD == "341" // Itaú
		cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
		cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 6), 6, '0')
		cDigEmp  := AllTrim(SA6->A6_DVCTA)
		cCtaEmp:= SubStr(AllTrim(cCtaEmp),2, 5)
		cCartEmp:= AllTrim(SEE->EE_CODCART) 
		
		Case SA6->A6_COD == "033" // Santander
		cAgeEmp:= Padl(Right(alltrim(StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
		cDigAge:= AllTrim(SA6->A6_DVAGE)

		cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 8), 8, '0')
		cDigEmp:= AllTrim(SA6->A6_DVCTA)

		cCartEmp:= AllTrim(SEE->EE_CODCART)

		Case SA6->A6_COD == "399" // HSBC
		cAgeEmp:= Padl(Right(alltrim(StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
		cCtaEmp:= Padl(Right(alltrim(StrTran(SA6->A6_NUMCON, '-', '')), 7), 7, '0')
		cDigEmp:= ""
		cCartEmp:= AllTrim(SEE->EE_CODCART)

		Case SA6->A6_COD == "001" // Banco do Brasil
		cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
		cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 7), 7, '0')
		//cDigEmp:= Right(cCtaEmp, 1)
		cDigEmp:= AllTrim(SEE->EE_DVCTA)
		cDigAge:= AllTrim(SEE->EE_DVAGE)
		//cCtaEmp:= Left(cCtaEmp, 6)
		cCartEmp:= AllTrim(SEE->EE_CODCART)
		// cVariaCart:= Alltrim(SEE->EE_X_VARIA)    //Customizado para preencher corretamente o campo Carteira, onde o Banco do Brasil exige também a variação da Carteira, #Rafael Achôa 17-07-2014
		nDiasProtesto := Val(Alltrim(SEE->EE_DIASPRT))

		Case SA6->A6_COD == "748" // Sicred
		cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
		cCtaEmp:= Padl(alltrim( Substr(SA6->A6_NUMCON, 1, AT( '-', SA6->A6_NUMCON ) - 1)), 9, '0')
		cDigEmp:= ''
		cCartEmp:= ''     // AllTrim(SEE->EE_CODCART)

		Case SA6->A6_COD == "104" // CEF
		cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
		cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 7), 7, '0')
		cDigEmp:= Right(cCtaEmp, 1)
		cCtaEmp:= Left(cCtaEmp, 6)
		cCartEmp:= "12"

   	    Case SA6->A6_COD == "246" // ABC Brasil
		cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
		cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 7), 7, '0')
		cDigEmp  := AllTrim(SA6->A6_DVCTA)
		//cCtaEmp:= SubStr(AllTrim(cCtaEmp),2, 5)
		cCartEmp:= AllTrim(SEE->EE_CODCART) 

	EndCase

	Processa({|| RunProc()})

Return

/******************************************************************************************************************/
Static Function RunProc()
	/******************************************************************************************************************/
	Local cSql
	Local cLog:= ""
	Local cNNLimpo:= ""
	Local cMsg
	Local cCliente
	Local cLoja
	Local lImpBol
	Local lUniCli := .t.
	Local cTipoTit := Replace("",",", "','")
	Local nRec:=0
	Local nEsp  := 15
	Local oFontNeg := TFont():New("Tahoma",,-16,.t.,.t.)

	Local _stru:= {}
	Local aCproBro := {}
	Local oDlgLocal
	Private aCores := {}
	Private lInverte := .F.
	Private cMark   := GetMark()
	Private oMark
	Private oSay1
	Private nVlrSel := 0
	Private lImprime := .f.

	//Cria um arquivo de Apoio

	if mv_par10 == 1

		AADD(_stru,{"OK"     		,"C"	,2		,0		})
		AADD(_stru,{"F2_DOC"    	,"C"	,TamSx3("F2_DOC")[1]			,0		})
		AADD(_stru,{"F2_SERIE"   	,"C"	,TamSx3("F2_SERIE")[1]			,0		})
		AADD(_stru,{"F2_CLIENTE"   	,"C"	,TamSx3("F2_CLIENTE")[1]		,0		})
		AADD(_stru,{"F2_LOJA"		,"C"	,TamSx3("F2_LOJA")[1]			,0		})
		AADD(_stru,{"F2_VALFAT"		,"N"	,TamSx3("F2_VALFAT")[1]		,TamSx3("F2_VALFAT")[2]		})
		AADD(_stru,{"F2_CARGA"		,"C"	,TamSx3("F2_CARGA")[1]		,TamSx3("F2_CARGA")[2]		})
		AADD(_stru,{"STATUS"		,"C"	,1			,0		})

	else

		AADD(_stru,{"OK"     		,"C"	,2		,0		})
		AADD(_stru,{"E1_NUM"    	,"C"	,TamSx3("E1_NUM")[1]			,0		})
		AADD(_stru,{"E1_PREFIXO"   	,"C"	,TamSx3("E1_PREFIXO")[1]			,0		})
		AADD(_stru,{"E1_CLIENTE"   	,"C"	,TamSx3("E1_CLIENTE")[1]		,0		})
		AADD(_stru,{"E1_LOJA"		,"C"	,TamSx3("E1_LOJA")[1]			,0		})
		AADD(_stru,{"E1_NOMCLI"		,"C"	,TamSx3("E1_NOMCLI")[1]			,0		})
		AADD(_stru,{"E1_PARCELA"	,"C"	,TamSx3("E1_PARCELA")[1]			,0		})
		AADD(_stru,{"E1_TIPO"		,"C"	,TamSx3("E1_TIPO")[1]			,0		})
		AADD(_stru,{"E1_SALDO"		,"N"	,TamSx3("E1_SALDO")[1]		,TamSx3("E1_SALDO")[2]		})
		AADD(_stru,{"E1_NUMBCO"		,"C"	,TamSx3("E1_NUMBCO")[1]			,0		})
		AADD(_stru,{"E1_VENCREA"	,"D"	,TamSx3("E1_VENCREA")[1]			,0		})
		AADD(_stru,{"STATUS"		,"C"	,1			,0		})
		AADD(_stru,{"RECNOE1"		,"N"	,10			,0		})

	endif

	if Select("Boletos") > 0
		Boletos->(DbCloseArea())
		//oTmpTable1:Delete()
	endif
	
	oTmpTable1 := FWTemporaryTable():New('BOLETOS')
	oTmpTable1:SetFields(_stru)
	oTmpTable1:Create()
	
	//cArq:=Criatrab(_stru,.T.)
	//DBUSEAREA(.t.,,carq,"BOLETOS")

	if mv_par10 ==1 //C5_FILIAL = F2_FILIAL AND
		cSql := " SELECT COUNT(*) CONTADOR FROM " + RetSqlName("SF2") + " SF2 " 
		cSql += " WHERE F2_FILIAL = '" + xFilial("SF2") + "' AND "
		cSql += " F2_DOC BETWEEN '" + mv_par04 + "' AND '" + mv_par05 + "' AND F2_SERIE = '" + mv_par06 + "'  AND "
		cSql += " F2_CARGA BETWEEN '"+mv_par08+"' AND '"+mv_par09+"' AND "
		cSql += " F2_VALFAT > 0 AND "
		cSql += " SF2.D_E_L_E_T_ <> '*' "

	else
		cSql := " Select Count(*) CONTADOR FROM "+RetSqlName("SE1")+" WHERE E1_FILIAL = '"+FWxFilial("SE1")+"' and "
		cSql += " E1_NUM >= '"+mv_par04+"' and E1_NUM <= '"+mv_par05+"' AND E1_SALDO > 0 AND D_E_L_E_T_='' "
	endif
	TcQuery cSql NEW ALIAS "QRY"
	
	If !Eof()
		nRegs:= QRY->CONTADOR
	EndIf
	QRY->(DbCloseArea())

	If nRegs == 0
		MsgBox(OemToAnsi("Não há dados selecionados!"), "Mensagem", "ALERT")
		Return
	EndIf

	oPrint := TMSPrinter():New("Boleto Laser")

	lImpBol:= oPrint:Setup()
	If !(lImpBol)
		Return
	EndIf
	oPrint:SetPortrait()
	oPrint:SetPaperSize(9) // Seta para papel A4

	oPrint:StartPage()   // Inicia uma nova página

	ProcRegua(nRegs)

	if mv_par10 == 1 //C5_FILIAL = F2_FILIAL AND 
		cSql := "SELECT ' ' F2_OK, F2_FILIAL, F2_DOC, F2_SERIE, F2_CLIENTE, F2_LOJA, F2_TIPO , F2_VALFAT,F2_CARGA "
		cSql += "FROM " + RetSqlName("SF2") + " SF2 "
		cSql += " LEFT JOIN "+RetSqlName("SC5")+" SC5 on (C5_NOTA = F2_DOC AND C5_SERIE = F2_SERIE AND SC5.D_E_L_E_T_='' ) "
		cSql += " WHERE F2_FILIAL = '"+xFilial("SF2") + "' AND F2_DOC BETWEEN '" + mv_par04 + "' AND '" + mv_par05 + "' AND F2_SERIE = '"
		cSql += mv_par06 + "' AND F2_CARGA BETWEEN '"+mv_par08+"' AND '"+mv_par09+"' AND " 
		cSql += " F2_VALFAT > 0 AND " 
		cSql += " SF2.D_E_L_E_T_ <> '*' ORDER BY F2_DOC ASC, F2_SERIE "
		
	else
		cSql := " Select ' ' E1_OK,E1_NUM,E1_PREFIXO,E1_CLIENTE,E1_LOJA,E1_NOMCLI,E1_PARCELA,E1_TIPO,E1_SALDO,E1_NUMBCO,E1_VENCREA,R_E_C_N_O_ E1RECNO FROM "+RetSqlName("SE1")+" WHERE E1_FILIAL = '"+FWxFilial("SE1")+"' and "
		cSql += " E1_NUM >= '"+mv_par04+"' and E1_NUM <= '"+mv_par05+"' AND UPPER(E1_X_FORMA)='BOL' and E1_SALDO > 0 AND D_E_L_E_T_='' ORDER BY E1_NUM ASC "
	endif
	if Select("QRYBOL") > 0
		QRYBOL->(DbCloseArea())
	endif
	TcQuery cSql new alias "QRYBOL"
	
	MemoWrite("C:\Temp\qrybol.txt", cSql)

	While !QRYBOL->(Eof())

		DbSelectArea("BOLETOS")

		if mv_par10== 1
			RecLock("BOLETOS",.t.)
			//BOLETOS->Ok := " "
			BOLETOS->F2_DOC   	:=  QRYBOL->F2_DOC
			BOLETOS->F2_SERIE 	:=  QRYBOL->F2_SERIE
			BOLETOS->F2_CLIENTE :=  QRYBOL->F2_CLIENTE
			BOLETOS->F2_LOJA	:=  QRYBOL->F2_LOJA
			BOLETOS->F2_VALFAT 	:=  QRYBOL->F2_VALFAT
			BOLETOS->F2_CARGA   :=  QRYBOL->F2_CARGA
			BOLETOS->STATUS     := '0'
			BOLETOS->(MsUnLock())

		else
			RecLock("BOLETOS",.t.)
			BOLETOS->E1_NUM  := QRYBOL->E1_NUM
			BOLETOS->E1_PREFIXO  := QRYBOL->E1_PREFIXO
			BOLETOS->E1_CLIENTE  := QRYBOL->E1_CLIENTE
			BOLETOS->E1_LOJA  := QRYBOL->E1_LOJA
			BOLETOS->E1_NOMCLI  := QRYBOL->E1_NOMCLI
			BOLETOS->E1_PARCELA  := QRYBOL->E1_PARCELA
			BOLETOS->E1_TIPO  := QRYBOL->E1_TIPO
			BOLETOS->E1_SALDO  := QRYBOL->E1_SALDO
			BOLETOS->E1_NUMBCO  := QRYBOL->E1_NUMBCO
			BOLETOS->STATUS     := If(Empty(QRYBOL->E1_NUMBCO),'0','1')
			BOLETOS->E1_VENCREA  := STOD(QRYBOL->E1_VENCREA)
			BOLETOS->RECNOE1    := QRYBOL->E1RECNO
			BOLETOS->(MsUnLock())
		endif

		DbSelectArea("QRYBOL")
		DbSkip()
	Enddo

	aCores := {}
	aAdd(aCores,{"BOLETOS->STATUS == '0'","BR_VERDE"	})
	aAdd(aCores,{"BOLETOS->STATUS == '1'","BR_VERMELHO"})//Define quais colunas (campos da TTRB) serao exibidas na MsSelect

	if mv_par10 == 1
		aCpoBro	:= {{ "OK"					,, "Mark"      		       ,"@!"},;
		{ "F2_DOC"				,, "Nota Fiscal"           ,PesqPict("SF2","F2_DOC")},;
		{ "F2_SERIE"			,, "Serie"  	   		   ,PesqPict("SF2","F2_SERIE")},;
		{ "F2_CLIENTE"			,, "Cliente"    	       ,PesqPict("SF2","F2_CLIENTE")},;
		{ "F2_LOJA"				,, "Loja"				   ,PesqPict("SF2","F2_LOJA")},;
		{ "F2_VALFAT"			,, "Valor Faturado"		   ,PesqPict("SF2","F2_VALFAT")},;
		{ "F2_CARGA"			,, "Carga"				   ,PesqPict("SF2","F2_CARGA")},;
		{ "STATUS"				,, " "       ,"@!"}}

	else
		aCpoBro	:= {{ "OK"					,, "Mark"      		       ,"@!"},;
			{ "E1_NUM"				,, "Titulo"           		,PesqPict("SE1","E1_NUM")},;
			{ "E1_PREFIXO"			,, "Prefixo"  	   		    ,PesqPict("SE1","E1_PREFIXO")},;
			{ "E1_CLIENTE"			,, "Cliente"    	        ,PesqPict("SE1","E1_CLIENTE")},;
			{ "E1_LOJA"				,, "Loja"				    ,PesqPict("SE1","E1_LOJA")},;
			{ "E1_NOMCLI"			,, "Nome Cliente"		    ,PesqPict("SE1","E1_NOMCLI")},;
			{ "E1_PARCELA"			,, "Parcela"				,PesqPict("SE1","E1_PARCELA")},;
			{ "E1_SALDO"			,, "Saldo do Titulo"		,PesqPict("SE1","E1_SALDO")},;
			{ "E1_VENCREA"			,, "Venc.Real"				,PesqPict("SE1","E1_VENCREA")},;
			{ "E1_NUMBCO"			,, "Nosso Número"			,PesqPict("SE1","E1_NUMBCO")},;
			{ "STATUS"				,, " "       ,"@!"}}
	endif

	//Cria uma Dialog
	DEFINE MSDIALOG oDlg TITLE "Seleção de Boletos" From 9,0 To 615,1200 PIXEL
	DbSelectArea("BOLETOS")
	DbGotop()
	//Cria a MsSelect
	oMark := MsSelect():New("BOLETOS","OK","",aCpoBro,@lInverte,@cMark,{37,5,270,600},,,,,aCores)
	oMark:bMark := {| | AtuVlr()} //Exibe a Dialog
	oMark:oBrowse:bAllMark:={|| altInvG()}
	
	oSay1 := TSay():New( 290,10,{||"Total Selecionado: R$ "+Alltrim(Transform(nVlrSel,"@E 9,999,999.99"))},oDlg,,oFontNeg,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,299,008)
	oSay2 := TSay():New( 290,180,{||"Banco: "+Alltrim(mv_par01)+" / Agencia: "+Alltrim(mv_par02)+" / Conta: "+Alltrim(mv_par03)},oDlg,,oFontNeg,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,299,008)
	ACTIVATE MSDIALOG oDlg CENTERED ON INIT EnchoiceBar(oDlg,{|| (lImprime :=.t.,oDlg:End()) },{|| (lImprime:=.f., oDlg:End() )})
	
	if !lImprime
		return
	endif


	if mv_par10 ==1
		cCliente := BOLETOS->F2_CLIENTE
	else
		cCliente := BOLETOS->E1_CLIENTE
	endif
	
	BOLETOS->(DbGoTop())
	Do While !BOLETOS->(Eof())

		if mv_par10 == 1
			IncProc("Processando Nota: " + BOLETOS->F2_DOC + " - " + BOLETOS->F2_SERIE + " ...")
		else
			IncProc("Processando Nota: " + BOLETOS->E1_NUM + " - " + BOLETOS->E1_PREFIXO + " ...")
		endif
		IF !Marked("OK")
			DbSkip()
			Loop
		endif
		cLog:= if(mv_par10==1,BOLETOS->F2_DOC + " - " + BOLETOS->F2_SERIE + ": ",BOLETOS->E1_NUM + " - " + BOLETOS->E1_PREFIXO + ": ")

		If LastKey() == 286 .Or. LastKey() == 27
			@ prow()+1,001 say "CANCELADO PELO OPERADOR"
			Exit
		EndIf

		//	lContinua := AtuBanco(QRY->E1_CLIENTE,E1_LOJA)

		//	If lContinua

		if !empty( cCliente ) .and. cCliente <> iif(mv_par10==1,BOLETOS->F2_CLIENTE,BOLETOS->E1_CLIENTE)
			lUniCli := .f.
		endif
		cCliente:= iif(mv_par10==1,BOLETOS->F2_CLIENTE,BOLETOS->E1_CLIENTE)
		cLoja:= iif(mv_par10==1,BOLETOS->F2_LOJA,BOLETOS->E1_LOJA)

		DbSelectArea("SA1")
		DbSetOrder(1)
		If !DbSeek(xFilial() + cCliente + cLoja)
			cLog += "Cliente " + cCliente + " - " + cLoja + " não encontrado"
		Else
			// Customizado para não deixar gerar boleto à Vista, #Rafael Achôa 11-11-2014

			if mv_par10==1
				DbSelectArea("SE1")
				DbSetOrder(2)
				SE1->(DbSeek(xFilial("SE1") + cCliente + cLoja + BOLETOS->F2_SERIE + BOLETOS->F2_DOC))
				cExprLaco := ' !Eof() .And. xFilial("SE1") + "'+cCliente+cLoja +'" +  BOLETOS->F2_SERIE + BOLETOS->F2_DOC == SE1->(E1_FILIAL + E1_CLIENTE + E1_LOJA + E1_PREFIXO + E1_NUM) '
			else
				DbSelectArea("SE1")
				DbGoTo(BOLETOS->RECNOE1)	
				cExprLaco := '!lSai'		
			endif
			lSai := .f.
			Do While &(cExprLaco)

				lSai :=.t.
				
				IF (SE1->E1_EMISSAO >= SE1->E1_VENCTO)
					cLog += "Título NÃO IMPRESSO, por ser à Vista! (Parc: " +SE1->E1_PARCELA +")"
					aAdd(aLog, OemToAnsi(cLog))
					cLog := ""

					DbSelectArea("SE1")
					DbSkip()
					Loop
				Endif

				If !Empty(SE1->E1_PORTADO) .And. !('C' $ SE1->E1_PORTADO)
					If SE1->E1_PORTADO <> mv_par01
						cLog += "Portador do título " + SE1->E1_PORTADO + " diferente do parametrizado " + mv_par01

						DbSelectArea("SE1")
						DbSkip()
						Loop
					EndIf
				EndIf
				//alert('F2')
				If SE1->E1_SITUACA <> "0"
					cMsg := "ATENÇÃO: O título: " + SE1->E1_PREFIXO + " - " + SE1->E1_NUM + " / "
					cMsg += SE1->E1_PARCELA + " não está em carteira. Deseja continuar?"
					If !MsgBox(OemToAnsi(cMsg), "Mensagem", "YESNO")
						cLog += " Título não está em carteira (Parc: " + SE1->E1_PARCELA + ")"
						aAdd(aLog, OemToAnsi(cLog))
						cLog:= ""

						DbSelectArea("SE1")
						DbSkip()
						loop
					EndIf
				EndIf

				If !Empty(SE1->E1_NUMBCO)
					cMsg := "ATENÇÃO: Já foi impresso o boleto do título: " + SE1->E1_PREFIXO
					cMsg += " - " + SE1->E1_NUM + " / " + SE1->E1_PARCELA + ". Deseja continuar?"
					If !MsgBox(OemToAnsi(cMsg), "Mensagem", "YESNO")
						cLog += " Já impresso - cancelado pelo usuário (Parc: " + SE1->E1_PARCELA + ")"
						aAdd(aLog, OemToAnsi(cLog))
						cLog:= ""

						DbSelectArea("SE1")
						DbSkip()
						Loop
					EndIf
				EndIf


				DbSelectArea("SEE")
				DbSetOrder(1)
				DbSeek(xFilial("SEE")+Pad(cBcSel,3) + Pad(cAgSel,9) + Pad(cCtSel,10)+'001')

				Do Case	
					Case SA6->A6_COD == "237" // Bradesco
					u_Bol_Brad()

					Case SA6->A6_COD == "084" // Uniprime
					u_Bol_Unip()
					
					Case SA6->A6_COD == "422" // safra
					u_Bol_safra()
					
					Case SA6->A6_COD == "341" // Itaú
					Bol_Itau()

					Case SA6->A6_COD == "033" // Santander
					Bol_Bane()

					Case SA6->A6_COD == "399" // HSBC
					Bol_HSBC()

					Case SA6->A6_COD == "001" // Banco do Brasil
					Bol_BB()

					Case SA6->A6_COD == "748" // Sicred
					Bol_Sicred()

					Case SA6->A6_COD == "104" // CEF
					Bol_CEF()

					Case SA6->A6_COD == "246" // ABC Brasil
					Bol_ABC()
				EndCase

				If Empty(SE1->E1_NUMBCO)
					GravaSE1()
					cLog += " OK! Impresso boleto com o novo NN: " + cNossoNum + " (Parc: " + SE1->E1_PARCELA + ")"
					aAdd(aLog, OemToAnsi(cLog))
					cLog:= ""
				Else
					cLog += " OK! Impresso boleto com o mesmo NN: " + cNossoNum + " (Parc: " + SE1->E1_PARCELA + ")"
					aAdd(aLog, OemToAnsi(cLog))
					cLog:= ""
				EndIf

				DbSelectArea("SE1")
				DbSkip()
			Enddo

		Endif

		If !Empty(cLog)
			aAdd(aLog, OemToAnsi(cLog))
		EndIf

		DbSelectArea("BOLETOS")
		BOLETOS->(DbSkip())

	EndDo

	oPrint:EndPage()     // Finaliza a página

	oPrint:Preview()     // Visualiza antes de imprimir

Return

Static Function GravaSE1()
	// cNNLimpo:= cNossoNum
	// cNNLimpo:= StrTran(cNNLimpo, "/", "")
	// cNNLimpo:= StrTran(cNNLimpo, "-", "")

	
	// //Gravar campos customizados de banco, agencia e conta 
	// //Pois com operações de desconto e caução, os campos de E1_PORTADO, E1_AGEDEP e E1_CONTA mudam e perde-se
	// //o Histórico
	// DbSelectArea("SE1")
	// RecLock("SE1", .f.)
	// Replace E1_PORTADO	With mv_par01
	// Replace E1_AGEDEP	With mv_par02
	// Replace E1_CONTA	With mv_par03
	// Replace E1_X_BANCO  With mv_par01
	// Replace E1_X_AGENC  With mv_par02
	// Replace E1_X_CONTA  With mv_par03
	// Replace E1_OCORREN  With '01'
	// Replace E1_NUMBCO	With cNNLimpo
	// Replace E1_X_NOSSO  With cNNLimpo
	// MsUnlock()
Return


Static Function FatVencto()
return	if(SE1->E1_VENCTO >= CTOD('22/02/2025'), SE1->E1_VENCTO - CTOD('29/05/2022'), SE1->E1_VENCTO - CTOD('07/10/1997') )
//Return SE1->E1_VENCTO - CTOD('07/10/1997')

Static Function Fat2Vencto()
//Return SE1->E1_VENCTO - CTOD('07/10/1997')
return	if(SE1->E1_VENCTO >= CTOD('22/02/2025'), SE1->E1_VENCTO - CTOD('29/05/2022'), SE1->E1_VENCTO - CTOD('07/10/1997') )

/******************************************************************************************************************/
Static Function FormatData(cData)
	/******************************************************************************************************************/
Return Substr(cData, 7, 2) + '/' + Substr(cData, 5, 2) + '/' + Substr(cData, 1, 4)

/******************************************************************************************************************/
Static Function Resultado()
	/******************************************************************************************************************/
	Local n
	aHeader:= {}
	aCols:= {}
	
	cCampo := "Z04_OBS"
	AADD(aHeader, {alltrim(GetSx3Cache(cCampo,"X3_TITULO")),;
	GetSx3Cache(cCampo,"X3_CAMPO") ,;
	GetSx3Cache(cCampo,"X3_PICTURE") ,;
	GetSx3Cache(cCampo,"X3_TAMANHO"),;
	GetSx3Cache(cCampo,"X3_DECIMAL") ,; 
	".F.",;
	GetSx3Cache(cCampo,"X3_USADO") ,;
	GetSx3Cache(cCampo,"X3_TIPO") ,;
	GetSx3Cache(cCampo,"X3_ARQUIVO") } )
	
	//DbSelectArea("SX3")
	//DbSetOrder(2)
	//DbSeek("Z04_OBS")
	//aAdd(aHeader,{X3_TITULO, X3_CAMPO, X3_PICTURE,X3_TAMANHO, X3_DECIMAL,".f.",X3_USADO, X3_TIPO, X3_ARQUIVO})

	For n:= 1 To Len(aLog)
		aAdd(aCols, {aLog[n]})
	Next n
	If Len(aCols) == 0
		aCols:= {{''}}
	EndIf
	n:= 1

	@ 000,000 TO 540,500 DIALOG fResultado TITLE "Resultados do Processamento"
	@ 003,006 TO 250,246
	@ 012,012 TO 245,240 MULTILINE OBJECT oResultado
	oResultado:oBrowse:bLDBLClick:= {|| }
	@ 255,210 BMPBUTTON TYPE 1 ACTION fResultado:End()
	ACTIVATE DIALOG fResultado CENTERED

Return

/******************************************/
// Rotinas de impressao do boleto do ITAU //
/******************************************/
/******************************************************************************************************************/
Static Function Bol_Itau()
	/******************************************************************************************************************/
	Local oFont8
	Local oFont9  //Customizado para criação de novo tamanho de fonte, #Rafael Achôa 14-05-2014
	Local oFont10
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	//Local cInst01 := mv_par10+mv_par11
	//Local cInst02 := mv_par12+mv_par13
	//Local cInst03 := mv_par14+mv_par15
	Local cBitmapBco:= "\system\LGITAU.bmp"
	Local i

	cForm1:= ''
	cForm2:= ''
	cForm3:= ''

	nValor:= IIf(!Empty(SE1->E1_SALDO),Round(SE1->E1_SALDO, 2),Round(SE1->E1_VALOR, 2))
	// msginfo(SE1->E1_SALDO,"saldo")  //Customizado para não aparecer mais o Saldo do Título antes de imprimir o Relatório #Rafael Achôa, 15/05/2014
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_Itau()
	Else
		//cNossoNum:= transform( SE1->E1_NUMBCO, '@R 999/99999999-9' )
		cNossoNum:= ALLTRIM(transform( SE1->E1_NUMBCO, '@R 999/9999999999' ))
		cNossoNum:= substr(cNossoNum,1,len(cNossoNum)-1)+'-'+substr(cNossoNum,len(cNossoNum),1)
	EndIf

	//If Empty(SE1->E1_CODBAR) // Customizado para verificar se já não existe o código de barras daquele título, #Rafael Achôa, 15/05/2014
	CdBarra_Itau()
	//Endif

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont9  := TFont():New("Arial",9,07,.T.,.T.,5,.T.,5,.T.,.F.) //Customizado para criação de novo tamanho de fonte, #Rafael Achôa 14-05-2014
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	If File(cBitmapBco)
		oPrint:SayBitmap( 0070, 0100, cBitmapBco, 0355, 0060 )
	Else
		oPrint:Say  (0084,100,'Itaú S.A.',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0062,567,'341-7',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0200,100 ,Substr(alltrim(SM0->M0_NOMECOM),1,30)+" - "+transform( SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	oPrint:Say  (0150,1060,"Agência/Código Beneficiário"                         ,oFont8)
	cCedente:= cAgeEmp + '/' + cCtaEmp + '-' + cDigEmp
	oPrint:Say  (0200,1060,cCedente                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Pagador"                                         ,oFont8)
	oPrint:Say  (0300,100 ,SubStr(AllTrim(SA1->A1_NOME),1,30) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1410,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1410,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)
	If File(cBitmapBco)
		oPrint:SayBitmap( 0630, 0100, cBitmapBco, 0355, 0060 )
	Else
		oPrint:Say  (0644,100,'Itaú S.A.'   ,oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0622,567,'341-7'       ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Pagador",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	//oPrint:Say  (0750,100 ,"QUALQUER BANCO ATÉ A DATA DO VENCIMENTO"        ,oFont10)
	oPrint:Say  (0750,100 ,"Até o vencimento, preferencialmente no Itaú. Após o vencimento, somente no Itaú."        ,oFont10)

	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2031,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0850,100 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" || "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont9) //Nome + CNPJ      //Customizado para Nome + Endereço completo + CNPJ, tamanho 9 #Rafael Achôa, 14-05-2014

	oPrint:Say  (0810,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (0850,1995,cCedente											,oFont10)

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                             ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	//oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	//oPrint:Say  (0940,2010,Padl(cNossoNum, 18)				                ,oFont10)

	oPrint:Say  (910,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (0940,1960,Padl(cNossoNum, 14)				                ,oFont10)

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (1010,555 ,'00' + cCartEmp                                	,oFont10)

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,Padl(AllTrim(cValor),16) 									,oFont10)

	oPrint:Say  (1050,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções
	/*cTexto := cInst01
	oPrint:Say  (1100,100 ,cTexto                                           ,oFont10)
	cTexto := cInst02
	oPrint:Say  (1150,100 ,cTexto                                           ,oFont10)
	cTexto := cInst03+CHR(10)+CHR(13)+cInst04
	oPrint:Say  (1200,100 ,cTexto		                                    ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1300,100 ,cTexto                                           ,oFont10)
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif */
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (1605,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)

	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	// Encerra aqui a alteracao para o novo layout - RAI

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)
	If File (cBitmapBco)
		oPrint:SayBitmap( 1920, 0100, cBitmapBco, 0355, 0060 )
	Else
		oPrint:Say  (1934,100,'Itaú S.A.',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (1912,567,'341-7'   ,oFont24 )	// [1]Numero do Banco
	// MsgInfo(cRepNum,'cRepNum') //Customizado para não aparecer mensagem de informação com o Número da linha digitável do Código de Barras #Rafael Achôa, 15/05/2014
	oPrint:Say  (1934,820,/*cRepNum  */Alltrim(SE1->E1_CODDIG) ,oFont14n)	//Linha Digitavel do Codigo de Barras  //Customizado para trazer direto da tabela de Títulos. #Rafael Achôa, 15/05/2014

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"QUALQUER BANCO ATÉ A DATA DO VENCIMENTO"        ,oFont10)

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (2140,100 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" || "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99')/*alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99')*/,oFont9) //Nome + CNPJ  //Customizado para Nome + Endereço completo + CNPJ, tamanho 9 #Rafael Achôa, 14-05-2014

	oPrint:Say  (2100,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedente											,oFont10)

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,1980,Padl(cNossoNum, 18)                              ,oFont10)

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (2300,555 ,"00" + cCartEmp                                 	,oFont10)

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	/*cTexto := cInst01
	oPrint:Say  (2390,100 ,cTexto                                           ,oFont10)
	cTexto := cInst02
	oPrint:Say  (2440,100 ,cTexto                                           ,oFont10)
	cTexto := cInst03+CHR(10)+CHR(13)+cInst04
	oPrint:Say  (2540,100 ,cTexto                                           ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2590,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (2895,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	If cLayOut == "LASER"
		MSBAR("INT25",26,1.5,/*cCdBarra*/SE1->E1_CODBAR,oPrint,.F.,,,,1.2,,,,.F.) //Customizado para pegar Código de Barras direto do título #Rafael Achôa, 15/05/2014
	Else
		MSBAR3("INT25",23.4,2.0,/*cCdBarra*/SE1->E1_CODBAR,oPrint,.F.,,,,1.3,,,,.F.)  //Customizado para pegar Código de Barras direto do título #Rafael Achôa, 15/05/2014
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/******************************************************************************************************************/
Static Function RetNN_Itau()
	/******************************************************************************************************************/
	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 8)
	//Local cA6__NOSSO:= Right(AllTrim(SEE->EE_FAXATU),8)
	Local cTexto:= ""

	cTexto:= cAgeEmp + cCtaEmp + cCartEmp + cA6__NOSSO
	cRet:= cCartEmp + "/" + cA6__NOSSO + "-" + Modu10(cTexto)

	/* Atualizar o nosso numero no cadastro do banco */
	DbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	MsUnlock()

Return cRet

/******************************************************************************************************************/
Static Function Modu10(cLinha)
/******************************************************************************************************************/
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local cDigRet
	Local nResult
	Local lDobra:= .f.
	Local cValor
	Local nAux

	For nCont:= Len(cLinha) To 1 Step -1
		lDobra:= !lDobra

		If lDobra
			cValor:= AllTrim(Str(Val(Substr(cLinha, nCont, 1)) * 2))
		Else
			cValor:= AllTrim(Str(Val(Substr(cLinha, nCont, 1))))
		EndIf

		For nAux:= 1 To Len(cValor)
			nSoma += Val(Substr(cValor, nAux, 1))
		Next n
	Next nCont

	nResto:= MOD(nSoma, 10)

	nResult:= 10 - nResto

	If nResult == 10
		cDigRet:= "0"
	Else
		cDigRet:= StrZero(10 - nResto, 1)
	EndIf

Return cDigRet

/******************************************************************************************************************/
Static Function Modu11(cLinha)
	/******************************************************************************************************************/
	Local cDigRet
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local nFator:= 9
	Local nResult

	For nCont:= Len(cLinha) TO 1 Step -1
		nFator++
		If nFator > 9
			nFator:= 2
		EndIf

		nSoma += Val(Substr(cLinha, nCont, 1)) * nFator
	Next nCont

	nResto:= Mod(nSoma, 11)

	nResult:= 11 - nResto

	If nResult == 0 .Or. nResult == 1 .Or. nResult == 10 .Or. nResult == 11
		cDigRet:= "1"
	Else
		cDigRet:= StrZero(11 - nResto, 1)
	EndIf

Return cDigRet

/******************************************************************************************************************/
Static Function CdBarra_Itau()
	/******************************************************************************************************************/
	Local cDigCdBarra
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCampo1:= ""
	Local cCampo2:= ""
	Local cCampo3:= ""
	Local cCampo4:= ""
	Local cCampo5:= ""

	cFatVencto:= StrZero(FatVencto(), 4)
	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= StrZero(nValor * 100, 10)

	/* Calculo do codigo de barras */
	cCdBarra:= SA6->A6_COD + "9" + cFatVencto + cValor + cCartEmp + Substr(cNossoNum, 5, 8) + Substr(cNossoNum, 14, 1) +;
	cAgeEmp + cCtaEmp + cDigEmp + "000"

	cDigCdBarra:= Modu11(cCdBarra)

	cCdBarra:= SA6->A6_COD + "9" + cDigCdBarra + StrZero(FatVencto(), 4) + StrZero(Int(SE1->E1_SALDO * 100), 10) + cCartEmp + ;
	Substr(cNossoNum, 5, 8) + Substr(cNossoNum, 14, 1) + cAgeEmp + cCtaEmp + cDigEmp + "000"

	//Alert (len(cCdBarra))

	/* Calculo da representacao numerica */
	cCampo1:= "341" + "9" + cCartEmp + Substr(cNossoNum, 5, 2)
	cCampo2:= Substr(cNossoNum, 7, 6) + Substr(cNossoNum, 14, 1) + Substr(cAgeEmp, 1, 3)
	cCampo3:= Substr(cAgeEmp, 4, 1) + cCtaEmp + cDigEmp + "000"
	cCampo4:= Substr(cCdBarra, 5, 1)
	cCampo5:= cFatVencto + cValor

	/* Calculando os DACs dos campos 1, 2 e 3 */
	cCampo1:= cCampo1 + Modu10(cCampo1)
	cCampo2:= cCampo2 + Modu10(cCampo2)
	cCampo3:= cCampo3 + Modu10(cCampo3)

	cRepNum := Substr(cCampo1, 1, 5) + "." + Substr(cCampo1, 6, 5) + "  "
	cRepNum += Substr(cCampo2, 1, 5) + "." + Substr(cCampo2, 6, 6) + "  "
	cRepNum += Substr(cCampo3, 1, 5) + "." + Substr(cCampo3, 6, 6) + "  "
	cRepNum += cCampo4 + "  "
	cRepNum += cCampo5

	RecLock("SE1",.F.)                                                          // Customizado para guardar o código de barras e sua linha digitável, #Rafael Achôa - 15/05/2014
	Replace E1_CODBAR With cCdBarra
	Replace E1_CODDIG With cRepNum
	MsUnlock()

Return

/**********************************************/
// Rotinas de impressao do boleto do Bradesco //
/**********************************************/
User Function Bol_Brad()
	Local oFont8
	Local oFont10
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	Local cBitmapBco:= "\system\bradesco.bmp"
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	Local i

	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= U_NumDoc()

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_Brad()
	Else
		cNossoNum:= SE1->E1_NUMBCO
	EndIf

	CdBarra_Brad()

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	//Customizado para trazer Logo do Bradesco, caso exista, #Rafael Achôa - 24-07-2014
	If File(cBitmapBco)
		oPrint:SayBitmap( 0045, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (0084,100,'Bradesco',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0062,567,'237-2',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Beneficiário",oFont8)
	If Empty(cCorrNom) // Não possui correspondente
		oPrint:Say  (0180,100 ,alltrim( SM0->M0_NOMECOM ),oFont10) //Nome + CNPJ
		oPrint:Say  (0210,100 ,transform( SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (0180,100 ,alltrim(cCorrNom),oFont10)
		oPrint:Say  (0210,100 ,cCorrCNPJ,oFont10) //Nome + CNPJ
	EndIf

	oPrint:Say  (0150,1060,"Agência/Código Beneficiário"                         ,oFont8)
	cCedente:= cAgeEmp+"-"+cDigAge+"/"+cCtaEmp+"-"+cDigEmp
	oPrint:Say  (0200,1060,cCedente                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Pagador"                                         ,oFont8)
	oPrint:Say  (0300,100 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1410,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1410,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)
	//Customizado para trazer Logo do Bradesco, caso exista, #Rafael Achôa - 24-07-2014
	If File(cBitmapBco)
		oPrint:SayBitmap( 0605, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (0644,100,'Bradesco'    ,oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0622,567,'237-2'       ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Pagador",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (0750,100 ,"Pagável preferencialmente na Rede Bradesco ou Bradesco Expresso"        ,oFont10)  // Customizado para novo layout do Bradesco, #Rafael Achôa - 26-08-2014

	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2010,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Beneficiário"                                        ,oFont8)
	If Empty(cCorrNom)
		oPrint:Say  (0850,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (0820,250 ,alltrim(cCorrNom)+ " - "+cCorrCNPJ,oFont10)
		oPrint:Say  (0860,250 ,alltrim(cCorrEnd),oFont10)
	EndIf

	oPrint:Say  (0810,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (0850,2010,cCedente											,oFont10)			// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                             ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (0940,2010,cCartEmp+"/"+Transform(cNossoNum, '@R 99999999999-X'),oFont10)

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (1010,555 , cCartEmp	                                	,oFont10)

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (1050,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções

	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1300,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	If Empty(cCorrNom) // Não possui correspondente
		oPrint:Say  (1605,100 ,"Pagador/Avalista"                               ,oFont8)
	Else
		oPrint:Say  (1605,100 ,"Sacador/Avalista"                               ,oFont8)
	EndIF

	If !Empty(cCorrNom) // Possui correspondente
		oPrint:Say  (1605,300 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_BAIRCOB)+" - "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" - "+Alltrim(SM0->M0_CEPCOB)+" CNPJ: "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont8)
	EndIf

	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)
	If File (cBitmapBco)
		oPrint:SayBitmap( 1895, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (1934,100,'Bradesco',oFont16 )	// [2]Nome do Banco
	Endif

	oPrint:Say  (1912,567,'237-2'   ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (1934,820,cRepNum   ,oFont14n)	//Linha Digitavel do Codigo de Barras

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"Pagável preferencialmente na Rede Bradesco ou Bradesco Expresso"        ,oFont10) 	 // Customizado para novo layout do Bradesco, #Rafael Achôa - 26-08-2014

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Beneficiário"                                        ,oFont8)
	If Empty(cCorrNom)
		oPrint:Say  (2140,100 ,alltrim(SM0->M0_NOMECOM)+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (2110,250 ,alltrim(cCorrNom)+ " - " + cCorrCNPJ,oFont10)
		oPrint:Say  (2150,250 ,alltrim(cCorrEnd),oFont10)
	EndIf

	oPrint:Say  (2100,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedente											,oFont10)		// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,2010,cCartEmp+"/"+Transform(cNossoNum, '@R 99999999999-X'),oFont10)

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (2300,555 , cCartEmp       		                          	,oFont10)

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2590,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	If Empty(cCorrNom) // Não possui correspondente
		oPrint:Say  (2895,100 ,"Pagador/Avalista"                               ,oFont8)
	Else
		oPrint:Say  (2895,100 ,"Sacador/Avalista"                               ,oFont8)
	EndIf

	If !Empty(cCorrNom) // Possui correspondente
		oPrint:Say  (2895,300 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_BAIRCOB)+" - "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" - "+Alltrim(SM0->M0_CEPCOB)+" CNPJ: "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont8)
	EndIf

	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	If cLayOut == "LASER"
		MSBAR("INT25",26,1.5,cCdBarra,oPrint,.F.,,,,1.2,,,,.F.)
	Else
		MSBAR3("INT25",23.4,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/**********************************************/
// Rotinas de impressao do boleto do Uniprime //
/**********************************************/
User Function Bol_Unip()
	Local oFont8
	Local oFont10
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	Local cBitmapBco:= "\system\Uniprime.bmp"
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	Local i

	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= U_NumDoc()

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_Uni()
	Else
		cNossoNum:= SE1->E1_NUMBCO
	EndIf

	CdBarra_Unip()

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	//Customizado para trazer Logo do Uniprime, caso exista, #Rafael Achôa - 24-07-2014
	If File(cBitmapBco)
		oPrint:SayBitmap( 0045, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (0084,100,'Uniprime',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0062,567,'084-1',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Beneficiário",oFont8)
	If Empty(cCorrNom) // Não possui correspondente
		oPrint:Say  (0180,100 ,alltrim( SM0->M0_NOMECOM ),oFont10) //Nome + CNPJ
		oPrint:Say  (0210,100 ,transform( SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (0180,100 ,alltrim(cCorrNom),oFont10)
		oPrint:Say  (0210,100 ,cCorrCNPJ,oFont10) //Nome + CNPJ
	EndIf

	oPrint:Say  (0150,1060,"Agência/Código Beneficiário"                         ,oFont8)
	//cCedente:= cAgeEmp+"-"+cDigAge+"/"+PadL(Substr(cCtaEmp,2,5),7,"0") + "-" +Substr(cCtaEmp,7,1) //+"-"+cDigEmp  20/08/24
	cCedente:= cAgeEmp+"-"+cDigAge+"/"+PadL(Substr(cCtaEmp,1,6),7,"0") + "-" +Substr(cCtaEmp,7,1) //+"-"+cDigEmp
	//PadL(cCedenteSi,5,"0")
	oPrint:Say  (0200,1060,cCedente                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Pagador"                                         ,oFont8)
	oPrint:Say  (0300,100 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1410,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1410,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)
	//Customizado para trazer Logo do Uniprime, caso exista, #Rafael Achôa - 24-07-2014
	If File(cBitmapBco)
		oPrint:SayBitmap( 0605, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (0644,100,'Uniprime'    ,oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0622,567,'084-1'       ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Pagador",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (0750,100 ,"Pagável em qualquer banco até o vencimento"        ,oFont10) 

	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2010,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Beneficiário"                                        ,oFont8)
	If Empty(cCorrNom)
		oPrint:Say  (0850,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (0820,250 ,alltrim(cCorrNom)+ " - "+cCorrCNPJ,oFont10)
		oPrint:Say  (0860,250 ,alltrim(cCorrEnd),oFont10)
	EndIf

	oPrint:Say  (0810,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (0850,2010,cCedente											,oFont10)			// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                             ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (0940,2010,"0"+ cCartEmp+"/"+Transform(cNossoNum, '@R 99999999999-X'),oFont10)

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (1010,555 ,"0"+ cCartEmp	                                	,oFont10)

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (1050,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções

	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1300,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	If Empty(cCorrNom) // Não possui correspondente
		oPrint:Say  (1605,100 ,"Pagador/Avalista"                               ,oFont8)
	Else
		oPrint:Say  (1605,100 ,"Sacador/Avalista"                               ,oFont8)
	EndIF

	If !Empty(cCorrNom) // Possui correspondente
		oPrint:Say  (1605,300 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_BAIRCOB)+" - "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" - "+Alltrim(SM0->M0_CEPCOB)+" CNPJ: "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont8)
	EndIf

	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)
	If File (cBitmapBco)
		oPrint:SayBitmap( 1895, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (1934,100,'Uniprime',oFont16 )	// [2]Nome do Banco
	Endif

	oPrint:Say  (1912,567,'084-1'   ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (1934,820,cRepNum   ,oFont14n)	//Linha Digitavel do Codigo de Barras

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"Pagável em qualquer banco até o vencimento"     ,oFont10) 	 

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Beneficiário"                                        ,oFont8)
	If Empty(cCorrNom)
		oPrint:Say  (2140,100 ,alltrim(SM0->M0_NOMECOM)+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (2110,250 ,alltrim(cCorrNom)+ " - " + cCorrCNPJ,oFont10)
		oPrint:Say  (2150,250 ,alltrim(cCorrEnd),oFont10)
	EndIf

	oPrint:Say  (2100,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedente											,oFont10)		// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,2010,"0"+ cCartEmp+"/"+Transform(cNossoNum, '@R 99999999999-X'),oFont10)

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (2300,555 ,"0"+ cCartEmp       		                          	,oFont10)

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2590,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	If Empty(cCorrNom) // Não possui correspondente
		oPrint:Say  (2895,100 ,"Pagador/Avalista"                               ,oFont8)
	Else
		oPrint:Say  (2895,100 ,"Sacador/Avalista"                               ,oFont8)
	EndIf

	If !Empty(cCorrNom) // Possui correspondente
		oPrint:Say  (2895,300 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_BAIRCOB)+" - "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" - "+Alltrim(SM0->M0_CEPCOB)+" CNPJ: "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont8)
	EndIf

	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	If cLayOut == "LASER"	
		MSBAR("INT25",26,1.5,cCdBarra,oPrint,.F.,,,,1.2,,,,.F.)
	Else
		MSBAR3("INT25",23.4,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	EndIf

	oPrint:EndPage() // Finaliza a página
Return

Static Function RetNN_Brad()
	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 11)
	Local cTexto:= ""
	cTexto:= cCartEmp + cA6__NOSSO
	cRet:= cA6__NOSSO + Modu11_Brad(cTexto)

	DbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	MsUnlock()
Return cRet

Static Function RetNN_Uni()
	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 11)
	Local cTexto:= ""
	cTexto:= cCartEmp + cA6__NOSSO
	cRet:= cA6__NOSSO + Modu11_Unip(cTexto)

	DbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	MsUnlock()
Return cRet


/******************************************************************************************************************/
Static Function Modu11_Brad(cdata)
	/******************************************************************************************************************/
	Local cDigRet
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local nFator:= 7
	Local nResult

	LOCAL L, D, P := 0
	L := Len(cdata)
	D := 0
	P := 1
	DV:= " "

	While L > 0
		P := P + 1
		D := D + (Val(SubStr(cData, L, 1)) * P)
		If P = 7   //Volta para o inicio, ou seja comeca a multiplicar por 2,3,4...
			P := 1
		End
		L := L - 1
	End
	_nResto := mod(D,11)  //Resto da Divisao
	//D := 11 - (mod(D,11)) // Diferenca 11 (-) Resto da Divisao
	D := 11 - _nResto
	DV:=STR(D)

	If _nResto = 0
		DV := "0"
	End
	If _nResto = 1
		DV := "P"
	End

Return(alltrim(DV))

/******************************************************************************************************************/
Static Function Modu11_Unip(cdata)
	/******************************************************************************************************************/
	Local cDigRet
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local nFator:= 7
	Local nResult

	LOCAL L, D, P := 0
	L := Len(cdata)
	D := 0
	P := 1
	DV:= " "

	While L > 0
		P := P + 1
		D := D + (Val(SubStr(cData, L, 1)) * P)
		If P = 7   //Volta para o inicio, ou seja comeca a multiplicar por 2,3,4...
			P := 1
		End
		L := L - 1
	End
	_nResto := mod(D,11)  //Resto da Divisao
	//D := 11 - (mod(D,11)) // Diferenca 11 (-) Resto da Divisao
	D := 11 - _nResto
	DV:=STR(D)

	If _nResto = 0
		DV := "0"
	End
	If _nResto = 1
		DV := "P"
	End

Return(alltrim(DV))

/*For nCont:= Len(cLinha) TO 1 Step -1
nFator++
If nFator > 7
nFator:= 2
EndIf

nSoma += Val(Substr(cLinha, nCont, 1)) * nFator
Next nCont

nResto:= Mod(nSoma, 11)

If nResto == 1
cDigRet:= "P"
ElseIf nResto == 0
cDigRet:= "0"
Else
nResult:= 11 - nResto
cDigRet:= StrZero(nResult, 1)
EndIf

Return cDigRet */

Static Function CdBarra_Brad()
	Local cDigCdBarra
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCampo1:= ""
	Local cCampo2:= ""
	Local cCampo3:= ""
	Local cCampo4:= ""
	Local cCampo5:= ""
	Local cCpoLivre:= ""

	cFatVencto:= StrZero(FatVencto(), 4)
	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= StrZero(nValor * 100, 10)

	cCpoLivre:= Left(cAgeEmp, 4) + cCartEmp + Substr(cNossoNum, /*4*/1, 11) + Right(cCtaEmp, 7) + "0"

	/* Calculo do codigo de barras */
	cCdBarra:= SA6->A6_COD + "9" + cFatVencto + cValor + cCpoLivre
	cDigCdBarra:= Modu11(cCdBarra)
	cCdBarra:= SA6->A6_COD + "9" + cDigCdBarra + cFatVencto + cValor + cCpoLivre

	/* Calculo da representacao numerica */
	cCampo1:= "237" + "9" + Left(cCpoLivre, 5)
	cCampo2:= Substr(cCpoLivre, 6, 10)
	cCampo3:= Substr(cCpoLivre, 16, 10)
	cCampo4:= Substr(cCdBarra, 5, 1)
	cCampo5:= cFatVencto + cValor

	/* Calculando os DACs dos campos 1, 2 e 3 */
	cCampo1:= cCampo1 + Modu10(cCampo1)
	cCampo2:= cCampo2 + Modu10(cCampo2)
	cCampo3:= cCampo3 + Modu10(cCampo3)

	cRepNum := Substr(cCampo1, 1, 5) + "." + Substr(cCampo1, 6, 5) + "  "
	cRepNum += Substr(cCampo2, 1, 5) + "." + Substr(cCampo2, 6, 6) + "  "
	cRepNum += Substr(cCampo3, 1, 5) + "." + Substr(cCampo3, 6, 6) + "  "
	cRepNum += cCampo4 + "  "
	cRepNum += cCampo5

Return

Static Function CdBarra_Unip()
	Local cDigCdBarra
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCampo1:= ""
	Local cCampo2:= ""
	Local cCampo3:= ""
	Local cCampo4:= ""
	Local cCampo5:= ""
	Local cCpoLivre:= ""

	cFatVencto:= StrZero(FatVencto(), 4)
	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= StrZero(nValor * 100, 10)

	//cCpoLivre:= Left(cAgeEmp, 4) + cCartEmp + Substr(cNossoNum, /*4*/1, 11) + Right(cCtaEmp, 7) + "0"
	//cCpoLivre:= Left(cAgeEmp, 4) + "0" + Substr(cCartEmp,2,2) + Substr(cNossoNum,1,11) + PadL(Substr(cCtaEmp,2,5),7,'0') + "0" 
	//cCpoLivre:= Left(cAgeEmp, 4) + cCartEmp + Substr(cNossoNum,1,11) + PadL(Substr(cCtaEmp,2,5),7,'0') + "0"  alterado 20/08/24
	cCpoLivre:= Left(cAgeEmp, 4) + cCartEmp + Substr(cNossoNum,1,11) + PadL(Substr(cCtaEmp,1,6),7,'0') + "0"

	/* Calculo do codigo de barras */
	cCdBarra:= SA6->A6_COD + "9" + cFatVencto + cValor + cCpoLivre
	cDigCdBarra:= Modu11(cCdBarra)
	cCdBarra:= SA6->A6_COD + "9" + cDigCdBarra + cFatVencto + cValor + cCpoLivre

	/* Calculo da representacao numerica */
	cCampo1:= "084" + "9" + Left(cCpoLivre, 5)
	cCampo2:= Substr(cCpoLivre, 6, 10)
	cCampo3:= Substr(cCpoLivre, 16, 10)
	cCampo4:= Substr(cCdBarra, 5, 1)
	cCampo5:= cFatVencto + cValor

	/* Calculando os DACs dos campos 1, 2 e 3 */
	cCampo1:= cCampo1 + Modu10(cCampo1)
	cCampo2:= cCampo2 + Modu10(cCampo2)
	cCampo3:= cCampo3 + Modu10(cCampo3)

	cRepNum := Substr(cCampo1, 1, 5) + "." + Substr(cCampo1, 6, 5) + "  "
	cRepNum += Substr(cCampo2, 1, 5) + "." + Substr(cCampo2, 6, 6) + "  "
	cRepNum += Substr(cCampo3, 1, 5) + "." + Substr(cCampo3, 6, 6) + "  "
	cRepNum += cCampo4 + "  "
	cRepNum += cCampo5
Return

Static Function CdBarra_Safra()
	Local cDigCdBarra
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCampo1:= ""
	Local cCampo2:= ""
	Local cCampo3:= ""
	Local cCampo4:= ""
	Local cCampo5:= ""
	Local cCpoLivre:= ""
	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 9)

	cFatVencto:= StrZero(Fat2Vencto(), 4)
	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= StrZero(nValor * 100, 10)

	//Nosso Numero sem digito
	cNNumSDig := strzero(val(cA6__NOSSO),9)
	//Nosso Numero
	cNNum := cNNumSDig //+ " "//Modu11_Safra(cNNumSDig,'422')
	//Nosso Numero para impressao
	cNossoNum := cNNumSDig //+"-"+ Modu11_Safra(cNNumSDig,'422')
	
	cCpoLivre := "7"+StrZero(Val(SEE->EE_AGENCIA),5) + StrZero(Val(SEE->EE_CONTA),9)+cNNum+"2"

	/* Calculo do codigo de barras */
	cCdBarra:= SEE->EE_CODIGO + "9" + cFatVencto + cValor + cCpoLivre
	cDigCdBarra:= cvaltochar(Modu11_Safra(SEE->EE_CODIGO + "9" + cFatVencto + cValor + cCpoLivre)) //Modu11(cCdBarra)
	cCdBarra:= SEE->EE_CODIGO + "9" + cDigCdBarra + cFatVencto + cValor + cCpoLivre
	//alert(cCdBarra)
	
	/* Calculo da representacao numerica */
	cCampo1:= "422" + "9" + Left(cCpoLivre, 5)
	//ALERT(cCampo1)
	cCampo2:= Substr(cCpoLivre, 6, 10)
	//ALERT(cCampo2)
	cCampo3:= Substr(cCpoLivre, 16, 10)
	//ALERT(cCampo3)
	
	cCampo4:= Substr(cCdBarra, 5, 1)
	//ALERT(cCampo4)
	
	cCampo5:= cFatVencto + cValor
	

	/* Calculando os DACs dos campos 1, 2 e 3 */
	cCampo1:= cCampo1 + Modu10(cCampo1)
	cCampo2:= cCampo2 + Modu10(cCampo2)
	cCampo3:= cCampo3 + Modu10(cCampo3)
	
	//ALERT(cCampo1)
	//ALERT(cCampo2)
	//ALERT(cCampo3)
	//ALERT(cCampo4)
	//ALERT(cCampo5)
	
	cRepNum := Substr(cCampo1, 1, 5) + "." + Substr(cCampo1, 6, 5) + "  "
	cRepNum += Substr(cCampo2, 1, 5) + "." + Substr(cCampo2, 6, 6) + "  "
	cRepNum += Substr(cCampo3, 1, 5) + "." + Substr(cCampo3, 6, 6) + "  " + Substr(cCampo3, 12, 1)
	cRepNum += cCampo4 + " "
	cRepNum += cCampo5
	
	//ALERT(cRepNum)

Return

Static Function RetNN_Safra()
	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 9)
	Local cTexto:= ""
	
	cRet:= cA6__NOSSO // + Modu11_Safra(cA6__NOSSO, '422')

	DbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	MsUnlock()
	
Return cRet

Static Function Modu11_Safra(cData)
Local nRet := 0
Local cFixo := '4329876543298765432987654329876543298765432'
//Modu11_Safra(SEE->EE_CODIGO + "9" + cFatVencto + cValor + cCpoLivre) 

nSoma := 0
for ni:=1 to len(cData)
	nSoma += val(substr(cData,ni,1))*val(substr(cFixo,ni,1))
next

//alert(nSoma)

nResto:= Mod(nSoma, 11)
//alert(nResto)

if nResto = 0 .or. nResto = 1 .or. nResto = 10
	nRet := 1
else
	nRet := 11 - nResto
endif
//alert(nRet)
return nRet

/**********************************************/
// Rotinas de impressao do boleto do SAFRA    //
/**********************************************/
User Function Bol_Safra()
	Local oFont8
	Local oFont10
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	Local cBitmapBco:= "\system\safra.bmp"
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	Local i

	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= Transform(SE1->E1_NUM, '@R '+Replicate("9",TamSx3("E1_NUM")[1])) //U_NumDoc()

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_Safra()
	Else
		cNossoNum:= SE1->E1_NUMBCO
	EndIf
	
	//alert("antes")
	//alert(cCdBarra)
	CdBarra_Safra()
	
	//alert("depois")
	//alert(cCdBarra)

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	//Customizado para trazer Logo do Bradesco, caso exista, #Rafael Achôa - 24-07-2014
	If File(cBitmapBco)
		oPrint:SayBitmap( 0045, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (0084,100,'Banco Safra S.A.',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0062,567,'422-7',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Beneficiário",oFont8)
	If Empty(cCorrNom) // Não possui correspondente
		oPrint:Say  (0180,100 ,alltrim( SM0->M0_NOMECOM ),oFont10) //Nome + CNPJ
		oPrint:Say  (0210,100 ,transform( SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (0180,100 ,alltrim(cCorrNom),oFont10)
		oPrint:Say  (0210,100 ,cCorrCNPJ,oFont10) //Nome + CNPJ
	EndIf
	
	oPrint:Say  (0150,1060,"Agência/Código Beneficiário"                         ,oFont8)
	cCedente:= cAgeEmp+cDigAge+"00"+cCtaEmp+cDigEmp
	cCedImp := ALLTRIM(SEE->EE_AGENCIA)+"/"+ALLTRIM(SEE->EE_CONTA)
	oPrint:Say  (0200,1060,cCedImp                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Pagador"                                         ,oFont8)
	oPrint:Say  (0300,100 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1410,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1410,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)
	//Customizado para trazer Logo do Bradesco, caso exista, #Rafael Achôa - 24-07-2014
	If File(cBitmapBco)
		oPrint:SayBitmap( 0605, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (0644,100,'Banco Safra S.A.'    ,oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0622,567,'422-7'       ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Pagador",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	//oPrint:Say  (0750,100 ,"Até o vencimento pagavel em qualquer banco"        ,oFont10)  // Customizado para novo layout do Bradesco, #Rafael Achôa - 26-08-2014
	oPrint:Say  (0750,100 ,"Pagável em qualquer Banco do Sistema de Compensação"        ,oFont10)  // Customizado para novo layout do Bradesco, #Rafael Achôa - 26-08-2014
	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2010,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Beneficiário"                                        ,oFont8)
	If Empty(cCorrNom)
		oPrint:Say  (0850,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (0820,250 ,alltrim(cCorrNom)+ " - "+cCorrCNPJ,oFont10)
		oPrint:Say  (0860,250 ,alltrim(cCorrEnd),oFont10)
	EndIf

	oPrint:Say  (0810,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (0850,2010,cCedImp											,oFont10)			// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                             ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (0940,2010,cNossoNum)

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (1010,555 , cCartEmp	                                	,oFont10)

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (1050,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções

	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1300,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB)+' - '+AllTrim(SA1->A1_BAIRRO), oFont10)
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END)+' - '+AllTrim(SA1->A1_BAIRRO), oFont10)
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	//If Empty(cCorrNom) // Não possui correspondente
		//oPrint:Say  (1605,100 ,"Pagador/Avalista"                               ,oFont8)
	//Else
		oPrint:Say  (1605,100 ,"Sacador/Avalista"                               ,oFont8)
	//EndIF

	If !Empty(cCorrNom) // Possui correspondente
		oPrint:Say  (1605,300 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_BAIRCOB)+" - "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" - "+Alltrim(SM0->M0_CEPCOB)+" CNPJ: "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont8)
	EndIf

	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)
	If File (cBitmapBco)
		oPrint:SayBitmap( 1895, 0100, cBitmapBco, 0355, 0095 )
	Else
		oPrint:Say  (1934,100,'Banco Safra S.A.',oFont16 )	// [2]Nome do Banco
	Endif

	oPrint:Say  (1912,567,'422-7'   ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (1934,820,cRepNum   ,oFont14n)	//Linha Digitavel do Codigo de Barras

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"Pagável em qualquer Banco do Sistema de Compensação"        ,oFont10) 	 // Customizado para novo layout do Bradesco, #Rafael Achôa - 26-08-2014

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Beneficiário"                                        ,oFont8)
	If Empty(cCorrNom)
		oPrint:Say  (2140,100 ,alltrim(SM0->M0_NOMECOM)+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	Else
		oPrint:Say  (2110,250 ,alltrim(cCorrNom)+ " - " + cCorrCNPJ,oFont10)
		oPrint:Say  (2150,250 ,alltrim(cCorrEnd),oFont10)
	EndIf

	oPrint:Say  (2100,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedImp											,oFont10)		// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,2010,cNossoNum)

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (2300,555 , cCartEmp       		                          	,oFont10)

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2590,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB)+' - '+AllTrim(SA1->A1_BAIRRO), oFont10)
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END)+' - '+AllTrim(SA1->A1_BAIRRO), oFont10)
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	//If Empty(cCorrNom) // Não possui correspondente
		//oPrint:Say  (2895,100 ,"Pagador/Avalista"                               ,oFont8)
	//Else
		oPrint:Say  (2895,100 ,"Sacador/Avalista"                               ,oFont8)
	//EndIf

	If !Empty(cCorrNom) // Possui correspondente
		oPrint:Say  (2895,300 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_BAIRCOB)+" - "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" - "+Alltrim(SM0->M0_CEPCOB)+" CNPJ: "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont8)
	EndIf

	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	//ALERT(cCdBarra)
	If cLayOut == "LASER"
		MSBAR("INT25",26,1.5,cCdBarra,oPrint,.F.,,,,1.2,,,,.F.)
	Else
		MSBAR3("INT25",23.4,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/**********************************************/
// Rotinas de impressao do boleto do Banespa  //
/**********************************************/
/******************************************************************************************************************/
Static Function Bol_Bane()
	/******************************************************************************************************************/
	Local oFont8
	Local oFont10
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	Local aCoords1:= {2250,1900,2350,2300}
	Local aCoords2:= {2520,1900,2590,2300}
	Local oBrush
	Local cTexto
	Local nValor
	Local cValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	Local cBitmapBco:= "\system\santander.bmp"
	Local i
	//Local cInst01 := mv_par10+mv_par11
	//Local cInst02 := mv_par12+mv_par13
	//Local cInst03 := mv_par14+mv_par15
	Private cD1
	Private cD2
	Private cD3

	cForm1:= '001'
	cForm2:= '002'

	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_Bane() // Verifica
		cNossoNum:= Alltrim(cNossoNum)
	Else
		cNossoNum:= Alltrim(SE1->E1_NUMBCO)
	EndIf

	CdBarra_Bane()

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)
	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oBrush:= TBrush():New("",4)

	oPrint:StartPage()   // Inicia uma nova página

	If (!Empty(cBitMapBco))
		oPrint:SayBitmap(008 , 100, cBitMapBco, 390, 120)
	Else
		oPrint:Say(072,100,'Banco Santander',oFont16 )	// Nome do Banco
	Endif

	oPrint:Say(050, 1800, SM0->M0_ENDCOB, oFont8)
	oPrint:Say(085, 1800, SM0->M0_CIDCOB + " - " + SM0->M0_ESTCOB, oFont8)
	cTexto:= "CEP: " + Substr(SM0->M0_CEPCOB, 1, 5) + "-" + Substr(SM0->M0_CEPCOB, 6, 3) + Space(3) + SM0->M0_BAIRCOB
	oPrint:Say(120, 1800, cTexto, oFont8)
	oPrint:Say(155, 1800, "PABX/FAX: " + SM0->M0_TEL, oFont8)
	cCNPJ:= "CNPJ: " + Substr(SM0->M0_CGC, 1, 2) + "." + Substr(SM0->M0_CGC, 3, 3) + "." + Substr(SM0->M0_CGC, 6, 3) +;
	"/" + Substr(SM0->M0_CGC, 9, 4) + "-" + Substr(SM0->M0_CGC, 13, 2)
	oPrint:Say(190, 1800, cCNPJ, oFont8)
	cTexto:= "I.E.: " + Substr(SM0->M0_INSC, 1, 3) + "." + Substr(SM0->M0_INSC, 4, 3) + "." + Substr(SM0->M0_INSC, 7, 3) +;
	"." + Substr(SM0->M0_INSC,10,3)
	oPrint:Say(225, 1800, cTexto, oFont8)

	oPrint:Say(225, 100, SM0->M0_NOMECOM, oFont14n)
	oPrint:SayBitmap(50 , 100, cBitMap, 250, 150)
	oPrint:Box  (300,100,1200,2300)
	oPrint:Line (400,100,400,2300 )
	oPrint:Line (600,100,600,2300 )
	oPrint:Line (300,400,400,400  )
	oPrint:Line (300,800,400,800  )
	oPrint:Line (300,1150,400,1150)
	oPrint:Line (300,1500,400,1500)
	oPrint:Line (300,1850,400,1850)

	oPrint:Say(430,115 ,"Dados do Sacado", oFont8)
	//oPrint:Say(470,115 ,SA1->A1_NOME, oFont10)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say(470,115 ,SA1->A1_NOME + "   CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3) + ".";
		+ Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2), oFont10)
	else
		oPrint:Say(470,115 ,SA1->A1_NOME + "   CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3) + ".";
		+ Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2), oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		oPrint:Say(505, 0115, SA1->A1_ENDCOB, oFont10)
		oPrint:Say(505, 1500, SA1->A1_BAIRROC, oFont10)
		oPrint:Say(540, 0115, AllTrim(SA1->A1_MUNC) + " - " + SA1->A1_ESTC, oFont10)
		oPrint:Say(540, 1500, Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3), oFont10)
	Else
		oPrint:Say(505, 0115, SA1->A1_END, oFont10)
		oPrint:Say(505, 1500, SA1->A1_BAIRRO, oFont10)
		oPrint:Say(540, 0115, AllTrim(SA1->A1_MUN) + " - " + SA1->A1_EST, oFont10)
		oPrint:Say(540, 1500, Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3), oFont10)
	EndIf

	//// tirar essa linha
	////oPrint:Say(600, 100, cCdBarra, oFont14n)

	oPrint:Say(315, 115, "Vencimento", oFont8)

	oPrint:Say(345, 115, FormatData(DTOS(SE1->E1_VENCTO)), oFont10)

	oPrint:Say(315, 415, "Valor R$", oFont8)
	oPrint:Say(345, 415, cValor, oFont10)

	oPrint:Say(315, 815, "Data da Operação", oFont8)
	//oPrint:Say(345, 815, FormatData(DTOS(Date())), oFont10)
	oPrint:Say(345, 815, FormatData(DTOS(dDataBase)), oFont10)

	oPrint:Say(315, 1165, "Nro.do Documento", oFont8)
	oPrint:Say(345, 1165, cNumDoc, oFont10)

	oPrint:Say(315, 1515, "Agência/Código Cedente", oFont8)

	// Formatar o cedente: AAA TT CCCCC D
	// Onde:	A -> Agencia
	//			T -> Carteira
	//		    C -> Conta
	//			D -> Digito da Conta
	cCedente:= cAgeEmp + "/" /*+ Alltrim(SEE->EE_CODEMP)*/ /*+ cCartEmp + " "*/ + Strzero(Val(right(Alltrim(SEE->EE_CODEMP),7)),7)

	oPrint:Say(345, 1515, cCedente, oFont10)

	oPrint:Say(315, 1865, "Nosso Número", oFont8)
	//cTexto:= Substr(cNossoNum, 1, 3) + " " + Substr(cNossoNum, 4, 7) + " " + Substr(cNossoNum, 11, 1)
	oPrint:Say(345, 1865, cNossoNum, oFont10)

	oPrint:Say(1210, 1605, "Destaque aqui, esta via não precisa ser levada ao banco", oFont8)

	For i:= 100 To 2300 Step 50
		oPrint:Line(1250, i, 1250, i + 30)
	Next i

	oPrint:Line(1400,100,1400,2300)
	oPrint:Line(1400,550,1300,550)
	oPrint:Line(1400,800,1300,800)

	If (!Empty(cBitMapBco))
		oPrint:SayBitmap(1270 , 100, cBitMapBco, 390, 120)
	Else
		//oPrint:Say(1310, 100, "Banco do Estado", oFont8)
		//oPrint:Say(1350, 100, "de São Paulo SA", oFont8)
		////oPrint:Say(1290, 420, "=", oFont24)
		////oPrint:Say(1334, 320, "banespa", oFont16)

		oPrint:Say(1334,100,'Banco Santander',oFont16 )	// Nome do Banco
	Endif
	oPrint:Say(1312, 567, "033-7", oFont24)
	oPrint:Say(1334, 1900, "Via do Sacado", oFont16)
	oPrint:Say(1450, 0247, "Cedente:", oFont8)
	oPrint:Say(1510, 0176, "Nro.Documento:", oFont8)
	oPrint:Say(1510, 1500, "Nosso Número:", oFont8)
	oPrint:Say(1570, 0140, "Data do Documento:", oFont8)
	oPrint:Say(1570, 1538, "Vencimento:", oFont8)
	oPrint:Say(1630, 0241, "Valor R$:", oFont8)

	oPrint:Say(1448, 0400, /*SM0->M0_NOMECOM + " (" + cCNPJ + ")"*/alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" || "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'), oFont10) // Customizado para aparecer NOME+END+CNPJ, #Rafael Achôa 28-08-14
	oPrint:Say(1508, 0400, cNumDoc ,oFont10)
	cTexto:= Alltrim(cNossoNum) //Substr(cNossoNum, 1, 3) + " " + Substr(cNossoNum, 4, 7) + " " + Substr(cNossoNum, 11, 1)  // Customizado para trazer o nosso número correto. #Rafael Achôa, 02-09-14
	oPrint:Say(1508, 1725, cTexto, oFont10)
	//oPrint:Say(1568, 0400, FormatData(DTOS(Date())), oFont10)
	oPrint:Say(1568, 0400, FormatData(DTOS(dDataBase)), oFont10)
	oPrint:Say(1568, 1725, FormatData(DTOS(SE1->E1_VENCTO)), oFont10)
	oPrint:Say(1628, 0400, cValor, oFont10)

	oPrint:Line (1700,100,1700,2300)
	oPrint:Say  (1720,1000,"Autenticação Mecânica",oFont8)

	For i := 100 To 2300 Step 50
		oPrint:Line(2000, i, 2000, i + 30)
	Next i

	//oPrint:FillRect(aCoords1,oBrush)
	//oPrint:FillRect(aCoords2,oBrush)

	oPrint:Line (2250,100,2250,2300)
	oPrint:Line (2250,550,2150,0550)
	oPrint:Line (2250,800,2150,0800)

	If (!Empty(cBitMapBco))
		oPrint:SayBitmap(2120 , 100, cBitMapBco, 390, 120)
	Else
		//oPrint:Say(2160, 100, "Banco do Estado", oFont8)
		//oPrint:Say(2200, 100, "de São Paulo SA", oFont8)
		//oPrint:Say(2140, 420, "=", oFont24)
		//oPrint:Say(2184, 320, "banespa", oFont16)
		oPrint:Say(2184,100,'Banco Santander',oFont16 )	// Nome do Banco
	Endif
	oPrint:Say(2162, 567, "033-7", oFont24)

	oPrint:Say(2184, 820, cRepNum, oFont14n)

	oPrint:Line (2350,100,2350,2300)
	oPrint:Line (2450,100,2450,2300)
	oPrint:Line (2520,100,2520,2300)
	oPrint:Line (2590,100,2590,2300)

	//oPrint:Line (2520,390,2590,390)
	oPrint:Line (2450,500,2520,500) // Customizado para retirar o quadro Uso do Banco   , #Rafael Achôa 28-08-14
	oPrint:Line (2520,750,2590,750)
	oPrint:Line (2450,1000,2590,1000)
	oPrint:Line (2450,1350,2520,1350)
	oPrint:Line (2450,1550,2590,1550)

	oPrint:Say(2250, 100, "Local de Pagamento", oFont8)
	cTexto:= "PAGAR PREFERENCIALMENTE NO BANCO SANTANDER"
	oPrint:Say(2290, 100, cTexto, oFont10)

	oPrint:Say(2250, 1910, "Vencimento", oFont8)
	oPrint:Say(2290, 1920, Padl(FormatData(DTOS(SE1->E1_VENCTO)), 16), oFont10)

	oPrint:Say(2350, 100, "Cedente", oFont8)
	oPrint:Say(2390, 100, /*SM0->M0_NOMECOM + " (" + cCNPJ + ")"*/alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" || "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'), oFont10) // Customizado para aparecer NOME+END+CNPJ, #Rafael Achôa 28-08-14

	//cCedente:= cAgeEmp /*+ "-" + cDigAge */+ " / "/* + Alltrim(SEE->EE_CODEMP)*/+cCtaEmp + "-" + cDigEmp
	cCedente:= cAgeEmp + "/" /*+ Alltrim(SEE->EE_CODEMP)*/ /*+ cCartEmp + " "*/ + Strzero(Val(right(Alltrim(SEE->EE_CODEMP),7)),7)
	oPrint:Say(2350, 1910, "Agência/Cód Cedente", oFont8)
	oPrint:Say(2390, 1920, Padl(cCedente, 20), oFont10)

	oPrint:Say(2450, 100, "Data do Documento", oFont8)
	//oPrint:Say(2480, 100, FormatData(DTOS(Date())), oFont10)
	oPrint:Say(2480, 100, FormatData(DTOS(dDataBase)), oFont10)

	oPrint:Say(2450, 505, "Nro.Documento", oFont8)
	oPrint:Say(2480, 605, cNumDoc, oFont10)

	oPrint:Say(2450, 1005, "Espécie Doc.", oFont8)
	oPrint:Say(2480, 1040, "DM", oFont10)

	oPrint:Say(2450, 1355, "Aceite", oFont8)
	oPrint:Say(2480, 1440, "N", oFont10)

	oPrint:Say(2450, 1555, "Data do Processamento", oFont8)
	oPrint:Say(2480, 1655, FormatData(DTOS(SE1->E1_EMISSAO)), oFont10)

	oPrint:Say(2450, 1910, "Nosso Número", oFont8)
	//cTexto:= Substr(cNossoNum, 1, 3) + " " + Substr(cNossoNum, 4, 7) + " " + Substr(cNossoNum, 11, 1)
	oPrint:Say(2480, 1920, Padl(cNossoNum, 16), oFont10)

	/*oPrint:Say(2520, 100 , "Uso do Banco", oFont8)
	oPrint:Say(2550, 110 , "         ", oFont10)*/ // Customizado para retirar o quadro Uso do Banco   , #Rafael Achôa 28-08-14

	oPrint:Say(2520, 100 , "Carteira", oFont8)
	oPrint:Say(2550, 120 , "RCR", oFont10)

	oPrint:Say(2520, 755 , "Espécie", oFont8)
	oPrint:Say(2550, 805 , "REAL", oFont10)

	oPrint:Say(2520, 1005, "Quantidade", oFont8)
	oPrint:Say(2550, 1055, /*"001"*/"", oFont10)

	oPrint:Say(2520, 1555, "Valor", oFont8)
	oPrint:Say(2550, 1605, /*Padl(cValor, 16)*/"", oFont10)

	oPrint:Say(2520, 1910, "(=)Valor do Documento", oFont8)
	oPrint:Say(2550, 1920, Padl(cValor, 16), oFont10)

	cTexto:= "Instruções:"
	oPrint:Say(2590, 100, cTexto, oFont8)
	U_ImpIT(oFont10,2590) //Imprime instruções
	/*
	cTexto := cInst01
	oPrint:Say(2650, 100, cTexto, oFont10)

	cTexto := cInst02
	oPrint:Say  (2700,100 ,cTexto                                           ,oFont10)

	cTexto := cInst03+CHR(10)+CHR(13)+cInst04
	oPrint:Say  (2800,100 ,cTexto                                           ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
		oPrint:Say(2900, 100, cTexto, oFont10)
	EndIf

	oPrint:Say(2590, 1910, "(-)Desconto/Abatimento", oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2630,1910,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say(2660, 1910, "(-)Outras Deduções", oFont8)
	oPrint:Say(2730, 1910, "(+)Mora/Multa", oFont8)
	oPrint:Say(2800, 1910, "(+)Outros Acréscimos", oFont8)
	oPrint:Say(2870, 1910, "(-)Valor Cobrado", oFont8)

	oPrint:Say(2940, 100, "Sacado", oFont8)

	//oPrint:Say(2970, 400, AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")", oFont10)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say(2970, 400, AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")";
		+ "   CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3) + "." + Substr(SA1->A1_CGC, 6, 3) +;
		"/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2), oFont10)
	else
		oPrint:Say(2970, 400, AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")";
		+ "   CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3) + "." + Substr(SA1->A1_CGC, 7, 3) +;
		"-" + Substr(SA1->A1_CGC, 10, 2), oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		oPrint:Say(3023, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(3076, 400, AllTrim(SA1->A1_MUNC) + " - " + AllTrim(SA1->A1_ESTC), oFont10)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3)
		oPrint:Say(3129, 400 , cTexto, oFont10)
		oPrint:Say(3129, 1000 , SA1->A1_BAIRROC, oFont10)
	Else
		oPrint:Say(3023, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(3076, 400, AllTrim(SA1->A1_MUN) + " - " + AllTrim(SA1->A1_EST), oFont10)
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3)
		oPrint:Say(3129, 400 , cTexto, oFont10)
		oPrint:Say(3129, 1000 , SA1->A1_BAIRRO, oFont10)
	EndIf

	oPrint:Say(3150, 0100, "Sacado/Avalista", oFont8)
	oPrint:Say(3185, 1500, "Autenticação Mecânica / Ficha de Compensação", oFont8)

	oPrint:Line (2250,1900,2940,1900)
	oPrint:Line (2660,1900,2660,2300)
	oPrint:Line (2730,1900,2730,2300)
	oPrint:Line (2800,1900,2800,2300)
	oPrint:Line (2870,1900,2870,2300)
	oPrint:Line (2940,0100,2940,2300)
	oPrint:Line (3180,0100,3180,2300)

	/*
	±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
	±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
	±±³Fun‡…o    ³MSBAR       ³ Autor ³ ALEX SANDRO VALARIO ³ Data ³  06/99   ³±±
	±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
	±±³Descri‡…o ³ Imprime codigo de barras                                   ³±±
	±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
	±±³Parametros³ 01 cTypeBar String com o tipo do codigo de barras          ³±±
	±±³          ³ 				"EAN13","EAN8","UPCA" ,"SUP5"   ,"CODE128"     ³±±
	±±³          ³ 				"INT25","MAT25,"IND25","CODABAR","CODE3_9"     ³±±
	±±³          ³ 02 nRow		Numero da Linha em centimentros                ³±±
	±±³          ³ 03 nCol		Numero da coluna em centimentros			   ³±±
	±±³          ³ 04 cCode		String com o conteudo do codigo                ³±±
	±±³          ³ 05 oPr		Objecto Printer                                ³±±
	±±³          ³ 06 lcheck	Se calcula o digito de controle                ³±±
	±±³          ³ 07 Cor 		Numero  da Cor, utilize a "common.ch"          ³±±
	±±³          ³ 08 lHort		Se imprime na Horizontal                       ³±±
	±±³          ³ 09 nWidth	Numero do Tamanho da barra em centimetros      ³±±
	±±³          ³ 10 nHeigth	Numero da Altura da barra em milimetros        ³±±
	±±³          ³ 11 lBanner	Se imprime o linha em baixo do codigo          ³±±
	±±³          ³ 12 cFont		String com o tipo de fonte                     ³±±
	±±³          ³ 13 cMode		String com o modo do codigo de barras CODE128  ³±±
	±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
	±±³ Uso      ³ ImpressÆo de etiquetas c¢digo de Barras para HP e Laser    ³±±
	±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
	±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
	ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
	*/
	//-- Exemplo:
	//-- MSBAR("CODE128",3,1,"12345678901",oPr,,,,,,,,"A")

	//MSBARHP.PRW
	If cLayOut == "LASER"
		MSBAR("CODE128",27.7,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	Else
		MSBAR3("CODE128",23.4,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/******************************************************************************************************************/
Static Function RetNN_Bane()
	/******************************************************************************************************************/
	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 12) // Customizado para layout atual do banco onde Nosso Numero = NNNNNNNNNNNN D, sendo N para sequencial e D o dígito no nosso numero, #Rafael Achôa 28-08-14
	//Local cA6__NOSSO:= Right(AllTrim(SEE->EE_FAXATU),7)
	//Local cFatores:= "7319731973"    // Customizado para layout atual do banco onde Nosso Numero = NNNNNNNNNNNN D, sendo N para sequencial e D o dígito no nosso numero, #Rafael Achôa 28-08-14
	Local nCont
	Local nSoma:= 0
	Local nAux

	/*cTexto:= Substr(SA6->A6_AGENCIA, 1, 3) + cA6__NOSSO

	For nCont:= 1 To Len(cTexto)
	nAux:= Val(Substr(cTexto, nCont, 1)) * Val(Substr(cFatores, nCont, 1))
	nSoma += Mod(nAux, 10)
	Next nCont

	nAux:= Mod(nSoma, 10)

	If nAux > 0
	nAux:= 10 - nAux
	EndIf

	cRet:= Substr(SA6->A6_AGENCIA, 1, 3) + cA6__NOSSO + StrZero(nAux, 1)       */ // Customizado para layout atual do banco onde Nosso Numero = NNNNNNNNNNNN D, sendo N para sequencial e D o dígito no nosso numero, #Rafael Achôa 28-08-14

	cTexto := cA6__NOSSO + U_Modu11_Sant(cA6__NOSSO)
	cRet:= cTexto
	/* Atualizar o nosso numero no cadastro do banco */
	DbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	MsUnlock()

Return cRet

/******************************************************************************************************************/
User Function Modu11_Sant(cLinha)
	/******************************************************************************************************************/
	Local cDigRet
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local nFator:= 9
	Local nResult

	For nCont:= Len(cLinha) TO 1 Step -1
		nFator++
		If nFator > 9
			nFator:= 2
		EndIf

		nSoma += Val(Substr(cLinha, nCont, 1)) * nFator
	Next nCont

	nResto:= Mod(nSoma, 11)

	If nResto == 0 .Or. nResto == 1 //.Or. nResult == 10 .Or. nResult == 11 //Customizado para correto cálculo do Nosso Número
		cDigRet:= "0"
	Elseif nResto == 10
		cDigRet:= Substr(Strzero(nResto, 2),1,1)
	Else
		nResult:= 11 - nResto
		cDigRet:= StrZero(nResult, 1)
	EndIf

Return cDigRet

/******************************************************************************************************************/
Static Function CdBarra_Bane()
	/******************************************************************************************************************/
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCpoLivre:= ""
	Local cChave
	Local cCampo1
	Local cCampo2
	Local cCampo3
	Local cCampo4
	Local cCampo5

	cFatVencto:= StrZero(FatVencto(), 4)
	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= StrZero(nValor * 100, 10)

	// cCpoLivre:= cAgeEmp + cCartEmp + cCtaEmp + cDigEmp + Substr(cNossoNum, 4, 7) + "00" + "033"		// Customizado para novo Campo livre, de acordo com o novo Layout do banco, #Rafael Achôa 28-08-14
	cCpoLivre:= "9" + Strzero(Val(right(Alltrim(SEE->EE_CODEMP),7)),7) + cNossoNum + "0" + "101"
	/*								// Customizado para novo Campo livre, de acordo com o novo Layout do banco, #Rafael Achôa 28-08-14
	// Cálculo do digito D1
	cD1:= Modu10_Bane(cCpoLivre)

	// Cálculo do digito D2
	cD2:= D2_Bane(cCpoLivre)

	cCpoLivre += cD1 + cD2

	*/
	// Cálculo do digito de conferência D3
	cChave:= SA6->A6_COD + "9" + cFatVencto + cValor + cCpoLivre
	cD3:= D3_Bane(cChave)

	cCdBarra:= SA6->A6_COD + "9" + cD3 + cFatVencto + cValor + cCpoLivre

	// Calculo da linha digitavel
	cCampo1:= SA6->A6_COD + "9" + Substr(cCpoLivre, 1, 5)
	cCampo2:= Substr(cCpoLivre, 6, 10)
	cCampo3:= Substr(cCpoLivre, 16, 10)
	cCampo4:= cD3
	cCampo5:= cFatVencto + cValor

	// Cálculo dos dígitos dos campos 1, 2 e 3
	cCampo1 += cCampo1 + Modu10_Bane(cCampo1)
	cCampo2 += cCampo2 + Modu10_Bane(cCampo2)
	cCampo3 += cCampo3 + Modu10_Bane(cCampo3)

	cRepNum := Left(cCampo1, 5) + "." + Right(cCampo1, 5) + "  "
	cRepNum += Left(cCampo2, 5) + "." + Right(cCampo2, 6) + "  "
	cRepNum += Left(cCampo3, 5) + "." + Right(cCampo3, 6) + "  "
	cRepNum += cCampo4 + "  "
	cRepNum += cCampo5

Return

/******************************************************************************************************************/
Static Function Modu10_Bane(cLinha)
	/******************************************************************************************************************/
	Local lDobra
	Local nCont
	Local nPeso
	Local nSoma
	Local nResto
	Local cRet

	lDobra:= .f.
	nSoma:= 0
	For nCont:= Len(cLinha) To 1 Step -1
		lDobra:= !lDobra

		nPeso:= Val(Substr(cLinha, nCont, 1)) * IIF(lDobra, 2, 1)

		If nPeso > 9
			nPeso:= nPeso - 9
		EndIf
		nSoma += nPeso
	Next nCont

	nResto:= MOD(nSoma, 10)

	If nResto == 0
		cRet:= "0"
	Else
		cRet:= StrZero(10 - nResto, 1)
	EndIf

Return cRet

/******************************************************************************************************************/
Static Function D2_Bane(cCpoLivre)
	/******************************************************************************************************************/
	Local cRet
	Local nFator
	Local nSoma
	Local nCont
	Local nResto
	Local cLinha:= cCpoLivre + cD1

	nFator:= 7
	nSoma:= 0
	For nCont:= Len(cLinha) To 1 Step -1
		nFator++
		If nFator > 7
			nFator:= 2
		EndIf

		nSoma += Val(Substr(cLinha, nCont, 1)) * nFator
	Next nCont

	nResto:= MOD(nSoma, 11)

	Do Case
		Case nResto == 10
		cRet:= StrZero(11 - nResto, 1)

		Case nResto == 0
		cRet:= "0"

		Case nResto == 1
		If Val(cD1) == 9
			cD1:= "0"
			cRet:= D2_Bane(cCpoLivre)
		ElseIf Val(cD1) < 9
			cD1:= StrZero(Val(cD1) + 1, 1)
			cRet:= D2_Bane(cCpoLivre)
		EndIf

		Case nResto > 1
		cRet:= StrZero(11 - nResto, 1)
	EndCase

Return cRet

/******************************************************************************************************************/
Static Function D3_Bane(cChave)
	/******************************************************************************************************************/
	Local cRet
	Local nCont
	Local nFator
	Local nSoma
	Local nResto

	nFator:= 9
	nSoma:= 0
	For nCont:= Len(cChave) To 1 Step -1
		nFator++
		If nFator > 9
			nFator:= 2
		EndIf

		nSoma += Val(Substr(cChave, nCont, 1)) * nFator
	Next nCont

	nResto:= MOD(nSoma, 11)

	If nResto == 0 .Or. nResto == 1 .Or. nResto == 10
		cRet:= "1"
	Else
		cRet:= StrZero(11 - nResto, 1)
	EndIf

Return cRet

/******************************************/
// Rotinas de impressao do boleto do HSBC //
/******************************************/
/******************************************************************************************************************/
Static Function Bol_HSBC()
	/******************************************************************************************************************/
	Local oFont8
	Local oFont10
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local cForm3
	Local nDescFin
	Local cBitmapBco:= "\system\HSBC.bmp"
	//Local cInst01 := mv_par10+mv_par11
	//Local cInst02 := mv_par12+mv_par13
	//Local cInst03 := mv_par14+mv_par15
	Local i

	cForm1:= '001'
	cForm2:= '002'
	cForm3:= '003'

	nValor:= IIf(!Empty(SE1->E1_SALDO),Round(SE1->E1_SALDO, 2),Round(SE1->E1_VALOR, 2))
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_HSBC()
	Else
		cNossoNum:= SE1->E1_NUMBCO
	EndIf
	CdBarra_HSBC()

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)
	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	oPrint:Say  (0084,100,'HSBC' ,oFont16 )	// [2]Nome do Banco
	oPrint:Say  (0062,567,'399-9',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0200,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform( SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	oPrint:Say  (0150,1060,"Agência/Código Beneficiário"                         ,oFont8)
	cCedente:= cAgeEmp + '-' + cCtaEmp
	oPrint:Say  (0200,1060,cCedente                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Pagador"                                         ,oFont8)
	oPrint:Say  (0300,100 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1410,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1410,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)
	oPrint:Say  (0644,100,'HSBC'    ,oFont16 )	// [2]Nome do Banco
	oPrint:Say  (0622,567,'399-9'   ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Pagador",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (0750,100 ,"QUALQUER BANCO ATÉ A DATA DO VENCIMENTO"        ,oFont10)

	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2010,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0850,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ

	oPrint:Say  (0810,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (0850,2010,cCedente											,oFont10)

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                             ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (0940,2010,Padl(cNossoNum, 18)				                ,oFont10)

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (1010,555 ,cCartEmp                                      	,oFont10)

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (1050,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE O BENEFICIÁRIO",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções
	/*
	cTexto := cInst01
	oPrint:Say  (1100,100 ,cTexto                                           ,oFont10)
	// COLOCAR ESSE TEXTO NO PARAMETROS DE BANCO "APÓS VENCIMENTO COBRAR JUROS MORA DIARIO DE 0,33% | R$ "+AllTrim(Transform((nValor*0.0033), "@E 999,999,999.99"))+" "
	cTexto := cInst02
	oPrint:Say  (1150,100 ,cTexto                                           ,oFont10)
	// COLOCAR ESSE TEXTO NO PARAMETROS DE BANCO "APÓS VENCIMENTO MULTA DE 2% | R$ "+AllTrim(Transform((nValor*0.02), "@E 999,999,999.99"))+" "
	cTexto := cInst03+CHR(10)+CHR(13)+cInst04
	oPrint:Say  (1200,100 ,cTexto		                                    ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1400,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (1605,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)

	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	// Encerra aqui a alteracao para o novo layout - RAI

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)
	oPrint:Say  (1934,100,'HSBC' ,oFont16 )	// [2]Nome do Banco
	oPrint:Say  (1912,567,'399-9',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (1934,820,cRepNum   ,oFont14n)	//Linha Digitavel do Codigo de Barras

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"QUALQUER BANCO ATÉ A DATA DO VENCIMENTO"        ,oFont10)

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (2140,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ

	oPrint:Say  (2100,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedente											,oFont10)

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,2010,Padl(cNossoNum, 18)                              ,oFont10)

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (2300,555 ,cCartEmp                                      	,oFont10)

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE O BENEFICIÁRIO",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	/*
	cTexto := cInst01
	oPrint:Say  (2390,100 ,cTexto ,oFont10)
	cTexto := cInst02
	// COLOCAR ESSE TEXTO NO PARAMETROS DE BANCO "APÓS VENCIMENTO COBRAR JUROS MORA DIARIO DE 0,33% | R$ "+AllTrim(Transform((nValor*0.0033), "@E 999,999,999.99"))+" "
	oPrint:Say  (2440,100 ,cTexto                                           ,oFont10)
	// COLOCAR ESSE TEXTO NO PARAMETROS DE BANCO "APÓS VENCIMENTO MULTA DE 2% | R$ "+AllTrim(Transform((nValor*0.02), "@E 999,999,999.99"))+" "
	cTexto := cInst03
	oPrint:Say  (2490,100 ,cTexto		                                    ,oFont10)
	cTexto := cInst04
	oPrint:Say  (2540,100 ,cTexto                                           ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2590,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (2895,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	If cLayOut == "LASER"
		MSBAR("INT25",26,1.5,cCdBarra,oPrint,.F.,,,,1.2,,,,.F.)
	Else
		MSBAR3("INT25",23.4,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/******************************************************************************************************************/
Static Function RetNN_HSBC()
	/******************************************************************************************************************/
	// Nosso Numero do HSBC: EEEEE NNNNN D
	// Onde:	EEEEE = Codigo do Cliente no Banco
	//			NNNNN = Numero Sequencial
	//			D = Digito verificador modulo 11, com pesos de 2 a 7 da direita para a esquerda

	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 5)
	//Local cA6__NOSSO:= Right(AllTrim(SEE->EE_FAXATU),5)
	Local cTexto:= ""
	Local cRet:= ""

	cTexto:= "65581" + cA6__NOSSO
	cRet:= cTexto + u_Modu11_HSBC(cTexto)

	/* Atualizar o nosso numero no cadastro do banco */
	DbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	MsUnlock()

Return cRet

/******************************************************************************************************************/
User Function Modu11_HSBC(cLinha)
	/******************************************************************************************************************/
	Local cDigRet
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local nFator:= 7
	Local nResult

	For nCont:= Len(cLinha) To 1 Step -1
		nFator++
		If nFator > 7
			nFator:= 2
		EndIf

		nSoma += Val(Substr(cLinha, nCont, 1)) * nFator
	Next nCont

	nResto:= Mod(nSoma, 11)

	If nResto == 0 .Or. nResto == 1
		cDigRet:= "0"
	Else
		nResult:= 11 - nResto
		cDigRet:= StrZero(nResult, 1)
	EndIf

Return cDigRet

/******************************************************************************************************************/
Static Function CdBarra_HSBC()
	/******************************************************************************************************************/
	Local cDigCdBarra
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCampo1:= ""
	Local cCampo2:= ""
	Local cCampo3:= ""
	Local cCampo4:= ""
	Local cCampo5:= ""
	Local cCtaAux:= StrTran(cCtaEmp, "-", "")

	cFatVencto:= StrZero(FatVencto(), 4)
	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= StrZero(nValor * 100, 10)

	/* Calculo do codigo de barras */
	cCdBarra:= SA6->A6_COD + "9" + cFatVencto + cValor + Substr(cNossoNum, 1, 11) + cAgeEmp + cCtaAux + "00" + "1"

	cDigCdBarra:= Modu11(cCdBarra)

	cCdBarra:= SA6->A6_COD + "9" + cDigCdBarra + cFatVencto + cValor + Substr(cNossoNum, 1, 11) + cAgeEmp + cCtaAux + "00" + "1"

	/* Calculo da representacao numerica */
	cCampo1:= SA6->A6_COD + "9" + Substr(cNossoNum, 1, 5)
	cCampo2:= Substr(cNossoNum, 6, 6) + cAgeEmp
	cCampo3:= cCtaAux + "00" + "1"
	cCampo4:= cDigCdBarra
	cCampo5:= cFatVencto + cValor

	/* Calculando os DACs dos campos 1, 2 e 3 */
	cCampo1:= cCampo1 + Modu10(cCampo1)
	cCampo2:= cCampo2 + Modu10(cCampo2)
	cCampo3:= cCampo3 + Modu10(cCampo3)

	cRepNum := Substr(cCampo1, 1, 5) + "." + Substr(cCampo1, 6, 5) + "  "
	cRepNum += Substr(cCampo2, 1, 5) + "." + Substr(cCampo2, 6, 6) + "  "
	cRepNum += Substr(cCampo3, 1, 5) + "." + Substr(cCampo3, 6, 6) + "  "
	cRepNum += cCampo4 + "  "
	cRepNum += cCampo5

Return

/*****************************************************/
// Rotinas de impressao do boleto do Banco do Brasil //
/*****************************************************/
/******************************************************************************************************************/
Static Function Bol_BB()
	/******************************************************************************************************************/
	Local oFont8
	Local oFont10
	Local oFont13
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	//Local aCoords1:= {2250,1900,2350,2300}
	//Local aCoords2:= {2520,1900,2590,2300}
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	Local cBitmapBco:= "\system\BBrasil.bmp"
	//Local cInst01 := SEE->EE_FORMEN1//mv_par10+mv_par11
	//Local cInst02 := SEE->EE_FORMEN2//mv_par12+mv_par13
	//Local cInst03 := SEE->EE_FOREXT1//mv_par14+mv_par15
	//Local cInst04 := SEE->EE_FOREXT2
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	LOCAL nPos     := 0
	Local i

	cForm1:= ''
	cForm2:= ''
	cForm3:= ''

	nValor:= IIf(!Empty(SE1->E1_SALDO),Round(SE1->E1_SALDO, 2),Round(SE1->E1_SALDO, 2))
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	IF !Empty(SE1->E1_PARCELA)
		cNumDoc:= SE1->E1_PREFIXO + ' ' + SE1->E1_NUM + '-' + SE1->E1_PARCELA
	Else
		cNumDoc:= SE1->E1_PREFIXO + ' ' + SE1->E1_NUM
	Endif

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_BB()
	Else
		cNossoNum:= /*Alltrim(SEE->EE_CODEMP) +*/ Transform( SE1->E1_NUMBCO, '@R XXXXXXXXXXXXXXXXX' )       // Customizado para puxar correto Nosso Número no Boleto, #Rafael Achôa 12-08-14
		//cNossoNum := AllTrim(Transform( SE1->E1_NUMBCO, '@R XXXXXXXXXXXXXXXXX-X' ))
	EndIf
	
	cNossoNumImpressao := cNossoNum		//#Israel 04-04-2024
	cNossoNum := substr(cNossoNum,8,10) //#Israel 04-04-2024

	CdBarra_BB()

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)
	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont13 := TFont():New("Arial",9,13,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	If File(cBitmapBco)
		oPrint:SayBitmap( 0060, 0100, cBitmapBco, 0445, 0085 )
	Else
		oPrint:Say  (0084,100,'Banco do Brasil',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0062,567,'001-9',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0200,100 ,SubStr(alltrim(SM0->M0_NOMECOM),1,45) ,oFont10) //Nome + CNPJ
	oPrint:Say  (0150,1060,"Agência/Código Beneficiário"                         ,oFont8)
	cCedente:= cAgeEmp + '-' + cDigAge +  '/' + Alltrim(SEE->EE_CODEMP)/*cCtaEmp + '-' + cDigEmp*/			// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014
	oPrint:Say  (0200,1060,cCedente                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Pagador"                                         ,oFont8)
	oPrint:Say  (0300,100 ,AllTrim(SUBSTR(SA1->A1_NOME,1,35)) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Emissao"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_EMISSAO))                 ,oFont10)
	oPrint:Say  (0250,1305,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1305,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1305,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1305,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0560,1300,0250,1300 ) //oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)
	// Customizado para encontrar, caso exista, um logo do banco para o Boleto, #Rafael Achôa 17-07-2014
	If File(cBitmapBco)
		oPrint:SayBitmap( 0620, 0100, cBitmapBco, 0445, 0085 )
	Else
		oPrint:Say  (0644,100,'Banco do Brasil'   ,oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0622,567,'001-9'       ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Pagador",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (0750,100 ,"PAGÁVEL EM QUALQUER BANCO ATÉ O VENCIMENTO"        ,oFont10)

	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2010,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0850,100 ,alltrim(SM0->M0_NOMECOM)+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ

	oPrint:Say  (0810,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (0850,2010,cCedente											,oFont10)		// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                             ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (0940,1950, PADL(SubStr(cNossoNumImpressao,1,17),19)                 ,oFont10)            // Customizado para ficar alinhado com o campo Agencia/Codigo Beneficiario, #Rafael Achôa 17-07-2014 //Alterado #Israel 04-04-2024

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                   	    ,oFont8)
	oPrint:Say  (1010,555 , StrZero(Val(cCartEmp),2)/*+ "/" + Alltrim(cVariaCart)*/	,oFont10) //Customizado para acrescentar também a variação da carteira, #Rafael Achôa 17-07-2014

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (1050,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções
	/*
	cTexto := cInst01
	oPrint:Say  (1100,100 ,cTexto                                           ,oFont10)
	cTexto := cInst02
	oPrint:Say  (1150,100 ,cTexto                                           ,oFont10)
	cTexto := cInst03+CHR(10)+CHR(13)+cInst04
	oPrint:Say  (1200,100 ,cTexto		                                    ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1300,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB)+"     "+AllTrim(SA1->A1_BAIRROC), oFont10)										//Customizado para trazer Bairro no Boleto #Rafael Achôa, 21/10/2014
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END)+"     "+AllTrim(SA1->A1_BAIRRO), oFont10)											//Customizado para trazer Bairro no Boleto #Rafael Achôa, 21/10/2014
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (1605,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)

	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	// Encerra aqui a alteracao para o novo layout - RAI

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)
	// Customizado para encontrar, caso exista, um logo do banco para o Boleto, #Rafael Achôa 17-07-2014
	If File(cBitmapBco)
		oPrint:SayBitmap( 1910, 0100, cBitmapBco, 0445, 0085 )
	Else
		oPrint:Say  (1934,100,'Banco do Brasil',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (1912,567,'001-9'   ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (1934,820,cRepNum   ,oFont14n)	//Linha Digitavel do Codigo de Barras

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"PAGÁVEL EM QUALQUER BANCO ATÉ O VENCIMENTO"        ,oFont10)

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (2140,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ

	oPrint:Say  (2100,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedente											,oFont10)				// Customizado para trazer Código de Convênio ao invés de Conta do cliente #Rafael Achôa , 25/09/2014

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,1950, PADL(SubStr(cNossoNumImpressao,1,17),19)                  ,oFont10) //#Israel 04-04-2024

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       	,oFont8)
	oPrint:Say  (2300,555 ,StrZero(Val(cCartEmp),2)/*+ "/" + Alltrim(cVariaCart)*/	,oFont10) //Customizado para acrescentar também a variação da carteira, #Rafael Achôa 17-07-2014

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	/*
	cTexto := cInst01
	oPrint:Say  (2390,100 ,cTexto                                           ,oFont10)
	// COLOCAR NO PARAMETROS DE BANCO "APÓS VENCIMENTO ("+ FormatData(DTOS(SE1->E1_VENCTO)) +") COBRAR JUROS DE MORA DIÁRIA DE 0,1% | R$ "+AllTrim(Transform(ROUND((nValor*0.001),2), "@E 999,999,999.99"))+" "
	cTexto := cInst02
	oPrint:Say  (2440,100 ,cTexto                                           ,oFont10)
	// COLOCAR NO PARAMETROS DE BANCO "PROTESTO: "+FormatData(DTOS(DaySum(SE1->E1_VENCTO,nDiasProtesto)))+" A PARTIR DESTA, CONSULTE O BB P/ PGTO."
	cTexto := cInst03+CHR(10)+CHR(13)+cInst04
	oPrint:Say  (2490,100 ,cTexto		                                    ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2590,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB)+"     "+AllTrim(SA1->A1_BAIRROC), oFont10)											//Customizado para trazer Bairro no Boleto #Rafael Achôa, 21/10/2014
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END)+"     "+AllTrim(SA1->A1_BAIRRO), oFont10)												//Customizado para trazer Bairro no Boleto #Rafael Achôa, 21/10/2014
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (2895,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	If cLayOut == "LASER"
		MSBAR("INT25",26,1.5,cCdBarra,oPrint,.F.,,,,1.2,,,,.F.)
	Else
		MSBAR3("INT25",23.4,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/******************************************************************************************************************/
Static Function RetNN_BB()
	/******************************************************************************************************************/
	// Nosso Numero do Banco do Brasil: CCCCNNNNNNN-X
	// Onde:	CCCC = Codigo do Cliente no Banco
	//			NNNNNNN = Numero Sequencial
	//			X = Digito verificador modulo 11, com pesos de 9 a 2 da direita para a esquerda

	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 10)
	//Local cA6__NOSSO:= Right(AllTrim(SEE->EE_FAXATU),10)
	Local cTexto:= ""
	Local cRet:= ""

	cTexto:= /*"2434947"*/Alltrim(SEE->EE_CODEMP) + cA6__NOSSO
	cRet:= cTexto + '-' + Modu11_BB(cTexto)

	/* Atualizar o nosso numero no cadastro do banco */
	// DbSelectArea("SEE")
	// RecLock("SEE", .f.)
	// Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	// MsUnlock()

Return cRet

/******************************************************************************************************************/
Static Function Modu11_BB(cLinha)
	/******************************************************************************************************************/
	Local cDigRet
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local nFator:= 2

	For nCont:= Len(cLinha) To 1 Step -1
		nFator--
		If nFator < 2
			nFator:= 9
		EndIf

		nSoma += Val(Substr(cLinha, nCont, 1)) * nFator
	Next nCont

	nResto:= Mod(nSoma, 11)

	If nResto == 10
		cDigRet:= "X"
	Else
		cDigRet:= StrZero(nResto, 1)
	EndIf

Return cDigRet

/******************************************************************************************************************/
Static Function CdBarra_BB()
	/******************************************************************************************************************/
	Local cDigCdBarra
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCampo1:= ""
	Local cCampo2:= ""
	Local cCampo3:= ""
	Local cCampo4:= ""
	Local cCampo5:= ""

	cFatVencto:= StrZero(FatVencto(), 4)
	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= StrZero(nValor * 100, 10)

	/* Calculo do codigo de barras */
	cCdBarra:= SA6->A6_COD + "9" + cFatVencto + cValor + '000000' + Substr(cNossoNumImpressao, 1, 17) + cCartEmp //#Israel 04-04-2024

	cDigCdBarra:= Modu11(cCdBarra)

	cCdBarra:= SA6->A6_COD + "9" + cDigCdBarra + cFatVencto + cValor + '000000' + Substr(cNossoNumImpressao, 1, 17) + cCartEmp //#Israel 04-04-2024

	/* Calculo da representacao numerica */
	cCampo1:= SA6->A6_COD + "9" + Substr(cCdBarra,20,5)
	cCampo2:= Substr(cCdBarra,25,10)
	cCampo3:= Substr(cCdBarra,35,10)

	//cCampo1:= SA6->A6_COD + "9" + '00000'
	//cCampo2:= '0' + Substr(cNossoNum, 1, 9)
	//cCampo3:= Substr(cNossoNum, 10, 8) + cCartEmp
	cCampo4:= cDigCdBarra
	cCampo5:= cFatVencto + cValor

	/* Calculando os DACs dos campos 1, 2 e 3 */
	cCampo1:= cCampo1 + Modu10(cCampo1)
	cCampo2:= cCampo2 + Modu10(cCampo2)
	cCampo3:= cCampo3 + Modu10(cCampo3)

	cRepNum := Substr(cCampo1, 1, 5) + "." + Substr(cCampo1, 6, 5) + "  "
	cRepNum += Substr(cCampo2, 1, 5) + "." + Substr(cCampo2, 6, 6) + "  "
	cRepNum += Substr(cCampo3, 1, 5) + "." + Substr(cCampo3, 6, 6) + "  "
	cRepNum += cCampo4 + "  "
	cRepNum += cCampo5

Return

/******************************************/
// Rotinas de impressao do boleto do Sicred //
/******************************************/
/******************************************************************************************************************/
Static Function Bol_Sicred()
	/******************************************************************************************************************/
	Local oFont8
	Local oFont10
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	Local cBitMapBco := '\system\sicredi.jpg'
	Local i

	cForm1:= '001'
	cForm2:= '002'
	cForm3:= '003'

	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= NossoNumSi()
	Else
		cNossoNum:= transform( SE1->E1_NUMBCO, '@R 99/999999-9' )
	EndIf

	CdBarra_Sicr()

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	oPrint:SayBitmap(44 , 100, cBitMapBco, 390, 100)
	//oPrint:Say  (0084,100,'Sicredi',oFont16 )	// [2]Nome do Banco
	oPrint:Say  (0062,567,'748-X',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0200,100 ,alltrim(SM0->M0_NOMECOM)+" - "+transform( SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	oPrint:Say  (0150,1060,"Agência/Código Beneficiário"                         ,oFont8)
	cCedente:= cAgeEmp + '.03/' + SubStr(SEE->EE_CODEMP,1,5)
	oPrint:Say  (0200,1060,cCedente                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Pagador"                                         ,oFont8)
	oPrint:Say  (0300,100 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1410,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1410,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)
	//oPrint:SayBitmap(50 , 100, cBitMap, 250, 150)
	oPrint:SayBitmap(0604 , 100, cBitMapBco, 390, 100)
	//oPrint:Say  (0644,100,'Sicredi'  ,oFont16 )	// [2]Nome do Banco
	oPrint:Say  (0622,567,'748-X'       ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Pagador",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (0750,100 ,"PAGÁVEL PREFERENCIALMENTE NAS COOPERATIVAS DE CRÉDITO DO SICREDI"        ,oFont10)

	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2010,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0850,100 ,alltrim(SM0->M0_NOMECOM)+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ

	oPrint:Say  (0810,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (0850,2010,cCedente											,oFont10)

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                             ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (0940,2010,Padl(cNossoNum, 18)				                ,oFont10)

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (1010,555 ,'00' + cCartEmp                                	,oFont10)

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (1050,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE O BENEFICIÁRIO",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções
	/*
	cTexto := ''
	oPrint:Say  (1100,100 ,cTexto                                           ,oFont10)
	cTexto:= Formula(cForm1)
	oPrint:Say  (1150,100 ,cTexto                                           ,oFont10)
	cTexto:= Formula(cForm2)
	oPrint:Say  (1200,100 ,cTexto		                                    ,oFont10)
	cTexto:= Formula(cForm3)
	oPrint:Say  (1250,100 ,cTexto                                           ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1300,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (1605,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)

	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	// Encerra aqui a alteracao para o novo layout - RAI

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)
	//oPrint:SayBitmap(1270 , 100, cBitMapBco, 390, 120)
	oPrint:SayBitmap(1884 , 100, cBitMapBco, 390, 100)
	//oPrint:Say  (1934,100,'Sicredi',oFont16 )	// [2]Nome do Banco
	oPrint:Say  (1912,567,'748-X'   ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (1934,820,cRepNum   ,oFont14n)	//Linha Digitavel do Codigo de Barras

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"PAGÁVEL PREFERENCIALMENTE NAS COOPERATIVAS DE CRÉDITO DO SICREDI"        ,oFont10)

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (2140,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ

	oPrint:Say  (2100,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedente											,oFont10)

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,2010,Padl(cNossoNum, 18)                              ,oFont10)

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (2300,555 ,"00" + cCartEmp                                 	,oFont10)

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE O BENEFICIÁRIO",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	/*
	cTexto := cInst01
	oPrint:Say  (2390,100 ,cTexto                                           ,oFont10)
	cTexto := cInst02
	oPrint:Say  (2440,100 ,cTexto                                           ,oFont10)
	cTexto := cInst03
	oPrint:Say  (2490,100 ,cTexto		                                    ,oFont10)
	cTexto := cInst04
	oPrint:Say  (2540,100 ,cTexto                                           ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2590,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (2895,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	If cLayOut == "LASER"
		MSBAR("INT25",26,1.5,cCdBarra,oPrint,.F.,,,,1.2,,,,.F.)
	Else
		MSBAR3("INT25",23.4,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/******************************************************************************************************************/
Static Function NossoNumSi()
	/******************************************************************************************************************/
	local aArrayn 	:= {}
	Local aArrayx 	:= {4,3,2,9,8,7,6,5,4,3,2,9,8,7,6,5,4,3,2}
	Local cAgenciaSi:= cAgeEmp
	Local cPostoSi	:= "03"
	Local cCedenteSi:= SubStr(SEE->EE_CODEMP,1,5)
	Local cAnoAual	:= SubStr(cValToChar(Year(dDataBase)),3,2)
	Local cIndicSi	:= "2"
	//Local cA6__NOSSO:= StrZero(Val(alltrim(SEE->EE_FAXATU)), 5)
	Local cA6__NOSSO:= Right(AllTrim(SEE->EE_FAXATU),5)
	Local xi

	cCedenteSi:= SubStr(SEE->EE_CODEMP,1,5)

	//	agencia	 posto   	cedente 	ano 		 ind. geraçao NN		Sequencial
	cretorno1:= cAgenciaSi + cPostoSi + cCedenteSi	+ cAnoAual + cIndicSi	+ cA6__NOSSO

	cCalc := cRetorno1

	For xi := 1 to Len(cCalc)
		aadd(aArrayn,val(Substr(cCalc,xi,1)))
	next xi

	nSomaArray := 0
	For xi := 1 to len(aArrayX)
		nSomaArray  := nSomaArray+ ( aArrayx[xi] * aArrayn[xi] )
	next xi

	nDividendo := 11

	nResto := Mod(nSomaArray,nDividendo)
	nResto := 11 - nResto

	if nResto == 10 .or. nResto == 11
		cDigito := "0"
	else
		cDigito := AllTrim(Str(nResto))
	endif

	cTotNum   := cAnoAual + "/" + cIndicSi	+ Right(ALLTRIM(SEE->EE_FAXATU),5)+"-"+cDigito
	Cgrava    := cretorno1+alltrim(cDigito)

	cNssnum := cTotNum

	dbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	MsUnlock()

Return cNssnum

/******************************************************************************************************************/
Static Function CdBarra_Sicr()
	/******************************************************************************************************************/
	Local cFatorVen	:= Alltrim(Str(FatVencto())) // acha a diferenca em dias para o fator de vencimento
	Local xi
	Local i

	cPostoSi	:= "03"

	cCedenteSi:= SubStr(SEE->EE_CODEMP,1,5)

	cNumDig10 := space(10)
	cDigVerif := "0"
	cCampoLivre:= 	"1"+"1"+SubStr(cNossoNum,1,2)+SubStr(cNossoNum,4,6)+SubStr(cNossoNum,11,1)+PadL(cAgeEmp,4,"0") + ;
	cPostoSi + PadL(cCedenteSi,5,"0") + ; // **Posto da Cooperativa ** Codigo do Cedente   **
	"1" + "0"

	//Calculo do DV do campo livre
	aArrayn 	:= {}
	aArrayx 	:= {9,8,7,6,5,4,3,2,9,8,7,6,5,4,3,2,9,8,7,6,5,4,3,2}

	cCalc := cCampoLivre

	For xi := 1 to Len(cCalc)
		aadd(aArrayn,val(Substr(cCalc,xi,1)))
	next xi

	nSomaArray := 0
	For xi := 1 to len(aArrayX)
		nSomaArray  := nSomaArray+ ( aArrayx[xi] * aArrayn[xi] )
	next xi

	nResto := nSomaArray - ((Int(nSomaArray/11)) * 11)

	If nResto=0 .or. nResto=1
		cDigito := "0"
	Else
		nResto := 11 - nResto
		cDigito := AllTrim(Str(nResto))
	Endif

	cCampoLivre:= cCampoLivre +  cDigito

	cValor := STRZERO(SE1->E1_SALDO,14,2)
	cNewValor := ""
	nZeros := 0
	for i:=1 to len(cValor)
		if Substr(cValor,i,1) # "." .and. Substr(cValor,i,1) # ","
			cNewValor += Substr(cValor,i,1)
		ELSE
			nZeros ++
		endif
	next
	cValor := ""
	For i:=1 to nZeros
		cValor += "0"
	next
	cValor += Substr(Alltrim(cNewValor),5,10)

	//---------------> CALCULO DOS GRUPOS PARA O IPTE
	//1o GRUPO
	cNumDig10 := "748"+"9"+SubStr(cCampoLivre,1,5)
	cDigVerif := CalcDigMOD10(cNumDig10)//Calculo modulo 10, RETORNA O DIGITO VERIF.
	cPriGrupo := cNumDig10 + cDigVerif //RETORNA 10 POSICOES

	//2o Grupo
	cNumDig10 := SubStr(cCampoLivre,6,10)
	cDigVerif := CalcDigMOD10(cNumDig10)//Calculo modulo 10, RETORNA O DIGITO VERIF.
	cSegGrupo := cNumDig10 + cDigVerif //RETORNA 10 POSICOES

	//3o Grupo
	cNumDig10 := AllTrim(SubStr(cCampoLivre,16,10))
	cDigVerif := CalcDigMOD10(cNumDig10)//Calculo modulo 10, RETORNA O DIGITO VERIF.
	cTerGrupo := cNumDig10+cDigVerif

	//4o Grupo
	//----> DIGITO VERIF. DO COD BARRAS
	//cNumDig10 := cPriGrupo + cSegGrupo + cTerGrupo+ cFatorVen + cValor
	cNumDig10 := "748"+"9"+ cFatorVen + cValor + cCampoLivre
	cDigVerif := CalcDigM11()//Calculo modulo 11, RETORNA O DIGITO VERIF.
	cQuaGrupo := cDigVerif

	//---->MONTAGEM DO LAYOUT DA LINHA DIGITAVEL
	cLinhaDig := cPriGrupo + cSegGrupo + cTerGrupo + cQuaGrupo + cFatorVen + cValor
	//      cLinhaDigImp := substr(cLinhaDig,1,5) + "." + substr(cLinhaDig,6,5) + " "+ ;
	cRepNum := substr(cLinhaDig,1,5) + "." + substr(cLinhaDig,6,5) + " "+ ;
	substr(cLinhaDig,11,5)+ "." + substr(cLinhaDig,16,6) +" "+ ;
	substr(cLinhaDig,22,5)+ "." + substr(cLinhaDig,27,6) +" "+ ;
	substr(cLinhaDig,33,1)+ " " + substr(cLinhaDig,34,14)

	//----> MONTAGEM CODIGO DE BARRAS PARA SER IMPRESSO
	//cCodBarAPT := cBancoM + "9" +cQuaGrupo + cFatorVen + cValor +SUBSTR(cAgenci,1,4)+ cCartei + cNossoNum + SUBSTR(STRZERO(Val(cContac),8),1,7) +"0"
	//		cCodBarAPT 	:= "748" + "9" +cQuaGrupo + cFatorVen + cValor + cCampoLivre
	cCdBarra := "748" + "9" +cQuaGrupo + cFatorVen + cValor + cCampoLivre

return

/*****************************************************/
// Rotinas de impressao do boleto da Caixa Federal // 																// Customizado para tratar Boleto da Caixa, #Rafael Achôa 12-08-14
/*****************************************************/
/******************************************************************************************************************/
Static Function Bol_CEF()
	/******************************************************************************************************************/
	Local oFont8
	Local oFont10
	Local oFont13
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	Local aCoords1:= {2250,1900,2350,2300}
	Local aCoords2:= {2520,1900,2590,2300}
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	Local cBitmapBco:= "\system\CEF.bmp"
	//Local cInst01 := SEE-> EE_X_INS01
	//Local cInst02 := SEE-> EE_X_INS02
	//Local cInst03 := SEE-> EE_X_INS03
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	LOCAL cBolText := ''
	LOCAL nMultaLj := val (GetMV( 'MV_X_LJMLT' )) //MV_LJMULTA
	LOCAL nJurosLj := GetMV( 'MV_LJJUROS' )

	nValor:= Round(SE1->E1_SALDO + SE1->E1_X_MULTA - SE1->E1_IRRF - SE1->E1_INSS - SE1->E1_PIS - SE1->E1_COFINS - SE1->E1_CSLL, 2)//- SE1->E1_ISS - TIRADO DA REGRA PELO FATO DE TER HABILITADO O PARAMETRO MV_TPABISS (TITULO JA GERA COM DESCONTO)
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_CEF()
	Else
		//Alterado para contemplar o novo Nosso Numero - Heliton 11/07/17
		//cNossoNum:= Alltrim(SEE->EE_CODEMP) + Transform( SE1->E1_NUMBCO, '@R XXXXXXXXXX-X' )
		cNossoNum:= Transform(alltrim(SE1->E1_NUMBCO), '@R XXXXXXXXXXXXXXXXX-X' )
	EndIf

	If !SE1->E1_PORCJUR == 0
		nJurosLj := SE1->E1_PORCJUR
	EndIf

	CdBarra_CEF()

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)
	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont13 := TFont():New("Arial",9,13,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	/*Alterado para contemplar novo layout Caixa - Heliton 11/07/17
	oPrint:Say  (0050,100,'Caixa Economica',oFont14n )	// [2]Nome do Banco
	oPrint:Say  (0088,100,'Federal',oFont14n )	// [2]Nome do Banco
	*/
	oPrint:SayBitmap(44 , 100, cBitMapBco, 390, 100)
	//Fim Alteração
	oPrint:Say  (0062,567,'104-0',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Cedente"                                        ,oFont8)
	oPrint:Say  (0200,100 ,SubStr(alltrim(SM0->M0_NOMECOM),1,45) ,oFont10) //Nome + CNPJ
	oPrint:Say  (0150,1060,"Agência/Código Cedente"                         ,oFont8)
	//Alterado para impressão novo layout - Heliton 13/07/17
	//cCedente:=  SEE->EE_X_CEDEN
	cCedente:=  AllTrim(Transform(SEE->EE_X_CEDEN, "@R 9999/999999-9"))
	oPrint:Say  (0200,1060,cCedente                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Sacado"                                         ,oFont8)
	oPrint:Say  (0300,100 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Emissao"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_EMISSAO))                 ,oFont10)
	oPrint:Say  (0250,1305,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1305,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1305,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1305,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0560,1300,0250,1300 ) //oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)

	/* Alterado para contemplar novo layout Caixa - Heliton 11/07/17
	//oPrint:Say  (0606,100,'Caixa Economica'   ,oFont14n )	// [2]Nome do Banco
	//oPrint:Say  (0644,100,'Federal'   ,oFont14n )	// [2]Nome do Banco
	*/
	oPrint:SayBitmap(0604 , 100, cBitMapBco, 390, 100)
	//Fim Alteração

	oPrint:Say  (0622,567,'104-0'       ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Sacado",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (0750,100 ,"PREFERENCIALMENTE NAS CASAS LOTÉRICAS ATÉ O VALOR LIMITE"        ,oFont10)

	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2010,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Cedente"                                        ,oFont8)
	oPrint:Say  (0850,100 ,alltrim(SM0->M0_NOMECOM)+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99') + " - " + alltrim(SM0->M0_ENDCOB),oFont10) //Nome + CNPJ

	oPrint:Say  (0810,1910,"Agência/Código Cedente"                         ,oFont8)
	oPrint:Say  (0850,2010,cCedente											,oFont10)

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                             ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (0940,1950,Padl(cNossoNum, 19)				                ,oFont10)

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (1010,555 , cCartEmp                                     	,oFont10)

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,AllTrim(cValor) 									,oFont10)
	oPrint:Say  (1050,100 ,"Instruções (Texto de responsabilidade do cedente)",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções
	/*
	cTexto := cInst01
	oPrint:Say  (1100,100 ,cTexto                                           ,oFont10)
	cTexto := cInst02
	oPrint:Say  (1150,100 ,cTexto                                           ,oFont10)
	cTexto := cInst03
	oPrint:Say  (1200,100 ,cTexto                                           ,oFont10)
	cTexto := cInst04
	oPrint:Say  (1250,100 ,cTexto		                                    ,oFont10)
	*/
	cTexto := ''
	nDescFin:= SC5->C5_DESCFI
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1400,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Sacado"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (1605,100 ,"Sacador/Avalista"                               ,oFont8)
	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)

	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	// Encerra aqui a alteracao para o novo layout - RAI

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)

	/*Alteração para contemplar novo layout caixa - Heliton 11/07/17
	oPrint:Say  (1896,100,'Caixa Economica',oFont14n )	// [2]Nome do Banco
	oPrint:Say  (1934,100,'Federal',oFont14n )	// [2]Nome do Banco
	*/
	oPrint:SayBitmap(1884 , 100, cBitMapBco, 390, 100)
	//Fim alteração

	oPrint:Say  (1912,567,'104-0'   ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (1934,820,cRepNum   ,oFont14n)	//Linha Digitavel do Codigo de Barras

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"QUALQUER BANCO ATÉ A DATA DO VENCIMENTO"        ,oFont10)

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Cedente"                                        ,oFont8)
	oPrint:Say  (2140,100 ,alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ

	oPrint:Say  (2100,1910,"Agência/Código Cedente"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedente											,oFont10)

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,1950,Padl(cNossoNum, 19)                              ,oFont10)

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (2300,555 , + cCartEmp	                                 	,oFont10)

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"Instruções (Texto de responsabilidade do cedente)",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	/*
	cTexto := cInst01
	oPrint:Say  (2390,100 ,cTexto                                           ,oFont10)
	cTexto := cInst02
	oPrint:Say  (2440,100 ,cTexto                                           ,oFont10)
	cTexto := cInst03
	oPrint:Say  (2490,100 ,cTexto                                           ,oFont10)
	cTexto := cInst04
	oPrint:Say  (2540,100 ,cTexto		                                    ,oFont10)
	*/
	cTexto := ''
	nDescFin:= SC5->C5_DESCFI
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2640,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Sacado"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (2895,100 ,"Sacador/Avalista"                               ,oFont8)
	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	If cLayOut == "LASER"
		MSBAR("INT25",26,1.5,cCdBarra,oPrint,.F.,,,,1.2,,,,.F.)
	Else
		MSBAR3("INT25",23.4,2.0,cCdBarra,oPrint,.F.,,,,1.3,,,,.F.)
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/******************************************************************************************************************/
Static Function RetNN_CEF()
	/******************************************************************************************************************/
	// Nosso Numero da Caixa Federal:
	//Formato: XYNNNNNNNNNNNNNNN-D, onde:
	//X Modalidade/Carteira de Cobrança (1-Registrada/2-Sem Registro)
	//Y Emissão do boleto (4-Beneficiário)
	//NNNNNNNNNNNNNNN Nosso Número (15 posições livres do Beneficiário)
	//D *Dígito Verificador

	Local cA6__NOSSO:= StrZero(Val(alltrim(SEE->EE_FAXATU)), 8)
	Local cTexto:= ""
	Local cRet:= ""

	//Alterado devido mudança layout caixa - Heliton 10/07/17
	//cTexto:= "90" + cA6__NOSSO
	cTexto:= "14" + StrZero(Val(alltrim(SEE->EE_FAXATU)), 15)
	cRet:= cTexto + '-' + Modu11_CEF(cTexto)

	/* Atualizar o nosso numero no cadastro do banco */
	DbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cA6__NOSSO)
	MsUnlock()

Return cRet

/******************************************************************************************************************/
Static Function Modu11_CEF(cLinha)
/******************************************************************************************************************/
	
	//cDigRet := u_DV1_CEF(cLinha)	
	Local cDigRet
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local nFator:= 2

	For nCont:= Len(cLinha) To 1 Step -1
		nFator--
		If nFator < 2
			nFator:= 9
		EndIf

		nSoma += Val(Substr(cLinha, nCont, 1)) * nFator
	Next nCont

	nResto:= Mod(nSoma, 11)	
	//nResto := 11 - nResto

	If nResto > 9
		cDigRet:= "0"
	Else
		cDigRet:= StrZero(nResto, 1)
	EndIf 
Return cDigRet  

/* Copiado de outro fonte

User Function DV1_CEF( cNossoNum )
Local nDV      := 0
Local nAux     := 0
Local cBase    := cNossoNum
Local cPesos   := "987654329876543298765432987654329876543298765432987654329876543298765432"
Local nSoma    := 0
Local nProduto := 0

	cPesos := Right( cPesos, Len( cNossoNum ) )
	For nAux := 1 to Len( cBase )
		nProduto := Val( SubStr( cBase, nAux, 1 ) ) * Val( SubStr( cPesos, nAux, 1 ) )
		nSoma += nProduto
	Next
	nSoma := nSoma % 11
	nDV   := 11 - nSoma
	If nDV > 9
		nDV := 1
	EndIf
Return StrZero( nDV, 1 ) */

/******************************************************************************************************************/
Static Function CdBarra_CEF()
	/******************************************************************************************************************/
	Local cDigCdBarra
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCampo1:= ""
	Local cCampo2:= ""
	Local cCampo3:= ""
	Local cCampo4:= ""
	Local cCampo5:= ""
	Local cDigCpLivre
	Private cCpLivre

	cFatVencto:= StrZero(FatVencto(), 4)
	nValor:= Round(SE1->E1_SALDO + SE1->E1_X_MULTA - SE1->E1_IRRF - SE1->E1_INSS - SE1->E1_PIS - SE1->E1_COFINS - SE1->E1_CSLL, 2)//SE1->E1_ISS
	cValor:= StrZero(nValor * 100, 10)

	/* Calculo do codigo de barras */
	//Alterado para contemplar novo layout caixa. Heliton 14/07/17
	cCpLivre := .T.
	cDigCpLivre:= Modu11_CEF(Substr(alltrim(SEE->EE_X_CEDEN),5,7) + Substr(cNossoNum,3,3) + Substr(cNossoNum,1,1) + Substr(cNossoNum,6,3) + Substr(cNossoNum,2,1) + Substr(cNossoNum,9,9))
	//cCdBarra:= SA6->A6_COD + "9" + cFatVencto + cValor + Substr(cNossoNum, 1, 10) + Substr (alltrim(SEE->EE_X_CEDEN), 1, 15)
	cCdBarra:= SA6->A6_COD + "9" + cFatVencto + cValor + Substr(alltrim(SEE->EE_X_CEDEN),5,7) + Substr(cNossoNum,3,3) + Substr(cNossoNum,1,1) + Substr(cNossoNum,6,3) + Substr(cNossoNum,2,1) + Substr(cNossoNum,9,9) + cDigCpLivre
	cCpLivre := .F.
	//cDigCdBarra:= Modu11_CEF(cCdBarra) diego
	cDigCdBarra:= Modu11(cCdBarra)

	//Alterado para contemplar novo layout caixa. Heliton 14/07/17
	//cCdBarra:= SA6->A6_COD + "9" + cDigCdBarra + cFatVencto + cValor + Substr(cNossoNum, 1, 10) + Substr (alltrim(SEE->EE_X_CEDEN), 1, 15)
	cCdBarra:= SA6->A6_COD + "9" + cDigCdBarra + cFatVencto + cValor + Substr(alltrim(SEE->EE_X_CEDEN),5,7) + Substr(cNossoNum,3,3) + Substr(cNossoNum,1,1) + Substr(cNossoNum,6,3) + Substr(cNossoNum,2,1) + Substr(cNossoNum,9,9) + cDigCpLivre

	memowrite("i:/home/kleber/cdbarras_"+SE1->E1_PARCELA+".txt",cCdBarra)

	/* Calculo da representacao numerica */
	cCampo1:= SA6->A6_COD + "9" + Substr(cCdBarra,20,5)
	cCampo2:= Substr(cCdBarra,25,10)
	cCampo3:= Substr(cCdBarra,35,10)

	//cCampo1:= SA6->A6_COD + "9" + '00000'
	//cCampo2:= '0' + Substr(cNossoNum, 1, 9)
	//cCampo3:= Substr(cNossoNum, 10, 8) + cCartEmp
	cCampo4:= cDigCdBarra
	cCampo5:= cFatVencto + cValor

	/* Calculando os DACs dos campos 1, 2 e 3 */
	cCampo1:= cCampo1 + Modu10(cCampo1)
	cCampo2:= cCampo2 + Modu10(cCampo2)
	cCampo3:= cCampo3 + Modu10(cCampo3)

	cRepNum := Substr(cCampo1, 1, 5) + "." + Substr(cCampo1, 6, 5) + "  "
	cRepNum += Substr(cCampo2, 1, 5) + "." + Substr(cCampo2, 6, 6) + "  "
	cRepNum += Substr(cCampo3, 1, 5) + "." + Substr(cCampo3, 6, 6) + "  "
	cRepNum += cCampo4 + "  "
	cRepNum += cCampo5

Return
/*/
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³          ³ Autor ³ PETERSON              ³ Data ³ 27.06.03 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Calcula o Digito Verificador MOD10                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ DIGITO da linha digitavel alterado por Silvio 04/01/05     ³±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function CalcDigMOD10()
	Local i
	nCont  	:= 2
	cDigito := "0"
	nSoma  	:= 0
	nAux 	:= 0

	For i:=Len(cNumDig10) to 1 step -1

		nAux := Val(Substr(cNumDig10,i,1)) * nCont
		if  nAux > 9
			nSoma += Val(Substr(Strzero(nAux,2),1,1)) + Val(Substr(Strzero(nAux,2),2,1))
		else
			nSoma += nAux
		endif

		if nCont == 2
			nCont := 1
		else
			nCont := 2
		endif

	Next i

	if 10-mod(nSoma,10) > 9
		cDigito := "0"
	else
		nDigito := 10-mod(nSoma,10)
		cDigito := Strzero(nDigito,1)
	endif
return(cDigito)

/***************************************************************************/
/******************************************/
// Rotinas de impressao do boleto do ABC BRASIL //
/******************************************/
/******************************************************************************************************************/
Static Function Bol_ABC()
	/******************************************************************************************************************/
	Local oFont8
	Local oFont9  //Customizado para criação de novo tamanho de fonte, #Rafael Achôa 14-05-2014
	Local oFont10
	Local oFont14n
	Local oFont16
	Local oFont16n
	Local oFont24
	LOCAL aCoords1 := {0150,1900,0550,2300}
	LOCAL aCoords2 := {0450,1050,0550,1900}
	LOCAL aCoords3 := {0710,1900,0810,2300}
	LOCAL aCoords4 := {0980,1900,1050,2300}
	LOCAL aCoords5 := {1330,1900,1400,2300}
	LOCAL aCoords6 := {2000,1900,2100,2300}
	LOCAL aCoords7 := {2270,1900,2340,2300}
	LOCAL aCoords8 := {2620,1900,2690,2300}
	Local oBrush
	Local cTexto
	Local cValor
	Local nValor
	Local cNumDoc
	Local cCedente
	Local cForm1
	Local cForm2
	Local nDescFin
	//Local cInst01 := mv_par10+mv_par11
	//Local cInst02 := mv_par12+mv_par13
	//Local cInst03 := mv_par14+mv_par15
	Local cBitmapBco:= "\system\LGABC.bmp"
	Local i

	cForm1:= ''
	cForm2:= ''
	cForm3:= ''

	nValor:= IIf(!Empty(SE1->E1_SALDO),Round(SE1->E1_SALDO, 2),Round(SE1->E1_VALOR, 2))
	// msginfo(SE1->E1_SALDO,"saldo")  //Customizado para não aparecer mais o Saldo do Título antes de imprimir o Relatório #Rafael Achôa, 15/05/2014
	cValor:= AllTrim(Transform(nValor, "@E 999,999,999.99"))
	cNumDoc:= SE1->E1_PREFIXO + SE1->E1_NUM + SE1->E1_PARCELA

	If Empty(SE1->E1_NUMBCO)
		cNossoNum:= RetNN_ABC()
	Else
		//cNossoNum:= transform( SE1->E1_NUMBCO, '@R 999/99999999-9' )
		cNossoNum:= ALLTRIM(transform( SE1->E1_NUMBCO, '@R 99999999999' ))
		cNossoNum:= substr(cNossoNum,1,len(cNossoNum)-1)+'-'+substr(cNossoNum,len(cNossoNum),1)
	EndIf

	//If Empty(SE1->E1_CODBAR) // Customizado para verificar se já não existe o código de barras daquele título, #Rafael Achôa, 15/05/2014
	CdBarra_ABC()
	//Endif

	//Parâmetros de TFont.New()
	//1.Nome da Fonte (Windows)
	//3.Tamanho em Pixels
	//5.Bold (T/F)

	oCouNew10N:= TFont():New("Courier New",10,10,,.T.,,,,.T.,.F.)

	oFont8  := TFont():New("Arial",9,8 ,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont9  := TFont():New("Arial",9,07,.T.,.T.,5,.T.,5,.T.,.F.) //Customizado para criação de novo tamanho de fonte, #Rafael Achôa 14-05-2014
	oFont10 := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14n:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont16 := TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n:= TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24 := TFont():New("Arial",9,23,.T.,.T.,5,.T.,5,.T.,.F.)

	oBrush := TBrush():New("",CLR_LIGHTGRAY)    // 4)

	oPrint:StartPage()   // Inicia uma nova página

	oPrint:FillRect(aCoords1,oBrush)
	oPrint:FillRect(aCoords2,oBrush)
	oPrint:FillRect(aCoords3,oBrush)
	oPrint:FillRect(aCoords4,oBrush)
	oPrint:FillRect(aCoords5,oBrush)
	oPrint:FillRect(aCoords6,oBrush)
	oPrint:FillRect(aCoords7,oBrush)
	oPrint:FillRect(aCoords8,oBrush)

	// Inicia aqui a alteracao para novo layout - RAI
	oPrint:Line (0150,550,0050, 550)
	oPrint:Line (0150,800,0050, 800)
	If File(cBitmapBco)
		oPrint:SayBitmap( 0040, 0100, cBitmapBco, 0355,0115)
	Else
		oPrint:Say  (0084,100,'ABC Brasil',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0062,567,'246-1',oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0084,1900,"Comprovante de Entrega",oFont10)
	oPrint:Line (0150,100,0150,2300)
	oPrint:Say  (0150,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (0200,100 ,Substr(alltrim(SM0->M0_NOMECOM),1,30)+" - "+transform( SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont10) //Nome + CNPJ
	oPrint:Say  (0150,1060,"Agência/Código Beneficiário"                         ,oFont8)
	cCedente:= cAgeEmp + '/' + cCtaEmp + '-' + cDigEmp
	oPrint:Say  (0200,1060,cCedente                                         ,oFont10)
	oPrint:Say  (0150,1510,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0200,1510,cNumDoc                       					,oFont10) //Prefixo +Numero+Parcela
	oPrint:Say  (0250,100 ,"Pagador"                                         ,oFont8)
	oPrint:Say  (0300,100 ,SubStr(AllTrim(SA1->A1_NOME),1,30) + " (" + AllTrim(SA1->A1_COD) + ")",oFont10)	//Nome + Codigo
	oPrint:Say  (0250,1060,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0300,1060,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)
	oPrint:Say  (0250,1510,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (0300,1550,AllTrim(cValor)									,oFont10)
	oPrint:Say  (0400,0100,"Recebi(emos) o bloqueto/título"                 ,oFont10)
	oPrint:Say  (0450,0100,"com as características acima."             		,oFont10)
	oPrint:Say  (0350,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0350,1410,"Assinatura"                                 	,oFont8)
	oPrint:Say  (0450,1060,"Data"                                           ,oFont8)
	oPrint:Say  (0450,1410,"Entregador"                                 	,oFont8)

	oPrint:Line (0250, 100,0250,1900 )
	oPrint:Line (0350, 100,0350,1900 )
	oPrint:Line (0450,1050,0450,1900 ) //---
	oPrint:Line (0550, 100,0550,2300 )

	oPrint:Line (0550,1050,0150,1050 )
	oPrint:Line (0550,1400,0350,1400 )
	oPrint:Line (0350,1500,0150,1500 ) //--
	oPrint:Line (0550,1900,0150,1900 )

	oPrint:Say  (0160,1910,"(  )Mudou-se"                                	,oFont8)
	oPrint:Say  (0200,1910,"(  )Ausente"                                    ,oFont8)
	oPrint:Say  (0240,1910,"(  )Não existe nº indicado"                  	,oFont8)
	oPrint:Say  (0280,1910,"(  )Recusado"                                	,oFont8)
	oPrint:Say  (0320,1910,"(  )Não procurado"                              ,oFont8)
	oPrint:Say  (0360,1910,"(  )Endereço insuficiente"                  	,oFont8)
	oPrint:Say  (0400,1910,"(  )Desconhecido"                            	,oFont8)
	oPrint:Say  (0440,1910,"(  )Falecido"                                   ,oFont8)
	oPrint:Say  (0480,1910,"(  )Outros(anotar no verso)"                  	,oFont8)

	For i := 100 to 2300 step 50
		oPrint:Line( 0600, i, 0600, i+30)
	Next i

 	oPrint:Line (0710,100,0710,2300)
	oPrint:Line (0710,550,0610, 550)
	oPrint:Line (0710,800,0610, 800)
	If File(cBitmapBco)
		oPrint:SayBitmap( 0603, 0100, cBitmapBco, 0355, 0105 )
	Else
		oPrint:Say  (0644,100,'ABC Brasil'   ,oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (0622,567,'246-1'       ,oFont24 )	// [1]Numero do Banco
	oPrint:Say  (0644,1900,"Recibo do Pagador",oFont10)

	oPrint:Line (0810,100,0810,2300 )
	oPrint:Line (0910,100,0910,2300 )
	oPrint:Line (0980,100,0980,2300 )
	oPrint:Line (1050,100,1050,2300 )

	oPrint:Line (0910,500,1050,500)
	oPrint:Line (0980,750,1050,750)
	oPrint:Line (0910,1000,1050,1000)
	oPrint:Line (0910,1350,0980,1350)
	oPrint:Line (0910,1550,1050,1550)

	oPrint:Say  (0710,100 ,"Local de Pagamento"                             ,oFont8)
	//oPrint:Say  (0750,100 ,"QUALQUER BANCO ATÉ A DATA DO VENCIMENTO"        ,oFont10)
	oPrint:Say  (0750,100 ,"Até o vencimento, preferencialmente no ABC Brasil. Após o vencimento, somente no ABC Brasil."        ,oFont10)

	oPrint:Say  (0710,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (0750,2031,FormatData(DTOS(SE1->E1_VENCTO))                 ,oFont10)

	oPrint:Say  (0810,100 ,"Beneficiário"                                   ,oFont8)
	oPrint:Say  (0850,100 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" || "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont9) //Nome + CNPJ      //Customizado para Nome + Endereço completo + CNPJ, tamanho 9 #Rafael Achôa, 14-05-2014

	oPrint:Say  (0810,1910,"Agência/Código Beneficiário"                    ,oFont8)
	oPrint:Say  (0850,1995,cCedente											,oFont10)

	oPrint:Say  (0910,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (0940,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (0910,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (0940,605 ,cNumDoc											,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (0910,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (0940,1050,"DM"												,oFont10) //Tipo do Titulo

	oPrint:Say  (0910,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (0940,1455,"N"                                              ,oFont10)

	oPrint:Say  (0910,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (0940,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	//oPrint:Say  (0910,1910,"Nosso Número"                                   ,oFont8)
	//oPrint:Say  (0940,2010,Padl(cNossoNum, 18)				                ,oFont10)

	oPrint:Say  (910,1910,"Nosso Número"                                    ,oFont8)
	oPrint:Say  (0940,1950,cAgeEmp + '/'+ cCartEmp +'/'+ Padl(cNossoNum, 12),oFont10)

	oPrint:Say  (0980,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (0980,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (1010,555 ,'00' + cCartEmp                                	,oFont10)

	oPrint:Say  (0980,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (1010,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (0980,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (0980,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (0980,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (1010,2010,Padl(AllTrim(cValor),16) 						,oFont10)

	oPrint:Say  (1050,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,1050) //Imprime instruções
	/*cTexto := cInst01
	oPrint:Say  (1100,100 ,cTexto                                           ,oFont10)
	cTexto := cInst02
	oPrint:Say  (1150,100 ,cTexto                                           ,oFont10)
	cTexto := cInst03+CHR(10)+CHR(13)+cInst04
	oPrint:Say  (1200,100 ,cTexto		                                    ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (1300,100 ,cTexto                                           ,oFont10)
	cTexto := ''
	oPrint:Say  (1200,1000,cTexto                                           ,oFont10) //PIS
	oPrint:Say  (1250,1000,cTexto                                           ,oFont10) //COFINS
	oPrint:Say  (1300,1000,cTexto                                           ,oFont10) //CSLL
	oPrint:Say  (1350,100 ,cTexto                                           ,oFont10) //OUTROS ABATIMENTOS

	oPrint:Say  (1050,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (1080,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif */
	oPrint:Say  (1120,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (1190,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (1260,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (1330,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (1400,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (1430,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")     CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(1483, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(1536, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(1483, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(1536, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (1605,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (1645,1500,"Autenticação Mecânica -"                        ,oFont8)

	oPrint:Line (0710,1900,1400,1900 )
	oPrint:Line (1120,1900,1120,2300 )
	oPrint:Line (1190,1900,1190,2300 )
	oPrint:Line (1260,1900,1260,2300 )
	oPrint:Line (1330,1900,1330,2300 )
	oPrint:Line (1400,100 ,1400,2300 )
	oPrint:Line (1640,100 ,1640,2300 )

	For i := 100 to 2300 step 50
		oPrint:Line( 1850, i, 1850, i+30)
	Next i

	// Encerra aqui a alteracao para o novo layout - RAI

	oPrint:Line (2000,100,2000,2300)
	oPrint:Line (2000,550,1900, 550)
	oPrint:Line (2000,800,1900, 800)
	If File (cBitmapBco)
		oPrint:SayBitmap( 1865, 0100, cBitmapBco, 0355, 0115 )
	Else
		oPrint:Say  (1934,100,'ABC Brasil',oFont16 )	// [2]Nome do Banco
	Endif
	oPrint:Say  (1912,567,'246-1'   ,oFont24 )	// [1]Numero do Banco
	// MsgInfo(cRepNum,'cRepNum') //Customizado para não aparecer mensagem de informação com o Número da linha digitável do Código de Barras #Rafael Achôa, 15/05/2014
	oPrint:Say  (1934,820,/*cRepNum  */Alltrim(SE1->E1_CODDIG) ,oFont14n)	//Linha Digitavel do Codigo de Barras  //Customizado para trazer direto da tabela de Títulos. #Rafael Achôa, 15/05/2014

	oPrint:Line (2100,100,2100,2300 )
	oPrint:Line (2200,100,2200,2300 )
	oPrint:Line (2270,100,2270,2300 )
	oPrint:Line (2340,100,2340,2300 )

	oPrint:Line (2200,500,2340,500)
	oPrint:Line (2270,750,2340,750)
	oPrint:Line (2200,1000,2340,1000)
	oPrint:Line (2200,1350,2270,1350)
	oPrint:Line (2200,1550,2340,1550)

	oPrint:Say  (2000,100 ,"Local de Pagamento"                             ,oFont8)
	oPrint:Say  (2040,100 ,"Até o vencimento, preferencialmente no ABC Brasil. Após o vencimento, somente no ABC Brasil."        ,oFont10)

	oPrint:Say  (2000,1910,"Vencimento"                                     ,oFont8)
	oPrint:Say  (2040,2010,FormatData(DTOS(SE1->E1_VENCTO))          		,oFont10)

	oPrint:Say  (2100,100 ,"Beneficiário"                                        ,oFont8)
	oPrint:Say  (2140,100 ,alltrim(SM0->M0_NOMECOM)+" - "+Alltrim(SM0->M0_ENDCOB)+", "+Alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)+" || "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99')/*alltrim( SM0->M0_NOMECOM )+" - "+transform(SM0->M0_CGC,'@R 99.999.999/9999-99')*/,oFont9) //Nome + CNPJ  //Customizado para Nome + Endereço completo + CNPJ, tamanho 9 #Rafael Achôa, 14-05-2014

	oPrint:Say  (2100,1910,"Agência/Código Beneficiário"                         ,oFont8)
	oPrint:Say  (2140,2010,cCedente											,oFont10)

	oPrint:Say  (2200,100 ,"Data do Documento"                              ,oFont8)
	oPrint:Say  (2230,100 ,FormatData(DTOS(dDataBase))                      ,oFont10) // Emissao do Titulo (E1_EMISSAO)

	oPrint:Say  (2200,505 ,"Nro.Documento"                                  ,oFont8)
	oPrint:Say  (2230,605 ,cNumDoc                     						,oFont10) //Prefixo +Numero+Parcela

	oPrint:Say  (2200,1005,"Espécie Doc."                                   ,oFont8)
	oPrint:Say  (2230,1050,"DM"                                         	,oFont10) //Tipo do Titulo

	oPrint:Say  (2200,1355,"Aceite"                                         ,oFont8)
	oPrint:Say  (2230,1455,"N"                                             ,oFont10)

	oPrint:Say  (2200,1555,"Data do Processamento"                          ,oFont8)
	oPrint:Say  (2230,1655,FormatData(DTOS(SE1->E1_EMISSAO))                ,oFont10) // Data impressao

	oPrint:Say  (2200,1910,"Nosso Número"                                   ,oFont8)
	oPrint:Say  (2230,1950,cAgeEmp + '/'+ cCartEmp+'/'+ Padl(cNossoNum, 12) ,oFont10)

	oPrint:Say  (2270,100 ,"Uso do Banco"                                   ,oFont8)

	oPrint:Say  (2270,505 ,"Carteira"                                       ,oFont8)
	oPrint:Say  (2300,555 ,"00" + cCartEmp                                 	,oFont10)

	oPrint:Say  (2270,755 ,"Espécie"                                        ,oFont8)
	oPrint:Say  (2300,805 ,"R$"                                             ,oFont10)

	oPrint:Say  (2270,1005,"Quantidade"                                     ,oFont8)
	oPrint:Say  (2270,1555,"Valor"                                          ,oFont8)

	oPrint:Say  (2270,1910,"Valor do Documento"                          	,oFont8)
	oPrint:Say  (2300,2010,AllTrim(cValor) 									,oFont10)

	oPrint:Say  (2340,100 ,"INSTRUÇÕES DE RESPONSABILIDADE DO BENEFICIÁRIO. QUALQUER DÚVIDA SOBRE ESTE BOLETO, CONTATE-O",oFont8)
	U_ImpIT(oFont10,2340) //Imprime instruções
	/*cTexto := cInst01
	oPrint:Say  (2390,100 ,cTexto                                           ,oFont10)
	cTexto := cInst02
	oPrint:Say  (2440,100 ,cTexto                                           ,oFont10)
	cTexto := cInst03+CHR(10)+CHR(13)+cInst04
	oPrint:Say  (2540,100 ,cTexto                                           ,oFont10)
	*/
	cTexto := ''
	nDescFin:= IIf(!Empty(SC5->C5_DESCFI),SC5->C5_DESCFI,SE1->E1_DESCFIN)
	If !Empty(nDescFin)
		cTexto:= "DESCONTO DE:" + Transform(nDescFin, "@E 99.99%") + " = R$ " + Transform(((SE1->E1_SALDO * nDescFin)/100), "@E 999,999.99") + " ATÉ O VENCIMENTO"
	EndIf
	oPrint:Say  (2590,100 ,cTexto                                           ,oFont10) //INSS
	cTexto := ''
	oPrint:Say  (2640,1000,cTexto                                           ,oFont10) //PIS

	oPrint:Say  (2340,1910,"(-)Desconto/Abatimento"                         ,oFont8)
	If SE1->E1_DECRESC > 0
		oPrint:Say  (2370,2010,AllTrim(Transform(SE1->E1_DECRESC,"@E 999,999,999.99")),oFont10)
	Endif
	oPrint:Say  (2410,1910,"(-)Outras Deduções"                             ,oFont8)
	oPrint:Say  (2480,1910,"(+)Mora/Multa"                                  ,oFont8)
	oPrint:Say  (2550,1910,"(+)Outros Acréscimos"                           ,oFont8)
	oPrint:Say  (2620,1910,"(=)Valor Cobrado"                               ,oFont8)

	oPrint:Say  (2690,100 ,"Pagador"                                         ,oFont8)
	if len( alltrim( SA1->A1_CGC ) ) > 11
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CNPJ: " + Substr(SA1->A1_CGC, 1, 2) + "." + Substr(SA1->A1_CGC, 3, 3);
		+ "." + Substr(SA1->A1_CGC, 6, 3) + "/" + Substr(SA1->A1_CGC, 9, 4) + "-" + Substr(SA1->A1_CGC, 13, 2),oFont10)
	else
		oPrint:Say  (2720,400 ,AllTrim(SA1->A1_NOME) + " (" + AllTrim(SA1->A1_COD) + ")    CPF: " + Substr(SA1->A1_CGC, 1, 3) + "." + Substr(SA1->A1_CGC, 4, 3);
		+ "." + Substr(SA1->A1_CGC, 7, 3) + "-" + Substr(SA1->A1_CGC, 10, 2),oFont10)
	endif
	If !Empty(SA1->A1_ENDCOB)
		cTexto:= Substr(SA1->A1_CEPC, 1, 5) + '-' + Substr(SA1->A1_CEPC, 6, 3) + "     " + SA1->A1_MUNC + "     " + SA1->A1_ESTC
		oPrint:Say(2773, 400, AllTrim(SA1->A1_ENDCOB), oFont10)
		oPrint:Say(2826, 400, cTexto				 , oFont10)
	Else
		cTexto:= Substr(SA1->A1_CEP, 1, 5) + '-' + Substr(SA1->A1_CEP, 6, 3) + "     " + SA1->A1_MUN + "     " + SA1->A1_EST
		oPrint:Say(2773, 400, AllTrim(SA1->A1_END), oFont10)
		oPrint:Say(2826, 400, cTexto              , oFont10)
	EndIf

	oPrint:Say  (2895,100 ,"Pagador/Avalista"                               ,oFont8)
	oPrint:Say  (2935,1500,"Autenticação Mecânica -"                        ,oFont8)
	oPrint:Say  (2935,1850,"Ficha de Compensação"                           ,oFont10)

	oPrint:Line (2000,1900,2690,1900 )
	oPrint:Line (2410,1900,2410,2300 )
	oPrint:Line (2480,1900,2480,2300 )
	oPrint:Line (2550,1900,2550,2300 )
	oPrint:Line (2620,1900,2620,2300 )
	oPrint:Line (2690,100 ,2690,2300 )

	oPrint:Line (2930,100,2930,2300  )
	If cLayOut == "LASER"
		MSBAR("INT25",26,1.5,/*cCdBarra*/SE1->E1_CODBAR,oPrint,.F.,,,,1.2,,,,.F.) //Customizado para pegar Código de Barras direto do título #Rafael Achôa, 15/05/2014
	Else
		MSBAR3("INT25",23.4,2.0,/*cCdBarra*/SE1->E1_CODBAR,oPrint,.F.,,,,1.3,,,,.F.)  //Customizado para pegar Código de Barras direto do título #Rafael Achôa, 15/05/2014
	EndIf

	oPrint:EndPage() // Finaliza a página

Return

/******************************************************************************************************************/
Static Function RetNN_ABC()
	/******************************************************************************************************************/
	Local cA6__NOSSO:= StrZero(Val(AllTrim(SEE->EE_FAXATU)), 10)
	//Local cA6__NOSSO:= Right(AllTrim(SEE->EE_FAXATU),8)
	Local cTexto:= ""

	//cTexto:= cAgeEmp + cCtaEmp + cCartEmp + cA6__NOSSO
	//cRet  := cCartEmp + "/" + cA6__NOSSO + "-" + Modu10(cTexto)

	cTexto   := ALLTRIM(transform(cA6__NOSSO, '@R 9999999999' ))
	cNossoNum:= cTexto + "-" + Ret_DV_ABC(cAgeEmp + cCartEmp + cTexto)
	cRet      := cNossoNum
	/* Atualizar o nosso numero no cadastro do banco */
	DbSelectArea("SEE")
	RecLock("SEE", .f.)
	Replace EE_FAXATU	With Soma1(cTexto)
	MsUnlock()

Return cRet

/******************************************************************************************************************/
Static Function Ret_DV_ABC(cLinha)
/******************************************************************************************************************/
	Local nSoma:= 0
	Local nResto
	Local nCont
	Local cDigRet
	Local nResult
	Local lDobra:= .f.
	Local cValor
	Local nAux

	For nCont:= Len(cLinha) To 1 Step -1
		lDobra:= !lDobra

		If lDobra
			cValor:= AllTrim(Str(Val(Substr(cLinha, nCont, 1)) * 2))
		Else
			cValor:= AllTrim(Str(Val(Substr(cLinha, nCont, 1))))
		EndIf

		For nAux:= 1 To Len(cValor)
			nSoma += Val(Substr(cValor, nAux, 1))
		Next n
	Next nCont

	nResto:= MOD(nSoma, 10)

	nResult:= 10 - nResto

	If nResult == 10
		cDigRet:= "0"
	Else
		cDigRet:= StrZero(10 - nResto, 1)
	EndIf

Return cDigRet

/******************************************************************************************************************/
//CdBarra_ABC()
/******************************************************************************************************************/
Static Function CdBarra_ABC()
	Local cDigCdBarra
	Local cFatVencto:= ""
	Local cValor
	Local nValor
	Local cCampo1:= ""
	Local cCampo2:= ""
	Local cCampo3:= ""
	Local cCampo4:= ""
	Local cCampo5:= ""

	cFatVencto:= StrZero(FatVencto(), 4)
	nValor:= Round(SE1->E1_SALDO, 2)
	cValor:= StrZero(nValor * 100, 10)

	cCpoLivre := Substr(cAgeEmp, 1, 4) + SubStr(cCartEmp, 1, 3)  + SubStr(cCtaEmp, 1, 7) + Substr(cNossoNum, 1, 10) + Substr(cNossoNum, 12, 1) 
	/* Calculo do codigo de barras */
	cCdBarra:= SA6->A6_COD + "9" + cFatVencto + cValor +  Substr(cAgeEmp, 1, 4) + SubStr(cCartEmp, 1, 3) + SubStr(cCtaEmp, 1, 7) + Substr(cNossoNum, 1, 10) + Substr(cNossoNum, 12, 1) 
	//cAgeEmp + cCtaEmp + cDigEmp + "000"
	cDigCdBarra:= Modu11(cCdBarra)
	cCdBarra:= SA6->A6_COD + "9" + cDigCdBarra + StrZero(FatVencto(), 4) + StrZero(Int(SE1->E1_SALDO * 100), 10) + cCpoLivre
	//Substr(cAgeEmp, 1, 4) + SubStr(cCartEmp, 1, 3) + SubStr(cCtaEmp, 1, 7) + Substr(cNossoNum, 1, 10) + Substr(cNossoNum, 12, 1) 


	//Alert (len(cCdBarra))

	/* Calculo da representacao numerica */
	cCampo1:= "246" + "9" + Substr(cAgeEmp, 1, 4) + SubStr(cCartEmp, 1, 1) //+ Substr(cNossoNum, 5, 2)
	cCampo2:= SubStr(cCartEmp,2,2) + SubStr(cCtaEmp, 1, 7) + Substr(cNossoNum, 1, 1) //+ Substr(cNossoNum, 11, 1) //+ Substr(cAgeEmp, 1, 3)
	cCampo3:= Substr(cNossoNum, 2, 9) + Substr(cNossoNum, 12, 1)  //Substr(cAgeEmp, 4, 1) + cCtaEmp + cDigEmp + "000"
	cCampo4:= Substr(cCdBarra, 5, 1) //ver 
	cCampo5:= cFatVencto + cValor

	/* Calculando os DACs dos campos 1, 2 e 3 */
	cCampo1:= cCampo1 + Modu10(cCampo1)
	cCampo2:= cCampo2 + Modu10(cCampo2)
	cCampo3:= cCampo3 + Modu10(cCampo3)

	cRepNum := Substr(cCampo1, 1, 5) + "." + Substr(cCampo1, 6, 5) + "  "
	cRepNum += Substr(cCampo2, 1, 5) + "." + Substr(cCampo2, 6, 6) + "  "
	cRepNum += Substr(cCampo3, 1, 5) + "." + Substr(cCampo3, 6, 6) + "  "
	cRepNum += cCampo4 + "  "
	cRepNum += cCampo5

	RecLock("SE1",.F.)                                                          // Customizado para guardar o código de barras e sua linha digitável, #Rafael Achôa - 15/05/2014
	Replace E1_CODBAR With cCdBarra
	Replace E1_CODDIG With cRepNum
	MsUnlock()

Return
/****************************************************************************/

/*/
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³          ³ Autor ³ PETERSON              ³ Data ³ 27.06.03 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Calcula o Digito Verificador MOD10                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ DIGITO DO CODIGO DE BARRAS   alterado por Silvio 04/01/05  ³±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
STATIC FUNCTION CalcDigM11() //Mod11_237( cNumero ) //-> Modulo 11 especifico p/ Banco Brandesco´para digito do codigo de barras
	LOCAL nCnt   := 0,;
	cDigito  := 0,;
	nSoma  := 0,;
	nBase  := 0,;
	aPeso  := {9,8,7,6,5,4,3,2};

	nBase := Len(aPeso)+1

	FOR nCnt := Len(cNumDig10) TO 1 STEP -1
		nBase := IF(--nBase = 0,Len(aPeso),nBase)
		nSoma += Val(SUBS(cNumDig10,nCnt,01)) * aPeso[ nBase ]
	NEXT

	cDigito := 11 - (nSoma % 11)

	DO CASE
		CASE cDigito = 0
		cDigito := "1"
		CASE cDigito > 9
		cDigito := "1"
		OTHERWISE
		cDigito := STR( cDigito, 1, 0 )
	ENDCASE
RETURN(cDigito)

/******************************************************************************************************************/
Static Function ValidPerg()
	/******************************************************************************************************************/
	Local _sAlias,i,j
	_sAlias := Alias()
	dbSelectArea("SX1")
	dbSetOrder(1)
	cPerg:= cPerg + space( 10 - len( cPerg ) )

	aRegs:={}

	//Grupo/Ordem/Pergunta/espanhol/ingles/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/DefSpa/DefEng/Cnt01/Var02/Def02/DefEsp/DefEng/Cnt02/Var03/Def03/DefEst/DefEng/Cnt03/Var04/Def04/Def4Esp/Def4Eng/Cnt04/Var05/Def05/Def5Esp/Def5Eng/Cnt05/Alias/grpsxg
	AADD(aRegs,{cPerg,"01","Banco              ?","","","mv_ch1","C",03,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","SA6","","","",""})
	AADD(aRegs,{cPerg,"02","Agencia            ?","","","mv_ch2","C",05,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	AADD(aRegs,{cPerg,"03","Conta              ?","","","mv_ch3","C",15,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	// AADD(aRegs,{cPerg,"04","Nota Fiscal de     ?","","","mv_ch4","C",09,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	// AADD(aRegs,{cPerg,"05","Nota Fiscal ate    ?","","","mv_ch5","C",09,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	// AADD(aRegs,{cPerg,"06","Serie              ?","","","mv_ch6","C",03,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	// AADD(aRegs,{cPerg,"07","Layout Impressao   ?","","","mv_ch7","N",01,0,0,"C","","mv_par07","Lexmark (Laser)","","","","","HP (Jato)","","","","","","","","","","","","","","","","","","","","","",""})
	// AADD(aRegs,{cPerg,"08","Da Carga           ?","","","mv_ch8","C",TamSx3("F2_CARGA")[1],0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	// AADD(aRegs,{cPerg,"09","Até a Carga        ?","","","mv_ch9","C",TamSx3("F2_CARGA")[1],0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	// AADD(aRegs,{cPerg,"10","Tipo               ?","","","mv_cha","N",01,0,0,"C","","mv_par10","Nota Fiscal","","","","","Boleto Avulso","","","","","","","","","","","","","","","","","","","","","",""})
	//AADD(aRegs,{cPerg,"08","Impressao por      ?","","","mv_ch8","N",01,0,0,"C","","mv_par08","Nota Fiscal","","","","","Avulso","","","","","","","","","","","","","","","","","","","","","",""})
	//AADD(aRegs,{cPerg,"09","Tipos Nao Permitidos","","","mv_ch9","C",20,0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	//aAdd(aRegs,{cPerg,"10","Texto 1 da instrucao","","","mv_ch10","C",50,0,0,"G","","mv_par10","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	//aAdd(aRegs,{cPerg,"11","                    ","","","mv_ch11","C",50,0,0,"G","","mv_par11","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	//aAdd(aRegs,{cPerg,"12","Texto 2 da instrucao","","","mv_ch12","C",50,0,0,"G","","mv_par12","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	//aAdd(aRegs,{cPerg,"13","                    ","","","mv_ch13","C",50,0,0,"G","","mv_par13","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	//aAdd(aRegs,{cPerg,"14","Texto 3 da instrucao","","","mv_ch14","C",50,0,0,"G","","mv_par14","","","","","","","","","","","","","","","","","","","","","","","","","",""})
	//aAdd(aRegs,{cPerg,"15","                    ","","","mv_ch15","C",50,0,0,"G","","mv_par15","","","","","","","","","","","","","","","","","","","","","","","","","",""})

	For i:=1 to Len(aRegs)
		If !dbSeek(cPerg+aRegs[i,2])
			RecLock("SX1",.T.)

			For j:=1 to Len(aRegs[i])
				FieldPut(j,aRegs[i,j])
			Next

			MsUnlock()
			dbCommit()
		EndIf
	Next
	dbSelectArea(_sAlias)
Return

//=======================================
// Função para envio de e-mail com anexo
//=======================================
User Function EnviarAne(cDe, cPara, cAssunto, cBody, cAtach, lLog)
	Local cAtach   := IIf(cAtach == Nil, "", cAtach)
	Local lLog     := IIf(lLog == Nil, .F., lLog)
	Local cServer  := AllTrim(getmv("MV_RELSERV"))
	Local cMail    := AllTrim(getmv("MV_RELAUSR"))
	Local cPass    := AllTrim(getmv("MV_RELAPSW"))
	Local lRelauth := getmv("MV_RELAUTH")
	Local lOk      := .F.
	Local cErro

	If AllTrim(cDe) == ""
		cDe := AllTrim(getmv("MV_RELAUSR"))
	EndIf

	cDe := AllTrim(cDe)

	CONNECT SMTP SERVER cServer ACCOUNT cMail PASSWORD cPass RESULT lOk

	If !lOK
		cErro := MailGetErr()
		If !lLog
			Alert("Erro ao Conectar o Servidor - "+cErro)
		Endif
		Return .F.
	Endif

	If lRelAuth
		lOk := MailAuth(cMail, cPass)
		If !lOK
			cErro := MailGetErr()
			If !lLog
				Alert("Erro ao conectar com autenticacao - " + cErro)
			EndIf
			lOk := MailSmtpOff()
			Return .F.
		Endif
	Endif

	SEND MAIL FROM cDe TO cPara SUBJECT cAssunto BODY cBody ATTACHMENT cAtach RESULT lOK

	If !lOK
		cErro := MailGetErr()
		If !lLog
			Alert("Erro ao Enviar Mensagem - " + cErro)
		EndIf
		lOk := MailSmtpOff()
		Return .F.
	EndIf

	lOk := MailSmtpOff()

	If !lOK
		If !lLog
			Alert("Erro ao Desconectar do Servidor")
		EndIf
		Return .F.
	EndIf

Return .T.

//ROTINA DE IMPRESSAO DAS INSTRUÇÕES NOS BOLETOS
//PARAMETROS: TAMANHO DA FONTE, LINHA INCIAL PARA IMPRESSÃO
User Function ImpIT(oFont10,nLinha)
	If !Empty(strIT01)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT01,oFont10)
	EndIf
	If !Empty(strIT02)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT02,oFont10)
	EndIf
	If !Empty(strIT03)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT03,oFont10)
	EndIf
	If !Empty(strIT04)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT04,oFont10)
	EndIf
	If !Empty(strIT05)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT05,oFont10)
	EndIf
	If !Empty(strIT06)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT06,oFont10)
	EndIf
	If !Empty(strIT07)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT07,oFont10)
	EndIf
	If !Empty(strIT08)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT08,oFont10)
	EndIf
	If !Empty(strIT09)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT09,oFont10)
	EndIf
	If !Empty(strIT10)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT10,oFont10)
	EndIf
	If !Empty(strIT11)
		nLinha:=nLinha+40
		oPrint:Say(nLinha,100 ,strIT11,oFont10)
	EndIf
Return()

Static Function AtuVlr()

	If Marked("OK")
		nVlrSel += Iif(mv_par10==1,BOLETOS->F2_VALFAT,BOLETOS->E1_SALDO)
	Else
		nVlrSel -= Iif(mv_par10==1,BOLETOS->F2_VALFAT,BOLETOS->E1_SALDO)
	endif

	oSay1:SetText("Total Selecionado: "+Alltrim(Transform(nVlrSel,"@E 9,999,999.99")))
	oSay1:CtrlRefresh()
return

/*Static Function AtuBanco(cCliente,cLoja)

Local lContinua := .T.

c_Banco := ''
c_Conta := ''
c_Agencia := ''

dbSelectArea("SA1")
SA1->(dbSetOrder(1))
If SA1->(dbSeek(xFilial("SA1")+cCliente+cLoja))
c_Banco := SA1->A1_BCO1
Endif

If Empty(c_Banco)
c_Banco := SuperGetMv( "MV_X_BANCO" , .F. , "001" ,  )
Endif

If !c_Banco $ "237,341,033,399,001,748,104"		// Customizado para aceitar Boletos da Caixa
//MsgBox(OemToAnsi("PROCESSO CANCELADO: Não existe layout de boleto preparado para o banco parametrizado!"), "Mensagem", "ALERT")
aAdd(aLog,"Não existe layout de boleto preparado para o banco parametrizado!")
lContinua := .F.
Return lContinua
EndIf

// Posicionar o banco
DbSelectArea("SA6")
DbSetOrder(1)
If !DbSeek(xFilial() + c_Banco) //+ mv_par02 + mv_par03)
//MsgBox(OemToAnsi("PROCESSO CANCELADO: Banco + Agencia + Conta não cadastrado no sistema!"), "Mensagem", "ALERT")
c_Agencia := SA6->A6_AGENCIA
c_Conta := SA6->A6_NUMCON
aAdd(aLog,"Banco não cadastrado no sistema!")
lContinua := .F.
Return lContinua
EndIf

// Posiciona no nosso numero (Parametros dos bancos)
DbSelectArea("SEE")
DbSetOrder(1)
if !DbSeek(xFilial("SEE")+c_Banco + c_Agencia + c_Conta)
//MsgBox(OemToAnsi("PROCESSO CANCELADO: Preencha primeiramente os parametros do banco selecionado!"), "Mensagem", "ALERT")
aAdd(aLog,"Preencha primeiramente os parametros do banco selecionado!")
lContinua := .F.
Return lContinua
EndIf

If lContinua

Do Case
Case SA6->A6_COD == "237" // Bradesco
cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 8), 8, '0')
cDigEmp:= alltrim(SA6->A6_DVCTA)		// Customizado para trazer dígito direto da SA6, #Rafael Achôa 21-08-2014
//	cCtaEmp:= Left(cCtaEmp, 7)
cCartEmp:= AllTrim(SEE->EE_CODCART)
nDiasProtesto := Val(Alltrim(SEE->EE_DIASPRT)) //Customizado para puxar protesto do campo da SEE #Rafael Achôa 24-07-2014

Case SA6->A6_COD == "341" // Itaú
cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
//MsgInfo(cAgeEmp,"cAgeEmp")
cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 6), 6, '0')
//MsgInfo(cCtaEmp,"cCtaEmp Antes")
//cDigEmp:= Right(cCtaEmp, 1)
cDigEmp  := AllTrim(SA6->A6_DVCTA)
//MsgInfo(cDigEmp,"cDigEmp")

cCtaEmp:= SubStr(AllTrim(cCtaEmp),2, 5)
//MsgInfo(cCtaEmp,"cCtaEmp Depois")
cCartEmp:= AllTrim(SEE->EE_CODCART) // '109'
//MsgInfo(cCartEmp,"cCartEmp")

Case SA6->A6_COD == "033" // Santander
cAgeEmp:= Padl(Right(alltrim(StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
cDigAge:= AllTrim(SA6->A6_DVAGE)

cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 8), 8, '0')
cDigEmp:= AllTrim(SA6->A6_DVCTA)

cCartEmp:= AllTrim(SEE->EE_CODCART)

Case SA6->A6_COD == "399" // HSBC
cAgeEmp:= Padl(Right(alltrim(StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
cCtaEmp:= Padl(Right(alltrim(StrTran(SA6->A6_NUMCON, '-', '')), 7), 7, '0')
cDigEmp:= ""
cCartEmp:= AllTrim(SEE->EE_CODCART)

Case SA6->A6_COD == "001" // Banco do Brasil
cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 7), 7, '0')
//cDigEmp:= Right(cCtaEmp, 1)
cDigEmp:= AllTrim(SEE->EE_DVCTA)
cDigAge:= AllTrim(SEE->EE_DVAGE)
//cCtaEmp:= Left(cCtaEmp, 6)
cCartEmp:= AllTrim(SEE->EE_CODCART)

cVariaCart:= Alltrim(SEE->EE_X_VARIA)    //Customizado para preencher corretamente o campo Carteira, onde o Banco do Brasil exige também a variação da Carteira, #Rafael Achôa 17-07-2014
nDiasProtesto := Val(Alltrim(SEE->EE_DIASPRT))

Case SA6->A6_COD == "748" // Sicred
cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
cCtaEmp:= Padl(alltrim( Substr(SA6->A6_NUMCON, 1, AT( '-', SA6->A6_NUMCON ) - 1)), 9, '0')
cDigEmp:= ''
cCartEmp:= ''     // AllTrim(SEE->EE_CODCART)

Case SA6->A6_COD == "104" // CEF
cAgeEmp:= Padl(Right(alltrim( StrTran(SA6->A6_AGENCIA, '-', '')), 4), 4, '0')
cCtaEmp:= Padl(Right(alltrim( StrTran(SA6->A6_NUMCON, '-', '')), 7), 7, '0')
cDigEmp:= Right(cCtaEmp, 1)
cCtaEmp:= Left(cCtaEmp, 6)
cCartEmp:= "12"

EndCase

Endif

Return lContinua        */





//------------------------------------
Static Function altInvG()
	local aAreaG:=BOLETOS->(GetArea())
	
	dbSelectArea("BOLETOS")
	dbGotop()
	While !eof()
		
		If Marked("OK")
				nVlrSel -= Iif(mv_par10==1,BOLETOS->F2_VALFAT,BOLETOS->E1_SALDO)
			Else
				nVlrSel += Iif(mv_par10==1,BOLETOS->F2_VALFAT,BOLETOS->E1_SALDO)
		endif

		
		RecLock("BOLETOS",.F.)
		BOLETOS->OK :=  iIf(BOLETOS->OK == cMark,' ',cMark)
		MsUnLock()
		dbSkip()
	End
	oSay1:SetText("Total Selecionado: "+Alltrim(Transform(nVlrSel,"@E 9,999,999.99")))
	oSay1:CtrlRefresh()
	restarea(aAreaG)
Return


user function retTotNFB(E1_NUM,E1_PREF,E1_CLI)
	Local vRet    := 0
	Local aArea   := getarea()
	Local cQryTot := ''

	cQryTot := " SELECT SUM(SD2.D2_TOTAL) + SUM(SD2.D2_VALIPI) + SUM(SD2.D2_ICMSRET) + SUM(SD2.D2_VALFRE) + SUM(SD2.D2_DESPESA) + SUM(SD2.D2_SEGURO) VALOR_NF "
	cQryTot += " FROM SD2010 SD2 
	cQryTot += " WHERE SD2.D2_DOC = '" + E1_NUM + "'     AND SD2.D2_SERIE = '" + E1_PREF + "'  AND SD2.D2_CLIENTE = '" + E1_CLI + "'  AND SD2.D_E_L_E_T_ = ''
	
	If Select("QRYSD2") > 0
		QRYSD2->(DbCloseArea())
	Endif
	TcQuery cQryTot New Alias "QRYSD2"

	dbselectarea("QRYSD2")
	dbgotop()
    vRet := QRYSD2->VALOR_NF
	QRYSD2->(DbCloseArea())

	restarea(aArea)
return vRet


user function retChVNFB(E1_NUM,E1_CLI)
	Local vRet    := ""
	Local aArea   := getarea()
	Local cQryTot := ''

	cQryTot := " SELECT SF2.F2_CHVNFE FROM SF2010 SF2 "
	cQryTot += " WHERE SF2.F2_DOC = '" + E1_NUM + "'  AND SF2.F2_CLIENTE = '" + E1_CLI + "'  AND SF2.D_E_L_E_T_ = '' 
	
	If Select("QRYSF2") > 0
		QRYSF2->(DbCloseArea())
	Endif
	TcQuery cQryTot New Alias "QRYSF2"

	dbselectarea("QRYSF2")
	dbgotop()
    vRet := QRYSF2->F2_CHVNFE
	QRYSF2->(DbCloseArea())

	restarea(aArea)
return vRet







