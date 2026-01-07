//Bibliotecas
#Include "Protheus.ch"
#Include "TopConn.ch"
#Include "RPTDef.ch"
#Include "FWPrintSetup.ch"
#Include "Ap5Mail.ch"
#Include "fileio.ch"
#Include "TbiConn.ch"
//Alinhamentos
#Define PAD_LEFT    0
#Define PAD_RIGHT   1
#Define PAD_CENTER  2

//Colunas
#Define POS_COD  0010 //COD. PROD
#Define POS_DESC 0050 //DESCRIÇÃO
#Define POS_UNID 0240 //UN
#Define POS_NCM  0260 //NCM
#Define POS_QUAN 0300 //QUANT.
#Define POS_VUNI 0330 //V.UNITARIO
#Define POS_VTOT 0360 //V.TOTAL
//#Define POS_BICM 0400 //BC.ICMS 
#Define POS_VICM 0430 //V.ICMS
#Define POS_VIPI 0460 //V.IPI
//#Define POS_AICM 0490 //A.ICMS 
#Define POS_AIPI 0460 //A.IPI
#Define POS_ICMSST 0500 //V.ICMST

//Variáveis estáticas
Static nTamFundo  := 15
Static cEmpEmail  := Alltrim(SuperGetMV('MV_X_EMAIL', .F., ""))
Static cEmpSite   := Alltrim(SuperGetMV('MV_X_HPAGE', .F., ""))
Static nCorAzul   := RGB(062,179,206)

/*===========================================================
Impressão gráfica genérica de Pedido de Venda (em pdf)
=============================================================*/

User Function LATFAR02()
	Local aArea      := GetArea()
	Local cPerg      := PadR("X_LATFAR02", 10)
	Private lConfirma := .f.
	Private cLogoEmp := fLogoEmp()
	Private cPedDe   := ""
	Private cPedAt   := ""
	Private cEmails  := "" 
	Private lAuto    := !Empty(cEmails)
	
	//Cria o grupo de perguntas

	Private aPergs := {;
		{1,"Pedido De",CRIAVAR("C5_NUM", .F.),"@!",'.T.',"SC5",'.T.',80,.F.},;
		{1,"Pedido Ate",CRIAVAR("C5_NUM", .F.),"@!",'.T.',"SC5",'.T.',80,.F.},;
		{1,"Data De",CRIAVAR("C5_EMISSAO", .F.),"@D",'.T.',"",'.T.',80,.F.},;
		{1,"Data Ate",CRIAVAR("C5_EMISSAO", .F.),"@D",'.T.',"",'.T.',80,.F.}}
		//{2,"Enviar Email","Não",{"Sim", "Não" },60,"",.F.};

		cPedDe := SC5->C5_NUM
		cPedAt := SC5->C5_NUM
		dDatDe := SC5->C5_EMISSAO
		dDatAte := SC5->C5_EMISSAO

		if isincallstack("MATA410")
			cPedDe := SC5->C5_NUM
			cPedAt := SC5->C5_NUM
			Processa({|| fMontaRel(cPedDe,cPedAt,dDatDe,dDatAte)}, "Processando...", "Aguarde gerando relatório",.F.)
		elseIf Parambox(aPergs, "Parametros para impressão Pedido Venda")
			cPedDe := MV_PAR01
			cPedAt := MV_PAR02
			dDatDe := MV_PAR03
			dDatAte := MV_PAR04
			Processa({|| fMontaRel(MV_PAR01,MV_PAR02,MV_PAR03,MV_PAR04)}, "Processando...", "Aguarde gerando relatório",.F.)
		endif

	RestArea(aArea)	
Return

/*---------------------------------------------------------------------*
 | Func:  fMontaRel                                                    |
 | Desc:  Função principal que monta o relatório                       |
 *---------------------------------------------------------------------*/

Static Function fMontaRel(xPedDe,xPedAt,xdDatDe,xdDatAte)

	Local cQryPed   := ""
	Local cQryIte   := ""
	Local nTotPed   := 0
	Local nPedAtu   := 0
	Local nBasICM   := 0
	Local nValICM   := 0
	Local nValIPI   := 0
	Local nAlqICM   := 0
	Local nAlqIPI   := 0
	Local nValorTot	:= 0
	Local nValorPrd	:= 0
	Local nBasIST	:= 0 //Base do ICMS ST
	Local nAlqIST 	:= 0 //Aliquota do ICMS ST
	Local nValIST 	:= 0 //Valor do ICMS ST
	Local nValDes 	:= 0 //Descontos
	Local nPesBru	:= 0
	Local nPesLiq	:= 0
	Local cFretePed := ""
	Local cMensFis	:= ""
	Local aMensagem	:= {}
	Local nLinhas   := ''
	Local nX := 0
	Local nLenMsg	:= 0
	Local nXi       := 0
	Local aVenctos  := {}
	Local nCl	:= 0
	Local cPedDe  := xPedDe
	Local cPedAt  := xPedAt
	Local dDatDe  := xdDatDe
	Local dDatAte := xdDatAte
	Local l2Vias  := .f.
	Local cNum_m  := ""
	Private cHoraEx  := Time()
	Private cNomeRel := "PedVenda_NUM_"+SC5->C5_NUM//+STRTRAN(cHoraEx, ":", "")
	Private nPagAtu  := 1
	Private aObsPed  := {}
	//Linhas e colunas
	Private nLinAtu   := 0
	Private nLinFin   := 820
	Private nColIni   := 010
	Private nColFin   := 550
	Private nColMeio  := (nColFin-nColIni)/2
	Private oPrintPvt
	//Declarando as fontes
	Private cNomeFont 	:= "Arial"
	Private oFontDet  	:= TFont():New(cNomeFont, 9, -7,  .T., .F., 5, .T., 5, .T., .F.)
	Private oFontDet2	:= TFont():New(cNomeFont, 5, -7,  .T., .F., 5, .T., 5, .T., .F.)
	Private oFontDet3	:= TFont():New(cNomeFont, 6, -7,  .T., .F., 5, .T., 5, .T., .F.)
	Private oFontDetN	:= TFont():New(cNomeFont, 9, -7,  .T., .T., 5, .T., 5, .T., .F.)
	Private oFontRod	:= TFont():New(cNomeFont, 9, -6,  .T., .F., 5, .T., 5, .T., .F.)
	Private oFontTit	:= TFont():New(cNomeFont, 9, -13, .T., .T., 5, .T., 5, .T., .F.)
	Private oFontCab	:= TFont():New(cNomeFont, 9, -9,  .T., .F., 5, .T., 5, .T., .F.)
	Private oFontCabN	:= TFont():New(cNomeFont, 9, -9,  .T., .T., 5, .T., 5, .T., .F.)
	Private oFontObs  := TFont():New(cNomeFont, 10, -10,  .T., .F., 5, .T., 5, .T., .F.)
	
	DbSelectArea('SA1')
	SA1->(DbSetOrder(1)) //A1_FILIAL+A1_COD+A1_LOJA
	SA1->(DbGoTop())
	DbSelectArea('SB1')
	SB1->(DbSetOrder(1)) //B1_FILIAL+B1_COD
	SB1->(DbGoTop())
	/*
	IF xFilial("SC5") == '99'
		if MsgNoYes("Imprimir 2 vias em folha unica?","2 vias x 1")
			l2Vias  := .t.
		endIf
	Endif*/


	//Se for automático por e-Mail
	If lAuto
		FErase("C:\TOTVS\"+cNomeRel+".pdf")
		oPrintPvt := FWMsPrinter():New(cNomeRel, IMP_PDF, .F., /*cStartPath*/, .T., , , , , .F., , .F.)
	
	//Senão, gera normalmente
	Else
		oPrintPvt := FWMSPrinter():New(cNomeRel, IMP_PDF, .F., /*cStartPath*/, .T., , @oPrintPvt, , , , , .T.)
	EndIf
	
	//Setando os atributos necessários
	oPrintPvt:cPathPDF := "C:\TOTVS\"
	oPrintPvt:SetResolution(72)
	oPrintPvt:SetPortrait()
	oPrintPvt:SetPaperSize(DMPAPER_A4)
	oPrintPvt:SetMargin(60, 60, 60, 60)
	
	//Selecionando os pedidos
	cQryPed := "SELECT "
	cQryPed += "C5_FILIAL, "
	cQryPed += "C5_EMISSAO, "
	cQryPed += "C5_NUM, "
	cQryPed += "C5_NOTA, "
	cQryPed += "C5_TIPO, "
	cQryPed += "C5_TIPOCLI, "
	cQryPed += "C5_CLIENTE, "
	cQryPed += "C5_LOJACLI, "
	cQryPed += "NVL(A1_NOME, '') AS A1_NOME, "	
	cQryPed += "NVL(A1_NREDUZ, '') AS A1_NREDUZ, "
	cQryPed += "NVL(A1_PESSOA, '') AS A1_PESSOA, "
	cQryPed += "NVL(A1_CGC, '') AS A1_CGC, "
	cQryPed += "NVL(A1_INSCR, '') AS A1_INSCR, "
	cQryPed += "NVL(A1_PFISICA, '') AS A1_PFISICA, "
	cQryPed += "C5_CONDPAG, "
	cQryPed += "NVL(E4_DESCRI, '') AS E4_DESCRI, "
	cQryPed += "C5_TRANSP, "
	cQryPed += "NVL(SA4T.A4_NREDUZ, '') AS A4_NREDUZ, "
	cQryPed += "NVL(SA4T.A4_NOME, '') AS A4_NOME, "
	cQryPed += "NVL(SA4T.A4_END, '') AS A4_END, "
	cQryPed += "NVL(SA4T.A4_BAIRRO, '') AS A4_BAIRRO, "
	cQryPed += "NVL(SA4T.A4_MUN, '') AS A4_MUN, "
	cQryPed += "NVL(SA4T.A4_EST, '') AS A4_EST, "
	cQryPed += "NVL(SA4T.A4_CEP, '') AS A4_CEP, "
	cQryPed += "NVL(SA4R.A4_NREDUZ, '') AS NREDUZ_RED, "
	cQryPed += "NVL(SA4R.A4_NOME, '') AS NOME_REDE, "
	cQryPed += "NVL(SA4R.A4_END, '') AS END_REDES, "
	cQryPed += "NVL(SA4R.A4_BAIRRO, '') AS BAIRRO_REDES, "
	cQryPed += "NVL(SA4R.A4_MUN, '') AS MUN_REDESP, "
	cQryPed += "NVL(SA4R.A4_EST, '') AS EST_REDESP, "
	cQryPed += "NVL(SA4R.A4_CEP, '') AS CEP_REDESP, "
	cQryPed += "C5_REDESP, "
	cQryPed += "C5_VEND1, "
	cQryPed += "NVL(A3_NREDUZ, '') AS A3_NREDUZ, "
	cQryPed += "C5_TPFRETE, "
	cQryPed += "C5_FRETE, "
	cQryPed += "C5_PESOL, "
	cQryPed += "C5_PBRUTO, "
	cQryPed += "C5_VOLUME1, "
	cQryPed += "C5_ESPECI1, "
	cQryPed += "C5_MENPAD, "
	cQryPed += "C5_MENNOTA, "
	cQryPed += "C5_X_FORPG, "
	//cQryPed += "ISNULL(CONVERT(VARCHAR(1024),CONVERT(VARBINARY(1024),C5_X_MSG)),'') AS C5_X_MSG, "
	//cQryPed += "NVL(UTL_RAW.CAST_TO_VARCHAR2(C5_X_MSG), '') AS C5_X_MSG, "
	cQryPed += "A1_END, "	
	cQryPed += "A1_BAIRRO, "	
	cQryPed += "A1_EST, "	
	cQryPed += "A1_MUN, "	
	cQryPed += "A1_DDD, "	
	cQryPed += "A1_TEL, "	
	cQryPed += "A1_CONTATO, "			
	cQryPed += "A1_EMAIL, "			
	cQryPed += "A1_ENDENT, "
	cQryPed += "A1_COMPENT, "
	cQryPed += "A1_CEPE, "
	cQryPed += "A1_BAIRROE, "
	cQryPed += "A1_ESTE, "
	cQryPed += "A1_MUNE "
	cQryPed += "FROM "+RetSQLName('SC5')+" SC5 "
	cQryPed += "LEFT JOIN "+RetSQLName('SA1')+" SA1 ON ("
	cQryPed += "A1_FILIAL = '"+FWxFilial('SA1')+"' "
	cQryPed += "AND A1_COD = SC5.C5_CLIENTE "
	cQryPed += "AND A1_LOJA = SC5.C5_LOJACLI "
	cQryPed += "AND SA1.D_E_L_E_T_ = ' ' "
	cQryPed += ") "
	cQryPed += "LEFT JOIN "+RetSQLName('SE4')+" SE4 ON ( "
	cQryPed += "E4_FILIAL = '"+FWxFilial('SE4')+"' "
	cQryPed += "AND E4_CODIGO = SC5.C5_CONDPAG "
	cQryPed += "AND SE4.D_E_L_E_T_ = ' ' "
	cQryPed += ") "
	cQryPed += "LEFT JOIN "+RetSQLName('SA4')+" SA4T ON ( "
	cQryPed += "SA4T.A4_FILIAL = '"+FWxFilial('SA4')+"' "
	cQryPed += "AND SA4T.A4_COD = SC5.C5_TRANSP "
	cQryPed += "AND SA4T.D_E_L_E_T_ = ' ' "
	cQryPed += ") "
	cQryPed += "LEFT JOIN "+RetSQLName('SA4')+" SA4R ON ( "
	cQryPed += "SA4R.A4_FILIAL = '"+FWxFilial('SA4')+"' "
	cQryPed += "AND SA4R.A4_COD = SC5.C5_REDESP "
	cQryPed += "AND SA4R.D_E_L_E_T_ = ' ' "
	cQryPed += ") "
	cQryPed += "LEFT JOIN "+RetSQLName('SA3')+" SA3 ON ( "
	cQryPed += "A3_FILIAL = '"+FWxFilial('SA3')+"' "
	cQryPed += "AND A3_COD = SC5.C5_VEND1 "
	cQryPed += "AND SA3.D_E_L_E_T_ = ' ' "
	cQryPed += ") "
	cQryPed += "WHERE "
	cQryPed += "C5_FILIAL = '"+FWxFilial('SC5')+"' "
	cQryPed += "AND C5_NUM >= '"+cPedDe+"' "
	cQryPed += "AND C5_NUM <= '"+cPedAt+"' "
	cQryPed += " AND C5_EMISSAO >= '"  + dToS(dDatDe) + "' "
	cQryPed += " AND C5_EMISSAO <= '"  + dToS(dDatAte) + "' "
	cQryPed += "AND SC5.D_E_L_E_T_ = ' ' "
	cQryPed += " ORDER BY C5_FILIAL, C5_NUM "
	cQryPed := ChangeQuery(cQryPed)
	TCQuery cQryPed New Alias "QRY_DAD"
	TCSetField("QRY_DAD", "C5_EMISSAO", "D")
	Count To nTotPed
	ProcRegua(nTotPed)
	QRY_DAD->(DbGoTop())
	
	//Somente se houver pedidos
	If nTotPed != 0
		//Enquanto houver pedidos
		cNum_m := QRY_DAD->C5_NUM
		While ! QRY_DAD->(EoF())
			dbSelectArea("SC5")
            		dbSetOrder(1)
			SC5->(dbSeek(xFilial("SC5") + QRY_DAD->C5_NUM	))
			
			nPagAtu := 1
			nPedAtu++
			IncProc("Processando o pedido "+cValToChar(nPedAtu)+" de "+cValToChar(nTotPed)+"...")
			
			//Imprime o cabeçalho
			fImpCab()
			cEmails := Rtrim(QRY_DAD->A1_EMAIL)
			/*if !Empty (cEmails)
				if cEnvmail == "Não"
					lAuto := .F.
				else
					lAuto := .T.
					FEnvCont()
					
				end if
			end if*/
			SA1->(DbSeek(FWxFilial('SA1') + QRY_DAD->C5_CLIENTE + QRY_DAD->C5_LOJACLI))
			
			//Seleciona agora os itens do pedido
			cQryIte := " SELECT "
			cQryIte += "    C6_PRODUTO, "
			cQryIte += "    NVL(C6_DESCRI, '') AS C6_DESCRI, " //C6_DESCRI
			cQryIte += "    C6_UM, "
			cQryIte += "    C6_TES, "
			cQryIte += "    B1_POSIPI, "
			cQryIte += "	C6_X_QTAUX, "
			cQryIte += "    C6_QTDVEN, "
			cQryIte += "    C6_PRCVEN, "
			cQryIte += "    C6_PRUNIT, " //Quando desconto for na SC5->C5_DESC1, então deverá calcular o desconto pelo preço da lista - valor unitário * qte
			//cQryIte += "    C6_BASEICM, "
			cQryIte += "    C6_VALDESC, "
			cQryIte += "    C6_VALOR, "
			cQryIte += "    C6_CF, "
			cQryIte += "    C6_CLASFIS, "
			cQryIte += "    C6_ENDPAD, "
			cQryIte += "    C6_LOTECTL, "
			cQryIte += "    B1_PESBRU, "
			cQryIte += "    B1_PESO "
			cQryIte += " FROM "
			cQryIte += "    "+RetSQLName('SC6')+" SC6 "
			cQryIte += "    LEFT JOIN "+RetSQLName('SB1')+" SB1 ON ( "
			cQryIte += "        B1_FILIAL = '"+FWxFilial('SB1')+"' "
			cQryIte += "        AND B1_COD = SC6.C6_PRODUTO "
			cQryIte += "        AND SB1.D_E_L_E_T_ = ' ' "
			cQryIte += "    ) "
			cQryIte += " WHERE "
			cQryIte += "    C6_FILIAL = '"+FWxFilial('SC6')+"' "
			cQryIte += "    AND C6_NUM = '"+QRY_DAD->C5_NUM+"' "
			cQryIte += "    AND SC6.D_E_L_E_T_ = ' ' "
			cQryIte += " ORDER BY C6_NUM, "
			cQryIte += "    C6_ITEM "
			cQryIte := ChangeQuery(cQryIte)

			TCQuery cQryIte New Alias "QRY_ITE"

			nValorTot 	:= 0
			nTValIPI	:= 0
			nTValICMST	:= 0
			nValorPrd	:= 0
			nTValDesc	:= 0

			//Enquanto houver itens
			While ! QRY_ITE->(EoF())
				//Pega os tratamentos de impostos
				SB1->(DbSeek(FWxFilial("SB1")+QRY_ITE->C6_PRODUTO))
				//MaFisIni(SA1->A1_COD, SA1->A1_LOJA, "C", "S", SA1->A1_TIPO,,, .F., "SB1")
				MaFisIni(   SA1->A1_COD,;		// 1-Codigo Cliente/Fornecedor
							SA1->A1_LOJA,;		// 2-Loja do Cliente/Fornecedor
            				"C",;				// 3-C:Cliente , F:Fornecedor
            				QRY_DAD->C5_TIPO,;	// 4-Tipo da NF
            				QRY_DAD->C5_TIPOCLI,;// 5-Tipo do Cliente/Fornecedor
				            Nil,;
							Nil,;
							Nil,;
							Nil,;
							"MATA461",;
							Nil,;
							Nil,;
							Nil,;
							Nil,;
							Nil,;
							Nil,;
							Nil,;
							QRY_DAD->C5_TRANSP)

				//MaFisAdd(SB1->B1_COD, QRY_ITE->C6_TES, QRY_ITE->C6_QTDVEN, QRY_ITE->C6_PRCVEN, QRY_ITE->C6_VALDESC, "", "",0, 0, 0, 0, 0, QRY_ITE->C6_VALOR               , 0, SB1->(RecNo()))
				//Agrega os itens para a funcao fiscal
				MaFisAdd(   SB1->B1_COD,;				// 1-Codigo do Produto ( Obrigatorio )
							QRY_ITE->C6_TES,;			// 2-Codigo do TES ( Opcional )
							QRY_ITE->C6_QTDVEN,;		// 3-Quantidade ( Obrigatorio )
							QRY_ITE->C6_PRCVEN,;		// 4-Preco Unitario ( Obrigatorio )
							QRY_ITE->C6_VALDESC,;		// 5-Valor do Desconto ( Opcional )
							"",;                    // 6-Numero da NF Original ( Devolucao/Benef )
							"",;                    // 7-Serie da NF Original ( Devolucao/Benef )
							0,;                     // 8-RecNo da NF Original no arq SD1/SD2
							0,;                     // 9-Valor do Frete do Item ( Opcional )
							0,;                     // 10-Valor da Despesa do item ( Opcional )
							0,;                     // 11-Valor do Seguro do item ( Opcional )
							0,;                     // 12-Valor do Frete Autonomo ( Opcional )
							QRY_ITE->C6_VALOR,;         // 13-Valor da Mercadoria ( Obrigatorio )
							0,;                     // 14-Valor da Embalagem ( Opiconal )   
							,;                      // 15
							,;                      // 16
							,;                      // 17 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
							0,;                     // 18-Despesas nao tributadas - Portugal
							0,;                     // 19-Tara - Portugal
							QRY_ITE->C6_CF,;              // 20-CFO
							,;                      // 21-Array para o calculo do IVA Ajustado (opcional)   
							,;                      // 22-Codigo Retencao - Equador
							0,;                     // 23-Valor Abatimento ISS
							,;                      // 24-Lote Produto
							,;                      // 25-Sub-Lote Produto
							,;
							,;
							QRY_ITE->C6_CLASFIS )              // 28-Classificação fiscal IT_CLASFIS

				nBasICM := MaFisRet(1,'IT_BASEICM')
				nValICM := MaFisRet(1,'IT_VALICM')
				nValIPI := MaFisRet(1,'IT_VALIPI')
				nAlqICM := MaFisRet(1,'IT_ALIQICM')
				nAlqIPI := MaFisRet(1,'IT_ALIQIPI')
				nBasIST	:= MaFisRet(1,"IT_BASESOL") //Base do ICMS ST
				nAlqIST := MaFisRet(1,"IT_ALIQSOL")	//Aliquota do ICMS ST
				nValIST := MaFisRet(1,"IT_VALSOL")	//Valor do ICMS ST
				nValDes := MaFisRet(1,"IT_DESCONTO") //Descontos
				MaFisEnd()

				//alert("nValIpi " + str(nValIpi))

				//alert("nAlqIPI " + str(nAlqIPI))

				//#PR
				//Se por acaso atingiu o limite da página, finaliza, e começa uma nova página
				If nLinAtu >= nLinFin
					fImpRod()
					fImpCab()
				EndIf
				oPrintPvt:SayAlign(nLinAtu, POS_COD,  QRY_ITE->C6_PRODUTO, oFontDet2, 040, 05, , PAD_LEFT,  )

				cDescPrd := Alltrim(QRY_ITE->C6_DESCRI)
                 
				 IF !Empty(QRY_ITE->C6_LOTECTL)
                        		cDescPrd += ' - Lote: ' + Alltrim(QRY_ITE->C6_LOTECTL)
				 Endif

				//oPrintPvt:SayAlign(nLinAtu, POS_DESC, substr(cDescPrd,1,50), oFontDet3, 200, 05, , PAD_LEFT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_DESC-5, substr(cDescPrd,1,50), oFontDet3, 200, 05, , PAD_LEFT,  )
				If len(cDescPrd) > 50
					nLinAtu += 9
					//oPrintPvt:SayAlign(nLinAtu, POS_DESC, substr(cDescPrd,51), oFontDet3, 200, 05, , PAD_LEFT,  )
					oPrintPvt:SayAlign(nLinAtu, POS_DESC-5, substr(cDescPrd,51), oFontDet3, 200, 05, , PAD_LEFT,  )
					nLinAtu -= 9
				Endif
				//oPrintPvt:SayAlign(nLinAtu, POS_UNID, QRY_ITE->C6_UM,  oFontDet, 030, 05, , PAD_LEFT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_UNID-20, QRY_ITE->C6_UM,  oFontDet, 030, 05, , PAD_LEFT,  )
				//oPrintPvt:SayAlign(nLinAtu, POS_NCM, QRY_ITE->B1_POSIPI,  oFontDet, 030, 05, , PAD_LEFT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_NCM-20, QRY_ITE->B1_POSIPI,  oFontDet, 030, 05, , PAD_LEFT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_QUAN-30, Alltrim(Transform(QRY_ITE->C6_X_QTAUX, "@E 999,999.99")),    oFontDet, 030, 05, , PAD_RIGHT, )
				oPrintPvt:SayAlign(nLinAtu, POS_QUAN, Alltrim(Transform(QRY_ITE->C6_QTDVEN, "@E 999,999.99")),    oFontDet, 030, 05, , PAD_RIGHT, )
				oPrintPvt:SayAlign(nLinAtu, POS_VUNI, Alltrim(Transform(QRY_ITE->C6_PRCVEN, "@E 999,999.99")),  oFontDet, 035, 05, , PAD_RIGHT, )
				oPrintPvt:SayAlign(nLinAtu, POS_VTOT, Alltrim(Transform(QRY_ITE->C6_VALOR, "@E 999,999.99")),  oFontDet, 060, 05, , PAD_RIGHT, )
				//oPrintPvt:SayAlign(nLinAtu, POS_BICM, Alltrim(Transform(nBasICM, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
				oPrintPvt:SayAlign(nLinAtu, POS_VICM, Alltrim(Transform(nValICM, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
				//oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nValIPI, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
				//oPrintPvt:SayAlign(nLinAtu, POS_AICM, Alltrim(Transform(nAlqICM, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
				oPrintPvt:SayAlign(nLinAtu, POS_AIPI, Alltrim(Transform(nAlqIPI, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
				oPrintPvt:SayAlign(nLinAtu, POS_ICMSST, Alltrim(Transform(nValIST, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
				If len(cDescPrd) > 50
					nLinAtu += 9
				Endif
				nLinAtu += 9
				
				nValorPrd 	+= QRY_ITE->C6_VALOR
				nTValIPI	+= nValIPI
				nTValICMST	+= nValIST //Val Icms
				nTValDesc	+= nValDes
				nValorTot 	+= QRY_ITE->C6_VALOR+nValIPI+nValIST-nValDes
				nPesBru		+= QRY_ITE->B1_PESBRU * QRY_ITE->C6_QTDVEN
				nPesLiq		+= QRY_ITE->B1_PESO * QRY_ITE->C6_QTDVEN 
				QRY_ITE->(DbSkip())
			EndDo
			QRY_ITE->(DbCloseArea())

			//Imprime o total do pedido
			nLinAtu += 2
			//Se por acaso atingiu o limite da página, finaliza, e começa uma nova página
			If nLinAtu >= nLinFin
				fImpRod()
				fImpCab(2)
			EndIf

			oPrintPvt:Line(nLinAtu,   nColIni, nLinAtu,   nColFin)
			nLinAtu += 5
			oPrintPvt:SayAlign(nLinAtu, POS_COD,  "VOLUMES: ", oFontDetN, 100, 05, , PAD_LEFT,  )
			oPrintPvt:SayAlign(nLinAtu, POS_DESC+10, Alltrim(Transform(QRY_DAD->C5_VOLUME1, PesqPict('SC5', 'C5_VOLUME1'))) + " - " + alltrim(QRY_DAD->C5_ESPECI1) ,  oFontDetN, 100, 05, , PAD_RIGHT, )
			oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "VALOR DOS PRODUTOS", oFontDetN, 100, 05, , PAD_RIGHT,  )
			oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nValorPrd, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
			nLinAtu += 10
			If nLinAtu >= nLinFin
				fImpRod()
				fImpCab(2)
			EndIf

			oPrintPvt:SayAlign(nLinAtu, POS_COD,    "PESO BRUTO: ", oFontCabN, 060, 05, , PAD_LEFT,  )
			If nPesBru == 0
				nPesBru := QRY_DAD->C5_PBRUTO
			Endif
			//oPrintPvt:SayAlign(nLinAtu, POS_DESC+10, Alltrim(Transform(nPesBru, PesqPict('SC5', 'C5_PBRUTO'))), oFontCab, 100, 05, , PAD_RIGHT,  )

			oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "VALOR IPI", oFontDetN, 100, 05, , PAD_RIGHT,  )
			oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nTValIPI, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
			nLinAtu += 10

			If nLinAtu >= nLinFin
				fImpRod()
				fImpCab(2)
			EndIf

			oPrintPvt:SayAlign(nLinAtu, POS_COD,    "PESO LIQUIDO: ", oFontCabN, 100, 05, , PAD_LEFT,  )
			If nPesLiq == 0
				nPesLiq := QRY_DAD->C5_PESOL
			Endif
			//oPrintPvt:SayAlign(nLinAtu, POS_DESC+10, Alltrim(Transform(nPesLiq, PesqPict('SC5', 'C5_PESOL'))), oFontCab, 100, 05, , PAD_RIGHT,  )

			oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "VALOR ICMS ST", oFontDetN,100, 05, , PAD_RIGHT,  )
			oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nTValICMST, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
			nLinAtu += 10
			If nLinAtu >= nLinFin
				fImpRod()
				fImpCab(2)
			EndIf

			oPrintPvt:SayAlign(nLinAtu, POS_COD, "TIPO FRETE: ", oFontCabN, 100, 05, , PAD_LEFT,  )
			If QRY_DAD->C5_TPFRETE == 'C'
				cFretePed := 'CIF'
			ElseIf QRY_DAD->C5_TPFRETE == 'F'
				cFretePed := 'FOB'
			ElseIf QRY_DAD->C5_TPFRETE == 'T'
				cFretePed := 'Por Conta Terceiros'
			ElseIf QRY_DAD->C5_TPFRETE == 'R'
				cFretePed := 'Por Conta Remetente'
			ElseIf QRY_DAD->C5_TPFRETE == 'D'
				cFretePed := 'Por Conta Destinatário'
			Else
				cFretePed := 'Frete Incluso'
			EndIf
			oPrintPvt:SayAlign(nLinAtu, POS_DESC+10, cFretePed, oFontCab, 100, 05, , PAD_RIGHT,  )

			oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "FRETE", oFontDetN, 100, 05, , PAD_RIGHT,  )
			oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(QRY_DAD->C5_FRETE, PesqPict('SC5', 'C5_FRETE'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
			nLinAtu += 10
			If nLinAtu >= nLinFin
				fImpRod()
				fImpCab(2)
			EndIf
			oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "DESCONTOS", oFontDetN, 100, 05, , PAD_RIGHT,  )
			oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nTValDesc, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
			nLinAtu += 10
			If nLinAtu >= nLinFin
				fImpRod()
				fImpCab(2)
			EndIf
			oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "VALOR TOTAL", oFontDetN, 100, 05, , PAD_RIGHT,  )
			nValorTot += QRY_DAD->C5_FRETE
			oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nValorTot, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
			nLinAtu += 20

			If nLinAtu >= nLinFin
				fImpRod()
				fImpCab(2)
			EndIf

			//Mensagem para nota			
			cMensFis := AllTrim(SC5->C5_MENNOTA)
			If !Empty(SC5->C5_MENPAD)
				cMensFis += " - " +AllTrim(FORMULA(SC5->C5_MENPAD))
			EndIf

			IF !Empty(cMensFis)
				oPrintPvt:SayAlign(nLinAtu, POS_COD, "MENSAGEM PARA NOTA", oFontCabN, 200, 05, , PAD_LEFT,  )
				nLinAtu += 15
				
				aMensagem := {}
				While !Empty(cMensFis)
 					aadd(aMensagem,SubStr(cMensFis,1,200))
   					cMensFis := SubStr(cMensFis,IIf(EspacoAt(cMensFis, 220) > 1, 199, 220) +2)
				EndDo

				nLenMsg:= Len(aMensagem)
				For nX := 1 To Min(nLenMsg, 220)
					oPrintPvt:SayAlign(nLinAtu, nColIni+5, aMensagem[nX], oFontCab, 500, 05, , PAD_LEFT,  )
					nLinAtu += 10

					If nLinAtu >= nLinFin
						fImpRod()
						fImpCab(2)
					EndIf
				Next
				nLinAtu += 05
				oPrintPvt:Line(nLinAtu,   nColIni, nLinAtu,   nColFin)
				nLinAtu += 10
				If nLinAtu >= nLinFin
					fImpRod()
					fImpCab(2)
				EndIf
			Endif
			// **Fim Mensagem para Nota

			aVenctos := Condicao(nValorTot, QRY_DAD->C5_CONDPAG, , QRY_DAD->C5_EMISSAO, )
			//Condição Pagamento
			oPrintPvt:SayAlign(nLinAtu, nColIni+5, "Cond.Pagto.:", oFontCabN, 060, 05, , PAD_LEFT,  )
			oPrintPvt:SayAlign(nLinAtu, nColIni+58, QRY_DAD->C5_CONDPAG +" - "+QRY_DAD->E4_DESCRI, oFontCab, 120, 05, , PAD_LEFT,  )
			oPrintPvt:SayAlign(nLinAtu, nColMeio, "Forma.Pagto.:", oFontCabN, 060, 05, , PAD_LEFT,  )
			oPrintPvt:SayAlign(nLinAtu, nColMeio+58, Alltrim(QRY_DAD->C5_X_FORPG), oFontCab, 120, 05, , PAD_LEFT,  )
			nLinAtu := nLinAtu + 10
			nCl := nColIni+5
			For nX := 1 to len(aVenctos)
				oPrintPvt:SayAlign(nLinAtu, nCl,dToC(aVenctos[nX][1]), oFontCab, 120, 05, , PAD_LEFT,  )
				oPrintPvt:SayAlign(nLinAtu+10, nCl,"R$ " + transform(aVenctos[nX][2],'@E 999,999.99') , oFontCab, 120, 05, , PAD_LEFT,  )
				nCl += 70
			Next
			nLinAtu += 30
			oPrintPvt:SayAlign(nLinAtu, nColIni+5, "Observação: ", oFontCabN, 060, 05, , PAD_LEFT,  )		
			//imprimir memo - bruno landi
			dbSelectArea("SC5")
            		dbSetOrder(1)
			if SC5->(dbSeek(xFilial("SC5") + QRY_DAD->C5_NUM	))
			    //nLinhas := MLCount(SC5->C5_X_MSG,140)
               		 	For nXi:= 1 To nLinhas
	           			  // cTxtLinha := MemoLine(SC5->C5_X_MSG,140,nXi)
	             			If !Empty(alltrim(cTxtLinha))
	              				oPrintPvt:SayAlign(nLinAtu, nColIni+58, cTxtLinha, oFontCab, 500, 05, , PAD_LEFT,  )
	             	       		 	//@ nLin, 119 PSAY cTxtLinha
	              			       	nLinAtu += 9
						If nLinAtu >= nLinFin
	               		      			fImpRod()
	               		      			fImpCab(2)
	             				EndIf
	                		EndIf
                		Next nXi
			endif

			nLinAtu += 20
			oPrintPvt:SayAlign(nLinAtu, nColMeio	, "_________________________________________", oFontCabN, 200, 05, , PAD_LEFT,  )
			nLinAtu += 20
			oPrintPvt:SayAlign(nLinAtu, nColMeio+5	, "       CONFIRMAÇÃO DE RECEBIMENTO         ",  oFontCab, 200, 05, , PAD_LEFT,  )

			If nLinAtu >= nLinFin
				fImpRod()
				fImpCab(2)
			EndIf
			

			///************************ impressao 2.a via na mesma pagina
			if xFilial("SC5") == '99'
				
				//Seleciona agora os itens do pedido
				cQryIte := " SELECT "
				cQryIte += "    C6_PRODUTO, "
				cQryIte += "    NVL(C6_DESCRI, '') AS C6_DESCRI, " //C6_DESCRI
				cQryIte += "    C6_UM, "
				cQryIte += "    C6_TES, "
				cQryIte += "    B1_POSIPI, "
				cQryIte += "	C6_X_QTAUX, "
				cQryIte += "    C6_QTDVEN, "
				cQryIte += "    C6_PRCVEN, "
				cQryIte += "    C6_PRUNIT, " //Quando desconto for na SC5->C5_DESC1, então deverá calcular o desconto pelo preço da lista - valor unitário * qte
				//cQryIte += "    C6_BASEICM, "
				cQryIte += "    C6_VALDESC, "
				cQryIte += "    C6_VALOR, "
				cQryIte += "    C6_CF, "
				cQryIte += "    C6_CLASFIS, "
				cQryIte += "    C6_ENDPAD, "
				cQryIte += "    C6_LOTECTL, "
				cQryIte += "    B1_PESBRU, "
				cQryIte += "    B1_PESO "
				cQryIte += " FROM "
				cQryIte += "    "+RetSQLName('SC6')+" SC6 "
				cQryIte += "    LEFT JOIN "+RetSQLName('SB1')+" SB1 ON ( "
				cQryIte += "        B1_FILIAL = '"+FWxFilial('SB1')+"' "
				cQryIte += "        AND B1_COD = SC6.C6_PRODUTO "
				cQryIte += "        AND SB1.D_E_L_E_T_ = ' ' "
				cQryIte += "    ) "
				cQryIte += " WHERE "
				cQryIte += "    C6_FILIAL = '"+FWxFilial('SC6')+"' "
				cQryIte += "    AND C6_NUM = '"+QRY_DAD->C5_NUM+"' "
				cQryIte += "    AND SC6.D_E_L_E_T_ = ' ' "
				cQryIte += " ORDER BY "
				cQryIte += "    C6_ITEM "
				cQryIte := ChangeQuery(cQryIte)

				TCQuery cQryIte New Alias "QRY_ITE"

				nValorTot 	:= 0
				nTValIPI	:= 0
				nTValICMST	:= 0
				nValorPrd	:= 0
				nTValDesc	:= 0
				
				lConfirma := .t.
				fImpCab(2)
				//Enquanto houver itens
				While ! QRY_ITE->(EoF())
					//Pega os tratamentos de impostos
					SB1->(DbSeek(FWxFilial("SB1")+QRY_ITE->C6_PRODUTO))
					//MaFisIni(SA1->A1_COD, SA1->A1_LOJA, "C", "S", SA1->A1_TIPO,,, .F., "SB1")
					MaFisIni(   SA1->A1_COD,;		// 1-Codigo Cliente/Fornecedor
								SA1->A1_LOJA,;		// 2-Loja do Cliente/Fornecedor
						"C",;				// 3-C:Cliente , F:Fornecedor
						QRY_DAD->C5_TIPO,;	// 4-Tipo da NF
						QRY_DAD->C5_TIPOCLI,;// 5-Tipo do Cliente/Fornecedor
						Nil,;
								Nil,;
								Nil,;
								Nil,;
								"MATA461",;
								Nil,;
								Nil,;
								Nil,;
								Nil,;
								Nil,;
								Nil,;
								Nil,;
								QRY_DAD->C5_TRANSP)

					//MaFisAdd(SB1->B1_COD, QRY_ITE->C6_TES, QRY_ITE->C6_QTDVEN, QRY_ITE->C6_PRCVEN, QRY_ITE->C6_VALDESC, "", "",0, 0, 0, 0, 0, QRY_ITE->C6_VALOR               , 0, SB1->(RecNo()))
					//Agrega os itens para a funcao fiscal
					MaFisAdd(   SB1->B1_COD,;				// 1-Codigo do Produto ( Obrigatorio )
								QRY_ITE->C6_TES,;			// 2-Codigo do TES ( Opcional )
								QRY_ITE->C6_QTDVEN,;		// 3-Quantidade ( Obrigatorio )
								QRY_ITE->C6_PRCVEN,;		// 4-Preco Unitario ( Obrigatorio )
								QRY_ITE->C6_VALDESC,;		// 5-Valor do Desconto ( Opcional )
								"",;                    // 6-Numero da NF Original ( Devolucao/Benef )
								"",;                    // 7-Serie da NF Original ( Devolucao/Benef )
								0,;                     // 8-RecNo da NF Original no arq SD1/SD2
								0,;                     // 9-Valor do Frete do Item ( Opcional )
								0,;                     // 10-Valor da Despesa do item ( Opcional )
								0,;                     // 11-Valor do Seguro do item ( Opcional )
								0,;                     // 12-Valor do Frete Autonomo ( Opcional )
								QRY_ITE->C6_VALOR,;         // 13-Valor da Mercadoria ( Obrigatorio )
								0,;                     // 14-Valor da Embalagem ( Opiconal )   
								,;                      // 15
								,;                      // 16
								,;                      // 17 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
								0,;                     // 18-Despesas nao tributadas - Portugal
								0,;                     // 19-Tara - Portugal
								QRY_ITE->C6_CF,;              // 20-CFO
								,;                      // 21-Array para o calculo do IVA Ajustado (opcional)   
								,;                      // 22-Codigo Retencao - Equador
								0,;                     // 23-Valor Abatimento ISS
								,;                      // 24-Lote Produto
								,;                      // 25-Sub-Lote Produto
								,;
								,;
								QRY_ITE->C6_CLASFIS )              // 28-Classificação fiscal IT_CLASFIS

					nBasICM := MaFisRet(1,'IT_BASEICM')
					nValICM := MaFisRet(1,'IT_VALICM')
					nValIPI := MaFisRet(1,'IT_VALIPI')
					nAlqICM := MaFisRet(1,'IT_ALIQICM')
					nAlqIPI := MaFisRet(1,'IT_ALIQIPI')
					nBasIST	:= MaFisRet(1,"IT_BASESOL") //Base do ICMS ST
					nAlqIST := MaFisRet(1,"IT_ALIQSOL")	//Aliquota do ICMS ST
					nValIST := MaFisRet(1,"IT_VALSOL")	//Valor do ICMS ST
					nValDes := MaFisRet(1,"IT_DESCONTO") //Descontos
					MaFisEnd()

					//alert("nValIpi " + str(nValIpi))

					//alert("nAlqIPI " + str(nAlqIPI))

					//#PR
					//Se por acaso atingiu o limite da página, finaliza, e começa uma nova página
					If nLinAtu >= nLinFin
						fImpRod()
						fImpCab()
					EndIf
					oPrintPvt:SayAlign(nLinAtu, POS_COD,  QRY_ITE->C6_PRODUTO, oFontDet2, 040, 05, , PAD_LEFT,  )

					cDescPrd := Alltrim(QRY_ITE->C6_DESCRI)
			
					IF !Empty(QRY_ITE->C6_LOTECTL)
						cDescPrd += ' - Lote: ' + Alltrim(QRY_ITE->C6_LOTECTL)
					Endif

					//oPrintPvt:SayAlign(nLinAtu, POS_DESC, substr(cDescPrd,1,50), oFontDet3, 200, 05, , PAD_LEFT,  )
					oPrintPvt:SayAlign(nLinAtu, POS_DESC-5, substr(cDescPrd,1,50), oFontDet3, 200, 05, , PAD_LEFT,  )
					If len(cDescPrd) > 50
						nLinAtu += 9
						//oPrintPvt:SayAlign(nLinAtu, POS_DESC, substr(cDescPrd,51), oFontDet3, 200, 05, , PAD_LEFT,  )
						oPrintPvt:SayAlign(nLinAtu, POS_DESC-5, substr(cDescPrd,51), oFontDet3, 200, 05, , PAD_LEFT,  )
						nLinAtu -= 9
					Endif
					//oPrintPvt:SayAlign(nLinAtu, POS_UNID, QRY_ITE->C6_UM,  oFontDet, 030, 05, , PAD_LEFT,  )
					oPrintPvt:SayAlign(nLinAtu, POS_UNID-20, QRY_ITE->C6_UM,  oFontDet, 030, 05, , PAD_LEFT,  )
					//oPrintPvt:SayAlign(nLinAtu, POS_NCM, QRY_ITE->B1_POSIPI,  oFontDet, 030, 05, , PAD_LEFT,  )
					oPrintPvt:SayAlign(nLinAtu, POS_NCM-20, QRY_ITE->B1_POSIPI,  oFontDet, 030, 05, , PAD_LEFT,  )
					oPrintPvt:SayAlign(nLinAtu, POS_QUAN-30, Alltrim(Transform(QRY_ITE->C6_X_QTAUX, "@E 999,999.99")),    oFontDet, 030, 05, , PAD_RIGHT, )
					oPrintPvt:SayAlign(nLinAtu, POS_QUAN, Alltrim(Transform(QRY_ITE->C6_QTDVEN, "@E 999,999.99")),    oFontDet, 030, 05, , PAD_RIGHT, )
					oPrintPvt:SayAlign(nLinAtu, POS_VUNI, Alltrim(Transform(QRY_ITE->C6_PRCVEN, "@E 999,999.99")),  oFontDet, 035, 05, , PAD_RIGHT, )
					oPrintPvt:SayAlign(nLinAtu, POS_VTOT, Alltrim(Transform(QRY_ITE->C6_VALOR, "@E 999,999.99")),  oFontDet, 060, 05, , PAD_RIGHT, )
					//oPrintPvt:SayAlign(nLinAtu, POS_BICM, Alltrim(Transform(nBasICM, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
					oPrintPvt:SayAlign(nLinAtu, POS_VICM, Alltrim(Transform(nValICM, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
					//oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nValIPI, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
					//oPrintPvt:SayAlign(nLinAtu, POS_AICM, Alltrim(Transform(nAlqICM, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
					oPrintPvt:SayAlign(nLinAtu, POS_AIPI, Alltrim(Transform(nAlqIPI, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
					oPrintPvt:SayAlign(nLinAtu, POS_ICMSST, Alltrim(Transform(nValIST, "@E 999,999.99")),   oFontDet, 030, 05, , PAD_RIGHT, )
					If len(cDescPrd) > 50
						nLinAtu += 9
					Endif
					nLinAtu += 9
					
					nValorPrd 	+= QRY_ITE->C6_VALOR
					nTValIPI	+= nValIPI
					nTValICMST	+= nValIST //Val Icms
					nTValDesc	+= nValDes
					nValorTot 	+= QRY_ITE->C6_VALOR+nValIPI+nValIST-nValDes
					nPesBru		+= QRY_ITE->B1_PESBRU * QRY_ITE->C6_QTDVEN
					nPesLiq		+= QRY_ITE->B1_PESO * QRY_ITE->C6_QTDVEN 
					QRY_ITE->(DbSkip())
				EndDo
				QRY_ITE->(DbCloseArea())

				//Imprime o total do pedido
				nLinAtu += 2
				//Se por acaso atingiu o limite da página, finaliza, e começa uma nova página
				If nLinAtu >= nLinFin
					fImpRod()
					fImpCab(2)
				EndIf

				oPrintPvt:Line(nLinAtu,   nColIni, nLinAtu,   nColFin)
				nLinAtu += 5
				oPrintPvt:SayAlign(nLinAtu, POS_COD,  "VOLUMES: ", oFontDetN, 100, 05, , PAD_LEFT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_DESC+10, Alltrim(Transform(QRY_DAD->C5_VOLUME1, PesqPict('SC5', 'C5_VOLUME1'))) + " - " + alltrim(QRY_DAD->C5_ESPECI1) ,  oFontDetN, 100, 05, , PAD_RIGHT, )
				oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "VALOR DOS PRODUTOS", oFontDetN, 100, 05, , PAD_RIGHT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nValorPrd, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
				nLinAtu += 10
				If nLinAtu >= nLinFin
					fImpRod()
					fImpCab(2)
				EndIf

				oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "VALOR IPI", oFontDetN, 100, 05, , PAD_RIGHT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nTValIPI, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
				nLinAtu += 10

				oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "VALOR ICMS ST", oFontDetN,100, 05, , PAD_RIGHT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nTValICMST, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
				nLinAtu += 10
				
				If nLinAtu >= nLinFin
					fImpRod()
					fImpCab(2)
				EndIf
				oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "DESCONTOS", oFontDetN, 100, 05, , PAD_RIGHT,  )
				oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nTValDesc, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
				nLinAtu += 10
				If nLinAtu >= nLinFin
					fImpRod()
					fImpCab(2)
				EndIf
				oPrintPvt:SayAlign(nLinAtu, POS_VTOT,  "VALOR TOTAL", oFontDetN, 100, 05, , PAD_RIGHT,  )
				nValorTot += QRY_DAD->C5_FRETE
				oPrintPvt:SayAlign(nLinAtu, POS_VIPI, Alltrim(Transform(nValorTot, PesqPict('SC6', 'C6_VALOR'))),  oFontDetN, 060, 05, , PAD_RIGHT, )
				nLinAtu += 20

				If nLinAtu >= nLinFin
					fImpRod()
					fImpCab(2)
				EndIf

				lConfirma := .f.

			endIf
/// fim impressao 2.a via na mesma pagina

			QRY_DAD->(DbSkip())

		EndDo
		
		//Se houver linhas na página ainda, imprime o rodapé
		fImpRod()
		QRY_DAD->(DbCloseArea())
		
		//Se for automático, gera o pdf e manda por email
		If lAuto
			oPrintPvt:Print()
			CpyT2S( "C:\TOTVS\"+cNomeRel+".pdf", "\spool", .F. )
			//fEnvEmail(cEmails, "Pedidos de Venda", "Segue relatório de pedido(s) em anexo", "\spool\"+cNomeRel+".pdf")
			zOutlook(cEmails)
		//Senão, mostra o relatório
		Else
			oPrintPvt:Preview()
			CpyT2S( "C:\TOTVS\"+cNomeRel+".pdf", "\spool", .F. )
		EndIf
	
	Else
		QRY_DAD->(DbCloseArea())
		MsgStop("Não há pedidos!", "Atenção")
	EndIf
Return

/*---------------------------------------------------------------------*
 | Func:  fImpCab                                                      |
 | Desc:  Função que imprime o cabeçalho                               |
 *---------------------------------------------------------------------*/

Static Function fImpCab(nTipo)
	//Local cTexto      := ""
	Local nLinCab     := 025
	Local nLinCabOrig := nLinCab
	Local cCodBar     := ""
	//Local nColMeiPed  := nColMeio+8+((nColMeio-nColIni)/2)
	Local lCNPJ       := (QRY_DAD->A1_PESSOA != 'F')
	Local cCGC        := ""
	Local cEmail1   := ''
	//Local cFretePed   := ""
	//Dados da empresa
	Local cEmpresa    := Iif(Empty(SM0->M0_NOMECOM), Alltrim(SM0->M0_NOME), Alltrim(SM0->M0_NOMECOM))
	Local cEmpTel     := Alltrim(Transform(SM0->M0_TEL,"@R (99) 9999-9999"))
	//Local cEmpFax     := Alltrim(Transform(SubStr(SM0->M0_FAX,3,Len(SM0->M0_FAX)),"@R (99) 9999-9999"))
	Local cEmpCidade  := AllTrim(SM0->M0_CIDENT)+" / "+SM0->M0_ESTENT
	Local cEmpCnpj    := Alltrim(Transform(SM0->M0_CGC,"@R 99.999.999/9999-99"))
	Local cEmpCep     := Alltrim(Transform(SM0->M0_CEPENT,"@R 99999-999"))
	Local nAltPg      := 0

	if len(alltrim(SM0->M0_CGC)) < 14

		cEmpresa    := 'IND. E COM. DE LATICINOS'
    		cEmpTel     := "(14) 3602-7100"//Alltrim(Transform(SubStr(SM0->M0_TEL,3,Len(SM0->M0_TEL)),"@R (99) 9999-9999"))
    		cEmpCidade  := 'LUTECIA/SP'

	endIf
	
	if !lConfirma
		//Iniciando Página
		oPrintPvt:StartPage()
	else
		nAltPg := oPrintPvt:nVertSize()
		nLinCab := (nAltPg / 2) + 80
		nLinCabOrig := nLinCab
		oPrintPvt:SayAlign(nLinCab-15, nColIni+30, "----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------", oFontRod, 1200, 05, , PAD_LEFT,  )
		//oPrintPvt:Line(nLinRod,   nColIni, nLinRod,   nColFin,"-2")
	endIf
	
	//Dados da Empresa
	oPrintPvt:Box(nLinCab, nColIni,    nLinCab + 075, nColMeio+27)
	//oPrintPvt:Line(nLinCab+nTamFundo,   nColIni, nLinCab+nTamFundo,   nColMeio-30)
	nLinCab += nTamFundo - 5
	//oPrintPvt:SayAlign(nLinCab-10, nColIni+5, "Empresa", oFontTit, 060, 05, nCorAzul, PAD_LEFT,  )
	//nLinCab += 5
	oPrintPvt:SayBitmap(nLinCab+10, nColIni+5, cLogoEmp, 054, 031)
	nLinCab -= 5
	oPrintPvt:SayAlign(nLinCab, nColIni+65,  "Empresa:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpresa, oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 9
	oPrintPvt:SayAlign(nLinCab, nColIni+65,  "CNPJ:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpCnpj, oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 9
	oPrintPvt:SayAlign(nLinCab, nColIni+65,  "Cidade:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpCidade, oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 9
	oPrintPvt:SayAlign(nLinCab, nColIni+65,  "CEP:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpCep, oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 9
	oPrintPvt:SayAlign(nLinCab, nColIni+65,  "Telefone:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpTel, oFontCab, 120, 05, , PAD_LEFT,  )
	//nLinCab += 9
	//oPrintPvt:SayAlign(nLinCab, nColIni+65,  "FAX:", oFontCabN, 060, 05, , PAD_LEFT,  )
	//oPrintPvt:SayAlign(nLinCab, nColIni+85,  cEmpFax, oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 9
	oPrintPvt:SayAlign(nLinCab, nColIni+65,  "E-mail:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+100,  cEmpEmail, oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 9
	oPrintPvt:SayAlign(nLinCab, nColIni+65,  "Home Page:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+110,  cEmpSite, oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 7
	
	//Dados do Pedido
	nLinCab := nLinCabOrig
	oPrintPvt:Box(nLinCab, nColMeio+30, nLinCab + 075, nColFin)
	oPrintPvt:Line(nLinCab+nTamFundo,   nColMeio+30, nLinCab+nTamFundo,   nColFin)
	nLinCab += nTamFundo - 5

	dbSelectArea("SC6")
	dbSetOrder(1)
	dbSeek(xFilial("SC6")+QRY_DAD->C5_NUM)
	if SC6->C6_TES$getMv("MV_X_TESBO")  // TES DE BONIFICACAO 
		oPrintPvt:SayAlign(nLinCab-8, nColMeio+100, "PEDIDO DE BONIFICACAO" , oFontTit, 200, 05, nCorAzul, PAD_LEFT,  )
		nLinCab += nTamFundo
	ElseIf SC6->C6_TES$getMv("MV_X_TESTR")  // TES DE TROCA
		oPrintPvt:SayAlign(nLinCab-8, nColMeio+100, "PEDIDO DE TROCA" , oFontTit, 200, 05, nCorAzul, PAD_LEFT,  )
		nLinCab += nTamFundo
	Else
		oPrintPvt:SayAlign(nLinCab-8, nColMeio+100, "PEDIDO DE VENDA" , oFontTit, 200, 05, nCorAzul, PAD_LEFT,  )
		nLinCab += nTamFundo
	endIf

	
	oPrintPvt:SayAlign(nLinCab, nColMeio+38, "Número: " + alltrim(QRY_DAD->C5_NUM) , oFontTit, 400, 05, nCorAzul, PAD_LEFT,  )
	nLinCab += nTamFundo
	oPrintPvt:SayAlign(nLinCab+10, nColMeio+38, "Emissão: " + dToC(QRY_DAD->C5_EMISSAO) , oFontTit, 400, 05, nCorAzul, PAD_LEFT,  )
	
	//Código de barras
	nLinCab := nLinCabOrig
	cCodBar := QRY_DAD->C5_NUM //QRY_DAD->C5_FILIAL+QRY_DAD->C5_NUM
	oPrintPvt:Code128C(nLinCab+38+nTamFundo, nColFin-80, cCodBar, 28)
	oPrintPvt:SayAlign(nLinCab+40+nTamFundo, nColFin-100, cCodBar, oFontRod, 080, 05, , PAD_CENTER,  )
	nLinCab += nTamFundo-2
	oPrintPvt:SayAlign(nLinCab+40+nTamFundo, nColFin-100, "Documento: " + alltrim(QRY_DAD->C5_NOTA) , oFontRod, 080, 05, , PAD_CENTER,  )
	
	//Dados do Cliente
	nLinCab := nLinCabOrig + 080	
	oPrintPvt:Box(nLinCab, nColIni, nLinCab+75, nColFin)
	nLinCab += 5
	oPrintPvt:SayAlign(nLinCab, nColIni+5,  "Cliente:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+48, QRY_DAD->C5_CLIENTE+"/"+QRY_DAD->C5_LOJACLI+" - "+alltrim(QRY_DAD->A1_NREDUZ), oFontCab, 350, 05, , PAD_LEFT,  )
	nLinCab += 10
	cCGC := QRY_DAD->A1_CGC
	If lCNPJ
		cCGC := Iif(!Empty(cCGC), Alltrim(Transform(cCGC, "@R 99.999.999/9999-99")), "-")
		oPrintPvt:SayAlign(nLinCab, nColIni+5, "CNPJ:", oFontCabN, 060, 05, , PAD_LEFT,  )
		cIE	:= alltrim(QRY_DAD->A1_INSCR)
		oPrintPvt:SayAlign(nLinCab, nColMeio, "IE:", oFontCabN, 060, 05, , PAD_LEFT,  )
	Else
		cCGC := Iif(!Empty(cCGC), Alltrim(Transform(cCGC, "@R 999.999.999-99")), "-")
		oPrintPvt:SayAlign(nLinCab, nColIni+5, "CPF:",  oFontCabN, 060, 05, , PAD_LEFT,  )
		oPrintPvt:SayAlign(nLinCab, nColMeio, "RG:", oFontCabN, 060, 05, , PAD_LEFT,  )
		cIE	:= alltrim(QRY_DAD->A1_PFISICA)
	EndIf
	oPrintPvt:SayAlign(nLinCab, nColIni+48	, cCGC,  oFontCab, 100, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColMeio+18	, cIE,  oFontCab, 060, 05, , PAD_LEFT,  )
	nLinCab += 10
	oPrintPvt:SayAlign(nLinCab, nColIni+5	, "Endereço:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+48	, alltrim(QRY_DAD->A1_END) + "    -    " + alltrim(QRY_DAD->A1_BAIRRO), oFontCab, 230, 05, , PAD_LEFT,  )
	nLinCab += 10
	oPrintPvt:SayAlign(nLinCab, nColIni+5	, "Cidade:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+48	, alltrim(QRY_DAD->A1_MUN) +" - "+ QRY_DAD->A1_EST, oFontCab, 150, 05, , PAD_LEFT,  )
	nLinCab += 10
	cEmail1   := alltrim(QRY_DAD->A1_EMAIL)
	oPrintPvt:SayAlign(nLinCab, nColIni+5	, "E-mail:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+48	, alltrim(cEmail1), oFontCab, 500, 05, , PAD_LEFT,  )
	nLinCab += 10
	oPrintPvt:SayAlign(nLinCab, nColIni+5	, "Fone:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+48	, "("+ alltrim(QRY_DAD->A1_DDD) +")-" +alltrim(QRY_DAD->A1_TEL), oFontCab, 120, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColMeio	, "Contato:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColMeio+48	, alltrim(QRY_DAD->A1_CONTATO), oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 10
	oPrintPvt:SayAlign(nLinCab, nColIni+5	, "Vendedor:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(nLinCab, nColIni+42	, QRY_DAD->C5_VEND1 + " "+alltrim(QRY_DAD->A3_NREDUZ), oFontCab, 120, 05, , PAD_LEFT,  )
	nLinCab += 7

	//Título
	//nLinCab := nLinCabOrig + 080
	nLinCab += 7
	oPrintPvt:Box(nLinCab, nColIni, nLinCab + nTamFundo, nColFin)
	nLinCab += nTamFundo - 5
	oPrintPvt:SayAlign(nLinCab-10, nColIni, "Detalhamento do Pedido", oFontTit, nColFin-nColIni, nTamFundo, nCorAzul, PAD_CENTER, )
	
	//Linha Separatório
	nLinCab += 8
	oPrintPvt:Line(nLinCab,   nColIni, nLinCab,   nColFin)
	
	//Cabeçalho com descrições das colunas
	nLinCab += 3

	IF nTipo < 2	
		//oPrintPvt:SayAlign(nLinCab, POS_COD,  "Cód.Prod.", oFontDetN, 040, 05, , PAD_LEFT,  )
		oPrintPvt:SayAlign(nLinCab, POS_COD,  "Cód.Prod.", oFontDetN, 040, 05, , PAD_LEFT,  )
		//oPrintPvt:SayAlign(nLinCab, POS_DESC, "Descrição", oFontDetN, 200, 05, , PAD_LEFT,  )
		oPrintPvt:SayAlign(nLinCab, POS_DESC-5, "Descrição", oFontDetN, 150, 05, , PAD_LEFT,  )
		//oPrintPvt:SayAlign(nLinCab, POS_UNID, "UN.",  oFontDetN, 030, 05, , PAD_LEFT,  )
		oPrintPvt:SayAlign(nLinCab, POS_UNID-20, "UN.",  oFontDetN, 030, 05, , PAD_LEFT,  )
		//oPrintPvt:SayAlign(nLinCab, POS_NCM, "NCM",  oFontDetN, 030, 05, , PAD_LEFT,  )
		oPrintPvt:SayAlign(nLinCab, POS_NCM-20, "NCM",  oFontDetN, 030, 05, , PAD_LEFT,  )
		//oPrintPvt:SayAlign(nLinCab, POS_QUAN+5, "Quant.",    oFontDetN, 030, 05, , PAD_RIGHT, )
		oPrintPvt:SayAlign(nLinCab, POS_QUAN-30, "Qtde.Aux",    oFontDetN, 030, 05, , PAD_RIGHT, )
		oPrintPvt:SayAlign(nLinCab, POS_QUAN, "Quant.",    oFontDetN, 030, 05, , PAD_RIGHT, )
		oPrintPvt:SayAlign(nLinCab, POS_VUNI+10, "Vl.Unit.",  oFontDetN, 030, 05, , PAD_RIGHT, )
		oPrintPvt:SayAlign(nLinCab, POS_VTOT, "Vl.Total",  oFontDetN, 060, 05, , PAD_RIGHT, )
		//oPrintPvt:SayAlign(nLinCab, POS_BICM, "BC.ICMS",   oFontDetN, 030, 05, , PAD_RIGHT, )
		oPrintPvt:SayAlign(nLinCab, POS_VICM, "Vl.ICMS",   oFontDetN, 030, 05, , PAD_RIGHT, )
		//oPrintPvt:SayAlign(nLinCab, POS_VIPI, "Vl.IPI",    oFontDetN, 030, 05, , PAD_RIGHT, )
		//oPrintPvt:SayAlign(nLinCab, POS_AICM, "A.ICMS",    oFontDetN, 030, 05, , PAD_RIGHT, )
		oPrintPvt:SayAlign(nLinCab, POS_AIPI, "A.IPI",     oFontDetN, 030, 05, , PAD_RIGHT, )
		oPrintPvt:SayAlign(nLinCab, POS_ICMSST-20, "Vl.ICMSST",     oFontDetN, 060, 05, , PAD_RIGHT, )

		//Atualizando a linha inicial do relatório
	Endif
	nLinAtu := nLinCab + 10
Return

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
	cTexto := "Pedido: "+QRY_DAD->C5_NUM+"    |    "+dToC(dDataBase)+"     "+cHoraEx+"     "+FunName()+"     "+cUserName
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
	//Local cCamFim		:= GetTempPath()
	//Local cStart		:= GetSrvProfString("Startpath","")
	
	    cDescLogo	:= cGrpCompany + cCodEmpGrp + cUnitGrp + cFilGrp

		cLogo	:= GetSrvProfString("Startpath","") + "danfe" + '0101' + ".bmp"

	//Copia para a temporária do s.o.
	//CpyS2T(cLogo, cCamFim)
	//cLogo := cCamFim + StrTran(cLogo, cStart, '')
	
	//Se o arquivo não existir na temporária, espera meio segundo para terminar a cópia
	//If !File(cLogo)
	//	Sleep(500)
	//EndIf
Return cLogo

Static Function EspacoAt(cString, nTam)

Local nRetorno := 0
Local nX       := 0

/**
* Caso a posição (nTam) for maior que o tamanho da string, ou for um valor
* inválido, retorna 0.
*/
If nTam > Len(cString) .Or. nTam < 1
	nRetorno := 0
Return nRetorno
EndIf

/**
* Procura pelo caractere de espaço anterior a posição e retorna a posição
* dele.
*/
nX := nTam
While nX > 1
	If Substr(cString, nX, 1) == " "
		nRetorno := nX
		Return nRetorno
	EndIf

	nX--
EndDo

/**
* Caso não encontre nenhum caractere de espaço, é retornado 0.
*/
nRetorno := 0

Return nRetorno

/*---------------------------------------------------------------------*
 | Func:  fImpObs                                                      |
 | Autor:                                                              |
 | Data:  25/11/2019                                                   |
 | Desc:  Função que imprime o onservação a partir de arquivo de texto |
 |        Localizado na pasta Protheus_Data/Complemento                |
 *---------------------------------------------------------------------*/

// ---
Static function fImpObs()
	Local x
	If Len(aObsPed) > 0
		For x := 1 To Len(aObsPed)
			oPrintPvt:SayAlign(nLinAtu, POS_COD, alltrim(aObsPed[x][1]), oFontObs, 550, 05, , PAD_LEFT,  ) 
			nLinAtu +=7       
		Next
	EndIf
Return


/*---------------------------------------------------------------------*
 | Func:  fEnvCont                                                     |
 | Autor:                                                              |
 | Data:  27/01/2020                                                   |
 | Desc:  Função que envia email para o cliente                        |
 *---------------------------------------------------------------------*/
Static function FEnvCont()	
	Local aRet		:= {}
	Local aParBox 	:= {}
	Local cQuery 	:= ""
	Local cTexto 	:= "Sub Categoria"
	Local i
	Local c
	Local e

	Public cRetSubC	 := ""  // retorno da pesqgrup
	Public cRetSubRC	 := ""  // retorno da pesqgrup

	cQuery := "	SELECT" 
	cQuery += "		A1_COD,"
	cQuery += "		A1_LOJA,"
	cQuery += "		AC8_CODCON,"
	cQuery += "		U5_CONTAT,"
	cQuery += "		U5_EMAIL"		
	cQuery += "	FROM"
	cQuery += "		"+RetSQLName('SA1')+" SA1"
	cQuery += "		LEFT JOIN" 
	cQuery += "		"+RetSQLName('AC8')+" AC8 ON ("
	cQuery += "			AC8_FILIAL = ''" 
	cQuery += "			AND AC8_CODENT = '"+QRY_DAD->C5_CLIENTE+"'+'"+QRY_DAD->C5_LOJACLI+"'"
	cQuery += "			AND AC8.D_E_L_E_T_ = ' ' )" 
	cQuery += "		LEFT JOIN" 
	cQuery += "		"+RetSQLName('SU5')+" SU5 ON ("
	cQuery += "			U5_FILIAL = ''"
	cQuery += "			AND  U5_CODCONT=AC8_CODCON"
	cQuery += "			AND SU5.D_E_L_E_T_ = '')"
	cQuery += "	WHERE" 
	cQuery += "    A1_FILIAL = '"+FWxFilial('SA1')+"' "
	cQuery  += "   AND A1_COD = '"+QRY_DAD->C5_CLIENTE+"' "
	cQuery  += "   AND A1_LOJA = '"+QRY_DAD->C5_LOJACLI+"' "
	cQuery += "	   AND SA1.D_E_L_E_T_= ' ' " 

	if select("QRY")>0
		QRY->(dbCloseArea())
	endif
	TcQuery cQuery new alias "QRY"

	While !QRY->(EOF())	
			
		If !Empty (QRY->U5_CONTAT) // retorna em tempo real o conteudo 		
			aadd(aParBox,{4,alltrim(QRY->U5_EMAIL)  , .T.	,alltrim(QRY->U5_CONTAT)  ,70	,""	,.F.	})
		Endif
		QRY->(DbSkip())
	end
	if len(aparbox)==0
		//MsgAlert("Não há contatos cadastrados para esse cliente!!!", "Não há contatos cadastrados")
		QRY->(dbCloseArea())
		return
	endif
	ParamBox(aParBox,cTexto)
	
	for c := 1 to len(aParBox)
		if &("MV_PAR"+strzero(c,2))  // .T. ou .F.
			cRetSubRC += aParBox[c][4]+";"
		endif
	next

	for e := 1 to len(aParBox)
		if &("MV_PAR"+strzero(e,2))  // .T. ou .F.
			cRetSubC += aParBox[e][2]+";"
		endif
	next
	
	oPrintPvt:SayAlign(82, nColMeio+8,  "Email:", oFontCabN, 060, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(82, nColMeio+48, cRetSubC, oFontCab, 160, 05, , PAD_LEFT,  )
	
	oPrintPvt:SayAlign(89, nColMeio+8,  "Contato:", oFontCabN, 065, 05, , PAD_LEFT,  )
	oPrintPvt:SayAlign(89, nColMeio+48, cRetSubRC, oFontCab, 160, 05, , PAD_LEFT,  )
	
	
	QRY->(DbCloseArea())
	cEmails+= ";"+cRetSubC

return .T. 

 
Static Function zOutlook(cEmails)
	Local  cTemp := GetTempPath()
    Local cExecute := '/c ipm.note /a C:\TOTVS\'+cNomeRel+'.pdf /m "'+Alltrim(cEmails)+'?Subject=Metalurgica Rivertec - Pedido nº: '+SC5->C5_NUM+'&Body='+SPACE(5000)+ Chr(13) + Chr(10) +'TROCA E/OU DEVOLUÇÃO' + Chr(13) + Chr(10) + '1. Somente serão aceitas troca e/ou devolução de produtos constantes no catálogo vigente da Rivertec. Portanto, produtos especiais (com especificações exclusivas do cliente) não serão aceitos;2. Os produtos somente poderão ser trocados e/ou devolvidos no prazo de trinta dias contados da emissão da respectiva Nota Fiscal;a. No momento da notificação de troca e/ou devolução o cliente deverá informar a(s) respectiva(s) Nota(s) Fiscal(is) de compra;3. Os produtos deverão estar preservados quanto a amassamento, oxidação etc., e não tenham sido utilizados;4. Todo produto enviado para a RIVERTEC deverá ser acompanhado de respectiva Nota Fiscal na modalidade de Remessa para troca ou Devolução de Compra conforme o caso;5. Todo produto recebido na RIVERTEC em troca e/ou devolução será obrigatoriamente inspecionado pela área de Qualidade a qual liberará ou não o seu recebimento;6. Os eventuais créditos decorrentes da troca e/ou devolução somente serão concedidos ao cliente após a liberação dos produtos pela área de Qualidade da RIVERTEC;a. Os eventuais créditos serão concedidos em produtos e/ou compensação de eventuais débitos pendentes. Em hipótese alguma será feita devolução em dinheiro;7. Caso o cliente solicite o envio dos produtos em substituição antes do recebimento daqueles em troca, será emitida respectiva Fatura no valor correspondente contra o cliente a qual será compensada com o eventual crédito dos produtos recebidos em troca e/ou devolução;8. Os custos relacionados ao transporte dos produtos, tanto no retorno do cliente para a RIVERTEC como no reenvio para o cliente, serão de responsabilidade da parte que provocou a ocorrência;Eventuais dúvidas ligar para 14-3602-7100 Dpto.Comercial ou e-mail: vendas@rivertec.com.br."'
        ShellExecute("OPEN", "outlook.exe", cExecute, "", 1)
Return

/*---------------------------------------------------------------------*
 | Func:  fEnvEmail                                                    |
 | Autor:                                                              |
 | Data:  13/01/2020                                                   |
 | Desc:  Função que envia email para o Fornecedo                      |
 *---------------------------------------------------------------------*/
/*
Static Function fEnvEmail(_cPara, _cTitulo, _cMsg, _aAnexo)
	Local oMail
	Local oMessage
	Local nRet
	Local nTimeout := GetMV("MV_RELTIME")	//Timeout no Envio de E-Mail;
	Local cServer  := GetMV("MV_RELSERV")	//Nome do Servidor de Envio de E-Mail utilizado nos relatorios;
	Local cEmail   := GetMV("MV_RELACNT")	//Conta a ser utilizada no envio de E-Mail para os relatorios;
	Local cEmailA  := GetMV("MV_RELAUSR")	//Usuario para Autenticacao no Servidor de E-Mail;
	Local cEmailFr := GetMV("MV_RELFROM")	//E-Mail utilizado no campo FROM no envio de relatorios por E-Mail;
	Local cPass    := GetMV("MV_RELPSW")	//Senha da Conta de E-Mail para envio de relatorios;
	Local lAuth    := GetMv("MV_RELAUTH")	//Servidor de E-Mail necessita de Autenticacao? Determina se o Servidor necessita de Autenticacao;
	Local cMailAud := GetMv("MV_MAILADT")	//Conta oculta de auditoria utilizada no envio de E-Mail para os relatorios;
	Local lUseSSL  := GetMv("MV_RELSSL")	//Define se o envio e recebimento de E-Mail na rotina SPED utilizara conexao segura (SSL);
	Local lUseTLS  := GetMv("MV_RELTLS")	//Informe se o servidor de SMTP possui conexao do tipo segura (SSL/TLS);
	Local _nPorta  := 587					//Porta Default;
	
	DEFAULT _cPara := ""
	DEFAULT _cMsg  := ""
	DEFAULT _aAnexo  := {}
	DEFAULT _cTitulo := ""

	ProcRegua(15)
	
	//---------------------------------------------------------------------------------------------------------------------
	//ENVIAR EMAIL PARA TI QUANDO FOR AMBIENTE TESTE
	if cEmpAnt <> "01" .and. cEmpAnt <> "02"
		_cPara := cEmails
	endif 
	
	//---------------------------------------------------------------------------------------------------------------------
	//PREENCHENDO O EMAIL PARA RESPOSTA
	DbSelectArea("SX5")
	
	if SX5->(DbSeek(xFilial("SX5")+"WK"+AllTrim(PswID())))
		cEmailFr := AllTrim(SX5->X5_DESCSPA)
	endif

	//---------------------------------------------------------------------------------------------------------------------
	//VALIDANDO OS PARAMETROS INFORMADOS
	If Empty(cServer) .OR. Empty(cEmail) .OR. Empty(cEmailA) .OR. Empty(cPass)
		MsgBox("Verifique os parametros: MV_RELSERV, MV_RELACNT, MV_RELAUSR ou MV_RELPSW!!!","Funcao EnvMail","STOP") 
		Return(.F.)
	EndIf

	If Empty(_cPara)
		MsgBox("Parametro 'Para' tem preenchimento obrigatorio!!!","Funcao EnvMail","STOP") 
		Return(.F.)
	EndIf

	//---------------------------------------------------------------------------------------------------------------------
	//CASO O ENDERECO DO SERVER TENHA A PORTA INFORMADA, SEPARA OS CAMPOS
	If(At(":",cServer) > 0)
		_nPorta := Val(Substr(cServer,At(":",cServer)+1,Len(cServer)))
		cServer := Substr(cServer,0,At(":",cServer)-1)
	EndIf

	//---------------------------------------------------------------------------------------------------------------------
	//CRIA UMA INSTANCIA DA CLASSE TMAILMANAGER
	oMail := TMailManager():New()
	If(lU   
		oMail:SetUseSSL(lUseSSL)
	EndIf
	If(lUseTLS)
		oMail:SetUseTLS(lUseTLS)
	EndIf

	//---------------------------------------------------------------------------------------------------------------------
	//DEFINE AS CONFIGURACOES, DA CLASSE TMAILMANAGER, PARA REALIZAR UMA CONEXAO COM O SERVIDOR DE E-MAIL
	oMail:Init("",cServer,cEmail,cPass,0,_nPorta)

	//---------------------------------------------------------------------------------------------------------------------
	//DEFINE O TEMPO DE ESPERA PARA UMA CONEXAO ESTABELECIDA COM O SERVIDOR DE E-MAIL SMTP (SIMPLE MAIL TRANSFER PROTOCOL)
	If (nTimeout <= 0)
		ConOut("[TIMEOUT] DISABLE")
	Else
		IncProc("[TIMEOUT] ENABLE()")
		ConOut("[TIMEOUT] ENABLE()")
		nRet := oMail:SetSmtpTimeOut(nTimeout)

		If nRet != 0
			ConOut("[TIMEOUT] Fail to set")
			ConOut("[TIMEOUT][ERROR] " + str(nRet,6) , oMail:GetErrorString(nRet))
			MsgBox("[TIMEOUT][ERROR] " + str(nRet,6) + " - " + oMail:GetErrorString(nRet),"Funcao EnvMail","STOP")
			oMail:SMTPDisconnect()
			Return(.F.)
		EndIf
	EndIf

	//---------------------------------------------------------------------------------------------------------------------
	//CONECTA COM O SERVIDOR DE E-MAIL SMTP (SIMPLE MAIL TRANSFER PROTOCOL)
	IncProc("[SMTPCONNECT] connecting ...")
	ConOut("[SMTPCONNECT] connecting ...")
	nRet := oMail:SmtpConnect()
	If nRet <> 0
		ConOut("[SMTPCONNECT] Falha ao conectar")
		ConOut("[SMTPCONNECT][ERROR] " + str(nRet,6) , oMail:GetErrorString(nRet))
		MsgBox("[SMTPCONNECT][ERROR] " + str(nRet,6) + " - " + oMail:GetErrorString(nRet),"Funcao EnvMail","STOP")
		oMail:SMTPDisconnect()
		Return(.F.)
	Else
		ConOut("[SMTPCONNECT] Sucesso ao conectar")
	EndIf

	//---------------------------------------------------------------------------------------------------------------------
	//REALIZA A AUTENTICACAO NO SERVIDOR DE E-MAIL SMTP (SIMPLE MAIL TRANSFER PROTOCOL) PARA ENVIO DE MENSAGENS
	If lAuth
		IncProc("[AUTH] ENABLE")
		ConOut("[AUTH] ENABLE")
		ConOut("[AUTH] TRY with ACCOUNT() and PASS()")

		nRet := oMail:SMTPAuth(cEmail,cPass)
		If nRet != 0
			IncProc("[AUTH] FAIL TRY with ACCOUNT() and PASS()")
			ConOut("[AUTH] FAIL TRY with ACCOUNT() and PASS()")
			ConOut("[AUTH][ERROR] " + str(nRet,6) , oMail:GetErrorString(nRet))

			ConOut("[AUTH] TRY with USER() and PASS()")
			MsgBox("[AUTH][ERROR] " + str(nRet,6) + " - " + oMail:GetErrorString(nRet),"Funcao EnvMail","STOP")
			nRet := oMail:SMTPAuth(cEmailA,cPass)
			If nRet != 0
				ConOut("[AUTH] FAIL TRY with USER() and PASS()")
				ConOut("[AUTH][ERROR] " + str(nRet,6) , oMail:GetErrorString(nRet))
				MsgBox("[AUTH][ERROR] " + str(nRet,6) + " - " + oMail:GetErrorString(nRet),"Funcao EnvMail","STOP")
				oMail:SMTPDisconnect()
				Return(.F.)
			Else
				IncProc("[AUTH] SUCEEDED TRY with USER() and PASS()")
				ConOut("[AUTH] SUCEEDED TRY with USER() and PASS()")
			EndIf
		Else
			IncProc("[AUTH] SUCEEDED TRY with ACCOUNT and PASS")
			ConOut("[AUTH] SUCEEDED TRY with ACCOUNT and PASS")
		EndIf
	Else
		IncProc("[AUTH] DISABLE")
		ConOut("[AUTH] DISABLE")
	EndIf

	//---------------------------------------------------------------------------------------------------------------------
	//CRIA UMA INSTANCIA DA CLASSE TMAILMANAGER
	IncProc("[MESSAGE] Criando mail message")
	ConOut("[MESSAGE] Criando mail message")
	oMessage := TMailMessage():New()
	oMessage:Clear()
	oMessage:cFrom    := cEmailFr
	oMessage:cTo      := _cPara
	oMessage:cSubject := _cTitulo
	oMessage:cBody    := _cMsg
	xRet := oMessage:AttachFile( _aAnexo )
	if xRet < 0
		cMsg := "O arquivo " + _aAnexo + " não foi anexado!"
		alert( cMsg )
		return
	endif
	

	oMessage:MsgBodyType("text/html")

	//---------------------------------------------------------------------------------------------------------------------
	//ENVIA E-MAIL ATRAVÉS DO PROTOCOLO SMTP
	IncProc("[SEND] Sending ...")
	ConOut("[SEND] Sending ...")
	nRet := oMessage:Send(oMail)
	If nRet <> 0
		ConOut("[SEND] Fail to send message")
		ConOut("[SEND][ERROR] " + str(nRet,6) , oMail:GetErrorString(nRet))
		MsgBox("Erro ao enviar e-mail " + str(nRet,6) + " - " + oMail:GetErrorString(nRet),"Funcao EnvMail","STOP")
		oMail:SMTPDisconnect()
		Return(.F.)
	Else
		IncProc("[SEND] Success to send message")
		ConOut("[SEND] Success to send message")
		MsgInfo("E-mail enviado com sucesso","Aviso")
	EndIf

	//---------------------------------------------------------------------------------------------------------------------
	//FINALIZA A CONEXAO ENTRE A APLICACAO E O SERVIDOR DE E-MAIL SMTP (SIMPLE MAIL TRANSFER PROTOCOL)
	IncProc("[DISCONNECT] smtp disconnecting ... ")
	ConOut("[DISCONNECT] smtp disconnecting ... ")
	oMail:SMTPDisconnect()
	If nRet != 0
		IncProc("[DISCONNECT] Fail smtp disconnecting ... ")
		ConOut("[DISCONNECT] Fail smtp disconnecting ... ")
		ConOut("[DISCONNECT][ERROR] " + str(nRet,6) , oMail:GetErrorString(nRet))
		MsgBox("[DISCONNECT][ERROR] " + str(nRet,6) + " - " + oMail:GetErrorString(nRet),"Funcao EnvMail","STOP")
	Else
		IncProc("[DISCONNECT] Success smtp disconnecting ... ")
		ConOut("[DISCONNECT] Success smtp disconnecting ... ")
	EndIf
*/
Return(.T.)

Return .T.
