#Include "PROTHEUS.CH"           
#Include "TOPCONN.CH"
#Include "RWMAKE.CH"

User Function RotImpCSV()

	Local aTexto  := {}
	Local aBotoes := {}
	Local nOpc     := 0
	private cCadastro := "*** Rotina Automática de Importação de Registros *** "	
	
	AADD(aTexto, "   - A primeira linha deve conter colunas com os nomes dos campos Protheus")
	AADD(aTexto, "   - As demais linhas devem conter os conteúdos, respeitando o formato de cada campo")
	AADD(aTexto, "   - Os campos chaves deverão ser sinalizados com um * antes de seu nome")
	AADD(aTexto, "   - A chave de busca será formada pelos campos sinalizados com * na ordem estabelecida")		
	AADD(aTexto, "     Ex: *SXX_FILIAL;*SXX_CODIGO, formará o indice SXX_FILIAL+SXX_CODIGO")	
	AADD(aTexto, "   - Se não houver o registro, a rotina inclui; se já houver, ela altera com base nas chaves")
	AADD(aTexto, "   - O primeiro campo (coluna) DEVE SER A FILIAL")
	AADD(aTexto, "Para iniciar a importação, clique no botão OK...")	
	
	AADD(aBotoes, {1, .T., {|| nOpc := 1, FechaBatch()} })
	AADD(aBotoes, {2, .T., {|| FechaBatch()} })	
	
	FormBatch(cCadastro, aTexto, aBotoes ) 
	
	if nOpc == 1
		Processa( {|| Importa() }, "Processando..." )
	endif
Return

Static Function Importa()
	Local cTitulo1  := "Selecione o arquivo" 
	Local cExtens   := "Arquivos CSV | *.csv"
	Local cMainPath := "C:\\" 
	Local cBuffer   := ""
	Local cFileOpen := ""
	Local lAchou    := .F.
	Local nCont     := 0
	Local aHeader   := {}
	Local aRegis    := {}
	Local aDefCamp  := {}
	Local cIndice   := ""
	Local nOrdem    := ""
	Local nAux
	
	cFileOpen := cGetFile(cExtens,cTitulo1,,cMainPath,.T.)
	
	If !File(cFileOpen)
		MsgAlert("Arquivo " + cFileOpen + " nao localizado", cTitulo1)
		Return	
	Endif     
	
	FT_FUSE(cFileOpen)
	FT_FGOTOP()
	ProcRegua(FT_FLASTREC())
	FT_FGOTOP()
	
	cBuffer := FT_FREADLN()
	aHeader := Separa(cBuffer, ";", .T.)
	FT_FSKIP()
	
	// Recupera alias e tabela e faz validação de nomes de campos
	aTabela := IdAlias(aHeader)
	if Empty(aTabela[1])
		Return
	endif
					
	// Recupera as informações dos campos da tabela
	aDefCamp := GetDfCmp(aHeader)
	
	// Recupera todos os indices da tabela
	aIndice := GetIndic(aTabela[1], aHeader)
	if !Empty(aIndice[1])
		cIndice := aIndice[1]
		nOrdem  := aIndice[2]
	endif
		
	While !FT_FEOF()
		IncProc("Importando " + aTabela[2] + "...")
		
		cBuffer := FT_FREADLN()
		aRegis  := Separa(cBuffer, ";", .T.)
		
	
		if Len(aRegis) <> Len(aHeader)
			Alert("A quantidade de campos da linha x difere a quantidade de campos do cabeçalho!")
			Return		
		endif
	
		// Monta a string para o DbSeek
		cBusca := GetSeek(cIndice, aHeader, aRegis, aDefCamp)
		             
		// Trata os campos sinalizados com "*"
/*		for nAux := 1 to Len(aHeader)
			if substr(aHeader[nAux], 1, 1) == "*"
				aHeader[nAux] := substr(aHeader[nAux], 2, len(aHeader[nAux]) - 1)
			endif
		next nAux*/
		
		DbSelectArea(aTabela[1])
		DbSetOrder(nOrdem)
		lAchou := DbSeek(cBusca)
		RecLock(aTabela[1], !lAchou)
//		RecLock(aTabela[1], .T.)
		
			for nAux := 1 to Len(aHeader)				
				
				if substr(aHeader[nAux], 1, 1) == "*"
					if lAchou //Alteracao
						Loop					                     
					else
						aHeader[nAux] := substr(aHeader[nAux], 2, len(aHeader[nAux]) - 1)					
					endif
				endif
				
				if  aDefCamp[nAux][1] == "N"
					&(aHeader[nAux]) := TrataVal(aRegis[nAux])
				elseif aDefCamp[nAux][1] == "D"
					&(aHeader[nAux]) := CTOD(aRegis[nAux])
				else
					&(aHeader[nAux]) := TrataStr(aRegis[nAux], aDefCamp[nAux][2])
				endif
			next nAux
						
		MsUnlock()			
			
		// Aplicar Commit quando tiver muitos registros pendentes
		if nCont > 1000
			nCont := 0
			DbCommit()		
		endif
		
		FT_FSKIP()		
	Enddo
	              
	FT_FUSE()
	
	MsgInfo("Importação realizada com sucesso!")
Return

/*
	Rotina para verificar o alias e o nome da tabela e valida se todos os campos estão no mesmo padrão.
*/
Static Function IdAlias(aHeader)
	Local nAux     := 0
	Local cCampo   := ""
	Local cCampoAn := ""
	Local cRet     := ""
	Local cAuxAnt  := ""
	
	For nAux := 1 to Len(aHeader)
		cCampo := aHeader[nAux]
		if substr(cCampo, 1, 1) == "*"
			cCampo := substr(cCampo, 2, len(cCampo)-1)
		endif		
		
		if At("_", cCampo) == 3 //Ex: A1_COD
			cRet := substr(cCampo, 1, 2)
		elseif At("_", cCampo) == 4 //Ex: DA0_CODTAB
			cRet := substr(cCampo, 1, 3)		
		endif
		
		if Empty(cRet)
			Alert("Formatdo do campo " + cCampo + " está incorreto!")
			exit
		endif
		
		if nAux > 1 
			if cRet <> cAuxAnt
				cRet := ""
				Alert("Os campos " + cCampo + " e " + cCampoAn + " não pertencem a mesma tabela!")
				exit
			endif		
		endif		
		
		cAuxAnt  := cRet
		cCampoAn := cCampo 
	next nAux
	
	if Len(cRet) == 2
		cRet := "S" + cRet	
	endif

	DbSelectArea("SX2")
	if !SX2->(DbSeek(cRet))
		cRet := ""
		Alert("Tabela " + cRet + " não encontrada no dicionário de dados!")	
	endif
		
Return {cRet, SX2->X2_NOME}

Static Function GetIndic(cTabela, aHeader)
	Local nOrdem := 1
	Local cChave := ""
	Local nAux
	
	//Obter o índice, especificado pelos campos na ordem sinalizados com "*"
	for nAux := 1 to len(aHeader)
		cCampo := aHeader[nAux]
		if substr(cCampo, 1, 1) == "*"
			cCampo := substr(cCampo, 2, len(cCampo)-1)
			cChave += cCampo + "+"
		endif		
	next nAux
	
	if substr(cChave, len(cChave), 1) == "+"
		cChave := substr(cChave, 1, len(cChave) - 1)	
	endif
		
//	DbSelectArea("SX2")
//	if SX2->(DbSeek(cTabela))
//		cInd := AllTrim(SX2->X2_UNICO)
		
		DbSelectArea("SIX")
		SIX->(DbSeek(cTabela))
		while !SIX->(EOF()) .AND. SIX->INDICE == cTabela 
			if AllTrim(SIX->CHAVE) == AllTrim(cChave)
				nOrdem := Val(SIX->ORDEM)
			endif
			SIX->(DbSkip())	
		enddo		
//	endif	
	
Return {cChave, nOrdem}

Static Function GetSeek(cIndice, aHeader, aRegis, aDefCamp)
	Local cRet    := ""
	Local aCampos := {}
	Local nAux     := 1
	Local cHeader
	
	aCampos := Separa(cIndice, "+", .T.)
	
	For nAux := 1 to Len(aHeader)
		if nAux <= len(aCampos) 
			cHeader := aHeader[nAux]
			if SubStr( aHeader[nAux], 1, 1 ) == '*'
				cHeader := SubStr( aHeader[nAux], 2, Len( aHeader[nAux] ) )
			endif
			if cHeader == aCampos[nAux] 
				//cRet += aRegis[nAux] 
				if alltrim(cHeader) == 'RF_DATABAS'  //retirar
					cRet += dtos(ctod(aRegis[nAux]))     //retirar
				elseif alltrim(cHeader) == 'R7_DATA'  //retirar
					cRet += dtos(ctod(aRegis[nAux]))     //retirar
				elseif alltrim(cHeader) == 'R3_DATA'  //retirar
					cRet += dtos(ctod(aRegis[nAux]))     //retirar					
				elseif alltrim(cHeader) == 'RG_DTGERAR'  //retirar
					cRet += dtos(ctod(aRegis[nAux]))     //retirar
				elseif alltrim(cHeader) == 'RT_DATACAL'  //retirar
					cRet += dtos(ctod(aRegis[nAux]))     //retirar					
				elseif alltrim(cHeader) == 'RH_DATABAS'  //retirar
					cRet += dtos(ctod(aRegis[nAux]))     //retirar					
				elseif alltrim(cHeader) == 'RH_DATAINI'  //retirar
					cRet += dtos(ctod(aRegis[nAux]))     //retirar

//				elseif alltrim(cHeader) == 'RA_FILIAL'  //retirar
//					cRet += STRZERO(VAL(TrataStr(aRegis[nAux], aDefCamp[nAux][2])),4)     //retirar										
//				elseif alltrim(cHeader) == 'RA_MAT'  //retirar
//					cRet += STRZERO(VAL(TrataStr(aRegis[nAux], aDefCamp[nAux][2])),6)     //retirar										
															
				else                                     //retirar
					cRet += TrataStr(aRegis[nAux], aDefCamp[nAux][2])
				endif	                                //retirar
//			else
//				exit
			endif
		endif		
	Next nAux
	
	// Retira o último "+", se for o caso
	if !Empty(cRet) .AND. substr(cRet, len(cRet), 1) == "+"
		cRet := substr(cRet, 1, len(cRet) - 1)	
	endif	
	
Return cRet

Static Function GetDfCmp(aHeader)
	Local aDefine := {}
	Local nAux    := 1
	Local cCampo  := ""
	
	for nAux := 1 to Len(aHeader)
		DbSelectArea("SX3")
		SX3->(DbSetOrder(2))
		cCampo := aHeader[nAux]
		if substr(cCampo, 1, 1) == "*"
			cCampo := substr(cCampo, 2, len(cCampo) - 1) 
		endif
		
		if SX3->(DbSeek(cCampo))
			AADD(aDefine, {SX3->X3_TIPO, SX3->X3_TAMANHO, SX3->X3_DECIMAL, SX3->X3_PICTURE, SX3->X3_VALID})
		else
			AADD(aDefine, {"", 0, 0, "", ""})
		endif	
	next nAux
	
Return aDefine

Static Function TrataVal(cValor)
	Local nRet := 0
	
	if !Empty(cValor)
		cValor := StrTran(cValor, ",", ".")
	 	nRet   := Val(cValor)
	else
		nRet := 0
	endif
Return nRet

Static Function TrataStr(cValor, nTam)
Return PADR(cValor, nTam)
