#Include "PROTHEUS.CH"           
#Include "TOPCONN.CH"
#INCLUDE "RWMAKE.CH"

//******************************************************************************************************************
// SETUP INICIAL TOTVS OESTE - VERSaO 2 - 03/11/2021 - Igor Mateus da Silva - 
//******************************************************************************************************************

//Colaboraaao de muitos Consultores da TOTVSOeste - Juntos Somos mais fortes! Layout do Wizard e Base utilizada do Antigo Setup_Inicial (Banho de Loja)

// 	***************************************************** ATENCAO!! *************************************************************
// ******* INSTRUCOES PARA A EXECUCAO DESTE FONTE - IMPRESCIND9VEL LER O ARQUIVO    **** LEIAME.TXT ****    PARA PARAMETRIZAaaO CORRETA ! *******


//AJUSTES
//FABRICIO AMARO - 29/04/2024
// - INCLUSAO PARA CHAMADA DA FUNCAO QUE AJUSTA AS TABELAS DO SIGAGPE - U_XINIFOL - COMPILAR O FONTE FOLHA_INI.PRW
// - INCLUSAO PARA CHAMADA VIA DEBUG

User Function SetupTO()

	Local oNewPag
	Local oStepWiz := nil
	Local oPanelBkg

	//FABRICIO AMARO - INCLUSAO PARA CHAMADA VIA DEBUG
	Local cEmp := "99"
	Local cFil := "01"

	//EM CASO DE EXECUÇÃO VIA DEBUG
	If Select("SX2") == 0
		RPCCLEARENV()
		RPCSetType(3)
		RPCSetEnv(cEmp,cFil,"","","","",{})
	else
		cEmp := cEmpAnt
		cFil := cFilAnt
	EndIf



	Private cEmpresa := ""+FWGrpCompany()+"0"  //  <<<<<<<<<<<<<<<<================  ****   PADRaO DE TABELA "990" OU "010"

	Private oDlg := nil
	Private oPanel
	Private cNomeBkp	:=""
    Private cUsa	    := "x       x       x       x       x       x       x       x       x       x       x       x       x       x       x x     "
    Private cRes 	    := "     x x        "
    Private strTabela   :=  ""
    Private strCampo    :=  ""
    Private strTam      :=  ""
    Private strDec      :=  ""
    Private strTipo     :=  ""
    Private strPict     :=  ""

    Private aSX6        := {}
	Private aParam		:= {}
	Private aGatilho	:= {}

	// Campos Passos que sobraram linhas comentadas
	Private aConsulta	:= {}
	Private aTrocaCons	:= {}
	Private IntCont 	
	// Campo armazena String SQL
	Private strSQL 		:= ""

	//Campos Lista Suspensa
	//BACKOFFICE
	Private cbComp1		:= {}
	Private cbComp2		:= {}
	Private cbComp3		:= {}
	Private cbComp4		:= {}
	//CONTROLADORIA
	Private cbEst1		:= {}
	Private cbEst2		:= {}	
	Private cbEst3		:= {}
	Private cbEst4		:= {}
	//MATERIAIS
	Private cbFat1		:= {}
	Private cbFat2		:= {}
	Private cbFat3		:= {}
	Private cbFat4		:= {}
	//DISTRIBUIaaO E Logistica
	Private cbFin1		:= {}
	Private cbFin2		:= {}
	Private cbFin3		:= {}
	Private cbFin4		:= {}
	//QUALIDADE
	Private cbQual1		:= {}
	Private cbQual2		:= {}
	Private cbQual3		:= {}
	Private cbQual4		:= {}
	//RECURSOS HUMANOS
	Private cbRh1		:= {}
	Private cbRh2		:= {}
	Private cbRh3		:= {}
	Private cbRh4		:= {}

	//GERAL 
	Private cbGer1		:= {}
	Private cbGer2		:= {}

	// Campos Cria Object de Buttons
	//CONTROLADORIA
	Private oCom1		:= {}
	Private oCom2		:= {}
	Private oCom3		:= {}
	Private oCom4		:= {}
	//VENDAS
	Private oEst1		:= {}
	Private oEst2		:= {}
	Private oEst3		:= {}
	Private oEst4		:= {}
	//MATERIAIS
	Private oFat1		:= {}
	Private oFat2		:= {}
	Private oFat3		:= {}
	Private oFat4		:= {}
	//DISTRIBUIaaO E Logistica
	Private oFin1		:= {}
	Private oFin2		:= {}
	Private oFin3		:= {}
	Private oFin4		:= {}
	//QUALIDADE
	Private oQual1		:= {}
	Private oQual2		:= {}
	Private oQual3		:= {}
	Private oQual4		:= {}
	//RECURSOS HUMANOS
	Private oRh1		:= {}
	Private oRh2		:= {}
	Private oRh3		:= {}
	Private oRh4		:= {}
	//GERAL

	Private oGer1		:= {}
	Private oGer2		:= {}

	// VETORES COM OPCOES DO SETUP INICIAL

	Private aItems 		:= {'Nao','Sim'}
	Private aItems2 	:= {'0','2','4','6','8'}


//******************************************************************************************************************
// CRIAaaO DAS STEPS WIZARD - VERSaO 2 - 03/11/2021 - Igor Mateus da Silva  
//******************************************************************************************************************

	DEFINE DIALOG oDlg TITLE 'SETUP INICIAL - TOTVS OESTE' PIXEL
	oDlg:nWidth := 1000
	oDlg:nHeight := 525
	oPanelBkg:= tPanel():New(0,0,"",oDlg,,,,,,500,250)
	oStepWiz:= FWWizardControl():New(oPanelBkg)//Instancia a classe FWWizard
	oStepWiz:ActiveUISteps()

	//----------------------
	// Pagina 1
	//----------------------
	oNewPag := oStepWiz:AddStep("1")
	//Altera a Descricao do step
	oNewPag:SetStepDescription("BCKOF: COM EST FIN FAT")
	//Define o bloco de construaao
	oNewPag:SetConstruction({|Panel|CriaPag1(Panel)})
	//Define o bloco ao clicar no botao Praximo
	oNewPag:SetNextAction({||ValPag1()})
	//Define o bloco ao clicar no botao Cancelar
	oNewPag:SetCancelAction({||oDlg:end(), .T.})

	//----------------------
	// Pagina 2
	//----------------------
	//oNewPag := oStepWiz:AddStep("2", {|Panel|cria_pg2(Panel, @nDescTab, @nDescCom)})
	oNewPag := oStepWiz:AddStep("2", {|Panel|CriaPag2(Panel)})
	oNewPag:SetStepDescription("CONTROLADORIA")
	oNewPag:SetNextAction({||ValPag2()})
	//Define o bloco ao clicar no botao Voltar
	oNewPag:SetCancelAction({||oDlg:end(), .T.})
	//oNewPag:SetPrevAction({||Alert(nDescTab), .F.})
	oNewPag:SetPrevTitle("Voltar")	

	//----------------------
	// Pagina 3
	//----------------------
	oNewPag := oStepWiz:AddStep("3", {|Panel|CriaPag3(Panel)})
	oNewPag:SetStepDescription("MATERIAIS")
	oNewPag:SetNextAction({||ValPag3()})
	//Define o bloco ao clicar no botao Voltar
	oNewPag:SetCancelAction({||oDlg:end(), .T.})
	//oNewPag:SetPrevAction({||Alert(nDescTab), .F.})
	oNewPag:SetPrevTitle("Voltar")	

	//----------------------
	// Pagina 4
	//----------------------
	oNewPag := oStepWiz:AddStep("4", {|Panel|CriaPag4(Panel)})
	oNewPag:SetStepDescription("DISTRIB. E LOGIST.")
	oNewPag:SetNextAction({||ValPag4()})
	//Define o bloco ao clicar no botao Voltar
	oNewPag:SetCancelAction({||oDlg:end(), .T.})
	//oNewPag:SetPrevAction({||Alert(nDescTab), .F.})
	oNewPag:SetPrevTitle("Voltar")		


	//----------------------
	// Pagina 5
	//----------------------
	oNewPag := oStepWiz:AddStep("5", {|Panel|CriaPag5(Panel)})
	oNewPag:SetStepDescription("QUALIDADE")
	oNewPag:SetNextAction({||ValPag5()})
	//Define o bloco ao clicar no botao Voltar
	oNewPag:SetCancelAction({||oDlg:end(), .T.})
	//oNewPag:SetPrevAction({||Alert(nDescTab), .F.})
	oNewPag:SetPrevTitle("Voltar")		


	//----------------------
	// Pagina 6
	//----------------------
	oNewPag := oStepWiz:AddStep("6", {|Panel|CriaPag6(Panel)})
	oNewPag:SetStepDescription("RECURSOS HUMANOS")
	oNewPag:SetNextAction({||ValPag6()})
	//Define o bloco ao clicar no botao Voltar
	oNewPag:SetCancelAction({||oDlg:end(), .T.})
	//oNewPag:SetPrevAction({||Alert(nDescTab), .F.})
	oNewPag:SetPrevTitle("Voltar")	


	//----------------------
	// Pagina 7
	//----------------------
	oNewPag := oStepWiz:AddStep("7", {|Panel|CriaPag7(Panel)})
	oNewPag:SetStepDescription("GERAL - MODULOS")
	oNewPag:SetNextAction({||ValPag7()})
	//Define o bloco ao clicar no botao Voltar
	oNewPag:SetCancelAction({||oDlg:end(), .T.})
	//oNewPag:SetPrevAction({||Alert(nDescTab), .F.})
	oNewPag:SetPrevTitle("Voltar")	


	//----------------------
	// Pagina 8
	//----------------------
	oNewPag := oStepWiz:AddStep("8", {|Panel|CriaPag8(Panel)})
	oNewPag:SetStepDescription("FINALIZACAO")
	oNewPag:SetNextAction({||Processa({|| AtuBase()}, "Processando..."), .T.})
	oNewPag:SetCancelAction({||oDlg:end(), .T.})
	oNewPag:SetCancelWhen({||.F.})

	oStepWiz:Activate()
	ACTIVATE DIALOG oDlg CENTER
	oStepWiz:Destroy()	

Return




//CONTROLADORIA
Static Function CriaPag1(oPanel)
	Local oCom1
	Local oCom2
	Local oCom3
	Local oCom4
	Local btConsul


	@ 220,070 BUTTON btConsul PROMPT "Consultar Conteudo Steps"  SIZE 090,015 OF oDlg PIXEL ACTION aMEn1()
	oSay1:= TSay():New(10,10,{||'Atualiza Campos / Abas:'},oPanel,,,,,,.T.,,,200,20)

	cbComp1:= aItems[1]
	oCom1 := TComboBox():New(20,10,{|u|if(PCount()>0,cbComp1:=u,cbComp1)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbComp1')

	oSay1:= TSay():New(40,10,{||'Atualiza Indices / Gatilhos:'},oPanel,,,,,,.T.,,,200,20)
	cbComp2:= aItems[1]
	oCom2 := TComboBox():New(50,10,{|u|if(PCount()>0,cbComp2:=u,cbComp2)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbComp2')

	oSay1:= TSay():New(70,10,{||'Atualiza Parametros:'},oPanel,,,,,,.T.,,,200,20)
	cbComp3:= aItems[1]
	oCom3 := TComboBox():New(80,10,{|u|if(PCount()>0,cbComp3:=u,cbComp3)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbComp3')

	oSay1:= TSay():New(100,10,{||'Upload Dados / Cadastros para Simulacoes:'},oPanel,,,,,,.T.,,,200,20)
	cbComp4:= aItems[1]
	oCom4 := TComboBox():New(110,10,{|u|if(PCount()>0,cbComp4:=u,cbComp4)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbComp4')

Return(.T.)

Static Function aMen1()
	Local oDlg

	DEFINE DIALOG oDlg TITLE "MODULOS CONTEMPLADOS NO SETUP INICIAL - TOTVS OESTE" FROM 150,150 TO 450,800 PIXEL
		
		@ 010,030 SAY "Modulos do STEP 01 (Backoffice): ATF - COM - EST - FAT - FIN" SIZE 400, 08 OF oDlg PIXEL
		@ 030,030 SAY "Modulos do STEP 02 (Controladoria): FIS - CTB - PMS - PCO" SIZE 400, 08 OF oDlg PIXEL
		@ 050,030 SAY "Modulos do STEP 03 (Materiais): DPR - AGR - GCT - PCP - MNT - SFC" SIZE 400, 08 OF oDlg PIXEL
		@ 070,030 SAY "Modulos do STEP 04 (Distrib. e Logistica): GTP - OMS - WMS - GFE" SIZE 400, 08 OF oDlg PIXEL
		@ 090,030 SAY "Modulos do STEP 05 (Qualidade): QDO - QMT - QIP - QIE - QNC - QAD - PPAP" SIZE 400, 08 OF oDlg PIXEL
		@ 110,030 SAY "Modulos do STEP 06 (Recursos Humanos): GPE - PON" SIZE 400, 08 OF oDlg PIXEL
		@ 130,030 SAY "Modulos do STEP 07 (Atualizacao Campos): Atualizacao campos QTD e VLR  (Casas Decimais)" SIZE 400, 08 OF oDlg PIXEL

	ACTIVATE DIALOG oDlg CENTERED
Return

Static Function ValPag1(oPanel)
Return(.T.)

//VENDAS
Static Function CriaPag2(oPanel)
	Local oEst1
	Local oEst2
	Local oEst3
	Local oEst4

	oSay1:= TSay():New(10,10,{||'Atualiza Campos / Abas:'},oPanel,,,,,,.T.,,,200,20)
	cbEst1:= aItems[1]
	oEst1 := TComboBox():New(20,10,{|u|if(PCount()>0,cbEst1:=u,cbEst1)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbEst1')

	oSay1:= TSay():New(40,10,{||'Atualiza indices / Gatilhos:'},oPanel,,,,,,.T.,,,200,20)
	cbEst2:= aItems[1]
	oEst2 := TComboBox():New(50,10,{|u|if(PCount()>0,cbEst2:=u,cbEst2)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbEst2')

	oSay1:= TSay():New(70,10,{||'Atualiza Parametros:'},oPanel,,,,,,.T.,,,200,20)
	cbEst3:= aItems[1]
	oEst3 := TComboBox():New(80,10,{|u|if(PCount()>0,cbEst3:=u,cbEst3)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbEst3')

	oSay1:= TSay():New(100,10,{||'Upload Dados / Cadastros para Simulacoes:'},oPanel,,,,,,.T.,,,200,20)	
	cbEst4:= aItems[1]
	oEst4 := TComboBox():New(110,10,{|u|if(PCount()>0,cbEst4:=u,cbEst4)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbEst4')
	
Return(.T.)

Static Function ValPag2(oPanel)
Return(.T.)

//MATERIAIS
Static Function CriaPag3(oPanel)

	Local oFat1
	Local oFat2
	Local oFat3
	Local oFat4

	oSay1:= TSay():New(10,10,{||'Atualiza Campos / Abas:'},oPanel,,,,,,.T.,,,200,20)
	cbFat1:= aItems[1]
	oFat1 := TComboBox():New(20,10,{|u|if(PCount()>0,cbFat1:=u,cbFat1)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbFat1')

	oSay1:= TSay():New(40,10,{||'Atualiza indices / Gatilhos:'},oPanel,,,,,,.T.,,,200,20)
	cbFat2:= aItems[1]
	oFat2 := TComboBox():New(50,10,{|u|if(PCount()>0,cbFat2:=u,cbFat2)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbFat2')

	oSay1:= TSay():New(70,10,{||'Atualiza Parametros:'},oPanel,,,,,,.T.,,,200,20)
	cbFat3:= aItems[1]
	oFat3 := TComboBox():New(80,10,{|u|if(PCount()>0,cbFat3:=u,cbFat3)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbFat3')

	oSay1:= TSay():New(100,10,{||'Upload Dados / Cadastros para Simulacoes:'},oPanel,,,,,,.T.,,,200,20)
	cbFat4:= aItems[1]
	oFat4 := TComboBox():New(110,10,{|u|if(PCount()>0,cbFat4:=u,cbFat4)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbFat4')
Return(.T.)

Static Function ValPag3(oPanel)
Return(.T.)

//DISTRIBUIaaO E Logistica
Static Function CriaPag4(oPanel)

	Local oFin1
	Local oFin2
	Local oFin3
	Local oFin4

	oSay1:= TSay():New(10,10,{||'Atualiza Campos / Abas:'},oPanel,,,,,,.T.,,,200,20)
	cbFin1:= aItems[1]
	oFin1 := TComboBox():New(20,10,{|u|if(PCount()>0,cbFin1:=u,cbFin1)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbFin1')

	oSay1:= TSay():New(40,10,{||'Atualiza indices / Gatilhos:'},oPanel,,,,,,.T.,,,200,20)
	cbFin2:= aItems[1]
	oFin2 := TComboBox():New(50,10,{|u|if(PCount()>0,cbFin2:=u,cbFin2)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbFin2')

	oSay1:= TSay():New(70,10,{||'Atualiza Parametros:'},oPanel,,,,,,.T.,,,200,20)
	cbFin3:= aItems[1]
	oFin3 := TComboBox():New(80,10,{|u|if(PCount()>0,cbFin3:=u,cbFin3)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbFin3')

	oSay1:= TSay():New(100,10,{||'Upload Dados / Cadastros para Simulacoes:'},oPanel,,,,,,.T.,,,200,20)
	cbFin4:= aItems[1]
	oFin4 := TComboBox():New(110,10,{|u|if(PCount()>0,cbFin4:=u,cbFin4)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbFin4')
Return(.T.)

Static Function ValPag4(oPanel)
Return(.T.)


//QUALIDADE
Static Function CriaPag5(oPanel)

	Local oQual1
	Local oQual2
	Local oQual3
	Local oQual4

	oSay1:= TSay():New(10,10,{||'Atualiza Campos / Abas:'},oPanel,,,,,,.T.,,,200,20)
	cbQual1:= aItems[1]
	oQual1 := TComboBox():New(20,10,{|u|if(PCount()>0,cbQual1:=u,cbQual1)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbQual1')

	oSay1:= TSay():New(40,10,{||'Atualiza indices / Gatilhos:'},oPanel,,,,,,.T.,,,200,20)
	cbQual2:= aItems[1]
	oQual2 := TComboBox():New(50,10,{|u|if(PCount()>0,cbQual2:=u,cbQual2)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbQual2')

	oSay1:= TSay():New(70,10,{||'Atualiza Parametros:'},oPanel,,,,,,.T.,,,200,20)
	cbQual3:= aItems[1]
	oQual3 := TComboBox():New(80,10,{|u|if(PCount()>0,cbQual3:=u,cbQual3)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbQual3')

	oSay1:= TSay():New(100,10,{||'Upload Dados / Cadastros para Simulacoes:'},oPanel,,,,,,.T.,,,200,20)
	cbQual4:= aItems[1]
	oQual4 := TComboBox():New(110,10,{|u|if(PCount()>0,cbQual4:=u,cbQual4)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbQual4')


Return(.T.)

Static Function ValPag5(oPanel)
Return(.T.)



//RECURSOS HUMANOS
Static Function CriaPag6(oPanel)

	Local oRh1
	Local oRh2
	Local oRh3
	Local oRh4

	oSay1:= TSay():New(10,10,{||'Atualiza Campos / Pastas / Gatilhos / Parametros:'},oPanel,,,,,,.T.,,,200,20)
	cbRh1:= aItems[1]
	oRh1 := TComboBox():New(20,10,{|u|if(PCount()>0,cbRh1:=u,cbRh1)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbRh1')

	/*
	oSay1:= TSay():New(40,10,{||'Atualiza indices / Gatilhos:'},oPanel,,,,,,.T.,,,200,20)
	cbRh2:= aItems[1]
	oRh2 := TComboBox():New(50,10,{|u|if(PCount()>0,cbRh2:=u,cbRh2)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbRh2')

	oSay1:= TSay():New(70,10,{||'Atualiza Parametros:'},oPanel,,,,,,.T.,,,200,20)
	cbRh3:= aItems[1]
	oRh3 := TComboBox():New(80,10,{|u|if(PCount()>0,cbRh3:=u,cbRh3)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbRh3')

	oSay1:= TSay():New(100,10,{||'Upload Dados / Cadastros para Simulacoes:'},oPanel,,,,,,.T.,,,200,20)
	cbRh4:= aItems[1]
	oRh4 := TComboBox():New(110,10,{|u|if(PCount()>0,cbRh4:=u,cbRh4)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbRh4')
	*/

Return(.T.)


Static Function ValPag6(oPanel)
Return(.T.)


//RECURSOS HUMANOS
Static Function CriaPag7(oPanel)

	Local oGer1
	Local oGer2
	//Local oRh2
	//Local oRh3 
	//Local oRh4

	oSay1:= TSay():New(10,10,{||'Escolha Quantidade Decimais (Apos Virgula) para Campos "QUANTIDADE" - Informe 0 para não atualizar'},oPanel,,,,,,.T.,,,300,20)
	cbGer1:= aItems2[1]
	oGer1 := TComboBox():New(20,10,{|u|if(PCount()>0,cbGer1:=u,cbGer1)},aItems2,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbGer1')


	oSay1:= TSay():New(40,10,{||'Escolha Quantidade Decimais (Apos Virgula) para Campos "VALOR" - Informe 0 para não atualizar'},oPanel,,,,,,.T.,,,300,20)
	cbGer2:= aItems2[1]
	oGer2 := TComboBox():New(50,10,{|u|if(PCount()>0,cbGer2:=u,cbGer2)},aItems2,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbGer2')

/*
	oSay1:= TSay():New(70,10,{||'Atualiza Parametros:'},oPanel,,,,,,.T.,,,200,20)
	cbRh3:= aItems[1]
	oRh3 := TComboBox():New(80,10,{|u|if(PCount()>0,cbRh3:=u,cbRh3)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbRh3')

	oSay1:= TSay():New(100,10,{||'Atualiza Layouts / Templates:'},oPanel,,,,,,.T.,,,200,20)
	cbRh4:= aItems[1]
	oRh4 := TComboBox():New(110,10,{|u|if(PCount()>0,cbRh4:=u,cbRh4)},aItems,50,20,oPanel,,{|| },,,,.T.,,,,,,,,,'cbRh4')
*/

Return(.T.)


Static Function ValPag7(oPanel)
Return(.T.)


Static Function CriaPag8(oPanel)

	oSay1:= TSay():New(10,10,{||'-> Iremos Aplicar a Atualizacao nas tabelas de acordo com o Selecionado! Clique em Concluir. '},oPanel,,,,,,.T.,,,300,20)

Return(.T.)


Static Function ValPag8(oPanel)
Return(.T.)


//******************************************************************************************************************
// USER FUNCTIONS COM AS ATUALIZACOES DE ACORDO COM MENUS - RESPOSTAS "SIM" E Parametros -  Igor Mateus 11/2021
//******************************************************************************************************************


Static Function AtuBase()


	u_CriaCHK() //Verifica as tabelas que ja estao inicializadas no DB, e cria caso nao esteja, para usar o RECLOCK.

	If !(MsgBox("Deseja realmente executar o Setup Inicial?" + chr(13) + chr(10) + ;
		"Sera executado para o grupo de empresa: " + cEmpresa,"Setup Inicial","YESNO"))
		oDlg:end()
		Return
	EndIf

	ProcRegua(25)

	// OPCOES DO BACKOFFICE
	If cbComp1 == "Sim"

		//Campos e Abas / Controladoria
		IncProc("Modulo 1 - Criando Campos / Abas... 1 de 25...")

		Atucampos()  	 // Atualiza e Cria Campos do (Backoffice)
		AbaSA1()		 // Reorganiza e Cria abas da tabela SA1 (Backoffice)
		AbaSA2()		 // Reorganiza e Cria abas da tabela SA2 (Backoffice)
		AbaSB1()		 // Reorganiza e Cria abas da Tabela SB1 (Backoffice)
		CriaSX5()		 // Cria Tabelas Genericas na SX5 (Backoffice)
		AtuConsulta()	 // Atualiza e Cria consultas Padroes 	 (Backoffice)
		AbaSED()	     // Reorganiza e Cria Abas da Tabela SED (Backoffice)
		AtuBack()		 // Atualiza Informacoes dentro de Campos do Backoffice em Geral (Backoffice)
		AbaSC5()	 	 // Reorganiza e Cria abas da Tabela SC5 (Backoffice)
	EndIF
	
	If cbComp2 == "Sim"
		//Indices e Gatilhos / Controladoria
		IncProc("Modulo 1 - Criando indices / Gatilhos... 2 de 25...")
		AtuGatilho() // Atualiza / Cria Gatilhos do Backoffice
	EndIF

	If cbComp3 == "Sim"
		//Parametros / Controladoria
		IncProc("Modulo 1 - Criando Parametros... 3 de 25...")
		AtuParam() // Atualiza / Cria Parametros do Backoffice
	EndIF

	If cbComp4 == "Sim"

		//Layouts e Templates / Controladoria
		IncProc("Modulo 1 - Aplicando Carga de Dados... 4 de 25...")
		u_CriaSB1()  // Realiza a Carga de Dados para Produtos
		u_CriaSA1()  // Realiza a Carga de Dados para Clientes
		u_CriaSA2()  // Realiza a Carga de Dados para Fornecedores
		u_CriaSA3()  // Realiza a Carga de Dados para Vendedores
		u_CriaSA4()  // Realiza a Carga de Dados para Transportadoras
		u_CriaSA5()  // Realiza a Carga de Dados para Produto x Fornecedor
		u_CriaSA6()  // Realiza a Carga de Dados para Bancos
		u_CriaSED()  // Realiza a Carga de Dados para Naturezas Financeiras
		u_CriaSF4()  // Realiza a Carga de Dados para Tes
		u_CriaSF5()  // Realiza a Carga de Dados para Tipos de MovimentaCOES
		u_CriaSBM()  // Realiza a Carga de Dados para Tipos de Grupos de Produtos
		u_CriaSN1()  // Realiza a Carga de Dados para Ativo Imobilizado
		u_CriaNNR()  // Realiza a Carga de Dados para Armazens
		u_CriaSE4()  // Realiza a Carga de Dados para CondiCOES de Pagamento

	EndIF


	//OPCOES DO CONTROLADORIA 
	If cbEst1 == "Sim"
		//Campos e Abas / Vendas
		IncProc("Modulo 2 - Criando Campos / Abas... 5 de 25..")
		AbaSF4() // Atualiza Abas da TES
	EndIF
	
	If cbEst2 == "Sim"
		//Indices e Gatilhos / Vendas
		IncProc("Modulo 2 - Criando indices / Gatilhos... 6 de 25..")
		Sleep(1000)
	EndIF
	
	If cbEst3 == "Sim"
		//Parametros / Vendas
		IncProc("Modulo 2 - Criando Parametros... 7 de 25..")
		Sleep(1000)
	EndIF
	
	If cbEst4 == "Sim"
		//Layouts e Templates / Vendas
		IncProc("Modulo 2 - Aplicando Carga de Dados... 8 de 25..")
		Sleep(1000)
	EndIF


	// OPCOES DO MATERIAIS
	If cbFat1 == "Sim"
		//Campos e Abas / Materiais
		IncProc("Modulo 3 - Criando Campos / Abas... 9 de 25..")
		Sleep(1000)
		
	EndIF

	If cbFat2 == "Sim"
		//indices e Gatilhos / Materiais
		IncProc("Modulo 3 - Criando indices / Gatilhos... 10 de 25..")
		Sleep(1000)
	EndIF
	
	If cbFat3 == "Sim"
		//Parametros / Materiais
		IncProc("Modulo 3 - Criando Parametros... 11 de 25..")
		AtParPCP() // Atualiza Parametros do PCP
	EndIF
	
	If cbFat4 == "Sim"
		//Layouts e Templates / Materiais
		IncProc("Modulo 3 - Aplicando Carga de Dados... 12 de 25..")
		Sleep(1000)
	EndIF


	// OPCOES DISTRIBUICAO E Logistica
	If cbFin1 == "Sim"
		//Campos e Abas / Distribuicao e Logistica
		IncProc("Modulo 4 - Criando Campos / Abas... 13 de 25..")
		Sleep(1000)
	EndIF
	
	If cbFin2 == "Sim"
		//indices e Gatilhos / Distribuiaao e Logistica
		IncProc("Modulo 4 - Criando indices / Gatilhos... 14 de 25..")
		Sleep(1000)
	EndIF
	
	If cbFin3 == "Sim"
		//Parametros / Distribuiaao e Logistica
		IncProc("Modulo 4 - Criando Parametros... 15 de 25..")
		AtParMNT() 
		AtParCob() 
		AtParPne() 
	EndIF
	
	If cbFin4 == "Sim"
		//Layouts e Templates / Distribuiaao e Logistica
		IncProc("Modulo 4 - Aplicando Carga de Dados... 16 de 25..")
		Sleep(1000)
	EndIF


	// OPCOES DA QUALIDADE
	If cbQual1 == "Sim"
		//Campos e Abas / Qualidade
		IncProc("Modulo 5 - Criando Campos / Abas... 17 de 25..")
		Sleep(1000)
	EndIF
	
	If  cbQual2 == "Sim"
		//indices e Gatilhos / Qualidade
		IncProc("Modulo 5 - Criando indices / Gatilhos... 18 de 25..")
		Sleep(1000)
	EndIF
	
	If  cbQual3 == "Sim"
		//Parametros / Qualidade
		IncProc("Modulo 5 - Criando Parametros... 19 de 25..")
		AtParIPP()  // Atualiza Parametros da Inspeaao de Processos
		AtParIPE()  // Atualiza Parametros da Inspeaao de Entradas
	EndIF
	
	If  cbQual4 == "Sim"
		//Layouts e Templates / Qualidade
		IncProc("Modulo 5 - Aplicando Carga de Dados... 20 de 25..")
		Sleep(1000)
	EndIF


	//OPCOES DO RECURSOS HUMANOS
	If cbRh1 == "Sim"
		//Campos e Abas / Recursos Humanos
		IncProc("Modulo 6 - GPE")
		//Sleep(1000)
		U_xIniFol()  //FABRICIO AMARO - 29/04/2024
	EndIF
	
	/*
	If cbRh2 == "Sim"
		//indices e Gatilhos / Recursos Humanos
		IncProc("Modulo 6 - Criando indices / Gatilhos... 22 de 25..")
	EndIF
	
	If cbRh3 == "Sim"
		//Parametros / Recursos Humanos
		IncProc("Modulo 6 - Criando Parametros... 23 de 25..")
		Sleep(1000)
	EndIF
	
	If cbRh4 == "Sim"
		//Layouts e Templates / Recursos Humanos
		IncProc("Modulo 6 - Aplicando Carga de Dados... 24 de 25..")
		Sleep(1000)
	EndIF
	*/

	
	//OPCOES GERAIS
	If cbGer1 != "0"
		//Campos e Abas / Recursos Humanos
		IncProc("Modulo 7 - Atualizando Decimais (VLR e QTD)... 25 de 25..")
		AtuDecim()


	//******************************************************************************************************************
	// Compatibiliza Campos do Dicionario com o Banco de Dados -  Igor Mateus 11/2021
	//******************************************************************************************************************
		u_UpdTabl2()

	EndIF

    //MsgInfo("Setup inicial executado com sucesso!")
	
	MsgBox("Setup inicial executado com sucesso!","Setup OK","INFO")
	oDlg:end()

Return

//******************************************************************************************************************
// Cria e Atualiza Campos de acordo com os dados apontados nos vetores -  Igor Mateus 11/2021
//******************************************************************************************************************


Static Function AtuCampos()

   	aCampos := 	{}

	Aadd (aCampos, {'SC5',           'C5_X_EST',       2,            0,           'C',        '@!',              '*UF',           '*UF',   			'V',        'V',        '',/*'U_BROWSC5(SA1->A1_EST,SA2->A2_EST)'*/			,/*'U_C5NOMCLI(SA1->A1_EST,SA2->A2_EST)'*/,                                              '',          ''})
    Aadd (aCampos, {'SC5',           'C5_X_MUNE',     30,            0,           'C',        '@!',              '*Municipio',    '*Municipio',  	'V',        'V',        '',/*'U_BROWSC5(SA1->A1_MUN,SA2->A2_MUN)'*/			,/*'U_C5NOMCLI(SA1->A1_MUN,SA2->A2_MUN)'*/,                                              '',          ''})
    Aadd (aCampos, {'SC5',           'C5_X_BAIRR',    20,            0,           'C',        '@!',              '*Bairro',       '*Bairro',   		'V',        'V',        '',/*'U_BROWSC5(SA1->A1_BAIRRO,SA2->A2_BAIRRO)'*/	,/*'U_C5NOMCLI(SA1->A1_BAIRRO,SA2->A2_BAIRRO)'*/,                                        '',          ''})
    Aadd (aCampos, {'SC5',           'C5_X_END',      30,            0,           'C',        '@!',              '*Endereco',     '*Endereco',   	'V',        'V',        '',/*'U_BROWSC5(SA1->A1_END,SA2->A2_END)'*/			,/*'U_C5NOMCLI(SA1->A1_END,SA2->A2_END)'*/,                                              '',          ''})
    Aadd (aCampos, {'SC5',           'C5_X_NOMCL',    50,            0,           'C',        '@!',              '*Nome',         '*Nome',   		'V',        'R',        '',''											,      '',                                                                                 '',          ''})
    Aadd (aCampos, {'SF2',           'F2_X_NOMCL',    40,            0,           'C',        '@!',              '*Nome Cliente', '*Nome Cliente',  'A',        'R',        '','U_F2NOMCLI()'								,      '',                                                                     '',          ''})
    Aadd (aCampos, {'SF1',           'F1_X_FORNN',    40,            0,           'C',        '@!',              '*Nome Fornece', '*Nome Fornece',  'V',        'V',        '',''											,'U_CpoCliFor("A2_NOME","A1_NOME",SF1->(F1_FORNECE+F1_LOJA),SF1->F1_TIPO,"E")',      '',          ''})
    Aadd (aCampos, {'SD1',           'D1_X_DESC',     40,            0,           'C',        '@!',              '*Descricao',    '*Descricao',   	'V',        'V',        '',''											,'U_RETSB1()',                                                                                 '',          ''})
    Aadd (aCampos, {'SC7',           'C7_X_NOME',     40,            0,           'C',        '@!',              '*Nome Fornece', '*Nome Fornece',  'V',        'V',        '',''											,'POSICIONE("SA2",1,XFILIAL("SA2")+SC7->C7_FORNECE+SC7->C7_LOJA,"A2_NOME")',         '',          ''})
    Aadd (aCampos, {'SE1',           'E1_X_NUMBC',     6,            0,           'C',        '@!',              '*Num BCO Bkp',  '*Num BCO Bkp',   'A',        'R',        '',''											,      '',                                                                                 '',          ''})
    Aadd (aCampos, {'SC5',           'C5_X_FPGT',      3,            0,           'C',        '@!',              '*Forma Pagto',  '*Forma Pagto',   'A',        'R',        '',''											,      '',                                                                                 '3',         'BOL=BOLETO;TRF=TRANSFERENCIA;CCR=CARTAOCREDITO;CDB=CARTAODEBITO;DEP=DEPOSITO'})
    Aadd (aCampos, {'SE1',           'E1_X_FPGT',      3,            0,           'C',        '@!',              '*Forma Pagto',  '*Forma Pagto',   'A',        'R',        '',''											,      '',                                                                                 '3',         'BOL=BOLETO;TRF=TRANSFERENCIA;CCR=CARTAOCREDITO;CDB=CARTAODEBITO;DEP=DEPOSITO'})
    Aadd (aCampos, {'SA1',           'A1_X_FPGT',      3,            0,           'C',        '@!',              '*Forma Pagto',  '*Forma Pagto',   'A',        'R',        '',''											,      '',                                                                                 '4',         'BOL=BOLETO;TRF=TRANSFERENCIA;CCR=CARTAOCREDITO;CDB=CARTAODEBITO;DEP=DEPOSITO'})
	Aadd (aCampos, {'SCJ',           'CJ_X_FPGT',      3,            0,           'C',        '@!',              '*Forma Pagto',  '*Forma Pagto',   'A',        'R',        '',''											,      '',                                                                                 '',         'BOL=BOLETO;TRF=TRANSFERENCIA;CCR=CARTAOCREDITO;CDB=CARTAODEBITO;DEP=DEPOSITO'})
	Aadd (aCampos, {'SF4',           'F4_X_OPER',      2,            0,           'C',        '@!',              '*Tip. Operacao','*Tip. Operacao', 'A',        'R',        '',''											,      '',                                                                                 '',          ''})
	Aadd (aCampos, {'SF4',           'F4_X_DESOP',    40,            0,           'C',        '@!',              '*Desc Operacao', '*Desc Operacao','V',        'R',        '',''											,      '',             	                                                                 '',          ''})
	Aadd (aCampos, {'SCK',           'CK_X_DSCPD',    80,            0,           'C',        '@!',              '*Descricao', '*Descricao','V',        'R',        '',''											,      '',             	                                                                 '',          ''})
    
	CriaCampos()
	
Return


Static Function CriaCampos()
	Local  _cAlias2 := ""
	Local IntCont := 1	
	    
	// Campos a serem atualizados antes da criacao 

	//APRESENTAR NO BROWSE
	cTab    := "SX3"+cEmpresa+""
	cCampos := "'D5_LOTECTL','C1_OP','C7_DESCRI','C1_DESCRI','CJ_NOMCLI'"
	TcSqlExec(" UPDATE " + cTab + " SET X3_BROWSE = 'S' WHERE X3_CAMPO IN (" + cCampos + ") ")

	//TORNAR CAMPOS OBRIGATORIOS
	cCampos := "'B1_POSIPI','B1_ORIGEM','B1_GRUPO','A2_BAIRRO','A1_BAIRRO','A2_CEP','A1_CEP','A2_COD_MUN','A1_COD_MUN','A1_PAIS','A2_NATUREZ','A1_NATUREZ','A1_COND', 'A2_COND','A1_PESSOA','A2_TIPO','F4_PISCOF','F4_PISCRED','F4_CSTPIS','F4_CSTCOF','F4_SITTRIB','ED_TIPO','C5_NATUREZ','F4_CTIPI'"
	TcSqlExec(" UPDATE " + cTab + " SET X3_OBRIGAT = 'x       ' WHERE X3_CAMPO IN (" + cCampos + ") ")

	//ATUALIZA ORDEM
	TcSqlExec(" UPDATE " + cTab + " SET X3_ORDEM = '13' WHERE X3_CAMPO = 'D1_TES' ")
	TcSqlExec(" UPDATE " + cTab + " SET X3_ORDEM = '04' WHERE X3_CAMPO = 'D3_DESCRI' ")
	TcSqlExec(" UPDATE " + cTab + " SET X3_ORDEM = '48' WHERE X3_CAMPO = 'B1_CLVL' ")

    For IntCont := 1 to Len(aCampos)

		_cAlias2 := Alltrim(aCampos[IntCont][1])
		cCMP     := Alltrim(aCampos[IntCont][2])

		cQuery := " SELECT * FROM " + RetSqlName("SX3")
		cQuery += " WHERE D_E_L_E_T_ = ' ' "
		cQuery += " AND X3_ARQUIVO = '"+_cAlias2+"' "
		cQuery += " AND X3_CAMPO = '"+cCMP+"'  "

		TcQuery cQuery New Alias "QSX3"

		If QSX3->(Eof())

            reclock('SX3',.T.)
				X3_ARQUIVO  :=  aCampos[IntCont][1]
				X3_ORDEM    :=  "ZZ"
				X3_CAMPO    :=  aCampos[IntCont][2]
				X3_TAMANHO  :=  aCampos[IntCont][3]
				X3_DECIMAL  :=  aCampos[IntCont][4]
				X3_TIPO     :=  aCampos[IntCont][5]
				X3_PICTURE  :=  aCampos[IntCont][6]
				X3_TITULO   :=  aCampos[IntCont][7]
				X3_DESCRIC  :=	aCampos[IntCont][8]
				X3_VISUAL   :=  aCampos[IntCont][9]
				X3_CONTEXT  :=  aCampos[IntCont][10]
				X3_RELACAO  :=  aCampos[IntCont][13]
				X3_INIBRW   :=  aCampos[IntCont][12]
				X3_USADO    :=  cUsa
				X3_RESERV   :=  cRes
				X3_PYME     :=  "S"
				X3_PROPRI   :=  "U"
				X3_BROWSE   :=  "S"
				X3_NIVEL    :=  1
				X3_FOLDER   := aCampos[IntCont][14]
            msunlock()
		
		Endif

        QSX3->(dbCloseArea())
		
    Next
	u_updtable('SA1')	
	u_updtable('SF4')	
Return 

//******************************************************************************************
// Vetores com dados de Atualizacao dos Parametros conforme modulo - Igor Maeus 12/11/2021
//******************************************************************************************

// Atualiza parametros do Modulo Backoffice
// Cria paramero para envio de CSV

Static Function AtuParam()
Local x := 1

    aadd(aSX6,{"MV_ALTPDOC", ".T."}) //Indica se a aba Documentos no Grupo de Aprovaaao a  exibida (T) ou nao (F)                           
    aadd(aSX6,{"MV_SELFOR",  "S"}) //Parametro que indica se o Comprador podera fazer selecao dos Fornecedores                          
    aadd(aSX6,{"MV_NFEMSA1", "C"}) //Parametro que determina o tratamento do campo A1_MENSAGE para NF-e. C=Inf. Adicionais cliente, F=Inf
    aadd(aSX6,{"MV_NFEMSF4", "F"}) //Parametro que determina o tratamento do campo F4_FORMULA para NF-e. C=Inf. Adicionais cliente, F=Inf
    aadd(aSX6,{"MV_PESOCAR", "B"}) //Indica o tipo do peso a ser usado na montagem de gargas e relatorios de cargas Liquido/Bruto.        
    aadd(aSX6,{"MV_NFEAFSD", "111"}) //Agrega Frete(F), Seguro(S), Despesa(D) NFE Dev/Ret Exemplo 000 = Nao Agrega, 100 = Agrega F          
    aadd(aSX6,{"MV_ALTPED",  "N"}) //Pode alterar Pedido de Venda ja faturado ?        
    aadd(aSX6,{"MV_1DUPNAT", "SC5->C5_NATUREZ"}) //Campo ou dado a ser gravado na natureza do titulo.
    aadd(aSX6,{"MV_1DUP",    "1"}) //Define inicializacao da 1.parcela do titulo gerado
    aadd(aSX6,{"MV_IMPSX1",  "N"}) //Imprime perguntas no cabecalho do relatario       
    aadd(aSX6,{"MV_GRSEFLQ", ".T."}) //Indica se deseja gravar cheque no cadastro de cheques (SEF) quando gerada uma liquidaaao de titulos do tipo CH                                        
    aadd(aSX6,{"MV_CUSTEXC", "N"}) //Determina o modo de recalculo do custo medio, se  exclusivo (S), so uma estacao utilizando o sistema ou compartilhado (N).                             
    aadd(aSX6,{"MV_HORANFE", ".T."}) //Utilizado para gravar o campo F1_HORA no momento  da inclusao do documento de entrada.              
    aadd(aSX6,{"MV_ATUSA1",  ".T."}) //Atualiza dados histaricos do cliente na exclusao  do documento de saada.                             
    aadd(aSX6,{"MV_TPCOMIS",  "O"}) //Flag de sistema utilizado para informar se o calcuo da comissao sera On-Line (O) ou em Batch (B).   
    aadd(aSX6,{"MV_TPCOMLJ",  "B"}) //Tem a mesma finalidade/caracteristicas do MV_TPCOMIS, entretanto somente a utilizado no SIGALOJA.   
    aadd(aSX6,{"MV_TRFBCO",   "1"}) //Possibilita a transferencia de titulos sem bordera entre bancos sem passar pela carteira. O conteudo do parametro pode ser "1" - "Sim" e "2" - "Nao".  
    aadd(aSX6,{"MV_SLDBXCR",  "C"}) //Indica como sera controlado o saldo da baixa do   contas a receber, somente quando o cheque for  compensado, ou no momento da baixa (B,C)                                              
    aadd(aSX6,{"MV_CTBAPLA",  "4"}) //Indica se o SigaCTB ira limpar os flags de contabilizaaao (_LA/_DTLANC) ao excluir lanaamentos.     1=Nao;2=Perguntar;3=Sim c/alertas;4=Sim s/ alertas
    aadd(aSX6,{"MV_CTBCUBE",  "0"}) //Utiliza cubo de entidades contabeis?              Definicao 0 = Nao, 1 = Sim                        
    aadd(aSX6,{"MV_RECEINF",  "100099"}) //Informe cadigo da receita por Operaaao.           
    aadd(aSX6,{"MV_VCHVNFE",  ".T."}) //Verifica se a Chave NFE confere com a nota de     entrada que esta sendo digitada.                  
    aadd(aSX6,{"MV_DCHVNFE",  ".T."}) //Informa se devera digitar a chave da Nfe/Cte      caso a espacie seja SPED/CTE e Form.Praprio=Nao   .T. = Sim; .F. = Nao.                             
    aadd(aSX6,{"MV_CHVNFE",   ".T."}) //Informa se havera consulta da                     NFE/CTE no portal da SEFAZ                        .T. = Sim; .F. = Nao.                             
    aadd(aSX6,{"MV_NFENAT",   ".T."}) //Indica se a natureza tem seu preenchimento obriga-torio durante a inclusao de um documento de entra-da.                                               
    aadd(aSX6,{"MV_APURANT",  ".T."}) //Indica se o nome do arquivo de apuracao do perio  do anterior devera ser informado automaticamente  
    aadd(aSX6,{"MV_DIDANFE",  ".T."}) //Define se os dados da DI-Declaracao de Importacao serao impressos nas inf.compl do danfe            .T. = Sim; .F. = Nao.                             
	aadd(aSX6,{"MV_NDESCTP",  ".T."}) //Considera ou nao a diferenaa entre o preao lista e o preao unitario com desconto                    .T. = Nao considera; .F. = Considera
	aadd(aSX6,{"MV_CADPROD",  ""})	 //Desabilita checagem padrao em multiplas tabelas na inclusao do produto, ganhando tempo.
	aadd(aSX6,{"MV_MSGTRIB",  "3"})
	aadd(aSX6,{"MV_SELFOR",  "S"})
	aadd(aSX6,{"MV_MCUSTO",  "1"})
	aadd(aSX6,{"MV_NATSINT",  "1"})
	aadd(aSX6,{"MV_APICCGC",  ".T."}) // Habilita Consulta CNPJ no Cadastro de Cliente via Carol (A partir vers 12.1.33) - Consulta na Receita
	aadd(aSX6,{"MV_APIFOR",  ".T."}) // Habilita Consulta CNPJ no Cadastro do FOrnecedor via Carol (A partir vers 12.1.33) - Consulta na Receita


	For x := 1 to Len(aSX6)
		TCSQLEXEC(" UPDATE SX6"+cEmpresa+" SET X6_CONTEUD = '" + aSX6[x][2] + "' WHERE X6_VAR = '" +  aSX6[x][1] + "' ") 
	Next
    
/*	
    //  Criando novos parametros (caso seja necessario, sa descomentar rotina e alimentar vetor) @IgorMateus 12/11/2021

    aadd(aParam,{'',              'MV_X_CSV',     'C',            'Envio CSV cotaaao',            'c:\totvs\envio cotacao\',           'U'})
    
    CriaParam()
*/


Return


/*

Static Function CriaParam()
	Local IntCont := 1	

	
    
    For IntCont := 1 to Len(aParam)


		strSQL:="SELECT * FROM SX6"+cEmpresa+" WHERE X6_VAR = '"+aParam[IntCont][1]+"' AND D_E_L_E_T_ <> '*' "

		If Select("QRY") > 0 
		QRY->(dbCloseArea())
		Endif 

		TcQuery strSQL NEW Alias "QRY"


		IF QRY->X6_VAR != aParam[IntCont][1]

            reclock('SX6',.T.)
            X6_FIL      :=  aParam[IntCont][1]   
            X6_VAR      :=  aParam[IntCont][2]
            X6_TIPO     :=  aParam[IntCont][3]
            X6_DESCRIC  :=  aParam[IntCont][4]
            X6_CONTEUD  :=  aParam[IntCont][5] 
            X6_PROPRI   :=  aParam[IntCont][6]
            msunlock()

		END IF

    Next
s
Return
*/

//******************************************************************************************
// Vetores com dados de Atualizacao dos Parametros conforme modulo - Igor Maeus 12/11/2021
//******************************************************************************************

Static Function AtParPCP()
	Local x := 1

	// Para a integraaao dos modulos SIGAPCP (Planejamento e Controle de Produaao), SIGAQIE (Inspeaao de Entradas), SIGAQIP (Inspeaao de Processos), configuraCOES listadas abaixo:
	aadd(aSX6,{"MV_A685OPE","N"}) 	// Indica se a rotina Apontamento de Perda (MATA685), deve ou nao aceitar ordens de produaao encerradas.
	aadd(aSX6,{"MV_APRESTR",".F."}) // Indica se o o cadastro de pra-estruturas sera submetido ao controle de alaadas.	.F.
	aadd(aSX6,{"MV_BLQOPAP","N"}) 	// Indica se havera ou nao, um tratamento para concorrancia no Apontamento de Produaao
	aadd(aSX6,{"MV_BXPROP","S"})	// Indica se proporciona baixa automatica de mataria-prima.
	aadd(aSX6,{"MV_EMPBN","F"})		// Indica se os empenhos de Ordens de Produaao, para componentes do tipo "BN" (Beneficiamento), devem ser gerados.
	aadd(aSX6,{"MV_ESTNEG","N"})	// Indica se o sistema permitira que os saldos em estoque dos produtos fiquem negativos atravas de movimentaCOES
	aadd(aSX6,{"MV_GANHOPR",".T."}) // Indica se utiliza o conceito de ganho de produaao ao efetuar o apontamento da produaao (rotina "Produaao" - MATA250
	aadd(aSX6,{"MV_GERAOPI","T"})   // Gera OP do produto intermediario a partir de OP do produto principal. 
	aadd(aSX6,{"MV_LOTEUNI","T"})	// Utiliza o conceito de lote anico para produtos com rastreabilidade por lote
	aadd(aSX6,{"MV_OPIPROC",".T."}) // Indica se o saldo de OPs intermediarias de produtos semi-acabados, geradas automaticamente, deve ser destinado ao local de processo.				
	aadd(aSX6,{"MV_PCPATOR",".T."}) // Indica que as operaCOES serao geradas individualmente na Ordem Produaao.	.T.   
	aadd(aSX6,{"MV_PERCPRM","100"}) // Percentual maximo da quantidade prevista na ordem de produaao.   
	aadd(aSX6,{"MV_PERDINF",".F."}) // Identifica qual conceito deve ser aplicado ao processo de apontamento de produaao, quando for informada quantidade de perda.
	aadd(aSX6,{"MV_PRZESTR",".T."}) // Permite definir se sera ou nao apresentada a mensagem para a confirmaaao da geraaao de OPs intermediarias e/ou solicitaCOES de compras com datas previstas de entrega anteriores a data atual (data-base) na rotina aOrdem de Produaaoa (MATA650), otimizando, assim, o processo de inclusao das ordens de produaao.
	aadd(aSX6,{"MV_QUEBRSC","S"})   // Define se as SCs serao quebradas de acordo com o lote econamico ou se sera gerada apenas uma solicitaaao com quantidade total. 
	aadd(aSX6,{"MV_QUEBROP","S"})   // Define se as OPs serao quebradas de acordo com o lote econamico ou se sera gerada apenas uma ordem de produto com quantidade total. 
	aadd(aSX6,{"MV_RASTRO","S"})    // Define se as OPs serao quebradas de acordo com o lote econamico ou se sera gerada apenas uma ordem de produto com quantidade total.
	aadd(aSX6,{"MV_REQAUT","A"})    // Verifica se o sistema deve ou nao gerar requisiCOES automaticas baseadas no empenho da produaao. A (Gera Automatico) ou D (Digita RequisiCOES).
	aadd(aSX6,{"MV_TPHR","C"})   	// Define a unidade padrao para tempos utilizados pelo sistema de PCP e Estoque. O tipo de hora pode ser ""N"" (Normal) ou ""C"" (Centesimal).
	aadd(aSX6,{"MV_TMPAD","010"})   // Tipo de Movimentaaao Padrao para produaao PCP.

	For x := 1 to Len(aSX6)
		TCSQLEXEC(" UPDATE SX6"+cEmpresa+" SET X6_CONTEUD = '" + aSX6[x][2] + "' WHERE X6_VAR = '" +  aSX6[x][1] + "' ") 
	Next
    
Return


//******************************************************************************************
// Vetores com dados de Atualizacao dos Parametros conforme modulo - Igor Maeus 12/11/2021
//******************************************************************************************

Static Function AtParIPE()
	Local x := 1

	// Configuraaao dos parametros Modulo SIGAQIE a Inspeaao de Entradas ************************
	aadd(aSX6,{"MV_CQ","98"})        		// Local (Armazam) do Controle de Qualidade.	98			
	aadd(aSX6,{"MV_DIRCQ","\RELATO\CONTROLE\"})    // Diretario destino onde serao gravados os certificados de Qualidade dos produtos que passaram no C.Q. - Controle de Qualidade, pelo processo de Skip-lote.	
	aadd(aSX6,{"MV_PRFOFAB","F"})    		// Seleciona indices de qualidade Legado.	F
	aadd(aSX6,{"MV_QACUPAM","F"})    		// Indica se os planos de amostragem NBR5426 serao aglutinados, quando possuarem as mesmas caracterasticas como: Navel, Tipo e NQA.	F  
	aadd(aSX6,{"MV_QAMOINI","F"})    		// Indica se os ensaios definidos como amostras iniciais serao inspecionados apenas na primeira ocorrancia de Entradas com Produto X Fornecedor.	F
	aadd(aSX6,{"MV_QAPCTOL","S"})    		// Sugere laudo com aprovaaao condicional se houver mediCOES fora dos limites de engenharia, poram dentro da tolerancia determinada.	S
	aadd(aSX6,{"MV_QASSIQS","N"}) 			// Situaaao de Produto X Fornecedor pode ter categoria skip-lote total se o fornecedor nao tiver nota de IQS.   
	aadd(aSX6,{"MV_QATULGE","T"})    		// Indica se o Laudo Geral da entrada sera realizado a partir das Baixas do CQ.
	aadd(aSX6,{"MV_QATUPAM","S"})    		// Atualizacao automatica do Plano de Amostragem.	
	aadd(aSX6,{"MV_QBLQSLA","T"})    		// Determina se a pasta de Laudo Geral sera bloqueada caso existam laboratarios sem laudo definido. As opCOES para este parametro sao: "T=True", para bloquear a pasta e "F=False" que permite o acesso mesmo faltando laudos.
	aadd(aSX6,{"MV_QBXANNC","F"})    		// Indica se as notificaCOES de nao-conformidade serao baixadas automaticamente na geraaao, quando utilizada integraaao com o Ambiente Controle de Nao-conformidades.
	aadd(aSX6,{"MV_QCERENT","N"})    		// Certifica entregas mesmo com ensaios a inspecionar pela avaliaaao do skip-teste.
	aadd(aSX6,{"MV_QCONDIA","S"})    		// Consiste no namero de dias em atraso no Cadastro de Entradas.
	aadd(aSX6,{"MV_QDIAIPO","180"})    		// Namero maximo de dias em atraso para o calculo do IPO.	
	aadd(aSX6,{"MV_QDIAMIQ","31"})    		// Dia de fechamento no mas para a geraaao do andice de Qualidade
	aadd(aSX6,{"MV_QDIREIN","180"})    		// Namero de dias retroativos em relaaao a data da entrada, p/calculo da reincidancia das nao-conformidades, no cadastro de Resultados.
	aadd(aSX6,{"MV_QDIRGRA","C:\TEMP"})  	// Diretario onde serao gerados os arquivos (extensao SPC), p/graficos Cartas de Controle.  
	aadd(aSX6,{"MV_QEINSOB","N"})    		// Define se a obrigataria a especificaaao do instrumento para a gravaaao dos resultados.
	aadd(aSX6,{"MV_QEJUSLA","Laudo aprovado em funcao do skip-lote"})  // Justificativa para o laudo de entradas certificadas pela avaliaaao do skip-lote.
	aadd(aSX6,{"MV_QELAUEN","S"})    		// Define se o laudo final da entrada sera digitado pelo usuario.
	aadd(aSX6,{"MV_QENTQEK","2"})			// Considera os registros da QEK no calculo de geraaao de Ind. Qualidade
	aadd(aSX6,{"MV_QFILENT","T"})    		// Indica se o filtro das entradas inspecionadas e/ou a inspecionar sera realizado antes da exibiaao do browse.
	aadd(aSX6,{"MV_QFRQSKP","T"})    		// Indica se a freqaancia de skip-lote por documento de entrada pode ser alterado pelo usuario.
	aadd(aSX6,{"MV_QLAUSQ","T"})    		// Define se o laudo final da permissao de uso sera dado somente pelo usuario da Qualidade.
	aadd(aSX6,{"MV_QEALTOB","2"})    		// Verifica se permite alteraaao da observaaao da entrada (1 = sim e 2 = nao).
	aadd(aSX6,{"MV_QENSMED","T"})    		// Indica se os ensaios que nao possuem mediCOES informadas serao exibidos nos Laudos de Laboratarios e Geral.
	aadd(aSX6,{"MV_QENTPRO","S"})    		// Excluir entradas ao excluir produto.
	aadd(aSX6,{"MV_QERENNC","30"})    		// Namero de dias para retorno da resposta da NNC pelo fornecedor.
	aadd(aSX6,{"MV_QERESNC","S"})    		// Considera resultado fora de especificaaao como uma nao-conformidade.
	aadd(aSX6,{"MV_QFATQIP","S"})    		// Indica a verificaaao da existancia dos fatores de IQP padraes, sendo A, B, C, D e E.
	aadd(aSX6,{"MV_QIEDOC","\SYSTEM\DOCS  "}) //Diretario que contera os documentos anexos relativo a tela de Resultados do Ambiente Inspeaao de Entradas.  
	aadd(aSX6,{"MV_QIMPNNC","T"})    		// Pergunta se imprime a NNC, a partir do cadastro de Resultados
	aadd(aSX6,{"MV_QINTQMT","N"})    		// Define se ha integraaao entre os ambientes Inspeaao de Entradas e Metrologia.
	aadd(aSX6,{"MV_QINTQNC","N"})    		// Define se as nao-conformidades serao enviadas para tratamento no Ambiente Controle de Nao-conformidades
	aadd(aSX6,{"MV_QIQSQUA","80.00 "})    	// IQS manimo do fornecedor para situaaao com categoria skip-lote 25%.
	aadd(aSX6,{"MV_QIQSASS","80.00 "})    	// Indica a nota manima de IQS para o fornecedor assegurado para que nao sofra demarito.
	aadd(aSX6,{"MV_QIQSREP","70.00 "})    	// Nota de IQS de reprovaaao. O IQS do fornecedor deve ser superior a esta nota para haver a atualizacao automatica do Plano de Amostragem (MV_QATUPAM).	70.00                                                                                                                                                                                                                                                     
	aadd(aSX6,{"MV_QLIBDEV","F"})    		// Determina a liberaaao de devoluaao de um material, cuja nota fiscal de origem ainda possui saldo no armazam da qualidade "CQ". As opCOES sao: "T=True" libera a devoluaao mesmo contendo saldo no CQ e "F=False" nao libera se ainda houver saldo no CQ.
	aadd(aSX6,{"MV_QLINS","F"})    			// Define o laudo do instrumento utilizado nas InspeCOES de Entrada e Processos.
	aadd(aSX6,{"MV_QLOTSEQ","N"})    		// Utiliza o namero do lote sequencial no cadastro de entradas.
	aadd(aSX6,{"MV_QLOTUNI","N"})    		// Utiliza o namero do lote anico no cadastro de entradas.
	aadd(aSX6,{"MV_QMEDLAU","N"})    		// Permite incluir/alterar/excluir mediCOES se houver laudo cadastrado.
	aadd(aSX6,{"MV_QMESACU","8"})    		// Namero de meses para o calculo dos indices de qualidade acumulados.
	aadd(aSX6,{"MV_QNAOCER","F"})    		// Indica se as entradas certificadas em regime de Skip-lote nao serao consideradas no calculo de IQF Mensal.
	aadd(aSX6,{"MV_QNAUSR","9"})    		// Navel de acesso necessario para registrar o follow-up de permissao de uso.
	aadd(aSX6,{"MV_QNBR542","N"})    		// Aplicaaao da NBR-5426 no skip-lote: somente aplica verificaaao do skip-lote apos 5 entradas consecutivas com Plano de Amostragem atenuado (Simples/Dupla).
	aadd(aSX6,{"MV_QPAMIQS","S"})    		// Atualizacao do Plano de Amostragem realizada a partir do cadastro de entradas, vinculadas ao IQS.
	aadd(aSX6,{"MV_QPATHWT","GETTEMPPATH()  "}) // Define o diretario do Client onde estarao os documentos gerados pelo sistema.    
	aadd(aSX6,{"MV_QPESQE6","T"})    		// Define se a pesquisa do produto sera efetuada atravas da tabela de especificaCOES do produto (QE6). As opCOES para este parametro sao: "T=True", a pesquisa a efetuada atravas desta tabela, e "F=False", a pesquisa a realizada no "Cadastro de Produtos" padrao do sistema (SB1).
	aadd(aSX6,{"MV_QPOSFOC","6"}) 			// Determina o namero de posiCOES a direita que o foco deve se movimentar, na inclusao das mediCOES para o foco no campo de mediaao.   
	aadd(aSX6,{"MV_QPVLIN","N"})			// Indica a validaaao do instrumento no momento da mediaao.	  
	aadd(aSX6,{"MV_QQTDREJ","S"})    		// Acumula a quantidade rejeitada no Laudo de Reprovaaao e a diferenaa entre o tamanho do lote e a quantidade rejeitada no Laudo de Entrega.
	aadd(aSX6,{"MV_QREINSP","F"}) 			// Indica se habilita a reinpeaao de produtos na mesma data.
	aadd(aSX6,{"MV_QSKLENT","3"})    		// Namero de entradas iniciais para aplicaaao do skip-lote
	aadd(aSX6,{"MV_QSKLIQS","N"})    		// Manter o skip-lote vinculado ao IQS, na atualizacao automatica do skip-lote.
	aadd(aSX6,{"MV_QSKPTES","2"})    		// Indica se ira calcular o skip-teste para todas as entradas (="1") ou para as entradas inspecionadas somente (="2").
	aadd(aSX6,{"MV_QSOLPQU","S"})    		// Exige o nome do solicitante quando a situaaao do Produto/Fornecedor tiver categoria "Sem skip-lote".
	aadd(aSX6,{"MV_QSUGLAU","S"})    		// Indica se os Laudos referentes a Entrada e Laboratario serao sugeridos pelo sistema quando alterados manualmente.
	aadd(aSX6,{"MV_QTRFREP","N"})    		// Indica se as transferancias enviadas para o CQ serao consideradas como demarito no calculo dos indices, quando reprovadas.
	aadd(aSX6,{"MV_QUTSK25","S"})    		// Indica se o tratamento do Skip-lote com 25% sera aplicado na atualizacao dos indices do fornecedor.
	aadd(aSX6,{"MV_QTXTIMP","IMPORT.IMP "}) // Informe neste parametro o nome do arquivo que armazena a configuraaao do leiaute TXT padrao a ser utilizado nas rotinas de importaaao. O nome "IMPORT.IMP" a o conteado padrao deste parametro e tambam o nome sugerido para gravaaao do arquivo em manutenaao de "Administraaao TXT de Importaaao".    
	aadd(aSX6,{"MV_QVALLOT","3"})    		// Define a gravaaao da data de validade do lote na integraaao dos ambientes aComprasa e/ou aEstoquea com o ambiente aInspeaao de Entradasa, sendo: "Conteado = 1" o sistema ira considerar a data de validade informada no ambiente "Estoque", alterando a data de validade informada no ambiente Inspeaao de entradas; "Conteado = 2" o sistema ira considerar a data de validade informada no ambiente "Inspeaao de Entradas" alterando a data de validade do  ambiente "Estoquea; "Conteado = 3"  o sistema assumira a maior validade informada para o lote.
	aadd(aSX6,{"MV_RASTRO","S"})    		// Determina a utilizacao da  Rastreabilidade dos Lotes de Produaao.
	aadd(aSX6,{"MV_RELT","\SPOOL\ "})    	// Diretario para gravaaao dos relatarios, quando impressao em disco.
	aadd(aSX6,{"MV_RODAPE",""})    			// Tatulo utilizado nos rodapas dos relatarios.
	aadd(aSX6,{"MV_SABFERI","S"})    		// Considerar sabados como feriado, quando da utilizaaao da DataValida().
	aadd(aSX6,{"MV_SALTPAG","S"})    		// Indica se o salto de pagina devera ser dado antes ou depois da impressao do relatario.
	aadd(aSX6,{"MV_TTS","S"})    			// Define se o controle de transaaao sera habilitado no banco de dados.

	For x := 1 to Len(aSX6)
		TCSQLEXEC(" UPDATE SX6"+cEmpresa+" SET X6_CONTEUD = '" + aSX6[x][2] + "' WHERE X6_VAR = '" +  aSX6[x][1] + "' ") 
	Next
 
Return


//******************************************************************************************
// Vetores com dados de Atualizacao dos Parametros conforme modulo - Igor Maeus 12/11/2021
//******************************************************************************************


Static Function AtParIPP() 
	Local x := 1

	//Configuraaao dos parametros Modulo SIGAQIP a Inspeaao de Processos *********************************************	
	aadd(aSX6,{"MV_INTDPR","0"})			// Define se existe integraaao entre o Modulo SIGADPR e outros modulos
	aadd(aSX6,{"MV_PRAPONT","1"})			// Configura o critario de prioridade para envio de produaao para o Controle de Qualidade no Apontamento de Produaao.
	aadd(aSX6,{"MV_PRODAUT","F"})    		// Gera lanaamentos automaticos de produaao das Ordens de Produaao vinculadas a O.P. Pai (produto acabado), sendo T -> Gera ou F -> Nao Gera.
	aadd(aSX6,{"MV_PRECISA","4"})    		// Define o fator de divisao para mediaao em tempo, referente a peraodos manimos de calendarios, sendo permitidos os valores 1, 2, 4, 6, 12 e 60, que correspondem a 60, 30, 15, 10, 5 e 1 minutos.
	aadd(aSX6,{"MV_QCALLIM","F"})   		// Indica se os limites de controles, inferior e superior, serao fixados na exibiaao do grafico da carta de controle. 
	aadd(aSX6,{"MV_QDIRGRA","C:\TEMP"})     // Diretario em que serao gerados os arquivos (extensao SPC), p/ graficos Cartas de Controle.	C:\TEMP
	aadd(aSX6,{"MV_QGINT","N"})    			// Indica se os modulos do Quality Protheus estao integrados com o modulo Gestao de Pessoal, nao habilitando a opaao "Incluir" funcionarios.
	aadd(aSX6,{"MV_QIMPRCQ","N"})			// Indica se a impressao do relatario "Certificado de Qualidade" sera impresso, sem informar o Laudo Geral da necessidade de Inspeaao
	aadd(aSX6,{"MV_QINOBFM","2"})			// Quando o instrumento nao for obrigatario e o ensaio possuir familias vinculadas
	aadd(aSX6,{"MV_QINVLTX","2"})			// Define se valida a obrigatoriedade do instrumento para ensaios do tipo Texto
	aadd(aSX6,{"MV_QIPAVIS","S"})    		// Utilizado para informar a mensagem de aviso no cadastramento das mediCOES
	aadd(aSX6,{"MV_QIPFROT","T"})    		// Indica se deve utilizar o roteiro de operaCOES primario, da especificaaao do produto, na abertura de Ordens de Produaao.
	aadd(aSX6,{"MV_QIPMAT","S"})   			// Indica se o modulo Inspeaao de Processos esta integrado com Materiais 
	aadd(aSX6,{"MV_QIPMULT","1"})			// Define o namero de processos que serao executados pela rotina, limitado a 15 (quinze) processos. Isto porque quanto maior o namero de processos, maior tambam sera o consumo de processamento.
	aadd(aSX6,{"MV_QIPOPEP","1"})			// Define a prioridade dos dados em relaaao a operaaao entre os modulos de Inspeaao de Processos e Planejamento e Controle de Produaao
	aadd(aSX6,{"MV_QIPQMT","N"})    		// Integraaao entre os modulos de Inspeaao de Processos e Metrologia.
	aadd(aSX6,{"MV_QIPQNC","2"})    		// Integraaao entre os modulos de Inspeaao de Processos e Controle de Nao-conformidades.
	aadd(aSX6,{"MV_QLINS","3"})    			// Define o limite de validaaao do instrumento
	aadd(aSX6,{"MV_QPCAROT","1"})    		// Define se ira efetuar a Carga Otimizada dos dados da OP. Somente leiaute Simplificado.
	aadd(aSX6,{"MV_QPCERPR","N"})    		// Certifica produaao mesmo com ensaios a inspecionar pela avaliaaao do Skip-teste.
	aadd(aSX6,{"MV_QPCPROP","2"})			// Carrega a primeira Operaaao na tela de Resultados ou ira possibilitar ao usuario carregar a Operaaao desejada.
	aadd(aSX6,{"MV_QPDELG2","F"}) 			// Determina se ao excluir as operaCOES do modulo QIP (tabela QQK), tambam devem ser  excluadas automaticamente as operaCOES do modulo PCP (tabela SG2). As opCOES para este parametro sao:
	aadd(aSX6,{"MV_QPIESMD","2"})			// Define se IGNORA ensaios sem mediCOES no Laudo do Laboratario mesmo que estes sejam definidos como Obrigatarios
	aadd(aSX6,{"MV_QPINAUT","N"})    		// Indica a atualizacao automatica a partir do(s) instrumento(s) na primeira mediaao.
	aadd(aSX6,{"MV_QPINDIS","E"})    		// Informa o laudo que indisponibiliza a Ordem de Produaao.
	aadd(aSX6,{"MV_QPINSOB","N"})    		// Define se a obrigataria a especificaaao do instrumento para a gravaaao dos resultados (SIGAQIP)
	aadd(aSX6,{"MV_QPJUSLA","Laudo aprovado em funcao do skip-lote"}) // Justificativa para o laudo das ordens de produaao certificadas pela avaliaaao do Skip-lote.   
	aadd(aSX6,{"MV_QPLDNIV","0"})    		// Define o navel que o usuario deve possuir, para emitir os laudos de laboratario, operaaao e geral, respectivamente. A primeira posiaao refere-se ao Laudo do Laboratario, a segunda ao Laudo da Operaaao e a terceira e altima ao Laudo Geral. Os naveis devem ser definidos com valor entre 0 e 9.
	aadd(aSX6,{"MV_QPLGAPR","S"})    		// Quando nao houver Laudo da Operaaao, o Laudo Geral sera: S (Aprovado), N (Rejeitado) ou I (Igual ao Laudo do Laboratario)
	aadd(aSX6,{"MV_QPLRAST","T"})    		// Indica se podera aceitar somente o laudo Aprovado (T) Total ou Aprovado Parcial (P).
	aadd(aSX6,{"MV_QPMEAUT","N"})    		// Define a utilizaaao do instrumento para coleta de resultados on-line.
	aadd(aSX6,{"MV_QPNRSER","F"})    		// Indica se ha controle sobre a Ordem de Produaao e o namero de sarie do produto.
	aadd(aSX6,{"MV_QPOPINT",".T."})			// Determina se as ordens de produaao geradas para produtos intermediarios por meio da rotina de explosao das OPs serao inspecionadas. 
	aadd(aSX6,{"MV_QPRESNC","N"})    		// Considera resultados fora do especificado como Nao-conformidades.
	aadd(aSX6,{"MV_QPSKLPR","3"})    		// Indica o namero inicial de produCOES iniciais para aplicaaao do Skip-lote
	aadd(aSX6,{"MV_QPSKPTE","2"})    		// Indica se ira considerar o Skip-teste somente para as produCOES ou somente para as produCOES inspecionadas.
	aadd(aSX6,{"MV_QPSLFSO","2"})			// Define de SUGERE Laudo Final da OP mesmo existindo OperaCOES sem Laudo.
	aadd(aSX6,{"MV_QPTRLAY","1"})			// Indica qual Layout da tela de Inspeaao de Resultados sera apresentados ao usuario.
	aadd(aSX6,{"MV_QPVLIN","N"})    		// Indica se ira validar o instrumento no momento na mediaao
	aadd(aSX6,{"MV_QPVMDEL","2"})			// Define se ira VALIDAR o registro deletado para o ensaio obrigatario em mediaao do Laudo
	aadd(aSX6,{"MV_QVLDTHR","1"})			// Permite retroagir as datas de mediCOES. Pode ser configurado com: 1 a o sistema valida se data/hora da mediaao a maior ou igual que a database do sistema, maior ou igual que a data de entrada no laboratario e maior ou igual que a data da Ordem de Produaao. 2 a o sistema consiste se a data de mediaao a maior que a data da Ordem de Produaao. 3 a o sistema verifica se a data da mediaao a maior ou igual a database do sistema e se a maior ou igual que a data da Ordem de Produaao.
	aadd(aSX6,{"MV_QVLENSR","T"})			// Determina se sera validada a manutenaao das mediCOES, em Resultados, do modulo Inspeaao de Processos: .T. (True) - O Sistema nao permitira que um segundo ensaiador altere os dados das mediCOES cadastrados por outro, entretanto sera permitida a alteraaao se o cadigo do ensaiador for substituado por aquele que esta logado. .F. (False) - O Sistema assumira o procedimento padrao, ou seja, permitira que um segundo ensaiador altere os dados das mediCOES cadastrados por outro.
	aadd(aSX6,{"MV_RASTRO","S"})    		// Determina a utilizaaao de rastreabilidade nos lotes de produaao (S - utiliza, ou N - nao utiliza).
	aadd(aSX6,{"MV_TPHR","C"})    			// Define a unidade padrao para tempos utilizados pelos modulos Planejamento e Controle de Produaao, Estoque/Custos e Inspeaao de Processos. O tipo de hora pode ser N (normal) ou C (centesimal).

		
	For x := 1 to Len(aSX6)
		TCSQLEXEC(" UPDATE SX6"+cEmpresa+" SET X6_CONTEUD = '" + aSX6[x][2] + "' WHERE X6_VAR = '" +  aSX6[x][1] + "' ") 
	Next
    
Return


//******************************************************************************************
// Vetores com dados de Atualizacao dos Parametros conforme modulo - Igor Maeus 07/12/2021
//******************************************************************************************


Static Function AtParMNT()
	Local x := 1

	//Configuraaao dos parametros Modulo SIGA MNT a Manutenaao de Ativos - Gestao de Frotas  *********************************************	
	aadd(aSX6,{"MV_NGCORAB","N"})   //Indica se o sistema deve verificar e alertar se tem corretiva aberta para o mesmo bem e mesmo serviao, no momento da inclusao de um O.S. corretiva. "
	aadd(aSX6,{"MV_NGCORPR","S"})   //Permitegerar empenho e SCs para O.S corretivas. Deve-se observar os parametros de integraaao que devem estar ligados. "
	aadd(aSX6,{"MV_NGGERPR","S"})   //Indica se o sistema deve gerar automaticamente O.S preventiva, se apos o reparter do contador for identificado que a manutenaao esta vencida. "
	aadd(aSX6,{"MV_NGMNTAT","3"})   //Integraaao Manutenaao com Ativo Imobilizado, a aou aNa nao integra; 1 = AlteraCOES do ATF replicarao no MNT; 2= MNT p/ ATF; 3= MNT p/ATF e ATF p/ MNT."
	aadd(aSX6,{"MV_NGFLUT","N"})    //Indica se a empresa trabalha com turno flutuante 
	aadd(aSX6,{"MV_NGMNTCM","S"})   //Integraaao MNT com Compras COM 
	aadd(aSX6,{"MV_NGMNTES","S"})   //Integraaao MNT com Estoque 
	aadd(aSX6,{"MV_NGMNTPC","S"})   //Integraaao MNT com PCP 
	aadd(aSX6,{"MV_NGMNTQN","N"})   //Integraaao MNT com QNC 
	aadd(aSX6,{"MV_NGMNTRH","N"})   //Integraaao MNT com RH 
	aadd(aSX6,{"MV_NGMNTSC","1"})   //Indica se devem aglutinar os itens da solicitaaao de compras 
	aadd(aSX6,{"MV_NGOSAUT","S"}) 	//Indica se devem gerar OS automaticamente sempre que uma OS por tempo for finalizada 
	aadd(aSX6,{"MV_NGOSPRO","30"})  //Indica se o sistema deve alertar se ja existe uma OS para o mesmobem/serviao/seqaancia com data praximo ( x dias ) a que esta sendo aberta. "
	aadd(aSX6,{"MV_NGPREVE","3"})   //Exibir alerta de manutenaao preventiva atrasada  (1=Abertura de OS Corretiva;2=Abertura de SS; 3=OS Corretiva ou SS;4=Nao exibir).               
	aadd(aSX6,{"MV_NGPRVDI","10"})  //Percentual de variaaao da variaaao dia 
	aadd(aSX6,{"MV_NGREPRO","S"})   //Reprograma O.S preventiva, caso a data fim real da OS finalizada seja maior que inicio previsto da O.S existente, p/ Bem/Serviao/seqaancia "
	aadd(aSX6,{"MV_NGRETOS","0"})   //Determina se ha retorno de O.S automatica de produaao 
	aadd(aSX6,{"MV_NGRHMKP","0"})   //Identifica o percentual de MARKUP a ser aplicado ao salario do funcionario de0,00 a 999,99 % "
	aadd(aSX6,{"MV_MNTOSCO","S"})   //Permite que mais de uma O.S corretiva possa ser aberta para a mesma datasem que a anterior esteja finalizada (sim / nao) "
	aadd(aSX6,{"MV_VARDIA","30"})   //Namero de ocorrancias na variaaao dia 
	aadd(aSX6,{"MV_PRODTER","TERCEIROS"})  //Item de estoque que indica terceiros no SIGAMT 
	aadd(aSX6,{"MV_NGSALHO","N"})   //Define se o campo Salario do funcionario da manutenaao a integrado ao GPE.(S=Sim; N=Nao) Esta no valide do campo (T1_SALARIO) "
	aadd(aSX6,{"MV_NGINTER","N"})   //a especifico para integraaao com ERP Logix da Logocenter. Deve ser criado via configurador caso o cliente utilize esta integraaao. "
	aadd(aSX6,{"MV_CUSMED","M"})    //Utilizado para selecionar o matodo contabilizado de custos dos movimentosde estoque. Sendo: (O-> Online; M -> Mensal)"
	aadd(aSX6,{"MV_NGLOCPA","1"})   //Indica o Almoxarifado padrao para gerar os produtos utilizados peloSIGAMNT (Manutenaao/terceiros)
	aadd(aSX6,{"MV_NGDPST9","2"})   //Indica se podera duplicar cadigo do bem e tag entre filiais e empresas (0=Nao verifica; 1=Por empresa; 2=Por filial; 3= Filial e empresa) "
	aadd(aSX6,{"MV_NGUNIDT","S"})   //Identifica o tipo de unidade da quantidade quando for informado um insumoque utiliza tipo de unidade e hora (D= Decimal; S= Sexagesimal) "
	aadd(aSX6,{"MV_NGMULOS","N"})   //Permite gerar maltiplas O.S a partir do retorno da solicitaaao de serviao. aadd(aSX6,{"MV_NGOSAES","C"})   //? Caso o parametro esteja configurado para aSa, ele verificara as ordens dosabens filhosa da estrutura de bens e gerara a ordem de serviao automaticapara os bens filhos, sem consultar o usuario.? Caso o parametro esteja configurado para aCa, ele consultara o usuario pormeio de uma mensagem de alerta perguntando se deverao ser geradas as ordens de serviao para os bens filhos. Caso o parametro esteja configurado para aNa, ele nao gerara as ordens deserviao para os bens filhos e tambam nao consultara o usuario.""aadd(aSX6,{""MV_NGVEROS"",""S""})   //? Se o conteado do parametro estiver preenchido com aVa a consultara umaanica vez o usuario sobre a existancia de ordens de serviao abertas,solicitando sua confirmaaao. Caso selecione aSima sera gerada a ordem deserviao automatica mesmo que possua ordem de serviao aberta para omesmo bem, serviao e seqaancia. Caso selecione aNaoa, nao sera gerada aordem de serviao para as manutenCOES que ja possuarem ordem de serviaoaberta.? Caso o conteado do parametro esteja preenchida com aSa - consultara ousuario a cada manutenaao sobre a existancia de ordens de serviaos abertas,slicitando sua confirmaaao. Caso o conteado do parametro esteja preenchida com aNa - nao consultara ousuario e gerara ordem de serviao automatica mesmo com ordens de serviaoem aberto."
	aadd(aSX6,{"MV_NGIOSAU","S"})   //Indica se o sistema deve permitir imprimir O.S geradas automaticamente por contador. Informar S=Sim; N=Nao "
	aadd(aSX6,{"MV_NGGERSA","P"})   //Indica se deve gerar Solicitaaao ao Armazam em vez de gerar Solicitaaao deCompra "
	aadd(aSX6,{"MV_NGCOFIX","50"})  //Percentual utilizado para calcular o contador fixo da manutenaao. Opercentual pode ser de 0 a 100. "
	aadd(aSX6,{"MV_NGSSWRK","S"})   //Indica se no modulo de Solicitaaao de Serviao sera utilizado envio de mensagens por Workflow online pelos solicitantes. (Informar S=Sim; N=Nao) "
	aadd(aSX6,{"MV_NGHISES","N"})   //? Quando o parametro estiver configurado em aNa, mesmo que o madulManutenaao de Ativos esteja integrado com o modulo Estoque, nao deveraser efetuada a baixa no estoque, quando o serviao da ordem estiver igual aaHISTORa - Histarico, devera habilitar tambam o campo de custo parainformar os valores do insumo no retorno da ordem de serviao.Quando o parametro estiver configurado em aSa, devera baixar do estoque os insumos tambam das ordens de serviao de aHistaricoa."
	aadd(aSX6,{"MV_SGAMNT","N"})    //Integraaao Gestao Ambiental (SGA) e Manutenaao de Ativos (MNT) 
	//aadd(aSX6,{"MV_ULMES","20211130"})   //Data do ultimo fechamento do estoque (Parametro do modulo de estoque)."
	aadd(aSX6,{"MV_ESTNEG","N"})    //Indica se o sistema permitira que os saldos em estoque dos produtos fiquem negativos atravas de movimentaCOES. Conteado deve ser S=Sim; N=Nao (Parametro do modulo de estoque)."
	aadd(aSX6,{"MV_NGCOQPR","S"})   //Caso o parametro esteja configurado com aSa, no momento do reporte de insumo, o Sistema verifica se a quantidade de peaas de reposiaao informada no Cadastro do Bem esta de acordo com a quantidade informada no Reporte. Nao sera permitida a inclusao de quantidades superiores ao que foiregistrado no Cadastro do Bem, nesta situaaao o Sistema exibira uma mensagem de alerta. Caso o parametro esteja configurado com aNa, no momento do reporte de insumo, o Sistema nao verifica se quantidade de peaas de reposiaao informada no Cadastro do Bem esta de acordo com a quantidade informada no Reporte."
	aadd(aSX6,{"MV_NGTNDFL","S"})   //Identifica se no retorno de O.S corretiva sera obrigataria a digitaaao de irregularidade. Informar S = SIM ou N = Nao. "

	For x := 1 to Len(aSX6)
		TCSQLEXEC(" UPDATE SX6"+cEmpresa+" SET X6_CONTEUD = '" + aSX6[x][2] + "' WHERE X6_VAR = '" +  aSX6[x][1] + "' ") 
	Next
    
Return


//******************************************************************************************
// Vetores com dados de Atualizacao dos Parametros conforme modulo - Igor Maeus 07/12/2021
//******************************************************************************************

//Configuraaao dos parametros Modulo SIGA MNT a Controle de Combustavel - Gestao de Frotas  *********************************************	

Static Function AtParCob() 
	Local x := 1

	aadd(aSX6,{"MV_NGTOLVL","0,1"})  	//Valor de Tolerancia para fechamento de Nota Fiscal de Abastecimento."
	aadd(aSX6,{"MV_ESTHOME","S"})   	//Permite controlar estoque Home Base ( Postos Internos) Sendo: S = No abastecimento; C = Na conciliaaao; N = Nao controla estoque de combustavel."
	aadd(aSX6,{"MV_NGMOTGE",""})   		//CPF do motorista Genarico
	aadd(aSX6,{"MV_NGMNTF","N"})   		//Integraaao com Financeiro (Gera SC7 pedido de compras) 
	aadd(aSX6,{"MV_NGPRAUT","0,1"})   	//Percentual aceitavel de variaaao de autonomia. O percentual pode variar de 0 a 100."
	aadd(aSX6,{"MV_NGPRENE","N"})   	//Indica se permite o usuario escolher se pode a partir da inconsistancia gerar preao negociado oriundo da importaaao"
	aadd(aSX6,{"MV_NGCONEG","N"})   	//Checa valor negociado com o posto 
	aadd(aSX6,{"MV_NGPRSB2","N"})   	//Busca preao madio na tabela SB2 
	aadd(aSX6,{"MV_MNTQDAB",""})   		//Dada/hora de fechamento de movimentaCOES no cadastro de abastecimento no MNT. Ex: 10/12/2008 18:00"
	aadd(aSX6,{"MV_NGMOTAB","1"})   	//Indica quais tipos de motoristas podem realizar abastecimentos 1=Praprio, 2=Terceiro, 3= Agregado. a possavel configurar este parametro com mais de um tipo, ex: 1;2."

	For x := 1 to Len(aSX6)
		TCSQLEXEC(" UPDATE SX6"+cEmpresa+" SET X6_CONTEUD = '" + aSX6[x][2] + "' WHERE X6_VAR = '" +  aSX6[x][1] + "' ") 
	Next
    
Return


//******************************************************************************************
// Vetores com dados de Atualizacao dos Parametros conforme modulo - Igor Maeus 07/12/2021
//******************************************************************************************

//Configuraaao dos parametros Modulo SIGA MNT a Controle de Pneus - Gestao de Frotas  *********************************************

Static Function AtParPne() 
	Local x := 1

	aadd(aSX6,{"MV_NGPNEUS","S"})   	//Indica que utiliza modulo de controle de pneus 
	aadd(aSX6,{"MV_NGSERPN","PNEMOV"})  //Cadigo de serviao para movimentaaao de pneus 
	aadd(aSX6,{"MV_NGRODIZ","PNEROD"})  //Cadigo de serviao para rodazio de pneus 
	aadd(aSX6,{"MV_NGSTEP","S"})   		//Indica se pode movimentar o pneu de STEP no serviao de rodazio
	aadd(aSX6,{"MV_NGSTEST","53"})   	//Cadigo do Status para pneu em Estoque 
	aadd(aSX6,{"MV_NGDIFSU","1"})   	//Diferenaa de sulco aceitavel para inclusao de pneus com sulco diferente no mesmo eixo.
	aadd(aSX6,{"MV_NGSTATR","55"})   	//Cadigo do Status para pneus em Transito 
	aadd(aSX6,{"MV_NGMOVTR",""})   		//Cadigo do movimento de estoque para transferancia de pneus entre filiais, Utilizado no programa de transferancia entre filiais."
	aadd(aSX6,{"MV_NGSTAAT","54"})   	//Cadigo do Status para Aguardando Analise Tacnica 
	aadd(aSX6,{"MV_NGSTARS","58"})   	//Cadigo do Status para Removido de Serviao 
	aadd(aSX6,{"MV_NGSEREF","PNEREF"})  ///Cadigo do serviao para Reforma de pneus a Recape 
	aadd(aSX6,{"MV_NGSECON","PNECON"})  //Cadigo do serviao para conserto de pneus 
	aadd(aSX6,{"MV_NGSTAGR","51"})   	//Cadigo do Status para Aguardando Reforma 
	aadd(aSX6,{"MV_NGSTAGC","52"})   	//Cadigo do Status para Aguardando Conserto 
	aadd(aSX6,{"MV_NGSTAPL","50"})   	//Cadigo do Status para pneu Aplicado 
	aadd(aSX6,{"MV_NGSTAEN","61"})   	//Pneu novo 
	aadd(aSX6,{"MV_NGSTAEU","59"})   	//Pneu usado 
	aadd(aSX6,{"MV_NGSTAER","60"})   	//Pneu recapado 
	aadd(aSX6,{"MV_NGSTACA","62"})   	//Cadigo do Status para CANIBALISMO (Bem em processo de Desmontagem para reuso)
	aadd(aSX6,{"MV_NGSERCA","PNECAN"})  //Indica o cadigo do Serviao para CANIBALISMO (Desmontagem da Estrutura de Rodados)
	aadd(aSX6,{"MV_NGSTEP","S"})   		//Indica se pode movimentar o Pneu Estepe no serviao de rodazio. Informar S = sim ou N = nao
	aadd(aSX6,{"MV_NGMOROD","63"})   	//Cadigo do motivo padrao para rodazio pneus 
	aadd(aSX6,{"MV_NGPNEST","S"})   	//Indica se a empresa utiliza a identificaaao do Bem no controle de estoque.
	aadd(aSX6,{"MV_NGMODEF","64"})   	//Cadigo de motivo de defeito de pneus 
	aadd(aSX6,{"MV_NGSINSP","PNEINS"})  //Indica o cadigo do serviao de inspeaao de pneus 
	aadd(aSX6,{"MV_NGOCINS","64"})   	//Indica a causa para movimentaaao posicional apos inspeaao

	For x := 1 to Len(aSX6)
		TCSQLEXEC(" UPDATE SX6"+cEmpresa+" SET X6_CONTEUD = '" + aSX6[x][2] + "' WHERE X6_VAR = '" +  aSX6[x][1] + "' ") 
	Next
    
Return



//**********************************************************************************************************************
// Ajusta a criaaao de indices na tabela SIX - Igor Maeus 12/11/2021
//**********************************************************************************************************************


Static Function AtuIndice()
	
    aIndice := {Array(1, 12)} //Linha e Coluna - Para novos campos incrementar somente o primeiro numero
                   //INDICE,    ORDEM,   CHAVE,   				DESCRICAO,   			DESCSPA,    			  DESCENG,        			PROPRI,          F3,  		NICKNAME, 		SHOWPESQ,  IX_VIRTUAL, IX_VIRCUST
    aIndice[1]  := {'SC5',      '',      'C5_FILIAL+C5_X_NOMCL',  'Filial+Num.Contrato',  'Filial+Num.Contrato',    'Filial+Num.Contrato',    'U',              '',       'NICKNCL',      'S',       '2',        '3'}
	CriaIndice()
Return

Static Function CriaIndice()
	Local strOrdem :=	""
	Local IntCont := 1

	For IntCont := 1 to Len(aIndice)

		strSQL:="SELECT * FROM SIX"+cEmpresa+" WHERE INDICE = '"+ aIndice[IntCont][1]+"' AND CHAVE = '"+ aIndice[IntCont][3]+"' AND D_E_L_E_T_ <> '*' "

		If Select("QRY") > 0 
		QRY->(dbCloseArea())
		Endif 

		TcQuery strSQL NEW Alias "QRY"


		IF QRY->INDICE != aIndice[IntCont][1]		

			//Busca a ultima ordem de indice na tabela SC5
			strSQL:="SELECT MAX(ORDEM) AS SOMA FROM SIX"+cEmpresa+" WHERE INDICE = '"+ aIndice[IntCont][1]+"' AND D_E_L_E_T_ <> '*' "

			If Select("QRY") > 0 
				QRY->(dbCloseArea())
			Endif 

			TcQuery strSQL NEW Alias "QRY"

			IF !QRY->(eOF())

				strOrdem := SOMA1(QRY->SOMA)
			
				reclock('SIX',.T.)
				INDICE  	:=  aIndice[IntCont][1]
				ORDEM    	:=  strOrdem
				CHAVE    	:=  aIndice[IntCont][3]
				DESCRICAO  	:=  aIndice[IntCont][4]
				DESCSPA  	:=  aIndice[IntCont][5]
				DESCENG     :=  aIndice[IntCont][6]
				PROPRI  	:=  aIndice[IntCont][7]
				F3   		:=  aIndice[IntCont][8]
				NICKNAME   	:=  aIndice[IfntCont][9]
				SHOWPESQ  	:=  aIndice[IntCont][10]
				IX_VIRTUAL  :=  aIndice[IntCont][11]
				IX_VIRCUST  :=  aIndice[IntCont][12]
				msunlock()

			ENDIF
		ENDIF
	Next

Return

//**************************************************************************************
//Ajusta as abas e campos da tabela SC5 - TES - Zanoti - 09/2019 - Igor Mateus 11/2021
//**************************************************************************************

Static Function AbaSC5()
	Local aErros 	:= {}
	Local cErro		:=""
	Local cLog		:=""
	Local ix		:=0
	Local cCpoAba1	:=""
	Local cCpoAba2	:=""
	Local cCpoAba3	:=""
	Local cCpoAba4	:=""
	Local cCpoAba5	:=""
			
		Begin Transaction
		 	
		  	//APAGA OS REGISTROS DA SC5 NA TABELA SXA 

			strSQL:="DELETE FROM SXA"+cEmpresa+" WHERE XA_ALIAS = 'SC5'"
			nResult := TCSQLEXEC(strSQL)
			If nResult <> 0
				cErro   := " Erro:"+TCSqlError()
		   		AADD(aErros,cErro)
		  	Else
		   		cErro := ""
		  	EndIf
		  	
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SC5"
		    XA_ORDEM	:="1"
		    XA_DESCRIC	:="Cliente"
		    XA_DESCSPA	:="Cliente"
		    XA_DESCENG	:="Cliente"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		  	
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SC5"
		    XA_ORDEM	:="2"
		    XA_DESCRIC	:="Vendedor"
		    XA_DESCSPA	:="Vendedor"
		    XA_DESCENG	:="Vendedor"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SC5"
		    XA_ORDEM	:="3"
		    XA_DESCRIC	:="Fiscal"
		    XA_DESCSPA	:="Fiscal"
		    XA_DESCENG	:="Fiscal"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SC5"
		    XA_ORDEM	:="4"
		    XA_DESCRIC	:="Financeiro"
		    XA_DESCSPA	:="Financeiro"
		    XA_DESCENG	:="Financeiro"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		    
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SC5"
		    XA_ORDEM	:="5"
		    XA_DESCRIC	:="Frete / Transp."
		    XA_DESCSPA	:="Frete / Transp."
		    XA_DESCENG	:="Frete / Transp."
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	    
	  
		    //ABA CLIENTE
			cCpoAba1	:="C5_NUM,C5_TIPO,C5_CLIENTE,C5_LOJACLI,C5_CLIENT,C5_LOJAENT,C5_CONDPAG,C5_TABELA,C5_EMISSAO,C5_COTACAO,C5_MOEDA,C5_MENNOTA,C5_MENPAD,C5_TIPLIB,C5_NATUREZ,C5_X_EST,C5_X_MUNE,C5_X_BAIRR,C5_X_END,C5_X_NOMCL,C5_X_FPGT"
			//ABA VENDEDOR
			cCpoAba2	:="C5_VEND1,C5_COMIS1,C5_VEND2,C5_COMIS2,C5_VEND3,C5_COMIS3,C5_VEND4,C5_COMIS4,C5_VEND5,C5_COMIS5"
			//ABA FISCAL
			cCpoAba3	:="C5_INCISS,C5_FORNISS,C5_RECISS,C5_MUNPRES,C5_TABTRF,C5_DESCMUN,C5_IMINT,C5_CGCINT,C5_CLIINT,C5_SERSUBS,C5_NFSUBST"
			//ABA FINANCEIRO
			cCpoAba4	:="C5_DESC1,C5_DESC2,C5_DESC3,C5_DESC4,C5_BANCO,C5_DESCFI,C5_PARC1,C5_DATA1,C5_PARC2,C5_DATA2,C5_PARC3,C5_DATA3,C5_PARC4,C5_DATA4,C5_DESPESA,C5_REAJUST,C5_ACRSFIN"
			//ABA FRETE / TRANSPORTE
			cCpoAba5	:="C5_TRANSP,C5_TIPOCLI,C5_TPFRETE,C5_FRETE,C5_SEGURO,C5_FRTCFOP,C5_FRETAUT,C5_PESOL,C5_PBRUTO,C5_REDESP,C5_TPCARGA,C5_KM,C5_VLR_FRT,C5_GERAWMS,C5_FECENT,C5_RECFAUT,C5_VEICULO,C5_PLACA1,C5_CODMOT,C5_CMUNDE,C5_PLACA2"
			
			UpdSX3("1","'SC5'",cCpoAba1) 
			UpdSX3("2","'SC5'",cCpoAba2)
			UpdSX3("3","'SC5'",cCpoAba3)
			UpdSX3("4","'SC5'",cCpoAba4)
			UpdSX3("5","'SC5'",cCpoAba5)
							
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	  	
		End Transaction
		
		//MsgInfo("Atualizacao efetuada com sucesso!")
	//EndIf
	
Return

//**************************************************************************************
//Ajusta as abas e campos da tabela SF4 - TES - Zanoti - 09/2019 - Igor Mateus 11/2021
//**************************************************************************************

Static Function AbaSF4()
Local aErros 	:= {}
Local cErro		:=""
Local cLog		:=""
Local ix		:=0
Local cCpoAba1	:=""
Local cCpoAba2	:=""
Local cCpoAba3	:=""
Local cCpoAba4	:=""
Local cCpoAba5	:=""
Local cCpoAba6	:=""
Local cCpoAba7	:=""
Local cCpoAba8	:=""
Local cCpoAba9	:=""
Local cCpoAbaA	:=""
Local cCpoAbaOut:=""
Local cCpoBrow	:=""
//Local cResp		:=""

	//If MsgYesNo("Deseja atualizar os campos e abas do cadastro de TES?", "Abas/Campos TES")
		Begin Transaction
		 	
		  	//APAGA OS REGISTROS DA SF4 NA TABELA SXA 
			strSQL:="DELETE FROM SXA"+cEmpresa+" WHERE XA_ALIAS = 'SF4'"
			nResult := TCSQLEXEC(strSQL)
			If nResult <> 0
				cErro   := " Erro:"+TCSqlError()
		   		AADD(aErros,cErro)
		  	Else
		   		cErro := ""
		  	EndIf
		  	
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="1"
		    XA_DESCRIC	:="Geral"
		    XA_DESCSPA	:="Geral"
		    XA_DESCENG	:="Geral"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		  	
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="2"
		    XA_DESCRIC	:="Icms"
		    XA_DESCSPA	:="Icms"
		    XA_DESCENG	:="Icms"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="3"
		    XA_DESCRIC	:="Ipi"
		    XA_DESCSPA	:="Ipi"
		    XA_DESCENG	:="Ipi"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="4"
		    XA_DESCRIC	:="Pis/Cofins"
		    XA_DESCSPA	:="Pis/Cofins"
		    XA_DESCENG	:="Pis/Cofins"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		    
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="5"
		    XA_DESCRIC	:="Iss"
		    XA_DESCSPA	:="Iss"
		    XA_DESCENG	:="Iss"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	    
		    
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="6"
		    XA_DESCRIC	:="Funrural"
		    XA_DESCSPA	:="Funrural"
		    XA_DESCENG	:="Funrural"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())		        
		    
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="7"
		    XA_DESCRIC	:="Simples"
		    XA_DESCSPA	:="Simples"
		    XA_DESCENG	:="Simples"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())
		    
		  	//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="8"
		    XA_DESCRIC	:="Contabilidade"
		    XA_DESCSPA	:="Contabilidade"
		    XA_DESCENG	:="Contabilidade"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	

			//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="9"
		    XA_DESCRIC	:="Ativo/CIAP"
		    XA_DESCSPA	:="Ativo/CIAP"
		    XA_DESCENG	:="Ativo/CIAP"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
			
			//CRIA OS REGISTROS DA SF4 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SF4"
		    XA_ORDEM	:="A"
		    XA_DESCRIC	:="SPED"
		    XA_DESCSPA	:="SPED"
		    XA_DESCENG	:="SPED"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())    
		    
		    //Geral
			cCpoAba1	:="F4_FILIAL,F4_CODIGO,F4_TIPO,F4_DUPLIC,F4_CF,F4_TEXTO,F4_FINALID,F4_CONSUMO,F4_FORMULA,F4_TESDV,F4_TESP3,F4_QTDZERO,F4_SLDNPT,F4_DEVZERO,F4_MSBLQL,F4_TESENV,F4_TRANFIL,F4_TESE3,F4_OPERSUC,F4_DUPLIPI,F4_DESCOND,F4_NATOPER,F4_FORINFC,F4_PAGCOM,F4_BONIF,F4_VLRZERO,F4_CUSENTR,F4_TRANCQ,F4_MSGLT,F4_CONTERC,F4_INFITEM,F4_ESTOQUE,F4_PODER3,F4_X_OPER,F4_X_DESOP"
			//Icms
			cCpoAba2	:="F4_ICM,F4_CREDICM,F4_BASEICM,F4_LFICM,F4_COMPL,F4_AGREG,F4_CIAP,F4_ATUATF,F4_DESPICM,F4_TRFICM,F4_OBSICM,F4_AJUSTE,F4_CRPRST,F4_CRPRELE,F4_AGRRETC,F4_BENSATF,F4_DSPRDIC,F4_CRDEST,F4_COPSIMP,F4_AGRDRED,F4_ESTCRED,F4_CREDACU,F4_CPRESPR,F4_CRDACUM,F4_ISEFECP,F4_BSRDICM,F4_DICMFUN,F4_CIAPTRB,F4_PARTICM,F4_REDANT,F4_RDBSICM,F4_APLIRED,F4_ISEFEMT,F4_ANTICMS,F4_FEEF,F4_REDBCCE,F4_RGESPCI,F4_FTRICMS,F4_DIFAL,F4_BASCMP,F4_MOTICMS,F4_AGREGCP,F4_CODOBSE,F4_ENQLEG,F4_CRDICMA,F4_INCSOL,F4_STDESC,F4_BSICMST,F4_CREDST,F4_OBSSOL,F4_ICMSDIF,F4_PICMDIF,F4_ICMSST,F4_MKPCMP,F4_LFICMST,F4_ALICRST,F4_APLREDP,F4_ATACVAR,F4_CRICMST,F4_STLIQ,F4_STREDU,F4_BSICMRE,F4_ICMSTMT,F4_IVAUTIL,F4_APLIIVA,F4_OUTPERC,F4_MKPSOL,F4_TRFICST,F4_ART274,F4_VARATAC"
			//Ipi
			cCpoAba3	:="F4_CREDIPI,F4_BASEIPI,F4_LFIPI,F4_DESTACA,F4_INCIDE,F4_IPIFRET,F4_DESPIPI,F4_TPIPI,F4_IPILICM,F4_IPIANT,F4_IPIPC,F4_IPIPECR,F4_TXAPIPI,F4_IPIOBS,F4_IPIVFCF,F4_PSCFST,F4_IPIMIN,F4_PISMIN,F4_COFMIN,F4_MALQPIS,F4_GRPNATR,F4_SOMAIPI,F4_GRPCST,F4_DUPLIPI,F4_IPI"
			//Pis/Cofins
			cCpoAba4	:="F4_PISCOF,F4_PISCRED,F4_BASEPIS,F4_BASECOF,F4_PISFISC,F4_DESPCOF,F4_TPREG,F4_PISBRUT,F4_COFDZYF,F4_BCRDPIS,F4_COFBRUT,F4_DESPPIS,F4_AGRPIS,F4_BCRDCOF,F4_AGRCOF,F4_PISDZYF,F4_CSTCF1,F4_CSTPF1,F4_EFDF100,F4_APSCFST,F4_MALQCOF,F4_COLVDIF,F4_APLREPC,F4_INTBSIC,F4_BCPCST,F4_DIFALPC,F4_REFATAN"
			//Iss
			cCpoAba5	:="F4_ISS,F4_LFISS,F4_BASEISS,F4_ISSST,F4_RETISS,F4_NATOPNF,F4_CTBHISS,F4_CSBHISS,F4_FRETISS,F4_AGRISS,F4_DESCISS,F4_TRIBPRD,F4_COP,F4_TRIBPRD,F4_CFPS"
			//Funrural
			cCpoAba6	:="F4_CONTSOC,F4_BSRURAL,F4_ALSENAR,F4_CSENAR"
			//Simples
			cCpoAba7	:="F4_CRPRSIM,F4_CSOSN,F4_TPRSPL"		
			//Contabilidade
			cCpoAba8	:="F4_X_CTB,F4_X_DEB,F4_X_CRED,F4_X_DICMS,F4_X_CICMS,F4_X_DIPI,F4_X_CIPI,F4_X_DPIS,F4_X_CPIS,F4_X_DCOF,F4_X_CCOF,F4_X_DISS,F4_X_CISS,F4_X_DICST,F4_X_CICST,F4_X_ISSRD,F4_X_ISSRC,F4_X_PISRD,F4_X_PISRC,F4_X_COFRD,F4_X_COFRC,F4_X_CSLRD,F4_X_CSLRC,F4_X_INSRD,F4_X_INSRC,F4_X_IRRRD,F4_X_IRRRC,F4_X_DDIFA,F4_X_CDIFA,F4_X_DII,F4_X_CII"
			//Ativo/CIAP
			cCpoAba9	:="F4_CIAP,F4_ATUATF,F4_AJUSTE,F4_BENSATF,F4_CIAPTRB"
			//SPED
			cCpoAbaA	:="F4_SITTRIB,F4_CNATREC,F4_GRPNATR,F4_TNATREC,F4_INDNTFR,F4_CODBCC,F4_CSTPIS,F4_CSTCOF,F4_CSTISS,F4_CTIPI"
			//Outros
			cCpoAbaOut	:="F4_CODINFC,F4_ALQFEEF,F4_STCONF,F4_PCREDAC,F4_COMPRED,F4_PAUTICM,F4_CRPRERO,F4_CRPREPE,F4_DUPLIST,F4_CRPREPR,F4_VLAGREG,F4_CINSS,F4_CRICMS,F4_DBSTIRR,F4_ISEFERN,F4_FCALCPR,F4_DEDDIF"
		
			//Campos a serem exibidos no Browser
			cCpoBrow	:="'F4_FILIAL','F4_CODIGO','F4_TIPO','F4_CF','F4_TEXTO','F4_FINALID','F4_ESTOQUE','F4_DUPLIC'"
			
			UpdSX3("1","'SF4'",cCpoAba1) 
			UpdSX3("2","'SF4'",cCpoAba2)
			UpdSX3("3","'SF4'",cCpoAba3)
			UpdSX3("4","'SF4'",cCpoAba4)
			UpdSX3("5","'SF4'",cCpoAba5)
			UpdSX3("6","'SF4'",cCpoAba6)
			UpdSX3("7","'SF4'",cCpoAba7)
			UpdSX3("8","'SF4'",cCpoAba8)
			UpdSX3("9","'SF4'",cCpoAba9)
			UpdSX3("A","'SF4'",cCpoAbaA)
			UpdSX3(" ","'SF4'",cCpoAbaOut)

			UpdSX3B("'SF4'",cCpoBrow)
							
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERACAO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	  	
		End Transaction
		//MsgInfo("Atualizacao efetuada com sucesso!")
	//EndIf
	
Return


//***************************************************************************************************
//Ajusta as abas e campos da tabela SA1 - Clientes - Renan Rescia - 22/02/2021 / Igor Mateus 11/2021
//***************************************************************************************************


Static Function AbaSA1()
Local aErros 	:= {}
Local cErro		:=""
Local cLog		:=""
Local ix		:=0
Local cCpoAba1	:=""
Local cCpoAba2	:=""
Local cCpoAba3	:=""
Local cCpoAba4	:=""
Local cCpoAba5	:=""
Local cCpoAba6	:=""
Local cCpoAbaOut:=""
Local cCpoBrow	:=""
//Local cResp		:=""

		Begin Transaction
		  	
		  	//APAGA OS REGISTROS DA SA1 NA TABELA SXA 
			strSQL:="DELETE FROM SXA"+cEmpresa+" WHERE XA_ALIAS = 'SA1'"
			nResult := TCSQLEXEC(strSQL)
			If nResult <> 0
				cErro   := " Erro:"+TCSqlError()
		   		AADD(aErros,cErro)
		  	Else
		   		cErro := ""
		  	EndIf
		  	
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	
		  	//CRIA OS REGISTROS DA SA1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA1"
		    XA_ORDEM	:="1"
		    XA_DESCRIC	:="Cadastrais"
		    XA_DESCSPA	:="Cadastrais"
		    XA_DESCENG	:="Cadastrais"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		  	
		  	//CRIA OS REGISTROS DA SA1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA1"
		    XA_ORDEM	:="2"
		    XA_DESCRIC	:="Admin/Fin"
		    XA_DESCSPA	:="Admin/Fin"
		    XA_DESCENG	:="Admin/Fin"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SA1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA1"
		    XA_ORDEM	:="3"
		    XA_DESCRIC	:="Fiscais"
		    XA_DESCSPA	:="Fiscais"
		    XA_DESCENG	:="Fiscais"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SA1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA1"
		    XA_ORDEM	:="4"
		    XA_DESCRIC	:="Vendas"
		    XA_DESCSPA	:="Vendas"
		    XA_DESCENG	:="Vendas"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		    
		  	//CRIA OS REGISTROS DA SA1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA1"
		    XA_ORDEM	:="5"
		    XA_DESCRIC	:="Logistica"
		    XA_DESCSPA	:="Logistica"
		    XA_DESCENG	:="Logistica"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())

			//CRIA OS REGISTROS DA SA1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA1"
		    XA_ORDEM	:="6"
		    XA_DESCRIC	:="Contabilidade"
		    XA_DESCSPA	:="Contabilidade"
		    XA_DESCENG	:="Contabilidade"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	  


			//CRIA OS REGISTROS DA SA1 NA TABELA SXA
	/* 		RecLock("SXA",.T.)
		    XA_ALIAS	:="SA1"
		    XA_ORDEM	:="7"
		    XA_DESCRIC	:="Segmentos"
		    XA_DESCSPA	:="Segmentos"
		    XA_DESCENG	:="Segmentos"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	   
		   	 */
			 
  //Cadastrais
			cCpoAba1	:= "A1_COD,A1_TIPO,A1_EST,A1_PESSOA,A1_CGC,A1_NOME,A1_NREDUZ,A1_LOJA,A1_CEP,A1_END,A1_BAIRRO,A1_COD_MUN,A1_MUN, A1_CONTATO,A1_DDD,A1_TEL,A1_EMAIL,A1_PAIS,A1_PAISDES,A1_PFISICA,A1_INSCR,A1_INSCRM,A1_INSCRUR,A1_MSBLQL,A1_DTCAD,A1_HRCAD"
			//Admin/Fin
			cCpoAba2	:="A1_NATUREZ,A1_ENDCOB,A1_BAIRROC,A1_MUNIC,A1_ESTC,A1_CEPC,A1_BCO1,A1_BCO2,A1_BCO3,A1_BCO4,A1_BCO5,A1_MSALDO,A1_MCOMPRA,A1_METR,A1_NROCOM,A1_SALDUP,A1_NROPAG,A1_ATR,A1_VACUM,A1_TITPROT,A1_DTULTIT,A1_CHQDEVO,A1_DTULCHQ,A1_MATR,A1_MAIDUPL,A1_SALDUPM,A1_PAGATR,A1_ATIVIDA,A1_SALFIN,A1_SALFINM,A1_RISCO,A1_LC,A1_VENCLC,A1_CLASSE,A1_LCFIN,A1_TIPPER"
			//Fiscais
			cCpoAba3	:="A1_CONTRIB,A1_SIMPNAC,A1_RECISS,A1_SUFRAMA,A1_INCISS,A1_ALIQIR,A1_CALCSUF,A1_GRPTRIB,A1_CODPAIS,A1_TPESSOA,A1_TPISSRS,A1_RECINSS,A1_RECCOFI,A1_RECCSLL,A1_RECPIS,A1_ABATIMP,A1_RECIRRF,A1_MINIRF,A1_CODMUN"
			//Vendas
			cCpoAba4	:="A1_COND,A1_VEND,A1_COMIS,A1_TPFRET,A1_DESC,A1_PRIOR,A1_PRICOM,A1_ULTCOM,A1_FORMVIS,A1_TEMVIS,A1_ULTVIS,A1_TMPVIS,A1_TMPSTD,A1_CLASVEN,A1_MENSAGE,A1_SALPEDL,A1_SALPED,A1_TABELA,A1_OBSERV,A1_SATIV1,A1_DSATIV,A1_SATIV2,A1_DSATIV2,A1_SATIV3,A1_DSATIV3,A1_SATIV4,A1_DSATIV4,A1_SATIV5,A1_DSATIV5,A1_SATIV6,A1_DSATIV6,A1_SATIV7,A1_DSATIV7,A1_SATIV8,A1_DSATIV8,A1_AGREG,A1_B2B,A1_GRPVEN"
			//Logistica
			cCpoAba5	:="A1_CODMUNE,A1_ENDENT,A1_BAIRROE,A1_ESTE,A1_COMPENT,A1_TRANS"
			//Contabilidade
			cCpoAba6	:="A1_CONTA"
			//Outros
			cCpoAbaOut	:="A1_INOVAUT,A1_RIMAMT,A1_RECFMD,A1_RFASEMT,A1_INCLTMG,A1_FOMEZER,A1_TPJ"
	
			//Campos a serem exibidos no Browser
			cCpoBrow	:="'A1_COD','A1_TIPO','A1_CGC','A1_NOME','A1_MUN','A1_DDD','A1_TEL','A1_CONTATO','A1_EMAIL'"

			UpdSX3("1","SA1",cCpoAba1) 
			UpdSX3("2","SA1",cCpoAba2)
			UpdSX3("3","SA1",cCpoAba3)
			UpdSX3("4","SA1",cCpoAba4)
			UpdSX3("5","SA1",cCpoAba5)
			UpdSX3("6","SA1",cCpoAba6)
			UpdSX3(" ","SA1",cCpoAbaOut)

			UpdSX3B("SA1",cCpoBrow)

							
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf

		  	  	
		End Transaction

		//MsgInfo("Atualizacao efetuada com sucesso!")
	//EndIf
	
Return


//*******************************************************************************************************
//Ajusta as abas e campos da tabela SA2 - Fornecedores - Renan Rescia - 22/02/2021 - Igor Mateus 11/2021
//*******************************************************************************************************


Static Function AbaSA2()
Local aErros 	:= {}
Local cErro		:=""
Local cLog		:=""
Local ix		:=0
Local cCpoAba1	:=""
Local cCpoAba2	:=""
Local cCpoAba3	:=""
Local cCpoAba4	:=""
Local cCpoAba5	:=""
Local cCpoAba6	:=""
Local cCpoAba7	:=""
Local cCpoAbaOut:=""
Local cCpoBrow	:=""
//Local cResp		:=""

		Begin Transaction

	  	
		  	//APAGA OS REGISTROS DA SA2 NA TABELA SXA 
			strSQL:="DELETE FROM SXA"+cEmpresa+" WHERE XA_ALIAS = 'SA2'"
			nResult := TCSQLEXEC(strSQL)
			If nResult <> 0
				cErro   := " Erro:"+TCSqlError()
		   		AADD(aErros,cErro)
		  	Else
		   		cErro := ""
		  	EndIf
		  	
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	
		  	//CRIA OS REGISTROS DA SA2 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA2"
		    XA_ORDEM	:="1"
		    XA_DESCRIC	:="Cadastrais"
		    XA_DESCSPA	:="Cadastrais"
		    XA_DESCENG	:="Cadastrais"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		  	
		  	//CRIA OS REGISTROS DA SA2 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA2"
		    XA_ORDEM	:="2"
		    XA_DESCRIC	:="Admin/Fin"
		    XA_DESCSPA	:="Admin/Fin"
		    XA_DESCENG	:="Admin/Fin"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SA2 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA2"
		    XA_ORDEM	:="3"
		    XA_DESCRIC	:="Fiscais"
		    XA_DESCSPA	:="Fiscais"
		    XA_DESCENG	:="Fiscais"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SA2 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA2"
		    XA_ORDEM	:="4"
		    XA_DESCRIC	:="Compras"
		    XA_DESCSPA	:="Compras"
		    XA_DESCENG	:="Compras"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		    
		  	//CRIA OS REGISTROS DA SA2 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA2"
		    XA_ORDEM	:="5"
		    XA_DESCRIC	:="Residente Exterior"
		    XA_DESCSPA	:="Residente Exterior"
		    XA_DESCENG	:="Residente Exterior"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())

			//CRIA OS REGISTROS DA SA2 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA2"
		    XA_ORDEM	:="6"
		    XA_DESCRIC	:="Autonomos"
		    XA_DESCSPA	:="Autonomos"
		    XA_DESCENG	:="Autonomos"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	

			//CRIA OS REGISTROS DA SA2 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA2"
		    XA_ORDEM	:="7"
		    XA_DESCRIC	:="Contabilidade"
		    XA_DESCSPA	:="Contabilidade"
		    XA_DESCENG	:="Contabilidade"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())

			//CRIA OS REGISTROS DA SA2 NA TABELA SXA
/* 		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SA2"
		    XA_ORDEM	:="8"
		    XA_DESCRIC	:="Aut / Resident Ext"
		    XA_DESCSPA	:="Aut / Resident Ext"
		    XA_DESCENG	:="Aut / Resident Ext"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())    */ 

			   		   
		    //Cadastrais
			cCpoAba1	:= "A2_COD,A2_TIPO,A2_EST,A2_CGC,A2_NOME,A2_NREDUZ,A2_LOJA,A2_CEP,A2_END,A2_BAIRRO,A2_COMPLEM,A2_COD_MUN,A2_MUN,A2_CX_POST,A2_PFISICA,A2_DDD,A2_TEL,A2_FAX,A2_INSCR,A2_INSCRM,A2_PAIS,A2_PAISDES,A2_EMAIL,A2_MSBLQL"
			//Admin/Fin
			cCpoAba2	:="A2_BANCO,A2_AGENCIA,A2_DVAGE,A2_NUMCON,A2_DVCTA,A2_NATUREZ,A2_COND,A2_MCOMPRA,A2_METR,A2_MSALDO,A2_NROCOM,A2_SALDUP,A2_SALDUPM,A2_CODADM,A2_FORMPAG,A2_CPFIRP,A2_TIPCTA"
			//Fiscais
			cCpoAba3	:="A2_GROSSIR,A2_TIPORUR,A2_RECISS,A2_CODMUN,A2_RECINSS,A2_CODPAIS,A2_TPESSOA,A2_TPISSRS,A2_GRPTRIB,A2_RECSEST,A2_RECPIS,A2_RECCOFI,A2_RECCSLL,A2_CALCIRF,A2_DEDBSPC,A2_CONTRIB,A2_IRPROG,A2_MINIRF"
			//Compras
			cCpoAba4	:="A2_CONTATO,A2_TRANSP,A2_RISCO,A2_PRICOM,A2_ULTCOM,A2_B2B"
			//Residente Exterior
			cCpoAba5	:="A2_NEMPR,A2_TPCON,A2_PAISEX,A2_NIFEX,A2_LOGEX,A2_NUMEX,A2_COMPLR,A2_BAIEX,A2_BREEX,A2_TPREX,A2_TRBEX,A2_ESTEX,A2_TELRE,A2_CGCEX,A2_DTINIR,A2_DTFIMR,A2_CIDEX,A2_POSEX,A2_PAISSUB"
			//Autonomos
			cCpoAba6	:="A2_DTNASC,A2_OCORREN,A2_CATEG,A2_CODNIT,A2_CATEFD"
			//Contabilidade
			cCpoAba7	:="A2_CONTA"	
			//Outros
			cCpoAbaOut  :="A2_SITESBH,A2_TPLOGR,A2_CPOMSP,A2_RECFET,A2_RFACS,A2_RFABOV,A2_TPJ,A2_DTCONV,A2_RECFMD,A2_CTARE,A2_INCLTMG,A2_INOVAUT,A2_NOMRESP"

			//Campos a serem exibidos no Browser
			cCpoBrow	:="'A2_COD','A2_TIPO','A2_CGC','A2_NOME','A2_MUN','A2_DDD','A2_TEL','A2_CONTATO','A2_EMAIL'"
			
			UpdSX3("1","SA2",cCpoAba1) 
			UpdSX3("2","SA2",cCpoAba2)
			UpdSX3("3","SA2",cCpoAba3)
			UpdSX3("4","SA2",cCpoAba4)
			UpdSX3("5","SA2",cCpoAba5)
			UpdSX3("6","SA2",cCpoAba6)
			UpdSX3("7","SA2",cCpoAba7)
			UpdSX3(" ","SA2",cCpoAbaOut)

			UpdSX3B("SA2",cCpoBrow)	
								
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	  	
		End Transaction
		//MsgInfo("Atualizacao efetuada com sucesso!")
	//EndIf
	
Return

//***************************************************************************************************************
//Ajusta as abas e campos da tabela SB1 - Cadastro de Produto - Renan Rescia - 22/02/2021 - Igor Mateus 11/2021
//***************************************************************************************************************

Static Function AbaSB1()
Local aErros 	:= {}
Local cErro		:=""
Local cLog		:=""
Local ix		:=0
Local cCpoAba1	:=""
Local cCpoAba2	:=""
Local cCpoAba3	:=""
Local cCpoAba4	:=""
Local cCpoAba5	:=""
Local cCpoAba6	:=""
Local cCpoAbaOut:=""
Local cCpoBrow	:=""
//Local cResp		:=""


		Begin Transaction
	
		  	//APAGA OS REGISTROS DA SB1 NA TABELA SXA 
			strSQL:="DELETE FROM SXA"+cEmpresa+" WHERE XA_ALIAS = 'SB1'"
			nResult := TCSQLEXEC(strSQL)
			If nResult <> 0
				cErro   := " Erro:"+TCSqlError()
		   		AADD(aErros,cErro)
		  	Else
		   		cErro := ""
		  	EndIf
		  	
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SB1"
		    XA_ORDEM	:="1"
		    XA_DESCRIC	:="Cadastrais"
		    XA_DESCSPA	:="Cadastrais"
		    XA_DESCENG	:="Cadastrais"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		  	
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SB1"
		    XA_ORDEM	:="2"
		    XA_DESCRIC	:="Icms/Ipi/Iss"
		    XA_DESCSPA	:="Icms/Ipi/Iss"
		    XA_DESCENG	:="Icms/Ipi/Iss"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SB1"
		    XA_ORDEM	:="3"
		    XA_DESCRIC	:="Pcc/Ir/Inss"
		    XA_DESCSPA	:="Pcc/Ir/Inss"
		    XA_DESCENG	:="Pcc/Ir/Inss"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SB1"
		    XA_ORDEM	:="4"
		    XA_DESCRIC	:="Mrp/Suprimentos"
		    XA_DESCSPA	:="Mrp/Suprimentos"
		    XA_DESCENG	:="Mrp/Suprimentos"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		    
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SB1"
		    XA_ORDEM	:="5"
		    XA_DESCRIC	:="C.q."
		    XA_DESCSPA	:="C.q."
		    XA_DESCENG	:="C.q."
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	    
		    
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SB1"
		    XA_ORDEM	:="6"
		    XA_DESCRIC	:="Atendimento"
		    XA_DESCSPA	:="Atendimento"
		    XA_DESCENG	:="Atendimento"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	

			//CRIA OS REGISTROS DA SB1 NA TABELA SXA     
			RecLock("SXA",.T.)
		    XA_ALIAS	:="SB1"
		    XA_ORDEM	:="7"
		    XA_DESCRIC	:="Mat / Med"
		    XA_DESCSPA	:="Mat / Med"
		    XA_DESCENG	:="Mat / Med"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())		        
		    	    
			//CRIA OS REGISTROS DA SB1 NA TABELA SXA
			RecLock("SXA",.T.)
		    XA_ALIAS	:="SB1"
		    XA_ORDEM	:="8"
		    XA_DESCRIC	:="Garant Estend"
		    XA_DESCSPA	:="Garant Estend"
		    XA_DESCENG	:="Garant Estend"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())		        
		    	    		    	    
		    //Cadastrais
			cCpoAba1	:="B1_GRUPO,B1_COD,B1_DESC,B1_TIPO,B1_UM,B1_POSIPI,B1_LOCPAD,B1_SEGUM,B1_TE,B1_TS,B1_CONV,B1_TIPCONV,B1_UCOM,B1_ALTER,B1_PRV1,B1_CUSTD,B1_UCALSTD,B1_UPRC,B1_MCUSTD,B1_PESO,B1_LOJPROC,B1_UREV,B1_DATREF,B1_DTREFP1,B1_CODBAR,B1_COMIS,B1_CEST,B1_MSBLQL,B1_PESBRU,B1_CCCUSTO"
			//Icms/Ipi/Iss
			cCpoAba2	:="B1_PICM,B1_IPI,B1_ALIQISS,B1_CODISS,B1_PICMRET,B1_PICMENT,B1_IMPZFRC,B1_ORIGEM,B1_GRTRIB,B1_TAB_IPI,B1_VLR_IPI,B1_INT_ICM,B1_VLR_ICM,B1_REGRISS,B1_PRN944I"
			//Pcc/Ir/Inss
			cCpoAba3	:="B1_CONTSOC,B1_IRRF,B1_INSS,B1_REDINSS,B1_REDIRRF,B1_REDPIS,B1_REDCOF,B1_PCSLL,B1_PCOFINS,B1_PPIS,B1_PIS,B1_VLR_PIS,B1_RETOPER,B1_COFINS,B1_CSLL,B1_VLR_COF"
			//Mrp/Suprimentos
			cCpoAba4	:="B1_EMIN,B1_QE,B1_ESTFOR,B1_ESTSEG,B1_FORPRZ,B1_PE,B1_TIPE,B1_LE,B1_LM,B1_TOLER,B1_TIPODEC,B1_MRP,B1_PRVALID,B1_EMAX"
			//C.q
			cCpoAba5	:="B1_NOTAMIN,B1_TIPOCQ,B1_NUMCQPR"
			//Atendimento
			cCpoAba6	:="B1_BITMAP"
			//Gatantia Estendida
			cCpoAba8	:="B1_BITMAP,B1_GARANT"
			//Outros
			cCpoAbaOut  := "B1_EX_NCM,B1_EX_NBM,B1_FORAEST,B1_ANUENTE"
			
			//Campos a serem exibidos no Browser
			cCpoBrow	:="'B1_GRUPO','B1_COD','B1_DESC','B1_TIPO','B1_UM','B1_POSIPI','B1_CUSTD','B1_UPRC'"
			
			UpdSX3("1","'SB1'",cCpoAba1) 
			UpdSX3("2","'SB1'",cCpoAba2)
			UpdSX3("3","'SB1'",cCpoAba3)
			UpdSX3("4","'SB1'",cCpoAba4)
			UpdSX3("5","'SB1'",cCpoAba5)
			UpdSX3("6","'SB1'",cCpoAba6)
			UpdSX3(" ","'SB1'",cCpoAbaOut)

			UpdSX3B("'SB1'",cCpoBrow)
							
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	  	
		End Transaction
		//MsgInfo("Atualizacao efetuada com sucesso!")
	//EndIf
	
Return

//******************************************************************************************************************
//Ajusta as abas e campos da tabela SED - Cadastro de Naturezas - Renan Rescia - 22/02/2021 - Igor Mateus 11/2021
//******************************************************************************************************************

Static Function AbaSED()
Local aErros 	:= {}
Local cErro		:=""
Local cLog		:=""
Local ix		:=0
Local cCpoAba1	:=""
Local cCpoAba2	:=""
Local cCpoAba3	:=""
Local cCpoAba4	:=""
Local cCpoAba5	:=""
Local cCpoAba6	:=""
Local cCpoAba7	:=""
Local cCpoBrow	:=""
//Local cResp		:=""

	//If MsgYesNo("Deseja atualizar os campos e abas do cadastro de Naturezas?", "Abas/Campos Cad. Naturezas")
		Begin Transaction

		
		 	//APAGA OS REGISTROS DA SED NA TABELA SXA 
			strSQL:="DELETE FROM SXA"+cEmpresa+" WHERE XA_ALIAS = 'SED'"
			nResult := TCSQLEXEC(strSQL)
			If nResult <> 0
				cErro   := " Erro:"+TCSqlError()
		   		AADD(aErros,cErro)
		  	Else
		   		cErro := ""
		  	EndIf
		  	
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SED"
		    XA_ORDEM	:="1"
		    XA_DESCRIC	:="Dados da Natureza"
		    XA_DESCSPA	:="Dados da Natureza"
		    XA_DESCENG	:="Dados da Natureza"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		  	
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SED"
		    XA_ORDEM	:="2"
		    XA_DESCRIC	:="Fiscal"
		    XA_DESCSPA	:="Fiscal"
		    XA_DESCENG	:="Fiscal"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SED"
		    XA_ORDEM	:="3"
		    XA_DESCRIC	:="Juridico"
		    XA_DESCSPA	:="Juridico"
		    XA_DESCENG	:="Juridico"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SED"
		    XA_ORDEM	:="4"
		    XA_DESCRIC	:="Dados Contabeis"
		    XA_DESCSPA	:="Dados Contabeis"
		    XA_DESCENG	:="Dados Contabeis"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	
		    
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SED"
		    XA_ORDEM	:="5"
		    XA_DESCRIC	:="Pcc/Cof/Csll."
		    XA_DESCSPA	:="Pcc/Cof/Csll"
		    XA_DESCENG	:="Pcc/Cof/Csll"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())	    
		    
		  	//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SED"
		    XA_ORDEM	:="6"
		    XA_DESCRIC	:="Iss/Ir/Inss"
		    XA_DESCSPA	:="Iss/Ir/Inss"
		    XA_DESCENG	:="Iss/Ir/Inss"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())		        

			//CRIA OS REGISTROS DA SB1 NA TABELA SXA
		    RecLock("SXA",.T.)
		    XA_ALIAS	:="SED"
		    XA_ORDEM	:="7"
		    XA_DESCRIC	:="SPED"
		    XA_DESCSPA	:="SPED"
		    XA_DESCENG	:="SPED"
		    XA_PROPRI	:="U"
		    SXA->(MSUnLock())
				    
		    
		    //Dados da Natureza
			cCpoAba1	:="ED_CODIGO,ED_DESCRIC,ED_TIPO,ED_PAI,ED_MSBLQL,ED_COND,ED_MOVBCO,ED_DTINCLU"
			//Fiscal
			cCpoAba2	:="ED_CPRB"
			//Juridico
			cCpoAba3	:="ED_ESCRIT,ED_GRPJUR"
			//Dados Contabeis
			cCpoAba4	:="ED_CONTA"
			//Pis/Cofins/Csll
			cCpoAba5	:="ED_CALCCSL,ED_CALCCOF,ED_CALCPIS,ED_PERCCSL,ED_PERCCOF,ED_PERCCOF,ED_DEDPIS,ED_DEDCOF,ED_BASECOF,ED_BASEPIS"
			//Iss/Ir/Inss
			cCpoAba6	:="ED_CALCIRF,ED_CALCISS,ED_PERCIRF,ED_CALCINS,ED_PERCINS,ED_RECIRRF,ED_CALCINP,ED_PERCINP,ED_RINSSPA,ED_CDRECSL"
			//SPED
			cCpoAba7	:="ED_APURPIS,ED_APURCOF,ED_PCAPPIS,ED_PCAPCOF,ED_REDPIS,ED_REDCOF,ED_CLASFIS,ED_TPREG,ED_INDCMLT,ED_CSTPIS,ED_CSTCOF,ED_RECDAC,ED_JURSPD,ED_F100,ED_TABCCZ,ED_CODCCZ,ED_GRUCCZ"

			//Campos a serem exibidos no Browser
			cCpoBrow	:="'ED_CODIGO','ED_DESCRIC','ED_CALCISS','ED_CALCPIS','ED_PERCPIS','ED_CALCCOF','ED_PERCCOF','ED_CALCCSL','ED_PERCCSL','ED_CALCIRF','ED_PERCIRF','ED_CALCINS','ED_PERCINS'"
			
			UpdSX3("1","'SED'",cCpoAba1) 
			UpdSX3("2","'SED'",cCpoAba2)
			UpdSX3("3","'SED'",cCpoAba3)
			UpdSX3("4","'SED'",cCpoAba4)
			UpdSX3("5","'SED'",cCpoAba5)
			UpdSX3("6","'SED'",cCpoAba6)
			UpdSX3("7","'SED'",cCpoAba7)
			
			UpdSX3B("'SED'",cCpoBrow)
			
							
		  	//SE ENCONTROU ALGUM ERRO AO EFETUAR A EXCLUSaO DOS REGISTROS DA SXA, EXIBE O ERRO A CANCELA A OPERAaaO
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
				MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
				Return
			EndIf
		  	  	 
		End Transaction
		//MsgInfo("Atualizacao efetuada com sucesso!")
	//EndIf
	
Return

//******************************************************************************************************************
// Atualiza SX3 - PASTAS E ORGANIZAaaO DOS CAMPOS -  Igor Mateus 11/2021
//******************************************************************************************************************

//Funcao que altera a pasta dos campos -- Gabriel Lucas
Static Function UpdSX3(cFolder,cAlias,cCampos)
	Local aCampos:= Separa(cCampos, ',', .F.)
	Local nX:= 0

	For nX:= 1 to Len(aCampos)

	cAtuCampo := Alltrim(aCampos[nX])

			SX3->(DBSetOrder(2))

		if 	SX3->(DBSeek(cAtuCampo))

			//Alteracao de pasta do campo pela SX3
			RecLock('SX3', .F.)
				X3_FOLDER := cFolder
			SX3->(MsUnlock())

		endif

	Next

Return

Static Function UpdSX3B(strTab,strCampos)
Local aErros 	:= {}
Local cErro		:=""

	//ATUALIZA SX3 PARA A TABELA SF4 PARA Nao EXIBIR NENHUM CAMPO NO BROWSE (Comentario atualizado)
	//ATUALIZA SX3 PARA Nao EXIBIR NENHUM CAMPO NO BROWSE

	strSQL:="UPDATE SX3"+cEmpresa+" SET X3_BROWSE = 'N' WHERE X3_ARQUIVO IN ("+strTab+")"
	nResult := TCSQLEXEC(strSQL)
	If nResult <> 0
		cErro   := " Erro:"+TCSqlError()
   		AADD(aErros,cErro)
  	Else
   		cErro := ""
  	EndIf
  	
  	//ATUALIZA SX3 PARA A TABELA SF4 PARA EXIBIR SOMENTE OS ALGUNS CAMPO NO BROWSE (Comentario atualizado)
	//ATUALIZA SX3 PARA EXIBIR SOMENTE OS ALGUNS CAMPO NO BROWSE

	strSQL:="UPDATE SX3"+cEmpresa+" SET X3_BROWSE = 'S' WHERE X3_ARQUIVO IN ("+strTab+") AND X3_CAMPO IN ("+strCampos+")"
	nResult := TCSQLEXEC(strSQL)
	If nResult <> 0
		cErro   := " Erro:"+TCSqlError()
   		AADD(aErros,cErro)
  	Else
   		cErro := ""
  	EndIf
  	
Return

//******************************************************************************************************************
// Cria Gatilhos com base nos dados do Vetor -  Igor Mateus 11/2021
//******************************************************************************************************************


Static Function AtuGatilho()

	// Qualquer adiaao de elementos, incrementar o valor dentro do Array e na declaraaao.

	aGatilho := Array(32, 11)

    //					X7_CAMPO,    X7_SEQUENC,        X7_REGRA,   												X7_CDOMIN,		 X7_TIPO,    	 X7_SEEK,  X7_ALIAS,    X7_ORDEM,	X7_CHAVE, 		                               X7_CONDIC,  X7_PROPRI
    aGatilho[1] := {'C5_CLIENTE',  '','Posicione("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_NOME")','C5_X_NOMCL','P','N','',0,'','','U'}
	aGatilho[2] := {'C5_CLIENTE',  '','Posicione("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_END")','C5_X_END','P','N','',0,'','','U'}
	aGatilho[3] := {'C5_CLIENTE',  '','Posicione("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_MUN")','C5_X_MUNE','P','N','',0,'','','U'}
	aGatilho[4] := {'C5_CLIENTE',  '','Posicione("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_BAIRRO")','C5_X_BAIRR','P','N','',0,'','','U'}
	aGatilho[5] := {'C5_CLIENTE',  '','Posicione("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_EST")','C5_X_EST','P','N','',0,'','','U'}
	aGatilho[6] := {'C5_TPCARGA',  '','IF(M->C5_TPCARGA=1,"C","F")','C5_TPFRETE','P','N','',0,'','','U'}
	aGatilho[7] := {'C5_CLIENTE',  '','SA3->A3_COD','C5_VEND1','P','S','SA3',7,'XFILIAL("SA3")+RETCODUSR()','EMPTY(M->C5_VEND1)','U'}
	aGatilho[8] := {'C5_CLIENTE',  '','SA2->A2_END','C5_X_END','P','S','SA2',1,'xFilial("SA2")+M->C5_CLIENT+M->C5_LOJAENT','M->C5_TIPO$"DB"','U'}
	aGatilho[9] := {'C5_CLIENTE',  '','SA2->A2_EST','C5_X_EST','P','S','SA2',1,'xFilial("SA2")+M->C5_CLIENT+M->C5_LOJAENT','M->C5_TIPO$"DB"','U'}
	aGatilho[10] := {'C5_CLIENTE',  '','SA2->A2_EST','C5_X_BAIRR','P','S','SA2',1,'xFilial("SA2")+M->C5_CLIENT+M->C5_LOJAENT','M->C5_TIPO$"DB"','U'}
	aGatilho[11] := {'D1_QUANT','', 'NoRound(M->D1_VUNIT*M->D1_QUANT,2)','D1_TOTAL','P','N','',0,'','','U'}
	aGatilho[12] := {'C5_CLIENTE',  '','SA2->A2_EST','C5_X_MUNE','P','S','SA2',1,'xFilial("SA2")+M->C5_CLIENT+M->C5_LOJAENT','M->C5_TIPO$"DB"','U'}
	aGatilho[13] := {'C5_CLIENTE',  '','M->C5_CLIENTE','C5_CLIENT','P','N','',0,'','','U'}
	aGatilho[14] := {'C5_CLIENTE',  '','"01"','C5_LOJAENT','P','N','',0,'','','U'}
	aGatilho[15] := {'C5_LOJACLI',  '','SA3->A3_COD','C5_VEND1','P','S','SA3',7,'XFILIAL("SA3")+RETCODUSR()','EMPTY(M->C5_VEND1)','U'}
	aGatilho[16] := {'C5_CLIENT',   '','"01"','C5_LOJAENT','P','N','',0,'','','U'}
	aGatilho[17] := {'C5_LOJAENT',  '','SA1->A1_END','C5_X_END','P','S','SA1',1,'xFilial("SA1")+M->C5_CLIENT+M->C5_LOJAENT','','U'}
	aGatilho[18] := {'C5_LOJAENT',  '','SA1->A1_BAIRRO','C5_X_BAIRR','P','S','SA1',1,'xFilial("SA1")+M->C5_CLIENT+M->C5_LOJAENT','','U'}
	aGatilho[19] := {'C5_LOJAENT',  '','SA1->A1_EST','C5_X_EST','P','S','SA1',1,'xFilial("SA1")+M->C5_CLIENT+M->C5_LOJAENT','','U'}
	aGatilho[20] := {'C5_LOJAENT',  '','SA1->A1_MUN','C5_X_MUNE','P','S','SA1',1,'xFilial("SA1")+M->C5_CLIENT+M->C5_LOJAENT','','U'}
	aGatilho[21] := {'C5_LOJAENT',  '','SA2->A2_END','C5_X_END','P','S','SA2',1,'xFilial("SA2")+M->C5_CLIENT+M->C5_LOJAENT','M->C5_TIPO$"DB"','U'}
	aGatilho[22] := {'C5_LOJAENT',  '','SA2->A2_BAIRRO','C5_X_BAIRR','P','S','SA2',1,'xFilial("SA2")+M->C5_CLIENT+M->C5_LOJAENT','M->C5_TIPO$"DB"','U'}
	aGatilho[23] := {'C5_LOJAENT',  '','SA2->A2_EST','C5_X_EST','P','S','SA2',1,'xFilial("SA2")+M->C5_CLIENT+M->C5_LOJAENT','M->C5_TIPO$"DB"','U'}
	aGatilho[24] := {'C5_LOJAENT',  '','SA2->A2_MUN','C5_X_MUNE','P','S','SA2',1,'xFilial("SA2")+M->C5_CLIENT+M->C5_LOJAENT','M->C5_TIPO$"DB"','U'}
	aGatilho[25] := {'D1_VUNIT',  '','M->D1_TOTAL:=NoRound(M->D1_QUANT*M->D1_VUNIT,TamSX3("D1_TOTAL")[2])','D1_TOTAL','P','N','',0,'','','U'}
	aGatilho[26] := {'D1_VUNIT',  '','If(A103Trigger("D1_TOTAL"),M->D1_TOTAL,CriaVar("D1_TOTAL"))','D1_TOTAL','P','N','',0,'','','U'}
	aGatilho[27] := {'D1_COD',  '','U_RETSB1()','D1_X_DESC','P','N','',0,'','','U'}
	aGatilho[28] := {'CK_PRODUTO',  '','Posicione("SB1",1,xFilial("SB1")+M->CK_PRODUTO,"B1_DESC")','CK_X_DSCPD','P','N','',0,'','','U'}
	aGatilho[29] := {'C7_PRODUTO',  '','Posicione("SB1",1,xFilial("SB1")+M->C7_PRODUTO,"B1_DESC")','C7_DESCRI','P','N','',0,'','','U'}
	aGatilho[30] := {'CJ_CLIENTE',  '','Posicione("SA1",1,xFilial("SA1")+M->CJ_CLIENTE+M->CJ_LOJA,"A1_X_FPGT")','CJ_X_FPGT','P','N','',0,'','','U'}
	aGatilho[31] := {'C5_CLIENTE',  '','Posicione("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_X_FPGT")','C5_X_FPGT','P','N','',0,'','','U'}
	aGatilho[32] := {'F4_X_OPER',  '','Posicione("SX5",1,xFilial("SX5")+"DJ"+M->F4_X_OPER,"X5_DESCRI")','F4_X_DESOP','P','N','',0,'','','U'}
 	
	CriaGatilho()

Return
//******************************************************************************************************************
// Cria Gatilhos com base nos dados do Vetor -  Igor Mateus 11/2021
//******************************************************************************************************************

Static Function CriaGatilho()
	Local strOrdem :=	""
	Local IntCont := 1

		

		For IntCont := 1 to Len(aGatilho)

			// STRING SQL BUSCA CAMPO, CONTRADOMINIO E ALIAS PARA VERIFICAR SE Ja Nao EXISTE GATILHO CRIADO

			strSQL:="SELECT * FROM SX7"+cEmpresa+" WHERE X7_CAMPO = '"+ aGatilho[IntCont][1]+"' AND X7_CDOMIN =  '"+ aGatilho[IntCont][4]+"' AND X7_ALIAS = '"+ aGatilho[IntCont][7]+"' AND D_E_L_E_T_ <> '*' "

			If Select("QRY") > 0 
			QRY->(dbCloseArea())
			Endif 

			TcQuery strSQL NEW Alias "QRY"
			
	//TRAZ O RESULTADO DA CONSULTA E CHECA COMPARANDO 2 CAMPOs, SE Ha INFORMAaaO. SE Nao HOUVER, CRIA GATILHO.

		If Alltrim(QRY->X7_CAMPO) != aGatilho[IntCont][1] .or.;
		   Alltrim(QRY->X7_CAMPO) == aGatilho[IntCont][1] .and.;
		   Alltrim(QRY->X7_REGRA) != aGatilho[IntCont][3]

				//Busca a ultima ordem de indice na tabela SC5

				strSQL:="SELECT MAX(X7_SEQUENC) AS SOMA FROM SX7"+cEmpresa+" WHERE X7_CAMPO = '"+ aGatilho[IntCont][1]+"' AND D_E_L_E_T_ <> '*' "

				If Select("QRY") > 0 
					QRY->(dbCloseArea())
				Endif 

				TcQuery strSQL NEW Alias "QRY"

				IF !QRY->(eOF())

					strOrdem := SOMA1(QRY->SOMA)
			
					reclock('SX7',.T.)
					X7_CAMPO  	:=  aGatilho[IntCont][1]      
					X7_SEQUENC  :=  strOrdem
					X7_REGRA  	:=  aGatilho[IntCont][3]   																	
					X7_CDOMIN  	:=  aGatilho[IntCont][4]		
					X7_TIPO  	:=  aGatilho[IntCont][5]   	 
					X7_SEEK  	:=  aGatilho[IntCont][6]  
					X7_ALIAS  	:=  aGatilho[IntCont][7]    
					X7_ORDEM  	:=  aGatilho[IntCont][8]	
					X7_CHAVE  	:=  aGatilho[IntCont][9] 		
					X7_CONDIC  	:=  aGatilho[IntCont][10]  
					X7_PROPRI  	:=  aGatilho[IntCont][11]	
					msunlock()

				ENDIF
		ENDIF

		Next
	

Return 

// Funcao que Cria Tabelas Genericas na SX5 -  Gabriel Lucas 08/2022
 Static Function CriaSX5()
	Local aCampoSX5 := {} // Campos que serao inseridos
	Local nX:= 0

	//Adicionando dados

	//Tabela de Prefixos ( CONTAS A RECEBER E A PAGAR)
							//TABELA/CHAVE/DESCRICAO/DESC. ESP./DESC. INGLES
	AAdd(aCampoSX5,{xFilial('SX5'),'00','ZY','*Prefixos','*Prefixos','*Prefixos'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','1','1','1','1'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','2','2','2','2'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','MAN','MANUAL','MANUAL','MANUAL'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','BOL','BOLETO','BOLETO','BOLETO'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','DES','DESPESA','DESPESA','DESPESA'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','IMP','IMPORTADOS','IMPORTADOS','IMPORTADOS'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','NF','NOTA FISCAL','NOTA FISCAL','NOTA FISCAL'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','RA','RECEBIMENTO ANTECIPADO','R. ANTECIPADO','R. ANTECIPADO'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','PA','PAGAMENTO ANTECIPADO','P. ANTECIPADO','P. ANTECIPADO'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','COM','COMISSOES','COMISSOES','COMISSOES'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','LIQ','LIQUIDACAO DE TITULOS','LIQUIDACAO DE TITULOS','LIQUIDACAO DE TITULOS'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','CH ','CHEQUE','CHEQUE','CHEQUE'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','001','SERIE DE NF','SERIE DE NF','SERIE DE NF'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZY','002','SERIE DE NF','SERIE DE NF','SERIE DE NF'})

	//Tabela de Grupo de Tributacao ( Clientes e Fornecedores)
							//TABELA/CHAVE/DESCRICAO/DESC. ESP./DESC. INGLES
	AAdd(aCampoSX5,{xFilial('SX5'),'00','ZZ','Grupos de Cli/For','*Grupos de Cli/For','Grupos de Cli/For'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZZ','001','Grupo Padrao de Clientes','Grupo Padrao de Clientes','Grupo Padrao de Clientes'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZZ','002','Grupo Padrao de Fornecedores','Grupo Padrao de Fornecedores','Grupo Padrao de Fornecedores'})
	AAdd(aCampoSX5,{xFilial('SX5'),'ZZ','A01','Grupo Alternativo de Clientes','Grupo Alternativo de Clientes','Grupo Alternativo de Clientes'})

	Begin Transaction

	for nX := 1 to Len(aCampoSX5)

		SX5->(dbSetOrder(1))

		//Inserindo tabelas genericas personalizadas na SX5

		if !SX5->(DBSeek(aCampoSX5[nX,1]+aCampoSX5[nX,2]+aCampoSX5[nX,3]))
			
			RecLock('SX5', .T.)

				X5_FILIAL := aCampoSX5[nX,1]
				X5_TABELA := aCampoSX5[nX,2]
				X5_CHAVE  := aCampoSX5[nX,3]
				X5_DESCRI  := aCampoSX5[nX,4]
				X5_DESCSPA := aCampoSX5[nX,5]
				X5_DESCENG := aCampoSX5[nX,6]
				
			SX5->(MsUnlock())

		endif	
	next

	End Transaction

Return 

//******************************************************************************************************************
// Atualiza Consulta Padrao "SE" Grupo de campos for igual aos dados mencionados -  Igor Mateus 11/2021
//******************************************************************************************************************

Static Function AtuConsulta()
	
	aConsulta 	:= Array(6, 55) //Linha e Coluna - Para novos campos incrementar somente o primeiro numero

                      //XB_ALIAS, XB_TIPO, XB_SEQ, XB_COLUNA, XB_DESCRI, XB_DESCSPA, XB_DESCENG, XB_CONTEM, XB_WCONTEM
    aConsulta[1]  := {'SB1NOV','1','01','RE','Produtos Nova','Produtos Nova','Produtos Nova','SB1', '', 'SB1NOV','2', '01', '01','','','','u_pesqprod()','','SB1NOV','5','01','','','','','SB1->B1_COD','','*'}
	aConsulta[2]  := {'SA1NOV','1','01','RE','Clientes Nova','Clientes Nova','Clientes Nova','SA1', '', 'SA1NOV','2', '01', '01','','','','u_pesqcli()' ,'','SA1NOV','5','01','','','','','SA1->A1_COD','','SA1NOV','5','02','','','','','SA1->A1_LOJA','','*'}
	aConsulta[3]  := {'SA2NOV','1','01','RE','Forneced Nova','Forneced Nova','Forneced Nova','SA2', '', 'SA2NOV','2', '01', '01','','','','u_pesqfor()' ,'','SA2NOV','5','01','','','','','SA2->A2_COD','','SA2NOV','5','02','','','','','SA2->A2_LOJA','','*'}
	aConsulta[4]  := {'X5DJ','1','01','DB','Tipo de Operacao','Tipo de Operacao','Tipo de Operacao','SX5', '', 'X5DJ','2', '01', '01','Tabela + Chave','Tabla + Clave','Table + Key','' ,'','X5DJ','4','01','01','Chave','Clave','Key','X5_CHAVE','','X5DJ','4','01','02','Descricao','Descripcion','Description','X5_DESCRI','','X5DJ','5','01','','','','','SX5->X5_CHAVE','','X5DJ','6','01','','','','','DJ','','*'}
	aConsulta[5]  := {'PRFX','1','01','DB','*Prefixos','*Prefixos','*Prefixos','SX5', '', 'PRFX','2', '01', '01','Tabela + Chave','Tabla + Clave','Table + Key','' ,'','PRFX','4','01','01','Chave','Clave','Key','X5_CHAVE','','PRFX','4','01','02','Descricao','Descripcion','Description','X5_DESCRI','','PRFX','5','01','','','','','SX5->X5_CHAVE','','PRFX','6','01','','','','','ZY','','*'}
	aConsulta[6]  := {'GRPTRB','1','01','DB','*Grupos de Cli/For','*Grupos de Cli/For','*Grupos de Cli/For','SX5', '', 'GRPTRB','2', '01', '01','Tabela + Chave','Tabla + Clave','Table + Key','' ,'','GRPTRB','4','01','01','Chave','Clave','Key','X5_CHAVE','','GRPTRB','4','01','02','Descricao','Descripcion','Description','X5_DESCRI','','GRPTRB','5','01','','','','','SX5->X5_CHAVE','','GRPTRB','6','01','','','','','ZZ','','*'}

//******************************************************************************************************************
// Atualiza Consulta Padrao "SE" Grupo de campos for igual aos dados mencionados -  Igor Mateus 11/2021
//******************************************************************************************************************

	CriaConsulta()

	aTrocaCons		:= Array(3, 2) //Linha e Coluna - Para novos campos incrementar somente o primeiro numero
	aTrocaCons[1]	:={'SB1','SB1NOV'}
	aTrocaCons[2]	:={'SA1','SA1NOV'}
	aTrocaCons[3]	:={'SA2','SA2NOV'}
	TrocaConsu()

Return

//******************************************************************************************************************
// Atualiza Consulta Padrao "SE" Grupo de campos for igual aos dados mencionados -  Igor Mateus 01/12/2021
//******************************************************************************************************************

Static Function CriaConsulta()
	Local nCont
	Local IntCont :=1

		For IntCont := 1 to Len(aConsulta)
			nCont := 0
		
					
			strSQL:="SELECT * FROM SXB"+cEmpresa+" WHERE XB_ALIAS = '"+aConsulta[IntCont][1+nCont]+"' AND XB_CONTEM = '"+aConsulta[IntCont][8+nCont]+"' AND D_E_L_E_T_ <> '*' "

			If Select("QRY") > 0 
			QRY->(dbCloseArea())
			Endif 

			TcQuery strSQL NEW Alias "QRY"

			IF QRY->XB_ALIAS != aConsulta[IntCont][1+nCont]

				While nCont >= 0
					reclock('SXB',.T.)
					XB_ALIAS  	:=  aConsulta[IntCont][1+nCont]      
					XB_TIPO  	:=  aConsulta[IntCont][2+nCont]
					XB_SEQ  	:=  aConsulta[IntCont][3+nCont]   																	
					XB_COLUNA  	:=  aConsulta[IntCont][4+nCont]		
					XB_DESCRI  	:=  aConsulta[IntCont][5+nCont]   	 
					XB_DESCSPA  :=  aConsulta[IntCont][6+nCont]  
					XB_DESCENG  :=  aConsulta[IntCont][7+nCont]    
					XB_CONTEM  	:=  aConsulta[IntCont][8+nCont]	
					XB_WCONTEM  :=  aConsulta[IntCont][9+nCont] 		
					msunlock()

					If aConsulta[IntCont][10+nCont] == '*'
						nCont := -1
					Else
						nCont += 9
					EndIf
				EndDo
			END IF
		Next

Return


//******************************************************************************************************************
// Atualiza Consulta Padrao "SE" Grupo de campos for igual aos dados mencionados -  Igor Mateus 11/2021
//******************************************************************************************************************


Static Function TrocaConsu()
Local IntCont := 1

	For IntCont := 1 to Len(aTrocaCons)
		TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = '"+ aTrocaCons[IntCont][2] +"' WHERE X3_F3 = '"+ aTrocaCons[IntCont][1] +" AND X3_CAMPO <> 'G1_COD' AND X3_CAMPO <> 'G1_COMP' AND D_E_L_E_T_ <> '*' ") 
	Next

Return


//*********************************************************************************************************************
// Troca Decimais de inameros campos dos modulos Protheus com base no Apontado dentro do Wizard -  Igor Mateus 11/2021
//*********************************************************************************************************************

Static Function AtuDecim()

	Local IntCont := 1	
	Local aDeci := {}
	Local aDeci2 := {}
	Local cTamanho := 16
	Local cTIpo := "N"
	Local cMascara
	Local cDecimal
	Local cDecimal2
	Local cMascara2
	Local cQueryX
	Local cQueryY
	Local aComp1
	Local aComp2

    DO CASE

		CASE cbGer1 == "2"
			cMascara := "@E 9999999999999.99"
			cDecimal := 2

        CASE cbGer1 == "4"
			cMascara := "@E 99999999999.9999"
			cDecimal := 4

        CASE cbGer1 == "6"	
			cMascara := "@E 999999999.999999"
			cDecimal := 6
		
		CASE cbGer1 == "8"	
			cMascara := "@E 9999999.99999999"
			cDecimal := 8

    END CASE

    DO CASE

		CASE cbGer2 == "2"
			cMascara2 := "@E 9,999,999,999,999.99"
			cDecimal2 := 2

        CASE cbGer2 == "4"
			cMascara2 := "@E 99,999,999,999.9999"
			cDecimal2 := 4

        CASE cbGer2 == "6"	
			cMascara2 := "@E 999,999,999.999999"
			cDecimal2 := 6

		CASE cbGer2 == "8"	
			cMascara2 := "@E 9999,999.99999999"
			cDecimal2 := 8

    END CASE

	// INICIO DOS VETORES COM OS CAMPOS A SEREM ATUALIZADOS (INCLUSaES, COLOCAR AQUI) // @igor.mateus - 02/12/2021

	aadd(aDeci,{'ADZ_QTDVEN'})
	aadd(aDeci,{'AF2_QUANT'})
	aadd(aDeci,{'AF3_QUANT'})
	aadd(aDeci,{'AFA_QUANT'})
	aadd(aDeci,{'AFC_QUANT'})
	aadd(aDeci,{'B1_EMIN'})
	aadd(aDeci,{'B1_ESTSEG'})
	aadd(aDeci,{'B1_LE'})
	aadd(aDeci,{'B1_LM'})
	aadd(aDeci,{'B2_NAOCLAS'})
	aadd(aDeci,{'B2_QACLASS'})
	aadd(aDeci,{'B2_QATU'})
	aadd(aDeci,{'B2_QEMP'})
	aadd(aDeci,{'B2_QEMP2'})
	aadd(aDeci,{'B2_QEMPN'})
	aadd(aDeci,{'B2_QEMPN2'})
	aadd(aDeci,{'B2_QEMPPR2'})
	aadd(aDeci,{'B2_QEMPPRE'})
	aadd(aDeci,{'B2_QEMPPRJ'})
	aadd(aDeci,{'B2_QEMPSA'})
	aadd(aDeci,{'B2_QEPRE2'})
	aadd(aDeci,{'B2_QFIM'})
	aadd(aDeci,{'B2_QFIM2'})
	aadd(aDeci,{'B2_QFIMFF'})
	aadd(aDeci,{'B2_QNPT'})
	aadd(aDeci,{'B2_QPEDVE2'})
	aadd(aDeci,{'B2_QPEDVEN'})
	aadd(aDeci,{'B2_QTER'})
	aadd(aDeci,{'B2_QTNP'})
	aadd(aDeci,{'B2_QTSEGUM'})
	aadd(aDeci,{'B2_RESERV2'})
	aadd(aDeci,{'B2_RESERVA'})
	aadd(aDeci,{'B2_SALPED2'})
	aadd(aDeci,{'B2_SALPEDI'})
	aadd(aDeci,{'B2_SALPPRE'})
	aadd(aDeci,{'B6_QTSEGUM'})
	aadd(aDeci,{'B6_QUANT'})
	aadd(aDeci,{'B6_QULIB'})
	aadd(aDeci,{'B6_SALDO'})
	aadd(aDeci,{'B7_QTSEGUM'})
	aadd(aDeci,{'B7_QUANT'})
	aadd(aDeci,{'B8_EMPENH2'})
	aadd(aDeci,{'B8_EMPENHO'})
	aadd(aDeci,{'B8_QACLAS2'})
	aadd(aDeci,{'B8_QACLASS'})
	aadd(aDeci,{'B8_QEMPPRE'})
	aadd(aDeci,{'B8_QEPRE2'})
	aadd(aDeci,{'B8_QTDORI'})
	aadd(aDeci,{'B8_QTDORI2'})
	aadd(aDeci,{'B8_SALDO'})
	aadd(aDeci,{'B9_QINI'})
	aadd(aDeci,{'B9_QISEGUM'})
	aadd(aDeci,{'BC_QTDDES2'})
	aadd(aDeci,{'BC_QTDDEST'})
	aadd(aDeci,{'BC_QTSEGUM'})
	aadd(aDeci,{'BC_QUANT'})
	aadd(aDeci,{'BD_QFIM'})
	aadd(aDeci,{'BD_QFIM2UM'})
	aadd(aDeci,{'BD_QINI'})
	aadd(aDeci,{'BD_QINI2UM'})
	aadd(aDeci,{'BD_QT2UM'})
	aadd(aDeci,{'BD_QUANT'})
	aadd(aDeci,{'BF_EMPEN2'})
	aadd(aDeci,{'BF_EMPENHO'})
	aadd(aDeci,{'BF_QEMPPRE'})
	aadd(aDeci,{'BF_QEPRE2'})
	aadd(aDeci,{'BF_QTSEGUM'})
	aadd(aDeci,{'BF_QUANT'})
	aadd(aDeci,{'BJ_QINI'})
	aadd(aDeci,{'BJ_QISEGUM'})
	aadd(aDeci,{'BK_QINI'})
	aadd(aDeci,{'BK_QISEGUM'})
	aadd(aDeci,{'C0_QTDELIM'})
	aadd(aDeci,{'C0_QTDORIG'})
	aadd(aDeci,{'C0_QTDPED'})
	aadd(aDeci,{'C0_QUANT'})
	aadd(aDeci,{'C1_QTDORIG'})
	aadd(aDeci,{'C1_QTSEGUM'})
	aadd(aDeci,{'C1_QUANT'})
	aadd(aDeci,{'C1_QUJE'})
	aadd(aDeci,{'C1_QUJE2'})
	aadd(aDeci,{'C2_PERDA'})
	aadd(aDeci,{'C2_QTSEGUM'})
	aadd(aDeci,{'C2_QTUPROG'})
	aadd(aDeci,{'C2_QUANT'})
	aadd(aDeci,{'C2_QUJE'})
	aadd(aDeci,{'C3_QTSEGUM'})
	aadd(aDeci,{'C3_QUANT'})
	aadd(aDeci,{'C3_QUJE'})
	aadd(aDeci,{'C4_QUANT'})
	aadd(aDeci,{'C6_QTDEMP'})
	aadd(aDeci,{'C6_QTDEMP2'})
	aadd(aDeci,{'C6_QTDENT'})
	aadd(aDeci,{'C6_QTDENT2'})
	aadd(aDeci,{'C6_QTDLIB'})
	aadd(aDeci,{'C6_QTDLIB2'})
	aadd(aDeci,{'C6_QTDRESE'})
	aadd(aDeci,{'C6_QTDVEN'})
	aadd(aDeci,{'C6_SLDALIB'})
	aadd(aDeci,{'C7_QTDACLA'})
	aadd(aDeci,{'C7_QTDSOL'})
	aadd(aDeci,{'C7_QTSEGUM'})
	aadd(aDeci,{'C7_QUANT'})
	aadd(aDeci,{'C7_QUJE'})
	aadd(aDeci,{'C8_QTDCTR'})
	aadd(aDeci,{'C8_QTSEGUM'})
	aadd(aDeci,{'C8_QUANT'})
	aadd(aDeci,{'C9_QTDLIB'})
	aadd(aDeci,{'C9_QTDLIB2'})
	aadd(aDeci,{'C9_QTDRESE'})
	aadd(aDeci,{'CK_QTDVEN'})
	aadd(aDeci,{'CL_QUANT'})
	aadd(aDeci,{'CNB_QUANT'})
	aadd(aDeci,{'CNE_QTAMED'})
	aadd(aDeci,{'CNE_QTDSOL'})
	aadd(aDeci,{'CNE_QUANT'})
	aadd(aDeci,{'CP_QTSEGUM'})
	aadd(aDeci,{'CP_QUANT'})
	aadd(aDeci,{'CP_QUJE'})
	aadd(aDeci,{'CQ_QTDISP'})
	aadd(aDeci,{'CQ_QTSEGUM'})
	aadd(aDeci,{'CQ_QUANT'})
	aadd(aDeci,{'CY_QTACLA'})
	aadd(aDeci,{'CY_QTDSOL'})
	aadd(aDeci,{'CY_QTSEGUM'})
	aadd(aDeci,{'CY_QUANT'})
	aadd(aDeci,{'CY_QUJE'})
	aadd(aDeci,{'D1_QTDEDEV'})
	aadd(aDeci,{'D1_QTDPEDI'})
	aadd(aDeci,{'D1_QTSEGUM'})
	aadd(aDeci,{'D1_QUANT'})
	aadd(aDeci,{'D2_QTDAFAT'})
	aadd(aDeci,{'D2_QTDEDEV'})
	aadd(aDeci,{'D2_QTDEFAT'})
	aadd(aDeci,{'D2_QTSEGUM'})
	aadd(aDeci,{'D2_QUANT'})
	aadd(aDeci,{'D3_PERDA'})
	aadd(aDeci,{'D3_QTSEGUM'})
	aadd(aDeci,{'D3_QUANT'})
	aadd(aDeci,{'D4_QSUSP'})
	aadd(aDeci,{'D4_QTDEORI'})
	aadd(aDeci,{'D4_QTSEGUM'})
	aadd(aDeci,{'D4_QUANT'})
	aadd(aDeci,{'D5_QTSEGUM'})
	aadd(aDeci,{'D5_QUANT'})
	aadd(aDeci,{'D6_QUANT'})
	aadd(aDeci,{'D7_QTDE'})
	aadd(aDeci,{'D7_QTSEGUM'})
	aadd(aDeci,{'D8_QFIMDEV'})
	aadd(aDeci,{'D8_QT2UM'})
	aadd(aDeci,{'D8_QTDDEV'})
	aadd(aDeci,{'D8_QUANT'})
	aadd(aDeci,{'DA_EMP2'})
	aadd(aDeci,{'DA_EMPENHO'})
	aadd(aDeci,{'DA_QTDORI'})
	aadd(aDeci,{'DA_QTDORI2'})
	aadd(aDeci,{'DA_QTSEGUM'})
	aadd(aDeci,{'DB_EMP2'})
	aadd(aDeci,{'DB_EMPENHO'})
	aadd(aDeci,{'DB_QTSEGUM'})
	aadd(aDeci,{'DB_QUANT'})
	aadd(aDeci,{'DC_QTDORIG'})
	aadd(aDeci,{'DC_QTSEGUM'})
	aadd(aDeci,{'DC_QUANT'})
	aadd(aDeci,{'FT_QUANT'})
	aadd(aDeci,{'H6_QTDPERD'})
	aadd(aDeci,{'H6_QTDPRO2'})
	aadd(aDeci,{'H6_QTDPROD'})
	aadd(aDeci,{'UB_QUANT'})
	aadd(aDeci,{'H6_QTDPERD'})
	aadd(aDeci,{'H6_QTDPRO2'})
	aadd(aDeci,{'H6_QTDPROD'})
	aadd(aDeci,{'HC_QUANT'})
	aadd(aDeci,{'L2_QTDEDEV'})
	aadd(aDeci,{'L2_QUANT'})
	aadd(aDeci,{'LR_QTDEDEV'})
	aadd(aDeci,{'LR_QUANT'})
	aadd(aDeci,{'TJ_QTDREP'})
	aadd(aDeci,{'TL_QUANTID'})
	aadd(aDeci,{'TVE_QTDE'})
	aadd(aDeci,{'UB_QUANT'})

	aadd(aDeci,{'ND1_QTDE'})
    aadd(aDeci,{'TT_QUANTID'})
    aadd(aDeci,{'GV6_QTMIN'})
    aadd(aDeci,{'GV7_QTFXFI'})
    aadd(aDeci,{'GV9_QTKGM3'})
    aadd(aDeci,{'GW8_QTDALT'})
    aadd(aDeci,{'GWI_QTCALC'})
    aadd(aDeci,{'GWN_CQTDE'})
    aadd(aDeci,{'F2_PLIQUI'}) 
    aadd(aDeci,{'F2_PBRUTO'})
    aadd(aDeci,{'F2_VOLUME1'}) 
    aadd(aDeci,{'B1_PESO'})
    aadd(aDeci,{'B1_PESBRU'})
    aadd(aDeci,{'D2_PESO'})
    aadd(aDeci,{'C5_VOLUME1'})
    aadd(aDeci,{'C5_VOLUME2'})
    aadd(aDeci,{'C5_VOLUME3'})
    aadd(aDeci,{'C5_VOLUME4'})
    aadd(aDeci,{'C5_VOLUME5'})
    aadd(aDeci,{'F2_VOLUME2'})
    aadd(aDeci,{'F2_VOLUME3'})
    aadd(aDeci,{'F2_VOLUME4'})
    aadd(aDeci,{'F2_VOLUME5'})
	
	aadd(aDeci2,{'B0_PRV1'})
	aadd(aDeci2,{'B0_PRV2'})
	aadd(aDeci2,{'B0_PRV3'})
	aadd(aDeci2,{'B0_PRV4'})
	aadd(aDeci2,{'B0_PRV5'})
	aadd(aDeci2,{'B0_PRV6'})
	aadd(aDeci2,{'B0_PRV7'})
	aadd(aDeci2,{'B0_PRV8'})
	aadd(aDeci2,{'B0_PRV9'})
	aadd(aDeci2,{'B1_CUSTD'})
	aadd(aDeci2,{'B1_UPRC'})
	aadd(aDeci2,{'B1_UVLRC'})
	aadd(aDeci2,{'B2_CM1'})
	aadd(aDeci2,{'B2_CM2'})
	aadd(aDeci2,{'B2_CM3'})
	aadd(aDeci2,{'B2_CM4'})
	aadd(aDeci2,{'B2_CM5'})
	aadd(aDeci2,{'B2_CMFIM1'})
	aadd(aDeci2,{'B2_CMFIM2'})
	aadd(aDeci2,{'B2_CMFIM3'})
	aadd(aDeci2,{'B2_CMFIM4'})
	aadd(aDeci2,{'B2_CMFIM5'})
	aadd(aDeci2,{'B2_CMRP1'})
	aadd(aDeci2,{'B2_CMRP2'})
	aadd(aDeci2,{'B2_CMRP3'})
	aadd(aDeci2,{'B2_CMRP4'})
	aadd(aDeci2,{'B2_CMRP5'})
	aadd(aDeci2,{'B2_VATU1'})
	aadd(aDeci2,{'B2_VATU2'})
	aadd(aDeci2,{'B2_VATU3'})
	aadd(aDeci2,{'B2_VATU4'})
	aadd(aDeci2,{'B2_VATU5'})
	aadd(aDeci2,{'B2_VFIM1'})
	aadd(aDeci2,{'B2_VFIM2'})
	aadd(aDeci2,{'B2_VFIM3'})
	aadd(aDeci2,{'B2_VFIM4'})
	aadd(aDeci2,{'B2_VFIM5'})
	aadd(aDeci2,{'B2_VFRP1'})
	aadd(aDeci2,{'B2_VFRP2'})
	aadd(aDeci2,{'B2_VFRP3'})
	aadd(aDeci2,{'B2_VFRP4'})
	aadd(aDeci2,{'B2_VFRP5'})
	aadd(aDeci2,{'B4_PRV1'})
	aadd(aDeci2,{'B4_PRV2'})
	aadd(aDeci2,{'B4_PRV3'})
	aadd(aDeci2,{'B4_PRV4'})
	aadd(aDeci2,{'B4_PRV5'})
	aadd(aDeci2,{'B4_PRV6'})
	aadd(aDeci2,{'B4_PRV7'})
	aadd(aDeci2,{'B5_PRV2'})
	aadd(aDeci2,{'B5_PRV3'})
	aadd(aDeci2,{'B5_PRV4'})
	aadd(aDeci2,{'B5_PRV5'})
	aadd(aDeci2,{'B5_PRV6'})
	aadd(aDeci2,{'B5_PRV7'})
	aadd(aDeci2,{'B6_PRUNIT'})
	aadd(aDeci2,{'B8_PRCLOT'})
	aadd(aDeci2,{'B9_VINI1'})
	aadd(aDeci2,{'B9_VINI2'})
	aadd(aDeci2,{'B9_VINI3'})
	aadd(aDeci2,{'B9_VINI4'})
	aadd(aDeci2,{'B9_VINI5'})
	aadd(aDeci2,{'B9_VINIFF1'})
	aadd(aDeci2,{'B9_VINIFF2'})
	aadd(aDeci2,{'B9_VINIFF3'})
	aadd(aDeci2,{'B9_VINIFF4'})
	aadd(aDeci2,{'B9_VINIFF5'})
	aadd(aDeci2,{'B9_VINIRP1'})
	aadd(aDeci2,{'B9_VINIRP2'})
	aadd(aDeci2,{'B9_VINIRP3'})
	aadd(aDeci2,{'B9_VINIRP4'})
	aadd(aDeci2,{'B9_VINIRP5'})
	aadd(aDeci2,{'BB_VALOR'})
	aadd(aDeci2,{'BI_PRV'})
	aadd(aDeci2,{'C1_PRECO'})
	aadd(aDeci2,{'C1_VUNIT'})
	aadd(aDeci2,{'C2_APFIFF1'})
	aadd(aDeci2,{'C2_APFIFF2'})
	aadd(aDeci2,{'C2_APFIFF3'})
	aadd(aDeci2,{'C2_APFIFF4'})
	aadd(aDeci2,{'C2_APFIFF5'})
	aadd(aDeci2,{'C2_APINFF1'})
	aadd(aDeci2,{'C2_APINFF2'})
	aadd(aDeci2,{'C2_APINFF3'})
	aadd(aDeci2,{'C2_APINFF4'})
	aadd(aDeci2,{'C2_APINFF5'})
	aadd(aDeci2,{'C2_APRATU1'})
	aadd(aDeci2,{'C2_APRATU2'})
	aadd(aDeci2,{'C2_APRATU3'})
	aadd(aDeci2,{'C2_APRATU4'})
	aadd(aDeci2,{'C2_APRATU5'})
	aadd(aDeci2,{'C2_APRFIM1'})
	aadd(aDeci2,{'C2_APRFIM2'})
	aadd(aDeci2,{'C2_APRFIM3'})
	aadd(aDeci2,{'C2_APRFIM4'})
	aadd(aDeci2,{'C2_APRFIM5'})
	aadd(aDeci2,{'C2_APRFRP1'})
	aadd(aDeci2,{'C2_APRFRP2'})
	aadd(aDeci2,{'C2_APRFRP3'})
	aadd(aDeci2,{'C2_APRFRP4'})
	aadd(aDeci2,{'C2_APRFRP5'})
	aadd(aDeci2,{'C2_APRINI1'})
	aadd(aDeci2,{'C2_APRINI2'})
	aadd(aDeci2,{'C2_APRINI3'})
	aadd(aDeci2,{'CY_PRECO'})
	aadd(aDeci2,{'C2_APRINI5'})
	aadd(aDeci2,{'C2_APRIRP1'})
	aadd(aDeci2,{'C2_APRIRP2'})
	aadd(aDeci2,{'C2_APRIRP3'})
	aadd(aDeci2,{'C2_APRIRP4'})
	aadd(aDeci2,{'C2_APRIRP5'})
	aadd(aDeci2,{'C2_VATU1'})
	aadd(aDeci2,{'C2_VATU2'})
	aadd(aDeci2,{'C2_VATU3'})
	aadd(aDeci2,{'C2_VATU4'})
	aadd(aDeci2,{'C2_VATU5'})
	aadd(aDeci2,{'C2_VFIM1'})
	aadd(aDeci2,{'C2_VFIM2'})
	aadd(aDeci2,{'C2_VFIM3'})
	aadd(aDeci2,{'C2_VFIM4'})
	aadd(aDeci2,{'C2_VFIM5'})
	aadd(aDeci2,{'C2_VFIMFF1'})
	aadd(aDeci2,{'C2_VFIMFF2'})
	aadd(aDeci2,{'C2_VFIMFF3'})
	aadd(aDeci2,{'C2_VFIMFF4'})
	aadd(aDeci2,{'C2_VFIMFF5'})
	aadd(aDeci2,{'C2_VFIMRP1'})
	aadd(aDeci2,{'C2_VFIMRP2'})
	aadd(aDeci2,{'C2_VFIMRP3'})
	aadd(aDeci2,{'C2_VFIMRP4'})
	aadd(aDeci2,{'C2_VFIMRP5'})
	aadd(aDeci2,{'C2_VINI1'})
	aadd(aDeci2,{'C2_VINI2'})
	aadd(aDeci2,{'C2_VINI3'})
	aadd(aDeci2,{'C2_VINI4'})
	aadd(aDeci2,{'C2_VINI5'})
	aadd(aDeci2,{'C2_VINIFF1'})
	aadd(aDeci2,{'C2_VINIFF2'})
	aadd(aDeci2,{'C2_VINIFF3'})
	aadd(aDeci2,{'C2_VINIFF4'})
	aadd(aDeci2,{'C2_VINIFF5'})
	aadd(aDeci2,{'C2_VINIRP1'})
	aadd(aDeci2,{'C2_VINIRP2'})
	aadd(aDeci2,{'C2_VINIRP3'})
	aadd(aDeci2,{'C2_VINIRP4'})
	aadd(aDeci2,{'C2_VINIRP5'})
	aadd(aDeci2,{'C3_AVISTA'})
	aadd(aDeci2,{'C3_PRECO'})
	aadd(aDeci2,{'C4_VALOR'})
	aadd(aDeci2,{'C6_PRCVEN'})
	aadd(aDeci2,{'C6_PRUNIT'})
	aadd(aDeci2,{'C7_PRECO'})
	aadd(aDeci2,{'C8_PRECO'})
	aadd(aDeci2,{'C9_PRCVEN'})
	aadd(aDeci2,{'CK_PRCVEN'})
	aadd(aDeci2,{'CK_PRUNIT'})
	aadd(aDeci2,{'CNB_VLUNIT'})
	aadd(aDeci2,{'CNE_VLUNIT'})
	aadd(aDeci2,{'CY_PRECO'})
	aadd(aDeci2,{'D1_VALDEV'})
	aadd(aDeci2,{'D1_VUNIT'})
	aadd(aDeci2,{'D2_PRCVEN'})
	aadd(aDeci2,{'D2_PRUNIT'})
	aadd(aDeci2,{'D2_VALDEV'})
	aadd(aDeci2,{'D2_VARPRUN'})
	aadd(aDeci2,{'D6_VRUNIT'})
	aadd(aDeci2,{'DH_SALDO'})
	aadd(aDeci2,{'DT_VUNIT'})
	aadd(aDeci2,{'DY_VALOR'})
	aadd(aDeci2,{'GA_PRCVEN'})
	aadd(aDeci2,{'L2_ECVALOR'})
	aadd(aDeci2,{'L2_PRCTAB'})
	aadd(aDeci2,{'L2_VLRITEM'})
	aadd(aDeci2,{'L2_VRUNIT'})
	aadd(aDeci2,{'LR_ECVALOR'})
	aadd(aDeci2,{'LR_PRCTAB'})
	aadd(aDeci2,{'LR_VLRITEM'})
	aadd(aDeci2,{'LR_VRUNIT'})
	aadd(aDeci2,{'UB_VRUNIT'})
	aadd(aDeci2,{'DA1_PRCVEN'})
	aadd(aDeci2,{'B1_PRV1'})
	aadd(aDeci2,{'FT_PRCUNIT'})
	aadd(aDeci2,{'L4_VALOR'})
	aadd(aDeci2,{'D1_CUSTO'})
	aadd(aDeci2,{'D1_CUSTO1'})
	aadd(aDeci2,{'D1_CUSTO2'})
	aadd(aDeci2,{'D1_CUSTO3'})
	aadd(aDeci2,{'D1_CUSTO4'})
	aadd(aDeci2,{'D1_CUSTO5'})
	aadd(aDeci2,{'D2_CUSTO1'})
	aadd(aDeci2,{'D2_CUSTO2'})
	aadd(aDeci2,{'D2_CUSTO3'})
	aadd(aDeci2,{'D2_CUSTO4'})
	aadd(aDeci2,{'D2_CUSTO5'})
	aadd(aDeci2,{'D3_CUSTO1'})
	aadd(aDeci2,{'D3_CUSTO2'})
	aadd(aDeci2,{'D3_CUSTO3'})
	aadd(aDeci2,{'D3_CUSTO4'})
	aadd(aDeci2,{'D3_CUSTO5'})

	aadd(aDeci2,{'CNB_VALDESC'})
	aadd(aDeci2,{'CNB_DESC'})
	aadd(aDeci2,{'CNB_DESC'})
	aadd(aDeci2,{'CNZ_PERC'})
	aadd(aDeci2,{'CNE_PDESC'})
	aadd(aDeci2,{'LR_VALDESC'})
	aadd(aDeci2,{'LR_DESC'})
	aadd(aDeci2,{'L2_VALDESC'})
	aadd(aDeci2,{'L2_DESC'})
	aadd(aDeci2,{'L1_DESCONT'})
	aadd(aDeci2,{'L1_DESCNF'})
	aadd(aDeci2,{'LQ_DESCONT'})
	aadd(aDeci2,{'L1_DESCNF'})
	aadd(aDeci2,{'NJB_VLRBAS'})
	aadd(aDeci2,{'NJJ_VLRUNI'})
	aadd(aDeci2,{'NJM_VLRUNI'})
	aadd(aDeci2,{'NJR_VLRBAS'})
	aadd(aDeci2,{'NJR_VLRUNI'})
	aadd(aDeci2,{'NJR_TIPFAT'})
	aadd(aDeci2,{'NJR_HVIFAT'})
	aadd(aDeci2,{'NJR_OUTFAT'})
	aadd(aDeci2,{'NJV_VALOR'})
	aadd(aDeci2,{'NJX_VLRBAS'})
	aadd(aDeci2,{'NK1_VALOR'})
	aadd(aDeci2,{'NK3_PRECO'})
	aadd(aDeci2,{'NKA_VLRIDX'})
	aadd(aDeci2,{'NKA_VLRCOM'})
	aadd(aDeci2,{'NKA_VLRUN1'})
	aadd(aDeci2,{'NKA_VLRUN2'})
	aadd(aDeci2,{'NKA_TXACOT'})
	aadd(aDeci2,{'NKC_VRUN'})
	aadd(aDeci2,{'NKO_VVNDTB'})
	aadd(aDeci2,{'NKO_PRCVEN'})
	aadd(aDeci2,{'NKO_VRPAUT'})
	aadd(aDeci2,{'NKT_VRVNDP'})
	aadd(aDeci2,{'NKT_VRPAUT'})
	aadd(aDeci2,{'NN5_VLRUNI'})
	aadd(aDeci2,{'NN7_VLRTAX'})
	aadd(aDeci2,{'NN8_VLRUNI'})
	aadd(aDeci2,{'NN8_VALUNI'})
	aadd(aDeci2,{'NN8_VLRLIQ'})
	aadd(aDeci2,{'NN8_VALLIQ'})
	aadd(aDeci2,{'NNC_DOCVUN'})
	aadd(aDeci2,{'NNF_VLBCOM'})
	aadd(aDeci2,{'N78_HVIPON'})
	aadd(aDeci2,{'N78_OUTPON'})
	aadd(aDeci2,{'N79_VLRUNI'})
	aadd(aDeci2,{'N7G_COR1'})
	aadd(aDeci2,{'N7G_COR2'})
	aadd(aDeci2,{'N7G_COR3'})
	aadd(aDeci2,{'N7G_COR4'})
	aadd(aDeci2,{'N7G_COR5'})
	aadd(aDeci2,{'NJ7_FOLHA1'})
	aadd(aDeci2,{'NJ7_FOLHA2'})
	aadd(aDeci2,{'NJ7_FOLHA3'})
	aadd(aDeci2,{'NJ7_FOLHA4'})
	aadd(aDeci2,{'NJ7_FOLHA5'})
	aadd(aDeci2,{'N89_RVRUNI'})
	aadd(aDeci2,{'N89_VLRDOC'})
	aadd(aDeci2,{'NC9_VLUNPR'})
	aadd(aDeci2,{'N9A_TRVTAX'})
	aadd(aDeci2,{'N9A_VLR2MO'})
	aadd(aDeci2,{'N9A_VLRTAX'})
	aadd(aDeci2,{'N9A_VLUDEM'})
	aadd(aDeci2,{'N9A_VLUDES'})
	aadd(aDeci2,{'N9A_VLUDPR'})
	aadd(aDeci2,{'N9A_VLUFPR'})
	aadd(aDeci2,{'N9D_VLRUNI'})
	aadd(aDeci2,{'N9H_VLINDT'})
	aadd(aDeci2,{'N9J_VLRTAX'})
	aadd(aDeci2,{'ND1_VLTAXA'})
	aadd(aDeci2,{'ND1_VLUDEM'})
	aadd(aDeci2,{'ND1_VLUDES'})
	aadd(aDeci2,{'ND1_VLUFAT'})
	aadd(aDeci2,{'ND1_VLUFIN'})
	aadd(aDeci2,{'ND1_VLUFIX'})
	aadd(aDeci2,{'TJ_CUSTMAT'})
	aadd(aDeci2,{'TJ_CUSTMAA'})
	aadd(aDeci2,{'TJ_CUSTMAS'})
	aadd(aDeci2,{'TJ_CUSTTER'})
	aadd(aDeci2,{'TJ_CUSTMDO'})
	aadd(aDeci2,{'TJ_CUSTFER'})
	aadd(aDeci2,{'TL_CUSTO'})  
	aadd(aDeci2,{'TS_CUSTMDO'})
	aadd(aDeci2,{'TS_CUSTMAT'})
	aadd(aDeci2,{'TS_CUSTMAA'})
	aadd(aDeci2,{'TS_CUSTMAS'})
	aadd(aDeci2,{'TS_CUSTTER'})
	aadd(aDeci2,{'TT_CUSTO'})  
	aadd(aDeci2,{'GUC_VLFIXN'})
	aadd(aDeci2,{'GUC_PCNORM'})
	aadd(aDeci2,{'GUC_VLUNIN'})
	aadd(aDeci2,{'GUC_VLFRAC'})
	aadd(aDeci2,{'GUC_VLMINN'})
	aadd(aDeci2,{'GUC_PCLIM'})
	aadd(aDeci2,{'GUC_VLLIM'})
	aadd(aDeci2,{'GUC_VLFIXE'})
	aadd(aDeci2,{'GUC_PCEXTR'})
	aadd(aDeci2,{'GUC_VLUNIE'})
	aadd(aDeci2,{'GUZ_VLFXFI'})
	aadd(aDeci2,{'GUZ_PCENTR'})
	aadd(aDeci2,{'GUZ_VLENTR'})
	aadd(aDeci2,{'GV1_VLFIXN'})
	aadd(aDeci2,{'GV1_PCNORM'})
	aadd(aDeci2,{'GV1_VLUNIN'})
	aadd(aDeci2,{'GV1_VLFRAC'})
	aadd(aDeci2,{'GV1_VLMINN'})
	aadd(aDeci2,{'GV1_VLLIM '})
	aadd(aDeci2,{'GV1_VLFIXE'})
	aadd(aDeci2,{'GV1_PCEXTR'})
	aadd(aDeci2,{'GV1_VLUNIE'})
	aadd(aDeci2,{'GV6_FRMIN '})
	aadd(aDeci2,{'GV7_VLALUG'})
	aadd(aDeci2,{'GV8_PCDEV '})
	aadd(aDeci2,{'GV8_PCREEN'})
	aadd(aDeci2,{'GV8_VLMXRE'})
	aadd(aDeci2,{'GV8_VLMINR'})
	aadd(aDeci2,{'GVA_VLMULT'})
	aadd(aDeci2,{'GVB_VLADIC'})
	aadd(aDeci2,{'GVQ_VALCOT'})
	aadd(aDeci2,{'GW3_VLDF'})  
	aadd(aDeci2,{'GW3_TAXAS'}) 
	aadd(aDeci2,{'GW3_FRPESO'})
	aadd(aDeci2,{'GW3_FRVAL'}) 
	aadd(aDeci2,{'GW3_PEDAG'}) 
	aadd(aDeci2,{'GW3_VOLUM'}) 
	aadd(aDeci2,{'GW3_PESOR'}) 
	aadd(aDeci2,{'GW3_PESOC'}) 
	aadd(aDeci2,{'GW3_VLCARG'})
	aadd(aDeci2,{'GW3_BASIMP'})
	aadd(aDeci2,{'GW3_VLIMP'}) 
	aadd(aDeci2,{'GW3_IMPRET'})
	aadd(aDeci2,{'GW3_BASCOF'})
	aadd(aDeci2,{'GW3_VLCOF'}) 
	aadd(aDeci2,{'GW3_BASPIS'})
	aadd(aDeci2,{'GW3_VLPIS'}) 
	aadd(aDeci2,{'GW3_VLDIV'}) 
	aadd(aDeci2,{'GW6_VLFATU'})
	aadd(aDeci2,{'GW6_VLDESC'})
	aadd(aDeci2,{'GW6_VLICMS'})
	aadd(aDeci2,{'GW6_VLICRE'})
	aadd(aDeci2,{'GW6_VLISS'}) 
	aadd(aDeci2,{'GW6_VLISRE'})
	aadd(aDeci2,{'GW6_VLJURO'})
	aadd(aDeci2,{'GW6_DINDEN'})
	aadd(aDeci2,{'GW6_VLIRRF'})
	aadd(aDeci2,{'GW8_VALOR'}) 
	aadd(aDeci2,{'GW8_VOLUME'})
	aadd(aDeci2,{'GW8_PESOR'}) 
	aadd(aDeci2,{'GW8_DIFICM'})
	aadd(aDeci2,{'GW8_VALLIQ'})
	aadd(aDeci2,{'GWF_BASISS'})
	aadd(aDeci2,{'GWF_VLISS'}) 
	aadd(aDeci2,{'GWF_BASICM'})
	aadd(aDeci2,{'GWF_VLICMS'})
	aadd(aDeci2,{'GWF_ICMRET'})
	aadd(aDeci2,{'GWF_BAPICO'})
	aadd(aDeci2,{'GWF_VLPIS'}) 
	aadd(aDeci2,{'GWF_VLCOFI'})
	aadd(aDeci2,{'GWI_VLFRET'})
	aadd(aDeci2,{'GWI_VLLIQ'}) 
	aadd(aDeci2,{'GWN_VPVAL'}) 
	aadd(aDeci2,{'GWN_TOTVOL'})
	aadd(aDeci2,{'GWN_TOTPER'})
	aadd(aDeci2,{'GWN_TOTPEC'})
	aadd(aDeci2,{'GWN_TOTALT'})
	aadd(aDeci2,{'GWN_CPESO'})
	aadd(aDeci2,{'GWN_CVOL'})
	aadd(aDeci2,{'GWN_CVAL'})
	aadd(aDeci2,{'B6_CUSTO1'})
	aadd(aDeci2,{'B6_CUSTO2'})
	aadd(aDeci2,{'B6_CUSTO3'})
	aadd(aDeci2,{'B6_CUSTO4'})
	aadd(aDeci2,{'B6_CUSTO5'})
	aadd(aDeci2,{'B6_CUSFF1'})
	aadd(aDeci2,{'B6_CUSFF2'})
	aadd(aDeci2,{'B6_CUSFF3'})
	aadd(aDeci2,{'B6_CUSFF4'})
	aadd(aDeci2,{'B6_CUSFF5'})
	aadd(aDeci2,{'B6_CUSRP1'})
	aadd(aDeci2,{'B6_CUSRP2'})
	aadd(aDeci2,{'B6_CUSRP3'})
	aadd(aDeci2,{'B6_CUSRP4'})
	aadd(aDeci2,{'B6_CUSRP5'})
	aadd(aDeci2,{'D3_CUSRP1'})
	aadd(aDeci2,{'D3_CUSRP2'})
	aadd(aDeci2,{'D3_CUSRP3'})
	aadd(aDeci2,{'D3_CUSRP4'})
	aadd(aDeci2,{'D3_CUSRP5'})
	aadd(aDeci2,{'D2_CUSRP1'})
	aadd(aDeci2,{'D2_CUSRP2'})
	aadd(aDeci2,{'D2_CUSRP3'})
	aadd(aDeci2,{'D2_CUSRP4'})
	aadd(aDeci2,{'D2_CUSRP5'})
	aadd(aDeci2,{'D1_CUSRP1'})
	aadd(aDeci2,{'D1_CUSRP2'})
	aadd(aDeci2,{'D1_CUSRP3'})
	aadd(aDeci2,{'D1_CUSRP4'})
	aadd(aDeci2,{'D1_CUSRP5'})
	aadd(aDeci2,{'B2_CMRP1'})
	aadd(aDeci2,{'B2_CMRP2'})
	aadd(aDeci2,{'B2_CMRP3'})
	aadd(aDeci2,{'B2_CMRP4'})
	aadd(aDeci2,{'B2_CMRP5'})
	aadd(aDeci2,{'B2_VFIMFF1'})
	aadd(aDeci2,{'B2_VFIMFF2'})
	aadd(aDeci2,{'B2_VFIMFF3'})
	aadd(aDeci2,{'B2_VFIMFF4'})
	aadd(aDeci2,{'B2_VFIMFF5'})
	aadd(aDeci2,{'B2_CMFF1'})
	aadd(aDeci2,{'B2_CMFF2'})
	aadd(aDeci2,{'B2_CMFF3'})
	aadd(aDeci2,{'B2_CMFF4'})
	aadd(aDeci2,{'B2_CMFF5'})
	aadd(aDeci2,{'B9_CMRP1'})
	aadd(aDeci2,{'B9_CMRP2'})
	aadd(aDeci2,{'B9_CMRP3'})
	aadd(aDeci2,{'B9_CMRP4'})
	aadd(aDeci2,{'B9_CMRP5'})
	aadd(aDeci2,{'D1_CUSFF1'})
	aadd(aDeci2,{'D1_CUSFF2'})
	aadd(aDeci2,{'D1_CUSFF3'})
	aadd(aDeci2,{'D1_CUSFF4'})
	aadd(aDeci2,{'D1_CUSFF5'})
	aadd(aDeci2,{'D2_CUSFF1'})
	aadd(aDeci2,{'D3_CUSFF1'})
	aadd(aDeci2,{'D3_CUSFF2'})
	aadd(aDeci2,{'D3_CUSFF3'})
	aadd(aDeci2,{'D3_CUSFF4'})
	aadd(aDeci2,{'D3_CUSFF5'})

	IntCont :=1

	For IntCont := 1 to Len(aDeci)
		
		strSQL:="SELECT * FROM SX3"+cEmpresa+" WHERE X3_CAMPO = '"+ STRTRAN((aDeci[IntCont][1])," ","") +"' AND D_E_L_E_T_ <> '*' "
		If Select("QRY") > 0 
			QRY->(dbCloseArea())
		Endif 
		TcQuery strSQL NEW Alias "QRY"
		aComp1 := STRTRAN(QRY->X3_GRPSXG," ","")

		IF aComp1 != ""

		  strSQL:="SELECT * FROM SXG"+cEmpresa+" WHERE XG_GRUPO = '"+aComp1+"' AND D_E_L_E_T_ <> '*' "
			If Select("QRY") > 0 
				QRY->(dbCloseArea())
			Endif 
			TcQuery strSQL NEW Alias "QRY"
			aComp2 := QRY->XG_SIZEMAX
		
			IF aComp2 >= cTamanho 

				cQueryX := "UPDATE SX3"+cEmpresa+" SET X3_TIPO = '" + cValToChar(cTipo) +"', X3_TAMANHO = '" +cValTOChar(cTamanho)+ "', X3_DECIMAL = '" + cValToChar(cDecimal) + "', X3_PICTURE = '" +cValToChar(cMascara)+"' WHERE X3_CAMPO = '"+ STRTRAN((aDeci[IntCont][1])," ","") +"' AND D_E_L_E_T_ <> '*' "
				TCSQLEXEC(cQueryX) 

			ENDIF

		ELSE

			cQueryX := "UPDATE SX3"+cEmpresa+" SET X3_TIPO = '" + cValToChar(cTipo) +"', X3_TAMANHO = '" +cValTOChar(cTamanho)+ "', X3_DECIMAL = '" + cValToChar(cDecimal) + "', X3_PICTURE = '" +cValToChar(cMascara)+"' WHERE X3_CAMPO = '"+ STRTRAN((aDeci[IntCont][1])," ","") +"' AND D_E_L_E_T_ <> '*' "
			TCSQLEXEC(cQueryX) 

		ENDIF

	Next

	IntCont :=1

	For IntCont := 1 to Len(aDeci2)

		strSQL:="SELECT * FROM SX3"+cEmpresa+" WHERE X3_CAMPO = '"+STRTRAN((aDeci2[IntCont][1])," ","") +"' AND D_E_L_E_T_ <> '*' "
		If Select("QRY") > 0 
		QRY->(dbCloseArea())
		Endif 
		TcQuery strSQL NEW Alias "QRY"
		aComp1 := STRTRAN(QRY->X3_GRPSXG," ","")

		IF aComp1 != ""

			strSQL:="SELECT * FROM SXG"+cEmpresa+" WHERE XG_GRUPO = '"+aComp1+"' AND D_E_L_E_T_ <> '*' "
			If Select("QRY") > 0 
				QRY->(dbCloseArea())
			Endif 
			TcQuery strSQL NEW Alias "QRY"
			aComp2 := QRY->XG_SIZEMAX
		

			IF aComp2 >= cTamanho

				cQueryY:= "UPDATE SX3"+cEmpresa+" SET X3_TIPO = '" + cValToChar(cTipo) +"', X3_TAMANHO = '" +cValToChar(cTamanho)+ "', X3_DECIMAL = '" + cValToChar(cDecimal2) + "', X3_PICTURE = '" +cValToChar(cMascara2)+"' WHERE X3_CAMPO = '"+ STRTRAN((aDeci2[IntCont][1])," ","") +"' AND D_E_L_E_T_ <> '*' "
				TCSQLEXEC(cQueryY) 
			
			ENDIF

		ELSE

			cQueryY:= "UPDATE SX3"+cEmpresa+" SET X3_TIPO = '" + cValToChar(cTipo) +"', X3_TAMANHO = '" +cValToChar(cTamanho)+ "', X3_DECIMAL = '" + cValToChar(cDecimal2) + "', X3_PICTURE = '" +cValToChar(cMascara2)+"' WHERE X3_CAMPO = '"+ STRTRAN((aDeci2[IntCont][1])," ","") +"' AND D_E_L_E_T_ <> '*' "
			TCSQLEXEC(cQueryY)

		ENDIF

	Next

Return


//******************************************************************************************************************
// Atualiza Detalhes dos Campos Backoffice (Ordem, tamanho, opCOES de Campos em Geral) -  Igor Mateus 11/2021
//******************************************************************************************************************
Static Function AtuBack()

	Local cParse1 	 := 'GETSXENUM("SA1","A1_COD")'
	Local cParse2 	 := 'GETSXENUM("SA2","A2_COD")'
	Local cPais   	 := 'ALLTRIM("105")'
	Local cPaisBacen := 'ALLTRIM("01058")'
	Local cLoja 	 := 'ALLTRIM("0001")'
	Local cValidSA1   :='pertence("FJX")'

	Local cUsa := "x       x       x       x       x       x       x       x       x       x       x       x       x       x       x x     "

	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TAMANHO = 70 WHERE X3_CAMPO = 'A1_NREDUZ'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_VISUAL = 'V' WHERE X3_CAMPO = 'A1_MUN'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_VISUAL = 'V' WHERE X3_CAMPO = 'A2_MUN'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_VISUAL = 'V', X3_RELACAO = '" + cParse1+"' WHERE X3_CAMPO = 'A1_COD'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_VISUAL = 'V', X3_RELACAO = '" + cParse2+"' WHERE X3_CAMPO = 'A2_COD'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_CBOX 	 = 'F=Fisica;J=Juridica;X=Outros',X3_VALID = '"+cValidSA1+"' WHERE X3_CAMPO = 'A1_PESSOA'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_RELACAO = '" +cLoja+"' WHERE X3_CAMPO IN('A1_LOJA','A2_LOJA')")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TAMANHO = 120 WHERE X3_CAMPO = 'A1_EMAIL'") 
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_RELACAO = '" + cPais + "' WHERE X3_F3 = 'SYA   '")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_RELACAO = '" + cPaisBacen + "' WHERE X3_F3 = 'CCH   '")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_RELACAO = '' WHERE X3_CAMPO = 'F4_PISCOF'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_RELACAO = '' WHERE X3_CAMPO = 'F4_PISCRED'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '08', X3_F3 = 'X5DJ  ', X3_OBRIGAT = 'x       ', X3_VLDUSER = 'U_VALSX5TOK(M->F4_X_OPER)' WHERE X3_CAMPO = 'F4_X_OPER'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '09' WHERE X3_CAMPO = 'F4_X_DESOP'")		
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TAMANHO = 120 WHERE X3_CAMPO = 'A2_EMAIL'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TAMANHO = 80 WHERE X3_CAMPO = 'B1_DESC'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '04', X3_BROWSE = 'S' WHERE X3_CAMPO = 'C6_DESCRI'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TAMANHO = 70 WHERE X3_CAMPO = 'A2_NREDUZ'")		
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TAMANHO = 80 WHERE X3_CAMPO = 'C6_DESCRI'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = '' WHERE X3_CAMPO = 'A2_BANCO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'SB1NOV' WHERE X3_CAMPO = 'C6_COD'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'SB1NOV' WHERE X3_CAMPO = 'C7_COD'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'SA1NOV' WHERE X3_CAMPO = 'C5_CLIENTE'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'SA1NOV' WHERE X3_CAMPO = 'E1_CLIENTE'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'SA2NOV' WHERE X3_CAMPO = 'F1_FORNECE'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'SA2NOV' WHERE X3_CAMPO = 'C7_FORNECE'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'SA2NOV' WHERE X3_CAMPO = 'E2_FORNECE'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'PRFX  ',X3_VLDUSER = 'U_VALSX5TOK(M->E1_PREFIXO)' WHERE X3_CAMPO = 'E1_PREFIXO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'PRFX  ',X3_VLDUSER = 'U_VALSX5TOK(M->E2_PREFIXO)' WHERE X3_CAMPO = 'E2_PREFIXO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'PRFX  ',X3_VLDUSER = 'U_VALSX5TOK(M->FO2_PREFIX)' WHERE X3_CAMPO = 'FO2_PREFIX'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'PRFX  ',X3_VLDUSER = 'U_VALSX5TOK(M->E5_PREFIXO)' WHERE X3_CAMPO = 'E5_PREFIXO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'PRFX  ',X3_VLDUSER = 'U_VALSX5TOK(M->EA_PREFIXO)' WHERE X3_CAMPO = 'EA_PREFIXO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'PRFX  ',X3_VLDUSER = 'U_VALSX5TOK(M->E3_PREFIXO)' WHERE X3_CAMPO = 'E3_PREFIXO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'PRFX  ',X3_VLDUSER = 'U_VALSX5TOK(M->EF_PREFIXO)' WHERE X3_CAMPO = 'EF_PREFIXO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'GRPTRB',X3_VLDUSER = 'U_VALSX5TOK(M->A1_GRPTRIB)' WHERE X3_CAMPO = 'A1_GRPTRIB'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'GRPTRB',X3_VLDUSER = 'U_VALSX5TOK(M->A2_GRPTRIB)' WHERE X3_CAMPO = 'A2_GRPTRIB'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 = 'GRPTRB',X3_VLDUSER = 'U_VALSX5TOK(M->F7_GRPCLI)' WHERE X3_CAMPO = 'F7_GRPCLI'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '10', X3_BROWSE = 'S' WHERE X3_CAMPO = 'C7_DESC'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_VISUAL = 'V', X3_BROWSE = 'S', X3_ORDEM = '06' WHERE X3_CAMPO = 'C7_DESCRI'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_USADO = '"+cUsa+"' WHERE X3_CAMPO = 'E1_NUMBOR'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_USADO = '"+cUsa+"' WHERE X3_CAMPO = 'E1_DATABOR'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_USADO = '"+cUsa+"' WHERE X3_CAMPO = 'E1_NUMBCO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_USADO = '"+cUsa+"', X3_BROWSE = 'S' WHERE X3_CAMPO = 'E1_SALDO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_VISUAL = 'V' WHERE X3_CAMPO = 'E1_SALDO'")	
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_BROWSE = 'S' WHERE X3_CAMPO = 'E1_NOMCLI'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_USADO = '"+cUsa+"', X3_BROWSE = 'S' WHERE X3_CAMPO = 'E1_PORTADO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_USADO = '"+cUsa+"', X3_BROWSE = 'S' WHERE X3_CAMPO = 'E1_AGEDEP'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_USADO = '"+cUsa+"', X3_BROWSE = 'S' WHERE X3_CAMPO = 'E1_CONTA'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_'ORDEM' = 15, X3_BROWSE = 'S', X3_VISUAL = 'V' WHERE X3_CAMPO = 'E2_SALDO'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_BROWSE = 'S' WHERE X3_CAMPO = 'E2_NOMFOR'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_VLDUSER = 'U_SF1DOC()' WHERE X3_CAMPO = 'F1_DOC'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_VLDUSER = 'U_SF1DOC()' WHERE X3_CAMPO = 'F1_DOC'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '04', X3_BROWSE = 'S' WHERE X3_CAMPO = 'D1_X_DESC'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '04', X3_BROWSE = 'S' WHERE X3_CAMPO = 'C5_X_NOMCL'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_BROWSE = 'S' WHERE X3_CAMPO = 'C7_X_NOME'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '47' WHERE X3_CAMPO = 'A1_X_FPGT'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_F3 ='MHIPRI' WHERE X3_CAMPO IN ('C5_X_FPGT','A1_X_FPGT','E1_X_FPGT','CJ_X_FPGT')")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '12' WHERE X3_CAMPO = 'CJ_X_FPGT'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_ORDEM = '04' WHERE X3_CAMPO = 'CK_X_DSCPD'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'C5_CLIENTE'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'D1_VUNIT'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'C5_TPCARGA'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'C5_LOJACLI'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'C5_CLIENT'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'C5_LOJAENT'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'D1_COD'")
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'D1_QUANT'")	
	TCSQLEXEC("UPDATE SX3"+cEmpresa+" SET X3_TRIGGER = 'S' WHERE X3_CAMPO = 'F4_X_OPER'")	 

Return

User Function updtable(_cTab)

    If (ChkFile(_cTab))
       	 __SetX31Mode(.F.)

        If Select(_cTab) > 0
            dbSelectArea(_cTab)
            dbCloseArea()
        EndIf

        X31UpdTable(cValToChar(_cTab))

        If __GetX31Error()
            Alert("Erro ao executar o X31UPDTABLE('" + _cTab + "'): " + CRLF + __GetX31Trace())
        Else
           //MsgInfo("Execucao do X31UPDTABLE('" + _cTab + "') executada com sucesso!")
        EndIf

        __SetX31Mode(.T.)
    Else
        Alert("Tabela [" + _cTab + "] nao encontrada!")
    EndIf

Return(.T.)
