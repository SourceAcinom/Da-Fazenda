/*
UPDATE CT5010 SET CT5_ORIGEM = '"' + CT5_LANPAD + '/' + CT5_SEQUEN + ' | " + Alltrim(UsrRetName(RetCodUsr()))'
UPDATE CT5010 SET CT5_SBLOTE = '001' WHERE CT5_SBLOTE = ''
UPDATE CT5010 SET CT5_HAGLUT = CT5_HIST
UPDATE CT1010 SET CT1_ACITEM = '2', CT1_ACCUST = '2', CT1_ACCLVL = '2', CT1_CCOBRG = '2', CT1_ITOBRG = '2', CT1_CLOBRG = '2'
UPDATE SF1010 SET F1_DTLANC = ''      
UPDATE SF2010 SET F2_DTLANC = ''
UPDATE SE1010 SET E1_LA = ''
UPDATE SE5010 SET E5_LA = ''
UPDATE SE2010 SET E2_LA = ''
UPDATE SEF010 SET EF_LA = ''
*/

#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"
#include "rwmake.ch"

//Cria itens contabeis a partir do cadastro de clientes e fornecedores
User Function ImpItem()
	if MsgYesNo("Confirma atualização dos itens contabéis?")
		U_IMPITCLI("T")
		U_IMPITFOR("T")
		MsgInfo("Importação dos registro de fornecedores/clientes em itens contabéis executado com sucesso!")
	End If
Return

//Criar itens contábeis automaticamente para os FORNECEDORES já cadastrados
User Function IMPITFOR(cReg)
	Local aArea  := GetArea()

	IF cReg == "T"
		dbSelectarea("SA2")
		dbSetOrder(1)
		While !Eof()
			dbSelectarea("CTD")
			dbSetOrder(1)
			ItemFor()
			dbSelectarea("SA2")
			dbSkip()
		End
	Else
		ItemFor()
	EndIf
	RestArea(aArea)

Return .T.

Static function ItemFor()
	dbSelectarea("CTD")
	dbSetOrder(1)
	If !dbSeek(xFilial("CTD")+'F'+Alltrim(SA2->A2_COD)+Alltrim(SA2->A2_LOJA))
		RecLock("CTD",.T.)
		CTD->CTD_FILIAL := xFilial("CTD")
		CTD->CTD_ITEM  := 'F'+Alltrim(SA2->A2_COD)+Alltrim(SA2->A2_LOJA)
		CTD->CTD_DESC01 := SA2->A2_NOME
		CTD->CTD_CLASSE := "2"
		CTD->CTD_NORMAL := "0"
		CTD->CTD_BLOQ   := "2"
		CTD->CTD_DTEXIS := CTOD("01/01/1980")
		CTD->CTD_ITLP   := 'F'+Alltrim(SA2->A2_COD)+Alltrim(SA2->A2_LOJA)
		MsUnLock()
	Endif
Return()

//Criar itens contábeis automaticamente para os CLIENTES já cadastrados
User Function IMPITCLI(cReg)
	Local aArea  := GetArea()

	IF cReg == "T"
		dbSelectarea("SA1")
		dbSetOrder(1)
		While !Eof()
			dbSelectarea("CTD")
			dbSetOrder(1)
			ItemCli()
			dbSelectarea("SA1")
			dbSkip()
		End
	Else
		ItemCli()
	EndIf
	RestArea(aArea)
Return .T.

Static Function ItemCli()
	dbSelectarea("CTD")
	dbSetOrder(1)
	If !dbSeek(xFilial("CTD")+'C'+Alltrim(SA1->A1_COD)+Alltrim(SA1->A1_LOJA))
		RecLock("CTD",.T.)
		CTD->CTD_FILIAL := xFilial("CTD")
		CTD->CTD_ITEM  := 'C'+Alltrim(SA1->A1_COD)+Alltrim(SA1->A1_LOJA)
		CTD->CTD_DESC01 := SA1->A1_NOME
		CTD->CTD_CLASSE := "2"
		CTD->CTD_NORMAL := "0"
		CTD->CTD_BLOQ   := "2"
		CTD->CTD_DTEXIS := CTOD("01/01/1980")
		CTD->CTD_ITLP   := 'C'+Alltrim(SA1->A1_COD)+Alltrim(SA1->A1_LOJA)
		MsUnLock()
	Endif
Return()

User Function XXCTBM01()
	Local aParamBox := {}
	Local aCombo    := {"Sempre Online","Sempre Offline","Sem Regra"}
	Local aReturn   := {}

	aAdd(aParamBox,{2,"Contabiliza","Sempre Offline",aCombo,100,"",.F.})

	If !ParamBox(aParamBox ,"Muda Contabilizacao ",aReturn,,,,,,,,.F.)  // o array de retorno aRet, já é atribuído automaticamente às variáveis MV_PAR
		Return nil
	EndIf

	RptStatus({|| fExec(aReturn)}, "Aguarde...", "Executando rotina...")

Static Function fExec(aReturn)
	Local k
	Local nUser
	Local aDados

	aDados := {}
	AADD(aDados,{'AFI100',	4})
	AADD(aDados,{'AFI170',	2})
	AADD(aDados,{'AFI171',	2})
	AADD(aDados,{'AFI180',	2})
	AADD(aDados,{'AFI181',	2})
	AADD(aDados,{'AFI183',	2})
	AADD(aDados,{'AFI190',	4})
	AADD(aDados,{'AFI200',	11})
	AADD(aDados,{'AFI280',	4})
	AADD(aDados,{'AFI280',	5})
	AADD(aDados,{'AFI290',	3})
	AADD(aDados,{'AFI290',	4})
	AADD(aDados,{'AFI300',	11})
	AADD(aDados,{'AFI340',	11})
	AADD(aDados,{'AFI375',	17})
	AADD(aDados,{'AFI376',	3})
	AADD(aDados,{'AFI377',	17})
	AADD(aDados,{'AFI430',	9})
	AADD(aDados,{'AFI460',	1})
	AADD(aDados,{'AFI470',	8})
	AADD(aDados,{'ATA480',	12})
	AADD(aDados,{'FI415C',	3})
	AADD(aDados,{'FIA550',	3})
	AADD(aDados,{'FIN014',	7})
	AADD(aDados,{'FIN040',	3})
	AADD(aDados,{'FIN050',	4})
	AADD(aDados,{'FIN060',	3})
	AADD(aDados,{'FIN070',	4})
	AADD(aDados,{'FIN080',	3})
	AADD(aDados,{'FIN087',	5})
	AADD(aDados,{'FIN089',	3})
	AADD(aDados,{'FIN090',	3})
	AADD(aDados,{'FIN097',	8})
	AADD(aDados,{'FIN110',	3})
	AADD(aDados,{'FIN191',	1})
	AADD(aDados,{'FIN330',	9})
	AADD(aDados,{'FIN370',	3})
	AADD(aDados,{'FIN370',	7})
	AADD(aDados,{'FIN390',	3})
	AADD(aDados,{'FIN565',	1})
	AADD(aDados,{'FIN85A',	9})
	AADD(aDados,{'FIN85R',	1})
	AADD(aDados,{'FIN87A',	3})
	AADD(aDados,{'FINA650001', 12})
	AADD(aDados,{'FTA350',	7	})
	AADD(aDados,{'MT460A',	3	})
	AADD(aDados,{'MT460A',	4	})
	AADD(aDados,{'MT462A',	11	})
	AADD(aDados,{'MT462E',	11	})
	AADD(aDados,{'MT462M',	3	})
	AADD(aDados,{'MT462R',	3	})
	AADD(aDados,{'MT468C',	3	})
	AADD(aDados,{'MTA100',	6	})
	AADD(aDados,{'MTA101',	6	})
	AADD(aDados,{'MTA102',	3	})
	AADD(aDados,{'MTA103',	6	})
	AADD(aDados,{'MTA120',	5	})
	AADD(aDados,{'MTA121',	3	})
	AADD(aDados,{'MTA123',	5	})
	AADD(aDados,{'MTA160',	11	})
	AADD(aDados,{'MTA330',	12	})
	AADD(aDados,{'MTA410',	5	})
	AADD(aDados,{'MTA460',	3	})
	AADD(aDados,{'MTA460',	4	})
	AADD(aDados,{'MTA462',	11	})
	AADD(aDados,{'MTA465',	5	})
	AADD(aDados,{'MTA466',	5	})
	AADD(aDados,{'MTA467',	5	})
	AADD(aDados,{'MTA500',	12	})
	AADD(aDados,{'MTA530',	8	})
	AADD(aDados,{'MTA901',	8	})
	AADD(aDados,{'MTA906',	6	})
	AADD(aDados,{'MTA996',	4	})
	AADD(aDados,{'MTXRED',	2	})
	AADD(aDados,{'OMS341',	1	})
	AADD(aDados,{'PAG009',	16	})
	AADD(aDados,{'PAG011',	10	})
	AADD(aDados,{'RCB010',	5	})
	AADD(aDados,{'REMT11',	4	})
	AADD(aDados,{'REMT12',	6	})
	AADD(aDados,{'REMV10',	3	})
	AADD(aDados,{'RFIA01',	3	})

	aAllusers := FWSFALLUSERS()

	SetRegua(Len(aAllUsers))

	DbSelectarea("SXK")

	DbSetOrder(1)

	For nUser :=1 to len(aAllUsers)

		IncRegua()

		For k:= 1 to Len(aDados)

			lAchou := DbSeek(Pad(aDados[k,1],10)+StrZero(aDados[k,2],2)+'U'+aAllusers[nUser,2])

			If !lAchou .AND. aReturn[1]!='Sem Regra'
				Reclock("SXK",.T.)
				Replace XK_GRUPO   With aDados[k,1]
				Replace XK_SEQ     With StrZero(aDados[k,2],2)
				Replace XK_IDUSER  With 'U'+aAllusers[nUser,2]
				Replace XK_CONTEUD With iif(aReturn[1]=='Sempre Offline','2','1')
				MsUnlock()
			EndIf

			If lAchou .AND. aReturn[1]=="Sem Regra"
				Reclock("SXK",.F.)
				DbDelete()
				MsUnlock()
			EndIf

		Next
	Next

	MsgInfo("Fim de Processamento","Mensagem")

Return nil

User Function VerCtb(cConta1,cConta2,cConta3,cConta4,cConta5)
	Local cConta:=""

	cConta1:=If(cConta1=Nil,"",cConta1)
	cConta2:=If(cConta2=Nil,"",cConta2)
	cConta3:=If(cConta3=Nil,"",cConta3)
	cConta4:=If(cConta4=Nil,"",cConta4)
	cConta5:=If(cConta5=Nil,"",cConta5)
	If Empty(cConta1)
		If Empty(cConta2)
			If Empty(cConta3)
				If Empty(cConta4)
					If !Empty(cConta5)
						cConta:=cConta5
					EndIf
				Else
					cConta:=cConta4
				EndIf
			Else
				cConta:=cConta3
			EndIf
		Else
			cConta:=cConta2
		EndIf
	Else
		cConta:=cConta1
	EndIf

Return(cConta)

//Função usada para reabilitar lançamentos contabéis por documento.
User Function ReabCTB(strTb)
	Local xRet      := .T.

    xRet := U_CheckUser()
    If xRet == .F.
        Return(.F.)  
    EndIF

	If MsgYesNo("Deseja reabilitar o documento selecionado para nova contabilização ?", "Reabilitar contabilização?")

		IF strTb == "SF1"
			//Documento de Entrada
			RecLock("SF1",.F.)
			Replace F1_DTLANC With Ctod("  /  /  ")
			SF1->(MsUnlock())
		ElseIF strTb == "SF2"
			//Documento de Saída
			RecLock("SF2",.F.)
			Replace F2_DTLANC With Ctod("  /  /  ")
			SF2->(MsUnlock())
		ElseIF strTb == "SE2"
			//Contas a pagar
			RecLock("SE2",.F.)
			Replace E2_LA With " "
			SE2->(MsUnlock())
		ElseIF strTb == "SE1"
			//Contas a Receber
			RecLock("SE1",.F.)
			Replace E1_LA With " "
			SE1->(MsUnlock())
		ElseIF strTb == "SE5"
			//Baixas e movimento bancário
			RecLock("SE5",.F.)
			Replace E5_LA With " "
			SE5->(MsUnlock())
		EndIF

		MsgInfo("Processo executado com sucesso!", "Informação")

	EndIf

Return()

//Rotina para checar permissão de usuário na alteração de manutenções.
User Function CheckUser()
	/*
	Local strUser := Alltrim(SuperGetMV('MV_X_USECT', .F., ""))

	If !Upper(cUserName) $ Upper(strUser)
		Alert("Usuário sem permissão para executar essa operação.")	
		Return(.F.)
	EndIf
	*/
	
Return(.T.)

//-----------------------------------
//Atualiza valor da moeda 1
//-----------------------------------
User Function ADtSM2()
	Local xDTINI
	Local xDTFIM
	Local aPergs := {;
		{1,"Data Inicio :",CRIAVAR("M2_DATA", .F.),"","","","",50,.F.},;
		{1,"Data Fim    :",CRIAVAR("M2_DATA", .F.),"","","","",50,.F.};
	}

	if Parambox(aPergs, "Parametros para filtro")
	    xDTINI := MV_PAR01
	    xDTFIM := MV_PAR02
		Processa({|| AtuSM2(MV_PAR01, MV_PAR02)}, "Processando...")
	endif

Return

Static Function AtuSM2(xDTINI,xDTFIM)
	Local cData

	cData := xDTINI
	Do While cData <= xDTFIM

		DbSelectArea("SM2")
		DbSetOrder(1)
		If !dbSeek(DtoS(cData))
			RecLock("SM2", .T.)
			SM2->M2_DATA	:= cData
			SM2->M2_MOEDA1	:= 0              
			SM2->M2_MOEDA2	:= 0              
			SM2->M2_MOEDA3	:= 0              
			SM2->M2_MOEDA4	:= 0              
			SM2->M2_MOEDA5	:= 0              
			SM2->M2_INFORM	:= "S" 
			SM2->M2_TXMOED2	:= 0             
			SM2->M2_TXMOED3	:= 0             
			SM2->M2_TXMOED4	:= 0             
			SM2->M2_TXMOED5	:= 0
			MsUnlock()
		EndIf
		cData ++
	EndDo	

	MsgInfo("Registros atualizados com sucesso!", "Mensagem")

Return()

//Rotina usada para reabilitar lançamentos contabeis já integrados e excluidos da contabilidade
User Function LimpaCtb()
	Local cPerg:="LIMPACTB1"
	Local _sAlias := Alias()
	Local aRegs:={}
	Local j
	Local i
	Local ix

	dbSelectArea("SX1")                          
	dbSetOrder(1)
	cPerg := PADR(cPerg,10)
	AADD(aRegs,{cPerg,"01","Modulo     ?","Espanhol","Ingles","mv_ch1","N",1,0,0,"C","","mv_par01","Doc.Entradas","","","","","Faturamento ","","","","","Financeiro","","","","","","","","","","","","","","",""})
	AADD(aRegs,{cPerg,"02","Data de    ?","Espanhol","Ingles","mv_ch2","D",8,0,0,"G","","mv_par02","","","","'"+DTOC(dDataBase)+"'","","","","","","","","","","","","","","","","","","","","","",""})
	AADD(aRegs,{cPerg,"03","Data ate   ?","Espanhol","Ingles","mv_ch3","D",8,0,0,"G","","mv_par03","","","","'"+DTOC(dDataBase)+"'","","","","","","","","","","","","","","","","","","","","","",""})
	AADD(aRegs,{cPerg,"04","Apaga Lote ?","Espanhol","Ingles","mv_ch4","N",1,0,0,"C","","mv_par04","Sim      ","","","","","Nao  ","","","","","","","","","","","","","","","","","","","",""})
	AADD(aRegs,{cPerg,"05","Filial 	   ?","Espanhol","Ingles","mv_ch5","C" ,8,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","SM0","","","",""})

	For i:=1 to Len(aRegs)
		If !dbSeek(cPerg+aRegs[i,2])
			RecLock("SX1",.T.)
		Else
			RecLock("SX1",.F.)
		EndIf
	
		For j:=1 to FCount()
			If j <= Len(aRegs[i])
				FieldPut(j,aRegs[i,j])
			Endif
		Next
		MsUnlock()
		dbCommit()
	Next
	dbSelectArea(_sAlias)

	If Pergunte(cPerg,.t.)
		aFrase := {}
		aErros := {}
		cApaga := ""
		Do Case
		Case mv_par01 == 1
			AADD(aFrase,"UPDATE "+RetSqlName("SF1")+" SET F1_DTLANC = ' ' WHERE F1_DTDIGIT BETWEEN '" +Dtos(mv_par02)+ "' AND '"+Dtos(mv_par03)+"' AND F1_FILIAL = '" + Alltrim(mv_par05) + "'")
			cApaga := "UPDATE "+RetSqlName("CT2")+" SET D_E_L_E_T_ = '*' WHERE CT2_DATA BETWEEN '"+Dtos(mv_par02)+"' AND '"+Dtos(mv_par03)+"' AND CT2_FILORI = '" + Alltrim(mv_par05) + "' AND CT2_LOTE = '008810' AND CT2_SBLOTE <> '002' "
			
		Case mv_par01 == 2
			AADD(aFrase,"UPDATE "+RetSqlName("SF2")+" SET F2_DTLANC = ' ' WHERE F2_EMISSAO BETWEEN '" +Dtos(mv_par02)+ "' AND '"+Dtos(mv_par03)+"' AND F2_FILIAL = '" + Alltrim(mv_par05) + "'")
			cApaga := "UPDATE "+RetSqlName("CT2")+" SET D_E_L_E_T_ = '*' WHERE CT2_DATA BETWEEN '"+Dtos(mv_par02)+"' AND '"+Dtos(mv_par03)+"' AND CT2_FILORI = '" + Alltrim(mv_par05) + "' AND CT2_LOTE = '008820' AND CT2_SBLOTE <> '002' "
			
		Case mv_par01 == 3
			AADD(aFrase,"UPDATE "+RetSqlName("SE1")+" SET E1_LA = ' ' WHERE E1_EMISSAO BETWEEN '"+Dtos(mv_par02)+"' AND '"+Dtos(mv_par03)+"' AND E1_FILIAL = '" + Alltrim(mv_par05) + "' ")
			AADD(aFrase,"UPDATE "+RetSqlName("SE5")+" SET E5_LA = ' ' WHERE E5_DATA  BETWEEN '"+Dtos(mv_par02)+"' AND '"+Dtos(mv_par03)+"' AND E5_FILIAL = '" + Alltrim(mv_par05) + "' ")
			AADD(aFrase,"UPDATE "+RetSqlName("SE2")+" SET E2_LA = ' ' WHERE E2_EMIS1   BETWEEN '"+Dtos(mv_par02)+"' AND '"+Dtos(mv_par03)+"' AND E2_FILIAL = '" + Alltrim(mv_par05) + "' ")
			AADD(aFrase,"UPDATE "+RetSqlName("SEF")+" SET EF_LA = ' ' WHERE EF_DATA    BETWEEN '"+Dtos(mv_par02)+"' AND '"+Dtos(mv_par03)+"' AND EF_FILIAL = '" + Alltrim(mv_par05) + "'")
			cApaga := "UPDATE "+RetSqlName("CT2")+" SET D_E_L_E_T_ = '*' WHERE CT2_DATA BETWEEN '"+Dtos(mv_par02)+"' AND '"+Dtos(mv_par03)+"' AND CT2_FILORI = '" + Alltrim(mv_par05) + "' AND CT2_LOTE = '008850' AND CT2_SBLOTE <> '002' "
			//Sublote 002 é contabilização online e não pode ser apagado			
		EndCase
		
		For i := 1 To len(aFrase)
			nResult := TCSQLEXEC(aFrase[i])
			If nResult <> 0
				cErro   := " Erro:"+TCSqlError()
		   		AADD(aErros,cErro)
		  	Else
		   		cErro := ""
		  	EndIf
		Next
		
		If mv_par04 == 1
			nResult := TCSQLEXEC(cApaga)
			If nResult <> 0
				cErro   := " Erro:"+TCSqlError()
				AADD(aErros,cErro)
			Else
				cErro := ""
			EndIf
		EndIf 
		
		If nResult == 0
			If mv_par04 == 1
				If aviso("Reprocessa","Comando Executado com Sucesso! Como os Lotes foram excluidos, e recomendados que sejam feitos os reprocessamentos de Saldos. Deseja Reprocessar os Saldos?",{"Sim","Não"}) == 1
					CTBA190() 
				EndIf
			Else
		   		Aviso("Processamento","Comando Executado com Sucesso!",{"Ok"}) 
		  	EndIf  
		Else
			If Len(aErros)>0
				cLog := ""
				For ix := 1 to Len(aErros)
					cLog += (aErros[ix]+chr(10)+chr(13))
				Next
				@ 116,090 To 416,707 Dialog oDlgMemo Title "Ocorrencias"
				@ 001,005 Get cLog   Size 300,120  MEMO                 Object  oMemo
				@ 137,10+5*50 Button OemToAnsi("_Fechar")   Size 35,14 Action Close(oDlgMemo)
				Activate Dialog oDlgMemo
			EndIf
			MsgInfo("Falha no Comando! Retorno "+Str(nResult)+cErro)
		EndIf
	EndIf
Return nil

//***********************************************************************
//IMPRIMIR A RAZAO SOCIAL NO CADASTRO CONFORME A TABELA INFORMADA
//***********************************************************************
User Function xRazao(cxTabela)
	Local sxRazao:=""

	If cxTabela=="SF1"
		If AllTrim(SF1->F1_TIPO) $ "DB"
			sxRazao:=POSICIONE("SA1",1,SubStr(SF1->F1_FILIAL,1,4)+SPACE(2)+SF1->(F1_FORNECE+F1_LOJA),"A1_NOME")
		Else
			sxRazao:=POSICIONE("SA2",1,SubStr(SF1->F1_FILIAL,1,4)+SPACE(2)+SF1->(F1_FORNECE+F1_LOJA),"A2_NOME")
		EndIf

	ElseIf cxTabela=="SF2"
		If AllTrim(SF2->F2_TIPO) $ "DB"
			sxRazao:=POSICIONE("SA2",1,SubStr(SF2->F2_FILIAL,1,4)+SPACE(2)+SF2->(F2_CLIENTE+F2_LOJA),"A2_NOME")
		Else
			sxRazao:=POSICIONE("SA1",1,SubStr(SF2->F2_FILIAL,1,4)+SPACE(2)+SF2->(F2_CLIENTE+F2_LOJA),"A1_NOME")
		EndIf
	ElseIf cxTabela=="SC5"
		If AllTrim(SC5->C5_TIPO) $ "DB"
			sxRazao:=POSICIONE("SA2",1,SubStr(SC5->C5_FILIAL,1,4)+SPACE(2)+SC5->(C5_CLIENTE+C5_LOJACLI),"A2_NOME")
		Else
			sxRazao:=POSICIONE("SA1",1,SubStr(SC5->C5_FILIAL,1,4)+SPACE(2)+SC5->(C5_CLIENTE+C5_LOJACLI),"A1_NOME")
		EndIf
	ElseIf cxTabela=="SC9"
		sxRazao:=POSICIONE("SA1",1,SubStr(SC9->C9_FILIAL,1,4)+SPACE(2)+SC9->(C9_CLIENTE+C9_LOJA),"A1_NOME")
		If Empty(sxRazao)
			sxRazao:=POSICIONE("SA2",1,SubStr(SC9->C9_FILIAL,1,4)+SPACE(2)+SC9->(C9_CLIENTE+C9_LOJA),"A2_NOME")
		EndIf
	ElseIf cxTabela=="SC8"
		sxRazao:=POSICIONE("SA2",1,SubStr(SC8->C8_FILIAL,1,4)+SPACE(2)+SC8->(C8_FORNECE+C8_LOJA),"A2_NOME")
	EndIf

Return(sxRazao)

//***********************************************************************************
//IMPRIMIR A RAZAO SOCIAL NO CADASTRO CONFORME A TABELA INFORMADA - VARIAVEL DE TELA
//***********************************************************************************
User Function xRazaoM(cxTabela)
	Local sxRazao:=""

	If cxTabela=="SF1"
		If AllTrim(M->F1_TIPO) $ "DB"
			sxRazao:=POSICIONE("SA1",1,xFilial("SA1")+M->(F1_FORNECE+F1_LOJA),"A1_NOME")
		Else
			sxRazao:=POSICIONE("SA2",1,xFilial("SA2")+M->(F1_FORNECE+F1_LOJA),"A2_NOME")
		EndIf

	ElseIf cxTabela=="SF2"
		If AllTrim(M->F2_TIPO) $ "DB"
			sxRazao:=POSICIONE("SA2",1,xFilial("SA2")+M->(F2_CLIENTE+F2_LOJA),"A2_NOME")
		Else
			sxRazao:=POSICIONE("SA1",1,xFilial("SA1")+M->(F2_CLIENTE+F2_LOJA),"A1_NOME")
		EndIf
	ElseIf cxTabela=="SC5"
		If AllTrim(M->C5_TIPO) $ "DB"
			sxRazao:=POSICIONE("SA2",1,xFilial("SA2")+M->(C5_CLIENTE+C5_LOJACLI),"A2_NOME")
		Else
			sxRazao:=POSICIONE("SA1",1,xFilial("SA1")+M->(C5_CLIENTE+C5_LOJACLI),"A1_NOME")
		EndIf

	ElseIf cxTabela=="SC9"
		sxRazao:=POSICIONE("SA1",1,xFilial("SA1")+M->(C9_CLIENTE+C9_LOJA),"A1_NOME")
		If Empty(sxRazao)
			sxRazao:=POSICIONE("SA2",1,xFilial("SA2")+M->(C9_CLIENTE+C9_LOJA),"A2_NOME")
		EndIf

	ElseIf cxTabela=="SC8"
		sxRazao:=POSICIONE("SA2",1,xFilial("SA2")+M->(C8_FORNECE+C8_LOJA),"A2_NOME")
	EndIf

Return(sxRazao)

//CONTABILIZAÇÃO - CONTA DEBITO FOLHA
User Function cF01D()
	Local aArea := getArea()
	Local cCta := ""
	Local cTpCC := ""

	cTpCC := POSICIONE("CTT",1,XFILIAL("CTT")+SRA->RA_CC,"CTT_X_TIPO")
	If cTpCC == "A"
		cCta := POSICIONE("SRV",1,XFILIAL("SRV")+SRZ->RZ_PD,"RV_X_DADM")
	ElseIf cTpCC == "P"
		cCta := POSICIONE("SRV",1,XFILIAL("SRV")+SRZ->RZ_PD,"RV_X_DEBIT")
	ElseIF cTpCC == "C"
		cCta := POSICIONE("SRV",1,XFILIAL("SRV")+SRZ->RZ_PD,"RV_X_DCOME")
	EndIf

	RestArea(aArea)
Return(cCta)

//CONTABILIZAÇÃO - CONTA CREDITO FOLHA
User Function cF01C()
	Local aArea := getArea()
	Local cCta := ""
	Local cTpCC := ""

	cTpCC := POSICIONE("CTT",1,XFILIAL("CTT")+SRA->RA_CC,"CTT_X_TIPO")
	If cTpCC == "A"
		cCta := POSICIONE("SRV",1,XFILIAL("SRV")+SRZ->RZ_PD,"RV_X_CADM")
	ElseIf cTpCC == "P"
		cCta := POSICIONE("SRV",1,XFILIAL("SRV")+SRZ->RZ_PD,"RV_X_CREDI")
	ElseIf cTpCC == "C"
		cCta := POSICIONE("SRV",1,XFILIAL("SRV")+SRZ->RZ_PD,"RV_X_CCOME")
	EndIf

	RestArea(aArea)
Return(cCta)

//Crédito - Devolução de compras
User Function C610012()
	Local strSQL	:=""
	Local strConta	:=""

	strSQL:="SELECT D1_CONTA, D1_TES, D1_COD, D1_GRUPO FROM "+ retsqlname("SD1")+" "
	strSQL+="WHERE D_E_L_E_T_ <> '*' "
	strSQL+="AND D1_FILIAL = '"+Alltrim(SD2->D2_FILIAL)+"' "
	strSQL+="AND D1_DOC = '"+Alltrim(SD2->D2_NFORI)+"' "
	strSQL+="AND D1_SERIE = '"+Alltrim(SD2->D2_SERIORI)+"' "
	strSQL+="AND D1_ITEM = '"+Alltrim(SD2->D2_ITEMORI)+"' "
	strSQL+="AND D1_FORNECE = '"+Alltrim(SD2->D2_CLIENTE)+"' "
	strSQL+="AND D1_LOJA = '"+Alltrim(SD2->D2_LOJA)+"' "

	If Select("ORS") > 0
		ORS->(DbCloseArea())
	EndIf    

	strSQL := ChangeQuery(strSQL)
	TcQuery strSQL new alias "ORS"
	If !ORS->(EOF())
		If alltrim(ORS->D1_CONTA) <> ''
			strConta := alltrim(ORS->D1_CONTA)
		Endif
	EndIf

	If Empty(strConta)
		strConta := GetAdvFVal("SF4","F4_X_CD",xFilial("SD1")+ORS->D1_TES,1,"") 
	EndIf

	If Empty(strConta)
		strConta := GetAdvFVal("SB1","B1_CONTA",xFilial("SB1")+ORS->D1_COD,1,"") 
	EndIf

	If Empty(strConta)
		strConta := GetAdvFVal("SBM","BM_X_CTA",xFilial("SBM")+ORS->D1_GRUPO,1,"")
	EndIf

Return(strConta)

//Centro de custo - Devolução de compras
User Function CC610012()
	Local strSQL	:=""
	Local strCC	:=""

	strSQL:="SELECT D1_CC FROM "+ retsqlname("SD1")+" "
	strSQL+="WHERE D_E_L_E_T_ <> '*' "
	strSQL+="AND D1_FILIAL = '"+Alltrim(SD2->D2_FILIAL)+"' "
	strSQL+="AND D1_DOC = '"+Alltrim(SD2->D2_NFORI)+"' "
	strSQL+="AND D1_SERIE = '"+Alltrim(SD2->D2_SERIORI)+"' "
	strSQL+="AND D1_ITEM = '"+Alltrim(SD2->D2_ITEMORI)+"' "
	strSQL+="AND D1_FORNECE = '"+Alltrim(SD2->D2_CLIENTE)+"' "
	strSQL+="AND D1_LOJA = '"+Alltrim(SD2->D2_LOJA)+"' "

	If Select("ORS") > 0
		ORS->(DbCloseArea())
	EndIf    

	strSQL := ChangeQuery(strSQL)
	TcQuery strSQL new alias "ORS"
    If !ORS->(EOF())
        If alltrim(ORS->D1_CC) <> ''
            strCC := alltrim(ORS->D1_CC)
        Endif
	EndIf                                                                                                                      

Return(strCC)
