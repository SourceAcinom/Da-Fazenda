#include "protheus.ch"
#include "vkey.ch"
#include "topconn.ch"


/*----------------------------------------------------------------------*
 | Programa  : LmpDB                                                    |
 | Descrição : Limpa as tabelas do banco de dados que não são CADASTROS |
 *----------------------------------------------------------------------*/
User Function LmpDB()
	Local aCores := {}

	Private c_Als     := "SZZ"
	Private c_SufTab  := "010"
	Private cDelFunc  := ".T."
	Private cCadastro := "Limpeza de tabelas"

	Private aRotina := {}

	If !cUserName $ "Administrador,bruno.totvs"
		Alert("Usuário sem permissão para acessar esta rotina!")
		Return
	EndIf

	// Menu
	AAdd(aRotina, {"Pesquisar"  , "AxPesqui", 0, 1 })
	AAdd(aRotina, {"Visualizar" , "AxVisual", 0, 2 })
	AAdd(aRotina, {"Alterar"    , "AxAltera", 0, 4 })
	AAdd(aRotina, {"Limpeza"    , "Processa({|| U_DltReg() },'Apagando Registros! Aguarde...')", 0, 4 })
	AAdd(aRotina, {"Legenda"    , "u_BLLmpDB" ,0 , 3})

	// Legenda
	aAdd(aCores, {c_Als + "->(ZZ_PROP = 'S' .AND. ZZ_FIN = '1')", "BR_VERDE"})
	aAdd(aCores, {c_Als + "->(ZZ_PROP = 'S' .AND. ZZ_FIN = '2')", "BR_VERMELHO"})
	aAdd(aCores, {c_Als + "->(ZZ_PROP = 'S' .AND. ZZ_FIN = ' ')", "BR_AZUL"})
	aAdd(aCores, {c_Als + "->(ZZ_PROP = 'U')", "BR_LARANJA"})

	DbSelectArea(c_Als)
	DbSetOrder(1)

	If MsgYesNo( "Deseja atualizar a listagem de tabelas?", "Confirma?" )
		Processa({|| RunProc() },"Atualizando tabelas! Aguarde...")
	EndIf

	DbSelectArea(c_Als)
	DbSetOrder(1)

	mBrowse( 6, 1, 22, 75, c_Als,,,,,,aCores)
Return


/*-----------------------------------------------------------*
 | Programa  : RunProc                                       |
 | Descrição : Atualiza a lista de tabelas SX2 na tabela SZZ |
 *-----------------------------------------------------------*/
Static Function RunProc()
	Local c_Qry := ""

	DbSelectArea("SX2")
	DbSetOrder(1)
	DbGoTop()

	ProcRegua(SX2->(RecCount()))
	DbGoTop()

	While ! Eof()
		IncProc(SX2->X2_CHAVE)

		//c_Qry := "SELECT ISNULL(OBJECT_ID('"+ SX2->X2_CHAVE + c_SufTab +"'), 0) AS TB"
		c_Qry := " SELECT NVL( "
    	c_Qry += " 		(SELECT 1 FROM USER_OBJECTS WHERE OBJECT_NAME = '" + SX2->X2_CHAVE + c_SufTab + "' AND OBJECT_TYPE = 'TABLE'), 0 "
		c_Qry += "      ) AS TB
		c_Qry += "   FROM DUAL " // Oracle

		If Select("_Q01") > 0
			DbCloseArea()
		EndIf

		TcQuery c_Qry New Alias "_Q01"

		If _Q01->TB > 0
			c_Qry := "SELECT COUNT(1) AS QREG FROM "+ SX2->X2_CHAVE + c_SufTab

			If Select("_Q02") > 0
				DbCloseArea()
			EndIf

			TcQuery c_Qry New Alias "_Q02"

			If !(c_Als)->(DbSeek(xFilial(c_Als) + SX2->X2_CHAVE))
				If _Q02->QREG > 0
					DbSelectArea(c_Als)
					RecLock(c_Als, .T.)

					(c_Als)->ZZ_FILIAL := "    "
					(c_Als)->ZZ_TABELA := SX2->X2_CHAVE
					(c_Als)->ZZ_DESC   := UPPER(SX2->X2_NOME)
					(c_Als)->ZZ_PROP   := IIf(SUBSTR(SX2->X2_CHAVE, 1, 2) == "SZ", "U", IIf(SUBSTR(SX2->X2_CHAVE, 1, 1) == "Z", "U", "S"))
					(c_Als)->ZZ_REGS   := _Q02->QREG

					MsUnLock()
				EndIf
			Else
				DbSelectArea(c_Als)
				RecLock(c_Als, .F.)

				(c_Als)->ZZ_REGS := _Q02->QREG

				MsUnLock()
			EndIf

			_Q02->(DbCloseArea())
		Else
			If (c_Als)->(DbSeek(xFilial(c_Als) + SX2->X2_CHAVE))
				RecLock(c_Als, .F.)

				(c_Als)->ZZ_REGS := 0

				MsUnLock()
			EndIf
		EndIf

		_Q01->(DbCloseArea())
		DbSelectArea("SX2")
		DbSkip()
	EndDo
Return


/*-------------------------------------------------------------------*
 | Programa  : DltReg                                                |
 | Descrição : Deleta os registros das tabelas que não são CADASTROS |
 *-------------------------------------------------------------------*/
User Function DltReg()
	Local c_Qry := ""

	c_Qry := "SELECT * FROM "+ RetSqlName(c_Als) +" "
	c_Qry += " WHERE D_E_L_E_T_ = ' ' "
	c_Qry += "   AND ZZ_FIN     = ' ' "

	If Select("_Q03") > 0
		DbCloseArea()
	EndIf

	TcQuery c_Qry New Alias "_Q03"

	If !Eof()
		Alert("Ainda existem tabelas a classificar, verifique!")
		_Q03->(DbCloseArea())
		Return
	EndIf

	_Q03->(DbCloseArea())

	DbSelectArea(c_Als)
	DbGoTop()
	ProcRegua((c_Als)->(RecCount()))
	DbGoTop()

	While !Eof()
		IncProc((c_Als)->ZZ_TABELA)

		If (c_Als)->ZZ_FIN == "1" .And. (c_Als)->ZZ_REGS > 0
			c_Sql := " DELETE FROM " + (c_Als)->ZZ_TABELA + c_SufTab

			//If (c_Als)->ZZ_TABELA == "SE1"
			//	c_Sql += " WHERE E1_NUMBCO = ' ' AND E1_NUMBOR <> ' ' "
			//EndIf

			TcSqlExec(c_Sql)
		EndIf

		DbSkip()
	EndDo

	Alert("Fim")
Return


/*----------------------------------*
 | Programa  : BLLmpDB              |
 | Descrição : Definição da legenda |
 *----------------------------------*/
User Function BLLmpDB()
	Local aLegenda := {}

	AAdd(aLegenda, {"BR_VERMELHO" , "Cadastro" })
	AAdd(aLegenda, {"BR_VERDE"    , "Movimento" })
	AAdd(aLegenda, {"BR_AZUL"     , "Classificar" })
	AAdd(aLegenda, {"BR_LARANJA"  , "Tab. Usuario" })

	BrwLegenda("Finalidades", "Legenda", aLegenda)
Return
