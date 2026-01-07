#INCLUDE "TOTVS.CH"
#INCLUDE "FWMVCDEF.CH"
#include "TOPCONN.CH"

static cDescrir := "Vendedores x % Comissaso"


User Function LATFAA02()
	Local oBrowse 	:= Nil

	Private aRotina := MenuDef()

	oBrowse:= FWMBrowse():New()
	oBrowse:SetAlias("SZG")
	oBrowse:SetDescription(cDescrir)
	oBrowse:DisableDetails()
	oBrowse:Activate()
Return

Static Function ModelDef()
	Local oStructSZG := FWFormStruct(1, 'SZG' )
	Local oStructSZH := FwFormStruct(1, "SZH")
	Local oModel     := MPFormModel():New('MODELSZG' )
	Local aSZHRel    := {}
	Private cCpoVld  := ""

	oModel:AddFields('SZGMASTER',/*cOwner*/,oStructSZG)
	oModel:AddGrid('SZHDETAIL','SZGMASTER',oStructSZH,/*bLinePre*/, /*bLinePost*/,/*bPre - Grid Inteiro*/,/*bPos - Grid Inteiro*/,/*bLoad - Carga do modelo manualmente*/)  //cOwner  para quem pertence

	//Fazendo o relacionamento entre o Pai e Filho
	aAdd(aSZHRel, {'ZH_FILIAL', 'ZG_FILIAL'})
	aAdd(aSZHRel, {'ZH_VEND', 'ZG_VEND'})

	//oStructSZH:SetProperty( "ZH_VEND", MODEL_FIELD_INIT, { || oModel:GetModel("SZGMASTER"):GetValue('ZG_VEND')})
	//oModel:GetModel("SZHDETAIL"):SetUniqueLine({"ZH_PRODUTO"})
	oModel:SetRelation('SZHDETAIL', aSZHRel, SZH->(IndexKey(2)))
	
	//oStructSZG:SetProperty("ZG_VEND", MODEL_FIELD_VALID,{|| ExistChav( "SZG", FwFldGet("ZG_VEND") ) })

	oStructSZH:SetProperty("ZH_PRODUTO", MODEL_FIELD_VALID,{|| validLin('P') })
	oStructSZH:SetProperty("ZH_GRUPO", MODEL_FIELD_VALID,{|| validLin('G') })

	//Setando as descrições
	oModel:SetDescription(cDescrir)
	oModel:GetModel('SZGMASTER'):SetDescription('Vendedor x Comissao')
	oModel:GetModel('SZHDETAIL'):SetDescription('Itens Comissao Vendedor')
Return ( oModel )

Static Function ViewDef()
	Local oView        	:= Nil
	Local oModel        := FWLoadModel('LATFAA02')
	Local oStructSZG    := FWFormStruct(2, 'SZG')
	Local oStructSZH    := Nil

	Private cCpoVld   := ""

	oStructSZH := FwFormStruct(2, "SZH")

	//Criando a View
	oView := FWFormView():New()
	oView:SetModel(oModel)

	//Adicionando os campos do cabealho e o grid dos filhos
	oView:AddField('VIEW_SZG',oStructSZG,'SZGMASTER')
	oView:AddGrid('VIEW_SZH',oStructSZH,'SZHDETAIL')

	//Setando o dimensionamento de tamanho
	oView:CreateHorizontalBox('CABEC',30)
	oView:CreateHorizontalBox('GRID',70)

	//Amarrando a view com as box
	oView:SetOwnerView('VIEW_SZG','CABEC')
	oView:SetOwnerView('VIEW_SZH','GRID')

	//Habilitando ttulo
	oView:EnableTitleView('VIEW_SZG','Vendedor x Comissao')
	oView:EnableTitleView('VIEW_SZH','Itens Comissao Vendedor')

	//Fora o fechamento da janela na confirmao
	oView:SetCloseOnOk({||.T.})
Return oView

Static Function MenuDef()
	Local aRotina := {}

	ADD OPTION aRotina TITLE "Pesquisar" ACTION "PesqBrw" OPERATION 1 ACCESS 0 //"Pesquisar"
	ADD OPTION aRotina TITLE "Visualizar" ACTION "VIEWDEF.LATFAA02" OPERATION 2 ACCESS 0 //"Visualizar"
	ADD OPTION aRotina TITLE "Incluir" ACTION "VIEWDEF.LATFAA02" OPERATION 3 ACCESS 0 //"Incluir"
	ADD OPTION aRotina TITLE "Alterar" ACTION "VIEWDEF.LATFAA02" OPERATION 4 ACCESS 0 //"Alterar"
	ADD OPTION aRotina TITLE "Excluir" ACTION "VIEWDEF.LATFAA02" OPERATION 5 ACCESS 0 //"Excluir"
	ADD OPTION aRotina TITLE "Clonar" ACTION "u_clonatb()" OPERATION 2 ACCESS 0 //"Clonar"
Return ( aRotina )

user Function clonatb()
	Local cVend := space(TamSx3("A3_COD")[1])
	Local cVendOld := space(TamSx3("A3_COD")[1])
	Local cQuery := ""
	Private aPergs := {}
	

	aAdd( aPergs ,{1,"Vendedor: "    , cVend    ,"","ExistCpo('SA3',&(ReadVar()))","SA3",".T.",80,.F.})
	
	if ParamBox(aPergs,"Informe o novo Vendedor", /*aRet*/, /*bOk*/, /*aButtons*/, /*lCentered*/, /*nPosx*/, /*nPosy*/, /*oDlgWizard*/, /*cLoad*/, .F., .F.)

		if MsgNoYes("Confirma cópia para o vendedor " + alltrim(Posicione("SA3",1,xFilial("SA3")+MV_PAR01,"A3_NOME")) + " ?","ATENÇÃO")
			cVendOld := SZG->ZG_VEND
			DBSelectArea("SZG")
			DBSetOrder(1)
			IF DBSeek(xFilial("SZG")+MV_PAR01)
				FWAlertError("Vendedor já existe, cópia não será realizada", "ATENÇÃO")
				return
			endIf

			DBSelectArea("SZG")
			recLock("SZG",.t.)
			SZG->ZG_FILIAL  := xFilial("SZG")
			SZG->ZG_VEND    := MV_PAR01
			msUnLock()

			cQuery := "SELECT ZH_FILIAL,ZH_GRUPO,ZH_PRODUTO,ZH_PERC FROM " + retSqlName("SZH")
			cQuery += "	WHERE ZH_FILIAL = '" + xFilial("SZH") + "' "
			cQuery += "		AND D_E_L_E_T_ = ' ' " 
			cQuery += "		AND ZH_VEND = '" + cVendOld + "' "
			tcQuery cQuery NEW alias "_ZH"
			DBSelectArea("_ZH")
			dbGoTop()
			
			while(_ZH->(!eof()))
				DBSelectArea("SZH")
				recLock("SZH",.t.)
				SZH->ZH_FILIAL  := xFilial("SZH")
				SZH->ZH_GRUPO   := _ZH->ZH_GRUPO
				SZH->ZH_PRODUTO := _ZH->ZH_PRODUTO
				SZH->ZH_PERC    := _ZH->ZH_PERC
				SZH->ZH_VEND    := MV_PAR01
				msUnLock()
				DBSelectArea("_ZH")
				DBSkip()
			endDo
			_ZH->(DBCloseArea())
		endIf
	endIf

return


Static Function validLin(xOpc)
	Local aArea      := GetArea()
	Local oModel     := FWModelActive()
	Local oModelGrid
	Local nOperation
	Local aSaveLines := FWSaveRows()
	Local lRet       := .T.
	Local nX         := 0
	Local cValTmp    := ""

	IF oModel <> Nil
		oModelGrid    := oModel:GetModel('SZHDETAIL')
		nOperation    := oModelGrid:GetOperation() //3 == Inclusão, 4 == Alteração

		IF nOperation == 3 .or. nOperation == 4

			nLinha :=     oModelGrid:GetLine()
			oModelGrid:GoLine(nLinha)
			IF(!oModelGrid:IsDeleted())
				if !empty(oModelGrid:GetValue('ZH_PRODUTO')) .and. !empty(oModelGrid:GetValue('ZH_GRUPO'))
					lRet := .f.
					FWAlertError("Não preencha produto e grupo em uma mesma linha!", "ATENÇÃO")
				else
					if xOpc == 'P'
						DBSelectArea("SB1")
						DBSetOrder(1)
						if !DBSeek(xFilial("SB1")+oModelGrid:GetValue('ZH_PRODUTO'))
							lRet := .f.
							FWAlertError("Produto não existe no cadastro!", "ATENÇÃO")
						else
							cValTmp := oModelGrid:GetValue('ZH_PRODUTO')
							For nX := 1 to oModelGrid:Length()
								if nX == oModelGrid:Length() .and. nX == 1
									exit
								endIf
								oModelGrid:GoLine(nX)
								if cValTmp == oModelGrid:GetValue('ZH_PRODUTO') .and. nX <> nLinha
									lRet := .f.
									FWAlertError("Produto duplicado!", "ATENÇÃO")
									exit
								endIf
							Next nX
						endIf
					else
						DBSelectArea("SBM")
						DBSetOrder(1)
						if !DBSeek(xFilial("SBM")+oModelGrid:GetValue('ZH_GRUPO'))
							lRet := .f.
							FWAlertError("Grupo não existe no cadastro!", "ATENÇÃO")
						else
							cValTmp := oModelGrid:GetValue('ZH_GRUPO')
							For nX := 1 to oModelGrid:Length()
								if nX == oModelGrid:Length()  .and. nX == 1
									exit
								endIf
								oModelGrid:GoLine(nX)
								if cValTmp == oModelGrid:GetValue('ZH_GRUPO') .and. nX <> nLinha
									lRet := .f.
									FWAlertError("Grupo duplicado!", "ATENÇÃO")
									exit
								endIf
							Next nX
						endIf
					endIf
				endIf
				oModelGrid:GoLine(nLinha)
			EndIF
		EndIF

	EndIF

	FWRestRows( aSaveLines )
	RestArea(aArea)

Return (lRet)
