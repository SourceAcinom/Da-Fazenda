#INCLUDE 'PROTHEUS.CH'
#INCLUDE "rwmake.ch"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TOTVS.CH"

/*/{Protheus.doc} CT105CTK
(Ponto de entrada utilizado na contabilização para verificar se deve manter as entidades contábeis Centro de Cuto, Item e Classe Valor.)
@author Ednilson
@since 02/02/2017
@version 1.0 
@return ${return}, ${return_description}
@example
(examples)
@see (links_or_references)
/*/

User Function CT105CTK()
	Local aArea		:= GetArea()
	Local aAreaCTK	:= 	CTK->(GetArea())
	Local aAreaCT5	:= 	CT5->(GetArea())
	Local aAreaCT1	:= 	CT1->(GetArea())

	DbSelectArea("CT1")
	CT1->(DbSetOrder(1))

	// Faz a análise das entidades a débito
	If CTK->CTK_DC $ "1#3"
		If CT1->(DbSeek(xFilial("CT1")+CTK->CTK_DEBITO))
			If Right(Alltrim(ParamIXB[aScan(ParamIXB,{|x| x[1] == "CT5_CCD"})][2]),4) == '+" "'
				If CT1->CT1_ACCUST != "1"
					CTK->CTK_CCD := Space(TamSX3("CTK_CCD")[1])
				EndIf
			EndIf
			If Right(Alltrim(ParamIXB[aScan(ParamIXB,{|x| x[1] == "CT5_ITEMD"})][2]),4) == '+" "'
				If CT1->CT1_ACITEM != "1"
					CTK->CTK_ITEMD := Space(TamSX3("CTK_ITEMD")[1])
				EndIf
			EndIf
			If Right(Alltrim(ParamIXB[aScan(ParamIXB,{|x| x[1] == "CT5_CLVLDB"})][2]),4) == '+" "'
				If CT1->CT1_ACCLVL != "1"
					CTK->CTK_CLVLDB := Space(TamSX3("CTK_CLVLDB")[1])
				EndIf
			EndIf
		EndIf
	EndIf

	// Faz a análise das entidades a crédito
	If CTK->CTK_DC $ "2#3"
		If CT1->(DbSeek(xFilial("CT1")+CTK->CTK_CREDIT))
			If Right(Alltrim(ParamIXB[aScan(ParamIXB,{|x| x[1] == "CT5_CCC"})][2]),4) == '+" "'
				If CT1->CT1_ACCUST != "1"
					CTK->CTK_CCC := Space(TamSX3("CTK_CCC")[1])
				EndIf
			EndIf
			If Right(Alltrim(ParamIXB[aScan(ParamIXB,{|x| x[1] == "CT5_ITEMC"})][2]),4) == '+" "'
				If CT1->CT1_ACITEM != "1"
					CTK->CTK_ITEMC := Space(TamSX3("CTK_ITEMC")[1])
				EndIf
			EndIf
			If Right(Alltrim(ParamIXB[aScan(ParamIXB,{|x| x[1] == "CT5_CLVLCR"})][2]),4) == '+" "'
				If CT1->CT1_ACCLVL != "1"
					CTK->CTK_CLVLCR := Space(TamSX3("CTK_CLVLCR")[1])
				EndIf
			EndIf
		EndIf
	EndIf

	RestArea(aAreaCT1)
	RestArea(aAreaCT5)
	RestArea(aAreaCTK)
	RestArea(aArea)
Return()
