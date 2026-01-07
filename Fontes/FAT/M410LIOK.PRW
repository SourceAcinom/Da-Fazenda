#include "ap5mail.ch"
#include "topconn.ch"
#include "protheus.ch"
//#Command @ <nRow>,<nCol> TO <nToRow>,<nToCol> BROWSE <cAlias> [MARK <cMark>] [ENABLE <cEnable>] [ OBJECT <oBrw> ] [ FIELDS <aFields> ];
//=> [ <oBrw> := ] IW_Browse(<nRow>,<nCol>,<nToRow>,<nToCol>,<cAlias>,<cMark>,<cEnable> [,<aFields>])


/*/{Protheus.doc} M410LIOK
    Valida linhas do Pedido de Venda ref a Bonif/Troca e TES
    @type  Function
    @author Matheus Oliveira - TOTVS Oeste
    @since 12/06/2025
    /*/

User Function M410LIOK()
	Local cTesBn 	:= getmv("MV_BONUSTS")
	Local aAreaDA1  := DA1->(getarea())
	Local lRet		:= .t.
	Local nPos0		:= aScan(aHeader,{ |x| Upper(AllTrim(x[2])) == "C6_X_BONIF"})
	Local nPos1		:= aScan(aHeader,{ |x| Upper(AllTrim(x[2])) == "C6_X_TROC"})
	Local nPos2		:= aScan(aHeader,{ |x| Upper(AllTrim(x[2])) == "C6_TES"})
	Local nPosProd 	:= aScan(aHeader,{ |x| Upper(AllTrim(x[2])) == "C6_PRODUTO" })
	Local nPosQtde:= 0
	Local nPosbDoc:= 0

	if ! aCols[n,len(acols[n])]

		If (aCols[n,nPos0]>0 .or. aCols[n,nPos1]>0) .and. Posicione("SF4",1,xfilial("SF4")+aCols[n,nPos2],"F4_X_BONIF")=="S" // //Posicione("SF4",1,xfilial("SF4")+aCols[n,nPos2],"F4_X_BONIF")=="S"   --FOI COMENTADO PARA SER FUNCIONAL PARA A VENDA DIRETA - ALTERADO F4_X_BONIF ='N'
			Alert("Quantidade de Bonificacao/Troca nao autorizada para este tipo de Saida")
			lRet :=.f.
		Endif

		// Valida nao permissao de inclusao de bonificacao do sistema
		if aCols[n,nPos2] == &cTesBn
			Alert("TES de uso exclusivo para bonificacao gerada pelo sistema")
			lRet:=.f.
		endif

		// Valida bonificacao Manual
		if Posicione("SF4",1,xfilial("SF4")+aCols[n,nPos2],"F4_X_BONIF")=="S"

			cEstado:=""
			If !M->C5_TIPO$"DB"
				dbSelectArea("SA1")
				dbSetOrder(1)
				If dbSeek(xFilial()+M->C5_CLIENTE+M->C5_LOJACLI)
					cEstado:=A1_EST
				Endif
			Endif

			dbSelectArea("DA1")
			//		dbSetOrder(4)     P8
			dbSetOrder(7)  // P10
			If dbSeek(xFilial()+M->C5_TABELA+aCols[n,nPosProd]+cEstado)
				If DA1->DA1_X_PBON < 99
					Alert("Uso da TES de Bonificacao deve ser liberado na tabela de preco")
					lRet:=.f.
				endif
			endif
		endif
	endif

	RestArea(aAreaDA1)
Return(lRet)
