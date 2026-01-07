#include "topconn.ch"
#INCLUDE "PROTHEUS.CH"

/*/{Protheus.doc} M460FIM
Ponto de Entrada na geracao de NFiscal de Saida para gravar campos de complementos
@type function
@author Matheus Oliveira - TOTVS Oeste
@since 16/08/2025
/*/

User Function M460FIM()
	Local _aAtu		:=GetArea()
	Local cPedido	:=''
	Local nVol		:=0

	dbSelectArea("SD2")
	dbSetorder(3)
	dbSeek(SF2->F2_FILIAL+SF2->(F2_DOC+F2_SERIE+F2_CLIENTE+F2_LOJA))
	do While D2_FILIAL==SF2->F2_FILIAL .and. D2_DOC==SF2->F2_DOC .and. D2_SERIE==SF2->F2_SERIE .and. D2_CLIENTE==SF2->F2_CLIENTE .and. D2_LOJA==SF2->F2_LOJA .and. !Eof()
		
		nVol+=SD2->D2_QUANT
		cPedido:=SD2->D2_PEDIDO
		dbSelectArea("SD2")
		dbSkip()

	Enddo

	If !Empty(cPedido)
		dbSelectArea("SC5")
		dbSetorder(1)
		If dbSeek(xfilial()+cPedido)
			dbSelectArea("SF2")
			reclock("SF2",.f.)
			If Empty(F2_VOLUME1)
				replace F2_VOLUME1 with nVol
				replace F2_ESPECI1 with "VOLUMES"
			Endif
			msunlock()
		Endif
	Endif

	RestArea(_aAtu)
	
Return(.t.)
