#include "rwmake.ch"
#include "topconn.ch"
#include "protheus.ch"

//PE para filtrar serie faturamento do pedido conforme tipo pedido

User Function SX5NOTA()

	Local _aArea   	:= GetArea()
	Local _lOk     	:= .F.
	//Local _aSerie  	:= {}
	//Local _cEmpAux 	:= ''
	//Local cNumPed	:= ''

	dbSelectArea("SX5")

	if FunName() == 'MATA460A' //Documento de saída

		dbSelectArea('SC5')
		SC5->(dbSetOrder(1))
		SC5->(dbSeek(SC9->C9_FILIAL+SC9->C9_PEDIDO))
	endif

	if FunName() == 'MATA460A' .OR. FunName() == 'MATA461' .OR. FunName() == 'MATA920' //doc saida e nf manual de saída

		IF Alltrim(SX5->X5_CHAVE)$'001-V99'
			_lOk := .T.
		Endif

	ElseIF FunName() == 'SPEDMDFE' //serie mdf-e

		If Alltrim(SX5->X5_CHAVE) == '005'
			_lOk := .T.
		Endif
	ElseIF FunName() == 'MATA103' //DOCUMENTO DE ENTRADA
		IF Alltrim(SX5->X5_CHAVE)$'002-D99'
			_lOk := .T.
		Endif
	else

		_lOk := .T.

	endif

	RestArea(_aArea)

Return _lOk
