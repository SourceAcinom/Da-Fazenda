#INCLUDE "PROTHEUS.CH"


//utilizado no inicializado de campos da SC5
// campos sc5 browse
User Function BROWSC5(cCampo,cCampo2)
	Local cNome := ""

	If SC5->C5_TIPO == "B" .OR. SC5->C5_TIPO == "D"
		cNome := POSICIONE("SA2",1,XFILIAL("SA2")+SC5->C5_CLIENTE+SC5->C5_LOJACLI,cCampo2)
	Else
		cNome := POSICIONE("SA1",1,XFILIAL("SA1")+SC5->C5_CLIENTE+SC5->C5_LOJACLI,cCampo)
	Endif

Return cNome
