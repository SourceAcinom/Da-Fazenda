
//utilizado no inicializado de campos da SC5
// ini padrão campos virtuais SC5
User Function C5NOMCLI(cCampo,cCampo2)
	Local cNome := ""

	If M->C5_TIPO == "B" .OR. M->C5_TIPO == "D"
		dbSelectArea("SA2")
		dbSetOrder(1)
		If dbSeek(xFilial("SA2")+M->C5_CLIENTE+M->C5_LOJACLI)
			cNome := cCampo2
		Endif
	Else
		dbSelectArea("SA1")
		dbSetOrder(1)
		If dbSeek(xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI)
			cNome := cCampo
		Endif
	Endif

Return cNome
