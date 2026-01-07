#Include "Protheus.ch"

#Define CLR_RGB_BRANCO   RGB(254,254,254) //Cor Branca em RGB
#Define CLR_RGB_VERMELHO RGB(255,000,000) //Cor Vermelha em RGB
#Define CLR_RGB_PRETO    RGB(000,000,000) //Cor Preta em RGB
#Define CLR_RGB_CINZA    RGB(140,140,140) //Cor Cinza em RGB

User Function MBlkColor()
	Local aRet := {} // Se deixar assim tem o retorno padrão

	aAdd(aRet, (CLR_RGB_BRANCO)) //Cor do texto
	aAdd(aRet, (CLR_RGB_CINZA))  //Cor de fundo
Return aRet
