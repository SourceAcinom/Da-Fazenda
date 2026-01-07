#INCLUDE "PROTHEUS.CH"

// Funcao para tratar nome de cliente e fornecedor no pedido de vendas em campos virtuais e de enderecos
// usado para verificar nome de fornecedores no ambiente de entradas
User Function CpoCliFor(cCampo,cCampo2,cCodCliFor,cTipo,cAmbiente)
	// cCampo		- campo a ser retornando quando cliente
	// cCampo2		- campo a ser retornado quando fornecedor
	// cCodCliFor   - Codigo+Loja do Cliente/Fornecedor                                                    
	// cTipo        - D(evolucao) / B(eneficiamento) / N(ormal) etc
	// cAmbiente 	- E-ntrada ou S-aida  
	// Exemplo chamando funcao de um inicializador padrao  -> U_CpoCliFor("A1_NOME","A2_NOME",(SF2->F2_CLIENTE+F2-LOJA),SF2->F2_TIPO,"S")                                                     
	Local cResult      := "" 
	Local aArea        := GetArea()

	If cAmbiente<>"E"                
		If alltrim(cTipo)$"DB"  
			cResult := Posicione("SA2",1,xfilial("SA2")+cCodCliFor,cCampo2)
		Else
			cResult := Posicione("SA1",1,xfilial("SA1")+cCodCliFor,cCampo)
		Endif 
	Else
		If alltrim(cTipo)$"DB"  
			cResult := Posicione("SA1",1,xfilial("SA1")+cCodCliFor,cCampo2)
		Else
			cResult := Posicione("SA2",1,xfilial("SA2")+cCodCliFor,cCampo)
		Endif 
	Endif

	RestArea(aArea)       
Return (cResult)
