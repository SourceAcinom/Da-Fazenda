#Include "Protheus.ch"
#Include "TopConn.ch"

/*/
Função que preenche o código do fornecedor e a loja com os dados do CNPJ/CPF
@author Matheus Oliveira
@since 01/05/2025
/*/

User Function xCliCod(cReturnType)
	Local cCod    := ""
	Local cLoja   := ""
	Local cDoc    := M->A1_CGC
	Local cTipo   := M->A1_PESSOA

	// Se houver grupo
	If ! Empty(cDoc)
		DO CASE
		CASE cTipo == "F"
			cCod = SUBSTR(cDoc, 1, 9)
			cLoja = SUBSTR(cDoc, 10, 2)
		CASE cTipo == "J"
            cCod = SUBSTR(cDoc, 1, 8)
			cLoja = SUBSTR(cDoc, 9, 4)
		ENDCASE
	ENDIF

	// Retorna o valor baseado no parâmetro cReturnType
	DO CASE
	CASE cReturnType == "Cod"
		return cCod
	CASE cReturnType == "Loja"
		return cLoja
	OTHERWISE
		// Valor padrão, caso o parâmetro seja inválido
		return cCod
	ENDCASE
Return nil

User Function xForCod(cReturnType)
	Local cCod    := ""
	Local cLoja   := ""
	Local cDoc    := M->A2_CGC
	Local cTipo   := M->A2_TIPO

	// Se houver grupo
	If ! Empty(cDoc)
		DO CASE
		CASE cTipo == "F"
			cCod = SUBSTR(cDoc, 1, 9)
			cLoja = SUBSTR(cDoc, 10, 2)
		CASE cTipo == "J"
            cCod = SUBSTR(cDoc, 1, 8)
			cLoja = SUBSTR(cDoc, 9, 4)
		ENDCASE
	ENDIF

	// Retorna o valor baseado no parâmetro cReturnType
	DO CASE
	CASE cReturnType == "Cod"
		return cCod
	CASE cReturnType == "Loja"
		return cLoja
	OTHERWISE
		// Valor padrão, caso o parâmetro seja inválido
		return cCod
	ENDCASE
Return nil
