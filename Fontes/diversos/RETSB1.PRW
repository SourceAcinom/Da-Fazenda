#INCLUDE "PROTHEUS.CH"
//---------------------------------------------------------------------------------------------------------
//Retorna a Descricao do produto na visualizaï¿½ï¿½o da NFs de entrada
//Utilizado na inicializaï¿½ï¿½o Padrï¿½o do campo D1_X_DESC (Virtual)
//---------------------------------------------------------------------------------------------------------
User Function RETSB1()
	Local aArea			:= GetArea()
	Local nPosCod 		:= Ascan(aHeader,{|x| Trim(x[2]) == "D1_COD"})
	Local nPosDesc		:= Ascan(aHeader,{|x| Trim(x[2]) == "D1_X_DESC"})
	Local cDesc		   	:= Space(30)
	Local i

	For i:= 1 To Len(aCols)
		aCols[i,nPosDesc] := FDESC("SB1",aCols[i,nPosCod],"B1_DESC")

		If i == n  .and. !FUNNAME()$"XMLNFE"
			cDesc	:= aCols[i,nPosDesc]
		Else
			cDesc	:= aCols[i,nPosDesc]
		EndIf
	Next
	Restarea(aArea)
Return cDesc
