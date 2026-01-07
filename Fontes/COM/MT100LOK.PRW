# include "rwmake.ch"
# include "topconn.ch"
# include "protheus.ch"

//P.E. NA LINHA DO DOC ENTRADA - MATA100
User Function MT100LOK
	Local aArea := GetArea()
	Local lRet    := PARAMIXB[1]
    Local nPosCod   := aScan(aHeader,{|x| AllTrim(Upper(x[2])) == "D1_COD" }) 
	Local nPosConta := aScan(aHeader,{|x| AllTrim(Upper(x[2])) == "D1_CONTA" }) 
	Local nPosTes   := aScan(aHeader,{|x| AllTrim(Upper(x[2])) == "D1_TES" })

	If !isincallstack("A103Devol")

        //Atribuição de conta contábil a linha do documento de entrada
        //1º Conta débito da TES
        //2º Cadastro do produto
        //3º Cadastro de grupo de produtos

		//POSICIONA NO CADASTRO DA TES
		SF4->(DbSetOrder(1))
		SF4->(DbSeek(xFilial("SF4")+aCols[n,nPosTes]))

        aCols[n,nPosConta]:=SF4->F4_X_CD

        If Empty(aCols[n,nPosConta])
            //POSICIONA NO CADASTRO DA PRODUTO
            SB1->(DbSetOrder(1))
            SB1->(DbSeek(xFilial("SB1")+aCols[n,nPosCod]))

            aCols[n,nPosConta] := SB1->B1_CONTA

            If Empty(aCols[n,nPosConta])
                //POSICIONA NO CADASTRO DE GRUPO DE PRODUTOS
                SBM->(DbSetOrder(1))
                SBM->(DbSeek(xFilial("SBM")+SB1->B1_GRUPO))  

                aCols[n,nPosConta] := SBM->BM_X_CTA 
            EndIf
        EndIf
    Else
        aCols[n,nPosConta]:=SF4->F4_X_CD
	EndIf

    RestArea(aArea)
Return lRet
