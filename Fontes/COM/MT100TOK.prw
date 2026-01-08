#Include "Protheus.ch"
#include "rwmake.ch" 
#INCLUDE "TOPCONN.CH"

User Function MT100TOK()
    Local lRet       := .T.
    Local nPosItem   := aScan(aHeader,{|x| Upper(Alltrim(x[2]))=="D1_ITEM"})
    Local nPosConta  := aScan(aHeader,{|x| Upper(Alltrim(x[2]))=="D1_CONTA"})
    Local nPosCC     := aScan(aHeader,{|x| Upper(Alltrim(x[2]))=="D1_CC"})
    Local nPosRateio := aScan(aHeader,{|x| Upper(Alltrim(x[2]))=="D1_RATEIO"})
    Local nPosTes    := aScan(aHeader,{|x| Upper(Alltrim(x[2]))=="D1_TES"})
    Local nPosCod    := aScan(aHeader,{|x| Upper(Alltrim(x[2]))=="D1_COD"})
    Local cCTTXTIPO  := ""
    Local nn         := 0
    Local cConta     := ""

	//Documento de entrada
	//Solicitação de transferência
	If !(FUNNAME() $ "MATA103,MATA311")
		Return lRet
	EndIF    

	If nPosTes > 0 
		For nn:=1 To Len(aCols)
			If !aCols[nn][len(aHeader)+1]

				//Posiciona na TES
				SF4->(DbSetOrder(1))
				SF4->(DbSeek(xFilial("SF4")+aCols[nn,nPosTes]))
				
				//Posiciona no cadastro do produto
				SB1->(DbSetOrder(1))
				SB1->(DbSeek(xFilial("SB1")+aCols[nn,nPosCod]))

				//Verifica se a TES contabiliza
				If SF4->F4_X_CTB=="S" 

                    //Se não movimentar estoque, obriga o centro de custos
                    If SF4->F4_ESTOQUE <> "S" .AND. (Empty(aCols[nn,nPosCC]) .AND. aCols[nn,nPosRateio] == '2')
                        MsgAlert("TES não movimenta estoque ("+aCols[nn,nPosTes]+"). Obrigatório a informação de centro de custos para o item " + AllTrim(aCols[nn,nPosItem]),"Informar Centro de Custos")
                        Return .F.
                    EndIf

                    //Se informado centro de custo e conta contábil, ajusta a conta conforme o tipo do centro de custo  
                    If !Empty(aCols[nn,nPosCC]) .AND. !Empty(aCols[nn,nPosConta])
                        cCTTXTIPO := AllTrim(GetAdvFVal('CTT','CTT_X_TIPO',xFilial('CTT')+AllTrim(aCols[nn,nPosCC]),1,""))    
                        
                        If cCTTXTIPO == "1"      //Administrativo
                            aCols[nn,nPosConta] := "431"+SubStr(AllTrim(aCols[nn,nPosConta]),4,Len(aCols[nn,nPosConta])-3)
                        ElseIf cCTTXTIPO == "2"  //Comercial
                            aCols[nn,nPosConta] := "431"+SubStr(AllTrim(aCols[nn,nPosConta]),4,Len(aCols[nn,nPosConta])-3)
                        ElseIf cCTTXTIPO == "3"  //Industrial   
                            aCols[nn,nPosConta] := "412"+SubStr(AllTrim(aCols[nn,nPosConta]),4,Len(aCols[nn,nPosConta])-3)
                        EndIf

                    EndIf

                    If !Empty(aCols[nn,nPosConta])
                        //Verifica se a conta existe no plano de contas
                        cConta:= AllTrim(GetAdvFVal('CT1','CT1_CONTA',xFilial('CT1')+AllTrim(aCols[nn,nPosConta]),1,""))
                        IF Empty(cConta)
                            MsgAlert("Conta informada inválida para o item " + AllTrim(aCols[nn,nPosItem]),"Informar conta válida")
                            Return .F.
                        EndIF
                    EndIF                    

                EndIf                
			EndIf
		Next
    EndIf      

Return lRet 
