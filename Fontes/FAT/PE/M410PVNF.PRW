//Bibliotecas
#Include 'Protheus.ch'
#Include 'RwMake.ch'
 
/*------------------------------------------------------------------------------------------------------*
 | P.E.:  M410PVNF                                                                                      |
 | Desc:  Validação na chamada do Prep Doc Saída no Ações Relacionadas do Pedido de Venda               |
 | Links: http://tdn.totvs.com/pages/releaseview.actionçpageId=6784152                                  |
 *------------------------------------------------------------------------------------------------------*/
User Function M410PVNF()
    Local lRet := .T.
    
    dbSelectArea("SC6")
    SC6->(dbGoTop())
    If SC6->(dbSeek(xFilial("SC6")+SC5->C5_NUM))
        While (!SC6->(EOF())) .and. SC6->C6_FILIAL = SC5->C5_FILIAL .And. SC6->C6_NUM == SC5->C5_NUM

            //Atribuição de conta contábil a linha do documento de saída
            //1º Conta crédito da TES
            //2º Cadastro do produto
            //3º Cadastro de grupo de produtos

            //POSICIONA NO CADASTRO DA TES
            SF4->(DbSetOrder(1))
            SF4->(DbSeek(xFilial("SF4")+SC6->C6_TES))   

            cxCta := AllTrim(SF4->F4_X_CC)

            If Empty(cxCta)         

				//POSICIONA NO CADASTRO DA PRODUTO
				SB1->(DbSetOrder(1))
				SB1->(DbSeek(xFilial("SB1")+SC6->C6_PRODUTO))

                cxCta := AllTrim(SB1->B1_CONTA)

                If Empty(cxCta)
                    //POSICIONA NO CADASTRO DE GRUPO DE PRODUTOS
                    SBM->(DbSetOrder(1))
                    SBM->(DbSeek(xFilial("SBM")+SB1->B1_GRUPO))  

                    cxCta := AllTrim(SBM->BM_X_CTA)
                EndIf               
            EndIf

            RecLock("SC6",.F.)
            SC6->C6_CONTA := cxCta
            SC6->(MsUnlock())

            SC6->(dbSkip())
        End
    Endif    

Return lRet
