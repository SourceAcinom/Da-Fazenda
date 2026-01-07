#include "totvs.ch"

/*/{Protheus.doc} VLDGRP
        Funcao utilizada no gatilho C6_PRODUTO - 501 para retornar o percentual da comissao. A comissao por grupo somente será considerada, caso nao encontre comissao para o produto. O produto sempre tera prioridade sobre o grupo.
        @type  Function
        @author Matheus Oliveira - TOTVS Oeste
        @since 21/09/2025
        /*/
user Function VLDGRP(xVend,xProduto)
        Local cProd  := xProduto
        Local cVend  := xVend
        Local cGrupo := ""
        Local nPerc  := 0

        DBSelectArea("SZH")
        DBSetOrder(1)
        if DBSeek(xFilial("SZH")+cVend+cProd)
                nPerc := SZH->ZH_PERC
        else
                cGrupo := Posicione("SB1",1,xFilial("SZH")+cProd,"B1_GRUPO")
                DBSelectArea("SZH")
                DBSetOrder(2)
                if DBSeek(xFilial("SZH")+cVend+cGrupo)              
                        nPerc := SZH->ZH_PERC
                else
                        nPerc := 0
                endIf
        endIf

Return (nPerc)
