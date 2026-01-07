#Include "rwmake.ch"
#Include "protheus.ch"
#Include "totvs.ch"

/*---------------------------------------------------------------------------------------------------------------*
 | P.E.      : MT381SE                                                                                           |
 | Descrição : Ponto de Entrada que permite alterar quantidade no ajuste de empenho                              |
 | Chamada   : lMT381SE := ExecBlock('MT381SE', .F., .F.)                                                        |
 | Link      : https://tdn.totvs.com/display/public/mp/MT381SE+-+Permite+alterar+quantidade+no+ajuste+de+empenho |
 *---------------------------------------------------------------------------------------------------------------*/
User Function MT381SE()
    Local lMT381SE := .F. // Não executa a validação
Return lMT381SE
