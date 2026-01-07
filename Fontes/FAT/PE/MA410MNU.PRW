//Bibliotecas
#Include 'Protheus.ch'
#Include 'RwMake.ch'
#Include 'TopConn.ch'
 
/*------------------------------------------------------------------------------------------------------*
 | P.E.:  MA410MNU                                                                                      |
 | Desc:  Adição de opção no menu de ações relacionadas do Pedido de Vendas                             |
 | Links: http://tdn.totvs.com/display/public/mp/MA410MNU                                               |
 *------------------------------------------------------------------------------------------------------*/
  
User Function MA410MNU()
    Local aArea := GetArea()
     
    //Adicionando função de vincular
    //aadd(aRotina,{"* Imprimir","U_DMFATR01()", 0 , 4, 0 , Nil})
    AAdd(aRotina,{"* NFE SEFAZ","SPEDNFE" , 0 , 5,0,NIL})
    aAdd(aRotina,{"* Impressao P.V.","U_LATFAR02()",0,7,0, nil} )
    aAdd(aRotina,{"* Ordem Carregamento","U_LATFAR01()",0,7,0, nil} )

    RestArea(aArea)
Return
