#INCLUDE "rwMake.ch"   
#INCLUDE "PROTHEUS.CH"
#Include "TopConn.ch"

//FABRICIO - INCLUSÃO DE NOVOS GRUPOS DE TRIBURAÇÃO DE CLIENTE- TABELA _G DA SX5
User Function MNTSX5_G()
       Local aArea := GetArea()
       dbselectarea("SX5")
       dbSetOrder(1)
       Set filter to X5_TABELA = "_G"
       AxCadastro("SX5","Cadastro de Grupos de Tributação de Cliente")
       Set Filter to
       RestArea(aArea)
Return

//INCLUIR NO X3_WHEN DOS CAMPOS X5_TABELA E X5_CHAVE
//!(FUNNAME()=="MNTSX5_G")                                    

//INCLUIR NO X3_RELACAO DO CAMPO X5_TABELA
//IIF(FUNNAME()=="MNTSX5_G","_G",.T.)
