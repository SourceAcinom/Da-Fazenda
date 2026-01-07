#include "totvs.ch"


/*/{Protheus.doc} RETPES
      Funcao para retornar o peso do produto. Utilizado no gatilho C6_X_QTAUX
      @type  Function
      @author Matheus Oliveira - TOTVS Oeste
      @since 19/08/2025
      /*/
user Function RETPES()

      Local nPeso := 0 

      if (SB1->B1_UM == 'KG', M->C6_X_QTAUX*SB1->B1_PESO,M->C6_X_QTAUX)

      endIf


      
      
Return 
