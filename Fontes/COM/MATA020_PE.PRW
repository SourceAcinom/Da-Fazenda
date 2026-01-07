#Include "Protheus.ch"
#Include "RwMake.ch"
#Include "FWMVCDEF.ch"

#INCLUDE "PROTHEUS.CH"
// FUNÇÃO CUSTOMER VENDOR
User Function CUSTOMERVENDOR()
	Local aParam     := PARAMIXB
	Local xRet       := .T.
	Local oObj       := ''
	Local cIdPonto   := ''
	Local cIdModel   := ''
	Local lIsGrid    := .F.

	If aParam <> NIL

		oObj       := aParam[1]
		cIdPonto   := aParam[2]
		cIdModel   := aParam[3]
		lIsGrid    := ( Len( aParam ) > 3 )

		If     cIdPonto == 'MODELPOS'

		ElseIf cIdPonto == 'FORMPOS'

		ElseIf cIdPonto == 'FORMLINEPRE'

            //xRet := U_CheckUser()

		ElseIf cIdPonto == 'FORMLINEPOS'

		ElseIf cIdPonto == 'MODELCOMMITTTS'

		ElseIf cIdPonto == 'MODELCOMMITNTTS'

            /*
            If Empty(M->A2_CONTA)    
				If MsgYesNo("Deseja adicionar conta contábil ao cadastro?")           
					RecLock("SA2",.F.)
					If M->A2_EST == "EX"           
						//IMPORTAÇÃO
						REPLACE SA2->A2_CONTA WITH "211010100002"
					Else
						//DEMAIS FORNECEDORES
						REPLACE SA2->A2_CONTA WITH "211010100001"
					EndIf
					SA2->(MsUnLock())
				EndIf
            EndIf        
            */

            U_IMPITFOR("I") //Inclui item contábil individual

        ElseIf cIdPonto == 'FORMCOMMITTTSPRE'

		ElseIf cIdPonto == 'FORMCOMMITTTSPOS'

		ElseIf cIdPonto == 'MODELCANCEL'

		ElseIf cIdPonto == 'BUTTONBAR'

		EndIf   

	EndIf

Return xRet
