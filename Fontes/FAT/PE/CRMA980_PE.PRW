#Include "Protheus.ch"
#Include "RwMake.ch"
#Include "FWMVCDEF.ch"

#INCLUDE "PROTHEUS.CH"

User Function CRMA980()
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
            If Empty(M->A1_CONTA) 
				If MsgYesNo("Deseja adicionar conta contábil ao cadastro?") 
					RecLock("SA1",.F.)
					If M->A1_TIPO == "X"           
						//EXPORTAÇAO
						REPLACE SA1->A1_CONTA WITH "111040100002"

					Else
						//DEMAIS CLIENTE
						REPLACE SA1->A1_CONTA WITH "111040100001"

					EndIf
					SA1->(MsUnLock())
				EndIf
            EndIf
            */        

            U_IMPITCLI("I") //Inclui item contábil individual

        ElseIf cIdPonto == 'FORMCOMMITTTSPRE'

		ElseIf cIdPonto == 'FORMCOMMITTTSPOS'

		ElseIf cIdPonto == 'MODELCANCEL'

		ElseIf cIdPonto == 'BUTTONBAR'

		EndIf

	EndIf

Return xRet
