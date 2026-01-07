#Include "Protheus.ch"
#Include "RwMake.ch"
#Include "FWMVCDEF.ch"

#INCLUDE "PROTHEUS.CH"

User Function ITEM()
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

		ElseIf cIdPonto == 'FORMLINEPOS'

		ElseIf cIdPonto == 'MODELCOMMITTTS'

		ElseIf cIdPonto == 'MODELCOMMITNTTS'

            If Empty(M->B1_CONTA)    
                RecLock("SB1",.F.)
                If M->B1_TIPO == "MP"           
                    REPLACE SB1->B1_CONTA WITH "1150100002"
                ElseIf M->B1_TIPO == "PA"
                    REPLACE SB1->B1_CONTA WITH "1150100004"
                ElseIf M->B1_TIPO == "EM"
                    REPLACE SB1->B1_CONTA WITH "1150100005"
                EndIf
                SB1->(MsUnLock())
            EndIf        

        ElseIf cIdPonto == 'FORMCOMMITTTSPRE'

		ElseIf cIdPonto == 'FORMCOMMITTTSPOS'

		ElseIf cIdPonto == 'MODELCANCEL'

		ElseIf cIdPonto == 'BUTTONBAR'

		EndIf   

	EndIf

Return xRet
