#include 'protheus.ch'
#include 'parmtype.ch'


//PE Executado na sele��o de registros a serem processados para o bordero.
                 
User Function FA060QRY()    
Local cFiltro := "1=1"
Local cDb  			:= UPPER(AllTrim(TcGetDb()))
Local cSubst  := IIf(cDb $ "DB2|ORACLE|INFORMIX|POSTGRES", "SUBSTR", "SUBSTRING")

	If IsInCAllStack("FINA060") .And. SE1->(FieldPos("E1_X_CBARR")) > 0 .And.; //Bordero a Receber 
	(!FindFunction("U_LibAcess") .OR. U_LibAcess("TRPBOLET", .F.)) 
		If MsgYesNo("Deseja filtrar apenas os Titulos com boletos gerados?")
			cFiltro :=  cSubst + "(E1_X_CBARR,1,3) = '" + cPort060 + "'"
		Else
			cFiltro :=  "E1_X_CBARR = '" + SPACE(Len(SE1->E1_X_CBARR)) + "' OR ("+cSubst+"(E1_X_CBARR,1,3) = '" + cPort060 + "')"
		EndIf 
	ENDIF

Return(cFiltro)                         
