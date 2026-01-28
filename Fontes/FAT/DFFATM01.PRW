#INCLUDE "PROTHEUS.CH"


// Função responsável por alterar peso liq, bru e volume do pedido de venda
User Function DFFATM01()
    
    Local aArea := GetArea()
    Local aPergs := {}
    Local nPesLiquido := 0
    Local nPesBruto := 0
    Local nVolume := 0

    aAdd(aPergs, {1, "Peso Líquido", nPesLiquido, X3Picture("C5_PESOL"), "Positivo()", "", ".T.", 80, .F.})
    aAdd(aPergs, {1, "Peso Bruto", nPesLiquido, X3Picture("C5_PBRUTO"), "Positivo()", "", ".T.", 80, .F.})
    aAdd(aPergs, {1, "Volume", nPesLiquido, X3Picture("C5_VOLUME1"), "Positivo()", "", ".T.", 80, .F.})

    IF Parambox(aPergs, "Informe os parâmetros")
        nPesLiquido := MV_PAR01
        nPesBruto   := MV_PAR02
        nVolume     := MV_PAR03

        RecLock("SC5", .F.)
            SC5->C5_PESOL := nPesLiquido
            SC5->C5_PBRUTO := nPesBruto
            SC5->C5_VOLUME1 := nVolume
        SC5->(MSUNLOCK())
    EndIf

    FWAlertSuccess("Peso e Volume alterados com sucesso!", "Alteração Realizada")

    RestArea(aArea)

Return Nil  
