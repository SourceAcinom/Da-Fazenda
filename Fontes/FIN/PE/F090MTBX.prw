#include "rwmake.ch"
 
User Function F090MTBX
Local nPos := 0
Local aDescMotBx := ParamIxb[1]
 
nPos := AScan( aDescMotBx, "DEBITO CC" )
                If nPos > 0         
                               cMotBx := aDescMotBx[ nPos ]
 
                EndIf
 
Return cMotBx
