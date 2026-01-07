

user function MT242CLS()
    local aret := aClone(acols)
    local npos

    for npos := 1 to Len(aret)
        aret[npos,2] := GetAdvFVal('SB1','B1_DESC',fwxfilial('SB1')+aret[npos,1],1,'')
    next
return aret
