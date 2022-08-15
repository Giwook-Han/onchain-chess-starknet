func negaMax():

    let bestScore = rec1(bestScore,0)

    negaMax()
end

func rec1(bestScore,index) -> (realBestScore):
    # check index's storage_val is empty
    # and if it is, return bestScore

    let score = rec1(bestScore,index+1)

    let aaa = rec2(index's storage_val)

    if score < aaa:
        return aaa
    else 
        return score
    end:
end

func rec2(index's_storage_val) -> (bestScoreInThisIndex):
    # check index's_storage_val & 0x3F is empty
    # and if it is, return -4_196

    let bestInThisIndex = rec2(index's storage_val >> 6)

    #check score of index's_storage_val & 0x3F
    #and compare with bestInThisIndex and return bigger one

end