#will leave comment later..

%lang starknet

from starkware.cairo.common.alloc import alloc
from starkware.cairo.common.math import unsigned_div_rem, assert_not_zero
from starkware.cairo.common.math_cmp import is_le
from starkware.cairo.common.bitwise import bitwise_xor
from starkware.cairo.common.cairo_builtins import HashBuiltin, BitwiseBuiltin
from starkware.cairo.common.find_element import search_sorted
from starkware.cairo.common.uint256 import (
    uint256_or,
    uint256_xor,
    uint256_shl,
    uint256_shr,
    uint256_and,
    uint256_le,
    uint256_lt,
    uint256_add,
    uint256_mul,
    uint256_sub,
    uint256_signed_div_rem,
    Uint256,
)

#bool
const TRUE = 1
const FALSE = 0
const initial_board = 331318787292356502577094498346479229205088297258959912939892506624
# hex: 3256430011111100000000000000000099999900BADECB000000000

#pieces in chess
const PWAN = 1
const BISHOP = 2
const ROOK = 3
const KNIGHT = 4
const QUEEN = 5
const KING = 6


# how to assign the board an initial value?
@storage_var
func board() -> (board : Uint256):
end

# func getAdjustedIndex(index : Uint256) -> (adjusted_index : Uint256):
#     let (adjusted_index : Uint256) = index.low
#     return(adjusted_index)
# end

#movesArray's index is starts from 0
#let's store next empty index of the movesArray in index 0's first 4 bits
# @Yetta: needs to discuss how movesArray can be used in other AI functions. 
@storage_var
func movesArray(index : felt) -> (moves : Uint256):
end

# func append(movesArray:felt*) -> ():
# end

# ========================================

# get the piece of the index
@view
func getPiece{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(index : Uint256) -> (piece_num : felt, piece_type : felt, piece_color : felt):
    
    alloc_locals
    let (getIndex) = getIndexAdjustedBoard(index)
    let (piece : Uint256) = uint256_and(getIndex, Uint256(15, 0))
    # get color and type of the piece; first bit represents color; the next 3 bits represent type
    let (piece_color : Uint256, piece_type : Uint256) = uint256_signed_div_rem(piece, Uint256(8, 0))
    # all this three instance is smaller than 2**128
    return (piece.low, piece_color.low, piece_type.low)
end

#getIndexAdjustedBoard
# this function gets the adjusted board sequence
# where the requested index is in the first 4 digits. 
func getIndexAdjustedBoard{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(index : Uint256) -> (indexAdjustedBoard : Uint256):
    alloc_locals

    with_attr error_message("index should be less than 63"):
        let (IsIndexBiggerThan63) = uint256_le(index, Uint256(63, 0))
        assert IsIndexBiggerThan63 = TRUE
    end
    # do '(board >> (index << 2)) & 0xF'
    # get current board state
    let (boardStatus : Uint256) = board.read()
    let (indexAdjustedBoard : Uint256) = uint256_shr(boardStatus, Uint256(index.low * 4, 0))
    return (indexAdjustedBoard)
end

# rotate a board
func rotate{range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(board : Uint256) -> (
    rotatedBoard : Uint256
):
    alloc_locals

    # if the board is 0, return
    if board.low + board.high == 0:
        return (Uint256(0, 0))
    end

    # shift the board 4 times to right
    let (rightShiftedBoard  : Uint256) = uint256_shr(board, Uint256(4, 0))
    # then use recursion with shifted board
    let (rotateBoard : Uint256) = rotate(rightShiftedBoard)
    # do the 'rotateBoard >> board & 0xF'
    let (getPiece : Uint256) = uint256_and(board, Uint256(15, 0))
    let (mid : Uint256) = uint256_shr(rotateBoard, Uint256(4,0)) # rotateBoard >> 4, right shift 4 bits
    let (result : Uint256) = uint256_or(mid, getPiece)
    return (result)
end

# returns if there is a clear path
# along a direction vector from one index to another
# every Argument is smaller than 2**128
func searchRay{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(fromIndex : Uint256, toIndex : Uint256, directionVector : felt) -> (
    bool : felt
):
    alloc_locals

    local indexChange = 0
    local rayStart = 0
    local rayEnd = 0

    # check which index is bigger
    let (isToBiggerThanFrom) = uint256_lt(fromIndex, toIndex)

    if isToBiggerThanFrom == TRUE:
        indexChange = toIndex.low - fromIndex.low
        rayStart = fromIndex.low + directionVector
        rayEnd = toIndex.low
    else:
        indexChange = fromIndex.low - toIndex.low
        rayStart = toIndex.low
        rayEnd = fromIndex.low - directionVector
    end

    # indexChange should be n*directionVector
    let (_, rem) = unsigned_div_rem(indexChange, directionVector)
    if rem != 0:
        return (FALSE)
    end

    let (bool) = checkPathUsingRecursion(rayStart, rayEnd, directionVector)
    let (isValidPosition : felt) = isValid(Uint256(rayEnd,0))
    let res = bool * isValidPosition 
    return (res)
end

# direction vector shows the direction a piece can go to
func checkPathUsingRecursion{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(rayStart : felt, rayEnd : felt, directionVector : felt) -> (bool : felt):
    alloc_locals

    let (done) = is_le(rayStart+1,rayEnd) 
    if done == FALSE:
        return (TRUE)
    end

    let (bool) = checkPathUsingRecursion(rayStart + directionVector, rayEnd, directionVector)
    
    #check isValidMove
    let (isValidPosition : felt) = isValid(Uint256(rayStart,0))
    #check is there any pieces in the way
    let (adjusted_board : Uint256) = getIndexAdjustedBoard(Uint256(rayStart,0))
    let (isCapturePiece : felt) = isCapture(adjusted_board)
    let (flipCapture : felt) = bitwise_xor(isCapturePiece, 1)
    let res_mid = isValidPosition*flipCapture
    let res = res_mid*bool
    return (res)

end

# generate every possible move
# let indexOfSquare start from 36 (6*6)
# let index : Uint256 = Uint256(0x238A179D71B69959551349138D30B289,0xDB5D33CB1BADB2BAA99A59)
# use single Uint256 and if it is full stor, clear it and start fill it again
func generateMove{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(indexOfSquare : felt, index : Uint256) -> (movesSoFar : Uint256):
    alloc_locals

    if indexOfSquare == 0:
        return (Uint256(0,0))
    end

    local isFinal
    if indexOfSquare == 36:
        assert isFinal = TRUE
    else:
        assert isFinal = FALSE
    end

    #do index >>= 6
    let (newIndex : Uint256) = uint256_shr(index,Uint256(6,0))

    let (movesSoFar : Uint256) = generateMove(indexOfSquare -1, newIndex)
 
    # get 6bits
    let (pieceIndex : Uint256) = uint256_and(newIndex,Uint256(63,0))
    # get piece of it
    let (_,pieceType,pieceColor) = getPiece(pieceIndex)
    # get adjustedBoard
    let (adjustedBoard : Uint256) = getIndexAdjustedBoard(pieceIndex)

    let (nowTurn : Uint256) = uint256_and(newIndex,Uint256(1,0))

    #need to make this clean ;(
    # if the piece is not there, return
    # or if the piece color is not mach with trun, return
    if pieceType == 0:
        return (movesSoFar)
    end
    if pieceColor - nowTurn.low != 0:
        return (movesSoFar)
    end

    # if piece is pwan
    # the tricky part of this is PWAN may can go all of 4 direction(Ldiagnal and Rdiagnal and forward and two steps forward) 
    # or can go one of that direction or none of that direction.
    # need to check that using if statment, and should make sure that single Uint256 variable has that information
    if pieceType == PWAN:
        # check Diagnal ##################################################################################

        # check diagnal sqr has enemy pieces
        let (getLeftDiagnalIndex : Uint256) = uint256_and(index,Uint256(28,0)) # 28 = 7 * 4
        let (canCaptureLeft) = isCapture(getLeftDiagnalIndex)

        # and if it is, add a path
        #PWAN's legal move
        local afterCheckLeft : Uint256
        if canCaptureLeft == TRUE:
            let (allocValue : Uint256) = appendMoves(movesSoFar,pieceIndex,Uint256(pieceIndex.low+8,0),isFinal)
            assert (afterCheckLeft) = allocValue

            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        else:
            assert afterCheckLeft = movesSoFar

            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        end

        # check diagnal sqr has enemy pieces
        let (getRightDiagnalIndex : Uint256) = uint256_and(index,Uint256(36,0)) # 36 = 9 * 4
        let (canCaptureRight) = isCapture(getRightDiagnalIndex)

        # and if it is, add a path
        #PWAN's legal move
        local afterCheckRight : Uint256
        if canCaptureRight == TRUE:
            let (allocValue : Uint256) = appendMoves(afterCheckLeft,pieceIndex,Uint256(pieceIndex.low+8,0),isFinal)
            assert afterCheckRight = allocValue

            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        else:
            assert afterCheckRight = afterCheckLeft

            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        end

        # avoid revoked refer
        tempvar syscall_ptr : felt* = syscall_ptr
        tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
        tempvar range_check_ptr = range_check_ptr
        tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr

        # Check Front Also ###############################################################################

        # check front sqr is empty
        let (getFrontSquare : Uint256) = uint256_shr(adjustedBoard,Uint256(32,0)) # 32 = 8 * 4
        let (checkEmpty : Uint256) = uint256_and(getFrontSquare,Uint256(15,0))

        # if front is empty, you should check this is the first time to move this pwan
        # and if is the first time, check one more step forward and if there is empty, then this pwan can move 2 steps forward
        if checkEmpty.low == 0:
            # append this move and get move
            let (afterCheckFront : Uint256) = appendMoves(afterCheckRight,pieceIndex,Uint256(pieceIndex.low+8,0),isFinal)

            # check this is first move Pwan's Starting Line is 16 to 23 and this value(16 to 23) >> 2 is 2
            let (isItFirstMove : Uint256) = uint256_shr(pieceIndex,Uint256(3,0))
            # check 2 steps forward sqr is empty
            let (get2StepsForwardSquare : Uint256) = uint256_shr(adjustedBoard,Uint256(64,0)) # 64 = 16 * 4
            let (check2StepsForwardIsEmpty : Uint256) = uint256_and(get2StepsForwardSquare,Uint256(15,0))
            # isItFirstMove == 2 & check2StepsForwardIsEmpty == 0
            if (isItFirstMove.low - 2) + check2StepsForwardIsEmpty.low == 0:
                # let a = (movesSoFar << 12) | fromIndex << 6 | toIndex
                let (res : Uint256) = appendMoves(afterCheckFront,pieceIndex,Uint256(pieceIndex.low+8,0),isFinal)
                
                # avoid revoked refer
                tempvar syscall_ptr : felt* = syscall_ptr
                tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
                tempvar range_check_ptr = range_check_ptr
                tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr

                return (res)
            else:
                # avoid revoked refer
                tempvar syscall_ptr : felt* = syscall_ptr
                tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
                tempvar range_check_ptr = range_check_ptr
                tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr

                return (afterCheckFront)
            end
        else:
            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
            
            return (afterCheckRight)
        end

    else:
        # if piece is knight or king
        if (pieceType - KNIGHT)*(pieceType - KING) == 0:
            # Piece is a knight or a king.
            # Knights and kings always only have 8 positions to check relative to their
            # current position, and the relative distances are always the same. For
            # knights, positions to check are ±{6, 10, 15, 17}. This is bitpacked into
            # `0x060A0F11` to reduce code redundancy. Similarly, the positions to check for
            # kings are ±{1, 7, 8, 9}, which is `0x01070809` when bitpacked.
            local directions : Uint256

            if pieceType == KNIGHT:
                assert directions = Uint256(0x060A0F11,0)
                let (mid,_) = checkKnightNKingsMove(pieceIndex,directions,movesSoFar,'ADD',FALSE)
                let (res,_) = checkKnightNKingsMove(pieceIndex,directions,mid,'SUB',isFinal)
                
                # avoid revoked refer
                tempvar syscall_ptr : felt* = syscall_ptr
                tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
                tempvar range_check_ptr = range_check_ptr
                tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr

                # finish to find every possible wa
                return (res)
            else:
                assert directions = Uint256(0x060A0F11,0)
                let (mid,_) = checkKnightNKingsMove(pieceIndex,directions,movesSoFar,'ADD',FALSE)
                let (res,_) = checkKnightNKingsMove(pieceIndex,directions,mid,'SUB',isFinal)

                return (res)
            end


        # other all sliding pieces
        else:
            if pieceType == ROOK:
            
                # Check straight moves
                # each index has 4 bits so do *4 every direction

                #check right straight moves
                let (checkRightWay : Uint256) = checkSlidingPiecesMove(pieceIndex,1*4,movesSoFar,FALSE)
                #check left straight moves
                let (checkLeftWay : Uint256) = checkSlidingPiecesMove(pieceIndex,-1*4,checkRightWay,FALSE)                
                #check upper straight moves
                let (checkUpperWay : Uint256) = checkSlidingPiecesMove(pieceIndex,8*4,checkLeftWay,FALSE)                
                #check lower straight moves
                let (checkRightWay : Uint256) = checkSlidingPiecesMove(pieceIndex,-8*4,checkUpperWay,isFinal)

                return (checkRightWay)
            end

            if pieceType == BISHOP:
            
                # Check diagnal moves
                # each index has 4 bits so do *4 every direction

                #check upper right straight moves
                let (checkUpperRightWay : Uint256) = checkSlidingPiecesMove(pieceIndex,9*4,movesSoFar,FALSE)
                #check upper left straight moves
                let (checkUpperLeftWay : Uint256) = checkSlidingPiecesMove(pieceIndex,7*4,checkUpperRightWay,FALSE)                
                #check lower right straight moves
                let (checkLowerRightWay : Uint256) = checkSlidingPiecesMove(pieceIndex,-9*4,checkUpperLeftWay,FALSE)                
                #check lower left straight moves
                let (checkLowerLeftWay : Uint256) = checkSlidingPiecesMove(pieceIndex,-7*4,checkLowerRightWay,isFinal)

                return (checkLowerLeftWay)
            end

            if pieceType == QUEEN:
            
                # Check diagnal + straight moves
                # each index has 4 bits so do *4 every direction

                #check right straight moves
                let (checkRightWay : Uint256) = checkSlidingPiecesMove(pieceIndex,1*4,movesSoFar,FALSE)
                #check left straight moves
                let (checkLeftWay : Uint256) = checkSlidingPiecesMove(pieceIndex,-1*4,checkRightWay,FALSE)                
                #check upper straight moves
                let (checkUpperWay : Uint256) = checkSlidingPiecesMove(pieceIndex,8*4,checkLeftWay,FALSE)                
                #check lower straight moves
                let (checkLowerWay : Uint256) = checkSlidingPiecesMove(pieceIndex,-8*4,checkUpperWay,isFinal)
                #check upper right straight moves
                let (checkUpperRightWay : Uint256) = checkSlidingPiecesMove(pieceIndex,9*4,checkLowerWay,FALSE)
                #check upper left straight moves
                let (checkUpperLeftWay : Uint256) = checkSlidingPiecesMove(pieceIndex,7*4,checkUpperRightWay,FALSE)                
                #check lower right straight moves
                let (checkLowerRightWay : Uint256) = checkSlidingPiecesMove(pieceIndex,-9*4,checkUpperLeftWay,FALSE)                
                #check lower left straight moves
                let (checkLowerLeftWay : Uint256) = checkSlidingPiecesMove(pieceIndex,-7*4,checkLowerRightWay,isFinal)

                return (checkLowerLeftWay)
            end

            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        end
        
            #if this code is exequte, that mean this function has an error lol
            with_attr error_message("Check generateMove function"):
                assert 1 = 0
            end
        return (Uint256(0,0))
    end

end

# check pieces valid moves
func checkKnightNKingsMove{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(pieceIndex : Uint256, directions : Uint256, movesSoFar : Uint256, addOrSub : felt ,isFinal : felt) -> (newMoveInfo : Uint256, lastVaildMove : Uint256):
    alloc_locals
    #check direction is empty
    if directions.low == 0:
        return (movesSoFar,Uint256(0,0))
    end 
    # check is this last time to call appendMoves function and is this the last recursion
    local isItFinal
    if isFinal == TRUE:
        assert isItFinal = TRUE
    else:
        assert isItFinal = FALSE
    end

    # direction >> 8
    let (newDirections : Uint256) = uint256_shr(directions,Uint256(8,0))
    # call recursively
    let (newMoves : Uint256, lastMove : Uint256) = checkKnightNKingsMove(pieceIndex, newDirections, movesSoFar, addOrSub, FALSE)

    # get 8 bits from directions, which is single direction
    let (direction : Uint256) = uint256_and(directions,Uint256(0xFF,0))
    # and add to pieceIndex so that can get a destination's index
    local destination : Uint256
    if addOrSub == 'ADD':
        let (getDestination : Uint256,_) = uint256_add(pieceIndex,direction)
        assert destination = getDestination
    else:
        let (getDestination : Uint256,_) = uint256_add(pieceIndex,direction)
        assert destination = getDestination
    end

    local afterCheckLastMove : Uint256
    local afterCheckValid : Uint256
    
    let (checkValid : felt) = isValid(destination)
    if checkValid == TRUE:
        let (allocMoves : Uint256) = appendMoves(newMoves,pieceIndex,destination,isItFinal)
        assert afterCheckValid = allocMoves
        # store last move
        assert afterCheckLastMove = destination

        # avoid revoked refer
        tempvar syscall_ptr : felt* = syscall_ptr
        tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
        tempvar range_check_ptr = range_check_ptr
        tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
    else:
        assert afterCheckValid = newMoves
        # point last move
        assert afterCheckLastMove = lastMove


        # if this is last chance to call appendMoves function BUT if not a Valid move
        # then store last vaild move to storage_val
        if isItFinal == 1:
            # remove last stored move (6 bits fromIndex, 6 bits toIndex 12bits total)
            let (toStoreMoves : Uint256) = uint256_shr(afterCheckValid,Uint256(12,0))
            let (res : Uint256) = appendMoves(newMoves,pieceIndex,lastMove,isItFinal)
            
            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        else:

            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        end

    end

    return (afterCheckValid,afterCheckLastMove)
end

# check sliding pieces avaliable move
func checkSlidingPiecesMove{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(pieceIndex : Uint256, direction : felt, movesSoFar : Uint256,isFinal : felt) -> (newMoveInfo : Uint256):
    alloc_locals

    local isItFinal
    if isFinal == TRUE:
        assert isItFinal = TRUE
    else:
        assert isItFinal = FALSE
    end
    
    let (checkIsVaild) = isValid(Uint256(pieceIndex.low + direction,0))
    # if it is not valid move, return it
    if checkIsVaild == FALSE:
        return (movesSoFar)
    end

    let (checkIsCapture) = isCapture(Uint256(pieceIndex.low + direction,0))
    # if it is capture move, store Data and return it
    if checkIsCapture == TRUE:
        let (res : Uint256) = appendMoves(movesSoFar, pieceIndex, Uint256(pieceIndex.low + direction,0),isItFinal)
        return (res)
    end

    # if it is valid and also not capture any pieces, check next direction.
    let (newMoveInfo : Uint256) = checkSlidingPiecesMove(Uint256(pieceIndex.low + direction,0),direction + direction,movesSoFar,FALSE)

    # and also append this move
    let (result : Uint256) = appendMoves(movesSoFar, pieceIndex, Uint256(pieceIndex.low + direction,0),isItFinal)

    return (result)

end


# apped moves to storage_val if movesArray is full or the final appends
# moveInfo is single Uint256 that moves is stored
func appendMoves{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(moveInfo : Uint256, fromIndex : Uint256, toIndex : Uint256, is_final : felt) -> (newMoveInfo : Uint256):
    alloc_locals

    let (checkIsFull : Uint256) = uint256_shr(moveInfo,Uint256(246,0))
    # if checkIsFull.low != 0 then moveInfo is full and if it's full we need to store that value
    if checkIsFull.low != 0:
        # let's store last index of the movesArray in index 0's first 4 bits
        let (getIndexArray : Uint256) = movesArray.read(0)
        # do (getIndexArray >> 252) & 0xF this give me to the last index of the 
        let (getIndex : Uint256) = uint256_shr(getIndexArray,Uint256(252,0))
        let (index : Uint256) = uint256_and(getIndex,Uint256(15,0))

        # write it to the storage
        movesArray.write(index.low,moveInfo)

        # make index + 1 and store to the index 0's first 4 bits
        let (newIndex : Uint256) = uint256_shl(Uint256(index.low+1,0),Uint256(252,0)) #is this should be 251? 
        #make Uint256 that only first 4 bits are 0 and the other are 1
        let toEraseIndex : Uint256 = Uint256(0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,0x0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
        # do (getIndexArray & toEraseIndex) | newIndex
        let (erasedIndex : Uint256) = uint256_and(getIndexArray,toEraseIndex)
        let (completeUint : Uint256) = uint256_or(erasedIndex,newIndex)
        # write a new index
        movesArray.write(0,completeUint)

        # make new moveInfo Instance
        let makeNewMoveInfo : Uint256 = Uint256(fromIndex.low,0)
        let (newMoveInfo : Uint256) = uint256_shl(makeNewMoveInfo, Uint256(6,0))
        let (res : Uint256) = uint256_or(newMoveInfo,Uint256(toIndex.low,0))

        # avoid revoked refer
        tempvar syscall_ptr : felt* = syscall_ptr
        tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
        tempvar range_check_ptr = range_check_ptr
        tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr

        return (res)
    # if not, it means that moveInfo is not full so you can insert moves
    else:
        # moveInfo << 12
        let (shl2MoveInfo : Uint256) = uint256_shl(moveInfo,Uint256(12,0))
        # make move ((fromIndex << 6) | toIndex)
        let (shl2FromIndex : Uint256) = uint256_shl(fromIndex,Uint256(6,0))
        let (move : Uint256) = uint256_or(shl2FromIndex,toIndex)

        let (res : Uint256) = uint256_or(shl2MoveInfo,move)

        # indexOfSquare's max value is 36 (board is 6*6)
        # so if indexOfSquare value is 36, this is the last time to call this function
        # so need to store value to the storage_val
        if is_final == TRUE: 
            # let's store last index of the movesArray in index 0's first 4 bits
            let (getIndexArray : Uint256) = movesArray.read(0)
            # do (getIndexArray >> 252) & 0xF this give me to the last index of the 
            let (getIndex : Uint256) = uint256_shr(getIndexArray,Uint256(252,0))
            let (index : Uint256) = uint256_and(getIndex,Uint256(15,0))

            # write it to the storage
            movesArray.write(index.low,moveInfo)

            # make index + 1 and store to the index 0's first 4 bits
            let (newIndex : Uint256) = uint256_shl(Uint256(index.low+1,0),Uint256(252,0)) #is this should be 251? 
            #make Uint256 that only first 4 bits are 0 and the other are 1
            let toEraseIndex : Uint256 = Uint256(0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,0x0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
            # do (getIndexArray & toEraseIndex) | newIndex
            let (erasedIndex : Uint256) = uint256_and(getIndexArray,toEraseIndex)
            let (completeUint : Uint256) = uint256_or(erasedIndex,newIndex)
            # write a new index
            movesArray.write(0,completeUint)

            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr

        else:
            # avoid revoked refer
            tempvar syscall_ptr : felt* = syscall_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
            tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        end

        # avoid revoked refer
        tempvar syscall_ptr : felt* = syscall_ptr
        tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
        tempvar range_check_ptr = range_check_ptr
        tempvar bitwise_ptr : BitwiseBuiltin* = bitwise_ptr
        
        return (res)
    end
end


func isLegalMove{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
    }(fromIndex : Uint256, toIndex : Uint256) -> (bool : felt):
    alloc_locals

    # check for out of bound issues
    with_attr error_message("Move Out of Bound"):
        let (from_inbound) = checkInBound(fromIndex)
        let (to_inbound) = checkInBound(toIndex)
        assert from_inbound = TRUE
        assert to_inbound = TRUE
    end

    let (piece_num, piece_color, piece_type) = getPiece(fromIndex)

    # check there IS a piece at from index
    with_attr error_message("No Piece at From Index"):
        assert_not_zero(piece_num)
    end

    # check the player does not move a piece of the other player
    with_attr error_message("Cannot Move the Piece of Another Player"):
        let (player_color) = getPlayerColor()
        assert piece_color = player_color
    end

    #let (piece_num2, piece_color2, piece_type2) = getPiece(toIndex)

    let toIndexfelt = toIndex.low
    let fromIndexfelt = fromIndex.low
    let (comparison) = is_le(toIndexfelt, fromIndexfelt)

    local indexChange
    let (isCapturePiece) = isCapture(toIndex)
    let (isValidMove) = isValid(toIndex)

    let toIndexFelt = toIndex.low
    let toIndexFelt_1 = toIndexFelt - 8
    let toIndex_1 = Uint256(toIndexFelt_1, 0)
    let (isValidMove_1) = isValid(toIndex_1)

    #@Yetta: this seemed to work. 
    if comparison == TRUE:
        assert indexChange = fromIndex.low - toIndex.low
    else:
        assert indexChange = toIndex.low - fromIndex.low
    end

    # piece type is 1 for pawn, 2 for rook, 3 for knight, 4 for bishop, 5 for queen, 6 for king
    # pawns
    if piece_type == 1:
        with_attr error_message("Pawns Cannot Move Backwards"):
            assert comparison = FALSE
        end
        # if pawn goes diagnally, check if there is a piece in the way
        let bool_7_9 = (indexChange-7)*(indexChange-9)

        if bool_7_9 == 0:
            # assert isCapturePiece = isCapture(toIndex)
            if isCapturePiece == FALSE: 
                return(FALSE)
            end
        end

        # moves one step forward, check validity. 
        if indexChange == 8:
            if isValidMove == FALSE:
                return(FALSE)
            end
        end 
        # moves two step forward, check validity.
        if indexChange == 16:
            if isValidMove == FALSE:
                return(FALSE)
            end
            if isValidMove_1 == FALSE:
                return(FALSE)
            end
        end
    end

    let indexChangeUint =  Uint256(indexChange,0)
    # knight 
    let possible_moves_knight = Uint256(0x28440,0)
    let (shifted_knight : Uint256) = uint256_shr(possible_moves_knight, indexChangeUint)
    let (resUint_knight : Uint256) = uint256_and(shifted_knight, Uint256(1,0))
    let res_knight = resUint_knight.low
    if piece_type == 4:
        # let possible_moves = Uint256(0x28440,0)
        # move is not legal
        if res_knight == 0:
            return(FALSE)
        end
        # check if move is valid
        if isValidMove == FALSE:
            return(FALSE)
        end
    end 

    # king 
    let possible_moves_king = Uint256(0x382,0)
    let (shifted_king : Uint256) = uint256_shr(possible_moves_king, indexChangeUint)
    let (resUint_king : Uint256) = uint256_and(shifted_king, Uint256(1,0))
    let res_king = resUint_king.low
    if piece_type == 6:
        if res_king == 0:
            return(FALSE)
        end
        # check if move is valid
        if isValidMove == FALSE:
            return(FALSE)
        end
    end 

    let (bool_lr : felt) = searchRay(fromIndex, toIndex, 1)
    let (bool_fb : felt) = searchRay(fromIndex, toIndex, 8)
    let bool_horizontal = bool_lr*bool_fb

    let (bool_diag_1 : felt) = searchRay(fromIndex, toIndex, 7)
    let (bool_diag_2 : felt) = searchRay(fromIndex, toIndex, 9)
    let bool_diagonal = bool_diag_1*bool_diag_2

    let bool_queen = bool_horizontal + bool_diagonal

    # rook 
    if piece_type == 2:
        if bool_horizontal == 0:
            return(FALSE)
        end
    end 

    # bishop
    if piece_type == 3:
        if bool_diagonal == 0:
            return(FALSE)
        end
    end

    # queen
    if piece_type == 5:
        if bool_queen == 0:
            return(FALSE)
        end
    end

    # use Engine's function to evaluate if the move results in winning or failing.

    return(TRUE)
end

func checkInBound{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
    }(index : Uint256) -> (bool : felt):
    alloc_locals
    let map = Uint256(0x7E7E7E7E7E7E00,0)
    let (adjusted_board : Uint256) = uint256_shr(map, index)
    let (uint_var) = uint256_and(adjusted_board, Uint256(1,0))
    let var = uint_var.low
    if var != 1:
        return(FALSE)
    end
    return (TRUE)
end

func getPlayerColor{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}() -> (color:felt):
    alloc_locals

    let (board_status : Uint256) = board.read()
    let(color_uint) = uint256_and(board_status,Uint256(1,0))
    let color = color_uint.low

    return (color)
end

func isCapture{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(toIndex : Uint256) -> (bool : felt):
    # return true if the move is a capture
    # input argument board is not necessary bcz we will gonna use storage_val
    # could be consider not get adjusted_board as an argument, and just get index and use getPiece function 
    alloc_locals
    
    let (piece_num, piece_color, piece_type) = getPiece(toIndex)

    # check if empty
    if piece_num == 0:
        return(FALSE)
    end
    # check if it's your own piece
    let (player_color) = getPlayerColor()
    if piece_color == player_color:
        return(FALSE)
    end

    return(TRUE)

end

func isValid{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(toIndex : Uint256) -> (bool : felt):
    # return true if the move is valid
    # input argument board is not necessary bcz we will gonna use storage_val
    alloc_locals

    # if out of bound, return false
    # do (0x7E7E7E7E7E7E00 >> _toIndex) & 1, where the 0x string is the mapping of 0 << 63 | ... | 0 << 0
    let (var) = checkInBound(toIndex)
    # valid positions are 1 
    if var == FALSE:
        return(FALSE)
    end 

    # if the to index is occupied by a chess of the same color, return false
    let (piece_num, piece_color, piece_type) = getPiece(toIndex)
    let (player_color) = getPlayerColor()
    if piece_num + piece_color - player_color == piece_num:
        return(FALSE)
    end

    return(TRUE)

end


@contract_interface
namespace IEngine:
    func makeMove(board:felt) -> (value):
    end
end

@constructor
func constructor{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}():
    #1 0000 0000 0000 0000 0000 0000 0000 0000
    #2 0000 0011 0010 0101 0110 0100 0011 0000
    #3 0000 0001 0001 0001 0001 0001 0001 0000
    #4 0000 0000 0000 0000 0000 0000 0000 0000
    #5 0000 0000 0000 0000 0000 0000 0000 0000
    #6 0000 1001 1001 1001 1001 1001 1001 0000
    #7 0000 1011 1010 1101 1110 1100 1011 0000
    #8 0000 0000 0000 0000 0000 0000 0000 0000

    # binary: 0000000000000000000000000000000000000011001001010110010000110000000000010001000100010001000100000000000000000000000000000000000000000000000000000000000000000000000010011001100110011001100100000000101110101101111011001011000000000000000000000000000000000000
    let initial = 331318787292356502577094498346479229205088297258959912939892506624
    # hex: 3256430011111100000000000000000099999900BADECB000000000
    return()
end

#                                Black
#                       00 00 00 00 00 00 00 00                    Black
#                       00 00 02 05 06 02 03 00                 ♜ ♝ ♛ ♚ ♝ ♜
#                       00 01 01 01 01 01 01 00                 ♟ ♟ ♟ ♟ ♟ ♟
#                       00 00 00 00 00 00 00 00     denotes
#                       00 00 00 00 00 00 00 00    the board
#                       00 09 09 09 09 09 00 00                 ♙ ♙ ♙ ♙ ♙ ♙
#                       00 11 12 13 14 12 11 00                 ♖ ♘ ♕ ♔ ♘ ♖
#                       00 00 00 00 00 00 00 *01*                    White
#                                White

@view
func getBoardStatus{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
    }() -> (res : Uint256):

    let (res) = board.read()
    return(res)
end

@external 
func applyMove{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr,
    bitwise_ptr : BitwiseBuiltin*
    }(fromIndex:Uint256, toIndex:Uint256) -> ():

    alloc_locals
    # Get piece at the from index
    # piece = (board >> ((_move >> 6) << 2)) & 0xF;
    let (piece_num, piece_color, piece_type) = getPiece(fromIndex)
    let piece = Uint256(piece_num,0)
    let (bool : felt) = isLegalMove(fromIndex, toIndex)

    # check if the move is valid
    with_attr error_message ("Illegal move"):
        assert bool = 1
    end

    let (curr_board : Uint256) = board.read()
    let largest_uint = Uint256(0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

    # apply the move
    # Replace 4 bits at the from index with 0000
    # _board &= type(uint256).max ^ (0xF << (fromIndex << 2));

    let (fromIndex_shifted) = uint256_shl(fromIndex, Uint256(2,0))
    let (temp1) = uint256_shl(Uint256(0xF,0), fromIndex_shifted)
    let (temp2) = uint256_xor(largest_uint, temp1)
    let (from_0000) = uint256_and(curr_board, temp2)

    # let (from_0 : Uint256) = uint256_shl(Uint256(0,0), fromIndex)
    # let (from_0000 : Uint256) = uint256_or(curr_board, from_0)
    
    # Replace 4 bits at the to index with 0000
    # _board &= type(uint256).max ^ (0xF << (toIndex << 2));
    let (toIndex_shifted) = uint256_shl(toIndex, Uint256(2,0))
    let (temp4) = uint256_shl(Uint256(0xF,0), toIndex_shifted)
    let (temp5) = uint256_xor(largest_uint, temp4)
    let (to_0000 : Uint256) = uint256_and(from_0000, temp5)

    # Place the piece at the to index
    # _board |= (piece << (toIndex << 2))
    let (to_piece) = uint256_shl(piece, toIndex_shifted)
    let (board_after_move : Uint256) = uint256_or(to_0000, to_piece)
    # rotate the board
    let (board_after_rotate : Uint256) = rotate(board_after_move)

    board.write(board_after_rotate)
    
    # IEngine.makeMove(
    #         contract_address=ENGINE_ADDRESS, board=board
    #     )
    
    return ()

    
end