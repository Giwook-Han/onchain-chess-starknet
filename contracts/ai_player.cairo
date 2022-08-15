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
    uint256_not,
    uint256_shr,
    uint256_and,
    uint256_le,
    uint256_lt,
    uint256_mul,
    uint256_sub,
    uint256_signed_div_rem,
    Uint256,
)
from contracts.chess_board import board, movesArray, rotate, generateMove, appendMoves, checkKnightNKingsMove, checkSlidingPiecesMove

const TRUE = 1
const FALSE = 0

# AI player functions

func searchMove{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(board : Uint256, depth : Uint256) -> (bestMove : Uint256, bool : felt):
    # github copilot please stop
    # initialized values? @Yetta board and movesArray
    let (movesArray) = generateMove(board)
    if movesArray.low == 0:
        return (Uint256(0,0), FALSE)
    end
    let (bestScore,bestMove) = rec1(board,0)
    if is_le(bestScore, -1261)==TRUE:
        return (Uint256(0,0), FALSE)
    end
    return (bestMove, TRUE)
end

func rec1{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(board : Uint256, index : Uint256) -> (realBest : felt, realBestMove : Uint256):
    alloc_locals
    # check index's storage_val is empty
    # and if it is, return bestScore
    let (aMovesArray : Uint256) = movesArray.read(index)
    if aMovesArray.low == 0:
        return (-4196, Uint256(0,0))
    end

    let (score, move) = rec1(board, index+1)
    let (aaa, bbb) = rec2(board, aMovesArray)

    if is_le(score,aaa-1) == TRUE:
        return (aaa, bbb)
    else:
        return (score, move)
    end
end

func rec2{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(board : Uint256, aMovesArray : Uint256) -> (bestScore : felt, bestMove : Uint256):
    alloc_locals

    let (aMovesArray_shifted) = uint256_shr(aMovesArray,Uint256(12,0))

    # check index's_storage_val & 0xFFF is empty
    # and if it is, return -4_196
    let (lastMove) = uint256_and(aMovesArray,Uint256(0xFFF,0))
    if lastMove == 0:
        return (-4196, Uint256(0,0))
    end

    #check score of index's_storage_val & 0x3F
    #and compare with currentScore and return bigger one
    let (bestScore, bestMove) = rec2(board, aMovesArray_shifted)

    let (currMove) = uint256_and(aMovesArray,Uint256(0xFFF,0))

    let (temp1 : Uint256) = uint256_shr(lastMove,Uint256(6,0))
    let (fromIndex : Uint256) = uint256_and(temp1,Uint256(0x3F,0))
    let (toIndex : Uint256) = uint256_and(lastMove,Uint256(0x3F,0))
    let (currScore_eval : felt) = evaluateMove(board, fromIndex, toIndex)
    let (sim_board : Uint256) = simApplyMove(currMove,board)

    let (currScore : felt) = currScore_eval + negaMax(sim_board, depth - 1)

    if is_le(bestScore,currScore) == TRUE:
        return (currScore, currMove)
    else:
        return (bestScore, bestMove)
    end
end


# @Yetta: need to define play depth
func negaMax{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(sim_board : Uint256, depth : Uint256) -> (res : felt):
    alloc_locals
    if depth ==0:
        return (0)
    end
    if movesArray(0) == 0:
        return (0)
    end

    # the best score in all possible moves stored in MovesArray
    let (bestScore : felt, bestMove : Uint256) = realBest(simBoard,0)

    # ((_board >> ((bestMove & 0x3F) << 2)) & 7)
    let (temp1) = uint256_and(bestMove, Uint256(0x3F))
    let (temp2) = uint256_shl(temp1, Uint256(2,0))
    let (toIndex_shifted) = uint256_shr(sim_board, temp2)
    let (temp4) = uint256_and(toIndex_shifted, Uint256(7,0))
    # if the king is captured
    if temp4 == 6:
        return (-4000)
    end

    # _board & 1
    let temp5 = uint256_and(sim_board, Uint256(1,0))
    # bestScore + negaMax(_board.applyMove(bestMove), _depth - 1)
    let depth_1 = uint256_sub(depth, Uint256(1,0))

    let sim_board_1 = simApplyMove(bestMove,sim_board)
    let res1 = bestScore + negaMax(sim_board_1, depth_1)
    let res2 = negaMax(sim_board_1, depth_1) - bestScore

    # if _board & 1 == 0, meaning the player is black
    if temp5 == 0:
        return (res1)
    else:
        return (res2)
    end
end

# find the best move in all arrays of moves
func realBest{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(board : Uint256, index : Uint256) -> (realBest : felt, realBestMove : Uint256):
    alloc_locals
    # check index's storage_val is empty
    # and if it is, return bestScore
    let (aMovesArray : Uint256) = movesArray.read(index)
    if aMovesArray.low == 0:
        return (-4196, Uint256(0,0))
    end

    let (score, move) = realBest(board, index+1)
    let (aaa, bbb) = findBestLocal(board, aMovesArray)

    if is_le(score,aaa-1) == TRUE:
        return (aaa, bbb)
    else:
        return (score, move)
    end
end

# find the best move in a given array of moves
func findBestLocal{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(board : Uint256, aMovesArray : Uint256) -> (bestScore : felt, bestMove : Uint256):
    alloc_locals

    let (aMovesArray_shifted) = uint256_shr(aMovesArray,Uint256(12,0))

    # check index's_storage_val & 0xFFF is empty
    # and if it is, return -4_196
    let (lastMove) = uint256_and(aMovesArray,Uint256(0xFFF,0))
    if lastMove == 0:
        return (-4196, Uint256(0,0))
    end

    #check score of index's_storage_val & 0x3F
    #and compare with currentScore and return bigger one
    let (bestScore, bestMove) = findBestLocal(board, aMovesArray_shifted)

    let (currMove) = uint256_and(aMovesArray,Uint256(0xFFF,0))

    let (temp1 : Uint256) = uint256_shr(lastMove,Uint256(6,0))
    let (fromIndex : Uint256) = uint256_and(temp1,Uint256(0x3F,0))
    let (toIndex : Uint256) = uint256_and(lastMove,Uint256(0x3F,0))
    let (currScore : felt) = evaluateMove(board, fromIndex, toIndex)

    if is_le(bestScore,currScore) == TRUE:
        return (currScore, currMove)
    else:
        return (bestScore, bestMove)
    end
end

func evaluateMove{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(board : Uint256,fromIndex : Uint256, toIndex : Uint256) -> (res : felt):
    alloc_locals
    let (piece_num_from, piece_color_from, piece_type_from) = simGetPiece(board, fromIndex)
    let (piece_num_to, piece_color_to, piece_type_to) = simGetPiece(board,toIndex)
    local captureValue
    local newPst
    local oldPst

    let (pst_value_from : Uint256) = getPst(piece_type_from)
    let (pst_value_from_2 : Uint256) = getPstTwo(piece_type_from)
    let (pst_value_to : Uint256) = getPst(piece_type_to)
    let (pst_value_to_2 : Uint256) = getPstTwo(piece_type_to)

    # (getPst(pieceAtToIndex) >> (7 * (0x23 - toIndex))) & 0x7F
    let (temp1 : felt) = 7 * (0x23 - toIndex.low)
    let temp1uint = Uint256(temp1,0)
    let c1_uint = uint256_shr(pst_value_to, temp1uint)
    let c1 = uint256_and(c1_uint, Uint256(0x7F,0))

    # getPst(pieceAtToIndex) >> (0xC * (0x11 - toIndex))) & 0xFFF
    let (temp2 : felt) = 0xC * (0x11 - toIndex.low)
    let temp2uint = Uint256(temp2,0)
    let c2_uint = uint256_shr(pst_value_to, temp2uint)
    let c2 = uint256_and(c2_uint, Uint256(0xFFF,0))

    # (getPstTwo(pieceAtToIndex) >> (0xC * (0x23 - toIndex))) & 0xFFF
    let (toIndex_shifted : felt) = 0xC * (0x23 - toIndex.low)
    let toIndex_shifteduint = Uint256(toIndex_shifted,0)
    let c3_uint = uint256_shr(pst_value_to_2, toIndex_shifteduint)
    let c3 = uint256_and(c3_uint, Uint256(0xFFF,0))

    # calculate capture value. 
    if piece_type != 0: 
        # not king or queen
        if is_le(piece_type,4) == TRUE:
            assert captureValue = c1
        else: 
        # piece is king or queen in closer half of the board
            if is_le(toIndex.low,17)==TRUE:
                assert captureValue = c2
            else:
        # king or queen in the further half
                assert captureValue = c3
            end
        end
    end

    # (getPst(pieceAtFromIndex) >> (7 * fromIndex)) & 0x7F
    let (temp4 : felt) = 7 * fromIndex.low
    let temp4uint = Uint256(temp4,0)
    let o1_uint = uint256_shr(pst_value_from, temp4uint)
    let o1 = uint256_and(o1_uint, Uint256(0x7F,0))

    # (getPst(pieceAtFromIndex) >> (7 * toIndex)) & 0x7F;
    let (temp5 : felt) = 7 * toIndex.low
    let temp5uint = Uint256(temp5,0)
    let n1_uint = uint256_shr(pst_value_from, temp5uint)
    let n1 = uint256_and(n1_uint, Uint256(0x7F,0))

    # (getPstTwo(pieceAtFromIndex) >> (0xC * fromIndex)) & 0xFFF
    let (temp6 : felt) = 0xC * fromIndex.low
    let temp6uint = Uint256(temp6,0)
    let o2_uint = uint256_shr(pst_value_from_2, temp6uint)
    let o2 = uint256_and(o2_uint, Uint256(0xFFF,0))

    # (getPst(pieceAtFromIndex) >> (0xC * (fromIndex - 0x12))) & 0xFFF
    let (temp7 : felt) = 0xC * (fromIndex.low - 0x12)
    let temp7uint = Uint256(temp7,0)
    let o3_uint = uint256_shr(pst_value_from, temp7uint)
    let o3 = uint256_and(n2_uint, Uint256(0xFFF,0))

    if is_le(piece_type,4)==TRUE:
        assert oldPst = o1
        assert newPst = n1
    else:
        # piece is king or queen in closer half of the board
        if is_le(fromIndex.low,17)==TRUE:
            assert oldPst = o2
        else:
        # king or queen in the further half
            assert oldPst = o3
        end
    end

    # (getPstTwo(pieceAtFromIndex) >> (0xC * toIndex)) & 0xFFF
    let (temp8 : felt) = 0xC * toIndex.low
    let temp8uint = Uint256(temp8,0)
    let n2_uint = uint256_shr(pst_value_from_2, temp8uint)
    let n2 = uint256_and(n2_uint, Uint256(0xFFF,0))

    # (getPst(pieceAtFromIndex) >> (0xC * (toIndex - 0x12))) & 0xFFF
    let (temp9 : felt) = 0xC * (toIndex.low - 0x12)
    let temp9uint = Uint256(temp9,0)
    let n3_uint = uint256_shr(pst_value_from, temp9uint)
    let n3 = uint256_and(n3_uint, Uint256(0xFFF,0))

    if is_le(toIndex.low,17)==TRUE:
        assert newPst = n2
    else:
        assert newPst = n3
    end

    let res = (captureValue + newPst) - (oldPst)
    return (res)
end

func getPst{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(type : Uint256) -> (res : Uint256):

    alloc_locals
    if type == 1: 
        return(0x2850A142850F1E3C78F1E2858C182C50A943468A152A788103C54A142850A14)
    end 

    if type == 2: 
        return(0x7D0204080FA042850A140810E24487020448912240810E1428701F40810203E)
    end 
        
    if type == 3:
        return(0xC993264C9932E6CD9B365C793264C98F1E4C993263C793264C98F264CB97264)
    end 

    if type == 4: 
        return(0x6CE1B3670E9C3C8101E38750224480E9D4189120BA70F20C178E1B3874E9C36)
    end 

    if type == 5:
        return(0xB00B20B30B30B20B00B20B40B40B40B40B20B30B40B50B50B40B3)
    end
    
    let res = 0xF9AF98F96F96F98F9AF9AF98F96F96F98F9AF9CF9AF98F98F9AF9B
    return(res)
end

func getPstTwo{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
}(type : Uint256) -> (res : Uint256):

    alloc_locals

    let bool = (type-5)*(type-6)

    with_attr error_message("PstTwo value only applies to Queens or Kings"):
        assert bool = 0
    end

    if type == 5:
        return (0xB30B50B50B50B40B30B20B40B50B40B40B20B00B20B30B30B20B0)
    end
    if type == 6:
        return(0xF9EF9CF9CF9CF9CF9EFA1FA1FA0FA0FA1FA1FA4FA6FA2FA2FA6FA4)
    end
end

func simApplyMove{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(Move : Uint256, sim_board : Uint256) -> (sim_board : Uint256):
    # apply the move to a given board
    # input argument board is not necessary bcz we will gonna use storage_val
    alloc_locals

    let toIndex = uint256_and(Move, Uint256(0x3F,0))
    let temp = uint256_shr(Move, Uint256(6,0))
    let fromIndex = uint256_and(temp, Uint256(0x3F,0))

    let curr_board = sim_board
    # get the piece at the from index
    let (piece_num_felt,_,_) = simGetPiece(curr_board, fromIndex)
    let (piece) = Uint256(piece_num_felt,0)

    # Replace 4 bits at the from index with 0000
    # _board &= type(uint256).max ^ (0xF << ((_move >> 6) << 2));
    # fromIndexshifted = ((_move >> 6) << 2)
    let (fromIndex_shifted) = uint256_shl(fromIndex, Uint256(2,0))
    # (0xF << fromIndexshifted)
    let (temp1) = uint256_shl(Uint256(0xF,0), fromIndex_shifted)
    let largest_uint = Uint256(0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
    let (temp2) = uint256_xor(largest_uint, temp1)
    let (from_0000) = uint256_and(curr_board, temp2)

    # Replace 4 bits at the to index with 0000
    # _board &= type(uint256).max ^ (0xF << (toIndex << 2));
    let (toIndex_shifted) = uint256_shl(toIndex, Uint256(2,0))
    let (temp4) = uint256_shl(Uint256(0xF,0), toIndex_shifted)
    let (temp5) = uint256_xor(largest_uint, temp4)
    let (to_0000 : Uint256) = uint256_and(from_0000, temp5)

    # Place the piece at the to index
    # _board |= (piece << (toIndex << 2))
    let (to_piece) = uint256_shl(piece, toIndex_shifted)
    # mask the move with the piece
    let (board_after_move : Uint256) = uint256_or(to_0000, to_piece)
    # rotate the board
    let (board_after_rotate : Uint256) = rotate(board_after_move)

    return(board_after_rotate)

end

func simGetPiece{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(board : Uint256, index : Uint256) -> (piece_num : felt, piece_type : felt, piece_color : felt):
    alloc_locals

    with_attr error_message("index should be less than 63"):
        let (IsIndexBiggerThan63) = uint256_le(index, Uint256(63, 0))
        assert IsIndexBiggerThan63 = TRUE
    end
    # do '(board >> (index << 2)) & 0xF'
    let (getIndex : Uint256) = uint256_shr(board, Uint256(index.low * 4, 0))
    let (piece : Uint256) = uint256_and(getIndex, Uint256(15, 0))
    # get color and type of the piece; first bit represents color; the next 3 bits represent type
    let (piece_color : Uint256, piece_type : Uint256) = uint256_signed_div_rem(piece, Uint256(8, 0))
    # all this three instance is smaller than 2**128
    return (piece.low, piece_color.low, piece_type.low)

end