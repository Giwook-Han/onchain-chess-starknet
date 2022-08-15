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
    uint256_mul,
    uint256_sub,
    uint256_signed_div_rem,
    Uint256,
)

const TRUE = 1
const FALSE = 0

# AI player functions

# @Yetta: need to define play depth
func negaMax{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
    }(depth : Uint256) -> (res : felt):

    let (moves) = generateMoves()
    if depth ==0:
        return (0)
    end
    if moves == 0:
        return (0)
    end 

    let bestScore = -4196
    local currentScore
    local bestMove

    

    return (res)
end

func evaluateMove{syscall_ptr : felt*,pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
    }(fromIndex : Uint256, toIndex : Uint256) -> (name : felt):
    alloc_locals
    let (piece_num_from, piece_color_from, piece_type_from) = getPiece(fromIndex)
    let (piece_num_to, piece_color_to, piece_type_to) = getPiece(toIndex)
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
    let (temp3 : felt) = 0xC * (0x23 - toIndex.low)
    let temp3uint = Uint256(temp3,0)
    let c3_uint = uint256_shr(pst_value_to_2, temp3uint)
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
    
    local res = (0xF9AF98F96F96F98F9AF9AF98F96F96F98F9AF9CF9AF98F98F9AF9B)
    return(res)
end

func getPstTwo{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr,bitwise_ptr : BitwiseBuiltin*
    }(type : Uint256) -> (res : Uint256):
    alloc_locals
    let bool = (type-5)*(type-6)
    with_attr error_message("Only eligible for Queens or Kings"):
        assert bool = 0
    end

    if type == 5:
        return (0xB30B50B50B50B40B30B20B40B50B40B40B20B00B20B30B30B20B0)
    end
    if type == 6:
        return(0xF9EF9CF9CF9CF9CF9EFA1FA1FA0FA0FA1FA1FA4FA6FA2FA2FA6FA4)
    end
end

func simpApplyMove{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(fromIndex : Uint256, toIndex : Uint256) -> (bool : felt):
    # apply the move to the board
    # input argument board is not necessary bcz we will gonna use storage_val
    alloc_locals

    return(TRUE)
end
