	#will leave comment later..

%lang starknet

from starkware.cairo.common.alloc import alloc
from starkware.cairo.common.math import unsigned_div_rem, assert_not_zero
from starkware.cairo.common.math_cmp import is_le
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

# shall we manually define the engine's address here?
const ENGINE_ADDRESS = 0x0

const TRUE = 1
const FALSE = 0

# how to assign the board an initial value?
@storage_var
func board() -> (board : Uint256):
end

# func getAdjustedIndex(index : Uint256) -> (adjusted_index : Uint256):
#     let (adjusted_index : Uint256) = index.low
#     return(adjusted_index)
# end

@storage_var
func movesArray() -> (moves : felt):
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
    # @Yetta Does every local var have to be declared with 
    #   'local' var = 3?
    #   see https://www.cairo-lang.org/docs/reference/syntax.html#locals
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

    ## @Yetta: the chess is 6x6, should be less than 35?
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
    let (result : Uint256) = uint256_shr(rotateBoard, getPiece)

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

    return (bool)
end

# @Yetta: is direction vector necessary? can we use the indexChange var directly?
func checkPathUsingRecursion{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(rayStart : felt, rayEnd : felt, directionVector : felt) -> (
    bool : felt
):
    alloc_locals

    let (done) = is_le(rayStart+1,rayEnd)
    if done == FALSE:
        return (TRUE)
    end

    let bool = checkPathUsingRecursion(rayStart + directionVector, rayEnd, directionVector)
    
    #check isValidMove
    let (isValidPosition : felt) = isValid(Uint256(rayStart,0))
    #check is there any pieces in the way
    let (adjusted_board : Uint256) = getIndexAdjustedBoard(Uint256(rayStart,0))
    let (isCapturePiece : felt) = isCapture(adjusted_board)

    let (res) = (isValidPosition * isCapturePiece)

    return (res*bool)

end

func isLegalMove{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
    }(fromIndex : Uint256, toIndex : Uint256) -> (bool : felt):
    alloc_locals

    # check for out of bound issues
    with_attr error_message("Move Out of Bound"):
        assert checkInBound(fromIndex) = TRUE
        assert checkInBound(toIndex) = TRUE
    end

    let (piece_num, piece_color, piece_type) = getPiece(fromIndex)

    # check there IS a piece at from index
    with_attr error_message("No Piece at From Index"):
        assert_not_zero(piece_num)
    end

    # check the player does not move a piece of the other player
    with_attr error_message("Cannot Move the Piece of Another Player"):
        assert piece_color = getPlayerColor()
    end

    # if move doesn't follow the rule of a chess type, return false
    let (piece_num2, piece_color2, piece_type2) = getPiece(toIndex)


    return(TRUE)
end

func checkInBound{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
    }(index : Uint256) -> (bool : felt):
    alloc_locals
    let (uint_var) = uint256_and(uint256_shr(0x7E7E7E7E7E7E00, index), 1)
    let (var) = uint_var.low
    if var != 1:
        return(FALSE)
    end
    return (TRUE)
end

func getPlayerColor{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}() -> (color:felt):
    alloc_locals

    let (board_status : Uint256) = board.read()
    let (firstpiece : Uint256) = uint256_and(board_status, Uint256(15, 0))
    # get color and type of the piece; first bit represents color; the next 3 bits represent type
    let (piece_color : Uint256, piece_type : Uint256) = uint256_signed_div_rem(firstpiece, Uint256(8, 0))
    let (color) = (piece_color.low)
    return(color)

end

func isCapture{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(toIndex : Uint256) -> (bool : felt):
    # return true if the move is a capture
    # input argument board is not necessary bcz we will gonna use storage_val
    # could be consider not get adjusted_board as an argument, and just get index and use getPiece function 
    alloc_locals
    
    let (piece_num, piece_color, piece_type) = getPiece(toIndex)

    if piece_num == 0:
        return(FALSE)
    end

    let (player_color) = getPlayerColor()
    if piece_color == player_color:
        return(FALSE)
    end

    return(TRUE)

end

func isValid(toIndex : Uint256) -> (bool : felt):
    # return true if the move is valid
    # input argument board is not necessary bcz we will gonna use storage_val
    alloc_locals

    # if out of bound, return false
    # do (0x7E7E7E7E7E7E00 >> _toIndex) & 1, where the 0x string is the mapping of 0 << 63 | ... | 0 << 0
    let var1 = uint256_and(uint256_shr(0x7E7E7E7E7E7E00, toIndex), 1)
    # valid positions are 1 
    if var1 != 1:
        return(FALSE)
    end 

    # if the to index is occupied by a chess of the same color, return false
    let (piece_num, piece_color, piece_type) = getPiece(toIndex)
    let (player_color) = getPlayerColor()
    if piece_num!=0 and piece_color == player_color:
        return(FALSE)
    end

    return(TRUE)
    # @Yetta: if move doesn't follow the rule of a chess type, return false
end



# #=========================================
# @contract_interface
# namespace IEngine:
#     func makeMove(board:felt) -> (value):
#     end
# end

# @external
# func applyMove{
#     syscall_ptr : felt*,
#     pedersen_ptr : HashBuiltin*,
#     range_check_ptr
#     }(fromIndex:Uint256, toIndex:Uint256) -> ():

# # Get piece at the from index
#     # piece = (_board >> ((_move >> 6) << 2)) & 0xF;

# # check if the move is valid
#     if isLegalMove(fromIndex, toIndex) == 1:
#         # apply the move
#         # board.write()
#         #// Replace 4 bits at the from index with 0000
#         _board &= type(uint256).max ^ (0xF << ((_move >> 6) << 2));
#         #// Replace 4 bits at the to index with 0000
#         _board &= type(uint256).max ^ (0xF << ((_move & 0x3F) << 2));
#         # // Place the piece at the to index
#         _board |= (piece << ((_move & 0x3F) << 2));
#         board.rotate()

# IEngine.makeMove(
#             contract_address=ENGINE_ADDRESS, board=board
#         )
#     else:
#         with_attr error_message ("Illegal move")"):
#             assert bool = 1
#         end
#     end

# return ()
# end

# @view
# func getBoardStatus{
#     syscall_ptr : felt*,
#     pedersen_ptr : HashBuiltin*,
#     range_check_ptr
#     }() -> (res:felt):

# let (res) = board.read()
#     return(res)

# end