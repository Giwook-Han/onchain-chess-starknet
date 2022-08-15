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
    uint256_mul,
    uint256_sub,
    uint256_signed_div_rem,
    Uint256,
)

const TRUE = 1
const FALSE = 0
const initial_board = 331318787292356502577094498346479229205088297258959912939892506624
# hex: 3256430011111100000000000000000099999900BADECB000000000

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
}(rayStart : felt, rayEnd : felt, directionVector : felt) -> (
    bool : felt
):
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
    let (bool : felt) = isLegalMove(fromIndex, toIndex)

    # check if the move is valid
    with_attr error_message ("Illegal move"):
        assert bool = 1
    end

    # apply the move
    # Replace 4 bits at the from index with 0000
    let (curr_board : Uint256) = board.read()
    let (from_0 : Uint256) = uint256_shl(Uint256(0,0), fromIndex)
    let (from_0000 : Uint256) = uint256_or(curr_board, from_0)
    
    # Replace 4 bits at the to index with 0000
    let (to_0 : Uint256) = uint256_shl(Uint256(0,0), toIndex)
    let (to_0000 : Uint256) = uint256_or(from_0000, to_0)

    # Place the piece at the to index
    let (to_piece) = uint256_shl(Uint256(piece_num,0), toIndex)
    let (board_after_move : Uint256) = uint256_or(to_0000, to_piece)
    # rotate the board
    let (board_after_rotate : Uint256) = rotate(board_after_move)
    board.write(board_after_rotate)
    
    # IEngine.makeMove(
    #         contract_address=ENGINE_ADDRESS, board=board
    #     )
    
    return ()

    
end