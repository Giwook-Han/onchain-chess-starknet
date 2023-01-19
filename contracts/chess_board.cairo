// this project starts from 5/9's onChain chess on eth
// LINK: https://github.com/fiveoutofnine/fiveoutofnine-chess

// the main thing is, we can represent 6x6 chess board with only one Uint256 val

// Uint256 has 256 bits, so high 128 bits will gonna upside of the board,
// and low 128 bits will gonna lowside of the board

// Each chess piece is defined with 4 bits as follows:
//      * The first bit denotes the color (0 means black; 1 means white).
//      * The last 3 bits denote the type:
//          | Bits | # | Type   |
//          | ---- | - | ------ |
//          | 000  | 0 | Empty  |
//          | 001  | 1 | Pawn   |
//          | 010  | 2 | Bishop |
//          | 011  | 3 | Rook   |
//          | 100  | 4 | Knight |
//          | 101  | 5 | Queen  |
//          | 110  | 6 | King   |

// so each sequare is 4 bits
// so the initial status of the boad will look like this

///                                Black
///                       00 00 00 00 00 00 00 00                    Black
///                       00 03 02 05 06 02 03 00                 ♜ ♝ ♛ ♚ ♝ ♜
///                       00 01 01 01 01 01 01 00                 ♟ ♟ ♟ ♟ ♟ ♟
///                       00 00 00 00 00 00 00 00     denotes
///                       00 00 00 00 00 00 00 00    the board
///                       00 09 09 09 09 09 09 00                 ♙ ♙ ♙ ♙ ♙ ♙
///                       00 11 12 13 14 12 11 00                 ♖ ♘ ♕ ♔ ♘ ♖
///                       00 00 00 00 00 00 00 01                    White
///                                White

///              1 0000 0000 0000 0000 0000 0000 0000 0000
///              2 0000 0011 0010 0101 0110 0100 0011 0000
///              3 0000 0001 0001 0001 0001 0001 0001 0000
///              4 0000 0000 0000 0000 0000 0000 0000 0000
///              5 0000 0000 0000 0000 0000 0000 0000 0000
///              6 0000 1001 1001 1001 1001 1001 1001 0000
///              7 0000 1011 1010 1101 1110 1100 1011 0000
///              8 0000 0000 0000 0000 0000 0000 0000 0001 <- this 4 bits determines which player is in turn

///              1 0000 0000 0000 0000 0000 0000 0000 0000 
///              2 0000 0000 0010 0101 0110 0011 0011 0000
///              3 0000 0001 0001 0001 0001 0001 0001 0000
///              4 0000 0000 0000 0000 0000 0000 0000 0000
///              5 0000 0000 0000 0000 0000 0000 1001 0000
///              6 0000 1001 1001 1001 1001 1001 0000 0000 
///              7 0000 1011 1010 1101 1110 1100 1011 0000 
///              8 0000 0000 0000 0000 0000 0000 0000 0001
///

/// there are 64 ``indices'' to access:
///                       63 62 61 60 59 58 57 56
///                       55 54 53 52 51 50 49 48
///                       47 46 45 44 43 42 41 40
///                       39 38 37 36 35 34 33 32
///                       31 30 29 28 27 26 25 24
///                       23 22 21 20 19 18 17 16
///                       15 14 13 12 11 10 09 08
///                       07 06 05 04 03 02 01 00
/// All numbers in the figure above are in decimal representation.
/// For example, the piece at index 27 is accessed with ``(board >> (27 << 2)) & 0xF''.


%lang starknet
%builtins pedersen range_check bitwise

from starkware.cairo.common.alloc import alloc
from starkware.cairo.common.math import unsigned_div_rem, assert_not_zero
from starkware.cairo.common.math_cmp import is_le, is_in_range, is_nn
from starkware.cairo.common.bitwise import bitwise_xor
from starkware.cairo.common.cairo_builtins import HashBuiltin, BitwiseBuiltin
from starkware.cairo.common.find_element import search_sorted
from starkware.starknet.common.syscalls import get_contract_address, get_caller_address
from starkware.cairo.common.uint256 import (
    uint256_or,
    uint256_xor,
    uint256_shl,
    uint256_shr,
    uint256_and,
    uint256_le,
    uint256_lt,
    uint256_add,
    uint256_sub,
    uint256_signed_div_rem,
    Uint256,
)

// bool
const TRUE = 1;
const FALSE = 0;

// pieces in chess
const PWAN = 1;
const BISHOP = 2;
const ROOK = 3;
const KNIGHT = 4;
const QUEEN = 5;
const KING = 6;

const INITIAL_BOARD_LOW = 0x00000000099999900BADECB000000001;
const INITIAL_BOARD_HIGH =  0x32564300111111000000000;

//
// @storage_var
//

// movesArray's index is starts from 0
// let's store next empty index of the movesArray in index 0's first 4 bits
@storage_var
func movesArray(player_addr: felt, index: felt) -> (moves: Uint256) {
}

@storage_var
func board(player_addr: felt) -> (board: Uint256) {
}

//
// @view
//

@view
func get_board_status{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    board: Uint256
) {
    let player_addr : felt = get_caller_address();
    let (res : Uint256) = board.read(player_addr);

    return (board = res);
}

@view
func get_palyer_board{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(addr : felt) -> (
    board: Uint256
) {
    let player_addr : felt = addr;
    let (res : Uint256) = board.read(player_addr);

    return (board = res);
}

@view
func get_moves_array{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    index: felt
) -> (move: Uint256) {
    let (player_addr : felt) = get_caller_address();
    let (res) = movesArray.read(player_addr,index);
    return (move = res);
}

// get the piece of the index
@view
func get_piece{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(index: Uint256) -> (piece_num: felt, piece_type: felt, piece_color: felt) {
    alloc_locals;
    let (getIndex) = get_index_adjusted_board(index);
    let (piece: Uint256) = uint256_and(getIndex, Uint256(15, 0));
    // get color and type of the piece; first bit represents color; the next 3 bits represent type
    let (piece_color: Uint256, piece_type: Uint256) = uint256_signed_div_rem(piece, Uint256(8, 0));
    // all this three instance is smaller than 2**128
    return (piece.low, piece_type.low, piece_color.low);
}

///
/// Inner function
///

// get_index_adjusted_board
// this function gets the adjusted board sequence
// where the requested index is in the first 4 digits.
func get_index_adjusted_board{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(index: Uint256) -> (indexAdjustedBoard: Uint256) {
    alloc_locals;

    with_attr error_message("index should be less than 63. Got index = {index}"){
        let (IsIndexBiggerThan63) = uint256_le(index, Uint256(63, 0));
        assert IsIndexBiggerThan63 = TRUE;
    }

    // do '(board >> (index << 2)) & 0xF'
    // get current board state
    let (board_status: Uint256) = get_board_status();
    let (index_adjusted_board: Uint256) = uint256_shr(board_status, Uint256(index.low * 4, 0));
    return (indexAdjustedBoard = index_adjusted_board);
}

// clear MovesArray
func clear_moves_array{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: felt) {
    
    if (index == 0) {
        let (player_addr : felt) = get_caller_address();
        movesArray.write(player_addr,0, Uint256(0, 0));
        return ();
    }

    clear_moves_array(index - 1);

    let (player_addr : felt) = get_caller_address();
    movesArray.write(player_addr, index, Uint256(0, 0));
    return ();
}

// check this move is legal or ilegal
func is_legal_move{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(fromIndex: Uint256, toIndex: Uint256) -> (bool: felt) {
    alloc_locals;

    // check for out of bound issues
    with_attr error_message("Move Out of Bound") {
        let (from_inbound) = check_in_bound(fromIndex);
        let (to_inbound) = check_in_bound(toIndex);
        assert from_inbound = TRUE;
        assert to_inbound = TRUE;
    }

    let (piece_num, piece_color, piece_type) = get_piece(fromIndex);

    // check there IS a piece at from index
    with_attr error_message("No Piece at From Index") {
        assert_not_zero(piece_num);
    }

    // check the player does not move a piece of the other player
    with_attr error_message("Cannot Move the Piece of Another Player") {
        let (player_color) = get_player_color();
        assert piece_color = player_color;
    }

    // let (piece_num2, piece_color2, piece_type2) = get_piece(toIndex)

    let to_index_felt = toIndex.low;
    let from_index_felt = fromIndex.low;
    let comparison = is_le(to_index_felt, from_index_felt);

    local indexChange;
    let (is_capture_piece) = is_capture(toIndex);
    let (is_valid_move) = is_valid(toIndex);

    let toIndexFelt = toIndex.low;
    let toIndexFelt_1 = toIndexFelt - 8;
    let toIndex_1 = Uint256(toIndexFelt_1, 0);
    let (_vMove_1) = is_valid(toIndex_1);

    // @Yetta: this seemed to work.
    if (comparison == TRUE) {
        assert indexChange = fromIndex.low - toIndex.low;
    } else {
        assert indexChange = toIndex.low - fromIndex.low;
    }

    // piece type is 1 for pawn, 2 for rook, 3 for knight, 4 for bishop, 5 for queen, 6 for king
    // pawns
    if (piece_type == PWAN) {
        with_attr error_message("Pawns Cannot Move Backwards") {
            assert comparison = FALSE;
        }
        // if pawn goes diagnally, check if there is a piece in the way
        let bool_7_9 = (indexChange - 7) * (indexChange - 9);

        if (bool_7_9 == 0) {
            // assert is_capture_piece = is_capture(toIndex)
            if (is_capture_piece == FALSE) {
                return (bool = FALSE);
            }
        }

        // moves one step forward, check validity.
        if (indexChange == 8) {
            if (is_valid_move == FALSE) {
                return (bool = FALSE);
            } else {
                return (bool = TRUE);
            }
        }
        // moves two step forward, check validity.
        if (indexChange == 16) {
            if (is_valid_move == FALSE) {
                return (bool = FALSE);
            }
            if (_vMove_1 == FALSE) {
                return (bool = FALSE);
            } else {
                return (bool = TRUE);
            }
        } else {
            return (bool = FALSE);
        }
    }

    let indexChangeUint = Uint256(indexChange, 0);
    // knight
    let possible_moves_knight = Uint256(0x28440, 0);
    let (shifted_knight: Uint256) = uint256_shr(possible_moves_knight, indexChangeUint);
    let (resUint_knight: Uint256) = uint256_and(shifted_knight, Uint256(1, 0));
    let res_knight = resUint_knight.low;
    if (piece_type == 4) {
        // let possible_moves = Uint256(0x28440,0)
        // move is not legal
        if (res_knight == 0) {
            return (bool = FALSE);
        }
        // check if move is valid
        if (is_valid_move == FALSE) {
            return (bool = FALSE);
        }
    }

    // king
    let possible_moves_king = Uint256(0x382, 0);
    let (shifted_king: Uint256) = uint256_shr(possible_moves_king, indexChangeUint);
    let (resUint_king: Uint256) = uint256_and(shifted_king, Uint256(1, 0));
    let res_king = resUint_king.low;
    if (piece_type == 6) {
        if (res_king == 0) {
            return (bool = FALSE);
        }
        // check if move is valid
        if (is_valid_move == FALSE) {
            return (bool = FALSE);
        }
    }

    let (bool_lr: felt) = searchRay(fromIndex, toIndex, 1);
    let (bool_fb: felt) = searchRay(fromIndex, toIndex, 8);
    let bool_horizontal = bool_lr * bool_fb;

    let (bool_diag_1: felt) = searchRay(fromIndex, toIndex, 7);
    let (bool_diag_2: felt) = searchRay(fromIndex, toIndex, 9);
    let bool_diagonal = bool_diag_1 * bool_diag_2;

    let bool_queen = bool_horizontal + bool_diagonal;

    // rook
    if (piece_type == 2) {
        if (bool_horizontal == 0) {
            return (bool = FALSE);
        }
    }

    // bishop
    if (piece_type == 3) {
        if (bool_diagonal == 0) {
            return (bool = FALSE);
        }
    }

    // queen
    if (piece_type == 5) {
        if (bool_queen == 0) {
            return (bool = FALSE);
        }
    }

    // use Engine's function to evaluate if the move results in winning or failing.

    return (bool = TRUE);
}

// check index is in bound (on board)
func check_in_bound{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(index: Uint256) -> (bool: felt) {
    alloc_locals;
    let map = Uint256(0x7E7E7E7E7E7E00, 0);
    let (adjusted_board: Uint256) = uint256_shr(map, index);
    let (uint_var) = uint256_and(adjusted_board, Uint256(1, 0));
    let var = uint_var.low;
    if (var != 1) {
        return (bool = FALSE);
    }
    return (bool = TRUE);
}

func get_player_color{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}() -> (color: felt) {
    alloc_locals;

    let (board_status: Uint256) = get_board_status();
    let (color_uint) = uint256_and(board_status, Uint256(1, 0));
    let color = color_uint.low;

    return (color = color);
}


// check this move capture any enemy's piece
func is_capture{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(toIndex: Uint256) -> (bool: felt) {
    // return true if the move is a capture
    // input argument board is not necessary bcz we will gonna use storage_val
    // could be consider not get adjusted_board as an argument, and just get index and use get_piece function
    alloc_locals;

    let (piece_num, piece_color, piece_type) = get_piece(toIndex);

    // check if empty
    if (piece_num == 0) {
        return (bool = FALSE);
    }
    // check if it's your own piece
    let (player_color) = get_player_color();
    if (piece_color == player_color) {
        return (bool = FALSE);
    }

    return (bool = TRUE);
}

// check this move is valid or not
func is_valid{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(toIndex: Uint256) -> (bool: felt) {
    // return true if the move is valid
    // input argument board is not necessary bcz we will gonna use storage_val
    alloc_locals;

    // if out of bound, return false
    // do (0x7E7E7E7E7E7E00 >> _toIndex) & 1, where the 0x string is the mapping of 0 << 63 | ... | 0 << 0
    let (var) = check_in_bound(toIndex);
    // valid positions are 1
    if (var == FALSE) {
        return (bool = FALSE);
    }

    // if the to index is occupied by a chess of the same color, return false
    let (piece_num, piece_color, piece_type) = get_piece(toIndex);
    let (player_color) = get_player_color();
    if (piece_num + piece_color - player_color == piece_num) {
        return (bool = FALSE);
    }

    return (bool = TRUE);
}

// rotate a board
func rotate{range_check_ptr, bitwise_ptr: BitwiseBuiltin*}(board: Uint256) -> (
    rotatedBoard: Uint256
) {
    alloc_locals;

    // if the board is 0, return
    if (board.low + board.high == 0) {
        return (rotatedBoard = Uint256(0, 0));
    }

    // shift the board 4 times to right
    let (rightShiftedBoard: Uint256) = uint256_shr(board, Uint256(4, 0));
    // then use recursion with shifted board
    let (rotateBoard: Uint256) = rotate(rightShiftedBoard);
    // do the 'rotateBoard >> board & 0xF'
    let (get_piece: Uint256) = uint256_and(board, Uint256(15, 0));
    let (mid: Uint256) = uint256_shr(rotateBoard, Uint256(4, 0));  // rotateBoard >> 4, right shift 4 bits
    let (makeGet_piece2RightIndex: Uint256) = uint256_shl(get_piece, Uint256(252, 0));
    let (result: Uint256) = uint256_or(mid, makeGet_piece2RightIndex);
    return (rotatedBoard = result);
}

// returns if there is a clear path
// along a direction vector from one index to another
// every Argument is smaller than 2**128
func searchRay{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(fromIndex: Uint256, toIndex: Uint256, directionVector: felt) -> (bool: felt) {
    alloc_locals;

    local indexChange = 0;
    local rayStart = 0;
    local rayEnd = 0;

    // check which index is bigger
    let (isToBiggerThanFrom) = uint256_lt(fromIndex, toIndex);

    if (isToBiggerThanFrom == TRUE) {
        indexChange = toIndex.low - fromIndex.low;
        rayStart = fromIndex.low + directionVector;
        rayEnd = toIndex.low;
    } else {
        indexChange = fromIndex.low - toIndex.low;
        rayStart = toIndex.low;
        rayEnd = fromIndex.low - directionVector;
    }

    // indexChange should be n*directionVector
    let (_, rem) = unsigned_div_rem(indexChange, directionVector);
    if (rem != 0) {
        return (bool = FALSE);
    }

    let (bool) = check_path_using_recursion(rayStart, rayEnd, directionVector);
    let (_vPosition: felt) = is_valid(Uint256(rayEnd, 0));
    let res = bool * _vPosition;
    return (bool = res);
}

// direction vector shows the direction a piece can go to
func check_path_using_recursion{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(rayStart: felt, rayEnd: felt, directionVector: felt) -> (bool: felt) {
    alloc_locals;

    let done = is_le(rayStart + 1, rayEnd);
    if (done == FALSE) {
        return (bool = TRUE);
    }
    if (rayStart == 63) {
        return (bool = TRUE);
    }

    let (bool) = check_path_using_recursion(rayStart + directionVector, rayEnd, directionVector);

    // check _vMove
    let (_vPosition: felt) = is_valid(Uint256(rayStart, 0));
    // check is there any pieces in the way
    let (adjusted_board: Uint256) = get_index_adjusted_board(Uint256(rayStart, 0));
    let (is_capturePiece: felt) = is_capture(adjusted_board);
    let (flipCapture: felt) = bitwise_xor(is_capturePiece, 1);
    let res_mid = _vPosition * flipCapture;
    let res = res_mid * bool;
    return (bool = res);
}

//
// @external
//


@external
func apply_move{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(fromIndex : Uint256, toIndex : Uint256) -> (){
    alloc_locals;

    // check this player palyed this game before and return the board state
    let (player_board : Uint256) = get_board_status();
    
    if (player_board.low + player_board.high + (player_board.low * player_board.high) == 0){
        let (player : felt) = get_caller_address();
        board.write(player, Uint256(INITIAL_BOARD_LOW,INITIAL_BOARD_HIGH));
        get_board_status();
        // avoid revoked refer
        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
    } else{
        // avoid revoked refer
        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
    }

    // check depth value and if it is smaller than 3 or lager then 20, return error
    // let checkDepthRange : felt = is_in_range(depth, 3, 20);
    // if (checkDepthRange == FALSE){
    //     with_attr error_message("Set the depth in range 3 to 20"){
    //         assert 1 = 0;
    //     }

    //     // avoid revoked refer
    //     tempvar syscall_ptr: felt* = syscall_ptr;
    //     tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
    //     tempvar range_check_ptr = range_check_ptr;
    //     tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
    // } else{
    //     // avoid revoked refer
    //     tempvar syscall_ptr: felt* = syscall_ptr;
    //     tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
    //     tempvar range_check_ptr = range_check_ptr;
    //     tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
    // }

    // Get piece at the from index
    // piece = (board >> ((_move >> 6) << 2)) & 0xF;
    let (piece_num, piece_color, piece_type) = get_piece(fromIndex);
    let piece = Uint256(piece_num, 0);
    let (bool : felt) = is_legal_move(fromIndex, toIndex);

    // check if the move is valid
    with_attr error_message("Illegal move"){
        assert bool = 1;
    }

    let (curr_board : Uint256) = get_board_status();
    let largest_uint = Uint256(
        0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    );

    // apply the move
    // Replace 4 bits at the from index with 0000
    // _board &= type(uint256).max ^ (0xF << (fromIndex << 2));

    let (fromIndex_shifted) = uint256_shl(fromIndex, Uint256(2, 0));
    let (temp1) = uint256_shl(Uint256(0xF, 0), fromIndex_shifted);
    let (temp2) = uint256_xor(largest_uint, temp1);
    let (from_0000) = uint256_and(curr_board, temp2);

    // let (from_0 : Uint256) = uint256_shl(Uint256(0,0), fromIndex)
    // let (from_0000 : Uint256) = uint256_or(curr_board, from_0)

    // Replace 4 bits at the to index with 0000
    // _board &= type(uint256).max ^ (0xF << (toIndex << 2));
    let (toIndex_shifted) = uint256_shl(toIndex, Uint256(2, 0));
    let (temp4) = uint256_shl(Uint256(0xF, 0), toIndex_shifted);
    let (temp5) = uint256_xor(largest_uint, temp4);
    let (to_0000 : Uint256) = uint256_and(from_0000, temp5);

    // Place the piece at the to index
    // _board |= (piece << (toIndex << 2))
    let (to_piece) = uint256_shl(piece, toIndex_shifted);
    let (board_after_move : Uint256) = uint256_or(to_0000, to_piece);
    // rotate the board
    let (boardAfterRotate : Uint256) = rotate(board_after_move);

    let (player : felt) = get_caller_address();
    board.write(player,boardAfterRotate);

    return();
}


//
// from here, code for AI
//
//

//
// inner func
//

// apply Move but don't trigger the state change.
func applyMove_forAI{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(fromIndex: Uint256, toIndex: Uint256, board: Uint256) -> (boardAfterPlay: Uint256) {
    alloc_locals;


    // Get piece at the from index
    // piece = (board >> ((_move >> 6) << 2)) & 0xF;
    let (piece_num, piece_color, piece_type) = get_piece(fromIndex);
    let piece = Uint256(piece_num, 0);

    // let (curr_board : Uint256) = board.read()
    let curr_board: Uint256 = board;
    let largest_uint = Uint256(
        0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    );

    // apply the move
    // Replace 4 bits at the from index with 0000
    // _board &= type(uint256).max ^ (0xF << (fromIndex << 2));

    let (fromIndex_shifted) = uint256_shl(fromIndex, Uint256(2, 0));
    let (temp1) = uint256_shl(Uint256(0xF, 0), fromIndex_shifted);
    let (temp2) = uint256_xor(largest_uint, temp1);
    let (from_0000) = uint256_and(curr_board, temp2);

    // let (from_0 : Uint256) = uint256_shl(Uint256(0,0), fromIndex)
    // let (from_0000 : Uint256) = uint256_or(curr_board, from_0)

    // Replace 4 bits at the to index with 0000
    // _board &= type(uint256).max ^ (0xF << (toIndex << 2));
    let (toIndex_shifted) = uint256_shl(toIndex, Uint256(2, 0));
    let (temp4) = uint256_shl(Uint256(0xF, 0), toIndex_shifted);
    let (temp5) = uint256_xor(largest_uint, temp4);
    let (to_0000: Uint256) = uint256_and(from_0000, temp5);

    // Place the piece at the to index
    // _board |= (piece << (toIndex << 2))
    let (to_piece) = uint256_shl(piece, toIndex_shifted);
    let (board_after_move: Uint256) = uint256_or(to_0000, to_piece);
    // rotate the board
    let (board_after_rotate: Uint256) = rotate(board_after_move);

    // board.write(board_after_rotate)
    return (boardAfterPlay = board_after_rotate);
}

// generate every possible move
// let index_of_square start from 36 (6*6)
// let index : Uint256 = Uint256(0x238A179D71B69959551349138D30B289,0xDB5D33CB1BADB2BAA99A59)
// use single Uint256 and if it is full stor, clear it and start fill it again
func gen_move{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*} (index_of_square: felt, index: Uint256
) -> (moves_untill_now: Uint256) {
    alloc_locals;

    if (index_of_square == 0) {
        return (moves_untill_now = Uint256(0, 0));
    }

    local isFinal;
    if (index_of_square == 36) {
        assert isFinal = TRUE;
    } else {
        assert isFinal = FALSE;
    }

    // do index >>= 6
    let (newIndex: Uint256) = uint256_shr(index, Uint256(6, 0));

    let (moves_untill_now: Uint256) = gen_move(index_of_square - 1, newIndex);

    // get 6bits
    let (pieceIndex: Uint256) = uint256_and(index, Uint256(63, 0));
    // get piece of it
    let (_, pieceType, pieceColor) = get_piece(pieceIndex);
    // get adjustedBoard
    let (adjustedBoard: Uint256) = get_index_adjusted_board(pieceIndex);

    let (getTurn: Uint256) = get_board_status();
    let (nowTurn: Uint256) = uint256_and(getTurn, Uint256(1, 0));

    // need to make this clean ;(
    // if the piece is not there, return
    // or if the piece color is not mach with trun, return
    if (pieceType == 0) {
        return (moves_untill_now = moves_untill_now);
    }

    if (pieceColor != nowTurn.low) {
        return (moves_untill_now = moves_untill_now);
    }

    // if piece is pwan
    // the tricky part of this is PWAN may can go all of 4 direction(Ldiagnal and Rdiagnal and forward and two steps forward)
    // or can go one of that direction or none of that direction.
    // need to check that using if statment, and should make sure that single Uint256 variable has that information
    if (pieceType == PWAN) {
        // check Diagnal ##################################################################################

        // check diagnal sqr has enemy pieces
        let (getLeftDiagnalIndex: Uint256) = uint256_and(index, Uint256(28, 0));  // 28 = 7 * 4
        let (canCaptureLeft) = is_capture(getLeftDiagnalIndex);

        // and if it is, add a path
        // PWAN's legal move
        local afterCheckLeft: Uint256;
        if (canCaptureLeft == TRUE) {
            let (allocValue: Uint256) = appendMoves(
                moves_untill_now, pieceIndex, Uint256(pieceIndex.low + 8, 0), isFinal
            );
            assert afterCheckLeft = allocValue;

            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        } else {
            assert afterCheckLeft = moves_untill_now;

            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        }

        // check diagnal sqr has enemy pieces
        let (getRightDiagnalIndex: Uint256) = uint256_and(index, Uint256(36, 0));  // 36 = 9 * 4
        let (canCaptureRight) = is_capture(getRightDiagnalIndex);

        // and if it is, add a path
        // PWAN's legal move
        local afterCheckRight: Uint256;
        if (canCaptureRight == TRUE) {
            let (allocValue: Uint256) = appendMoves(
                afterCheckLeft, pieceIndex, Uint256(pieceIndex.low + 8, 0), isFinal
            );
            assert afterCheckRight = allocValue;

            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        } else {
            assert afterCheckRight = afterCheckLeft;

            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        }

        // avoid revoked refer
        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

        // Check Front Also ###############################################################################

        // check front sqr is empty
        let (getFrontSquare: Uint256) = uint256_shr(adjustedBoard, Uint256(32, 0));  // 32 = 8 * 4

        let (checkEmpty: Uint256) = uint256_and(getFrontSquare, Uint256(15, 0));

        // if front is empty, you should check this is the first time to move this pwan
        // and if is the first time, check one more step forward and if there is empty, then this pwan can move 2 steps forward
        if (checkEmpty.low == 0) {
            // append this move and get move
            let (afterCheckFront: Uint256) = appendMoves(
                afterCheckRight, pieceIndex, Uint256(pieceIndex.low + 8, 0), isFinal
            );

            // check this is first move Pwan's Starting Line is 16 to 23 and this value(16 to 23) >> 2 is 2
            let (isItFirstMove: Uint256) = uint256_shr(pieceIndex, Uint256(3, 0));
            // check 2 steps forward sqr is empty
            let (get2StepsForwardSquare: Uint256) = uint256_shr(adjustedBoard, Uint256(64, 0));  // 64 = 16 * 4
            let (check2StepsForwardIsEmpty: Uint256) = uint256_and(
                get2StepsForwardSquare, Uint256(15, 0)
            );
            // isItFirstMove == 2 & check2StepsForwardIsEmpty == 0
            if ((isItFirstMove.low - 2) + check2StepsForwardIsEmpty.low == 0) {
                // let a = (moves_untill_now << 12) | fromIndex << 6 | toIndex
                let (res: Uint256) = appendMoves(
                    afterCheckFront, pieceIndex, Uint256(pieceIndex.low + 8, 0), isFinal
                );

                // avoid revoked refer
                tempvar syscall_ptr: felt* = syscall_ptr;
                tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
                tempvar range_check_ptr = range_check_ptr;
                tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

                return (moves_untill_now = res);
            } else {
                // avoid revoked refer
                tempvar syscall_ptr: felt* = syscall_ptr;
                tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
                tempvar range_check_ptr = range_check_ptr;
                tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

                return (moves_untill_now = afterCheckFront);
            }
        } else {
            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

            return (moves_untill_now = afterCheckRight);
        }
    } else {
        // if piece is knight or king
        if ((pieceType - KNIGHT) * (pieceType - KING) == 0) {
            // Piece is a knight or a king.
            // Knights and kings always only have 8 positions to check relative to their
            // current position, and the relative distances are always the same. For
            // knights, positions to check are ±{6, 10, 15, 17}. This is bitpacked into
            // `0x060A0F11` to reduce code redundancy. Similarly, the positions to check for
            // kings are ±{1, 7, 8, 9}, which is `0x01070809` when bitpacked.
            local directions: Uint256;  

            if (pieceType == KNIGHT) {
                assert directions = Uint256(0x060A0F11, 0);
                let (mid, _) = check_knight_kings_move(
                    pieceIndex, directions, moves_untill_now, 'ADD', FALSE
                );
                let (res, _) = check_knight_kings_move(pieceIndex, directions, mid, 'SUB', isFinal);

                // avoid revoked refer
                tempvar syscall_ptr: felt* = syscall_ptr;
                tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
                tempvar range_check_ptr = range_check_ptr;
                tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

                // finish to find every possible wa
                return (moves_untill_now = res);
            } else {
                assert directions = Uint256(0x060A0F11, 0);
                let (mid, _) = check_knight_kings_move(
                    pieceIndex, directions, moves_untill_now, 'ADD', FALSE
                );
                let (res, _) = check_knight_kings_move(pieceIndex, directions, mid, 'SUB', isFinal);

                return (moves_untill_now = res);
            }

            // other all sliding pieces
        } else {
            if (pieceType == ROOK) {
                // Check straight moves
                // each index has 4 bits so do *4 every direction

                // check right straight moves
                let (checkRightWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, 1 * 4, moves_untill_now, FALSE
                );
                // check left straight moves
                let (checkLeftWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, (-1) * 4, checkRightWay, FALSE
                );
                // check upper straight moves
                let (checkUpperWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, 8 * 4, checkLeftWay, FALSE
                );
                // check lower straight moves
                let (checkRightWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, (-8) * 4, checkUpperWay, isFinal
                );

                return (moves_untill_now = checkRightWay);
            }

            if (pieceType == BISHOP) {
                // Check diagnal moves
                // each index has 4 bits so do *4 every direction

                // check upper right straight moves
                let (checkUpperRightWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, 9 * 4, moves_untill_now, FALSE
                );
                // check upper left straight moves
                let (checkUpperLeftWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, 7 * 4, checkUpperRightWay, FALSE
                );
                // check lower right straight moves
                let (checkLowerRightWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, (-9) * 4, checkUpperLeftWay, FALSE
                );
                // check lower left straight moves
                let (checkLowerLeftWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, (-7) * 4, checkLowerRightWay, isFinal
                );

                return (moves_untill_now = checkLowerLeftWay);
            }

            if (pieceType == QUEEN) {
                // Check diagnal + straight moves
                // each index has 4 bits so do *4 every direction

                // check right straight moves
                let (checkRightWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, 1 * 4, moves_untill_now, FALSE
                );
                // check left straight moves
                let (checkLeftWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, (-1) * 4, checkRightWay, FALSE
                );
                // check upper straight moves
                let (checkUpperWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, 8 * 4, checkLeftWay, FALSE
                );
                // check lower straight moves
                let (checkLowerWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, (-8) * 4, checkUpperWay, isFinal
                );
                // check upper right straight moves
                let (checkUpperRightWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, 9 * 4, checkLowerWay, FALSE
                );
                // check upper left straight moves
                let (checkUpperLeftWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, 7 * 4, checkUpperRightWay, FALSE
                );
                // check lower right straight moves
                let (checkLowerRightWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, (-9) * 4, checkUpperLeftWay, FALSE
                );
                // check lower left straight moves
                let (checkLowerLeftWay: Uint256) = check_sliding_pieces_move(
                    pieceIndex, (-7) * 4, checkLowerRightWay, isFinal
                );

                return (moves_untill_now = checkLowerLeftWay);
            }

            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        }

        // if this code is exequte, that mean this function has an error lol
        with_attr error_message("Check gen_move function") {
            assert 1 = 0;
        }
        return (moves_untill_now = Uint256(0, 0));
    }
}

// check pieces valid moves
func check_knight_kings_move{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(pieceIndex: Uint256, directions: Uint256, moves_untill_now: Uint256, addOrSub: felt, isFinal: felt) -> (
    new_move_info: Uint256, lastVaildMove: Uint256
) {
    alloc_locals;
    // check direction is empty
    if (directions.low == 0) {
        return (moves_untill_now, Uint256(0, 0));
    }
    // check is this last time to call appendMoves function and is this the last recursion
    local isItFinal;
    if (isFinal == TRUE) {
        assert isItFinal = TRUE;
    } else {
        assert isItFinal = FALSE;
    }

    // direction >> 8
    let (newDirections: Uint256) = uint256_shr(directions, Uint256(8, 0));
    // call recursively
    let (newMoves: Uint256, lastMove: Uint256) = check_knight_kings_move(
        pieceIndex, newDirections, moves_untill_now, addOrSub, FALSE
    );

    // get 8 bits from directions, which is single direction
    let (direction: Uint256) = uint256_and(directions, Uint256(0xFF, 0));
    // and add to pieceIndex so that can get a destination's index
    local destination: Uint256;
    if (addOrSub == 'ADD') {
        let (getDestination: Uint256, _) = uint256_add(pieceIndex, direction);
        assert destination = getDestination;
    } else {
        let (getDestination: Uint256, _) = uint256_add(pieceIndex, direction);
        assert destination = getDestination;
    }

    local afterCheckLastMove: Uint256;
    local afterCheckValid: Uint256;

    let (checkValid: felt) = is_valid(destination);
    if (checkValid == TRUE) {
        let (allocMoves: Uint256) = appendMoves(newMoves, pieceIndex, destination, isItFinal);
        assert afterCheckValid = allocMoves;
        // store last move
        assert afterCheckLastMove = destination;

        // avoid revoked refer
        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
    } else {
        assert afterCheckValid = newMoves;
        // point last move
        assert afterCheckLastMove = lastMove;

        // if this is last chance to call appendMoves function BUT if not a Valid move
        // then store last vaild move to storage_val
        if (isItFinal == 1) {
            // remove last stored move (6 bits fromIndex, 6 bits toIndex 12bits total)
            let (toStoreMoves: Uint256) = uint256_shr(afterCheckValid, Uint256(12, 0));
            let (res: Uint256) = appendMoves(newMoves, pieceIndex, lastMove, isItFinal);

            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        } else {
            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        }
    }

    return (afterCheckValid, afterCheckLastMove);
}

// check sliding pieces avaliable move
func check_sliding_pieces_move{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(pieceIndex: Uint256, direction: felt, moves_untill_now: Uint256, isFinal: felt) -> (
    new_move_info: Uint256
) {
    alloc_locals;
    local isItFinal;
    if (isFinal == TRUE) {
        assert isItFinal = TRUE;
    } else {
        assert isItFinal = FALSE;
    }

    let check_is_not_negative: felt = is_nn(pieceIndex.low + direction);
    // if this is last chance to call appendMoves function BUT if not a Valid move
    if ((check_is_not_negative * isItFinal) + isItFinal == 1) {
        // remove last stored move (6 bits fromIndex, 6 bits toIndex 12bits total)
        let (toStoreMoves: Uint256) = uint256_shr(moves_untill_now, Uint256(12, 0));
        // get the last toIndex
        let (lastMove: Uint256) = uint256_and(moves_untill_now, Uint256(0x3F, 0));
        // store it
        let (res: Uint256) = appendMoves(toStoreMoves, pieceIndex, lastMove, isItFinal);

        return (new_move_info = moves_untill_now);
    }

    if (check_is_not_negative == FALSE) {
        return (new_move_info = moves_untill_now);
    }

    // check is valid
    let (check_is_vaild) = is_valid(Uint256(pieceIndex.low + direction, 0));
    // if this is last chance to call appendMoves function BUT if not a Valid move
    if ((check_is_vaild * isItFinal) + isItFinal == 1) {
        // remove last stored move (6 bits fromIndex, 6 bits toIndex 12bits total)
        let (toStoreMoves: Uint256) = uint256_shr(moves_untill_now, Uint256(12, 0));
        // get the last toIndex
        let (lastMove: Uint256) = uint256_and(moves_untill_now, Uint256(0x3F, 0));
        // store it
        let (res: Uint256) = appendMoves(toStoreMoves, pieceIndex, lastMove, isItFinal);

        return (new_move_info = moves_untill_now);
    }

    // check is valid
    if (check_is_vaild == FALSE) {
        return (new_move_info = moves_untill_now);
    }

    let (checkIsCapture) = is_capture(Uint256(pieceIndex.low + direction, 0));
    // if it is capture move, store Data and return it
    if (checkIsCapture == TRUE) {
        let (res: Uint256) = appendMoves(
            moves_untill_now, pieceIndex, Uint256(pieceIndex.low + direction, 0), isItFinal
        );
        return (new_move_info = res);
    }

    // if it is valid and also not capture any pieces, check next direction.
    let (new_move_info: Uint256) = check_sliding_pieces_move(
        Uint256(pieceIndex.low + direction, 0), direction + direction, moves_untill_now, FALSE
    );

    // and also append this move
    let (result: Uint256) = appendMoves(
        moves_untill_now, pieceIndex, Uint256(pieceIndex.low + direction, 0), isItFinal
    );

    return (new_move_info = result);
}


func search_best_move{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, depth: Uint256) -> (bestMove: Uint256, bool: felt) {

    let (bestScore, bestMove) = rec1(board, Uint256(0, 0), depth);
    let bool: felt = is_le(bestScore, -1261);
    if (bool == TRUE) {
        return (Uint256(0, 0), TRUE);
    }
    return (bestMove, FALSE);
}

func rec1{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, index: Uint256, depth: Uint256) -> (realBest: felt, realBestMove: Uint256) {
    alloc_locals;
    // check index's storage_val is empty
    // and if it is, return bestScore
    let (player_addr : felt) = get_caller_address();

    let (aMovesArray: Uint256) = movesArray.read(player_addr,index.low);
    if (aMovesArray.low == 0) {
        return (-4196, Uint256(0, 0));
    }

    let (curr_best, curr_move) = rec1(board, Uint256(index.low + 1, 0), depth);
    let (new_best, new_move) = rec2(board, aMovesArray, depth);
    let bool: felt = is_le(curr_best, new_best - 1);
    if (bool == TRUE) {
        return (new_best, new_move);
    } else {
        return (curr_best, curr_move);
    }
}

func rec2{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, aMovesArray: Uint256, depth: Uint256) -> (bestScore: felt, bestMove: Uint256) {
    alloc_locals;

    let (aMovesArray_shifted) = uint256_shr(aMovesArray, Uint256(12, 0));

    // check index's_storage_val & 0xFFF is empty
    // and if it is, return -4_196
    let (local lastMove) = uint256_and(aMovesArray, Uint256(0xFFF, 0));
    let isEmpty : felt = is_le(lastMove.low,65);
    if (isEmpty == TRUE) {
        return (-4196, Uint256(0, 0));
    }

    // check score of index's_storage_val & 0x3F
    // and compare with currentScore and return bigger one
    let (bestScore, bestMove) = rec2(board, aMovesArray_shifted, depth);

    let currMove = lastMove; //uint256_and(aMovesArray, Uint256(0xFFF, 0)); make comment fot the efficient

    let (temp1: Uint256) = uint256_shr(lastMove, Uint256(6, 0));
    let (fromIndex: Uint256) = uint256_and(temp1, Uint256(0x3F, 0));
    let (toIndex: Uint256) = uint256_and(lastMove, Uint256(0x3F, 0));
    let (currScore_eval: felt) = evaluateMove(board, fromIndex, toIndex);
    let (sim_board: Uint256) = simApplyMove(currMove, board);

    let (temp2: felt) = negaMax(sim_board, Uint256(depth.low - 1, 0));
    let currScore = currScore_eval + temp2;

    let bool: felt = is_le(bestScore, currScore);
    if (bool == TRUE) {
        return (currScore, currMove);
    } else {
        return (bestScore, bestMove);
    }
}

// apped moves to storage_val if movesArray is full or the final appends
// moveInfo is single Uint256 that moves is stored
func appendMoves{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(moveInfo: Uint256, fromIndex: Uint256, toIndex: Uint256, is_final: felt) -> (
    new_move_info: Uint256
) {
    alloc_locals;

    let (checkIsFull: Uint256) = uint256_shr(moveInfo, Uint256(246, 0));
    let (_, isItFull: Uint256) = uint256_signed_div_rem(checkIsFull, Uint256(64, 0));
    // if checkIsFull.low != 0 then moveInfo is full and if it's full we need to store that value
    if (isItFull.low != 0) {
        // let's store last index of the movesArray in index 0's first 4 bits
        let (player_addr : felt) = get_caller_address();
        let (getIndexArray: Uint256) = movesArray.read(player_addr,0);
        // do (getIndexArray >> 252) & 0xF this give me to the last index of the
        let (getIndex: Uint256) = uint256_shr(getIndexArray, Uint256(252, 0));
        let (index: Uint256) = uint256_and(getIndex, Uint256(15, 0));

        // write it to the storage
        movesArray.write(player_addr,index.low, moveInfo);

        // make index + 1 and store to the index 0's first 4 bits
        let (newIndex: Uint256) = uint256_shl(Uint256(index.low + 1, 0), Uint256(252, 0));  // is this should be 251?
        // make Uint256 that only first 4 bits are 0 and the other are 1
        let toEraseIndex: Uint256 = Uint256(
            0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, 0x0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        );
        // do (getIndexArray & toEraseIndex) | newIndex
        let (erasedIndex: Uint256) = uint256_and(getIndexArray, toEraseIndex);
        let (completeUint: Uint256) = uint256_or(erasedIndex, newIndex);
        // write a new index
        movesArray.write(player_addr,0, completeUint);

        // make new moveInfo Instance
        let makeNew_move_info: Uint256 = Uint256(fromIndex.low, 0);
        let (new_move_info: Uint256) = uint256_shl(makeNew_move_info, Uint256(6, 0));
        let (res: Uint256) = uint256_or(new_move_info, Uint256(toIndex.low, 0));

        // avoid revoked refer
        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

        return (new_move_info = res);
        // if not, it means that moveInfo is not full so you can insert moves
    } else {
        // moveInfo << 12
        let (shl2MoveInfo: Uint256) = uint256_shl(moveInfo, Uint256(12, 0));
        // make move ((fromIndex << 6) | toIndex)
        let (shl2FromIndex: Uint256) = uint256_shl(fromIndex, Uint256(6, 0));
        let (move: Uint256) = uint256_or(shl2FromIndex, toIndex);

        let (res: Uint256) = uint256_or(shl2MoveInfo, move);

        // index_of_square's max value is 36 (board is 6*6)
        // so if index_of_square value is 36, this is the last time to call this function
        // so need to store value to the storage_val
        if (is_final == TRUE) {
            // let's store last index of the movesArray in index 0's first 4 bits
            let (player_addr : felt) = get_caller_address();
            let (getIndexArray: Uint256) = movesArray.read(player_addr,0);
            // do (getIndexArray >> 252) & 0xF this give me to the last index of the
            let (getIndex: Uint256) = uint256_shr(getIndexArray, Uint256(252, 0));
            let (index: Uint256) = uint256_and(getIndex, Uint256(15, 0));

            // write it to the storage
            movesArray.write(player_addr,index.low - 1, moveInfo);

            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        } else {
            // # let's store last index of the movesArray in index 0's first 4 bits
            // let (getIndexArray : Uint256) = movesArray.read(0)
            // # do (getIndexArray >> 252) & 0xF this give me to the last index of the
            // let (getIndex : Uint256) = uint256_shr(getIndexArray,Uint256(252,0))
            // let (index : Uint256) = uint256_and(getIndex,Uint256(15,0))

            // # write it to the storage
            // movesArray.write(index.low,moveInfo)

            // # make index + 1 and store to the index 0's first 4 bits
            // let (newIndex : Uint256) = uint256_shl(Uint256(index.low+1,0),Uint256(252,0)) #is this should be 251?
            // #make Uint256 that only first 4 bits are 0 and the other are 1
            // let toEraseIndex : Uint256 = Uint256(0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,0x0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
            // # do (getIndexArray & toEraseIndex) | newIndex
            // let (erasedIndex : Uint256) = uint256_and(getIndexArray,toEraseIndex)
            // let (completeUint : Uint256) = uint256_or(erasedIndex,newIndex)
            // # write a new index
            // movesArray.write(0,completeUint)

            // avoid revoked refer
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        }

        // avoid revoked refer
        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

        return (new_move_info = res);
    }
}

// @Yetta: need to define play depth
func negaMax{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(sim_board: Uint256, depth: Uint256) -> (res: felt) {
    alloc_locals;
    if (depth.low == 0) {
        return (res = 0);
    }
    let (movesArray_0: Uint256) = get_moves_array(0);
    let (isEmpty: Uint256) = uint256_shr(movesArray_0, Uint256(4, 0));
    if (isEmpty.low == 0) {
        return (res = 0);
    }

    // the best score in all possible moves stored in MovesArray
    let (bestScore: felt, bestMove: Uint256) = realBest(sim_board, Uint256(0, 0));

    // ((_board >> ((bestMove & 0x3F) << 2)) & 7)
    let (temp1) = uint256_and(bestMove, Uint256(0x3F, 0));
    let (temp2) = uint256_shl(temp1, Uint256(2, 0));
    let (toIndex_shifted) = uint256_shr(sim_board, temp2);
    let (temp4) = uint256_and(toIndex_shifted, Uint256(7, 0));
    // if the king is captured
    if (temp4.low == 6) {
        return (res = -4000);
    }

    // _board & 1
    let (temp5: Uint256) = uint256_and(sim_board, Uint256(1, 0));
    // bestScore + negaMax(_board.applyMove(bestMove), _depth - 1)
    let (depth_1: Uint256) = uint256_sub(depth, Uint256(1, 0));

    let (sim_board_1: Uint256) = simApplyMove(bestMove, sim_board);

    let (temp6: felt) = negaMax(sim_board_1, depth_1);
    let res1 = bestScore + temp6;
    let res2 = temp6 - bestScore;

    // if _board & 1 == 0, meaning the player is black
    if (temp5.low == 0) {
        return (res = res1);
    } else {
        return (res = res2);
    }
}

// find the best move in all arrays of moves
func realBest{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, index: Uint256) -> (realBest: felt, realBestMove: Uint256) {
    alloc_locals;
    // check index's storage_val is empty
    // and if it is, return bestScore
    let (player_addr : felt) = get_caller_address();

    let (aMovesArray: Uint256) = movesArray.read(player_addr,index.low);
    if (aMovesArray.low == 0) {
        return (-4196, Uint256(0, 0));
    }

    let (score, move) = realBest(board, Uint256(index.low + 1, 0));
    let (aaa, bbb) = find_best_local(board, aMovesArray);

    let bool: felt = is_le(score, aaa - 1);
    if (bool == TRUE) {
        return (aaa, bbb);
    } else {
        return (score, move);
    }
}

// find the best move in a given array of moves
func find_best_local{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, aMovesArray: Uint256) -> (bestScore: felt, bestMove: Uint256) {
    alloc_locals;

    let (aMovesArray_shifted) = uint256_shr(aMovesArray, Uint256(12, 0));

    // check index's_storage_val & 0xFFF is empty
    // and if it is, return -4_196
    let (lastMove: Uint256) = uint256_and(aMovesArray, Uint256(0xFFF, 0));
    if (lastMove.low == 0) {
        return (-4196, Uint256(0, 0));
    }

    // check score of index's_storage_val & 0x3F
    // and compare with currentScore and return bigger one
    let (bestScore, bestMove) = find_best_local(board, aMovesArray_shifted);

    let (currMove) = uint256_and(aMovesArray, Uint256(0xFFF, 0));

    let (temp1: Uint256) = uint256_shr(lastMove, Uint256(6, 0));
    let (fromIndex: Uint256) = uint256_and(temp1, Uint256(0x3F, 0));
    let (toIndex: Uint256) = uint256_and(lastMove, Uint256(0x3F, 0));
    let (currScore: felt) = evaluateMove(board, fromIndex, toIndex);

    let bool: felt = is_le(bestScore, currScore);
    if (bool == TRUE) {
        return (currScore, currMove);
    } else {
        return (bestScore, bestMove);
    }
}

func evaluateMove{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, fromIndex: Uint256, toIndex: Uint256) -> (res: felt) {
    alloc_locals;
    let (piece_num_from, piece_color_from, piece_type_from) = simGet_piece(board, fromIndex);
    let (piece_num_to, piece_color_to, piece_type_to) = simGet_piece(board, toIndex);
    local captureValue;
    local newPst;
    local oldPst;

    let (pst_value_from: Uint256) = getPst(Uint256(piece_type_from, 0));
    tempvar syscall_ptr: felt* = syscall_ptr;
    tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
    tempvar range_check_ptr = range_check_ptr;
    tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

    let (topOfTheToIndex: Uint256) = uint256_shr(toIndex, Uint256(3, 0));
    let (bottomOfTheToIndex: Uint256) = uint256_and(toIndex, Uint256(7, 0));
    let newToIndex: Uint256 = Uint256(6 * topOfTheToIndex.low + bottomOfTheToIndex.low - 7, 0);

    let (topOfTheFromIndex: Uint256) = uint256_shr(fromIndex, Uint256(3, 0));
    let (bottomOfTheFromIndex: Uint256) = uint256_and(fromIndex, Uint256(7, 0));
    let newFromIndex: Uint256 = Uint256(
        6 * topOfTheFromIndex.low + bottomOfTheFromIndex.low - 7, 0
    );

    // calculate capture value.
    if (piece_type_to != 0) {
        // not king or queen
        let bool1: felt = is_le(piece_type_to, 4);
        if (bool1 == TRUE) {
            let (pst_value_to: Uint256) = getPst(Uint256(piece_type_to, 0));

            // (getPst(pieceAtToIndex) >> (7 * (0x23 - toIndex))) & 0x7F
            let temp1 = 7 * (0x23 - newToIndex.low);
            let temp1uint = Uint256(temp1, 0);

            let (c1_uint: Uint256) = uint256_shr(pst_value_to, temp1uint);
            let c1: Uint256 = uint256_and(c1_uint, Uint256(0x7F, 0));

            assert captureValue = c1.low;

            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        } else {
            // piece is king or queen in closer half of the board
            let bool2: felt = is_le(newToIndex.low, 17);
            if (bool2 == TRUE) {
                let (pst_value_to: Uint256) = getPst(Uint256(piece_type_to, 0));

                // getPst(pieceAtToIndex) >> (0xC * (0x11 - toIndex))) & 0xFFF
                let temp2 = 0xC * (0x11 - newToIndex.low);
                let temp2uint = Uint256(temp2, 0);
                let c2_uint: Uint256 = uint256_shr(pst_value_to, temp2uint);
                let c2: Uint256 = uint256_and(c2_uint, Uint256(0xFFF, 0));

                assert captureValue = c2.low;

                tempvar syscall_ptr: felt* = syscall_ptr;
                tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
                tempvar range_check_ptr = range_check_ptr;
                tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
            } else {
                // king or queen in the further half
                let (pst_value_to_2: Uint256) = getPstTwo(Uint256(piece_type_to, 0));

                // (getPstTwo(pieceAtToIndex) >> (0xC * (0x23 - toIndex))) & 0xFFF
                let toIndex_shifted = 0xC * (0x23 - newToIndex.low);
                let toIndex_shifteduint = Uint256(toIndex_shifted, 0);
                let c3_uint: Uint256 = uint256_shr(pst_value_to_2, toIndex_shifteduint);
                let c3: Uint256 = uint256_and(c3_uint, Uint256(0xFFF, 0));

                assert captureValue = c3.low;

                tempvar syscall_ptr: felt* = syscall_ptr;
                tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
                tempvar range_check_ptr = range_check_ptr;
                tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
            }
        }
    } else {
        assert captureValue = 0;

        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
    }

    tempvar syscall_ptr: felt* = syscall_ptr;
    tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
    tempvar range_check_ptr = range_check_ptr;
    tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

    let bool3: felt = is_le(piece_type_from, 4);
    if (bool3 == TRUE) {
        // (getPst(pieceAtFromIndex) >> (7 * fromIndex)) & 0x7F
        let temp4: felt = 7 * newFromIndex.low;
        let temp4uint = Uint256(temp4, 0);
        let (o1_uint: Uint256) = uint256_shr(pst_value_from, temp4uint);
        let o1: Uint256 = uint256_and(o1_uint, Uint256(0x7F, 0));

        // (getPst(pieceAtFromIndex) >> (7 * toIndex)) & 0x7F;
        let temp5: felt = 7 * newToIndex.low;
        let temp5uint = Uint256(temp5, 0);
        let (n1_uint: Uint256) = uint256_shr(pst_value_from, temp5uint);
        let n1: Uint256 = uint256_and(n1_uint, Uint256(0x7F, 0));

        assert oldPst = o1.low;
        assert newPst = n1.low;

        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
    } else {
        // piece is king or queen in closer half of the board
        let bool4: felt = is_le(newFromIndex.low, 17);
        if (bool4 == TRUE) {
            let (pst_value_from_2: Uint256) = getPstTwo(Uint256(piece_type_from, 0));

            // (getPstTwo(pieceAtFromIndex) >> (0xC * fromIndex)) & 0xFFF
            let temp6: felt = 0xC * newFromIndex.low;
            let temp6uint = Uint256(temp6, 0);
            let (o2_uint: Uint256) = uint256_shr(pst_value_from_2, temp6uint);
            let o2: Uint256 = uint256_and(o2_uint, Uint256(0xFFF, 0));

            assert oldPst = o2.low;

            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        } else {
            // king or queen in the further half

            // (getPst(pieceAtFromIndex) >> (0xC * (fromIndex - 0x12))) & 0xFFF
            let temp7: felt = 0xC * (newFromIndex.low - 0x12);
            let temp7uint = Uint256(temp7, 0);
            let (o3_uint: Uint256) = uint256_shr(pst_value_from, temp7uint);
            let o3: Uint256 = uint256_and(o3_uint, Uint256(0xFFF, 0));

            assert oldPst = o3.low;
            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        }

        tempvar syscall_ptr: felt* = syscall_ptr;
        tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

        let bool5: felt = is_le(newToIndex.low, 17);
        if (bool5 == TRUE) {
            let (pst_value_from_2: Uint256) = getPstTwo(Uint256(piece_type_from, 0));

            // (getPstTwo(pieceAtFromIndex) >> (0xC * toIndex)) & 0xFFF
            let temp8: felt = 0xC * newToIndex.low;
            let temp8uint = Uint256(temp8, 0);
            let (n2_uint: Uint256) = uint256_shr(pst_value_from_2, temp8uint);
            let n2: Uint256 = uint256_and(n2_uint, Uint256(0xFFF, 0));

            assert newPst = n2.low;

            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        } else {
            // (getPst(pieceAtFromIndex) >> (0xC * (toIndex - 0x12))) & 0xFFF
            let temp9: felt = 0xC * (newToIndex.low - 0x12);
            let temp9uint = Uint256(temp9, 0);
            let n3_uint: Uint256 = uint256_shr(pst_value_from, temp9uint);
            let n3: Uint256 = uint256_and(n3_uint, Uint256(0xFFF, 0));

            assert newPst = n3.low;

            tempvar syscall_ptr: felt* = syscall_ptr;
            tempvar pedersen_ptr: HashBuiltin* = pedersen_ptr;
            tempvar range_check_ptr = range_check_ptr;
            tempvar bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;
        }
    }

    let res = (captureValue + newPst) - (oldPst);
    return (res = res);
}

func getPst{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(type: Uint256) -> (res: Uint256) {
    alloc_locals;
    if (type.low == 1) {
        return (res = Uint256(0x0A943468A152A788103C54A142850A14, 0x2850A142850F1E3C78F1E2858C182C5));
    }

    if (type.low == 2) {
        return (res = Uint256(0x20448912240810E1428701F40810203E, 0x7D0204080FA042850A140810E244870));
    }

    if (type.low == 3) {
        return (res = Uint256(0xF1E4C993263C793264C98F264CB97264, 0xC993264C9932E6CD9B365C793264C98));
    }

    if (type.low == 4) {
        return (res = Uint256(0x9D4189120BA70F20C178E1B3874E9C36, 0x6CE1B3670E9C3C8101E38750224480E));
    }

    if (type.low == 5) {
        return (res = Uint256(0xB40B40B40B40B20B30B40B50B50B40B3, 0xB00B20B30B30B20B00B20));
    }

    let res = Uint256(0x98F96F96F98F9AF9CF9AF98F98F9AF9B, 0xF9AF98F96F96F98F9AF9AF);
    return (res = res);
}

func getPstTwo{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(type: Uint256) -> (res: Uint256) {
    alloc_locals;

    let bool = (type.low - QUEEN) * (type.low - KING);

    // with_attr error_message("PstTwo value only applies to Queens or Kings"):
    //     assert bool = 0
    // end
    if (bool != 0) {
        return (res = Uint256(0, 0));
    }

    if (type.low == 5) {
        return (res = Uint256(0xB40B50B40B40B20B00B20B30B30B20B0, 0xB30B50B50B50B40B30B20));
    }
    if (type.low == 6) {
        return (res = Uint256(0xA1FA0FA0FA1FA1FA4FA6FA2FA2FA6FA4, 0xF9EF9CF9CF9CF9CF9EFA1F));
    }

    // should be repair (there was no return thing)
    return (res = Uint256(0, 0));
}

func simApplyMove{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(Move: Uint256, sim_board: Uint256) -> (sim_board: Uint256) {
    // apply the move to a given board
    // input argument board is not necessary bcz we will gonna use storage_val
    alloc_locals;

    let (toIndex) = uint256_and(Move, Uint256(0x3F, 0));
    let (temp) = uint256_shr(Move, Uint256(6, 0));
    let (fromIndex) = uint256_and(temp, Uint256(0x3F, 0));

    let curr_board = sim_board;
    // get the piece at the from index
    let (piece_num_felt, _, _) = simGet_piece(curr_board, fromIndex);
    let piece: Uint256 = Uint256(piece_num_felt, 0);

    // Replace 4 bits at the from index with 0000
    // _board &= type(uint256).max ^ (0xF << ((_move >> 6) << 2));
    // fromIndexshifted = ((_move >> 6) << 2)
    let (fromIndex_shifted) = uint256_shl(fromIndex, Uint256(2, 0));
    // (0xF << fromIndexshifted)
    let (temp1) = uint256_shl(Uint256(0xF, 0), fromIndex_shifted);
    let largest_uint = Uint256(
        0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    );
    let (temp2) = uint256_xor(largest_uint, temp1);
    let (from_0000) = uint256_and(curr_board, temp2);

    // Replace 4 bits at the to index with 0000
    // _board &= type(uint256).max ^ (0xF << (toIndex << 2));
    let (toIndex_shifted) = uint256_shl(toIndex, Uint256(2, 0));
    let (temp4) = uint256_shl(Uint256(0xF, 0), toIndex_shifted);
    let (temp5) = uint256_xor(largest_uint, temp4);
    let (to_0000: Uint256) = uint256_and(from_0000, temp5);

    // Place the piece at the to index
    // _board |= (piece << (toIndex << 2))
    let (to_piece) = uint256_shl(piece, toIndex_shifted);
    // mask the move with the piece
    let (board_after_move: Uint256) = uint256_or(to_0000, to_piece);
    // rotate the board
    let (board_after_rotate: Uint256) = rotate(board_after_move);

    return (sim_board = board_after_rotate);
}

func simGet_piece{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, index: Uint256) -> (piece_num: felt, piece_type: felt, piece_color: felt) {
    alloc_locals;

    with_attr error_message("index should be less than 63") {
        let (IsIndexBiggerThan63) = uint256_le(index, Uint256(63, 0));
        assert IsIndexBiggerThan63 = TRUE;
    }
    // do '(board >> (index << 2)) & 0xF'
    let (getIndex: Uint256) = uint256_shr(board, Uint256(index.low * 4, 0));
    let (piece: Uint256) = uint256_and(getIndex, Uint256(15, 0));
    // get color and type of the piece; first bit represents color; the next 3 bits represent type
    let (piece_color: Uint256, piece_type: Uint256) = uint256_signed_div_rem(piece, Uint256(8, 0));
    // all this three instance is smaller than 2**128
    return (piece.low, piece_type.low, piece_color.low);
}

//
// @external
//

// AI player functions

// searchMove every possible move
@external
func searchMove{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}() {
    alloc_locals;
    // github copilot please stop
    // initialized values? @Yetta board and moves_array
    let (moves_array) = gen_move(
        36, Uint256(0x238A179D71B69959551349138D30B289, 0xDB5D33CB1BADB2BAA99A59)
    );

    return ();
}

// AI player functions
@external
func ai_apply_move{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(depth: Uint256) -> (boardAfterPlay: Uint256, isOver: felt) {
    alloc_locals;
    
    // search_best_move returns bestmoves
    // if bestMove is 0, then no move is available
    // else apply Best Move.
    let (board_status : Uint256) = get_board_status();
    let (bestMove, isOver) = search_best_move(board_status,depth);

    // if AI can't move anywhere, then the game is over
    if (isOver == TRUE) {
        let (player_addr : felt) = get_caller_address();
        board.write(player_addr,board_status);
        return (board_status, isOver);
    }

    // get first 6bits from the bestMove (111111000000)
    let (temp: Uint256) = uint256_shr(bestMove, Uint256(6, 0));
    let (fromIndex: Uint256) = uint256_and(temp, Uint256(0x3F, 0));
    // get last 6bits from the bestMove
    let (toIndex: Uint256) = uint256_and(bestMove, Uint256(0x3F, 0));

    let (boardAfterPlay: Uint256) = applyMove_forAI(fromIndex, toIndex, board_status);

    // should clear movesArray
    // by doing this here, we can reduce storage_val.write
    // get first item from the movesArray
    let (player_addr : felt) = get_caller_address();

    let (firstItem : Uint256) = movesArray.read(player_addr,0);
    // information of the index is exist in firstItem's first 4 bits
    // so do the firstItem >> 252
    let (getIndex : Uint256) = uint256_shr(firstItem, Uint256(252, 0));
    clear_moves_array(getIndex.low);

    let player_addr : felt = get_caller_address();

    board.write(player_addr,boardAfterPlay);

    return (boardAfterPlay, FALSE);
}