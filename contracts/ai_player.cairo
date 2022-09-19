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
from contracts.chess_board import (
    board,
    movesArray,
    getPiece,
    getMovesArray,
    isLegalMove,
    rotate,
    applyMove,
    generateMove,
    appendMoves,
    checkKnightNKingsMove,
    checkSlidingPiecesMove,
)

//
// const val
//

const TRUE = 1;
const FALSE = 0;

// pieces in chess
const PWAN = 1;
const BISHOP = 2;
const ROOK = 3;
const KNIGHT = 4;
const QUEEN = 5;
const KING = 6;

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
    let (piece_num, piece_color, piece_type) = getPiece(fromIndex);
    let piece = Uint256(piece_num, 0);
    let (bool: felt) = isLegalMove(fromIndex, toIndex);

    assert fromIndex.low = 0;

    // check if the move is valid
    with_attr error_message("Illegal move") {
        assert bool = 1;
    }

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

func searchMove{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, depth: Uint256) -> (bestMove: Uint256, bool: felt) {
    alloc_locals;
    // github copilot please stop
    // initialized values? @Yetta board and movesArray
    let (movesArray) = generateMove(
        36, Uint256(0x238A179D71B69959551349138D30B289, 0xDB5D33CB1BADB2BAA99A59)
    );

    if (movesArray.low == 0) {
        return (Uint256(0, 0), TRUE);
    }
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
    let (aMovesArray: Uint256) = movesArray.read(index.low);
    if (aMovesArray.low == 0) {
        return (-4196, Uint256(0, 0));
    }

    let (score, move) = rec1(board, Uint256(index.low + 1, 0), depth);
    let (aaa, bbb) = rec2(board, aMovesArray, depth);
    let bool: felt = is_le(score, aaa - 1);
    if (bool == TRUE) {
        return (aaa, bbb);
    } else {
        return (score, move);
    }
}

func rec2{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, aMovesArray: Uint256, depth: Uint256) -> (bestScore: felt, bestMove: Uint256) {
    alloc_locals;

    let (aMovesArray_shifted) = uint256_shr(aMovesArray, Uint256(12, 0));

    // check index's_storage_val & 0xFFF is empty
    // and if it is, return -4_196
    let (lastMove) = uint256_and(aMovesArray, Uint256(0xFFF, 0));
    if (lastMove.low == 0) {
        return (-4196, Uint256(0, 0));
    }

    // check score of index's_storage_val & 0x3F
    // and compare with currentScore and return bigger one
    let (bestScore, bestMove) = rec2(board, aMovesArray_shifted, depth);

    let (currMove) = uint256_and(aMovesArray, Uint256(0xFFF, 0));

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

// @Yetta: need to define play depth
func negaMax{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(sim_board: Uint256, depth: Uint256) -> (res: felt) {
    alloc_locals;
    if (depth.low == 0) {
        return (res = 0);
    }
    let (movesArray_0: Uint256) = getMovesArray(0);
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
    let (aMovesArray: Uint256) = movesArray.read(index.low);
    if (aMovesArray.low == 0) {
        return (-4196, Uint256(0, 0));
    }

    let (score, move) = realBest(board, Uint256(index.low + 1, 0));
    let (aaa, bbb) = findBestLocal(board, aMovesArray);

    let bool: felt = is_le(score, aaa - 1);
    if (bool == TRUE) {
        return (aaa, bbb);
    } else {
        return (score, move);
    }
}

// find the best move in a given array of moves
func findBestLocal{
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
    let (bestScore, bestMove) = findBestLocal(board, aMovesArray_shifted);

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
    let (piece_num_from, piece_color_from, piece_type_from) = simGetPiece(board, fromIndex);
    let (piece_num_to, piece_color_to, piece_type_to) = simGetPiece(board, toIndex);
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
    let (piece_num_felt, _, _) = simGetPiece(curr_board, fromIndex);
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

func simGetPiece{
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
// external func
//

// AI player functions
@external
func aiApplyMove{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr, bitwise_ptr: BitwiseBuiltin*
}(board: Uint256, depth: Uint256) -> (boardAfterPlay: Uint256, isOver: felt) {
    alloc_locals;
    // searchMove returns bestMove
    // if bestMove is 0, then no move is available
    // else apply Best Move.
    // let board_status : Uint256 = board
    let (bestMove, isOver) = searchMove(board, depth);

    // if AI can't move anywhere, then the game is over
    if (isOver == TRUE) {
        return (board, isOver);
    }

    // get first 6bits from the bestMove (111111000000 = 0xFC0)
    let (fromIndex: Uint256) = uint256_and(bestMove, Uint256(0xFC0, 0));
    // get last 6bits from the bestMove
    let (toIndex: Uint256) = uint256_and(bestMove, Uint256(0x3F, 0));

    let (boardAfterPlay: Uint256) = applyMove_forAI(fromIndex, toIndex, board);

    return (boardAfterPlay, FALSE);
}
