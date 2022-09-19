%lang starknet

from starkware.cairo.common.uint256 import Uint256

@contract_interface
namespace IAi{
    // moveFunction
    func aiApplyMove(board : Uint256,depth : Uint256) -> (board : Uint256, isOver : felt){
    }
}