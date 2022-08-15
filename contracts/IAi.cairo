%lang starknet

from starkware.cairo.common.uint256 import Uint256

@contract_interface
namespace IAi:
    # moveFunction
    func aiApplyMove(depth : Uint256) -> (isOver : felt):
    end
end