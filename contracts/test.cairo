%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

@storage_var
func negative_num() -> (res : felt):
end

@external
func assign_neg{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*, 
    range_check_ptr
    }(neg_num : felt) -> ():

    negative_num.write(neg_num)
    return ()
end