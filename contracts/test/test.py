from shutil import move
import pytest
import os
from starkware.starknet.testing.starknet import Starknet
import asyncio
import random
from enum import Enum
import logging

LOGGER = logging.getLogger(__name__)
TEST_NUM_PER_CASE = 200
PRIME = 3618502788666131213697322783095070105623107215331596699973092056135872020481
PRIME_HALF = PRIME//2
PLANET_DIM = 100

## Note to test logging:
## `--log-cli-level=INFO` to show logs

@pytest.mark.asyncio
async def test_game ():

    starknet = await Starknet.empty()

    print(f'> Deploying game.cairo ..')
    contract = await starknet.deploy (
        source = 'contracts/chess_board.cairo',
        constructor_calldata = []
    )

    #####################################################
    # Test `get_cube_operation()`
    #####################################################
    print('> Testing getBoardStatus()')
    initial = await contract.getBoardStatus().call()
    print(initial.result)

    await contract.applyMove((17,0),(25,0),3).invoke()

    print()
    print('> Testing getBoardStatus()')

    a = await contract.getBoardStatus().call()
    print(a.result)
    
    print()
    print('> Testing getTestBoardRotate()')
    aa = await contract.getTestBoardRotate().call()
    print(aa.result)

    print()
    print('> Testing getMovesArray()')
    aaa = await contract.getMovesArray(0).call()
    print(aaa.result)

    print()
    print('> Testing getMovesArray()')
    aaa = await contract.getMovesArray(1).call()
    print(aaa.result)

    