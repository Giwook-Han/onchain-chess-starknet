# Chess-On-Chain |StarkNet ✨

A fully on-chain player-vs-AI chess game built on StarkNet

## play the game 
- go to website: 
- View chess board contract: 
- View AI player contract: 

## Game Rules

                                Black
                       00 00 00 00 00 00 00 00                    Black
                       00 00 02 05 06 02 03 00                 ♜ ♝ ♛ ♚ ♝ ♜
                       00 01 01 01 01 01 01 00                 ♟ ♟ ♟ ♟ ♟ ♟
                       00 00 00 00 00 00 00 00     denotes
                       00 00 00 00 00 00 00 00    the board
                       00 09 09 09 09 09 00 00                 ♙ ♙ ♙ ♙ ♙ ♙
                       00 11 12 13 14 12 11 00                 ♖ ♘ ♕ ♔ ♘ ♖
                       00 00 00 00 00 00 00 01                    White
                                White

- A 6x6 simplified chess board (each side has one knight, one bishop, and six pawns)
- Player plays against an AI engine that can be adjusted to different game depths as defined by the player
    - The value `depth` can range from 3 to 20. 
    - The larger the `depth` value is, the more intelligent the AI becomes.
    - Larger `depth` value also comes with higher gas fees, but is generally affordable on StarkNet.
- The game automatically terminates when checkmate is found by an engine. 

## A Gas-optimized Game
Greatly enhanced computation efficiency via bitpacking and bitwise operations.

### chess board representation
- The board is an 8x8 representation of a 6x6 chess board. For efficiency, all information is
bitpacked into a single uint256, and board positions are accessed via bit shifts and bit masks.
- Since each piece is 4 bits, there are 64 `indices` to access, and a total numnber of 256 bits are used. 
- For example, the piece at index 14 is accessed with `(board >> (14 << 2)) & 'OxF'`.
- The top/bottom rows and left/right columns are treated as sentinel rows/columns for efficient
boundary validation. Namely, (07, ..., 00), (63, ..., 07), and (56, ..., 00) never contain pieces.

    | 63 | 62 | 61 | 60 | 59 | 58 | 57 | 56 |
    | 55 | 54 | 53 | 52 | 51 | 50 | 49 | 48 |
    | 47 | 46 | 45 | 44 | 43 | 42 | 41 | 40 |
    | 39 | 38 | 37 | 36 | 35 | 34 | 33 | 32 |  
    | 31 | 30 | 29 | 28 | 27 | 26 | 25 | 24 |
    | 23 | 22 | 21 | 20 | 19 | 18 | 17 | 16 |
    | 15 | 14 | 13 | 12 | 11 | 10 | 09 | 08 |
    | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |

and columns should be ignored, except for the last bit. The last bit denotes whose turn it is to
play (0 means black's turn; 1 means white's turn). e.g. a potential starting position:

### chess piece representation
- Each chess piece is defined with 4 bits as follows:
    The first bit denotes the color: 0 means black; 1 means white).
    The last 3 bits denote the type:

            | Bits | # | Type   |
            | ---- | - | ------ |
            | 000  | 0 | Empty  |
            | 001  | 1 | Pawn   |
            | 010  | 2 | Bishop |
            | 011  | 3 | Rook   |
            | 100  | 4 | Knight |
            | 101  | 5 | Queen  |
            | 110  | 6 | King   |

    - for example, a black Knight is represented by 0100, a white Bishop is represented by 1010, etc. 

- the program automatically assigns white to player and black to AI. 

- Highly efficient program, using bitpacking to represent chess moves. 
- Can play against an AI player. You can manually define the depth of the game, and the AI player will adjust accordingly;

## Program Architecture
- The game rules are defined in chess_board.cairo. 
- The AI decision-making machine is defined in ai_player.cairo

> *made with soul at StarkNet House Hackathon, 08.16.2022*