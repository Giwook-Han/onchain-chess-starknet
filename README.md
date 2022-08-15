# StarkNet-Hackathon-Chess

a fully On-Chain chess game built on the StarkNet

## play the game 
go to website: 

- A 6x6 simplified chess (each side gets one knight and one bishop).
- Highly efficient program, using bitpacking to represent chess moves. 
- Can play against an AI player. You can manually define the depth of the game, and the AI player will adjust accordingly; The larger the depth value is, the more intelligent the AI becomes. It also costs more gas fee but is quite affordable on StarkNet overall.  

The game rules are defined in chess_board.cairo. 
The AI decision-making machine is defined in ai_player.cairo

