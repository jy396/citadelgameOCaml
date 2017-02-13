README.txt
CS 3110 Final Project
Team of: gk297 Gee Hyun Kwon, mk898 Minae Kwon, jy396 Joo Ho Yeo, sk956 Se Yun Kim

INSTALLATION
-Make sure your OCaml version is 4.03.0.
-Make sure that you have ANSITerminal and Yojson library installed through opam.
-On console, in the src directory of our game source file, type in “make play.”

GAME RULES

Our game was inspired by the board game Citadel, by Bruno Faidutti.

The game is played with 4~7 players. There are 8 unique “jobs” that have special themed abilities (we will use the term actors interchangeably with jobs). At the beginning, the players are given 2 gold pieces and 2 building cards. 

The game consists of rounds. Each round, the players choose a new job from a deck of job cards. Before the players pick, depending on the number of players, several jobs are removed face up (so that everyone knows that these jobs are not played) and several are removed face down (for suspense-in practice to prevent the Assassin and the Thief being able to get value from their power all the time). The person who gets to pick the jobs first is chosen at random at the first round, then afterwards following the rule related with the job “king” (explained later). After the picking phase is over, the job and the person who has picked that job is revealed, in order of the job number. The player with the revealed job can start his/her turn. These are the jobs:

Assassin: Choose a job to assassinate. The player with that job cannot play his/her turn.
Thief: Choose a job to steal gold from. At the beginning of the player with that job’s turn, steal all his/her gold.
Magician: Choose either to receive 4 gold or receive 4 building cards. You cannot build any buildings this turn.
King: You get 1 extra gold for each yellow building you have built. At the next round, you are the first person to pick. This lasts until someone else becomes king.
Bishop: You get 1 extra gold for each blue building you have built. Your buildings cannot be destroyed by the warlord.
Merchant: You get 1 extra gold for each green building you have built. After doing an action (take 2 gold or draw 2 card) you receive 1 extra gold.
Architect: You can build 2 buildings in one turn. After doing an action you receive 2 extra building cards.
Warlord: You get 1 extra gold for each red building you have built. You can select a job and destroy a building owned by the player playing that job, by paying gold equal to the selected building’s cost -1.

On each player’s turn, the player does 3 things, in that order.
The player chooses to receive 2 gold or receive 2 building cards and discard one of them. This must be done--the player needs to pick between the two.
The player can use the power of their jobs. “Passive” job powers (such as of the Bishop) are activated immediately.
The player can choose to build a building from their hand by paying the gold equal to its value.
After all of the jobs have been revealed, the picking phase resumes. The game goes on until a person built 6 buildings. After this, the round where the person built 6 buildings becomes the last round. 

After the final round, the scores of each players’ buildings are added up. The person with the most points wins. 
There are several bonus points that are added up in the end:
-The first person to build 6 buildings gets 4 extra points.
-The second person to build 6 buildings gets 2 extra points, the third person to do so gets 1.
-Any player that has buildings of each color -- green, red, blue, yellow -- gets 4 extra points.

HOW TO PLAY OUR GAME

Our game will prompt you at the start to choose how many players (including you) would be in the game. Type a number between 4 and 7 and press enter. If you enter a number other than that, we will initialize a 4-player game for you.

During any phase you can type in these commands to get information (without the -'s)
-help : shows you the general help function of the game, which tells you how the game works.
-help # (where # is a number between 1~8): shows you the description of the job with the given number #
-hand : shows you the cards you have at hand
-score : shows you the score of all players
-buildings : shows you the buildings you have built

The first phase is the picking phase. During this phase, the other AI players will pick their respective jobs. When your turn comes, you can type a job name in lowercase in order to select the job for this round to play.

After the picking phase, the jobs and the player behind that job will be revealed, starting from job number 1 (Assassin) to job number 8 (Warlord). When your turn comes, you will be prompted to either choose to receive gold or drawing two cards and discarding one of them. Type "gold" or "draw" to choose. If you choose to draw you will be ask which card you would like to discard. Afterwards, if you have an ability that you can target an actor (such as the assassin, thief, and the warlock) you can choose the one you would like to use your ability on. The last part of your turn is deciding which building to build. Simply type the building name to build the building or type "none" to build nothing.
After this, unless there is someone who has 6 buildings by the end of the round (and thus finishing the game) the game goes back to the picking round.

