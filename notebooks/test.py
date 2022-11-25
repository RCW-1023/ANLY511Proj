# !pip install chessdata
from pathlib import Path
from chessdata.etf import pgn2df
path = Path("../")
pgns = Path(path/"pgns")
players = [pgn.stem for pgn in pgns.glob("*.pgn")]
players
for player in players:
    pgn = open(pgns/f"{player}.pgn")
    df = pgn2df(pgn)
    df = df[df['Date'] > '2020.01.01']
    print(player)
    print(df.shape)
    df.to_csv(path/f"output/metadata/{player}.csv")
from datetime import datetime
import chess.engine
from chessdata.engine import evaluate_pgn
stockfish = '../stockfish_15_x64_avx2.exe' # location of your stockfish executable
players1 = players[0]
for player in players1:
    print(player)
    print(datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
    engine = chess.engine.SimpleEngine.popen_uci(stockfish)
    pgn = open(pgns/f"{player}.pgn")
    evals = evaluate_pgn(pgn, engine, limit=chess.engine.Limit(depth=15))
    evals.to_csv(path/f"output/centipawns/{player}.csv")