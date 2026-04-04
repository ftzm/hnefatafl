# Game Rules (Copenhagen Hnefatafl)

## Overview

Two asymmetric sides on an 11x11 board. **Black** (attackers, 24 pawns) moves first and tries to capture the king. **White** (defenders, 12 pawns + 1 king) tries to escape the king to a corner.

## Board Layout

- **Throne**: center square (index 60). Only the king may stop on it. The empty throne acts as a hostile square for captures (against both sides).
- **Corners**: the 4 corner squares (indices 0, 10, 110, 120). Only the king may stop on them. Corners are hostile to both sides (participate in custodial captures).

## Movement

All pieces (king, black pawns, white pawns) move like a rook in chess: any number of empty squares along a rank or file. No jumping. No diagonal movement. Only the king may land on the throne or corners.

## Captures

- **Custodial capture**: a pawn is captured when an enemy move creates a sandwich — the pawn is flanked on two opposite sides (horizontally or vertically) by enemy pieces. Multiple captures can occur in a single move.
- **Hostile squares**: the throne (when empty) and all 4 corners count as enemy pieces for capture purposes (hostile to both sides).
- **King capture**: the king must be surrounded on all 4 orthogonal sides by black pieces (or black pieces + the empty throne). The king cannot be captured at a board edge.
- **Shield wall capture**: a row of enemy pieces along a board edge can be captured if they are completely enclosed — flanked at both ends and backed by an unbroken line of friendly pieces on the adjacent inner row. Corners also anchor shield walls. The king can participate in shield wall captures on the white side.

## Win Conditions

**White wins by:**
- **King escape**: the king reaches any corner square (the engine also treats squares adjacent to corners as effective escapes for early detection).
- **Exit fort**: the king is on an edge and surrounded by an unbreakable formation of white pieces forming a fort with access to a corner — black cannot prevent escape.
- **No black moves**: black has no legal moves on their turn.

**Black wins by:**
- **King captured**: the king is surrounded on all 4 sides (see above).
- **White surrounded**: all white pieces (king + pawns) are enclosed in a region that cannot reach any board edge, via flood-fill reachability check.
- **No white moves**: white has no legal moves on their turn.

## Administrative Endings

When all 4 corners are guarded by adjacent black pieces:
- **White resignation**: offered to white when white has fewer than 4 pawns and black has 16+ pawns.
- **Draw offer**: either side may be offered a draw when white has fewer than 4 pawns and black has fewer than 16 pawns.

## Repetition

The game tracks position history via Zobrist hashing. Repeated positions are detected for draw adjudication.
