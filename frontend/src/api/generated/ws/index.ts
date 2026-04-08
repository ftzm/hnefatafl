// This file was auto-generated from asyncapi.json. Do not edit.

type AiServerMessage = AiGameState | AiEngineMoved | AiGameOver | AiUndoApplied | AiError;

type AiClientMessage = AiMove | AiUndo | AiResign | AiOfferDraw | AiAcceptDraw | AiDeclineDraw;

type AiMinusWs = AiMove | AiUndo | AiResign | AiOfferDraw | AiAcceptDraw | AiDeclineDraw | AuthMessage;

type OnlineServerMessage = OnlineGameState | OnlineOpponentMoved | OnlineGameOver | OnlineDrawOffered | OnlineDrawDeclined | OnlineUndoRequested | OnlineUndoAccepted | OnlineUndoDeclined | OnlineError;

type OnlineClientMessage = OnlineMove | OnlineResign | OnlineOfferDraw | OnlineAcceptDraw | OnlineDeclineDraw | OnlineRequestUndo | OnlineAcceptUndo | OnlineDeclineUndo;

type OnlineMinusWs = OnlineMove | OnlineResign | OnlineOfferDraw | OnlineAcceptDraw | OnlineDeclineDraw | OnlineRequestUndo | OnlineAcceptUndo | OnlineDeclineUndo | AuthMessage;

interface AiGameState {
  board: ApiBoard;
  gameId: string;
  reservedHistory: AppliedMovePayload[];
  humanColor: PlayerColor;
  pendingAction?: PendingActionPayload;
  reservedStatus: Ongoing | Finished;
  turn: string;
  reservedType: AnonymousSchema_28;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

interface ApiBoard {
  black: number[];
  king: number;
  white: number[];
  additionalProperties?: Map<string, any>;
}

interface AppliedMovePayload {
  move: ApiMove;
  side: PlayerColor;
  additionalProperties?: Map<string, any>;
}

interface ApiMove {
  captures: number[];
  dest: number;
  orig: number;
  additionalProperties?: Map<string, any>;
}

type PlayerColor = "white" | "black";

interface PendingActionPayload {
  actionType: string;
  offeredBy: PlayerColor;
  additionalProperties?: Map<string, any>;
}

interface Ongoing {
  state: AnonymousSchema_24;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_24 = "ongoing";

interface Finished {
  reason: GameEndReason;
  state: AnonymousSchema_26;
  winner?: PlayerColor;
  additionalProperties?: Map<string, any>;
}

type GameEndReason = "king_captured" | "white_surrounded" | "no_moves" | "king_escaped" | "exit_fort" | "resignation" | "timeout" | "draw" | "abandoned";

type AnonymousSchema_26 = "finished";

type AnonymousSchema_28 = "game_state";

interface AiEngineMoved {
  board: ApiBoard;
  move: ApiMove;
  side: PlayerColor;
  reservedStatus: Ongoing | Finished;
  turn: string;
  reservedType: AnonymousSchema_32;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_32 = "engine_moved";

interface AiGameOver {
  reservedStatus: Ongoing | Finished;
  reservedType: AnonymousSchema_35;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_35 = "game_over";

interface AiUndoApplied {
  board: ApiBoard;
  reservedStatus: Ongoing | Finished;
  turn: string;
  reservedType: AnonymousSchema_38;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_38 = "undo_applied";

interface AiError {
  code: WsErrorCode;
  message: string;
  reservedType: AnonymousSchema_42;
  additionalProperties?: Map<string, any>;
}

type WsErrorCode = "invalid_message" | "invalid_auth" | "invalid_token" | "not_your_turn" | "game_already_finished" | "invalid_move" | "no_pending_offer" | "cannot_respond_to_own_offer" | "action_already_pending" | "no_moves_to_undo" | "engine_error" | "engine_search_failed";

type AnonymousSchema_42 = "error";

interface AiMove {
  dest: number;
  orig: number;
  reservedType: AnonymousSchema_6;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_6 = "move";

interface AiUndo {
  reservedType: AnonymousSchema_8;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_8 = "undo";

interface AiResign {
  reservedType: AnonymousSchema_10;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_10 = "resign";

interface AiOfferDraw {
  reservedType: AnonymousSchema_12;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_12 = "offer_draw";

interface AiAcceptDraw {
  reservedType: AnonymousSchema_14;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_14 = "accept_draw";

interface AiDeclineDraw {
  reservedType: AnonymousSchema_16;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_16 = "decline_draw";

interface AuthMessage {
  token: string;
  reservedType: AnonymousSchema_2;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_2 = "auth";

interface OnlineGameState {
  board: ApiBoard;
  gameId: string;
  reservedHistory: AppliedMovePayload[];
  pendingAction?: PendingActionPayload;
  reservedStatus: Ongoing | Finished;
  turn: PlayerColor;
  reservedType: AnonymousSchema_63;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_63 = "game_state";

interface OnlineOpponentMoved {
  board: ApiBoard;
  move: ApiMove;
  side: PlayerColor;
  reservedStatus: Ongoing | Finished;
  turn: PlayerColor;
  reservedType: AnonymousSchema_66;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_66 = "opponent_moved";

interface OnlineGameOver {
  reservedStatus: Ongoing | Finished;
  reservedType: AnonymousSchema_69;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_69 = "game_over";

interface OnlineDrawOffered {
  by: PlayerColor;
  reservedType: AnonymousSchema_71;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_71 = "draw_offered";

interface OnlineDrawDeclined {
  reservedType: AnonymousSchema_73;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_73 = "draw_declined";

interface OnlineUndoRequested {
  by: PlayerColor;
  reservedType: AnonymousSchema_75;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_75 = "undo_requested";

interface OnlineUndoAccepted {
  board: ApiBoard;
  reservedStatus: Ongoing | Finished;
  turn: PlayerColor;
  reservedType: AnonymousSchema_77;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_77 = "undo_accepted";

interface OnlineUndoDeclined {
  reservedType: AnonymousSchema_80;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_80 = "undo_declined";

interface OnlineError {
  code: WsErrorCode;
  message: string;
  reservedType: AnonymousSchema_83;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_83 = "error";

interface OnlineMove {
  dest: number;
  orig: number;
  reservedType: AnonymousSchema_46;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_46 = "move";

interface OnlineResign {
  reservedType: AnonymousSchema_48;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_48 = "resign";

interface OnlineOfferDraw {
  reservedType: AnonymousSchema_50;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_50 = "offer_draw";

interface OnlineAcceptDraw {
  reservedType: AnonymousSchema_52;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_52 = "accept_draw";

interface OnlineDeclineDraw {
  reservedType: AnonymousSchema_54;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_54 = "decline_draw";

interface OnlineRequestUndo {
  reservedType: AnonymousSchema_56;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_56 = "request_undo";

interface OnlineAcceptUndo {
  reservedType: AnonymousSchema_58;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_58 = "accept_undo";

interface OnlineDeclineUndo {
  reservedType: AnonymousSchema_60;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_60 = "decline_undo";

