// This file was auto-generated from asyncapi.json. Do not edit.

type AiServerMessage = AiGameState | AiEngineMoved | AiGameOver | AiUndoApplied;

type AiMinusWs = AiMove | AiUndo | AiResign | AiOfferDraw | AiAcceptDraw | AiDeclineDraw | AuthMessage;

type AiClientMessage = AiMove | AiUndo | AiResign | AiOfferDraw | AiAcceptDraw | AiDeclineDraw;

type OnlineServerMessage = OnlineGameState | OnlineOpponentMoved | OnlineGameOver | OnlineDrawOffered | OnlineDrawDeclined | OnlineUndoRequested | OnlineUndoAccepted | OnlineUndoDeclined;

type OnlineMinusWs = OnlineMove | OnlineResign | OnlineOfferDraw | OnlineAcceptDraw | OnlineDeclineDraw | OnlineRequestUndo | OnlineAcceptUndo | OnlineDeclineUndo | AuthMessage;

type OnlineClientMessage = OnlineMove | OnlineResign | OnlineOfferDraw | OnlineAcceptDraw | OnlineDeclineDraw | OnlineRequestUndo | OnlineAcceptUndo | OnlineDeclineUndo;

interface AiGameState {
  board: ApiBoard;
  gameId: string;
  reservedHistory: AppliedMovePayload[];
  humanColor: PlayerColor;
  pendingAction?: PendingActionPayload;
  reservedStatus: Ongoing | Finished;
  turn: string;
  reservedType: AnonymousSchema_30;
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
  state: AnonymousSchema_26;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_26 = "ongoing";

interface Finished {
  reason: GameEndReason;
  state: AnonymousSchema_28;
  winner?: PlayerColor;
  additionalProperties?: Map<string, any>;
}

type GameEndReason = "king_captured" | "white_surrounded" | "no_moves" | "king_escaped" | "exit_fort" | "resignation" | "timeout" | "draw" | "abandoned";

type AnonymousSchema_28 = "finished";

type AnonymousSchema_30 = "game_state";

interface AiEngineMoved {
  board: ApiBoard;
  move: ApiMove;
  side: PlayerColor;
  reservedStatus: Ongoing | Finished;
  turn: string;
  reservedType: AnonymousSchema_34;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_34 = "engine_moved";

interface AiGameOver {
  reservedStatus: Ongoing | Finished;
  reservedType: AnonymousSchema_37;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_37 = "game_over";

interface AiUndoApplied {
  board: ApiBoard;
  reservedStatus: Ongoing | Finished;
  turn: string;
  reservedType: AnonymousSchema_40;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_40 = "undo_applied";

interface WsError {
  code: WsErrorCode;
  message: string;
  reservedType: AnonymousSchema_18;
  additionalProperties?: Map<string, any>;
}

type WsErrorCode = "invalid_message" | "invalid_auth" | "invalid_token" | "not_your_turn" | "game_already_finished" | "invalid_move" | "no_pending_offer" | "cannot_respond_to_own_offer" | "action_already_pending" | "no_moves_to_undo" | "engine_error" | "engine_search_failed" | "internal_error";

type AnonymousSchema_18 = "error";

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
  reservedType: AnonymousSchema_62;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_62 = "game_state";

interface OnlineOpponentMoved {
  board: ApiBoard;
  move: ApiMove;
  side: PlayerColor;
  reservedStatus: Ongoing | Finished;
  turn: PlayerColor;
  reservedType: AnonymousSchema_65;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_65 = "opponent_moved";

interface OnlineGameOver {
  reservedStatus: Ongoing | Finished;
  reservedType: AnonymousSchema_68;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_68 = "game_over";

interface OnlineDrawOffered {
  by: PlayerColor;
  reservedType: AnonymousSchema_70;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_70 = "draw_offered";

interface OnlineDrawDeclined {
  reservedType: AnonymousSchema_72;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_72 = "draw_declined";

interface OnlineUndoRequested {
  by: PlayerColor;
  reservedType: AnonymousSchema_74;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_74 = "undo_requested";

interface OnlineUndoAccepted {
  board: ApiBoard;
  reservedStatus: Ongoing | Finished;
  turn: PlayerColor;
  reservedType: AnonymousSchema_76;
  validMoves: ApiMove[];
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_76 = "undo_accepted";

interface OnlineUndoDeclined {
  reservedType: AnonymousSchema_79;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_79 = "undo_declined";

interface OnlineMove {
  dest: number;
  orig: number;
  reservedType: AnonymousSchema_45;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_45 = "move";

interface OnlineResign {
  reservedType: AnonymousSchema_47;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_47 = "resign";

interface OnlineOfferDraw {
  reservedType: AnonymousSchema_49;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_49 = "offer_draw";

interface OnlineAcceptDraw {
  reservedType: AnonymousSchema_51;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_51 = "accept_draw";

interface OnlineDeclineDraw {
  reservedType: AnonymousSchema_53;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_53 = "decline_draw";

interface OnlineRequestUndo {
  reservedType: AnonymousSchema_55;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_55 = "request_undo";

interface OnlineAcceptUndo {
  reservedType: AnonymousSchema_57;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_57 = "accept_undo";

interface OnlineDeclineUndo {
  reservedType: AnonymousSchema_59;
  additionalProperties?: Map<string, any>;
}

type AnonymousSchema_59 = "decline_undo";

