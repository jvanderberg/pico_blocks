' ==========================================
' PicoCalc Blocks Game (with FRAMEBUFFER)
' - True double buffering using FRAMEBUFFER + LAYER
' - Static background on F, dynamic sprites on L
' - FRAMEBUFFER MERGE for flicker-free display
' ==========================================
OPTION EXPLICIT

' ---- Tunables ----
CONST PAD_ACCEL = 2.0
CONST PAD_DECAY = 0.88
CONST PAD_MAX   = 24
CONST BRADIUS   = 6
CONST BALL_PAD  = 4
CONST BALL_SPEED_INIT = 2.0
CONST BALL_SPEED_ACCEL_PER_SEC = 0.03
CONST PADDLE_KICK = 0.5
CONST TICK_MS   = 38

' ---- Screen & colors ----
CONST W% = MM.HRES
CONST H% = MM.VRES
CONST HUDH% = 18

CONST COL_BG%     = RGB(BLACK)
CONST COL_TXT%    = RGB(WHITE)
CONST COL_GRID%   = &H888888
CONST COL_BORDER% = RGB(MYRTLE)
CONST COL_PAD%    = RGB(GREEN)
CONST COL_BALL%   = RGB(RED)
CONST COL_BLOCK%  = RGB(CYAN)

' ---- Block layout ----
CONST BLOCK_ROWS% = 5
CONST BLOCK_COLS% = 8
CONST BLOCK_W% = 35
CONST BLOCK_H% = 12
CONST BLOCK_GAP% = 4
CONST BLOCK_TOP% = 40

' ---- State ----
DIM FLOAT bx!, by!
DIM INTEGER br%
DIM FLOAT vx!, vy!
DIM INTEGER lastAccelTime%
DIM px!, py!, pw%, ph%, pvx!
DIM INTEGER score%, misses%
DIM INTEGER frames%, t0%
DIM fps$
DIM k$
DIM INTEGER blocks%(BLOCK_ROWS%-1, BLOCK_COLS%-1)
DIM INTEGER totalBlocks%, blocksLeft%

' ---- Beeps ----
SUB BeepServe(): PLAY TONE 700,700 : PAUSE 40 : PLAY STOP : END SUB
SUB BeepPaddle(): PLAY TONE 800,800 : PAUSE 20 : PLAY STOP : END SUB
SUB BeepWall(): PLAY TONE 600,600 : PAUSE 20 : PLAY STOP : END SUB
SUB BeepBlock(): PLAY TONE 1200,1200 : PAUSE 25 : PLAY STOP : END SUB
SUB BeepMiss(): PLAY TONE 200,200 : PAUSE 80 : PLAY STOP : END SUB


' ---- Drawing helpers ----
SUB DrawStatic()
  CLS COL_BG%
END SUB

SUB DrawHUD()
  LOCAL s$
  s$ = "Score " + STR$(score%) + "   Miss " + STR$(misses%) + "   " + TIME$
  TEXT 6, 3, s$, "LT", , , COL_TXT%, COL_BG%
  IF fps$ <> "" THEN
    TEXT W%-4, 3, fps$, "RT", , , COL_TXT%, COL_BG%
  END IF
END SUB

SUB DrawPaddleAt(x%, y%)
  BOX x%, y%, pw%, ph%, 0, , COL_PAD%
END SUB

SUB DrawBallAt(x%, y%)
  CIRCLE x%+br%, y%+br%, br%, 0, 1.0, , COL_BALL%
END SUB

SUB InitBlocks()
  LOCAL r%, c%
  totalBlocks% = 0
  FOR r% = 0 TO BLOCK_ROWS%-1
    FOR c% = 0 TO BLOCK_COLS%-1
      blocks%(r%, c%) = 1
      totalBlocks% = totalBlocks% + 1
    NEXT
  NEXT
  blocksLeft% = totalBlocks%
END SUB

FUNCTION GetBlockX(c%) AS INTEGER
  GetBlockX = c% * (BLOCK_W% + BLOCK_GAP%) + BLOCK_GAP%
END FUNCTION

FUNCTION GetBlockY(r%) AS INTEGER
  GetBlockY = BLOCK_TOP% + r% * (BLOCK_H% + BLOCK_GAP%)
END FUNCTION

SUB DrawBlocks()
  LOCAL r%, c%, bx%, by%
  FOR r% = 0 TO BLOCK_ROWS%-1
    FOR c% = 0 TO BLOCK_COLS%-1
      IF blocks%(r%, c%) = 1 THEN
        bx% = GetBlockX(c%)
        by% = GetBlockY(r%)
        BOX bx%, by%, BLOCK_W%, BLOCK_H%, 0, , COL_BLOCK%
        BOX bx%, by%, BLOCK_W%, BLOCK_H%, 1, COL_BORDER%
      END IF
    NEXT
  NEXT
END SUB

SUB EraseBlock(r%, c%)
  LOCAL bx%, by%
  bx% = GetBlockX(c%)
  by% = GetBlockY(r%)
  FRAMEBUFFER WRITE F
  BOX bx%, by%, BLOCK_W%, BLOCK_H%, 0, , COL_BG%
  FRAMEBUFFER WRITE L  ' Restore to layer
END SUB

FUNCTION CheckBlockCollision(ballX%, ballY%, ballR%) AS INTEGER
  LOCAL r%, c%, bx%, by%, bx2%, by2%
  LOCAL ballLeft%, ballRight%, ballTop%, ballBot%

  ballLeft% = ballX%
  ballRight% = ballX% + 2*ballR%
  ballTop% = ballY%
  ballBot% = ballY% + 2*ballR%

  FOR r% = 0 TO BLOCK_ROWS%-1
    FOR c% = 0 TO BLOCK_COLS%-1
      IF blocks%(r%, c%) = 1 THEN
        bx% = GetBlockX(c%)
        by% = GetBlockY(r%)
        bx2% = bx% + BLOCK_W%
        by2% = by% + BLOCK_H%

        ' AABB collision test
        IF ballRight% >= bx% AND ballLeft% <= bx2% AND ballBot% >= by% AND ballTop% <= by2% THEN
          blocks%(r%, c%) = 0
          blocksLeft% = blocksLeft% - 1
          EraseBlock r%, c%
          CheckBlockCollision = 1
          EXIT FUNCTION
        END IF
      END IF
    NEXT
  NEXT
  CheckBlockCollision = 0
END FUNCTION

' ---- Init ----
pw% = W% \ 6 : IF pw% < 30 THEN pw% = 30  ' (no MAX(); clamp via IF)
ph% = 6
px! = (W% - pw%) / 2
py! = H% - (ph% + 6)
pvx! = 0

br% = BRADIUS
bx! = W% \ 2 : by! = H% \ 2
vx! = BALL_SPEED_INIT
vy! = BALL_SPEED_INIT

score% = 0 : misses% = 0
frames% = 0 : t0% = TIMER
fps$ = ""
lastAccelTime% = TIMER

InitBlocks

' ---- Setup Framebuffers ----
FRAMEBUFFER CREATE           ' Create framebuffer F
FRAMEBUFFER LAYER RGB(BLACK) ' Create layer L with black as transparent

' Draw static background to F
FRAMEBUFFER WRITE F
DrawStatic
DrawBlocks

' Draw initial sprites to layer L
FRAMEBUFFER WRITE L
CLS RGB(BLACK)  ' Clear layer with transparent color
DrawPaddleAt INT(px!), INT(py!)
DrawBallAt INT(bx!), INT(by!)

' Draw HUD to framebuffer F
FRAMEBUFFER WRITE F
DrawHUD

' Start timed continuous merge (updates at TICK_MS rate)
FRAMEBUFFER MERGE RGB(BLACK), R, TICK_MS

BeepServe

' ---- Main loop ----
DO
  ' INPUT (Left=130, Right=131, ESC=27; 1..8 tones)
  k$ = INKEY$
  IF k$ <> "" THEN
    SELECT CASE ASC(k$)
      CASE 130: pvx! = pvx! - PAD_ACCEL
      CASE 131: pvx! = pvx! + PAD_ACCEL
      CASE  27: EXIT DO
      CASE  49 TO 56
        PLAY TONE 220 * (2 ^ ((ASC(k$)-48)/12.0)), 0 : PAUSE 60 : PLAY STOP
    END SELECT
  ENDIF

  ' ---- Paddle physics ----
  pvx! = pvx! * PAD_DECAY
  IF pvx! >  PAD_MAX THEN pvx! =  PAD_MAX
  IF pvx! < -PAD_MAX THEN pvx! = -PAD_MAX
  px! = px! + pvx!
  IF px! < 0 THEN px! = 0 : IF pvx! < 0 THEN pvx! = 0
  IF px! > (W% - pw%) THEN px! = W% - pw% : IF pvx! > 0 THEN pvx! = 0

  ' ---- Ball physics ----
  ' Gradually increase speed over time (once per second)
  IF TIMER - lastAccelTime% >= 1000 THEN
    IF vy! > 0 THEN
      vy! = vy! + BALL_SPEED_ACCEL_PER_SEC
    ELSE
      vy! = vy! - BALL_SPEED_ACCEL_PER_SEC
    END IF
    lastAccelTime% = lastAccelTime% + 1000
  END IF

  bx! = bx! + vx!
  by! = by! + vy!

  ' Elastic wall bounces
  IF INT(bx!) < 0 THEN
    bx! = 0
    vx! = -vx!
    BeepWall
  END IF
  IF INT(bx!) > W% - 2*br% THEN
    bx! = W% - 2*br%
    vx! = -vx!
    BeepWall
  END IF
  IF INT(by!) < HUDH% + 1 THEN
    by! = HUDH% + 1
    vy! = -vy!
    BeepWall
  END IF

  ' ---- Block collision (elastic) ----
  IF CheckBlockCollision(INT(bx!), INT(by!), br%) = 1 THEN
    vy! = -vy!
    score% = score% + 10
    BeepBlock
  END IF

  ' ---- Paddle collision (elastic + small kick) ----
  IF INT(by!) + 2*br% >= INT(py!) AND INT(by!) <= INT(py!) + ph% _
     AND INT(bx!) + 2*br% >= INT(px!) AND INT(bx!) <= INT(px!) + pw% THEN
    by! = INT(py!) - 2*br%
    vy! = -vy!
    ' Add small horizontal kick from paddle velocity
    vx! = vx! + pvx! * PADDLE_KICK
    score% = score% + 1
    BeepPaddle
  ENDIF

  ' ---- Miss ----
  IF INT(by!) > H% - 2*br% THEN
    misses% = misses% + 1
    bx! = W% \ 2
    by! = H% \ 2
    IF RND > 0.5 THEN vx! = BALL_SPEED_INIT ELSE vx! = -BALL_SPEED_INIT
    vy! = BALL_SPEED_INIT
    lastAccelTime% = TIMER
    BeepMiss : BeepServe
  ENDIF

  ' ---- Win condition ----
  IF blocksLeft% = 0 THEN
    FRAMEBUFFER MERGE RGB(BLACK), A  ' Abort continuous merge
    FRAMEBUFFER CLOSE
    FRAMEBUFFER WRITE N
    CLS COL_BG%
    PRINT "YOU WIN!  Score="; score%; "  Misses="; misses%
    END
  END IF

  ' ---- Redraw layer with new sprite positions ----
  FRAMEBUFFER SYNC
  FRAMEBUFFER WRITE L
  CLS RGB(BLACK)    ' Clear layer with transparent color
  DrawPaddleAt INT(px!), INT(py!)
  DrawBallAt INT(bx!), INT(by!)

  ' ---- Update FPS display ----
  frames% = frames% + 1
  IF TIMER - t0% >= 1000 THEN
    fps$ = STR$(frames%) + " FPS"
    frames% = 0
    t0% = TIMER
    FRAMEBUFFER WRITE F
    DrawHUD
  END IF

  ' No PAUSE needed - continuous merge handles timing
LOOP

' ---- Cleanup ----
FRAMEBUFFER MERGE RGB(BLACK), A  ' Abort continuous merge
FRAMEBUFFER CLOSE
FRAMEBUFFER WRITE N  ' Back to normal screen
CLS COL_BG%
PRINT "Thanks!  Score="; score%; "  Misses="; misses%
END
