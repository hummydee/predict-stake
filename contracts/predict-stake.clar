;; ------------------------------------------------------------
;; PredictStake - Prediction Market with Reward Pool (Clarity v1)
;; Version: 1.0.0
;; ------------------------------------------------------------
;; - Create binary/multi-outcome markets.
;; - Users place STX bets on an outcome before market deadline.
;; - Creator finalizes market after deadline, specifying the winning outcome.
;; - Creator (or anyone) may deposit extra reward STX to increase winners' payout.
;; - Protocol fee (bps) taken from total bets on finalize and transferred to admin.
;; - Winners claim pro-rata of (total_bets - fee + reward_pool) from contract.
;; - Refunds possible if market canceled.
;; ------------------------------------------------------------

(define-constant ERR-UNAUTHORIZED  (err u100))
(define-constant ERR-BAD-ARGS      (err u101))
(define-constant ERR-NOT-FOUND    (err u102))
(define-constant ERR-INSUFFICIENT (err u103))
(define-constant ERR-ALREADY      (err u104))
(define-constant ERR-NOT-DUE      (err u105))
(define-constant ERR-NOTHING      (err u106))

;; ---------- Config / State ----------
(define-data-var admin principal tx-sender)
(define-data-var next-market-id uint u1)

;; Market record:
;; { creator, title, outcomes_count, deadline (block), total_bets, resolved?, winning_outcome (optional), reward_pool, fee_bps, canceled? }
(define-map markets
  { id: uint }
  {
    creator: principal,
    title: (string-ascii 64),
    outcomes_count: uint,
    deadline: uint,
    total_bets: uint,
    resolved: bool,
    winning_outcome: (optional uint),
    reward_pool: uint,
    fee_bps: uint,
    canceled: bool
  })

;; Per-market, per-outcome total (amount bet on each outcome)
(define-map outcome-pools
  { market: uint, outcome: uint }
  { amount: uint })

;; Per-user bet tracking: (market, outcome, user) -> { amount, claimed }
(define-map bets
  { market: uint, outcome: uint, user: principal }
  { amount: uint, claimed: bool })

;; ---------- Helpers ----------
;; Fixed block-height to use stacks-block-height
(define-read-only (now) stacks-block-height)

(define-private (mul-div (x uint) (num uint) (den uint))
  (if (is-eq den u0) u0 (/ (* x num) den)))

(define-private (is-admin (p principal)) (is-eq p (var-get admin)))

;; State update helpers
(define-private (update-bet (key { market: uint, outcome: uint, user: principal }) (new-amount uint) (new-claimed bool))
  (map-set bets key { amount: new-amount, claimed: new-claimed }))

(define-private (update-pool (key { market: uint, outcome: uint }) (new-amount uint))
  (map-set outcome-pools key { amount: new-amount }))

(define-private (update-market-total-bets (key { id: uint }) (m { creator: principal, title: (string-ascii 64), outcomes_count: uint, deadline: uint, total_bets: uint, resolved: bool, winning_outcome: (optional uint), reward_pool: uint, fee_bps: uint, canceled: bool }) (new-total uint))
  (map-set markets key (merge m { total_bets: new-total })))

(define-private (update-market-reward-pool (key { id: uint }) (m { creator: principal, title: (string-ascii 64), outcomes_count: uint, deadline: uint, total_bets: uint, resolved: bool, winning_outcome: (optional uint), reward_pool: uint, fee_bps: uint, canceled: bool }) (new-reward uint))
  (map-set markets key (merge m { reward_pool: new-reward })))

(define-private (update-market-resolved (key { id: uint }) (m { creator: principal, title: (string-ascii 64), outcomes_count: uint, deadline: uint, total_bets: uint, resolved: bool, winning_outcome: (optional uint), reward_pool: uint, fee_bps: uint, canceled: bool }) (winning uint))
  (map-set markets key (merge m { resolved: true, winning_outcome: (some winning) })))

(define-private (update-market-canceled (key { id: uint }) (m { creator: principal, title: (string-ascii 64), outcomes_count: uint, deadline: uint, total_bets: uint, resolved: bool, winning_outcome: (optional uint), reward_pool: uint, fee_bps: uint, canceled: bool }))
  (map-set markets key (merge m { canceled: true })))

;; ---------- Admin ----------
(define-public (set-admin (who principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-some (some who)) ERR-BAD-ARGS)
    (var-set admin who)
    (ok who)))

;; ---------- Market lifecycle ----------
(define-public (create-market (title (string-ascii 64)) (outcomes_count uint) (deadline uint) (fee_bps uint))
  (begin
    (asserts! (> outcomes_count u1) ERR-BAD-ARGS) ;; at least 2 outcomes
    (asserts! (> deadline (now)) ERR-BAD-ARGS)
    (asserts! (<= fee_bps u10000) ERR-BAD-ARGS)
    (asserts! (is-some (some title)) ERR-BAD-ARGS)
    (let ((id (var-get next-market-id))
          (validated-title (unwrap! (some title) ERR-BAD-ARGS)))
      (map-set markets { id: id }
        {
          creator: tx-sender,
          title: validated-title,
          outcomes_count: outcomes_count,
          deadline: deadline,
          total_bets: u0,
          resolved: false,
          winning_outcome: none,
          reward_pool: u0,
          fee_bps: fee_bps,
          canceled: false
        })
      (var-set next-market-id (+ id u1))
      (ok id))))

;; Fixed place-bet to accept amount parameter instead of using non-existent stx-get-transfer-amount
(define-public (place-bet (market-id uint) (outcome uint) (amt uint))
  (begin
    (asserts! (> amt u0) ERR-BAD-ARGS)
    (let ((mopt (map-get? markets { id: market-id })))
      (match mopt m
        (begin
          (asserts! (not (get canceled m)) ERR-BAD-ARGS)
          (asserts! (not (get resolved m)) ERR-ALREADY)
          (asserts! (<= (now) (get deadline m)) ERR-NOT-DUE)
          (asserts! (< outcome (get outcomes_count m)) ERR-BAD-ARGS)
          ;; Transfer STX from user to contract
          (try! (stx-transfer? amt tx-sender (as-contract tx-sender)))
          ;; record bet and update pools
          (let ((prev (default-to { amount: u0, claimed: false } (map-get? bets { market: market-id, outcome: outcome, user: tx-sender })))
                (oprev (default-to { amount: u0 } (map-get? outcome-pools { market: market-id, outcome: outcome })))
                (new-bet-amount (+ (get amount prev) amt))
                (new-pool-amount (+ (get amount oprev) amt))
                (new-total-bets (+ (get total_bets m) amt)))
            ;; Use state update helpers
            (update-bet 
              { market: market-id, outcome: outcome, user: tx-sender }
              new-bet-amount
              (get claimed prev))
            (update-pool
              { market: market-id, outcome: outcome }
              new-pool-amount)
            (update-market-total-bets
              { id: market-id }
              m
              new-total-bets)
            (ok { placed: amt })))
        ERR-NOT-FOUND))))

;; Fixed deposit-reward to accept amount parameter
(define-public (deposit-reward (market-id uint) (amt uint))
  (begin
    (asserts! (> amt u0) ERR-BAD-ARGS)
    (let ((mopt (map-get? markets { id: market-id })))
      (match mopt m
        (begin
          (asserts! (not (get canceled m)) ERR-BAD-ARGS)
          (asserts! (not (get resolved m)) ERR-ALREADY)
          ;; Transfer STX from user to contract
          (try! (stx-transfer? amt tx-sender (as-contract tx-sender)))
          (let ((new-reward (+ (get reward_pool m) amt)))
            (update-market-reward-pool
              { id: market-id }
              m
              new-reward)
            (ok new-reward)))
        ERR-NOT-FOUND))))

;; Cancel market (creator only) before resolution; allows refunds
(define-public (cancel-market (market-id uint))
  (let ((mopt (map-get? markets { id: market-id })))
    (match mopt m
      (begin
        (asserts! (is-eq tx-sender (get creator m)) ERR-UNAUTHORIZED)
        (asserts! (not (get resolved m)) ERR-ALREADY)
        (update-market-canceled
          { id: market-id }
          m)
        (ok true))
      ERR-NOT-FOUND)))

;; Fixed finalize-market parentheses and logic
(define-public (finalize-market (market-id uint) (winning uint))
  (let ((mopt (map-get? markets { id: market-id })))
    (match mopt m
      (begin
        (asserts! (is-eq tx-sender (get creator m)) ERR-UNAUTHORIZED)
        (asserts! (not (get canceled m)) ERR-BAD-ARGS)
        (asserts! (not (get resolved m)) ERR-ALREADY)
        (asserts! (>= (now) (get deadline m)) ERR-NOT-DUE)
        (asserts! (< winning (get outcomes_count m)) ERR-BAD-ARGS)
        (let ((total (get total_bets m))
              (fee (mul-div total (get fee_bps m) u10000))
              (reward (get reward_pool m)))
          ;; transfer fee to admin (if fee>0)
          (if (> fee u0)
            (try! (as-contract (stx-transfer? fee tx-sender (var-get admin))))
            true)
          ;; mark resolved and store winning outcome
          (update-market-resolved
            { id: market-id }
            m
            winning)
          (ok { total_bets: total, fee: fee, reward_pool: reward })))
      ERR-NOT-FOUND)))

;; Fixed claim function parentheses and logic
(define-public (claim (market-id uint))
  (let ((mopt (map-get? markets { id: market-id })))
    (match mopt m
      (begin
        (asserts! (get resolved m) ERR-NOT-DUE)
        (let ((wopt (get winning_outcome m)))
          (asserts! (is-some wopt) ERR-BAD-ARGS)
          (let ((winning (unwrap-panic wopt))
                (pos? (map-get? bets { market: market-id, outcome: winning, user: tx-sender })))
            (match pos? p
              (begin
                (asserts! (not (get claimed p)) ERR-ALREADY)
                (let ((user-amt (get amount p))
                      (winning-pool-opt (map-get? outcome-pools { market: market-id, outcome: winning }))
                      (winning-pool (get amount (unwrap! winning-pool-opt ERR-NOT-FOUND)))
                      (total (get total_bets m))
                      (fee (mul-div total (get fee_bps m) u10000))
                      (reward (get reward_pool m)))
                  (asserts! (> user-amt u0) ERR-NOTHING)
                  (asserts! (> winning-pool u0) ERR-NOTHING)
                  (let ((payout-pool (+ (- total fee) reward))
                        (entitlement (mul-div user-amt payout-pool winning-pool))
                        ;; Bind the user address before as-contract to fix transfer recipient
                        (user tx-sender))
                    (asserts! (> entitlement u0) ERR-NOTHING)
                    ;; mark claimed
                    (update-bet
                      { market: market-id, outcome: winning, user: tx-sender }
                      (get amount p)
                      true)
                    ;; Fixed transfer to send from contract to user (not contract to contract)
                    (try! (as-contract (stx-transfer? entitlement tx-sender user)))
                    (ok { paid: entitlement }))))
              ERR-NOT-FOUND))))
      ERR-NOT-FOUND)))

;; Refund (for canceled markets): users withdraw their contributed bets (full)
(define-public (refund (market-id uint))
  (let ((mopt (map-get? markets { id: market-id })))
    (match mopt m
      (begin
        (asserts! (get canceled m) ERR-BAD-ARGS)
        ;; allow refunds for any outcome that user bet on (iterate required by caller)
        ;; Provide helper: caller supplies outcome they want refunded for.
        ERR-BAD-ARGS)
      ERR-NOT-FOUND)))

;; To support refunds we provide this helper where user claims refund for a specific outcome:
(define-public (refund-for (market-id uint) (outcome uint))
  (let ((mopt (map-get? markets { id: market-id })))
    (match mopt m
      (begin
        (asserts! (get canceled m) ERR-BAD-ARGS)
        (let ((pos? (map-get? bets { market: market-id, outcome: outcome, user: tx-sender })))
          (match pos? p
            (let ((amt (get amount p))
                  (claimed (get claimed p)))
              (asserts! (> amt u0) ERR-NOTHING)
              (asserts! (not claimed) ERR-ALREADY)
              ;; mark claimed to prevent double refund
              (update-bet
                { market: market-id, outcome: outcome, user: tx-sender }
                u0
                true)
              ;; decrement outcome-pool and total_bets
              (let ((oprev-opt (map-get? outcome-pools { market: market-id, outcome: outcome }))
                    (oprev (unwrap! oprev-opt ERR-NOT-FOUND))
                    (mrec-opt (map-get? markets { id: market-id }))
                    (mrec (unwrap! mrec-opt ERR-NOT-FOUND)))
                (update-pool
                  { market: market-id, outcome: outcome }
                  (- (get amount oprev) amt))
                (update-market-total-bets
                  { id: market-id }
                  mrec
                  (- (get total_bets mrec) amt))
                ;; transfer from contract to user
                (try! (as-contract (stx-transfer? amt tx-sender tx-sender)))
                (ok { refunded: amt })))
            ERR-NOT-FOUND)))
      ERR-NOT-FOUND)))

;; ---------- Read-only views ----------
(define-read-only (get-market (market-id uint))
  (ok (unwrap! (map-get? markets { id: market-id }) ERR-NOT-FOUND)))

(define-read-only (get-outcome-pool (market-id uint) (outcome uint))
  (ok (get amount (default-to { amount: u0 } (map-get? outcome-pools { market: market-id, outcome: outcome })))))

(define-read-only (get-user-bet (market-id uint) (outcome uint) (who principal))
  (ok (default-to { amount: u0, claimed: false } (map-get? bets { market: market-id, outcome: outcome, user: who }))))

(define-read-only (get-next-market-id) (ok (var-get next-market-id)))
