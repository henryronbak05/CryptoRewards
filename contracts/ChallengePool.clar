(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-POOL-NOT-FOUND (err u301))
(define-constant ERR-INSUFFICIENT-BALANCE (err u302))
(define-constant ERR-INVALID-AMOUNT (err u303))
(define-constant ERR-POOL-CLOSED (err u304))
(define-constant ERR-ALREADY-CONTRIBUTED (err u305))
(define-constant ERR-CHALLENGE-ACTIVE (err u306))

(define-data-var pool-counter uint u0)

(define-map challenge-pools uint
  {
    challenge-id: uint,
    total-pool: uint,
    contributor-count: uint,
    is-active: bool,
    created-block: uint,
    target-amount: uint
  }
)

(define-map pool-contributions {pool-id: uint, contributor: principal}
  {
    amount: uint,
    percentage: uint,
    contributed-block: uint
  }
)

(define-map contributor-pools principal (list 20 uint))

(define-public (create-challenge-pool (challenge-id uint) (target-amount uint))
  (let (
    (pool-id (var-get pool-counter))
  )
    (asserts! (> target-amount u0) ERR-INVALID-AMOUNT)
    
    (map-set challenge-pools pool-id
      {
        challenge-id: challenge-id,
        total-pool: u0,
        contributor-count: u0,
        is-active: true,
        created-block: stacks-block-height,
        target-amount: target-amount
      }
    )
    
    (var-set pool-counter (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (contribute-to-pool (pool-id uint) (amount uint))
  (let (
    (pool (unwrap! (map-get? challenge-pools pool-id) ERR-POOL-NOT-FOUND))
    (contribution-key {pool-id: pool-id, contributor: tx-sender})
    (existing-contribution (map-get? pool-contributions contribution-key))
    (contributor-pools-list (default-to (list) (map-get? contributor-pools tx-sender)))
    (new-total (+ (get total-pool pool) amount))
    (new-percentage (/ (* amount u10000) new-total))
  )
    (asserts! (get is-active pool) ERR-POOL-CLOSED)
    (asserts! (is-none existing-contribution) ERR-ALREADY-CONTRIBUTED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (map-set pool-contributions contribution-key
      {
        amount: amount,
        percentage: new-percentage,
        contributed-block: stacks-block-height
      }
    )
    
    (map-set challenge-pools pool-id
      (merge pool 
        {
          total-pool: new-total,
          contributor-count: (+ (get contributor-count pool) u1)
        }
      )
    )
    
    (map-set contributor-pools tx-sender
      (unwrap! (as-max-len? (append contributor-pools-list pool-id) u20) ERR-INVALID-AMOUNT))
    
    (ok true)
  )
)

(define-public (close-pool (pool-id uint))
  (let (
    (pool (unwrap! (map-get? challenge-pools pool-id) ERR-POOL-NOT-FOUND))
  )
    (asserts! (get is-active pool) ERR-POOL-CLOSED)
    (asserts! (>= (get total-pool pool) (get target-amount pool)) ERR-INSUFFICIENT-BALANCE)
    
    (map-set challenge-pools pool-id
      (merge pool {is-active: false})
    )
    (ok true)
  )
)

(define-public (withdraw-contribution (pool-id uint))
  (let (
    (pool (unwrap! (map-get? challenge-pools pool-id) ERR-POOL-NOT-FOUND))
    (contribution-key {pool-id: pool-id, contributor: tx-sender})
    (contribution (unwrap! (map-get? pool-contributions contribution-key) ERR-NOT-AUTHORIZED))
  )
    (asserts! (get is-active pool) ERR-CHALLENGE-ACTIVE)
    
    (map-delete pool-contributions contribution-key)
    (map-set challenge-pools pool-id
      (merge pool 
        {
          total-pool: (- (get total-pool pool) (get amount contribution)),
          contributor-count: (- (get contributor-count pool) u1)
        }
      )
    )
    
    (ok (get amount contribution))
  )
)

(define-public (distribute-pool-rewards (pool-id uint) (customers (list 50 principal)) (reward-amounts (list 50 uint)))
  (let (
    (pool (unwrap! (map-get? challenge-pools pool-id) ERR-POOL-NOT-FOUND))
    (total-rewards (fold + reward-amounts u0))
  )
    (asserts! (not (get is-active pool)) ERR-POOL-CLOSED)
    (asserts! (<= total-rewards (get total-pool pool)) ERR-INSUFFICIENT-BALANCE)
    
    (map-set challenge-pools pool-id
      (merge pool {total-pool: (- (get total-pool pool) total-rewards)})
    )
    
    (ok true)
  )
)

(define-read-only (get-pool-details (pool-id uint))
  (map-get? challenge-pools pool-id)
)

(define-read-only (get-contribution-details (pool-id uint) (contributor principal))
  (map-get? pool-contributions {pool-id: pool-id, contributor: contributor})
)

(define-read-only (get-contributor-pools (contributor principal))
  (map-get? contributor-pools contributor)
)

(define-read-only (get-pool-funding-progress (pool-id uint))
  (match (map-get? challenge-pools pool-id)
    pool 
      {
        current-amount: (get total-pool pool),
        target-amount: (get target-amount pool),
        progress-percentage: (/ (* (get total-pool pool) u10000) (get target-amount pool)),
        is-funded: (>= (get total-pool pool) (get target-amount pool))
      }
    {current-amount: u0, target-amount: u0, progress-percentage: u0, is-funded: false}
  )
)

(define-read-only (calculate-contributor-share (pool-id uint) (contributor principal) (total-reward uint))
  (match (map-get? pool-contributions {pool-id: pool-id, contributor: contributor})
    contribution (/ (* total-reward (get percentage contribution)) u10000)
    u0
  )
)
