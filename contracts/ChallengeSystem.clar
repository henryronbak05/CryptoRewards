(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-CHALLENGE-NOT-FOUND (err u201))
(define-constant ERR-CHALLENGE-EXPIRED (err u202))
(define-constant ERR-ALREADY-PARTICIPATING (err u203))
(define-constant ERR-NOT-PARTICIPATING (err u204))
(define-constant ERR-CHALLENGE-NOT-COMPLETED (err u205))
(define-constant ERR-INVALID-CHALLENGE (err u206))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u207))

(define-data-var challenge-counter uint u0)

(define-map challenges uint
  {
    merchant: principal,
    title: (string-ascii 100),
    description: (string-ascii 200),
    challenge-type: (string-ascii 20),
    target-value: uint,
    reward-points: uint,
    start-block: uint,
    end-block: uint,
    max-participants: uint,
    current-participants: uint,
    is-active: bool
  }
)

(define-map challenge-participants {challenge-id: uint, customer: principal}
  {
    current-progress: uint,
    joined-block: uint,
    is-completed: bool,
    reward-claimed: bool
  }
)

(define-map customer-challenge-list principal (list 50 uint))

(define-map merchant-challenges principal (list 20 uint))

(define-public (create-challenge 
  (title (string-ascii 100))
  (description (string-ascii 200))
  (challenge-type (string-ascii 20))
  (target-value uint)
  (reward-points uint)
  (duration-blocks uint)
  (max-participants uint))
  (let (
    (challenge-id (var-get challenge-counter))
    (merchant-challenge-list (default-to (list) (map-get? merchant-challenges tx-sender)))
  )
    (asserts! (> target-value u0) ERR-INVALID-CHALLENGE)
    (asserts! (> reward-points u0) ERR-INVALID-CHALLENGE)
    (asserts! (> duration-blocks u0) ERR-INVALID-CHALLENGE)
    (asserts! (> max-participants u0) ERR-INVALID-CHALLENGE)
    
    (map-set challenges challenge-id
      {
        merchant: tx-sender,
        title: title,
        description: description,
        challenge-type: challenge-type,
        target-value: target-value,
        reward-points: reward-points,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration-blocks),
        max-participants: max-participants,
        current-participants: u0,
        is-active: true
      }
    )
    
    (map-set merchant-challenges tx-sender 
      (unwrap! (as-max-len? (append merchant-challenge-list challenge-id) u20) ERR-INVALID-CHALLENGE))
    
    (var-set challenge-counter (+ challenge-id u1))
    (ok challenge-id)
  )
)

(define-public (join-challenge (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    (participation-key {challenge-id: challenge-id, customer: tx-sender})
    (existing-participation (map-get? challenge-participants participation-key))
    (customer-challenges (default-to (list) (map-get? customer-challenge-list tx-sender)))
  )
    (asserts! (is-none existing-participation) ERR-ALREADY-PARTICIPATING)
    (asserts! (get is-active challenge) ERR-CHALLENGE-EXPIRED)
    (asserts! (< stacks-block-height (get end-block challenge)) ERR-CHALLENGE-EXPIRED)
    (asserts! (< (get current-participants challenge) (get max-participants challenge)) ERR-INVALID-CHALLENGE)
    
    (map-set challenge-participants participation-key
      {
        current-progress: u0,
        joined-block: stacks-block-height,
        is-completed: false,
        reward-claimed: false
      }
    )
    
    (map-set challenges challenge-id
      (merge challenge {current-participants: (+ (get current-participants challenge) u1)})
    )
    
    (map-set customer-challenge-list tx-sender
      (unwrap! (as-max-len? (append customer-challenges challenge-id) u50) ERR-INVALID-CHALLENGE))
    
    (ok true)
  )
)

(define-public (update-challenge-progress (customer principal) (challenge-id uint) (progress-increment uint))
  (let (
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    (participation-key {challenge-id: challenge-id, customer: customer})
    (participation (unwrap! (map-get? challenge-participants participation-key) ERR-NOT-PARTICIPATING))
  )
    (asserts! (is-eq tx-sender (get merchant challenge)) ERR-NOT-AUTHORIZED)
    (asserts! (< stacks-block-height (get end-block challenge)) ERR-CHALLENGE-EXPIRED)
    (asserts! (not (get is-completed participation)) ERR-CHALLENGE-NOT-COMPLETED)
    
    (let (
      (new-progress (+ (get current-progress participation) progress-increment))
      (is-now-completed (>= new-progress (get target-value challenge)))
    )
      (map-set challenge-participants participation-key
        (merge participation 
          {
            current-progress: new-progress,
            is-completed: is-now-completed
          }
        )
      )
      (ok is-now-completed)
    )
  )
)

(define-public (claim-challenge-reward (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    (participation-key {challenge-id: challenge-id, customer: tx-sender})
    (participation (unwrap! (map-get? challenge-participants participation-key) ERR-NOT-PARTICIPATING))
  )
    (asserts! (get is-completed participation) ERR-CHALLENGE-NOT-COMPLETED)
    (asserts! (not (get reward-claimed participation)) ERR-REWARD-ALREADY-CLAIMED)
    
    (map-set challenge-participants participation-key
      (merge participation {reward-claimed: true})
    )
    
    (ok (get reward-points challenge))
  )
)

(define-public (deactivate-challenge (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get merchant challenge)) ERR-NOT-AUTHORIZED)
    (map-set challenges challenge-id
      (merge challenge {is-active: false})
    )
    (ok true)
  )
)

(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges challenge-id)
)

(define-read-only (get-challenge-participation (challenge-id uint) (customer principal))
  (map-get? challenge-participants {challenge-id: challenge-id, customer: customer})
)

(define-read-only (get-customer-challenges (customer principal))
  (map-get? customer-challenge-list customer)
)

(define-read-only (get-merchant-challenges (merchant principal))
  (map-get? merchant-challenges merchant)
)



(define-read-only (get-challenge-leaderboard (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
  )
    (ok challenge-id)
  )
)

(define-read-only (is-challenge-completed (challenge-id uint) (customer principal))
  (match (map-get? challenge-participants {challenge-id: challenge-id, customer: customer})
    participation (get is-completed participation)
    false
  )
)

(define-read-only (get-challenge-progress (challenge-id uint) (customer principal))
  (match (map-get? challenge-participants {challenge-id: challenge-id, customer: customer})
    participation (some (get current-progress participation))
    none
  )
)