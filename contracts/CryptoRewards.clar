;; CryptoRewards - Customer Loyalty Platform
;; Handles point tracking, reward distribution and merchant management

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-MERCHANT-EXISTS (err u101))
(define-constant ERR-MERCHANT-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-POINTS (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant POINTS-MULTIPLIER u100) ;; 1 STX = 100 points

;; Data Variables
(define-data-var total-points-issued uint u0)
(define-data-var total-merchants uint u0)

;; Data Maps
(define-map merchants principal 
  {
    name: (string-ascii 50),
    points-multiplier: uint,
    is-active: bool,
    total-rewards-given: uint
  }
)

(define-map customer-points principal uint)

(define-map customer-transactions principal 
  {
    points-earned: uint,
    points-spent: uint,
    last-transaction: uint
  }
)

;; Public Functions

;; Register new merchant
(define-public (register-merchant (merchant-name (string-ascii 50)) (points-mult uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (get-merchant-data tx-sender)) ERR-MERCHANT-EXISTS)
    (map-set merchants tx-sender
      {
        name: merchant-name,
        points-multiplier: points-mult,
        is-active: true,
        total-rewards-given: u0
      }
    )
    (var-set total-merchants (+ (var-get total-merchants) u1))
    (ok true)
  )
)

;; Earn points when customer makes purchase
(define-public (earn-points (customer principal) (purchase-amount uint))
  (let (
    (merchant-data (unwrap! (get-merchant-data tx-sender) ERR-MERCHANT-NOT-FOUND))
    (points-to-award (* purchase-amount (get points-multiplier merchant-data)))
    (current-points (default-to u0 (map-get? customer-points customer)))
    (customer-tx (default-to {points-earned: u0, points-spent: u0, last-transaction: u0} 
                  (map-get? customer-transactions customer)))
  )
    (asserts! (get is-active merchant-data) ERR-NOT-AUTHORIZED)
    (map-set customer-points customer (+ current-points points-to-award))
    (map-set customer-transactions customer
      {
        points-earned: (+ (get points-earned customer-tx) points-to-award),
        points-spent: (get points-spent customer-tx),
        last-transaction: stacks-block-height
      }
    )
    (var-set total-points-issued (+ (var-get total-points-issued) points-to-award))
    (ok points-to-award)
  )
)

;; Redeem points for rewards
(define-public (redeem-points (amount uint))
  (let (
    (current-points (default-to u0 (map-get? customer-points tx-sender)))
    (customer-tx (default-to {points-earned: u0, points-spent: u0, last-transaction: u0} 
                  (map-get? customer-transactions tx-sender)))
  )
    (asserts! (>= current-points amount) ERR-INSUFFICIENT-POINTS)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (map-set customer-points tx-sender (- current-points amount))
    (map-set customer-transactions tx-sender
      {
        points-earned: (get points-earned customer-tx),
        points-spent: (+ (get points-spent customer-tx) amount),
        last-transaction: stacks-block-height
      }
    )
    (ok amount)
  )
)

;; Read Only Functions

;; Get merchant data
(define-read-only (get-merchant-data (merchant principal))
  (map-get? merchants merchant)
)

;; Get customer point balance
(define-read-only (get-point-balance (customer principal))
  (default-to u0 (map-get? customer-points customer))
)

;; Get customer transaction history
(define-read-only (get-customer-transactions (customer principal))
  (map-get? customer-transactions customer)
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-points: (var-get total-points-issued),
    total-merchants: (var-get total-merchants)
  }
)

;; Administrative Functions

;; Update merchant status
(define-public (update-merchant-status (merchant principal) (is-active bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (map-get? merchants merchant)
      merchant-data (ok (map-set merchants merchant 
        (merge merchant-data {is-active: is-active})))
      ERR-MERCHANT-NOT-FOUND
    )
  )
)

;; Update points multiplier
(define-public (update-points-multiplier (merchant principal) (new-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (map-get? merchants merchant)
      merchant-data (ok (map-set merchants merchant 
        (merge merchant-data {points-multiplier: new-multiplier})))
      ERR-MERCHANT-NOT-FOUND
    )
  )
)
