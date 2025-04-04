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


(define-data-var points-expiry-blocks uint u10000)

(define-map point-expiry-data principal 
  {
    points: uint,
    expiry-block: uint
  }
)

(define-public (set-points-expiry (blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set points-expiry-blocks blocks)
    (ok true)
  )
)

(define-public (expire-points (customer principal))
  (let (
    (expiry-data (default-to {points: u0, expiry-block: u0} 
                  (map-get? point-expiry-data customer)))
    (current-block stacks-block-height)
  )
    (if (>= current-block (get expiry-block expiry-data))
      (begin
        (map-delete point-expiry-data customer)
        (map-set customer-points customer u0)
        (ok true)
      )
      (ok false)
    )
  )
)


(define-map membership-tiers principal 
  {
    tier: (string-ascii 20),
    multiplier: uint,
    min-points: uint
  }
)

(define-public (create-tier (tier-name (string-ascii 20)) (tier-multiplier uint) (minimum-points uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set membership-tiers tx-sender
      {
        tier: tier-name,
        multiplier: tier-multiplier,
        min-points: minimum-points
      }
    )
    (ok true)
  )
)

(define-read-only (get-tier-by-points (points uint))
  (match (map-get? membership-tiers tx-sender)
    tier-data (if (>= points (get min-points tier-data))
      (some tier-data)
      none
    )
    none
  )
)

(define-read-only (get-customer-tier (customer principal))
  (let (
    (points (get-point-balance customer))
  )
    (get-tier-by-points points)
  )
)



(define-constant ERR-SELF-TRANSFER (err u105))

(define-public (transfer-points (recipient principal) (amount uint))
  (let (
    (sender-balance (default-to u0 (map-get? customer-points tx-sender)))
  )
    (asserts! (not (is-eq tx-sender recipient)) ERR-SELF-TRANSFER)
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-POINTS)
    
    (map-set customer-points tx-sender (- sender-balance amount))
    (map-set customer-points recipient 
      (+ (default-to u0 (map-get? customer-points recipient)) amount)
    )
    (ok true)
  )
)

(define-map promotions uint 
  {
    name: (string-ascii 50),
    bonus-multiplier: uint,
    start-block: uint,
    end-block: uint,
    is-active: bool
  }
)

(define-data-var promotion-counter uint u0)

(define-public (create-promotion (promo-name (string-ascii 50)) (multiplier uint) (duration uint))
  (let (
    (promo-id (var-get promotion-counter))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set promotions promo-id
      {
        name: promo-name,
        bonus-multiplier: multiplier,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        is-active: true
      }
    )
    (var-set promotion-counter (+ promo-id u1))
    (ok promo-id)
  )
)

(define-map merchant-ratings principal 
  {
    total-rating: uint,
    rating-count: uint,
    average-rating: uint
  }
)

(define-constant ERR-INVALID-RATING (err u106))

(define-public (rate-merchant (merchant principal) (rating uint))
  (let (
    (current-ratings (default-to {total-rating: u0, rating-count: u0, average-rating: u0} 
                      (map-get? merchant-ratings merchant)))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (map-set merchant-ratings merchant
      {
        total-rating: (+ (get total-rating current-ratings) rating),
        rating-count: (+ (get rating-count current-ratings) u1),
        average-rating: (/ (+ (get total-rating current-ratings) rating) 
                          (+ (get rating-count current-ratings) u1))
      }
    )
    (ok true)
  )
)


(define-map gift-points principal 
  {
    sender: principal,
    amount: uint,
    message: (string-ascii 100),
    block-height: uint
  }
)

(define-public (send-gift-points (recipient principal) (amount uint) (message (string-ascii 100)))
  (let (
    (sender-balance (default-to u0 (map-get? customer-points tx-sender)))
  )
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-POINTS)
    (map-set customer-points tx-sender (- sender-balance amount))
    (map-set customer-points recipient 
      (+ (default-to u0 (map-get? customer-points recipient)) amount)
    )
    (map-set gift-points recipient
      {
        sender: tx-sender,
        amount: amount,
        message: message,
        block-height: stacks-block-height
      }
    )
    (ok true)
  )
)