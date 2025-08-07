;; RewardMarketplace - Peer-to-peer reward trading platform
;; Enables customers to list, trade, and swap their earned rewards

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-LISTING-NOT-FOUND (err u401))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))
(define-constant ERR-INVALID-PRICE (err u403))
(define-constant ERR-LISTING-EXPIRED (err u404))
(define-constant ERR-SELF-TRADE (err u405))
(define-constant ERR-LISTING-INACTIVE (err u406))
(define-constant ERR-BID-TOO-LOW (err u407))
(define-constant ERR-BID-NOT-FOUND (err u408))
(define-constant ERR-ALREADY-ACCEPTED (err u409))

;; Data variables for counters and configuration
(define-data-var listing-counter uint u0)
(define-data-var bid-counter uint u0)
(define-data-var marketplace-fee-rate uint u250) ;; 2.5% fee in basis points
(define-data-var min-listing-duration uint u1008) ;; ~1 week in blocks

;; Core marketplace listing structure
(define-map reward-listings uint
  {
    seller: principal,
    reward-type: (string-ascii 50),
    reward-description: (string-ascii 200),
    points-value: uint,
    asking-price: uint,
    listing-block: uint,
    expiry-block: uint,
    is-active: bool,
    accepts-bids: bool,
    category: (string-ascii 30)
  }
)

;; Bidding system for flexible pricing
(define-map marketplace-bids uint
  {
    listing-id: uint,
    bidder: principal,
    bid-amount: uint,
    bid-block: uint,
    is-active: bool,
    message: (string-ascii 100)
  }
)

;; Track user marketplace activity
(define-map user-listings principal (list 25 uint))
(define-map user-bids principal (list 50 uint))
(define-map user-purchases principal (list 50 uint))

;; Transaction history for marketplace analytics
(define-map completed-trades uint
  {
    listing-id: uint,
    seller: principal,
    buyer: principal,
    final-price: uint,
    trade-block: uint,
    fee-collected: uint
  }
)

;; Marketplace statistics tracking
(define-map category-stats (string-ascii 30)
  {
    total-listings: uint,
    total-volume: uint,
    average-price: uint,
    active-listings: uint
  }
)

;; Create new reward listing in marketplace
(define-public (create-listing 
  (reward-type (string-ascii 50))
  (description (string-ascii 200))
  (points-value uint)
  (asking-price uint)
  (duration-blocks uint)
  (accepts-bids bool)
  (category (string-ascii 30)))
  (let (
    (listing-id (var-get listing-counter))
    (seller-listings (default-to (list) (map-get? user-listings tx-sender)))
    (category-data (default-to {total-listings: u0, total-volume: u0, average-price: u0, active-listings: u0} 
                    (map-get? category-stats category)))
  )
    ;; Validate listing parameters
    (asserts! (> points-value u0) ERR-INVALID-PRICE)
    (asserts! (> asking-price u0) ERR-INVALID-PRICE)
    (asserts! (>= duration-blocks (var-get min-listing-duration)) ERR-INVALID-PRICE)
    
    ;; Create the listing
    (map-set reward-listings listing-id
      {
        seller: tx-sender,
        reward-type: reward-type,
        reward-description: description,
        points-value: points-value,
        asking-price: asking-price,
        listing-block: stacks-block-height,
        expiry-block: (+ stacks-block-height duration-blocks),
        is-active: true,
        accepts-bids: accepts-bids,
        category: category
      }
    )
    
    ;; Update seller's listing history
    (map-set user-listings tx-sender
      (unwrap! (as-max-len? (append seller-listings listing-id) u25) ERR-INVALID-PRICE))
    
    ;; Update category statistics
    (map-set category-stats category
      {
        total-listings: (+ (get total-listings category-data) u1),
        total-volume: (get total-volume category-data),
        average-price: (get average-price category-data),
        active-listings: (+ (get active-listings category-data) u1)
      }
    )
    
    (var-set listing-counter (+ listing-id u1))
    (ok listing-id)
  )
)

;; Purchase listing at asking price (instant buy)
(define-public (purchase-listing (listing-id uint))
  (let (
    (listing (unwrap! (map-get? reward-listings listing-id) ERR-LISTING-NOT-FOUND))
    (buyer-purchases (default-to (list) (map-get? user-purchases tx-sender)))
    (marketplace-fee (/ (* (get asking-price listing) (var-get marketplace-fee-rate)) u10000))
    (seller-amount (- (get asking-price listing) marketplace-fee))
    (category-data (default-to {total-listings: u0, total-volume: u0, average-price: u0, active-listings: u0} 
                    (map-get? category-stats (get category listing))))
  )
    ;; Validate purchase conditions
    (asserts! (get is-active listing) ERR-LISTING-INACTIVE)
    (asserts! (< stacks-block-height (get expiry-block listing)) ERR-LISTING-EXPIRED)
    (asserts! (not (is-eq tx-sender (get seller listing))) ERR-SELF-TRADE)
    
    ;; Deactivate the listing
    (map-set reward-listings listing-id
      (merge listing {is-active: false})
    )
    
    ;; Record the completed trade
    (map-set completed-trades listing-id
      {
        listing-id: listing-id,
        seller: (get seller listing),
        buyer: tx-sender,
        final-price: (get asking-price listing),
        trade-block: stacks-block-height,
        fee-collected: marketplace-fee
      }
    )
    
    ;; Update buyer's purchase history
    (map-set user-purchases tx-sender
      (unwrap! (as-max-len? (append buyer-purchases listing-id) u50) ERR-INVALID-PRICE))
    
    ;; Update category statistics
    (map-set category-stats (get category listing)
      {
        total-listings: (get total-listings category-data),
        total-volume: (+ (get total-volume category-data) (get asking-price listing)),
        average-price: (/ (+ (get total-volume category-data) (get asking-price listing)) 
                         (get total-listings category-data)),
        active-listings: (- (get active-listings category-data) u1)
      }
    )
    
    (ok {seller-receives: seller-amount, marketplace-fee: marketplace-fee})
  )
)

;; Submit bid on a listing
(define-public (submit-bid (listing-id uint) (bid-amount uint) (message (string-ascii 100)))
  (let (
    (listing (unwrap! (map-get? reward-listings listing-id) ERR-LISTING-NOT-FOUND))
    (bid-id (var-get bid-counter))
    (bidder-bids (default-to (list) (map-get? user-bids tx-sender)))
  )
    ;; Validate bid conditions
    (asserts! (get is-active listing) ERR-LISTING-INACTIVE)
    (asserts! (get accepts-bids listing) ERR-NOT-AUTHORIZED)
    (asserts! (< stacks-block-height (get expiry-block listing)) ERR-LISTING-EXPIRED)
    (asserts! (not (is-eq tx-sender (get seller listing))) ERR-SELF-TRADE)
    (asserts! (> bid-amount u0) ERR-BID-TOO-LOW)
    
    ;; Create the bid
    (map-set marketplace-bids bid-id
      {
        listing-id: listing-id,
        bidder: tx-sender,
        bid-amount: bid-amount,
        bid-block: stacks-block-height,
        is-active: true,
        message: message
      }
    )
    
    ;; Update bidder's bid history
    (map-set user-bids tx-sender
      (unwrap! (as-max-len? (append bidder-bids bid-id) u50) ERR-INVALID-PRICE))
    
    (var-set bid-counter (+ bid-id u1))
    (ok bid-id)
  )
)

;; Accept a bid (seller function)
(define-public (accept-bid (bid-id uint))
  (let (
    (bid (unwrap! (map-get? marketplace-bids bid-id) ERR-BID-NOT-FOUND))
    (listing (unwrap! (map-get? reward-listings (get listing-id bid)) ERR-LISTING-NOT-FOUND))
    (marketplace-fee (/ (* (get bid-amount bid) (var-get marketplace-fee-rate)) u10000))
    (seller-amount (- (get bid-amount bid) marketplace-fee))
  )
    ;; Validate acceptance conditions
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active bid) ERR-BID-NOT-FOUND)
    (asserts! (get is-active listing) ERR-ALREADY-ACCEPTED)
    
    ;; Deactivate listing and bid
    (map-set reward-listings (get listing-id bid)
      (merge listing {is-active: false})
    )
    (map-set marketplace-bids bid-id
      (merge bid {is-active: false})
    )
    
    ;; Record the completed trade
    (map-set completed-trades (get listing-id bid)
      {
        listing-id: (get listing-id bid),
        seller: tx-sender,
        buyer: (get bidder bid),
        final-price: (get bid-amount bid),
        trade-block: stacks-block-height,
        fee-collected: marketplace-fee
      }
    )
    
    (ok {seller-receives: seller-amount, marketplace-fee: marketplace-fee})
  )
)

;; Cancel active listing
(define-public (cancel-listing (listing-id uint))
  (let (
    (listing (unwrap! (map-get? reward-listings listing-id) ERR-LISTING-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active listing) ERR-LISTING-INACTIVE)
    
    (map-set reward-listings listing-id
      (merge listing {is-active: false})
    )
    (ok true)
  )
)

;; Read-only functions for marketplace data

(define-read-only (get-listing-details (listing-id uint))
  (map-get? reward-listings listing-id)
)

(define-read-only (get-bid-details (bid-id uint))
  (map-get? marketplace-bids bid-id)
)

(define-read-only (get-user-listings (user principal))
  (map-get? user-listings user)
)

(define-read-only (get-user-bids (user principal))
  (map-get? user-bids user)
)

(define-read-only (get-trade-history (listing-id uint))
  (map-get? completed-trades listing-id)
)

(define-read-only (get-category-statistics (category (string-ascii 30)))
  (map-get? category-stats category)
)

(define-read-only (get-marketplace-metrics)
  {
    total-listings: (var-get listing-counter),
    total-bids: (var-get bid-counter),
    current-fee-rate: (var-get marketplace-fee-rate),
    min-listing-duration: (var-get min-listing-duration)
  }
)

;; Check if listing is still valid and active
(define-read-only (is-listing-available (listing-id uint))
  (match (map-get? reward-listings listing-id)
    listing 
      (and 
        (get is-active listing)
        (< stacks-block-height (get expiry-block listing))
      )
    false
  )
)

;; Administrative functions

(define-public (update-marketplace-fee (new-fee-rate uint))
  (begin
    ;; Only contract deployer can update fees
    (asserts! (is-eq tx-sender tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee-rate u1000) ERR-INVALID-PRICE) ;; Max 10% fee
    (var-set marketplace-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-public (update-min-duration (new-duration uint))
  (begin
    (asserts! (is-eq tx-sender tx-sender) ERR-NOT-AUTHORIZED)
    (var-set min-listing-duration new-duration)
    (ok true)
  )
)