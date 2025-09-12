;; Merchant Analytics Dashboard
;; Provides business intelligence and automated reporting for CryptoRewards merchants
;; Tracks KPIs, customer segments, and performance metrics in real-time

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-INVALID-PERIOD (err u401))
(define-constant ERR-NO-DATA (err u402))
(define-constant ERR-ALREADY-EXISTS (err u403))

;; Data Variables
(define-data-var total-merchants-tracked uint u0)
(define-data-var analytics-fee uint u100) ;; 1 STX fee for premium analytics

;; Merchant business metrics tracking
(define-map merchant-analytics principal {
    total-customers-served: uint,
    total-points-distributed: uint,
    total-revenue-tracked: uint,
    average-transaction-size: uint,
    customer-retention-rate: uint,
    peak-transaction-hour: uint,
    last-updated: uint
})

;; Daily performance snapshots
(define-map daily-metrics {merchant: principal, day: uint} {
    transactions-count: uint,
    new-customers: uint,
    points-given: uint,
    revenue-amount: uint,
    average-basket-size: uint
})

;; Customer segmentation data
(define-map customer-segments {merchant: principal, segment: (string-ascii 20)} {
    customer-count: uint,
    total-spend: uint,
    avg-points-balance: uint,
    last-activity: uint
})

;; Event tracking for real-time notifications
(define-map event-subscriptions {merchant: principal, event-type: (string-ascii 30)} {
    webhook-url: (string-ascii 200),
    is-active: bool,
    created-at: uint
})

;; Premium merchant features
(define-map premium-merchants principal {
    subscription-end: uint,
    features-enabled: (list 10 (string-ascii 30)),
    monthly-fee: uint
})

;; Public Functions

;; Initialize merchant analytics tracking
(define-public (setup-merchant-analytics)
    (let ((current-analytics (map-get? merchant-analytics tx-sender)))
        (asserts! (is-none current-analytics) ERR-ALREADY-EXISTS)
        (map-set merchant-analytics tx-sender {
            total-customers-served: u0,
            total-points-distributed: u0,
            total-revenue-tracked: u0,
            average-transaction-size: u0,
            customer-retention-rate: u0,
            peak-transaction-hour: u12,
            last-updated: stacks-block-height
        })
        (var-set total-merchants-tracked (+ (var-get total-merchants-tracked) u1))
        (ok true)
    )
)

;; Record transaction for analytics (called by main CryptoRewards contract)
(define-public (record-transaction (customer principal) (points-awarded uint) (revenue-amount uint))
    (let (
        (analytics (unwrap! (map-get? merchant-analytics tx-sender) ERR-NO-DATA))
        (today (/ stacks-block-height u144)) ;; Approximate days
        (daily-key {merchant: tx-sender, day: today})
        (current-daily (default-to {transactions-count: u0, new-customers: u0, points-given: u0, revenue-amount: u0, average-basket-size: u0}
                       (map-get? daily-metrics daily-key)))
    )
        ;; Update main analytics
        (map-set merchant-analytics tx-sender {
            total-customers-served: (+ (get total-customers-served analytics) u1),
            total-points-distributed: (+ (get total-points-distributed analytics) points-awarded),
            total-revenue-tracked: (+ (get total-revenue-tracked analytics) revenue-amount),
            average-transaction-size: (/ (+ (get total-revenue-tracked analytics) revenue-amount) 
                                        (+ (get total-customers-served analytics) u1)),
            customer-retention-rate: (get customer-retention-rate analytics),
            peak-transaction-hour: (get peak-transaction-hour analytics),
            last-updated: stacks-block-height
        })
        
        ;; Update daily metrics
        (map-set daily-metrics daily-key {
            transactions-count: (+ (get transactions-count current-daily) u1),
            new-customers: (get new-customers current-daily),
            points-given: (+ (get points-given current-daily) points-awarded),
            revenue-amount: (+ (get revenue-amount current-daily) revenue-amount),
            average-basket-size: (/ (+ (get revenue-amount current-daily) revenue-amount)
                                  (+ (get transactions-count current-daily) u1))
        })
        
        ;; Emit analytics event
        (print {
            event-type: "transaction-recorded",
            merchant: tx-sender,
            customer: customer,
            points: points-awarded,
            revenue: revenue-amount,
            timestamp: stacks-block-height
        })
        (ok true)
    )
)

;; Subscribe to webhook events
(define-public (subscribe-to-events (event-type (string-ascii 30)) (webhook-url (string-ascii 200)))
    (let ((subscription-key {merchant: tx-sender, event-type: event-type}))
        (map-set event-subscriptions subscription-key {
            webhook-url: webhook-url,
            is-active: true,
            created-at: stacks-block-height
        })
        (ok true)
    )
)

;; Update customer segment automatically
(define-public (update-customer-segment (customer principal) (segment (string-ascii 20)) (spend-amount uint))
    (let (
        (segment-key {merchant: tx-sender, segment: segment})
        (current-segment (default-to {customer-count: u0, total-spend: u0, avg-points-balance: u0, last-activity: u0}
                         (map-get? customer-segments segment-key)))
    )
        (map-set customer-segments segment-key {
            customer-count: (+ (get customer-count current-segment) u1),
            total-spend: (+ (get total-spend current-segment) spend-amount),
            avg-points-balance: (get avg-points-balance current-segment),
            last-activity: stacks-block-height
        })
        (ok true)
    )
)

;; Upgrade to premium analytics features
(define-public (upgrade-to-premium (duration-blocks uint))
    (let ((premium-fee (* (var-get analytics-fee) (/ duration-blocks u1008))))
        (try! (stx-transfer? premium-fee tx-sender CONTRACT-OWNER))
        (map-set premium-merchants tx-sender {
            subscription-end: (+ stacks-block-height duration-blocks),
            features-enabled: (list "real-time-alerts" "advanced-segmentation" "custom-reports"),
            monthly-fee: premium-fee
        })
        (ok true)
    )
)

;; Generate automated insights
(define-public (generate-insights)
    (let (
        (analytics (unwrap! (map-get? merchant-analytics tx-sender) ERR-NO-DATA))
        (is-premium (is-some (map-get? premium-merchants tx-sender)))
    )
        (if is-premium
            (let (
                (retention-trend (if (> (get customer-retention-rate analytics) u75) "high" "medium"))
                (growth-rate (/ (get total-customers-served analytics) u30))
            )
                (print {
                    event-type: "merchant-insights",
                    merchant: tx-sender,
                    insights: {
                        retention-trend: retention-trend,
                        growth-rate: growth-rate,
                        recommendation: "Focus on customer loyalty programs",
                        next-action: "Create targeted promotions for high-value segments"
                    },
                    generated-at: stacks-block-height
                })
                (ok true)
            )
            (ok false)
        )
    )
)

;; Read-only functions

(define-read-only (get-merchant-analytics (merchant principal))
    (map-get? merchant-analytics merchant)
)

(define-read-only (get-daily-metrics (merchant principal) (day uint))
    (map-get? daily-metrics {merchant: merchant, day: day})
)

(define-read-only (get-customer-segment-data (merchant principal) (segment (string-ascii 20)))
    (map-get? customer-segments {merchant: merchant, segment: segment})
)

(define-read-only (get-event-subscription (merchant principal) (event-type (string-ascii 30)))
    (map-get? event-subscriptions {merchant: merchant, event-type: event-type})
)

(define-read-only (is-premium-merchant (merchant principal))
    (match (map-get? premium-merchants merchant)
        premium-data (> (get subscription-end premium-data) stacks-block-height)
        false
    )
)

(define-read-only (get-dashboard-summary (merchant principal))
    (let ((analytics (map-get? merchant-analytics merchant)))
        (match analytics
            data (some {
                total-customers: (get total-customers-served data),
                total-points: (get total-points-distributed data),
                avg-transaction: (get average-transaction-size data),
                last-updated: (get last-updated data),
                is-premium: (is-premium-merchant merchant)
            })
            none
        )
    )
)

;; Administrative functions
(define-public (update-analytics-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set analytics-fee new-fee)
        (ok true)
    )
)
