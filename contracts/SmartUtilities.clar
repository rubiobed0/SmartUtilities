
;; SmartUtilities Contract
;; Utility bill management platform with usage tracking and payments

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-registered (err u104))

;; Data Variables
(define-data-var service-fee uint u1000) ;; in micro-STX

;; Data Maps
(define-map utility-providers 
    principal 
    {
        name: (string-ascii 50),
        active: bool,
        service-type: (string-ascii 20),
        rate-per-unit: uint
    }
)

(define-map consumer-accounts
    principal
    {
        balance: uint,
        active: bool,
        last-payment: uint,
        total-usage: uint
    }
)

(define-map usage-records
    {consumer: principal, period: uint}
    {
        units-consumed: uint,
        amount-due: uint,
        paid: bool,
        provider: principal
    }
)

;; Public Functions

;; Register new utility provider
(define-public (register-provider (name (string-ascii 50)) (service-type (string-ascii 20)) (rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? utility-providers tx-sender)) err-already-registered)
        (ok (map-set utility-providers tx-sender
            {
                name: name,
                active: true,
                service-type: service-type,
                rate-per-unit: rate
            }))
    )
)

;; Register new consumer
(define-public (register-consumer)
    (begin
        (asserts! (is-none (map-get? consumer-accounts tx-sender)) err-already-registered)
        (ok (map-set consumer-accounts tx-sender
            {
                balance: u0,
                active: true,
                last-payment: u0,
                total-usage: u0
            }))
    )
)

;; Record utility usage
(define-public (record-usage (consumer principal) (units uint) (period uint))
    (let (
        (provider (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
        (amount-to-charge (* units (get rate-per-unit provider)))
    )
        (asserts! (get active provider) err-unauthorized)
        (ok (map-set usage-records {consumer: consumer, period: period}
            {
                units-consumed: units,
                amount-due: amount-to-charge,
                paid: false,
                provider: tx-sender
            }))
    )
)

;; Make payment for utility usage
(define-public (make-payment (period uint))
    (let (
        (usage (unwrap! (map-get? usage-records {consumer: tx-sender, period: period}) err-not-found))
        (consumer (unwrap! (map-get? consumer-accounts tx-sender) err-not-found))
        (amount (get amount-due usage))
    )
        (asserts! (not (get paid usage)) err-already-registered)
        (try! (stx-transfer? amount tx-sender (get provider usage)))
        (map-set usage-records {consumer: tx-sender, period: period}
            (merge usage {paid: true}))
        (map-set consumer-accounts tx-sender
            (merge consumer 
                {
                    last-payment: period,
                    total-usage: (+ (get total-usage consumer) (get units-consumed usage))
                }))
        (ok true)
    )
)

;; Read-only functions

;; Get provider details
(define-read-only (get-provider-details (provider principal))
    (map-get? utility-providers provider)
)

;; Get consumer details
(define-read-only (get-consumer-details (consumer principal))
    (map-get? consumer-accounts consumer)
)

;; Get usage details for a period
(define-read-only (get-usage-details (consumer principal) (period uint))
    (map-get? usage-records {consumer: consumer, period: period})
)

;; Check if payment is due
(define-read-only (is-payment-due (consumer principal) (period uint))
    (match (map-get? usage-records {consumer: consumer, period: period})
        usage (not (get paid usage))
        false
    )
)

;; Get total amount due for consumer
(define-read-only (get-total-due (consumer principal))
    (match (map-get? usage-records {consumer: consumer, period: u0})
        usage (get amount-due usage)
        u0)
)

;; Private functions

;; Helper to get amount due from usage record
(define-private (get-amount-due (usage {units-consumed: uint, amount-due: uint, paid: bool, provider: principal}))
    (if (get paid usage)
        u0
        (get amount-due usage)
    )
)



(define-map provider-ratings
    principal 
    {
        total-ratings: uint,
        rating-sum: uint,
        average-rating: uint
    }
)

(define-public (rate-provider (provider principal) (rating uint))
    (let (
        (current-ratings (default-to {total-ratings: u0, rating-sum: u0, average-rating: u0} 
            (map-get? provider-ratings provider)))
        (new-total (+ (get total-ratings current-ratings) u1))
        (new-sum (+ (get rating-sum current-ratings) rating))
        (new-average (/ new-sum new-total))
    )
        (ok (map-set provider-ratings provider
            {
                total-ratings: new-total,
                rating-sum: new-sum,
                average-rating: new-average
            }))
    )
)



(define-public (get-provider-rating (provider principal))
    (let (
        (ratings (default-to {total-ratings: u0, rating-sum: u0, average-rating: u0} 
            (map-get? provider-ratings provider)))
    )
        (ok ratings)
    )
)


(define-map consumer-rewards
    principal
    {
        points: uint,
        tier: uint
    }
)

(define-public (calculate-rewards (payment-amount uint))
    (let (
        (current-rewards (default-to {points: u0, tier: u1} 
            (map-get? consumer-rewards tx-sender)))
        (new-points (+ (get points current-rewards) (/ payment-amount u100)))
    )
        (ok (map-set consumer-rewards tx-sender
            {
                points: new-points,
                tier: (if (> new-points u10000) u2 u1)
            }))
    )
)

(define-map payment-plans
    {consumer: principal, period: uint}
    {
        total-amount: uint,
        installments: uint,
        amount-paid: uint,
        completed: bool
    }
)

(define-public (create-payment-plan (period uint) (installments uint))
    (let (
        (usage (unwrap! (map-get? usage-records {consumer: tx-sender, period: period}) err-not-found))
    )
        (ok (map-set payment-plans {consumer: tx-sender, period: period}
            {
                total-amount: (get amount-due usage),
                installments: installments,
                amount-paid: u0,
                completed: false
            }))
    )
)



(define-map service-requests
    uint
    {
        consumer: principal,
        provider: principal,
        request-type: (string-ascii 50),
        status: (string-ascii 20),
        timestamp: uint
    }
)

(define-data-var request-counter uint u0)

(define-public (create-service-request (provider principal) (request-type (string-ascii 50)))
    (let (
        (request-id (+ (var-get request-counter) u1))
    )
        (var-set request-counter request-id)
        (ok (map-set service-requests request-id
            {
                consumer: tx-sender,
                provider: provider,
                request-type: request-type,
                status: "pending",
                timestamp: stacks-block-height
            }))
    )
)

(define-map usage-alerts
    principal
    {
        threshold: uint,
        alert-active: bool
    }
)

(define-public (set-usage-alert (threshold uint))
    (ok (map-set usage-alerts tx-sender
        {
            threshold: threshold,
            alert-active: true
        }))
)

(define-read-only (check-usage-alert (consumer principal) (current-usage uint))
    (match (map-get? usage-alerts consumer)
        alert (> current-usage (get threshold alert))
        false
    )
)



(define-map split-bills
    uint
    {
        primary-payer: principal,
        participants: (list 5 principal),
        amount-per-person: uint,
        settled: (list 5 bool)
    }
)

(define-data-var split-bill-counter uint u0)

(define-public (create-split-bill (participants (list 5 principal)) (period uint))
    (let (
        (usage (unwrap! (map-get? usage-records {consumer: tx-sender, period: period}) err-not-found))
        (split-id (+ (var-get split-bill-counter) u1))
        (amount-per-person (/ (get amount-due usage) (+ (len participants) u1)))
    )
        (var-set split-bill-counter split-id)
        (ok (map-set split-bills split-id
            {
                primary-payer: tx-sender,
                participants: participants,
                amount-per-person: amount-per-person,
                settled: (list false false false false false)
            }))
    )
)


(define-map auto-payments
    principal
    {
        enabled: bool,
        max-amount: uint
    }
)

(define-public (enable-auto-pay (max-amount uint))
    (ok (map-set auto-payments tx-sender
        {
            enabled: true,
            max-amount: max-amount
        }))
)

(define-read-only (can-auto-pay (consumer principal) (amount uint))
    (match (map-get? auto-payments consumer)
        settings (and (get enabled settings) (<= amount (get max-amount settings)))
        false
    )
)