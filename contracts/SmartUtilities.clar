
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

