
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


(define-map usage-history
    {consumer: principal, month: uint, year: uint}
    {
        total-units: uint,
        total-cost: uint,
        average-daily-usage: uint,
        days-in-period: uint,
        recorded-at: uint
    }
)

(define-map seasonal-patterns
    {consumer: principal, season: uint}
    {
        average-usage: uint,
        pattern-strength: uint,
        last-updated: uint
    }
)

(define-map consumption-forecasts
    {consumer: principal, forecast-month: uint, forecast-year: uint}
    {
        predicted-usage: uint,
        predicted-cost: uint,
        confidence-level: uint,
        generated-at: uint
    }
)

(define-constant err-insufficient-data (err u108))
(define-constant err-invalid-period (err u109))

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


(define-public (disable-auto-pay)
    (ok (map-set auto-payments tx-sender
        {
            enabled: false,
            max-amount: u0
        }))
)


(define-map provider-delegates
    {provider: principal, delegate: principal}
    {
        active: bool,
        role: (string-ascii 20),
        delegation-time: uint
    }
)

(define-public (add-provider-delegate (delegate principal) (role (string-ascii 20)))
    (let (
        (provider (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
    )
        (asserts! (get active provider) err-unauthorized)
        (ok (map-set provider-delegates 
            {provider: tx-sender, delegate: delegate}
            {
                active: true,
                role: role,
                delegation-time: stacks-block-height
            }))
    )
)

(define-public (remove-provider-delegate (delegate principal))
    (let (
        (provider (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
        (delegation (unwrap! (map-get? provider-delegates {provider: tx-sender, delegate: delegate}) err-not-found))
    )
        (ok (map-set provider-delegates 
            {provider: tx-sender, delegate: delegate}
            (merge delegation {active: false})))
    )
)

(define-read-only (is-authorized-delegate (provider principal) (delegate principal))
    (match (map-get? provider-delegates {provider: provider, delegate: delegate})
        delegation (get active delegation)
        false
    )
)


(define-map usage-analytics
    principal
    {
        peak-usage: uint,
        average-usage: uint,
        total-payments: uint,
        payment-count: uint,
        last-analysis: uint
    }
)

(define-public (update-usage-analytics (consumer principal) (current-usage uint) (payment-amount uint))
    (let (
        (existing-analytics (default-to 
            {peak-usage: u0, average-usage: u0, total-payments: u0, payment-count: u0, last-analysis: u0}
            (map-get? usage-analytics consumer)))
        (new-peak (if (> current-usage (get peak-usage existing-analytics))
            current-usage
            (get peak-usage existing-analytics)))
        (new-payment-count (+ (get payment-count existing-analytics) u1))
        (new-average (/ (+ current-usage (* (get average-usage existing-analytics) (get payment-count existing-analytics))) new-payment-count))
    )
        (ok (map-set usage-analytics consumer
            {
                peak-usage: new-peak,
                average-usage: new-average,
                total-payments: (+ (get total-payments existing-analytics) payment-amount),
                payment-count: new-payment-count,
                last-analysis: stacks-block-height
            }))
    )
)

(define-read-only (get-consumer-analytics (consumer principal))
    (map-get? usage-analytics consumer)
)

(define-map emergency-credits
    {consumer: principal, credit-id: uint}
    {
        provider: principal,
        credit-amount: uint,
        reason: (string-ascii 100),
        granted-at: uint,
        repayment-deadline: uint,
        amount-repaid: uint,
        status: (string-ascii 20),
        interest-rate: uint
    }
)

(define-map consumer-credit-eligibility
    principal
    {
        max-credit-limit: uint,
        current-credit-used: uint,
        credit-score: uint,
        emergency-credits-taken: uint,
        last-credit-date: uint
    }
)

(define-data-var emergency-credit-counter uint u0)

(define-constant err-credit-limit-exceeded (err u105))
(define-constant err-not-eligible (err u106))
(define-constant err-invalid-repayment (err u107))

(define-public (initialize-credit-eligibility (consumer principal) (credit-limit uint))
    (begin
        (asserts! (is-some (map-get? utility-providers tx-sender)) err-unauthorized)
        (ok (map-set consumer-credit-eligibility consumer
            {
                max-credit-limit: credit-limit,
                current-credit-used: u0,
                credit-score: u100,
                emergency-credits-taken: u0,
                last-credit-date: u0
            }))
    )
)

(define-public (request-emergency-credit (provider principal) (amount uint) (reason (string-ascii 100)))
    (let (
        (eligibility (unwrap! (map-get? consumer-credit-eligibility tx-sender) err-not-found))
        (provider-info (unwrap! (map-get? utility-providers provider) err-not-found))
        (credit-id (+ (var-get emergency-credit-counter) u1))
        (available-credit (- (get max-credit-limit eligibility) (get current-credit-used eligibility)))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (>= available-credit amount) err-credit-limit-exceeded)
        (asserts! (>= (get credit-score eligibility) u50) err-not-eligible)
        (var-set emergency-credit-counter credit-id)
        (map-set emergency-credits {consumer: tx-sender, credit-id: credit-id}
            {
                provider: provider,
                credit-amount: amount,
                reason: reason,
                granted-at: stacks-block-height,
                repayment-deadline: (+ stacks-block-height u4320),
                amount-repaid: u0,
                status: "pending",
                interest-rate: u5
            })
        (ok credit-id)
    )
)

(define-public (approve-emergency-credit (consumer principal) (credit-id uint))
    (let (
        (credit (unwrap! (map-get? emergency-credits {consumer: consumer, credit-id: credit-id}) err-not-found))
        (eligibility (unwrap! (map-get? consumer-credit-eligibility consumer) err-not-found))
    )
        (asserts! (is-eq tx-sender (get provider credit)) err-unauthorized)
        (asserts! (is-eq (get status credit) "pending") err-invalid-amount)
        (try! (stx-transfer? (get credit-amount credit) tx-sender consumer))
        (map-set emergency-credits {consumer: consumer, credit-id: credit-id}
            (merge credit {status: "active"}))
        (map-set consumer-credit-eligibility consumer
            (merge eligibility 
                {
                    current-credit-used: (+ (get current-credit-used eligibility) (get credit-amount credit)),
                    emergency-credits-taken: (+ (get emergency-credits-taken eligibility) u1),
                    last-credit-date: stacks-block-height
                }))
        (ok true)
    )
)

(define-public (repay-emergency-credit (credit-id uint) (repayment-amount uint))
    (let (
        (credit (unwrap! (map-get? emergency-credits {consumer: tx-sender, credit-id: credit-id}) err-not-found))
        (eligibility (unwrap! (map-get? consumer-credit-eligibility tx-sender) err-not-found))
        (interest-amount (/ (* (get credit-amount credit) (get interest-rate credit)) u100))
        (total-due (+ (get credit-amount credit) interest-amount))
        (remaining-balance (- total-due (get amount-repaid credit)))
        (new-amount-repaid (+ (get amount-repaid credit) repayment-amount))
    )
        (asserts! (is-eq (get status credit) "active") err-invalid-repayment)
        (asserts! (<= repayment-amount remaining-balance) err-invalid-amount)
        (try! (stx-transfer? repayment-amount tx-sender (get provider credit)))
        (let (
            (new-status (if (>= new-amount-repaid total-due) "completed" "active"))
            (credit-reduction (if (>= new-amount-repaid total-due) (get credit-amount credit) u0))
        )
            (map-set emergency-credits {consumer: tx-sender, credit-id: credit-id}
                (merge credit 
                    {
                        amount-repaid: new-amount-repaid,
                        status: new-status
                    }))
            (map-set consumer-credit-eligibility tx-sender
                (merge eligibility 
                    {
                        current-credit-used: (- (get current-credit-used eligibility) credit-reduction),
                        credit-score: (if (is-eq new-status "completed") 
                            (my-min (+ (get credit-score eligibility) u10) u100)
                            (get credit-score eligibility))
                    }))
            (ok true)
        )
    )
)

(define-public (deny-emergency-credit (consumer principal) (credit-id uint))
    (let (
        (credit (unwrap! (map-get? emergency-credits {consumer: consumer, credit-id: credit-id}) err-not-found))
    )
        (asserts! (is-eq tx-sender (get provider credit)) err-unauthorized)
        (asserts! (is-eq (get status credit) "pending") err-invalid-amount)
        (ok (map-set emergency-credits {consumer: consumer, credit-id: credit-id}
            (merge credit {status: "denied"})))
    )
)

(define-read-only (get-emergency-credit-details (consumer principal) (credit-id uint))
    (map-get? emergency-credits {consumer: consumer, credit-id: credit-id})
)

(define-read-only (get-consumer-credit-status (consumer principal))
    (map-get? consumer-credit-eligibility consumer)
)

(define-read-only (calculate-credit-interest (consumer principal) (credit-id uint))
    (match (map-get? emergency-credits {consumer: consumer, credit-id: credit-id})
        credit (/ (* (get credit-amount credit) (get interest-rate credit)) u100)
        u0
    )
)

(define-read-only (get-total-credit-due (consumer principal) (credit-id uint))
    (match (map-get? emergency-credits {consumer: consumer, credit-id: credit-id})
        credit (let (
            (interest (/ (* (get credit-amount credit) (get interest-rate credit)) u100))
            (total-due (+ (get credit-amount credit) interest))
        )
            (- total-due (get amount-repaid credit)))
        u0
    )
)

(define-read-only (is-credit-overdue (consumer principal) (credit-id uint))
    (match (map-get? emergency-credits {consumer: consumer, credit-id: credit-id})
        credit (and 
            (is-eq (get status credit) "active")
            (> stacks-block-height (get repayment-deadline credit)))
        false
    )
)

;; Helper function for minimum of two uints
(define-read-only (my-min (a uint) (b uint))
    (if (< a b) a b)
)

;; Dynamic Pricing Engine
;; Real-time utility rate adjustments based on demand, time, and grid conditions

;; Dynamic pricing constants
(define-constant err-invalid-tier (err u110))
(define-constant err-pricing-inactive (err u111))
(define-constant err-demand-overflow (err u112))

;; Time-based pricing tiers for providers
(define-map pricing-tiers
    {provider: principal, tier-level: uint}
    {
        rate-multiplier: uint,          ;; Percentage multiplier (100 = 1x, 150 = 1.5x base rate)
        demand-threshold: uint,         ;; Usage threshold to trigger this tier
        time-start: uint,              ;; Hour of day when tier becomes active (0-23)
        time-end: uint,                ;; Hour of day when tier ends (0-23)
        active: bool,                  ;; Whether this tier is currently enabled
        tier-name: (string-ascii 30)   ;; Description of the tier
    }
)

;; Current demand tracking for each provider
(define-map current-demand
    principal
    {
        total-active-consumers: uint,
        current-usage-rate: uint,       ;; Units consumed per hour across all consumers
        demand-level: uint,             ;; Current demand level (1-5, where 5 is highest)
        last-updated: uint,
        peak-demand-today: uint
    }
)

;; Dynamic rate history for transparency
(define-map rate-changes
    {provider: principal, change-id: uint}
    {
        old-rate: uint,
        new-rate: uint,
        reason: (string-ascii 50),      ;; "peak-hours", "high-demand", "emergency", etc.
        effective-time: uint,
        duration-hours: uint
    }
)

;; Consumer rate notifications
(define-map rate-notifications
    {consumer: principal, notification-id: uint}
    {
        provider: principal,
        new-rate: uint,
        rate-change-percent: uint,
        notification-type: (string-ascii 30),
        created-at: uint,
        acknowledged: bool
    }
)

;; Surge pricing triggers
(define-map surge-conditions
    principal
    {
        surge-active: bool,
        surge-multiplier: uint,         ;; Additional multiplier during surge
        trigger-threshold: uint,        ;; Demand level that triggers surge
        max-surge-rate: uint,          ;; Maximum rate during surge
        surge-started: uint,
        estimated-duration: uint
    }
)

;; Data variables for pricing system
(define-data-var rate-change-counter uint u0)
(define-data-var notification-counter uint u0)

;; Setup pricing tiers for a provider
(define-public (setup-pricing-tier (tier-level uint) (rate-multiplier uint) (demand-threshold uint) 
                                  (time-start uint) (time-end uint) (tier-name (string-ascii 30)))
    (let (
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (and (>= tier-level u1) (<= tier-level u5)) err-invalid-tier)
        (asserts! (and (>= rate-multiplier u50) (<= rate-multiplier u500)) err-invalid-amount)
        (asserts! (and (>= time-start u0) (<= time-start u23)) err-invalid-period)
        (asserts! (and (>= time-end u0) (<= time-end u23)) err-invalid-period)
        (ok (map-set pricing-tiers {provider: tx-sender, tier-level: tier-level}
            {
                rate-multiplier: rate-multiplier,
                demand-threshold: demand-threshold,
                time-start: time-start,
                time-end: time-end,
                active: true,
                tier-name: tier-name
            }))
    )
)

;; Update current demand levels for dynamic pricing
(define-public (update-demand-metrics (total-consumers uint) (current-usage-rate uint))
    (let (
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
        (existing-demand (default-to 
            {total-active-consumers: u0, current-usage-rate: u0, demand-level: u1, last-updated: u0, peak-demand-today: u0}
            (map-get? current-demand tx-sender)))
        (new-demand-level (calculate-demand-level current-usage-rate total-consumers))
        (new-peak (my-max (get peak-demand-today existing-demand) current-usage-rate))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (<= total-consumers u10000) err-demand-overflow)
        (ok (map-set current-demand tx-sender
            {
                total-active-consumers: total-consumers,
                current-usage-rate: current-usage-rate,
                demand-level: new-demand-level,
                last-updated: stacks-block-height,
                peak-demand-today: new-peak
            }))
    )
)

;; Calculate current dynamic rate for a provider
(define-public (get-current-dynamic-rate (provider principal) (current-hour uint))
    (let (
        (base-provider (unwrap! (map-get? utility-providers provider) err-not-found))
        (demand-info (default-to 
            {total-active-consumers: u0, current-usage-rate: u0, demand-level: u1, last-updated: u0, peak-demand-today: u0}
            (map-get? current-demand provider)))
        (base-rate (get rate-per-unit base-provider))
        (active-tier (find-active-pricing-tier provider (get demand-level demand-info) current-hour))
        (surge-info (map-get? surge-conditions provider))
    )
        (asserts! (get active base-provider) err-unauthorized)
        (let (
            (tier-adjusted-rate (match active-tier
                tier (* base-rate (/ (get rate-multiplier tier) u100))
                base-rate))
            (final-rate (match surge-info
                surge (if (get surge-active surge)
                    (my-min (* tier-adjusted-rate (/ (get surge-multiplier surge) u100)) (get max-surge-rate surge))
                    tier-adjusted-rate)
                tier-adjusted-rate))
        )
            (ok final-rate)
        )
    )
)

;; Activate surge pricing during high demand
(define-public (activate-surge-pricing (surge-multiplier uint) (estimated-duration uint))
    (let (
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
        (demand-info (unwrap! (map-get? current-demand tx-sender) err-not-found))
        (base-rate (get rate-per-unit provider-info))
        (max-surge (my-min (* base-rate u3) (* base-rate (/ surge-multiplier u100))))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (>= (get demand-level demand-info) u4) err-invalid-amount)
        (asserts! (and (>= surge-multiplier u120) (<= surge-multiplier u300)) err-invalid-amount)
        (ok (map-set surge-conditions tx-sender
            {
                surge-active: true,
                surge-multiplier: surge-multiplier,
                trigger-threshold: u4,
                max-surge-rate: max-surge,
                surge-started: stacks-block-height,
                estimated-duration: estimated-duration
            }))
    )
)

;; Deactivate surge pricing
(define-public (deactivate-surge-pricing)
    (let (
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
        (existing-surge (unwrap! (map-get? surge-conditions tx-sender) err-not-found))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (ok (map-set surge-conditions tx-sender
            (merge existing-surge {surge-active: false})))
    )
)

;; Send rate change notification to consumer
(define-public (notify-rate-change (consumer principal) (new-rate uint) (change-percent uint) (notification-type (string-ascii 30)))
    (let (
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
        (notification-id (+ (var-get notification-counter) u1))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (var-set notification-counter notification-id)
        (ok (map-set rate-notifications {consumer: consumer, notification-id: notification-id}
            {
                provider: tx-sender,
                new-rate: new-rate,
                rate-change-percent: change-percent,
                notification-type: notification-type,
                created-at: stacks-block-height,
                acknowledged: false
            }))
    )
)

;; Consumer acknowledges rate change notification
(define-public (acknowledge-rate-notification (notification-id uint))
    (let (
        (notification (unwrap! (map-get? rate-notifications {consumer: tx-sender, notification-id: notification-id}) err-not-found))
    )
        (ok (map-set rate-notifications {consumer: tx-sender, notification-id: notification-id}
            (merge notification {acknowledged: true})))
    )
)

;; Read-only functions for dynamic pricing

;; Get pricing tier details
(define-read-only (get-pricing-tier (provider principal) (tier-level uint))
    (map-get? pricing-tiers {provider: provider, tier-level: tier-level})
)

;; Get current demand information
(define-read-only (get-demand-status (provider principal))
    (map-get? current-demand provider)
)

;; Get surge pricing status
(define-read-only (get-surge-status (provider principal))
    (map-get? surge-conditions provider)
)

;; Get consumer notifications
(define-read-only (get-rate-notification (consumer principal) (notification-id uint))
    (map-get? rate-notifications {consumer: consumer, notification-id: notification-id})
)

;; Check if provider has dynamic pricing enabled
(define-read-only (has-dynamic-pricing (provider principal))
    (is-some (map-get? pricing-tiers {provider: provider, tier-level: u1}))
)

;; Private helper functions for dynamic pricing

;; Calculate demand level based on usage and consumer count
(define-read-only (calculate-demand-level (usage-rate uint) (consumer-count uint))
    (let (
        (usage-per-consumer (if (> consumer-count u0) (/ usage-rate consumer-count) u0))
    )
        (if (> usage-per-consumer u100) u5
            (if (> usage-per-consumer u75) u4
                (if (> usage-per-consumer u50) u3
                    (if (> usage-per-consumer u25) u2 u1))))
    )
)

;; Find active pricing tier based on demand and time
(define-read-only (find-active-pricing-tier (provider principal) (demand-level uint) (current-hour uint))
    (let (
        (tier1 (map-get? pricing-tiers {provider: provider, tier-level: u1}))
        (tier2 (map-get? pricing-tiers {provider: provider, tier-level: u2}))
        (tier3 (map-get? pricing-tiers {provider: provider, tier-level: u3}))
        (tier4 (map-get? pricing-tiers {provider: provider, tier-level: u4}))
        (tier5 (map-get? pricing-tiers {provider: provider, tier-level: u5}))
    )
        (if (and (>= demand-level u5) (is-tier-active-now tier5 current-hour)) tier5
            (if (and (>= demand-level u4) (is-tier-active-now tier4 current-hour)) tier4
                (if (and (>= demand-level u3) (is-tier-active-now tier3 current-hour)) tier3
                    (if (and (>= demand-level u2) (is-tier-active-now tier2 current-hour)) tier2
                        (if (is-tier-active-now tier1 current-hour) tier1 none)))))
    )
)

;; Check if a pricing tier is active at current time
(define-read-only (is-tier-active-now (tier (optional {rate-multiplier: uint, demand-threshold: uint, time-start: uint, time-end: uint, active: bool, tier-name: (string-ascii 30)})) (current-hour uint))
    (match tier
        tier-data (and 
            (get active tier-data)
            (or 
                (and (<= (get time-start tier-data) (get time-end tier-data))
                     (and (>= current-hour (get time-start tier-data)) (<= current-hour (get time-end tier-data))))
                (and (> (get time-start tier-data) (get time-end tier-data))
                     (or (>= current-hour (get time-start tier-data)) (<= current-hour (get time-end tier-data))))))
        false)
)

;; Helper function for maximum of two uints
(define-read-only (my-max (a uint) (b uint))
    (if (> a b) a b)
)


(define-public (record-monthly-usage (consumer principal) (month uint) (year uint) (total-units uint) (total-cost uint) (days-in-period uint))
    (let (
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
        (average-daily (/ total-units days-in-period))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (and (>= month u1) (<= month u12)) err-invalid-period)
        (asserts! (> year u0) err-invalid-period)
        (asserts! (> days-in-period u0) err-invalid-period)
        (ok (map-set usage-history {consumer: consumer, month: month, year: year}
            {
                total-units: total-units,
                total-cost: total-cost,
                average-daily-usage: average-daily,
                days-in-period: days-in-period,
                recorded-at: stacks-block-height
            }))
    )
)

(define-public (update-seasonal-pattern (consumer principal) (season uint) (average-usage uint) (pattern-strength uint))
    (begin
        (asserts! (and (>= season u1) (<= season u4)) err-invalid-period)
        (asserts! (<= pattern-strength u100) err-invalid-amount)
        (ok (map-set seasonal-patterns {consumer: consumer, season: season}
            {
                average-usage: average-usage,
                pattern-strength: pattern-strength,
                last-updated: stacks-block-height
            }))
    )
)

(define-public (generate-consumption-forecast (consumer principal) (forecast-month uint) (forecast-year uint))
    (let (
        (current-season (get-season-from-month forecast-month))
        (seasonal-data (map-get? seasonal-patterns {consumer: consumer, season: current-season}))
        (historical-usage (get-historical-average consumer forecast-month))
        (base-prediction (match seasonal-data
            pattern (weighted-average historical-usage (get average-usage pattern) (get pattern-strength pattern))
            historical-usage))
        (current-rate (get-current-rate-for-consumer consumer))
        (predicted-cost (* base-prediction current-rate))
        (confidence (calculate-confidence consumer forecast-month))
    )
        (asserts! (and (>= forecast-month u1) (<= forecast-month u12)) err-invalid-period)
        (asserts! (> forecast-year u0) err-invalid-period)
        (asserts! (> base-prediction u0) err-insufficient-data)
        (ok (map-set consumption-forecasts {consumer: consumer, forecast-month: forecast-month, forecast-year: forecast-year}
            {
                predicted-usage: base-prediction,
                predicted-cost: predicted-cost,
                confidence-level: confidence,
                generated-at: stacks-block-height
            }))
    )
)

(define-read-only (get-consumption-forecast (consumer principal) (forecast-month uint) (forecast-year uint))
    (map-get? consumption-forecasts {consumer: consumer, forecast-month: forecast-month, forecast-year: forecast-year})
)

(define-read-only (get-usage-history (consumer principal) (month uint) (year uint))
    (map-get? usage-history {consumer: consumer, month: month, year: year})
)

(define-read-only (get-seasonal-pattern (consumer principal) (season uint))
    (map-get? seasonal-patterns {consumer: consumer, season: season})
)

(define-read-only (get-season-from-month (month uint))
    (if (<= month u3) u1
        (if (<= month u6) u2
            (if (<= month u9) u3 u4)))
)

(define-read-only (get-historical-average (consumer principal) (month uint))
    (let (
        (last-year-data (map-get? usage-history {consumer: consumer, month: month, year: u2023}))
        (two-years-ago-data (map-get? usage-history {consumer: consumer, month: month, year: u2022}))
    )
        (match last-year-data
            recent-data (get total-units recent-data)
            (match two-years-ago-data
                older-data (get total-units older-data)
                u0))
    )
)

(define-read-only (get-current-rate-for-consumer (consumer principal))
    (let (
        (latest-usage (map-get? usage-records {consumer: consumer, period: u1}))
    )
        (match latest-usage
            usage-data (match (map-get? utility-providers (get provider usage-data))
                provider-data (get rate-per-unit provider-data)
                u0)
            u0)
    )
)

(define-read-only (weighted-average (value1 uint) (value2 uint) (weight uint))
    (/ (+ (* value1 (- u100 weight)) (* value2 weight)) u100)
)

(define-read-only (calculate-confidence (consumer principal) (month uint))
    (let (
        (historical-data-points (count-historical-data-points consumer month))
        (seasonal-data-available (is-some (map-get? seasonal-patterns {consumer: consumer, season: (get-season-from-month month)})))
    )
        (if (and (> historical-data-points u1) seasonal-data-available)
            (my-min (+ (* historical-data-points u20) u20) u100)
            (my-min (* historical-data-points u25) u60))
    )
)

(define-read-only (count-historical-data-points (consumer principal) (month uint))
    (let (
        (data-2023 (is-some (map-get? usage-history {consumer: consumer, month: month, year: u2023})))
        (data-2022 (is-some (map-get? usage-history {consumer: consumer, month: month, year: u2022})))
        (data-2021 (is-some (map-get? usage-history {consumer: consumer, month: month, year: u2021})))
    )
        (+ 
            (if data-2023 u1 u0)
            (if data-2022 u1 u0)
            (if data-2021 u1 u0))
    )
)

(define-read-only (get-forecast-accuracy (consumer principal) (actual-usage uint) (forecast-month uint) (forecast-year uint))
    (match (map-get? consumption-forecasts {consumer: consumer, forecast-month: forecast-month, forecast-year: forecast-year})
        forecast (let (
            (predicted-usage (get predicted-usage forecast))
            (difference (if (> actual-usage predicted-usage) 
                (- actual-usage predicted-usage) 
                (- predicted-usage actual-usage)))
            (accuracy-percentage (- u100 (/ (* difference u100) predicted-usage)))
        )
            accuracy-percentage)
        u0)
)

;; ==========================================
;; OUTAGE REPORTING & COMPENSATION SYSTEM
;; ==========================================

;; Additional error constants for outage system
(define-constant err-outage-not-found (err u113))
(define-constant err-outage-already-resolved (err u114))
(define-constant err-invalid-sla (err u115))
(define-constant err-compensation-already-paid (err u116))

;; Service outage reports
(define-map outage-reports
    {consumer: principal, outage-id: uint}
    {
        provider: principal,
        reported-at: uint,
        acknowledged-at: uint,
        resolved-at: uint,
        outage-type: (string-ascii 30),    ;; "power", "water", "gas", "internet"
        severity: uint,                     ;; 1-5 scale (5 = critical)
        status: (string-ascii 20),         ;; "reported", "acknowledged", "resolved"
        description: (string-ascii 200),
        compensation-calculated: bool,
        compensation-amount: uint
    }
)

;; Provider Service Level Agreements
(define-map provider-slas
    principal
    {
        max-outage-duration: uint,         ;; Maximum acceptable outage duration in blocks
        compensation-rate: uint,           ;; Compensation per block of outage (in microSTX)
        response-time-guarantee: uint,     ;; Guaranteed response time in blocks
        uptime-percentage: uint,           ;; Target uptime percentage (95, 99, etc.)
        penalty-multiplier: uint           ;; Multiplier for compensation if SLA breached
    }
)

;; Outage statistics per provider
(define-map outage-statistics
    principal
    {
        total-outages: uint,
        total-downtime: uint,              ;; Total downtime in blocks
        average-resolution-time: uint,
        sla-violations: uint,
        total-compensation-paid: uint,
        last-outage-date: uint
    }
)

;; Consumer compensation records
(define-map compensation-records
    {consumer: principal, outage-id: uint}
    {
        base-compensation: uint,
        sla-penalty: uint,
        total-compensation: uint,
        paid-at: uint,
        payment-status: (string-ascii 20)  ;; "pending", "paid", "disputed"
    }
)

;; Counters for outage tracking
(define-data-var outage-counter uint u0)

;; Set provider SLA terms
(define-public (set-provider-sla (max-duration uint) (compensation-rate uint) (response-time uint) (uptime-target uint) (penalty-multiplier uint))
    (let (
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (and (> max-duration u0) (<= max-duration u1440)) err-invalid-sla)  ;; Max 1440 blocks (~1 day)
        (asserts! (and (>= uptime-target u90) (<= uptime-target u100)) err-invalid-sla)
        (asserts! (and (>= penalty-multiplier u100) (<= penalty-multiplier u500)) err-invalid-sla)
        (ok (map-set provider-slas tx-sender
            {
                max-outage-duration: max-duration,
                compensation-rate: compensation-rate,
                response-time-guarantee: response-time,
                uptime-percentage: uptime-target,
                penalty-multiplier: penalty-multiplier
            }))
    )
)

;; Report service outage
(define-public (report-outage (provider principal) (outage-type (string-ascii 30)) (severity uint) (description (string-ascii 200)))
    (let (
        (consumer-account (unwrap! (map-get? consumer-accounts tx-sender) err-not-found))
        (provider-info (unwrap! (map-get? utility-providers provider) err-not-found))
        (outage-id (+ (var-get outage-counter) u1))
    )
        (asserts! (get active consumer-account) err-unauthorized)
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (and (>= severity u1) (<= severity u5)) err-invalid-amount)
        (var-set outage-counter outage-id)
        (ok (map-set outage-reports {consumer: tx-sender, outage-id: outage-id}
            {
                provider: provider,
                reported-at: stacks-block-height,
                acknowledged-at: u0,
                resolved-at: u0,
                outage-type: outage-type,
                severity: severity,
                status: "reported",
                description: description,
                compensation-calculated: false,
                compensation-amount: u0
            }))
    )
)

;; Provider acknowledges outage report
(define-public (acknowledge-outage (consumer principal) (outage-id uint))
    (let (
        (outage (unwrap! (map-get? outage-reports {consumer: consumer, outage-id: outage-id}) err-outage-not-found))
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (is-eq tx-sender (get provider outage)) err-unauthorized)
        (asserts! (is-eq (get status outage) "reported") err-invalid-amount)
        (ok (map-set outage-reports {consumer: consumer, outage-id: outage-id}
            (merge outage 
                {
                    acknowledged-at: stacks-block-height,
                    status: "acknowledged"
                })))
    )
)

;; Resolve outage and calculate compensation
(define-public (resolve-outage (consumer principal) (outage-id uint))
    (let (
        (outage (unwrap! (map-get? outage-reports {consumer: consumer, outage-id: outage-id}) err-outage-not-found))
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
        (sla (map-get? provider-slas tx-sender))
        (outage-duration (- stacks-block-height (get reported-at outage)))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (is-eq tx-sender (get provider outage)) err-unauthorized)
        (asserts! (is-eq (get status outage) "acknowledged") err-invalid-amount)
        
        ;; Mark outage as resolved
        (map-set outage-reports {consumer: consumer, outage-id: outage-id}
            (merge outage 
                {
                    resolved-at: stacks-block-height,
                    status: "resolved"
                }))
        
        ;; Calculate and process compensation
        (unwrap-panic (calculate-and-pay-compensation consumer outage-id outage-duration sla))
        
        ;; Update provider statistics
        (let (
            (max-duration (match sla sla-terms (get max-outage-duration sla-terms) u72))
            (sla-violated (> outage-duration max-duration))
        )
            (unwrap-panic (update-outage-statistics tx-sender outage-duration sla-violated)))
        
        (ok true)
    )
)

;; Pay compensation to consumer
(define-public (pay-compensation (consumer principal) (outage-id uint))
    (let (
        (compensation (unwrap! (map-get? compensation-records {consumer: consumer, outage-id: outage-id}) err-not-found))
        (provider-info (unwrap! (map-get? utility-providers tx-sender) err-unauthorized))
    )
        (asserts! (get active provider-info) err-unauthorized)
        (asserts! (is-eq (get payment-status compensation) "pending") err-compensation-already-paid)
        (try! (stx-transfer? (get total-compensation compensation) tx-sender consumer))
        (ok (map-set compensation-records {consumer: consumer, outage-id: outage-id}
            (merge compensation 
                {
                    paid-at: stacks-block-height,
                    payment-status: "paid"
                })))
    )
)

;; Private helper functions for outage system

;; Calculate and process compensation
(define-private (calculate-and-pay-compensation (consumer principal) (outage-id uint) (duration uint) (sla (optional {max-outage-duration: uint, compensation-rate: uint, response-time-guarantee: uint, uptime-percentage: uint, penalty-multiplier: uint})))
    (match sla
        sla-terms (let (
            (base-comp (* duration (get compensation-rate sla-terms)))
            (sla-penalty (if (> duration (get max-outage-duration sla-terms))
                (* base-comp (/ (get penalty-multiplier sla-terms) u100))
                u0))
            (total-comp (+ base-comp sla-penalty))
        )
            (map-set compensation-records {consumer: consumer, outage-id: outage-id}
                {
                    base-compensation: base-comp,
                    sla-penalty: sla-penalty,
                    total-compensation: total-comp,
                    paid-at: u0,
                    payment-status: "pending"
                })
            (ok total-comp))
        (ok u0))
)

;; Update provider outage statistics
(define-private (update-outage-statistics (provider principal) (duration uint) (sla-violated bool))
    (let (
        (current-stats (default-to
            {total-outages: u0, total-downtime: u0, average-resolution-time: u0, sla-violations: u0, total-compensation-paid: u0, last-outage-date: u0}
            (map-get? outage-statistics provider)))
        (new-total-outages (+ (get total-outages current-stats) u1))
        (new-total-downtime (+ (get total-downtime current-stats) duration))
        (new-avg-resolution (/ new-total-downtime new-total-outages))
        (new-sla-violations (if sla-violated (+ (get sla-violations current-stats) u1) (get sla-violations current-stats)))
    )
        (ok (map-set outage-statistics provider
            {
                total-outages: new-total-outages,
                total-downtime: new-total-downtime,
                average-resolution-time: new-avg-resolution,
                sla-violations: new-sla-violations,
                total-compensation-paid: (get total-compensation-paid current-stats),
                last-outage-date: stacks-block-height
            }))
    )
)

;; Read-only functions for outage system

;; Get outage report details
(define-read-only (get-outage-report (consumer principal) (outage-id uint))
    (map-get? outage-reports {consumer: consumer, outage-id: outage-id})
)

;; Get provider SLA details
(define-read-only (get-provider-sla (provider principal))
    (map-get? provider-slas provider)
)

;; Get outage statistics for provider
(define-read-only (get-outage-statistics (provider principal))
    (map-get? outage-statistics provider)
)

;; Get compensation record details
(define-read-only (get-compensation-record (consumer principal) (outage-id uint))
    (map-get? compensation-records {consumer: consumer, outage-id: outage-id})
)

;; Check provider SLA compliance
(define-read-only (check-sla-compliance (provider principal))
    (match (map-get? outage-statistics provider)
        stats (let (
            (sla (map-get? provider-slas provider))
        )
            (match sla
                sla-terms (let (
                    (total-outages (get total-outages stats))
                    (violation-rate (if (> total-outages u0)
                        (/ (* (get sla-violations stats) u100) total-outages)
                        u0))
                    (compliance-rate (- u100 violation-rate))
                )
                    compliance-rate)
                u100))
        u100)
)

;; Calculate expected compensation for outage duration
(define-read-only (calculate-expected-compensation (provider principal) (duration uint))
    (match (map-get? provider-slas provider)
        sla (let (
            (base-comp (* duration (get compensation-rate sla)))
            (penalty (if (> duration (get max-outage-duration sla))
                (* base-comp (/ (get penalty-multiplier sla) u100))
                u0))
        )
            (+ base-comp penalty))
        u0)
)
