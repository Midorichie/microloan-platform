;; loan-insurance.clar - Insurance for Microloan Platform

;; Error codes
(define-constant ERR_UNAUTHORIZED u1)
(define-constant ERR_ALREADY_INSURED u2)
(define-constant ERR_LOAN_NOT_FOUND u3)
(define-constant ERR_INSURANCE_NOT_FOUND u4)
(define-constant ERR_INSUFFICIENT_FUNDS u5)
(define-constant ERR_LOAN_NOT_DEFAULTED u6)
(define-constant ERR_LOAN_ALREADY_PAID u7)
(define-constant ERR_CLAIM_PERIOD_EXPIRED u8)
(define-constant ERR_INVALID_AMOUNT u9)

;; Constants
(define-constant INSURANCE_PREMIUM_RATE u500)  ;; 5% in basis points
(define-constant CLAIM_WINDOW u30)             ;; 30 days to make a claim
(define-constant MIN_LOAN_AMOUNT u10000)        ;; Minimum insurance amount

;; Data structures
(define-map insurance_policies
  { loan_id: uint }
  { 
    insurer: principal,
    premium: uint,
    coverage_amount: uint,
    created_at: uint,
    claimed: bool,
    claim_paid: bool
  }
)

(define-map claims
  { loan_id: uint }
  {
    claimant: principal,
    amount: uint,
    claimed_at: uint,
    status: (string-ascii 20)  ;; "PENDING", "APPROVED", "REJECTED", "PAID"
  }
)

;; Variables
(define-data-var total_premiums_collected uint u0)
(define-data-var total_claims_paid uint u0)

;; Helper function to wrap the user-supplied loan_id
(define-private (mk_loan_id_key (loan_id uint))
  { loan_id: loan_id }
)

;; Private functions
(define-private (calculate_premium (loan_amount uint))
  (/ (* loan_amount INSURANCE_PREMIUM_RATE) u10000)
)

(define-private (validate_insurance_amount (amount uint))
  (>= amount MIN_LOAN_AMOUNT)
)

;; Public functions

;; Purchase insurance for a loan
(define-public (purchase_insurance (loan_id uint) (coverage_amount uint))
  (let ((purchaser tx-sender))
    (asserts! (validate_insurance_amount coverage_amount) (err ERR_INVALID_AMOUNT))
    (asserts! (is-none (map-get? insurance_policies (mk_loan_id_key loan_id)))
              (err ERR_ALREADY_INSURED))
    (let ((premium (calculate_premium coverage_amount)))
      (unwrap! (stx-transfer? premium purchaser contract-caller)
               (err ERR_INSUFFICIENT_FUNDS))
      (var-set total_premiums_collected
               (+ (var-get total_premiums_collected) premium))
      (map-set insurance_policies
               (mk_loan_id_key loan_id)
               { insurer: purchaser,
                 premium: premium,
                 coverage_amount: coverage_amount,
                 created_at: block-height,
                 claimed: false,
                 claim_paid: false }
      )
      (ok true)
    )
  )
)

;; File a claim
(define-public (file_claim (loan_id uint))
  (let ((claimant tx-sender))
    (let ((policy (unwrap! (map-get? insurance_policies (mk_loan_id_key loan_id))
                           (err ERR_INSURANCE_NOT_FOUND))))
      (asserts! (is-eq (get insurer policy) claimant)
                (err ERR_UNAUTHORIZED))
      (asserts! (not (get claimed policy))
                (err ERR_LOAN_ALREADY_PAID))
      (asserts! (< (- block-height (get created_at policy)) CLAIM_WINDOW)
                (err ERR_CLAIM_PERIOD_EXPIRED))
      (map-set claims
               (mk_loan_id_key loan_id)
               { claimant: claimant,
                 amount: (get coverage_amount policy),
                 claimed_at: block-height,
                 status: "PENDING" }
      )
      (map-set insurance_policies
               (mk_loan_id_key loan_id)
               (merge policy { claimed: true })
      )
      (ok true)
    )
  )
)

;; Process claim (admin function)
(define-public (process_claim (loan_id uint) (approve bool))
  (let ((admin tx-sender))
    (let ((claim (unwrap! (map-get? claims (mk_loan_id_key loan_id))
                           (err ERR_INSURANCE_NOT_FOUND))))
      (let ((policy (unwrap! (map-get? insurance_policies (mk_loan_id_key loan_id))
                             (err ERR_INSURANCE_NOT_FOUND))))
        (if approve
            (begin
              (unwrap! (as-contract 
                        (stx-transfer? (get amount claim) contract-caller (get claimant claim)))
                       (err ERR_INSUFFICIENT_FUNDS))
              (map-set claims
                       (mk_loan_id_key loan_id)
                       (merge claim { status: "APPROVED" })
              )
              (map-set insurance_policies
                       (mk_loan_id_key loan_id)
                       (merge policy { claim_paid: true })
              )
              (var-set total_claims_paid
                       (+ (var-get total_claims_paid) (get amount claim)))
              (ok true)
            )
            (begin
              (map-set claims
                       (mk_loan_id_key loan_id)
                       (merge claim { status: "REJECTED" })
              )
              (ok true)
            )
        )
      )
    )
  )
)

;; Get insurance policy details
(define-read-only (get_insurance_policy (loan_id uint))
  (map-get? insurance_policies (mk_loan_id_key loan_id))
)

;; Get claim details
(define-read-only (get_claim (loan_id uint))
  (map-get? claims (mk_loan_id_key loan_id))
)

;; Get total premiums collected
(define-read-only (get_total_premiums)
  (var-get total_premiums_collected)
)

;; Get total claims paid
(define-read-only (get_total_claims_paid)
  (var-get total_claims_paid)
)

;; Check if a loan is eligible for insurance
(define-read-only (is_eligible_for_insurance (loan_id uint))
  (is-none (map-get? insurance_policies (mk_loan_id_key loan_id)))
)
