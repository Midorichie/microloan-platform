;; loan-rating.clar - Loan Rating System for Microloan Platform

;; Error codes
(define-constant ERR_UNAUTHORIZED u1)
(define-constant ERR_ALREADY_RATED u2)
(define-constant ERR_LOAN_NOT_FOUND u3)
(define-constant ERR_INVALID_RATING u4)
(define-constant ERR_NOT_LENDER u5)
(define-constant ERR_NOT_BORROWER u6)
(define-constant ERR_LOAN_NOT_COMPLETED u7)

;; Constants
(define-constant RATING_MIN u1)
(define-constant RATING_MAX u5)

;; Data structures
(define-map borrower_ratings
  { loan_id: uint, rater: principal }
  { rating: uint, comment: (string-ascii 500), timestamp: uint }
)

(define-map lender_ratings
  { loan_id: uint, rater: principal }
  { rating: uint, comment: (string-ascii 500), timestamp: uint }
)

(define-map aggregate_ratings
  { principal: principal }
  { total_ratings: uint, sum_ratings: uint, average_rating: uint }
)

;; Helper functions for keys
(define-private (mk_borrower_rating_key (loan_id uint) (rater principal))
  { loan_id: loan_id, rater: rater }
)

(define-private (mk_lender_rating_key (loan_id uint) (rater principal))
  { loan_id: loan_id, rater: rater }
)

(define-private (mk_aggregate_key (user principal))
  { principal: user }
)

;; Private functions
(define-private (validate_rating (rating uint))
  (and (>= rating RATING_MIN) (<= rating RATING_MAX))
)

(define-private (update_aggregate_rating (user principal) (new_rating uint))
  (let ((current (default-to 
                   { total_ratings: u0, sum_ratings: u0, average_rating: u0 }
                   (map-get? aggregate_ratings (mk_aggregate_key user)))))
    (let ((new_total (+ (get total_ratings current) u1))
          (new_sum (+ (get sum_ratings current) new_rating)))
      (map-set aggregate_ratings
               (mk_aggregate_key user)
               { total_ratings: new_total, sum_ratings: new_sum, average_rating: (/ (* new_sum u100) new_total) }
      )
      (ok true)
    )
  )
)

;; Public functions

;; Rate a borrower (can only be done by a lender)
(define-public (rate_borrower (loan_id uint) (borrower principal) (rating uint) (comment (string-ascii 500)))
  (let ((rater tx-sender))
    (asserts! (validate_rating rating) (err ERR_INVALID_RATING))
    (asserts! (is-none (map-get? borrower_ratings (mk_borrower_rating_key loan_id rater)))
              (err ERR_ALREADY_RATED))
    (map-set borrower_ratings
             (mk_borrower_rating_key loan_id rater)
             { rating: rating, comment: comment, timestamp: block-height }
    )
    (unwrap! (update_aggregate_rating borrower rating) (err ERR_INVALID_RATING))
    (ok true)
  )
)

;; Rate a lender (can only be done by a borrower)
(define-public (rate_lender (loan_id uint) (lender principal) (rating uint) (comment (string-ascii 500)))
  (let ((rater tx-sender))
    (asserts! (validate_rating rating) (err ERR_INVALID_RATING))
    (asserts! (is-none (map-get? lender_ratings (mk_lender_rating_key loan_id rater)))
              (err ERR_ALREADY_RATED))
    (map-set lender_ratings
             (mk_lender_rating_key loan_id rater)
             { rating: rating, comment: comment, timestamp: block-height }
    )
    (unwrap! (update_aggregate_rating lender rating) (err ERR_INVALID_RATING))
    (ok true)
  )
)

;; Get a borrower rating
(define-read-only (get_borrower_rating (loan_id uint) (rater principal))
  (map-get? borrower_ratings (mk_borrower_rating_key loan_id rater))
)

;; Get a lender rating
(define-read-only (get_lender_rating (loan_id uint) (rater principal))
  (map-get? lender_ratings (mk_lender_rating_key loan_id rater))
)

;; Get aggregate rating for a user
(define-read-only (get_aggregate_rating (user principal))
  (default-to 
    { total_ratings: u0, sum_ratings: u0, average_rating: u0 }
    (map-get? aggregate_ratings (mk_aggregate_key user))
  )
)

;; Get formatted average rating for a user (returns a value between 1.00 and 5.00)
(define-read-only (get_formatted_rating (user principal))
  (let ((ratings (get_aggregate_rating user)))
    (if (is-eq (get total_ratings ratings) u0)
        u0
        (/ (get average_rating ratings) u100)
    )
  )
)
