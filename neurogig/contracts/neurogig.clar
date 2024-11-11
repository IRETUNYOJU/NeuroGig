;; NeuroGig: AI-Powered Decentralized Talent Ecosystem Smart Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-ai-verification-failed (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-milestone-not-completed (err u106))
(define-constant err-invalid-referral (err u107))

;; Define tokens for rewards and incentives
(define-fungible-token neurogig-token)

;; Define data maps
(define-map Tasks 
  { task-id: uint }
  {
    client: principal,
    freelancer: principal,
    arbiter: (optional principal),
    total-payment: uint,
    milestones: (list 10 { description: (string-ascii 100), payment: uint, completed: bool, approved-by-client: bool, approved-by-freelancer: bool, deadline: uint }),
    current-milestone: uint,
    is-completed: bool,
    escrow-balance: uint,
    early-completion-bonus: uint
  }
)

(define-map Users
  { user: principal }
  {
    referral-count: uint,
    completed-tasks: uint,
    loyalty-points: uint
  }
)

(define-map Referrals
  { referrer: principal, referred: principal }
  { is-valid: bool }
)

;; Define variables
(define-data-var next-task-id uint u0)
(define-data-var referral-reward uint u100)  ;; 100 tokens per referral
(define-data-var early-completion-bonus-rate uint u10)  ;; 10% bonus for early completion
(define-data-var loyalty-point-rate uint u5)  ;; 5 loyalty points per completed task

;; Define functions
(define-public (create-task (freelancer principal) (arbiter (optional principal)) (total-payment uint) (milestones (list 10 { description: (string-ascii 100), payment: uint, completed: bool, approved-by-client: bool, approved-by-freelancer: bool, deadline: uint })))
  (let ((task-id (var-get next-task-id)))
    (asserts! (>= (stx-get-balance tx-sender) total-payment) err-insufficient-funds)
    (try! (stx-transfer? total-payment tx-sender (as-contract tx-sender)))
    (map-set Tasks { task-id: task-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        arbiter: arbiter,
        total-payment: total-payment,
        milestones: milestones,
        current-milestone: u0,
        is-completed: false,
        escrow-balance: total-payment,
        early-completion-bonus: u0
      }
    )
    (var-set next-task-id (+ task-id u1))
    (ok task-id)
  )
)

(define-public (complete-milestone (task-id uint))
  (let (
    (task (unwrap! (map-get? Tasks { task-id: task-id }) err-not-found))
    (current-milestone (get current-milestone task))
    (milestones (get milestones task))
    (milestone (unwrap! (element-at milestones current-milestone) err-not-found))
  )
    (asserts! (is-eq (get freelancer task) tx-sender) err-unauthorized)
    (asserts! (< current-milestone (len milestones)) err-not-found)
    
    ;; Simulate AI/ML verification (In a real-world scenario, this would be an oracle or external verification)
    (if (is-ai-verified task-id)
      (begin
        (map-set Tasks { task-id: task-id }
          (merge task {
            milestones: (replace-at milestones current-milestone (merge milestone { completed: true, approved-by-freelancer: true })),
            current-milestone: (+ current-milestone u1),
            early-completion-bonus: (if (< block-height (get deadline milestone))
                                        (+ (get early-completion-bonus task) (/ (* (get payment milestone) (var-get early-completion-bonus-rate)) u100))
                                        (get early-completion-bonus task))
          })
        )
        (ok true)
      )
      err-ai-verification-failed
    )
  )
)

(define-read-only (is-ai-verified (task-id uint))
  ;; Simulate AI/ML verification (replace with actual implementation)
  (is-eq (mod task-id u2) u0) ;; Simple simulation: even task-ids are verified
)

(define-public (approve-milestone (task-id uint) (milestone-index uint))
  (let (
    (task (unwrap! (map-get? Tasks { task-id: task-id }) err-not-found))
    (milestones (get milestones task))
    (milestone (unwrap! (element-at milestones milestone-index) err-not-found))
  )
    (asserts! (is-eq (get client task) tx-sender) err-unauthorized)
    (asserts! (get completed milestone) err-milestone-not-completed)
    (map-set Tasks { task-id: task-id }
      (merge task {
        milestones: (replace-at milestones milestone-index (merge milestone { approved-by-client: true }))
      })
    )
    (try! (release-payment task-id milestone-index))
    (ok true)
  )
)

(define-private (release-payment (task-id uint) (milestone-index uint))
  (let (
    (task (unwrap! (map-get? Tasks { task-id: task-id }) err-not-found))
    (milestone (unwrap! (element-at (get milestones task) milestone-index) err-not-found))
    (payment-amount (get payment milestone))
    (bonus-amount (get early-completion-bonus task))
    (total-amount (+ payment-amount bonus-amount))
  )
    (asserts! (and (get approved-by-client milestone) (get approved-by-freelancer milestone)) err-unauthorized)
    (asserts! (<= total-amount (get escrow-balance task)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? total-amount tx-sender (get freelancer task))))
    (map-set Tasks { task-id: task-id }
      (merge task {
        escrow-balance: (- (get escrow-balance task) total-amount),
        early-completion-bonus: u0
      })
    )
    (try! (update-user-stats (get freelancer task) (get client task)))
    (ok true)
  )
)

(define-public (request-partial-payment (task-id uint) (milestone-index uint) (partial-amount uint))
  (let (
    (task (unwrap! (map-get? Tasks { task-id: task-id }) err-not-found))
    (milestones (get milestones task))
    (milestone (unwrap! (element-at milestones milestone-index) err-not-found))
  )
    (asserts! (is-eq (get freelancer task) tx-sender) err-unauthorized)
    (asserts! (<= partial-amount (get payment milestone)) err-insufficient-funds)
    (asserts! (get completed milestone) err-milestone-not-completed)
    (map-set Tasks { task-id: task-id }
      (merge task {
        milestones: (replace-at milestones milestone-index (merge milestone { payment: (- (get payment milestone) partial-amount), approved-by-freelancer: true }))
      })
    )
    (ok true)
  )
)

(define-public (approve-partial-payment (task-id uint) (milestone-index uint))
  (let (
    (task (unwrap! (map-get? Tasks { task-id: task-id }) err-not-found))
    (milestones (get milestones task))
    (milestone (unwrap! (element-at milestones milestone-index) err-not-found))
  )
    (asserts! (is-eq (get client task) tx-sender) err-unauthorized)
    (asserts! (get completed milestone) err-milestone-not-completed)
    (map-set Tasks { task-id: task-id }
      (merge task {
        milestones: (replace-at milestones milestone-index (merge milestone { approved-by-client: true }))
      })
    )
    (try! (release-payment task-id milestone-index))
    (ok true)
  )
)

(define-public (complete-task (task-id uint))
  (let (
    (task (unwrap! (map-get? Tasks { task-id: task-id }) err-not-found))
  )
    (asserts! (is-eq (get client task) tx-sender) err-unauthorized)
    (map-set Tasks { task-id: task-id }
      (merge task { is-completed: true })
    )
    (ok true)
  )
)

(define-public (refund-remaining-balance (task-id uint))
  (let (
    (task (unwrap! (map-get? Tasks { task-id: task-id }) err-not-found))
    (remaining-balance (get escrow-balance task))
  )
    (asserts! (is-eq (get client task) tx-sender) err-unauthorized)
    (asserts! (get is-completed task) err-unauthorized)
    (asserts! (> remaining-balance u0) err-insufficient-funds)
    (try! (as-contract (stx-transfer? remaining-balance tx-sender (get client task))))
    (map-set Tasks { task-id: task-id }
      (merge task { escrow-balance: u0 })
    )
    (ok true)
  )
)

(define-public (refer-user (referred principal))
  (let (
    (referrer tx-sender)
  )
    (asserts! (not (is-eq referrer referred)) err-invalid-referral)
    (asserts! (is-none (map-get? Users { user: referred })) err-already-exists)
    (map-set Referrals { referrer: referrer, referred: referred } { is-valid: true })
    (ok true)
  )
)

(define-public (claim-referral-reward (referred principal))
  (let (
    (referrer tx-sender)
    (referral (unwrap! (map-get? Referrals { referrer: referrer, referred: referred }) err-not-found))
  )
    (asserts! (get is-valid referral) err-invalid-referral)
    (try! (mint-tokens referrer (var-get referral-reward)))
    (map-set Referrals { referrer: referrer, referred: referred } { is-valid: false })
    (ok true)
  )
)

(define-private (update-user-stats (freelancer principal) (client principal))
  (let (
    (freelancer-stats (default-to { referral-count: u0, completed-tasks: u0, loyalty-points: u0 } (map-get? Users { user: freelancer })))
    (client-stats (default-to { referral-count: u0, completed-tasks: u0, loyalty-points: u0 } (map-get? Users { user: client })))
  )
    (map-set Users { user: freelancer }
      (merge freelancer-stats {
        completed-tasks: (+ (get completed-tasks freelancer-stats) u1)
      })
    )
    (map-set Users { user: client }
      (merge client-stats {
        loyalty-points: (+ (get loyalty-points client-stats) (var-get loyalty-point-rate))
      })
    )
    (ok true)
  )
)

(define-public (redeem-loyalty-points (amount uint))
  (let (
    (user-stats (unwrap! (map-get? Users { user: tx-sender }) err-not-found))
    (current-points (get loyalty-points user-stats))
  )
    (asserts! (>= current-points amount) err-insufficient-funds)
    (map-set Users { user: tx-sender }
      (merge user-stats {
        loyalty-points: (- current-points amount)
      })
    )
    (try! (mint-tokens tx-sender amount))
    (ok true)
  )
)

(define-private (mint-tokens (recipient principal) (amount uint))
  (ft-mint? neurogig-token amount recipient)
)

;; Initialize variables
(define-data-var next-task-id uint u0)