;; NeuroGig: AI-Powered Decentralized Freelance Platform

;; Define the fungible token for payments
(define-fungible-token neurogig-token)

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u404))
(define-constant err-unauthorized (err u401))

;; Define data variables
(define-data-var next-user-id uint u0)
(define-data-var next-project-id uint u0)
(define-data-var next-bid-id uint u0)
(define-data-var next-contract-id uint u0)

;; Define data maps
(define-map users uint 
  {
    address: principal,
    role: (string-ascii 20),
    reputation: uint
  }
)

(define-map user-addresses principal uint)

(define-map projects uint 
  {
    owner: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    budget: uint,
    status: (string-ascii 20)
  }
)

(define-map bids uint 
  {
    project-id: uint,
    bidder: uint,
    amount: uint,
    proposal: (string-ascii 500)
  }
)

(define-map contracts uint 
  {
    project-id: uint,
    freelancer: uint,
    client: uint,
    amount: uint,
    status: (string-ascii 20)
  }
)

;; Define public functions

;; Register a new user
(define-public (register-user (role (string-ascii 20)))
  (let
    (
      (user-id (var-get next-user-id))
    )
    (map-set users user-id
      {
        address: tx-sender,
        role: role,
        reputation: u0
      }
    )
    (map-set user-addresses tx-sender user-id)
    (var-set next-user-id (+ user-id u1))
    (ok user-id)
  )
)

;; Create a new project
(define-public (create-project (title (string-ascii 100)) (description (string-ascii 500)) (budget uint))
  (let
    (
      (project-id (var-get next-project-id))
      (user-id (get-user-id tx-sender))
    )
    (asserts! (is-some user-id) err-not-found)
    (map-set projects project-id
      {
        owner: (unwrap-panic user-id),
        title: title,
        description: description,
        budget: budget,
        status: "open"
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

;; Submit a bid for a project
(define-public (submit-bid (project-id uint) (amount uint) (proposal (string-ascii 500)))
  (let
    (
      (bid-id (var-get next-bid-id))
      (user-id (get-user-id tx-sender))
    )
    (asserts! (is-some user-id) err-not-found)
    (asserts! (is-some (map-get? projects project-id)) err-not-found)
    (map-set bids bid-id
      {
        project-id: project-id,
        bidder: (unwrap-panic user-id),
        amount: amount,
        proposal: proposal
      }
    )
    (var-set next-bid-id (+ bid-id u1))
    (ok bid-id)
  )
)

;; Accept a bid and create a contract
(define-public (accept-bid (bid-id uint))
  (let
    (
      (bid (unwrap! (map-get? bids bid-id) err-not-found))
      (project (unwrap! (map-get? projects (get project-id bid)) err-not-found))
      (contract-id (var-get next-contract-id))
      (user-id (get-user-id tx-sender))
    )
    (asserts! (is-some user-id) err-not-found)
    (asserts! (is-eq (unwrap-panic user-id) (get owner project)) err-unauthorized)
    (map-set contracts contract-id
      {
        project-id: (get project-id bid),
        freelancer: (get bidder bid),
        client: (get owner project),
        amount: (get amount bid),
        status: "active"
      }
    )
    (map-set projects (get project-id bid)
      (merge project { status: "in-progress" })
    )
    (var-set next-contract-id (+ contract-id u1))
    (ok contract-id)
  )
)

;; Complete a contract and transfer payment
(define-public (complete-contract (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? contracts contract-id) err-not-found))
      (user-id (get-user-id tx-sender))
      (freelancer-address (unwrap! (get-user-address (get freelancer contract)) err-not-found))
    )
    (asserts! (is-some user-id) err-not-found)
    (asserts! (is-eq (unwrap-panic user-id) (get client contract)) err-unauthorized)
    (asserts! (is-eq (get status contract) "active") err-unauthorized)
    (try! (as-contract (ft-transfer? neurogig-token (get amount contract) tx-sender freelancer-address)))
    (map-set contracts contract-id
      (merge contract { status: "completed" })
    )
    (map-set projects (get project-id contract)
      (merge (unwrap-panic (map-get? projects (get project-id contract))) { status: "completed" })
    )
    (ok true)
  )
)

;; Helper function to get user ID from principal
(define-private (get-user-id (user principal))
  (map-get? user-addresses user)
)

;; Helper function to get user address (principal) from user ID
(define-private (get-user-address (user-id uint))
  (match (map-get? users user-id)
    user (some (get address user))
    none
  )
)

;; Initialize the contract
(begin
  (try! (ft-mint? neurogig-token u1000000000 contract-owner))
)