;; SmartDAO Robust Governance Contract
(define-constant CONTRACT-OWNER tx-sender)

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-TOKENS (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))

;; DAO Token with Fixed Supply
(define-fungible-token dao-token u1000000)

;; Proposal Status Types
(define-constant PROPOSAL-STATUS-PENDING "pending")
(define-constant PROPOSAL-STATUS-ACTIVE "active")
(define-constant PROPOSAL-STATUS-PASSED "passed")
(define-constant PROPOSAL-STATUS-REJECTED "rejected")

;; Proposal Structure Map
(define-map proposals
    {proposal-id: uint}
    {
        proposer: principal,
        description: (string-utf8 500),
        votes-for: uint,
        votes-against: uint,
        total-voting-power: uint,
        status: (string-utf8 20),
        creation-block: uint,
        voting-deadline: uint
    }
)

;; Voter Tracking
(define-map votes-cast 
    {proposal-id: uint, voter: principal}
    bool
)

;; Proposal Tracking
(define-data-var next-proposal-id uint u0)
(define-data-var voting-period uint u1000) ;; Blocks for voting

;; Token Minting (Initial Distribution)
(define-public (mint-tokens (amount uint) (recipient principal))
    (ft-mint? dao-token amount recipient)
)

;; Create Proposal with Enhanced Validation
(define-public (create-proposal 
    (description (string-utf8 500))
    (voting-duration uint)
)
    (let (
        (token-balance (ft-get-balance dao-token tx-sender))
        (current-proposal-id (+ (var-get next-proposal-id) u1))
        (current-block-height (unwrap! (get-block-info? "block-height") ERR-BLOCK-INFO))
        )
        ;; Require minimum token balance
        (asserts! (>= token-balance u100) ERR-INSUFFICIENT-TOKENS)
    
        ;; Validate voting duration
        (asserts! (and (>= voting-duration u100) (<= voting-duration u10000)) ERR-NOT-AUTHORIZED)
    
        ;; Insert Proposal
        (map-set proposals 
            {proposal-id: current-proposal-id}
            {
                proposer: tx-sender,
                description: description,
                votes-for: u0,
                votes-against: u0,
                total-voting-power: u0,
                status: PROPOSAL-STATUS-ACTIVE,
                creation-block: current-block-height,
                voting-deadline: (+ current-block-height voting-duration)
            }
        )
    
        ;; Update Proposal ID
        (var-set next-proposal-id current-proposal-id)
    
        (ok current-proposal-id)
    )
)

;; Enhanced Voting Mechanism
(define-public (vote-on-proposal 
  (proposal-id uint)
  (vote-direction bool)
)
  (let (
    (token-balance (ft-get-balance dao-token tx-sender))
    (current-proposal (unwrap! 
      (map-get? proposals {proposal-id: proposal-id}) 
      ERR-PROPOSAL-NOT-FOUND
    ))
    (already-voted (default-to false (map-get? votes-cast {proposal-id: proposal-id, voter: tx-sender})))
  )
    ;; Validation Checks
    (asserts! (not already-voted) ERR-ALREADY-VOTED)
    (asserts! (< block-height (get voting-deadline current-proposal)) ERR-VOTING-CLOSED)
    
    ;; Record Vote
    (map-set votes-cast 
      {proposal-id: proposal-id, voter: tx-sender} 
      true
    )
    
    ;; Update Proposal Votes
    (if vote-direction
      (map-set proposals 
        {proposal-id: proposal-id}
        (merge current-proposal {
          votes-for: (+ (get votes-for current-proposal) token-balance),
          total-voting-power: (+ (get total-voting-power current-proposal) token-balance)
        })
      )
      (map-set proposals 
        {proposal-id: proposal-id}
        (merge current-proposal {
          votes-against: (+ (get votes-against current-proposal) token-balance),
          total-voting-power: (+ (get total-voting-power current-proposal) token-balance)
        })
      )
    )
    
    (ok true)
  )
)

;; Finalize Proposal
(define-public (finalize-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! 
      (map-get? proposals {proposal-id: proposal-id}) 
      ERR-PROPOSAL-NOT-FOUND
    ))
  )
    ;; Ensure voting period has ended
    (asserts! (>= block-height (get voting-deadline proposal)) ERR-NOT-AUTHORIZED)
    
    ;; Determine Proposal Outcome
    (let ((final-status 
      (if (> (get votes-for proposal) (get votes-against proposal))
        PROPOSAL-STATUS-PASSED
        PROPOSAL-STATUS-REJECTED
      ))
    )
      (map-set proposals 
        {proposal-id: proposal-id}
        (merge proposal {status: final-status})
      )
      
      (ok final-status)
    )
  )
)

;; Query Proposal Details
(define-read-only (get-proposal-details (proposal-id uint))
    (map-get? proposals {proposal-id: proposal-id})
)