;; ---------------------------------------------------------
;; governance-proposal.clar
;; Generic DAO Proposal & Voting System
;; ---------------------------------------------------------

(define-constant ERR-NOT-MEMBER u100)
(define-constant ERR-ALREADY-VOTED u101)
(define-constant ERR-PROPOSAL-NOT-FOUND u102)
(define-constant ERR-PROPOSAL-CLOSED u103)
(define-constant ERR-NOT-AUTHORIZED u104)

(define-constant MIN-QUORUM u10)        ;; minimum total votes needed
(define-constant APPROVAL-RATIO u60)    ;; % yes needed (e.g., 60%)

;; ---------------------------------------------------------
;; Data Structures
;; ---------------------------------------------------------

(define-data-var proposal-counter uint u0)

(define-map proposals
  { id: uint }
  {
    creator: principal,
    title: (string-ascii 64),
    description: (string-ascii 256),
    yes-votes: uint,
    no-votes: uint,
    is-open: bool
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { voted: bool })

(define-map members
  { member: principal }
  { active: bool })

;; ---------------------------------------------------------
;; Admin: Register DAO members
;; ---------------------------------------------------------

(define-public (add-member (user principal))
  (begin
    (asserts! (is-some (some user)) (err u1))
    (map-set members { member: user } { active: true })
    (ok true)
  )
)

(define-read-only (is-member (user principal))
  (default-to false (get active (map-get? members { member: user })))
)

;; ---------------------------------------------------------
;; Create Proposal
;; ---------------------------------------------------------

(define-public (create-proposal (title (string-ascii 64)) (description (string-ascii 256)))
  (begin
    (if (not (is-member tx-sender))
        (err ERR-NOT-MEMBER)
        (begin
          (var-set proposal-counter (+ (var-get proposal-counter) u1))
          (let ((id (var-get proposal-counter)))
            (asserts! (and (is-some (some title)) (is-some (some description))) (err u1))
            (map-set proposals
              { id: id }
              {
                creator: tx-sender,
                title: title,
                description: description,
                yes-votes: u0,
                no-votes: u0,
                is-open: true
              }
            )
            (ok id)
          )
        )
    )
  )
)

;; ---------------------------------------------------------
;; Cast Vote
;; ---------------------------------------------------------

(define-public (vote (proposal-id uint) (support bool))
  (let (
        (proposal (map-get? proposals { id: proposal-id }))
       )
    (if (is-none proposal)
        (err ERR-PROPOSAL-NOT-FOUND)
        (let (
              (data (unwrap! proposal (err ERR-PROPOSAL-NOT-FOUND)))
             )
          (if (not (get is-open data))
              (err ERR-PROPOSAL-CLOSED)
              (if (not (is-member tx-sender))
                  (err ERR-NOT-MEMBER)
                  (let (
                        (past-vote (map-get? votes { proposal-id: proposal-id, voter: tx-sender }))
                       )
                    (if (is-some past-vote)
                        (err ERR-ALREADY-VOTED)
                        (begin
                          ;; Record vote
                          (asserts! (is-some (some proposal-id)) (err u1))
                          (map-set votes { proposal-id: proposal-id, voter: tx-sender } { voted: true })

                          ;; Update counters
                          (if support
                              (map-set proposals { id: proposal-id }
                                (merge data { yes-votes: (+ (get yes-votes data) u1) }))
                              (map-set proposals { id: proposal-id }
                                (merge data { no-votes: (+ (get no-votes data) u1) }))
                          )
                          (ok true)
                        )
                    )
                  )
              )
          )
        )
    )
  )
)

;; ---------------------------------------------------------
;; Finalize Proposal
;; Anyone can call this.
;; ---------------------------------------------------------

(define-public (finalize (proposal-id uint))
  (let ((proposal (map-get? proposals { id: proposal-id })))
    (if (is-none proposal)
        (err ERR-PROPOSAL-NOT-FOUND)
        (let (
              (data (unwrap! proposal (err ERR-PROPOSAL-NOT-FOUND)))
              (yes (get yes-votes (unwrap! proposal (err u0))))
              (no (get no-votes (unwrap! proposal (err u0))))
             )
          (if (not (get is-open data))
              (err ERR-PROPOSAL-CLOSED)
              (let ((total (+ yes no)))
                (asserts! (is-some (some proposal-id)) (err u1))
                (if (< total MIN-QUORUM)
                    (begin
                      (map-set proposals { id: proposal-id } (merge data { is-open: false }))
                      (ok { status: "failed-quorum", yes: yes, no: no })
                    )
                    (let (
                          (percent (* (/ yes total) u100))
                         )
                      (map-set proposals { id: proposal-id } (merge data { is-open: false }))
                      (if (>= percent APPROVAL-RATIO)
                          (ok { status: "approved", yes: yes, no: no })
                          (ok { status: "rejected", yes: yes, no: no })
                      )
                    )
                )
              )
          )
        )
    )
  )
)

;; ---------------------------------------------------------
;; Read-only views
;; ---------------------------------------------------------

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { id: proposal-id })
)

(define-read-only (has-voted (proposal-id uint) (user principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: user }))
)
