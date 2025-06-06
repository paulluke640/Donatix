(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-campaign-ended (err u104))
(define-constant err-milestone-not-ready (err u105))
(define-constant err-already-voted (err u106))
(define-constant err-insufficient-funds (err u107))

(define-data-var next-campaign-id uint u1)
(define-data-var next-milestone-id uint u1)

(define-map campaigns
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    target-amount: uint,
    raised-amount: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map milestones
  uint
  {
    campaign-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 300),
    amount: uint,
    is-completed: bool,
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    is-released: bool
  }
)

(define-map donations
  {campaign-id: uint, donor: principal}
  {amount: uint, donated-at: uint}
)

(define-map milestone-votes
  {milestone-id: uint, voter: principal}
  {vote: bool, voted-at: uint}
)

(define-map campaign-milestones
  uint
  (list 20 uint)
)

(define-map escrow-balances
  uint
  uint
)

(define-public (create-campaign (title (string-ascii 100)) (description (string-ascii 500)) (target-amount uint))
  (let
    (
      (campaign-id (var-get next-campaign-id))
    )
    (asserts! (> target-amount u0) err-invalid-amount)
    (map-set campaigns campaign-id
      {
        creator: tx-sender,
        title: title,
        description: description,
        target-amount: target-amount,
        raised-amount: u0,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    (map-set escrow-balances campaign-id u0)
    (var-set next-campaign-id (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (donate (campaign-id uint) (amount uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
      (current-donation (default-to {amount: u0, donated-at: u0} (map-get? donations {campaign-id: campaign-id, donor: tx-sender})))
      (current-escrow (default-to u0 (map-get? escrow-balances campaign-id)))
    )
    (asserts! (get is-active campaign) err-campaign-ended)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set donations {campaign-id: campaign-id, donor: tx-sender}
      {
        amount: (+ (get amount current-donation) amount),
        donated-at: stacks-block-height
      }
    )
    (map-set campaigns campaign-id
      (merge campaign {raised-amount: (+ (get raised-amount campaign) amount)})
    )
    (map-set escrow-balances campaign-id (+ current-escrow amount))
    (ok true)
  )
)

(define-public (create-milestone (campaign-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (amount uint) (voting-duration uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
      (milestone-id (var-get next-milestone-id))
      (current-milestones (default-to (list) (map-get? campaign-milestones campaign-id)))
    )
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get is-active campaign) err-campaign-ended)
    (map-set milestones milestone-id
      {
        campaign-id: campaign-id,
        title: title,
        description: description,
        amount: amount,
        is-completed: false,
        votes-for: u0,
        votes-against: u0,
        voting-deadline: (+ stacks-block-height voting-duration),
        is-released: false
      }
    )
    (map-set campaign-milestones campaign-id (unwrap! (as-max-len? (append current-milestones milestone-id) u20) err-invalid-amount))
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (submit-milestone-completion (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) err-not-found))
      (campaign (unwrap! (map-get? campaigns (get campaign-id milestone)) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (asserts! (not (get is-completed milestone)) err-milestone-not-ready)
    (map-set milestones milestone-id
      (merge milestone {is-completed: true})
    )
    (ok true)
  )
)

(define-public (vote-on-milestone (milestone-id uint) (vote bool))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) err-not-found))
      (campaign (unwrap! (map-get? campaigns (get campaign-id milestone)) err-not-found))
      (voter-donation (unwrap! (map-get? donations {campaign-id: (get campaign-id milestone), donor: tx-sender}) err-unauthorized))
    )
    (asserts! (get is-completed milestone) err-milestone-not-ready)
    (asserts! (< stacks-block-height (get voting-deadline milestone)) err-campaign-ended)
    (asserts! (is-none (map-get? milestone-votes {milestone-id: milestone-id, voter: tx-sender})) err-already-voted)
    (asserts! (> (get amount voter-donation) u0) err-unauthorized)
    (map-set milestone-votes {milestone-id: milestone-id, voter: tx-sender}
      {vote: vote, voted-at: stacks-block-height}
    )
    (if vote
      (map-set milestones milestone-id
        (merge milestone {votes-for: (+ (get votes-for milestone) u1)})
      )
      (map-set milestones milestone-id
        (merge milestone {votes-against: (+ (get votes-against milestone) u1)})
      )
    )
    (ok true)
  )
)

(define-public (release-milestone-funds (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) err-not-found))
      (campaign (unwrap! (map-get? campaigns (get campaign-id milestone)) err-not-found))
      (escrow-balance (default-to u0 (map-get? escrow-balances (get campaign-id milestone))))
    )
    (asserts! (get is-completed milestone) err-milestone-not-ready)
    (asserts! (>= stacks-block-height (get voting-deadline milestone)) err-milestone-not-ready)
    (asserts! (> (get votes-for milestone) (get votes-against milestone)) err-unauthorized)
    (asserts! (not (get is-released milestone)) err-milestone-not-ready)
    (asserts! (>= escrow-balance (get amount milestone)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get creator campaign))))
    (map-set milestones milestone-id
      (merge milestone {is-released: true})
    )
    (map-set escrow-balances (get campaign-id milestone) (- escrow-balance (get amount milestone)))
    (ok true)
  )
)

(define-public (close-campaign (campaign-id uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (map-set campaigns campaign-id
      (merge campaign {is-active: false})
    )
    (ok true)
  )
)

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns campaign-id)
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones milestone-id)
)

(define-read-only (get-donation (campaign-id uint) (donor principal))
  (map-get? donations {campaign-id: campaign-id, donor: donor})
)

(define-read-only (get-milestone-vote (milestone-id uint) (voter principal))
  (map-get? milestone-votes {milestone-id: milestone-id, voter: voter})
)

(define-read-only (get-campaign-milestones (campaign-id uint))
  (map-get? campaign-milestones campaign-id)
)

(define-read-only (get-escrow-balance (campaign-id uint))
  (map-get? escrow-balances campaign-id)
)

(define-read-only (get-next-campaign-id)
  (var-get next-campaign-id)
)

(define-read-only (get-next-milestone-id)
  (var-get next-milestone-id)
)