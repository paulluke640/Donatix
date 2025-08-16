(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-campaign-ended (err u104))
(define-constant err-milestone-not-ready (err u105))
(define-constant err-already-voted (err u106))
(define-constant err-insufficient-funds (err u107))
(define-constant err-campaign-still-active (err u108))
(define-constant err-no-refund-available (err u109))
(define-constant err-refund-not-allowed (err u110))
(define-constant err-already-refunded (err u111))
(define-constant err-update-not-found (err u112))
(define-constant err-comment-not-found (err u113))
(define-constant err-invalid-update-type (err u114))
(define-constant err-comment-moderated (err u115))
(define-constant err-update-limit-reached (err u116))

(define-data-var next-campaign-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var refund-window-blocks uint u4320)
(define-data-var next-update-id uint u1)
(define-data-var next-comment-id uint u1)

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

(define-map refund-requests
  {campaign-id: uint, donor: principal}
  {
    amount: uint,
    requested-at: uint,
    is-processed: bool,
    reason: uint
  }
)

(define-map donor-refund-status
  {campaign-id: uint, donor: principal}
  {
    total-donated: uint,
    total-refunded: uint,
    last-refund-at: uint
  }
)

;; Campaign updates for transparent communication
(define-map campaign-updates
  uint
  {
    campaign-id: uint,
    creator: principal,
    title: (string-ascii 100),
    content: (string-ascii 1000),
    update-type: uint, ;; 1=general, 2=financial, 3=progress, 4=announcement
    is-public: bool,
    created-at: uint,
    is-pinned: bool
  }
)

;; Donor comments on campaigns
(define-map campaign-comments
  uint
  {
    campaign-id: uint,
    commenter: principal,
    content: (string-ascii 500),
    created-at: uint,
    is-moderated: bool,
    reply-to: (optional uint)
  }
)

;; Track update count per campaign for limits
(define-map campaign-update-count
  uint
  uint
)

;; Track comment count per campaign
(define-map campaign-comment-count
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
    (let
      (
        (current-status (default-to {total-donated: u0, total-refunded: u0, last-refund-at: u0} (map-get? donor-refund-status {campaign-id: campaign-id, donor: tx-sender})))
      )
      (map-set donor-refund-status {campaign-id: campaign-id, donor: tx-sender}
        (merge current-status {total-donated: (+ (get total-donated current-status) amount)})
      )
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

(define-public (request-refund (campaign-id uint) (reason uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
      (donation (unwrap! (map-get? donations {campaign-id: campaign-id, donor: tx-sender}) err-not-found))
      (refund-status (default-to {total-donated: u0, total-refunded: u0, last-refund-at: u0} (map-get? donor-refund-status {campaign-id: campaign-id, donor: tx-sender})))
      (existing-request (map-get? refund-requests {campaign-id: campaign-id, donor: tx-sender}))
      (refundable-amount (- (get total-donated refund-status) (get total-refunded refund-status)))
    )
    (asserts! (is-none existing-request) err-already-refunded)
    (asserts! (> refundable-amount u0) err-no-refund-available)
    (asserts! (or (is-eq reason u1) (is-eq reason u2) (is-eq reason u3)) err-invalid-amount)
    (asserts! (or 
      (and (is-eq reason u1) (not (get is-active campaign)))
      (and (is-eq reason u2) (> (- stacks-block-height (get created-at campaign)) (var-get refund-window-blocks)))
      (and (is-eq reason u3) (< (get raised-amount campaign) (/ (get target-amount campaign) u2)))
    ) err-refund-not-allowed)
    (map-set refund-requests {campaign-id: campaign-id, donor: tx-sender}
      {
        amount: refundable-amount,
        requested-at: stacks-block-height,
        is-processed: false,
        reason: reason
      }
    )
    (ok refundable-amount)
  )
)

(define-public (process-refund (campaign-id uint) (donor principal))
  (let
    (
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
      (refund-request (unwrap! (map-get? refund-requests {campaign-id: campaign-id, donor: donor}) err-not-found))
      (escrow-balance (default-to u0 (map-get? escrow-balances campaign-id)))
      (refund-status (default-to {total-donated: u0, total-refunded: u0, last-refund-at: u0} (map-get? donor-refund-status {campaign-id: campaign-id, donor: donor})))
      (refund-amount (get amount refund-request))
    )
    (asserts! (not (get is-processed refund-request)) err-already-refunded)
    (asserts! (>= escrow-balance refund-amount) err-insufficient-funds)
    (asserts! (or 
      (is-eq tx-sender (get creator campaign))
      (is-eq tx-sender donor)
      (is-eq tx-sender contract-owner)
    ) err-unauthorized)
    (try! (as-contract (stx-transfer? refund-amount tx-sender donor)))
    (map-set refund-requests {campaign-id: campaign-id, donor: donor}
      (merge refund-request {is-processed: true})
    )
    (map-set escrow-balances campaign-id (- escrow-balance refund-amount))
    (map-set donor-refund-status {campaign-id: campaign-id, donor: donor}
      (merge refund-status {
        total-refunded: (+ (get total-refunded refund-status) refund-amount),
        last-refund-at: stacks-block-height
      })
    )
    (map-set campaigns campaign-id
      (merge campaign {raised-amount: (- (get raised-amount campaign) refund-amount)})
    )
    (ok refund-amount)
  )
)

(define-public (cancel-refund-request (campaign-id uint))
  (let
    (
      (refund-request (unwrap! (map-get? refund-requests {campaign-id: campaign-id, donor: tx-sender}) err-not-found))
    )
    (asserts! (not (get is-processed refund-request)) err-already-refunded)
    (map-delete refund-requests {campaign-id: campaign-id, donor: tx-sender})
    (ok true)
  )
)

(define-public (batch-process-refunds (refund-list (list 10 {campaign-id: uint, donor: principal})))
  (let
    (
      (results (map process-single-refund refund-list))
    )
    (ok results)
  )
)

(define-private (process-single-refund (refund-data {campaign-id: uint, donor: principal}))
  (let
    (
      (campaign-id (get campaign-id refund-data))
      (donor (get donor refund-data))
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
      (refund-request (unwrap! (map-get? refund-requests {campaign-id: campaign-id, donor: donor}) err-not-found))
      (escrow-balance (default-to u0 (map-get? escrow-balances campaign-id)))
      (refund-status (default-to {total-donated: u0, total-refunded: u0, last-refund-at: u0} (map-get? donor-refund-status {campaign-id: campaign-id, donor: donor})))
      (refund-amount (get amount refund-request))
    )
    (if (and 
      (not (get is-processed refund-request))
      (>= escrow-balance refund-amount)
    )
      (begin
        (try! (as-contract (stx-transfer? refund-amount tx-sender donor)))
        (map-set refund-requests {campaign-id: campaign-id, donor: donor}
          (merge refund-request {is-processed: true})
        )
        (map-set escrow-balances campaign-id (- escrow-balance refund-amount))
        (map-set donor-refund-status {campaign-id: campaign-id, donor: donor}
          (merge refund-status {
            total-refunded: (+ (get total-refunded refund-status) refund-amount),
            last-refund-at: stacks-block-height
          })
        )
        (map-set campaigns campaign-id
          (merge campaign {raised-amount: (- (get raised-amount campaign) refund-amount)})
        )
        (ok refund-amount)
      )
      err-insufficient-funds
    )
  )
)

(define-public (emergency-refund-all (campaign-id uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get is-active campaign)) err-campaign-still-active)
    (map-set campaigns campaign-id
      (merge campaign {is-active: false})
    )
    (ok true)
  )
)

(define-public (set-refund-window (new-window uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-window u0) err-invalid-amount)
    (var-set refund-window-blocks new-window)
    (ok new-window)
  )
)

(define-read-only (get-refund-eligibility (campaign-id uint) (donor principal))
  (let
    (
      (campaign (map-get? campaigns campaign-id))
      (donation (map-get? donations {campaign-id: campaign-id, donor: donor}))
      (refund-status (default-to {total-donated: u0, total-refunded: u0, last-refund-at: u0} (map-get? donor-refund-status {campaign-id: campaign-id, donor: donor})))
      (existing-request (map-get? refund-requests {campaign-id: campaign-id, donor: donor}))
    )
    (match campaign
      campaign-data
        (match donation
          donation-data
            (let
              (
                (refundable-amount (- (get total-donated refund-status) (get total-refunded refund-status)))
                (is-inactive (not (get is-active campaign-data)))
                (is-stale (> (- stacks-block-height (get created-at campaign-data)) (var-get refund-window-blocks)))
                (is-underfunded (< (get raised-amount campaign-data) (/ (get target-amount campaign-data) u2)))
              )
              (some {
                refundable-amount: refundable-amount,
                can-refund-inactive: is-inactive,
                can-refund-stale: is-stale,
                can-refund-underfunded: is-underfunded,
                has-pending-request: (is-some existing-request)
              })
            )
          none
        )
      none
    )
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

(define-read-only (get-refund-request (campaign-id uint) (donor principal))
  (map-get? refund-requests {campaign-id: campaign-id, donor: donor})
)

(define-read-only (get-donor-refund-status (campaign-id uint) (donor principal))
  (map-get? donor-refund-status {campaign-id: campaign-id, donor: donor})
)

(define-read-only (get-refund-window)
  (var-get refund-window-blocks)
)

;; Campaign update functions for transparent communication
(define-public (post-campaign-update (campaign-id uint) (title (string-ascii 100)) (content (string-ascii 1000)) (update-type uint) (is-public bool))
  (let
    (
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
      (update-id (var-get next-update-id))
      (current-count (default-to u0 (map-get? campaign-update-count campaign-id)))
    )
    ;; Only campaign creator can post updates
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    ;; Validate update type (1-4)
    (asserts! (and (>= update-type u1) (<= update-type u4)) err-invalid-update-type)
    ;; Limit updates per campaign (max 50)
    (asserts! (< current-count u50) err-update-limit-reached)
    ;; Create the update
    (map-set campaign-updates update-id
      {
        campaign-id: campaign-id,
        creator: tx-sender,
        title: title,
        content: content,
        update-type: update-type,
        is-public: is-public,
        created-at: stacks-block-height,
        is-pinned: false
      }
    )
    ;; Update counters
    (map-set campaign-update-count campaign-id (+ current-count u1))
    (var-set next-update-id (+ update-id u1))
    (ok update-id)
  )
)

;; Allow donors to comment on campaigns
(define-public (post-campaign-comment (campaign-id uint) (content (string-ascii 500)) (reply-to (optional uint)))
  (let
    (
      (campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
      (donation (unwrap! (map-get? donations {campaign-id: campaign-id, donor: tx-sender}) err-unauthorized))
      (comment-id (var-get next-comment-id))
      (current-count (default-to u0 (map-get? campaign-comment-count campaign-id)))
    )
    ;; Only donors can comment
    (asserts! (> (get amount donation) u0) err-unauthorized)
    ;; Validate reply-to comment exists if provided
    (match reply-to
      reply-id 
        (asserts! (is-some (map-get? campaign-comments reply-id)) err-comment-not-found)
      true
    )
    ;; Create the comment
    (map-set campaign-comments comment-id
      {
        campaign-id: campaign-id,
        commenter: tx-sender,
        content: content,
        created-at: stacks-block-height,
        is-moderated: false,
        reply-to: reply-to
      }
    )
    ;; Update counters
    (map-set campaign-comment-count campaign-id (+ current-count u1))
    (var-set next-comment-id (+ comment-id u1))
    (ok comment-id)
  )
)

;; Pin/unpin important updates
(define-public (toggle-update-pin (update-id uint))
  (let
    (
      (update (unwrap! (map-get? campaign-updates update-id) err-update-not-found))
      (campaign (unwrap! (map-get? campaigns (get campaign-id update)) err-not-found))
    )
    ;; Only campaign creator can pin updates
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    ;; Toggle pin status
    (map-set campaign-updates update-id
      (merge update {is-pinned: (not (get is-pinned update))})
    )
    (ok (not (get is-pinned update)))
  )
)

;; Moderate comments (hide inappropriate content)
(define-public (moderate-comment (comment-id uint) (moderate bool))
  (let
    (
      (comment (unwrap! (map-get? campaign-comments comment-id) err-comment-not-found))
      (campaign (unwrap! (map-get? campaigns (get campaign-id comment)) err-not-found))
    )
    ;; Only campaign creator or contract owner can moderate
    (asserts! (or 
      (is-eq tx-sender (get creator campaign))
      (is-eq tx-sender contract-owner)
    ) err-unauthorized)
    ;; Update moderation status
    (map-set campaign-comments comment-id
      (merge comment {is-moderated: moderate})
    )
    (ok moderate)
  )
)

;; Delete update (emergency only)
(define-public (delete-campaign-update (update-id uint))
  (let
    (
      (update (unwrap! (map-get? campaign-updates update-id) err-update-not-found))
      (campaign (unwrap! (map-get? campaigns (get campaign-id update)) err-not-found))
    )
    ;; Only campaign creator or contract owner can delete
    (asserts! (or
      (is-eq tx-sender (get creator campaign))
      (is-eq tx-sender contract-owner)
    ) err-unauthorized)
    ;; Remove the update
    (map-delete campaign-updates update-id)
    ;; Decrease counter
    (let
      (
        (current-count (default-to u0 (map-get? campaign-update-count (get campaign-id update))))
      )
      (if (> current-count u0)
        (map-set campaign-update-count (get campaign-id update) (- current-count u1))
        true
      )
    )
    (ok true)
  )
)

;; Get campaign updates with filtering
(define-read-only (get-campaign-update (update-id uint))
  (map-get? campaign-updates update-id)
)

;; Get campaign comment
(define-read-only (get-campaign-comment (comment-id uint))
  (map-get? campaign-comments comment-id)
)

;; Get update count for campaign
(define-read-only (get-campaign-update-count (campaign-id uint))
  (default-to u0 (map-get? campaign-update-count campaign-id))
)

;; Get comment count for campaign
(define-read-only (get-campaign-comment-count (campaign-id uint))
  (default-to u0 (map-get? campaign-comment-count campaign-id))
)

;; Check if user can comment on campaign
(define-read-only (can-comment-on-campaign (campaign-id uint) (user principal))
  (let
    (
      (donation (map-get? donations {campaign-id: campaign-id, donor: user}))
    )
    (match donation
      d (> (get amount d) u0)
      false
    )
  )
)

;; Get next IDs for updates and comments
(define-read-only (get-next-update-id)
  (var-get next-update-id)
)

(define-read-only (get-next-comment-id)
  (var-get next-comment-id)
)

