
;; title: pet-shelter-donation
;; version:
;; summary:
;; description:

;; Pet Shelter Donation System

;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Define the fungible token for donations
(define-fungible-token donation-token)

;; Define the non-fungible token for donor recognition
(define-non-fungible-token donor-nft uint)

;; Counter for NFT IDs
(define-data-var nft-id-counter uint u0)

;; Map to store donation records
(define-map donation-records
  { donation-id: uint }
  { donor: principal, amount: uint, recipient-id: (string-ascii 36), timestamp: uint })

;; Map to store donor records
(define-map donor-records
  { donor: principal }
  { total-donated: uint, donation-count: uint, last-donation: uint })

;; Counter for donation IDs
(define-data-var donation-id-counter uint u0)

;; Function to make a donation
(define-public (make-donation (amount uint) (recipient-id (string-ascii 36)))
  (let
    (
      (donor tx-sender)
      (donation-id (+ (var-get donation-id-counter) u1))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (begin
      ;; Transfer donation tokens from donor to contract
      (try! (ft-transfer? donation-token amount donor (as-contract tx-sender)))
      
      ;; Record the donation
      (map-set donation-records
        { donation-id: donation-id }
        { donor: donor, amount: amount, recipient-id: recipient-id, timestamp: current-time }
      )
      
      ;; Update donor record
      (map-set donor-records
        { donor: donor }
        (merge
          (default-to
            { total-donated: u0, donation-count: u0, last-donation: u0 }
            (map-get? donor-records { donor: donor })
          )
          {
            total-donated: (+ (default-to u0 (get total-donated (map-get? donor-records { donor: donor }))) amount),
            donation-count: (+ (default-to u0 (get donation-count (map-get? donor-records { donor: donor }))) u1),
            last-donation: current-time
          }
        )
      )
      
      ;; Increment donation ID counter
      (var-set donation-id-counter donation-id)
      
      ;; Mint donor NFT if it's their first donation
      (try! (if 
        (is-eq 
        (get donation-count (unwrap! (map-get? donor-records { donor: donor }) (err u404))) u1)
        (mint-donor-nft donor)
        (ok u1)
      ))
      (ok donation-id)
    )
  )
)

;; Function to mint donor recognition NFT
(define-private (mint-donor-nft (donor principal))
  (let
    ((new-id (+ (var-get nft-id-counter) u1)))
    (begin
      (try! (nft-mint? donor-nft new-id donor))
      (var-set nft-id-counter new-id)
      (ok new-id)
    )
  )
)

;; Function to get donor record
(define-read-only (get-donor-record (donor principal))
  (default-to
    { total-donated: u0, donation-count: u0, last-donation: u0 }
    (map-get? donor-records { donor: donor })
  )
)

;; Function to get donation details
(define-read-only (get-donation-details (donation-id uint))
  (map-get? donation-records { donation-id: donation-id })
)

;; Function to check if a donor has an NFT
(define-read-only (has-donor-nft (donor principal))
  (ok (is-some (nft-get-owner? donor-nft (unwrap! (get donation-count (map-get? donor-records { donor: donor })) (err u404)))))
)

;; Initialize the contract
(begin
  ;; Mint initial supply of donation tokens to contract owner
  (try! (ft-mint? donation-token u1000000000 contract-owner))
)