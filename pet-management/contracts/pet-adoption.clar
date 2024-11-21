
;; title: pet-adoption
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;; Constants for adoption status
(define-constant STATUS-PENDING u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-REJECTED u3)

;; Error constants
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-ALREADY-APPLIED (err u402))
(define-constant ERR-PET-NOT-AVAILABLE (err u403))
(define-constant ERR-INVALID-STATUS (err u405))

;; data vars
;; Track application counter
(define-data-var application-counter uint u0)


;; data maps
;; Define data variables for pet registration
(define-map pets
    { owner: principal, pet-id: uint }
    {
        name: (string-utf8 100),
        breed: (string-utf8 100),
        age: uint,
        registration-date: uint
    }
)

;; Define data structures
(define-map adoption-applications
    { application-id: uint }
    {
        pet-id: uint,
        pet-owner: principal,
        applicant: principal,
        status: uint,
        application-date: uint,
        review-date: (optional uint)
    }
)

;; Track pets that are available for adoption
(define-map pets-for-adoption
    { pet-id: uint }
    { 
        is-available: bool,
        listing-date: uint,
        price: uint
    }
)
;; public functions
;; Get the next application ID
(define-private (get-next-application-id)
    (begin
        (var-set application-counter (+ (var-get application-counter) u1))
        (var-get application-counter)
    )
)

;; List a pet for adoption
(define-public (list-pet-for-adoption (pet-id uint) (price uint))
    (let
        (
            (owner tx-sender)
        )
        ;; Verify pet ownership using the check-pet-ownership function
        (asserts! (check-pet-ownership owner pet-id) ERR-NOT-FOUND)
        (ok (map-set pets-for-adoption
            { pet-id: pet-id }
            {
                is-available: true,
                listing-date: block-height,
                price: price
            }
        ))
    )
)

;; Apply for adoption
(define-public (apply-for-adoption (pet-id uint))
    (let
        (
            (applicant tx-sender)
            (application-id (get-next-application-id))
            (pet-listing (map-get? pets-for-adoption { pet-id: pet-id }))
        )
        ;; Check if pet is available for adoption
        (asserts! (is-some pet-listing) ERR-PET-NOT-AVAILABLE)
        (asserts! (get is-available (unwrap! pet-listing ERR-PET-NOT-AVAILABLE)) ERR-PET-NOT-AVAILABLE)
        
        ;; Create new application
        (ok (map-set adoption-applications
            { application-id: application-id }
            {
                pet-id: pet-id,
                pet-owner: tx-sender,
                applicant: applicant,
                status: STATUS-PENDING,
                application-date: block-height,
                review-date: none
            }
        ))
    )
)

;; Approve or reject adoption application
(define-public (process-adoption-application (application-id uint) (new-status uint))
    (let
        (
            (application (map-get? adoption-applications { application-id: application-id }))
            (current-owner tx-sender)
        )
        ;; Verify application exists
        (asserts! (is-some application) ERR-NOT-FOUND)
        
        ;; Verify the processor is the pet owner
        (asserts! (is-eq current-owner (get pet-owner (unwrap! application ERR-NOT-FOUND))) ERR-UNAUTHORIZED)
        
        ;; Verify valid status
        (asserts! (or (is-eq new-status STATUS-APPROVED) (is-eq new-status STATUS-REJECTED)) ERR-INVALID-STATUS)
        
        ;; Update application status
        (ok (map-set adoption-applications
            { application-id: application-id }
            (merge (unwrap! application ERR-NOT-FOUND)
                {
                    status: new-status,
                    review-date: (some block-height)
                }
            )
        ))
    )
)

;; read only functions
;; Helper function to check if a pet exists and belongs to the owner
(define-read-only (check-pet-ownership (owner principal) (pet-id uint))
    (is-some (map-get? pets { owner: owner, pet-id: pet-id }))
)


;; private functions
;; Get adoption application status
(define-read-only (get-adoption-status (application-id uint))
    (match (map-get? adoption-applications { application-id: application-id })
        application (ok application)
        ERR-NOT-FOUND
    )
)

;; Check if pet is available for adoption
(define-read-only (is-pet-available (pet-id uint))
    (match (map-get? pets-for-adoption { pet-id: pet-id })
        listing (get is-available listing)
        false
    )
)

;; Get pet information
(define-read-only (get-pet-info (owner principal) (pet-id uint))
    (match (map-get? pets { owner: owner, pet-id: pet-id })
        pet-data (ok pet-data)
        ERR-NOT-FOUND
    )
)