! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     emitter.inc -------------------------------------------------------------
      INTEGER nr_source, nz_source
      REAL    mu_0, mu_width,                                           &
     &        Z_bound_min, Z_bound_max, R_bound_min, R_bound_max,       &
     &        Z_plasm_min, Z_plasm_max, R_plasm_min, R_plasm_max,       &
     &        r_source(NRDIM), z_source(NZDIM),                         &
     &        R_bound_min_sq, R_bound_max_sq
      REAL    PusherMajor, PusherMinor
      REAL    source_profile(NRDIM, NZDIM)
      COMMON /lsc_emcom1/ nr_source, nz_source
      COMMON /lsc_emcom2/ mu_0, mu_width,                                 &
     &        Z_bound_min, Z_bound_max, R_bound_min, R_bound_max,       &
     &        Z_plasm_min, Z_plasm_max, R_plasm_min, R_plasm_max,       &
     &     r_source, z_source, R_bound_min_sq,                          &
     &     R_bound_max_sq, PusherMajor, PusherMinor
      COMMON /lsc_emcom3/ source_profile
!     emitter.inc -------------------------------------------------------------
!
