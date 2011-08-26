! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     power.inc --------------------------------------------------------
!     -                                                                |
      REAL    PRay(NVELDIM, NPSIDIM), Pql(NVELDIM, NPSIDIM),            &
     &        PRaytot(NPSIDIM),       Pqltot(NPSIDIM),                  &
     &        PrIntgrl(NPSIDIM),      PqIntgrl(NPSIDIM),                &
     &        PraySum,                PqlSum,             PPwrSum
      COMMON /lsc_PwrQLCom/                                               &
     &        PRay,                   Pql,                              &
     &        PRaytot, Pqltot, PrIntgrl, PqIntgrl,                      &
     &        PraySum,                PqlSum,             PPwrSum
!     -                                                                |
!     power.inc ends ---------------------------------------------------
