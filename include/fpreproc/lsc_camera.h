! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     camera.inc --------------------------------------------------------------
      INTEGER n_pixel(SPACEDIM - 1)
      INTEGER n_pixel_x, n_pixel_y
!     INTEGER*4 npoints(NPIXDIM, NPIXDIM)
      INTEGER   npoints(NPIXDIM, NPIXDIM)
      REAL    pinhole_loc(SPACEDIM), camera_orientation(2),             &
     &        focal_length, screen_d,                                   &
     &        pinhole_size, pixel_size(SPACEDIM - 1),                   &
     &        R_tangent, z_tangent, RcntChrd,                           &
     &        mu_axis, phi_axis, Rpinhole,                              &
     &        Zpinhole, phi_pinhole, pix_size_x, pix_size_y,            &
     &        y_hat_camera(SPACEDIM, NPIXDIM, NPIXDIM),                 &
     &        chord_origin(SPACEDIM, NPIXDIM, NPIXDIM),                 &
     &        x_pix_vec(NPIXDIM), y_pix_vec(NPIXDIM),                   &
     &        r_tangent_actual(NPIXDIM, NPIXDIM),                       &
     &        z_tangent_actual(NPIXDIM, NPIXDIM)
!     REAL*4 chord(3, MAXPOINTS, NPIXDIM, NPIXDIM)
      REAL   chord(3, MAXPOINTS, NPIXDIM, NPIXDIM)
!     REAL*4 photon_count(NPIXDIM, NPIXDIM)
      REAL   photon_count(NPIXDIM, NPIXDIM)
      EQUIVALENCE (n_pixel(1), n_pixel_x), (n_pixel(2), n_pixel_y)
      EQUIVALENCE (camera_orientation(1), mu_axis),                     &
     &     (camera_orientation(2), phi_axis)
      EQUIVALENCE (pinhole_loc(1), Rpinhole),                           &
     &     (pinhole_loc(2), Zpinhole),                                  &
     &     (pinhole_loc(3), phi_pinhole),                               &
     &     (pixel_size(1), pix_size_x), (pixel_size(2), pix_size_y)
      COMMON /lsc_camcom0/ n_pixel, npoints
      COMMON /lsc_camcom1/ pinhole_loc, camera_orientation,               &
     &     focal_length, screen_d,                                      &
     &     pinhole_size, pixel_size, R_tangent, z_tangent, RcntChrd,    &
     &     y_hat_camera, x_pix_vec, y_pix_vec, chord_origin,            &
     &     r_tangent_actual, z_tangent_actual
      COMMON /lsc_camcom2/ chord, photon_count
!     camera.inc --------------------------------------------------------------
!
