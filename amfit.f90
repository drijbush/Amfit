Program amfit

  Use, Intrinsic :: iso_fortran_env, Only:  real64

  Implicit None

  Integer, Parameter :: wp = real64

  Real( wp ), Dimension( : ), Allocatable :: t, P

  Real( wp ) :: sum_inv_P, sum_inv_P_sq
  Real( wp ) :: sum_t, sum_t_over_P
  Real( wp ) :: serial, parallel
  Real( wp ) :: t_1, t_P, speed_up
  Real( wp ) :: denom, numer
  
  Integer :: n
  Integer :: i

  Write( *, '( "N points?" )' )
  Read ( *, * ) n

  Allocate( t( 1:n ) )
  Allocate( P( 1:n ) )

  Do i = 1, n
     Write( *, '( "Point ", i0, " cores, time?" )' ) i
     Read ( *, * ) P( i ), t( i )
  End Do

  sum_inv_P    = Sum( 1.0_wp / P )
  sum_inv_P_sq = Sum( 1.0_wp / ( P * P ) )

  sum_t        = Sum( t )
  sum_t_over_P = Sum( t / P )

  denom = sum_inv_P * sum_inv_P_sq - n * sum_inv_P_sq
  numer = sum_t * sum_inv_P_sq - n * sum_t_over_P

  parallel = numer / denom
  serial   = ( sum_t - parallel * sum_inv_p ) / n

  Write( *, * ) serial, parallel
  t_1 = serial + parallel
  Do i = 1, n
     t_P      =  serial + parallel / P( i )
     speed_up = t_1 / t( i )
     Write( *, '( i0, t10, f0.3, t25, f0.3, t40, f0.3 )' ) &
          Int( P( i ) ), t( i ), t_P, speed_up
  End Do
  
End Program amfit
