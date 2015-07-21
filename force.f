/*
 * force.f
 *
 *  Created on: Jul 13, 2015
 *      Author: Muhammad Salem
 */


subroutine gfat(n_c,n,n3,type,r,a,b,c,alpha,beta,gamma,
     +    length,beta_p,lo,hd2,gd2,sd2,k_e,k_ex,Nq,Nq3,core_pos,core_q,
     +    debye,q_l,phi_o,E,t_n,t_chg,t_X, t_Y, t_Z,t_X0, t_Y0, t_Z0,
     +    h_X,h_Y,h_Z,h_chg,h_n,evd_hcG,evd_hcC,evd_hlG,evd_hlC,
     +    t_bond, t_bond_v, t_bond_c, t_angle, t_angle_v, t_angle_c,
     +    t_exv_e, t_exv_d, t_Eb, t_Ea, t_Ec, t_Ev,t_grp, t_fix,
     +    vdw_cut, Rcut, evd_tc, evd_tl, evd_cc, evd_cl, evd_ll,
     +    Ec_LL, Ec_CC, Ec_TT, Ec_LC, Ec_CT, Ec_TL,Ec_TT1,
     +    Ec_TT2, Ec_CT1, Ec_CT2, Ec_HH, Ec_TH, Ec_HC, Ec_HL1,
     +    Ec_HL2, h_tc, Eb_tc, withlink, evd_link, evd_hh,
     +    np,myid,ierr)
     !Starting by defining the used parameters
     integer n_c,nc3,n,n3, type(n)
     double precision r(n3), a(n3),b(n3),c(n3)
     double precision alpha(n),beta(n),gamma(n), length(n)
     double precision c1,c2, s1,s2, g1,g2
     double precision Stri(3), Strim1(3)
     double precision Ai(3), Aim1(3), Bi(3), Bim1(3)
     double precision Chi(3),Chim1(3), Zhi(3),Zhim1(3)
     double precision force(n3), torque(n3)
     !
     !TO BE DONE
     !
     !
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! starting by calculating the mechanical forces
     !twisting forces , bending forces and streching forces
     !mechanical forces on cores are first calculated
     !! Note that Bending and steching force function won't change from last file only twisting energy
     ! Twising on first bead particle (a core)


     !Bending on first bead particle (a core)
           if ( beta(1) .ge. 1.0d-10 ) then
        g1 = beta(1) / ( dsin( beta(1) )*length(1) )
      else
        g1 = 1.0d0 / length(1)
      end if
            c1 = dcos( beta(1) )
      Ai(1) = g1*( a(4)-c1*a_dna(1) )
      Ai(2) = g1*( a(5)-c1*a_dna(2) )
      Ai(3) = g1*( a(6)-c1*a_dna(3) )
      if ( beta_p(1) .ge. 1.0d-10 ) then
        g2 = beta_p(1) / ( dsin( beta_p(1) )*length(1) )
      else
        g2 = 1.0d0 / length(1)
      end if
      c2 = dcos( beta_p(1) )
      Bi(1) = g2*( a(1)-c2*a_dna(1) )
      Bi(2) = g2*( a(2)-c2*a_dna(2) )
      Bi(3) = g2*( a(3)-c2*a_dna(3) )
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!! end of bending forces on core
     !!!! Remarks: I don't understand the if conditions (both ofthem to determine g1 and g2 )
     ! Streching force on first particle (a core)
      Stri(1) = (length(1)-lo)*a_dna(1)
      Stri(2) = (length(1)-lo)*a_dna(2)
      Stri(3) = (length(1)-lo)*a_dna(3)

      Strim1(1) = 0.d0
      Strim1(2) = 0.d0
      Strim1(3) = 0.d0
      ! calculating mechanical forces without twisting
         force(1) = h*( Stri(1) )
     +         - g*( Ai(1) + Bi(1) )

      force(2) = h*( Stri(2) )
     +         - g*( Ai(2) + Bi(2) )

      force(3) = h*( Stri(3) )
     +         - g*( Ai(3) + Bi(3) )





