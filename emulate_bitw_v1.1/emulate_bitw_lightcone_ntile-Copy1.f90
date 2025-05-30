!> last modified 22-06-2023

program emulate_bitw

  implicit none

  include 'PRM_emulate_bitw.f90'
  include 'PRM_we.f90'
  include 'PRM_string_attributes.f90'
  include 'PRM_selprop_bins.f90'
  
  character(msl) :: namin_part,pfx_out,namin_selmsk,pfx_selprop,namin_qref,emu_dir, tracer
  integer(4) :: npart,seed,thold
  logical(4) :: learn_lg,emulate_lg,individual_lg,true_ngsel_lg,close_friends_lg,antico_lg,adj_qref_lg
  real(8), dimension(:), allocatable :: ra,dec,rshift
  real(8), dimension(:), allocatable :: w,wiip
  logical, dimension(:), allocatable :: msk,msk_t
  integer(4), dimension(:), allocatable :: haloid,chain
  character(1) :: dum
  character(msl) :: namini,namini_wemu
  integer(itype), dimension(:,:), allocatable :: we
  integer(4) :: i,j,k
  integer(4), dimension(:), allocatable :: chain_ini
  real(8), dimension(:), allocatable :: w_tup
  logical, dimension(:,:), allocatable :: l
  integer(4) :: idum
  integer(4) :: nhalo
  logical, dimension(:), allocatable :: selmsk
  logical, dimension(:), allocatable :: selmsk_tup
  integer(4), dimension(:), allocatable :: sz,ngsel
  real(8), dimension(:), allocatable :: nt
  integer(4) :: ih
  real(8), dimension(nlogbins_sz,nlogbins_ntm) :: qref
  character(msl) :: str_tmp
  real(8) :: mu_sel,sg_sel,frac_nt0
  logical :: fix_qref_lg
  real(8) :: sz0_fix,sg_sz_fix,ntm0_fix,sg_ntm_fix
  real(8), dimension(nlogbins_ntm) :: qref_individual
  integer(4) :: smpl
  character(3) :: smpl_c
  integer(4), dimension(:), allocatable :: cfc
  real(8), dimension(nbins_cfc,nbins_tnt) :: qref_cfc
  integer(4), dimension(nbins_cfc,nbins_tnt) :: cnt_cfc
  real(8) :: deco,beta1,beta0
  integer(4), dimension(4) :: itest
  logical :: eff_lg
  character(4) :: mocknum
  real(8) :: linklength
  character(20) :: str_beta0, str_beta1, str_deco, str_linklen

  !namini='INI_emulate_bitw_lightcone_ntile.txt'
  call get_command_argument(1,namini)
  write(*,*)'namini = ',trim(namini)
  open(1000,file=trim(namini),action="read")
  read(1000,*)dum,namin_part
  read(1000,*)dum,namin_selmsk
  read(1000,*)dum,pfx_selprop
  read(1000,*)dum,namin_qref 
  read(1000,*)dum,pfx_out
  read(1000,*)dum,npart
  read(1000,*)dum,seed
  read(1000,*)dum,thold  
  read(1000,*)dum,learn_lg
  read(1000,*)dum,emulate_lg
  read(1000,*)dum,individual_lg
  read(1000,*)dum,close_friends_lg
  read(1000,*)dum,true_ngsel_lg
  read(1000,*)dum,mu_sel
  read(1000,*)dum,sg_sel
  read(1000,*)dum,frac_nt0
  read(1000,*)dum,fix_qref_lg
  read(1000,*)dum,sz0_fix
  read(1000,*)dum,sg_sz_fix
  read(1000,*)dum,ntm0_fix
  read(1000,*)dum,sg_ntm_fix
  read(1000,*)dum,smpl
  read(1000,*)dum,antico_lg
  read(1000,*)dum,deco
  read(1000,*)dum,adj_qref_lg
  read(1000,*)dum,beta0
  read(1000,*)dum,beta1
  read(1000,*)dum,emu_dir
  read(1000,*)dum,mocknum
  read(1000,*)dum,linklength
  read(1000,*)dum,tracer
  close(1000)
  
  write(*,*)'emu_dir          = ',trim(emu_dir)
  write(*,*)'namin_part       = ',trim(namin_part)
  write(*,*)'namin_selmsk     = ',trim(namin_selmsk)
  write(*,*)'pfx_selprop      = ',trim(pfx_selprop)
  write(*,*)'namin_qref       = ',trim(namin_qref)
  write(*,*)'pfx_out          = ',trim(pfx_out)
  write(*,*)'npart            =',npart
  write(*,*)'seed             =',seed
  write(*,*)'thold            =',thold
  write(*,*)'learn_lg         =',learn_lg
  write(*,*)'emulate_lg       =',emulate_lg
  write(*,*)'individual_lg    =',individual_lg
  write(*,*)'close_friends_lg =',close_friends_lg  
  write(*,*)'true_ngsel_lg    =',true_ngsel_lg
  write(*,*)'mu_sel   =',mu_sel
  write(*,*)'sg_sel   =',sg_sel  
  write(*,*)'frac_nt0 =',frac_nt0
  write(*,*)'fix_qref_lg = ',fix_qref_lg
  write(*,*)'sz0_fix     =',sz0_fix
  write(*,*)'sg_sz_fix   =',sg_sz_fix  
  write(*,*)'ntm0_fix    =',ntm0_fix
  write(*,*)'sg_ntm_fix  =',sg_ntm_fix
  write(*,*)'smpl =',smpl  
  write(*,*)'nwe   = ',nwe
  write(*,*)'nbits =',nbits
  write(*,*)'antico_lg = ',antico_lg
  write(*,*)'deco      =',deco
  write(*,*)'adj_qref_lg = ',adj_qref_lg
  write(*,*)'beta0       =',beta0
  write(*,*)'beta1       =',beta1
  write(*,*)'mocknum     =',mocknum
  write(*,*)'linklength  =',linklength
  write(*,*)'tracer      =',tracer
  write(str_beta0, '(F6.3)') beta0
  write(str_beta1, '(F6.3)') beta1
  write(str_deco, '(F6.3)') deco
  write(str_linklen, '(F6.3)') linklength
    
  allocate(w(npart))
  allocate(msk(npart))
  allocate(haloid(npart),chain(npart))
  allocate(chain_ini(npart))
  allocate(selmsk(npart))
  allocate(cfc(npart))
  
  idum=seed
  
  write(*,*)'reading particle catalogue ...'
  w=1.d0
  msk=.true.
  allocate(ra(npart),dec(npart),rshift(npart))
  call read_sample_lightcone(npart,ra,dec,rshift,haloid,w,msk,cfc,namin_part)
  !w=1.d0
  !deallocate(ra,dec,rshift)
  write(*,*)'done'
  write(*,*)'sum(w) =',sum(w)
  write(*,*)'count(msk) =',count(msk)
  
  write(*,*)'computing particle chain ...'
  call get_chain(npart,haloid,msk,chain,chain_ini)
  write(*,*)'done'
  
  write(*,*)'computing number of halos ...'
  call get_nhalo(npart,msk,haloid,thold,nhalo)
  write(*,*)'done'
  
  if (learn_lg) then
     
     allocate(sz(nhalo),ngsel(nhalo),nt(nhalo))

     sz=-1
     ngsel=-1
     nt=-1.d0
     
     write(*,*)'reading selection mask ...'
     selmsk=.true.
     call read_selmsk(npart,selmsk,namin_selmsk)
     write(*,*)'before intersecting: count(selmsk) =',count(selmsk)
     selmsk=selmsk.and.msk
     write(*,*)'done'
     write(*,*)'count(selmsk) =',count(selmsk)

     if (individual_lg) then
        if (close_friends_lg) then
           write(*,*)'learning selection properties (individual cfc) ...'
           call individual_selfrac_cfc(npart,w,cfc,msk,selmsk,pfx_selprop)
           write(*,*)'done'
        else
           write(*,*)'learning selection properties (individual) ...'
           call individual_selfrac(npart,w,msk,selmsk,pfx_selprop)
           write(*,*)'done'
        endif
     else
        
        write(*,*)'learning selection properties ...'
        ih=1
        do i=1,npart
           call advo(i,npart,5)
           if (chain_ini(i).ge.thold) then
              allocate(w_tup(chain_ini(i)))
              allocate(selmsk_tup(chain_ini(i)))     
              j=i
              k=1
              do while(j.gt.0)
                 w_tup(k)=w(j)
                 selmsk_tup(k)=selmsk(j)
                 j=chain(j)
                 k=k+1
              enddo
              sz(ih)=chain_ini(i)
              call get_selprop(sz(ih),w_tup,selmsk_tup,ngsel(ih),nt(ih))
              deallocate(w_tup)
              deallocate(selmsk_tup)
              ih=ih+1
           endif
        enddo
        write(*,*)'done'
        
        write(*,*)'writing selection properties ...'
        call write_selprop(nhalo,sz,ngsel,nt,.true.,.true.,pfx_selprop)
        write(*,*)'done'
        
        deallocate(sz,ngsel,nt)   
     endif
     
  endif
  
  if (emulate_lg) then

     allocate(we(npart,nwe))
     we=0
     
     if (individual_lg) then
        
        if (close_friends_lg) then
           
           write(*,*)'reading qref (individual cfc) ...'
           call read_qref_individual_cfc(qref_cfc,cnt_cfc,namin_qref)
           write(*,*)'done'
           if (adj_qref_lg) then
              write(*,*)'adjusting qref (individual cfc) ...'
              call adjust_qref(qref_cfc,cnt_cfc,beta0,beta1,deco,.true.,emu_dir,linklength,tracer,mocknum)
              write(*,*)'done'
           endif
           write(*,*)'emulating bitwise weights (individual cfc) ...'
           allocate(l(npart,nbits))
           l=.false.
           call wemu_lightcone_ntile_individual_cfc(npart,w,cfc,l,idum,qref_cfc,msk,mu_sel,sg_sel,frac_nt0)
           call l2we(npart,l,we)
           deallocate(l)
           write(*,*)'done'
           
        else
           
           write(*,*)'reading qref (individual) ...'
           call read_qref_individual(qref_individual,namin_qref)
           write(*,*)'done'
           write(*,*)'emulating bitwise weights (individual) ...'
           allocate(l(npart,nbits))
           l=.false.
           call wemu_lightcone_ntile_individual(npart,w,l,idum,qref_individual,msk,mu_sel,sg_sel,frac_nt0)
           call l2we(npart,l,we)
           deallocate(l)
           write(*,*)'done'
           
        endif

        if (antico_lg) then
           write(*,*)'"injecting" anticorrelation ...'
           do i=1,npart
              call advo(i,npart,5)
              if (chain_ini(i).ge.thold) then
                 allocate(l(chain_ini(i),nbits))
                 j=i
                 k=1
                 do while(j.gt.0)
                    call we2l(1,we(j,:),l(k,:))
                    !write(*,*)l(k,:)
                    !read(*,*)
                    j=chain(j)
                    k=k+1
                 enddo
                 
                 call antico(chain_ini(i),l,idum,deco)
        
                 j=i
                 k=1
                 do while(j.gt.0)
                    call l2we(1,l(k,:),we(j,:))
                    !write(*,*)we(j,:)
                    !read(*,*)
                    j=chain(j)
                    k=k+1
                 enddo
                 
                 deallocate(l)
              endif
           enddo
           write(*,*)'done'
        endif

     else

        write(*,*)'reading qref ...'
        call read_qref(qref,namin_qref)
        write(*,*)'done'
        
        if (fix_qref_lg) then
           write(*,*)'extrapolating qref ...'
           call fix_qref(qref,sz0_fix,sg_sz_fix,ntm0_fix,sg_ntm_fix,.true.)
           write(*,*)'done'
        endif
        
        write(*,*)'emulating bitwise weights ...'
        do i=1,npart
           call advo(i,npart,5)
           if (chain_ini(i).ge.thold) then
              !write(*,*)'i =',i
              !write(*,*)' chain_ini(i) =',chain_ini(i)
              allocate(w_tup(chain_ini(i)))
              allocate(l(chain_ini(i),nbits))
              allocate(selmsk_tup(chain_ini(i)))
              j=i
              k=1
              do while(j.gt.0)
                 w_tup(k)=w(j)
                 selmsk_tup(k)=selmsk(j)
                 j=chain(j)
                 k=k+1
              enddo
              l=.false.
              
              call wemu_lightcone_ntile(chain_ini(i),w_tup,l,idum,qref,selmsk_tup,mu_sel,sg_sel,frac_nt0,true_ngsel_lg)
              
              j=i
              k=1
              do while(j.gt.0)
                 call l2we(1,l(k,:),we(j,:))
                 j=chain(j)
                 k=k+1
              enddo
              deallocate(w_tup)
              deallocate(l)
              deallocate(selmsk_tup)
           endif
        enddo
        write(*,*)'done'
     endif
        
     allocate(wiip(npart))

     write(*,*)'computing IIP weights ...'
     eff_lg=.false.
     call we2wiip(npart,we,wiip,eff_lg)
     write(*,*)'done'
     write(*,*)'sum(wiip) =',sum(wiip)
     !write(*,*)'count(selmsk) =',count(selmsk)
     
     allocate(msk_t(npart))
     itest=(/1,nbits/2,2*nbits/3,int(nbits)/)
     do i=1,4
        msk_t=msk
        call smplmsk(npart,we,msk_t,itest(i))
        write(*,*)'test smpl =',itest(i)
        write(*,*)' count(msk_t) =',count(msk_t),' (vs',count(selmsk),')'
        write(*,*)' sum(wiip,msk_t) =',sum(wiip,msk_t),' (vs',count(msk),')'
     enddo
     if (smpl.gt.0.and.smpl.le.nbits) then
        msk_t=msk
        call smplmsk(npart,we,msk_t,smpl)
        call itg2chr(smpl,smpl_c)
        write(*,*)'writing test sample '//trim(smpl_c)//' ...'
        str_tmp=trim(pfx_out)//'obs_smpl'//trim(smpl_c)//'.dat'
        call write_obs_sample(npart,ra,dec,rshift,msk_t,wiip,str_tmp)
        write(*,*)'done'
     endif
     deallocate(msk_t)

     write(*,*)'count(selmsk.and.not.msk) =',count(selmsk.and..not.msk) 
     
     write(*,*)'writing output ...'
     call write_wemu(npart,we,pfx_out)
     write(*,*)'done'

     allocate(l(1000,nbits))
     do i=1,1000
        call we2l(1000,we(:1000,:),l)
     enddo
     open(7654,file=trim(emu_dir)//'out/bool_test_tracer_'//trim(tracer)//'_m'//trim(mocknum)//'_ll_'//trim(str_linklen)//'_b0_'//trim(str_beta0)//'_b1_'//trim(str_beta1)//'_deco_'//trim(str_deco)//'.txt')
     do i=1,1000
        write(7654,*)l(i,:)
     enddo
     close(7654)
     
     deallocate(we)
     deallocate(wiip)
     
  endif
  
  deallocate(ra,dec,rshift)
  deallocate(w)
  deallocate(msk)
  deallocate(haloid,chain)
  deallocate(chain_ini)
  deallocate(selmsk)
  deallocate(cfc)
  
end program emulate_bitw

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> ROUTINES

include 'adjust_qref.f90'
include 'advo.f90'
!include 'advs.f90'
include 'antico.f90'
include 'bilin_ipol.f90'
include 'fix_qref.f90'
include 'gasdev.for'
include 'get_chain.f90'
include 'get_nhalo.f90'
include 'get_selprop.f90'
include 'individual_selfrac.f90'
include 'individual_selfrac_cfc.f90'
include 'ipol.f90'
include 'itg2chr.f90'
include 'l2we.f90'
include 'ran1.for'
include 'ranidx.f90'
include 'ranidx_vetomask.f90'
include 'read_qref.f90'
include 'read_qref_individual.f90'
include 'read_qref_individual_cfc.f90'
include 'read_sample_lightcone.f90'
include 'read_selmsk.f90'
include 'sigmoid.f90'
include 'smplmsk.f90'
include 'we2l.f90'
include 'we2wiip.f90'
include 'wemu_lightcone_ntile.f90'
include 'wemu_lightcone_ntile_individual.f90'
include 'wemu_lightcone_ntile_individual_cfc.f90'
include 'write_selprop.f90'
include 'write_wemu.f90'
include 'write_obs_sample.f90'
