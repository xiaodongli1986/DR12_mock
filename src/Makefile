EXEA =  ../creat-HR3-mock-creat_patched_mocks
EXEB =  ../creat-HR3-mock-radial_select
EXEC =  ../creat-HR3-mock-add_mask
EXED =  ../creat-HR3-mock-add_info
#EXEZ =  ../creat-HR3-mock-RSDMocks_for_Test
EXEZ_filt = ../filt_data_by_mangle

OBJS = ap_tools.o ap_cosmo_funs.o mainA_creat_patched_mocks.o  mainB_radial_select.o mainC_add_mask.o mainD_add_info.o  mainZ_RSDMocks_for_Test.o mainZ_filt_data_by_mangle.o
F90C = ifort -heap-arrays
F90FLAGS = 

default: $(EXEA) $(EXEB) $(EXEC) $(EXED) $(EXEZ_filt)

$(EXEA): $(OBJS)
	$(F90C) -o $(EXEA)  ap_tools.o ap_cosmo_funs.o mainA_creat_patched_mocks.o
$(EXEB): $(OBJS)
	$(F90C) -o $(EXEB)  ap_tools.o ap_cosmo_funs.o mainB_radial_select.o
$(EXEC): $(OBJS)
	$(F90C) -o $(EXEC)  ap_tools.o ap_cosmo_funs.o mainC_add_mask.o
$(EXED): $(OBJS)
	$(F90C) -o $(EXED)  ap_tools.o ap_cosmo_funs.o mainD_add_info.o        
$(EXEZ_filt): $(OBJS)
	$(F90C) -o $(EXEZ_filt)  ap_tools.o ap_cosmo_funs.o mainZ_filt_data_by_mangle.o
#$(EXEZ): $(OBJS)
#	$(F90C) -o $(EXEZ)  ap_tools.o ap_cosmo_funs.o mainZ_RSDMocks_for_Test.o        

ap_cosmo_funs.o: ap_tools.o
mainA_creat_patched_mocks.o:  ap_cosmo_funs.o
mainB_radial_select.o:  ap_cosmo_funs.o
mainC_add_mask.o:  ap_cosmo_funs.o
mainD_add_info.o:  ap_cosmo_funs.o
mainZ_RSDMocks_for_Test.o:  ap_cosmo_funs.o
mainZ_filt_data_by_mangle.o: ap_tools.o

%.o: %.f90
	$(F90C) -c -heap-arrays $*.f90 $(F90FLAGS)

clean :
	rm -rf *.o *.mod $(EXEA) $(EXEB) $(EXEC) $(EXED) $(EXEZ_filt)
