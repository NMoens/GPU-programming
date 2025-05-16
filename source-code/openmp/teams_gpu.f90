program teams

use OMP_lib

implicit none

integer :: num_teams, team_num
integer :: num_threads, thread_num

write(*,*) "Using default number of teams and thread limit"
!$OMP target teams
!$OMP parallel
!$OMP single
num_teams = omp_get_num_teams()
num_threads = omp_get_num_threads()
write(*,*) "Number of teams: ", num_teams, ", number of threads: ", num_threads
!$OMP end single
team_num = omp_get_team_num()
thread_num = omp_get_thread_num()
write(*,*) "Hello from team ", team_num, ", thread ", thread_num
!$OMP end parallel
!$OMP end target teams

end program teams
