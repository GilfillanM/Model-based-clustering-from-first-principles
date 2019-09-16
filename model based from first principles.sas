data model_based;
input x y;
cards;
0 0
1.5 0
2 0
2.3 0
3 0
6 0
7.4 0
11.7 0
12 0
13 0
14 0
15 0
17 0
;
run;

proc gplot data = model_based ;
plot y*x;
run;


proc iml;
use model_based;
read all into xy;
x = xy[,1];

* Randomly choose a strating vlaue for mu and var;
mu_a = 2;
mu_b = 5;
var_a = 3;
var_b = 3;

* Make a loop to get the probability of each data being in a/b;
n = nrow(x);
pi = constant('pi');
prob_a = 0.5; *priors;
prob_b = 1-prob_a;

old_var_a =2;
old_var_b =2;
diff = 1111;

do kk = 1 to 15 until (diff = 0);
print "***********************" ;
print "Iteration: " kk ;

 do i = 1 to n;
 prob_x_given_a = prob_x_given_a // (1/(sqrt(2*pi*var_a)))*(exp(-((x[i,]-mu_a)**2)/2*var_a));
 prob_x_given_b = prob_x_given_b // (1/(sqrt(2*pi*var_b)))*(exp(-((x[i,]-mu_b)**2)/2*var_b));
 end;

  do j = 1 to n; *posteriors;
  a = a // (prob_x_given_a[j,]*prob_a)/(prob_x_given_a[j,]*prob_a + prob_x_given_b[j,]*prob_b);
  b = b // (prob_x_given_b[j,]*prob_b)/(prob_x_given_b[j,]*prob_b + prob_x_given_a[j,]*prob_a);
  end;
  
   do jj = 1 to n;
   *new mu's;
   top_a = sum(top_a // a[jj,]*x[jj,]);
   top_b = sum(top_b // b[jj,]*x[jj,]);
   mu_a = top_a /(a[+,]);
   mu_b = top_b / (b[+,]);
   end;
   print mu_a;
   print mu_b;


   * new variances;
   do k = 1 to n;
   top_var_a = sum(top_var_a // (a[k,]*(x[k,] - mu_a)**2));
   top_var_b = sum(top_var_b // (b[k,]*(x[k,] - mu_b)**2));
   var_a = top_var_a / (a[+,]);
   var_b = top_var_b / (b[+,]);
   end;
   print var_a;
   print var_b;

if var_a < old_var_a then diff=0;
if var_b < old_var_b then diff=0;

end;
  quit;
		
