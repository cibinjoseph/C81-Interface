function indx = binarySearch(A,T)
L = 0;
n = length(A);
R = n;
while (L < R)
  indx = floor((L+R)*0.5);
  if (A(indx) < T)
    L = indx+1;
  else
    R = indx;
  end
end
indx = L-1;
return;
