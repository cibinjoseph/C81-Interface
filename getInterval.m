function [L,R] = getInterval_my(A,S)
  % Uses binary search to determine interval 
  % in which S lies in sorted array A
  % A is assumed to be a sorted 1-d array

  L = 1;
  n = length(A);
  R = n;

  % Binary search algorithm
  while ((L~=R) && (R ~= (L+1)))
    i = floor((L+R)*0.5);
    if (S < A(i))
      R = i;
    elseif (S > A(i))
      L = i;
    else
      L = i;
      R = i;
    end
  end

  % Check end cases 
  if (abs(A(L)-S) <= eps)
    R = L;
  elseif (abs(A(R)-S) <= eps)
    L = R;
  end
  return;
